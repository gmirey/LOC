// Part of LocLang/Compiler
// Copyright 2022-2023 Guillaume Mirey
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License. 

#pragma once

#ifndef LOCLIB_TYPE_REGISTRATION_H_
#define LOCLIB_TYPE_REGISTRATION_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/HashSets.h"

#include "LocLib_ScanAndTok.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_IR_Types.h"
#include "LocLib_ProgramState.h"
#include "LocLib_TypeCheckerCore.h"

local_func bool wait_lock_type_registration(WholeProgramCompilationState* pCompState)
{
    // TODO
    return true;
}

local_func void unlock_type_registration(WholeProgramCompilationState* pCompState)
{
    // TODO
}

local_func u64 declare_prolog_const_int_as_nyka(u8 uFormat, u64 uPayload, u32 uFlags, WholeProgramCompilationState* pCompState)
{
    IRRepo* pRepo = &(pCompState->programwiseRepo);
    u32 uPosNewDecl = pRepo->uSize;
    IREntry* pDeclEntry = ir_append_new_entry(pRepo);
    pDeclEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_DECLARATION) | (u64(uFormat)<<16);
    u32 uAlignAndSlots = (get_log2_of_natural_align_from_format(uFormat) << 28) | 1u;
    pDeclEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_KNOWN|uFlags|(u64(uAlignAndSlots) << IR_STD_PARAM_SHIFT);
    pDeclEntry->metaValue._payload = uPayload;
    return ir_make_direct_nyka_value(ir_make_std_code_programwise_global(uPosNewDecl));
}

local_func TypeInfo_Integral* make_int_format_impl(ECoreType eCoreType, u8 uLog2OfByteCount, ESignedness eSignedness,
                                                   WholeProgramCompilationState* pCompState, u64 uNykaOfMax, u64 uNykaOfMin)
{
    TypeInfo_Integral* pResult = (TypeInfo_Integral*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_Integral), alignof(TypeInfo_Integral));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_INTEGRAL;
    pResult->_coreType = eCoreType;
    pResult->_coreFlags = u8(eSignedness);
    pResult->uIRFormat = uLog2OfByteCount;
    pResult->uSlotsAndAlign = (u32(uLog2OfByteCount) << 28) | 1u;
    pResult->uNykaOfMax = uNykaOfMax;
    pResult->uNykaOfMin = uNykaOfMin;
    return pResult;
}

local_func_inl u64 get_type_rtti_decl_ir(const TypeInfo* pType)
{
    Assert_(pType->asNode.info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    Assert_(pType->asNode.info.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST);
    Assert_(pType->asNode.info.uIRandMetaFlags & IRFLAG_TC_ONLY);
    return pType->asNode.info.uIRandMetaFlags & IR_STD_PARAM_MASK;
}

local_func u64 make_and_register_rtti(IRRepo* pRepo, Arena arena, TmpMap<u64, const TypeInfo*>* pMapTypeIdToTCTypeInfo,
                                      const TypeInfo* pType, u8* pRTTIData, u32 uRTTIByteSize, const u32* pNykasOffsets, u32 uNykasCount)
{
    u64 uIROfNewDecl = make_const_non_embd_decl_with_nykas(pRepo, arena,
        IR_INSTRFLAG_CONST_IS_RTTI,
        IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA,
        pRTTIData, 0x00u, uRTTIByteSize, 4u, pNykasOffsets, uNykasCount);
    // We'll cross-ref runtime typeinfo to comptime typeinfo with the runtimetypeid, being the nyka of the rtti.
    u64 uAsNykaResult = ir_make_direct_nyka_value(uIROfNewDecl);
    pMapTypeIdToTCTypeInfo->insert(uAsNykaResult, pType);
    // We return a weird thing, being the IR reference of the RTTI declaration, keeping the 'TC_ONLY' flag since it will be the value carried by our comptime types.
    return uIROfNewDecl|u64(IRFLAG_IS_KNOWN|IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST);
}

local_func_inl u64 make_and_register_programise_rtti(WholeProgramCompilationState* pCompState,
                                                     const TypeInfo* pType, u8* pRTTIData, u32 uRTTIByteSize, const u32* pNykasOffsets, u32 uNykasCount)
{
    return make_and_register_rtti(&(pCompState->programwiseRepo), pCompState->globalArena, &(pCompState->mapRuntimeTypeIdToTCTypeInfo),
                                 pType, pRTTIData, uRTTIByteSize, pNykasOffsets, uNykasCount);
}

local_func u64 make_and_register_filewise_rtti(SourceFileDescAndState* pSourceFile, const TypeInfo* pType,
                                               u8* pRTTIData, u32 uRTTIByteSize, const u32* pNykasOffsets, u32 uNykasCount)
{
    return make_and_register_rtti(&(pSourceFile->filewiseConstRepo), pSourceFile->localArena, &(pSourceFile->mapRuntimeTypeIdToTCTypeInfo),
                                 pType, pRTTIData, uRTTIByteSize, pNykasOffsets, uNykasCount);
}

//
// The declaration of core types registers them as runtime typeinfo and assigns that typeinfo to the crossref IR on the type.
//

local_func u64 declare_core_int_format(const TypeInfo_Integral* pType, WholeProgramCompilationState* pCompState)
{
    // We'll reuse all space of our comptime type info for runtime type info.
    u32 uRTTIByteSize = sizeof(TypeInfo_Integral); // the 3x64b of pType->asNode will be salvaged for 64b NYKA of compact string for identifier, 64b NamespaceId (?), 64b reserved.
    u8* pRTTIData = alloc_from(pCompState->globalArena, uRTTIByteSize, 8u);  // All allocs corresponding to runtime data are 64b aligned
    // And we begin by copying all comptime to runtime. We'll care about our special runtime content afterwards.
    memcpy(pRTTIData, (u8*)pType, uRTTIByteSize);
    // We'll have nykas here (the 3 salvaged pos of 'asNode', + min, + max)
    constexpr u32 uNykasCount = 5; // identifier, namespace, reserved, min, max
    constexpr u32 asNodeOffset = offsetof(TypeInfo_Integral, asNode);
    constexpr u32 tNykasOffsets[uNykasCount] = { asNodeOffset, asNodeOffset+8u, asNodeOffset+16u, offsetof(TypeInfo_Integral, uNykaOfMin), offsetof(TypeInfo_Integral, uNykaOfMax) };
    *(u64*)(pRTTIData + tNykasOffsets[0]) = 0uLL; // TODO: identifier
    *(u64*)(pRTTIData + tNykasOffsets[1]) = 0uLL; // TODO: namespace
    *(u64*)(pRTTIData + tNykasOffsets[2]) = 0uLL; // TODO: reserved

    return make_and_register_programise_rtti(pCompState, pType, pRTTIData, uRTTIByteSize, tNykasOffsets, uNykasCount);
}

local_func u64 declare_core_fp_format(const TypeInfo_FloatingPoint* pType, WholeProgramCompilationState* pCompState)
{
    // We'll reuse all space of our comptime type info for runtime type info.
    u32 uRTTIByteSize = sizeof(TypeInfo_FloatingPoint); // the 3x64b of pType->asNode will be salvaged for 64b NYKA of compact string for identifier, 64b NamespaceId (?), 64b reserved.
    u8* pRTTIData = alloc_from(pCompState->globalArena, uRTTIByteSize, 8u);  // All allocs corresponding to runtime data are 64b aligned
    // And we begin by copying all comptime to runtime. We'll care about our special runtime content afterwards.
    memcpy(pRTTIData, (u8*)pType, uRTTIByteSize);
    // We'll have nykas here (the 3 salvaged pos of 'asNode')
    constexpr u32 uNykasCount = 3; // identifier, namespace, reserved
    constexpr u32 asNodeOffset = offsetof(TypeInfo_Integral, asNode);
    constexpr u32 tNykasOffsets[uNykasCount] = { asNodeOffset, asNodeOffset+8u, asNodeOffset+16u };
    *(u64*)(pRTTIData + tNykasOffsets[0]) = 0uLL; // TODO: identifier
    *(u64*)(pRTTIData + tNykasOffsets[1]) = 0uLL; // TODO: namespace
    *(u64*)(pRTTIData + tNykasOffsets[2]) = 0uLL; // TODO: reserved

    return make_and_register_programise_rtti(pCompState, pType, pRTTIData, uRTTIByteSize, tNykasOffsets, uNykasCount);
}

local_func u64 declare_other_core_format(const TypeInfo_OtherCore* pType, WholeProgramCompilationState* pCompState)
{
    // We'll reuse all space of our comptime type info for runtime type info.
    u32 uRTTIByteSize = sizeof(TypeInfo_OtherCore); // the 3x64b of pType->asNode will be salvaged for 64b NYKA of compact string for identifier, 64b NamespaceId (?), 64b reserved.
    u8* pRTTIData = alloc_from(pCompState->globalArena, uRTTIByteSize, 8u);  // All allocs corresponding to runtime data are 64b aligned
    // And we begin by copying all comptime to runtime. We'll care about our special runtime content afterwards.
    memcpy(pRTTIData, (u8*)pType, uRTTIByteSize);
    // We'll have nykas here (the 3 salvaged pos of 'asNode' + AsRuntimeStruct)
    constexpr u32 uNykasCount = 4; // identifier, namespace, reserved, AsRuntimeStruct
    constexpr u32 asNodeOffset = offsetof(TypeInfo_Integral, asNode);
    constexpr u32 tNykasOffsets[uNykasCount] = { asNodeOffset, asNodeOffset+8u, asNodeOffset+16u, offsetof(TypeInfo_OtherCore, pAsRuntimeStruct) };
    *(u64*)(pRTTIData + tNykasOffsets[0]) = 0uLL; // TODO: identifier
    *(u64*)(pRTTIData + tNykasOffsets[1]) = 0uLL; // TODO: namespace
    *(u64*)(pRTTIData + tNykasOffsets[2]) = 0uLL; // TODO: reserved
    u64 uNykaAsRuntimeStruct = 0uLL;
    if (pType->pAsRuntimeStruct) {
        u64 uIRposAsRuntimeStruct = get_type_rtti_decl_ir(pType->pAsRuntimeStruct);
        Assert_(ir_is_valid_param(uIRposAsRuntimeStruct));
        uNykaAsRuntimeStruct = ir_make_direct_nyka_value(uIRposAsRuntimeStruct);
    }
    *(u64*)(pRTTIData + tNykasOffsets[3]) = uNykaAsRuntimeStruct;

    return make_and_register_programise_rtti(pCompState, pType, pRTTIData, uRTTIByteSize, tNykasOffsets, uNykasCount);
}

local_func u64 declare_core_vec_format(const TypeInfo_HwVector* pType, WholeProgramCompilationState* pCompState)
{
    // We'll reuse all space of our comptime type info for runtime type info.
    u32 uRTTIByteSize = sizeof(TypeInfo_HwVector); // the 3x64b of pType->asNode will be salvaged for 64b NYKA of compact string for identifier, 64b NamespaceId (?), 64b reserved.
    u8* pRTTIData = alloc_from(pCompState->globalArena, uRTTIByteSize, 8u);  // All allocs corresponding to runtime data are 64b aligned
    // And we begin by copying all comptime to runtime. We'll care about our special runtime content afterwards.
    memcpy(pRTTIData, (u8*)pType, uRTTIByteSize);
    // We'll have nykas here (the 3 salvaged pos of 'asNode' + ElementType)
    constexpr u32 uNykasCount = 4; // identifier, namespace, reserved, ElementType
    constexpr u32 asNodeOffset = offsetof(TypeInfo_Integral, asNode);
    constexpr u32 tNykasOffsets[uNykasCount] = { asNodeOffset, asNodeOffset+8u, asNodeOffset+16u, offsetof(TypeInfo_HwVector, pElementType) };
    *(u64*)(pRTTIData + tNykasOffsets[0]) = 0uLL; // TODO: identifier
    *(u64*)(pRTTIData + tNykasOffsets[1]) = 0uLL; // TODO: namespace
    *(u64*)(pRTTIData + tNykasOffsets[2]) = 0uLL; // TODO: reserved
    Assert_(pType->pElementType);
        u64 uIRposElementType = get_type_rtti_decl_ir(pType->pElementType);
        Assert_(ir_is_valid_param(uIRposElementType));
        u64 uNykaElementType = ir_make_direct_nyka_value(uIRposElementType);
    *(u64*)(pRTTIData + tNykasOffsets[3]) = uNykaElementType;

    return make_and_register_programise_rtti(pCompState, pType, pRTTIData, uRTTIByteSize, tNykasOffsets, uNykasCount);
}

local_func u64 declare_structlike_format(const TypeInfo_StructLike* pType, IRRepo* pRepo, Arena dataArena, Arena tmpArena, TmpMap<u64, const TypeInfo*>* pMapTypeIdToTCTypeInfo)
{
    // We'll do something a little special for runtime type info of structlikes
    // The base typeinfo is like every other type: salvaging the 3x64b of pType->crossRefIR for 64b NYKA of compact string for identifier, 64b NamespaceId (?), 64b reserved.
    // However, the 'members' array will be its own thing for RTTI:
    // 16b count of runtime members ; 16b count of const members ; 32b reserved (flags?), then list of runtime members, and list of const members as such:
    // For runtime members: each entry is 64b NYKA of identifier + 64b runtime type id + 32b byte offset in struct + 32b reserved (flags ?)
    // For const members: each entry is 64b NYKA of identifier + 64b type id + 64b NYKA of const declaration

    u16 uRuntimeCount = pType->uRuntimeMemberCount;
    u16 uConstCount = u16(pType->vecAllMembers.size()) - uRuntimeCount;
    u32 uRTTIByteSize = sizeof(TypeInfo_UserBase) + 8u + 24u*uRuntimeCount + 24u*uConstCount;
    u8* pRTTIData = alloc_from(dataArena, uRTTIByteSize, 8u);  // All allocs corresponding to runtime data are 64b aligned
    // And we begin by copying all comptime of a userbase to runtime.
    memcpy(pRTTIData, (u8*)pType, sizeof(TypeInfo_UserBase));
    // And then the various counts
    u16* pToRuntimeCount = (u16*)(pRTTIData + sizeof(TypeInfo_UserBase));
    u16* pToConstCount = pToRuntimeCount + 1;
    u32* pToFlags = (u32*)(((u8*)pToRuntimeCount) + 4u);
    *pToRuntimeCount = uRuntimeCount;
    *pToConstCount = uConstCount;
    *pToFlags = 0u; // TODO ?
    u32 uNykasCount = 3u + 2u*uRuntimeCount + 3u*uConstCount; // identifier, namespace, reserved, per runtime:id and typeid ; per const:id and typeid and const decl
    ArenaRefPoint beforeTmp = get_arena_ref_point(tmpArena);
    u32* pNykasOffsets = (u32*)alloc_from(tmpArena, sizeof(u32)*uNykasCount, alignof(u32));
    constexpr u32 asNodeOffset = offsetof(TypeInfo_StructLike, asNode);
    pNykasOffsets[0] = asNodeOffset;
    pNykasOffsets[1] = asNodeOffset+8u;
    pNykasOffsets[2] = asNodeOffset+16u;
    *(u64*)(pRTTIData + pNykasOffsets[0]) = 0uLL; // TODO: identifier
    *(u64*)(pRTTIData + pNykasOffsets[1]) = 0uLL; // TODO: namespace
    *(u64*)(pRTTIData + pNykasOffsets[2]) = 0uLL; // TODO: reserved
    constexpr u32 uStartOfMemberIndex = 3u;
    constexpr u32 uStartOfMemberOffset = sizeof(TypeInfo_UserBase) + 8u;
    for (u16 uMember = 0u; uMember < uRuntimeCount; uMember++) {
        u32 uOffsetOfIdentifierNyka = uStartOfMemberOffset + uMember*24u;
        u32 uOffsetOfTypeId = uOffsetOfIdentifierNyka + 8u;
        u32 uOffsetOfByteOffset = uOffsetOfTypeId + 8u;
        u32 uOffsetOfFlags = uOffsetOfByteOffset + 4u;
        pNykasOffsets[uStartOfMemberIndex + uMember*2u] = uOffsetOfIdentifierNyka;
        pNykasOffsets[uStartOfMemberIndex + 1u + uMember*2u] = uOffsetOfTypeId;
        *(u64*)(pRTTIData + uOffsetOfIdentifierNyka) = 0uLL; // TODO: identifier
        ValueBinding* pMember = pType->vecAllMembers[uMember];
        Assert_(pMember);
        Assert_(pMember->pType);
        u64 uIRposMemberType = get_type_rtti_decl_ir(pMember->pType);
        Assert_(ir_is_valid_param(uIRposMemberType));
        u64 uNykaMemberType = ir_make_direct_nyka_value(uIRposMemberType);
        *(u64*)(pRTTIData + uOffsetOfTypeId) = uNykaMemberType; // Member Type
        *(u32*)(pRTTIData + uOffsetOfByteOffset) = u32(pMember->info.uIRandMetaFlags);
        *(u32*)(pRTTIData + uOffsetOfFlags) = 0u; // TODO ?
    }
    u32 uStartOfConstIndex = uStartOfMemberIndex + uRuntimeCount*2u;
    u32 uStartOfConstOffset = uStartOfMemberOffset + uRuntimeCount*24u;
    for (u16 uConstMember = 0u; uConstMember < uConstCount; uConstMember++) {
        u32 uOffsetOfIdentifierNyka = uStartOfMemberOffset + uConstMember*24u;
        u32 uOffsetOfTypeId = uOffsetOfIdentifierNyka + 8u;
        u32 uOffsetOfConstDecl = uOffsetOfTypeId + 8u;
        pNykasOffsets[uStartOfConstIndex + uConstMember*2u] = uOffsetOfIdentifierNyka;
        pNykasOffsets[uStartOfConstIndex + 1u + uConstMember*2u] = uOffsetOfTypeId;
        pNykasOffsets[uStartOfConstIndex + 2u + uConstMember*2u] = uOffsetOfConstDecl;
        *(u64*)(pRTTIData + uOffsetOfIdentifierNyka) = 0uLL; // TODO: identifier
        const TypeInfo* pConstMemberType = pType->vecAllMembers[uConstMember + uRuntimeCount]->pType;
        u64 uIRposConstMemberType = get_type_rtti_decl_ir(pConstMemberType);
        Assert_(ir_is_valid_param(uIRposConstMemberType));
        u64 uNykaConstMemberType = ir_make_direct_nyka_value(uIRposConstMemberType);
        *(u64*)(pRTTIData + uOffsetOfTypeId) = uNykaConstMemberType; // const member type
        *(u64*)(pRTTIData + uOffsetOfConstDecl) = 0uLL; // TODO: const decl !!!
    }

    u64 uResult = make_and_register_rtti(pRepo, dataArena, pMapTypeIdToTCTypeInfo, pType, pRTTIData, uRTTIByteSize, pNykasOffsets, uNykasCount);
    reset_arena_no_release_to(beforeTmp, tmpArena);
    return uResult;
}

local_func_inl u64 declare_core_structlike_format(const TypeInfo_StructLike* pType, WholeProgramCompilationState* pCompState, Arena tmpArena)
{
    return declare_structlike_format(pType, &(pCompState->programwiseRepo), pCompState->globalArena, tmpArena, &(pCompState->mapRuntimeTypeIdToTCTypeInfo));
}

local_func_inl u64 declare_user_structlike_format(const TypeInfo_StructLike* pType, SourceFileDescAndState* pSourceFile, Arena tmpArena)
{
    return declare_structlike_format(pType, &(pSourceFile->filewiseConstRepo), pSourceFile->localArena, tmpArena, &(pSourceFile->mapRuntimeTypeIdToTCTypeInfo));
}

/*
local_func void ensure_of_type_registration(const TypeInfo* pType, TCContext* pTCContext)
{
    if (!ir_is_valid_param_(pType->asNode.info.uIRandMetaFlags)) {
        switch (get_type_kind(pType)) {

            case ETypeKind::ETYPEKIND_POINTER: {
                const TypeInfo* pPointedToType = ((const TypeInfo_Pointer*)pType)->pPointedToType;
                ensure_of_type_registration(pPointedToType);
                u64 uIRofPointedType = get_type_rtti_decl_ir(pPointedToType);
                wait_lock_type_registration(pTCContext->pProgCompilationState);
                auto itFound = pTCContext->pProgCompilationState->mapPointerTypesByPointedToType.find(uIRofPointedType);
                if (itFound != pTCContext->pProgCompilationState->mapPointerTypesByPointedToType.end()) {
                    Assert_(!ir_is_immediate(itFound.value()));
                    ir_get_info_from_non_imm(itFound.value(), pTCContext, &(pType->asNode.info));
                } else {
                    u64 uIRofNewPointerDecl = declare_pointer_type((const TypeInfo_Pointer*)pType, uIRofPointedType, pTCContext);
                    Assert_(!ir_is_immediate(uIRofNewPointerDecl));
                    pTCContext->pProgCompilationState->mapPointerTypesByPointedToType.insert(uIRofPointedType, uIRofNewPointerDecl);
                    ir_get_info_from_non_imm(uIRofNewPointerDecl, pTCContext, &(pType->asNode.info));
                }
                unlock_type_registration(pTCContext->pProgCompilationState);
            } break;

            case ETypeKind::ETYPEKIND_ARRAY: {
                const TypeInfo* pElemType = ((const TypeInfo_Array*)pType)->pElementType;
                ensure_of_type_registration(pElemType);
                u64 uIRofElemType = get_type_rtti_decl_ir(pElemType);
                wait_lock_type_registration(pTCContext->pProgCompilationState);
                auto itFoundThisElem = pTCContext->pProgCompilationState->mapArrayTypesByElemType.find(uIRofElemType);
                if (itFoundThisElem == pTCContext->pProgCompilationState->mapArrayTypesByElemType.end()) {
                    TmpMap<u32, u64> newMapBySizeAndKind;
                    itFoundThisElem = pTCContext->pProgCompilationState->mapArrayTypesByElemType.insert(uIRofElemType, newMapBySizeAndKind);
                }
                TmpMap<u32, u64>& mapBySizeAndKind = itFoundThisElem.value();
                u32 uCountAndKind = get_elem_count_and_category_for_array_type((const TypeInfo_Array*)pType);
                auto itFoundThisSizeAndKind = mapBySizeAndKind.find(uCountAndKind);
                if (itFoundThisSizeAndKind != mapBySizeAndKind.end()) {
                    Assert_(!ir_is_immediate(itFoundThisSizeAndKind.value()));
                    ir_get_info_from_non_imm(itFoundThisSizeAndKind.value(), pTCContext, &(pType->asNode.info));
                } else {
                    u64 uIRofNewArrayDecl = declare_array_type((const TypeInfo_Array*)pType, uCountAndKind, uIRofElemType, pTCContext);
                    Assert_(!ir_is_immediate(uIRofNewArrayDecl));
                    mapBySizeAndKind.insert(uCountAndKind, uIRofNewArrayDecl);
                    ir_get_info_from_non_imm(uIRofNewArrayDecl, pTCContext, &(pType->asNode.info));
                }
                unlock_type_registration(pTCContext->pProgCompilationState);
            } break;

            case ETypeKind::ETYPEKIND_MAP: {
                // TODO
                Assert(false, "ensure_of_type_registration() : map types registration not yet implemented.");
                wait_lock_type_registration(pTCContext->pProgCompilationState);
                unlock_type_registration(pTCContext->pProgCompilationState);
            } break;

            case ETypeKind::ETYPEKIND_SET: {
                // TODO
                Assert(false, "ensure_of_type_registration() : set types registration not yet implemented.");
                wait_lock_type_registration(pTCContext->pProgCompilationState);
                unlock_type_registration(pTCContext->pProgCompilationState);
            } break;

            default:
                Assert(false, "ensure_of_type_registration() : this type kind should have been registered right away...");
        }
    }
}
*/

local_func u64 declare_user_proc_sign_format(const TypeInfo_ProcLike* pType, TCContext* pTCContext)
{
    // We'll do something a little special for runtime type info of procsigns
    // The base typeinfo is like every other type: salvaging the 3x64b of pType->crossRefIR for 64b NYKA of compact string for identifier, 64b NamespaceId (?), 64b reserved.
    // However, the 'params' array will be its own thing for RTTI:
    // 8b count of total params ; 8b count of params with default ; 16b reserved1 ; 32b reserved2, then list of params (and possible list of default values) as such:
    // each param (either in or out) entry is 64b NYKA of identifier (possibly empty) + 64b runtime type id
    // each possible param with default is 64b NYKA of a declaration ; TODO: think about possible non-const evaluables there ????
    SourceFileDescAndState* pSourceFile = pTCContext->pIsolatedSourceFile;
    Arena tmpArena = pTCContext->pWorker->tmpArena;

    u8 uTotalParamsCount = u8(pType->params.size());
    u8 uParamsWithDefaultsCount = 0u; // TODO ?
    u32 uRTTIByteSize = sizeof(TypeInfo_UserBase) + 8u + 16u*uTotalParamsCount + 8u*uParamsWithDefaultsCount;
    u8* pRTTIData = alloc_from(pSourceFile->localArena, uRTTIByteSize, 8u);  // All allocs corresponding to runtime data are 64b aligned
    // And we begin by copying all comptime of a userbase to runtime.
    memcpy(pRTTIData, (u8*)pType, sizeof(TypeInfo_UserBase));
    // And then the various counts
    u8* pToTotalParamsCount = (u8*)(pRTTIData + sizeof(TypeInfo_UserBase));
    u8* pToParamsWithDefaultsCount = pToTotalParamsCount + 1;
    u16* pToReserved1 = (u16*)(((u8*)pToTotalParamsCount) + 2u);
    u32* pToReserved2 = (u32*)(((u8*)pToTotalParamsCount) + 4u);
    *pToTotalParamsCount = uTotalParamsCount;
    *pToParamsWithDefaultsCount = uParamsWithDefaultsCount;
    *pToReserved1 = 0u; // TODO ?
    *pToReserved2 = 0u; // TODO ?
    u32 uNykasCount = 3u + 2u*uTotalParamsCount + 1u*uParamsWithDefaultsCount; // identifier, namespace, reserved, per param:id and typeid ; per param ith default:default decl (?)
    ArenaRefPoint beforeTmp = get_arena_ref_point(tmpArena);
    u32* pNykasOffsets = (u32*)alloc_from(tmpArena, sizeof(u32)*uNykasCount, alignof(u32));
    constexpr u32 asNodeOffset = offsetof(TypeInfo_ProcLike, asNode);
    pNykasOffsets[0] = asNodeOffset;
    pNykasOffsets[1] = asNodeOffset+8u;
    pNykasOffsets[2] = asNodeOffset+16u;
    *(u64*)(pRTTIData + pNykasOffsets[0]) = 0uLL; // TODO: identifier
    *(u64*)(pRTTIData + pNykasOffsets[1]) = 0uLL; // TODO: namespace
    *(u64*)(pRTTIData + pNykasOffsets[2]) = 0uLL; // TODO: reserved
    constexpr u32 uStartOfParamsIndex = 3u;
    constexpr u32 uStartOfParamsOffset = sizeof(TypeInfo_UserBase) + 8u;
    for (u8 uParam = 0u; uParam < uTotalParamsCount; uParam++) {
        u32 uOffsetOfIdentifierNyka = uStartOfParamsOffset + uParam*16u;
        u32 uOffsetOfTypeId = uOffsetOfIdentifierNyka + 8u;
        pNykasOffsets[uStartOfParamsIndex + uParam*2u] = uOffsetOfIdentifierNyka;
        pNykasOffsets[uStartOfParamsIndex + 1u + uParam*2u] = uOffsetOfTypeId;
        *(u64*)(pRTTIData + uOffsetOfIdentifierNyka) = 0uLL; // TODO: identifier
        const TypeInfo* pMemberType = pType->params[uParam].pBinding->pType;

            // TMP TMP TMP
        if (!ir_is_valid_param_(pMemberType->asNode.info.uIRandMetaFlags)) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "*** Warning : declare_user_proc_sign_format() : not yet declared param type, nyka to RTTI set to 0"), pTCContext->pWorker);
            *(u64*)(pRTTIData + uOffsetOfTypeId) = 0uLL;
        } else {
            // TMP TMP TMP
            
            // TODO !!
            // ensure_of_type_registration(pMemberType, pTCContext);
            u64 uIRofMemberType = get_type_rtti_decl_ir(pMemberType);
            Assert_(ir_is_valid_param(uIRofMemberType));
            u64 uNykaMemberType = ir_make_direct_nyka_value(uIRofMemberType);
            *(u64*)(pRTTIData + uOffsetOfTypeId) = uNykaMemberType; // Member Type

            // TMP TMP TMP
        }
            // TMP TMP TMP
    }
    // TODO: params with defaults

    u64 uResult = make_and_register_rtti(&(pSourceFile->filewiseConstRepo), pSourceFile->localArena, &(pSourceFile->mapRuntimeTypeIdToTCTypeInfo),
                                         pType, pRTTIData, uRTTIByteSize, pNykasOffsets, uNykasCount);
    reset_arena_no_release_to(beforeTmp, tmpArena);
    return uResult;
}

// full bitset legs on pos 0 to 15 ; full unset legs on pos 16 to 31
constexpr u64 tTableFull[32] =              { 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0x7FFF'FFFF'FFFF'FFFFuLL, 
                                              /* following 16 are zeroes */
                                            };

// an u64 leg as 0x7FFF'FFFF'FFFF'FFFFuLL at pos 15 ; full bitset legs before that ; full unset legs after that
constexpr u64 tTableFullWithSignUnset[32] = { 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0x7FFF'FFFF'FFFF'FFFFuLL, 
                                              /* following 16 are zeroes */
                                            };

// an u64 leg as 0x8000'0000'0000'0000uLL at pos 15 ; full unset legs before that ; full bitset legs after that
constexpr u64 tTableFullInverted[32] =      { 0, 0, 0, 0,
                                              0, 0, 0, 0,
                                              0, 0, 0, 0,
                                              0, 0, 0, 0x8000'0000'0000'0000uLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL,
                                              0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 0xFFFF'FFFF'FFFF'FFFFuLL, 
                                            };

// a first u64 leg as 0x0000'0000'0000'0001uLL ; 15 full unset legs after that
constexpr u64 tTableOne[16] = { 1uLL, /* */ };

// a first u64 leg as max codepoint on 64b ; 15 full unset legs after that
constexpr u64 tTableMaxCodepoint[16] = { u64(UNICODE_CODEPOINT_MAX), /* */ };

global_var u64 g_uIRofDeclFullSet1024bAndZeroes1024b;
global_var u64 g_uIRofDeclFullSetWithSignUnset1024bAndZeroes1024b;
global_var u64 g_uIRofDeclFullUnsetWithSignSet1024bAndThenFullSet1024b;
global_var u64 g_uIRofDeclOneBitSetThenAllZeroes1024b;
global_var u64 g_uIRofDeclMaxCodepointThenAllZeroes1024b;

global_var IRInfo g_infoAddressOfZero1024b;
global_var IRInfo g_infoAddressOfOnlySetBits1024b;
global_var IRInfo g_infoAddressOfSingleBit1024b;
global_var IRInfo g_infoAddressOfMaxCodepoint1024b;

local_func TypeInfo_Integral* make_int_format_(ECoreType eCoreType, u8 uLog2OfByteCount, ESignedness eSignedness, WholeProgramCompilationState* pCompState)
{
    TypeInfo_Integral* pResult;
    i32 iOffsetWithinTwice1024bGivenByteCount = 16*8 - (1 << uLog2OfByteCount);
    if (eSignedness == ESignedness::ESIGNED) {
        u64 uNykaOfMaxSigned = ir_make_nyka_value(g_uIRofDeclFullSetWithSignUnset1024bAndZeroes1024b, iOffsetWithinTwice1024bGivenByteCount);
        u64 uNykaOfMinSigned = ir_make_nyka_value(g_uIRofDeclFullUnsetWithSignSet1024bAndThenFullSet1024b, iOffsetWithinTwice1024bGivenByteCount);
        pResult = make_int_format_impl(eCoreType, uLog2OfByteCount, ESignedness::ESIGNED, pCompState,
                             uNykaOfMaxSigned, uNykaOfMinSigned);
    } else {
        u64 uNykaOfMaxUnsigned = ir_make_nyka_value(g_uIRofDeclFullSet1024bAndZeroes1024b, iOffsetWithinTwice1024bGivenByteCount);
        pResult = make_int_format_impl(eCoreType, uLog2OfByteCount, eSignedness, pCompState,
                             uNykaOfMaxUnsigned, g_infoAddressOfZero1024b.metaValue.knownValue.uEmbeddedValue);
    }
    return pResult;
}

local_func TypeInfo_Integral* make_int_format(ECoreType eCoreType, u8 uLog2OfByteCount, ESignedness eSignedness, WholeProgramCompilationState* pCompState)
{
    TypeInfo_Integral* pResult = make_int_format_(eCoreType, uLog2OfByteCount, eSignedness, pCompState);
    pResult->asNode.info.uIRandMetaFlags = declare_core_int_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_Integral* make_special_int_format(ECoreType eCoreType, ESignedness eSignedness, WholeProgramCompilationState* pCompState)
{
    TypeInfo_Integral* pResult = make_int_format_(eCoreType, 0x02u, eSignedness, pCompState);
    pResult->_coreFlags |= 0x80u;
    pResult->asNode.info.uIRandMetaFlags = declare_core_int_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_Integral* make_special_compint_format(WholeProgramCompilationState* pCompState) {
    TypeInfo_Integral* pResult = (TypeInfo_Integral*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_Integral), alignof(TypeInfo_Integral));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_INTEGRAL;
    pResult->_coreType = ECoreType::ECORETYPE_COMPINT;
    pResult->_coreFlags = u8(ESignedness::ESIGNED);
    pResult->uIRFormat = COMPINT_PSEUDO_IR_FORMAT;
    pResult->uSlotsAndAlign = (u32(3u) << 28) | 1u;
    pResult->uNykaOfMax = 0;
    pResult->uNykaOfMin = 0;
    pResult->asNode.info.uIRandMetaFlags = declare_core_int_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}


local_func TypeInfo_Integral* make_special_bool_format(WholeProgramCompilationState* pCompState)
{
    TypeInfo_Integral* pResult = make_int_format_impl(ECoreType::ECORETYPE_BOOL, 0x00u, ESignedness::ESIGN_AGNOSTIC, pCompState,
                                                      g_infoAddressOfSingleBit1024b.metaValue.knownValue.uEmbeddedValue,
                                                      g_infoAddressOfZero1024b.metaValue.knownValue.uEmbeddedValue);
    pResult->asNode.info.uIRandMetaFlags = declare_core_int_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_Integral* make_special_codepoint_format(WholeProgramCompilationState* pCompState)
{
    TypeInfo_Integral* pResult = make_int_format_impl(ECoreType::ECORETYPE_CODEPOINT, 0x02u, ESignedness::ESIGN_AGNOSTIC, pCompState,
                                                      g_infoAddressOfMaxCodepoint1024b.metaValue.knownValue.uEmbeddedValue,
                                                      g_infoAddressOfZero1024b.metaValue.knownValue.uEmbeddedValue);
    pResult->asNode.info.uIRandMetaFlags = declare_core_int_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_OtherCore* make_simple_othercore_format(ECoreType eCoreType, u8 uIRFormat, WholeProgramCompilationState* pCompState)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = eCoreType;
    pResult->_coreFlags = 0;
    pResult->pAsRuntimeStruct = 0;
    pResult->uIRFormat = uIRFormat;
    pResult->uSlotsAndAlign = (get_log2_of_natural_align_from_format(uIRFormat) << 28) | 1u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = (eCoreType == ECORETYPE_TYPE) ? pResult : g_pCoreTypesInfo[ECORETYPE_TYPE]; // 'type' is first to be declared, and must point to self
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_OtherCore* make_special_void_format(WholeProgramCompilationState* pCompState)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = ECoreType::ECORETYPE_VOID;
    pResult->_coreFlags = 0;
    pResult->pAsRuntimeStruct = 0;
    pResult->uIRFormat = 0u;
    pResult->uSlotsAndAlign = 0u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_FloatingPoint* make_fp_format(ECoreType eCoreType, u8 uFPAboveF16, WholeProgramCompilationState* pCompState)
{
    TypeInfo_FloatingPoint* pResult = (TypeInfo_FloatingPoint*)alloc_from(pCompState->globalArena,
        sizeof(TypeInfo_FloatingPoint), alignof(TypeInfo_FloatingPoint));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_FLOATINGPOINT;
    pResult->_coreType = eCoreType;
    pResult->_coreFlags = fp_exp_bits(uFPAboveF16);
    u8 uLog2OfByteCount = uFPAboveF16+1u;
    pResult->uIRFormat = 0x08u | uLog2OfByteCount;
    pResult->uSlotsAndAlign = (u32(uLog2OfByteCount) << 28) | 1u;
    pResult->asNode.info.uIRandMetaFlags = declare_core_fp_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_FloatingPoint* make_special_float_format(ECoreType eCoreType, WholeProgramCompilationState* pCompState) {
    Assert_(eCoreType == ECoreType::ECORETYPE_FLOAT_LIT || eCoreType == ECoreType::ECORETYPE_XFLOAT);
    TypeInfo_FloatingPoint* pResult = (TypeInfo_FloatingPoint*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_FloatingPoint), alignof(TypeInfo_FloatingPoint));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_FLOATINGPOINT;
    pResult->_coreType = eCoreType;
    pResult->_coreFlags = 32u;
    pResult->uIRFormat = XFLOAT_PSEUDO_IR_FORMAT;
    pResult->uSlotsAndAlign = (u32(6u) << 28) | 1u;
    pResult->asNode.info.uIRandMetaFlags = declare_core_fp_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_HwVector* make_vec_int_format(ECoreType eCoreType, u8 uVecCount, u8 uLog2OfByteCount, ESignedness eSignedness,
    WholeProgramCompilationState* pCompState)
{
    TypeInfo_HwVector* pResult = (TypeInfo_HwVector*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_HwVector), alignof(TypeInfo_HwVector));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_HWVECTOR;
    pResult->_coreType = eCoreType;
    pResult->_coreFlags = u8(eSignedness);
    pResult->uIRFormat = uLog2OfByteCount | (uVecCount << 4);
    pResult->uSlotsAndAlign = (u32(uLog2OfByteCount+uVecCount) << 28) | 1u;
    ECoreType eBaseIntegral = (eSignedness == ESignedness::ESIGN_AGNOSTIC) ? ECORETYPE_R8 :
        ((eSignedness == ESignedness::EUNSIGNED) ? ECORETYPE_U8 : ECORETYPE_I8);
    pResult->pElementType = g_pCoreTypesInfo[eBaseIntegral+uLog2OfByteCount];
    pResult->asNode.info.uIRandMetaFlags = declare_core_vec_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_HwVector* make_vec_fp_format(ECoreType eCoreType, u8 uVecCount, u8 uFPAboveF16,
    WholeProgramCompilationState* pCompState)
{
    TypeInfo_HwVector* pResult = (TypeInfo_HwVector*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_HwVector), alignof(TypeInfo_HwVector));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_HWVECTOR;
    pResult->_coreType = eCoreType;
    pResult->_coreFlags = 0x3u;
    u8 uLog2OfByteCount = uFPAboveF16+1u;
    pResult->uIRFormat = 0x08u | uLog2OfByteCount | (uVecCount << 4);
    pResult->uSlotsAndAlign = (u32(uLog2OfByteCount+uVecCount) << 28) | 1u;
    pResult->pElementType = g_pCoreTypesInfo[ECORETYPE_F16+uFPAboveF16];
    pResult->asNode.info.uIRandMetaFlags = declare_core_vec_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_StructLike* make_core_structlike(WholeProgramCompilationState* pCompState, Arena tmpArena, int iIdentifier,
                                                     ArrayView<ValueBinding*> vecFields, u32 uUnalignedByteSize, u8 uFormat, u32 uSlotsAndAlign)
{
    TypeInfo_StructLike* pResult = (TypeInfo_StructLike*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_StructLike), alignof(TypeInfo_StructLike));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_STRUCTLIKE;
    pResult->_coreType = COMPOUNDTYPE_IS_STRUCT;
    pResult->_coreFlags = 0;
    u32 uFieldsCount = vecFields.size();
    Assert_(uFieldsCount > 0);
    Assert_(uFieldsCount < 0x1'0000u);
    pResult->uRuntimeMemberCount = u16(uFieldsCount);
    pResult->uUnalignedByteSize = uUnalignedByteSize;
    pResult->uRegistrationIndex = 0xFFFFu; // invalid 'registration' cross-indexing for core structlike
    pResult->vecAllMembers.init(pCompState->globalArena);
    pResult->vecAllMembers.append_all(vecFields);
    pResult->mapAllMembers.init(pCompState->globalArena);
    for (u32 uFieldPos = 0; uFieldPos < uFieldsCount; uFieldPos++) {
        ValueBinding* pBinding = vecFields[uFieldPos];
        pResult->mapAllMembers.insert(pBinding->iIdentifierHandle, uFieldPos);
    }
    pResult->uIRFormat = uFormat;
    pResult->uSlotsAndAlign = uSlotsAndAlign;
    pResult->pRegistration = (TCCompoundRegistration*)alloc_from(pCompState->globalArena, sizeof(TCCompoundRegistration), alignof(TCCompoundRegistration));
    pResult->pRegistration->iPrimaryIdentifier = iIdentifier;
    pResult->pRegistration->pCompoundType = pResult;
    pResult->pRegistration->setWaitingConstOnly.init(pCompState->globalArena);
    pResult->pRegistration->setWaitingPossiblyRuntime.init(pCompState->globalArena);
    pResult->pRegistration->pRootTcBlock = 0; // invalid 'registration' cross-indexing for core structlike
    pResult->pRegistration->pStatementWithSignature = 0; // invalid 'registration' cross-indexing for core structlike
    pResult->pRegistration->uTCProgress = ECompoundTCProgress::ECOMPOUND_DONE_ALL; // starting progress already 'done' for core structlike
    pResult->asNode.info.uIRandMetaFlags = declare_core_structlike_format(pResult, pCompState, tmpArena);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_OtherCore* make_special_compact_string_format(WholeProgramCompilationState* pCompState)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = ECoreType::ECORETYPE_COMPACT_STRING;
    pResult->_coreFlags = OTHERCOREFLAG_IS_STRING|STRINGFLAG_IS_COMPACT;
    pResult->pAsRuntimeStruct = 0;
    pResult->uIRFormat = 0x03u;
    pResult->uSlotsAndAlign = 0x3000'0001u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func TypeInfo_OtherCore* make_special_compact_ownedstring_format(WholeProgramCompilationState* pCompState)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = ECoreType::ECORETYPE_COMPACT_OWNEDSTRING;
    pResult->_coreFlags = OTHERCOREFLAG_IS_STRING|STRINGFLAG_IS_COMPACT|STRINGFLAG_HAS_ALLOC;
    pResult->pAsRuntimeStruct = 0;
    pResult->uIRFormat = 0x03u;
    pResult->uSlotsAndAlign = 0x3000'0001u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

// TODO: CLEANUP: this should probably go away (reorgs core types and 'implicit structlike' concept)
local_func ValueBinding* make_core_field_of_structlike_impl(int iIdentifierHandle, TypeInfo* pType, u32 uIndexInStruct, u32 uByteOffsetInStruct,
                                              WholeProgramCompilationState* pCompState)
{
    ValueBinding* pResult = (ValueBinding*)alloc_from(pCompState->globalArena, sizeof(ValueBinding), alignof(ValueBinding));
    pResult->pType = pType;
    pResult->info.uIRandMetaFlags = uByteOffsetInStruct; // 'IR' slot salvaged for bytewise offset in bindings representing fields of struct
    pResult->info.metaValue._payload = 0uLL;
    pResult->iIdentifierHandle = iIdentifierHandle;
    pResult->uScopeAndLocation = EScopeKind::SCOPEKIND_COMPOUND | (uIndexInStruct << 8);
    pResult->sourceRef = BindingSourceRefs{}; // dummy ref
    pResult->sourceRef.iSourceFile = -1;      // no source file per se
    return pResult;
}

// TODO: CLEANUP: this should probably go away (reorgs core types and 'implicit structlike' concept)
local_func TypeInfo_OtherCore* make_special_stringview_format(WholeProgramCompilationState* pCompState, Arena tmpArena)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = ECoreType::ECORETYPE_STRINGVIEW;
    pResult->_coreFlags = OTHERCOREFLAG_IS_STRING;
    TmpStackOptiArray<ValueBinding*, 24> vecFields(pCompState->globalArena);
    ValueBinding* bytes = make_core_field_of_structlike_impl(pCompState->get_or_make_id("bytes"),
        g_pCoreTypesInfo[ECORETYPE_RAWPTR], 0u, 0u, pCompState);
    ValueBinding* byte_length = make_core_field_of_structlike_impl(pCompState->get_or_make_id("byte_length"),
        g_pCoreTypesInfo[ECORETYPE_U32], 1u, 8u, pCompState);
    ValueBinding* flags = make_core_field_of_structlike_impl(pCompState->get_or_make_id("flags"),
        g_pCoreTypesInfo[ECORETYPE_U32], 2u, 12u, pCompState);
    vecFields.append(bytes);
    vecFields.append(byte_length);
    vecFields.append(flags);
    pResult->pAsRuntimeStruct = make_core_structlike(pCompState, tmpArena, pCompState->get_or_make_id("#stringview_impl"), vecFields, 16u,
                                                     0x03u, 0x3000'0002u);
    pResult->uIRFormat = 0x03u;
    pResult->uSlotsAndAlign = 0x3000'0002u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

// TODO: CLEANUP: this should probably go away (reorgs core types and 'implicit structlike' concept)
local_func TypeInfo_OtherCore* make_special_ownedstring_format(WholeProgramCompilationState* pCompState, Arena tmpArena)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = ECoreType::ECORETYPE_OWNEDSTRING;
    pResult->_coreFlags = OTHERCOREFLAG_IS_STRING|STRINGFLAG_HAS_ALLOC;
    TmpStackOptiArray<ValueBinding*, 40> vecFields(pCompState->globalArena);
    ValueBinding* bytes = make_core_field_of_structlike_impl(pCompState->get_or_make_id("bytes"),
        g_pCoreTypesInfo[ECORETYPE_RAWPTR], 0u, 0u, pCompState);
    ValueBinding* byte_length = make_core_field_of_structlike_impl(pCompState->get_or_make_id("byte_length"),
        g_pCoreTypesInfo[ECORETYPE_U32], 1u, 8u, pCompState);
    ValueBinding* flags = make_core_field_of_structlike_impl(pCompState->get_or_make_id("flags"),
        g_pCoreTypesInfo[ECORETYPE_U32], 2u, 12u, pCompState);
    ValueBinding* allocFn = make_core_field_of_structlike_impl(pCompState->get_or_make_id("allocFn"),
        g_pCoreTypesInfo[ECORETYPE_RAWPTR], 3u, 16u, pCompState);
    ValueBinding* allocData = make_core_field_of_structlike_impl(pCompState->get_or_make_id("allocData"),
        g_pCoreTypesInfo[ECORETYPE_RAWPTR], 4u, 24u, pCompState);
    vecFields.append(bytes);
    vecFields.append(byte_length);
    vecFields.append(flags);
    vecFields.append(allocFn);
    vecFields.append(allocData);
    pResult->pAsRuntimeStruct = make_core_structlike(pCompState, tmpArena, pCompState->get_or_make_id("#ownedstring_impl"), vecFields, 32u,
                                                     0x03u, 0x3000'0004u);
    pResult->uIRFormat = 0x03u;
    pResult->uSlotsAndAlign = 0x3000'0004u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

// TODO: CLEANUP: this should probably go away (reorgs core types and 'implicit structlike' concept)
local_func TypeInfo_OtherCore* make_special_any_format(WholeProgramCompilationState* pCompState, Arena tmpArena)
{
    TypeInfo_OtherCore* pResult = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pResult->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pResult->_coreType = ECoreType::ECORETYPE_ANY;
    pResult->_coreFlags = 0;
    TmpStackOptiArray<ValueBinding*, 16> vecFields(tmpArena);
    ValueBinding* type_id = make_core_field_of_structlike_impl(pCompState->get_or_make_id("type_id"),
        g_pCoreTypesInfo[ECORETYPE_TYPE_ID], 0u, 0u, pCompState);
    ValueBinding* opaque_value = make_core_field_of_structlike_impl(pCompState->get_or_make_id("opaque_value"),
        g_pCoreTypesInfo[ECORETYPE_R64], 1u, 8u, pCompState);
    vecFields.append(type_id);
    vecFields.append(opaque_value);
    pResult->pAsRuntimeStruct = make_core_structlike(pCompState, tmpArena, pCompState->get_or_make_id("#any_impl"), vecFields, 16u,
                                                     0x03u, 0x3000'0002u);
    pResult->uIRFormat = 0x03u;
    pResult->uSlotsAndAlign = 0x3000'0002u;
    pResult->asNode.info.uIRandMetaFlags = declare_other_core_format(pResult, pCompState);
    pResult->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
    pResult->asNode.info.metaValue.knownValue.pType = pResult;
    return pResult;
}

local_func void init_type_info_for_core_types(WholeProgramCompilationState* pCompState, WorkerDesc* pWorker)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Initializing Core Types"), pWorker);

    IRRepo* pRepo = &(pCompState->programwiseRepo);
    init_ir_repo(pRepo, 0u, pCompState->globalArena);

    // an instruction at offset #0 in first programwise repo would have an encoding (as a standard IR param) of all-0s
    // => We setup a 'noop' instruction right there, which no-one should ever refer to,
    //    to conveniently have IR code '0' represent a not-yet-set or invalid IR.
    // (Note that a 'nyka' to that invalid pos would also be 0)
    IREntry* pFirstEntryCode0ForInvalid = ir_append_new_entry(pRepo);
    Assert_(pRepo->uSize == 1u);
    pFirstEntryCode0ForInvalid->uInstrCodeAndFormatAndFirstParam = u64(IRInstructionType::IRIT_NO_OP);
    pFirstEntryCode0ForInvalid->uInstrMetaFlagsAndSecondParam = 0uLL;
    pFirstEntryCode0ForInvalid->metaValue = MetaValueIR{};

    u32 uPosOfDeclFullSet1024bAndZeroes1024b = ir_make_non_embd_decl_without_nykas(pRepo, 0u, IRFLAG_IS_KNOWN,
        (u8*)tTableFull, 0x03u, sizeof(tTableFull), 3u);
    g_uIRofDeclFullSet1024bAndZeroes1024b = ir_make_std_code_programwise_global(uPosOfDeclFullSet1024bAndZeroes1024b);

    u32 uPosOfDeclFullSetWithSignUnset1024bAndZeroes1024b = ir_make_non_embd_decl_without_nykas(pRepo, 0u, IRFLAG_IS_KNOWN,
        (u8*)tTableFullWithSignUnset, 0x03u, sizeof(tTableFullWithSignUnset), 3u);
    g_uIRofDeclFullSetWithSignUnset1024bAndZeroes1024b = ir_make_std_code_programwise_global(uPosOfDeclFullSetWithSignUnset1024bAndZeroes1024b);

    u32 uPosOfDeclFullUnsetWithSignSet1024bAndThenFullSet1024b = ir_make_non_embd_decl_without_nykas(pRepo, 0u, IRFLAG_IS_KNOWN,
        (u8*)tTableFullInverted, 0x03u, sizeof(tTableFullInverted), 3u);
    g_uIRofDeclFullUnsetWithSignSet1024bAndThenFullSet1024b = ir_make_std_code_programwise_global(uPosOfDeclFullUnsetWithSignSet1024bAndThenFullSet1024b);

    u32 uPosOfDeclOneBitSetThenAllZeroes1024b = ir_make_non_embd_decl_without_nykas(pRepo, 0u, IRFLAG_IS_KNOWN,
        (u8*)tTableOne, 0x03u, sizeof(tTableOne), 3u);
    g_uIRofDeclOneBitSetThenAllZeroes1024b = ir_make_std_code_programwise_global(uPosOfDeclOneBitSetThenAllZeroes1024b);

    u32 uPosOfDeclMaxCodepointThenAllZeroes1024b = ir_make_non_embd_decl_without_nykas(pRepo, 0u, IRFLAG_IS_KNOWN,
        (u8*)tTableMaxCodepoint, 0x03u, sizeof(tTableMaxCodepoint), 3u);
    g_uIRofDeclMaxCodepointThenAllZeroes1024b = ir_make_std_code_programwise_global(uPosOfDeclMaxCodepointThenAllZeroes1024b);

    constexpr u32 uFlagsToGlobalNykas = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA;
    u64 uNykaValueToZero1024b = ir_make_nyka_value(g_uIRofDeclFullSet1024bAndZeroes1024b, 16 * 8);
    u32 uDeclAddressOfZero = ir_make_decl_entry(pRepo, 0u, uFlagsToGlobalNykas, uNykaValueToZero1024b, 0x03u, 8u, 3u);
    g_infoAddressOfZero1024b = IRInfo { ir_make_std_code_programwise_global(uDeclAddressOfZero)|u64(uFlagsToGlobalNykas),
                                        uNykaValueToZero1024b };
    g_infoAddressOfOnlySetBits1024b = IRInfo { ir_make_nyka_immediate(g_uIRofDeclFullSet1024bAndZeroes1024b)|u64(uFlagsToGlobalNykas),
                                        ir_make_nyka_value(g_uIRofDeclFullSet1024bAndZeroes1024b, 0) };
    g_infoAddressOfSingleBit1024b = IRInfo { ir_make_nyka_immediate(g_uIRofDeclOneBitSetThenAllZeroes1024b)|u64(uFlagsToGlobalNykas),
                                        ir_make_nyka_value(g_uIRofDeclOneBitSetThenAllZeroes1024b, 0) };
    g_infoAddressOfMaxCodepoint1024b = IRInfo { ir_make_nyka_immediate(g_uIRofDeclMaxCodepointThenAllZeroes1024b)|u64(uFlagsToGlobalNykas),
                                        ir_make_nyka_value(g_uIRofDeclMaxCodepointThenAllZeroes1024b, 0) };

    // 'ECORETYPE_TYPE' is first to be declared, and must point to self
    TypeInfo_OtherCore* pTypeOfTypes = (TypeInfo_OtherCore*)alloc_from(pCompState->globalArena, sizeof(TypeInfo_OtherCore), alignof(TypeInfo_OtherCore));
    pTypeOfTypes->uTypeKind = ETypeKind::ETYPEKIND_OTHERCORE;
    pTypeOfTypes->_coreType = ECoreType::ECORETYPE_TYPE;
    pTypeOfTypes->_coreFlags = 0;
    pTypeOfTypes->pAsRuntimeStruct = 0;
    pTypeOfTypes->uIRFormat = 0x03u;
    pTypeOfTypes->uSlotsAndAlign = (3u << 28) | 1u;
    pTypeOfTypes->asNode.info.uIRandMetaFlags = declare_other_core_format(pTypeOfTypes, pCompState);
    pTypeOfTypes->asNode.pType = pTypeOfTypes;
    pTypeOfTypes->asNode.info.metaValue.knownValue.pType = pTypeOfTypes;
    g_pCoreTypesInfo[ECORETYPE_TYPE] = pTypeOfTypes;

    // Specials

    g_pCoreTypesInfo[ECORETYPE_INT] = make_special_int_format(ECORETYPE_INT, ESignedness::ESIGNED, pCompState);
    g_pCoreTypesInfo[ECORETYPE_NAT] = make_special_int_format(ECORETYPE_NAT, ESignedness::EUNSIGNED, pCompState);
    g_pCoreTypesInfo[ECORETYPE_BOOL] = make_special_bool_format(pCompState);
    g_pCoreTypesInfo[ECORETYPE_CODEPOINT] = make_special_codepoint_format(pCompState);
    g_pCoreTypesInfo[ECORETYPE_RAWPTR] = make_int_format(ECORETYPE_RAWPTR, 3u, ESignedness::ESIGN_AGNOSTIC, pCompState);
//    g_pCoreTypesInfo[ECORETYPE_NOTYETTYPED_ADDR] = make_int_format(ECORETYPE_NOTYETTYPED_ADDR, 3u, ESignedness::ESIGN_AGNOSTIC, pCompState);
    g_pCoreTypesInfo[ECORETYPE_COMPINT] = make_special_compint_format(pCompState);
    g_pCoreTypesInfo[ECORETYPE_XFLOAT] = make_special_float_format(ECORETYPE_XFLOAT, pCompState);
    g_pCoreTypesInfo[ECORETYPE_FLOAT_LIT] = make_special_float_format(ECORETYPE_FLOAT_LIT, pCompState);
    g_pCoreTypesInfo[ECORETYPE_VOID] = make_special_void_format(pCompState);

    // Comptime

    g_pCoreTypesInfo[ECORETYPE_TYPE_ID] = make_simple_othercore_format(ECORETYPE_TYPE_ID, 0x03u, pCompState);
    g_pCoreTypesInfo[ECORETYPE_NAMESPACE] = make_simple_othercore_format(ECORETYPE_NAMESPACE, 0x03u, pCompState);
    g_pCoreTypesInfo[ECORETYPE_ASTNODE] = make_simple_othercore_format(ECORETYPE_ASTNODE, 0x03u, pCompState);
    g_pCoreTypesInfo[ECORETYPE_ASTSEQ] = make_simple_othercore_format(ECORETYPE_ASTSEQ, 0x03u, pCompState);
    g_pCoreTypesInfo[ECORETYPE_SOURCEFILE] = make_simple_othercore_format(ECORETYPE_SOURCEFILE, 0x01u, pCompState);
    g_pCoreTypesInfo[ECORETYPE_FOREIGN_SOURCE] = make_simple_othercore_format(ECORETYPE_FOREIGN_SOURCE, 0x03u, pCompState);
    g_pCoreTypesInfo[ECORETYPE_BUILTIN] = make_simple_othercore_format(ECORETYPE_BUILTIN, 0x03u, pCompState);

    // Integral and FP

    for (u8 uIntFormat = 0x00u; uIntFormat <= 0x07u; uIntFormat++) {
        g_pCoreTypesInfo[ECORETYPE_R8 + uIntFormat] = make_int_format(ECoreType(ECORETYPE_R8 + uIntFormat), uIntFormat,
            ESignedness::ESIGN_AGNOSTIC, pCompState);
    }
    for (u8 uIntFormat = 0x00u; uIntFormat <= 0x05u; uIntFormat++) {
        g_pCoreTypesInfo[ECORETYPE_U8 + uIntFormat] = make_int_format(ECoreType(ECORETYPE_U8 + uIntFormat), uIntFormat,
            ESignedness::EUNSIGNED, pCompState);
    }
    for (u8 uIntFormat = 0x00u; uIntFormat <= 0x05u; uIntFormat++) {
        g_pCoreTypesInfo[ECORETYPE_I8 + uIntFormat] = make_int_format(ECoreType(ECORETYPE_I8 + uIntFormat), uIntFormat,
            ESignedness::ESIGNED, pCompState);
    }
    for (u8 uFPAboveF16 = 0u; uFPAboveF16 < 5u; uFPAboveF16++) {
        g_pCoreTypesInfo[ECORETYPE_F16 + uFPAboveF16] = make_fp_format(ECoreType(ECORETYPE_F16 + uFPAboveF16), uFPAboveF16, pCompState);
    }

    // Hardware vecors

    u8 uPosAboveV2R8 = 0u;
    for (u8 uVecCount = 1u; uVecCount <= 7u; uVecCount++) {
        u8 uMaxInt = _min(u8(7u-uVecCount), u8(0x05u));
        u8 uMaxFP = _min(u8(7u-uVecCount), u8(5u));
        for (u8 uFormat = 0x00u; uFormat <= 0x05u; uFormat++) {
            if (uFormat <= uMaxInt) {
                g_pCoreTypesInfo[ECORETYPE_R8x2 + uPosAboveV2R8] = make_vec_int_format(ECoreType(ECORETYPE_R8x2 + uPosAboveV2R8),
                    uVecCount, uFormat, ESignedness::ESIGN_AGNOSTIC, pCompState);
                uPosAboveV2R8++;
                g_pCoreTypesInfo[ECORETYPE_R8x2 + uPosAboveV2R8] = make_vec_int_format(ECoreType(ECORETYPE_R8x2 + uPosAboveV2R8),
                    uVecCount, uFormat, ESignedness::EUNSIGNED, pCompState);
                uPosAboveV2R8++;
                g_pCoreTypesInfo[ECORETYPE_R8x2 + uPosAboveV2R8] = make_vec_int_format(ECoreType(ECORETYPE_R8x2 + uPosAboveV2R8),
                    uVecCount, uFormat, ESignedness::ESIGNED, pCompState);
                uPosAboveV2R8++;
            }
            if (uFormat && uFormat <= uMaxFP) {
                g_pCoreTypesInfo[ECORETYPE_R8x2 + uPosAboveV2R8] = make_vec_fp_format(ECoreType(ECORETYPE_R8x2 + uPosAboveV2R8),
                    uVecCount, uFormat-1u, pCompState);
                uPosAboveV2R8++;
            }
        }
    }

    // Special for Any (after type-id and R64)

    g_pCoreTypesInfo[ECORETYPE_ANY] = make_special_any_format(pCompState, pWorker->tmpArena);

    // String-related

    g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING] = make_special_compact_string_format(pCompState);
    g_pCoreTypesInfo[ECORETYPE_STRINGVIEW] = make_special_stringview_format(pCompState, pWorker->tmpArena);
    g_pCoreTypesInfo[ECORETYPE_OWNEDSTRING] = make_special_ownedstring_format(pCompState, pWorker->tmpArena);
    g_pCoreTypesInfo[ECORETYPE_COMPACT_OWNEDSTRING] = make_special_compact_ownedstring_format(pCompState);
}


#endif // LOCLIB_TYPE_REGISTRATION_H_

