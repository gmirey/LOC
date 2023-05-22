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

#ifndef LOCLIB_TYPE_CHECKER_BASE_H_
#define LOCLIB_TYPE_CHECKER_BASE_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/compiler_dependent_msvc.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_Token.h"
#include "LocLib_ScanAndTok.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_ProgramState.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_DebugPrint.h"
#include "LocLib_PreParser.h"
#include "LocLib_Postparser.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_IR_Types.h"
#include "LocLib_IR.h"
#include "LocLib_TypeCheckerCore.h"
#include "LocLib_IR_SolverInterface.h"


// Helper function for the bubble sort in 'try_finalize_structlike_runtime_tc', below
local_func_inl void do_swap_value_members(TypeInfo_StructLike* ioToInit, u32 uIndex, u32 uPrevIndex,
    ValueBinding* pCurrentBinding, ValueBinding* pPrevBinding, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Swapping Current %u with Prev %u",
        u64(uIndex), u64(uPrevIndex)),
        pTCContext->pWorker);

    int iIdentifierCurrent = pCurrentBinding->iIdentifierHandle;
    int iIdentifierPrev = pPrevBinding->iIdentifierHandle;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Identified as Current '%s' and Prev '%s'",
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierCurrent).c_str()),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierPrev).c_str())),
        pTCContext->pWorker);

    if (iIdentifierCurrent > 0) { // can be negative for indicating a 'using'. Should not be zero.
        auto itInMap = ioToInit->mapAllMembers.find(iIdentifierCurrent);
        Assert_(itInMap != ioToInit->mapAllMembers.end());
        Assert_(ioToInit->vecAllMembers[itInMap.value()] == pCurrentBinding);
        Assert_(ioToInit->vecAllMembers[itInMap.value()]->iIdentifierHandle == iIdentifierCurrent);
        itInMap.value() = uPrevIndex;
        Assert_(ioToInit->vecAllMembers[ioToInit->mapAllMembers.find(iIdentifierCurrent).value()] == pPrevBinding);
    }
    if (iIdentifierPrev > 0) { // can be negative for indicating a 'using'. Should not be zero.
        auto itInMap = ioToInit->mapAllMembers.find(iIdentifierPrev);
        Assert_(itInMap != ioToInit->mapAllMembers.end());
        Assert_(ioToInit->vecAllMembers[itInMap.value()] == pPrevBinding);
        Assert_(ioToInit->vecAllMembers[itInMap.value()]->iIdentifierHandle == iIdentifierPrev);
        itInMap.value() = uIndex;
        Assert_(ioToInit->vecAllMembers[ioToInit->mapAllMembers.find(iIdentifierPrev).value()] == pCurrentBinding);
    }
    ioToInit->vecAllMembers[uPrevIndex] = pCurrentBinding;
    ioToInit->vecAllMembers[uIndex] = pPrevBinding;
    pCurrentBinding->uScopeAndLocation = EScopeKind::SCOPEKIND_COMPOUND | (uPrevIndex << 8);
    pPrevBinding->uScopeAndLocation = EScopeKind::SCOPEKIND_COMPOUND | (uIndex << 8);
}

// Helper function for reconstructing original node and statement for error emission 'try_finalize_structlike_runtime_tc', below
local_func void emit_compound_finalization_error(TypeInfo_CompoundBase* pCompound,
    TCContext* pTCContext, u16 uErrCode, const char* formatMsg,
    DisplayableCompilerEntity param1 = {}, DisplayableCompilerEntity param2 = {}, DisplayableCompilerEntity param3 = {})
{
    TCStatement* pTCStatement = pCompound->pRegistration->pStatementWithSignature;
    TmpTCNode main = init_tmp_tc_node(0, pTCStatement, pTCContext);
    if (u8(main.pTCNode->ast.uNodeKindAndFlags) == ENODE_ST_DECLARATION) {
        TmpTCNode assumedStruct = init_tmp_tc_node(main.pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
        emit_error(&assumedStruct, pTCStatement, pTCContext, uErrCode, formatMsg, param1, param2, param3);
    } else {
        emit_error(&main, pTCStatement, pTCContext, uErrCode, formatMsg, param1, param2, param3);
    }
}

local_func bool try_finalize_structlike_runtime_tc(TypeInfo_StructLike* ioToInit, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Trying to finalize compound runtime for compound %u in file '%s'",
        u64(ioToInit->uRegistrationIndex),
        reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(ioToInit->pRegistration->uTCProgress < ECOMPOUND_DONE_RUNTIME);
    Assert(0 == (ioToInit->_coreType & COMPOUNDTYPE_IS_COMPTIME_ONLY),
        "try_finalize_struct_type_runtime_tc() : should not be called on comptime-only struct");
    Assert_(0 == (ioToInit->_coreType & COMPOUNDTYPE_IS_ENUM));
    if (ioToInit->_coreType & COMPOUNDTYPE_IS_VIEW) {
        // TODO ?
        ioToInit->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME;
        emit_compound_finalization_error(ioToInit, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "try_finalize_structlike_runtime_tc() : struct views not yet implemented");
        return false;
    }
    u16 uCurrentIsUnion = ioToInit->_coreType & COMPOUNDTYPE_IS_UNION;
    u64 uFinalAlignedSize = 0;

    u32 uCountMembers = ioToInit->vecAllMembers.size();

    // TODO: prefer some tmp array for sorting ? do we really need them sorted in the actual compound vector ?
    // maybe the runtime vs constant 'split', but apart from that ?
    /*
    ArenaRefPoint beforeTmp = get_arena_ref_point(pTCContext->pWorker->tmpArena);
    defer { reset_arena_no_release_to(beforeTmp, pTCContext->pWorker->tmpArena); };
    TmpStackOptiArray<ValueBinding*, 64u> sortedMembers(pTCContext->pWorker->tmpArena);
    */

    u32 uCountRuntime = 0u;
    if (uCountMembers) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Sorting %u members for compound %u in file '%s'",
            u64(uCountMembers), u64(ioToInit->uRegistrationIndex),
            reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);
        u32 uFirstIndex = 0u;
        { do_bubble_sort_pass:
            // We need to *sort* structs after our out-of-order TC to ensure that the final layout will respect
            //   the user declaration order (and hopefully... intent). We'll also bubble-up all 'runtime' members
            //   at the start of the member declaration registry, to keep them conveniently sorted and counted apart from const.
            // Note: sorting is not that all important for unions, but we'll do it nonetheless, to at least have
            //   all 'runtime' at the start => we use same algorithm here for unions, for simplicity.
            // Important note: during TC, we arranged for the 'uCompoundDeclSort' slot of the ValueBinding to hold the position of the
            //   instruction (and node) in block, to know about the declaration order.
            // We also ensured of the struct body being single-block by spawning virtual sub-compounds, which should be sorted
            //   independently, as-if 'using' them, for any encountered subblock (such as an #if inside).
            ValueBinding* pPrevBinding = ioToInit->vecAllMembers[uFirstIndex];
            for (u32 uIndex = uFirstIndex+1u; uIndex < uCountMembers; uIndex++) {
                uFirstIndex = uIndex;
                ValueBinding* pBinding = ioToInit->vecAllMembers[uIndex];
                Assert_((pBinding->uScopeAndLocation >> 8) == uIndex);

                /*{
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Bubble-sort binding at #%u ('%s') against prev ('%s')",
                        u64(uIndex),
                        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pBinding->iIdentifierHandle).c_str()),
                        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pPrevBinding->iIdentifierHandle).c_str())),
                        pTCContext->pWorker);
                }*/

                if (!is_value_tc_const(pBinding)) {
                    const TypeInfo* pMemberType = pBinding->pType;
                    if (get_type_kind(pMemberType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                        if (pMemberType->_coreFlags & (COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME|COMPOUNDFLAG_BODY_IN_ERROR)) {
                            emit_compound_finalization_error(ioToInit, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                "try_finalize_structlike_runtime_tc() : cannot finalize against member in error");
                        }
                        if (!is_compound_type_full_typechecked((const TypeInfo_StructLike*)pMemberType)) {
                            int hello = 1;
                            Assert(false, "youhou?");
                        }
                    }

                    if (is_value_tc_const(pPrevBinding)) {
                        /*{
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                                "Current is runtime, and Prev is const => Marked for Bubble-up"),
                                pTCContext->pWorker);
                        }*/
                        goto bubble_up;

                    } else if (pPrevBinding->uCompoundDeclSort > pBinding->uCompoundDeclSort) { 
                        /*{
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                                "Previous runtime is marked as being *after* current, in decl order (Prev 0x%8x, Curr 0x%8x) => Marked for Bubble-up",
                                u64(pPrevBinding->uCompoundDeclSort), u64(pBinding->uCompoundDeclSort)),
                                pTCContext->pWorker);
                        }*/
                        { bubble_up:
                            u32 uPrevIndex = uIndex-1u;
                            do_swap_value_members(ioToInit, uIndex, uPrevIndex, pBinding, pPrevBinding, pTCContext);
                            uIndex = uPrevIndex;
                            if (uIndex) {

                                pBinding = ioToInit->vecAllMembers[uIndex];
                                pPrevBinding = ioToInit->vecAllMembers[uIndex-1u];
                                /*{
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                                        "Bubbling-up, rechecking binding at #%u ('%s') against prev ('%s')",
                                        u64(uIndex),
                                        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pBinding->iIdentifierHandle).c_str()),
                                        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pPrevBinding->iIdentifierHandle).c_str())),
                                        pTCContext->pWorker);
                                }*/

                                if (is_value_tc_const(pPrevBinding)) {
                                    goto bubble_up;
                                } else if (pPrevBinding->uCompoundDeclSort > pBinding->uCompoundDeclSort) {
                                    goto bubble_up;
                                } else {
                                    Assert_(pPrevBinding->uCompoundDeclSort < pBinding->uCompoundDeclSort);
                                }
                            }
                            goto do_bubble_sort_pass;
                        }

                    } else {
                        Assert_(pPrevBinding->uCompoundDeclSort < pBinding->uCompoundDeclSort);
                    }
                } // otherwise noop: all consts will stay kinda 'unsorted' at the end

                pPrevBinding = pBinding;
            }
        }
    }

    bool bHasUsingSameKind = false;
    bool bHasUsingInversions = false;
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("Now double-checking runtime counts for compound %u in file '%s'",
            u64(ioToInit->uRegistrationIndex),
            reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

        while (uCountRuntime < uCountMembers) {
            ValueBinding* pBinding = ioToInit->vecAllMembers[uCountRuntime];
            if (is_value_tc_const(pBinding))
                break;
            else if (pBinding->iIdentifierHandle < 0) {  // a negative identifier on a compound binding is our way to flag a 'using' specification
                Assert_(get_type_kind(pBinding->pType) == ETypeKind::ETYPEKIND_STRUCTLIKE);
                const TypeInfo_StructLike* pUsedOther = (const TypeInfo_StructLike*)pBinding->pType;
                if ((pUsedOther->_coreType & COMPOUNDTYPE_IS_UNION) == uCurrentIsUnion)
                    bHasUsingSameKind = true;
                else
                    bHasUsingInversions = true;
                // TODO!!!
                ioToInit->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME;
                emit_compound_finalization_error(ioToInit, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "try_finalize_structlike_runtime_tc() : taking a 'using' specification into account is not yet implemented");
                return false;
            } else {
                uCountRuntime++;
            }
        }
    }

    if (uCountRuntime > 0x0000'FFFFu) {
        ioToInit->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME;
        emit_compound_finalization_error(ioToInit, pTCContext, CERR_TOO_MANY_RUNTIME_MEMBERS_IN_STRUCT,
            "try_finalize_structlike_runtime_tc() : max runtime member count is 64K");
        return false;
    }
    ioToInit->uRuntimeMemberCount = u16(uCountRuntime);

    if (0 == uCurrentIsUnion && !bHasUsingInversions) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("Now assigning byte offsets to the %u runtime members for compound %u in file '%s'",
            u64(ioToInit->uRuntimeMemberCount), u64(ioToInit->uRegistrationIndex),
            reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

        u64 uFinalSize = 0;
        u32 uFinalAlignMask = 0;
        u32 uFlagsOfVecs = 0;
        u32 uFlagsOfScalars = 0;
        u64 bHasPaddingWithin = 0;
        u64 uNextStartPos = 0;
        for (u32 uMember = 0; uMember < uCountRuntime; uMember++) {
            ValueBinding* pBinding = ioToInit->vecAllMembers[uMember];
            Assert_(!is_value_tc_const(pBinding));
            u32 uAlignThere, uUnalignedSizeThere;
            u32 uUnusedAlignedSizeThere = get_runtime_sizeof_ext(pBinding->pType, &uAlignThere, &uUnalignedSizeThere);
            Assert_(uAlignThere <= 4096u);
            u64 uMaskRuntimeIsZero = u64(i64(uUnalignedSizeThere-1uLL) >> 63);
            u8 uIRFormat = get_ir_format(pBinding->pType);
            uFlagsOfVecs |= (1u << (uIRFormat >> 4)) & ~u32(uMaskRuntimeIsZero);
            uFlagsOfScalars |= (1u << (uIRFormat & 0x0Fu)) & ~u32(uMaskRuntimeIsZero);
            u32 uAlignMaskThere = uAlignThere - 1u;
            uFinalAlignMask |= uAlignMaskThere;
            u64 uStartPosThere = ((uNextStartPos-1uLL) | u64(uAlignMaskThere)) + 1uLL;
            bHasPaddingWithin |= (uStartPosThere - uFinalSize) & ~uMaskRuntimeIsZero;

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Once sorted, param #%u ('%s') is at byte offset %u",
                u64(uMember),
                reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pBinding->iIdentifierHandle).c_str()),
                uStartPosThere), pTCContext->pWorker);

            // We hack the 'IR' slot for compounds after typechecking, to represent their byte offset from base.
            pBinding->info.uIRandMetaFlags |= uStartPosThere << IR_STD_PARAM_SHIFT;
            Assert_((pBinding->info.uIRandMetaFlags >> IR_STD_PARAM_SHIFT) == uStartPosThere);
            Assert_(pBinding->info.uIRandMetaFlags & IRFLAG_TC_BINDING_INSTANCE);
            uNextStartPos = (uStartPosThere + uUnalignedSizeThere);
            uFinalSize = (uNextStartPos & ~uMaskRuntimeIsZero) | (uFinalSize & uMaskRuntimeIsZero);
        }

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("Now finalizing format and align, having found that size is %u bytes, for compound %u in file '%s'",
            u64(uFinalSize), u64(ioToInit->uRegistrationIndex),
            reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

        u32 uFinalAlign = uFinalAlignMask + 1u;
        Assert_(CountSetBits32(uFinalAlign) == 1);
        int iLog2OfFinalAlign = GetPosOfMostSignificantBitSet32(uFinalAlign);
        Assert_(iLog2OfFinalAlign <= 12);
        u8 uFinalFormat = 0x00u;
        u32 uSlotsCount = 0;
        if (uFinalSize) {
            uFinalAlignedSize = ((uFinalSize-1uLL) | u64(uFinalAlignMask)) + 1uLL;
            if (uFinalAlignedSize > MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE) {
                ioToInit->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME;
                emit_compound_finalization_error(ioToInit, pTCContext, CERR_TYPE_TOO_LARGE,
                    "try_finalize_structlike_runtime_tc() : max runtime size for a type is 6MB");
                return false;
            }

            u32 uFormatFootprint;
            if (CountSetBits32(uFlagsOfVecs) == 1 && CountSetBits32(uFlagsOfScalars) == 1) {
                // All members share a single format => reconstruct that format here
                u8 uVecPart = u8(GetPosOfMostSignificantBitSet32(uFlagsOfVecs));
                u8 uScalarPart = u8(GetPosOfMostSignificantBitSet32(uFlagsOfScalars));
                uFinalFormat = (uVecPart << 4) | uScalarPart;
                uFormatFootprint = 1u << (uVecPart + (uScalarPart & 0x07u));

            } else {
                // get format based on max align
                if (iLog2OfFinalAlign <= 5)
                    uFinalFormat = u8(iLog2OfFinalAlign);
                else {
                    uFinalFormat = 0x05u; // ...but no higher than a r256
                }
                uFormatFootprint = 1u << uFinalFormat;
            }
            uSlotsCount = u32(uFinalAlignedSize / uFormatFootprint);
            Assert_(0uLL == (uFinalAlignedSize % uFormatFootprint));
            Assert_(uSlotsCount <= MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE);
            if (0 != bHasPaddingWithin)
                ioToInit->_coreFlags |= COMPOUNDFLAG_HAS_PADDING_wITHIN;
        }
        ioToInit->uIRFormat = uFinalFormat;
        ioToInit->uSlotsAndAlign = (u32(iLog2OfFinalAlign) << 28) | uSlotsCount;
        ioToInit->uUnalignedByteSize = u32(uFinalSize);
        return true;

    } else {
        // TODO!!!
        ioToInit->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME;
        emit_compound_finalization_error(ioToInit, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "try_finalize_structlike_runtime_tc() : finalizing 'unions' or struct containing embedded unions is not implemented");
        return false;
    }
}

//#define NAMESPACEFLAG_IS_SOURCE_FILE        0x4000'0000'0000'0000uLL
#define NAMESPACEFLAG_HAS_PACKAGE_ACCESS    0x8000'0000'0000'0000uLL

local_func_inl u64 get_namespace_id(int iSourceFileIndex, u32 uRegistration, u64 uHasPackageAccess = NAMESPACEFLAG_HAS_PACKAGE_ACCESS)
{
    u64 uUID = u64(u32(iSourceFileIndex)) | (u64(uRegistration) << 32) | uHasPackageAccess;
    return uUID;
}

local_func_inl u64 get_namespace_id(ReferencedNamespace* pRefNamespace, u64 uHasPackageAccess = NAMESPACEFLAG_HAS_PACKAGE_ACCESS)
{
    int iSourceFileIndex = pRefNamespace->pOrigNamespace->pOriginalSourceFile->iRegistrationIndex;
    u32 uRegistration = pRefNamespace->pOrigNamespace->uRegistrationIndex;
    return get_namespace_id(iSourceFileIndex, uRegistration, uHasPackageAccess);
}

local_func_inl void decode_namespace_id(u64 uValue, i32* outSourceFile, u32* outRegistration, u64* outHasPackageAccess = 0)
{
    *outSourceFile = i32(uValue);
    *outRegistration = u32(uValue >> 32) & 0x7FFF'FFFFu;
    if (outHasPackageAccess) {
        *outHasPackageAccess = uValue & NAMESPACEFLAG_HAS_PACKAGE_ACCESS;
    }
}

local_func ETCResult register_source_file_as_namespace(u64* outRegistration, StringView fileName,
    u64 uHasPackageAccess, TmpTCNode* pExpr, TCStatement* pTCStatement, TCContext* pTCContext)
{
    if (fileName.uByteLength > 0x0000'FFFFu) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_FILENAME_TOO_LONG,
            "register_source_file_as_namespace() : filename too long !!!");
    }
    int iLine = 1; // TODO
    int iSourceFileIndex = get_or_register_source_file(fileName.start, u16(fileName.uByteLength),
        pTCContext->pIsolatedSourceFile->iRegistrationIndex, iLine, pTCContext->pProgCompilationState,
        pTCContext->pProgCompilationState->pOsFuncs, pTCContext->pCompilationParams,
        pTCContext->pProgCompilationState->pCompilationResults);
    if (iSourceFileIndex >= 0) {
        ESourceCompState unused = is_source_file_compiled_otherwise_spawn_task(iSourceFileIndex,
            pTCContext->pIsolatedSourceFile->iRegistrationIndex, pTCStatement->uBlockIndexInSourceFile,
            pTCStatement->uStatementIndexInBlock, pExpr->uNodeIndexInStatement, pTCContext->pProgCompilationState,
            pTCContext->pCompilationParams, pTCContext->pProgCompilationState->pCompilationResults);
        constexpr u32 uRootNamespaceId = 0u;
        *outRegistration = get_namespace_id(iSourceFileIndex, 0u, uHasPackageAccess);
        return ETCResult::ETCR_SUCCESS;
    } else {
        return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_FILE_LOAD_DIRECTIVE,
            "register_source_file_as_namespace() : get_or_register_source_file() indicated an error (returned negative index)");
    }
}

local_func ETCResult register_foreign_source(u64* outRegistration, StringView staticLibName,
    TmpTCNode* pExpr, TCStatement* pTCStatement, TCContext* pTCContext)
{
    if (staticLibName.uByteLength > 0x0000'FFFFu) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_FILENAME_TOO_LONG,
            "register_foreign_source() : static lib name too long !!!");
    }
    Assert_(staticLibName.uByteLength);
    
    u32 uPos = pTCContext->pIsolatedSourceFile->vecForeignSources.size();
    ForeignSourceDecl decl { staticLibName, "" };
    pTCContext->pIsolatedSourceFile->vecForeignSources.append(decl);
    *outRegistration = u64(uPos) | (u64(pTCContext->pIsolatedSourceFile->iRegistrationIndex + 1u) << 32);
    return ETCResult::ETCR_SUCCESS;
}

local_func ETCResult register_foreign_source(u64* outRegistration, StringView staticLibName, StringView dynamicLibName,
    TmpTCNode* pExpr, TCStatement* pTCStatement, TCContext* pTCContext)
{
    if (staticLibName.uByteLength > 0x0000'FFFFu) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_FILENAME_TOO_LONG,
            "register_foreign_source() : static lib name too long !!!");
    }
    if (dynamicLibName.uByteLength > 0x0000'FFFFu) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_FILENAME_TOO_LONG,
            "register_foreign_source() : dynamic lib name too long !!!");
    }
    u32 uPos = pTCContext->pIsolatedSourceFile->vecForeignSources.size();
    ForeignSourceDecl decl { staticLibName, dynamicLibName };
    pTCContext->pIsolatedSourceFile->vecForeignSources.append(decl);
    *outRegistration = u64(uPos) | (u64(pTCContext->pIsolatedSourceFile->iRegistrationIndex + 1u) << 32);
    return ETCResult::ETCR_SUCCESS;
}

local_func void tc_init_statement_from_ast(TCStatement* pStatementToInit, u32 uAstBlockIndex, u32 uAstStatementIndex,
    AstStatement* pAstStatement, SourceFileDescAndState* pSourceFile)
{
    pStatementToInit->pAstStatement = pAstStatement;
    pStatementToInit->pChildBlock = 0;

    pStatementToInit->vecNodes.init(pSourceFile->localArena);
    pStatementToInit->vecNodeValues.init(pSourceFile->localArena);
    u32 uNodeCount = pAstStatement->uNodeCount;
    if (0 == (pAstStatement->uFlags & IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD)) {
        Assert_(uNodeCount > 0);
        TCNode* tAllDirectNodesInOneAlloc = (TCNode*)alloc_from(pSourceFile->localArena,
            uNodeCount * sizeof(TCNode), alignof(TCNode));
        pStatementToInit->vecNodes.resize_non_zeroing(uNodeCount);
        for (u32 uNode = 0; uNode < uNodeCount; uNode++) {
            TCNode* pNode = tAllDirectNodesInOneAlloc + uNode;
            pNode->ast = pAstStatement->tNodes[uNode]; // copying contents of AstNode into TCNode
            pNode->uFinalValueIndex = 0;
            pNode->uIntrinsicValueIndex = 0;
            pStatementToInit->vecNodes[uNode] = pNode;
        }
    }

    pStatementToInit->uStatementIndexInBlock = uAstStatementIndex;
    pStatementToInit->uLastIRorGlobalTCResult = 0;

    pStatementToInit->uBlockIndexInSourceFile = uAstBlockIndex;
    pStatementToInit->iSourceFileIndex = pSourceFile->iRegistrationIndex;
}

local_func void init_base_block(TCBaseSourceBlock* pResult, u32 uAstBlockIndex, TCBaseSourceBlock* pParentBlock, TCContext* pTCContext)
{
    SourceFileDescAndState* pSourceFile = pTCContext->pIsolatedSourceFile;
    Assert_(pSourceFile->iBlockCount >= 0);
    Assert_(uAstBlockIndex < u32(pSourceFile->iBlockCount));
    AstBlock* pAstBlock = pSourceFile->tBlocks + uAstBlockIndex;
    pResult->iSourceFileIndex = pSourceFile->iRegistrationIndex;
    pResult->pAstBlock = pAstBlock;
    pResult->pNextTcBlockAfterCurrentStatement = 0;
    pResult->pBlockIsDirectChildOfNamespace = 0;
    pResult->pParentBlock = pParentBlock;
    pResult->uAstBlockIndex = uAstBlockIndex;
    pResult->uStatementBeingTypechecked = 0;
    pResult->vecStatements.init(pSourceFile->localArena);
    u32 uStatementCount = pAstBlock->uStatementCount;
    if (uStatementCount > 0) {
        TCStatement* tAllDirectStatementsInOneAlloc = (TCStatement*)alloc_from(pSourceFile->localArena,
            uStatementCount * sizeof(TCStatement), alignof(TCStatement));
        pResult->vecStatements.resize_non_zeroing(uStatementCount);
        for (u32 uStatement = 0; uStatement < uStatementCount; uStatement++) {
            TCStatement* pStatement = tAllDirectStatementsInOneAlloc + uStatement;
            AstStatement* pAstStatement = pAstBlock->tStatements + uStatement;
            tc_init_statement_from_ast(pStatement, uAstBlockIndex, uStatement, pAstStatement, pSourceFile);
            if (pAstStatement->iChildBlock > 0) {
                // flagging an unallocated child-block from AST as a taggued pointer:
                pStatement->pChildBlock = reinterpret_cast<TCBaseSourceBlock*>((u64(pAstStatement->iChildBlock) << 2) | 1u);
            }
            pResult->vecStatements[uStatement] = pStatement;
        }
    }
}

local_func TCBaseSourceBlock* tc_alloc_and_init_base_block(u32 uAstBlockIndex, u32 uGlobalScope,
    TCBaseSourceBlock* pParentBlock, TCContext* pTCContext)
{
    SourceFileDescAndState* pSourceFile = pTCContext->pIsolatedSourceFile;
    TCBaseSourceBlock* pResult = (TCBaseSourceBlock*)alloc_from(pSourceFile->localArena,
        sizeof(TCBaseSourceBlock), alignof(TCBaseSourceBlock));
    init_base_block(pResult, uAstBlockIndex,  pParentBlock, pTCContext);
    pResult->_uScopeLevelIffGlobal = uGlobalScope;
    return pResult;
}

local_func TCDeclSourceBlock* tc_alloc_and_init_decl_block(u32 uAstBlockIndex, u32 uGlobalScope,
    TCBaseSourceBlock* pParentBlock, TCContext* pTCContext)
{
    SourceFileDescAndState* pSourceFile = pTCContext->pIsolatedSourceFile;
    TCDeclSourceBlock* pResult = (TCDeclSourceBlock*)alloc_from(pSourceFile->localArena,
        sizeof(TCDeclSourceBlock), alignof(TCDeclSourceBlock));
    init_base_block(pResult, uAstBlockIndex, pParentBlock, pTCContext);
    FireAndForgetArenaAlloc localAlloc(pSourceFile->localArena);
    pResult->mapBlockDeclarationsById.init(localAlloc);
    return pResult;
}

local_func TCSeqSourceBlock* tc_alloc_and_init_seq_block(u32 uAstBlockIndex,
    TCSeqSourceBlock* pParentBlock, u32 uStatementInParentBlock, TCContext* pTCContext)
{
    SourceFileDescAndState* pSourceFile = pTCContext->pIsolatedSourceFile;
    TCSeqSourceBlock* pResult = (TCSeqSourceBlock*)alloc_from(pSourceFile->localArena,
        sizeof(TCSeqSourceBlock), alignof(TCSeqSourceBlock));
    init_base_block(pResult, uAstBlockIndex, pParentBlock, pTCContext);
    FireAndForgetArenaAlloc localAlloc(pSourceFile->localArena);
    pResult->mapBlockDeclarationsById.init(localAlloc);
    pResult->vecDeferredBlocksInDeclOrder.init(localAlloc);
    pResult->pVecPlaceholdersToAfterBlockAndAfterElses = 0;
    pResult->pVecPlaceholdersToElse = 0;
    pResult->uIROfAfterBlock = 0u;
    pResult->uIRBeforeLoopConditionIfBlockIsLoop = 0u;
    pResult->uIndexOfParentStatementInParentBlock = uStatementInParentBlock;
    pResult->uKindFlagsOfParentStatement = 0u;
    if (pParentBlock) {
        pResult->pVecPlaceholdersToAfterBlockAndAfterElses = (TmpArray<u32>*)alloc_from(pSourceFile->localArena,
            sizeof(TmpArray<u32>), alignof(TmpArray<u32>));
        pResult->pVecPlaceholdersToAfterBlockAndAfterElses->init(pSourceFile->localArena);
    }
    return pResult;
}

// predecls
ETCResult typecheck_any_non_invoc_expression(TmpTCNode* pExpr, u8 uNodeKind, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow);
ETCResult typecheck_possible_expr_list(u32 uStartingNodeIndex, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, bool bAllowNoInit,
    TmpTCNode* tAllExpr, u8* ioNodeCount, const TypeInfo** tAllInferredTypes = 0, u8 uInferredCount = 0);


local_func const TypeInfo_Pointer* get_pointer_type_to(const TypeInfo* pToType, CompilationContext* pEvalContext)
{
    SourceFileDescAndState* pSourceFile = pEvalContext->pIsolatedSourceFile;
    auto itFound = pEvalContext->pIsolatedSourceFile->mapAllPointersByToType.find(pToType);
    if (itFound != pEvalContext->pIsolatedSourceFile->mapAllPointersByToType.end())
        return itFound.value();
    TypeInfo_Pointer* pNewPtrType = (TypeInfo_Pointer*)alloc_from(pSourceFile->localArena,
        sizeof(TypeInfo_Pointer), alignof(TypeInfo_Pointer));
    init_ptr_type(pNewPtrType, pToType, pEvalContext);
    pEvalContext->pIsolatedSourceFile->mapAllPointersByToType.insert(pToType, pNewPtrType);
    return pNewPtrType;
}

local_func ETCResult tc_make_pointer_type_as(TmpTCNode* pNode, const TypeInfo* pToType,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    u16 uTypeErr;
    if (is_allowed_as_runtime_type(pToType, pTCContext, &uTypeErr)) {
        const TypeInfo_Pointer* pPtrType = get_pointer_type_to(pToType, pTCContext);
        NodeValue* pTypeValue = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pTypeValue->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
        pTypeValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST;
        pTypeValue->info.metaValue.knownValue.pType = pPtrType;
        return set_node_typecheck_expr_success(pNode->pTCNode);
    } else {
        return_error(pNode, pTCStatement, pTCContext, uTypeErr,
            "tc_make_pointer_type_as() : type pointed to is not allowed as a runtime type");
    }
}

local_func const TypeInfo_Array* get_array_type_to(const TypeInfo* pToType, u32 uElemCountAndKind, CompilationContext* pEvalContext)
{
    SourceFileDescAndState* pSourceFile = pEvalContext->pIsolatedSourceFile;
    auto itOfSameType = pEvalContext->pIsolatedSourceFile->mapAllArraysByToType.find(pToType);
    if (itOfSameType != pEvalContext->pIsolatedSourceFile->mapAllArraysByToType.end()) {
        auto itFoundCount = itOfSameType.value().find(uElemCountAndKind);
        if (itFoundCount != itOfSameType.value().end())
            return itFoundCount.value();
    } else {
        TmpMap<u32, TypeInfo_Array*> mapThere(pSourceFile->localArena);
        itOfSameType = pEvalContext->pIsolatedSourceFile->mapAllArraysByToType.insert(pToType, mapThere);
    }
    TypeInfo_Array* pNewArrayType = (TypeInfo_Array*)alloc_from(pSourceFile->localArena,
        sizeof(TypeInfo_Array), alignof(TypeInfo_Array));
    init_array_type(pNewArrayType, pToType, uElemCountAndKind, pEvalContext);
    itOfSameType.value().insert(uElemCountAndKind, pNewArrayType);
    return pNewArrayType;
}

local_func ETCResult tc_make_array_type_as(TmpTCNode* pNode, const TypeInfo* pToType, u32 uElemCountAndKind,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    u16 uTypeErr;

    u8 uArrayCategory = u8(uElemCountAndKind >> 24);
    u32 uElemCount = uElemCountAndKind & 0x00FF'FFFFu;

    if (is_allowed_as_runtime_type(pToType, pTCContext, &uTypeErr)) {
        u32 unused;
        u64 uElemByteSize = u64(get_runtime_sizeof(pToType, &unused));
        Assert_(uElemByteSize); // void types todo ???
        if (uArrayCategory == ARRAY_TYPE_KIND_STATIC || uArrayCategory == ARRAY_TYPE_KIND_BUFFER) {
            u32 uIfBufferAdditionalSize = (uArrayCategory == ARRAY_TYPE_KIND_BUFFER) ? align_to(1u << get_log2_of_align_bytes(pToType), 4u) : 0u;
            if (uElemByteSize * u64(uElemCount) + u64(uIfBufferAdditionalSize) > u64(MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE)) {
                return_error(pNode, pTCStatement, pTCContext, CERR_TYPE_TOO_LARGE,
                    "tc_make_array_type_as() : max byte count for static (or buffer) array type is 6M");
            }
        }
        const TypeInfo_Array* pArrayType = get_array_type_to(pToType, uElemCountAndKind, pTCContext);
        NodeValue* pTypeValue = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pTypeValue->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
        pTypeValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN;
        pTypeValue->info.metaValue.knownValue.pType = pArrayType;
        return set_node_typecheck_expr_success(pNode->pTCNode);
    } else {
        return_error(pNode, pTCStatement, pTCContext, uTypeErr,
            "tc_make_array_type_as() : element type for array-like is not allowed as a runtime type");
    }
}

local_func_inl ETCResult tc_make_slice_type_as(TmpTCNode* pNode, const TypeInfo* pToType,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Slice-Decl expression"), pTCContext->pWorker);
    return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_SLICE) << 24, pTCStatement, pTCContext);
}

local_func_inl ETCResult tc_try_make_compact_type_as(TmpTCNode* pNode, const TypeInfo* pToType,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    if (get_type_kind(pToType) == ETypeKind::ETYPEKIND_ARRAY) {
        u8 eArrayKind = get_array_category_type((const TypeInfo_Array*)pToType);
        switch (eArrayKind) {
            case ARRAY_TYPE_KIND_STATIC:
                return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE, "cannot compact-define over static");
            case ARRAY_TYPE_KIND_SLICE:
                return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_COMPACT_SLICE) << 24, pTCStatement, pTCContext);
            case ARRAY_TYPE_KIND_MUTABLE_SLICE:
                return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_MUTABLE_COMPACT_SLICE) << 24, pTCStatement, pTCContext);
            case ARRAY_TYPE_KIND_COMPACT_SLICE:
            case ARRAY_TYPE_KIND_MUTABLE_COMPACT_SLICE:
                return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE, "cannot compact-define over already compact slice");
            case ARRAY_TYPE_KIND_DYNAMIC:
                return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_COMPACT_DYNAMIC) << 24, pTCStatement, pTCContext);
            case ARRAY_TYPE_KIND_DYNAMIC_FF:
                return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_COMPACT_DYNAMIC_FF) << 24, pTCStatement, pTCContext);
            case ARRAY_TYPE_KIND_COMPACT_DYNAMIC:
            case ARRAY_TYPE_KIND_COMPACT_DYNAMIC_FF:
            case ARRAY_TYPE_KIND_DYNAMIC_WITH_BUFFER:
                return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE, "cannot compact-define over already compact dynamic");
            case ARRAY_TYPE_KIND_BUFFER:
                return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE, "cannot compact-define over buffer");
            case ARRAY_TYPE_KIND_STABLE_GROWING:
                return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_COMPACT_STABLE_GROWING) << 24, pTCStatement, pTCContext);
            case ARRAY_TYPE_KIND_STABLE_GROWING_FF:
                return tc_make_array_type_as(pNode, pToType, u32(ARRAY_TYPE_KIND_COMPACT_STABLE_GROWING_FF) << 24, pTCStatement, pTCContext);
            case ARRAY_TYPE_KIND_COMPACT_STABLE_GROWING:
            case ARRAY_TYPE_KIND_COMPACT_STABLE_GROWING_FF:
                return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE, "cannot compact-define over already compact stable growing");
            default:
                Assert(false, "unknown array category");
                return_error(pNode, pTCStatement, pTCContext, FERR_ASSERTION_FAILED, "tc_try_make_compact_type_as() : base type of unknown array category");
        }
    } else if (get_type_kind(pToType) == ETypeKind::ETYPEKIND_SET) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "set compact-define not yet implemented");
    } else if (get_type_kind(pToType) == ETypeKind::ETYPEKIND_MAP) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "map compact-define not yet implemented");
    } else {
        if (get_type_kind(pToType) == ETypeKind::ETYPEKIND_OTHERCORE && (pToType->_coreFlags & OTHERCOREFLAG_IS_STRING)) {
            if (pToType == g_pCoreTypesInfo[ECORETYPE_STRINGVIEW]) {
                NodeValue* pTypeValue = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                pTypeValue->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
                pTypeValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN;
                pTypeValue->info.metaValue.knownValue.pType = g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING];
                return set_node_typecheck_expr_success(pNode->pTCNode);
            } else if (pToType == g_pCoreTypesInfo[ECORETYPE_OWNEDSTRING]) {
                NodeValue* pTypeValue = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                pTypeValue->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
                pTypeValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN;
                pTypeValue->info.metaValue.knownValue.pType = g_pCoreTypesInfo[ECORETYPE_COMPACT_OWNEDSTRING];
                return set_node_typecheck_expr_success(pNode->pTCNode);
            } else if (pToType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING] || pToType == g_pCoreTypesInfo[ECORETYPE_COMPACT_OWNEDSTRING]) {
                return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE, "cannot compact-define over already compact string-related");
            } else {
                Assert(false, "unknown string-related");
                return_error(pNode, pTCStatement, pTCContext, FERR_ASSERTION_FAILED, "tc_try_make_compact_type_as() : string-flagged base type not being one of the four core string types");
            }
        } else {
            return_error(pNode, pTCStatement, pTCContext, CERR_CANNOT_COMPACT_THIS_TYPE,
                "cannot compact-define over a different type than array-kind, set, map, string or owned_string");
        }
    } 
}

local_func void debug_print_type_to(char* pBuffer, const TypeInfo* pType, CompilationContext* pEvalContext, bool isContextTC) {
    if (pType) {
        if (is_core_type(pType))
            sprintf(pBuffer, "%s", tCoreTypesStr[pType->_coreType]);
        else {
            switch (get_type_kind(pType)) {
                case ETypeKind::ETYPEKIND_POINTER: {
                    const TypeInfo_Pointer* asPtr = (const TypeInfo_Pointer*)pType;
                    sprintf(pBuffer, "^");
                    debug_print_type_to(pBuffer+1, asPtr->pPointedToType, pEvalContext, isContextTC);
                } break;

                case ETypeKind::ETYPEKIND_ARRAY: {
                    const TypeInfo_Array* asArray = (const TypeInfo_Array*)pType;
                    sprintf(pBuffer, "[kind=%u, count=%u]", asArray->_coreType, asArray->uElemCount);
                    debug_print_type_to(pBuffer+strlen(pBuffer), asArray->pElementType, pEvalContext, isContextTC);
                } break;

                default: {
                    sprintf(pBuffer, "[not yet handled]");
                } break;
            }
        }
    } else
        sprintf(pBuffer, "[null]");
}


local_func ETCResult tc_gather_string_info(TmpTCNode* pExprIfErr, IRInfo stringInfo, const TypeInfo_OtherCore* pStringType,
    TCStatement* pTCStatement, TCContext* pTCContext, u32 uIsAssignableFlag,
    IRInfo* outInfoPtrToBytes, IRInfo* outInfoByteLength, IRInfo* outInfoFlags, IRInfo* outInfoAllocStruct, IRInfo* outInfoAsRef)
{
    Assert_(uIsAssignableFlag == 0u || uIsAssignableFlag == IR_INSTRFLAG_IS_ASSIGNABLE);
    Assert_(outInfoPtrToBytes || outInfoByteLength || outInfoFlags || outInfoAllocStruct || outInfoAsRef);
    if (pStringType->_coreFlags & STRINGFLAG_IS_COMPACT) {
        if (outInfoAsRef) {
            EIRResult eGatherBaseAddress = ir_emit_or_solve_address_of(stringInfo, pTCStatement, pTCContext, outInfoAsRef);
            if (eGatherBaseAddress >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherBaseAddress),
                    "tc_gather_string_info() : gather ref address failed...");
            }
        }
        if (outInfoPtrToBytes) {
            *outInfoPtrToBytes = stringInfo;
        }
        if (outInfoByteLength) {
            IRInfo infoAddressOfByteLength;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(2u, stringInfo, 0x02u, ir_make_info_for_int_immediate(-1u, 0x02u), 4u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfByteLength);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherByteLength = ir_emit_or_solve_deref(infoAddressOfByteLength, 0x02u, 2u, 1u, 4u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoByteLength);
                if (eGatherByteLength >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherByteLength),
                        "tc_gather_string_info() : deref for byte length data failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_string_info() : gather address for byte length data failed...");
            }
        }
        if (outInfoFlags) {
            IRInfo infoAddressOfFlags;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(2u, stringInfo, 0x02u, ir_make_info_for_int_immediate(-2u, 0x02u), 4u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfFlags);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherFlags = ir_emit_or_solve_deref(infoAddressOfFlags, 0x02u, 2u, 1u, 4u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoFlags);
                if (eGatherFlags >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherFlags),
                        "tc_gather_string_info() : deref for flag data failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_string_info() : gather address for flag data failed...");
            }
        }
        if (outInfoAllocStruct) {
            Assert_(pStringType->_coreFlags & STRINGFLAG_HAS_ALLOC);
            IRInfo infoAddressOfAllocStruct;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(3u, stringInfo, 0x02u, ir_make_info_for_int_immediate(-3u, 0x02u), 8u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfAllocStruct);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherAlloc = ir_emit_or_solve_deref(infoAddressOfAllocStruct, 0x03u, 3u, 2u, 16u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoAllocStruct);
                if (eGatherAlloc >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAlloc),
                        "tc_gather_string_info() : deref for alloc struct failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_string_info() : gather address for alloc struct failed...");
            }
        }

    } else {
        IRInfo baseStructAddress;
        EIRResult eGatherBaseAddress = ir_emit_or_solve_address_of(stringInfo, pTCStatement, pTCContext, &baseStructAddress);
        if (eGatherBaseAddress >= EIRResult::EIRR_FIRST_ERROR) {
            return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherBaseAddress),
                "tc_gather_string_info() : gather ref address failed...");
        }
        if (outInfoAsRef) {
            *outInfoAsRef = baseStructAddress;
        }
        if (outInfoPtrToBytes) {
            EIRResult eDerefBaseAsPtrToBytes = ir_emit_or_solve_deref(baseStructAddress, 0x03u, 3u, 1u, 8u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoPtrToBytes);
            if (eDerefBaseAsPtrToBytes >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eDerefBaseAsPtrToBytes),
                    "tc_gather_string_info() : gather ptr to bytes failed...");
            }
        }
        if (outInfoByteLength) {
            IRInfo infoAddressOfByteLength;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(3u, baseStructAddress, 0x02u, ir_make_info_for_int_immediate(2u, 0x02u), 4u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfByteLength);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherByteLength = ir_emit_or_solve_deref(infoAddressOfByteLength, 0x02u, 3u, 1u, 4u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoByteLength);
                if (eGatherByteLength >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherByteLength),
                        "tc_gather_string_info() : deref for byte length data failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_string_info() : gather address for byte length data failed...");
            }
        }
        if (outInfoFlags) {
            IRInfo infoAddressOfFlags;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(2u, baseStructAddress, 0x02u, ir_make_info_for_int_immediate(3u, 0x02u), 4u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfFlags);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherFlags = ir_emit_or_solve_deref(infoAddressOfFlags, 0x02u, 2u, 1u, 4u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoFlags);
                if (eGatherFlags >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherFlags),
                        "tc_gather_string_info() : deref for flag data failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_string_info() : gather address for flag data failed...");
            }
        }
        if (outInfoAllocStruct) {
            Assert_(pStringType->_coreFlags & STRINGFLAG_HAS_ALLOC);
            IRInfo infoAddressOfAllocStruct;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(3u, baseStructAddress, 0x02u, ir_make_info_for_int_immediate(2u, 0x02u), 8u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfAllocStruct);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherAlloc = ir_emit_or_solve_deref(infoAddressOfAllocStruct, 0x03u, 3u, 2u, 16u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoAllocStruct);
                if (eGatherAlloc >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAlloc),
                        "tc_gather_string_info() : deref for alloc struct failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_string_info() : gather address for alloc struct failed...");
            }
        }
    }
    return ETCResult::ETCR_SUCCESS;
}

local_func StringView get_comptime_string_view_from_semantic_const_instance(const IRInfo& instance, const TypeInfo_OtherCore* pStringType,
                                                                            TCContext* pTCContext)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    Assert_(instance.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST);
    TmpTCNode mockNode {};
    TCStatement mockStatement {};
    IRInfo infoPtrToBytes;
    IRInfo infoByteLength;
    IRInfo infoFlags;
    ETCResult eGatherInfo = tc_gather_string_info(&mockNode, instance, pStringType, &mockStatement, pTCContext, 0u,
        &infoPtrToBytes, &infoByteLength, &infoFlags, 0, 0);
    Assert_(eGatherInfo == ETCResult::ETCR_SUCCESS);
    Assert_(irflag_is_known_embd(infoPtrToBytes.uIRandMetaFlags) && irflag_is_or_has_nyka(infoPtrToBytes.uIRandMetaFlags)); // TODO: allow a null string as semantic const ???
    Assert_(irflag_is_known_embd(infoByteLength.uIRandMetaFlags) && !irflag_is_or_has_nyka(infoByteLength.uIRandMetaFlags));
    Assert_(irflag_is_known_embd(infoFlags.uIRandMetaFlags) && !irflag_is_or_has_nyka(infoFlags.uIRandMetaFlags));
    i32 iOffset; u64 uIROfBaseArray = ir_decode_nyka_value(infoPtrToBytes.metaValue.knownValue.uEmbeddedValue, &iOffset);
    Assert_(!ir_is_immediate(uIROfBaseArray));
    IRInfo infoBaseArray;
    ir_get_info_from_non_imm(uIROfBaseArray, pTCContext, &infoBaseArray);
    StringView result;
    result.start = infoBaseArray.metaValue.knownValue.pPtrToRawData + iOffset;
    result.uByteLength = u32(infoByteLength.metaValue.knownValue.uEmbeddedValue);
    result.flags = u32(infoFlags.metaValue.knownValue.uEmbeddedValue);
    return result;
}

local_func FFString get_comptime_ffstring_from_compact_semantic_const_instance(const IRInfo& instance, const TypeInfo_OtherCore* pStringType,
                                                                               TCContext* pTCContext)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    Assert_(pStringType->_coreFlags & STRINGFLAG_IS_COMPACT);
    Assert_(instance.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST);
    TmpTCNode mockNode {};
    TCStatement mockStatement {};
    IRInfo infoPtrToBytes;
    ETCResult eGatherInfo = tc_gather_string_info(&mockNode, instance, pStringType, &mockStatement, pTCContext, 0u,
        &infoPtrToBytes, 0, 0, 0, 0);
    Assert_(eGatherInfo == ETCResult::ETCR_SUCCESS);
    Assert_(irflag_is_known_embd(infoPtrToBytes.uIRandMetaFlags) && irflag_is_or_has_nyka(infoPtrToBytes.uIRandMetaFlags)); // TODO: allow a null string as semantic const ???
    i32 iOffset; u64 uIROfBaseArray = ir_decode_nyka_value(infoPtrToBytes.metaValue.knownValue.uEmbeddedValue, &iOffset);
    Assert_(!ir_is_immediate(uIROfBaseArray));
    IRInfo infoBaseArray;
    ir_get_info_from_non_imm(uIROfBaseArray, pTCContext, &infoBaseArray);
    u8* pCompileTimeFFStringData = infoBaseArray.metaValue.knownValue.pPtrToRawData + iOffset;
    return FFString { pCompileTimeFFStringData };
}

local_func ETCResult tc_gather_array_info(TmpTCNode* pExprIfErr, IRInfo arrayInfo, const TypeInfo_Array* pArrayType,
    TCStatement* pTCStatement, TCContext* pTCContext, u32 uIsAssignableFlag,
    IRInfo* outInfoPtrToData, IRInfo* outInfoCount, u8* outCountFormat, IRInfo* outInfoAllocStruct, IRInfo* outInfoAsRef)
{
    Assert_(uIsAssignableFlag == 0u || uIsAssignableFlag == IR_INSTRFLAG_IS_ASSIGNABLE);
    Assert_(outInfoPtrToData || outInfoCount || outCountFormat || outInfoAllocStruct || outInfoAsRef);

    if (get_array_category_type(pArrayType) == ARRAY_TYPE_KIND_STATIC) {
        if (outInfoAsRef || outInfoPtrToData) {
            IRInfo baseAddress;
            EIRResult eGatherBaseAddress = ir_emit_or_solve_address_of(arrayInfo, pTCStatement, pTCContext, &baseAddress);
            if (eGatherBaseAddress >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherBaseAddress),
                    "tc_gather_array_info() : gather ref address failed...");
            }
            if (outInfoAsRef)
                *outInfoAsRef = baseAddress;
            if (outInfoPtrToData) {
                Assert_(0u == uIsAssignableFlag); // cannot assign to pointer to data from a static array...
                *outInfoPtrToData = baseAddress;
            }
        }
        if (outInfoCount) {
            Assert_(0u == uIsAssignableFlag); // cannot assign to count from a static array...
            *outInfoCount = ir_make_info_for_int_immediate(i32(pArrayType->uElemCount), 0x02u);
        }
        if (outCountFormat) {
            *outCountFormat = 0x02u;
        }
        Assert_(outInfoAllocStruct == 0);

    } else if (get_array_category_type(pArrayType) == ARRAY_TYPE_KIND_BUFFER) {
        IRInfo baseAddress;
        if (outInfoAsRef || outInfoPtrToData || outInfoCount) {
            IRInfo baseAddress;
            EIRResult eGatherBaseAddress = ir_emit_or_solve_address_of(arrayInfo, pTCStatement, pTCContext, &baseAddress);
            if (eGatherBaseAddress >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherBaseAddress),
                    "tc_gather_array_info() : gather ref address failed...");
            }
            if (outInfoAsRef)
                *outInfoAsRef = baseAddress;
            u32 uAlignElemLog2 = get_log2_of_align_bytes(pArrayType->pElementType);
            u32 uAdditionalBytesForSizeMember = align_to(1u << uAlignElemLog2, 4u);
            if (outInfoPtrToData) {
                Assert_(0u == uIsAssignableFlag); // cannot assign to pointer to data from a buffer...
                EIRResult eGatherAddressToData = ir_emit_or_solve_ptr_offset(uAlignElemLog2, baseAddress, 0x02u,
                    ir_make_info_for_int_immediate(i32(uAdditionalBytesForSizeMember), 0x02u), 1u, 0u, EIntSemantics::EINT_SEMANTIC_UNSIGNED,
                    pTCStatement, pTCContext, outInfoPtrToData);
                if (eGatherAddressToData >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddressToData),
                        "tc_gather_array_info() : gather address to data failed...");
                }
            }
            if (outInfoCount) {
                IRInfo infoPtrToCountMember;
                if (uAdditionalBytesForSizeMember == 4u) {
                    infoPtrToCountMember = baseAddress;
                } else {
                    Assert_(uAdditionalBytesForSizeMember > 4u);
                    u32 uOffsetToCountMember = uAdditionalBytesForSizeMember - 4u;
                    EIRResult eGatherAddressToCountMember = ir_emit_or_solve_ptr_offset(uAlignElemLog2, baseAddress, 0x02u,
                        ir_make_info_for_int_immediate(i32(uOffsetToCountMember), 0x02u), 1u, 0u, EIntSemantics::EINT_SEMANTIC_UNSIGNED,
                        pTCStatement, pTCContext, outInfoPtrToData);
                    if (eGatherAddressToCountMember >= EIRResult::EIRR_FIRST_ERROR) {
                        return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddressToCountMember),
                            "tc_gather_array_info() : gather address to data failed...");
                    }
                }
                EIRResult eGatherCountMember = ir_emit_or_solve_deref(infoPtrToCountMember, 0x02u, 2u, 1u, 4u,
                    uIsAssignableFlag, pTCStatement, pTCContext, outInfoCount);
                if (eGatherCountMember >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherCountMember),
                        "tc_gather_array_info() : gather address to data failed...");
                }
            }
        }
        if (outCountFormat) {
            *outCountFormat = 0x02u;
        }
        Assert_(outInfoAllocStruct == 0);

    } else if (get_array_category_type(pArrayType) == ARRAY_TYPE_KIND_SLICE) {

        IRInfo baseStructAddress;
        EIRResult eGatherBaseAddress = ir_emit_or_solve_address_of(arrayInfo, pTCStatement, pTCContext, &baseStructAddress);
        if (eGatherBaseAddress >= EIRResult::EIRR_FIRST_ERROR) {
            return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherBaseAddress),
                "tc_gather_array_info() : gather ref address failed...");
        }
        if (outInfoAsRef)
            *outInfoAsRef = baseStructAddress;
        if (outInfoPtrToData) {
            EIRResult eDerefBaseAsPtrToData = ir_emit_or_solve_deref(baseStructAddress, 0x03u, 3u, 1u, 8u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoPtrToData);
            if (eDerefBaseAsPtrToData >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eDerefBaseAsPtrToData),
                    "tc_gather_array_info() : gather ptr to bytes failed...");
            }
        }
        if (outInfoCount) {
            IRInfo infoAddressOfCount;
            EIRResult eGatherAddress = ir_emit_or_solve_ptr_offset(3u, baseStructAddress, 0x02u, ir_make_info_for_int_immediate(1u, 0x02u), 8u,
                IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoAddressOfCount);
            if (eGatherAddress < EIRResult::EIRR_FIRST_ERROR) {
                EIRResult eGatherCount = ir_emit_or_solve_deref(infoAddressOfCount, 0x03u, 3u, 1u, 8u, uIsAssignableFlag,
                    pTCStatement, pTCContext, outInfoCount);
                if (eGatherCount >= EIRResult::EIRR_FIRST_ERROR) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherCount),
                        "tc_gather_array_info() : deref for count data failed...");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eGatherAddress),
                    "tc_gather_array_info() : gather address for count data failed...");
            }
        }
        if (outCountFormat) {
            *outCountFormat = 0x03u;
        }
        Assert_(outInfoAllocStruct == 0);

    } else {
        // TODO
        return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_gather_array_info() : not yet implemented for other than static-arrays, buffer, or slices...");
    }
    return ETCResult::ETCR_SUCCESS;
}


#endif // LOCLIB_TYPE_CHECKER_BASE_H_
 