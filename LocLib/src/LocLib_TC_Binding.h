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

#ifndef LOCLIB_TC_BINDING_H_
#define LOCLIB_TC_BINDING_H_

#include "LocLib_TypeCheckerTypes.h"
#include "LocLib_TypeCheckerCore.h"
#include "LocLib_TypeCheckerBase.h"
#include "LocLib_IR_SolverInterface.h"
#include "LocLib_TC_Casts.h"

local_func ValueBinding* find_binding_within_enum(int iIdentifierHandle, u64 uHash, const TypeInfo_Enum* pEnum)
{
    auto itFound = pEnum->mapAllMembers.findHashed(uHash, iIdentifierHandle);
    if (itFound != pEnum->mapAllMembers.end()) {
        return pEnum->vecAllMembers[itFound.value()];
    }
    u32 uUsedCount = pEnum->vecUsed.size();
    for (u32 uUsed = 0u; uUsed < uUsedCount; uUsed++) {
        const TypeInfo_Enum* pUsedEnum = pEnum->vecUsed[uUsed];
        ValueBinding* pFoundThere = find_binding_within_enum(iIdentifierHandle, uHash, pUsedEnum);
        if (pFoundThere)
            return pFoundThere;
    }
    return 0;
}

local_func ValueBinding* find_binding_within_structlike(int iIdentifierHandle, u64 uHash,
    const TypeInfo_StructLike* pStructLike, u32* outByteOffsetFromStart, bool* outProperlyAligned)
{
    auto itFound = pStructLike->mapAllMembers.findHashed(uHash, iIdentifierHandle);
    *outProperlyAligned = 0u != (pStructLike->_coreFlags & COMPOUNDTYPE_IS_PACKED) ? false : true;
    *outByteOffsetFromStart = 0u;
    if (itFound != pStructLike->mapAllMembers.end()) {
        ValueBinding* pResult = pStructLike->vecAllMembers[itFound.value()];
        if (!is_value_tc_const(pResult)) {
            // the 'IR' slot for struct members was hacked after typechecking, to represent their byte offset from base.
            *outByteOffsetFromStart = u32(pResult->info.uIRandMetaFlags >> IR_STD_PARAM_SHIFT);
        }
        return pResult;
    }
    u32 uIncludedCount = pStructLike->vecIncluded.size();
    for (u32 uIncluded = 0u; uIncluded < uIncludedCount; uIncluded++) {
        u32 uInclusionPosAsMockBinding = pStructLike->vecIncluded[uIncluded];
        ValueBinding* pAsMockBinding = pStructLike->vecAllMembers[uInclusionPosAsMockBinding];
        Assert_(pAsMockBinding->iIdentifierHandle < 0); // Marker for an included thing indeed...
        Assert_(pAsMockBinding->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]);
        const TypeInfo* pIncludedType = type_from_type_node(pAsMockBinding);
        Assert_(get_type_kind(pIncludedType) == ETypeKind::ETYPEKIND_STRUCTLIKE);
        const TypeInfo_StructLike* pIncludedStructLike = (const TypeInfo_StructLike*)pIncludedType;
        if (is_compound_type_full_typechecked(pStructLike)) { // TODO: CLEANUP:
            // the above check should be enough to prevent any circular search *while* we typecheck some structlike ?
            //    (there will be an independent circular checker on structlike *finalization*).
            u32 uInclusionOffset = u32(pAsMockBinding->info.uIRandMetaFlags >> IR_STD_PARAM_SHIFT); // similar hack with IR slots for those mock bindings
            u32 uOffsetThere; bool bProperlyAlignedThere;
            ValueBinding* pFoundThere = find_binding_within_structlike(iIdentifierHandle, uHash, pIncludedStructLike,
                &uOffsetThere, &bProperlyAlignedThere);
            if (pFoundThere) {
                if (!is_value_tc_const(pFoundThere)) {
                    *outByteOffsetFromStart = uInclusionOffset + uOffsetThere;
                    *outProperlyAligned &= bProperlyAlignedThere;
                }
                return pFoundThere;
            }
        }
    }
    return 0;
}

local_func ValueBinding* find_binding_within_namespace(int iIdentifierHandle, u64 uHash, ReferencedNamespace* pRefNamespace,
    bool bAllowParents, TCContext* pTCContext)
{
    // If ref namespace is in current file, we'll use direct access (and taking *privates* into account)
    if (pRefNamespace->pOrigNamespace->pOriginalSourceFile == pTCContext->pIsolatedSourceFile) {

        TCNamespace* pOrigNamespace = pRefNamespace->pOrigNamespace;

        auto itFound = pOrigNamespace->mapAllGlobalDeclarationsById.findHashed(uHash, iIdentifierHandle);
        if (itFound != pOrigNamespace->mapAllGlobalDeclarationsById.end()) {
            return pTCContext->pIsolatedSourceFile->vecAllGlobalBindings[itFound.value()];
        }
        u32 uCountUsedEnums = pOrigNamespace->vecAllUsedEnums.size();
        for (u32 uUsedEnum = 0u; uUsedEnum < uCountUsedEnums; uUsedEnum++) {
            const TypeInfo_Enum* pUsedEnum = pOrigNamespace->vecAllUsedEnums[uUsedEnum];
            ValueBinding* pFoundThere = find_binding_within_enum(iIdentifierHandle, uHash, pUsedEnum);
            if (pFoundThere)
                return pFoundThere;
        }
        u32 uCountUsedNamespaces = pOrigNamespace->vecAllUsedNamespaces.size();
        for (u32 uUsedNamespace = 0u; uUsedNamespace < uCountUsedNamespaces; uUsedNamespace++) {
            ReferencedNamespace* pUsedNamespace = pOrigNamespace->vecAllUsedNamespaces[uUsedNamespace];
            ValueBinding* pFoundThere = find_binding_within_namespace(iIdentifierHandle, uHash, pUsedNamespace, false, pTCContext);
            if (pFoundThere)
                return pFoundThere;
        }

        // If ref namespace is in current file, then it is common that we ask for an *unqualified* name search, ie. taking current *parents* into account
        if (bAllowParents && pOrigNamespace->pParent) {
            return find_binding_within_namespace(iIdentifierHandle, uHash, (ReferencedNamespace*)pOrigNamespace->pParent, true, pTCContext);
        }

    } else { // Ref namespace is in another file => access through lagged state

        Assert_(!bAllowParents);
        LaggedNamespaceState& laggedState = pRefNamespace->laggedState;

        auto itFound = laggedState.mapKnownAccessibleDeclarationsById.findHashed(uHash, iIdentifierHandle);
        if (itFound != laggedState.mapKnownAccessibleDeclarationsById.end()) {
            return pRefNamespace->pOrigNamespace->pOriginalSourceFile->vecAllGlobalBindings[itFound.value()];
        }
        u32 uCountUsedEnums = laggedState.vecKnownUsedEnums.size();
        for (u32 uUsedEnum = 0u; uUsedEnum < uCountUsedEnums; uUsedEnum++) {
            const TypeInfo_Enum* pUsedEnum = laggedState.vecKnownUsedEnums[uUsedEnum];
            ValueBinding* pFoundThere = find_binding_within_enum(iIdentifierHandle, uHash, pUsedEnum);
            if (pFoundThere)
                return pFoundThere;
        }
        u32 uCountUsedNamespaces = laggedState.vecKnownUsedNamespaces.size();
        for (u32 uUsedNamespace = 0u; uUsedNamespace < uCountUsedNamespaces; uUsedNamespace++) {
            ReferencedNamespace* pUsedNamespace = laggedState.vecKnownUsedNamespaces[uUsedNamespace];
            ValueBinding* pFoundThere = find_binding_within_namespace(iIdentifierHandle, uHash, pUsedNamespace, false, pTCContext);
            if (pFoundThere)
                return pFoundThere;
        }

    }

    return 0;
}

local_func ValueBinding* find_binding_within_proclocal(int iIdentifierHandle, u64 uHash, bool bAllowParents, TCContext* pTCContext)
{
    Assert_(pTCContext->pProcResult);
    Assert_(pTCContext->pProcSource);
    Assert_(pTCContext->pProcSource->pRootTcBlock);
    Assert_(has_ctx_seq_blocks(pTCContext));
    TCSeqSourceBlock* pCurrentBlock = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;
    i32 iBaseScopedDeclIndex = 0;
    if (bAllowParents) {
        Assert_(0uLL == (reinterpret_cast<u64>(pTCContext->pProcSource->pRootTcBlock) & 1uLL));
        iBaseScopedDeclIndex = i32(pTCContext->pProcSource->pRootTcBlock->uFlagsAndScopeBaseIndex & BLOCK_SCOPED_ENTITY_BASE_INDEX_MASK);
    } else {
        iBaseScopedDeclIndex = i32(pCurrentBlock->uFlagsAndScopeBaseIndex & BLOCK_SCOPED_ENTITY_BASE_INDEX_MASK);
    }
    
    i32 iScopedEntityCount = i32(pTCContext->pProcResult->vecScopedEntities.size());
    for (i32 iScopedEntity = iScopedEntityCount-1; iScopedEntity >= iBaseScopedDeclIndex; iScopedEntity--) {
        ScopedEntityHandle hScopedEntity = pTCContext->pProcResult->vecScopedEntities[iScopedEntity];
        switch (get_scoped_entity_kind(hScopedEntity)) {
            case ESCOPEDENTITY_BINDING: {
                ValueBinding* pAsBinding = get_scoped_entity_as_declared_binding(hScopedEntity);
                if (pAsBinding->iIdentifierHandle == iIdentifierHandle)
                    return pAsBinding;
            } break;
            case ESCOPEDENTITY_USE_ENUM: {
                const TypeInfo_Enum* pUsedEnum = get_scoped_entity_as_used_enum(hScopedEntity);
                ValueBinding* pFoundThere = find_binding_within_enum(iIdentifierHandle, uHash, pUsedEnum);
                if (pFoundThere)
                    return pFoundThere;
            } break;
            case ESCOPEDENTITY_USE_NAMESPACE: {
                ReferencedNamespace* pUsedNamespace = get_scoped_entity_as_used_namespace(hScopedEntity);
                ValueBinding* pFoundThere = find_binding_within_namespace(iIdentifierHandle, uHash, pUsedNamespace, false, pTCContext);
                if (pFoundThere)
                    return pFoundThere;
            } break;
            default: Assert_(get_scoped_entity_kind(hScopedEntity) == ESCOPEDENTITY_DEFER); // And NOOP
        }
    }

    if (bAllowParents) {
        const TmpArray<ProcLikeParam>& vecParams = pTCContext->pProcSource->procSign->params;
        u32 uCountParams = vecParams.size();
        for (u32 uParam = 0u; uParam < uCountParams; uParam++) {
            const ProcLikeParam& param = vecParams[uParam];
            if (param.iIdentifier == iIdentifierHandle)
                return param.pBinding;
        }
    }
    return 0;
}

local_func ValueBinding* tc_find_binding_from_identifier(TCContext* pTCContext, int iIdentifierHandle, EIdentAttributes eAttr)
{
    u64 uHash = get_map_hash(iIdentifierHandle);

    // TODO ? special proc-sign context for when typechecking proc sign ? allow referencing already seen ids here.
    //   Or rather, locally to that...
    
    if (pTCContext->pProcSource) {

        Assert_(pTCContext->pProcResult);
        Assert_(!is_ctx_compound(pTCContext));
        ValueBinding* pFoundLocally = find_binding_within_proclocal(iIdentifierHandle, uHash, true, pTCContext);
        if (pFoundLocally)
            return pFoundLocally;

    } else if (is_ctx_compound(pTCContext)) {

        Assert_(pTCContext->pCompoundToTC);

        if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_ENUM) {

            ValueBinding* pFoundWithinCurrent = find_binding_within_enum(iIdentifierHandle, uHash,
                (const TypeInfo_Enum*)pTCContext->pCompoundToTC->pCompoundType);
            if (pFoundWithinCurrent != 0)
                return pFoundWithinCurrent;

        } else { Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE);

            u32 uUnusedOffset; bool bUnusedIsAlign; // We do not care about offsets and aligns when *currently* typechecking structlike bodies:
            // We can indeed try to access values of any discovered *consts*, but offset and aligns of runtime members won't be setup yet.
            // But that should be okay(ish?) since whatever the reason TC would ask for it at that point, it should tc-error on non-const ?
            ValueBinding* pFoundWithinCurrent = find_binding_within_structlike(iIdentifierHandle, uHash,
                (const TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType, &uUnusedOffset, &bUnusedIsAlign);
            if (pFoundWithinCurrent != 0)
                return pFoundWithinCurrent;
        }
    }

    // in all cases above, if we did not find anything (and in global contexts otherwise):
    //    we finally try from "current", "unqualified", "global" namespace (including. parent namespaces, if any)
    return find_binding_within_namespace(iIdentifierHandle, uHash, (ReferencedNamespace*)pTCContext->pNamespace, true, pTCContext);
}


local_func int get_id_from_decl_node(TmpTCNode* pDecl, TCStatement* pTCStatement)
{
    Assert_(is_node_already_typechecked(pDecl->pTCNode));
    Assert_(pDecl->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_ASTNODE]);
    Assert_(is_value_tc_only(pDecl->pIntrinsicValue));
    u32 uReferredToNodeIndex = u32(pDecl->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue);
    TCNode* pNodeThere = pTCStatement->vecNodes[uReferredToNodeIndex];
    Assert_(u8(pNodeThere->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ATOMICEXPR_IDENTIFIER);
    int iIdentifierHandle = int(pNodeThere->ast.uPrimaryPayload);
    Assert_(iIdentifierHandle != ERES_INVALID_ID);
    Assert_(iIdentifierHandle >= COUNT_RESERVED_WORDS);
    return iIdentifierHandle;
}

local_func ETCResult do_const_binding(TmpTCNode* pDecl, TmpTCNode* pValue, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_already_typechecked(pDecl->pTCNode));
    Assert_(pDecl->pIntrinsicValue->pType); // sinks should have been ruled out
    if (is_node_already_type_casted(pDecl->pTCNode)) { // special for decls, without truly a cast value.
        return ETCResult::ETCR_SUCCESS;                // ...indicates already bound => NOOP and returning success 
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Performing single-binding as constant"), pTCContext->pWorker);

    // 'regular' const declaration NOT allowed in enums, caller should have prevented this (explicit values in enum are set using the '=' token instead)
    Assert_(!is_ctx_compound(pTCContext) || get_type_kind(pTCContext->pCompoundToTC->pCompoundType) != ETYPEKIND_ENUM);

    Assert_(is_node_already_typechecked(pValue->pTCNode));
    NodeValue* pResultingValue = pValue->pIntrinsicValue;
    Assert_(is_value_tc_const(pResultingValue));

    int iIdentifierHandle = get_id_from_decl_node(pDecl, pTCStatement);
    Assert_(iIdentifierHandle >= COUNT_RESERVED_WORDS);

    ValueBinding* pBinding = (ValueBinding*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 
        sizeof(ValueBinding), alignof(ValueBinding));
    set_binding_source_ref(pBinding, pTCStatement, pTCContext, EDeclAttributes::EDECLATTR_REGULAR_CONST);
    *((NodeValue*)pBinding) = *pResultingValue;
    Assert_(pBinding->info.uIRandMetaFlags == pResultingValue->info.uIRandMetaFlags);
    pBinding->info.uIRandMetaFlags |= IRFLAG_TC_BINDING_INSTANCE;
    if (!is_value_tc_only(pBinding)) {
        Assert_(ir_is_valid_param_(pBinding->info.uIRandMetaFlags));
        if (ir_is_immediate(pBinding->info.uIRandMetaFlags)) {
            // if value is an immediate, we need to create a new declaration entry, for it to become referencable.
            u32 uIRPos = pTCContext->pRepo->uSize;
            IREntry* pDecl = ir_append_new_entry(pTCContext->pRepo);
            pDecl->uInstrCodeAndFormatAndFirstParam = u64(IRIT_DECLARATION) | (u64(get_ir_format(pBinding->pType)) << 16);
            u64 uSlotCountAndAlign = u64(get_slots_count(pBinding->pType)) | (u64(get_log2_of_align_bytes(pBinding->pType)) << 32);
            pDecl->uInstrMetaFlagsAndSecondParam = (pBinding->info.uIRandMetaFlags & IR_STD_PARAM_METAMASK) | (uSlotCountAndAlign << IR_STD_PARAM_SHIFT);
            pDecl->metaValue = pBinding->info.metaValue;
            
            pBinding->info.uIRandMetaFlags &= IR_STD_PARAM_METAMASK;
            pBinding->info.uIRandMetaFlags |= pTCContext->pRepo->uIRRepoId == IR_REPO_ID_CURRENT_PROC ? 
                ir_make_std_code_in_cur_proc(uIRPos) : ir_make_global_const_code_in_file(pTCContext->pIsolatedSourceFile->iRegistrationIndex, uIRPos);
        }
        pBinding->info.uIRandMetaFlags |= IRFLAG_TC_REFERENCABLE;
    } else {
        Assert_(!is_value_tc_referencable(pBinding));
    }
    pBinding->iIdentifierHandle = iIdentifierHandle;
    pBinding->_reserved = 0;

    if (pTCContext->pProcSource) {

        Assert_(pTCContext->pProcResult);
        Assert_(!is_ctx_compound(pTCContext));
        Assert_(has_ctx_seq_blocks(pTCContext));
        //TmpArray<ValueBinding*>* pVecRegistry = &(pTCContext->pProcResult->vecBindings);
        //u32 uRegistrationPos = pVecRegistry->size();
        //pBinding->uScopeAndLocation = (uRegistrationPos << 8) | EScopeKind::SCOPEKIND_PROC_BLOCK_LOCAL;
        //pVecRegistry->append(pBinding);
        //TCSeqSourceBlock* pDeclBlock = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;
        //pDeclBlock->pMapBlockDeclarationsById->insert(iIdentifierHandle, uRegistrationPos);
        u32 uRegistrationPos = pTCContext->pProcResult->vecBindings.size();
        pBinding->uScopeAndLocation = (uRegistrationPos << 8) | EScopeKind::SCOPEKIND_PROC_BLOCK_LOCAL;
        pTCContext->pProcResult->vecBindings.append(pBinding);
        pTCContext->pProcResult->vecScopedEntities.append(make_scoped_entity(pBinding));

    } else if (is_ctx_compound(pTCContext)) {

        Assert_(pTCContext->pCompoundToTC);
        Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE);
        TypeInfo_StructLike* pAsStructLike = (TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType;
        Assert_(pTCContext->pCurrentBlock == pTCContext->pCompoundToTC->pRootTcBlock);

        u32 uStatementIndex = pTCStatement->uStatementIndexInBlock;
        Assert_(0 == (uStatementIndex & 0xF800'0000u));

        u32 uRegistrationPos = pAsStructLike->vecAllMembers.size();

        pAsStructLike->vecAllMembers.append(pBinding);
        pAsStructLike->mapAllMembers.insert(iIdentifierHandle, uRegistrationPos);

        pBinding->uScopeAndLocation = EScopeKind::SCOPEKIND_COMPOUND | (uRegistrationPos << 8);
        pBinding->uCompoundDeclSort = (uStatementIndex << 5); // we don't really care about index in statement for consts.

    } else { Assert_(is_ctx_global(pTCContext)); // filewise global const

        u32 uRegistrationPos = pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.size();
        pBinding->uScopeAndLocation = (uRegistrationPos << 8) | pTCContext->eGlobalDeclScope;
        pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.append(pBinding);
        pTCContext->pNamespace->mapAllGlobalDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
        if (pTCContext->eGlobalDeclScope <= EScopeKind::SCOPEKIND_GLOBAL_PACKAGE) {
            pTCContext->pNamespace->mapAccessibleDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
            if (pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PUBLIC)
                pTCContext->pNamespace->mapPublicDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
        } else {
            Assert_(pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PRIVATE);
        }

        pTCContext->pNamespace->setOfNewlyDeclaredGlobalIdentifiers.insert(iIdentifierHandle);
    }

    switch (get_type_kind(pResultingValue->pType)) {
        case ETypeKind::ETYPEKIND_PROCLIKEBODY: {
            Assert_(ir_is_valid_param_(pResultingValue->info.uIRandMetaFlags));
            Assert_(irflag_is_known_or_nyka(pResultingValue->info.uIRandMetaFlags)); // Since this is a const...
            Assert_(irflag_is_or_has_nyka(pResultingValue->info.uIRandMetaFlags));   // Since this is a known *proc*
            Assert_(irflag_is_known_embd(pResultingValue->info.uIRandMetaFlags));    // Since this should be a single nyka...
            u64 uNykaOfProc = pResultingValue->info.metaValue.knownValue.uEmbeddedValue;
            i32 expect0;
            u64 uIRofProc = ir_decode_nyka_value(uNykaOfProc, &expect0);
            Assert_(0 == expect0);
            Assert_(!ir_is_immediate(pResultingValue->info.uIRandMetaFlags) ||
                    (ir_is_nyka_immediate(pResultingValue->info.uIRandMetaFlags) && ir_get_param_from_nyka_immediate(pResultingValue->info.uIRandMetaFlags) == uIRofProc));
            IRRepo* pRepo;
            u32 uIndex;
            SourceFileDescAndState* pSourceFile;
            EEntryKind eKind;
            ir_decode_non_imm(uIRofProc, pTCContext, &pRepo, &uIndex, &pSourceFile, &eKind);
            Assert_(eKind == EEntryKind::EEK_IS_PROCBODY_REF);
            Assert_(pRepo == 0);
            Assert_(pSourceFile == pTCContext->pIsolatedSourceFile); // TODO: check this assumption ?
            TCProcBodyRegistration* pProcReg = pSourceFile->vecAllProcBodies[uIndex];
            if (pProcReg->procResult.iPrimaryIdentifier <= 0) { // if proc not yet assigned a primary identifier: assign current id to be bound to that proc primary.
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("First binding of proc %u in file %u => bound to primary identifier '%s'",
                    u64(uIndex), u64(u32(pSourceFile->iRegistrationIndex)), reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str())), pTCContext->pWorker);
                pProcReg->procResult.iPrimaryIdentifier = iIdentifierHandle;
            }
        } break;

        case ETYPEKIND_OTHERCORE: {
            if (pResultingValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                const TypeInfo* pTypeValue = type_from_type_node(pResultingValue);
                switch (get_type_kind(pTypeValue)) {
                    case ETYPEKIND_STRUCTLIKE:
                    case ETYPEKIND_ENUM: {
                        const TypeInfo_CompoundBase* pAsCompound = (const TypeInfo_CompoundBase*)pTypeValue;
                        Assert_(pAsCompound->pRegistration);
                        if (pAsCompound->pRegistration->iPrimaryIdentifier <= 0) { // if compound not yet assigned a primary identifier: assign current id to be bound to that proc primary.
                            pAsCompound->pRegistration->iPrimaryIdentifier = iIdentifierHandle;
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("First binding of compound %u in file %u => bound to primary identifier '%s'",
                                u64(pAsCompound->uRegistrationIndex), u64(u32(pTCContext->pIsolatedSourceFile->iRegistrationIndex)), reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str())), pTCContext->pWorker);
                        }
                    } break;
                }
            }
        } break;
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Successfully bound identifier '%s' (%u) as a constant in current context",
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str()), u64(u32(iIdentifierHandle))), pTCContext->pWorker);
    return set_node_type_cast_expr_success(pDecl->pTCNode); // special for decls, without truly a cast value.
}

constexpr u64 tLargeConstAllZeroes[4096u] = {};

local_func ETCResult do_var_binding(TmpTCNode* pDecl, TmpTCNode* pOptType, TmpTCNode* pOptValue,
    TCStatement* pTCStatement, TCContext* pTCContext, TmpTCNode* tAllDeclBase = 0)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Performing single-binding as variable"), pTCContext->pWorker);

    Assert_(is_node_already_typechecked(pDecl->pTCNode));
    Assert_(pDecl->pIntrinsicValue->pType); // sinks should have been ruled out
    Assert_(pOptType || pOptValue);

    // Variable declaration NOT allowed in enums, caller should have prevented this
    Assert_(!is_ctx_compound(pTCContext) || get_type_kind(pTCContext->pCompoundToTC->pCompoundType) != ETYPEKIND_ENUM);

    const TypeInfo* pExplicitType = 0;
    if (pOptType) {
        Assert_(is_node_already_typechecked(pOptType->pTCNode));
        Assert_(is_value_tc_const(pOptType->pIntrinsicValue));
        if (pOptType->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
            Assert_(is_value_tc_only(pOptType->pIntrinsicValue));
            pExplicitType = type_from_type_node(pOptType->pIntrinsicValue);
            check_type_availability_may_return_wait_or_error(pExplicitType, pDecl, pTCStatement, pTCContext, "do_var_binding() : cannot finalize binding to ");
            u16 uTypeErr = 0;
            if (!is_allowed_as_runtime_type(pExplicitType, pTCContext, &uTypeErr)) {
                if (is_ctx_compound(pTCContext) && 
                    0 != (pTCContext->pCompoundToTC->pCompoundType->_coreType & COMPOUNDTYPE_IS_COMPTIME_ONLY)) {
                    // TODO
                    return_error(pOptType, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "do_var_binding() : authorized declaration of comptime-only member within compound with comptime-only flag... not yet implemented");
                } else {
                    return_error(pOptType, pTCStatement, pTCContext, uTypeErr,
                        "do_var_binding() : unallowed non-runtime type in explicit type-slot");
                }
            }
        } else {
            return_error(pOptType, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                "do_var_binding() : expected Type in type-slot");
        }
    } else {
        if (is_ctx_compound(pTCContext)) {
            return_error(pOptType, pTCStatement, pTCContext, CERR_STRUCT_OR_UNION_MEMBER_DECLARATION_MUST_BE_TYPE_ONLY,
                "do_var_binding() : within struct or union, expected var declaration with explicit type (and no initial value)");
        }
        Assert_(pOptValue);
        Assert_(pOptValue->pIntrinsicValue->pType); // explicit no-init should have been ruled out by caller if no type...
    }

    int iIdentifierHandle = get_id_from_decl_node(pDecl, pTCStatement);
    Assert_(iIdentifierHandle >= COUNT_RESERVED_WORDS);

    ValueBinding* pBinding = (ValueBinding*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 
        sizeof(ValueBinding), alignof(ValueBinding));
    set_binding_source_ref(pBinding, pTCStatement, pTCContext, EDeclAttributes::EDECLATTR_REGULAR_CONST);
    pBinding->iIdentifierHandle = iIdentifierHandle;
    pBinding->info.uIRandMetaFlags = IRFLAG_TC_BINDING_INSTANCE|IRFLAG_TC_REFERENCABLE;
    pBinding->info.metaValue._payload = 0uLL;

    if (pTCContext->pProcSource) {

        Assert_(pTCContext->pProcResult);
        Assert_(!is_ctx_compound(pTCContext));
        Assert_(has_ctx_seq_blocks(pTCContext));
        //TmpArray<ValueBinding*>* pVecRegistry = &(pTCContext->pProcResult->vecBindings);
        //u32 uRegistrationPos = pVecRegistry->size();
        //pBinding->uScopeAndLocation = (uRegistrationPos << 8) | EScopeKind::SCOPEKIND_PROC_BLOCK_LOCAL;
        //pVecRegistry->append(pBinding);
        //TCSeqSourceBlock* pDeclBlock = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;
        //pDeclBlock->pMapBlockDeclarationsById->insert(iIdentifierHandle, uRegistrationPos);
        u32 uRegistrationPos = pTCContext->pProcResult->vecBindings.size();
        pBinding->uScopeAndLocation = (uRegistrationPos << 8) | EScopeKind::SCOPEKIND_PROC_BLOCK_LOCAL;
        pTCContext->pProcResult->vecBindings.append(pBinding);
        pTCContext->pProcResult->vecScopedEntities.append(make_scoped_entity(pBinding));

        IRRepo* pLocalRepo = &(pTCContext->pProcResult->procwiseRepo);
        u32 uIRPos = pLocalRepo->uSize;
        IREntry* pDecl = ir_append_new_entry(pLocalRepo);

        NodeValue* pExplicitInitialValue = 0;
        const TypeInfo* pResultingType;
        if (pOptValue) {
            Assert_(is_node_already_typechecked(pOptValue->pTCNode));
            pExplicitInitialValue = pOptValue->pIntrinsicValue;
            if (pExplicitInitialValue->pType) { // Otherwise, denotes an explicit uninitialization
                if (pExplicitType) {
                    pResultingType = pExplicitType;
                } else {
                    pResultingType = when_no_explicit_cast_get_runtime_type(pExplicitInitialValue->pType, pTCContext);
                    u16 uTypeErr = 0;
                    if (!is_allowed_as_runtime_type(pResultingType, pTCContext, &uTypeErr)) {
                        return_error(pOptType, pTCStatement, pTCContext, uTypeErr,
                            "do_var_binding() : unallowed non-runtime type inferred from value");
                    }
                }
                if (!are_types_same(pResultingType, pExplicitInitialValue->pType, pTCContext)) {
                    ETCResult checkConv = do_implicit_cast(pOptValue, pResultingType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR);
                    if (checkConv != ETCResult::ETCR_SUCCESS)
                        return checkConv;
                    Assert_(is_node_already_type_casted(pOptValue->pTCNode));
                    pExplicitInitialValue = pOptValue->pFinalValue;
                }
            } else {
                Assert_(pExplicitType);
                pResultingType = pExplicitType;
            }
        } else {
            Assert_(pExplicitType);
            pResultingType = pExplicitType;
        }

        pBinding->pType = pResultingType;
        u8 uResultingFormat = get_ir_format(pResultingType);
        u32 uResultingSlotsCount = get_slots_count(pResultingType);
        pDecl->uInstrCodeAndFormatAndFirstParam = u64(IRIT_LOCAL_VAR_DECL) | u64(IR_INSTRFLAG_IS_ASSIGNABLE) | (u64(uResultingFormat) << 16);
        u64 uSlotCountAndAlign = u64(uResultingSlotsCount) | (u64(get_log2_of_align_bytes(pResultingType)) << 32);
        pDecl->uInstrMetaFlagsAndSecondParam = (u64(uSlotCountAndAlign) << IR_STD_PARAM_SHIFT);
        pDecl->metaValue._payload = 0uLL;

        u64 uIRofDecl = ir_make_std_code_in_cur_proc(uIRPos);
        pBinding->info.uIRandMetaFlags |= uIRofDecl;

        if (pExplicitInitialValue) {
            if (pExplicitInitialValue->pType) { // Otherwise, denotes an explicit uninitialization
                Assert_(ir_is_valid_param_(pExplicitInitialValue->info.uIRandMetaFlags));
                Assert_(!is_value_pseudo_valued_cond(pExplicitInitialValue));
                do_store_value_to(uIRofDecl, pExplicitInitialValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK,
                    uResultingFormat, uResultingSlotsCount, pTCStatement, pTCContext);
            } // Otherwise NOOP after declaration
        } else { // No explicit => implicit zeroed
            u32 uPosOfReset = ir_emit_reset_to_zero(uIRofDecl, uResultingFormat, ir_make_int_immediate(i32(uResultingSlotsCount)), pLocalRepo, pTCContext);
            pTCStatement->uLastIRorGlobalTCResult = uPosOfReset;
        }

    } else if (is_ctx_compound(pTCContext)) {

        Assert_(pTCContext->pCompoundToTC);

        Assert_(pTCContext->pCurrentBlock == pTCContext->pCompoundToTC->pRootTcBlock);
        Assert_(tAllDeclBase);
        Assert_(pDecl - tAllDeclBase < 32);
        u32 uDeclIndexInStatement = u32(pDecl - tAllDeclBase);
        u32 uStatementIndex = pTCStatement->uStatementIndexInBlock;
        Assert_(0 == (uStatementIndex & 0xF800'0000u));
        if (!pOptValue) {

            Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETYPEKIND_STRUCTLIKE); // no vars for enums
            TypeInfo_StructLike* pAsStructLike = (TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType;

            u32 uRegistrationPos = pAsStructLike->vecAllMembers.size();
            pAsStructLike->vecAllMembers.append(pBinding);
            pAsStructLike->mapAllMembers.insert(iIdentifierHandle, uRegistrationPos);

            pBinding->uScopeAndLocation = EScopeKind::SCOPEKIND_COMPOUND | (uRegistrationPos << 8);
            pBinding->uCompoundDeclSort = (uStatementIndex << 5) | uDeclIndexInStatement;
            pBinding->pType = pExplicitType;

        } else {
            return_error(pOptType, pTCStatement, pTCContext, CERR_STRUCT_OR_UNION_MEMBER_DECLARATION_MUST_BE_TYPE_ONLY,
                "do_var_binding() : within struct or union, all 'var' member declaration are expected of the explicit-type form, with no initial value");
        }

    } else { Assert_(is_ctx_global(pTCContext)); // filewise global var
    
        SourceFileDescAndState* pSourceFileForGlobalDecl = pTCContext->pIsolatedSourceFile;
        u32 uRegistrationPos = pSourceFileForGlobalDecl->vecAllGlobalBindings.size();
        pBinding->uScopeAndLocation = (uRegistrationPos << 8) | pTCContext->eGlobalDeclScope;

        u32 uIRPos = pSourceFileForGlobalDecl->filewiseGlobalVarRepo.uSize;
        IREntry* pDecl = ir_append_new_entry(&(pSourceFileForGlobalDecl->filewiseGlobalVarRepo));

        if (pOptValue) {
            Assert_(is_node_already_typechecked(pOptValue->pTCNode));
            NodeValue* pResultingInitialValue = pOptValue->pIntrinsicValue;
            Assert_(pResultingInitialValue->pType); // explicit no-init should have been ruled out here
            Assert_(is_value_tc_const(pResultingInitialValue));
            const TypeInfo* pResultingType;
            if (pExplicitType)
                pResultingType = pExplicitType;
            else {
                pResultingType = when_no_explicit_cast_get_runtime_type(pResultingInitialValue->pType, pTCContext);
                u16 uTypeErr = 0;
                if (!is_allowed_as_runtime_type(pResultingType, pTCContext, &uTypeErr)) {
                    return_error(pOptType, pTCStatement, pTCContext, uTypeErr,
                        "do_var_binding() : unallowed non-runtime type inferred from value");
                }
            }
            pBinding->pType = pResultingType;

            if (!are_types_same(pResultingType, pResultingInitialValue->pType, pTCContext)) {
                ETCResult checkConv = do_implicit_cast(pOptValue, pResultingType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT);
                if (checkConv != ETCResult::ETCR_SUCCESS)
                    return checkConv;
                Assert_(is_node_already_type_casted(pOptValue->pTCNode));
                pResultingInitialValue = pOptValue->pFinalValue;
                Assert_(is_value_tc_const(pResultingInitialValue));
            }

            pDecl->uInstrCodeAndFormatAndFirstParam = u64(IRIT_GLOBAL_VAR_DECL) | u64(IR_INSTRFLAG_IS_ASSIGNABLE) | (u64(get_ir_format(pResultingType)) << 16);
            u64 uSlotCountAndAlign = u64(get_slots_count(pResultingType)) | (u64(get_log2_of_align_bytes(pResultingType)) << 32);
            pDecl->uInstrMetaFlagsAndSecondParam = (pResultingInitialValue->info.uIRandMetaFlags & IR_STD_PARAM_METAMASK) | (u64(uSlotCountAndAlign) << IR_STD_PARAM_SHIFT);
            pDecl->metaValue = pResultingInitialValue->info.metaValue;

        } else {
            Assert_(pExplicitType);
            pBinding->pType = pExplicitType;
            pDecl->uInstrCodeAndFormatAndFirstParam = u64(IRIT_GLOBAL_VAR_DECL) | u64(IR_INSTRFLAG_IS_ASSIGNABLE) |  (u64(get_ir_format(pExplicitType)) << 16);
            u64 uSlotCountAndAlign = u64(get_slots_count(pExplicitType)) | (u64(get_log2_of_align_bytes(pExplicitType)) << 32);
            pDecl->uInstrMetaFlagsAndSecondParam = (uSlotCountAndAlign << IR_STD_PARAM_SHIFT);
            u32 uRuntimeByteSize = get_runtime_sizeof(pExplicitType);
            if (uRuntimeByteSize <= 8u) {
                pDecl->uInstrMetaFlagsAndSecondParam |= u64(IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO);
                pDecl->metaValue.knownValue.uEmbeddedValue = 0uLL;
            } else {
                pDecl->uInstrMetaFlagsAndSecondParam |= u64(IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_ZERO);
                if (uRuntimeByteSize <= 4096u * 8u)
                    pDecl->metaValue.knownValue.pPtrToRawData = const_cast<u8*>(reinterpret_cast<const u8*>(tLargeConstAllZeroes));
                else {
                    u8* pAllocZeroes = alloc_from(pSourceFileForGlobalDecl->localArena, uRuntimeByteSize, 8u);
                    memset(pAllocZeroes, 0x00u, uRuntimeByteSize);
                    pDecl->metaValue.knownValue.pPtrToRawData = pAllocZeroes;
                }
            }
        }

        pBinding->info.uIRandMetaFlags |= ir_make_global_var_code_in_file(u32(pSourceFileForGlobalDecl->iRegistrationIndex), uIRPos);

        pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.append(pBinding);
        pTCContext->pNamespace->mapAllGlobalDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
        if (pTCContext->eGlobalDeclScope <= EScopeKind::SCOPEKIND_GLOBAL_PACKAGE) {
            pTCContext->pNamespace->mapAccessibleDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
            if (pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PUBLIC)
                pTCContext->pNamespace->mapPublicDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
        } else {
            Assert_(pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PRIVATE);
        }

        pTCContext->pNamespace->setOfNewlyDeclaredGlobalIdentifiers.insert(iIdentifierHandle);
    }

    Assert_(!is_value_tc_const(pBinding));
    Assert_(ir_is_valid_param_(pBinding->info.uIRandMetaFlags) || is_ctx_compound(pTCContext)); // vars in compounds hack the IR slot
    Assert_(is_value_tc_referencable(pBinding));

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Successfully bound identifier '%s' (%u) as a variable in current context",
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str()), u64(u32(iIdentifierHandle))), pTCContext->pWorker);
    return set_node_type_cast_expr_success(pDecl->pTCNode); // special for decls, without truly a cast value.
}

local_func bool _check_not_circular_using_at(TCNamespace* pUsedNamespace, TmpSet<TCNamespace*>* ioSetUsed)
{
    Assert_(ioSetUsed->find(pUsedNamespace) == ioSetUsed->end());
    u32 uCountUsingThere = pUsedNamespace->vecAllUsedNamespaces.size();
    if (uCountUsingThere < 1u) {
        return true;
    } else {
        if (uCountUsingThere == 1u) {
            TCNamespace* pUsed = pUsedNamespace->vecAllUsedNamespaces[0u]->pOrigNamespace;
            if (ioSetUsed->find(pUsed) == ioSetUsed->end()) {
                ioSetUsed->insert(pUsedNamespace);
                return _check_not_circular_using_at(pUsed, ioSetUsed);
            } else
                return false;
        } else { Assert_(uCountUsingThere > 1u);
            ioSetUsed->insert(pUsedNamespace);
            for (u32 uUsed = 0u; uUsed < uCountUsingThere; uUsed++) {
                TCNamespace* pUsed = pUsedNamespace->vecAllUsedNamespaces[uUsed]->pOrigNamespace;
                if (ioSetUsed->find(pUsed) == ioSetUsed->end()) {
                    TmpSet<TCNamespace*> setUsedTmp;
                    setUsedTmp.init(ioSetUsed->_alloc, *ioSetUsed);
                    if (!_check_not_circular_using_at(pUsed, &setUsedTmp))
                        return false;
                } else
                    return false;
            }
            return true;
        }
    }
}

local_func bool check_not_circular_using_from(TCNamespace* pSrcNamespace, TCNamespace* pUsedNamespace, Arena tmpArena)
{
    if (pUsedNamespace == pSrcNamespace)
        return false;
    ArenaRefPoint refPointBefore = get_arena_ref_point(tmpArena);
    TmpSet<TCNamespace*> setUsed;
    setUsed.init(FireAndForgetArenaAlloc(tmpArena));
    setUsed.insert(pSrcNamespace);
    bool bResult = _check_not_circular_using_at(pUsedNamespace, &setUsed);
    reset_arena_no_release_to(refPointBefore, tmpArena);
    return bResult;
}


#endif // LOCLIB_TC_BINDING_H_
  