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

#ifndef LOCLIB_TC_ADVANCED_DECLARATIONS_H_
#define LOCLIB_TC_ADVANCED_DECLARATIONS_H_

#include "LocLib_TypeCheckerTypes.h"
#include "LocLib_TypeCheckerCore.h"
#include "LocLib_TypeCheckerBase.h"
#include "LocLib_IR_SolverInterface.h"
#include "LocLib_TC_Casts.h"



local_func ETCResult add_proclike_param_to(TmpArray<ProcLikeParam>* ioVecParams,
    int iIdentifierHandle, const TypeInfo* pExplicitType, u32 uOptDefaultValueNodeIndex,
    TmpTCNode* pParam, TCStatement* pTCStatement, TCContext* pTCContext)
{
    u32 uPosInIR = ioVecParams->size();
    if (uPosInIR >= 31) {
        return_error(pParam, pTCStatement, pTCContext, CERR_TOO_MANY_PROC_PARAMETERS,
            "add_proclike_param_to() : proc parameters count (in+out) has a hard maximum of 31");
    }

    ProcLikeParam param;
    param.iIdentifier = iIdentifierHandle;
    param.uOptDefaultValueNodeIndex = uOptDefaultValueNodeIndex;
    param.pBinding = (ValueBinding*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
        sizeof(ValueBinding), alignof(ValueBinding));
    set_binding_source_ref(param.pBinding, pTCStatement, pTCContext, EDeclAttributes::EDECLATTR_REGULAR_VAR);
    param.pBinding->iIdentifierHandle = iIdentifierHandle;
    param.pBinding->uScopeAndLocation = EScopeKind::SCOPEKIND_PROC_PARAM | (uPosInIR << 8);
    param.pBinding->info.uIRandMetaFlags = ir_make_std_code_in_cur_proc(uPosInIR);
    param.pBinding->info.metaValue._payload = 0uLL;

    if (uOptDefaultValueNodeIndex != INVALID_NODE_INDEX) {
        // TODO
        return_error(pParam, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "add_proclike_param_to() : default values not yet implemented");
    }

    if (pExplicitType) {
        u16 uErrIfFalse = 0;
        // TODO: support for parametric types
        Assert_(is_allowed_as_runtime_type(pExplicitType, pTCContext, &uErrIfFalse));
        param.pBinding->pType = pExplicitType;
        Assert_(get_type_kind(pExplicitType) < COUNT_TYPE_KINDS);
        ioVecParams->append(param);
        return set_node_type_cast_expr_success(pParam->pTCNode); // marker for bound
    } else {
        Assert_(uOptDefaultValueNodeIndex != INVALID_NODE_INDEX);
        // TODO: how to handle type inferrence if default-value can be runtime and would have to be typechecked with IR emission
        //   at the start of proc-body resolution ?
        return_error(pParam, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "add_proclike_param_to() : param with type inferrence from default value not yet implemented");
    }
}


local_func ETCResult typecheck_macro_or_paramdecl_to(TmpArray<ProcLikeParam>* ioVecParams, TmpTCNode* pParam,
    TCStatement* pTCStatement, TCContext* pTCContext, u32 uIndexOfOptNodeChainParent, bool* outWasMacroExpansion)
{
    *outWasMacroExpansion = false;
    u8 uNodeKind = u8(pParam->pTCNode->ast.uNodeKindAndFlags);
    Assert_(uNodeKind != ENodeKind::ENODE_EXPRLIST_NODE); // the caller should be discriminating against list-nodes already

    if (uNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
        Assert_(u8(pParam->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_INVOCATION);
        // proc invocations are to be treated separately, since they could very well be macros and require expansion...
        // furthermore, they may expand to multiple elements in a list on the right or left side of some statements.
        ETCResult checkInvoc = typecheck_invocation_form(pParam, pTCStatement,
            pTCContext, EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{},
            uIndexOfOptNodeChainParent, 0, 0, EInvocFormResultCount::EINVOC_RETURNS_ONE,
            outWasMacroExpansion);
        if (checkInvoc != ETCResult::ETCR_SUCCESS || *outWasMacroExpansion)
            return checkInvoc;
        goto on_typeckecked_as_constant_expression;

    } else if (uNodeKind == ENodeKind::ENODE_VARIABLE_DECL) {

        // TODO: support for parametric variables (ie static values)
        TmpTCNode varDecl = init_tmp_tc_node(pParam->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
        ETCResult checkDecl = typecheck_expression(&varDecl, pTCStatement, pTCContext,
            EExpectedExpr::EXPECT_DECLARABLE, UpwardsInference{});
        success_or_return_wait_or_error(checkDecl, pParam->pTCNode);

        TmpTCNode typeOrTypeAndInit = init_tmp_tc_node(pParam->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
        TmpTCNode tmp;
        TmpTCNode* pTypeNode = &typeOrTypeAndInit;
        u32 uOptDefaultValueNodeIndex = INVALID_NODE_INDEX;
        if (u8(typeOrTypeAndInit.pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPR_SINGLE_EQ) {
            if (pParam->pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                tmp = init_tmp_tc_node(pParam->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                pTypeNode = &tmp;
            } else
                pTypeNode = 0;
            uOptDefaultValueNodeIndex = typeOrTypeAndInit.pTCNode->ast.uSecondaryChildNodeIndex;
        }

        const TypeInfo* pExplicitType = 0;
        if (pTypeNode) {
            // TODO: support for parametric types
            ETCResult checkType = typecheck_expression(pTypeNode, pTCStatement, pTCContext,
                EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
            success_or_return_wait_or_error(checkType, pParam->pTCNode);
            Assert_(is_node_already_typechecked(pTypeNode->pTCNode));
            Assert_(is_value_tc_const(pTypeNode->pIntrinsicValue));
            if (pTypeNode->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                Assert_(is_value_tc_only(pTypeNode->pIntrinsicValue));
                pExplicitType = type_from_type_node(pTypeNode->pIntrinsicValue);
                Assert_(get_type_kind(pExplicitType) < COUNT_TYPE_KINDS);
                check_type_footprint_availability_may_return_wait_or_error(pExplicitType, pParam, pTCStatement, pTCContext,
                    "Cannot typecheck a proc signature with");
                u16 uTypeErr = 0;
                if (!is_allowed_as_runtime_type(pExplicitType, pTCContext, &uTypeErr)) {
                    // TODO: support for parametric types
                    return_error(pTypeNode, pTCStatement, pTCContext, uTypeErr,
                        "typecheck_macro_or_paramdecl_to() : unallowed type in explicit type-slot for a proc param");
                }
            } else {
                return_error(pParam, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                    "typecheck_macro_or_paramdecl_to() : non-vardecl expression in proc declaration should be a type");
            }
        }

        int iIdentifierHandle = get_id_from_decl_node(&varDecl, pTCStatement);
        return add_proclike_param_to(ioVecParams, iIdentifierHandle, pExplicitType, uOptDefaultValueNodeIndex, pParam, pTCStatement, pTCContext);

    } else {
        ETCResult checkExpr = typecheck_any_non_invoc_expression(pParam, uNodeKind, pTCStatement,
            pTCContext, EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
        if (checkExpr != ETCResult::ETCR_SUCCESS)
            return checkExpr;
        goto on_typeckecked_as_constant_expression;
    }

    on_typeckecked_as_constant_expression:
    Assert_(is_node_already_typechecked(pParam->pTCNode));
    Assert_(is_value_tc_const(pParam->pIntrinsicValue));

    if (pParam->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
        Assert_(is_value_tc_only(pParam->pIntrinsicValue));
        const TypeInfo* pExplicitType = type_from_type_node(pParam->pIntrinsicValue);
        Assert_(get_type_kind(pExplicitType) < COUNT_TYPE_KINDS);
        check_type_footprint_availability_may_return_wait_or_error(pExplicitType, pParam, pTCStatement, pTCContext,
            "Cannot typecheck a proc signature with");
        u16 uTypeErr = 0;
        if (!is_allowed_as_runtime_type(pExplicitType, pTCContext, &uTypeErr)) {
            // TODO: support for parametric types
            return_error(pParam, pTCStatement, pTCContext, uTypeErr,
                "typecheck_macro_or_paramdecl_to() : unallowed type for proc param");
        }
        return add_proclike_param_to(ioVecParams, ERES_INVALID_ID, pExplicitType, INVALID_NODE_INDEX, pParam, pTCStatement, pTCContext);
    } else {
        return_error(pParam, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
            "typecheck_macro_or_paramdecl_to() : non-vardecl expression in proc declaration should be a type");
    }

}

local_func ETCResult typecheck_all_param_decls_to(TmpArray<ProcLikeParam>* ioVecParams, u32 uStartingNodeIndex,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    u32 uCurrentNodeIndex = uStartingNodeIndex;

when_macro_start_again:
    {
        bool bWasMacroExpansion = false;
        TCNode* pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
        while (u8(pCurrentNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPRLIST_NODE) {
            u32 uNextNodeIndex = pCurrentNode->ast.uSecondaryChildNodeIndex;

            u32 uChildNodeIndex = pCurrentNode->ast.uPrimaryChildNodeIndex;

            Assert_(uChildNodeIndex != INVALID_NODE_INDEX);
            TmpTCNode child = init_tmp_tc_node(uChildNodeIndex, pTCStatement, pTCContext);
            Assert_(u8(child.pTCNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);

            ETCResult checkChild = typecheck_macro_or_paramdecl_to(ioVecParams, &child, pTCStatement, pTCContext,
                uCurrentNodeIndex, &bWasMacroExpansion);
            if (checkChild == ETCResult::ETCR_SUCCESS) {
                if (bWasMacroExpansion) {
                    goto when_macro_start_again;
                }
                // otherwise NOOP
            } else
                return checkChild;

            if (uNextNodeIndex != INVALID_NODE_INDEX) {
                uCurrentNodeIndex = uNextNodeIndex;
                pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
            }
        }

        Assert_(pCurrentNode);
        Assert_(u8(pCurrentNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);
        TmpTCNode last = init_tmp_tc_node(uCurrentNodeIndex, pTCStatement, pTCContext);

        ETCResult checkLast = typecheck_macro_or_paramdecl_to(ioVecParams, &last, pTCStatement, pTCContext,
            uCurrentNodeIndex, &bWasMacroExpansion);
        if (checkLast == ETCResult::ETCR_SUCCESS && bWasMacroExpansion) {
            goto when_macro_start_again;
        }
        return checkLast;
    }
}

local_func ETCResult do_typecheck_as_proc_signature(TmpTCNode* pProcDecl, TCStatement* pTCStatement, TCContext* pTCContext,
    TypeInfo_ProcLike* outProcSign)
{
    u8 uProcKindToken = u8(pProcDecl->pTCNode->ast.uNodeKindAndFlags >> 8);
    Assert_(uProcKindToken >= ETOK_PROC);
    u8 uProcKind = uProcKindToken - ETOK_PROC;
    Assert_(uProcKind < 16u);

    // TODO: setup a special binding for proc params and allow referencing among them as we go
    // TODO: take care of resolutions to local vars if typechecking a proc signature locally to another function...

    init_proc_like(outProcSign, uProcKind, pTCContext->pWorker->tmpArena);
    TmpTCNode signBase = init_tmp_tc_node(pProcDecl->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);

    if (pProcDecl->pTCNode->ast.uSecondaryChildNodeIndex != INVALID_NODE_INDEX) {
        TmpTCNode whereClause = init_tmp_tc_node(pProcDecl->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
        // TODO
        return_error(pProcDecl, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_proc_signature_ast() : where clause support not yet implemented");
    }

    Assert_(u8(signBase.pTCNode->ast.uNodeKindAndFlags) == ENODE_PROCPARAMS_WRAP_ALL);
    TmpTCNode inParamsWrapper = init_tmp_tc_node(signBase.pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    Assert_(u8(inParamsWrapper.pTCNode->ast.uNodeKindAndFlags) == ENODE_PROCPARAMS_IN); // otherwise postparser should have complained

    u32 uOptInParams = inParamsWrapper.pTCNode->ast.uPrimaryChildNodeIndex;
    if (uOptInParams != INVALID_NODE_INDEX) { // optional: none if no params
        ETCResult checkDecls = typecheck_all_param_decls_to(&(outProcSign->params), uOptInParams, pTCStatement, pTCContext);
        success_or_return_wait_or_error(checkDecls, pProcDecl->pTCNode);
    }
    Assert_(outProcSign->params.size() < 256u);
    u8 uCountInParams = u8(outProcSign->params.size());
    set_proc_like_input_param_count(outProcSign, uCountInParams);

    // TODO: check default counts and only trailing

    if (signBase.pTCNode->ast.uSecondaryChildNodeIndex != INVALID_NODE_INDEX) { // out params node is optional
        TmpTCNode outParamsWrapper = init_tmp_tc_node(signBase.pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
        Assert_(u8(outParamsWrapper.pTCNode->ast.uNodeKindAndFlags) == ENODE_PROCPARAMS_OUT); // otherwise postparser should have complained
        u32 uOutParams = outParamsWrapper.pTCNode->ast.uPrimaryChildNodeIndex; // but once there, required non-empty
        ETCResult checkDecls = typecheck_all_param_decls_to(&(outProcSign->params), uOutParams, pTCStatement, pTCContext);
        success_or_return_wait_or_error(checkDecls, pProcDecl->pTCNode);
    }

    return set_node_typecheck_expr_success(pProcDecl->pTCNode);
}


local_func ETCResult typecheck_proc_declaration(TmpTCNode* pExpr, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking a proclike definition"), pTCContext->pWorker);

    if_expr_already_typechecked_phase1_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    Assert_(eExpectation == EExpectedExpr::EXPECT_CONSTANT);
    TypeInfo_ProcLike resultingProcSign;
    ArenaRefPoint beforeTCSign = get_arena_ref_point(pTCContext->pWorker->tmpArena);
    ETCResult checkSign = do_typecheck_as_proc_signature(pExpr, pTCStatement, pTCContext, &resultingProcSign);

    if (checkSign == ETCResult::ETCR_SUCCESS) {

        if (!is_node_already_type_casted(pExpr->pTCNode)) { // marker for already recorded definition
            // TODO: check if overload. If not:
            // record a new proc body
            Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
            TypeInfo_ProcLike* pProcSign = (TypeInfo_ProcLike*)alloc_from(localArena,
                sizeof(TypeInfo_ProcLike), alignof(TypeInfo_ProcLike));

            init_proc_like(pProcSign, get_proc_kind(&resultingProcSign), localArena);
            pProcSign->params.append_all(resultingProcSign.params);
            set_proc_like_input_param_count(pProcSign, get_input_param_count(&resultingProcSign));
            pProcSign->asNode.info.uIRandMetaFlags = declare_user_proc_sign_format(pProcSign, pTCContext);
            pProcSign->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
            pProcSign->asNode.info.metaValue.knownValue.pType = pProcSign;

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Registering ProcLike with %u in-params and %u out-params",
                u64(get_input_param_count(pProcSign)), u64(get_output_param_count(pProcSign))), pTCContext->pWorker);

            TCProcBodyRegistration* pRegistration = (TCProcBodyRegistration*)alloc_from(localArena,
                sizeof(TCProcBodyRegistration), alignof(TCProcBodyRegistration));
            pRegistration->procSource.procSign = pProcSign;
            // Note: pChildBlock at this point should be a 'tagged' ptr with block index in the source file's ast.
            pRegistration->procSource.pRootTcBlock = reinterpret_cast<TCSeqSourceBlock*>(pTCStatement->pChildBlock);
            FireAndForgetArenaAlloc localAlloc(localArena);
            pRegistration->procSource.vecStaticInParams.init(localAlloc);
            // CLEANUP: check if this 'deep-copy' of the statement is really needed here ???
            pRegistration->procSource.pStatementWithSignature = (TCStatement*)alloc_from(localArena,
                sizeof(TCStatement), alignof(TCStatement));
            *(pRegistration->procSource.pStatementWithSignature) = *pTCStatement;
            pRegistration->procSource.pStatementWithSignature->vecNodes.init(localAlloc);
            pRegistration->procSource.pStatementWithSignature->vecNodes.append_all(pTCStatement->vecNodes);
            pRegistration->procSource.pStatementWithSignature->vecNodeValues.init(localAlloc);
            pRegistration->procSource.pStatementWithSignature->vecNodeValues.append_all(pTCStatement->vecNodeValues);

            pRegistration->procResult.iPrimaryIdentifier = ERES_INVALID_ID; // will be assigned later at binding stage
            pRegistration->procResult.procSign = pProcSign;
            pRegistration->procResult.uProcBodyTypechekingStatus = EPROCSTATUS_NOT_NET_STARTED;
            pRegistration->procResult.uRegistrationIndex = pTCContext->pIsolatedSourceFile->vecAllProcBodies.size();
            pTCContext->pIsolatedSourceFile->vecAllProcBodies.append(pRegistration);
            pRegistration->procResult.iSourceFileIndex = pTCContext->pIsolatedSourceFile->iRegistrationIndex;
            pRegistration->procResult.vecScopedEntities.init(localAlloc);
            pRegistration->procResult.vecBindings.init(localAlloc);
            pRegistration->procResult.vecErrChecks.init(localAlloc);
            init_ir_repo(&(pRegistration->procResult.procwiseRepo), IR_REPO_ID_CURRENT_PROC, localArena);
            pRegistration->procResult.uIsForeignSource = 0uLL;
            pRegistration->procResult.foreignSymbolName = FFString { 0 };
            pRegistration->procResult.pGraphResult = 0;
            pRegistration->procResult.vecLocalTasksWaitingForCompletion.init(localArena);
            pRegistration->procResult.vecNonLocalTasksWaitingForCompletion.init(localArena);

            NodeValue* pProcDefValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pProcDefValue->pType = pProcSign;
            pProcDefValue->info.uIRandMetaFlags = IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA;
            u64 uIRofProcBodyRef = ir_make_procbody_ref_in_file(u32(pTCContext->pIsolatedSourceFile->iRegistrationIndex), pRegistration->procResult.uRegistrationIndex);
            pProcDefValue->info.uIRandMetaFlags |= uIRofProcBodyRef;
            pProcDefValue->info.metaValue.knownValue.uEmbeddedValue = ir_make_direct_nyka_value(uIRofProcBodyRef);

            pRegistration->procResult.uIRofProcDecl = uIRofProcBodyRef;
            set_node_type_cast_expr_success(pExpr->pTCNode); // marker for already recorded definition

            if (pTCStatement->pChildBlock) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Queuing TC task for proclike body after proc declaration registration"), pTCContext->pWorker);

                TCContext* pTypecheckBodyCtx = (TCContext*)alloc_from(localArena, 
                    sizeof(TCContext), alignof(TCContext));
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Allocated new TC Task 0x%llx for typechecking proc body", reinterpret_cast<u64>(pTypecheckBodyCtx)),
                    pTCContext->pWorker);

                *((CompilationContext*)pTypecheckBodyCtx) = *((CompilationContext*)pTCContext);
                pTypecheckBodyCtx->eKind = ETypecheckContextKind::ECTXKIND_PROCBODY;
                pTypecheckBodyCtx->eBlockKind = ETypecheckBlockKind::EBLOCKKIND_SEQ;
                pTypecheckBodyCtx->eGlobalDeclScope = EScopeKind::SCOPEKIND_GLOBAL_PRIVATE; // ?
                pTypecheckBodyCtx->uNestedExpansionLevel = 0;
                pTypecheckBodyCtx->uFlags = CTXFLAG_HALT_ON_NON_SUCCESS | CTXFLAG_ALLOW_RUNTIME |
                                            CTXFLAG_DECLARATIONS_ARE_LOCAL | CTXFLAG_RESOLUTIONS_START_LOCAL;
                pTypecheckBodyCtx->pCurrentBlock = 0;
                pTypecheckBodyCtx->pNamespace = pTCContext->pNamespace;
                pTypecheckBodyCtx->pRepo = &(pRegistration->procResult.procwiseRepo);
                pTypecheckBodyCtx->pTmpRepo = 0;
                pTypecheckBodyCtx->pProcResult = &(pRegistration->procResult);
                //pTypecheckBodyCtx->pVecLocalBindings = &(pRegistration->procResult.vecBindings);
                pTypecheckBodyCtx->pProcSource = &(pRegistration->procSource);
                pTypecheckBodyCtx->vecTypecheckedBlocks.init(localArena);
                pTypecheckBodyCtx->pParentContext = 0;
                pTypecheckBodyCtx->pVecOfGotoPlaceholdersToReturnPoint = 0;

                pTypecheckBodyCtx->setOfNewlyDeclaredIdentifiers = {};

                pTypecheckBodyCtx->uSizeOfVecUsingAccessibleEnumBefore = 0u;
                pTypecheckBodyCtx->uSizeOfVecUsingAccessibleNamespaceBefore = 0u;
                pTypecheckBodyCtx->uSizeOfVecUsingAllEnumBefore = 0u;
                pTypecheckBodyCtx->uSizeOfVecUsingAllNamespaceBefore = 0u;
                pTypecheckBodyCtx->uSizeOfVecChildrenNamespacesBefore = 0u;
                pTypecheckBodyCtx->uSizeOfVecIncludedStructLikeBefore = 0u;

                pTypecheckBodyCtx->uGlobalStatementOnHold = 0;
                pTypecheckBodyCtx->mapLocalNodeInfoIfResumingCurrentStatement = {};
                pTypecheckBodyCtx->waitingReason._packed = u64(EWR_NONE);

                pTypecheckBodyCtx->eTaskPrio = ETaskPriority::ETASKPRIO_LOW;
                acquire_source_file_specific_task_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
                pTCContext->pIsolatedSourceFile->tvecTCTasksToLaunchByPrio[ETaskPriority::ETASKPRIO_LOW].append(pTypecheckBodyCtx);
                release_source_file_specific_task_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
            } else {
                platform_log_error("*** registered proc with missing body", true);
                pRegistration->procResult.uProcBodyTypechekingStatus = EPROCSTATUS_IN_ERROR_TC;
            }

        }
    }
    reset_arena_to(beforeTCSign, pTCContext->pWorker->tmpArena);
    return checkSign;
}

local_func ETCResult typecheck_compound_declaration(TmpTCNode* pExpr, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking a compound type definition"), pTCContext->pWorker);
    
    if_expr_already_typechecked_phase1_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    Assert_(eExpectation == EExpectedExpr::EXPECT_CONSTANT);
    u8 uTypeKind = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);

    switch (uTypeKind) {
        case ETOK_STRUCT:
        case ETOK_PACKED_STRUCT:
        case ETOK_UNION:
        {
            if (pExpr->pTCNode->ast.uPrimaryChildNodeIndex == INVALID_NODE_INDEX) {
                // No poly-params
                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                TypeInfo_StructLike* pNewStructType = (TypeInfo_StructLike*)alloc_from(localArena,
                    sizeof(TypeInfo_StructLike), alignof(TypeInfo_StructLike));
                u8 uCompoundType = 0;
                if (uTypeKind == ETOK_STRUCT)
                    uCompoundType = COMPOUNDTYPE_IS_STRUCT; // TODO: comptime-only flags ; structview ??
                else if (uTypeKind == ETOK_UNION)
                    uCompoundType = COMPOUNDTYPE_IS_PACKED;
                else if (uTypeKind == ETOK_VIEW) {
                    uCompoundType = COMPOUNDTYPE_IS_VIEW;
                    // TODO:
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_compound_declaration : struct view not yet implemented");
                } else { Assert_(uTypeKind == ETOK_PACKED_STRUCT);
                    uCompoundType = COMPOUNDTYPE_IS_PACKED;
                }
                init_structlike_type_before_tc(pNewStructType, uCompoundType, 0, localArena);

                TCCompoundRegistration* pRegistration = (TCCompoundRegistration*)alloc_from(localArena,
                    sizeof(TCCompoundRegistration), alignof(TCCompoundRegistration));
                pRegistration->pCompoundType = pNewStructType;
                // Note: pChildBlock at this point should be a 'tagged' ptr with block index in the source file's ast.
                pRegistration->pRootTcBlock = pTCStatement->pChildBlock;
                FireAndForgetArenaAlloc localAlloc(localArena);
                // CLEANUP: check if this 'deep-copy' of the statement is really needed here ???
                pRegistration->pStatementWithSignature = (TCStatement*)alloc_from(localArena,
                    sizeof(TCStatement), alignof(TCStatement));
                *(pRegistration->pStatementWithSignature) = *pTCStatement;
                pRegistration->pStatementWithSignature->vecNodes.init(localAlloc);
                pRegistration->pStatementWithSignature->vecNodes.append_all(pTCStatement->vecNodes);
                pRegistration->pStatementWithSignature->vecNodeValues.init(localAlloc);
                pRegistration->pStatementWithSignature->vecNodeValues.append_all(pTCStatement->vecNodeValues);
                pRegistration->iPrimaryIdentifier = ERES_INVALID_ID; // will be assigned later at binding stage
                pRegistration->uTCProgress = ECOMPOUND_NOT_YET_STARTED;
                pRegistration->vecLocalTasksWaitingForCompletion.init(localArena);
                pRegistration->vecNonLocalTasksWaitingForCompletion.init(localArena);
                pRegistration->uCountEnsuredConstTasks = 0u;
                pRegistration->uCountPossiblyRuntimeTasks = 0u;
                pRegistration->pSourceFile = pTCContext->pIsolatedSourceFile;

                u32 uRegistrationIndex = pTCContext->pIsolatedSourceFile->vecAllCompoundDef.size();
                if (uRegistrationIndex > 0x0000'FFFFu) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_MANY_COMPOUNDS_IN_SAME_FILE,
                        "typecheck_expr_or_multi_invoc() : max 64K compound-types definitions originating from a single file");
                }
                pTCContext->pIsolatedSourceFile->vecAllCompoundDef.append(pRegistration);
                pNewStructType->uRegistrationIndex = u16(uRegistrationIndex);
                pNewStructType->pRegistration = pRegistration;

                NodeValue* pStructDefValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC,
                    pTCStatement, pTCContext);
                pStructDefValue->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
                pStructDefValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_IS_KNOWN;
                pStructDefValue->info.metaValue.knownValue.pType = pNewStructType;
                set_node_type_cast_expr_success(pExpr->pTCNode); // marker for already recorded definition

                if (pTCStatement->pChildBlock) {

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Queuing TC task for structlike body after structlike declaration registration"), pTCContext->pWorker);

                    TCContext* pTypecheckStructBodyCtx = (TCContext*)alloc_from(localArena, 
                        sizeof(TCContext), alignof(TCContext));
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Allocated new TC Task 0x%llx for typechecking struct body", reinterpret_cast<u64>(pTypecheckStructBodyCtx)),
                        pTCContext->pWorker);
                    
                    *((CompilationContext*)pTypecheckStructBodyCtx) = *((CompilationContext*)pTCContext);
                    pTypecheckStructBodyCtx->eKind = ETypecheckContextKind::ECTXKIND_COMPOUND;
                    pTypecheckStructBodyCtx->eBlockKind = ETypecheckBlockKind::EBLOCKKIND_BASE;
                    pTypecheckStructBodyCtx->eGlobalDeclScope = EScopeKind::SCOPEKIND_GLOBAL_PRIVATE; // ?
                    pTypecheckStructBodyCtx->uNestedExpansionLevel = 0;
                    pTypecheckStructBodyCtx->uFlags = CTXFLAG_DECLARATIONS_ARE_LOCAL | CTXFLAG_RESOLUTIONS_START_LOCAL;
                    pTypecheckStructBodyCtx->pCurrentBlock = 0;
                    pTypecheckStructBodyCtx->pNamespace = pTCContext->pNamespace;
                    pTypecheckStructBodyCtx->pRepo = &(pTCContext->pIsolatedSourceFile->filewiseConstRepo);
                    pTypecheckStructBodyCtx->pTmpRepo = 0;
                    pTypecheckStructBodyCtx->pProcResult = 0;
                    //pTypecheckStructBodyCtx->pVecLocalBindings = 0;
                    pTypecheckStructBodyCtx->pProcSource = 0;
                    pTypecheckStructBodyCtx->vecTypecheckedBlocks = {};
                    pTypecheckStructBodyCtx->pParentContext = 0;
                    pTypecheckStructBodyCtx->pVecOfGotoPlaceholdersToReturnPoint = 0;
                    pTypecheckStructBodyCtx->pCompoundToTC = pRegistration;

                    pTypecheckStructBodyCtx->setOfNewlyDeclaredIdentifiers = {}; // will be init later, at task start

                    pTypecheckStructBodyCtx->uSizeOfVecUsingAccessibleEnumBefore = 0u;
                    pTypecheckStructBodyCtx->uSizeOfVecUsingAccessibleNamespaceBefore = 0u;
                    pTypecheckStructBodyCtx->uSizeOfVecUsingAllEnumBefore = 0u;
                    pTypecheckStructBodyCtx->uSizeOfVecUsingAllNamespaceBefore = 0u;
                    pTypecheckStructBodyCtx->uSizeOfVecChildrenNamespacesBefore = 0u;
                    pTypecheckStructBodyCtx->uSizeOfVecIncludedStructLikeBefore = 0u;

                    pTypecheckStructBodyCtx->uGlobalStatementOnHold = 0;
                    pTypecheckStructBodyCtx->mapLocalNodeInfoIfResumingCurrentStatement = {};
                    pTypecheckStructBodyCtx->waitingReason._packed = u64(EWR_NONE);

                    pTypecheckStructBodyCtx->eTaskPrio = ETaskPriority::ETASKPRIO_MED;
                    if (!is_ctx_global(pTCContext) && !is_ctx_compound(pTCContext))
                        pTypecheckStructBodyCtx->eTaskPrio = ETaskPriority::ETASKPRIO_LOW;
                    acquire_source_file_specific_task_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
                    pTCContext->pIsolatedSourceFile->tvecTCTasksToLaunchByPrio[pTypecheckStructBodyCtx->eTaskPrio].append(pTypecheckStructBodyCtx);
                    release_source_file_specific_task_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
                    pRegistration->uCountPossiblyRuntimeTasks = 1u;
                } else {
                    platform_log_error("*** registered struct or union with missing body", true);
                    pNewStructType->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR|COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME;
                    pRegistration->uTCProgress = ECOMPOUND_DONE_ALL;
                }

                return ETCResult::ETCR_SUCCESS;

            } else {
                // Struct with poly params
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_expr_or_multi_invoc() : struct or union decl expression with polymorphic-params not yet implemented");
            }

        } break;

        case ETOK_ENUM:
        {

            const TypeInfo_Integral* pBaseType = (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_INT];
            if (pExpr->pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                TmpTCNode exprAfter = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking base type expression of enum"),
                    pTCContext->pWorker);
                ETCResult eCheckBaseType = typecheck_expression(&exprAfter, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
                success_or_return_wait_or_error(eCheckBaseType, pExpr->pTCNode);
                Assert_(is_node_already_typechecked(exprAfter.pTCNode));
                Assert_(exprAfter.pIntrinsicValue);
                if (exprAfter.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                    Assert_(is_value_tc_only(exprAfter.pIntrinsicValue));
                    const TypeInfo* pTypeValue = type_from_type_node(exprAfter.pIntrinsicValue);
                    if (get_type_kind(pTypeValue) == ETypeKind::ETYPEKIND_INTEGRAL) {
                        pBaseType = (const TypeInfo_Integral*)pTypeValue;
                    } else {
                        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                            "typecheck_expr_or_multi_invoc() : base type of enum must be an integral type");
                    }
                } else {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                        "typecheck_expr_or_multi_invoc() : enum keyword must be last on statement, or followed by an integral base type");
                }
            }

            Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
            TypeInfo_Enum* pNewEnumType = (TypeInfo_Enum*)alloc_from(localArena, sizeof(TypeInfo_Enum), alignof(TypeInfo_Enum));
            init_enum_type_before_tc(pNewEnumType, pBaseType, 0, 0, localArena);

            TCCompoundRegistration* pRegistration = (TCCompoundRegistration*)alloc_from(localArena,
                sizeof(TCCompoundRegistration), alignof(TCCompoundRegistration));
            pRegistration->pCompoundType = pNewEnumType;
            // Note: pChildBlock at this point should be a 'tagged' ptr with block index in the source file's ast.
            pRegistration->pRootTcBlock = pTCStatement->pChildBlock;
            FireAndForgetArenaAlloc localAlloc(localArena);
            // CLEANUP: check if this 'deep-copy' of the statement is really needed here ???
            pRegistration->pStatementWithSignature = (TCStatement*)alloc_from(localArena,
                sizeof(TCStatement), alignof(TCStatement));
            *(pRegistration->pStatementWithSignature) = *pTCStatement;
            pRegistration->pStatementWithSignature->vecNodes.init(localAlloc);
            pRegistration->pStatementWithSignature->vecNodes.append_all(pTCStatement->vecNodes);
            pRegistration->pStatementWithSignature->vecNodeValues.init(localAlloc);
            pRegistration->pStatementWithSignature->vecNodeValues.append_all(pTCStatement->vecNodeValues);
            pRegistration->iPrimaryIdentifier = ERES_INVALID_ID; // will be assigned later at binding stage
            pRegistration->uTCProgress = ECOMPOUND_NOT_YET_STARTED;
            pRegistration->vecLocalTasksWaitingForCompletion.init(localArena);
            pRegistration->vecNonLocalTasksWaitingForCompletion.init(localArena);
            pRegistration->uCountEnsuredConstTasks = 0u;
            pRegistration->uCountPossiblyRuntimeTasks = 0u;
            pRegistration->pSourceFile = pTCContext->pIsolatedSourceFile;

            u32 uRegistrationIndex = pTCContext->pIsolatedSourceFile->vecAllCompoundDef.size();
            if (uRegistrationIndex > 0x0000'FFFFu) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_MANY_COMPOUNDS_IN_SAME_FILE,
                    "typecheck_expr_or_multi_invoc() : max 64K compound-types definitions originating from a single file");
            }
            pTCContext->pIsolatedSourceFile->vecAllCompoundDef.append(pRegistration);
            pNewEnumType->uRegistrationIndex = u16(uRegistrationIndex);
            pNewEnumType->pRegistration = pRegistration;

            NodeValue* pEnumDefValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC,
                pTCStatement, pTCContext);
            pEnumDefValue->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
            pEnumDefValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_IS_KNOWN;
            pEnumDefValue->info.metaValue.knownValue.pType = pNewEnumType;
            set_node_type_cast_expr_success(pExpr->pTCNode); // marker for already recorded definition

            if (pTCStatement->pChildBlock) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Queuing TC task for enum body after enum declaration registration"), pTCContext->pWorker);

                TCContext* pTypecheckEnumBodyCtx = (TCContext*)alloc_from(localArena, 
                    sizeof(TCContext), alignof(TCContext));
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Allocated new TC Task 0x%llx for typechecking enum body", reinterpret_cast<u64>(pTypecheckEnumBodyCtx)),
                    pTCContext->pWorker);

                *((CompilationContext*)pTypecheckEnumBodyCtx) = *((CompilationContext*)pTCContext);
                pTypecheckEnumBodyCtx->eKind = ETypecheckContextKind::ECTXKIND_COMPOUND;
                pTypecheckEnumBodyCtx->eBlockKind = ETypecheckBlockKind::EBLOCKKIND_BASE;
                pTypecheckEnumBodyCtx->eGlobalDeclScope = EScopeKind::SCOPEKIND_GLOBAL_PRIVATE; // ?
                pTypecheckEnumBodyCtx->uNestedExpansionLevel = 0;
                pTypecheckEnumBodyCtx->uFlags = CTXFLAG_HALT_ON_NON_SUCCESS | // enum TCs are sequential, contrary to struct/unions
                                                CTXFLAG_DECLARATIONS_ARE_LOCAL | CTXFLAG_RESOLUTIONS_START_LOCAL;
                pTypecheckEnumBodyCtx->pCurrentBlock = 0;
                pTypecheckEnumBodyCtx->pNamespace = pTCContext->pNamespace;
                pTypecheckEnumBodyCtx->pRepo = &(pTCContext->pIsolatedSourceFile->filewiseConstRepo);
                pTypecheckEnumBodyCtx->pTmpRepo = 0;
                pTypecheckEnumBodyCtx->pProcResult = 0;
                //pTypecheckEnumBodyCtx->pVecLocalBindings = 0;
                pTypecheckEnumBodyCtx->pProcSource = 0;
                pTypecheckEnumBodyCtx->vecTypecheckedBlocks = {};
                pTypecheckEnumBodyCtx->pParentContext = 0;
                pTypecheckEnumBodyCtx->pVecOfGotoPlaceholdersToReturnPoint = 0;
                pTypecheckEnumBodyCtx->pCompoundToTC = pRegistration;

                pTypecheckEnumBodyCtx->setOfNewlyDeclaredIdentifiers = {};

                pTypecheckEnumBodyCtx->uSizeOfVecUsingAccessibleEnumBefore = 0u;
                pTypecheckEnumBodyCtx->uSizeOfVecUsingAccessibleNamespaceBefore = 0u;
                pTypecheckEnumBodyCtx->uSizeOfVecUsingAllEnumBefore = 0u;
                pTypecheckEnumBodyCtx->uSizeOfVecUsingAllNamespaceBefore = 0u;
                pTypecheckEnumBodyCtx->uSizeOfVecChildrenNamespacesBefore = 0u;
                pTypecheckEnumBodyCtx->uSizeOfVecIncludedStructLikeBefore = 0u;

                pTypecheckEnumBodyCtx->uGlobalStatementOnHold = 0;
                pTypecheckEnumBodyCtx->mapLocalNodeInfoIfResumingCurrentStatement = {};

                pTypecheckEnumBodyCtx->eTaskPrio = ETaskPriority::ETASKPRIO_MED;
                if (!is_ctx_global(pTCContext) && !is_ctx_compound(pTCContext))
                    pTypecheckEnumBodyCtx->eTaskPrio = ETaskPriority::ETASKPRIO_LOW;
                acquire_source_file_specific_task_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
                pTCContext->pIsolatedSourceFile->tvecTCTasksToLaunchByPrio[pTypecheckEnumBodyCtx->eTaskPrio].append(pTypecheckEnumBodyCtx);
                release_source_file_specific_task_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
                pRegistration->uCountEnsuredConstTasks = 1u;
            } else {
                platform_log_error("*** registered enum with missing body", true);
                pNewEnumType->_coreFlags |= COMPOUNDFLAG_BODY_IN_ERROR;
                pRegistration->uTCProgress = ECOMPOUND_DONE_ALL;
            }

            return ETCResult::ETCR_SUCCESS;

        } break;

        default:
            return_error(pExpr, pTCStatement, pTCContext, FERR_UNREACHABLE,
                "typecheck_compound_declaration() : unknown complex-type kind");
    }
}

#endif // LOCLIB_TC_ADVANCED_DECLARATIONS_H_
  