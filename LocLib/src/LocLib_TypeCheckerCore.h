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

#ifndef LOCLIB_TYPE_CHECKER_CORE_H_
#define LOCLIB_TYPE_CHECKER_CORE_H_

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
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_IR_Types.h"
#include "LocLib_IR.h"
//#include "LocLib_TypeCasts.h"

// TODO : remove dependency to crt
#include <cstdio>
#include <cstdlib>

#define check_type_availability_may_return_wait_or_error(pType, pExpr, pTCStatement, pTCContext, msgStartIfErr) do { \
    if (get_type_kind(pType) == ETYPEKIND_STRUCTLIKE) { \
        const TypeInfo_StructLike* pAsStructLike = (const TypeInfo_StructLike*)pType; \
        if (!is_structlike_type_footprint_available(pAsStructLike)) { \
            return add_waiting_task_for_compound_body(pAsStructLike, pExpr->uNodeIndexInStatement, pTCContext); \
        } else if (pAsStructLike->_coreFlags & COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME) { \
            return_error(pExpr, pTCStatement, pTCContext, CERR_COMPOUND_TYPE_IN_ERROR, \
                msgStartIfErr " a struct-like base-type which happens to be in error"); \
        } \
    } \
} while (0)

// emits error **on 'pNode' token** (not necessarily corresponding to pTcNode, but should be in same statement) AND flags pTcNode as in error
local_func void emit_error(TmpTCNode* pNode, TCStatement* pTcStatement, TCContext* pEvalContext, u16 uErrCode,
    const char* formatMsg, DisplayableCompilerEntity param1 = DCE_NONE,
    DisplayableCompilerEntity param2 = DCE_NONE, DisplayableCompilerEntity param3 = DCE_NONE)
{
    // TODO: CLEANUP: temporary forced log
    platform_log_info("*** Tmp TC-Error Report : ", false);
    platform_log_info(formatMsg, true);

    // TODO: Handle some error messages instead of this ?
    //      quick placeholder for some messaging system:
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("### Emitting Compilation Error : %s",
        reinterpret_cast<u64>(formatMsg)), pEvalContext->pWorker);

    LocLib_Error tmpTypeCheckError;
	tmpTypeCheckError.errCode = uErrCode;
    tmpTypeCheckError.uBlockOrLineIfScanErr = pEvalContext->pCurrentBlock->uAstBlockIndex;
    tmpTypeCheckError.uStatement = pTcStatement->uStatementIndexInBlock;
    tmpTypeCheckError.uTokenRef = pNode->pTCNode->ast.uPivotalTokenPackedRef;
    pEvalContext->pIsolatedSourceFile->vecErrors.append(tmpTypeCheckError);
    set_node_tc_error(pNode->pTCNode, uErrCode);
}

#define return_error(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg) do { \
    emit_error(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg); \
    return ETCResult::ETCR_ERROR; \
} while(0)

#define return_error1(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg, param1) do { \
    emit_error(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg, param1); \
    return ETCResult::ETCR_ERROR; \
} while(0)

#define return_error2(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg, param1, param2) do { \
    emit_error(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg, param1, param2); \
    return ETCResult::ETCR_ERROR; \
} while(0)

#define return_error3(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg, param1, param2, param3) do { \
    emit_error(pNode, pTcStatement, pEvalContext, uErrCode, formatMsg, param1, param2, param3); \
    return ETCResult::ETCR_ERROR; \
} while(0)

// TODO: what about uErrCode ???
#define otherwise_return_wait_or_error(checkResult, pTcNode) else do { \
        if (NOMINAL(checkResult == ETCResult::ETCR_WAITING)) { \
        } else { \
            set_node_tc_error(pTcNode, 0); \
        } \
        return checkResult; \
    } while (0)

#define success_or_return_wait_or_error(checkResult, pTcNode) do { \
    if (NOMINAL(checkResult == ETCResult::ETCR_SUCCESS)) { \
    } otherwise_return_wait_or_error(checkResult, pTcNode); \
} while (0)

#define success_or_return(checkResult) do { \
    if (NOMINAL(checkResult == ETCResult::ETCR_SUCCESS)) { \
    } else \
        return checkResult; \
} while(0)

local_func_inl bool is_comptime_prefixed(AstNode* pNode) {
    return 0 != (pNode->uNodeKindAndFlags & ENODEKINDFLAG_IS_COMPTIME);
}

local_func ETCResult add_waiting_task_to(SourceFileDescAndState* pSourceFile, TCWaitingReason waitingReason,
    u32 uNodeIndexWithWait, TCContext* pTCContext, bool bIfCompoundConstOnly = false)
{
    pTCContext->uNodeIndexWithWait = uNodeIndexWithWait;

    acquire_source_file_specific_task_lock(pSourceFile, pTCContext->pWorker);

    auto itThisReason = pSourceFile->mapWaitingTasksByReason.find(waitingReason);
    if (itThisReason == pSourceFile->mapWaitingTasksByReason.end()) {
        TmpArray<TCContext*> newArray;
        newArray.init(FireAndForgetArenaAlloc(pSourceFile->localArena));
        itThisReason = pSourceFile->mapWaitingTasksByReason.insert(waitingReason, newArray);
    }
    TmpArray<TCContext*>& vecWaitingTasks = itThisReason.value();
    vecWaitingTasks.append(pTCContext);
    if (pTCContext->eKind <= ETypecheckContextKind::ECTXKIND_GLOBAL_PRIVATE) {
        Assert_(pTCContext->pNamespace);
        pTCContext->pNamespace->uCountGlobalTasksInWaitingTasks += 1u;
    }

    if (is_ctx_compound(pTCContext)) {
        Assert_(pTCContext->pCompoundToTC);
        if (!bIfCompoundConstOnly)
            pTCContext->pCompoundToTC->setWaitingPossiblyRuntime.insert(pTCContext);
        else
            pTCContext->pCompoundToTC->setWaitingConstOnly.insert(pTCContext);
    }

    release_source_file_specific_task_lock(pSourceFile, pTCContext->pWorker);

    return ETCResult::ETCR_WAITING;
}

local_func_inl TmpTCNode init_tmp_tc_node(u32 uNodeIndex, TCStatement* pTCStatement, TCContext* pEvalContext) {
    Assert_(uNodeIndex != INVALID_NODE_INDEX);
    TmpTCNode result = {};
    result.pTCNode = pTCStatement->vecNodes[uNodeIndex];
    result.uNodeIndexInStatement = uNodeIndex;
    return result;
}

enum EExpectedExpr {
    EXPECT_CONSTANT = 0,
    EXPECT_REGULAR,
    EXPECT_ASSIGNABLE,
    EXPECT_DECLARABLE,
};

constexpr const char* tExpectedExprStr[4] = {
    "CONSTANT",
    "REGULAR",
    "ASSIGNABLE",
    "DECLARABLE"
};

enum EInvocFormResultCount {
    EINVOC_NO_RETURN = 0,
    EINVOC_RETURNS_ONE = 1,
    EINVOC_RETURNS_ONE_OR_MANY = 2,
};

constexpr const char* tInvocFormResultCountStr[3] = {
    "NO-RETURN",
    "RETURNS-1",
    "RETURNS-1orMANY",
};

// predecl of our bread-and-butter typecheck-expression function, widely used throughout typechecker
ETCResult typecheck_expression(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow, bool bAllowUserTypeOrProcDecl = false);

local_func void get_common_type_flags(const TypeInfo* pType,
    bool* outIsNumeric, bool* outIsIntegral, bool* outIsVecOfNumeric, bool* outIsVecOfIntegral,
    bool* outIsPointer, bool* outIsBool, bool* outIsCodePoint, bool* outIsString)
{
    ETypeKind eKind = get_type_kind(pType);
    if (eKind == ETypeKind::ETYPEKIND_INTEGRAL) {
        *outIsIntegral = true;
        *outIsNumeric = !is_raw_integral_(pType);
        *outIsVecOfNumeric = false;
        *outIsVecOfIntegral = false;
        *outIsPointer = (pType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]);
        *outIsBool = (pType == g_pCoreTypesInfo[ECORETYPE_BOOL]);
        *outIsCodePoint = (pType == g_pCoreTypesInfo[ECORETYPE_CODEPOINT]);
        *outIsString = false; 
    } else if (eKind == ETypeKind::ETYPEKIND_FLOATINGPOINT) {
        *outIsNumeric = true;
        *outIsIntegral = false;
        *outIsVecOfNumeric = false;
        *outIsVecOfIntegral = false;
        *outIsPointer = false; *outIsBool = false; *outIsCodePoint = false; *outIsString = false; 
    } else {
        *outIsIntegral = false; *outIsNumeric = false; *outIsBool = false; *outIsCodePoint = false;
        if (eKind == ETypeKind::ETYPEKIND_HWVECTOR) {
            const TypeInfo* pElemType = get_element_type((const TypeInfo_HwVector*)pType);
            if (get_type_kind(pElemType) == ETypeKind::ETYPEKIND_INTEGRAL) {
                *outIsVecOfIntegral = true;
                *outIsVecOfNumeric = !is_raw_integral_(pElemType);
            } else {
                Assert_(get_type_kind(pElemType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
                *outIsVecOfIntegral = false;
                *outIsVecOfNumeric = true;
            }
        } else {
            *outIsVecOfIntegral = false; *outIsVecOfNumeric = false;
            *outIsPointer = (eKind == ETypeKind::ETYPEKIND_POINTER);
            *outIsString = (eKind == ETypeKind::ETYPEKIND_OTHERCORE && 0 != (((const TypeInfo_OtherCore*)pType)->_coreFlags & OTHERCOREFLAG_IS_STRING));
        }
    }
}

local_func_inl void get_common_type_flags(const TypeInfo* pType,
    bool* outIsNumeric, bool* outIsIntegral, bool* outIsVecOfNumeric, bool* outIsVecOfIntegral)
{
    bool unused1, unused2, unused3, unused4;
    return get_common_type_flags(pType, outIsNumeric, outIsIntegral, outIsVecOfNumeric, outIsVecOfIntegral,
        &unused1, &unused2, &unused3, &unused4);
}

local_func void set_binding_source_ref(ValueBinding* pBinding, TCStatement* pTCStatement,
    TCContext* pTCContext, EDeclAttributes eAttr)
{
    if (!is_ctx_proc_expansion(pTCContext)) {
        pBinding->sourceRef.iSourceFile = pTCStatement->iSourceFileIndex;
        pBinding->sourceRef.uAstBlock = pTCStatement->uBlockIndexInSourceFile;
        pBinding->sourceRef.uAstStatementIndexInBlock = pTCStatement->uStatementIndexInBlock;
        pBinding->sourceRef.iIfExpandedExpansionSourceFile = -1;
    } else {
        pBinding->sourceRef.iIfExpandedExpansionSourceFile = pTCStatement->iSourceFileIndex;
        pBinding->sourceRef.uIfExpandedExpansionAstBlock = pTCStatement->uBlockIndexInSourceFile;
        pBinding->sourceRef.uIfExpandedExpansionAstStatementIndexInBlock = pTCStatement->uStatementIndexInBlock;
        Assert(false, "set_binding_source_ref() : source ref not yet implemented during proc expansions");
        /*
        TCStatement* pOrigStatement =
            pPrimaryContext->pCurrentBlock->vecStatements[pPrimaryContext->pCurrentBlock->uStatementBeingTypechecked];
        pBinding->sourceRef.iSourceFile = pOrigStatement->iSourceFileIndex;
        pBinding->sourceRef.uAstBlock = pOrigStatement->uBlockIndexInSourceFile;
        pBinding->sourceRef.uAstStatementIndexInBlock = pOrigStatement->uStatementIndexInBlock;
        */
    }
}

local_func u32 do_runtime_err_check(u64 uIRtoCheck, u8 uFormatOfIRtoCheck, u32 uNonZeroFlag,
                                    u32 uOptPosOfCheckedInstr, u8 uErrCheckKind, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(ir_is_valid_param(uIRtoCheck));
    u32 uActiveAndNonZeroFlags = uNonZeroFlag|IR_INSTRFLAG_ERRCHK_IS_ACTIVE; // TODO: compilation params-dependent (retrieve from current context)

    LocalErrCheck errCheck;
    errCheck.uPosOfCheck = pTCContext->pRepo->uSize; // next instruction will be the check proper
    errCheck.uPosOfInstr = uOptPosOfCheckedInstr;
    errCheck.uTokenRef = pNode->pTCNode->ast.uPivotalTokenPackedRef;
    errCheck.uStatement = pTCStatement->uStatementIndexInBlock;
    errCheck.uBlockIndex = pTCStatement->uBlockIndexInSourceFile;
    errCheck.iSourceFile = pTCContext->pCurrentBlock->iSourceFileIndex;
    errCheck.uFlagsAndKind = uActiveAndNonZeroFlags | u32(uErrCheckKind);
    u32 uErrRegistrationPos = pTCContext->pProcResult->vecErrChecks.size();
    pTCContext->pProcResult->vecErrChecks.append(errCheck);
    IREntry* pNewEntry = ir_append_new_entry(pTCContext->pRepo);
    pNewEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_ERRCHK) | (u64(uFormatOfIRtoCheck) << 16) |
                                                  u64(uActiveAndNonZeroFlags) | uIRtoCheck;
    pNewEntry->uInstrMetaFlagsAndSecondParam = (u64(uErrRegistrationPos) << IR_STD_PARAM_SHIFT);
    pNewEntry->metaValue._payload = 0uLL;
    return errCheck.uPosOfCheck;
}

local_func bool is_allowed_as_runtime_type(const TypeInfo* pType, TCContext* pTCContext, u16* outErrWhenFalse)
{
    ETypeKind eTypeKind;
    pType = unalias_ext(pType, &eTypeKind);

    if (is_core_type(eTypeKind)) {
        ECoreType eType = get_core_type_(pType);
        switch (eType) {
            case ECORETYPE_COMPINT: *outErrWhenFalse = CERR_CANNOT_USE_COMPINT_TYPE_AT_RUNTIME; return false; 
            case ECORETYPE_FLOAT_LIT: *outErrWhenFalse = CERR_CANNOT_USE_FLOAT_LIT_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_ASTNODE: *outErrWhenFalse = CERR_CANNOT_USE_ASTCODE_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_ASTSEQ: *outErrWhenFalse = CERR_CANNOT_USE_ASTBLOCK_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_BUILTIN: *outErrWhenFalse = CERR_CANNOT_USE_BUILTIN_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_NAMESPACE: *outErrWhenFalse = CERR_CANNOT_USE_NAMESPACE_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_SOURCEFILE: *outErrWhenFalse = CERR_CANNOT_USE_SOURCEFILE_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_FOREIGN_SOURCE: *outErrWhenFalse = CERR_CANNOT_USE_FOREIGN_SOURCE_TYPE_AT_RUNTIME; return false;
            case ECORETYPE_TYPE: *outErrWhenFalse = CERR_CANNOT_USE_TYPE_OF_TYPES_AT_RUNTIME; return false;
        }

    } else switch (eTypeKind) {

        case ETypeKind::ETYPEKIND_ARRAY: {
            const TypeInfo_Array* pAsArray = (const TypeInfo_Array*)pType;
            return is_allowed_as_runtime_type(pAsArray->pElementType, pTCContext, outErrWhenFalse);
        } break;

        case ETypeKind::ETYPEKIND_POINTER: {
            const TypeInfo_Pointer* pAsPointer = (const TypeInfo_Pointer*)pType;
            return is_allowed_as_runtime_type(pAsPointer->pPointedToType, pTCContext, outErrWhenFalse);
        } break;

        case ETypeKind::ETYPEKIND_STRUCTLIKE: {
            if (pType->_coreType & COMPOUNDTYPE_IS_COMPTIME_ONLY) {
                *outErrWhenFalse = CERR_CANNOT_USE_STRUCTLIKE_FLAGGED_AS_COMPTIME_ONLY_AT_RUNTIME;
                return false;
            }
        } break;

        case ETypeKind::ETYPEKIND_PROCLIKEBODY: {
            u8 uProcKindAsTok = pType->_coreType + ETOK_PROC;
            if (uProcKindAsTok == ETOK_MACRO || uProcKindAsTok == ETOK_COMPTIMEFUNC) {
                *outErrWhenFalse = CERR_CANNOT_USE_MACRO_OR_COMPTIMEPROC_AT_RUNTIME;
                return false;
            }
        } break;

        case ETypeKind::ETYPEKIND_PROCLIKEOVERLOAD: {
            *outErrWhenFalse = CERR_CANNOT_USE_PROCLIKE_OVERLOAD_AT_RUNTIME;
            return false;
        } break;

        case ETypeKind::ETYPEKIND_PROCLIKEPOLYMORPH: {
            *outErrWhenFalse = CERR_CANNOT_USE_PROCLIKE_POLYMORPH_AT_RUNTIME;
            return false;
        } break;

        case ETypeKind::ETYPEKIND_STRUCTPOLYMORPH: {
            *outErrWhenFalse = CERR_CANNOT_USE_COMPOUND_POLYMORPH_AT_RUNTIME;
            return false;
        } break;
    }

    return true;
}

/*
local_func IREntry& tc_access_repo_instr(u64 uIRParam, IRAwareContext* pTCContext) {
    Assert_(ir_is_valid_param_(uIRParam));
    Assert_(!ir_is_immediate(uIRParam));
    // ----------------------
    IRRepo* pRepo;
    SourceFileDescAndState* pSourceFile;
    u32 uIndex;
    if (ir_is_non_imm_referencing_ir_entry(uIRParam, pTCContext, &pRepo, &uIndex, &pSourceFile)) {
        return ir_access_repo_instr(pRepo, uIndex);
    } else {
        Assume_(false);
    }
    // ----------------------
}
*/

local_func void recall_node_intrinsic_value_from_index(TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    if (has_node_no_tc_expr_value(pNode->pTCNode))
        return;
    pNode->pIntrinsicValue = pTCStatement->vecNodeValues[pNode->pTCNode->uIntrinsicValueIndex];
}

local_func void recall_node_final_value_from_index(TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    pNode->pFinalValue = pTCStatement->vecNodeValues[pNode->pTCNode->uFinalValueIndex];
}

#define if_expr_already_typechecked_phase1_recall_value_and_return_success(pNode, pTCStatement, pTCContext) do { \
    if (is_node_already_typechecked(pNode->pTCNode)) { \
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED( \
            "Node was already Typechecked => just recalling previous value"), pTCContext->pWorker); \
        recall_node_intrinsic_value_from_index(pNode, pTCStatement, pTCContext); \
        return ETCResult::ETCR_SUCCESS; \
    } \
} while(0)

#define if_expr_already_type_casted_phase2_recall_value_and_return_success(pNode, pTCStatement, pTCContext) do { \
    if (is_node_already_type_casted(pNode->pTCNode)) { \
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED( \
            "Node was already TC'd phase 2 (type-cast, or special) => just recalling previous value"), pTCContext->pWorker); \
        recall_node_final_value_from_index(pNode, pTCStatement, pTCContext); \
        return ETCResult::ETCR_SUCCESS; \
    } \
} while(0)

local_func ETCResult add_waiting_task_for_compound_body(const TypeInfo_CompoundBase* pAsCompound,
    u32 uNodeIndexWithWait, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Putting TC task for statement on hold, waiting for full-type resolution of compound %u in file %u ('%s')",
        u64(pAsCompound->uRegistrationIndex), u64(u32(pTCContext->pIsolatedSourceFile->iRegistrationIndex)), // TODO: we really need to assign a source file to compounds
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pAsCompound->pRegistration->iPrimaryIdentifier).c_str())),
        pTCContext->pWorker);

    TCWaitingReason waitingReason = make_waiting_reason(ETaskWaitingReason::EWR_COMPOUND_BODY_TYPECHECK_SUCCESS, 
        pAsCompound->pRegistration->pStatementWithSignature->iSourceFileIndex, pAsCompound->uRegistrationIndex);
    if (should_tc_ctx_halt_on_non_success(pTCContext))
        return add_waiting_task_to(pTCContext->pIsolatedSourceFile, waitingReason, uNodeIndexWithWait, pTCContext);
    else {
        TCContext* pNewWaitingContext = (TCContext*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
            sizeof(TCContext), alignof(TCContext));
        *pNewWaitingContext = *pTCContext;
        pNewWaitingContext->uGlobalStatementOnHold = pTCContext->pCurrentBlock->uStatementBeingTypechecked;
        return add_waiting_task_to(pTCContext->pIsolatedSourceFile, waitingReason, uNodeIndexWithWait, pNewWaitingContext);
    }
}

local_func void set_intrinsic_value_same_as_intrinsic_of(const TmpTCNode* pSourceExpr, TmpTCNode* pToSet)
{
    Assert_(is_node_already_typechecked(pSourceExpr->pTCNode));
    Assert_(!has_node_no_tc_expr_value(pSourceExpr->pTCNode));
    pToSet->pTCNode->uIntrinsicValueIndex = pSourceExpr->pTCNode->uIntrinsicValueIndex;
    pToSet->pIntrinsicValue = pSourceExpr->pIntrinsicValue;
}

local_func_inl ETCResult set_tc_success_with_same_value_as_intrinsic_of(const TmpTCNode* pSourceExpr, TmpTCNode* pToSet)
{
    set_intrinsic_value_same_as_intrinsic_of(pSourceExpr, pToSet);
    return set_node_typecheck_expr_success(pToSet->pTCNode);
}

local_func void set_intrinsic_value_same_as_final_of(const TmpTCNode* pSourceExpr, TmpTCNode* pToSet)
{
    Assert_(is_node_already_type_casted(pSourceExpr->pTCNode));
    Assert_(!has_node_no_tc_expr_value(pSourceExpr->pTCNode));
    pToSet->pTCNode->uIntrinsicValueIndex = pSourceExpr->pTCNode->uFinalValueIndex;
    pToSet->pIntrinsicValue = pSourceExpr->pFinalValue;
}

local_func_inl ETCResult set_tc_success_with_same_value_as_final_of(const TmpTCNode* pSourceExpr, TmpTCNode* pToSet)
{
    set_intrinsic_value_same_as_final_of(pSourceExpr, pToSet);
    return set_node_typecheck_expr_success(pToSet->pTCNode);
}

enum EValueSlotOnNode {
    ENODEVALUESLOT_INTRINSIC,
    ENODEVALUESLOT_FINAL,
};

local_func void set_existing_value_as(NodeValue* pValue, TmpTCNode* pNode, EValueSlotOnNode eSlot, TCStatement* pTCStatement)
{
    u32 uIndex = pTCStatement->vecNodeValues.size();
    pTCStatement->vecNodeValues.append(pValue);
    if (eSlot == EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC) {
        pNode->pTCNode->uIntrinsicValueIndex = uIndex;
        pNode->pIntrinsicValue = pValue;
    } else { Assert_(eSlot == EValueSlotOnNode::ENODEVALUESLOT_FINAL);
        pNode->pTCNode->uFinalValueIndex = uIndex;
        pNode->pFinalValue = pValue;
    }
}

local_func NodeValue* alloc_value_for(TmpTCNode* pNode, EValueSlotOnNode eSlot, TCStatement* pTCStatement, TCContext* pTCContext)
{
    u32 uIndex = pTCStatement->vecNodeValues.size();
    NodeValue* pResult = (NodeValue*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, sizeof(NodeValue), alignof(NodeValue));
    pTCStatement->vecNodeValues.append(pResult);
    if (eSlot == EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC) {
        pNode->pTCNode->uIntrinsicValueIndex = uIndex;
        pNode->pIntrinsicValue = pResult;
    } else { Assert_(eSlot == EValueSlotOnNode::ENODEVALUESLOT_FINAL);
        pNode->pTCNode->uFinalValueIndex = uIndex;
        pNode->pFinalValue = pResult;
    }
    return pResult;
}

local_func_inl ETCResult set_cast_success_with_same_value(TmpTCNode* pNode)
{
    Assert_(is_node_already_typechecked(pNode->pTCNode));
    pNode->pTCNode->uFinalValueIndex = pNode->pTCNode->uIntrinsicValueIndex;
    pNode->pFinalValue = pNode->pIntrinsicValue;
    return set_node_type_cast_expr_success(pNode->pTCNode);
}

local_func u32 do_store_value_to(u64 uIRtoStoreTo, u64 uIRofValueToStore, u8 uFormat, u32 uSlotsCount,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(pTCContext->uFlags & CTXFLAG_ALLOW_RUNTIME);
    Assert_(pTCContext->pProcResult);
    if (uSlotsCount) {
        Assert_(ir_is_valid_param(uIRtoStoreTo));
        Assert_(!ir_is_immediate(uIRtoStoreTo));
        Assert_(ir_is_valid_param(uIRofValueToStore));
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* pNewEntry = ir_append_new_entry(pTCContext->pRepo);
        if (uSlotsCount > 1u) {
            pNewEntry->uInstrCodeAndFormatAndFirstParam = IRIT_STORE_EXT | (u64(uFormat) << 16u) | uIRtoStoreTo;
            u64 uSlotsAndAlignDest = u64(uSlotsCount) | (u64(get_log2_of_natural_align_from_format(uFormat)) << 32);
            pNewEntry->uInstrMetaFlagsAndSecondParam = u64(uSlotsAndAlignDest) << IR_STD_PARAM_SHIFT;
            pNewEntry->metaValue._payload = 0;
            IREntry* pAdditionalParam = ir_append_new_entry(pTCContext->pRepo);
            pAdditionalParam->uInstrCodeAndFormatAndFirstParam = IRIT_CALLER_IN_PARAM | (u64(uFormat) << 16u) | uIRofValueToStore;
            u64 uSlotsAndAlignSrc = u64(uSlotsCount) | (u64(get_log2_of_natural_align_from_format(uFormat)) << 32); // TODO: possibly distinct align here ?
            pAdditionalParam->uInstrMetaFlagsAndSecondParam = u64(uSlotsAndAlignSrc) << IR_STD_PARAM_SHIFT;
            pAdditionalParam->metaValue._payload = 0;
            pTCStatement->uLastIRorGlobalTCResult = uPos + 1u;
        } else {
            pNewEntry->uInstrCodeAndFormatAndFirstParam = IRIT_STORE | (u64(uFormat) << 16u) | uIRtoStoreTo;
            pNewEntry->uInstrMetaFlagsAndSecondParam = uIRofValueToStore;
            pNewEntry->metaValue._payload = 0;
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        }
        return uPos;
    } else
        return 0u;
}

local_func u32 ir_make_decl_entry(IRRepo* pRepo, u32 uDeclIRITFlags, u32 uValueFlags, u64 uValuePayload, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign)
{
    u32 uValueSlotCount = uValueByteSize;
    u32 uLog2BytesPerSlot = get_log2_of_slot_size_from_format(uFormat);
    //Assert_(uLog2OfAlign >= uLog2BytesPerSlot);
    u32 uMaskBytesPerSlot = (1u << uLog2BytesPerSlot) - 1u;
    Assert_(0 == (uValueByteSize & uMaskBytesPerSlot));
    uValueSlotCount >>= uLog2BytesPerSlot;
    u32 uPosNewDecl = pRepo->uSize;
    IREntry* pDeclEntry = ir_append_new_entry(pRepo);
    static_assert(tIRITSecondParamSlot[IRIT_DECLARATION] == IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN);
    Assert_(0u == (uDeclIRITFlags & 0xFFFF00FFu));
    pDeclEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_DECLARATION) | u64(uDeclIRITFlags) | (u64(uFormat)<<16);
    Assert_(0u == (uValueFlags & 0xFF000000u));
    Assert_(uValueFlags & IRFLAG_IS_KNOWN);
    u64 uSlotCountAndAlign = u64(uValueSlotCount) | (u64(uLog2OfAlign) << 32);
    pDeclEntry->uInstrMetaFlagsAndSecondParam = u64(uValueFlags) | (uSlotCountAndAlign << IR_STD_PARAM_SHIFT);
    pDeclEntry->metaValue.knownValue._payload = uValuePayload;
    return uPosNewDecl;
}

local_func_inl u32 ir_make_non_embd_decl_without_nykas(IRRepo* pRepo, u32 uDeclIRITFlags, u32 uValueFlags,
                                                   u8* pValueData, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign)
{
    Assert_(0u == (uValueFlags & (IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_EMBD)));
    return ir_make_decl_entry(pRepo, uDeclIRITFlags, uValueFlags, reinterpret_cast<u64>(pValueData), uFormat, uValueByteSize, uLog2OfAlign);
}

local_func u32 ir_make_non_embd_decl_with_nykas(IRRepo* pRepo, Arena arena, u32 uDeclIRITFlags, u32 uValueFlags,
                                                u8* pValueData, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign, const u32* pNykaOffsets, u32 uNykaCount)
{
    Assert_(uValueFlags & IRFLAG_HAS_NYKA);
    Assert_(0u == (uValueFlags & IRFLAG_IS_KNOWN_EMBD));
    u32 uOffsetOfPtrAfterTable = align_to(8u, (uNykaCount + 1u) * 4u);
    u8* pNykaTableData = alloc_from(arena, uOffsetOfPtrAfterTable + 8u, 8u);
    u32* pNykaTable = (u32*)pNykaTableData;
    *pNykaTable = uNykaCount;
    const u32* pEndNykaOffsets = pNykaOffsets + uNykaCount;
    for (const u32* pCurrentNykaOffset = pNykaOffsets; pCurrentNykaOffset < pEndNykaOffsets; pCurrentNykaOffset++) {
        pNykaTable++;
        *pNykaTable = *pCurrentNykaOffset;
    }
    u8** pPtrToData = (u8**)(pNykaTableData + uOffsetOfPtrAfterTable);
    *pPtrToData = pValueData;
    return ir_make_decl_entry(pRepo, uDeclIRITFlags, uValueFlags, reinterpret_cast<u64>(pNykaTableData), uFormat, uValueByteSize, uLog2OfAlign);
}

local_func_inl u64 make_global_var_non_embd_decl_without_nykas(SourceFileDescAndState* pSourceFile, u32 uDeclIRITFlags, u32 uValueFlags,
                                                               u8* pValueData, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign)
{
    u32 uPos = ir_make_non_embd_decl_without_nykas(&(pSourceFile->filewiseGlobalVarRepo),
                                                   uDeclIRITFlags, uValueFlags, pValueData, uFormat, uValueByteSize, uLog2OfAlign);
    return ir_make_global_var_code_in_file(u32(pSourceFile->iRegistrationIndex), uPos);
}

local_func_inl u64 make_const_non_embd_decl_without_nykas(IRRepo* pRepo, u32 uDeclIRITFlags, u32 uValueFlags,
                                                          u8* pValueData, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign)
{
    u32 uPos = ir_make_non_embd_decl_without_nykas(pRepo,
                                                   uDeclIRITFlags, uValueFlags, pValueData, uFormat, uValueByteSize, uLog2OfAlign);
    return ir_make_std_code(pRepo->uIRRepoId, uPos);
}

local_func_inl u64 make_global_var_non_embd_decl_with_nykas(SourceFileDescAndState* pSourceFile, u32 uDeclIRITFlags, u32 uValueFlags,
                                                            u8* pValueData, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign, const u32* pNykaOffsets, u32 uNykaCount)
{
    u32 uPos = ir_make_non_embd_decl_with_nykas(&(pSourceFile->filewiseGlobalVarRepo), pSourceFile->localArena,
                                                uDeclIRITFlags, uValueFlags, pValueData, uFormat, uValueByteSize, uLog2OfAlign,
                                                pNykaOffsets, uNykaCount);
    return ir_make_global_var_code_in_file(u32(pSourceFile->iRegistrationIndex), uPos);
}

local_func_inl u64 make_const_non_embd_decl_with_nykas(IRRepo* pRepo, Arena arena, u32 uDeclIRITFlags, u32 uValueFlags,
                                                       u8* pValueData, u8 uFormat, u32 uValueByteSize, u32 uLog2OfAlign, const u32* pNykaOffsets, u32 uNykaCount)
{
    u32 uPos = ir_make_non_embd_decl_with_nykas(pRepo, arena,
                                                uDeclIRITFlags, uValueFlags, pValueData, uFormat, uValueByteSize, uLog2OfAlign,
                                                pNykaOffsets, uNykaCount);
    return ir_make_std_code(pRepo->uIRRepoId, uPos);
}

enum ERangeCheckResultReason {
    ERCR_IN_RANGE = 0,

    ERCR_NEGATIVE_VALUE_VS_NON_SIGNED,
    ERCR_NEGATIVE_VALUE_VS_SPECIAL_RAW,
    ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS,

    ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL,
    ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED,
    ERCR_POSITIVE_VALUE_TOO_LARGE,

    ERCR_ERROR,
};
static_assert(ERangeCheckResultReason::ERCR_IN_RANGE == 0, "range check result functions should return any 'error' reason with "
    "a non-zero value, and are currently named accordingly (as err-check, bool-like functions). being in-range should thus stay 0");

/*
local_func ERangeCheckResultReason is_known_non_compint_integral_outside_range_returning_reason(const TypeInfo_Integral* pKnownValueType,
    u32 uMetaValueFlags, AKnownValue knownValue, const TypeInfo_Integral* pDestType, CompilationContext* pContext)
{
    Assert_(pKnownValueType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pDestType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    u8 uKnownFormat = get_ir_format(pKnownValueType);
    u8 uDestFormat = get_ir_format(pDestType);
    Assert_(uKnownFormat <= 0x05u); // TODO: allow raw > 256b up to format 0x07u ?
    Assert_(uDestFormat <= 0x05u); // TODO: allow raw > 256b up to format 0x07u ?

    if (pKnownValueType == pDestType)
        return ERCR_IN_RANGE; // shortcut : ensured in range since was already encoded as such...

    bool bSignedKnown = is_signed(pKnownValueType);
    bool bSignedDest = is_signed(pDestType);
    bool bDestIsBool = (pDestType == g_pCoreTypesInfo[ECORETYPE_BOOL]);
    bool bDestIsCodepoint = (pDestType == g_pCoreTypesInfo[ECORETYPE_CODEPOINT]);
    bool bSpecialDest = bDestIsBool || bDestIsCodepoint;
    if (bSignedKnown == bSignedDest && !bSpecialDest && uDestFormat >= uKnownFormat)
        return ERCR_IN_RANGE; // shortcut : ensured in range if dest at least as-wide and same signedness...

    if (bSignedDest && uDestFormat > uKnownFormat)
        return ERCR_IN_RANGE; // shortcut : ensured in range if dest is signed of larger width

    if (!bSignedDest && bDestIsCodepoint && uKnownFormat < 0x02u)                   
        return ERCR_IN_RANGE; // shortcut : ensured in range if dest is codepoint and source is u8 or u16

    if (uKnownFormat <= 0x03u) { // formats up to 64b
        u64 uWhenPositiveValueAs64 = 0;
        i64 iWhenSignedValueAs64 = 0;
        IRInfo knownValueInfo = IRInfo{u64(uMetaValueFlags), meta_from_known(knownValue)};
        switch (uKnownFormat) {
            case 0x00u: { // 8b
                u8 uValue = ir_get_u8_value_from_known(knownValueInfo);
                uWhenPositiveValueAs64 = u64(uValue);
                iWhenSignedValueAs64 = i64(i8(uValue));
            } break;
            case 0x01u: { // 16b
                u16 uValue = ir_get_u16_value_from_known(knownValueInfo);
                uWhenPositiveValueAs64 = u64(uValue);
                iWhenSignedValueAs64 = i64(i16(uValue));
            } break;
            case 0x02u: { // 32b
                u32 uValue = ir_get_u32_value_from_known(knownValueInfo);
                uWhenPositiveValueAs64 = u64(uValue);
                iWhenSignedValueAs64 = i64(i16(uValue));
            } break;
            case 0x03u: { // 64b
                u64 uValue = ir_get_u64_value_from_known(knownValueInfo);
                uWhenPositiveValueAs64 = uValue;
                iWhenSignedValueAs64 = i64(uValue);
            } break;
            default:
                Assume_(false);
                return ERCR_ERROR;
        }
        if (bSignedKnown) {
            if (iWhenSignedValueAs64 < 0) {
                if (!bSignedDest)
                    return ERCR_NEGATIVE_VALUE_VS_NON_SIGNED;
                else {
                    Assert_(uDestFormat < uKnownFormat); // should have been shortcut above otherwise
                    switch (uDestFormat) {
                        case 0x00u: { // i8
                            return iWhenSignedValueAs64 < -i64(0x0080u) ? ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS : ERCR_IN_RANGE;
                        } break;
                        case 0x01u: { // i16
                            return iWhenSignedValueAs64 < -i64(0x00'8000u) ? ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS : ERCR_IN_RANGE;
                        } break;
                        case 0x02u: { // i32
                            return iWhenSignedValueAs64 < -i64(0x00'8000'0000uLL) ? ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS : ERCR_IN_RANGE;
                        } break;
                        default:
                            Assume_(false);
                            return ERCR_ERROR;
                    }
                }
            }
        }

        if (bSignedDest) {

            Assert_(uDestFormat < uKnownFormat || (uDestFormat == uKnownFormat && !bSignedKnown)); // should have been shortcut above otherwise
            switch (uDestFormat) {
                case 0x00u: { // i8
                    if (uWhenPositiveValueAs64 >= 0x0080u) {
                        return (uWhenPositiveValueAs64 < 0x0100u) ? ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED : ERCR_POSITIVE_VALUE_TOO_LARGE;
                    } else
                        return ERCR_IN_RANGE;
                } break;
                case 0x01u: { // i16
                    if (uWhenPositiveValueAs64 >= 0x00'8000u) {
                        return (uWhenPositiveValueAs64 < 0x01'0000u) ? ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED : ERCR_POSITIVE_VALUE_TOO_LARGE;
                    } else
                        return ERCR_IN_RANGE;
                } break;
                case 0x02u: { // i32
                    if (uWhenPositiveValueAs64 >= 0x00'8000'0000uLL) {
                        return (uWhenPositiveValueAs64 < 0x01'0000'0000uLL) ? ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED : ERCR_POSITIVE_VALUE_TOO_LARGE;
                    } else
                        return ERCR_IN_RANGE;
                } break;
                case 0x03u: { // i64
                    Assert_(!bSignedKnown && uKnownFormat == 0x03u);
                    if (uWhenPositiveValueAs64 >= 0x8000'0000'0000'0000uLL) {
                        return ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED;
                    } else
                        return ERCR_IN_RANGE;
                } break;
                default:
                    Assume_(false);
                    return ERCR_ERROR;
            }

        } else {

            if (bDestIsBool) {
                if (uWhenPositiveValueAs64 > 0x01u)
                    return ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL;
                else
                    return ERCR_IN_RANGE;
            } else if (bDestIsCodepoint) {
                if (uWhenPositiveValueAs64 > UNICODE_CODEPOINT_MAX)
                    return ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL;
                else
                    return ERCR_IN_RANGE;
            } else {
                if (uDestFormat >= uKnownFormat) {
                    Assert_(bSignedKnown);
                    return ERCR_IN_RANGE;
                } else {
                    switch (uDestFormat) {
                        case 0x00u: { // u8
                            return (uWhenPositiveValueAs64 >= 0x0100u) ? ERCR_POSITIVE_VALUE_TOO_LARGE : ERCR_IN_RANGE;
                        } break;
                        case 0x01u: { // u16
                            return (uWhenPositiveValueAs64 >= 0x01'0000u) ? ERCR_POSITIVE_VALUE_TOO_LARGE : ERCR_IN_RANGE;
                        } break;
                        case 0x02u: { // u32
                            return (uWhenPositiveValueAs64 >= 0x01'0000'0000uLL) ? ERCR_POSITIVE_VALUE_TOO_LARGE : ERCR_IN_RANGE;
                        } break;
                        default:
                            Assume_(false);
                            return ERCR_ERROR;
                    }
                }
            }
        }

    } else {
        platform_log_error("is_known_non_compint_integral_outside_range_returning_reason() : >64b not yet implemented");
        return ERCR_ERROR;
    }
}
*/

// returns true if the compint abs value is precisely equal to an integral value where only the high bit is set, depending on format. 
// Note that it was designed with 'abs' in mind since our caller indeed do not care about the sign of the compint value when doing this check.
// cuz... eg, '0 - 0x8000' modulo 16b == '0x8000'... @see tc_do_compint_cast_to_integral for original intent.
local_func bool compint_abs_equals_sign_of_format(u8 uFormat, u64 uCompintPayload)
{
    Assert_(uFormat <= 7u);
    u32 uSizeCategory = u32(uCompintPayload) & COMPINT_SIZE_MASK;
    if (uSizeCategory == COMPINT_SIZE_SMALL_EMBD) {
        if (uFormat >= 3u)
            return false;
        else {
            u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u32 uBitCount = (1u << (uFormat+3u)); // bytes count is 2^format ; bitcount is bytes count * 8
            return uAbsValue == (1uLL << (uBitCount-1u));
        }
    } else {
        // TODO
        Assert(false, "compint_abs_equals_sign_of_format() : not yet implemented when non-embedded");
    }
}

// returns a 'reason' why a non-compint (yet known) integral value would fall outside the representable range of an integral format.
// note that the returned 'reason' enum has value 0 when the compint *is okay within the range*.
local_func ERangeCheckResultReason is_non_compint_known_integral_outside_other_integral_range_returning_reason(u8 uSrcFormat, bool bSrcSigned,
    u32 uSrcMetaFlags, AKnownValue knownSrc, const TypeInfo_Integral* pDestType, CompilationContext* pContext)
{
    Assert_(uSrcFormat <= 0x07u);
    Assert_(!bSrcSigned || uSrcFormat <= 0x05u);
    Assert_(uSrcMetaFlags & IRFLAG_IS_KNOWN);
    Assert_(0u == (uSrcMetaFlags & IRFLAG_HAS_NYKA));
    Assert_(pDestType && pDestType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    u8 uDestFormat = get_ir_format(pDestType);
    Assert_(uDestFormat <= 0x07u);

    // Shortcuts from comparing types only:

    if (!bSrcSigned) {
        // unsigned to larger (either signed or unsigned) is always valid
        if (uDestFormat > uSrcFormat)
            return ERangeCheckResultReason::ERCR_IN_RANGE;
        
        // unsigned to unsigned of same width (unless dest is raw BOOL or raw CODEPOINT) is valid
        if (!is_signed(pDestType) && uDestFormat == uSrcFormat && pDestType != g_pCoreTypesInfo[ECORETYPE_BOOL] && pDestType != g_pCoreTypesInfo[ECORETYPE_CODEPOINT])
            return ERangeCheckResultReason::ERCR_IN_RANGE; // (may happend between u32 and nat, or if checking explicit casts vs aliases)

        // signed to larger signed, or same-width signed (may happend between i32 and int, or if checking explicit casts vs aliases)
    } else if (is_signed(pDestType) && uDestFormat >= uSrcFormat) {
        return ERangeCheckResultReason::ERCR_IN_RANGE;
    }

    // Did not take a shortcut => we need to check the value, now:

    if (uSrcMetaFlags & IRFLAG_IS_KNOWN_EMBD) {
        Assert_(uSrcFormat <= 0x03u);
        u64 uEmbeddedValue = knownSrc.uEmbeddedValue;
        u64 uValue64;
        if (!bSrcSigned) {
            // we can be confident that the unsigned embedded value indeed represents the value
            uValue64 = uEmbeddedValue;
            goto when_known_u64;
        } else {
            i64 iValue64 = i64(uEmbeddedValue);
            if (uSrcFormat >= 3u) {
                // we can be confident that i64-reinterp of the embedded uValue indeed represents the value
            } else if (uSrcFormat == 2u) { // 32b signed - we must ignore embedded high bits 32..63 and fill them with sign-extent of the 32 lsb.
                iValue64 = i64(i32(uEmbeddedValue));
            } else if (uSrcFormat == 1u) { // 16b signed - we must ignore embedded high bits 16..63 and fill them with sign-extent of the 16 lsb.
                iValue64 = i64(i16(uEmbeddedValue));
            } else {                       // 8b signed - we must ignore embedded high bits 8..63 and fill them with sign-extent of the 8 lsb.
                iValue64 = i64(i8(uEmbeddedValue));
            }

            if (iValue64 < 0 && !is_signed(pDestType)) {
                if (pDestType != g_pCoreTypesInfo[ECORETYPE_BOOL] && pDestType != g_pCoreTypesInfo[ECORETYPE_CODEPOINT])
                    return ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_VS_NON_SIGNED;
                else
                    return ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_VS_SPECIAL_RAW;
            }

            u32 uDestBitCount = (1u << (uDestFormat+3u)); // bytes count is 2^format ; bitcount is bytes count * 8
            if (iValue64 < 0) {
                Assert_(is_signed(pDestType));
                Assert_(uDestFormat < 0x03u); // should have been shortcut in prechecks above, otherwise
                u64 uMaxNegR64 = 0xFFFF'FFFF'FFFF'FFFFuLL << (uDestBitCount-1u);
                if (u64(iValue64) >= uMaxNegR64)
                    return ERangeCheckResultReason::ERCR_IN_RANGE;
                else
                    return ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS;
            } // otherwise fallthrough

            uValue64 = u64(iValue64);

            { when_known_u64:
                if (uDestFormat > 3u) {
                    // TODO
                    platform_log_error("is_non_compint_known_integral_outside_other_integral_range_returning_reason() : not yet implemented for dest formats > 64b");
                    return ERangeCheckResultReason::ERCR_ERROR;
                }
                u32 uMaskR64S = 0xFFFF'FFFF'FFFF'FFFFuLL << (uDestBitCount-1u);
                u32 uMaskR64U = uMaskR64S << 1u;
                if (uValue64 & uMaskR64U) // would overflow even an unsigned of same size => return 'positive too large', whatever the case
                    return ERangeCheckResultReason::ERCR_POSITIVE_VALUE_TOO_LARGE;
                else {
                    // we can specially handle 1b too large vs signed, or too large vs special (better err report ; and helps with compint transmute)
                    if (is_signed(pDestType)) {
                        if (uValue64 & uMaskR64S)
                            return ERangeCheckResultReason::ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED;
                        // otherwise fallthrough
                    } else if (pDestType == g_pCoreTypesInfo[ECORETYPE_BOOL]) {
                        if (uValue64 > 1uLL)
                            return ERangeCheckResultReason::ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL;
                        // otherwise fallthrough
                    } else if (pDestType == g_pCoreTypesInfo[ECORETYPE_CODEPOINT]) {
                        if (uValue64 > u64(UNICODE_CODEPOINT_MAX))
                            return ERangeCheckResultReason::ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL;
                        // otherwise fallthrough
                    }
                    return ERangeCheckResultReason::ERCR_IN_RANGE;
                }
            }
        }
    } else {
        Assert_(uSrcFormat >= 0x03u);
        // TODO
        platform_log_error("is_non_compint_known_integral_outside_other_integral_range_returning_reason() : not yet implemented for non-embedded values");
        return ERangeCheckResultReason::ERCR_ERROR;
    }
}

// returns a 'reason' why a compint value would fall outside the representable range of *another* integral format.
// note that the returned 'reason' enum has value 0 when the compint *is okay within the range*.
local_func ERangeCheckResultReason is_compint_outside_integral_range_returning_reason(u64 uCompintPayload,
    const TypeInfo_Integral* pDestType, CompilationContext* pContext)
{
    Assert_(pDestType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    u8 uDestFormat = get_ir_format(pDestType);
    Assert_(uDestFormat <= 0x07u);
    
    if ((uCompintPayload & COMPINT_FLAG_IS_NEGATIVE) && !is_signed(pDestType)) {
        if (pDestType != g_pCoreTypesInfo[ECORETYPE_BOOL] && pDestType != g_pCoreTypesInfo[ECORETYPE_CODEPOINT])
            return ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_VS_NON_SIGNED;
        else
            return ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_VS_SPECIAL_RAW;
    }

    u64 uSizeCategory = (uCompintPayload & COMPINT_SIZE_MASK);
    if (uSizeCategory == COMPINT_SIZE_SMALL_EMBD) {
        u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
        u64 uAsR64 = (uCompintPayload & COMPINT_FLAG_IS_NEGATIVE) ? 0uLL - uAbsValue : uAbsValue;
        // we can safely interpret embedded compints as 'i64' here, since their abs value is only encoded on 61b.
        return is_non_compint_known_integral_outside_other_integral_range_returning_reason(0x03u, true,
            IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST, known_from_payload(uAsR64), pDestType, pContext);
    } else {
        // TODO
        platform_log_error("is_compint_outside_integral_range_returning_reason() : not yet implemented for non-embedded compint values");
        return ERangeCheckResultReason::ERCR_ERROR;
    }
}

// returns a 'reason' why a known integral value would fall outside the representable range of any other integral format.
// note that the returned 'reason' enum has value 0 when the compint *is okay within the range*.
local_func_inl ERangeCheckResultReason is_known_integral_outside_range_returning_reason(const TypeInfo_Integral* pKnownValueType,
    const IRInfo& knownValueInfo, const TypeInfo_Integral* pDestType, CompilationContext* pContext)
{
    if (pDestType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) // not planned to be be called in case dest is compint, but... oh well, compints can take them all...
        return ERangeCheckResultReason::ERCR_IN_RANGE;
    if (pKnownValueType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(irflag_is_tc_only(knownValueInfo.uIRandMetaFlags));
        u64 uCompintPayload = knownValueInfo.metaValue.knownValue.uEmbeddedValue;
        return is_compint_outside_integral_range_returning_reason(uCompintPayload, pDestType, pContext);
    } else {
        return is_non_compint_known_integral_outside_other_integral_range_returning_reason(get_ir_format(pKnownValueType),
            is_signed(pKnownValueType), u32(knownValueInfo.uIRandMetaFlags), knownValueInfo.metaValue.knownValue, pDestType, pContext);
    }
}

local_func_inl bool is_known_integral_outside_range(const TypeInfo_Integral* pKnownValueType, const IRInfo& knownValueInfo, const TypeInfo_Integral* pDestType,
    CompilationContext* pContext, bool* outIsEnsuredLess)
{
    ERangeCheckResultReason eReason = is_known_integral_outside_range_returning_reason(pKnownValueType, knownValueInfo, pDestType, pContext);
    Assert(eReason != ERCR_ERROR, "is_known_integral_outside_range() : case not implemented");
    if (eReason != ERangeCheckResultReason::ERCR_IN_RANGE) {
        *outIsEnsuredLess = (eReason <= ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS);
        return true;
    } else {
        return false;
    }
}

// constexpr IRInfo info0 { ir_make_r32_immediate(0u)|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST, meta_from_payload(0uLL) };
// constexpr IRInfo info0_larger_than_64b { ir_make_r32_immediate(0u)|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_EMBD_SIGN_EXT, meta_from_payload(0uLL) };

/*
local_func u64 get_r64_from_compint(u64 uCompintPayload)
{
    u32 uSizeCategory = u32(uCompintPayload) & COMPINT_SIZE_MASK;
    u64 uAbsValue;
    if (uSizeCategory == COMPINT_SIZE_SMALL_EMBD) {
        uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
    } else {
        const u64* tData = reinterpret_cast<const u64*>(uCompintPayload & COMPINT_ASPTR_MASK);
        if (uSizeCategory == COMPINT_SIZE_NLEGS) {
            uAbsValue = tData[1];
        } else {
            Assert_(uSizeCategory == COMPINT_SIZE_1LEG || uSizeCategory == COMPINT_SIZE_2LEGS);
            uAbsValue = tData[0];
        }
    }
    return (uCompintPayload & COMPINT_FLAG_IS_NEGATIVE) ? 0uLL - uAbsValue : uAbsValue;
}

local_func u256 get_r256_from_compint(u64 uCompintPayload)
{
    u32 uSizeCategory = u32(uCompintPayload) & COMPINT_SIZE_MASK;
    u256 uAbsValue = u256{};
    if (uSizeCategory == COMPINT_SIZE_SMALL_EMBD) {
        uAbsValue.tLegs[0] = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
    } else {
        const u64* tData = reinterpret_cast<const u64*>(uCompintPayload & COMPINT_ASPTR_MASK);
        if (uSizeCategory == COMPINT_SIZE_NLEGS) {
            u64 uLegCount = tData[0];
            Assert_(uLegCount >= 3u);
            uAbsValue.tLegs[0] = tData[1];
            uAbsValue.tLegs[1] = tData[2];
            uAbsValue.tLegs[2] = tData[3];
            if (uLegCount >= 4u)
                uAbsValue.tLegs[3] = tData[4];
        } else {
            Assert_(uSizeCategory == COMPINT_SIZE_1LEG || uSizeCategory == COMPINT_SIZE_2LEGS);
            uAbsValue.tLegs[0] = tData[0];
            if (uSizeCategory == COMPINT_SIZE_2LEGS)
                uAbsValue.tLegs[1] = tData[1];
        }
    }
    return (uCompintPayload & COMPINT_FLAG_IS_NEGATIVE) ? neg256(uAbsValue) : uAbsValue;
}

local_func void get_r1024_from_compint(u64 uCompintPayload, u64* ioLegs)
{
    // TODO
    Assert_(false, "get_r1024_from_compint() : not yet implemented");
}

local_func u64 get_r64_from_known_non_compint_integral(u8 uFormat, bool bIsSignedType, u32 uMetaFlags, AKnownValue knownValue)
{
    Assert_(uFormat <= 0x07u);
    Assert_(irflag_is_known_non_nyka(uMetaFlags));
    Assert_(!irflag_is_tc_only(uMetaFlags));
    u64 uPayload64;
    if (irflag_is_known_embd(knownValue)) {
        uPayload64 = knownValue.uEmbeddedValue;
    } else {
        uPayload64 = *reinterpret_cast<const u64*>(knownValue.pPtrToRawData);
    }
    if (bIsSignedType) {
        switch (uFormat) {
            case 0x00u: { // 8b
                u8 uValue8 = u8(uPayload64);
                return u64(i64(i8(uValue8)));
            } break;
            case 0x01u: { // 16b
                u16 uValue16 = u16(uPayload64);
                return u64(i64(i16(uValue16)));
            } break;
            case 0x02u: { // 32b
                u32 uValue32 = u32(uPayload64);
                return u64(i64(i32(uValue32)));
            } break;
        } // otherwise fallthrough
    }
    return uPayload64;
}

local_func u256 get_r256_from_known_non_compint_integral(u8 uFormat, bool bIsSigned, u32 uMetaFlags, AKnownValue knownValue)
{
    Assert_(uFormat <= 0x07u);
    Assert_(irflag_is_known_non_nyka(uMetaFlags));
    Assert_(!irflag_is_tc_only(uMetaFlags));
    u256 uResult = u256{};
    // TODO
    Assert_(false, "get_r256_from_known_non_compint_integral() : not yet implemented");
    return uResult;
}

local_func void get_r1024_from_known_non_compint_integral(u8 uFormat, u32 uMetaFlags, AKnownValue knownValue, u64* ioLegs)
{
    Assert_(uFormat <= 0x07u);
    Assert_(irflag_is_known_non_nyka(uMetaFlags));
    Assert_(!irflag_is_tc_only(uMetaFlags));
    // TODO
    Assert_(false, "get_r1024_from_known_non_compint_integral() : not yet implemented");
}

local_func_inl u64 get_r64_from_known_integral(TypeInfo_Integral* pIntegralType, u32 uMetaFlags, AKnownValue knownValue)
{
    if (pIntegralType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(irflag_is_tc_only(uMetaFlags));
        return get_r64_from_compint(knownValue.uEmbeddedValue);
    } else {
        Assert_(!irflag_is_tc_only(uMetaFlags));
        return get_r64_from_known_non_compint_integral(get_ir_format(pIntegralType), is_signed(pIntegralType), uMetaFlags, knownValue);
    }
}

local_func_inl u256 get_r256_from_known_integral(TypeInfo_Integral* pIntegralType, u32 uMetaFlags, AKnownValue knownValue)
{
    if (pIntegralType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(irflag_is_tc_only(uMetaFlags));
        return get_r256_from_compint(knownValue.uEmbeddedValue);
    } else {
        Assert_(!irflag_is_tc_only(uMetaFlags));
        return get_r256_from_known_non_compint_integral(get_ir_format(pIntegralType), is_signed(pIntegralType), uMetaFlags, knownValue);
    }
}

local_func_inl void get_r1024_from_known_integral(TypeInfo_Integral* pIntegralType, u32 uMetaFlags, AKnownValue knownValue, u64* ioLegs)
{
    if (pIntegralType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(irflag_is_tc_only(uMetaFlags));
        get_r1024_from_compint(knownValue.uEmbeddedValue, ioLegs);
    } else {
        Assert_(!irflag_is_tc_only(uMetaFlags));
        get_r1024_from_known_non_compint_integral(get_ir_format(pIntegralType), is_signed(pIntegralType), uMetaFlags, knownValue, ioLegs);
    }
}

local_func_inl u64 get_r64_from_known_integral(NodeValue* pValue)
{
    Assert_(is_value_tc_const(pValue));
    return get_r64_from_known_integral(pValue->pType, u32(pValue->info.uIRandMetaFlags), pValue->info.metaValue.knownValue);
}

local_func_inl u256 get_r256_from_known_integral(NodeValue* pValue)
{
    Assert_(is_value_tc_const(pValue));
    return get_r256_from_known_integral(pValue->pType, u32(pValue->info.uIRandMetaFlags), pValue->info.metaValue.knownValue);
}

local_func_inl void get_r1024_from_known_integral(NodeValue* pValue, u64* ioLegs)
{
    Assert_(is_value_tc_const(pValue));
    get_r1024_from_known_integral(pValue->pType, u32(pValue->info.uIRandMetaFlags), pValue->info.metaValue.knownValue, ioLegs);
}
*/

local_func IRInfo get_max_of_integral_as_IRInfo(u8 uDesiredFormat, const TypeInfo_Integral* pType, IRAwareContext* pCtx)
{
    Assert_(pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(uDesiredFormat >= get_ir_format(pType));
    i32 expect0;
    u64 uIROfMax = ir_decode_nyka_value(pType->uNykaOfMax, &expect0);
    Assert_(expect0 == 0);
    Assert_(!ir_is_immediate(uIROfMax));
    IRRepo* pRepo;
    u32 uIndex;
    SourceFileDescAndState* pSourceFile;
    EEntryKind eKind;
    ir_decode_non_imm(uIROfMax, pCtx, &pRepo, &uIndex, &pSourceFile, &eKind);
    Assert_(pRepo);
    IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
    return IRInfo { uIROfMax|(entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_METAMASK), entry.metaValue };
}

local_func IRInfo get_min_of_integral_as_IRInfo(u8 uDesiredFormat, const TypeInfo_Integral* pType, IRAwareContext* pCtx)
{
    Assert_(pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(!is_signed(pType) || uDesiredFormat >= get_ir_format(pType));
    i32 expect0;
    u64 uIROfMin = ir_decode_nyka_value(pType->uNykaOfMin, &expect0);
    Assert_(expect0 == 0);
    Assert_(!ir_is_immediate(uIROfMin));
    IRRepo* pRepo;
    u32 uIndex;
    SourceFileDescAndState* pSourceFile;
    EEntryKind eKind;
    ir_decode_non_imm(uIROfMin, pCtx, &pRepo, &uIndex, &pSourceFile, &eKind);
    Assert_(pRepo);
    IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
    return IRInfo { uIROfMin|(entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_METAMASK), entry.metaValue };
}

local_func ETCResult consolidate_tc_const_flag_on_info(IRInfo* ioInfo, const TypeInfo* pType,
    TmpTCNode* pExprIfErr, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(!irflag_is_tc_only(ioInfo->uIRandMetaFlags));
    Assert_(ir_is_valid_param_(ioInfo->uIRandMetaFlags));

    if (!irflag_is_known_or_nyka(ioInfo->uIRandMetaFlags)) // if not known, cannot be const
        return ETCResult::ETCR_SUCCESS;
    if (ioInfo->uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA)   // if has local nykas, cannot be const
        return ETCResult::ETCR_SUCCESS;

    switch (get_type_kind(pType)) {
    
        case ETYPEKIND_INTEGRAL: {
            const TypeInfo_Integral* pAsIntegralType = (const TypeInfo_Integral*)pType;
            ioInfo->uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;    // instance of integral type is const as soon as known (and no local nykas)
        } break;

        case ETYPEKIND_FLOATINGPOINT: {
            const TypeInfo_FloatingPoint* pAsFPType = (const TypeInfo_FloatingPoint*)pType;
            if (!irflag_is_or_has_nyka(ioInfo->uIRandMetaFlags))
                ioInfo->uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;    // instance of floating-point type is const if known with no nykas
        } break;

        case ETYPEKIND_ENUM: {
            const TypeInfo_Enum* pAsEnumType = (const TypeInfo_Enum*)pType;
            return consolidate_tc_const_flag_on_info(ioInfo, pAsEnumType->pBaseType, pExprIfErr, pTCStatement, pTCContext);
        } break;
    
        case ETYPEKIND_POINTER: {
            const TypeInfo_Pointer* pAsPtrType = (const TypeInfo_Pointer*)pType;
            ioInfo->uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;    // instance of ptr type is const as soon as known (and no local nykas)
        } break;

        case ETYPEKIND_ARRAY: {
            const TypeInfo_Array* pAsArrayType = (const TypeInfo_Array*)pType;
            // TODO: if static array : const if can be said that each element is.
            // otherwise TODO
            platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards array types");
        } break;

        case ETYPEKIND_STRUCTLIKE: {
            const TypeInfo_StructLike* pAsStructType = (const TypeInfo_StructLike*)pType;
            // TODO: const if can be said that each member is.
                    platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards struct types");
        } break;

        case ETYPEKIND_OTHERCORE: {
            const TypeInfo_OtherCore* pAsOtherCoreType = (const TypeInfo_OtherCore*)pType;
            if (pAsOtherCoreType->_coreFlags & OTHERCOREFLAG_IS_STRING) {
                // TODO: CHECK: CLEANUP: accept null compacts as consts ?
                if (pAsOtherCoreType->_coreFlags & STRINGFLAG_IS_COMPACT) {
                    // TODO: const if pointing to known base with no nykas, at least offset 8 if no alloc or 24 if there is, and:
                    // given u32 length at -4, base is of sufficient length, and ends with 0
                    // if alloc: given alloc fn ptr at -16 and alloc opaque data ptr at -24, alloc should be const.
                    platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards compact string types");
                } else {
                    // TODO: const if length & flags have no nykas, and pointing to known base with no nykas, and of sufficient length.
                    platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards non-compact string types");
                }
            }
        } break;

        case ETYPEKIND_HWVECTOR: {
            const TypeInfo_HwVector* pAsHwVectorType = (const TypeInfo_HwVector*)pType;
            // TODO: const if can be said that each scalar in vector is.
            platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards hwvector types");
        } break;

        case ETYPEKIND_SET: {
            const TypeInfo_Set* pAsSetType = (const TypeInfo_Set*)pType;
            // TODO
            platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards set types");
        } break;

        case ETYPEKIND_MAP: {
            const TypeInfo_Map* pAsMapType = (const TypeInfo_Map*)pType;
            // TODO
            platform_log_info("*** warning : consolidate_tc_const_flag_on_info() : not implemented towards map types");
        } break;

        case ETYPEKIND_PROCLIKEBODY: {
            const TypeInfo_ProcLike* pAsProcType = (const TypeInfo_ProcLike*)pType;
            if (ioInfo->uIRandMetaFlags & IRFLAG_HAS_NYKA) {
                Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                i32 iOffset; u64 uBaseIR = ir_decode_nyka_value(ioInfo->metaValue.knownValue.uEmbeddedValue, &iOffset);
                Assert_(!ir_is_immediate(uBaseIR));
                if (iOffset == 0 && 0uLL != (uBaseIR & IR_STD_PARAM_HIGHMASK)) { // no offset, and not a programwise entry
                    u16 uRepoIndex = u16(uBaseIR >> IR_STD_PARAM_REPO_ID_SHIFT);
                    if (uRepoIndex >= IR_REPO_ID_FIRST_FILE) {
                        u32 uFileIndex = uRepoIndex - IR_REPO_ID_FIRST_FILE;
                        u32 uPayload32 = u32(uBaseIR >> IR_STD_PARAM_SHIFT);
                        u32 uHigh22and23 = uPayload32 & 0x00C0'0000u;
                        if (uHigh22and23 == 0x0080'0000u) { // this is indeed a known ref to a procbody
                            ioInfo->uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
                        }
                    }
                }
            }
        } break;
    }

    return ETCResult::ETCR_SUCCESS;
}

#endif // LOCLIB_TYPE_CHECKER_CORE_H_
 