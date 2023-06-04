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

#ifndef LOCLIB_TYPE_CHECKER_H_
#define LOCLIB_TYPE_CHECKER_H_

#include "LocLib_TypeCheckerBase.h"
#include "LocLib_TC_Casts.h"
#include "LocLib_TC_InvocLike.h"
#include "LocLib_TC_ConstEvals.h"
#include "LocLib_IR_SolverInterface.h"
#include "LocLib_TC_StdOpsEval.h"
#include "LocLib_TC_Branching.h"
#include "LocLib_TC_Binding.h"
#include "LocLib_TC_AdvancedDeclarations.h"
#include "LocLib_TC_PanStatements.h"
#include "LocLib_Type_Registration.h"
#include "LocLib_TC_TypeCtors.h"
#include "LocLib_TC_Indexing.h"

local_func void emit_proc_intro(TCProcBodyResult* pProcBody, CompilationContext* pEvalContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Emitting Proc intro"), pEvalContext->pWorker);

    // TODO: pre-intro with params having default values ??
    const TypeInfo_ProcLike* pProcSign = pProcBody->procSign;
    u8 uTotalParamsCount = u8(pProcSign->params.size());
    if (uTotalParamsCount) {
        for (u8 uParamIndex = 0; uParamIndex < uTotalParamsCount; uParamIndex++) {
            ValueBinding* pBinding = pProcSign->params[uParamIndex].pBinding;
            const TypeInfo* pType = pBinding->pType;
            u8 uFormat = get_ir_format(pType);
            u32 uAlign = get_log2_of_align_bytes(pType);
            u32 uSlotsCount = get_slots_count(pType);
            u32 uIRPos = ir_emit_local_variable_decl(uFormat, uAlign, uSlotsCount, 0u, &(pProcBody->procwiseRepo), pEvalContext);
            Assert_(u8(pBinding->uScopeAndLocation) == EScopeKind::SCOPEKIND_PROC_PARAM);
            Assert_((pBinding->uScopeAndLocation >> 8) == pProcBody->vecBindings.size());
            pProcBody->vecBindings.append(pBinding);
            // TODO: default values for retparams ?
        }
    } else {
        // emitting single noop as first IR in case of no-param, so that we always have an IR pos 0 which is not a valid jump target...
        ir_emit_entry(&(pProcBody->procwiseRepo), IRIT_NO_OP, 0u, 0u, 0uLL, 0uLL, 0u, MetaValueIR{}, pEvalContext->pWorker);
    }
}

local_func ETCResult typecheck_cmp_binary_op(TmpTCNode* pExpr, u8 uOp, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    // TODO !!
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "typecheck_cmp_binary_op() : not yet implemented");
}

// At statement level, typechecks nodes of the op-and-assignment kind
local_func ETCResult typecheck_op_and_assignment_statement(TmpTCNode* pMainNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    u8 uOpAndAssign = u8(pMainNode->pTCNode->ast.uNodeKindAndFlags >> 8);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Main Node of an Op-And-Assignment Statement (token: %s )",
        reinterpret_cast<u64>(tStandardPayloadsStr[uOpAndAssign])), pTCContext->pWorker);

    if (LIKELY(pTCContext->uFlags & CTXFLAG_ALLOW_RUNTIME)) {
        Assert_(pTCContext->pProcResult);
    } else {
        return_error(pMainNode, pTCStatement, pTCContext, CERR_SEQ_STATEMENT_WHEN_EXPECTING_NON_SEQ_STATEMENT,
            "typecheck_op_and_assignment_statement() : not available outside of sequential context");
    }

    u8 uOp;
    switch (uOpAndAssign) {
        case ETOK_ADD_ASSIGN: uOp = ETOK_PLUS; break;
        case ETOK_SUB_ASSIGN: uOp = ETOK_MINUS; break;
        case ETOK_MUL_ASSIGN: uOp = ETOK_STAR; break;
        case ETOK_DIV_ASSIGN: uOp = ETOK_SLASH; break;
        case ETOK_MOD_ASSIGN: uOp = ETOK_MODULUS; break;
        case ETOK_MODULO_ADD_ASSIGN: uOp = ETOK_MODULO_ADD; break;
        case ETOK_MODULO_SUB_ASSIGN: uOp = ETOK_MODULO_SUB; break;
        case ETOK_MODULO_MUL_ASSIGN: uOp = ETOK_MODULO_MUL; break;
        case ETOK_INT_QUO_ASSIGN: uOp = ETOK_INT_QUOTIENT; break;
        case ETOK_INT_REM_ASSIGN: uOp = ETOK_INT_REMAINDER; break;
        case ETOK_CONCAT_ASSIGN: uOp = ETOK_CONCAT; break;
        case ETOK_BIT_AND_ASSIGN: uOp = ETOK_BIT_AND; break;
        case ETOK_BIT_OR_ASSIGN: uOp = ETOK_BIT_OR; break;
        case ETOK_BIT_XOR_ASSIGN: uOp = ETOK_BIT_XOR; break;
        case ETOK_LSH_ASSIGN: uOp = ETOK_LEFT_SHIFT; break;
        case ETOK_RSH_ASSIGN: uOp = ETOK_RIGHT_SHIFT; break;
        case ETOK_POW_ASSIGN: uOp = ETOK_POW; break;
        default: {
            Assert_(false);
            return_error(pMainNode, pTCStatement, pTCContext, FERR_UNREACHABLE,
                "typecheck_op_and_assignment_statement() : unexpected op-and-assign kind");
        }
    }

    TmpTCNode destAndOpA = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Now typechecking as Dest&OperandA of the Op-And-Assignment Statement (token: %s )",
            reinterpret_cast<u64>(tStandardPayloadsStr[uOpAndAssign])), pTCContext->pWorker);
        ETCResult checkDestAndA = typecheck_expression(&destAndOpA, pTCStatement, pTCContext,
            EExpectedExpr::EXPECT_ASSIGNABLE, UpwardsInference{});
        success_or_return_wait_or_error(checkDestAndA, pMainNode->pTCNode);
        Assert_(is_node_already_typechecked(destAndOpA.pTCNode));
        if (destAndOpA.pIntrinsicValue->pType == 0) { // marker for sink, which can be nominally returned when typechecking assignable... But forbidden here.
            return_error(pMainNode, pTCStatement, pTCContext, CERR_CANNOT_USE_SINK_AS_LHV_OF_OP_AND_ASSIGN_STATEMENT,
                "typecheck_op_and_assignment_statement() : sink used as LHV of op-and-assign statement");
        } else {
            // NOOP
            Assert(is_value_tc_assignable(destAndOpA.pIntrinsicValue),
                "non-sink node having passed an 'assignable'-expected TC should be assignable");
        }
    }

    TmpTCNode operandB = init_tmp_tc_node(pMainNode->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Now typechecking OperandB of the Op-And-Assignment Statement (token: %s )",
            reinterpret_cast<u64>(tStandardPayloadsStr[uOpAndAssign])), pTCContext->pWorker);
        ETCResult checkOperandB = typecheck_expression(&operandB, pTCStatement, pTCContext,
            EExpectedExpr::EXPECT_REGULAR, infer_type(destAndOpA.pIntrinsicValue->pType));
        success_or_return_wait_or_error(checkOperandB, pMainNode->pTCNode);
        Assert_(is_node_already_typechecked(operandB.pTCNode));
    }

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Now invoking regular binop between Dest and B for Op-And-Assignment (token: %s as binop: %s)",
            reinterpret_cast<u64>(tStandardPayloadsStr[uOpAndAssign]), reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
        ETCResult checkOp = typecheck_regular_binary_op_dispatch(pMainNode, uOp, &destAndOpA, &operandB,
            pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR);
        success_or_return_wait_or_error(checkOp, pMainNode->pTCNode);
    }

    Assert_(!is_value_pseudo_valued_cond(pMainNode->pIntrinsicValue));
    do_store_value_to(destAndOpA.pIntrinsicValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK,
        pMainNode->pIntrinsicValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK, get_ir_format(destAndOpA.pIntrinsicValue->pType),
        get_slots_count(destAndOpA.pIntrinsicValue->pType), pTCStatement, pTCContext);
    return set_node_typecheck_expr_success(pMainNode->pTCNode);
}

local_func ETCResult typecheck_regular_binary_op(TmpTCNode* pExpr, u8 uOp, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    if (eExpectation > EExpectedExpr::EXPECT_REGULAR) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
            "typecheck_regular_binary_op() : cannot assign to result of regular binop");
    }

    TmpTCNode operandA = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Now typechecking as OperandA of Binary Op (token: %s )",
            reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
        ETCResult checkOpA = typecheck_expression(&operandA, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
        success_or_return_wait_or_error(checkOpA, pExpr->pTCNode);
    }

    TmpTCNode operandB = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Now typechecking as OperandB of Binary Op (token: %s )",
            reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
        ETCResult checkOpB = typecheck_expression(&operandB, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
        success_or_return_wait_or_error(checkOpB, pExpr->pTCNode);
    }

    ETCResult checkResult = typecheck_regular_binary_op_dispatch(pExpr, uOp, &operandA, &operandB,
        pTCStatement, pTCContext, eExpectation);

    Assert_(checkResult != ETCResult::ETCR_SUCCESS || is_node_already_typechecked(pExpr->pTCNode));
    Assert_(checkResult != ETCResult::ETCR_SUCCESS || is_value_tc_const(pExpr->pIntrinsicValue) || eExpectation != EExpectedExpr::EXPECT_CONSTANT);
    return checkResult;
}

local_func ETCResult typecheck_regular_unary_op(TmpTCNode* pExpr, u8 uOp, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking unary-op (token: %s ) against its parameter",
        reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);

    if (eExpectation > EExpectedExpr::EXPECT_REGULAR) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
            "typecheck_regular_unary_op() : cannot assign to result of regular unop");
    }

    TmpTCNode operand = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    ETCResult checkOp = typecheck_expression(&operand, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
    success_or_return_wait_or_error(checkOp, pExpr->pTCNode);

    Assert_(INVALID_NODE_INDEX == pExpr->pTCNode->ast.uSecondaryChildNodeIndex);

    ETCResult checkResult = typecheck_regular_unary_op_dispatch(pExpr, uOp, &operand, pTCStatement, pTCContext, eExpectation);

    Assert_(checkResult != ETCResult::ETCR_SUCCESS || is_node_already_typechecked(pExpr->pTCNode));
    Assert_(checkResult != ETCResult::ETCR_SUCCESS || is_value_tc_const(pExpr->pIntrinsicValue) || eExpectation != EExpectedExpr::EXPECT_CONSTANT);
    return checkResult;
}


local_func ETCResult typecheck_reserved_word(TmpTCNode* pExpr, int iIdentifierHandle,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(!is_node_already_typechecked(pExpr->pTCNode));
    Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ATOMICEXPR_IDENTIFIER);
    Assert_(iIdentifierHandle = int(pExpr->pTCNode->ast.uPrimaryPayload));
    Assert_(iIdentifierHandle >= 0);
    Assert_(iIdentifierHandle != ERES_INVALID_ID);
    Assert_(iIdentifierHandle < COUNT_RESERVED_WORDS);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Identifier: known Reserved Word"), pTCContext->pWorker);

    if (is_comptime_prefixed(&(pExpr->pTCNode->ast))) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_COMPTIME_PREFIX_THIS_NODE_KIND,
            "typecheck_any_non_invoc_expression() : comptime-prefix not allowed on reserved words or sink");
    }

    if (    (iIdentifierHandle != ERES_SINK && eExpectation <= EExpectedExpr::EXPECT_REGULAR) ||
            (iIdentifierHandle == ERES_SINK && eExpectation >= EExpectedExpr::EXPECT_ASSIGNABLE) ) {

        u32 uIndex = pTCStatement->vecNodeValues.size();
        NodeValue* pReservedWordVal = g_tReservedWordValues + iIdentifierHandle;
        pTCStatement->vecNodeValues.append(pReservedWordVal);
        pExpr->pIntrinsicValue = pReservedWordVal;
        pExpr->pTCNode->uIntrinsicValueIndex = uIndex;
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else {
        if (iIdentifierHandle == ERES_SINK) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_EVALUATE_SINK,
                "typecheck_any_non_invoc_expression() : trying to evaluate sink");
        } else if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_ASSIGN_TO_RESERVED_WORD,
                "typecheck_any_non_invoc_expression() : reserved word when expecting assignable");
        } else {
            Assert_(eExpectation == EExpectedExpr::EXPECT_DECLARABLE);
            return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_DECLARE_RESERVED_WORD,
                "typecheck_any_non_invoc_expression() : reserved word when expecting declarable");
        }
    }
}

local_func ETCResult typecheck_user_specified_identifier(TmpTCNode* pExpr, int iIdentifierHandle,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(eExpectation != EExpectedExpr::EXPECT_DECLARABLE); // declarables should *not* take this path
    Assert_(!is_node_already_typechecked(pExpr->pTCNode));
    Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ATOMICEXPR_IDENTIFIER);
    Assert_(iIdentifierHandle = int(pExpr->pTCNode->ast.uPrimaryPayload));
    Assert_(iIdentifierHandle >= COUNT_RESERVED_WORDS);

    ValueBinding* pBinding = tc_find_binding_from_identifier(pTCContext, iIdentifierHandle, EIdentAttributes::EIDENTATTR_REGULAR);
    if (pBinding != 0) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking User Identifier: known Binding"), pTCContext->pWorker);

        if (pBinding->pType == 0) { // marker for a global binding-in-error
            return_error(pExpr, pTCStatement, pTCContext, CERR_GLOBAL_BINDING_ON_STATEMENT_WITH_ERROR,
                "typecheck_user_specified_identifier() : found identifier, but its binding statement is reported as in error");
        }

        if (!is_value_tc_only(pBinding)) {
            Assert_(ir_is_valid_param_(pBinding->info.uIRandMetaFlags));
            Assert_(!ir_is_immediate(pBinding->info.uIRandMetaFlags));
            IRRepo* pBindingRepo;
            u32 uBindingIndex;
            SourceFileDescAndState* pBindingFile;
            EEntryKind eBindingKind;
            ir_decode_non_imm(pBinding->info.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                pTCContext, &pBindingRepo, &uBindingIndex, &pBindingFile, &eBindingKind);
            if (pTCContext->pProcResult && eBindingKind == EEK_CURRENT_PROC_LOCAL &&
                uBindingIndex < get_input_param_count(pTCContext->pProcResult->procSign)) {
                if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_PROC_PARAM_AS_LVALUE,
                        "typecheck_any_non_invoc_expression() : cannot assign to input param");
                }
                Assert_(!irflag_is_tc_referencable(pBinding->info.uIRandMetaFlags));
            } else {
                Assert_(irflag_is_tc_referencable(pBinding->info.uIRandMetaFlags));
            }
        }
        if (is_value_tc_const(pBinding)) {
            if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_CONSTANT_AS_LVALUE,
                    "typecheck_any_non_invoc_expression() : found binding to constant when expecting assignable");
            }
        } else {
            // This is a runtime binding. The metavalue stays 0, and only a small subset of all possible flags are expected on the value
            Assert_(0 == (u32(pBinding->info.uIRandMetaFlags) & IRFLAGS_IR_SPECIFIC_MASK)); // otherwise could mean we unexpectedly assigned some of
                                                                                            // the 'initial value' flags to the actual value...
            Assert_(pBinding->info.metaValue._payload == 0uLL);
            if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                    "typecheck_any_non_invoc_expression() : found binding to variable when expecting constant");
            }
        }

        u32 uPosInVecValues = pTCStatement->vecNodeValues.size();
        pTCStatement->vecNodeValues.append(pBinding);
        pExpr->pTCNode->uIntrinsicValueIndex = uPosInVecValues;
        pExpr->pIntrinsicValue = pBinding;
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
            "Putting TC task for statement on hold, waiting for global identifier '%s'",
            reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str())), pTCContext->pWorker);

        return add_waiting_task_for_identifier(iIdentifierHandle, pTCContext->pNamespace, false,
            pExpr->uNodeIndexInStatement, pTCContext);
    }
}

local_func ETCResult typecheck_atomic_literal(TmpTCNode* pExpr, u8 uNodeKind, TCStatement* pTCStatement, TCContext* pTCContext)
{
    const TypeInfo* pType = 0;
    u32 uConstFlags = IRFLAG_IS_KNOWN|IRFLAG_TC_SEMANTIC_CONST;
    if (pExpr->pTCNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_IS_LITERAL_EMBEDDED64)
        uConstFlags |= IRFLAG_IS_KNOWN_EMBD;

    u64 uPayload = pExpr->pTCNode->ast.uLiteralPayload64;

    if (uNodeKind == ENodeKind::ENODE_ATOMICEXPR_NATURAL_NUMBER_LITERAL) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Atomic: NatNum Literal"), pTCContext->pWorker);

        NodeValue* pResult = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResult->pType = g_pCoreTypesInfo[ECoreType::ECORETYPE_COMPINT];
        pResult->info.uIRandMetaFlags = uConstFlags|IRFLAG_TC_ONLY;
        Assert_(uConstFlags & IRFLAG_IS_KNOWN_EMBD);
        pResult->info.metaValue.knownValue.uEmbeddedValue = uPayload;
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else if (uNodeKind == ENodeKind::ENODE_ATOMICEXPR_FLOATING_POINT_LITERAL) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Atomic: FP Literal"), pTCContext->pWorker);

        NodeValue* pResult = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResult->pType = g_pCoreTypesInfo[ECoreType::ECORETYPE_FLOAT_LIT];
        Assert_(uConstFlags & IRFLAG_IS_KNOWN_EMBD); // since atm only f64: later may use XFloat representation. TODO !
        u32 uPos = ir_make_decl_entry(pTCContext->pRepo, 0u, uConstFlags, uPayload, 0x08u|0x03u, 8u, 3u);
        pResult->info.uIRandMetaFlags = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos) | uConstFlags;
        pResult->info.metaValue.knownValue._payload = uPayload;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else if (uNodeKind == ENodeKind::ENODE_ATOMICEXPR_STRING_LITERAL) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Atomic: String Literal"), pTCContext->pWorker);

        NodeValue* pResult = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResult->pType = g_pCoreTypesInfo[ECoreType::ECORETYPE_COMPACT_STRING];
        FFString asFFString { reinterpret_cast<u8*>(uPayload) };
        u64 uBackingArrayPayload = reinterpret_cast<u64>(asFFString._as_ff_start()); 
        u32 uPosBackingArray = ir_make_decl_entry(pTCContext->pRepo, 0u, IRFLAG_IS_KNOWN|IRFLAG_TC_SEMANTIC_CONST,
            uBackingArrayPayload, 0x02u, asFFString._as_ff_dword_count() * 4u, 2u);
        u64 uBackingArrayIR = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosBackingArray);
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPosBackingArray;
        IRInfo infoPtrToBackingArray = ir_make_info_for_nyka_immediate_to(uBackingArrayIR);
        infoPtrToBackingArray.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
        IRInfo infoTwoU32sIndex = ir_make_info_for_int_immediate(2, 0x02u);
        EIRResult eSolveStringAsPtrOffset = ir_emit_or_solve_ptr_offset(2u, infoPtrToBackingArray, 0x02u, infoTwoU32sIndex, 4u, 0u,
            EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &(pResult->info));
        Assert_(eSolveStringAsPtrOffset <= EIRResult::EIRR_ENSURED_VALID_KNOWN);
        pResult->info.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else if (uNodeKind == ENodeKind::ENODE_ATOMICEXPR_CODEPOINT_LITERAL) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Atomic: Codepoint Literal"), pTCContext->pWorker);

        NodeValue* pResult = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResult->pType = g_pCoreTypesInfo[ECoreType::ECORETYPE_CODEPOINT];
        Assert_(uConstFlags & IRFLAG_IS_KNOWN_EMBD);
        Assert_(uPayload <= UNICODE_CODEPOINT_MAX);
        pResult->info = ir_make_info_for_int_immediate(i32(uPayload), 0x02u);
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else {
        return_error(pExpr, pTCStatement, pTCContext, FERR_OTHER,
            "typecheck_atomic_literal() : unexpected literal kind");
    }
}

/*
local_func ETCResult add_declarable(TmpTCNode* pExpr, int iIdentifierHandle, TCStatement* pTCStatement,
    TCContext* pTCContext, bool bIsConstDecl)
{
    EDeclAttributes declAttr = bIsConstDecl ? EDeclAttributes::EDECLATTR_REGULAR_CONST : EDeclAttributes::EDECLATTR_REGULAR_VAR;
    bool bDeclareLocally = does_tc_ctx_require_declare_as_local(pTCContext, declAttr);
    bool bAllowShadowingOfLocals = bDeclareLocally ? does_tc_ctx_allow_shadowing_locals(pTCContext, declAttr) : false;
    bool bAllowShadowingOfGlobals = does_tc_ctx_allow_shadowing_globals(pTCContext, declAttr);
    const TypeInfo_ProcLike* pProcSign = is_ctx_with_proc_source(pTCContext) ? pTCContext->pProcSource->procSign : 0;
    SourceFileDescAndState* pGlobalDeclFile = get_tc_global_declaration_file(pTCContext, declAttr);
    u64 uHash = get_map_hash(iIdentifierHandle);

    if (bDeclareLocally) {
        Assert_(has_ctx_decl_blocks(pTCContext));
        TCDeclSourceBlock* pCurrentBlock = (TCDeclSourceBlock*)pTCContext->pCurrentBlock;
        do {
            TmpMap<int, u32>& mapLocalDeclsById = pCurrentBlock->mapBlockDeclarationsById;
            auto itFoundLocal = mapLocalDeclsById.findHashed(uHash, iIdentifierHandle);
            if (itFoundLocal != mapLocalDeclsById.end()) {
                emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                    "typecheck_declaration_statement() : already declared identifier (at local scope)");
                return set_node_tc_error(pMainNode->pTCNode, 0);
            }
            pCurrentBlock = bAllowShadowingOfLocals ? 0 : (TCDeclSourceBlock*)pCurrentBlock->pParentBlock;
        } while (pCurrentBlock);

        if (pProcSign) {
            u8 uTotalCount = get_total_param_count(pProcSign);
            for (u8 uParam = 0; uParam < uTotalCount; uParam++) {
                const ProcLikeParam* pParam = pProcSign->params.cat(uParam);
                if (iIdentifierHandle == pParam->iIdentifier) {
                    emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                        "typecheck_declaration_statement() : already declared identifier (as proc param)");
                    return set_node_tc_error(pMainNode->pTCNode, 0);
                }
            }
        }
    }

    if (!bAllowShadowingOfGlobals || !bDeclareLocally) {
        TmpMap<int, u32>& mapGlobalDeclsById = pGlobalDeclFile->mapAllGlobalDeclarationsById;
        auto itFoundGlobal = mapGlobalDeclsById.findHashed(uHash, iIdentifierHandle);
        if (itFoundGlobal != mapGlobalDeclsById.end()) {
            emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                "typecheck_declaration_statement() : already declared identifier (at global scope)");
            return set_node_tc_error(pMainNode->pTCNode, 0);
        }
        if (bDeclareLocally)
            pGlobalDeclFile->setLocalUnshadowing.insert(iIdentifierHandle);
    }

    ValueBinding* pNewBinding = alloc_from(pTCContext->pIsolatedSourceFile->localArena, sizeof(ValueBinding), alignof(ValueBinding));
    *pNewBinding = ValueBinding{};
    pNewBinding->info.uIRandMetaFlags = IRFLAG_TC_BINDING_INSTANCE;
    if (bIsConstDecl)
        pNewBinding->info.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_CONST;
    pNewBinding->iIdentifierHandle = iIdentifierHandle;
    set_binding_source_ref(pNewBinding, pTCStatement, pTCContext, declAttr);
    if (bDeclareLocally) {
        Assert_(pTCContext->pProcResult);
        u32 uNewBindingIRPos = pTCContext->pProcResult->procwiseRepo.uSize;
        IREntry* pNewDeclEntry = ir_append_new_entry(&(pTCContext->pProcResult->procwiseRepo)); // will not yet be initialized ; will init when found type & init value
        pNewBinding->info.uIRandMetaFlags |= ir_make_std_code_in_cur_proc(uNewBindingIRPos);
        pNewBinding->uScopeAndLocation = (uNewBindingIRPos << 8) | EScopeKind::SCOPEKIND_PROC_BLOCK_LOCAL;
        pTCContext->pProcResult->mapBindingsPerIRPos.insert(uNewBindingIRPos, pNewBinding);
        TCDeclSourceBlock* pCurrentBlock = (TCDeclSourceBlock*)pTCContext->pCurrentBlock;
        pCurrentBlock->mapBlockDeclarationsById.insert_not_present(uHash, iIdentifierHandle, uNewBindingIRPos);
    } else {
        Assert_(pGlobalDeclFile);
        u32 uNewBindingIRPos = pGlobalDeclFile->filewiseRepo.uSize;
        IREntry* pNewDeclEntry = ir_append_new_entry(&(pGlobalDeclFile->filewiseRepo)); // will not yet be initialized ; will init when found type & init value
        pNewBinding->info.uIRandMetaFlags |= ir_make_std_code_in_file(u32(pGlobalDeclFile->iRegistrationIndex), uNewBindingIRPos);
        u32 uNewBindingVecPos = pGlobalDeclFile->vecAllGlobalBindings.size();
        pGlobalDeclFile->vecAllGlobalBindings.insert(pNewBinding);
        pNewBinding->uScopeAndLocation = (uNewBindingVecPos << 8) | pTCContext->eGlobalDeclScope;
        pGlobalDeclFile->mapGlobalBindingsByIRPos.insert(uNewBindingIRPos, uNewBindingVecPos);
        pGlobalDeclFile->mapAllGlobalDeclarationsById.insert_not_present(uHash, iIdentifierHandle, uNewBindingVecPos);
        if (pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PUBLIC)
            pGlobalDeclFile->mapPublicGlobalDeclarationsById.insert_not_present(uHash, iIdentifierHandle, uNewBindingVecPos);
        else if (pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PACKAGE)
            pGlobalDeclFile->mapPackageGlobalDeclarationsById.insert_not_present(uHash, iIdentifierHandle, uNewBindingVecPos);
    }

    u32 uValuePos = pTCStatement->vecNodeValues.size();
    pTCStatement->vecNodeValues.append(pNewBinding);
    pExpr->pIntrinsicValue = pNewBinding;
    pExpr->pTCNode->uIntrinsicValueIndex = uValuePos;
    return set_node_typecheck_expr_success(pExpr->pTCNode);
}
*/


local_func ETCResult typecheck_dot_descent_expression_against_type(TmpTCNode* pExpr, const TypeInfo* pLeftHandType,
    int iRightHandIdentifier, TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Typechecking dot-descent operator against a type"), pTCContext->pWorker);

    if (iRightHandIdentifier < COUNT_RESERVED_WORDS) {
        if (iRightHandIdentifier != ERES_SINK) {
            NodeValue* pReservedWordVal = g_tReservedWordValues + iRightHandIdentifier;
            if (pReservedWordVal->pType == g_pCoreTypesInfo[ECORETYPE_BUILTIN]) {
                Assert_(is_value_tc_only(pReservedWordVal));
                u64 uBaseOfCastBuiltin = pReservedWordVal->info.metaValue.knownValue.uEmbeddedValue;
                switch (uBaseOfCastBuiltin) {
                    case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_TRANSMUTE_CAST:
                    case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_TRUNCATING_CAST:
                    case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_SATURATING_CAST:
                    case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_POINTER_CAST:
                        break;
                    default:
                        return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_DOT_DESCENT,
                            "typecheck_dot_descent_expression_against_type() : this builtin does not qualify as a valid dot-descent followup for a type");
                }
                u16 uErr = 0;
                if (!is_allowed_as_runtime_type(pLeftHandType, pTCContext, &uErr)) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_DOT_DESCENT,
                        "typecheck_dot_descent_expression_against_type() : cannot use special cast builtin against a comptime-only type");
                }
                u64 uLeftHandTypeAsR64 = reinterpret_cast<u64>(pLeftHandType);
                NodeValue* pNewValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                pNewValue->pType = g_pCoreTypesInfo[ECORETYPE_BUILTIN];
                pNewValue->info.uIRandMetaFlags = IRFLAG_TC_SEMANTIC_CONST|IRFLAG_TC_ONLY;
                Assert_(0uLL == (uLeftHandTypeAsR64 & 0x0FuLL));
                Assert_(uBaseOfCastBuiltin && 0uLL == (uBaseOfCastBuiltin & (~u64(0x0FuLL))));
                pNewValue->info.metaValue.knownValue.uEmbeddedValue = uLeftHandTypeAsR64 | uBaseOfCastBuiltin;
                return set_node_typecheck_expr_success(pExpr->pTCNode);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_DOT_DESCENT,
                    "typecheck_dot_descent_expression_against_type() : this reserved word does not apply as a valid dot-descent followup for a type");
            }
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_DOT_DESCENT,
                "typecheck_dot_descent_expression_against_type() : sink is not allowed as the right-hand side of dot-descent");
        }
    }

    if (get_type_kind(pLeftHandType) == ETYPEKIND_ENUM) {

        const TypeInfo_Enum* pAsEnum = (const TypeInfo_Enum*)pLeftHandType;
        check_compound_type_full_availability_may_return_wait_or_error(pAsEnum, pExpr, pTCStatement, pTCContext,
            "typecheck_dot_descent_expression_against_type() : (enum) cannot dot-descent against");

        ValueBinding* pFoundBinding = find_binding_within_enum(iRightHandIdentifier, get_map_hash(iRightHandIdentifier), pAsEnum);
        if (pFoundBinding) {
            Assert_(is_value_tc_const(pFoundBinding));
            if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                    "cannot assign to an enum value");
            }
            set_existing_value_as(pFoundBinding, pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement);
            return set_node_typecheck_expr_success(pExpr->pTCNode);
        } else {
            return_error(pExpr, pTCStatement, pTCContext, RERR_UNRESOLVED_IDENTIFIER,
                "typecheck_dot_descent_expression_against_type() : dot-descent against enum type did not find this identifier");
        }
    
    } else if (get_type_kind(pLeftHandType) == ETYPEKIND_STRUCTLIKE) {

        const TypeInfo_StructLike* pAsStructLike = (const TypeInfo_StructLike*)pLeftHandType;
        check_compound_type_full_availability_may_return_wait_or_error(pAsStructLike, pExpr, pTCStatement, pTCContext,
            "typecheck_dot_descent_expression_against_type() : (structlike) cannot dot-descent against");

        auto itFound = pAsStructLike->mapAllMembers.find(iRightHandIdentifier);
        if (itFound != pAsStructLike->mapAllMembers.end()) {
            if (itFound.value() >= pAsStructLike->uRuntimeMemberCount) {
                ValueBinding* pBinding = pAsStructLike->vecAllMembers[itFound.value()];
                Assert_(is_value_tc_const(pBinding));
                if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                        "cannot assign to a const declared within struct or enum");
                }
                set_existing_value_as(pBinding, pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement);
                return set_node_typecheck_expr_success(pExpr->pTCNode);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, RERR_UNRESOLVED_IDENTIFIER,
                    "typecheck_dot_descent_expression_against_type() : dot-descent against struct or union *type* cannot access runtime members. Use an instance instead");
            }
        } else {
            return_error(pExpr, pTCStatement, pTCContext, RERR_UNRESOLVED_IDENTIFIER,
                "typecheck_dot_descent_expression_against_type() : dot-descent against enum type did not find this identifier");
        }

    } else {
        return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_DOT_DESCENT,
            "typecheck_dot_descent_expression_against_type() : this type does not-expect dot-descents (other than special casting builtins)");
    }
}

local_func ETCResult typecheck_dot_descent_expression_against_string_related_instance(TmpTCNode* pExpr, TmpTCNode* pLeftHandExpr,
    const TypeInfo_OtherCore* pStringType, int iRightHandIdentifier, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation)
{
    Assert_(!is_node_already_typechecked(pExpr->pTCNode));
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Typechecking dot-descent operator against string-related instance"), pTCContext->pWorker);

    u32 uIsUserReferencableBase = pLeftHandExpr->pIntrinsicValue->info.uIRandMetaFlags & IRFLAG_TC_REFERENCABLE;
    u32 uIsSemanticConstBase = pLeftHandExpr->pIntrinsicValue->info.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST;
    
    NodeValue* pResultValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);

    u32 uIsAssignableFlag = 0u;
    if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
        if (0u == uIsUserReferencableBase) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_DOT_DECENT_NON_REFERENCABLE_AS_LVALUE,
                "typecheck_dot_descent_expression_against_string_related_instance() : base is non referencable => cannot be assignable");
        }
        if (uIsSemanticConstBase) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_CONSTANT_AS_LVALUE,
                "typecheck_dot_descent_expression_against_string_related_instance() : base is constant => cannot be assignable");
        }
        uIsAssignableFlag = IR_INSTRFLAG_IS_ASSIGNABLE;
    }

    switch (iRightHandIdentifier) {

        case EIMPLICIT_MEMBER_PTR_TO_BYTES: {
            pResultValue->pType = get_pointer_type_to(g_pCoreTypesInfo[ECORETYPE_R8], pTCContext);
            ETCResult eGatherInfo = tc_gather_string_info(pExpr, pLeftHandExpr->pIntrinsicValue->info, pStringType,
                pTCStatement, pTCContext, uIsAssignableFlag , &(pResultValue->info), 0u, 0u, 0u, 0u);
            success_or_return(eGatherInfo);
        } break;

        case EIMPLICIT_MEMBER_LENGTH_IN_BYTES: {
            pResultValue->pType = g_pCoreTypesInfo[ECORETYPE_U32];
            ETCResult eGatherInfo = tc_gather_string_info(pExpr, pLeftHandExpr->pIntrinsicValue->info, pStringType,
                pTCStatement, pTCContext, uIsAssignableFlag , 0u, &(pResultValue->info), 0u, 0u, 0u);
            success_or_return(eGatherInfo);
        } break;

        case EIMPLICIT_MEMBER_FLAGS: {
            pResultValue->pType = g_pCoreTypesInfo[ECORETYPE_U32];
            ETCResult eGatherInfo = tc_gather_string_info(pExpr, pLeftHandExpr->pIntrinsicValue->info, pStringType,
                pTCStatement, pTCContext, uIsAssignableFlag , 0u, 0u, &(pResultValue->info), 0u, 0u);
            success_or_return(eGatherInfo);
        } break;

        case EIMPLICIT_MEMBER_BYTES: {
            if (uIsAssignableFlag) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_TEMPORARY_AS_LVALUE,
                    "typecheck_dot_descent_expression_against_string_related_instance() : cannot assign to the 'bytes' pseudo-member");
            }
            pResultValue->pType = get_array_type_to(g_pCoreTypesInfo[ECORETYPE_R8], u32(ARRAY_TYPE_KIND_SLICE << 28u), pTCContext);
            IRInfo infoPtrToData;
            IRInfo infoByteLength;
            ETCResult eGatherInfo = tc_gather_string_info(pExpr, pLeftHandExpr->pIntrinsicValue->info, pStringType,
                pTCStatement, pTCContext, 0u, &infoPtrToData, &infoByteLength, 0u, 0u, 0u);
            success_or_return(eGatherInfo);

            if (irflag_is_tc_const(pLeftHandExpr->pIntrinsicValue->info.uIRandMetaFlags)) {

                Assert_(irflag_is_known_or_nyka(infoPtrToData.uIRandMetaFlags));
                Assert_(irflag_is_or_has_nyka(infoPtrToData.uIRandMetaFlags)); // TODO: allow nullptr as const ?
                Assert_(irflag_is_known_non_nyka(infoByteLength.uIRandMetaFlags));
                u8* pAllocTableAndData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, 32u, 8u);
                u32* pNykaTable = reinterpret_cast<u32*>(pAllocTableAndData);
                pNykaTable[0] = 1u; // Nyka count
                pNykaTable[1] = 0u; // Offset of single nyka
                u8** pPtrToData = reinterpret_cast<u8**>(pAllocTableAndData + 8u);
                u8* pRawDataStart = pAllocTableAndData + 16u;
                *pPtrToData = pRawDataStart;
                Assert_(irflag_is_known_embd(infoPtrToData.uIRandMetaFlags)); // TODO: allow nullptr as const ?
                *(reinterpret_cast<u64*>(pRawDataStart)) = infoPtrToData.metaValue.knownValue.uEmbeddedValue;
                Assert_(irflag_is_known_embd(infoByteLength.uIRandMetaFlags));
                *(reinterpret_cast<u64*>(pRawDataStart + 8u)) = infoByteLength.metaValue.knownValue.uEmbeddedValue; // Note: u32 to u64 here
                constexpr u32 uConstSliceFlags = IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_TC_SEMANTIC_CONST;
                u32 uPos = ir_make_decl_entry(pTCContext->pRepo, 0u, uConstSliceFlags,
                    reinterpret_cast<u64>(pAllocTableAndData), 0x03u, 16u, 3u);
                if (pTCContext->pProcResult)
                    pTCStatement->uLastIRorGlobalTCResult = uPos;
                pResultValue->info.uIRandMetaFlags = u64(uConstSliceFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
                pResultValue->info.metaValue.knownValue.pPtrToRawData = pAllocTableAndData;

            } else {

                Assert_(pTCContext->pProcResult);
                u32 uPos = ir_emit_local_variable_decl(0x03u, 3u, 2u, IR_INSTRFLAG_IS_ASSIGNABLE, pTCContext->pRepo, pTCContext);
                u64 uIRofDecl = ir_make_std_code_in_cur_proc(uPos);
                IRInfo infoDecl { uIRofDecl, meta_from_payload(0uLL) };

                IRInfo infoDestAddressOfPtrToData;
                IRInfo infoDestCount;
                ETCResult eGatherDestInfo = tc_gather_array_info(pExpr, infoDecl, (const TypeInfo_Array*)pResultValue->pType,
                    pTCStatement, pTCContext, IR_INSTRFLAG_IS_ASSIGNABLE, &infoDestAddressOfPtrToData, &infoDestCount, 0, 0, 0);
                success_or_return(eGatherDestInfo);
                IRInfo infoCount64;
                EIRResult eWidenByteLength32AsSliceCount64 = ir_emit_or_solve_truncate_or_extend_integral_to_integral(0x02u, 
                    infoByteLength, EIntSemantics::EINT_SEMANTIC_UNSIGNED, (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_U64],
                    pTCStatement, pTCContext, &infoCount64);
                if (eWidenByteLength32AsSliceCount64 < EIRResult::EIRR_FIRST_ERROR) {
                    do_store_value_to(infoDestAddressOfPtrToData.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoPtrToData.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x03u, 1u, pTCStatement, pTCContext);
                    do_store_value_to(infoDestCount.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoCount64.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x03u, 1u, pTCStatement, pTCContext);
                } else {
                    return_error(pExpr, pTCStatement, pTCContext, u16(eWidenByteLength32AsSliceCount64),
                        "typecheck_dot_descent_expression_against_string_related_instance() : widen byte length to slice count failed");
                }
                pResultValue->info.uIRandMetaFlags = uIRofDecl;
                pResultValue->info.metaValue._payload = 0uLL;
            }

            // skipping consolidation:
            return set_node_typecheck_expr_success(pExpr->pTCNode);

        } break;

        case EIMPLICIT_MEMBER_ALLOC: {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_dot_descent_expression_against_string_related_instance() : 'alloc' pseudo-member not yet implemented");
        } break;

        default:
            return_error(pExpr, pTCStatement, pTCContext, CERR_UNKOWN_MEMBER_IN_STRUCT,
                "typecheck_dot_descent_expression_against_string_related_instance() : identifier is not part of string pseudo-members");
    }

    ETCResult eConsolidate = consolidate_tc_const_flag_on_info(&(pResultValue->info), pResultValue->pType, pExpr, pTCStatement, pTCContext);
    success_or_return(eConsolidate);
    if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
        if (!is_value_tc_const(pResultValue)) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                "typecheck_dot_descent_expression_against_string_related_instance() : result is not constant");
        }
    }
    return set_node_typecheck_expr_success(pExpr->pTCNode);
}

local_func ETCResult typecheck_dot_descent_expression_against_array_instance(TmpTCNode* pExpr, TmpTCNode* pLeftHandExpr,
    const TypeInfo_Array* pArrayType, int iRightHandIdentifier, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation)
{
    Assert_(!is_node_already_typechecked(pExpr->pTCNode));

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Typechecking dot-descent operator against array instance"), pTCContext->pWorker);

    u32 uIsUserReferencableBase = pLeftHandExpr->pIntrinsicValue->info.uIRandMetaFlags & IRFLAG_TC_REFERENCABLE;
    u32 uIsSemanticConstBase = pLeftHandExpr->pIntrinsicValue->info.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST;
    
    NodeValue* pResultValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);

    u32 uIsAssignableFlag = 0u;
    if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
        if (0u == uIsUserReferencableBase) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_DOT_DECENT_NON_REFERENCABLE_AS_LVALUE,
                "typecheck_dot_descent_expression_against_array_instance() : base is non referencable => cannot be assignable");
        }
        if (uIsSemanticConstBase) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_CONSTANT_AS_LVALUE,
                "typecheck_dot_descent_expression_against_array_instance() : base is constant => cannot be assignable");
        }
        uIsAssignableFlag = IR_INSTRFLAG_IS_ASSIGNABLE;
    }

    switch (iRightHandIdentifier) {

        case EIMPLICIT_MEMBER_DATA_PTR: {
            pResultValue->pType = get_pointer_type_to(pArrayType->pElementType, pTCContext);
            ETCResult eGatherInfo = tc_gather_array_info(pExpr, pLeftHandExpr->pIntrinsicValue->info, pArrayType,
                pTCStatement, pTCContext, uIsAssignableFlag, &(pResultValue->info), 0u, 0u, 0u, 0u);
            success_or_return(eGatherInfo);
        } break;

        case EIMPLICIT_MEMBER_SIZE: {
            u8 uCountFormat;
            ETCResult eGatherInfo = tc_gather_array_info(pExpr, pLeftHandExpr->pIntrinsicValue->info, pArrayType,
                pTCStatement, pTCContext, uIsAssignableFlag, 0u, &(pResultValue->info), &uCountFormat, 0u, 0u);
            success_or_return(eGatherInfo);
            Assert_(uCountFormat == 0x02u || uCountFormat == 0x03u);
            if (uCountFormat == 0x02u)
                pResultValue->pType = g_pCoreTypesInfo[ECORETYPE_U32];
            else
                pResultValue->pType = g_pCoreTypesInfo[ECORETYPE_U64];
        } break;

        case EIMPLICIT_MEMBER_ALLOC: {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_dot_descent_expression_against_array_instance() : 'alloc' pseudo-member not yet implemented");
        } break;

        default:
            return_error(pExpr, pTCStatement, pTCContext, CERR_UNKOWN_MEMBER_IN_STRUCT,
                "typecheck_dot_descent_expression_against_array_instance() : identifier is not part of array pseudo-members");
    }

    ETCResult eConsolidate = consolidate_tc_const_flag_on_info(&(pResultValue->info), pResultValue->pType, pExpr, pTCStatement, pTCContext);
    success_or_return(eConsolidate);
    if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
        if (!is_value_tc_const(pResultValue)) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                "typecheck_dot_descent_expression_against_array_instance() : result is not constant");
        }
    }
    return set_node_typecheck_expr_success(pExpr->pTCNode);
}

local_func ETCResult typecheck_dot_descent_expression_against_structlike_instance(TmpTCNode* pExpr, TmpTCNode* pLeftHandExpr,
    const TypeInfo_StructLike* pStructType, int iRightHandIdentifier, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Typechecking dot-descent operator against structlike instance"), pTCContext->pWorker);

    auto itTryAsMember = pStructType->mapAllMembers.find(iRightHandIdentifier);
    if (itTryAsMember != pStructType->mapAllMembers.end()) {
        u32 uMemberRegistration = itTryAsMember.value();

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Dot-Descent: against valid member of struct instance"), pTCContext->pWorker);

        IRInfo infoAddressOfBase;
        EIRResult eSolveAddressOfBase = ir_emit_or_solve_address_of(pLeftHandExpr->pIntrinsicValue->info, pTCStatement, pTCContext, &infoAddressOfBase);
        Assert_(eSolveAddressOfBase < EIRResult::EIRR_FIRST_ERROR);
        Assert_(irflag_is_known_or_nyka(infoAddressOfBase.uIRandMetaFlags));
        ValueBinding* pMemberBinding = pStructType->vecAllMembers[uMemberRegistration];
        Assert_(!is_value_known_or_nyka(pMemberBinding));
        Assert_(is_value_tc_binding_instance(pMemberBinding));

        if (uMemberRegistration < pStructType->uRuntimeMemberCount) {
            Assert_(!is_value_tc_const(pMemberBinding));
            if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                    "typecheck_dot_descent_expression_against_structlike_instance() : accessing member in struct or union is not a constant, when context expects constant");
            }
            u64 uIsUserReferencableBase = pLeftHandExpr->pIntrinsicValue->info.uIRandMetaFlags & IRFLAG_TC_REFERENCABLE;
            if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE && 0 == uIsUserReferencableBase) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                    "typecheck_dot_descent_expression_against_struct_instance() : non-user referencable cannot be assignable");
            }

            // the 'IR' slot for compounds was hacked after typechecking, to represent their byte offset from base.
            u32 uMemberByteOffset = u32(pMemberBinding->info.uIRandMetaFlags >> IR_STD_PARAM_SHIFT);
            Assert_(uMemberByteOffset < MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE);
            IRInfo infoByteOffset = ir_make_info_for_int_immediate(i32(uMemberByteOffset), 0x02u);
            IRInfo infoAddressOfMember;
            u32 uAlignLog2 = get_log2_of_align_bytes(pMemberBinding->pType);
            EIRResult eSolveAddressOfMember = ir_emit_or_solve_ptr_offset(uAlignLog2, infoAddressOfBase, 0x02u,
                infoByteOffset, 1u, IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EINT_SEMANTIC_UNSIGNED, pTCStatement, pTCContext, &infoAddressOfMember);
            Assert_(eSolveAddressOfMember < EIRResult::EIRR_FIRST_ERROR);
            Assert_(irflag_is_known_or_nyka(infoAddressOfMember.uIRandMetaFlags));

            IRInfo resultInfo;
            EIRResult eSolveAccessMember = ir_emit_or_solve_deref(infoAddressOfMember, get_ir_format(pMemberBinding->pType), uAlignLog2,
                get_slots_count(pMemberBinding->pType), get_runtime_sizeof(pMemberBinding->pType), eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE ?
                IR_INSTRFLAG_IS_ASSIGNABLE : 0u, pTCStatement, pTCContext, &resultInfo);
            Assert_(eSolveAccessMember < EIRResult::EIRR_FIRST_ERROR);
            if (is_value_tc_const(pLeftHandExpr->pIntrinsicValue) && irflag_is_known_or_nyka(resultInfo.uIRandMetaFlags) &&
                    0 == (resultInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA)) {
                // TODO: ensure of constness shall depend on type (eg for strings: depends on content)
                resultInfo.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
            }

            resultInfo.uIRandMetaFlags |= uIsUserReferencableBase;
            NodeValue* pResultValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pResultValue->pType = pMemberBinding->pType;
            pResultValue->info = resultInfo;

        } else {
            Assert_(is_value_tc_const(pMemberBinding));
            if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                    "typecheck_dot_descent_expression_against_struct_instance() : constant member or value cannot be assigned to");
            }
            set_existing_value_as(pMemberBinding, pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement);
        }

        return set_node_typecheck_expr_success(pExpr->pTCNode);
    }

    // Note: if we ever relax the requirement of full-typechecked body here, we'd need to 'wait' here instead of error
    //   same if we allow 'method' syntax for functions, and external-to-struct method declarations... (probably won't, if only for this reason...)
    return_error(pExpr, pTCStatement, pTCContext, CERR_UNKOWN_MEMBER_IN_STRUCT,
        "typecheck_dot_descent_expression_against_structlike_instance() : identifier is not part of struct members");
}

local_func ETCResult typecheck_dot_descent_expression_against_namespace(TmpTCNode* pExpr, TmpTCNode* pLeftHandExpr,
    TCNamespace* pNamespace, u64 bHasPackageAccess, int iRightHandIdentifier, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Typechecking dot-descent operator against namespace %u (in source file : '%s')",
        u64(pNamespace->uRegistrationIndex),
        reinterpret_cast<u64>(pNamespace->pOriginalSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    ValueBinding* pBinding = 0;
    if (pNamespace->pOriginalSourceFile == pTCContext->pIsolatedSourceFile) { // namespace is in *same* file

        pBinding = find_binding_within_namespace_in_same_file(iRightHandIdentifier, get_map_hash(iRightHandIdentifier),
            pNamespace, false, pTCContext);

        if (!pBinding) {
            if (is_local_namespace_fully_discovered(pNamespace, pTCContext)) {
                return_error(pExpr, pTCStatement, pTCContext, RERR_UNRESOLVED_IDENTIFIER,
                    "identifier not found in local, fully solved context");
            } else {
                return add_waiting_task_for_identifier(iRightHandIdentifier, pNamespace, true, pExpr->uNodeIndexInStatement, pTCContext);
            }
        }

    } else {

        if (is_non_local_namespace_fully_discovered_otherwise_return_locked_for_wait(pNamespace, pTCContext)) {

            pBinding = find_binding_within_namespace_in_other_file(iRightHandIdentifier, get_map_hash(iRightHandIdentifier),
                pNamespace, pTCContext);

            if (!pBinding) {
                return_error(pExpr, pTCStatement, pTCContext, RERR_UNRESOLVED_IDENTIFIER,
                    "identifier not found in context from other file");
            }

            // Should not return privates, if from another file...
            Assert_(u8(pBinding->uScopeAndLocation) <= EScopeKind::SCOPEKIND_GLOBAL_PACKAGE);

        } else {
            return add_waiting_task_for_already_event_locked_othersource_namespace(pNamespace, pExpr->uNodeIndexInStatement, pTCContext);
        }

    }

    Assert_(pBinding);

    if (pBinding->pType == 0) { // marker for a global binding-in-error
        return_error(pExpr, pTCStatement, pTCContext, CERR_GLOBAL_BINDING_ON_STATEMENT_WITH_ERROR,
            "typecheck_dot_descent_expression_against_namespace() : found identifier, but its binding statement is reported as in error");
    }
    if (is_value_tc_const(pBinding)) {
        if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_CONSTANT_AS_LVALUE,
                "typecheck_dot_descent_expression_against_namespace() : found binding to constant when expecting assignable");
        }
    } else {
        if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                "typecheck_dot_descent_expression_against_namespace() : found binding to variable when expecting constant");
        }
    }
    set_existing_value_as(pBinding, pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement);
    return set_node_typecheck_expr_success(pExpr->pTCNode);
}

local_func ETCResult typecheck_dot_descent_expression(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Dot-Descent"), pTCContext->pWorker);

    Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPR_DOT_DESCENT);
    Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_DOT);

    Assert_(!is_node_already_typechecked(pExpr->pTCNode)); // our caller is parse_any_non_invoc_expression() and should have taken care of this
    if_expr_already_typechecked_phase1_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    TmpTCNode followingExpr = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
    int iIdentifierHandle = ERES_INVALID_ID;
    if (LIKELY(u8(followingExpr.pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ATOMICEXPR_IDENTIFIER)) {

        iIdentifierHandle = int(followingExpr.pTCNode->ast.uPrimaryPayload);
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Typechecking dot-descent-expression, found identifier '%s' as the leaf",
            reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str())), pTCContext->pWorker);

    } else if (u8(followingExpr.pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPR_PARENTISED) {
        // TODO ???
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_dot_descent_expression() : dot-descent followed by parens not yet implemented");
    } else {
        // TODO: possible macro-expansion ?? is this really even possible since invoc would be applied 'on the result' ??
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_dot_descent_expression() : dot-descent followed by non-identifier not yet implemented");
    }

    if (pExpr->pTCNode->ast.uPrimaryChildNodeIndex == INVALID_NODE_INDEX) {
        if (inferredFromBelow.pIfType == 0) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_ISOLATED_DOT_DESCENT_MISSING_UPWARDS_INFERRENCE,
                "typecheck_dot_descent_expression() : isolated-dot-descent misses upwards inferrence to succeed");
        } else {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Typechecking dot-descent with no left-hand expression => trying to typecheck as-if left expression was current inferred type"),
                pTCContext->pWorker);
            return typecheck_dot_descent_expression_against_type(pExpr, inferredFromBelow.pIfType,
                iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
        }
    } else {
        TmpTCNode baseExpr = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking left-hand operand of the dot-descent operator"), pTCContext->pWorker);
            ETCResult checkBase = typecheck_expression(&baseExpr, pTCStatement, pTCContext,
                eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE ? EExpectedExpr::EXPECT_REGULAR : eExpectation, UpwardsInference{});
            success_or_return_wait_or_error(checkBase, pExpr->pTCNode);
            Assert_(is_node_already_typechecked(baseExpr.pTCNode));
        }
        ETypeKind eTypeKind;
        const TypeInfo* pUnaliasedType = unalias_ext(baseExpr.pIntrinsicValue->pType, &eTypeKind);
        switch (eTypeKind) {
            case ETypeKind::ETYPEKIND_STRUCTLIKE:
            {
                const TypeInfo_StructLike* pAsStruct = (const TypeInfo_StructLike*)pUnaliasedType;
                check_compound_type_full_availability_may_return_wait_or_error(pAsStruct, pExpr, pTCStatement, pTCContext,
                    "typecheck_dot_descent_expression() : cannot dot-descent against struclike instance from");
                return typecheck_dot_descent_expression_against_structlike_instance(pExpr, &baseExpr, pAsStruct,
                    iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
            } break;
        
            case ETypeKind::ETYPEKIND_ARRAY: {
                const TypeInfo_Array* pAsArray = (const TypeInfo_Array*)pUnaliasedType;
                return typecheck_dot_descent_expression_against_array_instance(pExpr, &baseExpr, pAsArray,
                        iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
            } break;

            case ETypeKind::ETYPEKIND_SET:
            case ETypeKind::ETYPEKIND_MAP:
            {
                // TODO:
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_dot_descent_expression() : dot-descent on sets and maps not yet implemented");
            } break;

            case ETypeKind::ETYPEKIND_OTHERCORE: {

                const TypeInfo_OtherCore* pAsOther = (const TypeInfo_OtherCore*)pUnaliasedType;
                if (pUnaliasedType->_coreFlags & OTHERCOREFLAG_IS_STRING) {
                    return typecheck_dot_descent_expression_against_string_related_instance(pExpr, &baseExpr, pAsOther,
                        iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
                }

                if (get_core_type_(pUnaliasedType) == ECoreType::ECORETYPE_NAMESPACE) {
                    Assert_(is_value_tc_only(baseExpr.pIntrinsicValue));
                    u64 uNamespaceId = baseExpr.pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
                    i32 iSourceFileIndex = i32(uNamespaceId);
                    Assert_(iSourceFileIndex >= 0);
                    u32 uNamespaceIndexInFile = u32(uNamespaceId >> 32) & 0x7FFF'FFFFu;
                    u64 bHasPackageAccess = uNamespaceId & NAMESPACEFLAG_HAS_PACKAGE_ACCESS;                    
                    SourceFileDescAndState* pFileWithNamespace = pTCContext->pProgCompilationState->vecSourceFiles[u32(iSourceFileIndex)];
                    TCNamespace* pRefNamespace = pFileWithNamespace->vecNamespaces[uNamespaceIndexInFile];
                    return typecheck_dot_descent_expression_against_namespace(pExpr, &baseExpr, pRefNamespace, bHasPackageAccess,
                            iIdentifierHandle, pTCStatement, pTCContext, eExpectation);

                } else if (get_core_type_(pUnaliasedType) == ECoreType::ECORETYPE_TYPE) {
                    Assert_(is_value_tc_only(baseExpr.pIntrinsicValue));
                    const TypeInfo* pTypeBeforeDot = type_from_type_node(baseExpr.pIntrinsicValue);
                    return typecheck_dot_descent_expression_against_type(pExpr, pTypeBeforeDot,
                        iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
                }
            } // fallthrough:
            default:
                return_error(pExpr, pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
                    "typecheck_dot_descent_expression() : dot-descent not available against this left-operand type");
        }
    }
}

// Here is the true 'big-switch' against expression kind
// The 'non-invoc' thing is justified by the fact that invoc-forms in AST have a possibility to represent macros, necessitating more complex handling of AST reorg.
// Invocs also may be regular func-calls, but with our multi-ret-value syntax, be representing more than one resulting expression...
// Once invocs have been ruled out (or handled as macros and solved as something else), then, we can really switch over the expression kind.
//
local_func ETCResult typecheck_any_non_invoc_expression(TmpTCNode* pExpr, u8 uNodeKind, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    Assert_(uNodeKind != ENodeKind::ENODE_EXPR_INVOCATION_FORM); // it's in the name...
    Assert_(is_inferred_none(inferredFromBelow) || eExpectation <= EExpectedExpr::EXPECT_REGULAR);
    
    if_expr_already_typechecked_phase1_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking a non-invoc expression, expecting %s",
        reinterpret_cast<u64>(tExpectedExprStr[eExpectation])), pTCContext->pWorker);

    // special handling of identifiers wrt. 'declarable' expectations
    //
    if (uNodeKind == ENodeKind::ENODE_ATOMICEXPR_IDENTIFIER) {
       
        int iIdentifierHandle = int(pExpr->pTCNode->ast.uPrimaryPayload);
        Assert_(iIdentifierHandle >= 0);
        Assert_(iIdentifierHandle != ERES_INVALID_ID);

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Identifier '%s' (%d)",
            reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str()),
            u64(u32(iIdentifierHandle))), pTCContext->pWorker);

        if (iIdentifierHandle < COUNT_RESERVED_WORDS) {
            return typecheck_reserved_word(pExpr, iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
        } else {
            if (eExpectation < EExpectedExpr::EXPECT_DECLARABLE) { // Non-declarations
                return typecheck_user_specified_identifier(pExpr, iIdentifierHandle, pTCStatement, pTCContext, eExpectation);
            } else {
                NodeValue* pNewValueAsNodeToSelf = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                pNewValueAsNodeToSelf->pType = g_pCoreTypesInfo[ECORETYPE_ASTNODE];
                pNewValueAsNodeToSelf->info = ir_make_info_for_int_immediate(i32(pExpr->uNodeIndexInStatement), 0x02u);
                pNewValueAsNodeToSelf->info.uIRandMetaFlags |= IRFLAG_TC_ONLY;
                return set_node_typecheck_expr_success(pExpr->pTCNode);
            }
        }
    }

    // then, disallow declarable expectation for all others
    if (eExpectation == EExpectedExpr::EXPECT_DECLARABLE) {
        // TODO: allow quotes for converting decl to assignment ??
        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_IDENTIFIER,
            "typecheck_any_non_invoc_expression() : non-identifier when expecting declarable");
    }

    // special case: parentized expression, comptime prefixes allowed.
    //
    if (uNodeKind == ENodeKind::ENODE_EXPR_PARENTISED) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Parentized expression"), pTCContext->pWorker);

        if (is_comptime_prefixed(&(pExpr->pTCNode->ast))) {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : comptime-prefix parentized expression not yet implemented");
        }
        TmpTCNode enclosedExpr = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
        ETCResult enclosedCheck = typecheck_expression(&enclosedExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
        if (enclosedCheck == ETCResult::ETCR_SUCCESS) {
            return set_tc_success_with_same_value_as_intrinsic_of(&enclosedExpr, pExpr);
        } otherwise_return_wait_or_error(enclosedCheck, pExpr->pTCNode);

    // also allowed comptime prefixes on ternary if
    } else if (uNodeKind == ENodeKind::ENODE_EXPR_TERNARY_IF) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Ternary-if"), pTCContext->pWorker);

        // TODO
        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
            "typecheck_any_non_invoc_expression() : ternary-if expression not yet implemented");
    }

    // then, disallow comptime prefixes on all others
    if (is_comptime_prefixed(&(pExpr->pTCNode->ast))) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_COMPTIME_PREFIX_THIS_NODE_KIND,
            "typecheck_any_non_invoc_expression() : comptime-prefix on non-identifier and non-parentized ast node");
    }

    Assert_(uNodeKind != ENodeKind::ENODE_EXPR_SPECIAL_BINARYOP); // should all have been replaced by something else by postparser...

    // special case: the following may still be referencable and assignable
    if (uNodeKind == ENodeKind::ENODE_EXPR_DEREF) {
        Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_DEREFERENCE);

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Deref"), pTCContext->pWorker);

        TmpTCNode addressParam = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
        EExpectedExpr eExpectFromAddress = (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) ? EExpectedExpr::EXPECT_REGULAR : eExpectation;
        ETCResult checkParam = typecheck_expression(&addressParam, pTCStatement, pTCContext, eExpectFromAddress, inferredFromBelow);
        success_or_return_wait_or_error(checkParam, pExpr->pTCNode);
        Assert_(is_node_already_typechecked(addressParam.pTCNode));
        if (LIKELY(get_type_kind(addressParam.pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER)) {
            const TypeInfo_Pointer* pAsPtrType = (const TypeInfo_Pointer*)addressParam.pIntrinsicValue->pType;
            const TypeInfo* pPointedToType = pAsPtrType->pPointedToType;
            check_type_footprint_availability_may_return_wait_or_error(pPointedToType, pExpr, pTCStatement, pTCContext,
                "failed to dereference pointer to");

            if (is_value_known_or_nyka(addressParam.pIntrinsicValue)) {
                Assert_(is_value_known_embd(addressParam.pIntrinsicValue));
                if (is_value_nyka_or_has_nyka(addressParam.pIntrinsicValue)) {
                    Assert_(is_value_single_nyka(addressParam.pIntrinsicValue));
                    i32 iOffset; u64 uBaseIR = ir_decode_nyka_value(addressParam.pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue, &iOffset);
                    Assert_(!ir_is_immediate(uBaseIR));
                    IRRepo* pBaseRepo;
                    u32 uBaseIndex;
                    SourceFileDescAndState* pBaseSourceFile;
                    EEntryKind eBaseEntryKind;
                    ir_decode_non_imm(uBaseIR, pTCContext, &pBaseRepo, &uBaseIndex, &pBaseSourceFile, &eBaseEntryKind);
                    if (eBaseEntryKind == EEntryKind::EEK_IS_PROCBODY_REF) {
                        // TODO: CLEANUP : or could we ??
                        return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_DEREF_PROC_BODY,
                            "typecheck_any_non_invoc_expression() : deref : cannot deref a proc-body address");
                    }
                    if (eBaseEntryKind == EEntryKind::EEK_FILEWISE_CONST || eBaseEntryKind == EEntryKind::EEK_PROGRAMWISE_ENTRY) { on_non_assignable:
                        if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
                            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                                "typecheck_any_non_invoc_expression() : deref : this entry is known non assignable");
                        }
                    } else if (eBaseEntryKind == EEK_CURRENT_PROC_LOCAL) {
                        Assert_(pBaseRepo);
                        IREntry& baseEntry = ir_access_repo_instr(pBaseRepo, uBaseIndex);
                        if (0 == (baseEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_IS_ASSIGNABLE))
                            goto on_non_assignable;
                    }
                }
            }
            IRInfo infoResult;
            EIRResult eResultEmitDeref = ir_emit_or_solve_deref(addressParam.pIntrinsicValue->info, get_ir_format(pPointedToType),
                get_log2_of_align_bytes(pPointedToType), get_slots_count(pPointedToType), get_runtime_unaligned_size(pPointedToType),
                eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE ? IR_INSTRFLAG_IS_ASSIGNABLE : 0u, pTCStatement, pTCContext, &infoResult);
            if (eResultEmitDeref < EIRResult::EIRR_FIRST_ERROR) {
                if (is_value_tc_const(addressParam.pIntrinsicValue)) {
                    ETCResult eEnsureConst = consolidate_tc_const_flag_on_info(&infoResult, pPointedToType, pExpr, pTCStatement, pTCContext);
                    Assert_(eEnsureConst == ETCResult::ETCR_SUCCESS);
                }
                if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                    Assert_(irflag_is_tc_const(addressParam.pIntrinsicValue->info.uIRandMetaFlags));
                    if (!irflag_is_tc_const(infoResult.uIRandMetaFlags)) {
                        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                            "typecheck_any_non_invoc_expression() : this deref value cannot be a considered as constant,"
                            " either due to the dereferenced address being known in non-const range, or dereferencing as a type requiring stronger"
                            " guarantees than what is known of the value there, or from deref size requiring to truncate a nyka present at this"
                            " location.");
                    }
                }
                NodeValue* pValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                pValue->pType = pPointedToType;
                pValue->info = infoResult;
                return set_node_typecheck_expr_success(pExpr->pTCNode);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, u16(eResultEmitDeref),
                    "typecheck_any_non_invoc_expression() : address-of : emission failed");
            }
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_POINTER,
                "typecheck_any_non_invoc_expression() : deref expression expected pointer parameter");
        }

    } else if (uNodeKind == ENodeKind::ENODE_EXPR_INDEXING) {
        Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_OPENING_BRACKET);
        return typecheck_indexing_expression(pExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);

    } else if (uNodeKind == ENodeKind::ENODE_EXPR_DOT_DESCENT) {
        Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_DOT);
        return typecheck_dot_descent_expression(pExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
    }

    // expressions from now on cannot be referencable => not assignable either.
    if (eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE) {
        // TODO: allow quotes for converting decl to assignment ??
        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
            "typecheck_any_non_invoc_expression() : non-assignable expression when expecting assignable");
    }

    // special case for literals : ensured consts, with final value directly on the ast-node 'payload'

    if (pExpr->pTCNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_IS_LITERAL) {
        Assert_(uNodeKind < ENodeKind::ENODE_ATOMICEXPR_IDENTIFIER);  // All literals are node-kind codes before 'identifier'
        return typecheck_atomic_literal(pExpr, uNodeKind, pTCStatement, pTCContext);
    }

    switch (uNodeKind) {

        case ENodeKind::ENODE_EXPR_UNARYOP: {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Unary-op (token: %s )",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
            TmpTCNode param = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            Assert_(uOp != ETOK_NOT); // should have been converted to bool unop by preparser
            Assert_(uOp != ETOK_OPENING_BRACKET && uOp != ETOK_OPENING_DYNARRAY && uOp != ETOK_HDIC); // should have been converted to pseudo-unop by preparser
            switch (uOp) {
                case ETOK_ADDRESSOF: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Address-of form"), pTCContext->pWorker);
                    {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("now Typechecking parameter of the address-form"), pTCContext->pWorker);
                        // TODO: 'inferredFromBelow' could maybe get a treatment here, if was a pointer kind...
                        ETCResult checkParam = typecheck_expression(&param, pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR, UpwardsInference{});
                        success_or_return_wait_or_error(checkParam, pExpr->pTCNode);
                    }
                    Assert_(is_node_already_typechecked(param.pTCNode));
                    if (!is_value_tc_referencable(param.pIntrinsicValue)) {
                        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPRESSION_NOT_REFERENCABLE,
                            "typecheck_any_non_invoc_expression() : address-of expression requires referencable");
                    }
                    u64 uParamIR = param.pIntrinsicValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK;
                    Assert_(!ir_is_immediate(uParamIR));
                    IRInfo infoAddress;
                    EIRResult eResultEmitAddressof = ir_emit_or_solve_address_of(param.pIntrinsicValue->info, pTCStatement, pTCContext, &infoAddress);
                    if (eResultEmitAddressof < EIRResult::EIRR_FIRST_ERROR) {
                        if (infoAddress.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
                            Assert_(infoAddress.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                            if (0 == (infoAddress.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA))
                                infoAddress.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
                        }
                        if (eExpectation == EExpectedExpr::EXPECT_CONSTANT && 0 == (infoAddress.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST)) {
                            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                                "typecheck_any_non_invoc_expression() : address-of : this address cannot be a constant");
                        }
                        NodeValue* pValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                        pValue->pType = get_pointer_type_to(param.pIntrinsicValue->pType, pTCContext);
                        pValue->info = infoAddress;
                        return set_node_typecheck_expr_success(pExpr->pTCNode);
                    } else {
                        return_error(pExpr, pTCStatement, pTCContext, u16(eResultEmitAddressof),
                            "typecheck_any_non_invoc_expression() : address-of : emission failed");
                    }
                } break;

                default: {
                    {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("now Typechecking parameter of the unary op"), pTCContext->pWorker);
                        ETCResult checkParam = typecheck_expression(&param, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
                        success_or_return_wait_or_error(checkParam, pExpr->pTCNode);
                    }
                    Assert_(is_node_already_typechecked(param.pTCNode));
                    Assert_(param.pIntrinsicValue);
                    if (uOp == ETOK_UNARY_PLUS) { // unary plus is identity function for all numeric
                        // TODO: should we however check for signalling nan operands ???
                        ETypeKind eTypeKind;
                        const TypeInfo* pType = unalias_ext(param.pIntrinsicValue->pType, &eTypeKind);
                        if (eTypeKind == ETypeKind::ETYPEKIND_FLOATINGPOINT ||
                                (eTypeKind == ETypeKind::ETYPEKIND_INTEGRAL && !is_raw_integral_(pType))) {
                            return set_tc_success_with_same_value_as_intrinsic_of(&param, pExpr);
                        } else {
                            // TODO: allow vec of numeric ?
                            return_error(pExpr, pTCStatement, pTCContext, CERR_OPERATOR_REQUIRES_NUMERIC_OPERANDS,
                                "unary-plus operator requires a numeric operand (floating point or non-raw integral)");
                        }
                    } else {
                        u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
                        return typecheck_regular_unary_op(pExpr, uOp, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
                    }
                } break;

            } break;

        } break;

        case ENodeKind::ENODE_EXPR_BOOL_NOT: {
            Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_BOOL_NOT);

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Boolean NOT"), pTCContext->pWorker);
            
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : 'BOOL NOT' unary operator not yet implemented");
        } break;

        case ENodeKind::ENODE_EXPR_TYPE_CTOR: {

            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Type CTOR (token: %s )",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);

            TmpTCNode param = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("now typechecking as base type for type ctor"), pTCContext->pWorker);
                ETCResult paramCheck = typecheck_expression(&param, pTCStatement, pTCContext,
                    EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
                success_or_return_wait_or_error(paramCheck, pExpr->pTCNode);
            }
            const TypeInfo* pParamNodeType = param.pIntrinsicValue->pType;

            if (pParamNodeType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                Assert_(is_value_tc_const(param.pIntrinsicValue));
                const TypeInfo* pBaseType = type_from_type_node(param.pIntrinsicValue);
                
                switch (uOp) {
                    case ETOK_ARRAYLIKE_DECL: {
                        TmpTCNode insideWrapper = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
                        Assert_(u8(insideWrapper.pTCNode->ast.uNodeKindAndFlags) == ENODE_SUBEXPR_WRAPPER);
                        Assert_(u8(insideWrapper.pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_CLOSING_BRACKET);
                        if (insideWrapper.pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                            check_type_footprint_availability_may_return_wait_or_error(pBaseType, pExpr, pTCStatement, pTCContext,
                                "cannot succeed array delaration against");
                            TmpTCNode insideBrackets = init_tmp_tc_node(insideWrapper.pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                            return typecheck_arraylike_decl(pExpr, &insideBrackets, pBaseType, pTCStatement, pTCContext, inferredFromBelow);
                        } else { // empty brackets => basic slice declaration
                            // ...ensured common enough that we'd rather keep that here outside the 'arraylike' call altogether...
                            return tc_make_slice_type_as(pExpr, pBaseType, pTCStatement, pTCContext);
                        }
                    } break;

                    case ETOK_OPENING_DYNARRAY: {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_any_non_invoc_expression() : dynamic-array-type declaration not yet implemented");
                    } break;

                    case ETOK_POINTER_DECL: {
                        return tc_make_pointer_type_as(pExpr, pBaseType, pTCStatement, pTCContext);
                    } break;

                    case ETOK_COMPACT_DECL: {
                        return tc_try_make_compact_type_as(pExpr, pBaseType, pTCStatement, pTCContext);
                    } break;

                    case ETOK_HDIC: {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_any_non_invoc_expression() : map-type declaration not yet implemented");
                    } break;

                    default:
                        return_error(pExpr, pTCStatement, pTCContext, FERR_UNREACHABLE,
                            "typecheck_any_non_invoc_expression() : unknown type-constructor");
                }

            } else {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                    "type-ctor syntax requires a type.");
            }
        } break;

        case ENodeKind::ENODE_EXPR_BINARYOP: {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Binary Op (token: %s )",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);

            return typecheck_regular_binary_op(pExpr, uOp, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
        } break;

        case ENodeKind::ENODE_EXPR_BOOL_BINARYOP: {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Boolean Binary Op (token: %s )",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);

            bool isOpAnd = (uOp == ETOK_BOOL_AND);
            Assert_(isOpAnd || uOp == ETOK_BOOL_OR);
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : Boolean binary operator not yet implemented (outside cond)");
        } break;

        case ENodeKind::ENODE_EXPR_EQ_CMP_BINARYOP: {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            Assert_(uOp == ETOK_ARE_EQUAL || uOp == ETOK_ARE_NOT_EQUAL);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Eq-Cmp Binary Op (token: %s )",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
            TmpTCNode operandA = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            TmpTCNode operandB = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking LHS Expression of Eq-Cmp",
                    reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
                ETCResult eCheckA = typecheck_expression(&operandA, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckA, pExpr->pTCNode);
            }
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking RHS Expression of Eq-Cmp",
                    reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
                ETCResult eCheckB = typecheck_expression(&operandB, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckB, pExpr->pTCNode);
            }
            return typecheck_eq_comparison_cond_or_expr(pExpr, uOp, 0u, &operandA, &operandB, pTCStatement,
                0, false, EBranchKind::BRANCH_TAKEN_UNKNOWN, 0, false, EBranchKind::BRANCH_TAKEN_UNKNOWN,
                pTCContext, eExpectation, 0u);
        }

        case ENodeKind::ENODE_EXPR_ORD_CMP_BINARYOP: {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
                Assert_(uOp == ETOK_LESSER_THAN || uOp == ETOK_LESSER_OR_EQ || uOp == ETOK_GREATER_THAN || uOp == ETOK_GREATER_OR_EQ);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Ord-Cmp Binary Op (token: %s )",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
            TmpTCNode operandA = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            TmpTCNode operandB = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking LHS Expression of Ord-Cmp",
                    reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
                ETCResult eCheckA = typecheck_expression(&operandA, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckA, pExpr->pTCNode);
            }
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking RHS Expression of Ord-Cmp",
                    reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
                ETCResult eCheckB = typecheck_expression(&operandB, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckB, pExpr->pTCNode);
            }
            return typecheck_ord_comparison_cond_or_expr(pExpr, uOp, 0u, &operandA, &operandB, pTCStatement,
                0, false, EBranchKind::BRANCH_TAKEN_UNKNOWN, 0, false, EBranchKind::BRANCH_TAKEN_UNKNOWN,
                pTCContext, eExpectation, 0u);
        }

        case ENodeKind::ENODE_EXPR_PROCLIKE_DEF:
        case ENodeKind::ENODE_EXPR_OTHER_DEF:
        {
            return_error(pExpr, pTCStatement, pTCContext, CERR_COMPLEX_TYPE_DECL_FORM_OUTSIDE_CONST_DECLARATION_WITH_SINGLE_RHV,
                "typecheck_any_non_invoc_expression() : complex type decl form found outside of const declaration with single rhv");
        } break;

        case ENodeKind::ENODE_EXPR_SOMETHINGOF: {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : SomethingOf ('%s')",
                reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);

            TmpTCNode param = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            if (uOp == ETOK_SIGNATUREOF) {
                u8 uParamNodeKind = u8(param.pTCNode->ast.uNodeKindAndFlags);
                if (uParamNodeKind == ENodeKind::ENODE_EXPR_PROCLIKE_DEF) {
                    TypeInfo_ProcLike resultingProcSign;
                    ArenaRefPoint beforeTCSign = get_arena_ref_point(pTCContext->pWorker->tmpArena);
                    {
                        BLOCK_TRACE(ELOCPHASE_REPORT,_LLVL8_REGULAR_INFO,EventREPT_CUSTOM_HARDCODED("Now typechecking proc-decl param of the '%s' Expression",
                            reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
                        ETCResult checkSign = do_typecheck_as_proc_signature(&param, pTCStatement, pTCContext, &resultingProcSign);
                        if (checkSign == ETCResult::ETCR_SUCCESS) {
                            Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                            TypeInfo_ProcLike* pProcSign = (TypeInfo_ProcLike*)alloc_from(localArena,
                                sizeof(TypeInfo_ProcLike), alignof(TypeInfo_ProcLike));
                            init_proc_like(pProcSign, get_proc_kind(&resultingProcSign), localArena);
                            pProcSign->params.append_all(resultingProcSign.params);
                            set_proc_like_input_param_count(pProcSign, get_input_param_count(&resultingProcSign));
                            pProcSign->asNode.info.uIRandMetaFlags = declare_user_proc_sign_format(pProcSign, pTCContext);
                            pProcSign->asNode.pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
                            pProcSign->asNode.info.metaValue.knownValue.pType = pProcSign;
                            pProcSign->uReadyStatus = resultingProcSign.uReadyStatus;
                            u32 uAsNewValueId = pTCStatement->vecNodeValues.size();
                            pTCStatement->vecNodeValues.append(&(pProcSign->asNode));
                            param.pTCNode->uIntrinsicValueIndex = uAsNewValueId;
                            param.pIntrinsicValue = &(pProcSign->asNode);
                        }
                        reset_arena_to(beforeTCSign, pTCContext->pWorker->tmpArena);
                        success_or_return_wait_or_error(checkSign, pExpr->pTCNode);
                    }
                    return set_tc_success_with_same_value_as_intrinsic_of(&param, pExpr);

                } else if (uParamNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
                    // TODO
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_any_non_invoc_expression() : signature keyword expects proc declaration ; an invocation-form was found - it may be a valid macro expansion, but not yet implemented");
                } else {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_PROC_DECLARATION,
                        "typecheck_any_non_invoc_expression() : signature keyword expects proc declaration");
                }

            } else {
                {
                    BLOCK_TRACE(ELOCPHASE_REPORT,_LLVL8_REGULAR_INFO,EventREPT_CUSTOM_HARDCODED("Now typechecking param of the '%s' Expression",
                        reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);
                    ETCResult checkParam = typecheck_expression(&param, pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR, UpwardsInference{});
                    success_or_return_wait_or_error(checkParam, pExpr->pTCNode);
                }
                switch (uOp) {
                    case ETOK_SIZEOF: {
                        const TypeInfo* pQueriedType = 0;
                        if (param.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                            Assert_(is_value_tc_only(param.pIntrinsicValue));
                            pQueriedType = type_from_type_node(param.pIntrinsicValue);
                        } else {
                            pQueriedType = param.pIntrinsicValue->pType;
                        }
                        u16 uErr = 0u;
                        if (is_allowed_as_runtime_type(pQueriedType, pTCContext, &uErr)) {
                            u64 uCompintResultPayload = u64(get_runtime_sizeof(pQueriedType)) << COMPINT_VALUE_SHIFT_WHENSMALL;
                            return make_compint_result_with_payload(uCompintResultPayload, pExpr, pTCStatement, pTCContext);
                        } else {
                            if (param.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                                return_error(pExpr, pTCStatement, pTCContext, uErr,
                                    "typecheck_any_non_invoc_expression() : cannot evaluate sizeof on a type instance with value of a non-runtime type");
                            } else {
                                return_error(pExpr, pTCStatement, pTCContext, uErr,
                                    "typecheck_any_non_invoc_expression() : cannot evaluate sizeof on an instance of a non-runtime type");
                            }
                        }
                    } break;
                
                    case ETOK_ALIGNOF: {
                        const TypeInfo* pQueriedType = 0;
                        if (param.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                            Assert_(is_value_tc_only(param.pIntrinsicValue));
                            pQueriedType = type_from_type_node(param.pIntrinsicValue);
                        } else {
                            pQueriedType = param.pIntrinsicValue->pType;
                        }
                        u16 uErr = 0u;
                        if (is_allowed_as_runtime_type(pQueriedType, pTCContext, &uErr)) {
                            u64 uCompintResultPayload = u64(get_byte_count_of_align(pQueriedType)) << COMPINT_VALUE_SHIFT_WHENSMALL;
                            return make_compint_result_with_payload(uCompintResultPayload, pExpr, pTCStatement, pTCContext);
                        } else {
                            if (param.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                                return_error(pExpr, pTCStatement, pTCContext, uErr,
                                    "typecheck_any_non_invoc_expression() : cannot evaluate alignof on a type instance with value of a non-runtime type");
                            } else {
                                return_error(pExpr, pTCStatement, pTCContext, uErr,
                                    "typecheck_any_non_invoc_expression() : cannot evaluate alignof on an instance of a non-runtime type");
                            }
                        }
                    } break;

                    case ETOK_TYPEOF: {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_any_non_invoc_expression() : typeof expressions not yet implemented");
                    } break;

                    case ETOK_TYPEIDOF: {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_any_non_invoc_expression() : typeidof expressions not yet implemented");
                    } break;

                    case ETOK_TYPEINFOOF: {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_any_non_invoc_expression() : typeinfoof expressions not yet implemented");
                    } break;

                    default:
                        return_error(pExpr, pTCStatement, pTCContext, FERR_UNREACHABLE,
                            "typecheck_any_non_invoc_expression() : something-of expression with unexpected token");
                }
            }
        } break;

        case ENodeKind::ENODE_EXPR_CURLYINIT: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Curly-init form"), pTCContext->pWorker);
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : curly-init expression not yet implemented");
        } break;

        case ENodeKind::ENODE_EXPR_ARRAYINIT: {
            Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_OPENING_ARRAY_LIT);

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Array-Literal Initializer"),
                pTCContext->pWorker);

            const TypeInfo* pExplicitType = 0;
            if (pExpr->pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                TmpTCNode typeNode = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                ETCResult typeCheck = typecheck_expression(&typeNode, pTCStatement, pTCContext,
                    EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
                success_or_return_wait_or_error(typeCheck, pExpr->pTCNode);
                const TypeInfo* pTypeNodeType = typeNode.pIntrinsicValue->pType;
                if (pTypeNodeType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
                    Assert_(is_value_tc_only(typeNode.pIntrinsicValue));
                    pExplicitType = type_from_type_node(typeNode.pIntrinsicValue);
                    check_type_footprint_availability_may_return_wait_or_error(pExplicitType, pExpr, pTCStatement, pTCContext,
                        "Cannot literal-array-init from");
                } else {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                        "explicit expression before array literal should be a type");
                }
            }
            const TypeInfo* pElemType = pExplicitType;
            if (!pExplicitType) {
                const TypeInfo* pUpwardsInfType = inferredFromBelow.pIfType;
                if (pUpwardsInfType && get_type_kind(pUpwardsInfType) == ETypeKind::ETYPEKIND_ARRAY) {
                    pElemType = ((const TypeInfo_Array*)pUpwardsInfType)->pElementType;
                }
            }
            return typecheck_array_literal(pExpr, pElemType, pExplicitType == 0, pTCStatement, pTCContext, eExpectation);
        } break;

        case ENodeKind::ENODE_EXPR_SETINIT: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Set-init form"), pTCContext->pWorker);
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : set-init expression not yet implemented");
        } break;

        case ENodeKind::ENODE_EXPR_MAPINIT: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Map-init form"), pTCContext->pWorker);
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : map-init expression not yet implemented");
        } break;

        case ENodeKind::ENODE_EXPR_SET_DECL: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Set-declaration"), pTCContext->pWorker);
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_any_non_invoc_expression() : set-declaration not yet implemented");
        } break;

        case ENodeKind::ENODE_EXPR_LOAD: {

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : '#load' Expression"),
                pTCContext->pWorker);

            TmpTCNode filenameToLoad = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            ETCResult checkFileName = typecheck_expression(&filenameToLoad, pTCStatement, pTCContext,
                EExpectedExpr::EXPECT_CONSTANT, infer_type(g_pCoreTypesInfo[ECORETYPE_STRINGVIEW]));
            success_or_return_wait_or_error(checkFileName, pExpr->pTCNode);
            Assert_(is_node_already_typechecked(filenameToLoad.pTCNode));
            if (!is_string_related(filenameToLoad.pIntrinsicValue->pType)) {
                emit_error(&filenameToLoad, pTCStatement, pTCContext, CERR_EXPECTED_STRING,
                    "typecheck_any_non_invoc_expression() : #load expects following expression as string");
                set_node_tc_error(pExpr->pTCNode, CERR_EXPECTED_STRING);
                return ETCResult::ETCR_ERROR;
            } else if (0 == (filenameToLoad.pIntrinsicValue->info.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST)) {
                emit_error(&filenameToLoad, pTCStatement, pTCContext, CERR_EXPECTED_FULLY_KNOWN_CONSTANT,
                    "typecheck_any_non_invoc_expression() : #load expects following string expression as fully solvable at compile-time");
                set_node_tc_error(pExpr->pTCNode, CERR_EXPECTED_STRING);
                return ETCResult::ETCR_ERROR;
            }
            StringView filenameAsView = get_comptime_string_view_from_semantic_const_instance(filenameToLoad.pIntrinsicValue->info,
                (const TypeInfo_OtherCore*)filenameToLoad.pIntrinsicValue->pType, pTCContext);
                
            Assert_(filenameAsView.can_be_used_as_c_str()); // CLEANUP: really ?
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecked '#load' Expression on file '%s'",
                reinterpret_cast<u64>(filenameAsView.begin())), pTCContext->pWorker);

            u64 uRegistration;
            ETCResult checkRegistration = register_source_file_as_namespace(&uRegistration, filenameAsView,
                NAMESPACEFLAG_HAS_PACKAGE_ACCESS, // TODO... conditionnally package or not ?
                pExpr, pTCStatement, pTCContext);
            success_or_return(checkRegistration);

            NodeValue* pValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pValue->pType = g_pCoreTypesInfo[ECORETYPE_NAMESPACE];
            pValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN;
            pValue->info.metaValue.knownValue.uEmbeddedValue = uRegistration;
            return set_node_typecheck_expr_success(pExpr->pTCNode);

        } break;

        default:
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_EXPRESSION,
                "typecheck_any_non_invoc_expression() : unexpected node kind");
    }
}

// bread-and-butter 'typecheck_expression' implementation for most cases (pre-declared in LocLib_TypeCheckerBase.h)
local_func ETCResult typecheck_expression(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow, bool bAllowUserTypeOrProcDecl)
{
restart_if_macro:
    {
        // we need to handle invoc-forms differently from the rest => start by discriminating against those
        //   before falling back to 'typecheck_any_non_invoc_expression' which will have the true 'big switch'
        //   against node kind... proclike defs or compound type defs are also not usually allowed, unless explicitely stated as such here.

        u8 uNodeKind = u8(pExpr->pTCNode->ast.uNodeKindAndFlags);
        if (uNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
            u8 uOp = u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8);
            Assert_(uOp == ETOK_OPENING_PARENS);
            // proc invocation 'forms' are to be treated separately, since they could very well be macros and require expansion...
            u32 uNodeIndex = pExpr->uNodeIndexInStatement;
            bool bWasMacroExpansion = false;
            ETCResult checkInvoc = typecheck_invocation_form(pExpr, pTCStatement,
                pTCContext, eExpectation, inferredFromBelow,
                INVALID_NODE_INDEX, 0, 0, EInvocFormResultCount::EINVOC_RETURNS_ONE, &bWasMacroExpansion);
            if (checkInvoc == ETCResult::ETCR_SUCCESS) {
                if (bWasMacroExpansion) {
                    *pExpr = {};
                    pExpr->uNodeIndexInStatement = uNodeIndex;
                    pExpr->pTCNode = pTCStatement->vecNodes[uNodeIndex];
                    goto restart_if_macro;
                }
            }
            return checkInvoc;

        } else if (uNodeKind == ENodeKind::ENODE_EXPR_PROCLIKE_DEF) {
            if (bAllowUserTypeOrProcDecl) {
                return typecheck_proc_declaration(pExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, CERR_PROCLIKE_DECL_FORM_OUTSIDE_SIGN_OR_CONST_DECLARATION_WITH_SINGLE_RHV,
                    "typecheck_expression() : proclike decl form found outside of its possible contexts");
            }
        } else if (uNodeKind == ENodeKind::ENODE_EXPR_OTHER_DEF) {
            if (bAllowUserTypeOrProcDecl) {
                return typecheck_compound_declaration(pExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, CERR_COMPLEX_TYPE_DECL_FORM_OUTSIDE_CONST_DECLARATION_WITH_SINGLE_RHV,
                    "typecheck_expression() : compound-type decl form found outside of its possible contexts");
            }
        } else {
            return typecheck_any_non_invoc_expression(pExpr, uNodeKind, pTCStatement,
                pTCContext, eExpectation, inferredFromBelow);
        }
    }
}

local_func ETCResult typecheck_expr_or_multi_invoc(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, bool bAllowUserTypeOrProcDecl, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, bool* outWasMacroExpansion)
{
    *outWasMacroExpansion = false;
    u8 uNodeKind = u8(pExpr->pTCNode->ast.uNodeKindAndFlags);
    Assert_(uNodeKind != ENodeKind::ENODE_EXPRLIST_NODE); // the caller should be discriminating against list-nodes already
    
    // We're here similar to a 'typecheck_expression' implementation, with the particularity that invoc-forms are allowed to
    // emit multiple results to our 'tAllExpr';'ioNodeCount' array
    // => like typecheck_expression, we start by discriminating against those forms.

    if (uNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
        Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_INVOCATION);
        // proc invocations are to be treated separately, since they could very well be macros and require expansion...
        // furthermore, they may expand to multiple elements in a list on the right or left side of some statements.
        return typecheck_invocation_form(pExpr, pTCStatement,
            pTCContext, eExpectation, inferredFromBelow,
            uIndexOfOptNodeChainParent, tAllExpr, ioNodeCount, EInvocFormResultCount::EINVOC_RETURNS_ONE_OR_MANY,
            outWasMacroExpansion);

    } else if (uNodeKind == ENodeKind::ENODE_EXPR_PROCLIKE_DEF) {
        if (bAllowUserTypeOrProcDecl) {
            return typecheck_proc_declaration(pExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_PROCLIKE_DECL_FORM_OUTSIDE_SIGN_OR_CONST_DECLARATION_WITH_SINGLE_RHV,
                "typecheck_expr_or_multi_invoc() : proclike decl form found outside of const declaration with single rhv");
        }

    } else if (uNodeKind == ENodeKind::ENODE_EXPR_OTHER_DEF) {
        if (bAllowUserTypeOrProcDecl) {
            return typecheck_compound_declaration(pExpr, pTCStatement, pTCContext, eExpectation, inferredFromBelow);
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_COMPLEX_TYPE_DECL_FORM_OUTSIDE_CONST_DECLARATION_WITH_SINGLE_RHV,
                "typecheck_expr_or_multi_invoc() : complex type decl form found outside of const declaration with single rhv");
        }

    } else {
        return typecheck_any_non_invoc_expression(pExpr, uNodeKind, pTCStatement,
            pTCContext, eExpectation, inferredFromBelow);
    }
}

local_func ETCResult typecheck_possible_expr_list(u32 uStartingNodeIndex, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation,
    bool bAllowNoInit, TmpTCNode* tAllExpr, u8* ioNodeCount,
    const TypeInfo** tAllInferredTypes, u8 uInferredCount)
{
    u32 uCurrentNodeIndex = uStartingNodeIndex;

    // TODO: allow for totally vanishing node-count-macros ?

when_macro_start_again:
    {
        bool bWasMacroExpansion = false;
        TCNode* pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
        while (u8(pCurrentNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPRLIST_NODE) {
            u32 uNextNodeIndex = pCurrentNode->ast.uSecondaryChildNodeIndex;

            u32 uChildNodeIndex = pCurrentNode->ast.uPrimaryChildNodeIndex;
            Assert_(uChildNodeIndex != INVALID_NODE_INDEX);
            TmpTCNode* pChild = tAllExpr + (*ioNodeCount);
            *pChild = {};
            pChild->uNodeIndexInStatement = uChildNodeIndex;
            pChild->pTCNode = pTCStatement->vecNodes[uChildNodeIndex];
            if (*ioNodeCount >= 30u) {
                return_error(pChild, pTCStatement, pTCContext, CERR_TOO_MANY_EXPR_NODES,
                    "typecheck_possible_expr_list() : too many expr nodes in expr list");
            }
            UpwardsInference inferredFromBelow = {};
            if (tAllInferredTypes && (*ioNodeCount) < uInferredCount)
                inferredFromBelow.pIfType = tAllInferredTypes[*ioNodeCount];
            *ioNodeCount += 1;

            Assert_(u8(pChild->pTCNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);
            if (bAllowNoInit && u8(pChild->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ATOMICEXPR_SPECIAL &&
                    u8(pChild->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_UNINITIALIZED) {
                // allowed to encounter an 'explicit-no-init' value : and marker for this is a tc success with a nullptr type
                NodeValue* pNoInitMarkerValue = alloc_value_for(pChild, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                *pNoInitMarkerValue = {};
                set_node_typecheck_expr_success(pChild->pTCNode);
            } else {
                ETCResult checkChild = typecheck_expr_or_multi_invoc(pChild, pTCStatement, pTCContext,
                    eExpectation, false, inferredFromBelow, uCurrentNodeIndex, tAllExpr, ioNodeCount, &bWasMacroExpansion);
                if (checkChild == ETCResult::ETCR_SUCCESS) {
                    if (bWasMacroExpansion) {
                        *ioNodeCount -= 1;
                        goto when_macro_start_again;
                    }
                    // otherwise NOOP
                } else
                    return checkChild;
            }

            if (uNextNodeIndex != INVALID_NODE_INDEX) {
                uCurrentNodeIndex = uNextNodeIndex;
                pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
            } else {
                return_error(pChild, pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
                    "typecheck_possible_expr_list() : node with no sibling (dangling comma) are not allowed there");
            }
        }

        Assert_(pCurrentNode); // also a result of no dangling comma
        Assert_(u8(pCurrentNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);
        TmpTCNode* pLast = tAllExpr + (*ioNodeCount);
        *pLast = {};
        pLast->uNodeIndexInStatement = uCurrentNodeIndex;
        pLast->pTCNode = pTCStatement->vecNodes[uCurrentNodeIndex];
        UpwardsInference inferredFromBelow = {};
        if (tAllInferredTypes && (*ioNodeCount) < uInferredCount)
            inferredFromBelow.pIfType = tAllInferredTypes[*ioNodeCount];
        *ioNodeCount += 1;

        ETCResult checkLast = ETCResult::ETCR_SUCCESS;
        if (bAllowNoInit && u8(pLast->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ATOMICEXPR_SPECIAL &&
                u8(pLast->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_UNINITIALIZED) {
            // allowed to encounter an 'explicit-no-init' value : and marker for this is a tc success with a nullptr type
            NodeValue* pNoInitMarkerValue = alloc_value_for(pLast, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            *pNoInitMarkerValue = {};
            set_node_typecheck_expr_success(pLast->pTCNode);
        } else {
            bool bAllowUserTypeOrProcDecl = (eExpectation == EExpectedExpr::EXPECT_CONSTANT) && (*ioNodeCount) == 1;
            checkLast = typecheck_expr_or_multi_invoc(pLast, pTCStatement, pTCContext,
                eExpectation, bAllowUserTypeOrProcDecl, inferredFromBelow, INVALID_NODE_INDEX, tAllExpr, ioNodeCount, &bWasMacroExpansion);
            if (checkLast == ETCResult::ETCR_SUCCESS && bWasMacroExpansion) {
                *ioNodeCount -= 1;
                goto when_macro_start_again;
            }
        }
        return checkLast;
    }
}

local_func void set_global_binding_in_error(int iIdentifierHandle, u64 uHash,
    TmpTCNode* pDecl, bool bIsConstant, TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Setting global binding for identifier '%s' to special <in-error> (within namespace %u of file %s)",
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, iIdentifierHandle).c_str()),
        u64(pTCContext->pNamespace->uRegistrationIndex),
        reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    ValueBinding* pBinding = (ValueBinding*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 
        sizeof(ValueBinding), alignof(ValueBinding));
    set_binding_source_ref(pBinding, pTCStatement, pTCContext,
        bIsConstant ? EDeclAttributes::EDECLATTR_REGULAR_CONST : EDeclAttributes::EDECLATTR_REGULAR_VAR);
    pBinding->pType = 0; // marker for a binding in error
    pBinding->info.uIRandMetaFlags = bIsConstant ? u64(IRFLAG_TC_SEMANTIC_CONST|IRFLAG_TC_ONLY|IRFLAG_TC_BINDING_INSTANCE) :
                                                   u64(IRFLAG_TC_BINDING_INSTANCE);
    pBinding->info.metaValue._payload = 0uLL;

    u32 uRegistrationPos = pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.size();
    pBinding->uScopeAndLocation = (uRegistrationPos << 8) | pTCContext->eGlobalDeclScope;
    pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.append(pBinding);
    pTCContext->pNamespace->mapAllGlobalDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
    pTCContext->pNamespace->mapAllBindingsInclUsing.insert(iIdentifierHandle, pBinding);
    if (pTCContext->eGlobalDeclScope != EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
        pTCContext->pNamespace->mapAccessibleDeclarationsById.insert(iIdentifierHandle, uRegistrationPos);
        pTCContext->pNamespace->mapAccessibleBindingsInclUsing.insert(iIdentifierHandle, pBinding);
    }

    pTCContext->setOfNewlyDeclaredIdentifiers.insert(iIdentifierHandle);
}

// This function is to be called specially for errors in a declaration statement.
//   if the context is not local, then all identifiers to be declared will be registered as 'in-error',
//   so that possible tasks waiting on those identifiers to be bound and found can fail and report, instead of continue waiting.
local_func ETCResult set_declaration_in_error(TmpTCNode* tAllDeclLhv, u8 uDeclLhvNodeCount,
    TmpTCNode* pMainNode, bool bIsConstant, TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Putting whole %s declaration statement in error, with %u bindings",
        bIsConstant ? reinterpret_cast<u64>("const") : reinterpret_cast<u64>("variable"), u64(uDeclLhvNodeCount)), pTCContext->pWorker);

    EDeclAttributes declAttr = bIsConstant ? EDeclAttributes::EDECLATTR_REGULAR_CONST : EDeclAttributes::EDECLATTR_REGULAR_VAR;
    if (is_ctx_global(pTCContext)) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Bindings are global => emitting actual bindings in error"), pTCContext->pWorker);
        for (u8 uDecl = 0; uDecl < uDeclLhvNodeCount; uDecl++) {
            TmpTCNode* pDecl = tAllDeclLhv + uDecl;
            Assert_(is_node_already_typechecked(pDecl->pTCNode));
            if (pDecl->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no check
                int iIdentifierHandle = get_id_from_decl_node(pDecl, pTCStatement);
                u64 uHash = get_map_hash(iIdentifierHandle);
                ValueBinding* bAlreadyFound = find_binding_within_namespace(iIdentifierHandle, uHash,
                    pTCContext->pNamespace, false, pTCContext); // Cleanup: really 'false' check parents there ?
                if (!bAlreadyFound) { // if already found, we'll here have an already found error, but we'll not force that
                                      //   existing binding to be modified. This may make different schedules produce different error-results
                                      //   for a same source, maybe (TODO: CLEANUP ?) but we'll only register the *new* ones as in error
                    set_global_binding_in_error(iIdentifierHandle, uHash, pDecl, bIsConstant, pTCStatement, pTCContext);
                }
            }
        }
    }
    return set_node_tc_error(pMainNode->pTCNode, 0);
}

// At statement level, typechecks declaration expressions
local_func ETCResult typecheck_declaration_statement(TmpTCNode* pMainNode, bool bIsConstant,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Main Node of a %s declaration statement",
        bIsConstant ? reinterpret_cast<u64>("const") : reinterpret_cast<u64>("variable")), pTCContext->pWorker);

    if (is_ctx_compound(pTCContext)) {
        Assert_(pTCContext->pCompoundToTC);
        if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETYPEKIND_ENUM) {
            return_error(pMainNode, pTCStatement, pTCContext, CERR_INVALID_DECLARATION_IN_ENUM,
                "typecheck_declaration_statement() : cannot declare var or const within an enumerate. Use single identifiers, with explicit values set using the '=' token");
        } else { Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETYPEKIND_STRUCTLIKE);
            if (bIsConstant)
                pTCContext->uFlags |= CTXFLAG_CURRENT_TC_STRUCTCONST;
        }
    }

    u32 uDeclLhvNodeIndex = pMainNode->pTCNode->ast.uPrimaryChildNodeIndex;
    u32 uRhvNodeIndex = pMainNode->pTCNode->ast.uSecondaryChildNodeIndex;
    Assert_(uDeclLhvNodeIndex != INVALID_NODE_INDEX);
    Assert_(uRhvNodeIndex != INVALID_NODE_INDEX);

    TmpTCNode tAllDeclLhv[32];
    u8 uDeclLhvNodeCount = 0;

    ETCResult checkDeclLhv;
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Now typechecking left-hand expression (or list thereof) for the declaration statement => hoping to find declarable identifiers"), pTCContext->pWorker);
        checkDeclLhv = typecheck_possible_expr_list(uDeclLhvNodeIndex,
            pTCStatement, pTCContext, EExpectedExpr::EXPECT_DECLARABLE,
            false, tAllDeclLhv, &uDeclLhvNodeCount);
    }

    if (checkDeclLhv == ETCResult::ETCR_SUCCESS) {
        Assert_(uDeclLhvNodeCount); // otherwise checkDeclLhv should not have been success
        EDeclAttributes declAttr = bIsConstant ? EDeclAttributes::EDECLATTR_REGULAR_CONST : EDeclAttributes::EDECLATTR_REGULAR_VAR;
        bool bDeclareAsProcLocal = false;
        bool bDeclareAsStructLike = false;
        bool bDeclareAsGlobal = false;
        if (pTCContext->pProcSource)
            bDeclareAsProcLocal = true;
        else if (is_ctx_compound(pTCContext)) {
            Assert_(pTCContext->pCompoundToTC);
            Assert_(pTCContext->pCompoundToTC->pCompoundType);
            Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE); // enums ruled out at this point
            bDeclareAsStructLike = true;
        } else {
            bDeclareAsGlobal = true;
        }
        bool bAllowShadowingOfLocals = !bDeclareAsProcLocal; // TODO: @shadowing tag to override this
        bool bAllowShadowingOfGlobals = bDeclareAsProcLocal;
        SourceFileDescAndState* pGlobalDeclFile = pTCContext->pIsolatedSourceFile;
        int allIds[32]; Assert_(uDeclLhvNodeCount <= 32u);
        
        //
        // Checking now if identifier is okay. That is, not yet used for another binding which could conflict within current context
        //

        for (u8 uDecl = 0; uDecl < uDeclLhvNodeCount; uDecl++) {

            TmpTCNode* pDecl = tAllDeclLhv + uDecl;
            Assert_(is_node_already_typechecked(pDecl->pTCNode));
            if (pDecl->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no check

                int iIdentifierHandle = get_id_from_decl_node(pDecl, pTCStatement);
                Assert_(iIdentifierHandle >= COUNT_RESERVED_WORDS);

                // Checking that no identifier used twice in same declaration statement
                for (u8 uPrevious = 0; uPrevious < uDecl; uPrevious++) {
                    if (allIds[uPrevious] == iIdentifierHandle) {
                        emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                            "typecheck_declaration_statement() : already declared identifier (within same statement)");
                        return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                            bIsConstant, pTCStatement, pTCContext);
                    }
                }
                allIds[uDecl] = iIdentifierHandle;

                u64 uHash = get_map_hash(iIdentifierHandle);

                if (bDeclareAsGlobal) {
                    bool bConflictsUnshadowing = false;
                    ValueBinding* pAlreadyFound = find_binding_within_namespace(iIdentifierHandle, uHash, pTCContext->pNamespace, 
                        true, // also checking for parents of current namespace
                        pTCContext,
                        false, &bConflictsUnshadowing); // also checking against setLocalUnshadowing on each such visited...
                    if (pAlreadyFound) {
                        if (bConflictsUnshadowing) {
                            emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_declaration_statement() : this global declaration would conflict with shadowing rules");
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        } else {
                            emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_declaration_statement() : already declared global identifier");
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        }
                    }
                } else {
                    if (bDeclareAsProcLocal) {
                        // Checking that identifier is not already defined at same local scope,
                        //   or any other local parent if not explicitely allowed to shadow
                        ValueBinding* pAlreadyFoundLocally = find_binding_within_proclocal(iIdentifierHandle, uHash,
                            !bAllowShadowingOfLocals, pTCContext);
                        if (pAlreadyFoundLocally) {
                            emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_declaration_statement() : already declared identifier (at local scope)");
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        }
                    } else { Assert_(bDeclareAsStructLike);
                        // Checking that identifier is not already defined within members of same structlike
                        u32 uUnusedOffset; bool bUnusedAligned;
                        ValueBinding* pAlreadyFoundMember = find_binding_within_structlike(iIdentifierHandle, uHash,
                            (const TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType, &uUnusedOffset, &bUnusedAligned);
                        if (pAlreadyFoundMember) {
                            emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_declaration_statement() : already declared identifier (in same structlike)");
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        }
                    }

                    if (!bAllowShadowingOfGlobals) {
                        // Checking that identifier is not already defined within globals, if not allowed to shadow globals
                        ValueBinding* pAlreadyFoundGlobal = find_binding_within_namespace(iIdentifierHandle, uHash, pTCContext->pNamespace, 
                            true, // also checking for parents of current namespace
                            pTCContext,
                            true); // also adding to setLocalUnshadowing on each such visited...
                        if (pAlreadyFoundGlobal) {
                            emit_error(pDecl, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_declaration_statement() : already declared identifier (at global scope)");
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        }
                    }
                }

            } else {
                allIds[uDecl] = ERES_SINK;
            }
        }
    } otherwise_return_wait_or_error(checkDeclLhv, pMainNode->pTCNode);

    if (bIsConstant) {
        TmpTCNode tAllValuesRhv[32];
        u8 uValueRhvNodeCount = 0;
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking right-hand expression (or list thereof) for the const declaration statement => hoping to find constant values"), pTCContext->pWorker);
            ETCResult checkValueRhv = typecheck_possible_expr_list(uRhvNodeIndex,
                pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT,
                false, tAllValuesRhv, &uValueRhvNodeCount);
            if (checkValueRhv == ETCResult::ETCR_SUCCESS) {
                Assert_(uValueRhvNodeCount); // otherwise checkValueRhv should not have been success
            } else if (checkValueRhv == ETCResult::ETCR_ERROR) {
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            } otherwise_return_wait_or_error(checkValueRhv, pMainNode->pTCNode);
        }

        if (uValueRhvNodeCount == 1u) {

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking declaration as const from single-RHV"), pTCContext->pWorker);

            // single RHV => single or many LHV will get assigned same value
            for (u8 uDecl = 0; uDecl < uDeclLhvNodeCount; uDecl++) {
                TmpTCNode* pDecl = tAllDeclLhv + uDecl;
                Assert_(is_node_already_typechecked(pDecl->pTCNode));
                if (pDecl->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => noop
                    ETCResult checkBinding = do_const_binding(pDecl, tAllValuesRhv, pTCStatement, pTCContext);
                    if (checkBinding == ETCResult::ETCR_ERROR) {
                        return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                            bIsConstant, pTCStatement, pTCContext);
                    } else {
                        success_or_return_wait_or_error(checkBinding, pMainNode->pTCNode);
                    }
                }
            }
            return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

        } else {
            if (uDeclLhvNodeCount == uValueRhvNodeCount) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Now typechecking declaration as const from many RHVs, respectively to as-many LHVs"), pTCContext->pWorker);

                // many RHV => exactly as many LHV will get assigned corresponding values
                for (u8 uDecl = 0; uDecl < uDeclLhvNodeCount; uDecl++) {
                    TmpTCNode* pDecl = tAllDeclLhv + uDecl;
                    Assert_(is_node_already_typechecked(pDecl->pTCNode));
                    if (pDecl->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => noop
                        TmpTCNode* pValue = tAllValuesRhv + uDecl;
                        ETCResult checkBinding = do_const_binding(pDecl, pValue, pTCStatement, pTCContext);
                        if (checkBinding == ETCResult::ETCR_ERROR) {
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        } else {
                            success_or_return_wait_or_error(checkBinding, pMainNode->pTCNode);
                        }
                    }
                }
                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

            } else if (uDeclLhvNodeCount < uValueRhvNodeCount) {
                emit_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_FEW_LHV,
                    "typecheck_declaration_statement() : too few left-hand-values facing multi-right-handed list in const decl");
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            } else {
                emit_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_MANY_LHV,
                    "typecheck_declaration_statement() : too many left-hand-values facing multi-right-handed list in const decl");
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            }
        }

    } else {
        // 3 cases for var decls:
        //   1) <var> as <type>
        //   2) <var> as <type> = <initvalue>
        //   3) <var> as= <initvalue>
        // case 2 and 3 will have node kind for rhv as 'ENODE_TYPE_AND_INIT_VALUE'
        // note that <var>, <type> and <initvalue> are each allowed to be exprlists !!!

        if (0 == (pTCContext->uFlags & CTXFLAG_IS_ENUM_BODY)) {
            // NOOP ok
        } else {
            emit_error(pMainNode, pTCStatement, pTCContext, CERR_CANNOT_USE_REGULAR_VAR_DECLARATION_IN_ENUMS,
                "typecheck_declaration_statement() : cannot declare a variable in an enum declaration body");
            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                bIsConstant, pTCStatement, pTCContext);
        }

        TmpTCNode tAllValuesRhv[32];
        u8 uValueRhvNodeCount = 0;
        TmpTCNode tAllTypesRhv[32];
        const TypeInfo* tAllTypesForUpwardsInference[32];
        u8 uTypeRhvNodeCount = 0;
        u8 uRhvNodeCount = 0;
        bool bTypesAreImplicit = false;
        bool bValuesAreDefault = false;
        TCNode* pRhvCanBeTypeAndValue = pTCStatement->vecNodes[uRhvNodeIndex];
        if (u8(pRhvCanBeTypeAndValue->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPR_SINGLE_EQ) { // ie pivot is the '=' symbol after the 'as' keyword

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking variable declaration in the presence of the '=' symbol"), pTCContext->pWorker);

            if (pTCContext->eKind != ETypecheckContextKind::ECTXKIND_COMPOUND) {
                // NOOP ok
            } else {
                emit_error(pMainNode, pTCStatement, pTCContext, CERR_CANNOT_USE_TYPE_AND_INIT_VAR_DECL_FORM_IN_COMPOUNDS,
                    "typecheck_declaration_statement() : cannot use type-and-init form for variable declarations in struct declaration body");
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            }

            // primary of 'type and value' is an *optional* type (or list) ; secondary is a mandatory value (or list)

            u32 uTypeNodeIndex = pRhvCanBeTypeAndValue->ast.uPrimaryChildNodeIndex;
            if (uTypeNodeIndex != INVALID_NODE_INDEX) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Typechecking variable declaration with an explicit type before a '=' symbol => Now expecting type (or list thereof) between 'as' and '='"), pTCContext->pWorker);

                ETCResult checkTypeRhv = typecheck_possible_expr_list(uTypeNodeIndex,
                    pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT,
                    false, tAllTypesRhv, &uTypeRhvNodeCount);
                if (checkTypeRhv == ETCResult::ETCR_SUCCESS) {
                    Assert_(uTypeRhvNodeCount); // otherwise checkTypeRhv should not have been success
                } else if (checkTypeRhv == ETCResult::ETCR_ERROR) {
                    return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                        bIsConstant, pTCStatement, pTCContext);
                } otherwise_return_wait_or_error(checkTypeRhv, pMainNode->pTCNode);
                
                for (u8 uType = 0; uType < uTypeRhvNodeCount; uType++) {
                    Assert_(is_node_already_typechecked(tAllTypesRhv[uType].pTCNode));
                    NodeValue* pTypeValue = tAllTypesRhv[uType].pIntrinsicValue;
                    Assert_(is_value_tc_const(pTypeValue));
                    if (pTypeValue->pType == g_pCoreTypesInfo[ECoreType::ECORETYPE_TYPE]) {
                        Assert_(is_value_tc_only(pTypeValue));
                        const TypeInfo* pExplicitType = type_from_type_node(pTypeValue);
                        if (get_type_kind(pExplicitType) == ETYPEKIND_STRUCTLIKE) {
                            const TypeInfo_StructLike* pAsStructLike = (const TypeInfo_StructLike*)pExplicitType;
                            if (!is_structlike_type_footprint_available_otherwise_return_locked_for_wait_if_nonlocal(pAsStructLike, pTCContext)) {
                                return add_waiting_task_for_compound_body_already_event_locked_iff_other_file(pAsStructLike, tAllTypesRhv[uType].uNodeIndexInStatement, pTCContext);
                            } else if (pAsStructLike->_coreFlags & COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME) {
                                emit_error((tAllTypesRhv + uType),pTCStatement,pTCContext,CERR_COMPOUND_TYPE_IN_ERROR,
                                    "do_var_binding() : cannot finalize var declaration as a struct-like base-type which happens to be in error"); \
                                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                    bIsConstant, pTCStatement, pTCContext);
                            }
                        }
                        u16 uTypeErr = 0;
                        if (!is_allowed_as_runtime_type(pExplicitType, pTCContext, &uTypeErr)) {
                            emit_error(tAllTypesRhv + uType, pTCStatement, pTCContext, uTypeErr,
                                "typecheck_declaration_statement() : unallowed type in explicit type-slot for var declaration");
                            //set_node_tc_error(pMainNode->pTCNode, uTypeErr);
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        }
                        tAllTypesForUpwardsInference[uType] = pExplicitType;
                    } else {
                        emit_error(tAllTypesRhv + uType, pTCStatement, pTCContext, CERR_EXPECTED_TYPE,
                            "typecheck_declaration_statement() : expected type in type-slot for var declaration");
                        //set_node_tc_error(pMainNode->pTCNode, CERR_EXPECTED_TYPE);
                        return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                            bIsConstant, pTCStatement, pTCContext);
                    }
                }
            } else {
                bTypesAreImplicit = true;
            }

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking initial value (or list thereof) for the variable declaration statement after the '=' symbol"), pTCContext->pWorker);

            u32 uValueNodeIndex = pRhvCanBeTypeAndValue->ast.uSecondaryChildNodeIndex;
            Assert_(uValueNodeIndex != INVALID_NODE_INDEX);
            // global variables shall have constant initial value
            EExpectedExpr eValueExpectation = is_ctx_global(pTCContext) ? EExpectedExpr::EXPECT_CONSTANT : EExpectedExpr::EXPECT_REGULAR;
            ETCResult checkValueRhv = typecheck_possible_expr_list(uValueNodeIndex,
                pTCStatement, pTCContext, eValueExpectation,
                true, tAllValuesRhv, &uValueRhvNodeCount, tAllTypesForUpwardsInference, uTypeRhvNodeCount);
            if (checkValueRhv == ETCResult::ETCR_SUCCESS) {
                Assert_(uValueRhvNodeCount); // otherwise checkValueRhv should not have been success
                uRhvNodeCount = uValueRhvNodeCount;
                if (!bTypesAreImplicit && uTypeRhvNodeCount != uValueRhvNodeCount) {
                    emit_error(pMainNode, pTCStatement, pTCContext, CERR_TYPE_AND_VALUE_COUNT_DIFFER_IN_VAR_DECL,
                        "typecheck_declaration_statement() : distinct type count and value count in var decl RHV-lists");
                    return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                        bIsConstant, pTCStatement, pTCContext);
                }
            } else if (checkValueRhv == ETCResult::ETCR_ERROR) {
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            } otherwise_return_wait_or_error(checkValueRhv, pMainNode->pTCNode);

        } else {

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking variable declaration with a bare 'as' expression (ie without '=' symbol) => expecting type (or list thereof) as RHV now"), pTCContext->pWorker);

            // that node should eval to a type (or list).
            u32 uTypeNodeIndex = uRhvNodeIndex;
            ETCResult checkTypeRhv = typecheck_possible_expr_list(uTypeNodeIndex,
                pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT,
                false, tAllTypesRhv, &uTypeRhvNodeCount);
            if (checkTypeRhv == ETCResult::ETCR_SUCCESS) {
                Assert_(uTypeRhvNodeCount); // otherwise checkTypeRhv should not have been success
                uRhvNodeCount = uTypeRhvNodeCount;
            } else if (checkTypeRhv == ETCResult::ETCR_ERROR) {
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            } otherwise_return_wait_or_error(checkTypeRhv, pMainNode->pTCNode);

            bValuesAreDefault = true;
        }

        if (uRhvNodeCount == 1u) {

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking variable declaration from single-RHV"), pTCContext->pWorker);

            // single RHV => single or many LHV will get declared to same type and value
            for (u8 uDecl = 0; uDecl < uDeclLhvNodeCount; uDecl++) {
                TmpTCNode* pDecl = tAllDeclLhv + uDecl;
                Assert_(is_node_already_typechecked(pDecl->pTCNode));
                if (pDecl->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => noop
                    ETCResult checkBinding = do_var_binding(pDecl,
                        bTypesAreImplicit ? 0 : tAllTypesRhv, bValuesAreDefault ? 0 : tAllValuesRhv,
                        pTCStatement, pTCContext, tAllDeclLhv);
                    if (checkBinding == ETCResult::ETCR_ERROR) {
                        return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                            bIsConstant, pTCStatement, pTCContext);
                    } else {
                        success_or_return_wait_or_error(checkBinding, pMainNode->pTCNode);
                    }
                }
            }
            return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

        } else {
            if (uDeclLhvNodeCount == uRhvNodeCount) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Now typechecking variable declaration from many RHVs, respectively to as-many LHVs"), pTCContext->pWorker);

                // many RHV => exactly as many LHV will get declared to corresponding types and values
                for (u8 uDecl = 0; uDecl < uDeclLhvNodeCount; uDecl++) {
                    TmpTCNode* pDecl = tAllDeclLhv + uDecl;
                    Assert_(is_node_already_typechecked(pDecl->pTCNode));
                    if (pDecl->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no check
                        TmpTCNode* pType = bTypesAreImplicit ? 0 : tAllTypesRhv + uDecl;
                        TmpTCNode* pValue = bValuesAreDefault ? 0 : tAllValuesRhv + uDecl;
                        ETCResult checkBinding = do_var_binding(pDecl, pType, pValue, pTCStatement, pTCContext, tAllDeclLhv);
                        if (checkBinding == ETCResult::ETCR_ERROR) {
                            return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                                bIsConstant, pTCStatement, pTCContext);
                        } else {
                            success_or_return_wait_or_error(checkBinding, pMainNode->pTCNode);
                        }
                    }
                }
                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

            } else if (uDeclLhvNodeCount < uRhvNodeCount) {
                emit_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_FEW_LHV,
                    "typecheck_declaration_statement() : too few left-hand-values facing multi-right-handed list in var decl");
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            } else {
                emit_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_MANY_LHV,
                    "typecheck_declaration_statement() : too many left-hand-values facing multi-right-handed list in var decl");
                return set_declaration_in_error(tAllDeclLhv, uDeclLhvNodeCount, pMainNode,
                    bIsConstant, pTCStatement, pTCContext);
            }
        }
    }
}

// At statement level, typechecks nodes of the assignment kind
local_func ETCResult typecheck_assignment_statement(TmpTCNode* pMainNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Main Node of an Assignment Statement"),
        pTCContext->pWorker);

    if (pTCContext->uFlags & CTXFLAG_ALLOW_RUNTIME) {
        Assert_(pTCContext->pProcResult);

        u32 uDestLhvNodeIndex = pMainNode->pTCNode->ast.uPrimaryChildNodeIndex;
        u32 uSrcRhvNodeIndex = pMainNode->pTCNode->ast.uSecondaryChildNodeIndex;
        Assert_(uDestLhvNodeIndex != INVALID_NODE_INDEX);
        Assert_(uSrcRhvNodeIndex != INVALID_NODE_INDEX);

        TmpTCNode tAllDestLhv[32];
        u8 uDestLhvNodeCount = 0;
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking left-hand expression (or list thereof) for the assignment statement => hoping to find assignable entities"), pTCContext->pWorker);
            ETCResult checkDestLhv = typecheck_possible_expr_list(uDestLhvNodeIndex,
                pTCStatement, pTCContext, EExpectedExpr::EXPECT_ASSIGNABLE,
                false, tAllDestLhv, &uDestLhvNodeCount);
            success_or_return_wait_or_error(checkDestLhv, pMainNode->pTCNode);
        }
        Assert_(uDestLhvNodeCount); // otherwise checkDestLhv should not have been success
        const TypeInfo* tAllTypesForUpwardsInference[32];
        for (u8 uDest = 0; uDest < uDestLhvNodeCount; uDest++) {
            TmpTCNode* pDest = tAllDestLhv + uDest;
            Assert_(is_node_already_typechecked(pDest->pTCNode));
            tAllTypesForUpwardsInference[uDest] = pDest->pIntrinsicValue->pType; // if type is a sink, will be 0 => ok for a not-inferred
        }

        TmpTCNode tAllSrcRhv[32];
        u8 uSrcRhvNodeCount = 0;
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking right-hand expression (or list thereof) for the assignment statement"), pTCContext->pWorker);
            ETCResult checkSrcRhv = typecheck_possible_expr_list(uSrcRhvNodeIndex,
                pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR,
                false, tAllSrcRhv, &uSrcRhvNodeCount, tAllTypesForUpwardsInference, uDestLhvNodeCount);
            success_or_return_wait_or_error(checkSrcRhv, pMainNode->pTCNode);
        }
        Assert_(uSrcRhvNodeCount); // otherwise checkSrcRhv should not have been success

        if (uSrcRhvNodeCount == 1u) {

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Now typechecking assignment from single-RHV"), pTCContext->pWorker);

            // single RHV => single or many LHV will get declared to same type and value
            TmpTCNode* pSrc = tAllSrcRhv;
            Assert_(is_node_already_typechecked(pSrc->pTCNode));
            const TypeInfo* pDestType = 0;
            for (u8 uDest = 0; uDest < uDestLhvNodeCount; uDest++) {
                TmpTCNode* pDest = tAllDestLhv + uDest;
                if (pDest->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no check
                    if (pDestType == 0)
                        pDestType = pDest->pIntrinsicValue->pType;
                    else {
                        if (!are_types_same(pDestType, pDest->pIntrinsicValue->pType, pTCContext)) {
                            // TODO: maybe relax that requirement ? allow implicit casts ??
                            return_error(pMainNode, pTCStatement, pTCContext, CERR_SINGLE_TO_MULTI_ASSIGN_MUST_SHARE_TYPE,
                                "typecheck_assignment_statement() : single-source-to-multi-dest : all destinations should have same type");
                        }
                    }
                }
            }

            if (pDestType) {
                NodeValue* pSrcValue = pSrc->pIntrinsicValue;
                if (!are_types_same(pDestType, pSrcValue->pType, pTCContext)) {
                    // TODO: CHECK possibility of a short-circuit already typecast there ?
                    ETCResult checkConv = do_implicit_cast(pSrc, pDestType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR);
                    if (checkConv != ETCResult::ETCR_SUCCESS)
                        return checkConv;
                    Assert_(is_node_already_type_casted(pSrc->pTCNode));
                    pSrcValue = pSrc->pFinalValue;
                }
                for (u8 uDest = 0; uDest < uDestLhvNodeCount; uDest++) {
                    TmpTCNode* pDest = tAllDestLhv + uDest;
                    Assert_(is_node_already_typechecked(pDest->pTCNode));
                    if (pDest->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no check
                        if (is_node_already_type_casted(pDest->pTCNode)) // marker for already done
                            continue;
                        Assert(is_value_tc_referencable(pDest->pIntrinsicValue),
                            "non-sink node having passed an 'assignable'-expected TC should be user referencable");
                        Assert_(!is_value_tc_const(pDest->pIntrinsicValue));
                        u64 uDestIR = pDest->pIntrinsicValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK;
                        u64 uSrcIR = pSrcValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK;
                        Assert_(ir_is_valid_param(uDestIR));
                        Assert_(!ir_is_immediate(uDestIR));
                        Assert_(ir_is_valid_param(uSrcIR));
                        Assert_(!is_value_pseudo_valued_cond(pSrcValue));
                        do_store_value_to(uDestIR, uSrcIR, get_ir_format(pDestType), get_slots_count(pDestType),
                            pTCStatement, pTCContext);
                        set_node_type_cast_expr_success(pDest->pTCNode); // marker for already done
                    }
                }
            } // otherwise all are sinks => NOOP
            return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

        } else {
            if (uDestLhvNodeCount == uSrcRhvNodeCount) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Now typechecking assignment from multiple RHVs"), pTCContext->pWorker);

                // TODO!!
                return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_assignment_statement() : not yet implemented for multi-source");

                #if 0

                for (u8 uSrc = 0; uSrc < uSrcRhvNodeCount; uSrc++) {
                    TmpTCNode* pSrc = tAllSrcRhv + uSrc;
                    TmpTCNode* pDest = tAllDestLhv + uSrc;
                    Assert_(is_node_already_typechecked(pSrc->pTCNode));
                    if (pDest->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no reify
                        ETCResult checkReify = do_reify(pSrc, pDest->pIntrinsicValue->pType, false,
                            pTCStatement, pTCContext, true); // last bool pâram to 'true' to ensure of a sequential snapshot at this timepoint, allowing 'x,y := y,x'
                        success_or_return_wait_or_error(checkReify, pMainNode->pTCNode);
                    }
                }
                for (u8 uDest = 0; uDest < uDestLhvNodeCount; uDest++) {
                    TmpTCNode* pSrc = tAllSrcRhv + uDest;
                    Assert_(is_node_already_type_casted(pSrc->pTCNode));
                    Assert_(pSrc->pFinalValue->uIR != INVALID_IR_CODE);
                    TmpTCNode* pDest = tAllDestLhv + uDest;
                    Assert_(is_node_already_typechecked(pDest->pTCNode));
                    if (pDest->pIntrinsicValue->pType != 0) { // if type is 0, is a sink => no check
                        if (is_node_already_type_casted(pDest->pTCNode))
                            continue;
                        Assert(get_is_instrisic_user_referenceable_flag_from(pDest->pTCNode) != 0,
                            "non-sink node having passed an 'assignable'-expected TC should be user referencable");
                        Assert_(!is_value_const(pDest->pIntrinsicValue));
                        Assert_(pDest->pIntrinsicValue->uIR != INVALID_IR_CODE);
                        pTCStatement->uLastIRorGlobalTCResult = do_store_value_to(pDest->pIntrinsicValue->uIR, pSrc->pFinalValue, pTCContext);
                        set_node_type_cast_expr_success(pDest->pTCNode); // marker for already done
                    }
                }
                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                #endif

            } else if (uDestLhvNodeCount < uSrcRhvNodeCount) {
                return_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_FEW_LHV,
                    "typecheck_assignment_statement() : too few left-hand-values facing multi-right-handed list in assignment");
            } else {
                return_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_MANY_LHV,
                    "typecheck_assignment_statement() : too many left-hand-values facing multi-right-handed list in assignment");
            }
        }

    } else {
        return_error(pMainNode, pTCStatement, pTCContext, CERR_SEQ_STATEMENT_WHEN_EXPECTING_NON_SEQ_STATEMENT,
            "typecheck_assignment_statement() : not available outside of sequential context");
    }
}

// At statement level, typechecks other kinds of nodes...
local_func ETCResult typecheck_other_as_statement(TmpTCNode* pMainNode, u8 uNodeKind, TCStatement* pTCStatement,
    TCContext* pTCContext)
{
    return_error(pMainNode, pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
        "typecheck_other_as_statement() : node kind not allowed at statement level");
}


#endif // LOCLIB_TYPE_CHECKER_H_
