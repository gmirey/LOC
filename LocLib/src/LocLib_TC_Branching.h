#pragma once 

#ifndef LOCLIB_TC_BRANCHING_H_
#define LOCLIB_TC_BRANCHING_H_

#include "LocLib_TypeCheckerBase.h"
#include "LocLib_TC_ConstEvals.h"
#include "LocLib_TC_StdOpsEval.h"
#include "LocLib_TC_InvocLike.h"

local_func ETCResult on_emit_or_solve_pseudo_boolean_op(TmpTCNode* pResultNode, u8 uOpToken, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(!is_node_already_typechecked(pResultNode->pTCNode));
    Assert_(pOperandA != 0);
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_BOOL]);
    Assert_(uOpToken == ETOK_BOOL_NOT || pOperandB != 0);
    Assert_(pOperandB == 0 || is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandB == 0 || pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_BOOL]);

    IRInfo resultingInfo;
    EIRResult eResult = ir_emit_or_solve_pseudo_boolean_op(uOpToken,
        pOperandA->pIntrinsicValue->info, pOperandB ? pOperandB->pIntrinsicValue->info : IRInfo{}, pTCStatement, pTCContext, &resultingInfo);

    if (eResult == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A)
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pResultNode);
    if (eResult == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_B)
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pResultNode);
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        NodeValue* pResultingValue = alloc_value_for(pResultNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResultingValue->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
        pResultingValue->info = resultingInfo;
        return set_node_typecheck_expr_success(pResultNode->pTCNode);
    } else {
        return_error(pResultNode, pTCStatement, pTCContext, u16(eResult),
            "on_emit_or_solve_boolean_op() : IR Solver determined invalid op.");
    }
}

// predecls
ETCResult typecheck_expression(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow);
ETCResult typecheck_any_non_invoc_expression(TmpTCNode* pExpr, u8 uNodeKind, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow);
ETCResult typecheck_runtime_conditional(TmpTCNode* pNode, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uIsNegatedFromAbove);

// 'conjunction with shortcut' behaviour, identical to C for (conditionA() && conditionB()) :
//     * eval conditionA() :
//          - if false, do NOT eval conditionB(). results in false.
//          - if true, eval conditionB(). results in this last eval.
ETCResult typecheck_runtime_conditional_conjunction_with_shortcut(TmpTCNode* pNode, u8 uOriginalOp,
    TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uAreOperandsNegatedFromAbove)
{
    Assert_(uOriginalOp == ETOK_BOOL_AND || uOriginalOp == ETOK_BOOL_OR); // originally can be either, since 'conjunction' behaviour could come from an inverted OR
    Assert_(uAreOperandsNegatedFromAbove == 0u || uAreOperandsNegatedFromAbove == 1u);
    Assert_(pVecJumpSourcesWhenTrue && pVecJumpSourcesWhenFalse);
    Assert_(!bWhenTrueCanFallthrough || !bWhenFalseCanFallthrough);
    Assert_(eExpectation <= EExpectedExpr::EXPECT_REGULAR);

    // A little gymnastic with a special "on-resume" mechanism...
    LocalNodeInfoForResumedTask* pLocalResumeInfo = 0;
    if (pTCContext->bResumingCurrentStatement &&
            pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
        auto it = pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.find(pNode->uNodeIndexInStatement);
        if (it != pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.end())
            pLocalResumeInfo = it.value();
    }

    // if first operand is true, we need to branch back here to check afterwards for operand B.
    TmpStackOptiArray<u32, 24> whenFirstIsTrue(pTCContext->pWorker->tmpArena);

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Checking for first in a conjunction-with-shortcut behaved expression"), pTCContext->pWorker);

        if (pLocalResumeInfo && !pLocalResumeInfo->bTCboolBinopReachedOperandB) {
            whenFirstIsTrue.append_all(pLocalResumeInfo->jumpsToTrue);
        }
        // if first operand is false, we need to goto the false-target for the whole conjunction, hopefully bypassing eval of B.
        ETCResult checkA = typecheck_runtime_conditional(pOperandA, pTCStatement,
            // we cannot 'fallthrough' *false-cond* here even if parent expression could,
            //   since such a 'fallthrough' would lead to path for evaluation of B.
            &whenFirstIsTrue, true, EBranchKind::BRANCH_TAKEN_UNKNOWN, // ...but we may 'fallthrough' *true-cond* right to there
            pVecJumpSourcesWhenFalse, false, eKindOfFalsePath,
            pTCContext, eExpectation, uAreOperandsNegatedFromAbove);

        if (UNLIKELY(checkA == ETCResult::ETCR_WAITING)) { // unlikely or not, this should be a cold path

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "'WAITING' result while typechecking first operand of a boolean conjunction... recording locals"), pTCContext->pWorker);

            // if we're hit by a 'waiting' result here, we have here as local vars some info which is not written anywhere else,
            //   yet would need to be recalled on restarting the TC task for the whole statement, and reaching there again.
            // => we'll store that in same special map on the TCContext being halted, that what is used for "typecheck_bool_binop_as_expression()".
            Assert(should_tc_ctx_halt_on_non_success(pTCContext), // otherwise would mean we'd not even be writing to the 'halted' context
                "typecheck_bool_binop_as_expression() : registering resume info for this path assumes current context is interruptible in case of wait on statement");
            if (pLocalResumeInfo == 0) {
                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                    pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                }
                pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                    sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pNode->uNodeIndexInStatement, pLocalResumeInfo);
                *pLocalResumeInfo = {};
                pLocalResumeInfo->bTCboolBinopReachedOperandB = false;
                pLocalResumeInfo->jumpsToTrue.init(localArena);
            } else {
                Assert_(!pLocalResumeInfo->bTCboolBinopReachedOperandB);
                pLocalResumeInfo->jumpsToTrue.clear();
            }
            pLocalResumeInfo->jumpsToTrue.append_all(whenFirstIsTrue);
        }
        success_or_return_wait_or_error(checkA, pNode->pTCNode);
    }

    if (whenFirstIsTrue.size()) {
        Assert_(pTCContext->pProcResult);
        if (pLocalResumeInfo == 0 || !pLocalResumeInfo->bTCboolBinopReachedOperandB) {
            u32 uIRbeforeCheckB = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
            pTCStatement->uLastIRorGlobalTCResult = uIRbeforeCheckB;
            do_replace_jump_placeholders_to(uIRbeforeCheckB, whenFirstIsTrue, pTCContext->pRepo, pTCContext);
        }
    } // otherwise we had 'fallthrough' (or even a const 'goto false') => no need to setup a target before IR for operandB...
     
    // but we'll still **typecheck** it no matter what...

    // if runtime path ever gets here, the whole conjunction will have same final behaviour as operandB.
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Checking for second in a conjunction-with-shortcut behaved expression"), pTCContext->pWorker);

        ETCResult checkB = typecheck_runtime_conditional(pOperandB, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, eExpectation, uAreOperandsNegatedFromAbove);

        if (UNLIKELY(checkB == ETCResult::ETCR_WAITING)) { // unlikely or not, this should be a cold path
            
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "'WAITING' result while typechecking second operand of a boolean conjunction... recording locals"), pTCContext->pWorker);
            
            if (pLocalResumeInfo == 0) {
                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                    pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                }
                pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                    sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pNode->uNodeIndexInStatement, pLocalResumeInfo);
            }
            *pLocalResumeInfo = {};
            pLocalResumeInfo->bTCboolBinopReachedOperandB = true;
        }
        success_or_return_wait_or_error(checkB, pNode->pTCNode);
    }

    // The following 'only' deals with assigning value to the node... the behaviours from the condition itself have already been handled by the above.
    return on_emit_or_solve_pseudo_boolean_op(pNode, uOriginalOp, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
}

// 'disjunction with shortcut' behaviour, identical to C for (conditionA() || conditionB()) :
//     * eval conditionA() :
//          - if true, do NOT eval conditionB(). results in true.
//          - if false, eval conditionB(). results in this last eval.
ETCResult typecheck_runtime_conditional_disjunction_with_shortcut(TmpTCNode* pNode, u8 uOriginalOp,
    TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath, 
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uAreOperandsNegatedFromAbove)
{
    Assert_(uOriginalOp == ETOK_BOOL_AND || uOriginalOp == ETOK_BOOL_OR); // originally can be either, since 'disjunction' behaviour could come from an inverted AND
    Assert_(uAreOperandsNegatedFromAbove == 0u || uAreOperandsNegatedFromAbove == 1u);
    Assert_(pVecJumpSourcesWhenTrue && pVecJumpSourcesWhenFalse);
    Assert_(!bWhenTrueCanFallthrough || !bWhenFalseCanFallthrough);
    Assert_(eExpectation <= EExpectedExpr::EXPECT_REGULAR);

    // A little gymnastic with a special "on-resume" mechanism...
    LocalNodeInfoForResumedTask* pLocalResumeInfo = 0;
    if (pTCContext->bResumingCurrentStatement &&
            pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
        auto it = pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.find(pNode->uNodeIndexInStatement);
        if (it != pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.end())
            pLocalResumeInfo = it.value();
    }

    // if first operand is false, we need to branch back here to check afterwards for operand B.
    TmpStackOptiArray<u32, 24> whenFirstIsFalse(pTCContext->pWorker->tmpArena);

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Checking for first in a disjunction-with-shortcut behaved expression"), pTCContext->pWorker);

        if (pLocalResumeInfo && !pLocalResumeInfo->bTCboolBinopReachedOperandB) {
            whenFirstIsFalse.append_all(pLocalResumeInfo->jumpsToFalse);
        }
        // if first operand is true, we need to goto the true-target for the whole disjunction, hopefully bypassing eval of B.
        ETCResult checkA = typecheck_runtime_conditional(pOperandA, pTCStatement,
            // we cannot 'fallthrough' *true-cond* here even if parent expression could,
            //   since such a 'fallthrough' would lead to path for evaluation of B.
            pVecJumpSourcesWhenTrue, false, eKindOfTruePath,
            &whenFirstIsFalse, true, EBranchKind::BRANCH_TAKEN_UNKNOWN, // ...but we may 'fallthrough' *false-cond* right to there 
            pTCContext, eExpectation, uAreOperandsNegatedFromAbove);

        if (UNLIKELY(checkA == ETCResult::ETCR_WAITING)) { // unlikely or not, this should be a cold path
            
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "'WAITING' result while typechecking first operand of a boolean disjunction... recording locals"), pTCContext->pWorker);
            
            // if we're hit by a 'waiting' result here, we have here as local vars some info which is not written anywhere else,
            //   yet would need to be recalled on restarting the TC task for the whole statement, and reaching there again.
            // => we'll store that in same special map on the TCContext being halted, that what is used for "typecheck_bool_binop_as_expression()".
            Assert(should_tc_ctx_halt_on_non_success(pTCContext), // otherwise would mean we'd not even be writing to the 'halted' context
                "typecheck_bool_binop_as_expression() : registering resume info for this path assumes current context is interruptible in case of wait on statement");
            if (pLocalResumeInfo == 0) {
                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                    pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                }
                pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                    sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pNode->uNodeIndexInStatement, pLocalResumeInfo);
                *pLocalResumeInfo = {};
                pLocalResumeInfo->bTCboolBinopReachedOperandB = false;
                pLocalResumeInfo->jumpsToFalse.init(localArena);
            } else {
                Assert_(!pLocalResumeInfo->bTCboolBinopReachedOperandB);
                pLocalResumeInfo->jumpsToFalse.clear();
            }
            pLocalResumeInfo->jumpsToFalse.append_all(whenFirstIsFalse);
        }
        success_or_return_wait_or_error(checkA, pNode->pTCNode);
    }

    if (whenFirstIsFalse.size()) {
        Assert_(pTCContext->pProcResult);
        if (pLocalResumeInfo == 0 || !pLocalResumeInfo->bTCboolBinopReachedOperandB) {
            u32 uIRbeforeCheckB = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
            pTCStatement->uLastIRorGlobalTCResult = uIRbeforeCheckB;
            do_replace_jump_placeholders_to(uIRbeforeCheckB, whenFirstIsFalse, pTCContext->pRepo, pTCContext);
        }
    } // otherwise we probably had had a const 'goto true' (but also possibly a fallthrough) => no need to setup a target before IR for operandB...
    
    // but we'll still **typecheck** it no matter what...

    // if runtime path ever gets here, the whole disjunction will have same final behaviour as operandB.
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Checking for second in a disjunction-with-shortcut behaved expression"), pTCContext->pWorker);

        ETCResult checkB = typecheck_runtime_conditional(pOperandB, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, eExpectation, uAreOperandsNegatedFromAbove);

        if (UNLIKELY(checkB == ETCResult::ETCR_WAITING)) { // unlikely or not, this should be a cold path
            
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "'WAITING' result while typechecking second operand of a boolean disjunction... recording locals"), pTCContext->pWorker);
            
            if (pLocalResumeInfo == 0) {
                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                    pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                }
                pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                    sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pNode->uNodeIndexInStatement, pLocalResumeInfo);
            }
            *pLocalResumeInfo = {};
            pLocalResumeInfo->bTCboolBinopReachedOperandB = true;
        }
        success_or_return_wait_or_error(checkB, pNode->pTCNode);
    }

    // The following 'only' deals with assigning value to the node... the behaviours from the condition itself have already been handled by the above.
    return on_emit_or_solve_pseudo_boolean_op(pNode, uOriginalOp, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
}

local_func void on_conditional_known_const_bool_handle_branches(u32 uAsConstBool, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, u32 uIsNegatedFromAbove)
{
    Assert_(pVecJumpSourcesWhenTrue);
    Assert_(pVecJumpSourcesWhenFalse);
    Assert_(!bWhenTrueCanFallthrough || !bWhenFalseCanFallthrough);
    Assert_(uAsConstBool == 0u || uAsConstBool == 1u);
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);
    if (uAsConstBool ^ uIsNegatedFromAbove) { // true | not false
        if (!bWhenTrueCanFallthrough) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Known Conditionnal : Emitting GOTO for Ensured TRUE"), pTCContext->pWorker);
            u32 uGotoTrueIR = ir_emit_goto_placeholder(pTCContext->pRepo, pTCContext, eKindOfTruePath); // direct goto instead of branch
            pTCStatement->uLastIRorGlobalTCResult = uGotoTrueIR;
            pVecJumpSourcesWhenTrue->append(uGotoTrueIR);
        } else {
            // otherwise noop: ensured fallthrough 'when-true-target'
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Known Conditionnal : NOOP fallthrough TRUE"), pTCContext->pWorker);
        }
    } else {                                  // false | not true
        if (!bWhenFalseCanFallthrough) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Known Conditionnal : Emitting GOTO for Ensured FALSE"), pTCContext->pWorker);
            u32 uGotoFalseIR = ir_emit_goto_placeholder(pTCContext->pRepo, pTCContext, eKindOfFalsePath); // direct goto instead of branch
            pTCStatement->uLastIRorGlobalTCResult = uGotoFalseIR;
            pVecJumpSourcesWhenFalse->append(uGotoFalseIR);
        } else {
            // otherwise noop: ensured fallthrough 'when-false-target'
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Known Conditionnal : NOOP fallthrough FALSE"), pTCContext->pWorker);
        }
    }
}

local_func void on_conditional_runtime_bool_handle_branches(u64 uIRofBoolValue, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, u32 uIsNegatedFromAbove)
{
    Assert_(pVecJumpSourcesWhenTrue);
    Assert_(pVecJumpSourcesWhenFalse);
    Assert_(!bWhenTrueCanFallthrough || !bWhenFalseCanFallthrough);
    Assert_(ir_is_valid_param(uIRofBoolValue));
    Assert_(!ir_is_immediate(uIRofBoolValue));
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);
    static_assert(IR_INSTRFLAG_BRANCH_ON_NONZERO == (1u << 13), "shift-magic requires special flag positions to work");
    u32 uNegatedAsInstrFlag = u32(uIsNegatedFromAbove) << 13;
    if (bWhenTrueCanFallthrough) {
        Assert_(!bWhenFalseCanFallthrough);
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
            "Runtime Conditionnal : Emitting BRANCH to FALSE, leaving TRUE as fallthrough"), pTCContext->pWorker);
        // ontrue is fallthrough => we'll only emit a conditional jump to onfalse => jump when last cond is false (ie *zero*), unless our conditional jumps are negated
        u32 uBranchToFalseIR = ir_emit_branch_placeholder(uIRofBoolValue, 0x00u, 0u ^ uNegatedAsInstrFlag,
            pTCContext->pRepo, pTCContext, eKindOfFalsePath);
        pTCStatement->uLastIRorGlobalTCResult = uBranchToFalseIR;
        pVecJumpSourcesWhenFalse->append(uBranchToFalseIR);
    } else {
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Runtime Conditionnal : Emitting BRANCH to TRUE..."), pTCContext->pWorker);
            // conditional jump to ontrue => jump when last cond is true (ie *non-zero*), unless our conditional jumps are negated
            u32 uBranchToTrueIR = ir_emit_branch_placeholder(uIRofBoolValue, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO ^ uNegatedAsInstrFlag,
                pTCContext->pRepo, pTCContext, eKindOfTruePath);
            pVecJumpSourcesWhenTrue->append(uBranchToTrueIR);
            pTCStatement->uLastIRorGlobalTCResult = uBranchToTrueIR;
        }
        if (!bWhenFalseCanFallthrough) { // then, non-conditional jump to onfalse unless onfalse can fallthrough)
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Runtime Conditionnal : ...Emitting second BRANCH to FALSE"), pTCContext->pWorker);
            u32 uGotoFalseIR = ir_emit_goto_placeholder(pTCContext->pRepo, pTCContext, eKindOfFalsePath); // direct goto instead of branch
            pTCStatement->uLastIRorGlobalTCResult = uGotoFalseIR;
            pVecJumpSourcesWhenFalse->append(uGotoFalseIR);
        } else {
            // otherwise noop: ensured fallthrough 'when-false-target'
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                "Runtime Conditionnal : ...leaving FALSE as fallthrough"), pTCContext->pWorker);
        }
    }
}

local_func bool is_known_negative_integral(NodeValue* pValue)
{
    Assert_(pValue);
    Assert_(get_type_kind(pValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    if (pValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(is_value_tc_only(pValue));
        return 0uLL != (pValue->info.metaValue.knownValue.uEmbeddedValue & COMPINT_FLAG_IS_NEGATIVE);
    } else {
        const TypeInfo_Integral* pAsIntegralType = (const TypeInfo_Integral*)pValue->pType;
        Assert_(is_signed(pAsIntegralType));
        Assert_(is_value_known_non_nyka(pValue));
        switch (get_ir_format(pAsIntegralType)) {
            case 0x00u: { // 8b
                Assert_(is_value_known_embd(pValue));
                return 0 != sign8(u8(pValue->info.metaValue.knownValue.uEmbeddedValue));
            } break;
            case 0x01u: { // 16b
                Assert_(is_value_known_embd(pValue));
                return 0 != sign16(u16(pValue->info.metaValue.knownValue.uEmbeddedValue));
            } break;
            case 0x02u: { // 32b
                Assert_(is_value_known_embd(pValue));
                return 0 != sign32(u32(pValue->info.metaValue.knownValue.uEmbeddedValue));
            } break;
            case 0x03u: { // 64b
                Assert_(is_value_known_embd(pValue));
                return 0 != sign64(pValue->info.metaValue.knownValue.uEmbeddedValue);
            } break;
            case 0x04u: { // 128b
                Assert_(!is_value_known_embd(pValue));
                return 0 != sign128(*reinterpret_cast<u128*>(pValue->info.metaValue.knownValue.pPtrToRawData));
            } break;
            case 0x05u: { // 256b
                Assert_(!is_value_known_embd(pValue));
                return 0 != sign256(*reinterpret_cast<u256*>(pValue->info.metaValue.knownValue.pPtrToRawData));
            } break;
            default:
                Assume_(false);
                return false;
        }
    }
}

local_func ETCResult typecheck_eq_neq_integral_cond_or_expr(TmpTCNode* pNode, u8 uOp, u8 uIsCond, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uIsNegatedFromAbove)
{
    bool bIsEq = (uOp == ETOK_ARE_EQUAL);
    Assert_(bIsEq || uOp == ETOK_ARE_NOT_EQUAL);
    Assert_(!is_node_already_typechecked(pNode->pTCNode));
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(get_type_kind(pOperandB->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);

    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue) || ir_is_valid_param_(pOperandA->pIntrinsicValue->info.uIRandMetaFlags));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue) || ir_is_valid_param_(pOperandB->pIntrinsicValue->info.uIRandMetaFlags));
    // TODO: some comptime evals still in particular cases for nykas ?
    bool bIsKnownConstA = is_value_tc_const(pOperandA->pIntrinsicValue) && !is_value_nyka_or_has_nyka(pOperandA->pIntrinsicValue);
    bool bIsKnownConstB = is_value_tc_const(pOperandB->pIntrinsicValue) && !is_value_nyka_or_has_nyka(pOperandB->pIntrinsicValue);
    bool bBothConstant = bIsKnownConstA && bIsKnownConstB;

    if (eExpectation == EExpectedExpr::EXPECT_CONSTANT && !bBothConstant) {
        Assert_(is_value_tc_const(pOperandA->pIntrinsicValue));
        Assert_(is_value_tc_const(pOperandB->pIntrinsicValue));
        Assert_(is_value_nyka_or_has_nyka(pOperandA->pIntrinsicValue) || is_value_nyka_or_has_nyka(pOperandB->pIntrinsicValue));
        return_error(pNode, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
            "Cannot evaluate this comparison as a constant, since it contains nykas");
    }

    i32 iKnownAlready = -1;
    if (pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT] && pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
        Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));
        Assert_(bBothConstant);
        u64 uComptimePayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
        u64 uComptimePayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
        u64 uIsNegA = uComptimePayloadA & COMPINT_FLAG_IS_NEGATIVE;
        u64 uIsNegB = uComptimePayloadB & COMPINT_FLAG_IS_NEGATIVE;
        if (uIsNegA != uIsNegB) {
            iKnownAlready = bIsEq ? 0u : 1u;
            goto on_known;
        } else if ((uComptimePayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            if ((uComptimePayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
                iKnownAlready = ((uComptimePayloadA == uComptimePayloadB) == bIsEq) ? 1u : 0u;
                goto on_known;
            } else {
                // TODO
                return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_eq_neq_integral_cond_or_expr() : non-embedded compint not yet implemented");
            }
        } else {
            // TODO
            return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_eq_neq_integral_cond_or_expr() : non-embedded compint not yet implemented");
        }
    } else {
        const TypeInfo_Integral* pAsIntegralA = (const TypeInfo_Integral*)pOperandA->pIntrinsicValue->pType;
        const TypeInfo_Integral* pAsIntegralB = (const TypeInfo_Integral*)pOperandB->pIntrinsicValue->pType;
        if ((is_raw_integral(pAsIntegralA) && is_signed(pAsIntegralB)) || (is_raw_integral(pAsIntegralB) && is_signed(pAsIntegralA))) {
            if (is_signed(pAsIntegralB)) {
                if (is_value_known_non_nyka(pOperandB->pIntrinsicValue)) {
                    if (is_known_negative_integral(pOperandB->pIntrinsicValue)) {
                        return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                            "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and negative ints is forbidden. Requires explicit cast of one or the other");
                    } else {
                        // otherwise we know B is positive => we can just "consider" type of B as unsigned.
                        if (pAsIntegralB != g_pCoreTypesInfo[ECORETYPE_COMPINT])
                            pAsIntegralB = get_unsigned_type_of_same_width_from(pAsIntegralB);
                    }
                } else {
                    return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                        "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and non-comptime-evaluable signed ints is forbidden. Requires explicit cast of one or the other");
                }
            } else { Assert_(is_signed(pAsIntegralA));
                if (is_value_known_non_nyka(pOperandA->pIntrinsicValue)) {
                    if (is_known_negative_integral(pOperandA->pIntrinsicValue)) {
                        return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                            "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and negative ints is forbidden. Requires explicit cast of one or the other");
                    } else {
                        // otherwise we know A is positive => we can just "consider" type of A as unsigned.
                        if (pAsIntegralA != g_pCoreTypesInfo[ECORETYPE_COMPINT])
                            pAsIntegralA = get_unsigned_type_of_same_width_from(pAsIntegralA);
                    }
                } else {
                    return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                        "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and non-comptime-evaluable signed ints is forbidden. Requires explicit cast of one or the other");
                }
            }
        }
        bool bCastableAasB = false;
        bool bCastableBasA = false;
        if (bIsKnownConstA) {
            bool bUnused;
            if (pAsIntegralA != pAsIntegralB && is_known_integral_outside_range(pAsIntegralA, pOperandA->pIntrinsicValue->info, pAsIntegralB, pTCContext, &bUnused)) {
                // we're ensured not equal in case of known value not fitting within the other type (including negative const compared to unsigned)
                // TODO: emit warning ??
                iKnownAlready = bIsEq ? 0u : 1u;
                goto on_known;
            } else
                bCastableAasB = true;
        }
        if (bIsKnownConstB) {
            bool bUnused;
            if (pAsIntegralA != pAsIntegralB && is_known_integral_outside_range(pAsIntegralB, pOperandB->pIntrinsicValue->info, pAsIntegralA, pTCContext, &bUnused)) {
                // we're ensured not equal in case of known value not fitting within the other type (including negative const compared to unsigned)
                // TODO: emit warning ??
                iKnownAlready = bIsEq ? 0u : 1u;
                goto on_known;
            } else
                bCastableBasA = true;
        }

        TmpStackOptiArray<u32, 16u> vecJumpOverChecks;
        u32 uPosOfLocalRepresentative = 0u;
        u32 uPosOfInitialAssign = 0u;
        u64 uIRofLocalRepresentative = 0uLL;
        IRInfo infoIsNegative;
        bool bIsSignedA = is_signed(pAsIntegralA);
        bool bIsSignedB = is_signed(pAsIntegralB);
        // if one or the other is signed, non-const, while the other is unsigned, then we need to emit a *runtime* check of negativity beforehand.
        if ((bIsSignedA && !bIsKnownConstA && !bIsSignedB) || (bIsSignedB && !bIsKnownConstB && !bIsSignedA)) {
            Assert_(!bBothConstant);
            // if, moreover, we're evaluating that comparison to get a bool result outside of a conditional context with handling of branches, 
            //  then we may require an additional local variable to represent the result, since we need to check once for is negative of A,
            // then a second time between A and B.
            if (0u == uIsCond) {
                Assert_(pTCContext->pProcResult && pTCContext->pRepo == &(pTCContext->pProcResult->procwiseRepo));
                uPosOfLocalRepresentative = ir_emit_local_variable_decl(0x00u, 0u, 1u, IR_INSTRFLAG_IS_ASSIGNABLE, pTCContext->pRepo, pTCContext);
                u64 uIRofInitialValue = ir_make_int_immediate(bIsEq ? 0 : 1); // if we jump over comparison in case of negative, then we'll stay with A != B result. 
                do_store_value_to(uIRofLocalRepresentative, uIRofInitialValue, 0x00u, 1u, pTCStatement, pTCContext);
            }
            EIRResult eCheckNegative;
            if (bIsSignedA) {
                Assert_(!bIsKnownConstA);
                Assert_(!bIsSignedB);
                IRInfo info0;
                u8 uFormatA = get_ir_format(pAsIntegralA);
                if (uFormatA <= 0x03u) {
                    info0 = info0WhenEmbeddedIntegral;
                } else {
                    // TODO: some mechanism reclaiming memory from known data results & IR position, to avoid littering our IR with
                    // irrelevant derefs for min and max
                    EIRResult eSolveInfo0;
                    eSolveInfo0 = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, uFormatA, 3u, 1u,
                        1u << uFormatA, 0u, pTCStatement, pTCContext, &info0);
                    Assert_(eSolveInfo0 == EIRResult::EIRR_ENSURED_VALID_KNOWN);
                }
                eCheckNegative = ir_emit_or_solve_ord_cmp_integral(uFormatA, pOperandA->pIntrinsicValue->info, info0, 0u,
                    IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoIsNegative);
                // falling-through after that check, we can do as-if type of A was unsigned
                Assert_(pAsIntegralA != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
                pAsIntegralA = get_unsigned_type_of_same_width_from(pAsIntegralA);
            } else {
                Assert_(bIsSignedB);
                Assert_(!bIsKnownConstB);
                Assert_(!bIsSignedA);
                IRInfo info0;
                u8 uFormatB = get_ir_format(pAsIntegralB);
                if (uFormatB <= 0x03u) {
                    info0 = info0WhenEmbeddedIntegral;
                } else {
                    // TODO: some mechanism reclaiming memory from known data results & IR position, to avoid littering our IR with
                    // irrelevant derefs for min and max
                    EIRResult eSolveInfo0;
                    eSolveInfo0 = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, uFormatB, 3u, 1u,
                        1u << uFormatB, 0u, pTCStatement, pTCContext, &info0);
                    Assert_(eSolveInfo0 == EIRResult::EIRR_ENSURED_VALID_KNOWN);
                }
                eCheckNegative = ir_emit_or_solve_ord_cmp_integral(uFormatB, pOperandB->pIntrinsicValue->info, info0, 0u,
                    IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoIsNegative);
                // falling-through after that check, we can do as-if type of A was unsigned
                Assert_(pAsIntegralB != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
                pAsIntegralB = get_unsigned_type_of_same_width_from(pAsIntegralB);
            }
            // TODO: cleanup: what about nykas ??? Currently, the following assert may fail
            //   in particular cases where the solver did manage to find a result, even in their presence
            Assert_(eCheckNegative >= EIRResult::EIRR_ENSURED_VALID_UNKNOWN); // is negative should NOT be known, otherwise the signed operand would have to be const...
            if (eCheckNegative < EIRResult::EIRR_FIRST_ERROR) {
                // we're *NOT* ensured non-negative => we need to jump over the following check in case of runtime eval negative...
                Assert_(ir_is_valid_param_(infoIsNegative.uIRandMetaFlags));
                u32 uPosOfJumpIfNegative = ir_emit_branch_placeholder(infoIsNegative.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u,
                    IR_INSTRFLAG_BRANCH_ON_NONZERO, pTCContext->pRepo, pTCContext);
                Assert_(pTCContext->pProcResult);
                pTCStatement->uLastIRorGlobalTCResult = uPosOfJumpIfNegative;
                vecJumpOverChecks.append(uPosOfJumpIfNegative);
            } else {
                return_error(pNode, pTCStatement, pTCContext, u16(eCheckNegative),
                    "typecheck_eq_neq_integral_cond_or_expr() : emit or solve for check negative failed");
            }
        }

        // if we're dealing with one or the other as constants, since we already checked that the value fits in the other type,
        //   we now cast to the other type (choosing smallest if both are const... since in most cases, this should be more optimized to do so)
        const TypeInfo_Integral* pResultingType = 0; 
        if (bCastableAasB && (!bCastableBasA || pAsIntegralA == g_pCoreTypesInfo[ECORETYPE_COMPINT] || get_ir_format(pAsIntegralA) > get_ir_format(pAsIntegralB)))
            pResultingType = pAsIntegralB;
        else if (bCastableBasA)
            pResultingType = pAsIntegralA;
        else {
            // Otherwise, we find the common type as the greater of the two
            u16 uTypeErr = 0;
            pResultingType = get_resulting_integral_type(pAsIntegralA, pAsIntegralB, pTCContext,
                &uTypeErr, EIntMismatchChoice::EINT_CHOOSE_LARGEST);
            if (uTypeErr) {
                return_error(pNode, pTCStatement, pTCContext, uTypeErr,
                    "typecheck_eq_neq_integral_cond_or_expr() : find common integral type failed");
            }
        }
        Assert_(pResultingType && pResultingType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);

        ETCResult checkCastA = do_implicit_cast(pOperandA, pResultingType, pTCStatement, pTCContext, eExpectation);
        success_or_return_wait_or_error(checkCastA, pNode->pTCNode);
        ETCResult checkCastB = do_implicit_cast(pOperandB, pResultingType, pTCStatement, pTCContext, eExpectation);
        success_or_return_wait_or_error(checkCastB, pNode->pTCNode);

        // now we deal with the comparison, proper...
        IRInfo lastConditionInfo; 
        EIRResult eSolveCompare = ir_emit_or_solve_eq_cmp_integral(get_ir_format(pResultingType),
            pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, bIsEq ? 0u : IR_INSTRFLAG_CMP_OPPOSITE,
            uIsCond ? IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES : 0u, pTCStatement, pTCContext, &lastConditionInfo);

        if (eSolveCompare <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
            Assert_(bBothConstant);
            Assert_(uPosOfLocalRepresentative == 0u);
            Assert_(uPosOfInitialAssign == 0u);
            Assert_(vecJumpOverChecks.size() == 0u);
            Assert_(irflag_is_known_non_nyka(lastConditionInfo.uIRandMetaFlags) && irflag_is_known_embd(lastConditionInfo.uIRandMetaFlags));
            Assert_(lastConditionInfo.metaValue.knownValue.uEmbeddedValue <= 1uLL);
            iKnownAlready = i32(lastConditionInfo.metaValue.knownValue.uEmbeddedValue);
            goto on_known;

        } else if (eSolveCompare < EIRResult::EIRR_FIRST_ERROR) {
            Assert_(!bBothConstant);
            Assert_(0u == (lastConditionInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN));
            Assert_(0u == (lastConditionInfo.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST));
            u64 uLastConditionAsIR = lastConditionInfo.uIRandMetaFlags & IR_STD_PARAM_MASK;
            Assert_(ir_is_valid_param(uLastConditionAsIR));
            Assert_(!ir_is_immediate(uLastConditionAsIR));

            if (uIsCond) { // conditional context with handling of branches

                Assert_(uPosOfLocalRepresentative == 0u);
                Assert_(uPosOfInitialAssign == 0u);

                // positive or negated branching, over last condition taking into account intrisic eq vs neq: 
                on_conditional_runtime_bool_handle_branches(uLastConditionAsIR, pTCStatement,
                    pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                    pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                    pTCContext, uIsNegatedFromAbove);

                if (vecJumpOverChecks.size()) { // conditional context with handling of branches, with an additional check for is_negative
                    Assert_(0u == (infoIsNegative.uIRandMetaFlags & IRFLAG_IS_KNOWN));
                    u64 uIRinfoIsNegative = infoIsNegative.uIRandMetaFlags & IR_STD_PARAM_MASK;
                    Assert_(ir_is_valid_param(uIRinfoIsNegative));
                    Assert_(!ir_is_immediate(uIRinfoIsNegative));

                    // in case of jump over check, if those jumps were taken, the *equality* was ensured false.
                    // => if testing for eq, we should take the false path (unless inverted). if testing for neq, the false one.
                    //
                    // Note that we do not bother testing for possibility of fallthrough here, since taking them into account would add complexity
                    //   (and require another emitting another jump target anyway).
                    u32 uIsNeq = bIsEq ? 0u : 1u;
                    if (uIsNeq ^ uIsNegatedFromAbove) { // testing for neq (or inverted eq)
                        pVecJumpSourcesWhenFalse->append_all(vecJumpOverChecks);
                    } else {                                            // testing for eq (or inverted neq)
                        pVecJumpSourcesWhenTrue->append_all(vecJumpOverChecks);
                    }

                    // And we emit a pseudo-bool for our case:
                    u32 uPosPseudoOp = 0u;
                    if (bIsEq) { // if 'comp' tested for equality, result is notNeg && comp
                        u32 uPosNonNegative = pTCContext->pRepo->uSize;
                        IREntry* nonNegativeEntry = ir_append_new_entry(pTCContext->pRepo);
                        nonNegativeEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(ETOK_BOOL_NOT) << 16u) | uIRinfoIsNegative;
                        nonNegativeEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_PSEUDO_VALUED_COND;
                        nonNegativeEntry->metaValue._payload = 0uLL;
                        u64 uIRofNonNegative = ir_make_std_code_in_cur_proc(uPosNonNegative);
                        uPosPseudoOp = pTCContext->pRepo->uSize;
                        IREntry* pseudoOpEntry = ir_append_new_entry(pTCContext->pRepo);
                        pseudoOpEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(ETOK_BOOL_AND) << 16u) | uIRofNonNegative;
                        pseudoOpEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_PSEUDO_VALUED_COND | uLastConditionAsIR;
                        pseudoOpEntry->metaValue._payload = 0uLL;
                    } else {                            // if 'comp' tested for inequality, result is neg || comp
                        uPosPseudoOp = pTCContext->pRepo->uSize;
                        IREntry* pseudoOpEntry = ir_append_new_entry(pTCContext->pRepo);
                        pseudoOpEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(ETOK_BOOL_OR) << 16u) | uIRinfoIsNegative;
                        pseudoOpEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_PSEUDO_VALUED_COND | uLastConditionAsIR;
                        pseudoOpEntry->metaValue._payload = 0uLL;
                    }

                    Assert_(uPosPseudoOp);
                    pTCStatement->uLastIRorGlobalTCResult = uPosPseudoOp;

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info.uIRandMetaFlags = ir_make_std_code_in_cur_proc(uPosPseudoOp) | IRFLAG_IS_PSEUDO_VALUED_COND;
                    pResult->info.metaValue._payload = 0uLL;
                    return set_node_typecheck_expr_success(pNode->pTCNode);

                } else {                        // conditional context with handling of branches, when single check.

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info = lastConditionInfo;
                    return set_node_typecheck_expr_success(pNode->pTCNode);

                }

            } else {
                if (uPosOfLocalRepresentative) { // non-conditional context requiring expression, with an additional check for is_negative
                    Assert_(ir_is_valid_param(uIRofLocalRepresentative));
                    Assert_(!ir_is_immediate(uIRofLocalRepresentative));
                    Assert_(uPosOfInitialAssign);
                    Assert_(vecJumpOverChecks.size() != 0u);
                    do_store_value_to(uIRofLocalRepresentative, uLastConditionAsIR, 0x00u, 1u, pTCStatement, pTCContext);
                    u32 uPosOfJumpOverMarker = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                    do_replace_jump_placeholders_to(uPosOfJumpOverMarker, vecJumpOverChecks, pTCContext->pRepo, pTCContext);

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info.uIRandMetaFlags = uIRofLocalRepresentative;
                    pResult->info.metaValue._payload = 0uLL;
                    return set_node_typecheck_expr_success(pNode->pTCNode);
                
                } else {                         // simple expression context
                    Assert_(uPosOfInitialAssign == 0u);
                    Assert_(vecJumpOverChecks.size() == 0u);

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info = lastConditionInfo;
                    return set_node_typecheck_expr_success(pNode->pTCNode);
                }
            }

        } else {
            return_error(pNode, pTCStatement, pTCContext, u16(eSolveCompare),
                "typecheck_eq_neq_integral_cond_or_expr() : emit or solve compare failed");
        }
    }

on_known:
    Assert_(iKnownAlready == 0 || iKnownAlready == 1);
    u32 uAsConstBool = u32(iKnownAlready);

    if (uIsCond) {
        on_conditional_known_const_bool_handle_branches(uAsConstBool, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, uIsNegatedFromAbove);
    }

    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
    pResult->info.uIRandMetaFlags = ir_make_int_immediate(i32(uAsConstBool)) | IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST;
    pResult->info.metaValue.knownValue.uEmbeddedValue = u64(uAsConstBool);
    return set_node_typecheck_expr_success(pNode->pTCNode);
}

local_func ETCResult typecheck_lt_ge_integral_cond_or_expr(TmpTCNode* pNode, u8 uIsGe, u8 uIsCond, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uIsNegatedFromAbove)
{
    Assert_(uIsGe == 0u || uIsGe == 1u);
    bool bIsGe = (uIsGe == 1u);
    Assert_(!is_node_already_typechecked(pNode->pTCNode));
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(get_type_kind(pOperandB->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);

    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue) || ir_is_valid_param_(pOperandA->pIntrinsicValue->info.uIRandMetaFlags));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue) || ir_is_valid_param_(pOperandB->pIntrinsicValue->info.uIRandMetaFlags));
    // TODO: some comptime evals still in particular cases for nykas ?
    bool bIsKnownConstA = is_value_tc_const(pOperandA->pIntrinsicValue) && !is_value_nyka_or_has_nyka(pOperandA->pIntrinsicValue);
    bool bIsKnownConstB = is_value_tc_const(pOperandB->pIntrinsicValue) && !is_value_nyka_or_has_nyka(pOperandB->pIntrinsicValue);
    bool bBothConstant = bIsKnownConstA && bIsKnownConstB;

    if (eExpectation == EExpectedExpr::EXPECT_CONSTANT && !bBothConstant) {
        Assert_(is_value_tc_const(pOperandA->pIntrinsicValue));
        Assert_(is_value_tc_const(pOperandB->pIntrinsicValue));
        Assert_(is_value_nyka_or_has_nyka(pOperandA->pIntrinsicValue) || is_value_nyka_or_has_nyka(pOperandB->pIntrinsicValue));
        return_error(pNode, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
            "Cannot evaluate this comparison as a constant, since it contains nykas");
    }

    i32 iKnownAlready = -1;
    if (pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT] && pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
        Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));
        Assert_(bBothConstant);
        u64 uComptimePayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
        u64 uComptimePayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
        u64 uIsNegA = uComptimePayloadA & COMPINT_FLAG_IS_NEGATIVE;
        u64 uIsNegB = uComptimePayloadB & COMPINT_FLAG_IS_NEGATIVE;
        if (uIsNegA != uIsNegB) {
            if (bIsGe) {        // A >= B ?
                iKnownAlready = uIsNegA ? 0u : 1u;
            } else {            // A < B ?
                iKnownAlready = uIsNegA ? 1u : 0u;
            }
        } else if ((uComptimePayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            if ((uComptimePayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
                u64 uAbsA = uComptimePayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;
                u64 uAbsB = uComptimePayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
                i32 iKnownLesserThan;
                if (uIsNegA) {
                    iKnownLesserThan = uAbsA > uAbsB ? 1u : 0u;
                } else {
                    iKnownLesserThan = uAbsA < uAbsB ? 1u : 0u;
                }
                iKnownAlready = iKnownLesserThan ^ uIsGe;
                goto on_known;
            } else {
                // TODO
                return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_lt_ge_integral_cond_or_expr() : non-embedded compint not yet implemented");
            }
        } else {
            // TODO
            return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_lt_ge_integral_cond_or_expr() : non-embedded compint not yet implemented");
        }
    } else {
        const TypeInfo_Integral* pAsIntegralA = (const TypeInfo_Integral*)pOperandA->pIntrinsicValue->pType;
        const TypeInfo_Integral* pAsIntegralB = (const TypeInfo_Integral*)pOperandB->pIntrinsicValue->pType;
        if ((is_raw_integral(pAsIntegralA) && is_signed(pAsIntegralB)) || (is_raw_integral(pAsIntegralB) && is_signed(pAsIntegralA))) {
            if (is_signed(pAsIntegralB)) {
                if (is_value_known_non_nyka(pOperandB->pIntrinsicValue)) {
                    if (is_known_negative_integral(pOperandB->pIntrinsicValue)) {
                        return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                            "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and negative ints is forbidden. Requires explicit cast of one or the other");
                    } else {
                        // otherwise we know B is positive => we can just "consider" type of B as unsigned.
                        if (pAsIntegralB != g_pCoreTypesInfo[ECORETYPE_COMPINT])
                            pAsIntegralB = get_unsigned_type_of_same_width_from(pAsIntegralB);
                    }
                } else {
                    return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                        "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and non-comptime-evaluable signed ints is forbidden. Requires explicit cast of one or the other");
                }
            } else { Assert_(is_signed(pAsIntegralA));
                if (is_value_known_non_nyka(pOperandA->pIntrinsicValue)) {
                    if (is_known_negative_integral(pOperandA->pIntrinsicValue)) {
                        return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                            "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and negative ints is forbidden. Requires explicit cast of one or the other");
                    } else {
                        // otherwise we know A is positive => we can just "consider" type of A as unsigned.
                        if (pAsIntegralA != g_pCoreTypesInfo[ECORETYPE_COMPINT])
                            pAsIntegralA = get_unsigned_type_of_same_width_from(pAsIntegralA);
                    }
                } else {
                    return_error(pNode, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION,
                        "typecheck_eq_neq_integral_cond_or_expr() : comparisons between raw integral types and non-comptime-evaluable signed ints is forbidden. Requires explicit cast of one or the other");
                }
            }
        }
        bool bCastableAasB = false;
        bool bCastableBasA = false;
        if (bIsKnownConstA) {
            bool bAIsEnsuredLessThanB;
            if (pAsIntegralA != pAsIntegralB && is_known_integral_outside_range(pAsIntegralA, pOperandA->pIntrinsicValue->info, pAsIntegralB, pTCContext, &bAIsEnsuredLessThanB)) {
                // we're ensured known as either less or greater in case of known value not fitting within the other type (including negative const compared to unsigned)
                // TODO: emit warning ??
                iKnownAlready = (bAIsEnsuredLessThanB == bIsGe) ? 0u : 1u;
                goto on_known;
            } else
                bCastableAasB = true;
        }
        if (bIsKnownConstB) {
            bool bBIsEnsuredLessThanA;
            if (pAsIntegralA != pAsIntegralB && is_known_integral_outside_range(pAsIntegralB, pOperandB->pIntrinsicValue->info, pAsIntegralA, pTCContext, &bBIsEnsuredLessThanA)) {
                // we're ensured known as either less or greater in case of known value not fitting within the other type (including negative const compared to unsigned)
                // TODO: emit warning ??
                iKnownAlready = (bBIsEnsuredLessThanA == bIsGe) ? 1u : 0u;
                goto on_known;
            } else
                bCastableBasA = true;
        }

        TmpStackOptiArray<u32, 16u> vecJumpOverChecks;
        u32 uPosOfLocalRepresentative = 0u;
        u64 uIRofLocalRepresentative = 0uLL;
        IRInfo infoIsNegative;
        bool bIsSignedA = is_signed(pAsIntegralA);
        bool bIsSignedB = is_signed(pAsIntegralB);
        // if one or the other is signed, non-const, while the other is unsigned, then we need to emit a *runtime* check of negativity beforehand.
        if ((bIsSignedA && !bIsKnownConstA && !bIsSignedB) || (bIsSignedB && !bIsKnownConstB && !bIsSignedA)) {
            Assert_(!bBothConstant);
            // if, moreover, we're evaluating that comparison to get a bool result outside of a conditional context with handling of branches, 
            //  then we may require an additional local variable to represent the result, since we need to check once for is negative of A,
            // then a second time between A and B.
            if (0u == uIsCond) {
                Assert_(pTCContext->pProcResult && pTCContext->pRepo == &(pTCContext->pProcResult->procwiseRepo));
                uPosOfLocalRepresentative = ir_emit_local_variable_decl(0x00u, 0u, 1u, IR_INSTRFLAG_IS_ASSIGNABLE, pTCContext->pRepo, pTCContext);
            }
            EIRResult eCheckNegative;
            if (bIsSignedA) {
                Assert_(!bIsKnownConstA);
                Assert_(!bIsSignedB);
                u64 uIRofInitialValue = ir_make_int_immediate(bIsGe ? 0 : 1); // if we jump over comparison in case of negative A, then we'll stay with A < B result. 
                do_store_value_to(uIRofLocalRepresentative, uIRofInitialValue, 0x00u, 1u, pTCStatement, pTCContext);
                IRInfo info0;
                u8 uFormatA = get_ir_format(pAsIntegralA);
                if (uFormatA <= 0x03u) {
                    info0 = info0WhenEmbeddedIntegral;
                } else {
                    // TODO: some mechanism reclaiming memory from known data results & IR position, to avoid littering our IR with
                    // irrelevant derefs for zero
                    EIRResult eSolveInfo0;
                    eSolveInfo0 = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, uFormatA, 3u, 1u,
                        1u << uFormatA, 0u, pTCStatement, pTCContext, &info0);
                    Assert_(eSolveInfo0 == EIRResult::EIRR_ENSURED_VALID_KNOWN);
                }
                eCheckNegative = ir_emit_or_solve_ord_cmp_integral(uFormatA, pOperandA->pIntrinsicValue->info, info0, 0u,
                    IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoIsNegative);
                // falling-through after that check, we can do as-if type of A was unsigned
                Assert_(pAsIntegralA != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
                pAsIntegralA = get_unsigned_type_of_same_width_from(pAsIntegralA);
            } else {
                Assert_(bIsSignedB);
                Assert_(!bIsKnownConstB);
                Assert_(!bIsSignedA);
                IRInfo info0;
                u8 uFormatB = get_ir_format(pAsIntegralB);
                if (uFormatB <= 0x03u) {
                    info0 = info0WhenEmbeddedIntegral;
                } else {
                    // TODO: some mechanism reclaiming memory from known data results & IR position, to avoid littering our IR with
                    // irrelevant derefs for zero
                    EIRResult eSolveInfo0;
                    eSolveInfo0 = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, uFormatB, 3u, 1u,
                        1u << uFormatB, 0u, pTCStatement, pTCContext, &info0);
                    Assert_(eSolveInfo0 == EIRResult::EIRR_ENSURED_VALID_KNOWN);
                }
                u64 uIRofInitialValue = ir_make_int_immediate(bIsGe ? 1 : 0); // if we jump over comparison in case of negative B, then we'll stay with A >= B result. 
                do_store_value_to(uIRofLocalRepresentative, uIRofInitialValue, 0x00u, 1u, pTCStatement, pTCContext);
                eCheckNegative = ir_emit_or_solve_ord_cmp_integral(uFormatB, pOperandB->pIntrinsicValue->info, info0, 0u,
                    IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoIsNegative);
                // falling-through after that check, we can do as-if type of A was unsigned
                Assert_(pAsIntegralB != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
                pAsIntegralB = get_unsigned_type_of_same_width_from(pAsIntegralB);
            }
            // TODO: cleanup: what about nykas ??? Currently, the following assert may fail
            //   in particular cases where the solver did manage to find a result, even in their presence
            Assert_(eCheckNegative >= EIRResult::EIRR_ENSURED_VALID_UNKNOWN); // is negative should NOT be known, otherwise the signed operand would have to be const...
            if (eCheckNegative < EIRResult::EIRR_FIRST_ERROR) {
                // we're *NOT* ensured non-negative => we need to jump over the following check in case of runtime eval negative...
                Assert_(ir_is_valid_param_(infoIsNegative.uIRandMetaFlags));
                u32 uPosOfJumpIfNegative = ir_emit_branch_placeholder(infoIsNegative.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u,
                    IR_INSTRFLAG_BRANCH_ON_NONZERO, pTCContext->pRepo, pTCContext);
                Assert_(pTCContext->pProcResult);
                pTCStatement->uLastIRorGlobalTCResult = uPosOfJumpIfNegative;
                vecJumpOverChecks.append(uPosOfJumpIfNegative);
            } else {
                return_error(pNode, pTCStatement, pTCContext, u16(eCheckNegative),
                    "typecheck_lt_ge_integral_cond_or_expr() : emit or solve for check negative failed");
            }
        }

        // if we're dealing with one or the other as constants, since we already checked that the value fits in the other type,
        //   we now cast to the other type (choosing smallest if both are const... since in most cases, this should be more optimized to do so)
        const TypeInfo_Integral* pResultingType = 0; 
        if (bCastableAasB && (!bCastableBasA || pAsIntegralA == g_pCoreTypesInfo[ECORETYPE_COMPINT] || get_ir_format(pAsIntegralA) > get_ir_format(pAsIntegralB)))
            pResultingType = pAsIntegralB;
        else if (bCastableBasA)
            pResultingType = pAsIntegralA;
        else {
            // Otherwise, we find the common type as the greater of the two
            u16 uTypeErr = 0;
            pResultingType = get_resulting_integral_type(pAsIntegralA, pAsIntegralB, pTCContext,
                &uTypeErr, EIntMismatchChoice::EINT_CHOOSE_LARGEST);
            if (uTypeErr) {
                return_error(pNode, pTCStatement, pTCContext, uTypeErr,
                    "typecheck_lt_ge_integral_cond_or_expr() : find common integral type failed");
            }
        }
        Assert_(pResultingType && pResultingType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);

        ETCResult checkCastA = do_implicit_cast(pOperandA, pResultingType, pTCStatement, pTCContext, eExpectation);
        success_or_return_wait_or_error(checkCastA, pNode->pTCNode);
        ETCResult checkCastB = do_implicit_cast(pOperandB, pResultingType, pTCStatement, pTCContext, eExpectation);
        success_or_return_wait_or_error(checkCastB, pNode->pTCNode);

        // now we deal with the comparison, proper...
        IRInfo lastConditionInfo; 
        EIRResult eSolveCompare = ir_emit_or_solve_ord_cmp_integral(get_ir_format(pResultingType),
            pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, bIsGe ? IR_INSTRFLAG_CMP_OPPOSITE : 0u, uIsCond ? IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES : 0u,
            is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED, pTCStatement, pTCContext, &lastConditionInfo);

        if (eSolveCompare <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
            Assert_(bBothConstant);
            Assert_(uPosOfLocalRepresentative == 0u);
            Assert_(vecJumpOverChecks.size() == 0u);
            Assert_(irflag_is_known_non_nyka(lastConditionInfo.uIRandMetaFlags) && irflag_is_known_embd(lastConditionInfo.uIRandMetaFlags));
            Assert_(lastConditionInfo.metaValue.knownValue.uEmbeddedValue <= 1uLL);
            iKnownAlready = i32(lastConditionInfo.metaValue.knownValue.uEmbeddedValue);
            goto on_known;

        } else if (eSolveCompare < EIRResult::EIRR_FIRST_ERROR) {
            Assert_(!bBothConstant);
            Assert_(0u == (lastConditionInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN));
            Assert_(0u == (lastConditionInfo.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST));
            u64 uLastConditionAsIR = lastConditionInfo.uIRandMetaFlags & IR_STD_PARAM_MASK;
            Assert_(ir_is_valid_param(uLastConditionAsIR));
            Assert_(!ir_is_immediate(uLastConditionAsIR));

            if (uIsCond) { // conditional context with handling of branches

                Assert_(uPosOfLocalRepresentative == 0u);

                on_conditional_runtime_bool_handle_branches(uLastConditionAsIR, pTCStatement,
                    pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                    pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                    pTCContext, uIsNegatedFromAbove);

                if (vecJumpOverChecks.size()) { // conditional context with handling of branches, with an additional check for is_negative
                    Assert_(0u == (infoIsNegative.uIRandMetaFlags & IRFLAG_IS_KNOWN));
                    u64 uIRinfoIsNegative = infoIsNegative.uIRandMetaFlags & IR_STD_PARAM_MASK;
                    Assert_(ir_is_valid_param(uIRinfoIsNegative));
                    Assert_(!ir_is_immediate(uIRinfoIsNegative));

                    // in case of jump over check, if those jumps were taken, then either A or B was ensured negative, when the other was unsigned.
                    // Note that we do not bother testing for possibility of fallthrough here, since taking them into account would add complexity
                    //   (and require another emitting another jump target anyway).
                    u32 uTestedForNegativeWasB = bIsSignedA ? 0u : 1u;
                    // If A was the one tested for negative, ensured A < B => branch to false iff testing for ge, unless negated
                    if (uIsGe ^ uIsNegatedFromAbove ^ uTestedForNegativeWasB) {
                        pVecJumpSourcesWhenFalse->append_all(vecJumpOverChecks);
                    } else {
                        pVecJumpSourcesWhenTrue->append_all(vecJumpOverChecks);
                    }

                    // And we emit a pseudo-bool for our case:
                    u32 uPosPseudoOp = 0u;
                    // if 'comp' tested for A >= B, and neg = A<0? : result is notNeg && comp
                    // if 'comp' tested for A >= B, and neg = B<0? : result is neg || comp
                    // if 'comp' tested for A < B, and neg = A<0? : result is neg || comp
                    // if 'comp' tested for A < B, and neg = B<0? : result is notNeg && comp
                    if (uIsGe ^ uTestedForNegativeWasB) { // if 'comp' tested for A >= B, and neg = A<0? : result is notNeg && comp
                        u32 uPosNonNegative = pTCContext->pRepo->uSize;
                        IREntry* nonNegativeEntry = ir_append_new_entry(pTCContext->pRepo);
                        nonNegativeEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(ETOK_BOOL_NOT) << 16u) | uIRinfoIsNegative;
                        nonNegativeEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_PSEUDO_VALUED_COND;
                        nonNegativeEntry->metaValue._payload = 0uLL;
                        u64 uIRofNonNegative = ir_make_std_code_in_cur_proc(uPosNonNegative);
                        uPosPseudoOp = pTCContext->pRepo->uSize;
                        IREntry* pseudoOpEntry = ir_append_new_entry(pTCContext->pRepo);
                        pseudoOpEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(ETOK_BOOL_AND) << 16u) | uIRofNonNegative;
                        pseudoOpEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_PSEUDO_VALUED_COND | uLastConditionAsIR;
                        pseudoOpEntry->metaValue._payload = 0uLL;
                    } else {
                        uPosPseudoOp = pTCContext->pRepo->uSize;
                        IREntry* pseudoOpEntry = ir_append_new_entry(pTCContext->pRepo);
                        pseudoOpEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(ETOK_BOOL_OR) << 16u) | uIRinfoIsNegative;
                        pseudoOpEntry->uInstrMetaFlagsAndSecondParam = IRFLAG_IS_PSEUDO_VALUED_COND | uLastConditionAsIR;
                        pseudoOpEntry->metaValue._payload = 0uLL;
                    }

                    Assert_(uPosPseudoOp);
                    pTCStatement->uLastIRorGlobalTCResult = uPosPseudoOp;

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info.uIRandMetaFlags = ir_make_std_code_in_cur_proc(uPosPseudoOp) | IRFLAG_IS_PSEUDO_VALUED_COND;
                    pResult->info.metaValue._payload = 0uLL;
                    return set_node_typecheck_expr_success(pNode->pTCNode);

                } else {                        // conditional context with handling of branches, when single check.

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info = lastConditionInfo;
                    return set_node_typecheck_expr_success(pNode->pTCNode);

                }

            } else {
                if (uPosOfLocalRepresentative) { // non-conditional context requiring expression, with an additional check for is_negative
                    Assert_(ir_is_valid_param(uIRofLocalRepresentative));
                    Assert_(!ir_is_immediate(uIRofLocalRepresentative));
                    Assert_(vecJumpOverChecks.size() != 0u);
                    do_store_value_to(uIRofLocalRepresentative, uLastConditionAsIR, 0x00u, 1u, pTCStatement, pTCContext);
                    u32 uPosOfJumpOverMarker = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                    do_replace_jump_placeholders_to(uPosOfJumpOverMarker, vecJumpOverChecks, pTCContext->pRepo, pTCContext);

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info.uIRandMetaFlags = uIRofLocalRepresentative;
                    pResult->info.metaValue._payload = 0uLL;
                    return set_node_typecheck_expr_success(pNode->pTCNode);
                
                } else {                         // simple expression context
                    Assert_(vecJumpOverChecks.size() == 0u);

                    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
                    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
                    pResult->info = lastConditionInfo;
                    return set_node_typecheck_expr_success(pNode->pTCNode);
                }
            }

        } else {
            return_error(pNode, pTCStatement, pTCContext, u16(eSolveCompare),
                "typecheck_lt_ge_integral_cond_or_expr() : emit or solve compare failed");
        }
    }

on_known:
    Assert_(iKnownAlready == 0 || iKnownAlready == 1);
    u32 uAsConstBool = u32(iKnownAlready);

    if (uIsCond) {
        on_conditional_known_const_bool_handle_branches(uAsConstBool, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, uIsNegatedFromAbove);
    }

    NodeValue* pResult = alloc_value_for(pNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
    pResult->pType = g_pCoreTypesInfo[ECORETYPE_BOOL];
    pResult->info.uIRandMetaFlags = ir_make_int_immediate(i32(uAsConstBool)) | IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST;
    pResult->info.metaValue.knownValue.uEmbeddedValue = u64(uAsConstBool);
    return set_node_typecheck_expr_success(pNode->pTCNode);
}

// typecheck_eq_comparison_cond_or_expr:
//
// can work either within the framework of 'typecheck_runtime_conditional' (uIsCond == 1, and all additional params should be valid), or as an expression evaluation.
//
local_func ETCResult typecheck_eq_comparison_cond_or_expr(TmpTCNode* pNode, u8 uOp, u8 uIsCond, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uIsNegatedFromAbove)
{
    Assert_(uOp == ETOK_ARE_EQUAL || uOp == ETOK_ARE_NOT_EQUAL);
    Assert_(!is_node_already_typechecked(pNode->pTCNode));
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);

    if_expr_already_typechecked_phase1_recall_value_and_return_success(pNode, pTCStatement, pTCContext);

    bool bIsNumericA, bIsIntegralA, bIsVecOfNumericA, bIsVecOfIntegralA;
    bool bIsPointerA, bIsBoolA, bIsCodePointA, bIsStringA;
    get_common_type_flags(pOperandA->pIntrinsicValue->pType, &bIsNumericA, &bIsIntegralA,
        &bIsVecOfNumericA, &bIsVecOfIntegralA,
        &bIsPointerA, &bIsBoolA, &bIsCodePointA, &bIsStringA);

    bool bIsNumericB, bIsIntegralB, bIsVecOfNumericB, bIsVecOfIntegralB;
    bool bIsPointerB, bIsBoolB, bIsCodePointB, bIsStringB;
    get_common_type_flags(pOperandB->pIntrinsicValue->pType, &bIsNumericB, &bIsIntegralB,
        &bIsVecOfNumericB, &bIsVecOfIntegralB,
        &bIsPointerB, &bIsBoolB, &bIsCodePointB, &bIsStringB);

    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue) || ir_is_valid_param_(pOperandA->pIntrinsicValue->info.uIRandMetaFlags));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue) || ir_is_valid_param_(pOperandB->pIntrinsicValue->info.uIRandMetaFlags));

    bool bIsKnownConstA = is_value_tc_const(pOperandA->pIntrinsicValue);
    bool bIsKnownConstB = is_value_tc_const(pOperandB->pIntrinsicValue);
    bool bBothConstant = bIsKnownConstA && bIsKnownConstB;
    Assert_(eExpectation == EExpectedExpr::EXPECT_REGULAR || bBothConstant); // our expectation was passed on to Tc of each params => neither should be non-const if we expect const.
    Assert_(bBothConstant || does_tc_ctx_allow_runtime(pTCContext));
    Assert_(bBothConstant || get_tc_ctx_proc_result(pTCContext) != 0);
    Assert_(bBothConstant || pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);

    if (bIsIntegralA && bIsIntegralB) {
        return typecheck_eq_neq_integral_cond_or_expr(pNode, uOp, uIsCond, pOperandA, pOperandB, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, eExpectation, uIsNegatedFromAbove);
    } else if (bIsNumericA && bIsNumericB) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_eq_comparison_cond_or_expr() : floating point not yet implemented");
    } else if (bIsPointerA || bIsPointerB) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_eq_comparison_cond_or_expr() : comparisons involving pointer types not yet implemented");
    } else if (bIsStringA || bIsStringB) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_eq_comparison_cond_or_expr() : comparisons involving string types not yet implemented");
    } else {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_eq_comparison_cond_or_expr() : non-numeric or pointer or string not yet implemented");
    }
}

// typecheck_eq_comparison_cond_or_expr:
//
// can work either within the framework of 'typecheck_runtime_conditional' (uIsCond == 1, and all additional params should be valid), or as an expression evaluation.
//
local_func ETCResult typecheck_ord_comparison_cond_or_expr(TmpTCNode* pNode, u8 uOp, u8 uIsCond, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uIsNegatedFromAbove)
{
    Assert_(uOp == ETOK_LESSER_THAN || uOp == ETOK_LESSER_OR_EQ || uOp == ETOK_GREATER_THAN || uOp == ETOK_GREATER_OR_EQ);
    Assert_(!is_node_already_typechecked(pNode->pTCNode));
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);

    if_expr_already_typechecked_phase1_recall_value_and_return_success(pNode, pTCStatement, pTCContext);

    // Note: even if all 4 cases could technically solve as 'lt' ("simply" reverting the check afterwards to represent 'ge'),
    //   we prefer to keep a dichotomy between lt and ge, so that we can more easily represent the *intrinsic value* of the expression
    //   wrt. the emitted IR instruction.
    // Also, the fact that lt(A, B) == !ge(B, A) is not even the case for IEEE754 floats in the presence of NaNs...
    u8 uIsGe = 0u;
    bool bSwapOperands = false;

    if (uOp == ETOK_GREATER_THAN) {                 // A > B == lt(B, A)
        bSwapOperands = true;
    } else if (uOp == ETOK_LESSER_OR_EQ) {          // A <= B == ge(B, A)
        uIsGe = 1u;
        bSwapOperands = true;
    } else if (uOp == ETOK_GREATER_OR_EQ) {         // A >= B == ge(A, B)
        uIsGe = 1u;
    } else                                          // A < B == lt(A, B)
        Assert_(uOp == ETOK_LESSER_THAN);

    if (bSwapOperands) {
        TmpTCNode* pTmp = pOperandA;
        pOperandA = pOperandB;
        pOperandB = pTmp;
    }

    bool bIsNumericA, bIsIntegralA, bIsVecOfNumericA, bIsVecOfIntegralA;
    bool bIsPointerA, bIsBoolA, bIsCodePointA, bIsStringA;
    get_common_type_flags(pOperandA->pIntrinsicValue->pType, &bIsNumericA, &bIsIntegralA,
        &bIsVecOfNumericA, &bIsVecOfIntegralA,
        &bIsPointerA, &bIsBoolA, &bIsCodePointA, &bIsStringA);

    bool bIsNumericB, bIsIntegralB, bIsVecOfNumericB, bIsVecOfIntegralB;
    bool bIsPointerB, bIsBoolB, bIsCodePointB, bIsStringB;
    get_common_type_flags(pOperandB->pIntrinsicValue->pType, &bIsNumericB, &bIsIntegralB,
        &bIsVecOfNumericB, &bIsVecOfIntegralB,
        &bIsPointerB, &bIsBoolB, &bIsCodePointB, &bIsStringB);

    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue) || ir_is_valid_param_(pOperandA->pIntrinsicValue->info.uIRandMetaFlags));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue) || ir_is_valid_param_(pOperandB->pIntrinsicValue->info.uIRandMetaFlags));

    bool bIsKnownConstA = is_value_tc_const(pOperandA->pIntrinsicValue);
    bool bIsKnownConstB = is_value_tc_const(pOperandB->pIntrinsicValue);
    bool bBothConstant = bIsKnownConstA && bIsKnownConstB;
    Assert_(eExpectation == EExpectedExpr::EXPECT_REGULAR || bBothConstant); // our expectation was passed on to Tc of each params => neither should be non-const if we expect const.
    Assert_(bBothConstant || does_tc_ctx_allow_runtime(pTCContext));
    Assert_(bBothConstant || get_tc_ctx_proc_result(pTCContext) != 0);
    Assert_(bBothConstant || pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);

    if (bIsIntegralA && bIsIntegralB) {
        return typecheck_lt_ge_integral_cond_or_expr(pNode, uIsGe, uIsCond, pOperandA, pOperandB, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, eExpectation, uIsNegatedFromAbove);
    } else if (bIsNumericA && bIsNumericB) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_ord_comparison_cond_or_expr() : floating point not yet implemented");
    } else if (bIsPointerA || bIsPointerB) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_ord_comparison_cond_or_expr() : comparisons involving pointer types not yet implemented");
    } else if (bIsStringA || bIsStringB) {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_ord_comparison_cond_or_expr() : comparisons involving string types not yet implemented");
    } else {
        // TODO
        return_error(pNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_ord_comparison_cond_or_expr() : non-numeric or pointer or string not yet implemented");
    }
}


// typecheck_runtime_conditional
// 
// 'runtime' here refers to the fact that the branching *statement* at the base of this evaluation is runtime, ie,
//  not flagged for forced comptime evaluation. Hence both paths will be typechecked, even if value of conditional happens to be
//  constant and some path would get dead-code-elim afterwards (expression can *even* be forced to constness by, eg, a
//  comptime flag before a prens-expr... yet we'd still be 'runtime' wrt. the typecheck of control-flow).
//
// TODO: IMPORTANT: think about that runtime/comptime dichotomy some more, wrt. the question of constant-evals being safeguarded by conditionals,
//   in a context where the constant is an identifier which *may not be* constant in other cases: eg, 'if (x != 0) y := 5 /% x', when 
//   x is a function param and we'll be evaluating that function as *inlined with a known x=0*...
//   atm this could trigger a compilation error, since we may try to evaluate 5 /% 0 as const... ??
// SOLUTION for this particular case: do not evaluate params of inlined as consts at IR emission time: simply emit IR to store const value to their aliases,
//   and hope for the optimizer to do its job afterwards :x
//
local_func ETCResult typecheck_runtime_conditional(TmpTCNode* pNode, TCStatement* pTCStatement,
    TmpArray<u32>* pVecJumpSourcesWhenTrue, bool bWhenTrueCanFallthrough, EBranchKind eKindOfTruePath,
    TmpArray<u32>* pVecJumpSourcesWhenFalse, bool bWhenFalseCanFallthrough, EBranchKind eKindOfFalsePath,
    TCContext* pTCContext, EExpectedExpr eExpectation, u32 uIsNegatedFromAbove)
{
    Assert_(uIsNegatedFromAbove == 0u || uIsNegatedFromAbove == 1u);
    Assert_(pVecJumpSourcesWhenTrue && pVecJumpSourcesWhenFalse);
    Assert_(!bWhenTrueCanFallthrough || !bWhenFalseCanFallthrough);
    Assert_(eExpectation <= EExpectedExpr::EXPECT_REGULAR);

    if_expr_already_typechecked_phase1_recall_value_and_return_success(pNode, pTCStatement, pTCContext);

restart_if_macro:
    u8 uBroadKind = u8(pNode->pTCNode->ast.uNodeKindAndFlags);

    if (uBroadKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
        u8 uOp = u8(pNode->pTCNode->ast.uNodeKindAndFlags >> 8);
        Assert_(uOp == ETOK_OPENING_PARENS);
        // proc invocation 'forms' are to be treated separately, since they could very well be macros and require expansion...
        u32 uNodeIndex = pNode->uNodeIndexInStatement;
        bool bWasMacroExpansion = false;
        ETCResult checkInvoc = typecheck_invocation_form(pNode, pTCStatement,
            pTCContext, eExpectation, infer_type(g_pCoreTypesInfo[ECORETYPE_BOOL]),
            INVALID_NODE_INDEX, 0, 0, EInvocFormResultCount::EINVOC_RETURNS_ONE, &bWasMacroExpansion);
        if (checkInvoc == ETCResult::ETCR_SUCCESS) {
            if (bWasMacroExpansion) {
                *pNode = {};
                pNode->uNodeIndexInStatement = uNodeIndex;
                pNode->pTCNode = pTCStatement->vecNodes[uNodeIndex];
                goto restart_if_macro;
            } else
                goto on_has_typechecked_an_expression;
        } else
            return checkInvoc;

    } else {

        switch(uBroadKind) {

            case ENodeKind::ENODE_EXPR_BOOL_NOT: {
                // special treatment for 'not', so that we can keep our condition-related vecs of jumps at hand
                Assert_(u8(pNode->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_BOOL_NOT);
                TmpTCNode exprToInvert = init_tmp_tc_node(pNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                ETCResult checkToInvert = typecheck_runtime_conditional(&exprToInvert, pTCStatement,
                    pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                    pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                    pTCContext, eExpectation, uIsNegatedFromAbove ^ 1u);
                if (checkToInvert == ETCResult::ETCR_SUCCESS) {
                    return on_emit_or_solve_pseudo_boolean_op(pNode, ETOK_BOOL_NOT, &exprToInvert, 0u, pTCStatement, pTCContext, eExpectation);
                } otherwise_return_wait_or_error(checkToInvert, pNode->pTCNode);
            } break;

            case ENodeKind::ENODE_EXPR_BOOL_BINARYOP: {
                // special treatment for 'and' and 'or', since they'll interact with the condition-related vecs of jumps in special ways
                u8 uOp = u8(pNode->pTCNode->ast.uNodeKindAndFlags >> 8);
                bool isOpAnd = (uOp == ETOK_BOOL_AND);
                Assert_(isOpAnd || uOp == ETOK_BOOL_OR);
                TmpTCNode operandA = init_tmp_tc_node(pNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                TmpTCNode operandB = init_tmp_tc_node(pNode->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
                bool isNotInverted = (uIsNegatedFromAbove == 0u);
                if (isOpAnd == isNotInverted) {
                    return typecheck_runtime_conditional_conjunction_with_shortcut(pNode, uOp, &operandA, &operandB, pTCStatement,
                            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                            pTCContext, eExpectation, uIsNegatedFromAbove);
                } else {
                    return typecheck_runtime_conditional_disjunction_with_shortcut(pNode, uOp, &operandA, &operandB, pTCStatement,
                            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                            pTCContext, eExpectation, uIsNegatedFromAbove);
                }
            } break;

            case ENodeKind::ENODE_EXPR_PARENTISED: {
                // special treatment for parens, so that we can keep our condition-related vecs of jumps at hand
                TmpTCNode exprWithin = init_tmp_tc_node(pNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                if (is_comptime_prefixed(&(pNode->pTCNode->ast))) {
                    // comptime-prefix => evaluate as constant expression, and goto on has typechecked expression
                    ETCResult checkWithin = typecheck_expression(&exprWithin, pTCStatement, pTCContext,
                        EExpectedExpr::EXPECT_CONSTANT, infer_type(g_pCoreTypesInfo[ECORETYPE_BOOL]));
                    if (checkWithin == ETCResult::ETCR_SUCCESS) {
                        set_tc_success_with_same_value_as_intrinsic_of(&exprWithin, pNode);
                        goto on_has_typechecked_an_expression;
                    } otherwise_return_wait_or_error(checkWithin, pNode->pTCNode);
                } else {
                    // otherwise, pass the conditional treatment to expr within directly
                    ETCResult checkWithin = typecheck_runtime_conditional(&exprWithin, pTCStatement,
                        pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                        pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                        pTCContext, eExpectation, uIsNegatedFromAbove);
                    if (checkWithin == ETCResult::ETCR_SUCCESS) {
                        return set_tc_success_with_same_value_as_intrinsic_of(&exprWithin, pNode);
                    } otherwise_return_wait_or_error(checkWithin, pNode->pTCNode);
                }
            } break;


            case ENodeKind::ENODE_EXPR_EQ_CMP_BINARYOP: {
                // special treatment for eq/neq, so that we can keep our condition-related vecs of jumps at hand
                u8 uOp = u8(pNode->pTCNode->ast.uNodeKindAndFlags >> 8);
                Assert_(uOp == ETOK_ARE_EQUAL || uOp == ETOK_ARE_NOT_EQUAL);
                TmpTCNode operandA = init_tmp_tc_node(pNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                TmpTCNode operandB = init_tmp_tc_node(pNode->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
                ETCResult eCheckA = typecheck_expression(&operandA, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckA, pNode->pTCNode);
                ETCResult eCheckB = typecheck_expression(&operandB, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckB, pNode->pTCNode);
                return typecheck_eq_comparison_cond_or_expr(pNode, uOp, 1u, &operandA, &operandB, pTCStatement,
                    pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                    pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                    pTCContext, eExpectation, uIsNegatedFromAbove);
            } break;

            case ENodeKind::ENODE_EXPR_ORD_CMP_BINARYOP: {
                // special treatment for ordered comparisons, so that we can keep our condition-related vecs of jumps at hand
                u8 uOp = u8(pNode->pTCNode->ast.uNodeKindAndFlags >> 8);
                Assert_(uOp == ETOK_LESSER_THAN || uOp == ETOK_LESSER_OR_EQ || uOp == ETOK_GREATER_THAN || uOp == ETOK_GREATER_OR_EQ);
                TmpTCNode operandA = init_tmp_tc_node(pNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                TmpTCNode operandB = init_tmp_tc_node(pNode->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
                ETCResult eCheckA = typecheck_expression(&operandA, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckA, pNode->pTCNode);
                ETCResult eCheckB = typecheck_expression(&operandB, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
                success_or_return_wait_or_error(eCheckB, pNode->pTCNode);
                return typecheck_ord_comparison_cond_or_expr(pNode, uOp, 1u, &operandA, &operandB, pTCStatement,
                    pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
                    pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
                    pTCContext, eExpectation, uIsNegatedFromAbove);
            } break;

            // otherwise we typecheck as expression
            default: { check_other_exprs:
                ETCResult checkExpr = typecheck_any_non_invoc_expression(pNode, uBroadKind, pTCStatement,
                    pTCContext, eExpectation, infer_type(g_pCoreTypesInfo[ECORETYPE_BOOL]));
                if (checkExpr == ETCResult::ETCR_SUCCESS) {
                    goto on_has_typechecked_an_expression;
                } otherwise_return_wait_or_error(checkExpr, pNode->pTCNode);
            }                
        }
    }

on_has_typechecked_an_expression:
    Assert_(is_node_already_typechecked(pNode->pTCNode));
    if (pNode->pIntrinsicValue->pType != g_pCoreTypesInfo[ECoreType::ECORETYPE_BOOL]) {
        return_error(pNode, pTCStatement, pTCContext, CERR_EXPECTED_BOOLEAN,
            "typecheck_runtime_conditional() : expected boolean expression");
    }

    // once we evaluated as an expression, we handle branching afterwards here:

    if (is_value_known_or_nyka(pNode->pIntrinsicValue)) { // in case of known consts, we'll use direct gotos or ensured fallthroughs instead of branches.
        Assert_(!is_value_nyka_or_has_nyka(pNode->pIntrinsicValue));
        Assert_(is_value_known_embd(pNode->pIntrinsicValue));
        Assert_(pNode->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue <= 1uLL);
        // the value we have here is intrinsic => 1 represents true, 0 represents false ; negated from above *not* taken into account.
        u32 uAsConstBool = u32(pNode->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue);
        on_conditional_known_const_bool_handle_branches(uAsConstBool, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, uIsNegatedFromAbove);
    } else {
        // the value we have here is intrinsic => 1 represents true, 0 represents false ; negated from above *not* taken into account.
        u64 uValueIR = pNode->pIntrinsicValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK;
        on_conditional_runtime_bool_handle_branches(uValueIR, pTCStatement,
            pVecJumpSourcesWhenTrue, bWhenTrueCanFallthrough, eKindOfTruePath,
            pVecJumpSourcesWhenFalse, bWhenFalseCanFallthrough, eKindOfFalsePath,
            pTCContext, uIsNegatedFromAbove);
    }

    return ETCResult::ETCR_SUCCESS; // expression value as already assigned...
}


local_func ETCResult typecheck_return_statement(TmpTCNode* pMainNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(u8(pMainNode->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ST_CONTROL_FLOW &&
            u8(pMainNode->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_RETURN);
    Assert_(pTCContext->pProcSource);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'return' statement"), pTCContext->pWorker);

    const TypeInfo_ProcLike* pProcSign = pTCContext->pProcSource->procSign;
    u8 uOutParamsCount = get_output_param_count(pProcSign);

    if (is_ctx_proc_expansion(pTCContext)) {
        // TODO
        return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_return_statement() : not yet implemented for proc expansions");
    }

    if (pTCContext->pProcResult->uIsForeignSource) { // Returns needs special treatment WRT 'foreign' not-alone detection
        // ... to distinguish its IR from last automatic 'RET' at the end of typecheckinf a proc.
        return_error(pMainNode, pTCStatement, pTCContext, CERR_FOREIGN_SPECIFICATION_SHALL_BE_ALONE,
            "tc_foreign_builtin() : found 'return' op after 'foreign'");
    }

    if (pMainNode->pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
        TmpTCNode tAllRetValues[32];
        u8 uNodeCount = 0;
        // TODO: allow for totally vanishing node-count-macros ?
        ETCResult checkRetValues = typecheck_possible_expr_list(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex,
            pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR, false, tAllRetValues, &uNodeCount);
        success_or_return_wait_or_error(checkRetValues, pMainNode->pTCNode);
        if (uNodeCount == uOutParamsCount) {
            u8 uInParamsCount = get_input_param_count(pProcSign);
            for (u8 uRetNode = 0; uRetNode < uNodeCount; uRetNode++) {
                TmpTCNode* pRetNode = tAllRetValues + uRetNode;
                Assert_(is_node_already_typechecked(pRetNode->pTCNode));
                const ProcLikeParam* pDeclParam = pProcSign->params.cat(uRetNode + uInParamsCount);
                // TODO: CLEANUP: also check always reify by non-ref here ??
                ETCResult checkCast = do_implicit_cast(pRetNode, pDeclParam->pBinding->pType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_REGULAR);
                success_or_return_wait_or_error(checkCast, pMainNode->pTCNode);
                Assert_(is_node_already_type_casted(pRetNode->pTCNode));
                do_store_value_to(pDeclParam->pBinding->info.uIRandMetaFlags & IR_STD_PARAM_MASK,
                    pRetNode->pFinalValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK,
                    get_ir_format(pDeclParam->pBinding->pType), get_slots_count(pDeclParam->pBinding->pType), pTCStatement, pTCContext);
            }
        } else if (uNodeCount < uOutParamsCount) {
            return_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_FEW_RETURN_ARGUMENTS,
                "typecheck_return_statement() : too few return arguments for this proclike signature (default ret values TODO ?)");
        } else {
            return_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_MANY_RETURN_ARGUMENTS,
                "typecheck_return_statement() : too many return arguments for this proclike signature");
        }
    } else {
        if (uOutParamsCount) {
            return_error(pMainNode, pTCStatement, pTCContext, CERR_TOO_FEW_RETURN_ARGUMENTS,
                "typecheck_return_statement() : bare return ending proc with no return (default ret values TODO ?)");
        }
    }

    Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);
    TCSeqSourceBlock* pCurrentAsSeq = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;

    TCSeqSourceBlock* pCurrentOrAncestorAsSeq = pCurrentAsSeq;
    do {
        if (pCurrentOrAncestorAsSeq->pParentBlock) { // if we're not last
            // TODO: emission of markers for scope anyway ???
            if (pCurrentAsSeq->vecDeferredBlocksInDeclOrder.size()) {
                // TODO
                return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_return_statement() : return jumping above non-root-scope-deferred blocks not yet implemented");
            }
        }
        pCurrentOrAncestorAsSeq = (TCSeqSourceBlock*)pCurrentOrAncestorAsSeq->pParentBlock;
    } while(pCurrentOrAncestorAsSeq);

    // if we're not jumping through several defers (impl scheme TODO) but we still have defers here,
    //   then we're in the case where defer chain is already positionned to a final 'ret', so we simply need to branch to last defer
    u32 uDeferCount = pCurrentAsSeq->vecDeferredBlocksInDeclOrder.size();
    if (uDeferCount) {
        u32 uLastDefer = uDeferCount - 1u;
        // forcing-as-if there was an explicit goto-last-defer-with-default-exit statement at the end of this block
        u32 uGotoIR = ir_emit_goto(pCurrentAsSeq->vecDeferredBlocksInDeclOrder[uLastDefer]->_uBlockOpeningIRIffSeq,
            pTCContext->pRepo, pTCContext, EBranchKind::BRANCH_TAKEN_TO_DEFAULT_DEFER);
        pTCStatement->uLastIRorGlobalTCResult = uGotoIR;
    } else {
        u32 uRetPos = ir_emit_return(pTCContext->pRepo, pTCContext);
        pTCStatement->uLastIRorGlobalTCResult = uRetPos;
    }

    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
}

// typechecks a binary boolean operator ('and' or 'or'), as either a conjunction or disjunction with shortcut (accounting for
//   opposite by negation, already computed as 'isConjunction' here) when we *really* want the result of that operator to be
//   evaluated as an expression (ie, usually outside of a conditional control-flow). The implementation may, in effect, be forced
//   to reinterpret that evaluation as conditional control-flow gatekeeping the assignment to a temporary, implicit 'variable'.
local_func ETCResult typecheck_bool_binop_as_expression(TmpTCNode* pExpr, bool isConjunction,
    TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "typecheck_bool_binop_as_expression() : not yet implemented");
}

local_func ETCResult typecheck_runtime_ternary_if_expression(TmpTCNode* pExpr, TmpTCNode* pCondition,
    TmpTCNode* pExprWhenTrue, TmpTCNode* pExprWhenFalse, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode));

    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "typecheck_runtime_ternary_if_expression() : not yet implemented");
}

// At statement level, typechecks control-flow statements
local_func ETCResult typecheck_control_flow_statement(TmpTCNode* pMainNode, u8 uNodeKind,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(u8(pMainNode->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ST_CONTROL_FLOW);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Main Node of a control-flow statement"), pTCContext->pWorker);

    if (!is_comptime_prefixed(&(pMainNode->pTCNode->ast))) {

        if (pTCContext->uFlags & CTXFLAG_ALLOW_RUNTIME) {

            Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);
            TCSeqSourceBlock* pCurrentAsSeq = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;
            SourceFileDescAndState* pSourceFile = pTCContext->pIsolatedSourceFile;

            u8 uCtKind = u8(pMainNode->pTCNode->ast.uNodeKindAndFlags >> 8);

            switch(uCtKind) {

                case ETOK_RETURN: {
                    return typecheck_return_statement(pMainNode, pTCStatement, pTCContext);
                } break;

                case ETOK_NO_OP: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'noop' statement"), pTCContext->pWorker);

                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                } break;

                case ETOK_BREAK: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'break' statement"), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : 'break' statement not yet implemented");
                } break;

                case ETOK_CONTINUE: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'continue' statement"), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : 'continue' statement not yet implemented");
                } break;

                case ETOK_COLON: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'label' statement"), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : 'label' statement not yet implemented");
                } break;

                case ETOK_IF: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'if' statement"), pTCContext->pWorker);

                    TmpTCNode conditionNode = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);

                    TmpStackOptiArray<u32, 24> vecPlaceholdersToTrue(pTCContext->pWorker->tmpArena);
                    TmpStackOptiArray<u32, 24> dummyVecPlaceholdersToFalseWhenNoChild(pTCContext->pWorker->tmpArena);
                    TmpArray<u32>* pWhenFalse = &dummyVecPlaceholdersToFalseWhenNoChild;
                    if (pTCStatement->pChildBlock) { // in case there is a child block missing error, we still allow to tc the condition
                        u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                        TCSeqSourceBlock* pChildAsSeq;
                        if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                            u32 uAstBlockIndex = u32(uTagged >> 2); // when so, its ast block index is encoded in bits 2..33
                            // And we spawn it now...
                            pChildAsSeq = tc_alloc_and_init_seq_block(uAstBlockIndex, pCurrentAsSeq,
                                pTCContext->pCurrentBlock->uStatementBeingTypechecked, pTCContext);
                            // And it shall have a vector to else
                            pChildAsSeq->pVecPlaceholdersToElse = (TmpArray<u32>*)alloc_from(pSourceFile->localArena,
                                sizeof(TmpArray<u32>), alignof(TmpArray<u32>));
                            pChildAsSeq->pVecPlaceholdersToElse->init(pSourceFile->localArena);
                            // And we now can assign it as a real pointer instead of tagged id.
                            pTCStatement->pChildBlock = pChildAsSeq;
                        } else {
                            pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                            Assert_(pChildAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);
                            Assert_(pChildAsSeq->pVecPlaceholdersToElse);
                        }
                        pWhenFalse = pChildAsSeq->pVecPlaceholdersToElse;
                        Assert_(pWhenFalse);
                    }

                    {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                            "Now typechecking condition for the 'if' statement"), pTCContext->pWorker);

                        // A little gymnastic with a special "on-resume" mechanism...
                        LocalNodeInfoForResumedTask* pLocalResumeInfo = 0;
                        if (pTCContext->bResumingCurrentStatement &&
                                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                            auto it = pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.find(pMainNode->uNodeIndexInStatement);
                            if (it != pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.end())
                                pLocalResumeInfo = it.value();
                        }

                        if (pLocalResumeInfo) {
                            vecPlaceholdersToTrue.append_all(pLocalResumeInfo->jumpsToTrue);
                        }

                        ETCResult checkCond = typecheck_runtime_conditional(&conditionNode, pTCStatement,
                            &vecPlaceholdersToTrue, true, EBranchKind::BRANCH_TAKEN_IF,
                            pWhenFalse, false, EBranchKind::BRANCH_TAKEN_ELSE,
                            pTCContext, EExpectedExpr::EXPECT_REGULAR, 0u);

                        if (UNLIKELY(checkCond == ETCResult::ETCR_WAITING)) {

                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                                "'WAITING' result while typechecking condition of if-statement... recording locals"), pTCContext->pWorker);
                            Assert(should_tc_ctx_halt_on_non_success(pTCContext), // otherwise would mean we'd not even be writing to the 'halted' context
                                "typechecking else statement : registering resume info for this path assumes current context is interruptible in case of wait on statement");
                            if (pLocalResumeInfo == 0) {
                                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                                if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                                    pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                                }
                                pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                                    sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pMainNode->uNodeIndexInStatement, pLocalResumeInfo);
                                *pLocalResumeInfo = {};
                                pLocalResumeInfo->jumpsToTrue.init(localArena);
                            } else {
                                pLocalResumeInfo->jumpsToTrue.clear();
                            }
                            pLocalResumeInfo->jumpsToTrue.append_all(vecPlaceholdersToTrue);
                        }

                        success_or_return_wait_or_error(checkCond, pMainNode->pTCNode);
                    }

                    if (pTCStatement->pChildBlock) {
                        TCSeqSourceBlock* pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                        pCurrentAsSeq->pNextTcBlockAfterCurrentStatement = pChildAsSeq;
                        pChildAsSeq->_uBlockOpeningIRIffSeq = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                        if (vecPlaceholdersToTrue.size()) {
                            do_replace_jump_placeholders_to(pChildAsSeq->_uBlockOpeningIRIffSeq,
                                vecPlaceholdersToTrue, pTCContext->pRepo, pTCContext);
                        }
                        ir_emit_marker_start_scope(pTCContext->pRepo, pTCContext); // must follow '_uBlockOpeningIRIffSeq'
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                    } else {
                        // now only, we may "remember" there was a child-block-missing error
                        // if was a simple 'if', we may count on the already-reported parser error...
                        // but that 'if' could also have been spawned from a macro expansion,
                        //    in which case we did not necessarily detect that there was a child block required (and thus missing) here
                        //    => we need to report it now...
                        return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                            "typechecker error for control-flow statement missing child block");
                    }

                } break;

                case ETOK_ELIF: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'elif' statement"), pTCContext->pWorker);
                    
                    if (LIKELY(pCurrentAsSeq->uStatementBeingTypechecked > 0)) {
                        u32 uIndexOfPreviousStatement = pCurrentAsSeq->uStatementBeingTypechecked - 1u;
                        TCStatement* pPreviousStatement = pCurrentAsSeq->vecStatements[uIndexOfPreviousStatement];
                        if (LIKELY(pPreviousStatement->pChildBlock)) {
                            TCSeqSourceBlock* pPreviousChildAsSeq = (TCSeqSourceBlock*)pPreviousStatement->pChildBlock;
                            if (LIKELY(pPreviousChildAsSeq->pVecPlaceholdersToElse)) {
                            
                                TmpTCNode conditionNode = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);

                                TmpStackOptiArray<u32, 24> vecPlaceholdersToTrue(pTCContext->pWorker->tmpArena);
                                TmpStackOptiArray<u32, 24> dummyVecPlaceholdersToFalseWhenNoChild(pTCContext->pWorker->tmpArena);
                                TmpArray<u32>* pWhenFalse = &dummyVecPlaceholdersToFalseWhenNoChild;
                                if (pTCStatement->pChildBlock) { // in case there is a child block missing error, we still allow to tc the condition
                                    u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                                    TCSeqSourceBlock* pChildAsSeq;
                                    if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                                        u32 uAstBlockIndex = u32(uTagged >> 2); // when so, its ast block index is encoded in bits 2..33
                                        // And we spawn it now...
                                        pChildAsSeq = tc_alloc_and_init_seq_block(uAstBlockIndex, pCurrentAsSeq,
                                            pCurrentAsSeq->uStatementBeingTypechecked, pTCContext);
                                        // And it shall have a vector to else
                                        pChildAsSeq->pVecPlaceholdersToElse = (TmpArray<u32>*)alloc_from(pSourceFile->localArena,
                                            sizeof(TmpArray<u32>), alignof(TmpArray<u32>));
                                        pChildAsSeq->pVecPlaceholdersToElse->init(pSourceFile->localArena);
                                        pChildAsSeq->uKindFlagsOfParentStatement |= BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND; // an elif is else-kind
                                        // And we now can assign it as a real pointer instead of tagged id.
                                        pTCStatement->pChildBlock = pChildAsSeq;
                                    } else {
                                        pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                                        Assert_(pChildAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);
                                        Assert_(pChildAsSeq->pVecPlaceholdersToElse);
                                        Assert_(pChildAsSeq->uKindFlagsOfParentStatement & BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND);
                                    }
                                    pWhenFalse = pChildAsSeq->pVecPlaceholdersToElse;
                                    Assert_(pWhenFalse);
                                }

                                {
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                                        "Now typechecking condition for the 'elif' statement"), pTCContext->pWorker);

                                    // A little gymnastic with a special "on-resume" mechanism...
                                    LocalNodeInfoForResumedTask* pLocalResumeInfo = 0;
                                    if (pTCContext->bResumingCurrentStatement &&
                                            pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                                        auto it = pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.find(pMainNode->uNodeIndexInStatement);
                                        if (it != pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.end())
                                            pLocalResumeInfo = it.value();
                                    }

                                    if (pLocalResumeInfo) {
                                        vecPlaceholdersToTrue.append_all(pLocalResumeInfo->jumpsToTrue);
                                    }

                                    ETCResult checkCond = typecheck_runtime_conditional(&conditionNode, pTCStatement,
                                        &vecPlaceholdersToTrue, true, EBranchKind::BRANCH_TAKEN_IF,
                                        pWhenFalse, false, EBranchKind::BRANCH_TAKEN_ELSE,
                                        pTCContext, EExpectedExpr::EXPECT_REGULAR, 0u);

                                    if (UNLIKELY(checkCond == ETCResult::ETCR_WAITING)) {

                                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                                            "'WAITING' result while typechecking condition of elif-statement... recording locals"), pTCContext->pWorker);
                                        Assert(should_tc_ctx_halt_on_non_success(pTCContext), // otherwise would mean we'd not even be writing to the 'halted' context
                                            "typechecking elif statement : registering resume info for this path assumes current context is interruptible in case of wait on statement");
                                        if (pLocalResumeInfo == 0) {
                                            Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                                            if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                                                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                                            }
                                            pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                                                sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                                            pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pMainNode->uNodeIndexInStatement, pLocalResumeInfo);
                                            *pLocalResumeInfo = {};
                                            pLocalResumeInfo->jumpsToTrue.init(localArena);
                                        } else {
                                            pLocalResumeInfo->jumpsToTrue.clear();
                                        }
                                        pLocalResumeInfo->jumpsToTrue.append_all(vecPlaceholdersToTrue);
                                    }

                                    success_or_return_wait_or_error(checkCond, pMainNode->pTCNode);
                                }

                                if (pTCStatement->pChildBlock) {
                                    TCSeqSourceBlock* pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                                    pCurrentAsSeq->pNextTcBlockAfterCurrentStatement = pChildAsSeq;
                                    pChildAsSeq->_uBlockOpeningIRIffSeq = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                                    if (vecPlaceholdersToTrue.size()) {
                                        do_replace_jump_placeholders_to(pChildAsSeq->_uBlockOpeningIRIffSeq,
                                            vecPlaceholdersToTrue, pTCContext->pRepo, pTCContext);
                                    }
                                    ir_emit_marker_start_scope(pTCContext->pRepo, pTCContext); // must follow '_uBlockOpeningIRIffSeq'
                                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                                } else {
                                    // now only, we may "remember" there was a child-block-missing error
                                    // if was a simple 'elif', we may count on the already-reported parser error...
                                    // but that 'elif' could also have been spawned from a macro expansion,
                                    //    in which case we did not necessarily detect that there was a child block required (and thus missing) here
                                    //    => we need to report it now...
                                    return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                                        "typechecker error for control-flow statement missing child block");
                                }

                            } // otherwise fallthrough: elif missing if
                        } // otherwise fallthrough: elif missing if
                    } // otherwise fallthrough: elif missing if

                    return_error(pMainNode, pTCStatement, pTCContext, CERR_ELSE_WITHOUT_IF,
                        "typecheck_control_flow_statement() : 'elif' statement without preceeding 'if' or 'elif'");

                } break;

                case ETOK_ELSE: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'else' statement"), pTCContext->pWorker);
                    
                    if (LIKELY(pCurrentAsSeq->uStatementBeingTypechecked > 0)) {
                        u32 uIndexOfPreviousStatement = pCurrentAsSeq->uStatementBeingTypechecked - 1u;
                        TCStatement* pPreviousStatement = pCurrentAsSeq->vecStatements[uIndexOfPreviousStatement];
                        if (LIKELY(pPreviousStatement->pChildBlock)) {
                            TCSeqSourceBlock* pPreviousChildAsSeq = (TCSeqSourceBlock*)pPreviousStatement->pChildBlock;
                            if (LIKELY(pPreviousChildAsSeq->pVecPlaceholdersToElse)) {

                                if (pTCStatement->pChildBlock) {
                                    u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                                    if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                                        u32 uAstBlockIndex = u32(uTagged >> 2); // when so, its ast block index is encoded in bits 2..33
                                        // And we spawn it now...
                                        TCSeqSourceBlock* pChildAsSeq = tc_alloc_and_init_seq_block(uAstBlockIndex, pCurrentAsSeq,
                                            pCurrentAsSeq->uStatementBeingTypechecked, pTCContext);
                                        pChildAsSeq->uKindFlagsOfParentStatement |= BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND;
                                        // And we now can assign it as a real pointer instead of tagged id.
                                        pTCStatement->pChildBlock = pChildAsSeq;

                                        pCurrentAsSeq->pNextTcBlockAfterCurrentStatement = pChildAsSeq;
                                        pChildAsSeq->_uBlockOpeningIRIffSeq = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                                        ir_emit_marker_start_scope(pTCContext->pRepo, pTCContext); // must follow '_uBlockOpeningIRIffSeq'
                                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                                    } else {
                                        TCSeqSourceBlock* pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                                        Assert_(pChildAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);
                                        Assert_(pChildAsSeq->pVecPlaceholdersToElse == 0);
                                        Assert_(pChildAsSeq->uKindFlagsOfParentStatement & BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND);
                                    }

                                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                                } else {
                                    // now only, we may "remember" there was a child-block-missing error
                                    // if was a simple 'else', we may count on the already-reported parser error...
                                    // but that 'else' could also have been spawned from a macro expansion,
                                    //    in which case we did not necessarily detect that there was a child block required (and thus missing) here
                                    //    => we need to report it now...
                                    return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                                        "typechecker error for control-flow statement missing child block");
                                }

                            } // otherwise fallthrough: else missing if
                        } // otherwise fallthrough: else missing if
                    } // otherwise fallthrough: else missing if

                    return_error(pMainNode, pTCStatement, pTCContext, CERR_ELSE_WITHOUT_IF,
                        "typecheck_control_flow_statement() : 'else' statement without preceeding 'if' or 'elif'");

                } break;

                case ETOK_UNREACH: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'unreachable' statement"), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : 'unreachable' statement not yet implemented");
                } break;

                case ETOK_FOR: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'for' statement"), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : 'for' statement not yet implemented");
                } break;

                case ETOK_WHILE: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'while' statement"), pTCContext->pWorker);

                    TmpTCNode conditionNode = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);

                    TmpStackOptiArray<u32, 24> vecPlaceholdersToTrue(pTCContext->pWorker->tmpArena);
                    TmpStackOptiArray<u32, 24> dummyVecPlaceholdersToFalseWhenNoChild(pTCContext->pWorker->tmpArena);
                    TmpArray<u32>* pWhenFalse = &dummyVecPlaceholdersToFalseWhenNoChild;
                    if (pTCStatement->pChildBlock) { // in case there is a child block missing error, we still allow to tc the condition
                        u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                        TCSeqSourceBlock* pChildAsSeq;
                        if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                            u32 uAstBlockIndex = u32(uTagged >> 2); // when so, its ast block index is encoded in bits 2..33
                            // And we spawn it now...
                            pChildAsSeq = tc_alloc_and_init_seq_block(uAstBlockIndex, pCurrentAsSeq,
                                pCurrentAsSeq->uStatementBeingTypechecked, pTCContext);
                            // And we now can assign it as a real pointer instead of tagged id.
                            pTCStatement->pChildBlock = pChildAsSeq;

                        } else {
                            pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                            Assert_(pChildAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);
                            Assert_(pChildAsSeq->pVecPlaceholdersToElse == 0);
                        }
                        pWhenFalse = pChildAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses;
                    }

                    // A little gymnastic with a special "on-resume" mechanism...
                    LocalNodeInfoForResumedTask* pLocalResumeInfo = 0;
                    if (pTCContext->bResumingCurrentStatement &&
                            pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                        auto it = pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.find(pMainNode->uNodeIndexInStatement);
                        if (it != pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.end())
                            pLocalResumeInfo = it.value();
                    }

                    u32 uIRofJumpTargetBeforeCondCheck;
                    if (pLocalResumeInfo) {
                        vecPlaceholdersToTrue.append_all(pLocalResumeInfo->jumpsToTrue);
                        uIRofJumpTargetBeforeCondCheck = pLocalResumeInfo->uIRofLocalDecl;
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                            "Emitting jump-target before condition check of the loop, for the 'while' statement"), pTCContext->pWorker);
                        uIRofJumpTargetBeforeCondCheck = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                    }

                    {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                            "Now typechecking condition for the 'while' statement"), pTCContext->pWorker);

                        ETCResult checkCond = typecheck_runtime_conditional(&conditionNode, pTCStatement,
                            &vecPlaceholdersToTrue, true, EBranchKind::BRANCH_TAKEN_INLOOP_BEFORE,
                            pWhenFalse, false, EBranchKind::BRANCH_TAKEN_OUTLOOP,
                            pTCContext, EExpectedExpr::EXPECT_REGULAR, 0u);

                        if (UNLIKELY(checkCond == ETCResult::ETCR_WAITING)) {

                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                                "'WAITING' result while typechecking condition of while-statement... recording locals"), pTCContext->pWorker);
                            Assert(should_tc_ctx_halt_on_non_success(pTCContext), // otherwise would mean we'd not even be writing to the 'halted' context
                                "typechecking while statement : registering resume info for this path assumes current context is interruptible in case of wait on statement");
                            if (pLocalResumeInfo == 0) {
                                Arena localArena = pTCContext->pIsolatedSourceFile->localArena;
                                if (0 == pTCContext->mapLocalNodeInfoIfResumingCurrentStatement._alloc.arena.root_chunk_handle.uTagged) {
                                    pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.init(localArena);
                                }
                                pLocalResumeInfo = (LocalNodeInfoForResumedTask*)alloc_from(localArena,
                                    sizeof(LocalNodeInfoForResumedTask), alignof(LocalNodeInfoForResumedTask));
                                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement.insert(pMainNode->uNodeIndexInStatement, pLocalResumeInfo);
                                *pLocalResumeInfo = {};
                                pLocalResumeInfo->jumpsToTrue.init(localArena);
                            } else {
                                pLocalResumeInfo->jumpsToTrue.clear();
                            }
                            pLocalResumeInfo->jumpsToTrue.append_all(vecPlaceholdersToTrue);
                            pLocalResumeInfo->uIRofLocalDecl = uIRofJumpTargetBeforeCondCheck;
                        }

                        success_or_return_wait_or_error(checkCond, pMainNode->pTCNode);
                    }

                    if (pTCStatement->pChildBlock) {
                        TCSeqSourceBlock* pChildAsSeq = (TCSeqSourceBlock*)pTCStatement->pChildBlock;
                        pCurrentAsSeq->pNextTcBlockAfterCurrentStatement = pChildAsSeq;
                        pChildAsSeq->_uBlockOpeningIRIffSeq = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
                        pChildAsSeq->uIRBeforeLoopConditionIfBlockIsLoop = uIRofJumpTargetBeforeCondCheck;
                        ir_emit_marker_start_scope(pTCContext->pRepo, pTCContext); // must follow '_uBlockOpeningIRIffSeq'
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                    } else {
                        // now only, we may "remember" there was a child-block-missing error
                        // if was a simple 'while', we may count on the already-reported parser error...
                        // but that 'while' could also have been spawned from a macro expansion,
                        //    in which case we did not necessarily detect that there was a child block required (and thus missing) here
                        //    => we need to report it now...
                        return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                            "typechecker error for control-flow statement missing child block");
                    }
                } break;

                case ETOK_CASE: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking 'case' statement"), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : 'case' statement not yet implemented");
                } break;

                default:
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking unknown control-flow category..."), pTCContext->pWorker);
                    // TODO
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_control_flow_statement() : this kind not yet implemented");
            }

        } else {
            return_error(pMainNode, pTCStatement, pTCContext, CERR_SEQ_STATEMENT_WHEN_EXPECTING_NON_SEQ_STATEMENT,
                "typecheck_control_flow_statement() : control-flow statement in non-sequential compilation context");
        }
    } else {
        // TODO
        return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_control_flow_statement() : comptime-prefixed control-flow not yet implemented");
    }
}


#endif // LOCLIB_TC_BRANCHING_H_

