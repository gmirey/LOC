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

#ifndef LOCLIB_TC_STD_OPS_EVAL_H_
#define LOCLIB_TC_STD_OPS_EVAL_H_

#include "LocLib_TypeCheckerBase.h"
#include "LocLib_TC_Casts.h"
#include "LocLib_TC_InvocLike.h"
#include "LocLib_TC_ConstEvals.h"
#include "LocLib_IR_SolverInterface.h"


enum EIntegralBinopSpecialMismatch {
    EIBSM_NONE,
    EIBSM_FIRST_TYPE_RESULT,
    EIBSM_SECOND_TYPE_RESULT,
};

// typecheks any regular binary op (eg. all of them except boolean 'and', 'or', and comparisons) - but NOT pow or shift, when integral, typecasts already handled.
// All those ops should have an already typecasted opA and opB, both of same integral type (any, except compint).
local_func ETCResult typecheck_regular_binary_op_integral(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, EIntegralBinopSpecialMismatch eSpecialMismatch = EIBSM_NONE)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(pOperandA->pFinalValue->pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pFinalValue->pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(get_type_kind(pOperandB->pFinalValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(pOperandA->pFinalValue->pType == pOperandB->pFinalValue->pType || eSpecialMismatch != EIBSM_NONE);
    Assert_(get_log2_of_scalar_bytes(pOperandA->pFinalValue->pType) == get_log2_of_scalar_bytes(pOperandB->pFinalValue->pType));
    const TypeInfo_Integral* pResultingType = (const TypeInfo_Integral*)(
        (eSpecialMismatch == EIBSM_SECOND_TYPE_RESULT) ? pOperandB->pFinalValue->pType : pOperandA->pFinalValue->pType);

    u8 uResultFormat = get_ir_format(pResultingType);

    // Check non-zero operandB for ops based on division
    if (uOp == ETOK_INT_QUOTIENT || uOp == ETOK_INT_REMAINDER || uOp == ETOK_MOD) {
        IRInfo info0;
        if (uResultFormat <= 0x03u)
            info0 = ir_make_info_for_int_immediate(0, uResultFormat);
        else {
            EIRResult eCheckDeref0 = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, _max(uResultFormat, u8(3u)),
                uResultFormat, 1u, 1u << uResultFormat, 0u, pTCStatement, pTCContext, &info0);
            Assert_(eCheckDeref0 <= EIRResult::EIRR_ENSURED_VALID_KNOWN);
        }
        IRInfo infoIsZero;
        EIRResult eCheckIsZero = ir_emit_or_solve_eq_cmp_integral(uResultFormat, pOperandB->pFinalValue->info, info0, 0u,
            IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, pTCStatement, pTCContext, &infoIsZero);
        Assert_(eCheckIsZero < EIRResult::EIRR_FIRST_ERROR);
        if (infoIsZero.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
            Assert_(0 == (infoIsZero.uIRandMetaFlags & IRFLAG_HAS_NYKA));
            Assert_(infoIsZero.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
            Assert_(infoIsZero.metaValue.knownValue.uEmbeddedValue <= 1uLL);
            if (infoIsZero.metaValue.knownValue.uEmbeddedValue) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_DIVISION_BY_ZERO,
                    "typecheck_regular_binary_op_integral() : division by zero");
            } // otherwise NOOP
        }
        if (!ir_is_immediate(infoIsZero.uIRandMetaFlags)) { // if not an immediate, we assume an IR was actually emitted for the check
            // => we need to followup with an errcheck entry
            do_runtime_err_check(infoIsZero.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, 0u, 
                EErrCheckKind::ERR_CHECK_INT_DIVISION_BY_ZERO, pExpr, pTCStatement, pTCContext);
        }
    }

    IRInfo resultingInfo;
    EIRResult eResult;

    switch (uOp) {
        case ETOK_ADD:
        case ETOK_SUB:
        {
            EIntSemantics eSemantics;
            if (eSpecialMismatch == EIBSM_NONE) {
                eSemantics = is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED;
            } else {
                // TODO: relax the following assert if we use that for raw ?
                Assert_(is_signed_(pOperandA->pFinalValue->pType) != is_signed_(pOperandB->pFinalValue->pType));
                eSemantics = is_signed_(pOperandA->pFinalValue->pType) ? EIntSemantics::EINT_SEMANTIC_SA_UB : EIntSemantics::EINT_SEMANTIC_UA_SB;
            }
            u64 uHighBitSetIfSub = uOp == ETOK_ADD ? 0uLL : 0x8000'0000'0000'0000uLL;
            eResult = ir_emit_or_solve_add_or_sub_integral(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, uHighBitSetIfSub, eSemantics,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_MODULO_ADD: 
        case ETOK_MODULO_SUB: 
        {
            u64 uHighBitSetIfSub = uOp == ETOK_MODULO_ADD ? 0uLL : 0x8000'0000'0000'0000uLL;
            eResult = ir_emit_or_solve_add_or_sub_integral(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, uHighBitSetIfSub, EIntSemantics::EINT_SEMANTIC_MODULO_ARITH,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_MUL: {
            Assert_(eSpecialMismatch == EIBSM_NONE);
            EIntSemantics eSemantics = is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED;
            eResult = ir_emit_or_solve_mul_integral(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, eSemantics,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_MODULO_MUL: {
            Assert_(eSpecialMismatch == EIBSM_NONE);
            EIntSemantics eSemantics = is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_MODULO_SIGNED : EIntSemantics::EINT_SEMANTIC_MODULO_ARITH;
            eResult = ir_emit_or_solve_mul_integral(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, eSemantics,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_INT_QUOTIENT: {
            Assert_(eSpecialMismatch == EIBSM_NONE);
            EIntSemantics eSemantics = is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED;
            eResult = ir_emit_or_solve_quotient_integral(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, eSemantics,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_INT_REMAINDER: {
            Assert_(eSpecialMismatch == EIBSM_NONE);
            EIntSemantics eSemantics = is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED;
            eResult = ir_emit_or_solve_remainder_integral(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, eSemantics,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_MOD: {
            Assert_(eSpecialMismatch == EIBSM_NONE);
            if (is_signed(pResultingType)) {
                eResult = ir_emit_or_solve_signed_modulus_integral(uResultFormat,
                    pOperandA->pFinalValue->info, pOperandB->pFinalValue->info,
                    pExpr, pTCStatement, pTCContext, &resultingInfo);
            } else { // mod on unsigned is same as remainder...
                eResult = ir_emit_or_solve_remainder_integral(uResultFormat,
                    pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, EIntSemantics::EINT_SEMANTIC_UNSIGNED,
                    pExpr, pTCStatement, pTCContext, &resultingInfo);
            }
        } break;

        case ETOK_BIT_AND: {
            eResult = ir_emit_or_solve_bitwise_and(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_BIT_OR: {
            eResult = ir_emit_or_solve_bitwise_or(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        case ETOK_BIT_XOR: {
            eResult = ir_emit_or_solve_bitwise_xor(uResultFormat,
                pOperandA->pFinalValue->info, pOperandB->pFinalValue->info,
                pExpr, pTCStatement, pTCContext, &resultingInfo);
        } break;

        default:
            Assume_(false);
            return ETCResult::ETCR_ERROR;

    }

    if (eResult == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A && pResultingType == pOperandA->pFinalValue->pType)
        return set_tc_success_with_same_value_as_final_of(pOperandA, pExpr);
    if (eResult == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_B && pResultingType == pOperandB->pFinalValue->pType)
        return set_tc_success_with_same_value_as_final_of(pOperandB, pExpr);
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        if (irflag_is_known_or_nyka(resultingInfo.uIRandMetaFlags) && 0 == (resultingInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA)) {
            resultingInfo.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
        }
        if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
            Assert_(is_value_tc_const(pOperandA->pFinalValue) && is_value_tc_const(pOperandB->pFinalValue));
            if (!irflag_is_tc_const(resultingInfo.uIRandMetaFlags)) {
                Assert_(get_ir_format(pResultingType) >= 0x03u);
                return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_EVAL_NYKA,
                    "typecheck_regular_binary_op_integral() : non-const result from a regular operation on constant. One of your operands"
                    " probably carries a NYKA, preventing evaluation as a constant by the typechecker.");
            }
        }
        NodeValue* pResultingValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResultingValue->pType = pResultingType;
        pResultingValue->info = resultingInfo;
        return set_node_typecheck_expr_success(pExpr->pTCNode);
    } else {
        return_error(pExpr, pTCStatement, pTCContext, u16(eResult),
            "typecheck_regular_binary_op_integral() : IR Solver determined invalid op.");
    }

}

// typecheks pow or shift binary op on integrals, typecasts already handled (second operand should always be cast to int or nat).
// All those ops should have an already typecasted opA and opB: opA of any integral type (except compint), opB of fixed type INT or NAT.
local_func ETCResult typecheck_regular_binary_op_integral_pow_or_shift(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(uOp == ETOK_RIGHT_SHIFT || uOp == ETOK_LEFT_SHIFT || uOp == ETOK_POW);
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(pOperandA->pFinalValue->pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pFinalValue->pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    const TypeInfo_Integral* pResultingType = (const TypeInfo_Integral*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == g_pCoreTypesInfo[ECORETYPE_INT] || pOperandB->pFinalValue->pType == g_pCoreTypesInfo[ECORETYPE_NAT]);

    // Check 2nd operand non-negative
    //
    if (is_signed_(pOperandB->pFinalValue->pType)) {
        IRInfo info0 = ir_make_info_for_int_immediate(0, 0x02u);
        IRInfo infoIsNeg;
        EIRResult eCheckIsNeg = ir_emit_or_solve_ord_cmp_integral(0x02u, pOperandB->pFinalValue->info, info0, 0u, IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EINT_SEMANTIC_SIGNED,
            pTCStatement, pTCContext, &infoIsNeg);
        Assert_(eCheckIsNeg < EIRResult::EIRR_FIRST_ERROR);
        if (infoIsNeg.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
            Assert_(0 == (infoIsNeg.uIRandMetaFlags & IRFLAG_HAS_NYKA));
            Assert_(infoIsNeg.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
            Assert_(infoIsNeg.metaValue.knownValue.uEmbeddedValue <= 1uLL);
            if (infoIsNeg.metaValue.knownValue.uEmbeddedValue) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_OPERATOR_REQUIRES_NON_NEGATIVE_SECOND_OPERAND,
                    "typecheck_regular_binary_op_integral_pow_or_shift() : second operand to integral pow or shift cannot be negative");
            } // otherwise NOOP
        }
        if (!ir_is_immediate(infoIsNeg.uIRandMetaFlags)) { // if not an immediate, we assume an IR was actually emitted for the check
            // => we need to followup with an errcheck entry
            do_runtime_err_check(infoIsNeg.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, 0u, 
                EErrCheckKind::ERR_CHECK_OPERAND_CANNOT_BE_NEGATIVE, pExpr, pTCStatement, pTCContext);
        }
    }

    u8 uResultFormat = get_ir_format(pResultingType);

    // Check 2nd operand is strictly less than number of bits in resulting format
    {
        u32 uBitCount = 8u << uResultFormat;
        IRInfo infoBitCount = ir_make_info_for_int_immediate(i32(uBitCount), 0x02u);
        IRInfo infoIsTooLarge;
        EIRResult eCheckIsTooLarge = ir_emit_or_solve_ord_cmp_integral(0x02u, pOperandB->pFinalValue->info, infoBitCount,
            IR_INSTRFLAG_CMP_OPPOSITE, IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES,
            is_signed_(pOperandB->pFinalValue->pType) ? EINT_SEMANTIC_SIGNED : EINT_SEMANTIC_UNSIGNED,
            pTCStatement, pTCContext, &infoIsTooLarge);
        Assert_(eCheckIsTooLarge < EIRResult::EIRR_FIRST_ERROR);
        if (infoIsTooLarge.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
            Assert_(0 == (infoIsTooLarge.uIRandMetaFlags & IRFLAG_HAS_NYKA));
            Assert_(infoIsTooLarge.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
            Assert_(infoIsTooLarge.metaValue.knownValue.uEmbeddedValue <= 1uLL);
            if (infoIsTooLarge.metaValue.knownValue.uEmbeddedValue) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_OPERATOR_REQUIRES_SMALL_SECOND_OPERAND,
                    "typecheck_regular_binary_op_integral_pow_or_shift() : second operand to integral pow or shift shall be strictly less than number of bits in resulting format");
            } // otherwise NOOP
        }
        if (!ir_is_immediate(infoIsTooLarge.uIRandMetaFlags)) { // if not an immediate, we assume an IR was actually emitted for the check
            // => we need to followup with an errcheck entry
            do_runtime_err_check(infoIsTooLarge.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, 0u, 
                EErrCheckKind::ERR_CHECK_OPERAND_VALUE_TOO_LARGE, pExpr, pTCStatement, pTCContext);
        }
    }

    IRInfo resultingInfo;
    EIRResult eResult;

    if (uOp == ETOK_POW) {
        eResult = ir_emit_or_solve_integral_pow(uResultFormat,
            pOperandA->pFinalValue->info, is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED,
            pOperandB->pFinalValue->info, pTCStatement, pTCContext, &resultingInfo);
    } else if (uOp == ETOK_LEFT_SHIFT) {
        eResult = ir_emit_or_solve_left_shift(uResultFormat,
            pOperandA->pFinalValue->info, pOperandB->pFinalValue->info, pTCStatement, pTCContext, &resultingInfo);
    } else { Assert_(uOp == ETOK_RIGHT_SHIFT); 
        eResult = ir_emit_or_solve_right_shift(uResultFormat,
            pOperandA->pFinalValue->info, is_signed(pResultingType) ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED,
            pOperandB->pFinalValue->info, pTCStatement, pTCContext, &resultingInfo);
    }

    if (eResult == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A && pResultingType == pOperandA->pFinalValue->pType)
        return set_tc_success_with_same_value_as_final_of(pOperandA, pExpr);
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        if (irflag_is_known_or_nyka(resultingInfo.uIRandMetaFlags) && 0 == (resultingInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA)) {
            resultingInfo.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
        }
        if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
            Assert_(is_value_tc_const(pOperandA->pFinalValue) && is_value_tc_const(pOperandB->pFinalValue));
            if (!irflag_is_tc_const(resultingInfo.uIRandMetaFlags)) {
                Assert_(uResultFormat >= 0x03u); // should only be true in the presence of nykas => at least 64b
                return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_EVAL_NYKA,
                    "typecheck_regular_binary_op_integral_pow_or_shift() : non-const result from a regular operation on constant. One of your operands"
                    " probably carries a NYKA, preventing evaluation as a constant by the typechecker.");
            }
        }
        NodeValue* pResultingValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResultingValue->pType = pResultingType;
        pResultingValue->info = resultingInfo;
        return set_node_typecheck_expr_success(pExpr->pTCNode);
    } else {
        return_error(pExpr, pTCStatement, pTCContext, u16(eResult),
            "typecheck_regular_binary_op_integral_pow_or_shift() : IR Solver determined invalid op.");
    }

}

local_func bool is_compint_abs_payload_greater_than_unsigned(u64 uCompintPayload, u8 uLog2OfScalarBytes) {
    if ((uCompintPayload & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if (uLog2OfScalarBytes >= 0x03u) // 64b or more footprint => necessarily false if small-embedded payload
            return false;
        else {
            u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u32 uAboveUnsignedBitShift = (1u << uLog2OfScalarBytes) * 8u;
            u64 uAboveUnsigned = 1uLL << uAboveUnsignedBitShift;
            return (uAbsValue >= uAboveUnsigned);
        }
    } else {
        // TODO
        Assert(false, "is_compint_abs_payload_greater_than_unsigned() : non-embedded not yet implemented");
    }
}

local_func bool is_compint_abs_payload_greater_than_signed(u64 uCompintPayload, u8 uLog2OfScalarBytes) {
    if ((uCompintPayload & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
        if (uLog2OfScalarBytes >= 0x03u) // 64b or more footprint => necessarily false if small-embedded payload
            return false;
        else {
            u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u32 uAboveUnsignedBitShift = (1u << uLog2OfScalarBytes) * 8u;
            u64 uAboveSigned = 1uLL << (uAboveUnsignedBitShift - 1u);
            return (uAbsValue >= uAboveSigned);
        }
    } else {
        // TODO
        Assert(false, "is_compint_abs_payload_greater_than_signed() : non-embedded not yet implemented");
    }
}

local_func ETCResult tc_do_fp_pow_n(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
    const TypeInfo_FloatingPoint* pResultingType = (const TypeInfo_FloatingPoint*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == g_pCoreTypesInfo[ECORETYPE_INT]);

    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_do_fp_pow_n() : not yet implemented.");
}

local_func ETCResult tc_do_fp_pow(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
    const TypeInfo_FloatingPoint* pResultingType = (const TypeInfo_FloatingPoint*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == pResultingType);

    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_do_fp_pow() : not yet implemented.");
}

local_func ETCResult tc_do_fp_add_or_sub(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, u64 uHighBitSetIfSub,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
    const TypeInfo_FloatingPoint* pResultingType = (const TypeInfo_FloatingPoint*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == pResultingType);
    Assert_(uHighBitSetIfSub == 0uLL || uHighBitSetIfSub == 0x8000'0000'0000'0000uLL);

    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_do_fp_add_or_sub() : not yet implemented.");
}

local_func ETCResult tc_do_fp_mul(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
    const TypeInfo_FloatingPoint* pResultingType = (const TypeInfo_FloatingPoint*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == pResultingType);

    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_do_fp_mul() : not yet implemented.");
}

local_func ETCResult tc_do_fp_div_or_mod(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, bool bIsMod,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
    const TypeInfo_FloatingPoint* pResultingType = (const TypeInfo_FloatingPoint*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == pResultingType);

    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_do_fp_div_or_mod() : not yet implemented.");
}

local_func ETCResult typecheck_regular_binary_op_fp(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(get_type_kind(pOperandA->pFinalValue->pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
    const TypeInfo_FloatingPoint* pResultingType = (const TypeInfo_FloatingPoint*)pOperandA->pFinalValue->pType;
    Assert_(pOperandB->pFinalValue->pType == pResultingType);

    switch (uOp) {
        case ETOK_ADD: {
            return tc_do_fp_add_or_sub(pExpr, pOperandA, pOperandB, 0uLL, pTCStatement, pTCContext, eExpectation);
        } break;

        case ETOK_SUB: {
            return tc_do_fp_add_or_sub(pExpr, pOperandA, pOperandB, 0x8000'0000'0000'0000uLL, pTCStatement, pTCContext, eExpectation);
        } break;

        case ETOK_MUL: {
            return tc_do_fp_mul(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
        } break;

        case ETOK_DIV: {
            return tc_do_fp_div_or_mod(pExpr, pOperandA, pOperandB, false, pTCStatement, pTCContext, eExpectation);
        } break;

        case ETOK_MOD: {
            return tc_do_fp_div_or_mod(pExpr, pOperandA, pOperandB, true, pTCStatement, pTCContext, eExpectation);
        } break;

        default:
            Assume_(false);
            return ETCResult::ETCR_ERROR;
    }
}


local_func ETCResult typecheck_ptrdiff(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(!is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(!is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(!is_node_already_typechecked(pExpr->pTCNode));
    Assert_(get_type_kind(pOperandA->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER || pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]);
    Assert_(get_type_kind(pOperandB->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER || pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]);
    u32 uByteCountA = 1u;
    if (pOperandA->pIntrinsicValue->pType != g_pCoreTypesInfo[ECORETYPE_RAWPTR]) {
        Assert_(get_type_kind(pOperandA->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER);
        const TypeInfo_Pointer* pAsPtrTypeA = (const TypeInfo_Pointer*)pOperandA->pIntrinsicValue->pType;
        Assert_(get_ir_format(pAsPtrTypeA) == 0x03u);
        const TypeInfo* pPointedToByA = pAsPtrTypeA->pPointedToType;
        uByteCountA = get_runtime_sizeof(pPointedToByA);
    }
    u32 uByteCountB = 1u;
    if (pOperandB->pIntrinsicValue->pType != g_pCoreTypesInfo[ECORETYPE_RAWPTR]) {
        Assert_(get_type_kind(pOperandB->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER);
        const TypeInfo_Pointer* pAsPtrTypeB = (const TypeInfo_Pointer*)pOperandB->pIntrinsicValue->pType;
        Assert_(get_ir_format(pAsPtrTypeB) == 0x03u);
        const TypeInfo* pPointedToByB = pAsPtrTypeB->pPointedToType;
        uByteCountB = get_runtime_sizeof(pPointedToByB);
    }
    if (uByteCountA == uByteCountB) {
        if (uByteCountA) {
            IRInfo infoByteDiffResult;
            EIRResult eCheckDiff = ir_emit_or_solve_add_or_sub_integral(0x03u, pOperandA->pIntrinsicValue->info, pOperandB->pIntrinsicValue->info,
                0x8000'0000'0000'0000uLL, EIntSemantics::EINT_SEMANTIC_MODULO_ARITH, pExpr, pTCStatement, pTCContext, &infoByteDiffResult);
            if (eCheckDiff >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExpr, pTCStatement, pTCContext, u16(eCheckDiff),
                    "typecheck_ptrdiff() : emit sub failed");
            }
            NodeValue* pResultValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pResultValue->pType = g_pCoreTypesInfo[ECORETYPE_I64];
            if (uByteCountA > 1u) {
                IRInfo infoDivisor = ir_make_info_for_int_immediate(i32(uByteCountA), 0x03u);
                IRInfo infoScaledDiffResult;
                EIRResult eCheckDiv = ir_emit_or_solve_expected_exact_quotient(0x03u, infoByteDiffResult, infoDivisor,
                    EIntSemantics::EINT_SEMANTIC_SIGNED, pExpr, pTCStatement, pTCContext, ERR_CHECK_PTR_OFFSET_MISALIGNED,
                    &infoScaledDiffResult);
                if (eCheckDiff >= EIRResult::EIRR_FIRST_ERROR) { // TODO: better error report ?
                    return_error(pExpr, pTCStatement, pTCContext, u16(eCheckDiff),
                        "typecheck_ptrdiff() : emit exact quotient failed");
                }
                pResultValue->info = infoScaledDiffResult;
            } else {
                pResultValue->info = infoByteDiffResult;
            }
            ETCResult eCheckConst = consolidate_tc_const_flag_on_info(&(pResultValue->info), pResultValue->pType, pExpr, pTCStatement, pTCContext);
            Assert_(eCheckConst == ETCResult::ETCR_SUCCESS);
            if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                if (!is_value_tc_const(pResultValue)) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                        "Diff between those pointers cannot be solved as a constant value - although pointer values can themselves be flagged as"
                        " const, they represent absolute addresses in the final binary, and as such, cannot be known at this time. Except in"
                        " cases where two pointers are related to a base address which is known to be precisely same, pointer diffs cannot be"
                        " solved as constants. Try to relax the current const requirement if you can do with this result as a runtime value.");
                }
            }
            return set_node_typecheck_expr_success(pExpr->pTCNode);

        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_PTRDIFF_OP,
                "cannot ptrdiff between pointers to void-sized types");
        }
    } else {
        return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_PTRDIFF_OP,
            "cannot ptrdiff between pointers to different-sized types");
    }
}

local_func ETCResult typecheck_ptroffset(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    u64 uHighBitSetIfSub, TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(uHighBitSetIfSub == 0uLL || uHighBitSetIfSub == 0x8000'0000'0000'0000uLL);
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(!is_node_already_type_casted(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(!is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(!is_node_already_typechecked(pExpr->pTCNode));
    Assert_(get_type_kind(pOperandA->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER || pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]);
    Assert_(get_type_kind(pOperandB->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    Assert_(!is_raw_integral_(pOperandB->pIntrinsicValue->pType));

    u32 uByteCountA = 1u;
    u32 uLog2OfAlign = 0u;
    if (pOperandA->pIntrinsicValue->pType != g_pCoreTypesInfo[ECORETYPE_RAWPTR]) {
        Assert_(get_type_kind(pOperandA->pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER);
        const TypeInfo_Pointer* pAsPtrTypeA = (const TypeInfo_Pointer*)pOperandA->pIntrinsicValue->pType;
        Assert_(get_ir_format(pAsPtrTypeA) == 0x03u);
        const TypeInfo* pPointedToByA = pAsPtrTypeA->pPointedToType;
        uByteCountA = get_runtime_sizeof(pPointedToByA);
        uLog2OfAlign = get_log2_of_align_bytes(pPointedToByA);
    }
    const TypeInfo_Integral* pAsIntegralTypeB = (const TypeInfo_Integral*)pOperandB->pIntrinsicValue->pType;
    
    if (uByteCountA) {
        u8 uFinalFormatIndex;
        bool bFinalIndexSigned;
        if (pAsIntegralTypeB == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
            Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));
            u64 uCompintPayload = pOperandB->pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
            bFinalIndexSigned = (uCompintPayload & COMPINT_FLAG_IS_NEGATIVE) ? true : false;
            uFinalFormatIndex = 0x02u;
            if ((uCompintPayload & COMPINT_SIZE_MASK) != COMPINT_SIZE_SMALL_EMBD) {
                uFinalFormatIndex = 0x03u;
            } else {
                u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
                if (bFinalIndexSigned) {
                    if (uAbsValue > 0x0000'0000'8000'0000uLL)
                        uFinalFormatIndex = 0x03u;
                } else {
                    if (uAbsValue >= 0x0000'0001'0000'0000uLL)
                        uFinalFormatIndex = 0x03u;
                }
            }
        } else {
            u8 uFormatIndex = get_ir_format(pAsIntegralTypeB);
            Assert_(uFormatIndex <= 0x05u);
            uFinalFormatIndex = 0x02u;
            bool bIndexSigned = is_signed(pAsIntegralTypeB);
            bFinalIndexSigned = bIndexSigned;
            if (uHighBitSetIfSub)
                bFinalIndexSigned = true;
            if (uFormatIndex > 0x02u || (uFormatIndex == 0x02u && !bIndexSigned && uHighBitSetIfSub)) {
                uFinalFormatIndex = 0x03u;
            }
        }
        const TypeInfo_Integral* pFinalFormatType = (const TypeInfo_Integral*)((uFinalFormatIndex == 0x02u) ? 
            (bFinalIndexSigned ? g_pCoreTypesInfo[ECoreType::ECORETYPE_I32] : g_pCoreTypesInfo[ECoreType::ECORETYPE_U32]) :
            (bFinalIndexSigned ? g_pCoreTypesInfo[ECoreType::ECORETYPE_I64] : g_pCoreTypesInfo[ECoreType::ECORETYPE_U64]));
        ETCResult eCastFormat = do_implicit_cast(pOperandB, pFinalFormatType, pTCStatement, pTCContext, eExpectation);
        success_or_return_wait_or_error(eCastFormat, pExpr->pTCNode);
        Assert_(is_node_already_type_casted(pOperandB->pTCNode));
        Assert_(pOperandB->pFinalValue);
        IRInfo infoIndex = pOperandB->pFinalValue->info;
        if (uHighBitSetIfSub) {
            Assert_(bFinalIndexSigned);
            IRInfo info0 = ir_make_info_for_int_immediate(0, uFinalFormatIndex);
            IRInfo indexOpposite;
            EIRResult eEmitOpposite = ir_emit_or_solve_add_or_sub_integral(uFinalFormatIndex, info0, infoIndex, 0x8000'0000'0000'0000uLL,
                EIntSemantics::EINT_SEMANTIC_SIGNED, pExpr, pTCStatement, pTCContext, &indexOpposite);
            if (eEmitOpposite < EIRResult::EIRR_FIRST_ERROR) {
                infoIndex = indexOpposite;
            } else {
                return_error(pExpr, pTCStatement, pTCContext, u16(eEmitOpposite),
                    "typecheck_ptroffset() : emit opposite of index for ptroffset by sub failed.");
            }
        }
        IRInfo infoResult;
        EIRResult eOffsetResult = ir_emit_or_solve_ptr_offset(uLog2OfAlign, pOperandA->pIntrinsicValue->info,
            uFinalFormatIndex, infoIndex, uByteCountA, 0u, bFinalIndexSigned ? EINT_SEMANTIC_SIGNED : EINT_SEMANTIC_UNSIGNED,
            pTCStatement, pTCContext, &infoResult);
        if (eOffsetResult < EIRResult::EIRR_FIRST_ERROR) {
            NodeValue* pResultValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pResultValue->pType = pOperandA->pIntrinsicValue->pType;
            pResultValue->info = infoResult;
            ETCResult eCheckConst = consolidate_tc_const_flag_on_info(&(pResultValue->info), pResultValue->pType, pExpr, pTCStatement, pTCContext);
            Assert_(eCheckConst == ETCResult::ETCR_SUCCESS);
            if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                if (!is_value_tc_const(pResultValue)) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                        "This pointer offset cannot be solved as a constant value.");
                }
            }
            return set_node_typecheck_expr_success(pExpr->pTCNode);

        } else {
            return_error(pExpr, pTCStatement, pTCContext, u16(eOffsetResult),
                "typecheck_ptroffset() : emit ptroffset failed.");
        }

    } else {
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pOperandB);
    }
}

// typechecks any regular binary op (eg. all of them except boolean 'and', 'or', and comparison operators)
local_func ETCResult typecheck_regular_binary_op_dispatch(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking binary-op (token: %s ) against its parameter",
        reinterpret_cast<u64>(tStandardPayloadsStr[uOp])), pTCContext->pWorker);

    if (pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT] && pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) { // both compints => solve on TC-side.
        return typecheck_regular_binary_op_full_compint(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext);
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

    if (bIsNumericA && bIsNumericB) {
        // both numeric => allow most numeric ops

        if (uOp == ETOK_CONCAT) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_STRING_OR_ARRAY_TYPE,
                "typecheck_regular_binary_op_dispatch() : concat operator requires string or array type.");
        }

        if (bIsIntegralA && bIsIntegralB) {
            // both numeric integrals => numeric ops mode : integral

            const TypeInfo_Integral* pIntTypeA = (const TypeInfo_Integral*)pOperandA->pIntrinsicValue->pType;
            const TypeInfo_Integral* pIntTypeB = (const TypeInfo_Integral*)pOperandB->pIntrinsicValue->pType;
            Assert_(!is_raw_integral(pIntTypeA) && !is_raw_integral(pIntTypeB)); // otherwise we shouldn't be in the 'both numeric' path

            if (uOp == ETOK_DIV) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_FLOATING_POINT_TYPE,
                    "typecheck_regular_binary_op_dispatch() : the division operator '/' has a semantic image in the field of Rational Numbers."
                        " If you want the integer quotient instead, use the '/%' operator. Otherwise, "
                        "use at least one floating-point-typed operand to allow the compiler to decide of a resulting type.");
            }

            bool bOpIsPow = (uOp == ETOK_POW);
            bool bOpIsShift = (uOp == ETOK_LEFT_SHIFT || uOp == ETOK_RIGHT_SHIFT);

            if (bOpIsPow || bOpIsShift) {
                // Shift or pow require pre-cast operandB to regular (signed!) int beforehand.
                ETCResult checkCastB = is_signed(pIntTypeB) ?
                    do_implicit_cast(pOperandB, g_pCoreTypesInfo[ECORETYPE_INT], pTCStatement, pTCContext, eExpectation) : 
                    do_implicit_cast(pOperandB, g_pCoreTypesInfo[ECORETYPE_NAT], pTCStatement, pTCContext, eExpectation);
                success_or_return_wait_or_error(checkCastB, pExpr->pTCNode);
                if (pIntTypeA == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
                    if (is_value_tc_const(pOperandB->pFinalValue)) {
                        Assert_(!is_value_nyka_or_has_nyka(pOperandB->pFinalValue)); // NYKA couldn't fit in 32b
                        // Compint shift or pow by any known constant integral stays compint...
                        return typecheck_compint_pow_or_shift_by_constant(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext);
                    } else {
                        /*
                        // Compint shift or cast by runtime shall get cast to implicit int before solving as runtime op...
                        ETCResult checkCastA = do_implicit_cast(pOperandA, g_pCoreTypesInfo[ECORETYPE_INT], pTCStatement, pTCContext, eExpectation);
                        success_or_return_wait_or_error(checkCastA, pExpr->pTCNode);
                        return typecheck_regular_binary_op_integral_pow_or_shift(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
                        */
                        return_error(pExpr, pTCStatement, pTCContext, CERR_OPERATOR_REQUIRES_SIZED_OPERANDS,
                            "typecheck_regular_binary_op_dispatch() : compint shift by runtime value: please specify size of result by explicitely casting that compint value. The usual implicit cast to int from compint seems too dangerous in that context.");
                    }
                } else {
                    // other types shift or pow by any integral stays with their own type...
                    set_cast_success_with_same_value(pOperandA);
                    return typecheck_regular_binary_op_integral_pow_or_shift(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
                }

            } else {
                if (pIntTypeA == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
                    Assert_(pIntTypeB != g_pCoreTypesInfo[ECORETYPE_COMPINT]); // both compints should have been shortcut at this point
                    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
                    u64 uPayloadCompintA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
                    // Special case treatments in case of compint A:
                    // - can add to unsigned even if negative: sub abs value instead.
                    // - can add or sub (or modulo of those) to signed even if 1b-greater than signed abs-span: special add or sub 'mixed' (TODO).
                    // - can bitwise ops with any signedness providing significant bits fits.
                    if (is_unsigned(pIntTypeB)) {
                        if (uPayloadCompintA & COMPINT_FLAG_IS_NEGATIVE) {
                            const TypeInfo_Integral* pSignedEquiv = get_signed_type_of_same_width_from(pIntTypeB);
                            // TODO
                            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                "typecheck_regular_binary_op_dispatch() : negative compint operating with unsigned: handling of special cases not yet implemented.");
                        }
                    } else { Assert_(is_signed(pIntTypeB)); // raw integrals should not be in this codepath
                        if (0uLL == (uPayloadCompintA & COMPINT_FLAG_IS_NEGATIVE)) {
                            if (is_compint_abs_payload_greater_than_signed(uPayloadCompintA, get_log2_of_scalar_bytes(pIntTypeB))) {
                                const TypeInfo_Integral* pUnsignedEquiv = get_unsigned_type_of_same_width_from(pIntTypeB);
                                // TODO
                                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                    "typecheck_regular_binary_op_dispatch() : 1-bit larger positive compint operating with signed: handling of special cases not yet implemented.");
                            }
                        }
                    }

                } else if (pIntTypeB == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
                    Assert_(pIntTypeA != g_pCoreTypesInfo[ECORETYPE_COMPINT]); // both compints should have been shortcut at this point
                    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));
                    u64 uPayloadCompintB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;
                    // Special case treatments in case of compint B:
                    // - can add to unsigned even if negative: sub abs value instead.
                    // - can sub from unsigned even if negative: add abs value instead.
                    // - can add or sub (or modulo of those) to signed even if 1b-greater than signed abs-span: special add or sub 'mixed' (TODO).
                    // - can bitwise ops with any signedness providing significant bits fits.
                    if (is_unsigned(pIntTypeA)) {
                        if (uPayloadCompintB & COMPINT_FLAG_IS_NEGATIVE) {
                            const TypeInfo_Integral* pSignedEquiv = get_signed_type_of_same_width_from(pIntTypeA);
                            // TODO
                            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                "typecheck_regular_binary_op_dispatch() : negative compint operating with unsigned: handling of special cases not yet implemented.");
                        }
                    } else { Assert_(is_signed(pIntTypeA)); // raw integrals should not be in this codepath
                        if (0uLL == (uPayloadCompintB & COMPINT_FLAG_IS_NEGATIVE)) {
                            if (is_compint_abs_payload_greater_than_signed(uPayloadCompintB, get_log2_of_scalar_bytes(pIntTypeA))) {
                                const TypeInfo_Integral* pUnsignedEquiv = get_unsigned_type_of_same_width_from(pIntTypeA);
                                // TODO
                                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                    "typecheck_regular_binary_op_dispatch() : 1-bit larger positive compint operating with signed: handling of special cases not yet implemented.");
                            }
                        }
                    }
                }

                u16 uTypeErr = 0;
                const TypeInfo_Integral* pResultingType = get_resulting_integral_type(pIntTypeA, pIntTypeB, pTCContext, &uTypeErr,
                    uOp == ETOK_BIT_AND ? EIntMismatchChoice::EINT_CHOOSE_SMALLEST : (
                    uOp == ETOK_BIT_XOR ? EIntMismatchChoice::EINT_CHOOSE_ONLY_SAME_FOOTPRINT :
                    /* BIT_OR or non-bitwise */ EIntMismatchChoice::EINT_CHOOSE_LARGEST));

                if (uTypeErr) {
                    return_error(pExpr, pTCStatement, pTCContext, uTypeErr,
                        "typecheck_regular_binary_op_dispatch() : find common integral type failed");
                } else
                    Assert_(pResultingType && pResultingType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);

                // TODO: add runtime-reification from nyka to operations handled by this 'do_implicit_cast()' thing ?
                // TODO: where to handle error in conversion from nyka ?
                // TODO: special subtracting of nykas may be const-evalued if same base...

                ETCResult checkCastA = do_implicit_cast(pOperandA, pResultingType, pTCStatement, pTCContext, eExpectation);
                success_or_return_wait_or_error(checkCastA, pExpr->pTCNode);
                ETCResult checkCastB = do_implicit_cast(pOperandB, pResultingType, pTCStatement, pTCContext, eExpectation);
                success_or_return_wait_or_error(checkCastB, pExpr->pTCNode);

                return typecheck_regular_binary_op_integral(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
            }

        } else {
            // at least one numeric FP => numeric ops mode : FP

            if (uOp == ETOK_POW) {
                if (!bIsIntegralA) { // A should be rightfully floating-point here
                    // We keep the type of operand A
                    if (bIsIntegralB) { // any integer operand B must be casted to INT.
                        ETCResult checkCastB = do_implicit_cast(pOperandB, g_pCoreTypesInfo[ECORETYPE_INT], pTCStatement, pTCContext, eExpectation);
                        success_or_return_wait_or_error(checkCastB, pExpr->pTCNode);
                        return tc_do_fp_pow_n(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
                    } else { // convert B to type of A for floating point
                        ETCResult checkCastB = do_implicit_cast(pOperandB, pOperandA->pIntrinsicValue->pType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT);
                        success_or_return_wait_or_error(checkCastB, pExpr->pTCNode);
                        return tc_do_fp_pow(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
                    }
                } else {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_FLOATING_POINT_TYPE,
                        "typecheck_regular_binary_op_between_constants() : the power operator '**' always returns a value with type of its first operand."
                            " The floating-point version here (since second operand is floating-point) thus expects its *first* operand to be specified with a floating-point type.");
                }
            }

            if (uOp == ETOK_MODULO_ADD || uOp == ETOK_MODULO_SUB || uOp == ETOK_MODULO_MUL) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_dispatch() : the modulo-arithmetic operators only allow integral operands.");
            } else if (uOp == ETOK_INT_QUOTIENT || uOp == ETOK_INT_REMAINDER) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_dispatch() : the integer quotient or remainder operators only allow integral operands.");
            } else if (uOp == ETOK_LEFT_SHIFT || uOp == ETOK_RIGHT_SHIFT) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_dispatch() : the bit-shifts operators only allow integral operands.");
            } else if (uOp == ETOK_BIT_AND || uOp == ETOK_BIT_XOR || uOp == ETOK_BIT_OR) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_dispatch() : the bitwise operators only allow integral operands."
                    " If a bitwise op at the representation-level was your intent, you may reinterpret-cast a floating point operand to a raw integral.");
            }

            Assert_(!bIsIntegralA || !bIsIntegralB);
            const TypeInfo_FloatingPoint* pResultingType = get_resulting_fp_type(
                pOperandA->pIntrinsicValue->pType, pOperandB->pIntrinsicValue->pType, pTCContext);
            Assert_(pResultingType); // get_resulting_fp_type() is never expected to fail

            ETCResult checkCastA = do_implicit_cast(pOperandA, pResultingType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT);
            success_or_return_wait_or_error(checkCastA, pExpr->pTCNode);
            ETCResult checkCastB = do_implicit_cast(pOperandB, pResultingType, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT);
            success_or_return_wait_or_error(checkCastB, pExpr->pTCNode);

            return typecheck_regular_binary_op_fp(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);

        }

    } else if (bIsPointerA && !bIsIntegralA) {  // A is pointer, excluding rawpointer
    
        if (bIsPointerB && !bIsIntegralB) { // Both pointers
            if (uOp == ETOK_SUB || uOp == ETOK_MODULO_SUB) { // ptrdiff. Result either I64 or U64 (but arithmetics are unchecked)
                return typecheck_ptrdiff(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, CERR_NOT_A_VALID_OPERATION_ON_POINTER,
                    "typecheck_regular_binary_op_dispatch() : cannot do this operation between two pointer types. Expected: sub (or modulo sub)");
            }
        } else if (bIsNumericB && bIsIntegralB) { // ... and B is integral numeric => can do add or sub as ptr offsets. Result type of A.
            if (uOp == ETOK_ADD || uOp == ETOK_MODULO_ADD || uOp == ETOK_SUB || uOp == ETOK_MODULO_SUB) { // ptr offset
                u64 uHighBitSetIfSub = (uOp == ETOK_ADD || uOp == ETOK_MODULO_ADD) ? 0uLL : 0x8000'0000'0000'0000uLL;
                return typecheck_ptroffset(pExpr, pOperandA, pOperandB, uHighBitSetIfSub, pTCStatement, pTCContext, eExpectation);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, CERR_NOT_A_VALID_OPERATION_ON_POINTER,
                    "typecheck_regular_binary_op_dispatch() : cannot do this operation between a pointer type and a numeric integral."
                    " Expected: add or sub (or modulo add or sub)");
            }
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_NOT_A_VALID_OPERATION_ON_POINTER,
            "typecheck_regular_binary_op_dispatch() : unknown operation when involving a pointer as first operand");
        }

    } else if (bIsPointerB && !bIsIntegralB) {

        return_error(pExpr, pTCStatement, pTCContext, CERR_NOT_A_VALID_OPERATION_ON_POINTER,
            "typecheck_regular_binary_op_dispatch() : unknown operation when involving a pointer as second operand");

    } else if (bIsIntegralA && bIsIntegralB) { // both integrals (including rawptr). One of them at least non-numeric (such as rawptr, codepoint, bool...).             

        if (uOp == ETOK_LEFT_SHIFT || uOp == ETOK_RIGHT_SHIFT) {

            // TODO: cast operandB (shift amount) to u32 ? chose resulting type == type of A. invoke TC shift.
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_regular_binary_op_dispatch() : shift operator on raw integral not yet implemented.");

        } else if ((bIsPointerA && !bIsPointerB) && (uOp == ETOK_ADD || uOp == ETOK_MODULO_ADD || uOp == ETOK_SUB || uOp == ETOK_MODULO_SUB)) {
            // A is rawptr, B is other integral => allowed A+B, A-B, A+%B, A-%B as ptroffsets
            u64 uHighBitSetIfSub = (uOp == ETOK_ADD || uOp == ETOK_MODULO_ADD) ? 0uLL : 0x8000'0000'0000'0000uLL;
            return typecheck_ptroffset(pExpr, pOperandA, pOperandB, uHighBitSetIfSub, pTCStatement, pTCContext, eExpectation);

        } else if ((bIsPointerB && !bIsPointerA) && (uOp == ETOK_ADD)) {
            // A is rawptr, B is other integral => allowed B+A, B+%A as ptroffsets
            return typecheck_ptroffset(pExpr, pOperandB, pOperandA, 0u, pTCStatement, pTCContext, eExpectation);

        } else if (bIsPointerA && bIsPointerB && (uOp == ETOK_SUB || uOp == ETOK_MODULO_SUB)) {
            // Both rawptr => Allowed A-B, A-%B as ptrdiffs
            return typecheck_ptrdiff(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext, eExpectation);

        } else if (uOp == ETOK_MODULO_ADD || uOp == ETOK_MODULO_SUB || uOp == ETOK_BIT_AND || uOp == ETOK_BIT_OR || uOp == ETOK_BIT_XOR) {

            // TODO: find a matching check and function between integrals when at least one is raw.
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_regular_binary_op_dispatch() : modulo add/sub, or bitwise ops on raw integral types not yet implemented.");

        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_NOT_A_VALID_OPERATION_ON_RAW_INTEGRAL,
                "typecheck_regular_binary_op_dispatch() : operations other than shift, modulo add/sub, or bitwise ops not supported when involving raw integral types.");
        }

    } else if (bIsVecOfNumericA || bIsVecOfNumericB) {

        // TODO: vector FP and vector integral (when same slot-count), doing often hardware-enhanced pairwise-computations
        //   on slots. Think about the possibility of keeping auto-checks within those, or advertise and warn about vector ops
        //   being 'unguarded'. Think about the possibility to allow implicit cast of slot contents as we do for scalar ?

        // TODO: vector times scalar ; vector div scalar ; Probably scalar times vector ; maybe scalar div vector ?

        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_regular_binary_op_dispatch() : vector of numeric operations not yet implemented");

    } else if (bIsVecOfIntegralA || bIsVecOfIntegralB) {

        // TODO: vector integral
        //   on slots. Think about the possibility of keeping auto-checks within those, or advertise and warn about vector ops
        //   being 'unguarded'. Think about the possibility to allow implicit cast of slot contents as we do for scalar ?
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_regular_binary_op_dispatch() : vector of raw integrals operations not yet implemented");

    } else if (bIsIntegralA || bIsIntegralB) { // remaining integral or raw integral cases

        return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_MIX_RAW_INTEGRAL_WITH_NON_INTEGRAL_IN_OP,
            "typecheck_regular_binary_op_dispatch() : cannot mix raw integral with non-integral in an operation");

    } else {

        // TODO: concatenation infix op for string and arrays ??? require 'context' when non-constant, and an arbitrary choice of
        //    scratchArena to use, maybe ?

        // TODO: operator overloading allowed on all-but-core types ? Think some more about overloading vs. finding the overload
        //    in our declaration-order-independent scheme, vs. our will to avoid polymorphic 'mismatch' (for polymorphic functions
        //    or worse, polymorphic 'type'-functions invoked from places where knowledge about overloads could differ). Maybe allow
        //    operator overloading only coupled to the user-type declaration in some way. 
        // **Important Note**: our problem with overload finding within order-independent-decls is moot if the matter was
        //    finding an overload "or fail to compile". It becomes a concern only when the matter is finding an overload
        //    **or default to something else** => see below. (that's also the reason of our idea of disallowing arguments
        //    distinct-only by types which are default-castable to one-another in function overloading).
        
        // TODO: same typed distinct-alias-of-core (or distinct-alias-of-array ?) shared by both operands fallback to an
        //    as-if-core resolution if not overloaded ? (+think about the possibility of explicitly 'disallowing' the
        //    default-to-core on overload decl). Do not "simply" dismiss the idea of having a fallback-to-core on aliases :
        //    rethink this wrt. set & maps and default-equals and default-hash in particular. Then rethink this wrt.
        //    whatever we decide for custom 'iteration'. Think about the possibility of overloading cast-ops.
        //    Keep in mind the '#batch ... #endbatch' idea ? disallow conditionals in batches ?? (tough...) ;

        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_regular_binary_op_dispatch() : non-both numeric or integral (or vec thereof) not yet implemented");
    }
}

// typechecks regular unary ops (except boolean not, and unary plus)
local_func ETCResult typecheck_regular_unary_op_dispatch(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperand,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperand->pTCNode));

    if (pOperand->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) { // both compints => solve on TC-side.
        return typecheck_regular_unary_op_on_compint(pExpr, uOp, pOperand, pTCStatement, pTCContext);
    }

    bool bIsNumeric, bIsIntegral, bIsVecOfNumeric, bIsVecOfIntegral;
    bool bIsPointer, bIsBool, bIsCodePoint, bIsString;
    get_common_type_flags(pOperand->pIntrinsicValue->pType, &bIsNumeric, &bIsIntegral,
        &bIsVecOfNumeric, &bIsVecOfIntegral,
        &bIsPointer, &bIsBool, &bIsCodePoint, &bIsString);

    Assert_(is_value_tc_only(pOperand->pIntrinsicValue) || ir_is_valid_param_(pOperand->pIntrinsicValue->info.uIRandMetaFlags));

    bool bIsKnownConst = is_value_tc_const(pOperand->pIntrinsicValue);
    Assert_(eExpectation == EExpectedExpr::EXPECT_REGULAR || bIsKnownConst); // our expectation was passed on to Tc of each params => neither should be non-const if we expect const.
    Assert_(bIsKnownConst || does_tc_ctx_allow_runtime(pTCContext));
    Assert_(bIsKnownConst || get_tc_ctx_proc_result(pTCContext) != 0);
    Assert_(bIsKnownConst || pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);

    EIRResult eResult;
    IRInfo resultingInfo;

    if (uOp == ETOK_UNARY_MINUS) {
        if (bIsNumeric) {
            if (bIsIntegral) {
                if (is_signed_(pOperand->pIntrinsicValue->pType)) {
                    eResult = ir_emit_or_solve_unary_minus_integral(get_ir_format(pOperand->pIntrinsicValue->pType),
                        pOperand->pIntrinsicValue->info, EIntSemantics::EINT_SEMANTIC_SIGNED, pExpr, pTCStatement, pTCContext, &resultingInfo);
                } else {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_SIGNED_INTEGRAL_TYPE,
                        "typecheck_regular_unary_op_dispatch() : unary-minus operator cannot apply to unsigned integrals");
                }
            } else {
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_regular_unary_op_dispatch() : unary-minus operator on floating-point not yet implemented");
            }
        } else {
            // TODO: allow vec of numeric ?
            return_error(pExpr, pTCStatement, pTCContext, CERR_OPERATOR_REQUIRES_NUMERIC_OPERANDS,
                "unary-minus operator requires a numeric operand (floating point or non-raw integral)");
        }
    } else { Assert_(uOp == ETOK_UNARY_MODMINUS);
        if (bIsIntegral) {
            eResult = ir_emit_or_solve_unary_minus_integral(get_ir_format(pOperand->pIntrinsicValue->pType),
                pOperand->pIntrinsicValue->info, EIntSemantics::EINT_SEMANTIC_MODULO_ARITH, pExpr, pTCStatement, pTCContext, &resultingInfo);
        } else {
            // TODO: allow vec of integral ?
            return_error(pExpr, pTCStatement, pTCContext, CERR_OPERATOR_REQUIRES_RAW_OR_INTEGRAL_OPERANDS,
                "typecheck_regular_unary_op_dispatch() : unary-mod-minus operator requires integral or raw operands");
        }
    }

    if (eResult == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A)
        return set_tc_success_with_same_value_as_final_of(pOperand, pExpr);
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        NodeValue* pResultingValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResultingValue->pType = pOperand->pIntrinsicValue->pType;
        pResultingValue->info = resultingInfo;
        return set_node_typecheck_expr_success(pExpr->pTCNode);
    } else {
        return_error(pExpr, pTCStatement, pTCContext, u16(eResult),
            "typecheck_regular_unary_op_integral() : IR Solver determined invalid op.");
    }
}



#endif // LOCLIB_TC_STD_OPS_EVAL_H_


