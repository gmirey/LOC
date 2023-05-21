#pragma once 

#ifndef LOCLIB_TC_CONST_EVALS_H_
#define LOCLIB_TC_CONST_EVALS_H_

local_func ETCResult make_compint_result_with_payload(u64 uPayload, TmpTCNode* pExpr, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(uPayload != COMPINT_FLAG_IS_NEGATIVE); // would encore NEGATIVE-ZERO, hich is forbidden
    NodeValue* pResultingValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
    pResultingValue->pType = g_pCoreTypesInfo[ECORETYPE_COMPINT];
    pResultingValue->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST;
    pResultingValue->info.metaValue.knownValue._payload = uPayload;
    return set_node_typecheck_expr_success(pExpr->pTCNode);
}

local_func ETCResult typecheck_compint_pow_or_shift_by_constant(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));

    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(is_node_already_type_casted(pOperandB->pTCNode));
    Assert_(pOperandB->pFinalValue->pType == g_pCoreTypesInfo[ECORETYPE_INT] || pOperandB->pFinalValue->pType == g_pCoreTypesInfo[ECORETYPE_NAT]);
    Assert_(is_value_tc_const(pOperandB->pFinalValue));
    Assert_(is_value_known_or_nyka(pOperandB->pFinalValue));
    Assert_(!is_value_nyka_or_has_nyka(pOperandB->pFinalValue));
    Assert_(is_value_known_embd(pOperandB->pFinalValue));
    
    u64 uCompintPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    i32 iPayloadB = i32(pOperandB->pFinalValue->info.metaValue.knownValue.uEmbeddedValue);

    if (iPayloadB < 0) {
        if (uOp == ETOK_POW) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_NATURAL_NUMBER,
                "typecheck_compint_pow_or_shift_by_constant() : integral powers require a natural number as exponent (ie non-negative integer)."
                " Otherwise result would need an encoding as floating-point, and thus a known floating-point type as first operand to correctly typecheck.");
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_NATURAL_NUMBER,
                "typecheck_compint_pow_or_shift_by_constant() : integral shift-operators require a natural number as second operand (ie non-negative integer).");
        }
    }

    // TODO: max legs being a compilation param ??
    if (iPayloadB < COMPNAT_MAX_LEGS * 64u)
    {
        if (iPayloadB == 0u) {
            if (uOp == ETOK_POW) { // A ^ 0 == 1, whatever A (even if 0 ^ 0, by convention)
                return make_compint_result_with_payload(1uLL << COMPINT_VALUE_SHIFT_WHENSMALL, pExpr, pTCStatement, pTCContext);
            } else { // A << 0 == A or A >> 0 == A
                return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
            }
        } else if (uCompintPayloadA == 0uLL) { // 0^n, n!=0 == 0, 0 << n == 0 ; 0 >> n == 0
            return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
        } else if (iPayloadB == 1u && uOp == ETOK_POW) { // A ^ 1 == A, whatever A
            return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
        }

        if ((uCompintPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            i64 iValueA = i64(uCompintPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL);
            u64 uIsNegA = uCompintPayloadA & COMPINT_FLAG_IS_NEGATIVE;
            if (uIsNegA)
                iValueA = -iValueA;
            i64 iValueResult;
            if (uOp == ETOK_RIGHT_SHIFT) {
                if (iPayloadB < 64u)
                    iValueResult = iValueA >> iPayloadB;
                else
                    iValueResult = uIsNegA ? -1uLL : 0uLL;
                u64 uAbsResult = u64(iValueResult);
                u64 uIsNegResult = 0u;
                if (iValueResult < 0u) {
                    uAbsResult = u64(-iValueResult);
                    uIsNegResult = COMPINT_FLAG_IS_NEGATIVE;
                }
                return make_compint_result_with_payload((uAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);

            } else if (uOp == ETOK_LEFT_SHIFT) {
                i64 iValueHigh = uIsNegA ? 0xFFFF'FFFF'FFFF'FFFFuLL : 0uLL;
                if (iPayloadB < 64u) {
                    iValueResult = iValueA << iPayloadB;
                    iValueHigh = iValueA >> (64-iPayloadB) | (iValueHigh << iPayloadB);
                } else {
                    iValueResult = 0uLL;
                    if (iPayloadB < 128u) {
                        iValueHigh = iValueA << (iPayloadB-64u);
                    } else {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_compint_pow_or_shift_by_constant() : non-embedded left-shift result not yet implemented.");
                    }
                }
                if (iValueHigh == 0uLL) {
                    if (iValueResult & 0xE000'0000'0000'0000uLL) {
                        // TODO
                        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                            "typecheck_compint_pow_or_shift_by_constant() : non-embedded left-shift result not yet implemented.");
                    } else {
                        return make_compint_result_with_payload(u64(iValueResult) << COMPINT_VALUE_SHIFT_WHENSMALL, pExpr, pTCStatement, pTCContext);
                    }
                } else if (iValueHigh == 0xFFFF'FFFF'FFFF'FFFFuLL && (iValueResult & 0x8000'0000'0000'0000uLL)) {
                    u64 uAbsResult = u64(-iValueResult);
                    return make_compint_result_with_payload((uAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE, pExpr, pTCStatement, pTCContext);
                } else {
                    // TODO
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_compint_pow_or_shift_by_constant() : non-embedded left-shift result not yet implemented.");
                }

            } else { Assert_(uOp == ETOK_POW);
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_compint_pow_or_shift_by_constant() : pow not yet implemented (embedded case).");
            }

        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_compint_pow_or_shift_by_constant() : not yet implemented when non-embedded.");
        }
    } else {
        return_error(pExpr, pTCStatement, pTCContext, CERR_COMPINT_TOO_LARGE,
            "typecheck_compint_pow_or_shift_by_constant() : too large second operand for pow or shift.");
    }
}

local_func ETCResult tc_do_compint_add_or_sub(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, u64 uNegFlagIfSub,
                                              TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));
    
    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads:

    if (uPayloadB == 0uLL) { // A +- 0 == A
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadA == 0uLL) {
        if (uNegFlagIfSub == 0uLL) { // 0 + B == B
            return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
        } else {                     // 0 - B == -B
            return make_compint_result_with_payload(uPayloadB ^ COMPINT_FLAG_IS_NEGATIVE, pExpr, pTCStatement, pTCContext);
        }
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    Assert_(COMPINT_FLAG_IS_NEGATIVE == uNegFlagIfSub || 0uLL == uNegFlagIfSub);
    u64 uIsNegB = (uPayloadB & COMPINT_FLAG_IS_NEGATIVE) ^ uNegFlagIfSub;

    if (uIsNegA == uIsNegB) {
        if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {

            u64 uAbsValueA = uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;
            
            if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
                u64 uAbsValueB = uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
                u64 uAbsResult = uAbsValueA+uAbsValueB;
                if (uAbsResult & 0xE000'0000'0000'0000uLL) {
                    // TODO
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_do_compint_add_or_sub() : non-embeddable result not yet implemented.");
                } else {
                    return make_compint_result_with_payload((uAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegA, pExpr, pTCStatement, pTCContext);
                }
            } else {
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_do_compint_add_or_sub() : not yet implemented when adding abs-values and non-embedded.");
            }
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_add_or_sub() : not yet implemented when adding abs-values and non-embedded.");
        }
    } else {
        if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {

            u64 uAbsValueA = uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;

            if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
                u64 uAbsValueB = uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
                u64 uAbsResult;
                u64 uIsNegResult;
                if (uAbsValueA > uAbsValueB) {
                    uAbsResult = uAbsValueA - uAbsValueB;
                    uIsNegResult = uIsNegA;
                } else {
                    uAbsResult = uAbsValueB - uAbsValueA;
                    uIsNegResult = (uAbsValueA < uAbsValueB) ? uIsNegB : 0uLL;
                }
                if (uAbsResult & 0xE000'0000'0000'0000uLL) {
                    // TODO
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_do_compint_add_or_sub() : non-embeddable result not yet implemented.");
                } else {
                    return make_compint_result_with_payload((uAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
                }
            } else {
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_do_compint_add_or_sub() : not yet implemented when subtracting abs-values and non-embedded");
            }
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_add_or_sub() : not yet implemented when subtracting abs-values and non-embedded");
        }
    }
}

local_func ETCResult tc_do_compint_mul(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads:

    if (uPayloadA == 0uLL) {            // 0 * B == 0 == A
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == 0uLL) {     // A * 0 == 0 == B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    } else if (uPayloadA == (1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)) {      // 1 * B == B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    } else if (uPayloadB == (1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)) {      // A * 1 == A
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadA == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) {   // -1 * B == -B
        return make_compint_result_with_payload(uPayloadB ^ COMPINT_FLAG_IS_NEGATIVE, pExpr, pTCStatement, pTCContext);
    } else if (uPayloadB == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) {   // A * -1 == -A
        return make_compint_result_with_payload(uPayloadA ^ COMPINT_FLAG_IS_NEGATIVE, pExpr, pTCStatement, pTCContext);
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegResult = uIsNegA ^ uIsNegB;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            u64 uAbsValueA = uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueB = uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueHiResult;
            u64 uAbsResult = MulWithHigh64(uAbsValueA, uAbsValueB, &uAbsValueHiResult);
            if ((uAbsResult & 0xE000'0000'0000'0000uLL) || uAbsValueHiResult) {
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_do_compint_mul() : non-embeddable result not yet implemented.");
            } else {
                return make_compint_result_with_payload((uAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
            }
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_mul() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_mul() : not yet implemented when non-embedded");
    }
}

local_func ETCResult tc_do_compint_quo(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads - also div-by-zero check is there !!

    if (uPayloadA == 0uLL) {            // 0 /% B == 0 == A
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == 0uLL) {     // A /% 0 => ERROR
        return_error(pExpr, pTCStatement, pTCContext, CERR_DIVISION_BY_ZERO, "tc_do_compint_quo() : cannot divide by zero");
    } else if (uPayloadB == (1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)) { // A /% 1 == A
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) { // A /% -1 == -A
        return make_compint_result_with_payload(uPayloadA ^ COMPINT_FLAG_IS_NEGATIVE, pExpr, pTCStatement, pTCContext);
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegResult = uIsNegA ^ uIsNegB;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            u64 uAbsValueA = uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueB = uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueResult = uAbsValueA / uAbsValueB;
            Assert_(0uLL == (uAbsValueResult & 0xE000'0000'0000'0000uLL));
            return make_compint_result_with_payload(u64(uAbsValueResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_quo() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_quo() : not yet implemented when non-embedded");
    }
}

local_func ETCResult tc_do_compint_rem(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads - also div-by-zero check is there !!

    if (uPayloadA == 0uLL) {            // 0 %% B == 0
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == 0uLL) {     // A %% 0 => ERROR
        return_error(pExpr, pTCStatement, pTCContext, CERR_DIVISION_BY_ZERO, "tc_do_compint_rem() : cannot divide by zero and find a valid remainder");
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegResult = uIsNegB;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            u64 uAbsValueA = uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueB = uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueResult = uAbsValueA % uAbsValueB;
            Assert_(0uLL == (uAbsValueResult & 0xE000'0000'0000'0000uLL));
            return make_compint_result_with_payload(u64(uAbsValueResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_rem() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_rem() : not yet implemented when non-embedded");
    }
}

local_func ETCResult tc_do_compint_mod(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads - also div-by-zero check is there !!

    if (uPayloadA == 0uLL) {            // 0 % B == 0
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == 0uLL) {     // A % 0 => ERROR
        return_error(pExpr, pTCStatement, pTCContext, CERR_DIVISION_BY_ZERO, "tc_do_compint_mod() : cannot mod by zero");
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegResult = uIsNegB;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            u64 uAbsValueA = uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueB = uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL;
            u64 uAbsValueResult = uAbsValueA % uAbsValueB;
            if (uIsNegA != uIsNegB)
                uAbsValueResult = uAbsValueB - uAbsValueResult;
            Assert_(0uLL == (uAbsValueResult & 0xE000'0000'0000'0000uLL));
            return make_compint_result_with_payload(u64(uAbsValueResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_mod() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_mod() : not yet implemented when non-embedded");
    }
}

local_func ETCResult tc_do_compint_bitand(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads:

    if (uPayloadA == 0uLL) {            // 0 & B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == 0uLL) {     // A & 0
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    } else if (uPayloadA == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) { // -1 & B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    } else if (uPayloadB == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) { // A & -1
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            i64 iValueA = i64(uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL);
            i64 iValueB = i64(uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL);
            if (uIsNegA)
                iValueA = -iValueA;
            if (uIsNegB)
                iValueB = -iValueB;
            i64 iAbsResult = iValueA & iValueB;
            u64 uIsNegResult = 0uLL;
            if (iAbsResult < 0u) {
                iAbsResult = -iAbsResult;
                uIsNegResult = COMPINT_FLAG_IS_NEGATIVE;
            }
            Assert_(0LL == (iAbsResult & 0xE000'0000'0000'0000LL));
            return make_compint_result_with_payload(u64(iAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_bitand() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_bitand() : not yet implemented when non-embedded");
    }
}

local_func ETCResult tc_do_compint_bitor(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads:

    if (uPayloadA == 0uLL) {            // 0 | B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    } else if (uPayloadB == 0uLL) {     // A | 0
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadA == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) { // -1 | B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    } else if (uPayloadB == ((1uLL << COMPINT_VALUE_SHIFT_WHENSMALL)|COMPINT_FLAG_IS_NEGATIVE)) { // A | -1
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            i64 iValueA = i64(uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL);
            i64 iValueB = i64(uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL);
            if (uIsNegA)
                iValueA = -iValueA;
            if (uIsNegB)
                iValueB = -iValueB;
            i64 iAbsResult = iValueA | iValueB;
            u64 uIsNegResult = 0uLL;
            if (iAbsResult < 0u) {
                iAbsResult = -iAbsResult;
                uIsNegResult = COMPINT_FLAG_IS_NEGATIVE;
            }
            Assert_(0LL == (iAbsResult & 0xE000'0000'0000'0000LL));
            return make_compint_result_with_payload(u64(iAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_bitor() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_bitor() : not yet implemented when non-embedded");
    }
}

local_func ETCResult tc_do_compint_bitxor(TmpTCNode* pExpr, TmpTCNode* pOperandA, TmpTCNode* pOperandB, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    u64 uPayloadA = pOperandA->pIntrinsicValue->info.metaValue.knownValue._payload;
    u64 uPayloadB = pOperandB->pIntrinsicValue->info.metaValue.knownValue._payload;

    // The following shortcuts prevent constructing new values and/or new large payloads:

    if (uPayloadA == 0uLL) {            // 0 ^ B
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandB, pExpr);
    } else if (uPayloadB == 0uLL) {     // A ^ 0
        return set_tc_success_with_same_value_as_intrinsic_of(pOperandA, pExpr);
    }

    u64 uIsNegA = uPayloadA & COMPINT_FLAG_IS_NEGATIVE;
    u64 uIsNegB = uPayloadB & COMPINT_FLAG_IS_NEGATIVE;
    if ((uPayloadA & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
        if ((uPayloadB & COMPINT_SIZE_MASK) == COMPINT_SIZE_SMALL_EMBD) {
            i64 iValueA = i64(uPayloadA >> COMPINT_VALUE_SHIFT_WHENSMALL);
            i64 iValueB = i64(uPayloadB >> COMPINT_VALUE_SHIFT_WHENSMALL);
            if (uIsNegA)
                iValueA = -iValueA;
            if (uIsNegB)
                iValueB = -iValueB;
            i64 iAbsResult = iValueA ^ iValueB;
            u64 uIsNegResult = 0uLL;
            if (iAbsResult < 0u) {
                iAbsResult = -iAbsResult;
                uIsNegResult = COMPINT_FLAG_IS_NEGATIVE;
            }
            Assert_(0LL == (iAbsResult & 0xE000'0000'0000'0000LL));
            return make_compint_result_with_payload(u64(iAbsResult << COMPINT_VALUE_SHIFT_WHENSMALL)|uIsNegResult, pExpr, pTCStatement, pTCContext);
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_compint_bitxor() : not yet implemented when non-embedded");
        }
    } else {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_compint_bitxor() : not yet implemented when non-embedded");
    }
}

// typecheks any regular binary op (eg. all of them except boolean 'and', 'or', and comparisons) in case both operands are compint => result will also be compint.
local_func ETCResult typecheck_regular_binary_op_full_compint(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperandA, TmpTCNode* pOperandB,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperandA->pTCNode));
    Assert_(is_node_already_typechecked(pOperandB->pTCNode));
    Assert_(pOperandA->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pOperandB->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperandA->pIntrinsicValue));
    Assert_(is_value_tc_only(pOperandB->pIntrinsicValue));

    bool bOpIsPow = (uOp == ETOK_POW);
    bool bOpIsShift = (uOp == ETOK_LEFT_SHIFT || uOp == ETOK_RIGHT_SHIFT);
    if (bOpIsPow || bOpIsShift) {
        // Reuse compint shift or pow by any known constant integral (staying compint, but with 'int'-casted const operand B)...
        ETCResult checkCastB = do_implicit_cast(pOperandB, g_pCoreTypesInfo[ECORETYPE_INT], pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT);
        success_or_return_wait_or_error(checkCastB, pExpr->pTCNode);
        return typecheck_compint_pow_or_shift_by_constant(pExpr, uOp, pOperandA, pOperandB, pTCStatement, pTCContext);
    } else {
        switch (uOp) {

            case ETOK_ADD: {
                return tc_do_compint_add_or_sub(pExpr, pOperandA, pOperandB, 0, pTCStatement, pTCContext);
            } break;

            case ETOK_MODULO_ADD: {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_SIZED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_full_compint() : the modulo-add operator '+%' requires a sized integral operand."
                    " Currently those are two compints, which is a comptime-only, arbitrarily-large integral format. Cast at least one to a sized integral type.");
            } break;

            case ETOK_SUB: {
                return tc_do_compint_add_or_sub(pExpr, pOperandA, pOperandB, COMPINT_FLAG_IS_NEGATIVE, pTCStatement, pTCContext);
            } break;

            case ETOK_MODULO_SUB: {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_SIZED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_full_compint() : the modulo-sub operator '-%' requires a sized integral operand."
                    " Currently those are two compints, which is a comptime-only, arbitrarily-large integral format. Cast at least one to a sized integral type.");
            } break;

            case ETOK_MUL: {
                return tc_do_compint_mul(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_MODULO_MUL: {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_SIZED_INTEGRAL_TYPE,
                    "typecheck_regular_binary_op_full_compint() : the modulo-mul operator '*%' requires a sized integral operand."
                    " Currently those are two compints, which is a comptime-only, arbitrarily-large integral format. Cast at least one to a sized integral type.");
            } break;

            case ETOK_DIV: {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_FLOATING_POINT_TYPE,
                    "typecheck_regular_binary_op_full_compint() : the division operator '/' has a semantic image in the field of Rational Numbers."
                        " If you want the integer quotient instead, use the '/%' operator. Otherwise, "
                        "use at least one floating-point-typed operand to allow the compiler to decide of a resulting type.");
            } break;

            case ETOK_INT_QUOTIENT: {
                return tc_do_compint_quo(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_INT_REMAINDER: {
                return tc_do_compint_rem(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_MOD: {
                return tc_do_compint_mod(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_BIT_AND: {
                return tc_do_compint_bitand(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_BIT_OR: {
                return tc_do_compint_bitor(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_BIT_XOR: {
                return tc_do_compint_bitxor(pExpr, pOperandA, pOperandB, pTCStatement, pTCContext);
            } break;

            case ETOK_CONCAT: {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_STRING_OR_ARRAY_TYPE,
                    "typecheck_regular_binary_op_full_compint() : concat operator requires string or array type.");
            } break;

            default:
                Assume_(false);
                return ETCResult::ETCR_ERROR;
        }
    }
}

local_func ETCResult typecheck_regular_unary_op_on_compint(TmpTCNode* pExpr, u8 uOp, TmpTCNode* pOperand,
    TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(is_node_tc_not_started(pExpr->pTCNode)); // otherwise caller should have shortcut this call already
    Assert_(is_node_already_typechecked(pOperand->pTCNode));
    Assert_(pOperand->pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_only(pOperand->pIntrinsicValue));

    Assert_(uOp == ETOK_UNARY_MINUS || uOp == ETOK_UNARY_MODMINUS); // We accept modminus just as fine, for convenience...

    u64 uCompintPayload = pOperand->pIntrinsicValue->info.metaValue.knownValue._payload;
    return make_compint_result_with_payload(uCompintPayload ^ COMPINT_FLAG_IS_NEGATIVE, pExpr, pTCStatement, pTCContext);
}

#endif // LOCLIB_TC_CONST_EVALS_H_


