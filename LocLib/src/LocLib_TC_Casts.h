#pragma once 

#ifndef LOCLIB_TC_CASTS_H_
#define LOCLIB_TC_CASTS_H_

#include "LocLib_TypeCheckerCore.h"
#include "LocLib_IR_SolverInterface.h"

enum EIntMismatchChoice {
    EINT_CHOOSE_ONLY_EXACT_SAME,
    EINT_CHOOSE_ONLY_SAME_FOOTPRINT,
    EINT_CHOOSE_LARGEST,
    EINT_CHOOSE_SMALLEST,
};

local_func const TypeInfo_Integral* get_resulting_integral_type(
    const TypeInfo_Integral* pTypeA, const TypeInfo_Integral* pTypeB, TCContext* pTCContext,
    u16* outErr, EIntMismatchChoice eChoice = EIntMismatchChoice::EINT_CHOOSE_LARGEST)
{
    ECoreType coreA = get_core_type(pTypeA);
    ECoreType coreB = get_core_type(pTypeB);

    // 'compint' is not explicitely sized => always accept the other operand if one is compint
    if (coreA == ECoreType::ECORETYPE_COMPINT)
        return pTypeB; // note that 'B' can very well be also compint here, but in that case it is ok to return compint.

    else if (coreB == ECoreType::ECORETYPE_COMPINT)
        return pTypeA;

    else {

        if (pTypeB == pTypeB)
            return pTypeA;

        if (is_signed(pTypeA) != is_signed(pTypeB) || is_raw_integral(pTypeA) != is_raw_integral(pTypeB)) {
            // TODO ?
            platform_log_error("*** get_resulting_integral_type() has not yet a real implementation when signedness-kind differ");
            *outErr = FERR_NOT_YET_IMPLEMENTED;
            return 0;
        }

        // until implemented, we're left with the hopefully common case:
        //    barring compints (already set aside above), we have same signedness-kind (both signed or both unsigned or both raw)
        Assert_(is_unsigned(pTypeA) == is_unsigned(pTypeB));

        // first, compare footprints:
        u8 uSizeA = get_log2_of_scalar_bytes(pTypeA);
        u8 uSizeB = get_log2_of_scalar_bytes(pTypeB);
        if (uSizeA > uSizeB) { // in case of a footprint difference (here, A larger than B), we can chose based on that
            if (eChoice == EIntMismatchChoice::EINT_CHOOSE_LARGEST)
                return pTypeA;
            else if (eChoice == EIntMismatchChoice::EINT_CHOOSE_SMALLEST)
                return pTypeB;
            else {
                platform_log_error("*** get_resulting_integral_type() : "
                    "footprint size mismatch when choice of same_exact or same_footprint");
                *outErr = CERR_INVALID_CAST;
                return 0;
            }

        } else if (uSizeA < uSizeB) {  // in case of a footprint difference (here, B larger than A), we can chose based on that
            if (eChoice == EIntMismatchChoice::EINT_CHOOSE_LARGEST)
                return pTypeB;
            else if (eChoice == EIntMismatchChoice::EINT_CHOOSE_SMALLEST)
                return pTypeA;
            else {
                platform_log_error("*** get_resulting_integral_type() : "
                    "footprint size mismatch when choice of same_exact or same_footprint");
                *outErr = CERR_INVALID_CAST;
                return 0;
            }

        } else { // same-footprint but distinct types ?

            // We really have quite some possibilities here:
            // 1) aliases : INT, NAT, BYTE, REG, RAWPTR...
            // 2) same footprint but distinct max/semantics : 'CODEPOINT' vs u32 (or its aliases), 'BOOL' vs u8 (or its aliases)

            // We'll deal with the 'one-of-them-is-codepoint' case first.
            //    (ensured not happening at the same time than 'BOOL' below, by virtue of them having distinct footprints)
            if (coreA == ECORETYPE_CODEPOINT) {
                if (eChoice == EINT_CHOOSE_LARGEST || eChoice == EINT_CHOOSE_ONLY_SAME_FOOTPRINT)
                    return pTypeB;
                else if (eChoice == EINT_CHOOSE_SMALLEST)
                    return pTypeA;
                else {
                    platform_log_error("*** get_resulting_integral_type() : "
                        "max size mismatch (involving CODEPOINT) when choice of same_exact");
                    *outErr = CERR_INVALID_CAST;
                    return 0;
                }
            } else if (coreB == ECORETYPE_CODEPOINT) {
                if (eChoice == EINT_CHOOSE_LARGEST || eChoice == EINT_CHOOSE_ONLY_SAME_FOOTPRINT)
                    return pTypeA;
                else if (eChoice == EINT_CHOOSE_SMALLEST)
                    return pTypeB;
                else {
                    platform_log_error("*** get_resulting_integral_type() : "
                        "max size mismatch (involving CODEPOINT) when choice of same_exact");
                    *outErr = CERR_INVALID_CAST;
                    return 0;
                }

            // Then, we'll deal with the 'one-of-them-is-bool' case.
            //    (ensured not happening at the same time than 'CODEPOINT' above, by virtue of them having distinct footprints)
            } else if (coreA == ECORETYPE_BOOL) {
                if (eChoice == EINT_CHOOSE_LARGEST || eChoice == EINT_CHOOSE_ONLY_SAME_FOOTPRINT)
                    return pTypeB;
                else if (eChoice == EINT_CHOOSE_SMALLEST)
                    return pTypeA;
                else {
                    platform_log_error("*** get_resulting_integral_type() : "
                        "max size mismatch (involving BOOL) when choice of same_exact");
                    *outErr = CERR_INVALID_CAST;
                    return 0;
                }
            } else if (coreB == ECORETYPE_BOOL) {
                if (eChoice == EINT_CHOOSE_LARGEST || eChoice == EINT_CHOOSE_ONLY_SAME_FOOTPRINT)
                    return pTypeA;
                else if (eChoice == EINT_CHOOSE_SMALLEST)
                    return pTypeB;
                else {
                    platform_log_error("*** get_resulting_integral_type() : "
                        "max size mismatch (involving BOOL) when choice of same_exact");
                    *outErr = CERR_INVALID_CAST;
                    return 0;
                }

            // ...finally, we're left here with true aliases (or aliases-for-current-target) to each other.
            } else {
                // first, always prefer more specific in case of INT or NAT (both with a 'don't really care' semantic).
                if (coreA == ECORETYPE_INT || coreA == ECORETYPE_NAT)
                    return pTypeB;
                else if (coreB == ECORETYPE_INT || coreB == ECORETYPE_NAT)
                    return pTypeA;
                // next, always try to conserve 'RAWPTR' status over other integrals.
                else if (coreA == ECORETYPE_RAWPTR)
                    return pTypeA;
                else if (coreB == ECORETYPE_RAWPTR)
                    return pTypeB;
                // finally, just give up and accept the 'least unspecific' (as arbitrarily highest position in the enum...)
                else if (coreA > coreB)
                    return pTypeA;
                else
                    return pTypeB;
            }
        }
    }
}

local_func const TypeInfo_FloatingPoint* get_resulting_fp_type(const TypeInfo* pTypeA, const TypeInfo* pTypeB, TCContext* pTCContext)
{
    if (get_type_kind(pTypeA) != ETypeKind::ETYPEKIND_FLOATINGPOINT) { // B is the only floating point operand
        // => take the type of B as result (or default float if B is float literal)
        Assert_(get_type_kind(pTypeA) == ETypeKind::ETYPEKIND_INTEGRAL);
        Assert_(get_type_kind(pTypeB) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
        if (get_core_type_(pTypeB) == ECoreType::ECORETYPE_FLOAT_LIT)
            pTypeB = g_pCoreTypesInfo[ECORETYPE_F64];
        return (const TypeInfo_FloatingPoint*)pTypeB;

    } else if (get_type_kind(pTypeB) != ETypeKind::ETYPEKIND_FLOATINGPOINT) { // A is the only floating point operand
        // => take the type of A as result (or default float if A is float literal)
        Assert_(get_type_kind(pTypeB) == ETypeKind::ETYPEKIND_INTEGRAL);
        Assert_(get_type_kind(pTypeA) == ETypeKind::ETYPEKIND_FLOATINGPOINT);
        if (get_core_type_(pTypeA) == ECoreType::ECORETYPE_FLOAT_LIT)
            pTypeA = g_pCoreTypesInfo[ECORETYPE_F64];
        return (const TypeInfo_FloatingPoint*)pTypeA;

    } else { // Both operands are floating point
        const TypeInfo_FloatingPoint* pFpTypeA = (const TypeInfo_FloatingPoint*)pTypeA;
        const TypeInfo_FloatingPoint* pFpTypeB = (const TypeInfo_FloatingPoint*)pTypeB;
        ECoreType coreA = get_core_type(pFpTypeA);
        ECoreType coreB = get_core_type(pFpTypeB);
        
        if (coreA == ECoreType::ECORETYPE_FLOAT_LIT) { // A is literal
            // => take the type of B as result (or default float if B is float literal)
            if (coreB == ECoreType::ECORETYPE_FLOAT_LIT)
                pFpTypeB = (const TypeInfo_FloatingPoint*)g_pCoreTypesInfo[ECORETYPE_F64];
            return pFpTypeB;

        } else if (coreB == ECoreType::ECORETYPE_FLOAT_LIT) { // B is literal (and A is not)
            // => take the type of A as result
            return pFpTypeA;

        } else {
            if (pFpTypeA == pFpTypeB)
                return pFpTypeA;

            else { // in case of distinct types, we'll keep the one with larger size (which is the one with more exponent bits)
                // note that taking 'exponent' bits is ensured a more valid comparison in case there is an 'XFloat' here...
                u8 uExpBitsA = get_exponent_bitcount(pFpTypeA);
                u8 uExpBitsB = get_exponent_bitcount(pFpTypeB);
                if (uExpBitsA > uExpBitsB) {
                    return pFpTypeA;
                } else { Assert_(uExpBitsA < uExpBitsB);
                    return pFpTypeB;
                }
            }
        }
    }
}

local_func_inl const TypeInfo* when_no_explicit_cast_get_runtime_type(const TypeInfo* pType, TCContext* pTCContext)
{
    if (is_core_type(pType)) {
        // outside of an explicit cast, 'compint'-typed-values get inferred as default int
        if (get_core_type_(pType) == ECoreType::ECORETYPE_COMPINT)
            return g_pCoreTypesInfo[ECORETYPE_INT];
        // outside of an explicit cast, FP literals get converted to default FP
        if (get_core_type_(pType) == ECoreType::ECORETYPE_FLOAT_LIT)
            return g_pCoreTypesInfo[ECORETYPE_F64];
    }
    return pType;
}

/*
local_func void ir_get_u256_known_value_at(u64 uIROfKnownValue, IRAwareContext* pContext, u256* outValue)
{
    Assert_(ir_is_valid_param(uIROfKnownValue));
    Assert_(!ir_is_immediate(uIROfKnownValue));
    IRRepo* pRepo;
    u32 uIndex;
    SourceFileDescAndState* pSourceFile;
    EEntryKind eKind;
    ir_decode_non_imm(uIROfKnownValue, pContext, &pRepo, &uIndex, &pSourceFile, &eKind);
    Assert_(eKind != EEK_NOT_AN_ENTRY);
    Assert_(eKind != EEK_FILEWISE_VAR);
    Assert_(eKind != EEK_IS_PROCBODY_REF);
    Assert_(pRepo);
    IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));
    Assert_(irflag_is_known_non_nyka(entry.uInstrMetaFlagsAndSecondParam));
    if (irflag_is_known_embd(entry.uInstrMetaFlagsAndSecondParam)) {
        outValue->tLegs[0u] = entry.metaValue.knownValue.uEmbeddedValue;
        if (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_EMBD_NEG_EXT) {
            outValue->tLegs[1u] = 0xFFFF'FFFF'FFFF'FFFFuLL;
            outValue->tLegs[2u] = 0xFFFF'FFFF'FFFF'FFFFuLL;
            outValue->tLegs[3u] = 0xFFFF'FFFF'FFFF'FFFFuLL;
        } else {
            outValue->tLegs[1u] = 0uLL;
            outValue->tLegs[2u] = 0uLL;
            outValue->tLegs[3u] = 0uLL;
        }
    } else {
        const u64* pLegs = (u64*)entry.metaValue.knownValue.pPtrToRawData;
        outValue->tLegs[0u] = pLegs[0u];
        outValue->tLegs[1u] = pLegs[1u];
        outValue->tLegs[2u] = pLegs[2u];
        outValue->tLegs[3u] = pLegs[3u];
    }
}
*/

enum ECastKind : u8 {
    ECAST_IMPLICIT = 0u,
    ECAST_STANDARD = 1u,
    ECAST_TRANSMUTE,
    ECAST_TRUNCATE,
    ECAST_SATURATE,
};

local_func ETCResult report_cast_error_from_known_integral_to_other_integral(ERangeCheckResultReason eOutsideRange,
    TmpTCNode* pToCast, TCStatement* pTCStatement, TCContext* pTCContext)
{
    Assert_(eOutsideRange);
    switch (eOutsideRange) {
        case ERCR_NEGATIVE_VALUE_VS_NON_SIGNED: {
            return_error(pToCast, pTCStatement, pTCContext, CERR_CAST_NEGATIVE_TO_UNSIGNED,
                "cast negative int to unsigned integral format is forbidden (unless transmute or truncation)");
        } break;
        case ERCR_NEGATIVE_VALUE_VS_SPECIAL_RAW: {
            return_error(pToCast, pTCStatement, pTCContext, CERR_CAST_NEGATIVE_TO_UNSIGNED,
                "cast negative int to special raw format is always an error");
        } break;
        case ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS: {
            return_error(pToCast, pTCStatement, pTCContext, CERR_INVALID_CAST, // TODO: better err code for this
                "cast negative int to unsigned integral too large in absolute value to fit in resulting signed integral (may use truncation instead).");
        } break;

        case ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL: {
            return_error(pToCast, pTCStatement, pTCContext, CERR_INVALID_CAST, // TODO: better err code for this
                "cast int too large to fit in resulting special raw integral");
        } break;
        case ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED: {
            return_error(pToCast, pTCStatement, pTCContext, CERR_INVALID_CAST, // TODO: better err code for this
                "implicit cast positive compint being 1b too large to fit in resulting signed integral");
        } break;
        case ERCR_POSITIVE_VALUE_TOO_LARGE: {
            return_error(pToCast, pTCStatement, pTCContext, CERR_INVALID_CAST, // TODO: better err code for this
                "implicit cast positive compint being too large to fit in resulting integral");
        } break;

        case ERCR_ERROR:
        default:
        {
            return_error(pToCast, pTCStatement, pTCContext, FERR_OTHER, // TODO: better err code for this
                "implicit cast compint to integral failed for unknown reason");
        } break;
    }
}

local_func void do_convert_known_std_integral_to_compint(u8 uSrcFormat, bool bSrcSigned, u32 uSrcMetaFlags, AKnownValue srcValue,
    TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outInfo)
{
    Assert_(uSrcFormat <= 0x07u);
    Assert_(!bSrcSigned || uSrcFormat <= 0x05u);
    Assert_(uSrcMetaFlags & IRFLAG_IS_KNOWN);
    Assert_(0u == (uSrcMetaFlags & IRFLAG_HAS_NYKA));
    Assert_(uSrcMetaFlags & IRFLAG_TC_SEMANTIC_CONST);
    Assert_(0u == (uSrcMetaFlags & IRFLAG_TC_ONLY));

    if (uSrcMetaFlags & IRFLAG_IS_KNOWN_EMBD) {
        Assert_(uSrcFormat <= 0x03u);
        u64 uEmbd64 = srcValue.uEmbeddedValue;
        u64 uResultingAbs;
        u64 uResultingNegFlag = 0uLL;
        if (bSrcSigned) {
            i64 iAsSigned64;
            switch (uSrcFormat) {
                case 0x00u: { // 8b signed
                    iAsSigned64 = i64(i8(uEmbd64));
                } break;
                case 0x01u: { // 16b signed
                    iAsSigned64 = i64(i16(uEmbd64));
                } break;
                case 0x02u: { // 32b signed
                    iAsSigned64 = i64(i32(uEmbd64));
                } break;
                default: { // 64b signed or more
                    iAsSigned64 = i64(uEmbd64);
                }
            }
            if (iAsSigned64 < 0) {
                uResultingAbs = 0uLL - u64(iAsSigned64);
                uResultingNegFlag = COMPINT_FLAG_IS_NEGATIVE;
            } else {
                uResultingAbs = u64(iAsSigned64);
            }
        } else {
            uResultingAbs = uEmbd64;
        }
        if (0uLL == (uResultingAbs & 0xE000'0000'0000'0000uLL)) {
            outInfo->uIRandMetaFlags = IRFLAG_IS_KNOWN|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_TC_ONLY; // CLEANUP: TODO: CHECK: do we not require the immediate on r32 in case it fits ??
            outInfo->metaValue.knownValue.uEmbeddedValue = (uResultingAbs << COMPINT_VALUE_SHIFT_WHENSMALL) | uResultingNegFlag;
        } else {
            // TODO
            Assert(false, "do_convert_known_std_integral_to_compint() : not yet implemented when non-embeddable result");
        }
    } else {
        Assert_(uSrcFormat > 0x03u);
        // TODO
        Assert(false, "do_convert_known_std_integral_to_compint() : not yet implemented when src non embedded");
    }
}

local_func void when_range_checked_do_truncate_or_sign_extend_compint_to_integral(u64 uCompintPayload,
    const TypeInfo_Integral* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outInfo)
{
    Assert_(pResultType && pResultType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    u8 uDestFormat = get_ir_format(pResultType);
    Assert_(uDestFormat <= 0x07u);

    u64 uSizeCategory = (uCompintPayload & COMPINT_SIZE_MASK);
    if (uSizeCategory == COMPINT_SIZE_SMALL_EMBD) {
        u64 uAbsValue = uCompintPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
        u64 uAsR64 = (uCompintPayload & COMPINT_FLAG_IS_NEGATIVE) ? 0uLL - uAbsValue : uAbsValue;
        // we can safely interpret embedded compints as 'i64' here, since their abs value is only encoded on 61b.
        EIntSemantics eSemantics = EIntSemantics::EINT_SEMANTIC_SIGNED;
        if (get_ir_format(pResultType) <= 0x03u)
            eSemantics = EIntSemantics::EINT_SEMANTIC_MODULO_ARITH; 
        EIRResult eEmitResult = ir_emit_or_solve_truncate_or_extend_integral_to_integral(0x03u,
            IRInfo { u64(IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST), meta_from_payload(uAsR64) },
            eSemantics, pResultType, pTCStatement, pTCContext, outInfo);
        Assert_(eEmitResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN);
        outInfo->uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
    } else {
        // TODO
        Assert(false, "when_range_checked_do_truncate_or_sign_extend_compint_to_integral() : not yet implemented for non-embedded compint values");
    }
}

local_func ETCResult tc_do_compint_cast_to_integral_on_nodevalue(TmpTCNode* pExprIfErr, NodeValue* pSrcValue, ECastKind eCastKind,
    const TypeInfo_Integral* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outInfo)
{
    Assert_(pSrcValue);
    Assert_(pSrcValue->pType);
    Assert_(unalias(pSrcValue->pType) == g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(is_value_tc_const(pSrcValue));
    Assert_(!is_value_nyka_or_has_nyka(pSrcValue));
    Assert_(is_value_tc_only(pSrcValue));
    Assert_(get_type_kind((const TypeInfo*)pResultType) == ETypeKind::ETYPEKIND_INTEGRAL);
    
    // shortcuts compint to compint... could possibly happen if 'distinct' type aliases are involved as src or dest.
    if (pResultType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        *outInfo = pSrcValue->info;
        return ETCResult::ETCR_SUCCESS;
    }

    u64 uCompintPayload = pSrcValue->info.metaValue.knownValue.uEmbeddedValue;

    ERangeCheckResultReason eOutsideRange = ERangeCheckResultReason::ERCR_IN_RANGE;
    // truncation do not range-check (unless to special raw such as bool or codepoint)
    // we'll special-rangecheck however even for "transmute", since it requires special handling for compint sources.
    if (eCastKind != ECastKind::ECAST_TRUNCATE ||
        pResultType == g_pCoreTypesInfo[ECORETYPE_BOOL] || pResultType == g_pCoreTypesInfo[ECORETYPE_CODEPOINT]) {
        eOutsideRange = is_compint_outside_integral_range_returning_reason(uCompintPayload, pResultType, pTCContext);
    }
    
    if (!eOutsideRange) { when_cast_ok:

        when_range_checked_do_truncate_or_sign_extend_compint_to_integral(uCompintPayload, pResultType,
            pTCStatement, pTCContext, outInfo);
        return ETCResult::ETCR_SUCCESS;

    } else {
        if (eCastKind == ECastKind::ECAST_TRANSMUTE) { // transmute on compint is special:
            // we require that the resulting type is able to hold all *significant bits* of a 2's complement representation of the value.
            // otherwise, one should use explicit truncation instead.
            if (eOutsideRange == ERangeCheckResultReason::ERCR_POSITIVE_VALUE_1b_TOO_LARGE_VS_SIGNED) {
                eOutsideRange = ERangeCheckResultReason::ERCR_IN_RANGE;
                goto when_cast_ok;
            }
            if (eOutsideRange == ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_VS_NON_SIGNED &&
                    (compint_abs_equals_sign_of_format(get_ir_format(pResultType), uCompintPayload) ||
                     !is_compint_outside_integral_range_returning_reason(uCompintPayload ^ COMPINT_FLAG_IS_NEGATIVE,
                         pResultType, pTCContext))) {
                eOutsideRange = ERangeCheckResultReason::ERCR_IN_RANGE;
                goto when_cast_ok;
            }
            // otherwise fallthrough cast error

        } else if (eCastKind == ECastKind::ECAST_SATURATE) {
            // TODO
            return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "saturating case for saturating cast not yet implemented");
            // when done:
            eOutsideRange = ERangeCheckResultReason::ERCR_IN_RANGE;
            goto when_cast_ok;
        } // otherwise fallthrough cast error

        return report_cast_error_from_known_integral_to_other_integral(eOutsideRange, pExprIfErr, pTCStatement, pTCContext);
    }
}

constexpr u32 CASTFLAG_CANBE_GREATER_THAN_SPECIAL    = 0x01u;
constexpr u32 CASTFLAG_CANBE_GREATER_THAN_SIGNED     = 0x02u;
constexpr u32 CASTFLAG_CANBE_GREATER_THAN_UNSIGNED   = 0x04u;
constexpr u32 CASTFLAG_CANBE_INVALID_NEGATIVE        = 0x08u;
constexpr u32 CASTFLAG_CANBE_SMALLER_THAN_SIGNED     = 0x10u;

local_func u32 get_required_checks_from_integral_type_conversions(const TypeInfo_Integral* pSrcType, const TypeInfo_Integral* pResultType)
{
    Assert_(pSrcType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(pResultType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    bool bSignedSrc = is_signed_(pSrcType);
    bool bSignedDest = is_signed_(pResultType);
    u8 uSrcFormat = get_ir_format(pSrcType);
    u8 uDstFormat = get_ir_format(pResultType);
    Assert_(uSrcFormat <= 0x07u);
    Assert_(uDstFormat <= 0x07u);

    u32 uRequiredChecks = 0x00u;

    if (bSignedDest) {
        if (bSignedSrc) {
            if (uSrcFormat > uDstFormat)
                uRequiredChecks |= CASTFLAG_CANBE_GREATER_THAN_SIGNED|CASTFLAG_CANBE_SMALLER_THAN_SIGNED;
        } else {
            if (uSrcFormat > uDstFormat)
                uRequiredChecks |= CASTFLAG_CANBE_GREATER_THAN_SIGNED;
            else if (uSrcFormat == uDstFormat &&
                    pSrcType != g_pCoreTypesInfo[ECORETYPE_BOOL] && pSrcType != g_pCoreTypesInfo[ECORETYPE_CODEPOINT])
                uRequiredChecks |= CASTFLAG_CANBE_GREATER_THAN_SIGNED;
        }

    } else {
        if (bSignedSrc) {
            uRequiredChecks |= CASTFLAG_CANBE_INVALID_NEGATIVE;
        }
        if (pResultType == g_pCoreTypesInfo[ECORETYPE_BOOL] || pResultType == g_pCoreTypesInfo[ECORETYPE_CODEPOINT]) {
            if (uSrcFormat >= uDstFormat)
                uRequiredChecks |= CASTFLAG_CANBE_GREATER_THAN_SPECIAL;
        } else {
            if (uSrcFormat > uDstFormat)
                uRequiredChecks |= CASTFLAG_CANBE_GREATER_THAN_UNSIGNED;
        }
    }

    return uRequiredChecks;
}

local_func ETCResult tc_do_non_compint_integral_cast_to_other_integral_on_nodevalue(TmpTCNode* pExprIfErr, NodeValue* pSrcValue, ECastKind eCastKind,
    const TypeInfo_Integral* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(pSrcValue);
    Assert_(pSrcValue->pType);
    Assert_(get_type_kind(unalias(pSrcValue->pType)) == ETypeKind::ETYPEKIND_INTEGRAL);
    const TypeInfo_Integral* pSrcIntType = (const TypeInfo_Integral*)unalias(pSrcValue->pType);
    Assert_(pSrcIntType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    Assert_(!is_value_tc_only(pSrcValue));
    Assert_(get_type_kind((const TypeInfo*)pResultType) == ETypeKind::ETYPEKIND_INTEGRAL);

    u8 uSrcFormat = get_ir_format(pSrcIntType);
    Assert_(uSrcFormat <= 0x07u);
    bool bSrcSigned = is_signed(pSrcIntType);
    Assert_(eCastKind == ECastKind::ECAST_TRANSMUTE || eCastKind == ECastKind::ECAST_TRUNCATE || uSrcFormat <= 0x05u);
    Assert_(!bSrcSigned || uSrcFormat <= 0x05u);

    u32 uSrcMetaFlags = u32(pSrcValue->info.uIRandMetaFlags) & IR_STD_PARAM_MASK;

    if (pResultType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {

        if (is_value_known_or_nyka(pSrcValue)) {
            if (is_value_tc_const(pSrcValue)) {
                if (!is_value_nyka_or_has_nyka(pSrcValue)) {
                    do_convert_known_std_integral_to_compint(uSrcFormat, bSrcSigned, uSrcMetaFlags,
                        pSrcValue->info.metaValue.knownValue, pTCStatement, pTCContext, outInfo);
                    Assert_(outInfo->uIRandMetaFlags & IRFLAG_TC_ONLY);
                    return ETCResult::ETCR_SUCCESS;
                } else {
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
                        "This constant expression holds addresses in the final binary. While those may be carried around as"
                        " constants indeed, they cannot 'really' be evaluated at this point, and thus cannot be cast to compint");
                }
            } else {
                // CHECK: CLEANUP ?
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CANNOT_USE_COMPINT_TYPE_AT_RUNTIME,
                    "Although flaggued as 'known' in a certain context, this expression is not flaggued as a full constant, and is thus akin"
                    " to a runtime value. This may be the cases for expressions carrying addresses to proc-local runtime values (allocated"
                    " on stack => at an unknown position at compile-time), or expressions having lost their 'constant' status during a cast,"
                    " when they were carrying addresses in the final binary, which would have required evaluation for the cast to succeed."
                    " As a runtime value, then, this expression is unknown at compile time, and thus cannot be cast to compint");
            }
        } else {
            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CANNOT_USE_COMPINT_TYPE_AT_RUNTIME,
                "This expression is a runtime value. As such, it is unknown at compile time, and thus cannot be cast to compint");
        }

    } else {

        u32 uDestFormat = get_ir_format(pResultType);
        Assert_(uDestFormat <= 0x07u);
        bool bDestSigned = is_signed(pResultType);
        Assert_(!bDestSigned || uDestFormat <= 0x05u);

        u32 uRequiredRuntimeChecks = 0u;

        if (eCastKind == ECAST_TRANSMUTE) {
            if (uDestFormat != uSrcFormat) {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "transmute casts between integrals must have same width");
            }
        } else if (eCastKind == ECAST_TRUNCATE) {
            if (uDestFormat > uSrcFormat) {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "truncate casts from integrals must be towards same or lesser width");
            }
        } else {
            EIRResult eCheckTrySolveIsBelowMin = EIRResult::EIRR_ENSURED_VALID_KNOWN;
            EIRResult eCheckTrySolveIsAboveMax = EIRResult::EIRR_ENSURED_VALID_KNOWN;
            u32 uRequiredChecks = get_required_checks_from_integral_type_conversions(pSrcIntType, pResultType);

            u32 uReqCheckBelowMin = uRequiredChecks & (CASTFLAG_CANBE_INVALID_NEGATIVE|CASTFLAG_CANBE_SMALLER_THAN_SIGNED);
            u32 uReqChecksAboveMax = uRequiredChecks & (CASTFLAG_CANBE_GREATER_THAN_UNSIGNED|CASTFLAG_CANBE_GREATER_THAN_SIGNED|CASTFLAG_CANBE_GREATER_THAN_SPECIAL);

            IRInfo infoMin = IRInfo{};
            IRInfo infoMax = IRInfo{};
            if (uReqCheckBelowMin) {
                Assert_(bSrcSigned);
                Assert_(0 == (uRequiredChecks & CASTFLAG_CANBE_INVALID_NEGATIVE) || 0 == (uRequiredChecks & CASTFLAG_CANBE_SMALLER_THAN_SIGNED));
                if (uSrcFormat <= 0x03u && (uRequiredChecks & CASTFLAG_CANBE_INVALID_NEGATIVE)) {
                    infoMin = info0WhenEmbeddedIntegral;
                } else {
                    // TODO: some mechanism reclaiming memory from known data results & IR position, to avoid littering our IR with
                    // irrelevant derefs for min and max
                    EIRResult eSolveInfoMin;
                    if (uRequiredChecks & CASTFLAG_CANBE_INVALID_NEGATIVE) {
                        eSolveInfoMin = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, uSrcFormat, 3u, 1u,
                            1u << uSrcFormat, 0u, pTCStatement, pTCContext, &infoMin);
                    } else {
                        constexpr u32 uFlagsConstPtr = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA;
                        u32 uPosDeclPtrToMin = ir_make_decl_entry(pTCContext->pRepo, 0u, uFlagsConstPtr, pResultType->uNykaOfMin,
                            0x03u, 8u, 3u);
                        IRInfo infoPtrToMin = IRInfo{ ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosDeclPtrToMin)|
                            u64(uFlagsConstPtr), pResultType->uNykaOfMin };
                        eSolveInfoMin = ir_emit_or_solve_deref(infoPtrToMin, uSrcFormat, _max(u8(3u), uSrcFormat), 1u,
                            1u << uSrcFormat, 0u, pTCStatement, pTCContext, &infoMin);
                    }
                    Assert_(eSolveInfoMin == EIRResult::EIRR_ENSURED_VALID_KNOWN);
                }
                u32 uFlagsIsBelowMin;
                MetaValueIR metaIsBelowMin;
                eCheckTrySolveIsBelowMin = ir_try_solve_ord_cmp_integral(uSrcFormat, pSrcValue->info, infoMin, 0u,
                    EIntSemantics::EINT_SEMANTIC_SIGNED, pTCContext,&uFlagsIsBelowMin, &metaIsBelowMin);
                if (eCheckTrySolveIsBelowMin < EIRResult::EIRR_FIRST_ERROR) {
                    if (eCheckTrySolveIsBelowMin == EIRR_ENSURED_VALID_KNOWN) {
                        Assert_(is_value_known_or_nyka(pSrcValue));
                        Assert_(irflag_is_known_embd(uFlagsIsBelowMin));
                        Assert_(!irflag_is_or_has_nyka(uFlagsIsBelowMin));
                        Assert_(metaIsBelowMin.knownValue.uEmbeddedValue <= 1uLL);
                        if (metaIsBelowMin.knownValue.uEmbeddedValue) {
                            return report_cast_error_from_known_integral_to_other_integral(
                                (uRequiredChecks & CASTFLAG_CANBE_INVALID_NEGATIVE) ?
                                    ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_VS_NON_SIGNED :
                                    ERangeCheckResultReason::ERCR_NEGATIVE_VALUE_TOO_LARGE_ABS,
                                pExprIfErr, pTCStatement, pTCContext);
                        } // Otherwise ensured OK ! even in the presence of a NYKA.
                    } else {
                        // unknown check result => required runtime check
                        uRequiredRuntimeChecks |= uReqCheckBelowMin;
                    }
                } else {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eCheckTrySolveIsBelowMin),
                        "tc_do_std_known_integral_cast_to_integral_on_nodevalue() : check known below min failed");
                }
            }

            if (uReqChecksAboveMax) {
                // TODO: some mechanism reclaiming memory from known data results & IR position, to avoid littering our IR with
                // irrelevant derefs for min and max

                constexpr u32 uFlagsConstPtr = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA;
                u32 uPosDeclPtrToMax = ir_make_decl_entry(pTCContext->pRepo, 0u, uFlagsConstPtr, pResultType->uNykaOfMax,
                    0x03u, 8u, 3u);
                IRInfo infoPtrToMax = IRInfo{ ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosDeclPtrToMax)|
                    u64(uFlagsConstPtr), pResultType->uNykaOfMax };
                EIRResult eSolveInfoMax = ir_emit_or_solve_deref(infoPtrToMax, uSrcFormat, _max(u8(3u), uSrcFormat), 1u,
                    1u << uSrcFormat, 0u, pTCStatement, pTCContext, &infoMax);
                Assert_(eSolveInfoMax == EIRResult::EIRR_ENSURED_VALID_KNOWN);
                u32 uFlagsIsAboveMax;
                MetaValueIR metaIsAboveMax;
                eCheckTrySolveIsAboveMax = ir_try_solve_ord_cmp_integral(uSrcFormat, infoMax, pSrcValue->info, 0u,
                    (uRequiredChecks & CASTFLAG_CANBE_GREATER_THAN_SIGNED) ? EINT_SEMANTIC_SIGNED : EINT_SEMANTIC_UNSIGNED, pTCContext,
                    &uFlagsIsAboveMax, &metaIsAboveMax);
                if (eCheckTrySolveIsAboveMax < EIRResult::EIRR_FIRST_ERROR) {
                    if (eCheckTrySolveIsAboveMax == EIRR_ENSURED_VALID_KNOWN) {
                        Assert_(is_value_known_or_nyka(pSrcValue));
                        Assert_(irflag_is_known_embd(uFlagsIsAboveMax));
                        Assert_(!irflag_is_or_has_nyka(uFlagsIsAboveMax));
                        Assert_(metaIsAboveMax.knownValue.uEmbeddedValue <= 1uLL);
                        if (metaIsAboveMax.knownValue.uEmbeddedValue) {
                            return report_cast_error_from_known_integral_to_other_integral(
                                (uRequiredChecks & CASTFLAG_CANBE_GREATER_THAN_SPECIAL) ?
                                    ERangeCheckResultReason::ERCR_POSITIVE_VALUE_TOO_LARGE_VS_SPECIAL :
                                    ERangeCheckResultReason::ERCR_POSITIVE_VALUE_TOO_LARGE, // TODO: a way to detect '1b too large vs signed' again ?
                                pExprIfErr, pTCStatement, pTCContext);
                        } // Otherwise ensured OK ! even in the presence of a NYKA.
                    } else {
                        // unknown check result => required runtime check
                        uRequiredRuntimeChecks |= uReqChecksAboveMax;
                    }
                } else {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eCheckTrySolveIsAboveMax),
                        "tc_do_std_known_integral_cast_to_integral_on_nodevalue() : check known above max failed");
                }

                Assert_(eExpectation != EExpectedExpr::EXPECT_CONSTANT || is_value_tc_const(pSrcValue));
                if (uRequiredRuntimeChecks) {
                    if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                        Assert_(is_value_tc_const(pSrcValue));
                        Assert_(is_value_nyka_or_has_nyka(pSrcValue));
                        return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
                            "Cannot perform that cast as a 'constant' to the target type, as expected. Although this value is"
                            " indeed 'const', it contains addresses in the final binary. These addresses can be carried around"
                            " unmodified, but are not yet resolved to a definitive value, and their presence prevents knowledge about"
                            " the validity of that particular cast");
                    }

                    if (eCastKind == ECastKind::ECAST_IMPLICIT) {
                        if (!is_value_tc_const(pSrcValue)) {
                            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CAST_REQUIRES_EXPLICIT,
                                "Cannot implicitely cast between integral formats, unless the value is const and known to fit,"
                                " or unless all possible values from source type at runtime are ensured to fit. Forcing that conversion requires"
                                " an explicit cast in case you expect the numerical value preserved in the destination format, even if you carefully"
                                " guarded that expression behind your own validity checks or proofs (as, in fact, you should)."
                                " Note that reinterpret/truncate-style casts may also be used for converting between integral formats while"
                                " preserving a value known within bounds of the destination, but those may cause nasty silent-errors in case the"
                                " original value didn't, in fact, fit into the destination format as you expected.");
                        } else {
                            Assert_(is_value_nyka_or_has_nyka(pSrcValue));
                            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CAST_REQUIRES_EXPLICIT,
                                "Cannot implicitely cast between integral formats, unless the value is const and known to fit,"
                                " or unless all possible values from source type at runtime are ensured to fit. Although this value is"
                                " indeed 'const', it contains addresses in the final binary. These addresses can be carried around"
                                " unmodified, but are not yet resolved to a definitive value, and their presence prevents knowledge about"
                                " the validity of that particular cast (signed value vs. signed type min or 0)");
                        }
                    }

                    Assert_(pTCContext->pProcResult);

                    if (uRequiredRuntimeChecks & (CASTFLAG_CANBE_INVALID_NEGATIVE|CASTFLAG_CANBE_SMALLER_THAN_SIGNED)) {
                        Assert_(ir_is_valid_param_(infoMin.uIRandMetaFlags));
                        Assert_(eCheckTrySolveIsBelowMin > EIRResult::EIRR_ENSURED_VALID_KNOWN);
                        Assert_(bSrcSigned);
                        IRInfo infoIsBelowMin;
                        EIRResult eCheckEmitIsBelowMin = ir_emit_or_solve_ord_cmp_integral(uSrcFormat, pSrcValue->info, infoMin, 0u,
                            IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoIsBelowMin);
                        Assert_(eCheckEmitIsBelowMin == eCheckTrySolveIsBelowMin);
                        Assert_(!irflag_is_known_or_nyka(infoIsBelowMin.uIRandMetaFlags));
                        do_runtime_err_check(infoIsBelowMin.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, 0u,
                            (bDestSigned ? ERR_CHECK_CAST_OUT_OF_BOUNDS : ERR_CHECK_CAST_FORBIDS_NEGATIVE), pExprIfErr, pTCStatement, pTCContext);
                    }

                    if (uRequiredRuntimeChecks & (CASTFLAG_CANBE_GREATER_THAN_UNSIGNED|CASTFLAG_CANBE_GREATER_THAN_SIGNED|CASTFLAG_CANBE_GREATER_THAN_SPECIAL)) {
                        Assert_(ir_is_valid_param_(infoMax.uIRandMetaFlags));
                        Assert_(eCheckTrySolveIsAboveMax > EIRResult::EIRR_ENSURED_VALID_KNOWN);
                        IRInfo infoIsAboveMax;
                        EIRResult eCheckEmitIsAboveMax = ir_emit_or_solve_ord_cmp_integral(uSrcFormat, infoMax, pSrcValue->info, 0u,
                            IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, (uRequiredChecks & CASTFLAG_CANBE_GREATER_THAN_SIGNED) ?
                                EINT_SEMANTIC_SIGNED : EINT_SEMANTIC_UNSIGNED, pTCStatement, pTCContext, &infoIsAboveMax);
                        Assert_(eCheckEmitIsAboveMax == eCheckTrySolveIsAboveMax);
                        Assert_(!irflag_is_known_or_nyka(infoIsAboveMax.uIRandMetaFlags));
                        do_runtime_err_check(infoIsAboveMax.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, 0u,
                            ERR_CHECK_CAST_OUT_OF_BOUNDS, pExprIfErr, pTCStatement, pTCContext);
                    }
                }
            }
        }

        EIntSemantics eSemantics = EIntSemantics::EINT_SEMANTIC_MODULO_ARITH;
        if (uDestFormat > uSrcFormat) {
            eSemantics = bSrcSigned ? EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED;
        }
        EIRResult eEmitResult = ir_emit_or_solve_truncate_or_extend_integral_to_integral(uSrcFormat, pSrcValue->info,
            eSemantics, pResultType, pTCStatement, pTCContext, outInfo);
        Assert_(eEmitResult < EIRResult::EIRR_FIRST_ERROR);
        Assert_(0 == (outInfo->uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST) || !is_value_nyka_or_has_nyka(pSrcValue));
        if (is_value_tc_const(pSrcValue) && (outInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN) &&
            0 == (outInfo->uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA) && 0 == uRequiredRuntimeChecks) {
            // we add the 'TC-const' flag to the result if source had it, and result is fully known (may still include valid cases of known nykas).
            outInfo->uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
        }

        if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
            Assert_(is_value_tc_const(pSrcValue));
            if (eEmitResult > EIRResult::EIRR_ENSURED_VALID_KNOWN) {
                Assert_(is_value_nyka_or_has_nyka(pSrcValue));
                if (uDestFormat < uSrcFormat) {
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
                        "Cannot perform that cast as a 'constant' to the target type, as expected. Although this value is"
                        " indeed 'const', it contains addresses in the final binary. These addresses can be carried around"
                        " unmodified, but are not yet resolved to a definitive value, and this cast would result in the partial"
                        " truncation of one of these addresses");
                } else { Assert_(uDestFormat > uSrcFormat && eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED);
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
                        "Cannot perform that cast as a 'constant' to the target type, as expected. Although this value is"
                        " indeed 'const', it contains addresses in the final binary. These addresses can be carried around"
                        " unmodified, but are not yet resolved to a definitive value, and this cast would require the knowledge"
                        " about the sign bit within one of those.");
                }
            } else {
                Assert_(irflag_is_known_or_nyka(outInfo->uIRandMetaFlags));
                Assert_(irflag_is_tc_const(outInfo->uIRandMetaFlags));
            }
        }

        return ETCResult::ETCR_SUCCESS;
    }
}


local_func ETCResult do_implicit_cast_compint_to_fp(TmpTCNode* pExpr, const TypeInfo_FloatingPoint* pResultType,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "do_implicit_cast_compint_to_fp() : not yet implemented");
}

local_func ETCResult do_implicit_cast_non_compint_integral_to_fp(TmpTCNode* pExpr, const TypeInfo_FloatingPoint* pResultType,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "do_implicit_cast_non_compint_integral_to_fp() : not yet implemented");
}

local_func ETCResult do_implicit_cast_fp_to_integral(TmpTCNode* pExpr, const TypeInfo_Integral* pResultType,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "do_implicit_cast_fp_to_integral() : not yet implemented");
}

local_func ETCResult do_implicit_cast_fp_to_other_fp(TmpTCNode* pExpr, const TypeInfo_FloatingPoint* pResultType,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "do_implicit_cast_fp_to_other_fp() : not yet implemented");
}

/*
local_func bool would_cast_affect_nyka(NodeValue* pSrcValue, ECastKind eCastKind, const TypeInfo* pResultType, TCContext* pTCContext)
{
    Assert_(is_value_nyka_or_has_nyka(pSrcValue));
    Assert_(pSrcValue->info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    if (is_value_known_embd(pSrcValue)) {
        if (get_runtime_unaligned_size(pResultType) < 8u) // would necessarily truncate if below 64b
            return true;
        // otherwise, transmute or truncate of at least 64b would keep nyka okay
        if (eCastKind != ECastKind::ECAST_TRANSMUTE && eCastKind != ECastKind::ECAST_TRUNCATE) {
            // ...but if the cast is otherwise, then it seems like a value-preserving-cast => intended to change the nature of the representation...
            if (get_type_kind(pSrcValue->pType) != get_type_kind(pResultType)) // so, if the representation have distinct kind (eg int to float), this is a no go
                return true; // then it would necessarily require evaluation or reinterpretation of the nyka bits...
            if (get_type_kind(pResultType) != ETypeKind::ETYPEKIND_INTEGRAL)   // and in fact, other than between integrals is also a no go.
                return true;
            // Note as why the conditions above are this way: it may be the case that we reinterpret-cast to anything >= 64b,
            //   then reinterpret cast to integral >= 64b, then cast to larger integral, or same width and different signedness...
            //   those operations could very well keep the nyka intact, and we'll support them as such.
        }
    } else {
        // TODO
        Assert(false, "would_cast_affect_nyka() : non-embedded nyka not yet implemented");
    }

    return false;
}
*/

local_func ETCResult tc_do_string_cast_to_other_string_on_nodevalue(TmpTCNode* pExprIfErr, NodeValue* pSrcValue, ECastKind eCastKind,
    const TypeInfo_OtherCore* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(get_type_kind(pSrcValue->pType) == ETypeKind::ETYPEKIND_OTHERCORE);
    const TypeInfo_OtherCore* pSrcType = (const TypeInfo_OtherCore*)pSrcValue->pType;
    Assert_(pSrcType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    Assert_(pSrcType != pResultType);

    if (pResultType->_coreFlags & STRINGFLAG_IS_COMPACT) {

        if (pResultType->_coreFlags & STRINGFLAG_HAS_ALLOC) { // to compact with alloc : probably not okay

            Assert_(0 == (pSrcType->_coreFlags & STRINGFLAG_IS_COMPACT) || 0 == (pSrcType->_coreFlags & STRINGFLAG_HAS_ALLOC)); // otherwise would be same
            // given that source is not compact and/or has no alloc, we cannot cast to a compact with alloc from that.
            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                "tc_do_string_cast_to_other_string_on_nodevalue() : cast to compact string with alloc from another string-related type is not possible."
                " Make a copy instead, using available functions.");

        } else {                                              // to compact without alloc : only okay if src was compact with alloc

            if (pSrcType->_coreFlags & STRINGFLAG_IS_COMPACT) {
                Assert_(pSrcType->_coreFlags & STRINGFLAG_HAS_ALLOC); // otherwise would be same
                // Casting from compact with alloc to compact without is like a NO-OP :
                //   same base pointer would be used, and semantic constness would be preserved.
                *outInfo = pSrcValue->info;
                outInfo->uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a *binding*, we're not instanciating the cast as such here => simple NodeValue.
                return ETCResult::ETCR_SUCCESS;
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_string_cast_to_other_string_on_nodevalue() : cast to compact string from a non-compact string is not possible."
                    " Make a copy instead, using available functions.");
            }
        }

    } else {

        if (pResultType->_coreFlags & STRINGFLAG_HAS_ALLOC) {
            
            // to owned stringview : only okay if src is owned too...
            //   note that there may exist functions able to return an 'owned' stringview with a mock of a (de)allocator doing noop, when
            //   making that view from an FF... but that decision should be explicit, with a string-related function.
            //
            // TODO
            return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_do_string_cast_to_other_string_on_nodevalue() : cast string-related to owned stringview is not yet implemented...");

        } else {                                              // to basic stringview : always okay

            if (0 == (pSrcType->_coreFlags & STRINGFLAG_IS_COMPACT)) { // from non-compact : only requires deref...

                Assert_(pSrcType->_coreFlags & STRINGFLAG_HAS_ALLOC);  // otherwise would be same
                IRInfo infoStructAddress;
                EIRResult eGatherStructAddress = ir_emit_or_solve_address_of(pSrcValue->info, pTCStatement, pTCContext, &infoStructAddress);
                Assert_(eGatherStructAddress < EIRResult::EIRR_FIRST_ERROR);
                EIRResult eDerefAsCast = ir_emit_or_solve_deref(infoStructAddress, 0x03u, 3u, 2u, 16u,
                    0u, pTCStatement, pTCContext, outInfo);
                outInfo->uIRandMetaFlags |= pSrcValue->info.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST; // keeps constness of source
                return ETCResult::ETCR_SUCCESS;

            } else {                                                   // from compact : gather parts of source info, then recompose struct result.

                IRInfo infoSrcAddressOfByteData;
                IRInfo infoSrcByteLength;
                IRInfo infoSrcFlags;
                ETCResult eGatherSrcInfo = tc_gather_string_info(pExprIfErr, pSrcValue->info, pSrcType, pTCStatement, pTCContext,
                    0u, &infoSrcAddressOfByteData, &infoSrcByteLength, &infoSrcFlags, 0, 0);
                success_or_return(eGatherSrcInfo);

                if (is_value_tc_const(pSrcValue)) {
                    Assert_(irflag_is_known_or_nyka(infoSrcAddressOfByteData.uIRandMetaFlags));
                    Assert_(irflag_is_or_has_nyka(infoSrcAddressOfByteData.uIRandMetaFlags)); // TODO: allow nullptr as const ?
                    Assert_(irflag_is_known_non_nyka(infoSrcByteLength.uIRandMetaFlags));
                    Assert_(irflag_is_known_non_nyka(infoSrcFlags.uIRandMetaFlags));
                    u8* pAllocTableAndData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, 32u, 8u);
                    u32* pNykaTable = reinterpret_cast<u32*>(pAllocTableAndData);
                    pNykaTable[0] = 1u; // Nyka count
                    pNykaTable[1] = 0u; // Offset of single nyka
                    u8** pPtrToData = reinterpret_cast<u8**>(pAllocTableAndData + 8u);
                    u8* pRawDataStart = pAllocTableAndData + 16u;
                    *pPtrToData = pRawDataStart;
                    Assert_(irflag_is_known_embd(infoSrcAddressOfByteData.uIRandMetaFlags)); // TODO: allow nullptr as const ?
                    *(reinterpret_cast<u64*>(pRawDataStart)) = infoSrcAddressOfByteData.metaValue.knownValue.uEmbeddedValue;
                    Assert_(irflag_is_known_embd(infoSrcByteLength.uIRandMetaFlags));
                    *(reinterpret_cast<u32*>(pRawDataStart + 8u)) = u32(infoSrcByteLength.metaValue.knownValue.uEmbeddedValue);
                    Assert_(irflag_is_known_embd(infoSrcFlags.uIRandMetaFlags));
                    *(reinterpret_cast<u32*>(pRawDataStart + 12u)) = u32(infoSrcFlags.metaValue.knownValue.uEmbeddedValue);
                    constexpr u32 uConstFFStringFlags = IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_TC_SEMANTIC_CONST;
                    u32 uPos = ir_make_decl_entry(pTCContext->pRepo, 0u, uConstFFStringFlags,
                        reinterpret_cast<u64>(pAllocTableAndData), 0x03u, 16u, 3u);
                    if (pTCContext->pProcResult)
                        pTCStatement->uLastIRorGlobalTCResult = uPos;

                    outInfo->uIRandMetaFlags = u64(uConstFFStringFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
                    outInfo->metaValue.knownValue.pPtrToRawData = pAllocTableAndData;
                    return ETCResult::ETCR_SUCCESS;

                } else {
                    Assert_(pTCContext->pProcResult);
                    u32 uPos = ir_emit_local_variable_decl(0x03u, 3u, 2u, IR_INSTRFLAG_IS_ASSIGNABLE, pTCContext->pRepo, pTCContext);
                    u64 uIRofDecl = ir_make_std_code_in_cur_proc(uPos);
                    IRInfo infoDecl { uIRofDecl, meta_from_payload(0uLL) };

                    IRInfo infoDestAddressOfByteData;
                    IRInfo infoDestByteLength;
                    IRInfo infoDestFlags;
                    ETCResult eGatherDestInfo = tc_gather_string_info(pExprIfErr, infoDecl, pResultType, pTCStatement, pTCContext,
                        IR_INSTRFLAG_IS_ASSIGNABLE, &infoDestAddressOfByteData, &infoDestByteLength, &infoDestFlags, 0, 0);
                    success_or_return(eGatherDestInfo);

                    do_store_value_to(infoDestAddressOfByteData.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoSrcAddressOfByteData.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x03u, 1u, pTCStatement, pTCContext);
                    do_store_value_to(infoDestByteLength.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoSrcByteLength.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x02u, 1u, pTCStatement, pTCContext);
                    do_store_value_to(infoDestFlags.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoSrcFlags.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x02u, 1u, pTCStatement, pTCContext);
                                
                    outInfo->uIRandMetaFlags = uIRofDecl;
                    outInfo->metaValue._payload = 0uLL;
                    return ETCResult::ETCR_SUCCESS;
                }
            }
        }
    }
}

local_func ETCResult tc_do_array_cast_to_other_array_on_nodevalue(TmpTCNode* pExprIfErr, NodeValue* pSrcValue, ECastKind eCastKind,
    const TypeInfo_Array* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(get_type_kind(pSrcValue->pType) == ETypeKind::ETYPEKIND_ARRAY);
    const TypeInfo_Array* pSrcType = (const TypeInfo_Array*)pSrcValue->pType;
    Assert_(pSrcType != pResultType);

    if (get_array_category_type(pResultType) == ARRAY_TYPE_KIND_SLICE) {
        if (are_types_same(pResultType->pElementType, pSrcType->pElementType, pTCContext)) {
            Assert_(get_array_category_type(pSrcType) != ARRAY_TYPE_KIND_SLICE); // Otherwise types would be same
            IRInfo infoPtrToData;
            IRInfo infoCount;
            u8 uCountFormat;
            ETCResult eGatherSrcInfo = tc_gather_array_info(pExprIfErr, pSrcValue->info, pSrcType, pTCStatement, pTCContext, 0u,
                &infoPtrToData, &infoCount, &uCountFormat, 0, 0);
            success_or_return(eGatherSrcInfo);
            Assert_(uCountFormat == 0x02u); // CHECK: atm, only slices are 64b-counted
            
            if (is_value_tc_const(pSrcValue)) {

                Assert_(irflag_is_known_or_nyka(infoPtrToData.uIRandMetaFlags));
                Assert_(irflag_is_or_has_nyka(infoPtrToData.uIRandMetaFlags)); // TODO: allow nullptr as const ?
                Assert_(irflag_is_known_non_nyka(infoCount.uIRandMetaFlags));
                u8* pAllocTableAndData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, 32u, 8u);
                u32* pNykaTable = reinterpret_cast<u32*>(pAllocTableAndData);
                pNykaTable[0] = 1u; // Nyka count
                pNykaTable[1] = 0u; // Offset of single nyka
                u8** pPtrToData = reinterpret_cast<u8**>(pAllocTableAndData + 8u);
                u8* pRawDataStart = pAllocTableAndData + 16u;
                *pPtrToData = pRawDataStart;
                Assert_(irflag_is_known_embd(infoPtrToData.uIRandMetaFlags)); // TODO: allow nullptr as const ?
                *(reinterpret_cast<u64*>(pRawDataStart)) = infoPtrToData.metaValue.knownValue.uEmbeddedValue;
                Assert_(irflag_is_known_embd(infoCount.uIRandMetaFlags));
                *(reinterpret_cast<u64*>(pRawDataStart + 8u)) = infoCount.metaValue.knownValue.uEmbeddedValue; // Note: u32 to u64 here
                constexpr u32 uConstSliceFlags = IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_TC_SEMANTIC_CONST;
                u32 uPos = ir_make_decl_entry(pTCContext->pRepo, 0u, uConstSliceFlags,
                    reinterpret_cast<u64>(pAllocTableAndData), 0x03u, 16u, 3u);
                if (pTCContext->pProcResult)
                    pTCStatement->uLastIRorGlobalTCResult = uPos;
                outInfo->uIRandMetaFlags = u64(uConstSliceFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
                outInfo->metaValue.knownValue.pPtrToRawData = pAllocTableAndData;

            } else {

                Assert_(pTCContext->pProcResult);
                u32 uPos = ir_emit_local_variable_decl(0x03u, 3u, 2u, IR_INSTRFLAG_IS_ASSIGNABLE, pTCContext->pRepo, pTCContext);
                u64 uIRofDecl = ir_make_std_code_in_cur_proc(uPos);
                IRInfo infoDecl { uIRofDecl, meta_from_payload(0uLL) };

                IRInfo infoDestAddressOfPtrToData;
                IRInfo infoDestCount;
                ETCResult eGatherDestInfo = tc_gather_array_info(pExprIfErr, infoDecl, pResultType,
                    pTCStatement, pTCContext, IR_INSTRFLAG_IS_ASSIGNABLE, &infoDestAddressOfPtrToData, &infoDestCount, 0, 0, 0);
                success_or_return(eGatherDestInfo);
                IRInfo infoCount64;
                EIRResult eWidenCount32AsSliceCount64 = ir_emit_or_solve_truncate_or_extend_integral_to_integral(0x02u, 
                    infoCount, EIntSemantics::EINT_SEMANTIC_UNSIGNED, (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_U64],
                    pTCStatement, pTCContext, &infoCount64);
                if (eWidenCount32AsSliceCount64 < EIRResult::EIRR_FIRST_ERROR) {
                    do_store_value_to(infoDestAddressOfPtrToData.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoPtrToData.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x03u, 1u, pTCStatement, pTCContext);
                    do_store_value_to(infoDestCount.uIRandMetaFlags & IR_STD_PARAM_MASK, 
                                      infoCount64.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x03u, 1u, pTCStatement, pTCContext);
                } else {
                    return_error(pExprIfErr, pTCStatement, pTCContext, u16(eWidenCount32AsSliceCount64),
                        "tc_do_array_cast_to_other_array_on_nodevalue() : widen count 32 to slice count 64 failed");
                }
                outInfo->uIRandMetaFlags = uIRofDecl;
                outInfo->metaValue._payload = 0uLL;
            }

            // skipping consolidation:
            return ETCResult::ETCR_SUCCESS;

        } else {
            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                "tc_do_array_cast_to_other_array_on_nodevalue() : cast to slice from array-related type must be to a slice with same *element* type...");
        }
    } else {
        // TODO
        return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
            "tc_do_array_cast_to_other_array_on_nodevalue() : cast to non-slice array-related type either forbidden or not yet implemented...");
    }
}


local_func ETCResult tc_do_cast_dispatch_on_node(TmpTCNode* pExprIfErr, NodeValue* pSrcValue, ECastKind eCastKind,
    const TypeInfo* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(pSrcValue);
    const TypeInfo* pSrcType = pSrcValue->pType;
    Assert_(pSrcType && pResultType);
    Assert_(!are_types_same(pSrcType, pResultType, pTCContext));

    if (pResultType == g_pCoreTypesInfo[ECORETYPE_ANY]) {
        // TODO
        return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "tc_do_cast_dispatch_on_node() : not yet implemented to ANY");
    }

    ETypeKind eTypeKind = get_type_kind(pResultType);
    ETypeKind eSrcTypeKind = get_type_kind(pSrcType);

    if (eTypeKind == ETypeKind::ETYPEKIND_DISTINCTALIAS || eSrcTypeKind == ETypeKind::ETYPEKIND_DISTINCTALIAS) {
        if (eCastKind == ECastKind::ECAST_IMPLICIT) {

            // TODO: could there be custom conversion rules for aliases, working with implicit ? eg. uom-decorated values ?

            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CAST_REQUIRES_EXPLICIT,
                "tc_do_cast_dispatch_on_node() : cast to or from aliased types must be explicit");

        } else {

            // TODO: could there be custom conversion rules for aliases ???

            if (eTypeKind == ETypeKind::ETYPEKIND_DISTINCTALIAS) {
                pResultType = unalias_ext(pResultType, &eTypeKind);
                Assert_(eTypeKind != ETypeKind::ETYPEKIND_DISTINCTALIAS);
            }
            if (eSrcTypeKind == ETypeKind::ETYPEKIND_DISTINCTALIAS) {
                pSrcType = unalias_ext(pSrcType, &eSrcTypeKind);
                Assert_(eSrcTypeKind != ETypeKind::ETYPEKIND_DISTINCTALIAS);
            }
            if (are_types_same(pSrcType, pResultType, pTCContext)) {
                *outInfo = pSrcValue->info;
                outInfo->uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a *binding*, we're not instanciating the cast as such here => simple NodeValue.
                return ETCResult::ETCR_SUCCESS;
            }
        }
    }

    if (eCastKind == ECastKind::ECAST_TRANSMUTE || eCastKind == ECastKind::ECAST_TRUNCATE) {
        u16 uErr = 0u;
        if (!is_allowed_as_runtime_type(pResultType, pTCContext, &uErr)) {
            return_error(pExprIfErr, pTCStatement, pTCContext, uErr,
                "tc_do_cast_dispatch_on_node() : destination of a transmute or truncating cast must be a runtime type");
        }
        if (pSrcType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
            // TODO: allow cast to anything, fom compint ??
            if (get_type_kind(pResultType) == ETYPEKIND_INTEGRAL) {
                return tc_do_compint_cast_to_integral_on_nodevalue(pExprIfErr, pSrcValue,
                        eCastKind, (const TypeInfo_Integral*)pResultType, pTCStatement, pTCContext, outInfo);
            } else {
                // TODO ?
                return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_do_cast_dispatch_on_node() : compint transmute or truncate to non-integral not yet implemented");
            }
        } else if (!is_allowed_as_runtime_type(pSrcType, pTCContext, &uErr)) {
            return_error(pExprIfErr, pTCStatement, pTCContext, uErr,
                "tc_do_cast_dispatch_on_node() : source of a transmute or truncating cast must be a runtime type (or compint)");
        }

        // TODO: CLEANUP: what about unaligned size ?
        u32 uBytesSource = get_runtime_sizeof(pSrcType);
        u32 uBytesDest = get_runtime_sizeof(pResultType);
        if (eCastKind == ECastKind::ECAST_TRANSMUTE) {
            if (uBytesDest != uBytesSource) {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_cast_dispatch_on_node() : cannot transmute to a type with distinct byte size");
            }
        } else { Assert_(eCastKind == ECastKind::ECAST_TRUNCATE);
            if (uBytesDest > uBytesSource) {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_cast_dispatch_on_node() : cannot truncate to a type with higher byte size");
            }
        }

        u8 uFormatSrc = get_ir_format(pSrcType);
        u32 uAlignSrcLog2 = get_log2_of_align_bytes(pSrcType);
        u32 uSrcSlotsCount = get_slots_count(pSrcType);

        u8 uFormatDest = get_ir_format(pResultType);
        u32 uAlignDestLog2 = get_log2_of_align_bytes(pResultType);
        u32 uDestSlotsCount = get_slots_count(pResultType);
 
        if (uAlignDestLog2 > uAlignSrcLog2) {
            // TODO: Possibly allow higher align dest ? maybe with a special implementation distinct from deref ?
            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                "tc_do_cast_dispatch_on_node() : truncate or transmute to a type with higher align requirement not yet implemented");
        }

        if (uFormatSrc == uFormatDest && uSrcSlotsCount == uDestSlotsCount) {
            *outInfo = pSrcValue->info;
            outInfo->uIRandMetaFlags &= ~u64(IRFLAGS_TC_SPECIFIC_MASK);
        } else {
            IRInfo infoAddress;
            EIRResult eEmitAddressOfResult = ir_emit_or_solve_address_of(pSrcValue->info, pTCStatement, pTCContext, &infoAddress);
            Assert_(eEmitAddressOfResult < EIRResult::EIRR_FIRST_ERROR);
            Assert_(irflag_is_known_or_nyka(infoAddress.uIRandMetaFlags));
            EIRResult eEmitDerefResult = ir_emit_or_solve_deref(infoAddress, uFormatDest, uAlignDestLog2, uDestSlotsCount, uBytesDest, 0u,
                pTCStatement, pTCContext, outInfo);
            if (eEmitDerefResult >= EIRResult::EIRR_FIRST_ERROR) {
                return_error(pExprIfErr, pTCStatement, pTCContext, u16(eEmitDerefResult),
                    "tc_do_cast_dispatch_on_node() : emitting deref failed (trying to solve for truncate or transmute)");
            }
        }
        ETCResult eEnsureConst = consolidate_tc_const_flag_on_info(outInfo, pResultType, pExprIfErr, pTCStatement, pTCContext);
        Assert_(eEnsureConst == ETCResult::ETCR_SUCCESS);
        if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
            Assert_(is_value_tc_const(pSrcValue));
            if (!irflag_is_tc_const(outInfo->uIRandMetaFlags)) {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_EXPECTED_CONSTANT,
                    "tc_do_cast_dispatch_on_node() : result from reinterp or truncate cast this value cannot be a considered as constant,"
                    " either due to the resulting type requiring more guarantees than the source type, or from requiring to truncate a nyka"
                    " present in the source instance.");
            }
        }

        return ETCResult::ETCR_SUCCESS;
    }

    /*
    NodeValue mockValueIfNykaConvertedToRuntime;
    if (is_value_nyka_or_has_nyka(pSrcValue)) {
        Assert_(is_value_known_or_nyka(pSrcValue));
        Assert_(!is_value_tc_only(pSrcValue));
        if (pResultType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
                "tc_do_cast_dispatch_on_node() : cannot convert a value known to carry an absolute address in the final binary to"
                " a comptime int. Casting it, still const, to large-enough, sized integral formats may be possible, though.");
            // TODO: also prevent casts to any other tc-only type ?
        }
        if (would_cast_affect_nyka(pSrcValue, eCastKind, pResultType, pTCContext)) {
            if (eExpectation == EExpectedExpr::EXPECT_CONSTANT) {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST_NYKA_AS_CONSTANT,
                    "tc_do_cast_dispatch_on_node() : such a cast, when requiring the resulting expression to be constant, would require"
                    " the modification, truncation or otherwise evaluation - by the typechecker - of a value being known to carry an"
                    " absolute address in the final binary. Although LOC typechecker is able to propagate such values (unmodified) as"
                    " 'constants' indeed, and to deal with them efficiently when it is time for the backend to emit the final binary, such"
                    " a truncation or evaluation at this stage is impossible. You would still be allowed to perform this cast at runtime,"
                    " provided the current semantic requirement of constness is relaxed.");
            } else {
                Assert_(pTCContext->pProcResult);
                u8 uSrcFormat = get_ir_format(pSrcType);
                u32 uSlotsCount = get_slots_count(pSrcType);
                Assert_(uSlotsCount);
                u32 uPosOfNewLocal = ir_emit_local_variable_decl(uSrcFormat, get_log2_of_align_bytes(pSrcType), uSlotsCount, 0u,
                    pTCContext->pRepo, pTCContext);
                u64 uIRofNewLocal = ir_make_std_code_in_cur_proc(uPosOfNewLocal);
                do_store_value_to(uIRofNewLocal, pSrcValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK, get_ir_format(pSrcType),
                    get_slots_count(pSrcType), pTCStatement, pTCContext);
                mockValueIfNykaConvertedToRuntime.pType = pSrcType;
                mockValueIfNykaConvertedToRuntime.info.uIRandMetaFlags = uIRofNewLocal;
                mockValueIfNykaConvertedToRuntime.info.metaValue._payload = 0uLL;
                pSrcValue = &mockValueIfNykaConvertedToRuntime;
            }
        }
    }
    */

    switch (eSrcTypeKind) {

        case ETypeKind::ETYPEKIND_INTEGRAL: {   // Integral source

            if (pSrcType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
                Assert_(is_value_tc_const(pSrcValue));
                Assert_(is_value_tc_only(pSrcValue));
                if (eTypeKind == ETypeKind::ETYPEKIND_INTEGRAL) {
                    return tc_do_compint_cast_to_integral_on_nodevalue(pExprIfErr, pSrcValue,
                        eCastKind, (const TypeInfo_Integral*)pResultType, pTCStatement, pTCContext, outInfo);
                } else if (eTypeKind == ETypeKind::ETYPEKIND_FLOATINGPOINT) {
                    // TODO
                    return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_do_cast_dispatch_on_node() : from compint to fp not yet implemented");
                } else if (eTypeKind == ETypeKind::ETYPEKIND_POINTER) {
                    if (pSrcValue->info.metaValue.knownValue.uEmbeddedValue == 0uLL || // compint zero can implicit cast to pointer
                        eCastKind != ECAST_IMPLICIT) {                                 // explicit cast to pointer... well, ok
                        return tc_do_compint_cast_to_integral_on_nodevalue(pExprIfErr, pSrcValue,
                            eCastKind, (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_RAWPTR], pTCStatement, pTCContext, outInfo);
                    } else {
                        return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CAST_REQUIRES_EXPLICIT,
                            "tc_do_cast_dispatch_on_node() : implicit cast from compint to pointer is forbidden (except if 0)");
                    }
                } else {
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                        "tc_do_cast_dispatch_on_node() : value-cast from compint to non-numeric is forbidden");
                }
            } else {
                if (eTypeKind == ETypeKind::ETYPEKIND_INTEGRAL) {
                    return tc_do_non_compint_integral_cast_to_other_integral_on_nodevalue(pExprIfErr, pSrcValue,
                            eCastKind, (const TypeInfo_Integral*)pResultType, pTCStatement, pTCContext, eExpectation, outInfo);
                } else if (eTypeKind == ETypeKind::ETYPEKIND_FLOATINGPOINT) {
                    // TODO
                    return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_do_cast_dispatch_on_node() : from non-compint to fp not yet implemented");
                } else if (eTypeKind == ETypeKind::ETYPEKIND_POINTER) {
                    if ((pSrcValue->info.uIRandMetaFlags & IRFLAG_IS_KNOWN_ZERO) ||    // full known zero can implicit cast to pointer
                        eCastKind != ECAST_IMPLICIT ||                                 // explicit cast to pointer... well, ok
                        pSrcType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]) {              // rawptr types can implicit cast to any other
                            // We do as-if result type is rawpointer to call the following:
                            return tc_do_non_compint_integral_cast_to_other_integral_on_nodevalue(pExprIfErr, pSrcValue,
                                    eCastKind, (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_RAWPTR], pTCStatement, pTCContext, eExpectation, outInfo);
                    } else {
                        return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CAST_REQUIRES_EXPLICIT,
                            "tc_do_cast_dispatch_on_node() : implicit cast from non-rawptr integral to pointer is forbidden (except if known 0)");
                    }
                } else {
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                        "tc_do_cast_dispatch_on_node() : value-cast from integral type to non-numeric is forbidden");
                }
            }

        } break;

        case ETypeKind::ETYPEKIND_FLOATINGPOINT: {   // FP source
            if (eTypeKind == ETypeKind::ETYPEKIND_INTEGRAL) {
                // TODO
                return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_do_cast_dispatch_on_node() : from fp to integral not yet implemented");
            } else if (eTypeKind == ETypeKind::ETYPEKIND_FLOATINGPOINT) {
                // TODO
                return_error(pExprIfErr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_do_cast_dispatch_on_node() : from fp to fp not yet implemented");
            } else {
                // TODO: allow reinterps and truncates
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_cast_dispatch_on_node() : value-cast from floating-point type to non-numeric is forbidden");
            }
        } break;

        case ETypeKind::ETYPEKIND_POINTER: {         // Pointer source
            if (eTypeKind == ETypeKind::ETYPEKIND_POINTER) {
                if (eCastKind != ECastKind::ECAST_IMPLICIT) { // simple copy from dest to source...
                    *outInfo = pSrcValue->info;
                    outInfo->uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a variable *binding*, we're not instanciating the cast as such here => simple NodeValue.
                    return ETCResult::ETCR_SUCCESS;
                } else {
                    const TypeInfo_Pointer* pSrcTypePtr = (const TypeInfo_Pointer*)pSrcType;
                    if (get_type_kind(pSrcTypePtr->pPointedToType) == ETypeKind::ETYPEKIND_ARRAY) { // Specially allowing cast from ptr to static array towards pointer to array elements.
                        const TypeInfo_Array* pAsPointedArraySourceType = (const TypeInfo_Array*)pSrcTypePtr->pPointedToType;
                        if (get_array_category_type(pAsPointedArraySourceType) == ARRAY_TYPE_KIND_STATIC) {
                            const TypeInfo_Pointer* pDstTypePtr = (const TypeInfo_Pointer*)pResultType;
                            if (are_types_same(pAsPointedArraySourceType->pElementType, pDstTypePtr->pPointedToType, pTCContext)) {
                                *outInfo = pSrcValue->info;
                                outInfo->uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a variable *binding*, we're not instanciating the cast as such here => simple NodeValue.
                                return ETCResult::ETCR_SUCCESS;
                            } // otherwise fallthrough...
                        } // otherwise fallthrough...
                    }
                    const TypeInfo_Pointer* pDstTypePtr = (const TypeInfo_Pointer*)pResultType;
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_CAST_REQUIRES_EXPLICIT,
                        "tc_do_cast_dispatch_on_node() : cast from pointer type to other pointer type must be explicit");
                }
            } else if (pResultType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]) { // simple copy from dest to source...
                *outInfo = pSrcValue->info;
                outInfo->uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a variable *binding*, we're not instanciating the cast as such here => simple NodeValue.
                return ETCResult::ETCR_SUCCESS;
            } else {
                // TODO ? to other integrals ?
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_cast_dispatch_on_node() : value-cast from pointer type to non-rawptr integral is forbidden");
            }
        } break;

        case ETypeKind::ETYPEKIND_OTHERCORE: {
            if (pSrcType->_coreFlags & OTHERCOREFLAG_IS_STRING) { // String source
                if (eTypeKind == ETypeKind::ETYPEKIND_OTHERCORE && (pResultType->_coreFlags & OTHERCOREFLAG_IS_STRING)) { // to string dest...
                    return tc_do_string_cast_to_other_string_on_nodevalue(pExprIfErr, pSrcValue,
                            eCastKind, (const TypeInfo_OtherCore*)pResultType, pTCStatement, pTCContext, eExpectation, outInfo);
                } else {
                    if (pSrcType->_coreFlags & STRINGFLAG_IS_COMPACT) {
                        if (pResultType == g_pCoreTypesInfo[ECORETYPE_RAWPTR]) { on_allowed_cast_compact_string_to_pointer:
                            *outInfo = pSrcValue->info;
                            outInfo->uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a variable *binding*, we're not instanciating the cast as such here => simple NodeValue.
                            return ETCResult::ETCR_SUCCESS;
                        } else if (get_type_kind(pResultType) == ETypeKind::ETYPEKIND_POINTER) {
                            const TypeInfo_Pointer* pAsPtrResult = (const TypeInfo_Pointer*)pResultType;
                            if (pAsPtrResult->pPointedToType == g_pCoreTypesInfo[ECORETYPE_R8] ||
                                pAsPtrResult->pPointedToType == g_pCoreTypesInfo[ECORETYPE_U8]) {
                                goto on_allowed_cast_compact_string_to_pointer;
                            }
                        }
                    }
                    return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                        "tc_do_cast_dispatch_on_node() : cast from string-related to non-string-related is forbidden, unless compact to rawptr or byte ptr");
                }
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_cast_dispatch_on_node() : cast to non-string-related othercore is forbidden");
            }
        } break;

        case ETypeKind::ETYPEKIND_ARRAY: {  // Array source
            if (eTypeKind == ETypeKind::ETYPEKIND_ARRAY) { // to array dest...
                return tc_do_array_cast_to_other_array_on_nodevalue(pExprIfErr, pSrcValue,
                        eCastKind, (const TypeInfo_Array*)pResultType, pTCStatement, pTCContext, eExpectation, outInfo);
            } else {
                return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                    "tc_do_cast_dispatch_on_node() : cast from array-related to non-array-related is forbidden");
            }
        } break;

        default: {
            // TODO : maps, sets, hwvectors...
            return_error(pExprIfErr, pTCStatement, pTCContext, CERR_INVALID_CAST,
                "tc_do_cast_dispatch_on_node() : cast from non-numeric, string or array types either forbidden or not yet handled");
        } break;
    }
}

local_func ETCResult do_implicit_cast(TmpTCNode* pExpr, const TypeInfo* pResultType,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(is_node_already_typechecked(pExpr->pTCNode));
    Assert_(pExpr->pIntrinsicValue);
    Assert_(pExpr->pIntrinsicValue->pType && pResultType);

    if (pExpr->pIntrinsicValue->pType == pResultType) { // pointer equality between types => can directly assign same allocated value
        return set_cast_success_with_same_value(pExpr);
    }

    if_expr_already_type_casted_phase2_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    NodeValue* pValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_FINAL, pTCStatement, pTCContext);
    pValue->pType = pResultType;

    if (are_types_same(pExpr->pIntrinsicValue->pType, pResultType, pTCContext)) {
        pValue->info = pExpr->pIntrinsicValue->info;
        pValue->info.uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a *binding*, we're not instanciating the cast as such here => simple NodeValue.
        return set_node_type_cast_expr_success(pExpr->pTCNode);
    } else {
        ETCResult eCheckCast = tc_do_cast_dispatch_on_node(pExpr, pExpr->pIntrinsicValue,
            ECastKind::ECAST_IMPLICIT, pResultType, pTCStatement, pTCContext, eExpectation, &(pValue->info));
        success_or_return(eCheckCast);
        Assert_(ir_is_valid_param_(pExpr->pFinalValue->info.uIRandMetaFlags) || is_value_tc_only(pExpr->pFinalValue));
        return set_node_type_cast_expr_success(pExpr->pTCNode);
    }
}

local_func ETCResult do_explicit_cast(TmpTCNode* pToCast, const TypeInfo* pResultType, ECastKind eCastKind, TmpTCNode* pResult, 
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    Assert_(eCastKind != ECastKind::ECAST_IMPLICIT);

    Assert_(is_node_already_typechecked(pToCast->pTCNode));
    Assert_(!is_node_already_typechecked(pResult->pTCNode));
    Assert_(pToCast->pIntrinsicValue);
    Assert_(pToCast->pIntrinsicValue->pType && pResultType);

    if (pToCast->pIntrinsicValue->pType == pResultType) { // pointer equality between types => can directly assign same allocated value
        return set_tc_success_with_same_value_as_intrinsic_of(pToCast, pResult);
    }

    NodeValue* pValue = alloc_value_for(pResult, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
    pValue->pType = pResultType;

    if (are_types_same(pToCast->pIntrinsicValue->pType, pResultType, pTCContext)) {
        pValue->info = pToCast->pIntrinsicValue->info;
        pValue->info.uIRandMetaFlags &= ~u64(IRFLAG_TC_BINDING_INSTANCE); // if the value is a *binding*, we're not instanciating the cast as such here => simple NodeValue.
        return set_node_typecheck_expr_success(pResult->pTCNode);
    } else {
        ETCResult eCheckCast = tc_do_cast_dispatch_on_node(pResult, pToCast->pIntrinsicValue,
            eCastKind, pResultType, pTCStatement, pTCContext, eExpectation, &(pValue->info));
        success_or_return(eCheckCast);
        Assert_(ir_is_valid_param_(pResult->pIntrinsicValue->info.uIRandMetaFlags) || is_value_tc_only(pResult->pIntrinsicValue));
        return set_node_typecheck_expr_success(pResult->pTCNode);
    }
}

#endif // LOCLIB_TC_CASTS_H_

