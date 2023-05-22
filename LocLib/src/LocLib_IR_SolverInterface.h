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

#ifndef LOCLIB_IR_SOLVER_INTERFACE_H_
#define LOCLIB_IR_SOLVER_INTERFACE_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/arithmetic_operations.h"
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
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_IR_Types.h"
#include "LocLib_IR_Solver.h"

/*
local_func u64 try_integral_as_immediate(u8 uFormat, u32 uMetaFlags, AKnownValue knownValue)
{
    Assert_(uFormat <= 0x07u);
    Assert_(uMetaFlags & IRFLAG_IS_KNOWN);
    if (0u == (uMetaFlags & IRFLAG_HAS_NYKA)) {
        switch(uFormat) {
            case 0x00u: { // 8b
                Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                u8 uValue = u8(knownValue.uEmbeddedValue);
                return ir_make_r32_immediate(u32(uValue));
            } break;
            case 0x01u: { // 16b
                Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                u16 uValue = u16(knownValue.uEmbeddedValue);
                return ir_make_r32_immediate(u32(uValue));
            } break;
            case 0x02u: { // 32b
                Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                u32 uValue = u32(knownValue.uEmbeddedValue);
                return ir_make_r32_immediate(uValue);
            } break;
            case 0x03u: { // 64b
                Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                u64 uValue = knownValue.uEmbeddedValue;
                u64 uHighBits = uValue & 0xFFFF'FFFF'0000'0000uLL;
                if (uHighBits == 0u) {
                    return ir_make_r32_immediate(u32(uValue));
                } else if (uHighBits == 0xFFFF'FFFF'0000'0000uLL) {
                    return ir_make_large_neg_immediate(u32(uValue));
                }
            } break;
        }
    }
    return 0uLL; // returns an invalid IR in case we did not find a match as an immediate
}
*/


#define IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult)    do { \
    Assert_(eResult < EIRResult::EIRR_FIRST_ERROR); \
    if (uMetaFlags & IRFLAG_IS_KNOWN) { /* May not be the case even if IR Result is marked 'known', since can be 'same as A' or 'same as B' */ \
        if (uFormat < 0x03u) { \
            Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD); \
            Assert_(0 == (uMetaFlags & IRFLAG_HAS_NYKA)); \
            outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | \
                ir_make_int_immediate(i32(outResultingInfo->metaValue.knownValue.uEmbeddedValue)); \
            return eResult; \
        } else if (uFormat == 0x03u) { \
            Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD); \
            if (0 == (uMetaFlags & IRFLAG_HAS_NYKA)) { \
                constexpr u64 uMaskHighSigned = 0xFFFF'FFFF'8000'0000uLL; \
                u64 uMaskedVal = outResultingInfo->metaValue.knownValue.uEmbeddedValue & uMaskHighSigned; \
                if (uMaskedVal == 0uLL || uMaskedVal == uMaskHighSigned) { \
                    outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | \
                        ir_make_int_immediate(i32(outResultingInfo->metaValue.knownValue.uEmbeddedValue)); \
                    return eResult; \
                } \
            } else { \
                i32 iOffset; u64 uBaseIR = ir_decode_nyka_value(outResultingInfo->metaValue.knownValue.uEmbeddedValue, &iOffset); \
                if (0 == iOffset) { \
                    outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_nyka_immediate(uBaseIR); \
                    return eResult; \
                } \
            } \
        } \
    } \
} while (0)

local_func EIRResult ir_set_ir_and_flags_on_enum_value_info(u8 uFormat, u32 uMetaFlags, IRInfo* ioResultingInfo, TCContext* pContext)
{
    uMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
    constexpr EIRResult eResult = EIRResult::EIRR_ENSURED_VALID_KNOWN;
    IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, ioResultingInfo, eResult);
    u32 uDeclPos = ir_make_decl_entry(pContext->pRepo, 0u, uMetaFlags, ioResultingInfo->metaValue._payload, uFormat,
        1u << get_log2_of_slot_size_from_format(uFormat), get_log2_of_natural_align_from_format(uFormat));
    ioResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pContext->pRepo->uIRRepoId, uDeclPos);
    return eResult;
}

local_func EIRResult ir_emit_or_solve_add_or_sub_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB, u64 uHighBitSetIfSub,
                                                          EIntSemantics eSemantics, TmpTCNode* pNode, TCStatement* pTCStatement,
                                                          TCContext* pTCContext, IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_add_or_sub_integral(uFormat, infoA, infoB, uHighBitSetIfSub, eSemantics,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u8 uOpEncoding = uHighBitSetIfSub ? IRIT_SUB : IRIT_ADD;
        u32 uInstrFlags = 0;
        if (eSemantics == EINT_SEMANTIC_MODULO_ARITH || eResult <= EIRResult::EIRR_ENSURED_VALID_UNKNOWN) {
            // NOOP for checks...
        } else if (eSemantics == EINT_SEMANTIC_SIGNED) {
            uInstrFlags |= IR_INSTRFLAG_EMBD_CHECK|IR_INSTRFLAG_POSTOP_CHKSIGNED;
        } else if (eSemantics == EINT_SEMANTIC_UNSIGNED) {
            uInstrFlags |= IR_INSTRFLAG_EMBD_CHECK|IR_INSTRFLAG_POSTOP_CHKUNSIGNED;
        } else {
            platform_log_error("ir_emit_or_solve_add_or_sub_integral() : mixed semantics not yet implemented");
            return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
        }

        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(uOpEncoding) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);

        if (pTCContext->pProcResult) {
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        }

        if (eSemantics != EINT_SEMANTIC_MODULO_ARITH && eResult > EIRResult::EIRR_ENSURED_VALID_UNKNOWN) {
            Assert_(uInstrFlags);
            u32 uPosOfOvfParam = uPos + 1u;
            Assert_(uPosOfOvfParam == pTCContext->pRepo->uSize);
            u8 uErrCheckKind = ERR_CHECK_SIGNED_RESULT_OUT_OF_BOUNDS;
            if (eSemantics != EINT_SEMANTIC_SIGNED)
                uErrCheckKind = ERR_CHECK_UNSIGNED_RESULT_OUT_OF_BOUNDS;
            if (uInstrFlags & IR_INSTRFLAG_EMBD_CHECK) {
                do_runtime_err_check(uIRofResult, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO|IR_INSTRFLAG_BRANCH_REPLACES_PARAM, uPos, uErrCheckKind, pNode, pTCStatement, pTCContext);
                if (pTCContext->pProcResult) {
                    pTCStatement->uLastIRorGlobalTCResult = uPos + 1u;
                }
            } else {
                IREntry* ovfParamEntry = ir_append_new_entry(pTCContext->pRepo);
                ovfParamEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CALLER_RET_PARAM) | (u64(uPos) << IR_STD_PARAM_SHIFT);
                u64 uSlotsCountAndAlign = u64(1u) | (u64(uFormat) << 32);
                ovfParamEntry->uInstrMetaFlagsAndSecondParam = (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT);
                ovfParamEntry->metaValue._payload = 0uLL;
                u64 uIRofOvfParam = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosOfOvfParam);
                do_runtime_err_check(uIRofOvfParam, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, uPos, uErrCheckKind, pNode, pTCStatement, pTCContext);
                if (pTCContext->pProcResult) {
                    pTCStatement->uLastIRorGlobalTCResult = uPos + 2u;
                }
            }
        }
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_mul_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                EIntSemantics eSemantics, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_mul_integral(uFormat, infoA, infoB, eSemantics, pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u8 uOpEncoding = (eSemantics == EINT_SEMANTIC_SIGNED || eSemantics == EINT_SEMANTIC_MODULO_SIGNED) ? IRIT_MUL : IRIT_MUL_U;
        u32 uInstrFlags = 0;
        if (eSemantics == EINT_SEMANTIC_MODULO_ARITH || eSemantics == EINT_SEMANTIC_MODULO_SIGNED || eResult <= EIRResult::EIRR_ENSURED_VALID_UNKNOWN) {
            // NOOP for checks...
        } else if (eSemantics == EINT_SEMANTIC_SIGNED) {
            uInstrFlags |= IR_INSTRFLAG_EMBD_CHECK|IR_INSTRFLAG_POSTOP_CHKSIGNED;
        } else if (eSemantics == EINT_SEMANTIC_UNSIGNED) {
            uInstrFlags |= IR_INSTRFLAG_EMBD_CHECK|IR_INSTRFLAG_POSTOP_CHKUNSIGNED;
        } else {
            platform_log_error("ir_emit_or_solve_mul_integral() : mixed semantics not yet implemented");
            return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
        }

        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(uOpEncoding) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);

        if (pTCContext->pProcResult) {
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        }

        if (eSemantics != EINT_SEMANTIC_MODULO_ARITH && eSemantics != EINT_SEMANTIC_MODULO_SIGNED && eResult > EIRResult::EIRR_ENSURED_VALID_UNKNOWN) {
            Assert_(uInstrFlags);
            u32 uPosOfOvfParam = uPos + 1u;
            Assert_(uPosOfOvfParam == pTCContext->pRepo->uSize);
            u8 uErrCheckKind = ERR_CHECK_SIGNED_RESULT_OUT_OF_BOUNDS;
            if (eSemantics != EINT_SEMANTIC_SIGNED)
                uErrCheckKind = ERR_CHECK_UNSIGNED_RESULT_OUT_OF_BOUNDS;
            if (uInstrFlags & IR_INSTRFLAG_EMBD_CHECK) {
                do_runtime_err_check(uIRofResult, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO|IR_INSTRFLAG_BRANCH_REPLACES_PARAM, uPos, uErrCheckKind, pNode, pTCStatement, pTCContext);
                if (pTCContext->pProcResult) {
                    pTCStatement->uLastIRorGlobalTCResult = uPos + 1u;
                }
            } else {
                IREntry* ovfParamEntry = ir_append_new_entry(pTCContext->pRepo);
                ovfParamEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CALLER_RET_PARAM) | (u64(uPos) << IR_STD_PARAM_SHIFT);
                u64 uSlotsCountAndAlign = u64(1u) | (u64(uFormat) << 32);
                ovfParamEntry->uInstrMetaFlagsAndSecondParam = (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT);
                ovfParamEntry->metaValue._payload = 0uLL;
                u64 uIRofOvfParam = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosOfOvfParam);
                do_runtime_err_check(uIRofOvfParam, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, uPos, uErrCheckKind, pNode, pTCStatement, pTCContext);
                if (pTCContext->pProcResult) {
                    pTCStatement->uLastIRorGlobalTCResult = uPos + 2u;
                }
            }
        }
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_expected_exact_quotient(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                    EIntSemantics eSemantics, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                    EErrCheckKind eErrChkKind, IRInfo* outResultingInfo)
{
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_quo_or_rem_or_mod_integral(ETOK_INT_QUOTIENT, uFormat, infoA, infoB, eSemantics,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        u32 uRemFlags; MetaValueIR flagValue;
        EIRResult eResultRem = ir_try_solve_quo_or_rem_or_mod_integral(ETOK_INT_REMAINDER, uFormat, infoA, infoB, eSemantics,
            pTCContext, &uRemFlags, &flagValue);
        if (eResultRem <= EIRResult::EIRR_ENSURED_VALID_KNOWN && (uRemFlags & IRFLAG_IS_KNOWN) && 0 == (uRemFlags & IRFLAG_HAS_NYKA)) {
            if (uRemFlags & IRFLAG_IS_KNOWN_ZERO) {
                IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
            } else {
                platform_log_error("ir_emit_or_solve_expected_exact_quotient() : inexact result found");
                return EIRR_INEXACT_QUOTIENT;
            }
        } // otherwise we couldn't guarantee a 0 remainder...
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        u32 uInstrFlags = IR_INSTRFLAG_EMBD_CHECK;
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED)
            uInstrFlags |= IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED;
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_EXACT_QUO) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);
        
        Assert_(uInstrFlags);
        u32 uPosOfNonExactParam = uPos + 1u;
        Assert_(uPosOfNonExactParam == pTCContext->pRepo->uSize);
        if (uInstrFlags & IR_INSTRFLAG_EMBD_CHECK) {
            do_runtime_err_check(uIRofResult, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO|IR_INSTRFLAG_BRANCH_REPLACES_PARAM, uPos, u8(eErrChkKind), pNode, pTCStatement, pTCContext);
            if (pTCContext->pProcResult) {
                pTCStatement->uLastIRorGlobalTCResult = uPos + 1u;
            }
        } else {
            IREntry* nonExactParamEntry = ir_append_new_entry(pTCContext->pRepo);
            nonExactParamEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CALLER_RET_PARAM) | (u64(uPos) << IR_STD_PARAM_SHIFT);
            u64 uSlotsCountAndAlign = u64(1u) | (u64(uFormat) << 32);
            nonExactParamEntry->uInstrMetaFlagsAndSecondParam = (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT);
            nonExactParamEntry->metaValue._payload = 0uLL;
            u64 uIRofNonExactParam = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosOfNonExactParam);
            do_runtime_err_check(uIRofNonExactParam, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, uPos, u8(eErrChkKind), pNode, pTCStatement, pTCContext);
            if (pTCContext->pProcResult) {
                pTCStatement->uLastIRorGlobalTCResult = uPos + 2u;
            }
        }
    }
    return eResult;
}

// Warning: shall be called AFTER ensuring of non zero operandB
local_func EIRResult ir_emit_or_solve_quotient_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                EIntSemantics eSemantics, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_quo_or_rem_or_mod_integral(ETOK_INT_QUOTIENT, uFormat, infoA, infoB, eSemantics,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        u32 uInstrFlags = eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED ? IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED : 0u;
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_QUO) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

// Warning: shall be called AFTER ensuring of non zero operandB
local_func EIRResult ir_emit_or_solve_remainder_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                EIntSemantics eSemantics, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_quo_or_rem_or_mod_integral(ETOK_INT_REMAINDER, uFormat, infoA, infoB, eSemantics,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        u32 uInstrFlags = eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED ? IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED : 0u;
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_REM) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

// Warning: shall be called AFTER ensuring of non zero operandB. Assumes signed semantics. Note: for unsigned "mod", use unsigned "rem" instead.
local_func EIRResult ir_emit_or_solve_signed_modulus_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_quo_or_rem_or_mod_integral(ETOK_MOD, uFormat, infoA, infoB, EIntSemantics::EINT_SEMANTIC_SIGNED,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_MOD) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_bitwise_and(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_bitwise_op(ETOK_BIT_AND, uFormat, infoA, infoB,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BIT_AND) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_bitwise_or(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_bitwise_op(ETOK_BIT_OR, uFormat, infoA, infoB,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BIT_OR) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_bitwise_xor(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                                TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_bitwise_op(ETOK_BIT_XOR, uFormat, infoA, infoB,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BIT_XOR) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_bitwise_not(u8 uFormat, const IRInfo& infoIn,
                                                  TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                  IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_bitwise_op(ETOK_BIT_NOT, uFormat, infoIn, IRInfo{},
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BIT_NOT) | (u64(uFormat) << 16u) | (infoIn.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

// Warning: infoPow format is always supposed 32b, and shall be called AFTER ensuring of non negative, and strict less than number of bits in format
local_func EIRResult ir_emit_or_solve_integral_pow(u8 uFormat, const IRInfo& infoA, EIntSemantics eSemantics, const IRInfo& infoPow,
    TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)
{
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);

    // TODO
    platform_log_error("ir_emit_or_solve_integral_pow() : not yet implemented");
    return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;

    /*
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_integral_pow(uFormat, infoA, eSemantics, infoPow,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }
    */

    /*
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {

        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);

        opEntry->uInstrCodeAndFormatAndFirstParam = u64(uOpEncoding) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);

        if (pTCContext->pProcResult) {
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        }

        if (eResult > EIRResult::EIRR_ENSURED_VALID_UNKNOWN) {
            Assert_(uInstrFlags);
            u32 uPosOfOvfParam = uPos + 1u;
            Assert_(uPosOfOvfParam == pTCContext->pRepo->uSize);
            u8 uErrCheckKind = ERR_CHECK_SIGNED_RESULT_OUT_OF_BOUNDS;
            if (eSemantics != EINT_SEMANTIC_SIGNED)
                uErrCheckKind = ERR_CHECK_UNSIGNED_RESULT_OUT_OF_BOUNDS;
            if (uInstrFlags & IR_INSTRFLAG_EMBD_CHECK) {
                do_runtime_err_check(uIRofResult, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO|IR_INSTRFLAG_BRANCH_REPLACES_PARAM, uPos, uErrCheckKind, pNode, pTCStatement, pTCContext);
                if (pTCContext->pProcResult) {
                    pTCStatement->uLastIRorGlobalTCResult = uPos + 1u;
                }
            } else {
                IREntry* ovfParamEntry = ir_append_new_entry(pTCContext->pRepo);
                ovfParamEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CALLER_RET_PARAM) | (u64(uPos) << IR_STD_PARAM_SHIFT);
                u64 uSlotsCountAndAlign = u64(1u) | (u64(uFormat) << 32);
                ovfParamEntry->uInstrMetaFlagsAndSecondParam = (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT);
                ovfParamEntry->metaValue._payload = 0uLL;
                u64 uIRofOvfParam = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosOfOvfParam);
                do_runtime_err_check(uIRofOvfParam, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, uPos, uErrCheckKind, pNode, pTCStatement, pTCContext);
                if (pTCContext->pProcResult) {
                    pTCStatement->uLastIRorGlobalTCResult = uPos + 2u;
                }
            }
        }
    }
    return eResult;
    */
}

// Warning: infoShift format is always supposed 32b, and shall be called AFTER ensuring of non negative, and strict less than number of bits in format
local_func EIRResult ir_emit_or_solve_left_shift(u8 uFormat, const IRInfo& infoA, const IRInfo& infoShift,
    TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_left_shift(uFormat, infoA, infoShift,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BIT_LSH) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoShift.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

// Warning: infoShift format is always supposed 32b, and shall be called AFTER ensuring of non negative, and strict less than number of bits in format
local_func EIRResult ir_emit_or_solve_right_shift(u8 uFormat, const IRInfo& infoA, EIntSemantics eSemantics, const IRInfo& infoShift,
    TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)
{
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_right_shift(uFormat, infoA, eSemantics, infoShift,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }

    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        u32 uInstrFlags = eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED ? IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED : 0u;
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BIT_RSH) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoShift.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outResultingInfo->uIRandMetaFlags = u64(uMetaFlags) | ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
    }
    return eResult;
}

// AND / OR / NOT *when already handled by branching conditions*
local_func EIRResult ir_emit_or_solve_pseudo_boolean_op(u8 uOp, const IRInfo& infoA, const IRInfo& infoB, TCStatement* pTCStatement, TCContext* pTCContext,
                                                        IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_boolean_op(uOp, infoA, infoB, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(0x00u, uMetaFlags, outResultingInfo, eResult);
    }
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PSEUDO_VALUED_COND) | (u64(uOp) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags|IRFLAG_IS_PSEUDO_VALUED_COND) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags|IRFLAG_IS_PSEUDO_VALUED_COND);

        Assert_(pTCContext->pProcResult); // a const bool would necessarily be immediate => skipped above => we're requiring runtime here.
        pTCStatement->uLastIRorGlobalTCResult = uPos;
    }
    return eResult;
}

// NOT *when NOT already handled by branching conditions*
local_func EIRResult ir_emit_or_solve_boolean_not(const IRInfo& infoIn, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                  IRInfo* outResultingInfo)
{
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_boolean_op(ETOK_BOOL_NOT, infoIn, IRInfo{}, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(0x00u, uMetaFlags, outResultingInfo, eResult);
    }
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_BOOL_NOT) | (infoIn.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);

        Assert_(pTCContext->pProcResult); // a const bool would necessarily be immediate => skipped above => we're requiring runtime here.
        pTCStatement->uLastIRorGlobalTCResult = uPos;
    }
    return eResult;
}

// EQ / NEQ
local_func EIRResult ir_emit_or_solve_eq_cmp_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB, u32 uIsNeqFlag, u32 uDirectUsageFlag,
        TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)
{
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(ir_is_valid_param_(infoB.uIRandMetaFlags));
    Assert_(uIsNeqFlag == 0u || uIsNeqFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uDirectUsageFlag == 0u || uDirectUsageFlag == IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES);
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_eq_cmp_integral(uFormat, infoA, infoB, uIsNeqFlag, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        u32 uInstrFlags = uIsNeqFlag|uDirectUsageFlag;
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CMP_EQ) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);

        Assert_(pTCContext->pProcResult); // a const bool would necessarily be immediate => skipped above => we're requiring runtime here.
        pTCStatement->uLastIRorGlobalTCResult = uPos;
    }
    return eResult;
}

// LT / GE
local_func EIRResult ir_emit_or_solve_ord_cmp_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB, u32 uIsGeFlag, u32 uDirectUsageFlag,
        EIntSemantics eSemantics, TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)
{
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(ir_is_valid_param_(infoB.uIRandMetaFlags));
    Assert_(uIsGeFlag == 0u || uIsGeFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uDirectUsageFlag == 0u || uDirectUsageFlag == IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES);
    u32 uMetaFlags;
    EIRResult eResult = ir_try_solve_ord_cmp_integral(uFormat, infoA, infoB, uIsGeFlag, eSemantics, pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eResult <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uFormat, uMetaFlags, outResultingInfo, eResult);
    }
    if (eResult < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* opEntry = ir_append_new_entry(pTCContext->pRepo);
        u32 uInstrFlags = uIsGeFlag|uDirectUsageFlag;
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED)
            uInstrFlags |= IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED;
        opEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CMP_ORD) | u64(uInstrFlags) | (u64(uFormat) << 16u) | (infoA.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (infoB.uIRandMetaFlags & IR_STD_PARAM_MASK);
        opEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);

        Assert_(pTCContext->pProcResult); // a const bool would necessarily be immediate => skipped above => we're requiring runtime here.
        pTCStatement->uLastIRorGlobalTCResult = uPos;
    }
    return eResult;
}

local_func EIRResult ir_emit_or_solve_address_of(const IRInfo& infoEntity, TCStatement* pTCStatement, TCContext* pTCContext,
    IRInfo* outResultingInfo)
{
    u64 uEntityIR = infoEntity.uIRandMetaFlags & IR_STD_PARAM_MASK;
    Assert_(!ir_is_immediate(uEntityIR));
    u32 uMetaFlags;
    EIRResult eSolveAddressOf = ir_try_solve_address_of(infoEntity, pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eSolveAddressOf < EIRResult::EIRR_FIRST_ERROR) {
        // in all valid cases, meta-info is nyka of entity IR (with offset 0)
        Assert_(outResultingInfo->metaValue.knownValue.uEmbeddedValue == ir_make_direct_nyka_value(uEntityIR));
        Assert_(uMetaFlags & IRFLAG_IS_KNOWN);
        Assert_(uMetaFlags & IRFLAG_HAS_NYKA);
        Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD);

        if (eSolveAddressOf <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {

            // and if const or non-local, IR is also nyka immediate of entity IR
            Assert_(0u == (uMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
            outResultingInfo->uIRandMetaFlags = ir_make_nyka_immediate(uEntityIR) | u64(uMetaFlags);

        } else {

            // otherwise, we represent it with a local address entry.
            Assert_(uMetaFlags & IRFLAG_HAS_LOCAL_NYKA);
            Assert_(pTCContext->pProcResult); // ...and the only contexts with such IRs are themselves local

            // => helps with detecting them (will help in the future with alias analysis etc).
            //   those are furthermore "special" in that, even if a nyka value is "known" for them to the typechecker and IR,
            //   their actual address will not be solvable as a "constant" in the final executable (for most backends).

            u32 uPos = pTCContext->pRepo->uSize;
            IREntry* addressofEntry = ir_append_new_entry(pTCContext->pRepo);
            addressofEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_LOCAL_ADDRESS) | uEntityIR;
            addressofEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags);
            addressofEntry->metaValue = outResultingInfo->metaValue;

            u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
            outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        }
    }
    return eSolveAddressOf;
}

local_func bool is_erroneous_nyka_immediate(const IRInfo& infoPtr, IRAwareContext* pContext)
{
    u64 uIRofPtr = infoPtr.uIRandMetaFlags & IR_STD_PARAM_MASK;
    if (ir_is_nyka_immediate(uIRofPtr)) {
        u64 uRefIR = ir_get_param_from_nyka_immediate(uIRofPtr);
        Assert_(!ir_is_immediate(uRefIR));
        IRRepo* pRefRepo;
        u32 uRefIndex;
        SourceFileDescAndState* pRefFile;
        EEntryKind eRefKind;
        ir_decode_non_imm(uRefIR, pContext, &pRefRepo, &uRefIndex, &pRefFile, &eRefKind);
        if (eRefKind == EEntryKind::EEK_CURRENT_PROC_LOCAL) {
            Assert_(pRefRepo);
            IREntry& entry = ir_access_repo_instr(pRefRepo, uRefIndex);
            if (0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN))
                return true;
            if (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)
                return true;
        }
    }
    return false;
}

local_func EIRResult ir_emit_or_solve_ptr_offset(u32 uEnsuredAlignLog2, const IRInfo& infoBasePtr, u8 uIndexFormat, const IRInfo& infoIndex,
    u32 uIndexScale, u32 uOnlyForDerefFlag, EIntSemantics eSemantics, TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)
{
    Assert_(uEnsuredAlignLog2 <= 12u);
    Assert(uIndexFormat >= 0x02u && uIndexFormat <= 0x07u, "Only possible formats for ptr_offset index are >= 32b integrals");
    Assert_(uIndexScale > 0u);
    Assert_(uOnlyForDerefFlag == 0u || uOnlyForDerefFlag == IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    Assert_(!is_erroneous_nyka_immediate(infoBasePtr, pTCContext));
    u32 uMetaFlags;
    EIRResult eSolvePtrOffset = ir_try_solve_ptr_offset(uEnsuredAlignLog2, infoBasePtr, uIndexFormat, infoIndex, uIndexScale, eSemantics,
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));
    if (eSolvePtrOffset <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
        if ((uMetaFlags & IRFLAG_IS_KNOWN) && (uMetaFlags & IRFLAG_HAS_NYKA) && 0u == (uMetaFlags & IRFLAG_HAS_LOCAL_NYKA))  {
            Assert_(uMetaFlags & IRFLAG_IS_KNOWN_EMBD);
            i32 iConstOffset;
            u64 uBaseIR = ir_decode_nyka_value(outResultingInfo->metaValue.knownValue.uEmbeddedValue, &iConstOffset);
            if (iConstOffset == 0) {
                outResultingInfo->uIRandMetaFlags = ir_make_nyka_immediate(uBaseIR) | u64(uMetaFlags);
                return eSolvePtrOffset;
            }
        }
    }
    if (eSolvePtrOffset < EIRResult::EIRR_FIRST_ERROR) {
        u32 uPos = pTCContext->pRepo->uSize;
        u64 uIRofBasePtr = infoBasePtr.uIRandMetaFlags & IR_STD_PARAM_MASK;
        u64 uIRofIndex = infoIndex.uIRandMetaFlags & IR_STD_PARAM_MASK;
        u64 uInstrFlags = u64(uOnlyForDerefFlag|(uEnsuredAlignLog2<<8u));
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED)
            uInstrFlags |= IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED;

        // for simple 'IRIT_PTR_OFFSET', We'll encode 1..256 index-scale in 8b format slot, 4b align in 4lsb of instrflags, and suppose indexformat as i32.
        if (uIndexFormat == 0x02u && uIndexScale <= 256u) {

            IREntry* offsetEntry = ir_append_new_entry(pTCContext->pRepo);
            u8 uEncodedScale = u8(uIndexScale-1u);
            offsetEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PTR_OFFSET) | uInstrFlags | (u64(uEncodedScale)<<16) | uIRofBasePtr;
            offsetEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | uIRofIndex;
            offsetEntry->metaValue = outResultingInfo->metaValue;
            if (pTCContext->pProcResult)
                pTCStatement->uLastIRorGlobalTCResult = uPos;

        } else { // otherwise (larger index scale, or different index format) we'd need IRIT_PTR_OFFSET_EXT

            IREntry* offsetExtEntry = ir_append_new_entry(pTCContext->pRepo);
            offsetExtEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_PTR_OFFSET_EXT) | uInstrFlags | uIRofBasePtr;
            offsetExtEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (u64(uIndexScale) << IR_STD_PARAM_SHIFT);
            offsetExtEntry->metaValue = outResultingInfo->metaValue;
            IREntry* additionalParamEntry = ir_append_new_entry(pTCContext->pRepo);
            additionalParamEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CALLER_IN_PARAM) | (u64(uIndexFormat)<<16u) | uIRofIndex;
            u64 uSlotsCountAndAlign = 1uLL | (u64(get_log2_of_natural_align_from_format(uIndexFormat)) << 32);
            additionalParamEntry->uInstrMetaFlagsAndSecondParam = (infoIndex.uIRandMetaFlags & IR_STD_PARAM_METAMASK) | (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT);
            additionalParamEntry->metaValue = infoIndex.metaValue;
            if (pTCContext->pProcResult)
                pTCStatement->uLastIRorGlobalTCResult = uPos + 1u;
        }

        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);
    }
    return eSolvePtrOffset;
}

local_func EIRResult ir_emit_or_solve_deref(const IRInfo& infoPtr, u8 uFormat, u32 uAlignLog2, u32 uSlotsCount, u32 uBytesCount,
    u32 uIsAssignableFlag, TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outResultingInfo)

{
    Assert_(uIsAssignableFlag == 0u || uIsAssignableFlag == IR_INSTRFLAG_IS_ASSIGNABLE);
    Assert_(!is_erroneous_nyka_immediate(infoPtr, pTCContext));
    u32 uMetaFlags;
    EIRResult eSolveDeref = ir_try_solve_deref(infoPtr, uFormat, uAlignLog2, uSlotsCount, uBytesCount, uIsAssignableFlag, 
        pTCContext, &uMetaFlags, &(outResultingInfo->metaValue));

    // Always emit IR for a deref : (must be re-referencable...)
    if (eSolveDeref < EIRResult::EIRR_FIRST_ERROR) {
        uMetaFlags |= IRFLAG_TC_REFERENCABLE;
        u64 uIRofPtr = infoPtr.uIRandMetaFlags & IR_STD_PARAM_MASK;
        u32 uPos = pTCContext->pRepo->uSize;
        IREntry* offsetEntry = ir_append_new_entry(pTCContext->pRepo);
        offsetEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_DEREF) | uIsAssignableFlag | (u64(uFormat)<<16) | uIRofPtr;
        u64 uSlotCountAndAlign = u64(uSlotsCount) | (u64(uAlignLog2) << 32);
        offsetEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (uSlotCountAndAlign << IR_STD_PARAM_SHIFT);
        offsetEntry->metaValue = outResultingInfo->metaValue;
        u64 uIRofResult = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos);
        outResultingInfo->uIRandMetaFlags = uIRofResult | u64(uMetaFlags);
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
    }
    return eSolveDeref;
}

local_func EIRResult ir_emit_or_solve_unary_minus_integral(u8 uFormat, const IRInfo& infoIn,
                                                           EIntSemantics eSemantics, TmpTCNode* pNode, TCStatement* pTCStatement, TCContext* pTCContext,
                                                           IRInfo* outResultingInfo)
{
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || EIntSemantics::EINT_SEMANTIC_MODULO_ARITH);
    Assert_(uFormat <= 0x05u);
    IRInfo infoZero;
    if (uFormat <= 0x03u)
        infoZero = info0WhenEmbeddedIntegral;
    else {
        EIRResult checkDeref0 = ir_emit_or_solve_deref(g_infoAddressOfZero1024b, uFormat, uFormat, 1u, 1u << uFormat, 0u,
            pTCStatement, pTCContext, &infoZero);
        Assert_(checkDeref0 == EIRResult::EIRR_ENSURED_VALID_KNOWN);
        Assert_(infoZero.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        Assert_(0u == (infoZero.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
        Assert_(0u == (infoZero.uIRandMetaFlags & IRFLAG_HAS_NYKA));
        Assert_(infoZero.uIRandMetaFlags & IRFLAG_IS_KNOWN_ZERO);
    }

    return ir_emit_or_solve_add_or_sub_integral(uFormat, infoZero, infoIn, 0x8000'0000'0000'0000uLL, eSemantics,
        pNode, pTCStatement, pTCContext, outResultingInfo);
}

local_func EIRResult ir_emit_or_solve_truncate_or_extend_integral_to_integral(u8 uSrcFormat, const IRInfo& srcInfo,
    EIntSemantics eSemantics, const TypeInfo_Integral* pResultType, TCStatement* pTCStatement, TCContext* pTCContext, IRInfo* outInfo)
{
    Assert_(uSrcFormat <= 0x07u);
    Assert_(pResultType && pResultType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    u8 uDestFormat = get_ir_format(pResultType);
    Assert_(uDestFormat <= 0x07u);
    bool bDestSigned = is_signed(pResultType);
    Assert_(!bDestSigned || uDestFormat <= 0x05u);
    Assert_(eSemantics != EIntSemantics::EINT_SEMANTIC_SIGNED || (uSrcFormat <= 5u && uDestFormat <= 5u)); // TODO: CLEANUP: ???
    Assert_(0 == (srcInfo.uIRandMetaFlags & IRFLAG_HAS_NYKA)); //TODO ??
    Assert_(0 == (srcInfo.uIRandMetaFlags & IRFLAG_TC_ONLY));
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED ||
            eSemantics == EIntSemantics::EINT_SEMANTIC_MODULO_ARITH);

    u32 uMetaFlags = 0u;
    EIRResult eSolved = ir_try_solve_truncate_or_extend_integral_to_integral(uSrcFormat, srcInfo, eSemantics, uDestFormat, pTCContext,
        &uMetaFlags, &(outInfo->metaValue));

    u64 uSourceIR = srcInfo.uIRandMetaFlags & IR_STD_PARAM_MASK;
    if (ir_is_valid_param(uSourceIR)) { // may not be the case if called with mock from compint 

        if (uSrcFormat == uDestFormat) {
            Assert_(eSolved == EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A);
            Assert_(uMetaFlags == (u32(srcInfo.uIRandMetaFlags) & IRFLAGS_IR_SPECIFIC_MASK));
            Assert_(outInfo->metaValue._payload == srcInfo.metaValue._payload);
            outInfo->uIRandMetaFlags = srcInfo.uIRandMetaFlags & (IR_STD_PARAM_MASK|u64(IRFLAGS_IR_SPECIFIC_MASK));
            return eSolved;
        } else {
            u32 uPos = pTCContext->pRepo->uSize;
            IREntry* pNewEntry = ir_append_new_entry(pTCContext->pRepo);
            if (uSrcFormat < uDestFormat) {
                Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
                Assert_(eSemantics != EIntSemantics::EINT_SEMANTIC_SIGNED || uDestFormat <= 0x05u); // TODO: CLEANUP: ???
                u32 uInstrFlags = (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED) ? IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED : 0u;
                pNewEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_CAST) | u64(uInstrFlags) | (u64(uDestFormat) << 16) | uSourceIR;
                pNewEntry->uInstrMetaFlagsAndSecondParam = (uSrcFormat << IR_STD_PARAM_SHIFT) | uMetaFlags;
            } else { Assert_(uSrcFormat > uDestFormat);
                Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_MODULO_ARITH);
                // srcformat replaces up to 7lsb of instr flags in IRIT_REINTERP (up to 3 in this case for scalar integrals)
                pNewEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_REINTERP) | (u64(uSrcFormat) << 8) | (u64(uDestFormat) << 16) | uSourceIR;
                u64 uSlotsCountAndAlign = (u64(uDestFormat) << 32) | 1uLL;
                pNewEntry->uInstrMetaFlagsAndSecondParam = (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT) | uMetaFlags;
            }
            pNewEntry->metaValue = outInfo->metaValue;
            if (pTCContext->pProcResult)
                pTCStatement->uLastIRorGlobalTCResult = uPos;
            outInfo->uIRandMetaFlags = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos) | uMetaFlags;
            return eSolved;
        }

    // so, in case our source had invalid IR (ie, it was a mock tmp value used to cast from a compint),
    } else { //... we'll need to create an IR entry from scratch to hold our value.

        Assert_(uMetaFlags & IRFLAG_IS_KNOWN);
        Assert_(0u == (uMetaFlags & IRFLAG_HAS_NYKA));

        if (uMetaFlags & IRFLAG_IS_KNOWN_EMBD) { // but in case of embedded results...
            // ... we'll, first, check if this could not fit in an IR immediate instead of creating a new entry
            IR_EMITTER_MAY_SHORTCUT_RETURN_INTEGRAL_AS_IMMEDIATE(uDestFormat, uMetaFlags, outInfo, eSolved);
        }
        // so, in case we could not create a new entry, we'll declare a new value directly holding the *result* of the cast,
        // without referencing a new value
        u32 uPos = ir_make_decl_entry(pTCContext->pRepo, 0u, uMetaFlags,
            outInfo->metaValue._payload, uDestFormat, get_runtime_sizeof(pResultType), get_log2_of_align_bytes(pResultType));
        if (pTCContext->pProcResult)
            pTCStatement->uLastIRorGlobalTCResult = uPos;
        outInfo->uIRandMetaFlags = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPos) | uMetaFlags;
        return eSolved;
    }
}

local_func EIRResult ir_try_get_nyka_to_byte_array_from_string_instance(const IRInfo& stringInfo,
    const TypeInfo_OtherCore* pStringType, TCContext* pTCContext, u64* outNykaResult)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    if (stringInfo.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST) {
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_HAS_NYKA); // TODO: allow const null FF ??
        Assert_(0u == (stringInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
        if (pStringType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING]) {
            *outNykaResult = stringInfo.metaValue.knownValue.uEmbeddedValue;
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        } else {
            // TODO
            platform_log_error("ir_try_get_nyka_to_byte_array_from_string_instance() : not yet implemented for other than compact strings");
            return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
        }
    }
    *outNykaResult = 0u;
    return EIRResult::EIRR_UNKNOWN;
}

local_func EIRResult ir_try_get_nyka_to_length_in_bytes_from_string_instance(const IRInfo& stringInfo,
    const TypeInfo_OtherCore* pStringType, TCContext* pTCContext, u64* outNykaResult)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    if (stringInfo.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST) {
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_HAS_NYKA); // TODO: allow const null FF ??
        Assert_(0u == (stringInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
        if (pStringType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING]) {
            i32 iOffset; u64 uBaseIR = ir_decode_nyka_value(stringInfo.metaValue.knownValue.uEmbeddedValue, &iOffset);
            Assert_(!ir_is_immediate(uBaseIR));
            Assert_(iOffset > 4);
            *outNykaResult = ir_make_nyka_value(uBaseIR, iOffset - 4);
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        } else {
            // TODO
            platform_log_error("ir_try_get_nyka_to_length_in_bytes_from_string_instance() : not yet implemented for other than compact strings");
            return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
        }
    }
    *outNykaResult = 0u;
    return EIRResult::EIRR_UNKNOWN;
}

local_func EIRResult ir_try_get_nyka_to_flags_from_string_instance(const IRInfo& stringInfo,
    const TypeInfo_OtherCore* pStringType, TCContext* pTCContext, u64* outNykaResult)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    if (stringInfo.uIRandMetaFlags & IRFLAG_TC_SEMANTIC_CONST) {
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        Assert_(stringInfo.uIRandMetaFlags & IRFLAG_HAS_NYKA); // TODO: allow const null FF ??
        Assert_(0u == (stringInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
        if (pStringType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING]) {
            i32 iOffset; u64 uBaseIR = ir_decode_nyka_value(stringInfo.metaValue.knownValue.uEmbeddedValue, &iOffset);
            Assert_(!ir_is_immediate(uBaseIR));
            Assert_(iOffset > 4);
            *outNykaResult = ir_make_nyka_value(uBaseIR, iOffset - 8);
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        } else {
            // TODO
            platform_log_error("ir_try_get_nyka_to_flags_from_string_instance() : not yet implemented for other than compact strings");
            return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
        }
    }
    *outNykaResult = 0u;
    return EIRResult::EIRR_UNKNOWN;
}

/*
local_func EIRResult ir_emit_or_solve_get_ptr_to_bytes_from_string_instance(const IRInfo& stringInfo,
    const TypeInfo_OtherCore* pStringType, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    if (pStringType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING]) {
        *outInfo = stringInfo;
        return EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A;
    } else {
        // TODO
        platform_log_error("ir_emit_or_solve_get_ptr_to_bytes_from_string_instance() : not yet implemented for other than compact strings");
        return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
    }
}

local_func EIRResult ir_emit_or_solve_get_length_in_bytes_from_string_instance(const IRInfo& stringInfo,
    const TypeInfo_OtherCore* pStringType, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    if (pStringType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING]) {
        IRInfo infoOffset = ir_make_info_for_int_immediate(-1, 0x02u);
        IRInfo ptrToLengthInfo;
        EIRResult eResultPtrOffset = ir_emit_or_solve_ptr_offset(2u, stringInfo, 0x02u, infoOffset, 4u, IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF,
            EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &ptrToLengthInfo);
        if (eResultPtrOffset < EIRResult::EIRR_FIRST_ERROR) {
            return ir_emit_or_solve_deref(ptrToLengthInfo, 0x02u, 2u, 1u, 4u, 0u, pTCStatement, pTCContext, outInfo);
        } else {
            return eResultPtrOffset;
        }
    } else {
        // TODO
        platform_log_error("ir_emit_or_solve_get_length_in_bytes_from_string_instance() : not yet implemented for other than compact strings");
        return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
    }
}

local_func EIRResult ir_emit_or_solve_get_flags_from_string_instance(const IRInfo& stringInfo,
    const TypeInfo_OtherCore* pStringType, TCStatement* pTCStatement, TCContext* pTCContext,
    EExpectedExpr eExpectation, IRInfo* outInfo)
{
    Assert_(pStringType->_coreFlags & OTHERCOREFLAG_IS_STRING);
    if (pStringType == g_pCoreTypesInfo[ECORETYPE_COMPACT_STRING]) {
        IRInfo infoOffset = ir_make_info_for_int_immediate(-2, 0x02u);
        IRInfo ptrToFlagsInfo;
        EIRResult eResultPtrOffset = ir_emit_or_solve_ptr_offset(2u, stringInfo, 0x02u, infoOffset, 4u, IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF,
            EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &ptrToFlagsInfo);
        if (eResultPtrOffset < EIRResult::EIRR_FIRST_ERROR) {
            return ir_emit_or_solve_deref(ptrToFlagsInfo, 0x02u, 2u, 1u, 4u, 0u, pTCStatement, pTCContext, outInfo);
        } else {
            return eResultPtrOffset;
        }
    } else {
        // TODO
        platform_log_error("ir_emit_or_solve_get_flags_from_string_instance() : not yet implemented for other than compact strings");
        return EIRResult::EIRR_EMITTER_NOT_YET_IMPLEMENTED;
    }
}
*/


#endif // LOCLIB_IR_SOLVER_INTERFACE_H_

