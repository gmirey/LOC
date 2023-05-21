#pragma once 

#ifndef LOCLIB_IR_SOLVER_H_
#define LOCLIB_IR_SOLVER_H_

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

enum EIRResult : u16 {
    EIRR_ENSURED_VALID_SAME_AS_OPERAND_A,
    EIRR_ENSURED_VALID_SAME_AS_OPERAND_B,
    EIRR_ENSURED_VALID_KNOWN,
    EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA, // if operation required truncation or evaluation of a NYKA value.
    EIRR_ENSURED_VALID_UNKNOWN,

    EIRR_UNKNOWN_CAUSED_BY_NYKA,               // if operation required truncation or evaluation of a NYKA value.
    EIRR_UNKNOWN,
    EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED,
    
    EIRR_FIRST_ERROR,

    // EIRR with same codes as TC codes for certain categories of errors...

    EIRR_SIGNED_OVERFLOW = CERR_OPERATION_WOULD_OVERFLOW,
    EIRR_INEXACT_QUOTIENT = CERR_INEXACT_QUOTIENT,
    EIRR_UNSIGNED_OVERFLOW = CERR_OPERATION_WOULD_OVERFLOW,
    EIRR_DIVISION_BY_ZERO = CERR_DIVISION_BY_ZERO,
    EIRR_EMITTER_NOT_YET_IMPLEMENTED = FERR_NOT_YET_IMPLEMENTED,
    EIRR_ERROR = FERR_OTHER,

};

enum EIntSemantics {
    EINT_SEMANTIC_MODULO_ARITH,     // modulo arithmetics (and no check)
    EINT_SEMANTIC_MODULO_SIGNED,    // modulo arithmetics (and no check - yet signed (for mul))
    EINT_SEMANTIC_UNSIGNED,         // unsigned semantics and/or checks
    EINT_SEMANTIC_SIGNED,           // signed semantics and/or checks
    EINT_SEMANTIC_UA_SB,            // mixed semantics and/or checks, with unsigned operand A and signed operand B
    EINT_SEMANTIC_SA_UB,            // mixed semantics and/or checks, with unsigned operand A and signed operand B
};

local_func void ir_get_info_from_non_imm(u64 uIR, IRAwareContext* pCtx, IRInfo* outInfo) {
    IRRepo* pRepo;
    u32 uIndex;
    SourceFileDescAndState* pSourceFile;
    EEntryKind eKind;
    ir_decode_non_imm(uIR, pCtx, &pRepo, &uIndex, &pSourceFile, &eKind);
    if (pRepo) {
        Assert_(eKind != EEntryKind::EEK_IS_PROCBODY_REF);
        Assert_(eKind != EEntryKind::EEK_NOT_AN_ENTRY);
        if (eKind != EEntryKind::EEK_FILEWISE_VAR) {
            // for all but global-variables entries, info about the currently known value (or lack thereof) is carried by the IR-entry itself
            IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
            outInfo->uIRandMetaFlags = uIR | (entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_METAMASK);
            outInfo->metaValue = entry.metaValue;
        } else { // for global variables, we should not read its entry metaflags and metavalue as we did above.
            // ... since they contains info of *initial value* ; when the actual knowledge of the current value by the typechecker is "none":
            outInfo->uIRandMetaFlags = uIR;
            outInfo->metaValue._payload = 0uLL;
        }
    } else {
        Assert_(eKind == EEntryKind::EEK_IS_PROCBODY_REF);
        // value for IR-param of a proc body ref is a nyka to itself...
        outInfo->uIRandMetaFlags = uIR | (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA|IRFLAG_TC_SEMANTIC_CONST);
        outInfo->metaValue.knownValue.uEmbeddedValue = ir_make_direct_nyka_value(uIR);
    }
}

local_func void ir_get_info_from_ir_param(u64 uIR, u8 uFormat, IRAwareContext* pCtx, IRInfo* outInfo) {
    if (ir_is_numeric_immediate(uIR)) {
        Assert_(0u == (uFormat & 0xF0u));                       // true after Major Immediate & Embedded simplification
        Assert_(0x03u >= (uFormat & 0x07u));                    // true after Major Immediate & Embedded simplification
        ir_get_info_from_numeric_immediate(uIR, uFormat, outInfo);
    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(uIR)) {
        Assert_(uFormat == 0x03u);                              // true after Major Immediate & Embedded simplification
        ir_get_info_from_nyka_immediate(uIR, outInfo);
    } else {
        ir_get_info_from_non_imm(uIR, pCtx, outInfo);
    }
}

local_func_inl u8 ir_get_u8_value_from_known(const IRInfo& info) {
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    return *ir_get_ptr_to_data_from_known(info);
}

local_func_inl u16 ir_get_u16_value_from_known(const IRInfo& info) {
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    return *reinterpret_cast<const u16*>(ir_get_ptr_to_data_from_known(info));
}

local_func_inl u32 ir_get_u32_value_from_known(const IRInfo& info) {
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    return *reinterpret_cast<const u32*>(ir_get_ptr_to_data_from_known(info));
}

local_func_inl u64 ir_get_u64_value_from_known(const IRInfo& info) {
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    return *reinterpret_cast<const u64*>(ir_get_ptr_to_data_from_known(info));
}

local_func_inl u128 ir_get_u128_value_from_known(const IRInfo& info) {
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    return *reinterpret_cast<const u128*>(ir_get_ptr_to_data_from_known(info));
}

local_func_inl u256 ir_get_u256_value_from_known(const IRInfo& info) {
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    return *reinterpret_cast<const u256*>(ir_get_ptr_to_data_from_known(info));
}

local_func bool ir_check_if_integral_is_one(u8 uFormat, const IRInfo& info)
{
    if (irflag_is_known_non_nyka(info.uIRandMetaFlags)) {
        if (uFormat <= 0x03u) {
            Assert_(irflag_is_known_embd(info.uIRandMetaFlags));
            // (1u << (uFormat+3u)) is number of bits in format. The following assert checks that high bits above format are zeroed as they should
            Assert_(uFormat == 0x03u || 0uLL == ((0xFFFF'FFFF'FFFF'FFFFuLL << (1u << (uFormat+3u))) & info.metaValue.knownValue.uEmbeddedValue));
            return info.metaValue.knownValue.uEmbeddedValue == 1uLL; // should work for 8b, 16b, 32b, 64b ; provided the above assert holds.
        } else { Assert_(uFormat >= 0x04u && uFormat <= 0x07u);
            Assert_(!irflag_is_known_embd(info.uIRandMetaFlags));
            u64* pLegs = reinterpret_cast<u64*>(info.metaValue.knownValue.pPtrToRawData);
            if (pLegs[0] == 1uLL) {
                u32 uLegCount = (1u << (uFormat-3u));    // eg for 0x05u, == 1u << 2 == 4 == count of u64 legs in an u256.
                for (u32 uLeg = 1u; uLeg < uLegCount; uLeg++) {
                    if (pLegs[uLeg])
                        return false;
                }
                return true;
            } // otherwise fallthrough false
        }
    }
    return false;
}

local_func bool ir_check_if_integral_is_all_bits_set(u8 uFormat, const IRInfo& info)
{
    if (irflag_is_known_non_nyka(info.uIRandMetaFlags)) {
        if (uFormat <= 0x03u) {
            Assert_(irflag_is_known_embd(info.uIRandMetaFlags));
            // (1u << (uFormat+3u)) is number of bits in format
            u64 uMaskOutOfRange = uFormat == 0x03u ? 0uLL : (0xFFFF'FFFF'FFFF'FFFFuLL << (1u << (uFormat+3u)));
            // The following assert checks that high bits above format are zeroed as they should
            Assert_(0uLL == (uMaskOutOfRange & info.metaValue.knownValue.uEmbeddedValue));
            return ((~info.metaValue.knownValue.uEmbeddedValue) == uMaskOutOfRange); // should work for 8b, 16b, 32b, 64b
        } else { Assert_(uFormat >= 0x04u && uFormat <= 0x07u);
            Assert_(!irflag_is_known_embd(info.uIRandMetaFlags));
            u64* pLegs = reinterpret_cast<u64*>(info.metaValue.knownValue.pPtrToRawData);
            u32 uLegCount = (1u << (uFormat-3u));    // eg for 0x05u, == 1u << 2 == 4 == count of u64 legs in an u256.
            for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
                if (pLegs[uLeg] != 0xFFFF'FFFF'FFFF'FFFFuLL)
                    return false;
            }
            return true;
        }
    } else
        return false;
}

local_func_inl const u64* ir_get_tlegs_from_known_non_nyka_integral(u32 uIsEmbedded, AKnownValue knownValue) {
    Assert_(uIsEmbedded == 0u || uIsEmbedded == IRFLAG_IS_KNOWN_EMBD);
    if (uIsEmbedded) {
        return &(knownValue.uEmbeddedValue);
    } else {
        return reinterpret_cast<const u64*>(knownValue.pPtrToRawData);
    }
}

local_func bool does_footprint_avoids_nyka_in_table(const IRInfo& info, u32 uUnalignedBytes, u32 uOffset = 0)
{
    Assert_(uUnalignedBytes);
    Assert_(0uLL == (info.uIRandMetaFlags & IRFLAG_TC_ONLY));
    Assert_(info.uIRandMetaFlags & IRFLAG_IS_KNOWN);
    Assert_(info.uIRandMetaFlags & IRFLAG_HAS_NYKA);
    Assert_(0uLL == (info.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
    // We need to iterate over all registered NYKA positions to check whether they overlap the desired footprint
    const u32* pPtrToNykaTable = reinterpret_cast<const u32*>(info.metaValue.knownValue.pPtrToRawData);
    u32 uNykaCount = pPtrToNykaTable[0u];
    Assert_(uNykaCount);
    u32 uEnd = uOffset + uUnalignedBytes;
    for (u32 uNykaIndexAndOne = 1u; uNykaIndexAndOne <= uNykaCount; uNykaIndexAndOne++) {
        u32 uNykaOffset = pPtrToNykaTable[uNykaIndexAndOne];
        if (uNykaOffset < uEnd && uNykaOffset + 8u >= uOffset)
            return false;
    }
    return true; // we did not find any overlap !
}

// Returns whether bytes at a given position within an instance are fully known at compile (TC) time,
//   that is, known AND not overlapping any position flaggued as NYKA.
local_func_inl bool is_footprint_fully_evaluable(const IRInfo& info, u32 uUnalignedBytes, u32 uOffset = 0) {
    Assert_(uUnalignedBytes);
    Assert_(0uLL == (info.uIRandMetaFlags & IRFLAG_TC_ONLY));
    if (info.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
        u32 uEnd = uOffset + uUnalignedBytes;
        Assert_(uEnd <= 8 || 0uLL == (info.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD)); // true after Immediates and Embedding simplification
        if (0uLL == (info.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
            return true;  // embedded with no nyka => OK
        } else if (info.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD) {
            return false; // embedded with nyka => necessarily within a nyka, since NYKA spans the full 64b of the embedding
        } else
            return does_footprint_avoids_nyka_in_table(info, uUnalignedBytes, uOffset);
    } else      // not a known info to begin with...
        return false;
}

// Returns whether a given value is known and fully zero (excludes all cases of NYKAs),
// Note that is is not exactly the same as testing for 0 for eg, floats (with values being -0)
local_func_inl bool ir_check_full_zero_value(const IRInfo& info, u32 uUnalignedBytes)
{
    Assert_(uUnalignedBytes);
    Assert_(0uLL == (info.uIRandMetaFlags & IRFLAG_TC_ONLY));
    if (irflag_is_known_non_nyka(info.uIRandMetaFlags)) {
        const u8* pPtrToData = ir_get_ptr_to_data_from_known(info);
        constexpr u64 tZeroes[4096] = {};
        u32 uRemainingToTest = uUnalignedBytes;
        while (uRemainingToTest > 4096*sizeof(u64)) {
            if (memcmp(pPtrToData, tZeroes, 4096*sizeof(u64)))
                return false;
            pPtrToData += 4096*sizeof(u64);
            uRemainingToTest -= 4096*sizeof(u64);
        }
        Assert_(uRemainingToTest);
        return memcmp(pPtrToData, tZeroes, uRemainingToTest) == 0u;
    } else
        return false;
}

#define IR_SOLVER_RETURN_ENSURED_SAME_AS(letterAorB) do { \
    *outFlags = u32(info ## letterAorB.uIRandMetaFlags) & IRFLAGS_IR_SPECIFIC_MASK; \
    *outMetaValue = info ## letterAorB.metaValue; \
    return EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_ ## letterAorB; \
} while(0)


local_func EIRResult ir_try_solve_add_or_sub_integral_when_fully_evaluable_nz(u8 uFormat, AKnownValue knownValueA, AKnownValue knownValueB,
    u64 uHighBitSetIfSub, EIntSemantics eSemantics, TCContext* pTCContext, u32* outFlags, u64* outLegs)
{
    Assert_(uHighBitSetIfSub == 0uLL || uHighBitSetIfSub == 0x8000'0000'0000'0000uLL);
    Assert_(uFormat <= 0x05u);

    #define COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(uBits) do { \
        switch (eSemantics) { \
            case EIntSemantics::EINT_SEMANTIC_MODULO_ARITH: { \
                uResult = uHighBitSetIfSub ? sub ## uBits(uA, uB) : add ## uBits(uA, uB); \
            } break; \
            case EIntSemantics::EINT_SEMANTIC_SIGNED: { \
                u8 err; \
                uResult = uHighBitSetIfSub ? sub ## uBits ## _chk_sgn(uA, uB, &err) : add ## uBits ## _chk_sgn(uA, uB, &err); \
                if (err) \
                    return EIRResult::EIRR_SIGNED_OVERFLOW; \
            } break; \
            case EIntSemantics::EINT_SEMANTIC_UNSIGNED: { \
                u8 err; \
                uResult = uHighBitSetIfSub ? sub ## uBits ## _chk_uns(uA, uB, &err) : add ## uBits ## _chk_uns(uA, uB, &err); \
                if (err) \
                    return EIRResult::EIRR_UNSIGNED_OVERFLOW; \
            } break; \
            default: { \
                platform_log_error("ir_try_solve_add_or_sub_integral() : mixed semantics not yet implemented"); /* TODO */ \
                return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED; \
            } break; \
        } \
    } while (0)

    switch (uFormat) {
        case 0x00u: {   // 8b
            u8 uA = u8(knownValueA.uEmbeddedValue);
            u8 uB = u8(knownValueB.uEmbeddedValue);
            u8 uResult; COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(8);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x01u: {   // 16b
            u16 uA = u16(knownValueA.uEmbeddedValue);
            u16 uB = u16(knownValueB.uEmbeddedValue);
            u16 uResult; COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(16);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x02u: {   // 32b
            u32 uA = u32(knownValueA.uEmbeddedValue);
            u32 uB = u32(knownValueB.uEmbeddedValue);
            u32 uResult; COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(32);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x03u: {   // 64b
            u64 uA = knownValueA.uEmbeddedValue;
            u64 uB = knownValueB.uEmbeddedValue;
            u64 uResult; COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(64);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult;
        } break;
        case 0x04u: {   // 128b
            u128 uA = *reinterpret_cast<const u128*>(knownValueA.pPtrToRawData);
            u128 uB = *reinterpret_cast<const u128*>(knownValueB.pPtrToRawData);
            u128 uResult; COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(128);
            *outFlags = IRFLAG_IS_KNOWN;
            if (0 == uResult.tLegs[0] && 0 == uResult.tLegs[1])
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult.tLegs[0];
            outLegs[1] = uResult.tLegs[1];
        } break;
        case 0x05u: {   // 256b
            u256 uA = *reinterpret_cast<const u256*>(knownValueA.pPtrToRawData);
            u256 uB = *reinterpret_cast<const u256*>(knownValueB.pPtrToRawData);
            u256 uResult; COMPUTE_ADD_OR_SUB_SWITCHING_ON_SEMANTICS(256);
            *outFlags = IRFLAG_IS_KNOWN;
            if (0 == uResult.tLegs[0] && 0 == uResult.tLegs[1] && 0 == uResult.tLegs[2] && 0 == uResult.tLegs[3])
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult.tLegs[0];
            outLegs[1] = uResult.tLegs[1];
            outLegs[2] = uResult.tLegs[2];
            outLegs[3] = uResult.tLegs[3];
        } break;
        default: Assume_(false);
    }

    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
}

// this func is an impl detail for 'ir_try_solve_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz', below
local_func EIRResult ir_try_solve_edge_case_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz_but_zero_at_nyka_pos(
    u8 uFormat, AKnownValue knownNykaA, AKnownValue knownValueB, u64 uHighBitSetIfSub, EIntSemantics eSemantics,
    TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uHighBitSetIfSub == 0uLL || uHighBitSetIfSub == 0x8000'0000'0000'0000uLL);
    Assert_(uFormat >= 0x04u && uFormat <= 0x05u); // only 128b or 256b context for this function.

    const u32* tOrigNykaTable = reinterpret_cast<const u32*>(knownNykaA.pPtrToRawData);
    u32 uNykaCount = tOrigNykaTable[0u];
    // CLEANUP: we could maybe do a version of this function working without the following asserts:
    Assert_(uNykaCount == 1u);
    Assert_(tOrigNykaTable[1u] == 0u);
    u8* pPtrToDataWithoutTable = *reinterpret_cast<u8**>(knownNykaA.pPtrToRawData + align_to(8u, (uNykaCount+1u) * sizeof(u32)));
    AKnownValue mockAsNonNykaValueA;
    mockAsNonNykaValueA.pPtrToRawData = pPtrToDataWithoutTable;
    
    // CLEANUP: Note: this alloc will be wasted in case of error (when detecting overflow on const). Probably okay to leave it as-is, though.
    u32 uFormatBytes = 1u << uFormat;
    u8* pNewAllocTableAndData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, 16u + uFormatBytes, 8u);
    u32* pResultingNykaTable = reinterpret_cast<u32*>(pNewAllocTableAndData);
    pResultingNykaTable[0] = 1u; // nyka count
    pResultingNykaTable[1] = 0u; // a single nyka at the base
    u8** pTableAtOffsetToResultingRawData = reinterpret_cast<u8**>(pNewAllocTableAndData + 8u);
    u8* pResultingRawData = pNewAllocTableAndData + 16u;
    *pTableAtOffsetToResultingRawData = pResultingRawData;
    outMetaValue->knownValue.pPtrToRawData = pNewAllocTableAndData;
    u64* pResultingLegs = reinterpret_cast<u64*>(pResultingRawData);

    return ir_try_solve_add_or_sub_integral_when_fully_evaluable_nz(uFormat, mockAsNonNykaValueA, knownValueB, uHighBitSetIfSub,
        eSemantics, pTCContext, outFlags, pResultingLegs);
}

local_func EIRResult ir_try_solve_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz(u8 uFormat,
    AKnownValue knownNykaA, AKnownValue knownValueB, u64 uHighBitSetIfSub, EIntSemantics eSemantics,
    TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uHighBitSetIfSub == 0uLL || uHighBitSetIfSub == 0x8000'0000'0000'0000uLL);
    Assert_(uFormat >= 0x03u && uFormat <= 0x05u); // only 64b or more can carry nykas

    if (uFormat == 0x03u) {
        // the result of nykaA +- evaluableB can be a nyka of same base, with offset modified with +-evaluableB,
        //   provided resulting offset would fit.

        i64 iValB = i64(knownValueB.uEmbeddedValue);
        if (iValB < 0 && eSemantics != EIntSemantics::EINT_SEMANTIC_SIGNED)
            goto on_unknown; // unsigned or modulo, when high bit of B is set, cannot be solved as nyka (otherwise could)
        if (iValB > 0x0000'0001'0000'0000LL || iValB < -0x0000'0001'0000'0000LL) 
            goto on_unknown; // too large abs value of B to even bother (and ensure we'll be able to check the op in 64b without overflow)
        i32 iOffsetA; u64 uBaseA = ir_decode_nyka_value(knownNykaA.uEmbeddedValue, &iOffsetA);
        i64 iResultingOffset = uHighBitSetIfSub ? (i64(iOffsetA) - iValB) : (i64(iOffsetA) + iValB);
        if (iResultingOffset >= 0x0000'0000'0080'0000LL || iResultingOffset < -0x0000'0000'0080'0000LL)
            goto on_unknown; // result can only fit into a nyka if offset fits into 24b signed

        // but if it does, then we can present it as a perfectly valid (and known), nyka:
        *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
        outMetaValue->knownValue.uEmbeddedValue = ir_make_nyka_value(uBaseA, i32(iResultingOffset));
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;

    } else {
        const u32* pNykaTable = reinterpret_cast<const u32*>(knownNykaA.pPtrToRawData);
        u32 uNykaCount = pNykaTable[0];
        Assert_(uNykaCount);
        if (pNykaTable[uNykaCount-1u] == 0u) {
            Assert_(uNykaCount == 1u);
            // we're solving for (and from) a format which is larger than a 64b NYKA, but which contains a single nyka at offset 0.
            // => if all bits of B above those 64b are same as the "sign" bit on 64b (at position 63), then we can confidently
            //    do a very similar treatment as above.
            u64* tLegsB = reinterpret_cast<u64*>(knownValueB.pPtrToRawData);
            i64 iLowB = i64(tLegsB[0]);
            if (iLowB < 0) {
                if (tLegsB[1] != 0xFFFF'FFFF'FFFF'FFFFuLL || (uFormat == 0x05u &&
                   (tLegsB[2] != 0xFFFF'FFFF'FFFF'FFFFuLL || tLegsB[3] != 0xFFFF'FFFF'FFFF'FFFFuLL)))
                    goto on_unknown; // B is not encodable as 64b signed if any leg above the first is not full 1s, in case low leg is negative
                if (eSemantics != EIntSemantics::EINT_SEMANTIC_SIGNED)
                    goto on_unknown; // unsigned or modulo, when high bit of B is set, cannot be solved as nyka (otherwise could)
            } else if (tLegsB[1] || (uFormat == 0x05u && (tLegsB[2] || tLegsB[3]))) {
                if (tLegsB[0])
                    goto on_unknown; // B is not encodable as 64b if any leg above the fist is not full 0s, in case low leg is non-negative
                else { // however, if lower leg of B is full 0s (ie, over the position of the nyka in A)...
                    // ...then we can very well compute a result as-if A did not, in fact, carry a nyka !!
                    return ir_try_solve_edge_case_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz_but_zero_at_nyka_pos(
                            uFormat, knownNykaA, knownValueB, uHighBitSetIfSub, eSemantics, pTCContext, outFlags, outMetaValue);
                }
            }

            u8* pPtrToDataWithoutTable = *reinterpret_cast<u8**>(knownNykaA.pPtrToRawData + align_to(8u, (uNykaCount+1u) * sizeof(u32)));
            u64* tLegsA = reinterpret_cast<u64*>(pPtrToDataWithoutTable);
            i32 iOffsetA; u64 uBaseA = ir_decode_nyka_value(tLegsA[0], &iOffsetA);
            i64 iResultingOffset = uHighBitSetIfSub ? (i64(iOffsetA) - iLowB) : (i64(iOffsetA) + iLowB);
            if (iResultingOffset >= 0x0000'0000'0080'0000LL || iResultingOffset < -0x0000'0000'0080'0000LL)
                goto on_unknown; // result can only fit into a nyka if offset fits into 24b signed

            // but if it does, then we can present it as a perfectly valid (and known), nyka:
            *outFlags = IRFLAG_IS_KNOWN;
            u32 uFormatBytes = 1u << uFormat;
            u8* pNewAllocTableAndData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, 16u + uFormatBytes, 8u);
            u32* pResultingNykaTable = reinterpret_cast<u32*>(pNewAllocTableAndData);
            pResultingNykaTable[0] = 1u; // nyka count
            pResultingNykaTable[1] = 0u; // a single nyka at the base
            u8** pTableAtOffsetToResultingRawData = reinterpret_cast<u8**>(pNewAllocTableAndData + 8u);
            u8* pResultingRawData = pNewAllocTableAndData + 16u;
            *pTableAtOffsetToResultingRawData = pResultingRawData;
            u64* pResultingRawDataAsLegs = reinterpret_cast<u64*>(pResultingRawData);
            pResultingRawDataAsLegs[0] = ir_make_nyka_value(uBaseA, i32(iResultingOffset));
            // high leg(s) of A are unchanged, since all displacement from value of B was sucked up as a nyka offset change
            pResultingRawDataAsLegs[1] = tLegsA[1];
            if (uFormat == 0x05u) { // 256b : two more legs
                pResultingRawDataAsLegs[2] = tLegsA[2];
                pResultingRawDataAsLegs[3] = tLegsA[3];
            }
            outMetaValue->knownValue.pPtrToRawData = pNewAllocTableAndData;
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        } else {
            // TODO
            platform_log_error("ir_try_solve_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz() : solving with shifted and/or several nykas not yet implemented");
            return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
        }
    }

    on_unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;
}

local_func bool ir_try_find_more_basal_ir(TCContext* pTCContext, u64* ioBaseIR, i64* ioOffsetFromBase)
{
    // TODO
    platform_log_info("**** Warning : ir_try_find_more_basal_ir() : not yet implemented, returning as-if always false");
    return false;
}

local_func EIRResult ir_try_solve_add_or_sub_integral_by_nyka_diff(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
    EIntSemantics eSemantics, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(ir_is_valid_param_(infoB.uIRandMetaFlags));
    Assert_(uFormat >= 0x03u && uFormat <= 0x05u); // only 64b or more can carry nykas

    Assert_(irflag_is_known_or_nyka(infoA.uIRandMetaFlags) && irflag_is_or_has_nyka(infoA.uIRandMetaFlags));
    Assert_(irflag_is_known_or_nyka(infoB.uIRandMetaFlags) && irflag_is_or_has_nyka(infoB.uIRandMetaFlags));

    if (uFormat == 0x03u) {
        Assert_(irflag_is_known_embd(infoA.uIRandMetaFlags));
        Assert_(irflag_is_known_embd(infoB.uIRandMetaFlags));

        // the result of nykaA - nykaB can be evaluated to the difference of offsets, provided they have same base...
        // Note that we may need to punch-through nykas referring to 'deref' IRs.

        i32 iOffsetA; u64 uBaseA = ir_decode_nyka_value(infoA.metaValue.knownValue.uEmbeddedValue, &iOffsetA);
        i32 iOffsetB; u64 uBaseB = ir_decode_nyka_value(infoB.metaValue.knownValue.uEmbeddedValue, &iOffsetB);
        Assert_(!ir_is_immediate(uBaseA));
        Assert_(!ir_is_immediate(uBaseB));

        u64 uBasalBaseA = uBaseA;
        i64 iBasalOffsetA = i64(iOffsetA);
        u64 uBasalBaseB = uBaseB;
        i64 iBasalOffsetB = i64(iOffsetB);

        if (uBasalBaseA != uBasalBaseB) { // only when they differ, shall we try to punch-through derefs to find most basal of both:
            while (ir_try_find_more_basal_ir(pTCContext, &uBasalBaseA, &iBasalOffsetA)) {
                // NOOP: continue
            }
            while (ir_try_find_more_basal_ir(pTCContext, &uBasalBaseB, &iBasalOffsetB)) {
                // NOOP: continue
            }
        }

        if (uBasalBaseA == uBasalBaseB) {
            i64 iResultingOffset = iBasalOffsetA - iBasalOffsetB;
            if (iResultingOffset < 0 && eSemantics == EINT_SEMANTIC_UNSIGNED) {
                return EIRResult::EIRR_UNSIGNED_OVERFLOW;
            }
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (iResultingOffset == 0)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outMetaValue->knownValue.uEmbeddedValue = u64(iResultingOffset);
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        }

    } else {
        // TODO
        platform_log_error("ir_try_solve_add_or_sub_integral_by_nyka_diff() : solving with nyka-carriers > 64b not yet implemented");
        return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;
}

local_func EIRResult ir_try_solve_add_or_sub_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB, u64 uHighBitSetIfSub,
                                                      EIntSemantics eSemantics, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uHighBitSetIfSub == 0uLL || uHighBitSetIfSub == 0x8000'0000'0000'0000uLL);
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(ir_is_valid_param_(infoB.uIRandMetaFlags));
    Assert_(uFormat <= 0x05u);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED ||
            eSemantics == EIntSemantics::EINT_SEMANTIC_MODULO_ARITH);

    Assert_(irflag_is_known_zero(infoA.uIRandMetaFlags) == 
                (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && ir_check_full_zero_value(infoA, 1u << uFormat)));
    Assert_(irflag_is_known_zero(infoB.uIRandMetaFlags) ==
                (irflag_is_known_non_nyka(infoB.uIRandMetaFlags) && ir_check_full_zero_value(infoB, 1u << uFormat)));

    if (irflag_is_known_zero(infoB.uIRandMetaFlags)) {                                // A +- 0 == A
        Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
        IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
    }

    if (irflag_is_known_zero(infoA.uIRandMetaFlags)) {
        Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
        if (0uLL == uHighBitSetIfSub) {                                               // 0 + B == B
            IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
        }
    }

    EIRResult eResultWhyUnknown = EIRResult::EIRR_UNKNOWN;
    if (irflag_is_known_or_nyka(infoA.uIRandMetaFlags)) {
        if (!irflag_is_or_has_nyka(infoA.uIRandMetaFlags)) {
            if (irflag_is_known_or_nyka(infoB.uIRandMetaFlags)) {

                if (!irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {    // evaluableA +- evaluableB   => fully evaluable result
                    u64* pLegs;
                    if (uFormat <= 0x03u) {
                        Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                        Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                        pLegs = &(outMetaValue->knownValue.uEmbeddedValue);
                    } else {
                        Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                        Assert_(0 == (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                        // CLEANUP: Note: this alloc will be wasted in case of error (when detecting overflow on const). Probably okay to leave it as-is, though.
                        pLegs = (u64*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 1u << uFormat, 8u);
                        outMetaValue->knownValue.pPtrToRawData = (u8*)pLegs;
                    }
                    return ir_try_solve_add_or_sub_integral_when_fully_evaluable_nz(uFormat,
                        infoA.metaValue.knownValue, infoB.metaValue.knownValue,
                        uHighBitSetIfSub, eSemantics, pTCContext, outFlags, pLegs);

                } else if (0uLL == uHighBitSetIfSub) {  // evaluableA + nykaB         => can work out some resulting NYKAs
                    return ir_try_solve_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz(uFormat,
                        infoB.metaValue.knownValue, infoA.metaValue.knownValue, // reverted operands
                        0uLL, eSemantics, pTCContext, outFlags, outMetaValue);

                } else {
                    eResultWhyUnknown = EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;    // 'evaluableA - nykaB' makes const expr turn to runtime
                }
            } 
        } else if (irflag_is_known_or_nyka(infoB.uIRandMetaFlags)) {

            if (!irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {        // nykaA +- evaluableB        => can work out some resulting NYKAs
                return ir_try_solve_add_or_sub_integral_when_first_is_nyka_second_evaluable_nz(uFormat,
                    infoA.metaValue.knownValue, infoB.metaValue.knownValue,
                    uHighBitSetIfSub, eSemantics, pTCContext, outFlags, outMetaValue);

            } else if (uHighBitSetIfSub) {              // nykaA - nykaB              => may find a result if same base
                return ir_try_solve_add_or_sub_integral_by_nyka_diff(uFormat, infoA, infoB, eSemantics, pTCContext, outFlags, outMetaValue);

            } else {
                eResultWhyUnknown = EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;        // 'nykaA + nykaB' makes const expr turn to runtime
            }
        }
    }

    // otherwise, result is necessarily unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eResultWhyUnknown; // either EIRResult::EIRR_UNKNOWN or EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA
}

local_func EIRResult ir_try_solve_mul_integral_when_fully_evaluable_non_zero_non_one(u8 uFormat,
    AKnownValue knownValueA, AKnownValue knownValueB, EIntSemantics eSemantics, u32* outFlags, u64* outLegs)
{
    Assert_(uFormat <= 0x05u);

    #define COMPUTE_MUL_SWITCHING_ON_SEMANTICS(uBits) do { \
        switch (eSemantics) { \
            case EIntSemantics::EINT_SEMANTIC_MODULO_ARITH: { \
                uResult = mulu ## uBits(uA, uB); \
            } break; \
            case EIntSemantics::EINT_SEMANTIC_MODULO_SIGNED: { \
                uResult = muli ## uBits(uA, uB); \
            } break; \
            case EIntSemantics::EINT_SEMANTIC_SIGNED: { \
                u8 err; \
                uResult = muli ## uBits ## _ovf(uA, uB, &err); \
                if (err) \
                    return EIRResult::EIRR_SIGNED_OVERFLOW; \
            } break; \
            case EIntSemantics::EINT_SEMANTIC_UNSIGNED: { \
                u8 err; \
                uResult = mulu ## uBits ## _ovf(uA, uB, &err); \
                if (err) \
                    return EIRResult::EIRR_UNSIGNED_OVERFLOW; \
            } break; \
            default: { \
                platform_log_error("ir_try_solve_mul_integral() : mixed semantics not yet implemented"); /* TODO */ \
                return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED; \
            } break; \
        } \
    } while (0)

    switch (uFormat) {
        case 0x00u: {   // 8b
            u8 uA = u8(knownValueA.uEmbeddedValue);
            u8 uB = u8(knownValueB.uEmbeddedValue);
            u8 uResult; COMPUTE_MUL_SWITCHING_ON_SEMANTICS(8);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x01u: {   // 16b
            u16 uA = u16(knownValueA.uEmbeddedValue);
            u16 uB = u16(knownValueB.uEmbeddedValue);
            u16 uResult; COMPUTE_MUL_SWITCHING_ON_SEMANTICS(16);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x02u: {   // 32b
            u32 uA = u32(knownValueA.uEmbeddedValue);
            u32 uB = u32(knownValueB.uEmbeddedValue);
            u32 uResult; COMPUTE_MUL_SWITCHING_ON_SEMANTICS(32);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x03u: {   // 64b
            u64 uA = knownValueA.uEmbeddedValue;
            u64 uB = knownValueB.uEmbeddedValue;
            u64 uResult; COMPUTE_MUL_SWITCHING_ON_SEMANTICS(64);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult;
        } break;
        case 0x04u: {   // 128b
            u128 uA = *reinterpret_cast<const u128*>(knownValueA.pPtrToRawData);
            u128 uB = *reinterpret_cast<const u128*>(knownValueB.pPtrToRawData);
            u128 uResult; COMPUTE_MUL_SWITCHING_ON_SEMANTICS(128);
            *outFlags = IRFLAG_IS_KNOWN;
            if (0 == uResult.tLegs[0] && 0 == uResult.tLegs[1])
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult.tLegs[0];
            outLegs[1] = uResult.tLegs[1];
        } break;
        case 0x05u: {   // 256b
            u256 uA = *reinterpret_cast<const u256*>(knownValueA.pPtrToRawData);
            u256 uB = *reinterpret_cast<const u256*>(knownValueB.pPtrToRawData);
            u256 uResult; COMPUTE_MUL_SWITCHING_ON_SEMANTICS(256);
            *outFlags = IRFLAG_IS_KNOWN;
            if (0 == uResult.tLegs[0] && 0 == uResult.tLegs[1] && 0 == uResult.tLegs[2] && 0 == uResult.tLegs[3])
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult.tLegs[0];
            outLegs[1] = uResult.tLegs[1];
            outLegs[2] = uResult.tLegs[2];
            outLegs[3] = uResult.tLegs[3];
        } break;
        default: Assume_(false);
    }

    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
}

local_func EIRResult ir_try_solve_mul_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
                                               EIntSemantics eSemantics, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(ir_is_valid_param_(infoB.uIRandMetaFlags));
    Assert_(uFormat <= 0x05u);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED ||
            eSemantics == EIntSemantics::EINT_SEMANTIC_MODULO_ARITH || eSemantics == EIntSemantics::EINT_SEMANTIC_MODULO_SIGNED);

    Assert_(irflag_is_known_zero(infoA.uIRandMetaFlags) == 
                (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && ir_check_full_zero_value(infoA, 1u << uFormat)));
    Assert_(irflag_is_known_zero(infoB.uIRandMetaFlags) ==
                (irflag_is_known_non_nyka(infoB.uIRandMetaFlags) && ir_check_full_zero_value(infoB, 1u << uFormat)));

    if (irflag_is_known_zero(infoA.uIRandMetaFlags)) {                                // 0 * B == 0 == A
        Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
        IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
    }

    if (irflag_is_known_zero(infoB.uIRandMetaFlags)) {                                // A * 0 == 0 == B
        Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
        IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
    }

    if (ir_check_if_integral_is_one(uFormat, infoA)) {                                // 1 * B == B
        Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
        IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
    }

    if (ir_check_if_integral_is_one(uFormat, infoB)) {                                // A * 1 == A
        Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
        IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
    }

    EIRResult eResultWhyUnknown = EIRResult::EIRR_UNKNOWN;
    if (irflag_is_known_or_nyka(infoA.uIRandMetaFlags) && irflag_is_known_or_nyka(infoB.uIRandMetaFlags)) {
        if (!irflag_is_or_has_nyka(infoA.uIRandMetaFlags) && !irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {
            u64* pLegs;
            if (uFormat <= 0x03u) {
                Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                pLegs = &(outMetaValue->knownValue.uEmbeddedValue);
            } else {
                Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                Assert_(0 == (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                // CLEANUP: Note: this alloc will be wasted in case of error (when detecting overflow on const). Probably okay to leave it as-is, though.
                pLegs = (u64*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 1u << uFormat, 8u);
                outMetaValue->knownValue.pPtrToRawData = (u8*)pLegs;
            }
            return ir_try_solve_mul_integral_when_fully_evaluable_non_zero_non_one(uFormat,
                infoA.metaValue.knownValue, infoB.metaValue.knownValue, eSemantics, outFlags, pLegs);
        } else {
            eResultWhyUnknown = EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;
        }
    }

    // otherwise, result is necessarily unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eResultWhyUnknown; // either EIRResult::EIRR_UNKNOWN or EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA
}

local_func EIRResult ir_try_solve_quo_or_rem_or_mod_integral_when_fully_evaluable_non_zero_non_one(u8 uOp, u8 uFormat,
    AKnownValue knownValueA, AKnownValue knownValueB, EIntSemantics eSemantics, u32* outFlags, u64* outLegs)
{
    Assert_(uOp == ETOK_INT_QUOTIENT || uOp == ETOK_INT_REMAINDER || uOp == ETOK_MOD);
    Assert_(uFormat <= 0x05u);

    #define COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(uBits) do { \
        switch (eSemantics) { \
            case EIntSemantics::EINT_SEMANTIC_SIGNED: { \
                uResult = (uOp == ETOK_INT_QUOTIENT) ? divi ## uBits (uA, uB) : \
                                                       ((uOp == ETOK_INT_REMAINDER) ? remi ## uBits (uA, uB) : mod ## uBits (uA, uB)); \
            } break; \
            case EIntSemantics::EINT_SEMANTIC_UNSIGNED: { \
                uResult = (uOp == ETOK_INT_QUOTIENT) ? divu ## uBits (uA, uB) : remu ## uBits (uA, uB); /* mod or rem are same unsigned */ \
            } break; \
            default: { \
                Assume_(false); \
                return EIRResult::EIRR_ERROR; \
            } break; \
        } \
    } while (0)

    switch (uFormat) {
        case 0x00u: {   // 8b
            u8 uA = u8(knownValueA.uEmbeddedValue);
            u8 uB = u8(knownValueB.uEmbeddedValue);
            u8 uResult; COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(8);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x01u: {   // 16b
            u16 uA = u16(knownValueA.uEmbeddedValue);
            u16 uB = u16(knownValueB.uEmbeddedValue);
            u16 uResult; COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(16);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x02u: {   // 32b
            u32 uA = u32(knownValueA.uEmbeddedValue);
            u32 uB = u32(knownValueB.uEmbeddedValue);
            u32 uResult; COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(32);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = u64(uResult);
        } break;
        case 0x03u: {   // 64b
            u64 uA = knownValueA.uEmbeddedValue;
            u64 uB = knownValueB.uEmbeddedValue;
            u64 uResult; COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(64);
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            if (0 == uResult)
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult;
        } break;
        case 0x04u: {   // 128b
            u128 uA = *reinterpret_cast<const u128*>(knownValueA.pPtrToRawData);
            u128 uB = *reinterpret_cast<const u128*>(knownValueB.pPtrToRawData);
            u128 uResult; COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(128);
            *outFlags = IRFLAG_IS_KNOWN;
            if (0 == uResult.tLegs[0] && 0 == uResult.tLegs[1])
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult.tLegs[0];
            outLegs[1] = uResult.tLegs[1];
        } break;
        case 0x05u: {   // 256b
            u256 uA = *reinterpret_cast<const u256*>(knownValueA.pPtrToRawData);
            u256 uB = *reinterpret_cast<const u256*>(knownValueB.pPtrToRawData);
            u256 uResult; COMPUTE_QUO_REM_MOD_SWITCHING_ON_SEMANTICS(256);
            *outFlags = IRFLAG_IS_KNOWN;
            if (0 == uResult.tLegs[0] && 0 == uResult.tLegs[1] && 0 == uResult.tLegs[2] && 0 == uResult.tLegs[3])
                *outFlags |= IRFLAG_IS_KNOWN_ZERO;
            outLegs[0] = uResult.tLegs[0];
            outLegs[1] = uResult.tLegs[1];
            outLegs[2] = uResult.tLegs[2];
            outLegs[3] = uResult.tLegs[3];
        } break;
        default: Assume_(false);
    }

    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
}

constexpr u64 tZero1024b[16u] = { 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL, 0uLL }; 
local_func EIRResult ir_solver_return_meta_for_ensured_integral_zero(u8 uFormat, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uFormat <= 0x07u);
    if (uFormat <= 0x03u) {
        *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
        outMetaValue->knownValue.uEmbeddedValue = 0uLL;
    } else {
        *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_ZERO;
        outMetaValue->knownValue.pPtrToRawData = const_cast<u8*>(reinterpret_cast<const u8*>(tZero1024b));
    }
    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
}

// integral quotient, remainder, or modulus
local_func EIRResult ir_try_solve_quo_or_rem_or_mod_integral(u8 uOp, u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
    EIntSemantics eSemantics, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uOp == ETOK_INT_QUOTIENT || uOp == ETOK_INT_REMAINDER || uOp == ETOK_MOD);
    Assert_(uFormat <= 0x05u);
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(ir_is_valid_param_(infoB.uIRandMetaFlags));

    Assert_(irflag_is_known_zero(infoA.uIRandMetaFlags) == 
                (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && ir_check_full_zero_value(infoA, 1u << uFormat)));
    Assert_(irflag_is_known_zero(infoB.uIRandMetaFlags) ==
                (irflag_is_known_non_nyka(infoB.uIRandMetaFlags) && ir_check_full_zero_value(infoB, 1u << uFormat)));

    if (irflag_is_known_zero(infoB.uIRandMetaFlags)) {
        return EIRResult::EIRR_DIVISION_BY_ZERO;
    }

    // CLEANUP: Comptime 0 / comptime 0 will raise an error due to the check above, yet comptime 0 / runtime 0 may be shortcut by
    //   the following without firing any error at all. It it ok, though ?
    if (irflag_is_known_zero(infoA.uIRandMetaFlags)) {                      // 0 /% B == 0 %% B == 0 % B == 0 == A
        Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
        IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
    }

    if (ir_check_if_integral_is_one(uFormat, infoB)) {
        Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
        if (uOp == ETOK_INT_QUOTIENT) {                                     // A /% 1 == A
            IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
        } else {                                                            // A %% 1 == 0, A % 1 == 0   
            return ir_solver_return_meta_for_ensured_integral_zero(uFormat, outFlags, outMetaValue);  
        }
    }

    if (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED && uOp != ETOK_INT_QUOTIENT) {
        if (ir_check_if_integral_is_all_bits_set(uFormat, infoB)) {         // A %% -1 == 0, A % -1 == 0 
            Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
            return ir_solver_return_meta_for_ensured_integral_zero(uFormat, outFlags, outMetaValue);  
        }
    }

    // TODO: may return ensured 0 on A %% -1 or A % 1, ie. signed semantics and B == all bits 1s

    EIRResult eResultWhyUnknown = irflag_is_known_or_nyka(infoB.uIRandMetaFlags) ? EIRResult::EIRR_ENSURED_VALID_UNKNOWN : EIRResult::EIRR_UNKNOWN;
    if (irflag_is_known_or_nyka(infoA.uIRandMetaFlags) && irflag_is_known_or_nyka(infoB.uIRandMetaFlags)) {
        if (!irflag_is_or_has_nyka(infoA.uIRandMetaFlags) && !irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {
            u64* pLegs;
            if (uFormat <= 0x03u) {
                Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                pLegs = &(outMetaValue->knownValue.uEmbeddedValue);
            } else {
                Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                Assert_(0 == (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                pLegs = (u64*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 1u << uFormat, 8u);
                outMetaValue->knownValue.pPtrToRawData = (u8*)pLegs;
            }
            return ir_try_solve_quo_or_rem_or_mod_integral_when_fully_evaluable_non_zero_non_one(uOp, uFormat,
                infoA.metaValue.knownValue, infoB.metaValue.knownValue, eSemantics, outFlags, pLegs);
        } else {
            eResultWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
        }
    }

    // otherwise, result is necessarily unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eResultWhyUnknown; // either EIRResult::EIRR_UNKNOWN or EIRR_ENSURED_VALID_UNKNOWN or EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA
}

local_func EIRResult ir_try_solve_bitwise_not_when_fully_evaluable(u8 uFormat, AKnownValue knownValue, u32* outFlags, u64* outLegs)
{
    Assert_(uFormat <= 0x07u);

    if (uFormat <= 0x03u) { // up to 64b, source (and result) should be embedded, and ensured zeroed above their actual range if less than 64b
        
        u64 uResultOn64b;
        switch (uFormat) {
            case 0x00u: uResultOn64b = u64(u8(~u8(knownValue.uEmbeddedValue))); break;
            case 0x01u: uResultOn64b = u64(u16(~u16(knownValue.uEmbeddedValue))); break;
            case 0x02u: uResultOn64b = u64(u32(~u32(knownValue.uEmbeddedValue))); break;
            case 0x03u: uResultOn64b = ~knownValue.uEmbeddedValue; break;
            default: Assume_(false); uResultOn64b = 0u;
        }
        if (0 == uResultOn64b)
            *outFlags |= IRFLAG_IS_KNOWN_ZERO;
        outLegs[0] = uResultOn64b;
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;

    } else {

        const u64* pSrcLegs = reinterpret_cast<u64*>(knownValue.pPtrToRawData);
        u32 uLegCount = (1u << (uFormat-3u));    // eg for 0x05u, == 1u << 2 == 4 == count of u64 legs in an u256.
        u64 uOringAllCheckingNZ = 0uLL;
        for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
            u64 uThisLeg = ~pSrcLegs[uLeg];
            uOringAllCheckingNZ |= uThisLeg;
            outLegs[uLeg] = uThisLeg;
        }
        *outFlags = IRFLAG_IS_KNOWN;
        if (0 == uOringAllCheckingNZ)
            *outFlags |= IRFLAG_IS_KNOWN_ZERO;
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;
    }
}

local_func EIRResult ir_try_solve_bitwise_binary_op_when_fully_evaluable_nz(u8 uOp, u8 uFormat,
    AKnownValue knownValueA, AKnownValue knownValueB, u32* outFlags, u64* outLegs)
{
    Assert_(uOp == ETOK_BIT_AND || uOp == ETOK_BIT_OR || uOp == ETOK_BIT_XOR);
    Assert_(uFormat <= 0x07u);

    if (uFormat <= 0x03u) { // up to 64b, source (and result) should be embedded, and ensured zeroed above their actual range if less than 64b
        // => We can perform the op directly onto the 64b embedding, whatever the format.

        // (1u << (uFormat+3u)) is number of bits in format. The following asserts checks that high bits above format are zeroed as they should
        Assert_(0uLL == ((0xFFFF'FFFF'FFFF'FFFFuLL << (1u << (uFormat+3u))) & knownValueA.uEmbeddedValue));
        Assert_(0uLL == ((0xFFFF'FFFF'FFFF'FFFFuLL << (1u << (uFormat+3u))) & knownValueB.uEmbeddedValue));
        // => the following should work for 8b, 16b, 32b, 64b ; provided the above assert holds.
        u64 uResultOn64b;
        if (uOp == ETOK_BIT_AND) {
            uResultOn64b = knownValueA.uEmbeddedValue & knownValueB.uEmbeddedValue;
        } else if (uOp == ETOK_BIT_OR) {
            uResultOn64b = knownValueA.uEmbeddedValue | knownValueB.uEmbeddedValue;
        } else { Assert_(uOp == ETOK_BIT_XOR);
            uResultOn64b = knownValueA.uEmbeddedValue ^ knownValueB.uEmbeddedValue;
        }
        *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
        if (0 == uResultOn64b)
            *outFlags |= IRFLAG_IS_KNOWN_ZERO;
        outLegs[0] = uResultOn64b;
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;
    
    } else {

        const u64* pLegsA = reinterpret_cast<u64*>(knownValueA.pPtrToRawData);
        const u64* pLegsB = reinterpret_cast<u64*>(knownValueA.pPtrToRawData);
        u32 uLegCount = (1u << (uFormat-3u));    // eg for 0x05u, == 1u << 2 == 4 == count of u64 legs in an u256.
        u64 uOringAllCheckingNZ = 0uLL;
        if (uOp == ETOK_BIT_AND) {
            for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
                u64 uThisLeg = pLegsA[uLeg] & pLegsB[uLeg];
                uOringAllCheckingNZ |= uThisLeg;
                outLegs[uLeg] = uThisLeg;
            }
        } else if (uOp == ETOK_BIT_OR) {
            for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
                u64 uThisLeg = pLegsA[uLeg] | pLegsB[uLeg];
                uOringAllCheckingNZ |= uThisLeg;
                outLegs[uLeg] = uThisLeg;
            }
        } else { Assert_(uOp == ETOK_BIT_XOR);
            for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
                u64 uThisLeg = pLegsA[uLeg] ^ pLegsB[uLeg];
                uOringAllCheckingNZ |= uThisLeg;
                outLegs[uLeg] = uThisLeg;
            }
        }
        *outFlags = IRFLAG_IS_KNOWN;
        if (0 == uOringAllCheckingNZ)
            *outFlags |= IRFLAG_IS_KNOWN_ZERO;
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;
    }
}

// bitwise and, or, xor, not (expecting infoA only).
local_func EIRResult ir_try_solve_bitwise_op(u8 uOp, u8 uFormat, const IRInfo& infoA, const IRInfo& infoB,
    TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uOp == ETOK_BIT_AND || uOp == ETOK_BIT_OR || uOp == ETOK_BIT_XOR || uOp == ETOK_BIT_NOT);
    Assert_(uFormat <= 0x07u);
    Assert_(ir_is_valid_param_(infoA.uIRandMetaFlags));
    Assert_(uOp == ETOK_BIT_NOT || ir_is_valid_param_(infoB.uIRandMetaFlags));

    Assert_(irflag_is_known_zero(infoA.uIRandMetaFlags) == 
                (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && ir_check_full_zero_value(infoA, 1u << uFormat)));
    Assert_(uOp == ETOK_BIT_NOT || irflag_is_known_zero(infoB.uIRandMetaFlags) ==
                (irflag_is_known_non_nyka(infoB.uIRandMetaFlags) && ir_check_full_zero_value(infoB, 1u << uFormat)));

    if (uOp != ETOK_BIT_NOT) {

        // Shortcuts for full zeroes :

        if (irflag_is_known_zero(infoA.uIRandMetaFlags)) {
            Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
            if (uOp == ETOK_BIT_AND) {                                              // 0 & B == 0 == A
                IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
            } else {                                                                // 0 | B == 0 ^ B == B
                IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
            }
        }

        if (irflag_is_known_zero(infoB.uIRandMetaFlags)) {
            Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
            if (uOp == ETOK_BIT_AND) {                                              // A & 0 == 0 == B
                IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
            } else {                                                                // A | 0 == A ^ 0 == A
                IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
            }
        }

        if (uOp != ETOK_BIT_XOR) {

            // Shortcuts for full ones :

            if (ir_check_if_integral_is_all_bits_set(uFormat, infoA)) {
                Assert_(irflag_is_known_non_nyka(infoA.uIRandMetaFlags));
                if (uOp == ETOK_BIT_AND) {                                          // 0xF...F & B == B
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
                } else { Assert_(uOp == ETOK_BIT_OR);                               // 0xF...F | B == 0xF...F == A
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
                }
            }

            if (ir_check_if_integral_is_all_bits_set(uFormat, infoB)) {
                Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
                if (uOp == ETOK_BIT_AND) {                                          // A & 0xF...F == A
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
                } else { Assert_(uOp == ETOK_BIT_OR);                               // A | 0xF...F == 0xF...F == B
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
                }
            }
        }
    }

    // TODO: computability in case of nyka for partial ranges of either full 0s or full 1s overlapping the nyka footprints ?

    EIRResult eResultWhyUnknown = EIRResult::EIRR_ENSURED_VALID_KNOWN;
    if (irflag_is_known_or_nyka(infoA.uIRandMetaFlags) && (uOp == ETOK_BIT_NOT || irflag_is_known_or_nyka(infoB.uIRandMetaFlags))) {
        if (!irflag_is_or_has_nyka(infoA.uIRandMetaFlags) && (uOp == ETOK_BIT_NOT || !irflag_is_or_has_nyka(infoB.uIRandMetaFlags))) {
            if (uFormat <= 0x03u) {
                Assert_(irflag_is_known_embd(infoA.uIRandMetaFlags));
                Assert_(uOp == ETOK_BIT_NOT || irflag_is_known_embd(infoB.uIRandMetaFlags));
                u64* pLegs;
                if (uFormat <= 0x03u) {
                    Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                    Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                    pLegs = &(outMetaValue->knownValue.uEmbeddedValue);
                } else {
                    Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                    Assert_(0 == (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
                    pLegs = (u64*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, 1u << uFormat, 8u);
                    outMetaValue->knownValue.pPtrToRawData = (u8*)pLegs;
                }
                if (uOp == ETOK_BIT_NOT) {
                    return ir_try_solve_bitwise_not_when_fully_evaluable(uFormat,
                        infoA.metaValue.knownValue, outFlags, pLegs);
                } else {
                    return ir_try_solve_bitwise_binary_op_when_fully_evaluable_nz(uOp, uFormat,
                        infoA.metaValue.knownValue, infoB.metaValue.knownValue, outFlags, pLegs);
                }
            }
        } else {
            eResultWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
        }
    }

    // otherwise, result is necessarily unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eResultWhyUnknown; // either EIRR_ENSURED_VALID_KNOWN or EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA
}

// AND, OR, NOT (required format 0x00u)
local_func EIRResult ir_try_solve_boolean_op(u8 uOp, const IRInfo& infoA, const IRInfo& infoB, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uOp == ETOK_BOOL_NOT || uOp == ETOK_BOOL_AND || uOp == ETOK_BOOL_OR);
    Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_HAS_NYKA));
    Assert_(uOp == ETOK_BOOL_NOT || 0 == (infoB.uIRandMetaFlags & IRFLAG_HAS_NYKA));

    switch (uOp) {
        case ETOK_BOOL_NOT:
            if (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
                Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_HAS_NYKA));
                u64 uKnownBoolA = infoA.metaValue.knownValue.uEmbeddedValue;
                Assert_(uKnownBoolA <= 1uLL);
                u32 uKnownBoolResult = u32(uKnownBoolA) ^ 1u;
                *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                if (uKnownBoolResult == 0u)
                    *outFlags |= IRFLAG_IS_KNOWN_ZERO;
                outMetaValue->knownValue.uEmbeddedValue = u64(uKnownBoolResult);
                return EIRResult::EIRR_ENSURED_VALID_KNOWN;
            }
            break;

        case ETOK_BOOL_AND:
            if (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
                Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_HAS_NYKA));
                u64 uKnownBoolA = infoA.metaValue.knownValue.uEmbeddedValue;
                Assert_(uKnownBoolA <= 1uLL);
                if (u32(uKnownBoolA) == 0u) { // A false => A and B == false == A
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
                } else {                      // A true  => A and B == B
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
                }
            }
            if (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
                Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(0 == (infoB.uIRandMetaFlags & IRFLAG_HAS_NYKA));
                u64 uKnownBoolB = infoA.metaValue.knownValue.uEmbeddedValue;
                Assert_(uKnownBoolB <= 1uLL);
                if (u32(uKnownBoolB) == 0u) {  // B false => A and B == false == B
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
                } else {                       // B true => A and B == A
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
                }
            }
            break;

        case ETOK_BOOL_OR:
            if (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
                Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(0 == (infoA.uIRandMetaFlags & IRFLAG_HAS_NYKA));
                u64 uKnownBoolA = infoA.metaValue.knownValue.uEmbeddedValue;
                Assert_(uKnownBoolA <= 1uLL);
                if (u32(uKnownBoolA) == 1u) { // A true => A or B == true == A
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
                } else {                      // A false  => A or B == B
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
                }
            }
            if (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN) {
                Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                Assert_(0 == (infoB.uIRandMetaFlags & IRFLAG_HAS_NYKA));
                u64 uKnownBoolB = infoA.metaValue.knownValue.uEmbeddedValue;
                Assert_(uKnownBoolB <= 1uLL);
                if (u32(uKnownBoolB) == 1u) {  // B true => A or B == true == B
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(B);
                } else {                       // B false => A or B == A
                    IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
                }
            }
            break;
        default:
            Assume_(false);
    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
}

local_func EIRResult ir_try_solve_left_shift(u8 uFormat, const IRInfo& infoA, const IRInfo& infoShift,
    TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(irflag_is_known_zero(infoA.uIRandMetaFlags) == 
                (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && ir_check_full_zero_value(infoA, 1u << uFormat)));
    Assert_(!irflag_is_or_has_nyka(infoShift.uIRandMetaFlags));
    
    if (irflag_is_known_zero(infoA.uIRandMetaFlags) || irflag_is_known_zero(infoShift.uIRandMetaFlags)) {      // 0 << n == 0 == A ; A << 0 == A
        IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
    }

    if (irflag_is_known_or_nyka(infoShift.uIRandMetaFlags)) {
        Assert_(!irflag_is_or_has_nyka(infoShift.uIRandMetaFlags)); // Nykas can't be 32b
        Assert_(irflag_is_known_embd(infoShift.uIRandMetaFlags));
        u32 uShift = u32(infoShift.metaValue.knownValue.uEmbeddedValue);
        if (irflag_is_known_or_nyka(infoA.uIRandMetaFlags)) {
            if (!irflag_is_or_has_nyka(infoA.uIRandMetaFlags)) {
                if (uFormat <= 0x03u) {
                    Assert_(irflag_is_known_embd(infoA.uIRandMetaFlags));
                    u64 uValue64 = infoA.metaValue.knownValue.uEmbeddedValue;
                    u64 uShifted = uValue64 << uShift;
                    switch (uFormat) {
                        case 0x00u: { // 8b
                            outMetaValue->knownValue.uEmbeddedValue = u64(u8(uShifted));
                        } break;
                        case 0x01u: { // 16b
                            outMetaValue->knownValue.uEmbeddedValue = u64(u16(uShifted));
                        } break;
                        case 0x02u: { // 32b
                            outMetaValue->knownValue.uEmbeddedValue = u64(u32(uShifted));
                        } break;
                        case 0x03u: { // 64b
                            outMetaValue->knownValue.uEmbeddedValue = uShifted;
                        } break;
                    }
                    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                    if (0uLL == outMetaValue->knownValue.uEmbeddedValue)
                        *outFlags |= IRFLAG_IS_KNOWN_ZERO;
                    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                } else {
                    // TODO
                    platform_log_error("ir_try_solve_left_shift() : >64b not yet implemented");
                    *outFlags = 0;
                    outMetaValue->_payload = 0uLL;
                    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
                }
            } else {
                // TODO: some known byte-aligned shifts can still be solved if no truncation occurs ?
                *outFlags = 0;
                outMetaValue->_payload = 0uLL;
                return EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;
            }
        } 
    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
}

local_func EIRResult ir_try_solve_right_shift(u8 uFormat, const IRInfo& infoA, EIntSemantics eSemantics, const IRInfo& infoShift,
    TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(irflag_is_known_zero(infoA.uIRandMetaFlags) == 
                (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && ir_check_full_zero_value(infoA, 1u << uFormat)));
    Assert_(!irflag_is_or_has_nyka(infoShift.uIRandMetaFlags));

    if (irflag_is_known_zero(infoA.uIRandMetaFlags) || irflag_is_known_zero(infoShift.uIRandMetaFlags)) {      // 0 << n == 0 == A ; A >> 0 == A
        IR_SOLVER_RETURN_ENSURED_SAME_AS(A);
    }

    if (irflag_is_known_or_nyka(infoShift.uIRandMetaFlags)) {
        Assert_(!irflag_is_or_has_nyka(infoShift.uIRandMetaFlags)); // Nykas can't be 32b
        Assert_(irflag_is_known_embd(infoShift.uIRandMetaFlags));
        u32 uShift = u32(infoShift.metaValue.knownValue.uEmbeddedValue);
        if (irflag_is_known_or_nyka(infoA.uIRandMetaFlags)) {
            if (!irflag_is_or_has_nyka(infoA.uIRandMetaFlags)) {
                if (uFormat <= 0x03u) {
                    Assert_(irflag_is_known_embd(infoA.uIRandMetaFlags));
                    u64 uValue64 = infoA.metaValue.knownValue.uEmbeddedValue;
                    if (eSemantics == EINT_SEMANTIC_UNSIGNED) {
                        outMetaValue->knownValue.uEmbeddedValue = uValue64 >> uShift;
                    } else {
                        switch (uFormat) {
                            case 0x00u: { // 8b
                                i8 iShifted = i8(uValue64) >> uShift;
                                outMetaValue->knownValue.uEmbeddedValue = u64(u8(iShifted));
                            } break;
                            case 0x01u: { // 16b
                                i16 iShifted = i16(uValue64) >> uShift;
                                outMetaValue->knownValue.uEmbeddedValue = u64(u16(iShifted));
                            } break;
                            case 0x02u: { // 32b
                                i32 iShifted = i32(uValue64) >> uShift;
                                outMetaValue->knownValue.uEmbeddedValue = u64(u32(iShifted));
                            } break;
                            case 0x03u: { // 64b
                                i64 iShifted = i64(uValue64) >> uShift;
                                outMetaValue->knownValue.uEmbeddedValue = u64(iShifted);
                            } break;
                        }
                    }
                    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                    if (0uLL == outMetaValue->knownValue.uEmbeddedValue)
                        *outFlags |= IRFLAG_IS_KNOWN_ZERO;
                    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                } else {
                    // TODO
                    platform_log_error("ir_try_solve_right_shift() : >64b not yet implemented");
                    *outFlags = 0;
                    outMetaValue->_payload = 0uLL;
                    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
                }
            } else {
                // TODO: some known byte-aligned shifts can still be solved if no truncation occurs ?
                *outFlags = 0;
                outMetaValue->_payload = 0uLL;
                return EIRResult::EIRR_UNKNOWN_CAUSED_BY_NYKA;
            }
        } 
    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
}

local_func EIRResult ir_try_solve_eq_cmp_integral_two_nykas(u8 uFormat, AKnownValue nykaA, AKnownValue nykaB,
    u32 uIsNeqFlag, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsNeqFlag == 0u || uIsNeqFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uFormat <= 0x07u);
    // TODO
    platform_log_error("ir_try_solve_eq_cmp_integral_two_nykas() : not yet implemented");
    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
}

local_func EIRResult ir_try_solve_eq_cmp_integral_two_solvables(u8 uFormat, AKnownValue knownValueA, AKnownValue knownValueB,
    u32 uIsNeqFlag, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsNeqFlag == 0u || uIsNeqFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uFormat <= 0x07u);

    if (uFormat <= 0x03u) {
        // 8u << uFormat is bitcount of source format.
        // => The following asserts ensures all high bits above src footprint were all zeroed, as they should.
        Assert_( uFormat == 0x03u || 0uLL == ( knownValueA.uEmbeddedValue & (0xFFFF'FFFF'FFFF'FFFFuLL << (8u << uFormat)) ) );
        Assert_( uFormat == 0x03u || 0uLL == ( knownValueB.uEmbeddedValue & (0xFFFF'FFFF'FFFF'FFFFuLL << (8u << uFormat)) ) );
        // The following test should work for 8b, 16b, 32b, 64b provided the above asserts hold
        if ((knownValueA.uEmbeddedValue == knownValueB.uEmbeddedValue) == (uIsNeqFlag == 0u)) {
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            outMetaValue->knownValue.uEmbeddedValue = 1uLL;
        } else {
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
            outMetaValue->knownValue.uEmbeddedValue = 0uLL;
        }
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;

    } else {
        const u64* pLegsA = reinterpret_cast<u64*>(knownValueA.pPtrToRawData);
        const u64* pLegsB = reinterpret_cast<u64*>(knownValueB.pPtrToRawData);
        u32 uLegCount = (1u << (uFormat-3u));    // eg for 0x05u, == 1u << 2 == 4 == count of u64 legs in an u256.
        if (uIsNeqFlag == 0u) {
            for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
                if (pLegsA[uLeg] != pLegsB[uLeg])
                    goto whenfalse;
            }
            goto whentrue;
        } else { // testing for *not* equal
            for (u32 uLeg = 0u; uLeg < uLegCount; uLeg++) {
                if (pLegsA[uLeg] != pLegsB[uLeg])
                    goto whentrue;
            }
            goto whenfalse;
        }

        { whentrue:
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            outMetaValue->knownValue.uEmbeddedValue = 1uLL;
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        }

        { whenfalse:
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
            outMetaValue->knownValue.uEmbeddedValue = 0uLL;
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        }
    }
}

// EQ, NEQ
local_func EIRResult ir_try_solve_eq_cmp_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB, u32 uIsNeqFlag, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsNeqFlag == 0u || uIsNeqFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uFormat <= 0x07u); // otherwise not integral

    // 1) should a nyka be necessarily non-null ? maybe, iff offset ensured above minus 64K on windows ? iff offset ensured >= 0 otherwise ?
    //          also, are we really 100% sure than address wrapping will never make a nyka with semi-large positive offset solve to 0 ??
    //          should we simply *decide* that we won't let that happen, or simply always suppose it won't ?
    // 2) should two nykas of distinct bases be necessarily unequal ?
    //          yes, provided not both are const decls (otherwise const value merging could be done on backend).
    //          => should ensure, if checking for such shortcut, that no local value we're taking the address of are const-evalued away.
    EIRResult eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
    
    if (irflag_is_or_has_nyka(infoA.uIRandMetaFlags)) {
        Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        if (irflag_is_known_zero(infoB.uIRandMetaFlags)) {      // A is nyka, B is zero => necessarily not equal
            if (uIsNeqFlag) {
                *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                outMetaValue->knownValue.uEmbeddedValue = 1uLL;
            } else {
                *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
                outMetaValue->knownValue.uEmbeddedValue = 0uLL;
            }
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;

        } else if (irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) { // A is nyka, B is nyka
            Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN);
            return ir_try_solve_eq_cmp_integral_two_nykas(uFormat, infoA.metaValue.knownValue, infoB.metaValue.knownValue, uIsNeqFlag,
                outFlags, outMetaValue);

        } else if (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN) {      // A is nyka, B is a known non-zero, non-nyka
            eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
            goto on_unknown;
        }

    } else if (irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {
        Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        if (irflag_is_known_zero(infoA.uIRandMetaFlags)) {          // B is nyka, A is zero => necessarily not equal
            if (uIsNeqFlag) {
                *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                outMetaValue->knownValue.uEmbeddedValue = 1uLL;
            } else {
                *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
                outMetaValue->knownValue.uEmbeddedValue = 0uLL;
            }
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;

        } else if (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN) {       // B is nyka, A is a known non-zero, non-nyka
            eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
            goto on_unknown;
        }
    }

    if (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && irflag_is_known_non_nyka(infoB.uIRandMetaFlags)) {
        return ir_try_solve_eq_cmp_integral_two_solvables(uFormat, infoA.metaValue.knownValue, infoB.metaValue.knownValue, uIsNeqFlag,
            outFlags, outMetaValue);
    }

    on_unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eReasonWhyUnknown;
}

local_func EIRResult ir_try_solve_ord_cmp_integral_two_nykas(u8 uFormat, AKnownValue nykaA, AKnownValue nykaB,
    u32 uIsGeFlag, EIntSemantics eSemantics, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsGeFlag == 0u || uIsGeFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uFormat <= 0x05u);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    // TODO
    platform_log_error("ir_try_solve_ord_cmp_integral_two_nykas() : not yet implemented");
    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
}

local_func EIRResult ir_try_solve_eq_cmp_integral_two_solvables(u8 uFormat, AKnownValue knownValueA, AKnownValue knownValueB,
    u32 uIsGeFlag, EIntSemantics eSemantics, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsGeFlag == 0u || uIsGeFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uFormat <= 0x05u);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    
    #define DO_COMPARE_UP_TO_64b_SWITCH_ON_RESULT_AND_FLAG(valueA, valueB) do { \
        if ((valueA < valueB) == (uIsGeFlag == 0u)) { \
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD; \
            outMetaValue->knownValue.uEmbeddedValue = 1uLL; \
        } else { \
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO; \
            outMetaValue->knownValue.uEmbeddedValue = 0uLL; \
        } \
    } while(0)

    if (uFormat <= 0x03u) {
        // 8u << uFormat is bitcount of source format.
        // => The following asserts ensures all high bits above src footprint were all zeroed, as they should.
        Assert_( uFormat == 0x03u || 0uLL == ( knownValueA.uEmbeddedValue & (0xFFFF'FFFF'FFFF'FFFFuLL << (8u << uFormat)) ) );
        Assert_( uFormat == 0x03u || 0uLL == ( knownValueB.uEmbeddedValue & (0xFFFF'FFFF'FFFF'FFFFuLL << (8u << uFormat)) ) );
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED) {
            // The following test should work for 8b, 16b, 32b, 64b provided the above asserts hold
            DO_COMPARE_UP_TO_64b_SWITCH_ON_RESULT_AND_FLAG(knownValueA.uEmbeddedValue, knownValueB.uEmbeddedValue);
        } else {
            switch (uFormat) {
                case 0x00u: { // 8b
                    i8 iValA = i8(knownValueA.uEmbeddedValue);
                    i8 iValB = i8(knownValueB.uEmbeddedValue);
                    DO_COMPARE_UP_TO_64b_SWITCH_ON_RESULT_AND_FLAG(iValA, iValB);
                } break;
                case 0x01u: { // 16b
                    i16 iValA = i16(knownValueA.uEmbeddedValue);
                    i16 iValB = i16(knownValueB.uEmbeddedValue);
                    DO_COMPARE_UP_TO_64b_SWITCH_ON_RESULT_AND_FLAG(iValA, iValB);
                } break;
                case 0x02u: { // 32b
                    i32 iValA = i32(knownValueA.uEmbeddedValue);
                    i32 iValB = i32(knownValueB.uEmbeddedValue);
                    DO_COMPARE_UP_TO_64b_SWITCH_ON_RESULT_AND_FLAG(iValA, iValB);
                } break;
                case 0x03u: { // 64b
                    i64 iValA = i64(knownValueA.uEmbeddedValue);
                    i64 iValB = i64(knownValueB.uEmbeddedValue);
                    DO_COMPARE_UP_TO_64b_SWITCH_ON_RESULT_AND_FLAG(iValA, iValB);
                } break;
                default: Assume_(false); return EIRResult::EIRR_ERROR;
            }
        }
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;

    } else {

        const u64* pLegsA = reinterpret_cast<u64*>(knownValueA.pPtrToRawData);
        const u64* pLegsB = reinterpret_cast<u64*>(knownValueB.pPtrToRawData);
        u32 uLegCount = (1u << (uFormat-3u));    // eg for 0x05u, == 1u << 2 == 4 == count of u64 legs in an u256.
        u32 uLastUnsignedLegAndOne = uLegCount;
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) { // we need to check the most significant leg as signed first, in case of signed
            i64 uLegA = i64(pLegsA[uLegCount-1u]);
            i64 uLegB = i64(pLegsB[uLegCount-1u]);
            if (uLegA < uLegB)
                goto whenlt;
            else if (uLegA > uLegB)
                goto whenge;
            // otherwise fallthrough the for-loop below
            uLastUnsignedLegAndOne--;
        }
        for (u32 uLegAndOne = uLastUnsignedLegAndOne; uLegAndOne; uLegAndOne--) { // iterate from most- to least-significant leg => in reverse
            u64 uLegA = pLegsA[uLegAndOne-1u];
            u64 uLegB = pLegsB[uLegAndOne-1u];
            if (uLegA < uLegB)
                goto whenlt;
            else if (uLegA > uLegB)
                goto whenge;
            // otherwise continue
        }
        // if we fallthrough the for loop, not legs where unequal => compared equal => false lt, true ge

        { whenge:
            if (uIsGeFlag)
                goto whentrue;
            else
                goto whenfalse;
        }

        { whenlt:
            if (uIsGeFlag)
                goto whenfalse;
            else
                goto whentrue;
        }

        { whentrue:
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            outMetaValue->knownValue.uEmbeddedValue = 1uLL;
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        }

        { whenfalse:
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
            outMetaValue->knownValue.uEmbeddedValue = 0uLL;
            return EIRResult::EIRR_ENSURED_VALID_KNOWN;
        }
    }
}

// LT, GE
local_func EIRResult ir_try_solve_ord_cmp_integral(u8 uFormat, const IRInfo& infoA, const IRInfo& infoB, u32 uIsGeFlag, EIntSemantics eSemantics,
    TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsGeFlag == 0u || uIsGeFlag == IR_INSTRFLAG_CMP_OPPOSITE);
    Assert_(uFormat <= 0x05u); // otherwise not an integral on which we can do arithmetics (including ordered comparisons). 

    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);

    EIRResult eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
    if (irflag_is_or_has_nyka(infoA.uIRandMetaFlags)) {
        Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        if (irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {     // A is nyka, B is nyka
            Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN);
            return ir_try_solve_ord_cmp_integral_two_nykas(uFormat, infoA.metaValue.knownValue, infoB.metaValue.knownValue, uIsGeFlag,
                eSemantics, outFlags, outMetaValue);

        } else if (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN) {   // A is nyka, B is known non-nyka
            eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
            goto on_unknown;
        }

    } else if (irflag_is_or_has_nyka(infoB.uIRandMetaFlags)) {  
        Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN);
        if (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN) {          // B is nyka, A is known non-nyka
            eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
            goto on_unknown;
        }
    }

    if (irflag_is_known_non_nyka(infoA.uIRandMetaFlags) && irflag_is_known_non_nyka(infoB.uIRandMetaFlags)) {
        return ir_try_solve_eq_cmp_integral_two_solvables(uFormat, infoA.metaValue.knownValue, infoB.metaValue.knownValue, uIsGeFlag,
            eSemantics, outFlags, outMetaValue);
    }

    on_unknown:
    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eReasonWhyUnknown;

    // TODO: also work with some nykas ? we could already simply test nyka equality, but could some distinct nykas solve to same address at one point ?
    bool bIsFullyKnownA = (infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN) && 0uLL == (infoA.uIRandMetaFlags & IRFLAG_HAS_NYKA);
    bool bIsFullyKnownB = (infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN) && 0uLL == (infoB.uIRandMetaFlags & IRFLAG_HAS_NYKA);
    if (bIsFullyKnownA && bIsFullyKnownB) {
        Assert_(infoA.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        Assert_(infoB.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        bool bIsLt = (uIsGeFlag == 0u);
        u32 uKnownBoolResult;
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) {
            switch (uFormat) {
                case 0x00u: { // 8b
                    i8 iValueA = i8(ir_get_u8_value_from_known(infoA));
                    i8 iValueB = i8(ir_get_u8_value_from_known(infoB));
                    uKnownBoolResult = ((iValueA < iValueB) == bIsLt) ? 1u : 0u;
                } break;
                case 0x01u: { // 16b
                    i16 iValueA = i16(ir_get_u16_value_from_known(infoA));
                    i16 iValueB = i16(ir_get_u16_value_from_known(infoB));
                    uKnownBoolResult = ((iValueA < iValueB) == bIsLt) ? 1u : 0u;
                } break;
                case 0x02u: { // 32b
                    i32 iValueA = i32(ir_get_u32_value_from_known(infoA));
                    i32 iValueB = i32(ir_get_u32_value_from_known(infoB));
                    uKnownBoolResult = ((iValueA < iValueB) == bIsLt) ? 1u : 0u;
                } break;
                case 0x03u: { // 64b
                    i64 iValueA = i64(ir_get_u64_value_from_known(infoA));
                    i64 iValueB = i64(ir_get_u64_value_from_known(infoB));
                    uKnownBoolResult = ((iValueA < iValueB) == bIsLt) ? 1u : 0u;
                } break;
                default:
                    // TODO
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL0_IMPL_ERROR, EventREPT_CUSTOM_HARDCODED(
                        "### ir_try_solve_eq_cmp_integral() : integrals > 64b not yet implemented"), pTCContext->pWorker);
                    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
            }
        } else {
            u64 uValueA;
            u64 uValueB;
            switch (uFormat) {
                case 0x00u: { // 8b
                    uValueA = u64(ir_get_u8_value_from_known(infoA));
                    uValueB = u64(ir_get_u8_value_from_known(infoB));
                } break;
                case 0x01u: { // 16b
                    uValueA = u64(ir_get_u16_value_from_known(infoA));
                    uValueB = u64(ir_get_u16_value_from_known(infoB));
                } break;
                case 0x02u: { // 32b
                    uValueA = u64(ir_get_u32_value_from_known(infoA));
                    uValueB = u64(ir_get_u32_value_from_known(infoB));
                } break;
                case 0x03u: { // 64b
                    uValueA = ir_get_u64_value_from_known(infoA);
                    uValueB = ir_get_u64_value_from_known(infoB);
                } break;
                default:
                    // TODO
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL0_IMPL_ERROR, EventREPT_CUSTOM_HARDCODED(
                        "### ir_try_solve_eq_cmp_integral() : integrals > 64b not yet implemented"), pTCContext->pWorker);
                    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
            }
            uKnownBoolResult = ((uValueA < uValueB) == bIsLt) ? 1u : 0u;
        }
        *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_TC_SEMANTIC_CONST;
        outMetaValue->knownValue.uEmbeddedValue = u64(uKnownBoolResult);
        return EIRResult::EIRR_ENSURED_VALID_KNOWN;
    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
}

local_func EIRResult ir_try_solve_address_of(const IRInfo& infoEntity, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    u64 uEntityIR = infoEntity.uIRandMetaFlags & IR_STD_PARAM_MASK;
    Assert_(!ir_is_immediate(uEntityIR));

    outMetaValue->knownValue.uEmbeddedValue = ir_make_direct_nyka_value(uEntityIR);

    // lots of results can be encoded as embedded nykas, unless this is a local, and non 'const'.
    if (0uLL != (uEntityIR & IR_STD_PARAM_HIGHMASK) &&
            (u16(uEntityIR >> IR_STD_PARAM_REPO_ID_SHIFT) == IR_REPO_ID_CURRENT_PROC ||
             u16(uEntityIR >> IR_STD_PARAM_REPO_ID_SHIFT) == IR_REPO_ID_TEMPORARY)) {
        u32 uEntryFlags = u32(infoEntity.uIRandMetaFlags);
        if (0u == (uEntryFlags & IRFLAG_IS_KNOWN) || (uEntryFlags & IRFLAG_HAS_LOCAL_NYKA)) {
            *outFlags = IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_HAS_LOCAL_NYKA|IRFLAG_IS_KNOWN_EMBD;
            return EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
        }
    }

    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_EMBD;
    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
}

local_func EIRResult ir_try_solve_ptr_offset(u32 uEnsuredAlignLog2, const IRInfo& infoBasePtr, u8 uIndexFormat, const IRInfo& infoIndex,
    u32 uIndexScale, EIntSemantics eSemantics, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uEnsuredAlignLog2 <= 12u);
    Assert(uIndexFormat >= 0x02u && uIndexFormat <= 0x07u, "Only possible formats for ptr_offset index are >= 32b integrals");
    Assert_(uIndexScale > 0u);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);

    if ((infoBasePtr.uIRandMetaFlags & IRFLAG_IS_KNOWN) && (infoBasePtr.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
        Assert_(infoBasePtr.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        i32 iOffsetThere;
        u64 uBaseIR = ir_decode_nyka_value(infoBasePtr.metaValue.knownValue.uEmbeddedValue, &iOffsetThere);
        Assert_(!ir_is_immediate(uBaseIR));

        IRRepo* pRepoOfBaseIR;
        u32 uIndexOfBaseIR;
        SourceFileDescAndState* pSourceFileOfBaseIR;
        EEntryKind eEntryKindOfBaseIR;
        ir_decode_non_imm(uBaseIR, pTCContext,
            &pRepoOfBaseIR, &uIndexOfBaseIR, &pSourceFileOfBaseIR, &eEntryKindOfBaseIR);
        if (eEntryKindOfBaseIR != EEntryKind::EEK_IS_PROCBODY_REF) {
            Assert_(pRepoOfBaseIR);
            IREntry& baseEntry = ir_access_repo_instr(pRepoOfBaseIR, uIndexOfBaseIR);
            if (u8(baseEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_DEREF) {
                // TODO ?
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "*** ir_try_solve_ptr_offset() : punch-through deref maybe possible but not yet implemented"), pTCContext->pWorker);
            }
        }

    } else if (!ir_is_immediate(infoBasePtr.uIRandMetaFlags & IR_STD_PARAM_MASK)) {

        IRRepo* pRepoOfAddressIR;
        u32 uIndexOfAddressIR;
        SourceFileDescAndState* pSourceFileOfAddressIR;
        EEntryKind eEntryKindOfAddressIR;
        ir_decode_non_imm(infoBasePtr.uIRandMetaFlags & IR_STD_PARAM_MASK, pTCContext,
            &pRepoOfAddressIR, &uIndexOfAddressIR, &pSourceFileOfAddressIR, &eEntryKindOfAddressIR);
        if (eEntryKindOfAddressIR != EEntryKind::EEK_IS_PROCBODY_REF) {
            Assert_(pRepoOfAddressIR);
            IREntry& addressEntry = ir_access_repo_instr(pRepoOfAddressIR, uIndexOfAddressIR);
            u8 uAddressIRIT = u8(addressEntry.uInstrCodeAndFormatAndFirstParam);
            if (uAddressIRIT == IRIT_PTR_OFFSET || uAddressIRIT == IRIT_PTR_OFFSET_EXT) {
                // TODO ?
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "*** ir_try_solve_ptr_offset() : punch-through other ptr-offset maybe possible but not yet implemented"), pTCContext->pWorker);
            } else if (uAddressIRIT == IRIT_LOCAL_ADDRESS) {
                // TODO ?
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "*** ir_try_solve_ptr_offset() : punch-through local address maybe possible but not yet implemented"), pTCContext->pWorker);
                Assert_(!irflag_is_tc_const(infoBasePtr.uIRandMetaFlags));
                Assert_(infoBasePtr.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA);
                Assert_(addressEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA);
            }
        }
    }

    EIRResult eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN;

    if (irflag_is_known_or_nyka(infoIndex.uIRandMetaFlags)) {
        if (irflag_is_known_zero(infoIndex.uIRandMetaFlags)) {
            *outFlags = u32(infoBasePtr.uIRandMetaFlags) & IRFLAGS_IR_SPECIFIC_MASK;
            *outMetaValue = infoBasePtr.metaValue;
            return EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A;
        } else if (0u == (infoIndex.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
            i64 iIndexValue;
            if (uIndexFormat == 0x02u) {
                u32 uIndexValue = ir_get_u32_value_from_known(infoIndex);
                if (eSemantics == EINT_SEMANTIC_UNSIGNED) {
                    iIndexValue = i64(u64(uIndexValue));
                } else {
                    iIndexValue = i64(i32(uIndexValue));
                }
            } else if (uIndexFormat == 0x03u) {
                // TODO: bounds check ?
                iIndexValue = i64(ir_get_u64_value_from_known(infoIndex));
            } else {
                // TODO
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL0_IMPL_ERROR, EventREPT_CUSTOM_HARDCODED(
                    "### ir_try_solve_ptr_offset() : index format > 64b not yet supported"), pTCContext->pWorker);
                return EIRResult::EIRR_ERROR;
            }
            Assert_(uIndexScale <= 0x1000'0000u);
            i64 iIndexMore = iIndexValue * i64(uIndexScale); // TODO: bounds check ?

            if ((infoBasePtr.uIRandMetaFlags & IRFLAG_IS_KNOWN) && (infoBasePtr.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
                Assert_(infoBasePtr.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
                i32 iOffsetThere;
                u64 uBaseIR = ir_decode_nyka_value(infoBasePtr.metaValue.knownValue.uEmbeddedValue, &iOffsetThere);
                Assert_(!ir_is_immediate(uBaseIR));

                i64 iResultingOffset = i64(iOffsetThere) + iIndexMore;
                if (iResultingOffset < 0x0080'0000 && iResultingOffset >= -0x0080'0000) {
                    outMetaValue->knownValue.uEmbeddedValue = ir_make_nyka_value(uBaseIR, i32(iResultingOffset));
                    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_EMBD;
                    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                }
            }
        } else {
            eReasonWhyUnknown = EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
        }
    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return eReasonWhyUnknown;
}

local_func u32 ir_get_runtime_size_of_entry(const IREntry& entry)
{
    u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
    u8 uFormat = get_outvalue_format_from_instruction(uIRIT, entry.uInstrCodeAndFormatAndFirstParam);
    u32 uSlotsCount;
    u32 uAlignLog2;
    if (tIRITSecondParamSlot[uIRIT] == IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN) {
        uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
        uAlignLog2 = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT + 32));
    } else {
        uSlotsCount = 1u;
        uAlignLog2 = get_log2_of_natural_align_from_format(uFormat);
    }
    u32 uSlotSize = 1u << get_log2_of_slot_size_from_format(uFormat);
    return align_to(1u << uAlignLog2, uSlotsCount * uSlotSize);
}

local_func EIRResult ir_try_solve_deref(const IRInfo& infoPtr, u8 uFormat, u32 uAlignLog2, u32 uSlotsCount, u32 uUnalignedBytesCount,
    u32 uIsAssignableFlag, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uIsAssignableFlag == 0u || uIsAssignableFlag == IR_INSTRFLAG_IS_ASSIGNABLE);
    Assert_(uAlignLog2 <= 12u);
    Assert_(uAlignLog2 >= get_log2_of_natural_align_from_format(uFormat));
    if ((infoPtr.uIRandMetaFlags & IRFLAG_IS_KNOWN) && (infoPtr.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
        Assert_(infoPtr.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
        i32 iOffsetThere;
        u64 uBaseIR = ir_decode_nyka_value(infoPtr.metaValue.knownValue.uEmbeddedValue, &iOffsetThere);
        Assert_(!ir_is_immediate(uBaseIR));
        IRRepo* pRepoOfBaseIR;
        u32 uIndexOfBaseIR;
        SourceFileDescAndState* pSourceFileOfBaseIR;
        EEntryKind eEntryKindOfBaseIR;
        ir_decode_non_imm(uBaseIR, pTCContext,
            &pRepoOfBaseIR, &uIndexOfBaseIR, &pSourceFileOfBaseIR, &eEntryKindOfBaseIR);
        if (eEntryKindOfBaseIR != EEntryKind::EEK_IS_PROCBODY_REF) {
            Assert_(pRepoOfBaseIR);
            u32 uAlignedBytesCount = align_to(1u << uAlignLog2, uUnalignedBytesCount);

            IREntry& baseEntry = ir_access_repo_instr(pRepoOfBaseIR, uIndexOfBaseIR);
            Assert_(0 == (baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_TC_ONLY));
            u32 uRuntimeSizeOfEntry = ir_get_runtime_size_of_entry(baseEntry);
            if (iOffsetThere < 0 || u32(iOffsetThere) + uAlignedBytesCount > ir_get_runtime_size_of_entry(baseEntry)) {
                // Note: we enclose deref to a more basal entry, only in case of trying to eval larger than base
                //   (so that we won't uneccessarily iterate over possibly lots of nykas from a large basal if we already
                //    computed some smaller sub-object meta-info here...)
                if (u8(baseEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_DEREF) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "*** ir_try_solve_deref() : deref trying a more basal entry to succeed"), pTCContext->pWorker);
                    u64 uOtherAddressIR = baseEntry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
                    Assert_(ir_is_valid_param(uOtherAddressIR));
                    Assert_(ir_is_nyka_immediate(uOtherAddressIR) || !ir_is_immediate(uOtherAddressIR));
                    Assert_(uIsAssignableFlag == 0u || (baseEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_IS_ASSIGNABLE));
                    IRInfo infoOtherAddress;
                    ir_get_info_from_ir_param(uOtherAddressIR, 0x03u, pTCContext, &infoOtherAddress);
                    return ir_try_solve_deref(infoOtherAddress, uFormat, uAlignLog2, uSlotsCount, uUnalignedBytesCount, uIsAssignableFlag,
                        pTCContext, outFlags, outMetaValue);
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "*** ir_try_solve_deref() : deref of a valid NYKA seems out of bounds of the known base entry"), pTCContext->pWorker);
                    // => fallthrough unknown
                }
            } else {
                // try to recover actual value from base and offset if known (if not requiring assignable, otherwise eval is simply skipped...)
                if (uIsAssignableFlag == 0u && (baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) && eEntryKindOfBaseIR != EEntryKind::EEK_FILEWISE_VAR) {
                    if (baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_NYKA) {
                        if (baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN_EMBD) {
                            Assert_(uRuntimeSizeOfEntry == 8u);
                            if (iOffsetThere == 0u && uUnalignedBytesCount == 8u) {
                                *outMetaValue = baseEntry.metaValue;
                                *outFlags = baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAGS_IR_SPECIFIC_MASK;
                                return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                            } else { // ensured cut-through nyka...
                                *outFlags = 0;                  // => in that case, result is necessarily unknown, now.
                                outMetaValue->_payload = 0uLL;
                                return EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
                            }
                        } else {
                            // if same footprint: we can skip all the fuss
                            if (iOffsetThere == 0u && uUnalignedBytesCount == uRuntimeSizeOfEntry && uAlignedBytesCount == uUnalignedBytesCount) {
                                *outMetaValue = baseEntry.metaValue;
                                *outFlags = baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAGS_IR_SPECIFIC_MASK;
                                return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                            }
                            // otherwise, we need to iterate over all nykas in src table to check if any of them is kept within result... or cut.
                            const u32* pPtrToNykaTable = reinterpret_cast<const u32*>(baseEntry.metaValue.knownValue.pPtrToRawData);
                            u32 uNykaCount = pPtrToNykaTable[0u];
                            Assert_(uNykaCount);
                            u32 uStart = u32(iOffsetThere);
                            u32 uEnd = uStart + uUnalignedBytesCount;
                            ArenaRefPoint beforeTmp = get_arena_ref_point(pTCContext->pWorker->tmpArena);
                            TmpStackOptiArray<u32, 16u> vecRemainingNykas(pTCContext->pWorker->tmpArena);
                            for (u32 uNykaIndexAndOne = 1u; uNykaIndexAndOne <= uNykaCount; uNykaIndexAndOne++) {
                                u32 uNykaOffset = pPtrToNykaTable[uNykaIndexAndOne];
                                if (uNykaOffset < uEnd && uNykaOffset + 8u >= uStart) {     // deref footprint overlaps a nyka...
                                    if (uNykaOffset < uStart || uNykaOffset + 8u > uEnd) {  // deref footprint cuts through an overlapped nyka
                                        *outFlags = 0;                  // => in that case, result is necessarily unknown, now.
                                        outMetaValue->_payload = 0uLL;
                                        reset_arena_no_release_to(beforeTmp, pTCContext->pWorker->tmpArena);
                                        return EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
                                    } else {
                                        vecRemainingNykas.append(uNykaIndexAndOne);
                                    }
                                }
                            }
                            
                            u8* pOrigPtrToData = *reinterpret_cast<u8**>(baseEntry.metaValue.knownValue.pPtrToRawData + align_to(8u, 4u * (uNykaCount+1u)));
                            
                            if (vecRemainingNykas.size()) { // we have remaining nykas in our result
                                
                                if (vecRemainingNykas.size() < uNykaCount || uStart) { // ...and we won't have same table as the other one
                                    
                                    if ((baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA) && vecRemainingNykas.size() < uNykaCount) { // if we had local nykas, we'll browse through them to know whether any is remaining
                                        // TODO
                                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                                            "*** ir_try_solve_deref() : check of remaining local nykas within known deref not yet implemented"), pTCContext->pWorker);
                                        reset_arena_no_release_to(beforeTmp, pTCContext->pWorker->tmpArena);
                                        return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;
                                    }
                                    // TODO
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                                        "*** ir_try_solve_deref() : reindexing of remaining nykas within known deref not yet implemented"), pTCContext->pWorker);
                                    reset_arena_no_release_to(beforeTmp, pTCContext->pWorker->tmpArena);
                                    return EIRResult::EIRR_UNKNOWN_SOLVER_NOT_YET_IMPLEMENTED;

                                } else { // we can keep same table as the other one...
                                    *outMetaValue = baseEntry.metaValue;
                                    *outFlags = baseEntry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_METAMASK;
                                    reset_arena_no_release_to(beforeTmp, pTCContext->pWorker->tmpArena);
                                    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                                }

                            } else { // we do not have any remaining nyka in our result !!

                                if (uAlignedBytesCount <= 8u) {
                                    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                                    outMetaValue->knownValue.uEmbeddedValue = 0uLL;
                                    memcpy(&(outMetaValue->knownValue.uEmbeddedValue), pOrigPtrToData + iOffsetThere, uUnalignedBytesCount);
                                    if (outMetaValue->knownValue.uEmbeddedValue == 0uLL)
                                        *outFlags |= IRFLAG_IS_KNOWN_ZERO;
                                } else {
                                    *outFlags = IRFLAG_IS_KNOWN;
                                    outMetaValue->knownValue.pPtrToRawData = pOrigPtrToData + iOffsetThere;
                                    IRInfo mockResult{u64(*outFlags), *outMetaValue };
                                    if (ir_check_full_zero_value(mockResult, uUnalignedBytesCount))
                                        *outFlags |= IRFLAG_IS_KNOWN_ZERO;
                                }
                                return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                            }
                        }

                    } else { // no nykas

                        u8* pOrigPtrToData;
                        if (baseEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN_EMBD) {
                            Assert_(uRuntimeSizeOfEntry == 8u);
                            pOrigPtrToData = reinterpret_cast<u8*>(&(baseEntry.metaValue.knownValue.uEmbeddedValue));
                        } else {
                            pOrigPtrToData = baseEntry.metaValue.knownValue.pPtrToRawData;
                        }

                        if (uAlignedBytesCount <= 8u) {
                            *outFlags = IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN;
                            outMetaValue->knownValue.uEmbeddedValue = 0uLL;
                            memcpy(&(outMetaValue->knownValue.uEmbeddedValue), pOrigPtrToData + iOffsetThere, uUnalignedBytesCount);
                        } else {
                            *outFlags = IRFLAG_IS_KNOWN;
                            outMetaValue->knownValue.pPtrToRawData = pOrigPtrToData + iOffsetThere;
                        }
                        return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                    }
                }
            }

        } else {
            // CLEANUP: could be a valid thing to catch by typechecker and throw a hard error... ?
            // but we're in IR-solver here... so, well...
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "*** ir_try_solve_deref() : warning : dereferencing NYKA of a proc-body..."), pTCContext->pWorker);
            // Maybe the user really did want to deref contents at address of proc at runtime ???
            // => fallthrough unknown
        }

    }

    // NOTE: punch-through addresses is no longer expected to be performed here:
    //    the solver for the addressing instruction should deal with that (and with derefs as well)
    // (although we did keep punch-through deref... but only in case of trying to eval "larger" than expected...)

    *outFlags = 0;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_UNKNOWN;
}

local_func EIRResult ir_try_solve_truncate_or_extend_integral_to_integral(u8 uSrcFormat, IRInfo srcInfo, EIntSemantics eSemantics, 
    u8 uDestFormat, TCContext* pTCContext, u32* outFlags, MetaValueIR* outMetaValue)
{
    Assert_(uSrcFormat <= 0x07u);
    Assert_(eSemantics != EIntSemantics::EINT_SEMANTIC_SIGNED || (uSrcFormat <= 5u && uDestFormat <= 5u)); // TODO: CLEANUP: ???
    Assert_(uDestFormat <= 0x07u);

    if (uSrcFormat == uDestFormat) {            // Same format-width case => we can keep data as-is (even if reinterp signedness)

        *outFlags = srcInfo.uIRandMetaFlags & IRFLAGS_IR_SPECIFIC_MASK;
        *outMetaValue = srcInfo.metaValue;
        return EIRResult::EIRR_ENSURED_VALID_SAME_AS_OPERAND_A;

    } else if (uDestFormat < uSrcFormat) {      // Truncation case

        if (srcInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN) { // if source is unknown to start with, all bets are off...
            if (uSrcFormat > 0x03u) { // over 64b, the source IR is necessarily not an immediate.
                u64 uSourceIR = srcInfo.uIRandMetaFlags & IR_STD_PARAM_MASK;
                Assert_(!ir_is_immediate(uSourceIR));
                // => we can confidently use solver for 'deref' to perform a truncation.
                // Note that *even if* we should not technically encode address of *any* IRs as NYKA immediates in actual emitted IR
                //   (since they could be local vars, which should *not* be encoded as nyka immediate, and flagged as local nykas),
                //   we *can* use that trick temporarily here, only for our 'deref' solver (which will not check the origin or 'local' flag of the address).
                IRInfo ptrToSource { ir_make_nyka_immediate(uSourceIR)|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA,
                                     ir_make_direct_nyka_value(uSourceIR) };
                u32 uDestByteCount = 1u << uDestFormat;
                // Note that 'Deref' can itself check for 'EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA' results, and since this is
                //   quite a hairy algorithm in its own right, we prefer handling the task over to it...
                return ir_try_solve_deref(ptrToSource, uDestFormat, get_log2_of_natural_align_from_format(uDestFormat), 1u,
                                          uDestByteCount, 0u, pTCContext, outFlags, outMetaValue);
            } else { // up to 64b, there is a possibility that our src is carried by an immediate => we can't use deref and need to eval locally here.
                Assert_(srcInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD); // CLEANUP: double-check that all our derefs actually ensure embedding for <= 64b
                if (0u == (srcInfo.uIRandMetaFlags & IRFLAG_HAS_NYKA)) { // no nyka
                    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
                    u64 uResult64b;
                    switch(uDestFormat) {
                        case 0x00u: { // truncate to 8b
                            uResult64b = u64(u8(srcInfo.metaValue.knownValue.uEmbeddedValue));
                        } break;
                        case 0x01u: { // truncate to 16b
                            uResult64b = u64(u16(srcInfo.metaValue.knownValue.uEmbeddedValue));
                        } break;
                        case 0x02u: { // truncate to 32b
                            uResult64b = u64(u32(srcInfo.metaValue.knownValue.uEmbeddedValue));
                        } break;
                        default: Assume_(false); return EIRResult::EIRR_ERROR;
                    }
                    if (0uLL == uResult64b)
                        *outFlags |= IRFLAG_IS_KNOWN_ZERO;
                    outMetaValue->knownValue.uEmbeddedValue = uResult64b;
                    return EIRResult::EIRR_ENSURED_VALID_KNOWN;
                } else { // if source had nyka, and at most 64b, and dest is less...
                    Assert_(uSrcFormat == 0x03u); // source at most 64b with nyka => should be exactly 64b.
                    // ... then dest cannot contain a non-truncated nyka => we're out of luck
                    // => unknown result
                    *outFlags = 0u;
                    outMetaValue->_payload = 0uLL;
                    return EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
                    // Although, in an extreme vision of the far future, we *could* check whether ensured align of ptr > 256,
                    //   and known that result is all zeroes is we ask for a 8b dest format in that case...
                }
            }
        } 

    } else { Assert_(uDestFormat > uSrcFormat);     // Zero-Extension or Sign-Extension case

        Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
        
        if (srcInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN) {

            if (0u == (srcInfo.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
                if (uDestFormat <= 0x03u) { // Necessarily embedded destination
                    *outFlags = IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|(u32(srcInfo.uIRandMetaFlags) & IRFLAG_IS_KNOWN_ZERO); // result can only be zero if src is already full zero
                    Assert_(uSrcFormat < 0x03u);
                    Assert_(srcInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD); // CLEANUP: double-check that all our derefs actually ensure embedding for <= 64b
                    // 8u << uSrcFormat is bitcount of source format.
                    // => The following assert ensures all high bits above src footprint were all zeroed, as they should.
                    Assert_( 0uLL == ( srcInfo.metaValue.knownValue.uEmbeddedValue & (0xFFFF'FFFF'FFFF'FFFFuLL << (8u << uSrcFormat)) ) );
                    if (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED) { // zero-extending source
                        // => same embedded contents as source, since all high bits are already zero in source.
                        outMetaValue->knownValue.uEmbeddedValue = srcInfo.metaValue.knownValue.uEmbeddedValue;
                    } else { // sign-extending source...
                        switch (uSrcFormat) {
                            case 0x00u: { // 8b source
                                i8 iOrigValue = i8(srcInfo.metaValue.knownValue.uEmbeddedValue);
                                if (uDestFormat == 0x01u) {
                                    outMetaValue->knownValue.uEmbeddedValue = u64(u16(i16(iOrigValue))); // sign-extension to 16b
                                } else if (uDestFormat == 0x02u) {
                                    outMetaValue->knownValue.uEmbeddedValue = u64(u32(i32(iOrigValue))); // sign-extension to 32b
                                } else { Assert_(uDestFormat == 0x03u);
                                    outMetaValue->knownValue.uEmbeddedValue = u64(i64(iOrigValue)); // sign-extension to 64b
                                }
                            } break;
                            case 0x01u: { // 16b source
                                i16 iOrigValue = i16(srcInfo.metaValue.knownValue.uEmbeddedValue);
                                if (uDestFormat == 0x02u) {
                                    outMetaValue->knownValue.uEmbeddedValue = u64(u32(i32(iOrigValue))); // sign-extension to 32b
                                } else { Assert_(uDestFormat == 0x03u);
                                    outMetaValue->knownValue.uEmbeddedValue = u64(i64(iOrigValue)); // sign-extension to 64b
                                }
                            } break;
                            case 0x02u: { // 32b source
                                i32 iOrigValue = i32(srcInfo.metaValue.knownValue.uEmbeddedValue);
                                Assert_(uDestFormat == 0x03u);
                                outMetaValue->knownValue.uEmbeddedValue = u64(i64(iOrigValue)); // sign-extension to 64b
                            } break;
                            default: Assume_(false);
                        }
                    }
                } else { // Necessarily non-embedded destination
                    *outFlags = IRFLAG_IS_KNOWN|(u32(srcInfo.uIRandMetaFlags) & IRFLAG_IS_KNOWN_ZERO); // result can only be zero if src is already full zero
                    u32 uSrcByteCount = 1u << uSrcFormat;
                    u32 uDestByteCount = 1u << uDestFormat;
                    u8* pNewData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, uDestByteCount, 8u); // all runtime are 8-bytes aligned
                    const u8* pOrigData = ir_get_ptr_to_data_from_known(srcInfo);
                    memcpy(pNewData, pOrigData, uSrcByteCount);
                    u32 uFillerByte = 0x00u; // by default, and if unsigned: high-bytes of result will be zero.
                    if (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) { // if signed semantics, though, we need to sign-extend...
                        if (pOrigData[uSrcByteCount-1u] & 0x80u)   // ...so we check 7th bit of highest byte from source...
                            uFillerByte = 0xFFu;                        // ...and if it is set, filler-bytes will be all ones.
                    }
                    Assert_(uSrcByteCount < uDestByteCount);
                    memset(pNewData + uSrcByteCount, uFillerByte, uDestByteCount - uSrcByteCount);
                }
                return EIRResult::EIRR_ENSURED_VALID_KNOWN;

            } else { // extending a source containing nykas : possibly okay.
                // if signed semantics however, a nyka *cannot* be found at the last (or only) 8Bytes of the src footprint, since we cannot
                // sign-extend from a not-yet known sign-bit.
                u32 uDestByteCount = 1u << uDestFormat;
                if (uSrcFormat == 0x03u) {
                    Assert_(srcInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD); // CLEANUP: double-check that all our derefs actually ensure embedding for <= 64b
                    if (eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED) {
                        Assert_(uDestByteCount > 8u);
                        u8* pNewAllocWithNykaTable = alloc_from(pTCContext->pIsolatedSourceFile->localArena, 16u + uDestByteCount, 8u);
                        u32* pNykaTable = reinterpret_cast<u32*>(pNewAllocWithNykaTable);
                        pNykaTable[0u] = 1u; // here is nyka count: containing a single NYKA
                        pNykaTable[1u] = 0u; // here is offset to single NYKA: necessarily at pos 0
                        u8** pPtrToDataAfterTable = reinterpret_cast<u8**>(pNewAllocWithNykaTable + 8u); // first 2x 32b taken by nyka table
                        u8* pActualResultData = pNewAllocWithNykaTable + 16u; // and 64b after that taken by pointer to data.
                        *pPtrToDataAfterTable = pActualResultData;            // => points to after itself.
                        memcpy(pPtrToDataAfterTable, &(srcInfo.metaValue.knownValue.uEmbeddedValue), 8u);
                        memset(pPtrToDataAfterTable + 8u, 0x00u, uDestByteCount - 8u);
                        *outFlags = u32(srcInfo.uIRandMetaFlags) & (IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_HAS_LOCAL_NYKA);
                        Assert_((*outFlags) & IRFLAG_IS_KNOWN);
                        Assert_((*outFlags) & IRFLAG_HAS_NYKA);
                        outMetaValue->knownValue.pPtrToRawData = pNewAllocWithNykaTable;
                    } else { // we're asking for sign-extension of a value with a high bits necessarily from a NYKA
                        // => out of luck, here
                        *outFlags = 0u;
                        outMetaValue->_payload = 0uLL;
                        return EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
                    }
                } else { Assert_(uSrcFormat > 0x03u); // less than 64b sources should not be flaggued as containing nykas in the first place...
                    Assert_(0 == (srcInfo.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD)); // true after simplification of immediates and embedding
                    const u32* pOrigNykaTable = reinterpret_cast<u32*>(srcInfo.metaValue.knownValue.pPtrToRawData);
                    u32 uNykaCount = pOrigNykaTable[0u];
                    Assert_(uNykaCount);
                    u32 uOffsetOfPtrToData = align_to(8u, (uNykaCount+1u)*4u);
                    const u8* pOrigRawData = *reinterpret_cast<u8**>(srcInfo.metaValue.knownValue.pPtrToRawData + uOffsetOfPtrToData);
                    u32 uSrcByteCount = 1u << uSrcFormat;
                    u32 uFillerByte = 0x00u; // by default, and if unsigned: high-bytes of result will be zero.
                    if (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) {
                        u32 uLastNykaOffset = pOrigNykaTable[uNykaCount];
                        if (uLastNykaOffset >= uSrcByteCount - 8u) { // last position of source is taken by a NYKA...
                            Assert_(pOrigNykaTable[uNykaCount] == uSrcByteCount - 8u); // (should be a full one, btw)
                            // => out of luck here
                            *outFlags = 0;
                            outMetaValue->_payload = 0uLL;
                            return EIRResult::EIRR_ENSURED_VALID_UNKNOWN_CAUSED_BY_NYKA;
                        } else { // otherwise, we check 7th bit of last byte from source data
                            if (pOrigRawData[uSrcByteCount-1u] & 0x80u)
                                uFillerByte = 0xFFu;                        // ...and if it is set, filler-bytes will be all ones.
                        }
                    }
                    u8* pNewAllocWithNykaTable = alloc_from(pTCContext->pIsolatedSourceFile->localArena, uOffsetOfPtrToData + uDestByteCount, 8u);
                    // we copy nyka table with exact same offsets
                    memcpy(pNewAllocWithNykaTable, pOrigNykaTable, uOffsetOfPtrToData);
                    u8** pPtrToDataAfterTable = reinterpret_cast<u8**>(pNewAllocWithNykaTable + uOffsetOfPtrToData);
                    // ptr to data after table, however, ofc changes to somewhere within new alloc.
                    u8* pActualResultData = pNewAllocWithNykaTable + uOffsetOfPtrToData + 8u;
                    *pPtrToDataAfterTable = pActualResultData;
                    memcpy(pActualResultData, pOrigRawData, uSrcByteCount);
                    Assert_(uSrcByteCount < uDestByteCount);
                    memset(pActualResultData + uSrcByteCount, uFillerByte, uDestByteCount - uSrcByteCount);
                }
            }

        } // otherwise unknown => fallthrough unknown result

    }

    *outFlags = 0u;
    outMetaValue->_payload = 0uLL;
    return EIRResult::EIRR_ENSURED_VALID_UNKNOWN;
}


// TMP TMP TMP
#if 0

local_func EOpSolveResult ir_solve_compint_cast_to_integral(IRInfo opA, const TypeInfo_Integral* pDestType, IRInfo* outResult, CompilationContext* pEvalContext)
{
    Assert_(opA.uIRandMetaFlags & IRFLAG_IS_CONST);             // compints are necessary const
    Assert_(opA.uIRandMetaFlags & IRFLAG_IS_CONST_EMBEDDED);    // compints have a special "always-IR-embedded" representation (even if tagged ptrs)
    Assert_(0 == (opA.uIRandMetaFlags & IRFLAG_IS_NYKA));       // compints cannot carry NYKAs
    u8 uDestFormat = get_ir_format(pDestType);
    ESignedness eSignedness = get_signedness(pDestType);
    Assert_(uDestFormat <= 0x05u || (eSignedness == ESignedness::ESIGN_AGNOSTIC && uDestFormat <= 0x07u)); // only supported standard ints
    u64 uPayload = opA.metaValue.knownValue.uEmbeddedValue;
    u8 uIsNeg = u8(uPayload) & 0x04u;
    if ((uPayload & 0x03u) == 0) {
        u64 uAbsValue = uPayload >> 3;
        if (uIsNeg) {
            Assert_(uAbsValue);
            if (eSignedness == ESignedness::EUNSIGNED || pDestType == g_pCoreTypesInfo[ECORETYPE_BOOL] || pDestType == g_pCoreTypesInfo[ECORETYPE_CODEPOINT]) {
                return IROPSOLVE_OPERAND_CANNOT_BE_NEGATIVE;
            } else if (eSignedness == ESignedness::ESIGNED) {
                i64 iValue = i64(-uAbsValue);
                outResult->uIRandMetaFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED;
                if (uDestFormat >= 0x03u) {
                    if (uDestFormat > 0x03u)
                        outResult->uIRandMetaFlags |= IRFLAG_IS_EMBD_SIGN_EXT|IRFLAG_IS_EMBD_NEG_EXT;
                    outResult->metaValue.knownValue.uEmbeddedValue = u64(iValue);
                    return IROPSOLVE_OK_AND_ENSURED_VALID;
                } else {
                    Assert_(pDestType->IRInfosMin.uIRandMetaFlags & IRFLAG_IS_CONST_EMBEDDED);
                    switch (uDestFormat) {
                        case 0x00u: {
                            i8 iMin = i8(pDestType->IRInfosMax.metaValue.knownValue.uEmbeddedValue);
                            if (iValue < i64(iMin))
                                return IROPSOLVE_OK_AND_ENSURED_VALID;
                            outResult->metaValue.knownValue.uEmbeddedValue = u64(u8(iValue));
                        } break;
                        case 0x01u: {
                            i16 iMin = i16(pDestType->IRInfosMax.metaValue.knownValue.uEmbeddedValue);
                            if (iValue < i64(iMin))
                                return IROPSOLVE_OK_AND_ENSURED_VALID;
                            outResult->metaValue.knownValue.uEmbeddedValue = u64(u16(iValue));
                        } break;
                        case 0x02u: {
                            i32 iMin = i32(pDestType->IRInfosMax.metaValue.knownValue.uEmbeddedValue);
                            if (iValue < i64(iMin))
                                return IROPSOLVE_OK_AND_ENSURED_VALID;
                            outResult->metaValue.knownValue.uEmbeddedValue = u64(u32(iValue));
                        } break;
                        default:
                            Assume_(false);
                    }
                    return IROPSOLVE_RANGE_CHECK_FAILURE;
                }
            } else {
                // TODO
                platform_log_error("*** ir_solve_compint_cast_to_integral() : 61b-embd-compint <0 to raw integral not yet implemented",true);
                return IROPSOLVE_NOT_YET_IMPLEMENTED;
            }
        } else {
            outResult->uIRandMetaFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED;
            outResult->metaValue.knownValue.uEmbeddedValue = uAbsValue;
            if (uDestFormat >= 0x03u) {
                if (uDestFormat > 0x03u)
                    outResult->uIRandMetaFlags |= IRFLAG_IS_EMBD_SIGN_EXT;
                return IROPSOLVE_OK_AND_ENSURED_VALID;
            } else {
                Assert_(pDestType->IRInfosMax.uIRandMetaFlags & IRFLAG_IS_CONST_EMBEDDED);
                u64 uEmbeddedMax = pDestType->IRInfosMax.metaValue.knownValue.uEmbeddedValue;
                if (uAbsValue <= uEmbeddedMax) {
                    return IROPSOLVE_OK_AND_ENSURED_VALID;
                } else {
                    return IROPSOLVE_RANGE_CHECK_FAILURE;
                }
            }
        }
    } else {
        // TODO
        platform_log_error("*** ir_solve_compint_cast_to_integral() : non-61b-embd-compint not yet implemented", true);
        return IROPSOLVE_NOT_YET_IMPLEMENTED;
    }
}

#endif // TMP TMP

#if 0  // TMP TMP

//#include "LocLib_SourceFileDescAndState.h"
//#include "LocLib_TypeInfoDecls.h"

/*
struct IRInfo {
    DECL_TRIVIAL_STRUCT_OPS(IRInfo);
    IRInfo(u32 uIR, u32 uFlags, MetaValueIR metaValue):_uIR(uIR), _uFlags(uFlags), _metaValue(metaValue) {}
    u32 _uIR;
    u32 _uFlags;
    MetaValueIR _metaValue;
};
*/

// returns the repository, and pos there, from a 'full' IR with its location flags.
local_func IRRepo* ir_get_localized_ir(u32 uBaseIR, IRRepo* pRepo, CompilationContext* pEvalContext,
    u32* outLoc, u32* outPos)
{
    u32 uBaseIRLoc = (uBaseIR & IR_FLAG32_MASK);
    u32 uBaseIRPos = uBaseIR & IR_INSTRUCTION_PARAM_MASK;
    Assert_(!ir_is_immediate(uBaseIRPos));
    // Finding source repository from full-IR
    IRRepo* pSourceRepo = pRepo;
    if (uBaseIRLoc != pRepo->uIRLocationBits) {
        Assert_(pRepo->uIRLocationBits < uBaseIRLoc);
        if (IR_FLAG32_FILE == uBaseIRLoc) {
            Assert_(pEvalContext->pIsolatedSourceFile);
            pSourceRepo = pEvalContext->pIsolatedSourceFile->filewiseIRRepo;
        } else { Assert_(IR_FLAG32_PROGRAM == uBaseIRLoc);
            pSourceRepo = pEvalContext->pProgCompilationState->globalIRRepo;
            Assert_(uMetaFlags == pSourceRepo->vecFlags[uIRPos]);
        }
    }

    *outLoc = uBaseIRLoc;
    *outPos = uBaseIRPos;
    return pSourceRepo;
}

// a more-involved version of 'ir_get_localized_ir':
// will also follow, to the most basic declaration repo, any other-repo-addressing IRIT found.
local_func IRRepo* ir_punchthrough_to_base_ir(u32 uBaseIR, IRRepo* pRepo, CompilationContext* pEvalContext,
    u32* outLoc, u32* outPos)
{
    u32 uBaseIRLoc = (uBaseIR & IR_FLAG32_MASK);
    u32 uBaseIRPos = uBaseIR & IR_INSTRUCTION_PARAM_MASK;
    Assert_(!ir_is_immediate(uBaseIRPos));
    // Finding source repository from full-IR
    IRRepo* pSourceRepo = pRepo;
    if (uBaseIRLoc != pRepo->uIRLocationBits) {
        Assert_(pRepo->uIRLocationBits < uBaseIRLoc);
        if (IR_FLAG32_FILE == uBaseIRLoc) {
            Assert_(pEvalContext->pIsolatedSourceFile);
            pSourceRepo = pEvalContext->pIsolatedSourceFile->filewiseIRRepo;
        } else { Assert_(IR_FLAG32_PROGRAM == uBaseIRLoc);
            pSourceRepo = pEvalContext->pProgCompilationState->globalIRRepo;
            Assert_(uMetaFlags == pSourceRepo->vecFlags[uIRPos]);
        }
    }

    // punching-through-refs to primary repo
    u64 uInstructionThere = pSourceRepo->vecInstr[uBaseIRPos];
    if (u8(uInstructionThere) == IRIT_ACCESS_FILEWISE) {
        Assert_(pSourceRepo->uIRLocationBits < IR_FLAG32_FILE);
        uBaseIRPos = (uInstructionThere >> 16u) & IR_INSTRUCTION_PARAM_MASK;
        Assert_(!ir_is_immediate(uBaseIRPos));
        pSourceRepo = pEvalContext->pIsolatedSourceFile->filewiseIRRepo;
        uBaseIRLoc = IR_FLAG32_FILE;
        uInstructionThere = pSourceRepo->vecInstr[uBaseIRPos];
    }
    if (u8(uInstructionThere) == IRIT_ACCESS_PROGRAMWISE) {
        Assert_(pSourceRepo->uIRLocationBits < IR_FLAG32_PROGRAM);
        uBaseIRPos = (uInstructionThere >> 16u) & IR_INSTRUCTION_PARAM_MASK;
        Assert_(!ir_is_immediate(uBaseIRPos));
        pSourceRepo = pEvalContext->pProgCompilationState->globalIRRepo;
        uBaseIRLoc = IR_FLAG32_FILE;
        uInstructionThere = pSourceRepo->vecInstr[uBaseIRPos];
    }

    *outLoc = uBaseIRLoc;
    *outPos = uBaseIRPos;
    return pSourceRepo;
}

// predecl
void ir_solve_get_address(u32 uBaseIR, IRRepo* pRepo, CompilationContext* pEvalContext,
    u32* outFlags, MetaValueIR* outValue);

// solves a 'get_address' operation in case the base IR to retrieve address from happens to be a deref...
// since derefs carry an address themselves, addressof(derefof(a)) == id(a)
// Also, will setup a recursion-pair with 'ir_solve_get_address' in case 'id(a)' is addressof(b) in the example above...
local_func void ir_solve_get_address_of_deref(u64 uDerefInstruction, u32 uCheckPos, IRRepo* pRepo, CompilationContext* pEvalContext,
    u32* outFlags, MetaValueIR* outValue)
{
    Assert_(pRepo->vecInstr[uCheckPos] == uDerefInstruction);
    u32 uDereferencedAddressIRPos = u32(uInstructionThere >> 16) & IR_INSTRUCTION_PARAM_MASK;
    Assert_(!ir_is_immediate(uDereferencedAddressIRPos));
    Assert_(uDereferencedAddressIRPos < uCheckPos); // should prevent infinite recursions in case of encoding error...
    u64 uInstructionThere = pRepo->vecInstr[uDereferencedAddressIRPos];
    
    if (u8(uInstructionThere) == IRIT_ADDRESS_OF) {
        u32 ofWhatFullIR = (uInstructionThere >> 32);
        Assert_((ofWhatFullIR & IR_INSTRUCTION_PARAM_MASK) < uDereferencedAddressIRPos ||
            (ofWhatFullIR & IR_FLAG32_MASK) != pRepo->uIRLocationBits); // also to prevent infinite recursions
        ir_solve_get_address(ofWhatFullIR, pRepo, pEvalContext, outFlags, outValue);
    } else {
        *outFlags = pSourceRepo->vecFlags[uDereferencedAddressIR];
        *outValue = pSourceRepo->vecMeta[uDereferencedAddressIR];
    }
}

// solves a 'get_address' operation
// will in-place try to get to deepest possible base, following deref/addressof... pairs
local_func void ir_solve_get_address(u32 uBaseIR, IRRepo* pRepo, CompilationContext* pEvalContext,
    u32* outFlags, MetaValueIR* outValue)
{
    u32 uBaseIRLoc, uBaseIRPos;
    IRRepo* pSourceRepo = ir_punchthrough_to_base_ir(uBaseIR, pRepo, pEvalContext, &uBaseIRLoc, &uBaseIRPos);

    // checking flags on the referenced value to determine constness
    u32 uFlagsThere = pSourceRepo->vecFlags[uBaseIRPos];
    Assert_(uFlagsThere & IRFLAG_IS_USER_REFERENCABLE);
    if (uFlagsThere & IRFLAG_IS_CONST_REFERENCABLE)
        *outFlags = IRFLAG_IS_NYKA|IRFLAG_IS_CONST;
    else
        *outFlags = IRFLAG_IS_NYKA;

    u64 uInstructionThere = pSourceRepo->vecInstr[uBaseIRPos];

    if (u8(uInstructionThere) == IRIT_DEREF) {
        u32 uFurtherFlags;
        ir_solve_get_address_of_deref(uInstructionThere, uBaseIRPos, pSourceRepo, pEvalContext, &uFurtherFlags, outValue);
        Assert_(uFurtherFlags == *outFlags);
    } else {
        outValue->knownValue.nyka.uBaseIR = uBaseIRPos|uBaseIRLoc;
        outValue->knownValue.nyka._iEmbdOffset = 0;
    }
}

#define NYKAMASK_FULL_IR            0xC07F'0000uLL
#define NYKAFLAG_IS_BY_IR_OFFSET    0x0080'0000uLL
#define NYKAFLAG_IS_LARGE_OFFSET    0x0080'0000uLL
#define NYKAFLAG_IS_BYTE_FACTOR_NEG 0x2000'0000uLL

local_func void ir_get_info_from(u32 uBaseIR, IRRepo* pRepo, CompilationContext* pEvalContext,
    IRInfo* outInfo)
{
    u32 uBaseIRLoc, uBaseIRPos;
    IRRepo* pSourceRepo = ir_punchthrough_to_base_ir(uBaseIR, pRepo, pEvalContext, &uBaseIRLoc, &uBaseIRPos);
    outInfo->_uIR = uBaseIR;
    outInfo->_uFlags = pSourceRepo->vecFlags[uBaseIRPos];
    outInfo->_metaValue = pSourceRepo->vecMeta[uBaseIRPos];
}


local_func void ir_get_info_ext_from(u32 uBaseIR, IRRepo* pRepo, CompilationContext* pEvalContext,
    IRInfo* outInfo, u8* outFormat, u32* outSlotsCount)
{
    u32 uBaseIRLoc, uBaseIRPos;
    IRRepo* pSourceRepo = ir_punchthrough_to_base_ir(uBaseIR, pRepo, pEvalContext, &uBaseIRLoc, &uBaseIRPos);
    outInfo->_uIR = uBaseIR;
    outInfo->_uFlags = pSourceRepo->vecFlags[uBaseIRPos];
    outInfo->_metaValue = pSourceRepo->vecMeta[uBaseIRPos];
    u64 uInstr = pSourceRepo->vecInstr[uBaseIRPos];
    *outFormat = ir_get_format_from_instruction(uInstr, pEvalContext);
    *outSlotsCount = (outInfo->_uFlags & IRFLAG_INSTR_HAS_SLOTCOUNT) ? u32(uInstr >> 40) : 1u;
}

struct AdressedElement {
    IRRepo* pSource;
    u32 uPosInSource;
    i32 iConstByteFactorZeroIfKnown;
    i64 iWhenKnownOffsetOrIs64IfUnknown;
    IRInfo whenUnknownOffset;
};

// decodes any NYKA into its consituent parts, also trying to compute a bytewise const offset from a base when possible
local_func void ir_decode_nyka(NYKA nyka, IRRepo* pRepo, CompilationContext* pEvalContext, AdressedElement* outResult)
{
    u32 unusedLoc;
    u32 uBaseIR = nyka.uBaseIR & NYKAMASK_FULL_IR;
    outResult->pSource = ir_punchthrough_to_base_ir(uBaseIR, pRepo, pEvalContext, &unusedLoc, &(outResult->uPosInSource));

    if (nyka.uBaseIR & NYKAFLAG_IS_BY_IR_OFFSET) {
        u32 uOffsetIR = nyka._uOffsetIR & NYKAMASK_FULL_IR;
        Assert_(!ir_is_immediate(uOffsetIR));
        u32 uOffsetPos;
        IRRepo* pSourceOfOffset = ir_get_localized_ir(uOffsetIR, pRepo, pEvalContext, &unusedLoc, &uOffsetPos);
        IRInfo whenNonEmbeddedOffset;
        whenNonEmbeddedOffset._uIR = uOffsetIR;
        whenNonEmbeddedOffset._uFlags = pSourceOfOffset->vecFlags[uOffsetPos];
        whenNonEmbeddedOffset._metaValue = pSourceOfOffset->vecMeta[uOffsetPos];
        u32 uOffsetFactor = (nyka.uBaseIR >> 24) & 0x3Fu;
        uOffsetFactor |= (nyka._uOffsetIR >> 18) & 0x7C0u;
        uOffsetFactor += 1u;
        i32 iConstByteFactorWhenByIR = i32(uOffsetFactor);
        if (nyka._uOffsetIR & NYKAFLAG_IS_BYTE_FACTOR_NEG)
            iConstByteFactorWhenByIR = -iConstByteFactorWhenByIR;
        if (is_ir_flag_indicating_known_const(whenNonEmbeddedOffset._uFlags)) {
            Assert_(whenNonEmbeddedOffset._uFlags & IRFLAG_IS_CONST_EMBEDDED); // should be either a 64b or 32b value => necessarily embedded
            i64 iKnownOffset32or64b = (nyka._uOffsetIR & NYKAFLAG_IS_LARGE_OFFSET) ?
                i64(whenNonEmbeddedOffset._metaValue.knownValue.uEmbeddedValue) :
                i64(i32(whenNonEmbeddedOffset._metaValue.knownValue.uEmbeddedValue));
            outResult->iConstByteFactorZeroIfKnown = 0;
            // TODO: CLEANUP: ensure C++ compiler allowance of signed mul modulo64 when overflow here!!!
            outResult->iWhenKnownOffsetOrIs64IfUnknown = iKnownOffset32or64b * iConstByteFactorWhenByIR;
        } else {
            outResult->iConstByteFactorZeroIfKnown = iConstByteFactorWhenByIR;
            outResult->iWhenKnownOffsetOrIs64IfUnknown = (nyka._uOffsetIR & NYKAFLAG_IS_LARGE_OFFSET) ? 1uLL : 0uLL;
            outResult->whenUnknownOffset = whenNonEmbeddedOffset;
        }
    } else {
        outResult->iConstByteFactorZeroIfKnown = 0;
        outResult->iWhenKnownOffsetOrIs64IfUnknown = i64(nyka._iEmbdOffset);
    }
}

// TODO once IR-optims are in-place:
//   a version of nyka-solving allowing it to browse a full hierarchy of 'AdressedElement' (even if an unknown is reached)
//   To be able to consolidate together all const-parts to a single NYKA, and where only remains as distinct operations the
//      different unknowns
//   Improvement: if within the hierarchy, one unknown and same unknown are added with opposite signs -> that's a NOOP

// TODO once IR-optims are in-place:
//    allow NYKA-solver to reference 'virtual' IR (which, crucially, it could emit itself) for either:
//    * adressing NYKA with const byte offsets beyond 32b
//    * even deciding on interesting 'intermediate' unknown-offset values to solve with adds or subs (or muls or shifts and adds or subs, etc)

// tries to solves an add-ofset to a nyka, returning another nyka
local_func bool ir_try_solve_add_offset_to_nyka(u32 uBaseFlags, NYKA baseNyka, const IRInfo& newOffset, bool bNewOffsetIs64b, i32 iConstByteFactor,
                                                IRRepo* pRepo, CompilationContext* pEvalContext, u32* outFlags, MetaValueIR* outValue)
{
    Assert_(iConstByteFactor);
    Assert_(abs(iConstByteFactor) <= 2048);
    Assert_(uBaseFlags & IRFLAG_IS_NYKA);

    // Adding offset 0 => return original nyka
    // TODO: CLEANUP: ...ooor, do not return right away and still allow that solver to punch through base and try to find a more
    //   direct definition from the current one ???
    if (is_ir_flag_indicating_known_embedded_const(newOffset._uFlags) && newOffset._metaValue.knownValue.uEmbeddedValue == 0uLL) {
        *outFlags = uBaseFlags;
        outValue->knownValue.nyka = baseNyka;
        return true;
    }

    AdressedElement origin;
    ir_decode_nyka(baseNyka, pRepo, pEvalContext, &origin);

    if (origin.iConstByteFactorZeroIfKnown == 0)
        goto when_known_base_offset;
    else
        goto when_unknown_base_offset;

    when_known_base_offset:
        i64 iNewByteOffset64;
        if (is_ir_flag_indicating_known_const(newOffset)) {
            i64 iOffsetValue64;
            if (bNewOffsetIs64b) {
                ir_output_runtime_const_value_to(&iOffsetValue64, 0x03u, 1u, newOffset._uFlags, newOffset._metaValue.knownValue);
            } else {
                i32 iOffsetValue32;
                ir_output_runtime_const_value_to(&iOffsetValue32, 0x02u, 1u, newOffset._uFlags, newOffset._metaValue.knownValue);
                iOffsetValue64 = i64(iOffsetValue32);
            }
            iNewByteOffset64 = iOffsetValue64 * i64(iConstByteFactor);
            goto when_known_both;
        } else {
            goto when_known_base_offset_and_unknown_new;
        }

    when_known_both:
        i64 iResultByteOffset = origin.iWhenKnownOffsetOrIs64IfUnknown + iNewByteOffset64;
        if (iResultByteOffset >= -0x0000'0000'8000'0000u && iResultByteOffset < 0x0000'0000'8000'0000u) {
            *outFlags = uBaseFlags;
            outValue->knownValue.nyka.uBaseIR = uBaseLoc | uBasePos;
            outValue->knownValue.nyka._iEmbdOffset = i32(iResultByteOffset);
            return true;
        } else {
            if (origin.iWhenKnownOffsetOrIs64IfUnknown) {
                // TODO: maybe allow solver to 'emit' temporary IR values to make progress ??
                // or, TODO: maybe allow special repo flag only on NYKA *offsets* ; and solver can write or read from that index in some new format
                return false;
            } else {
                if (!ir_is_immediate(newOffset._uIR)) {
                    goto when_no_base_offset_and_non_embd_new;
                } else {
                    // "could" happen since max 2048 factor * max 4M IR-immediate-int is well-outside of 'i32' range
                    // TODO: maybe allow solver to 'emit' temporary IR values to make progress ??
                    // or, TODO: maybe allow special repo flag only on NYKA *offsets* ; and solver can write or read from that index in some new format
                    return false;
                }
            }
        }

    when_known_base_offset_and_unknown_new:
        if (origin.iWhenKnownOffsetOrIs64IfUnknown) {
            // TODO: maybe allow solver to 'emit' temporary IR values to make progress ??
            // or, TODO: maybe allow special repo flag only on NYKA *offsets* ; and solver can write or read from that index in some new format
            return false;

        } else { when_no_base_offset_and_non_embd_new:
            Assert_(!ir_is_immediate(newOffset._uIR));
            u32 uAbsConstByteFactorM1, uSignConstByteFactor;
            if (iConstByteFactor > 0) {
                uAbsConstByteFactorM1 = u32(iConstByteFactor) - 1u;
                uSignConstByteFactor = 0;
            } else {
                uAbsConstByteFactorM1 = u32(-iConstByteFactor) - 1u;
                uSignConstByteFactor = NYKAFLAG_IS_BYTE_FACTOR_NEG;
            }
            outValue->knownValue.nyka.uBaseIR = uBaseLoc | uBasePos | NYKAFLAG_IS_BY_IR_OFFSET | ((uAbsConstByteFactorM1 & 0x3Fu) << 24);
            outValue->knownValue.nyka._uOffsetIR = newOffset._uIR | uSignConstByteFactor | ((uAbsConstByteFactorM1 & 0x7C0u) << 18);
            if (bNewOffsetIs64b)
                outValue->knownValue.nyka._uOffsetIR |= NYKAFLAG_IS_LARGE_OFFSET;
            return true;
        }

    when_unknown_base_offset:
        // TODO: maybe allow solver to 'emit' temporary IR values to make progress ??
        // or, TODO: maybe allow special repo flag only on NYKA *offsets* ; and solver can write or read from that index in some new format
        return false;
}


struct KnownDerefQueryResult {
    IRInfo irInfo;
    IRRepo* pSource;
    u32 uFormat8MsbAndSlotCount24Lsb;
    u32 uByteOffset32;
};

// impl for 'ir_is_deref_nyka_range_const_or_known'
local_func bool ir_is_deref_base_and_offset_range_const_or_known(u32 uBaseIRPos, i64 iByteOffset, u32 uTotalDerefBytes,
    IRRepo* pRepo, CompilationContext* pEvalContext, KnownDerefQueryResult* optOutKnown)
{
    Assert_(0 == (uBaseIRPos & IR_FLAG32_MASK));
    Assert_(!ir_is_immediate(uBaseIRPos));

    u64 uInstructionThere = pRepo->vecInstr[uBaseIRPos];
    u8 uIRIT = u8(uInstructionThere);
    if (uIRIT == IRIT_DECLARE_VAR)          // the referenced thingie is a variable => we cannot solve that deref as const
        return false;
    Assert_(uIRIT == IRIT_DECLARE_CONST || uIRIT == IRIT_DEREF); // Should not be a nyka otherwise...

    if (iByteOffset >= 0) {

        u32 uFlagsThere = pRepo->vecFlags[uBaseIRPos];
        Assert_(uFlagsThere & IRFLAG_INSTR_HAS_SLOTCOUNT);
        u32 uOriginSlots = u32(uInstructionThere >> 40); // All instructions with specified constant slotcount follow this protocol.
        u8 uOriginFormat = u8(uInstructionThere >> 8);   // Mosts instructions follow this protocol. IRIT_DECLARE_CONST and IRIT_DEREF certainly do.
        u32 uBytesPerSlotOrigin = 1u << get_log2_of_slot_size_from_format(uOriginFormat);
        Assert_(u64(uBytesPerSlotOrigin) * u64(uOriginSlots) <= u64(MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE));
        u32 uTotalBytesOrigin = uBytesPerSlotOrigin * uOriginSlots;

        if (u64(iByteOffset) + u64(uTotalDerefBytes) <= u64(uTotalBytesOrigin)) {
            // our footprint fits the declaration => we may be able to interpret as a constant (or a fully known ?) here.
            if (uFlagsThere & (IRFLAG_IS_CONST|IRFLAG_IS_NYKA)) {
                if (uFlagsThere & IRFLAG_IS_CONST) {
                    if (optOutKnown) {
                        optOutKnown->irInfo._uIR = uBaseIRPos | pRepo->uIRLocationBits;
                        optOutKnown->irInfo._uFlags = uFlagsThere;
                        optOutKnown->irInfo._metaValue = pRepo->vecMeta[uBaseIRPos];
                        optOutKnown->pSource = pRepo;
                        optOutKnown->uFormat8MsbAndSlotCount24Lsb = uOriginSlots | (u32(uOriginFormat) << 24);
                        optOutKnown->uByteOffset32 = u32(iByteOffset);
                    }
                    return true;
                }
            } // But if unknown : also fallthrough !!!
            // Rationale: a deref, even flagged unknown, could be unknown by *laziness* of the deref-solving strategy.
            //            => following derefs to their base decl (which, if const, *shall* be flagged known) is the solution here also.

        } // otherwise, fallthrough from positive out-of-range check...

    } // otherwise, if negative offset: (+fallthroughs)

    // if we reach here, we're out-of-range for that declaration (OR we got there after in-range-but-unknown-fallthrough)

    if (uIRIT == IRIT_DECLARE_CONST)    // ...so, if our base was an independant const declaration, we're simply out of luck.
        return false;

    else { Assert_(uIRIT == IRIT_DEREF);
        // on the other hand, if we're referencing another 'deref', then it is possible that its own *base* is a sufficiently large const
        // => recursively try to complete through it:
        u32 uDereferencedAddressIRPos = u32(uInstructionThere >> 16) & IR_INSTRUCTION_PARAM_MASK;
        Assert_(!ir_is_immediate(uDereferencedAddressIRPos));
        Assert_(uDereferencedAddressIRPos < origin.uPosInSource); // should prevent infinite recursions in case of encoding error...
        u32 uAddressFlags = origin.pSource->vecFlags[uDereferencedAddressIRPos];
        if (uAddressFlags & IRFLAG_IS_NYKA) {
            NYKA baseNyka = origin.pSource->vecMeta[uDereferencedAddressIRPos].knownValue.nyka;
            AdressedElement newOrigin;
            ir_decode_nyka(baseNyka, pRepo, pEvalContext, &newOrigin);
            if (newOrigin.iConstByteFactorZeroIfKnown) // the new base NYKA is by unknown offset => we cannot solve that deref as const
                return false;
            i64 iNewByteOffset = i64(u64(iByteOffset) + u64(newOrigin.iWhenKnownOffsetOrIs64IfUnknown)); // casts to allow C++ compiler modulo arith
            return ir_is_deref_base_and_offset_range_const_or_known(newOrigin.uPosInSource, iNewByteOffset, uTotalDerefBytes,
                newOrigin.pSource, pEvalContext, outFlags, outValue);
        
        } else // not a nyka ??? => fully unknown runtime address, null address or even absolute const address forced so by user...
            return false;   // ...we cannot help in any of those cases.
    }
}

// only queries about the constness of a deref range, without actually trying to solve it. Returns known valid base, though.
local_func_inl bool ir_is_deref_nyka_range_const_or_known(NYKA adrNyka, u32 uByteFootprint,
                                                          IRRepo* pRepo, CompilationContext* pEvalContext,
                                                          KnownDerefQueryResult* optOutKnown)
{
    AdressedElement origin;
    ir_decode_nyka(adrNyka, pRepo, pEvalContext, &origin);
    if (origin.iConstByteFactorZeroIfKnown) // the NYKA is by unknown offset => we cannot know further about that deref
        return false;

    return ir_is_deref_base_and_offset_range_const_or_known(origin.uPosInSource, origin.iWhenKnownOffsetOrIs64IfUnknown, uByteFootprint,
                                                            origin.pSource, pEvalContext, outFlags, outValue);
}

// tries to solves a deref as a const
// TODO: a version for IR-'optim' which can work with *current values* when referencing variable decls.
// TODO: a version taking padding into account, or something ?
// ... or simply allow derefs to also punch-through derefs towards higher bases when non-solved
//     (since it could also solve our lazyness problem ? would punchtrough not introduce new perf problems of its own ? maybe not)
// => okay try that...
local_func_inl bool ir_try_solve_deref_nyka_as_const(NYKA adrNyka, u8 uFormat, u32 uSlotsCount,
                                                     IRRepo* pRepo, CompilationContext* pEvalContext, u32* outFlags, AKnownValue* outValue)
{
    u32 uBytesPerSlot = 1u << get_log2_of_slot_size_from_format(uFormat);
    Assert_(u64(uBytesPerSlot) * u64(uSlotsCount) <= u64(MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE));
    u32 uTotalDerefBytes = uBytesPerSlot * uSlotsCount;

    KnownDerefQueryResult resultWhenKnown;
    if (ir_is_deref_nyka_range_const_or_known(adrNyka, uTotalDerefBytes, pRepo, pEvalContext, &resultWhenKnown)) {
        AKnownValue baseValue = resultWhenKnown.irInfo._metaValue.knownValue;
        u32 uBaseFlags = resultWhenKnown.irInfo._uFlags;
        u8 uBaseFormat = u8(resultWhenKnown.uFormat8MsbAndSlotCount24Lsb >> 24);
        u32 uBaseSlotsCount = resultWhenKnown.uFormat8MsbAndSlotCount24Lsb & 0x00FF'FFFFu;
        if (uBaseFormat == uFormat && uBaseSlotsCount == uSlotsCount && resultWhenKnown.uByteOffset32 == 0u) { // special case dereferencing same
            *outFlags = uBaseFlags;
            *outValue = baseValue;
            return true;
        } else {
            u32 uBytesPerSlotOrigin = 1u << get_log2_of_slot_size_from_format(uBaseFormat);
            Assert_(u64(uBytesPerSlotOrigin) * u64(uBaseSlotsCount) <= u64(MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE));
            u32 uTotalBytesOrigin = uBytesPerSlotOrigin * uBaseSlotsCount;
            return extract_from_known(uBaseFlags, baseValue, uBaseFormat, uBaseSlotsCount, uTotalBytesOrigin,
                                      pRepo, pEvalContext, uFormat, uSlotsCount, uTotalDerefBytes, outFlags, outValue);
        }
    }
}

// impl method for 'ir_get_nyka_primary_base'
local_func void ir_get_primary_from_base_and_offset(u32 uBaseIRPos, i64 iByteOffset, bool isByteOffsetKnownUpTillNow,
    IRRepo* pRepo, CompilationContext* pEvalContext, KnownDerefQueryResult* outResult, bool* outOffsetIsKnown)
{
    Assert_(0 == (uBaseIRPos & IR_FLAG32_MASK));
    Assert_(!ir_is_immediate(uBaseIRPos));

    u64 uInstructionThere = pRepo->vecInstr[uBaseIRPos];
    u8 uIRIT = u8(uInstructionThere);

    if (uIRIT == IRIT_DECLARE_VAR || uIRIT == IRIT_DECLARE_CONST) { return_current:
        outResult->irInfo._uIR = uBaseIRPos | pRepo->uIRLocationBits;
        outResult->irInfo._uFlags = pRepo->vecFlags[uBaseIRPos];
        outResult->irInfo._metaValue = pRepo->vecMeta[uBaseIRPos];
        outResult->pSource = pRepo;
        outResult->uByteOffset32 = u32(iByteOffset);
        outResult->uFormat8MsbAndSlotCount24Lsb = uOriginSlots | (u32(uOriginFormat) << 24);
        u32 uOriginSlots = u32(uInstructionThere >> 40); // All instructions with specified constant slotcount follow this protocol.
        u8 uOriginFormat = u8(uInstructionThere >> 8);   // Mosts instructions follow this protocol. the 3 possible ones here certainly do.
        *outOffsetIsKnown = isByteOffsetKnownUpTillNow;
        return; // for clarity to the reader :p
    
    } else { Assert_(uIRIT == IRIT_DEREF);
        u32 uDereferencedAddressIRPos = u32(uInstructionThere >> 16) & IR_INSTRUCTION_PARAM_MASK;
        Assert_(!ir_is_immediate(uDereferencedAddressIRPos));
        Assert_(uDereferencedAddressIRPos < origin.uPosInSource); // should prevent infinite recursions in case of encoding error...
        u32 uAddressFlags = origin.pSource->vecFlags[uDereferencedAddressIRPos];
        if (uAddressFlags & IRFLAG_IS_NYKA) {
            NYKA baseNyka = origin.pSource->vecMeta[uDereferencedAddressIRPos].knownValue.nyka;
            AdressedElement newOrigin;
            ir_decode_nyka(baseNyka, pRepo, pEvalContext, &newOrigin);
            if (newOrigin.iConstByteFactorZeroIfKnown) // the new base NYKA is by unknown offset => we force our offset to unknown.
                isByteOffsetKnownUpTillNow = false;
            i64 iNewByteOffset = i64(u64(iByteOffset) + u64(newOrigin.iWhenKnownOffsetOrIs64IfUnknown)); // casts to allow C++ compiler modulo arith
            ir_get_primary_from_base_and_offset(newOrigin.uPosInSource, iNewByteOffset, isByteOffsetKnownUpTillNow,
                newOrigin.pSource, pEvalContext, outResult, outOffsetIsKnown);
            return; // not necessary maybe, but could help C++ compiler to detect a possible tail-call here ?

        } else // not a nyka ??? => fully unknown runtime address, null address or even absolute const address forced so by user...
            goto return_current;   // ... we cannot go further in any of those cases => return that 'deref' as a primary...
    }
}

// retrieves the primary declaration (and its info) corresponding to a NYKA.
local_func_inl void ir_get_nyka_primary_base(NYKA adrNyka, u8 uFormat, IRRepo* pRepo, CompilationContext* pEvalContext,
                                             KnownDerefQueryResult* outResult, bool* outOffsetIsKnown)
{
    AdressedElement origin;
    ir_decode_nyka(adrNyka, pRepo, pEvalContext, &origin);
    ir_get_primary_from_base_and_offset(origin.uPosInSource, origin.iWhenKnownOffsetOrIs64IfUnknown,
        origin.iConstByteFactorZeroIfKnown == 0, origin.pSource, pEvalContext, outResult, outOffsetIsKnown);
}

// Okay, with those infrastructure designs in place, now 'only' remains to see whether it could get nicely hand in hand with TC for
//   the more basic operations:
// 
// => starting with 'add'
//   We need a version of 'add' here, where TC would have already decided of the resulting format, would have already required implicit
//     casts where needed, and which now would operate fully in-ir.
//
// Three possibilities from there on:
// 
// 1) keep current scheme of TC checking constness beforehand, and when-const, dealing with const checks + const result.
//      ...this would somewhat defeat the purpose of this 'full-IR-switch' design change.
// 2) go 'really-full-IR' and emit each IR as-if runtime, record the range of new-IR, and subsequently emit compiler error
//      if a check is known to fail.
//      ...this seems cumbersome to implement in terms of infrastructure, and not sure it would play well when we do IR-optim,
//         with 'known' runtime possibly failing afterwards ? although... maybe this is what we need ? but what if we're in a dead path on eval ?
// 3) take a somewhat hybrid approach: (that was the primary intent when we created this 'LocLib_IRSolver.h' file separately in the first place):
//      * previous IR emission functions will now take more in charge, and be closer to an intermediate with TC.
//      * in particular, they will take in charge the calling of an associated 'ir_solve' function on their arguments.
//      * they will also be fed with all infos for performing required check, and *they* (at the time of emission) can then decide
//          to 'fail' with a compilation error in case of const-detected failure.
//
// => ir_do_add_or_sub_integral(IRInfo opA, IRInfo opB, bool bIsSub, check enum:
//                              - ENOCHECK(wrap case)
//                              - ECHECKSIGNED
//                              - ECHECKUNSIGNED
//                              - ECHECKMIXED(for signed opA and unsigned "1b larger" const opB)
//                             ) =>
// 
//  will, first, call a ir_solve_add_or_sub_integral with same params.
//  that solver can:
//      - decide that the result best corresponds to one of the operands, unchanged => returns that IR with no actual IR emission
//      - decide that the result is some other known constant, which it has latitude to compute => returns flags and KnownValue for it
//          * in which case we'll *emit* the IR and right-away position those flags and KnownValue
//      - decide that the result fails the associated check-behavior
//          * in which case we'll report the error upwards to the typechecker
//      - decide none of the above (but the versions we'll do later could maybe also handle partial constraints resolution and return non-null KnownConstraints)  
//          * in which case we'll *emit* the IR (yet still right-away position those flags and MetaIRValue, same as when const)
//      the typechecker itself will now be agnostic to all that, but can for example, query the constness of the result through the resulting flags.
// 
//  the do_add_or_sub will also, when not failed, and when not ensured decide to add a runtime check on the value
//

enum EOpSolveResult {
    IROPSOLVE_TOO_MANY_INSTRUCTIONS,
    IROPSOLVE_DIV_BY_ZERO,
    IROPSOLVE_RANGE_CHECK_FAILURE,
    IROPSOLVE_OPERAND_CANNOT_BE_NEGATIVE,
    IROPSOLVE_OK,
    IROPSOLVE_OK_AND_ENSURED_VALID,
};

enum EIntSemantics {
    EINT_SEMANTIC_MODULO_ARITH,
    EINT_SEMANTIC_CHK_UNSIGNED,
    EINT_SEMANTIC_CHK_SIGNED,
    EINT_SEMANTIC_CHK_MIXED,
};

// solves neg specifically between compints
local_func_inl EOpSolveResult ir_solve_neg_compint(const IRInfo& opA, IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    // compints have a special 'always-considered-embd' representation, even-if de facto possibly tagged pointers
    Assert_(opA._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opA._uFlags & IRFLAG_IS_COMPTIMEONLY);
    u64 uEncoding = opA._metaValue.knownValue.uEmbeddedValue;
    if (uEncoding == 0uLL) { // -0 => returns 0 (==A)
        *outResult = opA;
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    // out of special cases => prepare to solve nominal:
    outResult->_uIR = INVALID_IR_CODE;
    outResult->_metaValue.knownValue.uEmbeddedValue ^= 0x04uLL; // switches the sign flag
    outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|IRFLAG_IS_COMPTIMEONLY;
    return IROPSOLVE_OK_AND_ENSURED_VALID;
}

// solves add or sub specifically between compints
local_func EOpSolveResult ir_solve_add_or_sub_compint(const IRInfo& opA, const IRInfo& opB, bool bIsSub,
    IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    // compints have a special 'always-considered-embd' representation, even-if de facto possibly tagged pointers
    Assert_(opA._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opB._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opA._uFlags & IRFLAG_IS_COMPTIMEONLY);
    Assert_(opB._uFlags & IRFLAG_IS_COMPTIMEONLY);
    u64 uEncodingA = opA._metaValue.knownValue.uEmbeddedValue;
    u64 uEncodingB = opB._metaValue.knownValue.uEmbeddedValue;
    // Note: full zero encoding also represents value 0 when compint.
    if (uEncodingB == 0uLL) { // A +- 0 => returns A
        *outResult = opA;
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    if (uEncodingA == 0uLL) { // 0 +- B => returns B if op is add, or -B otherwise.
        if (!bIsSub) {
            *outResult = opB;
            return IROPSOLVE_OK_AND_ENSURED_VALID;
        } else {
            return ir_solve_neg_compint(opB, pRepo, pEvalContext, outResult);
        }
    }
    // out of special cases => prepare to solve nominal:
    outResult->_uIR = INVALID_IR_CODE;
    u64 uIsNegA = uEncodingA & 0x04uLL;
    u64 uIsNegB = uEncodingB & 0x04uLL;
    u64 uIsFinalNegB = bIsSub ? uIsNegB ^ 0x04uLL : uIsNegB;
    u64 uTagA = uEncodingA & 0x03uLL;
    u64 uTagB = uEncodingB & 0x03uLL;
    if (0 == uTagA && 0 == uTagB) { // both are embedded
        u64 uAbsValA = uEncodingA >> 3;
        u64 uAbsValB = uEncodingB >> 3;
        if (uIsNegA == uIsFinalNegB) {
            u64 uAbsValResult = uAbsValA + uAbsValB;
            if (uAbsValResult < 0x2000'0000'0000'0000uLL) {  // result is embeddable
                outResult->_metaValue.knownValue.uEmbeddedValue = (uAbsValResult << 3) | uIsNegA;
            } else {
                // TODO
                Assert(false, "ir_solve_add_or_sub_compint() : non-embd result not yet impl");
            }
        } else {
            if (uAbsValA > uAbsValB) {
                u64 uAbsValResult = uAbsValA - uAbsValB;
                outResult->_metaValue.knownValue.uEmbeddedValue = (uAbsValResult << 3) | uIsNegA;
            } else if (uAbsValA < uAbsValB) {
                u64 uAbsValResult = uAbsValB - uAbsValA;
                outResult->_metaValue.knownValue.uEmbeddedValue = (uAbsValResult << 3) | uIsFinalNegB;
            } else { // uAbsValA == uAbsValB
                outResult->_uIR = IR_REF_ZEROING_ANY;
                outResult->_metaValue.knownValue.uEmbeddedValue = 0uLL;
            }
        }
    } else {
        // TODO
        Assert(false, "ir_solve_add_or_sub_compint() : non-embd either operand not yet impl");
    }
    outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|IRFLAG_IS_COMPTIMEONLY;
    return IROPSOLVE_OK_AND_ENSURED_VALID;
}

// solves mul specifically between compints
local_func EOpSolveResult ir_solve_mul_compint(const IRInfo& opA, const IRInfo& opB,
    IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    // compints have a special 'always-considered-embd' representation, even-if de facto possibly tagged pointers
    Assert_(opA._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opB._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opA._uFlags & IRFLAG_IS_COMPTIMEONLY);
    Assert_(opB._uFlags & IRFLAG_IS_COMPTIMEONLY);
    u64 uEncodingA = opA._metaValue.knownValue.uEmbeddedValue;
    u64 uEncodingB = opB._metaValue.knownValue.uEmbeddedValue;
    // Note: full zero encoding also represents value 0 when compint.
    if (uEncodingA == 0uLL || uEncodingB == (1uLL << 3)) { // (A=0) * B, or A * (B=1) => returns A
        *outResult = opA;
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    if (uEncodingB == 0uLL || uEncodingA == (1uLL << 3)) { // (A=1) * B, or A * (B=0) => returns B
        *outResult = opB;
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    if (uEncodingA == ((1uLL << 3)|0x04uLL)) { // A==-1 => returns -B
        return ir_solve_neg_compint(opB, pRepo, pEvalContext, outResult);
    }
    if (uEncodingB == ((1uLL << 3)|0x04uLL)) { // B==-1 => returns -A
        return ir_solve_neg_compint(opA, pRepo, pEvalContext, outResult);
    }
    // out of special cases => prepare to solve nominal, knowning |A|>1, |B|>1
    outResult->_uIR = INVALID_IR_CODE;
    u64 uIsNegA = uEncodingA & 0x04uLL;
    u64 uIsNegB = uEncodingB & 0x04uLL;
    u64 uNegResult = uIsNegA ^ uIsNegB;
    u64 uTagA = uEncodingA & 0x03uLL;
    u64 uTagB = uEncodingB & 0x03uLL;
    if (0 == uTagA && 0 == uTagB) { // both are embedded
        u64 uAbsValA = uEncodingA >> 3;
        u64 uAbsValB = uEncodingB >> 3;
        u64 uAbsResultHigh; u64 uAbsResultLow = MulWithHigh64(uAbsValA, uAbsValB, &uAbsResultHigh);
        if (uAbsResultHigh == 0uLL && uAbsResultLow < 0x2000'0000'0000'0000uLL) {  // result is embeddable
            outResult->_metaValue.knownValue.uEmbeddedValue = (uAbsResultLow << 3) | uNegResult;
        } else {
            // TODO
            Assert(false, "ir_solve_mul_compint() : non-embd result not yet impl");
        }
    } else {
        // TODO
        Assert(false, "ir_solve_mul_compint() : non-embd either operand not yet impl");
    }
    outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|IRFLAG_IS_COMPTIMEONLY;
    return IROPSOLVE_OK_AND_ENSURED_VALID;
}

// solves integer quotient or remainder specifically between compints
local_func EOpSolveResult ir_solve_int_quo_or_rem_compint(const IRInfo& opA, const IRInfo& opB, bool bIsRem,
    IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    // compints have a special 'always-considered-embd' representation, even-if de facto possibly tagged pointers
    Assert_(opA._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opB._uFlags & IRFLAG_IS_CONST_EMBEDDED);
    Assert_(opA._uFlags & IRFLAG_IS_COMPTIMEONLY);
    Assert_(opB._uFlags & IRFLAG_IS_COMPTIMEONLY);
    u64 uEncodingA = opA._metaValue.knownValue.uEmbeddedValue;
    u64 uEncodingB = opB._metaValue.knownValue.uEmbeddedValue;
    // Note: full zero encoding also represents value 0 when compint.
    if (uEncodingB == 0uLL) {
        return EOpSolveResult::IROPSOLVE_DIV_BY_ZERO;
    }
    if (uEncodingA == 0uLL) {           // (A=0) /%,%% B, or A /% (B==1)  => returns A
        *outResult = opA;
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    u64 uIsNegA = uEncodingA & 0x04uLL;
    u64 uIsNegB = uEncodingB & 0x04uLL;
    if ((uEncodingB & ~0x04uLL) == (1uLL << 3)) {    // A /% (B=+-1) => returns A or -A, A %% (B=+-1) => returns 0
        if (bIsRem) {
            outResult->_uIR = IR_REF_ZEROING_ANY;
            outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|IRFLAG_IS_COMPTIMEONLY;
            outResult->_metaValue.knownValue.uEmbeddedValue = 0uLL;
        } else {
            if (uIsNegB) {
                return ir_solve_neg_compint(opA, pRepo, pEvalContext, outResult);
            } else {
                *outResult = opA;
            }
        }
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    if ((uEncodingA & ~0x04uLL) == (1uLL << 3)) {   // knowing |B|>1, (A=+-1) /% B => returns 0, (A=+-1) %% B => returns A (or -A if B neg)
        if (bIsRem) {
            if (uIsNegB) {
                return ir_solve_neg_compint(opA, pRepo, pEvalContext, outResult);
            } else
                *outResult = opA;
        } else {
            outResult->_uIR = IR_REF_ZEROING_ANY;
            outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|IRFLAG_IS_COMPTIMEONLY;
            outResult->_metaValue.knownValue.uEmbeddedValue = 0uLL;
        }
        return IROPSOLVE_OK_AND_ENSURED_VALID;
    }
    // out of special cases => prepare to solve nominal, knowning |A|>1, |B|>1
    outResult->_uIR = INVALID_IR_CODE;
    u64 uIsNegA = uEncodingA & 0x04uLL;
    u64 uIsNegB = uEncodingB & 0x04uLL;
    u64 uTagA = uEncodingA & 0x03uLL;
    u64 uTagB = uEncodingB & 0x03uLL;
    if (0 == uTagA && 0 == uTagB) { // both are embedded
        u64 uAbsValA = uEncodingA >> 3;
        u64 uAbsValB = uEncodingB >> 3;
        if (bIsRem) {
            u64 uAbsResult = uAbsValA % uAbsValB;
            u64 uIsNegResult = uAbsResult ? uIsNegB : 0uLL; // sign of remainder follows C remainder semantics. A == quo(A/B) * B + rem(A/B)
            outResult->_metaValue.knownValue.uEmbeddedValue = (uAbsResult << 3) | uIsNegResult;
        } else {
            u64 uAbsResult = uAbsValA / uAbsValB;
            u64 uIsNegResult = uIsNegA ^ uIsNegB;
            outResult->_metaValue.knownValue.uEmbeddedValue = (uAbsResult << 3) | uIsNegResult;
        }
    } else {
        // TODO
        Assert(false, "ir_solve_int_quo_or_rem_compint() : non-embd either operand not yet impl");
    }
    outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|IRFLAG_IS_COMPTIMEONLY;
    return IROPSOLVE_OK_AND_ENSURED_VALID;
}

local_func bool ir_check_equals_minint_known_embd_const(u64 uEmbdValue, u32 uFlags, u8 uFormat, IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    Assert_(uFormat <= 0x05u);  // allows all integral formats, from r8 to r256. But no other (esp. not compint)
    if (uFormat <= 3u) {
        u32 uSignBitPos = (8u << uFormat) - 1u;
        return uEmbdValue == 1uLL << uSignBitPos;
    } else {
        return false; // value of minint for i128 or i256 is not embedded.
    }
}
local_func bool ir_check_equals_minint_known_large_const(u64* tLegs, u8 uFormat, IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    Assert_(uFormat <= 0x05u);  // allows all integral formats, from r8 to r256. But no other (esp. not compint)
    Assert_(uFormat > 0x03u);   // ...but all integral formats up to 64b should have an embedded value
    Assert_(tLegs);
    if (uFormat == 0x04u) {
        return tLegs[0] == 0uLL && tLegs[1] == (1uLL << 63);
    } else { Assert_(uFormat == 0x05u);
        return tLegs[0] == 0uLL && tLegs[1] == 0uLL && tLegs[2] == 0uLL && tLegs[3] == (1uLL << 63);
    }
}
local_func bool ir_check_known_const_unsigned_strictly_above_max_sint_and_one(const IRInfo& opA, u8 uFormat, ) {
    Assert_(uFormat <= 0x05u);  // allows all integral formats, from r8 to r256. But no other (esp. not compint)
    Assert_(is_ir_flag_indicating_known_const(opA._uFlags));
    if (uFormat <= 0x03u) {
        Assert_(opA._uFlags & IRFLAG_IS_CONST_EMBEDDED);
        u64 uEmbdValue = opA._metaValue.knownValue.uEmbeddedValue;
        u32 uFormatBits = 8u << uFormat;
        u64 uMaxintAndOneForFormat = (1uLL << (uFormatBits-1u));
        return uEmbdValue > uMaxintAndOneForFormat;
    } else {
        if (opA._uFlags & IRFLAG_IS_CONST_EMBEDDED)
            return false;
        else if (uFormat == 0x04u) {            // 128b
            return opA._metaValue.knownValue.tLegs[1] > (1uLL << 63);
        } else { Assert_(uFormat == 0x05u);     // 256b
            return opA._metaValue.knownValue.tLegs[3] > (1uLL << 63);
        }
    }
}


// solves neg for runtime-compatible integral formats.
// 'outResult' usually carries flags and meta with an invalid ir code. in case it *does* carry some ir code, it means ir emission could be skipped.
local_func EOpSolveResult ir_solve_neg_integral(
    const IRInfo& opA, u8 uFormat, EIntSemantics eSemantics,
    IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    Assert_(uFormat <= 0x05u);  // allows all integral formats, from r8 to r256. But no other (esp. not compint : use specific ir_solve_neg_compint)
    Assert_(eSemantics != EIntSemantics::EINT_SEMANTIC_CHK_MIXED);    // 'mixed' has no meaning for an unop, right?
    Assert_(eSemantics != EIntSemantics::EINT_SEMANTIC_CHK_UNSIGNED); // this op not allowed for unsigned formats
    bool bKnownEmbdA = false;
    if (is_ir_flag_indicating_known_embedded_const(opA._uFlags)) {
        if (opA._metaValue.knownValue.uEmbeddedValue == 0uLL) { // -0 => returns 0 (as A)
            *outResult = opA;
            return IROPSOLVE_OK_AND_ENSURED_VALID;
        }
        bKnownEmbdA = true;
    }

    // out of special cases => prepare to solve nominal
    outResult->_uIR = INVALID_IR_CODE;
    if (is_ir_flag_indicating_known_const(opA._uFlags)) {

        if (bKnownEmbdA) {
            u64 uEmbdValue = opA._metaValue.knownValue.uEmbeddedValue;
            // 'semantic value' of -minint is **one above** maxint, for all our 2's complement formats => if not modulo arithmetics semantics:
            if (eSemantics == EIntSemantics::EINT_SEMANTIC_CHK_SIGNED &&
                    ir_check_equals_minint_known_embd_const(uEmbdValue, opA._uFlags, uFormat, pRepo, pEvalContext)) {
                return IROPSOLVE_RANGE_CHECK_FAILURE;  // out-of-range error in that specific case
            } // otherwise, we'll let -minint correctly wrap to itself

            if (uFormat > 0x03u) {
                u64 uSignBit = uEmbdValue & (1uLL << 63);
                u32 uSourceExpandsAsNeg = opA._uFlags & IRFLAG_IS_EMBD_EXPAND_INT_NEG;
                u64 uResultValue;
                if ((0 != uSignBit) == (0 != uSourceExpandsAsNeg)) {
                    outResult->_metaValue.knownValue.uEmbeddedValue = -uEmbdValue;
                    outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST_EMBEDDED|(uSourceExpandsAsNeg ^ IRFLAG_IS_EMBD_EXPAND_INT_NEG);
                } else {
                    Assert(false, "ir_solve_neg_integral : non-implemented for large embedded of larger-than 64b");
                }
            } else {
                if (uSignBit)
            }
            return IROPSOLVE_OK_AND_ENSURED_VALID;

        } else {
            // 'semantic value' of -minint is **one above** maxint, for all our 2's complement formats => if not modulo arithmetics semantics:
            if (eSemantics == EIntSemantics::EINT_SEMANTIC_CHK_SIGNED &&
                    ir_check_equals_minint_known_large_const(opA._metaValue.knownValue.tLegs, uFormat, pRepo, pEvalContext)) {
                return IROPSOLVE_RANGE_CHECK_FAILURE;  // out-of-range error in that specific case
            } // otherwise, we'll let -minint correctly wrap to itself

            // TODO
            Assert(false, "ir_solve_neg_integral() : non-embd not yet impl");
        }
    }

    // nothing could be said at this time about the result... => will need full runtime solving (or further analysis).
    outResult->_uFlags = 0;
    outResult->_metaValue.pConstraints = 0;
    return IROPSOLVE_OK;
}

// solves add or sub for runtime-compatible formats.
// 'outResult' usually carries flags and meta with an invalid ir code. in case it *does* carry some ir code, it means ir emission could be skipped.
local_func EOpSolveResult ir_solve_add_or_sub_integral(
    const IRInfo& opA, const IRInfo& opB, u8 uFormat, bool bIsSub, EIntSemantics eSemantics,
    IRRepo* pRepo, CompilationContext* pEvalContext, IRInfo* outResult)
{
    Assert_(uFormat <= 0x05u);  // allows all integral formats, from r8 to r256. But no other (esp. not compint : use specific ir_solve_add_or_sub_compint)
    Assert_(!is_ir_flag_indicating_nyka(opA._uFlags) && !is_ir_flag_indicating_nyka(opB._uFlags)); // nykas should take another path
    bool bKnownEmbdA = false;
    bool bKnownEmbdB = false;
    if (is_ir_flag_indicating_known_embedded_const(opB._uFlags)) {
        if (opB._metaValue.knownValue.uEmbeddedValue == 0uLL) { // A +- 0 => returns A
            *outResult = opA;
            return IROPSOLVE_OK_AND_ENSURED_VALID;
        }
        bKnownEmbdB = true;
    }
    if (is_ir_flag_indicating_known_embedded_const(opA._uFlags)) { // 0 +- B
        if (opA._metaValue.knownValue.uEmbeddedValue == 0uLL) { // A +- 0 => returns A
            if (!bIsSub) {
                *outResult = opB;
            } else {
                // resulting in the negative of B... when we know B>0
                if (eSemantics == EIntSemantics::EINT_SEMANTIC_CHK_UNSIGNED) // => an error for 'unsigned' semantics
                    return IROPSOLVE_RANGE_CHECK_FAILURE;
                else if (eSemantics == EIntSemantics::EINT_SEMANTIC_CHK_MIXED) { // 'mixed' means B is to be taken as unsigned
                    // => shall return the neg of B indeed, provided it is not *strictly above* the MAXINT+1 (for its format)
                    if (is_ir_flag_indicating_known_const(opB._uFlags) &&
                            ir_check_known_const_unsigned_strictly_above_maxsint_and_one(opB, uFormat, pRepo, pEvalContext)) {
                        return IROPSOLVE_RANGE_CHECK_FAILURE;
                    }
                    // and since we checked there for input range, it should never fail the 'neg' now
                    eSemantics = EIntSemantics::EINT_SEMANTIC_MODULO_ARITH; // ...(even for when == -minint raw) => switch to modulo for neg
                }
                return ir_solve_neg_integral(opB, uFormat, eSemantics, pRepo, pEvalContext, outResult);
            }
        }
        bKnownEmbdA = true;
    }

    outResult->_uIR = INVALID_IR_CODE;
    if (bKnownEmbdA && bKnownEmbdB) {
        u64 uEmbdA = opB._metaValue.knownValue.uEmbeddedValue;
        u64 uEmbdB = opB._metaValue.knownValue.uEmbeddedValue;
        if (uFormat <= 0x03u) {
            u64 uCBatSignPosForFormat;
            u64 uResult64;
            u64 uFormatMask;
            u32 uFormatBits = 8u << uFormat;
            if (uFormat == 0x03u) {
                u8 uCB;
                uResult64 = bIsSub ? SubBorrow64(uEmbdA, uEmbdB, 0u, &uCB) : AddCarry64(uEmbdA, uEmbdB, 0u, &uCB);
                uFormatMask = 0xFFFF'FFFF'FFFF'FFFFuLL;
            } else {
                uResult64 = bIsSub ? uEmbdA - uEmbdB : uEmbdA + uEmbdB;
                uCBatSignPosForFormat = uResult64 >> 1u;
                uFormatMask = (1uLL << uFormatBits) - 1uLL;
            }
            outResult->_uFlags = IRFLAG_IS_CONST|IRFLAG_IS_CONST;
            outResult->_metaValue.knownValue.uEmbeddedValue = uResult & uFormatMask;
            u32 uSignShift = uFormatBits-1u;
            switch (eSemantics) {
                case EIntSemantics::EINT_SEMANTIC_CHK_UNSIGNED:
                    // if carry or borrow set, it is the sign of an overflow for unsigned semantics
                    if (uCBatSignPosForFormat & (1uLL<<uSignShift))
                        return IROPSOLVE_RANGE_CHECK_FAILURE;
                    break;

                case EINT_SEMANTIC_CHK_SIGNED: {
                    // error cases for signed semantics
                    //      For Adder == when (signA == signB) *And* (signR != carry out)
                    //      For Subtracter == when (signA != signB) *And* (signR == borrow out)
                    u64 checkBoth;
                    if (bIsSub) {
                        u64 a_neq_b = uEmbdA ^ uEmbdB;
                        u64 r_eq_c = ~(uResult64 ^ uCBatSignPosForFormat);
                        checkBoth = a_neq_b & r_eq_c;
                    } else {
                        u64 a_eq_b = ~(uEmbdA ^ uEmbdB);
                        u64 r_neq_c = uResult64 ^ uCBatSignPosForFormat;
                        checkBoth = a_eq_b & r_neq_c;
                    }
                    if (checkBoth & (1uLL<<uSignShift)) // we here use the fact that 'sign(x) bitwiseop sign(y)' == 'sign(x bitwiseop y)'
                        return IROPSOLVE_RANGE_CHECK_FAILURE;
                } break;

                case EINT_SEMANTIC_CHK_SIGNED: {
                    // error cases for mixed semantics (ie when A and result have signed semantics, but B is considered *unsigned*)
                    //      For Adder == ((~SA)|SR) & (SB|(SR^SA))
                    //      For Subtracter == (SA|(~SR)) & (SB|(SR^SA))
                    u64 b_or_r_neq_a = (uEmbdB|(uResult64^uEmbdA));
                    u64 checkBoth;
                    if (bIsSub) {
                        u64 a_or_nr = uEmbdA|(~uResult64);
                        checkBoth = a_or_nr & b_or_r_neq_a;
                    } else {
                        u64 na_or_r = (~uEmbdA)|uResult64;
                        checkBoth = na_or_r & b_or_r_neq_a;
                    }
                    if (checkBoth & (1uLL<<uSignShift)) // we here use the fact that 'sign(x) bitwiseop sign(y)' == 'sign(x bitwiseop y)'
                        return IROPSOLVE_RANGE_CHECK_FAILURE;
                } break;

            }
            return IROPSOLVE_OK_AND_ENSURED_VALID;
        }
    } else if () {
    }
    } else if (is_ir_flag_indicating_known_const(opA._uFlags) && is_ir_flag_indicating_known_const(opB._uFlags)) {

        if (uFormat == 0x04u) { // 128b

        } else {

        }
        return IROPSOLVE_OK_AND_ENSURED_VALID;

    } else if (is_ir_flag_indicating_nyka(opA._uFlags) && is_ir_flag_indicating_known_const(opB._uFlags)) {
        // TODO: take carry/borrow flag into account... AND op kind (add vs sub) to decide of the extension to 128 or 256
        Assert(false, "ir_solve_add_or_sub_integral() : not yet implemented with nyka +- const...");
    } else if (is_ir_flag_indicating_known_const(opA._uFlags) && is_ir_flag_indicating_nyka(opB._uFlags) && !bIsSub) {
        // TODO: take carry/borrow flag into account... AND op kind (add vs sub) to decide of the extension to 128 or 256
        Assert(false, "ir_solve_add_or_sub_integral() : not yet implemented with const + nyka...");
    }

    // nothing could be said at this time about the result... => will need full runtime solving (or further analysis).
    outResult->_uFlags = 0;
    outResult->_metaValue.pConstraints = 0;
    return IROPSOLVE_OK;
}

#endif // TMP TMP

#endif // LOCLIB_IR_SOLVER_H_

