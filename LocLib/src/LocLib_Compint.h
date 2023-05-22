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

#ifndef LOCLIB_COMPINT_H_
#define LOCLIB_COMPINT_H_

#include "LocLib_TypeInfoDecls.h"
#include "../../HighPerfTools/arithmetic_operations.h"
#include "../../HighPerfTools/legs64_core.h"

#define COMPNAT_MAX_LEGS	255

struct CompNat {
	i32 iLegCount;
	u32 _pad0;
	u64 tLegs[0];
};

struct EmbdAsCompNat {
	i32 iLegCount;
	u32 _pad0;
	u64 tLegs[1];
};

struct TwoLeggeedAsCompNat {
	i32 iLegCount;
	u32 _pad0;
	u64 tLegs[2];
};

local_func_inl EmbdAsCompNat compnat_from_non_zero_embedded(u64 uEmbeddedValue) {
	EmbdAsCompNat result;
    result.iLegCount = 1; result.tLegs[0] = uEmbeddedValue;
    return result;
}

local_func_inl TwoLeggeedAsCompNat compnat_from_two_legged(u64 uNonZeroHigh, u64 uLow) {
	TwoLeggeedAsCompNat result;
    result.iLegCount = 2; result.tLegs[0] = uLow;  result.tLegs[1] = uNonZeroHigh;
    return result;
}

local_func_inl bool compnat_are_equal(CompNat* pA, CompNat* pB) {
	i32 iLegCount = pA->iLegCount;
	return (iLegCount == pB->iLegCount) && equal_leg64(pA->tLegs, pB->tLegs, iLegCount);
}

local_func_inl bool compnat_is_strict_less(CompNat* pA, CompNat* pB) {
	i32 iLegCount = pA->iLegCount;
	if (iLegCount < pB->iLegCount)
		return true;
	else if (iLegCount > pB->iLegCount)
		return false;
	else
		return lesser_than_legs64(pA->tLegs, pB->tLegs, iLegCount);
}

local_func_inl int compnat_cmp(CompNat* pA, CompNat* pB) {
	i32 iLegCount = pA->iLegCount;
	if (iLegCount < pB->iLegCount)
		return -1;
	else if (iLegCount > pB->iLegCount)
		return +1;
	else {
        return cmp_leg64(pA->tLegs, pB->tLegs, iLegCount);
    }
}

local_func CompNat* compnat_add_larger_first(CompNat* pA, CompNat* pB, CompilationContext* pEvalContext, Arena resultArena) {
	i32 iMaxLegs = pA->iLegCount + 1;
	CompNat* pResult = (CompNat*)alloc_from(resultArena, sizeof(u64) * (iMaxLegs + 1), alignof(CompNat));
    copy_legs_to(pResult->tLegs, pA->tLegs, pA->iLegCount);
    add_inplace_legs_64(pResult->tLegs, pA->iLegCount, pB->tLegs, pB->iLegCount);
	pResult->iLegCount = find_highest_significant_leg64(pResult->tLegs, iMaxLegs) + 1;
	return pResult;
}

local_func_inl CompNat* compnat_add(CompNat* pA, CompNat* pB, CompilationContext* pEvalContext, Arena resultArena) {
	CompNat* pLarger; CompNat* pOther;
	if (pA->iLegCount >= pB->iLegCount) {
		pLarger = pA; pOther = pB;
	} else {
		pLarger = pB; pOther = pA;
	}
	return compnat_add_larger_first(pLarger, pOther, pEvalContext, resultArena);
}

local_func CompNat* compnat_sub_from_larger(CompNat* pA, CompNat* pB, CompilationContext* pEvalContext, Arena resultArena) {
	i32 iMaxLegs = pA->iLegCount;
	CompNat* pResult = (CompNat*)alloc_from(resultArena, sizeof(u64) * (iMaxLegs + 1), alignof(CompNat));
    copy_legs_to(pResult->tLegs, pA->tLegs, pA->iLegCount);
    sub_inplace_legs_64(pResult->tLegs, pA->iLegCount, pB->tLegs, pB->iLegCount);
	pResult->iLegCount = find_highest_significant_leg64(pResult->tLegs, iMaxLegs) + 1;
	return pResult;
}

local_func CompNat* compnat_mul(CompNat* pA, CompNat* pB, CompilationContext* pEvalContext, Arena resultArena) {
	i32 iMaxLegs = pA->iLegCount + pB->iLegCount;
	CompNat* pResult = (CompNat*)alloc_from(resultArena, sizeof(u64) * (iMaxLegs + 1), alignof(CompNat));
	naive_mul_legs_64_to(pResult->tLegs, pA->tLegs, pA->iLegCount, pB->tLegs, pB->iLegCount);
	pResult->iLegCount = find_highest_significant_leg64(pResult->tLegs, iMaxLegs) + 1;
	return pResult;
}

// Divides two compnats A and B, returns A /% B (and additionnal outparam A %% B), when A >= B.
// Note: Our requirement is in fact that A has at least same leg count than B. However, caller would be well-advised to
//   handle the whole 'A < B' case separately anyway (could decide on allocs of 'remainder == A' better than we could do
//   here, for example)... and by the way, the 'A == B' case could also benefit from a separate path.
local_func CompNat* compnat_div_rem_when_no_smaller(CompNat* pA, CompNat* pB, CompilationContext* pEvalContext, CompNat** outRem,
	Arena quoArena, Arena remArena)
{
	i32 iCountB	= pB->iLegCount;
	i32 iCountA = pA->iLegCount;
	Assert(iCountA >= iCountB, "'_when_no_smaller' is in the name...");

    // Already 16K on stack here if COMPNAT_MAX_LEGS was 1023
	static_assert(COMPNAT_MAX_LEGS < 1024, "COMPNAT_MAX_LEGS maybe a little too large for our stack-alloc temporaries during division");
	u64 tLegsDividendAndRem[COMPNAT_MAX_LEGS+1];
	u64 tLegsDivisor[COMPNAT_MAX_LEGS];

	i32 iMaxDividend = iCountA + 1;

    copy_legs_to(tLegsDivisor, pB->tLegs, iCountB);
	copy_legs_to(tLegsDividendAndRem, pA->tLegs, iCountA);
	tLegsDividendAndRem[iCountA] = 0;

	int iMSLegB;
	int iBitShift = left_justify_legs_64(tLegsDivisor, iCountB, &iMSLegB);
	// no leg from A or B should be 'trimmed high', since 'CompNat::iLegCount' should be the count of significant already
	Assert_(iMSLegB == iCountB-1);
    int iStartB = find_lowest_significant_leg64(tLegsDivisor, iCountB);
    int iTrimmedCountB = iCountB - iStartB;

    u64 expect0 = left_shift_legs_64_bitonly_by(iBitShift, tLegsDividendAndRem, iMaxDividend);
    Assert_(0 == expect0);

	int iQuotientLegs = iMaxDividend - iMSLegB; // base config
	iQuotientLegs -= iStartB;					// remove one per trimmed low divisor
    // We may remove a further quotient leg if shifted dividend has finally no bits in its additional high leg
    if (0 == tLegsDividendAndRem[iCountA])
        iQuotientLegs--;
	Assert_(iQuotientLegs);
	
    CompNat* pQuotient = (CompNat*)alloc_from(quoArena, sizeof(u64) * (iQuotientLegs + 1), alignof(CompNat));

    // => We're now ready to call our long-division algorithm:
    long_div_leg64_to(pQuotient->tLegs, tLegsDividendAndRem + iStartB, iQuotientLegs, tLegsDivisor + iStartB, iTrimmedCountB);
    // after that call:
    //   in tQuotientLegs is already the desired quotient
    //   in tDividendAndRem, we should find the remainder of the *shifted* representation
    Assert_(are_all_zero_leg64(tLegsDividendAndRem + iCountB, iMaxDividend - iCountB));
    //   => compensate its shift, by applying this time a right-shift on it:
    expect0 = right_shift_legs_64_bitonly_by(iBitShift, tLegsDividendAndRem, iCountB);
    Assert_(0 == expect0);
    Assert_(lesser_than_legs64(tLegsDividendAndRem, pB->tLegs, iCountB));

    pQuotient->iLegCount = find_highest_significant_leg64(pQuotient->tLegs, iQuotientLegs) + 1;
    int iActualRemLegCount = find_highest_significant_leg64(tLegsDividendAndRem, iCountB) + 1;
    *outRem = (CompNat*)alloc_from(remArena, sizeof(u64) * (iActualRemLegCount + 1), alignof(CompNat));
    copy_legs_to((*outRem)->tLegs, tLegsDividendAndRem, iActualRemLegCount);
    (*outRem)->iLegCount = iActualRemLegCount;
    return pQuotient;
}

local_func i32 get_bin_char_count(CompNat* pCompNat) {
    i32 iLegCount = pCompNat->iLegCount;
    i32 iLastIndex = iLegCount-1;
    u64 uHighestLeg = pCompNat->tLegs[iLastIndex];
    i32 iPosHigh = GetPosOfMostSignificantBitSet64(uHighestLeg);
    return iLastIndex*64 + iPosHigh + 1;
}
local_func i32 get_hex_char_count(CompNat* pCompNat) {
    i32 iCountBits = get_bin_char_count(pCompNat);
    return (iCountBits+3) >> 2;
}
local_func i32 get_overestim_dec_char_count(CompNat* pCompNat) {
    i32 iCountBits = get_bin_char_count(pCompNat);
    i32 iApproxPowerOfTen = ((iCountBits+9) / 10) * 3;
    return iApproxPowerOfTen + 1 + (iCountBits / 970);
}

// Emit the binary representation of a single 64b leg in ASCII, to a buffer of bytes.
// Note: We won't bother to specialize distinct versions depending on whether to insert optional chars each 4b and/or 16b for binary
//   => we let binary leg emission take the hit of those 'ifs' ;
//   it is expected that binary emission is never really on critical paths anyway.
local_func int emit_leg_bin_to(u8* pBuffer, u64 uLeg, u8 optCharBetween16b, u8 optCharBetween4b, i32 iMSBPos = 63)
{
    int iEmitted = 0;
    for (int iBitPos = iMSBPos; iBitPos >= 0; iBitPos--) {
        u8 uBit = u8(uLeg >> iBitPos) & 0x01u;
        pBuffer[iEmitted] = uBit + u8('0');
        iEmitted++;
        if (0 == (iBitPos & 0x03) && iBitPos) {
            if (0 == (iBitPos & 0x0F)) {
                if (optCharBetween16b) {
                    pBuffer[iEmitted] = optCharBetween16b;
                    iEmitted++;
                }
            } else {
                if (optCharBetween4b) {
                    pBuffer[iEmitted] = optCharBetween4b;
                    iEmitted++;
                }
            }
        }
    }
    return iEmitted;
}

// Emit the binary representation of a CompNat in ASCII, to a buffer of bytes.
local_func int emit_bin_to(u8* pBuffer, CompNat* pCompNat, int iMinChars = 0, bool bSpacesToMin = true,
    u8 optCharBetweenLegs = u8('.'), u8 optCharBetween16b = u8('\''), u8 optCharBetween4b = 0,
    u8 optLeadingChar = 0, bool bOnlyFullLegs = false)
{
    int iRawBitCount = get_bin_char_count(pCompNat);
    Assert_(iRawBitCount > 0);
    if (bOnlyFullLegs)
        iRawBitCount = (iRawBitCount + 63) & ~0x3F;
    int iBitCountWithOpt = iRawBitCount;
    int iLegCount = pCompNat->iLegCount;
    Assert_(iLegCount == ((iRawBitCount + 63) >> 6));
    int i16bCount = ((iRawBitCount + 15) >> 4);
    int i4bCount = ((iRawBitCount + 3) >> 2);

    if (optCharBetween16b == 0)
        optCharBetween16b = optCharBetween4b;
    if (optCharBetweenLegs == 0)
        optCharBetweenLegs = optCharBetween16b;

    if (optCharBetweenLegs)
        iBitCountWithOpt += iLegCount - 1;
    if (optCharBetween16b)
        iBitCountWithOpt += i16bCount - 1 - iLegCount;
    if (optCharBetween4b)
        iBitCountWithOpt += i4bCount - 1 - iLegCount - i16bCount;
    if (optLeadingChar)
        iBitCountWithOpt++;

    if (iBitCountWithOpt < iMinChars) {
        // TODO
        Assert(false, "emit_bin_to() : filling up to min chars not yet implemented");
    }

    int iEmitted = 0;
    if (optLeadingChar) { // TODO once we fill: leading char comes before zero-fills, but *after* space-fills
        pBuffer[0] = optLeadingChar;
        iEmitted++;
    }
    i32 iMSLegPos = iLegCount-1;
    u64 uMSLeg = pCompNat->tLegs[iMSLegPos];
    i32 iFirstMSBPos = (iRawBitCount - 1) & 0x3F;
    iEmitted += emit_leg_bin_to(pBuffer + iEmitted, uMSLeg, optCharBetween16b, optCharBetween4b, iFirstMSBPos);

    if (optCharBetweenLegs) {
        for (i32 iLeg = iMSLegPos-1; iLeg>0; iLeg--) {
            pBuffer[iEmitted] = optCharBetweenLegs;
            iEmitted++;
            iEmitted += emit_leg_bin_to(pBuffer + iEmitted, pCompNat->tLegs[iLeg], optCharBetween16b, optCharBetween4b);
        }
        iEmitted += emit_leg_bin_to(pBuffer + iEmitted, pCompNat->tLegs[0], optCharBetween16b, optCharBetween4b);
    } else {
        for (i32 iLeg = iMSLegPos-1; iLeg>=0; iLeg--) {
            iEmitted += emit_leg_bin_to(pBuffer + iEmitted, pCompNat->tLegs[iLeg], optCharBetween16b, optCharBetween4b);
        }
    }

    return iEmitted;
}

constexpr u8 tHexToCharLower[] = { u8('0'), u8('1'), u8('2'), u8('3'), u8('4'), u8('5'), u8('6'), u8('7'), u8('8'), u8('9'),
                                   u8('a'), u8('b'), u8('c'), u8('d'), u8('e'), u8('f') };
constexpr u8 tHexToCharUpper[] = { u8('0'), u8('1'), u8('2'), u8('3'), u8('4'), u8('5'), u8('6'), u8('7'), u8('8'), u8('9'),
                                   u8('A'), u8('B'), u8('C'), u8('D'), u8('E'), u8('F') };

// Emit the hexadecimal representation of a single 64b leg in ASCII, to a buffer of bytes.
// Only two cases for hex 'optional chars' within hex legs : with, or without
// => contrary to binary, we specialize here between a version with optional char and a version without
local_func int emit_leg_hex_to(u8* pBuffer, u64 uLeg, const u8* tHexToChar, u8 uCharBetween4h, i32 iMSHPos = 15)
{
    Assert(uCharBetween4h, "emit_leg_hex_to() : should use the specialized version without separator instead of having 0 here");
    int iEmitted = 0;
    for (int iHexPos = iMSHPos; iHexPos >= 0; iHexPos--) {
        u8 uHex = u8(uLeg >> (iHexPos*4)) & 0x0Fu;
        pBuffer[iEmitted] = tHexToChar[uHex];
        iEmitted++;
        if (0 == (iHexPos & 0x03) && iHexPos) {
            pBuffer[iEmitted] = uCharBetween4h;
            iEmitted++;
        }
    }
    return iEmitted;
}

// Emit the hexadecimal representation of a single 64b leg in ASCII, to a buffer of bytes.
// ...this is the version without an optional char every 4 hex
local_func int emit_leg_hex_to(u8* pBuffer, u64 uLeg, const u8* tHexToChar, i32 iMSHPos = 15)
{
    int iEmitted = 0;
    for (int iHexPos = iMSHPos; iHexPos >= 0; iHexPos--) {
        u8 uHex = u8(uLeg >> (iHexPos*4)) & 0x0Fu;
        pBuffer[iEmitted] = tHexToChar[uHex];
        iEmitted++;
    }
    return iEmitted;
}

// Emit the hexadecimal representation of a CompNat in ASCII, to a buffer of bytes.
local_func int emit_hex_to(u8* pBuffer, CompNat* pCompNat, bool bLowerCase, int iMinChars = 0, bool bSpacesToMin = true,
    u8 optCharBetweenLegs = u8('.'), u8 optCharBetween4h = u8('\''), u8 optLeadingChar = 0, bool bOnlyFullLegs = false)
{
    int iRawHexCount = get_hex_char_count(pCompNat);
    Assert_(iRawHexCount > 0);
    if (bOnlyFullLegs)
        iRawHexCount = (iRawHexCount + 15) & ~0x0F;
    int iHexCountWithOpt = iRawHexCount;
    int iLegCount = pCompNat->iLegCount;
    Assert_(iLegCount == ((iRawHexCount + 15) >> 4));
    int i4hCount = ((iRawHexCount + 3) >> 2);

    if (optCharBetweenLegs == 0)
        optCharBetweenLegs = optCharBetween4h;

    if (optCharBetweenLegs)
        iHexCountWithOpt += iLegCount - 1;
    if (optCharBetween4h)
        iHexCountWithOpt += i4hCount - 1 - iLegCount;
    if (optLeadingChar)
        iHexCountWithOpt++;

    if (iHexCountWithOpt < iMinChars) {
        // TODO
        Assert(false, "emit_hex_to() : filling up to min chars not yet implemented");
    }

    const u8* pHexToChar = bLowerCase ? tHexToCharLower : tHexToCharUpper;

    int iEmitted = 0;
    if (optLeadingChar) { // TODO once we fill: leading char comes before zero-fills, but *after* space-fills
        pBuffer[0] = optLeadingChar;
        iEmitted++;
    }
    i32 iMSLegPos = iLegCount-1;
    u64 uMSLeg = pCompNat->tLegs[iMSLegPos];
    i32 iFirstMSHexPos = (iRawHexCount - 1) & 0x0F;
    if (optCharBetween4h) {
        iEmitted += emit_leg_hex_to(pBuffer + iEmitted, uMSLeg, pHexToChar, optCharBetween4h, iFirstMSHexPos);
        if (optCharBetweenLegs) {
            for (i32 iLeg = iMSLegPos-1; iLeg>0; iLeg--) {
                pBuffer[iEmitted] = optCharBetweenLegs;
                iEmitted++;
                iEmitted += emit_leg_hex_to(pBuffer + iEmitted, pCompNat->tLegs[iLeg], pHexToChar, optCharBetween4h);
            }
            iEmitted += emit_leg_hex_to(pBuffer + iEmitted, pCompNat->tLegs[0], pHexToChar, optCharBetween4h);
        } else {
            for (i32 iLeg = iMSLegPos-1; iLeg>=0; iLeg--) {
                iEmitted += emit_leg_hex_to(pBuffer + iEmitted, pCompNat->tLegs[iLeg], pHexToChar, optCharBetween4h);
            }
        }
    } else {
        iEmitted += emit_leg_hex_to(pBuffer + iEmitted, uMSLeg, pHexToChar, iFirstMSHexPos);
        if (optCharBetweenLegs) {
            for (i32 iLeg = iMSLegPos-1; iLeg>0; iLeg--) {
                pBuffer[iEmitted] = optCharBetweenLegs;
                iEmitted++;
                iEmitted += emit_leg_hex_to(pBuffer + iEmitted, pCompNat->tLegs[iLeg], pHexToChar);
            }
            iEmitted += emit_leg_hex_to(pBuffer + iEmitted, pCompNat->tLegs[0], pHexToChar);
        } else {
            for (i32 iLeg = iMSLegPos-1; iLeg>=0; iLeg--) {
                iEmitted += emit_leg_hex_to(pBuffer + iEmitted, pCompNat->tLegs[iLeg], pHexToChar);
            }
        }
    }

    return iEmitted;
}

local_func int emit_decimal_single_leg_reverse_to(u8* pBuffer, u64 uLeg, i32* ioEmittedDigits,
    u8 optCharBetweenThousands, u8 optCharBetweenBillions, u8 optCharBetweenOCtillions, bool bUpToMax)
{
    i32 iEmittedDigits = *ioEmittedDigits;
    i32 iEmittedDigitsAtStart = iEmittedDigits;
    u64 uRemainingToEmit = uLeg;
    int iEmitted = 0;

    if (0 == iEmittedDigits)
        goto after_sep;

    do {
        if (0 == iEmittedDigits % 3) {
            if (0 == iEmittedDigits % 27) {
                if (optCharBetweenOCtillions) {
                    pBuffer[iEmitted] = optCharBetweenOCtillions;
                    iEmitted++;
                }
            } else if (0 == iEmittedDigits % 9) {
                if (optCharBetweenBillions) {
                    pBuffer[iEmitted] = optCharBetweenBillions;
                    iEmitted++;
                }
            } else {
                if (optCharBetweenThousands) {
                    pBuffer[iEmitted] = optCharBetweenBillions;
                    iEmitted++;
                }
            }
        }

    after_sep:
        u8 uDigit = uRemainingToEmit % 10;
        uRemainingToEmit = uRemainingToEmit / 10;
        pBuffer[iEmitted] = uDigit + u8('0');
        iEmitted++;
        iEmittedDigits++;

    } while (uRemainingToEmit || (bUpToMax && (iEmittedDigits-iEmittedDigitsAtStart) < 19));

    *ioEmittedDigits = iEmittedDigits;
    return iEmitted;
}

local_func_inl void swap_byte_positions_highs_with_lows(u8* pBuffer, int iBytes) {
    Assert_(iBytes > 0);
    int iHalfCount = iBytes >> 1;
    u8* pLastByte = pBuffer + (iBytes-1);
    for (int i = 0; i<iHalfCount; i++) {
        u8* pLow = pBuffer + i;
        u8* pHigh = pLastByte - i;
        u8 uTmpForSwap = *pLow;
        *pLow = *pHigh;
        *pHigh = uTmpForSwap;
    }
}

// Emit the decimal representation of a CompNat in ASCII, to a buffer of bytes.
local_func int emit_dec_to(u8* pBuffer, CompNat* pCompNat, int iMinChars = 0, bool bSpacesToMin = true,
    u8 optCharBetweenThousands = u8(','), u8 optCharBetweenBillions = u8('_'),
    u8 optCharBetweenOctillions = 0, u8 optLeadingChar = 0, bool bOnlyFullLegs = false)
{
    // we'll chose to emit 'in reverse', then swap each single-byte digit.
    int iLegCount = pCompNat->iLegCount;

    if (optCharBetweenBillions == 0)
        optCharBetweenBillions = optCharBetweenThousands;
    if (optCharBetweenOctillions == 0)
        optCharBetweenOctillions = optCharBetweenBillions;

    int iEmitted=0;
	u64 tTmp1[COMPNAT_MAX_LEGS];
	u64 tTmp2[COMPNAT_MAX_LEGS];
    copy_legs_to(tTmp1, pCompNat->tLegs, iLegCount);
    i32 iLegsToStillEmit = iLegCount;
    u64* pCurrentToStillEmit = tTmp1;
    u64* pNextQuotient = tTmp2;
    u64 uSingleLeggedDivisor = 10'000'000'000'000'000'000uLL;
    Assert_(uSingleLeggedDivisor & 0x8000'0000'0000'0000uLL);
    int iEmittedDigits = 0;
    while (iLegsToStillEmit > 1) {
        semi_long_div_leg64_to(pNextQuotient, pCurrentToStillEmit, iLegsToStillEmit, uSingleLeggedDivisor);
        Assert_(are_all_zero_leg64(pCurrentToStillEmit + 1, iLegCount-1));
        Assert_(pCurrentToStillEmit[0] < uSingleLeggedDivisor);
        iEmitted += emit_decimal_single_leg_reverse_to(pBuffer + iEmitted, pCurrentToStillEmit[0],
            &iEmittedDigits, optCharBetweenThousands, optCharBetweenBillions, optCharBetweenOctillions, true);
        iLegsToStillEmit = find_highest_significant_leg64(pNextQuotient, iLegsToStillEmit) + 1;
        u64* pTmpForSwap = pCurrentToStillEmit;
        pCurrentToStillEmit = pNextQuotient;
        pNextQuotient = pTmpForSwap;
    }
    iEmitted += emit_decimal_single_leg_reverse_to(pBuffer + iEmitted, pCurrentToStillEmit[0],
        &iEmittedDigits, optCharBetweenThousands, optCharBetweenBillions, optCharBetweenOctillions, false);

    int iAddCharForLeading = optLeadingChar ? 1 : 0;
    if (iEmitted + iAddCharForLeading < iMinChars) {
        if (bSpacesToMin) {
            if (optLeadingChar) {
                pBuffer[iEmitted] = optLeadingChar;
                iEmitted++;
            }
            while (iEmitted < iMinChars) {
                pBuffer[iEmitted] = u8(' ');
                iEmitted++;
            }
        } else {
            // TODO: this one should not be very hard
            Assert(false, "emit_dec_to() : filling up to min chars with zeroes not yet implemented"); 
        }
    } else {
        if (optLeadingChar) {
            pBuffer[iEmitted] = optLeadingChar;
            iEmitted++;
        }
    }
    swap_byte_positions_highs_with_lows(pBuffer, iEmitted);
    return iEmitted;
}

#endif // LOCLIB_COMPINT_H_


