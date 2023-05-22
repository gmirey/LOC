// Part of LocLang/HighPerfTools
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

#include "BaseDecls.h"

// *****************************************************
// Unsigned-integer arbitrary-length with 64b-legs support
// *****************************************************

// Resets all legs to 0
local_func_inl void zero_out_legs(u64* tLegs, int iLegsCount)
{
    for (int i=0; i<iLegsCount; i++)
        tLegs[i] = 0;
}

// Copys given amount of legs from In to Out. Leg arrays shall not overlap, or overlap such that tLegsOut < tLegsIn
local_func_inl void copy_legs_to(u64* tLegsOut,
                                      const u64* tLegsIn, int iLegsCount)
{
    for (int i=0; i<iLegsCount; i++)
        tLegsOut[i] = tLegsIn[i];
}

// Copys given amount of legs from In to Out in reverse, so that one can copy with overlap when tLegsOut > tLegsIn
local_func_inl void copy_legs_in_reverse_to(u64* tLegsOut,
                                                 const u64* tLegsIn, int iLegsCount)
{
    for (int i=iLegsCount-1; i>=0; i--)
        tLegsOut[i] = tLegsIn[i];
}

// Perform two's complement transform inplace. Return carry (0 or 1... 1 meaning negativity of end-result).
local_func_inl u8 two_complement_legs64(u64* tLegsIO, int iLegsCount)
{
    u8 carry;
    tLegsIO[0] = AddCarry64(1uLL, ~(tLegsIO[0]), 0, &carry);
    for (int i=1; i<iLegsCount; i++)
        tLegsIO[i] = AddCarry64(0uLL, ~(tLegsIO[i]), carry, &carry);
    return carry;
}

// Adds legs of B to legs of A into Legs of R, when all have same leg-count. Return carry (0 or 1)
local_func_inl u8 add_carry_legs_64_to(u64* tLegsR,
                                            const u64* tLegsA, const u64* tLegsB, int iLegsCount)
{
    u8 carry = 0;
    for (int i=0; i<iLegsCount; i++)
        tLegsR[i] = AddCarry64(tLegsA[i], tLegsB[i], carry, &carry);
    return carry;
}

// Adds legs of B to legs of A (inplace on A). leg count on A shall be at least as large as legs count on B. Return carry (0 or 1)
local_func_inl u8 add_inplace_legs_64(u64* tLegsA, int iLegsCountA,
                                           const u64* tLegsB, int iLegsCountB)
{
    u8 carry = 0;
    int i=0;
    for ( ; i<iLegsCountB; i++)
        tLegsA[i] = AddCarry64(tLegsA[i], tLegsB[i], carry, &carry);
    for ( ; carry && i<iLegsCountA; i++)
        tLegsA[i] = AddCarry64(tLegsA[i], 0uLL, carry, &carry);
    return carry;
}

// subtracts legs of B from legs of A into Legs of R, when all have same leg-count. Return borrow (0 or 1)
local_func_inl u8 sub_borrow_legs_64_to(u64* tLegsR,
                                             const u64* tLegsA, const u64* tLegsB, int iLegsCount)
{
    u8 borrow = 0;
    for (int i=0; i<iLegsCount; i++)
        tLegsR[i] = SubBorrow64(tLegsA[i], tLegsB[i], borrow, &borrow);
    return borrow;
}

// subtracts legs of B from legs of A (inplace on A). leg count on A shall be at least as large as legs count on B. Return borrow (0 or 1)
local_func_inl u8 sub_inplace_legs_64(u64* tLegsA, int iLegsCountA,
                                           const u64* tLegsB, int iLegsCountB)
{
    u8 borrow = 0;
    int i=0;
    for ( ; i<iLegsCountB; i++)
        tLegsA[i] = SubBorrow64(tLegsA[i], tLegsB[i], borrow, &borrow);
    for ( ; borrow && i<iLegsCountA; i++)
        tLegsA[i] = SubBorrow64(tLegsA[i], 0uLL, borrow, &borrow);
    return borrow;
}

// compares legs of A with same-sized legs of B. returns true if A is found strictly less than B, false otherwise.
local_func_inl bool lesser_than_legs64(const u64* tLegsA, const u64* tLegsB, int iLegsCount) {
    for (int i=iLegsCount-1; i >= 0; i--) {
        u64 uLegA = tLegsA[i];
        u64 uLegB = tLegsB[i];
        if (uLegA < uLegB)
            return true;
        else if (uLegA > uLegB)
            return false;
    }
    return false;
}

// compares legs of A with same-sized legs of B. returns -1 if A is found strictly lesser than B, +1 if stricly greater, 0 if equal.
local_func_inl int cmp_leg64(const u64* tLegsA, const u64* tLegsB, int iLegsCount) {
    for (int i=iLegsCount-1; i >= 0; i--) {
        u64 uLegA = tLegsA[i];
        u64 uLegB = tLegsB[i];
        if (uLegA < uLegB)
            return -1;
        else if (uLegA > uLegB)
            return +1;
    }
    return 0;
}

// compares legs of A with same-sized legs of B. returns true if A is found equal to B, false otherwise.
local_func_inl bool equal_leg64(const u64* tLegsA, const u64* tLegsB, int iLegsCount) {
    for (int i=iLegsCount-1; i >= 0; i--) {
        if (tLegsA[i] != tLegsB[i])
            return false;
    }
    return true;
}

// indicates whether all legs are zero...
local_func_inl bool are_all_zero_leg64(const u64* tLegs, int iLegsCount) {
    for (int i=iLegsCount-1; i >= 0; i--) {
        if (tLegs[i])
            return false;
    }
    return true;
}

// multiplies legs of A with legs of B in the naive way (maybe fast when small, but O(n²)).
// leg count on R should be allocated large enough for storing 'leg count A + leg count B' legs
local_func_inl void naive_mul_legs_64_to(u64* tLegsR, const u64* tLegsA, int iLegsCountA,
                                                           const u64* tLegsB, int iLegsCountB)
{
    u64 tTmpLowHigh[2];
    int iLegsCountR = iLegsCountA + iLegsCountB;
    zero_out_legs(tLegsR, iLegsCountR);
    for (int iA=0; iA < iLegsCountA; iA++) {
        for (int iB=0; iB < iLegsCountB; iB++) {
            int iLow = iA + iB;
            tTmpLowHigh[0] = MulWithHigh64(tLegsA[iA], tLegsB[iB], tTmpLowHigh + 1);
            u8 c = add_inplace_legs_64(tLegsR + iLow, iLegsCountR-iLow, tTmpLowHigh, 2);
            Assert_(c == 0);
        }
    }
}

// multiplies legs together once (to do a square operation) in the naive way (maybe fast when small, but O(n²)).
// leg count on R should be allocated large enough for storing twice the leg count of the original value
local_func_inl void naive_square_legs_64_to(u64* tLegsR, const u64* tLegs, int iLegsCount)
{
    u64 tTmpLowHigh[2];
    int iLegsCountR = iLegsCount * 2;
    zero_out_legs(tLegsR, iLegsCountR);
    for (int iA=0; iA < iLegsCount; iA++) {
        for (int iB=0; iB < iLegsCount; iB++) {
            int iLow = iA + iB;
            tTmpLowHigh[0] = MulWithHigh64(tLegs[iA], tLegs[iB], tTmpLowHigh + 1);
            u8 c = add_inplace_legs_64(tLegsR + iLow, iLegsCountR-iLow, tTmpLowHigh, 2);
            Assert_(c == 0);
        }
    }
}

// multiplies legs of A with legs of B in the naive way, while discarding higher legs of the result
local_func_inl void naive_mul_legs_64_truncated_to(u64* tLegsR, int iLegsCountR,
    const u64* tLegsA, int iLegsCountA, const u64* tLegsB, int iLegsCountB)
{
    u64 tTmpLowHigh[2];
    zero_out_legs(tLegsR, iLegsCountR);
    for (int iA=0; iA < iLegsCountA; iA++) {
        for (int iB=0; iB < iLegsCountB; iB++) {
            int iLow = iA + iB;
            if (iLow < iLegsCountR-1) {
                tTmpLowHigh[0] = MulWithHigh64(tLegsA[iA], tLegsB[iB], tTmpLowHigh + 1);
                add_inplace_legs_64(tLegsR + iLow, iLegsCountR-iLow, tTmpLowHigh, 2);
            } else if (iLow == iLegsCountR-1) {
                tLegsR[iLow] += tLegsA[iA] * tLegsB[iB];
            }
        }
    }
}

// multiplies legs of A with legs of B, adding the result directly to whatever was already in tLegsR
local_func_inl void mul_and_add_legs_64_to(u64* tLegsR, int iLegsCountR, const u64* tLegsA, int iLegsCountA,
                                                const u64* tLegsB, int iLegsCountB)
{
    u64 tTmpLowHigh[2];
    // Compared to 'naive_mul_legs_64_to', we allow caller to specify a larger leg count to add to
    Assert_(iLegsCountR >= iLegsCountA + iLegsCountB);
    // compared to 'naive_mul_legs_64_to', we simply omit the zeroing here :p
    for (int iA=0; iA < iLegsCountA; iA++) {
        for (int iB=0; iB < iLegsCountB; iB++) {
            int iLow = iA + iB;
            tTmpLowHigh[0] = MulWithHigh64(tLegsA[iA], tLegsB[iB], tTmpLowHigh + 1);
            // ...really the same algorithm otherwise
            u8 c = add_inplace_legs_64(tLegsR + iLow, iLegsCountR-iLow, tTmpLowHigh, 2);
            Assert_(c == 0);
        }
    }
}

// multiplies legs of A with legs of B, subtracting the result directly from whatever was already in tLegsR
local_func_inl void mul_and_sub_legs_64_from(u64* tLegsR, int iLegsCountR, const u64* tLegsA, int iLegsCountA,
                                                  const u64* tLegsB, int iLegsCountB)
{
    u64 tTmpLowHigh[2];
    // Compared to 'naive_mul_legs_64_to', we allow caller to specify a larger leg count to subtract from
    Assert_(iLegsCountR >= iLegsCountA + iLegsCountB);
    // compared to 'naive_mul_legs_64_to', we simply omit the zeroing here :p
    for (int iA=0; iA < iLegsCountA; iA++) {
        for (int iB=0; iB < iLegsCountB; iB++) {
            int iLow = iA + iB;
            tTmpLowHigh[0] = MulWithHigh64(tLegsA[iA], tLegsB[iB], tTmpLowHigh + 1);
            // compared to 'mul_and_add_legs_64_to', we simply 'sub' instead of 'add' here :p
            u8 c = sub_inplace_legs_64(tLegsR + iLow, iLegsCountR-iLow, tTmpLowHigh, 2);
            Assert_(c == 0);
        }
    }
}

#if 0
#include "Arenas.h"
// multiplies legs of A with legs of B using karatsuba algorithm when both leg counts are quite large (like, both > 8)
// leg count on R should be allocated large enough for storing 'leg count A + leg count B' legs
local_func void karatsuba_mul_legs_64_to(u64* tLegsR, const u64* tLegsA, int iLegsCountA,
                                                const u64* tLegsB, int iLegsCountB,
                                                Arena tmpAlloc)
{
    Assert(false, "karatsuba_mul_legs_64_to() : not yet implemented");
}
#endif

// prereqs : iLegsCount > 0
//           0 <= iBitCount < 64
local_func_inl u64 left_shift_legs_64_bitonly_by(int iBitCount, u64* tLegs, int iLegCount) {
    Assert_(iBitCount < 64);
    u64 uBitsFromLower = 0;
    if (iBitCount) {
        int iRev = 64-iBitCount;
        for (int i=0; i<iLegCount; i++) {
            u64 uLeg = tLegs[i];
            tLegs[i] = (uLeg << iBitCount) | uBitsFromLower;
            uBitsFromLower = uLeg >> iRev;
        }
    }
    return uBitsFromLower;
}

// prereqs : iLegsCount > 0
//           0 <= iBitCount < 64
local_func_inl u64 right_shift_legs_64_bitonly_by(int iBitCount, u64* tLegs, int iLegCount) {
    Assert_(iBitCount < 64);
    u64 uBitsFromHigher = 0;
    if (iBitCount) {
        int iRev = 64-iBitCount;
        for (int i=iLegCount-1; i>=0; i--) {
            u64 uLeg = tLegs[i];
            tLegs[i] = (uLeg >> iBitCount) | uBitsFromHigher;
            uBitsFromHigher = uLeg << iRev;
        }
    }
    return uBitsFromHigher;
}

// prereqs : iLegsCount > 0
//           value != 0
local_func int left_justify_legs_64(u64* tLegs, int iLegCount, int* outMSLegIndex) {
    for (int i=iLegCount-1; i>=0; i--) {
        u64 uLeg = tLegs[i];
        if (uLeg) {
            int iHighBit = GetPosOfMostSignificantBitSet64(uLeg);
            int iShift = 63 - iHighBit;
            left_shift_legs_64_bitonly_by(iShift, tLegs, i+1);
            *outMSLegIndex = i;
            return iShift;
        }
    }
    Assume(false, "left_justify_legs_64() : value should not be zero");
}

// Returns the index of the highest significant (non-zero) leg from legs in tLegs. Returns -1 if all zero
local_func_inl int find_highest_significant_leg64(u64* tLegs, int iLegCount) {
    for (int i=iLegCount-1; i>=0; i--) {
        if (tLegs[i])
            return i;
    }
    return -1;
}

// Returns the index of the lowest significant (non-zero) leg from legs in tLegs. Returns iLegCount if all zero
local_func_inl int find_lowest_significant_leg64(u64* tLegs, int iLegCount) {
    for (int i=0; i<iLegCount; i++) {
        if (tLegs[i])
            return i;
    }
    return iLegCount;
}

// optimized-case of a single-legged divisor. See 'long_div_leg64_to' below for a general explanation of the technique.
local_func_inl void semi_long_div_leg64_to(u64* tQuotientLegs, u64* ioDividendAndRem, int iLegsCountQuotient, u64 uSingleLeggedDivisor)
{
    Assert_(uSingleLeggedDivisor & 0x8000'0000'0000'0000uLL);
    Assert_(iLegsCountQuotient > 0);
    int iRemainingQuotientLegs = iLegsCountQuotient-1;
    u64* pCurrentQuotientLeg = tQuotientLegs + iRemainingQuotientLegs;  // highest leg of quotient
    u64* pCurrentRem = ioDividendAndRem + iRemainingQuotientLegs;    // highest leg of dividend when divisor leg count is at
    u8 expectNone; // simply to assert of carry- or borrow- out as zero.
    u64* pCurrentRemAndOne = pCurrentRem + 1;

    {
        //
        // Phase A) : after startup, do one check against MSLeg of dividend to know whether first quotient needs +1
        //
        if (*pCurrentRem >= uSingleLeggedDivisor) {
            *pCurrentQuotientLeg = 1uLL;
            *pCurrentRem -= uSingleLeggedDivisor;
        }
    }

    //
    // Phase B) : check if remaining quotient legs (and if so decrease counts).
    //
    while (iRemainingQuotientLegs) { // We'll stop simply when we run out of quotient legs to emit to
                
        // we still have legs: position quotient leg to the 'next' (lower)
        iRemainingQuotientLegs--;
        pCurrentQuotientLeg--;

        // also slide our ersatz-"window" of divisor-positionning wrt legs of the current remainder
        pCurrentRem--; 
        pCurrentRemAndOne--;

        //
        // Phase C: actually partial-divide at current window
        //

        // since we have a single-leg divisor, we do not need to 'probe' at all: the x86-64 instruction
        // for large div will already return the correct answer!
        *pCurrentQuotientLeg = DivAndRemLarge64(*pCurrentRemAndOne, *pCurrentRem,
                                                uSingleLeggedDivisor, pCurrentRem);

        // As a bonus, we do not need a separate mul and sub either, we already have our correct remainder leg
        //      directly computed in-place from the instruction !!
        *pCurrentRemAndOne = 0;
        Assert_(*pCurrentRem < uSingleLeggedDivisor);

    } // and that's it: we're ready to iterate for next quotient leg (phase B)
}


// -------------------------------------------------------------------------
// General-purpose core implementation of a long-division algorithm for arbitrary counts of 64b legs
// -------------------------------------------------------------------------
// 'tQuotientLegs' shall be allocated at iLegCountQuotient
// 'tLegsDivisor'  shall shall hold an always left-justified representation of the divisor.
// 'ioDividendAndRem' shall be allocated at iLegCountQuotient + iLegsCountDivisor - 1. Should start with MSleg
//    of the dividend copied at its highest pos, all its lower legs on previous positions, and all potential lower-still
//    legs zeroed out.
// Algorithm will start emitting quotient legs at the highest available position (tQuotientLegs + iLegCountQuotient-1), which
//    incidentally, due to the fact that 'tLegsDivisor' must hold a left-justifed value, will always be either 0 or 1.
// All other quotient legs after that will get the results of the successive partial-division steps of a high-school
//    'long-division' methodology... only operating here on 64b legs instead of digits!! We rely on the x86 family ability for
//    'large-division' (and x86-64 in 64b), to estimate a very good 'first guess' against two 64b-legs of the dividend, and
//    the highest leg of the divisor at a time. When there are no more available quotient legs to emit to, we simply stop.
// By carefully chosing leg-counts and inputs, you can arrange so that the quotient result really is the integral quotient
//    of an integer division, and the legs remaining in 'tLegsDivisor' really are the representation of the integral remainder
//    of the same.
// Alternatively, you can provide arbitrary quotient leg counts an let that algorithm compute any number of legs for a
//    fixed-point or floating point representation... for those rational results, if remainder when algo stops is all 0,
//    then you can be sure you had an exact result in binary... (otherwise you may even compare remainder to half-the divisor
//    to decide of an accurately rounded result).
//
// Generic schema of positionning:
//
//    D&R  /  Div  ->  Quo
//
//   +---+   +---+
//   |MSL|   |Prb|             Starting pos of MSLeg of Dividend  ;  Auto Starting pos of MSLeg of Window for Divisor
//   +---+   +---+
//   |   |   |   |
//   +---+   +---+    +---+
//   |   |   |   | -> |   |    Auto-Starting pos of LSLeg of Window from Divisor  ;  first check quotient => 0 or 1
//   +---+   +---+    +---+
//   |   |            |   |    First window slide ; second quotient leg (first full-fledged output)
//   +---+            +---+
//   |   |            |   |
//   +---+            +---+
//   |   |            |   |    Last full-fledged output at the end of algorithm
//   +---+            +---+
//
// For integral divisions: Starting from a configuration like the above, where we allocate arrays at max capacity for the
//   max potential sizes of Dividend (generally == dividend size + 1 to allow same left-shift than justification of Divisor),
//   and where we positionned the (shifted) dividend straight from the base of the D&R
// * either, keep everything in that same configuration whatever the case, knowing that highest leg of quotient here will
//      always be 0.
// * or (recommended): try at least to trim divisor as much as possible.
//   - if divisor is finally 1 less leg by trimming a MSLeg away:
//          * specify its legs one less indeed, and add one to the quotient size. (repeatable n times for n trimmed high)
//   - if divisor is finally 1 less leg by trimming a LSLeg away:
//          * specify its legs one less indeed, remove one from the quotient size, and compensate by offsetting start in
//            D&R by 1 in the table. (repeatable n times for n trimmed low). This may overshot actual legs of the dividend away,
//            but this will still produce the intended result: you'll find remainder starting at index 0 (below what was
//            specified to the algorithm), with its lowest leg 'untouched' by the algorithm, which is perfectly fine here.
// * and (potentially): also trim dividend:
//   - if dividend shift has not produced any 1 in high:
//          * you may reduce quotient size by 1. this will indeed 'cut' the algorithm visibility for the otherwise
//            High-after-shift leg, as 0 in D&R now 'above' the algorithm awareness. In that case, the highest leg of
//            quotient is no longuer a guaranteed 0 (but maybe surprisingly, it may only be '0' or '1' instead).
//   - if dividend started with 1 trimmable MSLeg to begin with (that is, before shift):
//          * you may also reduce quotient size by 1. (repeatable n times for n trimmed high).
//   - if dividend has trimmable LSLeg:
//          * do nothing... keep the sizes here intact and those legs at zero... you may only 'un-compensate' the offset-start
//            case of trimming LSLegs from the divisor, by not having them at all in the table to begin with, but that's quite
//            a lot of logic for the general case. caring about that will only saves you a few legs of *memory*, and does
//            nothingn to computation.
local_func void long_div_leg64_to(u64* tQuotientLegs, u64* ioDividendAndRem, int iLegsCountQuotient,
                              const u64* tLegsDivisor, int iLegsCountDivisor)
{
    // We'll dispatch towards distinct versions of the algorithm against three main cases:
    //   1) general case of a multi-legged divisor
    //   2) edge-case of a multi-legged divisor, when MSB of divisor is full-1s
    //   3) case of single-legged divisor.
    //
    // In all cases, the general phases are the following:
    //   0) starting the partial division 'window' by matching the MSB of 'dividend' (aka future 'remainder') in front of the
    //      MSB of the 'divisor', and consider emitting towards the provided highest leg of quotient.
    //   A) from there, if dividend legs in current window are greater-or-equal to current divisor, add '1' to
    //      current quotient position, and subtract divisor from remainder legs (up to divisor leg count). '1' is ensured
    //      the correct value by virtue of the left-justification of the divisor. Otherwise, leave that quotient leg at 0.
    //   B) - If there are remaining quotient leg count to fill, decrement that count and decrement quotient emission pointer
    //        to the next-lower quotient leg. the 'window' of the algorithm is shifted so that the MSLeg of the divisor is in
    //        front of the next-lower leg of the remainder ; in the general case, there will be remaining bits to divide in
    //        the remainder in the just-slided-past leg, though.
    //      - Otherwise, this signals the end of the algorithm. 
    //   C) Partial-divide of the remainder legs (up to divisor leg-count + 1) by the divisor. We're greatly helper in that by
    //      probing (in the general case) a large-divide of the two highest legs of remainder (current MSLeg in-window as low,
    //      just-bypassed-one as high) by 'MSLeg of divisor + 1'. By virtue of the left-justification of the divisor (again),
    //      we're ensured the result of the probe is either the correct result already, or at very few iterations from the
    //      correct result. => after applying the quotient-by probe (adding probe result to the quotient, and subtracting
    //      divisor time probe-result from the remainder), check if remainder legs (up to divisor leg count +1) are still 
    //      greater-or-equal to current divisor. While true, add '1' to current quotient position, and subtract divisor from
    //      remainder legs (up to divisor leg count +1).
    //      => Once it's done, we're ensured we do not need a 'phase A' any more on next round => loop to phase B directly !
    //

    Assert_(iLegsCountDivisor > 0);
    u64 uMSLegDiv = tLegsDivisor[iLegsCountDivisor - 1];
    Assert_(iLegsCountQuotient > 0);
    Assert_(uMSLegDiv & 0x8000'0000'0000'0000uLL);

    int iRemainingQuotientLegs = iLegsCountQuotient-1;
    u64* pCurrentQuotientLeg = tQuotientLegs + iRemainingQuotientLegs;  // highest leg of quotient
    u64* pCurrentLowRem = ioDividendAndRem + iRemainingQuotientLegs;    // highest leg of dividend when divisor leg count is at
                                                                        //   its min of 1, otherwise, all the more further down.
    u8 expectNone; // simply to assert of carry- or borrow- out as zero.

    if (iLegsCountDivisor > 1) {

        // general case of multi-legged divisors
        int iLegsCountDivisorAndOne = iLegsCountDivisor + 1;
        u64* pCurrentHighRemAndOne = pCurrentLowRem + iLegsCountDivisor;
        u64* pCurrentHighRem = pCurrentHighRemAndOne - 1;

        {
            //
            // Phase A) : after startup, do one check against MSLeg of dividend to know whether first quotient needs +1
            //
            if (!lesser_than_legs64(tLegsDivisor, pCurrentLowRem, iLegsCountDivisor)) {                
                *pCurrentQuotientLeg = 1uLL; // we can hard-set '1' here
                expectNone = sub_inplace_legs_64(pCurrentLowRem, iLegsCountDivisor, tLegsDivisor, iLegsCountDivisor);
                Assert_(expectNone == 0);
            }
        }

        if (LIKELY(uMSLegDiv != 0xFFFF'FFFF'FFFF'FFFFuLL)) {
            // "nominal" general-case of multi-legged divisor, with a non-full-1s MSB

            u64 uDivProbe = uMSLegDiv + 1u;

            //
            // Phase B) : check if remaining quotient legs (and if so decrease counts).
            //
            while (iRemainingQuotientLegs) { // We'll stop simply when we run out of quotient legs to emit to
                
                // we still have legs: position quotient leg to the 'next' (lower)
                iRemainingQuotientLegs--;
                pCurrentQuotientLeg--;

                // also slide our window of divisor-positionning wrt legs of the current remainder
                pCurrentLowRem--;
                pCurrentHighRem--;
                pCurrentHighRemAndOne--;

                //
                // Phase C: actually partial-divide at current window
                //

                // ... very accurate estimate of the true partial-divide with help of the probe !!
                u64 unused;
                u64 uProbedResult = DivAndRemLarge64(*pCurrentHighRemAndOne, *pCurrentHighRem,
                                                     uDivProbe, &unused);
                *pCurrentQuotientLeg = uProbedResult;
                mul_and_sub_legs_64_from(pCurrentLowRem, iLegsCountDivisorAndOne,
                                     tLegsDivisor, iLegsCountDivisor, &uProbedResult, 1);
                // ... and perform a few compare-and-increase iterations after that to pinpoint the true partial-divide
                while (*pCurrentHighRemAndOne || !lesser_than_legs64(tLegsDivisor, pCurrentLowRem, iLegsCountDivisor)) {
                    *pCurrentQuotientLeg += 1uLL; // should NEVER overflow a single leg
                    expectNone = sub_inplace_legs_64(pCurrentLowRem, iLegsCountDivisorAndOne, tLegsDivisor, iLegsCountDivisor);
                    Assert_(expectNone == 0);
                }
                Assert_(*pCurrentHighRemAndOne == 0);
                Assert_(*pCurrentHighRem <= uMSLegDiv && *pCurrentHighRem < uDivProbe); 

            } // and that's it: we're ready to iterate for next quotient leg (phase B)

        } else {
            // edge-case of multi-legged divisor, with a full-1s MSB
            // very similar to the above, but the 'probe' part is actually simply a 'take highlegandone' as a result.

            //
            // Phase B) : check if remaining quotient legs (and if so decrease counts).
            //
            while (iRemainingQuotientLegs) { // We'll stop simply when we run out of quotient legs to emit to
                
                // we still have legs: position quotient leg to the 'next' (lower)
                iRemainingQuotientLegs--;
                pCurrentQuotientLeg--;

                // also slide our window of divisor-positionning wrt legs of the current remainder
                pCurrentLowRem--;
                pCurrentHighRem--;
                pCurrentHighRemAndOne--;

                //
                // Phase C: actually partial-divide at current window
                //

                // ...taking the highleg+1 of the remainder as a first estimate !
                *pCurrentQuotientLeg = *pCurrentHighRemAndOne;
                mul_and_sub_legs_64_from(pCurrentLowRem, iLegsCountDivisorAndOne,
                                         tLegsDivisor, iLegsCountDivisor, pCurrentQuotientLeg, 1);

                // ... and perform a few compare-and-increase iterations after that to pinpoint the true partial-divide
                while (*pCurrentHighRemAndOne || !lesser_than_legs64(tLegsDivisor, pCurrentLowRem, iLegsCountDivisor)) {
                    *pCurrentQuotientLeg += 1uLL; // should NEVER overflow a single leg
                    expectNone = sub_inplace_legs_64(pCurrentLowRem, iLegsCountDivisorAndOne, tLegsDivisor, iLegsCountDivisor);
                    Assert_(expectNone == 0);
                }
                Assert_(*pCurrentHighRemAndOne == 0);

            } // and that's it: we're ready to iterate for next quotient leg (phase B)

        }

    } else {
        // optimized-case of a single-legged divisor

        semi_long_div_leg64_to(tQuotientLegs, ioDividendAndRem, iLegsCountQuotient, uMSLegDiv);

    }
}

// same leg-count, R = A & B
local_func_inl void bit_and_legs64_to(u64* tLegsR, const u64* tLegsA, const u64* tLegsB, i32 iLegCount) {
    for (i32 i=0; i<iLegCount; i++)
        tLegsR[i] = tLegsA[i] & tLegsB[i];
}

// same leg-count, R = A | B
local_func_inl void bit_or_legs64_to(u64* tLegsR, const u64* tLegsA, const u64* tLegsB, i32 iLegCount) {
    for (i32 i=0; i<iLegCount; i++)
        tLegsR[i] = tLegsA[i] | tLegsB[i];
}

// same leg-count, R = A ^ B
local_func_inl void bit_xor_legs64_to(u64* tLegsR, const u64* tLegsA, const u64* tLegsB, i32 iLegCount) {
    for (i32 i=0; i<iLegCount; i++)
        tLegsR[i] = tLegsA[i] ^ tLegsB[i];
}

// same leg-count, A &= B
local_func_inl void bit_and_legs64_inplace(u64* tLegsA, const u64* tLegsB, i32 iLegCount) {
    for (i32 i=0; i<iLegCount; i++)
        tLegsA[i] &= tLegsB[i];
}

// same leg-count, A |= B
local_func_inl void bit_or_legs64_inplace(u64* tLegsA, const u64* tLegsB, i32 iLegCount) {
    for (i32 i=0; i<iLegCount; i++)
        tLegsA[i] |= tLegsB[i];
}

// same leg-count, A ^= B
local_func_inl void bit_xor_legs64_inplace(u64* tLegsA, const u64* tLegsB, i32 iLegCount) {
    for (i32 i=0; i<iLegCount; i++)
        tLegsA[i] ^= tLegsB[i];
}
