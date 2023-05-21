#pragma once

#define DEBUG_BREAK()           __debugbreak()

#include <intrin.h>

// A compiler hint, not to reorder reads across that point in a code sequence
#define COMPILER_READ_FENCE()   _ReadBarrier()
// A compiler hint, not to reorder writes across that point in a code sequence
#define COMPILER_WRITE_FENCE()  _WriteBarrier()
// A compiler hint, not to reorder any reads or writes across that point in a code sequence
#define COMPILER_FULL_FENCE()   _ReadWriteBarrier()

// A target architecture hint, not to reorder reads across that point in a code sequence
#define TARGET_READ_FENCE()     _mm_lfence()
// A target architecture hint, not to reorder writes across that point in a code sequence
#define TARGET_WRITE_FENCE()    _mm_sfence()
// A target architecture hint, not to reorder any reads or writes across that point in a code sequence
#define TARGET_FULL_FENCE()     _mm_mfence()

// A hint for both the compiler and the target architecture, not to reorder reads across that point in a code sequence
#define READ_FENCE()            do { COMPILER_READ_FENCE(); TARGET_READ_FENCE(); } while(0)
// A hint for both the compiler and the target architecture, not to reorder writes across that point in a code sequence
#define WRITE_FENCE()           do { COMPILER_WRITE_FENCE(); TARGET_WRITE_FENCE(); } while(0)
// A hint for both the compiler and the target architecture, not to reorder any reads or writes across that point in a code sequence
#define FULL_FENCE()            do { COMPILER_FULL_FENCE(); TARGET_FULL_FENCE(); } while(0)

// Returns the position (starting at 0 on LSB) of the MOST significant bit set to 1 on a 32b value. Results in [0..31].
// Warning: Undefined for value 0.
// Example for uValue == 12 (ie binary ...00001100), returns 3
static FORCE_INLINE int GetPosOfMostSignificantBitSet32(u32 uValue) {
    unsigned long result; _BitScanReverse(&result, uValue);
    return int(result);
}
// Returns the position (starting at 0 on LSB) of the LEAST significant bit set to 1 on a 32b value. Results in [0..31].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary ...00001100), returns 2
static FORCE_INLINE int GetPosOfLeastSignificantBitSet32(u32 uValue) {
    unsigned long result; _BitScanForward(&result, uValue);
    return int(result);
}

// Returns the position (starting at 0 on LSB) of the MOST significant bit set to 1 on a 8b value. Results in [0..7].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary 00001100), returns 3
static FORCE_INLINE int GetPosOfMostSignificantBitSet8(u8 uValue) {
    return GetPosOfMostSignificantBitSet32(u32(uValue));
}
// Returns the position (starting at 0 on LSB) of the LEAST significant bit set to 1 on a 8b value. Results in [0..7].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary 00001100), returns 2
static FORCE_INLINE int GetPosOfLeastSignificantBitSet8(u8 uValue) {
    return GetPosOfLeastSignificantBitSet32(u32(uValue));
}

// Returns the position (starting at 0 on LSB) of the MOST significant bit set to 1 on a 16b value. Results in [0..15].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary ...00001100), returns 3
static FORCE_INLINE int GetPosOfMostSignificantBitSet16(u16 uValue) {
    return GetPosOfMostSignificantBitSet32(u32(uValue));
}
// Returns the position (starting at 0 on LSB) of the LEAST significant bit set to 1 on a 16b value. Results in [0..15].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary ...00001100), returns 2
static FORCE_INLINE int GetPosOfLeastSignificantBitSet16(u16 uValue) {
    return GetPosOfLeastSignificantBitSet32(u32(uValue));
}

// Returns the position (starting at 0 on LSB) of the MOST significant bit set to 1 on a 64b value. Results in [0..63].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary ...00001100), returns 3
static FORCE_INLINE int GetPosOfMostSignificantBitSet64(u64 uValue) {
#  ifdef _WIN64     // 64b arch
    unsigned long result; _BitScanReverse64(&result, uValue);
    return int(result);
#  else             // 32b arch
    u32 uHigh = u32(uValue >> 32);
    if (uHigh)
        return GetPosOfMostSignificantBitSet32(uHigh) + 32;
    else
        return GetPosOfMostSignificantBitSet32(u32(uValue));
#  endif
}
// Returns the position (starting at 0 on LSB) of the LEAST significant bit set to 1 on a 16b value. Results in [0..15].
// Warning: Undefined for value 0
// Example for uValue == 12 (ie binary ...00001100), returns 2
static FORCE_INLINE int GetPosOfLeastSignificantBitSet64(u64 uValue) {
#  ifdef _WIN64     // 64b arch
    unsigned long result; _BitScanForward64(&result, uValue);
    return int(result);
#  else             // 32b arch
    u32 uLow = u32(uValue);
    if (uLow)
        return GetPosOfLeastSignificantBitSet32(uLow);
    else
        return GetPosOfLeastSignificantBitSet32(u32(uValue >> 32)) + 32;
#  endif
}

// Returns the number of bits set to 1 in a 16b value. Results in [0..16].
static FORCE_INLINE int CountSetBits16(u16 uValue) {
    return int(__popcnt16(uValue));
}
// Returns the number of bits set to 1 in a 32b value. Results in [0..32].
static FORCE_INLINE int CountSetBits32(u32 uValue) {
    return __popcnt(uValue);
}
// Returns the number of bits set to 1 in a 8b value. Results in [0..8].
static FORCE_INLINE int CountSetBits8(u8 uValue) {
    return CountSetBits32(u32(uValue));
}
// Returns the number of bits set to 1 in a 64b value. Results in [0..64].
static FORCE_INLINE int CountSetBits64(u64 uValue) {
#  ifdef _WIN64     // 64b arch
    return int(__popcnt64(uValue));
#  else             // 32b arch
    return CountSetBits32(u32(uValue >> 32)) + CountSetBits32(u32(uValue));
#  endif
}

// Returns the number of contiguous zeroed bits, starting from the LEAST significant, in a 8b value. Results in [0..8].
// Note that contrary to the 'GetPosOfLeastSignificantBitSet8' implementation on which it is based, uValue==0 is accepted here, returning 8.
// Example for uValue == 12 (ie binary 00001100), returns 2.
static FORCE_INLINE int CountTrailingZeroes8(u8 uValue) {
    if (uValue)
        return GetPosOfLeastSignificantBitSet8(uValue);
    else
        return 8;
}
// Returns the number of contiguous zeroed bits, starting from the MOST significant, in a 8b value. Results in [0..8].
// Note that contrary to the 'GetPosOfMostSignificantBitSet8' implementation on which it is based, uValue==0 is accepted here, returning 8.
// Example for uValue == 12 (ie binary 00001100), returns 4.
static FORCE_INLINE int CountLeadingZeroes8(u8 uValue) {
    if (uValue)
        return GetPosOfMostSignificantBitSet8(uValue) - 7;
    else
        return 8;
}

// Returns the number of contiguous zeroed bits, starting from the LEAST significant, in a 16b value. Results in [0..16].
// Note that contrary to the 'GetPosOfLeastSignificantBitSet16' implementation on which it is based, uValue==0 is accepted here, returning 16.
// Example for uValue == 12 (ie binary 00000000 00001100), returns 2.
static FORCE_INLINE int CountTrailingZeroes16(u16 uValue) {
    if (uValue)
        return GetPosOfLeastSignificantBitSet16(uValue);
    else
        return 16;
}
// Returns the number of contiguous zeroed bits, starting from the MOST significant, in a 16b value. Results in [0..16].
// Note that contrary to the 'GetPosOfMostSignificantBitSet16' implementation on which it is based, uValue==0 is accepted here, returning 16.
// Example for uValue == 12 (ie binary 00000000 00001100), returns 12.
static FORCE_INLINE int CountLeadingZeroes16(u16 uValue) {
    if (uValue)
        return GetPosOfMostSignificantBitSet16(uValue) - 15;
    else
        return 16;
}

// Returns the number of contiguous zeroed bits, starting from the LEAST significant, in a 32b value. Results in [0..32].
// Note that contrary to the 'GetPosOfLeastSignificantBitSet32' implementation on which it is based, uValue==0 is accepted here, returning 32.
// Example for uValue == 12 (ie binary 00000000 00000000 00000000 00001100), returns 2.
static FORCE_INLINE int CountTrailingZeroes32(u32 uValue) {
    if (uValue)
        return GetPosOfLeastSignificantBitSet32(uValue);
    else
        return 32;
}
// Returns the number of contiguous zeroed bits, starting from the MOST significant, in a 32b value. Results in [0..32].
// Note that contrary to the 'GetPosOfMostSignificantBitSet32' implementation on which it is based, uValue==0 is accepted here, returning 32.
// Example for uValue == 12 (ie binary 00000000 00000000 00000000 00001100), returns 28.
static FORCE_INLINE int CountLeadingZeroes32(u32 uValue) {
    if (uValue)
        return 31 - GetPosOfMostSignificantBitSet32(uValue);
    else
        return 32;
}

// Returns the number of contiguous zeroed bits, starting from the LEAST significant, in a 64b value. Results in [0..64].
// Note that contrary to the 'GetPosOfLeastSignificantBitSet64' implementation on which it is based, uValue==0 is accepted here, returning 64.
// Example for uValue == 12 (ie binary 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00001100), returns 2.
static FORCE_INLINE int CountTrailingZeroes64(u64 uValue) {
    if (uValue)
        return GetPosOfLeastSignificantBitSet64(uValue);
    else
        return 64;
}
// Returns the number of contiguous zeroed bits, starting from the MOST significant, in a 64b value. Results in [0..64].
// Note that contrary to the 'GetPosOfMostSignificantBitSet64' implementation on which it is based, uValue==0 is accepted here, returning 64.
// Example for uValue == 12 (ie binary 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00001100), returns 60.
static FORCE_INLINE int CountLeadingZeroes64(u64 uValue) {
    if (uValue)
        return 63 - GetPosOfMostSignificantBitSet64(uValue);
    else
        return 64;
}

// Addition of two 8b values, returning carry-out. Also Able to take an additional carry-in.
static FORCE_INLINE u8 AddCarry8(u8 uValueA, u8 uValueB, u8 uCIn, u8* outCOut) {
    u8 uResult;
    *outCOut = _addcarry_u8(uCIn, uValueA, uValueB, &uResult);
    return uResult;
}

// Addition of two 16b values, returning carry-out. Also Able to take an additional carry-in.
static FORCE_INLINE u16 AddCarry16(u16 uValueA, u16 uValueB, u8 uCIn, u8* outCOut) {
    u16 uResult;
    *outCOut = _addcarry_u16(uCIn, uValueA, uValueB, &uResult);
    return uResult;
}

// Addition of two 32b values, returning carry-out. Also Able to take an additional carry-in.
static FORCE_INLINE u32 AddCarry32(u32 uValueA, u32 uValueB, u8 uCIn, u8* outCOut) {
    u32 uResult;
    *outCOut = _addcarry_u32(uCIn, uValueA, uValueB, &uResult);
    return uResult;
}

// Subtraction of two 8b values, returning borrow-out. Also Able to take an additional borrow-in.
static FORCE_INLINE u8 SubBorrow8(u8 uValueA, u8 uValueB, u8 uBIn, u8* outBOut) {
    u8 uResult;
    *outBOut = _subborrow_u8(uBIn, uValueA, uValueB, &uResult);
    return uResult;
}

// Subtraction of two 16b values, returning borrow-out. Also Able to take an additional borrow-in.
static FORCE_INLINE u16 SubBorrow16(u16 uValueA, u16 uValueB, u8 uBIn, u8* outBOut) {
    u16 uResult;
    *outBOut = _subborrow_u16(uBIn, uValueA, uValueB, &uResult);
    return uResult;
}

// Subtraction of two 32b values, returning borrow-out. Also Able to take an additional borrow-in.
static FORCE_INLINE u32 SubBorrow32(u32 uValueA, u32 uValueB, u8 uBIn, u8* outBOut) {
    u32 uResult;
    *outBOut = _subborrow_u32(uBIn, uValueA, uValueB, &uResult);
    return uResult;
}

#include <immintrin.h>

// Multiplication of two 32b values with 32b registers, returning high 32b result as well.
static FORCE_INLINE u32 MulWithHigh32(u32 uValueA, u32 uValueB, u32* outHigh) {
    return _mulx_u32(uValueA, uValueB, outHigh);
}

// Multiplication of two 32b values with 32b registers, returning high 32b result as well. (signed version)
static FORCE_INLINE i32 MulWithHigh32_Signed(i32 iValueA, i32 iValueB, i32* outHigh) {
    i64 result = __emul(iValueA, iValueB);
    *outHigh = i32(result >> 32);
    return i32(result);
}

// 32b-Division of 32b values using 32b registers, able to take a second 32b high-leg for the dividend
//   in a single instruction (as x86 allows natively). Also returns 32b remainder. IMHO comes with the 'exactness'
//   requirement that uHighValueA < uValueB, otherwise quotient may not fit in an u32.
//   (And, as usual for division, uValueB > 0)
static FORCE_INLINE u32 DivAndRemLarge32(u32 uHighValueA, u32 uLowValueA, u32 uValueB, u32* outRem) {
    u64 uFullValueA = (u64(uHighValueA) << 32) | uLowValueA;
    #if _MSC_VER >= 1600    // _udiv64 first introduced in VS2019 'RTM' --> is this really 16.00 ?
        return _udiv64(uFullValueA, uValueB, outRem);
    #else                   // simulate by a full 64b div...
        u64 uResult64 = uFullValueA / u64(uValueB);
        u64 uRem64 = uFullValueA % u64(uValueB); // hoping for the compiler to indeed emit a single div if on a 32b arch!!!
        *outRem = u32(uRem64);
        return u32(uResult64);
    #endif
}

// Performs an intrinsic interlocked compare exchange on a 32b value.
// Atomically:
//      - reads content pointed by 'inout'
//      - if content pointed by 'inout' equals to 'uValueToExpect', writes 'uValueToSet' to address pointed by 'inout'. Otherwise does nothing.
//      - returns content pointed by 'inout' originally.
static FORCE_INLINE u32 InterlockedCmpEx32(u32 volatile * inout, u32 uValueToSet, u32 uValueToExpect) {
    return u32(_InterlockedCompareExchange((long volatile *)inout, long(uValueToSet), long(uValueToExpect)));
}

// Atomically increments a 32b value by 1, and returns resulting value.
static FORCE_INLINE u32 InterlockedIncAndGet32(u32 volatile * inout) {
    return u32(_InterlockedIncrement((long volatile *)inout));
}
// Atomically increments a 32b value by 1, returning previous value.
static FORCE_INLINE u32 InterlockedGetAndInc32(u32 volatile * inout) {
    return InterlockedIncAndGet32(inout) - 1u;
}

// Atomically decrements a 32b value by 1, and returns resulting value.
static FORCE_INLINE u32 InterlockedDecAndGet32(u32 volatile * inout) {
    return u32(_InterlockedDecrement((long volatile *)inout));
}
// Atomically decrements a 32b value by 1, returning previous value.
static FORCE_INLINE u32 InterlockedGetAndDec32(u32 volatile * inout) {
    return InterlockedDecAndGet32(inout) + 1u;
}

// Atomically increments a 32b value by some amount, returning previous value.
static FORCE_INLINE u32 InterlockedGetAndAdd32(u32 volatile * inout, u32 uToAdd) {
    return u32(_InterlockedExchangeAdd((long volatile *)inout, long(uToAdd)));
}

// Atomically replace a 32b value by another, returning previous value.
static FORCE_INLINE u32 InterlockedExch32(u32 volatile * inout, u32 uToReplaceWith) {
    return u32(_InterlockedExchange((long volatile *)inout, long(uToReplaceWith)));
}

// Emits the x86/x64 'Rdtsc' instruction, returning the processor timestamp.
static FORCE_INLINE u64 Rdtsc() {
    return __rdtsc();
}


#ifdef _WIN64     // 64b arch

    // Addition of two 64b values, returning carry-out. Also Able to take an additional carry-in.
    static FORCE_INLINE u64 AddCarry64(u64 uValueA, u64 uValueB, u8 uCIn, u8* outCOut) {
        u64 uResult;
        *outCOut = _addcarryx_u64(uCIn, uValueA, uValueB, &uResult);
        return uResult;
    }

    // Subtraction of two 64b values, returning borrow-out. Also Able to take an additional borrow-in.
    static FORCE_INLINE u64 SubBorrow64(u64 uValueA, u64 uValueB, u8 uBIn, u8* outBOut) {
        u64 uResult;
        *outBOut = _subborrow_u64(uBIn, uValueA, uValueB, &uResult);
        return uResult;
    }

    // Multiplication of two 64b values with 64b registers, returning high 64b result as well.
    static FORCE_INLINE u64 MulWithHigh64(u64 uValueA, u64 uValueB, u64* outHigh) {
        return _mulx_u64(uValueA, uValueB, outHigh);
    }

    // Multiplication of two 32b values with 32b registers, returning high 32b result as well. (signed version)
    static FORCE_INLINE i64 MulWithHigh64_Signed(i64 iValueA, i64 iValueB, i64* outHigh) {
        return _mul128(iValueA, iValueB, outHigh);
    }


#if _MSC_VER >= 1600    // _udiv128 first introduced in VS2019 'RTM' --> is this really 16.00 ?
    // 64b-Division of 64b values using 64b registers, able to take a second 64b high-leg for the dividend
    //   in a single instruction (as x86-64 allows natively). Also returns 64b remainder. IMHO comes with the 'exactness'
    //   requirement that uHighValueA < uValueB, otherwise quotient may not fit in an u64.
    //   (And, as usual for division, uValueB > 0)
    static FORCE_INLINE u64 DivAndRemLarge64(u64 uHighValueA, u64 uLowValueA, u64 uValueB, u64* outRem) {
        return _udiv128(uHighValueA, uLowValueA, uValueB, outRem);
    }
#else
    // On second thought, we do *not* want to fallback to softare here
    #error "LocLib currently requires support for x86 '2x64b division' in hardware"
    // => only keep the following code aside atm
    static u64 DivAndRemLarge64(u64 uHighValueA, u64 uLowValueA, u64 uValueB, u64* outRem) {
        // Barring direct access to x64 assembly, We'll need to simulate kind of a long div there,
        //   even though this instrinsic was itself probably needed in the context of a larger long-div algorithm... oh, well...

        // Left-justifying B for greater accuracy of our 32b probe first-estimates (and a better chance at single-32b optim)
        int iPosOfMsbDivisor = GetPosOfMostSignificantBitSet64(uValueB); // We do not worry about B being '0': B is the divisor!!
        int iShift = 63 - iPosOfMsbDivisor;
        uValueB <<= iShift;
        // Left-shifting A of same amount than B for getting exact quotient ; and a remainder requiring only a simple shift.
        uHighValueA <<= iShift; // We do not care about lost bits there: high of A should not be greater than B
        uHighValueA |= uLowValueA >> (64 - iShift);
        uLowValueA <<= iShift;
        // Preparing for long-div algorithm in 2 steps of 3x32b partial divs, each estimated by 2x-32b-large-probes
        u32 uHighOfDiv = (uValueB >> 32); // Will be used as single-32b divisor of our 2x-32b-large-probes
        u32 uHighQuotient, uLowQuotient;  // Will hold high and low 32b of quotient, almost ready to return.
        u64 uSecondPassLowRem;  // Will hold shifted remainder (by iShift to the left) at the end of our two passes

        if (uLowOfDiv) { // Nominal case

            bool bProbeByHighOfDivPlusOne = (uHighOfDiv != 0xFFFF'FFFFu); // Can we do +1 on the high-32b probe, or not ?

            // First Pass:   ( '| ... |' is partial-div window. '| .. :' is part of window with probe-estimate)
            //  | HighA.MS32  HighA.LS32  :  LowA.MS32  | LowA.LS32
            //  |   00000      DivB.MS32  :  DivB.LS32  |
            //                                   v
            //                               HighQuo32
            u64 uFirstPassHighProbe = uHighValueA;
            u64 uFirstEstimFirstPass = bProbeByHighOfDivPlusOne ? // We cannot do div by +1 safely if already full 1s,
                uFirstPassHighProbe / u64(uHighOfDiv+1u) : uFirstPassHighProbe >> 32; // ...but we can shift instead
            uHighQuotient = u32(uFirstEstimFirstPass);
            u64 uHighTrue; u64 uLowTrue = MulWithHigh64(uFirstEstimFirstPass, uValueB, &uHighTrue);
            u64 uFirstPassHighRem = uHighValueA >> 32;
            u64 uFirstPassLowRem = (u64(u32(uHighValueA)) << 32) | uLowValueA << 32;
            u8 uBorrow;
            uFirstPassLowRem = SubBorrow64(uFirstPassLowRem, uLowTrue, 0, &uBorrow);
            uFirstPassHighRem = SubBorrow64(uFirstPassHighRem, uHighTrue, uBorrow, &uBorrow);
            while (uFirstPassHighRem) {
                uFirstPassLowRem = SubBorrow64(uFirstPassLowRem, uValueB, 0, &uBorrow);
                uFirstPassHighRem -= uBorrow;
                uHighQuotient++;
            }
            // Second Pass:   ( '| ... |' is partial-div window. '| .. :' is part of window with probe-estimate)
            //   00000   | LRemA.MS32   LRemA.LS32  :  LowA.LS32  |
            //           |    00000      DivB.MS32  :  DivB.LS32  |
            //                                             v
            //                           HighQuo32     LowQuo32
            u64 uSecondPassHighProbe = uFirstPassLowRem;
            u64 uFirstEstimSecondPass = bProbeByHighOfDivPlusOne ? // We cannot do div by +1 safely if already full 1s,
                uSecondPassHighProbe / u64(uHighOfDiv+1u) : uSecondPassHighProbe >> 32; // ...but we can shift instead
            uLowQuotient = u32(uFirstEstimSecondPass);
            uLowTrue = MulWithHigh64(uFirstEstimSecondPass, uValueB, &uHighTrue);
            u8 uBorrow;
            u64 uSecondPassHighRem = uSecondPassHighProbe >> 32;
            uSecondPassLowRem = (u64(u32(uSecondPassHighProbe)) << 32) | u64(u32(uLowValueA));
            uSecondPassLowRem = SubBorrow64(uSecondPassLowRem, uLowTrue, 0, &uBorrow);
            uSecondPassHighRem = SubBorrow64(uSecondPassHighRem, uHighTrue, uBorrow, &uBorrow);
            while (uSecondPassHighRem) {
                uSecondPassLowRem = SubBorrow64(uSecondPassLowRem, uValueB, 0, &uBorrow);
                uSecondPassHighRem -= uBorrow;
                uLowQuotient++;
            }

        } else { // Optimized path, when significant bits of divisor fit in a single 32b value.

            // First Pass:   ( '| ... |' is partial-div window. '| .. :' is part of window with probe-estimate)
            //  | HighA.MS32  HighA.LS32  :  LowA.MS32  | LowA.LS32
            //  |   00000      DivB.MS32  :    00000    |
            //                                   v
            //                               HighQuo32
            u64 uFirstPassHighProbe = uHighValueA;
            uHighQuotient = u32(uFirstPassHighProbe / u64(uHighOfDiv)); // We can replace our probe + iterate by direct results !
            u64 uFirstPassLowRem = ((uFirstPassHighProbe % u64(uHighOfDiv)) << 32) | (uLowValueA >> 32);
            // Second Pass:   ( '| ... |' is partial-div window. '| .. :' is part of window with probe-estimate)
            //   00000   | LRemA.MS32   LRemA.LS32  :  LowA.LS32  |
            //           |    00000      DivB.MS32  :    00000    |
            //                                             v
            //                           HighQuo32     LowQuo32
            u64 uSecondPassHighProbe = uFirstPassLowRem;
            uLowQuotient = u32(uSecondPassHighProbe / u64(uHighOfDiv)); // We can replace our probe + iterate by direct results !
            uSecondPassLowRem = ((uSecondPassHighProbe % u64(uHighOfDiv)) << 32) | u64(u32(uLowValueA));
        }
        *outRem = uSecondPassLowRem >> iShift;
        return (u64(uHighQuotient) << 32) | u64(uLowQuotient);
    }
#endif

    // Performs an intrinsic interlocked compare exchange on a 64b value.
    // Atomically:
    //      - reads content pointed by 'inout'
    //      - if content pointed by 'inout' equals to 'uValueToExpect', writes 'uValueToSet' to address pointed by 'inout'. Otherwise does nothing.
    //      - returns content pointed by 'inout' originally.
    static FORCE_INLINE u64 InterlockedCmpEx64(u64 volatile * inout, u64 uValueToSet, u64 uValueToExpect) {
        return u64(_InterlockedCompareExchange64((__int64 volatile *)inout, __int64(uValueToSet), __int64(uValueToExpect)));
    }

    // Atomically increments a 64b value by 1, and returns resulting value.
    static FORCE_INLINE u64 InterlockedIncAndGet64(u64 volatile * inout) {
        return u64(_InterlockedIncrement64((__int64 volatile *)inout));
    }
    // Atomically increments a 64b value by 1, returning previous value.
    static FORCE_INLINE u64 InterlockedGetAndInc64(u64 volatile * inout) {
        return InterlockedIncAndGet64(inout) - 1uLL;
    }

    // Atomically decrements a 64b value by 1, and returns resulting value.
    static FORCE_INLINE u64 InterlockedDecAndGet64(u64 volatile * inout) {
        return u64(_InterlockedDecrement64((__int64 volatile *)inout));
    }
    // Atomically decrements a 64b value by 1, returning previous value.
    static FORCE_INLINE u64 InterlockedGetAndDec64(u64 volatile * inout) {
        return InterlockedDecAndGet64(inout) + 1uLL;
    }

    // Atomically increments a 64b value by some amount, returning previous value.
    static FORCE_INLINE u64 InterlockedGetAndAdd64(u64 volatile * inout, u64 uToAdd) {
        return u64(_InterlockedExchangeAdd64((__int64 volatile *)inout, __int64(uToAdd)));
    }

    // Atomically replace a 64b value by another, returning previous value.
    static FORCE_INLINE u64 InterlockedExch64(u64 volatile * inout, u64 uToReplaceWith) {
        return u64(_InterlockedExchange64((__int64 volatile *)inout, __int64(uToReplaceWith)));
    }

#else
    #error "LocLib currently requires a 64b target arch for its compiler"
#endif


