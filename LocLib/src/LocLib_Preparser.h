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

#ifndef LOCLIB_PREPARSER_H_
#define LOCLIB_PREPARSER_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/legs64_core.h"
#include "../../HighPerfTools/arithmetic_operations.h"
#include "LocLib_Compint.h"
#include "LocLib_ScanAndTok.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_ProgramState.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_DebugPrint.h"

// TODO : remove dependency to crt
#include <stdio.h>
//#include <cstdlib>

#if TRACE_PRE_PARSER_PRINTLEVEL > 0
    #define debug_print_preparse_err(strMessage) platform_log_error(strMessage, true);
#else
    #define debug_print_preparse_err(strMessage)
#endif

#define pre_parser_set_error(uErrCode, tokenRef) do { \
    *outError = uErrCode; \
    parserParams.parserState.uRefStartOfError = make_packed_token_ref(tokenRef); \
} while(0)

local_func TokenRef _on_error_get_ref_from_position(ParserParams& parserParams) {
    TokenRef result = {};
    if (parserParams.parserState.pCurrentToken < parserParams.parserState.pLineTokenEnd) {
        result.token = *parserParams.parserState.pCurrentToken;
        result.uIndex = u16(parserParams.parserState.pCurrentToken - parserParams.parserState.pLineTokens);
        result.uBytesFromLineStart = parserParams.parserState.uCurrentLineIndent + parserParams.parserState.pCurrentToken->uTokenStartCharOnLine;
    } else {
        result.uBytesFromLineStart = parserParams.parserState.uCurrentLineIndent + parserParams.parserState.uCurrentBytesOnLine;
    }
    result.uLine = u32(parserParams.parserState.iCurrentlyParsedLine - parserParams.parserState.iInlinedRootStartLine);
    return result;
}

/*
#define pre_parser_set_error_from_position(uErrCode) do { \
    TokenRef tmpErrTokenRef = _on_error_get_ref_from_position(parserParams); \
    pre_parser_set_error(uErrCode, tmpErrTokenRef); \
} while(0)
*/
template<i32 BASE>
local_func bool parse_natnum_payload_from_preparsed_info_with_base(const u8* pData, int iDigitsCount,
    TokenRef& tokenRef, ParserParams& parserParams, u64* outResult, u16* outError)
{
    constexpr i32 iDigitsPerLeg64 = BASE == 2 ? 64 : (
                                    BASE == 8 ? 21 : (
                                    BASE == 10 ? 19 : 16));
    int iRequiredLegs64b = (iDigitsCount + iDigitsPerLeg64 - 1) / iDigitsPerLeg64;
    const u8* pDbgEnd = parserParams.parserState.pStartOfLineAfterIndent + tokenRef.token.uTokenStartCharOnLine + tokenRef.token.uTokenCharCount;
    if (iRequiredLegs64b == 1) {
        u64 uResult64 = 0;
        int iReadDigits = 0;
        while (iReadDigits < iDigitsCount) {
            Assert_(pData < pDbgEnd);
            u8 c = *pData;
            pData++;
            static_assert(u8('9') < u8('_'));
            if (c <= u8('9')) {
                Assert_(c >= u8('0'));
                uResult64 *= BASE;
                uResult64 += u64(c - u8('0'));
                iReadDigits++;
            } else {
                if (BASE == 16) {
                    char cLower = c | 32;
                    if (cLower >= u8('a') && cLower <= u8('f')) {
                        uResult64 *= BASE;
                        uResult64 += u64(10u + cLower - u8('a'));
                        iReadDigits++;
                    } else
                        Assert_(c == u8('_'));
                } else
                    Assert_(c == u8('_'));
            }
        }

        if (uResult64 < 0x2000'0000'0000'0000uLL) {
            // this here is the *same encoding* as what TC will subsequently consider to be an embedded, positive, "compint".
            *outResult = uResult64 << 3;
            // (As a side note: our literals are never negative: the operator '-' is not part of the literal, for declaring negative numbers)
            return true;
        } else {
            // TODO
            debug_print_preparse_err("*** large 64b integer to 1-legged compint not yet implemented");
            pre_parser_set_error(FERR_NOT_YET_IMPLEMENTED, tokenRef);
            return false;
        }

    } else {
        static_assert(BASE <= 16 && iDigitsPerLeg64 >= 16);
        static_assert(LINE_BUFFERSIZE <= 2048, "a small line buffersize limit is used to our advantage in current number-decode implementation");
        Assert_(iDigitsCount <= LINE_BUFFERSIZE && iRequiredLegs64b <= 128); // if both above static asserts hold...
        u64 tAllLegs64[128];
        i32 iCurrentLeg = 0;
        int iReadDigits = 0;
        int iReadDigitsInCurrentLeg = 0;
        u64 uResult64 = 0;
        while (iReadDigits < iDigitsCount) {
            Assert_(pData < pDbgEnd);
            u8 c = *pData;
            pData++;
            static_assert(u8('9') < u8('_'));
            if (c <= u8('9')) {
                Assert_(c >= u8('0'));
                uResult64 *= BASE;
                uResult64 += u64(c - u8('0'));
                iReadDigits++;
                iReadDigitsInCurrentLeg++;
                if (iReadDigitsInCurrentLeg >= iDigitsPerLeg64) {
                    Assume_(iReadDigitsInCurrentLeg == iDigitsPerLeg64);
                    tAllLegs64[iCurrentLeg] = uResult64;
                    iCurrentLeg++;
                    iReadDigitsInCurrentLeg = 0;
                    uResult64 = 0;
                }
            } else {
                if (BASE == 16) {
                    char cLower = c | 32;
                    if (cLower >= u8('a') && cLower <= u8('f')) {
                        uResult64 *= BASE;
                        uResult64 += u64(10u + cLower - u8('a'));
                        iReadDigits++;
                        iReadDigitsInCurrentLeg++;
                        if (iReadDigitsInCurrentLeg >= iDigitsPerLeg64) {
                            Assume_(iReadDigitsInCurrentLeg == iDigitsPerLeg64);
                            tAllLegs64[iCurrentLeg] = uResult64;
                            iCurrentLeg++;
                            iReadDigitsInCurrentLeg = 0;
                            uResult64 = 0;
                        }
                    } else
                        Assert_(c == u8('_'));
                } else
                    Assert_(c == u8('_'));
            }
        }
        // TODO: if decimal, multiply payload in each leg by a large-num binary representation of (10^19)^legIndex, and add those together.
        // TODO: if octal, do the same with (2^63)^legIndex, or could also 'simply' compact the 1b leftover at msb of each leg.
        // TODO: if decimal or octal, re-ensure significant result is above 64b, and possibly return as embedded 64b if it fits.
        // TODO: otherwise, alloc a 'largenatnum' of sufficient size to hold those legs and point to that (and return 'false' as is_small)
        //
        debug_print_preparse_err("*** parsing large integers not yet implemented");
        pre_parser_set_error(FERR_NOT_YET_IMPLEMENTED, tokenRef);
        return false;
    }
}
; // template termination

template<i32 BASE>
local_func bool parse_float_payload_from_preparsed_info_with_base(const u8* pData, int iSignificantDigitsCount,
    int iExponentAtLastSignificantDigit, TokenRef& tokenRef, ParserParams& parserParams, u64* outResult, u16* outError)
{
    Assert_(iSignificantDigitsCount);
    constexpr int iExponentShiftPerDigit = BASE == 16 ? 4 : (BASE == 8 ? 3 : 1); // decimal with 10^ exponent, or binary with 2^ exponent are 1 exp shift per digit.
    // our final XFloat format will have a 320b mantissa.
    // If a float literal seems to be specified with much more precision that this, we may warn the user that his expected result would differ.
    // This is important however to possibly let a small amount of precision digits as a margin. We chose 4 here.
    // Note : a 320b mantissa is a little above 96 significant digits in decimal representation.
    if ((BASE == 10 && iSignificantDigitsCount-4 > 96) || (BASE != 10 && (iSignificantDigitsCount-4) * iExponentShiftPerDigit > 320)) {
        debug_print_preparse_err("*** This floating-point literal is expressed significantly over our max 320b precision for LOC 'XFloats' - Severe rounding warning"); 
        pre_parser_set_error(PERR_OVERPRECISE_FLOATING_POINT_LITERAL, tokenRef);    // TODO: warning instead ?
        return false;
    }

    constexpr i32 iDigitsPerLeg32 = BASE == 2 ? 32 : (
                                    BASE == 8 ? 10 : (
                                    BASE == 10 ? 9 : 8));
    int iRequiredLegs32b = (iSignificantDigitsCount + iDigitsPerLeg32 - 1) / iDigitsPerLeg32;
    const u8* pDbgEnd = parserParams.parserState.pStartOfLineAfterIndent + tokenRef.token.uTokenStartCharOnLine + tokenRef.token.uTokenCharCount;
    
    // TODO!!! A true XFloat reader
    //
    // TMP TMP : currently a not-even-so-great approximation to a representation as 'double' in all cases.
    //
    u64 uTmpResult64 = 0;
    int iReadDigits = 0;
    constexpr i32 iDigitsPerLeg64 = BASE == 2 ? 64 : (
                                    BASE == 8 ? 21 : (
                                    BASE == 10 ? 19 : 16));
    int iDigitsCount = _min(iSignificantDigitsCount, iDigitsPerLeg64);
    iExponentAtLastSignificantDigit += (iSignificantDigitsCount - iDigitsCount) * iExponentShiftPerDigit;
    if (iSignificantDigitsCount > iDigitsPerLeg32) {
        // this is potentially a significant loss of precision in case larger than 32b, but also imprecise in the general case.
        //   here is just a remainder that this needs some more work:
        platform_log_debug("!!! parse_float_payload_from_preparsed_info_with_base() : currently parsing all to f64 instead of float_lit");
    }
    u64 uResult64 = 0;
    while (iReadDigits < iDigitsCount) {
        Assert_(pData < pDbgEnd);
        u8 c = *pData;
        pData++;
        if (c >= u8('0') && c <= u8('9')) {
            uResult64 *= BASE;
            uResult64 += u64(c - u8('0'));
            iReadDigits++;
        } else if (BASE == 16) {
            char cLower = c | 32;
            if (cLower >= u8('a') && cLower <= u8('f')) {
                uResult64 *= BASE;
                uResult64 += u64(10u + cLower - u8('a'));
                iReadDigits++;
            }
        }
    }
    Assert_(uResult64);
    if (BASE == 10) {
        i32 iApproxDecimalExponent = (iExponentAtLastSignificantDigit + iSignificantDigitsCount - 1);

        // we will not bother about 'true' ~10^307 boundaries, nor will we bother about subnormal literals for this temporary solution
        if (iApproxDecimalExponent < -300 || iApproxDecimalExponent > 300) {
            debug_print_preparse_err("*** parsing floating points with huge exponents not yet implemented");
            pre_parser_set_error(FERR_NOT_YET_IMPLEMENTED, tokenRef);
            return false;
        }

        // we will not bother about cumulated precision loss at each pass of this iterative method for this temporary solution
        double dApproxResult = double(uResult64);
        if (iExponentAtLastSignificantDigit < 0) {
            i32 iRemainingExponent = -iExponentAtLastSignificantDigit;
            while (iRemainingExponent >= 16) {
                iRemainingExponent -= 16;
                dApproxResult /= 1e16;
            }
            while (iRemainingExponent >= 4) {
                iRemainingExponent -= 4;
                dApproxResult /= 1e4;
            }
            while (iRemainingExponent) {
                iRemainingExponent -= 1;
                dApproxResult /= 10;
            }
        } else {
            i32 iRemainingExponent = iExponentAtLastSignificantDigit;
            while (iRemainingExponent >= 16) {
                iRemainingExponent -= 16;
                dApproxResult *= 1e16;
            }
            while (iRemainingExponent >= 4) {
                iRemainingExponent -= 4;
                dApproxResult *= 1e4;
            }
            while (iRemainingExponent) {
                iRemainingExponent -= 1;
                dApproxResult *= 10;
            }
        }
        *outResult = type_pun_from_double(dApproxResult);
        return true;

    } else {
        int iMsb = GetPosOfMostSignificantBitSet64(uResult64);
        u64 uMantissa64 = uResult64;
        int iDiff = iMsb - 52;
        // We will not bother about correct rounding for this temporary solution.
        if (iDiff > 0)
            uMantissa64 >>= iDiff;
        else if (iDiff < 0)
            uMantissa64 <<= -iDiff;
        uMantissa64 &= ~(1uLL << 52);
        Assert_(0 == (uMantissa64 & 0xFFF0'0000'0000'0000uLL));
        int iExponentAsDouble = iExponentAtLastSignificantDigit + iDiff + 52;

        // we will not bother about subnormal literals for this temporary solution
        if (iExponentAsDouble < -1022 || iExponentAsDouble > 1023) {
            debug_print_preparse_err("*** parsing floating points with huge exponents not yet implemented");
            pre_parser_set_error(FERR_NOT_YET_IMPLEMENTED, tokenRef);
            return false;
        }
        *outResult = uMantissa64 | (u64(iExponentAsDouble + 1023) << 52);
        return true;
    }
    //
    // TMP TMP
}
; // template termination

template<i32 BASE>
local_func bool reparse_float_payload_from_extreme_base(const u8* pCurrent, const u8* pEndOrExponentStart,
    i32 iHasExponent, i32 iIsExponentNeg, TokenRef& tokenRef, ParserParams& parserParams, u64* outResult, u16* outError)
{
    // Here, float literal format or value was too extreme to fit pre-parsed info in the token payload...
    // What we need to do here is 'simply' to pre-parse it again to reconstruct that info (from a slightly better standpoint
    //   than the tokenizer, though...), then call 'parse_float_payload_from_preparsed_info_with_base' as usual.

    i32 iSignificantDigitCountIntPart = 0;
    i32 iSignificantDigitCountTotal = 0;
    i32 iIntPartTrailingZeroes = 0;
    i32 iFracPartTrailingZeroes = 0;

    u8 uHasSeenDot = 0;
    u8 uHasSeenNonZeroDigit = 0;

    const u8* pEnd = parserParams.parserState.pStartOfLineAfterIndent + tokenRef.token.uTokenStartCharOnLine + tokenRef.token.uTokenCharCount;
    const u8* pPosOfFirstSignificantDigit = 0;
    Assert_(pEndOrExponentStart <= pEnd);
	while (pCurrent < pEndOrExponentStart) {
		u8 c = *pCurrent;
		if (c == u8('0')) {
            if (uHasSeenDot == 0) { // still processing int part
                Assert_(iSignificantDigitCountIntPart); // otherwise should not be first...
                iSignificantDigitCountIntPart++;
                iIntPartTrailingZeroes++;
            } else {               // processing frac part
                iFracPartTrailingZeroes++;
                if (uHasSeenNonZeroDigit) // ...and already found non-zero digit
                    iFracPartTrailingZeroes++;
                else               // processing frac part while having found only zeroes:
                    iSignificantDigitCountIntPart--; // counting number in int part in reverse
            }

		} else if ((c >= u8('0') && c <= u8('9')) ||
                   (BASE == 16 && ((c >= u8('A') && c <= u8('F')) ||
                                   (c >= u8('a') && c <= u8('f'))))) {
            if (uHasSeenNonZeroDigit == 0) {
                uHasSeenNonZeroDigit = 1u;
                pPosOfFirstSignificantDigit = pCurrent;
            }
            if (uHasSeenDot == 0) { // still processing int part
                iSignificantDigitCountIntPart++;
                iSignificantDigitCountTotal += iIntPartTrailingZeroes + 1;
                iIntPartTrailingZeroes = 0;
            } else { // processing frac part
                iSignificantDigitCountTotal += iIntPartTrailingZeroes + iFracPartTrailingZeroes + 1;
                iIntPartTrailingZeroes = 0;
                iFracPartTrailingZeroes = 0;
            }

		} else if (c == u8('.')) {
            Assert_(uHasSeenDot == 0);
            uHasSeenDot = 1u;
        }

		pCurrent++;
	}

    Assert_(pPosOfFirstSignificantDigit);

    i32 iExponentRaw = 0;
    if (iHasExponent) {
        Assert_(pEndOrExponentStart < pEnd);
        i32 iExponentAbs = 0;
	    i32 iExponentSign = iIsExponentNeg ? -1 : 1;
        pCurrent = pEndOrExponentStart;
        while (pCurrent < pEnd) {
		    u8 c = *pCurrent;
            if (c >= u8('0') && c <= u8('9')) {
                iExponentAbs *= 10;          // we can assume it is safe to take that input blindly:
                iExponentAbs += c - u8('0'); //   parser should not have emitted if those were to overflow an i32
            } else
                Assert_(c == '_'); // parser should have set the token-end there otherwise
            pCurrent++;
        }
        iExponentRaw = iExponentAbs;
	    if (iExponentSign)
            iExponentRaw = -iExponentRaw;
    }

    int iSignificantDigitsAfterDot = iSignificantDigitCountTotal - iSignificantDigitCountIntPart;
    constexpr int iExponentShiftPerDigit = BASE == 16 ? 4 : (BASE == 8 ? 3 : 1); // decimal with 10^ exponent, or binary with 2^ exponent are 1 exp shift per digit.
    int iExponentAtLastSignificantDigit = iExponentRaw - (iSignificantDigitsAfterDot * iExponentShiftPerDigit);

    return parse_float_payload_from_preparsed_info_with_base<BASE>(pPosOfFirstSignificantDigit, iSignificantDigitCountTotal,
        iExponentAtLastSignificantDigit, tokenRef, parserParams, outResult, outError);
}
; // template termination

local_func bool make_nat_payload_from_token_return_is_small(TokenRef& tokenRef,
    ParserParams& parserParams, u64* outResult, u16* outError)
{
    if (0 == (tokenRef.token.uTokenPayloadAndKind & TOKENKINDFLAG_EXTREME_NUMERAL)) { // simple case: 24b payload already decoded
        *outResult = u64(tokenRef.token.uTokenPayloadAndKind >> 8) << 3;
        return true;
    } else { // payload contains 11b positions and count of digits to decode (thanks to limits on our possible scanned line length).
        u32 uEncodedBase = tokenRef.token.uTokenPayloadAndKind & 0x03u;
        i32 iPositionOfFirstSignificantDigit = i32(tokenRef.token.uTokenPayloadAndKind >> 8) & 0x0000'07FF; // relative to start pos of token
        i32 iDigitsCount = i32(tokenRef.token.uTokenPayloadAndKind >> 19) & 0x0000'07FF;
        const u8* pStart = parserParams.parserState.pStartOfLineAfterIndent + tokenRef.token.uTokenStartCharOnLine;
        const u8* pCurrent = pStart + iPositionOfFirstSignificantDigit;
        switch (uEncodedBase) {
            case 0: return parse_natnum_payload_from_preparsed_info_with_base<2>(pCurrent, iDigitsCount,
                tokenRef, parserParams, outResult, outError);
            case 1: return parse_natnum_payload_from_preparsed_info_with_base<8>(pCurrent, iDigitsCount,
                tokenRef, parserParams, outResult, outError);
            case 2: return parse_natnum_payload_from_preparsed_info_with_base<10>(pCurrent, iDigitsCount,
                tokenRef, parserParams, outResult, outError);
            default: return parse_natnum_payload_from_preparsed_info_with_base<16>(pCurrent, iDigitsCount,
                tokenRef, parserParams, outResult, outError);
        }
    }
}

local_func bool make_float_payload_from_token_return_is_small(TokenRef& tokenRef, ParserParams& parserParams,
    u64* outResult, u16* outError)
{
    u32 uEncodedBase = tokenRef.token.uTokenPayloadAndKind & 0x03u;
    const u8* pStart = parserParams.parserState.pStartOfLineAfterIndent + tokenRef.token.uTokenStartCharOnLine;
    if (0 == (tokenRef.token.uTokenPayloadAndKind & TOKENKINDFLAG_EXTREME_NUMERAL)) {
        if (tokenRef.token.uTokenPayloadAndKind & 0xFFFF'FF00u) {
            // nominal case: payload has 5b start pos ; 7b total significant digits ; 12b exponent at Least Significant Digit
            i32 iPositionOfFirstSignificantDigit = i32(tokenRef.token.uTokenPayloadAndKind >> 8) & 0x0000'001F; // relative to start pos of token
            i32 iSignificantDigitsCount = i32(tokenRef.token.uTokenPayloadAndKind >> 13) & 0x0000'007F;
            i32 iExponentAtLastSignificantDigit = i32(tokenRef.token.uTokenPayloadAndKind) >> 20; // kept already signed from MSB
            const u8* pCurrent = pStart + iPositionOfFirstSignificantDigit;
            switch (uEncodedBase) {
                case 0: return parse_float_payload_from_preparsed_info_with_base<2>(pCurrent, iSignificantDigitsCount,
                    iExponentAtLastSignificantDigit, tokenRef, parserParams, outResult, outError);
                case 1: return parse_float_payload_from_preparsed_info_with_base<8>(pCurrent, iSignificantDigitsCount,
                    iExponentAtLastSignificantDigit, tokenRef, parserParams, outResult, outError);
                case 2: return parse_float_payload_from_preparsed_info_with_base<10>(pCurrent, iSignificantDigitsCount,
                    iExponentAtLastSignificantDigit, tokenRef, parserParams, outResult, outError);
                default: return parse_float_payload_from_preparsed_info_with_base<16>(pCurrent, iSignificantDigitsCount,
                    iExponentAtLastSignificantDigit, tokenRef, parserParams, outResult, outError);
            }
        } else { // token payload 0 is float value 0 which is an embedded result 0
            *outResult = 0;
            return true;
        }
    } else {
        // extreme case: payload has 11b start pos (first significant or dot) ; 11b pos of end or exponent, 1b has exponent, 1b isExponentNeg
        i32 iPositionOfFirstSignificantDigitOrDot = i32(tokenRef.token.uTokenPayloadAndKind >> 8) & 0x0000'07FF;
        i32 iPositionOfEndOrExponent = i32(tokenRef.token.uTokenPayloadAndKind >> 19) & 0x0000'07FF;
        i32 iHasExponent = i32(tokenRef.token.uTokenPayloadAndKind & 0x4000'0000u);
        i32 iIsExponentNeg = i32(tokenRef.token.uTokenPayloadAndKind & 0x8000'0000u);
        const u8* pCurrent = pStart + iPositionOfFirstSignificantDigitOrDot;
        const u8* pEndOrExponentStart = pStart + iPositionOfEndOrExponent;
        switch (uEncodedBase) {
            case 0: return reparse_float_payload_from_extreme_base<2>(pCurrent, pEndOrExponentStart,
                iHasExponent, iIsExponentNeg, tokenRef, parserParams, outResult, outError);
            case 1: return reparse_float_payload_from_extreme_base<8>(pCurrent, pEndOrExponentStart,
                iHasExponent, iIsExponentNeg, tokenRef, parserParams, outResult, outError);
            case 2: return reparse_float_payload_from_extreme_base<10>(pCurrent, pEndOrExponentStart,
                iHasExponent, iIsExponentNeg, tokenRef, parserParams, outResult, outError);
            default: return reparse_float_payload_from_extreme_base<16>(pCurrent, pEndOrExponentStart,
                iHasExponent, iIsExponentNeg, tokenRef, parserParams, outResult, outError);
        }
    }
}

local_func i32 get_unicode_byte_count_for_4h(const u8* pData) {
    // TODO
    return 0;
}

local_func i32 get_unicode_byte_count_for_8h(const u8* pData) {
    // TODO
    return 0;
}

local_func bool make_string_payload_from_token_return_is_small(TokenRef& tokenRef,
    ParserParams& parserParams, u64* outResult, u16* outError)
{
    const u8* pStart = parserParams.parserState.pStartOfLineAfterIndent + tokenRef.token.uTokenStartCharOnLine;
    const u8* pEnd = pStart + tokenRef.token.uTokenCharCount;
    u32 uTokenPayload = tokenRef.token.uTokenPayloadAndKind;
    u8 uStartOfStringDataOffset = u8(uTokenPayload >> 8);
    u16 uCodeStringBytesCount = u16(uTokenPayload >> 16) & 0x1FFFu;
    u32 bIsStrictWithoutBackslashEscape = uTokenPayload & (1u << 29);
    u32 bHasMultiByteUtf8 = uTokenPayload & (1u << 30);
    u32 bHasBackslashEscapeSeq = uTokenPayload & (1u << 31);
    const u8* pStringData = pStart + uStartOfStringDataOffset;
    if (!bHasBackslashEscapeSeq) {
        FFString strPayload = FFString::makeFF(StringView::from_known_c_str((const char*)pStringData, uCodeStringBytesCount, false),
            parserParams.pSourceFile->parseOnlyArena);
        *outResult = reinterpret_cast<u64>(strPayload.pStart);
        return false;
    } else {
        u16 uDiff = 0;
        for (u32 uByte = 0; uByte < uCodeStringBytesCount; uByte++) {
            if (pStringData[uByte] == u8('\\')) {
                uDiff += 1;
                if (uByte+1 < uCodeStringBytesCount) {
                    switch(pStringData[uByte+1]) {
                        case u8('a'): break;    // sequence will be replaced by 0x07 (bell)
                        case u8('b'): break;    // sequence will be replaced by 0x08 (backspace)
                        case u8('e'): break;    // sequence will be replaced by 0x1B (escape char)
                        case u8('f'): break;    // sequence will be replaced by 0x0C (formfeed)
                        case u8('n'): break;    // sequence will be replaced by 0x0A (line feed)
                        case u8('r'): break;    // sequence will be replaced by 0x0D (carriage return)
                        case u8('t'): break;    // sequence will be replaced by 0x09 (tabulation)
                        case u8('v'): break;    // sequence will be replaced by 0x0B (vertical tab)
                        case u8('\\'): break;   // sequence will be replaced by 0x5C (backslash)
                        case u8('\''): break;   // sequence will be replaced by 0x27 (single quote)
                        case u8('\"'): break;   // sequence will be replaced by 0x22 (double quote)
                        case u8('?'): break;    // sequence will be replaced by 0x3F (question mark)
                        case u8('x'):
                            if (uByte+3 < uCodeStringBytesCount) {
                                uDiff += 2;     // 2-char sequence after \x will be replaced by 1 byte
                                uByte += 2;
                            } else
                                goto errEscape;
                            break;
                        case u8('u'):           // 4-char sequence after \u will be replaced by n bytes
                            if (uByte+5 < uCodeStringBytesCount) {
                                // TODO
                                goto errEscape;
                                //uDiff += 4 - (get_unicode_byte_count_for_4h(pStringData + uByte+2) - 1);
                                //uByte += 4;
                            } else
                                goto errEscape;
                            break;
                        case u8('U'):           // 8-char sequence after \U will be replaced by n bytes
                            if (uByte+9 < uCodeStringBytesCount) {
                                // TODO
                                goto errEscape;
                                //uDiff += 8 - (get_unicode_byte_count_for_8h(pStringData + uByte+2) - 1);
                                //uByte += 8;
                            } else
                                goto errEscape;
                            break;
                        default:
                            goto errEscape;
                    }
                    uByte++;
                } else {
                    goto errEscape;
                }
            }
        }
        u8 tTmpUnescaped[4096];
        u32 uByteOut = 0;
        for (u32 uByteIn = 0; uByteIn < uCodeStringBytesCount; uByteIn++) {
            if (pStringData[uByteIn] == u8('\\')) {
                switch(pStringData[uByteIn+1]) {
                    case u8('a'):
                        tTmpUnescaped[uByteOut] = 0x07; break;    // sequence will be replaced by 0x07 (bell)
                    case u8('b'):
                        tTmpUnescaped[uByteOut] = 0x08; break;    // sequence will be replaced by 0x08 (backspace)
                    case u8('e'):
                        tTmpUnescaped[uByteOut] = 0x1B; break;    // sequence will be replaced by 0x1B (escape char)
                    case u8('f'):
                        tTmpUnescaped[uByteOut] = 0x0C; break;    // sequence will be replaced by 0x0C (formfeed)
                    case u8('n'):
                        tTmpUnescaped[uByteOut] = 0x0A; break;    // sequence will be replaced by 0x0A (line feed)
                    case u8('r'):
                        tTmpUnescaped[uByteOut] = 0x0D; break;    // sequence will be replaced by 0x0D (carriage return)
                    case u8('t'):
                        tTmpUnescaped[uByteOut] = 0x09; break;    // sequence will be replaced by 0x09 (tabulation)
                    case u8('v'):
                        tTmpUnescaped[uByteOut] = 0x0B; break;    // sequence will be replaced by 0x0B (vertical tab)
                    case u8('\\'):
                        tTmpUnescaped[uByteOut] = 0x5C; break;    // sequence will be replaced by 0x5C (backslash)
                    case u8('\''):
                        tTmpUnescaped[uByteOut] = 0x27; break;    // sequence will be replaced by 0x27 (single quote)
                    case u8('\"'):
                        tTmpUnescaped[uByteOut] = 0x22; break;    // sequence will be replaced by 0x22 (double quote)
                    case u8('?'):
                        tTmpUnescaped[uByteOut] = 0x3F; break;    // sequence will be replaced by 0x3F (question mark)                    
                    case u8('x'):
                    case u8('u'):
                    case u8('U'):
                        //TODO
                    default:
                        goto errEscape;
                }
                uByteOut++;
                uByteIn++;
            } else {
                tTmpUnescaped[uByteOut++] = pStringData[uByteIn];
            }
        }
        // TODO ?? align-to 8 bytes for direct use of this result as runtime values by the typechecker ? (currently 'makeFF' aligns to 4...)
        FFString strPayload = FFString::makeFF(StringView::from_known_c_str((const char*)tTmpUnescaped, uByteOut, false),
            parserParams.pSourceFile->parseOnlyArena);
        *outResult = reinterpret_cast<u64>(strPayload.pStart);
        return false;

        errEscape:
            debug_print_preparse_err("*** parsing escaped code error");
            pre_parser_set_error(PERR_FAILED_INTEGRATION_OF_LITERAL, tokenRef);
            return false;
    }
}

/*
typedef PreAstNode*
    (*ContinuationParsingProc_Sign) (ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError);
typedef PreAstNode*
    (*ExpressionParsingProc_Sign) (ParserParams& parserParams, u16 uDepthGuard, u16* outError);
typedef void
    (*StatementParsingProc_Sign) (PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError);

ContinuationParsingProc_Sign tContinuationParsingProcsPerKeywordOrSymbol[255];
ExpressionParsingProc_Sign tExpressionParsingProcsPerKeywordOrSymbol[255];
StatementParsingProc_Sign tStatementParsingProcsPerKeywordOrSymbol[255];
*/

#define reference_current_token_as(name)  TokenRef name; name.token = *pCurrent; \
    name.uIndex = u16(pCurrent - parserParams.parserState.pLineTokens); \
    name.uBytesFromLineStart = parserParams.parserState.uCurrentLineIndent + pCurrent->uTokenStartCharOnLine; \
    name.uLine = u32(parserParams.parserState.iCurrentlyParsedLine - parserParams.parserState.iInlinedRootStartLine); \
    parserParams.parserState.iLineOfLastRegisteredToken = parserParams.parserState.iCurrentlyParsedLine; \
    parserParams.parserState.uIndexOfLastRegisteredToken = name.uIndex

local_func PreAstNode* _prepare_pre_node(ParserParams& parserParams, u8 uNodeKind, u16 uNodeFlags, const TokenRef& pivotalToken) {
    PreAstNode* pResult = (PreAstNode*)alloc_from(parserParams.preparsingArena, sizeof(PreAstNode), 8);
    pResult->uNodeKind = uNodeKind;
    pResult->uNodeFlags = uNodeFlags;
    pResult->pivotalToken = pivotalToken;
    pResult->primaryChildNode = 0;
    pResult->secondaryChildNode = 0;
    pResult->iCountSubNodesOrNegErr = 0;
    pResult->firstPreModifier = parserParams.parserState.inFlightModifierNodes;
    parserParams.parserState.inFlightModifierNodes = 0;
    pResult->firstPostModifier = 0;
    return pResult;
}

static const i32 TOO_MANY_PREPARSER_NODES = 0x01000000; // 256 million nodes in a single expr seems a tad too much
static const i32 TOO_MANY_CHAINED_ELEMENTS = TOO_MANY_PREPARSER_NODES >> 1; // 2 nodes per element in a chain...

void debug_print_ast_node(PreAstNode* pNode, u32 uLevel, LocLib_OS_WrapperFunctions* pOsFuncs, SourceFileDescAndState* pSourceFile);

local_func i32 debug_count_pre_ast_nodes(PreAstNode* pNode) {
    i32 iResult = 0;
    if (pNode) {
        Assert_(pNode->iCountSubNodesOrNegErr >= 0);
        switch (pNode->uNodeKind) {
        case ENODE_ATOMICEXPR_NATURAL_NUMBER_LITERAL:
        case ENODE_ATOMICEXPR_FLOATING_POINT_LITERAL:
        case ENODE_ATOMICEXPR_STRING_LITERAL:
        case ENODE_ATOMICEXPR_CODEPOINT_LITERAL:
        case ENODE_ATOMICEXPR_IDENTIFIER:
        case ENODE_ATOMICEXPR_SPECIAL:
            iResult = 0; break;
        case ENODE_EXPR_PARENTISED: // secondary is closing parens token
            iResult = debug_count_pre_ast_nodes(pNode->primaryChildNode);
            break;
        default: // all other may or may not have primaries and secondaries, but if anything, should be a node
            iResult = debug_count_pre_ast_nodes(pNode->primaryChildNode) +
                      debug_count_pre_ast_nodes(pNode->secondaryChildNode);
        }
        Assert_(iResult == pNode->iCountSubNodesOrNegErr);
        iResult += 1;
    }
    return iResult;
}

local_func void _wrapup_pre_node_primary_debug(PreAstNode* pNodeToWrapup, PreAstNode& primaryChildNode, ParserParams& parserParams)
{
    pNodeToWrapup->primaryChildNode = &primaryChildNode;
    i32 iCountOnChild = primaryChildNode.iCountSubNodesOrNegErr;
    i32 iResult;
    if (iCountOnChild >= 0) {
        iResult = 1 + pNodeToWrapup->iCountSubNodesOrNegErr + iCountOnChild;
        if (iResult > TOO_MANY_PREPARSER_NODES)
            iResult = TOO_MANY_PREPARSER_NODES;
    } else
        iResult = iCountOnChild; // error case
    pNodeToWrapup->iCountSubNodesOrNegErr = iResult;
    debug_print_ast_node(pNodeToWrapup, 0, parserParams.pOsFuncs, parserParams.pSourceFile);
#if DEBUG_PREPARSER_NODECOUNTS
    if (iResult >= 0 && iResult < TOO_MANY_PREPARSER_NODES)
        debug_count_pre_ast_nodes(pNodeToWrapup);
#endif
}

local_func void _wrapup_pre_node_primary(PreAstNode* pNodeToWrapup, PreAstNode& primaryChildNode)
{
    pNodeToWrapup->primaryChildNode = &primaryChildNode;
    i32 iCountOnChild = primaryChildNode.iCountSubNodesOrNegErr;
    i32 iResult;
    if (iCountOnChild >= 0) {
        iResult = 1 + pNodeToWrapup->iCountSubNodesOrNegErr + iCountOnChild;
        if (iResult > TOO_MANY_PREPARSER_NODES)
            iResult = TOO_MANY_PREPARSER_NODES;
    } else
        iResult = iCountOnChild; // error case
    pNodeToWrapup->iCountSubNodesOrNegErr = iResult;

#if DEBUG_PREPARSER_NODECOUNTS
    if (iResult >= 0 && iResult < TOO_MANY_PREPARSER_NODES)
        debug_count_pre_ast_nodes(pNodeToWrapup);
#endif
}

local_func void _wrapup_pre_node_secondary(PreAstNode* pNodeToWrapup, PreAstNode& secondaryChildNode)
{
    pNodeToWrapup->secondaryChildNode = &secondaryChildNode;
    i32 iCountOnChild = secondaryChildNode.iCountSubNodesOrNegErr;
    i32 iResult;
    if (iCountOnChild >= 0) {
        iResult = 1 + pNodeToWrapup->iCountSubNodesOrNegErr + iCountOnChild;
        if (iResult > TOO_MANY_PREPARSER_NODES)
            iResult = TOO_MANY_PREPARSER_NODES;
    } else
        iResult = iCountOnChild; // error case
    pNodeToWrapup->iCountSubNodesOrNegErr = iResult;

#if DEBUG_PREPARSER_NODECOUNTS
    if (iResult >= 0 && iResult < TOO_MANY_PREPARSER_NODES)
        debug_count_pre_ast_nodes(pNodeToWrapup);
#endif
}

#define check_and_wrapup_pre_node(pNodeToWrapup, primOrSec, refChildNode) do { \
    _wrapup_pre_node_##primOrSec(pNodeToWrapup, refChildNode); \
    if (*outError) \
        return pNodeToWrapup; \
} while(0)
#define check_and_wrapup_pre_node_err_if_null(pNodeToWrapup, primOrSec, childNode, strMsgIfNull) do { \
    if (childNode) { \
        _wrapup_pre_node_##primOrSec(pNodeToWrapup, *childNode); \
        if (*outError) \
            return pNodeToWrapup; \
    } else { \
        PreAstNode* expectedErr; make_err_pre_node_expected_vs_found(expectedErr, 0, strMsgIfNull); \
        _wrapup_pre_node_##primOrSec(pNodeToWrapup, *expectedErr); \
        return pNodeToWrapup; \
    } \
} while(0)

#define on_unexpected_return_err_self(uExpectedIfSingleKeyOrSymb, strMsg) do {\
    PreAstNode* expectedErr; \
    make_err_pre_node_expected_vs_found(expectedErr, uExpectedIfSingleKeyOrSymb, strMsg); \
    return expectedErr; \
} while(0)

#define on_unexpected_return_err_within(uExpectedIfSingleKeyOrSymb, pNodeToWrapup, primOrSec, strMsg) do {\
    PreAstNode* expectedErr; \
    make_err_pre_node_expected_vs_found(expectedErr, uExpectedIfSingleKeyOrSymb, strMsg); \
    _wrapup_pre_node_##primOrSec(pNodeToWrapup, *expectedErr); \
    return pNodeToWrapup; \
} while(0)

#define from_current_token_expects_on_line_strict_otherwise_err_self(pCurrentToken, uExpectedIfSingleKeyOrSymb, strMsgIfNone) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd)) { \
        PreAstNode* expectedErr; \
        make_err_pre_node_expected_found_eol(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
        return expectedErr; \
    } \
} while(0)

#define from_current_token_expects_on_line_or_break_otherwise_err_self(pCurrentToken, uExpectedIfSingleKeyOrSymb, strMsgIfNone) do { \
    if (pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        if (is_explicit_linebreak(pCurrentToken)) { \
            pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
            if (*outError) { \
                PreAstNode* expectedErr; \
                make_err_pre_node_expected_had_multiline_error(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
                return expectedErr; \
            } \
        } \
    } else { \
        PreAstNode* expectedErr; \
        make_err_pre_node_expected_found_eol(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
        return expectedErr; \
    } \
} while(0)

#define from_current_token_expects_on_any_line_otherwise_err_self(pCurrentToken, uExpectedIfSingleKeyOrSymb, strMsgIfNone) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd) || is_explicit_linebreak(pCurrentToken)) { \
        pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
        if (*outError) { \
            PreAstNode* expectedErr; \
            make_err_pre_node_expected_had_multiline_error(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
            return expectedErr; \
        } \
    } \
} while(0)

#define from_current_token_expects_on_line_strict_otherwise_err_within(pCurrentToken, pNodeToWrapup, primOrSec, uExpectedIfSingleKeyOrSymb, strMsgIfNone, whatToReturn) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd)) { \
        PreAstNode* expectedErr; \
        make_err_pre_node_expected_found_eol(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
        _wrapup_pre_node_##primOrSec(pNodeToWrapup, *expectedErr); \
        return whatToReturn; \
    } \
} while(0)

#define from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrentToken, pNodeToWrapup, primOrSec, uExpectedIfSingleKeyOrSymb, strMsgIfNone, whatToReturn) do { \
    if (pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        if (is_explicit_linebreak(pCurrentToken)) { \
            pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
            if (*outError) { \
                PreAstNode* expectedErr; \
                make_err_pre_node_expected_had_multiline_error(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
                _wrapup_pre_node_##primOrSec(pNodeToWrapup, *expectedErr); \
                return whatToReturn; \
            } \
        } \
    } else { \
        PreAstNode* expectedErr; \
        make_err_pre_node_expected_found_eol(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
        _wrapup_pre_node_##primOrSec(pNodeToWrapup, *expectedErr); \
        return whatToReturn; \
    } \
} while(0)

#define from_current_token_expects_on_any_line_otherwise_err_within(pCurrentToken, pNodeToWrapup, primOrSec, uExpectedIfSingleKeyOrSymb, strMsgIfNone, whatToReturn) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd) || is_explicit_linebreak(pCurrentToken)) { \
        pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
        if (*outError) { \
            PreAstNode* expectedErr; \
            make_err_pre_node_expected_had_multiline_error(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
            _wrapup_pre_node_##primOrSec(pNodeToWrapup, *expectedErr); \
            return whatToReturn; \
        } \
    } \
} while(0)

#define from_current_token_expects_on_line_strict_otherwise_err_around(pCurrentToken, refNodeToEmbedIfErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd)) { \
        TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams); \
        PreAstNode* errorWrapper = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken); \
        _wrapup_pre_node_primary(errorWrapper, refNodeToEmbedIfErr); \
        PreAstNode* expectedErr; \
        make_err_pre_node_expected_found_eol(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
        _wrapup_pre_node_secondary(errorWrapper, *expectedErr); \
        return errorWrapper; \
    } \
} while(0)

#define from_current_token_expects_on_line_or_break_otherwise_err_around(pCurrentToken, refNodeToEmbedIfErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone) do { \
    if (pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        if (is_explicit_linebreak(pCurrentToken)) { \
            pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
            if (*outError) { \
                TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams); \
                PreAstNode* errorWrapper = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken); \
                _wrapup_pre_node_primary(errorWrapper, refNodeToEmbedIfErr); \
                PreAstNode* expectedErr; \
                make_err_pre_node_expected_had_multiline_error(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
                _wrapup_pre_node_secondary(errorWrapper, *expectedErr); \
                return errorWrapper; \
            } \
        } \
    } else { \
        TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams); \
        PreAstNode* errorWrapper = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken); \
        _wrapup_pre_node_primary(errorWrapper, refNodeToEmbedIfErr); \
        PreAstNode* expectedErr; \
        make_err_pre_node_expected_found_eol(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
        _wrapup_pre_node_secondary(errorWrapper, *expectedErr); \
        return errorWrapper; \
    } \
} while(0)

#define from_current_token_expects_on_any_line_otherwise_err_around(pCurrentToken, refNodeToEmbedIfErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd) || is_explicit_linebreak(pCurrentToken)) { \
        pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
        if (*outError) { \
            TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams); \
            PreAstNode* errorWrapper = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken); \
            _wrapup_pre_node_primary(errorWrapper, refNodeToEmbedIfErr); \
            PreAstNode* expectedErr; \
            make_err_pre_node_expected_had_multiline_error(expectedErr, uExpectedIfSingleKeyOrSymb, strMsgIfNone); \
            _wrapup_pre_node_secondary(errorWrapper, *expectedErr); \
            return errorWrapper; \
        } \
    } \
} while(0)

#define from_current_token_allows_on_line_strict_otherwise_null_self(pCurrentToken, strMsgIfErr) do { \
    if (!(pCurrentToken < parserParams.parserState.pLineTokenEnd)) { \
        return 0; \
    } \
} while(0)

#define from_current_token_allows_on_line_or_break_otherwise_null_self(pCurrentToken, strMsgIfErr) do { \
    if (pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        if (is_explicit_linebreak(pCurrentToken)) { \
            pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
            if (*outError) { \
                PreAstNode* multilineErr; \
                make_err_pre_node_during_multiline(multilineErr, strMsgIfErr); \
                return multilineErr; \
            } \
        } \
    } else { \
        return 0; \
    } \
} while(0)

#define from_current_token_allows_break_if_multiline_err_wraparound(pCurrentToken, refNodeToEmbedIfErr, strMsgIfErr) do { \
    if (pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        if (is_explicit_linebreak(pCurrentToken)) { \
            pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
            if (*outError) { \
                TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams); \
                PreAstNode* errorWrapper = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken); \
                _wrapup_pre_node_primary(errorWrapper, refNodeToEmbedIfErr); \
                PreAstNode* multilineErr; \
                make_err_pre_node_during_multiline(multilineErr, strMsgIfErr); \
                _wrapup_pre_node_secondary(errorWrapper, *multilineErr); \
                return errorWrapper; \
            } \
        } \
    } \
} while(0)

#define from_current_token_allows_on_line_strict_otherwise_null_within(pCurrentToken, pNodeToWrapup, primOrSec, strMsgIfErr, whatToReturn) do { \
} while(0) // NOOP!

#define from_current_token_allows_on_line_or_break_otherwise_null_within(pCurrentToken, pNodeToWrapup, primOrSec, strMsgIfErr, whatToReturn) do { \
    if (pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        if (is_explicit_linebreak(pCurrentToken)) { \
            pCurrentToken = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError); \
            if (*outError) { \
                PreAstNode* multilineErr; \
                make_err_pre_node_during_multiline(multilineErr, strMsgIfErr); \
                _wrapup_pre_node_##primOrSec(pNodeToWrapup, *multilineErr); \
                return whatToReturn; \
            } \
        } \
    } \
} while(0)

#define make_err_pre_node_expected_had_multiline_error(nodePtr, uExpectedIfSingleKeyOrSymb, strReason) do { \
    debug_print_preparse_err("*** Parse Error, Multiline while expected : "#strReason); \
    TokenRef tmpErrTok = _on_error_get_ref_from_position(parserParams); \
    nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_UNEXPECTED, 0, tmpErrTok); \
    nodePtr->primaryPayload = uExpectedIfSingleKeyOrSymb; \
    Assert_(*outError); /* *outError should have been filled out by the multiline search itself */ \
    parserParams.parserState.uRefStartOfError = make_packed_token_ref(tmpErrTok); \
    nodePtr->iCountSubNodesOrNegErr = -i32(u32(*outError)); \
} while(0)

#define make_err_pre_node_expected_found_eol(nodePtr, uExpectedIfSingleKeyOrSymb, strReason) do { \
    debug_print_preparse_err("*** Parse Error, Expected Token found EOL : "#strReason); \
    TokenRef tmpErrTok = _on_error_get_ref_from_position(parserParams); \
    nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_EXPECTED_WHEN_EOL, 0, tmpErrTok); \
    nodePtr->primaryPayload = uExpectedIfSingleKeyOrSymb; \
    pre_parser_set_error(PERR_EXPECTED_TOKEN_FOUND_EOL, tmpErrTok); \
    nodePtr->iCountSubNodesOrNegErr = -i32(u32(*outError)); \
} while(0)

#define make_err_pre_node_expected_vs_found(nodePtr, uExpectedIfSingleKeyOrSymb, strReason) do { \
    if (parserParams.parserState.pCurrentToken < parserParams.parserState.pLineTokenEnd) { \
        debug_print_preparse_err("*** Parse Error, Unexpected Token : "#strReason); \
        reference_current_token_as(unexpected); \
        nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_UNEXPECTED, 0, unexpected); \
        nodePtr->primaryPayload = uExpectedIfSingleKeyOrSymb; \
        pre_parser_set_error(PERR_UNEXPECTED_TOKEN, unexpected); \
        nodePtr->iCountSubNodesOrNegErr = -i32(u32(*outError)); \
    } else \
        make_err_pre_node_expected_found_eol(nodePtr, uExpectedIfSingleKeyOrSymb, strReason); \
} while(0)

#define make_err_pre_node_during_multiline(nodePtr, strReason) do { \
    debug_print_preparse_err("*** Multiline Parse Error : "#strReason); \
    TokenRef tmpErrTok = _on_error_get_ref_from_position(parserParams); \
    nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_INVALID_MULTILINE, 0, tmpErrTok); \
    Assert_(*outError); /* *outError should have been filled out by the multiline search itself */ \
    parserParams.parserState.uRefStartOfError = make_packed_token_ref(tmpErrTok); \
    nodePtr->iCountSubNodesOrNegErr = -i32(u32(*outError)); \
} while(0)

#define make_err_pre_node_unrecognized_token(nodePtr) do { \
    debug_print_preparse_err("*** Unrecognized Token Error"); \
    TokenRef tmpErrTok = _on_error_get_ref_from_position(parserParams); \
    *outError = tmpErrTok.token.uTokenPayloadAndKind & TOKEN_CATEGORY_MASK ? \
        u16(PERR_UNMAPPED_KEYWORD) : u16(PERR_INVALID_CHARACTER_OR_SYMBOL); \
    nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_UNRECOGNIZED_TOKEN, 0, tmpErrTok); \
    parserParams.parserState.uRefStartOfError = make_packed_token_ref(tmpErrTok); \
} while (0)

#define make_err_pre_node_other(nodePtr, uError, strReason) do { \
    debug_print_preparse_err("*** Parse Error : "#strReason); \
    TokenRef tmpErrTok = _on_error_get_ref_from_position(parserParams); \
    nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_OTHER, 0, tmpErrTok); \
    pre_parser_set_error(uError, tmpErrTok); \
    nodePtr->iCountSubNodesOrNegErr = -i32(u32(*outError)); \
} while(0)

#define make_err_pre_node_unimplemented(nodePtr, strReason) do { \
    debug_print_preparse_err("*** Parse Error, Unimplemented feature : "#strReason); \
    TokenRef tmpErrTok = _on_error_get_ref_from_position(parserParams); \
    nodePtr = _prepare_pre_node(parserParams, EPRENODE_ERROR_OTHER, 0, tmpErrTok); \
    pre_parser_set_error(FERR_NOT_YET_IMPLEMENTED, tmpErrTok); \
    nodePtr->iCountSubNodesOrNegErr = -i32(u32(*outError)); \
} while(0)

local_func_inl PreAstNode* fixup_unary_op_node(PreAstNode* pInParseOrder, ParserParams& parserParams) 
{
    PreAstNode* pChildInParseOrder = pInParseOrder->primaryChildNode;
    if (pChildInParseOrder->uNodeKind == ENODE_EXPR_BINARYOP) {
        static const u8 uExpectedRootPriorityIfLeftAsIs = 13u; // all unary ops have priority 13
        Assert_(is_token_symbol_or_keyword(pChildInParseOrder->pivotalToken.token));
        u8 uBinaryOp = u8(pChildInParseOrder->pivotalToken.token.uTokenPayloadAndKind >> 8);
        Assert_(tPayloadFlags[uBinaryOp] & BINOP);
        u8 uExpectedChildPriorityIfLeftAsIs = tBinopPriorities[uBinaryOp];
        if (uExpectedChildPriorityIfLeftAsIs < uExpectedRootPriorityIfLeftAsIs) { // Requires reordering

            //   "~ a * b"  
            //  raw, wrong:  ~(a * b)             ---->        (~a) * b
            //
            //                  ~  top unop      becomes           *  transformed pFollowingExpr returned as new root
            //                  |                ------>          / \ 
            // pFollowingExpr   *                         reorg  ~   b
            //                 / \                              /
            //                a   b                            a

            PreAstNode* pReorgUnary = pInParseOrder;
            pReorgUnary->primaryChildNode = pChildInParseOrder->primaryChildNode;
            pReorgUnary->iCountSubNodesOrNegErr = 1 + pChildInParseOrder->primaryChildNode->iCountSubNodesOrNegErr;
            if (pReorgUnary->secondaryChildNode) // may happen for '[...]' as unary op
                pReorgUnary->iCountSubNodesOrNegErr += 1 + pReorgUnary->secondaryChildNode->iCountSubNodesOrNegErr;
            if (pReorgUnary->iCountSubNodesOrNegErr > TOO_MANY_PREPARSER_NODES)
                pReorgUnary->iCountSubNodesOrNegErr = TOO_MANY_PREPARSER_NODES;

            PreAstNode* pReorgBinary = pChildInParseOrder;
            pReorgBinary->primaryChildNode = pReorgUnary;
            pReorgBinary->iCountSubNodesOrNegErr = 1 + pReorgUnary->iCountSubNodesOrNegErr;
            if (pReorgBinary->secondaryChildNode->iCountSubNodesOrNegErr >= 0) {
                pReorgBinary->iCountSubNodesOrNegErr += 1 + pReorgBinary->secondaryChildNode->iCountSubNodesOrNegErr;
                if (pReorgBinary->iCountSubNodesOrNegErr > TOO_MANY_PREPARSER_NODES)
                    pReorgBinary->iCountSubNodesOrNegErr = TOO_MANY_PREPARSER_NODES;
            } else {
                // cleanup: may already be the case ?
                pReorgBinary->iCountSubNodesOrNegErr = pReorgBinary->secondaryChildNode->iCountSubNodesOrNegErr;
            }
            return pReorgBinary;

            // Also not reorg => right-assoc with each other => - ~ a  is parsed as -(~a)
        }
    }
    return pInParseOrder;
}

local_func_inl PreAstNode* fixup_binary_op_node(PreAstNode* pInParseOrder, ParserParams& parserParams) 
{
    Assert_(pInParseOrder);
    Assert_(pInParseOrder->uNodeKind == ENODE_EXPR_BINARYOP);
    PreAstNode* pChildInParseOrder = pInParseOrder->secondaryChildNode;
    Assert_(pChildInParseOrder);
    
#if 0
    // dbg dbg
    ENodeKind eNodeKindThis = (ENodeKind)(pInParseOrder->uNodeKind);
    EKeyOrSymbTokenPayload ePayloadThis = (EKeyOrSymbTokenPayload)(u8(pInParseOrder->pivotalToken.token.uTokenPayload));

    PreAstNode* pFirstChildInParseOrder = pInParseOrder->primaryChildNode;
    ENodeKind eNodeKindChild1 = (ENodeKind)(pFirstChildInParseOrder->uNodeKind);
    EKeyOrSymbTokenPayload ePayloadChild1 = (EKeyOrSymbTokenPayload)(u8(pFirstChildInParseOrder->pivotalToken.token.uTokenPayload));

    PreAstNode* pSecondChildInParseOrder = pInParseOrder->secondaryChildNode;
    ENodeKind eNodeKindChild2 = (ENodeKind)(pSecondChildInParseOrder->uNodeKind);
    EKeyOrSymbTokenPayload ePayloadChild2 =  (EKeyOrSymbTokenPayload)(u8(pSecondChildInParseOrder->pivotalToken.token.uTokenPayload));
    // dbg dbg
#endif

    if (pChildInParseOrder->uNodeKind == ENODE_EXPR_BINARYOP) {
        Assert_(is_token_symbol_or_keyword(pInParseOrder->pivotalToken.token));
        Assert_(is_token_symbol_or_keyword(pChildInParseOrder->pivotalToken.token));
        u8 uTopBinaryOp = u8(pInParseOrder->pivotalToken.token.uTokenPayloadAndKind >> 8);
        u8 uBottomBinaryOp = u8(pChildInParseOrder->pivotalToken.token.uTokenPayloadAndKind >> 8);
        Assert_(tPayloadFlags[uTopBinaryOp] & BINOP);
        Assert_(tPayloadFlags[uBottomBinaryOp] & BINOP);
        u8 uExpectedRootPriorityIfLeftAsIs = tBinopPriorities[uTopBinaryOp];
        u8 uExpectedChildPriorityIfLeftAsIs = tBinopPriorities[uBottomBinaryOp];
        // Note : in the following test, <= instead of < is required for left-assoc, ie :
        //   a / b / c   in parse order is a / (b / c) ==> requires reorg as (a / b) / c
        if (uExpectedChildPriorityIfLeftAsIs <= uExpectedRootPriorityIfLeftAsIs) { // Requires reordering

            //  "a * b + c"
            //  with pInParseOrder being the node with '*'
            //     first: a
            //     second: '+'
            //         first: b
            //         second: c
            //  raw, wrong:  a * (b + c)  ---->  transformed, correct:  (a * b) + c
            //
            //         *  top binop     becomes           +  transformed pRHS returned as new root
            //        /?\               ------>          / \ 
            // pLHS  a   +  pRHS                 reorg  *   c    
            //          / \                            / \   
            //         b   c                          a   b  

            PreAstNode* pReorgWasTop = pInParseOrder;
            pReorgWasTop->secondaryChildNode = pChildInParseOrder->primaryChildNode;
            pReorgWasTop->iCountSubNodesOrNegErr = 2 +
                pReorgWasTop->primaryChildNode->iCountSubNodesOrNegErr +
                pReorgWasTop->secondaryChildNode->iCountSubNodesOrNegErr;
            PreAstNode* pReorgWasChild = pChildInParseOrder;
            pReorgWasChild->primaryChildNode = pReorgWasTop;
            if (pReorgWasChild->secondaryChildNode->iCountSubNodesOrNegErr >= 0) {
                pReorgWasChild->iCountSubNodesOrNegErr = 2 +
                    pReorgWasChild->primaryChildNode->iCountSubNodesOrNegErr +
                    pReorgWasChild->secondaryChildNode->iCountSubNodesOrNegErr;
            } else {
                // cleanup: may already be the case ?
                pReorgWasChild->iCountSubNodesOrNegErr = pReorgWasChild->secondaryChildNode->iCountSubNodesOrNegErr;
            }
            return pReorgWasChild;
        }
    }
    return pInParseOrder;
}

#define is_explicit_linebreak(pToken) (is_token_symbol_or_keyword(*pToken) && u8(pToken->uTokenPayloadAndKind >> 8) == ESYMB_BACKSLASH)

local_func Token* starting_true_eat_lines_while_eol_or_forced_break(ParserParams& parserParams, u16* outErr) {
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    do {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        platform_log_info("Getting next line searching for multiline continuation", true);
#endif
        if (pCurrent < parserParams.parserState.pLineTokenEnd) { // assumes forced-break case
            if (pCurrent + 1 != parserParams.parserState.pLineTokenEnd) {
                *outErr = PERR_TOKEN_ON_LINE_AFTER_EXPLICIT_LINEBREAK;
                return pCurrent;
            }
        }
        int iNonCommentTokenCount = parserParams.tokenizer.pGetNextLineTokensFn(&parserParams.parserState,
            parserParams.pSourceFile, parserParams.pOsFuncs, parserParams.tokenizer.pClosureData, outErr);
        if (iNonCommentTokenCount > 0) {
            pCurrent = parserParams.parserState.pCurrentToken;
            i32 iCurrentRelativeLine = parserParams.parserState.iCurrentlyParsedLine - parserParams.parserState.iInlinedRootStartLine;
            if (iCurrentRelativeLine & 0xFFC00000u) {
                // max 22 bits to encode relative line in token ref
                *outErr = PERR_TOO_MANY_LINES_IN_SAME_STATEMENT_OR_INLINED_CHAIN;
                return parserParams.parserState.pCurrentToken;
            }
            if (parserParams.parserState.uCurrentLineTabIndents < parserParams.parserState.uCurrentBlockTabIndents + 2) {
                *outErr = PERR_EXPECTED_LINE_CONTINUATION_FOUND_WRONG_INDENT;
                return parserParams.parserState.pCurrentToken;
            }
        } else if (iNonCommentTokenCount == 0) {
            *outErr = PERR_EXPECTED_LINE_CONTINUATION_FOUND_NONE;
            return parserParams.parserState.pCurrentToken;
        } else {
            Assert_(*outErr);
            return parserParams.parserState.pCurrentToken;
        }
    } while (is_explicit_linebreak(pCurrent));

    return pCurrent;
}


static const u16 EXPRESSION_TOO_DEEP = 255;

//Predecls

PreAstNode* try_parse_atomic_expression(ParserParams& parserParams,
    u16 uStartsWithComptime, u16 uDepthGuard, u16* outError);

PreAstNode* try_parse_expression(ParserParams& parserParams,
    bool bExplicitNamesAllowed, u16 uStartsWithComptime, u16 uDepthGuard, u16* outError);

PreAstNode* try_parse_expression_or_list(ParserParams& parserParams,
    bool bTrailingCommaAllowed, bool bExplicitNamesAllowed, u16 uStartsWithComptime, u16 uDepthGuard, u16* outError);

PreAstModifierNode* _parse_modifier(ParserParams& parserParams,
    u16 uStartsWithComptime, u16 uDepthGuard, u16* outError);

void _give_post_modifier_to_node(PreAstNode* pNode, PreAstModifierNode* pModifier);

// Utility function to call at the root of an ENODE_EXPRLIST_NODE chain
local_func PreAstNode* update_chain_node_counts(PreAstNode* pRoot, u16 uErrInFLight, ParserParams& parserParams)
{
    if (uErrInFLight) {
        i32 iErr = -i32(u32(uErrInFLight));
        PreAstNode* pCurrentNode = pRoot;
        while (pCurrentNode && pCurrentNode->uNodeKind == ENodeKind::ENODE_EXPRLIST_NODE) {
            pCurrentNode->iCountSubNodesOrNegErr = iErr;
            pCurrentNode = pCurrentNode->secondaryChildNode;
        }
    } else {
        ArenaRefPoint refBeforeVector = get_arena_ref_point(parserParams.preparsingArena);
        TmpStackOptiArray<PreAstNode*, 256u> vecNodes;
        vecNodes._alloc = FireAndForgetArenaAlloc(parserParams.preparsingArena);
        PreAstNode* pCurrentNode = pRoot;
        while (pCurrentNode && pCurrentNode->uNodeKind == ENodeKind::ENODE_EXPRLIST_NODE) {
            vecNodes.append(pCurrentNode);
            pCurrentNode = pCurrentNode->secondaryChildNode;
        }
        u32 uCountNodes = vecNodes.size();
        if (uCountNodes) {
            PreAstNode* pLastNode = vecNodes[uCountNodes-1];
            int iCumulCount = 1 + pLastNode->primaryChildNode->iCountSubNodesOrNegErr;
            if (pLastNode->secondaryChildNode)
                iCumulCount += 1 + pLastNode->secondaryChildNode->iCountSubNodesOrNegErr;
            if (iCumulCount > TOO_MANY_PREPARSER_NODES) {
                iCumulCount = TOO_MANY_PREPARSER_NODES;
            }
            pLastNode->iCountSubNodesOrNegErr = iCumulCount;
            for (size_t iterAbove = uCountNodes-1; iterAbove; --iterAbove) {
                PreAstNode* pCurrentNode = vecNodes[iterAbove-1];
                iCumulCount += 2 + pCurrentNode->primaryChildNode->iCountSubNodesOrNegErr;
                if (iCumulCount > TOO_MANY_PREPARSER_NODES) {
                    iCumulCount = TOO_MANY_PREPARSER_NODES;
                }
                pCurrentNode->iCountSubNodesOrNegErr = iCumulCount;
#if DEBUG_PREPARSER_NODECOUNTS
                if (iCumulCount < TOO_MANY_PREPARSER_NODES)
                    debug_count_pre_ast_nodes(pCurrentNode);
#endif
            }
        }
        reset_arena_to(refBeforeVector, parserParams.preparsingArena);
    }
    return pRoot;
}


// Trying to parse remaining tokens as part of same expression,
//   once parse_expression has already found a valid possible leaf on left hand side.
// SHALL start already positionned on a valid, and non-explicit-linebreak token
// May return LHS param as-is without error if nothing started to match an expression continuation (otherwise error)
// if no error occurred, will leave parser state either to EOL or to a non-explicit line-break.
local_func PreAstNode* try_parse_expression_continuation(ParserParams& parserParams,
    PreAstNode* pLastAtomicLeftHandSide, bool bExplicitNamesAllowed, u16 uDepthGuard, u16* outError)
{
    if (uDepthGuard < EXPRESSION_TOO_DEEP) {

        Token* pCurrent = parserParams.parserState.pCurrentToken;
        Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);        
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        debug_print_indent(uDepthGuard*2);
        platform_log_info("now try parsing expression continuation, considering ", false);
        debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
        platform_log_info("", true);
#endif
        u8 uTokenCategory = pCurrent->uTokenPayloadAndKind & TOKEN_CATEGORY_MASK;

        if ((uTokenCategory == TOKEN_CATEGORY_IDENTIFIER) && (pCurrent->uTokenPayloadAndKind & AT_BEFORE)) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard*2 + 1);
            platform_log_info("found post-modifier", true);
#endif
            PreAstModifierNode* postModifier = _parse_modifier(parserParams, false, uDepthGuard, outError);
            _give_post_modifier_to_node(pLastAtomicLeftHandSide, postModifier);
            if (*outError) {
                TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams);
                PreAstNode* errWrapup = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken);
                _wrapup_pre_node_primary(errWrapup, *pLastAtomicLeftHandSide);
                return errWrapup;
            }
            return try_parse_expression_continuation(parserParams, pLastAtomicLeftHandSide,
                bExplicitNamesAllowed, uDepthGuard+1, outError);
        }

        if (is_token_symbol_or_keyword(*pCurrent)) {
            u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
            if (tPayloadFlags[uPayload] == 0) {
                PreAstNode* pErrUnk; make_err_pre_node_unrecognized_token(pErrUnk);
                return pErrUnk;
            }

            // only allows for explicit-param-name-assignop in special circumstances
            if (uPayload == ETOK_SINGLE_EQ && bExplicitNamesAllowed) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found '=' symbol when explicit names are allowed in expression => continues as-if binop", true);
#endif
                bExplicitNamesAllowed = false; // and only once per expression
                goto on_parse_binaryop;        // Note: '=' as a binop has lowest possible priority of 0

            // standard-case 'binary op'
            } else if (tPayloadFlags[uPayload] & BINOP) { on_parse_binaryop:
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found binop continuation => spawn standard binop node vs previous as operandA, then expects expression as operandB", true);
#endif
                reference_current_token_as(binop);
                PreAstNode* pBinop = _prepare_pre_node(parserParams, ENODE_EXPR_BINARYOP, 0, binop);
                _wrapup_pre_node_primary(pBinop, *pLastAtomicLeftHandSide);
                pCurrent = ++parserParams.parserState.pCurrentToken;
                from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pBinop, secondary, 0, "Looking for RHS of binary operator", pBinop);
                PreAstNode* pFollowingExpr = try_parse_expression(parserParams, bExplicitNamesAllowed, 0, uDepthGuard+1, outError);
                check_and_wrapup_pre_node_err_if_null(pBinop, secondary, pFollowingExpr, "Expected expression as RHS for binary operator");
                return fixup_binary_op_node(pBinop, parserParams);

            // special-superbinops
            } else if (tPayloadFlags[uPayload] & SPE_B) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found special binop continuation", true);
#endif
                SpeContinuationParsingProc_Sign* pContinuationFn = tSpeContinuationParsingFn[uPayload];
                Assert_(pContinuationFn);  // otherwise should not be flaggued as such
                PreAstNode* pSpecial = pContinuationFn(parserParams, pLastAtomicLeftHandSide, uDepthGuard, outError);
                if (*outError)
                    return pSpecial;

                // Note: "special" continuations, such as function invocation starting with '(', do not pursue
                //   the recursive parsing of expression continuations after themselves, as other binops do
                //   => if need be, force parsing for continuations again.
                pCurrent = ++parserParams.parserState.pCurrentToken;
                if (pCurrent < parserParams.parserState.pLineTokenEnd) {
                    from_current_token_expects_on_line_or_break_otherwise_err_around(pCurrent, *pSpecial, 0, 
                        "while trying to find further continuation after special binop");
                    return try_parse_expression_continuation(parserParams, pSpecial, bExplicitNamesAllowed, uDepthGuard+1, outError);
                } else
                    return pSpecial;

                // Also note: they are all supertight left-associative, and as such will bind whatever expression
                //   was given as 'pLastAtomicLeftHandSide', and be given themselved *in full* as
                //   'pLastAtomicLeftHandSide' of their continuation,
                //   and not be affected by unary or regular binary op reorgs
                // => example:
                //      my.namespace.funcReturningArray(3, 5)[6].property1
                // will be parsed as :
                //      ((((my.namepsace).funcReturningArray)(3, 5))[6]).property1

            }
        }

        // No expression continuation found => return first expression as-is
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
        debug_print_indent(uDepthGuard*2 + 1);
        platform_log_info("no continuation found => return as is", true);
#endif
        return pLastAtomicLeftHandSide;

    } else {
        TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams);
        PreAstNode* pErrWrap = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken);
        _wrapup_pre_node_primary(pErrWrap, *pLastAtomicLeftHandSide);
        PreAstNode* pErrTooDeep; make_err_pre_node_other(pErrTooDeep, PERR_EXPRESSION_TOO_DEEP, "Too deeply nested expression");
        _wrapup_pre_node_secondary(pErrWrap, *pErrTooDeep);
        return pErrWrap;
    }
}

local_func PreAstNode* _parse_enclosed_finalization_as_params(ParserParams& parserParams, PreAstNode* pParamsList,
    u8 uNodeKind, u8 uExpectedClosingToken, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2+1);
    platform_log_info("Special handling of parens/bracket/curly-enclosing", true);
#endif
    PreAstNode* pEnclosingParams = _prepare_pre_node(parserParams, uNodeKind, 0, _on_error_get_ref_from_position(parserParams));
    if (pParamsList) {
        _wrapup_pre_node_primary(pEnclosingParams, *pParamsList);
    }
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == uExpectedClosingToken) {
            reference_current_token_as(closing);
            pEnclosingParams->pivotalToken = closing;
        } else {
            pEnclosingParams->pivotalToken = _on_error_get_ref_from_position(parserParams);
            PreAstNode* errExpected; make_err_pre_node_expected_vs_found(errExpected, uExpectedClosingToken, "Expected closing token");
            _wrapup_pre_node_secondary(pEnclosingParams, *errExpected);
            return pEnclosingParams;
        }
    } else {
        PreAstNode* errExpected; make_err_pre_node_expected_found_eol(errExpected, uExpectedClosingToken, "Expected closing token");
        _wrapup_pre_node_secondary(pEnclosingParams, *errExpected);
        return pEnclosingParams;
    }
    return pEnclosingParams;
}

// Starting right after type decl token => needs to check for possibly multiline continuation
// Shall not return null.
local_func_inl PreAstNode* _parse_vardecl_rhs(ParserParams& parserParams, bool bAllowMultiValues, u16 uDepthGuard, u16* outError) 
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_self(pCurrent, 0, "Looking for RHS of var-delaration-form");
    PreAstNode* pRhsTypeExpression = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (*outError)
        return pRhsTypeExpression;
    pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_SINGLE_EQ) {
            reference_current_token_as(initialAssign);
            PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_SINGLE_EQ, 0, initialAssign);
            if (pRhsTypeExpression)
                _wrapup_pre_node_primary(pResult, *pRhsTypeExpression);
            pCurrent = ++parserParams.parserState.pCurrentToken;
            from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pResult, secondary, 0, "Looking for initial value", pResult);
            PreAstNode* pInitialValue = bAllowMultiValues ?
                try_parse_expression_or_list(parserParams, false, false, 0, uDepthGuard+1, outError) :
                try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
            check_and_wrapup_pre_node_err_if_null(pResult, secondary, pInitialValue, "Expecting expression as initial value of declaration");
            return pResult;
        }
    }
    if (!pRhsTypeExpression)
        make_err_pre_node_expected_vs_found(pRhsTypeExpression, 0, "Expecting type expression or '= <initial value>' for declaration");
    return pRhsTypeExpression;
}

local_func PreAstNode* try_parse_expression_allowing_vardecl(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
    PreAstNode* pStartingExpr = try_parse_expression(parserParams, false, 0, uDepthGuard, outError);
    if (*outError)
        return pStartingExpr;
    else if (!pStartingExpr)
        return 0; // nominal null when not started to match.

    Token* pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_VARDECL) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
            debug_print_indent(uDepthGuard*2+1);
            platform_log_info("keyword 'as' found => defining vardecl-expression", true);
#endif
            reference_current_token_as(tokenAs);
            PreAstNode* pVarDecl = _prepare_pre_node(parserParams, ENODE_VARIABLE_DECL, 0, tokenAs);
            _wrapup_pre_node_primary(pVarDecl, *pStartingExpr);
            pCurrent = ++parserParams.parserState.pCurrentToken;
            PreAstNode* pTypeAndInitValue = _parse_vardecl_rhs(parserParams, false, uDepthGuard+1, outError);
            Assert_(pTypeAndInitValue); // _parse_vardecl_rhs should not return null
            _wrapup_pre_node_secondary(pVarDecl, *pTypeAndInitValue);
            return pVarDecl;
        }
    }
    return pStartingExpr;
}

local_func PreAstNode* try_parse_expression_or_list_allowing_vardecl(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
    PreAstNode* pSingleExpr = try_parse_expression_allowing_vardecl(parserParams, uDepthGuard, outError);
    if (*outError)
        return pSingleExpr;
    else if (!pSingleExpr)
        return 0; // nominal null when not started to match.

    Token* pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        // any explicit linebreak at this point should already have been eaten by try_parse_expression
        // eat_explicit_linebreak_and_find_next_token(pCurrent);
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_COMMA) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard*2);
            platform_log_info("Found first comma in chain (allowing vardecls).", true);
#endif
            PreAstNode* pChainRoot = 0;
            PreAstNode* pLastInChain = 0;
            i32 iElementCount = 0;
            do {
                reference_current_token_as(comma);
                {
                    PreAstNode* pNewInChain = _prepare_pre_node(parserParams, ENODE_EXPRLIST_NODE, 0, comma);
                    pNewInChain->primaryChildNode = pSingleExpr;
                    if (pLastInChain)
                        pLastInChain->secondaryChildNode = pNewInChain;
                    else
                        pChainRoot = pNewInChain;
                    pLastInChain = pNewInChain;
                }
                pCurrent = ++parserParams.parserState.pCurrentToken;
                from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pLastInChain, secondary, 0,
                    "Looking after comma", update_chain_node_counts(pChainRoot, *outError, parserParams));
                iElementCount++;
                if (iElementCount >= TOO_MANY_CHAINED_ELEMENTS) {
                    make_err_pre_node_other(pLastInChain->secondaryChildNode, PERR_TOO_MANY_ELEMENTS_IN_LIST, "Too many elements in list");
                    break;
                }
                pSingleExpr = try_parse_expression_allowing_vardecl(parserParams, uDepthGuard, outError);
                if (*outError || !pSingleExpr)
                    break;
                pCurrent = parserParams.parserState.pCurrentToken;

            } while (pCurrent < parserParams.parserState.pLineTokenEnd &&
                     is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_COMMA);

            if (pSingleExpr) {
                pLastInChain->secondaryChildNode = pSingleExpr;
            } else {
                PreAstNode* inErrorNode; make_err_pre_node_expected_vs_found(inErrorNode, 0, "Expecting var-decl-expression after comma");
                pLastInChain->secondaryChildNode = inErrorNode;
            }
            pChainRoot = update_chain_node_counts(pChainRoot, *outError, parserParams);
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
//            platform_log_info("Showing result of list pre-parsing:", true);
//            debug_print_ast_node(pChainRoot, 1, parserParams.pOsFuncs, parserParams.pSourceFile);
#endif
            return pChainRoot;
        }
    }

    return pSingleExpr;
}

local_func PreAstNode* parse_complex_lit_contents(ParserParams& parserParams, u8 uPayload, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of { or #[ or #{ or #( contents (complex-lit expressions or pseudo-binops)", true);
#endif
    PreAstNode* pParamsList = try_parse_expression_or_list(parserParams, true, uPayload == ETOK_OPENING_STRUCT_LIT, 0, uDepthGuard+1, outError);
    if (*outError) {
        return pParamsList;
    } else {
        u8 uClosingToken = ETOK_CLOSING_CURLY;
        if (uPayload == ETOK_OPENING_ARRAY_LIT) {
            uClosingToken = ETOK_CLOSING_BRACKET;
        } else if (uPayload == ETOK_OPENING_MAP_LIT) {
            uClosingToken = ETOK_CLOSING_PARENS;
        }
        PreAstNode* pInitParams = _parse_enclosed_finalization_as_params(parserParams, pParamsList, ENODE_SUBEXPR_WRAPPER, uClosingToken, uDepthGuard, outError);
        Assert_(pInitParams); // _parse_enclosed_finalization_as_params should not return null
        return pInitParams;
    }
}

local_func PreAstNode* parse_any_noleft_complex_literal(ParserParams& parserParams, ENodeKind eKind, u16 uDepthGuard, u16* outError)
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
    reference_current_token_as(opening);
    PreAstNode* pExpr = _prepare_pre_node(parserParams, u8(eKind), 0, opening);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pExpr, primary, 0, "Looking for contents of complex literal", pExpr);
    PreAstNode* pFollowingExpr = parse_complex_lit_contents(parserParams, uPayload, uDepthGuard, outError);
    if (!pFollowingExpr) {
        make_err_pre_node_expected_vs_found(pFollowingExpr, 0, "expected: expression or list");
    }
    _wrapup_pre_node_primary(pExpr, *pFollowingExpr);
    return pExpr;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(special_load)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> Special handling of 'load' expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(panLoad);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_LOAD, 0, panLoad);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pResult, primary, 0, "Looking for filename after '#load' directive", pResult);
    PreAstNode* pWhatToLoadExpr = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    check_and_wrapup_pre_node_err_if_null(pResult, primary, pWhatToLoadExpr, "Expecting filename after '#load' directive");
    return pResult;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(no_left_arr_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> array literal as an isolated expression (hopefully with implicit or inferrable type)", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_ARRAY_LIT);
    return parse_any_noleft_complex_literal(parserParams, ENODE_EXPR_ARRAYINIT, uDepthGuard, outError);
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(no_left_set_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> set literal as an isolated expression (hopefully with implicit or inferrable type)", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_SET_LIT);
    return parse_any_noleft_complex_literal(parserParams, ENODE_EXPR_SETINIT, uDepthGuard, outError);
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(no_left_map_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> map literal as an isolated expression (hopefully with implicit or inferrable type)", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_MAP_LIT);
    return parse_any_noleft_complex_literal(parserParams, ENODE_EXPR_MAPINIT, uDepthGuard, outError);
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(no_left_st_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> struct literal as an isolated expression (hopefully with implicit type)", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_CURLY);
    return parse_any_noleft_complex_literal(parserParams, ENODE_EXPR_CURLYINIT, uDepthGuard, outError);
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(parentised)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> start of parentized expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    Assert_(is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_OPENING_PARENS);
    reference_current_token_as(openingParens);
    PreAstNode* parentizedExpr = _prepare_pre_node(parserParams, ENODE_EXPR_PARENTISED, 0, openingParens);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, parentizedExpr, primary, 0, "Looking after opening parens", parentizedExpr);
    PreAstNode* exprWithin = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError); // TODO: expression-or-list, for lambda syntax '(a, b, c -> d, e)' ??? 
    if (!exprWithin)
        make_err_pre_node_expected_vs_found(exprWithin, 0, "expected expression within parens");
    check_and_wrapup_pre_node(parentizedExpr, primary, *exprWithin);
    pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_CLOSING_PARENS) {
            reference_current_token_as(closingParens);
            parentizedExpr->secondaryToken = closingParens;
        } else {
            PreAstNode* expectedErr; make_err_pre_node_expected_vs_found(expectedErr, ESYMB_CLOSING_PARENS, "Expected closing parens");
            _wrapup_pre_node_secondary(parentizedExpr, *expectedErr);
        }
    } else {
        PreAstNode* expectedErr; make_err_pre_node_expected_found_eol(expectedErr, ESYMB_CLOSING_PARENS, "Expected closing parens");
        _wrapup_pre_node_secondary(parentizedExpr, *expectedErr);
    }
    return parentizedExpr;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(no_left_dot)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> dot without a left-hand expression => parsing as implicit descent", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_OPENING_PARENS);
    reference_current_token_as(singleDot);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_DOT_DESCENT, 0, singleDot);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    // We'll fill 'secondary' here to have same layout as post-parsing of dot-spe-binop, only without a lhs 'primary'.
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pResult, secondary, 0,
        "Looking for argument to standalone dot", pResult);
    PreAstNode* pAtomicFollow = try_parse_atomic_expression(parserParams, 0, uDepthGuard+1, outError);
    check_and_wrapup_pre_node_err_if_null(pResult, secondary, pAtomicFollow, "expected atomic expression after standalone dot");
    return pResult;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(special_token)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> special-token expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(specialToken);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_SPECIAL, 0, specialToken);
    return pResult;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(ternary_if)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> ternary-if expression", true);
#endif
    // TODO
    PreAstNode* pResult;
    make_err_pre_node_other(pResult, FERR_NOT_YET_IMPLEMENTED, "ternary_if not yet implemented");
    return pResult;
}

local_func_inl PreAstNode* _try_parse_proc_params_out(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("looking for optional proc params out", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    from_current_token_allows_on_line_or_break_otherwise_null_self(pCurrent, "while looking for optional out params");
    if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_ARROW) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        debug_print_indent(uDepthGuard*2 + 1);
        platform_log_info("found '->' => parsing the following as proc params out", true);
#endif
        reference_current_token_as(arrow);
        PreAstNode* paramsOut = _prepare_pre_node(parserParams, ENODE_PROCPARAMS_OUT, 0, arrow);
        pCurrent = ++parserParams.parserState.pCurrentToken;
        from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, paramsOut, primary, 0, "Looking for out parameters", paramsOut);
        PreAstNode* pParamsDeclList = try_parse_expression_or_list_allowing_vardecl(parserParams, uDepthGuard+1, outError);
        check_and_wrapup_pre_node_err_if_null(paramsOut, primary, pParamsDeclList, "Expecting return parameters declaration after ->");
        return paramsOut;
    }
    return 0; // nominal case : no output params
}

local_func_inl PreAstNode* _parse_proc_params_in(ParserParams& parserParams, u16 uDepthGuard, u16* outError)  
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("parsing the following as proc params in", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_self(pCurrent, 0, "Looking for function parameter declaration");
    PreAstNode* pParamsDeclList = try_parse_expression_or_list_allowing_vardecl(parserParams, uDepthGuard+1, outError);
    if (*outError) {
        return pParamsDeclList;
    } else {
        return _parse_enclosed_finalization_as_params(parserParams, pParamsDeclList,
            ENODE_PROCPARAMS_IN, ETOK_CLOSING_PARENS, uDepthGuard, outError);
    }
}

local_func_inl PreAstNode* _parse_proc_params_all(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_self(pCurrent, 0, "Looking for opening parens");
    if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_OPENING_PARENS) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        debug_print_indent(uDepthGuard*2 + 1);
        platform_log_info("correctly found opening parens starting proc params definition", true);
#endif
        reference_current_token_as(openingParens);
        PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_PROCPARAMS_WRAP_ALL, 0, openingParens);
        pCurrent = ++parserParams.parserState.pCurrentToken;
        PreAstNode* pParamsIn = _parse_proc_params_in(parserParams, uDepthGuard+1, outError);
        check_and_wrapup_pre_node(pResult, primary, *pParamsIn);
        pCurrent = ++parserParams.parserState.pCurrentToken;
        PreAstNode* pOptParamsOut = 0;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            pOptParamsOut = _try_parse_proc_params_out(parserParams, uDepthGuard+1, outError);
            if (pOptParamsOut)
                _wrapup_pre_node_secondary(pResult, *pOptParamsOut);
        }
        return pResult;
    } else {
        PreAstNode* expectedErr; make_err_pre_node_expected_vs_found(expectedErr, ETOK_OPENING_PARENS,
            "Expected opening parens for proc input parameters");
        return expectedErr;
    }
}

local_func_inl PreAstNode* _try_parse_where_clause(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("looking for optional where clause", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_WHERE) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard*2 + 1);
            platform_log_info("found 'where' => parsing the following as where clauses", true);
#endif
            reference_current_token_as(whereClause);
            PreAstNode* paramsOut = _prepare_pre_node(parserParams, ENODE_PROCPARAMS_WHERE_CLAUSE, 0, whereClause);
            pCurrent = ++parserParams.parserState.pCurrentToken;
            from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, paramsOut, primary, 0, "Looking for clauses after where", paramsOut);
            PreAstNode* pParamsDeclList = try_parse_expression_or_list(parserParams, false, false, 0, uDepthGuard+1, outError);
            check_and_wrapup_pre_node_err_if_null(paramsOut, primary, pParamsDeclList, "Expecting clauses after where");
            return paramsOut;
        }
    }
    return 0; // nominal case : no where clause
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(proclike_decl)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> proclike declaration expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(procKind);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_PROCLIKE_DEF, 0, procKind);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    PreAstNode* pParamsPart = _parse_proc_params_all(parserParams, uDepthGuard+1, outError);
    Assert_(pParamsPart); // _parse_proc_params_all shall not return null
    check_and_wrapup_pre_node(pResult, primary, *pParamsPart);
    PreAstNode* pOptWhereClause = _try_parse_where_clause(parserParams, uDepthGuard+1, outError);
    if (pOptWhereClause)
        _wrapup_pre_node_secondary(pResult, *pOptWhereClause);
    return pResult;
}


// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(enum_decl)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> enum declaration expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(enumKey);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_OTHER_DEF, 0, enumKey);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_allows_break_if_multiline_err_wraparound(pCurrent, *pResult, "While looking for optional explicit type params on enum def");
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        PreAstNode* pOptType = try_parse_expression(parserParams, false, 0u, uDepthGuard + 1u, outError);
        check_and_wrapup_pre_node(pResult, primary, *pOptType);
    }
    return pResult;
}


// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(structlike_decl)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> struct-like declaration expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(structKey);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_OTHER_DEF, 0, structKey);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_allows_break_if_multiline_err_wraparound(pCurrent, *pResult, "While looking for optional polymorphic params on struct def");
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_OPENING_PARENS) {
            reference_current_token_as(openingParens);
            PreAstNode* pOptStructPolyParams = _prepare_pre_node(parserParams, ENODE_PROCPARAMS_WRAP_ALL, 0, openingParens);
            pCurrent = ++parserParams.parserState.pCurrentToken;
            PreAstNode* pParamsIn = _parse_proc_params_in(parserParams, uDepthGuard+1, outError);
            check_and_wrapup_pre_node(pOptStructPolyParams, primary, *pParamsIn);
            check_and_wrapup_pre_node(pResult, primary, *pOptStructPolyParams);
            pCurrent = ++parserParams.parserState.pCurrentToken;
        }
    }
    return pResult;
}


// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_expr_parsing_fn(set_decl)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> set-type expression", true);
#endif
    // TODO
    PreAstNode* pResult;
    make_err_pre_node_other(pResult, FERR_NOT_YET_IMPLEMENTED, "set_decl not yet implemented");
    return pResult;
}

local_func PreAstNode* try_parse_atomic_expression(ParserParams& parserParams,
    u16 uStartsWithComptime, u16 uDepthGuard, u16* outError)
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    PreAstNode* pFirstExpr = 0;

    // in case current token is a keyword or symbol, we can know what to do with the help of the 'payload' tables...
    //
    if (is_token_symbol_or_keyword(*pCurrent)) {
        u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
        if (tPayloadFlags[uPayload] == 0) {
            PreAstNode* pErrUnk; make_err_pre_node_unrecognized_token(pErrUnk);
            return pErrUnk;
        }
        
        Assert_(uPayload != ETOK_DOLLAR); // nobody should have invoked us withouit first eating those...

        // if that keyword or symbol is indeed flaggued as 'expression', then we use its associated expression-parsing function
        if (tPayloadFlags[uPayload] & EXPR_) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard*2 + 1);
            platform_log_info("found special symbol-or-keyword starting expression", true);
#endif
            Assert_(0 == (tPayloadFlags[uPayload] & UN_OP)); // unary ops are not allowed as atomics
            ExpressionParsingProc_Sign* pExprParsingFn = tExpressionParsingFn[uPayload];
            Assert_(pExprParsingFn); // otherwise should not be flaggued as such
            pFirstExpr = pExprParsingFn(parserParams, uDepthGuard, outError);
            Assert_(pFirstExpr); // special expr parsing functions should not return null
            pFirstExpr->uNodeFlags |= uStartsWithComptime;
            if (*outError)
                return pFirstExpr;
            // several special expr parsing functions do not check for break-after by themselves
            pCurrent = parserParams.parserState.pCurrentToken;
            from_current_token_allows_break_if_multiline_err_wraparound(pCurrent, *pFirstExpr, "after special parse expression");
        }

    } else {

        u8 uCategory = pCurrent->uTokenPayloadAndKind & TOKEN_CATEGORY_MASK;
        switch (uCategory) {

            // Is the current token an identifier ?
            case TOKEN_CATEGORY_IDENTIFIER: {
                Assert_((pCurrent->uTokenPayloadAndKind & 0x03u) != DOLLAR_BEFORE); // this '$-before' concept should be deprecated in tokenizer
                if ((pCurrent->uTokenPayloadAndKind & 0x03u) != AT_BEFORE) { // non @-identifier => user ident or reserved word
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                    debug_print_indent(uDepthGuard*2 + 1);
                    platform_log_info("found leaf: identifier", true);
#endif
                    reference_current_token_as(ident);
                    pFirstExpr = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_IDENTIFIER, uStartsWithComptime, ident);
                    pFirstExpr->primaryPayload = u64(pCurrent->uTokenPayloadAndKind >> 8);
                } // otherwise not-an expression (all @-identifiers are parsed as 'modifiers')
            } break;

            // Is the current token a natural number literal ?
            case TOKEN_CATEGORY_NATNUM: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found leaf: natural number literal", true);
#endif
                reference_current_token_as(literal);
                pFirstExpr = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_NATURAL_NUMBER_LITERAL, uStartsWithComptime, literal);
                bool bIsEmbedded = make_nat_payload_from_token_return_is_small(literal, parserParams, &(pFirstExpr->primaryPayload), outError);
                if (bIsEmbedded)
                    pFirstExpr->uNodeFlags |= EPRENODEFLAG_LITERAL_IS_EMBEDDED64;
            } break;

            // Is the current token a floating point literal ?
            case TOKEN_CATEGORY_FLOATINGPT: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found leaf: floating point literal", true);
#endif
                reference_current_token_as(literal);
                pFirstExpr = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_FLOATING_POINT_LITERAL, uStartsWithComptime, literal);
                bool bIsEmbedded = make_float_payload_from_token_return_is_small(literal, parserParams, &(pFirstExpr->primaryPayload), outError);
                if (bIsEmbedded)
                    pFirstExpr->uNodeFlags |= EPRENODEFLAG_LITERAL_IS_EMBEDDED64;
            } break;

            // Is the current token a string (or codepoint) literal ?
            case TOKEN_CATEGORY_STRING: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found leaf: string-related literal", true);
#endif
                reference_current_token_as(literal);
                if ((pCurrent->uTokenPayloadAndKind & TOKEN_KIND_MASK) == ETOK_KIND_CODEPOINT) {
                    pFirstExpr = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_CODEPOINT_LITERAL, uStartsWithComptime, literal);
                    pFirstExpr->primaryPayload = u64(literal.token.uTokenPayloadAndKind >> 8);
                    pFirstExpr->uNodeFlags |= EPRENODEFLAG_LITERAL_IS_EMBEDDED64;
                } else {
                    Assert_((pCurrent->uTokenPayloadAndKind & TOKEN_KIND_MASK) == ETOK_KIND_STRING);
                    pFirstExpr = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_STRING_LITERAL, uStartsWithComptime, literal);
                    bool bIsEmbedded = make_string_payload_from_token_return_is_small(literal,
                        parserParams, &(pFirstExpr->primaryPayload), outError);
                    if (bIsEmbedded)
                        pFirstExpr->uNodeFlags |= EPRENODEFLAG_LITERAL_IS_EMBEDDED64;
                }
            } break;
        }
    }

    // returns the atomic expression which was found, or null...
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    if (!pFirstExpr) {
        debug_print_indent(uDepthGuard*2 + 1);
        platform_log_info("not matching any atomic...", true);
    }
#endif
    return pFirstExpr;
}

#if 0
local_func_inl PreAstNode* _parse_sliced_expr(Token* pCurrent, ParserParams& parserParams, PreAstNode* pSliceLHS, u16 uDepthGuard, u16* outError)
{
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2+1);
    platform_log_info("Parsing slicing parameters with ", false);
    debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent);
    platform_log_info("", true);
#endif
    reference_current_token_as(slicing);
    PreAstNode* slicedExpr = _prepare_pre_node(parserParams, ENODE_SLICE, 0, slicing);
    if (pSliceLHS) // allowed to be null
        _wrapup_pre_node_primary(slicedExpr, *pSliceLHS);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, slicedExpr, secondary, 0, "expecting RHS of slicing params or closing brackets", slicedExpr);
    PreAstNode* pSliceEnd = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (pSliceEnd) // allowed to be null
        _wrapup_pre_node_secondary(slicedExpr, *pSliceEnd);
    else
        Assert_(0 == *outError);
    return slicedExpr;
}

local_func_inl PreAstNode* _parse_indexed_type_or_slice_as_unary_op(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2+1);
    platform_log_info("Parsing following expression as indexed type with ", false);
    debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent);
    platform_log_info("", true);
#endif
    reference_current_token_as(openingBracket);
    PreAstNode* indexedTypeExpr = _prepare_pre_node(parserParams, ENODE_EXPR_UNARYOP, 0, openingBracket);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, indexedTypeExpr, primary, 0, "expecting indexed-type parameters and closing brackets", indexedTypeExpr);
    PreAstNode* pParam = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (0 == *outError) {
        pCurrent = parserParams.parserState.pCurrentToken;
        from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, indexedTypeExpr, primary, 0, "expecting indexed-type parameters and closing brackets", indexedTypeExpr);
        if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x00) {
            if (u8(pCurrent->uTokenPayload) == u8(ESYMB_SLICE) || u8(pCurrent->uTokenPayload) == EMS_SLICE_TO_EXCL) {
                pParam = _parse_sliced_expr(pCurrent, parserParams, pParam, uDepthGuard, outError);
                Assert_(pParam);
                pCurrent = parserParams.parserState.pCurrentToken;
            }
        }
    }

    if (*outError) {
        // we fill "secondary" slot here since, as an "unaryop", its primary will be reserved for actual operand...
        _wrapup_pre_node_secondary(indexedTypeExpr, *pParam);
    } else {
        PreAstNode* pIndexedTypeParams = _parse_enclosed_finalization_as_params(parserParams, pParam,
            ENODE_EXPR_INDEXING_PARAMS, u8(']'), uDepthGuard, outError);
        Assert_(pIndexedTypeParams); // _parse_enclosed_finalization_as_params should not return null
        // we fill "secondary" slot here since, as an "unaryop", its primary will be reserved for actual operand...
        _wrapup_pre_node_secondary(indexedTypeExpr, *pIndexedTypeParams);
    }
    return indexedTypeExpr;
}
#endif

local_func_inl PreAstNode* _parse_modifier_params(Token* pCurrent, ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
    from_current_token_expects_on_any_line_otherwise_err_self(pCurrent, 0, "Looking after opening parens");
    PreAstNode* pParamsList = try_parse_expression_or_list(parserParams, false, true, 0, uDepthGuard, outError);
    if (*outError)
        return pParamsList;
    PreAstNode* pParams = _parse_enclosed_finalization_as_params(parserParams, pParamsList,
        ENODE_SUBEXPR_WRAPPER, ESYMB_CLOSING_PARENS, uDepthGuard, outError);
    if (0 == *outError) {
        Assert_(pParams->iCountSubNodesOrNegErr >= 0);
        parserParams.parserState.uCountParamNodesWithinModifiers = 1 + pParams->iCountSubNodesOrNegErr;
    }
    return pParams;
}

local_func PreAstModifierNode* _parse_modifier(ParserParams& parserParams, u16 uStartsWithComptime, u16 uDepthGuard, u16* outError)
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2);
    platform_log_info("Parsing modifier with ", false);
    debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
    platform_log_info("", true);
#endif
    if (uStartsWithComptime) {
        debug_print_preparse_err("Comptime not allowed before modifier");
        TokenRef errTok = _on_error_get_ref_from_position(parserParams);
        pre_parser_set_error(PERR_COMPTIME_NOT_ALLOWED_THERE, errTok);
        return 0;
    }
    reference_current_token_as(modifierTok);
    PreAstModifierNode* pModifier = (PreAstModifierNode*)alloc_from(parserParams.preparsingArena, sizeof(PreAstModifierNode), 8);
    pModifier->pivotalToken = modifierTok;
    pModifier->next = 0;
    if (0 == (pCurrent->uTokenPayloadAndKind & TOKENKINDFLAG_SPACE_AFTERWARDS)) {
        Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
        pCurrent = ++parserParams.parserState.pCurrentToken;
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_OPENING_PARENS) {
            reference_current_token_as(openingParens);
            pModifier->optOpeningParens = openingParens;
            pCurrent = ++parserParams.parserState.pCurrentToken;
            pModifier->valueExpression = _parse_modifier_params(pCurrent, parserParams, uDepthGuard+1, outError);
            pCurrent = parserParams.parserState.pCurrentToken;
        } else
            goto otherwiseNoParam;
    }
    else
    {
        pCurrent = ++parserParams.parserState.pCurrentToken;
    otherwiseNoParam:
        pModifier->valueExpression = 0;
        pModifier->optOpeningParens = pivotTokenNone;
    }
    parserParams.parserState.uCountModifierNodes++;
    if (*outError)
        return pModifier;

    if (pCurrent < parserParams.parserState.pLineTokenEnd && is_explicit_linebreak(pCurrent))
    {
        pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            PreAstNode* errDuringMultiline; make_err_pre_node_during_multiline(errDuringMultiline, "While following explicit linebreak after having parsed modifier");
            if (pModifier->valueExpression) {
                TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams);
                PreAstNode* errWrapper = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken);
                _wrapup_pre_node_primary(errWrapper, *pModifier->valueExpression);
                _wrapup_pre_node_secondary(errWrapper, *errDuringMultiline);
                pModifier->valueExpression = errWrapper;
            } else {
                pModifier->valueExpression = errDuringMultiline;
            }
        }
    }
    return pModifier;
}

local_func void _parse_adding_in_flight_modifier_to(ParserParams& parserParams, u16 uStartsWithComptime, u16 uDepthGuard, u16* outError)
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    PreAstModifierNode* pModifier = _parse_modifier(parserParams, uStartsWithComptime, uDepthGuard, outError);
    if (pModifier) {
        pModifier->next = parserParams.parserState.inFlightModifierNodes;
        parserParams.parserState.inFlightModifierNodes = pModifier;
    }
}

local_func void _give_in_flight_modifier_counts_to_statement(PreStatement* pStatement, ParserParams& parserParams)
{
    pStatement->uCountModifierNodes += parserParams.parserState.uCountModifierNodes;
    pStatement->uCountParamNodesWithinModifiers += parserParams.parserState.uCountParamNodesWithinModifiers;
    parserParams.parserState.uCountModifierNodes = 0;
    parserParams.parserState.uCountParamNodesWithinModifiers = 0;
}

local_func void _give_in_flight_modifiers_to_pre_statement(PreStatement* pStatement, ParserParams& parserParams)
{
    PreAstModifierNode* pModifier = parserParams.parserState.inFlightModifierNodes;
    while (pModifier) {
        PreAstModifierNode* pNextModifier = pModifier->next;
        pModifier->next = pStatement->firstPreStatementModifier;
        pStatement->firstPreStatementModifier = pModifier;
        pModifier = pNextModifier;
    }
    parserParams.parserState.inFlightModifierNodes = 0;
    _give_in_flight_modifier_counts_to_statement(pStatement, parserParams);
}

local_func void _give_in_flight_modifiers_to_post_statement(PreStatement* pStatement, ParserParams& parserParams)
{
    PreAstModifierNode* pModifier = parserParams.parserState.inFlightModifierNodes;
    while (pModifier) {
        PreAstModifierNode* pNextModifier = pModifier->next;
        pModifier->next = pStatement->firstPostStatementModifier;
        pStatement->firstPostStatementModifier = pModifier;
        pModifier = pNextModifier;
    }
    parserParams.parserState.inFlightModifierNodes = 0;
    _give_in_flight_modifier_counts_to_statement(pStatement, parserParams);
}

local_func void _give_post_modifier_to_node(PreAstNode* pNode, PreAstModifierNode* pModifier)
{
    Assert_(pNode);
    Assert_(pModifier);
    pModifier->next = pNode->firstPostModifier;
    pNode->firstPostModifier = pModifier;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_unary_parsing_fn(array_like_decl)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> pseudo-unary-op : array-like-decl syntax (hopefully prefixing its element type)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);

    reference_current_token_as(openingBracket);
    PreAstNode* pArrayLikeTypeExpr = _prepare_pre_node(parserParams, ENODE_EXPR_UNARYOP, 0, openingBracket);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pArrayLikeTypeExpr, secondary, 0,
        "expecting indexed-type parameters and closing brackets", pArrayLikeTypeExpr);
    PreAstNode* pInsideBrackets = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);

    if (*outError) {
        // we fill "secondary" slot here since, as an "unaryop", its primary will be reserved for actual operand...
        _wrapup_pre_node_secondary(pArrayLikeTypeExpr, *pInsideBrackets);
    } else {
        PreAstNode* pArrayLikeParamWrapper = _parse_enclosed_finalization_as_params(parserParams, pInsideBrackets,
            ENODE_SUBEXPR_WRAPPER, ETOK_CLOSING_BRACKET, uDepthGuard, outError);
        Assert_(pArrayLikeParamWrapper); // _parse_enclosed_finalization_as_params should not return null
        // we fill "secondary" slot here since, as an "unaryop", its primary will be reserved for actual operand...
        _wrapup_pre_node_secondary(pArrayLikeTypeExpr, *pArrayLikeParamWrapper);
    }
    return pArrayLikeTypeExpr;
}

// ParserParams& parserParams, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_unary_parsing_fn(map_decl)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> pseudo-unary-op : map-decl syntax (hopefully prefixing its value type)", true);
#endif
    // TODO
    PreAstNode* pResult;
    make_err_pre_node_other(pResult, FERR_NOT_YET_IMPLEMENTED, "map_decl not yet implemented");
    return pResult;
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(invoc_like)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : invocation-like form", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(openingParens);
    PreAstNode* pParensInvoke = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINARYOP, 0, openingParens);
    _wrapup_pre_node_primary(pParensInvoke, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pParensInvoke, secondary, 0,
        "Looking for parameters of invocation", pParensInvoke);
    PreAstNode* pParamsList = try_parse_expression_or_list(parserParams, false, true, 0, uDepthGuard+1, outError);
    if (*outError) {
        _wrapup_pre_node_secondary(pParensInvoke, *pParamsList);
    } else {
        PreAstNode* pInvokeParams = _parse_enclosed_finalization_as_params(parserParams, pParamsList,
            ENODE_SUBEXPR_WRAPPER, ETOK_CLOSING_PARENS, uDepthGuard, outError);
        Assert_(pInvokeParams); // _parse_enclosed_finalization_as_params should not return null
        check_and_wrapup_pre_node(pParensInvoke, secondary, *pInvokeParams);
    }
    // No reorg needed for left assoc.
    // However, we steal any comptime-flag on the LHS and keep it to ourselves.
    if (pLHSExpr->uNodeFlags & EPRENODEFLAG_IS_COMPTIME) {
        pLHSExpr->uNodeFlags &= ~EPRENODEFLAG_IS_COMPTIME;
        pParensInvoke->uNodeFlags |= EPRENODEFLAG_IS_COMPTIME;
    }
    return pParensInvoke;
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(dot_descent)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : dot-descent (eg over namespace or structs)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(dotSymb);
    PreAstNode* pDotDescent = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINARYOP, 0, dotSymb);
    _wrapup_pre_node_primary(pDotDescent, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pDotDescent, primary, 0,
        "Looking for followup of dot descent", pDotDescent);
    PreAstNode* pFollowingAtom = try_parse_atomic_expression(parserParams, 0, uDepthGuard+1, outError);
    if (!pFollowingAtom)
        make_err_pre_node_expected_vs_found(pFollowingAtom, 0, "expected: expression");
    _wrapup_pre_node_secondary(pDotDescent, *pFollowingAtom);
    return pDotDescent;
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(deref_descent)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : deref-dot-descent (eg over namespace or structs)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(ptrAndDotSymb);
    PreAstNode* pImplicitDerefLHS = _prepare_pre_node(parserParams, ENODE_EXPR_DEREF, 0, ptrAndDotSymb);
    _wrapup_pre_node_primary(pImplicitDerefLHS, *pLHSExpr);
    PreAstNode* pDotDescent = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINARYOP, 0, ptrAndDotSymb);
    _wrapup_pre_node_primary(pDotDescent, *pImplicitDerefLHS);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pDotDescent, primary, 0,
        "Looking for followup of deref dot descent", pDotDescent);
    PreAstNode* pFollowingAtom = try_parse_atomic_expression(parserParams, 0, uDepthGuard+1, outError);
    if (!pFollowingAtom)
        make_err_pre_node_expected_vs_found(pFollowingAtom, 0, "expected: expression");
    _wrapup_pre_node_secondary(pDotDescent, *pFollowingAtom);
    return pDotDescent;
}

local_func PreAstNode* parse_any_binary_complex_literal(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError)
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
    reference_current_token_as(opening);
    PreAstNode* pExpr = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINARYOP, 0, opening);
    _wrapup_pre_node_primary(pExpr, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pExpr, secondary, 0,
        "Looking for contents of complex literal", pExpr);
    PreAstNode* pFollowingExpr = parse_complex_lit_contents(parserParams, uPayload, uDepthGuard, outError);
    if (!pFollowingExpr) {
        make_err_pre_node_expected_vs_found(pFollowingExpr, 0, "expected: expression or list");
    }
    _wrapup_pre_node_secondary(pExpr, *pFollowingExpr);
    return pExpr;
}


// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(arr_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : explicitely-typed array-literal", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_ARRAY_LIT);
    return parse_any_binary_complex_literal(parserParams, pLHSExpr, uDepthGuard, outError);
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(set_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : explicitely-typed set-literal", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_SET_LIT);
    return parse_any_binary_complex_literal(parserParams, pLHSExpr, uDepthGuard, outError);
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(map_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : explicitely-typed map-literal", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_MAP_LIT);
    return parse_any_binary_complex_literal(parserParams, pLHSExpr, uDepthGuard, outError);
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(st_lit)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : explicitely-typed struct-literal", true);
#endif
    Assert_(u8(parserParams.parserState.pCurrentToken->uTokenPayloadAndKind >> 8) == ETOK_OPENING_STRUCT_LIT);
    return parse_any_binary_complex_literal(parserParams, pLHSExpr, uDepthGuard, outError);
}

local_func_inl PreAstNode* _parse_sliced_expr(Token* pCurrent, ParserParams& parserParams, PreAstNode* pSliceLHS, u16 uDepthGuard, u16* outError)
{
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2);
    platform_log_info("Parsing slicing parameters with ", false);
    debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
    platform_log_info("", true);
#endif
    reference_current_token_as(slicing);
    PreAstNode* pSlicedExpr = _prepare_pre_node(parserParams, ENODE_SUBEXPR_SLICE, 0, slicing);
    if (pSliceLHS) // allowed to be null
        _wrapup_pre_node_primary(pSlicedExpr, *pSliceLHS);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pSlicedExpr, secondary, 0,
        "expecting RHS of slicing params or closing brackets", pSlicedExpr);
    PreAstNode* pSliceRHS = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (pSliceRHS) // allowed to be null
        _wrapup_pre_node_secondary(pSlicedExpr, *pSliceRHS);
    return pSlicedExpr;
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(index_like)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : indexing-or-slicing brackets", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(openingBracket);
    PreAstNode* pIndexingExpr = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINARYOP, 0, openingBracket);
    _wrapup_pre_node_primary(pIndexingExpr, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pIndexingExpr, primary, 0,
        "expecting indexing parameters and closing brackets", pIndexingExpr);
    PreAstNode* pParam = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (0 == *outError) {
        pCurrent = parserParams.parserState.pCurrentToken;
        from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pIndexingExpr, primary, 0,
            "expecting indexed-type parameters and closing brackets", pIndexingExpr);
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_SLICING) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard*2+1);
            platform_log_info("> found slicing op", true);
#endif
            pParam = _parse_sliced_expr(pCurrent, parserParams, pParam, uDepthGuard + 1, outError);
            Assert_(pParam);
            pCurrent = parserParams.parserState.pCurrentToken;
        }
    }

    if (pParam == 0) {
        make_err_pre_node_expected_vs_found(pParam, 0, "expected non-null param");
    } 
    
    if (*outError) {
        _wrapup_pre_node_secondary(pIndexingExpr, *pParam);
    } else {
        PreAstNode* pIndexingWrapper = _parse_enclosed_finalization_as_params(parserParams, pParam,
            ENODE_SUBEXPR_WRAPPER, ETOK_CLOSING_BRACKET, uDepthGuard, outError);
        Assert_(pIndexingWrapper); // _parse_enclosed_finalization_as_params should not return null
        // we fill "secondary" slot here since, as an "unaryop", its primary will be reserved for actual operand...
        _wrapup_pre_node_secondary(pIndexingExpr, *pIndexingWrapper);
    }
    return pIndexingExpr;
}

// ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError -> PreAstNode*
declare_spe_cont_parsing_fn(deref)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> supertight-binary continuation : simple 'deref' token", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(caret);
    PreAstNode* pDerefExpr = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINARYOP, 0, caret);
    // this 'special binop' has exceptionally *no* secondary...
    _wrapup_pre_node_primary(pDerefExpr, *pLHSExpr);
    return pDerefExpr;
}

// Trying to parse the following tokens as part of an expression.
// SHALL start already positionned on a valid, and non-explicit-linebreak token.
// May return null without error if nothing started to match an expression (otherwise error)
// Will recursively call try_parse_expression_continuation and find everything matching a full expression.
local_func PreAstNode* try_parse_expression(ParserParams& parserParams,
    bool bExplicitNamesAllowed, u16 uStartsWithComptime, u16 uDepthGuard, u16* outError)
{
    if (uDepthGuard < EXPRESSION_TOO_DEEP) {
        Token* pCurrent = parserParams.parserState.pCurrentToken;
        Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
        debug_print_indent(uDepthGuard*2);
        platform_log_info("now trying to parse as expression, considering ", false);
        debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
        platform_log_info("", true);
#endif

        PreAstNode* pFirstExpr = 0;

        if (is_token_symbol_or_keyword(*pCurrent)) {
            u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
            if (tPayloadFlags[uPayload] == 0) {
                PreAstNode* pErrUnk; make_err_pre_node_unrecognized_token(pErrUnk);
                return pErrUnk;
            }

            // If we find a 'dollar' token for the first time here, we'll try from the next token and will be flagging next node as 'comptime'
            if (uPayload == ETOK_DOLLAR && 0 == (pCurrent->uTokenPayloadAndKind & TOKENKINDFLAG_SPACE_AFTERWARDS)) {
                if (!uStartsWithComptime) { // hopefully we did not 'already' see a dollar there...
                    pCurrent = ++parserParams.parserState.pCurrentToken;
                    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd); // token was flagged with no space afterwards => should be ok without check
                    PreAstNode* pExprAfter = try_parse_expression(parserParams, bExplicitNamesAllowed, EPRENODEFLAG_IS_COMPTIME,
                        uDepthGuard, outError);
                    if (pExprAfter)
                        return pExprAfter;
                    else
                        on_unexpected_return_err_self(0, "Expecting atomic expression after comptime token '$'");

                } else
                    on_unexpected_return_err_self(0, "Expecting not-yet-another '$' after comptime token '$'");
            }

            if (tPayloadFlags[uPayload] & UN_OP) {
                Assert_(0 == (tPayloadFlags[uPayload] & EXPR_)); // nothing should be flaggued both expr and unary...
                UnaryOpParsingProc_Sign* pSpecialUnaryHandlingFn = tUnaryParsingFn[uPayload];
                if (pSpecialUnaryHandlingFn) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                    debug_print_indent(uDepthGuard*2 + 1);
                    platform_log_info("found token with special unary operator behaviour", true);
#endif
                    pFirstExpr = pSpecialUnaryHandlingFn(parserParams, uDepthGuard, outError);
                    Assert_(pFirstExpr); // should not return null
                    pFirstExpr->uNodeFlags |= uStartsWithComptime;
                } else {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                    debug_print_indent(uDepthGuard*2 + 1);
                    platform_log_info("found token with regular unary operator behaviour", true);
#endif
                    reference_current_token_as(unop);
                    pFirstExpr = _prepare_pre_node(parserParams, ENODE_EXPR_UNARYOP, uStartsWithComptime, unop);
                }
                pCurrent = ++parserParams.parserState.pCurrentToken;
                from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pFirstExpr, primary, 0, "Looking for expression after unary operator", pFirstExpr);
                PreAstNode* pFollowingExpr = try_parse_expression(parserParams, bExplicitNamesAllowed, 0, uDepthGuard+1, outError);
                pCurrent = parserParams.parserState.pCurrentToken;
                check_and_wrapup_pre_node_err_if_null(pFirstExpr, primary, pFollowingExpr, "Expected expression after unary operator");
                pFirstExpr = fixup_unary_op_node(pFirstExpr, parserParams);
                return pFirstExpr;

            } else
                goto otherwise_core_expr;

        } else {

            u8 uCategory = pCurrent->uTokenPayloadAndKind & TOKEN_CATEGORY_MASK;
            if ((uCategory == TOKEN_CATEGORY_IDENTIFIER) && (pCurrent->uTokenPayloadAndKind & AT_BEFORE)) { // @-identifiers
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2 + 1);
                platform_log_info("found pre-modifier", true);
#endif
                _parse_adding_in_flight_modifier_to(parserParams, uStartsWithComptime, uDepthGuard, outError);
                if (*outError) {
                    debug_print_preparse_err("*** Modifier Parse Error while trying to parse expression ***");
                    TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams);
                    PreAstNode* modifierInError = _prepare_pre_node(parserParams, EPRENODE_ERROR_WITH_PRE_MODIFIERS, 0, wrapperToken);
                    modifierInError->iCountSubNodesOrNegErr = -i32(u32(*outError));
                    return modifierInError;
                } else {
                    pCurrent = parserParams.parserState.pCurrentToken;
                    if (pCurrent < parserParams.parserState.pLineTokenEnd)
                        return try_parse_expression(parserParams, bExplicitNamesAllowed, 0, uDepthGuard+1, outError);
                    else
                        return pFirstExpr; // EOL => do not search for continuation
                }

            } else { otherwise_core_expr:
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                debug_print_indent(uDepthGuard*2);
                platform_log_info("> not a special modifier, $-token, nor an unary op => try to match same token as an atomic expression", true);
#endif

                pFirstExpr = try_parse_atomic_expression(parserParams, uStartsWithComptime, uDepthGuard, outError);
                if (*outError)
                    return pFirstExpr;

                if (pFirstExpr) {
                    pCurrent = ++parserParams.parserState.pCurrentToken;
                    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
                        from_current_token_allows_break_if_multiline_err_wraparound(pCurrent, *pFirstExpr, "while checking for expressing continuation");
                        return try_parse_expression_continuation(parserParams, pFirstExpr,
                            bExplicitNamesAllowed, uDepthGuard + 1, outError);
                    } else {
                        return pFirstExpr; // EOL => do not search for continuation
                    }
                } else {
                    return 0; // nominal null when not started to match.
                }
            }
        }

    } else {
        PreAstNode* pErrTooDeep; make_err_pre_node_other(pErrTooDeep, PERR_EXPRESSION_TOO_DEEP, "Too deeply nested expression");
        return pErrTooDeep;
    }
}

// Trying to parse the following tokens as part of an expression OR expression-list, 
// SHALL start already positionned on a valid, and non-explicit-linebreak token.
// May return null without error if nothing started to match an expression (otherwise error)
// will continue to parse all expressions as part of same list while a comma is encountered after a valid expression.
local_func PreAstNode* try_parse_expression_or_list(ParserParams& parserParams,
    bool bTrailingCommaAllowed, bool bExplicitNamesAllowed, u16 uStartsWithComptime, u16 uDepthGuard, u16* outError)
{
    PreAstNode* pSingleExpr = try_parse_expression(parserParams, bExplicitNamesAllowed, uStartsWithComptime, uDepthGuard, outError);
    if (*outError)
        return pSingleExpr;
    else if (!pSingleExpr)
        return 0; // nominal null when not started to match.

    Token* pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        // any explicit linebreak at this point should already have been eaten by
        //    'eat_explicit_linebreak_and_find_next_token(pCurrent);' in 'try_parse_expression'
        if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_COMMA) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard*2);
            platform_log_info("Found first comma in chain.", true);
#endif
            PreAstNode* pChainRoot = 0;
            PreAstNode* pLastInChain = 0;
            i32 iElementCount = 0;
            do {
                reference_current_token_as(comma);
                {
                    PreAstNode* pNewInChain = _prepare_pre_node(parserParams, ENODE_EXPRLIST_NODE, 0, comma);
                    pNewInChain->primaryChildNode = pSingleExpr;
                    if (pLastInChain)
                        pLastInChain->secondaryChildNode = pNewInChain;
                    else
                        pChainRoot = pNewInChain;
                    pLastInChain = pNewInChain;
                }
                pCurrent = ++parserParams.parserState.pCurrentToken;
                from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pLastInChain, secondary, 0,
                    "Looking after comma", update_chain_node_counts(pChainRoot, *outError, parserParams));
                iElementCount++;
                if (iElementCount >= TOO_MANY_CHAINED_ELEMENTS) {
                    make_err_pre_node_other(pLastInChain->secondaryChildNode, PERR_TOO_MANY_ELEMENTS_IN_LIST, "Too many elements in list");
                    break;
                }
                pSingleExpr = try_parse_expression(parserParams, bExplicitNamesAllowed, 0, uDepthGuard, outError);
                if (*outError || !pSingleExpr)
                    break;
                pCurrent = parserParams.parserState.pCurrentToken;

            } while (pCurrent < parserParams.parserState.pLineTokenEnd &&
                     is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_COMMA);

            if (pSingleExpr || bTrailingCommaAllowed) {
                pLastInChain->secondaryChildNode = pSingleExpr;
            } else {
                PreAstNode* inErrorNode; make_err_pre_node_expected_vs_found(inErrorNode, 0, "Expecting expression after comma");
                pLastInChain->secondaryChildNode = inErrorNode;
            }
            pChainRoot = update_chain_node_counts(pChainRoot, *outError, parserParams);
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
//            platform_log_info("Showing result of list pre-parsing:", true);
//            debug_print_ast_node(pChainRoot, 1, parserParams.pOsFuncs, parserParams.pSourceFile);
#endif
            return pChainRoot;
        }
    }

    return pSingleExpr;
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(conditional)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> statement of the form conditional-expr [and block-opening]", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(if_or_elif_or_while);
    pStatement->pivotToken1 = if_or_elif_or_while;
    u8 uStatementKind = ESTATEMENT_IF;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_ELIF)
        uStatementKind = ESTATEMENT_ELIF;
    else if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_WHILE)
        uStatementKind = ESTATEMENT_WHILE;
    else Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_IF);
    pStatement->uStatementKind = uStatementKind;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After if or elif or while keyword");
            return;
        }
        // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
        //      but this is part of trying to parse "more" and error-report later.
        PreAstNode* pConditionExpr = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard+1, outError);
        if (pConditionExpr) {
            pStatement->pMainNode = pConditionExpr;
            if (*outError)
                return;
        } else {
            make_err_pre_node_expected_vs_found(pStatement->pMainNode, 0, "Expecting condition expression after if or elif or while");
            return;
        }
        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            if (is_token_symbol_or_keyword(*pCurrent)) {
                if (u8(pCurrent->uTokenPayloadAndKind >> 8) == ((uStatementKind == ESTATEMENT_WHILE) ? EKEY_DO : EKEY_THEN)) {
                    reference_current_token_as(then_or_do);
                    pStatement->pivotToken2 = then_or_do;
                    pCurrent = ++parserParams.parserState.pCurrentToken;
                    if (pCurrent < parserParams.parserState.pLineTokenEnd && is_explicit_linebreak(pCurrent)) {
                        pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                        if (*outError) {
                            make_err_pre_node_during_multiline(pStatement->pSecondaryNode, "After then or do keyword");
                            return;
                        }
                    }
                }
            }
        }
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;

    } else {
        make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "Expecting condition expression after if or elif or while");
        return;
    }
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(block_opening)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> statement of the form [block-opening-only]", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(else_or_defer);
    pStatement->pivotToken1 = else_or_defer;
    u8 uStatementKind = ESTATEMENT_ELSE;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_DEFER)
        uStatementKind = ESTATEMENT_DEFER;
    else Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_ELSE);
    pStatement->uStatementKind = uStatementKind;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After else or defer keyword");
            return;
        }
    }
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(st_for)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> special 'for' statement", true);
#endif
    // TODO
    make_err_pre_node_other(pStatement->pMainNode, FERR_NOT_YET_IMPLEMENTED, "st_for not yet implemented");
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(loop_finalizer)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> statement of the form 'loop-finalizer'", true);
#endif
    // TODO
    make_err_pre_node_other(pStatement->pMainNode, FERR_NOT_YET_IMPLEMENTED, "loop_finalizer not yet implemented");
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(st_case)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> special 'case' statement", true);
#endif
    // TODO
    make_err_pre_node_other(pStatement->pMainNode, FERR_NOT_YET_IMPLEMENTED, "st_case not yet implemented");
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(special_token)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> statement of the form 'single-special-token'", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(noop_or_unreach);
    pStatement->pivotToken1 = noop_or_unreach;
    u8 uStatementKind = ESTATEMENT_NO_OP;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_UNREACH)
        uStatementKind = ESTATEMENT_UNREACH;
    else Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_NO_OP);
    pStatement->uStatementKind = uStatementKind;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After noop or unreach keyword");
            return;
        }
    }
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(st_return)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> special 'return' statement", true);
#endif
    pStatement->uStatementKind = ESTATEMENT_RETURN;
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After return keyword");
            return;
        }
        PreAstNode* pListToReturn = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
        pStatement->pMainNode = pListToReturn; // can be null, but empty return is okay
    } else {
        // NOOP: empty return is okay as far as parser is concerned
    }
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(st_opt_expr)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> statement of the form 'single-token-with-optional-expr' : break, or continue", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    u8 uStatementKind = ESTATEMENT_BREAK;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_CONTINUE) {
        uStatementKind = ESTATEMENT_CONTINUE;
    } else {
        Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == ETOK_BREAK);
    }
    pStatement->uStatementKind = uStatementKind;
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After break or continue keyword");
            return;
        }
        PreAstNode* pListToReturn = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
        pStatement->pMainNode = pListToReturn; // can be null, but empty return is okay
    } else {
        // NOOP: empty break or continue is okay
    }
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(special_using)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> statement of the form 'special-using/including'", true);
#endif
    pStatement->uStatementKind = ESTATEMENT_USING;
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_USING || u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_INCLUDING);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After 'using' or 'including' keyword");
            return;
        }
    }
    PreAstNode* pWhatToUse = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
    if (!pWhatToUse) {
        make_err_pre_node_expected_vs_found(pWhatToUse, 0, "expected expression or list after 'using' or 'including'");
    } else {
        if (pWhatToUse->uNodeKind == ENODE_EXPR_OTHER_DEF)
            pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
    }
    pStatement->pMainNode = pWhatToUse;
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(pan_opening)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> pan-statement of the form 'expects-expression [and block-opening]'", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(panSt);
    pStatement->pivotToken1 = panSt;
    u8 uStatementKind = ESTATEMENT_PAN_IF;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ELIF) {
        uStatementKind = ESTATEMENT_PAN_ELIF;
        pStatement->uStatementFlags |= ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE;
    } else if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_SCOPE) {
        uStatementKind = ESTATEMENT_PAN_SCOPE;
    } else {
        Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_IF);
    }
    pStatement->uStatementKind = uStatementKind;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_PAN_DIRECTIVE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After pan keyword");
            return;
        }
    }
    PreAstNode* pExpression = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
    if (!pExpression) {
        make_err_pre_node_expected_vs_found(pExpression, 0, "expected an expression after this pan keword");
    }
    pStatement->pMainNode = pExpression;
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(pan_else)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> special pan-statement 'else'", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(panSt);
    pStatement->pivotToken1 = panSt;
    Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ELSE);
    pStatement->uStatementKind = ESTATEMENT_PAN_ELSE;
    pStatement->uStatementFlags |= ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_PAN_DIRECTIVE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        make_err_pre_node_expected_vs_found(pStatement->pMainNode, 0, "Pan-else does not expect anything afterwards");
        return;
    }
}

// PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError
declare_statement_parsing_fn(pan_closing)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("> pan-statement of the form [block-closing-only]", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(panSt);
    pStatement->pivotToken1 = panSt;
    u8 uStatementKind = ESTATEMENT_PAN_ENDIF;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ENDSCOPE)
        uStatementKind = ESTATEMENT_PAN_ENDSCOPE;
    else {
        Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ENDIF);
    }
    pStatement->uStatementKind = uStatementKind;
    pStatement->uStatementFlags |= ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        make_err_pre_node_expected_vs_found(pStatement->pMainNode, 0, "Pan-closing does not expect anything afterwards");
        return;
    }
}

// Starting right after assign token => needs to check for possibly multiline continuation
// Shall not return null.
local_func_inl PreAstNode* _parse_assign_rhs(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_self(pCurrent, 0, "Looking for RHS of assign-like-form");
    PreAstNode* pRhsValueExpression = try_parse_expression(parserParams, false, 0, uDepthGuard, outError);
    if (!pRhsValueExpression)
        make_err_pre_node_expected_vs_found(pRhsValueExpression, 0, "Expecting expression or list as RHS of assign-like form"); 
    return pRhsValueExpression;
}

#define set_last_token_and_return(pPreStatement) do { \
    pResult->iLineOfLastToken = parserParams.parserState.iLineOfLastRegisteredToken; \
    pResult->uLastTokenIndexOnLine = parserParams.parserState.uIndexOfLastRegisteredToken; \
    return pPreStatement; \
} while (0)

local_func PreStatement* _init_pre_statement(ParserParams& parserParams) {
    PreStatement* pPreStatement = (PreStatement*)alloc_from(parserParams.preparsingArena, sizeof(PreStatement), 8);
    pPreStatement->pLhsStatementWhenInlined = 0;
    pPreStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pPreStatement->uStatementKind = ESTATEMENT_UNKNOWN;
    pPreStatement->uStatementFlags = 0;
    pPreStatement->pMainNode = 0;
    pPreStatement->pSecondaryNode = 0;
    pPreStatement->firstPreStatementModifier = parserParams.parserState.inFlightModifierNodes;
    pPreStatement->firstPostStatementModifier = 0;
    pPreStatement->uCountModifierNodes = parserParams.parserState.uCountModifierNodes;
    pPreStatement->uCountParamNodesWithinModifiers = parserParams.parserState.uCountParamNodesWithinModifiers;
    pPreStatement->iStartLine = parserParams.parserState.iCurrentlyParsedLine;
    pPreStatement->pStartingByteOnLine = parserParams.parserState.pStartOfLineAfterIndent - parserParams.parserState.uCurrentLineIndent;
    if (parserParams.parserState.pCurrentToken < parserParams.parserState.pLineTokenEnd)
        pPreStatement->uByteCountOnLineToFirst = parserParams.parserState.pCurrentToken->uTokenStartCharOnLine;
    else
        pPreStatement->uByteCountOnLineToFirst = parserParams.parserState.uCurrentLineIndent + parserParams.parserState.uCurrentBytesOnLine;
    pPreStatement->uFirstTokenIndexOnLine = u16(parserParams.parserState.pCurrentToken - parserParams.parserState.pLineTokens);
    pPreStatement->iLineOfLastToken = -1;
    pPreStatement->uLastTokenIndexOnLine = 0xFFFFFFFFu;
    pPreStatement->uRefStartOfError = 0;
    pPreStatement->pivotToken1 = TokenRef{};
    pPreStatement->pivotToken2 = TokenRef{};
    parserParams.parserState.inFlightModifierNodes = 0;
    parserParams.parserState.uCountModifierNodes = 0;
    parserParams.parserState.uCountParamNodesWithinModifiers = 0;
    return pPreStatement;
}

// Trying to parse the following tokens as part of a statement.
// SHALL start already positionned on a valid, and non-explicit-linebreak token.
// Will continue to parse all inlined statements as part of a statement chain while end of line (or multiline)
//   is not encountered. Any EOL found while expecting a statement will return an error.
local_func PreStatement* parse_pre_statement(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
    PreStatement* pResult = 0;
    PreStatement* pStatementBefore = 0; 
    bool bAppend;
    do { // iterating while finding inline continuation or ';'
        bAppend = false;
        
        pResult = _init_pre_statement(parserParams);
        pResult->pLhsStatementWhenInlined = pStatementBefore;

        Token* pCurrent = parserParams.parserState.pCurrentToken;
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        debug_print_indent(uDepthGuard + 1);
        platform_log_info("Parsing new statement with ", false);
        debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
        platform_log_info("", true);
#endif

        u8 uStartsWithComptime = 0;
        after_comptime_tryagain:

        PreAstNode* pStartingExpression = 0;

        if (is_token_symbol_or_keyword(*pCurrent)) {
            u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
            if (tPayloadFlags[uPayload] == 0) {
                PreAstNode* pErrUnk; make_err_pre_node_unrecognized_token(pErrUnk);
                pResult->pMainNode = pErrUnk;
                set_last_token_and_return(pResult);
            }
            if (uPayload == ESYMB_DOLLAR && 0 == (pCurrent->uTokenPayloadAndKind & TOKENKINDFLAG_SPACE_AFTERWARDS)) {
                reference_current_token_as(comptime);
                pCurrent = ++parserParams.parserState.pCurrentToken;
                Assert_(pCurrent < parserParams.parserState.pLineTokenEnd); // token was flagged with no space afterwards => should be ok without check
                uStartsWithComptime = ESTATEMENTFLAGS_COMPTIME;
                goto after_comptime_tryagain;
            }
            StatementParsingProc_Sign* pStatementParsingFn = tStatementParsingFn[uPayload];
            if (pStatementParsingFn) {
                pResult->uStatementFlags |= uStartsWithComptime;
                pStatementParsingFn(pResult, parserParams, uDepthGuard+1, outError);
                if (*outError)
                    set_last_token_and_return(pResult);
                goto after_pivot; // does not accept statement pivots...
            }
        }

        u8 uTokenCategory = u8(pCurrent->uTokenPayloadAndKind) & TOKEN_CATEGORY_MASK;
        if (uTokenCategory == TOKEN_CATEGORY_IDENTIFIER && (pCurrent->uTokenPayloadAndKind & AT_BEFORE)) { // @-identifiers...
            // TODO
            make_err_pre_node_unimplemented(pResult->pMainNode, "Modifiers");
            pResult->uStatementFlags |= uStartsWithComptime;
            set_last_token_and_return(pResult);
        }

        /*
        if ((pCurrent->uTokenKindAndSpaceFlag & 0x7F) == ETOK_KIND_IDENTIFIER_PAN) {
            // user-defined pan-identifier => macros.
            // Parse as a macro (very special stuff... could be anything)
            // TODO
            make_err_pre_node_unimplemented(pResult->pMainNode, "User macros");
            set_last_token_and_return(pResult);
        }
        */

        // if we get to here, starting token should not be a known, statement-starting keyword or symbol
        // => we'll now expect that statement to start with an expression
        pStartingExpression = try_parse_expression_or_list(
                parserParams, false, true, u16(uStartsWithComptime), uDepthGuard+1, outError);
        pResult->pMainNode = pStartingExpression;
        pResult->uStatementKind = ESTATEMENT_SINGLE_EXPRESSION;
        if (*outError)
            set_last_token_and_return(pResult);
        if (!pStartingExpression) {
            if (parserParams.parserState.inFlightModifierNodes) {
                _give_in_flight_modifiers_to_pre_statement(pResult, parserParams);
                pResult->uStatementKind = ESTATEMENT_MODIFIER_ONLY_LINE;
                goto after_pivot; // does not accept statement continuations...
            } else {
                pResult->uStatementKind = ESTATEMENT_UNKNOWN;
                Assert_(parserParams.parserState.pCurrentToken < parserParams.parserState.pLineTokenEnd);
                make_err_pre_node_expected_vs_found(pResult->pMainNode, 0, "Expecting statement");
                set_last_token_and_return(pResult);
            }
        }

        after_found_statement_start:

        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            // any explicit linebreak at this point should already have been eaten by try_parse_expression_or_list
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
            debug_print_indent(uDepthGuard + 1);
            platform_log_info("  trying to parse statement pivot after single-expression, with ", false);
            debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
            platform_log_info("", true);
#endif
            if (is_token_symbol_or_keyword(*pCurrent)) {
                u8 uPayload = u8(pCurrent->uTokenPayloadAndKind >> 8);
                if (tPayloadFlags[uPayload] == 0) {
                    PreAstNode* pErrUnk; make_err_pre_node_unrecognized_token(pErrUnk);
                    pResult->pMainNode = pStartingExpression;
                    pResult->pSecondaryNode = pErrUnk;
                    set_last_token_and_return(pResult);
                }
                if (tPayloadFlags[uPayload] & OP_AND_ASIGN) {
                    // op-and-assign pivot after start expression
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                    debug_print_indent(uDepthGuard + 2);
                    platform_log_info("  Op-Assign statement found", true);
#endif
                    reference_current_token_as(opassign);
                    pResult->uStatementKind = ESTATEMENT_OP_AND_ASSIGNMENT;
                    pResult->pivotToken1 = opassign;
                    pCurrent = ++parserParams.parserState.pCurrentToken;
                    pResult->pSecondaryNode = _parse_assign_rhs(parserParams, uDepthGuard+1, outError);
                    if (*outError)
                        set_last_token_and_return(pResult);
                } else {
                    switch (uPayload) {
                            
                        // assignment pivot ':='
                        case ETOK_ASSIGNMENT: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                            debug_print_indent(uDepthGuard + 2);
                            platform_log_info("  Assign statement found", true);
#endif
                            reference_current_token_as(assignop);
                            pResult->uStatementKind = ESTATEMENT_ASSIGNMENT;
                            pResult->pivotToken1 = assignop;
                            pCurrent = ++parserParams.parserState.pCurrentToken;
                            pResult->pSecondaryNode = _parse_assign_rhs(parserParams, uDepthGuard+1, outError);
                            if (*outError)
                                set_last_token_and_return(pResult);
                            if (pResult->pSecondaryNode->uNodeKind == ENODE_EXPR_PROCLIKE_DEF ||
                                pResult->pSecondaryNode->uNodeKind == ENODE_EXPR_OTHER_DEF)
                            {
                                pResult->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
                            }
                        } break;

                        // const-decl-pivot '::'
                        case ETOK_CONST_DECL: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                            debug_print_indent(uDepthGuard + 2);
                            platform_log_info("  const-decl statement found", true);
#endif
                            reference_current_token_as(constdecl);
                            pResult->uStatementKind = ESTATEMENT_CONST_DECLARATION;
                            pResult->pivotToken1 = constdecl;
                            pCurrent = ++parserParams.parserState.pCurrentToken;
                            pResult->pSecondaryNode = _parse_assign_rhs(parserParams, uDepthGuard+1, outError);
                            Assert_(pResult->pSecondaryNode); // _parse_assign_rhs should not return null
                            if (*outError)
                                set_last_token_and_return(pResult);
                            if (pResult->pSecondaryNode->uNodeKind == ENODE_EXPR_PROCLIKE_DEF ||
                                pResult->pSecondaryNode->uNodeKind == ENODE_EXPR_OTHER_DEF)
                            {
                                pResult->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
                            }
                        } break;

                        // var-decl pivot 'as'
                        case ETOK_VARDECL: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                            debug_print_indent(uDepthGuard + 2);
                            platform_log_info("  Vardecl statement found", true);
#endif
                            reference_current_token_as(vartypesymb);
                            pResult->uStatementKind = ESTATEMENT_VAR_DECLARATION;
                            pResult->pivotToken1 = vartypesymb;
                            pCurrent = ++parserParams.parserState.pCurrentToken;
                            pResult->pSecondaryNode = _parse_vardecl_rhs(parserParams, true, uDepthGuard+1, outError);
                            Assert_(pResult->pSecondaryNode); // _parse_vardecl_rhs should not return null
                            if (*outError)
                                set_last_token_and_return(pResult);
                            if (pResult->pSecondaryNode->uNodeKind == ENODE_SUBEXPR_WRAPPER) {
                                if (pResult->pSecondaryNode->secondaryChildNode &&
                                    (pResult->pSecondaryNode->secondaryChildNode->uNodeKind == ENODE_EXPR_PROCLIKE_DEF ||
                                        pResult->pSecondaryNode->secondaryChildNode->uNodeKind == ENODE_EXPR_OTHER_DEF))
                                {
                                    pResult->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
                                }
                            }
                        } break;

                        // colon ending ':' (labels)
                        case ETOK_COLON: {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                            debug_print_indent(uDepthGuard + 2);
                            platform_log_info("  Label indicator found", true);
#endif
                            reference_current_token_as(column);
                            pResult->uStatementKind = ESTATEMENT_LABEL;
                            pResult->pivotToken1 = column;
                            pResult->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
                            pCurrent = ++parserParams.parserState.pCurrentToken;
                            if (pCurrent < parserParams.parserState.pLineTokenEnd) {
                                if (is_explicit_linebreak(pCurrent)) {
                                    pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                                    if (*outError) {
                                        make_err_pre_node_during_multiline(pResult->pSecondaryNode, "after column");
                                        set_last_token_and_return(pResult);
                                    }
                                }
                            }
                        } break;
                    }
                }
            }
        } else { // single expression
            if (pStartingExpression->uNodeKind == ENODE_EXPR_PROCLIKE_DEF || 
                pStartingExpression->uNodeKind == ENODE_EXPR_OTHER_DEF)
            {
                pResult->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
            }
        }

        after_pivot:

        _give_in_flight_modifier_counts_to_statement(pResult, parserParams);

        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            // any explicit linebreak at this point should already have been eaten by any of the above
            u8 uTokenCategory = u8(pCurrent->uTokenPayloadAndKind) & TOKEN_CATEGORY_MASK;

            // handling post-statement modifiers
            if (uTokenCategory == TOKEN_CATEGORY_IDENTIFIER && (pCurrent->uTokenPayloadAndKind & AT_BEFORE)) {
                do {
                    _parse_adding_in_flight_modifier_to(parserParams, 0, uDepthGuard+1, outError);
                    pCurrent = parserParams.parserState.pCurrentToken;
                    if (pCurrent >= parserParams.parserState.pLineTokenEnd)
                        break;
                    uTokenCategory = u8(pCurrent->uTokenPayloadAndKind >> 8);
                    if (*outError)
                        set_last_token_and_return(pResult);
                } while (uTokenCategory == TOKEN_CATEGORY_IDENTIFIER && (pCurrent->uTokenPayloadAndKind & AT_BEFORE));
                _give_in_flight_modifiers_to_post_statement(pResult, parserParams);
                if (pCurrent >= parserParams.parserState.pLineTokenEnd)
                    goto end_of_line;
            }

            // any explicit linebreak at this point should already have been eaten by any of the above
            if (uTokenCategory == TOKEN_CATEGORY_SYMBOL && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_IMPLIES &&
                    (pResult->uStatementFlags == EBLOCK_SPAWNING_NONE || pResult->uStatementFlags == EBLOCK_SPAWNING_EXPECTED) &&
                    pResult->uStatementKind != ESTATEMENT_MODIFIER_ONLY_LINE) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                debug_print_indent(uDepthGuard + 2);
                platform_log_info("Explicit-child-block-to-follow token found", true);
#endif
                reference_current_token_as(implies);
                pResult->uStatementFlags = EBLOCK_SPAWNING_EXPECTED; 
                pResult->pivotToken2 = implies;
                pCurrent = ++parserParams.parserState.pCurrentToken;
                if (pCurrent < parserParams.parserState.pLineTokenEnd) {
                    if (is_explicit_linebreak(pCurrent)) {
                        pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                        if (*outError) {
                            if (pResult->pSecondaryNode) {
                                TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams);
                                PreAstNode* errWrapup = _prepare_pre_node(parserParams, EPRENODE_ERROR_WRAPPER, 0, wrapperToken);
                                _wrapup_pre_node_primary(errWrapup, *pResult->pSecondaryNode);
                                PreAstNode* multilineErr; make_err_pre_node_during_multiline(multilineErr, "Looking after => token");
                                _wrapup_pre_node_secondary(errWrapup, *multilineErr);
                                pResult->pSecondaryNode = errWrapup;
                            } else
                                make_err_pre_node_during_multiline(pResult->pSecondaryNode, "Looking after => token");
                            set_last_token_and_return(pResult);
                        }
                    }
                }
            }
        }

        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
            debug_print_indent(uDepthGuard + 2);
            platform_log_info("Current line has remaining tokens after fully parsed statement! Trying to parse remainder as inlined statement: ", false);
            debug_print_token(pCurrent, parserParams.parserState.pStartOfLineAfterIndent, parserParams.pSourceFile);
            platform_log_info("", true);
#endif
            if (pResult->uExpectedNextBlockSpawning == EBLOCK_SPAWNING_EXPECTED) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                debug_print_indent(uDepthGuard + 2);
                platform_log_info("Last statement was either implicitely or explicitely child-block spawning => parsing remainder as inlined-child-block", true);
#endif
                bAppend = true;
            } else {
                // Note: Disabling statement-chaining-with-semicolon for the time being:
                // 1) It seems error-prone currently with our habit of line-ending semicolon in C
                // 2) Error report after a failed multiline-search seems really off, especially before EOF (TODO:, CLEANUP:)
                /*
                if (pResult->uExpectedNextBlockSpawning == EBLOCK_SPAWNING_NONE && pResult->uStatementKind != ESTATEMENT_MODIFIER_ONLY_LINE) {
                    pCurrent = parserParams.parserState.pCurrentToken;
                    if (is_token_symbol_or_keyword(*pCurrent) && u8(pCurrent->uTokenPayloadAndKind >> 8) == ESYMB_SEMICOLON) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                        debug_print_indent(uDepthGuard + 2);
                        platform_log_info("Found statement chaining token ';' when last statement was regular => parsing remainder as chained statement at same level", false);
#endif
                        pCurrent = ++parserParams.parserState.pCurrentToken;
                        if (!(pCurrent < parserParams.parserState.pLineTokenEnd) || is_explicit_linebreak(pCurrent)) {
                            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                            if (*outError) {
                                PreStatement* pAfterResult = _init_pre_statement(parserParams);
                                pAfterResult->pLhsStatementWhenInlined = pResult;
                                make_err_pre_node_during_multiline(pAfterResult->pMainNode, "After statement-chaining token ';'");
                                set_last_token_and_return(pAfterResult);
                            }
                        }
                        bAppend = true;
                    }
                }
                */
                if (!bAppend) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
                    debug_print_indent(uDepthGuard + 2);
                    platform_log_info("*** Did not find statement chaining token, or last statement was NOT regular !!!", true);
#endif
                    PreStatement* pAfterResult = _init_pre_statement(parserParams);
                    pAfterResult->pLhsStatementWhenInlined = pResult;
                    make_err_pre_node_expected_vs_found(pAfterResult->pMainNode, 0, "expected EOL after standalone statement");
                    set_last_token_and_return(pAfterResult);
                }
            }
        }

        end_of_line:

        if (bAppend) {
            pStatementBefore = pResult;
            uDepthGuard++;
            if (uDepthGuard >= EXPRESSION_TOO_DEEP) {
                PreStatement* pAfterResult = _init_pre_statement(parserParams);
                pAfterResult->pLhsStatementWhenInlined = pResult;
                make_err_pre_node_other(pAfterResult->pMainNode, PERR_EXPRESSION_TOO_DEEP, "reached expression too deep");
                set_last_token_and_return(pAfterResult);
            }
        }

    } while (bAppend);

    set_last_token_and_return(pResult);
}

#if 0
local_func void parse_pan_if_or_elif_or_scope(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard + 2);
    platform_log_info("Special handling of #if-or-#elif-or-#scope-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(pan_if_or_elif_or_scope);
    pStatement->pivotToken1 = pan_if_or_elif_or_scope;
    u8 uStatementKind = ESTATEMENT_PAN_IF;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ELIF) {
        pStatement->uStatementFlags |= ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE;
        uStatementKind = ESTATEMENT_PAN_ELIF;
    } else if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_SCOPE)
        uStatementKind = ESTATEMENT_PAN_SCOPE;
    else Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_IF);
    pStatement->uStatementKind = uStatementKind;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After #if or #elif or #scope keyword");
            return;
        }
        // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
        //      but this is part of trying to parse "more" and error-report later.
        PreAstNode* pConditionOrScopeLevelExpr = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard+1, outError);
        if (pConditionOrScopeLevelExpr) {
            pStatement->pMainNode = pConditionOrScopeLevelExpr;
            if (*outError)
                return;
        } else {
            make_err_pre_node_expected_vs_found(pStatement->pMainNode, 0, "Expecting condition expression or scope level after #if or #elif or #scope");
            return;
        }
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_PAN_DIRECTIVE;
    } else {
        make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "Expecting condition expression or scope level after #if or #elif or #scope");
        return;
    }
}

local_func void parse_pan_else_or_endif_or_endscope(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard + 2);
    platform_log_info("Special handling of #endif-or-#endscope-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    u8 uStatementKind = ESTATEMENT_PAN_ELSE;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_PAN_DIRECTIVE;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ENDSCOPE) {
        uStatementKind = ESTATEMENT_PAN_ENDSCOPE;
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    } else if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ENDIF) {
        uStatementKind = ESTATEMENT_PAN_ENDIF;
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    } else Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_PAN_ELSE);
    pStatement->uStatementKind = uStatementKind;
    pStatement->uStatementFlags |= ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After #else or #endif or #endscope keyword");
            return;
        }
    }
}


local_func void parse_if_or_elif_or_while(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of if-or-elif-or-while-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(if_or_elif_or_while);
    pStatement->pivotToken1 = if_or_elif_or_while;
    u8 uStatementKind = ESTATEMENT_IF;
    if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_ELIF)
        uStatementKind = ESTATEMENT_ELIF;
    else if (u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_WHILE)
        uStatementKind = ESTATEMENT_WHILE;
    else Assert_(u8(pCurrent->uTokenPayloadAndKind >> 8) == EKEY_IF);
    pStatement->uStatementKind = uStatementKind;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After if or elif or while keyword");
            return;
        }
        // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
        //      but this is part of trying to parse "more" and error-report later.
        PreAstNode* pConditionExpr = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard+1, outError);
        if (pConditionExpr) {
            pStatement->pMainNode = pConditionExpr;
            if (*outError)
                return;
        } else {
            make_err_pre_node_expected_vs_found(pStatement->pMainNode, 0, "Expecting condition expression after if or elif or while");
            return;
        }
        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04) {
                if (u8(pCurrent->uTokenPayload) == ((uStatementKind == ESTATEMENT_WHILE) ? EKEY_DO : EKEY_THEN)) {
                    reference_current_token_as(then_or_do);
                    pStatement->pivotToken2 = then_or_do;
                    pCurrent = ++parserParams.parserState.pCurrentToken;
                    if (pCurrent < parserParams.parserState.pLineTokenEnd && is_explicit_linebreak(pCurrent)) {
                        pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                        if (*outError) {
                            make_err_pre_node_during_multiline(pStatement->pSecondaryNode, "After then or do keyword");
                            return;
                        }
                    }
                }
            }
        }
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;

    } else {
        make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "Expecting condition expression after if or elif or while");
        return;
    }
}

local_func void parse_else_or_defer(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of else-or-defer-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    u8 uStatementKind = ESTATEMENT_ELSE;
    if (u8(pCurrent->uTokenPayload) == EKEY_DEFER)
        uStatementKind = ESTATEMENT_DEFER;
    else Assert_(u8(pCurrent->uTokenPayload) == EKEY_ELSE);
    pStatement->uStatementKind = uStatementKind;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After else or defer keyword");
            return;
        }
    }
}

local_func_inl PreAstNode* _parse_iteration(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
        //      but this is part of trying to parse "more" and error-report later.
        PreAstNode* startingExpr = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
        if (!startingExpr)
            make_err_pre_node_expected_vs_found(startingExpr, 0, "expected expression after 'for'");
        if (*outError)
            return startingExpr;
        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04 && u8(pCurrent->uTokenPayload) == EKEY_IN) {
                reference_current_token_as(inKey);
                PreAstNode* iterationExpr = _prepare_pre_node(parserParams, ENODE_ITERATION, 0, inKey);
                _wrapup_pre_node_primary(iterationExpr, *startingExpr);
                pCurrent = ++parserParams.parserState.pCurrentToken;
                from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, iterationExpr, secondary, 0, "expecting expression after 'in'", iterationExpr);
                // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
                //      but this is part of trying to parse "more" and error-report later.
                PreAstNode* iterable = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
                if (!iterable)
                    make_err_pre_node_expected_vs_found(iterable, 0, "expected expression after 'in'");
                _wrapup_pre_node_secondary(iterationExpr, *iterable);
                return iterationExpr;
            }
        }
        return startingExpr;
    }
    else {
        PreAstNode* errExpected; make_err_pre_node_expected_found_eol(errExpected, 0, "expected expression after 'for'");
        return errExpected;
    }
}

local_func void parse_for(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of for-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    reference_current_token_as(forKey);
    pStatement->pivotToken1 = forKey;
    pStatement->uStatementKind = ESTATEMENT_FOR;
    u8 uForKeyHasSpaceAfterwards = pCurrent->uTokenKindAndSpaceFlag & 0x80;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (uForKeyHasSpaceAfterwards && (pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0 && u8(pCurrent->uTokenPayload) == u8('>')) {
        pStatement->uStatementFlags |= ESTATEMENTFLAGS_REVERSE_FOR;
        pCurrent = ++parserParams.parserState.pCurrentToken;
    }
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After for keyword");
            return;
        }
        PreAstNode* pIterationExpr = _parse_iteration(parserParams, uDepthGuard+1, outError);
        Assert_(pIterationExpr);
        pStatement->pMainNode = pIterationExpr;
        if (*outError)
            return;
        pCurrent = parserParams.parserState.pCurrentToken;
        if (pCurrent < parserParams.parserState.pLineTokenEnd) {
            if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04) {
                if (u8(pCurrent->uTokenPayload) == EKEY_DO) {
                    reference_current_token_as(doKey);
                    pStatement->pivotToken2 = doKey;
                    pCurrent = ++parserParams.parserState.pCurrentToken;
                    if (pCurrent < parserParams.parserState.pLineTokenEnd && is_explicit_linebreak(pCurrent)) {
                        pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                        if (*outError) {
                            make_err_pre_node_during_multiline(pStatement->pSecondaryNode, "After do keyword");
                            return;
                        }
                    }
                }
            }
        }
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;

    } else {
        make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "Expecting iteration expression after for");
        return;
    }
}

local_func void parse_noop_or_unreachable(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of noop-or-unreachable-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    u8 uStatementKind = ESTATEMENT_NOOP;
    if (u8(pCurrent->uTokenPayload) == EKEY_UNREACHABLE)
        uStatementKind = ESTATEMENT_UNREACHABLE;
    else Assert_(u8(pCurrent->uTokenPayload) == EKEY_NOOP);
    pStatement->uStatementKind = uStatementKind;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After unreachable or noop keyword");
            return;
        }
    }
}

local_func void parse_case(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of case-statement", true);
#endif
    pStatement->uStatementKind = ESTATEMENT_CASE;
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After case keyword");
            return;
        }
        // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
        //      but this is part of trying to parse "more" and error-report later.
        PreAstNode* pExprToSwitchAgainst = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
        if (!pExprToSwitchAgainst)
            make_err_pre_node_expected_vs_found(pExprToSwitchAgainst, 0, "expected expression after 'case'");
        pStatement->pMainNode = pExprToSwitchAgainst;
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
    } else {
        make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "expected expression after 'case'");
    }
};

local_func void parse_with(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of with-statement", true);
#endif
    pStatement->uStatementKind = ESTATEMENT_WITH;
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After with keyword");
            return;
        }
        PreAstNode* pDeclaration = try_parse_expression_or_list_allowing_vardecl(parserParams, uDepthGuard, outError);
        if (!pDeclaration)
            make_err_pre_node_expected_vs_found(pDeclaration, 0, "expected declaration after 'with'");
        pStatement->pMainNode = pDeclaration;
        pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
    } else {
        make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "expected declaration after 'with'");
    }
}

local_func void parse_return(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of return-statement", true);
#endif
    pStatement->uStatementKind = ESTATEMENT_RETURN;
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After return keyword");
            return;
        }
        PreAstNode* pListToReturn = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
        pStatement->pMainNode = pListToReturn; // can be null, but empty return is okay
    } else {
        // NOOP: empty return is okay
    }
}

local_func void parse_break_or_loop(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of break-or-loop-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    u8 uStatementKind = ESTATEMENT_BREAK;
    if (u8(pCurrent->uTokenPayload) == EKEY_LOOP)
        uStatementKind = ESTATEMENT_LOOP;
    else Assert_(u8(pCurrent->uTokenPayload) == EKEY_BREAK);
    pStatement->uStatementKind = uStatementKind;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_NONE;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After break or loop keyword");
            return;
        }
        PreAstNode* pWhereToBreakFrom = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard, outError);
        pStatement->pMainNode = pWhereToBreakFrom; // can be null, but implicit break or loop is okay
    } else {
        // NOOP: implicit break or loop is okay
    }
}

local_func PreAstNode* parse_pointer_deref(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of ^-pointer deref continuation", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(openingParens);
    PreAstNode* pPtrDeref = _prepare_pre_node(parserParams, ENODE_EXPR_PTR_DEREF, 0, openingParens);
    _wrapup_pre_node_primary(pPtrDeref, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_allows_on_line_or_break_otherwise_null_within(pCurrent, pPtrDeref, secondary, "Pursuing explicit linebreak after ptr deref token", pPtrDeref);
    // No reorg needed for left assoc.
    // However, we steal any comptime-flag on the LHS and keep it to ourselves.
    if (pLHSExpr->uNodeFlags & EPRENODEFLAG_IS_COMPTIME) {
        pLHSExpr->uNodeFlags &= ~EPRENODEFLAG_IS_COMPTIME;
        pPtrDeref->uNodeFlags |= EPRENODEFLAG_IS_COMPTIME;
    }
    return pPtrDeref;
}

local_func PreAstNode* parse_paren_continuation(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of (-started continuations (invocation superbinop)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(openingParens);
    PreAstNode* pParensInvoke = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINOP, 0, openingParens);
    _wrapup_pre_node_primary(pParensInvoke, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pParensInvoke, primary, 0, "Looking for parameters of invocation", pParensInvoke);
    PreAstNode* pParamsList = try_parse_expression_or_list(parserParams, false, true, 0, uDepthGuard+1, outError);
    if (*outError) {
        _wrapup_pre_node_secondary(pParensInvoke, *pParamsList);
    } else {
        PreAstNode* pInvokeParams = _parse_enclosed_finalization_as_params(parserParams, pParamsList,
            ENODE_EXPR_INVOCATION_PARAMS, u8(')'), uDepthGuard, outError);
        Assert_(pInvokeParams); // _parse_enclosed_finalization_as_params should not return null
        check_and_wrapup_pre_node(pParensInvoke, secondary, *pInvokeParams);
    }
    // No reorg needed for left assoc.
    // However, we steal any comptime-flag on the LHS and keep it to ourselves.
    if (pLHSExpr->uNodeFlags & EPRENODEFLAG_IS_COMPTIME) {
        pLHSExpr->uNodeFlags &= ~EPRENODEFLAG_IS_COMPTIME;
        pParensInvoke->uNodeFlags |= EPRENODEFLAG_IS_COMPTIME;
    }
    return pParensInvoke;
}

local_func PreAstNode* parse_bracket_continuation(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of [-started continuations (indexing superbinop)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(openingBracket);
    PreAstNode* pBracketIndexing = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINOP, 0, openingBracket);
    _wrapup_pre_node_primary(pBracketIndexing, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pBracketIndexing, primary, 0, "Looking for parameters of indexing", pBracketIndexing);
    PreAstNode* pIndex = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (*outError) {
        _wrapup_pre_node_secondary(pBracketIndexing, *pIndex);
    } else {
        PreAstNode* pIndexParams = _parse_enclosed_finalization_as_params(parserParams, pIndex,
            ENODE_EXPR_INDEXING_PARAMS, u8(']'), uDepthGuard, outError);
        Assert_(pIndexParams); // _parse_enclosed_finalization_as_params should not return null
        check_and_wrapup_pre_node(pBracketIndexing, secondary, *pIndexParams);
    }
    // No reorg needed for left assoc.
    // However, we steal any comptime-flag on the LHS and keep it to ourselves.
    if (pLHSExpr->uNodeFlags & EPRENODEFLAG_IS_COMPTIME) {
        pLHSExpr->uNodeFlags &= ~EPRENODEFLAG_IS_COMPTIME;
        pBracketIndexing->uNodeFlags |= EPRENODEFLAG_IS_COMPTIME;
    }
    return pBracketIndexing;
}

local_func PreAstNode* parse_curly_standalone(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of {-started standalones (curlinit)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(openingCurlyBraces);
    PreAstNode* pCurlyInit = _prepare_pre_node(parserParams, ENODE_EXPR_CURLYINIT, 0, openingCurlyBraces);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pCurlyInit, primary, 0, "Looking after opening curly braces", pCurlyInit);
    PreAstNode* pParamsList = try_parse_expression_or_list(parserParams, true, true, 0, uDepthGuard+1, outError);
    if (*outError) {
        _wrapup_pre_node_secondary(pCurlyInit, *pParamsList);
    } else {
        PreAstNode* pInitParams = _parse_enclosed_finalization_as_params(parserParams, pParamsList,
            ENODE_CURLYINIT_PARAMS, u8('}'), uDepthGuard, outError);
        Assert_(pInitParams); // _parse_enclosed_finalization_as_params should not return null
        check_and_wrapup_pre_node(pCurlyInit, primary, *pInitParams);
    }
    return pCurlyInit;
}

local_func PreAstNode* parse_supertight_binop(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("found supertight binop continuation => expects atomic expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(superbinop);
    PreAstNode* pBinop = _prepare_pre_node(parserParams, ENODE_EXPR_SPECIAL_BINOP, 0, superbinop);
    _wrapup_pre_node_primary(pBinop, *pLHSExpr);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pBinop, secondary, 0, "Looking for RHS of supertight binary operator", pBinop);
    PreAstNode* pFollowingAtomicExpr = try_parse_atomic_expression(parserParams, false, uDepthGuard+1, outError);
    check_and_wrapup_pre_node_err_if_null(pBinop, secondary, pFollowingAtomicExpr, "Expected atomic expression as RHS for supertight binary operator");
    // No reorg needed for left assoc since we return ourselves with atomic right hand side.
    // However, we steal any comptime-flag on the LHS and keep it to ourselves.
    if (pLHSExpr->uNodeFlags & EPRENODEFLAG_IS_COMPTIME) {
        pLHSExpr->uNodeFlags &= ~EPRENODEFLAG_IS_COMPTIME;
        pBinop->uNodeFlags |= EPRENODEFLAG_IS_COMPTIME;
    }
    return pBinop; // our caller will handle searching for continuations against the full resulting supertight-binop
}

local_func PreAstNode* parse_expr_special(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("parsing token as special atomic expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(special);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_ATOMICEXPR_SPECIAL, 0, special);
    pResult->primaryPayload = pCurrent->uTokenPayload;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    return pResult;
}

local_func void parse_loop_finalizer(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 3
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("Special handling of break-or-loop-statement", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    pStatement->pivotToken1 = key;
    pStatement->uStatementKind = ESTATEMENT_LOOPFINALIZER;
    pCurrent = ++parserParams.parserState.pCurrentToken;
    pStatement->uExpectedNextBlockSpawning = EBLOCK_SPAWNING_EXPECTED;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if (is_explicit_linebreak(pCurrent))
            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
        if (*outError) {
            make_err_pre_node_during_multiline(pStatement->pMainNode, "After a loop finalizer keyword");
            return;
        }
        if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04 && u8(pCurrent->uTokenPayload) == EKEY_OR)
        {
            reference_current_token_as(orchainkey);
            pStatement->pivotToken2 = orchainkey;
            pCurrent = ++parserParams.parserState.pCurrentToken;
            if (is_explicit_linebreak(pCurrent))
                pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
            if (*outError) {
                make_err_pre_node_during_multiline(pStatement->pMainNode, "After 'or' keyword chaining loop finalizer");
                return;
            }
            if (pCurrent < parserParams.parserState.pLineTokenEnd)
            {
                if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04)
                {
                    switch(pCurrent->uTokenPayload)
                    {
                    case EKEY_ONBREAK:
                    case EKEY_ONDONE:
                    case EKEY_WHENNONE:
                        pStatement->pMainNode = parse_expr_special(parserParams, uDepthGuard+1, outError);
                        pCurrent = parserParams.parserState.pCurrentToken;
                        return;
                        if (is_explicit_linebreak(pCurrent))
                            pCurrent = starting_true_eat_lines_while_eol_or_forced_break(parserParams, outError);
                        if (*outError) {
                            make_err_pre_node_during_multiline(pStatement->pMainNode, "After chained loop finalizer");
                            return;
                        }
                    }
                }
                make_err_pre_node_expected_vs_found(pStatement->pMainNode, 0, "Expected other loop finalizer after or");
            } else
                make_err_pre_node_expected_found_eol(pStatement->pMainNode, 0, "Expected other loop finalizer after or");
        }
    }
}


local_func PreAstNode* parse_proc_signature(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of starting proc definition token", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(procKind);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_PROCLIKE_DEFINITION, 0, procKind);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    PreAstNode* pParamsPart = _parse_proc_params_all(parserParams, uDepthGuard+1, outError);
    Assert_(pParamsPart); // _parse_proc_params_all shall not return null
    check_and_wrapup_pre_node(pResult, primary, *pParamsPart);
    PreAstNode* pOptWhereClause = _try_parse_where_clause(parserParams, uDepthGuard+1, outError);
    if (pOptWhereClause)
        _wrapup_pre_node_secondary(pResult, *pOptWhereClause);
    return pResult;
}

local_func PreAstNode* parse_explicit_signature_key(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of starting explicit proc signature token", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(signkey);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPLICIT_SIGNATURE, 0, signkey);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pResult, primary, 0, "Looking for expression after 'signature' keyword", pResult);
    // Note: simply call parse_expression, so that macro-forms could also be validly parsed there
    PreAstNode* pActualSignatureExpr = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    check_and_wrapup_pre_node_err_if_null(pResult, primary, pActualSignatureExpr, "Expecting proc-like definition expression after 'signature' keyword");
    return pResult;
}

local_func PreAstNode* parse_parentized_expression(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of parentized expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(openingParens);
    PreAstNode* parentizedExpr = _prepare_pre_node(parserParams, ENODE_EXPR_PARENTISED, 0, openingParens);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, parentizedExpr, primary, 0, "Looking after opening parens", parentizedExpr);
    PreAstNode* exprWithin = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (!exprWithin)
        make_err_pre_node_expected_vs_found(exprWithin, 0, "expected expression within parens");
    check_and_wrapup_pre_node(parentizedExpr, primary, *exprWithin);
    pCurrent = parserParams.parserState.pCurrentToken;
    if (pCurrent < parserParams.parserState.pLineTokenEnd) {
        if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x00 && u8(pCurrent->uTokenPayload) == u8(')')) {
            reference_current_token_as(closingParens);
            parentizedExpr->secondaryToken = closingParens;
            pCurrent = ++parserParams.parserState.pCurrentToken;
            from_current_token_allows_break_if_multiline_err_wraparound(pCurrent, *parentizedExpr, "after closing parens");
        } else {
            PreAstNode* expectedErr; make_err_pre_node_expected_vs_found(expectedErr, u8(')'), "Expected closing parens");
            _wrapup_pre_node_secondary(parentizedExpr, *expectedErr);
        }
    } else {
        PreAstNode* expectedErr; make_err_pre_node_expected_found_eol(expectedErr, u8(')'), "Expected closing parens");
        _wrapup_pre_node_secondary(parentizedExpr, *expectedErr);
    }
    return parentizedExpr;
}

local_func_inl PreAstNode* _parse_ternary_if_either_or_no_then(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
    PreAstNode* eitherOrExpr = _prepare_pre_node(parserParams, ENODE_TERNARY_IF_EITHER_OR, 0, _on_error_get_ref_from_position(parserParams));
    PreAstNode* exprWhenTrue = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    check_and_wrapup_pre_node_err_if_null(eitherOrExpr, primary, exprWhenTrue, "expected expression when true for ternary if operator (ifx)");
    from_current_token_expects_on_line_strict_otherwise_err_within(pCurrent, eitherOrExpr, secondary, EKEY_ELSE, "expected 'else' keyword for ternary if operator (ifx)", eitherOrExpr);
    if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04 && u8(pCurrent->uTokenPayload) == EKEY_ELSE) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        debug_print_indent(uDepthGuard*2 + 1);
        platform_log_info("found 'else' keyword separation, parsing the following as expression when false of ternary-if expression (ifx)", true);
#endif
        reference_current_token_as(elseKey);
        eitherOrExpr->pivotalToken = elseKey;
        pCurrent = ++parserParams.parserState.pCurrentToken;
        from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, eitherOrExpr, secondary, 0, "looking for expression when false after 'else' for ternary if operator (ifx)", eitherOrExpr);
        PreAstNode* exprWhenFalse = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
        check_and_wrapup_pre_node_err_if_null(eitherOrExpr, secondary, exprWhenFalse, "expected expression when false after 'else' for ternary if operator (ifx)");
    } else {
        TokenRef wrapperToken = _on_error_get_ref_from_position(parserParams);
        eitherOrExpr->pivotalToken = wrapperToken;
        PreAstNode* expectedErr; make_err_pre_node_expected_vs_found(expectedErr, EKEY_ELSE, "expected 'else' keyword after expression when true for ternary if operator (ifx)");
        _wrapup_pre_node_secondary(eitherOrExpr, *expectedErr);
    }
    return eitherOrExpr;
}

local_func_inl PreAstNode* _parse_ternary_if_either_or(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("parsing the following as either/or of ternary if (ifx)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x04 && u8(pCurrent->uTokenPayload) == EKEY_THEN) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        debug_print_indent(uDepthGuard*2 + 1);
        platform_log_info("found optional 'then' keyword separation, continuing...", true);
#endif
        reference_current_token_as(thenKey);
        PreAstNode* thenWrap = _prepare_pre_node(parserParams, ENODE_TERNARY_IF_THENWRAP, 0, thenKey);
        pCurrent = ++parserParams.parserState.pCurrentToken;
        from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, thenWrap, primary, 0, "looking for expression after then", thenWrap);
        PreAstNode* afterThat = _parse_ternary_if_either_or_no_then(parserParams, uDepthGuard, outError);
        Assert_(afterThat); // _parse_ternary_if_either_or_no_then should not return null
        _wrapup_pre_node_primary(thenWrap, *afterThat);
        return thenWrap;
    } else {
        return _parse_ternary_if_either_or_no_then(parserParams, uDepthGuard, outError);
    }
}

local_func PreAstNode* parse_ternary_if_expression(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of ternary-if expression (ifx)", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(ifxKey);
    PreAstNode* ifxResult = _prepare_pre_node(parserParams, ENODE_EXPR_TERNARY_IF, 0, ifxKey);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, ifxResult, primary, 0, "looking for expression after 'ifx' (ternary if)", ifxResult);
    PreAstNode* conditionExpr = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    check_and_wrapup_pre_node_err_if_null(ifxResult, primary, conditionExpr, "expecting expression after 'ifx' (ternary if)");
    pCurrent = parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, ifxResult, secondary, EKEY_THEN, "looking for expression when true, or 'then' keyword", ifxResult);
    PreAstNode* thenElseExpr = _parse_ternary_if_either_or(parserParams, uDepthGuard+1, outError);
    Assert_(thenElseExpr); // _parse_ternary_if_either_or should not return null
    _wrapup_pre_node_secondary(ifxResult, *thenElseExpr);
    return ifxResult;
}

local_func PreAstNode* parse_static_or_dynamic(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of static-or-dynamic keyword", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_FOREIGNSOURCE, 0, key);
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pResult, primary, 0, "Looking for opening parens", pResult);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x00 && u8(pCurrent->uTokenPayload) == u8('(')) {
        // well' let parse_parentized_expression to handle everything after expected parens, and we'll
        //   further check for single expected expression param later.
        PreAstNode* pParams = parse_parentized_expression(parserParams, uDepthGuard+1, outError);
        Assert_(pParams); // parse_parentized_expression should not return null
        check_and_wrapup_pre_node(pResult, primary, *pParams);
        pCurrent = parserParams.parserState.pCurrentToken;
        from_current_token_expects_on_any_line_otherwise_err_within(pCurrent, pResult, secondary, 0, "Looking for procedure declaration expression after 'static(...)' or 'dynamic(...)'", pResult);
        // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
        //      but this is part of trying to parse "more" and error-report later.
        PreAstNode* pFollowup = try_parse_expression_or_list_allowing_vardecl(parserParams, uDepthGuard+1, outError);
        check_and_wrapup_pre_node_err_if_null(pResult, secondary, pFollowup, "Expected procedure declaration expression after 'static(...)' or 'dynamic(...)'");
    } else {
        PreAstNode* errExpected; make_err_pre_node_expected_vs_found(errExpected, u8('('), "Expected opening parens after static-or-dynamic keyword");
        _wrapup_pre_node_primary(pResult, *errExpected);
    }
    return pResult;
}

local_func PreAstNode* parse_distinct(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of distinct keyword", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_DISTINCT, 0, key);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pResult, primary, 0, "Looking for type expression after 'distinct' keyword", pResult);
    PreAstNode* pType = try_parse_expression_or_list_allowing_vardecl(parserParams, uDepthGuard+1, outError);
    // could do with "parse_expression" for the following, and furthermore not allowing explicit names...
    //      but this is part of trying to parse "more" and error-report later.
    check_and_wrapup_pre_node_err_if_null(pResult, primary, pType, "Expected type expression after 'distinct' keyword");
    return pResult;
}

local_func PreAstNode* parse_somethingof(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of sizeof/alignof/strideof/typeof keyword", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_SOMETHINGOF, 0, key);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_expects_on_line_or_break_otherwise_err_within(pCurrent, pResult, primary, 0, "Looking for opening parens", pResult);
    if ((pCurrent->uTokenKindAndSpaceFlag & 0x7C) == 0x00 && u8(pCurrent->uTokenPayload) == u8('(')) {
        // well' let parse_parentized_expression to handle everything after expected parens, and we'll
        //   further check for single expected expression param later.
        PreAstNode* pParams = parse_parentized_expression(parserParams, uDepthGuard+1, outError);
        Assert_(pParams); // parse_parentized_expression should not return null
        check_and_wrapup_pre_node(pResult, primary, *pParams);
    } else {
        PreAstNode* errExpected; make_err_pre_node_expected_vs_found(errExpected, u8('('), "Expected opening parens after sizeof/alignof/strideof/typeof keyword");
        _wrapup_pre_node_primary(pResult, *errExpected);
    }
    return pResult;
}

local_func PreAstNode* parse_complex_type_declaration(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of struct/enum/view keyword", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(key);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_COMPLEX_TYPE_DECL, 0, key);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_allows_on_line_or_break_otherwise_null_within(pCurrent, pResult, primary, "Following explicit linebreak after struct or enum or view keyword", pResult);
    PreAstNode* pOptParameter = try_parse_expression_or_list_allowing_vardecl(parserParams, uDepthGuard+1, outError);
    if (pOptParameter)
        _wrapup_pre_node_primary(pResult, *pOptParameter);
    return pResult;
}

local_func PreAstNode* parse_no_left_range_expression(ParserParams& parserParams, u16 uDepthGuard, u16* outError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    debug_print_indent(uDepthGuard*2 + 1);
    platform_log_info("special handling of '..' starting an expression", true);
#endif
    Token* pCurrent = parserParams.parserState.pCurrentToken;
    Assert_(pCurrent < parserParams.parserState.pLineTokenEnd);
    reference_current_token_as(rangeKey);
    PreAstNode* pResult = _prepare_pre_node(parserParams, ENODE_EXPR_NOLEFT_RANGE, 0, rangeKey);
    pCurrent = ++parserParams.parserState.pCurrentToken;
    from_current_token_allows_on_line_or_break_otherwise_null_within(pCurrent, pResult, primary, "Following explicit linebreak after '..' starting an expression", pResult);
    PreAstNode* pExprAfterwards = try_parse_expression(parserParams, false, 0, uDepthGuard+1, outError);
    if (pExprAfterwards)
        _wrapup_pre_node_secondary(pResult, *pExprAfterwards);
    return pResult;
}
#endif

/*
local_func void init_proc_tables_per_symbol_or_keyword()
{
    for (int i = 0; i < 255; i++) {
        tContinuationParsingProcsPerKeywordOrSymbol[i] = 0;
        tExpressionParsingProcsPerKeywordOrSymbol[i] = 0;
        tStatementParsingProcsPerKeywordOrSymbol[i] = 0;
    }

    tStatementParsingProcsPerKeywordOrSymbol[EKEY_IF] = parse_if_or_elif_or_while;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_ELIF] = parse_if_or_elif_or_while;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_ELSE] = parse_else_or_defer;

    tStatementParsingProcsPerKeywordOrSymbol[EKEY_FOR] = parse_for;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_WHILE] = parse_if_or_elif_or_while;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_ONBREAK] = parse_loop_finalizer;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_WHENNONE] = parse_loop_finalizer;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_ONDONE] = parse_loop_finalizer;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_CASE] = parse_case;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_WITH] = parse_with;

    tStatementParsingProcsPerKeywordOrSymbol[EKEY_NOOP] = parse_noop_or_unreachable;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_UNREACHABLE] = parse_noop_or_unreachable;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_RETURN] = parse_return;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_BREAK] = parse_break_or_loop;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_LOOP] = parse_break_or_loop;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_DEFER] = parse_else_or_defer;

    tStatementParsingProcsPerKeywordOrSymbol[EKEY_PAN_IF] = parse_pan_if_or_elif_or_scope;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_PAN_ELIF] = parse_pan_if_or_elif_or_scope;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_PAN_ELSE] = parse_pan_else_or_endif_or_endscope;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_PAN_ENDIF] = parse_pan_else_or_endif_or_endscope;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_PAN_SCOPE] = parse_pan_if_or_elif_or_scope;
    tStatementParsingProcsPerKeywordOrSymbol[EKEY_PAN_ENDSCOPE] = parse_pan_else_or_endif_or_endscope;

    tContinuationParsingProcsPerKeywordOrSymbol[u8('(')] = parse_paren_continuation;
    tContinuationParsingProcsPerKeywordOrSymbol[u8('[')] = parse_bracket_continuation;
    //tContinuationParsingProcsPerKeywordOrSymbol[u8('{')] = parse_curly_continuation;
    tContinuationParsingProcsPerKeywordOrSymbol[u8('.')] = parse_supertight_binop;
    tContinuationParsingProcsPerKeywordOrSymbol[u8('^')] = parse_pointer_deref;

    tExpressionParsingProcsPerKeywordOrSymbol[EMS_UNINITIALIZED] = parse_expr_special;
//    tExpressionParsingProcsPerKeywordOrSymbol[u8('^')] = parse_expr_special;
//    tExpressionParsingProcsPerKeywordOrSymbol[u8('*')] = parse_expr_special;
//    tExpressionParsingProcsPerKeywordOrSymbol[u8('?')] = parse_expr_special;

    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_SIGNATURE] = parse_explicit_signature_key;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_IFX] = parse_ternary_if_expression;
    tExpressionParsingProcsPerKeywordOrSymbol[ETOK_STRUCT_INIT] = parse_complex_lit_expression;
    tExpressionParsingProcsPerKeywordOrSymbol[ETOK_ARRAY_LIT] = parse_complex_lit_expression;

    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_PROC] = parse_proc_signature;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_FUNC] = parse_proc_signature;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_NOCTXPROC] = parse_proc_signature;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_NOCTXFUNC] = parse_proc_signature;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_PURE] = parse_proc_signature;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_COMPTIMEFUNC] = parse_proc_signature;
    // TODO : a little different than the above ?
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_MACRO] = parse_proc_signature;
    tExpressionParsingProcsPerKeywordOrSymbol[EMS_RANGE_INCL] = parse_no_left_range_expression;

    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_STATIC] = parse_static_or_dynamic;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_DYNAMIC] = parse_static_or_dynamic;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_DISTINCT] = parse_distinct;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_SIZEOF] = parse_somethingof;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_ALIGNOF] = parse_somethingof;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_STRIDEOF] = parse_somethingof;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_TYPEOF] = parse_somethingof;

    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_STRUCT] = parse_complex_type_declaration;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_ENUM] = parse_complex_type_declaration;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_VIEW] = parse_complex_type_declaration;
    tExpressionParsingProcsPerKeywordOrSymbol[EKEY_UNION] = parse_complex_type_declaration;

    tExpressionParsingProcsPerKeywordOrSymbol[u8('(')] = parse_parentized_expression;
    tExpressionParsingProcsPerKeywordOrSymbol[u8('{')] = parse_curly_standalone;
}
*/

#endif // LOCLIB_PREPARSER_H_

