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

#ifndef LOCLIB_SCAN_AND_TOK_H_
#define LOCLIB_SCAN_AND_TOK_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/HashSets.h"
#include "LocLib_TokenizerEnums.h"
//#include "LocLib_ParserEnums.h"
//#include "LocLib_Token.h"
#include "LocLib_SourceFileDescAndState.h"

exported_func_impl const u8* text_scan_next_line(const u8* pStart, const u8* pEnd,
    u16* outLineIndent, u16* outStartingTabIndents, u16* outCharCountWithoutIndentOrEOL, u16* outEOL, u16* ioErr)
{
    u64 indentField = 0;
    bool bOtherThanIndent = false;
    u64 uEOL;
    const u8* pStartOfLineAfterIndent = pStart;
    const u8* pCurrent = pStart;
    u8 c;

    // start of iteration on chars...
returnFromUTF8Check: // Or returning from 'utf8Check' label-block,
                     //   with 'pCurrent' already positionned to after the codepoint encoding in utf8.
    while (pCurrent < pEnd) {
        c = *pCurrent;
        if (c == '\0')
            goto errNul;
        else if (c == '\r') { // CarriageReturn. Expects LineFeed afterwards, or error
            if (pCurrent+1 < pEnd && pCurrent[1] == '\n') {
                uEOL = 2;
                goto nominalResult;
            } else
                goto errCR;
        } else if (c == '\n') { // Isolated LineFeed. Preferred for Unix, linux, macOS...
            uEOL = 1;
            goto nominalResult;
        } else {
            do
            {
                if (!bOtherThanIndent) {
                    if (c == ' ' || c == '\t') {
                        i64 iPosInIndent = i64(pCurrent - pStart);
                        if (iPosInIndent < 45) {
                            if (c == '\t')
                                indentField |= (1LL << iPosInIndent);
                            pStartOfLineAfterIndent++;
                            break;
                        } else
                            bOtherThanIndent = true;
                    } else
                        bOtherThanIndent = true;
                }
                // else : bOtherThanIndent OR fallthrough :
                if (c & 0x80) { // non-7b character. Expects valid utf8 or error
                    goto utf8Check;
                } // Otherwise NOOP : simply eats 7b ASCII, iterating until EOL
            } while (0);
            // Prepares next iteration
            pCurrent++;
        }
    }
    // exiting loop by end-of-string (by size) : (eg. EOF, or end of isolated string to parse...)

    // Note : outEOL would encode the number of control characters to skip in order
    //   to be positionned at start of next line after current call.
    uEOL = 0; // => 0 here since we reached EOF or EOS

    // => falling through nominal result

nominalResult: // or target of GOTO when reaching EOL within loop
    u16 uOutOfLineIndent = u16(pStartOfLineAfterIndent - pStart);
    *outLineIndent = uOutOfLineIndent;
    i64 iCharCountAfterIndent = u16(pCurrent - pStartOfLineAfterIndent);
    *outCharCountWithoutIndentOrEOL = u16(iCharCountAfterIndent);
    if (iCharCountAfterIndent + i64(uOutOfLineIndent) >= LINE_BUFFERSIZE)
        goto errLineTooLong;
    u16 uStartingTabs = 0;
    for (;uStartingTabs<uOutOfLineIndent; uStartingTabs++) {
        if (pStart[uStartingTabs] != '\t')
            break;
    }
    *outStartingTabIndents = uStartingTabs;
    *outEOL = u16(uEOL);
    return pCurrent + uEOL;

utf8Check:
    {   // When we get here, we just stubled upon a byte in text whose msb is 1
        //  => we cannot interpret it as simply a basic 7b ASCII carried by a single byte, 
        //      and thus we need to decode a multibyte utf8 encoding (and to check for its validity)
        i64 iCountUtf8Bytes;
        u8 threeMsb = c & 0xE0;
        u32 uCurrentCode;
        // Decoding and checking first byte of multi-byte encoding
        if (threeMsb == 0xC0) {             // 110xxxxx : start of two bytes encoding
            iCountUtf8Bytes = 2;
            uCurrentCode = u32(c & 0x1F) << 6;  // the 5 msb of final up-to-11b code are carried by first byte
        } else if (threeMsb == 0xE0) {
            char fourMsb = c & 0xF0;
            if (fourMsb == 0xF0) {          // 1110xxxx : start of three bytes encoding
                iCountUtf8Bytes = 3;
                uCurrentCode = u32(c & 0x0F) << 12; // the 4 msb of final up-to-16b code are carried by first byte
            } else {
                char fiveMsb = c & 0xF8;
                if (fiveMsb == 0xF0) {      // 11110xxx : start of four bytes encoding
                    iCountUtf8Bytes = 4;
                    uCurrentCode = u32(c & 0x07) << 18; // the 3 msb of final up-to-21b code are carried by first byte
                } else
                    goto errUTF8;
            }
        } else // if we got here, first byte starts with '10' as its two msb. This would mark a continuation byte...
            goto errUTF8;   // ... and as such, should never happen as first byte of a valid ut8 encoding...

        // Decoding and checking continuation bytes of multi-byte encoding
        for (i64 iByte = 1; iByte < iCountUtf8Bytes; iByte++) {
            if (pCurrent + iByte < pEnd) {
                char cCont = pCurrent[iByte];
                if (cCont & 0xC0 == 0x80) { // 10xxxxxx : valid continuation byte
                    uCurrentCode |= u32(cCont & 0x3F) << ((iCountUtf8Bytes - iByte - 1) * 6); // each carries 6b
                } else
                    goto errUTF8;           // hey, expected explicit continuation byte, marked with '10' as its two msb...
            } else
                goto errUTF8Truncation;     // end of string (or file) before we read all expected continuation bytes
        }
        // Checking resulting code-point (for validity of codepoint AND validity of actual encoding choice)
        if (uCurrentCode >= 0xD800 && uCurrentCode < 0xE000) // surrogate half for UTF16
            goto errUTF8; // ... is invalid as a codepoint as represented with utf8 encoding
        if (uCurrentCode >= 0x10FFFF) // non-encodable by UTF16
            goto errUTF8; // ... is invalid as a codepoint
        if (uCurrentCode < 0x80) // 7b ASCII
            goto errUTF8; // ... should not have used more than 1B encoding in the first place
        if (uCurrentCode < 0x0800) // no more than 11 bits required to encode
            if (iCountUtf8Bytes > 2)   // and more than a 2 bytes encoding
                goto errUTF8; // ... is also a forbidden case of overlong encoding
        if (uCurrentCode < 0x10000) // no more than 16 bits required to encode
            if (iCountUtf8Bytes > 3)   // and more than a 3 bytes encoding
                goto errUTF8; // ... is also a forbidden case of overlong encoding
        
        // Otherwise cool! this is valid multibytes utf8.
        pCurrent += iCountUtf8Bytes;
        //   and... we don't care about the value at this point. Just keep scanning for next char...
        goto returnFromUTF8Check;
    }

errNul:
    *ioErr = SERR_FOUND_CHARACTER_ZERO;
    goto errFinalize;
errCR:
    *ioErr = SERR_ISOLATED_CR_CHARACTER;
    goto errFinalize;
errLineTooLong:
    *ioErr = SERR_LINE_TOO_LONG;
    // We err of line too long, BUT at this point we read all of it, up to EOL => revert 'pCurrent'
    //   (which will now represent position of error) to actual position where we read over max.
    pCurrent = pStart + LINE_BUFFERSIZE;
    goto errFinalize;
errUTF8:
    *ioErr = SERR_INVALID_UTF8_ENCODING;
    goto errFinalize;
errUTF8Truncation:
    *ioErr = SERR_TRUNCATED_UTF8_CHARACTER;
    // fallthrough:
errFinalize:
    return pCurrent;
}

StringMap<int> g_allKeywords;

local_func void init_keywords() {

    g_allKeywords.init();

#define KEYWORD(syntaxName, name, assignValue)  g_allKeywords.insert(syntaxName, EKEY_ ## name);
    KEYWORDS_EMITTER_
#undef KEYWORD

}

class LiteTokenizer {
public:
    LiteTokenizer(const u8* pStart, const u8* pEnd,
                  i32* ioCommentLevel,
                  Token* outTokenArray,
                  SourceFileDescAndState* pSourceFile):
        _pLineStart(pStart), _pCurrentChar(pStart), _pEnd(pEnd),
        _pTokenStart(outTokenArray), _pNextTokenToEmit(outTokenArray),
        _pSourceFile(pSourceFile),
        _pCommentLevel(ioCommentLevel) {}

    int tokenize_skipping_comments(u16* ioErr);

private:
    const u8* _pCurrentChar;
    const u8* _pEnd;
    const u8* _pLineStart;
    Token* _pTokenStart;
    Token* _pNextTokenToEmit;
    SourceFileDescAndState* _pSourceFile;
    i32* _pCommentLevel;

    bool tokenize_skipping_within_nestable_comment(u16* ioErr);
    bool tokenize_number(u16* ioErr);
    bool tokenize_string(u16* ioErr);
    bool tokenize_identifier(u8 uStartsWithSymbolBefore, u16* ioErr);

    bool tokenize_potential_multi_symbol();

    void emit_symbol_token(u8 uSpaceAfterwards) {
        Token tok;
        tok.uTokenPayloadAndKind = u8(ETOK_KIND_SYMBOL) | uSpaceAfterwards | (u32(*_pCurrentChar) << 8);
        tok.uTokenStartCharOnLine = u16(_pCurrentChar - _pLineStart);
        tok.uTokenCharCount = 1;
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
    }

    void emit_multisymbol_token(const u8* pStartOfToken, int iMultiSymbolId, u8 uSpaceAfterwards) {
        Token tok;
        tok.uTokenPayloadAndKind = u8(ETOK_KIND_MULTISYMBOL) | uSpaceAfterwards | (iMultiSymbolId << 8);
        tok.uTokenStartCharOnLine = u16(pStartOfToken - _pLineStart);
        tok.uTokenCharCount = u16(_pCurrentChar - pStartOfToken);
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;	
    }

	void emit_keyword_token(u8 uStartsWithSymbolBefore, u16 uCharCount, int iKeywordId, u8 uSpaceAfterwards) {
        Token tok;
        tok.uTokenPayloadAndKind = u8(ETOK_KIND_KEYWORD) | uStartsWithSymbolBefore | uSpaceAfterwards | (iKeywordId << 8);
        tok.uTokenStartCharOnLine = u16(_pCurrentChar - _pLineStart) - uCharCount;
        tok.uTokenCharCount = uCharCount;
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
	}

	void emit_identifier_token(u8 uStartsWithSymbolBefore, u16 uCharCount, int iIdentifierIndex, u8 uSpaceAfterwards) {
        Token tok;
        tok.uTokenPayloadAndKind = u8(ETOK_KIND_IDENTIFIER) | uStartsWithSymbolBefore | uSpaceAfterwards | (iIdentifierIndex << 8);
        tok.uTokenStartCharOnLine = u16(_pCurrentChar - _pLineStart) - uCharCount;
        tok.uTokenCharCount = uCharCount;
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
	}
	void emit_codepoint_literal_token(const u8* pStartOfToken, const u8* pStartOfStringData,
									  const u8* pPositionOfEndingDelim,
									  u32 uCodePoint, u64 bHasMultiByteUtf8, u64 bHasBackslashEscapeSeq,
									  u8 uSpaceAfterwards)
    {
        Token tok;
        tok.uTokenPayloadAndKind = u8(ETOK_KIND_CODEPOINT) | uSpaceAfterwards | (uCodePoint << 8);
        tok.uTokenStartCharOnLine = u16(pStartOfToken - _pLineStart);
		// TODO : bHasMultiByteUtf8, bHasBackslashEscapeSeq ?
        tok.uTokenCharCount = u16(pPositionOfEndingDelim - pStartOfToken) + 1;
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
    }
	void emit_string_literal_token(const u8* pStartOfToken, const u8* pStartOfStringData, const u8* pPositionOfEndingDelim,
		u64 bIsStrictWithoutBackslashEscape, u64 bHasMultiByteUtf8, u64 bHasBackslashEscapeSeq, u8 uSpaceAfterwards)
	{
        Token tok;
        u16 uStartOfStringDataOffset = u16(pStartOfStringData - pStartOfToken);
        u16 uCodeStringBytesCount = u16(pPositionOfEndingDelim - pStartOfStringData);
        Assert_(uStartOfStringDataOffset < 256u);
        Assert_(uCodeStringBytesCount < 8192u);
        Assert_(bIsStrictWithoutBackslashEscape < 2u);
        Assert_(bHasMultiByteUtf8 < 2u);
        Assert_(bHasBackslashEscapeSeq < 2u);
        u32 uAdditionalPayload = (u32(uStartOfStringDataOffset) << 8) | (u32(uCodeStringBytesCount) << 16) | 
                                 (u32(bIsStrictWithoutBackslashEscape) << 29) | (u32(bHasMultiByteUtf8) << 30) | (u32(bHasBackslashEscapeSeq) << 31);
        tok.uTokenPayloadAndKind = u8(ETOK_KIND_STRING) | uSpaceAfterwards | uAdditionalPayload;
        tok.uTokenStartCharOnLine = u16(pStartOfToken - _pLineStart);
		// TODO : bIsStrictWithoutBackslashEscape, bHasMultiByteUtf8, bHasBackslashEscapeSeq ?
        tok.uTokenCharCount = u16(pPositionOfEndingDelim - pStartOfToken) + 1;
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
	}

    void emit_float_num_literal_token(const u8* pStartOfToken, i32 iBase, i32 iPositionOfFirstSignificantDigit,
        i32 iSignificantDigitsCount, i32 iExponentAtLastSignificantDigit, i32 iPositionOfDot,
        i32 iPositionOfExponentDigits, u8 uIsRawExponentNeg, i32 iExponentAbs, u8 uSpaceAfterwards)
    {
        u32 uBaseEncoding;
        switch (iBase) {
            case 2: uBaseEncoding = 0u; break;
            case 8: uBaseEncoding = 1u; break;
            case 10: uBaseEncoding = 2u; break;
            case 16: uBaseEncoding = 3u; break;
            default: Assert_(false); uBaseEncoding = 0;
        }
        u32 uAdditionalPayload = 0;                  // payload 0 would instantly denote '0'
        if (iPositionOfFirstSignificantDigit >= 0) { // necessarily zero if did not find significant digit...
            // two possibilities:
            //      1) first significant digit position less than 32, total significant digits less than 128,
            //          final exponent at Least Significant Digit between [-2048, 2048[
            //         --> encodes as 5b start pos ; 7b total significant digits ; 12b exponent at Least Significant Digit ;
            //      2) otherwise:
            //         --> encodes as 11b start pos of first significant digit OR dot, (max 2047),
            //             11b pos of exponent digits or end-of-(max 2047), 1b hasExponent, 1b isExponentNeg.
            //             ...and flag as 'TOKENKINDFLAG_EXTREME_NUMERAL'
            Assert_(iPositionOfFirstSignificantDigit < 2048);
            Assert_(iPositionOfFirstSignificantDigit + iSignificantDigitsCount < 2048);
            Assert_(iPositionOfExponentDigits < 2048);
            if (iPositionOfFirstSignificantDigit < 32 && iSignificantDigitsCount < 128 && // nominal case, of 'reasonable' dimensions
                    iExponentAtLastSignificantDigit >= -2048 && iExponentAtLastSignificantDigit < 2048) {
                uAdditionalPayload = (iPositionOfFirstSignificantDigit << 8) |
                                     (iSignificantDigitsCount << 13) |
                                     (iExponentAtLastSignificantDigit << 20);
            } else {
                int iFirstPosition = iPositionOfFirstSignificantDigit < iPositionOfDot ?
                                        iPositionOfFirstSignificantDigit : iPositionOfDot;
                int iSecondPosition = iPositionOfExponentDigits > 0 ?
                                        iPositionOfExponentDigits : iPositionOfFirstSignificantDigit + iSignificantDigitsCount;
                uAdditionalPayload = (iFirstPosition << 8) | (iSecondPosition << 19) |
                                     (iPositionOfExponentDigits > 0 && iExponentAbs ? (1u << 30) : 0u) |
                                     (uIsRawExponentNeg << 31) | TOKENKINDFLAG_EXTREME_NUMERAL;
            }
        }
        Token tok;
        tok.uTokenPayloadAndKind = u8(TOKEN_CATEGORY_FLOATINGPT) | uBaseEncoding | uSpaceAfterwards | uAdditionalPayload;
        tok.uTokenStartCharOnLine = u16(pStartOfToken - _pLineStart);
        tok.uTokenCharCount = u16(_pCurrentChar - pStartOfToken);
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
    }

    void emit_natural_num_literal_token(const u8* pStartOfToken, i32 iBase, i32 iPositionOfFirstSignificantDigit, 
        i32 iSignificantDigitsCountIntPart, u64 uAlreadyDecodedIfLessThan24b, u8 uSpaceAfterwards)
    {
        u32 uBaseEncoding;
        switch (iBase) {
            case 2: uBaseEncoding = 0u; break;
            case 8: uBaseEncoding = 1u; break;
            case 10: uBaseEncoding = 2u; break;
            case 16: uBaseEncoding = 3u; break;
            default: Assert_(false); uBaseEncoding = 0;
        }
        u32 uAdditionalPayload = 0;                  // payload 0 would instantly denote '0'
        if (iPositionOfFirstSignificantDigit >= 0) { // necessarily zero if did not find significant digit...
            Assert_(iPositionOfFirstSignificantDigit < 2048);
            Assert_(iSignificantDigitsCountIntPart < 2048);
            // two possibilities:
            //      1) 'uAlreadyDecodedIfLessThan24b' does indeed fit in 24b (otherwise caller shall position to arbitrary larger),
            //          --> simply encodes 24b uAdditionalPayload
            //      2) otherwise:
            //         --> encodes as 11b start pos of first significant digit (max 2047),
            //         --> 11b significant digit count (max 2047),
            //         --> 3b unused
            if (uAlreadyDecodedIfLessThan24b < u64(0x0100'0000u)) {
                uAdditionalPayload = u32(uAlreadyDecodedIfLessThan24b) << 8;
            } else {
                uAdditionalPayload = (iPositionOfFirstSignificantDigit << 8) | (iSignificantDigitsCountIntPart << 19) | 
                                     TOKENKINDFLAG_EXTREME_NUMERAL;
            }
        }
        Token tok;
        tok.uTokenPayloadAndKind = u8(TOKEN_CATEGORY_NATNUM) | uBaseEncoding | uSpaceAfterwards | uAdditionalPayload;
        tok.uTokenStartCharOnLine = u16(pStartOfToken - _pLineStart);
        tok.uTokenCharCount = u16(_pCurrentChar - pStartOfToken);
        *_pNextTokenToEmit = tok;
        _pNextTokenToEmit++;
    }
};

int LiteTokenizer::tokenize_skipping_comments(u16* ioErr)
{
    const u8* pEnd = _pEnd;
    u8 c;
    if (*_pCommentLevel) {
        if (!tokenize_skipping_within_nestable_comment(ioErr))
            goto errFromChildSub;
    }

onIterate:
    while (_pCurrentChar < pEnd) {
        c = *_pCurrentChar;
        if (c == ' ' || c == '\t') {
            _pCurrentChar++;
        } else {
            u8 cLowerIfAlpha = c | 32;
            if ((cLowerIfAlpha >= u8('a') && cLowerIfAlpha <= u8('z')) || c == u8('_')) {
                if (!tokenize_identifier(0, ioErr))
                    goto errFromChildSub;
            } else if (c >= u8('0') && c <= u8('9')) {
                if (!tokenize_number(ioErr))
                    goto errFromChildSub;
            } else if (c == u8('/')) {
                if (_pCurrentChar + 1 < pEnd) {
                    char cNext = _pCurrentChar[1];
                    if (cNext == u8('*')) {
                        *_pCommentLevel += 1;
                        goto onReachNestableComment;
                    } else if (cNext == u8('/')) {
                        goto onReachCommentToEOL;
                    }
                }
                goto onReachSymbol;
            } else if (c == u8('"'))
                goto onReachString;
            else if (c == u8('!')) {
                if (_pCurrentChar + 1 < pEnd) {
                    u8 cNext = _pCurrentChar[1];
                    if (cNext == u8('"'))
                        goto onReachString;
                }
                goto onReachSymbol;
            } else if (c & 0x80)
                goto errNonEscapedUtf8;
            else
                goto onReachSymbol;
        }
    } // Nominal end of scanned line
    return int(_pNextTokenToEmit - _pTokenStart);

onReachCommentToEOL:
    // we do not emit tokens for comments, and this comment takes us to end of line
    // => we'll simply end there, since our job is to tokenize only this line.
    return int(_pNextTokenToEmit - _pTokenStart);

onReachNestableComment:
    _pCurrentChar += 2;
    if (!tokenize_skipping_within_nestable_comment(ioErr))
        goto errFromChildSub;
    goto onIterate;

onReachSymbol:
    if (_pCurrentChar + 1 < pEnd) {
        u8 cNext = _pCurrentChar[1];
        if (cNext != u8(' ') && cNext != u8('\t')) {
            u8 cLowerIfAlpha = cNext | 32;
            if (cNext == u8('_') || (cLowerIfAlpha >= u8('a') && cLowerIfAlpha <= u8('z'))) {
                if (c == u8('#')) {
                    if (!tokenize_identifier(PAN_BEFORE, ioErr))
                        goto errFromChildSub;
                    goto onIterate;
                } else if (c == u8('@')) {
                    if (!tokenize_identifier(AT_BEFORE, ioErr))
                        goto errFromChildSub;
                    goto onIterate;
                } /*else if (c == u8('$')) {
                    if (!tokenize_identifier(DOLLAR_BEFORE, ioErr))
                        goto errFromChildSub;
                    goto onIterate;
                }*/
            } else if (cNext == u8('"') && c == u8('#'))
                goto onReachString;
            if (!tokenize_potential_multi_symbol()) {
                emit_symbol_token(0);
                _pCurrentChar++;
            }
            goto onIterate;
        }
    }
    emit_symbol_token(TOKENKINDFLAG_SPACE_AFTERWARDS);
    _pCurrentChar++;
    goto onIterate;

onReachString:
    if (!tokenize_string(ioErr))
        goto errFromChildSub;
    goto onIterate;

errNonEscapedUtf8:
    *ioErr = TERR_INVALID_EXTENDED_UTF8_IN_CODE;
    goto onError;

errFromChildSub:
    // fallthrough
onError:
    return int(_pNextTokenToEmit - _pTokenStart);
}

bool LiteTokenizer::tokenize_potential_multi_symbol()
{
    const u8* pStart = _pCurrentChar;
    u16 uMax = u16(_pEnd - pStart);
    int iMultiSymbol = 0;

    if (uMax > 2) {
        u8 first = pStart[0];
        u8 second = pStart[1];
        u8 third = pStart[2];
        if (third == u8('=')) {
            if (second == u8('%')) {
                switch(first) {
                    case u8('+'): iMultiSymbol = ESYMB_MODULO_ADD_ASSIGN;
                        static_assert(static_strings_equal("+%=", tStandardPayloadsStr[ESYMB_MODULO_ADD_ASSIGN]), "multisymbol decode mismatch");
                        break;
                    case u8('-'): iMultiSymbol = ESYMB_MODULO_SUB_ASSIGN;
                        static_assert(static_strings_equal("-%=", tStandardPayloadsStr[ESYMB_MODULO_SUB_ASSIGN]), "multisymbol decode mismatch");
                        break;
                    case u8('*'): iMultiSymbol = ESYMB_MODULO_MUL_ASSIGN;
                        static_assert(static_strings_equal("*%=", tStandardPayloadsStr[ESYMB_MODULO_MUL_ASSIGN]), "multisymbol decode mismatch");
                        break;
                    case u8('/'): iMultiSymbol = ESYMB_INT_QUO_ASSIGN;
                        static_assert(static_strings_equal("/%=", tStandardPayloadsStr[ESYMB_INT_QUO_ASSIGN]), "multisymbol decode mismatch");
                        break;
                    case u8('%'): iMultiSymbol = ESYMB_INT_REM_ASSIGN;
                        static_assert(static_strings_equal("%%=", tStandardPayloadsStr[ESYMB_INT_REM_ASSIGN]), "multisymbol decode mismatch");
                        break;
                    case u8('^'): iMultiSymbol = ESYMB_BIT_XOR_ASSIGN;
                        static_assert(static_strings_equal("^%=", tStandardPayloadsStr[ESYMB_BIT_XOR_ASSIGN]), "multisymbol decode mismatch");
                        break;
                }
            } else if (first == u8('>') && second == u8('>')) {
                iMultiSymbol = ESYMB_RSH_ASSIGN;
                static_assert(static_strings_equal(">>=", tStandardPayloadsStr[ESYMB_RSH_ASSIGN]), "multisymbol decode mismatch");
            } else if (first == u8('<') && second == u8('<')) {
                iMultiSymbol = ESYMB_LSH_ASSIGN;
                static_assert(static_strings_equal("<<=", tStandardPayloadsStr[ESYMB_LSH_ASSIGN]), "multisymbol decode mismatch");
            } else if (first == u8('*') && second == u8('*')) {
                iMultiSymbol = ESYMB_POW_ASSIGN;
                static_assert(static_strings_equal("**=", tStandardPayloadsStr[ESYMB_POW_ASSIGN]), "multisymbol decode mismatch");
            } else if (first == u8('+') && second == u8('+')) {
                iMultiSymbol = ESYMB_CONCAT_ASSIGN;
                static_assert(static_strings_equal("++=", tStandardPayloadsStr[ESYMB_CONCAT_ASSIGN]), "multisymbol decode mismatch");
            }

        } else if (third == u8('-') && first == u8('-') && second == u8('-')) {
            iMultiSymbol = ESYMB_UNINITIALIZED;
            static_assert(static_strings_equal("---", tStandardPayloadsStr[ESYMB_UNINITIALIZED]), "multisymbol decode mismatch");
        } else if (first == u8('.') && second == u8('.') && third == u8('<')) {
            iMultiSymbol = ESYMB_RANGE_EXCL;
            static_assert(static_strings_equal("..<", tStandardPayloadsStr[ESYMB_RANGE_EXCL]), "multisymbol decode mismatch");
        }

        if (iMultiSymbol)
            _pCurrentChar += 3;
    }

    if (!iMultiSymbol && uMax > 1) {
        u8 first = pStart[0];
        u8 second = pStart[1];
        if (second == u8('=')) {
            switch(first) {
                case u8('='): iMultiSymbol = ESYMB_ARE_EQUAL;
                    static_assert(static_strings_equal("==", tStandardPayloadsStr[ESYMB_ARE_EQUAL]), "multisymbol decode mismatch");
                    break;
                case u8('!'): iMultiSymbol = ESYMB_ARE_NOT_EQUAL;
                    static_assert(static_strings_equal("!=", tStandardPayloadsStr[ESYMB_ARE_NOT_EQUAL]), "multisymbol decode mismatch");
                    break;
                case u8('<'): iMultiSymbol = ESYMB_LESSER_OR_EQ;
                    static_assert(static_strings_equal("<=", tStandardPayloadsStr[ESYMB_LESSER_OR_EQ]), "multisymbol decode mismatch");
                    break;
                case u8('>'): iMultiSymbol = ESYMB_GREATER_OR_EQ;
                    static_assert(static_strings_equal(">=", tStandardPayloadsStr[ESYMB_GREATER_OR_EQ]), "multisymbol decode mismatch");
                    break;
                case u8(':'): iMultiSymbol = ESYMB_ASSIGNMENT;
                    static_assert(static_strings_equal(":=", tStandardPayloadsStr[ESYMB_ASSIGNMENT]), "multisymbol decode mismatch");
                    break;
                case u8('+'): iMultiSymbol = ESYMB_ADD_ASSIGN;
                    static_assert(static_strings_equal("+=", tStandardPayloadsStr[ESYMB_ADD_ASSIGN]), "multisymbol decode mismatch");
                    break;
                case u8('-'): iMultiSymbol = ESYMB_SUB_ASSIGN;
                    static_assert(static_strings_equal("-=", tStandardPayloadsStr[ESYMB_SUB_ASSIGN]), "multisymbol decode mismatch");
                    break;
                case u8('*'): iMultiSymbol = ESYMB_MUL_ASSIGN;
                    static_assert(static_strings_equal("*=", tStandardPayloadsStr[ESYMB_MUL_ASSIGN]), "multisymbol decode mismatch");
                    break;
                case u8('/'): iMultiSymbol = ESYMB_DIV_ASSIGN;
                    static_assert(static_strings_equal("/=", tStandardPayloadsStr[ESYMB_DIV_ASSIGN]), "multisymbol decode mismatch");
                    break;
                case u8('%'): iMultiSymbol = ESYMB_MOD_ASSIGN;
                    static_assert(static_strings_equal("%=", tStandardPayloadsStr[ESYMB_MOD_ASSIGN]), "multisymbol decode mismatch");
                    break;
                case u8('&'): iMultiSymbol = ESYMB_BIT_AND_ASSIGN;
                    static_assert(static_strings_equal("&=", tStandardPayloadsStr[ESYMB_BIT_AND_ASSIGN]), "multisymbol decode mismatch");
                    break;
                case u8('|'): iMultiSymbol = ESYMB_BIT_OR_ASSIGN;
                    static_assert(static_strings_equal("|=", tStandardPayloadsStr[ESYMB_BIT_OR_ASSIGN]), "multisymbol decode mismatch");
                    break;
            }
        } else if (second == u8('%')) {
            switch(first) {
                case u8('+'): iMultiSymbol = ESYMB_MODULO_ADD;
                    static_assert(static_strings_equal("+%", tStandardPayloadsStr[ESYMB_MODULO_ADD]), "multisymbol decode mismatch");
                    break;
                case u8('-'): iMultiSymbol = ESYMB_MODULO_SUB;
                    static_assert(static_strings_equal("-%", tStandardPayloadsStr[ESYMB_MODULO_SUB]), "multisymbol decode mismatch");
                    break;
                case u8('*'): iMultiSymbol = ESYMB_MODULO_MUL;
                    static_assert(static_strings_equal("*%", tStandardPayloadsStr[ESYMB_MODULO_MUL]), "multisymbol decode mismatch");
                    break;
                case u8('/'): iMultiSymbol = ESYMB_INT_QUOTIENT;
                    static_assert(static_strings_equal("/%", tStandardPayloadsStr[ESYMB_INT_QUOTIENT]), "multisymbol decode mismatch");
                    break;
                case u8('%'): iMultiSymbol = ESYMB_INT_REMAINDER;
                    static_assert(static_strings_equal("%%", tStandardPayloadsStr[ESYMB_INT_REMAINDER]), "multisymbol decode mismatch");
                    break;
                case u8('^'): iMultiSymbol = ESYMB_BIT_XOR;
                    static_assert(static_strings_equal("^%", tStandardPayloadsStr[ESYMB_BIT_XOR]), "multisymbol decode mismatch");
                    break;
            }
        } else if (second == u8('>')) {
            switch (first) {
                case u8('>'): iMultiSymbol = ESYMB_RIGHT_SHIFT;
                    static_assert(static_strings_equal(">>", tStandardPayloadsStr[ESYMB_RIGHT_SHIFT]), "multisymbol decode mismatch");
                    break;
                case u8('='): iMultiSymbol = ESYMB_IMPLIES;
                    static_assert(static_strings_equal("=>", tStandardPayloadsStr[ESYMB_IMPLIES]), "multisymbol decode mismatch");
                    break;
                case u8('-'): iMultiSymbol = ESYMB_ARROW;
                    static_assert(static_strings_equal("->", tStandardPayloadsStr[ESYMB_ARROW]), "multisymbol decode mismatch");
                    break;
            }
        } else if (first == u8('.')) {
            switch (second) {
                case u8('['): iMultiSymbol = ESYMB_OPENING_ARRAY_LIT;
                    static_assert(static_strings_equal(".[", tStandardPayloadsStr[ESYMB_OPENING_ARRAY_LIT]), "multisymbol decode mismatch");
                    break;
                case u8('{'): iMultiSymbol = ESYMB_OPENING_SET_LIT;
                    static_assert(static_strings_equal(".{", tStandardPayloadsStr[ESYMB_OPENING_SET_LIT]), "multisymbol decode mismatch");
                    break;
                case u8('('): iMultiSymbol = ESYMB_OPENING_MAP_LIT;
                    static_assert(static_strings_equal(".(", tStandardPayloadsStr[ESYMB_OPENING_MAP_LIT]), "multisymbol decode mismatch");
                    break;
            }
        } else if (first == u8('<') && second == u8('<')) {
            iMultiSymbol = ESYMB_LEFT_SHIFT;
            static_assert(static_strings_equal("<<", tStandardPayloadsStr[ESYMB_LEFT_SHIFT]), "multisymbol decode mismatch");
        } else if (first == u8('<') && second == u8(':')) {
            iMultiSymbol = ESYMB_SUBTAG_DECL;
            static_assert(static_strings_equal("<:", tStandardPayloadsStr[ESYMB_SUBTAG_DECL]), "multisymbol decode mismatch");
        } else if (first == u8('.') && second == u8('.')) {
            iMultiSymbol = ESYMB_RANGE_INCL;
            static_assert(static_strings_equal("..", tStandardPayloadsStr[ESYMB_RANGE_INCL]), "multisymbol decode mismatch");
        } else if (first == u8('*') && second == u8('*')) {
            iMultiSymbol = ESYMB_POW;
            static_assert(static_strings_equal("**", tStandardPayloadsStr[ESYMB_POW]), "multisymbol decode mismatch");
        } else if (first == u8(':') && second == u8(':')) {
            iMultiSymbol = ESYMB_CONST_DECL;
            static_assert(static_strings_equal("::", tStandardPayloadsStr[ESYMB_CONST_DECL]), "multisymbol decode mismatch");
        } else if (first == u8(':') && second == u8('<')) {
            iMultiSymbol = ESYMB_SLICE_TO_EXCL;
            static_assert(static_strings_equal(":<", tStandardPayloadsStr[ESYMB_SLICE_TO_EXCL]), "multisymbol decode mismatch");
        } else if (first == u8('@') && second == u8('[')) {
            iMultiSymbol = ESYMB_OPENING_DYNARRAY;
            static_assert(static_strings_equal("@[", tStandardPayloadsStr[ESYMB_OPENING_DYNARRAY]), "multisymbol decode mismatch");
        } else if (first == u8('+') && second == u8('+')) {
            iMultiSymbol = ESYMB_CONCAT;
            static_assert(static_strings_equal("++", tStandardPayloadsStr[ESYMB_CONCAT]), "multisymbol decode mismatch");
        } else if (first == u8('^') && second == u8('.')) {
            iMultiSymbol = ESYMB_POINTER_DECL;
            static_assert(static_strings_equal("^.", tStandardPayloadsStr[ESYMB_POINTER_DECL]), "multisymbol decode mismatch");
        }

        if (iMultiSymbol)
            _pCurrentChar += 2;
    }

    if (iMultiSymbol) {
	    u8 bSpaceAfterwards = TOKENKINDFLAG_SPACE_AFTERWARDS;
	    if (_pCurrentChar < _pEnd) {
		    u8 cNext = *_pCurrentChar;
		    if (cNext != u8(' ') && cNext != u8('\t')) {
			    bSpaceAfterwards = 0;
		    }
	    }
        emit_multisymbol_token(pStart, iMultiSymbol, bSpaceAfterwards);
        return true;
    } else
        return false;
}

bool LiteTokenizer::tokenize_skipping_within_nestable_comment(u16* ioErr)
{
    const u8* pEnd = _pEnd;
    const u8* pCommentStart = _pCurrentChar;
    const u8* pOneBeforeEnd = pEnd-1;
    i64 bEndOfBlock = 0;
    u8 c;
    while (_pCurrentChar < pEnd) {
        c = *_pCurrentChar;
        if (_pCurrentChar < pOneBeforeEnd) {
            if (c == u8('/')) {
                u8 cNext = _pCurrentChar[1];
                if (cNext == u8('*')) {
                    *_pCommentLevel += 1;
                    _pCurrentChar += 2;
                } else if (cNext == u8('/')) {
                    _pCurrentChar = pEnd;
                    return true;
                } else
                    _pCurrentChar++;
            } else if (c == u8('*')) {
                u8 cNext = _pCurrentChar[1];
                if (cNext == u8('/')) {
                    *_pCommentLevel -= 1;
                    _pCurrentChar += 2;
                    if (*_pCommentLevel == 0)
                        return true;
                    Assert_(*_pCommentLevel > 0);
                } else
                    _pCurrentChar++;
            } else
                _pCurrentChar++;
        } else
            _pCurrentChar++;
    }
    return true;
}

bool LiteTokenizer::tokenize_number(u16* ioErr)
{
    // Assumed called ONLY when _pCurrentChar points to a digit in ['0'..'9']
    const u8* pStart = _pCurrentChar;
    const u8* pEnd = _pEnd;
    u8 c = *pStart;
	i32 iBase = 10;
	i32 iMaxDigitBelow10 = 9;
    i32 iMaxAlreadyDecodedIntDigits = 8; // an 'already decoded' below 2^24 (16M) => Max 8 decimal digits
    // detecting and reacting to possible '0x', '0b' or '0o' prefix
    if (c == u8('0') && pStart + 1 < pEnd) {
        u8 cNext = pStart[1];
        if (cNext == u8('x')) {
			_pCurrentChar += 2;
            iMaxAlreadyDecodedIntDigits = 6; // Max 2^24 (16M) => Max 6 hex digits
            iBase = 16;
        } else if (cNext == u8('b')) {
			_pCurrentChar += 2;
			iMaxDigitBelow10 = 1;
            iMaxAlreadyDecodedIntDigits = 24; // Max 2^24 (16M) => Max 24 binary digits
            iBase = 2;
        } else if (cNext == u8('o')) {
			_pCurrentChar += 2;
			iMaxDigitBelow10 = 7;
            iMaxAlreadyDecodedIntDigits = 8; // Max 2^24 (16M) => Max 8 octal digits
            iBase = 8;
        }
    }

    i32 iPositionOfFirstSignificantDigit = -1;
    i32 iPositionOfDot = -1;
    i32 iPositionOfExponentFirstDigit = -1;

    i32 iSignificantDigitCountIntPart = 0;
    i32 iSignificantDigitCountTotal = 0;
    i32 iIntPartTrailingZeroes = 0;
    i32 iFracPartTrailingZeroes = 0;
    u32 uHasSeenZeroDigit = 0; // 'non-significant' 0 - only for error report if stays false (after 0x, 0b or 0o prefixes).

    i32 iExponentAbs = 0;
	i32 iExponentSign = 1;
    i32 iExponentDigits = 0;

    u64 uAlreadyDecodedSmallInt = 0;

	while (_pCurrentChar < pEnd) {
		c = *_pCurrentChar;
		if (c == u8('0')) {
            if (iPositionOfDot < 0) { // still processing int part
                if (iPositionOfFirstSignificantDigit >= 0) {    // ...and already found non-zero digit
                    iSignificantDigitCountIntPart++;
                    if (iSignificantDigitCountIntPart <= iMaxAlreadyDecodedIntDigits)
                        uAlreadyDecodedSmallInt *= u64(iBase);
                    iIntPartTrailingZeroes++;
                } else             // processing int part, still not having found significant digit
                    uHasSeenZeroDigit = 1;

            } else {               // processing frac part
                if (iPositionOfFirstSignificantDigit >= 0) // ...and already found non-zero digit
                    iFracPartTrailingZeroes++;
                else {             // processing frac part while having found only zeroes:
                    iSignificantDigitCountIntPart--; // counting number in int part in reverse
                    uHasSeenZeroDigit = 1;
                }
            }

		} else if ((c >= u8('0') && c <= u8('0') + iMaxDigitBelow10) ||
                   (iBase == 16 && ((c >= u8('A') && c <= u8('F')) ||
                                    (c >= u8('a') && c <= u8('f'))))) {
            if (iPositionOfFirstSignificantDigit < 0)
                iPositionOfFirstSignificantDigit = i32(_pCurrentChar - pStart);
            if (iPositionOfDot < 0) { // still processing int part
                iSignificantDigitCountIntPart++;
                if (iSignificantDigitCountIntPart <= iMaxAlreadyDecodedIntDigits) {
                    uAlreadyDecodedSmallInt *= u64(iBase);
                    if (c <= u8('9'))
                        uAlreadyDecodedSmallInt += u64(c - u8('0'));
                    else {
                        Assert_(iBase == 16);
                        u8 cLowerIfAlpha = c | 32;
                        uAlreadyDecodedSmallInt += 10uLL + u64(cLowerIfAlpha - u8('a'));
                    }
                }
                iSignificantDigitCountTotal += iIntPartTrailingZeroes + 1;
                iIntPartTrailingZeroes = 0;
            } else { // processing frac part
                iSignificantDigitCountTotal += iIntPartTrailingZeroes + iFracPartTrailingZeroes + 1;
                iIntPartTrailingZeroes = 0;
                iFracPartTrailingZeroes = 0;
            }
		} else if (c == u8('.')) {
            if (_pCurrentChar + 1 < pEnd && _pCurrentChar[1] == u8('.')) {
                // stops on dot followed by other dot to avoid eating ".." and "..<" multisymbols
                break;
            } else {
                if (iPositionOfDot < 0)
                    iPositionOfDot = i32(_pCurrentChar - pStart);
                else
                    break; // will raise error later
            }
        } else if (c == u8('_')) {
            // NOOP, eating '_' silently
        } else {
			u8 cLowerIfAlpha = c | 32;
            if (iBase == 10) {
                if (cLowerIfAlpha == u8('e'))
                    goto parseExponent;
            } else {
                if (cLowerIfAlpha == u8('p'))
                    goto parseExponent;
            }
            // or reached char not part of the number format => breaking
            break;
        }
		_pCurrentChar++;
	}
    goto afterParse;

parseExponent:
    if (_pCurrentChar + 1 < pEnd) {
        if (_pCurrentChar[1] == u8('-')) {
            iExponentSign = -1;
            _pCurrentChar++;
            if (_pCurrentChar + 1 >= pEnd)
                goto noExponentDigit;
        } else if (_pCurrentChar[1] == u8('+')) {
            _pCurrentChar++;
            if (_pCurrentChar + 1 >= pEnd)
                goto noExponentDigit;
        }
        _pCurrentChar++;
        // now decode exponent (always decimal...)
        while (_pCurrentChar < pEnd) {
		    c = *_pCurrentChar;
            if (c >= u8('0') && c <= u8('9')) {
                if (iPositionOfExponentFirstDigit < 0)
                    iPositionOfExponentFirstDigit = i32(_pCurrentChar - pStart);
                iExponentDigits++;
                if (iExponentDigits > 9) { // max +-999,999,999 exponent (either for decimal point or binary point) during parsing
                    // we even include leading 'zeroes' in that max, to simplify matters here.
		            *ioErr = TERR_OVERSHOT_MAX_EXPONENT_DIGITS_IN_NUMERIC_LITERAL;
		            return false;
                }
                iExponentAbs *= 10;
                iExponentAbs += c - u8('0');
            } else if (c == u8('_')) {
                // NOOP, eating '_' silently
            } else
                break;
            _pCurrentChar++;
        }
    }
    if (!iExponentDigits) { noExponentDigit:
		*ioErr = TERR_NO_EXPONENT_DIGIT_AFTER_EXPONENT_CHARACTER_IN_NUMERIC_LITERAL;
		return false;
    }

afterParse:
    // uHasSeenDigit was only set previously when encountered a non-significant 0
    // => consolidate that check against "iSignificantDigitCountTotal" also being non zero
	if (0 == (uHasSeenZeroDigit | iSignificantDigitCountTotal)) {
		*ioErr = TERR_NO_VALID_DIGIT_AFTER_NUMBER_LITERAL_BASE_PREFIX;
		return false;
	}
	u8 bSpaceAfterwards = TOKENKINDFLAG_SPACE_AFTERWARDS;
	if (_pCurrentChar < pEnd) {
		u8 cNext = *_pCurrentChar;
		if (cNext != u8(' ') && cNext != u8('\t')) {
			bSpaceAfterwards = 0;
			u8 cNextLowerIfAlpha = cNext | 32;
			if ((cNext >= u8('0') && cNext <= u8('9')) || (cNext == u8('.') && iPositionOfDot >= 0) ||
				    (cNextLowerIfAlpha >= u8('a') && cNextLowerIfAlpha <= u8('z'))) {
				*ioErr = TERR_INVALID_CHARACTER_FOLLOWING_NUMBER_LITERAL;
				return false;
			}
		}
	}
	
	if (iPositionOfDot > 0 || iPositionOfExponentFirstDigit > 0) {
        int iSignificantDigitsAfterDot = iSignificantDigitCountTotal - iSignificantDigitCountIntPart;
        int iExponentShiftPerDigit = 1; // decimal with 10^ exponent, or binary with 2^ exponent are 1 exp shift per digit.
        if (iBase == 16)
            iExponentShiftPerDigit = 4;
        else if (iBase == 8)
            iExponentShiftPerDigit = 3;
        int iExponentAtLastSignificantDigit = (iExponentAbs * iExponentSign) - (iSignificantDigitsAfterDot * iExponentShiftPerDigit);
		emit_float_num_literal_token(pStart, iBase, iPositionOfFirstSignificantDigit,
            iSignificantDigitCountTotal, iExponentAtLastSignificantDigit, iPositionOfDot, iPositionOfExponentFirstDigit,
            iExponentSign > 0 ? 0u : 1u, iExponentAbs, bSpaceAfterwards);
    } else {
        Assert_(iSignificantDigitCountIntPart >= 0);
        Assert_(iSignificantDigitCountIntPart == iSignificantDigitCountTotal + iIntPartTrailingZeroes);
        Assert_(iSignificantDigitCountIntPart > iMaxAlreadyDecodedIntDigits || uAlreadyDecodedSmallInt <= 0x0100'0000uLL);
        if (iSignificantDigitCountIntPart > iMaxAlreadyDecodedIntDigits)
            uAlreadyDecodedSmallInt = 0xFFFF'FFFF'FFFF'FFFFuLL; // flagging an obvious out-of 24b range
		emit_natural_num_literal_token(pStart, iBase, iPositionOfFirstSignificantDigit, iSignificantDigitCountIntPart,
            uAlreadyDecodedSmallInt, bSpaceAfterwards);
    }

	return true;
}

// shall return 0xFFFFFFFF if error
static u32 get_single_code_point(const u8* pStartOfStringData, const u8* pPositionOfEndingDelim) {
    if (pPositionOfEndingDelim <= pStartOfStringData)
	    return 0xFFFFFFFFu;
    u8 c = *pStartOfStringData;
    if (c < 128) {
        if (c == u8('\\'))
            return 0xFFFFFFFFu;
        if (pPositionOfEndingDelim > pStartOfStringData + 1)
            return 0xFFFFFFFFu;
        return u32(c);
    } else {
        // TODO!!!
        return 0xFFFFFFFFu;
    }
}

bool LiteTokenizer::tokenize_string(u16* ioErr)
{
    i64 bIsStrictWithoutBackslashEscape = 0;
    i64 bHasMultiByteUtf8 = 0;
    i64 bHasBackslashEscapeSeq = 0;
	i64 bIsExpectedSingleCodePoint = 0;
	
	// eats potentially starting modifier
	const u8* pStart = _pCurrentChar;
	u8 c = *_pCurrentChar;
	if (c == u8('!')) {
		bIsStrictWithoutBackslashEscape = 1;
		_pCurrentChar++;
	} else if (c == u8('#')) {
		bIsExpectedSingleCodePoint = 1;
		_pCurrentChar++;
		c = *_pCurrentChar;
	}

	// starting delimiter is usually '"', but this code is agnostic to starting delim
	//		=> see logic on caller side for actually allowed string delimiters
    u8 cDelimitingChar = *_pCurrentChar;
    _pCurrentChar++; // eats starting delimiter
	
    const u8* pStartOfStringData = _pCurrentChar;
    const u8* pEnd = _pEnd;
    while (_pCurrentChar < pEnd) {
        c = *_pCurrentChar;
        if (c == cDelimitingChar) {
			u8 bSpaceAfterwards = TOKENKINDFLAG_SPACE_AFTERWARDS;
			if (_pCurrentChar + 1 < pEnd) {
				u8 cNext = _pCurrentChar[1];
				if (cNext != u8(' ') && cNext != u8('\t'))
					bSpaceAfterwards = 0;
			}
			if (bIsExpectedSingleCodePoint) {
				u32 uCodePoint = get_single_code_point(pStartOfStringData, _pCurrentChar);
				if (uCodePoint == 0xFFFFFFFFu) {
					*ioErr = TERR_STRING_IS_NOT_SINGLE_CODEPOINT;
					return false;
				} else
					emit_codepoint_literal_token(pStart, pStartOfStringData, _pCurrentChar, uCodePoint,
						bHasMultiByteUtf8, bHasBackslashEscapeSeq, bSpaceAfterwards);
			} else {
				emit_string_literal_token(pStart, pStartOfStringData, _pCurrentChar,
					bIsStrictWithoutBackslashEscape, bHasMultiByteUtf8, bHasBackslashEscapeSeq, bSpaceAfterwards);
			}
			_pCurrentChar++; // eats end of string delimiter
			return true;
        }
        if (0 == bIsStrictWithoutBackslashEscape && c == u8('\\')) {
            bHasBackslashEscapeSeq = 1;
            _pCurrentChar++;
            c = *_pCurrentChar;
        }

        if (c & 0x80)
            bHasMultiByteUtf8 = 1;

        _pCurrentChar++;
    }
	
	// If we get here, we've reached EOL before having closed the string... which is invalid
    *ioErr = TERR_UNCLOSED_STRING_LITERAL_BEFORE_EOL;
    return false;
}

bool LiteTokenizer::tokenize_identifier(u8 uStartsWithSymbolBefore, u16* ioErr)
{
    const u8* pStart = _pCurrentChar;
    const u8* pEnd = _pEnd;
    u8 c;
    if (uStartsWithSymbolBefore)
        _pCurrentChar++;
    while (_pCurrentChar < pEnd) {
        c = *_pCurrentChar;
        u8 cLowerIfAlpha = c | 32;
        if ((c >= u8('0') && c <= u8('9')) || c == u8('_') || (cLowerIfAlpha >= u8('a') && cLowerIfAlpha <= u8('z'))) {
			_pCurrentChar++;
        } else
            break;
    }
	
    u8 bSpaceAfterwards = TOKENKINDFLAG_SPACE_AFTERWARDS;
    if (_pCurrentChar < pEnd) {
        u8 cNext = *_pCurrentChar;
        if (cNext != u8(' ') && cNext != u8('\t'))
            bSpaceAfterwards = 0;
    }
	
    u16 uCharCount = u16(_pCurrentChar-pStart);
    StringView strIdentifier;
    strIdentifier.start = pStart;
    strIdentifier.uByteLength = u32(uCharCount);
    strIdentifier.flags = STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;

	auto itRes = g_allKeywords.find(strIdentifier);
	if (itRes != g_allKeywords.end()) {
		emit_keyword_token(uStartsWithSymbolBefore, uCharCount, itRes.value(), bSpaceAfterwards);
	} else {
		int iIndex = register_identifier_during_parsing(strIdentifier, _pSourceFile);
		emit_identifier_token(uStartsWithSymbolBefore, uCharCount, iIndex, bSpaceAfterwards);
	}
    return true;
}

exported_func_impl int tokenize_line(const u8* pCurrentLineStartWithoutIndent, const u8* pLineEnd, i32* ioCommentLevel,
                  Token* outTokenArray, SourceFileDescAndState* pSourceFile, u16* outError) {
    LiteTokenizer tokenizer(pCurrentLineStartWithoutIndent, pLineEnd, ioCommentLevel, outTokenArray, pSourceFile);
    return tokenizer.tokenize_skipping_comments(outError);
}

#endif // LOCLIB_SCAN_AND_TOK_H_
