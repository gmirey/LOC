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

#ifndef LOCLIB_PROGRAM_H_
#define LOCLIB_PROGRAM_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_Cmd_API.h"

#include "LocLib_ScanAndTok.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_ProgramState.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_DebugPrint.h"
#include "LocLib_PreParser.h"
#include "LocLib_PostParserTypes.h"
#include "LocLib_Postparser.h"
#include "LocLib_TypeChecker.h"
#include "LocLib_ir_dump.h"

#include "LocLib_PE.h"
#include "LocLib_x64.h"
#include "LocLib_winx64_backend.h"

// TODO : remove dependency to crt
#include <cstdio>
#include <cstdlib>

//
// TODO: we'll probably need to revise our scan-like-and-tok strategy to iron out the weirdness of current parsing wrt multiline and blocks...
// => Maybe when parsing a new line with actual tokens, shall we lookahead for the *next* line with actual tokens, and decide *purely* based
// on indentation, whether it is a multiline continuation, or a child block, or a sibling statement. Currently, the decision system about
// multiline is like an hybrid between indentation based, and previous-line-contents-based... which does not "feel" that great in the end.
// It would be simpler to have a purely indentation based system.
// Also, we can gather all indentation info in the scanner => we could thus very easily "mock" a correct indentation in the IDE to try out
//   indentation 'corrections'-proposals, when we're on an IDE-provided parsing configuration.
//

// GetNextLineTokens_Sign
exported_func_impl int get_next_line_tokens_when_reading_full_file(
    StatementParserState* ioParserState,
    SourceFileDescAndState* pSourceFile,
    LocLib_OS_WrapperFunctions* pOsFuncs,
    TokenizerClosureBaseData* pClosureData, u16* outError)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Get Next Line Tokens"), ioParserState->pWorker);

    GetNextLineTokens_WhenReadingFullFile_ClosureData* pFullFileClosureData =
        reinterpret_cast<GetNextLineTokens_WhenReadingFullFile_ClosureData*>(pClosureData);
    const u8* pNextLineCharScan = pFullFileClosureData->pNextLineCharScan;
    const u8* pAfterLastCharInFile = pFullFileClosureData->pAfterLastCharInFile;
    i32 iLineAtStart = pFullFileClosureData->_baseData.iLineIndex;
    
    int iCommentLevel = 0;
    Token* pLineTokens = ioParserState->pLineTokens;
    const u8* pCurrentCharScan = pNextLineCharScan;
    i32 iConsumedLines = 0;
    while (pCurrentCharScan < pAfterLastCharInFile) {
        u16 uCurrentLineIndent, uCurrentLineTabIndents, uCharCountWithoutIndentOrEOL, uEOL;
        pNextLineCharScan = text_scan_next_line(pCurrentCharScan, pAfterLastCharInFile,
            &uCurrentLineIndent, &uCurrentLineTabIndents, &uCharCountWithoutIndentOrEOL, &uEOL, outError);
        const u8* pCurrentLineStartWithoutIndent = pCurrentCharScan + uCurrentLineIndent;
        u16 uCurrentLineContentsSize = u16(pNextLineCharScan - pCurrentLineStartWithoutIndent) - uEOL;
        i64 iCheckNotTooManyLines = i64(iLineAtStart) + i64(iConsumedLines) + 1LL;
        if (!*outError) {
            iConsumedLines++;
            i64 iCheckNotTooManyLines = i64(iLineAtStart) + i64(iConsumedLines);
            if (iCheckNotTooManyLines > 0 && (iCheckNotTooManyLines & 0xFFFFFFFF80000000uLL) != 0)
                *outError = SERR_TOO_MANY_LINES_IN_SINGLE_FILE;
        }
        if (!*outError) {
            LiteTokenizer tokenizer(pCurrentLineStartWithoutIndent,
                                    pCurrentLineStartWithoutIndent + uCurrentLineContentsSize,
                                    &iCommentLevel, pLineTokens, pSourceFile);
            int iNonCommentTokenCount = tokenizer.tokenize_skipping_comments(outError);

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Line %5d Was successfully scanned: Indent %u (%u Starting Tabs)",
                u64(u32(iLineAtStart+iConsumedLines+1)), u64(uCurrentLineIndent), u64(uCurrentLineTabIndents)), ioParserState->pWorker);

            /*
            char szLine[LINE_BUFFERSIZE];
            snprintf(szLine, uCurrentLineContentsSize+1, "%s", pCurrentLineStartWithoutIndent);
            char szTmp[2048];
            sprintf(szTmp, "%5d: %d(%d) [%s] : %s",
                iLineAtStart+iConsumedLines+1,
                uCurrentLineIndent,
                uCurrentLineTabIndents,
                uEOL ? (uEOL == 1 ? "LF" : "CRLF") : "",
                szLine);
            platform_log_info(szTmp, true);
		    sprintf(szTmp, "Tokens:%d : ", iNonCommentTokenCount);
		    platform_log_info(szTmp, false);
		    for (int iToken = 0; iToken < iNonCommentTokenCount; iToken++) {
			    debug_print_token(pLineTokens + iToken, pCurrentLineStartWithoutIndent, pSourceFile);
		    }
            platform_log_info("", true);
            */

            if (!*outError) {
                if (iNonCommentTokenCount) {

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Line %5d Was successfully tokenized with %u Tokens",
                        u64(u32(iLineAtStart+iConsumedLines+1)), u64(u32(iNonCommentTokenCount))), ioParserState->pWorker);

                    if (uCurrentLineIndent < 256 && uCurrentLineTabIndents < 256)
                    {
                        ioParserState->iCurrentlyParsedLine = iLineAtStart + iConsumedLines;
                        ioParserState->uCurrentLineTabIndents = u8(uCurrentLineTabIndents);
                        ioParserState->uCurrentLineIndent = u8(uCurrentLineIndent);
                        ioParserState->uRefStartOfError = 0;
                        ioParserState->uCurrentBytesOnLine = uCharCountWithoutIndentOrEOL;
                        ioParserState->pStartOfLineAfterIndent = pCurrentLineStartWithoutIndent;
                        ioParserState->pCurrentToken = pLineTokens;
                        ioParserState->pLineTokenEnd = pLineTokens + iNonCommentTokenCount;
                        pFullFileClosureData->pNextLineCharScan = pNextLineCharScan;
                        pFullFileClosureData->_baseData.iLineIndex = iLineAtStart + iConsumedLines;
                        pFullFileClosureData->_baseData.pStartOfCurrentLine = pCurrentCharScan;
                        return iNonCommentTokenCount;
                    }
                    else
                    {
                        *outError = TERR_TOO_MANY_INDENTATION_CHARACTERS;
                        ioParserState->iCurrentlyParsedLine = iLineAtStart + iConsumedLines;
                        ioParserState->uCurrentLineTabIndents = 0;
                        ioParserState->uCurrentLineIndent = 0;
                        ioParserState->uRefStartOfError = 0;
                        ioParserState->uCurrentBytesOnLine = uCharCountWithoutIndentOrEOL;
                        ioParserState->pStartOfLineAfterIndent = pCurrentLineStartWithoutIndent;
                        ioParserState->pCurrentToken = pLineTokens;
                        ioParserState->pLineTokenEnd = pLineTokens;
                        pFullFileClosureData->pNextLineCharScan = pNextLineCharScan;
                        pFullFileClosureData->_baseData.iLineIndex = iLineAtStart + iConsumedLines;
                        pFullFileClosureData->_baseData.pStartOfCurrentLine = pCurrentCharScan;
                        return -3;
                    }
                } else {
                    pCurrentCharScan = pNextLineCharScan;
                }
            } else {
                ioParserState->iCurrentlyParsedLine = iLineAtStart + iConsumedLines;
                ioParserState->uCurrentLineTabIndents = u8(uCurrentLineTabIndents);
                ioParserState->uCurrentLineIndent = u8(uCurrentLineIndent);
                ioParserState->uRefStartOfError = 0;
                ioParserState->uCurrentBytesOnLine = uCharCountWithoutIndentOrEOL;
                ioParserState->pStartOfLineAfterIndent = pCurrentLineStartWithoutIndent;
                ioParserState->pCurrentToken = pLineTokens;
                ioParserState->pLineTokenEnd = pLineTokens + iNonCommentTokenCount;
                pFullFileClosureData->pNextLineCharScan = pNextLineCharScan;
                pFullFileClosureData->_baseData.iLineIndex = iLineAtStart + iConsumedLines;
                pFullFileClosureData->_baseData.pStartOfCurrentLine = pCurrentCharScan;
                return -1;
            }
        } else {
            ioParserState->iCurrentlyParsedLine = iLineAtStart + iConsumedLines;
            ioParserState->uCurrentLineTabIndents = u8(uCurrentLineTabIndents);
            ioParserState->uCurrentLineIndent = u8(uCurrentLineIndent);
            ioParserState->uRefStartOfError = 0;
            ioParserState->uCurrentBytesOnLine = uCharCountWithoutIndentOrEOL;
            ioParserState->pStartOfLineAfterIndent = pCurrentLineStartWithoutIndent;
            ioParserState->pCurrentToken = pLineTokens;
            ioParserState->pLineTokenEnd = pLineTokens;
            pFullFileClosureData->pNextLineCharScan = pNextLineCharScan;
            pFullFileClosureData->_baseData.iLineIndex = iLineAtStart + iConsumedLines;
            pFullFileClosureData->_baseData.pStartOfCurrentLine = pCurrentCharScan;
            return -2;
        }
    }

    ioParserState->iCurrentlyParsedLine = iLineAtStart + iConsumedLines + 1;
    ioParserState->uCurrentLineTabIndents = 0;
    ioParserState->uCurrentLineIndent = 0;
    ioParserState->uRefStartOfError = 0;
    ioParserState->uCurrentBytesOnLine = 0;
    ioParserState->pStartOfLineAfterIndent = pNextLineCharScan;
    ioParserState->pCurrentToken = pLineTokens;
    ioParserState->pLineTokenEnd = pLineTokens;
    pFullFileClosureData->pNextLineCharScan = pNextLineCharScan;
    pFullFileClosureData->_baseData.iLineIndex = iLineAtStart + iConsumedLines;
    pFullFileClosureData->_baseData.pStartOfCurrentLine = pCurrentCharScan;
    return 0;
}

exported_func_impl const u8* check_for_BOM(const u8* pFileContents, u64 uFileByteSize, LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError)
{
	u8 firstByte = *pFileContents;
	if (firstByte == 0xEF) { // possibly UTF8 BOM
		if (uFileByteSize >= 3) {
			if (pFileContents[1] == 0xBB && pFileContents[2] == 0xBF) { // ... yep
				pFileContents += 3; // eats UTF8 BOM silently
                /*
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
                char szTmp[255];
                sprintf(szTmp, "UTF8 BOM found in file - actual payload without BOM: %llu bytes", uFileByteSize - 3u);
                platform_log_info(szTmp, true);
#endif
                */
				return pFileContents;
			}
		}
	} else if (firstByte == 0xFF) { // possibly UTF16 BE BOM (or maybe UTF32 BE)
		if (uFileByteSize >= 2) {
			if (pFileContents[1] == 0xFE) { // ... yep
                if (uFileByteSize >= 4 && pFileContents[2] == 0x00 && pFileContents[3] == 0x00) {
				    *outError = SERR_UNALLOWED_BOM_UTF32BE;
                } else {
				    *outError = SERR_UNALLOWED_BOM_UTF16BE;
                }
			}
		}
	} else if (firstByte == 0xFE) { // possibly UTF16 LE BOM
		if (uFileByteSize >= 2) {
			if (pFileContents[1] == 0xFF) { // ... yep
				*outError = SERR_UNALLOWED_BOM_UTF16;
			}
		}
	} else if (firstByte == 0x00) { // possibly UTF32 LE BOM
		if (uFileByteSize >= 4) {
			if (pFileContents[1] == 0x00 &&
				pFileContents[2] == 0xFE && pFileContents[3] == 0xFF) { // ... yep
				*outError = SERR_UNALLOWED_BOM_UTF32;
			}
		}
	}
    if (*outError) {
        platform_log_info("*** invalid BOM found in file", true);
        return 0;
    }

    /*
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    char szTmp[255];
    sprintf(szTmp, "no BOM recognized in file - payload %llu bytes", uFileByteSize);
    platform_log_info(szTmp, true);
#endif
    */
    return pFileContents;
}

#define parser_report_indent_error(uErrCode) do { \
    LocLib_Error indentError; \
    indentError.errCode = uErrCode; \
    indentError.uBlockOrLineIfScanErr = pCurrentBlock->uIndexInBlockVec; \
    indentError.uStatement = pCurrentBlock->vecStatements.size(); \
    int iLineFromThere = parserParams.parserState.iCurrentlyParsedLine - iLastParsedLine; \
    indentError.uTokenRef = u32(u32(parserParams.parserState.uCurrentLineIndent) | (u32(iLineFromThere) << 10)); \
    pSourceFile->vecErrors.append(indentError); \
    bErrorAlreadyReported = true; \
} while (0)

local_func bool make_ast_for_source_file(int iSourceFileIndex,
                              TokenizerClosure tokenizer,
                              WholeProgramCompilationState* pProgramCompState,
                              LocLib_OS_WrapperFunctions* pOsFuncs,
                              LocLib_CompilationParams* pCompilationParams,
                              LocLib_CompilationResults* oCompilationResults,
                              WorkerDesc* pWorker)
{
    Assert_(iSourceFileIndex >= 0 && iSourceFileIndex < (int)pProgramCompState->vecSourceFiles.size());
    SourceFileDescAndState* pSourceFile = pProgramCompState->vecSourceFiles[iSourceFileIndex];
    pSourceFile->uIdentifierNamesOffset = pProgramCompState->vecAllIdentifierNamesById.size();
    Assert_(pSourceFile->locallyFoundIdentifierNamesByOffsetId.size() == 0);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Making AST for source-file '%s'", reinterpret_cast<u64>(pSourceFile->sourceFileName.c_str())), pWorker);

    GetNextLineTokens_Sign pGetNextLineTokensFn = tokenizer.pGetNextLineTokensFn;
    TokenizerClosureBaseData* pTokenizerData = tokenizer.pClosureData;

    Token tTokens[MAX_TOKENS_ON_LINE];
    ParserParams parserParams; 
    parserParams.parserState.pLineTokens = tTokens;
    parserParams.parserState.pCurrentToken = tTokens;
    parserParams.parserState.pLineTokenEnd = tTokens;
    parserParams.parserState.pStartOfLineAfterIndent = 0;
    parserParams.parserState.pWorker = pWorker;
    parserParams.parserState.uCurrentBlockTabIndents = 0;
    parserParams.parserState.uCurrentLineIndent = 0;
    parserParams.parserState.uCurrentLineTabIndents = 0;
    parserParams.parserState.iCurrentlyParsedLine = pTokenizerData->iLineIndex; // TODO ?
    parserParams.parserState.iInlinedRootStartLine = -1;
    parserParams.parserState.iLineOfLastRegisteredToken = -1;
    parserParams.parserState.uIndexOfLastRegisteredToken = 0;
    parserParams.parserState.inFlightModifierNodes = 0;
    parserParams.parserState.uCountModifierNodes = 0;
    parserParams.parserState.uCountParamNodesWithinModifiers = 0;
    parserParams.parserState.uRefStartOfError = 0;
    
    // we'll use sourcefile-wise local arena as scratchmem for preparsing. It will be reset to empty in the nominal case before being userd by typechecker,
    //   but it may also be "not-totally emptied" to retain PreAstNodes in error, until error-reporting.
    parserParams.preparsingArena = pSourceFile->localArena;
    parserParams.pSourceFile = pSourceFile;
    
    Arena blockParsingArena;
    init_arena(&blockParsingArena, pProgramCompState->parseOnlyProvider);
    Arena blockVectorArena;
    init_arena(&blockVectorArena, pProgramCompState->parseOnlyProvider);

    Arena arenaForFinalAST = pSourceFile->parseOnlyArena;

    parserParams.tokenizer = tokenizer;
    parserParams.pOsFuncs = pOsFuncs;
    parserParams.pProgramCompState = pProgramCompState;
    parserParams.pCompilationParams = pCompilationParams;
    parserParams.oCompilationResults = oCompilationResults;

    BlockParsingState tBlocks[OPENED_BLOCK_MAX_OVERSIZE];
    BlockParsingState* pCurrentBlock = tBlocks;

    TmpStackOptiArray<AstBlock*, 256u> vecFinalBlocks;
    vecFinalBlocks._alloc = FireAndForgetArenaAlloc(blockVectorArena);
    vecFinalBlocks.append(0); // We'll assign ptrs to astblocks in already reserved slots for encountered blocks. And we're currently ready for first block.

    _init_block_parsing_state(pCurrentBlock, 0, 0, 0, pTokenizerData->iLineIndex+1, // +1 since first line of block 0 will be 'next'
        pTokenizerData->pStartOfCurrentLine, blockParsingArena);
    EBlockSpawningState eBlockSpawningState = EBLOCK_SPAWNING_NONE;

    u16 uError = 0;
    u16 uPreviousLineTokenError = 0;
    ArenaRefPoint refScratchMemKeepingErrors = get_arena_ref_point(parserParams.preparsingArena);
    bool bErrorAlreadyReported = false;
    int iLastParsedLine = pTokenizerData->iLineIndex;
    
    do {
        int iRet = pGetNextLineTokensFn(
                    &parserParams.parserState, pSourceFile, pOsFuncs, pTokenizerData, &uError);
        
        // Error-handling of scan&tok result
        //
        if (iRet > 0) { // Nominal and common case
            // NOOP here
        } else {
            if (iRet == 0) {    // nominally reached end of file
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Reached EOF successfully"), pWorker);
                break;

            } else { // iRet < 0 => scan&tok error
                Assert_(uError); // scan&tok should not have returned a negative value without setting up an error code.
                Assert_(uError < BASE_OF_PARSER_ERRORS);     // scan&tok should only report scanner or tokenizer errors.
                if (uError < BASE_OF_TOKENIZER_ERRORS) {    // tokenization errors are usually recoverable
                    // ...but this one is a more severe scanner error
                    // => report that error right here.
                    LocLib_Error scannerError = {};
                    scannerError.errCode = uError;
                    // TODO: instead of '_min', ensure pTokenizerData->iLineIndex actually holds correct line (=> necessarily first = 0)
                    scannerError.uBlockOrLineIfScanErr = u32(_min(0, pTokenizerData->iLineIndex)); 
                    pSourceFile->vecErrors.append(scannerError);
                    break; // and stop trying parsing that file altogether  
                } else {
                    // NOOP => will simply handle that error as a report later, but do not end iterating on other lines
                    eBlockSpawningState = EBlockSpawningState::EBLOCK_SPAWNING_NONE;
                }
            }
        }
        
        //
        // Handling block-starting and block-ending concerns which can be decided purely on indentation (even in case of tokenizer error)
        //

        // Keeping track of indentation info for that line (even in case of tokenizer error)
        u8 uTabIndentAtStartOfMultiline = parserParams.parserState.uCurrentLineTabIndents;
        u8 uAllIndentsAtStartOfMultiline = parserParams.parserState.uCurrentLineIndent;
        const u8* pActualByteAtStartOfMultiline = parserParams.parserState.pStartOfLineAfterIndent - uAllIndentsAtStartOfMultiline;

        bool bBlockJustClosedByIndent = false;

        // Nominal-case : same indentation as previously
        if (uTabIndentAtStartOfMultiline == parserParams.parserState.uCurrentBlockTabIndents) {
            if (eBlockSpawningState != EBLOCK_SPAWNING_EXPECTED) {
                // nominal case NOOP
            } else {
                debug_print_preparse_err("*** Found next line at same indent than current block when expecting child block");
                Assert_(pCurrentBlock->vecStatements.size());
                pCurrentBlock->vecStatements.last().uFlags |= STATEMENT_MISSES_CHILD_BLOCK;
                pCurrentBlock->uPreParsingFlags |= BLOCK_HAS_STATEMENTS_WITH_MISSING_CHILD;
                parser_report_indent_error(PERR_EXPECTED_CHILD_BLOCK_MISSING);
                uError = PERR_EXPECTED_CHILD_BLOCK_MISSING;
                eBlockSpawningState = EBLOCK_SPAWNING_NONE;
            }
        }
        // Opens new block in case of indentation increase from previous statement (even in case of tokenizer error)
        // Note: checks for *all* indent increases, including larger that +1 tab, and including forbidden non-tab (will be error cases...)
        else if (uAllIndentsAtStartOfMultiline > parserParams.parserState.uCurrentBlockTabIndents) {
            // ... but only setup parser indentation for handling valid tab indents
            u32 uPreviousBlockTabIndents = parserParams.parserState.uCurrentBlockTabIndents;
            parserParams.parserState.uCurrentBlockTabIndents = uTabIndentAtStartOfMultiline;
            // opens block, all fine by default
            pCurrentBlock = _open_child_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, blockParsingArena);

            if (uAllIndentsAtStartOfMultiline == uPreviousBlockTabIndents + 1 &&
                uAllIndentsAtStartOfMultiline == uTabIndentAtStartOfMultiline) {
                // nominal case NOOP
            } else {
                debug_print_preparse_err("*** Found statement at an invalid or multi-line-indentation level relative to current block");
                pCurrentBlock->uPreParsingFlags |= BLOCK_IS_IN_INDENTATION_ERROR;       // error case
                parser_report_indent_error(PERR_INVALID_MULTILINE_INDENTATION_NORECOVERY);
                uError = PERR_INVALID_MULTILINE_INDENTATION_NORECOVERY;
            }

            if (eBlockSpawningState != EBLOCK_SPAWNING_NONE) { //... good news: we expected that child block to be opened indeed
                // nominal case NOOP
            } else {
                if (uError == 0u) { // if no tokenizer error...
                    debug_print_preparse_err("*** Found statement at a further level relative to current block, when no child block was expected");
                    pCurrentBlock->uPreParsingFlags |= BLOCK_IS_UNEXPECTED_CHILD;           // error case
                    parser_report_indent_error(PERR_INVALID_SUBBLOCK_INDENTATION_NORECOVERY);
                    uError = PERR_INVALID_SUBBLOCK_INDENTATION_NORECOVERY;
                }
            }
            // block spawning has actually been handled by now => reset to none
            eBlockSpawningState = EBLOCK_SPAWNING_NONE;
        }
        // ... And closes blocks in case of indentation decrease (also even in case of tokenizer error)
        else {
            // ... until having reached the indentation of a parent block
            while (uTabIndentAtStartOfMultiline < parserParams.parserState.uCurrentBlockTabIndents) {
                Assert_(pCurrentBlock > tBlocks); // should be algorithmically sound, not to go past root block by means of indentation
                pCurrentBlock = _convert_and_close_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, eBlockSpawningState, arenaForFinalAST);
                // Note: the above call also handles setting parser block intent to that known on parent block
                // if there were dangling block spawning expectations, they actually have been handled by now => reset to none
                eBlockSpawningState = EBLOCK_SPAWNING_NONE;
            }
            // Also handle the hairy case of having a wrong-indent sub-block closing to a still-child indent level from parent block...
            if (uAllIndentsAtStartOfMultiline == parserParams.parserState.uCurrentBlockTabIndents) {
                // nominal case is NOOP
            } else { // uAllIndentsAtStartOfMultiline > parserParams.parserState.uCurrentBlockTabIndents... told you this was hairy
                // ...only setup parser indentation for handling valid tab indents
                u32 uPreviousBlockTabIndents = parserParams.parserState.uCurrentBlockTabIndents;
                parserParams.parserState.uCurrentBlockTabIndents = uTabIndentAtStartOfMultiline;
                debug_print_preparse_err("*** Recovering from a block indentation error with another block possibly in error");
                parser_report_indent_error(PERR_INVALID_SUBBLOCK_INDENTATION_NORECOVERY);
                uError = PERR_INVALID_SUBBLOCK_INDENTATION_NORECOVERY;
                // Warning: those child blocks in case of such an error may end up having same parent
                pCurrentBlock = _open_child_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, blockParsingArena);
                pCurrentBlock->uPreParsingFlags |= BLOCK_IS_UNEXPECTED_CHILD;               // ensured error case by now
                if (uAllIndentsAtStartOfMultiline > uPreviousBlockTabIndents + 1 || uAllIndentsAtStartOfMultiline != uTabIndentAtStartOfMultiline) {
                    debug_print_preparse_err("*** ... furthermore at an invalid or multi-line-indentation level relative to current block");                    
                    pCurrentBlock->uPreParsingFlags |= BLOCK_IS_IN_INDENTATION_ERROR;       // further error case
                }
                // each iteration of the loop before should have set block spawning state to none, and current block does not change it
                Assert_(eBlockSpawningState == EBLOCK_SPAWNING_NONE);
            }
            bBlockJustClosedByIndent = true;
        }

        //
        // Pre-parsing
        //

        PreStatement* pPreStatement;
        if (!uError) { // no scanner or tokenizer error => actually pre-parse that line.
            
            iLastParsedLine = pTokenizerData->iLineIndex;

            // nominal case : no modifiers "in flight" from previous modifier-only lines
            if (0 == parserParams.parserState.inFlightModifierNodes) {
                parserParams.parserState.iInlinedRootStartLine = pTokenizerData->iLineIndex;
            } // otherwise do not reset uInlinedRootRelativeLine, as modifiers on previous lines count within same statement.

            // Actually pre-parse that statement (along with all inlined statements)
            pPreStatement = parse_pre_statement(parserParams, 0, &uError);
            Assert_(pPreStatement); // parse_statement should always return an instance of PreStatement, even in case of error
            if (!uError) {
                // nominal case NOOP
            } else {
                pPreStatement->uRefStartOfError = parserParams.parserState.uRefStartOfError;
            }
        } else {       // tokenizer error => mock that line as a dummy statement in error
            pPreStatement = _init_pre_statement(parserParams);
        }

        //
        // Handling block-starting and block-ending concerns related to pan directives, for which identation increase is optional
        //

        // if previous line was a pan-directive starting block, handle block-opening in those case where child was opened at same-indentation
        // if line is a pan-directive ending block, handle block closing in those case where:
        //      - child was opened at same-indentation, or
        //      - there was no statement as child block
        // 
        // Note: 'eBlockSpawningState' at this point is *still* dependent from the *previous* parsed statement, not the one we just did.
        // Moreover: If was EBLOCK_SPAWNING_NONE : nothing happened between previously parsed statement, and current preparsed.
        //           If was EBLOCK_SPAWNING_EXPECTED (OR EBLOCK_SPAWNING_PAN_DIRECTIVE) and we detected an indentation increase:
        //              the block was already spawned, and we reset eBlockSpawningState to EBLOCK_SPAWNING_NONE already
        // => only distinct possibility here is 'EBLOCK_SPAWNING_PAN_DIRECTIVE' where there was *no* subsequent indentation increase !
        if (eBlockSpawningState != EBLOCK_SPAWNING_PAN_DIRECTIVE) {
            // most common case: previous statement (at same indentation) is not a block-spawning pan-directive
            Assert_(eBlockSpawningState == EBLOCK_SPAWNING_NONE);
            // And if the statement just-parsed is *not* pan-closing either...
            if (0 == (pPreStatement->uStatementFlags & ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE)) {
                // then we're in nominal case: nothing to do in here relative to pan statements :)
                // NOOP
            } else { // the just-parsed statement is pan-closing (and previous one was not block-opening)
                // most common case when encountering a block-ending pan directive, is we actually had some statements inbetween
                // furthermore, if we got to that case, means we did not close that in-between block by indent-only
                if (pCurrentBlock->uPreParsingFlags & BLOCK_WAS_SPAWNED_AT_SAME_INDENT_AS_PARENT) {
                    // nominal case, indeed => close that block.
                    pCurrentBlock = _convert_and_close_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams,
                        EBLOCK_SPAWNING_NONE, arenaForFinalAST, true);
                } else if (bBlockJustClosedByIndent) {
                    // also okay: we had a child block, and we closed it the regular way...
                    // NOOP there
                } else { // some indent-or-pan-directive-matching error here...
                    debug_print_preparse_err("*** Found block-ending pan-directive at an invalid or multi-line-indentation level relative to current block");
                    // => trying to match it with a pan directive in parent block, for recovery... ?
                    // TODO !!!!
                    // In the meantime (and otherwise), let that fall-through to post-processing as-is, where
                    //   it is expected that statement-pair-matching-checks will identify an error anyway.
                }
            }

        } else {
            // previous statement at same indentation was a pan-opener
            // moreover, just-parsed statement *is* not a block-ending
            if (0 == (pPreStatement->uStatementFlags & ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE)) {
                // block spawning before, not block ending current, and not having spawned child block yet
                // => do so now, flagging it as opened at same indent.
                pCurrentBlock = _open_child_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, blockParsingArena);
                pCurrentBlock->uPreParsingFlags |= BLOCK_WAS_SPAWNED_AT_SAME_INDENT_AS_PARENT;
            } else { // current statement is block-ending, and previous one seems to be block-opening,
                // => flag the previous (hopefully pan-directive-opening) statement as (validly) being with empty-child.
                Assert_(pCurrentBlock->vecStatements.size());
                pCurrentBlock->vecStatements.last().uFlags |=
                    STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD;
            }
        }

        //
        // Post-parsing
        //

#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        PreStatement* pCurrentStatement = pPreStatement;
        do {
            debug_print_pre_statement(pCurrentStatement, pSourceFile);
            pCurrentStatement = pCurrentStatement->pLhsStatementWhenInlined;
            if (pCurrentStatement) {
                platform_log_info("## Chained After: ##", true);
            }
        } while (pCurrentStatement);
        platform_log_info("", true);
#endif

        if (!uError) {

            if (pPreStatement->uStatementKind != ESTATEMENT_MODIFIER_ONLY_LINE) {
                // nominal case...
                if (0 == pPreStatement->pLhsStatementWhenInlined) {
                    // most common case: single statement on this line (even if multiline)
                    _convert_and_push_pre_statement_to_current_block(pCurrentBlock, pPreStatement, &vecFinalBlocks,
                        pSourceFile, pOsFuncs, arenaForFinalAST, &uError);
                    eBlockSpawningState = EBlockSpawningState(pPreStatement->uExpectedNextBlockSpawning);
                    if (uError) {
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                        debug_print_preparse_err("*** failed-conversion of pre-parser results to actual statement and nodes");
#endif
                        bErrorAlreadyReported = true;
                    }
                } else {
                    // several statements chained or child-inlined: build vector of them, then iterate through them in reverse.
                    TmpStackOptiArray<PreStatement*, 128u> vecStatementsLastToFirst;
                    vecStatementsLastToFirst._alloc = FireAndForgetArenaAlloc(parserParams.preparsingArena);
                    PreStatement* pIterBackwards = pPreStatement;
                    while (pIterBackwards) {
                        vecStatementsLastToFirst.append(pIterBackwards);
                        pIterBackwards = pIterBackwards->pLhsStatementWhenInlined;
                    }
                    i32 iCount = i32(vecStatementsLastToFirst.size());
                    u32 uCountSubBlocks = 0;
                    BlockParsingState* pCurrentBlockBefore = pCurrentBlock;
                    for (i32 iInVec = iCount - 1; iInVec >= 0; iInVec--) {
                        PreStatement* pStatement = vecStatementsLastToFirst[iInVec];
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                            "Parser-Management: Pre-parsed inlined statement %d/%d conversion to final AST (%s)",
                            u64(i32(iCount-iInVec)), u64(i32(iCount)),
                            reinterpret_cast<u64>(iInVec ? (pStatement->uExpectedNextBlockSpawning == EBLOCK_SPAWNING_EXPECTED ? "before opening child" : "before chained") : "last")), pWorker);

                        switch (pStatement->uStatementKind) {
                        case ESTATEMENT_PAN_IF: case ESTATEMENT_PAN_ELIF: case ESTATEMENT_PAN_ELSE:
                        case ESTATEMENT_PAN_SCOPE: case ESTATEMENT_PAN_ENDSCOPE: case ESTATEMENT_PAN_ENDIF:
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                            platform_log_info("*** this #-directive cannnot to be among several inlined statement", true);
#endif
                            uError = PERR_FORBIDDEN_INLINING_OF_PAN_DIRECTIVE;
                            pPreStatement->uRefStartOfError = (u32(pStatement->iStartLine - parserParams.parserState.iInlinedRootStartLine) << 10) |
                                u32(pStatement->uByteCountOnLineToFirst);
                            _convert_and_push_pre_statement_to_current_block(pCurrentBlock, pPreStatement, &vecFinalBlocks,
                                pSourceFile, pOsFuncs, arenaForFinalAST, &uError);
                            bErrorAlreadyReported = true;
                            break;
                        default:
                            _convert_and_push_pre_statement_to_current_block(pCurrentBlock, pStatement, &vecFinalBlocks,
                                pSourceFile, pOsFuncs, arenaForFinalAST, &uError);
                            if (uError) {
                                bErrorAlreadyReported = true;
                                break;
                            }
                            if (pStatement->uExpectedNextBlockSpawning == EBLOCK_SPAWNING_EXPECTED) {
                                if (iInVec) {
                                    pCurrentBlock = _open_child_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, blockParsingArena);
                                    pCurrentBlock->uPreParsingFlags |= BLOCK_WAS_SPANWED_AS_INLINED;
                                    uCountSubBlocks++;
                                } else {
                                    // TODO: error ??
                                    int iCoucou = 1;
                                }
                            } else if (pStatement->uExpectedNextBlockSpawning == EBLOCK_SPAWNING_NONE) {
                                // NOOP
                            } else {
                                Assert_(false); // well, in fact all those should have been caught by previous error about #-directives.
                            }
                        }
                    }
                    if (uCountSubBlocks && pPreStatement->uExpectedNextBlockSpawning != EBLOCK_SPAWNING_EXPECTED) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                            "Parser-Management: Now closing one opened child after inlines"), pWorker);
                        pCurrentBlock = _convert_and_close_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, EBLOCK_SPAWNING_NONE, arenaForFinalAST);
                        eBlockSpawningState = EBLOCK_SPAWNING_NONE;
                        Assert_(pCurrentBlock == pCurrentBlockBefore);
                    }
                    else
                        eBlockSpawningState = EBlockSpawningState(pPreStatement->uExpectedNextBlockSpawning);

                    // all of the above should not have modified current tab indents...
                    Assert_(parserParams.parserState.uCurrentBlockTabIndents == uTabIndentAtStartOfMultiline);
                }
            } else {
                // modifier-only-line : we put the modifier list back into parser state "in flight", for being handled on next statement 
                Assert_(0 == pPreStatement->pLhsStatementWhenInlined); // inlined statements should never produce modifier only without error
                Assert_(pPreStatement->firstPreStatementModifier);
                Assert_(pPreStatement->uCountModifierNodes);
                parserParams.parserState.inFlightModifierNodes = pPreStatement->firstPreStatementModifier;
                parserParams.parserState.uCountModifierNodes = pPreStatement->uCountModifierNodes;
                parserParams.parserState.uCountParamNodesWithinModifiers = pPreStatement->uCountParamNodesWithinModifiers;
                eBlockSpawningState = EBlockSpawningState(pPreStatement->uExpectedNextBlockSpawning);
            }

        } else {
            // convert, even in case of error...
            _convert_and_push_pre_statement_to_current_block(pCurrentBlock, pPreStatement, &vecFinalBlocks,
                pSourceFile, pOsFuncs, arenaForFinalAST, &uError);
            eBlockSpawningState = EBlockSpawningState(pPreStatement->uExpectedNextBlockSpawning);
            bErrorAlreadyReported = true;
        }

        // prepares iteration to next line, including cleanup
        //
        if (0 == uError) {
            // nominal case: actually scratch that temporary memory
            reset_arena_to(refScratchMemKeepingErrors, parserParams.preparsingArena);
        } else {
            // sets the tail of current temporary memory as a restoration-point, to keep all those for err-report.
            refScratchMemKeepingErrors = get_arena_ref_point(parserParams.preparsingArena);
            // keep that error on the source-file info for access by end-user
            //
            Assert_(bErrorAlreadyReported);
            bErrorAlreadyReported = false;
            // resets uError for next iteration
            uError = 0;
            parserParams.parserState.uRefStartOfError = 0;
            eBlockSpawningState = EBlockSpawningState::EBLOCK_SPAWNING_NONE;
        }

    } while(true); // we'll only break in case of EOF, or severe scanner error

    bool bResult;
    if (uError == 0) { // a non-zero value for current 'uError' leaking though here would specifically mean an (irrecoverable) scanner error.
        if (pCurrentBlock > tBlocks) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
                "Parser-Management: Closing all opened blocks until root after having reached EOF"), pWorker);
        }

        while (pCurrentBlock > tBlocks) {
            pCurrentBlock = _convert_and_close_block(tBlocks, &vecFinalBlocks, pCurrentBlock, parserParams, eBlockSpawningState, arenaForFinalAST);
            eBlockSpawningState = EBLOCK_SPAWNING_NONE;
        }

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Parser-Management: Now converting root block to final AST"), pWorker);

        if (tBlocks->vecStatements.size() || (tBlocks->uPreParsingFlags & BLOCK_HAS_UNEXPECTED_CHILDREN)) {
            AstBlock* pRootBlock = _convert_to_ast_block(tBlocks, tBlocks, blockParsingArena, arenaForFinalAST);
            Assert_(tBlocks->uIndexInBlockVec == 0);
            vecFinalBlocks[0] = pRootBlock;
            Assert_(vecFinalBlocks.size() < 0x40000000u);
            u32 uBlockCount = vecFinalBlocks.size();
            pSourceFile->iBlockCount = i32(uBlockCount);
            AstBlock* tFinalBlocks = reinterpret_cast<AstBlock*>(alloc_from(arenaForFinalAST,
                uBlockCount * sizeof(AstBlock), alignof(AstBlock)));
            for (u32 uBlock = 0; uBlock < uBlockCount; uBlock++) {
                tFinalBlocks[uBlock] = *(vecFinalBlocks[uBlock]);
            }
            pSourceFile->tBlocks = tFinalBlocks;
        } else { // No actual statement found in this file
            Assert_(tBlocks->vecChildBlockIndices.size() == 0);
            pSourceFile->iBlockCount = 0;
            pSourceFile->tBlocks = 0;
        }
        bResult = true;
    } else {
        pSourceFile->iBlockCount = -2;
        pSourceFile->tBlocks = 0;
        bResult = false; // in case of irrecoverable scanner error
    }
    pSourceFile->pRootNamespace->eCompState = ESOURCE_COMP_STATE_DONE_PARSED;

    release_arena(&blockParsingArena);
    release_arena(&blockVectorArena);

    // Transfers identifiers found locally in this file to stable and global vec of identifiers, in one go
    Slice<FFString> localIds = pSourceFile->locallyFoundIdentifierNamesByOffsetId.asSlice();
    u32 uCountLocalIds = pSourceFile->locallyFoundIdentifierNamesByOffsetId.size();
    for (u32 uLocalIndex = 0; uLocalIndex < uCountLocalIds; uLocalIndex++)
        pProgramCompState->vecAllIdentifierNamesById.append(localIds[uLocalIndex]);
    pSourceFile->uNextVirtualBlock = u32(pSourceFile->iBlockCount);

    return bResult;
}

local_func TCContext* make_typecheck_task_for_whole_source_file(SourceFileDescAndState* pSourceFile,
    WholeProgramCompilationState* pCompState, LocLib_CompilationResults* pCompilationResults)
{
    TCContext* pResult = (TCContext*)alloc_from(pCompState->globalArena, sizeof(TCContext), alignof(TCContext));

    pResult->pIsolatedSourceFile = pSourceFile;
    pResult->pCompilationParams = pCompState->pCompilationParams;
    pResult->pWorker = 0; // will be assigned later on task launch
    pResult->pProgCompilationState = pCompState;    // the compilation state for the entire program

    pResult->eKind = ETypecheckContextKind::ECTXKIND_GLOBAL_PACKAGE;
    pResult->eBlockKind = ETypecheckBlockKind::EBLOCKKIND_BASE;
    pResult->eGlobalDeclScope = EScopeKind::SCOPEKIND_GLOBAL_PACKAGE;
    pResult->uNestedExpansionLevel = 0;
    pResult->uFlags = 0; // most flags are unset at basic, root level context
    pResult->pCurrentBlock = 0; // marker for starting by alloc-and-init of block 0
    pResult->pNamespace = pSourceFile->pRootNamespace;
    pResult->pRepo = &(pSourceFile->filewiseConstRepo);

    // the following are unused for basic, root level context

    //pResult->pVecLocalBindings = 0;
    pResult->pProcResult = 0;
    pResult->pProcSource = 0;
    pResult->vecTypecheckedBlocks = {};
    pResult->pTmpRepo = 0;
    pResult->pParentContext = 0;
    pResult->pVecOfGotoPlaceholdersToReturnPoint = 0;

    pResult->uGlobalStatementOnHold = 0;
    pResult->mapLocalNodeInfoIfResumingCurrentStatement = {};

    return pResult;
}


local_func void do_handle_seq_block_exit(TCContext* pTCContext)
{
    Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);
    Assert_(pTCContext->pProcResult);
    Assert_(pTCContext->pCurrentBlock->pParentBlock);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
        "TC Block exit for block %u, returning to parent block %u",
        u64(pTCContext->pCurrentBlock->uAstBlockIndex), u64(pTCContext->pCurrentBlock->pParentBlock->uAstBlockIndex)), pTCContext->pWorker);

    TCSeqSourceBlock* pCurrentAsSeq = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;
    TCSeqSourceBlock* pParentAsSeq = (TCSeqSourceBlock*)pTCContext->pCurrentBlock->pParentBlock;

    if (0u == (pCurrentAsSeq->uFlagsAndScopeBaseIndex & BLOCKFLAG_IS_NON_SCOPING)) { // nominal case

        //Assert_(pCurrentAsSeq->pVecDeferredBlocksInDeclOrder != pParentAsSeq->pVecDeferredBlocksInDeclOrder);
        Assert_(pCurrentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses != pParentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);

        i32 iBaseScopedDeclIndex = i32(pCurrentAsSeq->uFlagsAndScopeBaseIndex & BLOCK_SCOPED_ENTITY_BASE_INDEX_MASK);
        i32 iScopedEntityCount = i32(pTCContext->pProcResult->vecScopedEntities.size());
        for (i32 iScopedEntity = iScopedEntityCount-1; iScopedEntity >= iBaseScopedDeclIndex; iScopedEntity--) {
            ScopedEntityHandle hScopedEntity = pTCContext->pProcResult->vecScopedEntities[iScopedEntity];
            if (get_scoped_entity_kind(hScopedEntity) == ESCOPEDENTITY_DEFER) { // path in the presence of defers
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Defer already positionned on block fallthrough"), pTCContext->pWorker);
                RegisteredDeferInDeclOrder* pLastDeclaredDefer = get_scoped_entity_as_declared_defer(hScopedEntity);
                // This defer case is very simply handled, since we always emit deferred blocks, as soon as encountered,
                //   towards "standard" block exit (or LOOP) => All we'd need to to here is to branch to start-IR for that last deferred block.
                // scope ending (similar to the "default path" as appears below) will be already handled at the end of
                //   first-declared (and last-unstacked) deferred block, eventually reached by the defer chain to which we goto now:
                ir_emit_goto(pLastDeclaredDefer->pEmittedDeferedBlockWhenEncountered->_uBlockOpeningIRIffSeq,
                    pTCContext->pRepo, pTCContext, EBranchKind::BRANCH_TAKEN_TO_DEFAULT_DEFER);
                goto after_default_path;
            }
        }

        { default_path: // Will be skipped if found defer
            
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Block fallthrough, without defer"), pTCContext->pWorker);

            // scope ending... note that 'block opening +1' shall be IR of scope start
            ir_emit_marker_end_scope(pCurrentAsSeq->_uBlockOpeningIRIffSeq + 1u, pTCContext->pRepo, pTCContext);

            // then branch to wherever is needed at the end of that block:
            if (pCurrentAsSeq->uIRBeforeLoopConditionIfBlockIsLoop != INVALID_IR_CODE) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Block is marked as a loop => emitting GOTO its start point"), pTCContext->pWorker);

                // If we have a special start-of-loop target, then directly jump to it.
                ir_emit_goto(pCurrentAsSeq->uIRBeforeLoopConditionIfBlockIsLoop,   // 'toloopcond_before' is default for all just-TC loops.
                    pTCContext->pRepo, pTCContext, EBranchKind::BRANCH_TAKEN_TOLOOPCOND_BEFORE); // optimizer may prefer otherwise
                // and that should be the 'only' registered special-way to end that block:
                Assert_(pCurrentAsSeq->pVecPlaceholdersToElse == 0);

            } else { Assert_(pCurrentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Block is standard => emitting GOTO placeholders, registered to its 'after-block' vector"), pTCContext->pWorker);

                u32 uGotoIR = ir_emit_goto_placeholder(pTCContext->pRepo, pTCContext, EBranchKind::BRANCH_TAKEN_BLOCK_END);
                pCurrentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses->append(uGotoIR);
            }

        } 

        after_default_path:

        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Emitting the after-block marker"), pTCContext->pWorker);
            pCurrentAsSeq->uIROfAfterBlock = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
        }

        if (pCurrentAsSeq->pVecPlaceholdersToElse)
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Block is of an 'if' or 'elif' kind => directly solving placeholder to the 'else' path now, to its after-block marker at IR #%u",
                u64(pCurrentAsSeq->uIROfAfterBlock)), pTCContext->pWorker);
            do_replace_jump_placeholders_to(pCurrentAsSeq->uIROfAfterBlock, *(pCurrentAsSeq->pVecPlaceholdersToElse),
                pTCContext->pRepo, pTCContext);
        }

        Assert_(iBaseScopedDeclIndex >= (pParentAsSeq->uFlagsAndScopeBaseIndex & BLOCK_SCOPED_ENTITY_BASE_INDEX_MASK));
        pTCContext->pProcResult->vecScopedEntities.resize_non_zeroing(u32(iBaseScopedDeclIndex));

    } else { // case of a non-scoped block
        // => not really a 'block' as far as regular block termination strategies are concerned
        //Assert_(pCurrentAsSeq->pVecDeferredBlocksInDeclOrder == pParentAsSeq->pVecDeferredBlocksInDeclOrder);
        Assert_(pCurrentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses == pParentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses);
        Assert_(pCurrentAsSeq->uIRBeforeLoopConditionIfBlockIsLoop == pParentAsSeq->uIRBeforeLoopConditionIfBlockIsLoop);
    }
}

local_func void do_handle_proc_end(TCContext* pTCContext)
{
    Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);
    Assert_(pTCContext->pProcResult);
    Assert_(pTCContext->pCurrentBlock->pParentBlock == 0); // TODO: change this check to comparison with root of source once we do inlining

    Assert(pTCContext->eKind == ETypecheckContextKind::ECTXKIND_PROCBODY,
        "do_handle_proc_end() not yet implemented for expansions"); // TODO !!

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
        "TC Block exit for block with ast index %u, being last in proc", u64(pTCContext->pCurrentBlock->uAstBlockIndex)), pTCContext->pWorker);

    TCSeqSourceBlock* pCurrentAsSeq = (TCSeqSourceBlock*)pTCContext->pCurrentBlock;
    Assert_(pCurrentAsSeq->uIRBeforeLoopConditionIfBlockIsLoop == INVALID_IR_CODE);
    Assert_(pCurrentAsSeq->pVecPlaceholdersToAfterBlockAndAfterElses == 0);
    Assert_(pCurrentAsSeq->pVecPlaceholdersToElse == 0);
    Assert_(0u == (pCurrentAsSeq->uFlagsAndScopeBaseIndex & BLOCKFLAG_IS_NON_SCOPING));

    Assert_(pTCContext->vecTypecheckedBlocks._alloc.arena.root_chunk_handle.uTagged != 0uLL);
    u32 uCountBlocks = pTCContext->vecTypecheckedBlocks.size();
    Assert_(uCountBlocks);
    Assert_(pTCContext->vecTypecheckedBlocks[0] == pCurrentAsSeq); // first emitted should be root

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Solving branches for whole emitted procedure (%u emitted blocks)", u64(uCountBlocks)), pTCContext->pWorker);

        for (u32 uBlock = 1u; uBlock < uCountBlocks; uBlock++) { // Starting from index 1 => skipping first emitted (should be root)
            TCSeqSourceBlock* pBlock = pTCContext->vecTypecheckedBlocks[uBlock];
            if (pBlock->pVecPlaceholdersToAfterBlockAndAfterElses) { // May not be the case if child of #if at root of proc

                if (pBlock->pVecPlaceholdersToAfterBlockAndAfterElses->size()) {

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Solving exits for block recorded in pos #%u (Ast %u)", u64(uBlock), u64(pBlock->uAstBlockIndex)), pTCContext->pWorker);
                
                    u32 uCurrentBlockExitCandidate = pBlock->uIROfAfterBlock;
                    TCSeqSourceBlock* pParentBlock = (TCSeqSourceBlock*)pBlock->pParentBlock;
                    Assert_(pParentBlock);
                    u32 uCountStatementsInParent = pParentBlock->vecStatements.size();
                    u32 uIndexOfNextStatement = pBlock->uIndexOfParentStatementInParentBlock + 1u;
                    while (uIndexOfNextStatement < uCountStatementsInParent) {
                        TCStatement* pNextStatement = pParentBlock->vecStatements[uIndexOfNextStatement];
                        if (pNextStatement->pChildBlock == 0) // Next statement has no child block => cannot be a valid 'else' (or 'elif')
                            break;
                        TCSeqSourceBlock* pChildBlockOfNextStatement = (TCSeqSourceBlock*)pNextStatement->pChildBlock;
                        if (0u == (pChildBlockOfNextStatement->uFlagsAndScopeBaseIndex & BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND))
                            break;
                        // Otherwise, this next statement is indeed of an 'else' kind (or elif in an elif..else chain)
                        // => we record its IR-after as possible position, and we need to continue iterating until we find a statement which is not.
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                            "Next-Statement %u in parent-block (ast %u) is an else or elif => target of current block-end will be after that",
                            u64(uIndexOfNextStatement), u64(pParentBlock->uAstBlockIndex)), pTCContext->pWorker);
                        uCurrentBlockExitCandidate = pChildBlockOfNextStatement->uIROfAfterBlock;
                        uIndexOfNextStatement++;
                    }
                    {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                            "Solving jumps and branches to block-exit towards IR position #%u", u64(uCurrentBlockExitCandidate)), pTCContext->pWorker);
                        do_replace_jump_placeholders_to(uCurrentBlockExitCandidate, *(pBlock->pVecPlaceholdersToAfterBlockAndAfterElses),
                            pTCContext->pRepo, pTCContext);
                    }
                    if (pBlock->pVecPlaceholdersToElse) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                            "Note that this bloc is of an if-kind ; but its jumps-to-else were already solved"), pTCContext->pWorker);
                    }

                } else {
                    // this is a block with no recorded 'exits'...
                    // maybe case of known infinite loops, eg "while true" ?
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Block recorded in pos #%u (Ast %u) has no exits to solve...", u64(uBlock), u64(pBlock->uAstBlockIndex)), pTCContext->pWorker);
                }
            } else {
                Assert_(pBlock->uFlagsAndScopeBaseIndex & BLOCKFLAG_IS_NON_SCOPING);
            }
        }
    }

    i32 iBaseScopedDeclIndex = i32(pTCContext->pProcSource->pRootTcBlock->uFlagsAndScopeBaseIndex & BLOCK_SCOPED_ENTITY_BASE_INDEX_MASK);
    i32 iScopedEntityCount = i32(pTCContext->pProcResult->vecScopedEntities.size());
    for (i32 iScopedEntity = iScopedEntityCount-1; iScopedEntity >= iBaseScopedDeclIndex; iScopedEntity--) {
        ScopedEntityHandle hScopedEntity = pTCContext->pProcResult->vecScopedEntities[iScopedEntity];
        if (get_scoped_entity_kind(hScopedEntity) == ESCOPEDENTITY_DEFER) { // path in the presence of defers
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Defer already positionned on proc fallthrough"), pTCContext->pWorker);
            RegisteredDeferInDeclOrder* pLastDeclaredDefer = get_scoped_entity_as_declared_defer(hScopedEntity);
            // This defer case is very simply handled, since we always emit deferred blocks, as soon as encountered,
            //   towards "standard" block exit, which is this case will automatically be a defer chain to a final already emitted 'ret' (naked)
            // proc ending with ret (similar to the "default path" as appears below) will be already handled at the end of
            //   first-declared (and last-unstacked) deferred block, eventually reached by the defer chain to which we goto now:
            u32 uGotoIR = ir_emit_goto(pLastDeclaredDefer->pEmittedDeferedBlockWhenEncountered->_uBlockOpeningIRIffSeq,
                pTCContext->pRepo, pTCContext, EBranchKind::BRANCH_TAKEN_TO_DEFAULT_DEFER);
            Assert_(pCurrentAsSeq->vecStatements.size());
            pCurrentAsSeq->vecStatements.last()->uLastIRorGlobalTCResult = uGotoIR;
            return; // and we're done !
        }
    } // otherwise fallthrough default path:

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Emitting naked return for proc fallthrough"), pTCContext->pWorker);

    // a naked return in IR in case of fall-through...
    ir_emit_return(pTCContext->pRepo, pTCContext);  // CLEANUP ? currently not registered as 'uLastIRorGlobalTCResult' of anything here

}

local_func_inl TracableEvent EventTCMG_TC_FILE(u32 uFileId, FFString fileName, bool bResumed, u32 uStatementIfResumed = 0u, u32 uBlockIfResumed = 0) {
    TracableEvent result = {};
    result.uEventType = EEvtTCMG::EEVT_TCMG_TC_FILE;
    Assert_(uFileId <= 0x0000'FFFFu);
    result.uEventPayload32 = uFileId;
    if (bResumed)
        result.uEventPayload32 |= 0x8000'0000u;
    result.tPayload[0] = reinterpret_cast<u64>(fileName.begin());
    result.tPayload[3] = (u64(uBlockIfResumed) << 32) | u64(uStatementIfResumed);
    return result;
}
local_func void print_event_TCMG_TC_FILE_to(char* szBuffer, const TracableEvent& eventEntry)
{
    Assert_(eventEntry.eLocPhase == ELOCPHASE_TC_MGR);
    Assert_(eventEntry.uEventType == EEVT_TCMG_TC_FILE);
    u32 uFileId = eventEntry.uEventPayload32 & 0x7FFF'FFFFu;
    FFString asFFStringFileName;
    asFFStringFileName.pStart = reinterpret_cast<u8*>(eventEntry.tPayload[0]);
    Assert_(asFFStringFileName.can_be_used_as_c_str());
    if (eventEntry.uEventPayload32 & 0x8000'0000u) {
        u32 uStatementInBlock = u32(eventEntry.tPayload[3]);
        u32 uBlockInFile = u32(eventEntry.tPayload[3] >> 32);
        sprintf(szBuffer, "Resuming %s on file %u (%s) - from statement %u on block %u", tEvtTCMGDbgStr[eventEntry.uEventType],
            uFileId, asFFStringFileName.c_str(), uStatementInBlock, uBlockInFile);
    } else {
        sprintf(szBuffer, "First Attempt %s on file %u (%s)", tEvtTCMGDbgStr[eventEntry.uEventType],
            uFileId, asFFStringFileName.c_str());
    }
}

local_func TracableEvent EventTCMG_TC_PROC(TCContext* pTCContext, bool bResumed)
{
    TracableEvent result = {};
    result.uEventType = EEvtTCMG::EEVT_TCMG_TC_PROC;
    Assert_(pTCContext->pProcResult);
    Assert_(pTCContext->pProcSource);
    result.uEventPayload32 = pTCContext->pProcResult->uRegistrationIndex;
    if (bResumed)
        result.uEventPayload32 |= 0x8000'0000u;
    SourceFileDescAndState* pFileWithSource = pTCContext->pIsolatedSourceFile; // CLEANUP: assumed always true ?
    TCStatement* pStatementWithSignature = pTCContext->pProcSource->pStatementWithSignature;
    int iLine, unused1, unused2;
    get_line_and_col_from(0, pStatementWithSignature->pAstStatement,
        pFileWithSource->tBlocks + pStatementWithSignature->uBlockIndexInSourceFile,
        pFileWithSource, &unused1, &iLine, &unused2);
    result.tPayload[0] = (u64(u32(pFileWithSource->iRegistrationIndex)) << 32) | u64(u32(iLine));
    result.tPayload[1] = reinterpret_cast<u64>(pFileWithSource->sourceFileName.begin());
    result.tPayload[2] = (u64(u32(pTCContext->pProcResult->iSourceFileIndex)) << 32) | u64(u32(pTCContext->pProcResult->iPrimaryIdentifier));
    result.tPayload[3] = reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pTCContext->pProcResult->iPrimaryIdentifier).begin());
    return result;
}
local_func void print_event_TCMG_TC_PROC_to(char* szBuffer, const TracableEvent& eventEntry)
{
    Assert_(eventEntry.eLocPhase == ELOCPHASE_TC_MGR);
    Assert_(eventEntry.uEventType == EEVT_TCMG_TC_PROC);
    u32 uProcIdinFile = eventEntry.uEventPayload32 & 0x7FFF'FFFFu;
    i32 iProcFileId = i32(eventEntry.tPayload[2] >> 32);
    i32 iSourceFileId = i32(eventEntry.tPayload[0] >> 32);
    i32 iSourceFileLine = i32(eventEntry.tPayload[0]);
    i32 iPrimaryIdentifier = i32(eventEntry.tPayload[2]);
    FFString asFFStringPrimaryId;
    FFString asFFStringSourceFileName;
    asFFStringPrimaryId.pStart = reinterpret_cast<u8*>(eventEntry.tPayload[3]);
    asFFStringSourceFileName.pStart = reinterpret_cast<u8*>(eventEntry.tPayload[1]);
    Assert_(asFFStringPrimaryId.can_be_used_as_c_str());
    Assert_(asFFStringSourceFileName.can_be_used_as_c_str());
    sprintf(szBuffer, "%s %s on proc %u of file %d (declared at line %d in file %d (%s)) with primary id %d (%s)",
        (eventEntry.uEventPayload32 & 0x8000'0000u) ? "Resuming" : "First Attempt", tEvtTCMGDbgStr[eventEntry.uEventType],
        uProcIdinFile, iProcFileId, iSourceFileLine, iSourceFileId, asFFStringSourceFileName.c_str(), iPrimaryIdentifier, asFFStringPrimaryId.c_str());
}

local_func TracableEvent EventTCMG_TC_COMPOUND(TCContext* pTCContext, bool bResumed)
{
    TracableEvent result = {};
    result.uEventType = EEvtTCMG::EEVT_TCMG_TC_COMPOUND;
    Assert_(pTCContext->pCompoundToTC);
    result.uEventPayload32 = pTCContext->pCompoundToTC->pCompoundType->uRegistrationIndex;
    if (bResumed)
        result.uEventPayload32 |= 0x8000'0000u;
    SourceFileDescAndState* pFileWithSource = pTCContext->pIsolatedSourceFile; // CLEANUP: assumed always true ?
    TCStatement* pStatementWithSignature = pTCContext->pCompoundToTC->pStatementWithSignature;
    int iLine, unused1, unused2;
    get_line_and_col_from(0, pStatementWithSignature->pAstStatement,
        pFileWithSource->tBlocks + pStatementWithSignature->uBlockIndexInSourceFile,
        pFileWithSource, &unused1, &iLine, &unused2);
    result.tPayload[0] = (u64(u32(pFileWithSource->iRegistrationIndex)) << 32) | u64(u32(iLine));
    result.tPayload[1] = reinterpret_cast<u64>(pFileWithSource->sourceFileName.begin());
    result.tPayload[2] = (u64(u32(pTCContext->pIsolatedSourceFile->iRegistrationIndex)) << 32) | u64(u32(pTCContext->pCompoundToTC->iPrimaryIdentifier));
    result.tPayload[3] = reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pTCContext->pCompoundToTC->iPrimaryIdentifier).begin());
    return result;
}
local_func void print_event_TCMG_TC_COMPOUND_to(char* szBuffer, const TracableEvent& eventEntry)
{
    Assert_(eventEntry.eLocPhase == ELOCPHASE_TC_MGR);
    Assert_(eventEntry.uEventType == EEVT_TCMG_TC_COMPOUND);
    u32 uCompoundIdinFile = eventEntry.uEventPayload32 & 0x7FFF'FFFFu;
    i32 iCompoundFileId = i32(eventEntry.tPayload[2] >> 32);
    i32 iSourceFileId = i32(eventEntry.tPayload[0] >> 32);
    i32 iSourceFileLine = i32(eventEntry.tPayload[0]);
    i32 iPrimaryIdentifier = i32(eventEntry.tPayload[2]);
    FFString asFFStringPrimaryId;
    FFString asFFStringSourceFileName;
    asFFStringPrimaryId.pStart = reinterpret_cast<u8*>(eventEntry.tPayload[3]);
    asFFStringSourceFileName.pStart = reinterpret_cast<u8*>(eventEntry.tPayload[1]);
    Assert_(asFFStringPrimaryId.can_be_used_as_c_str());
    Assert_(asFFStringSourceFileName.can_be_used_as_c_str());
    sprintf(szBuffer, "%s %s on compound %u of file %d (declared at line %d in file %d (%s)) with primary id %d (%s)",
        (eventEntry.uEventPayload32 & 0x8000'0000u) ? "Resuming" : "First Attempt", tEvtTCMGDbgStr[eventEntry.uEventType],
        uCompoundIdinFile, iCompoundFileId, iSourceFileLine, iSourceFileId, asFFStringSourceFileName.c_str(), iPrimaryIdentifier, asFFStringPrimaryId.c_str());
}

local_func TracableEvent EventTCMG_TC_STATEMENT(u32 uStatementIndex, TCStatement* pTCStatement, TCContext* pTCContext)
{
    TracableEvent result = {};
    result.uEventType = EEvtTCMG::EEVT_TCMG_TC_STATEMENT;
    result.uEventPayload32 = uStatementIndex;
    if (pTCContext->bResumingCurrentStatement)
        result.uEventPayload32 |= 0x8000'0000u;
    result.tPayload[0] = (u64(pTCStatement->uBlockIndexInSourceFile) << 32) | u64(pTCStatement->uStatementIndexInBlock);
    result.tPayload[1] = u64(pTCStatement->pAstStatement->uFlags);
    return result;
}
local_func void print_event_TCMG_TC_STATEMENT_to(char* szBuffer, const TracableEvent& eventEntry)
{
    Assert_(eventEntry.eLocPhase == ELOCPHASE_TC_MGR);
    Assert_(eventEntry.uEventType == EEVT_TCMG_TC_STATEMENT);
    u32 uStatement = eventEntry.uEventPayload32 & 0x7FFF'FFFFu;
    u32 uAstIndex = u32(eventEntry.tPayload[0]);
    u32 uAstBlockIndex = u32(eventEntry.tPayload[0] >> 32);
    sprintf(szBuffer, "%s at index %u in current block (from AST: %u in block %u) - flags 0x%08x", tEvtTCMGDbgStr[eventEntry.uEventType],
        uStatement, uAstIndex, uAstBlockIndex, u32(eventEntry.tPayload[1]));
    if (eventEntry.uEventPayload32 & 0x8000'0000u) {
        sprintf(szBuffer + strlen(szBuffer), " (resuming)");
    }
}


local_func ETCResult start_or_resume_typecheck_task(TCContext* pTCContext)
{
    Assert_(pTCContext->pWorker);

    pTCContext->bResumingCurrentStatement = 0;

    if (pTCContext->pCurrentBlock == 0) { // This signals a tc context starting afresh

        if (pTCContext->pProcResult == 0 && pTCContext->pCompoundToTC == 0) { // This moreover signals a "whole source file TC task", first attempt...
            Assert_(pTCContext->eKind == ETypecheckContextKind::ECTXKIND_GLOBAL_PACKAGE); // 'package' is starting default on new file
            Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_BASE);
            Assert_(pTCContext->pIsolatedSourceFile->iBlockCount >= 0); // simply check non-taggued as negative

            TRACE_ENTER(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, EventTCMG_TC_FILE(
                u32(pTCContext->pIsolatedSourceFile->iRegistrationIndex),
                pTCContext->pIsolatedSourceFile->sourceFileName, false),
                pTCContext->pWorker);

            if (pTCContext->pIsolatedSourceFile->iBlockCount > 0 && pTCContext->pIsolatedSourceFile->tBlocks->uStatementCount > 0)
                pTCContext->pCurrentBlock = tc_alloc_and_init_base_block(0, ECTXKIND_GLOBAL_PACKAGE, 0, pTCContext);
            else { // otherwise NOOP for typechecking this file
                platform_log_info("Empty file, done...", true);
                TRACE_EXIT(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, pTCContext->pWorker);
                return ETCResult::ETCR_SUCCESS;
            }

        } else if (pTCContext->pProcResult) { // This moreover signals a "proc body TC task", first attempt...
            Assert_(pTCContext->eKind == ETypecheckContextKind::ECTXKIND_PROCBODY);
            Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ);
            Assert_(pTCContext->pIsolatedSourceFile->iBlockCount > 0 && pTCContext->pIsolatedSourceFile->tBlocks->uStatementCount > 0);
            Assert_(pTCContext->pProcSource);
            Assert_(pTCContext->pProcSource->pRootTcBlock);
            u32 uBlockIndex = u32(reinterpret_cast<u64>(pTCContext->pProcSource->pRootTcBlock) >> 2);
            TCSeqSourceBlock* pRootTCBlock = tc_alloc_and_init_seq_block(uBlockIndex, 0, 0u, pTCContext);

            TRACE_ENTER(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, EventTCMG_TC_PROC(pTCContext, false), pTCContext->pWorker);

            emit_proc_intro(pTCContext->pProcResult, pTCContext);
            pRootTCBlock->_uBlockOpeningIRIffSeq = ir_emit_marker_jump_target(pTCContext->pRepo, pTCContext);
            pTCContext->pProcSource->pRootTcBlock = pRootTCBlock;
            pTCContext->pCurrentBlock = pRootTCBlock;

            Assert_(pTCContext->vecTypecheckedBlocks._alloc.arena.root_chunk_handle.uTagged != 0uLL);
            pTCContext->vecTypecheckedBlocks.append(pRootTCBlock);

        } else { Assert_(pTCContext->pCompoundToTC); // Structs, Unions, Enums, 
            Assert_(pTCContext->eKind == ETypecheckContextKind::ECTXKIND_COMPOUND);
            Assert_(pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_BASE);
            Assert_(pTCContext->pIsolatedSourceFile->iBlockCount > 0 && pTCContext->pIsolatedSourceFile->tBlocks->uStatementCount > 0);
            Assert_(pTCContext->pProcSource == 0);
            Assert_(pTCContext->pProcResult == 0);
            u32 uBlockIndex = u32(reinterpret_cast<u64>(pTCContext->pCompoundToTC->pRootTcBlock) >> 2);
            TCBaseSourceBlock* pRootTCBlock = tc_alloc_and_init_base_block(uBlockIndex, ECTXKIND_COMPOUND, 0u, pTCContext);

            TRACE_ENTER(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, EventTCMG_TC_COMPOUND(pTCContext, false), pTCContext->pWorker);

            pTCContext->pCompoundToTC->pRootTcBlock = pRootTCBlock;
            pTCContext->pCurrentBlock = pRootTCBlock;
        }

    } else { // resuming an on-hold task

        if (pTCContext->pProcResult == 0 && pTCContext->pCompoundToTC == 0) { // resuming a "whole source file TC task" on a subpart
            u32 uStatementToResume = pTCContext->uGlobalStatementOnHold;
            pTCContext->pCurrentBlock->uStatementBeingTypechecked = uStatementToResume;
            Assert_(uStatementToResume < pTCContext->pCurrentBlock->vecStatements.size());
            TCStatement* pStatement = pTCContext->pCurrentBlock->vecStatements[uStatementToResume];

            TRACE_ENTER(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, EventTCMG_TC_FILE(
                u32(pTCContext->pIsolatedSourceFile->iRegistrationIndex),
                pTCContext->pIsolatedSourceFile->sourceFileName, true,
                uStatementToResume, pTCContext->pCurrentBlock->uAstBlockIndex),
                pTCContext->pWorker);

            Assert_(pStatement->uLastIRorGlobalTCResult == 1 + ETCResult::ETCR_WAITING);
            pStatement->uLastIRorGlobalTCResult = 0;
            pTCContext->bResumingCurrentStatement = 1u;

        } else if (pTCContext->pProcResult) { // resuming a "proc body TC task" until the end
            pTCContext->bResumingCurrentStatement = 1u;

            TRACE_ENTER(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, EventTCMG_TC_PROC(pTCContext, true), pTCContext->pWorker);

        } else { Assert_(pTCContext->pCompoundToTC); // resuming a struct, union, or enum body-TC-task
            u32 uStatementToResume = pTCContext->uGlobalStatementOnHold;
            pTCContext->pCurrentBlock->uStatementBeingTypechecked = uStatementToResume;
            Assert_(uStatementToResume < pTCContext->pCurrentBlock->vecStatements.size());
            TCStatement* pStatement = pTCContext->pCurrentBlock->vecStatements[uStatementToResume];

            TRACE_ENTER(ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO, EventTCMG_TC_COMPOUND(pTCContext, true), pTCContext->pWorker);

            if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                Assert_(pStatement->uLastIRorGlobalTCResult == 1 + ETCResult::ETCR_WAITING);
                pStatement->uLastIRorGlobalTCResult = 0u;
            } else {
                Assert_(pStatement->uLastIRorGlobalTCResult == 0u);
            }
            pTCContext->bResumingCurrentStatement = 1u;
        }
    }

    ATraceBlockAutoPopper autoPopTrace(pTCContext->pWorker, ELOCPHASE_TC_MGR, _LLVL2_IMPORTANT_INFO);

    pTCContext->uNodeIndexWithWait = 0;
    u32 uCurrentStatement = pTCContext->pCurrentBlock->uStatementBeingTypechecked;

    while (true) {

        if (uCurrentStatement < pTCContext->pCurrentBlock->vecStatements.size()) {
            TCStatement* pStatement = pTCContext->pCurrentBlock->vecStatements[uCurrentStatement];
            ETCResult result = ETCResult::ETCR_ERROR;

            if (0 == (pStatement->pAstStatement->uFlags & IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD)) {

                BLOCK_TRACE(ELOCPHASE_TC_MGR, _LLVL5_SIGNIFICANT_STEP, EventTCMG_TC_STATEMENT(
                    uCurrentStatement, pStatement, pTCContext), pTCContext->pWorker);

                ArenaRefPoint refTmpBeforeStatement = get_arena_ref_point(pTCContext->pWorker->tmpArena);

                Assert_(pStatement->vecNodes.size()); // 'NOOP' statement if you really want... but no empty nodelist allowed.
                TCNode* pMainTcNode = pStatement->vecNodes[0];

                if (LIKELY( ! pTCContext->bResumingCurrentStatement )) {
                    if (pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ) {
                        Assert_(pTCContext->uFlags & CTXFLAG_ALLOW_RUNTIME);
                        Assert_(pTCContext->pProcResult);
                        Assert_(0 == pStatement->uLastIRorGlobalTCResult);
                    } else {
                        Assert_(0 == (pTCContext->uFlags & CTXFLAG_ALLOW_RUNTIME));
                        if (!should_tc_ctx_halt_on_non_success(pTCContext)) {
                            if (pStatement->uLastIRorGlobalTCResult) {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                                    "TC task at non-seq scope reached an already TC'd statement. Ending current task right there"), pTCContext->pWorker);
                                return ETCResult::ETCR_SUCCESS;      // => marks the end of a resumed typecheck at a non-sequential scope
                            }
                        } else {
                            Assert_(0 == pStatement->uLastIRorGlobalTCResult);
                        }
                    }
                }
            
                TmpTCNode mainNode = {};
                mainNode.pTCNode = pMainTcNode;

                typecheck_main_node:
                Assert_(mainNode.pTCNode);
                // even if resuming a previous-'wait' result, main tc node should not be 'typechecked' (nor in error)
                Assert_(is_node_tc_not_started(mainNode.pTCNode));

                u8 uNodeKind = u8(pMainTcNode->ast.uNodeKindAndFlags);
                if (uNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
                    // TODO: some syntax for comptime eval... and/or implicit comptime if in non-sequential context.
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking invocation-form at statement level"), pTCContext->pWorker);
                    Assert_(u8(pMainTcNode->ast.uNodeKindAndFlags >> 8) == ETOK_INVOCATION);
                    bool bWasMacroExpansion = false;
                    // proc invocations are to be treated separately, since they could very well be macros and require expansion...
                    result = typecheck_invocation_form(&mainNode, pStatement, pTCContext,
                        EExpectedExpr::EXPECT_REGULAR, UpwardsInference{},
                        INVALID_NODE_INDEX, 0, 0, EInvocFormResultCount::EINVOC_NO_RETURN, &bWasMacroExpansion);
                    if (bWasMacroExpansion)
                        continue; // we'll thus retry typecheck from resulting statement at same position (if any...)
                } else {
                    switch (uNodeKind) {
                        case ENodeKind::ENODE_ST_ASSIGNMENT:
                            result = typecheck_assignment_statement(&mainNode, pStatement, pTCContext);
                            break;
                        case ENodeKind::ENODE_ST_OP_AND_ASSIGN:
                            result = typecheck_op_and_assignment_statement(&mainNode, pStatement, pTCContext);
                            break;
                        case ENodeKind::ENODE_ST_USING: 
                            result = typecheck_using_statement(&mainNode, pStatement, pTCContext);
                            break;
                        case ENodeKind::ENODE_ST_CONTROL_FLOW: 
                            result = typecheck_control_flow_statement(&mainNode, pStatement, pTCContext);
                            break;
                        case ENodeKind::ENODE_ST_PAN_SPECIAL: 
                            result = typecheck_pan_statement(&mainNode, pStatement, pTCContext);
                            break;
                        case ENodeKind::ENODE_EXPRLIST_NODE:
                            result = typecheck_statement_level_exprlist(&mainNode, pStatement, pTCContext);
                            break;

                        case ENodeKind::ENODE_ST_DECLARATION: {
                            u8 uOp = u8(pMainTcNode->ast.uNodeKindAndFlags >> 8);
                            bool bIsConst = (uOp == ETOK_CONST_DECL);
                            Assert_(bIsConst || (uOp == ETOK_VARDECL));
                            result = typecheck_declaration_statement(&mainNode, bIsConst, pStatement, pTCContext);
                        } break;

                        case ENodeKind::ENODE_SECONDARY_TOKEN_WRAPPER: {
                            u32 uTrueMainNodeIndex = pMainTcNode->ast.uPrimaryChildNodeIndex;
                            u8 uTok = u8(pMainTcNode->ast.uNodeKindAndFlags >> 8);
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                                "Unwrapping sec-token wrapper before main node %u (token '%s')",
                                u64(uTrueMainNodeIndex),
                                reinterpret_cast<u64>(tStandardPayloadsStr[uTok])), pTCContext->pWorker);
                            pMainTcNode = pStatement->vecNodes[uTrueMainNodeIndex];
                            mainNode.pTCNode = pMainTcNode;
                        } goto typecheck_main_node;

                        default: {
                            // if not in one of the above cases, then below this positions are all 'expression' nodes
                            if (uNodeKind < ENodeKind::ENODE_SUBEXPR_SLICE) {
                                result = typecheck_statement_level_expression(&mainNode, uNodeKind, pStatement, pTCContext);
                            } else {
                                // Other nodekinds, assumed invalid...
                                result = typecheck_other_as_statement(&mainNode, uNodeKind, pStatement, pTCContext);
                            }
                        }
                    }
                }

                reset_arena_no_release_to(refTmpBeforeStatement, pTCContext->pWorker->tmpArena);
            }

            pTCContext->bResumingCurrentStatement = 0;

            TCBaseSourceBlock* pForcedNextBlock = 0;
            if (result == ETCResult::ETCR_SUCCESS) {
                pTCContext->mapLocalNodeInfoIfResumingCurrentStatement = {};
                pForcedNextBlock = pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement;
                pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement = 0;
            } else {
                if (should_tc_ctx_halt_on_non_success(pTCContext)) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
                        "Halting TC current context after error or wait"), pTCContext->pWorker);
                    return result;
                }
            }

            if (!should_tc_ctx_halt_on_non_success(pTCContext)) {
                pStatement->uLastIRorGlobalTCResult = 1u + u32(result);
            }

            if (pForcedNextBlock) {
                Assert(0 == (reinterpret_cast<u64>(pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement) & 0x01uLL),
                        "nextblock after current statement should not be a tagged block index");

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
                    "Switching TC to start of Forced-Next-Block"), pTCContext->pWorker);

                pTCContext->pCurrentBlock = pForcedNextBlock;
                uCurrentStatement = 0;
                pTCContext->pCurrentBlock->uStatementBeingTypechecked = 0;
                if (is_ctx_with_proc_source(pTCContext)) {
                    Assert_(has_ctx_seq_blocks(pTCContext));
                    Assert_(pTCContext->vecTypecheckedBlocks._alloc.arena.root_chunk_handle.uTagged != 0uLL);
                    pTCContext->vecTypecheckedBlocks.append((TCSeqSourceBlock*)pForcedNextBlock);
                }
            } else {
                uCurrentStatement++;
                pTCContext->pCurrentBlock->uStatementBeingTypechecked = uCurrentStatement;
            }

        } else {

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
                "Done with last statement of current block"), pTCContext->pWorker);

            if (pTCContext->pCurrentBlock->pParentBlock) {

                if (pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ)
                    do_handle_seq_block_exit(pTCContext);
                else {
                    if (pTCContext->pCurrentBlock->pBlockIsDirectChildOfNamespace) {
                        Assert_(pTCContext->pCurrentBlock->pBlockIsDirectChildOfNamespace == pTCContext->pNamespace);
                        pTCContext->pNamespace = pTCContext->pNamespace->pParent;
                        Assert_(pTCContext->pNamespace);
                    }
                }

                pTCContext->pCurrentBlock = pTCContext->pCurrentBlock->pParentBlock;
                uCurrentStatement = pTCContext->pCurrentBlock->uStatementBeingTypechecked + 1u;
                pTCContext->pCurrentBlock->uStatementBeingTypechecked = uCurrentStatement;

            } else {
                if (pTCContext->eBlockKind == ETypecheckBlockKind::EBLOCKKIND_SEQ)
                    do_handle_proc_end(pTCContext);

                return ETCResult::ETCR_SUCCESS;
            }
        }
    }
}

local_func TCProcBodyResult* backend_find_proc_main(WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
        "Searching for user-defined proc 'main'"), pWorker);

    auto itFindMainAsId = pProgramState->mapAllIdentifierIdsByName.find("main");
    if (itFindMainAsId != pProgramState->mapAllIdentifierIdsByName.end()) {
        int iIdentifierHandle = itFindMainAsId.value();
        u64 uHash = get_map_hash(iIdentifierHandle);
        u32 uSourceFilesCount = pProgramState->vecSourceFiles.size();
        for (u32 uFileIndex = 0; uFileIndex < uSourceFilesCount; uFileIndex++) {
            SourceFileDescAndState* pSourceFile = pProgramState->vecSourceFiles[uFileIndex];
            auto itFindMainAsBinding = pSourceFile->pRootNamespace->mapAllGlobalDeclarationsById.findHashed(uHash, iIdentifierHandle);
            if (itFindMainAsBinding != pSourceFile->pRootNamespace->mapAllGlobalDeclarationsById.end()) {
                ValueBinding* pBinding = pSourceFile->vecAllGlobalBindings[itFindMainAsBinding.value()];
                if (is_value_tc_const(pBinding) && get_type_kind(pBinding->pType) == ETypeKind::ETYPEKIND_PROCLIKEBODY) {
                    const TypeInfo_ProcLike* pAsProc = (const TypeInfo_ProcLike*)pBinding->pType;
                    Assert_(is_value_known_or_nyka(pBinding));
                    Assert_(is_value_nyka_or_has_nyka(pBinding));
                    Assert_(is_value_known_embd(pBinding));
                    u64 uAsNYKA = pBinding->info.metaValue.knownValue.uEmbeddedValue;
                    i32 expectedZero;
                    u64 uIRofDecl = ir_decode_nyka_value(uAsNYKA, &expectedZero);
                    Assert_(0 == expectedZero);
                    Assert_(!ir_is_immediate(uIRofDecl));
                    Assert_(0uLL != (uIRofDecl & IR_STD_PARAM_HIGHMASK));
                    u16 uRepoIndex = u16(uIRofDecl >> IR_STD_PARAM_REPO_ID_SHIFT);
                    Assert_(uRepoIndex >= IR_REPO_ID_FIRST_FILE);
                    u32 uFileIndex = uRepoIndex - IR_REPO_ID_FIRST_FILE;
                    SourceFileDescAndState* pSourceFile = pProgramState->vecSourceFiles[uFileIndex];
                    u32 uPayload32 = u32(uIRofDecl >> IR_STD_PARAM_SHIFT);
                    u32 uHigh22and23 = uPayload32 & 0x00C0'0000u;
                    Assert_(0x0080'0000u == uHigh22and23);
                    u32 uIndex = uPayload32 & 0x003F'FFFFu;
                    TCProcBodyRegistration* pProcRegistration = pSourceFile->vecAllProcBodies[uIndex];
                    if (pProcRegistration->procResult.uIsForeignSource) {
                        platform_log_error("*** proc bound to identifier 'main' is supposed declared locally");
                        return 0;
                    }
                    return &(pProcRegistration->procResult);

                } else {
                    platform_log_error("*** identifier 'main' found at global scope but is not a proc-typed constant !!");
                    return 0;
                }
            }
            // TODO: also ensure not bound somewhere else after first hit ???
        }

        platform_log_error("*** could not find identifier 'main' : was not bound at global scope in any file...");
        return 0;

    } else {
        platform_log_error("*** could not find identifier 'main' : was not parsed as an identifier anywhere...");
        return 0;
    }
}

#include <cstdio>

#if 0
/*
const char szCBackendHeader[] = "\n"
    "typedef unsigned char        u8;\n"
    "typedef unsigned short      u16;\n"
    "typedef unsigned int        u32;\n"
    "typedef unsigned __int64    u64;\n"
    "typedef signed char          i8;\n"
    "typedef signed short        i16;\n"
    "typedef signed int          i32;\n"
    "typedef signed __int64      i64;\n"
    "typedef float               f32;\n"
    "typedef double              f64;\n"
        "\n"
    "#include <cstdio>\n"
    "#include \"windows.h\"\n"
        "\n";
*/

const char szCBackendHeader[] = "\n"
    "#include \"../HighPerfTools/BaseDecls.h\"\n"
    "#include \"../HighPerfTools/arithmetic_operations.h\"\n"
    "#include \"../HighPerfTools/cbck_support.h\"\n"
    "#include <windows.h>\n"
    "#include <stdio.h>\n"
        "\n";

const char szCBackendOnErrorDef[] = "\n"
    "static void " ERR_REPORT_FUNC_NAME "(u8 uCheckErrKind, int iFileIndex, u32 uAstBlock, u32 uAstStatement, u32 uTokenRef, u32 uIRofCheck, u32 uIRofInstr) {\n"
    "\tprintf(\"*** runtime-error type %u, from source-code expression in file #%d (%s):\\n\\t\"\n"
    "\t\t\"AstBlock #%u, AstStatement #%u TokenRef #%u, Check-IRpos #%u, Checked-instr-IRpos #%u\\n\",\n"
    "\t\tuCheckErrKind, iFileIndex, tFileNames__[iFileIndex], uAstBlock, uAstStatement, uTokenRef, uIRofCheck, uIRofInstr);\n"
    "}\n\n"
/*
    "static void " ERR_REPORT_FUNC_NAME_NO_AST_INFO "(u8 uCheckErrType, int iFileIndex, u32 uFuncBlock, u32 uIRInstrRef) {\n"
    "\tprintf(\"*** runtime-error type %u, from source-code expression in file #%d (%s):\\n\\t\"\n"
    "\t\t\"ProcBodyRoot #%u, IRCheck #%u, unknown AST info\\n\",\n"
    "\t\tuCheckErrType, iFileIndex, tFileNames__[iFileIndex], uFuncBlock, uIRInstrRef);\n"
    "}\n\n"
    "__declspec(noreturn) static void " ERR_CHECK_FUNCTION_NAME "(u64 uLastErrCheck, const u32* tValues, u32 uValuesCount, int iFileIndex, u32 uFuncBlock) {\n"
    "\tu32 err_check_c__ = u32(uLastErrCheck);\n"
    "\tu32 err_check_irpos__ = err_check_c__ & 0x00FFFFFFu;\n"
    "\tu8  err_check_type__ = u8(err_check_c__ >> 24);\n"
    "\tconst u32* pValues__ = tValues;\n"
    "\tfor (u32 err_check_it__ = 0; err_check_it__ < uValuesCount; err_check_it__++, pValues__ += 4) {\n" 
    "\t\tif(pValues__[0] == err_check_c__) {\n"
    "\t\t\t" ERR_REPORT_FUNC_NAME "(err_check_type__, iFileIndex, uFuncBlock, err_check_irpos__, pValues__[1], pValues__[2], pValues__[3]);\n"
    "\t\t\t__debugbreak();\n"
    "\t\t\tbreak;\n"
    "\t\t}\n"
    "\t}\n"
    "\t" ERR_REPORT_FUNC_NAME_NO_AST_INFO "(err_check_type__, iFileIndex, uFuncBlock, err_check_irpos__);\n"
    "\t__debugbreak();\n"
    "\tExitThread(0);\n"
    "}\n\n"
    ;
*/
    "__declspec(noreturn) static void " ERR_CHECK_FUNCTION_NAME "(u32 uErrCheckIndex, const u32* tValues) {\n"
    "\tconst u32* pVal = tValues + uErrCheckIndex * 7u;\n"
    "\t" ERR_REPORT_FUNC_NAME "(u8(pVal[0]), int(pVal[1]), pVal[2], pVal[3], pVal[4], pVal[5], pVal[6]);\n"
    "\t__debugbreak();\n"
    "\tExitThread(0);\n"
    "}\n\n"
    ;

#endif

// TODO: not hardcoded...
#define IR_DUMP_HARDCODED_FILENAME   "ir_dump.txt"

local_func bool process_whole_backend(WholeProgramCompilationState* progCompilationState,
                           LocLib_OS_WrapperFunctions* pOsFuncs,
                           LocLib_CompilationParams* pCompilationParams,
                           LocLib_CompilationResults* oCompilationResults,
                           WorkerDesc* pWorker, Arena secondaryTmpArena)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Entering Backend Phases"), pWorker);

    {
        platform_log_info("Writing IR-dump to '" IR_DUMP_HARDCODED_FILENAME "'");
        
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Now writing IR-dump to '%s'", reinterpret_cast<u64>(IR_DUMP_HARDCODED_FILENAME)), pWorker);
        EFileOpenErr err;
        PlatformFileHandle dumpFile = platform_open_file_for_writing(IR_DUMP_HARDCODED_FILENAME, &err);
        {
            Assert_(dumpFile);
            u32 uSourceFileCount = progCompilationState->vecSourceFiles.size();
            for (u32 uFile = 0; uFile < uSourceFileCount; uFile++) {

                SourceFileDescAndState* pSourceFile = progCompilationState->vecSourceFiles[uFile];

                IRAwareContext context;
                context.pIsolatedSourceFile = pSourceFile;
                context.pCompilationParams = pCompilationParams;
                context.pWorker = pWorker;
                context.pProgCompilationState = progCompilationState;
                context.pRepo = &(pSourceFile->filewiseConstRepo);
                context.pTmpRepo = 0;

                ir_dump_to_file(dumpFile, &context);
            }

            platform_close_file(dumpFile);
        }
    }

    // TODO ?? add some 'hardcoded' implementations of a few things *in LOC*, to add to the set of stuff to include there ?
    // TODO: or maybe even, not *hardcoded* ?
    // --> The code around invocation of main ? the global error handler ? other stuff such as core types, or even decl of 'builtins' ??

    // TODO: accept compilation as library, by adding all "public" declarations to the sets of entities
    //   touched by existence requirement, instead of starting from 'main'

    TCProcBodyResult* pBodyOfMain = backend_find_proc_main(progCompilationState, pWorker);
    if (pBodyOfMain) {
        u64 uIRofMain = ir_make_procbody_ref_in_file(u32(pBodyOfMain->iSourceFileIndex), pBodyOfMain->uRegistrationIndex);
        const char* szDirectExe = "testWinX64Backend.exe";

        platform_log_info("Emitting directly to final binary as '", false);
        platform_log_info(szDirectExe, false);
        platform_log_info("'\n", false);

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Emitting directly to final binary as '%s'", reinterpret_cast<u64>(szDirectExe)), pWorker);

        EFileOpenErr err;
        PlatformFileHandle directExe = platform_open_file_for_writing(szDirectExe, &err);
        if (directExe) {
            Arena secondaryTmpArena;
            init_arena(&(secondaryTmpArena), progCompilationState->globalProvider);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Emitting to an x64 PE file for windows platform, as-required from proc 'main'"), pWorker);
            u32 uRetValue = winx64_backend_emit_all_to(directExe, uIRofMain, progCompilationState, pWorker, secondaryTmpArena);
            return uRetValue > 0;
        }  else {
            // TODO: report error
            platform_log_error("*** could not open for writing : ", false);
            platform_log_info(szDirectExe, true);
        }
    } else {
        // TODO: report error
        platform_log_error("*** could not find entry point: 'main'", true);
    }

    return true;
}

#endif // LOCLIB_PROGRAM_H_

