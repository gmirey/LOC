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

#define CHECK_ASSERTS                   true

#define DEBUG_PREPARSER_NODECOUNTS      true
#define ASSERT_FALSE_ON_OOM             true

#define DEBUG_ARENAS_ALLOC_PRINTLEVEL       0

#if 1

// TMP TMP TMP
#define TRACE_PRE_PARSER_PRINTLEVEL         1   // TODO: replace traces there with new system 
// TMP TMP TMP

#define DISABLE_ALL_TRACES                          0

#define TRACABLE_EVENTS_WITH_RDTSC                  1
#define TRACABLE_EVENTS_KEEP_FULL_LOG               1
#define TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG     1
#define LOG_TRACES_IN_FILE                          "trace_report.log"

#define TRACE_ORCH_PRINTLEVEL               1

#define TRACE_IORD_PRINTLEVEL               1
#define TRACE_IOWR_PRINTLEVEL               1

#define TRACE_SCAN_PRINTLEVEL               1
#define TRACE_TOKN_PRINTLEVEL               1

#define TRACE_PRMG_PRINTLEVEL               1
#define TRACE_PPAR_PRINTLEVEL               1
#define TRACE_PSTP_PRINTLEVEL               1

#define TRACE_TCMG_PRINTLEVEL               1
#define TRACE_EXPR_PRINTLEVEL               1
#define TRACE_CAST_PRINTLEVEL               1

#define TRACE_SOLV_PRINTLEVEL               1
#define TRACE_IREM_PRINTLEVEL               1
#define TRACE_OPTI_PRINTLEVEL               1

#define TRACE_REPT_PRINTLEVEL               1

#define TRACE_BCKP_PRINTLEVEL               1
#define TRACE_BCKE_PRINTLEVEL               1
#define TRACE_BCKL_PRINTLEVEL               1

#else

// TMP TMP TMP
#define TRACE_PRE_PARSER_PRINTLEVEL         4 // TODO: replace traces there with new system 
// TMP TMP TMP

#define DISABLE_ALL_TRACES                          0

#define TRACABLE_EVENTS_WITH_RDTSC                  1
#define TRACABLE_EVENTS_KEEP_FULL_LOG               1
#define TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG     1
#define LOG_TRACES_IN_FILE                          "trace_report.log"

#define TRACE_ORCH_PRINTLEVEL               9

#define TRACE_IORD_PRINTLEVEL               9
#define TRACE_IOWR_PRINTLEVEL               9

#define TRACE_SCAN_PRINTLEVEL               9
#define TRACE_TOKN_PRINTLEVEL               9

#define TRACE_PRMG_PRINTLEVEL               9
#define TRACE_PPAR_PRINTLEVEL               9
#define TRACE_PSTP_PRINTLEVEL               9

#define TRACE_TCMG_PRINTLEVEL               9
#define TRACE_EXPR_PRINTLEVEL               9
#define TRACE_CAST_PRINTLEVEL               9

#define TRACE_SOLV_PRINTLEVEL               9
#define TRACE_IREM_PRINTLEVEL               9
#define TRACE_OPTI_PRINTLEVEL               9

#define TRACE_REPT_PRINTLEVEL               9

#define TRACE_BCKP_PRINTLEVEL               9
#define TRACE_BCKE_PRINTLEVEL               9
#define TRACE_BCKL_PRINTLEVEL               9

#endif

#include "LocLib_Cmd_API.h"

#include "LocLib_Events.h"
#include "LocLib_Worker.h"

#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_ProgramState.h"
#include "LocLib_Localization.h"
#include "LocLib_Type_Registration.h"
#include "LocLib_Orchestrator.h"
#include "LocLib_Program.h"
#include "LocLib_IR_BlockGraph.h"

#ifndef ALWAYS_DEFINED_TRICK
#include "../../bak/TmpParserState.h.bak"
#endif

// Called by the 'user interface' to the compiler DLL; to know about errors
exported_func_impl bool hasErrors(WholeProgramCompilationState* pProgCompState)
{
    u32 uSourceFileCount = pProgCompState->vecSourceFiles.size();
    for (u32 uFile = 0; uFile < uSourceFileCount; uFile++) {
        SourceFileDescAndState* pSourceFile = pProgCompState->vecSourceFiles[uFile];
        if (pSourceFile->vecErrors.size())
            return true;
    }
    if (pProgCompState->vecBackendErrors.size())
        return true;
    return false;
}

// Intended ? but no longer... called by the 'user interface' to the compiler DLL. Now we pack up those results ourselves by calling that.
exported_func_impl void prepare_results(WholeProgramCompilationState* pProgCompState,
	LocLib_OS_WrapperFunctions* pOsFuncs, 
	LocLib_CompilationParams* pCompilationParams,
    LocLib_CompilationResults* oCompilationResults)
{
    u32 uErrorCount = 0;
    int iFirstFileWithError = -1;

    u32 uSourceFileCount = pProgCompState->vecSourceFiles.size();
    oCompilationResults->uFileCount = uSourceFileCount;
    oCompilationResults->tFiles = (SourceFileDescAndState**)alloc_from(pProgCompState->globalArena,
        sizeof(SourceFileDescAndState*) * uSourceFileCount, alignof(SourceFileDescAndState*));

    for (u32 uFile = 0; uFile < uSourceFileCount; uFile++) {
        SourceFileDescAndState* pSourceFile = pProgCompState->vecSourceFiles[uFile];
        oCompilationResults->tFiles[uFile] = pSourceFile;
        if (pSourceFile->vecErrors.size()) {
            uErrorCount += pSourceFile->vecErrors.size();
            if (iFirstFileWithError == -1)
                iFirstFileWithError = int(uFile);
        }
    }
    if (pProgCompState->vecBackendErrors.size()) {
        uErrorCount += pProgCompState->vecBackendErrors.size();
        if (iFirstFileWithError == -1)
            iFirstFileWithError = -2;
    }
    oCompilationResults->uErrorCount = uErrorCount;
    oCompilationResults->uWarningCount = 0; // TODO
    oCompilationResults->iFirstFileWithError = iFirstFileWithError;
}

#if defined(LOG_TRACES_IN_FILE) && TRACABLE_EVENTS_KEEP_FULL_LOG
    #define prepare_results_and_return(what) do { \
        prepare_results(pProgCompilationState, pOsFuncs, pCompilationParams, oCompilationResults); \
        log_traces_to_file(pProgCompilationState, &mainWorker, LOG_TRACES_IN_FILE); /* TODO: multi-worker */ \
        return what; \
    } while (0)
#else
    #define prepare_results_and_return(what) do { \
        prepare_results(pProgCompilationState, pOsFuncs, pCompilationParams, oCompilationResults); \
        return what; \
    } while (0)
#endif


// logs some single event in our logs to a buffer... intented for logging to file in the end: impl detail of 'log_traces_to_file', below
local_func_inl u32 log_event_to_buffer(char* pBuffer, const TracableEvent& eventToLog, u8 uIndent)
{
    int iCharsWritten = 0;
    #if TRACABLE_EVENTS_WITH_RDTSC
        iCharsWritten += sprintf(pBuffer, "%020llu: ", eventToLog.uRdtsc);
    #endif
    u32 uNumSpaces = uIndent * 2u;
    memset(pBuffer + iCharsWritten, int(' '), uNumSpaces);
    iCharsWritten += i32(uNumSpaces);
    print_event_to(pBuffer + iCharsWritten, eventToLog);
    iCharsWritten += strlen(pBuffer + iCharsWritten);
    pBuffer[iCharsWritten] = '\n';
    iCharsWritten++;
    pBuffer[iCharsWritten] = 0;
    return u32(iCharsWritten);
}

// logs all events in our log to a file.
// TODO: a multi-worker version
local_func void log_traces_to_file(WholeProgramCompilationState* pProgState, WorkerDesc* pWorker, StringView fileName) {

    static_assert(TRACABLE_EVENTS_KEEP_FULL_LOG, "allo?");

    platform_log_info("Writing log report to '", false);
    platform_log_info(fileName, false);
    platform_log_info("'", true);

    EFileOpenErr err;
    PlatformFileHandle logFile = platform_open_file_for_writing(fileName, &err);
    if (!logFile) {
        platform_log_error("Could not open log-file for writing: '", false);
        platform_log_info(fileName, false);
        platform_log_info("'", true);
        return;
    }

    u8 uCurrentIndent = 0u;

    char szBuffer[2048];
    u32 uJobsCount = pWorker->vecFullLogs.size();
    for (u32 uJob = 0u; uJob < uJobsCount; uJob++) {
        const StableGrowingVector<TracableEvent>& vecEvents = pWorker->vecFullLogs[uJob];
        u32 uEventsCount = vecEvents.size();
        for (u32 uEvent = 0u; uEvent < uEventsCount; uEvent++) {
            const TracableEvent& eventToLog = vecEvents[uEvent];
            #if TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG
                if (u8(eventToLog.eLocPhase) & 0x80u) { // a 'pop'
                    Assert_(uCurrentIndent);
                    uCurrentIndent--;
                } else {
            #endif
                    Assert_(uCurrentIndent < LOC_WORKER_TRACE_STACKSIZE);
                    u32 uWrittenChars = log_event_to_buffer(szBuffer, eventToLog, uCurrentIndent);
                    platform_write_to_file(logFile, (u8*)szBuffer, uWrittenChars);
            #if TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG
                    uCurrentIndent++;
                }
            #endif
        }
    }

    platform_close_file(logFile);
}

// ***************************************************************************************
// Currently our main "entry point" for our compiler DLL, called from our user-interface.
//    Yes, we intend to be able to run compiler from elsewhere. IDE partial run on a file... dunno, something.
// ***************************************************************************************
exported_func_impl bool run_loc_compiler_from_first_file(
    int iFirstFileIndex,
	LocLib_OS_WrapperFunctions* pOsFuncs, 
	LocLib_CompilationParams* pCompilationParams,
    LocLib_CompilationResults* oCompilationResults)
{
    // Any 0 on required input parameters from user code would instantly fail (without an error report, thus returning false)
    if (    iFirstFileIndex < 0 || !pOsFuncs || !pCompilationParams || !oCompilationResults ||
            !pOsFuncs->pGetSourceFileIndexFn || !pOsFuncs->pGetSourceFileFullNameByteCountFn || !pOsFuncs->pGetSourceFileFullNameFn ||
            !pOsFuncs->pGetSourceFileTokenizerFn || !pOsFuncs->pFreeSourceFileTokenizerFn ||
            !pOsFuncs->pTryAcquireSourceFileReaderResourceFn || !pOsFuncs->pBlockingAcquireSourceFileReaderResourceFn || !pOsFuncs->pReleaseSourceFileReaderResourceFn)
        return false;

    oCompilationResults->uErrorCount = 0;
    oCompilationResults->uWarningCount = 0;
    oCompilationResults->uFileCount = 0;
    oCompilationResults->iFirstFileWithError = -1;
    oCompilationResults->tFiles = 0;

    u32 uReqForProvider = align_to(8, sizeof(ChunkProvider)) >> 3;
    ChunkProvider* pPersistentProvider = (ChunkProvider*)(pCompilationParams->tPersistentStorage);
    Arena* pPersistentArena = (Arena*)(pCompilationParams->tPersistentStorage + uReqForProvider);

    init_chunk_provider(pPersistentProvider);
    init_arena(pPersistentArena, *pPersistentProvider);
    WholeProgramCompilationState* pProgCompilationState = (WholeProgramCompilationState*)alloc_from(*pPersistentArena,
        sizeof(WholeProgramCompilationState), alignof(WholeProgramCompilationState));

    if (!init_program_compilation_state(pProgCompilationState, pOsFuncs, pCompilationParams, oCompilationResults)) {
        prepare_results(pProgCompilationState, pOsFuncs, pCompilationParams, oCompilationResults);
        return true;
    }

    Arena tmpArena;
    init_arena(&tmpArena, pProgCompilationState->globalProvider);

    // TODO: multi-thread this eventually
    //  - with a single-worker able to parse
    //  - with multiple workers for TC...
    //      yet our Task scheme will ensure only a single TC thread working on a single source file at any time.
    WorkerDesc mainWorker;
    Arena mainWorkerTmpArena;
    Arena mainWorkerFullLogArena;
    init_arena(&mainWorkerTmpArena, pProgCompilationState->globalProvider); // TODO: 1 distinct provider per thread
    init_arena(&mainWorkerFullLogArena, pProgCompilationState->globalProvider);
    init_worker(&mainWorker, mainWorkerTmpArena, 0u, 1u, 1u, mainWorkerFullLogArena);

    // We then define and init all type ids
    {
        on_set_newjob_to_worker(&mainWorker);
        // TODO: timing
        u8 uSysHour = 0u;
        u8 uSysMin = 0u;
        u8 uSysSec = 0u;
        u64 uSecondsSinceStart = 0uLL;
        u32 uNanoSecondsSinceStart = 0u;
        BLOCK_TRACE(ELOCPHASE_ORCHESTRATION, _LLVL2_IMPORTANT_INFO, EventORCH_WORKER_ASSIGN(
            mainWorker.uLocThreadId, uSysHour, uSysMin, uSysSec, uSecondsSinceStart, uNanoSecondsSinceStart), &mainWorker);

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Initializing core types and reserved words"), &mainWorker);

        init_type_info_for_core_types(pProgCompilationState, &mainWorker);
        // Only then, can we assign reserved words to their values (lots of them will indeed point to types ids we just defined)
        pProgCompilationState->init_reserved_words_values(&mainWorker);
    }

    if (!register_source_file(iFirstFileIndex, -1, -1, pProgCompilationState, pOsFuncs, pCompilationParams, oCompilationResults)) {
        prepare_results_and_return(true);
    }

    Assert_(iFirstFileIndex >= 0 && iFirstFileIndex < (int)pProgCompilationState->vecSourceFiles.size());
    SourceFileDescAndState* pSourceFileDesc = pProgCompilationState->vecSourceFiles[iFirstFileIndex];
    Assert_(pSourceFileDesc->iBlockCount == -1); // otherwise, has already been done!
    
    on_set_newjob_to_worker(&mainWorker);

    ParseFileTaskDescription firstToBeParsed = {};
    firstToBeParsed.iIndexOfDemandingSourceFile = -1;
    pProgCompilationState->mapOfParseSourceFileTasksToLaunch.insert(iFirstFileIndex, firstToBeParsed);

    {
        u32 uWorkerCount = 1u; // TODO: add other parallel workers
        BLOCK_TRACE(ELOCPHASE_ORCHESTRATION, _LLVL2_IMPORTANT_INFO, EventORCH_MAIN_FRONTEND_LOOP(uWorkerCount), &mainWorker);
        run_parse_and_tc_worker(&mainWorker, pProgCompilationState, oCompilationResults);
    }
    // TODO: join

    #if 0
    // TODO: Check all after-the-fact identifier collisions between local contexts and namespace,
    //   or within a namespace, taking all 'using', and parents, and 'using' of parents... into account.

    // Unsolved identifiers report pass
    for (auto itFileWithUnsolved = pProgCompilationState->setOfFilesWithTCWaitingTasks.begin(),
              itEnd = pProgCompilationState->setOfFilesWithTCWaitingTasks.end() ; itFileWithUnsolved != itEnd; itFileWithUnsolved++)
    {
        // TODO: graph-sort those unsolved declarations: ie, if one declaration error prevents resolution of another declaration,
        //   then make sure the latter is only reported after the former

        SourceFileDescAndState* pSourceFile = pProgCompilationState->vecSourceFiles[*itFileWithUnsolved];
        Assert_(pSourceFile->mapWaitingTasksByReason.size());
        for (auto itWaitingReason = pSourceFile->mapWaitingTasksByReason.begin(),
                  itEndReason = pSourceFile->mapWaitingTasksByReason.end() ; itWaitingReason != itEndReason; itWaitingReason++)
        {
            if (itWaitingReason.key().getWaitingReason() == EWR_GLOBAL_IDENTIFIER_RESOLUTION)
                emit_unsolved_identifier_errors(itWaitingReason.value());
        }
    }

    // Other still-sleeping-task errors report pass
    for (auto itFileWithUnsolved = pProgCompilationState->setOfFilesWithTCWaitingTasks.begin(),
              itEnd = pProgCompilationState->setOfFilesWithTCWaitingTasks.end() ; itFileWithUnsolved != itEnd; itFileWithUnsolved++)
    {
        SourceFileDescAndState* pSourceFile = pProgCompilationState->vecSourceFiles[*itFileWithUnsolved];
        Assert_(pSourceFile->mapWaitingTasksByReason.size());
        for (auto itWaitingReason = pSourceFile->mapWaitingTasksByReason.begin(),
                  itEndReason = pSourceFile->mapWaitingTasksByReason.end() ; itWaitingReason != itEndReason; itWaitingReason++)
        {
            switch (itWaitingReason.key().getWaitingReason()) {

                case EWR_GLOBAL_IDENTIFIER_RESOLUTION: {
                    // NOOP : already reported on previous pass
                } break;

                case EWR_PROC_BODY_TYPECHECK_SUCCESS: {
                    emit_unfinished_tc_procbody_errors(itWaitingReason.value());
                } break;

                case EWR_COMPOUND_BODY_TYPECHECK_SUCCESS: {
                    emit_unfinished_tc_compound_errors(itWaitingReason.value());
                } break;

                case EWR_OVERLOAD_GATHER_MORE: {
                    emit_unfinished_gathermore_errors(itWaitingReason.value());
                } break;

                default: Assume_(false);
            }
        }
    }

    #endif

    {
        on_set_newjob_to_worker(&mainWorker);

        // TODO: timing
        u8 uSysHour = 0u;
        u8 uSysMin = 0u;
        u8 uSysSec = 0u;
        u64 uSecondsSinceStart = 0uLL;
        u32 uNanoSecondsSinceStart = 0u;
        BLOCK_TRACE(ELOCPHASE_ORCHESTRATION, _LLVL2_IMPORTANT_INFO, EventORCH_WORKER_ASSIGN(
            mainWorker.uLocThreadId, uSysHour, uSysMin, uSysSec, uSecondsSinceStart, uNanoSecondsSinceStart), &mainWorker);

        if (!hasErrors(pProgCompilationState)) {
            if (!process_whole_backend(pProgCompilationState, pOsFuncs, pCompilationParams, oCompilationResults, &mainWorker, tmpArena))
                prepare_results_and_return(true); // process_whole_backend only returns false in case of irrecoverable error...
        }
    }

    prepare_results_and_return(true);
}


exported_func_impl const char* write_error_report_stub(
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs)
{
    // TODO: much better than this
    if (pError->errCode < BASE_OF_SCANNER_ERRORS) {
        switch (pError->errCode) {
        case IERR_UNKNOWN_TARGET:
            return "Unknown compilation target";
        case IERR_FILEOPEN_INVALID_PARAM:
            return "Source File Open: Invalid params";
        case IERR_FILEOPEN_DIRECTORY_NOT_FOUND:
            return "Source File Open: Directory not found";
        case IERR_FILEOPEN_CANNOT_ACCESS_DIRECTORY:
            return "Source File Open: Cannot access directory";
        case IERR_FILEOPEN_FILE_NOT_FOUND:
            return "Source File Open: File not found";
        case IERR_FILEOPEN_UNAUTHORIZED_READ:
            return "Source File Open: Unauthorized read";
        case IERR_FILEOPEN_FAILED_READ:
            return "Source File Open: Failed read";
        case IERR_FILEOPEN_OUT_OF_MEMORY:
            return "Source File Open: Out of memory for buffering file contents";
        case IERR_FILEOPEN_FILE_TOO_BIG:
            return "Source File Open: Source file too big (>1GB)";
        default:
            return "<unexpected error-code, category: general compilation parameters error>";
        }
    } else if (pError->errCode < BASE_OF_TOKENIZER_ERRORS) {       // from BASE_OF_SCANNER_ERRORS
        switch (pError->errCode) {
        case SERR_UNALLOWED_BOM_UTF16:
            return "Found UTF16 BOM (Byte-Order-Mark) at start of source file. Only allows UTF8 format";
        case SERR_UNALLOWED_BOM_UTF32:
            return "Found UTF32 BOM (Byte-Order-Mark) at start of source file. Only allows UTF8 format";
        case SERR_UNALLOWED_BOM_UTF16BE:
            return "Found big-endian UTF16 BOM (Byte-Order-Mark) at start of source file. Only allows UTF8 format";
        case SERR_UNALLOWED_BOM_UTF32BE:
            return "Found big-endian UTF32 BOM (Byte-Order-Mark) at start of source file. Only allows UTF8 format";
        case SERR_FOUND_CHARACTER_ZERO:
            return "Found byte or character-code zero (0 = 0x00) within source file. This is not allowed";
        case SERR_ISOLATED_CR_CHARACTER:
            return "Found isolated CR (Carriage Return) character (13 = 0x0D) within source file. Line-endings must be either LF, or CR-LF";
        case SERR_INVALID_UTF8_ENCODING:
            return "Found invalid UTF8 encoding within source file. Parsing this file any further is disallowed.";
        case SERR_TRUNCATED_UTF8_CHARACTER:
            return "Found truncated UTF8 encoding at end of source file.";
        case SERR_INVALID_EOF:
            return "Found invalid ASCII control character for EOF (End-Of-File,  = ) within source file.";
        case SERR_LINE_TOO_LONG:
            return "Line too long within source file. Max allowed bytes on a line is 1024. Please properly use Line Endings (either LF or CR-LF).";
        case SERR_TOO_MANY_LINES_IN_SINGLE_FILE:
            return "Too many lines for a single source file. Max allowed is around 4 millions.";
        default:
            return "<unexpected error-code, category: file-scanner error>";
        }

    } else if (pError->errCode < BASE_OF_PARSER_ERRORS) {       // from BASE_OF_TOKENIZER_ERRORS
        switch (pError->errCode) {
        case TERR_INVALID_EXTENDED_UTF8_IN_CODE:
            return "Found non-ASCII character within code (interpreted as UTF8). This is only allowed in comments or string literals.";
        case TERR_INVALID_CHARACTER_IN_CODE:
            return "Found invalid character within code, ie. outside comment block or string literal.";
        case TERR_INVALID_CHARACTER_FOLLOWING_NUMBER_LITERAL:
            return "Found invalid character following a number literal.";
        case TERR_NO_VALID_DIGIT_AFTER_NUMBER_LITERAL_BASE_PREFIX:
            return "Found no valid digit after prefix characters denoting a numerical base (0x, 0b...).";
        case TERR_NO_EXPONENT_DIGIT_AFTER_EXPONENT_CHARACTER_IN_NUMERIC_LITERAL:
            return "Found no valid exponent digit after exponent character in numeric literal.";
        case TERR_OVERSHOT_MAX_EXPONENT_DIGITS_IN_NUMERIC_LITERAL:
            return "Overshot the maximum number of five digits for expressing exponent in numeric literal.";
        case TERR_UNCLOSED_STRING_LITERAL_BEFORE_EOL:
            return "A string literal was found still-opened when reaching line-ending in source file";
        case TERR_STRING_IS_NOT_SINGLE_CODEPOINT:
            return "A codepoint literal failed to contain a single code point";
        case TERR_INVALID_INDENTATION_CHARACTER:
            return "An indentation character was invalid. All meaningfull indentations at start of line must consist of tabulation characters (8 = 0x08)";
        case TERR_TOO_MANY_INDENTATION_CHARACTERS:
            return "Too many indentation characters. Max allowed is 255";
        default:
            return "<unexpected error-code, category: tokenizer error>";
        }

    } else if (pError->errCode < BASE_OF_RESOLUTION_ERRORS) {   // from BASE_OF_PARSER_ERRORS
        switch (pError->errCode) {
        case PERR_INVALID_MULTILINE_INDENTATION_MAYBESTATEMENT:
        case PERR_INVALID_MULTILINE_INDENTATION_MAYBESUB:
        case PERR_INVALID_MULTILINE_INDENTATION_NORECOVERY:
            return "Multiline indentation found when none was expected";
        case PERR_INVALID_SUBBLOCK_INDENTATION_MAYBECONTINUATION:
        case PERR_INVALID_SUBBLOCK_INDENTATION_MAYBESAME:
        case PERR_INVALID_SUBBLOCK_INDENTATION_NORECOVERY:
            return "Sub-block indentation found when none was expected";
        case PERR_UNEXPECTED_BLOCK_ENDING_AND_WRONG_INDENT:
        case PERR_UNEXPECTED_BLOCK_ENDING:
        case PERR_MISSING_BLOCK_ENDING_MAYBE_WRONG_INDENT:
        case PERR_MISSING_BLOCK_ENDING:
            return "Block-ending error";
        case PERR_EXPECTED_CHILD_BLOCK_MISSING:
            return "Expected child-block not found, or line indentation error";
        case PERR_EXPECTED_LINE_CONTINUATION_FOUND_WRONG_INDENT:
            return "Expected line continuation, found line at wrong indent";
        case PERR_EXPECTED_LINE_CONTINUATION_FOUND_NONE:
            return "Expected line continuation, found none";
        case PERR_TOKEN_ON_LINE_AFTER_EXPLICIT_LINEBREAK:
            return "Found remaining token on line after explicit linebreak";
        case PERR_EXPRESSION_TOO_DEEP:
            return "Expression too deep.";
        case PERR_TOO_MANY_ELEMENTS_IN_LIST:
            return "Too many elements in expression list.";
        case PERR_TOO_MANY_AST_NODES_IN_STATEMENT:
            return "Too many AST nodes in statement or inlined sequence.";
        case PERR_UNEXPECTED_TOKEN:
            return "Unexpected token found.";
        case PERR_EXPECTED_TOKEN_FOUND_EOL:
            return "Expected token, found Line-ending.";
        case PERR_EXPECTED_TOKEN_FAILED_MULTILINE_SEARCH:
            return "Expected token, failure of multi-line search.";
        case PERR_FAILED_INTEGRATION_OF_LITERAL:
            return "Integration of literal as a compiler constant failed.";
        case PERR_FLOATING_POINT_LITERAL_OVERFLOW:
            return "Floating-point literal overflow. Should explicitely replace with 'infinity'";
        case PERR_FLOATING_POINT_LITERAL_UNDERFLOW:
            return "Floating-point literal underflow. Should explicitely replace with zero (0.0)";
        case PERR_TOO_MANY_LINES_IN_SAME_STATEMENT_OR_INLINED_CHAIN:
            return "Too many lines in statement or inlined sequence.";
        case PERR_COMPTIME_NOT_ALLOWED_THERE:
            return "Compilation-time token '$' is not allowed there.";
        case PERR_FORBIDDEN_INLINING_OF_PAN_DIRECTIVE:
            return "Such #-directives cannot be part of an inlined code sequence.";
        case PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT:
            return "Expected-paired statement missing its matching statement.";
        case PERR_MODIFIER_NOT_ALLOWED_WITHIN_MODIFIER_PARAMETERS:
            return "Use of expression-modifiers within modifier parameters is not allowed.";
        case PERR_DANGLING_MODIFIER_ONLY_LINE:
            return "Modifier-only line expected following statement, but was last in its block.";
        case PERR_EXPECTED_EXPRESSION:
            return "Expected expression";
        case PERR_EXPECTED_SIMPLE_EXPRESSION:
            return "Expected single, simple expression";
        default:
            return "<unexpected error-code, category: parser error>";
        }
    } else if (pError->errCode < BASE_OF_TYPECHECK_ERRORS) {    // from BASE_OF_RESOLUTION_ERRORS
        switch (pError->errCode) {
        case RERR_INVALID_PATH_FORMAT:
            return "Invalid Path format";
        case RERR_INVALID_PATH_REF_TO_PARENT_DIRECTORY_BEYOND_ROOT:
            return "Invalid Path: ref to parent directory goes beyond root";
        case RERR_ALREADY_DECLARED_IDENTIFIER:
            return "Identifier was already declared";
        case RERR_UNRESOLVED_IDENTIFIER:
            return "Unresolved identifier";
        case RERR_STILL_SLEEPING_WAITPROCBODY:
            return "Some procedures have not been fully typechecked";
        case RERR_STILL_SLEEPING_WAITCOMPOUND:
            return "Some compounds have not been fully typechecked";
        case RERR_STILL_SLEEPING_WAITGATHERMORE:
            return "Possibly circular source-code references";
        default:
            return "<unexpected error-code, category: resolution error>";
        }
    } else if (pError->errCode < BASE_OF_VALIDATION_ERRORS) {   // from BASE_OF_TYPECHECK_ERRORS
        switch(pError->errCode) {
        case CERR_EXPRESSION_WHEN_EXPECTING_NON_SEQ_STATEMENT: return "CERR_EXPRESSION_WHEN_EXPECTING_NON_SEQ_STATEMENT";
        case CERR_EXPRESSION_WHEN_EXPECTING_CTEVAL_STATEMENT: return "CERR_EXPRESSION_WHEN_EXPECTING_CTEVAL_STATEMENT";
        case CERR_EXPRESSION_WHEN_EXPECTING_STATEMENT: return "CERR_EXPRESSION_WHEN_EXPECTING_STATEMENT";
        case CERR_EXPRESSION_NOT_REFERENCABLE: return "CERR_EXPRESSION_NOT_REFERENCABLE";
        case CERR_EXPECTED_ASSIGNABLE_EXPRESSION: return "CERR_EXPECTED_ASSIGNABLE_EXPRESSION";
        case CERR_CANNOT_DEREF_PROC_BODY: return "CERR_CANNOT_DEREF_PROC_BODY";
        case CERR_EXPECTED_PROC_DECLARATION: return "CERR_EXPECTED_PROC_DECLARATION";
        case CERR_EXPECTED_POINTER: return "CERR_EXPECTED_POINTER";
        case CERR_EXPECTED_CONSTANT: return "CERR_EXPECTED_CONSTANT";
        case CERR_EXPECTED_FULLY_KNOWN_CONSTANT: return "CERR_EXPECTED_FULLY_KNOWN_CONSTANT";
        case CERR_EXPECTED_EXPRESSION: return "CERR_EXPECTED_EXPRESSION";
        case CERR_INVALID_DOT_DESCENT: return "CERR_INVALID_DOT_DESCENT";
        case CERR_MULTIPARAM_INVOCATION_WHEN_EXPECTED_SINGLE_EXPRESSION: return "CERR_MULTIPARAM_INVOCATION_WHEN_EXPECTED_SINGLE_EXPRESSION";
        case CERR_EXPECTED_GLOBAL_SCOPE: return "CERR_EXPECTED_GLOBAL_SCOPE";

        case CERR_LITERAL_WHEN_EXPECTING_NON_SEQ_STATEMENT: return "CERR_LITERAL_WHEN_EXPECTING_NON_SEQ_STATEMENT";
        case CERR_LITERAL_WHEN_EXPECTING_CTEVAL_STATEMENT: return "CERR_LITERAL_WHEN_EXPECTING_CTEVAL_STATEMENT";
        case CERR_LITERAL_WHEN_EXPECTING_STATEMENT: return "CERR_LITERAL_WHEN_EXPECTING_STATEMENT";
        case CERR_LITERAL_WHEN_EXPECTING_ASSIGNABLE_EXPRESSION: return "CERR_LITERAL_WHEN_EXPECTING_ASSIGNABLE_EXPRESSION";

        case CERR_SPECIAL_WHEN_EXPECTING_NON_SEQ_STATEMENT: return "CERR_SPECIAL_WHEN_EXPECTING_NON_SEQ_STATEMENT";
        case CERR_SPECIAL_WHEN_EXPECTING_CTEVAL_STATEMENT: return "CERR_SPECIAL_WHEN_EXPECTING_CTEVAL_STATEMENT";
        case CERR_SPECIAL_WHEN_EXPECTING_STATEMENT: return "CERR_SPECIAL_WHEN_EXPECTING_STATEMENT";
        case CERR_SPECIAL_WHEN_EXPECTING_ASSIGNABLE_EXPRESSION: return "CERR_SPECIAL_WHEN_EXPECTING_ASSIGNABLE_EXPRESSION";
        case CERR_SPECIAL_WHEN_EXPECTING_EXPRESSION: return "CERR_SPECIAL_WHEN_EXPECTING_EXPRESSION";

        case CERR_EXPRLIST_WHEN_EXPECTING_NON_SEQ_STATEMENT: return "CERR_EXPRLIST_WHEN_EXPECTING_NON_SEQ_STATEMENT";
        case CERR_EXPRLIST_WHEN_EXPECTING_CTEVAL_STATEMENT: return "CERR_EXPRLIST_WHEN_EXPECTING_CTEVAL_STATEMENT";
        case CERR_EXPRLIST_WHEN_EXPECTING_STATEMENT: return "CERR_EXPRLIST_WHEN_EXPECTING_STATEMENT";
        case CERR_EXPRLIST_WHEN_EXPECTING_SIMPLE_ASSIGNABLE_EXPRESSION: return "CERR_EXPRLIST_WHEN_EXPECTING_SIMPLE_ASSIGNABLE_EXPRESSION";
        case CERR_EXPRLIST_WHEN_EXPECTING_SIMPLE_EXPRESSION: return "CERR_EXPRLIST_WHEN_EXPECTING_SIMPLE_EXPRESSION";

        case CERR_SEQ_STATEMENT_WHEN_EXPECTING_NON_SEQ_STATEMENT: return "CERR_SEQ_STATEMENT_WHEN_EXPECTING_NON_SEQ_STATEMENT";
        case CERR_SEQ_STATEMENT_WHEN_EXPECTING_CTEVAL_STATEMENT: return "CERR_SEQ_STATEMENT_WHEN_EXPECTING_CTEVAL_STATEMENT";
        case CERR_SEQ_STATEMENT_WHEN_EXPECTING_LHV_EXPRESSION: return "CERR_SEQ_STATEMENT_WHEN_EXPECTING_LHV_EXPRESSION";
        case CERR_SEQ_STATEMENT_WHEN_EXPECTING_EXPRESSION: return "CERR_SEQ_STATEMENT_WHEN_EXPECTING_EXPRESSION";

        case CERR_USER_TYPE_DEFINITION_AS_NON_CONSTANT: return "CERR_USER_TYPE_DEFINITION_AS_NON_CONSTANT";
        case CERR_INVALID_FOREIGN_SPECIFICATION: return "CERR_INVALID_FOREIGN_SPECIFICATION";
        case CERR_FOREIGN_SPECIFICATION_SHALL_BE_ALONE: return "CERR_FOREIGN_SPECIFICATION_SHALL_BE_ALONE";
        case CERR_MULTIPLE_FOREIGN_SPECIFICATIONS_ON_PROC: return "CERR_MULTIPLE_FOREIGN_SPECIFICATIONS_ON_PROC";

        case CERR_NOT_EXPECTING_STATEMENT: return "CERR_NOT_EXPECTING_STATEMENT";
        case CERR_CANNOT_ASSIGN_TO_RESERVED_WORD: return "CERR_CANNOT_ASSIGN_TO_RESERVED_WORD";
        case CERR_CANNOT_DECLARE_RESERVED_WORD: return "CERR_CANNOT_DECLARE_RESERVED_WORD";
        case CERR_CANNOT_EVALUATE_SINK: return "CERR_CANNOT_EVALUATE_SINK";
        case CERR_CANNOT_EXPLICIT_NOINIT_AT_GLOBAL_SCOPE: return "CERR_CANNOT_EXPLICIT_NOINIT_AT_GLOBAL_SCOPE";
        case CERR_CANNOT_TYPE_INFER_FROM_EXPLICIT_NOINIT: return "CERR_CANNOT_TYPE_INFER_FROM_EXPLICIT_NOINIT";
        case CERR_CANNOT_COMPTIME_PREFIX_CONST_DECL: return "CERR_CANNOT_COMPTIME_PREFIX_CONST_DECL";
        case CERR_CANNOT_COMPTIME_PREFIX_THIS_NODE_KIND: return "CERR_CANNOT_COMPTIME_PREFIX_THIS_NODE_KIND";
        case CERR_PROC_OR_USER_TYPE_DECLARATION_REQUIRE_SINGLE_RHV: return "CERR_PROC_OR_USER_TYPE_DECLARATION_REQUIRE_SINGLE_RHV";
        case CERR_TYPE_TOO_LARGE: return "CERR_TYPE_TOO_LARGE";
        case CERR_COMPINT_TOO_LARGE: return "CERR_COMPINT_TOO_LARGE";
        case CERR_EXPECTED_NATURAL_NUMBER: return "CERR_EXPECTED_NATURAL_NUMBER";
        case CERR_EXPECTED_INTEGRAL_TYPE: return "CERR_EXPECTED_INTEGRAL_TYPE";
        case CERR_EXPECTED_NUMERIC_INTEGRAL_TYPE: return "CERR_EXPECTED_NUMERIC_INTEGRAL_TYPE";
        case CERR_EXPECTED_POINTER_SIZED_INTEGRAL_OR_LESS: return "CERR_EXPECTED_POINTER_SIZED_INTEGRAL_OR_LESS";
        case CERR_EXPECTED_SIZED_INTEGRAL_TYPE: return "CERR_EXPECTED_SIZED_INTEGRAL_TYPE";
        case CERR_EXPECTED_SIGNED_INTEGRAL_TYPE: return "CERR_EXPECTED_SIGNED_INTEGRAL_TYPE";
        case CERR_EXPECTED_FLOATING_POINT_TYPE: return "CERR_EXPECTED_FLOATING_POINT_TYPE";
        case CERR_EXPECTED_STRING: return "CERR_EXPECTED_STRING";
        case CERR_EXPECTED_STRING_OR_ARRAY_TYPE: return "CERR_EXPECTED_STRING_OR_ARRAY_TYPE";
        case CERR_EXPECTED_IDENTIFIER: return "CERR_EXPECTED_IDENTIFIER";
        case CERR_EXPECTED_TYPE: return "CERR_EXPECTED_TYPE";
        case CERR_STRUCT_OR_UNION_MEMBER_DECLARATION_MUST_BE_TYPE_ONLY: return "CERR_STRUCT_OR_UNION_MEMBER_DECLARATION_MUST_BE_TYPE_ONLY";
        case CERR_COMPOUND_TYPE_IN_ERROR: return "CERR_COMPOUND_TYPE_IN_ERROR";

        case CERR_SIGNALLING_NAN_CONSTANT_WAS_USED: return "CERR_SIGNALLING_NAN_CONSTANT_WAS_USED";

        case CERR_CANNOT_USE_COMPINT_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_COMPINT_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_FLOAT_LIT_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_FLOAT_LIT_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_ASTCODE_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_ASTCODE_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_ASTBLOCK_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_ASTBLOCK_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_BUILTIN_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_BUILTIN_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_NAMESPACE_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_NAMESPACE_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_SOURCEFILE_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_SOURCEFILE_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_FOREIGN_SOURCE_TYPE_AT_RUNTIME: return "CERR_CANNOT_USE_FOREIGN_SOURCE_TYPE_AT_RUNTIME";
        case CERR_CANNOT_USE_TYPE_OF_TYPES_AT_RUNTIME: return "CERR_CANNOT_USE_TYPE_OF_TYPES_AT_RUNTIME";
        case CERR_CANNOT_USE_STRUCTLIKE_FLAGGED_AS_COMPTIME_ONLY_AT_RUNTIME: return "CERR_CANNOT_USE_STRUCTLIKE_FLAGGED_AS_COMPTIME_ONLY_AT_RUNTIME";
        case CERR_CANNOT_USE_MACRO_OR_COMPTIMEPROC_AT_RUNTIME: return "CERR_CANNOT_USE_MACRO_OR_COMPTIMEPROC_AT_RUNTIME";
        case CERR_CANNOT_USE_PROCLIKE_OVERLOAD_AT_RUNTIME: return "CERR_CANNOT_USE_PROCLIKE_OVERLOAD_AT_RUNTIME";
        case CERR_CANNOT_USE_PROCLIKE_POLYMORPH_AT_RUNTIME: return "CERR_CANNOT_USE_PROCLIKE_POLYMORPH_AT_RUNTIME";
        case CERR_CANNOT_USE_COMPOUND_POLYMORPH_AT_RUNTIME: return "CERR_CANNOT_USE_COMPOUND_POLYMORPH_AT_RUNTIME";
        case CERR_UNKNOWN_WHEN_EXPECTED_KNOWN_CONSTANT: return "CERR_UNKNOWN_WHEN_EXPECTED_KNOWN_CONSTANT";
        case CERR_EXPECTED_TYPE_OR_PROCLIKE: return "CERR_EXPECTED_TYPE_OR_PROCLIKE";
        case CERR_CANNOT_USE_SINK_AS_LHV_OF_OP_AND_ASSIGN_STATEMENT: return "CERR_CANNOT_USE_SINK_AS_LHV_OF_OP_AND_ASSIGN_STATEMENT";
        case CERR_UNKOWN_MEMBER_IN_STRUCT: return "CERR_UNKOWN_MEMBER_IN_STRUCT";

        case CERR_CAST_REQUIRES_ONE_PARAMETER: return "CERR_CAST_REQUIRES_ONE_PARAMETER";
        case CERR_CAST_REQUIRES_SINGLE_PARAMETER: return "CERR_CAST_REQUIRES_SINGLE_PARAMETER";
        case CERR_PROC_INVOC_PARAM_COUNT_MISMATCH: return "CERR_PROC_INVOC_PARAM_COUNT_MISMATCH";
        case CERR_TOO_FEW_PROC_PARAMETERS: return "CERR_TOO_FEW_PROC_PARAMETERS";
        case CERR_TOO_MANY_PROC_PARAMETERS: return "CERR_TOO_MANY_PROC_PARAMETERS";
        case CERR_TOO_MANY_EXPR_NODES: return "CERR_TOO_MANY_EXPR_NODES";
        case CERR_TOO_MANY_VIRTUAL_NODES: return "CERR_TOO_MANY_VIRTUAL_NODES";
        case CERR_TOO_MANY_NODES_IN_LARGE_LIST: return "CERR_TOO_MANY_NODES_IN_LARGE_LIST";
        case CERR_TOO_FEW_RETURN_ARGUMENTS: return "CERR_TOO_FEW_RETURN_ARGUMENTS";
        case CERR_TOO_MANY_RETURN_ARGUMENTS: return "CERR_TOO_MANY_RETURN_ARGUMENTS";
        case CERR_TOO_FEW_LHV: return "CERR_TOO_FEW_LHV";
        case CERR_TOO_MANY_LHV: return "CERR_TOO_MANY_LHV";
        case CERR_TYPE_AND_VALUE_COUNT_DIFFER_IN_VAR_DECL: return "CERR_TYPE_AND_VALUE_COUNT_DIFFER_IN_VAR_DECL";
        case CERR_CANNOT_USE_REGULAR_VAR_DECLARATION_IN_ENUMS: return "CERR_CANNOT_USE_REGULAR_VAR_DECLARATION_IN_ENUMS";
        case CERR_CANNOT_USE_TYPE_AND_INIT_VAR_DECL_FORM_IN_COMPOUNDS: return "CERR_CANNOT_USE_TYPE_AND_INIT_VAR_DECL_FORM_IN_COMPOUNDS";
        case CERR_SINGLE_TO_MULTI_ASSIGN_MUST_SHARE_TYPE: return "CERR_SINGLE_TO_MULTI_ASSIGN_MUST_SHARE_TYPE";
        case CERR_TOO_MANY_COMPOUNDS_IN_SAME_FILE: return "CERR_TOO_MANY_COMPOUNDS_IN_SAME_FILE";

        case CERR_TRYING_TO_EVALUATE_SINK: return "CERR_TRYING_TO_EVALUATE_SINK";
        case CERR_RESERVED_IDENTIFIER_AS_LVALUE: return "CERR_RESERVED_IDENTIFIER_AS_LVALUE";
        case CERR_CONSTANT_AS_LVALUE: return "CERR_CONSTANT_AS_LVALUE";
        case CERR_TEMPORARY_AS_LVALUE: return "CERR_TEMPORARY_AS_LVALUE";
        case CERR_DOT_DECENT_NON_REFERENCABLE_AS_LVALUE: return "CERR_DOT_DECENT_NON_REFERENCABLE_AS_LVALUE";
        case CERR_PROC_PARAM_AS_LVALUE: return "CERR_PROC_PARAM_AS_LVALUE";
        case CERR_INVALID_PTRDIFF_OP: return "CERR_INVALID_PTRDIFF_OP";
        case CERR_INVALID_CAST: return "CERR_INVALID_CAST";
        case CERR_INVALID_CAST_NYKA_AS_CONSTANT: return "CERR_INVALID_CAST_NYKA_AS_CONSTANT";
        case CERR_INVALID_EVAL_NYKA: return "CERR_INVALID_EVAL_NYKA";
        case CERR_CAST_NEGATIVE_TO_UNSIGNED: return "CERR_CAST_NEGATIVE_TO_UNSIGNED";
        case CERR_CAST_REQUIRES_EXPLICIT: return "CERR_CAST_REQUIRES_EXPLICIT";
        case CERR_IMPLICIT_CONST_INT_CAST_OOB: return "CERR_IMPLICIT_CONST_INT_CAST_OOB";
        case CERR_DIVISION_BY_ZERO: return "CERR_DIVISION_BY_ZERO";

        case CERR_SATURATING_CAST_REQUIRES_NUMERIC_OR_RAW_INTEGRAL: return "CERR_SATURATING_CAST_REQUIRES_NUMERIC_OR_RAW_INTEGRAL";
        case CERR_OPERATOR_REQUIRES_SIZED_OPERANDS: return "CERR_OPERATOR_REQUIRES_SIZED_OPERANDS";
        case CERR_OPERATOR_REQUIRES_NUMERIC_OPERANDS: return "CERR_OPERATOR_REQUIRES_NUMERIC_OPERANDS";
        case CERR_OPERATOR_REQUIRES_ONE_FP_OPERAND: return "CERR_OPERATOR_REQUIRES_ONE_FP_OPERAND";
        case CERR_OPERATOR_REQUIRES_NUMERIC_INTEGRAL_OPERANDS: return "CERR_OPERATOR_REQUIRES_NUMERIC_INTEGRAL_OPERANDS";
        case CERR_OPERATOR_REQUIRES_RAW_OR_INTEGRAL_OPERANDS: return "CERR_OPERATOR_REQUIRES_RAW_OR_INTEGRAL_OPERANDS";
        case CERR_OPERATOR_REQUIRES_BOOLEAN_OPERANDS: return "CERR_OPERATOR_REQUIRES_BOOLEAN_OPERANDS";
        case CERR_OPERATOR_REQUIRES_NON_NEGATIVE_SECOND_OPERAND: return "CERR_OPERATOR_REQUIRES_NON_NEGATIVE_SECOND_OPERAND";
        case CERR_OPERATOR_REQUIRES_SMALL_SECOND_OPERAND: return "CERR_OPERATOR_REQUIRES_SMALL_SECOND_OPERAND";
        case CERR_CANNOT_MIX_RAW_INTEGRAL_WITH_NON_INTEGRAL_IN_OP: return "CERR_CANNOT_MIX_RAW_INTEGRAL_WITH_NON_INTEGRAL_IN_OP";
        case CERR_CANNOT_MIX_POINTER_WITH_NON_INTEGRAL_IN_OP: return "CERR_CANNOT_MIX_POINTER_WITH_NON_INTEGRAL_IN_OP";
        case CERR_NOT_A_VALID_OPERATION_ON_POINTER: return "CERR_NOT_A_VALID_OPERATION_ON_POINTER";
        case CERR_NOT_A_VALID_OPERATION_ON_RAW_INTEGRAL: return "CERR_NOT_A_VALID_OPERATION_ON_RAW_INTEGRAL";

        case CERR_INDEXING_REQUIRES_NUMERIC_INDEX: return "CERR_INDEXING_REQUIRES_NUMERIC_INDEX";
        case CERR_INDEXING_REQUIRES_TOTAL_BYTES_COUNT_FITTING_IN_I64: return "CERR_INDEXING_REQUIRES_TOTAL_BYTES_COUNT_FITTING_IN_I64";
        case CERR_INDEXING_OUT_OF_BOUNDS: return "CERR_INDEXING_OUT_OF_BOUNDS";
        case CERR_CANNOT_RUNTIME_ITERATE_THROUGH_ARRAY_WITH_COMPTIME_ONLY: return "CERR_CANNOT_RUNTIME_ITERATE_THROUGH_ARRAY_WITH_COMPTIME_ONLY";

        case CERR_COMPINT_OVERFLOW: return "CERR_COMPINT_OVERFLOW";
        case CERR_EXPECTED_NO_HIGHER_THAN_32_BITS: return "CERR_EXPECTED_NO_HIGHER_THAN_32_BITS";
        case CERR_FILENAME_TOO_LONG: return "CERR_FILENAME_TOO_LONG";
        case CERR_INVALID_FILE_LOAD_DIRECTIVE: return "CERR_INVALID_FILE_LOAD_DIRECTIVE";

        case CERR_EXPECTED_BOOLEAN: return "CERR_EXPECTED_BOOLEAN";
        case CERR_ADDRESSOF_EXPECTED_BOUND: return "CERR_ADDRESSOF_EXPECTED_BOUND";
        case CERR_ADDRESSOF_EXPECTED_NON_COMPINT_BOUND: return "CERR_ADDRESSOF_EXPECTED_NON_COMPINT_BOUND";
        case CERR_CONST_ADDRESSOF_EXPECTED_NON_LOCAL_VAR_BOUND: return "CERR_CONST_ADDRESSOF_EXPECTED_NON_LOCAL_VAR_BOUND";
        case CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK: return "CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK";
        case CERR_CANNOT_COMPACT_THIS_TYPE: return "CERR_CANNOT_COMPACT_THIS_TYPE";
        case CERR_CANNOT_DO_PTRDIFF_BETWEEN_RAWPTR_AND_PTR: return "CERR_CANNOT_DO_PTRDIFF_BETWEEN_RAWPTR_AND_PTR";
        case CERR_CANNOT_DO_PTRDIFF_BETWEEN_POINTERS_TO_DIFFERENT_SIZED_TYPES: return "CERR_CANNOT_DO_PTRDIFF_BETWEEN_POINTERS_TO_DIFFERENT_SIZED_TYPES";

        case CERR_CANNOT_ACCESS_PACKAGE_DECLARATION_FROM_OTHER: return "CERR_CANNOT_ACCESS_PACKAGE_DECLARATION_FROM_OTHER";

        case CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION: return "CERR_SIGNED_UNSIGNED_MISMATCH_ON_OPERATION";
        case CERR_SIGNED_UNSIGNED_MISMATCH_WITHIN_ARRAY_LITERAL: return "CERR_SIGNED_UNSIGNED_MISMATCH_WITHIN_ARRAY_LITERAL";
        case CERR_TYPE_MISMATCH_WITHIN_NON_NUMERIC_ARRAY_LITERAL: return "CERR_TYPE_MISMATCH_WITHIN_NON_NUMERIC_ARRAY_LITERAL";
        case CERR_TYPE_MISMATCH_FOR_OPERATION: return "CERR_TYPE_MISMATCH_FOR_OPERATION";
        case CERR_OPERATION_WOULD_OVERFLOW: return "CERR_OPERATION_WOULD_OVERFLOW";
        case CERR_INEXACT_QUOTIENT: return "CERR_INEXACT_QUOTIENT";

        case CERR_MISSING_TYPE_INFERRENCE: return "CERR_MISSING_TYPE_INFERRENCE";

        case CERR_INVALID_MODULO_OP_BETWEEN_NON_RAW: return "CERR_INVALID_MODULO_OP_BETWEEN_NON_RAW";
        case CERR_TYPE_NOT_INDEXABLE: return "CERR_TYPE_NOT_INDEXABLE";

        case CERR_TOO_MANY_ARGUMENTS_IN_SIGNATURE: return "CERR_TOO_MANY_ARGUMENTS_IN_SIGNATURE";

        case CERR_TOO_MANY_IDENTIFIERS: return "CERR_TOO_MANY_IDENTIFIERS";
        case CERR_RESULTING_IDENTIFIER_TOO_LONG: return "CERR_RESULTING_IDENTIFIER_TOO_LONG";
        case CERR_TOO_MANY_PROC_BODIES: return "CERR_TOO_MANY_PROC_BODIES";
        case CERR_TOO_MANY_USER_TYPES: return "CERR_TOO_MANY_USER_TYPES";
        case CERR_TOO_MANY_GLOBAL_VARIABLES: return "CERR_TOO_MANY_GLOBAL_VARIABLES";
        case CERR_TOO_MANY_GLOBAL_CONSTANT_VALUES: return "CERR_TOO_MANY_GLOBAL_CONSTANT_VALUES";
        case CERR_TOO_MANY_LOCAL_CONSTANTS: return "CERR_TOO_MANY_LOCAL_CONSTANTS";
        case CERR_TOO_MANY_INSTRUCTIONS_IN_PROC: return "CERR_TOO_MANY_INSTRUCTIONS_IN_PROC";
        case CERR_TOO_MANY_EXPANSION_LEVELS: return "CERR_TOO_MANY_EXPANSION_LEVELS";
        case CERR_TOO_MANY_RUNTIME_MEMBERS_IN_STRUCT: return "CERR_TOO_MANY_RUNTIME_MEMBERS_IN_STRUCT";

        case CERR_ELSE_WITHOUT_IF: return "CERR_ELSE_WITHOUT_IF";

        case CERR_NAN_RESULT_FROM_OP_INVOLVING_NON_NAN: return "CERR_NAN_RESULT_FROM_OP_INVOLVING_NON_NAN";
        case CERR_INFINITE_RESULT_FROM_OP_INVOLVING_FINITE: return "CERR_INFINITE_RESULT_FROM_OP_INVOLVING_FINITE";
        case CERR_SUBNORMAL_RESULT_FROM_OP_INVOLVING_NORMAL: return "CERR_SUBNORMAL_RESULT_FROM_OP_INVOLVING_NORMAL";
        case CERR_LOSING_PREC_ON_INT_TO_FP_CONVERSION: return "CERR_LOSING_PREC_ON_INT_TO_FP_CONVERSION";
        case CERR_RAW_INTEGRAL_HAS_NO_DEFAULT_CAST_TO_FP: return "CERR_RAW_INTEGRAL_HAS_NO_DEFAULT_CAST_TO_FP";
        case CERR_ISOLATED_DOT_DESCENT_MISSING_UPWARDS_INFERRENCE: return "CERR_ISOLATED_DOT_DESCENT_MISSING_UPWARDS_INFERRENCE";

        case CERR_COMPLEX_TYPE_DECL_FORM_OUTSIDE_CONST_DECLARATION_WITH_SINGLE_RHV: return "CERR_COMPLEX_TYPE_DECL_FORM_OUTSIDE_CONST_DECLARATION_WITH_SINGLE_RHV";
        case CERR_PROCLIKE_DECL_FORM_OUTSIDE_SIGN_OR_CONST_DECLARATION_WITH_SINGLE_RHV: return "CERR_PROCLIKE_DECL_FORM_OUTSIDE_SIGN_OR_CONST_DECLARATION_WITH_SINGLE_RHV";

        case CERR_TRYING_TO_DEREF_ADDRESS_0: return "CERR_TRYING_TO_DEREF_ADDRESS_0";

        default:
            return "<unexpected error-code, category: typecheck error>";
        }
        // TODO
        return "Typechecker Error: Not yet with a precise description.";
    } else if (pError->errCode < BASE_OF_LINKER_ERRORS) {       // from BASE_OF_VALIDATION_ERRORS
        switch (pError->errCode) {
        case VERR_EXPLICIT_ERROR:
            return "Explicit 'error' statement reached in typechecker";
        case VERR_STATIC_ASSERT_FAILED:
            return "Static 'assert' statement failed";
        case VERR_COMPTIME_EXEC_FAILED:
            return "Comptime execution failed";
        case VERR_UNALLOWED_IDENTIFIER_SHADOWING:
            return "Identifier shadowing detected (after the fact)";
        default:
            return "<unexpected error-code, category: validation error>";
        }
    } else if (pError->errCode < BASE_OF_FATAL_ERRORS) {        // from BASE_OF_LINKER_ERRORS
        // TODO
        return "Linker Error: Not yet with a precise description.";
    } else {
        switch (pError->errCode) {
        case FERR_ASSERTION_FAILED:
            return "Failed assertion within typechecker";
        case FERR_UNEXPECTED_SYNTAX:
            return "Syntax error found during typechecking";
	    case FERR_UNREACHABLE:
            return "Reached 'unreachable' during compile-time evaluation";
        case FERR_UNKNOWN_CONSTANT_OP_CONVERTED_TO_RUNTIME:
            return "Unknown const op to runtime";
        case FERR_NOT_YET_IMPLEMENTED:
            return "Functionality not yet implemented";
        case FERR_TOO_MANY_ENTITIES:
            return "Too many entities";
        case FERR_OTHER:
            return "Fatal error";
        default:
            return "<unexpected error-code, category: fatal error>";
        }
    }
}

// To display a longer, possibly multiline description of an error
exported_func_impl const char* write_error_report_desc(
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs)
{
    // TODO
    //Assert_(false); // Not yet implemented
    return 0;
}

// To display an advice to hopefully correct some error
exported_func_impl const char* write_error_report_advice(
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs)
{
    // TODO
    //Assert_(false); // Not yet implemented
    return 0;
}

exported_func_impl void release_compilation_results(
    LocLib_CompilationResults* pCompilationResults,
    LocLib_OS_WrapperFunctions* pOsFuncs)
{
    // TODO
    //Assert_(false); // Not yet implemented
}
