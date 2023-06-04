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

#ifndef LOCLIB_CMD_API_H_
#define LOCLIB_CMD_API_H_

#include "../../HighPerfTools/BaseDecls.h"

#if defined(COMPILING_LOCLIB_DLL)
// if we parse that header when compiling loclib dll, then declared-exported-by-loclib functions will be
//   flagged as auto-exported to the compilation-chain 
#  define DECL_EXPORTED_LOCLIB  __declspec(dllexport)
#elif defined(MANUALLY_USING_LOCLIB_DLL) || defined(USING_LOCLIB_STATICALLY) || defined(COMPILING_LOCLIB_STATICALLY)
// if we parse that header as a user of loclib and specifying that we'll manually import the functions, or as a
//   producer or user of a static version of loclib, then declared-exported-by-loclib functions will have no
//   particular decoration.
#  define DECL_EXPORTED_LOCLIB  
#else
// if we parse that header otherwise, we're supposed a user of loclib dll, then declared-exported-by-loclib functions will be
//   flagged as auto-imported to the compilation-chain 
#  define DECL_EXPORTED_LOCLIB  __declspec(dllimport)
#endif

#include "../../HighPerfTools/Platform_API.h"
#include "LocLib_ErrorEnums.h"

// predecls
struct StatementParserState;
struct SourceFileDescAndState;
struct LocLib_OS_WrapperFunctions;

struct TokenizerClosureBaseData {
    int iLineIndex;
    const u8* pStartOfCurrentLine;
}; // NOTE: meant to be extended on a per-implementation basis for functions with the following signature
typedef int (*GetNextLineTokens_Sign)(
    StatementParserState* ioParserState,
    SourceFileDescAndState* pSourceFile,
    LocLib_OS_WrapperFunctions* pOsFuncs,
    TokenizerClosureBaseData* pClosureData, u16* outError);

struct TokenizerClosure {
    GetNextLineTokens_Sign pGetNextLineTokensFn;
    TokenizerClosureBaseData* pClosureData;
};

// default full-file tokenizer params
struct GetNextLineTokens_WhenReadingFullFile_ClosureData {
    TokenizerClosureBaseData _baseData;
    const u8* pNextLineCharScan;
    const u8* pAfterLastCharInFile;
};

// default full-file tokenizer
exported_func_decl(LOCLIB) int get_next_line_tokens_when_reading_full_file(
    StatementParserState* ioParserState,
    SourceFileDescAndState* pSourceFile,
    LocLib_OS_WrapperFunctions* pOsFuncs,
    TokenizerClosureBaseData* pClosureData, u16* outError);

typedef int     (*GetSourceFileIndex_Sign)          (int iOpenerIndex, const char* pRelativeFileName, i16 iFileNameByteSize, u16* outError);
typedef int     (*GetSourceFileFullNameByteCount_Sign)  (int iSourceFileIndex);
typedef int     (*GetSourceFileFullName_Sign)       (int iSourceFileIndex, char* szFileNameBufferMax4096);
typedef bool    (*GetSourceFileTokenizer_Sign)      (int iSourceFileIndex, TokenizerClosure* outTokenizer, u16* outError);
typedef void    (*FreeSourceFileTokenizer_Sign)     (int iSourceFileIndex, TokenizerClosure* pTokenizer);
typedef bool    (*TryAcquireSourceFileReaderResource_Sign)      ();
typedef bool    (*BlockingAcquireSourceFileReaderResource_Sign)    ();
typedef void    (*ReleaseSourceFileReaderResource_Sign)         ();

// Functions provided by the layer having knowledge of the platform (OS) to the otherwise platform-independent LOC library
struct LocLib_OS_WrapperFunctions {

    //
    // The following functions are to ask controler program (with final knowledge about file system and active directories) for
    //   source files, given their relative path as expressed in source code, and the source file from which it is specified.
    //
    
    GetSourceFileIndex_Sign     pGetSourceFileIndexFn;          // from current source file by index, and relative file name, get index. -1 if error
    GetSourceFileFullNameByteCount_Sign pGetSourceFileFullNameByteCountFn; // get byte count for full name
    GetSourceFileFullName_Sign  pGetSourceFileFullNameFn;       // get diplayable, possibly full-path, name (and macro #filename), from source file index. 
    
    // get an actual *tokenizer* for a source file... allows controler program to be an IDE with already being-edited source files,
    //   and provide already-cached tokenization results, OR simply return the default tokenizer against full file byte contents...
    GetSourceFileTokenizer_Sign  pGetSourceFileTokenizerFn;
    FreeSourceFileTokenizer_Sign pFreeSourceFileTokenizerFn;     // frees a *tokenizer* for a source file...
    
    // In that situation described above, the following functions are to "allow" the compiler to wait for actually available
    //   "file reader resource" before asking to read files...
    // It is intended as a way to sequentialize disk-access in a multithreaded, by-task compiling scheme,
    //   and it is thus expected that any call to pGetSourceFileBytesFn will be performed inside a (tight) acquire/release block.
    //
    TryAcquireSourceFileReaderResource_Sign     pTryAcquireSourceFileReaderResourceFn;      // non-blocking try acquire resource
    BlockingAcquireSourceFileReaderResource_Sign   pBlockingAcquireSourceFileReaderResourceFn;    // blocking try acquire resource
    ReleaseSourceFileReaderResource_Sign        pReleaseSourceFileReaderResourceFn;         // release resource.
    //  Note: can release right after "pGetSourceFileTokenizerFn" returns: no need to have that acquired for pFreeSourceFileTokenizerFn...
};

// Compilation parameters to provide
typedef struct LocLib_CompilationParams {
    bool bRegSizeIs64;
    bool bPtrSizeIs64;
    bool bComptimeFloatIs64;
    bool bLittleEndianTarget;
    bool bRuntimeChecksOn;
    bool bEmitIRDump;
    bool bEmitAvailableTraces;
    bool bSilentOutput;
    u32 uMaxCompintLegs;
    u64 tPersistentStorage[16];
	// TODO
} LocLib_CompilationParams;


// Encoded description of an error, provided as part of compilation results.
// User inteface code can use write_error_report_* functions against those.
typedef struct LocLib_Error {
	u16 errCode;
    u16 _pad0;
    u32 uBlockOrLineIfScanErr;
    u32 uStatement;
    u32 uTokenRef;
} LocLib_Error;

// Description of a loc source file which was processed during compilation, provided as part of compilation results.
// User interface code can compile and display whatever stats it needs from those...
// it is also free to use it to, for example, display the actual contents of the source code file in the vicinity of an error.
typedef struct LocLib_SourceFileStats {
    u32 uByteSize;
    u32 uNumberOfLines;
    u32 uNumberOfNonEmptyLines;
    u32 uNumberOfCodeLines;
    u32 uNumberOfTokens;
    u32 uNumberOfStatements;
    u32 uNumberOfBlocks;
    u32 uFirstLoadedFromFileId;
    u32 uFirstLoadedAtLine;
    char* szFileName;
    char* szDirectoryFromSourcePath;
} LocLib_SourceFileStats;

// Opaque scratch-memory, returned as part of fire-and-forget compilation results, such as when invoking run_Loc_compiler_on_single_file().
// Most tables, pointers, strings. etc. which can be retrieved from those compilation results will be pointing towards memory which is tracked down by
//   this structure. user-interface code shall release it manually once done processing it.
struct LocLib_FireAndForgetAllocator;
typedef struct LocLib_FireAndForgetAllocator LocLib_FireAndForgetAllocator;

// Descriptor of compilation results.
//      uErrorCount > 0 iff any error occured during compilation, in which case for 0 <= i < uErrorCount, 'pFirstError[i]' is a LocLib_Error description.
//      uFileCount > 0 iff any source file was processed, and for 0 <= j < uFileCount, 'pFirstFile[j]' is a LocLib_SourceFileStats description.
//      pScratchMem shall be passed to write_error_report_* functions to process errors from this compilation run,
//          and shall be explicitely released by user interface code (by invoking release_scratch_mem()) once no longer in use.
typedef struct LocLib_CompilationResults {
	u32 uErrorCount;
    u32 uWarningCount;
    int iFirstFileWithError;
	u32 uFileCount;
    SourceFileDescAndState** tFiles;
} LocLib_CompilationResults;

// Main function to call for fire-and-forget compilation...
//   pFileName and uFileNameBytes shall be such that a subsequent call to pReadFileEntirelyFn will be able to find and open the correct source file
//   pOSFuncs shall be provided with valid function pointer contents.
//   pCompilationParams shall be initialized with the desired compilation parameters.
//   oCompilationResults shall not be null.
typedef bool (*RunLocCompilerFromFirstFile_Sign) (
    int iFirstSourceFileIndex,
	LocLib_OS_WrapperFunctions* pOsFuncs, 
	LocLib_CompilationParams* pCompilationParams,
	LocLib_CompilationResults* oCompilationResults);
exported_func_decl(LOCLIB) bool run_loc_compiler_from_first_file(
    int iFirstSourceFileIndex,
	LocLib_OS_WrapperFunctions* pOsFuncs, 
	LocLib_CompilationParams* pCompilationParams,
	LocLib_CompilationResults* oCompilationResults);

// Function to be called by user interface code on the returned LocLib_CompilationResults::pScratchMem element after a fire-and-forget compilation run
//   such as a call to run_Loc_compiler_on_single_file(), once those results are no longer required (incl. displaying errors from a particular run). 
typedef void (*ReleaseCompilationResults_Sign) (
	LocLib_CompilationResults* pCompilationResults,
    LocLib_OS_WrapperFunctions* pOsFuncs);
exported_func_decl(LOCLIB) void release_compilation_results(
	LocLib_CompilationResults* pCompilationResults,
    LocLib_OS_WrapperFunctions* pOsFuncs);

// Opaque structure for holding report-language-override data in case of non-english user interface
struct LocLib_ReportLanguageOverride;
typedef struct LocLib_ReportLanguageOverride LocLib_ReportLanguageOverride;

// Function to be called to initialize report-language-override data from a file.
//   result must be freed manually by a call to release_report_language()
typedef LocLib_ReportLanguageOverride* (*InitReportLanguage_Sign) (
    const u8* pFileContents,
    u64 uFileContentsByteSize,
    LocLib_OS_WrapperFunctions* pOsFuncs);
exported_func_decl(LOCLIB) LocLib_ReportLanguageOverride* init_report_language(
    const u8* pFileContents,
    u64 uFileContentsByteSize,
    LocLib_OS_WrapperFunctions* pOsFuncs);

// Function to release report-language-override data returned by init_report_language()
typedef void (*ReleaseReportLanguage_Sign) (
	LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs);
exported_func_decl(LOCLIB) void release_report_language(
	LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs);

// Call one the following functions to have LocLib write some displayable results about a particular error,
//    it will be allocated as scratch memory known to the compilation result, which will get freed
//       when releasing the compilation results themselves afterwards.
//    if pReportLanguage is 0, the hardcoded english-version of the error reports will be used. use init_report_language() beforehand to
//      support additional report languages for error descriptions and advices...
// Note: those reports may include actual contextual info about the problem, but if your user interface wants to display the code in the vicinity
//      of an error, then you'd need to read the file contents yourself at the hinted position, using uFileId from the error description as an index
//      to find file stats descriptor (incl. finename) from the pFirstFile pointer returned in the compilation report structure.


typedef const char* (*GetFilenameFromFileDesc_Sign) (SourceFileDescAndState* pSourceFileDesc);
exported_func_decl(LOCLIB) const char* get_filename_from_filedesc(SourceFileDescAndState* pSourceFileDesc);

typedef const LocLib_Error* (*GetErrorsOnSourceFile_Sign) (SourceFileDescAndState* pSourceFileDesc, u64* outErrCount);
exported_func_decl(LOCLIB) const LocLib_Error* get_errors_on_sourcefile(SourceFileDescAndState* pSourceFileDesc, u64* outErrCount);

typedef char* (*WriteErrorReport_Sign) (
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs);

// To display a short, one-line report of an error
exported_func_decl(LOCLIB) const char* write_error_report_stub(
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs);

// To display a longer, possibly multiline description of an error
exported_func_decl(LOCLIB) const char* write_error_report_desc(
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs);

// To display an advice to hopefully correct some error
exported_func_decl(LOCLIB) const char* write_error_report_advice(
    const LocLib_Error* pError,
    SourceFileDescAndState* pSourceFileDesc, int iSourceFileIndex,
    LocLib_CompilationResults* pCompilationResults,
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs);

typedef const u8* (*CheckForByteOrderMark_Sign)(const u8* pFileData, u64 uByteSize, LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError);
exported_func_decl(LOCLIB) const u8* check_for_BOM(const u8* pFileData, u64 uByteSize, LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError);

struct LocLib_ErrorInfo {
    u32 uLineOfStatementFromFileStart;
    u32 uByteOffsetOfStatementFromFileStart;
    u32 uLineOffsetOfTokenFromStatement;
    u32 uColumnOfTokenStart;
};

typedef void (*GetNonScanErrorInfo_Sign)(const SourceFileDescAndState* pSourceFile,
    const LocLib_Error* pError, LocLib_ErrorInfo* outInfo);

exported_func_decl(LOCLIB) void get_non_scan_error_info(const SourceFileDescAndState* pSourceFile,
    const LocLib_Error* pError, LocLib_ErrorInfo* outInfo);

typedef const u8* (*ScanSourceFileLine_Sign)(const u8* pStart, const u8* pEnd,
    u16* outLineIndent, u16* outStartingTabIndents, u16* outCharCountWithoutIndentOrEOL, u16* outEOL, u16* ioErr);

exported_func_decl(LOCLIB) const u8* text_scan_next_line(const u8* pStart, const u8* pEnd,
    u16* outLineIndent, u16* outStartingTabIndents, u16* outCharCountWithoutIndentOrEOL, u16* outEOL, u16* ioErr);

struct Token;
typedef int (*TokenizeLine_Sign)(const u8* pCurrentLineStartWithoutIndent, const u8* pLineEnd, i32* ioCommentLevel,
                  Token* outTokenArray, SourceFileDescAndState* pSourceFile, u16* outError);

exported_func_decl(LOCLIB) int tokenize_line(const u8* pCurrentLineStartWithoutIndent, const u8* pLineEnd, i32* ioCommentLevel,
                  Token* outTokenArray, SourceFileDescAndState* pSourceFile, u16* outError);

#endif // LOCLIB_CMD_API_H_
