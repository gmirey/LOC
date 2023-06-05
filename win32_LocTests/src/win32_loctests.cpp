// Part of LocLang/WinTest
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


#include "../../HighPerfTools/BaseDecls.h"
#include "../../LocLib/src/LocLib_Cmd_API.h"
#include "../../LocLib/src/LocLib_Token.h"

#define UNICODE
#include <windows.h>
#include <string>
#include <vector>
#include <unordered_map>
#include <iostream>
#include <cassert>
#include <string_view>

#define TRACE_VIRTUAL_ALLOCS    0

static std::wstring _convert_to_utf16(std::string sPathUtf8)
{
    if (sPathUtf8.length() == 0)
        return L"";
	int iRequiredCount = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                                             sPathUtf8.c_str(), sPathUtf8.length()+1, 0, 0);
    assert(iRequiredCount > 0);
    assert(iRequiredCount < 2048);
    wchar_t szBuffer[2048];
 	int iResultingWords = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS,
                                              sPathUtf8.c_str(), sPathUtf8.length()+1, szBuffer, iRequiredCount);
    assert(iResultingWords == iRequiredCount);
    assert(szBuffer[iResultingWords-1] == L'\0');
    return std::wstring(szBuffer);
}

static std::string _convert_to_utf8(std::wstring sPathUtf16)
{
    if (sPathUtf16.length() == 0)
        return "";
	int iRequiredCount = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS|WC_NO_BEST_FIT_CHARS,
                                             sPathUtf16.c_str(), sPathUtf16.length()+1, 0, 0, 0, 0);
    assert(iRequiredCount > 0);
    assert(iRequiredCount < 4096);
    char szBuffer[4096];
 	int iResultingBytes = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS|WC_NO_BEST_FIT_CHARS,
                                              sPathUtf16.c_str(), sPathUtf16.length()+1, szBuffer, iRequiredCount, 0, 0);
    assert(iResultingBytes == iRequiredCount);
    assert(szBuffer[iResultingBytes-1] == '\0');
    return std::string(szBuffer);
}

// logs stuff using OutputDebugStringA or OutputDebugStringW.
//		will append a CR LF automatically if bAndEOL is true (default).
//		will use OutputDebugStringA directly if eUtf8Contents is specified as eUTF8_CONTENTS_ASCII
//		will convert to UTF16 and use OutputDebugStringW if eUtf8Contents is specified as eUTF8_CONTENTS_EXTENDED
//		will first check if contains complex content if eUtf8Contents is left as eUTF8_CONTENTS_UNKNOWN (default)
//		if an UTF16 conversion (or check) is needed, and UTF16 contents require more than 2048 wide chars,
//			this method will log an error instead.
static void log_with_output_debug_string(StringView strToLog, bool bAndEOL = true)
{
    strToLog.check_utf8();
    if (strToLog.flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII) {
        if (strToLog.flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO) {
		    OutputDebugStringA((const char*)strToLog.start);
        } else if (strToLog.byte_length() < 4096) {
            char szTemp[4096];
            memcpy(szTemp, strToLog.start, strToLog.byte_length());
            szTemp[strToLog.byte_length()] = '\0';
		    OutputDebugStringA(szTemp);
        } else {
			OutputDebugStringA("***log_string internal error*** : ASCII parameter to log is too long.\n");
        }
    } else {
        if (strToLog.flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO) {
            std::wstring asUtf16 = _convert_to_utf16(std::string((const char*)strToLog.start));
            OutputDebugStringW(asUtf16.c_str());
        } else if (strToLog.byte_length() < 4096) {
            char szTemp[4096];
            memcpy(szTemp, strToLog.start, strToLog.byte_length());
            szTemp[strToLog.byte_length()] = '\0';
            std::wstring asUtf16 = _convert_to_utf16(std::string(szTemp));
            OutputDebugStringW(asUtf16.c_str());
        } else {
			OutputDebugStringA("***log_string internal error*** : UTF8 parameter to log is too long.\n");
        }
    }
    if (bAndEOL)
        OutputDebugStringA("\n");
}

static void log_to_stdout(StringView strToLog, bool bAndEOL = true)
{
    strToLog.check_utf8();
    if (strToLog.flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII) {
        if (strToLog.flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO) {
		    std::cout << (const char*)strToLog.start;
        } else if (strToLog.byte_length() < 4096) {
            char szTemp[4096];
            memcpy(szTemp, strToLog.start, strToLog.byte_length());
            szTemp[strToLog.byte_length()] = '\0';
		    std::cout << szTemp;
        } else {
			std::cout << "***log_string internal error*** : ASCII parameter to log is too long.\n";
        }
    } else {
        if (strToLog.flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO) {
            std::wstring asUtf16 = _convert_to_utf16(std::string((const char*)strToLog.start));
            std::wcout << asUtf16;
        } else if (strToLog.byte_length() < 4096) {
            char szTemp[4096];
            memcpy(szTemp, strToLog.start, strToLog.byte_length());
            szTemp[strToLog.byte_length()] = '\0';
            std::wstring asUtf16 = _convert_to_utf16(std::string(szTemp));
            std::wcout << asUtf16;
        } else {
			std::cout << "***log_string internal error*** : UTF8 parameter to log is too long.\n";
        }
    }
    if (bAndEOL)
        std::cout << std::endl;
}

static LogFn* log_info_impl = log_to_stdout;
static LogFn* log_debug_impl = log_to_stdout;

static void log_info(StringView strToLog, bool bAndEOL = true)
{
	return log_info_impl(strToLog, bAndEOL);
}
static void log_debug(StringView strToLog, bool bAndEOL = true)
{
	return log_debug_impl(strToLog, bAndEOL);
}


static u8* page_alloc(u64 uMinBytes)
{
	u8* pResult = (u8*)VirtualAlloc(0, (LONGLONG)uMinBytes, MEM_COMMIT, PAGE_READWRITE);
    #if TRACE_VIRTUAL_ALLOCS > 0
    {
        char szTmp[256];
        sprintf(szTmp, "VirtualAlloc %llu bytes at 0x%llx", uMinBytes, reinterpret_cast<u64>(pResult));
        log_info(szTmp);
    }
    #endif
    return pResult;
}

static void page_free(u8* pAllocResult)
{
    #if TRACE_VIRTUAL_ALLOCS    > 0
    {
        char szTmp[256];
        sprintf(szTmp, "VirtualFree of 0x%llx", reinterpret_cast<u64>(pAllocResult));
        log_info(szTmp);
    }
    #endif
	VirtualFree((void*)pAllocResult, 0, MEM_RELEASE);
}

// read_file_entirely_* implementation once a hFile has been opened
static u8* read_file_entirely_hfile(HANDLE hFile, u64* outByteSize, EFileOpenErr* outErr)
{
    u8* pResult = 0;
    if (hFile != INVALID_HANDLE_VALUE) {
        LARGE_INTEGER lSize;
        if (GetFileSizeEx(hFile, &lSize)) {
            if (lSize.QuadPart > 0) {
			    i64 iSizeIter = (i64)lSize.QuadPart;
                *outByteSize = u64(iSizeIter);
			    pResult = (u8*)VirtualAlloc(0, lSize.QuadPart, MEM_COMMIT, PAGE_READWRITE);
			    const u32 TwoGB = 0x80000000LL;
			    if (pResult) {
				    u8* pCurrentWriteStart = pResult;
				    while (lSize.QuadPart > TwoGB) {
                        DWORD uBytesReadExpect2GB;
                        if (!ReadFile(hFile, pCurrentWriteStart, TwoGB, &uBytesReadExpect2GB, 0) ||
						    uBytesReadExpect2GB != TwoGB) {
							    VirtualFree((void*)pResult, 0, MEM_RELEASE);
							    pResult = 0;
                                *outErr = EFileOpenErr::FILEOPEN_FAILED_READ;
                                log_debug("*** read_file_entirely_hfile() : Failed read (large 2GB iteration)");
							    break;
                        }
					    pCurrentWriteStart += TwoGB;
					    lSize.QuadPart -= TwoGB;
				    }
				    DWORD uBytesReadFinally;
				    if (pResult) {
					    if (!ReadFile(hFile, pCurrentWriteStart, lSize.LowPart, &uBytesReadFinally, 0) ||
						    uBytesReadFinally != lSize.LowPart) {
							    VirtualFree((void*)pResult, 0, MEM_RELEASE);
							    pResult = 0; {
                                    *outErr = EFileOpenErr::FILEOPEN_FAILED_READ;
                                    log_debug("*** read_file_entirely_hfile() : Failed read");
                                }
					    } else
                            *outErr = EFileOpenErr::FILE_OPENED_SUCCESSFULLY;
				    }
			    } else {
                    *outErr = EFileOpenErr::FILEOPEN_OUT_OF_MEMORY;
                    log_debug("*** read_file_entirely_hfile() : Out Of Memory");
                }
            } else {
                *outByteSize = 0;
                *outErr = EFileOpenErr::FILE_OPENED_SUCCESSFULLY;
            }
        } else {
            *outErr = EFileOpenErr::FILEOPEN_FAILED_READ;
            log_debug("*** read_file_entirely_hfile() : GetFileSizeEx failed");
        }
        CloseHandle(hFile);
    } else {
        *outErr = EFileOpenErr::FILEOPEN_FILE_NOT_FOUND;
        log_debug("*** read_file_entirely_hfile() : File not found");
    }
    return pResult;	
}

// read_file_entirely_* entry point, with a file path expressed in 8bit ASCII < 128
//   will open the file with CreateFileA(), then call read_file_entirely_hfile on the returned handle
static u8* read_file_entirely_ascii(const char* szFilePath, i16 iPathByteSize, u64* outByteSize, EFileOpenErr* outErr)
{
    UNUSED(iPathByteSize); // TODO : use iPathByteSize for further validation
    HANDLE hFile = CreateFileA(szFilePath, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
	return read_file_entirely_hfile(hFile, outByteSize, outErr);
}

// read_file_entirely_* entry point, with a file path expressed in Windows UTF16 mess
//   will open the file with CreateFileW(), then call read_file_entirely_hfile on the returned handle
static u8* read_file_entirely_wchar(const wchar_t* szFilePathUTF16, i32 iPathWCharSize, u64* outByteSize, EFileOpenErr* outErr)
{
    UNUSED(iPathWCharSize); // TODO : use iPathWCharSize for further validation
    HANDLE hFile = CreateFileW(szFilePathUTF16, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (hFile == INVALID_HANDLE_VALUE) {
        log_debug("*** read_file_entirely_wchar() : could not find file");
        std::cout << "   *** szFilePathUTF16=" << szFilePathUTF16 << std::endl;
        std::cout << "   *** iPathWCharSize=" << iPathWCharSize << std::endl;
        MessageBoxW(NULL, szFilePathUTF16, NULL, MB_OK);
    }
	return read_file_entirely_hfile(hFile, outByteSize, outErr);
}

static const int UNKNOWN_SIZE_BEFORE_UTF_CONVERSION = -1;

// read_file_entirely_* entry point, with a file path expressed in UTF8.
//   will handle conversion of path name to Windows UTF16, then call read_file_entirely_wchar with it
//   returns 0 on UTF8 to UTF16 conversion error.
static u8* read_file_entirely_utf8(StringView strFilePathUTF8, u64* outByteSize, EFileOpenErr* outErr)
{
	int iRequiredCount = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, (const char*)strFilePathUTF8.start, strFilePathUTF8.uByteLength, 0, 0);
	if (iRequiredCount <= 0) {
        DWORD dwLastError = GetLastError();
        *outErr = FILEOPEN_INVALID_PARAM;
        log_debug("*** read_file_entirely_utf8() : Invalid input to MultiByteToWideChar()");
        switch (dwLastError) {
            case ERROR_INSUFFICIENT_BUFFER: log_debug("   *** A supplied buffer size was not large enough, or it was incorrectly set to NULL"); break;
            case ERROR_INVALID_FLAGS: log_debug("   *** The values supplied for flags were not valid"); break;
            case ERROR_INVALID_PARAMETER: log_debug("   *** Any of the parameter values was invalid"); break;
            case ERROR_NO_UNICODE_TRANSLATION: log_debug("   *** Invalid Unicode was found in a string"); break;
        }
        *outByteSize = 0;
		return 0;
	}
    int iIndexOfZero = iRequiredCount;
	//static const int PREFIX_SIZE = 4; // for using (non-"ANSI") extended path length
	//if (iRequiredCount + iManuallyAddTrailingZero > 32767-PREFIX_SIZE) {
	if (iIndexOfZero >= 260) {
		*outByteSize = 0;
        *outErr = FILEOPEN_INVALID_PARAM;
        log_debug("*** read_file_entirely_utf8() : Path too large");
		return 0;
	}

	wchar_t tFilePathUTF16Buffer[260];
    /*
    // 4-wchar prefix:
	tFilePathUTF16Buffer[0] = L'\\';
	tFilePathUTF16Buffer[1] = L'\\';
	tFilePathUTF16Buffer[2] = L'?';
	tFilePathUTF16Buffer[3] = L'\\';
    */
	int iResultingWCharCount = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, (const char*)strFilePathUTF8.start, strFilePathUTF8.uByteLength,
//									  tFilePathUTF16Buffer + PREFIX_SIZE, iRequiredCount);
									  tFilePathUTF16Buffer, iRequiredCount);
	if (iIndexOfZero <= 0 || tFilePathUTF16Buffer[0] == 0) {
		*outByteSize = 0;
        *outErr = FILEOPEN_INVALID_PARAM;
        log_debug("*** read_file_entirely_utf8() : Failed conversion to UTF16");
		return 0;
	}
	tFilePathUTF16Buffer[iIndexOfZero] = 0;
	
//	return read_file_entirely_wchar(tFilePathUTF16Buffer, PREFIX_SIZE+iResultingWCharCount, outByteSize, outErr);
	return read_file_entirely_wchar(tFilePathUTF16Buffer, iIndexOfZero, outByteSize, outErr);
}

// Frees allocated content of a successfull call to read_file_entirely_*
static void free_file_memory(u8* pBuffer, u64 uByteSize)
{
    UNUSED(uByteSize);
    VirtualFree((void*)pBuffer, 0, MEM_RELEASE);
}

// converts an UTF16 path to UTF8, using the user-provided buffer. Returns size in bytes of the result (excluding trailing zero)
static int convert_path_to_utf8(char* pDestBuffer, int iDestBufferSize, const wchar_t* szUtf16string, int iPathWideCharCount)
{
	int iRequiredCount = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS|WC_NO_BEST_FIT_CHARS,
                                             szUtf16string, iPathWideCharCount, 0, 0, 0, 0);
	if (iRequiredCount <= 0) {
        log_debug("*** convert_path_to_utf8() : Invalid input to WideCharToMultiByte");
		return 0;
    }
                                      
	int iManuallyAddTrailingZero = 0;
	if (iPathWideCharCount != UNKNOWN_SIZE_BEFORE_UTF_CONVERSION && szUtf16string[iPathWideCharCount-1] != L'\0') {
		iManuallyAddTrailingZero = 1;
	}
    int iIndexOfZero = iRequiredCount + iManuallyAddTrailingZero;

    if (iIndexOfZero >= iDestBufferSize) {
        log_debug("*** convert_path_to_utf8() : Path too large");
        return 0;
    }

 	int iResultingBytes = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS|WC_NO_BEST_FIT_CHARS,
                                      szUtf16string, iPathWideCharCount, pDestBuffer, iRequiredCount, 0, 0);
	int iRemoveZeroFromResult = 1;
	pDestBuffer[iIndexOfZero] = 0;

    // Converts windows-specific backslashes to LOC-path-definition slashes
    for (int i = 0; i < iResultingBytes; i++) {
        if (pDestBuffer[i] == '\\')
            pDestBuffer[i] = '/';
    }

    return iIndexOfZero - 1;
}

struct RegisteredSourceFile {
    std::string sPathFromRootUtf8;
    std::wstring sAbsPathUtf16;
    std::wstring sFullFilePathUtf16;
    std::string sFullFilePathUtf8;
    u8* pOpenedFileData;
    u64 iOpenedFileByteSize;
};

std::unordered_map<std::string, int> g_mapIndexRegisteredByPathFromRoot;
std::vector<RegisteredSourceFile>    g_vecAllRegistered;
std::wstring g_FullPathOfRoot;
GetNextLineTokens_Sign g_pGetNextLineTokensWhileReadingFullFileFn = 0;
CheckForByteOrderMark_Sign g_pCheckForByteOrderMarkFn = 0;
LocLib_OS_WrapperFunctions g_osWrapperFunctions;
WriteErrorReport_Sign g_pWriteErrorReportStubFn = 0;
WriteErrorReport_Sign g_pWriteErrorReportDescFn = 0;
WriteErrorReport_Sign g_pWriteErrorReportAdviceFn = 0;
GetNonScanErrorInfo_Sign g_pGetNonScanErrorInfoFn = 0;
ScanSourceFileLine_Sign g_pScanSourceFileLineFn = 0;
TokenizeLine_Sign g_pTokenizeLineFn = 0;

static std::string _extract_path_from(std::string sPathAndNameWithSlashesAsDirectorySeparator)
{
    auto itFound = sPathAndNameWithSlashesAsDirectorySeparator.find_last_of('/');   // "some/path/to/sourcefile.loc"
    if (itFound != std::string::npos)                                               //              ^
        return sPathAndNameWithSlashesAsDirectorySeparator.substr(0, itFound+1);    // "some/path/to/"
    else
        return "";
}

static std::string _reduce_path(std::string sPathFromRootUTF8WithSlashesAsDirectorySeparator, u16* outError)
{
    std::string sResult = sPathFromRootUTF8WithSlashesAsDirectorySeparator;
    if (sResult.length()) {
        if (sResult.at(0) == '/') {                                         // "/not/allowed/starting/slash/"
            *outError = RERR_INVALID_PATH_FORMAT;
            return "";
        }
        // NOES! _reduce_path can be called upon full file path, including filename
        /*
        if (sResult.at(sResult.length()-1) != '/') {                        // "not/allowed/non/ending/slash"
            *outError = RERR_INVALID_PATH_FORMAT;
            return "";
        }
        */
        if (sResult.find_first_of('\\') != std::string::npos) {             // "not\allowed\format\using\backslashes"
            *outError = RERR_INVALID_PATH_FORMAT;
            return "";
        }
    }

    auto itFound = sResult.find("../");                         //  "this/is/./a/valid/../path/example/"
    while (itFound != std::string::npos) {                      //                     ^
        if (itFound == 0) {
            *outError = RERR_INVALID_PATH_REF_TO_PARENT_DIRECTORY_BEYOND_ROOT;  // "../where/to/go/i/dont/know/"
            return "";
        }
        if (sResult.at(itFound-1) != '/') {
            *outError = RERR_INVALID_PATH_FORMAT;                               // "are/you../serious/"
            return "";
        }
        std::string sBefore = sResult.substr(0, itFound-1);     //  "this/is/./a/valid"
        auto itSlashBefore = sBefore.find_last_of('/');         //              ^
        assert(itSlashBefore != std::string::npos);
        sResult = sBefore.substr(0, itSlashBefore+1) + sResult.substr(itFound + 3);     // "this/is/./a/" + "path/example/"
        // prepares next iteration
        itFound = sResult.find("../");
    }

    itFound = sResult.find("./");                               //  "this/is/./a/path/example/"
    while (itFound != std::string::npos) {                      //           ^
        if (itFound != 0 && sResult.at(itFound-1) != '/') {
            *outError = RERR_INVALID_PATH_FORMAT;                               // "told/you./not/to/do/that/"
            return "";
        }
        sResult = sResult.substr(0, itFound) + sResult.substr(itFound+2);               // "this/is/" + "a/path/example/"
        // prepares next iteration
        itFound = sResult.find("./");
    }

    return sResult;                                             // "this/is/a/path/example/"
}

static std::wstring _convert_to_full_path_utf16(std::string sPathFromRootUTF8)
{
    std::wstring sPathFromRootUTF16 = _convert_to_utf16(sPathFromRootUTF8);
    std::wstring sPathFromRootUTF16usingBackslashes = sPathFromRootUTF16;
    do {
        auto itFound = sPathFromRootUTF16usingBackslashes.find_last_of(L'/');
        if (itFound != std::wstring::npos)
            sPathFromRootUTF16usingBackslashes.replace(itFound, 1, L"\\");
        else
            break;
    } while (true);
    return g_FullPathOfRoot + sPathFromRootUTF16usingBackslashes;
}

int get_source_file_index(int iOpenerIndex, const char* pRelativeFileName, i16 iFileNameByteSize, u16* outError)
{
    assert(iOpenerIndex >= 0);
    assert(iOpenerIndex < int(g_vecAllRegistered.size()));
    RegisteredSourceFile& registeredOpener = g_vecAllRegistered[iOpenerIndex];
    assert(pRelativeFileName);
    assert(iFileNameByteSize > 0);
    std::string sFromRootPathAndName = registeredOpener.sPathFromRootUtf8 + std::string(pRelativeFileName, size_t(iFileNameByteSize));
    // '_reduce_path' will reinterpret './' and '../' and (TODO) maybe links ?
    sFromRootPathAndName = _reduce_path(sFromRootPathAndName, outError);
    if (*outError)
        return -1;
    // once reduced, we can perform a single query against our map to know for sure if we already registered that same file...
    auto itFound = g_mapIndexRegisteredByPathFromRoot.find(sFromRootPathAndName);
    if (itFound != g_mapIndexRegisteredByPathFromRoot.end())
        return itFound->second;
    else {
        // if not already registered, well, register it now (at index = next slot in vector)
        int iIndex = int(g_vecAllRegistered.size());
        g_mapIndexRegisteredByPathFromRoot[sFromRootPathAndName] = iIndex;
        RegisteredSourceFile newRegistered;
        newRegistered.sPathFromRootUtf8 = _extract_path_from(sFromRootPathAndName);
        newRegistered.sAbsPathUtf16 = _convert_to_full_path_utf16(sFromRootPathAndName);
        newRegistered.sFullFilePathUtf16 = newRegistered.sAbsPathUtf16; // TODO ? for the moment, those are same
        newRegistered.sFullFilePathUtf8 = _convert_to_utf8(newRegistered.sFullFilePathUtf16);
        newRegistered.pOpenedFileData = 0;
        newRegistered.iOpenedFileByteSize = 0;
        g_vecAllRegistered.push_back(newRegistered);
        return iIndex;
    }
}

int get_source_file_full_name_byte_count(int iSourceFileIndex)
{
    assert(iSourceFileIndex >= 0);
    assert(iSourceFileIndex < int(g_vecAllRegistered.size()));
    RegisteredSourceFile& registered = g_vecAllRegistered[iSourceFileIndex];
    assert(registered.sFullFilePathUtf8.length() < 4096); // TODO: maybe actually handle those oversizes cases...
    return registered.sFullFilePathUtf8.length();
}

int get_source_file_full_name(int iSourceFileIndex, char* szFileNameBufferMax4096)
{
    assert(iSourceFileIndex >= 0);
    assert(iSourceFileIndex < int(g_vecAllRegistered.size()));
    RegisteredSourceFile& registered = g_vecAllRegistered[iSourceFileIndex];
    assert(registered.sFullFilePathUtf8.length() < 4096); // TODO: maybe actually handle those oversizes cases...
    memcpy(szFileNameBufferMax4096, registered.sFullFilePathUtf8.c_str(), registered.sFullFilePathUtf8.length()+1);
    return registered.sFullFilePathUtf8.length();
}

bool get_source_file_tokenizer(int iSourceFileIndex, TokenizerClosure* outTokenizer, u16* outError)
{
    assert(iSourceFileIndex >= 0);
    assert(iSourceFileIndex < int(g_vecAllRegistered.size()));
    RegisteredSourceFile& registered = g_vecAllRegistered[iSourceFileIndex];
    assert(0 == registered.pOpenedFileData);        // otherwise is currently being read for tokenization elsewhere
    assert(-1 != registered.iOpenedFileByteSize);   // otherwise has already been tokenized
    
    // TODO: have a LocLang-specific bigfile format and handle packed file cases ?
    u64 uByteSize = 0;
    EFileOpenErr fileError = FILE_OPENED_SUCCESSFULLY;
    std::wstring sAbsPathInclExtPrefix = L"\\\\?\\" + registered.sAbsPathUtf16;
    u8* pFileData = read_file_entirely_wchar(sAbsPathInclExtPrefix.c_str(), i32(sAbsPathInclExtPrefix.length()), &uByteSize, &fileError);
    if (pFileData) {
        assert(uByteSize > 0);
        assert(FILE_OPENED_SUCCESSFULLY == fileError);
        if (uByteSize < 40000000) {
            const u8* pFileDataAfterBOM = g_pCheckForByteOrderMarkFn(pFileData, uByteSize, &g_osWrapperFunctions, outError);
            if (pFileDataAfterBOM) {
                assert(0 == *outError);
                GetNextLineTokens_WhenReadingFullFile_ClosureData* pFileReadingState = new GetNextLineTokens_WhenReadingFullFile_ClosureData();
                pFileReadingState->_baseData.iLineIndex = -1;
                pFileReadingState->_baseData.pStartOfCurrentLine = pFileDataAfterBOM;
                pFileReadingState->pNextLineCharScan =    pFileDataAfterBOM;
                pFileReadingState->pAfterLastCharInFile = pFileData + uByteSize;
                outTokenizer->pGetNextLineTokensFn = g_pGetNextLineTokensWhileReadingFullFileFn;
                outTokenizer->pClosureData = reinterpret_cast<TokenizerClosureBaseData*>(pFileReadingState);
                registered.pOpenedFileData = pFileData;
                registered.iOpenedFileByteSize = i64(uByteSize);
                return true;
            } else {
                // invalid BOM
                assert(*outError);
                return false;
            }
        } else {
            // File too big (>= 1GB source code...) : stop right there
            free_file_memory(pFileData, uByteSize);
            *outError = IERR_FILEOPEN_FILE_TOO_BIG;
            return false;
        }
    } else {
        if (fileError) {
            *outError = u16(fileError + IERR_SOURCE_FILE_ERROR_BASE);
            return false;
        } else {
            // case of empty files... TODO: differently ? ATM simply instanciate an actual tokenizer and let it handle that...
            GetNextLineTokens_WhenReadingFullFile_ClosureData* pFileReadingState = new GetNextLineTokens_WhenReadingFullFile_ClosureData();
            pFileReadingState->_baseData.iLineIndex = -1;
            pFileReadingState->_baseData.pStartOfCurrentLine = pFileData;
            pFileReadingState->pNextLineCharScan =    pFileData;
            pFileReadingState->pAfterLastCharInFile = pFileData + uByteSize;
            outTokenizer->pGetNextLineTokensFn = g_pGetNextLineTokensWhileReadingFullFileFn;
            outTokenizer->pClosureData = reinterpret_cast<TokenizerClosureBaseData*>(pFileReadingState);
            registered.pOpenedFileData = pFileData;
            registered.iOpenedFileByteSize = i64(uByteSize);
            return true;
        }

    }
}

void free_source_file_tokenizer(int iSourceFileIndex, TokenizerClosure* pTokenizer)
{
    assert(iSourceFileIndex >= 0);
    assert(iSourceFileIndex < int(g_vecAllRegistered.size()));
    RegisteredSourceFile& registered = g_vecAllRegistered[iSourceFileIndex];
    if (registered.iOpenedFileByteSize) {
        assert(registered.pOpenedFileData);
        free_file_memory(registered.pOpenedFileData, registered.iOpenedFileByteSize);
    }
    registered.pOpenedFileData = 0;
    registered.iOpenedFileByteSize = -1; // to flag it as "already having been tokenized", for debug purposes...
    delete pTokenizer->pClosureData;
    pTokenizer->pClosureData = 0;
    pTokenizer->pGetNextLineTokensFn = 0;
}

bool try_acquire_source_file_reader_resource() {
    // TODO
    return true;
}
bool blocking_acquire_source_file_reader_resource() {
    // TODO
    return true;
}
void release_source_file_reader_resource() {
    // TODO
}

struct ReTokLine {
    const u8* pCurrentLineStart;
    u16 uCurrentLineIndent;
    u16 uCurrentLineTabIndents;
    u16 uCharCountWithoutIndentOrEOL;
    u16 uEOL;
};

GetFilenameFromFileDesc_Sign g_pGetFilenameFromFileDescFn;

#if 0

void log_source_line(StringView line)
{
    StringView viewBefore;
    viewBefore.flags = line.flags & (STRING_VIEW_FLAG_KNOWN_7b_ASCII|STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII|
                                     STRING_VIEW_FLAG_KNOWN_VALID_Utf8|STRING_VIEW_FLAG_KNOWN_INVALID_Utf8);
    const u8* pPrevStart = line.start;
    const u8* pCurrent = line.start;
    const u8* pEnd = line.start + line.uByteLength;
    for (; pCurrent < pEnd; pCurrent++) {
        if (*pCurrent == 0x09) { // tab => replace default output with 4 spaces
            viewBefore.start = pPrevStart;
            viewBefore.uByteLength = u32(pCurrent-pPrevStart);
            log_info(viewBefore, false);
            log_info("    ", false);
            pPrevStart = pCurrent + 1;
        }
    }
    viewBefore.start = pPrevStart;
    viewBefore.uByteLength = u32(pCurrent-pPrevStart);
    log_info(viewBefore, true);
}

void report_detailed_error(int iFileIndex, SourceFileDescAndState* pFileDesc, const LocLib_Error* pError,
                           LocLib_CompilationResults* pWholeResults,
                           LocLib_OS_WrapperFunctions* pOsFuncs)
{
    char* pTmpErrorStub = g_pWriteErrorReportStubFn(pError, pFileDesc, iFileIndex, pWholeResults, 0, pOsFuncs);
    assert(pTmpErrorStub);
    char szTmp[2048];

    //log_info(reinterpret_cast<const char*>(pFileDesc->sourceFileName), false, EUtf8Contents::eUTF8_CONTENTS_UNKNOWN);
    //char szTmp[1024];
    if (pError->errCode >= BASE_OF_TOKENIZER_ERRORS) { // if not a scanner error... we'll show the lines:

        LocLib_ErrorInfo errorInfo;
        g_pGetNonScanErrorInfoFn(pFileDesc, pError, &errorInfo);
        sprintf(szTmp, "%s(%d,%d): error X%d: *** ", g_pGetFilenameFromFileDescFn(pFileDesc),
            errorInfo.uLineOfStatementFromFileStart + errorInfo.uLineOffsetOfTokenFromStatement+1,
            errorInfo.uColumnOfTokenStart+1, u32(pError->errCode));
        log_info(StringView::from_c_str(szTmp), false);

        //sprintf(szTmp, " : *** error %05d, line %6d : ", u32(pError->errCode),
        //    errorInfo.uLineOfStatementFromFileStart + errorInfo.uLineOffsetOfTokenFromStatement + 1);
        //log_info(szTmp, false, EUtf8Contents::eUTF8_CONTENTS_ASCII);
        // TODO: pass optionnal report language override
        char* pTmpErrorStub = g_pWriteErrorReportStubFn(pError, pFileDesc, iFileIndex, pWholeResults, 0, pOsFuncs);
        assert(pTmpErrorStub);
        log_info(StringView::from_c_str(pTmpErrorStub), true);

        assert(iFileIndex >= 0 && iFileIndex < g_vecAllRegistered.size());
        RegisteredSourceFile& registered = g_vecAllRegistered[iFileIndex];
        // TODO: have a LocLang-specific bigfile format and handle packed file cases ?
        u64 uByteSize = 0;
        EFileOpenErr fileError = FILE_OPENED_SUCCESSFULLY;
        std::wstring sAbsPathInclExtPrefix = L"\\\\?\\" + registered.sAbsPathUtf16;
        u8* pFileData = read_file_entirely_wchar(sAbsPathInclExtPrefix.c_str(), i32(sAbsPathInclExtPrefix.length()), &uByteSize, &fileError);
        assert(pFileData);
        assert(FILE_OPENED_SUCCESSFULLY == fileError);
        assert(uByteSize > 0 && uByteSize < 40000000);
        u16 uError = 0;
        const u8* pAfterLastCharInFile = pFileData + uByteSize;
        const u8* pFileDataAfterBOM = g_pCheckForByteOrderMarkFn(pFileData, uByteSize, &g_osWrapperFunctions, &uError);
        assert(pFileDataAfterBOM);
        assert(0 == uError);

        const u8* pStartOfStatementWithError = pFileDataAfterBOM + errorInfo.uByteOffsetOfStatementFromFileStart;
        u32 uByteCountToBeforeError = errorInfo.uByteOffsetOfStatementFromFileStart;
        if (uByteCountToBeforeError > 3000)
            uByteCountToBeforeError -= 3000;
        else
            uByteCountToBeforeError = 0;
        const u8* pStartOfCurrentLine = pFileDataAfterBOM + uByteCountToBeforeError;

        u32 uRememberedCount = 0;
        ReTokLine rememberThree[3];

        while (pStartOfCurrentLine <= pStartOfStatementWithError) {
            if (uRememberedCount == 3) {
                rememberThree[0] = rememberThree[1];
                rememberThree[1] = rememberThree[2];
            } else {
                uRememberedCount++;
            }
            ReTokLine& toRemember = rememberThree[uRememberedCount-1];
            toRemember.pCurrentLineStart = pStartOfCurrentLine;
            const u8* pNextLineCharScan = g_pScanSourceFileLineFn(pStartOfCurrentLine, pAfterLastCharInFile,
                &toRemember.uCurrentLineIndent, &toRemember.uCurrentLineTabIndents, &toRemember.uCharCountWithoutIndentOrEOL,
                &toRemember.uEOL, &uError);
            assert(0 == uError); // otherwise compiler should have reported a scanner error instead of this one...
            if (pStartOfCurrentLine == pNextLineCharScan) {
                log_debug("breaking out of line-finding loop : scanSourceFileLineFn returned same line ???", true);
                break;
            }
            pStartOfCurrentLine = pNextLineCharScan;
        }
        assert(rememberThree[uRememberedCount-1].pCurrentLineStart == pStartOfStatementWithError);
        for (u32 uRemLine = 0; uRemLine < uRememberedCount; uRemLine++) {
            char szTmpLine[2048*4];
            sprintf(szTmpLine, "%6d: ", errorInfo.uLineOfStatementFromFileStart + 1 - (uRememberedCount-1) + uRemLine);
            log_info(StringView::from_c_str(szTmpLine), false);
            u32 uTabs = rememberThree[uRemLine].uCurrentLineTabIndents;
            u32 uNonTabIndents = rememberThree[uRemLine].uCurrentLineIndent - uTabs;
            u32 uLineBytes = uTabs*4 + uNonTabIndents + rememberThree[uRemLine].uCharCountWithoutIndentOrEOL;
            if (uLineBytes) {
                if (uTabs)
                    memset(szTmpLine, ' ', uTabs * 4u);
                memcpy(szTmpLine + uTabs * 4u, rememberThree[uRemLine].pCurrentLineStart + uTabs, uLineBytes - uTabs);
                szTmpLine[uLineBytes] = '\0';
                log_source_line(StringView::from_c_str(szTmpLine));
            } else {
                log_info(StringView::from_c_str(""), true);
            }
        }
        if (errorInfo.uLineOffsetOfTokenFromStatement) {
            bool bShownEllipsis = false;
            for (u32 uOffsetBefore = 0; uOffsetBefore < errorInfo.uLineOffsetOfTokenFromStatement; uOffsetBefore++) {
                ReTokLine& toRemember = rememberThree[uRememberedCount-1];
                toRemember.pCurrentLineStart = pStartOfCurrentLine;
                const u8* pNextLineCharScan = g_pScanSourceFileLineFn(pStartOfCurrentLine, pAfterLastCharInFile,
                    &toRemember.uCurrentLineIndent, &toRemember.uCurrentLineTabIndents, &toRemember.uCharCountWithoutIndentOrEOL,
                    &toRemember.uEOL, &uError);
                assert(0 == uError); // otherwise compiler should have reported a scanner error instead of this one...
                if (errorInfo.uLineOffsetOfTokenFromStatement < 4 ||
                    uOffsetBefore == 0 || uOffsetBefore == errorInfo.uLineOffsetOfTokenFromStatement-1) {
                    char szTmpLine[2048*4];
                    sprintf(szTmpLine, "%6d: ", errorInfo.uLineOfStatementFromFileStart + 1 + uOffsetBefore+1);
                    log_info(StringView::from_c_str(szTmpLine), false);
                    u32 uTabs = toRemember.uCurrentLineTabIndents;
                    u32 uNonTabIndents = toRemember.uCurrentLineIndent - uTabs;
                    u32 uLineBytes = uTabs*4 + uNonTabIndents + toRemember.uCharCountWithoutIndentOrEOL;
                    if (uLineBytes) {
                        if (uTabs)
                            memset(szTmpLine, ' ', uTabs * 4u);
                        memcpy(szTmpLine + uTabs * 4u, toRemember.pCurrentLineStart + uTabs, uLineBytes - uTabs);
                        szTmpLine[uLineBytes] = '\0';
                        log_source_line(StringView::from_c_str(szTmpLine));
                    } else {
                        log_info(StringView::from_c_str(""), true);
                    }
                } else if (!bShownEllipsis) {
                    log_info(StringView::from_c_str("..."), true);
                    bShownEllipsis = true;
                }
                pStartOfCurrentLine = pNextLineCharScan;
            }
        }

        log_info(StringView::from_c_str("        "), false); // 8 spaces for "<line #>: " on other lines
        ReTokLine& lineWithError = rememberThree[uRememberedCount-1];
        for (u16 uByte = 0; uByte < errorInfo.uColumnOfTokenStart; uByte++) {
            if (lineWithError.pCurrentLineStart[uByte] != '\t') // TODO: handle fusion of UTF8 extended chars ??
                log_info(StringView::from_c_str("."), false);
            else
                log_info(StringView::from_c_str(" -> "), false); // TODO: correctly handle tabs and ensure spacing of terminal 
        }
        log_info(StringView::from_c_str("^"), false); // TODO : tokenize and get size of token ?
        Token tTokens[MAX_TOKENS_ON_LINE];
        i32 assumingNoneCommentLevel = 0;
        int iTokenCount = g_pTokenizeLineFn(lineWithError.pCurrentLineStart + lineWithError.uCurrentLineIndent,
            lineWithError.pCurrentLineStart + lineWithError.uCurrentLineIndent + lineWithError.uCharCountWithoutIndentOrEOL,
            &assumingNoneCommentLevel, tTokens, pFileDesc, &uError);
        if (iTokenCount && !uError) { // in case 'assumingNoneCommentLevel' was the wrong assumption, could fail...
            for (int iToken = 0; iToken < iTokenCount; iToken++) {
                Token* pToken = tTokens + iToken;
                // TODO: solve discrepancy between that pos on line and token emitted on error...
                //   a matter of offset from line start vs. from line start without indent ?
                if (pToken->uTokenStartCharOnLine < errorInfo.uColumnOfTokenStart)
                    continue;
                else if (pToken->uTokenStartCharOnLine == errorInfo.uColumnOfTokenStart) {
                    u16 uCharCountOfToken = pToken->uTokenCharCount;
                    for (u16 uCharMore = 1; uCharMore < uCharCountOfToken; uCharMore++) // TODO: handle fusion of UTF8 extended chars ??
                        log_info(StringView::from_c_str("~"), false);
                    break;
                } else {
                    log_info(StringView::from_c_str("?~~"), false);
                    break;
                }
            }
            log_info(StringView::from_c_str(""), true);
        } else {
            log_info(StringView::from_c_str("?~~"), true);
        }

        for (int twoMore = 0; twoMore < 2; twoMore++) {
            if (pStartOfCurrentLine < pAfterLastCharInFile) {
                ReTokLine toDisplayNow;
                const u8* pNextLineCharScan = g_pScanSourceFileLineFn(pStartOfCurrentLine, pAfterLastCharInFile,
                    &toDisplayNow.uCurrentLineIndent, &toDisplayNow.uCurrentLineTabIndents, &toDisplayNow.uCharCountWithoutIndentOrEOL,
                    &toDisplayNow.uEOL, &uError);
                if (!pNextLineCharScan || uError)
                    break;
                char szTmpLine[2048*4];
                sprintf(szTmpLine, "%6d: ", errorInfo.uLineOfStatementFromFileStart + 1 + errorInfo.uLineOffsetOfTokenFromStatement + twoMore + 1);
                log_info(StringView::from_c_str(szTmpLine), false);
                u32 uTabs = toDisplayNow.uCurrentLineTabIndents;
                u32 uNonTabIndents = toDisplayNow.uCurrentLineIndent - uTabs;
                u32 uLineBytes = uTabs*4 + uNonTabIndents + toDisplayNow.uCharCountWithoutIndentOrEOL;
                if (uLineBytes) {
                    if (uTabs)
                        memset(szTmpLine, ' ', uTabs * 4u);
                    memcpy(szTmpLine + uTabs * 4u, pStartOfCurrentLine + uTabs, uLineBytes - uTabs);
                    szTmpLine[uLineBytes] = '\0';
                    log_source_line(StringView::from_c_str(szTmpLine));
                } else {
                    log_info(StringView::from_c_str(""), true);
                }
                pStartOfCurrentLine = pNextLineCharScan;
            } else
                break;
        }

        free_file_memory(pFileData, uByteSize);

    } else {
        sprintf(szTmp, "%s(%d): error S%d: *** ", g_pGetFilenameFromFileDescFn(pFileDesc),
            pError->uBlockOrLineIfScanErr + 1, u32(pError->errCode));

        //sprintf(szTmp, " : *** error %05d, line %6d : ", u32(pError->errCode), pError->uBlockOrLineIfScanErr + 1);
        log_info(StringView::from_c_str(szTmp), false);
        // TODO: pass optionnal report language override
        char* pTmpErrorStub = g_pWriteErrorReportStubFn(pError, pFileDesc, iFileIndex, pWholeResults, 0, pOsFuncs);
        assert(pTmpErrorStub);
        log_info(StringView::from_c_str(pTmpErrorStub), true);
    }

    char* pTmpErrorDesc = g_pWriteErrorReportDescFn(pError, pFileDesc, iFileIndex, pWholeResults, 0, pOsFuncs);
    if (pTmpErrorDesc) {
        log_info(StringView::from_c_str("\nDetailed description : "), false);
        log_info(StringView::from_c_str(pTmpErrorDesc), true);
    }
    char* pTmpErrorAdvice = g_pWriteErrorReportAdviceFn(pError, pFileDesc, iFileIndex, pWholeResults, 0, pOsFuncs);
    if (pTmpErrorAdvice) {
        log_info(StringView::from_c_str("\nAdvice : "), false);
        log_info(StringView::from_c_str(pTmpErrorAdvice), true);
    }
    log_info(StringView::from_c_str(""), true);
}

#endif

GetErrorsOnSourceFile_Sign g_pGetErrorsOnSourceFileFn;

#if 0

void report_errors(int iFileIndex, SourceFileDescAndState* pFileDesc, bool bNeedsToDetailFirstError,
                   LocLib_CompilationResults* pWholeResults,
                   LocLib_OS_WrapperFunctions* pOsFuncs)
{
    u64 uErrorCount;
    const LocLib_Error* tErrors = g_pGetErrorsOnSourceFileFn(pFileDesc, &uErrorCount);
    assert(uErrorCount);
    u32 uError = 0;
    if (bNeedsToDetailFirstError) {
        log_info(StringView::from_c_str("First Error:"), true);
        report_detailed_error(iFileIndex, pFileDesc, tErrors + 0, pWholeResults, pOsFuncs);
        log_info(StringView::from_c_str("\nOther Errors:"), true);
        uError++;
    }
    for (; uError < uErrorCount; uError++) {
        const LocLib_Error* pError = tErrors + uError;
        //log_info(reinterpret_cast<const char*>(pFileDesc->sourceFileName), false, EUtf8Contents::eUTF8_CONTENTS_UNKNOWN);
        char szTmp[2048];
        if (pError->errCode >= BASE_OF_TOKENIZER_ERRORS) {
            LocLib_ErrorInfo errorInfo;
            g_pGetNonScanErrorInfoFn(pFileDesc, pError, &errorInfo);
            sprintf(szTmp, "%s(%d,%d): error X%d: *** ", g_pGetFilenameFromFileDescFn(pFileDesc),
                errorInfo.uLineOfStatementFromFileStart + errorInfo.uLineOffsetOfTokenFromStatement + 1,
                errorInfo.uColumnOfTokenStart+1, u32(pError->errCode));
        } else {
            sprintf(szTmp, "%s(%d): error S%d: *** ", g_pGetFilenameFromFileDescFn(pFileDesc),
                pError->uBlockOrLineIfScanErr + 1, u32(pError->errCode));
        }
        log_info(StringView::from_c_str(szTmp), false);
        // TODO: pass optionnal report language override
        char* pTmpErrorStub = g_pWriteErrorReportStubFn(pError, pFileDesc, iFileIndex, pWholeResults, 0, pOsFuncs);
        log_info(StringView::from_c_str(pTmpErrorStub), true);
    }
}

#endif

struct PlatformFile {
    // EMPTY
};

PlatformFileHandle open_file_for_writing(StringView strFileNameUtf8, EFileOpenErr* outErr, bool bCreateAnew)
{
    strFileNameUtf8.check_utf8();
    if (0 == (strFileNameUtf8.flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII) || 0 == (strFileNameUtf8.flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO)) {
        // TODO: not yet implemented
        assert(false);
    }
    HANDLE hResult = CreateFileA((const char*)strFileNameUtf8.start, bCreateAnew ? GENERIC_WRITE : (GENERIC_READ|GENERIC_WRITE),
                                 0, 0, bCreateAnew ? CREATE_ALWAYS : OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if (hResult != INVALID_HANDLE_VALUE) {
        *outErr = FILE_OPENED_SUCCESSFULLY;
        return reinterpret_cast<PlatformFile*>(hResult);
    } else {
        *outErr = FILEOPEN_INVALID_PARAM; // placeholder. TODO: error codes for file opened on writes, and calling GetLastError() maybe.
        return 0;
    }
}

PlatformFileHandle open_file_for_reading(StringView strFileNameUtf8, EFileOpenErr* outErr)
{
    strFileNameUtf8.check_utf8();
    if (0 == (strFileNameUtf8.flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII) || 0 == (strFileNameUtf8.flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO)) {
        // TODO: not yet implemented
        assert(false);
    }
    HANDLE hResult = CreateFileA((const char*)strFileNameUtf8.start, GENERIC_READ, 0, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hResult != INVALID_HANDLE_VALUE) {
        *outErr = FILE_OPENED_SUCCESSFULLY;
        return reinterpret_cast<PlatformFile*>(hResult);
    } else {
        *outErr = FILEOPEN_INVALID_PARAM; // placeholder. TODO: error codes for file opened on writes, and calling GetLastError() maybe.
        return 0;
    }
}

u32 write_to_file(PlatformFileHandle file, const u8* pData, u32 uByteSize)
{
    HANDLE hFile = reinterpret_cast<HANDLE>(file);
    DWORD uWrittenBytes;
    if (WriteFile(hFile, (const void*)pData, uByteSize, &uWrittenBytes, 0)) {
        return uWrittenBytes;
    } else
        return 0;
}

u32 read_from_file(PlatformFileHandle file, u8* outData, u32 uByteSize)
{
    HANDLE hFile = reinterpret_cast<HANDLE>(file);
    DWORD uReadBytes;
    if (ReadFile(hFile, (void*)outData, uByteSize, &uReadBytes, 0)) {
        return uReadBytes;
    } else
        return 0;
}

void close_file(PlatformFileHandle file)
{
    HANDLE hFile = reinterpret_cast<HANDLE>(file);
    CloseHandle(hFile);
}

bool on_report_error_to_user_should_break(const char* failure_message)
{
    log_info(failure_message);
    return true;
}

void exit_process()
{
    ExitProcess(0xEa70beeFu);
}

RunLocCompilerFromFirstFile_Sign g_pRunLocCompilerFn = 0;
ReleaseCompilationResults_Sign g_pReleaseCompResultsFn = 0;
InitReportLanguage_Sign g_pInitReportLanguageFn = 0;
ReleaseReportLanguage_Sign g_pReleaseReportLanguageFn = 0;
SetPlatformConfsFn* g_pSetPlatformConfsFn = 0;

enum ETestResultExpectation {
    ETRE_SUCCESS,
    ETRE_FAIL_PARSE,
    ETRE_FAIL_TC,
};

static std::unordered_map<u8*, u64> g_sandboxed_allocs;
static std::unordered_map<PlatformFileHandle, bool> g_sandboxed_files;

static u8* page_alloc_sanboxed(u64 uMinBytes)
{
    u8* pResult = page_alloc(uMinBytes);
    if (pResult) {
        g_sandboxed_allocs[pResult] = uMinBytes;
    }
    return pResult;
}

static void page_free_sandboxed(u8* pAllocResult)
{
    if (pAllocResult) {
        g_sandboxed_allocs.erase(pAllocResult);
    }
    page_free(pAllocResult);
}

static u8* read_file_entirely_utf8_sandboxed(StringView strFilePathUtf8, u64* outByteSize, EFileOpenErr* outErr)
{
    u8* pResult = read_file_entirely_utf8(strFilePathUtf8, outByteSize, outErr);
    if (*outErr == FILE_OPENED_SUCCESSFULLY) {
        g_sandboxed_allocs[pResult] = *outByteSize;
    }
    return pResult;
}

static void free_file_memory_sandboxed(u8* pBuffer, u64 uByteSize)
{
    if (pBuffer) {
        g_sandboxed_allocs.erase(pBuffer);
    }
    free_file_memory(pBuffer, uByteSize);
}

static bool on_report_error_to_user_should_break_sandboxed(const char* failure_message)
{
    bool unused = on_report_error_to_user_should_break(failure_message);
    log_info("Handling Panic in Sandboxed mode - now throwing");
    throw std::exception("this is Cpparta"); // Jokefix by @darkblueflow
    return false; // probably not idiomatic either for a semantic 'noreturn'... Oh, well...
}

static PlatformFileHandle open_file_for_writing_sandboxed(StringView strFileNameUTf8, EFileOpenErr* outErr, bool bCreateAnew)
{
    PlatformFileHandle hResult = open_file_for_writing(strFileNameUTf8, outErr, bCreateAnew);
    if (*outErr == FILE_OPENED_SUCCESSFULLY) {
        g_sandboxed_files[hResult] = bCreateAnew;
    }
    return hResult;
}

static void close_file_sandboxed(PlatformFileHandle file)
{
    g_sandboxed_files.erase(file);
    close_file(file);
}

static u32 g_uTotalTests;
static u32 g_uSucceededTests;

int run_tests_against_file(const std::wstring& originalFileNameUTF16, const char* tFilePathUTF8, ETestResultExpectation eExpectedTestResult)
{
    std::string sFirstFilePathUtf8 = std::string(tFilePathUTF8);
    u16 uError = 0;
    sFirstFilePathUtf8 = _reduce_path(sFirstFilePathUtf8, &uError);
    if (uError) {
		log_info("problem with path of first file");
		return -6;
    }

    g_mapIndexRegisteredByPathFromRoot.clear();
    g_vecAllRegistered.clear();

    RegisteredSourceFile firstRegisteredFile;
    firstRegisteredFile.sPathFromRootUtf8 = _extract_path_from(sFirstFilePathUtf8);
    firstRegisteredFile.sAbsPathUtf16 = _convert_to_full_path_utf16(sFirstFilePathUtf8);
    firstRegisteredFile.sFullFilePathUtf16 = firstRegisteredFile.sAbsPathUtf16; // TODO ? for the moment, those are same
    firstRegisteredFile.sFullFilePathUtf8 = _convert_to_utf8(firstRegisteredFile.sFullFilePathUtf16);
    firstRegisteredFile.pOpenedFileData = 0;
    firstRegisteredFile.iOpenedFileByteSize = 0;
    g_vecAllRegistered.push_back(firstRegisteredFile);

    PlatformPageAllocConf theAllocConf;
    theAllocConf.page_alloc = page_alloc_sanboxed;
    theAllocConf.page_free = page_free_sandboxed;
    PlatformLoggingConf theLoggingConf;
    theLoggingConf.on_report_error_to_user_should_break = on_report_error_to_user_should_break_sandboxed;
    theLoggingConf.log_info = log_info_impl;
    theLoggingConf.log_debug = log_debug_impl;
    theLoggingConf.log_error = log_info_impl;
    PlatformIOConf theIOConf;
    theIOConf.load_file_entirely = read_file_entirely_utf8_sandboxed;
    theIOConf.release_whole_file_buffer = free_file_memory_sandboxed;
    theIOConf.exit_process = exit_process;
    theIOConf.open_file_for_writing = open_file_for_writing_sandboxed;
    theIOConf.write_to_file = write_to_file;
    theIOConf.close_file = close_file_sandboxed;

    g_pSetPlatformConfsFn(theAllocConf, theLoggingConf, theIOConf);

    g_osWrapperFunctions.pGetSourceFileIndexFn = get_source_file_index;
    g_osWrapperFunctions.pGetSourceFileFullNameFn = get_source_file_full_name;
    g_osWrapperFunctions.pGetSourceFileFullNameByteCountFn = get_source_file_full_name_byte_count;
    g_osWrapperFunctions.pGetSourceFileTokenizerFn = get_source_file_tokenizer;
    g_osWrapperFunctions.pFreeSourceFileTokenizerFn = free_source_file_tokenizer;
    g_osWrapperFunctions.pTryAcquireSourceFileReaderResourceFn = try_acquire_source_file_reader_resource;
    g_osWrapperFunctions.pBlockingAcquireSourceFileReaderResourceFn = blocking_acquire_source_file_reader_resource;
    g_osWrapperFunctions.pReleaseSourceFileReaderResourceFn = release_source_file_reader_resource;
	
	LocLib_CompilationParams compilationParams;
    // TODO : fundamentally rework those params...
    compilationParams.bRegSizeIs64 = true;
    compilationParams.bPtrSizeIs64 = true;
    compilationParams.bComptimeFloatIs64 = true;
    compilationParams.bLittleEndianTarget = true;
    compilationParams.bRuntimeChecksOn = true;
    compilationParams.uMaxCompintLegs = 4u; // TODO: expand this to go beyond 64b embeddeds... set to what ??? 1024b ? max bigint impl of 64K * 16b ???
    compilationParams.bEmitIRDump = false;
    compilationParams.bEmitAvailableTraces = false;
    compilationParams.bSilentOutput = true;

    log_info("testing compilation from '", false);
    log_info(sFirstFilePathUtf8.c_str(), false);
    log_info("' : ", false);
    try {
        g_uTotalTests++;
	    LocLib_CompilationResults compilationResults;
        if (g_pRunLocCompilerFn(0, &g_osWrapperFunctions, &compilationParams, &compilationResults)) {
	        if (compilationResults.uErrorCount) {
                if (eExpectedTestResult != ETRE_SUCCESS) { // TODO: disctinction parser error / TC error ?
                    log_info(" resulted in compilation error, as expected");
                    g_uSucceededTests++;
                } else {
                    log_info(" resulted in compilation error, when success was expected **** KO ****");
                }
            } else {
                if (eExpectedTestResult == ETRE_SUCCESS) { // TODO: disctinction parser error / TC error ?
                    log_info(" succeeded, as expected");
                    g_uSucceededTests++;
                } else {
                    log_info(" succeeded, when a compiler error was expected **** KO ****");
                }
            }
        } else {
            log_info(" run compilation unexpectedly returned false **** KO ****");
        }
    } catch (...) {
        log_info(" assertion, fault, or exception within compilation library **** KO ****");
    }

    for (auto it = g_sandboxed_files.begin(), itEnd = g_sandboxed_files.end(); it != itEnd; it++) {
        close_file(it->first);
    }
    g_sandboxed_files.clear();
    for (auto it = g_sandboxed_allocs.begin(), itEnd = g_sandboxed_allocs.end(); it != itEnd; it++) {
        page_free(it->first);
    }
    g_sandboxed_allocs.clear();

    return 0;
}

bool _str_starts_with(const char* pPrefix, const char* pToCheck) {
    const char* pCurrentRef = pPrefix;
    const char* pCurrentChk = pToCheck;
    char c = *pCurrentRef;
    while (*pCurrentRef) {
        if (*pCurrentRef != *pCurrentChk)
            return false;
        pCurrentRef++;
        pCurrentChk++;
    }
    return true;
}

int run_tests_with_module(HMODULE hLocDLL)
{
	g_pRunLocCompilerFn = (RunLocCompilerFromFirstFile_Sign)GetProcAddress(hLocDLL, "run_loc_compiler_from_first_file");
    g_pReleaseCompResultsFn = (ReleaseCompilationResults_Sign)GetProcAddress(hLocDLL, "release_compilation_results");
    g_pInitReportLanguageFn = (InitReportLanguage_Sign)GetProcAddress(hLocDLL, "init_report_language");
    g_pReleaseReportLanguageFn = (ReleaseReportLanguage_Sign)GetProcAddress(hLocDLL, "release_report_language");
    g_pSetPlatformConfsFn = (SetPlatformConfsFn*)GetProcAddress(hLocDLL, "set_platform_confs");
    g_pGetFilenameFromFileDescFn = (GetFilenameFromFileDesc_Sign)GetProcAddress(hLocDLL, "get_filename_from_filedesc");
    g_pGetErrorsOnSourceFileFn = (GetErrorsOnSourceFile_Sign)GetProcAddress(hLocDLL, "get_errors_on_sourcefile");
    g_pWriteErrorReportStubFn = (WriteErrorReport_Sign)GetProcAddress(hLocDLL, "write_error_report_stub");
    g_pWriteErrorReportDescFn = (WriteErrorReport_Sign)GetProcAddress(hLocDLL, "write_error_report_desc");
    g_pWriteErrorReportAdviceFn = (WriteErrorReport_Sign)GetProcAddress(hLocDLL, "write_error_report_advice");
    g_pGetNextLineTokensWhileReadingFullFileFn = (GetNextLineTokens_Sign)GetProcAddress(
        hLocDLL, "get_next_line_tokens_when_reading_full_file");
    g_pCheckForByteOrderMarkFn = (CheckForByteOrderMark_Sign)GetProcAddress(hLocDLL, "check_for_BOM");
    g_pGetNonScanErrorInfoFn = (GetNonScanErrorInfo_Sign)GetProcAddress(hLocDLL, "get_non_scan_error_info");
    g_pScanSourceFileLineFn = (ScanSourceFileLine_Sign)GetProcAddress(hLocDLL, "text_scan_next_line");
    g_pTokenizeLineFn = (TokenizeLine_Sign)GetProcAddress(hLocDLL, "tokenize_line");

	if (!g_pRunLocCompilerFn || !g_pReleaseCompResultsFn || !g_pInitReportLanguageFn || !g_pReleaseReportLanguageFn || !g_pSetPlatformConfsFn ||
        !g_pGetFilenameFromFileDescFn || !g_pGetErrorsOnSourceFileFn ||
        !g_pWriteErrorReportStubFn || !g_pWriteErrorReportDescFn || !g_pWriteErrorReportAdviceFn ||
        !g_pGetNextLineTokensWhileReadingFullFileFn || !g_pCheckForByteOrderMarkFn || !g_pGetNonScanErrorInfoFn ||
        !g_pScanSourceFileLineFn || !g_pTokenizeLineFn) {
		log_info("Invalid LOC library: Missing expected functions");
		return -4;
	}

    wchar_t szCurrentDirectory[2048];
    DWORD uWordSize = GetCurrentDirectoryW(2048, szCurrentDirectory);
    g_FullPathOfRoot = std::wstring(szCurrentDirectory);
    if (g_FullPathOfRoot.length() && g_FullPathOfRoot.at(g_FullPathOfRoot.length()-1) != L'\\')
        g_FullPathOfRoot += L"\\";

	char tFilePathUTF8Buffer[32767];

    wchar_t szDir[MAX_PATH];
    HANDLE hFind = INVALID_HANDLE_VALUE;
    wsprintf(szDir, L".\\*");
    WIN32_FIND_DATAW ffd;
    hFind = FindFirstFileW(szDir, &ffd);
    if (INVALID_HANDLE_VALUE == hFind) 
    {
        log_info("error : find first file");
        return -6;
    }

    log_info("Starting tests...\n");
    g_uTotalTests = 0u;
    g_uSucceededTests = 0u;

    do {
        if (ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            // log_info("skipping directory");
        } else {
            std::wstring originalPathUtf16 = std::wstring(ffd.cFileName);
            int iUtf8FilePathBytes = convert_path_to_utf8(tFilePathUTF8Buffer, 32767, originalPathUtf16.c_str(), int(originalPathUtf16.length()) + 1);
            if (iUtf8FilePathBytes <= 0) {
		        log_info("Could not convert source file path to UTF8");
		        return -5;
            }
            if (_str_starts_with("test", tFilePathUTF8Buffer)) {
                if (_str_starts_with("test_success_", tFilePathUTF8Buffer)) {
                    run_tests_against_file(originalPathUtf16, tFilePathUTF8Buffer, ETestResultExpectation::ETRE_SUCCESS);
                } else if (_str_starts_with("test_failparse_", tFilePathUTF8Buffer)) {
                    run_tests_against_file(originalPathUtf16, tFilePathUTF8Buffer, ETestResultExpectation::ETRE_FAIL_PARSE);
                } else if (_str_starts_with("test_failtc_", tFilePathUTF8Buffer)) {
                    run_tests_against_file(originalPathUtf16, tFilePathUTF8Buffer, ETestResultExpectation::ETRE_FAIL_TC);
                } else {
		            log_info(tFilePathUTF8Buffer, false);
                    log_info(" : file name starts with 'test' but is not one of the expected 'test_success_' 'test_failparse_' 'test_failtc_' => skipping", true);
                }
            } else {
                // silently skipping
            }
        }
    } while (FindNextFileW(hFind, &ffd) != 0);
    if (GetLastError() != ERROR_NO_MORE_FILES) {
        log_info("error : find next file");
        return -6;
    }

    char szTmp[1024];
    sprintf(szTmp, "\n%u tests failed (%u/%u succeeded)",
        (g_uTotalTests-g_uSucceededTests), g_uSucceededTests, g_uTotalTests);
    log_info(szTmp);

    return 0;
}

int wmain( int argc, wchar_t *argv[ ], wchar_t *envp[ ] )
{
    if (IsDebuggerPresent()) {
		log_info_impl = log_with_output_debug_string;
        log_debug_impl = log_with_output_debug_string;
    }

	HMODULE hLocDLL = LoadLibraryA("..\\build\\loclib.dll");
	if (hLocDLL) {
        int result = run_tests_with_module(hLocDLL);
        if (result)
            return result;
    } else {
		log_info("Could not find LOC library (loclib.dll) in 'build' directory");
		return -4;
    }

	//log_info("now closing");
	return 0;
}
