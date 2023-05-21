#pragma once 

#ifndef LOCLIB_PE_H_
#define LOCLIB_PE_H_


#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_Cmd_API.h"


const u32 uPositionOfPEHeader = 0x80u;

constexpr u8 tDosHeaderDataBeforePEOffset[] = {
    u8('M'), u8('Z'), 0x90u, 0x00u, 0x03u, 0x00u, 0x00u, 0x00u,
    0x04u, 0x00u, 0x00u, 0x00u, 0xFFu, 0xFFu, 0x00u, 0x00u,
    0xB8u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u,
    0x40u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u,
    0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u,
    0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u,
    0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u, 0x00u,
    0x00u, 0x00u, 0x00u, 0x00u,
};
constexpr u8 tDosHeaderDataAfterPEOffset[] = {
    0x0Eu, 0x1Fu, 0xBAu, 0x0Eu, 0x00u, 0xB4u, 0x09u, 0xCDu,
    0x21u, 0xB8u, 0x01u, 0x4Cu, 0xCDu, 0x21u,
};
constexpr char* szDosStubDisplay = "This program cannot be run in DOS mode.\r\r\n$";

local_func u32 write_padding(PlatformFileHandle file, u32 uPaddingBytes)
{
    if (uPaddingBytes) {
        constexpr u8 t4KZeroes[4096u] = {};
        u32 uRemaining = uPaddingBytes;
        while (uRemaining > 4096u) {
            platform_write_to_file(file, t4KZeroes, 4096u);
            uRemaining -= 4096u;
        }
        platform_write_to_file(file, t4KZeroes, uRemaining);
    }
    return uPaddingBytes;
}

local_func u32 write_data_with_align(PlatformFileHandle file, const u8* pData, u32 uByteCount, u32 uAlign)
{
    Assert_(0u == (uAlign & (uAlign - 1u)));
    u32 uCountBytes = 0u;
    uCountBytes += platform_write_to_file(file, pData, uByteCount);
    u32 uAlignedByteCount = align_to(uAlign, uByteCount);
    u32 uReqPadding = uAlignedByteCount - uByteCount;
    uCountBytes += write_padding(file, uReqPadding);
    return uCountBytes;
}

local_func u32 pe_write_msdos_header_and_stub(PlatformFileHandle file)
{
    u32 uCountBytes = 0u;
    uCountBytes += platform_write_to_file(file, tDosHeaderDataBeforePEOffset, sizeof(tDosHeaderDataBeforePEOffset));
    uCountBytes += platform_write_to_file(file, (const u8*)(&uPositionOfPEHeader), sizeof(u32)); // hopefully little endian
    uCountBytes += platform_write_to_file(file, tDosHeaderDataAfterPEOffset, sizeof(tDosHeaderDataAfterPEOffset));
    uCountBytes += platform_write_to_file(file, (const u8*)szDosStubDisplay, strlen(szDosStubDisplay));
    uCountBytes += write_padding(file, 7u);
    Assert_(uCountBytes == uPositionOfPEHeader);
    return uCountBytes;
}

local_func_inl constexpr u32 pe_get_msdos_header_and_stub_size() { return uPositionOfPEHeader; }

struct COFF_HEADER {
    u16 Machine;
    u16 NumberOfSections;
    u32 TimeDateStamp;
    u32 PointerToSymbolTable;
    u32 NumberOfSymbols;
    u16 SizeOfOptionalHeader;
    u16 Characteristics;
};

local_func_inl constexpr u32 pe_get_coff_header_size() { return u32(sizeof(COFF_HEADER)); }

struct IMAGE_DATA_DIRECTORY {
    u32 VirtualAddress;
    u32 Size;
};

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES    16u

struct OPTIONAL_HEADER {
    u16 Magic;
    u8 MajorLinkerVersion;
    u8 MinorLinkerVersion;
    u32 SizeOfCode;
    u32 SizeOfInitializedData;
    u32 SizeOfUninitializedData;
    u32 AddressOfEntryPoint;
    u32 BaseOfCode;

    u64 ImageBase;
    u32 SectionAlignment;
    u32 FileAlignment;
    u16 MajorOperatingSystemVersion;
    u16 MinorOperatingSystemVersion;
    u16 MajorImageVersion;
    u16 MinorImageVersion;
    u16 MajorSubsystemVersion;
    u16 MinorSubsystemVersion;
    u32 Win32VersionValue;
    u32 SizeOfImage;
    u32 SizeOfHeaders;
    u32 CheckSum;
    u16 Subsystem;
    u16 DllCharacteristics;
    u64 SizeOfStackReserve;
    u64 SizeOfStackCommit;
    u64 SizeOfHeapReserve;
    u64 SizeOfHeapCommit;
    u32 LoaderFlags;
    u32 NumberOfRvaAndSizes;

    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
};

static_assert(sizeof(OPTIONAL_HEADER) == 240u, "size of Optional Header for a 64b executable should be 240 bytes");
local_func_inl constexpr u32 pe_get_optional_header_size() { return u32(sizeof(OPTIONAL_HEADER)); }


#define PE_DEFAULT_IMAGE_BASE       0x0'00400000uLL     // follows defaults for .exe

constexpr u8 tPESignature[] = { u8('P'), u8('E'), 0x00u, 0x00u };
static_assert(sizeof(tPESignature) == 4u, "size of PE signature must be 4 bytes");

local_func u32 pe_write_coff_anb_opt_header(PlatformFileHandle file, const COFF_HEADER& coffHeader, const OPTIONAL_HEADER& optHeader)
{
    u32 uCountBytes = 0u;
    uCountBytes += platform_write_to_file(file, tPESignature, sizeof(tPESignature));
    uCountBytes += platform_write_to_file(file, (const u8*)(&coffHeader), sizeof(COFF_HEADER));
    uCountBytes += platform_write_to_file(file, (const u8*)(&optHeader), sizeof(OPTIONAL_HEADER));
    return uCountBytes;
}

local_func_inl constexpr u32 pe_get_coff_and_opt_header_size() { return u32(sizeof(tPESignature) + sizeof(COFF_HEADER) + sizeof(OPTIONAL_HEADER)); }

struct SECTION_HEADER {
    u8 Name[8u];
    u32 VirtualSize;
    u32 VirtualAddress;
    u32 SizeOfRawData;
    u32 PointerToRawData;
    u32 PointerToRelocations;
    u32 PointerToLineNumbers;
    u16 NumberOfRelocations;
    u16 NumberOfLineNumbers;
    u32 Characteristics;
};

local_func u32 pe_write_section_headers(PlatformFileHandle file, const SECTION_HEADER* pSectionHeaders, u32 uCountSections)
{
    u32 uCountBytes = 0u;
    for (u32 uSection = 0u; uSection < uCountSections; uSection++) {
        uCountBytes += platform_write_to_file(file, (const u8*)(pSectionHeaders + uSection), sizeof(SECTION_HEADER));
    }
    return uCountBytes;
}

local_func_inl constexpr u32 pe_get_section_headers_size(u32 uCountSections) { return u32(sizeof(SECTION_HEADER)) * uCountSections; }

local_func_inl constexpr u32 pe_get_total_header_size(u32 uCountSections) {
    return pe_get_msdos_header_and_stub_size() + pe_get_coff_and_opt_header_size() + pe_get_section_headers_size(uCountSections);
}

#define PE_FILE_ALIGNMENT_ON_DISK       0x0200u // 512
#define PE_SECTION_ALIGNMENT_IN_MEM     0x1000u // 4K

local_func_inl constexpr u32 pe_get_raw_pos_at_start_of_first_section(u32 uCountSections) {
    return align_to(PE_FILE_ALIGNMENT_ON_DISK, pe_get_total_header_size(uCountSections));
}
local_func_inl constexpr u32 pe_get_rva_at_start_of_first_section(u32 uCountSections) {
    return align_to(PE_SECTION_ALIGNMENT_IN_MEM, pe_get_total_header_size(uCountSections));
}

#define FILE_SECTION_CHUNK_SHIFT    12u
#define FILE_SECTION_CHUNK_SIZE     (1u << FILE_SECTION_CHUNK_SHIFT)

struct FileSection {
    u32 uSize;
    u32 uImageSize;
    TmpArray<u8*> vecChunks;
};

local_func void init_file_section(FileSection* ioSection, Arena arena)
{
    ioSection->uSize = 0u;
    ioSection->uImageSize = 0u;
    ioSection->vecChunks.init(arena);
}

local_func void write_to_file_section(FileSection* ioSection, const u8* pData, u32 uCountBytes)
{
    Assert_(uCountBytes);
    Assert_(pData);
    Assert_(ioSection);

    u32 uCapacity = align_to(FILE_SECTION_CHUNK_SIZE, ioSection->uSize);
    u32 uRemainingCapacity = uCapacity - ioSection->uSize;
    u32 uStartChunk = ioSection->uSize >> FILE_SECTION_CHUNK_SHIFT;
    if (uCountBytes <= uRemainingCapacity) {
        u8* pChunkData = ioSection->vecChunks[uStartChunk];
        u32 uStartPosInChunk = (ioSection->uSize & (FILE_SECTION_CHUNK_SIZE - 1u));
        memcpy(pChunkData + uStartPosInChunk, pData, uCountBytes);
        ioSection->uSize += uCountBytes;
    } else {
        u32 uRemainingToWrite = uCountBytes;
        if (uRemainingCapacity) {
            u8* pChunkData = ioSection->vecChunks[uStartChunk];
            u32 uStartPosInChunk = (ioSection->uSize & (FILE_SECTION_CHUNK_SIZE - 1u));
            memcpy(pChunkData + uStartPosInChunk, pData, uRemainingCapacity);
            uRemainingToWrite -= uRemainingCapacity;
            pData += uRemainingCapacity;
        }
        Arena arena = ioSection->vecChunks._alloc.arena;
        while(uRemainingToWrite > FILE_SECTION_CHUNK_SIZE) {
            u8* pNewChunk = alloc_from(arena, FILE_SECTION_CHUNK_SIZE, 8u);
            ioSection->vecChunks.append(pNewChunk);
            memcpy(pNewChunk, pData, FILE_SECTION_CHUNK_SIZE);
            uRemainingToWrite -= FILE_SECTION_CHUNK_SIZE;
            pData += FILE_SECTION_CHUNK_SIZE;
        }
        Assert_(uRemainingToWrite);
        u8* pLastNewChunk = alloc_from(arena, FILE_SECTION_CHUNK_SIZE, 8u);
        ioSection->vecChunks.append(pLastNewChunk);
        memset(pLastNewChunk, 0, FILE_SECTION_CHUNK_SIZE);
        memcpy(pLastNewChunk, pData, uRemainingToWrite);
        ioSection->uSize += uCountBytes;
    }
    /*
    #if TRACE_BACKEND_PRINTLEVEL == 4
        char szTmp[1024];
        sprintf(szTmp, "\tWritten %u bytes to PE section. Starting offset %u, New size %u", uCountBytes, ioSection->uSize - uCountBytes, ioSection->uSize);
        platform_log_debug(szTmp, true);
    #endif
    */
}

constexpr u8 tPaddingData[FILE_SECTION_CHUNK_SIZE] = {};

local_func void pad_file_section_to_align(FileSection* ioSection, u32 uAlignInBytes)
{
    Assert_(ioSection);
    u32 uPrevSize = ioSection->uSize;
    u32 uAligned = align_to(uAlignInBytes, uPrevSize);
    Assert_(uAligned >= uPrevSize);
    u32 uDiffInBytes = uAligned - uPrevSize;
    if (uDiffInBytes) {
        /*
        #if TRACE_BACKEND_PRINTLEVEL == 4
            char szTmp[1024];
            sprintf(szTmp, "\tPadding PE section with %u bytes to match a required alignment of %u bytes:", uDiffInBytes, uAlignInBytes);
            platform_log_debug(szTmp, true);
        #endif
        */
        write_to_file_section(ioSection, tPaddingData, uDiffInBytes);
    }
}

local_func void rewrite_over_file_section(FileSection* ioSection, u32 uOffset, const u8* pData, u32 uCountBytes)
{
    Assert_(uCountBytes);
    Assert_(pData);
    Assert_(ioSection);
    Assert_(uOffset + uCountBytes <= ioSection->uSize);
    u32 uStartChunk = uOffset >> FILE_SECTION_CHUNK_SHIFT;
    u32 uStartPosInChunk = (uOffset & (FILE_SECTION_CHUNK_SIZE - 1u));
    u32 uRemainingCapacity = FILE_SECTION_CHUNK_SIZE - uStartPosInChunk;
    if (uCountBytes <= uRemainingCapacity) {
        u8* pChunkData = ioSection->vecChunks[uStartChunk];
        memcpy(pChunkData + uStartPosInChunk, pData, uCountBytes);
    } else {
        u32 uRemainingToWrite = uCountBytes;
        Assert_(uRemainingCapacity);
        u8* pChunkData = ioSection->vecChunks[uStartChunk];
        memcpy(pChunkData + uStartPosInChunk, pData, uRemainingCapacity);
        uRemainingToWrite -= uRemainingCapacity;
        pData += uRemainingCapacity;

        u32 uCurrentChunk = uStartChunk + 1u;
        while(uRemainingToWrite > FILE_SECTION_CHUNK_SIZE) {
            u8* pChunk = ioSection->vecChunks[uCurrentChunk];
            memcpy(pChunk, pData, FILE_SECTION_CHUNK_SIZE);
            uRemainingToWrite -= FILE_SECTION_CHUNK_SIZE;
            pData += FILE_SECTION_CHUNK_SIZE;
            uCurrentChunk++;
        }
        Assert_(uRemainingToWrite);
        u8* pLastChunk = ioSection->vecChunks[uCurrentChunk];
        memcpy(pLastChunk, pData, uRemainingToWrite);
    }
    /*
    #if TRACE_BACKEND_PRINTLEVEL == 4
        char szTmp[1024];
        sprintf(szTmp, "\tRewritten %u bytes over PE section at offset %u", uCountBytes, uOffset);
        platform_log_debug(szTmp, true);
    #endif
    */
}

local_func void inc32_over_file_section(FileSection* ioSection, u32 uOffset, u32 uToAdd)
{
    Assert_(ioSection);
    Assert_(uOffset + 4u <= ioSection->uSize);
    u32 uStartChunk = uOffset >> FILE_SECTION_CHUNK_SHIFT;
    u32 uStartPosInChunk = (uOffset & (FILE_SECTION_CHUNK_SIZE - 1u));
    u32 uRemainingCapacity = FILE_SECTION_CHUNK_SIZE - uStartPosInChunk;
    u32 uValueBefore;
    u32 uValue;
    u8* pChunkData = ioSection->vecChunks[uStartChunk];
    if (4u <= uRemainingCapacity) {
        memcpy((u8*)&uValue, pChunkData + uStartPosInChunk, 4u);
        uValueBefore = uValue;
        uValue += uToAdd;
        memcpy(pChunkData + uStartPosInChunk, (const u8*)&uValue, 4u);
    } else {
        Assert_(uRemainingCapacity);
        memcpy((u8*)&uValue, pChunkData + uStartPosInChunk, uRemainingCapacity);
        u8* pNextChunkData = ioSection->vecChunks[uStartChunk+1u];
        memcpy(((u8*)&uValue) + uRemainingCapacity, pNextChunkData, 4u - uRemainingCapacity);
        uValueBefore = uValue;
        uValue += uToAdd;
        memcpy(pChunkData + uStartPosInChunk, (const u8*)&uValue, uRemainingCapacity);
        memcpy(pNextChunkData, ((const u8*)&uValue) + uRemainingCapacity, 4u - uRemainingCapacity);
    }
    /*
    #if TRACE_BACKEND_PRINTLEVEL == 4
        char szTmp[1024];
        sprintf(szTmp, "\tReplaced by 32b increase on PE section, PrevValue 0x%08x at offset %u by Value 0x%08x", uValueBefore, uOffset, uValue);
        platform_log_debug(szTmp, true);
    #endif
    */
}

local_func void inc64_over_file_section(FileSection* ioSection, u32 uOffset, u64 uToAdd)
{
    Assert_(ioSection);
    Assert_(uOffset + 8u <= ioSection->uSize);
    u32 uStartChunk = uOffset >> FILE_SECTION_CHUNK_SHIFT;
    u32 uStartPosInChunk = (uOffset & (FILE_SECTION_CHUNK_SIZE - 1u));
    u32 uRemainingCapacity = FILE_SECTION_CHUNK_SIZE - uStartPosInChunk;
    u64 uValueBefore;
    u64 uValue;
    u8* pChunkData = ioSection->vecChunks[uStartChunk];
    if (8u <= uRemainingCapacity) {
        memcpy((u8*)&uValue, pChunkData + uStartPosInChunk, 8u);
        uValueBefore = uValue;
        uValue += uToAdd;
        memcpy(pChunkData + uStartPosInChunk, (const u8*)&uValue, 8u);
    } else {
        Assert_(uRemainingCapacity);
        memcpy((u8*)&uValue, pChunkData + uStartPosInChunk, uRemainingCapacity);
        u8* pNextChunkData = ioSection->vecChunks[uStartChunk+1u];
        memcpy(((u8*)&uValue) + uRemainingCapacity, pNextChunkData, 8u - uRemainingCapacity);
        uValueBefore = uValue;
        uValue += uToAdd;
        memcpy(pChunkData + uStartPosInChunk, (const u8*)&uValue, uRemainingCapacity);
        memcpy(pNextChunkData, ((const u8*)&uValue) + uRemainingCapacity, 8u - uRemainingCapacity);
    }
    /*
    #if TRACE_BACKEND_PRINTLEVEL == 4
        char szTmp[1024];
        sprintf(szTmp, "\tReplaced by 64b increase on PE section, PrevValue 0x%016llx at offset %u by Value 0x%016llx", uValueBefore, uOffset, uValue);
        platform_log_debug(szTmp, true);
    #endif
    */
}

local_func u32 pe_write_file_section(PlatformFileHandle file, const FileSection* pSection)
{
    u32 uCountBytes = 0u;
    if (pSection->uSize) {
        u32 uCountChunks = pSection->vecChunks.size();
        Assert_(uCountChunks);
        u32 uIndexOfLastChunk = uCountChunks - 1u;
        for (u32 uFullChunk = 0u; uFullChunk < uIndexOfLastChunk; uFullChunk++) {
            uCountBytes += platform_write_to_file(file, pSection->vecChunks[uFullChunk], FILE_SECTION_CHUNK_SIZE);
        }
        u32 uSizeInLastChunk = pSection->uSize & (FILE_SECTION_CHUNK_SIZE - 1u);
        Assert_(uSizeInLastChunk);
        uCountBytes += platform_write_to_file(file, pSection->vecChunks[uIndexOfLastChunk], uSizeInLastChunk);
    }
    Assert_(uCountBytes == pSection->uSize);
    return uCountBytes;
}

// A zero-terminated array of those structures should be found at start of .idata section. TODO: what references this, apart from section name ???
struct IMAGE_IMPORT_DESCRIPTOR {
    union {
        u32   Characteristics;
        u32   OriginalFirstThunk;       // RVA of the ILT for this DLL. ILT is a zero-terminated array of 64b entries for PE32+
    };
    u32 TimeDateStamp;                  // 0 for unbound
    u32 ForwarderChain;                 // 0 is maybe ok ?
    u32 Name;                           // RVA of a name (zero-terminated ?)
    u32 FirstThunk;                     // RVA of the IAT. IAT should be same sized and same-filled than ILT (but at a distinct address) ?
                                        //      On load, will be replaced with actual addresses of imported functions (or more generally symbols?).
};
static_assert(sizeof(IMAGE_IMPORT_DESCRIPTOR) == 20u, "hola?");

// Note: 64b ILT: bit 63:whether already known by index (1), or to be solved with an IMPORT_BY_NAME entry (0).
//                       if 1: Bits 0..15 hold ordinal for function, bits 16..62 should be 0.
//                       if 0: Bits 0..30 hold RVA of the associated Hint/Name table 

struct IMAGE_IMPORT_BY_NAME {
    u16 Hint;                           // known ordinal to try first. 0 for unknown ?
    u8  Name[1];                        // trailing array, zero terminated.
};


#endif // LOCLIB_PE_H_
