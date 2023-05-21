#pragma once 

#ifndef LOCLIB_WINX64_BACKEND_BASE_H_
#define LOCLIB_WINX64_BACKEND_BASE_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_ProgramState.h"
#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_NodeValue.h"

#include "LocLib_PE.h"
#include "LocLib_x64.h"

#include <time.h>

enum EPESection {
    PE_SECTION_CODE,
    PE_SECTION_CONST,
    PE_SECTION_GLOBVAR_INI,
    PE_SECTION_GLOBVAR_ZERO,
    PE_SECTION_RELOC,
    PE_SECTION_IMPORTS,

    COUNT_PE_SECTIONS
};

struct OffsetToNotYetEmitted {
    DECL_TRIVIAL_STRUCT_OPS(OffsetToNotYetEmitted);
    u32 uOffset;
    u64 uIROfNotYetEmitted; 
};

struct DLLToImportInfo {
    FFString dllName;
    TmpArray<FFString> vecSymbolNames;
    TmpStringMap<u32> mapAlreadyRegisteredSymbols;
    u32 uStartIndex;
    u32 uStartOffsetInIAT;
};
local_func void init_dll_to_import_info(DLLToImportInfo* ioInfo, FFString dllName, Arena tmpArena)
{
    ioInfo->dllName = dllName;
    ioInfo->vecSymbolNames.init(tmpArena);
    ioInfo->mapAlreadyRegisteredSymbols.init(FireAndForgetArenaAlloc(tmpArena));
    ioInfo->uStartIndex = 0;
    ioInfo->uStartOffsetInIAT = 0;
}

struct NotYetEmittedProc {
    TmpArray<u32> vecOffsetsInCodeToPatchWithProcOffset;        // offsets within code section, where a 32b relative-to-ip was prepared, currently to offset 0 in code section. To be patched with offset to actual proc once known.
    TmpArray<u32> vecOffsetsInConstsToPatchWithProcAddress;     // offsets within const section, where a 64b address was prepared. To be patched with address of actual proc once known.
    TmpArray<u32> vecOffsetsInGlobIniToPatchWithProcAddress;    // offsets within glob-ini section, where a 64b address was prepared. To be patched with address of actual proc once known.
};

struct WinX64BackendCtx : public IRAwareContext {
    FileSection* pSections;
    u32 uActualSectionsCount;
    u32 _pad0;
    Arena secondaryTmpArena;
    u32* pCurrentRepoInfo;
    TmpArray<u64> vecConstDeclarationsToEmit;     // 64b IR of a procwise or filewise global const which is referenced in code
    TmpArray<u64> vecVarDeclarationsToEmit;       // 64b IR of a filewise global var which is referenced in code
    TmpArray<u64> vecProcDeclarationsToEmit;      // 64b IR of a proc which is referenced in code
    TmpArray<u32*> vecTables32bInfo;

    TmpArray<u32> vecOffsetsInCodeToPatchWithConstStart;        // offsets within code section, where a 32b relative-to-IP was written, to a position in const section. Currently written knowing actual virtual address of code section and offset-in-const, but as-if const section started at virtual address 0.
    TmpArray<u32> vecOffsetsInCodeToPatchWithGlobIniStart;      // offsets within code section, where a 32b relative-to-IP was written, to a position in global ini section. Currently written knowing actual virtual address of code section and offset-in-global-ini, but as-if global ini section started at virtual address 0.
    TmpArray<u32> vecOffsetsInCodeToPatchWithGlobZeroStart;     // offsets within code section, where a 32b relative-to-IP was written, to a position in global zeroed section. Currently written knowing actual virtual address of code section and offset-in-global-zeroed, but as-if global zeroed section started at virtual address 0.
    TmpArray<u32> vecOffsetsInCodeToPatchWithImportStart;       // offsets within code section, where a 32b relative-to-IP was written, to a position in import section. Currently written knowing actual virtual address of code section and offset-in-import, but as-if import section started at virtual address 0.

    TmpMap<u64, NotYetEmittedProc> mapVecNotYetEmittedByProc;   // By-IR of not-yet emitted proc.

    TmpArray<u32> vecOffsetsInConstReferringToAddressOfConst;           // offsets within const section, where a 64b address was written, to another position in const section. Currently written as-if const section started at absolute address 0. May also be added to the positions requiring auto-patch by loader...
    TmpArray<u32> vecOffsetsInConstReferringToAddressOfGlobIni;         // offsets within const section, where a 64b address was written, to another position in glob-ini section. Currently written as-if glob-ini section started at absolute address 0. May also be added to the positions requiring auto-patch by loader...
    TmpArray<u32> vecOffsetsInConstReferringToAddressOfGlobZero;        // offsets within const section, where a 64b address was written, to another position in glob-zero section. Currently written as-if glob-zero section started at absolute address 0. May also be added to the positions requiring auto-patch by loader...
    TmpArray<u32> vecOffsetsInConstReferringToAddressOfProc;            // offsets within const section, where a 64b address was written, to the address of a proc. Currently written as-if code section started at absolute address 0. May be added to the positions requiring auto-patch by loader...

    TmpArray<u32> vecOffsetsInGlobIniReferringToAddressOfConst;         // offsets within global ini section, where a 64b address was written, to a position in const section. Currently written as-if const section started at absolute address 0. May also be added to the positions requiring auto-patch by loader...
    TmpArray<u32> vecOffsetsInGlobIniReferringToAddressOfGlobIni;       // offsets within global ini section, where a 64b address was written, to a position in glob-ini section. Currently written as-if const section started at absolute address 0. May also be added to the positions requiring auto-patch by loader...
    TmpArray<u32> vecOffsetsInGlobIniReferringToAddressOfGlobZero;      // offsets within global ini section, where a 64b address was written, to a position in glob-zero section. Currently written as-if const section started at absolute address 0. May also be added to the positions requiring auto-patch by loader...
    TmpArray<u32> vecOffsetsInGlobIniReferringToAddressOfProc;          // offsets within global ini section, where a 64b address was written, to the address of a proc. Currently written as-if code section started at absolute address 0. May be added to the positions requiring auto-patch by loader...

    TmpArray<TmpArray<u32>> vecOfSalvageableVec32s;             // when we remove a tmp vector from a map, we can put it right here to be able to reuse its (tmp!) alloc.

    TmpArray<DLLToImportInfo*> vecDLLToImportInfos;
    u32 uSizeOfDllHeaders;
    u32 uSizeOfILTAndIAT;
    u32 uSizeOfRelocs;
    u32 uPosOfErrHandlingFuncInCodeSection;
};

// Returns offset of entry from the begining of the section
local_func u32 winx64_backend_reserve_initialized_entry_in_section(EPESection eSection, u32 uAlignInBytes, u32 uAlignedByteCount, WinX64BackendCtx* pCtx)
{
    FileSection* pSection = pCtx->pSections + eSection;
    pad_file_section_to_align(pSection, uAlignInBytes);
    u32 uCurrentOffset = pSection->uSize;
    u32 uRemainingToWrite = uAlignedByteCount;
    while (uRemainingToWrite > FILE_SECTION_CHUNK_SIZE) {
        write_to_file_section(pSection, tPaddingData, FILE_SECTION_CHUNK_SIZE);
        uRemainingToWrite -= FILE_SECTION_CHUNK_SIZE;
    }
    Assert_(uRemainingToWrite);
    write_to_file_section(pSection, tPaddingData, uRemainingToWrite);
    return uCurrentOffset;
}

local_func u32 winx64_backend_reserve_global_zeroed_entry(u32 uAlignInBytes, u32 uAlignedByteCount, WinX64BackendCtx* pCtx)
{
    FileSection* pSection = pCtx->pSections + EPESection::PE_SECTION_GLOBVAR_ZERO;
    u32 uSizeBefore = pSection->uImageSize;
    pSection->uImageSize = align_to(uAlignInBytes, uSizeBefore);
    u32 uPaddingBytes = pSection->uImageSize - uSizeBefore;
    u32 uCurrentOffset = pSection->uImageSize;
    pSection->uImageSize += uAlignedByteCount;
    return uCurrentOffset;
}

local_func u32 winx64_backend_on_access_global_var(SourceFileDescAndState* pSourceFile, u32 uVarIndex, u64 uAsIR, WinX64BackendCtx* pCtx)
{
    u32* pRepoInfo = pCtx->vecTables32bInfo[2 + pSourceFile->iRegistrationIndex * 3];
    u32 uCurrentValue = pRepoInfo[uVarIndex];
    if (uCurrentValue)
        return uCurrentValue;

    IREntry& entry = ir_access_repo_instr(&(pSourceFile->filewiseGlobalVarRepo), uVarIndex);
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));
    Assert_(IRIT_GLOBAL_VAR_DECL == u8(entry.uInstrCodeAndFormatAndFirstParam));
    u8 uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16u);
    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
    Assert_(uSlotsCount);
    u32 uAlignLog2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
    u32 uAlignInBytes = (1u << uAlignLog2);
    u32 uByteCount = (1u << get_log2_of_slot_size_from_format(uFormat)) * uSlotsCount;
    u32 uAlignedByteCount = align_to(uAlignInBytes, uByteCount);

    if (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN_ZERO) { // All zeroed
        u32 uCurrentOffset = winx64_backend_reserve_global_zeroed_entry(uAlignInBytes, uAlignedByteCount, pCtx);
        uCurrentValue = uCurrentOffset | ((EPESection::PE_SECTION_GLOBVAR_ZERO + 1u) << 29);
    } else {
        u32 uCurrentOffset = winx64_backend_reserve_initialized_entry_in_section(EPESection::PE_SECTION_GLOBVAR_INI, uAlignInBytes, uAlignedByteCount, pCtx);
        uCurrentValue = uCurrentOffset | ((EPESection::PE_SECTION_GLOBVAR_INI + 1u) << 29);
        pCtx->vecVarDeclarationsToEmit.append(uAsIR);
    }

    pRepoInfo[uVarIndex] = uCurrentValue;
    return uCurrentValue;
}

local_func u32 winx64_backend_on_reserve_const_entry_at_any_scope(const IREntry& entry, WinX64BackendCtx* pCtx)
{
    u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
    Assert_(has_irit_a_value(uIRIT));
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
    u8 uFormat = get_outvalue_format_from_instruction(uIRIT, entry.uInstrCodeAndFormatAndFirstParam);
    u32 uAlignLog2;
    u32 uSlotsCount;
    if (tIRITSecondParamSlot[uIRIT] == IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN) {
        uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
        uAlignLog2 = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
    } else {
        uSlotsCount = 1u;
        uAlignLog2 = get_log2_of_natural_align_from_format(uFormat);
    }
    u32 uAlignInBytes = (1u << uAlignLog2);
    u32 uByteCount = (1u << get_log2_of_slot_size_from_format(uFormat)) * uSlotsCount;
    u32 uAlignedByteCount = align_to(uAlignInBytes, uByteCount);

    return winx64_backend_reserve_initialized_entry_in_section(EPESection::PE_SECTION_CONST, uAlignInBytes, uAlignedByteCount, pCtx);
}
// predecl
u32 winx64_backend_on_access_filewise_const(SourceFileDescAndState* pSourceFile, u32 uConstIndex, u64 uAsIR, WinX64BackendCtx* pCtx);

local_func u32 winx64_backend_on_access_programwise_const(u32 uConstIndex, u64 uAsIR, WinX64BackendCtx* pCtx)
{
    u32* pRepoInfo = pCtx->vecTables32bInfo[0u];
    u32 uCurrentValue = pRepoInfo[uConstIndex];
    if (uCurrentValue)
        return uCurrentValue;

    IREntry& entry = ir_access_repo_instr(&(pCtx->pProgCompilationState->programwiseRepo), uConstIndex);
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
    if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DEREF) {
        u64 uAddressIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
        IRInfo addressInfo;
        ir_get_info_from_ir_param(uAddressIR, 0x03u, pCtx, &addressInfo);
        Assert_(irflag_is_known_or_nyka(addressInfo.uIRandMetaFlags));
        Assert_(0 == (addressInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
        Assert_(irflag_is_known_embd(addressInfo.uIRandMetaFlags));
        i32 iOffsetFromBase; u64 uBaseIR = ir_decode_nyka_value(addressInfo.metaValue.knownValue.uEmbeddedValue, &iOffsetFromBase);
        Assert_(!ir_is_immediate(uBaseIR));
        IRRepo* pBaseRepo;
        u32 uBaseIndex;
        SourceFileDescAndState* pBaseSourceFile;
        EEntryKind eBaseKind;
        ir_decode_non_imm(uBaseIR, pCtx, &pBaseRepo, &uBaseIndex, &pBaseSourceFile, &eBaseKind);
        Assert(eBaseKind != EEntryKind::EEK_IS_PROCBODY_REF, "winx64_backend_on_access_programwise_const() : trying to deref procbody as const");
        u32 uBaseRepoInfo;
        if (eBaseKind == EEntryKind::EEK_PROGRAMWISE_ENTRY) {
            uBaseRepoInfo = winx64_backend_on_access_programwise_const(uBaseIndex, uBaseIR, pCtx);
        } else { Assert_(eBaseKind == EEntryKind::EEK_FILEWISE_CONST); // can't be proc local, and can't be global var, otherwise deref should not be const
            Assert_(pBaseSourceFile);
            uBaseRepoInfo = winx64_backend_on_access_filewise_const(pBaseSourceFile, uBaseIndex, uBaseIR, pCtx);
        }
        Assert_((uBaseRepoInfo >> 29) - 1u == EPESection::PE_SECTION_CONST);
        u32 uBaseOffsetInSection = uBaseRepoInfo & 0x1FFF'FFFFu;
        u32 uResultingOffset = uBaseOffsetInSection + u32(iOffsetFromBase);
        Assert_(uResultingOffset < pCtx->pSections[EPESection::PE_SECTION_CONST].uSize);
        uCurrentValue = uResultingOffset | ((EPESection::PE_SECTION_CONST + 1u) << 29);
    } else {
        u32 uCurrentOffset = winx64_backend_on_reserve_const_entry_at_any_scope(entry, pCtx);
        uCurrentValue = uCurrentOffset | ((EPESection::PE_SECTION_CONST + 1u) << 29);
        pCtx->vecConstDeclarationsToEmit.append(uAsIR);
    }

    pRepoInfo[uConstIndex] = uCurrentValue;
    return uCurrentValue;
}

local_func u32 winx64_backend_on_access_filewise_const(SourceFileDescAndState* pSourceFile, u32 uConstIndex, u64 uAsIR, WinX64BackendCtx* pCtx)
{
    u32* pRepoInfo = pCtx->vecTables32bInfo[1 + pSourceFile->iRegistrationIndex * 3];
    u32 uCurrentValue = pRepoInfo[uConstIndex];
    if (uCurrentValue)
        return uCurrentValue;

    IREntry& entry = ir_access_repo_instr(&(pSourceFile->filewiseConstRepo), uConstIndex);
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
    if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DEREF) {
        u64 uAddressIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
        IRInfo addressInfo;
        ir_get_info_from_ir_param(uAddressIR, 0x03u, pCtx, &addressInfo);
        Assert_(irflag_is_known_or_nyka(addressInfo.uIRandMetaFlags));
        Assert_(0 == (addressInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
        Assert_(irflag_is_known_embd(addressInfo.uIRandMetaFlags));
        i32 iOffsetFromBase; u64 uBaseIR = ir_decode_nyka_value(addressInfo.metaValue.knownValue.uEmbeddedValue, &iOffsetFromBase);
        Assert_(!ir_is_immediate(uBaseIR));
        IRRepo* pBaseRepo;
        u32 uBaseIndex;
        SourceFileDescAndState* pBaseSourceFile;
        EEntryKind eBaseKind;
        ir_decode_non_imm(uBaseIR, pCtx, &pBaseRepo, &uBaseIndex, &pBaseSourceFile, &eBaseKind);
        Assert(eBaseKind != EEntryKind::EEK_IS_PROCBODY_REF, "winx64_backend_on_access_programwise_const() : trying to deref procbody as const");
        u32 uBaseRepoInfo;
        if (eBaseKind == EEntryKind::EEK_PROGRAMWISE_ENTRY) {
            uBaseRepoInfo = winx64_backend_on_access_programwise_const(uBaseIndex, uBaseIR, pCtx);
        } else { Assert_(eBaseKind == EEntryKind::EEK_FILEWISE_CONST); // can't be proc local, and can't be global var, otherwise deref should not be const
            Assert_(pBaseSourceFile);
            uBaseRepoInfo = winx64_backend_on_access_filewise_const(pBaseSourceFile, uBaseIndex, uBaseIR, pCtx);
        }
        Assert_((uBaseRepoInfo >> 29) - 1u == EPESection::PE_SECTION_CONST);
        u32 uBaseOffsetInSection = uBaseRepoInfo & 0x1FFF'FFFFu;
        u32 uResultingOffset = uBaseOffsetInSection + u32(iOffsetFromBase);
        Assert_(uResultingOffset < pCtx->pSections[EPESection::PE_SECTION_CONST].uSize);
        uCurrentValue = uResultingOffset | ((EPESection::PE_SECTION_CONST + 1u) << 29);
    } else {
        u32 uCurrentOffset = winx64_backend_on_reserve_const_entry_at_any_scope(entry, pCtx);
        uCurrentValue = uCurrentOffset | ((EPESection::PE_SECTION_CONST + 1u) << 29);
        pCtx->vecConstDeclarationsToEmit.append(uAsIR);
    }

    pRepoInfo[uConstIndex] = uCurrentValue;
    return uCurrentValue;
}

#define WIN64_BACKEND_LOCAL_KIND_MASK   0xF000'0000u    // 
#define WIN64_BACKEND_LOCAL_VALUE_MASK  0x0FFF'FFFFu    // 
#define WIN64_BACKEND_LOCAL_NONE        0xFFFF'FFFFu    // 
#define WIN64_BACKEND_LOCAL_KIND_REG    0xF000'0000u    // a hardcoded register known for this value
#define WIN64_BACKEND_LOCAL_KIND_PARAM  0xE000'0000u    // input to a callee within this proc
#define WIN64_BACKEND_LOCAL_KIND_DEREF  0xD000'0000u    // 
#define WIN64_BACKEND_LOCAL_KIND_SUBMASK        0xC000'0000u    // if neither of the above, can still be local const (yet emitted in global const section).
#define WIN64_BACKEND_LOCAL_KIND_CONST_SUBMASK  0x8000'0000u    // Remaining 30 bytes 0..29 encode offset in const section.

local_func u32 winx64_backend_on_access_local_const(const IREntry& entry, u32 uIndex, u32* pLocalAssign, TmpArray<u32>& vecLocalConstsToEmit, WinX64BackendCtx* pCtx)
{
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));    
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
    u32 uPrevLocalAssign = pLocalAssign[uIndex];

    if ((uPrevLocalAssign & WIN64_BACKEND_LOCAL_KIND_SUBMASK) == WIN64_BACKEND_LOCAL_KIND_CONST_SUBMASK) {
        return uPrevLocalAssign & 0x3FFF'FFFFu;

    } else { Assert_(uPrevLocalAssign == WIN64_BACKEND_LOCAL_NONE);
        u32 uOffsetInConstSection;
        // if known const is a deref, we'd rather get to the base...
        if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DEREF) {
            u64 uAddressIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
            IRInfo addressInfo;
            ir_get_info_from_ir_param(uAddressIR, 0x03u, pCtx, &addressInfo);
            Assert_(irflag_is_known_or_nyka(addressInfo.uIRandMetaFlags));
            Assert_(0 == (addressInfo.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));
            Assert_(irflag_is_known_embd(addressInfo.uIRandMetaFlags));
            i32 iOffsetFromBase; u64 uBaseIR = ir_decode_nyka_value(addressInfo.metaValue.knownValue.uEmbeddedValue, &iOffsetFromBase);
            Assert_(!ir_is_immediate(uBaseIR));
            IRRepo* pBaseRepo;
            u32 uBaseIndex;
            SourceFileDescAndState* pBaseSourceFile;
            EEntryKind eBaseKind;
            ir_decode_non_imm(uBaseIR, pCtx, &pBaseRepo, &uBaseIndex, &pBaseSourceFile, &eBaseKind);
            Assert(eBaseKind != EEntryKind::EEK_IS_PROCBODY_REF, "winx64_backend_on_access_programwise_const() : trying to deref procbody as const");
            u32 uBaseOffset;
            if (eBaseKind == EEntryKind::EEK_PROGRAMWISE_ENTRY) {
                u32 uBaseRepoInfo = winx64_backend_on_access_programwise_const(uBaseIndex, uBaseIR, pCtx);
                Assert_((uBaseRepoInfo >> 29) - 1u == EPESection::PE_SECTION_CONST);
                uBaseOffset = uBaseRepoInfo & 0x1FFF'FFFFu;
            } else if (eBaseKind == EEntryKind::EEK_FILEWISE_CONST) {
                Assert_(pBaseSourceFile);
                u32 uBaseRepoInfo = winx64_backend_on_access_filewise_const(pBaseSourceFile, uBaseIndex, uBaseIR, pCtx);
                Assert_((uBaseRepoInfo >> 29) - 1u == EPESection::PE_SECTION_CONST);
                uBaseOffset = uBaseRepoInfo & 0x1FFF'FFFFu;
            } else { Assert_(eBaseKind == EEntryKind::EEK_CURRENT_PROC_LOCAL); // can't be global var, otherwise deref should not be const
                Assert_(pBaseRepo == &(pCtx->pProcResult->procwiseRepo));
                IREntry& entry = ir_access_repo_instr(pBaseRepo, uBaseIndex);
                uBaseOffset = winx64_backend_on_access_local_const(entry, uBaseIndex, pLocalAssign, vecLocalConstsToEmit, pCtx);
            }
            uOffsetInConstSection = uBaseOffset + u32(iOffsetFromBase);
            Assert_(uOffsetInConstSection < pCtx->pSections[EPESection::PE_SECTION_CONST].uSize);
        } else {
            uOffsetInConstSection = winx64_backend_on_reserve_const_entry_at_any_scope(entry, pCtx);
            vecLocalConstsToEmit.append(uIndex);
        }

        Assert_(uOffsetInConstSection < 0xC000'0000u);
        pLocalAssign[uIndex] = uOffsetInConstSection | WIN64_BACKEND_LOCAL_KIND_CONST_SUBMASK;
        return uOffsetInConstSection;
    }
}

local_func_inl u32 winx64_backend_on_access_proc(SourceFileDescAndState* pSourceFile, u32 uProcIndex, u64 uAsIR, WinX64BackendCtx* pCtx)
{
    u32* pRepoInfo = pCtx->vecTables32bInfo[3 + pSourceFile->iRegistrationIndex * 3];
    u32 uCurrentValue = pRepoInfo[uProcIndex];
    if (uCurrentValue == 0) {
        TCProcBodyRegistration* pRegisteredProc = pSourceFile->vecAllProcBodies[uProcIndex];
        if (pRegisteredProc->procResult.uIsForeignSource) {
            Assert(false, "stubs for foreign sources should all have been emitted prior to these calls");
        } else {
            pRepoInfo[uProcIndex] = 1u;
            pCtx->vecProcDeclarationsToEmit.append(uAsIR);
        }
    }
    return uCurrentValue;
}

struct ParamInfo {
    u8 uFormat;
    u8 uLog2OfAlign;
    u8 uParamKind;
    u8 _unused;
    u32 uSlotsCount;
};

enum EWin64ParamKind {
    EPARAM_KIND_STD_REG,
    EPARAM_KIND_XMM_REG,
    EPARAM_KIND_REF,

    EPARAM_KIND_UNKNOWN,
    EPARAM_KIND_NOPARAM,
};

local_func_inl void init_func_param_from_type(const TypeInfo* pType, ParamInfo* outInfo)
{
    outInfo->uFormat = get_ir_format(pType);
    outInfo->uLog2OfAlign = u8(get_log2_of_align_bytes(pType));
    outInfo->uSlotsCount = get_slots_count(pType);
    outInfo->uParamKind = EWin64ParamKind::EPARAM_KIND_UNKNOWN;
}

local_func_inl void init_func_param_from_in_entry(const IREntry& entry, ParamInfo* outInfo)
{
    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_CALLER_IN_PARAM);
    outInfo->uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16);
    outInfo->uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
    u32 uLog2OfAlign = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
    Assert_(uLog2OfAlign <= 12u);
    outInfo->uLog2OfAlign = u8(uLog2OfAlign);
    outInfo->uParamKind = EWin64ParamKind::EPARAM_KIND_UNKNOWN;
}

local_func_inl void init_func_param_from_out_entry(const IREntry& entry, ParamInfo* outInfo)
{
    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_CALLER_RET_PARAM);
    outInfo->uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16);
    outInfo->uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
    u32 uLog2OfAlign = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
    Assert_(uLog2OfAlign <= 12u);
    outInfo->uLog2OfAlign = u8(uLog2OfAlign);
    outInfo->uParamKind = EWin64ParamKind::EPARAM_KIND_UNKNOWN;
}

// Returns calling convention
local_func u8 init_func_param_info_from_proc_sign(const TypeInfo_ProcLike* pProcSign,
                                                  ParamInfo* pInParams, u8* oInParamsCount,
                                                  ParamInfo* pOutParams, u8* oOutParamsCount)
{
    u8 uInParamsCount = get_input_param_count(pProcSign);
    u8 uOutParamsCount = u8(pProcSign->params.size()) - uInParamsCount;
    for (u8 uIn = 0u; uIn < uInParamsCount; uIn++) {
        const TypeInfo* pType = pProcSign->params[uIn].pBinding->pType;
        init_func_param_from_type(pType, pInParams + uIn);
    }
    for (u8 uOut = 0u; uOut < uOutParamsCount; uOut++) {
        const TypeInfo* pType = pProcSign->params[uInParamsCount + uOut].pBinding->pType;
        init_func_param_from_type(pType, pOutParams + uOut);
    }
    *oInParamsCount = uInParamsCount;
    *oOutParamsCount = uOutParamsCount;
    return get_proc_kind(pProcSign);
}

local_func u8 init_func_param_info_from_proc_call(const IREntry& callEntry, IRRepo* pRepo, u32 uCallIndex,
                                                  ParamInfo* pInParams, u8* oInParamsCount,
                                                  ParamInfo* pOutParams, u8* oOutParamsCount)
{
    Assert_(u8(callEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_CALL);
    u8 uInParamsCount = u8(callEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
    u8 uOutParamsCount = u8(callEntry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+8));
    *oInParamsCount = uInParamsCount;
    *oOutParamsCount = uOutParamsCount;
    //u8 uDefaultedIn = u8(callEntry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+16));
    for (u8 uIn = 0u; uIn < uInParamsCount; uIn++) {
        const IREntry& paramEntry = ir_access_repo_instr(pRepo, uCallIndex + 1u + uIn);
        init_func_param_from_in_entry(paramEntry, pInParams + uIn);
    }
    for (u8 uOut = 0u; uOut < uOutParamsCount; uOut++) {
        const IREntry& paramEntry = ir_access_repo_instr(pRepo, uCallIndex + 1u + uInParamsCount + uOut);
        init_func_param_from_out_entry(paramEntry, pOutParams + uOut);
    }
    return u8(callEntry.uInstrCodeAndFormatAndFirstParam >> 16);
}

struct WinX64ProcInfo {
    ParamInfo tThisFuncInParams[32u];
    ParamInfo tThisFuncOutParams[32u];
    u8 uInParamsCount, uOutParamsCount, uRetRequiresFirstParamAsPtr, uCallingConv;
    u32 uSizeOfThisFuncParamsOnStack;

    u32 tCountLocalBytesByLog2OfAlign[5u]; // 0..16-bytes as 0..4 log2 of align
    u8 uLog2OfMaxAlignReqForCalls, uLog2OfMaxAlignReqForLocalVars, bCallingOtherProcs;
    u8 _unused0;
    u32 uMaxStackSpaceRequiredForCalls;
    u32 uLocalStackSize;
};

// inits the 'uParamKind' member of each ParamInfo, and evaluates 'uRetRequiresFirstParamAsPtr'
local_func void winx64_eval_proc_param_kinds(WinX64ProcInfo* ioInfo)
{
    Assert(ioInfo->uOutParamsCount <= 1u, "winx64_eval_proc_param_kinds() : win x64 calling convention does not allow for out params count > 1");
    u8 uRetRequiresFirstParamAsPtr = 0;
    if (ioInfo->uOutParamsCount == 1u) {
        u32 uRetSlots = ioInfo->tThisFuncOutParams[0u].uSlotsCount;
        if (uRetSlots) {
            u8 uRetParamKind = EWin64ParamKind::EPARAM_KIND_REF;
            u8 uRetFormat = ioInfo->tThisFuncOutParams[0u].uFormat;
            u8 uRetFitsInStd64Reg = 0u;
            u8 uRetFitsInMMXReg = 0u;
            if (uRetSlots == 1u) {
                if (uRetFormat <= 0x03u) {
                    uRetFitsInStd64Reg = 1u;
                    uRetParamKind = EWin64ParamKind::EPARAM_KIND_STD_REG;
                } else if (get_log2_of_scalar_bytes_from_format(uRetFormat)+get_log2_of_vector_count_from_format(uRetFormat) <= 4u) {
                    uRetFitsInMMXReg = 1u;
                    uRetParamKind = EWin64ParamKind::EPARAM_KIND_XMM_REG;
                }
            } else {
                u32 uBytesPerSlot = 1u << get_log2_of_slot_size_from_format(uRetFormat);
                if (align_to(1u << ioInfo->tThisFuncOutParams[0u].uLog2OfAlign, uBytesPerSlot * uRetSlots) <= 8u) {
                    uRetFitsInStd64Reg = 1u;
                    uRetParamKind = EWin64ParamKind::EPARAM_KIND_STD_REG;
                }
            }
            u8 uRetFitsInRetReg = uRetFitsInStd64Reg | uRetFitsInMMXReg;
            uRetRequiresFirstParamAsPtr = uRetFitsInRetReg ^ 1u;
            ioInfo->tThisFuncOutParams[0u].uParamKind = uRetParamKind;

        } else
            ioInfo->tThisFuncOutParams[0u].uParamKind = EWin64ParamKind::EPARAM_KIND_NOPARAM;
    }
    ioInfo->uRetRequiresFirstParamAsPtr = uRetRequiresFirstParamAsPtr;
    
    u8 uInCount = ioInfo->uInParamsCount;
    for (u8 uIn = 0u; uIn < uInCount; uIn++) {
        u32 uSlots = ioInfo->tThisFuncInParams[uIn].uSlotsCount;
        if (uSlots) {
            u8 uParamKind = EWin64ParamKind::EPARAM_KIND_REF;
            // TODO: encode and check volatility of input param decls. If volatile: necessarily by-ref.
            u8 uFormat = ioInfo->tThisFuncInParams[uIn].uFormat;
            if (uSlots == 1u) {
                if (uFormat <= 0x03u) {
                    uParamKind = EWin64ParamKind::EPARAM_KIND_STD_REG;
                } else if (uFormat <= 0x83u) {
                    uParamKind = EWin64ParamKind::EPARAM_KIND_XMM_REG;
                }
            } else {
                u32 uBytesPerSlot = 1u << get_log2_of_slot_size_from_format(uFormat);
                u32 uAlignedSize = align_to(1u << ioInfo->tThisFuncInParams[uIn].uLog2OfAlign, uBytesPerSlot * uSlots);
                if (uAlignedSize == 1u || uAlignedSize == 2u || uAlignedSize == 4u || uAlignedSize == 8u) {
                    uParamKind = EWin64ParamKind::EPARAM_KIND_STD_REG;
                }
            }
            ioInfo->tThisFuncInParams[uIn].uParamKind = uParamKind;

        } else
            ioInfo->tThisFuncInParams[uIn].uParamKind = EWin64ParamKind::EPARAM_KIND_NOPARAM;
    }
}

local_func void count_stack_usage_and_param_related_info(TCProcBodyResult* pProcResult, WinX64ProcInfo* oOutInfo, u32* oOutLocalAssign, WinX64BackendCtx* pCtx)
{
    // TODO: Use that graph info !!!
    Assert_(pProcResult->pGraphResult);
    // TODO: TODO TODO


    oOutInfo->uCallingConv = init_func_param_info_from_proc_sign(pProcResult->procSign, oOutInfo->tThisFuncInParams, &oOutInfo->uInParamsCount, oOutInfo->tThisFuncOutParams, &oOutInfo->uOutParamsCount);
    // TODO: allow for other calling conventions than win x64 (our in particular - supporting multiple ret values, and possibly usage of a ctx register) -
    //    for current function as well as any callee found here.
    winx64_eval_proc_param_kinds(oOutInfo);

    // Any input param is either up to 64b of a register or 64b on stack, but stack space is reserved even if register (and greater-than 64b is passed by 64b ptr)
    // => stack space reserved for our parameters is straightforward as:
    oOutInfo->uSizeOfThisFuncParamsOnStack = u32(oOutInfo->uInParamsCount + oOutInfo->uRetRequiresFirstParamAsPtr) * 8u;

    //
    // Now we'll browse the full IR for that function and record required stack usage for locals, + any proc calls... 
    // TODO: build graph of blocks and decide of stack usage for locals based on that graph instead (we can reuse space for closed blocks...)
    // TODO: could also be based on some more advanced liveliness anaysis... well, requires graphs of blocks anyway...
    //

    IRRepo* pLocalRepo = &(pProcResult->procwiseRepo);
    u32 uIRCount = pLocalRepo->uSize;

    oOutInfo->tCountLocalBytesByLog2OfAlign[0u] = 0u;
    oOutInfo->tCountLocalBytesByLog2OfAlign[1u] = 0u;
    oOutInfo->tCountLocalBytesByLog2OfAlign[2u] = 0u;
    oOutInfo->tCountLocalBytesByLog2OfAlign[3u] = 0u;
    oOutInfo->tCountLocalBytesByLog2OfAlign[4u] = 0u;
    oOutInfo->bCallingOtherProcs = 0u;
    oOutInfo->uLog2OfMaxAlignReqForCalls = 0u;
    oOutInfo->uMaxStackSpaceRequiredForCalls = 0u;
    oOutInfo->uLog2OfMaxAlignReqForLocalVars = 0u;

    if (!oOutInfo->uRetRequiresFirstParamAsPtr && oOutInfo->uOutParamsCount) {
        Assert_(oOutInfo->uOutParamsCount == 1u);
        u32 uRetSlots = oOutInfo->tThisFuncOutParams[0u].uSlotsCount;
        Assert_(uRetSlots);
        u8 uRetFormat = oOutInfo->tThisFuncOutParams[0u].uFormat;
        u8 uRetAlignLog2 = oOutInfo->tThisFuncOutParams[0u].uLog2OfAlign;
        Assert_(uRetAlignLog2 <= 4u);
        u32 uBytesPerSlot = 1u << get_log2_of_slot_size_from_format(uRetFormat);
        u32 uBytesForRetParam = align_to(1u << uRetAlignLog2, uBytesPerSlot*uRetSlots);
        // TODO: force align to 128 if returned in case returned XMM0 ??
        Assert_(uBytesForRetParam < 128u);
        oOutInfo->tCountLocalBytesByLog2OfAlign[uRetAlignLog2] = uBytesForRetParam;
    }

    WinX64ProcInfo calledProcInfo;
    u8 uTotalParamsCount = oOutInfo->uInParamsCount + oOutInfo->uOutParamsCount;
    for (u32 uIRIndex = uTotalParamsCount; uIRIndex < uIRCount; uIRIndex++) { // loop all IR entries which are not param declarations
        IREntry& entry = ir_access_repo_instr(pLocalRepo, uIRIndex);
        u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
        oOutLocalAssign[uIRIndex] = WIN64_BACKEND_LOCAL_NONE;
        if (uIRIT == IRIT_LOCAL_VAR_DECL || uIRIT == IRIT_CALLER_RET_PARAM) {
            u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
            u32 uAlignLog2 = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
            u8 uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16);
            Assert(uAlignLog2 <= 4u, "count_stack_usage_and_param_related_info() : we currently do not support local variables of alignment > 16 bytes");
            u32 uBytesPerSlot = 1u << get_log2_of_slot_size_from_format(uFormat);
            u32 uAlignedSize = align_to(1u << uAlignLog2, uBytesPerSlot*uSlotsCount);
            oOutInfo->tCountLocalBytesByLog2OfAlign[uAlignLog2] += uAlignedSize; // Stack is indexed 'in reverse' => we increase size before registering offset...
            oOutLocalAssign[uIRIndex] = oOutInfo->tCountLocalBytesByLog2OfAlign[uAlignLog2] | uAlignLog2 << 28u; // bits 28..31 contains align log 2
            if (uAlignLog2 > oOutInfo->uLog2OfMaxAlignReqForLocalVars)
                oOutInfo->uLog2OfMaxAlignReqForLocalVars = u8(uAlignLog2);
        } else if (uIRIT == IRIT_CALL) {
            calledProcInfo = {};
            calledProcInfo.uCallingConv = init_func_param_info_from_proc_call(entry, pLocalRepo, uIRIndex, calledProcInfo.tThisFuncInParams, &calledProcInfo.uInParamsCount, calledProcInfo.tThisFuncOutParams, &calledProcInfo.uOutParamsCount);
            oOutInfo->uLog2OfMaxAlignReqForCalls = 4u; // TODO: maybe depends on calling conv ??
            oOutInfo->bCallingOtherProcs = true;
            winx64_eval_proc_param_kinds(&calledProcInfo);
            u32 uSizeOfCalledFuncParamsOnStack = u32(_max(calledProcInfo.uInParamsCount, u8(4u)) + calledProcInfo.uRetRequiresFirstParamAsPtr) * 8u;
            if (uSizeOfCalledFuncParamsOnStack > oOutInfo->uMaxStackSpaceRequiredForCalls)
                oOutInfo->uMaxStackSpaceRequiredForCalls = uSizeOfCalledFuncParamsOnStack;
        } else if (uIRIT == IRIT_STORE) {
            u64 uDestIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
            Assert_(!ir_is_immediate(uDestIR));
            IRRepo* pDestRepo;
            u32 uDestIndex;
            SourceFileDescAndState* pDestSourceFile;
            EEntryKind eDestKind;
            ir_decode_non_imm(uDestIR, pCtx, &pDestRepo, &uDestIndex, &pDestSourceFile, &eDestKind);
            if (eDestKind == EEntryKind::EEK_CURRENT_PROC_LOCAL) {
                Assert_(uDestIndex >= oOutInfo->uInParamsCount);
                if (uDestIndex == oOutInfo->uInParamsCount) {
                    // We found a store to out param... TODO: record this, and try to determine whether it is obvious that it is last such store before 'ret'.
                }
            }
        } else if (uIRIT == IRIT_CALLER_IN_PARAM) {
            // TODO: check volatility of passed content vs. non-volatile of proc decl (TODO: add a way to flag volatility of decl in param)
            // if non-volatile decl and volatile content, we'd need to create some more stack space for a local copy before proccall.
        } else {
            // since our IR is pseudo SSA, any instruction being a non-const value is qualifiable as also requiring possible stack space...
            //   (unless it is an offset to something else, or a 'deref' instruction, or a 'reinterp' instruction, or a pseudo-value, or a comparison only for the purpose
            //    of an adjacent branch instruction)
            // TODO: in case 'reinterp' is not between single integral formats, we should only 'skip' a slot for reinterp there if we're sure the source is *not* a single register. 
            if (uIRIT != IRIT_PTR_OFFSET && uIRIT != IRIT_PTR_OFFSET_EXT && uIRIT != IRIT_LOCAL_ADDRESS && uIRIT != IRIT_DEREF &&
                uIRIT != IRIT_REINTERP && uIRIT != IRIT_PSEUDO_VALUED_COND && has_irit_a_value(uIRIT) &&
                (0uLL == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) || (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)) &&
                ((uIRIT != IRIT_CMP_EQ && uIRIT != IRIT_CMP_ORD) || 0uLL == (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES)))
            {
                u8 uFormat = get_outvalue_format_from_instruction(uIRIT, entry.uInstrCodeAndFormatAndFirstParam);
                u32 uAlignLog2 = get_log2_of_natural_align_from_format(uFormat);
                u32 uSlotCount = 1u;
                if (tIRITSecondParamSlot[uIRIT] == IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN) {
                    uSlotCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    uAlignLog2 = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
                }
                Assert(uAlignLog2 <= 4u, "count_stack_usage_and_param_related_info() : we currently do not support intermediate computation of alignment > 16 bytes");
                u32 uBytesPerSlot = 1u << get_log2_of_slot_size_from_format(uFormat);
                u32 uAlignedSize = align_to(1u << uAlignLog2, uBytesPerSlot * uSlotCount);
                oOutInfo->tCountLocalBytesByLog2OfAlign[uAlignLog2] += uAlignedSize; // Stack is indexed 'in reverse' => we increase size before registering offset...
                oOutLocalAssign[uIRIndex] = oOutInfo->tCountLocalBytesByLog2OfAlign[uAlignLog2] | uAlignLog2 << 28u; // bits 28..31 contains align log 2
                if (uAlignLog2 > oOutInfo->uLog2OfMaxAlignReqForLocalVars)
                    oOutInfo->uLog2OfMaxAlignReqForLocalVars = u8(uAlignLog2);
            }
        }
    }

    // We'll do another pass on all those:
    //   if any PTR_OFFSET is referencing an address from another PTR_OFFSET, then decide to allocate storage space for the other one.
    // TODO: also specially flag local runtime values which get taken "address-of", then use that in yet another pass
    //   to decide whether non-such-flaggued values could get very-dumbly assigned to some register (preferring those on 64b for R10..R14).
    // TODO: also detect reinterp for which source would be a register operand, and also assign a slot to them.
    for (u32 uIRIndex = uTotalParamsCount; uIRIndex < uIRCount; uIRIndex++) { // loop all IR entries which are not param declarations
        IREntry& entry = ir_access_repo_instr(pLocalRepo, uIRIndex);
        u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
        if (uIRIT == IRIT_PTR_OFFSET || uIRIT == IRIT_PTR_OFFSET_EXT) {
            u64 uIRofBaseAddress = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
            if (0uLL == (uIRofBaseAddress & IR_STD_PARAM_HIGHMASK) && u16(uIRofBaseAddress >> IR_STD_PARAM_REPO_ID_SHIFT) == IR_REPO_ID_CURRENT_PROC) {
                u32 uLocalIndexOfBaseAddress = u32(uIRofBaseAddress >> IR_STD_PARAM_SHIFT) & 0x00FF'FFFFu;
                if (oOutLocalAssign[uLocalIndexOfBaseAddress] == WIN64_BACKEND_LOCAL_NONE) {
                    IREntry& localBaseAddressEntry = ir_access_repo_instr(pLocalRepo, uLocalIndexOfBaseAddress);
                    u8 uOtherIRIT = u8(localBaseAddressEntry.uInstrCodeAndFormatAndFirstParam);
                    if (uOtherIRIT == IRIT_PTR_OFFSET || uOtherIRIT == IRIT_PTR_OFFSET_EXT) {
                        constexpr u8 uFormat = 0x03u;
                        constexpr u32 uAlignLog2 = 3u;
                        constexpr u32 uBytesPerSlot = 8u;
                        oOutInfo->tCountLocalBytesByLog2OfAlign[uAlignLog2] += uBytesPerSlot; // Stack is indexed 'in reverse' => we increase size before registering offset...
                        oOutLocalAssign[uLocalIndexOfBaseAddress] = oOutInfo->tCountLocalBytesByLog2OfAlign[uAlignLog2] | uAlignLog2 << 28u; // bits 28..31 contains align log 2
                        if (uAlignLog2 > oOutInfo->uLog2OfMaxAlignReqForLocalVars)
                            oOutInfo->uLog2OfMaxAlignReqForLocalVars = u8(uAlignLog2);
                    }
                }
            }
        }
    }


    // Once we know about all this info, we can decide of stack space.
    // At proc call, 8-Bytes ret address is stored after a hopefully ensured 16-Bytes aligned stack. This also seems to be the case for first function called from loader.
    // => if we add another 8 bytes to the stack, we're ensured 16-Bytes aligned again.
    
    u32 tOffsetsAfterBaseOfStartByAlign[5u] = {};
    tOffsetsAfterBaseOfStartByAlign[3u] = 8u; // we'll pack *all* 8-Bytes aligned locals after return address on stack (already 8-bytes aligned).
    tOffsetsAfterBaseOfStartByAlign[2u] = tOffsetsAfterBaseOfStartByAlign[3u] + oOutInfo->tCountLocalBytesByLog2OfAlign[3u]; // all 4-bytes aligned locals after that
    tOffsetsAfterBaseOfStartByAlign[1u] = tOffsetsAfterBaseOfStartByAlign[2u] + oOutInfo->tCountLocalBytesByLog2OfAlign[2u]; // all 2-bytes aligned locals after that
    tOffsetsAfterBaseOfStartByAlign[0u] = tOffsetsAfterBaseOfStartByAlign[1u] + oOutInfo->tCountLocalBytesByLog2OfAlign[1u]; // all 1-byte aligned locals after that
    u32 uCurrentLocalStackSize = tOffsetsAfterBaseOfStartByAlign[0u] + oOutInfo->tCountLocalBytesByLog2OfAlign[0u];
    
    if (oOutInfo->uLog2OfMaxAlignReqForLocalVars > 3u) { // only if need be, we ensure of 16B-align for local vars, and pack all 16B-aligned after that.
        Assert_(oOutInfo->uLog2OfMaxAlignReqForLocalVars == 4u);
        Assert_(oOutInfo->tCountLocalBytesByLog2OfAlign[4u]);
        uCurrentLocalStackSize = align_to(16u, uCurrentLocalStackSize);
        tOffsetsAfterBaseOfStartByAlign[4u] = uCurrentLocalStackSize;
        uCurrentLocalStackSize += oOutInfo->tCountLocalBytesByLog2OfAlign[4u];
        Assert_(0u == (uCurrentLocalStackSize & 0x0Fu)); // all 16-Bytes aligned locals should have a 16-Bytes aligned size also => we should still be 16-B aligned now.
    }

    if (pProcResult->vecErrChecks.size()) { // we'll need to call at least the err-handler in case there are errs => also counts as a call, requiring stack align 16.
        oOutInfo->bCallingOtherProcs = 1u;
        oOutInfo->uLog2OfMaxAlignReqForCalls = 4u;
    }

    if (oOutInfo->bCallingOtherProcs) {
        uCurrentLocalStackSize = align_to(8u, uCurrentLocalStackSize); // all params-on-stack should be 8-Bytes aligned at least.
        u32 uAfterLocals = uCurrentLocalStackSize;
        uCurrentLocalStackSize += oOutInfo->uMaxStackSpaceRequiredForCalls;
        uCurrentLocalStackSize = align_to(1u << oOutInfo->uLog2OfMaxAlignReqForCalls, uCurrentLocalStackSize);
        oOutInfo->uMaxStackSpaceRequiredForCalls = uCurrentLocalStackSize - uAfterLocals;
    }

    Assert_(uCurrentLocalStackSize >= 8u);
    oOutInfo->uLocalStackSize = uCurrentLocalStackSize - 8u; // we remove return address on stack from our registered stack size...

    // We'll do another pass over all IR to assign all locals to their slots:

    // First: input params to their shadow space
    for (u32 uIRIndex = 0u; uIRIndex < oOutInfo->uInParamsCount; uIRIndex++) {
        oOutLocalAssign[uIRIndex] = uCurrentLocalStackSize + 8u * (uIRIndex + oOutInfo->uRetRequiresFirstParamAsPtr); // stack is build in reverse => pos = stack size (including 8-Bytes ret address) + 0 for offset 0, 8 for offset 1...
    }

    // Next: ret param
    if (oOutInfo->uOutParamsCount) {
        Assert_(oOutInfo->uOutParamsCount == 1u);
        if (oOutInfo->uRetRequiresFirstParamAsPtr) {
            oOutLocalAssign[oOutInfo->uInParamsCount] = uCurrentLocalStackSize + 8u; // stack is build in reverse => pos = stack size (including 8-Bytes ret address) + 0 for offset 0 (as in case of a forced-by-ref retparam, ret is like first input param).
        } else {
            u8 uRetAlignLog2 = oOutInfo->tThisFuncOutParams[0u].uLog2OfAlign;
            Assert_(uRetAlignLog2 <= 4u);
            u32 uSize = (1u << get_log2_of_slot_size_from_format(oOutInfo->tThisFuncOutParams[0u].uFormat)) * oOutInfo->tThisFuncOutParams[0u].uSlotsCount;
            oOutLocalAssign[oOutInfo->uInParamsCount] = uCurrentLocalStackSize - (tOffsetsAfterBaseOfStartByAlign[uRetAlignLog2] + align_to(1u << uRetAlignLog2, uSize)); // stack is indexed in reverse => pos = stack-size - (offset to specified align (first slot) + aligned-size) 
        }
    }

    // Then : all other IR with local space
    for (u32 uIRIndex = uTotalParamsCount; uIRIndex < uIRCount; uIRIndex++) {
        if (oOutLocalAssign[uIRIndex] != WIN64_BACKEND_LOCAL_NONE) {
            if ((oOutLocalAssign[uIRIndex] & WIN64_BACKEND_LOCAL_KIND_MASK) == WIN64_BACKEND_LOCAL_KIND_PARAM) { // in case this slot is for an input param to a callee
                u32 uParamIndex = oOutLocalAssign[uIRIndex] & WIN64_BACKEND_LOCAL_VALUE_MASK;
                oOutLocalAssign[uIRIndex] = uParamIndex * 8u;
            } else if ((oOutLocalAssign[uIRIndex] & WIN64_BACKEND_LOCAL_KIND_MASK) != WIN64_BACKEND_LOCAL_KIND_REG) { // Regular local slot:
                u8 uLog2OfAlign = u8(oOutLocalAssign[uIRIndex] >> 28);
                Assert_(uLog2OfAlign <= 4u);
                u32 uOffsetOfBaseWithThisAlign = tOffsetsAfterBaseOfStartByAlign[uLog2OfAlign];
                u32 uOffsetFromAllWithThisAlign = oOutLocalAssign[uIRIndex] & WIN64_BACKEND_LOCAL_VALUE_MASK;
                oOutLocalAssign[uIRIndex] = uCurrentLocalStackSize - (uOffsetOfBaseWithThisAlign + uOffsetFromAllWithThisAlign);
            } // Otherwise reg: we leave as-is...
        }
    }
}

local_func void append_offset_from_ip_to_patch(EPESection eRelativeToWhere, u32 uPosInCode, WinX64BackendCtx* pCtx)
{
    switch (eRelativeToWhere) {
        case EPESection::PE_SECTION_CONST:
            pCtx->vecOffsetsInCodeToPatchWithConstStart.append(uPosInCode);
            break;
        case EPESection::PE_SECTION_GLOBVAR_INI:
            pCtx->vecOffsetsInCodeToPatchWithGlobIniStart.append(uPosInCode);
            break;
        case EPESection::PE_SECTION_GLOBVAR_ZERO:
            pCtx->vecOffsetsInCodeToPatchWithGlobZeroStart.append(uPosInCode);
            break;
        case EPESection::PE_SECTION_IMPORTS:
            pCtx->vecOffsetsInCodeToPatchWithImportStart.append(uPosInCode);
            break;
        case EPESection::PE_SECTION_CODE:
            // NOOP
            break;
        default:
            Assume_(false);
            break;
    }
}

local_func void append_offset_from_ip_to_patch_with_nyep(u64 uIRofProc, u32 uPosInCode, WinX64BackendCtx* pCtx)
{
    auto itThisProc = pCtx->mapVecNotYetEmittedByProc.find(uIRofProc);
    if (itThisProc == pCtx->mapVecNotYetEmittedByProc.end()) {
        NotYetEmittedProc newNYEP;
        newNYEP.vecOffsetsInCodeToPatchWithProcOffset.init(pCtx->mapVecNotYetEmittedByProc._alloc);
        newNYEP.vecOffsetsInConstsToPatchWithProcAddress.init(pCtx->mapVecNotYetEmittedByProc._alloc);
        newNYEP.vecOffsetsInGlobIniToPatchWithProcAddress.init(pCtx->mapVecNotYetEmittedByProc._alloc);
        itThisProc = pCtx->mapVecNotYetEmittedByProc.insert(uIRofProc, newNYEP);
    }
    itThisProc.value().vecOffsetsInCodeToPatchWithProcOffset.append(uPosInCode);
}

local_func_inl void append_offset_from_ip_to_patch(const X64OperandRef& operand, u32 uPosInCode, WinX64BackendCtx* pCtx) {
    Assert_(operand.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED);
    if (operand.uFlags & X64OPERAND_FLAG_IP_BASED_IS_NYEP)
        append_offset_from_ip_to_patch_with_nyep(additional_payload_from_operand_ip_based_address(operand), uPosInCode, pCtx);
    else
        append_offset_from_ip_to_patch(EPESection(u8(additional_payload_from_operand_ip_based_address(operand))), uPosInCode, pCtx);
}

local_func void winx64_emit_dest_src_op(EX64DestSrcOp eOp, X64OperandRef dest, X64OperandRef src, u8 uFormat, WinX64BackendCtx* pCtx)
{
    u32 uInstructionPosInCode = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;

    u8 tData[32];
    u8 uToOffsetDest = 0;
    u8 uToOffsetSrc = 0;

    u8 uInstructionBytes = x64_encode_dest_src_op(eOp, dest, src, uFormat, tData, uInstructionPosInCode, &uToOffsetDest, &uToOffsetSrc);

    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uInstructionBytes);

    if (dest.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(uToOffsetDest && uToOffsetDest + 4u <= uInstructionBytes);
        append_offset_from_ip_to_patch(dest, uInstructionPosInCode + u32(uToOffsetDest), pCtx);
    }
    if (src.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(uToOffsetSrc && uToOffsetSrc + 4u <= uInstructionBytes);
        append_offset_from_ip_to_patch(src, uInstructionPosInCode + u32(uToOffsetSrc), pCtx);
    }
}

// Similar to winx64_emit_dest_src_op, but will handle going through intermediate register RBX in case of 2 memory operands (or in case src is imm and imm to dest isn't supported)
local_func void winx64_handle_emit_dest_src_op(EX64DestSrcOp eOp, X64OperandRef dest, X64OperandRef src, u8 uFormat, WinX64BackendCtx* pCtx)
{
    u32 uInstructionPosInCode = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;

    u8 tData[64];
    u8 uToOffsetDest = 0;
    u8 uToOffsetSrc = 0;

    Assert_(0 == (dest.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    if ((dest.uFlags & X64OPERAND_FLAG_IS_REG) | (src.uFlags & (X64OPERAND_FLAG_IS_REG|X64OPERAND_FLAG_IS_IMMEDIATE))) {
        if (src.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
            const X64DestSrcOp& op = x64_tDestSrcOps[eOp];
            if (op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED)
                goto when_standard;
            if (dest.uFlags & X64OPERAND_FLAG_IS_REG) {
                if ((op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED) && ((op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALL8R_PLUS) ||
                        ((dest.uFlags & X64OPERAND_FLAG_IS_REG) && reg_code_from_operand_reg(dest) == REG_X64_xAX)))
                    goto when_standard;
            }
            // Otherwise : we go through RBX for source
            u8 uIntermediateFormat = uFormat;
            if (uFormat == 0x03u) {
                u64 uAsImmediate = immu64_from_operand_imm(src);
                if (can_encode_32b_signed(uAsImmediate)) {
                    if (i32(uAsImmediate) >= 0) {
                        uIntermediateFormat = 0x02u; // takes advantage of the fact that high 32b of RBX will get zeroed out, to encode a much smaller MOV instruction.
                    }
                }
            }
            u8 uUnused1 = 0;
            u8 uUnused2 = 0;
            X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
            u8 uInstructionBytes1 = x64_encode_dest_src_op(EX64_DEST_SRC_OP_MOV, xBX, src, uIntermediateFormat,
                                                           tData, uInstructionPosInCode, &uUnused1, &uUnused2);
            Assert_(uUnused1 == 0);
            Assert_(uUnused2 == 0);
            u8 uInstructionBytes2 = x64_encode_dest_src_op(eOp, dest, xBX, uFormat,
                                                           tData+uInstructionBytes1, uInstructionPosInCode+uInstructionBytes1, &uToOffsetDest, &uUnused2);
            Assert_(uUnused2 == 0);
            uToOffsetDest += uInstructionBytes1;
            write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uInstructionBytes1+uInstructionBytes2);

        } else { when_standard:
            u8 uInstructionBytes = x64_encode_dest_src_op(eOp, dest, src, uFormat,
                                                          tData, uInstructionPosInCode, &uToOffsetDest, &uToOffsetSrc);
            write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uInstructionBytes);
        }
    } else {
        u8 uUnused = 0;
        X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
        u8 uInstructionBytes1 = x64_encode_dest_src_op(EX64_DEST_SRC_OP_MOV, xBX, src, uFormat,
                                                       tData, uInstructionPosInCode, &uUnused, &uToOffsetSrc);
        Assert_(uUnused == 0);
        u8 uInstructionBytes2 = x64_encode_dest_src_op(eOp, dest, xBX, uFormat,
                                                       tData+uInstructionBytes1, uInstructionPosInCode+uInstructionBytes1, &uToOffsetDest, &uUnused);
        Assert_(uUnused == 0);
        uToOffsetDest += uInstructionBytes1;
        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uInstructionBytes1 + uInstructionBytes2);
    }

    if (dest.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(uToOffsetDest);
        append_offset_from_ip_to_patch(dest, uInstructionPosInCode + u32(uToOffsetDest), pCtx);
    }
    if (src.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(uToOffsetSrc);
        append_offset_from_ip_to_patch(src, uInstructionPosInCode + u32(uToOffsetSrc), pCtx);
    }
}

local_func void winx64_emit_singleRM_op(EX64SingleRMOp eOp, X64OperandRef operand, u8 uFormat, WinX64BackendCtx* pCtx)
{
    u32 uInstructionPosInCode = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;

    u8 tData[32];
    u8 uToOffsetRM = 0;

    u8 uInstructionBytes = x64_encode_singleRM_op(eOp, operand, uFormat, tData, uInstructionPosInCode, &uToOffsetRM);

    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uInstructionBytes);

    if (operand.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(uToOffsetRM && uToOffsetRM + 4u <= uInstructionBytes);
        append_offset_from_ip_to_patch(operand, uInstructionPosInCode + u32(uToOffsetRM), pCtx);
    }
}

// Similar to winx64_emit_singleRM_op, but will handle going through intermediate register RBX in case of immediate
local_func void winx64_handle_emit_singleRM_op(EX64SingleRMOp eOp, X64OperandRef operand, u8 uFormat, WinX64BackendCtx* pCtx)
{
    if (operand.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
        u8 tData[64];
        u8 uUnused1 = 0;
        u8 uUnused2 = 0;
        X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
        u8 uIntermediateFormat = uFormat;
        if (uFormat == 0x03u) {
            u64 uAsImmediate = immu64_from_operand_imm(operand);
            if (can_encode_32b_signed(uAsImmediate)) {
                if (i32(uAsImmediate) >= 0) {
                    uIntermediateFormat = 0x02u; // takes advantage of the fact that high 32b of RBX will get zeroed out, to encode a much smaller MOV instruction.
                }
            }
        }
        u8 uInstructionBytes1 = x64_encode_dest_src_op(EX64_DEST_SRC_OP_MOV, xBX, operand, uIntermediateFormat,
                                                       tData, 0u, &uUnused1, &uUnused2);
        Assert_(uUnused1 == 0);
        Assert_(uUnused2 == 0);
        u8 uInstructionBytes2 = x64_encode_singleRM_op(eOp, xBX, uFormat, tData + uInstructionBytes1, 0u, &uUnused1);
        Assert_(uUnused1 == 0);

        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uInstructionBytes1 + uInstructionBytes2);

    } else {
        winx64_emit_singleRM_op(eOp, operand, uFormat, pCtx);
    }
}



enum EExpectedX64ImmFormat {
    EX64_IMM_FORMAT_FLOAT32,    // Can accomodate F32 immediates
    EX64_IMM_FORMAT_INT8,       // Can accomodate R32 payload
    EX64_IMM_FORMAT_INT16,      // Can accomodate R32 payload 
    EX64_IMM_FORMAT_INT32,      // Can accomodate R32 payload 
    EX64_IMM_FORMAT_INT64,      // Can accomodate R32 payload 
    EX64_NO_IMM_FORMAT,
};

local_func bool winx64_can_be_32b_immediate(IREntry& entry, EExpectedX64ImmFormat eImmFormat, WinX64BackendCtx* pCtx, X64OperandRef* outResult)
{
    if (0u == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN))
        return false;
    if (0u == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN_EMBD))
        return false;
    if (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)
        return false;
    switch (eImmFormat) {

        case EX64_IMM_FORMAT_FLOAT32: {
            // TODO !!!
            return false;
        } break;

        case EX64_IMM_FORMAT_INT8: {
            // TODO !!!
            return false;
        } break;

        case EX64_IMM_FORMAT_INT16: {
            // TODO !!!
            return false;
        } break;

        case EX64_IMM_FORMAT_INT32: {
            // TODO !!!
            return false;
        } break;

        case EX64_IMM_FORMAT_INT64: {
            // TODO !!!
            return false;
        } break;

        default:
            return false;
    }

}

// predecl:

// From the IR-encoding of an IR param, returns an 'X64OperandRef', as digested for the 'x64' backend to interpret as an operand in various
//   instructions. The result is based on knowledge of declarations from the the full program (through pCtx) but especially from local IR,
//   and the associated 'pLocalAssign', with info about association of most valued instructions with positions on stack, or even registers.
//   The result can also be encoded as an immediate (depending on eImmFormat).
// In case where the result would need to be an indirection (other than from stack pointer or from IP), 'regToUseIfAddressOrIndirect' can
//   be used to store base address, and 'optRegToUseForIndex' for an indexing register (if non zero).
// That function is also capable of returning the representative of something found 'at the address of its argument', when it is obvious that
//   the argument is an address, when and dereferencing-it could be interpreted directly as the 'something' : to enable that behaviour, pass
//   a non-null pointer as 'optAlreadyDerefResult'. In which case, and only if that behaviour is triggered, that output will be set to 1
//   (left unchanged otherwise).
X64OperandRef winx64_get_x64_operand_from_ir_param(u64 uIRParam, WinX64ProcInfo* pProcInfo, u32* pLocalAssign,
    TmpArray<u32>& vecLocalConstsToEmit, EExpectedX64ImmFormat eImmFormat,
    eRegCategory regToUseIfAddressOrIndirect, eRegCategory optRegToUseForIndex, WinX64BackendCtx* pCtx, u8* optAlreadyDerefResult = 0);



X64OperandRef winx64_get_x64_operand_from_address_plus_immediate(u64 uIROfBaseAddress, u64 iByteOffset, WinX64ProcInfo* pProcInfo,
    u32* pLocalAssign, TmpArray<u32>& vecLocalConstsToEmit, EExpectedX64ImmFormat eImmFormat,
    eRegCategory regToUseIfAddressOrIndirect, eRegCategory optRegToUseForIndex, WinX64BackendCtx* pCtx, u8* optAlreadyDerefResult = 0)
{
    if (iByteOffset == 0) {
        // Offset is 0 => resulting address is whatever is at uBaseAddressIR.
        // We can also recursively pass to it the trouble of deciding beteen 'already deref' or not...
        return winx64_get_x64_operand_from_ir_param(uIROfBaseAddress, pProcInfo, pLocalAssign,
            vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32, regToUseIfAddressOrIndirect,
            optRegToUseForIndex, pCtx, optAlreadyDerefResult);
    }

    X64OperandRef refBase;
    if (can_encode_32b_signed(u64(iByteOffset))) {
        // we have an immediate offset => we can apply that to base address *before* any computation
        i32 iByteOffset32 = i32(iByteOffset); // TODO: CLEANUP: checking bounds there ???
        u8 bAlreadyDeref = 0u;
        refBase = winx64_get_x64_operand_from_ir_param(uIROfBaseAddress, pProcInfo, pLocalAssign,
            vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32, regToUseIfAddressOrIndirect,
            optRegToUseForIndex, pCtx, &bAlreadyDeref);
        if (bAlreadyDeref) {
            X64OperandRef reindexedBase;
            if (refBase.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
                u32 uOffsetBase = base_offset32_from_operand_ip_based_address(refBase);
                u32 uOffsetResult = uOffsetBase + u32(iByteOffset32);
                u64 uAdditionalPayload = additional_payload_from_operand_ip_based_address(refBase);
                reindexedBase = make_operand_offset_from_ip(i32(uOffsetResult), uAdditionalPayload);
                goto on_has_reindexed_base;
            } else if (0 == (refBase.uFlags & X64OPERAND_FLAG_IS_REG) && 0 == (refBase.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE)) {
                u32 uRegBase = base_regcode_from_operand_regular_mem(refBase);
                u32 uRegIndex = index_regcode_from_operand_regular_mem(refBase);
                i32 iOffsetBase = displacement_from_operand_regular_mem(refBase);
                u32 uOffsetResult = u32(iOffsetBase) + u32(iByteOffset32);
                u8 log2Scale = log2scale_from_operand_regular_mem(refBase);
                reindexedBase = make_operand_at_address_in_reg_indexed(uRegBase, uRegIndex, log2Scale, i32(uOffsetResult));
                { on_has_reindexed_base:
                    if (optAlreadyDerefResult) {
                        *optAlreadyDerefResult = 1u;
                        return reindexedBase;
                    } else {
                        X64OperandRef refResult = make_operand_reg(regToUseIfAddressOrIndirect);
                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, refResult, reindexedBase, 0x03u, pCtx);
                        return refResult;
                    }
                }
            } else {
                Assert_(false); // how would an immediate or reg be already deref ?
            }

        } else { // refBase contains the address of base... we need to add our index to that

            // but if it's a reg and we want a deref...
            if (optAlreadyDerefResult && (refBase.uFlags & X64OPERAND_FLAG_IS_REG)) {
                // we're then lucky and can encode that in one go into a single resulting "indirect" operand
                *optAlreadyDerefResult = 1u;
                u32 uRegBase = reg_code_from_operand_reg(refBase);
                return make_operand_at_address_in_reg_indexed(uRegBase, 0u, 0u, iByteOffset32);
            }

            goto on_add_immediate_to_base;

        }

    } else {  // immediate not encodable as i32 anyway... we'll do a manual "add" on the address...
                                    
        refBase = winx64_get_x64_operand_from_ir_param(uIROfBaseAddress, pProcInfo, pLocalAssign,
            vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32, regToUseIfAddressOrIndirect, optRegToUseForIndex, pCtx);

        { on_add_immediate_to_base:

            X64OperandRef refResult = make_operand_reg(regToUseIfAddressOrIndirect);
            if ((refBase.uFlags & X64OPERAND_FLAG_IS_REG) && reg_code_from_operand_reg(refBase) == regToUseIfAddressOrIndirect) {
                // Nominal case no move required
            } else {
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refBase, 0x03u, pCtx);
            }

            if (iByteOffset > 0) {
                X64OperandRef toAdd = make_operand_immediate(iByteOffset);
                // The "handle_emit" variant should take care of using an intermediate reg is fitting in max 32b for adding immediates
                winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_ADD, refResult, toAdd, 0x03u, pCtx);
            } else {
                X64OperandRef toRemove = make_operand_immediate(-iByteOffset);
                // The "handle_emit" variant should take care of using an intermediate reg is fitting in max 32b for adding immediates
                winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_SUB, refResult, toRemove, 0x03u, pCtx);
            }

            return refResult;
        }
    }

}

// Moves from an r/m op to a larger register, setting additional bits to zero (if eSemantics is unsigned), otherwise to sign of source (if eSemantics is signed)
local_func void winx64_emit_mov_extend(eRegCategory destReg, u8 uDestFormat, const X64OperandRef& source, u8 uSrcFormat,
    EIntSemantics eSemantics, WinX64BackendCtx* pCtx)
{
    Assert_(uDestFormat <= 0x03u);
    Assert_(uSrcFormat < uDestFormat);
    Assert_(eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED || eSemantics == EIntSemantics::EINT_SEMANTIC_UNSIGNED);
    u8 tData[32];
    u8 uPrefixBytes = 0u;
    u8 uBytes = 0u;
    u8 uPosOfPatchableOffset = 0u;
    u32 uPosBefore = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
    if (uSrcFormat < 0x02u) { // 8b or 16b source:

        // 'movsx' examples:
        // case of 8 to 16      66 (REX) 0F BE
        // case of 8 to 32      (REX) 0F BE
        // case of 8 to 64      REX 0F BE
        // case of 16 to 32     (REX) 0F BF
        // case of 16 to 64     REX 0F BF

        u8 uCode = (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) ? 0xBEu : 0xB6u; // 0F BE is opcode for movsx / 0F B6 is opcode for movzx ; for 8b r/m (to 16b+ dest reg)
        uCode += uSrcFormat; // 0F BF is opcode for movsx / 0F B7 is opcode for movzx for: for 16b r/m (to 32b+ dest reg)
        u8 uBytesWithout0F = x64_encode_dest_src_op_as_standard_modrm(uCode, source, destReg, uDestFormat, X64FLAG_CODE_NO_8b,
            tData + 1u, uPosBefore + 1u, &uPosOfPatchableOffset);
        if (tData[1] == uCode) {
            tData[0] = 0x0F;
        } else if (tData[2] == uCode) {
            tData[0] = tData[1];
            tData[1] = 0x0F;
        } else { Assert_(tData[3] == uCode);
            tData[0] = tData[1];
            tData[1] = tData[2];
            tData[2] = 0x0F;
        }
        uBytes = uBytesWithout0F + 1u;
        if (uPosOfPatchableOffset)
            uPosOfPatchableOffset += 1u;

        /*
        tData[uPrefixBytes++] = 0x0Fu; // prefix
        u8 uCode = (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) ? 0xBEu : 0xB6u; // 0F BE is opcode for movsx / 0F B6 is opcode for movzx ; for 8b r/m (to 16b+ dest reg)
        uCode += uSrcFormat; // 0F BF is opcode for movsx / 0F B7 is opcode for movzx for: for 16b r/m (to 32b+ dest reg)
        uBytesAfterPrefix = x64_encode_dest_src_op_as_standard_modrm(uCode, source, destReg, uDestFormat, X64FLAG_CODE_NO_8b,
            tData + 1u, uPosBefore + 1u, &uPosOfPatchableOffsetAfterPrefix);
        */
    } else { // 32b source, 64b dest:
        Assert_(uSrcFormat == 0x02u);
        Assert_(uDestFormat == 0x03u);
        if (eSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) {
            // REX.W 63h is opcode for movsx 32b to 64b. Note that the 'REX' byte emission is handled by the following:
            uBytes = x64_encode_dest_src_op_as_standard_modrm(0x63u, source, destReg, 0x03u, X64FLAG_CODE_NO_8b,
                tData, uPosBefore, &uPosOfPatchableOffset);
        } else {
            // in the unsigned case, mov 32b r/m to 64b register will, by default, zero-out bits 32..63 in destination
            //   => we simply use mov with a **32b** format.
            // 8Bh is direct code for mov reg <- r/m for 16b+
            uBytes = x64_encode_dest_src_op_as_standard_modrm(0x8Bu, source, destReg, 0x02u, X64FLAG_CODE_NO_8b,
                tData, uPosBefore, &uPosOfPatchableOffset);
        }
    }

    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uBytes);

    if (source.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(uPosOfPatchableOffset && uPosOfPatchableOffset + 4u <= uBytes);
        append_offset_from_ip_to_patch(source, uPosBefore + u32(uPosOfPatchableOffset), pCtx);
    }
}

local_func X64OperandRef winx64_get_x64_operand_from_ptr_offset_entry(const IREntry& entry, u32 uIndex, WinX64ProcInfo* pProcInfo,
    u32* pLocalAssign, TmpArray<u32>& vecLocalConstsToEmit, EExpectedX64ImmFormat eImmFormat,
    eRegCategory regToUseIfAddressOrIndirect, eRegCategory optRegToUseForIndex, WinX64BackendCtx* pCtx, u8* optAlreadyDerefResult = 0)
{
    u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
    Assert_(uIRIT == IRIT_PTR_OFFSET || uIRIT == IRIT_PTR_OFFSET_EXT);

    Assert_(regToUseIfAddressOrIndirect != 0);
//    Assert_(regToUseIfAddressOrIndirect != REG_X64_xAX);
//    Assert_(regToUseIfAddressOrIndirect != REG_X64_xCX);
//    Assert_(regToUseIfAddressOrIndirect != REG_X64_xDX);
//    Assert_(regToUseIfAddressOrIndirect != REG_X64_xBX);
    Assert_(regToUseIfAddressOrIndirect != optRegToUseForIndex);

    u64 uBaseAddressIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
    Assert_(ir_is_valid_param(uBaseAddressIR));

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
        "Computing IRIT_PTR_OFFSET (or _EXT)"), pCtx->pWorker);

    u32 uIndexScale;
    u8 uIndexFormat;
    u64 uIndexIR;
    if (uIRIT == IRIT_PTR_OFFSET_EXT) {
        // TODO
        uIndexScale = 0;
        uIndexFormat = 0;
        uIndexIR = 0;
        Assert(false, "winx64_get_x64_operand_from_ptr_offset_entry() : deref ptr offset not yet implemented in the 'ext' case");
    } else {
        uIndexScale = ((entry.uInstrCodeAndFormatAndFirstParam >> 16u) & 0x0000'00FFu) + 1u;
        uIndexFormat = 0x02u;
        uIndexIR = entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_MASK;
    }
    Assert_(uIndexScale);
    Assert_(uIndexFormat >= 0x02u && uIndexFormat <= 0x03u); // TODO: CLEANUP: accept larger than u64 ??
    Assert_(ir_is_valid_param(uIndexIR));
    
    EIntSemantics eIndexSemantics = (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED) ?
        EIntSemantics::EINT_SEMANTIC_UNSIGNED : EIntSemantics::EINT_SEMANTIC_SIGNED;
    // TODO: rework all those immediates and embedding complexities.
    // This current logic may fail getting the sign right.
    EExpectedX64ImmFormat immFormatIndex = EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32;
    if (uIndexFormat > 0x02u || (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED))
        immFormatIndex = EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT64;

    X64OperandRef refIndex = winx64_get_x64_operand_from_ir_param(uIndexIR, pProcInfo, pLocalAssign,
        vecLocalConstsToEmit, immFormatIndex, REG_X64_xCX, eRegCategory(0u), pCtx);
                        
    if ((refIndex.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE)) {

        i64 iAsImmediate = i64(immu64_from_operand_imm(refIndex));
        i64 iByteOffsetAsImmediate = iAsImmediate * i64(uIndexScale);

        return winx64_get_x64_operand_from_address_plus_immediate(uBaseAddressIR, iByteOffsetAsImmediate, 
            pProcInfo, pLocalAssign, vecLocalConstsToEmit, eImmFormat, regToUseIfAddressOrIndirect, optRegToUseForIndex,
            pCtx, optAlreadyDerefResult);

    } else { // our index is *not* an immediate...

        X64OperandRef refBase;

        if (optAlreadyDerefResult) {
            u8 uScaleLog2 = 0u;
            switch (uIndexScale) {
                case 1u: uScaleLog2 = 0u; break;
                case 2u: uScaleLog2 = 1u; break;
                case 4u: uScaleLog2 = 2u; break;
                case 8u: uScaleLog2 = 3u; break;
                default:
                    goto on_return_dumb_ptr_arithmetics;
            }

            u8 bAlreadyDeref = 0u;
            refBase = winx64_get_x64_operand_from_ir_param(uBaseAddressIR, pProcInfo, pLocalAssign,
                vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32, regToUseIfAddressOrIndirect,
                eRegCategory(0u), pCtx, &bAlreadyDeref);

            if (bAlreadyDeref) {
                // TODO!! try to return base address and index, as indirect

            // TMP TMP TMP TMP
            }
            goto on_return_dumb_ptr_arithmetics;
            // TMP TMP TMP TMP

        } else { on_return_dumb_ptr_arithmetics:

            // TMP TMP TMP TMP
            refBase = winx64_get_x64_operand_from_ir_param(uBaseAddressIR, pProcInfo, pLocalAssign,
                vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32, regToUseIfAddressOrIndirect,
                eRegCategory(0u), pCtx);
            // TMP TMP TMP TMP

            X64OperandRef refResult = make_operand_reg(regToUseIfAddressOrIndirect);
            if ((refBase.uFlags & X64OPERAND_FLAG_IS_REG) && reg_code_from_operand_reg(refBase) == regToUseIfAddressOrIndirect) {
                // Nominal case, no move required
            } else {
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refBase, 0x03u, pCtx);
            }

            X64OperandRef RAX = make_operand_reg(REG_X64_xAX);
            if (uIndexFormat < 0x03u) {
                winx64_emit_mov_extend(REG_X64_xAX, 0x03u, refIndex, uIndexFormat, eIndexSemantics, pCtx);
            } else {
                Assert_(uIndexFormat == 0x03u);
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, RAX, refIndex, uIndexFormat, pCtx);
            }
            if (uIndexScale != 1u) { // ...no mul required otherwise
                Assert_(uIndexScale > 0 && uIndexScale < 0x0100'0000u);
                // Moves scale to EBX (effectively RBX)
                X64OperandRef RBX = make_operand_reg(REG_X64_xBX);
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, RBX, make_operand_immediate(i64(uIndexScale)), 0x02u, pCtx);
                EX64SingleRMOp eMulOp = (eIndexSemantics == EIntSemantics::EINT_SEMANTIC_SIGNED) ? EX64_SINGLERM_OP_IMUL : EX64_SINGLERM_OP_MUL;
                // Muls RBX with RAX (64b modulo result in RAX)
                winx64_emit_singleRM_op(eMulOp, RBX, 0x03u, pCtx);
            }
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_ADD, refResult, RAX, 0x03u, pCtx);

            return refResult;
        }
    }
}

local_func_inl EExpectedX64ImmFormat get_imm_integrals_from_format(u8 uFormat) {
    if (uFormat <= 0x03u)
        return EExpectedX64ImmFormat(EX64_IMM_FORMAT_INT8 + uFormat);
    else
        return EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT;
}

local_func X64OperandRef winx64_get_x64_operand_from_ir_param(u64 uIRParam, WinX64ProcInfo* pProcInfo, u32* pLocalAssign,
    TmpArray<u32>& vecLocalConstsToEmit, EExpectedX64ImmFormat eImmFormat,
    eRegCategory regToUseIfAddressOrIndirect, eRegCategory optRegToUseForIndex, WinX64BackendCtx* pCtx, u8* optAlreadyDerefResult)
{
    if (ir_is_numeric_immediate(uIRParam)) {
        u32 uPayload = u32(uIRParam >> IR_STD_PARAM_SHIFT);
        switch (eImmFormat) {
            case EX64_IMM_FORMAT_FLOAT32: {
                return make_operand_immediate(i64(u64(u32(uPayload))));
            } break;
            case EX64_IMM_FORMAT_INT8: {
                // TODO: assert that we do not lose anything there ?
                return make_operand_immediate(i64(i8(uPayload)));
            } break;
            case EX64_IMM_FORMAT_INT16: {
                // TODO: assert that we do not lose anything there ?
                return make_operand_immediate(i64(i16(uPayload)));
            } break;
            case EX64_IMM_FORMAT_INT32: {
                // TODO: assert that we do not lose anything there ?
                return make_operand_immediate(i64(i32(uPayload)));
            } break;
            case EX64_IMM_FORMAT_INT64: {
                // TODO: assert that we do not lose anything there ?
                return make_operand_immediate(i64(i32(uPayload)));
            } break;
            default: {
                Assert(false, "winx64_get_x64_operand_from_ir_param() : non-nyka immediate found when no immediate format was specified");
                return X64OperandRef{};
            }
        }

    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(uIRParam)) {

        u64 uAddressedEntityIR = ir_get_param_from_nyka_immediate(uIRParam);
        Assert_(!ir_is_immediate(uAddressedEntityIR));
        Assert_(regToUseIfAddressOrIndirect != 0);
        Assert_(regToUseIfAddressOrIndirect != REG_X64_xBX);
        X64OperandRef addressedEntityRef = winx64_get_x64_operand_from_ir_param(uAddressedEntityIR, pProcInfo, pLocalAssign,
            vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, regToUseIfAddressOrIndirect, eRegCategory(0u), pCtx);
        if (optAlreadyDerefResult) {
            *optAlreadyDerefResult = 1u;
            return addressedEntityRef;
        } else {
            X64OperandRef addressResult = make_operand_reg(regToUseIfAddressOrIndirect);
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, addressResult, addressedEntityRef, 0x03u, pCtx);
            return addressResult;
        }

    } else { Assert_(!ir_is_immediate(uIRParam));
        IRRepo* pRepo;
        u32 uIndex;
        SourceFileDescAndState* pSourceFile;
        EEntryKind eKind;
        ir_decode_non_imm(uIRParam, pCtx, &pRepo, &uIndex, &pSourceFile, &eKind);
        if (eKind == EEntryKind::EEK_FILEWISE_VAR) {
            u32 uInfo = winx64_backend_on_access_global_var(pSourceFile, uIndex, uIRParam, pCtx);
            u32 uSectionAndOne = (uInfo >> 29);
            Assert_(uSectionAndOne);
            u32 uOffset = uInfo & 0x1FFF'FFFFu;
            return make_operand_offset_from_ip(i32(uOffset), u8(uSectionAndOne - 1u));

        } else if (eKind == EEntryKind::EEK_FILEWISE_CONST) {
            // TODO: winx64_backend_on_access_filewise_const_or_immediate : possibly returning known value as immediate if fits in EExpectedX64ImmFormat
            // TODO: special on IRIT_REINTERP or IRIT_DEREF as consts ?
            u32 uInfo = winx64_backend_on_access_filewise_const(pSourceFile, uIndex, uIRParam, pCtx);
            Assert_(uInfo >> 29 == EPESection::PE_SECTION_CONST + 1u);
            u32 uOffset = uInfo & 0x1FFF'FFFFu;
            return make_operand_offset_from_ip(i32(uOffset), u8(EPESection::PE_SECTION_CONST));

        } else if (eKind == EEntryKind::EEK_PROGRAMWISE_ENTRY) {
            // TODO: winx64_backend_on_access_programwise_const_or_immediate : possibly returning known value as immediate if fits in EExpectedX64ImmFormat
            u32 uInfo = winx64_backend_on_access_programwise_const(uIndex, uIRParam, pCtx);
            Assert_(uInfo >> 29 == EPESection::PE_SECTION_CONST + 1u);
            u32 uOffset = uInfo & 0x1FFF'FFFFu;
            return make_operand_offset_from_ip(i32(uOffset), u8(EPESection::PE_SECTION_CONST));

        } else if (eKind == EEntryKind::EEK_IS_PROCBODY_REF) {

            u32 uRepoInfo = winx64_backend_on_access_proc(pSourceFile, uIndex, uIRParam, pCtx);
            X64OperandRef procAddressAsOffsetFromIP;
            if (uRepoInfo > 1u) {
                u32 uKnownOffsetInCode = uRepoInfo & 0x1FFF'FFFFu;
                procAddressAsOffsetFromIP = make_operand_offset_from_ip(i32(uKnownOffsetInCode), u8(EPESection::PE_SECTION_CODE));
            } else {
                procAddressAsOffsetFromIP = make_operand_offset_from_ip(0, uIRParam);
                procAddressAsOffsetFromIP.uFlags |= X64OPERAND_FLAG_IP_BASED_IS_NYEP;
            }
            Assert_(regToUseIfAddressOrIndirect);
            X64OperandRef resultProcAddressInReg = make_operand_reg(regToUseIfAddressOrIndirect);
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, resultProcAddressInReg, procAddressAsOffsetFromIP, 0x03u, pCtx);
            return resultProcAddressInReg;

        } else { Assert_(eKind == EEntryKind::EEK_CURRENT_PROC_LOCAL);
            Assert_(pRepo == pCtx->pRepo);
            u32 uLocalAssign = pLocalAssign[uIndex];
            if (uLocalAssign != WIN64_BACKEND_LOCAL_NONE) {
                u32 uLocalKind = uLocalAssign & WIN64_BACKEND_LOCAL_KIND_MASK;
                if (uLocalKind == WIN64_BACKEND_LOCAL_KIND_REG) {
                    return make_operand_reg(uLocalAssign & WIN64_BACKEND_LOCAL_VALUE_MASK);
                } else if ((uLocalAssign & WIN64_BACKEND_LOCAL_KIND_SUBMASK) == WIN64_BACKEND_LOCAL_KIND_CONST_SUBMASK) {
                    return make_operand_offset_from_ip(i32(uLocalAssign & 0x3FFF'FFFFu), u8(EPESection::PE_SECTION_CONST));
                } else { Assert_(uLocalKind != WIN64_BACKEND_LOCAL_KIND_PARAM); // should not reference a 'param' local at this point...
                    if (uIndex < pProcInfo->uInParamsCount + pProcInfo->uOutParamsCount) {
                        if (uIndex < pProcInfo->uInParamsCount) {
                            if (pProcInfo->tThisFuncInParams[uIndex].uParamKind == EPARAM_KIND_REF) { whenParamByRef:
                                // moving "contents" of param slot (being address of param) to 'regToUseIfAddressOrIndirect',
                                //   then returning an X64OperandRef being the value *at this address*
                                X64OperandRef refIndirect = make_operand_at_address_in_reg(REG_X64_xSP, i32(uLocalAssign));
                                Assert_(regToUseIfAddressOrIndirect);
                                X64OperandRef refAsAddress = make_operand_reg(regToUseIfAddressOrIndirect);
                                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refAsAddress, refIndirect, 0x03u, pCtx);
                                return make_operand_at_address_in_reg(regToUseIfAddressOrIndirect);
                            }
                        } else {
                            if (pProcInfo->tThisFuncOutParams[uIndex - pProcInfo->uInParamsCount].uParamKind == EPARAM_KIND_REF)
                                goto whenParamByRef;
                        }
                    }
                    return make_operand_at_address_in_reg(REG_X64_xSP, i32(uLocalAssign));
                }
            } else { // we're trying to evalue a value which was not assigned to anything yet.
                IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
                u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
                Assert_(has_irit_a_value(uIRIT));

                // possible non-local assigned are 'reinterp'.
                // Either const or not, we currently get directly to the source
                if (uIRIT == IRIT_REINTERP) {
                    u64 uIRofSource = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
                    u8 uDestFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16);
                    u8 uSrcFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 8) & 0x7Fu;
                    u32 uSlots = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    Assert_(ir_is_valid_param(uIRofSource));
                    X64OperandRef result = winx64_get_x64_operand_from_ir_param(uIRofSource, pProcInfo, pLocalAssign,
                        vecLocalConstsToEmit, get_imm_integrals_from_format(uSrcFormat), regToUseIfAddressOrIndirect, optRegToUseForIndex,
                        pCtx, optAlreadyDerefResult);
                    if (result.uFlags & X64OPERAND_FLAG_IS_REG) {
                        // if dest format and src formats are single integrals: we can directly use source even if register.
                        Assert_(pCtx->pProcResult);
                        // TODO: otherwise, simply make sure that the entry at this position has a slot... and thus compute that while scanning IR
                        Assert_(uDestFormat <= 0x03u && uSrcFormat <= 0x03u && uSlots == 1u);
                    }
                    if (result.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
                        // we found an immediate as a value for source... this is not cool, since reinterp may modify it.
                        Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam) && 0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
                        // we can however fall back on our feet since it means that the source is constant, thus *we* should also be ale to emit it then reinterp
                        // => fallthrough on_constant
                    } else
                        return result;
                }

                // it may be a known const...
                if (irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam) && 0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)) {
                    if (irflag_is_single_nyka(entry.uInstrMetaFlagsAndSecondParam)) {
                        i32 iOffset;
                        u64 uBaseIR = ir_decode_nyka_value(entry.metaValue.knownValue.uEmbeddedValue, &iOffset);
                        Assert_(ir_is_valid_param(uBaseIR));
                        u64 uNykaImmOfBaseIR = ir_make_nyka_immediate(uBaseIR);
                        return winx64_get_x64_operand_from_address_plus_immediate(uNykaImmOfBaseIR, i64(iOffset), pProcInfo,
                            pLocalAssign, vecLocalConstsToEmit, eImmFormat, regToUseIfAddressOrIndirect, optRegToUseForIndex,
                            pCtx, optAlreadyDerefResult);
                    }
                    X64OperandRef resultIfImmediate;
                    if (winx64_can_be_32b_immediate(entry, eImmFormat, pCtx, &resultIfImmediate)) {
                        return resultIfImmediate;
                    } else {
                        Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)); // local nykas cannot be emitted as const...
                        u32 uOffset = winx64_backend_on_access_local_const(entry, uIndex, pLocalAssign, vecLocalConstsToEmit, pCtx);
                        return make_operand_offset_from_ip(i32(uOffset), u8(EPESection::PE_SECTION_CONST));
                    }

                // or it may specially be a REINTERP, DEREF, LOCAL_ADDRESS, or PTR_OFFSET entry
                } else {

                    if (uIRIT == IRIT_DEREF) {              // possible non-local assigned are 'derefs'

                        Assert_(regToUseIfAddressOrIndirect != 0);
                        u64 uIRofAddress = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
                        Assert_(ir_is_valid_param(uIRofAddress));
                        u8 bAlreadyDeref = 0u;
                        X64OperandRef refAddressToDeref = winx64_get_x64_operand_from_ir_param(uIRofAddress, pProcInfo, pLocalAssign,
                            vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, regToUseIfAddressOrIndirect, optRegToUseForIndex,
                            pCtx, &bAlreadyDeref);
                        if (bAlreadyDeref) { // since we passed a non-null ptr to 'bAlreadyDeref' the above call,
                            // it may have returned with an already deref'd address...
                            return refAddressToDeref; // in which case, we're done...
                        } else {
                            // we'll have otherwise computed the "address" to deref into the 'refAddressToDeref' operand.
                            // ...it may itself be represented by the register which we specifically passed for this case.
                            if ((refAddressToDeref.uFlags & X64OPERAND_FLAG_IS_REG) && reg_code_from_operand_reg(refAddressToDeref) == regToUseIfAddressOrIndirect) {
                                // Nominal case, NOOP: address already in 'regToUseIfAddressOrIndirect'
                            } else { // or it may be something else, in which case we 'mov' it to that register by hand now.
                                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_reg(regToUseIfAddressOrIndirect), refAddressToDeref, 0x03u, pCtx);
                            }
                            // ... now, the resulting value of the 'deref' IR instruction is to specify, as ASM, for example:
                            // [R8]     #if R8 was the specified register for addresses
                            return make_operand_at_address_in_reg(regToUseIfAddressOrIndirect);
                        }

                    } else if (uIRIT == IRIT_LOCAL_ADDRESS) {       // possible non-local assigned are local_address

                        Assert_(regToUseIfAddressOrIndirect != 0);
                        u64 uAddressedEntityIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
                        Assert_(!ir_is_immediate(uAddressedEntityIR));
                        X64OperandRef addressedEntityRef = winx64_get_x64_operand_from_ir_param(uAddressedEntityIR, pProcInfo, pLocalAssign,
                            vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, regToUseIfAddressOrIndirect, eRegCategory(0u), pCtx);
                        if (optAlreadyDerefResult) {
                            *optAlreadyDerefResult = 1u;
                            return addressedEntityRef;
                        } else {
                            X64OperandRef addressResult = make_operand_reg(regToUseIfAddressOrIndirect);
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, addressResult, addressedEntityRef, 0x03u, pCtx);
                            return addressResult;
                        }

                    } else {    // only possible non-local assigned otherwise are 'ptr_offset'
                        Assert_(uIRIT == IRIT_PTR_OFFSET || uIRIT == IRIT_PTR_OFFSET_EXT);
                        return winx64_get_x64_operand_from_ptr_offset_entry(entry, uIndex, pProcInfo, pLocalAssign, vecLocalConstsToEmit,
                            eImmFormat, regToUseIfAddressOrIndirect, optRegToUseForIndex, pCtx, optAlreadyDerefResult);
                    }
                }
            }
        }
    }
}

local_func X64OperandRef winx64_get_write_operand_from_local_assign(u32 uIndex, u32* pLocalAssign)
{
    u32 uLocalAssign = pLocalAssign[uIndex];
    Assert_(uLocalAssign != WIN64_BACKEND_LOCAL_NONE);
    u32 uLocalKind = uLocalAssign & WIN64_BACKEND_LOCAL_KIND_MASK;
    if (uLocalKind == WIN64_BACKEND_LOCAL_KIND_REG) {
        return make_operand_reg(uLocalAssign & WIN64_BACKEND_LOCAL_VALUE_MASK);
    } else { Assert_(uLocalKind == 0u); // cannot be a const, and cannot be a param at this point either
        return make_operand_at_address_in_reg(REG_X64_xSP, i32(uLocalAssign));
    }
}

/*
local_func X64OperandRef winx64_get_write_operand_from_ir_param(u64 uIRParam, WinX64ProcInfo* pProcInfo, u32* pLocalAssign, WinX64BackendCtx* pCtx)
{
    Assert_(!ir_is_immediate(uIRParam));
    IRRepo* pRepo;
    u32 uIndex;
    SourceFileDescAndState* pSourceFile;
    EEntryKind eKind;
    ir_decode_non_imm(uIRParam, pCtx, &pRepo, &uIndex, &pSourceFile, &eKind);

    if (eKind == EEntryKind::EEK_FILEWISE_VAR) {
        u32 uInfo = winx64_backend_on_access_global_var(pSourceFile, uIndex, uIRParam, pCtx);
        u32 uSectionAndOne = (uInfo >> 29);
        Assert_(uSectionAndOne);
        u32 uOffset = uInfo & 0x1FFF'FFFFu;
        return make_operand_offset_from_ip(i32(uOffset), u8(uSectionAndOne - 1u));

    } else { Assert_(eKind == EEntryKind::EEK_CURRENT_PROC_LOCAL);
        Assert_(pRepo == pCtx->pRepo);
        return winx64_get_write_operand_from_local_assign(uIndex, pLocalAssign);
    }
}
*/

local_func void winx64_add_ref_to_vec_of_addresses_to_patch(EPESection eReferrencing, u32 uOffsetInReferencingSection,
    EPESection eReferrenced, WinX64BackendCtx* pCtx)
{
    switch (eReferrencing) {
        case EPESection::PE_SECTION_CONST: {
            switch (eReferrenced) {
                case EPESection::PE_SECTION_CONST: {
                    pCtx->vecOffsetsInConstReferringToAddressOfConst.append(uOffsetInReferencingSection);
                } break;
                case EPESection::PE_SECTION_GLOBVAR_INI: {
                    pCtx->vecOffsetsInConstReferringToAddressOfGlobIni.append(uOffsetInReferencingSection);
                } break;
                case EPESection::PE_SECTION_GLOBVAR_ZERO: {
                    pCtx->vecOffsetsInConstReferringToAddressOfGlobZero.append(uOffsetInReferencingSection);
                } break;
                case EPESection::PE_SECTION_CODE: {
                    pCtx->vecOffsetsInConstReferringToAddressOfProc.append(uOffsetInReferencingSection);
                } break;
                default:
                    Assert_(false);
            }
        } break;

        case EPESection::PE_SECTION_GLOBVAR_INI: {
            switch (eReferrenced) {
                case EPESection::PE_SECTION_CONST: {
                    pCtx->vecOffsetsInGlobIniReferringToAddressOfConst.append(uOffsetInReferencingSection);
                } break;
                case EPESection::PE_SECTION_GLOBVAR_INI: {
                    pCtx->vecOffsetsInGlobIniReferringToAddressOfGlobIni.append(uOffsetInReferencingSection);
                } break;
                case EPESection::PE_SECTION_GLOBVAR_ZERO: {
                    pCtx->vecOffsetsInGlobIniReferringToAddressOfGlobZero.append(uOffsetInReferencingSection);
                } break;
                case EPESection::PE_SECTION_CODE: {
                    pCtx->vecOffsetsInGlobIniReferringToAddressOfProc.append(uOffsetInReferencingSection);
                } break;
                default:
                    Assert_(false);
            }
        } break;

        default:
            Assert_(false);
    }
}

local_func bool try_access_known_proc(u64 uIRofProcParam, SourceFileDescAndState** outProcFile, u32* outProcIndex, u64* outProcAsIR, IRAwareContext* pCtx) {
    Assert_(ir_is_valid_param(uIRofProcParam));
    u64 uIRofProc = 0u;
    if (ir_is_numeric_immediate(uIRofProcParam)) {
        return false; // ref of a known proc cannot be a numeric immediate...
    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(uIRofProcParam)) {
        uIRofProc = ir_get_param_from_nyka_immediate(uIRofProcParam);
        { on_found_possible_proc_ir:
            Assert_(ir_is_valid_param(uIRofProc));
            Assert_(!ir_is_immediate(uIRofProc));
            IRRepo* pCalledProcRepo;
            EEntryKind eCalledProcKind;
            ir_decode_non_imm(uIRofProc, pCtx, &pCalledProcRepo, outProcIndex, outProcFile, &eCalledProcKind);
            if (eCalledProcKind == EEntryKind::EEK_IS_PROCBODY_REF) {
                *outProcAsIR = uIRofProc;
                return true;
            }
        }
    } else { Assert_(!ir_is_immediate(uIRofProcParam));
        IRRepo* pCalledProcValueRepo;
        u32 uCalledProcValueIndex;
        SourceFileDescAndState* pCalledProcValueFile;
        EEntryKind eCalledProcValueKind;
        ir_decode_non_imm(uIRofProcParam, pCtx, &pCalledProcValueRepo, &uCalledProcValueIndex, &pCalledProcValueFile, &eCalledProcValueKind);
        if (eCalledProcValueKind == EEntryKind::EEK_IS_PROCBODY_REF) {
            *outProcAsIR = uIRofProcParam;
            *outProcFile = pCalledProcValueFile;
            *outProcIndex = uCalledProcValueIndex;
            return true;
        } else if (eCalledProcValueKind == EEK_FILEWISE_VAR) { // global var have a 'known' value corresponding to their *initial*
            return false;                                      // => we need to force to in fact 'unknown value' if this is one...
        } else {
            IREntry& asEntry = ir_access_repo_instr(pCalledProcValueRepo, uCalledProcValueIndex);
            if (asEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) {
                if (asEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_NYKA) {
                    if (asEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN_EMBD) {
                        u64 uNykaValue = asEntry.metaValue.knownValue.uEmbeddedValue;
                        i32 iOffset;
                        uIRofProc = ir_decode_nyka_value(uNykaValue, &iOffset);
                        if (iOffset == 0)
                            goto on_found_possible_proc_ir;
                    }
                }
            }
        }
    }
    return false;
}

#endif // LOCLIB_WINX64_BACKEND_BASE_H_
