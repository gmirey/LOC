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

#ifndef LOCLIB_WINX64_BACKEND_TOOLS_H_
#define LOCLIB_WINX64_BACKEND_TOOLS_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/compiler_dependent_msvc.h"
#include "../../HighPerfTools/arithmetic_operations.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_ProgramState.h"
#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_NodeValue.h"

#include "LocLib_PE.h"
#include "LocLib_x64.h"
#include "LocLib_winx64_backend_base.h"

local_func void emit_store_to_reg(u32 uRegCategory, u64 uSource, u8 uFormat, FileSection* pSection, WinX64BackendCtx* pCtx)
{
    u8 tData[32];
    X64OperandRef dest = make_operand_reg(uRegCategory); // first scalar return in RAX. TODO: save that when we use RAX for other things...
    u32 uOffsetToStartOfInstructionInCodeSection = pSection->uSize;
    if (ir_is_numeric_immediate(uSource)) {
        // TODO
        Assert(false, "emit_store_to_reg not yet implemented when source is numeric immediate");
    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(uSource)) {
        // TODO
        Assert(false, "emit_store_to_reg not yet implemented when source is nyka immediate");
    } /* else if (ir_is_register(uSource)) { // TODO ??
        // TODO
        Assert(false, "emit_store_to_reg not yet implemented when source is a register");
    } */ else {
        IRRepo* pSrcRepo;
        u32 uSrcIndex;
        SourceFileDescAndState* pSrcSourceFile;
        EEntryKind eSrcKind;
        ir_decode_non_imm(uSource, pCtx, &pSrcRepo, &uSrcIndex, &pSrcSourceFile, &eSrcKind);
        if (eSrcKind == EEntryKind::EEK_FILEWISE_VAR) {
            Assert_(pSrcSourceFile);
            u32 uSourceLocationInPE = winx64_backend_on_access_global_var(pSrcSourceFile, uSrcIndex, uSource, pCtx);
            u32 uSection = (uSourceLocationInPE >> 29) - 1u;
            u32 uDisplacement = uSourceLocationInPE & 0x1FFF'FFFFu;
            X64OperandRef src = make_operand_offset_from_ip(i32(uDisplacement), u8(uSection));
            u8 unused;
            u8 uBytesFromStartOfInstructionTo32bOffset;
            u8 uInstructionBytes = x64_encode_dest_src_op(EX64_DEST_SRC_OP_MOV, dest, src, uFormat,
                tData, uOffsetToStartOfInstructionInCodeSection, &unused, &uBytesFromStartOfInstructionTo32bOffset);
            Assert_(uBytesFromStartOfInstructionTo32bOffset);
            if (uSection == EPESection::PE_SECTION_GLOBVAR_INI) {
                pCtx->vecOffsetsInCodeToPatchWithGlobIniStart.append(uOffsetToStartOfInstructionInCodeSection + uBytesFromStartOfInstructionTo32bOffset);
            } else { Assert_(EPESection::PE_SECTION_GLOBVAR_ZERO); 
                pCtx->vecOffsetsInCodeToPatchWithGlobZeroStart.append(uOffsetToStartOfInstructionInCodeSection + uBytesFromStartOfInstructionTo32bOffset);
            }
            Assert_(uInstructionBytes);
            write_to_file_section(pSection, tData, uInstructionBytes);
        } else {
            // TODO
            Assert(false, "emit_store_to_reg not yet implemented when source is not a fileise global var entry");
        }
    }
}

local_func_inl void emit_ret(FileSection* pSection)
{
    u8 uRetOpCode = 0xC3u;
    write_to_file_section(pSection, &uRetOpCode, 1u);
}

local_func void winx64_on_write_nyka_value(EPESection eSection, u32 uOffsetInSection, u64 uNykaValue,
    u32* pLocalAssignIfProcBody, TmpArray<u32>* pVecLocalConstsToEmitIfProcBody, WinX64BackendCtx* pCtx)
{
    FileSection* pSection = pCtx->pSections + eSection;
    i32 iOffsetFromBase; u64 uBaseIR = ir_decode_nyka_value(uNykaValue, &iOffsetFromBase);
    Assert_(!ir_is_immediate(uBaseIR));
    Assert_(!ir_is_immediate(uBaseIR));
    IRRepo* pBaseRepo;
    u32 uBaseIndex;
    SourceFileDescAndState* pBaseFile;
    EEntryKind eBaseKind;
    ir_decode_non_imm(uBaseIR, pCtx, &pBaseRepo, &uBaseIndex, &pBaseFile, &eBaseKind);
    switch (eBaseKind) {
        case EEntryKind::EEK_PROGRAMWISE_ENTRY: {
            u32 uRepoInfo = winx64_backend_on_access_programwise_const(uBaseIndex, uBaseIR, pCtx);
            EPESection eReferrenced = EPESection((uRepoInfo >> 29) - 1u);
            u32 uBaseOffsetInReferrenced = uRepoInfo & 0x1FFF'FFFFu;
            winx64_add_ref_to_vec_of_addresses_to_patch(eSection, uOffsetInSection, eReferrenced, pCtx);
            u64 uTmpAddressInReferrenced = u64(i64(i32(uBaseOffsetInReferrenced + u32(iOffsetFromBase))));
            rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)(&uTmpAddressInReferrenced), 8u);
        } break;
        case EEntryKind::EEK_FILEWISE_CONST: {
            u32 uRepoInfo = winx64_backend_on_access_filewise_const(pBaseFile, uBaseIndex, uBaseIR, pCtx);
            EPESection eReferrenced = EPESection((uRepoInfo >> 29) - 1u);
            u32 uBaseOffsetInReferrenced = uRepoInfo & 0x1FFF'FFFFu;
            winx64_add_ref_to_vec_of_addresses_to_patch(eSection, uOffsetInSection, eReferrenced, pCtx);
            u64 uTmpAddressInReferrenced = u64(i64(i32(uBaseOffsetInReferrenced + u32(iOffsetFromBase))));
            rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)(&uTmpAddressInReferrenced), 8u);
        } break;
        case EEntryKind::EEK_FILEWISE_VAR: {
            u32 uRepoInfo = winx64_backend_on_access_global_var(pBaseFile, uBaseIndex, uBaseIR, pCtx);
            EPESection eReferrenced = EPESection((uRepoInfo >> 29) - 1u);
            u32 uBaseOffsetInReferrenced = uRepoInfo & 0x1FFF'FFFFu;
            winx64_add_ref_to_vec_of_addresses_to_patch(eSection, uOffsetInSection, eReferrenced, pCtx);
            u64 uTmpAddressInReferrenced = u64(i64(i32(uBaseOffsetInReferrenced + u32(iOffsetFromBase))));
            rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)(&uTmpAddressInReferrenced), 8u);
        } break;
        case EEntryKind::EEK_CURRENT_PROC_LOCAL: {
            IREntry& entry = ir_access_repo_instr(pBaseRepo, uBaseIndex);
            Assert_(entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN);
            Assert_(0u == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
            u32 uBaseOffsetInReferrenced = winx64_backend_on_access_local_const(entry, uBaseIndex, pLocalAssignIfProcBody,
                *pVecLocalConstsToEmitIfProcBody, pCtx);
            winx64_add_ref_to_vec_of_addresses_to_patch(eSection, uOffsetInSection, EPESection::PE_SECTION_CONST, pCtx);
            u64 uTmpAddressInReferrenced = u64(i64(i32(uBaseOffsetInReferrenced + u32(iOffsetFromBase))));
            rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)(&uTmpAddressInReferrenced), 8u);
        } break;
        case EEntryKind::EEK_IS_PROCBODY_REF: {
            u32 uRepoInfo = winx64_backend_on_access_proc(pBaseFile, uBaseIndex, uBaseIR, pCtx);
            if (iOffsetFromBase) {
                platform_log_info("warning: found NYKA to proc with a non-null offset");
            }
            if (uRepoInfo > 1u) {
                u32 uKnownOffsetInCode = uRepoInfo & 0x1FFF'FFFFu;
                winx64_add_ref_to_vec_of_addresses_to_patch(eSection, uOffsetInSection, EPESection::PE_SECTION_CODE, pCtx);
                u64 uTmpAddressInReferrenced = u64(i64(i32(uKnownOffsetInCode + u32(iOffsetFromBase))));
                rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)(&uTmpAddressInReferrenced), 8u);
            } else {
                u64 uOffset64FromBase = u64(i64(iOffsetFromBase));
                rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)(&uOffset64FromBase), 8u);
                auto itProc = pCtx->mapVecNotYetEmittedByProc.find(uBaseIR);
                if (itProc == pCtx->mapVecNotYetEmittedByProc.end()) {
                    NotYetEmittedProc newNYEPEntry;
                    newNYEPEntry.vecOffsetsInCodeToPatchWithProcOffset.init(pCtx->pWorker->tmpArena);
                    newNYEPEntry.vecOffsetsInConstsToPatchWithProcAddress.init(pCtx->pWorker->tmpArena);
                    newNYEPEntry.vecOffsetsInGlobIniToPatchWithProcAddress.init(pCtx->pWorker->tmpArena);
                    itProc = pCtx->mapVecNotYetEmittedByProc.insert(uBaseIR, newNYEPEntry);
                }
                if (eSection == EPESection::PE_SECTION_CONST) {
                    itProc.value().vecOffsetsInConstsToPatchWithProcAddress.append(uOffsetInSection);
                } else { Assert_(eSection == EPESection::PE_SECTION_GLOBVAR_INI);
                    itProc.value().vecOffsetsInGlobIniToPatchWithProcAddress.append(uOffsetInSection);
                }
            }
        } break;
        default:
            Assert_(false);
    }
}

local_func void winx64_backend_rewrite_as_known_data(u64 uMetaFlags, AKnownValue knownValue, u8 uFormat, u32 uSlotsCount,
                                                     u32 uOffsetInSection, bool bSectionIsGlobalVarIni, u32* pLocalAssignIfProcBody,
                                                     TmpArray<u32>* pVecLocalConstsToEmitIfProcBody, WinX64BackendCtx* pCtx)
{
    EPESection eSection = bSectionIsGlobalVarIni ? EPESection::PE_SECTION_GLOBVAR_INI : EPESection::PE_SECTION_CONST;
    FileSection* pSection = pCtx->pSections + eSection;
    u32 uByteCount = (1u << get_log2_of_slot_size_from_format(uFormat)) * uSlotsCount;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
        "Rewrite as known data : %u bytes at offset %u in %s section", u64(uByteCount), u64(uOffsetInSection),
        reinterpret_cast<u64>(bSectionIsGlobalVarIni ? "glob-ini":"const")), pCtx->pWorker);

    Assert_(0u == (uMetaFlags & IRFLAG_HAS_LOCAL_NYKA)); // local nykas should not be part of such consts...
    if (irflag_is_known_embd(uMetaFlags)) {
        if (irflag_is_or_has_nyka(uMetaFlags)) {
            winx64_on_write_nyka_value(eSection, uOffsetInSection, knownValue.uEmbeddedValue,
                pLocalAssignIfProcBody, pVecLocalConstsToEmitIfProcBody, pCtx);
        } else {
            if (uFormat & 0xF8u) {
                // TODO
                Assert(false, "winx64_backend_rewrite_as_known_data() : decoding of embedded non-scalar integral not yet implemented");
            }
            Assert_(uByteCount <= 8u);
            rewrite_over_file_section(pSection, uOffsetInSection, (const u8*)&(knownValue.uEmbeddedValue), uByteCount);
        }
    } else {
        if (irflag_is_or_has_nyka(uMetaFlags)) {
            const u32* pNykaTable = reinterpret_cast<const u32*>(knownValue.pPtrToRawData);
            u32 uNykaCount = pNykaTable[0];
            Assert_(uNykaCount);
            u32 uBytesOfNykaTable = align_to(8u, (uNykaCount+1u)*4u);
            const u8* pPtrToActualData = *reinterpret_cast<const u8**>(knownValue.pPtrToRawData + uBytesOfNykaTable);
            rewrite_over_file_section(pSection, uOffsetInSection, pPtrToActualData, uByteCount);
            const u32* pNykaTableAfterSize = pNykaTable + 1;
            for (u32 uNykaIndex = 0u; uNykaIndex < uNykaCount; uNykaIndex++) {
                u32 uNykaOffset = pNykaTableAfterSize[uNykaIndex];
                u64 uNykaValue = *reinterpret_cast<const u64*>(pPtrToActualData + uNykaOffset);
                winx64_on_write_nyka_value(eSection, uOffsetInSection+uNykaOffset, uNykaValue,
                    pLocalAssignIfProcBody, pVecLocalConstsToEmitIfProcBody, pCtx);
            }
        } else {
            rewrite_over_file_section(pSection, uOffsetInSection, knownValue.pPtrToRawData, uByteCount);
        }
    }

}


local_func EX64JumpCodeRelative winx64_handle_compare_and_get_relative_jmp_opcode_testing(u64 uIRToBeTested, u8 uFormatFromCaller, u32 uJumpIfNonZero,
    WinX64ProcInfo* pProcInfo, u32* pLocalAssign, TmpArray<u32>& vecLocalConstsToEmit, WinX64BackendCtx* pCtx)
{
    Assert_(!ir_is_immediate(uIRToBeTested)); // TODO ? could it be possible ? thus shortcut when known ?
    Assert_(uFormatFromCaller == 0x00u); // Only std integrals up to 64b supported atm
    IRRepo* pTestedEntryRepo;
    u32 uTestedEntryIndex;
    SourceFileDescAndState* pTestedEntrySourceFile;
    EEntryKind eTestedEntryKind;
    ir_decode_non_imm(uIRToBeTested, pCtx, &pTestedEntryRepo, &uTestedEntryIndex, &pTestedEntrySourceFile, &eTestedEntryKind);
    if (pTestedEntryRepo) {
        IREntry& testedEntry = ir_access_repo_instr(pTestedEntryRepo, uTestedEntryIndex);
        u8 uTestedIRIT = u8(testedEntry.uInstrCodeAndFormatAndFirstParam);
        if ((testedEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) && uTestedIRIT != IRIT_GLOBAL_VAR_DECL) {
            // TODO: could shortcut when known
        }
        if (eTestedEntryKind == EEK_CURRENT_PROC_LOCAL) {
            Assert_(pCtx->pRepo == pTestedEntryRepo);
            if (uTestedIRIT == IRIT_CMP_EQ || uTestedIRIT == IRIT_CMP_ORD) {
                u8 uFormat = u8(testedEntry.uInstrCodeAndFormatAndFirstParam >> 16);
                EExpectedX64ImmFormat eImmFormat = get_imm_integrals_from_format(uFormat);
                if (testedEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES) {
                    u64 uIRParamOpdA = testedEntry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
                    u64 uIRParamOpdB = testedEntry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_MASK;
                    X64OperandRef refOpdA = winx64_get_x64_operand_from_ir_param(uIRParamOpdA, pProcInfo, pLocalAssign,
                        vecLocalConstsToEmit, eImmFormat, REG_X64_R8x, REG_X64_xAX, pCtx);
                    X64OperandRef refOpdB = winx64_get_x64_operand_from_ir_param(uIRParamOpdB, pProcInfo, pLocalAssign,
                        vecLocalConstsToEmit, eImmFormat, REG_X64_R9x, REG_X64_xDX, pCtx);
                    u32 uIsOpposite = u32(testedEntry.uInstrCodeAndFormatAndFirstParam) & IR_INSTRFLAG_CMP_OPPOSITE;
                    bool bOperandsSwapped = false;
                    if (refOpdA.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
                        bOperandsSwapped = true; // we cannot have a first operand being an immediate...
                        winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_CMP, refOpdB, refOpdA, uFormat, pCtx);
                    } else {
                        winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_CMP, refOpdA, refOpdB, uFormat, pCtx);
                    }
                    if (uTestedIRIT == IRIT_CMP_EQ) {
                        // We need to take care about the intrinsic semantics for the CMP_EQ IR (EQ or NEQ when uIsOpposite) *and* the Branch IR (uJumpIfNonZero)
                        // Note that we don't care about swapped operands in case of eq or neq.
                        if (uIsOpposite) { // NEQ => IR semantics for branch on 'non-zero' : 'non-zero' is when A != B
                            return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JNE : EX64JUMPCODE_RELATIVE_JE;
                        } else {           // EQ => IR semantics for branch on 'non-zero' : 'non-zero' is when A == B
                            return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JE : EX64JUMPCODE_RELATIVE_JNE;
                        }

                    } else { Assert_(uTestedIRIT == IRIT_CMP_ORD);
                        // We need to take care about the intrinsic semantics for the CMP_ORD IR (LT or GE when uIsOpposite) *and* the Branch IR (uJumpIfNonZero) *and*
                        // the int semantics for the CMP_ORD (signed comparison vs unsigned) *and* the possible reversal of operands (to avoid immediates as first)
                        u32 uIsUnsigned = u32(testedEntry.uInstrCodeAndFormatAndFirstParam) & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED;
                        if (uIsUnsigned) { // x86 'below' mnemonic is for unsigned A < B (check CF = 1). inverse is 'above or equal'

                            if (uIsOpposite) { // GE => IR semantics for branch on 'non-zero' : 'non-zero' is when A >= B
                                if (bOperandsSwapped)
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JBE : EX64JUMPCODE_RELATIVE_JA;
                                else
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JAE : EX64JUMPCODE_RELATIVE_JB;
                            } else {           // LT => IR semantics for branch on 'non-zero' : 'non-zero' is when A < B
                                if (bOperandsSwapped)
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JA : EX64JUMPCODE_RELATIVE_JBE;
                                else
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JB : EX64JUMPCODE_RELATIVE_JAE;
                            }

                        } else { // x86 'less' mnemonic is for signed A < B (check (SF xOR OF) = 1). inverse is 'greater or equal'

                            if (uIsOpposite) { // GE => IR semantics for branch on 'non-zero' : 'non-zero' is when A >= B
                                if (bOperandsSwapped)
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JLE : EX64JUMPCODE_RELATIVE_JG;
                                else
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JGE : EX64JUMPCODE_RELATIVE_JL;
                            } else {           // LT => IR semantics for branch on 'non-zero' : 'non-zero' is when A < B
                                if (bOperandsSwapped)
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JG : EX64JUMPCODE_RELATIVE_JLE;
                                else
                                    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JL : EX64JUMPCODE_RELATIVE_JGE;
                            }
                        }
                    }
                }
            } // Otherwise fallthrough...
        }
    }
    Assert_(uFormatFromCaller <= 0x03u); // Only integrals 8b..64b possibly directly testable against 0 atm
    X64OperandRef testedAsRef = winx64_get_x64_operand_from_ir_param(uIRToBeTested, pProcInfo, pLocalAssign, vecLocalConstsToEmit,
        get_imm_integrals_from_format(uFormatFromCaller), REG_X64_xAX, REG_X64_xCX, pCtx);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_CMP, testedAsRef, make_operand_immediate(0), uFormatFromCaller, pCtx);
    return uJumpIfNonZero ? EX64JUMPCODE_RELATIVE_JNE : EX64JUMPCODE_RELATIVE_JE;
}

struct LocalJumpInfo {
    u32 uOffsetInCodeWithRelJump;
    u32 uPosInIRofTargetMarker;
};

local_func void winx64_emit_local_jump(EX64JumpCodeRelative eX64RelJumpOpcode, u32 uPosInIRofTargetMarker,
    TmpArray<LocalJumpInfo>& ioVecLocalJumps, WinX64BackendCtx* pCtx)
{
    if (eX64RelJumpOpcode != EX64JUMPCODE_NONE) {
        u8 tData[32u];
        u32 uPosOfInstruction = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
        u8 uBytesToOffset;
        u8 uTotalBytes = (eX64RelJumpOpcode != EX64JUMPCODE_RELATIVE_JMP) ?
            x64_encode_jcc_relative(eX64RelJumpOpcode, tData, uPosOfInstruction, &uBytesToOffset) :
            x64_encode_jmp_relative(tData, uPosOfInstruction, &uBytesToOffset);
        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uTotalBytes);
        Assert_(u8(ir_access_repo_instr(pCtx->pRepo, uPosInIRofTargetMarker).uInstrCodeAndFormatAndFirstParam) == IRIT_MARKER_JUMP_TARGET);
        ioVecLocalJumps.append( LocalJumpInfo { uPosOfInstruction+u32(uBytesToOffset), uPosInIRofTargetMarker } );
    }
}

local_func void winx64_emit_local_jump_to_err(EX64JumpCodeRelative eX64RelJumpOpcode,
    TmpArray<u32>& ioVecJumpsToErrCheck, WinX64BackendCtx* pCtx)
{
    Assert_(eX64RelJumpOpcode != EX64JUMPCODE_NONE);
    u8 tData[32u];
    u32 uPosOfInstruction = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
    u8 uBytesToOffset;
    u8 uTotalBytes = (eX64RelJumpOpcode != EX64JUMPCODE_RELATIVE_JMP) ?
        x64_encode_jcc_relative(eX64RelJumpOpcode, tData, uPosOfInstruction, &uBytesToOffset) :
        x64_encode_jmp_relative(tData, uPosOfInstruction, &uBytesToOffset);
    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tData, uTotalBytes);
    ioVecJumpsToErrCheck.append(uPosOfInstruction+u32(uBytesToOffset));
}

local_func void winx64_emit_memcpy_by_ir(u8 uFormat, u64 uIRofDest, u64 uIRofSource, const X64OperandRef& refSlotsCount, u32 uAlignLog2Dest, u32 uAlignLog2Src,
    WinX64ProcInfo* pProcInfo, u32* pLocalAssign, TmpArray<u32>& vecLocalConstsToEmit, WinX64BackendCtx* pCtx)
{
    Assert_(uAlignLog2Dest >= get_log2_of_natural_align_from_format(uFormat));
    Assert_(uAlignLog2Src >= get_log2_of_natural_align_from_format(uFormat));
    u32 uSlotSizeLog2 = get_log2_of_slot_size_from_format(uFormat);
    // TODO:
    // * if ensured align src and dest > slot size, we can increase our move around format...
    // * if slot size > 64b ; clamp move around format to 64b (or implement with XMM for 128b moves or YMM for 256b registers)
    // * in both the above cases, we need to divide or multiply or loop counter with the applied factor from slot size to move around format.
    Assert(uSlotSizeLog2 <= 3u, "winx64_emit_memcpy_by_ir() : Above 64b slots not yet implemented");
    u8 uScaleLog2 = u8(uSlotSizeLog2);
    u8 uMoveAroundFormat = u8(uSlotSizeLog2);

    //
    // LEA RAX <- refFromIR_read(uIRofSource)
    // LEA RDX <- refFromIR_write(uIRofDest)
    // MOV ECX <- refSlotsCount                 #(r32)
    // SUB ECX, 1                               # could be skipped and replaced by MOV ECX <- #SlotsCount-1 if refSlotsCount is immediate...
    // repeat:
    //    MOV xBX <- [RAX + RCX*scale]          #(uMoveAroundFormat) - "scale" is log2 of format bytecount (0..3 => 1B ; 2B ; 4B ; 8B => supports 8b..64b)
    //    MOV [RDX + RCX*scale] <- xBX          #(uMoveAroundFormat) - "scale" is log2 of format bytecount (0..3 => 1B ; 2B ; 4B ; 8B => supports 8b..64b)
    // LOOP repeat                              # will auto-decrease ECX and stop when 0
    // last_one_at_index_0:
    //    MOV xBX <- [RAX]                      #(uMoveAroundFormat) 
    //    MOV [RDX] <- xBX                      #(uMoveAroundFormat)
    //

    // note: 'loop' is 0xE2 8bImm

    X64OperandRef ECX = make_operand_reg(REG_X64_xCX);
    if (refSlotsCount.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
        i64 iSlotsCount = i64(immu64_from_operand_imm(refSlotsCount));
        Assert_(iSlotsCount > 1); // TODO ?
        Assert_(iSlotsCount < 0x0100'0000);
        i64 iLastSlot = iSlotsCount - 1;
        Assert_(iLastSlot);
        // TODO: also unroll that loop if iLastSlot is 1 (or even very small ?)
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, ECX, make_operand_immediate(iLastSlot), 0x02u, pCtx);
    } else {
        // TODO: mul by number of "max 64b slot" per IR slot, check > 0 and > 1... 
        Assert(false, "winx64_emit_memcpy_by_ir() : non-immediate count not yet implemented");
    }

    X64OperandRef derefSrc  = winx64_get_x64_operand_from_ir_param(uIRofSource, pProcInfo, pLocalAssign, vecLocalConstsToEmit,
        EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_R8x, eRegCategory(0u), pCtx);
    Assert_(0u == (derefSrc.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    Assert_(0u == (derefSrc.uFlags & X64OPERAND_FLAG_IS_REG)); // TODO: support reg ?
    X64OperandRef derefDest = winx64_get_x64_operand_from_ir_param(uIRofDest, pProcInfo, pLocalAssign, vecLocalConstsToEmit,
        EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_R9x, eRegCategory(0u), pCtx);
    Assert_(0u == (derefDest.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    Assert_(0u == (derefDest.uFlags & X64OPERAND_FLAG_IS_REG)); // TODO: support reg ?
    X64OperandRef RAX = make_operand_reg(REG_X64_xAX);
    X64OperandRef RDX = make_operand_reg(REG_X64_xDX);
    X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, RAX, derefSrc, 0x03u, pCtx);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, RDX, derefDest, 0x03u, pCtx);
    u32 uPosOfStartLoop = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
    X64OperandRef fromMemAtRAXplusECXtimesScale = make_operand_at_address_in_reg_indexed(REG_X64_xAX, REG_X64_xCX, uScaleLog2);
    X64OperandRef toMemAtRDXplusECXtimesScale = make_operand_at_address_in_reg_indexed(REG_X64_xDX, REG_X64_xCX, uScaleLog2);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, xBX, fromMemAtRAXplusECXtimesScale, uMoveAroundFormat, pCtx);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, toMemAtRDXplusECXtimesScale, xBX, uMoveAroundFormat, pCtx);
    u32 uPosOfLoopInstruction = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
    u32 uInstructionAfterLoop = uPosOfLoopInstruction + 2u;
    Assert_(uPosOfStartLoop + 128 > uInstructionAfterLoop);
    i8 iDiff = i8(uPosOfStartLoop - uInstructionAfterLoop);
    Assert_(iDiff < 0);
    u8 tLoop[2] = { 0xE2u, u8(iDiff) }; // ie. decrement ECX ; then if ECX != 0 goto start of loop
    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tLoop, 2u);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, xBX, make_operand_at_address_in_reg(REG_X64_xAX), uMoveAroundFormat,pCtx);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_at_address_in_reg(REG_X64_xDX), xBX, uMoveAroundFormat, pCtx);
}


struct CurrentProcParams {
    u32* pRepoInfo;
    u32* pLocalAssign;
    WinX64ProcInfo* pProcInfo;
    TmpArray<u32>* pVecLocalConstsToEmit;
    TmpArray<u32>* pVecJumpsToErrCheck;
    TmpArray<LocalJumpInfo>* pVecLocalJumps;
};

local_func void winx64_emit_setzero_by_ir_naive(u8 uFormat, u64 uIRofDest, const X64OperandRef& refSlotsCount, u32 uAlignLog2,
    const CurrentProcParams& procParams, WinX64BackendCtx* pCtx)
{
    Assert_(uAlignLog2 >= get_log2_of_natural_align_from_format(uFormat));
    u32 uSlotSizeLog2 = get_log2_of_slot_size_from_format(uFormat);
    // TODO:
    // * if ensured align > slot size, we can increase our move around format...
    // * if slot size > 64b ; clamp move around format to 64b (or implement with XMM for 128b moves or YMM for 256b registers)
    // * in both the above cases, we need to divide or multiply or loop counter with the applied factor from slot size to move around format.
    Assert(uSlotSizeLog2 <= 3u, "winx64_emit_memcpy_by_ir() : Above 64b slots not yet implemented");
    u8 uScaleLog2 = u8(uSlotSizeLog2);
    u8 uMoveAroundFormat = u8(uSlotSizeLog2);

    //
    // LEA RDX <- refFromIR_write(uIRofDest)
    // XOR RBX, RBX
    // MOV ECX <- refSlotsCount                 #(r32)
    // SUB ECX, 1                               # could be skipped and replaced by MOV ECX <- #SlotsCount-1 if refSlotsCount is immediate...
    // repeat:
    //    MOV [RDX + RCX*scale] <- xBX          #(uMoveAroundFormat) - "scale" is log2 of format bytecount (0..3 => 1B ; 2B ; 4B ; 8B => supports 8b..64b)
    // LOOP repeat                              # will auto-decrease ECX and stop when 0
    // last_one_at_index_0:
    //    MOV [RDX] <- xBX                      #(uMoveAroundFormat)
    //

    // note: 'loop' is 0xE2 8bImm

    // TODO: mul by number of "max 64b slot" per IR slot... 

    X64OperandRef ECX = make_operand_reg(REG_X64_xCX);
    if (refSlotsCount.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
        i64 iSlotsCount = i64(immu64_from_operand_imm(refSlotsCount));
        Assert_(iSlotsCount > 1);
        Assert_(iSlotsCount < 0x0100'0000);
        i64 iLastSlot = iSlotsCount - 1;
        Assert_(iLastSlot);
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, ECX, make_operand_immediate(iLastSlot), 0x02u, pCtx);
    } else {
        // TODO: check if > 1 before that... if not:
            // if 0: jump over whole to after all that, for a NOOP.
            // if 1: only do last part (after loop), for a single assign.
        Assert(false, "winx64_emit_setzero_by_ir_naive() : non-immediate not implemented");
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, ECX, refSlotsCount, 0x02u, pCtx);
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_SUB, ECX, make_operand_immediate(1u), 0x02u, pCtx);
    }

    X64OperandRef derefDest = winx64_get_x64_operand_from_ir_param(uIRofDest, procParams.pProcInfo, procParams.pLocalAssign,
        *(procParams.pVecLocalConstsToEmit), EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_R9x, eRegCategory(0u), pCtx);
    Assert_(0u == (derefDest.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    Assert_(0u == (derefDest.uFlags & X64OPERAND_FLAG_IS_REG)); // TODO: support reg ?
    X64OperandRef RDX = make_operand_reg(REG_X64_xDX);
    X64OperandRef xBX = make_operand_reg(REG_X64_xBX);

    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, RDX, derefDest, 0x03u, pCtx);
    constexpr u8 tXorEBXEBX[] = { 0x31u, 0xDBu };
    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tXorEBXEBX, sizeof(tXorEBXEBX));
    u32 uPosOfStartLoop = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
    X64OperandRef toMemAtRDXplusECXtimesScale = make_operand_at_address_in_reg_indexed(REG_X64_xDX, REG_X64_xCX, uScaleLog2);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, toMemAtRDXplusECXtimesScale, xBX, uMoveAroundFormat, pCtx);
    u32 uPosOfLoopInstruction = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
    u32 uInstructionAfterLoop = uPosOfLoopInstruction + 2u;
    Assert_(uPosOfStartLoop + 128 > uInstructionAfterLoop);
    i8 iDiff = i8(uPosOfStartLoop - uInstructionAfterLoop);
    Assert_(iDiff < 0);
    u8 tLoop[2] = { 0xE2u, u8(iDiff) }; // ie. decrement ECX ; then if ECX != 0 goto start of loop
    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tLoop, 2u);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_at_address_in_reg(REG_X64_xDX), xBX, uMoveAroundFormat, pCtx);
}

local_func_inl void winx64_emit_zeroing_ref(u8 uFormat, const CurrentProcParams& procParams,
    const X64OperandRef& refToZero, WinX64BackendCtx* pCtx)
{
    Assert_(0u == (refToZero.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    if (refToZero.uFlags & X64OPERAND_FLAG_IS_REG) {
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_XOR, refToZero, refToZero, 0x02u, pCtx); // format 0x02 (32b) is small and enough for all zeroing on x64
    } else {
        if (uFormat < 0x02u) { // 8b or 16b : shorter to mov immediate 0
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refToZero, make_operand_immediate(0), uFormat, pCtx);
        } else { // 32b: 6 bytes+ for a mov immediate 0; 64b : 7 bytes+ for a mov immediate 0
            // Whereas XOR EBX is 2 bytes, then mov => 4+ for 32b, 5+ for 64b => net win.
            Assert_((refToZero.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) ||
                (refToZero.uCodeBase != REG_X64_xBX && refToZero.uCodeIndex != REG_X64_xBX)); // Otherwise, we'd need a more roust alloc scheme
            X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
            constexpr u8 tXorEBXEBX[] = { 0x31u, 0xDBu };
            write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tXorEBXEBX, sizeof(tXorEBXEBX));
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refToZero, xBX, uFormat, pCtx);
        }
    }
}

local_func void winx64_emit_setzero_by_ir_optimizable(u8 uFormat, u64 uIRofDest, IRInfo& infoSlotsCount, u32 uAlignLog2,
    const CurrentProcParams& procParams, WinX64BackendCtx* pCtx)
{
    Assert_(uAlignLog2 >= get_log2_of_natural_align_from_format(uFormat));
    u32 uSlotSizeLog2 = get_log2_of_slot_size_from_format(uFormat);
    // TODO:
    // * if ensured align > slot size, we can increase our move around format...
    // * if slot size > 64b ; clamp move around format to 64b (or implement with XMM for 128b moves or YMM for 256b registers)
    // * in both the above cases, we need to divide or multiply or loop counter with the applied factor from slot size to move around format.
    Assert(uSlotSizeLog2 <= 3u, "winx64_emit_memcpy_by_ir() : Above 64b slots not yet implemented");
    u8 uScaleLog2 = u8(uSlotSizeLog2);
    u8 uMoveAroundFormat = u8(uSlotSizeLog2);

    if (irflag_is_known_non_nyka(infoSlotsCount.uIRandMetaFlags)) {
        Assert_(irflag_is_known_embd(infoSlotsCount.uIRandMetaFlags));
        u32 uSlotsCount = u32(infoSlotsCount.metaValue.knownValue.uEmbeddedValue);
        if (uSlotsCount < 4u) {
            if (uSlotsCount == 0u)
                return; // NOOP...
            X64OperandRef derefDest = winx64_get_x64_operand_from_ir_param(uIRofDest, procParams.pProcInfo,
                procParams.pLocalAssign, *(procParams.pVecLocalConstsToEmit),
                EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_R9x, eRegCategory(0u), pCtx);
            if (uSlotsCount == 1u) {
                winx64_emit_zeroing_ref(uMoveAroundFormat, procParams, derefDest, pCtx);
            } else {
                X64OperandRef RDX = make_operand_reg(REG_X64_xDX);
                X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, RDX, derefDest, 0x03u, pCtx);
                constexpr u8 tXorEBXEBX[] = { 0x31u, 0xDBu };
                write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tXorEBXEBX, sizeof(tXorEBXEBX));
                if (uSlotsCount > 2u)
                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                        make_operand_at_address_in_reg(REG_X64_xDX, 2u << uScaleLog2),
                        xBX, uMoveAroundFormat, pCtx);
                if (uSlotsCount > 1u)
                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                        make_operand_at_address_in_reg(REG_X64_xDX, 1u << uScaleLog2),
                        xBX, uMoveAroundFormat, pCtx);
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, derefDest, xBX, uMoveAroundFormat, pCtx);
            }
        } else {
            winx64_emit_setzero_by_ir_naive(uFormat, uIRofDest, make_operand_immediate(u64(uSlotsCount)), uAlignLog2, procParams, pCtx);
        }
    }
}

local_func void winx64_emit_shift_by_constant(u8 uIRIT, u32 uInstrFlags, u8 uStdFormat,
    const CurrentProcParams& procParams, const X64OperandRef& refResult,
    const X64OperandRef& refOpA, u32 uKnownShiftAmount, WinX64BackendCtx* pCtx)
{
    Assert_(uIRIT == IRIT_BIT_LSH || uIRIT == IRIT_BIT_RSH);
    Assert(uStdFormat <= 0x03u, "Shifts on higher bitcount than 64b not yet implemented");
    u8 tEncoding[32];
    u8 uByteCount = 0u;
    u8 uImm8 = 0u;
    if (uStdFormat == 0x03u) { // 64b
        tEncoding[0] = X64_OPCODE_PREFIX_REX_W;
        uByteCount++;
    } else if (uStdFormat == 0x01u) { // 16b
        tEncoding[0] = X64_OPCODE_PREFIX_16b;
        uByteCount++;
    }
    Assert_(uKnownShiftAmount < (8uLL << uStdFormat));
    if (uKnownShiftAmount == 1u) {
        tEncoding[uByteCount] = 0xD0u;  // SHL/SHR/SAR r/m8 by 1
    } else {
        if (uKnownShiftAmount) {
            uImm8 = u8(uKnownShiftAmount);
            tEncoding[uByteCount] = 0xC0u;  // SHL/SHR/SAR r/m8 by 8b immediate
        } else {
            // shift by 0 => identity function
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refOpA, uStdFormat, pCtx);
            return;
        }
    }

    if (uStdFormat) // > 8b
        tEncoding[uByteCount] |= X64_OPCODE_MODIFIER_8b_TO_16_32_64b;
    uByteCount++;
    X64OperandRef refTmpResult = make_operand_reg(REG_X64_xAX);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refTmpResult, refOpA, uStdFormat, pCtx);
    u8 uRegCode = 4u;      // SHL
    if (uIRIT == IRIT_BIT_RSH) {
        if (uInstrFlags & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED)
            uRegCode = 5u; // SHR
        else
            uRegCode = 7u; // SAR
    }
    tEncoding[uByteCount] = X64_MODRM_DIRECT_RAX_OR_XMM0 | (uRegCode << 3u);
    uByteCount++;
    if (uImm8) {
        tEncoding[uByteCount] = uImm8;
        uByteCount++;
    }
    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tEncoding, uByteCount);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refTmpResult, uStdFormat, pCtx);
}

local_func void winx64_emit_shift_by_unknown(u8 uIRIT, u32 uInstrFlags, u8 uStdFormat,
    const CurrentProcParams& procParams, const X64OperandRef& refResult,
    const X64OperandRef& refOpA, const X64OperandRef& refShiftAmount, WinX64BackendCtx* pCtx)
{
    Assert_(uIRIT == IRIT_BIT_LSH || uIRIT == IRIT_BIT_RSH);
    Assert(uStdFormat <= 0x03u, "Shifts on higher bitcount than 64b not yet implemented");
    Assert_(0u == (refShiftAmount.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    u8 tEncoding[32];
    u8 uByteCount = 0u;
    if (uStdFormat == 0x03u) { // 64b
        tEncoding[0] = X64_OPCODE_PREFIX_REX_W;
        uByteCount++;
    } else if (uStdFormat == 0x01u) { // 16b
        tEncoding[0] = X64_OPCODE_PREFIX_16b;
        uByteCount++;
    }
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_reg(REG_X64_xCX), refShiftAmount, 0x00u, pCtx);
    tEncoding[uByteCount] = 0xD2u;  // SHL/SHR/SAR r/m8 by CL
    if (uStdFormat) // > 8b
        tEncoding[uByteCount] |= X64_OPCODE_MODIFIER_8b_TO_16_32_64b;
    uByteCount++;
    X64OperandRef refTmpResult = make_operand_reg(REG_X64_xAX);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refTmpResult, refOpA, uStdFormat, pCtx);
    u8 uRegCode = 4u;      // SHL
    if (uIRIT == IRIT_BIT_RSH) {
        if (uInstrFlags & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED)
            uRegCode = 5u; // SHR
        else
            uRegCode = 7u; // SAR
    }
    tEncoding[uByteCount] = X64_MODRM_DIRECT_RAX_OR_XMM0 | (uRegCode << 3u);
    uByteCount++;
    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tEncoding, uByteCount);
    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refTmpResult, uStdFormat, pCtx);
}

// Returns the number of *additional* IRIT instructions which have been eaten
local_func u32 winx64_emit_mul_related_from_IR_naive(u8 uIRIT, u32 uInstrFlags, u32 uInstrIndex, u8 uStdFormat,
    const CurrentProcParams& procParams, const X64OperandRef& refOpA, const X64OperandRef& refOpB, WinX64BackendCtx* pCtx)
{
    Assert_(uIRIT == IRIT_MUL || uIRIT == IRIT_MUL_U);
    Assert_(uStdFormat <= 0x03u);
    X64OperandRef refResult = make_operand_reg(REG_X64_xAX);
    if (refOpB.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
        Assert_(0u == (refOpA.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
        u8 uIntermediateFormat = uStdFormat;
        if (uStdFormat == 0x03u) {
            u64 uAsImmediate = immu64_from_operand_imm(refOpB);
            if (can_encode_32b_signed(uAsImmediate)) {
                if (i32(uAsImmediate) >= 0) {
                    uIntermediateFormat = 0x02u; // takes advantage of the fact that high 32b of RBX will get zeroed out, to encode a much smaller MOV instruction.
                }
            }
        }
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                refResult,
                                refOpB, uIntermediateFormat, pCtx);
        winx64_emit_singleRM_op((uIRIT == IRIT_MUL) ? EX64_SINGLERM_OP_IMUL : EX64_SINGLERM_OP_MUL,
                                refOpA, uStdFormat, pCtx);
    } else {
        u8 uIntermediateFormat = uStdFormat;
        if (uStdFormat == 0x03u && (refOpA.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE)) {
            u64 uAsImmediate = immu64_from_operand_imm(refOpA);
            if (can_encode_32b_signed(uAsImmediate)) {
                if (i32(uAsImmediate) >= 0) {
                    uIntermediateFormat = 0x02u; // takes advantage of the fact that high 32b of RBX will get zeroed out, to encode a much smaller MOV instruction.
                }
            }
        }
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                refResult,
                                refOpA, uIntermediateFormat, pCtx);
        winx64_emit_singleRM_op((uIRIT == IRIT_MUL) ? EX64_SINGLERM_OP_IMUL : EX64_SINGLERM_OP_MUL,
                                refOpB, uStdFormat, pCtx);
    }

    u32 uCheckFlags = uInstrFlags &
        (IR_INSTRFLAG_EMBD_CHECK|IR_INSTRFLAG_POSTOP_CHKSIGNED|IR_INSTRFLAG_POSTOP_CHKUNSIGNED|IR_INSTRFLAG_POSTOP_SPECIAL);
    // Possible checks
    u32 uIncreaseFromCheck = 0u;
    if (uCheckFlags & IR_INSTRFLAG_EMBD_CHECK) {
        uIncreaseFromCheck = 1u;
        IREntry& nextEntry = ir_access_repo_instr(pCtx->pRepo, uInstrIndex+uIncreaseFromCheck);
        Assert_(u8(nextEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_ERRCHK);
        u32 uIndexOfLocalErrCheck = u32(nextEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
        Assert_(uIndexOfLocalErrCheck < pCtx->pProcResult->vecErrChecks.size());
        u32 uByteOffsetToErrInfoTableForThis = uIndexOfLocalErrCheck * 8u * 4u; // 8x 32b entries per local errcheck index
        if (nextEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_ERRCHK_IS_ACTIVE) {
            Assert_(nextEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_BRANCH_ON_NONZERO);
            // Moving byte offset to local err check to EBX (effectively RBX since ensured positive, and zero extended)
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                    make_operand_reg(REG_X64_xBX),
                                    make_operand_immediate(i64(i32(uByteOffsetToErrInfoTableForThis))),
                                    0x02u, pCtx);
            if (0u == (uCheckFlags & IR_INSTRFLAG_POSTOP_SPECIAL)) {
                u32 uPosOfJumpInstruction = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
                u8 tJumpIfErr[] = { 0x0F, 0x80, 0x00, 0x00, 0x00, 0x00 }; // jump near (rel to IP of next instr.) if overflow
                constexpr u32 uOffsetToOffset = 2u;
                u32 uPosOfNextInstruction = uPosOfJumpInstruction + 6u;
                u32 uTmpOffset = 0u - uPosOfNextInstruction;
                memcpy(tJumpIfErr + uOffsetToOffset, &uTmpOffset, 4u);
                write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tJumpIfErr, sizeof(tJumpIfErr));
                procParams.pVecJumpsToErrCheck->append(uPosOfJumpInstruction + uOffsetToOffset);
            } else {
                // TODO
                Assert(false, "special-semantics checks not yet implemented");
            }
        }
    } else {
        if (uCheckFlags == 0u) {
            // NOOP
        } else {
            //TODO
            Assert(false, "non-embedded checks not yet implemented");
        }
    }

    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                            winx64_get_write_operand_from_local_assign(uInstrIndex, procParams.pLocalAssign),
                            refResult, uStdFormat, pCtx);

    return uIncreaseFromCheck;
}

// Returns the number of *additional* IRIT instructions which have been eaten
// For when one or the other operand is a known value
local_func u32 winx64_emit_mul_related_from_IR_optimizable(u8 uIRIT, u32 uInstrFlags, u32 uInstrIndex, u8 uStdFormat,
    const CurrentProcParams& procParams, const X64OperandRef& refOpA, const IRInfo& infoB,
    eRegCategory uToUseIfNaiveOpB1, eRegCategory uToUseIfNaiveOpB2, WinX64BackendCtx* pCtx)
{
    Assert_(uIRIT == IRIT_MUL || uIRIT == IRIT_MUL_U);
    Assert_(uStdFormat <= 0x03u);
    
    Assert_(irflag_is_known_non_nyka(infoB.uIRandMetaFlags));
    Assert_(irflag_is_known_embd(infoB.uIRandMetaFlags));
    u64 uEmbdValue64 = infoB.metaValue.knownValue.uEmbeddedValue;

    u32 uCheckFlags = uInstrFlags &
        (IR_INSTRFLAG_EMBD_CHECK|IR_INSTRFLAG_POSTOP_CHKSIGNED|IR_INSTRFLAG_POSTOP_CHKUNSIGNED|IR_INSTRFLAG_POSTOP_SPECIAL);
    u32 uIncreaseFromCheck = 0u;

    if (uEmbdValue64 == 0uLL) { // signed or unsigned, mul by 0 is 0... and 0 had embd value info 0 for all formats <= 0x03u

        X64OperandRef refResult = winx64_get_write_operand_from_local_assign(uInstrIndex, procParams.pLocalAssign);
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, make_operand_immediate(0), uStdFormat, pCtx);
        if (uCheckFlags) {
            uIncreaseFromCheck = 1u;
            Assert(uCheckFlags & IR_INSTRFLAG_EMBD_CHECK, "non-embedded checks not yet implemented"); // TODO
        }
        return uIncreaseFromCheck;

    } else if (uEmbdValue64 == 1uLL) { // signed or unsigned, mul by 1 is identity... and 1 had embd value info 1 for all formats <= 0x03u
    
        X64OperandRef refResult = winx64_get_write_operand_from_local_assign(uInstrIndex, procParams.pLocalAssign);
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refOpA, uStdFormat, pCtx);
        if (uCheckFlags) {
            uIncreaseFromCheck = 1u;
            Assert(uCheckFlags & IR_INSTRFLAG_EMBD_CHECK, "non-embedded checks not yet implemented"); // TODO
        }
        return uIncreaseFromCheck;

    } else if (uIRIT == IRIT_MUL_U) { // Unsigned case
    
        if (0uLL == ((uEmbdValue64 - 1uLL) & uEmbdValue64)) { // If that test is true, value is a power of two...
            Assert_(uEmbdValue64);
            int iBitWith1 = GetPosOfMostSignificantBitSet64(uEmbdValue64);
            Assert_(iBitWith1 > 0);
            Assert_(uEmbdValue64 == 1uLL << iBitWith1);
            X64OperandRef refResult = winx64_get_write_operand_from_local_assign(uInstrIndex, procParams.pLocalAssign);
            winx64_emit_shift_by_constant(IRIT_BIT_LSH, 0u, uStdFormat, procParams, refResult, refOpA, u32(iBitWith1), pCtx);
            if (uCheckFlags) {
                uIncreaseFromCheck = 1u;
                Assert(uCheckFlags & IR_INSTRFLAG_EMBD_CHECK, "non-embedded checks not yet implemented"); // TODO
            }
            return uIncreaseFromCheck;
        }
        // TODO: mul by 3: LEA [reg + 2*reg]
        // TODO: mul by 5: LEA [reg + 4*reg]
        // TODO: mul by 6 ?
        // TODO: mul by 7 ?
        // TODO: mul by 9: LEA [reg + 8*reg]
        // TODO: mul by 10 ?
        // TODO: mul by 12 ?
        // TODO: 2^n + 1 : left shift by n then add
        // note: 2^n - 1 could be left shift by n then sub, if it wasn't for the possible tmp value overflow problem

    } else { Assert_(uIRIT == IRIT_MUL_U); // Signed case

        i64 iAsSigned64 = i64(uEmbdValue64);
        if (uStdFormat == 0x00u) {
            iAsSigned64 = i64(i8(uEmbdValue64));
        } else if (uStdFormat == 0x01u) {
            iAsSigned64 = i64(i16(uEmbdValue64));
        } else if (uStdFormat == 0x02u) {
            iAsSigned64 = i64(i32(uEmbdValue64));
        }
        if (iAsSigned64 > 0) {
            if (0uLL == ((uEmbdValue64 - 1uLL) & uEmbdValue64)) { // If that test is true, value is a power of two...
                Assert_(uEmbdValue64);
                int iBitWith1 = GetPosOfMostSignificantBitSet64(uEmbdValue64);
                Assert_(iBitWith1 > 0);
                Assert_(uEmbdValue64 == 1uLL << iBitWith1);
                X64OperandRef refResult = winx64_get_write_operand_from_local_assign(uInstrIndex, procParams.pLocalAssign);
                winx64_emit_shift_by_constant(IRIT_BIT_LSH, 0u, uStdFormat, procParams, refResult, refOpA, u32(iBitWith1), pCtx);
                if (uCheckFlags) {
                    uIncreaseFromCheck = 1u;
                    Assert(uCheckFlags & IR_INSTRFLAG_EMBD_CHECK, "non-embedded checks not yet implemented"); // TODO
                }
                return uIncreaseFromCheck;
            }
            // TODO: mul by 3   ?
            // TODO: mul by 5   ?
            // TODO: mul by 6   ?
            // TODO: mul by 7   ?
            // TODO: mul by 9   ?
            // TODO: mul by 10  ?
            // TODO: mul by 12  ?
            // TODO: mul by 2^n + 1 : left shift by n then add
            // note: mul by 2^n - 1 : left shift by n then sub (no tmp value overflow problem here as signed)
        } else {
            // TODO: mul by -1
            // TODO: mul by -2^n ?
            // TODO: mul by -3   ?
            // TODO: mul by -5   ?
            // TODO: mul by -6   ?
            // TODO: mul by -7   ?
            // TODO: mul by -9   ?
            // TODO: mul by -10  ?
            // TODO: mul by -12  ?
            // TODO: mul by -(2^n + 1) : left shift by n then add, then neg ?
            // TODO: mul by -(2^n - 1) : left shift by n then sub (no tmp value overflow problem here as signed), then neg ?
        }
    }

    // Falling back to naive mul op emission in all other cases
    X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(infoB.uIRandMetaFlags & IR_STD_PARAM_MASK,
        procParams.pProcInfo, procParams.pLocalAssign, *(procParams.pVecLocalConstsToEmit),
        get_imm_integrals_from_format(uStdFormat), uToUseIfNaiveOpB1, uToUseIfNaiveOpB2, pCtx);
    return winx64_emit_mul_related_from_IR_naive(uIRIT, uInstrFlags, uInstrIndex, uStdFormat, procParams, refOpA, refOpB, pCtx);
}

// Returns the number of *additional* IRIT instructions which have been eaten
local_func u32 winx64_emit_div_related_from_IR_naive(u8 uIRIT, u32 uInstrFlags, u32 uInstrIndex, u8 uStdFormat,
    const CurrentProcParams& procParams, const X64OperandRef& refOpA, const X64OperandRef& refDivisor, WinX64BackendCtx* pCtx)
{
    Assert_(uIRIT == IRIT_QUO || uIRIT == IRIT_EXACT_QUO || uIRIT == IRIT_REM || uIRIT == IRIT_MOD);
    Assert_(uStdFormat <= 0x03u);

    if (uStdFormat) {
        // xor RDX, and mov operandA in xAX
        constexpr u8 tXorEDX[2] = { 0x31u, 0xD2u }; // xoring EDX with itself should zero RDX in full...
        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tXorEDX, sizeof(tXorEDX));
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_reg(REG_X64_xAX), refOpA, uStdFormat, pCtx);
    } else {
        // zero-extend 8b operandA to AX => zeroing AH
        winx64_emit_mov_extend(REG_X64_xAX, 0x01u, refOpA, 0x00u, EIntSemantics::EINT_SEMANTIC_UNSIGNED, pCtx);
    }
    EX64SingleRMOp uSingleRmOp = (uInstrFlags & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED) ?
        EX64_SINGLERM_OP_DIV : EX64_SINGLERM_OP_IDIV;
    if (refDivisor.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
        X64OperandRef xBX = make_operand_reg(REG_X64_xBX);
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, xBX, refDivisor, uStdFormat, pCtx);
        winx64_emit_singleRM_op(uSingleRmOp, xBX, uStdFormat, pCtx);
    } else {
        winx64_emit_singleRM_op(uSingleRmOp, refDivisor, uStdFormat, pCtx);
    }
    X64OperandRef refDest = winx64_get_write_operand_from_local_assign(uInstrIndex, procParams.pLocalAssign);
    // Possible checks
    u32 uIncreaseFromCheck = 0u;
    switch (uIRIT) {

        case IRIT_EXACT_QUO: {
            if (uInstrFlags & IR_INSTRFLAG_EMBD_CHECK) {
                uIncreaseFromCheck = 1u;
                IREntry& nextEntry = ir_access_repo_instr(pCtx->pRepo, uInstrIndex+uIncreaseFromCheck);
                Assert_(u8(nextEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_ERRCHK);
                u32 uIndexOfLocalErrCheck = u32(nextEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                Assert_(uIndexOfLocalErrCheck < pCtx->pProcResult->vecErrChecks.size());
                u32 uByteOffsetToErrInfoTableForThis = uIndexOfLocalErrCheck * 8u * 4u; // 8x 32b entries per local errcheck index
                if (nextEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_ERRCHK_IS_ACTIVE) {
                    Assert_(nextEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_BRANCH_ON_NONZERO);
                    // TODO
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "*** warning : IRIT_EXACT_QUO: embedded check not yet implemented: skipped"), pCtx->pWorker);
                }
            } else {
                //TODO
                Assert(false, "non-embedded checks not yet implemented");
            }
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refDest, make_operand_reg(REG_X64_xAX), uStdFormat, pCtx);
        } break;

        case IRIT_QUO: { // if signed, it truncates towards zero
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refDest, make_operand_reg(REG_X64_xAX), uStdFormat, pCtx);
        } break;

        case IRIT_REM: { // if signed, it abides to q * b + r = a => negative whenever *a* is
            if (uStdFormat) { // in case format is 16b+, remainder is in xDX
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refDest, make_operand_reg(REG_X64_xDX), uStdFormat, pCtx);
            } else { // cannot mov directly from ah in a number of cases where we need a REX prefix => prefer shifting it
                constexpr u8 tShrEAXBy8[3] = { 0xC1u, 0xE8u, 0x08u }; // shr whole EAX by 8 will, as a matter of fact, shift AH to AL ; and requires less bytes to encode...
                write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tShrEAXBy8, sizeof(tShrEAXBy8));
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refDest, make_operand_reg(REG_X64_xAX), uStdFormat, pCtx);
            }
        } break;

        case IRIT_MOD: { // we need to operate on sign of remainder...
            Assert(false, "MOD operation not yet implemented for x64");
        } break;

        default: Assume_(false);
    }

    return uIncreaseFromCheck;
}

// Returns the number of *additional* IRIT instructions which have been eaten
// For when the divisor is a known value
local_func u32 winx64_emit_div_related_from_IR_optimizable(u8 uIRIT, u32 uInstrFlags, u32 uInstrIndex, u8 uStdFormat,
    const CurrentProcParams& procParams, const X64OperandRef& refOpA, const IRInfo& infoDivisor, WinX64BackendCtx* pCtx)
{
    // TODO !!

    // Falling back to naive div op emission in all other cases
    X64OperandRef refDivisor = winx64_get_x64_operand_from_ir_param(infoDivisor.uIRandMetaFlags & IR_STD_PARAM_MASK,
        procParams.pProcInfo, procParams.pLocalAssign, *(procParams.pVecLocalConstsToEmit),
        get_imm_integrals_from_format(uStdFormat), REG_X64_R9x, REG_X64_xCX, pCtx);
    return winx64_emit_div_related_from_IR_naive(uIRIT, uInstrFlags, uInstrIndex, uStdFormat, procParams, refOpA, refDivisor, pCtx);
}

#endif // LOCLIB_WINX64_BACKEND_TOOLS_H_
