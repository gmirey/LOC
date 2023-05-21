#pragma once 

#ifndef LOCLIB_WINX64_BACKEND_H_
#define LOCLIB_WINX64_BACKEND_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_ProgramState.h"
#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_NodeValue.h"

#include "LocLib_PE.h"
#include "LocLib_x64.h"
#include "LocLib_winx64_backend_base.h"
#include "LocLib_winx64_backend_tools.h"

#include <time.h>

//-------------------------------------------------
// The WIN-X64 Backend Will proceed as follows:
//-------------------------------------------------
// for exe:
// We'll setup a PE file with common sections: .text (code), .rdata(consts), .data(global vars with a non-zero init), .bss(global vars with an initial value of all zeroes)
// We'll add one 32b array of additional info per IRRepo. This array will replace a mapping between declarations and positions in file:
//      32b as follow: 3msb 'location' : 0=uninitialized, non-zero=a particular section ; 29lsb: byte offset into that section where that thing is declared.
//          Special values: 0000'0000 : uninitialized and not marked for emission
//                          0000'0001 : uninitialized and already marked for emission
// We'll start emission from the IR of the declaration of a procedure recognized as the entry point (our caller shall provide it).
// --> We add that declaration in an array of "things to declare at global scope", then iterate:
// --> While there are things to declare at global scope, we go find the (hopefully 'declaration'), then:
//      * if declaration is of a (global) variable, either it is full zeroes, or it is not:
//          - if full-zero: we reserve some space in .bss, and record that position in the map of handled declarations.
//          - otherwise : we emit in .data using 'on_emit_known_value()' then record that position in the associated 32b additional info.
//      * if declaration is of a (global) constant:
//          - if non-proc:
//               - if non-RTTI, or RTTI is allowed, we emit in .rdata using 'on_emit_known_value()' then record that position in the associated 32b additional info.
//               - otherwise TODO
//          - if proc: we record that position in the associated 32b additional info, then start a proc emission procedure for the associated proc body.
// --> on_emit_known_value():
//      - if that value has no nykas:
//          we simply emit the runtime representation of the value to the section
//      - if that value is (or has) nykas:
//          we find back the IR of base from the nyka.
//          if the IR of base corresponds to a known 32b position (=> an already emitted declaration:)
//              we emit on 64b : the offset to the declaration in its own section, +nyka offset, +PE_DEFAULT_IMAGE_BASE
//              we record the position of that emission in a vector of locations to update by loader in case of base relocation.
//          otherwise:
//              we add that declaration in the array of "things to declare at global scope"
//              we emit on 64b : the nyka
//              we record the position of that emission in a vector of nykas to resolve later
// --> proc emission procedure:
//       we'll emit machine code instructions following the IR instructions.
//       --> we also temporarily allocate 32b array of additional info for the procwise IRRepo.
//       we shall in particular associate a stack space for each remaining local variable declaration *OR* temporary
//       --> some specific IR 'optim' shall also be able to convert thoses to using actual registers for tmp storage,
//           but this is not the role of the backend to do that right now from 'regular' IR... --> we can use the 32b additional info for that
//       we shall compute the required max stack size as we go (and be ready to patch that value later - with a provision to erase that altogether)
//       we shall record the positions of the jump targets as we go --> we can use the 32b additional info for that
//       we shall be able to know the position of the "next" instruction on instruction emission.
//       whenever some IR value is referenced:
//          - if value is a local var declaration or temporary : we can work out how to reference that, relative to stack pointer
//          - if value is a 32b immediate in our IR, it shall also be possible to emit a 32b immediate in x64
//          - if value is a global constant or variable - OR a nyka to a global constant or variable - OR a value being a nyka (we may also want to puch-through value to get if-nyka ?)
//              -> a priori it will always be referenced by address in x64, whether we need the value or the address...
//              -> but we'll solve for that address as 32b relative to IP
//              -> if the IR of base corresponds to a known 32b position (=> an already emitted declaration:)
//                  we emit on 32b : the IP of next instruction - (the offset to the declaration in its own section, +nyka offset)
//                  we record that position to a vector of 32b relative addresses to update later with the actual Virtual Address at the start of the section.
//              -> otherwise:
//                  if not yet marked for emission: we mark it for emission and add it to the array of "things to declare at global scope"
//                  we emit on 32b : the IP of next instruction
//                  we record that position + nyka in a vector of yet-to-be-32b-resolved-from-global-nykas
//       and... that's about it ?


#define IMAGE_DIRECTORY_ENTRY_EXPORT          0   // Export Directory
#define IMAGE_DIRECTORY_ENTRY_IMPORT          1   // Import Directory
#define IMAGE_DIRECTORY_ENTRY_RESOURCE        2   // Resource Directory
#define IMAGE_DIRECTORY_ENTRY_EXCEPTION       3   // Exception Directory
#define IMAGE_DIRECTORY_ENTRY_SECURITY        4   // Security Directory
#define IMAGE_DIRECTORY_ENTRY_BASERELOC       5   // Base Relocation Table
#define IMAGE_DIRECTORY_ENTRY_DEBUG           6   // Debug Directory
//      IMAGE_DIRECTORY_ENTRY_COPYRIGHT       7   // (X86 usage)
#define IMAGE_DIRECTORY_ENTRY_ARCHITECTURE    7   // Architecture Specific Data
#define IMAGE_DIRECTORY_ENTRY_GLOBALPTR       8   // RVA of GP
#define IMAGE_DIRECTORY_ENTRY_TLS             9   // TLS Directory
#define IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    10   // Load Configuration Directory
#define IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   11   // Bound Import Directory in headers
#define IMAGE_DIRECTORY_ENTRY_IAT            12   // Import Address Table
#define IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   13   // Delay Load Import Descriptors
#define IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR 14   // COM Runtime descriptor


local_func void winx64_backend_on_emit_proc(u64 uAsIR, WinX64BackendCtx* pCtx)
{
    Assert_(pCtx->pProcResult);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
        "Emitting code for proc %u in file %d", u64(pCtx->pProcResult->uRegistrationIndex),
        u64(u32(pCtx->pProcResult->iSourceFileIndex))), pCtx->pWorker);

    Assert_(pCtx->pRepo == &(pCtx->pProcResult->procwiseRepo));
    u32 uInstructionsCount = pCtx->pRepo->uSize;
    u32 uOffsetOfFirstInstruction = pCtx->pSections[PE_SECTION_CODE].uSize;

    auto itFound = pCtx->mapVecNotYetEmittedByProc.find(uAsIR);
    if (itFound != pCtx->mapVecNotYetEmittedByProc.end()) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Replacements of previously emitted offsets and addresses"), pCtx->pWorker);

        TmpArray<u32>* pVecToPatch = &(itFound.value().vecOffsetsInCodeToPatchWithProcOffset);
        u32 uCount = pVecToPatch->size();
        for (u32 i = 0u; i < uCount; i++) {
            u32 uOffset = (*pVecToPatch)[i];
            inc32_over_file_section(pCtx->pSections + PE_SECTION_CODE, uOffset, uOffsetOfFirstInstruction);
        }
        pVecToPatch = &(itFound.value().vecOffsetsInConstsToPatchWithProcAddress);
        uCount = pVecToPatch->size();
        for (u32 i = 0u; i < uCount; i++) {
            u32 uOffset = (*pVecToPatch)[i];
            inc64_over_file_section(pCtx->pSections + PE_SECTION_CONST, uOffset, u64(uOffsetOfFirstInstruction));
            pCtx->vecOffsetsInConstReferringToAddressOfProc.append(uOffset);
        }
        pVecToPatch = &(itFound.value().vecOffsetsInGlobIniToPatchWithProcAddress);
        uCount = pVecToPatch->size();
        for (u32 i = 0u; i < uCount; i++) {
            u32 uOffset = (*pVecToPatch)[i];
            inc64_over_file_section(pCtx->pSections + PE_SECTION_GLOBVAR_INI, uOffset, u64(uOffsetOfFirstInstruction));
            pCtx->vecOffsetsInGlobIniReferringToAddressOfProc.append(uOffset);
        }
        pCtx->vecOfSalvageableVec32s.append(itFound.value().vecOffsetsInCodeToPatchWithProcOffset);
        pCtx->vecOfSalvageableVec32s.append(itFound.value().vecOffsetsInConstsToPatchWithProcAddress);
        pCtx->vecOfSalvageableVec32s.append(itFound.value().vecOffsetsInGlobIniToPatchWithProcAddress);
        pCtx->mapVecNotYetEmittedByProc.remove_at_iter(itFound);
    }

    u32 uCurrentStackSize = 0u;
    const TypeInfo_ProcLike* procSign = pCtx->pProcResult->procSign;
    Assert_(0uLL == pCtx->pProcResult->uIsForeignSource); // foreign should be handled separately

    Assert_(uInstructionsCount);
    u32* pRepoInfo = (u32*)alloc_from(pCtx->secondaryTmpArena, uInstructionsCount * sizeof(u32), alignof(u32));
    pCtx->pCurrentRepoInfo = pRepoInfo;
    u32* pLocalAssign = (u32*)alloc_from(pCtx->secondaryTmpArena, uInstructionsCount * sizeof(u32), alignof(u32));
    WinX64ProcInfo procInfo;
    count_stack_usage_and_param_related_info(pCtx->pProcResult, &procInfo, pLocalAssign, pCtx);

    TmpArray<u32> vecLocalConstsToEmit(pCtx->secondaryTmpArena);
    TmpArray<u32> vecJumpsToErrCheck(pCtx->secondaryTmpArena);
    TmpArray<LocalJumpInfo> vecLocalJumps(pCtx->secondaryTmpArena);

    CurrentProcParams currentProcParams;
    currentProcParams.pRepoInfo = pRepoInfo;
    currentProcParams.pLocalAssign = pLocalAssign;
    currentProcParams.pProcInfo = &procInfo;
    currentProcParams.pVecLocalConstsToEmit = &vecLocalConstsToEmit;
    currentProcParams.pVecJumpsToErrCheck = &vecJumpsToErrCheck;
    currentProcParams.pVecLocalJumps = &vecLocalJumps;

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Emitting function prolog"), pCtx->pWorker);

        if (procInfo.uLocalStackSize) {
            // increasing stack size (decreasing stack ptr)
            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_SUB, make_operand_reg(REG_X64_xSP), make_operand_immediate(i64(u64(procInfo.uLocalStackSize))), 0x03u, pCtx);
        }

        u8 uParamCountOnStack = procInfo.uInParamsCount + procInfo.uRetRequiresFirstParamAsPtr;
        if (uParamCountOnStack) {
            // first in RCX or XMM0, which are very volatile -> we write it to its position on stack
            u8 uIndexOfFirst = 0u;
            u8 uFirstParamKind;
            u8 uFirstParamFormat = 0x03u;
            if (procInfo.uRetRequiresFirstParamAsPtr) {
                uIndexOfFirst = procInfo.uInParamsCount; // after in params is first ret param
                uFirstParamKind = EWin64ParamKind::EPARAM_KIND_REF;
            } else {
                uFirstParamKind = procInfo.tThisFuncInParams[0u].uParamKind;
                uFirstParamFormat = procInfo.tThisFuncInParams[0u].uFormat;
                // uFirstParamFormat = procInfo.tThisFuncOutParams[0u].uFormat; // -- no: for example for std regs, always write 64b : format could be less but with slot counts > 1u...
            }
            Assert_(0u == (pLocalAssign[uIndexOfFirst] & WIN64_BACKEND_LOCAL_KIND_MASK));
            if (uFirstParamKind == EWin64ParamKind::EPARAM_KIND_STD_REG) {
                goto write_rcx_to_first_param_on_stack;
            } else if (uFirstParamKind == EWin64ParamKind::EPARAM_KIND_REF) {
                uFirstParamFormat = 0x03u;
                write_rcx_to_first_param_on_stack:
                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                        make_operand_at_address_in_reg(REG_X64_xSP, i32(pLocalAssign[uIndexOfFirst])),
                                        make_operand_reg(REG_X64_xCX),
                                        uFirstParamFormat, pCtx);
            } else if (uFirstParamKind == EWin64ParamKind::EPARAM_KIND_XMM_REG) {
                // Write XMM0 to position on stack
                Assert(false, "write from XMM not yet implemented");
            }

            if (uParamCountOnStack > 1u) {
                // second in RDX or XMM1, which are very volatile -> we write it to its position on stack
                u8 uIndexOfSecond = 1u - procInfo.uRetRequiresFirstParamAsPtr;
                u8 uSecondParamKind = procInfo.tThisFuncInParams[uIndexOfSecond].uParamKind;
                u8 uSecondParamFormat = procInfo.tThisFuncInParams[uIndexOfSecond].uFormat;
                Assert_(0u == (pLocalAssign[uIndexOfSecond] & WIN64_BACKEND_LOCAL_KIND_MASK));
                if (uSecondParamKind == EWin64ParamKind::EPARAM_KIND_STD_REG) {
                    goto write_rdx_to_second_param_on_stack;
                } else if (uSecondParamKind == EWin64ParamKind::EPARAM_KIND_REF) {
                    uSecondParamFormat = 0x03u;
                    write_rdx_to_second_param_on_stack:
                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                            make_operand_at_address_in_reg(REG_X64_xSP, i32(pLocalAssign[uIndexOfSecond])),
                                            make_operand_reg(REG_X64_xDX),
                                            uSecondParamFormat, pCtx);
                } else if (uSecondParamKind == EWin64ParamKind::EPARAM_KIND_XMM_REG) {
                    // Write XMM1 to position on stack
                    Assert(false, "write from XMM not yet implemented");
                }

                if (uParamCountOnStack > 2u) {
                    // third in R8 or XMM2 -> we write it to its position on stack
                    u8 uIndexOfThird = 2u - procInfo.uRetRequiresFirstParamAsPtr;
                    u8 uThirdParamKind = procInfo.tThisFuncInParams[uIndexOfThird].uParamKind;
                    u8 uThirdParamFormat = procInfo.tThisFuncInParams[uIndexOfThird].uFormat;
                    Assert_(0u == (pLocalAssign[uIndexOfThird] & WIN64_BACKEND_LOCAL_KIND_MASK));
                    if (uThirdParamKind == EWin64ParamKind::EPARAM_KIND_STD_REG) {
                        goto write_r8_to_third_param_on_stack;
                    } else if (uThirdParamKind == EWin64ParamKind::EPARAM_KIND_REF) {
                        uThirdParamFormat = 0x03u;
                        write_r8_to_third_param_on_stack:
                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                make_operand_at_address_in_reg(REG_X64_xSP, i32(pLocalAssign[uIndexOfThird])),
                                                make_operand_reg(REG_X64_R8x),
                                                uThirdParamFormat, pCtx);
                    } else if (uThirdParamKind == EWin64ParamKind::EPARAM_KIND_XMM_REG) {
                        // Write XMM2 to position on stack
                        Assert(false, "write from XMM not yet implemented");
                    }

                    if (uParamCountOnStack > 3u) {
                        // fourth in R9 or XMM3 -> we write it to its position on stack
                        u8 uIndexOfFourth = 3u - procInfo.uRetRequiresFirstParamAsPtr;
                        u8 uFourthParamKind = procInfo.tThisFuncInParams[uIndexOfFourth].uParamKind;
                        u8 uFourthParamFormat = procInfo.tThisFuncInParams[uIndexOfFourth].uFormat;
                        Assert_(0u == (pLocalAssign[uIndexOfFourth] & WIN64_BACKEND_LOCAL_KIND_MASK));
                        if (uFourthParamKind == EWin64ParamKind::EPARAM_KIND_STD_REG) {
                            goto write_r9_to_fourth_param_on_stack;
                        } else if (uFourthParamKind == EWin64ParamKind::EPARAM_KIND_REF) {
                            uFourthParamFormat = 0x03u;
                            write_r9_to_fourth_param_on_stack:
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                    make_operand_at_address_in_reg(REG_X64_xSP, i32(pLocalAssign[uIndexOfFourth])),
                                                    make_operand_reg(REG_X64_R9x),
                                                    uFourthParamFormat, pCtx);
                        } else if (uFourthParamKind == EWin64ParamKind::EPARAM_KIND_XMM_REG) {
                            // Write XMM2 to position on stack
                            Assert(false, "write from XMM not yet implemented");
                        }

                        // Note: beyond fourth should already be on stack (and marked as being there) => NOOP atm
                    }
                }
            }
        }
    }

    u32 uErrCheckCount = pCtx->pProcResult->vecErrChecks.size();
    u32 uPosOfErrCheckTableInConstSection;
    if (uErrCheckCount) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Emitting err-check-info table in const section for this function"), pCtx->pWorker);
        u32 uValuesCount = 8u * uErrCheckCount;
        u32* tValues = (u32*)alloc_from(pCtx->secondaryTmpArena, uValuesCount*sizeof(u32), alignof(u32));
        for(u32 uErrCheck = 0; uErrCheck < uErrCheckCount; uErrCheck++) {
            LocalErrCheck& errCheck = pCtx->pProcResult->vecErrChecks[uErrCheck];
            Assert_(errCheck.iSourceFile >= 0 && u32(errCheck.iSourceFile) < pCtx->pProgCompilationState->vecSourceFiles.size());
            u32* pTheseValues = tValues + (uErrCheck * 7u);
            pTheseValues[0u] = u32(errCheck.uFlagsAndKind & 0x00FFu) | (u32(errCheck.iSourceFile) << 8u);
            pTheseValues[1u] = errCheck.uBlockIndex;
            pTheseValues[2u] = errCheck.uStatement;
            pTheseValues[3u] = errCheck.uTokenRef;
            pTheseValues[4u] = u32(pCtx->pProcResult->iSourceFileIndex);
            pTheseValues[5u] = pCtx->pProcResult->uRegistrationIndex;
            pTheseValues[6u] = errCheck.uPosOfCheck;
            pTheseValues[7u] = errCheck.uPosOfInstr;
        }
        pad_file_section_to_align(pCtx->pSections + EPESection::PE_SECTION_CONST, 4u);
        uPosOfErrCheckTableInConstSection = pCtx->pSections[EPESection::PE_SECTION_CONST].uSize;
        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CONST, (const u8*)tValues, uValuesCount*sizeof(u32));
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
        "Emitting actual code for this function"), pCtx->pWorker);

    WinX64ProcInfo calledProcInfo;

    u32 uProcParamsCount = procSign->params.size();
    for (u32 uInstrIndex = uProcParamsCount; uInstrIndex < uInstructionsCount; uInstrIndex++) {
        IREntry& entry = ir_access_repo_instr(pCtx->pRepo, uInstrIndex);
        u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
        u8 uStdFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16);
        u64 uStdFirstParam = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
        u64 uStdSecondParam = entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_MASK;
        switch (uIRIT) {

            case IRIT_MARKER_JUMP_TARGET: {
                pRepoInfo[uInstrIndex] = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
            } break;

            case IRIT_MARKER_START_SOURCE_SCOPE:
            case IRIT_MARKER_END_SOURCE_SCOPE:
            case IRIT_LOCAL_VAR_DECL:
            case IRIT_NO_OP:
            {
                // NOOP
            } break;

            case IRIT_GOTO: {
                u32 uPosInIRofMarker = u32(uStdSecondParam >> IR_STD_PARAM_SHIFT);
                winx64_emit_local_jump(EX64JUMPCODE_RELATIVE_JMP, uPosInIRofMarker, vecLocalJumps, pCtx);
            } break;

            case IRIT_BRANCH: {
                u32 uPosInIRofMarker = u32(uStdSecondParam >> IR_STD_PARAM_SHIFT);
                EX64JumpCodeRelative uOpCode = winx64_handle_compare_and_get_relative_jmp_opcode_testing(uStdFirstParam, uStdFormat,
                    entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_BRANCH_ON_NONZERO, &procInfo, pLocalAssign, vecLocalConstsToEmit, pCtx);
                if (uOpCode != EX64JUMPCODE_NONE)
                    winx64_emit_local_jump(uOpCode, uPosInIRofMarker, vecLocalJumps, pCtx);
            } break;

            case IRIT_ERRCHK: {
                if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_ERRCHK_IS_ACTIVE) {
                    EX64JumpCodeRelative uOpCode = winx64_handle_compare_and_get_relative_jmp_opcode_testing(uStdFirstParam, uStdFormat,
                        entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_BRANCH_ON_NONZERO, &procInfo, pLocalAssign, vecLocalConstsToEmit, pCtx);
                    if (uOpCode != EX64JUMPCODE_NONE) {
                        u32 uIndexOfLocalErrCheck = u32(uStdSecondParam >> IR_STD_PARAM_SHIFT);
                        Assert_(uIndexOfLocalErrCheck < pCtx->pProcResult->vecErrChecks.size());
                        u32 uByteOffsetToErrInfoTableForThis = uIndexOfLocalErrCheck * 8u * 4u; // 8x 32b entries per local errcheck index
                        if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_ERRCHK_IS_ACTIVE) {
                            Assert_(entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_BRANCH_ON_NONZERO);
                            // Moving byte offset to local err check to EBX (effectively RBX since ensured positive, and zero extended)
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                    make_operand_reg(REG_X64_xBX),
                                                    make_operand_immediate(i64(i32(uByteOffsetToErrInfoTableForThis))),
                                                    0x02u, pCtx);
                            winx64_emit_local_jump_to_err(uOpCode, vecJumpsToErrCheck, pCtx);
                        }
                    }
                }
            } break;

            case IRIT_STORE: {
                u64 uDestIR = uStdFirstParam;
                u64 uSrcIR = uStdSecondParam;
                u8 uFormat = uStdFormat;
                // TODO
                Assert(uFormat <= 0x03u, "store instruction : Non-standard integral register formats not yet implemented");
                winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                    winx64_get_x64_operand_from_ir_param(uDestIR, &procInfo, pLocalAssign, vecLocalConstsToEmit,
                        EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_R8x, REG_X64_xAX, pCtx),
                    winx64_get_x64_operand_from_ir_param(uSrcIR, &procInfo, pLocalAssign, vecLocalConstsToEmit,
                        get_imm_integrals_from_format(uFormat), REG_X64_R9x, REG_X64_xCX, pCtx),
                    uFormat, pCtx);
            } break;

            case IRIT_STORE_EXT: {
                u64 uDestIR = uStdFirstParam;
                u32 uSlotsCount = u32(uStdSecondParam >> IR_STD_PARAM_SHIFT);
                u32 uAlignLog2 = u32(uStdSecondParam >> (IR_STD_PARAM_SHIFT+32));
                Assert_(uSlotsCount);
                u8 uFormat = uStdFormat;

                IREntry& nextEntry = ir_access_repo_instr(pCtx->pRepo, uInstrIndex+1u);
                Assert_(u8(nextEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_CALLER_IN_PARAM);
                Assert_(u8(nextEntry.uInstrCodeAndFormatAndFirstParam >> 16) == uFormat);
                Assert_(u32(nextEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT) == uSlotsCount);
                u32 uAlignLog2Src = u32(nextEntry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
                u64 uSrcIR = nextEntry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;

                winx64_emit_memcpy_by_ir(uFormat, uDestIR, uSrcIR, make_operand_immediate(i64(uSlotsCount)), uAlignLog2, uAlignLog2Src,
                    &procInfo, pLocalAssign, vecLocalConstsToEmit, pCtx);

                uInstrIndex += 1u;
            } break;

            case IRIT_SETZERO: {
                u32 uSlotsCount = u32(uStdSecondParam >> IR_STD_PARAM_SHIFT);
                if (uSlotsCount) {
                    u64 uDestIR = uStdFirstParam;
                    Assert_(!ir_is_immediate(uDestIR));
                    u8 uFormat = uStdFormat;
                    if (uSlotsCount == 1u) {
                        // TODO
                        Assert(uFormat <= 0x03u, "set zero instruction : Non-standard integral register formats not yet implemented");

                        // TODO: use XOR instead to reset to 0 if dest operand is reg ?
                        winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                            winx64_get_x64_operand_from_ir_param(uDestIR, &procInfo, pLocalAssign, vecLocalConstsToEmit,
                                EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_R8x, REG_X64_xAX, pCtx),
                            make_operand_immediate(0), uFormat, pCtx);
                    } else {
                        u32 uAlignLog2 = get_log2_of_natural_align_from_format(uFormat);
                        winx64_emit_setzero_by_ir_optimizable(uFormat, uDestIR, ir_make_info_for_int_immediate(i32(uSlotsCount), 0x02u),
                            uAlignLog2, currentProcParams, pCtx);
                    }
                } // otherwise NOOP; CLEANUP: is store with slotcount 0 a valid instruction anyway ?

            } break;

            case IRIT_RET: {
                // Emitting outtro:
                if (procInfo.uOutParamsCount) {
                    Assert_(procInfo.uOutParamsCount == 1u);
                    u8 uIndexOfRetParam = procInfo.uInParamsCount;
                    u8 uRetFormat;
                    Assert_(0u == (pLocalAssign[uIndexOfRetParam] & WIN64_BACKEND_LOCAL_KIND_MASK));
                    if (procInfo.tThisFuncOutParams[0u].uSlotsCount) {
                        if (procInfo.tThisFuncOutParams[0u].uParamKind == EPARAM_KIND_STD_REG) { // moving ret param to RAX
                            uRetFormat = procInfo.tThisFuncOutParams[0u].uFormat; // TODO: 'format' there could be less than required but with slot counts > 1u => requires distinct 'actual format in backend' somewhere.
                            goto move_ret_to_rax;
                        } else if (procInfo.tThisFuncOutParams[0u].uParamKind == EPARAM_KIND_REF) { // by-convention moving address of ret param to RAX also
                            uRetFormat = 0x03u;
                            move_ret_to_rax:
                            if (procInfo.tThisFuncOutParams[0u].uSlotsCount == 1u) {
                                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                        make_operand_reg(REG_X64_xAX),
                                                        make_operand_at_address_in_reg(REG_X64_xSP, i32(pLocalAssign[uIndexOfRetParam])),
                                                        uRetFormat, pCtx);
                            } else {
                                u32 uByteSize = procInfo.tThisFuncOutParams[0u].uSlotsCount * (1u << get_log2_of_slot_size_from_format(procInfo.tThisFuncOutParams[0u].uFormat));
                                i32 iOffsetOfBaseFromRSP = i32(pLocalAssign[uIndexOfRetParam]);
                                constexpr u8 tShrEAXBy16[3] = { 0xC1u, 0xE8u, 0x10u };
                                constexpr u8 tShrRAXBy32[4] = { 0x48u, 0xC1u, 0xE8u, 0x20u };
                                switch (uByteSize) {
                                    case 2u: { // move 16b in one go with format 0x01
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 16b from AX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP),
                                                                0x01u, pCtx);
                                    } break;

                                    case 3u: { // move 16b with format 0x01, then shift and move remaining 8b with format 0x00
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 16b from AX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP),
                                                                0x02u, pCtx);
                                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tShrEAXBy16, sizeof(tShrEAXBy16));
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving other 8b from AL to base + 2
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP + 2),
                                                                0x00u, pCtx);
                                    } break;

                                    case 4u: { // move 32b in one go with format 0x02
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 32b from EAX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP),
                                                                0x02u, pCtx);
                                    } break;

                                    case 5u: { // move 32b with format 0x02, then shift and move remaining 8b with format 0x00
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 32b from EAX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP),
                                                                0x02u, pCtx);
                                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tShrRAXBy32, sizeof(tShrRAXBy32));
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving other 8b from AL to base + 4
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP + 4),
                                                                0x00u, pCtx);
                                    } break;

                                    case 6u: { // move 32b with format 0x02, then shift and move remaining 16b with format 0x01
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 32b from EAX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP),
                                                                0x02u, pCtx);
                                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tShrRAXBy32, sizeof(tShrRAXBy32));
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 16b from AX to base + 4
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP + 4),
                                                                0x01u, pCtx);
                                    } break;

                                    case 7u: { // move 32b with format 0x02, then shift and move 16b with format 0x01, then final shift and move remaining 8b with format 0x00
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 32b from EAX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, i32(pLocalAssign[uIndexOfRetParam])),
                                                                0x02u, pCtx);
                                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tShrRAXBy32, sizeof(tShrRAXBy32));
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving other 16b from AX to base + 4
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP + 4),
                                                                0x01u, pCtx);
                                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tShrEAXBy16, sizeof(tShrEAXBy16));
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving other 8b from AL to base + 6
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP + 6),
                                                                0x00u, pCtx);
                                    } break;

                                    case 8u: { // move 64b in one go with format 0x03
                                        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, // Moving 64b from RAX to base
                                                                make_operand_reg(REG_X64_xAX),
                                                                make_operand_at_address_in_reg(REG_X64_xSP, iOffsetOfBaseFromRSP),
                                                                0x03u, pCtx);
                                    } break;

                                    default:
                                        Assert_(false);
                                }
                            }
                        } else if (procInfo.tThisFuncOutParams[0u].uParamKind == EPARAM_KIND_XMM_REG) {
                            // moving ret param to XMM0: TODO
                            Assert(false, "move to XMM not yet implemented");
                        }
                    } else {
                        // TODO
                        Assert(false, "void slot for not yet correctly handled for current proc retparam");
                    }
                }
                if (procInfo.uLocalStackSize) {
                    // decreasing stack size (increasing stack ptr) to where it was before our prolog (ie will point to ret address)
                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_ADD,
                                            make_operand_reg(REG_X64_xSP),
                                            make_operand_immediate(i64(u64(procInfo.uLocalStackSize))),
                                            0x03u, pCtx);
                }
                emit_ret(pCtx->pSections + EPESection::PE_SECTION_CODE);
            } break;

            case IRIT_CALL: {
                calledProcInfo = {};
                calledProcInfo.uCallingConv = init_func_param_info_from_proc_call(entry, pCtx->pRepo, uInstrIndex,
                    calledProcInfo.tThisFuncInParams, &calledProcInfo.uInParamsCount, calledProcInfo.tThisFuncOutParams, &calledProcInfo.uOutParamsCount);
                winx64_eval_proc_param_kinds(&calledProcInfo);
                u32 uCountSkippedEntries = 0u;
                for (u8 uInParam = 0u; uInParam < calledProcInfo.uInParamsCount; uInParam++) {
                    Assert(calledProcInfo.tThisFuncInParams[uInParam].uSlotsCount, "void-type params not yet correctly handled"); // TODO
                    uCountSkippedEntries++;
                    IREntry& inParamEntry = ir_access_repo_instr(pCtx->pRepo, uInstrIndex+uCountSkippedEntries);
                    Assert_(u8(inParamEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_CALLER_IN_PARAM);
                    u8 uParamFormat = u8(inParamEntry.uInstrCodeAndFormatAndFirstParam >> 16u);
                    u32 uSlotsCount = u32(inParamEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    Assert_(calledProcInfo.tThisFuncInParams[uInParam].uSlotsCount == uSlotsCount);
                    u32 uAlignLog2 = u32(inParamEntry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
                    Assert_(calledProcInfo.tThisFuncInParams[uInParam].uFormat == uParamFormat);
                    Assert_(calledProcInfo.tThisFuncInParams[uInParam].uLog2OfAlign == uAlignLog2);
                    Assert(uAlignLog2 <= 4u, "align larger than 16-bytes for params not yet implemented");
                    X64OperandRef paramRef = winx64_get_x64_operand_from_ir_param(
                        inParamEntry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK,
                        &procInfo, pLocalAssign, vecLocalConstsToEmit, get_imm_integrals_from_format(uParamFormat),
                        REG_X64_xAX, eRegCategory(0u), pCtx);
                    u8 uParamIndex = uInParam + calledProcInfo.uRetRequiresFirstParamAsPtr;
                    switch (calledProcInfo.tThisFuncInParams[uInParam].uParamKind) {
                        case EWin64ParamKind::EPARAM_KIND_STD_REG: {
                            if (uSlotsCount == 1u) {
                                if (uParamIndex < 4u) {
                                    X64OperandRef regDest;
                                    switch (uParamIndex) {
                                        case 0: {
                                            regDest = make_operand_reg(REG_X64_xCX);
                                        } break;
                                        case 1: {
                                            regDest = make_operand_reg(REG_X64_xDX);
                                        } break;
                                        case 2: {
                                            regDest = make_operand_reg(REG_X64_R8x);
                                        } break;
                                        case 3: {
                                            regDest = make_operand_reg(REG_X64_R9x);
                                        } break;
                                        default:
                                            Assume_(false);
                                    }
                                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, regDest, paramRef, uParamFormat, pCtx);
                                } else {
                                    u32 uFromSP = u32(uParamIndex) * 8u;
                                    X64OperandRef stackDest = make_operand_at_address_in_reg_indexed(REG_X64_xSP, 0u, 0u, i32(uFromSP));
                                    winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, stackDest, paramRef, uParamFormat, pCtx);
                                }
                            } else {
                                // TODO
                                Assert(false, "multi-slot to single std reg param not yet implemented");
                            }
                        } break;

                        case EWin64ParamKind::EPARAM_KIND_XMM_REG: {
                            // TODO
                            Assert(false, "XMM params not yet implemented");
                        } break;

                        case EWin64ParamKind::EPARAM_KIND_REF: {
                            // TODO: CLEANUP: CHECK:
                            // 1) Atm, we do *not* follow the winx64 rule that params by ref should be 16-bytes aligned. And we do
                            //    *not* make a local copy. We indeed only pass a ref without a copy: very fast, and can ref anything
                            //    (including consts, etc)... However, C functions have the right to modify such param values => they may
                            //    require a copy (LOC functions, to the contrary, do not... unless in case of volatile maybe).
                            //    => either this is always ok, or we need to fix it here, and/or we need to warn users that any such struct
                            //       must be "user-declared" as 16-bytes aligned... and/or we need to provide a way to flag for forced copy...
                            // 2) We'll probably need to ensure in the future, at least in some cases, that such a copy is actually performed,
                            //    when the address is taken to something declared as *volatile* : so that semantics of a param by value is
                            //    preserved.
                            if (uParamIndex < 4u) {
                                X64OperandRef regDest;
                                switch (uParamIndex) {
                                    case 0: {
                                        regDest = make_operand_reg(REG_X64_xCX);
                                    } break;
                                    case 1: {
                                        regDest = make_operand_reg(REG_X64_xDX);
                                    } break;
                                    case 2: {
                                        regDest = make_operand_reg(REG_X64_R8x);
                                    } break;
                                    case 3: {
                                        regDest = make_operand_reg(REG_X64_R9x);
                                    } break;
                                    default:
                                        Assume_(false);
                                }
                                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, regDest, paramRef, uParamFormat, pCtx);
                            } else {
                                u32 uFromSP = u32(uParamIndex) * 8u;
                                X64OperandRef stackDest = make_operand_at_address_in_reg_indexed(REG_X64_xSP, 0u, 0u, i32(uFromSP));
                                winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_LEA, stackDest, paramRef, uParamFormat, pCtx);
                            }
                        } break;

                        default:
                            Assert_(false);
                    }
                }
                if (calledProcInfo.uRetRequiresFirstParamAsPtr) {
                    // TODO : pass address to retparam in RCX
                    Assert(false, "ptr-based retparam not yet implemented");
                }

                u64 uIRofCalledProc = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
                SourceFileDescAndState* pCalledProcFile;
                u32 uCalledProcIndex;
                u64 uProcAsIR;
                if (try_access_known_proc(uIRofCalledProc, &pCalledProcFile, &uCalledProcIndex, &uProcAsIR, pCtx)) {
                    Assert_(pCalledProcFile);
                    Assert_(ir_is_valid_param(uProcAsIR));
                    u32 uRepoInfo = winx64_backend_on_access_proc(pCalledProcFile, uCalledProcIndex, uProcAsIR, pCtx);
                    constexpr u8 uCallOpRelative = 0xE8u;
                    write_to_file_section(pCtx->pSections + PE_SECTION_CODE, &uCallOpRelative, 1u);
                    u32 uPosOfRelativeOffset = pCtx->pSections[PE_SECTION_CODE].uSize;
                    u32 uIPafterCall = uPosOfRelativeOffset + 4u;
                    if (uRepoInfo > 1u) {
                        Assert_((uRepoInfo >> 29) == EPESection::PE_SECTION_CODE + 1u);
                        u32 uKnownOffsetInCode = uRepoInfo & 0x1FFF'FFFFu;
                        u32 uRelativeOffset = uKnownOffsetInCode - uIPafterCall;
                        write_to_file_section(pCtx->pSections + PE_SECTION_CODE, (const u8*)&uRelativeOffset, 4u);
                    } else {
                        u32 uTmpRelativeOffset = 0u - uIPafterCall;
                        write_to_file_section(pCtx->pSections + PE_SECTION_CODE, (const u8*)&uTmpRelativeOffset, 4u);
                        auto itThisProc = pCtx->mapVecNotYetEmittedByProc.find(uProcAsIR);
                        if (itThisProc == pCtx->mapVecNotYetEmittedByProc.end()) {
                            NotYetEmittedProc newNYEPentry;
                            newNYEPentry.vecOffsetsInCodeToPatchWithProcOffset.init(pCtx->pWorker->tmpArena);
                            newNYEPentry.vecOffsetsInConstsToPatchWithProcAddress.init(pCtx->pWorker->tmpArena);
                            newNYEPentry.vecOffsetsInGlobIniToPatchWithProcAddress.init(pCtx->pWorker->tmpArena);
                            itThisProc = pCtx->mapVecNotYetEmittedByProc.insert(uProcAsIR, newNYEPentry);
                        }
                        itThisProc.value().vecOffsetsInCodeToPatchWithProcOffset.append(uPosOfRelativeOffset);
                    }
                } else {
                    X64OperandRef sourceRef = winx64_get_x64_operand_from_ir_param(uIRofCalledProc, &procInfo, pLocalAssign,
                        vecLocalConstsToEmit, EExpectedX64ImmFormat::EX64_NO_IMM_FORMAT, REG_X64_xBX, REG_X64_xCX, pCtx);
                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_reg(REG_X64_xAX), sourceRef, 0x03u, pCtx);
                    constexpr u8 tCallRAX[] = { 0xFFu, 0xD0u };
                    write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tCallRAX, sizeof(tCallRAX));
                }

                Assert(calledProcInfo.uOutParamsCount <= 1u, "more than 1-retvalue not yet implemented");
                if (calledProcInfo.uOutParamsCount) {
                    constexpr u8 uOutParam = 0u;
                    Assert(calledProcInfo.tThisFuncOutParams[uOutParam].uSlotsCount, "void-type params not yet correctly handled"); // TODO
                    uCountSkippedEntries++;
                    IREntry& outParamEntry = ir_access_repo_instr(pCtx->pRepo, uInstrIndex+uCountSkippedEntries);
                    Assert_(u8(outParamEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_CALLER_RET_PARAM);
                    u8 uParamFormat = u8(outParamEntry.uInstrCodeAndFormatAndFirstParam >> 16u);
                    u32 uSlotsCount = u32(outParamEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    Assert_(calledProcInfo.tThisFuncOutParams[uOutParam].uSlotsCount == uSlotsCount);
                    u32 uAlignLog2 = u32(outParamEntry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
                    Assert_(calledProcInfo.tThisFuncOutParams[uOutParam].uFormat == uParamFormat);
                    Assert_(calledProcInfo.tThisFuncOutParams[uOutParam].uLog2OfAlign == uAlignLog2);
                    Assert(uAlignLog2 <= 4u, "align larger than 16-bytes for params not yet implemented");
                    if (calledProcInfo.uRetRequiresFirstParamAsPtr == 0u) {
                        u32 uLocalAssign = pLocalAssign[uInstrIndex + uCountSkippedEntries];
                        Assert_(uLocalAssign != WIN64_BACKEND_LOCAL_NONE);
                        Assert_(0u == (uLocalAssign & WIN64_BACKEND_LOCAL_KIND_MASK));
                        Assert_(uLocalAssign < procInfo.uLocalStackSize);
                        X64OperandRef retParamOnStack = make_operand_at_address_in_reg_indexed(REG_X64_xSP, 0u, 0u, i32(uLocalAssign));
                        switch (calledProcInfo.tThisFuncOutParams[uOutParam].uParamKind) {
                            case EWin64ParamKind::EPARAM_KIND_STD_REG: {
                                if (uSlotsCount == 1u) {
                                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, retParamOnStack, make_operand_reg(REG_X64_xAX), uParamFormat, pCtx);
                                } else {
                                    u32 uByteSize = uSlotsCount * (1u << get_log2_of_slot_size_from_format(uParamFormat));
                                    Assert_(uByteSize <= 8u);
                                    // well, in fact, we'll just do same... but forcing full RAX to the stack (which should be ok, since all
                                    // params should have exactly 64b reserved on stack for them, even if taking actually less)
                                    // ... unless we know it fits in AX, in which case we emit the shorter 32b version of mov...
                                    winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, retParamOnStack, make_operand_reg(REG_X64_xAX),
                                        uByteSize > 4u ? 0x03u : 0x02u, pCtx);
                                }
                            } break;
                            case EWin64ParamKind::EPARAM_KIND_XMM_REG: {
                                // TODO
                                Assert(false, "XMM params not yet implemented");
                            } break;
                            case EWin64ParamKind::EPARAM_KIND_REF: // should not be a ref, otherwise 'calledProcInfo.uRetRequiresFirstParamAsPtr'
                            default:
                                Assert_(false);
                        }
                    } else {
                        Assert_(calledProcInfo.tThisFuncOutParams[uOutParam].uParamKind == EWin64ParamKind::EPARAM_KIND_REF);
                        // TODO
                        Assert(false, "ret param as ref not yet implemented");
                    }
                }

                uInstrIndex += uCountSkippedEntries;

            } break;

            default: {
                if (!has_irit_a_value(uIRIT)) {
                    Assert(false, "non-value instruction not yet implemented");
                } else if ((entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) &&
                        0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)) {

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Known-value instruction skipped at pos %u", u64(uInstrIndex)), pCtx->pWorker);

                } else if (pLocalAssign[uInstrIndex] != WIN64_BACKEND_LOCAL_NONE) {
                    switch (uIRIT) {

                        // Add or sub (any signedness)
                        case IRIT_ADD: 
                        case IRIT_SUB: 
                        {
                            // TODO (fp, vecs)
                            Assert(uStdFormat <= 0x03u, "ADD or SUB on formats other than integral 8b..64b not yet implemented");
                            u64 uOpA = uStdFirstParam;
                            u64 uOpB = uStdSecondParam;
                            X64OperandRef refOpA = winx64_get_x64_operand_from_ir_param(uOpA, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xDX, pCtx);
                            X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(uOpB, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R9x, REG_X64_xCX, pCtx);
                            X64OperandRef refResult = make_operand_reg(REG_X64_xAX);

                            // TODO: switch the mov if opB is immediate >32b and we add.
                            // TODO: add intermediate if opB is immediate >32b and we sub
                            // TODO: otherwise skip the mov if one of the operands is already a possible reg for ADD.
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                    refResult,
                                                    refOpA, uStdFormat, pCtx);
                            winx64_emit_dest_src_op(uIRIT == IRIT_ADD ? EX64_DEST_SRC_OP_ADD : EX64_DEST_SRC_OP_SUB,
                                                    refResult,
                                                    refOpB, uStdFormat, pCtx);

                            u32 uCheckFlags = entry.uInstrCodeAndFormatAndFirstParam &
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
                                        if (uCheckFlags & IR_INSTRFLAG_POSTOP_CHKUNSIGNED) {
                                            tJumpIfErr[1u] = 0x82;                                // replaces jump if overflow by jump if carry...
                                        }
                                        constexpr u32 uOffsetToOffset = 2u;
                                        u32 uPosOfNextInstruction = uPosOfJumpInstruction + 6u;
                                        u32 uTmpOffset = 0u - uPosOfNextInstruction;
                                        memcpy(tJumpIfErr + uOffsetToOffset, &uTmpOffset, 4u);
                                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tJumpIfErr, sizeof(tJumpIfErr));
                                        vecJumpsToErrCheck.append(uPosOfJumpInstruction + uOffsetToOffset);
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

                            // TODO: mov one to dest (only opdA if sub) then perform add or sub of the other on dest directly, if not both are r/m ?
                            // TODO: replace by 'NEG' instruction if sub from 0
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                    winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign),
                                                    refResult, uStdFormat, pCtx);
                            uInstrIndex += uIncreaseFromCheck;
                        } break;

                        // Signed or unsigned multiply
                        case IRIT_MUL:
                        case IRIT_MUL_U:
                        {
                            // TODO (fp (for signed version only), vecs)
                            Assert(uStdFormat <= 0x03u, "MUL on formats other than integral 8b..64b not yet implemented");
                            u64 uOpA = uStdFirstParam;
                            u64 uOpB = uStdSecondParam;

                            IRInfo infoA; ir_get_info_from_ir_param(uOpA, uStdFormat, pCtx, &infoA);
                            if (irflag_is_known_non_nyka(infoA.uIRandMetaFlags)) {
                                X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(uOpB, &procInfo, pLocalAssign,
                                    vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R9x, REG_X64_xCX, pCtx);
                                uInstrIndex += winx64_emit_mul_related_from_IR_optimizable(uIRIT,
                                    u32(entry.uInstrCodeAndFormatAndFirstParam), uInstrIndex, uStdFormat,
                                    currentProcParams, refOpB, infoA, REG_X64_R8x, REG_X64_xBX, pCtx);
                                break;
                            }
                            X64OperandRef refOpA = winx64_get_x64_operand_from_ir_param(uOpA, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xBX, pCtx);

                            IRInfo infoB; ir_get_info_from_ir_param(uOpB, uStdFormat, pCtx, &infoB);
                            if (irflag_is_known_non_nyka(infoB.uIRandMetaFlags)) {
                                uInstrIndex += winx64_emit_mul_related_from_IR_optimizable(uIRIT,
                                    u32(entry.uInstrCodeAndFormatAndFirstParam), uInstrIndex, uStdFormat,
                                    currentProcParams, refOpA, infoB, REG_X64_R9x, REG_X64_xCX, pCtx);
                                break;
                            }
                            X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(uOpB, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R9x, REG_X64_xCX, pCtx);

                            uInstrIndex += winx64_emit_mul_related_from_IR_naive(uIRIT,
                                u32(entry.uInstrCodeAndFormatAndFirstParam), uInstrIndex, uStdFormat, currentProcParams,
                                refOpA, refOpB, pCtx);

                        } break;

                        case IRIT_QUO:
                        case IRIT_EXACT_QUO:
                        case IRIT_REM:
                        case IRIT_MOD:
                        {
                            // TODO: if immediate operandB:
                            //    if power of 2 : try to replace with shift and masks
                            //    otherwise : still try to replace with a faster formula 
                            
                            // TODO: mod on fp, vecs
                            Assert(uStdFormat <= 0x03u, "Quo/Rem/Mod on formats other than integral 8b..64b not yet implemented");
                            u64 uOpA = uStdFirstParam;
                            u64 uOpB = uStdSecondParam;

                            X64OperandRef refOpA = winx64_get_x64_operand_from_ir_param(uOpA, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xBX, pCtx);

                            IRInfo infoDivisor; ir_get_info_from_ir_param(uOpB, uStdFormat, pCtx, &infoDivisor);
                            if (irflag_is_known_non_nyka(infoDivisor.uIRandMetaFlags)) {
                                uInstrIndex += winx64_emit_div_related_from_IR_optimizable(uIRIT,
                                    u32(entry.uInstrCodeAndFormatAndFirstParam), uInstrIndex, uStdFormat,
                                    currentProcParams, refOpA, infoDivisor, pCtx);
                                break;
                            }
                            X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(uOpB, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R9x, REG_X64_xCX, pCtx);

                            uInstrIndex += winx64_emit_div_related_from_IR_naive(uIRIT,
                                u32(entry.uInstrCodeAndFormatAndFirstParam), uInstrIndex, uStdFormat, currentProcParams,
                                refOpA, refOpB, pCtx);

                        } break;

                        case IRIT_BIT_AND:
                        case IRIT_BIT_OR:
                        case IRIT_BIT_XOR:
                        {
                            // TODO: vecs
                            Assert(uStdFormat <= 0x03u, "Bitwise binary on formats other than integral 8b..64b not yet implemented");
                            u64 uOpA = uStdFirstParam;
                            u64 uOpB = uStdSecondParam;
                            X64OperandRef refOpA = winx64_get_x64_operand_from_ir_param(uOpA, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xBX, pCtx);
                            X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(uOpB, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, EX64_IMM_FORMAT_INT32, REG_X64_R9x, REG_X64_xCX, pCtx);
                            X64OperandRef refResult = make_operand_reg(REG_X64_xAX);
                            EX64DestSrcOp uDestSrcOp = EX64_DEST_SRC_OP_AND;
                            switch (uIRIT) {
                                case IRIT_BIT_AND: uDestSrcOp = EX64_DEST_SRC_OP_AND; break;
                                case IRIT_BIT_OR: uDestSrcOp = EX64_DEST_SRC_OP_OR; break;
                                case IRIT_BIT_XOR: uDestSrcOp = EX64_DEST_SRC_OP_XOR; break;
                                default: Assume_(false);
                            }
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refOpA, uStdFormat, pCtx);
                            winx64_emit_dest_src_op(uDestSrcOp, refResult, refOpB, uStdFormat, pCtx);
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV,
                                                    winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign),
                                                    refResult, uStdFormat, pCtx);
                        } break;

                        case IRIT_BIT_NOT:
                        {
                            // TODO: vecs
                            Assert(uStdFormat <= 0x03u, "NOT on formats other than integral 8b..64b not yet implemented");
                            u64 uOpA = uStdFirstParam;
                            X64OperandRef refOpA = winx64_get_x64_operand_from_ir_param(uOpA, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xBX, pCtx);
                            X64OperandRef refResult = winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign);
                            winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, refResult, refOpA, uStdFormat, pCtx);
                            winx64_emit_singleRM_op(EX64_SINGLERM_OP_NOT, refResult, uStdFormat, pCtx);
                        } break;

                        case IRIT_BIT_LSH:
                        case IRIT_BIT_RSH:
                        {
                            // TODO ?
                            Assert(uStdFormat <= 0x03u, "Shifts on higher bitcount than 64b not yet implemented");
                            u64 uOpA = uStdFirstParam;
                            u64 uOpB = uStdSecondParam;

                            X64OperandRef refOpA = winx64_get_x64_operand_from_ir_param(uOpA, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xBX, pCtx);

                            IRInfo infoShiftAmount;
                            ir_get_info_from_ir_param(uOpB, 0x02u, pCtx, &infoShiftAmount);
                            if (irflag_is_known_non_nyka(infoShiftAmount.uIRandMetaFlags)) {
                                Assert_(irflag_is_known_embd(infoShiftAmount.uIRandMetaFlags));
                                u32 uShiftAmount = u32(infoShiftAmount.metaValue.knownValue.uEmbeddedValue);
                                winx64_emit_shift_by_constant(uIRIT, u32(entry.uInstrCodeAndFormatAndFirstParam), uStdFormat,
                                    currentProcParams, winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign),
                                    refOpA, uShiftAmount, pCtx);
                            } else {
                                X64OperandRef refOpB = winx64_get_x64_operand_from_ir_param(uOpB, &procInfo, pLocalAssign,
                                    vecLocalConstsToEmit, EX64_IMM_FORMAT_INT32, REG_X64_R9x, REG_X64_xDX, pCtx);
                                winx64_emit_shift_by_unknown(uIRIT, u32(entry.uInstrCodeAndFormatAndFirstParam), uStdFormat,
                                    currentProcParams, winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign),
                                    refOpA, refOpB, pCtx);
                            }
                        } break;

                        case IRIT_CMP_EQ:
                        case IRIT_CMP_ORD:
                        {
                            //TODO
                            Assert(false, "non-direct used cmp not yet implemented");
                        } break;

                        case IRIT_PTR_OFFSET:
                        case IRIT_PTR_OFFSET_EXT:
                        {
                            X64OperandRef destFromLocalAssign = winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign);
                            X64OperandRef srcFromInstruction = winx64_get_x64_operand_from_ptr_offset_entry(
                                entry, uInstrIndex, &procInfo, pLocalAssign,
                                vecLocalConstsToEmit, get_imm_integrals_from_format(uStdFormat), REG_X64_R8x, REG_X64_xCX, pCtx);
                            winx64_handle_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, destFromLocalAssign, srcFromInstruction, 0x03u, pCtx);
                        } break;

                        case IRIT_CAST:
                        {
                            X64OperandRef dest = winx64_get_write_operand_from_local_assign(uInstrIndex, pLocalAssign);
                            X64OperandRef srcRef = winx64_get_x64_operand_from_ir_param(uStdFirstParam, &procInfo, pLocalAssign, vecLocalConstsToEmit,
                                EExpectedX64ImmFormat::EX64_IMM_FORMAT_INT32, REG_X64_R8x, REG_X64_xCX, pCtx);
                            u8 uDestFormat = uStdFormat;
                            u8 uSrcFormat = u8(uStdSecondParam >> IR_STD_PARAM_SHIFT);
                            // TODO
                            Assert(uDestFormat <= 0x03u && uSrcFormat <= 0x03u, "casts to/from other than 8b..64b scalar integrals not yet implemented");
                            if (uDestFormat <= 0x07u && uSrcFormat <= 0x07u) {
                                Assert_(uDestFormat > uSrcFormat); // otherwise, between scalar integrals, instruction should be 'reinterp' instead.
                                EIntSemantics eSemantics = (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED) ?
                                    EIntSemantics::EINT_SEMANTIC_UNSIGNED : EIntSemantics::EINT_SEMANTIC_SIGNED;
                                winx64_emit_mov_extend(REG_X64_xAX, uDestFormat, srcRef, uSrcFormat, eSemantics, pCtx);
                                winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, dest, make_operand_reg(REG_X64_xAX), uDestFormat, pCtx);
                            } else {
                                // TODO
                                Assert(false, "casts to/from other than scalar integrals not yet implemented");
                            }
                        } break;

                        default:
                            //TODO
                            Assert(false, "non-known value instruction not yet implemented");
                    }
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Special runtime value instruction with on-the-fly eval skipped at pos %u", u64(uInstrIndex)), pCtx->pWorker);
                }
            } break;
        }
    }

    u32 uJumpsCount = vecLocalJumps.size();
    if (uJumpsCount) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Replacement of jumps to local branches"), pCtx->pWorker);

        for (u32 uJumpToSolve = 0u; uJumpToSolve < uJumpsCount; uJumpToSolve++) {
            LocalJumpInfo jumpInfo = vecLocalJumps[uJumpToSolve];
            Assert_(u8(ir_access_repo_instr(pCtx->pRepo, jumpInfo.uPosInIRofTargetMarker).uInstrCodeAndFormatAndFirstParam) == IRIT_MARKER_JUMP_TARGET);
            u32 uActualPosInCodeOfTarget = pRepoInfo[jumpInfo.uPosInIRofTargetMarker];
            Assert_(uActualPosInCodeOfTarget);
            inc32_over_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, jumpInfo.uOffsetInCodeWithRelJump, uActualPosInCodeOfTarget);
        }
    }

    if (uErrCheckCount) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Emission of err-check handler for this function"), pCtx->pWorker);
        u32 uPosOfErrCheckingCode = pCtx->pSections[EPESection::PE_SECTION_CODE].uSize;
        for (u32 uJumpToRerite = 0u; uJumpToRerite < vecJumpsToErrCheck.size(); uJumpToRerite++) {
            u32 uPosOfJumpInCode = vecJumpsToErrCheck[uJumpToRerite];
            inc32_over_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, uPosOfJumpInCode, uPosOfErrCheckingCode);
        }

        // offset of entry in local err-check table = EBX (positionned before the jump, offset is in bytes => already x7 x4-Bytes)
        // local err check table offset in const section = uPosOfErrCheckTableInConstSection
        u8 tPrepareAndCallErrorHandler[] = {
            0x48, 0x8d, 0x0d, 0x00, 0x00, 0x00, 0x00,   // LEA RCX, [RIP+offset]    # compute the base address to the local err-info table in RCX
            0x48, 0x01, 0xd9,                           // ADD RCX, RBX             # in RCX is now the base address to the relevant error *entry* in this table
            0xE8, 0x00, 0x00, 0x00, 0x00,               // CALL [RIP+offset]        # calls our error-handling function (with RCX as first param)
            0xCC,                                       // INT 3                    # ensure we do not return from that
        };
        constexpr u32 uInstrOffsetToConstTable = 3u;
        u32 uPosOfInstructionAfterLEA = uPosOfErrCheckingCode + 7u;
        constexpr u32 uInstrOffsetToErrHandlingFunc = 11u;
        u32 uPosOfInstructionAfterCALL = uPosOfErrCheckingCode + 15u;
            
        u32 uTmpOffsetToConstTable = uPosOfErrCheckTableInConstSection - uPosOfInstructionAfterLEA;
        memcpy(tPrepareAndCallErrorHandler + uInstrOffsetToConstTable, &uTmpOffsetToConstTable, 4u);
        u32 uOffsetToErrHandlingFunc = pCtx->uPosOfErrHandlingFuncInCodeSection - uPosOfInstructionAfterCALL;
        memcpy(tPrepareAndCallErrorHandler + uInstrOffsetToErrHandlingFunc, &uOffsetToErrHandlingFunc, 4u);

        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_CODE, tPrepareAndCallErrorHandler, sizeof(tPrepareAndCallErrorHandler));
        pCtx->vecOffsetsInCodeToPatchWithConstStart.append(uPosOfErrCheckingCode+uInstrOffsetToConstTable);
    }

    if (vecLocalConstsToEmit.size()) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Emission of required local consts for this function"), pCtx->pWorker);

        while (vecLocalConstsToEmit.size()) {
            u32 uNextToEmit = vecLocalConstsToEmit.pop_last();
            u32 uLocalAssign = pLocalAssign[uNextToEmit];
            Assert_((uLocalAssign & WIN64_BACKEND_LOCAL_KIND_SUBMASK) == WIN64_BACKEND_LOCAL_KIND_CONST_SUBMASK);
            u32 uReservedOffsetInConstSection = uLocalAssign & 0x3FFF'FFFFu;
            IREntry& entry = ir_access_repo_instr(pCtx->pRepo, uNextToEmit);
            u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
            Assert_(has_irit_a_value(uIRIT));
            Assert_(entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN);
            u8 uFormat = get_outvalue_format_from_instruction(uIRIT, entry.uInstrCodeAndFormatAndFirstParam);
            u32 uSlotsCount = 1u;
            if (tIRITSecondParamSlot[uIRIT] == IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN) {
                uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                Assert_(uSlotsCount);
            }
            winx64_backend_rewrite_as_known_data(entry.uInstrMetaFlagsAndSecondParam, entry.metaValue.knownValue, uFormat, uSlotsCount,
                uReservedOffsetInConstSection, false, pLocalAssign, &vecLocalConstsToEmit, pCtx);
        }
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Proc Emission done"), pCtx->pWorker);

    pCtx->pCurrentRepoInfo = 0;
    pCtx->pRepo = 0;
    pCtx->pProcResult = 0;
}

local_func void winx64_backend_on_emit_global_const_declaration(u32 uPosInRepo, WinX64BackendCtx* pCtx)
{
    Assert_(pCtx->pRepo);
    Assert_(pCtx->pCurrentRepoInfo);
    IREntry& entry = ir_access_repo_instr(pCtx->pRepo, uPosInRepo);
    u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
    Assert_(has_irit_a_value(uIRIT));
    Assert_(entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN);
    Assert_(0u == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA));
    u8 uFormat = get_outvalue_format_from_instruction(uIRIT, entry.uInstrCodeAndFormatAndFirstParam);
    u32 uSlotsCount;
    u32 uAlignLog2;
    if (tIRITSecondParamSlot[uIRIT] == IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN) {
        uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
        uAlignLog2 = u32(entry.uInstrMetaFlagsAndSecondParam >> (IR_STD_PARAM_SHIFT+32));
    } else {
        uSlotsCount = 1u;
        uAlignLog2 = get_log2_of_natural_align_from_format(uFormat);
    }

    if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION) {
        bool bEmitRTTI = true; // TODO: param
        if ((entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_CONST_IS_RTTI) && !bEmitRTTI) {
            // TODO
            Assert(false, "RTTI skipping on backend phase not yet properly handled");
        }
    }
    u32 uAlreadyPositionnedInfo = pCtx->pCurrentRepoInfo[uPosInRepo];
    u32 uSection = (uAlreadyPositionnedInfo >> 29) - 1u;
    Assert_(uSection == EPESection::PE_SECTION_CONST); // Otherwise should not have been positionned there...
    u32 uStartOffset = uAlreadyPositionnedInfo & 0x1FFF'FFFFu;
    u32 uAlignMask = (1u << uAlignLog2) - 1u;
    Assert_(0u == (uStartOffset & uAlignMask));
    winx64_backend_rewrite_as_known_data(entry.uInstrMetaFlagsAndSecondParam, entry.metaValue.knownValue, uFormat, uSlotsCount,
        uStartOffset, false, 0, 0, pCtx);
}

local_func void winx64_backend_on_emit_global_var_declaration(u32 uPosInRepo, WinX64BackendCtx* pCtx)
{
    Assert_(pCtx->pRepo);
    Assert_(pCtx->pCurrentRepoInfo);
    u32 uAlreadyPositionnedInfo = pCtx->pCurrentRepoInfo[uPosInRepo];
    u32 uSection = (uAlreadyPositionnedInfo >> 29) - 1u;
    Assert_(uSection == EPESection::PE_SECTION_GLOBVAR_INI); // Otherwise should not have been positionned as a global var requiring initial value emission...
    u32 uStartOffset = uAlreadyPositionnedInfo & 0x1FFF'FFFFu;

    IREntry& entry = ir_access_repo_instr(pCtx->pRepo, uPosInRepo);
    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_GLOBAL_VAR_DECL);
    Assert_(irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam));
    u8 uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16u);
    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);

    u32 uAlignLog2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
    u32 uAlignMask = (1u << uAlignLog2) - 1u;
    Assert_(0u == (uStartOffset & uAlignMask));

    winx64_backend_rewrite_as_known_data(entry.uInstrMetaFlagsAndSecondParam, entry.metaValue.knownValue, uFormat, uSlotsCount,
        uStartOffset, true, 0, 0, pCtx);
}

local_func void winx64_backend_on_emit_proclike_declaration(u32 uPosInRepo, u64 uAsIR, WinX64BackendCtx* pCtx)
{
    Assert_(pCtx->pProcResult);
    Assert_(pCtx->pCurrentRepoInfo);
    Assert_(pCtx->pRepo == &(pCtx->pProcResult->procwiseRepo));
    Assert_(pCtx->pRepo->uIRRepoId == IR_REPO_ID_CURRENT_PROC);
    Assert_(pCtx->pIsolatedSourceFile && pCtx->pIsolatedSourceFile->iRegistrationIndex == pCtx->pProcResult->iSourceFileIndex);
    // Preparing to append proc emission in code section
    u32 uPosOfProc = pCtx->pSections[PE_SECTION_CODE].uSize;
    // Tagging proc declaration with pos of proc emission
    Assert_(0 == (pCtx->pCurrentRepoInfo[uPosInRepo] & 0xE000'0000u));
    pCtx->pCurrentRepoInfo[uPosInRepo] = (u32(PE_SECTION_CODE + 1u) << 29) | uPosOfProc;
    // Actually asking for proc emission
    ArenaRefPoint beforeProcEmission = get_arena_ref_point(pCtx->secondaryTmpArena);
    winx64_backend_on_emit_proc(uAsIR, pCtx);
    reset_arena_no_release_to(beforeProcEmission, pCtx->secondaryTmpArena);
}

constexpr u32 WINX64_KERNEL32_DLL_INDEX = 0u;
enum EWinx64Kernel32ProcIndex : u32 {
    WINX64_EXITPROCESS_INDEX_IN_KERNEL32_DLL = 0u,
    WINX64_GETSTDHANDLE_INDEX_IN_KERNEL32_DLL,
    WINX64_WRITECONSOLEA_INDEX_IN_KERNEL32_DLL,
    WINX64_WRITECONSOLEW_INDEX_IN_KERNEL32_DLL,
    WINX64_SETCONSOLEOUTPUTCP_INDEX_IN_KERNEL32_DLL,
    WINX64_SETCONSOLECP_INDEX_IN_KERNEL32_DLL,
};

local_func void gather_foreign_procs_registered_from_all_sources(TmpStringMap<u32>* ioMapAlreadyRegisteredDLLToImport,
    WinX64BackendCtx* pCtx, Arena tmpArena)
{
    u32 uSourceFilesCount = pCtx->pProgCompilationState->vecSourceFiles.size();
    for (u32 uSourceFileId = 0u; uSourceFileId < uSourceFilesCount; uSourceFileId++) {
        SourceFileDescAndState* pSourceFile = pCtx->pProgCompilationState->vecSourceFiles[uSourceFileId];
        u32 uCountForeignsThere = pSourceFile->vecProcsIndicesDeclaredAsForeign.size();
        for (u32 uForeign = 0u; uForeign < uCountForeignsThere; uForeign++) {
            u32 uProcIndex = pSourceFile->vecProcsIndicesDeclaredAsForeign[uForeign];
            TCProcBodyRegistration* pRegistered = pSourceFile->vecAllProcBodies[uProcIndex];
            Assert_(pRegistered->procResult.uIsForeignSource);
            Assert_(pRegistered->procResult.foreignSymbolName.pStart);
            Assert_(pRegistered->procResult.foreignSymbolName.byte_length());
            u32 uFileIndexForForeignSource = u32(pRegistered->procResult.uIsForeignSource >> 32) - 1u;
            u32 uForeignSourceIndexInFile = u32(pRegistered->procResult.uIsForeignSource);
            const ForeignSourceDecl& decl =
                pCtx->pProgCompilationState->vecSourceFiles[uFileIndexForForeignSource]->vecForeignSources[uForeignSourceIndexInFile];
            if (decl.dynamicLibName.uByteLength) {
                if (decl.staticLibName.uByteLength) {
                    // TODO
                    platform_log_error("winx64_backend() : import library coupled with dynamic DLLs not yet implemented - passing as-if dynamic-only");
                }
                auto itDLL = ioMapAlreadyRegisteredDLLToImport->find(decl.dynamicLibName);
                if (itDLL == ioMapAlreadyRegisteredDLLToImport->end()) {
                    u32 uPosDll = pCtx->vecDLLToImportInfos.size();
                    itDLL = ioMapAlreadyRegisteredDLLToImport->insert(decl.dynamicLibName, uPosDll);
                    DLLToImportInfo* pDllInfo = (DLLToImportInfo*)alloc_from(tmpArena, sizeof(DLLToImportInfo), alignof(DLLToImportInfo));
                    init_dll_to_import_info(pDllInfo, itDLL.key(), tmpArena);
                    pCtx->vecDLLToImportInfos.append(pDllInfo);
                }
                DLLToImportInfo* pDllInfo = pCtx->vecDLLToImportInfos[itDLL.value()];
                auto itProc = pDllInfo->mapAlreadyRegisteredSymbols.find(pRegistered->procResult.foreignSymbolName);
                if (itProc == pDllInfo->mapAlreadyRegisteredSymbols.end()) {
                    u32 uPos = pDllInfo->vecSymbolNames.size();
                    itProc = pDllInfo->mapAlreadyRegisteredSymbols.insert(pRegistered->procResult.foreignSymbolName, uPos);
                    pDllInfo->vecSymbolNames.append(itProc.key());
                }
            } else {
                Assert_(decl.staticLibName.uByteLength);
                // TODO
                Assert(false, "winx64_backend() : static library support not yet implemented - use dynamic foreign sources for your foreign procs");
            }
        }
    }
}

local_func void set_infos_for_foreign_procs_registered_from_all_sources(const TmpStringMap<u32>& mapAlreadyRegisteredDLLToImport,
                                                                        WinX64BackendCtx* pCtx)
{
    u32 uSourceFilesCount = pCtx->pProgCompilationState->vecSourceFiles.size();
    for (u32 uSourceFileId = 0u; uSourceFileId < uSourceFilesCount; uSourceFileId++) {
        SourceFileDescAndState* pSourceFile = pCtx->pProgCompilationState->vecSourceFiles[uSourceFileId];
        Assert_(uSourceFileId == u32(pSourceFile->iRegistrationIndex));
        u32 uCountForeignsThere = pSourceFile->vecProcsIndicesDeclaredAsForeign.size();
        for (u32 uForeign = 0u; uForeign < uCountForeignsThere; uForeign++) {
            u32 uProcIndex = pSourceFile->vecProcsIndicesDeclaredAsForeign[uForeign];
            TCProcBodyRegistration* pRegistered = pSourceFile->vecAllProcBodies[uProcIndex];
            Assert_(pRegistered->procResult.uIsForeignSource);
            Assert_(pRegistered->procResult.foreignSymbolName.pStart);
            Assert_(pRegistered->procResult.foreignSymbolName.byte_length());
            u32 uFileIndexForForeignSource = u32(pRegistered->procResult.uIsForeignSource >> 32) - 1u;
            u32 uForeignSourceIndexInFile = u32(pRegistered->procResult.uIsForeignSource);
            const ForeignSourceDecl& decl =
                pCtx->pProgCompilationState->vecSourceFiles[uFileIndexForForeignSource]->vecForeignSources[uForeignSourceIndexInFile];
            if (decl.dynamicLibName.uByteLength) {
                if (decl.staticLibName.uByteLength) {
                    // TODO
                    platform_log_error("winx64_backend() : import library coupled with dynamic DLLs not yet implemented - passing as-if dynamic-only");
                }
                auto itDLL = mapAlreadyRegisteredDLLToImport.find(decl.dynamicLibName);
                Assert_(itDLL != mapAlreadyRegisteredDLLToImport.end());
                u32 uDllIndex = itDLL.value();
                DLLToImportInfo* pDllInfo = pCtx->vecDLLToImportInfos[uDllIndex];
                auto itProc = pDllInfo->mapAlreadyRegisteredSymbols.find(pRegistered->procResult.foreignSymbolName);
                Assert_(itProc != pDllInfo->mapAlreadyRegisteredSymbols.end());
                u32 uProcIndexInDll = itProc.value();

                u32* pRepoInfoForProcsInFile = pCtx->vecTables32bInfo[3u + uSourceFileId*3u];
                u32 uIndexAsForeign = pDllInfo->uStartIndex + uProcIndexInDll;
                u32 uOffsetInCodeSection = uIndexAsForeign * 6u;
                Assert_(pRepoInfoForProcsInFile[uProcIndex] == 0);
                pRepoInfoForProcsInFile[uProcIndex] = (u32(PE_SECTION_CODE + 1u) << 29) | uOffsetInCodeSection;

            } else {
                Assert_(decl.staticLibName.uByteLength);
                // TODO
                Assert(false, "winx64_backend() : static library support not yet implemented - use dynamic foreign sources for your foreign procs");
            }
        }
    }
}

local_func void winx64_emit_relocs_in_block_for(const TmpArray<u32>& vecToConst, const TmpArray<u32>& vecToGlobIni,
    const TmpArray<u32>& vecToGlobZero, const TmpArray<u32>& vecToProcs, TmpArray<u32>* ioVecRelocBlocksToPatch,
    EPESection eSection, WinX64BackendCtx* pCtx)
{
    u32 uCountRequiredEntries = vecToConst.size() +
                                vecToGlobIni.size() +
                                vecToGlobZero.size() +
                                vecToProcs.size();
    if (uCountRequiredEntries) {
        ArenaRefPoint beforeRelocs = get_arena_ref_point(pCtx->secondaryTmpArena);
        TmpArray<u32> vecAllOffsetsThereContainingAbsoluteAddresses(pCtx->secondaryTmpArena);
        vecAllOffsetsThereContainingAbsoluteAddresses.reserve(uCountRequiredEntries);
        vecAllOffsetsThereContainingAbsoluteAddresses.append_all(vecToConst);
        vecAllOffsetsThereContainingAbsoluteAddresses.append_all(vecToGlobIni);
        vecAllOffsetsThereContainingAbsoluteAddresses.append_all(vecToGlobZero);
        vecAllOffsetsThereContainingAbsoluteAddresses.append_all(vecToProcs);
        Assert_(uCountRequiredEntries == vecAllOffsetsThereContainingAbsoluteAddresses.size());
        std::qsort(vecAllOffsetsThereContainingAbsoluteAddresses.begin(), uCountRequiredEntries, sizeof(u32),
            [](const void* x, const void* y)
            {
                u32 arg1 = *static_cast<const u32*>(x);
                u32 arg2 = *static_cast<const u32*>(y);
                if (arg1 < arg2)
                    return -1;
                else { Assert_(arg1 > arg2); // There should be no two same entries
                    return 1;
                }
            }
        );
        Assert_(vecAllOffsetsThereContainingAbsoluteAddresses[uCountRequiredEntries-1u] <= pCtx->pSections[eSection].uSize-8u);
        Assert_(uCountRequiredEntries == 1u || vecAllOffsetsThereContainingAbsoluteAddresses[0] < vecAllOffsetsThereContainingAbsoluteAddresses[1]);
            
        u32 uCurrentBlockOffset4K = 0xFFFF'FFFFu;
        u32 uCountInCurrentBlock = 0u;
        u32 uPosAtBaseOfCurrentBlock = 0u;
        for (u32 uEntryIndex = 0u; uEntryIndex < uCountRequiredEntries; uEntryIndex++) {
            u32 uEntryOffset = vecAllOffsetsThereContainingAbsoluteAddresses[uEntryIndex];
            u32 uBlockOffsetMask4K = uEntryOffset & 0xFFFF'F000u;
            if (uCurrentBlockOffset4K != uBlockOffsetMask4K) {
                if (uCurrentBlockOffset4K != 0xFFFF'FFFFu) { // not the first block
                    Assert_(uCurrentBlockOffset4K < uBlockOffsetMask4K);
                    // Ensuring of align to a 4-bytes boundary for next block:
                    if (uCountInCurrentBlock & 1u) { // odd number of entries => we need to add one
                        constexpr u8 tSkipEntryCode[2u] = {}; // zeroing an additional 16b entry (in particular 4msb == 0 => flags as to be skipped)
                        write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, tSkipEntryCode, sizeof(tSkipEntryCode));
                        uCountInCurrentBlock++;
                    }
                    u32 uBlockSize = 8u + 2u * uCountInCurrentBlock; // 8 bytes for block header, then 2 bytes per (16b) entry.
                    rewrite_over_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, uPosAtBaseOfCurrentBlock+4u, 
                        (const u8*)(&uBlockSize), 4u); // writing the 32b 'size' at offset 4 in the block header.
                }
                uPosAtBaseOfCurrentBlock = pCtx->pSections[EPESection::PE_SECTION_RELOC].uSize;
                uCurrentBlockOffset4K = uBlockOffsetMask4K;
                uCountInCurrentBlock = 0u;
                Assert_(0u == (uPosAtBaseOfCurrentBlock & 0x0000'0003u)); // all block headers should be aligned on 32b (4 bytes)
                // writing base VA (currently without VA of section itself, will be patched later) for this 4K range.
                write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, (const u8*)(&uBlockOffsetMask4K), sizeof(u32));
                constexpr u8 tBlockHeaderSizeMock[4u] = {}; // preparing the size-entry in block header of 4 bytes
                write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, tBlockHeaderSizeMock, sizeof(tBlockHeaderSizeMock));
                ioVecRelocBlocksToPatch->append(uPosAtBaseOfCurrentBlock);
            }
            u32 uOffsetInBlock = uEntryOffset & 0x0000'0FFFu;
            Assert_(uBlockOffsetMask4K + uOffsetInBlock == uEntryOffset);
            constexpr u32 uCodeFor64bReloc = 10u << 12;  // 'IMAGE_REL_BASED_DIR64', on the 4msb of a 16b entry.
            u32 uEntryValueOnly16lsb = uOffsetInBlock | uCodeFor64bReloc;
            write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, (const u8*)(&uEntryValueOnly16lsb), 2u);
            uCountInCurrentBlock++;
        }
        Assert_(uCountInCurrentBlock);
        if (uCountInCurrentBlock & 1u) { // odd number of entries => we need to add one
            constexpr u8 tSkipEntryCode[2u] = {}; // zeroing an additional 16b entry (in particular 4msb == 0 => flags as to be skipped)
            write_to_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, tSkipEntryCode, sizeof(tSkipEntryCode));
            uCountInCurrentBlock++;
        }
        u32 uLastBlockSize = 8u + 2u * uCountInCurrentBlock; // 8 bytes for block header, then 2 bytes per (16b) entry.
        rewrite_over_file_section(pCtx->pSections + EPESection::PE_SECTION_RELOC, uPosAtBaseOfCurrentBlock+4u, 
            (const u8*)(&uLastBlockSize), 4u); // writing the 32b 'size' at offset 4 in the block header.
            
        reset_arena_no_release_to(beforeRelocs, pCtx->secondaryTmpArena);
    }
}

local_func u32 winx64_backend_emit_all_to(PlatformFileHandle file, u64 uIRofMain, WholeProgramCompilationState* pCompState, WorkerDesc* pWorker, Arena secondaryTmpArena)
{
    Arena tmpArena = pWorker->tmpArena;
    COFF_HEADER coffHeader = {};
    OPTIONAL_HEADER optHeader = {};

    SECTION_HEADER tSectionHeaders[COUNT_PE_SECTIONS] = {};
    FileSection tSections[COUNT_PE_SECTIONS];
    for (u32 uSection = 0u; uSection < COUNT_PE_SECTIONS; uSection++) {
        init_file_section(tSections + uSection, tmpArena);
    }
    constexpr u8 tCodeSectionName[8u] = { u8('.'), u8('t'), u8('e'), u8('x'), u8('t'), 0, 0, 0 };
    constexpr u8 tConstSectionName[8u] = { u8('.'), u8('r'), u8('d'), u8('a'), u8('t'), u8('a'), 0, 0 };
    constexpr u8 tGlobalVarIniSectionName[8u] = { u8('.'), u8('d'), u8('a'), u8('t'), u8('a'), 0, 0, 0 };
    constexpr u8 tGlobalVarZeroSectionName[8u] = { u8('.'), u8('b'), u8('s'), u8('s'), 0, 0, 0, 0 };
    constexpr u8 tRelocSectionName[8u] = { u8('.'), u8('r'), u8('e'), u8('l'), u8('o'), u8('c'), 0, 0 };
    constexpr u8 tImportsSectionName[8u] = { u8('.'), u8('i'), u8('d'), u8('a'), u8('t'), u8('a'), 0, 0 };
    memcpy(tSectionHeaders[PE_SECTION_CODE].Name, tCodeSectionName, 8u);
    memcpy(tSectionHeaders[PE_SECTION_CONST].Name, tConstSectionName, 8u);
    memcpy(tSectionHeaders[PE_SECTION_GLOBVAR_INI].Name, tGlobalVarIniSectionName, 8u);
    memcpy(tSectionHeaders[PE_SECTION_GLOBVAR_ZERO].Name, tGlobalVarZeroSectionName, 8u);
    memcpy(tSectionHeaders[PE_SECTION_RELOC].Name, tRelocSectionName, 8u);
    memcpy(tSectionHeaders[PE_SECTION_IMPORTS].Name, tImportsSectionName, 8u);
    #define IMAGE_SCN_CNT_CODE                  0x00000020u
    #define IMAGE_SCN_CNT_INITIALIZED_DATA      0x00000040u
    #define IMAGE_SCN_CNT_UNINITIALIZED_DATA    0x00000080u
    #define IMAGE_SCN_MEM_SHARED                0x10000000u
    #define IMAGE_SCN_MEM_EXECUTE               0x20000000u
    #define IMAGE_SCN_MEM_READ                  0x40000000u
    #define IMAGE_SCN_MEM_WRITE                 0x80000000u
    tSectionHeaders[PE_SECTION_CODE].Characteristics = IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE;
    tSectionHeaders[PE_SECTION_CONST].Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_SHARED;
    tSectionHeaders[PE_SECTION_GLOBVAR_INI].Characteristics = IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    tSectionHeaders[PE_SECTION_GLOBVAR_ZERO].Characteristics = IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    tSectionHeaders[PE_SECTION_RELOC].Characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    tSectionHeaders[PE_SECTION_IMPORTS].Characteristics = IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
    
    constexpr u32 uActualNumberOfSections = COUNT_PE_SECTIONS;
    u32 uTotalHeaderSize = pe_get_total_header_size(uActualNumberOfSections);

    coffHeader.Machine = 0x8664u;
    coffHeader.NumberOfSections = u16(uActualNumberOfSections);
    coffHeader.NumberOfSymbols = 0u;
    coffHeader.PointerToSymbolTable = 0u;
    coffHeader.SizeOfOptionalHeader = u16(pe_get_optional_header_size());
    #define IMAGE_FILE_EXECUTABLE_IMAGE     0x0002u
    #define IMAGE_FILE_LARGE_ADDRESS_AWARE  0x0020u
    #define IMAGE_FILE_DEBUG_STRIPPED       0x0200u
    coffHeader.Characteristics = IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE | IMAGE_FILE_DEBUG_STRIPPED;
    time_t whatsthetime; time(&whatsthetime);
    coffHeader.TimeDateStamp = u32(whatsthetime);

    optHeader.Magic = 0x020Bu; // PE32+
    optHeader.MajorLinkerVersion = 0u;
    optHeader.MinorLinkerVersion = 100u;
    optHeader.ImageBase = PE_DEFAULT_IMAGE_BASE;
    optHeader.SectionAlignment = PE_SECTION_ALIGNMENT_IN_MEM;
    optHeader.FileAlignment = PE_FILE_ALIGNMENT_ON_DISK;
    optHeader.SizeOfHeaders = align_to(PE_FILE_ALIGNMENT_ON_DISK, uTotalHeaderSize); 
    optHeader.MajorOperatingSystemVersion = 6u; // Windows Vista and above ?
    optHeader.MinorOperatingSystemVersion = 0u;
    optHeader.MajorImageVersion = 1u; // TODO: user-program specific ?
    optHeader.MinorImageVersion = 2u;
    #define IMAGE_SUBSYSTEM_WINDOWS_CUI     3
    #define IMAGE_SUBSYSTEM_WINDOWS_GUI     2
    optHeader.Subsystem = IMAGE_SUBSYSTEM_WINDOWS_CUI; // Console atm
    optHeader.MajorSubsystemVersion = 6u;
    optHeader.MinorSubsystemVersion = 0u;
    optHeader.SizeOfStackReserve = 0x100000u;
    optHeader.SizeOfHeapReserve = 0x100000u;
    optHeader.SizeOfStackCommit = 0x1000u;
    optHeader.SizeOfHeapCommit = 0x1000u;
    #define IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE               0x0040u
    #define IMAGE_DLLCHARACTERISTICS_NO_SEH                     0x0400u
    #define IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE      0x8000u
    optHeader.DllCharacteristics = IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE | IMAGE_DLLCHARACTERISTICS_NO_SEH | IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE;
    optHeader.NumberOfRvaAndSizes = IMAGE_NUMBEROF_DIRECTORY_ENTRIES;

    WinX64BackendCtx ctx;

    ctx.pIsolatedSourceFile = 0;
    ctx.pCompilationParams = pCompState->pCompilationParams;
    ctx.pWorker = pWorker;
    ctx.pProgCompilationState = pCompState;

    ctx.pRepo = 0;
    ctx.pTmpRepo = 0;
    ctx.pProcResult = 0;

    ctx.pSections = tSections;
    ctx.uActualSectionsCount = uActualNumberOfSections;
    ctx._pad0 = 0;
    ctx.secondaryTmpArena = secondaryTmpArena;
    ctx.pCurrentRepoInfo = 0;
    ctx.vecConstDeclarationsToEmit.init(tmpArena);
    ctx.vecVarDeclarationsToEmit.init(tmpArena);
    ctx.vecProcDeclarationsToEmit.init(tmpArena);
    ctx.vecTables32bInfo.init(tmpArena);
    u32 uSourceFilesCount = pCompState->vecSourceFiles.size();
    ctx.vecTables32bInfo.resize(1 + uSourceFilesCount*3);
    {
        u32 uSizeThere = pCompState->programwiseRepo.uSize;
        if (uSizeThere) {
            u32* pData = (u32*)alloc_from(tmpArena, uSizeThere * sizeof(u32), alignof(u32));
            memset(pData, 0u, uSizeThere * sizeof(u32));
            ctx.vecTables32bInfo[0] = pData;
        } else {
            ctx.vecTables32bInfo[0] = 0;
        }
    }
    for (u32 uFileIndex = 0u; uFileIndex < uSourceFilesCount; uFileIndex++) {
        SourceFileDescAndState* pSourceFile = pCompState->vecSourceFiles[uFileIndex];
        {
            u32 uSizeThere = pSourceFile->filewiseConstRepo.uSize;
            if (uSizeThere) {
                u32* pData = (u32*)alloc_from(tmpArena, uSizeThere * sizeof(u32), alignof(u32));
                memset(pData, 0u, uSizeThere * sizeof(u32));
                ctx.vecTables32bInfo[uFileIndex*3 + 1] = pData;
            } else {
                ctx.vecTables32bInfo[uFileIndex*3 + 1] = 0;
            }
        }
        {
            u32 uSizeThere = pSourceFile->filewiseGlobalVarRepo.uSize;
            if (uSizeThere) {
                u32* pData = (u32*)alloc_from(tmpArena, uSizeThere * sizeof(u32), alignof(u32));
                memset(pData, 0u, uSizeThere * sizeof(u32));
                ctx.vecTables32bInfo[uFileIndex*3 + 2] = pData;
            } else {
                ctx.vecTables32bInfo[uFileIndex*3 + 2] = 0;
            }
        }
        {
            u32 uSizeThere = pSourceFile->vecAllProcBodies.size();
            if (uSizeThere) {
                u32* pData = (u32*)alloc_from(tmpArena, uSizeThere * sizeof(u32), alignof(u32));
                memset(pData, 0u, uSizeThere * sizeof(u32));
                ctx.vecTables32bInfo[uFileIndex*3 + 3] = pData;
            } else {
                ctx.vecTables32bInfo[uFileIndex*3 + 3] = 0;
            }
        }
    }

    ctx.vecOffsetsInCodeToPatchWithConstStart.init(tmpArena);
    ctx.vecOffsetsInCodeToPatchWithGlobIniStart.init(tmpArena);
    ctx.vecOffsetsInCodeToPatchWithGlobZeroStart.init(tmpArena);
    ctx.vecOffsetsInCodeToPatchWithImportStart.init(tmpArena);

    ctx.mapVecNotYetEmittedByProc.init(FireAndForgetArenaAlloc(tmpArena));

    ctx.vecOffsetsInConstReferringToAddressOfConst.init(tmpArena);
    ctx.vecOffsetsInConstReferringToAddressOfGlobIni.init(tmpArena);
    ctx.vecOffsetsInConstReferringToAddressOfGlobZero.init(tmpArena);
    ctx.vecOffsetsInConstReferringToAddressOfProc.init(tmpArena);

    ctx.vecOffsetsInGlobIniReferringToAddressOfConst.init(tmpArena);
    ctx.vecOffsetsInGlobIniReferringToAddressOfGlobIni.init(tmpArena);
    ctx.vecOffsetsInGlobIniReferringToAddressOfGlobZero.init(tmpArena);
    ctx.vecOffsetsInGlobIniReferringToAddressOfProc.init(tmpArena);

    ctx.vecOfSalvageableVec32s.init(tmpArena);

    // TODO: build this from actual imports in source
    ctx.vecDLLToImportInfos.init(tmpArena);
    TmpStringMap<u32> mapAlreadyRegisteredDLLToImport;
    mapAlreadyRegisteredDLLToImport.init(FireAndForgetArenaAlloc(tmpArena));

    Assert(WINX64_KERNEL32_DLL_INDEX == ctx.vecDLLToImportInfos.size(), "Mismatch hardcoded index for Kernel32.dll");
    auto itInsertK32 = mapAlreadyRegisteredDLLToImport.insert("KERNEL32.dll", WINX64_KERNEL32_DLL_INDEX);
    DLLToImportInfo* pReqKernel32 = (DLLToImportInfo*)alloc_from(tmpArena, sizeof(DLLToImportInfo), alignof(DLLToImportInfo));
    init_dll_to_import_info(pReqKernel32, itInsertK32.key(), tmpArena);
    ctx.vecDLLToImportInfos.append(pReqKernel32);

    // ExitProcess(INT retcode)
    Assert(WINX64_EXITPROCESS_INDEX_IN_KERNEL32_DLL == pReqKernel32->vecSymbolNames.size(), "Mismatch hardcoded index for ExitProcess in Kernel32.dll");
    auto itInsertExitProcess = pReqKernel32->mapAlreadyRegisteredSymbols.insert("ExitProcess", WINX64_EXITPROCESS_INDEX_IN_KERNEL32_DLL);
    pReqKernel32->vecSymbolNames.append(itInsertExitProcess.key());

    // note: HANDLE GetStdHandle(DWORD):
    //      STD_INPUT_HANDLE ((DWORD)-10)   // The standard input device. Initially, this is the console input buffer, CONIN$.
    //      STD_OUTPUT_HANDLE ((DWORD)-11)  // The standard output device. Initially, this is the active console screen buffer, CONOUT$
    //      STD_ERROR_HANDLE ((DWORD)-12)   // The standard error device. Initially, this is the active console screen buffer, CONOUT$
    Assert(WINX64_GETSTDHANDLE_INDEX_IN_KERNEL32_DLL == pReqKernel32->vecSymbolNames.size(), "Mismatch hardcoded index for GetStdHandle in Kernel32.dll");
    auto itInsertGetStdHandle = pReqKernel32->mapAlreadyRegisteredSymbols.insert("GetStdHandle", WINX64_GETSTDHANDLE_INDEX_IN_KERNEL32_DLL);
    pReqKernel32->vecSymbolNames.append(itInsertGetStdHandle.key());

    // UTF-16 formatted text can be sent to the W family of console APIs.
    // UTF-8 formatted text can be sent to the A family of console APIs after ensuring the code page is first set to 65001 (CP_UTF8)
    //       with the SetConsoleCP and SetConsoleOutputCP functions

    // note: BOOL WriteConsoleA(HANDLE hConsoleOutput, const VOID *lpBuffer, DWORD nNumberOfCharsToWrite, DWORD* lpNumberOfCharsWritten, void* lpReserved)
    // uses current codepage !!! @see SetConsoleCP or SetConsoleOutputCP
    Assert(WINX64_WRITECONSOLEA_INDEX_IN_KERNEL32_DLL == pReqKernel32->vecSymbolNames.size(), "Mismatch hardcoded index for WriteConsoleA in Kernel32.dll");
    auto itInsertWriteConsoleA = pReqKernel32->mapAlreadyRegisteredSymbols.insert("WriteConsoleA", WINX64_WRITECONSOLEA_INDEX_IN_KERNEL32_DLL);
    pReqKernel32->vecSymbolNames.append(itInsertWriteConsoleA.key());

    // note: BOOL WriteConsoleW(HANDLE hConsoleOutput, const VOID *lpBuffer, DWORD nNumberOfCharsToWrite, DWORD* lpNumberOfCharsWritten, void* lpReserved)
    Assert(WINX64_WRITECONSOLEW_INDEX_IN_KERNEL32_DLL == pReqKernel32->vecSymbolNames.size(), "Mismatch hardcoded index for WriteConsoleW in Kernel32.dll");
    auto itInsertWriteConsoleW = pReqKernel32->mapAlreadyRegisteredSymbols.insert("WriteConsoleW", WINX64_WRITECONSOLEW_INDEX_IN_KERNEL32_DLL);
    pReqKernel32->vecSymbolNames.append(itInsertWriteConsoleW.key());

    // note: BOOL WINAPI SetConsoleOutputCP(UINT).
    Assert(WINX64_SETCONSOLEOUTPUTCP_INDEX_IN_KERNEL32_DLL == pReqKernel32->vecSymbolNames.size(), "Mismatch hardcoded index for SetConsoleOutputCP in Kernel32.dll");
    auto itInsertSetConsoleOutputCP = pReqKernel32->mapAlreadyRegisteredSymbols.insert("SetConsoleOutputCP", WINX64_SETCONSOLEOUTPUTCP_INDEX_IN_KERNEL32_DLL);
    pReqKernel32->vecSymbolNames.append(itInsertSetConsoleOutputCP.key());

    // note: BOOL WINAPI SetConsoleCP(UINT).
    Assert(WINX64_SETCONSOLECP_INDEX_IN_KERNEL32_DLL == pReqKernel32->vecSymbolNames.size(), "Mismatch hardcoded index for SetConsoleCP in Kernel32.dll");
    auto itInsertSetConsoleCP = pReqKernel32->mapAlreadyRegisteredSymbols.insert("SetConsoleCP", WINX64_SETCONSOLECP_INDEX_IN_KERNEL32_DLL);
    pReqKernel32->vecSymbolNames.append(itInsertSetConsoleCP.key());

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Gathering all user-declared foreign procs"), pWorker);
        gather_foreign_procs_registered_from_all_sources(&mapAlreadyRegisteredDLLToImport, &ctx, tmpArena);
    }

    ctx.uSizeOfDllHeaders = ctx.vecDLLToImportInfos.size() * sizeof(IMAGE_IMPORT_DESCRIPTOR);
    u32 uSizeOfDllHeadersWithLastZeroed = ctx.uSizeOfDllHeaders + sizeof(IMAGE_IMPORT_DESCRIPTOR);
    u32 uCurrentOffsetInIAT = 0u;
    u32 uCurrentImportIndex = 0u;
    for (u32 uDLL = 0u; uDLL < ctx.vecDLLToImportInfos.size(); uDLL++) {
        DLLToImportInfo* pDllInfo = ctx.vecDLLToImportInfos[uDLL];
        pDllInfo->uStartIndex = uCurrentImportIndex;
        pDllInfo->uStartOffsetInIAT = uCurrentOffsetInIAT;
        u32 uSymbolCount = pDllInfo->vecSymbolNames.size();
        uCurrentImportIndex += uSymbolCount;
        uCurrentOffsetInIAT += (uSymbolCount+1u)* 8u; // +1 for last being zeroed.
    }
    ctx.uSizeOfILTAndIAT = uCurrentOffsetInIAT;
    
    {
        Assert_(uCurrentImportIndex);
        Assert_(tSections[PE_SECTION_CODE].uSize == 0u);
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Emitting stubs for each foreign function"), pWorker);

        // TODO: CHECK: all imports would be functions ??? it is, atm, since we do stubs for funccalls...
        constexpr u8 tStubJumpOpAndModRM[2u] = { 0xFFu, 0x25u }; // Jump near, absolute indirect, to address stored in [RIP+disp32] (4Bytes coming after that).
        for (u32 uDLL = 0u; uDLL < ctx.vecDLLToImportInfos.size(); uDLL++) {
            DLLToImportInfo* pDllInfo = ctx.vecDLLToImportInfos[uDLL];
            u32 uCountEntries = pDllInfo->vecSymbolNames.size();
            u32 uStartOfIAT = uSizeOfDllHeadersWithLastZeroed + pDllInfo->uStartOffsetInIAT;
            for (u32 uEntry = 0u; uEntry < uCountEntries; uEntry++) {
                u32 uIPAtInstructionStart = tSections[PE_SECTION_CODE].uSize;
                u32 uIPAtNextInstruction = uIPAtInstructionStart + 6u;
                u32 uOffsetOfTargetInImports = uStartOfIAT + uEntry * 8u; // Each entry is 8 Bytes
                u32 uOffsetToPosInImports = uOffsetOfTargetInImports - uIPAtNextInstruction;
                write_to_file_section(tSections + PE_SECTION_CODE, tStubJumpOpAndModRM, 2u);
                ctx.vecOffsetsInCodeToPatchWithImportStart.append(uIPAtInstructionStart+2u);
                write_to_file_section(tSections + PE_SECTION_CODE, (const u8*)&uOffsetToPosInImports, 4u);
            }
        }
    }

    Assert_(tSections[PE_SECTION_CODE].uSize == uCurrentImportIndex * 6u); // 6 bytes per stub for imports, being a jump to an address stored in IAT (by 32b-offset-to-IP)

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Assignment of all foreign procs to their stubs"), pWorker);
        set_infos_for_foreign_procs_registered_from_all_sources(mapAlreadyRegisteredDLLToImport, &ctx);
    }

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Emission of the error-handling function"), pWorker);
        
        // Error-handling function code
        ctx.uPosOfErrHandlingFuncInCodeSection = tSections[PE_SECTION_CODE].uSize;
        // In RCX is the address of the entry in a local err table, with 8x 32b infos about that error
        //   =>
        // DWORD[RCX+0] contains err kind (8 lsb), and source file index containing the source code at the origin of the error (24 msb)
        // DWORD[RCX+4] contains block index in source file
        // DWORD[RCX+8] contains statement index in block
        // DWORD[RCX+12] contains token ref in statement
        // DWORD[RCX+16] contains source file index containing procbody with IR at the origin of the error
        // DWORD[RCX+20] contains procbody index in source file
        // DWORD[RCX+24] contains IR position of err-check instruction in procbody
        // DWORD[RCX+28] may contain IR position of instruction at the origin of the error in procbody (or 0)
        //
        // TODO: buff this with error-reports written to output.
        //
        // Currently, we only 'ExitProcess(errKind|sourceFileIndex)'

        // prolog: increase stack size (decrease stack pointer) by 40 => 16-bytes-align with room for 4x 8-bytes-params
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_SUB, make_operand_reg(REG_X64_xSP), make_operand_immediate(40), 0x03u, &ctx);

        u32 uPosOfCallExitProcessCode = tSections[PE_SECTION_CODE].uSize;
        // prepare and call ExitProcess() with value of first entry in table
        u8 tPrepareAndCallExitProcess[] = {
            0x48, 0x89, 0xc8,               // MOV    RAX, RCX              # TODO: could we skip this and mov to ECX a value computed from base address in RCX ???
            0x8b, 0x08,                     // MOV    ECX, DWORD PTR [RAX]  # select first 32b entry from base address of err-info-table, and put that in ECX
            0xE8, 0x00, 0x00, 0x00, 0x00,   // CALL   [RIP+offset]          # calls 'ExitProcess' (with ECX as param)
            0xCC,                           // INT 3                        # ensure we do not return from that
        };
        constexpr u32 uInstrOffsetToErrHandlingFunc = 6u;
        u32 uPosOfInstructionAfterCALL = uPosOfCallExitProcessCode + 10u;
        u32 uIndexOfDll = 0u;
        u32 uFuncEntryInDll = 0u;
        u32 uAbsoluteEntryIndex = ctx.vecDLLToImportInfos[uIndexOfDll]->uStartIndex + uFuncEntryInDll;
        u32 uPosOfImportedFuncStub = uAbsoluteEntryIndex * 6u;
        u32 uOffsetToExitProcessAtPos0 = uPosOfImportedFuncStub - uPosOfInstructionAfterCALL;
        memcpy(tPrepareAndCallExitProcess + uInstrOffsetToErrHandlingFunc, &uOffsetToExitProcessAtPos0, 4u);
        write_to_file_section(tSections + PE_SECTION_CODE, tPrepareAndCallExitProcess, sizeof(tPrepareAndCallExitProcess));
    }

    // ENTRY-point:
    u32 uOffsetOfEntryPointInCode = tSections[PE_SECTION_CODE].uSize;

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Emission of code at entry-point for the process, at offset %u in code", u64(uOffsetOfEntryPointInCode)), pWorker);

        //   prolog, then call user-specified 'main ()' (->eax), then call imported 'ExitProcess(eax)'
    
        // prolog: increase stack size (decrease stack pointer) by 40 => 16-bytes-align with room for 4x 8-bytes-params
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_SUB, make_operand_reg(REG_X64_xSP), make_operand_immediate(40), 0x03u, &ctx);

        constexpr u8 uCallOpRelative = 0xE8u;

        {
            // call 'Main' (with no params atm)
            u32 uIPAtInstructionStart = tSections[PE_SECTION_CODE].uSize;
            u32 uIPAtNextInstruction = uIPAtInstructionStart + 5u;
            u32 uOffsetToPosToPatchLater = 0u - uIPAtNextInstruction;
            write_to_file_section(tSections + PE_SECTION_CODE, &uCallOpRelative, 1u);
            NotYetEmittedProc newNYEPEntryForMain;
            newNYEPEntryForMain.vecOffsetsInCodeToPatchWithProcOffset.init(tmpArena);
            newNYEPEntryForMain.vecOffsetsInConstsToPatchWithProcAddress.init(tmpArena);
            newNYEPEntryForMain.vecOffsetsInGlobIniToPatchWithProcAddress.init(tmpArena);
            newNYEPEntryForMain.vecOffsetsInCodeToPatchWithProcOffset.append(uIPAtInstructionStart + 1u);
            Assert_(ctx.mapVecNotYetEmittedByProc.find(uIRofMain) == ctx.mapVecNotYetEmittedByProc.end());
            ctx.mapVecNotYetEmittedByProc.insert(uIRofMain, newNYEPEntryForMain);
            write_to_file_section(tSections + PE_SECTION_CODE, (const u8*)&uOffsetToPosToPatchLater, 4u);
        }
    
        // mov retvalue of main (EAX) to input param of 'ExitProcess' (ECX)
        winx64_emit_dest_src_op(EX64_DEST_SRC_OP_MOV, make_operand_reg(REG_X64_xCX), make_operand_reg(REG_X64_xAX), 0x02u, &ctx);
    
        { // call 'ExitProcess'
            u32 uIPAtInstructionStart = tSections[PE_SECTION_CODE].uSize;
            u32 uIPAtNextInstruction = uIPAtInstructionStart + 5u;
            u32 uIndexOfDll = 0u;
            u32 uFuncEntryInDll = 0u;
            u32 uAbsoluteEntryIndex = ctx.vecDLLToImportInfos[uIndexOfDll]->uStartIndex + uFuncEntryInDll;
            u32 uPosOfImportedFuncStub = uAbsoluteEntryIndex * 6u;
            u32 uOffsetToPosOfStub = uPosOfImportedFuncStub - uIPAtNextInstruction;
            write_to_file_section(tSections + PE_SECTION_CODE, &uCallOpRelative, 1u);
            write_to_file_section(tSections + PE_SECTION_CODE, (const u8*)&uOffsetToPosOfStub, 4u);
            constexpr u8 x86int3 = 0xCCu;
            write_to_file_section(tSections + PE_SECTION_CODE, &x86int3, 1u); // ensure we do not return from that...
        }
    }

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Main machine-code emission loop"), pWorker);

        u64 uDeclarationToEmit = uIRofMain;
        goto declare_proc_iteration;

        {
            declare_proc_iteration: {
                Assert_(ir_is_valid_param(uDeclarationToEmit));
                Assert_(!ir_is_immediate(uDeclarationToEmit));
                IRRepo* pRepo;
                u32 uIndex;
                SourceFileDescAndState* pSourceFile;
                EEntryKind eKind;
                ir_decode_non_imm(uDeclarationToEmit, &ctx, &pRepo, &uIndex, &pSourceFile, &eKind);
                Assert_(eKind == EEntryKind::EEK_IS_PROCBODY_REF);
                Assert_(pSourceFile);
                ctx.pIsolatedSourceFile = pSourceFile;
                int iFileIndex = pSourceFile->iRegistrationIndex;
                TCProcBodyRegistration* pRegistration = pSourceFile->vecAllProcBodies[uIndex];
                ctx.pProcResult = &(pRegistration->procResult);
                ctx.pRepo = &(pRegistration->procResult.procwiseRepo);
                ctx.pCurrentRepoInfo = ctx.vecTables32bInfo[3 + iFileIndex*3];
                winx64_backend_on_emit_proclike_declaration(uIndex, uDeclarationToEmit, &ctx);
                goto check_next_emission;
            }

            declare_var_iteration: {
                Assert_(ir_is_valid_param(uDeclarationToEmit));
                Assert_(!ir_is_immediate(uDeclarationToEmit));
                IRRepo* pRepo;
                u32 uIndex;
                SourceFileDescAndState* pSourceFile;
                EEntryKind eKind;
                ir_decode_non_imm(uDeclarationToEmit, &ctx, &pRepo, &uIndex, &pSourceFile, &eKind);
                Assert_(eKind == EEntryKind::EEK_FILEWISE_VAR);
                ctx.pIsolatedSourceFile = pSourceFile;
                int iFileIndex = pSourceFile->iRegistrationIndex;
                Assert_(pRepo);
                Assert_(iFileIndex == int(u32(pRepo->uIRRepoId) - IR_REPO_ID_FIRST_FILE));
                ctx.pRepo = pRepo;
                ctx.pCurrentRepoInfo = ctx.vecTables32bInfo[2 + iFileIndex*3];
                winx64_backend_on_emit_global_var_declaration(uIndex, &ctx);
                goto check_next_emission;
            }

            declare_const_iteration: {
                Assert_(ir_is_valid_param(uDeclarationToEmit));
                Assert_(!ir_is_immediate(uDeclarationToEmit));
                IRRepo* pRepo;
                u32 uIndex;
                SourceFileDescAndState* pSourceFile;
                EEntryKind eKind;
                ir_decode_non_imm(uDeclarationToEmit, &ctx, &pRepo, &uIndex, &pSourceFile, &eKind);
                if (eKind == EEntryKind::EEK_PROGRAMWISE_ENTRY) {
                    Assert_(pRepo);
                    ctx.pRepo = pRepo;
                    ctx.pCurrentRepoInfo = ctx.vecTables32bInfo[0];
                } else { Assert_(eKind == EEntryKind::EEK_FILEWISE_CONST);
                    Assert_(pSourceFile);
                    ctx.pIsolatedSourceFile = pSourceFile;
                    int iFileIndex = pSourceFile->iRegistrationIndex;
                    Assert_(pRepo);
                    Assert_(iFileIndex == int(u32(pRepo->uIRRepoId) - IR_REPO_ID_FIRST_FILE));
                    ctx.pRepo = pRepo;
                    ctx.pCurrentRepoInfo = ctx.vecTables32bInfo[1 + iFileIndex*3];
                }
                winx64_backend_on_emit_global_const_declaration(uIndex, &ctx);
                goto check_next_emission;
            }

            check_next_emission : {
                if (ctx.vecConstDeclarationsToEmit.size()) {
                    uDeclarationToEmit = ctx.vecConstDeclarationsToEmit.pop_last();
                    goto declare_const_iteration;
                } else if (ctx.vecVarDeclarationsToEmit.size()) {
                    uDeclarationToEmit = ctx.vecVarDeclarationsToEmit.pop_last();
                    goto declare_var_iteration;
                } else if (ctx.vecProcDeclarationsToEmit.size()) {
                    uDeclarationToEmit = ctx.vecProcDeclarationsToEmit.pop_last();
                    goto declare_proc_iteration;
                } // otherwise, this is the end of IR conversion to data...
            }
        }
    }

    TmpArray<u32> vecRelocBlocksToPatchWithRVAofConsts(ctx.pWorker->tmpArena);
    TmpArray<u32> vecRelocBlocksToPatchWithRVAofGlobIni(ctx.pWorker->tmpArena);

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Gathering required relocations"), pWorker);

        // in-memory alignment of sections must be at least 4K for the following to work, since there must be a distinct
        //   relocation block per entries in a '4K' range.
        Assert_(0u == (optHeader.SectionAlignment & 0x0000'0FFFu));

        winx64_emit_relocs_in_block_for(ctx.vecOffsetsInConstReferringToAddressOfConst,
                                        ctx.vecOffsetsInConstReferringToAddressOfGlobIni,
                                        ctx.vecOffsetsInConstReferringToAddressOfGlobZero,
                                        ctx.vecOffsetsInConstReferringToAddressOfProc,
                                        &vecRelocBlocksToPatchWithRVAofConsts,
                                        EPESection::PE_SECTION_CONST, &ctx);

        winx64_emit_relocs_in_block_for(ctx.vecOffsetsInGlobIniReferringToAddressOfConst,
                                        ctx.vecOffsetsInGlobIniReferringToAddressOfGlobIni,
                                        ctx.vecOffsetsInGlobIniReferringToAddressOfGlobZero,
                                        ctx.vecOffsetsInGlobIniReferringToAddressOfProc,
                                        &vecRelocBlocksToPatchWithRVAofGlobIni,
                                        EPESection::PE_SECTION_GLOBVAR_INI, &ctx);
    }

    ctx.uSizeOfRelocs = tSections[EPESection::PE_SECTION_RELOC].uSize;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Finalizing construction of PE-file"), pWorker);

    const u8 tEnsureNonEmpty[] = { 0xDeu, 0xadu, 0xBeu, 0xefu, };
    if (0u == tSections[PE_SECTION_CONST].uSize)
        write_to_file_section(tSections + PE_SECTION_CONST, tEnsureNonEmpty, sizeof(tEnsureNonEmpty));
    if (0u == tSections[PE_SECTION_GLOBVAR_INI].uSize)
        write_to_file_section(tSections + PE_SECTION_GLOBVAR_INI, tEnsureNonEmpty, sizeof(tEnsureNonEmpty));
    if (0u == tSections[PE_SECTION_GLOBVAR_ZERO].uImageSize)
        tSections[PE_SECTION_GLOBVAR_ZERO].uImageSize += 4u; // also ensure non-empty...
    if (0u == tSections[PE_SECTION_RELOC].uSize)
        write_to_file_section(tSections + PE_SECTION_RELOC, tEnsureNonEmpty, sizeof(tEnsureNonEmpty));

    u32 uCurrentRva = pe_get_rva_at_start_of_first_section(uActualNumberOfSections);
    u32 uCurrentRawPos = pe_get_raw_pos_at_start_of_first_section(uActualNumberOfSections);
    Assert_(uActualNumberOfSections == EPESection::PE_SECTION_IMPORTS + 1u); // 'Imports' should be last section here: we need to add some not-yet-sized data to it. 
    for (u32 uSection = 0u; uSection < EPESection::PE_SECTION_IMPORTS; uSection++) {
        if (0 == (tSectionHeaders[uSection].Characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA)) {
            Assert_(0 != tSections[uSection].uSize);
            tSections[uSection].uImageSize = tSections[uSection].uSize;
            tSectionHeaders[uSection].PointerToRawData = uCurrentRawPos;
        } else { // otherwise 'uImageSize' should have been setup accordingly during init
            Assert_(0 == tSections[uSection].uSize);
            Assert_(0 != tSections[uSection].uImageSize);
            tSectionHeaders[uSection].PointerToRawData = 0;
        }
        u32 uSizeOnDisk = align_to(PE_FILE_ALIGNMENT_ON_DISK, tSections[uSection].uSize);
        u32 uSizeInMem = align_to(PE_SECTION_ALIGNMENT_IN_MEM, tSections[uSection].uImageSize);

        tSectionHeaders[uSection].VirtualAddress = uCurrentRva;
        tSectionHeaders[uSection].SizeOfRawData = uSizeOnDisk;
        tSectionHeaders[uSection].VirtualSize = uSizeInMem;
        uCurrentRawPos += uSizeOnDisk;
        uCurrentRva += uSizeInMem;
    }

    u32 uImportVA = uCurrentRva;
    tSectionHeaders[EPESection::PE_SECTION_IMPORTS].VirtualAddress = uImportVA;
    tSectionHeaders[EPESection::PE_SECTION_IMPORTS].PointerToRawData = uCurrentRawPos;

    optHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress = uImportVA;
    optHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].Size = ctx.uSizeOfDllHeaders; // this count does not contain zeroed last... TODO: check this
    optHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].VirtualAddress = uImportVA + uSizeOfDllHeadersWithLastZeroed;
    optHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IAT].Size = ctx.uSizeOfILTAndIAT;
    optHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress = tSectionHeaders[EPESection::PE_SECTION_RELOC].VirtualAddress;
    optHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size = ctx.uSizeOfRelocs; // this count does not contain zeroed last... TODO: check this

    IMAGE_IMPORT_DESCRIPTOR dllImportDescriptor;
    u32 uOffsetToUnknownSizedInImports = uSizeOfDllHeadersWithLastZeroed + ctx.uSizeOfILTAndIAT * 2u;
    FileSection pseudoSectionForUnknSized;
    init_file_section(&pseudoSectionForUnknSized, tmpArena);
    FileSection pseudoSectionForIATandILT;
    init_file_section(&pseudoSectionForIATandILT, tmpArena);
    constexpr u8 tZeroes[sizeof(IMAGE_IMPORT_DESCRIPTOR)] = {};
    for (u32 uDLL = 0u; uDLL < ctx.vecDLLToImportInfos.size(); uDLL++) {
        DLLToImportInfo* pDLLInfo = ctx.vecDLLToImportInfos[uDLL];
        dllImportDescriptor.FirstThunk = uImportVA + uSizeOfDllHeadersWithLastZeroed + pDLLInfo->uStartOffsetInIAT;    // IAT for all DLLs comes right after import descriptor table
        dllImportDescriptor.OriginalFirstThunk = dllImportDescriptor.FirstThunk + ctx.uSizeOfILTAndIAT; // we'll setup ILT right after IAT
        dllImportDescriptor.ForwarderChain = 0u;
        dllImportDescriptor.TimeDateStamp = 0u; // unbound...
        dllImportDescriptor.Name = uImportVA + uOffsetToUnknownSizedInImports + pseudoSectionForUnknSized.uSize;
        write_to_file_section(tSections + EPESection::PE_SECTION_IMPORTS, (const u8*)&dllImportDescriptor, sizeof(IMAGE_IMPORT_DESCRIPTOR));
        Assert_(pDLLInfo->dllName.can_be_used_as_c_str());
        write_to_file_section(&pseudoSectionForUnknSized, pDLLInfo->dllName.begin(), pDLLInfo->dllName.byte_length()+1u); // +1u for zero-term
        for (u32 uSymbol = 0u; uSymbol < pDLLInfo->vecSymbolNames.size(); uSymbol++) {
            u64 uILTEntry = 0x0000'0000'0000'0000uLL | u64(uImportVA + uOffsetToUnknownSizedInImports + pseudoSectionForUnknSized.uSize);
            write_to_file_section(&pseudoSectionForIATandILT, (const u8*)&uILTEntry, 8u);
            FFString symbolName = pDLLInfo->vecSymbolNames[uSymbol];
            Assert_(symbolName.can_be_used_as_c_str());
            write_to_file_section(&pseudoSectionForUnknSized, tZeroes, 2u); // Hint 0
            write_to_file_section(&pseudoSectionForUnknSized, symbolName.begin(), symbolName.byte_length()+1u); // +1u for zero-term
        }
        write_to_file_section(&pseudoSectionForIATandILT, tZeroes, 8u); // Zero-out a last entry in ILT
    }
    write_to_file_section(tSections + EPESection::PE_SECTION_IMPORTS, tZeroes, sizeof(IMAGE_IMPORT_DESCRIPTOR)); // Zero-out a last descriptor

    Assert_(tSections[PE_SECTION_IMPORTS].uSize); // Should have at least kernel32 for 'ExitProcess'

    u32 uTotalSizeOfImportSection = tSections[EPESection::PE_SECTION_IMPORTS].uSize + pseudoSectionForIATandILT.uSize * 2u + pseudoSectionForUnknSized.uSize;
    u32 uImportSizeOnDisk = align_to(PE_FILE_ALIGNMENT_ON_DISK, uTotalSizeOfImportSection);
    u32 uImportSizeInMem = align_to(PE_SECTION_ALIGNMENT_IN_MEM, uTotalSizeOfImportSection);
    tSectionHeaders[EPESection::PE_SECTION_IMPORTS].SizeOfRawData = uImportSizeOnDisk;
    tSectionHeaders[EPESection::PE_SECTION_IMPORTS].VirtualSize = uImportSizeInMem;

    u32 uTotalImageSize = uCurrentRva + uImportSizeOnDisk;
    u32 uTotalFileSize = uCurrentRawPos + uImportSizeInMem;

    optHeader.SizeOfImage = uTotalImageSize;
    optHeader.AddressOfEntryPoint = tSectionHeaders[PE_SECTION_CODE].VirtualAddress + uOffsetOfEntryPointInCode;

    {
        u32 uCodeVA = tSectionHeaders[EPESection::PE_SECTION_CODE].VirtualAddress;
        u32 uConstVA = tSectionHeaders[EPESection::PE_SECTION_CONST].VirtualAddress;
        u32 uGlobIniVA = tSectionHeaders[EPESection::PE_SECTION_GLOBVAR_INI].VirtualAddress;
        u32 uGlobZeroVA = tSectionHeaders[EPESection::PE_SECTION_GLOBVAR_ZERO].VirtualAddress;

        FileSection* pCodeSection = ctx.pSections + EPESection::PE_SECTION_CODE;
        {
            u32 uCountOffsets = ctx.vecOffsetsInCodeToPatchWithConstStart.size();
            u32 uToAdd = uConstVA - uCodeVA;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u offsets to const with VA difference to const-start (0x%08x) in code section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInCodeToPatchWithConstStart[uIndex];
                inc32_over_file_section(pCodeSection, uOffset, uToAdd);
            }
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInCodeToPatchWithGlobIniStart.size();
            u32 uToAdd = uGlobIniVA - uCodeVA;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u offsets to glob-ini with VA difference to glob-ini-start (0x%08x) in code section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInCodeToPatchWithGlobIniStart[uIndex];
                inc32_over_file_section(pCodeSection, uOffset, uToAdd);
            }
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInCodeToPatchWithGlobZeroStart.size();
            u32 uToAdd = uGlobZeroVA - uCodeVA;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u offsets to glob-zero with VA difference to glob-zero-start (0x%08x) in code section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInCodeToPatchWithGlobZeroStart[uIndex];
                inc32_over_file_section(pCodeSection, uOffset, uToAdd);
            }
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInCodeToPatchWithImportStart.size();
            u32 uToAdd = uImportVA - uCodeVA;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u offsets to imports with VA difference to imports (0x%08x) in code section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInCodeToPatchWithImportStart[uIndex];
                inc32_over_file_section(pCodeSection, uOffset, uToAdd);
            }
        }

        if (ctx.mapVecNotYetEmittedByProc.size()) {
            Assert(false, "there should be no remaining entry in mapVecNotYetEmittedByProc at this time");
        }

        FileSection* pConstSection = ctx.pSections + EPESection::PE_SECTION_CONST;
        {
            u32 uCountOffsets = ctx.vecOffsetsInConstReferringToAddressOfConst.size();
            u64 uToAdd = u64(uConstVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to const with VA of const-start + default image base (0x%08x) in const section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInConstReferringToAddressOfConst[uIndex];
                inc64_over_file_section(pConstSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofConsts.size());
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInConstReferringToAddressOfGlobIni.size();
            u64 uToAdd = u64(uGlobIniVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to glob-ini with VA of glob-ini-start + default image base (0x%08x) in const section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInConstReferringToAddressOfGlobIni[uIndex];
                inc64_over_file_section(pConstSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofConsts.size());
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInConstReferringToAddressOfGlobZero.size();
            u64 uToAdd = u64(uGlobZeroVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to glob-zero with VA of glob-zero-start + default image base (0x%08x) in const section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInConstReferringToAddressOfGlobZero[uIndex];
                inc64_over_file_section(pConstSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofConsts.size());
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInConstReferringToAddressOfProc.size();
            u64 uToAdd = u64(uCodeVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to procs with VA of code-start + default image base (0x%08x) in const section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInConstReferringToAddressOfProc[uIndex];
                inc64_over_file_section(pConstSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofConsts.size());
        }

        FileSection* pGlobIniSection = ctx.pSections + EPESection::PE_SECTION_GLOBVAR_INI;
        {
            u32 uCountOffsets = ctx.vecOffsetsInGlobIniReferringToAddressOfConst.size();
            u64 uToAdd = u64(uConstVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to const with VA of const-start + default image base (0x%08x) in glob-ini section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInConstReferringToAddressOfConst[uIndex];
                inc64_over_file_section(pGlobIniSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofGlobIni.size());
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInGlobIniReferringToAddressOfGlobIni.size();
            u64 uToAdd = u64(uGlobIniVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to glob-ini with VA of glob-ini-start + default image base (0x%08x) in glob-ini section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInConstReferringToAddressOfGlobIni[uIndex];
                inc64_over_file_section(pGlobIniSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofGlobIni.size());
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInGlobIniReferringToAddressOfGlobZero.size();
            u64 uToAdd = u64(uGlobZeroVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to glob-zero with VA of glob-zero-start + default image base (0x%08x) in glob-ini section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInGlobIniReferringToAddressOfGlobZero[uIndex];
                inc64_over_file_section(pGlobIniSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofGlobIni.size());
        }
        {
            u32 uCountOffsets = ctx.vecOffsetsInGlobIniReferringToAddressOfProc.size();
            u64 uToAdd = u64(uCodeVA) + optHeader.ImageBase;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u addresses to procs with VA of code-start + default image base (0x%08x) in glob-ini section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = ctx.vecOffsetsInGlobIniReferringToAddressOfProc[uIndex];
                inc64_over_file_section(pGlobIniSection, uOffset, uToAdd);
            }
            Assert_(uCountOffsets == 0u || vecRelocBlocksToPatchWithRVAofGlobIni.size());
        }
        FileSection* pRelocSection = ctx.pSections + EPESection::PE_SECTION_RELOC;
        {
            u32 uCountOffsets = vecRelocBlocksToPatchWithRVAofConsts.size();
            u32 uToAdd = uConstVA;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u reloc-block-VA with VA of consts-start (0x%04x) in reloc section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = vecRelocBlocksToPatchWithRVAofConsts[uIndex];
                inc32_over_file_section(pRelocSection, uOffset, uToAdd);
            }
        }
        {
            u32 uCountOffsets = vecRelocBlocksToPatchWithRVAofGlobIni.size();
            u32 uToAdd = uGlobIniVA;
            if (uCountOffsets) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Patching %u reloc-block-VA with VA of glob-ini-start (0x%04x) in reloc section:",
                    uCountOffsets, uToAdd), pWorker);
            }

            for (u32 uIndex = 0u; uIndex < uCountOffsets; uIndex++) {
                u32 uOffset = vecRelocBlocksToPatchWithRVAofGlobIni[uIndex];
                inc32_over_file_section(pRelocSection, uOffset, uToAdd);
            }
        }
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Now actually writing to file"), pWorker);

    u32 uWrittenBytes = 0u;

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
            "Writing the various headers"), pWorker);
        uWrittenBytes += pe_write_msdos_header_and_stub(file);
        uWrittenBytes += pe_write_coff_anb_opt_header(file, coffHeader, optHeader);
        uWrittenBytes += pe_write_section_headers(file, tSectionHeaders, uActualNumberOfSections);
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED(
        "Writing all sections"), pWorker);

    for (u32 uSection = 0u; uSection < uActualNumberOfSections; uSection++) {
        uWrittenBytes += write_padding(file, align_to(PE_FILE_ALIGNMENT_ON_DISK, uWrittenBytes) - uWrittenBytes);
        uWrittenBytes += pe_write_file_section(file, tSections + uSection);
    }
    Assert(uActualNumberOfSections == EPESection::PE_SECTION_IMPORTS + 1u, "Imports section should be last written iteratively");
    // => the above prerequisite so that we can append IAT, ILT, and values for names afterwards:
    uWrittenBytes += pe_write_file_section(file, &pseudoSectionForIATandILT); // IAT
    uWrittenBytes += pe_write_file_section(file, &pseudoSectionForIATandILT); // ILT
    uWrittenBytes += pe_write_file_section(file, &pseudoSectionForUnknSized); // DllNames and Hint/Name tables for imports
    uWrittenBytes += write_padding(file, align_to(PE_FILE_ALIGNMENT_ON_DISK, uWrittenBytes) - uWrittenBytes);

    return uWrittenBytes;
}

#endif // LOCLIB_WINX64_BACKEND_H_
