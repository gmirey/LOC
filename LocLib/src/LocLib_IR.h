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

#ifndef LOCLIB_IR_H_
#define LOCLIB_IR_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/arithmetic_operations.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_Token.h"
#include "LocLib_ScanAndTok.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_ProgramState.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_DebugPrint.h"
#include "LocLib_PreParser.h"
#include "LocLib_Postparser.h"
#include "LocLib_IR_Info.h"
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_IR_Types.h"
#include "LocLib_IR_Solver.h"

// Deals with one or several emitted 'IRIT_GOTO' or 'IRIT_BRANCH', once their target IR-position (expected an IRIT_MARKER_JUMP_TARGER) is known
local_func void do_replace_jump_placeholders_to(u32 uInstrIndex, ArrayView<u32>& vecPlaceholders, IRRepo* pRepo, CompilationContext* pEvalContext)
{
    Assert_(uInstrIndex < pRepo->uSize);
    Assert_(u8(ir_access_repo_instr(pRepo, uInstrIndex).uInstrCodeAndFormatAndFirstParam) == IRIT_MARKER_JUMP_TARGET);
    u32 uCount = vecPlaceholders.size();
    for (u32 uIndex = 0; uIndex < uCount; uIndex++) {
        u32 uPos = vecPlaceholders[uIndex];

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Placeholder of GOTO or BRANCH at pos %u will now be linked to jump-target at pos %u",
            u64(uPos), u64(uInstrIndex)), pEvalContext->pWorker);

        Assert_(uPos < pRepo->uSize);
        IREntry& entry = ir_access_repo_instr(pRepo, uPos);
        u8 uInstrType = u8(entry.uInstrCodeAndFormatAndFirstParam);
        Assert_(uInstrType == IRIT_GOTO || uInstrType == IRIT_BRANCH);
        Assert_(0uLL == (entry.uInstrMetaFlagsAndSecondParam & 0x00FF'FFFF'FF00'0000uLL));
        entry.uInstrMetaFlagsAndSecondParam |= u64(uInstrIndex) << IR_STD_PARAM_SHIFT;
    }
}

// could be useful ? TODO: CLEANUP
local_func_inl void ir_erase_entry_with_noop(IRRepo* pRepo, u32 uEntryPos)
{
    IREntry& entryToErase = ir_access_repo_instr(pRepo, uEntryPos);
    entryToErase.uInstrCodeAndFormatAndFirstParam = IRIT_NO_OP;
    entryToErase.uInstrMetaFlagsAndSecondParam = 0uLL;
    entryToErase.metaValue._payload = 0uLL;
}

// generic emit_entry, blindly, with the provided parameters. returns position (where appended) in provided repo
local_func_inl u32 ir_emit_entry(IRRepo* pRepo, IRInstructionType eIRIT, u32 uInstrFlags, u8 uFormat,
    u64 uFirstParam, u64 uSecondParam, u32 uMetaFlags, MetaValueIR metaValue, WorkerDesc* pWorker)
{
    Assert_(0uLL == (uFirstParam & ~IR_STD_PARAM_MASK));
    Assert_(0uLL == (uSecondParam & ~IR_STD_PARAM_MASK));
    Assert_(0u == (uInstrFlags & 0xFFFF'00FFu));
    Assert_(0u == (uMetaFlags & 0xFF00'0000u));
    u32 uPos = pRepo->uSize;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "IR-Emission at pos %u : IRIT_%s", u64(uPos), reinterpret_cast<u64>(tIRITStr[eIRIT])), pWorker);

    IREntry* pNewEntry = ir_append_new_entry(pRepo);
    pNewEntry->uInstrCodeAndFormatAndFirstParam = u64(eIRIT) | u64(uInstrFlags) | (u64(uFormat) << 16) | uFirstParam;
    pNewEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | uSecondParam;
    pNewEntry->metaValue = metaValue;
    return uPos;
}

// a jump-target emission. Jump-targets shall be the only IR on the position of which we're allowed to IRIT_GOTO or IRIT_BRANCH.
local_func u32 ir_emit_marker_jump_target(IRRepo* pRepo, CompilationContext* pEvalContext, u8 uPathColdness = 0u)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Jump-Target emission at pos %u", u64(pRepo->uSize)), pEvalContext->pWorker);
    return ir_emit_entry(pRepo, IRIT_MARKER_JUMP_TARGET, u32(uPathColdness<<8), 0u, 0uLL, 0uLL, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// enums for flagging the *reason* of a jump emission, in the (premature?-)hope that we may use that info during IR optims
enum EBranchKind : u8 {
    BRANCH_TAKEN_UNKNOWN,               // TC did not specify branch-kind.

    BRANCH_TAKEN_FALLTHROUGH,           // explicit fallthrough from a case in a switch
    BRANCH_TAKEN_IF,                    // true path from of an if statement
    BRANCH_TAKEN_ELSE,                  // false path from of an if statement
    BRANCH_TAKEN_BLOCK_END,             // Flling through the end of any block, back to its parent block
    BRANCH_TAKEN_SWITCHCASE,            // to a specific case in a switch
    BRANCH_TAKEN_DEFAULTCASE,           // to the default case in a switch
    BRANCH_TAKEN_INLOOP_BEFORE,         // branching to true-path from a loop iteration guard, when condition precedes body
    BRANCH_TAKEN_TOLOOPCOND_BEFORE,     // branching towards just before loop-condition-test, when condition precedes body
    BRANCH_TAKEN_INLOOP_AFTER,          // branching to true-path from a loop iteration guard, when condition comes after body
    BRANCH_TAKEN_TOLOOPCOND_AFTER,      // branching towards just before loop-condition-test, when condition comes after body
    BRANCH_TAKEN_OUTLOOP,               // branching to false-path from a loop iteration guard
    BRANCH_TAKEN_BREAK,                 // from an explicit break when not better specified otherwise
    BRANCH_TAKEN_CONTINUE,              // from an explicit continue when not better specified otherwise
    BRANCH_TAKEN_TO_DEFAULT_DEFER,      // to a 'default' defer emitted first at defer-block TC, ending that block as-if by fallthrough
    BRANCH_TAKEN_TO_SPECIAL_DEFER,      // to a special defer emitted on an explicit 'break', 'return' or 'continue' which is not followed by same path as a simple fallthrough.

    COUNT_BRANCH_TAKEN
};

    /*
    BRANCH_TAKEN_FALLTHROUGH,
    BRANCH_TAKEN_REGULARBREAK,
    BRANCH_TAKEN_LIKELYBREAK,
    BRANCH_TAKEN_NOMINALBREAK,
    BRANCH_TAKEN_REGULARCONTINUE,
    BRANCH_TAKEN_LIKELYCONTINUE,
    BRANCH_TAKEN_NOMINALCONTINUE,
    BRANCH_TAKEN_UNLIKELYBREAK,
    BRANCH_TAKEN_EDGECASEBREAK,
    BRANCH_TAKEN_UNLIKELYCONTINUE,
    BRANCH_TAKEN_EDGECASECONTINUE,
    BRANCH_TAKEN_REGULARIF,
    BRANCH_TAKEN_LIKELYIF,
    BRANCH_TAKEN_NOMINALIF,
    BRANCH_TAKEN_REGULARELSE,
    BRANCH_TAKEN_LIKELYELSE,
    BRANCH_TAKEN_NOMINALELSE,
    BRANCH_TAKEN_UNLIKELYIF,
    BRANCH_TAKEN_EDGECASEIF,
    BRANCH_TAKEN_UNLIKELYELSE,
    BRANCH_TAKEN_EDGECASEELSE,
    BRANCH_TAKEN_OUT_REGULARLOOP_BEFORE,
    BRANCH_TAKEN_IN_REGULARLOOP_BEFORE,
    BRANCH_TAKEN_OUT_SMALLLOOP_BEFORE,
    BRANCH_TAKEN_IN_SMALLLOOP_BEFORE,
    BRANCH_TAKEN_OUT_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_IN_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_OUT_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_IN_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_IN_EDGELOOP_BEFORE,
    BRANCH_TAKEN_OUT_EDGELOOP_BEFORE,
    BRANCH_TAKEN_OUT_REGULARLOOP_BEFORE,
    BRANCH_TAKEN_IN_REGULARLOOP_BEFORE,
    BRANCH_TAKEN_OUT_SMALLLOOP_BEFORE,
    BRANCH_TAKEN_IN_SMALLLOOP_BEFORE,
    BRANCH_TAKEN_OUT_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_IN_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_OUT_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_IN_UNLIKELYLOOP_BEFORE,
    BRANCH_TAKEN_IN_EDGELOOP_BEFORE,
    BRANCH_TAKEN_OUT_EDGELOOP_BEFORE,
    */

// emission of IRIT_GOTO, when already known where to
local_func u32 ir_emit_goto(u32 uInstrIndex, IRRepo* pRepo, CompilationContext* pEvalContext,
    EBranchKind eBranchKind = EBranchKind::BRANCH_TAKEN_UNKNOWN)
{
    Assert_(uInstrIndex < pRepo->uSize);
    Assert_(u8(ir_access_repo_instr(pRepo, uInstrIndex).uInstrCodeAndFormatAndFirstParam) == IRIT_MARKER_JUMP_TARGET);
    
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "GOTO emission at pos %u, to local IR at %u (branch kind %u)", u64(pRepo->uSize), u64(uInstrIndex), u64(eBranchKind)),
        pEvalContext->pWorker);
    u64 uAsSecondParam = (u64(uInstrIndex) << IR_STD_PARAM_SHIFT) | (u64(eBranchKind) << 56);
    return ir_emit_entry(pRepo, IRIT_GOTO, 0u, 0u, 0uLL, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of IRIT_GOTO, when we don't yet know where to jump exactly. Typically used together with an 'ArrayView<u32> vecPlaceholders'
local_func u32 ir_emit_goto_placeholder(IRRepo* pRepo, CompilationContext* pEvalContext,
    EBranchKind eBranchKind = EBranchKind::BRANCH_TAKEN_UNKNOWN)
{
    u32 uSlot = pRepo->uSize;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "GOTO emission at pos %u (placeholder, branch kind %u)", u64(pRepo->uSize), u64(eBranchKind)),
        pEvalContext->pWorker);
    u64 uAsSecondParam = u64(eBranchKind) << 56;
    return ir_emit_entry(pRepo, IRIT_GOTO, 0u, 0u, 0uLL, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of IRIT_SETZERO
local_func u32 ir_emit_reset_to_zero(u64 uVarToResetToZero, u8 uFormat, u64 uIRofSlotCount,
    IRRepo* pRepo, CompilationContext* pEvalContext)
{
    Assert_(ir_is_valid_param(uVarToResetToZero) && !ir_is_immediate(uVarToResetToZero));
    Assert_(ir_is_valid_param(uIRofSlotCount));
    return ir_emit_entry(pRepo, IRIT_SETZERO, 0u, uFormat, uVarToResetToZero, uIRofSlotCount, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// For debugging purposes...
local_func void _ir_emit_param_repr_to(char* pBuffer, u64 uIRParam, u8 uFormat, u32 uSlotsCount) {
    Assert_(ir_is_valid_param(uIRParam));
    if (ir_is_numeric_immediate(uIRParam)) {
        Assert_(uSlotsCount == 1u);
        if (uFormat <= 0x03u) {
            i32 iEmbd = ir_get_value_from_int_immediate(uIRParam);
            sprintf(pBuffer, "ImmUI.0x%08x", u32(iEmbd));
        } else {
            Assert_((uFormat & 0xF7u) >= 0x01u && (uFormat & 0xF7u) <= 0x03u);
            float fEmbd = ir_get_value_from_float_immediate(uIRParam);
            sprintf(pBuffer, "ImmFP.%f", fEmbd);
        }
    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(uIRParam)) {
        Assert_(uSlotsCount == 1u);
        Assert_(uFormat == 0x03u);
        u64 uAdressedIR = ir_get_param_from_nyka_immediate(uIRParam);
        u32 uPayload32 = u32(uAdressedIR >> IR_STD_PARAM_SHIFT);
        if (0uLL == (uAdressedIR & IR_STD_PARAM_HIGHMASK)) { // programise global IR
            u32 uProgramwisePos = uPayload32;
            sprintf(pBuffer, "NykaG.#%09u", uProgramwisePos);
        } else {
            u16 uRepoIndex = u16(uAdressedIR >> IR_STD_PARAM_REPO_ID_SHIFT);
            u32 uEntityPos = uPayload32 & 0x003F'FFFFu;
            if (uRepoIndex >= IR_REPO_ID_FIRST_FILE) {
                u32 uEntityIdLocation = uPayload32 & 0x00C0'0000u;
                if (0u == uEntityIdLocation) { // filewise global IR entry
                    sprintf(pBuffer, "NykaF%05u.#%08u", uRepoIndex-IR_REPO_ID_FIRST_FILE, uEntityPos);
                } else {
                    if (0x0040'0000u == uEntityIdLocation) { // filewise global var
                        sprintf(pBuffer, "NykaV%05u.#%08u", uRepoIndex-IR_REPO_ID_FIRST_FILE, uEntityPos);
                    } else { Assert_(0x0080'0000u == uEntityIdLocation); // filewise proc body
                        sprintf(pBuffer, "NykaP%05u.#%08u", uRepoIndex-IR_REPO_ID_FIRST_FILE, uEntityPos);
                    }
                }
            } else if (uRepoIndex == IR_REPO_ID_CURRENT_PROC) {
                sprintf(pBuffer, "NykaL.#%08u", uEntityPos);
            } else { Assert_(uRepoIndex == IR_REPO_ID_TEMPORARY);
                sprintf(pBuffer, "NykaT.#%08u", uEntityPos);
            }
        }

    } else {
        u32 uPayload32 = u32(uIRParam >> IR_STD_PARAM_SHIFT);
        if (0uLL == (uIRParam & IR_STD_PARAM_HIGHMASK)) { // programise global IR
            u32 uProgramwisePos = uPayload32;
            sprintf(pBuffer, "Global.#%09u", uProgramwisePos);
        } else {
            u16 uRepoIndex = u16(uIRParam >> IR_STD_PARAM_REPO_ID_SHIFT);
            u32 uEntityPos = uPayload32 & 0x003F'FFFFu;
            if (uRepoIndex >= IR_REPO_ID_FIRST_FILE) {
                u32 uEntityIdLocation = uPayload32 & 0x00C0'0000u;
                if (0u == uEntityIdLocation) { // filewise global IR entry
                    sprintf(pBuffer, "F%05u.#%08u", uRepoIndex-IR_REPO_ID_FIRST_FILE, uEntityPos);
                } else {
                    if (0x0040'0000u == uEntityIdLocation) { // filewise global var
                        sprintf(pBuffer, "V%05u.#%08u", uRepoIndex-IR_REPO_ID_FIRST_FILE, uEntityPos);
                    } else { Assert_(0x0080'0000u == uEntityIdLocation); // filewise proc body
                        sprintf(pBuffer, "P%05u.#%08u", uRepoIndex-IR_REPO_ID_FIRST_FILE, uEntityPos);
                    }
                }
            } else if (uRepoIndex == IR_REPO_ID_CURRENT_PROC) {
                sprintf(pBuffer, "Local.#%08u", uEntityPos);
            } else { Assert_(uRepoIndex == IR_REPO_ID_TEMPORARY);
                sprintf(pBuffer, "Tempo.#%08u", uEntityPos);
            }
        }
    }
}

// emission of an IRIT_BRANCH, when we don't yet know where to jump exactly. Typically used together with an 'ArrayView<u32> vecPlaceholders'
local_func u32 ir_emit_branch_placeholder(u64 uIRParam, u8 uFormat, u32 uWhenNonZeroFlag, IRRepo* pRepo, CompilationContext* pEvalContext,
    EBranchKind eBranchKind = EBranchKind::BRANCH_TAKEN_UNKNOWN)
{
    Assert_(ir_is_valid_param(uIRParam));
    Assert_(uFormat <= 0x07u);
    Assert_(uWhenNonZeroFlag == 0u || uWhenNonZeroFlag == IR_INSTRFLAG_BRANCH_ON_NONZERO);
 
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "BRANCH emission at pos %u (placeholder, branch kind %u) testing 0x%016llx", u64(pRepo->uSize), u64(eBranchKind), uIRParam),
        pEvalContext->pWorker);
    u64 uAsSecondParam = u64(eBranchKind) << 56;
    return ir_emit_entry(pRepo, IRIT_BRANCH, uWhenNonZeroFlag, uFormat, uIRParam, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an IRIT_LOCAL_VAR_DECL
local_func u32 ir_emit_local_variable_decl(u8 uFormat, u32 uAlignPow2, u32 uSlotsCount, u32 uInstrFlags,
                                           IRRepo* pRepo, CompilationContext* pEvalContext)
{
    Assert_(uAlignPow2 <= 12);
    Assert_(uSlotsCount <= MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE);
    u64 uSlotsAndAlign = u64(uSlotsCount) | (u64(uAlignPow2)<<32);
    u64 uAsSecondParam = uSlotsAndAlign << IR_STD_PARAM_SHIFT;
    return ir_emit_entry(pRepo, IRIT_LOCAL_VAR_DECL, 0u, uFormat, 0uLL, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an IRIT_RET
local_func u32 ir_emit_return(IRRepo* pRepo, CompilationContext* pEvalContext)
{
    return ir_emit_entry(pRepo, IRIT_RET, 0u, 0u, 0uLL, 0uLL, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an IRIT_MARKER_START_SOURCE_SCOPE
local_func u32 ir_emit_marker_start_scope(IRRepo* pRepo, CompilationContext* pEvalContext)
{
    return ir_emit_entry(pRepo, IRIT_MARKER_START_SOURCE_SCOPE, 0u, 0u, 0uLL, 0uLL, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an IRIT_MARKER_END_SOURCE_SCOPE
local_func u32 ir_emit_marker_end_scope(u32 uIRPosOfScopeStart, IRRepo* pRepo, CompilationContext* pEvalContext)
{
    Assert_(uIRPosOfScopeStart < pRepo->uSize);
    Assert_(u8(ir_access_repo_instr(pRepo, uIRPosOfScopeStart).uInstrCodeAndFormatAndFirstParam) == IRIT_MARKER_START_SOURCE_SCOPE);
    u64 uAsFirstParam = u64(uIRPosOfScopeStart) << IR_STD_PARAM_SHIFT;
    return ir_emit_entry(pRepo, IRIT_MARKER_END_SOURCE_SCOPE, 0u, 0u, uAsFirstParam, 0uLL, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an IRIT_CALL - Single-'line', no in or out params provided at this point.
local_func u32 ir_emit_proccall(u64 uIRofProc, u8 uInParamsCount, u8 uOutParamsCount, u8 uImplicitTrailingParams,
    u8 uCallConv, u8 bCallIsTail, IRRepo* pRepo, CompilationContext* pEvalContext)
{
    Assert_(ir_is_valid_param(uIRofProc));
    Assert_(u32(uInParamsCount) + u32(uOutParamsCount) < 32u);
    Assert_(uImplicitTrailingParams <= uInParamsCount);
    Assert_(bCallIsTail < 2);
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "CALL emission (convention %u, inparams %u, outparams %u)", u64(uCallConv), u64(uInParamsCount), u64(uOutParamsCount)),
        pEvalContext->pWorker);
    u64 uPackedCounts = u64(uInParamsCount) | (u64(uOutParamsCount) << 8) | (u64(uImplicitTrailingParams) << 16);
    u64 uAsSecondParam = uPackedCounts << IR_STD_PARAM_SHIFT;
    return ir_emit_entry(pRepo, IRIT_CALL, 0u, uCallConv, uIRofProc, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an 'input' param (IRIT_CALLER_IN_PARAM) - typically *after* emission of an associated IRIT_CALL
local_func u32 ir_emit_proc_param_on_caller_side(u64 uIRofParamValue, u8 uFormat, u32 uSlotsCount, u32 uAlignLog2,
    IRRepo* pRepo, CompilationContext* pEvalContext)
{
    Assert_(ir_is_valid_param(uIRofParamValue));
    u64 uSlotsAndAlign = u64(uSlotsCount) | (u64(uAlignLog2)<<32);
    u64 uAsSecondParam = uSlotsAndAlign << IR_STD_PARAM_SHIFT;
    return ir_emit_entry(pRepo, IRIT_CALLER_IN_PARAM, 0u, uFormat, uIRofParamValue, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

// emission of an 'output' param (IRIT_CALLER_RET_PARAM) - typically *after* emission of an associated IRIT_CALL.
// Also used after emission of a few IR codes with possible additional ret values.
local_func u32 ir_emit_proc_result_on_caller_side(u32 uProcCallIRPos, u8 uFormat, u32 uSlotsCount, u32 uAlignLog2,
    IRRepo* pRepo, CompilationContext* pEvalContext)
{
    u32 uSlot = pRepo->uSize;
    Assert_(uProcCallIRPos < uSlot);
    u64 uAsFirstPAram = u64(uProcCallIRPos) << IR_STD_PARAM_SHIFT;
    u64 uSlotsAndAlign = u64(uSlotsCount) | (u64(uAlignLog2)<<32);
    u64 uAsSecondParam = uSlotsAndAlign << IR_STD_PARAM_SHIFT;
    return ir_emit_entry(pRepo, IRIT_CALLER_RET_PARAM, 0u, uFormat, uAsFirstPAram, uAsSecondParam, 0u, MetaValueIR{}, pEvalContext->pWorker);
}

//
// TODO: consistently have an 'emit' func here for *ALL* IRIT ? atm many are directly emitted, eg. in LocLib_IR_SolverInterface.h
//

#endif // LOCLIB_IR_H_

