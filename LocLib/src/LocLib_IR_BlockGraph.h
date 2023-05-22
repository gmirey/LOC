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

#ifndef LOCLIB_IR_BLOCK_GRAPH_H_
#define LOCLIB_IR_BLOCK_GRAPH_H_

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
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_IR_Types.h"
#include "LocLib_IR_Solver.h"
#include "LocLib_IR.h"

// allows graph-builder to actually skip branched based on already-solved flags ?
// TODO: CLEANUP: Is that really necessary ? Wouldn't known branches be directly replaced by gotos at IR level ??
// TODO: THOUGHTS: 'path hotness' flags should be reported to GOTOs in that case...
local_func bool is_branching_decision_known(const IREntry& entry, u32 uInstrIndex, TCProcBodyResult* pProc, TCContext* pContext, bool* outWhenKnownIsTaken)
{
    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_BRANCH || u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_ERRCHK);
    // TODO !!!
    return false;
}

#define IRGRAPH_INVALID_INDEX       0xFFFF'FFFFu
#define IRGRAPH_RET_INDEX           0xFFFF'FFFDu
#define IRGRAPH_ERR_INDEX           0xFFFF'FFFEu

#define IRGRAPH_START_SPECIAL       0xFFFF'FFF0u

#define IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN          0x8000'0000u
#define IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN        0x4000'0000u

#define IRGRAPH_FLAG_REDUCE_EXIT_HAS_ERRCHK         0x2000'0000u

#define IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE        0x0800'0000u

#define IRGRAPH_FLAG_CONTAINS_START_SCOPE           0x0000'0001u
#define IRGRAPH_FLAG_CONTAINS_END_SCOPE             0x0000'0002u

#define IRGRAPH_FLAG_CONTAINS_LOCAL_DECL            0x0000'0010u

#define IRGRAPH_FLAG_CONTAINS_CONST                 0x0000'0100u
#define IRGRAPH_FLAG_CONTAINS_OTHER_SKIPPABLE       0x0000'0200u

#define IRGRAPH_FLAG_CONTAINS_TMP_RUNTIME           0x0000'0800u

#define IRGRAPH_FLAG_CONTAINS_DIRECT_WRITES         0x0000'1000u
#define IRGRAPH_FLAG_CONTAINS_NON_GRAPHED_ERRCHK    0x0000'2000u
#define IRGRAPH_FLAG_CONTAINS_OTHER_INSTRUCTIONS    0x0000'8000u

#define IRGRAPH_FLAG_CONTAINS_OTHER_INSTRUCTIONS    0x0000'8000u


// Raw Graph structure:
// --------------------
// The raw graph is a directed graph, with potential cycles, and output egdes count from a given node enforced in [1..2] (input edges count 0..*)
// Its main representation uses the 'IRBlock' structure representing a node, with edge knowledge being referenced only as
//   the max two out-edges for each node. It is usually presented as a contiguous array of IRBlock`s (in which block refs are indices
//   into that same array), and ordered so as to follow perfectly the original IR declaration order, such that each IR instruction
//   (beyond prolog) is part of exactly one such node ; We can thus have a trivial IRpos to RawNode finder based on only that representation.
// 


// IRBlock is the Node for the 'Raw' Graph + knowledge of the 1 or 2 output edges.
struct IRBlock {
    u32 uInstrStart;                    // IR Instruction index starting that block. If not a fallthrough block, should be a marker jump target. 
    // Note: instruction ending the block is implicitely previous instruction before start of next block, or last IR instruction for last block.
    u32 uNextBlockGotoOrFallthrough;    // Should not be invalid ?
    u32 uNextBlockByBranching;          // invalid unless BRANCH or ERRCHK
    u32 uFlags;                         // see IRGRAPH_FLAG_...
};

local_func_inl void init_ir_block(IRBlock* ioBlock, IRRepo* pRepo, u32 uStartIRindex)
{
    ioBlock->uInstrStart = uStartIRindex;
    ioBlock->uNextBlockGotoOrFallthrough = IRGRAPH_INVALID_INDEX;
    ioBlock->uNextBlockByBranching = IRGRAPH_INVALID_INDEX;
    ioBlock->uFlags = 0u;
}

// Browses some whole procbody IR, building an IR graph of blocks to other blocks,
//   all the while remembering where blocks start and end as IR instructions.
local_func TmpArray<IRBlock> build_IR_graph_for_proc(TCProcBodyResult* pProc, TCContext* pTCContext, Arena blockArena, bool bConsiderErrHandler = false)
{
    Assert_(pProc);
    Assert_(0 == pProc->uIsForeignSource);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Building IR graph for procbody %u (main id '%s') in file %s",
        u64(pProc->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProc->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pProgCompilationState->vecSourceFiles[u32(pProc->iSourceFileIndex)]->sourceFileName.c_str())), pTCContext->pWorker);

    IRRepo* pRepo = &(pProc->procwiseRepo);
    u32 uTotalParams = get_total_param_count(pProc->procSign);
    u32 uInstructionCount = pRepo->uSize;
    u32 uBlockCount = 0u;
    Assert_(uInstructionCount > uTotalParams);

    TmpArray<IRBlock> vecBlocks(blockArena);

    ArenaRefPoint beforeAll = get_arena_ref_point(pTCContext->pWorker->tmpArena);
    TmpArray<u32> vecBlocksRequiringFinalizeEnding(pTCContext->pWorker->tmpArena);
    IRBlock entryBlock;
    init_ir_block(&entryBlock, pRepo, uTotalParams);
    vecBlocks.append(entryBlock);
    u32 uCurrentBlock = 0u;
    u32 uLastIndex = uInstructionCount-1u;
    u8 bLastInstrOpenedAuto = false;
    for (u32 uInstrIndex = uTotalParams+1u; uInstrIndex < uInstructionCount; uInstrIndex++) {
        Assert_(uCurrentBlock == vecBlocks.size() - 1u);
        IREntry& entry = ir_access_repo_instr(pRepo, uInstrIndex);
        u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
        bool bThisInstrOpenedAuto = false;

        switch (uIRIT) {

            case IRIT_MARKER_JUMP_TARGET: {
                // All jump targets force the beginning of a new (raw) block
                if (!bLastInstrOpenedAuto) { // Although that new block may already have been prepared by instruction before
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Jump-target at pos %u, forcing start of new block %u",
                        u64(uInstrIndex), u64(uCurrentBlock+1u)), pTCContext->pWorker);
                    vecBlocks[uCurrentBlock].uNextBlockGotoOrFallthrough = uCurrentBlock+1u; // => points to block we're about to spawn
                    vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN;
                    IRBlock newBlock;
                    init_ir_block(&newBlock, pRepo, uInstrIndex);
                    uCurrentBlock++;
                    vecBlocks.append(newBlock);
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Jump-target at pos %u already positionned at start of brand new block %u => noop",
                        u64(uInstrIndex), u64(uCurrentBlock)), pTCContext->pWorker);
                }
            } break;

            case IRIT_GOTO: {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "GOTO at pos %u forcing the end of current block %u",
                    u64(uInstrIndex), u64(uCurrentBlock)), pTCContext->pWorker);
                // Gotos force the closing of current block by an ensured jump.
                vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN;
                // We'll defer resolution of the link to next pass
                vecBlocksRequiringFinalizeEnding.append(uCurrentBlock);
                if (uInstrIndex < uLastIndex) { // in case something afterwards, we'll still open another 'block' for taking next instruction
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Opening new block %u for instructions remaining after goto",
                        u64(uCurrentBlock+1u)), pTCContext->pWorker);
                    IRBlock newBlock;
                    init_ir_block(&newBlock, pRepo, uInstrIndex+1u);
                    uCurrentBlock++;
                    vecBlocks.append(newBlock);
                    bThisInstrOpenedAuto = true;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "No remaining instructions after goto"), pTCContext->pWorker);
                }
            } break;

            case IRIT_BRANCH: {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "BRANCH at pos %u forcing the end of current block %u",
                    u64(uInstrIndex), u64(uCurrentBlock)), pTCContext->pWorker);
                // Branch force the closing of current block by a possible fallthrough *AND* a conditional jump.
                // We'll defer resolution of the link (and knowledge of taken vs not) to next pass
                vecBlocksRequiringFinalizeEnding.append(uCurrentBlock);
                if (uInstrIndex < uLastIndex) { // in case something afterwards, we'll still open another 'block' for taking next instruction
                    // and that next instruction (and block) is on our fallthrough path...
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Opening new block %u for fallthrough after branch",
                        u64(uCurrentBlock+1u)), pTCContext->pWorker);
                    vecBlocks[uCurrentBlock].uNextBlockGotoOrFallthrough = uCurrentBlock+1u; // => points to block we're about to spawn
                    IRBlock newBlock;
                    init_ir_block(&newBlock, pRepo, uInstrIndex+1u);
                    uCurrentBlock++;
                    vecBlocks.append(newBlock);
                    bThisInstrOpenedAuto = true;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "No remaining instructions after branch"), pTCContext->pWorker);
                }
            } break;

            case IRIT_ERRCHK: {
                // THOUGTHS: When graphing for first analysis from raw to err, we do not want errchks to spawn numerous
                //   blocks all over the place, so we simply disable the consideration of err handlers. This does not mean they
                //   will be skipped in backend !
                // When compiling in release mode with runtime checks disabled, we do not perform those errchks in backend
                //   anymore... yet we are not *allowed* to do optimization passes on the code as-if they were not taken.
                //   so... divs by zero shall fault, derefs out of index may access violation, and arithmetics shall wrap...
                //   but that's about it => we can still ignore them for a pure 'raw-graph" point of view.
                // When compiling in ultra-release mode with "self-confidence" enabled, we'd want to instruct the compiler that
                //   it is allowed to treat assumptions as granted. This encompass the belief that errchecks will *not* trigger,
                //   and the right to act upon that belief as input to all optimization passes : const-prop, dead-code elim,
                //   etc. So: if a div by zero cannot occur anymore, it means we're allowed to assume that divisor isn't zero,
                //   even *before* the div, proper. If a default case in a switch is *unreachable*, it means we're allowed to
                //   assume that the parameter is necessarily one of the listed ones, even *before* we check for it. It can
                //   have profound effects on constraint analysis, and results may be quite puzzling and illogical if you were
                //   wrong in enabling "self-confidence" in the first place. This would be the *theoretical* equivalent of allowed
                //   behaviour of an optimizing-C-compiler-extremist, allowed to act on any UB in the book... obviously to use with
                //   caution, if we were to ever pursue that idea...
                if (bConsiderErrHandler) {
                    // ErrChks force the closing of current block by a possible fallthrough *AND* a conditional jump.
                    vecBlocks[uCurrentBlock].uNextBlockByBranching = IRGRAPH_ERR_INDEX;
                    bool bWhenKnownIsTaken;
                    // Contrary to Branches, though, we know where it leads in case taken...
                    // => do not defer resolution, and in particular check knowledge of taken vs not right away.
                    if (is_branching_decision_known(entry, uInstrIndex, pProc, pTCContext, &bWhenKnownIsTaken)) {
                        if (bWhenKnownIsTaken) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                "ERRCHK at pos %u, *ensured* taken to err-handler, forcing the end of current block %u",
                                u64(uInstrIndex), u64(uCurrentBlock)), pTCContext->pWorker);
                            vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN;
                        } else {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                "ERRCHK at pos %u, ensured not taken => simply ignored for this graph",
                                u64(uInstrIndex)), pTCContext->pWorker);
                            vecBlocks[uCurrentBlock].uNextBlockByBranching = IRGRAPH_INVALID_INDEX;
                            vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_OTHER_SKIPPABLE;
                            break;
                        }
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "ERRCHK at pos %u forcing the end of current block %u",
                            u64(uInstrIndex), u64(uCurrentBlock)), pTCContext->pWorker);
                    }
                    if (uInstrIndex < uLastIndex) { // in case something afterwards, we'll open another 'block' for taking next instruction
                        // and that next instruction (and block) is on our fallthrough path...
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "Opening new block %u for fallthrough after errchk",
                            u64(uCurrentBlock+1u)), pTCContext->pWorker);
                        vecBlocks[uCurrentBlock].uNextBlockGotoOrFallthrough = uCurrentBlock + 1u; // => points to block we're about to spawn
                        IRBlock newBlock;
                        init_ir_block(&newBlock, pRepo, uInstrIndex+1u);
                        uCurrentBlock++;
                        vecBlocks.append(newBlock);
                        bThisInstrOpenedAuto = true;
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "No remaining instructions after errchk"), pTCContext->pWorker);
                    }
                } else {
                    bool bWhenKnownIsTaken;
                    if (is_branching_decision_known(entry, uInstrIndex, pProc, pTCContext, &bWhenKnownIsTaken)) {
                        if (bWhenKnownIsTaken) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                "ERRCHK at pos %u, normally ignored for current request, but this one is *ensured* taken to err-handler",
                                u64(uInstrIndex)), pTCContext->pWorker);
                            vecBlocks[uCurrentBlock].uNextBlockByBranching = IRGRAPH_ERR_INDEX;
                            vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN;
                            if (uInstrIndex < uLastIndex) { // in case something afterwards, we'll open another 'block' for taking next instruction
                                // and that next instruction (and block) is on our fallthrough path...
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                    "Opening new block %u for instructions remaining after errchk",
                                    u64(uCurrentBlock+1u)), pTCContext->pWorker);
                                IRBlock newBlock;
                                init_ir_block(&newBlock, pRepo, uInstrIndex+1u);
                                uCurrentBlock++;
                                vecBlocks.append(newBlock);
                                bThisInstrOpenedAuto = true;
                            } else {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                    "No remaining instructions after errchk"), pTCContext->pWorker);
                            }
                            break;

                        } else {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                "ERRCHK at pos %u, ignored for current graph request, AND ensured not taken",
                                u64(uInstrIndex)), pTCContext->pWorker);
                            vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_OTHER_SKIPPABLE;
                        }

                    } // otherwise fallthrough:

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "ERRCHK at pos %u, ignored for current graph request",
                        u64(uInstrIndex)), pTCContext->pWorker);
                    vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_NON_GRAPHED_ERRCHK;
                }
            } break;

            case IRIT_RET: {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "RET at pos %u forcing the end of current block %u",
                    u64(uInstrIndex), u64(uCurrentBlock)), pTCContext->pWorker);
                // Rets force the closing of current block by an ensured termination.
                vecBlocks[uCurrentBlock].uNextBlockGotoOrFallthrough = IRGRAPH_RET_INDEX;
                vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN;
                if (uInstrIndex < uLastIndex) { // in case something afterwards, we'll still open another 'block' for taking next instruction
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Opening new block %u for instructions remaining after ret",
                        u64(uCurrentBlock+1u)), pTCContext->pWorker);
                    IRBlock newBlock;
                    init_ir_block(&newBlock, pRepo, uInstrIndex+1u);
                    uCurrentBlock++;
                    vecBlocks.append(newBlock);
                    bThisInstrOpenedAuto = true;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "No remaining instructions after ret"), pTCContext->pWorker);
                }
            } break;

            case IRIT_NO_OP: {
                // well, nuthin'...
            } break;

            case IRIT_MARKER_START_SOURCE_SCOPE: {
                // Start source scope are marked separately
                vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_START_SCOPE;
            } break;

            case IRIT_MARKER_END_SOURCE_SCOPE: {
                // End source scope are marked separately
                vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_END_SCOPE;
            } break;

            case IRIT_LOCAL_VAR_DECL: {
                // Var decls are marked separately
                vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_LOCAL_DECL;
            } break;

            case IRIT_PSEUDO_VALUED_COND: {
                // should not 'need' any treatment here also ? individual branching is already taking care of that, in theory.
            } break;

            case IRIT_STORE:
            case IRIT_STORE_EXT:
            case IRIT_SETZERO:
            {
                // those three instructions are the direct value "setters", marked separately as such
                vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_DIRECT_WRITES;
            } break;

            default: {
                if (has_irit_a_value(uIRIT)) { // our IR is somewhat 'ready for SSA' as far as computing instructions are concerned.
                    // => if an instruction has a value, it is either an already known value, or a runtime temporary
                    if (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) {
                        if (0u == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)) {
                            // As far as IR is concerned, an instruction with a known value, and no
                            // local NYKA within, is ensured to be treated as a true 'constant' of some kind.
                            vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_CONST;
                            // Note that we won't bother tracking those as sequentially meaningful.
                            //   We can think of our flags relationship as 'CONST => SKIPPABLE', see notes for 'skippable', below
                        } else {
                            // A known value holding local nykas is somewhat different from a constant
                            // for IR and backend, although for the purpose of those graph analysis ops,
                            // they also can be 'skipped' in the general case, since it means they are position
                            // independent => do not need real 'blocks' to track their sequential access.
                            vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_OTHER_SKIPPABLE;
                        }
                    } else { // if unknown, however, their position is sequentially important, even if they are temporaries.
                        vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_TMP_RUNTIME;
                    }
                } else {
                    // Any other instruction with no value, we assume, is a *command*, so is quite important
                    //   as they could have any side effect (including hidden *STORE*).
                    // Note that all calls currently fall into that category.
                    // TODO: CLEANUP: separate calls to known "pure" or "functional" from the rest of commands in that regard ?
                    //      (Note: 'pure' is a function with no side effect whatsoever. 'functional' is a function with a promise
                    //       not to operate on our local vars, or on most global vars for that matter... even if we pass pointers
                    //       or stuff - unless we do some local assignment to the implicit 'context', as they are allowed to allocate,
                    //       make sys calls, etc. We should probably add a category of 'functional' which can *only* allocate through
                    //       the well-defined context... althrough we'd need to add tags for calling them in a context where their side
                    //       effect *could*, in fact, matter (eg trace handlers, etc).
                    // THOUGHTS: a special treatment in case of inline assembly ???
                    //      probably not. If there is asm, we can assume user wants them there, whatever we feel about them ;
                    //      but... what about 'jumps', then ???? we can simply warn the user that jumps within inline asm
                    //      must be *self-contained*. If they are not, we simply cannot ensure the validity of our own asm scheme
                    //      surrounding it. Maybe possibly add a tag for saying that we have to drop the ir graph around those asm
                    //      blocks altogether, and revert to a *straightforward* asm around that (but would defeat the purpose of
                    //      optimizing by inline asm in the first place... well, what to do).
                    vecBlocks[uCurrentBlock].uFlags |= IRGRAPH_FLAG_CONTAINS_OTHER_INSTRUCTIONS;
                    //   We can think of our flags relationship as 'OTHER_INSTR => WRITES', see notes for 'store', above
                }
            }
        }
        bLastInstrOpenedAuto = bThisInstrOpenedAuto;
    }

    u32 uLastBlockIndex = uCurrentBlock;
    Assert_(uLastBlockIndex == vecBlocks.size() - 1u);
    Assert_(vecBlocks[uLastBlockIndex].uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX ||
            vecBlocksRequiringFinalizeEnding[vecBlocksRequiringFinalizeEnding.size()-1u] == uLastBlockIndex); // hmm ??

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Discovered %u raw blocks in total", u64(vecBlocks.size())), pTCContext->pWorker);

    if (vecBlocksRequiringFinalizeEnding.size()) {
        u32 uBlockCount = vecBlocks.size();
        TmpMap<u32, u32> mapBlocksByStartIRIndex(FireAndForgetArenaAlloc(pTCContext->pWorker->tmpArena), uBlockCount);
        for (u32 uBlock = 0u; uBlock < uBlockCount; uBlock++) {
            mapBlocksByStartIRIndex.insert(vecBlocks[uBlock].uInstrStart, uBlock);
        }
        Assert_(mapBlocksByStartIRIndex.size() == uBlockCount);
        u32 uBlocksRequiringAttention = vecBlocksRequiringFinalizeEnding.size();
        for (u32 uInVec = 0u; uInVec < uBlocksRequiringAttention; uInVec++) {
            u32 uBlock = vecBlocksRequiringFinalizeEnding[uInVec];
            IRBlock& blockThere = vecBlocks[uBlock];
            u32 uInstrEnd = (uBlock < uLastBlockIndex) ? vecBlocks[uBlock+1u].uInstrStart - 1u : uInstructionCount - 1u;
            IREntry& endingEntry = ir_access_repo_instr(pRepo, uInstrEnd);
            switch (u8(endingEntry.uInstrCodeAndFormatAndFirstParam)) {
                case IRIT_GOTO: {
                    Assert_(blockThere.uNextBlockGotoOrFallthrough == IRGRAPH_INVALID_INDEX); // cuz not yet assigned
                    Assert_(blockThere.uNextBlockByBranching == IRGRAPH_INVALID_INDEX);
                    Assert_(blockThere.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN);
                    u32 uPosInIRofMarker = u32(endingEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    auto itFound = mapBlocksByStartIRIndex.find(uPosInIRofMarker);
                    Assert_(itFound != mapBlocksByStartIRIndex.end());
                    blockThere.uNextBlockGotoOrFallthrough = itFound.value();
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Finalized end-of block %u, with goto IR %u, solved to goto block %u",
                        u64(uBlock), u64(uPosInIRofMarker), u64(blockThere.uNextBlockGotoOrFallthrough)), pTCContext->pWorker);
                } break;
                case IRIT_BRANCH: {
                    Assert_(blockThere.uNextBlockByBranching == IRGRAPH_INVALID_INDEX); // cuz not yet assigned
                    u32 uPosInIRofMarker = u32(endingEntry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    auto itFound = mapBlocksByStartIRIndex.find(uPosInIRofMarker);
                    Assert_(itFound != mapBlocksByStartIRIndex.end());
                    blockThere.uNextBlockByBranching = itFound.value();
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Finalized end-of block %u, with branchto IR %u, solved to branchto block %u",
                        u64(uBlock), u64(uPosInIRofMarker), u64(blockThere.uNextBlockByBranching)), pTCContext->pWorker);
                    bool bWhenKnownIsTaken;
                    if (is_branching_decision_known(endingEntry, uInstrEnd, pProc, pTCContext, &bWhenKnownIsTaken)) {
                        if (bWhenKnownIsTaken)
                            blockThere.uFlags |= IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN;
                        else {
                            blockThere.uFlags |= IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN;
                            Assert_(blockThere.uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX);
                        }
                    } else {
                        Assert_(blockThere.uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX);
                    }
                } break;
                default:
                    Assert_(false);
            }
        }
    }

    reset_arena_no_release_to(beforeAll, pTCContext->pWorker->tmpArena);

    return vecBlocks;
}

// Live-ScopeVector:
// A procwise global support with stack behaviour.
//   u32 entries, each flagged as either a scope-decl, or a var-decl.
// => live-scope vector at start of block: ref a pos+count in that global vector
//    very often, a block can reuse same as parent.
//    live-scope vector at end of block: also ref a pos+count in that global vector

union LiveScopeStackEntry {
    u64 uPayload;
    struct {
        u32 uIndexOfParentScope;    // can be 0 for root scope - otherwise IR position of the IR marker for scope opening.
        u32 uIndexOfLocalVarDecl;   // can be 0 for an entry associated with a scope declaration - otherwise IR position of the local decl.
    };
};

struct LiveAndDeadInSameBlockEntry {
    u32 uIndexOfLocalVarDecl;   // IR position of the local decl. Should never be 0
    u32 uIndexOfScopeEnd;       // IR position of the IR marker for scope closing. Should never be 0
};

struct VisitInfoOnRawGraph {
    u32 uIncomingCount;
    u32 uReferenceHeadIndex;
    // Once block is fully visited:
    //   uLiveScopeVector at start of block == uLiveScopeVectorCount and uLiveScopeVectorIndex found on parentHead for our refHead
    //   uLiveScopeVector at end of block == uLiveScopeVectorCount and uLiveScopeVectorIndex found on our refHead
    // And the following marks entries which have been declared, then marked dead afterwards within that same block
    //   (thus with no remains in the stack, proper)
    u32 uLiveAndDeadCount;
    u32 uLiveAndDeadIndex;
};

struct WalkingHead {
    u32 uCurrentBlock;
    u32 uParentHead;
    // The following reference the *current* vector, and gathers info on the fly =>
    //  * before a head visits a block, shall point to same as parent head => state at block start
    //  * after a head has visited a block, shall point to state at the end => state at block end
    u32 uLiveScopeVectorCount;
    u32 uLiveScopeVectorIndex;
    //u32 uFlags;
};

// IRTrimmedBlock is the Node for the 'Trimmed' Graph + knowledge of the 1 or 2 output edges.
// Trimmed graph is a vector of blocks, were we skipped all skippable blocks, and ordered so that:
//      Trimmed block 0 still marks the entry point of the program.
//      Ordered in an already optimized order (possibly taking path coldness into account, once we have that (TODO!),
//          assuming bakend will emit them one after the other, and trying to minimize jump counts, knowing that a link
//          to current trimmed block + 1 does not need any jump for backend.
struct IRTrimmedBlock {
    u32 uFirstSignificantInstr;         // position of first relevant (non-skippable) IR Instruction index on that block. 
    u32 uLastSignificantInstr;          // position of last relevant (non-skippable) IR Instruction on that block.
    u32 uFirstInstrForComputingBranch;  // position of first relevant (non-skippable) IR instruction for computing the branch, if branch. May be same as 'instr-branch', in which case there is nothing special to do before
    u32 uBranchingInstr;                // If dual to non-err, index of branch instruction. If dual or single to err, index of errchk instruction.
    u32 uNextBlockGotoOrFallthrough;    // either the index of a target block, or 'ret', uness this ends with an ensured-taken errchk.
    u32 uNextBlockByBranching;          // the index of a target block (can here be ret), or errchk. Note that the choice of target block
                                        //   being the 'secondary' can here be reversed from what it was in IR. if so, we'll find that info on flags.
    u32 uIncomingLiveScopeCount;        // the number of LiveScopeStackEntry representing the set of *statically* live variables (by source scope) at start of block. Same for all incoming paths.
    u32 uIncomingLiveScopeIndex;        // the index (in the associated vecScopeStacksAll, in GraphResult) at which to find the first of the uIncomingLiveScopeCount entries representing live variables at start of block
    
    u32 uLiveAndDeadCount;              // the number of LiveAndDeadInSameBlockEntry representing all variables created and destroyed locally to this block.
    u32 uLiveAndDeadIndex;              // the index (in the associated vecLiveAndDeadInSameBlock, in GraphResult) at which to find the of the uLiveAndDeadCount entries representing variables created and destroyed locally

    u32 uOriginRawBlock;                // the raw block at the origin of this graph. can be IRGRAPH_INVALID_INDEX for the trimmed block representing 'proc-entry' 
    u32 uFinalFlags;                    // the flags for that IRTrimmedBlock, @see IRGRAPH_FINAL_FLAG_...
};

#define IRGRAPH_FINAL_FLAG_IS_MULTI_INCOMING    0x0000'0001u
#define IRGRAPH_FINAL_FLAG_HAS_NON_ENSURED_EXIT 0x0000'0002u
#define IRGRAPH_FINAL_FLAG_HAS_NON_SKIPPABLE    0x0000'0004u


/*
struct VarLiveliness {
    u32 uIRofVarDeclAsId;
    u32 uLiveAtPos; // INVALID if said pos is out of block
    u32 uDeadAtPos; // INVALID if said pos is out of block
}
*/

struct GraphResult {
    TmpArray<IRBlock> vecRawBlocks;
    TmpArray<IRTrimmedBlock> vecFinalBlocks;
    TmpArray<LiveScopeStackEntry> vecScopeStacksAll;
    TmpArray<LiveAndDeadInSameBlockEntry> vecLiveAndDeadInSameBlock;
};

local_func GraphResult walk_IR_graph_for_proc_trimming_nodes_and_gather_locals(TCProcBodyResult* pProc, TCContext* pTCContext,
    const TmpArray<IRBlock>& rawBlocks, Arena resultArena)
{

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL5_SIGNIFICANT_STEP, EventREPT_CUSTOM_HARDCODED("Walking raw IR graph for procbody %u (main id '%s') in file %s",
        u64(pProc->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProc->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pProgCompilationState->vecSourceFiles[u32(pProc->iSourceFileIndex)]->sourceFileName.c_str())), pTCContext->pWorker);

    u32 uRawBlockCount = rawBlocks.size();
    Assert_(uRawBlockCount);

    ArenaRefPoint beforeAll = get_arena_ref_point(pTCContext->pWorker->tmpArena);

    // One per node:
    VisitInfoOnRawGraph* tVisitInfo = (VisitInfoOnRawGraph*)alloc_from(pTCContext->pWorker->tmpArena,
        sizeof(VisitInfoOnRawGraph)*uRawBlockCount, alignof(VisitInfoOnRawGraph));
    for (u32 uRaw = 0u; uRaw < uRawBlockCount; uRaw++) {
        tVisitInfo[uRaw].uIncomingCount = 0u;
        tVisitInfo[uRaw].uReferenceHeadIndex = IRGRAPH_INVALID_INDEX;
        tVisitInfo[uRaw].uLiveAndDeadCount = 0u;
        tVisitInfo[uRaw].uLiveAndDeadIndex = 0u;
    }
    //TmpArray<VarLiveliness> vecVarLivelinessAll(pTCContext->pWorker->tmpArena);

    // TODO: put the following two arrays in temporary mem, and also try to trim those against trimmed result afterwards, to a final alloc
    TmpArray<LiveScopeStackEntry> vecScopeStacksAll(resultArena);
    TmpArray<LiveAndDeadInSameBlockEntry> vecLiveAndDeadInSameBlock(resultArena);

    // One per possible distinct head : 1 for proc-entry, + max 2 per node ; statically allocated in one go now.
    u32 uMaxWalkingHeadsCount = 1u + 2u * uRawBlockCount;
    WalkingHead* tWalkingHeads = (WalkingHead*)alloc_from(pTCContext->pWorker->tmpArena,
        sizeof(WalkingHead)*uMaxWalkingHeadsCount, alignof(WalkingHead));
    u32 uEntryPointHeadIndex = uMaxWalkingHeadsCount-1u;
    for (u32 uHead = 0u; uHead < uEntryPointHeadIndex; uHead++) {
        tWalkingHeads[uHead].uCurrentBlock = IRGRAPH_INVALID_INDEX;
        tWalkingHeads[uHead].uParentHead = IRGRAPH_INVALID_INDEX;
        tWalkingHeads[uHead].uLiveScopeVectorCount = 0;
        tWalkingHeads[uHead].uLiveScopeVectorIndex = 0;
        //tWalkingHeads[uHead].uFlags = 0u;
    }
    tWalkingHeads[uEntryPointHeadIndex].uCurrentBlock = 0u;
    tWalkingHeads[uEntryPointHeadIndex].uParentHead = IRGRAPH_INVALID_INDEX;
    tWalkingHeads[uEntryPointHeadIndex].uLiveScopeVectorCount = 0u;
    tWalkingHeads[uEntryPointHeadIndex].uLiveScopeVectorIndex = 0u;
    //tWalkingHeads[uEntryPointHeadIndex].uFlags = 0u;

    TmpArray<u32> vecActiveHeadIndex(pTCContext->pWorker->tmpArena); // Heads which still may try to open new paths on the graphs
    TmpArray<u32> vecStdEdgeIndex(pTCContext->pWorker->tmpArena);    // All Heads that have been treated (corresponding to all active graph edges, barring errchks and rets)
    TmpArray<u32> vecOnHoldHeadIndex(pTCContext->pWorker->tmpArena); // All Heads leading to a node which has already been handled by another head.
    TmpArray<u32> vecLeafHeadIndex(pTCContext->pWorker->tmpArena);   // All Heads, not-on-hold, whose node haven't spawned any other head (=> exits to ret or errchk)
    TmpArray<u32> vecOtherHeadIndex(pTCContext->pWorker->tmpArena);  // All Heads, not-on-hold, and not-leaf.
    vecActiveHeadIndex.reserve(uMaxWalkingHeadsCount);
    vecStdEdgeIndex.reserve(uMaxWalkingHeadsCount);
    vecOnHoldHeadIndex.reserve(uMaxWalkingHeadsCount);
    vecLeafHeadIndex.reserve(uMaxWalkingHeadsCount);
    vecOtherHeadIndex.reserve(uMaxWalkingHeadsCount);

    //#define PLAY_WITH_MAP_REFS_OF_LOCAL_VARS 1
    #if PLAY_WITH_MAP_REFS_OF_LOCAL_VARS
        TmpMap<u32, u32> mapKnownParentScopeFromChildScopeOrDecl(FireAndForgetArenaAlloc(pTCContext->pWorker->tmpArena));
        TmpMap<u32, u32> mapVarDeclIRtoBlockIR(FireAndForgetArenaAlloc(pTCContext->pWorker->tmpArena));
        TmpMap<u32, TmpArray<u32>> mapVarsByDirectScope(FireAndForgetArenaAlloc(pTCContext->pWorker->tmpArena));
        TmpArray<u32> newVarDeclArrayForRootScope(pTCContext->pWorker->tmpArena);
        mapVarsByDirectScope.insert(0u, newVarDeclArrayForRootScope);
    #endif

    constexpr u32 IRGRAPH_NON_SKIPPABLE_INSTRUCTIONS_FLAGS = 
        IRGRAPH_FLAG_CONTAINS_TMP_RUNTIME|
        IRGRAPH_FLAG_CONTAINS_DIRECT_WRITES|
        IRGRAPH_FLAG_CONTAINS_OTHER_INSTRUCTIONS|
        IRGRAPH_FLAG_CONTAINS_NON_GRAPHED_ERRCHK;

    IRRepo* pRepo = &(pProc->procwiseRepo);

    u32 uActiveRawBlocksCount = 0u;
    u32 uActiveBlocksWithLocalsCount = 0u;
    u32 uActiveSkippableByContentBlocksCount = 0u;
    u32 uActiveBlocksWithNonErrDualEdgeCount = 0u;
    u32 uActiveEdgesToErr = 0u;
    u32 uActiveEdgesToRet = 0u;
    u32 uMultiIncomingRawBlockCount = 0u;
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Raw graph has %u nodes, thus max %u edges",
            u64(uRawBlockCount), u64(uMaxWalkingHeadsCount)), pTCContext->pWorker);

        vecActiveHeadIndex.append(uEntryPointHeadIndex);
        while (vecActiveHeadIndex.size()) {
            u32 uActiveHeadIndex = vecActiveHeadIndex.pop_last();
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Waking active head %u. Remaining active after that:%u",
                u64(uActiveHeadIndex), u64(vecActiveHeadIndex.size())), pTCContext->pWorker);

            while (true) { // on_chosen_active:
                WalkingHead& currentHead = tWalkingHeads[uActiveHeadIndex];
                Assert_(currentHead.uLiveScopeVectorIndex + currentHead.uLiveScopeVectorCount <= vecScopeStacksAll.size());
                vecStdEdgeIndex.append(uActiveHeadIndex);
                u32 uBlockIndex = currentHead.uCurrentBlock;
                Assert_(uBlockIndex < uRawBlockCount);
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Considering current active head %u, over raw block %u",
                    u64(uActiveHeadIndex), u64(uBlockIndex)), pTCContext->pWorker);
                VisitInfoOnRawGraph& blockVisitInfo = tVisitInfo[uBlockIndex];

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Before checking current block, we have count %u for scope stack (from index %u), with current scope %u.",
                    u64(currentHead.uLiveScopeVectorCount), u64(currentHead.uLiveScopeVectorIndex),
                    u64(currentHead.uLiveScopeVectorCount ?
                        vecScopeStacksAll[currentHead.uLiveScopeVectorIndex + currentHead.uLiveScopeVectorCount - 1u].uIndexOfParentScope : 0u)),
                    pTCContext->pWorker);

                if (blockVisitInfo.uIncomingCount == 0u) {
                    uActiveRawBlocksCount++;
                    Assert_(blockVisitInfo.uReferenceHeadIndex == IRGRAPH_INVALID_INDEX);
                    blockVisitInfo.uReferenceHeadIndex = uActiveHeadIndex;
                    blockVisitInfo.uIncomingCount = 1u;
                    const IRBlock& rawBlock = rawBlocks[uBlockIndex];
                    blockVisitInfo.uLiveAndDeadIndex = vecLiveAndDeadInSameBlock.size();
                    Assert_(blockVisitInfo.uLiveAndDeadCount == 0u); 

                    if (rawBlock.uFlags & (IRGRAPH_FLAG_CONTAINS_START_SCOPE|IRGRAPH_FLAG_CONTAINS_LOCAL_DECL|IRGRAPH_FLAG_CONTAINS_END_SCOPE)) {
                        // while we're at it, register local variables on active paths and their static (scope-bound) liveliness range.
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "Block %u has instructions affecting local variable static liveliness analysis.",
                            u64(uBlockIndex)), pTCContext->pWorker);
                        u32 uStartingStackSizeAll = vecScopeStacksAll.size();
                        u32 uBaseIndexInScopeVector = currentHead.uLiveScopeVectorIndex;
                        u32 uBaseEntriesCount = currentHead.uLiveScopeVectorCount;
                        bool bIsAtEndOfStackForAll = (uBaseIndexInScopeVector + uBaseEntriesCount == uStartingStackSizeAll);
                        bool bIsEnsuredNonAliasingStackOfOther = false;
                        if (rawBlock.uFlags & (IRGRAPH_FLAG_CONTAINS_START_SCOPE|IRGRAPH_FLAG_CONTAINS_LOCAL_DECL)) {
                            if (!bIsAtEndOfStackForAll) {
                                Assert_(uBaseIndexInScopeVector + uBaseEntriesCount < uStartingStackSizeAll);
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                    "Block %u has instructions adding to the stack, and we're not at the end => re-append the current %u entries to index %u",
                                    u64(uBlockIndex), u32(uBaseEntriesCount), u64(uStartingStackSizeAll)), pTCContext->pWorker);
                                uBaseIndexInScopeVector = uStartingStackSizeAll;
                                u32 uEntriesEnd = currentHead.uLiveScopeVectorIndex + uBaseEntriesCount;
                                vecScopeStacksAll.reserve(uStartingStackSizeAll + uBaseEntriesCount);
                                for (u32 uEntryIndex = currentHead.uLiveScopeVectorIndex; uEntryIndex < uEntriesEnd; uEntryIndex++) {
                                    vecScopeStacksAll.append(vecScopeStacksAll[uEntryIndex]);
                                }
                                // Note: 'index' can be pointing to *after* current end of vecScopeStacksAll if count is 0
                                currentHead.uLiveScopeVectorIndex = uBaseIndexInScopeVector;
                                bIsAtEndOfStackForAll = true;
                                bIsEnsuredNonAliasingStackOfOther = true;
                            } else {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                    "Block %u has instructions adding to the liveliness stack, but we are already positionned at the end => growing in-place",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                            }
                        }
                        u32 uCurrentScope = uBaseEntriesCount ?
                            vecScopeStacksAll[uBaseIndexInScopeVector + uBaseEntriesCount - 1u].uIndexOfParentScope : 0u;

                        u32 uNextBlock = uBlockIndex+1u;
                        u32 uInstrEnd = (uNextBlock < uRawBlockCount) ? rawBlocks[uNextBlock].uInstrStart : pRepo->uSize;
                        for (u32 uInstrIndex = rawBlock.uInstrStart; uInstrIndex < uInstrEnd; uInstrIndex++) {
                            IREntry& entry = ir_access_repo_instr(pRepo, uInstrIndex);
                            switch (u8(entry.uInstrCodeAndFormatAndFirstParam)) {
                                case IRIT_MARKER_START_SOURCE_SCOPE: {
                                    Assert_(bIsAtEndOfStackForAll);
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                        "Block %u has START_SOURCE_SCOPE at IR pos %u.",
                                        u64(uBlockIndex), u64(uInstrIndex)), pTCContext->pWorker);
                                    #if PLAY_WITH_MAP_REFS_OF_LOCAL_VARS
                                        mapKnownParentScopeFromChildScopeOrDecl.insert(uInstrIndex, uCurrentScope);
                                        TmpArray<u32> newVarDeclArrayForThisScope(pTCContext->pWorker->tmpArena);
                                        mapVarsByDirectScope.insert(uInstrIndex, newVarDeclArrayForThisScope);
                                    #endif
                                    uCurrentScope = uInstrIndex;
                                    LiveScopeStackEntry newLiveScopeEntry;
                                    newLiveScopeEntry.uIndexOfParentScope = uCurrentScope;
                                    newLiveScopeEntry.uIndexOfLocalVarDecl = 0u;
                                    vecScopeStacksAll.append(newLiveScopeEntry);
                                    currentHead.uLiveScopeVectorCount++;
                                    Assert_(uBaseIndexInScopeVector + currentHead.uLiveScopeVectorCount == vecScopeStacksAll.size());
                                } break;
                                case IRIT_LOCAL_VAR_DECL: {
                                    Assert_(bIsAtEndOfStackForAll);
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                        "Block %u has LOCAL_VAR_DEC at IR pos %u.",
                                        u64(uBlockIndex), u64(uInstrIndex)), pTCContext->pWorker);
                                    #if PLAY_WITH_MAP_REFS_OF_LOCAL_VARS
                                        mapKnownParentScopeFromChildScopeOrDecl.insert(uInstrIndex, uCurrentScope);
                                        mapVarDeclIRtoBlockIR.insert(uInstrIndex, uBlockIndex);
                                        mapVarsByDirectScope[uCurrentScope].append(uInstrIndex);
                                    #endif
                                    LiveScopeStackEntry newLiveScopeEntry;
                                    newLiveScopeEntry.uIndexOfParentScope = uCurrentScope;
                                    newLiveScopeEntry.uIndexOfLocalVarDecl = uInstrIndex;
                                    vecScopeStacksAll.append(newLiveScopeEntry);
                                    currentHead.uLiveScopeVectorCount++;
                                    Assert_(uBaseIndexInScopeVector + currentHead.uLiveScopeVectorCount == vecScopeStacksAll.size());
                                } break;
                                case IRIT_MARKER_END_SOURCE_SCOPE: {
                                    u32 uClosedScope = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                        "Block %u has END_SOURCE_SCOPE at IR pos %u - target is scope opened at %u.",
                                        u64(uBlockIndex), u64(uInstrIndex), u64(uClosedScope)), pTCContext->pWorker);
                                    Assert_(uCurrentScope);
                                    Assert_(uClosedScope == uCurrentScope);
                                    #if PLAY_WITH_MAP_REFS_OF_LOCAL_VARS
                                        TmpArray<u32>& vecVarsInThisScope = mapVarsByDirectScope[uClosedScope];
                                        u32 uCountVarsInThisScope = vecVarsInThisScope.size();
                                        for (u32 uVarInVec = 0u; uVarInVec < uCountVarsInThisScope; uVarInVec++) {
                                            u32 uVarId = vecVarsInThisScope[uVarInVec];
                                            Assert_(uClosedScope == mapKnownParentScopeFromChildScopeOrDecl[uVarId]);
                                        }
                                        u32 uCurrentScopeAfterClose = mapKnownParentScopeFromChildScopeOrDecl[uClosedScope];
                                        // TODO: register end scope of all variables with current scope...
                                    #endif
                                    Assert_(currentHead.uLiveScopeVectorCount);
                                    u32 uLastIndex = uBaseIndexInScopeVector + currentHead.uLiveScopeVectorCount; // not yet "last index" per se... last index is -1 from that
                                    while (uLastIndex > uBaseIndexInScopeVector) { // not yet "last index" per se... last index is -1 from that
                                        uLastIndex--; // now, it's really last index...
                                        if (vecScopeStacksAll[uLastIndex].uIndexOfParentScope != uClosedScope) { // nope, do not kill this one
                                            // since it a variable from a scope definition, or scope definition itself, which is more basal than the scope we're ending
                                            uLastIndex++; // => revert decrementing last index, and stop iterating
                                            break;
                                        }
                                        if (uLastIndex >= uBaseIndexInScopeVector + uBaseEntriesCount) {
                                            if (vecScopeStacksAll[uLastIndex].uIndexOfLocalVarDecl) {
                                                LiveAndDeadInSameBlockEntry newLiveAndDeadEntry;
                                                //newLiveAndDeadEntry.uIndexOfParentScope = vecScopeStacksAll[uLastIndex].uIndexOfParentScope;
                                                newLiveAndDeadEntry.uIndexOfLocalVarDecl = vecScopeStacksAll[uLastIndex].uIndexOfLocalVarDecl;
                                                newLiveAndDeadEntry.uIndexOfScopeEnd = uInstrIndex;
                                                vecLiveAndDeadInSameBlock.append(newLiveAndDeadEntry);
                                                blockVisitInfo.uLiveAndDeadCount++;
                                            }
                                        }
                                    }
                                    uBaseEntriesCount = uLastIndex - uBaseIndexInScopeVector;
                                    uCurrentScope = uBaseEntriesCount ?
                                        vecScopeStacksAll[uBaseIndexInScopeVector + uBaseEntriesCount - 1u].uIndexOfParentScope : 0u;
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                        "After end scope, Block %u now has base entries count %u for scope stack, with current scope reset to %u.",
                                        u64(uBlockIndex), u64(uBaseEntriesCount), u64(uCurrentScope)), pTCContext->pWorker);
                                    currentHead.uLiveScopeVectorCount = uBaseEntriesCount;
                                    if (bIsAtEndOfStackForAll) {
                                        if (bIsEnsuredNonAliasingStackOfOther)
                                            vecScopeStacksAll.resize(uBaseIndexInScopeVector + uBaseEntriesCount);
                                        else {
                                            Assert_(uBaseIndexInScopeVector + uBaseEntriesCount < vecScopeStacksAll.size());
                                            bIsAtEndOfStackForAll = false;
                                        }
                                    } else {
                                        Assert_(uBaseIndexInScopeVector + uBaseEntriesCount < vecScopeStacksAll.size());
                                    }
                                    #if PLAY_WITH_MAP_REFS_OF_LOCAL_VARS
                                        Assert_(uCurrentScopeAfterClose == uCurrentScope);
                                    #endif
                                } break;
                            }
                        }
                        uActiveBlocksWithLocalsCount++;
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "After browsing for scope and locals, we have count %u for scope stack (from index %u), with current scope %u.",
                            u64(currentHead.uLiveScopeVectorCount), u64(currentHead.uLiveScopeVectorIndex), u64(uCurrentScope)), pTCContext->pWorker);
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "Block %u has %u variables registered as spawned-then-killed locally (from index %u in the spawn-and-kill registry).",
                            u64(uBlockIndex), u64(blockVisitInfo.uLiveAndDeadCount), u64(blockVisitInfo.uLiveAndDeadIndex)), pTCContext->pWorker);
                    }

                    if (rawBlock.uFlags & IRGRAPH_NON_SKIPPABLE_INSTRUCTIONS_FLAGS) {
                        // Block contains instructions which are not skippable in a sequential analysis...
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u has instructions which cannot be skipped. TODO",
                            u64(uBlockIndex)), pTCContext->pWorker);
                    } else {
                        // Aside from exit branches, block seems fully skippable.
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Aside from exit-point(s), Block %u has only skippable instructions for sequential analysis. TODO",
                            u64(uBlockIndex)), pTCContext->pWorker);
                        uActiveSkippableByContentBlocksCount++;
                    }

                    if (rawBlock.uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                        // Block has an ensured single-exit point.
                        u32 uEnsuredRawTarget = (rawBlock.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                            rawBlock.uNextBlockGotoOrFallthrough : rawBlock.uNextBlockByBranching;
                        Assert_(uEnsuredRawTarget != IRGRAPH_INVALID_INDEX);
                        if (uEnsuredRawTarget < IRGRAPH_START_SPECIAL) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u has a single, standard exit-point (to block %u)",
                                u64(uBlockIndex), u64(uEnsuredRawTarget)), pTCContext->pWorker);
                            u32 uEdgeIndex = uBlockIndex * 2u;
                            tWalkingHeads[uEdgeIndex].uCurrentBlock = uEnsuredRawTarget;
                            tWalkingHeads[uEdgeIndex].uParentHead = uActiveHeadIndex;
                            tWalkingHeads[uEdgeIndex].uLiveScopeVectorCount = currentHead.uLiveScopeVectorCount;
                            tWalkingHeads[uEdgeIndex].uLiveScopeVectorIndex = currentHead.uLiveScopeVectorIndex;
                            vecOtherHeadIndex.append(uActiveHeadIndex);
                            uActiveHeadIndex = uEdgeIndex;
                            continue;
                        } else {
                            if (uEnsuredRawTarget == IRGRAPH_ERR_INDEX) {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u has a single exit-point to err-handler !!",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                uActiveEdgesToErr++;
                            } else { Assert_(uEnsuredRawTarget == IRGRAPH_RET_INDEX);
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u has a proc-return as single exit-point",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                uActiveEdgesToRet++;
                            }
                            vecLeafHeadIndex.append(uActiveHeadIndex);
                            break;
                        }

                    } else {
                        // Block has two exit-points.
                        u32 uPotentialPrim = rawBlock.uNextBlockGotoOrFallthrough;
                        u32 uPotentialSeco = rawBlock.uNextBlockByBranching;
                        Assert_(uPotentialPrim != IRGRAPH_INVALID_INDEX);
                        Assert_(uPotentialSeco != IRGRAPH_INVALID_INDEX);
                        u32 uNextHead = IRGRAPH_INVALID_INDEX;
                        if (uPotentialPrim < IRGRAPH_START_SPECIAL) {
                            u32 uPrimEdgeIndex = uBlockIndex * 2u;
                            tWalkingHeads[uPrimEdgeIndex].uCurrentBlock = uPotentialPrim;
                            tWalkingHeads[uPrimEdgeIndex].uParentHead = uActiveHeadIndex;
                            tWalkingHeads[uPrimEdgeIndex].uLiveScopeVectorCount = currentHead.uLiveScopeVectorCount;
                            tWalkingHeads[uPrimEdgeIndex].uLiveScopeVectorIndex = currentHead.uLiveScopeVectorIndex;
                            uNextHead = uPrimEdgeIndex;
                        } else {
                            if (uPotentialPrim == IRGRAPH_ERR_INDEX) {
                                uActiveEdgesToErr++;
                            } else { Assert_(uPotentialPrim == IRGRAPH_RET_INDEX);
                                uActiveEdgesToRet++;
                            }
                        }
                        if (uPotentialSeco < IRGRAPH_START_SPECIAL) {
                            u32 uSecoEdgeIndex = (uBlockIndex * 2u) + 1u;
                            tWalkingHeads[uSecoEdgeIndex].uCurrentBlock = uPotentialSeco;
                            tWalkingHeads[uSecoEdgeIndex].uParentHead = uActiveHeadIndex;
                            tWalkingHeads[uSecoEdgeIndex].uLiveScopeVectorCount = currentHead.uLiveScopeVectorCount;
                            tWalkingHeads[uSecoEdgeIndex].uLiveScopeVectorIndex = currentHead.uLiveScopeVectorIndex;
                            if (uNextHead == IRGRAPH_INVALID_INDEX)
                                uNextHead = uSecoEdgeIndex;
                            else
                                vecActiveHeadIndex.append(uSecoEdgeIndex);

                        } else {
                            if (uPotentialSeco == IRGRAPH_ERR_INDEX) {
                                uActiveEdgesToErr++;
                            } else { Assert_(uPotentialSeco == IRGRAPH_RET_INDEX);
                                uActiveEdgesToRet++;
                            }
                        }
                        if (uPotentialPrim < IRGRAPH_START_SPECIAL && uPotentialSeco < IRGRAPH_START_SPECIAL) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u has two standard exit-points (to blocks %u and %u).",
                                u64(uBlockIndex), u64(uPotentialPrim), u64(uPotentialSeco)), pTCContext->pWorker);
                            uActiveBlocksWithNonErrDualEdgeCount++;
                            Assert_(uNextHead != IRGRAPH_INVALID_INDEX);
                        } else if (uPotentialPrim != IRGRAPH_ERR_INDEX && uPotentialSeco != IRGRAPH_ERR_INDEX) {
                            Assert_(uPotentialPrim < IRGRAPH_START_SPECIAL || uPotentialPrim == IRGRAPH_RET_INDEX);
                            Assert_(uPotentialSeco < IRGRAPH_START_SPECIAL || uPotentialSeco == IRGRAPH_RET_INDEX);
                            if (uPotentialPrim < IRGRAPH_START_SPECIAL || uPotentialSeco < IRGRAPH_START_SPECIAL) {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u (strangely?) has two exit points, one of which is ret",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                Assert_(uNextHead != IRGRAPH_INVALID_INDEX);
                            } else {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u (strangely?) has two exit points to ret",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                Assert_(uNextHead == IRGRAPH_INVALID_INDEX);
                            }
                            uActiveBlocksWithNonErrDualEdgeCount++;
                        } else {
                            Assert_(uPotentialPrim == IRGRAPH_ERR_INDEX || uPotentialSeco == IRGRAPH_ERR_INDEX);
                            if (uPotentialPrim < IRGRAPH_ERR_INDEX || uPotentialSeco < IRGRAPH_ERR_INDEX) {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u has a two exit-point termination, formed by standard vs errhandler",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                Assert_(uNextHead != IRGRAPH_INVALID_INDEX);
                            } else if (uPotentialPrim == IRGRAPH_RET_INDEX || uPotentialSeco == IRGRAPH_RET_INDEX) {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u (strangely?) has two exit points, one of which is ret, the other being errhandler",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                Assert_(uNextHead == IRGRAPH_INVALID_INDEX);
                            } else {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u (strangely?) has two exit points to errhandler",
                                    u64(uBlockIndex)), pTCContext->pWorker);
                                Assert_(uNextHead == IRGRAPH_INVALID_INDEX);
                            }
                        }
                        if (uNextHead != IRGRAPH_INVALID_INDEX) {
                            vecOtherHeadIndex.append(uActiveHeadIndex);
                            uActiveHeadIndex = uNextHead;
                            continue;
                        } else {
                            vecLeafHeadIndex.append(uActiveHeadIndex);
                            break;
                        }
                    }

                } else {
                    Assert_(blockVisitInfo.uReferenceHeadIndex != IRGRAPH_INVALID_INDEX);
                    WalkingHead& refHeadBefore = tWalkingHeads[tWalkingHeads[blockVisitInfo.uReferenceHeadIndex].uParentHead];
                    Assert_(currentHead.uLiveScopeVectorCount == refHeadBefore.uLiveScopeVectorCount);
                    Assert_(currentHead.uLiveScopeVectorCount == 0 ||
                            vecScopeStacksAll[currentHead.uLiveScopeVectorIndex + currentHead.uLiveScopeVectorCount - 1u].uPayload ==
                            vecScopeStacksAll[refHeadBefore.uLiveScopeVectorIndex + refHeadBefore.uLiveScopeVectorCount - 1u].uPayload);
                    if (blockVisitInfo.uIncomingCount == 1u) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u already visited once => Counting one more multi-incoming block. Putting head %u on hold",
                            u64(uBlockIndex), u64(uActiveHeadIndex)), pTCContext->pWorker);
                        uMultiIncomingRawBlockCount++;
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED("Block %u already visited %u times. Increasing visit count %u. Putting head %u on hold",
                            u64(uBlockIndex), u64(blockVisitInfo.uIncomingCount), u64(uActiveHeadIndex)), pTCContext->pWorker);
                    }
                    vecOnHoldHeadIndex.append(uActiveHeadIndex);
                    blockVisitInfo.uIncomingCount++;
                    break;
                }
            }
        }
    }

    //#define PLAY_WITH_CROSS_REFS_BLOCK_EDGES 1
    #if PLAY_WITH_CROSS_REFS_BLOCK_EDGES
        Assert_(uMultiIncomingRawBlockCount < uActiveRawBlocksCount);
        TmpMap<u32, u32>           mapSingleIncomingRaw(FireAndForgetArenaAlloc(pTCContext->pWorker->tmpArena), uActiveRawBlocksCount - uMultiIncomingRawBlockCount);
        TmpMap<u32, TmpArray<u32>> mapMultiIncomingsRaw(FireAndForgetArenaAlloc(pTCContext->pWorker->tmpArena), uMultiIncomingRawBlockCount);
        for (u32 uRaw = 0u; uRaw < uRawBlockCount; uRaw++) {
            VisitInfoOnRawGraph& blockInfo = tVisitInfo[uRaw];
            if (blockInfo.uIncomingCount > 1u) {
                TmpArray<u32> newArray(pTCContext->pWorker->tmpArena);
                mapMultiIncomingsRaw.insert(uRaw, newArray);
            }
        }
        for (u32 uRaw = 0u; uRaw < uRawBlockCount; uRaw++) {
            VisitInfoOnRawGraph& blockInfo = tVisitInfo[uRaw];
            if (blockInfo.uIncomingCount) {
                const IRBlock& block = rawBlocks[uRaw];
                if (block.uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                    u32 uEnsuredRawTarget = (block.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                        block.uNextBlockGotoOrFallthrough : block.uNextBlockByBranching;
                    Assert_(uEnsuredRawTarget != IRGRAPH_INVALID_INDEX);
                    if (uEnsuredRawTarget < IRGRAPH_START_SPECIAL) {
                        VisitInfoOnRawGraph& targetBlockInfo = tVisitInfo[uEnsuredRawTarget];
                        if (targetBlockInfo.uIncomingCount > 1u) {
                            mapMultiIncomingsRaw[uEnsuredRawTarget].append(uRaw);
                        } else {
                            mapSingleIncomingRaw.insert(uEnsuredRawTarget, uRaw);
                        }
                    }
                } else {
                    u32 uPotentialPrim = block.uNextBlockGotoOrFallthrough;
                    u32 uPotentialSeco = block.uNextBlockByBranching;
                    Assert_(uPotentialPrim != IRGRAPH_INVALID_INDEX);
                    Assert_(uPotentialSeco != IRGRAPH_INVALID_INDEX);
                    if (uPotentialPrim < IRGRAPH_START_SPECIAL) {
                        VisitInfoOnRawGraph& targetBlockInfo = tVisitInfo[uPotentialPrim];
                        if (targetBlockInfo.uIncomingCount > 1u) {
                            mapMultiIncomingsRaw[uPotentialPrim].append(uRaw);
                        } else {
                            mapSingleIncomingRaw.insert(uPotentialPrim, uRaw);
                        }
                    }
                    if (uPotentialSeco < IRGRAPH_START_SPECIAL) {
                        VisitInfoOnRawGraph& targetBlockInfo = tVisitInfo[uPotentialSeco];
                        if (targetBlockInfo.uIncomingCount > 1u) {
                            mapMultiIncomingsRaw[uPotentialSeco].append(uRaw);
                        } else {
                            mapSingleIncomingRaw.insert(uPotentialSeco, uRaw);
                        }
                    }
                }
            }
        }
        Assert_(mapSingleIncomingRaw.size() == uActiveRawBlocksCount - uMultiIncomingRawBlockCount - 1u); // -1 for proc-entry
    #endif // PLAY_WITH_CROSS_REFS_BLOCK_EDGES

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("Done walking raw graph. From Raw %u nodes, %u nodes were potentially visited, %u standard edges potentially taken",
            u64(uRawBlockCount), u64(uActiveRawBlocksCount), u64(vecStdEdgeIndex.size())), pTCContext->pWorker);
    }
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("Also %u potentially taken edges to err-handler, and %u active edges to proc-return. All source-scope consistency successfully checked.",
            u64(uActiveEdgesToErr), u64(uActiveEdgesToRet)), pTCContext->pWorker);
    }
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("%u active blocks considered for var-decl. %u active blocks with otherwise skippable content. %u active blocks with non-err dual-exit potential",
            u64(uActiveBlocksWithLocalsCount), u64(uActiveSkippableByContentBlocksCount), u64(uActiveBlocksWithNonErrDualEdgeCount)), pTCContext->pWorker);
    }
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("%u heads are on-hold, %u heads are leaves, %u heads are other",
            u64(vecOnHoldHeadIndex.size()), u64(vecLeafHeadIndex.size()), u64(vecOtherHeadIndex.size())), pTCContext->pWorker);
        Assert_(vecOnHoldHeadIndex.size() + vecLeafHeadIndex.size() + vecOtherHeadIndex.size() == vecStdEdgeIndex.size());
    }
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("global liveliness stack has grown to final size %u, and global liveAndDeadInSameBlock has total count %u",
            u64(vecScopeStacksAll.size()), u64(vecLiveAndDeadInSameBlock.size())), pTCContext->pWorker);
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Now doing block-reduce pass:"), pTCContext->pWorker);

    #if PLAY_WITH_MAP_REFS_OF_LOCAL_VARS
    {
        Assert_(mapVarsByDirectScope.size()); // at least root scope
        Assert_(mapKnownParentScopeFromChildScopeOrDecl.size() - mapVarDeclIRtoBlockIR.size() == mapVarsByDirectScope.size() - 1u);
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED("%u var declarations, %u local scopes (including proc root)",
            u64(mapVarDeclIRtoBlockIR.size()), u64(mapVarsByDirectScope.size())), pTCContext->pWorker);
        Assert_(vecOnHoldHeadIndex.size() + vecLeafHeadIndex.size() + vecOtherHeadIndex.size() == vecStdEdgeIndex.size());
    }
    #endif

    __declspec(align(16)) struct TmpBlockReduce {
        u32 uIncomingCount;
        u32 uPrimTarget;
        u32 uSecoTarget;
        u32 uBranchInstruction;
        u32 uFlags;
        u32 _pad0;
    };

    // Direct correspondance with raw block index + we'll put a virtual block for holding tight as a proc-entry, at the end
    u32 uBlockReduceArraySize = uRawBlockCount+1u;
    //
    // TODO: CLEANUP: do we really need that concept of a "virtual block holding tight as a proc-entry, at the end", when we could have
    // used a more 'concrete' initial block spanning the actual prolog (ensured non-empty, by virtue of the added 'NOOP' in empty cases).
    // currently we're just 'skipping' that prolog entirely on raw graph itself.
    //
    TmpBlockReduce* tBlockReduce = (TmpBlockReduce*)alloc_from(pTCContext->pWorker->tmpArena,
        sizeof(TmpBlockReduce)*uBlockReduceArraySize, alignof(TmpBlockReduce));
    TmpArray<u32> vecModifiableBlockReduce(pTCContext->pWorker->tmpArena);
    vecModifiableBlockReduce.reserve(uBlockReduceArraySize);
    for (u32 uBlock = 0u; uBlock < uRawBlockCount; uBlock++) {
        TmpBlockReduce& blockReduceToInit = tBlockReduce[uBlock];
        const IRBlock& rawBlock = rawBlocks[uBlock];
        blockReduceToInit.uPrimTarget = rawBlock.uNextBlockGotoOrFallthrough;
        blockReduceToInit.uSecoTarget = rawBlock.uNextBlockByBranching;
        blockReduceToInit.uFlags = rawBlock.uFlags;
        blockReduceToInit.uIncomingCount = tVisitInfo[uBlock].uIncomingCount;
        if (blockReduceToInit.uIncomingCount) {
            blockReduceToInit.uFlags |= IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE;
            vecModifiableBlockReduce.append(uBlock);
            Assert_(blockReduceToInit.uPrimTarget != IRGRAPH_ERR_INDEX);
            if (blockReduceToInit.uSecoTarget == IRGRAPH_ERR_INDEX)
                blockReduceToInit.uFlags |= IRGRAPH_FLAG_REDUCE_EXIT_HAS_ERRCHK;
        }
    }
    u32 uLastBlock = uRawBlockCount - 1u;
    for (u32 uBlock = 0u; uBlock < uLastBlock; uBlock++) {
        // if there is a branch or errchk or ret instruction, then it is last in block => its pos equals to the pos before the start of next raw block
        tBlockReduce[uBlock].uBranchInstruction = rawBlocks[uBlock+1u].uInstrStart - 1u;  // if there isn't, well... we don't care => do same.
    }
    // if there is a branch or errchk or ret instruction, then it is last in block => pos for last block is last instruction in IR for whole proc.
    tBlockReduce[uLastBlock].uBranchInstruction = pRepo->uSize - 1u; // if there isn't, well... we don't care => do same.
    // Now special init of the block-mock for proc entry, at the end of array:
    tBlockReduce[uRawBlockCount].uIncomingCount = 1u; // will stay at 1 whatever happens during reduction (and that's the point of it being there).
    tBlockReduce[uRawBlockCount].uPrimTarget = 0u; // the default proc-entry is always raw block 0
    tBlockReduce[uRawBlockCount].uSecoTarget = IRGRAPH_INVALID_INDEX;
    tBlockReduce[uRawBlockCount].uBranchInstruction = 0u; // this one is for the proc-entry block mock => nothing (atm)
    tBlockReduce[uRawBlockCount].uFlags = IRGRAPH_FLAG_CONTAINS_OTHER_INSTRUCTIONS|IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE;
    vecModifiableBlockReduce.append(uRawBlockCount);

    while (true) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "block-reduce iteration over %u blocks still potentially visited (including proc-start mock)", vecModifiableBlockReduce.size()), pTCContext->pWorker);

        bool bRetrySame = false;
        bool bDidSomeModification = false;
        for (   u32 uBlockIndexInVecModifiable = 0u; uBlockIndexInVecModifiable < vecModifiableBlockReduce.size(); 
                uBlockIndexInVecModifiable = bRetrySame ? uBlockIndexInVecModifiable : uBlockIndexInVecModifiable+1u) {
            bDidSomeModification |= bRetrySame;
            bRetrySame = false;
            u32 uBlockIndex = vecModifiableBlockReduce[uBlockIndexInVecModifiable];
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                "Considering block %u", u64(uBlockIndex)), pTCContext->pWorker);
            TmpBlockReduce& blockReduceToConsider = tBlockReduce[uBlockIndex];
            u32 uSelfReferencesCount = 0u;
            if (blockReduceToConsider.uPrimTarget == uBlockIndex)
                uSelfReferencesCount++;
            if (blockReduceToConsider.uSecoTarget == uBlockIndex)
                uSelfReferencesCount++;
            if (blockReduceToConsider.uIncomingCount == uSelfReferencesCount) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "block %u has no more incoming count (apart from its %u self-references). Removing it",
                    u64(uBlockIndex), u64(uSelfReferencesCount)), pTCContext->pWorker);
                // no one references us any more => decrements count to all our targets, without doubt, then remove ourselves from active (or modifiable).
                if (blockReduceToConsider.uPrimTarget < IRGRAPH_START_SPECIAL && blockReduceToConsider.uPrimTarget != uBlockIndex) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Also decrementing its primary target %u", u64(blockReduceToConsider.uPrimTarget)), pTCContext->pWorker);
                    Assert_(tBlockReduce[blockReduceToConsider.uPrimTarget].uIncomingCount);
                    tBlockReduce[blockReduceToConsider.uPrimTarget].uIncomingCount--;
                    bDidSomeModification = true;
                }
                if (blockReduceToConsider.uSecoTarget < IRGRAPH_START_SPECIAL && blockReduceToConsider.uSecoTarget != uBlockIndex) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Also decrementing its secondary target %u", u64(blockReduceToConsider.uPrimTarget)), pTCContext->pWorker);
                    Assert_(tBlockReduce[blockReduceToConsider.uSecoTarget].uIncomingCount);
                    tBlockReduce[blockReduceToConsider.uSecoTarget].uIncomingCount--;
                    bDidSomeModification = true;
                }
                blockReduceToConsider.uFlags &= ~IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE;
                bDidSomeModification = true; // otherwise, remove by endswap could skip someone which still has to be checked...
                vecModifiableBlockReduce.remove_by_endswap_at(uBlockIndexInVecModifiable);
                continue;
            }

            if (blockReduceToConsider.uFlags & IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE) {
                if (blockReduceToConsider.uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                    u32 uTarget = (blockReduceToConsider.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                        blockReduceToConsider.uPrimTarget : blockReduceToConsider.uSecoTarget;
                    Assert_(uTarget != IRGRAPH_INVALID_INDEX);
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "block %u has a single, ensured target to %u",
                        u64(uBlockIndex), u64(uTarget)), pTCContext->pWorker);
                    if (uTarget >= IRGRAPH_START_SPECIAL || uTarget == uBlockIndex) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "block %u single ensured target is a special or self reference => cannot reduce further",
                            u64(uBlockIndex)), pTCContext->pWorker);
                        // ensured target is special (or self) => cannot reduce any further
                        blockReduceToConsider.uFlags &= ~IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE;
                        continue;
                    }
                    // otherwise, we check status of our target...
                    TmpBlockReduce& target = tBlockReduce[uTarget];
                    Assert_(target.uIncomingCount);
                    if (0u == (target.uFlags & IRGRAPH_NON_SKIPPABLE_INSTRUCTIONS_FLAGS)) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "block %u single ensured target is fully skippable => swallowing its exit status (%u, %u) as ours",
                            u64(uBlockIndex), u64(target.uPrimTarget), u64(target.uSecoTarget)), pTCContext->pWorker);
                        // this ensured target has a fully skippable body => we take its whole end-specs as our own, no question asked.
                        blockReduceToConsider.uPrimTarget = target.uPrimTarget;
                        blockReduceToConsider.uSecoTarget = target.uSecoTarget;
                        blockReduceToConsider.uBranchInstruction = target.uBranchInstruction;
                        blockReduceToConsider.uFlags &= 0x0FFF'FFFFu;
                        blockReduceToConsider.uFlags |= (target.uFlags & 0xF000'0000u);
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "... and decrementing target %u current incoming count of %u to %u, since we no longer pass through it",
                            u64(uTarget), u64(target.uIncomingCount), u64(target.uIncomingCount-1u)), pTCContext->pWorker);
                        // and we now fully skip over it...
                        target.uIncomingCount--;
                        bRetrySame = true;
                        // however, we need not forget to increase count to our new targets !!!!
                        if (blockReduceToConsider.uPrimTarget < IRGRAPH_START_SPECIAL) {
                            tBlockReduce[blockReduceToConsider.uPrimTarget].uIncomingCount++;
                        }
                        if (blockReduceToConsider.uSecoTarget < IRGRAPH_START_SPECIAL) {
                            tBlockReduce[blockReduceToConsider.uSecoTarget].uIncomingCount++;
                        }
                        continue; 
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "block %u single ensured target %u is not skippable => we're not reducible any more",
                            u64(uBlockIndex), u64(uTarget)), pTCContext->pWorker);
                        // this ensured target has a non-skippable body => in theory, we are no longer modifiable.
                        blockReduceToConsider.uFlags &= ~IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE;
                        continue;
                    }
                }
                if (blockReduceToConsider.uPrimTarget == blockReduceToConsider.uSecoTarget) {
                    Assert_(blockReduceToConsider.uPrimTarget != IRGRAPH_INVALID_INDEX);
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "block %u now has a branching condition to exact same target %u => converting that to a single, ensured target",
                        u64(uBlockIndex), u64(blockReduceToConsider.uPrimTarget)), pTCContext->pWorker);
                    // our path as conditional, but by block reduction, we now have same target on branching
                    // (eg, branching over two detected skippable path, then joining again).
                    Assert_(blockReduceToConsider.uPrimTarget != IRGRAPH_ERR_INDEX); // such case should never be an exit to err-handler...
                    blockReduceToConsider.uSecoTarget = IRGRAPH_INVALID_INDEX;
                    blockReduceToConsider.uFlags |= IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN;
                    bRetrySame = true;
                    continue;
                }
                Assert_(blockReduceToConsider.uPrimTarget != IRGRAPH_INVALID_INDEX);
                Assert_(blockReduceToConsider.uSecoTarget != IRGRAPH_INVALID_INDEX);
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "block %u still has two active targets: %u and %u",
                    u64(uBlockIndex), u64(blockReduceToConsider.uPrimTarget), u64(blockReduceToConsider.uSecoTarget)), pTCContext->pWorker);
                bool bPrimTargetUnmodifiable = false;
                bool bSecoTargetUnmodifiable = false;
                if (blockReduceToConsider.uPrimTarget < IRGRAPH_START_SPECIAL && blockReduceToConsider.uPrimTarget != uBlockIndex) {
                    TmpBlockReduce& target = tBlockReduce[blockReduceToConsider.uPrimTarget];
                    if (0u == (target.uFlags & IRGRAPH_NON_SKIPPABLE_INSTRUCTIONS_FLAGS)) {
                        // target on our primary branch has a skippable body... if moreover, it has itself an ensured target:
                        if (target.uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                            // ...then we can take its own ensured target as primary...
                            u32 uTargetOfTarget = (target.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                                target.uPrimTarget : target.uSecoTarget;
                            Assert_(uTargetOfTarget != IRGRAPH_INVALID_INDEX);
                            if (uTargetOfTarget != IRGRAPH_ERR_INDEX) { // target being an err-check path would also prevent the following reduce.
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                    "Prim target of block %u is block %u: recognized as a fully skippable block with an ensured, non-err exit %u",
                                    u64(uBlockIndex), u64(blockReduceToConsider.uPrimTarget), u64(uTargetOfTarget)), pTCContext->pWorker);
                                blockReduceToConsider.uPrimTarget = uTargetOfTarget;
                                // we now fully skip over it...
                                target.uIncomingCount--;
                                bRetrySame = true;
                                // however, we need not forget to increase count to our new target !!!!
                                if (uTargetOfTarget < IRGRAPH_START_SPECIAL) {
                                    tBlockReduce[uTargetOfTarget].uIncomingCount++;
                                }
                                continue; 
                            } else
                                bPrimTargetUnmodifiable = true;
                        } else
                            bPrimTargetUnmodifiable = (0u == (target.uFlags & IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE));
                    } else 
                        bPrimTargetUnmodifiable = true;
                } else
                    bPrimTargetUnmodifiable = true;

                if (blockReduceToConsider.uSecoTarget < IRGRAPH_START_SPECIAL && blockReduceToConsider.uSecoTarget != uBlockIndex) {
                    Assert_(blockReduceToConsider.uPrimTarget != IRGRAPH_ERR_INDEX); // errchk should never be prim...
                    TmpBlockReduce& target = tBlockReduce[blockReduceToConsider.uSecoTarget];
                    if (0u == (target.uFlags & IRGRAPH_NON_SKIPPABLE_INSTRUCTIONS_FLAGS)) {
                        // target on our secondary branch has a skippable body... if moreover, it has itself an ensured target:
                        if (target.uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                            // ...then we can take its own ensured target as primary...
                            u32 uTargetOfTarget = (target.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                                target.uPrimTarget : target.uSecoTarget;
                            Assert_(uTargetOfTarget != IRGRAPH_INVALID_INDEX);
                            if (uTargetOfTarget != IRGRAPH_ERR_INDEX) { // target being an err-check path would prevent the following reduce.
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                                    "Seco target of block %u is block %u: recognized as a fully skippable block with an ensured, non-err exit %u",
                                    u64(uBlockIndex), u64(blockReduceToConsider.uSecoTarget), u64(uTargetOfTarget)), pTCContext->pWorker);
                                blockReduceToConsider.uSecoTarget = uTargetOfTarget;
                                // we now fully skip over it...
                                target.uIncomingCount--;
                                bRetrySame = true;
                                // however, we need not forget to increase count to our new target !!!!
                                if (uTargetOfTarget < IRGRAPH_START_SPECIAL) {
                                    tBlockReduce[uTargetOfTarget].uIncomingCount++;
                                }
                                continue;
                            } else
                                bSecoTargetUnmodifiable = true;
                        } else
                            bSecoTargetUnmodifiable = (0u == (target.uFlags & IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE));
                    } else
                        bSecoTargetUnmodifiable = true;
                } else
                    bSecoTargetUnmodifiable = true;

                if (bPrimTargetUnmodifiable && bSecoTargetUnmodifiable) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Block %u had no possible modifications on either prim nor seco => marking as no longer modifiable",
                        u64(uBlockIndex)), pTCContext->pWorker);
                    blockReduceToConsider.uFlags &= ~IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "No modification performed on block %u, but was undecided wether still modifiable",
                        u64(uBlockIndex)), pTCContext->pWorker);
                }
            } else {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Block %u is marked as no longer modifiable => skipping it", u64(uBlockIndex)), pTCContext->pWorker);
            }
        }

        if (!bDidSomeModification)
            break;
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Done checking for block reductions, from original %u raw blocks (%u active) with %u active edges : ",
            u64(uRawBlockCount), u64(uActiveRawBlocksCount), u64(vecStdEdgeIndex.size())), pTCContext->pWorker);

    u32 uStillVisitedBlocks = 0u;
    u32 uStdEdgesCount = 0u;
    u32 uRetEdgesCount = 0u;
    u32 uErrEdgesCount = 0u;
    u32 uDualStandardCount = 0u;
    u32 uDualWithErrCount = 0u;
    u32 uDualWithRetCount = 0u;
    u32 uDualErrVsRetCount = 0u;
    u32 uBlocksWithMultiIncomingCount = 0u;
    u32 uBlocksWithEnsuredErrCount = 0u;
    for (u32 uBlock = 0u; uBlock < uRawBlockCount; uBlock++) {
        TmpBlockReduce& blockReduce = tBlockReduce[uBlock];
        //Assert_(0u == (blockReduce.uFlags & IRGRAPH_FLAG_STILL_MODIFIABLE_REDUCE)); // we remove this assert: maybe this is too much to ask in the general case
        if (blockReduce.uIncomingCount) {
            uStillVisitedBlocks++;
            if (blockReduce.uIncomingCount > 1u)
                uBlocksWithMultiIncomingCount++;
            if (blockReduce.uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                u32 uTarget = (blockReduce.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                            blockReduce.uPrimTarget : blockReduce.uSecoTarget;
                Assert_(uTarget != IRGRAPH_INVALID_INDEX);
                if (uTarget < IRGRAPH_START_SPECIAL) {
                    Assert_(tBlockReduce[uTarget].uIncomingCount);
                    uStdEdgesCount++;
                } else if (uTarget == IRGRAPH_RET_INDEX) {
                    uRetEdgesCount++;
                } else { Assert_(uTarget == IRGRAPH_ERR_INDEX);
                    uErrEdgesCount++;
                    uBlocksWithEnsuredErrCount++;
                }
            } else {
                Assert_(blockReduce.uPrimTarget != IRGRAPH_INVALID_INDEX);
                Assert_(blockReduce.uSecoTarget != IRGRAPH_INVALID_INDEX);
                if (blockReduce.uPrimTarget < IRGRAPH_START_SPECIAL) {
                    uStdEdgesCount++;
                    if (blockReduce.uSecoTarget < IRGRAPH_START_SPECIAL) {
                        uStdEdgesCount++;
                        uDualStandardCount++;
                    } else if (blockReduce.uSecoTarget == IRGRAPH_RET_INDEX) {
                        uRetEdgesCount++;
                        uDualWithRetCount++;
                    } else { Assert_(blockReduce.uSecoTarget == IRGRAPH_ERR_INDEX);
                        uErrEdgesCount++;
                        uDualWithErrCount++;
                    }
                } else { Assert_(blockReduce.uPrimTarget == IRGRAPH_RET_INDEX); // prims should not be errs...
                    uRetEdgesCount++;
                    if (blockReduce.uSecoTarget < IRGRAPH_START_SPECIAL) {
                        uStdEdgesCount++;
                        uDualWithRetCount++;
                    } else { Assert_(blockReduce.uSecoTarget == IRGRAPH_ERR_INDEX); // prim should not equal seco
                        uErrEdgesCount++;
                        uDualErrVsRetCount++;
                    }
                }
            }
        }
    }

    Assert_(uStillVisitedBlocks + 1u == vecModifiableBlockReduce.size()); // +1 for mock
    if (uStillVisitedBlocks == 0u) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Done trimming raw graph. No more resulting Block (entry point will do a ret and/or errchk)"), pTCContext->pWorker);
    } else {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Done trimming raw graph. %u Still visited blocks, with %u standard edges, and %u distinct return points",
            u64(uStillVisitedBlocks), u64(uStdEdgesCount), u64(uRetEdgesCount)), pTCContext->pWorker);
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "MultiIncoming blocks : %u, DualOutgoing Standard : %u, DualOutgoingWithRet : %u",
                u64(uBlocksWithMultiIncomingCount), u64(uDualStandardCount), u64(uDualWithRetCount)), pTCContext->pWorker);
        }
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Ensured errs (maybe on some branch still): %u, Errchecks (only those taken for raw graph): %u, DualRetVsErr : %u",
                u64(uBlocksWithEnsuredErrCount), u64(uDualWithErrCount), u64(uDualErrVsRetCount)), pTCContext->pWorker);
        }
    }

    GraphResult result;

    result.vecRawBlocks = rawBlocks;
    result.vecFinalBlocks.init(resultArena);
    result.vecFinalBlocks.reserve(uStillVisitedBlocks + 1u); // +1 for proc entry
    result.vecScopeStacksAll = vecScopeStacksAll;
    result.vecLiveAndDeadInSameBlock = vecLiveAndDeadInSameBlock;

    u32* tReduceOrRawToFinal = (u32*)alloc_from(pTCContext->pWorker->tmpArena, sizeof(u32)*uRawBlockCount, alignof(u32));
    for (u32 uRaw = 0u; uRaw < uRawBlockCount; uRaw++)
        tReduceOrRawToFinal[uRaw] = IRGRAPH_INVALID_INDEX;

    IRTrimmedBlock procEntry;
    procEntry.uNextBlockGotoOrFallthrough = tBlockReduce[uRawBlockCount].uPrimTarget;
    procEntry.uNextBlockByBranching = tBlockReduce[uRawBlockCount].uSecoTarget;
    procEntry.uBranchingInstr = tBlockReduce[uRawBlockCount].uBranchInstruction;
    procEntry.uFirstInstrForComputingBranch = tBlockReduce[uRawBlockCount].uBranchInstruction; // TODO ?
    procEntry.uFirstSignificantInstr = 0u;
    procEntry.uLastSignificantInstr = 0u;
    procEntry.uIncomingLiveScopeCount = 0u;
    procEntry.uIncomingLiveScopeIndex = 0u;
    procEntry.uLiveAndDeadCount = 0u;
    procEntry.uLiveAndDeadIndex = 0u;
    procEntry.uFinalFlags = 0u;
    procEntry.uOriginRawBlock = IRGRAPH_INVALID_INDEX;
    result.vecFinalBlocks.append(procEntry);

    for (u32 uRaw = 0u; uRaw < uRawBlockCount; uRaw++) {
        if (tBlockReduce[uRaw].uIncomingCount) {
            IRTrimmedBlock newStillActiveBlockEntry;
            newStillActiveBlockEntry.uNextBlockGotoOrFallthrough = tBlockReduce[uRaw].uPrimTarget;
            newStillActiveBlockEntry.uNextBlockByBranching = tBlockReduce[uRaw].uSecoTarget;
            newStillActiveBlockEntry.uBranchingInstr = tBlockReduce[uRaw].uBranchInstruction;
            newStillActiveBlockEntry.uFirstInstrForComputingBranch = tBlockReduce[uRaw].uBranchInstruction; // TODO ?
            newStillActiveBlockEntry.uFirstSignificantInstr = rawBlocks[uRaw].uInstrStart; // TODO: find trim further after
            u32 uNextRaw = uRaw + 1u; // TODO: find trim further before
            newStillActiveBlockEntry.uLastSignificantInstr = (uNextRaw < uRawBlockCount) ? rawBlocks[uNextRaw].uInstrStart - 1u : pRepo->uSize - 1u;
            WalkingHead& refHead = tWalkingHeads[tVisitInfo[uRaw].uReferenceHeadIndex];
            if (refHead.uParentHead != IRGRAPH_INVALID_INDEX) {
                WalkingHead& refIncomingHead = tWalkingHeads[refHead.uParentHead];
                newStillActiveBlockEntry.uIncomingLiveScopeCount = refIncomingHead.uLiveScopeVectorCount;
                newStillActiveBlockEntry.uIncomingLiveScopeIndex = refIncomingHead.uLiveScopeVectorIndex;
                newStillActiveBlockEntry.uLiveAndDeadCount = tVisitInfo[uRaw].uLiveAndDeadCount;
                newStillActiveBlockEntry.uLiveAndDeadIndex = tVisitInfo[uRaw].uLiveAndDeadIndex;
            } else {
                newStillActiveBlockEntry.uIncomingLiveScopeCount = 0u;
                newStillActiveBlockEntry.uIncomingLiveScopeIndex = 0u;
                newStillActiveBlockEntry.uLiveAndDeadCount = 0u;
                newStillActiveBlockEntry.uLiveAndDeadIndex = 0u;
            }
            newStillActiveBlockEntry.uFinalFlags = 0u;
            if (tBlockReduce[uRaw].uIncomingCount > 1u)
                newStillActiveBlockEntry.uFinalFlags |= IRGRAPH_FINAL_FLAG_IS_MULTI_INCOMING;
            if (0u != (rawBlocks[uRaw].uFlags & IRGRAPH_NON_SKIPPABLE_INSTRUCTIONS_FLAGS))
                newStillActiveBlockEntry.uFinalFlags |= IRGRAPH_FINAL_FLAG_HAS_NON_SKIPPABLE;
            newStillActiveBlockEntry.uOriginRawBlock = uRaw;
            tReduceOrRawToFinal[uRaw] = result.vecFinalBlocks.size();
            result.vecFinalBlocks.append(newStillActiveBlockEntry);
        }
    }

    Assert_(result.vecFinalBlocks.size() == uStillVisitedBlocks + 1u); // +1 for proc entry

    for (u32 uFinal = 0u; uFinal < result.vecFinalBlocks.size(); uFinal++) {
        IRTrimmedBlock& finalBlock = result.vecFinalBlocks[uFinal];
        Assert_(finalBlock.uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX || finalBlock.uNextBlockByBranching != IRGRAPH_INVALID_INDEX);
        Assert_(finalBlock.uNextBlockGotoOrFallthrough != IRGRAPH_ERR_INDEX);
        Assert_(finalBlock.uNextBlockGotoOrFallthrough != finalBlock.uNextBlockByBranching);
        Assert_(finalBlock.uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX || finalBlock.uNextBlockByBranching == IRGRAPH_ERR_INDEX);
        if (finalBlock.uNextBlockGotoOrFallthrough == IRGRAPH_INVALID_INDEX || finalBlock.uNextBlockByBranching == IRGRAPH_INVALID_INDEX) {
            finalBlock.uFinalFlags |= IRGRAPH_FINAL_FLAG_HAS_NON_ENSURED_EXIT;
        }
        if (finalBlock.uNextBlockGotoOrFallthrough < IRGRAPH_START_SPECIAL) {
            Assert_(finalBlock.uNextBlockGotoOrFallthrough < uRawBlockCount);
            Assert_(tReduceOrRawToFinal[finalBlock.uNextBlockGotoOrFallthrough] != IRGRAPH_INVALID_INDEX);
            Assert_(tReduceOrRawToFinal[finalBlock.uNextBlockGotoOrFallthrough] > 0 && tReduceOrRawToFinal[finalBlock.uNextBlockGotoOrFallthrough] < result.vecFinalBlocks.size());
            finalBlock.uNextBlockGotoOrFallthrough = tReduceOrRawToFinal[finalBlock.uNextBlockGotoOrFallthrough];
        }
        if (finalBlock.uNextBlockByBranching < IRGRAPH_START_SPECIAL) {
            Assert_(finalBlock.uNextBlockByBranching < uRawBlockCount);
            Assert_(tReduceOrRawToFinal[finalBlock.uNextBlockByBranching] != IRGRAPH_INVALID_INDEX);
            Assert_(tReduceOrRawToFinal[finalBlock.uNextBlockByBranching] > 0 && tReduceOrRawToFinal[finalBlock.uNextBlockByBranching] < result.vecFinalBlocks.size());
            finalBlock.uNextBlockByBranching = tReduceOrRawToFinal[finalBlock.uNextBlockByBranching];
        }
    }

    reset_arena_no_release_to(beforeAll, pTCContext->pWorker->tmpArena);

    return result;
}

#if 0


struct IRBlockTag {
    u32 uCountIncoming;
    u32 uHotnessMaskAndFlags;
};

#define WAS_SCANNED_FLAG    0x8000'0000u
#define WAS_ADDED_FLAG      0x4000'0000u
#define IS_ENSURED_FLAG     0x2000'0000u

#define HOTNESS_MASKALL     0x1FFF'FFFFu
#define HOTNESS_DEFAULT     0x0000'8000u           // very hot, at proc start

// Scans some graph for finding which blocks are dead and which are live based on branching knowledge, also trying to colour path hotness,
// Returns result as a same-sized vector of "IRBlockTag" with that info, for each block
local_func TmpArray<IRBlockTag> tag_IR_graph_for_proc(TCProcBodyResult* pProc, TCContext* pTCContext, TmpArray<IRBlock>& vecBlocks, Arena tagArena)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Tagging IR graph for procbody %u (main id '%s') in file %s",
        u64(pProc->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProc->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pProgCompilationState->vecSourceFiles[u32(pProc->iSourceFileIndex)]->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(pProc);
    Assert_(0 == pProc->uIsForeignSource);
    IRRepo* pRepo = &(pProc->procwiseRepo);
    u32 uBlockCount = vecBlocks.size();
    Assert_(uBlockCount);
    TmpArray<IRBlockTag> vecTags(tagArena);
    vecTags.resize(uBlockCount);

    ArenaRefPoint beforeAll = get_arena_ref_point(pTCContext->pWorker->tmpArena);
    TmpArray<u32> vecBlocksRequiringScan(pTCContext->pWorker->tmpArena);
    vecTags[0u].uCountIncoming = 1u; // entry point of proc is always and shall always be block 0...
    vecTags[0u].uHotnessMaskAndFlags = HOTNESS_DEFAULT | WAS_ADDED_FLAG;
    vecBlocksRequiringScan.append(0u);

    while (vecBlocksRequiringScan.size()) {
        u32 uBlockRequiringScan = vecBlocksRequiringScan.pop_last();
        IRBlock& blockThere = vecBlocks[uBlockRequiringScan];
        IRBlockTag& tagThere = vecTags[uBlockRequiringScan];
        Assert_(0u == (tagThere.uHotnessMaskAndFlags & WAS_SCANNED_FLAG));
        Assert_(tagThere.uHotnessMaskAndFlags & WAS_ADDED_FLAG);
        u32 uHotnessThere = tagThere.uHotnessMaskAndFlags & HOTNESS_MASKALL;
        Assert_(uHotnessThere);
        Assert_(tagThere.uCountIncoming);
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
            "Scanning block %u for tags, with current known incoming %u, current known hotness 0x%08x",
                u64(uBlockRequiringScan), u64(tagThere.uCountIncoming), u64(uHotnessThere)), pTCContext->pWorker);
        tagThere.uHotnessMaskAndFlags |= WAS_SCANNED_FLAG;
        u32 uNextBlockGotoOrFallthrough = blockThere.uNextBlockGotoOrFallthrough;
        Assert_(uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX); // sure ??
        if (blockThere.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                "Block-ending for block %u is Primary-ensured to block (or special) %u",
                    u64(uBlockRequiringScan), u64(uNextBlockGotoOrFallthrough)), pTCContext->pWorker);
            Assert_(0u == (blockThere.uFlags & IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN));
            if (uNextBlockGotoOrFallthrough < IRGRAPH_START_SPECIAL) {
                IRBlockTag& primaryTagAsSingleTarget = vecTags[uNextBlockGotoOrFallthrough];
                primaryTagAsSingleTarget.uCountIncoming++;
                primaryTagAsSingleTarget.uHotnessMaskAndFlags |= uHotnessThere; // TODO: we need to further iterations on this...
                if (0u == (primaryTagAsSingleTarget.uHotnessMaskAndFlags & WAS_ADDED_FLAG)) {
                    primaryTagAsSingleTarget.uHotnessMaskAndFlags |= WAS_ADDED_FLAG;
                    vecBlocksRequiringScan.append(uNextBlockGotoOrFallthrough);
                }
            } // otherwise TODO ? what if err
        } else {
            u32 uNextBlockByBranching = blockThere.uNextBlockByBranching;
            Assert_(uNextBlockByBranching != IRGRAPH_INVALID_INDEX);
            if (blockThere.uFlags & IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Block-ending for block %u is Secondary-Branch-Ensured to block (or special) %u",
                        u64(uBlockRequiringScan), u64(uNextBlockByBranching)), pTCContext->pWorker);
                if (uNextBlockByBranching < IRGRAPH_START_SPECIAL) {
                    IRBlockTag& secondaryTagAsSingleTarget = vecTags[uNextBlockByBranching];
                    secondaryTagAsSingleTarget.uCountIncoming++;
                    // TODO: path hotness considerations ???
                    secondaryTagAsSingleTarget.uHotnessMaskAndFlags |= uHotnessThere; // TODO: we need to further iterations on this...
                    if (0u == (secondaryTagAsSingleTarget.uHotnessMaskAndFlags & WAS_ADDED_FLAG)) {
                        secondaryTagAsSingleTarget.uHotnessMaskAndFlags |= WAS_ADDED_FLAG;
                        vecBlocksRequiringScan.append(uNextBlockByBranching);
                    }
                } // otherwise TODO ? what if err

            } else { // primary or secondary...
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Block-ending for block %u is a two-option branch : fallthrough-or-default-to block %u, or branch to block (or special) %u",
                        u64(uBlockRequiringScan), u64(uNextBlockGotoOrFallthrough), u64(uNextBlockByBranching)), pTCContext->pWorker);
                Assert_(uNextBlockGotoOrFallthrough < IRGRAPH_START_SPECIAL);
                IRBlockTag& primaryPossible = vecTags[uNextBlockGotoOrFallthrough];
                primaryPossible.uCountIncoming++;
                // TODO: path hotness considerations ???
                primaryPossible.uHotnessMaskAndFlags |= uHotnessThere; // TODO: we need to further iterations on this...
                if (0u == (primaryPossible.uHotnessMaskAndFlags & WAS_ADDED_FLAG)) {
                    primaryPossible.uHotnessMaskAndFlags |= WAS_ADDED_FLAG;
                    vecBlocksRequiringScan.append(uNextBlockGotoOrFallthrough);
                }
                if (uNextBlockByBranching < IRGRAPH_START_SPECIAL) {
                    IRBlockTag& secondaryPossible = vecTags[uNextBlockByBranching];
                    secondaryPossible.uCountIncoming++;
                    // TODO: path hotness considerations ???
                    secondaryPossible.uHotnessMaskAndFlags |= uHotnessThere; // TODO: we need to further iterations on this...
                    if (0u == (secondaryPossible.uHotnessMaskAndFlags & WAS_ADDED_FLAG)) {
                        secondaryPossible.uHotnessMaskAndFlags |= WAS_ADDED_FLAG;
                        vecBlocksRequiringScan.append(uNextBlockByBranching);
                    }
                } // otherwise noop ok ?
            }
        }
    }

    reset_arena_no_release_to(beforeAll, pTCContext->pWorker->tmpArena);

    return vecTags;
}

local_func u32 get_resulting_block_index_after_fuse_and_sort_and_trim_from(u32 uOrigBlockIndex, 
    const u32* tBlocksOrigToResult, const TmpArray<u32>& vecIsSkippedResultAsIncoming, const u32* tSkipOffsets)
{
    u32 uIndexToLookup = uOrigBlockIndex;
    while (true) {
        u32 uIndexInTmpWithoutSkips = tBlocksOrigToResult[uIndexToLookup];
        u32 uIsSkippedThere = vecIsSkippedResultAsIncoming[uIndexInTmpWithoutSkips];
        if (uIsSkippedThere == IRGRAPH_INVALID_INDEX) {
            return uIndexInTmpWithoutSkips - tSkipOffsets[uIndexInTmpWithoutSkips];
        } else { // try again, once more through the loop, with uIsSkippedThere as new index to lookup
            uIndexToLookup = uIsSkippedThere; // in isskipped, if not invalid, we put orig index of block to where skip goes...
        }
    }
}

// Given a block graph and its associated block tag vector, will:
//   - retain only blocks which are actually possibly visited
//   - try to organize them so that "chainable" blocks (ensured fallthrough one to the other) are ordered and adjacent 
//   - try to merge blocks by removing all reference of any "chainable" block which has no flag in common with nonskippableflags
//   - try to organize them in hot to cold order
local_func TmpArray<IRBlock> fuse_and_sort_and_trim_blocks_inplace(TCProcBodyResult* pProc, TCContext* pTCContext,
                                                      TmpArray<IRBlock>& vecBlocks, TmpArray<IRBlockTag>& vecTags,
                                                      u32 uNonSkippableFlags, Arena blockArena)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED("Merging IR graph for procbody %u (main id '%s') in file %s",
        u64(pProc->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProc->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pProgCompilationState->vecSourceFiles[u32(pProc->iSourceFileIndex)]->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(pProc);
    Assert_(0 == pProc->uIsForeignSource);
    IRRepo* pRepo = &(pProc->procwiseRepo);
    u32 uTotalParams = get_total_param_count(pProc->procSign);
    u32 uInstructionCount = pRepo->uSize;
    u32 uBlockCount = 0u;
    Assert_(uInstructionCount > uTotalParams);

    u32 uOrigBlockCount = vecBlocks.size();
    Assert_(uOrigBlockCount);
    Assert_(uOrigBlockCount == vecTags.size());

    u32 uCurrentBlockOrig = 0;
    u32 uCurrentBlockResult = 0;
    Assert_(vecTags[0].uCountIncoming);
    Assert_(vecTags[0].uHotnessMaskAndFlags & HOTNESS_MASKALL);

    ArenaRefPoint beforeAll = get_arena_ref_point(pTCContext->pWorker->tmpArena);
    TmpArray<IRBlock> vecTmpSortedResults(pTCContext->pWorker->tmpArena);
    TmpArray<u32> vecIsSkippedResultAsIncoming(pTCContext->pWorker->tmpArena);
    TmpArray<u32> vecBlocksToConsiderForAddition(pTCContext->pWorker->tmpArena);
    vecBlocksToConsiderForAddition.append(0u);
    u32 uCountSkippedAsIncoming = 0u;
    u32* tBlocksOrigToResult = (u32*)alloc_from(pTCContext->pWorker->tmpArena, sizeof(u32)*uOrigBlockCount, alignof(u32));
    for (u32 uBlock = 0u; uBlock < uOrigBlockCount; uBlock++)
        tBlocksOrigToResult[uBlock] = IRGRAPH_INVALID_INDEX;

    u32 uCountSkippableExceptEndBranch = 0u;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
        "Original graph had %u total blocks, now reorganizing and fusing them, given tags", u64(uOrigBlockCount)), pTCContext->pWorker);

    while (vecBlocksToConsiderForAddition.size()) {
        uCurrentBlockOrig = vecBlocksToConsiderForAddition.pop_last();
        Assert_(vecTags[uCurrentBlockOrig].uCountIncoming);
        if (tBlocksOrigToResult[uCurrentBlockOrig] != IRGRAPH_INVALID_INDEX) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                "Orig block %u found in vec of blocks to handle, was already handled in another block chain. Skipping it.",
                    u64(uCurrentBlockOrig), u64(uCurrentBlockResult)), pTCContext->pWorker);
            continue;
        }

        uCurrentBlockResult = vecTmpSortedResults.size();
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
            "New primary path from Current orig block checked:%u, as tmp result index:%u",
                u64(uCurrentBlockOrig), u64(uCurrentBlockResult)), pTCContext->pWorker);
        vecTmpSortedResults.append(vecBlocks[uCurrentBlockOrig]);
        bool bAppendIncoming = true;
        u32 uSkippedAsIncoming = IRGRAPH_START_SPECIAL; // starting a new block-chain, we are still (potentially) skippable

        while (true) { // processes iteratively all chainable links right after current block
            Assert_(vecTmpSortedResults.size() <= uOrigBlockCount);
            Assert_(tBlocksOrigToResult[uCurrentBlockOrig] == IRGRAPH_INVALID_INDEX);
            tBlocksOrigToResult[uCurrentBlockOrig] = uCurrentBlockResult;
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                "Registering current orig block checked:%u, as tmp result index:%u",
                    u64(uCurrentBlockOrig), u64(uCurrentBlockResult)), pTCContext->pWorker);

            IRBlock& currentBlockResult = vecTmpSortedResults[uCurrentBlockResult];
            if (bAppendIncoming) { // We were not explicitely asked for skipping handling of 'incoming-block-skips' (could be the case if iterating after a merge)
                if (uSkippedAsIncoming != IRGRAPH_INVALID_INDEX) { // ...and we're still potentially skippable (first block in chain, or block before was also)
                    if (currentBlockResult.uFlags & uNonSkippableFlags) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                            "Incoming-Skip NoMore : Having checked for incoming-skippable result %u", u64(uCurrentBlockResult)), pTCContext->pWorker);
                        uSkippedAsIncoming = IRGRAPH_INVALID_INDEX;
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                            "Continue Incoming-Skip : Having checked for incoming-skippable result %u", u64(uCurrentBlockResult)), pTCContext->pWorker);
                        uCountSkippedAsIncoming++;
                    }
                }
                vecIsSkippedResultAsIncoming.append(uSkippedAsIncoming);
            } else {
                bAppendIncoming = true;
            }

            bool bEnsuredPath = false;
            u32 uNextBlockAsDefaultChained = IRGRAPH_INVALID_INDEX;

            if (currentBlockResult.uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) {
                u32 uPrimaryNextOrig = currentBlockResult.uNextBlockGotoOrFallthrough;
                Assert_(uPrimaryNextOrig != IRGRAPH_INVALID_INDEX);
                if (uPrimaryNextOrig < IRGRAPH_START_SPECIAL) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "EnsuredPrim: Trying to organize current result block %u, being orig block %u, as linked to default orig block %u",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uPrimaryNextOrig)), pTCContext->pWorker);
                    uNextBlockAsDefaultChained = uPrimaryNextOrig;
                    bEnsuredPath = true;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Special-Ensured-Prim: current result block %u, being orig block %u, has an ensured primary to a special %u => ending chain",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uPrimaryNextOrig)), pTCContext->pWorker);
                    // In case we end while being skippable, we'll leave a skip pointing to a special...
                    break;
                }
            } else if (currentBlockResult.uFlags & IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN) {
                u32 uSecondaryNextOrig = currentBlockResult.uNextBlockByBranching;
                Assert_(uSecondaryNextOrig != IRGRAPH_INVALID_INDEX);
                if (uSecondaryNextOrig < IRGRAPH_START_SPECIAL) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "EnsuredSeco: Trying to organize current result block %u, being orig block %u, as linked to default orig block %u",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uSecondaryNextOrig)), pTCContext->pWorker);
                    uNextBlockAsDefaultChained = uSecondaryNextOrig;
                    bEnsuredPath = true;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Special-Ensured-Seco: current result block %u, being orig block %u, has an ensured secondary to a special %u => ending chain",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uSecondaryNextOrig)), pTCContext->pWorker);
                    // In case we end while being skippable, we'll leave a skip pointing to a special...
                    break;
                }
            } else {
                u32 uPrimaryNextOrig = currentBlockResult.uNextBlockGotoOrFallthrough;
                Assert_(uPrimaryNextOrig != IRGRAPH_INVALID_INDEX);
                u32 uSecondaryNextOrig = currentBlockResult.uNextBlockByBranching;
                Assert_(uSecondaryNextOrig != IRGRAPH_INVALID_INDEX);
                if (uPrimaryNextOrig < IRGRAPH_START_SPECIAL) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "PotentialPrim : Trying to organize current result block %u, being orig block %u, as linked to default orig block %u",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uPrimaryNextOrig)), pTCContext->pWorker);
                    if (uSecondaryNextOrig < IRGRAPH_START_SPECIAL) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                            "Handling potential secondary path from current result block %u, being orig block %u, to orig block %u",
                                u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uSecondaryNextOrig)), pTCContext->pWorker);
                        Assert_(vecTags[uSecondaryNextOrig].uCountIncoming);
                        if (tBlocksOrigToResult[uSecondaryNextOrig] == IRGRAPH_INVALID_INDEX) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                                "Secondary Target block %u not yet handled => adding it to deferred vector",
                                    u64(uSecondaryNextOrig)), pTCContext->pWorker);
                            vecBlocksToConsiderForAddition.append(uSecondaryNextOrig);
                        } else {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                                "Secondary Target block %u already handled => NOOP",
                                    u64(uSecondaryNextOrig)), pTCContext->pWorker);
                            Assert_(vecTags[uSecondaryNextOrig].uCountIncoming > 1);
                        }
                    }
                    uNextBlockAsDefaultChained = uPrimaryNextOrig;

                } else if (uSecondaryNextOrig < IRGRAPH_START_SPECIAL) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "PotentialSeco : Trying to organize current result block %u, being orig block %u, as linked to default orig block %u",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig), u64(uSecondaryNextOrig)), pTCContext->pWorker);
                    uNextBlockAsDefaultChained = uSecondaryNextOrig;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "All-special-branches... edge case : current result block %u, being orig block %u, has only special for both default prim and branching seco => ending chain",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig)), pTCContext->pWorker);
                    // In case we end while being skippable, we'll surely have to roll-back the skip, to leave a trace of the branching itself...
                    break;
                }
            }

            Assert_(uNextBlockAsDefaultChained < IRGRAPH_START_SPECIAL);
            uCurrentBlockOrig = uNextBlockAsDefaultChained;
            if (uSkippedAsIncoming != IRGRAPH_INVALID_INDEX) {
                if (bEnsuredPath) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Current block specified for skipped as incoming, with ensured path to next => marking current block result %u as skippable incoming, as-if orig-block %u",
                            u64(uCurrentBlockResult), u64(uCurrentBlockOrig)), pTCContext->pWorker);
                    vecIsSkippedResultAsIncoming[uCurrentBlockResult] = uCurrentBlockOrig;
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Current block specified for skipped as incoming, without path-knowledge on exit => removing skippability of block-result %u, flagging it as trimmed-to-exit instead",
                            u64(uCurrentBlockResult)), pTCContext->pWorker);
                    currentBlockResult.uFlags |= IRGRAPH_FLAG_SKIPPABLE_EXCEPT_END_BRANCH;
                    uCountSkippableExceptEndBranch++;
                    vecIsSkippedResultAsIncoming[uCurrentBlockResult] = IRGRAPH_INVALID_INDEX;
                    uCountSkippedAsIncoming--;
                    uSkippedAsIncoming = IRGRAPH_INVALID_INDEX;
                }
            }
            if (tBlocksOrigToResult[uCurrentBlockOrig] == IRGRAPH_INVALID_INDEX) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                    "Target orig-block %u recognized as not yet handled",
                        u64(uCurrentBlockOrig)), pTCContext->pWorker);
                IRBlockTag& nextTag = vecTags[uCurrentBlockOrig];
                Assert_(nextTag.uCountIncoming);
                IRBlock& nextBlock = vecBlocks[uCurrentBlockOrig];
                if (bEnsuredPath && nextTag.uCountIncoming == 1u && (nextBlock.uFlags & uNonSkippableFlags) == 0u) { // Merge possible !
                    if (uSkippedAsIncoming == IRGRAPH_INVALID_INDEX) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                            "Mergeability by block-exit detected: Leaving current block result index %u inplace, "
                            " swallowing branch data from target-block %u",
                                u64(uCurrentBlockResult), u64(uCurrentBlockOrig)), pTCContext->pWorker);
                        currentBlockResult.uFlags &= 0x0FFF'FFFFu;      // keep only instruction flags, remove end-of-block branch flags
                        currentBlockResult.uFlags |= nextBlock.uFlags;  // add merged instruction flags, set end-of-block to merged branch flags
                        currentBlockResult.uFinalInstrEnd = nextBlock.uFinalInstrEnd;
                        currentBlockResult.uNextBlockGotoOrFallthrough = nextBlock.uNextBlockGotoOrFallthrough;
                        currentBlockResult.uNextBlockByBranching = nextBlock.uNextBlockByBranching;
                        bAppendIncoming = false;
                    } else {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                            "Invalidated mergeability of next-block, having a skippable current block => fallback to regular block-chain, from result %u to new target result %u (as orig target %u)",
                                u64(uCurrentBlockResult), u64(vecTmpSortedResults.size()), u64(uCurrentBlockOrig)), pTCContext->pWorker);
                        uCurrentBlockResult = vecTmpSortedResults.size();
                        vecTmpSortedResults.append(nextBlock);
                    }
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Handling block-exit as regular block-chain, from result %u to new target result %u (as orig target %u)",
                            u64(uCurrentBlockResult), u64(vecTmpSortedResults.size()), u64(uCurrentBlockOrig)), pTCContext->pWorker);
                    uCurrentBlockResult = vecTmpSortedResults.size();
                    vecTmpSortedResults.append(nextBlock);
                }
            } else {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                    "Target orig-block %u already handled => ending-chain",
                        u64(uCurrentBlockOrig)), pTCContext->pWorker);
                Assert_(vecTags[uCurrentBlockOrig].uCountIncoming > 1);
                // In case we end while being skippable, we'll possibly need to roll-back the skip in case of non-ensured path,
                //   or leave an ensured skip to a special...
                break;
            } 
        }

        if (uSkippedAsIncoming != IRGRAPH_INVALID_INDEX) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                "Double-Check Skippable: Ending a block-chain for block result %u, being orig block %u, while current is marked as skippable incoming",
                    u64(uCurrentBlockResult), u64(uCurrentBlockOrig)), pTCContext->pWorker);
            if (vecTmpSortedResults[uCurrentBlockResult].uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) { // ensured primary
                Assert_(vecTmpSortedResults[uCurrentBlockResult].uNextBlockGotoOrFallthrough != IRGRAPH_INVALID_INDEX);
                if (vecTmpSortedResults[uCurrentBlockResult].uNextBlockGotoOrFallthrough != IRGRAPH_ERR_INDEX) {
                    // regular jmp, or 'ret' instruction => can be skipped indeed.
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Current block-result %u specified for skipped as incoming, with ensured path (primary) to non-errchk path (ie, regular or ret) => left skipped ok",
                            u64(uCurrentBlockResult)), pTCContext->pWorker);
                    vecIsSkippedResultAsIncoming[uCurrentBlockResult] = uCurrentBlockOrig;
                    continue;
                } else { // Otherwise ensured branch to errchk. Left as-is, so that we get the pos of the errchk-ir instruction => Fallthrough
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Current block-result %u specified for skipped as incoming, to an ensured err-check (primary). Requires keeping err-check instruction proper",
                            u64(uCurrentBlockResult)), pTCContext->pWorker);
                }
            } else if (vecTmpSortedResults[uCurrentBlockResult].uFlags & IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN) { // ensured secondary
                Assert_(vecTmpSortedResults[uCurrentBlockResult].uNextBlockByBranching != IRGRAPH_INVALID_INDEX);
                if (vecTmpSortedResults[uCurrentBlockResult].uNextBlockByBranching != IRGRAPH_ERR_INDEX) {
                    // regular jmp, or 'ret' instruction => can be skipped indeed.
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Current block-result %u specified for skipped as incoming, with ensured path (secondary) to a non-errchk path (ie, regular or ret) => left skipped ok",
                            u64(uCurrentBlockResult)), pTCContext->pWorker);
                    vecIsSkippedResultAsIncoming[uCurrentBlockResult] = uCurrentBlockOrig;
                    continue;
                } else { // Otherwise ensured branch to errchk. Left as-is, so that we get the pos of the errchk-ir instruction => Fallthrough
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                        "Current block-result %u specified for skipped as incoming, to an ensured err-check (secondary). Requires keeping err-check instruction proper",
                            u64(uCurrentBlockResult)), pTCContext->pWorker);
                }
            } else {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                    "Current block-result %u specified for skipped as incoming, without path-knowledge on exit",
                        u64(uCurrentBlockResult)), pTCContext->pWorker);
            }
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                "Removing skippability of block-result %u, flagging it as trimmed-to-exit instead",
                    u64(uCurrentBlockResult)), pTCContext->pWorker);
            vecTmpSortedResults[uCurrentBlockResult].uFlags |= IRGRAPH_FLAG_SKIPPABLE_EXCEPT_END_BRANCH;
            uCountSkippableExceptEndBranch++;
            vecIsSkippedResultAsIncoming[uCurrentBlockResult] = IRGRAPH_INVALID_INDEX;
            uCountSkippedAsIncoming--;
            uSkippedAsIncoming = IRGRAPH_INVALID_INDEX;
        }
    }

    u32 uResultingTmpCount = vecTmpSortedResults.size();
    Assert_(uResultingTmpCount);
    Assert_(uResultingTmpCount == vecIsSkippedResultAsIncoming.size());
    Assert_(uCountSkippedAsIncoming <= uResultingTmpCount);

    u32* tSkipOffsets = (u32*)alloc_from(pTCContext->pWorker->tmpArena, sizeof(u32)*uResultingTmpCount, alignof(u32));

    /*
    if (uCountSkippableExceptEndBranch) {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
            "Additional pass for trying to remove-by-merge some of the %u blocks marked with trimmed-to-exit (from %u temporaries, %u marked as skippable)",
                u64(uCountSkippableExceptEndBranch), u64(uResultingTmpCount), u64(uCountSkippedAsIncoming)), pTCContext->pWorker);
        for (u32 uTmp = 0u; uTmp < uResultingTmpCount; uTmp++) {
            tSkipOffsets[uTmp] = 0u;
        }
        u32 uMergedCount = 0u;
        for (u32 uTmp = 0u; uTmp < uResultingTmpCount; uTmp++) {
            if (vecIsSkippedResultAsIncoming[uTmp] == IRGRAPH_INVALID_INDEX) {
                if (vecTmpSortedResults[uTmp].uFlags & (IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN|IRGRAPH_FLAG_SECONDARY_ENSURED_TAKEN)) {
                    u32 uEnsuredTarget = (vecTmpSortedResults[uTmp].uFlags & IRGRAPH_FLAG_PRIMARY_ENSURED_TAKEN) ?
                        vecTmpSortedResults[uTmp].uNextBlockGotoOrFallthrough : vecTmpSortedResults[uTmp].uNextBlockByBranching;
                    Assert_(uEnsuredTarget != IRGRAPH_INVALID_INDEX);
                    Assert_(uEnsuredTarget != IRGRAPH_START_SPECIAL);
                    if (uEnsuredTarget < IRGRAPH_START_SPECIAL) { // could be err index or ret index
                        Assert_(uEnsuredTarget < uOrigBlockCount);
                        u32 uToWhat = get_resulting_block_index_after_fuse_and_sort_and_trim_from(uEnsuredTarget,
                            tBlocksOrigToResult, vecIsSkippedResultAsIncoming, tSkipOffsets);
                        Assert_(uToWhat != IRGRAPH_INVALID_INDEX);
                        Assert_(uToWhat != IRGRAPH_START_SPECIAL);
                        Assert_(uToWhat != IRGRAPH_ERR_INDEX);
                        if (uToWhat < IRGRAPH_START_SPECIAL) { // could still be ret index
                            Assert_(uToWhat < uResultingTmpCount);
                            Assert_(vecIsSkippedResultAsIncoming[uToWhat] == IRGRAPH_INVALID_INDEX);
                            if (vecTmpSortedResults[uToWhat].uFlags & IRGRAPH_FLAG_SKIPPABLE_EXCEPT_END_BRANCH) {
                                vecTmpSortedResults[uTmp].uFinalInstrEnd = vecTmpSortedResults[uToWhat].uFinalInstrEnd;
                                vecTmpSortedResults[uTmp].uNextBlockGotoOrFallthrough = vecTmpSortedResults[uToWhat].uNextBlockGotoOrFallthrough;
                                vecTmpSortedResults[uTmp].uNextBlockByBranching = vecTmpSortedResults[uToWhat].uNextBlockByBranching;
                                vecTmpSortedResults[uTmp].uFlags &= (0x0FFF'FFFFu|IRGRAPH_FLAG_SKIPPABLE_EXCEPT_END_BRANCH);
                                vecTmpSortedResults[uTmp].uFlags |= (vecTmpSortedResults[uToWhat].uFlags & ~IRGRAPH_FLAG_SKIPPABLE_EXCEPT_END_BRANCH);
                                // TODO: decrement count and *actually* mark the trimmed as skippable if count reaches 0 ??
                                //    problem is that, count is on the original *tag* :(
                                uMergedCount++;
                            }
                        }
                    }
                }
            }
        }
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
            "The Additional pass managed to merge %u trimmed-to-exit blocks to their calling block ; new skippable count = %u (TODO)",
                u64(uMergedCount), u64(uCountSkippedAsIncoming)), pTCContext->pWorker);
    }
    */

    if (uCountSkippedAsIncoming == uResultingTmpCount) {
        // TODO !! keep only first block ?
        Assert(false, "case not yet implemented");
    }
    u32 uFinalCount = uResultingTmpCount - uCountSkippedAsIncoming;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Will be trimming %u raw blocks, currently as %u temporaries, to a final count of %u",
        u64(uOrigBlockCount), u64(uResultingTmpCount), u64(uFinalCount)), pTCContext->pWorker);

    u32 uCurrentSkipOffset = 0u;
    for (u32 uInTmp = 0u; uInTmp < uResultingTmpCount; uInTmp++) {
        tSkipOffsets[uInTmp] = uCurrentSkipOffset;
        Assert_(vecIsSkippedResultAsIncoming[uInTmp] != IRGRAPH_START_SPECIAL);
        Assert_(vecIsSkippedResultAsIncoming[uInTmp] != IRGRAPH_ERR_INDEX);
        if (vecIsSkippedResultAsIncoming[uInTmp] != IRGRAPH_INVALID_INDEX) {
            uCurrentSkipOffset++;
        }
    }

    // TODO: sorting by hotness !!!

    Assert_(uFinalCount);
    TmpArray<IRBlock> vecFinalResults(blockArena);
    vecFinalResults.reserve(uFinalCount);

    Assert_(0u == get_resulting_block_index_after_fuse_and_sort_and_trim_from(0u, // entry point of proc should stay block 0...
        tBlocksOrigToResult, vecIsSkippedResultAsIncoming, tSkipOffsets));

    for (u32 uInTmp = 0u; uInTmp < uResultingTmpCount; uInTmp++) {
        if (vecIsSkippedResultAsIncoming[uInTmp] == IRGRAPH_INVALID_INDEX) {
            IRBlock& currentTmpBlockResult = vecTmpSortedResults[uInTmp];
            u32 uFinalPos = vecFinalResults.size();
            Assert_(uFinalPos == uInTmp - tSkipOffsets[uInTmp]);
            vecFinalResults.append(currentTmpBlockResult);
            IRBlock& finalBlockResult = vecFinalResults[uFinalPos];
            u32 uPrimaryNextOrig = currentTmpBlockResult.uNextBlockGotoOrFallthrough;
            if (uPrimaryNextOrig < IRGRAPH_START_SPECIAL) {
                finalBlockResult.uNextBlockGotoOrFallthrough = get_resulting_block_index_after_fuse_and_sort_and_trim_from(uPrimaryNextOrig, 
                    tBlocksOrigToResult, vecIsSkippedResultAsIncoming, tSkipOffsets);
            }
            u32 uSecondaryNextOrig = currentTmpBlockResult.uNextBlockByBranching;
            if (uSecondaryNextOrig < IRGRAPH_START_SPECIAL) {
                finalBlockResult.uNextBlockByBranching = get_resulting_block_index_after_fuse_and_sort_and_trim_from(uSecondaryNextOrig, 
                    tBlocksOrigToResult, vecIsSkippedResultAsIncoming, tSkipOffsets);
            }
        }
    }

    Assert_(uFinalCount);
    Assert_(uFinalCount == vecFinalResults.size());

    reset_arena_no_release_to(beforeAll, pTCContext->pWorker->tmpArena);
    return vecFinalResults;

}

#endif

local_func void do_graph_passes_on_procbody(TCProcBodyResult* pProcBody, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Doing basic graph analysis passes on procbody %u (main id '%s') in file %s",
        u64(pProcBody->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProcBody->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pProgCompilationState->vecSourceFiles[u32(pProcBody->iSourceFileIndex)]->sourceFileName.c_str())), pTCContext->pWorker);

    TmpArray<IRBlock> rawGraph = build_IR_graph_for_proc(pProcBody, pTCContext, pTCContext->pIsolatedSourceFile->localArena, false);
    pProcBody->pGraphResult = (GraphResult*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, sizeof(GraphResult), alignof(GraphResult));
    *(pProcBody->pGraphResult) = walk_IR_graph_for_proc_trimming_nodes_and_gather_locals(pProcBody, pTCContext,
        rawGraph, pTCContext->pIsolatedSourceFile->localArena);

    /*
    TmpArray<IRBlockTag> rawTags = tag_IR_graph_for_proc(pProcBody, pTCContext, rawGraph, pTCContext->pIsolatedSourceFile->localArena);

    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Now performing liveliness analysis on graph"), pTCContext->pWorker);

        // TODO: build the first pass livelines analysis, based on *scope* on the untrimmed graph, holding all instructions,
        //   including local var decls and scope marks in particular.

        TmpArray<IRBlock> graphForLivelinessAnalysis = fuse_and_sort_and_trim_blocks_inplace(pProcBody, pTCContext, rawGraph, rawTags,
            IRGRAPH_FLAG_CONTAINS_TMP_RUNTIME|IRGRAPH_FLAG_CONTAINS_DIRECT_WRITES|IRGRAPH_FLAG_CONTAINS_OTHER_INSTRUCTIONS, 
            pTCContext->pIsolatedSourceFile->localArena);
    }
    */
}

#endif // LOCLIB_IR_BLOCK_GRAPH_H_

