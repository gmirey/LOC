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

#ifndef LOCLIB_ORCHESTRATOR_H_
#define LOCLIB_ORCHESTRATOR_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/ChunkProvider.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/HashSets.h"

#include "LocLib_Worker.h"
#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_ProgramState.h"
#include "LocLib_Program.h" // TODO? split up that "LocLib_Program.h" file into the following sub-parts at some point:
//#include "LocLib_ParserOrchestration.h"
//#include "LocLib_TypecheckerOrchestration.h"
//#include "LocLib_OptimizerOrchestration.h"
//#include "LocLib_BackendOrchestration.h"
#include "LocLib_IR_BlockGraph.h"


/*
    A General overview of LOC compiler organiation
    -------------------------------------------------

        When asked to compile a whole binary from source, the LOC compiler takes a single (first) file as its input.
        It will progressively discover all files through the various #load directives in the source, starting from this first file.

        There is one "struct" dealing with knowledge common to the whole process:
            WholeProgramCompilationState
        There is one "struct" per source file, dealing with all AST, Typechecker, and IR results from a single file:
            SourceFileDescAndState

        WholeProgramCompilationState, and every SourceFileDescAndState, each have their own, independent 'arena' for allocating
            most of their dynamic data, allowing them to almost never conflict for allocations and work independently.
            TODO: A multithreading *lock* on the ChunkProvider for source-files itself, still (even if maybe several of them to
            reduce this contention ? - Or we could also add some additional constraint that each source file is also assigned to
            a specific worker/thread, since these will probably be one distinct ChunkProvider per worker/thread anyway... which is maybe a
            not-so-bad strategy there...).

        -----------------------------------------
        Our task-schedule strategy is as follows:
        -----------------------------------------

        1 - Even in a multithreaded environment, there shall be a *single* worker allowed to do parsing, at a time.
            This *may* be a bottleneck, but since it is assumed that parsing in itself will always be only a few percents of the
            overall computational requirement for compiling any complex source base, we shall still do fine with it. Moreover, this
            decision interacts nicely with the following facts:
            
            * Our parser itself is not really in charge of accessing the actual source code - assumed on relatively slow disk storage -
                that may be a Platform-Layer/User-Interface decision ; typically, there is never a real speed gain (but often a speed
                penalty) in asking a drive to access multiple locations concurrently.
            * But even if we *could* read sources concurrently (or at least, arrange for sequential i/o reads but still concurrent
                parsing "treatments" of the resulting data in memory), there would be reasons against that: First, it is unclear
                at which point we could parse *slower* than the source file i/o can provide new data, so this multithreading effort
                may get wasted anyway. Second, ensuring a single parser at a time allow us to assume that a single point will ever
                emit new *identifiers*, which are interned in a map at parse time and subsequently only ever manipulated as fast
                integer indices, across the *whole* program: two identifiers are same if and only if they have same integer index,
                no matter in which file it was.

        2 - There can be as may additional, independent-thread workers, as there are possible runtime-concurrent threads on the
                compilation machine. Those workers will be assigned various *typechecking* or *optimiaztion* jobs, with the
                following constraints:

            * Only a single *typechecking* task shall ever be run at a time, within the boundaries of any given *source file*:
                This allows us to take many things for granted while working on the dynamic allocation data of a single file.
                All user types and procedure declarations, bindings, expression types and values results, and all resulting IR,
                can thus all use the source-file specific arena freely, without fear of multithreaded allocation or usage. All
                shared entities which could be manipulated across files are by-pointer (possibly stored in address-stable
                containers if need be).
            * The interaction points between possible tasks, and scheduler-specific problems to solve are better identified and
                isolated to a few, well-defined places. Some design decisions regarding the LOC language, and the treatment of
                bindings, shadowing rules, accessibility rules across files, etc. have in fact taken that specificity into account.
            * The optimization jobs, on the other end (graph-building, IR reorgs, even IR-tailoring-to-target-arch) may be allowed
                to run independently from the source-file themselves: so one worker at a time *per proc*, but not constrained that
                only a single of those workers is ever working on the procedures of a whole file: Their allocs shall use
                thread-independent arenas, specific to the worker in question... and once a worker is done doing that pass,
                its persistent allocated data is considered frozen (TODO: think about a way to release them still, if allowing many
                such passes on same proc)

        3 - Throughout the typechecker, there are well-defined paths which can halt the current typechecking task, waiting for some
            outside event to happen: future discovery of a binding (we have out of order declarations), finishing typechecking members of
            a struct type, or having gathered all identifiers from another source file, for example. When that happens, a worker actually
            will fallthrough the end of its current task, set it up as "waiting", and be available for another task. Our code paths from
            typechecking tasks are designed to take into account the eventuality of being in the process of *resuming* such a halted task.

        -------------------------------------------------
        Our multithreading locks are organized as follows:
        -------------------------------------------------

        1 - There is *no* lock on the programwise identifier-interning map. The map access itself is reserved to the parser and the
            scheduling algorithm. The identifier ids, once interned, are enough for most typechecking tasks. If anyone wants to query
            the string from its id, it is stored in an ensured stable-on-append vector, with a specific lock-free mechanism for both
            reads and appends. Any parser parses with knowledge of the programwise identifier-interning map (and associated vector)
            *at the time it started*... indeed currently, write-access to the stable-groing-vector is even deferred until after we parsed
            for the whole file, in the hope of speeding things up (slower write to stable than to the tmp vector)... but this would in fact
            need profiling [TODO. CLEANUP]. If, in the future, any typechecking task (macros ?) is ever able to spawn new identifiers
            (or get identifier ids based on string), this will be gated behind our 'wait' mechanisms, actually waiting for the parsing
            tasks to finish, and will be treated next, inbetween parsing tasks, never overlapping with them: Only one thing at a time
            shall be able to query or append to that map.

        2 - There is *no* lock on the source-file-specific data such as AST, binding maps, user procs and compound type declarations,
            value repositories, IR repositories, or more generally the local arena used for any associated data. The scheduling shall ensure
            that only one thread at a time can ever access the associated containers or modify such data.

        3 - There is a programwise global lock for keeping track of files having tasks to run, and scheduling those tasks. Once a worker
            is able to take on next typechecking tasks, it takes that lock, fast-decide on which file to work, flag it globally as being worked
            upon, and release that lock. Now, it knows it is sole controller of that file and associated data, and can take some further time
            to decide *which* task to actually carry on within those available for file... see 4.

        4 - There is a file specific task lock for each file. Some of the tasks require indeed to wait for an event *in another file* to occur.
            when that happens, it is at the end of a typechking task *for the other file* that we may be able to wake them. Since this involves
            two disctinct source files, we need an additional lock mechanism. All tasks which are not *waiting for identifiers* (see 5) and
            available or waiting to be resumed are thus stored in various containers whose access is behind that lock.

        5 - Tasks "waiting-for-identifiers", on the other hand, do not need such lock, since our strategy now finally ensures that they can only
            wait for them within the limits of a single file: We can wait for discovery of the binding associated with an identifier at global
            scope with out of order declarations, or even wait for discovery of the binding associated with an identifier *within* a
            currently-being-typechecked struct-like body further down the line, but anything involving bindings from other source files (through
            namespaces or struct-likes) are gated higher-up, waiting for the full resolution of the namespace or structlike beforehand.

        6 - There is a file specific *event lock* for each file: When a namespace, enum, structlike, or proc, is fully done with its job,
            we can change its TC state, and notify to all those tasks which were 'waiting' on that state that they may resume. However, when
            doing so, we need:
                a) to ensure that no-one can query our state and register as a waiter *while* we're in the process of gathering known
                   waiters and updating our state and waking them up.
                b) to ensure that we cannot deadlock between the end-of-task notification lock for a file, and a lock on other-file for
                   waking it up.
                c) to avoid having to go to the global lock too otfen or too long.
            => We thus have an event-specific lock, per source file, distinct from the task lock:
                * Whenever we need to actually update the TC status of a namespace/compound/proc, we need to *acquire* that lock
                * With the lock acquired, we gather the list of all waiters for that even *across distinct files*, then we change status and release.
                * We can fast-read completion *status* for availability or not *without* the lock (status update or query is behind atomic+fences), 
                    but to *act* upon non-completion by registering as a waiter we need to actually *acquire the lock*, *check again for completion*,
                    and if indeed not completed in the meantime, register as new waiter before releasing it.

        7 - Once a typechecking task is done, we thus need to do some book-keeping:
            * Internally gathering any task *on current file* which was waiting for newly discovered identifiers during that tc pass. Which
                involves accessing only those concerned through our 'waiting reason' maps, removing them from said waiting maps, and adding
                them to a temporary container of local tasks to wake at the end. Also flagging counts of waiting-for-id tasks to remove.
            * Signalling typechecking completion for namespaces, compound types, or even proc-bodies, to tasks within current files *and*
                in other source files. For that, we need to take a lock on the *events* for our source file, gather all waiters from
                    other source on a temporary container, update status, and close the lock.
                    Then, per other source file with waiting tasks:
                        We lock taks on *the other source*, remove those tasks from *waiting* and putting them in *ready to launch*,
                        and *iff increased from zero to non zero tasks to launch*, take a *second lock* on the programwise global
                        task-scheduler (ensured non-deadlocking as this is the only way and only order we can request
                        both at a time), and *iff non currently running TC on this file*, update the set of to-launch.
                        Then we release both locks.
                    Then, we also gather all waiters from *our* source file to completion, which can be done out of lock, before
                    finalization below:
            * Take a lock on *our* specific task lock, waking local-waiting-for-id and local-waiting-for-non-id by putting them in
                tasks to launch, take a *second lock* on the programwise global task-scheduler (ensured non-deadlocking as this is the only
                way and only order we can request both at a time), and signal and our changes, and in particular that there's no longer
                a *running* task on that file. Then we release both locks.

        8 - Although not really needed in the general case of a perfectly valid file to compile, we decide to take the hit of still 
            keeping a global container for tasks in waiting... and behind a lock at that. Now, when we get to the end of tasks to perform,
            we can check on this tasks-in-waiting container ; if there are any, well... it is likely that there will be errors, such as
            unresolved identifiers, that we need to report. But before going straight on to that:
                we may want to build a representation of namespaces and compounds and procs with still-waiting tasks.
                with that representation, if any entity (namespace/compound/proc) does *not* have 'using' in waiting,
                then its global-in-waiting are 'leaves':
                    -> we can report those errors as unresolved identifiers, then notify that entity as 'done'...
                    -> if that notification wakes up some tasks, then we can let the scheduler pursue as 'normal' until we get to that case again.
                    -> if, on the other hand, that notification wakes up nothing, then we try to find another of those leaves.
                if we run out of such 'leaves'... well, that would be weird ? maybe a circular ref somewhere, but it should have been caught by
                other means... in any case, we can maybe report all those unresolved 'waitings' as hard, unqualified errors.
            => We thus have an in-waiting-specific lock, global, distinct from the task-to-run lock.
            Note: if we ever need to update both at a time, then the task-to-run lock must be acquired *before* that one.

        9 - RECAP LOCKING ORDER:
            --------------------
                Worker File Scheduling:                         GLOBAL_TASK
                Worker Task Scheduling in file                  FILE_TASK(self)
                Waiting Task Registration (other)               FILE_EVENT(other)
                Waiting Task Registration (any)                 GLOBAL_WAITING
                Worker Done Entity                              FILE_EVENT(self)
                Worker Notify id or Done Entity (any)           GLOBAL_WAITING
                Worker Notify Done Entity (other)               FILE_TASK(other) > GLOBAL_TASK
                Worker Notify id or Done Entity (self)          FILE_TASK(self)  > GLOBAL_TASK
                Check Remaining Waiting                         GLOBAL_TASK      > GLOBAL_WAITING
                Error on Remaining Waiting                      GLOBAL_TASK      > FILE_TASK(one in err)... but ensured *sole* possible in flight.
*/

local_func_inl TracableEvent EventORCH_MAIN_FRONTEND_LOOP(u32 uTotalWorkerCount) {
    TracableEvent result = {};
    result.uEventType = EEvtORCH::EEVT_ORCH_MAIN_FRONTEND_LOOP;
    result.uEventPayload32 = uTotalWorkerCount;
    return result;
}
local_func void print_event_ORCH_MAIN_FRONTEND_LOOP_to(char* szBuffer, const TracableEvent& eventEntry) {
    Assert_(eventEntry.eLocPhase == ELOCPHASE_ORCHESTRATION);
    Assert_(eventEntry.uEventType == EEVT_ORCH_MAIN_FRONTEND_LOOP);
    sprintf(szBuffer, "%s with %u workers", tEvtORCHDbgStr[eventEntry.uEventType], eventEntry.uEventPayload32);
}

local_func_inl TracableEvent EventORCH_WORKER_ASSIGN(u16 uWorkerId, u8 uSysHour, u8 uSysMin, u8 uSysSec, u64 uSecondsElapsed, u32 uNanosecondsElapsed) {
    TracableEvent result = {};
    result.uEventType = EEvtORCH::EEVT_ORCH_WORKER_ASSIGN;
    result.uEventPayload32 = uNanosecondsElapsed;
    result.tPayload[0] = uSecondsElapsed;
    result.tPayload[1] = u64(uWorkerId) | (u64(uSysSec) << 16) | (u64(uSysMin) << 24) | (u64(uSysHour) << 32);
    return result;
}

local_func void print_event_ORCH_WORKER_ASSIGN_to(char* szBuffer, const TracableEvent& eventEntry) {
    Assert_(eventEntry.eLocPhase == ELOCPHASE_ORCHESTRATION);
    Assert_(eventEntry.uEventType == EEVT_ORCH_WORKER_ASSIGN);
    sprintf(szBuffer, "%s for worker %u, at %02u:%02u'%02u (%llu.%09u seconds from sys ref)", tEvtORCHDbgStr[eventEntry.uEventType],
        u16(eventEntry.tPayload[1]), u8(eventEntry.tPayload[1]>>32), u8(eventEntry.tPayload[1]>>24), u8(eventEntry.tPayload[1]>>16),
        eventEntry.tPayload[0], eventEntry.uEventPayload32);
}

local_func void handle_parsing_task(int iSourceFileIndexToParse, ParseFileTaskDescription taskDesc, WorkerDesc* pWorkerDesc,
    WholeProgramCompilationState* pCompState, LocLib_CompilationResults* oCompilationResults)
{
    Assert_(iSourceFileIndexToParse >= 0 && u32(iSourceFileIndexToParse) < pCompState->vecSourceFiles.size());
    on_set_newjob_to_worker(pWorkerDesc, u16(iSourceFileIndexToParse));

    // TODO: timing
    u8 uSysHour = 0u;
    u8 uSysMin = 0u;
    u8 uSysSec = 0u;
    u64 uSecondsSinceStart = 0uLL;
    u32 uNanoSecondsSinceStart = 0u;
    BLOCK_TRACE(ELOCPHASE_ORCHESTRATION, _LLVL2_IMPORTANT_INFO, EventORCH_WORKER_ASSIGN(
        pWorkerDesc->uLocThreadId, uSysHour, uSysMin, uSysSec, uSecondsSinceStart, uNanoSecondsSinceStart), pWorkerDesc);

    SourceFileDescAndState* pSourceFile = pCompState->vecSourceFiles[iSourceFileIndexToParse];

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Starting to parse source-file '%s'",
        reinterpret_cast<u64>(pSourceFile->sourceFileName.c_str())), pWorkerDesc);

    bool bOk = false;
    TokenizerClosure tokenizer;
    u16 uError = 0;
    if (pCompState->pOsFuncs->pGetSourceFileTokenizerFn(iSourceFileIndexToParse, &tokenizer, &uError)) {
        pCompState->pOsFuncs->pReleaseSourceFileReaderResourceFn();
        bOk = make_ast_for_source_file(iSourceFileIndexToParse, tokenizer,
            pCompState, pCompState->pOsFuncs, pCompState->pCompilationParams, oCompilationResults, pWorkerDesc);
        pCompState->pOsFuncs->pFreeSourceFileTokenizerFn(iSourceFileIndexToParse, &tokenizer);
    } else {
        Assert_(uError);
        Assert_(taskDesc.iIndexOfDemandingSourceFile >= 0 && taskDesc.iIndexOfDemandingSourceFile < pCompState->vecSourceFiles.size());
        SourceFileDescAndState* pFailedRequestOriginator = pCompState->vecSourceFiles[u32(taskDesc.iIndexOfDemandingSourceFile)];
        LocLib_Error retrieveTokenizerError = {};
        retrieveTokenizerError.errCode = uError;
        retrieveTokenizerError.uBlockOrLineIfScanErr = u32(taskDesc.uBlockInDemandingSourceFile);
        retrieveTokenizerError.uStatement = u32(taskDesc.uStatementInDemandingBlock);
        Assert_(i32(taskDesc.uBlockInDemandingSourceFile) < pFailedRequestOriginator->iBlockCount);
        AstBlock* pBlock = pFailedRequestOriginator->tBlocks + taskDesc.uBlockInDemandingSourceFile;
        Assert_(taskDesc.uStatementInDemandingBlock < pBlock->uStatementCount);
        AstStatement* pStatement = pBlock->tStatements + taskDesc.uStatementInDemandingBlock;
        Assert_(taskDesc.uNodeInDemandingStatement < pStatement->uNodeCount);
        AstNode* pNode = pStatement->tNodes + taskDesc.uNodeInDemandingStatement;
        retrieveTokenizerError.uTokenRef = pNode->uPivotalTokenPackedRef;
        pFailedRequestOriginator->vecErrors.append(retrieveTokenizerError);
        bOk = false;
    }

    ETaskPriority ePrio = ETASKPRIO_HIGHEST;
    if (bOk) {
        TCContext* pTCContextToPush = make_typecheck_task_for_whole_source_file(pSourceFile,
            pCompState, oCompilationResults);
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
            "Allocated new TC Task 0x%llx for typechecking whole source file", reinterpret_cast<u64>(pTCContextToPush)), pWorkerDesc);

        Assert_(pTCContextToPush->pIsolatedSourceFile == pSourceFile);
        Assert_(pTCContextToPush->pNamespace);
        Assert_(pTCContextToPush->pNamespace->pOriginalSourceFile == pSourceFile);
        Assert_(pTCContextToPush->pNamespace = pSourceFile->pRootNamespace);
        ePrio = pTCContextToPush->eTaskPrio;
        acquire_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
        pSourceFile->tvecTCTasksToLaunchByPrio[ePrio].append(pTCContextToPush);
        pSourceFile->pRootNamespace->uCountGlobalAccessibleTasks = 1u;
        release_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
    }

    acquire_global_task_launching_lock(pCompState, pWorkerDesc);
    if (bOk)
        pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].insert(iSourceFileIndexToParse);
    pCompState->setOfFilesWithActiveTasksBeingRun.remove_at_key(iSourceFileIndexToParse);
    release_global_task_launching_lock(pCompState, pWorkerDesc);
}

local_func void on_need_notify_identifiers_available_on_local_namespace_and_descendants(TCNamespace* pNamespace,
    SetView<int> setOfNewlyDeclaredIdentifiers, WholeProgramCompilationState* pCompState, WorkerDesc* pWorkerDesc,
    SourceFileDescAndState* pSourceFile, TmpArray<TCContext*>* outVecTasksToWake)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Notifying %u identifiers newly available for namespace %u of source file '%s'",
        u64(setOfNewlyDeclaredIdentifiers.size()), u64(pNamespace->uRegistrationIndex),
        reinterpret_cast<u64>(pSourceFile->sourceFileName.c_str())), pWorkerDesc);

    Assert_(pNamespace->pOriginalSourceFile == pSourceFile);
    if (pNamespace->mapTasksWaitingForGlobalIds.size()) {
        for (auto it = setOfNewlyDeclaredIdentifiers.begin(), itEnd = setOfNewlyDeclaredIdentifiers.end(); it != itEnd; it++) {
            TCWaitingReason waitingReason = make_waiting_reason(ETaskWaitingReason::EWR_GLOBAL_IDENTIFIER_RESOLUTION,
                WAITING_REASON_NO_SPECIFIC_INDEX, u32(*it));
            auto itWaiting = pNamespace->mapTasksWaitingForGlobalIds.find(waitingReason);
            if (itWaiting != pNamespace->mapTasksWaitingForGlobalIds.end()) {
                TmpArray<TCContext*>& vecTaskToWakeThere = itWaiting.value();
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Will wake %u task waiting for identifier '%s'", u64(vecTaskToWakeThere.size()),
                    reinterpret_cast<u64>(get_identifier_string(pCompState, *it).c_str())), pWorkerDesc);
                outVecTasksToWake->append_all(vecTaskToWakeThere);
                pNamespace->mapTasksWaitingForGlobalIds.remove_at_iter(itWaiting);
            } else {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "No task was waiting for identifier '%s'",
                    reinterpret_cast<u64>(get_identifier_string(pCompState, *it).c_str())), pWorkerDesc);
            }
        }
    } else {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
            "No task waiting for any identifier on this namespace"), pWorkerDesc);
    }
    u32 uChildrenCount = pNamespace->vecChildrenNamespaces.size();
    for (u32 uChild = 0u; uChild < uChildrenCount; uChild++) {
        TCNamespace* pChildNamespace = pSourceFile->vecNamespaces[uChild];
        on_need_notify_identifiers_available_on_local_namespace_and_descendants(pChildNamespace, setOfNewlyDeclaredIdentifiers,
            pCompState, pWorkerDesc, pSourceFile, outVecTasksToWake);
    }
}

local_func void on_can_notify_namepace_state_change(TCNamespace* pNamespace, ESourceCompState eNewCompState, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pNamespace == pTCContext->pNamespace);
    Assert_(pNamespace->pOriginalSourceFile == pTCContext->pIsolatedSourceFile);
    Assert_(eNewCompState != pNamespace->uTCProgress);
    
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Notifying state change on namespace %u source file '%s' : new state %u",
        u64(pNamespace->uRegistrationIndex),
        reinterpret_cast<u64>(pNamespace->pOriginalSourceFile->sourceFileName.c_str()),
        u64(eNewCompState)), pTCContext->pWorker);

    WRITE_FENCE();
    
    acquire_source_file_specific_event_lock(pNamespace->pOriginalSourceFile, pTCContext->pWorker);
    u32 uTasksCount = pNamespace->vecTasksWaitingForCompletion.size();
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "There were %u tasks waiting for that state (namespace : '%s')",
        u64(uTasksCount),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pNamespace->iPrimaryIdentifier).c_str())),
        pTCContext->pWorker);
    for (u32 uTask = 0u; uTask < uTasksCount; uTask++) {
        TCContext* pTask = pNamespace->vecTasksWaitingForCompletion[uTask];
        if (pTask->pIsolatedSourceFile == pTCContext->pIsolatedSourceFile)
            outVecLocalTasksToWake->append(pTask);
        else
            outVecNonLocalNonIdTasksToWake->append(pTask);
    }
    pNamespace->vecTasksWaitingForCompletion.clear();
    pNamespace->uTCProgress = eNewCompState;
    release_source_file_specific_event_lock(pNamespace->pOriginalSourceFile, pTCContext->pWorker);
}

local_func void on_can_notify_compound_state_change(TypeInfo_CompoundBase* pCompound, ECompoundTCProgress eNewCompoundState, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    TCCompoundRegistration* pRegistration = pCompound->pRegistration;
    Assert_(pRegistration->pSourceFile == pTCContext->pIsolatedSourceFile);
    Assert_(pRegistration == pTCContext->pCompoundToTC);
    Assert_(pCompound == pRegistration->pCompoundType);
    Assert_(eNewCompoundState != pRegistration->uTCProgress);
    
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Notifying state change on compound %u of source file '%s' : new state %u",
        u64(pCompound->uRegistrationIndex),
        reinterpret_cast<u64>(pRegistration->pSourceFile->sourceFileName.c_str()),
        u64(eNewCompoundState)), pTCContext->pWorker);

    WRITE_FENCE();
    
    acquire_source_file_specific_event_lock(pRegistration->pSourceFile, pTCContext->pWorker);
    u32 uTasksCount = pRegistration->vecTasksWaitingForCompletion.size();
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "There were %u tasks waiting for that state (compound : '%s')",
        u64(uTasksCount),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pRegistration->iPrimaryIdentifier).c_str())),
        pTCContext->pWorker);
    for (u32 uTask = 0u; uTask < uTasksCount; uTask++) {
        TCContext* pTask = pRegistration->vecTasksWaitingForCompletion[uTask];
        if (pTask->pIsolatedSourceFile == pTCContext->pIsolatedSourceFile)
            outVecLocalTasksToWake->append(pTask);
        else
            outVecNonLocalNonIdTasksToWake->append(pTask);
    }
    pRegistration->vecTasksWaitingForCompletion.clear();
    pRegistration->uTCProgress = eNewCompoundState;
    release_source_file_specific_event_lock(pRegistration->pSourceFile, pTCContext->pWorker);
}

local_func void on_can_notify_proc_state_change(TCProcBodyResult* pProcBody, EProcTypecheckingStatus eNewProcStatus, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pProcBody == pTCContext->pProcResult);
    Assert_(pProcBody->iSourceFileIndex == pTCContext->pIsolatedSourceFile->iRegistrationIndex);
    Assert_(eNewProcStatus != pProcBody->uProcBodyTypechekingStatus);
    
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Notifying state change on proc %u of source file '%s' : new state %u",
        u64(pProcBody->uRegistrationIndex),
        reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str()),
        u64(eNewProcStatus)), pTCContext->pWorker);

    WRITE_FENCE();
    
    acquire_source_file_specific_event_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
    u32 uTasksCount = pProcBody->vecTasksWaitingForCompletion.size();
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "There were %u tasks waiting for that state (proc : '%s')",
        u64(uTasksCount),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProcBody->iPrimaryIdentifier).c_str())),
        pTCContext->pWorker);
    for (u32 uTask = 0u; uTask < uTasksCount; uTask++) {
        TCContext* pTask = pProcBody->vecTasksWaitingForCompletion[uTask];
        if (pTask->pIsolatedSourceFile == pTCContext->pIsolatedSourceFile)
            outVecLocalTasksToWake->append(pTask);
        else
            outVecNonLocalNonIdTasksToWake->append(pTask);
    }
    pProcBody->vecTasksWaitingForCompletion.clear();
    pProcBody->uProcBodyTypechekingStatus = eNewProcStatus;
    release_source_file_specific_event_lock(pTCContext->pIsolatedSourceFile, pTCContext->pWorker);
}

local_func void after_task_on_global_tc(TCNamespace* pNamespace, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pNamespace == pTCContext->pNamespace);
    Assert_(pNamespace->pOriginalSourceFile == pTCContext->pIsolatedSourceFile);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Done with one global OutOfOrder task on namespace %u of source file '%s'",
        u64(pNamespace->uRegistrationIndex),
        reinterpret_cast<u64>(pNamespace->pOriginalSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    if (pTCContext->setOfNewlyDeclaredIdentifiers.size()) {
        on_need_notify_identifiers_available_on_local_namespace_and_descendants(pNamespace,
            pTCContext->setOfNewlyDeclaredIdentifiers, pTCContext->pProgCompilationState, pTCContext->pWorker, 
            pTCContext->pIsolatedSourceFile, outVecLocalTasksToWake);
    }

    if (pTCContext->eGlobalDeclScope == SCOPEKIND_GLOBAL_PRIVATE) {
        pNamespace->uCountGlobalPrivateTasks--;
    } else {
        pNamespace->uCountGlobalAccessibleTasks--;
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Remaining tasks in flight there : %u private, %u accessible",
        u64(pNamespace->uCountGlobalPrivateTasks), u64(pNamespace->uCountGlobalAccessibleTasks)), pTCContext->pWorker);

    if (pNamespace->uCountGlobalAccessibleTasks == 0u) {
        if (pNamespace->uCountGlobalPrivateTasks == 0u) {
            if (pNamespace->uTCProgress < ESourceCompState::ESOURCE_COMP_STATE_DONE_ALL_DISCOVERY) {
                on_can_notify_namepace_state_change(pNamespace, ESourceCompState::ESOURCE_COMP_STATE_DONE_ALL_DISCOVERY,
                    pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
            }
        } else {
            if (pNamespace->uTCProgress < ESourceCompState::ESOURCE_COMP_STATE_DONE_ACCESSIBLE_DISCOVERY) {
                on_can_notify_namepace_state_change(pNamespace, ESourceCompState::ESOURCE_COMP_STATE_DONE_ACCESSIBLE_DISCOVERY,
                    pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
            }
        } 
    }
}

local_func void after_task_on_structlike_tc(TypeInfo_StructLike* pStructLike, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pStructLike);
    Assert_(pTCContext->pCompoundToTC);
    Assert_(pStructLike == pTCContext->pCompoundToTC->pCompoundType);
    Assert_(pStructLike->pRegistration == pTCContext->pCompoundToTC);
    Assert_(pTCContext->pCompoundToTC->pSourceFile == pTCContext->pIsolatedSourceFile);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Done with one OutOfOrder task on structlike %u ('%s') of source file '%s'",
        u64(pStructLike->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pStructLike->pRegistration->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pStructLike->pRegistration->pSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    if (pTCContext->setOfNewlyDeclaredIdentifiers.size()) {
        // Note: those identifiers within structlike are *not* globally available per se, but, within current *namespace*,
        //   we'll also possibly find waiters *from within this very structlike* which want to know about our ids. => We'd wake all
        //   tasks waiting for those same identifiers, just in case. Worst which could happen is tasks that were actually waiting
        //   for same *global* ids will be put back to sleep. It shall become an err-case anyway because of our non-shadowing rules
        //   there... so this would be a 'cold' scenario.
        on_need_notify_identifiers_available_on_local_namespace_and_descendants(pTCContext->pNamespace,
            pTCContext->setOfNewlyDeclaredIdentifiers, pTCContext->pProgCompilationState, pTCContext->pWorker, 
            pTCContext->pIsolatedSourceFile, outVecLocalTasksToWake);
    }

    if (pTCContext->uFlags & CTXFLAG_CURRENT_TC_STRUCTCONST) {
        pStructLike->pRegistration->uCountEnsuredConstTasks--;
    } else {
        pStructLike->pRegistration->uCountPossiblyRuntimeTasks--;
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Remaining tasks in flight there : %u ensured const, %u possibly runtime",
        u64(pStructLike->pRegistration->uCountEnsuredConstTasks), u64(pStructLike->pRegistration->uCountPossiblyRuntimeTasks)), pTCContext->pWorker);

    // By default, no progress.
    ECompoundTCProgress progressToSignal = ECompoundTCProgress(pStructLike->pRegistration->uTCProgress);
    // Now, we'll try to refine that below:
    //

    if (pStructLike->pRegistration->uCountPossiblyRuntimeTasks == 0u) {
        // progressToSignal = ECOMPOUND_DONE_RUNTIME; // TODO: CLEANUP: notify runtime...
        if (pStructLike->pRegistration->uTCProgress < ECompoundTCProgress::ECOMPOUND_DONE_RUNTIME) {
            if (!try_finalize_structlike_runtime_tc(pStructLike, pTCContext)) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "*** Trying to finalize compound returned false!!"), pTCContext->pWorker);
                if (0u == (pStructLike->_coreFlags & COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME)) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "*** Warning : Trying to finalize compound returned false, without the compound being in error. "
                        "This case is not yet properly handled"),
                        pTCContext->pWorker);
                }
                progressToSignal = ECOMPOUND_DONE_ALL;
            }
        }
        if (pStructLike->pRegistration->uCountEnsuredConstTasks == 0u) {
            progressToSignal = ECOMPOUND_DONE_ALL; // We're only notifying done all atm... and only when it's done...
        }
    }

    if (pStructLike->_coreFlags & (COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME|COMPOUNDFLAG_BODY_IN_ERROR)) { // TODO: CLEANUP: distinction runtime/all
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Compound is flaggued as in-error"), pTCContext->pWorker);
        progressToSignal = ECOMPOUND_DONE_ALL; // We're only notifying done all atm... and only when it's done...
    }

    if (pStructLike->pRegistration->uTCProgress < progressToSignal) {
        on_can_notify_compound_state_change(pStructLike, progressToSignal,
            pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
    }
}

local_func void after_task_on_enum_tc_success(TypeInfo_Enum* pEnum, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pEnum);
    Assert_(pTCContext->pCompoundToTC);
    Assert_(pEnum == pTCContext->pCompoundToTC->pCompoundType);
    Assert_(pEnum->pRegistration == pTCContext->pCompoundToTC);
    Assert_(pTCContext->pCompoundToTC->pSourceFile == pTCContext->pIsolatedSourceFile);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Succeeded TC task on enum %u ('%s') of source file '%s'",
        u64(pEnum->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pEnum->pRegistration->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pEnum->pRegistration->pSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(0u == pTCContext->setOfNewlyDeclaredIdentifiers.size()); // New identifiers from enum should not be declared as such
    Assert_(0u == (pEnum->_coreFlags & (COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME|COMPOUNDFLAG_BODY_IN_ERROR))); // should not be sucess there...

    Assert_(pEnum->pRegistration->uTCProgress < ECOMPOUND_DONE_ALL);
    on_can_notify_compound_state_change(pEnum, ECOMPOUND_DONE_ALL,
        pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
}

local_func void after_task_on_enum_tc_error(TypeInfo_Enum* pEnum, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pEnum);
    Assert_(pTCContext->pCompoundToTC);
    Assert_(pEnum == pTCContext->pCompoundToTC->pCompoundType);
    Assert_(pEnum->pRegistration == pTCContext->pCompoundToTC);
    Assert_(pTCContext->pCompoundToTC->pSourceFile == pTCContext->pIsolatedSourceFile);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Failed TC task on enum %u ('%s') of source file '%s'",
        u64(pEnum->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pEnum->pRegistration->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pEnum->pRegistration->pSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(0u == pTCContext->setOfNewlyDeclaredIdentifiers.size()); // New identifiers from enum should not be declared as such
    Assert_(pEnum->_coreFlags & (COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME|COMPOUNDFLAG_BODY_IN_ERROR)); // should be flagged as such already...

    Assert_(pEnum->pRegistration->uTCProgress < ECOMPOUND_DONE_ALL);
    on_can_notify_compound_state_change(pEnum, ECOMPOUND_DONE_ALL,
        pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
}

local_func void after_task_on_proc_tc_success(TCProcBodyResult* pProcBody, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pProcBody);
    Assert_(pProcBody == pTCContext->pProcResult);
    Assert_(pProcBody->iSourceFileIndex == pTCContext->pIsolatedSourceFile->iRegistrationIndex);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Succeeded TC task on proc %u ('%s') of source file '%s'",
        u64(pProcBody->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProcBody->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(0u == pTCContext->setOfNewlyDeclaredIdentifiers.size()); // No new identifiers from proc
    Assert_(pProcBody->uProcBodyTypechekingStatus < EPROCSTATUS_SUCCESS); // should not be sucess there...

    if (pProcBody->uIsForeignSource) {
        bool bOnlyForeignSpec = true;
        IRRepo* pRepo = &(pTCContext->pProcResult->procwiseRepo);
        u32 uInstrCount = pRepo->uSize;
        for (u32 uInstr = pTCContext->pProcResult->procSign->params.size(); uInstr < uInstrCount; uInstr++) {
            IREntry& entry = ir_access_repo_instr(pRepo, uInstr);
            u8 uInstructionKind = u8(entry.uInstrCodeAndFormatAndFirstParam);
            if (tIRITFormatSlot[uInstructionKind] != IR_INSTR_NOVALUE) {
                if (0u == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) ||
                            (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)) { // string for 'foreign' itself adds declarations
                    bOnlyForeignSpec = false;
                    break;
                }
            } else {
                if (uInstructionKind != IRIT_MARKER_JUMP_TARGET &&
                    uInstructionKind != IRIT_MARKER_START_SOURCE_SCOPE &&
                    uInstructionKind != IRIT_MARKER_END_SOURCE_SCOPE &&
                    uInstructionKind != IRIT_RET) {
                        bOnlyForeignSpec = false;
                        break;
                }
            }
        }
        if (bOnlyForeignSpec) {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "Successfully Ended Proc with a 'foreign' specification"), pTCContext->pWorker);

            Assert_(pTCContext->pIsolatedSourceFile->iRegistrationIndex == pTCContext->pProcResult->iSourceFileIndex);
            pTCContext->pIsolatedSourceFile->vecProcsIndicesDeclaredAsForeign.append(pTCContext->pProcResult->uRegistrationIndex);
            on_can_notify_proc_state_change(pProcBody, EPROCSTATUS_SUCCESS_FOREIGN,
                pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
        } else {
            // TODO: log error
            platform_log_info("**** Proc with a 'foreign' specification had other runtime instructions...***");
            on_can_notify_proc_state_change(pProcBody, EPROCSTATUS_IN_ERROR_TC,
                pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
        }
    } else {
        // TODO: CLEANUP: not called from there at some point: spawn an 'optim' task for it ?
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Ended TC of regular Proc - Now checking basic block graph"), pTCContext->pWorker);
        do_graph_passes_on_procbody(pTCContext->pProcResult, pTCContext);

        on_can_notify_proc_state_change(pProcBody, EPROCSTATUS_SUCCESS,
            pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
    }
}

local_func void after_task_on_proc_tc_error(TCProcBodyResult* pProcBody, TCContext* pTCContext,
    TmpArray<TCContext*>* outVecLocalTasksToWake, TmpArray<TCContext*>* outVecNonLocalNonIdTasksToWake)
{
    Assert_(pProcBody);
    Assert_(pProcBody == pTCContext->pProcResult);
    Assert_(pProcBody->iSourceFileIndex == pTCContext->pIsolatedSourceFile->iRegistrationIndex);

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Failed TC task on proc %u ('%s') of source file '%s'",
        u64(pProcBody->uRegistrationIndex),
        reinterpret_cast<u64>(get_identifier_string(pTCContext->pProgCompilationState, pProcBody->iPrimaryIdentifier).c_str()),
        reinterpret_cast<u64>(pTCContext->pIsolatedSourceFile->sourceFileName.c_str())), pTCContext->pWorker);

    Assert_(0u == pTCContext->setOfNewlyDeclaredIdentifiers.size()); // No new identifiers from proc
    Assert_(pProcBody->uProcBodyTypechekingStatus < EPROCSTATUS_SUCCESS); // should not be sucess there...

    on_can_notify_proc_state_change(pProcBody, EPROCSTATUS_IN_ERROR_TC,
        pTCContext, outVecLocalTasksToWake, outVecNonLocalNonIdTasksToWake);
}

local_func void notify_wakeup_non_id_tasks_in_other_source(ArrayView<TCContext*> vecTasksToWake,
    WorkerDesc* pWorkerDesc, WholeProgramCompilationState* pCompState)
{
    u32 uCountTasks = vecTasksToWake.size();
    Assert_(uCountTasks);
    SourceFileDescAndState* pSourceFile = vecTasksToWake[0u]->pIsolatedSourceFile;

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "Waking up %u tasks waiting on distinct source file %s",
        u64(vecTasksToWake.size()),
        reinterpret_cast<u64>(pSourceFile->sourceFileName.c_str())), pWorkerDesc);

    bool tStartEmpty[ETaskPriority::E_COUNT_TASKPRIO];
    bool tAppended[ETaskPriority::E_COUNT_TASKPRIO];

    acquire_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
    for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
        tStartEmpty[ePrio] = 0u == pSourceFile->tvecTCTasksToLaunchByPrio[ePrio].size();
        tAppended[ePrio] = false;
    }
    // Adding wakable tasks in current source to the tasks ready to launch in current file
    //
    for (u32 uTask = 0u; uTask < uCountTasks; uTask++) {
        TCContext* pTask = vecTasksToWake[uTask];
        pSourceFile->tvecTCTasksToLaunchByPrio[pTask->eTaskPrio].append(pTask);
        tAppended[pTask->eTaskPrio] = true;
    }

    bool bPossiblyNeedsNotifyAsNewToLaunch = false;
    for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
        if (tStartEmpty[ePrio] && tAppended[ePrio]) {
            bPossiblyNeedsNotifyAsNewToLaunch = true;
            break;
        }
    }

    if (bPossiblyNeedsNotifyAsNewToLaunch) {
        acquire_global_task_launching_lock(pCompState, pWorkerDesc);
        if (pCompState->setOfFilesWithActiveTasksBeingRun.find(pSourceFile->iRegistrationIndex) == pCompState->setOfFilesWithActiveTasksBeingRun.end()) {
            for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
                if (tStartEmpty[ePrio] && tAppended[ePrio])
                    pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].insert(pSourceFile->iRegistrationIndex);
            }
        } // Otherwise NOOP: we cannot update that file with any 'to launch' since it currently has a running task upon it already...
        release_global_task_launching_lock(pCompState, pWorkerDesc);
    }

    release_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
}

local_func void handle_next_tc_task(int iSourceFileIndexToTC, WorkerDesc* pWorkerDesc,
    WholeProgramCompilationState* pCompState, LocLib_CompilationResults* oCompilationResults)
{
    Assert_(iSourceFileIndexToTC >= 0 && u32(iSourceFileIndexToTC) < pCompState->vecSourceFiles.size());
    on_set_newjob_to_worker(pWorkerDesc, u16(iSourceFileIndexToTC));

    // TODO: timing
    u8 uSysHour = 0u;
    u8 uSysMin = 0u;
    u8 uSysSec = 0u;
    u64 uSecondsSinceStart = 0uLL;
    u32 uNanoSecondsSinceStart = 0u;
    BLOCK_TRACE(ELOCPHASE_ORCHESTRATION, _LLVL2_IMPORTANT_INFO, EventORCH_WORKER_ASSIGN(
        pWorkerDesc->uLocThreadId, uSysHour, uSysMin, uSysSec, uSecondsSinceStart, uNanoSecondsSinceStart), pWorkerDesc);

    SourceFileDescAndState* pSourceFile = pCompState->vecSourceFiles[u32(iSourceFileIndexToTC)];

    acquire_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
    TCContext* pTCContext = 0;
    for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
        if (pSourceFile->tvecTCTasksToLaunchByPrio[ePrio].size()) {
            pTCContext = pSourceFile->tvecTCTasksToLaunchByPrio[ePrio].pop_last();
            break;
        }
    }
    release_source_file_specific_task_lock(pSourceFile, pWorkerDesc);

    Assert_(pTCContext);
    Assert_(pTCContext->pNamespace);

    pTCContext->pWorker = pWorkerDesc;
    if (is_ctx_global(pTCContext)) {
        pTCContext->setOfNewlyDeclaredIdentifiers.init(pWorkerDesc->tmpArena);  // the set of all newly declared identifiers on this TC *pass* (worker-tmp allocated)
        pTCContext->uSizeOfVecIncludedStructLikeBefore = 0u;
    } else {
        if (is_ctx_compound(pTCContext)) {
            Assert_(pTCContext->pCompoundToTC);
            Assert_(pTCContext->pCompoundToTC->pCompoundType);
            if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                pTCContext->setOfNewlyDeclaredIdentifiers.init(pWorkerDesc->tmpArena);  // the set of all newly declared identifiers on this TC *pass* (worker-tmp allocated)
                TypeInfo_StructLike* pAsStructLike = (TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType;
                pTCContext->uSizeOfVecIncludedStructLikeBefore = pAsStructLike->vecIncluded.size();
            } else {
                pTCContext->uSizeOfVecIncludedStructLikeBefore = 0u;
            }
        } else {
            pTCContext->uSizeOfVecIncludedStructLikeBefore = 0u;
        }
        pTCContext->uSizeOfVecUsingAccessibleEnumBefore = 0u;
        pTCContext->uSizeOfVecUsingAccessibleNamespaceBefore = 0u;
        pTCContext->uSizeOfVecUsingAllEnumBefore = 0u;
        pTCContext->uSizeOfVecUsingAllNamespaceBefore = 0u;
        pTCContext->uSizeOfVecChildrenNamespacesBefore = 0u;
    }


    //
    // This is the main thing right here !!!

    ETCResult eResult = start_or_resume_typecheck_task(pTCContext);

    //
    //

    ArenaRefPoint tmpBefore = get_arena_ref_point(pWorkerDesc->tmpArena);
    TmpStackOptiArray<TCContext*, 64u> vecLocalTasksToWake(pWorkerDesc->tmpArena);
    TmpStackOptiArray<TCContext*, 64u> vecNonLocalNonIdTasksToWake(pWorkerDesc->tmpArena);
    Assert_(pSourceFile == pTCContext->pIsolatedSourceFile);
    Assert_(pTCContext->pNamespace);
    Assert_(pSourceFile == pTCContext->pNamespace->pOriginalSourceFile);

    if (is_ctx_global(pTCContext)) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Global TC task terminated with result %u",
            u64(eResult)), pTCContext->pWorker);

        Assert_(!should_tc_ctx_halt_on_non_success(pTCContext));
        Assert_(eResult == ETCResult::ETCR_SUCCESS);
        after_task_on_global_tc(pTCContext->pNamespace, pTCContext,
            &vecLocalTasksToWake, &vecNonLocalNonIdTasksToWake);

    } else if (is_ctx_compound(pTCContext)) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Compound TC task terminated with result %u",
            u64(eResult)), pTCContext->pWorker);

        Assert_(pTCContext->pCompoundToTC);
        Assert_(pSourceFile == pTCContext->pCompoundToTC->pSourceFile);
        Assert_(pTCContext->pCompoundToTC->pCompoundType);
        if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {

            Assert_(!should_tc_ctx_halt_on_non_success(pTCContext));
            Assert_(eResult == ETCResult::ETCR_SUCCESS);
            after_task_on_structlike_tc((TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType, pTCContext,
                &vecLocalTasksToWake, &vecNonLocalNonIdTasksToWake);

        } else { Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_ENUM);

            Assert_(should_tc_ctx_halt_on_non_success(pTCContext));
            if (eResult == ETCResult::ETCR_SUCCESS) {
                after_task_on_enum_tc_success((TypeInfo_Enum*)pTCContext->pCompoundToTC->pCompoundType, pTCContext,
                    &vecLocalTasksToWake, &vecNonLocalNonIdTasksToWake);

            } else if (NOMINAL(eResult == ETCResult::ETCR_WAITING)) {
                // NOOP
            } else {
                after_task_on_enum_tc_error((TypeInfo_Enum*)pTCContext->pCompoundToTC->pCompoundType, pTCContext,
                    &vecLocalTasksToWake, &vecNonLocalNonIdTasksToWake);
            }

        }

    } else { Assert_(is_ctx_with_proc_source(pTCContext));

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Proc TC task terminated with result %u",
            u64(eResult)), pTCContext->pWorker);

        Assert_(pTCContext->pProcSource);
        Assert_(pTCContext->pProcResult);
        Assert_(should_tc_ctx_halt_on_non_success(pTCContext));

        if (eResult == ETCResult::ETCR_SUCCESS) {
            after_task_on_proc_tc_success(pTCContext->pProcResult, pTCContext,
                &vecLocalTasksToWake, &vecNonLocalNonIdTasksToWake);

        } else if (NOMINAL(eResult == ETCResult::ETCR_WAITING)) {
            // NOOP
        } else {
            after_task_on_proc_tc_error(pTCContext->pProcResult, pTCContext,
                &vecLocalTasksToWake, &vecNonLocalNonIdTasksToWake);
        }

    }

    u32 uCountNonIdTasksToWakeInOtherSources = vecNonLocalNonIdTasksToWake.size();
    u32 uCountLocalTasksToWake = vecLocalTasksToWake.size();

    if (uCountNonIdTasksToWakeInOtherSources + uCountLocalTasksToWake) {

        acquire_global_waiting_task_lock(pCompState, pWorkerDesc);

        // Removing wakable tasks in other sources from the global waiting-task bookkeeper
        //
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Removing %u tasks waiting in other files from the global wait-bookkeeper as a result of this TC completion",
                u64(uCountNonIdTasksToWakeInOtherSources)), pWorkerDesc);
            for (u32 uTask = 0u; uTask < uCountNonIdTasksToWakeInOtherSources; uTask++) {
                TCContext* pTask = vecNonLocalNonIdTasksToWake[uTask];
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Removing Task 0x%llx", reinterpret_cast<u64>(pTask)), pWorkerDesc);
                bool bRemoved = pCompState->setOfTCWaitingTasks.remove_at_key(pTask);
                Assert_(bRemoved);
            }
        }
        // Removing wakable tasks in current source from the global waiting-task bookkeeper
        //
        {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Removing %u tasks waiting in same file from the global wait-bookkeeper as a result of this TC completion",
                u64(uCountLocalTasksToWake)), pWorkerDesc);
            for (u32 uTask = 0u; uTask < uCountLocalTasksToWake; uTask++) {
                TCContext* pTask = vecLocalTasksToWake[uTask];
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                    "Removing Task 0x%llx", reinterpret_cast<u64>(pTask)), pWorkerDesc);
                bool bRemoved = pCompState->setOfTCWaitingTasks.remove_at_key(pTask);
                Assert_(bRemoved);
            }
        }

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "Remaining TC tasks on-hold known to the global wait-bookkeeper : %u", 
            u64(pCompState->setOfTCWaitingTasks.size())), pWorkerDesc);
        release_global_waiting_task_lock(pCompState, pWorkerDesc);
    } else {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
            "No tasks will be awoken as a result of this TC completion"), pWorkerDesc);
    }

    if (uCountNonIdTasksToWakeInOtherSources >= 1u) { // Waking up tasks waiting for (ensured non-id) in other sources

        if (uCountNonIdTasksToWakeInOtherSources == 1u) {
            Assert_(vecNonLocalNonIdTasksToWake[0u]->pIsolatedSourceFile != pSourceFile);
            notify_wakeup_non_id_tasks_in_other_source(vecNonLocalNonIdTasksToWake, pWorkerDesc, pCompState);

        } else {
            TmpMap<int, TmpArray<TCContext*>> mapPerFile;
            mapPerFile.init(FireAndForgetArenaAlloc(pWorkerDesc->tmpArena));
            for (u32 uTask = 0u; uTask < uCountNonIdTasksToWakeInOtherSources; uTask++) {
                TCContext* pTask = vecNonLocalNonIdTasksToWake[uTask];
                SourceFileDescAndState* pOtherSource = pTask->pIsolatedSourceFile;
                Assert_(pOtherSource != pSourceFile);
                int iOtherFileIndex = pOtherSource->iRegistrationIndex;
                u64 uOtherHash = get_map_hash(iOtherFileIndex);
                auto itFile = mapPerFile.findHashed(uOtherHash, iOtherFileIndex);
                if (itFile == mapPerFile.end()) {
                    TmpArray<TCContext*> newArrayThere;
                    newArrayThere.init(pWorkerDesc->tmpArena);
                    itFile = mapPerFile.insert_not_present(uOtherHash, iOtherFileIndex, newArrayThere);
                }
                itFile.value().append(pTask);
            }
            Assert_(mapPerFile.size());
            for (auto it = mapPerFile.begin(), itEnd = mapPerFile.end(); it != itEnd; it++) {
                notify_wakeup_non_id_tasks_in_other_source(it.value(), pWorkerDesc, pCompState);
            }
        }
    }
     
    {
        acquire_source_file_specific_task_lock(pSourceFile, pWorkerDesc);

        // Adding wakable tasks in current source to the tasks ready to launch in current file
        //
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Waking up %u tasks waiting on same source file %s, before registering current task 0x%llx as done",
            u64(vecLocalTasksToWake.size()),
            reinterpret_cast<u64>(pSourceFile->sourceFileName.c_str()),
            reinterpret_cast<u64>(pTCContext)), pTCContext->pWorker);

        for (u32 uTask = 0u; uTask < uCountLocalTasksToWake; uTask++) {
            TCContext* pTask = vecLocalTasksToWake[uTask];
            pSourceFile->tvecTCTasksToLaunchByPrio[pTask->eTaskPrio].append(pTask);
        }

        {
            acquire_global_task_launching_lock(pCompState, pWorkerDesc);
            // Re-adding ourselves to set of files with tasks to launch by prio
            for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
                if (pSourceFile->tvecTCTasksToLaunchByPrio[ePrio].size())
                    pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].insert(pSourceFile->iRegistrationIndex);
            }
            // Removing ourselves from set of files with tasks being run
            pCompState->setOfFilesWithActiveTasksBeingRun.remove_at_key(pSourceFile->iRegistrationIndex);
            release_global_task_launching_lock(pCompState, pWorkerDesc);
        }
        release_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
    }

}

local_func void handle_put_worker_to_sleep(WorkerDesc* pWorkerDesc, WholeProgramCompilationState* pCompState)
{
    // TODO: timing ?

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
        "A worker thread shall enter sleeping state..."), pWorkerDesc);

    // TODO: enter sleeping state...
    //platform_sleep_ms(1u); // Simply that...
                             // or TODO CLEANUP setting up some wait condition (much more involved + requires to actually trigger wakeup somewhere!)
}

local_func bool when_remaining_waiting_tasks_found_unresolved_ids_to_report_return_unlocked(WorkerDesc* pWorkerDesc,
    WholeProgramCompilationState* pCompState, LocLib_CompilationResults* oCompilationResults)
{
    Assert_(pCompState->setOfTCWaitingTasks.size());
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL3_USER_ERROR, EventREPT_CUSTOM_HARDCODED(
        "No active TC task remaining while there still exist TC tasks waiting for resolutions... looking for unresolved id errors to report first"), pWorkerDesc);

    ArenaRefPoint beforeTmp = get_arena_ref_point(pWorkerDesc->tmpArena);
    defer { reset_arena_no_release_to(beforeTmp, pWorkerDesc->tmpArena); };

    TmpMap<u64, TmpArray<TCContext*>> mapAllEntitiesOnHoldDueToWaitForNonId;
    TmpMap<u64, TmpArray<TCContext*>> mapAllEntitiesOnHoldDueToWaitForId;
    mapAllEntitiesOnHoldDueToWaitForNonId.init(FireAndForgetArenaAlloc(pWorkerDesc->tmpArena));
    mapAllEntitiesOnHoldDueToWaitForId.init(FireAndForgetArenaAlloc(pWorkerDesc->tmpArena));
    for (auto it = pCompState->setOfTCWaitingTasks.begin(), itEnd = pCompState->setOfTCWaitingTasks.end(); it != itEnd; it++) {
        TCContext* pTask = *it;
        Assert_(pTask->pNamespace);
        Assert_(pTask->pIsolatedSourceFile);
        u64 uEntityId = u64(u32(pTask->pIsolatedSourceFile->iRegistrationIndex)) << 32u;
        if (is_ctx_global(pTask)) {
            uEntityId |= u64(0xA000'0000u | pTask->pNamespace->uRegistrationIndex);
        } else if (is_ctx_compound(pTask)) {
            Assert_(pTask->pCompoundToTC);
            Assert_(pTask->pCompoundToTC->pCompoundType);
            uEntityId |= u64(0xB000'0000u | u32(pTask->pCompoundToTC->pCompoundType->uRegistrationIndex));
        } else { Assert_(pTask->pProcResult);
            uEntityId |= u64(0xC000'0000u | pTask->pProcResult->uRegistrationIndex);
        }

        if (pTask->waitingReason.getWaitingReason() == EWR_GLOBAL_IDENTIFIER_RESOLUTION) {
            auto itFoundEntityForId = mapAllEntitiesOnHoldDueToWaitForId.find(uEntityId);
            if (itFoundEntityForId == mapAllEntitiesOnHoldDueToWaitForId.end()) {
                TmpArray<TCContext*> newArray;
                newArray.init(pWorkerDesc->tmpArena);
                itFoundEntityForId = mapAllEntitiesOnHoldDueToWaitForId.insert(uEntityId, newArray);
            }
            itFoundEntityForId.value().append(pTask);
        } else {
            auto itFoundEntityForNonId = mapAllEntitiesOnHoldDueToWaitForNonId.find(uEntityId);
            if (itFoundEntityForNonId == mapAllEntitiesOnHoldDueToWaitForNonId.end()) {
                TmpArray<TCContext*> newArray;
                newArray.init(pWorkerDesc->tmpArena);
                itFoundEntityForNonId = mapAllEntitiesOnHoldDueToWaitForNonId.insert(uEntityId, newArray);
            }
            itFoundEntityForNonId.value().append(pTask);
        }
    }

    for (auto it = mapAllEntitiesOnHoldDueToWaitForId.begin(), itEnd = mapAllEntitiesOnHoldDueToWaitForId.end(); it != itEnd; it++) {
        u64 uEntityId = it.key();
        if (mapAllEntitiesOnHoldDueToWaitForNonId.find(uEntityId) == mapAllEntitiesOnHoldDueToWaitForNonId.end()) {
            // TODO: CLEANUP: much better than this...
            // we surely need to report compounds first, then namespace *iff a parent isn't itself on hold*, etc.
            u32 uEntityKind = u32(uEntityId & 0xF000'0000u);
            switch (uEntityKind) {
                case 0xA000'0000u: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL3_USER_ERROR, EventREPT_CUSTOM_HARDCODED(
                        "Found namespace with tasks waiting for id resolution, and not waiting on anything else => reporting those ids as unresolved"), pWorkerDesc);
                } break;
                case 0xB000'0000u: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL3_USER_ERROR, EventREPT_CUSTOM_HARDCODED(
                        "Found compound with tasks waiting for id resolution, and not waiting on anything else => reporting those ids as unresolved"), pWorkerDesc);
                } break;
                case 0xC000'0000u: {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL3_USER_ERROR, EventREPT_CUSTOM_HARDCODED(
                        "Found proc with tasks waiting for id resolution, and not waiting on anything else => reporting those ids as unresolved"), pWorkerDesc);
                } break;
                default:
                    Assert_(false);
            }

            u32 uCountTasks = it.value().size();
            Assert_(uCountTasks);
            TCNamespace* pAssociatedNamespace = it.value()[0u]->pNamespace;
            SourceFileDescAndState* pSourceFile = pAssociatedNamespace->pOriginalSourceFile;
            TmpSet<int> setAwaitedIds;
            setAwaitedIds.init(FireAndForgetArenaAlloc(pWorkerDesc->tmpArena));
            for (u32 uTask = 0u; uTask < uCountTasks; uTask++) {
                TCContext* pTask = it.value()[uTask];
                Assert_(pTask->pNamespace == pAssociatedNamespace);
                Assert_(pTask->waitingReason.getWaitingReason() == EWR_GLOBAL_IDENTIFIER_RESOLUTION);
                u32 uId = pTask->waitingReason.getAwaitedId();
                setAwaitedIds.insert(int(uId));
            }
            for (auto it = setAwaitedIds.begin(), itEnd = setAwaitedIds.end(); it != itEnd; it++) {
                int iId = *it;
                ValueBinding* pBindingInError = (ValueBinding*)alloc_from(pSourceFile->localArena,
                    sizeof(ValueBinding), alignof(ValueBinding));
                pBindingInError->iIdentifierHandle = iId;
                pBindingInError->pType = 0;
                pBindingInError->info.uIRandMetaFlags = IRFLAG_TC_BINDING_INSTANCE;
                u32 uBindingPos = pSourceFile->vecAllGlobalBindings.size();
                pBindingInError->uScopeAndLocation = u32(EScopeKind::SCOPEKIND_GLOBAL_PACKAGE) | (uBindingPos << 8);
                pAssociatedNamespace->mapAccessibleBindingsInclUsing.insert(iId, pBindingInError);
                pAssociatedNamespace->mapAccessibleDeclarationsById.insert(iId, uBindingPos);
                pSourceFile->vecAllGlobalBindings.append(pBindingInError);
            }

            TmpArray<TCContext*> vecTasksToWake;
            vecTasksToWake.init(pWorkerDesc->tmpArena);
            on_need_notify_identifiers_available_on_local_namespace_and_descendants(pAssociatedNamespace, setAwaitedIds,
                pCompState, pWorkerDesc, pSourceFile, &vecTasksToWake);

            u32 uCountToWake = vecTasksToWake.size();
            Assert_(uCountToWake != 0u);
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Removing %u tasks waiting in same file from the global wait-bookkeeper as a result of this TC completion",
                    u64(uCountToWake)), pWorkerDesc);
                for (u32 uTask = 0u; uTask < uCountToWake; uTask++) {
                    TCContext* pTask = vecTasksToWake[uTask];
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Removing Task 0x%llx", reinterpret_cast<u64>(pTask)), pWorkerDesc);
                    bool bRemoved = pCompState->setOfTCWaitingTasks.remove_at_key(pTask);
                    Assert_(bRemoved);
                }
            }

            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL8_REGULAR_INFO, EventREPT_CUSTOM_HARDCODED(
                "Remaining TC tasks on-hold known to the global wait-bookkeeper : %u", 
                u64(pCompState->setOfTCWaitingTasks.size())), pWorkerDesc);
            release_global_waiting_task_lock(pCompState, pWorkerDesc);

            Assert_(pCompState->setOfFilesWithActiveTasksBeingRun.size() == 0u); // Could deadlock here if this wasn't the case...

            acquire_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
            for (u32 uTask = 0u; uTask < uCountToWake; uTask++) {
                TCContext* pTask = vecTasksToWake[uTask];
                pSourceFile->tvecTCTasksToLaunchByPrio[pTask->eTaskPrio].append(pTask);
            }
            // Re-adding to set of files with tasks to launch by prio
            for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
                if (pSourceFile->tvecTCTasksToLaunchByPrio[ePrio].size())
                    pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].insert(pSourceFile->iRegistrationIndex);
            }
            release_source_file_specific_task_lock(pSourceFile, pWorkerDesc);
            return true;
        }
    }

    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
        "### No Entity was found with only on-hold-tasks waiting for unresolved ids... looks like a circular def error ??"), pWorkerDesc);
    
    TCContext* pPickupOne = *(pCompState->setOfTCWaitingTasks.begin());
    LocLib_Error error;
    error.errCode = FERR_OTHER;
    error.uBlockOrLineIfScanErr = pPickupOne->pCurrentBlock->uAstBlockIndex;
    error.uStatement = should_tc_ctx_halt_on_non_success(pPickupOne) ?
        pPickupOne->pCurrentBlock->uStatementBeingTypechecked : pPickupOne->uGlobalStatementOnHold;
    error.uTokenRef = pPickupOne->pCurrentBlock->vecStatements[error.uStatement]->vecNodes[0u]->ast.uPivotalTokenPackedRef;
    pPickupOne->pIsolatedSourceFile->vecErrors.append(error);

    release_global_waiting_task_lock(pCompState, pWorkerDesc);
    return false;
}

local_func void run_parse_and_tc_worker(WorkerDesc* pWorkerDesc,
    WholeProgramCompilationState* pCompState, LocLib_CompilationResults* oCompilationResults)
{
    while (true) { iterate_search_for_task_and_run:

        acquire_global_task_launching_lock(pCompState, pWorkerDesc);

        if (pWorkerDesc->bAllowParse) { // only *one* worker out of all should be allowed to parse
            if (pCompState->mapOfParseSourceFileTasksToLaunch.size()) {
                auto it = pCompState->mapOfParseSourceFileTasksToLaunch.begin();
                int iSourceFileIndexToParse = it.key();
                ParseFileTaskDescription taskDesc = it.value();
                pCompState->setOfFilesWithActiveTasksBeingRun.insert(iSourceFileIndexToParse);
                pCompState->mapOfParseSourceFileTasksToLaunch.remove_at_iter(it);
                release_global_task_launching_lock(pCompState, pWorkerDesc);

                handle_parsing_task(iSourceFileIndexToParse, taskDesc, pWorkerDesc, pCompState, oCompilationResults);
                goto iterate_search_for_task_and_run;
            }
        }

        if (pWorkerDesc->bAllowTC) { // a priori all of them... TODO: maybe have a priority system instead ?
            for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
                if (pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].size()) {
                    auto it = pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].begin();
                    int iSourceFileIndexToTC = *it;
                    pCompState->setOfFilesWithActiveTasksBeingRun.insert(iSourceFileIndexToTC);
                    pCompState->tsetOfFilesWithTCTasksByPrio[ePrio].remove_at_iter(it);
                    for (ETaskPriority ePrio2 = ETaskPriority::ETASKPRIO_HIGHEST;
                        ePrio2 <= ETaskPriority::ETASKPRIO_LOWEST; ePrio2 = ETaskPriority(ePrio2 + 1u)) {
                        if (ePrio2 != ePrio)
                            pCompState->tsetOfFilesWithTCTasksByPrio[ePrio2].remove_at_key(iSourceFileIndexToTC);
                    }
                    release_global_task_launching_lock(pCompState, pWorkerDesc);

                    handle_next_tc_task(iSourceFileIndexToTC, pWorkerDesc, pCompState, oCompilationResults);
                    goto iterate_search_for_task_and_run;
                }
            }
        }

        // TODO: also distinct container of IR-optim-tasks ?

        if (pCompState->setOfFilesWithActiveTasksBeingRun.size() > 0) {
            release_global_task_launching_lock(pCompState, pWorkerDesc);
            handle_put_worker_to_sleep(pWorkerDesc, pCompState); // Note: Possibly wait condition ? ... but only a simple 'sleep' there could do.

            goto iterate_search_for_task_and_run;
        }

        acquire_global_waiting_task_lock(pCompState, pWorkerDesc); // CLEANUP: since we should be sole task in flight there, maybe not even need to lock
        if (pCompState->setOfTCWaitingTasks.size()) {
            if (when_remaining_waiting_tasks_found_unresolved_ids_to_report_return_unlocked(pWorkerDesc, pCompState, oCompilationResults)) {
                release_global_task_launching_lock(pCompState, pWorkerDesc);
                goto iterate_search_for_task_and_run;
            } else {
                release_global_task_launching_lock(pCompState, pWorkerDesc);
                // TODO: CLEANUP: report circular def error ???
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL1_IMPL_WARNING, EventREPT_CUSTOM_HARDCODED(
                    "### Circular def error ?? current worker breaking out of TC iteration..."), pWorkerDesc);           
                break;
            }
        } else {
            release_global_waiting_task_lock(pCompState, pWorkerDesc);
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL2_IMPORTANT_INFO, EventREPT_CUSTOM_HARDCODED(
                "All TC and Parsing work finished..."), pWorkerDesc);
            release_global_task_launching_lock(pCompState, pWorkerDesc);
            break;
        }

    }

}


/*

    TODO: uncomment at some point; 'prematurely'-prepared for multithread

#define THREAD_INDEX_0_MAIN     0u
struct WorkingThread {
    u32 uThreadIndex;
    u32 _pad0;
    ChunkProvider localProvider;
};

struct LOCWorkingThread {
    WorkingThread base;
    WorkerDesc worker;
    WholeProgramCompilationState* pCompState;
    LocLib_CompilationResults* oCompilationResults;
};


local_func LOCWorkingThread* init_thread_info_table(
    ChunkProvider mainProvider, Arena globalArena, u32 uNonMainThreadCount,
    WholeProgramCompilationState* pCompState, LocLib_CompilationResults* oCompilationResults)
{
    Assert(uNonMainThreadCount < 512u, "Soft-Assert : Thread-count is currently expected like a handlful (circa 2023 implementation),",
                                       " and each thread will currently init an independent ChunkProvider, each requiring at least 8 MB"
                                       " just by being there (not all really used - but still reserved). If those thread count assumptions"
                                       " changed that much, maybe rethink that design.");
    Assert_(uNonMainThreadCount <= 0x0'FFFFu);
    u32 uTotalThreadCount = uNonMainThreadCount + 1u;
    LOCWorkingThread* tResult = (LOCWorkingThread*)alloc_from(globalArena,
        uTotalThreadCount*sizeof(LOCWorkingThread), alignof(LOCWorkingThread));
    tResult[0].base.uThreadIndex = THREAD_INDEX_0_MAIN;
    tResult[0].base.localProvider = mainProvider;
    Arena mainTmpArena, mainLogArena;
    init_arena(&mainTmpArena, mainProvider);
    init_arena(&mainLogArena, mainProvider);
    init_worker(&tResult[0].worker, mainTmpArena, u16(THREAD_INDEX_0_MAIN), 1u, 1u, mainLogArena);
    tResult[0].pCompState = pCompState;
    tResult[0].oCompilationResults = oCompilationResults;
    for (u32 uThread = 1u; uThread < uTotalThreadCount; uThread++) {
        tResult[uThread].base.uThreadIndex = uThread;
        ChunkProvider localProvider;
        init_chunk_provider(&localProvider, false);
        Arena tmpArena, logArena;
        init_arena(&tmpArena, localProvider);
        init_arena(&logArena, localProvider);
        tResult[uThread].base.localProvider = localProvider;
        init_worker(&tResult[uThread].worker, tmpArena, u16(uThread), 0u, 1u, logArena);
        tResult[uThread].pCompState = pCompState;
        tResult[uThread].oCompilationResults = oCompilationResults;
    }
    return tResult;
}

typedef void (ThreadRunFn)(LOCWorkingThread* pThreadInfo);

local_func void spawn_and_run_threads(LOCWorkingThread* tThreadInfoTable, u32 uNonMainThreadCount, ThreadRunFn* pRunFn) {
    for (u32 uThread = 1u; uThread <= uNonMainThreadCount; uThread++) {
        platform_spawn_and_run_thread(pRunFn, tThreadInfoTable + uThread);
    }
    pRunFn(tThreadInfoTable + THREAD_INDEX_0_MAIN);
}

local_func void run_parse_and_tc_worker(LOCWorkingThread* pThreadInfo)
{
    run_parse_and_tc_worker(&pThreadInfo->worker, pThreadInfo->pCompState, pThreadInfo->oCompilationResults);
}

*/

#endif // LOCLIB_ORCHESTRATOR_H_
