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

#ifndef LOCLIB_WORKER_H_
#define LOCLIB_WORKER_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/HashSets.h"
#include "../../HighPerfTools/Strings.h"

#include "LocLib_Events.h"
#include "LocLib_LogTrace.h"

#define LOC_WORKER_TRACE_STACKSIZE      128u

struct WorkerDesc {
    Arena tmpArena;
    u16 uCurrentStackIndex;
    u8 bAllowParse;
    u8 bAllowTC;
    u16 uLocThreadId;                  // TODO: programwise dense indexing starting from 0 to N=thread count
    u16 uLockedSourceFile;
    TracableEvent tEventStack[LOC_WORKER_TRACE_STACKSIZE];  // shall always be set
    #if TRACABLE_EVENTS_KEEP_FULL_LOG
        TmpArray<StableGrowingVector<TracableEvent>> vecFullLogs;          // can be present if TRACABLE_EVENTS_KEEP_FULL_LOG
    #endif
};

local_func void init_worker(WorkerDesc* ioWorker, Arena tmpArena, u16 uLocThreadId, u8 bAllowParse, u8 bAllowTC, Arena logArena)
{
    ioWorker->tmpArena = tmpArena;
    ioWorker->uCurrentStackIndex = 0u;
    ioWorker->bAllowParse = bAllowParse;
    ioWorker->bAllowTC = bAllowTC;
    ioWorker->uLocThreadId = uLocThreadId;
    ioWorker->uLockedSourceFile = 0xFFFFu;
    #if TRACABLE_EVENTS_KEEP_FULL_LOG
        ioWorker->vecFullLogs.init(logArena);
    #endif
}

local_func void on_set_newjob_to_worker(WorkerDesc* ioWorker, u16 uLockedSourceFile = 0xFFFFu)
{
    ioWorker->uLockedSourceFile = uLockedSourceFile;
    #if TRACABLE_EVENTS_KEEP_FULL_LOG
        StableGrowingVector<TracableEvent> vecEventsOnTheJob(ioWorker->vecFullLogs._alloc.arena);
        ioWorker->vecFullLogs.append(vecEventsOnTheJob);
    #endif
}

local_func_inl void trace_event_push(WorkerDesc* pWorker, ELocPhase ePhase, i8 iLogLevel, TracableEvent eventEntry)
{
    #ifdef TRACABLE_EVENTS_WITH_RDTSC
        eventEntry.uRdtsc = __rdtsc();
    #else
        eventEntry.uRdtsc = 0uLL;
    #endif
    Assert_(iLogLevel >= 0 && iLogLevel < 10);
    eventEntry.eLocPhase = ePhase;
    eventEntry.uLevel = u8(iLogLevel);
    Assert_(pWorker->uCurrentStackIndex < LOC_WORKER_TRACE_STACKSIZE);
    pWorker->tEventStack[pWorker->uCurrentStackIndex] = eventEntry;
    pWorker->uCurrentStackIndex++;
    #if TRACABLE_EVENTS_KEEP_FULL_LOG
        pWorker->vecFullLogs.last().append(eventEntry);
    #endif
}

local_func_inl void trace_event_pop(WorkerDesc* pWorker, ELocPhase ePhase, i8 iLogLevel)
{
    Assert_(iLogLevel >= 0 && iLogLevel < 10);
    Assert_(pWorker->uCurrentStackIndex);
    pWorker->uCurrentStackIndex--;
    TracableEvent& poppedEvt = pWorker->tEventStack[pWorker->uCurrentStackIndex];
    Assert_(poppedEvt.uLevel == u8(iLogLevel));
    Assert_(poppedEvt.eLocPhase == ePhase);
    #if TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG
        poppedEvt.eLocPhase = ELocPhase(poppedEvt.eLocPhase | 0x80u);   // hacking 'phase' with a 'popped' flag.
        pWorker->vecFullLogs.last().append(poppedEvt);
    #endif
}

local_func void debug_print_last_event(WorkerDesc* pWorker)
{
    Assert_(pWorker->uCurrentStackIndex);
    u16 uLastIndex = pWorker->uCurrentStackIndex - 1u;
    const TracableEvent& lastEvent = pWorker->tEventStack[uLastIndex];
    #if TRACABLE_EVENTS_WITH_RDTSC
        char szTmpRDTSC[32];
        sprintf(szTmpRDTSC, "%020llu: ", lastEvent.uRdtsc);
        platform_log_info(szTmpRDTSC, false);
    #endif
    if (uLastIndex) {
        u16 uRemainingIndent = uLastIndex;
        if (uLastIndex <= 20) {        // prints up to 20 identations of 2 spaces each
            // NOOP
        } else if (uLastIndex <= 40) { // prints '+ ' then up to 19 more
            platform_log_info("+ ", false);
            uRemainingIndent -= 21u;
        } else if (uLastIndex <= 60) { // prints '++' then up to 19 more
            platform_log_info("++", false);
            uRemainingIndent -= 41u;
        } else if (uLastIndex <= 79) { // prints '+++ ' then up to 18 more
            platform_log_info("+++ ", false);
            uRemainingIndent -= 61u;
        } else if (uLastIndex <= 98) { // prints '++++' then up to 18 more
            platform_log_info("++++", false);
            uRemainingIndent -= 80u;
        } else if (uLastIndex <= 116) {// prints '++++ +' then up to 17 more
            platform_log_info("++++ +", false);
            uRemainingIndent -= 99u;
        } else { // prints '++++ ++ ' then up to 16 more
            platform_log_info("++++ ++ ", false);
            uRemainingIndent -= 117u;
        }
        constexpr char* pLotsOfSpaces = "                                                            ";
        platform_log_info(StringView::from_known_c_str(pLotsOfSpaces, uRemainingIndent*2u, false), false);
    }
    char szTmp[PRINT_EVENT_REQ_BUFFER_SIZE];
    print_event_to(szTmp, lastEvent);
    platform_log_info(szTmp, true);
}

struct ATraceBlockAutoPopper {
    ATraceBlockAutoPopper(WorkerDesc* pWorker, ELocPhase ePhase, i8 iLogLevel):_pWorker(pWorker),_ePhase(ePhase),_iLogLevel(iLogLevel) {}
    ~ATraceBlockAutoPopper() { trace_event_pop(_pWorker, _ePhase, _iLogLevel); }
    WorkerDesc* _pWorker;
    ELocPhase _ePhase;
    i8 _iLogLevel;
};


#endif // LOCLIB_WORKER_H_
