#pragma once

#ifndef LOCLIB_WORKER_H_
#define LOCLIB_WORKER_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/HashSets.h"
#include "../../HighPerfTools/Strings.h"

#include "LocLib_Events.h"

enum EPrintAndLogLevel : i8 {
    ELOGLVL_NEVER_EVEN_LOG    = -2, // -- to be set as value on 'TRACE_XXXX_PRINTLEVEL' to 
    ELOGLVL_NEVER_PRINT       = -1, // -- to be set as value on 'TRACE_XXXX_PRINTLEVEL' to disable all printed reports, even of errors
    #define LOG_LEVEL(suffix, dbgName)  ELOGLVL ## suffix ,
        LOG_LEVELS_EMITTER_
    #undef LOG_LEVEL
};

#ifndef TRACE_ORCH_PRINTLEVEL
    #define TRACE_ORCH_PRINTLEVEL           0
    #define TRACE_ORCH_LOG_IFNOTAFTER       0x0000'0000u
    #define TRACE_ORCH_PRINT_IFNOTAFTER     0x0000'0000u
#endif
#if TRACE_ORCH_PRINTLEVEL > -2

    #define _IMPL_TRACE_ENTER_ELOCPHASE_ORCHESTRATION(logLevel, eventEntry, worker) \
        trace_event_push(worker, ELOCPHASE_ORCHESTRATION, logLevel, eventEntry); \
        _CODE_CONCAT(_IMPL_ON_ORCHESTRATION_EVENT, logLevel)(worker);

    #define _IMPL_TRACE_EXIT_ELOCPHASE_ORCHESTRATION(logLevel, worker) \
        trace_event_pop(worker, ELOCPHASE_ORCHESTRATION, logLevel);

    #define _IMPL_BLOCK_TRACE_ELOCPHASE_ORCHESTRATION(logLevel, eventEntry, worker, theLine) \
        _IMPL_TRACE_ENTER_ELOCPHASE_ORCHESTRATION(logLevel, eventEntry, worker); \
        ATraceBlockAutoPopper _CODE_CONCAT(_autoPopTrace, theLine)(worker, ELOCPHASE_ORCHESTRATION, logLevel);

    #if TRACE_ORCH_PRINTLEVEL >= 0
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL0_IMPL_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL0_IMPL_ERROR(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 1
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL1_IMPL_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL1_IMPL_WARNING(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 2
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL2_IMPORTANT_INFO(worker)    debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL2_IMPORTANT_INFO(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 3
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL3_USER_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL3_USER_ERROR(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 4
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL4_USER_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL4_USER_WARNING(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 5
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL5_SIGNIFICANT_STEP(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL5_SIGNIFICANT_STEP(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 6
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL6_SIGNIFICANT_INFO(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL6_SIGNIFICANT_INFO(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 7
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL7_REGULAR_STEP(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL7_REGULAR_STEP(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 8
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL8_REGULAR_INFO(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL8_REGULAR_INFO(worker)
    #endif

    #if TRACE_ORCH_PRINTLEVEL >= 9
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL9_VERBOSE(worker)           debug_print_last_event(worker)
    #else
        #define _IMPL_ON_ORCHESTRATION_EVENT_LLVL9_VERBOSE(worker)
    #endif

#else
    #define _IMPL_TRACE_ENTER_ELOCPHASE_ORCHESTRATION(logLevel, eventEntry, worker)
    #define _IMPL_TRACE_EXIT_ELOCPHASE_ORCHESTRATION(logLevel, worker)
    #define _IMPL_BLOCK_TRACE_ELOCPHASE_ORCHESTRATION(logLevel, eventEntry, worker)
#endif

#ifndef TRACE_TCMG_PRINTLEVEL
    #define TRACE_TCMG_PRINTLEVEL           0
    #define TRACE_TCMG_LOG_IFNOTAFTER       0x0000'0000u
    #define TRACE_TCMG_PRINT_IFNOTAFTER     0x0000'0000u
#endif
#if TRACE_TCMG_PRINTLEVEL > -2

    #define _IMPL_TRACE_ENTER_ELOCPHASE_TC_MGR(logLevel, eventEntry, worker) \
        trace_event_push(worker, ELOCPHASE_TC_MGR, logLevel, eventEntry); \
        _CODE_CONCAT(_IMPL_ON_TC_MGR_EVENT, logLevel)(worker);

    #define _IMPL_TRACE_EXIT_ELOCPHASE_TC_MGR(logLevel, worker) \
        trace_event_pop(worker, ELOCPHASE_TC_MGR, logLevel);

    #define _IMPL_BLOCK_TRACE_ELOCPHASE_TC_MGR(logLevel, eventEntry, worker, theLine) \
        _IMPL_TRACE_ENTER_ELOCPHASE_TC_MGR(logLevel, eventEntry, worker); \
        ATraceBlockAutoPopper _CODE_CONCAT(_autoPopTrace, theLine)(worker, ELOCPHASE_TC_MGR, logLevel);

    #if TRACE_TCMG_PRINTLEVEL >= 0
        #define _IMPL_ON_TC_MGR_EVENT_LLVL0_IMPL_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL0_IMPL_ERROR(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 1
        #define _IMPL_ON_TC_MGR_EVENT_LLVL1_IMPL_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL1_IMPL_WARNING(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 2
        #define _IMPL_ON_TC_MGR_EVENT_LLVL2_IMPORTANT_INFO(worker)    debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL2_IMPORTANT_INFO(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 3
        #define _IMPL_ON_TC_MGR_EVENT_LLVL3_USER_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL3_USER_ERROR(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 4
        #define _IMPL_ON_TC_MGR_EVENT_LLVL4_USER_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL4_USER_WARNING(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 5
        #define _IMPL_ON_TC_MGR_EVENT_LLVL5_SIGNIFICANT_STEP(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL5_SIGNIFICANT_STEP(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 6
        #define _IMPL_ON_TC_MGR_EVENT_LLVL6_SIGNIFICANT_INFO(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL6_SIGNIFICANT_INFO(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 7
        #define _IMPL_ON_TC_MGR_EVENT_LLVL7_REGULAR_STEP(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL7_REGULAR_STEP(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 8
        #define _IMPL_ON_TC_MGR_EVENT_LLVL8_REGULAR_INFO(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL8_REGULAR_INFO(worker)
    #endif

    #if TRACE_TCMG_PRINTLEVEL >= 9
        #define _IMPL_ON_TC_MGR_EVENT_LLVL9_VERBOSE(worker)           debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_MGR_EVENT_LLVL9_VERBOSE(worker)
    #endif

#else
    #define _IMPL_TRACE_ENTER_ELOCPHASE_TC_MGR(logLevel, eventEntry, worker)
    #define _IMPL_TRACE_EXIT_ELOCPHASE_TC_MGR(logLevel, worker)
    #define _IMPL_BLOCK_TRACE_ELOCPHASE_TC_MGR(logLevel, eventEntry, worker)
#endif

#ifndef TRACE_EXPR_PRINTLEVEL
    #define TRACE_EXPR_PRINTLEVEL           0
    #define TRACE_EXPR_LOG_IFNOTAFTER       0x0000'0000u
    #define TRACE_EXPR_PRINT_IFNOTAFTER     0x0000'0000u
#endif
#if TRACE_EXPR_PRINTLEVEL > -2

    #define _IMPL_TRACE_ENTER_ELOCPHASE_TC_EXPR(logLevel, eventEntry, worker) \
        trace_event_push(worker, ELOCPHASE_TC_EXPR, logLevel, eventEntry); \
        _CODE_CONCAT(_IMPL_ON_TC_EXPR_EVENT, logLevel)(worker);

    #define _IMPL_TRACE_EXIT_ELOCPHASE_TC_EXPR(logLevel, worker) \
        trace_event_pop(worker, ELOCPHASE_TC_EXPR, logLevel);

    #define _IMPL_BLOCK_TRACE_ELOCPHASE_TC_EXPR(logLevel, eventEntry, worker, theLine) \
        _IMPL_TRACE_ENTER_ELOCPHASE_TC_EXPR(logLevel, eventEntry, worker); \
        ATraceBlockAutoPopper _CODE_CONCAT(_autoPopTrace, theLine)(worker, ELOCPHASE_TC_EXPR, logLevel);

    #if TRACE_EXPR_PRINTLEVEL >= 0
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL0_IMPL_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL0_IMPL_ERROR(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 1
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL1_IMPL_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL1_IMPL_WARNING(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 2
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL2_IMPORTANT_INFO(worker)    debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL2_IMPORTANT_INFO(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 3
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL3_USER_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL3_USER_ERROR(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 4
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL4_USER_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL4_USER_WARNING(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 5
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL5_SIGNIFICANT_STEP(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL5_SIGNIFICANT_STEP(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 6
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL6_SIGNIFICANT_INFO(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL6_SIGNIFICANT_INFO(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 7
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL7_REGULAR_STEP(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL7_REGULAR_STEP(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 8
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL8_REGULAR_INFO(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL8_REGULAR_INFO(worker)
    #endif

    #if TRACE_EXPR_PRINTLEVEL >= 9
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL9_VERBOSE(worker)           debug_print_last_event(worker)
    #else
        #define _IMPL_ON_TC_EXPR_EVENT_LLVL9_VERBOSE(worker)
    #endif

#else
    #define _IMPL_TRACE_ENTER_ELOCPHASE_TC_EXPR(logLevel, eventEntry, worker)
    #define _IMPL_TRACE_EXIT_ELOCPHASE_TC_EXPR(logLevel, worker)
    #define _IMPL_BLOCK_TRACE_ELOCPHASE_TC_EXPR(logLevel, eventEntry, worker)
#endif

#ifndef TRACE_REPT_PRINTLEVEL
    #define TRACE_REPT_PRINTLEVEL           0
    #define TRACE_REPT_LOG_IFNOTAFTER       0x0000'0000u
    #define TRACE_REPT_PRINT_IFNOTAFTER     0x0000'0000u
#endif
#if TRACE_REPT_PRINTLEVEL > -2

    #define _IMPL_TRACE_ENTER_ELOCPHASE_REPORT(logLevel, eventEntry, worker) \
        trace_event_push(worker, ELOCPHASE_REPORT, logLevel, eventEntry); \
        _CODE_CONCAT(_IMPL_ON_REPORT_EVENT, logLevel)(worker);

    #define _IMPL_TRACE_EXIT_ELOCPHASE_REPORT(logLevel, worker) \
        trace_event_pop(worker, ELOCPHASE_REPORT, logLevel);

    #define _IMPL_BLOCK_TRACE_ELOCPHASE_REPORT(logLevel, eventEntry, worker, theLine) \
        _IMPL_TRACE_ENTER_ELOCPHASE_REPORT(logLevel, eventEntry, worker); \
        ATraceBlockAutoPopper _CODE_CONCAT(_autoPopTrace, theLine)(worker, ELOCPHASE_REPORT, logLevel);

    #if TRACE_REPT_PRINTLEVEL >= 0
        #define _IMPL_ON_REPORT_EVENT_LLVL0_IMPL_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL0_IMPL_ERROR(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 1
        #define _IMPL_ON_REPORT_EVENT_LLVL1_IMPL_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL1_IMPL_WARNING(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 2
        #define _IMPL_ON_REPORT_EVENT_LLVL2_IMPORTANT_INFO(worker)    debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL2_IMPORTANT_INFO(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 3
        #define _IMPL_ON_REPORT_EVENT_LLVL3_USER_ERROR(worker)        debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL3_USER_ERROR(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 4
        #define _IMPL_ON_REPORT_EVENT_LLVL4_USER_WARNING(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL4_USER_WARNING(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 5
        #define _IMPL_ON_REPORT_EVENT_LLVL5_SIGNIFICANT_STEP(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL5_SIGNIFICANT_STEP(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 6
        #define _IMPL_ON_REPORT_EVENT_LLVL6_SIGNIFICANT_INFO(worker)  debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL6_SIGNIFICANT_INFO(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 7
        #define _IMPL_ON_REPORT_EVENT_LLVL7_REGULAR_STEP(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL7_REGULAR_STEP(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 8
        #define _IMPL_ON_REPORT_EVENT_LLVL8_REGULAR_INFO(worker)      debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL8_REGULAR_INFO(worker)
    #endif

    #if TRACE_REPT_PRINTLEVEL >= 9
        #define _IMPL_ON_REPORT_EVENT_LLVL9_VERBOSE(worker)           debug_print_last_event(worker)
    #else
        #define _IMPL_ON_REPORT_EVENT_LLVL9_VERBOSE(worker)
    #endif

#else
    #define _IMPL_TRACE_ENTER_ELOCPHASE_REPORT(logLevel, eventEntry, worker)
    #define _IMPL_TRACE_EXIT_ELOCPHASE_REPORT(logLevel, worker)
    #define _IMPL_BLOCK_TRACE_ELOCPHASE_REPORT(logLevel, eventEntry, worker)
#endif

#ifndef DISABLE_ALL_TRACES
    #define DISABLE_ALL_TRACES 0
#endif

#ifndef TRACABLE_EVENTS_WITH_RDTSC
    #define TRACABLE_EVENTS_WITH_RDTSC   0
#endif

#ifndef TRACABLE_EVENTS_KEEP_FULL_LOG
    #define TRACABLE_EVENTS_KEEP_FULL_LOG   0
#endif

#ifndef TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG
    #define TRACABLE_EVENTS_RECORD_POPS_IN_FULL_LOG   0
#endif

// TODO: an "event-only" version of trace, which adds to the full log without modifying the stack
// TODO: an full set of #define for the *trace level* itself, above which we shall kill all code emission for trace macros
// TODO: a #define for a perf-only setting, which would kill emission of the custom *eventEntry*, and replace it with rdtsc-only,
//          for all traces > warning. this should help in building a quite accurate visualization of where time is spent (note that
//          _LLVL2_IMPORTANT_INFO can still be used for full event entry emission) and in building either a precise timing scheme for
//          the application, to report to end-user in release mode ; or more of a flame graph for perf hunting.

#if DISABLE_ALL_TRACES
    #define TRACE_ENTER(locPhase, logLevel, eventEntry, worker)
    #define TRACE_EXIT(locPhase, logLevel, eventEntry, worker)
    #define BLOCK_TRACE(locPhase, logLevel, eventEntry, worker)
#else
    #define TRACE_ENTER(locPhase, logLevel, eventEntry, worker)     _CODE_CONCAT(_IMPL_TRACE_ENTER_, locPhase)(logLevel, eventEntry, worker)
    #define TRACE_EXIT(locPhase, logLevel, worker)                  _CODE_CONCAT(_IMPL_TRACE_EXIT_, locPhase)(logLevel, worker)
    #define BLOCK_TRACE(locPhase, logLevel, eventEntry, worker)     _CODE_CONCAT(_IMPL_BLOCK_TRACE_, locPhase)(logLevel, eventEntry, worker, __LINE__)
#endif

// another handy way to "comment out" a trace, while keeping it around for documentation, or if you change your mind
#define DISABLED_TRACE(locPhase, logLevel, eventEntry, worker)
#define DISABLED_TRACE_EXIT(locPhase, logLevel, worker)  

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
