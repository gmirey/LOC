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

#ifndef LOCLIB_LOG_TRACE_H_
#define LOCLIB_LOG_TRACE_H_

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


//
// Disclaimer
//
// This-long, tedious, combinatory-exploded-yet-still-written-by-hand set of #defines
// are all related to our trace & log & even profiling (at some point) system.
// cuz I prematurely-belive it will have a real value in the long run, to be able to vanish
//  them all, or vanish only "some of them" precisely based on a desired (and independent) level. Just like that.
//
// Note that the current-already-long, already-tedious, already-combinatory-exploded-yet-still-written-by-hand set of #defines
// is only 4/19 of our intended enums (TODO!) for decomposing what relates to what... and that we shall probably add some more options.
// Happy days!
// 
// Important note: those traces are already somewhat widespread in the codebase ; although lacking:
// - TODO: a replacement pass of previous system on all things related to parsing 
// - TODO: a much better coverage of the backend side
// - TODO: all-round specialized "events" instead of current mostly "tmp-hardcoded-report-event-as-string"
//

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



#endif // LOCLIB_LOG_TRACE_H_

