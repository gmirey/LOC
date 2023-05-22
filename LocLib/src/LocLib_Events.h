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

#ifndef LOCLIB_EVENTS_H_
#define LOCLIB_EVENTS_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/HashSets.h"
#include "../../HighPerfTools/Strings.h"

#if 0
// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define LOC_PHASE(name, shortName, dbgName)
#define LOC_PHASES_EMITTER_ \
        \
    LOC_PHASE(ORCHESTRATION,    ORCH, "General orchestration")     /* Deciding what to do, synchronization, high-level allocations, etc */ \
        \
    LOC_PHASE(ALLOC,            ALLC, "Allocation")                /* Generic allocation events */ \           
    LOC_PHASE(IO_READ,          IORD, "I/O File Read")             /* Reading, typically from file on disk */ \
    LOC_PHASE(IO_WRITE,         IOWR, "I/O File Write")            /* Writing, typically to file on disk */ \
        \
    LOC_PHASE(SCANNER,          SCAN, "Scanning Source Line")      /* From source file: checking utf8 validity, EOL, and line length */ \
    LOC_PHASE(TOKENIZER,        TOKN, "Tokenizing Scanned Line")   /* Remembering known identifiers, recognizing symbols, evaluating simple literals... */ \
        \
    LOC_PHASE(PARSER_MGR,       PRMG, "Parser Management")         /* Orchestration of parser steps, handling of block concerns */ \
    LOC_PHASE(PARSER_PRE,       PPAR, "Pre-parsing")               /* Making a pre-AST based on grammar. May reinvoke scanner/tokenizer if multiline. */ \
    LOC_PHASE(PARSER_POST,      PSTP, "Post-parsing")              /* Consolidation of pre-parsed statements to final AST. */ \
        \
    LOC_PHASE(TC_MGR,           TCMG, "TC Management")             /* Orchestration of TC steps, handling of statements, blocks, virtual elses, macro-expansions, defers, prolog/epilogs... */ \
    LOC_PHASE(TC_EXPR,          EXPR, "TC Expression")             /* Typechecking an expression. */ \
    LOC_PHASE(TC_CAST,          CAST, "TC Cast")                   /* Deciding of resulting type for an expression, and/or casting an expression to it. */ \
        \
    LOC_PHASE(IR_SOLVER,        SOLV, "IR Solver")                 /* Trying to solve an IR operation to a known result, and/or early detection of overflows, etc at comptime. */ \
    LOC_PHASE(IR_EMISSION,      IREM, "IR Emission")               /* Emitting any IR. */ \
    LOC_PHASE(IR_OPTIMIZER,     OPTI, "IR Optimization")           /* Optimizing already emitted IR: may emit anew, reorganize blocks, gather meta info, reinvoke solver... */ \
        \
    LOC_PHASE(REPORT,           REPT, "Reporting")                 /* All tasks related to ir dumping, or other complex reports. */ \
        \
    LOC_PHASE(BCK_PREP,         BCKP, "Backend Preparation")       /* Preparing IR for the backend: may convert unhandled formats to software-ir-solving, and emit anew. */ \
    LOC_PHASE(BCK_EMISSION,     BCKE, "Backend Emission")          /* Emitting actual machine code ! */ \
    LOC_PHASE(BCK_LINKS,        BCKL, "Backend Links")             /* Backend-link Organizing final steps before emission to binary, solving for placeholders to addresses, etc. */ \

#endif

#if 1
// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define LOC_PHASE(name, shortName, dbgName)
#define LOC_PHASES_EMITTER_ \
        \
    LOC_PHASE(ORCHESTRATION,    ORCH, "General orchestration")     /* Deciding what to do, synchronization, high-level allocations, etc */ \
    LOC_PHASE(TC_MGR,           TCMG, "TC Management")             /* Orchestration of TC steps, handling of statements, blocks, virtual elses, macro-expansions, defers, prolog/epilogs... */ \
    LOC_PHASE(TC_EXPR,          EXPR, "TC Expression")             /* Typechecking an expression. */ \
    LOC_PHASE(REPORT,           REPT, "Reporting")                 /* All tasks related to ir dumping, or other complex reports. */ \

#endif

enum ELocPhase : u8 {
    #define LOC_PHASE(name, shortName, dbgName) ELOCPHASE_ ## name ,
        LOC_PHASES_EMITTER_
    #undef LOC_PHASE
};

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define LOG_LEVEL(suffix, dbgName)
#define LOG_LEVELS_EMITTER_ \
        \
    LOG_LEVEL(_LLVL0_IMPL_ERROR,        "Compiler Implementation Error") \
    LOG_LEVEL(_LLVL1_IMPL_WARNING,      "Compiler Implementation Warning") \
    LOG_LEVEL(_LLVL2_IMPORTANT_INFO,    "Information of general importance") \
    LOG_LEVEL(_LLVL3_USER_ERROR,        "LOC-Language 'Error'") \
    LOG_LEVEL(_LLVL4_USER_WARNING,      "LOC-Language 'Warning'") \
    LOG_LEVEL(_LLVL5_SIGNIFICANT_STEP,  "Entering a significant phase") \
    LOG_LEVEL(_LLVL6_SIGNIFICANT_INFO,  "Additional info about a significant phase") \
    LOG_LEVEL(_LLVL7_REGULAR_STEP,      "Entering a regular phase") \
    LOG_LEVEL(_LLVL8_REGULAR_INFO,      "Additional info about a regular phase") \
    LOG_LEVEL(_LLVL9_VERBOSE,           "Minor events, for when you really need the full story...")


// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum {
    #define LOG_LEVEL(suffix, dbgName)  suffix ,
        LOG_LEVELS_EMITTER_
    #undef LOG_LEVEL
};

#if 1
// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define EVT_ORCH(name, dbgName)
#define ORCH_EVTS_EMITTER_ \
        \
    EVT_ORCH(MAIN_FRONTEND_LOOP, "Main Front-End Loop (Parsing And TC)") \
    EVT_ORCH(WORKER_ASSIGN, "Worker job assignment and Sync") \

#endif

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum EEvtORCH : u16 {
    #define EVT_ORCH(name, dbgName)  EEVT_ORCH_ ## name ,
        ORCH_EVTS_EMITTER_ 
    #undef EVT_ORCH
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tEvtORCHDbgStr[] = {
#define EVT_ORCH(name, dbgName)  dbgName ,
    ORCH_EVTS_EMITTER_
#undef EVT_ORCH
};
static const u8 COUNT_EVT_ORCH = u8(sizeof(tEvtORCHDbgStr) / sizeof(char*));

#if 1
// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define EVT_TCMG(name, dbgName)
#define TCMG_EVTS_EMITTER_ \
        \
    EVT_TCMG(TC_FILE, "TC-ing File Globals") \
    EVT_TCMG(TC_PROC, "TC-ing Proc Body") \
    EVT_TCMG(TC_COMPOUND, "TC-ing Compound Body") \
        \
    EVT_TCMG(TC_STATEMENT, "TC-ing Statement") \

#endif

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum EEvtTCMG : u16 {
    #define EVT_TCMG(name, dbgName) EEVT_TCMG_ ## name ,
        TCMG_EVTS_EMITTER_
    #undef EVT_TCMG
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tEvtTCMGDbgStr[] = {
#define EVT_TCMG(name, dbgName)  dbgName ,
    TCMG_EVTS_EMITTER_
#undef EVT_TCMG
};
static const u8 COUNT_EVT_TCMG = u8(sizeof(tEvtTCMGDbgStr) / sizeof(char*));

#if 1
// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define EVT_EXPR(name, dbgName)
#define EXPR_EVTS_EMITTER_ \
        \
    EVT_EXPR(TC_NON_INVOC, "TC-ing Non-Invoc-Expression") \

#endif

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum EEvtEXPR : u16 {
    #define EVT_EXPR(name, dbgName) EEVT_EXPR_ ## name ,
        EXPR_EVTS_EMITTER_
    #undef EVT_EXPR
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tEvtEXPRDbgStr[] = {
#define EVT_EXPR(name, dbgName)  dbgName ,
    EXPR_EVTS_EMITTER_
#undef EVT_EXPR
};
static const u8 COUNT_EVT_EXPR = u8(sizeof(tEvtEXPRDbgStr) / sizeof(char*));

#if 1
// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define EVT_REPT(name, dbgName)
#define REPT_EVTS_EMITTER_ \
        \
    EVT_REPT(CUSTOM_HARDCODED, "Custom HardCoded Report") /* for temporary logs until given a proper event type in an appropriate phase */ \

#endif

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum EEvtREPT : u16 {
    #define EVT_REPT(name, dbgName)  EEVT_REPT_ ## name ,
        REPT_EVTS_EMITTER_ 
    #undef EVT_REPT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tEvtREPTDbgStr[] = {
#define EVT_REPT(name, dbgName)  dbgName ,
    REPT_EVTS_EMITTER_
#undef EVT_REPT
};
static const u8 COUNT_EVT_REPT = u8(sizeof(tEvtREPTDbgStr) / sizeof(char*));

struct TracableEvent {
    u64 uRdtsc;                     // may be set #ifdef TRACABLE_EVENTS_WITH_RDTSC
    union {
        u64 _eventTypeAndPayload;
        struct {
            u32 uEventPayload32;    // can be index in a node chain, or identifying statement level, or rhv vs lhv, or operandA vs operand B...
            u16 uEventType;         // see EEvtORCH, EEvtIORD, EEvtIOWR, ETokEvents, ETCMgEvents, EExprTCEvents, ECastEvents, ESolverEvents, EIREvents, EIROptim, EPrepEvents, EBckEvents  
            u8 uLevel;              // 'criticity' (ie log level) at which this entry was set 
            ELocPhase eLocPhase;    // see ELocPhase: Orchestration, Tokenization, Parsing, TC, IR, Backend... and even subsystems of those
        };
    };
    u64 tPayload[6];                // enough room to store contents from two NodeValues or two IREntries, or 3 TCNode, or lots of things...
};
typedef void (*print_event_fn)(char* szBuffer, const TracableEvent& eventEntry);

#include "stdio.h"

#define EVT_ORCH(name, dbgName)  void print_event_ORCH_ ## name ## _to(char* szBuffer, const TracableEvent& eventEntry); /* predecl */
    ORCH_EVTS_EMITTER_
#undef EVT_ORCH

#define EVT_TCMG(name, dbgName)  void print_event_TCMG_ ## name ## _to(char* szBuffer, const TracableEvent& eventEntry); /* predecl */
    TCMG_EVTS_EMITTER_
#undef EVT_TCMG

#define EVT_EXPR(name, dbgName)  void print_event_EXPR_ ## name ## _to(char* szBuffer, const TracableEvent& eventEntry); /* predecl */
    EXPR_EVTS_EMITTER_
#undef EVT_EXPR

#define EVT_REPT(name, dbgName)  void print_event_REPT_ ## name ## _to(char* szBuffer, const TracableEvent& eventEntry); /* predecl */
    REPT_EVTS_EMITTER_
#undef EVT_REPT

        
#define PRINT_EVENT_REQ_BUFFER_SIZE     1024u

local_func TracableEvent EventREPT_CUSTOM_HARDCODED(const char* szHardCodedCString,
    u64 uParam1 = 0uLL, u64 uParam2 = 0uLL, u64 uParam3 = 0uLL)
{
    TracableEvent result = {};
    result.uEventType = EEvtREPT::EEVT_REPT_CUSTOM_HARDCODED;
    result.uEventPayload32 = 0u;
    result.tPayload[0] = reinterpret_cast<u64>(szHardCodedCString);
    result.tPayload[1] = uParam1;
    result.tPayload[2] = uParam2;
    result.tPayload[3] = uParam3;
    return result;
}
local_func void print_event_REPT_CUSTOM_HARDCODED_to(char* szBuffer, const TracableEvent& eventEntry)
{
    Assert_(eventEntry.eLocPhase == ELOCPHASE_REPORT);
    Assert_(eventEntry.uEventType == EEVT_REPT_CUSTOM_HARDCODED);
    const char* szHardCodedCString = reinterpret_cast<const char*>(eventEntry.tPayload[0]);
    sprintf(szBuffer, szHardCodedCString, eventEntry.tPayload[1], eventEntry.tPayload[2], eventEntry.tPayload[3]);
}

local_func void print_event_to(char* szBuffer, const TracableEvent& eventEntry)
{
    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    constexpr print_event_fn tPrintEvtORCH[] = {
        #define EVT_ORCH(name, dbgName)  &print_event_ORCH_ ## name ## _to,
            ORCH_EVTS_EMITTER_
        #undef EVT_ORCH
    };

    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    constexpr print_event_fn tPrintEvtTCMG[] = {
        #define EVT_TCMG(name, dbgName)  &print_event_TCMG_ ## name ## _to,
            TCMG_EVTS_EMITTER_
        #undef EVT_TCMG
    };

    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    constexpr print_event_fn tPrintEvtEXPR[] = {
        #define EVT_EXPR(name, dbgName)  &print_event_EXPR_ ## name ## _to,
            EXPR_EVTS_EMITTER_
        #undef EVT_EXPR
    };

    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    constexpr print_event_fn tPrintEvtREPT[] = {
        #define EVT_REPT(name, dbgName)  &print_event_REPT_ ## name ## _to,
            REPT_EVTS_EMITTER_
        #undef EVT_REPT
    };

    switch (eventEntry.eLocPhase) {

        // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
        #define LOC_PHASE(name, shortName, dbgName) case ELOCPHASE_ ## name: { \
                Assert_(eventEntry.uEventType < COUNT_EVT_ ## shortName); \
                tPrintEvt ## shortName[eventEntry.uEventType](szBuffer, eventEntry); \
            } break;

            LOC_PHASES_EMITTER_

        #undef LOC_PHASE

        default:
            Assert_(false);
    }
}


#endif // LOCLIB_EVENTS_H_
