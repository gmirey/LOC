#pragma once

#include "BaseDecls_API.h"  // basic stuff for everything
#include "Arenas.h"         // Arenas for recording traces

// ************************************************************
// Debug, Trace and Perf utilities
// ************************************************************

#define MAX_DEBUGGED_THREADS    128

// Note: Any code using these should be compiled in a translation unit having #defined 'DebugRecordArray' and using DECLARE_DEBUG_RECORD_ARRAY() at the very end of the chain

#define DebugRecordArray DebugRecordArray_Test

#if defined(DebugRecordArray)

struct DebugRecord {
    const char* file_name;
    const char* func_name;
    u32 line_number;
    u32 _pad0;
    u64 hit_count_24_msb_and_cycle_count_40_lsb;
};
DebugRecord DebugRecordArray[];
struct TimedBlockHelper {
    DebugRecord* record;
    u64 hit_count_more_offset40;
    u64 start_tsc;
    FORCE_INLINE TimedBlockHelper(u32 counter, const char* file_name, const char* func_name, u32 line_number, u32 hit_inc = 1u) {
        DebugRecord* the_record = DebugRecordArray + counter;
        the_record->file_name = file_name;
        the_record->func_name = func_name;
        the_record->line_number = line_number;
        record = the_record;
        hit_count_more_offset40 = u64(hit_inc) << 40;
        start_tsc = Rdtsc();                    // retrives total count of elapsed cpu cyles at start
    }
    FORCE_INLINE ~TimedBlockHelper() {
        u64 delta_tsc = Rdtsc() - start_tsc;    // retrives total count of elapsed cpu cyles at end, and computes difference with tsc at start
        // atomically increments by both cycle count (up to 2 trillions cycles) and hit count (up to 16 million calls) in a thread-safe manner
        // note: 2 trillions cycles can gather a few hundreds seconds at max, on hardware around the current 3 GHz plateau...
        u64 to_add = hit_count_more_offset40 + delta_tsc;
        InterlockedGetAndAdd64(&(record->hit_count_24_msb_and_cycle_count_40_lsb), to_add);
    }
};

#define _TIMED_BLOCK_COUNTED(line, cnt) TimedBlockHelper _CODE_CONCAT(timed_block_, line) (__COUNTER__, __FILE__, __FUNCTION__, line, cnt)
#define TIMED_BLOCK_COUNTED(cnt)        _TIMED_BLOCK_COUNTED(__LINE__, cnt)

#define _TIMED_BLOCK(line)              TimedBlockHelper _CODE_CONCAT(timed_block_, line) (__COUNTER__, __FILE__, __FUNCTION__, line)
#define TIMED_BLOCK                     _TIMED_BLOCK(__LINE__)


struct TraceRecordEntry {
    u32 debug_record_index;
    u32 hit_count_more;
    u64 start_tsc;
    u64 end_tsc;
};
struct TracePerThread {
    GrowingArena arena;
    FORCE_INLINE void AddEntry(u32 record_index, u32 hit_inc, u64 start_tsc, u64 delta_tsc) {
        TraceRecordEntry* new_entry = (TraceRecordEntry*)arena.allocate(sizeof(TraceRecordEntry), alignof(TraceRecordEntry));
        new_entry->debug_record_index = record_index;
        new_entry->hit_count_more = hit_inc;
        new_entry->start_tsc = start_tsc;
        new_entry->end_tsc = delta_tsc;
    }
};
#define TRACE_RECORD_PER_THREAD_ID      _CODE_CONCAT(DebugRecordArray, _TracesPerThread)
TracePerThread TRACE_RECORD_PER_THREAD_ID [MAX_DEBUGGED_THREADS];
struct TracedBlockHelper {
    DebugRecord* debug_record;
    u32 hit_count_more;
    u16 debug_record_index;
    u16 thread_index;
    u64 start_tsc;
    FORCE_INLINE TracedBlockHelper(u32 counter, const char* file_name, const char* func_name, u32 line_number, u32 the_thread_index, u32 hit_inc = 1u) {
        DebugRecord* the_dbg_record = DebugRecordArray + counter;
        the_dbg_record->file_name = file_name;
        the_dbg_record->func_name = func_name;
        the_dbg_record->line_number = line_number;
        debug_record = the_dbg_record;
        hit_count_more = hit_inc;
        debug_record_index = counter;
        thread_index = the_thread_index;
        start_tsc = Rdtsc();                    // retrives total count of elapsed cpu cyles at start
    }
    FORCE_INLINE ~TracedBlockHelper() {
        u64 delta_tsc = Rdtsc() - start_tsc;    // retrives total count of elapsed cpu cyles at end, and computes difference with tsc at start
        // increments by both cycle count (up to 2 trillions cycles) and hit count (up to 16 million calls) in a thread-safe manner
        // note: 2 trillions cycles can gather a few hundreds seconds at max, on hardware around the current 3 GHz plateau...
        u64 to_add = (u64(hit_count_more) << 40) + delta_tsc;
        InterlockedGetAndAdd64(&(record->hit_count_24_msb_and_cycle_count_40_lsb), to_add);
        // now also add a trace record entry to that
        TracePerThread* trace_record = TRACE_RECORD_PER_THREAD_ID + thread_index;
        trace_record->AddEntry(debug_record_index, hit_count_more, start_tsc, delta_tsc);
    }
};

#define _TRACED_BLOCK_COUNTED(line, uThreadIndex, cnt)   TracedBlockHelper _CODE_CONCAT(traced_block_, line) (__COUNTER__, __FILE__, __FUNCTION__, line, uThreadIndex, cnt)
#define TRACED_BLOCK_COUNTED(uThreadIndex, cnt)          _TRACED_BLOCK_COUNTED(__LINE__, uThreadIndex, cnt)

#define _TRACED_BLOCK(line, uThreadIndex)   TracedBlockHelper _CODE_CONCAT(traced_block_, line) (__COUNTER__, __FILE__, __FUNCTION__, line, uThreadIndex)
#define TRACED_BLOCK(uThreadIndex)          _TRACED_BLOCK(__LINE__, uThreadIndex)

#else

#define TIMED_BLOCK \
    static_assert(false, "'DebugRecordArray' should have been #defined to a unique name for current translation unit before being able to use 'TIMED_BLOCK' macro")
#define TIMED_BLOCK_COUNTED(cnt) \
    static_assert(false, "'DebugRecordArray' should have been #defined to a unique name for current translation unit before being able to use 'TIMED_BLOCK_COUNTED' macro")
#define DECLARE_DEBUG_RECORD_ARRAY \
    static_assert(false, "'DebugRecordArray' should have been #defined to a unique name for current translation unit before being able to use 'DECLARE_DEBUG_RECORD_ARRAY' macro")
#define TRACED_BLOCK(uThreadIndex) \
    static_assert(false, "'DebugRecordArray' should have been #defined to a unique name for current translation unit before being able to use 'TRACED_BLOCK' macro")
#define TRACED_BLOCK_COUNTED(uThreadIndex, cnt) \
    static_assert(false, "'DebugRecordArray' should have been #defined to a unique name for current translation unit before being able to use 'TRACED_BLOCK_COUNTED' macro")

#endif
