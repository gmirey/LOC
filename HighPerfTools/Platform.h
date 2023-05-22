// Part of LocLang/HighPerfTools
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

#include "BaseDecls.h"      // basic stuff for everything
#include "Platform_API.h"   // we'll implement a few things and helpers around decls from there

PlatformPageAllocConf platform_page_alloc_conf;
//PlatformTasksConf platform_task_conf;
PlatformLoggingConf platform_logging_conf;
PlatformIOConf platform_io_conf;

// set_platform_confs: to be exported (as declared in Platform_API.h)
exported_func_impl void set_platform_confs(const PlatformPageAllocConf& page_alloc_conf, const PlatformLoggingConf& logging_conf, const PlatformIOConf& io_conf)
{
    platform_page_alloc_conf = page_alloc_conf;
    platform_logging_conf = logging_conf;
    platform_io_conf = io_conf;
}

static FORCE_INLINE u8* platform_page_alloc(u64 min_bytes) { return platform_page_alloc_conf.page_alloc(min_bytes); }
static FORCE_INLINE void platform_page_free(u8* alloc_result) { platform_page_alloc_conf.page_free(alloc_result); }

/*
static FORCE_INLINE void platform_push_high_load_task(TaskFn* task, UParam param) { platform_task_conf.push_high_load_task(task, param); }
static FORCE_INLINE void platform_push_io_task(TaskFn* task, u16 io_index, UParam param) { platform_task_conf.push_io_task(task, io_index, param); }
static FORCE_INLINE void platform_add_event_waiter(TaskFn* task, UParam param) { platform_task_conf.add_event_waiter(task, param); }
*/

static FORCE_INLINE bool platform_on_report_error_to_user_should_break(const char* message) { return platform_logging_conf.on_report_error_to_user_should_break(message); }
//static FORCE_INLINE bool platform_on_report_assert_fail_should_break(StringView message) { return platform_logging_conf.on_report_assert_fail_should_break(message); }
static FORCE_INLINE void platform_log_info(StringView message, bool bAddEndOfLine = true) { platform_logging_conf.log_info(message, bAddEndOfLine); }
static FORCE_INLINE void platform_log_debug(StringView message, bool bAddEndOfLine = true) { platform_logging_conf.log_debug(message, bAddEndOfLine); }
static FORCE_INLINE void platform_log_error(StringView message, bool bAddEndOfLine = true) { platform_logging_conf.log_error(message, bAddEndOfLine); }

//static FORCE_INLINE bool platform_on_report_error_to_user_should_break(const char* message) { return platform_logging_conf.on_report_assert_fail_should_break_cstr(message); }

static FORCE_INLINE u8* platform_load_file_entirely(StringView fileNameUtf8, u64* outByteSize, EFileOpenErr* optErr) {
    return platform_io_conf.load_file_entirely(fileNameUtf8, outByteSize, optErr); }
static FORCE_INLINE void platform_release_whole_file_buffer(u8* file_data, u64 byte_size) { return platform_io_conf.release_whole_file_buffer(file_data, byte_size); }
static FORCE_INLINE void platform_exit_process() { return platform_io_conf.exit_process(); }

static FORCE_INLINE PlatformFileHandle 	platform_open_file_for_writing(StringView fileNameUtf8, EFileOpenErr* outErr, bool bCreateAnew = true) {
    return platform_io_conf.open_file_for_writing(fileNameUtf8, outErr, bCreateAnew); }
static FORCE_INLINE u32 platform_write_to_file(PlatformFileHandle file, const u8* pData, u32 uByteSize) { return platform_io_conf.write_to_file(file, pData, uByteSize); }
//static FORCE_INLINE PlatformFileHandle platform_open_file_for_reading(StringView fileNameUtf8, u64* outByteSize, EFileOpenErr* outErr) {
//    return platform_io_conf.open_file_for_reading(fileNameUtf8, outByteSize, outErr); }
//static FORCE_INLINE u32 platform_read_from_file(PlatformFileHandle file, u8* outData, u32 uByteSize) { return platform_io_conf.read_from_file(file, outData, uByteSize); }

//static FORCE_INLINE bool platform_set_pos_in_file(PlatformFileHandle file, u64 uPos) { return platform_io_conf.set_pos_in_file(file, uPos); }
static FORCE_INLINE void platform_close_file(PlatformFileHandle file) { platform_io_conf.close_file(file); }

