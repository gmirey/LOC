#pragma once

#include "BaseDecls.h"
#include "StringView.h"

typedef u8* PageAllocFn(u64 min_bytes);
typedef void PageFreeFn(u8* alloc_result);

struct PlatformPageAllocConf {
    PageAllocFn *page_alloc;
    PageFreeFn  *page_free;
};

/*
enum EIOPorts {
    EIOPORT_FILESYSTEM,
    EIOPORT_GRAPHICS,
    EIOPORT_NETWORK,
    EIOPORT_SOUND,

    IO_PORTS_COUNT
};

typedef void TaskFn(u16 thread_index, UParam param);
typedef void PushTaskFn(TaskFn* task, UParam param);
typedef void PushIOTaskFn(TaskFn* task, u16 io_index, UParam param);

struct PlatformTasksConf {
    u16 uNumberOfHighLoadThreads;
    u16 uNumberOfIOThreads[IO_PORTS_COUNT];
    u16 uNumberOfEventWaitingThreads;
    PushTaskFn      *push_high_load_task;
    PushIOTaskFn    *push_io_task;
    PushTaskFn      *add_event_waiter;
};
*/

typedef bool OnReportFailureCStringFn(const char* failure_message);
//typedef bool OnReportFailureFn(StringView failure_message);
typedef void LogFn(StringView message, bool bAddEndOfLine);

struct PlatformLoggingConf {
    OnReportFailureCStringFn    *on_report_error_to_user_should_break;
//    OnReportFailureFn           *on_report_assert_fail_should_break;
//    OnReportFailureFn           *on_report_error_should_stop;
    LogFn                       *log_info;
    LogFn                       *log_debug;
    LogFn                       *log_error;
};

enum EFileOpenErr {
    FILE_OPENED_SUCCESSFULLY = 0,
    
    FILEOPEN_INVALID_PARAM,
    FILEOPEN_DIRECTORY_NOT_FOUND,
    FILEOPEN_CANNOT_ACCESS_DIRECTORY,
    FILEOPEN_FILE_NOT_FOUND,
    FILEOPEN_UNAUTHORIZED_READ,
    FILEOPEN_FAILED_READ,
    FILEOPEN_OUT_OF_MEMORY,
};

typedef u8* LoadFileEntirelyFn(StringView fileNameUtf8, u64* outByteSize, EFileOpenErr* optErr);
typedef void ReleaseFileBufferFn(u8* file_data, u64 byte_size);

struct PlatformFile;
typedef PlatformFile* PlatformFileHandle;

typedef PlatformFileHandle OpenFileForWritingFn(StringView fileNameUtf8, EFileOpenErr* outErr, bool bCreateAnew);
typedef u32 WriteToFileFn(PlatformFileHandle file, const u8* pData, u32 uByteSize);

//typedef PlatformFileHandle OpenFileForReadingFn(StringView fileNameUtf8, u64* outByteSize, EFileOpenErr* outErr);
//typedef u32 ReadFromFileFn(PlatformFileHandle file, u8* outData, u32 uByteSize);

//typedef bool SetPosInFileFn(PlatformFileHandle file, u64 uPos);

// TODO also: paths, doesFileExist, isDirectory, getFileInfo, Iterate all in directory, Solve path to canonical absolute...

typedef void CloseFileFn(PlatformFileHandle file);

typedef void ExitProcessFn();

struct PlatformIOConf {
    LoadFileEntirelyFn   *load_file_entirely;
    ReleaseFileBufferFn  *release_whole_file_buffer;
    OpenFileForWritingFn *open_file_for_writing;
    //OpenFileForReadingFn *open_file_for_reading;
    WriteToFileFn        *write_to_file;
    //ReadFromFileFn       *read_from_file;
    //SetPosInFileFn       *set_pos_in_file;
    CloseFileFn          *close_file;
    ExitProcessFn        *exit_process;
};

typedef void SetPlatformConfsFn(const PlatformPageAllocConf&, const PlatformLoggingConf&, const PlatformIOConf&);
exported_func_decl(LOCLIB) void set_platform_confs(const PlatformPageAllocConf& page_alloc_conf, const PlatformLoggingConf& logging_conf, const PlatformIOConf& io_conf);

