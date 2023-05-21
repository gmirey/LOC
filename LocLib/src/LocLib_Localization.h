#pragma once

#ifndef LOCLIB_LOCALIZATION_H_
#define LOCLIB_LOCALIZATION_H_

#include "LocLib_Cmd_API.h"

struct LocLib_ReportLanguageOverride {
    // TODO
};

exported_func_decl(LOCLIB) LocLib_ReportLanguageOverride* init_report_language(
    const u8* pFileContents,
    u64 uFileContentsByteSize,
    LocLib_OS_WrapperFunctions* pOsFuncs)
{
    // TODO
    return 0;
}

exported_func_decl(LOCLIB) void release_report_language(
    LocLib_ReportLanguageOverride* pReportLanguage,
    LocLib_OS_WrapperFunctions* pOsFuncs)
{
    // TODO
}

#endif // LOCLIB_LOCALIZATION_H_

