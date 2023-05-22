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

