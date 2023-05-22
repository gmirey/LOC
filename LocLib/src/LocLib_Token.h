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

#ifndef LOCLIB_TOKEN_H_
#define LOCLIB_TOKEN_H_

#include "../../HighPerfTools/BaseDecls.h"

static const i64 LINE_BUFFERSIZE                = 1024;
static const i64 MAX_TOKENS_ON_LINE             = 1024;
//static const i64 MAX_BYTES_AFTER_INDENT_ON_LINE = 1016;
//static const i64 MAX_INDENT_BYTES               = 47;

struct Token {
    u32 uTokenPayloadAndKind;  // 24 msb payload (starting at bit 8), 8lsb = OR of an ETokenKind value, and flag "space afterwards" if 8b-msb is set
    u16 uTokenStartCharOnLine;
    u16 uTokenCharCount;
};

#endif // LOCLIB_TOKEN_H_
