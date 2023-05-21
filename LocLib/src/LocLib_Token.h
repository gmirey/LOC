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
