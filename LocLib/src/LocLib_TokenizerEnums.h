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

#ifndef LOCLIB_TOKENIZER_ENUMS_H_
#define LOCLIB_TOKENIZER_ENUMS_H_

#include "../../HighPerfTools/BaseDecls.h"

// 2b flag to be added to either "reserved" or "identifier" kinds
#define NOTHING_BEFORE          0x00
#define PAN_BEFORE              0x01
#define AT_BEFORE               0x02
#define DOLLAR_BEFORE           0x03  // finally not used in current grammar (dollar is parsed as independent)

#define TOKEN_KIND_MASK                       0x7F  // 'token kind' least significant byte, without 'space afterwards' flag
#define TOKEN_CATEGORY_MASK                   0x3C  // 'token kind' bytes to discriminate between all categories
#define TOKEN_CATEGORY_IS_NONSYMBORKEY_MASK   0x78  // 'token kind' bytes to discriminate between symbols&keywords, and others

#define TOKEN_CATEGORY_SYMBOL                 0x00  // check this value against uTokenPayloadAndKind & TOKEN_CATEGORY_MASK
#define TOKEN_CATEGORY_KEYWORD                0x04  // check this value against uTokenPayloadAndKind & TOKEN_CATEGORY_MASK
#define TOKEN_CATEGORY_IDENTIFIER             0x08  // check this value against uTokenPayloadAndKind & TOKEN_CATEGORY_MASK
#define TOKEN_CATEGORY_NATNUM                 0x18  // check this value against uTokenPayloadAndKind & TOKEN_CATEGORY_MASK
#define TOKEN_CATEGORY_FLOATINGPT             0x28  // check this value against uTokenPayloadAndKind & TOKEN_CATEGORY_MASK
#define TOKEN_CATEGORY_STRING                 0x38  // check this value against uTokenPayloadAndKind & TOKEN_CATEGORY_MASK

#define TOKENKINDFLAG_EXTREME_NUMERAL         0x40  // additional flag for floating points or natural numbers at the time of decoding
#define TOKENKINDFLAG_SPACE_AFTERWARDS        0x80  // additional flag for all token kinds, indicating whether space afterwards


//****************************************************
// A Note on enums-and-structs synchronization here:
// ---------------------------------------------------
// Throughout the parser, there are enums and associated structs which need to be kept in sync,
//   and as always, lots of need for a string representation of enums, for debug purposes at least.
//
// This was tedious by hand, but this is now done with the help following technique :
// cf. "The Magic of Macros" - Using a C-Preprocessor as Code Generator?
//  - András Gáspár, Dr. Lászlí Gianone, Dr. Gábor Tevesz - Beitrag - Embedded Software Engineering Kongress, 2015
// https://www.microconsult.de/1624-0-Magic-of-Macros---ESE-2015.html
//
// + improvement of 'list' of things to emit in a macro itself instead of the above 'include',
//     as seen first (afaik) in Odin source code, (c) Ginger Bill 2016-2022.
//     --> those macros are here recognizable in the following with suffix _EMITTER_
//
// + improvement of various further synchronization checkers, directly derived from those techniques
//
// + a few quite contrived uses at times, pushing the technique further where I could (sorry...)
//
//****************************************************

constexpr bool static_strings_equal(char const * a, char const * b) { return *a == *b && (*a == '\0' || static_strings_equal(a + 1, b + 1)); }

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization"
// use together with    #define TOKEN_KIND(name, assignValue)
#define TOKEN_KINDS_EMITTER_ \
        \
    TOKEN_KIND(SYMBOL, = TOKEN_CATEGORY_SYMBOL) \
    TOKEN_KIND(MULTISYMBOL, = TOKEN_CATEGORY_SYMBOL|1) \
        \
    TOKEN_KIND(KEYWORD, = TOKEN_CATEGORY_KEYWORD|NOTHING_BEFORE) \
    TOKEN_KIND(KEYWORD_PAN, = TOKEN_CATEGORY_KEYWORD|PAN_BEFORE) \
    TOKEN_KIND(KEYWORD_AT, = TOKEN_CATEGORY_KEYWORD|AT_BEFORE) \
    TOKEN_KIND(KEYWORD_DOLLAR, = TOKEN_CATEGORY_KEYWORD|DOLLAR_BEFORE) \
        \
    TOKEN_KIND(IDENTIFIER, = TOKEN_CATEGORY_IDENTIFIER|NOTHING_BEFORE) \
    TOKEN_KIND(IDENTIFIER_PAN, = TOKEN_CATEGORY_IDENTIFIER|PAN_BEFORE) \
    TOKEN_KIND(IDENTIFIER_AT, = TOKEN_CATEGORY_IDENTIFIER|AT_BEFORE) \
    TOKEN_KIND(IDENTIFIER_DOLLAR, = TOKEN_CATEGORY_IDENTIFIER|DOLLAR_BEFORE) \
        \
    TOKEN_KIND(NATURAL_NUM_BIN, = TOKEN_CATEGORY_NATNUM|0) \
    TOKEN_KIND(NATURAL_NUM_OCT, = TOKEN_CATEGORY_NATNUM|1) \
    TOKEN_KIND(NATURAL_NUM_DEC, = TOKEN_CATEGORY_NATNUM|2) \
    TOKEN_KIND(NATURAL_NUM_HEX, = TOKEN_CATEGORY_NATNUM|3) \
        \
    TOKEN_KIND(NATURAL_NUM_BIN_EXT, = TOKEN_CATEGORY_NATNUM|0|TOKENKINDFLAG_EXTREME_NUMERAL) \
    TOKEN_KIND(NATURAL_NUM_OCT_EXT, = TOKEN_CATEGORY_NATNUM|1|TOKENKINDFLAG_EXTREME_NUMERAL) \
    TOKEN_KIND(NATURAL_NUM_DEC_EXT, = TOKEN_CATEGORY_NATNUM|2|TOKENKINDFLAG_EXTREME_NUMERAL) \
    TOKEN_KIND(NATURAL_NUM_HEX_EXT, = TOKEN_CATEGORY_NATNUM|3|TOKENKINDFLAG_EXTREME_NUMERAL) \
        \
    TOKEN_KIND(FLOATING_POINT_BIN, = TOKEN_CATEGORY_FLOATINGPT|0) \
    TOKEN_KIND(FLOATING_POINT_OCT, = TOKEN_CATEGORY_FLOATINGPT|1) \
    TOKEN_KIND(FLOATING_POINT_DEC, = TOKEN_CATEGORY_FLOATINGPT|2) \
    TOKEN_KIND(FLOATING_POINT_HEX, = TOKEN_CATEGORY_FLOATINGPT|3) \
        \
    TOKEN_KIND(FLOATING_POINT_BIN_EXT, = TOKEN_CATEGORY_FLOATINGPT|0|TOKENKINDFLAG_EXTREME_NUMERAL) \
    TOKEN_KIND(FLOATING_POINT_OCT_EXT, = TOKEN_CATEGORY_FLOATINGPT|1|TOKENKINDFLAG_EXTREME_NUMERAL) \
    TOKEN_KIND(FLOATING_POINT_DEC_EXT, = TOKEN_CATEGORY_FLOATINGPT|2|TOKENKINDFLAG_EXTREME_NUMERAL) \
    TOKEN_KIND(FLOATING_POINT_HEX_EXT, = TOKEN_CATEGORY_FLOATINGPT|3|TOKENKINDFLAG_EXTREME_NUMERAL) \
        \
    TOKEN_KIND(STRING, = TOKEN_CATEGORY_STRING|NOTHING_BEFORE) \
    TOKEN_KIND(CODEPOINT, = TOKEN_CATEGORY_STRING|PAN_BEFORE)

// Macro-magic... cf "A Note on enums-and-structs synchronization"
enum ETokenKind : u8 {
#define TOKEN_KIND(name, assignValue)      ETOK_KIND_ ## name    assignValue ,
    TOKEN_KINDS_EMITTER_
#undef TOKEN_KIND
};

// Macro-magic... cf "A Note on enums-and-structs synchronization"
constexpr char const* tTokenKindStr[] = {
#define TOKEN_KIND(name, assignValue)      #name ,
    TOKEN_KINDS_EMITTER_
#undef TOKEN_KIND
};
static const size_t COUNT_TOKEN_KINDS = sizeof(tTokenKindStr) / sizeof(char*);

local_func_inl bool is_token_symbol_or_keyword(ETokenKind eKind) { return (eKind & TOKEN_CATEGORY_IS_NONSYMBORKEY_MASK) == 0x00; }

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization"
// use together with    #define KEYWORD(syntaxName, name, assignValue)
#define KEYWORDS_EMITTER_ \
        \
    KEYWORD("or", OR, = 128) \
    KEYWORD("and", AND,) \
    KEYWORD("xor", XOR,) /* reserved, but not currently in grammar */ \
    KEYWORD("not", NOT,) \
        \
    KEYWORD("if", IF,) \
    KEYWORD("ifx", IFX,) \
    KEYWORD("then", THEN,) \
	KEYWORD("elif", ELIF,) \
	KEYWORD("else", ELSE,) \
	KEYWORD("for", FOR,) \
	KEYWORD("in", IN,) \
	KEYWORD("in_rev", INREV,) \
	KEYWORD("while", WHILE,) \
	KEYWORD("do", DO,) /* reserved, but not currently in grammar */ \
    KEYWORD("on_break", ONBREAK,) \
    KEYWORD("when_none", WHENNONE,) \
    KEYWORD("on_done", ONDONE,) \
    KEYWORD("case", CASE,) \
    KEYWORD("switch", SWITCH,) /* reserved, but not currently in grammar : 'case' is our keyword for C's 'switch' */ \
    KEYWORD("with", WITH,) /* reserved, but not currently in grammar */ \
    KEYWORD("let", LET,) /* reserved, but not currently in grammar */ \
        \
    KEYWORD("noop", NO_OP,) \
    KEYWORD("unreachable", UNREACH,) \
    KEYWORD("return", RETURN,) \
    KEYWORD("break", BREAK,) \
    KEYWORD("continue", CONTINUE,) \
    KEYWORD("to_else", TOELSE,) /* reserved, but not currently in grammar */ \
    KEYWORD("loop", LOOP,) /* reserved, but not currently in grammar */ \
    KEYWORD("defer", DEFER,) \
    KEYWORD("err_defer", ERRDEFER,) /* reserved, but not currently in grammar */ \
    KEYWORD("fail", FAIL,) /* reserved, but not currently in grammar */ \
    KEYWORD("fail_with", FAILWITH,) /* reserved, but not currently in grammar */ \
    KEYWORD("or_fail", ORFAIL,) /* reserved, but not currently in grammar */ \
    KEYWORD("or_fail_with", ORFAILWITH,) /* reserved, but not currently in grammar */ \
    KEYWORD("or_return", ORRETURN,) /* reserved, but not currently in grammar */ \
    KEYWORD("fallthrough", FALLTHROUGH,) /* reserved, but not currently in grammar */ \
        \
    KEYWORD("where", WHERE,) \
    KEYWORD("as", VARDECL,) \
        \
    KEYWORD("proc", PROC,) \
    KEYWORD("func", FUNC,) \
    KEYWORD("noctx_proc", NOCTXPROC,) \
    KEYWORD("noctx_func", NOCTXFUNC,) \
    KEYWORD("pure", PURE,) \
    KEYWORD("semipure", SEMIPURE,) \
    KEYWORD("macro", MACRO,) \
    KEYWORD("comptime_func", COMPTIMEFUNC,) \
    KEYWORD("enum", ENUM,) \
    KEYWORD("struct", STRUCT,) \
    KEYWORD("packed_struct", PACKED_STRUCT,) \
    KEYWORD("view", VIEW,) \
    KEYWORD("union", UNION,) \
    KEYWORD("hset", HSET,) \
    KEYWORD("hdic", HDIC,) \
        \
	KEYWORD("using", USING,) \
	KEYWORD("including", INCLUDING,) \
	KEYWORD("static", STATIC,) /* reserved, but not currently in grammar */ \
	KEYWORD("dynamic", DYNAMIC,) /* reserved, but not currently in grammar */ \
    KEYWORD("distinct", DISTINCT,) \
    KEYWORD("sizeof", SIZEOF,) \
    KEYWORD("alignof", ALIGNOF,) \
    KEYWORD("strideof", STRIDEOF,) /* reserved, but not currently in grammar */ \
    KEYWORD("typeof", TYPEOF,) \
    KEYWORD("typeinfoof", TYPEINFOOF,) \
    KEYWORD("typeidof", TYPEIDOF,) \
    KEYWORD("signature", SIGNATUREOF,) \
    KEYWORD("closure", CLOSUREOF,) /* reserved, but not currently in grammar */ \
    KEYWORD("as_closure", ASCLOSURE,) /* reserved, but not currently in grammar */ \
        \
	KEYWORD("#load", PAN_LOAD,) \
	KEYWORD("#import", PAN_IMPORT,) /* reserved, but not currently in grammar */   \
	KEYWORD("#package", PAN_PACKAGE,) /* reserved, but not currently in grammar */ \
	/*KEYWORD("#foreign", PAN_FOREIGN,) /* now a reserved word instead (builtin) */ \
	/*KEYWORD("#foreign_source", PAN_FOREIGN_SOURCE,) /* now a reserved word instead (builtin) */ \
	KEYWORD("#inline", PAN_INLINE,) /* reserved, but not currently in grammar */   \
	KEYWORD("#include", PAN_INCLUDE,) /* reserved, but not currently in grammar */ \
        \
	KEYWORD("#if", PAN_IF,) \
	KEYWORD("#elif", PAN_ELIF,) \
	KEYWORD("#else", PAN_ELSE,) \
	KEYWORD("#endif", PAN_ENDIF,) \
	KEYWORD("#scope", PAN_SCOPE,) \
	KEYWORD("public", PUBLIC,) \
	KEYWORD("package", PACKAGE,) \
	KEYWORD("private", PRIVATE,) \
	KEYWORD("#endscope", PAN_ENDSCOPE,)

// Macro-magic... cf "A Note on enums-and-structs synchronization"
enum EKeyword : u8 {
#define KEYWORD(syntaxName, name, assignValue)  EKEY_ ## name   assignValue ,
    KEYWORDS_EMITTER_
#undef KEYWORD
};

// Macro-magic... cf "A Note on enums-and-structs synchronization"
constexpr char const* tKeywordsStr[] = {
#define KEYWORD(syntaxName, name, assignValue)  syntaxName ,
    KEYWORDS_EMITTER_
#undef KEYWORD
};
static const size_t COUNT_KEYWORDS = sizeof(tKeywordsStr) / sizeof(char*);
static_assert(COUNT_KEYWORDS <= 128, "too many keywords for current 8b payload scheme");

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization"
// use together with    #define SYMBOL_PAYLOAD(syntaxName, name, assignValue, check)
#define SYMBOL_PAYLOADS_EMITTER_ \
        \
        /* first 32 positions [0..31] correspond to ascii control chars and have no representation here */ \
        \
    SYMBOL_PAYLOAD(" ", SPACE, = 32,             u8(' ')) /* should not appear as token, but is left here to be safe */ \
    SYMBOL_PAYLOAD("!", EXCLAMATION, ,           u8('!')) /* compact-modifier for some builtin type-kinds */ \
    SYMBOL_PAYLOAD("\"",DOUBLEQUOTE, ,           u8('"')) /* should usually not appear as an isolated token */ \
    SYMBOL_PAYLOAD("#", PAN, ,                   u8('#')) /* should usually not appear as an isolated token */ \
    SYMBOL_PAYLOAD("$", DOLLAR, ,                u8('$')) /* denotes comp-time statement, variable, or polymorphic type */ \
    SYMBOL_PAYLOAD("%", MODULUS, ,               u8('%')) /* standard modulus operator (float or int, from truncation of quotient towards-neg-infinity) */ \
    SYMBOL_PAYLOAD("&", AMPERSAND, ,             u8('&')) /* bitwise and binop ; address-of unop */ \
    SYMBOL_PAYLOAD("'", SINGLEQUOTE, ,           u8('\'')) /* mutable-modifier for some builtin type-kinds... or identifier-resolution exemption (for already-decl within declarations, or unhygienic declarations in macros) */ \
        \
    SYMBOL_PAYLOAD("(", OPENING_PARENS, ,        u8('(')) /* starts invocation-like forms, or encompasses parentized subexpressions */ \
    SYMBOL_PAYLOAD(")", CLOSING_PARENS, ,        u8(')')) /* ends invocation-like forms, parentized subexpressions, or map-literals */ \
    SYMBOL_PAYLOAD("*", STAR, ,                  u8('*')) /* standard multiplication */ \
    SYMBOL_PAYLOAD("+", PLUS, ,                  u8('+')) /* standard addition, or unary plus */ \
    SYMBOL_PAYLOAD(",", COMMA, ,                 u8(',')) /* ubiquitous expr-list separator */ \
    SYMBOL_PAYLOAD("-", MINUS, ,                 u8('-')) /* standard subtraction, or unary minus */ \
    SYMBOL_PAYLOAD(".", DOT, ,                   u8('.')) /* struct-or-namespace-descent special-binop */ \
    SYMBOL_PAYLOAD("/", SLASH, ,                 u8('/')) /* standard division */ \
        \
    SYMBOL_PAYLOAD("::",CONST_DECL, ,            48u) /* const declaration */ \
    SYMBOL_PAYLOAD("=>",IMPLIES, ,               49u) /* forced code block followup */ \
    SYMBOL_PAYLOAD("->",ARROW, ,                 50u) /* separator between input param list and return value list */ \
    SYMBOL_PAYLOAD("---", UNINITIALIZED, ,       51u) /* explicit left-uninitialized */ \
    SYMBOL_PAYLOAD("<:",SUBTAG_DECL, ,           52u) /* type subtag of ; not yet in grammar, reserved */ \
    SYMBOL_PAYLOAD(":=",ASSIGNMENT, ,            53u) /* assignment operator */ \
    SYMBOL_PAYLOAD(":<",SLICE_TO_EXCL, ,         54u) /* slice separator to excluded end */ \
    SYMBOL_PAYLOAD("!=",ARE_NOT_EQUAL, ,         55u) /* are-not-equal operator */ \
        \
    SYMBOL_PAYLOAD("",  _RESERVED_56, ,           56u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_57, ,           57u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD(":", COLON, ,                 u8(':')) /* label declaration, slice separator to included end */ \
    SYMBOL_PAYLOAD(";", SEMICOLON, ,             u8(';')) /* inlined-statement-chaining */ \
    SYMBOL_PAYLOAD("<", LESSER_THAN, ,           u8('<')) /* is-lesser-than operator */ \
    SYMBOL_PAYLOAD("=", SINGLE_EQ, ,             u8('=')) /* explicit parameter assignment */ \
    SYMBOL_PAYLOAD(">", GREATER_THAN, ,          u8('>')) /* is-greater-than operator */ \
    SYMBOL_PAYLOAD("?", QUESTION_MARK, ,         u8('?')) /* not yet in grammar */ \
        \
    SYMBOL_PAYLOAD("@", ATSIGN, ,                u8('@')) /* should usually not appear as an isolated token */ \
    SYMBOL_PAYLOAD("==",ARE_EQUAL, ,             65u) /* are-equal operator */ \
    SYMBOL_PAYLOAD("<=",LESSER_OR_EQ, ,          66u) /* is-lesser-or-eq operator */ \
    SYMBOL_PAYLOAD(">=",GREATER_OR_EQ, ,         67u) /* is-greater-or-eq operator */ \
    SYMBOL_PAYLOAD("<<",LEFT_SHIFT, ,            68u) /* left-shift operator (inserting zeros in lsb) */ \
    SYMBOL_PAYLOAD(">>",RIGHT_SHIFT, ,           69u) /* right-shift operator (inserting zeros or sign in lsb, depending on signedness of left operand) */ \
    SYMBOL_PAYLOAD("**",POW, ,                   70u) /* power operator */ \
    SYMBOL_PAYLOAD("+%",MODULO_ADD, ,            71u) /* add modulo 2^bits, 2's complement semantics if signed */ \
        \
    SYMBOL_PAYLOAD("-%",MODULO_SUB, ,            72u) /* sub modulo 2^bits, 2's complement semantics if signed */ \
    SYMBOL_PAYLOAD("*%",MODULO_MUL, ,            73u) /* mul modulo 2^bits, 2's complement semantics if signed */ \
    SYMBOL_PAYLOAD("/%",INT_QUOTIENT, ,          74u) /* integer-quotient from division between integers, truncated towards zero */ \
    SYMBOL_PAYLOAD("%%",INT_REMAINDER, ,         75u) /* rest of integer-division between integers (can be neg). different from % which is a towards-neg-infinity */ \
    SYMBOL_PAYLOAD("++",CONCAT, ,                76u) /* array-or-string concatenation operator */ \
    SYMBOL_PAYLOAD("",  _RESERVED_77, ,          77u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("..",RANGE_INCL, ,            78u) /* countable range-operator with included end */ \
    SYMBOL_PAYLOAD("..<", RANGE_EXCL, ,          79u) /* countable range-operator with excluded end */ \
        \
    SYMBOL_PAYLOAD(".[",OPENING_ARRAY_LIT, ,     80u) /* starts declaration of array-literals */ \
    SYMBOL_PAYLOAD(".{",OPENING_SET_LIT, ,       81u) /* starts declaration of set-literals */ \
    SYMBOL_PAYLOAD(".(",OPENING_MAP_LIT, ,       82u) /* starts declaration of map-literals */ \
    SYMBOL_PAYLOAD("@[",OPENING_DYNARRAY, ,      83u) /* starts declaration of dynamic-arrays */ \
    SYMBOL_PAYLOAD("'(",OPENING_LAM_STRICT, ,    84u) /* not yet supported */ \
    SYMBOL_PAYLOAD("@(",OPENING_LAM_CLOSURE, ,   85u) /* not yet supported */ \
    SYMBOL_PAYLOAD("^.",POINTER_DECL, ,          86u) /* type-modifier prefix to pointer */ \
    SYMBOL_PAYLOAD("",  _RESERVED_87, ,           87u) /* reserved, not yet in grammar */ \
        \
    SYMBOL_PAYLOAD("",  _RESERVED_88, ,           88u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_89, ,           89u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_90, ,           90u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("[", OPENING_BRACKET, ,       u8('[')) /* starts indexing-like forms, declaration of array-like types, or slicing op */ \
    SYMBOL_PAYLOAD("\\",BACKSLASH, ,             u8('\\')) /* forces multiline continuation */ \
    SYMBOL_PAYLOAD("]", CLOSING_BRACKET, ,       u8(']')) /* ends indexing-like forms, declaration of array-like types, slicing op, or array literals */ \
    SYMBOL_PAYLOAD("^", CARET, ,                 u8('^')) /* deref suffix */ \
    SYMBOL_PAYLOAD("_", UNDERSCORE, ,            u8('_')) /* not a symbol token (will be emitted as a reserved id instead), but left special here... */ \
        \
    SYMBOL_PAYLOAD("`", BACKTICK, ,              u8('`')) /* not yet in grammar */ \
    SYMBOL_PAYLOAD("+=",ADD_ASSIGN, ,            97u) /* op-and-assign for standard addition */ \
    SYMBOL_PAYLOAD("-=",SUB_ASSIGN, ,            98u) /* op-and-assign for standard subtraction */ \
    SYMBOL_PAYLOAD("*=",MUL_ASSIGN, ,            99u) /* op-and-assign for standard multiplication */ \
    SYMBOL_PAYLOAD("/=",DIV_ASSIGN, ,            100u) /* op-and-assign for standard division */ \
    SYMBOL_PAYLOAD("%=",MOD_ASSIGN, ,            101u) /* op-and-assign for standard modulus */ \
    SYMBOL_PAYLOAD("&=",BIT_AND_ASSIGN, ,        102u) /* op-and-assign for bitwise and */ \
    SYMBOL_PAYLOAD("|=",BIT_OR_ASSIGN, ,         103u) /* op-and-assign for bitwise or */ \
        \
    SYMBOL_PAYLOAD("~=",  BIT_XOR_ASSIGN, ,       104u) /* op-and-assign for bitwise xor */ \
    SYMBOL_PAYLOAD("<<=", LSH_ASSIGN, ,           105u) /* op-and-assign for left-shift */ \
    SYMBOL_PAYLOAD(">>=", RSH_ASSIGN, ,           106u) /* op-and-assign for right-shift */ \
    SYMBOL_PAYLOAD("**=", POW_ASSIGN, ,           107u) /* op-and-assign for pow */ \
    SYMBOL_PAYLOAD("+%=", MODULO_ADD_ASSIGN, ,    108u) /* op-and-assign for modulo add */ \
    SYMBOL_PAYLOAD("-%=", MODULO_SUB_ASSIGN, ,    109u) /* op-and-assign for modulo sub */ \
    SYMBOL_PAYLOAD("*%=", MODULO_MUL_ASSIGN, ,    110u) /* op-and-assign for modulo mul */ \
    SYMBOL_PAYLOAD("/%=", INT_QUO_ASSIGN, ,       111u) /* op-and-assign for integer quotient */ \
        \
    SYMBOL_PAYLOAD("%%=", INT_REM_ASSIGN, ,       112u) /* op-and-assign for integer remainder */ \
    SYMBOL_PAYLOAD("++=", CONCAT_ASSIGN, ,        113u) /* op-and-assign for concat */ \
    SYMBOL_PAYLOAD("",  _RESERVED_114, ,           114u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_115, ,           115u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_116, ,           116u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_117, ,           117u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_118, ,           118u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_119, ,           119u) /* reserved, not yet in grammar */ \
        \
    SYMBOL_PAYLOAD("",  _RESERVED_120, ,           120u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_121, ,           121u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("",  _RESERVED_122, ,           122u) /* reserved, not yet in grammar */ \
    SYMBOL_PAYLOAD("{", OPENING_CURLY, ,          u8('{')) /* starts declaration of struct literals */ \
    SYMBOL_PAYLOAD("|", PIPE, ,                   u8('|')) /* bitwise or binop */ \
    SYMBOL_PAYLOAD("}", CLOSING_CURLY, ,          u8('}')) /* ends declaration of struct or set literals */ \
    SYMBOL_PAYLOAD("~", TILDE, ,                  u8('~')) /* bitwise not unop, bitwise xor binop */ \
    SYMBOL_PAYLOAD("DEL",  DEL, ,                  127u) /* shall always stay 'reserved' */

// Macro-magic... cf "A Note on enums-and-structs synchronization"
// payloads associated with 'symbol' or 'multisymbol' token kinds
enum ESymbolPayload : u8 {
#define SYMBOL_PAYLOAD(syntaxName, name, assignValue, check)     ESYMB_ ## name   assignValue ,
    SYMBOL_PAYLOADS_EMITTER_
#undef SYMBOL_PAYLOAD
};

// Macro-magic... cf "A Note on enums-and-structs synchronization"
// Cheker for symbol payloads
#define SYMBOL_PAYLOAD(syntaxName, name, assignValue, check)     static_assert(ESYMB_ ## name == check, "Symbol payload enum position mismatch with explicit check : " syntaxName);
    SYMBOL_PAYLOADS_EMITTER_
#undef SYMBOL_PAYLOAD

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization"
// all payloads associated with either 'symbol', 'multisymbol' or 'keyword' token kinds
enum ETokenPayload : u8 {
#define SYMBOL_PAYLOAD(syntaxName, name, assignValue, check)     ETOK_ ## name   assignValue ,
    SYMBOL_PAYLOADS_EMITTER_
#undef SYMBOL_PAYLOAD
#define KEYWORD(syntaxName, name, assignValue)                   ETOK_ ## name   assignValue ,
    KEYWORDS_EMITTER_
#undef KEYWORD

    //
    // Additional aliases for some symbols, in a more semantically-oriented manner than the above
    //

    ETOK_ADD = ETOK_PLUS,
    ETOK_SUB = ETOK_MINUS,
    ETOK_MUL = ETOK_STAR,
    ETOK_DIV = ETOK_SLASH,
    ETOK_MOD = ETOK_MODULUS,

    ETOK_BIT_AND = ETOK_AMPERSAND,
    ETOK_BIT_OR = ETOK_PIPE,
    ETOK_BIT_NOT = ETOK_TILDE,
    ETOK_BIT_XOR = ETOK_TILDE,

    ETOK_BOOL_AND = ETOK_AND,
    ETOK_BOOL_OR = ETOK_OR,
    ETOK_BOOL_NOT = ETOK_NOT,

    ETOK_UNARY_PLUS = ETOK_PLUS,
    ETOK_UNARY_MINUS = ETOK_MINUS,
    ETOK_UNARY_MODMINUS = ETOK_MODULO_SUB,
    ETOK_DEREFERENCE = ETOK_CARET,
    ETOK_ADDRESSOF = ETOK_AMPERSAND,
    ETOK_ARRAYLIKE_DECL = ETOK_OPENING_BRACKET,
    ETOK_COMPACT_DECL = ETOK_EXCLAMATION,
    
    ETOK_INVOCATION = ETOK_OPENING_PARENS,
    ETOK_INDEXING = ETOK_OPENING_BRACKET,
    ETOK_OPENING_STRUCT_LIT = ETOK_OPENING_CURLY,

    ETOK_SLICING = ETOK_COLON,

};

// Macro-magic... cf "A Note on enums-and-structs synchronization"
//
constexpr char const* inval_payload = "<inval>";
// debug-representation for symbol payload
constexpr char const* tStandardPayloadsStr[] = {
    // first 32 positions [0..31] are ruled out
    inval_payload, inval_payload, inval_payload, inval_payload,   inval_payload, inval_payload, inval_payload, inval_payload,
    inval_payload, inval_payload, inval_payload, inval_payload,   inval_payload, inval_payload, inval_payload, inval_payload,
    inval_payload, inval_payload, inval_payload, inval_payload,   inval_payload, inval_payload, inval_payload, inval_payload,
    inval_payload, inval_payload, inval_payload, inval_payload,   inval_payload, inval_payload, inval_payload, inval_payload,
    // then comes ASCII symbols (or multisynmbols in non-symbol ASCII pos) from 32 to 127
#define SYMBOL_PAYLOAD(syntaxName, name, assignValue, check)    syntaxName ,
    SYMBOL_PAYLOADS_EMITTER_
#undef SYMBOL_PAYLOAD
    // then comes keywords from 128 onwards
#define KEYWORD(syntaxName, name, assignValue)                  syntaxName ,
    KEYWORDS_EMITTER_
#undef KEYWORD
};
static const size_t COUNT_STANDARD_PAYLOADS = sizeof(tStandardPayloadsStr) / sizeof(char*);
static_assert(COUNT_STANDARD_PAYLOADS == 128u + COUNT_KEYWORDS, "assumption violation on standard payload");

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization"
// use together with    #define RESERVED_WORD(syntaxName, name, specialExpand)
#define RESERVED_WORDS_EMITTER_ \
        \
    RESERVED_WORD("", INVALID_ID, RESERVED_WORD_VALUE(BOOL, FALSITY)) /* cannot match anything */ \
        \
    RESERVED_WORD("_", SINK, RESERVED_WORD_VALUE(BOOL, FALSITY)) /* the sink */ \
        \
        /* various common aliases or types */ \
    RESERVED_WORD("rawptr", RAWPTR, RESERVED_WORD_TYPE(RAWPTR)) /* a pointer-sized value without a pointed-type */ \
    RESERVED_WORD("reg", REG, RESERVED_WORD_TYPE_ALIAS(REG))       /* a raw integral of native register size for target architecture */ \
    RESERVED_WORD("int", INT, RESERVED_WORD_TYPE(INT))       /* 32b signed with special semantics of reg-sizability, and when-inline-expansion polymorphism */ \
    RESERVED_WORD("nat", NAT, RESERVED_WORD_TYPE(NAT))       /* 32b unsigned with special semantics of reg-sizability, and when-inline-expansion polymorphism */ \
    RESERVED_WORD("byte", BYTE, RESERVED_WORD_TYPE_ALIAS(BYTE))     /* really an alias for u8 */ \
    RESERVED_WORD("float", FLOAT, RESERVED_WORD_TYPE_ALIAS(FLOAT))   /* an alias for f64 */ \
        \
    RESERVED_WORD("bool", BOOL, RESERVED_WORD_TYPE(BOOL))     /* the boolean type as smallest 'r8' */ \
        \
        /* a raw integral of at-least 32b footprint, and larger if required for holding a pointer on target arch */ \
    RESERVED_WORD("rptrmin32", RPTRMIN32, RESERVED_WORD_TYPE_ALIAS(RPTRMIN32)) \
    RESERVED_WORD("uptrmin32", UPTRMIN32, RESERVED_WORD_TYPE_ALIAS(UPTRMIN32)) \
    RESERVED_WORD("iptrmin32", IPTRMIN32, RESERVED_WORD_TYPE_ALIAS(IPTRMIN32)) \
        \
        /* a raw integral of at-least 64b footprint, and larger if required for holding a pointer on target arch. */ \
        /* higher-than-this is not required since we can use fix-128-sizes instead, */ \
        /* since all pointer-size ever to be invented are not expected to be higher than 128 */ \
    RESERVED_WORD("rptrmin64", RPTRMIN64, RESERVED_WORD_TYPE_ALIAS(RPTRMIN64)) \
    RESERVED_WORD("uptrmin64", UPTRMIN64, RESERVED_WORD_TYPE_ALIAS(UPTRMIN64)) \
    RESERVED_WORD("iptrmin64", IPTRMIN64, RESERVED_WORD_TYPE_ALIAS(IPTRMIN64)) \
        \
    RESERVED_WORD("compint", COMPINT, RESERVED_WORD_TYPE(COMPINT)) /* an integer of arbitrary magnitude, only available at compile-time. */ \
    RESERVED_WORD("unsizedfloat", FLOAT_LIT, RESERVED_WORD_TYPE(FLOAT_LIT)) /* the internal type of float literals, with auto-sizing on use. Not expected to be defined by end-users, but why not */ \
    RESERVED_WORD("xfloat", XFLOAT, RESERVED_WORD_TYPE(XFLOAT))   /* the internal type of extreme-precision floats, available at compile-time */ \
        \
    RESERVED_WORD("string", STRING, RESERVED_WORD_TYPE(STRINGVIEW)) /* a ffstring if has the 'compacted' modifier ; a stringview otherwise. */ \
    RESERVED_WORD("ownedstring", OWNEDSTRING, RESERVED_WORD_TYPE(OWNEDSTRING))   /* an ownedstring if has the 'compacted' modifier ; a fatstring otherwise. */ \
    RESERVED_WORD("codepoint", CODEPOINT, RESERVED_WORD_TYPE(CODEPOINT)) /* unicode codepoint stored on a 32b raw. */ \
        \
        /* non-runtime types */ \
    RESERVED_WORD("#any", PAN_ANY, RESERVED_WORD_TYPE(ANY)) /* type of an 'any type' struct, for runtime polymorphism */ \
    RESERVED_WORD("#node", PAN_NODE, RESERVED_WORD_TYPE(ASTNODE)) /* type of an AST node for macros definitions */ \
    RESERVED_WORD("#block", PAN_CODESEQ, RESERVED_WORD_TYPE(ASTSEQ)) /* type of an AST block for macros definitions */ \
    RESERVED_WORD("#file", PAN_FILE, RESERVED_WORD_TYPE(SOURCEFILE)) /* type of a source-file for macros definitions */ \
    RESERVED_WORD("#foreign_source_t", FOREIGN_SOURCE_T, RESERVED_WORD_TYPE(FOREIGN_SOURCE)) /* the type of foreign-source declarations (comptime_only) */ \
    RESERVED_WORD("#namespace", NAMESPACE, RESERVED_WORD_TYPE(NAMESPACE)) /* the type of namespaces (comptime-only) */ \
    RESERVED_WORD("#type", TYPE, RESERVED_WORD_TYPE(TYPE)) /* the type of types (comptime-only) */ \
    RESERVED_WORD("#type_info", TYPE_INFO, RESERVED_WORD_TYPE(TYPE_INFO)) /* the type-info struct */ \
    RESERVED_WORD("#type_id", TYPE_ID, RESERVED_WORD_TYPE(TYPE_ID)) /* the type-id */ \
        \
        /* values */ \
    RESERVED_WORD("true", TRUTH, RESERVED_WORD_VALUE(BOOL, TRUTH)) /* boolean const of truth */ \
    RESERVED_WORD("false", FALSITY, RESERVED_WORD_VALUE(BOOL, FALSITY)) /* boolean const of falsity */ \
    RESERVED_WORD("infinity", INF, RESERVED_WORD_VALUE(FLOAT_LIT, INF)) /* floating point const of +infinity. prefix with unary '-' for negative. */ \
    RESERVED_WORD("nan", QNAN, RESERVED_WORD_VALUE(FLOAT_LIT, QNAN)) /* floating point const of standard quiet nan */ \
    RESERVED_WORD("snan", SNAN, RESERVED_WORD_VALUE(FLOAT_LIT, SNAN)) /* floating point const of standard signalling nan */ \
        \
        /* core builtins */ \
    RESERVED_WORD("#foreign", FOREIGN, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, FOREIGN)) \
    RESERVED_WORD("#foreign_source", FOREIGN_SOURCE, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, FOREIGN_SOURCE)) \
        \
        /* builtins */ \
    /* RESERVED_WORD("print", PRINT, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, PRINT)) /* tmp builtin, printing value to std output */ \
    RESERVED_WORD("memcpy", MEMCPY, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, MEMCPY)) \
    RESERVED_WORD("rev_memcpy", REV_MEMCPY, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, REV_MEMCPY)) \
    RESERVED_WORD("zeromem", ZEROMEM, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, ZEROMEM)) \
    RESERVED_WORD("memset", MEMSET, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, MEMSET)) \
    RESERVED_WORD("memcmp", MEMCMP, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, MEMCMP)) \
    RESERVED_WORD("rdtsc", RDTSC, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, RDTSC)) \
    RESERVED_WORD("#trans", TRANS, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, SPECIAL_TRANSMUTE_CAST)) \
    RESERVED_WORD("#trunc", TRUNC, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, SPECIAL_TRUNCATING_CAST)) \
    RESERVED_WORD("#sat", SAT, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, SPECIAL_SATURATING_CAST)) \
    RESERVED_WORD("#ptr", PTR, RESERVED_WORD_SPECIAL_VALUE(BUILTIN, SPECIAL_POINTER_CAST)) \
        \
        /* raw-integral types, of size 8b to 1024b */ \
    RESERVED_WORD("r8", R8, RESERVED_WORD_TYPE(R8)) \
    RESERVED_WORD("r16", R16, RESERVED_WORD_TYPE(R16)) \
    RESERVED_WORD("r32", R32, RESERVED_WORD_TYPE(R32)) \
    RESERVED_WORD("r64", R64, RESERVED_WORD_TYPE(R64)) \
    RESERVED_WORD("r128", R128, RESERVED_WORD_TYPE(R128)) \
    RESERVED_WORD("r256", R256, RESERVED_WORD_TYPE(R256)) \
    RESERVED_WORD("r512", R512, RESERVED_WORD_TYPE(R512)) \
    RESERVED_WORD("r1024", R1024, RESERVED_WORD_TYPE(R1024)) \
        \
        /* unsigned-integral types, of size 8b to 256b */ \
    RESERVED_WORD("u8", U8, RESERVED_WORD_TYPE(U8)) \
    RESERVED_WORD("u16", U16, RESERVED_WORD_TYPE(U16)) \
    RESERVED_WORD("u32", U32, RESERVED_WORD_TYPE(U32)) \
    RESERVED_WORD("u64", U64, RESERVED_WORD_TYPE(U64)) \
    RESERVED_WORD("u128", U128, RESERVED_WORD_TYPE(U128)) \
    RESERVED_WORD("u256", U256, RESERVED_WORD_TYPE(U256)) \
        \
        /* signed-integral types, of size 8b to 256b. Ensured 2's complement */ \
    RESERVED_WORD("i8", I8, RESERVED_WORD_TYPE(I8)) \
    RESERVED_WORD("i16", I16, RESERVED_WORD_TYPE(I16)) \
    RESERVED_WORD("i32", I32, RESERVED_WORD_TYPE(I32)) \
    RESERVED_WORD("i64", I64, RESERVED_WORD_TYPE(I64)) \
    RESERVED_WORD("i128", I128, RESERVED_WORD_TYPE(I128)) \
    RESERVED_WORD("i256", I256, RESERVED_WORD_TYPE(I256)) \
        \
        /* floating point types, of size 16b to 256b */ \
    RESERVED_WORD("f16", F16, RESERVED_WORD_TYPE(F16)) \
    RESERVED_WORD("f32", F32, RESERVED_WORD_TYPE(F32)) \
    RESERVED_WORD("f64", F64, RESERVED_WORD_TYPE(F64)) \
    RESERVED_WORD("f128", F128, RESERVED_WORD_TYPE(F128)) \
    RESERVED_WORD("f256", F256, RESERVED_WORD_TYPE(F256)) \
        \
        /* vector types, of 2 elements */ \
    RESERVED_WORD("r8x2", R8x2, RESERVED_WORD_TYPE(R8x2)) \
    RESERVED_WORD("u8x2", U8x2, RESERVED_WORD_TYPE(U8x2)) \
    RESERVED_WORD("i8x2", I8x2, RESERVED_WORD_TYPE(I8x2)) \
        \
    RESERVED_WORD("r16x2", R16x2, RESERVED_WORD_TYPE(R16x2)) \
    RESERVED_WORD("u16x2", U16x2, RESERVED_WORD_TYPE(U16x2)) \
    RESERVED_WORD("i16x2", I16x2, RESERVED_WORD_TYPE(I16x2)) \
    RESERVED_WORD("f16x2", F16x2, RESERVED_WORD_TYPE(F16x2)) \
        \
    RESERVED_WORD("r32x2", R32x2, RESERVED_WORD_TYPE(R32x2)) \
    RESERVED_WORD("u32x2", U32x2, RESERVED_WORD_TYPE(U32x2)) \
    RESERVED_WORD("i32x2", I32x2, RESERVED_WORD_TYPE(I32x2)) \
    RESERVED_WORD("f32x2", F32x2, RESERVED_WORD_TYPE(F32x2)) \
        \
    RESERVED_WORD("r64x2", R64x2, RESERVED_WORD_TYPE(R64x2)) \
    RESERVED_WORD("u64x2", U64x2, RESERVED_WORD_TYPE(U64x2)) \
    RESERVED_WORD("i64x2", I64x2, RESERVED_WORD_TYPE(I64x2)) \
    RESERVED_WORD("f64x2", F64x2, RESERVED_WORD_TYPE(F64x2)) \
        \
    RESERVED_WORD("r128x2", R128x2, RESERVED_WORD_TYPE(R128x2)) \
    RESERVED_WORD("u128x2", U128x2, RESERVED_WORD_TYPE(U128x2)) \
    RESERVED_WORD("i128x2", I128x2, RESERVED_WORD_TYPE(I128x2)) \
    RESERVED_WORD("f128x2", F128x2, RESERVED_WORD_TYPE(F128x2)) \
        \
    RESERVED_WORD("r256x2", R256x2, RESERVED_WORD_TYPE(R256x2)) \
    RESERVED_WORD("u256x2", U256x2, RESERVED_WORD_TYPE(U256x2)) \
    RESERVED_WORD("i256x2", I256x2, RESERVED_WORD_TYPE(I256x2)) \
    RESERVED_WORD("f256x2", F256x2, RESERVED_WORD_TYPE(F256x2)) \
        \
        /* vector types, of 4 elements */ \
    RESERVED_WORD("r8x4", R8x4, RESERVED_WORD_TYPE(R8x4)) \
    RESERVED_WORD("u8x4", U8x4, RESERVED_WORD_TYPE(U8x4)) \
    RESERVED_WORD("i8x4", I8x4, RESERVED_WORD_TYPE(I8x4)) \
        \
    RESERVED_WORD("r16x4", R16x4, RESERVED_WORD_TYPE(R16x4)) \
    RESERVED_WORD("u16x4", U16x4, RESERVED_WORD_TYPE(U16x4)) \
    RESERVED_WORD("i16x4", I16x4, RESERVED_WORD_TYPE(I16x4)) \
    RESERVED_WORD("f16x4", F16x4, RESERVED_WORD_TYPE(F16x4)) \
        \
    RESERVED_WORD("r32x4", R32x4, RESERVED_WORD_TYPE(R32x4)) \
    RESERVED_WORD("u32x4", U32x4, RESERVED_WORD_TYPE(U32x4)) \
    RESERVED_WORD("i32x4", I32x4, RESERVED_WORD_TYPE(I32x4)) \
    RESERVED_WORD("f32x4", F32x4, RESERVED_WORD_TYPE(F32x4)) \
        \
    RESERVED_WORD("r64x4", R64x4, RESERVED_WORD_TYPE(R64x4)) \
    RESERVED_WORD("u64x4", U64x4, RESERVED_WORD_TYPE(U64x4)) \
    RESERVED_WORD("i64x4", I64x4, RESERVED_WORD_TYPE(I64x4)) \
    RESERVED_WORD("f64x4", F64x4, RESERVED_WORD_TYPE(F64x4)) \
        \
    RESERVED_WORD("r128x4", R128x4, RESERVED_WORD_TYPE(R128x4)) \
    RESERVED_WORD("u128x4", U128x4, RESERVED_WORD_TYPE(U128x4)) \
    RESERVED_WORD("i128x4", I128x4, RESERVED_WORD_TYPE(I128x4)) \
    RESERVED_WORD("f128x4", F128x4, RESERVED_WORD_TYPE(F128x4)) \
        \
    RESERVED_WORD("r256x4", R256x4, RESERVED_WORD_TYPE(R256x4)) \
    RESERVED_WORD("u256x4", U256x4, RESERVED_WORD_TYPE(U256x4)) \
    RESERVED_WORD("i256x4", I256x4, RESERVED_WORD_TYPE(I256x4)) \
    RESERVED_WORD("f256x4", F256x4, RESERVED_WORD_TYPE(F256x4)) \
        \
        /* vector types, of 8 elements */ \
    RESERVED_WORD("r8x8", R8x8, RESERVED_WORD_TYPE(R8x8)) \
    RESERVED_WORD("u8x8", U8x8, RESERVED_WORD_TYPE(U8x8)) \
    RESERVED_WORD("i8x8", I8x8, RESERVED_WORD_TYPE(I8x8)) \
        \
    RESERVED_WORD("r16x8", R16x8, RESERVED_WORD_TYPE(R16x8)) \
    RESERVED_WORD("u16x8", U16x8, RESERVED_WORD_TYPE(U16x8)) \
    RESERVED_WORD("i16x8", I16x8, RESERVED_WORD_TYPE(I16x8)) \
    RESERVED_WORD("f16x8", F16x8, RESERVED_WORD_TYPE(F16x8)) \
        \
    RESERVED_WORD("r32x8", R32x8, RESERVED_WORD_TYPE(R32x8)) \
    RESERVED_WORD("u32x8", U32x8, RESERVED_WORD_TYPE(U32x8)) \
    RESERVED_WORD("i32x8", I32x8, RESERVED_WORD_TYPE(I32x8)) \
    RESERVED_WORD("f32x8", F32x8, RESERVED_WORD_TYPE(F32x8)) \
        \
    RESERVED_WORD("r64x8", R64x8, RESERVED_WORD_TYPE(R64x8)) \
    RESERVED_WORD("u64x8", U64x8, RESERVED_WORD_TYPE(U64x8)) \
    RESERVED_WORD("i64x8", I64x8, RESERVED_WORD_TYPE(I64x8)) \
    RESERVED_WORD("f64x8", F64x8, RESERVED_WORD_TYPE(F64x8)) \
        \
    RESERVED_WORD("r128x8", R128x8, RESERVED_WORD_TYPE(R128x8)) \
    RESERVED_WORD("u128x8", U128x8, RESERVED_WORD_TYPE(U128x8)) \
    RESERVED_WORD("i128x8", I128x8, RESERVED_WORD_TYPE(I128x8)) \
    RESERVED_WORD("f128x8", F128x8, RESERVED_WORD_TYPE(F128x8)) \
        \
        /* vector types, of 16 elements */ \
    RESERVED_WORD("r8x16", R8x16, RESERVED_WORD_TYPE(R8x16)) \
    RESERVED_WORD("u8x16", U8x16, RESERVED_WORD_TYPE(U8x16)) \
    RESERVED_WORD("i8x16", I8x16, RESERVED_WORD_TYPE(I8x16)) \
        \
    RESERVED_WORD("r16x16", R16x16, RESERVED_WORD_TYPE(R16x16)) \
    RESERVED_WORD("u16x16", U16x16, RESERVED_WORD_TYPE(U16x16)) \
    RESERVED_WORD("i16x16", I16x16, RESERVED_WORD_TYPE(I16x16)) \
    RESERVED_WORD("f16x16", F16x16, RESERVED_WORD_TYPE(F16x16)) \
        \
    RESERVED_WORD("r32x16", R32x16, RESERVED_WORD_TYPE(R32x16)) \
    RESERVED_WORD("u32x16", U32x16, RESERVED_WORD_TYPE(U32x16)) \
    RESERVED_WORD("i32x16", I32x16, RESERVED_WORD_TYPE(I32x16)) \
    RESERVED_WORD("f32x16", F32x16, RESERVED_WORD_TYPE(F32x16)) \
        \
    RESERVED_WORD("r64x16", R64x16, RESERVED_WORD_TYPE(R64x16)) \
    RESERVED_WORD("u64x16", U64x16, RESERVED_WORD_TYPE(U64x16)) \
    RESERVED_WORD("i64x16", I64x16, RESERVED_WORD_TYPE(I64x16)) \
    RESERVED_WORD("f64x16", F64x16, RESERVED_WORD_TYPE(F64x16)) \
        \
        /* vector types, of 32 elements */ \
    RESERVED_WORD("r8x32", R8x32, RESERVED_WORD_TYPE(R8x32)) \
    RESERVED_WORD("u8x32", U8x32, RESERVED_WORD_TYPE(U8x32)) \
    RESERVED_WORD("i8x32", I8x32, RESERVED_WORD_TYPE(I8x32)) \
        \
    RESERVED_WORD("r16x32", R16x32, RESERVED_WORD_TYPE(R16x32)) \
    RESERVED_WORD("u16x32", U16x32, RESERVED_WORD_TYPE(U16x32)) \
    RESERVED_WORD("i16x32", I16x32, RESERVED_WORD_TYPE(I16x32)) \
    RESERVED_WORD("f16x32", F16x32, RESERVED_WORD_TYPE(F16x32)) \
        \
    RESERVED_WORD("r32x32", R32x32, RESERVED_WORD_TYPE(R32x32)) \
    RESERVED_WORD("u32x32", U32x32, RESERVED_WORD_TYPE(U32x32)) \
    RESERVED_WORD("i32x32", I32x32, RESERVED_WORD_TYPE(I32x32)) \
    RESERVED_WORD("f32x32", F32x32, RESERVED_WORD_TYPE(F32x32)) \
        \
        /* vector types, of 64 elements */ \
    RESERVED_WORD("r8x64", R8x64, RESERVED_WORD_TYPE(R8x64)) \
    RESERVED_WORD("u8x64", U8x64, RESERVED_WORD_TYPE(U8x64)) \
    RESERVED_WORD("i8x64", I8x64, RESERVED_WORD_TYPE(I8x64)) \
        \
    RESERVED_WORD("r16x64", R16x64, RESERVED_WORD_TYPE(R16x64)) \
    RESERVED_WORD("u16x64", U16x64, RESERVED_WORD_TYPE(U16x64)) \
    RESERVED_WORD("i16x64", I16x64, RESERVED_WORD_TYPE(I16x64)) \
    RESERVED_WORD("f16x64", F16x64, RESERVED_WORD_TYPE(F16x64)) \
        \
        /* vector types, of 128 elements */ \
    RESERVED_WORD("r8x128", R8x128, RESERVED_WORD_TYPE(R8x128)) \
    RESERVED_WORD("u8x128", U8x128, RESERVED_WORD_TYPE(U8x128)) \
    RESERVED_WORD("i8x128", I8x128, RESERVED_WORD_TYPE(I8x128)) \
        \
    RESERVED_WORD("void", VOID, RESERVED_WORD_TYPE(VOID)) /* the void type */

// Macro-magic... cf "A Note on enums-and-structs synchronization"
enum EReservedWord : u8 {
#define RESERVED_WORD(syntaxName, name, specialExpand)  ERES_ ## name ,
    RESERVED_WORDS_EMITTER_
#undef RESERVED_WORD
};

// Macro-magic... cf "A Note on enums-and-structs synchronization"
constexpr char const* tReservedWordsStr[] = {
#define RESERVED_WORD(syntaxName, name, specialExpand)  syntaxName ,
    RESERVED_WORDS_EMITTER_
#undef RESERVED_WORD
};
static const size_t COUNT_RESERVED_WORDS = sizeof(tReservedWordsStr) / sizeof(char*);


#endif // LOCLIB_TOKENIZER_ENUMS_H_

