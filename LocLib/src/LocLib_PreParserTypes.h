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

#ifndef LOCLIB_PRE_PARSER_TYPES_H_
#define LOCLIB_PRE_PARSER_TYPES_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_Token.h"

local_func_inl bool is_token_symbol_or_keyword(Token token) { return is_token_symbol_or_keyword(ETokenKind(u8(token.uTokenPayloadAndKind))); }

struct TokenRef {
    Token token;
    u32 uLine;
    u16 uIndex;
    u16 uBytesFromLineStart;
};

static const TokenRef pivotTokenNone = {};

enum EPreNodeFlags {
    EPRENODEFLAG_LITERAL_IS_EMBEDDED64     = 0x0001,
    EPRENODEFLAG_IS_COMPTIME               = 0x0002,
};

// predecl
struct PreAstModifierNode;

struct PreAstNode {
    u8  uNodeKind;  // ENodeKind
    u8  _pad0;
    u16 uNodeFlags;
    i32 iCountSubNodesOrNegErr;
    TokenRef pivotalToken;
    union {
        PreAstNode* primaryChildNode;
        u64 primaryPayload;
    };
    union {
        TokenRef secondaryToken;
        PreAstNode* secondaryChildNode;
        u64 secondaryPayload;
    };
    PreAstModifierNode* firstPreModifier;
    PreAstModifierNode* firstPostModifier;
};

struct PreAstModifierNode {
    TokenRef pivotalToken;
    TokenRef optOpeningParens;
    PreAstNode* valueExpression;
    PreAstModifierNode* next;
};

enum EBlockSpawningState {
    EBLOCK_SPAWNING_NONE,
    EBLOCK_SPAWNING_EXPECTED,
    EBLOCK_SPAWNING_PAN_DIRECTIVE,
};

enum EStatementFlags {
    ESTATEMENTFLAGS_COMPTIME = u8(EPRENODEFLAG_IS_COMPTIME),
    ESTATEMENTFLAGS_REVERSE_FOR = 0x10,
    ESTATEMENTFLAGS_BLOCK_ENDING_PAN_DIRECTIVE = 0x20,
};

struct PreStatement {
    u8  uExpectedNextBlockSpawning;   // EBlockSpawningState
    u8  uStatementFlags;
    u8  uStatementKind;               // EStatementKind
    u8  _pad0;
    u16 uFirstTokenIndexOnLine;
    u16 uLastTokenIndexOnLine;
    PreAstNode*   pMainNode;
    PreAstNode*   pSecondaryNode;
    TokenRef      pivotToken1;
    TokenRef      pivotToken2;
    PreAstModifierNode* firstPreStatementModifier;
    PreAstModifierNode* firstPostStatementModifier;
    PreStatement* pLhsStatementWhenInlined;
    u32 uCountModifierNodes;
    u32 uCountParamNodesWithinModifiers;
    i32 iStartLine;
    i32 iLineOfLastToken;
    const u8* pStartingByteOnLine;
    u32 uRefStartOfError;
    u16 uByteCountOnLineToFirst;
    u16 _pad1;
};

struct StatementParserState {
    Token* pLineTokens;
    Token* pLineTokenEnd;
    Token* pCurrentToken;
    const u8* pStartOfLineAfterIndent;
    WorkerDesc* pWorker;
    i32 iInlinedRootStartLine;
    i32 iCurrentlyParsedLine;
    u8  uCurrentBlockTabIndents;
    u8  uCurrentLineTabIndents;
    u16 uIndexOfLastRegisteredToken;
    i32 iLineOfLastRegisteredToken;
    PreAstModifierNode* inFlightModifierNodes; // will be applied to nextExprOrStatement;
    u32 uCountModifierNodes;
    u32 uCountParamNodesWithinModifiers;
    u8 uCurrentLineIndent;
    u8 _pad0;
    u16 uCurrentBytesOnLine;
    u32 uRefStartOfError;
};

struct WholeProgramCompilationState;
struct ParserParams {

    StatementParserState    parserState;

    Arena  preparsingArena;
    SourceFileDescAndState* pSourceFile;

    TokenizerClosure        tokenizer;

    LocLib_OS_WrapperFunctions*     pOsFuncs;
    WholeProgramCompilationState*   pProgramCompState;
    LocLib_CompilationParams*       pCompilationParams;
    LocLib_CompilationResults*      oCompilationResults;

};

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define NODE_KIND(name, dbgName)
#define NODE_KINDS_EMITTER_ \
        \
        /* atomic expressions */ \
    NODE_KIND(ATOMICEXPR_NATURAL_NUMBER_LITERAL, "<NATNUM>") \
    NODE_KIND(ATOMICEXPR_FLOATING_POINT_LITERAL, "<FLT_PT>") \
    NODE_KIND(ATOMICEXPR_STRING_LITERAL,         "<STRING>") \
    NODE_KIND(ATOMICEXPR_CODEPOINT_LITERAL,      "<CODEPT>") \
    NODE_KIND(ATOMICEXPR_IDENTIFIER,             "<IDENTF>") \
    NODE_KIND(ATOMICEXPR_SPECIAL,                "<SPECTK>") \
        \
        /* bread-and-butter expressions */ \
    NODE_KIND(EXPR_PARENTISED,                   "<PARENS>") \
    NODE_KIND(EXPR_BINARYOP,                     "<BIN-OP>") \
    NODE_KIND(EXPR_SPECIAL_BINARYOP,             "<SBINOP>") /* Pre-parser only, shall all convert to something else as post */ \
    NODE_KIND(EXPR_BOOL_BINARYOP,                "<BOO-BO>") /* Post-parser only, preparses as binaryop */ \
    NODE_KIND(EXPR_EQ_CMP_BINARYOP,              "<EQ--BO>") /* Post-parser only, preparses as binaryop */ \
    NODE_KIND(EXPR_ORD_CMP_BINARYOP,             "<ORD-BO>") /* Post-parser only, preparses as binaryop */ \
    NODE_KIND(EXPR_UNARYOP,                      "<UN--OP>") \
    NODE_KIND(EXPR_TYPE_CTOR,                    "<PSU-OP>") /* Post-parser only, preparses as unaryop */ \
    NODE_KIND(EXPR_BOOL_NOT,                     "<BL-NOT>") \
    NODE_KIND(EXPR_INVOCATION_FORM,              "<INVOC(>") /* Post-parser only, preparses as special_binaryop */ \
    NODE_KIND(EXPR_DOT_DESCENT,                  "<DOT.DS>") /* Post-parser only, preparses as special_binaryop */ \
    NODE_KIND(EXPR_INDEXING,                     "<INDEX[>") /* Post-parser only, preparses as special_binaryop */ \
    NODE_KIND(EXPR_DEREF,                        "<DEREF^>") /* Post-parser only, preparses as special_binaryop */ \
    NODE_KIND(EXPR_TERNARY_IF,                   "<TERNIF>") \
    NODE_KIND(EXPR_LAMBDA,                       "<LAMBDA>") /* Post-parser only, if '->' found within parens */ \
    NODE_KIND(EXPR_SINGLE_EQ,                    "<LHV=VA>") \
    NODE_KIND(EXPR_IN,                           "<--IN-->") /* Post-parser only, preparses as binaryop */ \
    NODE_KIND(EXPR_RANGE,                        "<RA..NG>") /* Post-parser only, preparse as binary or unary op */ \
    NODE_KIND(EXPR_SOMETHINGOF,                  "<SMTHOF>") /* sizeof|alignof|strideof.. (<type>): Post-parser only, preparses as unary */ \
    NODE_KIND(EXPR_PROCLIKE_DEF,                 "<PROC-D>") /* composed of in,out wrapper, then optional where */ \
    NODE_KIND(EXPR_OTHER_DEF,                    "<OTHR-D>") /* structs, views, unions, enums... */ \
    NODE_KIND(EXPR_LOAD,                         "<-LOAD->") /* #load <filename-string> */ \
    NODE_KIND(EXPR_FOREIGN,                      "<FOREIG>") /* #foreign <foreign-source> */ \
    NODE_KIND(EXPR_USING,                        "<USING->") /* using expr or statement */ \
    NODE_KIND(EXPR_SET_DECL,                     "<SETDCL>") /* hset(type) */ \
        \
        /* At post-parser phase, the following encompasses possible preceeding type expressions */ \
    NODE_KIND(EXPR_CURLYINIT,                    "<CURINI>") \
    NODE_KIND(EXPR_ARRAYINIT,                    "<ARRINI>") \
    NODE_KIND(EXPR_SETINIT,                      "<SETINI>") \
    NODE_KIND(EXPR_MAPINIT,                      "<MAPINI>") \
        \
        /* The following are not 'exprs' but helper node inside or around expressions */ \
    NODE_KIND(SUBEXPR_SLICE,                     "<SLICEX>") /* found within indexations */ \
    NODE_KIND(SUBEXPR_ARROW,                     "<ARROWT>") /* pre-parser-only : should become a lambda if successful */ \
    NODE_KIND(SUBEXPR_WRAPPER,                   "<WRAP_C>") /* wrapping closing token around enclosed expressions, or optional 'then' for ifx */ \
    NODE_KIND(SUBEXPR_EITHER_OR,                 "<IFXWRP>") /* wrapping 'either' and 'or' subexpressions around the 'else' of a ternary if */ \
    NODE_KIND(VARIABLE_DECL,                     "<VARDCL>") /* *NOT* the statement form of the same */ \
    NODE_KIND(EXPRLIST_NODE,                     "<LISTND>") /* ubiquitous node from an expression-lists, has primary token ',' */ \
    NODE_KIND(PROCPARAMS_WRAP_ALL,               "<PROCWR>") /* params part of signature, [primary of proclike] */ \
    NODE_KIND(PROCPARAMS_IN,                     "<PROCIN>") /* in-params part of signature (pivot token ')') [primary of procparamwrap] */ \
    NODE_KIND(PROCPARAMS_OUT,                    "<PROCRS>") /* out-params part of signature (pivot token '->'), [optional secondart of procparamwrap] */ \
    NODE_KIND(PROCPARAMS_WHERE_CLAUSE,           "<PROCWH>") /* where-clause part of signature (pivot token 'where') [optional secondary of proclike] */ \
    NODE_KIND(SECONDARY_TOKEN_WRAPPER,           "<TOKWRP>") /* post-parser only, since prenode can hold 2 tokens by default */ \
        \
        /* The following are pre-parser statement conversions to post-parser main-nodes */ \
    NODE_KIND(ST_DECLARATION,                    "<-DECLA>") \
    NODE_KIND(ST_ASSIGNMENT,                     "<-ASSIG>") \
    NODE_KIND(ST_OP_AND_ASSIGN,                  "<-OP&AS>") \
    NODE_KIND(ST_CONTROL_FLOW,                   "<-CTLFW>") \
    NODE_KIND(ST_PAN_SPECIAL,                    "<-PANSP>") \
    NODE_KIND(ST_LOAD,                           "<-LOAD->") \
    NODE_KIND(ST_USING,                          "<-USING>") \
    NODE_KIND(ST_FOREIGN,                        "<-FOREG>")

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum ENodeKind {
#define NODE_KIND(name, dbgName)    ENODE_ ## name ,
    NODE_KINDS_EMITTER_
#undef NODE_KIND
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tNodeKindsDbgStr[] = {
#define NODE_KIND(name, dbgName)  dbgName ,
    NODE_KINDS_EMITTER_
#undef NODE_KIND
};
static const u8 COUNT_NODE_KINDS = u8(sizeof(tNodeKindsDbgStr) / sizeof(char*));
    
static const u8 EPRENODE_ERROR_WRAPPER              = COUNT_NODE_KINDS;    // wrapper for a primary not in error, and secondary in error (usable for continuation errors after already full nodes)
static const u8 EPRENODE_ERROR_UNEXPECTED           = COUNT_NODE_KINDS+1u; // expected specific token or expression, found unexpected token
static const u8 EPRENODE_ERROR_EXPECTED_WHEN_EOL    = COUNT_NODE_KINDS+2u; // expected specific token or expression, found EOL
static const u8 EPRENODE_ERROR_INVALID_MULTILINE    = COUNT_NODE_KINDS+3u; // parse-error occurred during multiline search for continuation
static const u8 EPRENODE_ERROR_WITH_PRE_MODIFIERS   = COUNT_NODE_KINDS+4u; // parse-error occurred during parsing of pre-expr modifiers
static const u8 EPRENODE_ERROR_WITH_POST_MODIFIERS  = COUNT_NODE_KINDS+5u; // parse-error occurred during parsing of post-expr modifiers
static const u8 EPRENODE_ERROR_UNRECOGNIZED_TOKEN   = COUNT_NODE_KINDS+6u; // parse-error occurred by stumbling upon a fully unclassified keyword or symbol
static const u8 EPRENODE_ERROR_OTHER                = COUNT_NODE_KINDS+7u; // parse-error occurred for a reason not listed above

static const u8 EPRENODE_MODIFIER_NODE              = COUNT_NODE_KINDS+8u; // @-words

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define STATEMENT_KIND(name, dbgName)
#define STATEMENT_KINDS_EMITTER_ \
        \
    STATEMENT_KIND(UNKNOWN,             "<<!UNKNOWN STATEMENT!>>") \
        \
    STATEMENT_KIND(CONST_DECLARATION,   "<- CONST-DECLARATION ->") /* will transform postnode to st_const_declaration */ \
    STATEMENT_KIND(VAR_DECLARATION,     "<-  VAR-DECLARATION  ->") /* will transform postnode to st_var_declaration */ \
    STATEMENT_KIND(ASSIGNMENT,          "<-     ASSIGNMENT    ->") /* will transform postnode to st_assignment_declaration */ \
    STATEMENT_KIND(OP_AND_ASSIGNMENT,   "<- OP_AND_ASSIGNMENT ->") /* will transform postnode to st_op_and_assignment */ \
        \
        /* the following will transform postnode to st_controlflow */ \
    STATEMENT_KIND(IF,                  "<-    IF_STATEMENT   ->") \
    STATEMENT_KIND(ELIF,                "<-   ELIF_STATEMENT  ->") \
    STATEMENT_KIND(ELSE,                "<-   ELSE_STATEMENT  ->") \
    STATEMENT_KIND(FOR,                 "<-    FOR_STATEMENT  ->") \
    STATEMENT_KIND(WHILE,               "<-  WHILE_STATEMENT  ->") \
    STATEMENT_KIND(LOOP_FINALIZER,      "<-   LOOP_FINALIZER  ->") \
    STATEMENT_KIND(CASE,                "<-   CASE_STATEMENT  ->") \
    STATEMENT_KIND(WITH,                "<-   WITH_STATEMENT  ->") \
    STATEMENT_KIND(RETURN,              "<-  RETURN_STATEMENT ->") \
    STATEMENT_KIND(BREAK,               "<-  BREAK_STATEMENT  ->") \
    STATEMENT_KIND(CONTINUE,            "<- CONTINUE_STATEMENT->") \
    STATEMENT_KIND(LOOP,                "<-   LOOP_STATEMENT  ->") \
    STATEMENT_KIND(DEFER,               "<-  DEFER_STATEMENT  ->") \
    STATEMENT_KIND(NO_OP,                "<-  NOOP_STATEMENT  ->") \
    STATEMENT_KIND(UNREACH,             "<-   UNREACHABLE_ST  ->") \
    STATEMENT_KIND(LABEL,               "<-  LABEL_STATEMENT  ->") \
        \
        /* the following will transform postnode to st_pan_special */ \
    STATEMENT_KIND(PAN_IF,              "<-      PAN_IF       ->") \
    STATEMENT_KIND(PAN_ELIF,            "<-     PAN_ELIF      ->") \
    STATEMENT_KIND(PAN_ELSE,            "<-     PAN_ELSE      ->") \
    STATEMENT_KIND(PAN_ENDIF,           "<-    PAN_ENDIF      ->") \
    STATEMENT_KIND(PAN_PRIVATE,         "<-   PAN_PRIVATE     ->") \
    STATEMENT_KIND(PAN_ENDPRIVATE,      "<-  PAN_ENDPRIVATE   ->") \
    STATEMENT_KIND(PAN_NAMESPACE,       "<-  PAN_NAMESPACE    ->") \
    STATEMENT_KIND(PAN_ENDNAMESPACE,    "<- PAN_ENDNAMESPACE  ->") \
        \
        /* the following will keep their main node as postnode (unless 'load' and 'using' as statements) */ \
    STATEMENT_KIND(SINGLE_EXPRESSION,   "<- SINGLE_EXPRESSION ->") \
    STATEMENT_KIND(USING,               "<-  USING_STATEMENT  ->") \
        \
        /* the following will not emit node by itself */ \
    STATEMENT_KIND(MODIFIER_ONLY_LINE,  "<-   MODIFIERS_ONLY  ->")


// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum EStatementKind {
#define STATEMENT_KIND(name, dbgName)    ESTATEMENT_ ## name ,
    STATEMENT_KINDS_EMITTER_
#undef STATEMENT_KIND
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tStatementsDbgStr[] = {
#define STATEMENT_KIND(name, dbgName)  dbgName ,
    STATEMENT_KINDS_EMITTER_
#undef STATEMENT_KIND
};
static const u8 COUNT_STATEMENT_KINDS = u8(sizeof(tStatementsDbgStr) / sizeof(char*));

// In prep of Macro-magic...
typedef PreAstNode* ExpressionParsingProc_Sign (ParserParams& parserParams, u16 uDepthGuard, u16* outError);
#define declare_expr_parsing_fn(coreName) \
    local_func PreAstNode* try_parse_ ## coreName ## _expr(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with #define EXPR_FN_PREDECL(coreName)
#define EXPR_FN_PREDECL_EMITTER_ \
    EXPR_FN_PREDECL(parentised) \
    EXPR_FN_PREDECL(no_left_dot) \
    EXPR_FN_PREDECL(special_token) \
    EXPR_FN_PREDECL(no_left_arr_lit) \
    EXPR_FN_PREDECL(no_left_set_lit) \
    EXPR_FN_PREDECL(no_left_map_lit) \
    EXPR_FN_PREDECL(no_left_st_lit) \
    EXPR_FN_PREDECL(ternary_if) \
    EXPR_FN_PREDECL(proclike_decl) \
    EXPR_FN_PREDECL(enum_decl) \
    EXPR_FN_PREDECL(structlike_decl) \
    EXPR_FN_PREDECL(set_decl) \
    EXPR_FN_PREDECL(special_using) \
    EXPR_FN_PREDECL(special_load)

// In prep of Macro-magic...
typedef PreAstNode* UnaryOpParsingProc_Sign (ParserParams& parserParams, u16 uDepthGuard, u16* outError);
#define declare_unary_parsing_fn(coreName) \
    local_func PreAstNode* try_parse_ ## coreName ## _unary(ParserParams& parserParams, u16 uDepthGuard, u16* outError) 

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with #define UNARY_FN_PREDECL(coreName)
#define UNARY_FN_PREDECL_EMITTER_ \
    UNARY_FN_PREDECL(array_like_decl) \
    UNARY_FN_PREDECL(map_decl)

// In prep of Macro-magic...
typedef void StatementParsingProc_Sign (PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError);
#define declare_statement_parsing_fn(coreName) \
    local_func void try_parse_ ## coreName ## _statement(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError) 

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with #define STATEMENT_FN_PREDECL(coreName)
#define STATEMENT_FN_PREDECL_EMITTER_ \
    STATEMENT_FN_PREDECL(conditional) \
    STATEMENT_FN_PREDECL(block_opening) \
    STATEMENT_FN_PREDECL(st_for) \
    STATEMENT_FN_PREDECL(loop_finalizer) \
    STATEMENT_FN_PREDECL(st_case) \
    STATEMENT_FN_PREDECL(special_token) \
    STATEMENT_FN_PREDECL(st_return) \
    STATEMENT_FN_PREDECL(st_opt_expr) \
    STATEMENT_FN_PREDECL(special_using) \
    STATEMENT_FN_PREDECL(special_load) \
    STATEMENT_FN_PREDECL(pan_opening) \
    STATEMENT_FN_PREDECL(pan_else) \
    STATEMENT_FN_PREDECL(pan_closing) \
    STATEMENT_FN_PREDECL(pan_private)

// In prep of Macro-magic...
typedef PreAstNode* SpeContinuationParsingProc_Sign (ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError);
#define declare_spe_cont_parsing_fn(coreName) \
    local_func PreAstNode* try_parse_ ## coreName ## _spe_cont(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError) 

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with #define SPE_CONT_FN_PREDECL(coreName)
#define SPE_CONT_FN_PREDECL_EMITTER_ \
    SPE_CONT_FN_PREDECL(invoc_like) \
    SPE_CONT_FN_PREDECL(dot_descent) \
    SPE_CONT_FN_PREDECL(deref_descent) \
    SPE_CONT_FN_PREDECL(arr_lit) \
    SPE_CONT_FN_PREDECL(set_lit) \
    SPE_CONT_FN_PREDECL(map_lit) \
    SPE_CONT_FN_PREDECL(st_lit) \
    SPE_CONT_FN_PREDECL(index_like) \
    SPE_CONT_FN_PREDECL(deref)

#define EXPR_           0x01u
#define UN_OP           0x02u
#define BINOP           0x04u
#define SPE_B           0x08u

#define STATEMENT       0x10u
#define TYPE_DEF        0x20u
#define OP_AND_ASIGN    0x40u
#define CUSTOM          0x80u

// TODO: CLEANUP: Rework all that, and preparser algorithms, so that maybe '=' and '->' really are handled as binary ops

// Priorities:
//  0 (very loose) : algo start ; would also be the de-facto priority of 'comma' if it was an op.
//  1 ultra-lose : would be the de-facto priority of '=' or '->' operators between anything
//      if they had not a very special behaviour wrt single-appearance and their relationship with commas.
//  2 boolean binary ops                    (loose so that 'a == 5 and b == 6' parses as '(a == 5) and (b == 6)')
//  3 comparison operators                  (looser than most, seems unlike C in that regard : '0 == a & b' parses here '0 == (a & b)')
//  4 range                                 loose
//  5 addition, subtraction, concat         quite intuitively
//  6
//  7 multiplication, division, modulo      ( 'a * b + c * d' parses '(a * b) + (c * d)', no surprise there)
//  8 power                                 ( 'a * b ** c' parses 'a * (b ** c)')
//  9 bitops                                (tighter than other ops except shifts and unary. 'a & b * 2' parses '(a & b) * 2')
// 10 bitshifts                             (tighter than most : 'a | b << 2' parses 'a | (b << 2)')
// 11 
// 12
// 13 unary operators
// 14
// 15 supertight left-associative for special binops : 'myNamespace.myStruct.someMember[i].functionPtr(x).hop' parses as
//      (((((myNamespace DOT myStruct) DOT someMember) IDX i) DOT functionPtr) IVK x) DOT hop

// Special parsing Functions:
//   Keep those refs used in the emitter below in sync with the 'coreName' parameters in the four parsing-function emitter lists above:
//   @see EXPR_FN_PREDECL_EMITTER_, UNARY_FN_PREDECL_EMITTER_, STATEMENT_FN_PREDECL_EMITTER_, SPE_CONT_FN_PREDECL_EMITTER_

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
//   (Yes, I feel almost sorry for this one...)
// use together with    #define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)
#define PARSER_METAINFO_EMITTER_ \
        \
        /* first 32 positions [0..31] correspond to ascii control chars and have no representation here */ \
        \
    PARSER_META(SPACE               , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(COMPACT_DECL        , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(DOUBLEQUOTE         , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(PAN                 , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(DOLLAR              , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(MODULUS             , BINOP             , 7  ,               ,               ,               ,               ) \
    PARSER_META(AMPERSAND           , BINOP|UN_OP       , 9  ,               ,               ,               ,               ) \
    PARSER_META(SINGLEQUOTE         , UN_OP             , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(OPENING_PARENS      , EXPR_|SPE_B       , 0  ,     parentised,               ,               ,     invoc_like) \
    PARSER_META(CLOSING_PARENS      , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(MUL                 , BINOP             , 7  ,               ,               ,               ,               ) \
    PARSER_META(PLUS                , BINOP|UN_OP       , 5  ,               ,               ,               ,               ) \
    PARSER_META(COMMA               , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(MINUS               , BINOP|UN_OP       , 5  ,               ,               ,               ,               ) \
    PARSER_META(DOT                 , EXPR_|SPE_B       , 0  ,    no_left_dot,               ,               ,    dot_descent) \
    PARSER_META(DIV                 , BINOP             , 7  ,               ,               ,               ,               ) \
        \
    PARSER_META(CONST_DECL          , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(IMPLIES             , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(ARROW               , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(UNINITIALIZED       , EXPR_             , 0  ,  special_token,               ,               ,               ) \
    PARSER_META(SUBTAG_DECL         , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(ASSIGNMENT          , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(SLICE_TO_EXCL       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ARE_NOT_EQUAL       , BINOP             , 3  ,               ,               ,               ,               ) \
        \
    PARSER_META(_RESERVED_56        , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_57        , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(COLON               , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(SEMICOLON           , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(LESSER_THAN         , BINOP             , 3  ,               ,               ,               ,               ) \
    PARSER_META(SINGLE_EQ           , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(GREATER_THAN        , BINOP             , 3  ,               ,               ,               ,               ) \
    PARSER_META(QUESTION_MARK       , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(ATSIGN              , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ARE_EQUAL           , BINOP             , 3  ,               ,               ,               ,               ) \
    PARSER_META(LESSER_OR_EQ        , BINOP             , 3  ,               ,               ,               ,               ) \
    PARSER_META(GREATER_OR_EQ       , BINOP             , 3  ,               ,               ,               ,               ) \
    PARSER_META(LEFT_SHIFT          , BINOP             , 10 ,               ,               ,               ,               ) \
    PARSER_META(RIGHT_SHIFT         , BINOP             , 10 ,               ,               ,               ,               ) \
    PARSER_META(POW                 , BINOP             , 8  ,               ,               ,               ,               ) \
    PARSER_META(MODULO_ADD          , BINOP             , 5  ,               ,               ,               ,               ) \
        \
    PARSER_META(MODULO_SUB          , BINOP|UN_OP       , 5  ,               ,               ,               ,               ) \
    PARSER_META(MODULO_MUL          , BINOP             , 7  ,               ,               ,               ,               ) \
    PARSER_META(INT_QUOTIENT        , BINOP             , 7  ,               ,               ,               ,               ) \
    PARSER_META(INT_REMAINDER       , BINOP             , 7  ,               ,               ,               ,               ) \
    PARSER_META(CONCAT              , BINOP             , 5  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_77        , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(RANGE_INCL          , BINOP             , 4  ,               ,               ,               ,               ) \
    PARSER_META(RANGE_EXCL          , BINOP             , 4  ,               ,               ,               ,               ) \
        \
    PARSER_META(OPENING_ARRAY_LIT   , EXPR_|SPE_B       , 0  ,no_left_arr_lit,               ,               ,        arr_lit) \
    PARSER_META(OPENING_SET_LIT     , EXPR_|SPE_B       , 0  ,no_left_set_lit,               ,               ,        set_lit) \
    PARSER_META(OPENING_MAP_LIT     , EXPR_|SPE_B       , 0  ,no_left_map_lit,               ,               ,        map_lit) \
    PARSER_META(OPENING_DYNARRAY    , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(OPENING_LAM_STRICT  , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(OPENING_LAM_CLOSURE , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(POINTER_DECL        , UN_OP|SPE_B       , 0  ,               ,               ,               ,  deref_descent) \
    PARSER_META(_RESERVED_87        , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(_RESERVED_88        , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_89        , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_90        , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(OPENING_BRACKET     , UN_OP|SPE_B       , 0  ,               ,array_like_decl,               ,     index_like) \
    PARSER_META(BACKSLASH           , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(CLOSING_BRACKET     , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(CARET               , SPE_B             , 0  ,               ,               ,               ,          deref) \
    PARSER_META(UNDERSCORE          , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(BACKTICK            , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ADD_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(SUB_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(MUL_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(DIV_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(MOD_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(BIT_AND_ASSIGN      , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(BIT_OR_ASSIGN       , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(BIT_XOR_ASSIGN      , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(LSH_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(RSH_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(POW_ASSIGN          , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(MODULO_ADD_ASSIGN   , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(MODULO_SUB_ASSIGN   , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(MODULO_MUL_ASSIGN   , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(INT_QUO_ASSIGN      , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(INT_REM_ASSIGN      , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(CONCAT_ASSIGN       , OP_AND_ASIGN      , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_114       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_115       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_116       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_117       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_118       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_119       , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(_RESERVED_120       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_121       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(_RESERVED_122       , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(OPENING_CURLY       , EXPR_|SPE_B       , 0  , no_left_st_lit,               ,               ,         st_lit) \
    PARSER_META(BIT_OR              , BINOP             , 9  ,               ,               ,               ,               ) \
    PARSER_META(CLOSING_CURLY       , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(BIT_NOT             , UN_OP|BINOP       , 0  ,               ,               ,               ,               ) \
    PARSER_META(DEL                 , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(OR                  , BINOP             , 2  ,               ,               ,               ,               ) \
    PARSER_META(AND                 , BINOP             , 2  ,               ,               ,               ,               ) \
    PARSER_META(XOR                 , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(NOT                 , UN_OP             , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(IF                  , STATEMENT         , 0  ,               ,               ,    conditional,               ) \
    PARSER_META(IFX                 , EXPR_             , 0  ,     ternary_if,               ,               ,               ) \
    PARSER_META(THEN                , CUSTOM            , 0  ,               ,               ,               ,               ) \
	PARSER_META(ELIF                , STATEMENT         , 0  ,               ,               ,    conditional,               ) \
	PARSER_META(ELSE                , CUSTOM|STATEMENT  , 0  ,               ,               ,  block_opening,               ) \
	PARSER_META(FOR                 , STATEMENT         , 0  ,               ,               ,         st_for,               ) \
	PARSER_META(IN                  , CUSTOM            , 0  ,               ,               ,               ,               ) \
	PARSER_META(INREV               , CUSTOM            , 0  ,               ,               ,               ,               ) \
	PARSER_META(WHILE               , STATEMENT         , 0  ,               ,               ,    conditional,               ) \
	PARSER_META(DO                  , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(ONBREAK             , CUSTOM            , 0  ,               ,               , loop_finalizer,               ) \
    PARSER_META(WHENNONE            , CUSTOM            , 0  ,               ,               , loop_finalizer,               ) \
    PARSER_META(ONDONE              , CUSTOM            , 0  ,               ,               , loop_finalizer,               ) \
    PARSER_META(CASE                , STATEMENT         , 0  ,               ,               ,        st_case,               ) \
    PARSER_META(SWITCH              , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(WITH                , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(LET                 , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(NO_OP               , STATEMENT         , 0  ,               ,               ,  special_token,               ) \
    PARSER_META(UNREACH             , STATEMENT         , 0  ,               ,               ,  special_token,               ) \
    PARSER_META(RETURN              , STATEMENT         , 0  ,               ,               ,      st_return,               ) \
    PARSER_META(BREAK               , STATEMENT         , 0  ,               ,               ,    st_opt_expr,               ) \
    PARSER_META(CONTINUE            , STATEMENT         , 0  ,               ,               ,    st_opt_expr,               ) \
    PARSER_META(TOELSE              , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(LOOP                , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(DEFER               , STATEMENT         , 0  ,               ,               ,  block_opening,               ) \
    PARSER_META(ERRDEFER            , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(FAIL                , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(FAILWITH            , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ORFAIL              , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ORFAILWITH          , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ORRETURN            , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(FALLTHROUGH         , 0                 , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(WHERE               , CUSTOM            , 0  ,               ,               ,               ,               ) \
    PARSER_META(VARDECL             , CUSTOM            , 0  ,               ,               ,               ,               ) \
        \
    PARSER_META(PROC                , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(FUNC                , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(NOCTXPROC           , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(NOCTXFUNC           , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(PURE                , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(SEMIPURE            , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(MACRO               , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(COMPTIMEFUNC        , TYPE_DEF|EXPR_    , 0  ,  proclike_decl,               ,               ,               ) \
    PARSER_META(ENUM                , TYPE_DEF|EXPR_    , 0  ,      enum_decl,               ,               ,               ) \
    PARSER_META(STRUCT              , TYPE_DEF|EXPR_    , 0  ,structlike_decl,               ,               ,               ) \
    PARSER_META(PACKED_STRUCT       , TYPE_DEF|EXPR_    , 0  ,structlike_decl,               ,               ,               ) \
    PARSER_META(VIEW                , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(UNION               , TYPE_DEF|EXPR_    , 0  ,structlike_decl,               ,               ,               ) \
    PARSER_META(HSET                , EXPR_             , 0  ,       set_decl,               ,               ,               ) \
    PARSER_META(HDIC                , UN_OP             , 0  ,               ,       map_decl,               ,               ) \
        \
	PARSER_META(USING               , STATEMENT         , 0  ,               ,               ,  special_using,               ) \
	PARSER_META(INCLUDING           , STATEMENT         , 0  ,               ,               ,  special_using,               ) \
	PARSER_META(STATIC              , 0                 , 0  ,               ,               ,               ,               ) \
	PARSER_META(DYNAMIC             , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(DISTINCT            , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(SIZEOF              , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(ALIGNOF             , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(STRIDEOF            , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(TYPEOF              , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(TYPEINFOOF          , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(TYPEIDOF            , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(SIGNATUREOF         , UN_OP             , 0  ,               ,               ,               ,               ) \
    PARSER_META(CLOSUREOF           , 0                 , 0  ,               ,               ,               ,               ) \
    PARSER_META(ASCLOSURE           , 0                 , 0  ,               ,               ,               ,               ) \
        \
	PARSER_META(PAN_LOAD            , EXPR_             , 0  ,   special_load,               ,               ,               ) \
	PARSER_META(PAN_IMPORT          , 0                 , 0  ,               ,               ,               ,               ) \
	PARSER_META(PAN_PACKAGE         , 0                 , 0  ,               ,               ,               ,               ) \
	/*PARSER_META(PAN_FOREIGN         , STATEMENT         , 0  ,special_foreign,               ,               ,               )*/ \
	/*PARSER_META(PAN_FOREIGN_SOURCE  , EXPR_             , 0  ,special_frgnsrc,               ,               ,               )*/ \
	PARSER_META(PAN_INLINE          , 0                 , 0  ,               ,               ,               ,               ) \
	PARSER_META(PAN_INCLUDE         , 0                 , 0  ,               ,               ,               ,               ) \
        \
	PARSER_META(PAN_IF              , STATEMENT         , 0  ,               ,               ,    pan_opening,               ) \
	PARSER_META(PAN_ELIF            , STATEMENT         , 0  ,               ,               ,    pan_opening,               ) \
	PARSER_META(PAN_ELSE            , STATEMENT         , 0  ,               ,               ,       pan_else,               ) \
	PARSER_META(PAN_ENDIF           , STATEMENT         , 0  ,               ,               ,    pan_closing,               ) \
	PARSER_META(PAN_PRIVATE         , STATEMENT         , 0  ,               ,               ,    pan_private,               ) \
	PARSER_META(PAN_ENDPRIVATE      , STATEMENT         , 0  ,               ,               ,    pan_closing,               ) \
	PARSER_META(PAN_NAMESPACE       , STATEMENT         , 0  ,               ,               ,    pan_opening,               ) \
	PARSER_META(PAN_ENDNAMESPACE    , STATEMENT         , 0  ,               ,               ,    pan_closing,               )

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// Checking parser table
//
enum _EParserTableChecker {
    _EPARSERTABLE_CHECKER_Before = 31,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    _EPARSERTABLE_CHECKER_ ## name , 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn) \
       static_assert(_EPARSERTABLE_CHECKER_ ## name == ETOK_ ## name, "Parser Meta-info Table vs Token mismatch on : " #name);
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
//
// End checking


//
// Array of parsing flags, per keyword or symbol :
//

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tPayloadFlags[] = {
    // first 32 positions [0..31] correspond to ascii control chars and are set to 0 here
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    flags , 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};


//
// Array of priority levels when standard binary op, per keyword or symbol :
//

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tBinopPriorities[] = {
    // first 32 positions [0..31] correspond to ascii control chars and are set to 0 here
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    binopPriority , 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};


//
// Array of pointers to special expression-parsing functions, per keyword or symbol :
//

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// ... + preps for the metamagic just below (sorry)
#define EXPR_FN_PREDECL(coreName)   \
    PreAstNode* try_parse_ ## coreName ## _expr(ParserParams& parserParams, u16 uDepthGuard, u16* outError); \
    constexpr ExpressionParsingProc_Sign* try_parse_ ## coreName ## _expr_ptr = & try_parse_ ## coreName ## _expr;
        EXPR_FN_PREDECL_EMITTER_
#undef EXPR_FN_PREDECL

#define try_parse__expr_ptr       0
// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr ExpressionParsingProc_Sign* tExpressionParsingFn[] = {
    // first 32 positions [0..31] correspond to ascii control chars and are set to 0 here
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    try_parse_ ## exprFn ## _expr_ptr, 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};


//
// Array of pointers to special unary-op-parsing functions, per keyword or symbol :
//

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// ... + preps for the metamagic just below (sorry)
#define UNARY_FN_PREDECL(coreName)   \
    PreAstNode* try_parse_ ## coreName ## _unary(ParserParams& parserParams, u16 uDepthGuard, u16* outError); \
    constexpr ExpressionParsingProc_Sign* try_parse_ ## coreName ## _unary_ptr = & try_parse_ ## coreName ## _unary;
        UNARY_FN_PREDECL_EMITTER_
#undef UNARY_FN_PREDECL

#define try_parse__unary_ptr      0
// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr UnaryOpParsingProc_Sign* tUnaryParsingFn[] = {
    // first 32 positions [0..31] correspond to ascii control chars and are set to 0 here
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    try_parse_ ## unaryFn ## _unary_ptr , 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};


//
// Array of pointers to special statement-parsing functions, per keyword or symbol :
//

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// ... + preps for the metamagic just below (sorry)
#define STATEMENT_FN_PREDECL(coreName)   \
    void try_parse_ ## coreName ## _statement(PreStatement* pStatement, ParserParams& parserParams, u16 uDepthGuard, u16* outError); \
    constexpr StatementParsingProc_Sign* try_parse_ ## coreName ## _statement_ptr = & try_parse_ ## coreName ## _statement;
        STATEMENT_FN_PREDECL_EMITTER_
#undef STATEMENT_FN_PREDECL

#define try_parse__statement_ptr  0
// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr StatementParsingProc_Sign* tStatementParsingFn[] = {
    // first 32 positions [0..31] correspond to ascii control chars and are set to 0 here
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    try_parse_ ## statmFn ## _statement_ptr , 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};


//
// Array of pointers to special continuation functions (usually for 'superbinops'), per keyword or symbol :
//

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// ... + preps for the metamagic just below (sorry)
#define SPE_CONT_FN_PREDECL(coreName)   \
    PreAstNode* try_parse_ ## coreName ## _spe_cont(ParserParams& parserParams, PreAstNode* pLHSExpr, u16 uDepthGuard, u16* outError); \
    constexpr SpeContinuationParsingProc_Sign* try_parse_ ## coreName ## _spe_cont_ptr = & try_parse_ ## coreName ## _spe_cont;
        SPE_CONT_FN_PREDECL_EMITTER_
#undef SPE_CONT_FN_PREDECL

#define try_parse__spe_cont_ptr        0
// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr SpeContinuationParsingProc_Sign* tSpeContinuationParsingFn[] = {
    // first 32 positions [0..31] correspond to ascii control chars and are set to 0 here
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
    0, 0, 0, 0,     0, 0, 0, 0,
#define PARSER_META(name, flags, binopPriority, exprFn, unaryFn, statmFn, speFn)    try_parse_ ## speFn ## _spe_cont_ptr , 
    PARSER_METAINFO_EMITTER_
#undef PARSER_META
};

#endif // LOCLIB_PRE_PARSER_TYPES_H_

