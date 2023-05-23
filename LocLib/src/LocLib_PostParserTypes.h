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

#ifndef LOCLIB_POST_PARSER_TYPES_H_
#define LOCLIB_POST_PARSER_TYPES_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_PreParserTypes.h"

#define EMODIFIERFLAG_POSTSTATEMENT_OR_EXPRESSION   0x80000000u
struct AstModifierNode {
    u32 uPayload;                   // handle to the modifier-builtin or user-id (ored with the above flag, if post)
    u32 uPivotalTokenPackedRef;     // Statement-relative line and start byte index of pivotal token
    u32 uOpenParensTokenPackedRef;  // Statement-relative line and start byte index of open parens token, if any
    u32 uCloseParensTokenPackedRef; // Statement-relative line and start byte index of close parens token, if any
    u32 uValueExpressionIndex;      // An index into statement-wise expression array, if any
    u32 uNext;                      // Any statement can hold a chain of these modifiers... this is the index of the next in that chain (in the
                                    //   blockwise-array of modifiers), if != 0xFFFFFFFF
};

//***********************************
// AstNode structure
//***********************************
// An AST is comprised ultimately of those AstNodes, arranged in an array of nodes specific to a single statement.
// Typically, those AstNodes are not produced directly from parsing the raw source code (or even tokens),
//   but are formed after "PreAstNodes" were digested by the PreParser.
// This brings several advantages:
//    * The aforementionned "array" of those can be predimensionned for each validly pre-parsed statement.
//    * Packing of ptr-to-child nodes as 32b indices in that array is also made easy, thanks to this pre-pass.
//    * PreAstNode can themselves be decorated with ways to better communicate about parser errors, without
//      impacting the size (thus cache-perf during the following compilation phases) of AstNode proper.
//    * The PreParser can also be a step towards improving the communication about errors, since nothing
//      prevents this pre-pass to be more tolerant about the grammar, since conversion to the actual AST
//      will allow for further validity checks: this grammar "tolerance" thus allows to parse pieces of code
//      which, in another context, could be valid, and THEN inform the user of the invalidity of that
//      different context usage. eg:
//          - PreParser could allow any form of expression-chain after an 'if'
//          - Then PreAstNode to AstNode conversion ensures the 'condition' is a **single** expression.
//          - If not, we can confidently report to the user that a condition needs to be a single expression
//          - This example may be contrieved, but any step towards better error messages is great (instead of
//              reporting a generic : "unexpected comma" as some other parser implementation would do...)
// With all that said, here is the AstNode definition, which is only 2x64b (=> fitting 4 of these in a cache line),
//      all-the while holding sufficient info (pivotal token refs...) to round-trip to their definition in code, for
//      any program (such as LocLang IDE) with knowledge of same source code and tokens and associated statement.
struct AstNode {

    DECL_TRIVIAL_STRUCT_OPS(AstNode);

    // 64b
    u32 uNodeKindAndFlags;              // 8b ENodeKind at LSB, then possibly 8b Node Payload after that (eg. subkind of a 'binop' kind), and flags
    u32 uPivotalTokenPackedRef;         // Statement-relative line (22b) and start byte index (10b) of pivotal token for this node
    
    // 64b
    union {
        struct {
            union { // differenciating these cases depends on node kind
                u32 uPrimaryChildNodeIndex;     // An index into statement-wise expression array
                u32 uPrimaryPayload;            // Alternative payload if primary is something else
            };
            union { // differenciating these cases depends on node kind
                u32 uSecondaryChildNodeIndex;   // An index into statement-wise expression array
                u32 uSecondaryTokenPackedRef;   // Statement-relative line (22b) and start byte index (10b) if secondary is a token
                u32 uSecondaryPayload;          // Alternative payload if secondary is something else
            };
        };
        u64 uLiteralPayload64; // 64b of payload for literals, will correspond to the 'constValue' of an appropriately typed ValueHolder
    };
};

#define INVALID_NODE_INDEX    0xFFFFFFFFu

enum ENodeKindFlag {

    ENODEKINDFLAG_IS_LITERAL                = 0x00020000,
    ENODEKINDFLAG_IS_LITERAL_EMBEDDED64     = 0x00040000,

    ENODEKINDFLAG_IS_COMPTIME               = 0x00100000,
    ENODEKINDFLAG_HAS_VAR_DECL              = 0x00200000,
    ENODEKINDFLAG_HAS_NAMED_VALUE           = 0x00400000,
    ENODEKINDFLAG_IS_LOOPFINALIZER_CHAINED  = 0x00800000,

    //
    // TC specific
    //

    ENODEKINDFLAG_IS_TYPECHECKED_PHASE1      = 0x01000000,
    ENODEKINDFLAG_IS_TYPECHECKED_PHASE2      = 0x02000000,
    ENODEKINDFLAG_HAS_NO_TC_EXPR_VALUE       = 0x04000000,
    ENODEKINDFLAG_WAS_TC_PANIF_NOT_TAKEN     = 0x08000000,

    // positionned by the typechecker, but not part of the 'tc' flags proper
    // Note: Not used any more with current scheme...
//    ENODEKINDFLAG_HAS_BOOL_PRE_INVERSION            = 0x08000000,
//    ENODEKINDFLAG_IS_INTRINSIC_USER_REFERENCEABLE   = 0x10000000,

};

#define get_node_kind(uNodeKindAndFlags)                  u8(uNodeKindAndFlags)
#define get_node_opt_subkind(uNodeKindAndFlags)           u8((uNodeKindAndFlags) >> 8)
#define make_node_kind(uNodeKindAndFlags, uOptSubKind)    u32(u32(uNodeKindAndFlags) | (u32(uOptSubKind) << 8))
#define make_packed_token_ref(tokenRef)                   u32(u32(tokenRef.uBytesFromLineStart) | (u32(tokenRef.uLine) << 10))

enum EStatementParsingStateFlags {
    STATEMENT_MISSES_CHILD_BLOCK            = 0x10000000u,
    STATEMENT_CHILD_IN_ERROR                = 0x20000000u,
    STATEMENT_IN_ERROR                      = 0x40000000u,
    
    STATEMENT_IS_PARENT_OF_INLINED          = 0x00100000u,
    STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD = 0x00200000u,
};

#define IN_ERROR_STATEMENT_MASK     0xFF000000u
#define IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD      (IN_ERROR_STATEMENT_MASK & (~STATEMENT_MISSES_CHILD_BLOCK))

// 4x64b
struct AstStatement {
    u32 uNodeCount;
    u32 uFirstLineRelToLineOfParentStatementInParentBlock;
    u32 uLineCount;
    u32 uFlags;
    u32 uStartOfLineByteOffsetRelToStartOfParentStatement;
    int iChildBlock;
    AstNode* tNodes;
};

#define BLOCK_PARSING_STATE_MASK_NOFLAGS    0x007F'FFFFu
#define IN_ERROR_BLOCK_MASK                 0x3F80'0000u
enum EBlockParsingStateFlags {
    BLOCK_WAS_SPAWNED_AT_SAME_INDENT_AS_PARENT  = 0x8000'0000u,
    BLOCK_WAS_SPANWED_AS_INLINED                = 0x4000'0000u,

    BLOCK_IS_IN_INDENTATION_ERROR               = 0x2000'0000u,
    BLOCK_IS_UNEXPECTED_CHILD                   = 0x1000'0000u,
    BLOCK_HAS_UNEXPECTED_CHILDREN               = 0x0800'0000u,
    BLOCK_HAS_STATEMENTS_IN_ERROR               = 0x0400'0000u,
    BLOCK_HAS_STATEMENTS_WITH_MISSING_CHILD     = 0x0200'0000u,
    BLOCK_HAS_CHILDREN_BLOCKS_IN_ERROR          = 0x0100'0000u,
    BLOCK_IS_MISSING_CLOSING_PAN_DIRECTIVE      = 0x0080'0000u,
};

// 6x64b
struct AstBlock {
    u32 uStatementCount;
    u32 uModifierCountAndFlags; // 23b count LSB
    u32 uChildBlockCount;
    u32 uDescendantBlockCount;
    u32 uParentBlock;
    u32 uIndexOfParentStatementInParentBlock;
    AstStatement* tStatements;
    AstModifierNode* tModifiers; // TODO: rethink relationship between nodes and modifiers
    u32* tChildBlockIndices;
};


#endif // LOCLIB_POST_PARSER_TYPES_H_

