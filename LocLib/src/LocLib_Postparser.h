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

#ifndef LOCLIB_POSTPARSER_H_
#define LOCLIB_POSTPARSER_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_Token.h"
#include "LocLib_ScanAndTok.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_ProgramState.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_DebugPrint.h"
#include "LocLib_Preparser.h"
#include "LocLib_PostParserTypes.h"

struct BlockParsingState {
    i32 iStartLineOfParentStatementInParentBlock;
    u32 uIndexOfParentStatementInParentBlock;
    u32 uPreParsingFlags;
    u16 uBlockTabIndent;
    u16 _pad0;
    u32 uDescendantBlockCount;
    u32 _pad1;
    i32 iStartLineOfLastParsedStatement;
    i32 uIndexInBlockVec;
    const u8* pStartingByteOfLastParsedStatement;
    const u8* pStartingByteOfParentStatementInParentBlock;
    TmpArray<AstStatement> vecStatements;        // allocated from a fileparser-specific scratchAlloc
    TmpArray<u32> vecChildBlockIndices;          // allocated from a fileparser-specific scratchAlloc
    TmpArray<AstModifierNode*> vecModifiers;     // allocated from a fileparser-specific scratchAlloc
    ArenaRefPoint allocRefBefore;                // since all BlockParsingState are visited in a stack-like fashion,
                                                 //   we can reset to allocRefBefore once we convert that to an AstBlock
                                                 //   (provided no error, or all errors have been copied elsewhere)
};

local_func void _init_block_parsing_state(BlockParsingState* outBlockParsingState,
    u32 uIndexInBlockVec, u32 uIndexOfParentStatement, u16 uBlockTabIndent,
    int iStartLineOfParentStatementInParentBlock,
    const u8* pStartingByteOfParentStatementInParentBlock,
    Arena tmpVecAllocArena)
{
    outBlockParsingState->uIndexInBlockVec = uIndexInBlockVec;
    outBlockParsingState->allocRefBefore = get_arena_ref_point(tmpVecAllocArena);
    outBlockParsingState->uIndexOfParentStatementInParentBlock = uIndexOfParentStatement;
    outBlockParsingState->uPreParsingFlags = 0;
    outBlockParsingState->uBlockTabIndent = uBlockTabIndent;
    outBlockParsingState->uDescendantBlockCount = 0;
    outBlockParsingState->vecStatements.init(tmpVecAllocArena);
    outBlockParsingState->vecChildBlockIndices.init(tmpVecAllocArena);
    outBlockParsingState->vecModifiers.init(tmpVecAllocArena);
    outBlockParsingState->iStartLineOfParentStatementInParentBlock = iStartLineOfParentStatementInParentBlock;
    outBlockParsingState->pStartingByteOfParentStatementInParentBlock = pStartingByteOfParentStatementInParentBlock;
    outBlockParsingState->pStartingByteOfLastParsedStatement = 0;
    outBlockParsingState->iStartLineOfLastParsedStatement = -1;
}

#define OPENED_BLOCK_MAX_OVERSIZE   150

#define ENODE_FOR_IS_REVERSED 0x00000100u


local_func AstBlock* _convert_to_ast_block(BlockParsingState* pBlockParsingState,
    BlockParsingState* tBlocks, Arena arenaForBlock, Arena arenaForFinalAST)
{
    u32 uStatementCount = pBlockParsingState->vecStatements.size();
    Assert_(uStatementCount);
    AstStatement* tStatements = reinterpret_cast<AstStatement*>(alloc_from(arenaForFinalAST,
        sizeof(AstStatement) * uStatementCount, alignof(AstStatement)));
    memcpy(tStatements, pBlockParsingState->vecStatements.begin(), sizeof(AstStatement) * uStatementCount);
    
    AstModifierNode* tModifiers = 0;
    u32 uModifierCount = pBlockParsingState->vecModifiers.size();
    if (uModifierCount) {
        tModifiers = reinterpret_cast<AstModifierNode*>(alloc_from(arenaForFinalAST,
            sizeof(AstModifierNode) * uModifierCount, alignof(AstModifierNode)));
        memcpy(tModifiers, pBlockParsingState->vecModifiers.begin(), sizeof(AstModifierNode) * uModifierCount);
    }

    u32* tChildBlockIndices = 0;
    u32 uChildBlockCount = pBlockParsingState->vecChildBlockIndices.size();
    if (uChildBlockCount) {
        tChildBlockIndices = reinterpret_cast<u32*>(alloc_from(arenaForFinalAST,
            sizeof(u32) * uChildBlockCount, alignof(u32)));
        memcpy(tChildBlockIndices, pBlockParsingState->vecChildBlockIndices.begin(), sizeof(u32) * uChildBlockCount);
    }

    AstBlock* pNewBlock = reinterpret_cast<AstBlock*>(alloc_from(arenaForBlock, sizeof(AstBlock), alignof(AstBlock)));
    pNewBlock->uStatementCount = uStatementCount;
    pNewBlock->uModifierCountAndFlags = uModifierCount | pBlockParsingState->uPreParsingFlags;
    pNewBlock->uChildBlockCount = uChildBlockCount;
    pNewBlock->uDescendantBlockCount = pBlockParsingState->uDescendantBlockCount;
    pNewBlock->tStatements = tStatements;
    pNewBlock->tModifiers = tModifiers;
    pNewBlock->tChildBlockIndices = tChildBlockIndices;

    if (pBlockParsingState > tBlocks) {
        pNewBlock->uParentBlock = (pBlockParsingState-1)->uIndexInBlockVec;
        (pBlockParsingState-1)->vecStatements[pBlockParsingState->uIndexOfParentStatementInParentBlock].iChildBlock = int(pBlockParsingState->uIndexInBlockVec);
        (pBlockParsingState-1)->uDescendantBlockCount += 1u + pBlockParsingState->uDescendantBlockCount;
    } else {
        pNewBlock->uParentBlock = 0xFFFFFFFFu;
    }
    pNewBlock->uIndexOfParentStatementInParentBlock = pBlockParsingState->uIndexOfParentStatementInParentBlock;

#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    char szTmp[256];
    sprintf(szTmp, "Converted block %u to final AstBlock, with %u total descendants", pBlockParsingState->uIndexInBlockVec, pNewBlock->uDescendantBlockCount);
    platform_log_info(szTmp, true);
#endif

    return pNewBlock;
}

#define post_parser_set_error_from_pre_statement(uErrCode, pPreStatement) do { \
    *outError = uErrCode; \
    pPreStatement->uRefStartOfError = make_packed_token_ref(pPreStatement->pivotToken1); \
} while(0)

#define post_parser_set_error_from_pre_node(uErrCode, pPreNode) do { \
    *outError = uErrCode; \
    pPreStatement->uRefStartOfError = make_packed_token_ref(pPreNode->pivotalToken); \
} while(0)

#define post_parser_set_error_from_node(uErrCode, pNode) do { \
    *outError = uErrCode; \
    pPreStatement->uRefStartOfError = pNode->uPivotalTokenPackedRef; \
} while(0)

#define post_parser_set_error_from_pre_modifier(uErrCode, pModifier) do { \
    *outError = uErrCode; \
    pPreStatement->uRefStartOfError = make_packed_token_ref(pPreNode->pivotalToken); \
} while(0)

local_func void _check_paired_statements(BlockParsingState* pCurrentBlock, TmpArray<AstBlock*>* pVecFinalBlocks, 
    PreStatement* pPreStatement, LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError)
{
    AstStatement* pLastStatement = 0;
    AstNode* firstNodeInLastStatement = 0;
    bool bLastStatementInError = false;
    if (pCurrentBlock->vecStatements.size()) {
        pLastStatement = &pCurrentBlock->vecStatements.last();
        if (0 == (pLastStatement->uFlags & IN_ERROR_STATEMENT_MASK))
        {
            AstBlock* pFirstChildBlock = 0;
        punchthrough_statement:
            Assert_(pLastStatement->uNodeCount);
            firstNodeInLastStatement = pLastStatement->tNodes;
            if ((firstNodeInLastStatement->uNodeKindAndFlags & 0x000000FFu) == ENODE_SECONDARY_TOKEN_WRAPPER) {
                firstNodeInLastStatement = pLastStatement->tNodes + firstNodeInLastStatement->uPrimaryChildNodeIndex;
            }
            // TODO: WITH ?
            /*
            if ((firstNodeInLastStatement->uNodeKindAndFlags & 0x000000FFu) == ENODE_WITH &&
                (pLastStatement->uFlags & STATEMENT_IS_PARENT_OF_INLINED)) {
                if (pFirstChildBlock) {
                    Assert_(pFirstChildBlock->uChildBlockCount);
                    pFirstChildBlock = (*pVecFinalBlocks)[pFirstChildBlock->tChildBlockIndices[0]]; // first child block of punched-through block
                } else {
                    Assert_(pCurrentBlock->vecChildBlockIndices.size());
                    pFirstChildBlock = (*pVecFinalBlocks)[pCurrentBlock->vecChildBlockIndices.last()]; // last parsed child block of current block
                }
                if (pFirstChildBlock->uModifierCountAndFlags & IN_ERROR_BLOCK_MASK)
                    goto statement_in_error;
                Assert_(pFirstChildBlock->uStatementCount);
                pLastStatement = pFirstChildBlock->tStatements;
                goto punchthrough_statement; // loop
            }
            */

        } else {
        statement_in_error:
            bLastStatementInError = true;
        }
    }

    switch(pPreStatement->uStatementKind) {
        case ESTATEMENT_PAN_ENDNAMESPACE:
            if (bLastStatementInError || !firstNodeInLastStatement ||
                u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_ST_PAN_SPECIAL ||
                u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_NAMESPACE)
            {
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                platform_log_info("*** #endnamespace directive without matching #namespace (as last statement of same block)", true);
#endif
                post_parser_set_error_from_pre_statement(PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT, pPreStatement);
            }
            return;

        case ESTATEMENT_PAN_ENDPRIVATE:
            if (bLastStatementInError || !firstNodeInLastStatement ||
                u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_ST_PAN_SPECIAL ||
                u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_PRIVATE)
            {
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                platform_log_info("*** #endprivate directive without matching #private (as last statement of same block)", true);
#endif
                post_parser_set_error_from_pre_statement(PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT, pPreStatement);
            }
            return;

        case ESTATEMENT_PAN_ELIF: case ESTATEMENT_PAN_ELSE:
            if (bLastStatementInError || !firstNodeInLastStatement || 
                u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_ST_PAN_SPECIAL ||
                (u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_IF &&
                 u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_ELIF))
            {
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                platform_log_info("*** #elif or #else directive without matching #if or #elif (as last statement of same block)", true);
#endif
                post_parser_set_error_from_pre_statement(PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT, pPreStatement);
            }
            return;

        case ESTATEMENT_PAN_ENDIF:
            if (bLastStatementInError || !firstNodeInLastStatement || 
                u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_ST_PAN_SPECIAL ||
                (u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_IF &&
                 u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_ELIF &&
                 u8(firstNodeInLastStatement->uNodeKindAndFlags >> 8) != EKEY_PAN_ELSE))
            {
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                platform_log_info("*** #endif directive without matching #if or #elif or #else (as last statement of same block)", true);
#endif
                post_parser_set_error_from_pre_statement(PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT, pPreStatement);
            }
            return;

        // disabled: TC can take care of regular if pairing
        /*
        case ESTATEMENT_ELIF: case ESTATEMENT_ELSE:
            if (bLastStatementInError || !firstNodeInLastStatement || 
                (u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_IF &&
                 u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_ELIF))
            {
                // TODO: maybe 'else' is also usabe out of these if-chains, as part of case statement ?
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                platform_log_info("*** elif or else statement without matching if or elif (as last statement of same block)", true);
#endif
                post_parser_set_error_from_pre_statement(PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT, pPreStatement);
            }
            return;
        */

        // disabled: TC can take care of loop finalizers
        /*
        case ESTATEMENT_LOOPFINALIZER:
            if (bLastStatementInError || !firstNodeInLastStatement || 
                    (u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_FOR &&
                     u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_WHILE &&
                     u8(firstNodeInLastStatement->uNodeKindAndFlags) != ENODE_LOOP_FINALIZER))
            {
                // TODO: also check no doubles of same loop finalizers ?
#if TRACE_PRE_PARSER_PRINTLEVEL > 0
                platform_log_info("*** onbreak|whennone|ondone loop finalizer without matching peer or 'for' or 'while' (as last statement of same block)", true);
#endif
                post_parser_set_error_from_pre_statement(PERR_PAIRED_STATEMENT_MISSING_MATCHING_STATEMENT, pPreStatement);
            }
            return;
        */
    }

    // TODO: check 'case' statements, check other stuff ?
}

u32 _convert_pre_node_to_node(u32* ioWrittenNodes, AstNode* tNodes, PreAstNode* pPreNode,
    BlockParsingState* pCurrentBlock, bool bAllowModifiers, PreStatement* pPreStatement,
    LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError);

local_func_inl AstModifierNode* _convert_pre_modifier_to_modifier(PreAstModifierNode* preModifier,
    u32* ioWrittenNodes, AstNode* tNodes, PreStatement* pPreStatement, LocLib_OS_WrapperFunctions* pOsFuncs,
    Arena arenaForFinalAst, u16* outError) 
{
    AstModifierNode* pNewModifier = reinterpret_cast<AstModifierNode*>(alloc_from(arenaForFinalAst,
        sizeof(AstModifierNode), alignof(AstModifierNode)));
    pNewModifier->uPayload = preModifier->pivotalToken.token.uTokenPayloadAndKind >> 8;
    pNewModifier->uPivotalTokenPackedRef = make_packed_token_ref(preModifier->pivotalToken);
    if (preModifier->optOpeningParens.token.uTokenCharCount) {
        Assert_(preModifier->valueExpression &&
               preModifier->valueExpression->uNodeKind == ENODE_EXPR_PARENTISED &&
               preModifier->valueExpression->primaryChildNode);
        pNewModifier->uOpenParensTokenPackedRef = make_packed_token_ref(preModifier->optOpeningParens);
        pNewModifier->uCloseParensTokenPackedRef = make_packed_token_ref(preModifier->valueExpression->secondaryToken);
        pNewModifier->uValueExpressionIndex = _convert_pre_node_to_node(ioWrittenNodes,
            tNodes, preModifier->valueExpression->primaryChildNode, 0, false, pPreStatement, pOsFuncs, outError); 
    } else {
        Assert_(0 == preModifier->valueExpression);
        pNewModifier->uValueExpressionIndex = INVALID_NODE_INDEX;
    }
    pNewModifier->uNext = INVALID_NODE_INDEX;
    return pNewModifier;
}

local_func bool _check_is_single_simple_expression(PreAstNode* pPreNode, PreAstNode* pParentNode, PreStatement* pPreStatement, u16* outError)
{
    if (!pPreNode) {
        post_parser_set_error_from_pre_node(PERR_EXPECTED_EXPRESSION, pParentNode);
        return false;
    } else {
        switch (pPreNode->uNodeKind) {
            case ENODE_EXPRLIST_NODE:
            case ENODE_VARIABLE_DECL:
                post_parser_set_error_from_pre_node(PERR_EXPECTED_EXPRESSION, pPreNode);
                return false;
            case ENODE_EXPR_BINARYOP:
                if (u8(pPreNode->pivotalToken.token.uTokenPayloadAndKind >> 8) == ETOK_SINGLE_EQ) {
                    post_parser_set_error_from_pre_node(PERR_EXPECTED_EXPRESSION, pPreNode);
                    return false;
                }
                break;
        }
        return true;
    }
}

local_func void _add_modifiers_to_node(AstNode* pNode, u32* ioWrittenNodes, AstNode* tNodes,
    PreAstModifierNode* preModifiers, PreAstModifierNode* postModifiers, PreStatement* pPreStatement,
    BlockParsingState* pCurrentBlock, LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError)
{
    if (preModifiers || postModifiers)
        Assert_(false); // TODO: not yet implemented

    /*
    u32 uIndexOfNext = pNode->uFirstModifierNodeIndex;
    PreAstModifierNode* pToAppend = preModifiers;
    while (pToAppend) {
        AstModifierNode* pAsModifier = _convert_pre_modifier_to_modifier(pToAppend,
            ioWrittenNodes, tNodes, pPreStatement, pOsFuncs, pCurrentBlock->vecModifiers._impl.pAllocator, outError);
        pAsModifier->uNext = uIndexOfNext;
        uIndexOfNext = pCurrentBlock->vecModifiers.size();
        pCurrentBlock->vecModifiers.append(pAsModifier);
        pToAppend = pToAppend->next;
    }
    pToAppend = postModifiers;
    while (pToAppend) {
        AstModifierNode* pAsModifier = _convert_pre_modifier_to_modifier(pToAppend,
            ioWrittenNodes, tNodes, pPreStatement, pOsFuncs, pCurrentBlock->vecModifiers._impl.pAllocator, outError);
        pAsModifier->uNext = uIndexOfNext;
        pAsModifier->uPayload |= EMODIFIERFLAG_POSTSTATEMENT_OR_EXPRESSION;
        uIndexOfNext = pCurrentBlock->vecModifiers.size();
        pCurrentBlock->vecModifiers.append(pAsModifier);
        pToAppend = pToAppend->next;
    }
    pNode->uFirstModifierNodeIndex = uIndexOfNext;
    */
}

local_func u32 _convert_pre_node_to_node(u32* ioWrittenNodes, AstNode* tNodes, PreAstNode* pPreNode,
    BlockParsingState* pCurrentBlock, bool bAllowModifiers, PreStatement* pPreStatement, 
    LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError)
{
    Assert_(pPreNode->iCountSubNodesOrNegErr >= 0);
    Assert_(pPreNode->uNodeKind < COUNT_NODE_KINDS);
    u32 uIndex = *ioWrittenNodes;
    AstNode* pNode = tNodes + uIndex;
    *ioWrittenNodes += 1;
    pNode->uNodeKindAndFlags = pPreNode->uNodeKind;
    if (pPreNode->uNodeFlags & EPRENODEFLAG_IS_COMPTIME)
        pNode->uNodeKindAndFlags |= ENODEKINDFLAG_IS_COMPTIME;
    pNode->uPivotalTokenPackedRef = make_packed_token_ref(pPreNode->pivotalToken);
    // pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX; // most common case is already assigned in the following
    pNode->uSecondaryChildNodeIndex = INVALID_NODE_INDEX;
    //pNode->uFirstModifierNodeIndex = INVALID_NODE_INDEX;

    if (pPreNode->uNodeKind == ENODE_EXPRLIST_NODE) { // <namable-expr-or-decl> [, <namable-expr-or-decl>]*
        // breaking recursion for chains to avoid stack overflow on very long chains
        PreAstNode* pPreNodeIt = pPreNode;
        AstNode* pNodeIt = pNode;
        do {
        iterate_list:
            Assert_(pPreNodeIt->primaryChildNode);
            pNodeIt->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNodeIt->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            if (*outError)
                break;
            if (pPreNodeIt->secondaryChildNode && pPreNodeIt->secondaryChildNode->uNodeKind == ENODE_EXPRLIST_NODE) {
                u32 uNewListNodeIndex = *ioWrittenNodes;
                pNodeIt->uSecondaryChildNodeIndex = uNewListNodeIndex;
                pNodeIt = tNodes + uNewListNodeIndex;
                *ioWrittenNodes += 1;
                pNodeIt->uNodeKindAndFlags = ENODE_EXPRLIST_NODE;
                pNodeIt->uPivotalTokenPackedRef = make_packed_token_ref(pPreNodeIt->pivotalToken);
                //pNodeIt->uFirstModifierNodeIndex = INVALID_NODE_INDEX;
                if (pPreNodeIt->firstPreModifier || pPreNodeIt->firstPostModifier) {
                    if (bAllowModifiers) {
                        _add_modifiers_to_node(pNodeIt, ioWrittenNodes, tNodes,
                            pPreNodeIt->firstPreModifier, pPreNodeIt->firstPostModifier,
                            pPreStatement, pCurrentBlock, pOsFuncs, outError);
                    } else {
                        if (pPreNodeIt->firstPreModifier)
                            post_parser_set_error_from_pre_modifier(PERR_MODIFIER_NOT_ALLOWED_WITHIN_MODIFIER_PARAMETERS,
                                pPreNodeIt->firstPreModifier);
                        else 
                            post_parser_set_error_from_pre_modifier(PERR_MODIFIER_NOT_ALLOWED_WITHIN_MODIFIER_PARAMETERS,
                                pPreNodeIt->firstPostModifier);
                        break;
                    }
                }
                pPreNodeIt = pPreNodeIt->secondaryChildNode;
                goto iterate_list;
            }
        } while (0);

        if (pPreNodeIt->secondaryChildNode && !*outError) { // last expression in expr list if no trailing comma
            pNodeIt->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNodeIt->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
        } else {
            pNodeIt->uSecondaryChildNodeIndex = INVALID_NODE_INDEX; // trailing comma case ?
        }




    } else {

        switch (pPreNode->uNodeKind) {
        case ENODE_ATOMICEXPR_NATURAL_NUMBER_LITERAL:    // 128, 0x5F, 0b_0010_1000_1111
        case ENODE_ATOMICEXPR_FLOATING_POINT_LITERAL:    // 10.0, 3.14159265, 5e-8, 0x1.ABCD_EF01_23p55, 0b_1.1p-2
        case ENODE_ATOMICEXPR_STRING_LITERAL:            // "hello, world"
        case ENODE_ATOMICEXPR_CODEPOINT_LITERAL:         // #"h", #"\u03FF", #"\U000003FF"
            pNode->uNodeKindAndFlags |= ENODEKINDFLAG_IS_LITERAL;
            if (pPreNode->uNodeFlags & EPRENODEFLAG_LITERAL_IS_EMBEDDED64) {
                pNode->uNodeKindAndFlags |= ENODEKINDFLAG_IS_LITERAL_EMBEDDED64;
            }
            pNode->uLiteralPayload64 = pPreNode->primaryPayload;
            break;

        case ENODE_ATOMICEXPR_IDENTIFIER:                // myVariable
            pNode->uPrimaryPayload = u32(pPreNode->primaryPayload);
            break;

        case ENODE_ATOMICEXPR_SPECIAL:
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            pNode->uPrimaryPayload = 0;
            break;

        case ENODE_EXPR_PARENTISED:          // ( <expr> )
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                pNode->uSecondaryTokenPackedRef = make_packed_token_ref(pPreNode->secondaryToken);
            }
            break;

        case ENODE_EXPR_CURLYINIT:          // { <expr-or-list (namable)> }
        case ENODE_EXPR_ARRAYINIT:          // #[ <expr-or-list> ]
        case ENODE_EXPR_SETINIT:            // #{ <expr-or-list> }
        case ENODE_EXPR_MAPINIT:            // #( <expr-or-list (arrows? namable?)> )
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode); // preparser should not have emitted otherwise
            Assert_(pPreNode->primaryChildNode->uNodeKind == ENODE_SUBEXPR_WRAPPER);
            // we invert primary and secondary in the post-parsed node,
            //   to have same layout as the associated spe-binop with explicit type in first position...
            //   note that those spe-binops would themselves be kind-converted to those EXPR_**INIT post-nodes here.
            pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_EXPR_BINARYOP:
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)); // preparser should not have emitted otherwise
            /*if (u8(pNode->uNodeKindAndFlags >> 8) == ETOK_SINGLE_EQ && !*outError) {
                Assert_(pPreNode->secondaryChildNode);
                Assert_(_check_is_single_simple_expression(pPreNode->secondaryChildNode, pPreNode, pPreStatement, outError));
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                pNode->uNodeKindAndFlags |= ENODEKINDFLAG_HAS_NAMED_VALUE;
                pNode->uNodeKindAndFlags |= tNodes[pNode->uSecondaryChildNodeIndex].uNodeKindAndFlags & ENODEKINDFLAG_HAS_VAR_DECL;
            } else*/ {
                Assert_(_check_is_single_simple_expression(pPreNode->secondaryChildNode, pPreNode, pPreStatement, outError)); // preparser should not have emitted otherwise
                switch (u8(pNode->uNodeKindAndFlags >> 8)) {
                    case ETOK_AND:
                    case ETOK_OR:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_BOOL_BINARYOP;
                        break;
                    case ETOK_ARE_EQUAL:
                    case ETOK_ARE_NOT_EQUAL:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_EQ_CMP_BINARYOP;
                        break;
                    case ETOK_LESSER_THAN:
                    case ETOK_GREATER_THAN:
                    case ETOK_LESSER_OR_EQ:
                    case ETOK_GREATER_OR_EQ:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_ORD_CMP_BINARYOP;
                        break;
                } // what would thus remain as special binops in post-nodes are:
                  //   array-indexing and slicing
                  //   dot_descent
                  //   dereference
            }
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            if (!*outError)
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_EXPR_DEREF: // a bare 'deref' would not trigger this case, since it should be preparsed as a special binop, see post-treatment below.
            // => This case is only for the implicit-deref of a deref-dot-and-descent, identifiable by token '.^'
            Assert_(u8((pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u) >> 8) == ETOK_POINTER_DECL); // '^.' is the implicit 'deref' before deref-dot-descent
            pNode->uNodeKindAndFlags |= u32(ETOK_DEREFERENCE) << 8; // and we replace its token-payload with payload for 'deref'...
            Assert_(_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)); // preparser should not have emitted otherwise
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            Assert_(pPreNode->secondaryChildNode == 0);
            break;

        case ENODE_EXPR_SPECIAL_BINARYOP:       // <lhs-expr> . ( [ { <rhs-expr>, 
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)); // preparser should not have emitted otherwise
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            if (u8(pNode->uNodeKindAndFlags >> 8) == ETOK_DEREFERENCE) { // '^' as a spe binop is a 'deref', and has no further arg.
                pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                pNode->uNodeKindAndFlags |= ENODE_EXPR_DEREF;
                Assert_(pPreNode->secondaryChildNode == 0);
            } else { // all others shall have a secondary
                Assert_(pPreNode->secondaryChildNode);
                // '^.' as a spe binop is dot-descent after implicit deref (implicit deref shall have been spawned as a distinct prenode)
                if (u8(pNode->uNodeKindAndFlags >> 8) == ETOK_POINTER_DECL) {
                    pNode->uNodeKindAndFlags &= 0xFFFF'00FFu;
                    pNode->uNodeKindAndFlags |= u32(ETOK_DOT) << 8; // and we replace its token-payload with payload for 'dot-descent'...
                }
                Assert_(u8(pNode->uNodeKindAndFlags >> 8) == ETOK_DOT ||
                    pPreNode->secondaryChildNode->uNodeKind == ENODE_SUBEXPR_WRAPPER);
                if (!*outError)
                    pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                switch (u8(pNode->uNodeKindAndFlags >> 8)) {
                    case ETOK_INVOCATION:       // '(' as a spe binop is invocation-form.
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_INVOCATION_FORM;
                        break;

                    case ETOK_INDEXING:         // '[' as a spe binop is indexing-or-slicing.
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_INDEXING;
                        break;

                    case ETOK_DOT:              // '.' as a spe binop is dot-descent
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_DOT_DESCENT;
                        break;

                    // the following inits-with-explicit-left-hand-types are converted to the associated 'EXPR_**INIT'
                    //
                    case ETOK_OPENING_CURLY:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_CURLYINIT;
                        break;
                    case ETOK_OPENING_ARRAY_LIT:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_ARRAYINIT;
                        break;
                    case ETOK_OPENING_SET_LIT:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_SETINIT;
                        break;
                    case ETOK_OPENING_MAP_LIT:
                        pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                        pNode->uNodeKindAndFlags |= ENODE_EXPR_SETINIT;
                        break;

                    default:
                        Assert_(false); // there should be nothing left as a spe-binop in post-nodes.
                }
            }
            break;

        case ENODE_EXPR_UNARYOP:             // <~><following-expr>
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            Assert_(_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)); // preparser should not have emitted otherwise
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            switch (u8(pNode->uNodeKindAndFlags >> 8)) {
                case ETOK_ARRAYLIKE_DECL:
                //case ETOK_OPENING_DYNARRAY: // this one is currently deprecated, but hey...
                case ETOK_HDIC:
                    // Those three were parsed as unaryop because of how we parse tokens, and for op-priority checks purposes...
                    //   but are not really 'unary' anything, as they all have child nodes within them (as secondaries).
                    Assert_(pPreNode->secondaryChildNode && pPreNode->secondaryChildNode->uNodeKind == ENODE_SUBEXPR_WRAPPER);
                    if (!*outError)
                        pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                            pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                    // Anyway, they'll all be recategorized as type-constructors...
                    pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                    pNode->uNodeKindAndFlags |= ENODE_EXPR_TYPE_CTOR;
                    break;

                // All other unary 'type-related' modifiers will be categorized as type-constructors instead.
                case ETOK_COMPACT_DECL:
                case ETOK_POINTER_DECL:
                    pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                    pNode->uNodeKindAndFlags |= ENODE_EXPR_TYPE_CTOR;
                    break;

                case ETOK_NOT:
                    // we prefer separating the boolean 'not' from the other unaries, to make it slightly easier to the typechecker.
                    pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                    pNode->uNodeKindAndFlags |= ENODE_EXPR_BOOL_NOT;
                    break;

                // The following are grouped as 'somethingof' unaries... maybe for no good reason.
                //   TODO: CLEANUP: check if this is really a great idea and helping with anything.
                case ETOK_DISTINCT:     // CLEANUP: check if distinct really belongs here
                case ETOK_SIZEOF:
                case ETOK_ALIGNOF:
                case ETOK_STRIDEOF:     // currently deprecated
                case ETOK_TYPEOF:
                case ETOK_TYPEINFOOF:
                case ETOK_SIGNATUREOF:  // currently deprecated
                case ETOK_CLOSUREOF:    // currently unsupported
                case ETOK_ASCLOSURE:    // CLEANUP: check if as_closure really belongs here
                    pNode->uNodeKindAndFlags &= 0xFFFF'FF00u;
                    pNode->uNodeKindAndFlags |= ENODE_EXPR_SOMETHINGOF;
                    break;
            }
            break;

        case ENODE_SUBEXPR_WRAPPER:
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (pPreNode->primaryChildNode)
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            else
                pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            break;

        case ENODE_SUBEXPR_SLICE:              // colon token within indexing form
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if ((!pPreNode->primaryChildNode || _check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) && 
                (!pPreNode->secondaryChildNode || _check_is_single_simple_expression(pPreNode->secondaryChildNode, pPreNode, pPreStatement, outError))) {
                if (pPreNode->primaryChildNode)
                    pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                else
                    pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;

                if (!*outError && pPreNode->secondaryChildNode)
                    pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                else
                    pNode->uSecondaryChildNodeIndex = INVALID_NODE_INDEX;
            }
            break;

        case ENODE_EXPR_TERNARY_IF:          // ifx <condition> [then(as wrapper)] <subexpr-either-or>
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            Assert_(pPreNode->secondaryChildNode && (pPreNode->secondaryChildNode->uNodeKind == ENODE_SUBEXPR_WRAPPER ||
                                                    pPreNode->secondaryChildNode->uNodeKind == ENODE_SUBEXPR_EITHER_OR));
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError))
            {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                if (!*outError)
                    pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ENODE_SUBEXPR_EITHER_OR:     // ([then]...) <exprwhentrue> else <exprwhenfalse>
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError) &&
                _check_is_single_simple_expression(pPreNode->secondaryChildNode, pPreNode, pPreStatement, outError))
            {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                if (!*outError)
                    pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ENODE_VARIABLE_DECL:            // <ident> [, <ident>]* as <type-and-init-value>
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            Assert_(pPreNode->secondaryChildNode);
            pNode->uNodeKindAndFlags |= ENODEKINDFLAG_HAS_VAR_DECL;
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            if (!*outError)
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_EXPR_SINGLE_EQ:      // [<type-expr>] = <initial-value-expr>
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->secondaryChildNode);
            if (pPreNode->primaryChildNode && _check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            } else
                pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            if (!*outError)
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_EXPR_IN:                // <iterator-decl> in <iterable-expr>
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            Assert_(pPreNode->secondaryChildNode);
            if (_check_is_single_simple_expression(pPreNode->secondaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                if (!*outError)
                    pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;

        // TODO: not yet implemented
        /*
        case ENODE_FOREIGNSOURCE:            // static|dynamic (<libname-expr>) <proc-decl>
            Assert_(pPreNode->primaryChildNode && pPreNode->primaryChildNode->uNodeKind == ENODE_EXPR_PARENTISED);
            if (_check_is_single_simple_expression(pPreNode->secondaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uNodeKindAndFlags |= (pPreNode->pivotalToken.token.uTokenPayload & 0x0FF) << 8;
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
                if (!*outError)
                    pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                        pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;
        */

        // disabled: now an unary ?
        /*
        case ENODE_DISTINCT:                 // distinct <type>
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;
        */

        case ENODE_EXPR_PROCLIKE_DEF:      // procs, functions, macros... (tailprocs, tailerrs... ??). pivot token : <kind>.
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode->uNodeKind == ENODE_PROCPARAMS_WRAP_ALL);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            if (pPreNode->secondaryChildNode && !*outError) { // where clauses
                Assert_(pPreNode->secondaryChildNode->uNodeKind == ENODE_PROCPARAMS_WHERE_CLAUSE);
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            } else {
                pNode->uSecondaryChildNodeIndex = INVALID_NODE_INDEX;
            }
            break;

        case ENODE_EXPR_OTHER_DEF:        // struct, union, views, enum...
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (pPreNode->primaryChildNode) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            } else {
                pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            }
            break;

        case ENODE_PROCPARAMS_WRAP_ALL:           // params part of signature (in+out) (pivot token '(').
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode && pPreNode->primaryChildNode->uNodeKind == ENODE_PROCPARAMS_IN);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            if (pPreNode->secondaryChildNode && !*outError) { // params out
                Assert_(pPreNode->secondaryChildNode->uNodeKind == ENODE_PROCPARAMS_OUT);
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->secondaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            } else {
                pNode->uSecondaryChildNodeIndex = INVALID_NODE_INDEX;
            }
            break;

        case ENODE_PROCPARAMS_IN:            // in-params part of signature (pivot token ')').
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (pPreNode->primaryChildNode) { // optional content of in-params within parens
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            } else {
                pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            }
            break;

        case ENODE_PROCPARAMS_OUT:           // out-params part of signature (pivot token '->').
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_PROCPARAMS_WHERE_CLAUSE:        // where-clause part of signature (pivot token 'where').
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_EXPR_LOAD:                      // #load filename
            pNode->uNodeKindAndFlags |= pPreNode->pivotalToken.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreNode->primaryChildNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        case ENODE_ST_USING:    // using <namespace>
        case ENODE_EXPR_USING:  // using <namespace>
            Assert_(pPreNode->primaryChildNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;

        // currently disabled (maybe typeof instead)
        /*
        case ENODE_EXPLICIT_SIGNATURE:       // signature <proclike definition>
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;
        */

        // TODO: not yet implemented
        /*
        case ENODE_LOAD_EXPR_OR_STATEMENT:   // #load <filename-string>
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ENODE_FOREIGN_EXPR:             // #foreign <foreign-source> 
            if (_check_is_single_simple_expression(pPreNode->primaryChildNode, pPreNode, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                    pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ENODE_USING_EXPR_OR_STATEMENT:  // using <namespace>
            Assert_(pPreNode->primaryChildNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreNode->primaryChildNode,
                pCurrentBlock, bAllowModifiers, pPreStatement, pOsFuncs, outError);
            break;
        */

        default:
            Assert_(false);
        }
    }

    if (!*outError) {
        if (pPreNode->firstPreModifier || pPreNode->firstPostModifier) {
            if (bAllowModifiers) {
                _add_modifiers_to_node(pNode, ioWrittenNodes, tNodes,
                    pPreNode->firstPreModifier, pPreNode->firstPostModifier,
                    pPreStatement, pCurrentBlock, pOsFuncs, outError);
            } else {
                if (pPreNode->firstPreModifier)
                    post_parser_set_error_from_pre_modifier(PERR_MODIFIER_NOT_ALLOWED_WITHIN_MODIFIER_PARAMETERS,
                        pPreNode->firstPreModifier);
                else
                    post_parser_set_error_from_pre_modifier(PERR_MODIFIER_NOT_ALLOWED_WITHIN_MODIFIER_PARAMETERS,
                        pPreNode->firstPostModifier);
            }
        }
    }

    return uIndex;
}

local_func u32 _convert_pre_statement_to_node(u32* ioWrittenNodes, AstNode* tNodes, PreStatement* pPreStatement,
    BlockParsingState* pCurrentBlock, LocLib_OS_WrapperFunctions* pOsFuncs, u16* outError)
{
    Assert_(pPreStatement->uStatementKind < ESTATEMENT_MODIFIER_ONLY_LINE);
    if (pPreStatement->uStatementKind != ESTATEMENT_SINGLE_EXPRESSION) {
        u32 uIndex = *ioWrittenNodes;
        AstNode* pNode = tNodes + uIndex;
        *ioWrittenNodes += 1;
        pNode->uPivotalTokenPackedRef = make_packed_token_ref(pPreStatement->pivotToken1);
        // pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX; // most common case is already assigned in the following
        pNode->uSecondaryChildNodeIndex = INVALID_NODE_INDEX;
        //pNode->uFirstModifierNodeIndex = INVALID_NODE_INDEX;
        switch (pPreStatement->uStatementKind) {
        case ESTATEMENT_CONST_DECLARATION:       // <identifier> :: <expression>
        case ESTATEMENT_VAR_DECLARATION:         // <identifier-list> as [<type>] [= <initialvalue>]
            pNode->uNodeKindAndFlags = ENODE_ST_DECLARATION;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreStatement->pMainNode);
            Assert_(pPreStatement->pSecondaryNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            if (!*outError)
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pSecondaryNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            break;

        case ESTATEMENT_ASSIGNMENT:              // <expression-list> := <expression-list>
            pNode->uNodeKindAndFlags = ENODE_ST_ASSIGNMENT;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreStatement->pMainNode);
            Assert_(pPreStatement->pSecondaryNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            if (!*outError)
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pSecondaryNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            break;

        case ESTATEMENT_OP_AND_ASSIGNMENT:       // <expression> *= <expression>
            pNode->uNodeKindAndFlags = ENODE_ST_OP_AND_ASSIGN;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            Assert_(pPreStatement->pMainNode);
            Assert_(pPreStatement->pSecondaryNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            if (!*outError)
                pNode->uSecondaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pSecondaryNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            break;

        case ESTATEMENT_NO_OP:                   // noop
        case ESTATEMENT_ELSE:                    // (if/elif) ... else //
        case ESTATEMENT_UNREACH:                 // unreachable
        case ESTATEMENT_DEFER:                   // defer //
            pNode->uNodeKindAndFlags = ENODE_ST_CONTROL_FLOW;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            break;

        case ESTATEMENT_RETURN:                  // return [<expr-list>]
            pNode->uNodeKindAndFlags = ENODE_ST_CONTROL_FLOW;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (pPreStatement->pMainNode) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            } else
                pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            break;

        case ESTATEMENT_BREAK:                   // break [<label-or-iter>]
        case ESTATEMENT_LOOP:                    // loop [<label-or-iter>]
        case ESTATEMENT_CONTINUE:                // continue [<label-or-iter>]
            pNode->uNodeKindAndFlags = ENODE_ST_CONTROL_FLOW;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (pPreStatement->pMainNode) {
                if (_check_is_single_simple_expression(pPreStatement->pMainNode, 0, pPreStatement, outError)) {
                    pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                        pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
                }
            } else
                pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            break;

        case ESTATEMENT_IF:                      // if <condition> [then] // 
        case ESTATEMENT_ELIF:                    // (if/elif) ... elif <condition> [then] //
        case ESTATEMENT_WHILE:                   // while <condition> [do] //
        case ESTATEMENT_CASE:                    // case <expression> //
        case ESTATEMENT_LABEL:                   // <label>: //*
            pNode->uNodeKindAndFlags = ENODE_ST_CONTROL_FLOW;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (_check_is_single_simple_expression(pPreStatement->pMainNode, 0, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ESTATEMENT_FOR:                     // for [<iterator> in] <iterable> [do] //
            pNode->uNodeKindAndFlags = ENODE_ST_CONTROL_FLOW;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (_check_is_single_simple_expression(pPreStatement->pMainNode, 0, pPreStatement, outError)) {
                if (pPreStatement->uStatementFlags & ESTATEMENTFLAGS_REVERSE_FOR)
                    pNode->uNodeKindAndFlags |= ENODE_FOR_IS_REVERSED;
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ESTATEMENT_LOOP_FINALIZER:          // (for/while) ... onbreak/whennone/ondone [or...] //
            pNode->uNodeKindAndFlags = ENODE_ST_CONTROL_FLOW;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (pPreStatement->pivotToken2.token.uTokenCharCount) {
                Assert_(u8(pPreStatement->pivotToken2.token.uTokenPayloadAndKind >> 8) == EKEY_OR);
                pNode->uNodeKindAndFlags |= ENODEKINDFLAG_IS_LOOPFINALIZER_CHAINED;
                Assert_(pPreStatement->pMainNode && pPreStatement->pMainNode->uNodeKind == ENODE_ATOMICEXPR_SPECIAL);
                u8 uTokenPayload = u8(pPreStatement->pMainNode->pivotalToken.token.uTokenPayloadAndKind >> 8);
                Assert_(uTokenPayload == EKEY_ONBREAK || uTokenPayload == EKEY_WHENNONE || uTokenPayload == EKEY_ONDONE);
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
                // re-purposing of this special node as a loop-finalizer ?? // TODO: CLEANUP: check this again...
                tNodes[pNode->uPrimaryChildNodeIndex].uNodeKindAndFlags &= 0xFFFFFF00u;
                tNodes[pNode->uPrimaryChildNodeIndex].uNodeKindAndFlags |= ENODE_ST_CONTROL_FLOW;
            } else {
                Assert_(0 == pPreStatement->pMainNode);
            }
            break;

        case ESTATEMENT_PAN_IF:                  // #if <condition>  ///
        case ESTATEMENT_PAN_ELIF:                // (#if/#elif) ... #elif <condition>  ///
        case ESTATEMENT_PAN_NAMESPACE:           // #namespace <id> ///
            pNode->uNodeKindAndFlags = ENODE_ST_PAN_SPECIAL;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (_check_is_single_simple_expression(pPreStatement->pMainNode, 0, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            }
            break;

        case ESTATEMENT_PAN_ELSE:                // (#if/#elif) ... #else ///
        case ESTATEMENT_PAN_ENDIF:               // (#if) ... #endif
        case ESTATEMENT_PAN_PRIVATE:             // #private ///
        case ESTATEMENT_PAN_ENDPRIVATE:          // (#private) ... #endprivate
        case ESTATEMENT_PAN_ENDNAMESPACE:        // (#namespace) ... #endnamespace
            pNode->uNodeKindAndFlags = ENODE_ST_PAN_SPECIAL;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            pNode->uPrimaryChildNodeIndex = INVALID_NODE_INDEX;
            break;

        case ESTATEMENT_USING:                   // using #load "somefile.loc" ; using <namespace> ; using <enum> ; including <structlike>
            pNode->uNodeKindAndFlags = ENODE_ST_USING;
            pNode->uNodeKindAndFlags |= pPreStatement->pivotToken1.token.uTokenPayloadAndKind & 0x0000'FF00u;
            if (_check_is_single_simple_expression(pPreStatement->pMainNode, 0, pPreStatement, outError)) {
                pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                    pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            }
            break;

        // TODO: not yet implemented
        /*
        case ESTATEMENT_WITH:                    // with <vardecl> [do] //*
            Assert_(pPreStatement->pMainNode);
            pNode->uPrimaryChildNodeIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
                pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
            break;
        */

        default:
            Assert_(false);
        }

        if (!*outError)
            _add_modifiers_to_node(tNodes + uIndex, ioWrittenNodes, tNodes,
                pPreStatement->firstPreStatementModifier, pPreStatement->firstPostStatementModifier,
                pPreStatement, pCurrentBlock, pOsFuncs, outError);
        return uIndex;

    } else {

        Assert_(pPreStatement->pSecondaryNode == 0);
        u32 uIndex = _convert_pre_node_to_node(ioWrittenNodes, tNodes, pPreStatement->pMainNode,
            pCurrentBlock, true, pPreStatement, pOsFuncs, outError);
        if (!*outError && (pPreStatement->firstPreStatementModifier || pPreStatement->firstPostStatementModifier))
            _add_modifiers_to_node(tNodes + uIndex, ioWrittenNodes, tNodes,
                pPreStatement->firstPreStatementModifier, pPreStatement->firstPostStatementModifier,
                pPreStatement, pCurrentBlock, pOsFuncs, outError);
        return uIndex;
    }
}

local_func void _convert_and_push_pre_statement_to_current_block(BlockParsingState* pCurrentBlock,
    PreStatement* pPreStatement, TmpArray<AstBlock*>* pVecFinalBlocks,
    SourceFileDescAndState* pSourceFile,
    LocLib_OS_WrapperFunctions* pOsFuncs,
    Arena arenaForFinalAST, u16* ioError)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
    char szTmp[256];
    sprintf(szTmp, "  Converting PreStatement to current block");
    platform_log_info(szTmp, true);
    debug_print_pre_statement(pPreStatement, pSourceFile);
#endif
    pCurrentBlock->iStartLineOfLastParsedStatement = pPreStatement->iStartLine;
    pCurrentBlock->pStartingByteOfLastParsedStatement = pPreStatement->pStartingByteOnLine;
    AstStatement newStatement = {};
    newStatement.uFirstLineRelToLineOfParentStatementInParentBlock =
        u32(pPreStatement->iStartLine - pCurrentBlock->iStartLineOfParentStatementInParentBlock);
    newStatement.uStartOfLineByteOffsetRelToStartOfParentStatement =
        u32(pPreStatement->pStartingByteOnLine - pCurrentBlock->pStartingByteOfParentStatementInParentBlock);
    newStatement.uLineCount = u32((1 + pPreStatement->iLineOfLastToken) - pPreStatement->iStartLine);
    newStatement.iChildBlock = -1;

    if (!*ioError) {
        _check_paired_statements(pCurrentBlock, pVecFinalBlocks, pPreStatement, pOsFuncs, ioError);
    } else {
        goto onError;
    }

    if (!*ioError) {
        ArenaRefPoint beforeThisAlloc = get_arena_ref_point(arenaForFinalAST); // to reset in case of error
        Assert_(!pPreStatement->pMainNode || pPreStatement->pMainNode->iCountSubNodesOrNegErr >= 0);
        Assert_(!pPreStatement->pSecondaryNode || pPreStatement->pSecondaryNode->iCountSubNodesOrNegErr >= 0);
        u32 uTotalAstNodeCount = (pPreStatement->uStatementKind == ESTATEMENT_SINGLE_EXPRESSION ? 0 : 1) +
                         (pPreStatement->pMainNode ? 1 + pPreStatement->pMainNode->iCountSubNodesOrNegErr : 0) +
                         (pPreStatement->pSecondaryNode ? 1 + pPreStatement->pSecondaryNode->iCountSubNodesOrNegErr : 0) +
                         pPreStatement->uCountParamNodesWithinModifiers;
        if (pPreStatement->pivotToken2.token.uTokenCharCount && pPreStatement->uStatementKind != ESTATEMENT_LOOP_FINALIZER)
            uTotalAstNodeCount++;
        AstNode* tNodes = (AstNode*)alloc_from(arenaForFinalAST, sizeof(AstNode)*uTotalAstNodeCount, alignof(AstNode));
        newStatement.uNodeCount = uTotalAstNodeCount;
        u32 uWrittenNodes = 0;
        if (0 == pPreStatement->pivotToken2.token.uTokenCharCount ||
            pPreStatement->uStatementKind == ESTATEMENT_LOOP_FINALIZER) { // loop finalizers may have an 'or' as pivotToken2 which does not require wrap
            _convert_pre_statement_to_node(&uWrittenNodes, tNodes, pPreStatement, pCurrentBlock, pOsFuncs, ioError);
        } else {
            uWrittenNodes++;
            /*
            {
                char szTmp[256];
                sprintf(szTmp, "Secondary-Token Wrapper... :");
                ETokenKind eKind = ETokenKind(u8(pPreStatement->pivotToken2.token.uTokenPayloadAndKind) & TOKEN_KIND_MASK);
                if (is_token_symbol_or_keyword(eKind)) {
                    sprintf(szTmp + strlen(szTmp), " '%s'", tStandardPayloadsStr[u8(pPreStatement->pivotToken2.token.uTokenPayloadAndKind >> 8)]);
                }
                platform_log_info(szTmp);
            }
            */
            // TODO: CLEANUP: possibly add u8 from token payload as was commented, below ?
            tNodes[0].uNodeKindAndFlags = ENODE_SECONDARY_TOKEN_WRAPPER; // | (pPreStatement->pivotToken2.token.uTokenPayloadAndKind & 0x0000'FF00u);
            tNodes[0].uPivotalTokenPackedRef = make_packed_token_ref(pPreStatement->pivotToken2);
            tNodes[0].uPrimaryChildNodeIndex = _convert_pre_statement_to_node(&uWrittenNodes, tNodes, pPreStatement,
                pCurrentBlock, pOsFuncs, ioError);
            tNodes[0].uSecondaryChildNodeIndex = INVALID_NODE_INDEX;
            //tNodes[0].uFirstModifierNodeIndex = 0;
        }
        newStatement.tNodes = tNodes;
        Assert_(uWrittenNodes == uTotalAstNodeCount);
#if TRACE_PRE_PARSER_PRINTLEVEL > 2
        sprintf(szTmp, "  Converted %u PreAstNode to nodes", uWrittenNodes);
        platform_log_info(szTmp, true);
        platform_log_info(" ----------------- ", true);
        platform_log_info(" ----------------- ", true);
#endif
        if (*ioError) {
            reset_arena_to(beforeThisAlloc, arenaForFinalAST);
            goto onError;
        }
    } else {
        goto onError;
    }

finalize:
    pCurrentBlock->vecStatements.append(newStatement);
    return;

onError:
    newStatement.tNodes = reinterpret_cast<AstNode*>(pPreStatement);
    newStatement.uFlags |= STATEMENT_IN_ERROR;
    pCurrentBlock->uPreParsingFlags |= BLOCK_HAS_STATEMENTS_IN_ERROR;
    LocLib_Error parserError = {};
    parserError.errCode = *ioError;
    parserError.uBlockOrLineIfScanErr = pCurrentBlock->uIndexInBlockVec;
    u32 uStatement = pCurrentBlock->vecStatements.size();
    parserError.uStatement = uStatement;
    parserError.uTokenRef = pPreStatement->uRefStartOfError;
    pSourceFile->vecErrors.append(parserError);
    goto finalize;
}

local_func BlockParsingState* _open_child_block(BlockParsingState* tBlocks, TmpArray<AstBlock*>* pVecFinalBlocks, BlockParsingState* pCurrentBlock,
                                     ParserParams& parserParams, Arena blockParsingArena)
{
    // TODO: could assert no in-flight modifiers.
    u32 uIndexOfParentStatement = 0xFFFFFFFFu; // in case of child-indent error
    i32 iStartLineOfParentStatementInParentBlock = -1;
    const u8* pStartingByteOfParentStatementInParentBlock = 0;
    if (pCurrentBlock->vecStatements.size()) {
        uIndexOfParentStatement = pCurrentBlock->vecStatements.size()-1;
        iStartLineOfParentStatementInParentBlock = pCurrentBlock->iStartLineOfLastParsedStatement;
        pStartingByteOfParentStatementInParentBlock = pCurrentBlock->pStartingByteOfLastParsedStatement;
    }
    pCurrentBlock++;
#if TRACE_PRE_PARSER_PRINTLEVEL > 1
    char szTmp[256];
    sprintf(szTmp, "Opening child block #%d", u32(pCurrentBlock - tBlocks));
    platform_log_info(szTmp, true);
#endif
    if (u32(pCurrentBlock - tBlocks) >= OPENED_BLOCK_MAX_OVERSIZE) {
        // TODO: report error
        Assert_(false);
    }
    _init_block_parsing_state(pCurrentBlock, pVecFinalBlocks->size(), uIndexOfParentStatement, parserParams.parserState.uCurrentLineTabIndents,
        iStartLineOfParentStatementInParentBlock, pStartingByteOfParentStatementInParentBlock, blockParsingArena);
    pVecFinalBlocks->append(0); // We'll assign ptrs to astblocks in already reserved slots for encountered blocks.
    parserParams.parserState.uCurrentBlockTabIndents = parserParams.parserState.uCurrentLineTabIndents;
    return pCurrentBlock;
}

local_func BlockParsingState* _convert_and_close_block(BlockParsingState* tBlocks, TmpArray<AstBlock*>* pVecFinalBlocks, BlockParsingState* pCurrentBlock,
                                            ParserParams& parserParams, EBlockSpawningState eBlockSpawningState,
                                            Arena arenaForFinalAST, bool bClosedByPanDirective = false)
{
#if TRACE_PRE_PARSER_PRINTLEVEL > 1
    char szTmp[256];
    sprintf(szTmp, "Converting block #%d to final AST before closing:", u32(pCurrentBlock - tBlocks));
    platform_log_info(szTmp, true);
#endif
    if (eBlockSpawningState != EBLOCK_SPAWNING_NONE) {
        debug_print_preparse_err("*** Closing block when expecting child");
        Assert_(pCurrentBlock->vecStatements.size());
        LocLib_Error closingError;
        closingError.errCode = PERR_EXPECTED_CHILD_BLOCK_MISSING;
        closingError.uBlockOrLineIfScanErr = pCurrentBlock->uIndexInBlockVec;
        closingError.uStatement = pCurrentBlock->vecStatements.size() - 1;
        if (pCurrentBlock->vecStatements.last().uFlags & IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD) {
            PreStatement* pAsPreStatement = reinterpret_cast<PreStatement*>(pCurrentBlock->vecStatements.last().tNodes);
            closingError.uTokenRef = make_packed_token_ref(pAsPreStatement->pivotToken1);
        } else {
            closingError.uTokenRef = pCurrentBlock->vecStatements.last().tNodes->uPivotalTokenPackedRef;
        }
        parserParams.pSourceFile->vecErrors.append(closingError);
        pCurrentBlock->vecStatements.last().uFlags |= STATEMENT_MISSES_CHILD_BLOCK;
        pCurrentBlock->uPreParsingFlags |= BLOCK_HAS_STATEMENTS_WITH_MISSING_CHILD;
    }
    if (parserParams.parserState.inFlightModifierNodes) {
        debug_print_preparse_err("*** Closing block with dangling in-flight modifiers");
        PreStatement* pInFlightMissing = _init_pre_statement(parserParams);
        u16 uError = PERR_DANGLING_MODIFIER_ONLY_LINE;
        pInFlightMissing->uRefStartOfError = (u32(pInFlightMissing->iStartLine - parserParams.parserState.iInlinedRootStartLine) << 10) |
            u32(pInFlightMissing->uByteCountOnLineToFirst);
        _convert_and_push_pre_statement_to_current_block(pCurrentBlock, pInFlightMissing, pVecFinalBlocks,
            parserParams.pSourceFile, parserParams.pOsFuncs, arenaForFinalAST, &uError);
    }
    // pan-directives such as #if or #startscope allow their child block to have same indentation as the block-opening statement.
    if (pCurrentBlock->uPreParsingFlags & BLOCK_WAS_SPAWNED_AT_SAME_INDENT_AS_PARENT) {
        // ... in which case, if we're not explicitely informed that we were being closed by a pan directive, this is an error: we need an explicit one...
        if (!bClosedByPanDirective) {
            debug_print_preparse_err("*** Closing block from indentation, when expecting #endif or #endscope");
            pCurrentBlock->uPreParsingFlags |= BLOCK_IS_MISSING_CLOSING_PAN_DIRECTIVE;
            LocLib_Error closingError;
            closingError.errCode = PERR_MISSING_BLOCK_ENDING;
            if (pCurrentBlock->vecStatements.size()) {
                closingError.uBlockOrLineIfScanErr = pCurrentBlock->uIndexInBlockVec;
                closingError.uStatement = pCurrentBlock->vecStatements.size() - 1u;
                if (pCurrentBlock->vecStatements.last().uFlags & IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD) {
                    PreStatement* pAsPreStatement = reinterpret_cast<PreStatement*>(pCurrentBlock->vecStatements.last().tNodes);
                    closingError.uTokenRef = make_packed_token_ref(pAsPreStatement->pivotToken1);
                } else {
                    closingError.uTokenRef = pCurrentBlock->vecStatements.last().tNodes->uPivotalTokenPackedRef;
                }
            } else {
                Assert_(pCurrentBlock > tBlocks);
                closingError.uBlockOrLineIfScanErr = (pCurrentBlock-1)->uIndexInBlockVec;
                Assert_((pCurrentBlock-1)->vecStatements.size());
                closingError.uStatement = (pCurrentBlock-1)->vecStatements.size() - 1u;
                if ((pCurrentBlock-1)->vecStatements.last().uFlags & IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD) {
                    PreStatement* pAsPreStatement = reinterpret_cast<PreStatement*>((pCurrentBlock-1)->vecStatements.last().tNodes);
                    closingError.uTokenRef = make_packed_token_ref(pAsPreStatement->pivotToken1);
                } else {
                    closingError.uTokenRef = (pCurrentBlock-1)->vecStatements.last().tNodes->uPivotalTokenPackedRef;
                }
            }
            parserParams.pSourceFile->vecErrors.append(closingError);
        }
    }
    AstBlock* pBlock = _convert_to_ast_block(pCurrentBlock, tBlocks, pVecFinalBlocks->_alloc.arena, arenaForFinalAST);
    u32 bClosedBlockInError = pCurrentBlock->uPreParsingFlags & IN_ERROR_BLOCK_MASK;
    u32 bClosedBlockInIndentError = pCurrentBlock->uPreParsingFlags & BLOCK_IS_IN_INDENTATION_ERROR;
    u32 uParentStatement = pCurrentBlock->uIndexOfParentStatementInParentBlock;
    Assert_(pCurrentBlock > tBlocks);
    reset_arena_to(pCurrentBlock->allocRefBefore, pCurrentBlock->vecStatements._alloc.arena);
    u32 uIndexInBlockVec = pCurrentBlock->uIndexInBlockVec;
    (*pVecFinalBlocks)[uIndexInBlockVec] = pBlock;
    pCurrentBlock--;
    pCurrentBlock->vecChildBlockIndices.append(uIndexInBlockVec);
    if (bClosedBlockInError) {
        pCurrentBlock->uPreParsingFlags |= BLOCK_HAS_CHILDREN_BLOCKS_IN_ERROR;
        if (!bClosedBlockInIndentError) {
            Assert_(uParentStatement == pCurrentBlock->vecStatements.size() - 1);
            pCurrentBlock->vecStatements[uParentStatement].uFlags |= STATEMENT_CHILD_IN_ERROR;
        }
    } else {
        if (!bClosedBlockInIndentError) {
            Assert_(uParentStatement == pCurrentBlock->vecStatements.size() - 1);
        }
    }
    Assert_(pCurrentBlock->uBlockTabIndent < 256u);
    parserParams.parserState.uCurrentBlockTabIndents = u8(pCurrentBlock->uBlockTabIndent);
    if (pCurrentBlock->uPreParsingFlags & BLOCK_WAS_SPANWED_AS_INLINED) // also recursively close inlined blocks...
        pCurrentBlock = _convert_and_close_block(tBlocks, pVecFinalBlocks, pCurrentBlock, parserParams, EBLOCK_SPAWNING_NONE, arenaForFinalAST);
    return pCurrentBlock;
}



#endif // LOCLIB_POSTPARSER_H_

