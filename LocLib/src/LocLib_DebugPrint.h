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

#ifndef LOCLIB_DEBUG_PRINT_H_
#define LOCLIB_DEBUG_PRINT_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_ScanAndTok.h"
#include "LocLib_PreParserTypes.h"

//
//
// This file is about a legacy print of parsed nodes while working on the parser quite some time ago... to be removed short term in favor of new traces
// 
//

// TODO: remove dependency to crt
#include <stdio.h>

local_func void debug_print_indent(int iIndent)
{
    for (int i = 0; i < iIndent; i++)
        platform_log_info("  ", false);
}
local_func void debug_print_token(const Token* pToken, const u8* pLineStart, SourceFileDescAndState* pSourceFile)
{
	char szTmp[256];
	const char* pName = "";
	switch (u8(pToken->uTokenPayloadAndKind) & TOKEN_CATEGORY_MASK) {
		case TOKEN_CATEGORY_SYMBOL:
            sprintf(szTmp, "[%s] '%s'", (u8(pToken->uTokenPayloadAndKind) & 1) ? "SYMB" : "MSYM",
                tStandardPayloadsStr[u8(pToken->uTokenPayloadAndKind >> 8)]);
            break;

		case TOKEN_CATEGORY_KEYWORD:
            sprintf(szTmp, "[KEYW] '%s'",
                tStandardPayloadsStr[u8(pToken->uTokenPayloadAndKind >> 8)]);
            break;

        case TOKEN_CATEGORY_IDENTIFIER:
            sprintf(szTmp, "[IDEN] '%s'",
                (const char*)get_identifier_stringview_when_parsing(pSourceFile, pToken->uTokenPayloadAndKind >> 8).begin());
            break;

        case TOKEN_CATEGORY_NATNUM:
            if (pToken->uTokenPayloadAndKind & TOKENKINDFLAG_EXTREME_NUMERAL)
                sprintf(szTmp, "[NATN] (large)");
            else
                sprintf(szTmp, "[NATN] = %u", pToken->uTokenPayloadAndKind >> 8);
            break;

        case TOKEN_CATEGORY_FLOATINGPT:
            if (pToken->uTokenPayloadAndKind & TOKENKINDFLAG_EXTREME_NUMERAL)
                sprintf(szTmp, "[FLTN] (extreme)");
            else
                sprintf(szTmp, "[FLTN] (standard)");
            break;

		case TOKEN_CATEGORY_STRING:
            sprintf(szTmp, "[%s]", (u8(pToken->uTokenPayloadAndKind) & PAN_BEFORE) ? "CDPT" : "STRG");
            break;

        default:
            sprintf(szTmp, "???");
	}
	//char cSpace = '-';
	//if (pToken->uTokenPayloadAndKind & TOKENKINDFLAG_SPACE_AFTERWARDS)
	//	cSpace = ' ';
	platform_log_info(szTmp, false);
}

local_func void debug_print_ast_node(PreAstNode* pNode, u32 uLevel, SourceFileDescAndState* pSourceFile)
{
    debug_print_indent(uLevel);
    if (!pNode) {
        platform_log_info("> <null>", true);
        return;
    }

    char szTmp[256];
    if (pNode->iCountSubNodesOrNegErr < 0) {
        sprintf(szTmp, "#inErr(%d)#", -pNode->iCountSubNodesOrNegErr);
    } else {
        sprintf(szTmp, "<%d subs> ", pNode->iCountSubNodesOrNegErr);
    }
    platform_log_info(szTmp, false);
    bool bShownPrimary = false;
    bool bShownSecondary = false;
    switch (pNode->uNodeKind) {
        case ENODE_ATOMICEXPR_NATURAL_NUMBER_LITERAL:
            if (pNode->uNodeFlags & EPRENODEFLAG_LITERAL_IS_EMBEDDED64) {
                sprintf(szTmp, "> NatLit = %llu (0x%llx)", pNode->primaryPayload, pNode->primaryPayload);
            } else
                sprintf(szTmp, "> NatLit (large), stored at %llx <representation not implemented>", pNode->primaryPayload);
            platform_log_info(szTmp, true);
            bShownPrimary = true;
            break;
        case ENODE_ATOMICEXPR_FLOATING_POINT_LITERAL:
            if (pNode->uNodeFlags & EPRENODEFLAG_LITERAL_IS_EMBEDDED64) {
                sprintf(szTmp, "> FltLit = %f", type_pun_to_double(pNode->primaryPayload));
            } else
                sprintf(szTmp, "> FltLit (large), stored at %llx <representation not implemented>", pNode->primaryPayload);
            platform_log_info(szTmp, true);
            bShownPrimary = true;
            break;
        case ENODE_ATOMICEXPR_STRING_LITERAL:
            if (pNode->uNodeFlags & EPRENODEFLAG_LITERAL_IS_EMBEDDED64) {
                char szTmp2[9];
                for (int i = 0; i < 8; i++) {
                    szTmp2[i] = char(pNode->primaryPayload >> (i*8));
                }
                szTmp2[8] = '\0';
                sprintf(szTmp, "> StrLit = \"%s\"", szTmp2);
            } else
                sprintf(szTmp, "> StrLit (large), stored at %llx = \"%s\"", pNode->primaryPayload, (char*)(pNode->primaryPayload));
            platform_log_info(szTmp, true);
            bShownPrimary = true;
            break;

        case ENODE_ATOMICEXPR_CODEPOINT_LITERAL:
            sprintf(szTmp, "> StrCPt = U%x", u32(pNode->primaryPayload));
            platform_log_info(szTmp, true);
            bShownPrimary = true;
            break;

        case ENODE_ATOMICEXPR_IDENTIFIER:
            if (pNode->primaryPayload >= pSourceFile->uIdentifierNamesOffset)
                sprintf(szTmp, "> Identifier = %s", pSourceFile->locallyFoundIdentifierNamesByOffsetId[u32(pNode->primaryPayload) - pSourceFile->uIdentifierNamesOffset].c_str());
            else
                sprintf(szTmp, "> Identifier = %s", pSourceFile->programState->vecAllIdentifierNamesById[u32(pNode->primaryPayload)].c_str());
            platform_log_info(szTmp, true);
            bShownPrimary = true;
            break;

        case ENODE_EXPRLIST_NODE:
            if (pNode->secondaryChildNode)
                sprintf(szTmp, "> In-List : ");
            else
                sprintf(szTmp, "> In-List (last) : ");
            if (!pNode->primaryChildNode)
                sprintf(szTmp + strlen(szTmp), "<null>");
            platform_log_info(szTmp, true);
            if (pNode->primaryChildNode)
                debug_print_ast_node(pNode->primaryChildNode, uLevel+1, pSourceFile);
            bShownPrimary = true;
            if (pNode->secondaryChildNode) { // print sibling nodes at same level
                debug_print_ast_node(pNode->secondaryChildNode, uLevel, pSourceFile);
                bShownSecondary = true;
            }
            break;

        default: {
            if (pNode->uNodeKind < COUNT_NODE_KINDS) {
                sprintf(szTmp, "> %s", tNodeKindsDbgStr[pNode->uNodeKind]);
                ETokenKind eKind = ETokenKind(u8(pNode->pivotalToken.token.uTokenPayloadAndKind) & TOKEN_KIND_MASK);
                if (is_token_symbol_or_keyword(eKind))
                    sprintf(szTmp + strlen(szTmp), " '%s'", tStandardPayloadsStr[u8(pNode->pivotalToken.token.uTokenPayloadAndKind >> 8)]);
                if (pNode->primaryChildNode || pNode->secondaryChildNode)
                    sprintf(szTmp + strlen(szTmp), " : ");
                platform_log_info(szTmp, true);
                if (pNode->primaryChildNode) {
                    debug_print_ast_node(pNode->primaryChildNode, uLevel+1, pSourceFile);
                    bShownPrimary = true;
                }
                if (pNode->secondaryChildNode && pNode->uNodeKind != ENODE_EXPR_PARENTISED) { // secondary of parentized is a secondary 'token'
                    debug_print_indent(uLevel);
                    platform_log_info("--secondary:", true);
                    debug_print_ast_node(pNode->secondaryChildNode, uLevel+1, pSourceFile);
                    bShownSecondary = true;
                }

            } else if (pNode->uNodeKind == EPRENODE_MODIFIER_NODE) {
                sprintf(szTmp, "> MODIFIER-NODE");
                platform_log_info(szTmp, true);

            } else {
                sprintf(szTmp, "> ERR-NODE %u", pNode->uNodeKind - COUNT_NODE_KINDS);
                platform_log_info(szTmp, true);
            }

        }   break;
    }
/*
    if (!bShownPrimary && pNode->primaryChildNode) {
        debug_print_indent(uLevel);
        platform_log_info("-#-forced shown secondary:", true);
        debug_print_ast_node(pNode->secondaryChildNode, uLevel+1, pSourceFile);
    }
    if (!bShownSecondary && pNode->secondaryChildNode) {
        debug_print_indent(uLevel);
        platform_log_info("-#-forced shown secondary:", true);
        debug_print_ast_node(pNode->secondaryChildNode, uLevel+1, pSourceFile);
    }
*/
}

local_func void debug_print_pre_statement(PreStatement* preStatement, SourceFileDescAndState* pSourceFile)
{
    platform_log_info("Showing Resulting Pre-AST:", true);
    char szTmp[256];
    platform_log_info("Statement : ", false);
    if (preStatement->uStatementFlags & ESTATEMENTFLAGS_COMPTIME)
        platform_log_info("[comptime]-", false);
    platform_log_info(tStatementsDbgStr[preStatement->uStatementKind], true);
    switch (preStatement->uStatementKind) {
    case ESTATEMENT_CONST_DECLARATION:
        platform_log_info("  Declared =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        platform_log_info("  Value =", true);
        debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        break;
    case ESTATEMENT_VAR_DECLARATION:
        platform_log_info("  Declared =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        platform_log_info("  TypeAndOrValue =", true);
        debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        break;
    case ESTATEMENT_ASSIGNMENT:
        platform_log_info("  Assigned-To =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        platform_log_info("  Value =", true);
        debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        break;
    case ESTATEMENT_OP_AND_ASSIGNMENT:
        sprintf(szTmp, "> Op = %s :", tStandardPayloadsStr[u8(preStatement->pivotToken1.token.uTokenPayloadAndKind >> 8)]);
        platform_log_info(szTmp, true);
        platform_log_info("  Assigned-To =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        platform_log_info("  Value =", true);
        debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        break;
    case ESTATEMENT_SINGLE_EXPRESSION:
        platform_log_info("ESTATEMENT_SINGLE_EXPRESSION", true);
        platform_log_info("  Expression =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_IF:
        platform_log_info("  Condition =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_ELIF:
        platform_log_info("  Condition =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_ELSE:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_FOR:
        platform_log_info("  Iterable =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        platform_log_info("  [Opt]Iterator =", true);
        debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        break;
    case ESTATEMENT_WHILE:
        platform_log_info("  Condition =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_LOOP_FINALIZER:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_CASE:
        platform_log_info("  EvalExpression =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;

    case ESTATEMENT_USING:
        if (preStatement->uStatementFlags & ESTATEMENTFLAGS_COMPTIME)
            platform_log_info("comptime-", false);
        platform_log_info("ESTATEMENT_USING", true);
        platform_log_info("  Namespace =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;

    case ESTATEMENT_PAN_IF:
        platform_log_info("  Condition =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_PAN_ELIF:
        platform_log_info("  Condition =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_PAN_ELSE:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_PAN_ENDIF:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_PAN_NAMESPACE:
        platform_log_info("  Name =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_PAN_ENDNAMESPACE:
    case ESTATEMENT_PAN_PRIVATE:
    case ESTATEMENT_PAN_ENDPRIVATE:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;
    case ESTATEMENT_LABEL:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;

    case ESTATEMENT_MODIFIER_ONLY_LINE:
        if (preStatement->pMainNode) {
            platform_log_info("  Forced Shown Primary =", true);
            debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        }
        if (preStatement->pSecondaryNode) {
            platform_log_info("  Forced Shown Secondary =", true);
            debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        }
        break;

    default:
        platform_log_info("  Main? =", true);
        debug_print_ast_node(preStatement->pMainNode, 2, pSourceFile);
        platform_log_info("  Secondary? =", true);
        debug_print_ast_node(preStatement->pSecondaryNode, 2, pSourceFile);
        break;
    }
}


#endif // LOCLIB_DEBUG_PRINT_H_

