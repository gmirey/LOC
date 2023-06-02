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

#ifndef LOCLIB_TC_INVOC_LIKE_H_
#define LOCLIB_TC_INVOC_LIKE_H_


#include "LocLib_TypeCheckerBase.h"
#include "LocLib_TC_Casts.h"


local_func ETCResult typecheck_param_list(u32 uStartingNodeIndex, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, TmpTCNode* tAllExpr, u8* ioNodeCount,
    const TypeInfo_ProcLike* pOptSign)
{
    // TODO: allow var-and-init node kind here
    // TODO: possibly allow last node in chain with null (INVALID_NODE_INDEX)

    u32 uCurrentNodeIndex = uStartingNodeIndex;
    TCNode* pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];

    while (u8(pCurrentNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPRLIST_NODE) {
        u32 uNextNodeIndex = pCurrentNode->ast.uSecondaryChildNodeIndex;

        u32 uChildNodeIndex = pCurrentNode->ast.uPrimaryChildNodeIndex;
        Assert_(uChildNodeIndex != INVALID_NODE_INDEX);
        TmpTCNode* pChild = tAllExpr + (*ioNodeCount);
        *pChild = {};
        pChild->uNodeIndexInStatement = uChildNodeIndex;
        pChild->pTCNode = pTCStatement->vecNodes[uChildNodeIndex];
        if (*ioNodeCount >= 30u) {
            return_error(pChild, pTCStatement, pTCContext, CERR_TOO_MANY_EXPR_NODES,
                "typecheck_possible_expr_list() : too many expr nodes in expr list");
        }
        UpwardsInference inferred = {};
        if (pOptSign && (*ioNodeCount) < get_input_param_count(pOptSign))
            inferred.pIfType = pOptSign->params[*ioNodeCount].pBinding->pType;
        *ioNodeCount += 1;

        Assert_(u8(pChild->pTCNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);
        ETCResult checkChild = typecheck_expression(pChild, pTCStatement, pTCContext, eExpectation, inferred);
        if (checkChild == ETCResult::ETCR_SUCCESS) {
            // NOOP
        } else
            return checkChild;

        if (uNextNodeIndex != INVALID_NODE_INDEX) {
            uCurrentNodeIndex = uNextNodeIndex;
            pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
        }
    }

    Assert_(pCurrentNode);
    Assert_(u8(pCurrentNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);
    TmpTCNode* pLast = tAllExpr + (*ioNodeCount);
    *pLast = {};
    pLast->uNodeIndexInStatement = uCurrentNodeIndex;
    pLast->pTCNode = pTCStatement->vecNodes[uCurrentNodeIndex];

    UpwardsInference inferred = {};
    if (pOptSign && (*ioNodeCount) < get_input_param_count(pOptSign))
        inferred.pIfType = pOptSign->params[*ioNodeCount].pBinding->pType;
    *ioNodeCount += 1;

    ETCResult checkLast = typecheck_expression(pLast, pTCStatement, pTCContext, eExpectation, inferred);
    return checkLast;
}

local_func void _alloc_additional_node_for(TmpTCNode* pAdditionalNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    u32 uNewIndex = pTCStatement->vecNodes.size();
    TCNode* pNewNode = (TCNode*)alloc_from(pTCContext->pIsolatedSourceFile->localArena, sizeof(TCNode), alignof(TCNode));
    *pNewNode = {};
    pTCStatement->vecNodes.append(pNewNode);
    *pAdditionalNode = {};
    pAdditionalNode->pTCNode = pNewNode;
    pAdditionalNode->uNodeIndexInStatement = uNewIndex;
}

typedef ETCResult TCBuiltinSign(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion);

local_func ETCResult tc_foreign_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    if (LIKELY(uInParamsCount == 2)) {
        if (tAllInParams[0].pIntrinsicValue->pType != g_pCoreTypesInfo[ECORETYPE_FOREIGN_SOURCE]) {
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_foreign_builtin() : first param (foreign source) must be of the 'foreign-source' comptime-type. Typically used with a constant binding to a #foreign_source builtin");
        }
        if (get_type_kind(tAllInParams[1].pIntrinsicValue->pType) != ETypeKind::ETYPEKIND_OTHERCORE ||
                0u == (tAllInParams[1].pIntrinsicValue->pType->_coreFlags & OTHERCOREFLAG_IS_STRING)) {
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_foreign_builtin() : second param (symbol name in object file) must be of a string type");
        }
        Assert_(is_value_tc_only(tAllInParams[0].pIntrinsicValue));
        if (!is_value_tc_const(tAllInParams[1].pIntrinsicValue)) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_FULLY_KNOWN_CONSTANT,
                "tc_foreign_builtin() : requires all arguments to be fully solvable at compile-time");
        }
        u64 uForeignSourceRegistration = tAllInParams[0].pIntrinsicValue->info.metaValue.knownValue._payload; // TODO: use it...
        Assert_(uForeignSourceRegistration); // '0' would mean 'no foreign' in subsequent proc result...

        if (eRetCount != EInvocFormResultCount::EINVOC_NO_RETURN) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_EXPRESSION,
                "tc_foreign_builtin() : expecting an expression, cannot call a builtin with no return values");
        }
        if (0 == pTCContext->pProcResult) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_SEQ_STATEMENT_WHEN_EXPECTING_CTEVAL_STATEMENT,
                "tc_foreign_builtin() : cannot be invoked outside of a proc body");
        }
        if (pTCContext->pProcResult->uIsForeignSource) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_MULTIPLE_FOREIGN_SPECIFICATIONS_ON_PROC,
                "tc_foreign_builtin() : proc already has a 'foreign' specification");
        }

        pTCContext->pProcResult->uIsForeignSource = uForeignSourceRegistration;
        const TypeInfo_OtherCore* pStringType = (const TypeInfo_OtherCore*)tAllInParams[1].pIntrinsicValue->pType;
        if (pStringType->_coreFlags & STRINGFLAG_IS_COMPACT) {
            pTCContext->pProcResult->foreignSymbolName = get_comptime_ffstring_from_compact_semantic_const_instance(tAllInParams[1].pIntrinsicValue->info, pStringType, pTCContext);
            if (pTCContext->pProcResult->foreignSymbolName.byte_length() == 0) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_INVALID_FOREIGN_SPECIFICATION,
                    "tc_foreign_builtin() : symbol name cannot be empty");
            }
            bool bOnlyForeignSpec = true;
            u32 uInstrCount = pTCContext->pProcResult->procwiseRepo.uSize;
            for (u32 uInstr = pTCContext->pProcResult->procSign->params.size(); uInstr < uInstrCount; uInstr++) {
                IREntry& entryThere = ir_access_repo_instr(&(pTCContext->pProcResult->procwiseRepo), uInstr);
                u8 uInstructionKind = u8(entryThere.uInstrCodeAndFormatAndFirstParam);
                if (tIRITFormatSlot[uInstructionKind] != IR_INSTR_NOVALUE) {
                    if (0u == (entryThere.uInstrMetaFlagsAndSecondParam & IRFLAG_IS_KNOWN) ||
                              (entryThere.uInstrMetaFlagsAndSecondParam & IRFLAG_HAS_LOCAL_NYKA)) { // string for 'foreign' itself adds declarations
                        bOnlyForeignSpec = false;
                        break;
                    }
                } else {
                    if (uInstructionKind != IRIT_MARKER_JUMP_TARGET &&
                        uInstructionKind != IRIT_MARKER_START_SOURCE_SCOPE &&
                        uInstructionKind != IRIT_MARKER_END_SOURCE_SCOPE) { // We need to check here *without* the last 'return'
                            bOnlyForeignSpec = false;
                            break;
                    }
                }
            }
            if (!bOnlyForeignSpec) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_FOREIGN_SPECIFICATION_SHALL_BE_ALONE,
                    "tc_foreign_builtin() : found runtime ops at the time of 'foreign'");
            }
        } else {
            // TODO...
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "tc_foreign_builtin() : not yet implemented for non-compact strings on symbol name");
        }
        return set_node_typecheck_notanexpr_success(pExpr->pTCNode);

    } else {
        if (uInParamsCount < 2) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_FEW_PROC_PARAMETERS,
                "tc_foreign_builtin() : too few parameters - requires two : foreign source, symbol name (in object file)");
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_MANY_PROC_PARAMETERS,
                "tc_foreign_builtin() : too many parameters - requires two : foreign source, symbol name (in object file)");
        }
    }
}

local_func ETCResult tc_foreign_source_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    if (LIKELY(uInParamsCount >= 1)) {
        if (LIKELY(uInParamsCount <= 2)) {
            if (get_type_kind(tAllInParams[0].pIntrinsicValue->pType) != ETypeKind::ETYPEKIND_OTHERCORE ||
                    0u == (tAllInParams[0].pIntrinsicValue->pType->_coreFlags & OTHERCOREFLAG_IS_STRING)) {
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "tc_foreign_source_builtin() : first param (static lib name) must be of a string type");
            }
            if (!is_value_tc_const(tAllInParams[0].pIntrinsicValue)) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_FULLY_KNOWN_CONSTANT,
                    "tc_foreign_source_builtin() : requires all arguments to be fully solvable at compile-time");
            }
            u64 uRegistration;
            if (uInParamsCount == 1) {
                if (pTCContext->eKind > ETypecheckContextKind::ECTXKIND_GLOBAL_PRIVATE) {
                    // TODO: relax this ??
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_GLOBAL_SCOPE,
                        "tc_foreign_source_builtin() : cannot be invoked outside of a global scope");
                }
                if (eRetCount == EInvocFormResultCount::EINVOC_NO_RETURN) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPRESSION_WHEN_EXPECTING_CTEVAL_STATEMENT,
                        "tc_foreign_source_builtin() : expecting a statement, cannot call a builtin with an expression-form");
                }
                const TypeInfo_OtherCore* pStringTypeFirstArg = (const TypeInfo_OtherCore*)tAllInParams[0].pIntrinsicValue->pType;
                if (pStringTypeFirstArg->_coreFlags & STRINGFLAG_IS_COMPACT) {
                    FFString asFF = get_comptime_ffstring_from_compact_semantic_const_instance(tAllInParams[0].pIntrinsicValue->info, pStringTypeFirstArg, pTCContext);
                    ETCResult checkRegistration = register_foreign_source(&uRegistration, asFF, pExpr, pTCStatement, pTCContext);
                    success_or_return(checkRegistration);
                } else {
                    // TODO...
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_foreign_source_builtin() : not yet implemented with non-compact string on first arg");
                }

            } else {
                if (get_type_kind(tAllInParams[1].pIntrinsicValue->pType) != ETypeKind::ETYPEKIND_OTHERCORE ||
                        0u == (tAllInParams[1].pIntrinsicValue->pType->_coreFlags & OTHERCOREFLAG_IS_STRING)) {
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_foreign_source_builtin() : second param (dynamic lib name) must be of a string type");
                }
                if (!is_value_tc_const(tAllInParams[1].pIntrinsicValue)) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_FULLY_KNOWN_CONSTANT,
                        "tc_foreign_source_builtin() : requires all argument as fully solvable at compile-time");
                }
                const TypeInfo_OtherCore* pStringTypeFirstArg = (const TypeInfo_OtherCore*)tAllInParams[0].pIntrinsicValue->pType;
                const TypeInfo_OtherCore* pStringTypeSecondArg = (const TypeInfo_OtherCore*)tAllInParams[1].pIntrinsicValue->pType;
                if ((pStringTypeFirstArg->_coreFlags & STRINGFLAG_IS_COMPACT) && (pStringTypeSecondArg->_coreFlags & STRINGFLAG_IS_COMPACT)) {
                    FFString asFF0 = get_comptime_ffstring_from_compact_semantic_const_instance(tAllInParams[0].pIntrinsicValue->info, pStringTypeFirstArg, pTCContext);
                    FFString asFF1 = get_comptime_ffstring_from_compact_semantic_const_instance(tAllInParams[1].pIntrinsicValue->info, pStringTypeSecondArg, pTCContext);
                    ETCResult checkRegistration = register_foreign_source(&uRegistration, asFF0, asFF1, pExpr, pTCStatement, pTCContext);
                    success_or_return(checkRegistration);
                } else {
                    // TODO...
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "tc_foreign_source_builtin() : not yet implemented with non-compact string on first or second arg");
                }
            }

            Assert(uRegistration, "register_foreign_source should have returned a non-zero code or their failure should have been detected already");

            NodeValue* pResult = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pResult->pType = g_pCoreTypesInfo[ECORETYPE_FOREIGN_SOURCE];
            pResult->info.uIRandMetaFlags = IRFLAG_TC_ONLY;
            pResult->info.metaValue.knownValue._payload = uRegistration;
            return set_node_typecheck_expr_success(pExpr->pTCNode);

        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_MANY_PROC_PARAMETERS,
                "tc_tmp_print_builtin() : too many parameters - requires at most two : static_lib_name (may be empty), dynamic_lib_name");
        }
    } else {
        return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_FEW_PROC_PARAMETERS,
            "tc_foreign_source_builtin() : too few parameters - requires at least one : static_lib_name");
    }
}

local_func ETCResult tc_memcpy_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_memcpy_builtin() : not yet implemented");
}

local_func ETCResult tc_rev_memcpy_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_rev_memcpy_builtin() : not yet implemented");
}

local_func ETCResult tc_zeromem_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_zeromem_builtin() : not yet implemented");
}

local_func ETCResult tc_memset_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_memset_builtin() : not yet implemented");
}

local_func ETCResult tc_memcmp_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_memcmp_builtin() : not yet implemented");
}

local_func ETCResult tc_rdtsc_builtin(TmpTCNode* pExpr, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    // TODO
    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
        "tc_rdtsc_builtin() : not yet implemented");
}

TCBuiltinSign* t_allBuiltins[] = {
    &tc_foreign_builtin,        //EBUILTIN_FOREIGN,
    &tc_foreign_source_builtin, //EBUILTIN_FOREIGN_SOURCE,

    &tc_memcpy_builtin,         //EBUILTIN_MEMCPY,
    &tc_rev_memcpy_builtin,     //EBUILTIN_REV_MEMCPY,
    &tc_zeromem_builtin,        //EBUILTIN_ZEROMEM,
    &tc_memset_builtin,         //EBUILTIN_MEMSET,
    &tc_memcmp_builtin,         //EBUILTIN_MEMCMP,
    &tc_rdtsc_builtin,          //EBUILTIN_RDTSC, 
};
static_assert(COUNT_BUILTINS == sizeof(t_allBuiltins) / sizeof(TCBuiltinSign*), "t_allBuiltins and COUNT_BUILTINS mismatch");

local_func ETCResult do_special_cast_as_builtin(TmpTCNode* pExpr, ECastKind eCastKind, const TypeInfo* pCastDestType, TmpTCNode* pBuiltin, TmpTCNode* tAllInParams, u8 uInParamsCount,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    if (LIKELY(uInParamsCount == 1)) {
        if (eRetCount == EInvocFormResultCount::EINVOC_NO_RETURN) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPRESSION_WHEN_EXPECTING_STATEMENT,
                "do_special_cast_as_builtin() : expecting a statement, cannot call a builtin returning a result");
        }
        Assert_(is_node_already_typechecked(tAllInParams[0].pTCNode));
        Assert_(tAllInParams[0].pIntrinsicValue && tAllInParams[0].pIntrinsicValue->pType);
        return do_explicit_cast(&(tAllInParams[0]), pCastDestType, eCastKind, pExpr, pTCStatement, pTCContext, eExpectation);
    } else {
        if (uInParamsCount == 0) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_FEW_PROC_PARAMETERS,
                "do_special_cast_as_builtin() : too few parameters - requires one");
        } else {
            return_error(pExpr, pTCStatement, pTCContext, CERR_TOO_MANY_PROC_PARAMETERS,
                "do_special_cast_as_builtin() : too many parameters - requires one");
        }
    }
}

// invocation forms are really special in that they can be macros, able to transform the ast itself
//   if not macros, they can also be type-casts.
//   if really proc-like, they can have from 0 to many return values... and they can be inlined or comptime-evaluated...
//   in many cases, proc-like identifiers may also be common aliases to parapoly or overloads...
// you got it, they're a mess.
local_func ETCResult typecheck_invocation_form(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow,
    u32 uIndexOfOptNodeChainParent, TmpTCNode* tAllExpr, u8* ioNodeCount, EInvocFormResultCount eRetCount, bool* oWasMacroExpansion)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking an invocation-form, expecting %s, %s",
        reinterpret_cast<u64>(tExpectedExprStr[eExpectation]), reinterpret_cast<u64>(tInvocFormResultCountStr[eRetCount])), pTCContext->pWorker);

    *oWasMacroExpansion = false;
    Assert_(eRetCount != EInvocFormResultCount::EINVOC_RETURNS_ONE_OR_MANY || tAllExpr != 0);
    Assert_(eRetCount != EInvocFormResultCount::EINVOC_RETURNS_ONE_OR_MANY || ioNodeCount != 0);
    //Assert_(eRetCount == EInvocFormResultCount::EINVOC_RETURNS_ONE || !is_bool_transform(inferredFromBelow));

    if_expr_already_typechecked_phase1_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    if (is_comptime_prefixed(&(pExpr->pTCNode->ast))) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_COMPTIME_PREFIX_THIS_NODE_KIND,
            "typecheck_invocation_form() : comptime-prefix on invoc-forms not allowed (could be on identifier of the invoc instead)");
    }

    TmpTCNode invocator = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    TmpTCNode parensWrapper = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
    
    if (is_comptime_prefixed(&(invocator.pTCNode->ast))) {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_invocation_form() : comptime-prefix on invocator not yet implemented");
    }
    if (is_comptime_prefixed(&(parensWrapper.pTCNode->ast))) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_CANNOT_COMPTIME_PREFIX_THIS_NODE_KIND,
            "typecheck_invocation_form() : comptime-prefix on parens of invoc-forms not allowed (could be on identifier of the invoc instead)");
    }

    ETCResult checkInvocator = typecheck_expression(&invocator, pTCStatement, pTCContext, eExpectation, UpwardsInference{});
    success_or_return_wait_or_error(checkInvocator, pExpr->pTCNode);
    Assert_(is_node_already_typechecked(invocator.pTCNode));
    ETypeKind eInvocatorKind;
    const TypeInfo* pUnaliasedInvocType = unalias_ext(invocator.pIntrinsicValue->pType, &eInvocatorKind);

    switch (eInvocatorKind) {
        case ETypeKind::ETYPEKIND_PROCLIKEBODY: {
            const TypeInfo_ProcLike* pProcLikeType = (const TypeInfo_ProcLike*)pUnaliasedInvocType;
            u8 uBiasedProcKind = get_proc_kind(pProcLikeType) + ETOK_PROC;
            switch (uBiasedProcKind) {
                case ETOK_PROC:
                case ETOK_FUNC:
                case ETOK_NOCTXFUNC:
                case ETOK_NOCTXPROC:
                case ETOK_PURE:
                {
                    if (eExpectation >= EExpectedExpr::EXPECT_ASSIGNABLE) {
                        return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                            "typecheck_invocation_form() : regular invocation-forms cannot be assignable");
                    }

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking invocation of a proclike"), pTCContext->pWorker);

                    u8 uExpectedInParamsCount = get_input_param_count(pProcLikeType);
                    // TODO: handling params with default
                    TmpTCNode tAllInParams[32];
                    u8 uInParamsCount = 0;
                    if (parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                        ETCResult checkParams = typecheck_param_list(parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex,
                            pTCStatement, pTCContext, eExpectation, tAllInParams, &uInParamsCount, pProcLikeType);
                        success_or_return_wait_or_error(checkParams, pExpr->pTCNode);
                        Assert_(uInParamsCount); // otherwise checkParams should have complained
                    }
                    if (uInParamsCount == uExpectedInParamsCount) {
                        u8 uOutParamsCount = get_output_param_count(pProcLikeType);
                        if (uOutParamsCount) {
                            if (eRetCount == EInvocFormResultCount::EINVOC_NO_RETURN) {
                                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPRESSION_WHEN_EXPECTING_STATEMENT,
                                    "typecheck_invocation_form() : cannot call a proclike with return results as a statement. Use explicit assignment");
                            } else if (EInvocFormResultCount::EINVOC_RETURNS_ONE && uOutParamsCount > 1u) {
                                return_error(pExpr, pTCStatement, pTCContext, CERR_MULTIPARAM_INVOCATION_WHEN_EXPECTED_SINGLE_EXPRESSION,
                                    "typecheck_invocation_form() : expecting single expression, found proclike call with multiple results.");
                            }
                        } else {
                            if (eRetCount != EInvocFormResultCount::EINVOC_NO_RETURN) {
                                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_EXPRESSION,
                                    "typecheck_invocation_form() : expecting an expression, cannot call a proclike with no return values");
                            }
                            if (pTCContext->pProcResult == 0) { // necessarily non-'const', since it has no result (even if a noop... cannot do)
                                return_error(pExpr, pTCStatement, pTCContext, CERR_SEQ_STATEMENT_WHEN_EXPECTING_NON_SEQ_STATEMENT,
                                    "typecheck_invocation_form() : cannot invoc a no-valued procedure outside of a sequential procbody");
                            }
                        }

                        if (pTCContext->pProcResult == 0) {
                            // TODO: check if EExpectedExpr is const ? or something, for comptime-evals of functions.
                            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                "typecheck_invocation_form() : cannot invoc a runtime function outside of a sequential procbody, and comptime evaluations not yet implemented");
                        }

                        const TmpArray<ProcLikeParam>& vecParams = pProcLikeType->params;
                        for (u8 uInParam = 0; uInParam < uInParamsCount; uInParam++) {
                            TmpTCNode* pParam = tAllInParams + uInParam;
                            // TODO: CLEANUP: maybe at times here we'd like to reify as referencable, if we ever do implict by-ref ?
                            ETCResult checkCast = do_implicit_cast(pParam, vecParams[uInParam].pBinding->pType, pTCStatement, pTCContext, eExpectation);
                            success_or_return_wait_or_error(checkCast, pExpr->pTCNode);
                        }

                        Assert_(!is_value_tc_only(invocator.pIntrinsicValue));
                        Assert_(ir_is_valid_param_(invocator.pIntrinsicValue->info.uIRandMetaFlags));
                        u32 uCallPos = ir_emit_proccall(invocator.pIntrinsicValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK, uExpectedInParamsCount, uOutParamsCount,
                            0u, get_proc_kind(pProcLikeType), 0u, pTCContext->pRepo, pTCContext);

                        u32 uLastEmitted = uCallPos;

                        for (u8 uInParam = 0; uInParam < uInParamsCount; uInParam++) {
                            TmpTCNode* pParam = tAllInParams + uInParam;
                            Assert_(is_node_already_type_casted(pParam->pTCNode));
                            Assert_(ir_is_valid_param_(pParam->pFinalValue->info.uIRandMetaFlags));
                            u8 uFormat = get_ir_format(pParam->pFinalValue->pType);
                            u32 uSlotsCount = get_slots_count(pParam->pFinalValue->pType);
                            u32 uAlignLog2 = get_log2_of_align_bytes(pParam->pFinalValue->pType);
                            u32 uPushIn = ir_emit_proc_param_on_caller_side(pParam->pFinalValue->info.uIRandMetaFlags & IR_STD_PARAM_MASK, uFormat,
                                uSlotsCount, uAlignLog2, pTCContext->pRepo, pTCContext);
                            uLastEmitted = uPushIn;
                        }
                        u8 uTotalParamsCount = uExpectedInParamsCount + uOutParamsCount;
                        Assert_(get_total_param_count(pProcLikeType) == uTotalParamsCount);
                        if (uOutParamsCount) {
                            for (u8 uRetParam = uInParamsCount; uRetParam < uTotalParamsCount; uRetParam++) {
                                const TypeInfo* pRetType = vecParams[uRetParam].pBinding->pType;
                                u8 uFormat = get_ir_format(pRetType);
                                u32 uSlotsCount = get_slots_count(pRetType);
                                u32 uAlignLog2 = get_log2_of_align_bytes(pRetType);
                                u32 uRefOut = ir_emit_proc_result_on_caller_side(uCallPos, uFormat,
                                    uSlotsCount, uAlignLog2, pTCContext->pRepo, pTCContext);
                                uLastEmitted = uRefOut;
                                TmpTCNode* pRetParamNode = pExpr;
                                if (uRetParam == uInParamsCount) { // ret param index == in-count => first (or single) ret param
                                    Assert_(tAllExpr == 0 || (tAllExpr + *ioNodeCount - 1) == pExpr);
                                } else { // additional param after the first one.
                                    // we should already have reported an error in case we expect single, and there are many.
                                    Assert_(tAllExpr); // ... and if we expect many, tAllExpr shall not be null.
                                    pRetParamNode = tAllExpr + *ioNodeCount - 1; // -1 since node count was already taking 'pExpr' into account
                                    _alloc_additional_node_for(pRetParamNode, pTCStatement, pTCContext);
                                    *ioNodeCount += 1;
                                }
                                Assert_(!is_node_already_typechecked(pRetParamNode->pTCNode));
                                NodeValue* pRetAsValue = alloc_value_for(pRetParamNode, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC,
                                    pTCStatement, pTCContext);
                                pRetAsValue->pType = pRetType;
                                pRetAsValue->info.uIRandMetaFlags = ir_make_std_code_in_cur_proc(uRefOut);
                                pRetAsValue->info.metaValue._payload = 0uLL;
                                set_node_typecheck_expr_success(pRetParamNode->pTCNode);
                            }
                            pTCStatement->uLastIRorGlobalTCResult = uLastEmitted;
                            if (eRetCount == EInvocFormResultCount::EINVOC_RETURNS_ONE) {
                                return set_node_typecheck_expr_success(pExpr->pTCNode);
                            } else
                                return ETCResult::ETCR_SUCCESS;
                        } else {
                            pTCStatement->uLastIRorGlobalTCResult = uLastEmitted;
                            return set_node_typecheck_notanexpr_success(pExpr->pTCNode);
                        }

                    } else {
                        // TODO: handling params with default
                        return_error(pExpr, pTCStatement, pTCContext, CERR_PROC_INVOC_PARAM_COUNT_MISMATCH,
                            "typecheck_invocation_form() : param count mismatch");
                    }
                } break;

                case ETOK_COMPTIMEFUNC:
                {
                    // TODO
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_invocation_form() : comptime-func evaluation not yet implemented");
                } break;

                case ETOK_MACRO: {
                    // TODO
                    return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                        "typecheck_invocation_form() : macro expansion not yet implemented");
                } break;

                default: {
                    return_error(pExpr, pTCStatement, pTCContext, FERR_UNREACHABLE,
                        "typecheck_invocation_form() : unexpected proc-kind for basic proc-like invocation");
                }
            }
        } break;

        case ETypeKind::ETYPEKIND_PROCLIKEOVERLOAD: {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_invocation_form() : invocation through overloads not yet implemented");
        } break;

        case ETypeKind::ETYPEKIND_PROCLIKEPOLYMORPH: {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_invocation_form() : invocation through static polymorphs not yet implemented");
        } break;

        case ETypeKind::ETYPEKIND_OTHERCORE: {
            if (get_core_type_(invocator.pIntrinsicValue->pType) == ECoreType::ECORETYPE_TYPE) {
                if (eExpectation >= EExpectedExpr::EXPECT_ASSIGNABLE) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                        "typecheck_invocation_form() : standard cast forms cannot be directly assignable");
                }

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking standard cast"), pTCContext->pWorker);

                Assert_(is_value_tc_only(invocator.pIntrinsicValue));
                const TypeInfo* pCastDestType = type_from_type_node(invocator.pIntrinsicValue);
                check_type_footprint_availability_may_return_wait_or_error(pCastDestType, pExpr, pTCStatement, pTCContext,
                    "typecheck_invocation_form() : Cannot cast to");
                u16 unused;
                if (!is_allowed_as_runtime_type(pCastDestType, pTCContext, &unused)) {
                    eExpectation = EExpectedExpr::EXPECT_CONSTANT;
                }

                TmpTCNode tAllInParams[32];
                u8 uInParamsCount = 0;
                if (parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                    ETCResult checkParams = typecheck_param_list(parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex,
                        pTCStatement, pTCContext, eExpectation, tAllInParams, &uInParamsCount, 0); // TODO: pass that signature as last param here
                    success_or_return_wait_or_error(checkParams, pExpr->pTCNode);
                    Assert_(uInParamsCount); // otherwise checkParams should have complained
                }

                return do_special_cast_as_builtin(pExpr, ECastKind::ECAST_STANDARD, pCastDestType, &invocator, tAllInParams, uInParamsCount,
                    pTCStatement, pTCContext, eExpectation, inferredFromBelow,
                    uIndexOfOptNodeChainParent, tAllExpr, ioNodeCount, eRetCount, oWasMacroExpansion);

            } else if (get_core_type_(invocator.pIntrinsicValue->pType) == ECoreType::ECORETYPE_BUILTIN) {

                Assert_(is_value_tc_only(invocator.pIntrinsicValue));

                TmpTCNode tAllInParams[32];
                u8 uInParamsCount = 0;

                u64 uBuiltinId = invocator.pIntrinsicValue->info.metaValue.knownValue._payload;
                if (uBuiltinId & 0x0FuLL) {
                    const TypeInfo* pCastDestType = reinterpret_cast<const TypeInfo*>(uBuiltinId & ~0x0FuLL);
                    if (0 == pCastDestType) {
                        return_error(pExpr, pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
                            "typecheck_invocation_form() : cannot invoke a special casting builtin without a preceeding type-and-dot form");
                    }

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking special-cast builtin"), pTCContext->pWorker);

                    check_type_footprint_availability_may_return_wait_or_error(pCastDestType, pExpr, pTCStatement, pTCContext,
                        "typecheck_invocation_form() : Cannot invoke a special casting builtin to");
                    u16 uTypeErr = 0;
                    if (!is_allowed_as_runtime_type(pCastDestType, pTCContext, &uTypeErr)) {
                        eExpectation = EExpectedExpr::EXPECT_CONSTANT;
                        if ((uBuiltinId & 0x0FuLL) == RESERVED_WORD_SPECIAL_VALUE_SPECIAL_POINTER_CAST) {
                            return_error(pExpr, pTCStatement, pTCContext, u16(uTypeErr),
                                "typecheck_invocation_form() : pointer cast only accept runtime types");
                        }
                    }

                    if (parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                        ETCResult checkParams = typecheck_param_list(parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex,
                            pTCStatement, pTCContext, eExpectation, tAllInParams, &uInParamsCount, 0); // TODO: pass that signature as last param here
                        success_or_return_wait_or_error(checkParams, pExpr->pTCNode);
                        Assert_(uInParamsCount); // otherwise checkParams should have complained
                    }

                    ECastKind eCastKind;
                    switch (uBuiltinId & 0x0FuLL) {
                        case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_TRANSMUTE_CAST:
                            eCastKind = ECastKind::ECAST_TRANSMUTE;
                            break;
                        case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_TRUNCATING_CAST:
                            eCastKind = ECastKind::ECAST_TRUNCATE;
                            break;
                        case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_SATURATING_CAST:
                            eCastKind = ECastKind::ECAST_SATURATE;
                            break;
                        case RESERVED_WORD_SPECIAL_VALUE_SPECIAL_POINTER_CAST:
                            pCastDestType = get_pointer_type_to(pCastDestType, pTCContext);
                            eCastKind = ECastKind::ECAST_TRANSMUTE;
                            break;
                        default:
                            return_error(pExpr, pTCStatement, pTCContext, FERR_OTHER,
                                "typecheck_invocation_form() : found unknown builtin id with 'special' marks");
                    }

                    return do_special_cast_as_builtin(pExpr, eCastKind, pCastDestType, &invocator, tAllInParams, uInParamsCount,
                        pTCStatement, pTCContext, eExpectation, inferredFromBelow,
                        uIndexOfOptNodeChainParent, tAllExpr, ioNodeCount, eRetCount, oWasMacroExpansion);

                } else {
                    u64 uStandardBuiltinIndex = uBuiltinId >> 4;
                    Assert_(uStandardBuiltinIndex < u64(COUNT_BUILTINS));

                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking builtin %llu", 
                        uStandardBuiltinIndex), pTCContext->pWorker);

                    if (parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
                        ETCResult checkParams = typecheck_param_list(parensWrapper.pTCNode->ast.uPrimaryChildNodeIndex,
                            pTCStatement, pTCContext, eExpectation, tAllInParams, &uInParamsCount, 0); // TODO: pass that signature as last param here
                        success_or_return_wait_or_error(checkParams, pExpr->pTCNode);
                        Assert_(uInParamsCount); // otherwise checkParams should have complained
                    }

                    // TODO: check signature here before invoc
                    TCBuiltinSign* pTCBuiltin = t_allBuiltins[uStandardBuiltinIndex];
                    return pTCBuiltin(pExpr, &invocator, tAllInParams, uInParamsCount,
                        pTCStatement, pTCContext, eExpectation, inferredFromBelow,
                        uIndexOfOptNodeChainParent, tAllExpr, ioNodeCount, eRetCount, oWasMacroExpansion);
                }

            } else {
                return_error(pExpr, pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
                    "typecheck_invocation_form() : expected proc-like or macro or type or builtin-cast");
            }
        } break;

        default: {
            return_error(pExpr, pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
                "typecheck_invocation_form() : expected proc-like or macro or type or builtin-cast");
        }
    }
}

#endif // LOCLIB_TC_INVOC_LIKE_H_

