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

#ifndef LOCLIB_TC_PAN_STATEMENTS_H_
#define LOCLIB_TC_PAN_STATEMENTS_H_

#include "LocLib_TypeCheckerTypes.h"
#include "LocLib_TypeCheckerCore.h"
#include "LocLib_TypeCheckerBase.h"
#include "LocLib_IR_SolverInterface.h"
#include "LocLib_TC_Casts.h"


local_func ETCResult typecheck_using_statement(TmpTCNode* pMainNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Typechecking Main Node of a 'using' statement"), pTCContext->pWorker);

    TmpTCNode whatToUse = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
            "Typechecking Expr after 'using' or 'including' statement"), pTCContext->pWorker);
        ETCResult checkWhatToUse = typecheck_expression(&whatToUse, pTCStatement, pTCContext,
            EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{}, true); // last 'true' here allows declarations
        success_or_return_wait_or_error(checkWhatToUse, pMainNode->pTCNode);
    }

    Assert_(whatToUse.pIntrinsicValue);
    Assert_(whatToUse.pIntrinsicValue->pType);

    u8 uKey = u8(pMainNode->pTCNode->ast.uNodeKindAndFlags >> 8);
    if (uKey == ETOK_USING) {

        if (whatToUse.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_NAMESPACE]) {
            Assert_(is_value_tc_only(whatToUse.pIntrinsicValue));
            i32 iNamespaceSourceFile; u32 uNamespaceRegistration;
            decode_namespace_id(whatToUse.pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue,
                &iNamespaceSourceFile, &uNamespaceRegistration);
            SourceFileDescAndState* pOrigSourceFile = pTCContext->pProgCompilationState->vecSourceFiles[u32(iNamespaceSourceFile)];
            TCNamespace* pOrigNamespace = pOrigSourceFile->vecNamespaces[uNamespaceRegistration];

            if (!is_ctx_compound(pTCContext)) {
                if (is_ctx_global(pTCContext)) {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                        "Registration of a new namespace as 'using' in current namespace"), pTCContext->pWorker);
                    if (pOrigSourceFile == pTCContext->pIsolatedSourceFile) {
                        if (!check_not_circular_using_from(pTCContext->pNamespace, pOrigNamespace, pTCContext->pWorker->tmpArena)) {
                            return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                                "'using' of this namespace here would create a circular using chain. This is forbidden.");
                        }
                        bool bLocalShadowing;
                        if (!check_no_double_inclusions_namespace_from_same_source_in_current_namespace(pOrigNamespace, pTCContext, &bLocalShadowing)) {
                            if (bLocalShadowing) {
                                return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                    "'using' of this namespace here would violate a name shadowing interdiction."); // TODO: log which
                            } else {
                                return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                    "'using' of this namespace here would result in name collision."); // TODO: log which
                            }
                        }
                        pOrigNamespace->vecAllNamespacesInSameFileUsingSelf.append(pTCContext->pNamespace->uRegistrationIndex);
                        // whether currently public or private, all bindings from namespace in same file are copied to our map-of-all
                        for (auto it = pOrigNamespace->mapAllBindingsInclUsing.begin(), itEnd = pOrigNamespace->mapAllBindingsInclUsing.end();
                                it != itEnd; it++) {
                            pTCContext->pNamespace->mapAllBindingsInclUsing.insert(it.key(), it.value());
                        }
                    } else {
                        if (!is_non_local_namespace_fully_discovered_otherwise_return_locked_for_wait(pOrigNamespace, pTCContext)) {
                            return add_waiting_task_for_already_event_locked_othersource_namespace(pOrigNamespace, pMainNode->uNodeIndexInStatement, pTCContext);
                        }
                        bool bLocalShadowing;
                        if (!check_no_double_inclusions_namespace_from_other_source_in_current_namespace(pOrigNamespace, pTCContext, &bLocalShadowing)) {
                            if (bLocalShadowing) {
                                return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                    "'using' of this namespace (from other file) here would violate a name shadowing interdiction."); // TODO: log which
                            } else {
                                return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                    "'using' of this namespace (from other file) here would result in name collision."); // TODO: log which
                            }
                        }
                        // whether currently public or private, accessible bindings from namespace in other file are copied to our map-of-all
                        for (auto it = pOrigNamespace->mapAccessibleBindingsInclUsing.begin(), itEnd = pOrigNamespace->mapAccessibleBindingsInclUsing.end();
                                it != itEnd; it++) {
                            pTCContext->pNamespace->mapAllBindingsInclUsing.insert(it.key(), it.value());
                        }
                    }
                    pTCContext->pNamespace->vecAllUsedNamespaces.append(pOrigNamespace);
                    if (pTCContext->eGlobalDeclScope != EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
                        // that 'used' becomes accessible also
                        pTCContext->pNamespace->vecAccessibleUsedNamespaces.append(pOrigNamespace);
                        // ... and only if non-private, accessible-only bindings from namespace in same file are copied to our map-of-accessible
                        for (auto it = pOrigNamespace->mapAccessibleBindingsInclUsing.begin(), itEnd = pOrigNamespace->mapAccessibleBindingsInclUsing.end();
                                it != itEnd; it++) {
                            pTCContext->pNamespace->mapAccessibleBindingsInclUsing.insert(it.key(), it.value());
                        }
                    }
                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                } else { Assert_(is_ctx_with_proc_source(pTCContext));

                    Assert_(pTCContext->pProcResult);
                    if (pOrigSourceFile == pTCContext->pIsolatedSourceFile) {
                        // within procs, we enforce strict termination of the discovery for all used namespaces, even if from same file.
                        // the complexity of checking non-conflicts against multi-using proc-local statements seems too high otherwise.
                        if (!is_local_namespace_fully_discovered(pOrigNamespace, pTCContext)) {
                            return add_waiting_task_for_fully_discovered_namespace_in_same_source(pOrigNamespace,
                                pMainNode->uNodeIndexInStatement, pTCContext);
                        }
                    } else {
                        if (!is_non_local_namespace_fully_discovered_otherwise_return_locked_for_wait(pOrigNamespace, pTCContext)) {
                            return add_waiting_task_for_already_event_locked_othersource_namespace(pOrigNamespace,
                                pMainNode->uNodeIndexInStatement, pTCContext);
                        }
                    }
                    if (!check_no_double_inclusions_namespace_in_proclocal(pOrigNamespace, pTCContext)) {
                        return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                            "'using' of this namespace in this proc-local scope would result in name collision."); // TODO: log which
                    }
                    // Note: we do not check for namespace in current *namespace* there,
                    //    since we dropped out the idea of global unshadowing from proc scopes
                    pTCContext->pProcResult->vecScopedEntities.append(make_scoped_entity(pOrigNamespace));
                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                }

            } else {
                Assert_(pTCContext->pCompoundToTC);
                if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                        "'using' of namespace within a struct-like is invalid.");
                } else { Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_ENUM);
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                        "'using' of namespace within enum is invalid.");
                }
            }

        } else if (whatToUse.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {

            const TypeInfo* pUsedType = type_from_type_node(whatToUse.pIntrinsicValue);
            if (get_type_kind(pUsedType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                if (!is_ctx_compound(pTCContext)) {
                    if (is_ctx_global(pTCContext)) {
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "'using' of struct-like at global scope is invalid");
                    } else {
                        Assert_(is_ctx_regular_proc_body(pTCContext));
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "'using' of struct-like within proc-body is invalid");
                    }
                } else { Assert_(is_ctx_compound(pTCContext));
                    Assert_(pTCContext->pCompoundToTC);
                    if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "'using' of struct-like within another struct-like is invalid. Did you intend 'including' ?");
                    } else { Assert_(get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_ENUM);
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "'using' of struct-like within enum is invalid.");
                    }
                }
            } else if (get_type_kind(pUsedType) == ETypeKind::ETYPEKIND_ENUM) {
                const TypeInfo_Enum* pAsUsedEnum = (const TypeInfo_Enum*)pUsedType;
                check_compound_type_full_availability_may_return_wait_or_error(pAsUsedEnum, pMainNode, pTCStatement, pTCContext,
                    "cannot succeed on 'using' of");

                if (!is_ctx_compound(pTCContext)) {
                    if (is_ctx_global(pTCContext)) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                            "Registration of a new enum as 'using' in current namespace"), pTCContext->pWorker);
                        bool bLocalShadowing;
                        ValueBinding* foundDoubleInclusion = check_no_double_inclusions_enum_in_current_namespace(pAsUsedEnum,
                            pTCContext, &bLocalShadowing);
                        if (foundDoubleInclusion) {
                            if (bLocalShadowing) {
                                return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                    "'using' of enum in current namespace would violate a name shadowing interdiction."); // TODO: log which
                            } else {
                                return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                    "'using' of enum in current namespace would result in name collisions"); // TODO: log which
                            }
                        }

                        pTCContext->pNamespace->vecAllUsedEnums.append(pAsUsedEnum);
                        add_all_bindings_from_enum_to(&pTCContext->pNamespace->mapAllBindingsInclUsing, pAsUsedEnum);
                        if (pTCContext->eGlobalDeclScope < SCOPEKIND_GLOBAL_PRIVATE) {
                            pTCContext->pNamespace->vecAccessibleUsedEnums.append(pAsUsedEnum);
                            add_all_bindings_from_enum_to(&pTCContext->pNamespace->mapAccessibleBindingsInclUsing, pAsUsedEnum);
                        }
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                    } else { Assert_(is_ctx_with_proc_source(pTCContext));

                        Assert_(pTCContext->pProcResult);
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                            "Registration of a new enum as 'using' in current proc scope"), pTCContext->pWorker);

                        ValueBinding* foundDoubleInclusion = check_no_double_inclusions_enum_in_proclocal(pAsUsedEnum, pTCContext);
                        // Note: we do not check for enum in current *namespace* there,
                        //    since we dropped out the idea of global unshadowing from proc scopes
                        if (foundDoubleInclusion) {
                            // TODO: report which
                            return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "'using' of enum at current proclocal scope would result in name collisions");
                        }

                        pTCContext->pProcResult->vecScopedEntities.append(make_scoped_entity(pAsUsedEnum));
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                    }
                } else { Assert_(is_ctx_compound(pTCContext));
                    Assert_(pTCContext->pCompoundToTC);
                    if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_ENUM) {
                        TypeInfo_Enum* pAsCurrentEnum = (TypeInfo_Enum*)pTCContext->pCompoundToTC->pCompoundType;
                        if (!are_types_same(pAsCurrentEnum->pBaseType, pAsUsedEnum->pBaseType, pTCContext)) {
                            // TODO: some could still be acceptable conversions ?
                            return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                                "'using' of enum with a distinct base type is forbidden");
                        }

                        ValueBinding* foundDoubleInclusion = check_no_double_inclusions_enum_in_enum(pAsUsedEnum, pAsCurrentEnum, pTCContext);
                        // Note: we do not check for enum in current *namespace* there,
                        //    since we dropped out the idea of global unshadowing from enums
                        if (foundDoubleInclusion) {
                            return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "'using' of enum in current enum would result in name collisions"); // TODO: log which
                        }
                        // Note: we do *not* need to check if not circular here... since we typecheck enum bodies *sequentially*,
                        //    and wait for completion before usage.
                        pAsCurrentEnum->vecUsed.append(pAsUsedEnum);
                        if (pAsUsedEnum->pLastValue) {
                            pAsCurrentEnum->pLastValue = pAsUsedEnum->pLastValue;
                        }
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                    } else {
                        // TODO: CLEANUP: THOUGHTS: ... or maybe allow that ???
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "'using' of enum within struct-like is invalid");
                    }
                }
            } // otherwise fallthrough
        }
    
        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
            "invalid expression after 'using'. Expected namespace, or enum.");
    
    } else { Assert_(uKey == ETOK_INCLUDING);
        
        if (whatToUse.pIntrinsicValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
            const TypeInfo* pIncludedType = type_from_type_node(whatToUse.pIntrinsicValue);
            if (get_type_kind(pIncludedType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                const TypeInfo_StructLike* pAsIncludedStructLike = (const TypeInfo_StructLike*)pIncludedType;
                if (is_ctx_compound(pTCContext)) {
                    Assert_(pTCContext->pCompoundToTC);
                    if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
                            "Registration of a structlike as an included aggregate in current structlike"), pTCContext->pWorker);
                        check_compound_type_full_availability_may_return_wait_or_error(pAsIncludedStructLike, pMainNode, pTCStatement, pTCContext,
                            "cannot succeed 'including' of");

                        TypeInfo_StructLike* pTCedStructLike = (TypeInfo_StructLike*)pTCContext->pCompoundToTC->pCompoundType;
                        if (!check_no_double_inclusions_structlike_in_structlike(pAsIncludedStructLike, pTCedStructLike, pTCContext)) {
                            return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "'including' of this other structlike in this structlike scope would result in name collision."); // TODO: log which
                        }
                        if (!check_no_double_inclusions_structlike_in_current_namespace_and_set_unshadow(pAsIncludedStructLike, pTCContext)) {
                            return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "'including' of this other structlike in this structlike scope would result in name collision with encompassing namespace."); // TODO: log which
                        }

                        u32 uInclusionPos = pTCedStructLike->vecAllMembers.size();
                        pTCedStructLike->vecIncluded.append(uInclusionPos);
                        ValueBinding* pMockBinding = (ValueBinding*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
                            sizeof(ValueBinding), alignof(ValueBinding));
                        set_binding_source_ref(pMockBinding, pTCStatement, pTCContext, EDeclAttributes::EDECLATTR_REGULAR_CONST);
                        pMockBinding->iIdentifierHandle = STRUCTLIKE_MOCK_BINDING_INCLUSION;
                        pMockBinding->pType = g_pCoreTypesInfo[ECORETYPE_TYPE];
                        pMockBinding->info.uIRandMetaFlags = IRFLAG_TC_ONLY | IRFLAG_TC_SEMANTIC_CONST | IRFLAG_TC_BINDING_INSTANCE;
                        pMockBinding->info.metaValue.knownValue.pType = pAsIncludedStructLike;
                        pMockBinding->uCompoundDeclSort = pTCStatement->uBlockIndexInSourceFile << 5u;
                        pTCedStructLike->vecAllMembers.append(pMockBinding);
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                    } // Otherwise fallthrough
                } // Otherwise fallthrough

                return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                    "'including' a struct-like not supported in that context. It is meant to aggregate struct-likes within other struct-likes.");

            } // Otherwise fallthrough
        } // Otherwise fallthrough

        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
            "invalid expression after 'including'. Expected struct-like type.");
    }
}


// At statement level, typechecks pan-keywords
local_func ETCResult typecheck_pan_statement(TmpTCNode* pMainNode, TCStatement* pTCStatement, TCContext* pTCContext)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Main Node of a pan-statement"), pTCContext->pWorker);
    
    Assert_(u8(pMainNode->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ST_PAN_SPECIAL);
    u8 uTok = u8(pMainNode->pTCNode->ast.uNodeKindAndFlags >> 8);
    switch (uTok) {
        case ETOK_PAN_IF: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Typechecking pan-if statement"), pTCContext->pWorker);
            u32 uStatementIndex = pTCStatement->uStatementIndexInBlock;
            Assert_(uStatementIndex == pTCContext->pCurrentBlock->uStatementBeingTypechecked);
            if (uStatementIndex > 0) {
                TCNode* pPrevMainNode = pTCContext->pCurrentBlock->vecStatements[uStatementIndex-1u]->vecNodes[0u];
                u8 uPrevNodeKind = u8(pPrevMainNode->ast.uNodeKindAndFlags);
                if (uPrevNodeKind == ENodeKind::ENODE_ST_PAN_SPECIAL) {
                    u8 uPrevTok = u8(pPrevMainNode->ast.uNodeKindAndFlags >> 8);
                    if (uPrevTok == ETOK_PAN_IF || uPrevTok == ETOK_PAN_ELIF || uPrevTok == ETOK_PAN_ELSE || uPrevTok == ETOK_PAN_NAMESPACE || uPrevTok == ETOK_PAN_PRIVATE) {
                        // TODO: should maybe be an assert instead: PostParser should have prevented this (opening of child block after all those => cannot be preceeding in same block...)
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "typecheck_pan_statement() : incorrect pan-if position in another pan-chain");
                    }
                }
            }

            TmpTCNode conditionNode = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
            {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Condition of pan-if statement"), pTCContext->pWorker);
                ETCResult eCheckCond = typecheck_expression(&conditionNode, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
                success_or_return_wait_or_error(eCheckCond, pMainNode->pTCNode);
                Assert_(is_node_already_typechecked(conditionNode.pTCNode));
                Assert_(conditionNode.pIntrinsicValue);
                Assert_(conditionNode.pIntrinsicValue->pType);
                if (conditionNode.pIntrinsicValue->pType != g_pCoreTypesInfo[ECORETYPE_BOOL]) {
                    return_error(pMainNode, pTCStatement, pTCContext, CERR_EXPECTED_BOOLEAN,
                        "typecheck_pan_statement() : pan-if condition expression should typecheck to a boolean");
                }
            }
            Assert_(is_value_tc_const(conditionNode.pIntrinsicValue));
            Assert_(is_value_known_non_nyka_embd(conditionNode.pIntrinsicValue));
            u64 uBoolValue = conditionNode.pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
            Assert_(uBoolValue <= 1uLL);
            if (uBoolValue) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-if condition evaluated to true"), pTCContext->pWorker);
                if (pTCStatement->pChildBlock) { // in case there is a child block missing error, we'd still have allowed to tc the condition
                    u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                    if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                        // And we spawn it now...
                        pTCStatement->pChildBlock = init_new_child_block_of_pan_from_tagged(uTagged, pTCContext);
                    }
                    if (is_ctx_compound(pTCContext)) {
                        Assert_(pTCContext->pCompoundToTC);
                        if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("=> pan-if set-up to spawn a virtual structlike as its child"), pTCContext->pWorker);
                            // TODO
                            return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                "typecheck_pan_statement() : spawning virtual within struct-like not yet implemented");
                        } // Otherwise fallthrough
                    }
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-if set-up to continue typechecking through true-path child"), pTCContext->pWorker);
                    pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement = pTCStatement->pChildBlock;
                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                } else {
                    if (pTCStatement->pAstStatement->uFlags & STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD) {
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-if (true path) validly without child block"), pTCContext->pWorker);
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                    } else {
                        return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                            "typechecker error for #if statement missing child block");
                    }
                }

            } else {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-if condition evaluated to false"), pTCContext->pWorker);
                // in case of false, we simply do *not* register our child block for typechecking... and carry-on
                pMainNode->pTCNode->ast.uNodeKindAndFlags |= ENODEKINDFLAG_WAS_TC_PANIF_NOT_TAKEN; // we'll flag ourselves as *not taken* for possible elif or else, though
                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
            }

        } break;

        case ETOK_PAN_ELIF: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Typechecking pan-elif statement"), pTCContext->pWorker);
            u32 uStatementIndex = pTCStatement->uStatementIndexInBlock;
            Assert_(uStatementIndex == pTCContext->pCurrentBlock->uStatementBeingTypechecked);
            if (uStatementIndex > 0) {
                TCNode* pPrevMainNode = pTCContext->pCurrentBlock->vecStatements[uStatementIndex-1u]->vecNodes[0u];
                u8 uPrevNodeKind = u8(pPrevMainNode->ast.uNodeKindAndFlags);
                if (uPrevNodeKind == ENodeKind::ENODE_ST_PAN_SPECIAL) {
                    u8 uPrevTok = u8(pPrevMainNode->ast.uNodeKindAndFlags >> 8);
                    if (uPrevTok != ETOK_PAN_IF && uPrevTok != ETOK_PAN_ELIF) {
                        // TODO: should maybe be an assert instead: PostParser should have prevented this...
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "typecheck_pan_statement() : incorrect pan-elif position (expected #if or #elif before)");
                    }
                    if (is_node_already_typechecked(pPrevMainNode)) {
                        u32 bPreviousWasNotTaken = pPrevMainNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_WAS_TC_PANIF_NOT_TAKEN;
                        if (bPreviousWasNotTaken) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif : previous chain is still looking for its path"), pTCContext->pWorker);
                            TmpTCNode conditionNode = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                            {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Condition of pan-elif statement"), pTCContext->pWorker);
                                ETCResult eCheckCond = typecheck_expression(&conditionNode, pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
                                success_or_return_wait_or_error(eCheckCond, pMainNode->pTCNode);
                                Assert_(is_node_already_typechecked(conditionNode.pTCNode));
                                Assert_(conditionNode.pIntrinsicValue);
                                Assert_(conditionNode.pIntrinsicValue->pType);
                                if (conditionNode.pIntrinsicValue->pType != g_pCoreTypesInfo[ECORETYPE_BOOL]) {
                                    return_error(pMainNode, pTCStatement, pTCContext, CERR_EXPECTED_BOOLEAN,
                                        "typecheck_pan_statement() : pan-elif condition expression should typecheck to a boolean");
                                }
                            }
                            Assert_(is_value_tc_const(conditionNode.pIntrinsicValue));
                            Assert_(is_value_known_non_nyka_embd(conditionNode.pIntrinsicValue));
                            u64 uBoolValue = conditionNode.pIntrinsicValue->info.metaValue.knownValue.uEmbeddedValue;
                            Assert_(uBoolValue <= 1uLL);
                            if (uBoolValue) {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif condition evaluated to true"), pTCContext->pWorker);
                                if (pTCStatement->pChildBlock) { // in case there is a child block missing error, we'd still have allowed to tc the condition
                                    u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                                    if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                                        // And we spawn it now...
                                        pTCStatement->pChildBlock = init_new_child_block_of_pan_from_tagged(uTagged, pTCContext);
                                    }
                                    if (is_ctx_compound(pTCContext)) {
                                        Assert_(pTCContext->pCompoundToTC);
                                        if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("=> pan-elif set-up to spawn a virtual structlike as its child"), pTCContext->pWorker);
                                            // TODO
                                            return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                                "typecheck_pan_statement() : spawning virtual within struct-like not yet implemented");
                                        } // Otherwise fallthrough
                                    }
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif set-up to continue typechecking through true-path child"), pTCContext->pWorker);
                                    pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement = pTCStatement->pChildBlock;
                                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                                } else {
                                    if (pTCStatement->pAstStatement->uFlags & STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD) {
                                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif (true path) validly without child block"), pTCContext->pWorker);
                                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                                    } else {
                                        return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                                            "typechecker error for #elif statement missing child block");
                                    }
                                }

                            } else {
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif condition evaluated to false"), pTCContext->pWorker);
                                // in case of false, we simply do *not* register our child block for typechecking... and carry-on
                                pMainNode->pTCNode->ast.uNodeKindAndFlags |= ENODEKINDFLAG_WAS_TC_PANIF_NOT_TAKEN; // we'll flag ourselves as *not taken* for possible elif or else, though
                                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                            }

                        } else {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif : previous chain is done looking for its path => skipping as success"), pTCContext->pWorker);
                            // in case of previous not-flaggued as not-taken, we *won't* flag ourselves as not-taken either => no other successor in the chain will get to typecheck its contents
                            return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                        }
                    } else {
                        Assert_(!should_tc_ctx_halt_on_non_success(pTCContext));
                        if (is_node_tc_in_error(pPrevMainNode)) {
                            return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                                "typecheck_pan_statement() : cannot typecheck #elif after a #if-chain in error.");
                        } else { // previous node not typechecked, nor in error... it can either be waiting, or be silently skipped from a previous wait
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-elif : previous chain is waiting => skipping silently, atm (should be tc'd again)"), pTCContext->pWorker);
                            // => in that case, we'll hack a little arrangement to make this one be silently skipped also:
                            // it will be typechecked afterwards by the supposed task-in-waiting at some point in the chain, once it resumes.
                            return ETCResult(0u - 1u); // TC coordinator will assign 1+our result to the TC status => shall be kept 0 that way :p
                        }
                    }
                } else {
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                        "typecheck_pan_statement() : incorrect pan-elif position (not a pan-op before)");
                }
            } else {
                return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                    "typecheck_pan_statement() : incorrect pan-elif position (first in its block)");
            }
        } break;

        case ETOK_PAN_ELSE: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Typechecking pan-else statement"), pTCContext->pWorker);
            u32 uStatementIndex = pTCStatement->uStatementIndexInBlock;
            Assert_(uStatementIndex == pTCContext->pCurrentBlock->uStatementBeingTypechecked);
            if (uStatementIndex > 0) {
                TCNode* pPrevMainNode = pTCContext->pCurrentBlock->vecStatements[uStatementIndex-1u]->vecNodes[0u];
                u8 uPrevNodeKind = u8(pPrevMainNode->ast.uNodeKindAndFlags);
                if (uPrevNodeKind == ENodeKind::ENODE_ST_PAN_SPECIAL) {
                    u8 uPrevTok = u8(pPrevMainNode->ast.uNodeKindAndFlags >> 8);
                    if (uPrevTok != ETOK_PAN_IF && uPrevTok != ETOK_PAN_ELIF) {
                        // TODO: should maybe be an assert instead: PostParser should have prevented this...
                        return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                            "typecheck_pan_statement() : incorrect pan-elif position (expected #if or #elif before)");
                    }
                    if (is_node_already_typechecked(pPrevMainNode)) {
                        u32 bPreviousWasNotTaken = pPrevMainNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_WAS_TC_PANIF_NOT_TAKEN;
                        if (bPreviousWasNotTaken) {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-else : previous chain is still looking for its path"), pTCContext->pWorker);
                            if (pTCStatement->pChildBlock) { // in case there is a child block missing error, we'd still have allowed to tc the condition
                                u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                                if (uTagged & 0x01uLL) {    // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                                    // And we spawn it now...
                                    pTCStatement->pChildBlock = init_new_child_block_of_pan_from_tagged(uTagged, pTCContext);
                                }
                                if (is_ctx_compound(pTCContext)) {
                                    Assert_(pTCContext->pCompoundToTC);
                                    if (get_type_kind(pTCContext->pCompoundToTC->pCompoundType) == ETypeKind::ETYPEKIND_STRUCTLIKE) {
                                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("=> pan-else set-up to spawn a virtual structlike as its child"), pTCContext->pWorker);
                                        // TODO
                                        return_error(pMainNode, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                                            "typecheck_pan_statement() : spawning virtual within struct-like not yet implemented");
                                    } // Otherwise fallthrough
                                }
                                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("=> pan-else set-up to continue typechecking through its child"), pTCContext->pWorker);
                                pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement = pTCStatement->pChildBlock;
                                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                            } else {
                                if (pTCStatement->pAstStatement->uFlags & STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD) {
                                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-else validly without child block"), pTCContext->pWorker);
                                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                                } else {
                                    return_error(pMainNode, pTCStatement, pTCContext, CERR_CONTROL_FLOW_STATEMENT_MISSING_CHILD_BLOCK,
                                        "typechecker error for #else statement missing child block");
                                }
                            }

                        } else {
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-else : previous chain is done looking for its path => skipping as success"), pTCContext->pWorker);
                            // in case of previous not-flaggued as not-taken, we *won't* flag ourselves as not-taken either => no other successor in the chain will get to typecheck its contents
                            return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                        }
                    } else {
                        Assert_(!should_tc_ctx_halt_on_non_success(pTCContext));
                        if (is_node_tc_in_error(pPrevMainNode)) {
                            return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                                "typecheck_pan_statement() : cannot typecheck #elif after a #if-chain in error.");
                        } else { // previous node not typechecked, nor in error... it can either be waiting, or be silently skipped from a previous wait
                            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("pan-else : previous chain is waiting => skipping silently, atm (should be tc'd again)"), pTCContext->pWorker);
                            // => in that case, we'll hack a little arrangement to make this one be silently skipped also:
                            // it will be typechecked afterwards by the supposed task-in-waiting at some point in the chain, once it resumes.
                            return ETCResult(0u - 1u); // TC coordinator will assign 1+our result to the TC status => shall be kept 0 that way :p
                        }
                    }
                } else {
                    return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                        "typecheck_pan_statement() : incorrect pan-else position (not a pan-op before)");
                }
            } else {
                return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                    "typecheck_pan_statement() : incorrect pan-else position (first in its block)");
            }
        } break;

        case ETOK_PAN_ENDIF: {
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED("Typechecking pan-endif statement => NOOP, success"), pTCContext->pWorker);
            return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
        } break;

        case ETOK_PAN_PRIVATE: {
            // If preparser has done its verification job correctly, then next statement *within current block* should be #endprivate.
            Assert_(pTCContext->pCurrentBlock->vecStatements.size() > pTCStatement->uStatementIndexInBlock + 1u);
            Assert_(u8(pTCContext->pCurrentBlock->vecStatements[pTCStatement->uStatementIndexInBlock + 1u]->
                        vecNodes[0u]->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ST_PAN_SPECIAL);
            Assert_(u8(pTCContext->pCurrentBlock->vecStatements[pTCStatement->uStatementIndexInBlock + 1u]->
                        vecNodes[0u]->ast.uNodeKindAndFlags >> 8) == ETOK_PAN_ENDPRIVATE);

            if (is_ctx_global(pTCContext)) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Typechecking pan-private statement at global scope"), pTCContext->pWorker);

                if (0u == (pTCStatement->pAstStatement->uFlags & STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD)) {

                    Assert_(pTCStatement->pChildBlock);
                    u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                    Assert_(uTagged & 0x01uLL); // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                    // And we spawn it now...
                    pTCStatement->pChildBlock = init_new_child_block_of_pan_from_tagged(uTagged, pTCContext);

                    if (pTCContext->eGlobalDeclScope < EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
                        // then we'll spawn a *new*, independent context, with *its* scope as private, for all statements and block-descendants
                        //   of the child-block from that private scope. Our very context itself will continue next statement at *current* block,
                        //   which should happen to be a valid #endprivate, provided preparser has done its verification job. By doing that,
                        //   our current context will plow grass under the private child's feet when it reaches same #endprivate, which will
                        //   very conveniently instruct it to stop.
                        TCContext* pNewPrivateCtx = (TCContext*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
                            sizeof(TCContext), alignof(TCContext));
                        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                            "Allocated new TC Task 0x%llx for typechecking contents within a #private block",
                            reinterpret_cast<u64>(pNewPrivateCtx)), pTCContext->pWorker);
                        *pNewPrivateCtx = *pTCContext;
                        pNewPrivateCtx->eKind = ETypecheckContextKind::ECTXKIND_GLOBAL_PRIVATE;
                        pNewPrivateCtx->eGlobalDeclScope = EScopeKind::SCOPEKIND_GLOBAL_PRIVATE;
                        pNewPrivateCtx->setOfNewlyDeclaredIdentifiers = {}; // will be init later, at task start
                        pNewPrivateCtx->uSizeOfVecUsingAccessibleEnumBefore = 0u;
                        pNewPrivateCtx->uSizeOfVecUsingAccessibleNamespaceBefore = 0u;
                        pNewPrivateCtx->uSizeOfVecUsingAllEnumBefore = 0u;
                        pNewPrivateCtx->uSizeOfVecUsingAllNamespaceBefore = 0u;
                        pNewPrivateCtx->uSizeOfVecChildrenNamespacesBefore = 0u;
                        pNewPrivateCtx->uSizeOfVecIncludedStructLikeBefore = 0u;
                        pNewPrivateCtx->pCurrentBlock = pTCStatement->pChildBlock;
                        pNewPrivateCtx->bResumingCurrentStatement = 0u;
                        pNewPrivateCtx->uFlags = 0u;
                        pTCContext->pIsolatedSourceFile->tvecTCTasksToLaunchByPrio[pNewPrivateCtx->eTaskPrio].append(pNewPrivateCtx);
                        pTCContext->pNamespace->uCountGlobalPrivateTasks++;
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);

                    } else {  Assert_(pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PRIVATE);
                        // Current context is already private on reaching this new '#private' statement
                        // aaaannd... we'll continue parsing at that same private scope, no need to spawn a new child task:
                        //   stoping of the actual (full) private-nested-structure will be when we encounter an already parsed line
                        //   from non-private global context (the one which originally spawned ourselves), which, as a matter of fact
                        //   would be same line as the correct #endprivate.
                        // We however need to setup for continuing parsing within the child block
                        pTCContext->pCurrentBlock->pNextTcBlockAfterCurrentStatement = pTCStatement->pChildBlock;
                        return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                    }
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Pan-private statement with empty child block: skipped"), pTCContext->pWorker);
                    // NOOP
                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                }
            } else {
                return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                    "typecheck_pan_statement() : pan-private is only allowed at global scope");
            }
        } break;

        case ETOK_PAN_NAMESPACE: {
            // If preparser has done its verification job correctly, then next statement *within current block* should be #endnamespace.
            Assert_(pTCContext->pCurrentBlock->vecStatements.size() > pTCStatement->uStatementIndexInBlock + 1u);
            Assert_(u8(pTCContext->pCurrentBlock->vecStatements[pTCStatement->uStatementIndexInBlock + 1u]->
                        vecNodes[0u]->ast.uNodeKindAndFlags) == ENodeKind::ENODE_ST_PAN_SPECIAL);
            Assert_(u8(pTCContext->pCurrentBlock->vecStatements[pTCStatement->uStatementIndexInBlock + 1u]->
                        vecNodes[0u]->ast.uNodeKindAndFlags >> 8) == ETOK_PAN_ENDNAMESPACE);

            if (is_ctx_global(pTCContext)) {

                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Typechecking pan-namespace statement at global scope"), pTCContext->pWorker);

                TmpTCNode nodeName = init_tmp_tc_node(pMainNode->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
                {
                    ETCResult eCheckName = typecheck_expression(&nodeName, pTCStatement, pTCContext, EExpectedExpr::EXPECT_DECLARABLE, UpwardsInference{});
                    success_or_return_wait_or_error(eCheckName, pMainNode->pTCNode);
                }

                int iIdentifier = get_id_from_decl_node(&nodeName, pTCStatement);
                if (iIdentifier == ERES_SINK) { // sink is still a valid result for typecheck_expression with 'EXPECT_DECLARABLE'
                    // ... but not here...
                    return_error(pMainNode, pTCStatement, pTCContext, CERR_CANNOT_EVALUATE_SINK, // CLEANUP: not a good err code there
                        "typecheck_pan_statement() : pan-namespace cannot use 'sink' as id");
                }
                
                Assert_(iIdentifier >= COUNT_RESERVED_WORDS);
                u64 uHash = get_map_hash(iIdentifier);

                // Checking that identifier is not already defined within globals, if not allowed to shadow globals
                {
                    bool bConflictsUnshadowing = false;
                    ValueBinding* pAlreadyFound = find_binding_within_namespace(iIdentifier, uHash, pTCContext->pNamespace, 
                        true, // also checking for parents of current namespace
                        pTCContext,
                        false, &bConflictsUnshadowing); // also checking against setLocalUnshadowing on each such visited...
                    if (pAlreadyFound) {
                        if (bConflictsUnshadowing) {
                            return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_pan_statement() : this namespace id would conflict with shadowing rules");
                        } else {
                            return_error(pMainNode, pTCStatement, pTCContext, RERR_ALREADY_DECLARED_IDENTIFIER,
                                "typecheck_pan_statement() : this namespace id was already declared as a global identifier");
                        }
                    }
                }

                // Spawning a new namespace, and register as child namespace of current
                //

                TCNamespace* pNewNamespace = (TCNamespace*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
                    sizeof(TCNamespace), alignof(TCNamespace));
                pTCContext->pIsolatedSourceFile->init_and_register_namespace(pNewNamespace, pTCContext->pIsolatedSourceFile, pTCContext->pNamespace);
                pNewNamespace->iPrimaryIdentifier = iIdentifier;
                pTCContext->pNamespace->vecChildrenNamespaces.append(pNewNamespace->uRegistrationIndex);
                u64 uRegistration = get_namespace_id(pNewNamespace->pOriginalSourceFile->iRegistrationIndex,
                                                     pNewNamespace->uRegistrationIndex, NAMESPACEFLAG_HAS_PACKAGE_ACCESS);

                // binding id as tc-only of namespace type
                //

                ValueBinding* pNewBinding = (ValueBinding*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
                    sizeof(ValueBinding), alignof(ValueBinding));
                u32 uBindingPos = pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.size();
                pNewBinding->iIdentifierHandle = iIdentifier;
                pNewBinding->pType = g_pCoreTypesInfo[ECORETYPE_NAMESPACE];
                pNewBinding->info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN;
                pNewBinding->info.metaValue.knownValue.uEmbeddedValue = uRegistration;
                set_binding_source_ref(pNewBinding, pTCStatement, pTCContext, EDeclAttributes::EDECLATTR_REGULAR_CONST);
                pNewBinding->uCompoundDeclSort = 0u;
                pNewBinding->uScopeAndLocation = (uBindingPos << 8) | pTCContext->eGlobalDeclScope;
                pTCContext->pIsolatedSourceFile->vecAllGlobalBindings.append(pNewBinding);
                pTCContext->pNamespace->mapAllGlobalDeclarationsById.insert_not_present(uHash, iIdentifier, uBindingPos);
                pTCContext->pNamespace->mapAllBindingsInclUsing.insert_not_present(uHash, iIdentifier, pNewBinding);
                if (pTCContext->eGlobalDeclScope != EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
                    Assert_(pTCContext->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PACKAGE);
                    pTCContext->pNamespace->mapAccessibleDeclarationsById.insert_not_present(uHash, iIdentifier, uBindingPos);
                    pTCContext->pNamespace->mapAccessibleBindingsInclUsing.insert_not_present(uHash, iIdentifier, pNewBinding);
                }
                pTCContext->setOfNewlyDeclaredIdentifiers.insertHashed(uHash, iIdentifier);

                if (0u == (pTCStatement->pAstStatement->uFlags & STATEMENT_IS_PAN_DIRECTIVE_VALIDLY_WITHOUT_CHILD)) {
                
                    Assert_(pTCStatement->pChildBlock);
                    u64 uTagged = reinterpret_cast<u64>(pTCStatement->pChildBlock);
                    Assert_(uTagged & 0x01uLL); // tagged-ptr with 1 as lsb is indication that the child block has not yet been spawned
                    // And we spawn it now...
                    pTCStatement->pChildBlock = init_new_child_block_of_pan_from_tagged(uTagged, pTCContext);

                    // then we'll spawn a *new*, independent context, with *its* namespace as the new one,
                    //   since we need all TC task to be assigned to a single namespace.
                    // Our very context itself will continue next statement at *current* block, which should happen to be a valid #endnamespace,
                    //   provided preparser has done its verification job. By doing that, our current context will plow grass under the new
                    //   namespace child's feet when it reaches same #endnamespace, which will very conveniently instruct it to stop.
                    TCContext* pNewCtxForNamespace = (TCContext*)alloc_from(pTCContext->pIsolatedSourceFile->localArena,
                        sizeof(TCContext), alignof(TCContext));
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL9_VERBOSE, EventREPT_CUSTOM_HARDCODED(
                        "Allocated new TC Task 0x%llx for typechecking contents within a new #namespace block for namespace %u in current file",
                        reinterpret_cast<u64>(pNewCtxForNamespace), u64(pNewNamespace->uRegistrationIndex)), pTCContext->pWorker);
                    *pNewCtxForNamespace = *pTCContext;
                    pNewCtxForNamespace->pNamespace = pNewNamespace;
                    pNewCtxForNamespace->setOfNewlyDeclaredIdentifiers = {}; // will be init later, at task start
                    pNewCtxForNamespace->uSizeOfVecUsingAccessibleEnumBefore = 0u;
                    pNewCtxForNamespace->uSizeOfVecUsingAccessibleNamespaceBefore = 0u;
                    pNewCtxForNamespace->uSizeOfVecUsingAllEnumBefore = 0u;
                    pNewCtxForNamespace->uSizeOfVecUsingAllNamespaceBefore = 0u;
                    pNewCtxForNamespace->uSizeOfVecChildrenNamespacesBefore = 0u;
                    pNewCtxForNamespace->uSizeOfVecIncludedStructLikeBefore = 0u;
                    pNewCtxForNamespace->pCurrentBlock = pTCStatement->pChildBlock;
                    pNewCtxForNamespace->bResumingCurrentStatement = 0u;
                    pNewCtxForNamespace->uFlags = 0u;
                    pTCContext->pIsolatedSourceFile->tvecTCTasksToLaunchByPrio[pNewCtxForNamespace->eTaskPrio].append(pNewCtxForNamespace);
                    if (pNewCtxForNamespace->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
                        pNewNamespace->uCountGlobalPrivateTasks = 1u;
                        pNewNamespace->uTCProgress = ESourceCompState::ESOURCE_COMP_STATE_DONE_ACCESSIBLE_DISCOVERY;
                    } else { Assert_(pNewCtxForNamespace->eGlobalDeclScope == EScopeKind::SCOPEKIND_GLOBAL_PACKAGE);
                        pNewNamespace->uCountGlobalAccessibleTasks = 1u;
                        pNewNamespace->uTCProgress = ESourceCompState::ESOURCE_COMP_STATE_NOT_STARTED;
                    }
                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                } else {
                    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                        "Pan-namespace statement is with empty child block"), pTCContext->pWorker);
                    pNewNamespace->uTCProgress = ESourceCompState::ESOURCE_COMP_STATE_DONE_ALL_DISCOVERY;
                    return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
                }
            } else {
                return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                    "typecheck_pan_statement() : pan-namespace is only allowed at global scope");
            }
        } break;

        case ETOK_PAN_ENDPRIVATE:
        case ETOK_PAN_ENDNAMESPACE:
        {
            if (is_ctx_global(pTCContext)) {
                BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
                    "Typechecking pan-endprivate or endnamespace statement => NOOP, success"), pTCContext->pWorker);
                return set_node_typecheck_notanexpr_success(pMainNode->pTCNode);
            } else {
                // Should not happen in isolation from an associated pan-private or pan-namespace also-error... since in theory caught
                //   by postparser if not properly paired
                // => CLEANUP: make that silent ?
                return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                    "typecheck_pan_statement() : pan-endprivate / pan-endnamespace are only allowed at global scope");
            }
        } break;
        default:
            return_error(pMainNode, pTCStatement, pTCContext, FERR_OTHER,
                "typecheck_pan_statement() : unexpected pan-token in pan-special category");
    }
}

#endif // LOCLIB_TC_PAN_STATEMENTS_H_
  