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

#ifndef LOCLIB_TC_TYPE_CTORS_H_
#define LOCLIB_TC_TYPE_CTORS_H_

#include "LocLib_TypeCheckerTypes.h"
#include "LocLib_TypeCheckerCore.h"
#include "LocLib_TypeCheckerBase.h"
#include "LocLib_TC_Casts.h"

// TODO : remove dependency to crt
#include <cstdio>
#include <cstdlib>

local_func ETCResult typecheck_arraylike_decl(TmpTCNode* pExpr, TmpTCNode* pInsideBrackets, const TypeInfo* pBaseType,
    TCStatement* pTCStatement, TCContext* pTCContext, UpwardsInference inferredFromBelow)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Table-Decl-Like expression"), pTCContext->pWorker);

    Assert_(pInsideBrackets);

    // first, check what's inside those brackets

    //
    // TODO: think about macro-expansion possibilities over all that...
    //

    if (u8(pInsideBrackets->pTCNode->ast.uNodeKindAndFlags) == ENODE_ATOMICEXPR_SPECIAL ||
        u8(pInsideBrackets->pTCNode->ast.uNodeKindAndFlags) == ENODE_SUBEXPR_WRAPPER) {

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("special node or wrapper within []' => non-static array"), pTCContext->pWorker);

        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_arraylike_decl() : non-static array not yet implemented");

    } else { // otherwise : expects expression with known constant, positive integral value

        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("other within []' => assumed static-array-type declaration"), pTCContext->pWorker);

        // => ... and TC whole expression as static array type declaration
        ETCResult insideCheck = typecheck_expression(pInsideBrackets, pTCStatement, pTCContext,
            EExpectedExpr::EXPECT_CONSTANT, UpwardsInference{});
        success_or_return_wait_or_error(insideCheck, pExpr->pTCNode);
        Assert_(is_node_already_typechecked(pInsideBrackets->pTCNode));
        Assert_(is_value_tc_const(pInsideBrackets->pIntrinsicValue));
        if (get_type_kind(pInsideBrackets->pIntrinsicValue->pType) != ETypeKind::ETYPEKIND_INTEGRAL) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_NATURAL_NUMBER,
                "typecheck_arraylike_decl() : static array declaration requires a natural number as element count");
        }
        if (is_known_integral_outside_range_returning_reason((const TypeInfo_Integral*)pInsideBrackets->pIntrinsicValue->pType,
                pInsideBrackets->pIntrinsicValue->info, (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_INT], pTCContext)) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_NO_HIGHER_THAN_32_BITS,
                "typecheck_arraylike_decl() : static array declaration requires element count param to fit in a 32b int");
        }
        ETCResult eAsInt32 = do_implicit_cast(pInsideBrackets, g_pCoreTypesInfo[ECORETYPE_INT], pTCStatement, pTCContext, EExpectedExpr::EXPECT_CONSTANT);
        success_or_return_wait_or_error(eAsInt32, pExpr->pTCNode);
        Assert_(is_node_already_type_casted(pInsideBrackets->pTCNode));
        Assert_(is_value_tc_const(pInsideBrackets->pFinalValue));
        Assert_(is_value_known_non_nyka(pInsideBrackets->pFinalValue));
        i32 iSizeValue = i32(ir_get_u32_value_from_known(pInsideBrackets->pFinalValue->info));
        if (iSizeValue < 0) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_NATURAL_NUMBER,
                "typecheck_arraylike_decl() : static array declaration requires a non-negative number as slot-count");
        }
        u32 uElemCount = u32(iSizeValue);
        if (uElemCount > MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE) {
            return_error(pExpr, pTCStatement, pTCContext, CERR_TYPE_TOO_LARGE,
                "typecheck_arraylike_decl() : max byte count AND elem count for static array type is below 1M");
        }
        check_type_footprint_availability_may_return_wait_or_error(pBaseType, pExpr, pTCStatement, pTCContext,
            "typecheck_arraylike_decl() : Cannot make a static-array on");
        return tc_make_array_type_as(pExpr, pBaseType, uElemCount, pTCStatement, pTCContext);
    }
}

#define MAX_NODES_IN_LARGE_LIST     0x0010'0000u    // 1M

// This function is distinct from 'typecheck_possible_expr_list' in that:
// - it can have an (almost) arbitrarily high number of nodes
// - multi-invocs are not allowed here
// - last list_node may not have a primary child (when there is an allowed, dangling last comma in source code)
local_func ETCResult tc_values_in_node_list_to(TmpArray<TmpTCNode>* ioArray, u32 uStartingNodeIndex, TCStatement* pTCStatement, 
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    u32 uCurrentNodeIndex = uStartingNodeIndex;

    // TODO: allow for totally vanishing node-count-macros ?

when_macro_start_again:
    {
        TCNode* pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
        while (u8(pCurrentNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPRLIST_NODE) {
            u32 uNextNodeIndex = pCurrentNode->ast.uSecondaryChildNodeIndex;

            u32 uChildNodeIndex = pCurrentNode->ast.uPrimaryChildNodeIndex;
            if (uChildNodeIndex == INVALID_NODE_INDEX) {
                if (uNextNodeIndex == INVALID_NODE_INDEX) {
                    TmpTCNode currentForErrReport = init_tmp_tc_node(uCurrentNodeIndex, pTCStatement, pTCContext);
                    return_error((&currentForErrReport), pTCStatement, pTCContext, FERR_UNEXPECTED_SYNTAX,
                        "tc_values_in_node_list_to() : node with neither sibling nor payload ???");
                } else {
                    // Allowed dangling comma case ??? CLEANUP: Maybe keep only one of the two...
                    // Here, this is a "last" list-node (holding the comma and payload) without a valid child.
                    platform_log_debug("!!! tc_values_in_node_list_to() : validly parsed dangling comma case seems subpar here", true);
                    return ETCResult::ETCR_SUCCESS;
                }
            }

            ioArray->append(TmpTCNode{});
            TmpTCNode* pChild = &ioArray->last();  // may get invalidated afterwards, but not for the duration of our treatment this iteration
            pChild->uNodeIndexInStatement = uChildNodeIndex;
            pChild->pTCNode = pTCStatement->vecNodes[uChildNodeIndex];
            if (ioArray->size() >= MAX_NODES_IN_LARGE_LIST) {
                return_error(pChild, pTCStatement, pTCContext, CERR_TOO_MANY_NODES_IN_LARGE_LIST,
                    "tc_values_in_node_list_to() : too many expr nodes in expr list");
            }

            u8 uNodeKind = u8(pChild->pTCNode->ast.uNodeKindAndFlags);
            Assert_(uNodeKind != ENodeKind::ENODE_EXPRLIST_NODE);
            if (uNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
                Assert_(u8(pChild->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_INVOCATION);
                // proc invocations are to be treated separately, since they could very well be macros and require expansion...
                // furthermore, they may expand to multiple elements in a list on the right or left side of some statements.
                bool bWasMacroExpansion = false;
                ETCResult checkChild = typecheck_invocation_form(pChild, pTCStatement,
                    pTCContext, eExpectation, inferredFromBelow, uCurrentNodeIndex,
                    0, 0, EInvocFormResultCount::EINVOC_RETURNS_ONE, &bWasMacroExpansion);
                if (checkChild == ETCResult::ETCR_SUCCESS) {
                    if (bWasMacroExpansion) {
                        ioArray->pop_last();
                        goto when_macro_start_again;
                    }
                } else
                    return checkChild;
            } else {
                ETCResult checkChild = typecheck_any_non_invoc_expression(pChild, uNodeKind, pTCStatement,
                    pTCContext, eExpectation, inferredFromBelow);
                if (checkChild != ETCResult::ETCR_SUCCESS)
                    return checkChild;
            }

            if (uNextNodeIndex != INVALID_NODE_INDEX) {
                uCurrentNodeIndex = uNextNodeIndex;
                pCurrentNode = pTCStatement->vecNodes[uCurrentNodeIndex];
            } else {
                // Allowed dangling comma case ??? CLEANUP: Maybe keep only one of the two...
                // Here, this is a list-node (holding the comma and payload) without a sibling. Seems sensible.
                platform_log_debug("tc_values_in_node_list_to() : validly parsed dangling comma case ends 'tc_values_in_node_list' iteration", true);
                return ETCResult::ETCR_SUCCESS;
            }
        }

        Assert_(pCurrentNode);
        Assert_(u8(pCurrentNode->ast.uNodeKindAndFlags) != ENodeKind::ENODE_EXPRLIST_NODE);

        ioArray->append(TmpTCNode{});
        TmpTCNode* pLast = &ioArray->last();
        pLast->uNodeIndexInStatement = uCurrentNodeIndex;
        pLast->pTCNode = pTCStatement->vecNodes[uCurrentNodeIndex];
        // CLEANUP: should add yet another check for size ??? maybe not.

        u8 uNodeKind = u8(pLast->pTCNode->ast.uNodeKindAndFlags);
        Assert_(uNodeKind != ENodeKind::ENODE_EXPRLIST_NODE);
        if (uNodeKind == ENodeKind::ENODE_EXPR_INVOCATION_FORM) {
            Assert_(u8(pLast->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_INVOCATION);
            // proc invocations are to be treated separately, since they could very well be macros and require expansion...
            // furthermore, they may expand to multiple elements in a list on the right or left side of some statements.
            bool bWasMacroExpansion = false;
            ETCResult checkLast = typecheck_invocation_form(pLast, pTCStatement,
                pTCContext, eExpectation, inferredFromBelow, INVALID_NODE_INDEX,
                0, 0, EInvocFormResultCount::EINVOC_RETURNS_ONE, &bWasMacroExpansion);
            if (checkLast == ETCResult::ETCR_SUCCESS) {
                if (bWasMacroExpansion) {
                    ioArray->pop_last();
                    goto when_macro_start_again;
                }
            } else
                return checkLast;
        } else {
            ETCResult checkLast = typecheck_any_non_invoc_expression(pLast, uNodeKind, pTCStatement,
                pTCContext, eExpectation, inferredFromBelow);
            if (checkLast != ETCResult::ETCR_SUCCESS)
                return checkLast;
        }
        return ETCResult::ETCR_SUCCESS;
    }
}

local_func void emit_runtime_representation_from_known_const_to(u8* pDest, u32 uByteOffset, u32 uByteSize,
    const TypeInfo* pKnownType, const IRInfo& knownInfo, CompilationContext* pContext)
{
    Assert_(irflag_is_known_non_nyka(knownInfo.uIRandMetaFlags));
    if (irflag_is_known_embd(knownInfo.uIRandMetaFlags)) {
        Assert(0u == (get_ir_format(pKnownType) & 0xF8u), "Embedded Floating-point and/or HVEC decoding not yet implemented"); // TODO
        const u8* pEmbdSrc = (const u8*)&(knownInfo.metaValue.knownValue.uEmbeddedValue);
        Assert_(uByteOffset + uByteSize <= 8u);
        memcpy(pDest, pEmbdSrc + uByteOffset, uByteSize);
    } else {
        memcpy(pDest, knownInfo.metaValue.knownValue.pPtrToRawData + uByteOffset, uByteSize);
    }
}

local_func ETCResult typecheck_array_literal(TmpTCNode* pExpr, const TypeInfo* pElemType, bool bSoftConstraint,
    TCStatement* pTCStatement, TCContext* pTCContext, EExpectedExpr eExpectation)
{
    TmpTCNode contentsWrapper = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
    Assert_(u8(contentsWrapper.pTCNode->ast.uNodeKindAndFlags) == ENODE_SUBEXPR_WRAPPER);
    if (contentsWrapper.pTCNode->ast.uPrimaryChildNodeIndex != INVALID_NODE_INDEX) {
        Arena tmpArena = pTCContext->pWorker->tmpArena;
        ArenaRefPoint beforeTmp = get_arena_ref_point(tmpArena);
        defer { reset_arena_no_release_to(beforeTmp, tmpArena); };
        TmpStackOptiArray<TmpTCNode, 16> vecValueNodes;  // 16 is already 640 Bytes on stack, and seems common enough (eg 4x4 matrices)
        vecValueNodes.init(FireAndForgetArenaAlloc(tmpArena));

        ETCResult checkList = tc_values_in_node_list_to(&vecValueNodes, contentsWrapper.pTCNode->ast.uPrimaryChildNodeIndex,
            pTCStatement, pTCContext, eExpectation, infer_type(pElemType));
        success_or_return_wait_or_error(checkList, pExpr->pTCNode);

        u32 uCountValues = vecValueNodes.size();
        Assert_(uCountValues);
        const TypeInfo* pResultingType = pElemType;
        if (bSoftConstraint) {
            // Finding common type between all.
            if (!pResultingType)
                pResultingType = vecValueNodes[0].pIntrinsicValue->pType;

            bool bAllNumeric = true, bAllIntegral = true;
            bool unused1, unused2;
            get_common_type_flags(pResultingType, &bAllNumeric, &bAllIntegral, &unused1, &unused2);
            const TypeInfo_FloatingPoint* pLastFoundFP = 0;
            if (get_type_kind(pResultingType) == ETypeKind::ETYPEKIND_FLOATINGPOINT)
                pLastFoundFP = (const TypeInfo_FloatingPoint*)pResultingType;
            const TypeInfo_Integral* pLastFoundRaw = 0;
            if (get_type_kind(pResultingType) == ETypeKind::ETYPEKIND_INTEGRAL && is_raw_integral_(pResultingType))
                pLastFoundRaw = (const TypeInfo_Integral*)pResultingType;

            const TypeInfo** tAllTypes = (const TypeInfo**)alloc_from(tmpArena,
                sizeof(const TypeInfo*)*uCountValues, alignof(const TypeInfo*));

            for (u32 uValue = 0; uValue < uCountValues; uValue++) {
                Assert_(is_node_already_typechecked(vecValueNodes[uValue].pTCNode));
                Assert_(vecValueNodes[uValue].pIntrinsicValue);
                const TypeInfo* pTypeThere = vecValueNodes[uValue].pIntrinsicValue->pType;
                tAllTypes[uValue] = pTypeThere;
                bool bIsNumeric, bIsIntegral, bIsVecOfNumeric, bIsVecOfIntegral;
                get_common_type_flags(pTypeThere, &bIsNumeric, &bIsIntegral, &bIsVecOfNumeric, &bIsVecOfIntegral);
                bAllNumeric &= bIsNumeric;
                bAllIntegral &= bIsIntegral;
                if (bIsNumeric && !bIsIntegral)
                    pLastFoundFP = (const TypeInfo_FloatingPoint*)pTypeThere;
                else if (!bIsNumeric && bIsIntegral)
                    pLastFoundRaw = (const TypeInfo_Integral*)pTypeThere;
            }

            if (bAllNumeric) {
                if (bAllIntegral) {
                    // integral result : check same signedness and results in greater type when specified (compint if all compint).
                    Assert_(get_type_kind(pResultingType) == ETypeKind::ETYPEKIND_INTEGRAL);
                    const TypeInfo_Integral* asIntegralResult = (const TypeInfo_Integral*)pResultingType;
                    Assert_(!is_raw_integral(asIntegralResult)); // otherwise not numeric
                    for (u32 uValue = 0; uValue < uCountValues; uValue++) {
                        Assert_(get_type_kind(tAllTypes[uValue]) == ETypeKind::ETYPEKIND_INTEGRAL);
                        const TypeInfo_Integral* asIntegralThere = (const TypeInfo_Integral*)tAllTypes[uValue];
                        Assert_(!is_raw_integral(asIntegralThere)); // otherwise not numeric
                        if (!is_compint(asIntegralResult) && !is_compint(asIntegralThere) &&
                            is_signed(asIntegralResult) != is_signed(asIntegralThere)) {
                            return_error(pExpr, pTCStatement, pTCContext, CERR_SIGNED_UNSIGNED_MISMATCH_WITHIN_ARRAY_LITERAL,
                                "typecheck_array_literal() : signed/unsigned mismatch between elements");
                        }
                        u16 uTypeErr = 0;
                        asIntegralResult = get_resulting_integral_type(asIntegralResult, asIntegralThere, pTCContext, &uTypeErr);
                        if (uTypeErr) {
                            return_error(pExpr, pTCStatement, pTCContext, uTypeErr,
                                "typecheck_array_literal() : find common integral type failed");
                        } else Assert_(asIntegralResult);
                    }
                    pResultingType = asIntegralResult;
                    if (is_compint(asIntegralResult)) // we force an INT array instead of compint if we found *only* compint.
                        pResultingType = when_no_explicit_cast_get_runtime_type(asIntegralResult, pTCContext);
                    // Note: allowing 'true comptime' int arrays simply requires an array literal with explicit type.
                    //   (which does not follow that 'bSoftConstraint' path here...)

                } else {
                    // all numeric, but not all integral => at least one floating point
                    //   floating-point result : results in greater type when specified (float if all float_lit)
                    Assert_(pLastFoundFP);
                    const TypeInfo_FloatingPoint* asFPResult = get_resulting_fp_type(pResultingType, pLastFoundFP, pTCContext);
                    for (u32 uValue = 0; uValue < uCountValues; uValue++) {
                        asFPResult = get_resulting_fp_type(asFPResult, tAllTypes[uValue], pTCContext);
                    }
                    pResultingType = asFPResult;
                    if (is_float_literal(asFPResult)) // we force a F64 array instead of float_lit if we found *only* float_lit.
                        pResultingType = when_no_explicit_cast_get_runtime_type(asFPResult, pTCContext);
                    // Note: allowing 'true comptime' float arrays simply requires an array literal with explicit type.
                    //   (which does not follow that 'bSoftConstraint' path here...)
                }

            } else {
                if (bAllIntegral) {
                    Assert_(pLastFoundRaw && is_raw_integral_(pLastFoundRaw));
                    // all integrals, but not all numeric => at least one raw integral
                    // => requires almost-exact match (ie representative max) BUT allow compint to stand as integral.
                    for (u32 uValue = 0; uValue < uCountValues; uValue++) {
                        Assert_(get_type_kind(tAllTypes[uValue]) == ETypeKind::ETYPEKIND_INTEGRAL);
                        const TypeInfo_Integral* asIntegralThere = (const TypeInfo_Integral*)tAllTypes[uValue];
                        if (!is_compint(asIntegralThere)) {
                            if (!is_raw_integral(asIntegralThere)) {
                                return_error(pExpr, pTCStatement, pTCContext, CERR_TYPE_MISMATCH_WITHIN_NON_NUMERIC_ARRAY_LITERAL,
                                    "typecheck_array_literal() : cannot combine raw integrals with non-raw integral types");
                            } else {
                                if (asIntegralThere != pLastFoundRaw) {
                                    return_error(pExpr, pTCStatement, pTCContext, CERR_TYPE_MISMATCH_WITHIN_NON_NUMERIC_ARRAY_LITERAL,
                                        "typecheck_array_literal() : cannot combine raw integrals with integrals of distinct types, (except compint)");
                                }
                            }
                        } // otherwise NOOP: compints are allowed and will not modify the resulting raw type.
                    }

                } else {
                    // other element types (or combinations) : requires exact match.
                    for (u32 uValue = 0; uValue < uCountValues; uValue++) {
                        if (!are_types_same(pResultingType, tAllTypes[uValue], pTCContext)) {
                            return_error(pExpr, pTCStatement, pTCContext, CERR_TYPE_MISMATCH_WITHIN_NON_NUMERIC_ARRAY_LITERAL,
                                "typecheck_array_literal() : non-numeric element types within array literals must match exactly");
                        }
                    }
                }
            }
        }

        const TypeInfo_Array* pResultingArrayType = get_array_type_to(pResultingType, uCountValues, pTCContext);

        bool bAllConst = true;
        u32 uNykasCount = 0u;
        for (u32 uValue = 0; uValue < uCountValues; uValue++) {
            TmpTCNode* pNode = vecValueNodes.at(uValue);
            Assert_(is_node_already_typechecked(pNode->pTCNode));
            Assert_(pNode->pIntrinsicValue);
            if (!is_node_already_type_casted(pNode->pTCNode)) {
                if (are_types_same(pNode->pIntrinsicValue->pType, pResultingType, pTCContext)) {
                    set_cast_success_with_same_value(pNode);
                } else {
                    ETCResult eCastResult = do_implicit_cast(pNode, pResultingType, pTCStatement, pTCContext, eExpectation);
                    success_or_return_wait_or_error(eCastResult, pExpr->pTCNode);
                }
            } else {
                recall_node_final_value_from_index(pNode, pTCStatement, pTCContext);
            }
            Assert_(is_node_already_type_casted(pNode->pTCNode));
            Assert_(pNode->pFinalValue);
            if (is_value_nyka_or_has_nyka(pNode->pFinalValue)) {
                // TODO
                // uNykasCount += ?
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_array_literal() : elements with nykas not yet implemented");
            }
            if (!is_value_tc_const(pNode->pFinalValue)) {
                bAllConst = false;
            }
        }

        NodeValue* pResultValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pResultValue->pType = pResultingArrayType;
        pResultValue->info.uIRandMetaFlags = INVALID_IR_CODE;
        u32 uMetaFlags;

        if (bAllConst) {
            uMetaFlags = IRFLAG_IS_KNOWN|IRFLAG_TC_SEMANTIC_CONST;
            if (uNykasCount) {
                uMetaFlags |= IRFLAG_HAS_NYKA;
                // TODO
                return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                    "typecheck_array_literal() : const with nykas not yet implemented");
            }
            u32 uElemAlign;
            u32 uElemRuntimeSize = get_runtime_sizeof(pResultingType, &uElemAlign);
            u8* pAsRuntimeData = alloc_from(pTCContext->pIsolatedSourceFile->localArena, uCountValues * uElemRuntimeSize, 8u); // comptime align of runtime data is always 8u
            u8* pEltRuntimeData = pAsRuntimeData;
            for (u32 uValue = 0; uValue < uCountValues; uValue++, pEltRuntimeData += uElemRuntimeSize) {
                TmpTCNode* pNode = vecValueNodes.at(uValue);
                Assert_(pNode->pFinalValue->pType == pResultingType);
                Assert_(is_value_tc_const(pNode->pFinalValue));
                Assert_(!is_value_tc_only(pNode->pFinalValue));
                Assert_(ir_is_valid_param_(pNode->pFinalValue->info.uIRandMetaFlags));
                emit_runtime_representation_from_known_const_to(pEltRuntimeData, 0, uElemRuntimeSize, pResultingType, pNode->pFinalValue->info, pTCContext);
            }
            u32 uPosOfDecl = pTCContext->pRepo->uSize;
            IREntry* pDeclEntry = ir_append_new_entry(pTCContext->pRepo);
            u8 uFormat = get_ir_format(pResultingType);
            pDeclEntry->uInstrCodeAndFormatAndFirstParam = u64(IRIT_DECLARATION)|(u64(uFormat)<<16);
            u32 uLog2OfSlotSize = get_log2_of_slot_size_from_format(uFormat);
            u32 uTotalBytes = uCountValues * uElemRuntimeSize;
            Assert_(align_to(1u << uLog2OfSlotSize, uTotalBytes) == uTotalBytes);
            u64 uSlotsCountAndAlign = (u64(uTotalBytes) >> uLog2OfSlotSize) | (u64(get_log2_of_align_bytes(pResultingType)) << 32);
            pDeclEntry->uInstrMetaFlagsAndSecondParam = u64(uMetaFlags) | (uSlotsCountAndAlign << IR_STD_PARAM_SHIFT);
            pDeclEntry->metaValue.knownValue.pPtrToRawData = pAsRuntimeData;

            pResultValue->info.uIRandMetaFlags = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uPosOfDecl) | u64(uMetaFlags);
            pResultValue->info.metaValue = pDeclEntry->metaValue;
            return set_node_typecheck_expr_success(pExpr->pTCNode);

        } else {

            // TODO : create a local variable, (maybe storing const part in one go) then emitting store for each runtime value iteratively.
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_array_literal() : not yet implemented when non-fully-const.");
        }

    } else { // no child node => empty-literal

        if (pElemType) {
            // THOUGHTS: CLEANUP: really allow zero-sized array literals anyway ??
            const TypeInfo_Array* pZeroSizedArrayType = get_array_type_to(pElemType, 0, pTCContext);
            NodeValue* pZeroSizedValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
            pZeroSizedValue->pType = pZeroSizedArrayType;
            constexpr u32 uZeroSizedFlags = IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO;
            u32 uDeclPos = ir_make_decl_entry(pTCContext->pRepo, 0u, uZeroSizedFlags, 0uLL,
                get_ir_format(pElemType), 0u, get_log2_of_align_bytes(pElemType));
            pZeroSizedValue->info.uIRandMetaFlags = ir_make_std_code(pTCContext->pRepo->uIRRepoId, uDeclPos) | u64(uZeroSizedFlags);
            pZeroSizedValue->info.metaValue.knownValue.uEmbeddedValue = 0uLL;
            return set_node_typecheck_expr_success(pExpr->pTCNode);
        } else {
            Assert_(bSoftConstraint);
            return_error(pExpr, pTCStatement, pTCContext, CERR_MISSING_TYPE_INFERRENCE,
                "typecheck_array_literal() : no upwards inferrence was available to help typecheck this non-explicitely typed, and empty, array literal.");
        }
    }
}

#endif // LOCLIB_TC_TYPE_CTORS_H_
 