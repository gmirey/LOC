#pragma once 

#ifndef LOCLIB_TC_INEDXING_H_
#define LOCLIB_TC_INEDXING_H_

#include "LocLib_TypeCheckerTypes.h"
#include "LocLib_TypeCheckerCore.h"
#include "LocLib_TypeCheckerBase.h"
#include "LocLib_IR_SolverInterface.h"
#include "LocLib_TC_Casts.h"

// TODO : remove dependency to crt
#include <cstdio>
#include <cstdlib>

// indexing and slicing forms : starting with an opening bracket following some other expression.
local_func ETCResult typecheck_indexing_expression(TmpTCNode* pExpr, TCStatement* pTCStatement,
    TCContext* pTCContext, EExpectedExpr eExpectation, UpwardsInference inferredFromBelow)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("Typechecking Node : Indexing"), pTCContext->pWorker);

    Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags) == ENodeKind::ENODE_EXPR_INDEXING);
    Assert_(u8(pExpr->pTCNode->ast.uNodeKindAndFlags >> 8) == ETOK_OPENING_BRACKET);

    Assert_(!is_node_already_typechecked(pExpr->pTCNode)); // our caller is parse_any_non_invoc_expression() and should have taken care of this
    if_expr_already_typechecked_phase1_recall_value_and_return_success(pExpr, pTCStatement, pTCContext);

    TmpTCNode baseExpr = init_tmp_tc_node(pExpr->pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    TmpTCNode indexWrapper = init_tmp_tc_node(pExpr->pTCNode->ast.uSecondaryChildNodeIndex, pTCStatement, pTCContext);
    TmpTCNode indexExpr = init_tmp_tc_node(indexWrapper.pTCNode->ast.uPrimaryChildNodeIndex, pTCStatement, pTCContext);
    ETCResult checkBase = typecheck_expression(&baseExpr, pTCStatement, pTCContext,
        eExpectation == EXPECT_ASSIGNABLE ? EXPECT_REGULAR : eExpectation, UpwardsInference{});
    success_or_return_wait_or_error(checkBase, pExpr->pTCNode);

    if (u8(indexExpr.pTCNode->ast.uNodeKindAndFlags) == ENODE_EXPR_INVOCATION_FORM) {

        // TODO: think about macro-expansion possibilities here, before checking the 'slice' at AST level, below...
        platform_log_info("*** warning : invocation-form found as param of an indexing expression. Not yet checked for macro expansion vs. 'slicing' forms");

    } else if (u8(indexExpr.pTCNode->ast.uNodeKindAndFlags) == ENODE_SUBEXPR_SLICE) {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_indexing_expression() : 'slicing' expression not yet implemented");
    }

    // TODO: could maybe be replaced by 'typecheck_any_non_invoc_expression()' once we take care of the warning above...
    ETCResult checkIndex = typecheck_expression(&indexExpr, pTCStatement, pTCContext,
        eExpectation == EXPECT_ASSIGNABLE ? EXPECT_REGULAR : eExpectation, UpwardsInference{});
    success_or_return_wait_or_error(checkIndex, pExpr->pTCNode);

    Assert_(is_node_already_typechecked(baseExpr.pTCNode));
    Assert_(is_node_already_typechecked(indexExpr.pTCNode));

    const TypeInfo* pUnaliasedIndexType = unalias(indexExpr.pIntrinsicValue->pType);
    if (get_type_kind(pUnaliasedIndexType) != ETypeKind::ETYPEKIND_INTEGRAL) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_INDEXING_REQUIRES_NUMERIC_INDEX,
            "typecheck_indexing_expression() : 'indexing' special-binop requires integral type");
    }
    bool bIndexSigned = is_signed_(pUnaliasedIndexType);
    // TODO: allow up to 64b for indexing slices (and handle both signednesses + error if negative)
    // TODO: allow up to 64b for indexing through pointers (and handle both signednesses, *accepting* negative).
    // TODO: also allow other index types for maps and sets...
    ETCResult eCheckCastInt = do_implicit_cast(&indexExpr, bIndexSigned ? g_pCoreTypesInfo[ECORETYPE_INT] : g_pCoreTypesInfo[ECORETYPE_NAT],
        pTCStatement, pTCContext, eExpectation);
    if (eCheckCastInt != ETCResult::ETCR_SUCCESS) {
        return_error(pExpr, pTCStatement, pTCContext, CERR_INDEXING_REQUIRES_NUMERIC_INDEX,
            "typecheck_indexing_expression() : 'index' should be castable to 32b int (signed or not)");
    }
    Assert_(is_node_already_type_casted(indexExpr.pTCNode));
    NodeValue* pIndexValue = indexExpr.pFinalValue;
    Assert_(pIndexValue->pType == g_pCoreTypesInfo[ECORETYPE_INT] || pIndexValue->pType == g_pCoreTypesInfo[ECORETYPE_NAT]);

    if (get_type_kind(baseExpr.pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_ARRAY) {

        if (is_value_tc_only(baseExpr.pIntrinsicValue)) {
            // TODO ?
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_indexing_expression() : indexing a tc-only array not (yet?) implemented");
        }

        const TypeInfo_Array* pAsArrayType = (const TypeInfo_Array*)baseExpr.pIntrinsicValue->pType;
        if (bIndexSigned) {
            IRInfo infoCheckNegative;
            EIRResult eCheckNegative = ir_emit_or_solve_ord_cmp_integral(0x02u, pIndexValue->info, info0WhenEmbeddedIntegral, 0u,
                IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoCheckNegative);
            if (eCheckNegative <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
                u8 uLessThanZero = ir_get_u8_value_from_known(infoCheckNegative);
                Assert_(uLessThanZero <= 1u);
                if (uLessThanZero) {
                    return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_NATURAL_NUMBER,
                        "typecheck_indexing_expression() : cannot index an array with a negative value");
                } // otherwise NOOP
            } else if (eCheckNegative < EIRResult::EIRR_FIRST_ERROR) {
                Assert_(eCheckNegative == EIRResult::EIRR_ENSURED_VALID_UNKNOWN);
                do_runtime_err_check(infoCheckNegative.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO,
                    0u, ERR_CHECK_INDEX_OUT_OF_RANGE, pExpr, pTCStatement, pTCContext);
            } else {
                return_error(pExpr, pTCStatement, pTCContext, u16(eCheckNegative),
                    "typecheck_indexing_expression() : check negative failed");
            }
        } // otherwise noop

        IRInfo infoBasePtr;
        IRInfo infoMaxIndex;
        if (get_array_category_type(pAsArrayType) == ARRAY_TYPE_KIND_STATIC) {
            infoMaxIndex = ir_make_info_for_int_immediate(i32(pAsArrayType->uElemCount), 0x02u);
            EIRResult eSolveBasePtr = ir_emit_or_solve_address_of(baseExpr.pIntrinsicValue->info, pTCStatement, pTCContext, &infoBasePtr);
            if (eSolveBasePtr < EIRResult::EIRR_FIRST_ERROR) {
                // NOOP, we're fine
            } else {
                return_error(pExpr, pTCStatement, pTCContext, u16(eSolveBasePtr),
                    "typecheck_indexing_expression() : emit address-of failed");
            }
        } else {
            // TODO
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_indexing_expression() : non-static array type not yet implemented");
        }

        Assert_(eExpectation != EExpectedExpr::EXPECT_CONSTANT || 0 == (infoBasePtr.uIRandMetaFlags & IRFLAG_HAS_LOCAL_NYKA));

        IRInfo infoCheckAboveLastIndex;
        EIRResult eCheckAboveLastIndex = ir_emit_or_solve_ord_cmp_integral(0x02u, pIndexValue->info, infoMaxIndex,
            IR_INSTRFLAG_CMP_OPPOSITE, IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES, bIndexSigned ?
                EIntSemantics::EINT_SEMANTIC_SIGNED : EIntSemantics::EINT_SEMANTIC_UNSIGNED,
                pTCStatement, pTCContext, &infoCheckAboveLastIndex);
        if (eCheckAboveLastIndex <= EIRResult::EIRR_ENSURED_VALID_KNOWN) {
            u8 uAboveLastIndex = ir_get_u8_value_from_known(infoCheckAboveLastIndex);
            Assert_(uAboveLastIndex <= 1u);
            if (uAboveLastIndex) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_INDEXING_OUT_OF_BOUNDS,
                    "typecheck_indexing_expression() : index out of range");
            } // otherwise NOOP
        } else if (eCheckAboveLastIndex < EIRResult::EIRR_FIRST_ERROR) {
            Assert_(eCheckAboveLastIndex == EIRResult::EIRR_ENSURED_VALID_UNKNOWN);
            do_runtime_err_check(infoCheckAboveLastIndex.uIRandMetaFlags & IR_STD_PARAM_MASK, 0x00u, IR_INSTRFLAG_BRANCH_ON_NONZERO, 0u, ERR_CHECK_INDEX_OUT_OF_RANGE,
                pExpr, pTCStatement, pTCContext);
        } else {
            return_error(pExpr, pTCStatement, pTCContext, u16(eCheckAboveLastIndex),
                "typecheck_indexing_expression() : check above last index failed");
        }

        u8 uEltFormat = get_ir_format(pAsArrayType->pElementType);
        u32 uEltAlignLog2 = get_log2_of_align_bytes(pAsArrayType->pElementType);
        u32 uEltSlotCount = get_slots_count(pAsArrayType->pElementType);
        u32 uEltBytesCount = get_runtime_sizeof(pAsArrayType->pElementType);

        IRInfo infoPtrOffset;
        EIRResult eOffsetResult = ir_emit_or_solve_ptr_offset(uEltAlignLog2, infoBasePtr, 0x02u, pIndexValue->info,
            uEltBytesCount, IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF, EIntSemantics::EINT_SEMANTIC_SIGNED, pTCStatement, pTCContext, &infoPtrOffset);
        if (eOffsetResult >= EIRResult::EIRR_FIRST_ERROR) {
            return_error(pExpr, pTCStatement, pTCContext, u16(eOffsetResult),
                "typecheck_indexing_expression() : emit ptr offset failed");
        }
        IRInfo infoResult;
        EIRResult eDerefResult = ir_emit_or_solve_deref(infoPtrOffset, uEltFormat, uEltAlignLog2, uEltSlotCount, uEltBytesCount,
            eExpectation == EExpectedExpr::EXPECT_ASSIGNABLE ? IR_INSTRFLAG_IS_ASSIGNABLE : 0u, pTCStatement, pTCContext, &infoResult);
        if (eDerefResult >= EIRResult::EIRR_FIRST_ERROR) {
            return_error(pExpr, pTCStatement, pTCContext, u16(eDerefResult),
                "typecheck_indexing_expression() : emit deref failed");
        }
        Assert_(irflag_is_tc_referencable(infoResult.uIRandMetaFlags));
        Assert_(!ir_is_immediate(infoResult.uIRandMetaFlags & IR_STD_PARAM_MASK));
        if (irflag_is_known_or_nyka(infoResult.uIRandMetaFlags) && is_value_tc_const(baseExpr.pIntrinsicValue)) {
            infoResult.uIRandMetaFlags |= IRFLAG_TC_SEMANTIC_CONST;
        }
        NodeValue* pEltValue = alloc_value_for(pExpr, EValueSlotOnNode::ENODEVALUESLOT_INTRINSIC, pTCStatement, pTCContext);
        pEltValue->pType = pAsArrayType->pElementType;
        pEltValue->info = infoResult;
        return set_node_typecheck_expr_success(pExpr->pTCNode);

    } else if (get_type_kind(baseExpr.pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_POINTER) {

        if (is_value_tc_only(baseExpr.pIntrinsicValue)) {
            // TODO ?
            return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
                "typecheck_indexing_expression() : indexing a tc-only pointer not (yet?) implemented");
        }

        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_indexing_expression() : pointer base type not yet implemented");

        /*
        ETCResult checkAsOffset = typecheck_ptr_offset_op(pExpr, ETOK_ADD, &baseExpr, &indexExpr,
                                                          pTCStatement, pTCContext, eExpectation, true);
        if (LIKELY(checkAsOffset == ETCResult::ETCR_SUCCESS)) {
            // NOOP
        } else
            return checkAsOffset;

        EBoolTransform boolTransform = get_if_bool_transform(inferredFromBelow);
        if (boolTransform == EBoolTransform::NO_BOOL_TRANSFORM) {
            if (is_value_const(pExpr->pIntrinsicValue) && eExpectation == EXPECT_ASSIGNABLE) {
                return_error(pExpr, pTCStatement, pTCContext, CERR_EXPECTED_ASSIGNABLE_EXPRESSION,
                    "typecheck_indexing_expression() : TC-result of pointer deref at index is const... cannot assign to it");
            }
            // a pointer deref is always referencable
            pExpr->pTCNode->ast.uNodeKindAndFlags |= ENODEKINDFLAG_IS_INTRINSIC_USER_REFERENCEABLE;
            return set_node_typecheck_expr_success(pExpr->pTCNode, EBoolTransform::NO_BOOL_TRANSFORM);
        } else {
            // ... unless coerced to a boolean *rvalue*
            Assert_(eExpectation != EXPECT_ASSIGNABLE);
            return_application_of_bool_transform(pExpr, pTCStatement, pTCContext, boolTransform, eExpectation);
        }
        */

    } else if (get_type_kind(baseExpr.pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_OTHERCORE && (baseExpr.pIntrinsicValue->pType->_coreFlags & OTHERCOREFLAG_IS_STRING)) {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_indexing_expression() : string-related base type not yet implemented");
    } else if (get_type_kind(baseExpr.pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_SET) {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_indexing_expression() : set base type not yet implemented");
    } else if (get_type_kind(baseExpr.pIntrinsicValue->pType) == ETypeKind::ETYPEKIND_MAP) {
        // TODO
        return_error(pExpr, pTCStatement, pTCContext, FERR_NOT_YET_IMPLEMENTED,
            "typecheck_indexing_expression() : map base type not yet implemented");
    } else {
        return_error(pExpr, pTCStatement, pTCContext, CERR_TYPE_NOT_INDEXABLE,
            "typecheck_indexing_expression() : 'indexing' special-binop not available against base type");
    }

}

#endif // LOCLIB_TC_INEDXING_H_
  