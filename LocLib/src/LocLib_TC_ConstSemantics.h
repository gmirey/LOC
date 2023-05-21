#pragma once 

#ifndef LOCLIB_TC_CONST_SEMANTICS_H_
#define LOCLIB_TC_CONST_SEMANTICS_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "LocLib_TypeCheckerCore.h"


local_func bool has_stringview_const_semantics(NodeValue* pValue, MetaValueIR metaValue, TCContext* pTCContext, bool bAllowNonConstOnFlags,
    StringView* optStringResult)
{
    Assert_(unalias(pValue->pType) == g_pCoreTypes[ECORETYPE_STRINGVIEW] || unalias(pValue->pType) == g_pCoreTypes[ECORETYPE_OWNEDSTRING]);
    Assert_(get_ir_format(pValue->pType) == 0x03u); // all string-related types should be r64-based.
    
    u32 uAddressFlags; MetaValueIR metaAddressValue;
    ir_virtual_solve_reinterp(0x03u, pValue->uIR, 0x03u, pTCContext, &uAddressFlags, &metaAddressValue);

    u32 uByteLengthFlags; MetaValueIR metaByteLengthValue;
    ir_virtual_solve_reinterp_at_offset(0x02u, 8u, pValue->uIR, 0x03u, pTCContext, &uByteLengthFlags, &metaByteLengthValue);

    u32 uStringFlagsFlags; MetaValueIR metaStringFlagsValue;
    ir_virtual_solve_reinterp_at_offset(0x02u, 12u, pValue->uIR, 0x03u, pTCContext, &uStringFlagsFlags, &metaStringFlagsValue);

    if (is_ir_flag_indicating_known_const(uByteLengthFlags) && (bAllowNonConstOnFlags || is_ir_flag_indicating_known_const(uStringFlagsFlags))) {
        u32 uByteLength = ir_interp_known_as_u32(uByteLengthFlags, metaByteLengthValue, pTCContext);

        if (is_ir_flag_indicating_const_nyka(uAddressFlags)) {
            u32 uPointedRangeFlags;
            ir_virtual_solve_flags_only_from_nyka_and_range(uAddressFlags, metaAddressValue, 0x00u, uByteLength, pTCContext, &uPointedRangeFlags);
            bool bKnownConst = is_ir_flag_indicating_known_const(uPointedRangeFlags);
            if (bKnownConst && *optStringResult) {
                if (is_ir_flag_indicating_known_const(uStringFlagsFlags)) {
                    optStringResult->flags = ir_interp_known_as_u32(uStringFlagsFlags, metaStringFlagsValue, pTCContext);
                } else {
                    // for now bypass as flags to 0... TODO: possibly raise a warning, possibly also retinterp constraints ???
                    optStringResult->flags = 0;
                }
                optStringResult->uByteLength = uByteLength;
                optStringResult->start = ir_interp_nyka_as_ptr_to_runtime_like_data(uAddressFlags, metaAddressValue, pTCContext);
            }
            return bKnownConst;

        } else if (is_ir_flag_indicating_known_embedded_const(uAddressFlags) && metaAddressValue.knownValue.uEmbeddedValue == 0uLL) {
            // Case of a const stringview to nullptr...
            if (*optStringResult) {
                optStringResult->flags = ir_interp_known_as_u32(uStringFlagsFlags, metaStringFlagsValue, pTCContext);
            } else {
                // for now bypass as flags to 0... TODO: possibly raise a warning, possibly also retinterp constraints ???
                optStringResult->flags = 0;
            }
            optStringResult->uByteLength = uByteLength;
            optStringResult->start = 0;
            return true;
        }
    }
    return false;
}

local_func bool has_compact_string_const_semantics(NodeValue* pValue, MetaValueIR metaValue, TCContext* pTCContext, bool bAllowNonConstOnFlags,
    StringView* optStringResult)
{
    Assert_(unalias(pValue->pType) == g_pCoreTypes[ECORETYPE_COMPACT_STRING] || unalias(pValue->pType) == g_pCoreTypes[ECORETYPE_COMPACT_OWNEDSTRING]);
    Assert_(get_ir_format(pValue->pType) == 0x03u); // all string-related types should be r64-based.

    if (is_ir_flag_indicating_const_nyka(pValue->uFlags)) {

        u32 uAddressMinus4Flags{}; MetaValueIR metaAddressMinus4Value{};
        ir_virtual_solve_sub(0x03u, pValue->uIR, ir_make_positive_int_immediate(4u), &uAddressMinus4Flags, &metaAddressMinus4Value);
        NodeValue uAddressMinus8Flags{}; MetaValueIR metaAddressMinus8Value{};
        ir_virtual_solve_sub(0x03u, pValue->uIR, ir_make_positive_int_immediate(8u), &uAddressMinus8Flags, &metaAddressMinus8Value);

        Assert_(is_ir_flag_indicating_const_nyka(uAddressMinus4Flags));
        Assert_(is_ir_flag_indicating_const_nyka(uAddressMinus8Flags));

        u32 uByteLengthFlags; MetaValueIR metaByteLengthValue;
        ir_virtual_retrieve_slot_at_nyka(uAddressMinus4Flags, metaAddressMinus4Value, 0x02u, pTCContext, &uByteLengthFlags, &metaByteLengthValue);
        
        u32 uStringFlagsFlags; MetaValueIR metaStringFlagsValue;
        ir_virtual_retrieve_slot_at_nyka(uAddressMinus8Flags, metaAddressMinus8Value, 0x02u, pTCContext, &uStringFlagsFlags, &metaStringFlagsValue);

        if (is_ir_flag_indicating_known_const(uByteLengthFlags) && (bAllowNonConstOnFlags || is_ir_flag_indicating_known_const(uStringFlagsFlags))) {
            u32 uByteLength = ir_interp_known_as_u32(uByteLengthFlags, metaByteLengthValue, pTCContext);
            u32 uPointedRangeFlags;
            ir_virtual_solve_flags_only_from_nyka_and_range(pValue->uFlags, metaValue, 0x00u, uByteLength, pTCContext, &uPointedRangeFlags);
            bool bKnownConst = is_ir_flag_indicating_known_const(uPointedRangeFlags);
            if (bKnownConst && *optStringResult) {
                if (is_ir_flag_indicating_known_const(uStringFlagsFlags)) {
                    optStringResult->flags = ir_interp_known_as_u32(uStringFlagsFlags, metaStringFlagsValue, pTCContext);
                } else {
                    // for now bypass as flags to 0... TODO: possibly raise a warning, possibly also retinterp constraints ???
                    optStringResult->flags = 0u;
                }
                optStringResult->uByteLength = uByteLength;
                optStringResult->start = ir_interp_nyka_as_ptr_to_runtime_like_data(pValue->uFlags, metaValue, pTCContext);
            }
            return bKnownConst;
        }

    } else if (is_ir_flag_indicating_known_embedded_const(pValue->uFlags) && metaValue.knownValue.uEmbeddedValue == 0uLL) {
        // Case of a null ffString...
        if (*optStringResult) {
            optStringResult->flags = 0u;
            optStringResult->uByteLength = 0u;
            optStringResult->start = 0;
        }
        return true;
    }

    return false;
}

local_func_inl bool has_compact_string_const_semantics_to_ff(NodeValue* pValue, MetaValueIR metaValue, TCContext* pTCContext, FFString* outStringResultFF)
{
    if (has_compact_string_const_semantics(pValue, metaValue, pTCContext, false, 0)) {
        outStringResultFF->pStart = ir_interp_nyka_as_ptr_to_runtime_like_data(pValue->uFlags, metaValue, pTCContext);
        return true;
    } else
        return false;
}




#endif // LOCLIB_TC_CONST_SEMANTICS_H_


