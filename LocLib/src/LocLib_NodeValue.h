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

#ifndef LOCLIB_NODE_VALUE_H_
#define LOCLIB_NODE_VALUE_H_


#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"

#include "LocLib_IR_Info.h"

/*
    ********************************************
    Explanations about the 'NodeValue' concept
    ********************************************
 
    A NodeValue represents a value to the typechecker:
    ----------------------------------------------------
     * Every typechecked node which is recognized as a valid expression should have an associated 'NodeValue'
     * All 'NodeValue's have a known type (except in very special and well-bounded circumstances, not affecting other parts
            of the typechecker, where a null-type may be used to represent the 'sink' or the special 'uninitialized' initial-value)
     * Oftentimes nodes even have two 'NodeValue's associated to them: one for the 'intrinsic' value they represent, and
            one for the value once implicit-casted before its usage in a derived expression.
     * 'NodeValue's are usually accessed through pointers during active typechecking, and are usually allocated from the
            localArena of an associated source file. However, stored 'TCNodes' only have an index to a statementwise array of
            pointers, if typecheck was to be interrupted and restarted (or accessed from outside for report or an helping IDE).
            Note that those pointers can indifferently point to a locally allocated 'NodeValue', or to a 'ValueBinding' (a struct
            derived from NodeValue). Within a NodeValue flags is the necessary info to discriminate ValueBindings when it matters.
     * 'ValueBinding' is a somewhat more involved structure: it usually holds the IR to a declaration, as well as meta-flags
            indicating that it *is* a binding. In case of a *const* binding, its meta-flags and meta-value are same as the
            meta-flags and meta-value found directly on the instruction, representing the constant value. In case of a *var* binding,
            however, meta-flags and meta-value will *not* be reported to the 'ValueHolder' and 'ValueBinding' fields (even though
            there are possibly meaningful meta-flags and meta-value on the instruction also, since in that case they represent the
            *initial* value of a *global* variable. As for local variables, members or params, they do not have an 'initial' value
            as far as the IR instruction is concerned).

    Cases where NodeValue does *not* have a type (pType == 0):
    - when it represents the sink in the LHV of a declaration or assignment ; additionally, its IR is then 0
    - when it represents the special "uninitialized" value in the RHV of a local declaration ; additionally, its IR is then 0
    - CLEANUP: Not used any more: when we're in the process of declaring a new Binding, but the RHV has not yet been evaluated - the interruptability of declarations is to allow for even cyclic references to NYKAs as constants or init values of other bindings.
    - when it is a ValueBinding which was created to represent an identifier, declared on a statement happening to be in error.

    So: Besides type, a NodeValue has a 64b value encoding both a 40b IR code and the 24b meta-IR flags associated with it, it also has a copy of the 64b MetaValueIR found at that IR position (relevant for known values)

    That IR code can now reference *any instruction or declaration within all source-files*, as well as a large panel of 32b immediate values.
    => IR codes can represent global variables, global or local constants, local variables or parameters, procedures, types,
        temporary runtime or constant *values*, usually also *reference* all of those indirectly for referencing or assignment
        operations, and may sometimes be 'adresses' which will not be known until very late in the backend stage, yet considered
        as almost-constants by the typechecker : also known as 'NYKA' (not-yet-known-addresses)
    After the 'full-IR-switch' in our design, most computations about constant values also go through 'IR': adding two constants
        together is not done on the typechecker side any more. An IR for the add instruction will be virtually emitted (even at
        global scope), then same code responsible for constant propagation optimizations will be invoked to compute the resulting
        value as *meta* info on top of the IR ; and an actual 'add' instruction will be emitted for holding that meta-info whenever
        the result is not an IR immediate. Such meta-info will, in turn, be available for the next similar operation, and so on.
    To understand the possibilities offered by our IR to represent both locations and the values associated to them,
        @see the top-file comments in LocLib_IR_Meta.h, 

    To achieve that, we limit:
        - The source-file count:
            65 thousand max.
                -> does not *seem* anywhere close to a very 'limiting' file count for most source-code I know of (that is, in a single binary: exe or DLL).
                -> can always split too-large codebases into separate DLLs
        - The number of IR instruction slots (including specific 'declaration' instructions) usable for global declarations, per sourcefile:
            16M max (global vars, global constants/procs, user-defined types, possibly some local constants promoted to global, such as proc-defs)
                -> does not *seem* anywhere close to a very 'limiting' declaration count for most source-code I know of.
                -> can always split too-large files into several smaller files.
        - The number of IR instruction slots (including 'declaration' instructions) usable for encoding actual instructions, per procedure.
            16M max (actual instructions, a few markups, local vars declarations...)
                -> does not *seem* anywhere close to a very 'limiting' declaration count for most procedures.
                -> can always split too-large procedures into several smaller procedures.
    This allows:
        -> 40b 'IR', freed from the need to specially-handle cross-sourcefile-references, or referencing global from local...
            -> larger space for 'immediates', now allowing the whole range of f32, and zero-or-one-extensible, full u32 (=> +4M-1 to -4M)
        -> 64b 'NYKAs'n able to reference any declaration or deref within those IR slots, and represent the resulting address +-a 2MB known offset.
            Also allowed from the fact that we limit the size of any type construct to a max of 2MegaBytes.
    This supposes:
        -> now larger-than-64b IR instructions. Mitigated by the fact that we otherwise started to require meta-info all over the place anyway.
        -> IR instructions + flags associated with the knowledge we have about that value is now 2x64b.
        -> + very-handly, actually known up-to-64b value (or ptr to it) => 3x64b total, per IR instruction slot.
 */

struct TypeInfo; // predecl

// 3x 64b
struct NodeValue {
    DECL_TRIVIAL_STRUCT_OPS(NodeValue);
    const TypeInfo* pType;  // Actual 'type' ; known only to the typechecker
    IRInfo info;            // 2x64b : 40b MSB IR ; 24b LSB meta-flags of that IR *value* + Copy of "MetaInfo" found at the IR position ; especially relevant for TC when IRFLAG_IS_KNOWN is raised (as AKnownValue through the .whenKnown member).
};

// Note: Currently, compints are not supposed to get anywhere near the IR representation
// Yet... we *might* one day want to allow that, so that we could maybe have entities which could represent TC-only structures, but *in IR*,
//    and use them is some corner cases of comptime execution of user-specified functions ??? not really sure about all this atm.
// So... Compints may also detectable by IR from the special IR format encoding '0x38u'
//   The decision to downgrade compints as 'int' will stay a typechecker-only thing.
// Note that in that 'future-plan' case, 'reinterpret-cast' to or from a compint is 'somewhat possible' and must be understood as reinterp-cast to/from that *special-encoding* itself, and not to/from the value it represents.
//   To cast a compint *value* to another representation with a similar numerical 'semantic', we may use the simple cast operators in IR.
// 
// Note that 'compints' have a special representation, **not** based on the standard IR flags (so that we can encode any compint, even within compound-types)
//   They'll be a tagged 64b encoding of their own : 3 lsb represent: is_negative (100) + is_embedded (.00) or points_to_1leg (.01) or points to two legs (.10) or points to N legs (.11, with first pointed leg in fact holding leg count)

#define COMPINT_PSEUDO_IR_FORMAT    0x38u   // corresponds to a vec-slot of 8x f8... thus it has same footprint as our '64b' tagged-ptr/embd encoding, and does not conflict with others since 'f8' is not a valid format.

#define COMPINT_VALUE_SHIFT_WHENSMALL    3u
#define COMPINT_ASPTR_MASK               0xFFFF'FFFF'FFFF'FFF8uLL
#define COMPINT_FLAG_IS_NEGATIVE         0x0000'0000'0000'0004uLL
#define COMPINT_SIZE_MASK                0x0000'0003u
#define COMPINT_SIZE_SMALL_EMBD          0u
#define COMPINT_SIZE_1LEG                1u
#define COMPINT_SIZE_2LEGS               2u
#define COMPINT_SIZE_NLEGS               3u


// For the 'other' *unsized* comptime kind which is float literals, they shall, at one point, be encoded exacly same as 'xfloat' (0x0E) ...:
//     their known full-payload will thus be 8x64b, with possible flag if their value fit unmodified as an f64.
//   The decision to downgrade float literals as f64 will however be a typechecker-only thing.
//   The possibility that any 'xfloat' remains within the 'runtime' IR may be subordinate to a compilation-param-flag
//   It is expected that many unqualified conversions of float literals will use them as in fact f64 at runtime
//      (and this is the current alias for 'float' in LOC)... but they'll be kept as xfloat until known whether user asks explicitely for
//      more precision on usage. Note that we may add a compilation param allowing a forced-downgrade of comptime floats to f64.
// CLEANUP:THOUGHTS: 
//   At start, we went with the idea that all known special values such as +0.0, -0.0, +inf, -inf, snan, or qnan... shall always have an 'embedded',
//      representation, whatever the float type. However, following the simplification of embedding and immediates, integral formats larger than
//      64b are now *never embedded* => we'll follow same rule for FP (as for any other type, btw)
//   Besides, not forcing embedding of NaNs would allow LOC to deal with possible NaN *payloads*, something which was ruled out with previous plan.

#define XFLOAT_PSEUDO_IR_FORMAT     0x0Eu   // corresponds to a scalar-slot of f512... thus it has same footprint as our 8x64b encoding, and does not conflict with others since 'f512' is not a valid format.


local_func_inl bool is_value_tc_only(const NodeValue* pValue) {
    return irflag_is_tc_only(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_semantic_const(const NodeValue* pValue) {
    return irflag_is_semantic_const(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_tc_const(const NodeValue* pValue) {
    return irflag_is_tc_const(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_tc_binding_instance(const NodeValue* pValue) {
    return irflag_is_tc_binding_instance(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_tc_referencable(const NodeValue* pValue) {
    return irflag_is_tc_referencable(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_tc_assignable(const NodeValue* pValue) {
    return irflag_is_tc_assignable(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_tc_variable_binding(const NodeValue* pValue) {
    return irflag_is_tc_variable_binding(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_tc_const_binding(const NodeValue* pValue) {
    return irflag_is_tc_const_binding(pValue->info.uIRandMetaFlags);
}

/*
local_func_inl bool is_value_ir_const(const NodeValue* pValue) {
    return irflag_is_ir_const(pValue->info.uIRandMetaFlags);
}
*/
local_func_inl bool is_value_known_or_nyka(const NodeValue* pValue) {
    return irflag_is_known_or_nyka(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_known_non_nyka(const NodeValue* pValue) {
    return irflag_is_known_non_nyka(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_nyka_or_has_nyka(const NodeValue* pValue) {
    return irflag_is_or_has_nyka(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_single_nyka(const NodeValue* pValue) {
    return irflag_is_single_nyka(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_known_embd(const NodeValue* pValue) {
    return irflag_is_known_embd(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_known_non_nyka_embd(const NodeValue* pValue) {
    return irflag_is_known_non_nyka_embd(pValue->info.uIRandMetaFlags);
}
/*
local_func_inl bool is_value_known_embd_sign_ext(const NodeValue* pValue) {
    return irflag_is_known_embd_sign_ext(pValue->info.uIRandMetaFlags);
}
local_func_inl bool is_value_known_embd_neg_ext(const NodeValue* pValue) {
    return irflag_is_known_embd_neg_ext(pValue->info.uIRandMetaFlags);
}
*/
local_func_inl bool is_value_pseudo_valued_cond(const NodeValue* pValue) {
    return irflag_is_pseudo_valued_cond(pValue->info.uIRandMetaFlags);
}


// the equivalent of an 'AST' node, as seen through the eyes of the typechecker...
// 3x 64b
struct TCNode {
    DECL_TRIVIAL_STRUCT_OPS(TCNode);
    AstNode ast;                // a copy of the original ast (or could be generated/modified by macro expansion) + TC flags
    u32 uIntrinsicValueIndex;   // If an expression, index into the statementwise array of ValueHolder after TC phase 1 success.
    u32 uFinalValueIndex;       // If an expression, index into the statementwise array of ValueHolder after TC phase 2 success (casts and reification).
};

/*
// Indicates whether a value is known to be all-zeroed, for all types.
local_func_inl bool is_value_zeroed(NodeValue* pValue)
{
    return is_value_zeroed(u32(pValue->info.uIRandMetaFlags), pValue->info.metaValue);
}
*/

// Some structure representing a TCNode during active typechecking, with additional info and unpacking of some stuff already.
// This structure is called 'temporary' since it is intended to be allocated directly from the typechecker stack... ;
//   its footprint will thus vanish following the stack path, walking through a statement node-tree, and in particular would have
//   vanished for all nodes in a statement after this statement has been typechecked. It will ALSO vanish in case of a 'waiting'
//   tc result somewhere within the statement, however it shall be perfectly reconstructible from the persisting 'TCNode' and
//   'ValueHolder' vectors on the statement.
// 4x 64b
struct TmpTCNode {
    TCNode* pTCNode;                  // A reference to the core tc node (which remains in memory in case of halt)
    NodeValue* pIntrinsicValue;       // If an expression, a reference to the intrinsic value after tc phase 1. Shall be null before that.
    NodeValue* pFinalValue;           // If an expression, a reference to the final value after tc phase 2 (casts and reification...). Shall be null before that.
    u32 uNodeIndexInStatement;        // The index of the node in statement, for cross-indexing to AST during err-reports...
    u32 _pad0;                        // reserved, unused atm
};

// 3x 64b
struct BindingSourceRefs {
    int iSourceFile;                // obvious for regular ; for bindings declared in an unhygienic expansion, ref at primary expansion caller
    u32 uAstBlock;                  // obvious for regular ; for bindings declared in an unhygienic expansion, ref at primary expansion caller
    u32 uAstStatementIndexInBlock;  // obvious for regular ; for bindings declared in an unhygienic expansion, ref at primary expansion caller
    int iIfExpandedExpansionSourceFile;               // -1 for regular ; for bindings declared in an unhygienic expansion, ref at leaf macro source site
    u32 uIfExpandedExpansionAstBlock;                 // unused for regular ; for bindings declared in an unhygienic expansion, ref at leaf macro source site
    u32 uIfExpandedExpansionAstStatementIndexInBlock; // unused for regular ; for bindings declared in an unhygienic expansion, ref at leaf macro source site
};

// Definition of a variable or constant *binding*.
//   Happens to also be usable as a ValueHolder (shall be flagged with IRFLAG_TC_BINDING_INSTANCE in its info.uIRandMetaFlags)
// 8x 64b
__declspec(align(64))
struct ValueBinding : public NodeValue {
    // this is just a cross-referencing of the source-code identifier used on registration...
    int iIdentifierHandle;
    u32 uScopeAndLocation;         // eScopeKind (whether global or local, whether global/package/private, if global...). 24 Msb 'Location' is a cross-ref to a repository of binding, depending on scope and context.
    u32 uCompoundDeclSort;         // statement index 27msb + 0..31 decl index in statement 5lsb
    u32 _reserved;                 // reserved for future use
    BindingSourceRefs sourceRef;   // 3x 64b behemoth just for cross-referencing the source... ??
};

static_assert(alignof(ValueBinding) >= 4, "ValueBinding alignment shall be at least 4 for usage as ScopedEntityHandle");

local_func_inl IRInfo get_info_from_value(const NodeValue* pNodeValue) {
    return pNodeValue->info;
}
local_func_inl IRInfo& get_info_ref_from_value(NodeValue* pNodeValue) {
    return pNodeValue->info;
}

#endif // LOCLIB_NODE_VALUE_H_

