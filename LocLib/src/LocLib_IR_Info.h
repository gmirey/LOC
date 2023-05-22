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

#ifndef LOCLIB_IR_INFO_H_
#define LOCLIB_IR_INFO_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/arithmetic_operations.h"

#include "LocLib_PostParserTypes.h"

struct SourceFileDescAndState;       // predecl for 'CompilationContext' definition
struct LocLib_CompilationParams;     // predecl for 'CompilationContext' definition
struct WorkerDesc;                   // predecl for 'CompilationContext' definition
struct WholeProgramCompilationState; // predecl for 'CompilationContext' definition

struct CompilationContext {
    SourceFileDescAndState* pIsolatedSourceFile;            // the source file which is ensured accessed in isolation here
    LocLib_CompilationParams* pCompilationParams;           // the current compilation params
    WorkerDesc* pWorker;                                    // the worker currently operating (for thread id and local arena)
    WholeProgramCompilationState* pProgCompilationState;    // the compilation state for the entire program
};

// Our IR is organized as follows:
// 
// * Each distinct proc body has an independent container for its IR (local vars, local consts, runtime instructions)
// * Each distinct source file has an independent container for some const definitions at the sourcefile global scope
// * Each distinct source file also has an additional, separate container for global *variable* definitions
// * There exists an independent container for some definitions at a programwise global scope
// * There may exist an independent container for temporary IR (used by IR optimizer during IR optim phases)
// 
// * Within each container, there exists one entry, of fixed size, per IR position.
// - Each of these entries has sufficient space for encoding:
//    * a particular IR instruction ('IRIT' for IR Instruction Type) on 8b
//    * some additional instruction specific flags on 8b
//    * a particular IR format on 8b
//    * two standard IR parameters, of 40b each.
//    * meta-flags on the result, 19b
//    * additional overall flags, 5b
//    * possibly known meta-value on the result, 64b
//   Totalling 3x 64b
// Standard IR parameters are typically arranged in the 40 msb of a 64b slot within these entries
// A standard IR parameter thus has 24 lsb of available 'space' below it.
//      from the TC standpoint: we can encode any 'value' as a standard IR parameter + 24b flags below it.
//          the lowest 4b of those 24b are TC-specific ; leaving 20b flags for meta-value characteristics.
//          => the TC thus can have an IR representation for: an IR entry code + flags about whether value is known + 64b meta-value when known.
//      from the IR standpoint:
//          below first std param are 8b IRIT code + 8b instr flags + 8b fomat ;
//          below second std param are 24b of the flags for meta-value characteristics of the resulting value.


//
// IR flags: found in the **second** 64b of an ir entry, or as part of the 'uIRandFlags' property of a NodeValue (or IRInfo)
//

#define IRFLAGS_TC_SPECIFIC_MASK      0x0000'000Fu    // part of an IR flag value which is reserved for TC-specific info
#define IRFLAGS_IR_SPECIFIC_MASK      0x00FF'FFF0u    // part of an IR flag value which is relevant to IR solvers

#define IRFLAG_TC_ONLY                0x0000'0001u    // this IR value is a TC-only constant: the rest of the code is irrelevant as IR. This is the case in particular for nodes carrying a 'type' as a value.
#define IRFLAG_TC_SEMANTIC_CONST      0x0000'0002u    // this IR value represents a constant for the typechecker (prefer to check for this instead of IR_FLAG_IS_CONST or IR_FLAG_IS_KNOWN: it should be set on TC-only values, and will also be of importance for complex types with refs, such as string types).
#define IRFLAG_TC_BINDING_INSTANCE    0x0000'0004u    // the 'NodeValue' carrying this value can be cast to a ValueBinding
#define IRFLAG_TC_REFERENCABLE        0x0000'0008u    // the value associated with this IR value can be referenced. If not a semantic const, it can be assigned to.

#define IRFLAG_IS_KNOWN               0x0080'0000u    // the associated meta-info can use the '.whenKnown' union. Should be either 'const' or 'has_nyka' (being full nyka if non-const, in particular has_nyka + known_embd).
#define IRFLAG_IS_KNOWN_EMBD          0x0040'0000u    // should only be set on values with the 'known' flag. It indicates that the 64b meta info is (or carries, or represents) the runtime payload, instead of being a *pointer* to the runtime payload. 
#define IRFLAG_HAS_NYKA               0x0020'0000u    // associated meta-info carries NYKA. if known, should be full nykas (or const + some), or 'is_known_embd' (in which case only a nyka)
#define IRFLAG_HAS_LOCAL_NYKA         0x0010'0000u    // if has nyka, there is at least one based on a local variable. Such values can never be fully const...

#define IRFLAG_IS_KNOWN_ZERO          0x0004'0000u

#define IRFLAG_IS_PSEUDO_VALUED_COND  0x0001'0000u    // should only be set on values *without* the 'known' flag, with a corresponding IR being a position in current proc, associated with an IRIT_PSEUDO_VALUED_COND. 


local_func_inl bool irflag_is_tc_only(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_TC_ONLY);
}
local_func_inl bool irflag_is_semantic_const(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_TC_SEMANTIC_CONST);
}
local_func_inl bool irflag_is_tc_const(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & (IRFLAG_TC_SEMANTIC_CONST|IRFLAG_TC_ONLY));
}
local_func_inl bool irflag_is_tc_binding_instance(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_TC_BINDING_INSTANCE);
}
local_func_inl bool irflag_is_tc_referencable(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_TC_REFERENCABLE);
}
local_func_inl bool irflag_is_tc_assignable(u64 uFlagsInIRParam) {
    return IRFLAG_TC_REFERENCABLE == (uFlagsInIRParam & (IRFLAG_TC_REFERENCABLE|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_TC_ONLY));
}
local_func_inl bool irflag_is_tc_variable_binding(u64 uFlagsInIRParam) {
    return IRFLAG_TC_BINDING_INSTANCE == (uFlagsInIRParam & (IRFLAG_TC_BINDING_INSTANCE|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_TC_ONLY));
}
local_func_inl bool irflag_is_tc_const_binding(u64 uFlagsInIRParam) {
    return irflag_is_tc_binding_instance(uFlagsInIRParam) && irflag_is_tc_const(uFlagsInIRParam);
}
local_func_inl bool irflag_is_known_or_nyka(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_IS_KNOWN);
}
local_func_inl bool irflag_is_known_non_nyka(u64 uFlagsInIRParam) {
    return IRFLAG_IS_KNOWN == (uFlagsInIRParam & (IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA));
}
local_func_inl bool irflag_is_or_has_nyka(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_HAS_NYKA);
}
local_func_inl bool irflag_is_single_nyka(u64 uFlagsInIRParam) {
    return (IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_EMBD) == (uFlagsInIRParam & (IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_EMBD));
}
local_func_inl bool irflag_is_known_embd(u64 uFlagsInIRParam) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) == (uFlagsInIRParam & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD));
}
local_func_inl bool irflag_is_known_non_nyka_embd(u64 uFlagsInIRParam) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) == (uFlagsInIRParam & (IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_EMBD));
}
local_func_inl bool irflag_is_known_zero(u64 uFlagsInIRParam) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_ZERO) == (uFlagsInIRParam & (IRFLAG_IS_KNOWN|IRFLAG_HAS_NYKA|IRFLAG_IS_KNOWN_ZERO));
}
local_func_inl bool irflag_is_pseudo_valued_cond(u64 uFlagsInIRParam) {
    return 0 != (uFlagsInIRParam & IRFLAG_IS_PSEUDO_VALUED_COND);
}

//
// IR-instruction flags (found in the **first** 64b of an ir entry, on bitpos #8..#15)
//

#define IR_INSTRFLAG_IS_ASSIGNABLE          0x00'8000u    // Indicates that the slot at this position is also assignable

#define IR_INSTRFLAG_ERRCHK_IS_ACTIVE       0x00'4000u    // is an err-check active
#define IR_INSTRFLAG_BRANCH_ON_NONZERO      0x00'2000u    // is a branch (or goto-err of an err-check) taken when conditional is non-zero (otherwise taken when zero)
#define IR_INSTRFLAG_BRANCH_REPLACES_PARAM  0x00'1000u    // branch instruction takes place of an 'additional retparam' on an instruction with 'IR_INSTRFLAG_EMBD_CHECK' raised.

#define IR_INSTRFLAG_CONST_IS_RTTI          0x00'4000u    // indicating that a *const* declaration is an RTTI (may not be emitted depending on flags)

#define IR_INSTRFLAG_VAR_IS_VOLATILE        0x00'4000u    // indicating that a *var* or *by-ref-param* declaration is volatile
#define IR_INSTRFLAG_PARAM_IS_BYREF         0x00'2000u    // indication that a param declaration is a proc-param which is implicitely by-ref

#define IR_INSTRFLAG_EMBD_CHECK             0x00'4000u    // instruction with post-op flag has an embedded err-check associated with its value (when 1) - err-check info entry replacing the default 'additional retparam'.
#define IR_INSTRFLAG_POSTOP_CHKSIGNED       0x00'2000u    // instruction will emit a post-op flag (encoded as additional retparam on 8b), corresponding with invalidity (when set) for signed semantics.
#define IR_INSTRFLAG_POSTOP_CHKUNSIGNED     0x00'1000u    // instruction will emit a post-op flag (encoded as additional retparam on 8b), corresponding with invalidity (when set) for unsigned semantics.
#define IR_INSTRFLAG_POSTOP_SPECIAL         0x00'0800u    // instruction will emit a post-op flag (encoded as additional retparam on 8b), corresponding with invalidity (when set) for special semantics (ie mixed).

#define IR_INSTRFLAG_ONLY_FOR_NEXT_BRANCHES 0x00'4000u    // comparison instruction result will be directly used by a following branch or errcheck instruction (thus no need to store independently for backends with branches based on flag registers).
#define IR_INSTRFLAG_CMP_OPPOSITE           0x00'2000u    // cmp instruction is opposite (neq for cmp_eq, >= instead of < for cmp_strict_ord) 
#define IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED 0x00'1000u    // instruction has unsigned semantics for ints, instead of default signed.
#define IR_INSTRFLAG_POWSHIFT_CHECKSIGNED   0x00'2000u    // instruction has unsigned semantics for ints, instead of default signed.

#define IR_INSTRFLAG_OFFSET_TMP_FOR_DEREF   0x00'4000u    // offset instruction result will be directly used by a following deref instruction (thus no need to store independently for backends with possible ptr arithmetics encoded within instruction operands).

// 
//   The 40b of an IR parameter are sufficent to refer to any entity from our IR, anywhere in the program + some special values:
//      1 msb : special flag 'S'
//      7 bits below : high part of repository index 'H'
//      8 bits below : low part of repository index 'L'
//      24 bits below : standard entry index 'I'
//   Then we decode as follows:
//   * if H == '7F':
//      this IR parameter is a numerical immediate, either integral or floating point, depending on the instruction, which has knowledge of this
//        distinction from its format or context:
//        - If context calls for an fp16: then the 16b of the fp16 representation are in the lower 16b of I
//        - If context calls for an fp32: then the 32b of the fp32 representation are formed by L and I concatenated (L in the msb)
//        - If context calls for a larger floating point: then we can decode as-if fp32 then standard "cast" to the larger fp value => the whole range of fp32
//              is available for immediates of any larger fp size ; note that any non-finite value we support can be expressed as fp32. We use standard payloads '...001' and '1...000' for SNaNs and QNaNs respectively. Other payloads may or may not be encodable as fp32, thus as immediates.
//        - If context calls for an r8 : then the 8b of the r8 representation are in the lower 8b of I
//        - If context calls for an r16 : then the 16b of the r16 representation are in the lower 16b of I
//        - If context calls for an r32 : then the 32b of the r32 representation are formed by L and I concatenated (L in the msb)
//        - If context calls for a larger integral immediate: then we can decode as-if r32 and extend to the larger integral value:
//              * with zeroes if 'S' is not set
//              * with ones if 'S' is set...
//        - note that 'all zeroes' for any context is thus encodable as an immediate, of 40b value '7F00000000'
//   * otherwise:
//      this IR parameter refers to any declared entity within our IR, as follows:
//      - depending on 'S':
//          * if S is set, it indicates that the reference is by address => this IR parameter can be envisionned as the immediate 'address' of that entity.
//          * otherwise, it indicates that the reference is direct => this IR parameter represents the value (rvalue OR lvalue) held by this entity.
//      - depending on 'H':
//          * if 'H' == '00', the associated IR entry is in the programwise global repository. the full remaining 32b (L msb, I lsb) is available for its index.
//              => max 4G for prolog constants, and RTTI of core-types + any type defined by type-constructor, or being a function signature, and/or being polymorphic.
//              - note that we emit a NOOP at index 0 there, that no-one should refer to, so that IR standard parameter "all-zeroes" is indicative of a no-init.
//          * otherwise, let 'R' be: H msb, L lsb on 15 bits:
//              - if 'R' == '0100', the associated IR entry is in the current proc-local repository. 'I' is its 24b index (=> max 16M instructions *per proc*)
//                                  => only 'one' is sufficient since proc-bodies cannot refer to anything declared within another.
//              - if 'R' == '0101', the associated IR entry is in the temporary IR repository. 'I' is its 24b index (=> max 16M temporary instructions)
//              - otherwise:
//                  the associated IR entry is an entity specific to a particular source file, whose index is "R - 0x0102" => **max 32,253 files**
//                  the entity itself has id 'I', which is further decomposed as such:
//                  * if two msb of I are 00, the 22 lsb are the 22b index in the associated IR repository (=> max 4M for global constants and RTTI of compound types)
//                  * if two msb of I are 01, the 22 lsb are the 22b index of a global *variable* within a special container for them.
//                  * if two msb of I are 10, the 22 lsb are the 22b index of a procbody (The 'S' flag is supposed always raised for referring to those => as NYKAs)
//                  * if two msb of I are 11, this is currently reserved for future use.
//              

// predecls
struct TypeInfo;
struct RuntimeTypeInfo;
struct CompNat;
struct XFloat;
struct KnownConstraints;

// AKnownValue : part of KnownNodeValue, of IRInfo, or as the third 64b of an IREntry (as fitting within the MetaValueIR union)
union AKnownValue {
    u64 _payload;
    u64 uEmbeddedValue;
    double dEmbeddedFP;
    u8* pPtrToRawData;
    u64* tLegs;
    const TypeInfo* pType;
    RuntimeTypeInfo* pRuntimeTypeInfo;
    XFloat* pXFloat;
};
static_assert(sizeof(AKnownValue) == 8u, "AKnownValue was designed to be 64b");

local_func_inl constexpr AKnownValue known_from_payload(u64 payload) { AKnownValue result = {}; result._payload = payload; return result; }

// MetaValueIR : goes to the third 64b of an IREntry
union MetaValueIR {
    u64 _payload;
    AKnownValue knownValue;
    KnownConstraints* pConstraints;
};

local_func_inl constexpr MetaValueIR meta_from_known(AKnownValue knownValue) { MetaValueIR result = {}; result.knownValue = knownValue; return result; }
local_func_inl constexpr MetaValueIR meta_from_payload(u64 payload) { MetaValueIR result = {}; result._payload = payload; return result; }

struct IREntry {
    u64 uInstrCodeAndFormatAndFirstParam;   // from least to most significant bits: 8b instruction code ; 8b instruction-specific flags ; 8b format ; 40b IR if standard first param
    u64 uInstrMetaFlagsAndSecondParam;      // from least to most significant bits: 24b flags for metaValue ; 40b IR if standard second param
    MetaValueIR metaValue;
};

#define IR_STD_PARAM_SHIFT                  24u
#define IR_STD_PARAM_MASK                   0xFFFF'FFFF'FF00'0000uLL
#define IR_STD_PARAM_METAMASK               0x0000'0000'00FF'FFFFuLL
#define IR_STD_PARAM_SPECIAL_FLAG           0x8000'0000'0000'0000uLL
#define IR_STD_PARAM_HIGHMASK               0x7F00'0000'0000'0000uLL

#define IR_STD_PARAM_HIGH_MARK_IMMEDIATE    0x7F00'0000'0000'0000uLL
#define IR_STD_PARAM_HIGH_MARK_PROG_GLOBAL  0x0000'0000'0000'0000uLL
#define IR_STD_PARAM_REPO_ID_SHIFT          48u
#define IR_STD_PARAM_REPO_ID_MASK           0x7FFFu

#define IR_REPO_ID_CURRENT_PROC             0x0100u
#define IR_REPO_ID_TEMPORARY                0x0101u
#define IR_REPO_ID_FIRST_FILE               0x0102u

#define IR_MAX_FILE_COUNT                   (u16(IR_STD_PARAM_HIGH_MARK_IMMEDIATE >> IR_STD_PARAM_REPO_ID_SHIFT) - IR_REPO_ID_FIRST_FILE)   // 32254 

struct IRRepo {
    u32 uSize;
    u16 uIRRepoId;
    u16 _pad0;
    TmpArray<IREntry*>  vecIRChunks;
};

#define IRCHUNK_INSTR_SHIFT     5u                              // IR instructions will be allocated in chunks of 32 of them
#define IRCHUNK_INSTR_COUNT     (1u << IRCHUNK_INSTR_SHIFT)
#define IRCHUNK_INSTR_MASK      (IRCHUNK_INSTR_COUNT - 1u)

local_func_inl IREntry& ir_access_repo_instr(IRRepo* pRepo, u32 uPos) {
    Assert_(uPos < pRepo->uSize); 
    IREntry* tEntries = pRepo->vecIRChunks[uPos >> IRCHUNK_INSTR_SHIFT];
    return tEntries[uPos & IRCHUNK_INSTR_MASK];
}

local_func_inl constexpr u8 ir_get_IRIT(IREntry entry) { return u8(entry.uInstrCodeAndFormatAndFirstParam); }
local_func_inl constexpr u8 ir_get_std_format(IREntry entry) { return u8(entry.uInstrCodeAndFormatAndFirstParam >> 16); }
local_func_inl constexpr u32 ir_get_instruction_flags_(IREntry entry) { return u32(entry.uInstrCodeAndFormatAndFirstParam); }
local_func_inl constexpr u32 ir_get_instruction_flags(IREntry entry) { return ir_get_instruction_flags_(entry) & 0x0000'FF00u; }
local_func_inl constexpr u64 ir_get_std_first_param_(IREntry entry) { return entry.uInstrCodeAndFormatAndFirstParam; }
local_func_inl constexpr u64 ir_get_std_first_param(IREntry entry) { return ir_get_std_first_param_(entry) & IR_STD_PARAM_MASK; }
local_func_inl constexpr u64 ir_get_std_second_param_(IREntry entry) { return entry.uInstrMetaFlagsAndSecondParam; }
local_func_inl constexpr u64 ir_get_std_second_param(IREntry entry) { return ir_get_std_second_param_(entry) & IR_STD_PARAM_MASK; }
local_func_inl constexpr u32 ir_get_meta_value_flags_(IREntry entry) { return u32(entry.uInstrMetaFlagsAndSecondParam); }
local_func_inl constexpr u32 ir_get_meta_value_flags(IREntry entry) { return ir_get_meta_value_flags_(entry) & 0x00FF'FFFFu; }
local_func_inl constexpr MetaValueIR ir_get_meta_value(IREntry entry) { return entry.metaValue; }

#define INVALID_IR_CODE             0uLL          // can now zero-initialize this directly to invalid.

local_func_inl constexpr bool ir_is_valid_param(u64 uParam) {
    return 0uLL == (uParam & ~IR_STD_PARAM_MASK) && uParam != INVALID_IR_CODE;
}
local_func_inl constexpr bool ir_is_valid_param_(u64 uParam) {
    return ir_is_valid_param(uParam & IR_STD_PARAM_MASK);
}
local_func_inl constexpr u64 ir_make_std_code(u16 uRepoLocation, u32 uInstrIndexInRepo) {
    return (u64(uInstrIndexInRepo) << IR_STD_PARAM_SHIFT) | (u64(uRepoLocation) << IR_STD_PARAM_REPO_ID_SHIFT);
}
local_func_inl constexpr u64 ir_make_std_code_in_cur_proc(u32 uInstrIndexInProcRepo) {
    return ir_make_std_code(IR_REPO_ID_CURRENT_PROC, uInstrIndexInProcRepo);
}
local_func_inl constexpr u64 ir_make_std_code_in_tmp_repo(u32 uInstrIndexInTmpRepo) {
    return ir_make_std_code(IR_REPO_ID_TEMPORARY, uInstrIndexInTmpRepo);
}
local_func_inl constexpr u64 ir_make_global_const_code_in_file(u32 uFileIndex, u32 uInstrIndexInFile) {
    Assert_(uFileIndex < IR_MAX_FILE_COUNT);
    Assert_(uInstrIndexInFile < 0x0040'0000uLL);
    return ir_make_std_code(u16(uFileIndex + IR_REPO_ID_FIRST_FILE), uInstrIndexInFile);
}
local_func_inl constexpr u64 ir_make_global_var_code_in_file(u32 uFileIndex, u32 uInstrIndexInFile) {
    Assert_(uFileIndex < IR_MAX_FILE_COUNT);
    Assert_(uInstrIndexInFile < 0x0040'0000uLL);
    return ir_make_std_code(u16(uFileIndex + IR_REPO_ID_FIRST_FILE), uInstrIndexInFile|0x0040'0000uLL);
}
local_func_inl constexpr u64 ir_make_procbody_ref_in_file(u32 uFileIndex, u32 uProcBodyIndexInFile) {
    Assert_(uFileIndex < IR_MAX_FILE_COUNT);
    Assert_(uProcBodyIndexInFile < 0x0040'0000uLL);
    return ir_make_std_code(u16(uFileIndex + IR_REPO_ID_FIRST_FILE), uProcBodyIndexInFile|0x0080'0000uLL);
}
local_func_inl constexpr u64 ir_make_std_code_programwise_global(u32 uInstrIndexInProgGlobals) {
    return u64(uInstrIndexInProgGlobals) << IR_STD_PARAM_SHIFT;
}
local_func_inl constexpr bool ir_is_immediate(u64 uParam) {
    Assert_(ir_is_valid_param(uParam & IR_STD_PARAM_MASK));
    return 0 != (uParam & IR_STD_PARAM_SPECIAL_FLAG) || IR_STD_PARAM_HIGH_MARK_IMMEDIATE == (uParam & IR_STD_PARAM_HIGHMASK);
}
local_func_inl constexpr bool ir_is_numeric_immediate(u64 uParam) {
    Assert_(ir_is_valid_param(uParam & IR_STD_PARAM_MASK));
    return IR_STD_PARAM_HIGH_MARK_IMMEDIATE == (uParam & IR_STD_PARAM_HIGHMASK);
}
local_func_inl constexpr bool ir_is_known_other_than_numeric_imm_a_nyka_imm(u64 uParam) {
    Assert_(!ir_is_numeric_immediate(uParam & IR_STD_PARAM_MASK));
    return 0 != (uParam & IR_STD_PARAM_SPECIAL_FLAG);
}
local_func_inl constexpr bool ir_is_nyka_immediate(u64 uParam) {
    Assert_(ir_is_valid_param(uParam & IR_STD_PARAM_MASK));
    return !ir_is_numeric_immediate(uParam) && ir_is_known_other_than_numeric_imm_a_nyka_imm(uParam);
}
local_func_inl constexpr bool ir_is_int_immediate(u64 uParam) {
    Assert_(ir_is_valid_param(uParam & IR_STD_PARAM_MASK));
    return ir_is_numeric_immediate(uParam) && 0 == (uParam & IR_STD_PARAM_SPECIAL_FLAG);
}
local_func_inl constexpr bool ir_is_float_immediate(u64 uParam) {
    Assert_(ir_is_valid_param(uParam & IR_STD_PARAM_MASK));
    return ir_is_numeric_immediate(uParam) && 0 != (uParam & IR_STD_PARAM_SPECIAL_FLAG);
}

local_func_inl constexpr u64 ir_make_direct_nyka_value(u64 uParam) {
    return uParam >> IR_STD_PARAM_SHIFT;
}
local_func_inl constexpr u64 ir_make_nyka_value(u64 uParam, i32 iOffset) {
    return (uParam >> IR_STD_PARAM_SHIFT) | u64(i64(iOffset) << 40);
}
local_func_inl u64 ir_decode_nyka_value(u64 uNykaValue, i32* outOffset) {
    *outOffset = i32(i64(uNykaValue) >> 40);
    return uNykaValue << IR_STD_PARAM_SHIFT;
}

local_func_inl constexpr u64 ir_make_int_immediate(i32 rValue) {
    return (u64(u32(rValue)) << IR_STD_PARAM_SHIFT) | IR_STD_PARAM_HIGH_MARK_IMMEDIATE;
}
local_func_inl constexpr i32 ir_get_value_from_int_immediate(u64 uParam) {
    Assert_(ir_is_int_immediate(uParam & IR_STD_PARAM_MASK));
    return i32(u32(uParam >> IR_STD_PARAM_SHIFT));
}

local_func_inl u64 ir_make_float_immediate(f32 fValue) {
    return ir_make_int_immediate(i32(type_pun_from_float(fValue))) | IR_STD_PARAM_SPECIAL_FLAG;
}
local_func_inl f32 ir_get_value_from_float_immediate(u64 uParam) {
    Assert_(ir_is_float_immediate(uParam & IR_STD_PARAM_MASK));
    return type_pun_to_float(u32(uParam >> IR_STD_PARAM_SHIFT));
}

local_func_inl constexpr u64 ir_make_nyka_immediate(u64 uParam) {
    Assert_(!ir_is_immediate(uParam));
    return uParam | IR_STD_PARAM_SPECIAL_FLAG;
}
local_func_inl constexpr u64 ir_get_param_from_nyka_immediate(u64 uNykaParam) {
    Assert_(ir_is_nyka_immediate(uNykaParam));
    return uNykaParam & (~IR_STD_PARAM_SPECIAL_FLAG);
}


struct TCProcBodyResult;

struct IRAwareContext: public CompilationContext
{
    IRRepo* pRepo; // either procwise or filewise global const
    IRRepo* pTmpRepo;
    TCProcBodyResult* pProcResult;
};

enum EEntryKind {
    EEK_NOT_AN_ENTRY,
    EEK_PROGRAMWISE_ENTRY,
    EEK_FILEWISE_CONST,
    EEK_FILEWISE_VAR,
    EEK_CURRENT_PROC_LOCAL,
    EEK_CURRENT_TEMPORARY,
    EEK_IS_PROCBODY_REF,
};

local_func bool ir_is_known_all_zeroes(u32 uMetaFlags, u32 uUnalignedByteCount, AKnownValue knownValue)
{
    if (uMetaFlags & IRFLAG_IS_KNOWN) {
        if (0u == (uMetaFlags & IRFLAG_HAS_NYKA)) {
            if (uMetaFlags & IRFLAG_IS_KNOWN_EMBD) {
                Assert_(uUnalignedByteCount < 8u);              // true after Major Immediate & Embedded simplification
                return knownValue.uEmbeddedValue == 0uLL;
            } else {
                u32 uRemainingBytesToTest = uUnalignedByteCount;
                u64 tNothingThere[4096u] = {};
                while (uRemainingBytesToTest > sizeof(tNothingThere)) {
                    if (memcmp(knownValue.pPtrToRawData, tNothingThere, sizeof(tNothingThere)))
                        return false;
                    uRemainingBytesToTest -= sizeof(tNothingThere);
                }
                if (memcmp(knownValue.pPtrToRawData, tNothingThere, uRemainingBytesToTest))
                    return false;
                return true;
            }
        }
    }
    return false;
}

local_func IREntry* ir_append_new_entry(IRRepo* pRepo /*, IRAwareContext* pEvalContext*/) {
    u32 uSlot = pRepo->uSize;
    u32 uPosInChunk = uSlot & IRCHUNK_INSTR_MASK;
    IREntry* tChunkEntries;
    if (uPosInChunk)
        tChunkEntries = pRepo->vecIRChunks[uSlot >> IRCHUNK_INSTR_SHIFT];
    else {
        tChunkEntries = (IREntry*)alloc_from(pRepo->vecIRChunks._alloc.arena, sizeof(IREntry)*IRCHUNK_INSTR_COUNT, alignof(IREntry));
        Assert_(tChunkEntries);
        pRepo->vecIRChunks.append(tChunkEntries);
    }
    pRepo->uSize = uSlot+1u;
    return tChunkEntries + uPosInChunk;
}

local_func void init_ir_repo(IRRepo* ioRepo, u16 uIRRepoId, Arena arena)
{
    ioRepo->uSize = 0;
    ioRepo->uIRRepoId = uIRRepoId;
    ioRepo->vecIRChunks.init(FireAndForgetArenaAlloc(arena));
}

struct IRInfo {
    u64 uIRandMetaFlags;
    MetaValueIR metaValue;
};

local_func_inl void ir_get_info_from_i32_immediate(u64 uIR, u8 uFormat, IRInfo* outInfo) {
    Assert_(ir_is_int_immediate(uIR));                        // true after Major Immediate & Embedded simplification
    Assert_(uFormat <= 0x03u);                                // true after Major Immediate & Embedded simplification
    i32 iEmbeddedI32 = ir_get_value_from_int_immediate(uIR);  // true after Major Immediate & Embedded simplification
    switch (uFormat) {
        case 0x00u: outInfo->metaValue.knownValue.uEmbeddedValue = u64(u8(iEmbeddedI32));   break; // only 8lsb, whatever signedness
        case 0x01u: outInfo->metaValue.knownValue.uEmbeddedValue = u64(u16(iEmbeddedI32));  break; // only 16lsb, whatever signedness
        case 0x02u: outInfo->metaValue.knownValue.uEmbeddedValue = u64(u32(iEmbeddedI32));  break; // only 32lsb, whatever signedness
        case 0x03u: outInfo->metaValue.knownValue.uEmbeddedValue = u64(i64(iEmbeddedI32));  break; // always sign-extension to i64
        default: Assume_(false);
    };
    outInfo->uIRandMetaFlags = uIR | (IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD);
    if (outInfo->metaValue.knownValue.uEmbeddedValue == 0uLL)
        outInfo->uIRandMetaFlags |= IRFLAG_IS_KNOWN_ZERO;
}
local_func_inl IRInfo ir_make_info_for_int_immediate(i32 iImmIntValue, u8 uFormat) {
    IRInfo result;
    ir_get_info_from_i32_immediate(ir_make_int_immediate(iImmIntValue), uFormat, &result);
    return result;
}

local_func_inl void ir_get_info_from_f32_immediate(u64 uIR, u8 uFormat, IRInfo* outInfo) {
    Assert_(ir_is_float_immediate(uIR));                            // true after Major Immediate & Embedded simplification
    Assert_(uFormat & 0x08u);
    Assert_((uFormat & 0x07u) <= 0x03u);                            // true after Major Immediate & Embedded simplification
    Assert_((uFormat & 0x07u) >= 0x01u);                            // no 8b float does exist
    u32 uEmbeddedF32Payload = u32(uIR >> IR_STD_PARAM_SHIFT);
    switch (uFormat) {
        case 0x08u|0x01u: Assert(false, "f16 not yet implemented"); break; // TODO
        case 0x08u|0x02u: outInfo->metaValue.knownValue.uEmbeddedValue = u64(uEmbeddedF32Payload); break;                    // only 32lsb
        case 0x08u|0x03u: outInfo->metaValue.knownValue.dEmbeddedFP = double(type_pun_to_float(uEmbeddedF32Payload)); break; // converts to double
        default: Assume_(false);
    };
    outInfo->uIRandMetaFlags = uIR | (IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD);
    if (outInfo->metaValue.knownValue.uEmbeddedValue == 0uLL)
        outInfo->uIRandMetaFlags |= IRFLAG_IS_KNOWN_ZERO;
}
local_func_inl IRInfo ir_make_info_for_float_immediate(f32 fImmFloatValue, u8 uFormat) {
    IRInfo result;
    ir_get_info_from_f32_immediate(ir_make_float_immediate(fImmFloatValue), uFormat, &result);
    return result;
}

local_func_inl void ir_get_info_from_numeric_immediate(u64 uIR, u8 uFormat, IRInfo* outInfo) {
    Assert_(ir_is_numeric_immediate(uIR));
    Assert_(0u == (uFormat & 0xF0u));                       // true after Major Immediate & Embedded simplification
    Assert_(0x03u >= (uFormat & 0x07u));                    // true after Major Immediate & Embedded simplification
    if (uFormat & 0x08u) {
        ir_get_info_from_f32_immediate(uIR, uFormat, outInfo);
    } else {
        ir_get_info_from_i32_immediate(uIR, uFormat, outInfo);
    }
}

local_func_inl void ir_get_info_from_nyka_immediate(u64 uIR, IRInfo* outInfo) {
    Assert_(ir_is_nyka_immediate(uIR));
    outInfo->metaValue.knownValue.uEmbeddedValue = ir_make_direct_nyka_value(ir_get_param_from_nyka_immediate(uIR));
    outInfo->uIRandMetaFlags = uIR | (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA);
}
local_func_inl IRInfo ir_make_info_for_nyka_immediate_to(u64 uIRofTarget) {
    IRInfo result;
    ir_get_info_from_nyka_immediate(ir_make_nyka_immediate(uIRofTarget), &result);
    return result;
}

local_func_inl const u8* ir_get_ptr_to_data_from_known_non_embd_with_nykas(AKnownValue knownNyka)
{
    const u32* pNykaTable = reinterpret_cast<const u32*>(knownNyka.pPtrToRawData);
    u32 uNykaCount = pNykaTable[0];
    Assert_(uNykaCount);
    u32 uOffsetToPtrToData = align_to(8u, (uNykaCount+1u)*sizeof(u32));
    return *reinterpret_cast<const u8**>(knownNyka.pPtrToRawData + uOffsetToPtrToData);
}


local_func const u8* ir_get_ptr_to_data_from_known_non_embd(const IRInfo& info)
{
    Assert_(0uLL == (info.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD));
    if (0uLL == (info.uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
        return info.metaValue.knownValue.pPtrToRawData;
    } else {
        return ir_get_ptr_to_data_from_known_non_embd_with_nykas(info.metaValue.knownValue);
    }
}

local_func_inl const u8* ir_get_ptr_to_data_from_known(const IRInfo& info)
{
    if (info.uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD) {
        return reinterpret_cast<const u8*>(&info.metaValue.knownValue.uEmbeddedValue);
    } else {
        return ir_get_ptr_to_data_from_known_non_embd(info);
    }
}

constexpr IRInfo info0WhenEmbeddedIntegral { ir_make_int_immediate(0)|u64(IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_KNOWN_ZERO),
                                             meta_from_payload(0uLL)
                                           };

#endif // LOCLIB_IR_INFO_H_

