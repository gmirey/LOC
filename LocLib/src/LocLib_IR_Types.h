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

#ifndef LOCLIB_IR_TYPES_H_
#define LOCLIB_IR_TYPES_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/arithmetic_operations.h"

#include "LocLib_IR_Info.h"


local_func_inl u8 get_ir_format_for_addresses(CompilationContext* pEvalContext) {
    Assert_(pEvalContext->pCompilationParams->bPtrSizeIs64); // TODO when changes. If it ever changes. Which now that we have 64b NYKA seems complicated.
    return 0x03u;
}

#define IR_INSTR_STDFORMAT      0x08u                   // result format is to be taken directly from format slot
#define IR_INSTR_STDINT         0x18u                   // result format is to be taken as a mask of 3 lowest bits of format slot. other bits are reserved
#define IR_INSTR_NOVALUE        0xFFu                   // although there can very well be a format in the standard format slot, there is no "resulting" format per se: this instruction is not a value which can be referred to

#define IRPARAM_NONE                          0x00u     // there should be nothing in that param slot
#define IRPARAM_STANDARD                      0x01u     // there should be a standard IR param code in that param slot
#define IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN   0x02u     // in that param slot is statically-defined align-log2 (bits #32..#39... although should not be >12), and bits #0..#31 should hold a 32b slot count (although should not be > max of 2M)
#define IRPARAM_SPECIAL                       0x03u     // there is some other encoding in that param slot

#define IR_INSTR_REFERENCABLE   0x01u

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags)
#define IRIT_MAGIC_EMITTER_ \
        \
        /* base instructions range */ \
        \
        /* DECLARATIONS : standard format ; no first param ; in second param is slot count and align ; DECLARATION and GLOBAL_VAR_DECL should hold their initial value as meta. */ \
    IRIT_MAGIC_EMIT( DECLARATION,       IR_INSTR_STDFORMAT, IRPARAM_NONE,      IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, IR_INSTR_REFERENCABLE) \
    IRIT_MAGIC_EMIT( GLOBAL_VAR_DECL,   IR_INSTR_STDFORMAT, IRPARAM_NONE,      IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, IR_INSTR_REFERENCABLE) \
    IRIT_MAGIC_EMIT( LOCAL_VAR_DECL,    IR_INSTR_STDFORMAT, IRPARAM_NONE,      IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, IR_INSTR_REFERENCABLE) \
        /* DEREF : standard format ; in first param is address of what to deref ; in second param is slot count and align. */ \
    IRIT_MAGIC_EMIT( DEREF,             IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, IR_INSTR_REFERENCABLE) \
        /* IRIT_LOCAL_ADDRESS: in first param is IR of entity to reference by address. Should only be used on local, non-const (otherwise resulting IR is encodable as nyka immediate). */ \
    IRIT_MAGIC_EMIT( LOCAL_ADDRESS,     0x03u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_PTR_OFFSET: in first param is IR of base address. in format slot is index scale x1..x256. in instr flag is IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED for whether 32b index is u32 (otherwise i32), and 4lsb instr flags is log2 of ensured align. in second param is IR of index */ \
    IRIT_MAGIC_EMIT( PTR_OFFSET,        0x03u,              IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_PTR_OFFSET_EXT: in first param is IR of base address. in instr flag is IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED for whether index is unsigned, and 4lsb instr flags is log2 of ensured align. in format is nothing. in second param is index scale. */ \
        /* Awaits as additional param: the index proper (any scalar integral >= 32b). */ \
    IRIT_MAGIC_EMIT( PTR_OFFSET_EXT,    0x03u,              IRPARAM_STANDARD,  IRPARAM_SPECIAL                    , 0u                   ) \
        /* IRIT_PTR_DIFF: format is necessarily <= 0x05 => remaining 5b format + some bits of instr flags for small byte factors */ \
    IRIT_MAGIC_EMIT( PTR_DIFF,          IR_INSTR_STDINT,    IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_PTR_DIFF_EXT: use when byte factor is larger (encoded statically in second param) awaits as proc param: second arg. */ \
    IRIT_MAGIC_EMIT( PTR_DIFF_EXT,      IR_INSTR_STDINT,    IRPARAM_STANDARD,  IRPARAM_SPECIAL                    , 0u                   ) \
        /* Note: IRIT_REINTERP could be encoded as an IRIT_DEREF ; but this version can maybe work with reg-typed params. in 7lsb of instr flags is format of source. */ \
    IRIT_MAGIC_EMIT( REINTERP,          IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, 0u                   ) \
        /* IRIT_CAST: in format is format of *result* ; in 2nd param is format of *source* (and flags for when int: sign-extend?) */ \
    IRIT_MAGIC_EMIT( CAST,              IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_SPECIAL                    , 0u                   ) \
        /* IRIT_CALL: in format is call-convention ; in first param is address to go to. in 2nd param is static number of in params, static number of out params, static number of defaulted-inp */ \
    IRIT_MAGIC_EMIT( CALL,              IR_INSTR_NOVALUE,   IRPARAM_STANDARD,  IRPARAM_SPECIAL                    , 0u                   ) \
        /* CALLER_IN_PARAM: in 1st param is value to pass as in-param */ \
    IRIT_MAGIC_EMIT( CALLER_IN_PARAM,   IR_INSTR_NOVALUE,   IRPARAM_STANDARD,  IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, 0u                   ) \
        /* IRIT_CALLER_RET_PARAM: in 1st param is static position of 'call' instruction (or any other instruction awaiting for it) */ \
    IRIT_MAGIC_EMIT( CALLER_RET_PARAM,  IR_INSTR_STDFORMAT, IRPARAM_SPECIAL,   IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, 0u                   ) \
        /* IRIT_STORE: in first param is destination ; in second param is source. */ \
    IRIT_MAGIC_EMIT( STORE,             IR_INSTR_NOVALUE,   IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_CALLER_STORE_EXT: for when slot count is >1 : in 2nd param is slot count => awaits as proc param: source. */ \
    IRIT_MAGIC_EMIT( STORE_EXT,         IR_INSTR_NOVALUE,   IRPARAM_STANDARD,  IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, 0u                   ) \
        /* IRIT_CALLER_SETZERO: in 2nd param is slot count. */ \
    IRIT_MAGIC_EMIT( SETZERO,           IR_INSTR_NOVALUE,   IRPARAM_SPECIAL,   IRPARAM_STATIC_SLOT_COUNT_AND_ALIGN, 0u                   ) \
        /* IRIT_RET : indicator for setting up retparams, emitting outtro, and returning from current proc. */ \
    IRIT_MAGIC_EMIT( RET,               IR_INSTR_NOVALUE,   IRPARAM_NONE,      IRPARAM_NONE                       , 0u                   ) \
        \
        /* markers */ \
        \
    IRIT_MAGIC_EMIT( MARKER_JUMP_TARGET,IR_INSTR_NOVALUE,   IRPARAM_NONE,      IRPARAM_NONE                       , 0u                   ) \
    IRIT_MAGIC_EMIT( MARKER_START_SOURCE_SCOPE, IR_INSTR_NOVALUE,   IRPARAM_NONE,      IRPARAM_NONE               , 0u                   ) \
        /* IRIT_MARKER_END_SOURCE_SCOPE: in 1st param is static position of the associated 'block_open' marker */ \
    IRIT_MAGIC_EMIT( MARKER_END_SOURCE_SCOPE,IR_INSTR_NOVALUE,   IRPARAM_SPECIAL,  IRPARAM_NONE                       , 0u                   ) \
        \
        /* branching */ \
        \
        /* IRIT_GOTO: in 2nd param is static position of the associated 'jump_target' marker */ \
    IRIT_MAGIC_EMIT( GOTO,              IR_INSTR_NOVALUE,   IRPARAM_SPECIAL,   IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_BRANCH: in 2nd param is static position of the associated 'jump_target' marker ; in 1st param is value to test against zero or non-zero */ \
    IRIT_MAGIC_EMIT( BRANCH,            IR_INSTR_NOVALUE,   IRPARAM_SPECIAL,   IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_ERRCHK: in 2nd param is index of LocalErr, in 1st param is value to test against zero or non-zero ;   */ \
    IRIT_MAGIC_EMIT( ERRCHK,            IR_INSTR_NOVALUE,   IRPARAM_SPECIAL,   IRPARAM_STANDARD                   , 0u                   ) \
        \
        /* comparisons */ \
        \
        /* IRIT_CMP_EQ: works bitwise for integrals, or semantics of IEEE754 equality comp when fp. can be flaggued to return neq instead. can be flaggued for direct use by branch. */ \
    IRIT_MAGIC_EMIT( CMP_EQ,            0x00u,              IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_CMP_ORD: default strict lesser than can be flaggued to return the opposite instead (ge). can be flaggued with int semantics for sign. can be flaggued for direct use by branch. */ \
    IRIT_MAGIC_EMIT( CMP_ORD,           0x00u,              IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* 'bool not' is same as xor with 1... but left as distinct op for easier analysis - usually format is 0, but works with all integral */ \
    IRIT_MAGIC_EMIT( BOOL_NOT,          IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_PSEUDO_VALUED_COND: in format is original 'token' between A and B (and / or / not : no second param in case of not) */ \
    IRIT_MAGIC_EMIT( PSEUDO_VALUED_COND,0x00u,              IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        \
        /* basic bitwise */ \
        \
    IRIT_MAGIC_EMIT( BIT_NOT,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_AND,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_OR,            IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_XOR,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_LSH,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_RSH,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_ROL,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( BIT_ROR,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_BIT_CNT: counts the number of set bits - format is format of source - always returns an r16 */ \
    IRIT_MAGIC_EMIT( BIT_CNT,           0x01u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_BIT_CNT_LZ: counts the number of unset bits from msb towards lsb, stopping at first non zero - format is format of source - always returns an r16 */ \
    IRIT_MAGIC_EMIT( BIT_CNT_LZ,        0x01u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_BIT_CNT_TZ: counts the number of unset bits from lsb towards msb, stopping at first non zero - format is format of source - always returns an r16 */ \
    IRIT_MAGIC_EMIT( BIT_CNT_TZ,        0x01u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_BIT_ISODD_SET: returns bit parity of its argument (whether total number of set bits is odd) - format is format of source - always returns an r8 */ \
    IRIT_MAGIC_EMIT( BIT_ISODD,         0x00u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        \
        /* basic arithmetics */ \
        \
    IRIT_MAGIC_EMIT( NEG,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
    IRIT_MAGIC_EMIT( ABS,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_ISNEG: returns 1 when flagged as negative ; does return 1 for -0.0, for -infinity, and even for neg NaN (but will interrupt on SNaN). format is format of source - always returns an r8 */ \
    IRIT_MAGIC_EMIT( ISNEG,             0x00u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_SIGNFACTOR: returns either 1 or -1 in same format as source. can be flagged to return either 0, or sign of epsilon when 0. */ \
    IRIT_MAGIC_EMIT( SIGNFACTOR,        IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
        /* IRIT_ISNAN: returns 1 whether SNaN or QNaN, without interrupt - format is format of source - always returns an r8 */ \
    IRIT_MAGIC_EMIT( ISNAN,             0x00u,              IRPARAM_STANDARD,  IRPARAM_NONE                       , 0u                   ) \
    IRIT_MAGIC_EMIT( ADD,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( SUB,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( MUL,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_MUL_U: integral only. flags on mul are related to the *check* to perform => mul_u separate instruction for unsigned. */ \
    IRIT_MAGIC_EMIT( MUL_U,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_QUO: integral only. can be flaggued IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED. */ \
    IRIT_MAGIC_EMIT( QUO,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_EXACT_QUO: integral only. can be flaggued IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED. Same as rem, but followed by u8 is-err additional result (can be embedded as err check). Also, can be shortcut as bitshift even if signed. */ \
    IRIT_MAGIC_EMIT( EXACT_QUO,         IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_REM: integral only. can be flaggued IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED. */ \
    IRIT_MAGIC_EMIT( REM,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_POWN: format is format of first arg 'A' in A^b (integral or fp). can be flagged IR_INSTRFLAG_INT_SEMANTICS_UNSIGNED if integral. second arg 'b' is always expected i32 (but is better be >= 0 for integers, otherwise as bad as a divby0) */ \
    IRIT_MAGIC_EMIT( POWN,              IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_POW: necessarily fp. use 'POWN' for integer power */ \
    IRIT_MAGIC_EMIT( POW,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_DIV: necessarily fp. use 'QUO' for integer quotient */ \
    IRIT_MAGIC_EMIT( DIV,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        /* IRIT_MOD: integral or fp. integral supposed signed ; use 'REM' for modulus on unsigned integrals */ \
    IRIT_MAGIC_EMIT( MOD,               IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        \
        /* extended arithmetics */ \
        \
    IRIT_MAGIC_EMIT( ADD_CAR,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( SUB_BOR,           IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( MUL_HIGH,          IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( MUL_HIGH_U,        IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( QUO_LARGE,         IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( REM_LARGE,         IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
    IRIT_MAGIC_EMIT( MOD_LARGE,         IR_INSTR_STDFORMAT, IRPARAM_STANDARD,  IRPARAM_STANDARD                   , 0u                   ) \
        \
        \
    IRIT_MAGIC_EMIT( NO_OP,             IR_INSTR_NOVALUE,   IRPARAM_NONE,      IRPARAM_NONE                       , 0u                   )

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum IRInstructionType : u8 {
    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags) IRIT_ ## name ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tIRITStr[] = {
    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags) #name ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};
constexpr u8 COUNT_IRIT_INSTRUCTIONS = sizeof(tIRITStr) / sizeof(char*);

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITFormatSlot[] = {
    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags) result_format,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

constexpr bool has_irit_a_value(u8 uIRIT) {
    u8 uFormatKind = tIRITFormatSlot[uIRIT];    // output instruction format can be set fixed per IRIT instead of being encoded within the instruction slot.
    return uFormatKind != IR_INSTR_NOVALUE;
}

constexpr u8 get_outvalue_format_from_instruction(u8 uIRIT, u64 uInstrCodeAndFormatAndFirstParam) {
    u8 uFormatKind = tIRITFormatSlot[uIRIT];    // output instruction format can be set fixed per IRIT instead of being encoded within the instruction slot.
    Assert_(uFormatKind != IR_INSTR_NOVALUE);   // instruction should not be a novalue if we want to query its output format...
    if ((uFormatKind & 0x0Fu) == 0x08u) {       // if 4 low bits fixed for this IRIT are '0x08u', however, it denotes a standard format by instruction slot
        static_assert((IR_INSTR_STDINT & 0x0Fu) == 0x08u, "hello?");
        static_assert((IR_INSTR_STDFORMAT & 0x0Fu) == 0x08u, "hola??");
        u8 uFormat = u8(uInstrCodeAndFormatAndFirstParam >> 16);   // ...which are encoded as 8b at offset 16 in first part of the IR entry
        if (uFormatKind == IR_INSTR_STDINT) {                   // if IRIT is maked as STDINT, however...
            uFormat = uFormat & 0x07u;                          // ...format is restricted to scalar integral and only encoded in the 3 lowest bits of that field.
        } else {
            Assert_(uFormatKind == IR_INSTR_STDFORMAT);         // ...otherwise we check this IRIT is indeed marked as standard format
        }
        return uFormat;
    } else
        return uFormatKind;
}

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITFirstParamSlot[] = {
    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags) first_param_slot_kind,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITSecondParamSlot[] = {
    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags) second_param_slot_kind,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITFlagSlot[] = {
    #define IRIT_MAGIC_EMIT(name, result_format, first_param_slot_kind, second_param_slot_kind, flags) flags,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

#if 0

local_func IRInfo ir_solve_int_immediate_as(u8 uDestFormat, u64 uImmediateValue) {
    Assert_(uDestFormat <= 0x07u);
    Assert_(ir_is_valid_param(uImmediateValue));
    Assert_(ir_is_non_nyka_immediate(uImmediateValue));
    IRInfo result;
    result.uIRandMetaFlags = uImmediateValue|IRFLAG_IS_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
    result.metaValue.knownValue._payload = u64(u32(uImmediateValue >> IR_STD_PARAM_SHIFT));
    if (ir_is_large_neg_immediate(uImmediateValue)) {
        if (uDestFormat > 0x02u) {
            result.metaValue.knownValue._payload |= 0xFFFF'FFFF'0000'0000uLL;
            if (uDestFormat > 0x03u) {
                result.uIRandMetaFlags |= IRFLAG_IS_EMBD_NEG_EXT|IRFLAG_IS_EMBD_SIGN_EXT;
            }
        }
    } else {
        if (uDestFormat > 0x03u) {
            result.uIRandMetaFlags |= IRFLAG_IS_EMBD_SIGN_EXT;
        }
    }
    return result;
}

local_func bool is_large_int_embeddable(const u64* tLegs, u32 uLegsCount, u32* outFlags) {
    if (uLegsCount <= 1u) {
        *outFlags = IRFLAG_IS_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
        return true;
    }
    u32 uLastLegIndex = uLegsCount-1u;
    u64 uLastLegValue = tLegs[uLastLegIndex];
    if (uLastLegValue == 0xFFFF'FFFF'FFFF'FFFFuLL) {
        for (u32 uIndex = uLastLegIndex-1u; uIndex; uIndex--) {
            if (tLegs[uIndex] != 0xFFFF'FFFF'FFFF'FFFFuLL)
                return false;
        }
        *outFlags = IRFLAG_IS_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_IS_EMBD_NEG_EXT;
        if (tLegs[0] & 0x8000'0000'0000'0000) {
            *outFlags |= IRFLAG_IS_EMBD_SIGN_EXT;
        }
        return true;
    } else if (uLastLegValue == 0uLL) {
        for (u32 uIndex = uLastLegIndex-1u; uIndex; uIndex--) {
            if (tLegs[uIndex])
                return false;
        }
        *outFlags = IRFLAG_IS_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
        if (0 == (tLegs[0] & 0x8000'0000'0000'0000)) {
            *outFlags |= IRFLAG_IS_EMBD_SIGN_EXT;
        }
        return true;
    }
    return false;
}

local_func bool ir_try_upgrade_compint_with_ir_of_immediate(IRInfo* ioInfo)
{
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_CONST);
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN);
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD);
    Assert_(0 == (ioInfo->uIRandMetaFlags & IRFLAG_HAS_NYKA));
    u64 uPayload64 = ioInfo->metaValue.knownValue.uEmbeddedValue;
    if (0 == (uPayload64 & 0x03u)) {
        u64 uAbsValue64b = uPayload64 >> 3;
        u8 uIsNeg = u8(uPayload64) & 0x04u;
        if (uIsNeg) {
            i64 iValue64b = i64(-uAbsValue64b);
            if (iValue64b >= -0x0000'0001'0000'0000) {
                ioInfo->uIRandMetaFlags |= ir_make_large_neg_immediate(u32(iValue64b));
                return true;
            }
        } else {
            if (uAbsValue64b < 0x0000'0001'0000'0000uLL) {
                ioInfo->uIRandMetaFlags |= ir_make_r32_immediate(u32(uAbsValue64b));
                return true;
            }
        }        
    }
    return false;
}

local_func bool ir_try_upgrade_const_integral_with_ir_of_immediate(IRInfo* ioInfo, bool bIsAtMost64b)
{
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_CONST);
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN);
    if (0 != (ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD) && 0 == (ioInfo->uIRandMetaFlags & IRFLAG_HAS_NYKA)) {
        if (bIsAtMost64b || 0 != (ioInfo->uIRandMetaFlags & IRFLAG_IS_EMBD_SIGN_EXT)) {
            u64 uPayload64 = ioInfo->metaValue.knownValue.uEmbeddedValue;
            u32 uHighOfEmbedded = u32(uPayload64 >> 32);
            if (uHighOfEmbedded) {
                if (uHighOfEmbedded == 0xFFFF'FFFFu) {
                    Assert_(bIsAtMost64b || 0 != (ioInfo->uIRandMetaFlags & IRFLAG_IS_EMBD_NEG_EXT));
                    ioInfo->uIRandMetaFlags |= ir_make_large_neg_immediate(u32(uPayload64));
                    return true;
                }
            } else {
                Assert_(0 == (ioInfo->uIRandMetaFlags & IRFLAG_IS_EMBD_NEG_EXT));
                ioInfo->uIRandMetaFlags |= ir_make_r32_immediate(u32(uPayload64));
                return true;
            }
        }
    }
    return false;
}

local_func bool ir_try_upgrade_const_fp_with_ir_of_immediate(IRInfo* ioInfo)
{
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_CONST);
    Assert_(ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN);
    Assert_(0 == (ioInfo->uIRandMetaFlags & IRFLAG_HAS_NYKA));
    if (ioInfo->uIRandMetaFlags & IRFLAG_IS_KNOWN_EMBD) {
        u64 uValuePayload = ioInfo->metaValue.knownValue.uEmbeddedValue;
        u64 uResultSingleLegFP32; u8 uLostPrec;
        fp_static_downcast_to<1u, 2u>(&uResultSingleLegFP32, &uValuePayload, &uLostPrec);
        if (uLostPrec == 0u) {
            ioInfo->uIRandMetaFlags |= ir_make_r32_immediate(u32(uResultSingleLegFP32));
            return true;
        }
    }
    return false;
}

local_func void ir_get_meta_from_non_nyka_immediate(u64 uIRParam, u32* outFlags, MetaValueIR* outMeta, u8 uFormat, u32 uSlotsCount) {
    Assert_(ir_is_valid_param(uIRParam));
    Assert_(ir_is_non_nyka_immediate(uIRParam));
    Assert(uSlotsCount, "ir_get_meta_from_immediate() : zero-slot count not (yet?) supported");
    // TODO
    Assert(uSlotsCount == 1u, "ir_get_meta_from_immediate() : multi-slot count not yet supported");
    // TODO
    Assert(0 == (uFormat & 0xF0u), "ir_get_meta_from_immediate() : vector formats not yet supported");

    *outFlags = IRFLAG_IS_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
    u32 uPayload32 = u32(uIRParam >> IR_STD_PARAM_SHIFT);
    if (uFormat & 0x08u) {
        Assert_(ir_is_non_nyka_immediate(uIRParam) && !ir_is_large_neg_immediate(uIRParam));
        u64 uPayloadF32asLeg = u64(uPayload32);
        switch (uFormat & 0x07u) {

            case 0x01u: { // f16
                u64 uPayloadF16asLeg; u8 uExpectedZero;
                fp_static_downcast_to<0u, 1u>(&uPayloadF16asLeg, &uPayloadF32asLeg, &uExpectedZero);
                Assert_(0 == uExpectedZero);
                outMeta->knownValue.uEmbeddedValue = uPayloadF16asLeg;
            } break;

            case 0x02u: { // f32
                outMeta->knownValue.uEmbeddedValue = uPayloadF32asLeg;
            } break;

            case 0x03u: // f64
            case 0x04u: // f128
            case 0x05u: // f256
            {
                u64 uPayloadF64;
                fp_static_upcast_to<2u, 1u>(&uPayloadF64, &uPayloadF32asLeg);
                outMeta->knownValue.uEmbeddedValue = uPayloadF64;
            } break;

            default:
                Assume_(false);
        }

    } else {
        if (!ir_is_large_neg_immediate(uIRParam)) {
            switch (uFormat) {
                case 0x00u: { // r8
                    Assert_(uPayload32 < 0x0000'0100u);
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32);
                } break;
                case 0x01u: { // r16
                    Assert_(uPayload32 < 0x0001'0000u);
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32);
                } break;
                case 0x02u: { // r32
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32);
                } break;
                case 0x03u: { // r64
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32);
                } break;
                case 0x04u: // r128
                case 0x05u: // r256
                case 0x06u: // r512
                case 0x07u: // r1024
                {
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32);
                    *outFlags |= IRFLAG_IS_EMBD_SIGN_EXT;
                } break;
                default:
                    Assume_(false);
            }

        } else { Assert_(ir_is_large_neg_immediate(uIRParam));
            switch (uFormat) {
                case 0x00u: { // r8
                    Assert_(uPayload32 < 0x0000'0100u || i32(uPayload32) >= 0xFFFF'FF80);
                    outMeta->knownValue.uEmbeddedValue = u64(u8(uPayload32));
                } break;
                case 0x01u: { // r16
                    Assert_(uPayload32 < 0x0001'0000u || i32(uPayload32) >= 0xFFFF'8000);
                    outMeta->knownValue.uEmbeddedValue = u64(u16(uPayload32));
                } break;
                case 0x02u: { // r32
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32);
                } break;
                case 0x03u: { // r64
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32) | 0xFFFF'FFFF'0000'0000uLL;
                } break;
                case 0x04u: // r128
                case 0x05u: // r256
                case 0x06u: // r512
                case 0x07u: // r1024
                {
                    outMeta->knownValue.uEmbeddedValue = u64(uPayload32) | 0xFFFF'FFFF'0000'0000uLL;
                    *outFlags |= IRFLAG_IS_EMBD_SIGN_EXT|IRFLAG_IS_EMBD_NEG_EXT;
                } break;
                default:
                    Assume_(false);
            }
        }
    }
}


local_func IRInfo ir_solve_known_int_reinterp_as(u8 uDestFormat, u8 uBaseFormat, u32 uBaseFlags, MetaValueIR baseMeta) {
    Assert_(uDestFormat <= uBaseFormat);
    Assert_(uBaseFormat <= 0x07u);
    Assert_(uBaseFlags & IRFLAG_IS_CONST);
    Assert_(uBaseFlags & IRFLAG_IS_KNOWN);
    IRInfo result;

    // First, we must decide of the representation of the 'meta' itself...

    if (uDestFormat == uBaseFormat) { // Same dest format as source => simply copy same flags and meta
        result.uIRandMetaFlags = uBaseFlags;
        result.metaValue = baseMeta;

    } else {                          // Distinct dest format (necessarily smaller!) 

        if (uDestFormat > 0x03u) {    // possibly non-embedded-dest...
            if (uBaseFlags & IRFLAG_IS_KNOWN_EMBD) {    // from a possibly non-embedded-source which happens to be embedded
                result.uIRandMetaFlags = uBaseFlags;            // => same embedded representation and flags
                result.metaValue = baseMeta;
            } else {                                        // from a non-embedded-source
                u32 uFlagsIfEmbeddable;
                u32 uLegsCount = 1u << (uDestFormat-0x03u);     // => pointer to same data, unless embeddable when truncated
                if (is_large_int_embeddable(baseMeta.knownValue.tLegs, uLegsCount, &uFlagsIfEmbeddable)) {
                    result.uIRandMetaFlags = uFlagsIfEmbeddable;
                    result.metaValue.knownValue.uEmbeddedValue = baseMeta.knownValue.tLegs[0];
                } else {
                    result.uIRandMetaFlags = uBaseFlags;        // non-embeddable when truncated => pointer to same data
                    result.metaValue.knownValue.tLegs = baseMeta.knownValue.tLegs;
                }
            }

        } else {                        // always-embedded-dest...
            result.uIRandMetaFlags = IRFLAG_IS_CONST|IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD;
            u64 uSmallestLeg = (uBaseFlags & IRFLAG_IS_KNOWN_EMBD) ? baseMeta.knownValue.uEmbeddedValue : baseMeta.knownValue.tLegs[0];
            switch(uDestFormat) {
                case 0x00u: result.metaValue.knownValue.uEmbeddedValue = u64(u8(uSmallestLeg)); break;
                case 0x01u: result.metaValue.knownValue.uEmbeddedValue = u64(u16(uSmallestLeg)); break;
                case 0x02u: result.metaValue.knownValue.uEmbeddedValue = u64(u32(uSmallestLeg)); break;
                case 0x03u: result.metaValue.knownValue.uEmbeddedValue = uSmallestLeg; break;
                default: Assume_(false);
            }
        }
    }

    // Whatever the case, we try to also return an immediate IR when possible
    ir_try_upgrade_const_integral_with_ir_of_immediate(&result, uDestFormat <= 0x03u);

    // we're done!
    return result;
}

#endif

#if 0 // TMP TMP

#define DEFAULT_FLAGS_UNASSIGNED        (IRFLAG_IS_NOT_AN_INSTRUCTION|IRFLAG_IS_NOT_A_VALUE)
#define DEFAULT_FLAGS_DECL_STD          (IRFLAG_IS_DECLARATION|IRFLAG_HAS_NYKA_WITHIN)
#define DEFAULT_FLAGS_DECL_OTHER        (IRFLAG_IS_DECLARATION|IRFLAG_IS_NOT_A_VALUE)

#define NO_IR_FORMAT                0x80u
#define STD_IR_FORMAT               0x81u
#define TAGGED_IR_FORMAT            0x82u
#define NOT_AN_IR_FORMAT            0xFFu

#define NO_IR_PARAM                 0x00u
#define STD_IR_PARAM                0x01u
#define STATIC_SLOTPARAM            0x02u
#define STATIC_ALIGNPARAM           0x03u
#define STATIC_OTHERPARAM           0x04u
#define NOT_AN_IR_PARAM             0xFFu

#define STANDARD_IRIT_CODE          0x01u
#define TAGGEDPTR_ALIGN8            0x08u
#define TAGGEDPTR_ALIGN16_1         0x11u
#define TAGGEDPTR_ALIGN16_2         0x12u
#define TAGGEDPTR_ALIGN16_3         0x13u

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags)
#define IRIT_MAGIC_EMITTER_ \
        \
        /* 0x00 range : basic declarations */ \
    IRIT_MAGIC_EMIT( 0x00u,         UNUSED_IR_00,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x01u,    DECLARE_TYPEPTR_0,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x02u,         UNUSED_IR_02,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x03u,         UNUSED_IR_03,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x04u,   IRIT_DECL_CONSTANT,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STATIC_ALIGNPARAM, STATIC_SLOTPARAM,    DEFAULT_FLAGS_DECL_STD) \
    IRIT_MAGIC_EMIT( 0x05u,   IRIT_DECL_VARIABLE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STATIC_ALIGNPARAM, STATIC_SLOTPARAM,    DEFAULT_FLAGS_DECL_STD) \
    IRIT_MAGIC_EMIT( 0x06u,   IRIT_DECL_PROCBODY,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STATIC_OTHERPARAM, STATIC_SLOTPARAM,    DEFAULT_FLAGS_DECL_STD) \
    IRIT_MAGIC_EMIT( 0x07u,       IRIT_DECL_TYPE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STATIC_OTHERPARAM, STATIC_SLOTPARAM,    DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x08u,         UNUSED_IR_08,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x09u,  IRIT_OTHRFILE_CONST,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STATIC_OTHERPARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x0Au,  IRIT_OTHER_FILE_VAR,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STATIC_OTHERPARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x0Bu, IRIT_OTHER_FILE_PROC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STATIC_OTHERPARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x0Cu, IRIT_OTHER_FILE_TYPE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STATIC_OTHERPARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x0Du, IRIT_OTHRFILE_LCONST,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STATIC_OTHERPARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x0Eu,   IRIT_OTHRFILE_LVAR,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x0Fu,  IRIT_OTHRFILE_LTYPE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x10 range */ \
    IRIT_MAGIC_EMIT( 0x10u,         UNUSED_IR_10,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x11u,    DECLARE_TYPEPTR_1,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x12u,         UNUSED_IR_12,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x13u,         UNUSED_IR_13,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x14u,         UNUSED_IR_14,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x15u,         UNUSED_IR_15,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x16u,         UNUSED_IR_16,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x17u,         UNUSED_IR_17,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x18u,         UNUSED_IR_18,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x19u,         UNUSED_IR_19,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x1Au,         UNUSED_IR_1A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x1Bu,         UNUSED_IR_1B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x1Cu,         UNUSED_IR_1C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x1Du,         UNUSED_IR_1D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x1Eu,         UNUSED_IR_1E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x1Fu,         UNUSED_IR_1F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x20 range */ \
    IRIT_MAGIC_EMIT( 0x20u,         UNUSED_IR_20,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x21u,    DECLARE_TYPEPTR_2,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x22u,         UNUSED_IR_22,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x23u,         UNUSED_IR_23,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x24u,         UNUSED_IR_24,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x25u,         UNUSED_IR_25,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x26u,         UNUSED_IR_26,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x27u,         UNUSED_IR_27,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x28u,         UNUSED_IR_28,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x29u,         UNUSED_IR_29,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x2Au,         UNUSED_IR_2A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x2Bu,         UNUSED_IR_2B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x2Cu,         UNUSED_IR_2C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x2Du,         UNUSED_IR_2D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x2Eu,         UNUSED_IR_2E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x2Fu,         UNUSED_IR_2F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x30 range */ \
    IRIT_MAGIC_EMIT( 0x30u,         UNUSED_IR_30,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x31u,    DECLARE_TYPEPTR_3,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x32u,         UNUSED_IR_32,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x33u,         UNUSED_IR_33,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x34u,         UNUSED_IR_34,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x35u,         UNUSED_IR_35,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x36u,         UNUSED_IR_36,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x37u,         UNUSED_IR_37,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x38u,         UNUSED_IR_38,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x39u,         UNUSED_IR_39,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x3Au,         UNUSED_IR_3A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x3Bu,         UNUSED_IR_3B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x3Cu,         UNUSED_IR_3C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x3Du,         UNUSED_IR_3D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x3Eu,         UNUSED_IR_3E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x3Fu,         UNUSED_IR_3F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x40 range */ \
    IRIT_MAGIC_EMIT( 0x40u,         UNUSED_IR_40,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x41u,    DECLARE_TYPEPTR_4,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x42u,         UNUSED_IR_42,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x43u,         UNUSED_IR_43,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x44u,         UNUSED_IR_44,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x45u,         UNUSED_IR_45,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x46u,         UNUSED_IR_46,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x47u,         UNUSED_IR_47,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x48u,         UNUSED_IR_48,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x49u,         UNUSED_IR_49,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x4Au,         UNUSED_IR_4A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x4Bu,         UNUSED_IR_4B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x4Cu,         UNUSED_IR_4C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x4Du,         UNUSED_IR_4D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x4Eu,         UNUSED_IR_4E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x4Fu,         UNUSED_IR_4F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x50 range */ \
    IRIT_MAGIC_EMIT( 0x50u,         UNUSED_IR_50,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x51u,    DECLARE_TYPEPTR_5,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x52u,         UNUSED_IR_52,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x53u,         UNUSED_IR_53,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x54u,         UNUSED_IR_54,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x55u,         UNUSED_IR_55,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x56u,         UNUSED_IR_56,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x57u,         UNUSED_IR_57,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x58u,         UNUSED_IR_58,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x59u,         UNUSED_IR_59,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x5Au,         UNUSED_IR_5A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x5Bu,         UNUSED_IR_5B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x5Cu,         UNUSED_IR_5C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x5Du,         UNUSED_IR_5D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x5Eu,         UNUSED_IR_5E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x5Fu,         UNUSED_IR_5F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x60 range */ \
    IRIT_MAGIC_EMIT( 0x60u,         UNUSED_IR_60,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x61u,    DECLARE_TYPEPTR_6,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x62u,         UNUSED_IR_62,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x63u,         UNUSED_IR_63,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x64u,         UNUSED_IR_64,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x65u,         UNUSED_IR_65,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x66u,         UNUSED_IR_66,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x67u,         UNUSED_IR_67,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x68u,         UNUSED_IR_68,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x69u,         UNUSED_IR_69,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x6Au,         UNUSED_IR_6A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x6Bu,         UNUSED_IR_6B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x6Cu,         UNUSED_IR_6C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x6Du,         UNUSED_IR_6D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x6Eu,         UNUSED_IR_6E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x6Fu,         UNUSED_IR_6F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x70 range */ \
    IRIT_MAGIC_EMIT( 0x70u,         UNUSED_IR_70,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x71u,    DECLARE_TYPEPTR_7,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x72u,         UNUSED_IR_72,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x73u,         UNUSED_IR_73,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x74u,         UNUSED_IR_74,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x75u,         UNUSED_IR_75,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x76u,         UNUSED_IR_76,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x77u,         UNUSED_IR_77,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x78u,         UNUSED_IR_78,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x79u,         UNUSED_IR_79,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x7Au,         UNUSED_IR_7A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x7Bu,         UNUSED_IR_7B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x7Cu,         UNUSED_IR_7C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x7Du,         UNUSED_IR_7D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x7Eu,         UNUSED_IR_7E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x7Fu,         UNUSED_IR_7F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x80 range */ \
    IRIT_MAGIC_EMIT( 0x80u,         UNUSED_IR_80,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x81u,    DECLARE_TYPEPTR_8,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x82u,         UNUSED_IR_82,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x83u,         UNUSED_IR_83,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x84u,         UNUSED_IR_84,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x85u,         UNUSED_IR_85,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x86u,         UNUSED_IR_86,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x87u,         UNUSED_IR_87,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x88u,         UNUSED_IR_88,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x89u,         UNUSED_IR_89,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x8Au,         UNUSED_IR_8A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x8Bu,         UNUSED_IR_8B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x8Cu,         UNUSED_IR_8C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x8Du,         UNUSED_IR_8D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x8Eu,         UNUSED_IR_8E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x8Fu,         UNUSED_IR_8F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0x90 range */ \
    IRIT_MAGIC_EMIT( 0x90u,         UNUSED_IR_90,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x91u,    DECLARE_TYPEPTR_9,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x92u,         UNUSED_IR_92,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x93u,         UNUSED_IR_93,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x94u,         UNUSED_IR_94,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x95u,         UNUSED_IR_95,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x96u,         UNUSED_IR_96,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x97u,         UNUSED_IR_97,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x98u,         UNUSED_IR_98,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0x99u,         UNUSED_IR_99,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x9Au,         UNUSED_IR_9A,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x9Bu,         UNUSED_IR_9B,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x9Cu,         UNUSED_IR_9C,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x9Du,         UNUSED_IR_9D,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x9Eu,         UNUSED_IR_9E,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0x9Fu,         UNUSED_IR_9F,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0xA0 range */ \
    IRIT_MAGIC_EMIT( 0xA0u,         UNUSED_IR_A0,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xA1u,    DECLARE_TYPEPTR_A,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xA2u,         UNUSED_IR_A2,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xA3u,         UNUSED_IR_A3,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xA4u,         UNUSED_IR_A4,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xA5u,         UNUSED_IR_A5,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xA6u,         UNUSED_IR_A6,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xA7u,         UNUSED_IR_A7,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xA8u,         UNUSED_IR_A8,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xA9u,         UNUSED_IR_A9,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xAAu,         UNUSED_IR_AA,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xABu,         UNUSED_IR_AB,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xACu,         UNUSED_IR_AC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xADu,         UNUSED_IR_AD,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xAEu,         UNUSED_IR_AE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xAFu,         UNUSED_IR_AF,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0xB0 range */ \
    IRIT_MAGIC_EMIT( 0xB0u,         UNUSED_IR_B0,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xB1u,    DECLARE_TYPEPTR_B,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xB2u,         UNUSED_IR_B2,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xB3u,         UNUSED_IR_B3,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xB4u,         UNUSED_IR_B4,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xB5u,         UNUSED_IR_B5,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xB6u,         UNUSED_IR_B6,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xB7u,         UNUSED_IR_B7,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xB8u,         UNUSED_IR_B8,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xB9u,         UNUSED_IR_B9,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xBAu,         UNUSED_IR_BA,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xBBu,         UNUSED_IR_BB,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xBCu,         UNUSED_IR_BC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xBDu,         UNUSED_IR_BD,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xBEu,         UNUSED_IR_BE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xBFu,         UNUSED_IR_BF,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0xC0 range */ \
    IRIT_MAGIC_EMIT( 0xC0u,         UNUSED_IR_C0,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xC1u,    DECLARE_TYPEPTR_C,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xC2u,         UNUSED_IR_C2,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xC3u,         UNUSED_IR_C3,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xC4u,         UNUSED_IR_C4,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xC5u,         UNUSED_IR_C5,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xC6u,         UNUSED_IR_C6,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xC7u,         UNUSED_IR_C7,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xC8u,         UNUSED_IR_C8,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xC9u,         UNUSED_IR_C9,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xCAu,         UNUSED_IR_CA,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xCBu,         UNUSED_IR_CB,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xCCu,         UNUSED_IR_CC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xCDu,         UNUSED_IR_CD,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xCEu,         UNUSED_IR_CE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xCFu,         UNUSED_IR_CF,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0xD0 range */ \
    IRIT_MAGIC_EMIT( 0xD0u,         UNUSED_IR_D0,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xD1u,    DECLARE_TYPEPTR_D,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xD2u,         UNUSED_IR_D2,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xD3u,         UNUSED_IR_D3,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xD4u,         UNUSED_IR_D4,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xD5u,         UNUSED_IR_D5,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xD6u,         UNUSED_IR_D6,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xD7u,         UNUSED_IR_D7,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xD8u,         UNUSED_IR_D8,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xD9u,         UNUSED_IR_D9,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xDAu,         UNUSED_IR_DA,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xDBu,         UNUSED_IR_DB,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xDCu,         UNUSED_IR_DC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xDDu,         UNUSED_IR_DD,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xDEu,         UNUSED_IR_DE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xDFu,         UNUSED_IR_DF,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0xE0 range */ \
    IRIT_MAGIC_EMIT( 0xE0u,         UNUSED_IR_E0,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xE1u,    DECLARE_TYPEPTR_E,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xE2u,         UNUSED_IR_E2,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xE3u,         UNUSED_IR_E3,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xE4u,         UNUSED_IR_E4,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xE5u,         UNUSED_IR_E5,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xE6u,         UNUSED_IR_E6,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xE7u,         UNUSED_IR_E7,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xE8u,         UNUSED_IR_E8,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xE9u,         UNUSED_IR_E9,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xEAu,         UNUSED_IR_EA,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xEBu,         UNUSED_IR_EB,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xECu,         UNUSED_IR_EC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xEDu,         UNUSED_IR_ED,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xEEu,         UNUSED_IR_EE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xEFu,         UNUSED_IR_EF,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
        /* 0xF0 range */ \
    IRIT_MAGIC_EMIT( 0xF0u,         UNUSED_IR_F0,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xF1u,    DECLARE_TYPEPTR_F,  TAGGEDPTR_ALIGN16_1,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xF2u,         UNUSED_IR_F2,  TAGGEDPTR_ALIGN16_2,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xF3u,         UNUSED_IR_F3,  TAGGEDPTR_ALIGN16_3,     NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xF4u,         UNUSED_IR_F4,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xF5u,         UNUSED_IR_F5,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xF6u,         UNUSED_IR_F6,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xF7u,         UNUSED_IR_F7,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xF8u,         UNUSED_IR_F8,  TAGGEDPTR_ALIGN8,        NOT_AN_IR_FORMAT,   NOT_AN_IR_PARAM,   NOT_AN_IR_PARAM,     DEFAULT_FLAGS_UNASSIGNED) \
        \
    IRIT_MAGIC_EMIT( 0xF9u,         UNUSED_IR_F9,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xFAu,         UNUSED_IR_FA,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xFBu,         UNUSED_IR_FB,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xFCu,         UNUSED_IR_FC,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xFDu,         UNUSED_IR_FD,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xFEu,         UNUSED_IR_FE,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED) \
    IRIT_MAGIC_EMIT( 0xFFu,         UNUSED_IR_FF,  STANDARD_IRIT_CODE,      STD_IR_FORMAT,      STD_IR_PARAM,      STD_IR_PARAM,        DEFAULT_FLAGS_UNASSIGNED)

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
enum IRInstructionType : u8 {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        name = value ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr char const* tIRITStr[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        #name ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};
static_assert(256u == sizeof(tIRITStr) / sizeof(char*), "missing IRIT codes in emitter");

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITCodeSlot[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        irit_code_slot_kind ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITFormatSlot[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        format_slot_kind ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITFirstParamSlot[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        first_param_slot_kind ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u8 tIRITSecondParamSlot[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        second_param_slot_kind ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u32 tIRITDefaultFlags[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        default_meta_flags ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

// Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
constexpr u16 tIRITFormatSlot[] = {
    #define IRIT_MAGIC_EMIT(value, name, irit_code_slot_kind, format_slot_kind, first_param_slot_kind, second_param_slot_kind, default_meta_flags) \
        format_slot_kind ,
        IRIT_MAGIC_EMITTER_
    #undef IRIT_MAGIC_EMIT
};

//   usually, this is simply the index of the AST instruction (23b max for max 8 millions instructions in a single proc)... 
//
/*
enum ETranscendOpKind {
    ETRANS_OP_EXP,
    ETRANS_OP_POW10,
    ETRANS_OP_POW2,
    ETRANS_OP_EXP_ONEMINUSX,
    ETRANS_OP_POW10_ONEMINUSX,
    ETRANS_OP_POW2_ONEMINUSX,
    ETRANS_OP_LN,
    ETRANS_OP_LOG10,
    ETRANS_OP_LOG2,
    ETRANS_OP_LN_ONEPLUSX,
    ETRANS_OP_LOG10_ONEPLUSX,
    ETRANS_OP_LOG2_ONEPLUSX,
    ETRANS_OP_INVSQRT,
    ETRANS_OP_SQRT,
    ETRANS_OP_SINH,
    ETRANS_OP_COSH,
    ETRANS_OP_TANH,
    ETRANS_OP_ASINH,
    ETRANS_OP_ACOSH,
    ETRANS_OP_ATANH,
    ETRANS_OP_SIN,
    ETRANS_OP_COS,
    ETRANS_OP_TAN,
    ETRANS_OP_ASIN,
    ETRANS_OP_ACOS,
    ETRANS_OP_ATAN,
    ETRANS_OP_SIN_DEG,
    ETRANS_OP_COS_DEG,
    ETRANS_OP_TAN_DEG,
    ETRANS_OP_ASIN_DEG,
    ETRANS_OP_ACOS_DEG,
    ETRANS_OP_ATAN_DEG,
    ETRANS_OP_SIN_BRAD8,
    ETRANS_OP_COS_BRAD8,
    ETRANS_OP_TAN_BRAD8,
    ETRANS_OP_SIN_BRAD16,
    ETRANS_OP_COS_BRAD16,
    ETRANS_OP_TAN_BRAD16,
    ETRANS_OP_SIN_BRAD32,
    ETRANS_OP_COS_BRAD32,
    ETRANS_OP_TAN_BRAD32,
    ETRANS_OP_SIN_BRAD64,
    ETRANS_OP_COS_BRAD64,
    ETRANS_OP_TAN_BRAD64,
    ETRANS_OP_SIN_BRAD128,
    ETRANS_OP_COS_BRAD128,
    ETRANS_OP_TAN_BRAD128,
};

enum EBradInvTrigKind {
    BRAD_INVTRIG_ASIN,
    BRAD_INVTRIG_ACOS,
    BRAD_INVTRIG_ATAN,
};
*/

#define IRITMULTI_MASK16            0x0Fu

// if binding: whole 64b IR instruction is the comptime address of a 16Bytes-aligned ValueBinding
#define IRITMULTI_IS_BINDING        0x00u

// if extended : coming after the 4 LSB 0001:
// 12b extended code (4096 possibles), 24b IRfirstParam(if any), 24b IRsecondParam(if any)
#define IRITMULTI_IS_EXTENDED       0x01u

enum IRInstructionType : u8 {

    IRIT_BINDING_0 = 0,         // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_0 = 1,        // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // basic declaration and referencing instructions
    //

    // local vars + proc-intro params in and out (if at position == proc-param decl index)
    // can be used as value or ref
    // 8b format, 24b log2 of req align (high bits unused), 24b slot-count (16M max)
    IRIT_RESERVE_LOCAL_VAR = 2,

    // offset from an otherwise known base, when resulting format is known same as format of base and within range
    // can be used as value or ref
    // 8b format, 24b IR of base, 24b IR of offset, in multiples of the format footprint (as 32b signed int)
    IRIT_OFFSET = 3,

    // offset from an otherwise known base, when either non-ensured within valid range, or resulting format is distinct
    //   from format of base. Also differs from 'IRIT_OFFSET' in that the offset arg can be distinct from a 32b value
    // can be used as value or ref
    // 8b format of result, 24b IR of base, 8b format of base, 1b isKnownValidRange, 1b is Bytewise, 4b ensuredAlign, 10b unused.
    //   awaits as additional in-param : 24b IR of offset (as signed scalar integral).
    //                                   if 'isBytewise' is set on the value, offset is to be taken as bytewise
    //                                   otherwise, offset in expected expressed in multiples of the resulting format.
    IRIT_OFFSET_EXT = 4,

    // represents a value in another format, reinterpreting lesser or equal bitcount from source to dest format
    // can be used as value only
    // 8b format of result, 24b IR of base, 8b format of base
    IRIT_REINTERP = 5,

    // representing accessing memory pointed to by its long-address value (for read OR write)
    // can be used as value or ref
    // Note that it can represent unaligned access if ensured align is lower than native align for formant
    // 8b primary format of result, 24b ensured align (high bits unused), 24b IR of value holding the address
    IRIT_DEREF_LONG_ADDRESS = 6,

    // representing taking the long-address of its current definition
    // can be used as value only
    // 8b primary format of result == ensured 0x03, 24b ensured align (high bits unused), 24b IR of value to retrieve address of
    IRIT_LONG_ADDRESS = 7,

    // explicitely stores a value to a mutable slot
    // has no value of its own
    // 8b format, 24b IR of slot to store to, 24b IR of value to store
    IRIT_STORE = 8,

    // Explicitely stores a multi-slot value to another. This is distinct from memcpy since it is better at keeping slot-provenance info.
    // has no value of its own
    // 8b format, 24b IR of base slot to store to, 24b IR of base slot for value to store
    //   awaits as additional in-param : 24b IR of slot count (expects int immediate > 1)
    IRIT_STORE_MULTI = 9,

    // resets a range of slots to all-zeroes
    // has no value of its own
    // 8b format, 24b IR of base slot to reset, 24b IR of slot count (u32)
    IRIT_RESET_TO_ZERO = 10,

    // copies a range of memory by source and dest addresses. can also represent 'memmove'
    // has no value of its own
    // 4b ensured align dest, 4b ensured align src, 24b IR of address to copy to, 24b IR of address to copy from
    //   awaits as additional in-param : 24b IR of byte count (as unsigned scalar integral), 1b if byte count is ensured non-zero,
    //                                   1b if ensured multiple of align dest, 1b if ensured multiple of align src,
    //                                   1b if known copyable increasing, 1b if known copyable decreasing. Same address is error.
    IRIT_MEMCPY = 11,

    // repeat of a given value for the purpose of allowing position-dependent code to work properly,
    // u8: format, 24b: IR of value, 24b: slot count
    IRIT_RECALL = 12,

    // represents a temporary copy of a runtime value, frozen at some point in time
    // u8: format, 24b: IR of value to snapshot, 24b: slot-count
    IRIT_SNAPSHOT = 13,

    // represents a similar value in a larger integral format, or towards a floating point format (float to float or int to float).
    // Applies to scalar or vecs as long as vec count is identical between source and dest.
    // can be used as value only
    // u8: format of result, 24b: IR of value to cast, 8b: format of base, 1b: (source integrals only): issigned
    IRIT_CAST_SCALAR_ELEMS = 14,

    // cast, when fp-to-int, with additional param for cast-failure
    // can be used as value only
    // u8: format of result, 24b: IR of value to cast, 8b: format of base,
    //                       1b: issigned dest, 1b: fail-silent-out-of-range (as-minint), 1b: fail-silent-non-integral (as-ttz)
    // if NOT fail-silent-out-of-range, then awaits additional out-param (bool*) for failure (nan, infinities, too large...)
    // if NOT fail-silent-non-integral, then moreover awaits additional out-param (bool*) for failure
    IRIT_CAST_FP_TO_INT_SCALAR_ELEMS,

    // no remaining slot here
        COUNT_NONMULTI_IRIT_0,

    IRIT_BINDING_1 = 16,        // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_1 = 17,       // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // proc-call related
    //

    // the call itself, preceeding a specified, fixed and required amount of CALLER_PROC_PARAM and CALLER_PROC_RESULT
    // 8b calling convention, 24b IR of address of proc to call, 5b count of in-params, 5b count of out-params, 5b count of default-in,
    // 1b whether IR is (or holds address to) a comptime binding to a proc, 1b whether proc is constexpr. 1b whether the call is tail.
    // reserved 6b for further flags.
    IRIT_PROC_CALL = 18,

    // in-param on caller side, following proc call or asm instruction or builtin
    // if standard proc-call: 8b format of param, 24b IR of param value, 24b slot count (max 4M)
    IRIT_CALLER_PROC_PARAM = 19,

    // represents a ret-param on caller side, after any 'CALLER_PROC_PARAM', following proc call or asm instruction or builtin
    // if standard proc-call: 8b format of param, 24b 'IR'(position) of associated call, 24b slot count (max 4M)
    IRIT_CALLER_PROC_RESULT = 20,

    // a 'call' to a target-arch dependent instruction, requires a mapping between 32b codes and the instruction encodings.
    // 32b in-code for the mapping to target-arch-encoding ; 24b IRfirstParam(if any)
    IRIT_ASM_INSTRUCTION = 21,

    // target-arch dependent register
    // 56b code... ? todo, flags ?
    IRIT_EXPLICIT_ASM_REG = 22,

    //
    // Branching
    //

    // an instruction to unconditionnally jump to an IR position
    // 4b unused, 4b: branch kind, 24b : 'IR' (position) to jump to, 24b unused
    IRIT_GOTO = 23,

    // an instruction to jump to an IR position depending on comparison of param with 0
    // 3b: log2 of integral format, 1b:jump when zero (0) vs jump when non-zero (1), 4b: branch kind,
    //     24b: 'IR' (position) to jump to, 24b: IR of integral to check
    IRIT_BRANCH = 24,

    // an instruction to trigger an halt with auto-debug and report (hopefully always on a cold branch)
    // depending on comparison of param with 0
    // 8b: format of var to check, 24b: IR of integral to check,
    //     1b:err when zero (0) vs err when non-zero (1), 1b is-active, 22b err-check-index, 
    IRIT_ERR_BRANCH = 25,

    // an instruction to automatically execute proc outtro and return from current proc, to caller
    IRIT_RET = 26,

    // remaining 5 slots here
        COUNT_NONMULTI_IRIT_1,

    IRIT_BINDING_2 = 32,        // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_2 = 33,      // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // Markers
    //

    // an IR position which does nothing
    IRIT_NOOP = 34,

    // an IR position which indicates a jump target. C-backend can use this for label definitions.
    // 2b: user-specified path coldness (00:nominal (hot), 01:unlikely, 10:edgecase or errhandling code, 11:unreachable)
    IRIT_MARKER_JUMP_TARGET = 35,

    // an IR position which indicates opening of a scoping block at source level.
    // VM or optimizer can use this as a hint to mark current size of stack.
    IRIT_MARKER_START_SOURCE_SCOPE = 36,

    // an IR position which indicates returning to just-before-of-entering a scoping block at source level.
    // VM or optimizer can use this as a hint to pop the stack to a previously recorded size.
    // 8b unused. 24b : 'IR' (position) of an IRIT_MARKER_START_SOURCE_SCOPE to return to, 24b unused
    IRIT_MARKER_POP_TO_SCOPE = 37,

    // (optim phase) an IR position representing a join point for multiple paths
    // 8b count of phi nodes following, 24b 'IR' position of first jump to there, 24b 'IR' position of second jump to there
    IRIT_MARKER_JOIN_BEFORE_PHI = 38,

    // (optim phase) an IR position representing an SSA value, dependent on 'which' of multiple paths was taken previously.
    // 8b format, 24b IR if coming from 'first' path as defined in the preceding 'join before phi', 24b IR if coming from 'second' path.
    IRIT_MARKER_PHI = 39,

    // (optim phase) explicit reloading of a slot due to possible store to aliased addresses
    IRIT_MARKER_RELOAD_SLOT = 40,

    // remaining 7 slots here
        COUNT_NONMULTI_IRIT_2,

    IRIT_BINDING_3 = 48,        // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_3 = 49,       // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // basic arithmetic #1
    //

    // addition of two values : A+B ; modulo when integral, standard for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_ADD,

    // subtraction of two values : A-B ; modulo when integral, standard for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SUB,

    // multiplication of two unsigned integers : A*B ; modulo
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_MUL_UINT,

    // multiplication of two values : A*B ; modulo (signed) when integral, standard for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_MUL,

    // int quotient of two unsigned integers : A/B
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_DIV_UINT,

    // quotient of two values : A/B ; int (signed) and truncated towards zero when integral, standard for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_DIV,

    // int remainder of two unsigned integers : A%B
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_REM_UINT,

    // int remainder of two signed integers : A%B. Negative if non zero for a negative value of A/B in the Reals.
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_REM_SINT,

    // modulus of two values A and B ; results in [0..B[ if B positive, ]B..0] otherwise.
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_MOD,
    
    // A to the Bth power, when B is an integer ; format (of A) can also be integer, in which case B shall be positive
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    **if integral format** awaits for additional out-param (8b bool), representing overflow
    IRIT_POW_N,

    // A to the Bth power, when B is a non-negative integer and A is an unsigned integral
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param (8b bool), representing overflow
    IRIT_POW_N_UINT,

    // A to the Bth power, when both are floating point
    IRIT_POW_FP,

    // remaining 2 slots here
        COUNT_NONMULTI_IRIT_3,

    IRIT_BINDING_4 = 64,        // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_4 = 65,       // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // Bitwise arithmetic
    //

    // bitwise and integral values A and B
    // 8b format ; 24b: operand A ; 24b: operandB
    IRIT_BIT_AND,

    // bitwise or integral values A and B
    // 8b format ; 24b: operand A ; 24b: operandB
    IRIT_BIT_OR,

    // bitwise xor integral values A and B
    // 8b format ; 24b: operand A ; 24b: operandB
    IRIT_BIT_XOR,
    
    // bitwise left shift integral value A by a bit amount equal to a 8-bit integral value B, injecting zero from the right
    // 8b: format with special flag at 'fp' pos : indicates whether to use same format than A for B instead of 8b;
    //   24b: operand A ; 24b: operandB
    IRIT_LEFT_SHIFT,

    // bitwise right shift integral value A by a bit amount equal to a 8-bit integral value B, injecting zero from the left
    // 8b: format with special flag at 'fp' pos : indicates whether to use same format than A for B instead of 8b;
    //   24b: operand A ; 24b: operandB
    IRIT_RIGHT_SHIFT,

    // bitwise right shift integral value A by a bit amount equal to a 8-bit integral value B, injecting a repeat of sign-bit from the left
    // 8b: format with special flag at 'fp' pos : indicates whether to use same format than A for B instead of 8b;
    //   24b: operand A ; 24b: operandB
    IRIT_RIGHT_SHIFT_SIGNED,

    // bitwise left rotate integral value A by a bit amount equal to a 8-bit integral value B
    // 8b: format with special flag at 'fp' pos : indicates whether to use same format than A for B instead of 8b;
    //   24b: operand A ; 24b: operandB
    IRIT_LEFT_ROTATE,

    // bitwise right rotate integral value A by a bit amount equal to a 8-bit integral value B
    // 8b: format with special flag at 'fp' pos : indicates whether to use same format than A for B instead of 8b;
    //   24b: operand A ; 24b: operandB
    IRIT_RIGHT_ROTATE,

    // checks for equality between values A and B. scalars returns 8b true=01, false=00 ; vectors check elementwise equality
    // and return same-vec-length integral with full 1s elements where eq was true, full 0 otherwise. IEEE754 spec for FP equals
    // 8b: format, 24b: IR of operandA, 24: IR of operandB
    IRIT_ARE_EQUAL,

    // checks whether unsigned integral value A is strictly less than value B. scalars returns 8b true=01, false=00 ;
    // vectors check elementwise equality and return same-vec-length integral with full 1s elements where eq was true, full 0 otherwise.
    // 8b: format, 24b: IR of operandA, 24: IR of operandB
    IRIT_IS_LESS_UINT,
             
    // checks whether signed integral value A is strictly less than value B. scalars returns 8b true=01, false=00 ; vectors check elementwise
    // equality and return same-vec-length integral with full 1s elements where eq was true, full 0 otherwise. int only.
    // 8b: format, 24b: IR of operandA, 24: IR of operandB
    IRIT_IS_LESS_SINT,

    // checks whether fp value A is strictly less than value B. scalars returns 8b true=01, false=00 ; vectors check elementwise
    // equality and return same-vec-length integral with full 1s elements where eq was true, full 0 otherwise. IEEE754 spec for FP lessthan
    // 8b: format, 24b: IR of operandA, 24b: IR of operandB
    //    awaits for additional out-param (8b bool) to 1 if unord (comparisons involving nan) - when scalar. When vector, req. vector of same slots.
    IRIT_IS_LESS_OR_UNORD_FP,

        COUNT_NONMULTI_IRIT_4,

    IRIT_BINDING_5 = 80,        // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_5 = 81,       // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // Unary ops arithmetic
    //

    // strips the sign from its operand to result in an absolute value ; whether integral or fp. note that min int value results in itself.
    // 8b format, 24b: IR of operand
    IRIT_ABS,

    // negates its operand to a result of same magnitude and opposite sign ; whether integral or fp. note that min int value results in itself.
    // 8b format, 24b: IR of operand
    IRIT_NEG,

    // bitwise inverts all bits from its operand
    // 8b format, 24b: IR of operand
    IRIT_BIT_NOT,

    // swaps each half-word with its coupled neighbour in each word element of its operand
    // 8b format, 24b: IR of operand
    IRIT_SWAP_HILO,

    // repeatedly swaps half-words of its elements, starting from size of elements down to when half-word is at byte level
    // 8b format, 24b: IR of operand
    IRIT_SWAP_ENDIAN,

    // inverse the ordering of all elements in a vector
    // 8b format, 24b: IR of operand
    IRIT_INVERSE_VEC,

    // represents a vector value with a distinct slot count (but same scalar format). Additionals are zeroed.
    // can be used as value only
    // u8: format of result, 24b: IR of base, 8b: format of base.
    IRIT_INCREASE_VEC_COUNT,

    // initialises a vector from a scalar of same type as vector elements, resulting in all elements being equal to the scalar
    // 8b format, 24b: IR of scalar to expand to vec
    IRIT_VEC_INIT_FROM_SCALAR,

    // initialises an integral from a vector, each bit being equal to the lowest bit from vector elements. Result has same bitsize as vector count.
    // 8b format, 24b: IR of vector to extract bits from
    IRIT_VEC_LOBIT_TO_SCALAR,

    // initialises an integral from a vector, each bit being equal to the highest bit from vector elements. Result has same bitsize as vector count.
    // 8b format, 24b: IR of vector to extract bits from
    IRIT_VEC_HIBIT_TO_SCALAR,

    // checks whether its FP parameter is a NaN value. scalars returns 8b true=01, false=00 ; // vectors check elementwise and return
    //   same-vec-length integral with full 1s elements where was NaN, full 0 otherwise.
    // 8b format, 24b : IR of operand
    IRIT_IS_NAN,

    // inverts a boolean value to its opposite value...
    // Note: this will in effect be same as xoring with value '..0001', but is kept as a separate instruction to help with optimizations
    // 8b: format, 24b: IR of operand to negate. 24b unused
    IRIT_BOOLEAN_NOT,

        COUNT_NONMULTI_IRIT_5,

    IRIT_BINDING_6 = 96,        // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_6 = 97,       // see IRITMULTI_IS_EXTENDED

    // ******************************************************************************************************
    // basic arithmetic, extended with additional flags
    //

    // addition of two values : A+B ; emits additional flags for subsequent overflow check
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param (8b bool), representing overflow flag (for signed ints or float to infinity from finite).
    //    if integral format, further awaits for additional out-param (8b bool), representing carry flag (for unsigned int overflow).
    IRIT_ADD_EXT,

    // addition of two values : A+B with support for carry;
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional in-param, representing carry-in
    //    awaits for additional out-param, representing carry out
    IRIT_ADD_CARRY,

    // subtraction of two values : A-B ; emits additional flags for subsequent overflow check
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param (8b bool), representing overflow flag (for signed ints or float to infinity from finite).
    //    if integral format, further awaits for additional out-param (8b bool), representing carry flag (for unsigned int overflow).
    IRIT_SUB_EXT,

    // subtraction of two values : A-B with support for borrow;
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional in-param, representing borrow-in
    //    awaits for additional out-param, representing borrow out
    IRIT_SUB_BORROW,

    // multiplication of two unsigned integers : A*B ; emits additional flags for subsequent overflow check
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param (8b bool), representing overflow
    IRIT_MUL_UINT_EXT,

        // multiplication of two signed values : A*B ; emits additional flags for subsequent overflow check
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param (8b bool), representing overflow flag (for signed ints or float to infinity from finite).
    IRIT_MUL_EXT,

    // multiplication of two unsigned integers : A*B ; emits high-bits into additional value
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param representing high bits of the multiplication (unsigned)
    IRIT_MUL_UINT_WITH_HIGH,

    // multiplication of two integer values : A*B ; emits high-bits into additional value
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    //    awaits for additional out-param representing high bits of the multiplication (includes sign)
    IRIT_MUL_SINT_WITH_HIGH,



/*
    // Saturated add of two values : A+B ; unsigned and saturated to full 1s if would wrap when integral, to [0..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_ADD_UNS,

    // Saturated add of two values : A+B ; to min+1 or max int of that size if would wrap when integral, to [-1..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_ADD,

    // Saturated subtract of two values : A-B ; unsigned and saturated to full 0s if would wrap when integral, to [0..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_SUB_UNS,

    // Saturated subtract of two values : A-B ; to min+1 or max int of that size if would wrap when integral, to [-1..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_SUB,

    // Saturated multiplication of two values : A*B ; unsigned and saturated to full 1s if would wrap when integral, to [0..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_MUL_UNS,

    // Saturated multiplication of two values : A*B ; to min+1 or max int of that size if would wrap when integral, to [-1..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_MUL,

    // Saturated division of two FP values : A/B ; to [0..1]
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_DIV_UFP,

    // Saturated division of two FP values : A/B ; to [-1..1]
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_SAT_DIV_FP,

    // unsigned multiplication of integral vector A with scalar B of same type as vector elts ; modulo
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_MUL_UINT,

    // multiplication of vector A with scalar B of same type as vector elts ; modulo (signed) when integral, standard for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_MUL,

    // int quotient of vector A by scalar B of same type as vector elts
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_DIV_UINT,

    // quotient of vector A with scalar B of same type as vector elts ; int (signed) and truncated towards zero when integral, standard for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_DIV,

    // unsigned remainder of integral vector A with scalar B of same type as vector elts
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_REM_UINT,

    // signed remainder of integral vector A with scalar B of same type as vector elts ; Negative if non zero for a negative value of A/B in the Reals.
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_REM_SINT,

    // Saturated multiplication of integral vector A with scalar B of same type as vector elts ; unsigned and saturated to full 1s when integral, to [0..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_SAT_MUL_UNS,

    // Saturated multiplication of vector A with scalar B of same type as vector elts ; saturated to min+1 and max when integral, to [0..1] for FP
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_SAT_MUL,

    // Saturated FP division of vector A by scalar B of same type as vector elts ; to [0..1]
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_SAT_DIV_UFP,

    // Saturated FP division of vector A with scalar B of same type as vector elts ; to [-1..1]
    // 8b: format, 24b: IR of operandA, 24b: IR of operand B
    IRIT_VXS_SAT_DIV_FP,
*/

    // remaining 3 slots here
        COUNT_NONMULTI_IRIT_6,

    IRIT_BINDING_7 = 112,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_7 = 113,      // see IRITMULTI_IS_EXTENDED

        // tmp helper for printing a value fitting in a standard format
        // 8b: format, 24b: IR of value to print, 8b: value or formatting kind (see ETmpPrintKind), 16b unused
        IRIT_TMP_BUILTIN_PRINT_REG,

        // tmp helper for printing a zero-terminated string
        // 8b: unused, 24b : IR to start address of string
        IRIT_TMP_BUILTIN_PRINT_CSTRING,

        // tmp helper for printing from a buffer of bytes
        // 8b: unused, 24b : IR to start address of string, 24b : IR to byte count (u32)
        IRIT_TMP_BUILTIN_PRINT_BYTES,

        COUNT_NONMULTI_IRIT_7,

    IRIT_BINDING_8 = 128,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_8 = 129,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_8,

    IRIT_BINDING_9 = 144,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_9 = 145,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_9,

    IRIT_BINDING_A = 160,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_A = 161,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_A,

    IRIT_BINDING_B = 176,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_B = 177,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_B,

    IRIT_BINDING_C = 192,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_C = 193,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_C,

    IRIT_BINDING_D = 208,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_D = 209,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_D,

    IRIT_BINDING_E = 224,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_E = 225,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_E,

    IRIT_BINDING_F = 240,       // see IRITMULTI_IS_BINDING
    IRIT_EXTENDED_F = 241,      // see IRITMULTI_IS_EXTENDED

    // remaining 14 slots here
        COUNT_NONMULTI_IRIT_F,

/*
    IRIT_FTRANSCEND,            // fp transcendental function (secondary holds kind) with fast, possibly hardware implementation
    IRIT_FATAN2,                // fp special arity-2 transcendental for ATAN2(y, x) with fast, possibly hardware implementation
    IRIT_FPOW,                  // fp special arity-2 transcendental for pow A**B with fast, possibly hardware implementation
    IRIT_FPOW_ONEMINUSY,        // fp special arity-2 transcendental for pow A**(B-1) with fast, possibly hardware implementation
    IRIT_FLOG,                  // fp special arity-2 transcendental for log base B of A with fast, possibly hardware implementation
    IRIT_FLOG_ONEPLUSX,         // fp special arity-2 transcendental for log base B of (A+1) with fast, possibly hardware implementation
    
    IRIT_BRAD_INVTRIG,          // inverse trig function to 'brad' encodings
    IRIT_BRAD_ATAN2_HALF,       // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f16
    IRIT_BRAD_ATAN2_SINGLE,     // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f32
    IRIT_BRAD_ATAN2_DOUBLE,     // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f64
    IRIT_BRAD_ATAN2_QUAD,       // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f128
    IRIT_BRAD_ATAN2_OCTUPLE,    // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f256

    IRIT_FTRANSCEND_EXACT,      // fp transcendental function (secondary holds kind) with exact (and deterministic) IEEE754 precision
    IRIT_FATAN2_EXACT,          // fp special arity-2 transcendental for ATAN2(y, x) with exact (and deterministic) IEEE754 precision
    IRIT_FPOW_EXACT,            // fp special arity-2 transcendental for pow A**B with exact (and deterministic) IEEE754 precision
    IRIT_FPOW_ONEMINUSY_EXACT,  // fp special arity-2 transcendental for pow A**(B-1) with exact (and deterministic) IEEE754 precision
    IRIT_FLOG_EXACT,            // fp special arity-2 transcendental for log base B of A with exact (and deterministic) IEEE754 precision
    IRIT_FLOG_ONEPLUSX_EXACT,   // fp special arity-2 transcendental for log base B of (A+1) with exact (and deterministic) IEEE754 precision

    IRIT_BRAD_INVTRIG_EXACT,    // inverse trig function to 'brad' encodings with exact (and deterministic) precision
    IRIT_BRAD_ATAN2_HALF_EXACT,  // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f16 with exact (and deterministic) precision
    IRIT_BRAD_ATAN2_SINGLE_EXACT,// special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f32 with exact (and deterministic) precision
    IRIT_BRAD_ATAN2_DOUBLE_EXACT,// special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f64 with exact (and deterministic) precision
    IRIT_BRAD_ATAN2_QUAD_EXACT, // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f128 with exact (and deterministic) precision
    //IRIT_BRAD_ATAN2_OCTUPLE_EXACT, // special arity-2 inverse trig function to 'brad' encodings, from (y,x) as f256 with exact (and deterministic) precision
*/

/*
    IRIT_REPLICATE_TO_VECTOR,   // emitted to get a value from an associated scalar format and insert it in all slots of a vector
    IRIT_EXTRACT_VECTOR,        // emitted to extract part of a vector value and represent that in a vector with less slots (or a scalar)
    IRIT_STORE_VECTOR,          // emitted store a vector with less slots (or a scalar) as part of a vector with more slots
    IRIT_PACK_FROM_VECTOR,      // emitted to take bits from each slot in a vector and repack that as a representation with less bits.
    IRIT_MASK_NEXT_VECTOR,      // emitted to setup a mask for the next vector operation. MUST be followed by an operation on vector.

    IRIT_BIT_AND,
    IRIT_BIT_OR,
    IRIT_BIT_XOR,

    IRIT_ARE_EQUAL,             // used for both == and != comparisons
    IRIT_LESSERTHAN,            // used for all ordering comparisons with signed integral or fp values
    IRIT_LESSERTHAN_UNSIGNED,   // used for ordering comparisons with unsigned integral values
    IRIT_IS_FP_NAN,

    IRIT_RET,                   // emitted to return from a procedure (implicitely cleaning stack)
    IRIT_MARKER_END_OF_PROC,    // emitted at the end of a procedure

    IRIT_VALUE_OF,          // emitted to force the representation of a value at that particular IR position, either as a mean of:
                            // - dereferencing a value-by-pointer
                            // - ensuring that a particular value is last referenced, before an 'IRIT_BRANCH_ON_PREV'
                            //   or 'IRIT_ERR_CHECK_ON_PREV' instruction which need a particular sequence in the 'prev+themselves' window.

    IRIT_OFFSET_VALUE_FROM,      // emitted to get a value from the base of a local or global variable or constant,
                                 // given an offset into it

    IRIT_OFFSET_ASSIGNABLE_FROM, // emitted to represent an assignable from the base of a local or global variable,
                                 // given an offset into it

    IRIT_LONG_ADDRESS_OF,   // emitted to get the full, absolute address (in all accessible virtual memory) of a local or
                            //   global variable or constant, possibly also specifying an offset from that base address.
                            // The instruction shall have the format of native pointers for the platform. For access to global
                            //   variables or constants (including function pointers) or local constants, it typically,
                            //   at compile-time, produce some relocation mechanism which will need to be solved at link-stage.
                            //   For access to local variables, it will typically issue an instruction which reads from the IP,
                            //   and apply the offset to it.
                            // This is also the only instruction where an 'IRIT_ADDRESS_OF' value can be used in a
                            //   non-dereferenced manner : this will convert the local address to a global address: this last use
                            //   case is typically emitted by typechecker when stumbling upon a final step in expression
                            //   resolution which up to this point worked well with 'IRIT_ADDRESS_OF', and now needs to switch
                            //   to 'far' addressing mode: before calling a function taking a pointer as a parameter, or more
                            //   generally when source-code explicitely ask for the address of some entity (at an offset within
                            //   an array, or being a specific member of a struct, or plainly the base address of some variable
                            //   or constant).

    IRIT_RESET,             // emitted to reset some range of values to all-0.
                            // First param typically an address. Second param the count,
                            //   assumed of regular reg size (64b on 64b platforms).

    IRIT_CAST,              // emitted to convert an IR format to another

    IRIT_TMP_PAGE_ALLOC,    // (temporary as IR until we have support for foreign functions)
    IRIT_TMP_PAGE_FREE,     // (temporary as IR until we have support for foreign functions)
    IRIT_TMP_PRINT_STR,     // (temporary as IR until we have support for foreign functions)

    IR_INSTRUCTIONS_COUNT
*/
};
static_assert(COUNT_NONMULTI_IRIT_0 <= IRIT_BINDING_1, "too many non-multi instructions at range 0");
static_assert(COUNT_NONMULTI_IRIT_1 <= IRIT_BINDING_2, "too many non-multi instructions at range 1");
static_assert(COUNT_NONMULTI_IRIT_2 <= IRIT_BINDING_3, "too many non-multi instructions at range 2");
static_assert(COUNT_NONMULTI_IRIT_3 <= IRIT_BINDING_4, "too many non-multi instructions at range 3");
static_assert(COUNT_NONMULTI_IRIT_4 <= IRIT_BINDING_5, "too many non-multi instructions at range 4");
static_assert(COUNT_NONMULTI_IRIT_5 <= IRIT_BINDING_6, "too many non-multi instructions at range 5");
static_assert(COUNT_NONMULTI_IRIT_6 <= IRIT_BINDING_7, "too many non-multi instructions at range 6");
static_assert(COUNT_NONMULTI_IRIT_7 <= IRIT_BINDING_8, "too many non-multi instructions at range 7");
static_assert(COUNT_NONMULTI_IRIT_8 <= IRIT_BINDING_9, "too many non-multi instructions at range 8");
static_assert(COUNT_NONMULTI_IRIT_9 <= IRIT_BINDING_A, "too many non-multi instructions at range 9");
static_assert(COUNT_NONMULTI_IRIT_A <= IRIT_BINDING_B, "too many non-multi instructions at range A");
static_assert(COUNT_NONMULTI_IRIT_B <= IRIT_BINDING_C, "too many non-multi instructions at range B");
static_assert(COUNT_NONMULTI_IRIT_C <= IRIT_BINDING_D, "too many non-multi instructions at range C");
static_assert(COUNT_NONMULTI_IRIT_D <= IRIT_BINDING_E, "too many non-multi instructions at range D");
static_assert(COUNT_NONMULTI_IRIT_E <= IRIT_BINDING_F, "too many non-multi instructions at range E");
static_assert(COUNT_NONMULTI_IRIT_F <= 256,            "too many non-multi instructions at range F");

// alternative for immediates:
    // 1000'0000'0000'0000'0000'0000 to 1100'0000'0000'0000'0000'0000 (included) : 4M+1 positive-or-zero ints
    // 1111'1111'1111'1111'1111'1111 to 1100'0000'0000'0000'0000'0001 : 4M-1 negative ints, except:
    // 1100'0000'0000'0000'0000'0001 to 1100'0000'0000'0000'1111'1111 : 64K-1 FP 16 immediates (excluding positive zero, which is as int).
    // => 4M-64K-2 negative ints total... no additional complexity for flags and whatnot...
    //
    // or could be like 6.5M positives, 64K FP, 1.5M-64K negatives...
    //
    // anyway, with the value for invalid-ir flag now in non-immediate-range:
    // 0111'1111'1111'1111'1111'1111
    // => leaving 4M-1 slots for a standard instruction index...
    //
    // OR: an 'immediate' IR instruction with 56b payload instead...

#endif // TMP TMP

#endif // LOCLIB_IR_TYPES_H_
