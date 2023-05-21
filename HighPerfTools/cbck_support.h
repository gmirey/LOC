#pragma once

#include "BaseDecls.h"
#include "arithmetic_operations.h"

#define r8                  u8
#define r16                 u16
#define r32                 u32
#define r64                 u64
#define r128                u128
#define r256                u256

#define FOREIGN_PROC    extern "C"

struct _DummyEmptyStruct {};

// TODO:
// cast_all_scalars_
// cast_all_scalars_ftoi_
// lsh_
// rshu_
// rshs_
// rol_
// ror_
// ltf_

#define add_r8               add8
#define add_r16              add16
#define add_r32              add32
#define add_r64              add64
#define add_r128             add128
#define add_r256             add256
//TODO: FP, vec integral, vec FP

#define sub_r8               sub8
#define sub_r16              sub16
#define sub_r32              sub32
#define sub_r64              sub64
#define sub_r128             sub128
#define sub_r256             sub256
//TODO: FP, vec integral, vec FP

#define addx_r8              add8_ext
#define addx_r16             add16_ext
#define addx_r32             add32_ext
#define addx_r64             add64_ext
#define addx_r128            add128_ext
#define addx_r256            add256_ext

#define subx_r8              sub8_ext
#define subx_r16             sub16_ext
#define subx_r32             sub32_ext
#define subx_r64             sub64_ext
#define subx_r128            sub128_ext
#define subx_r256            sub256_ext

#define mulu_r8              mulu8
#define mulu_r16             mulu16
#define mulu_r32             mulu32
#define mulu_r64             mulu64
#define mulu_r128            mulu128
#define mulu_r256            mulu256
//TODO: vec integral

#define muls_r8              muli8
#define muls_r16             muli16
#define muls_r32             muli32
#define muls_r64             muli64
#define muls_r128            muli128
#define muls_r256            muli256
//TODO: FP, vec integral, vec FP

#define mulux_r8             mulu8_ovf
#define mulux_r16            mulu16_ovf
#define mulux_r32            mulu32_ovf
#define mulux_r64            mulu64_ovf
#define mulux_r128           mulu128_ovf
#define mulux_r256           mulu256_ovf

#define mulsx_r8             muli8_ovf
#define mulsx_r16            muli16_ovf
#define mulsx_r32            muli32_ovf
#define mulsx_r64            muli64_ovf
#define mulsx_r128           muli128_ovf
#define mulsx_r256           muli256_ovf

#define mulu_high_r8         mulu8_with_high
#define mulu_high_r16        mulu16_with_high
#define mulu_high_r32        mulu32_with_high
#define mulu_high_r64        mulu64_with_high
#define mulu_high_r128       mulu128_with_high
#define mulu_high_r256       mulu256_with_high
//TODO: vec integral

#define muls_high_r8         muli8_with_high
#define muls_high_r16        muli16_with_high
#define muls_high_r32        muli32_with_high
#define muls_high_r64        muli64_with_high
#define muls_high_r128       muli128_with_high
#define muls_high_r256       muli256_with_high
//TODO: FP, vec integral, vec FP

#define divux_r8		     divu8_large
#define divux_r16		     divu16_large
#define divux_r32		     divu32_large
#define divux_r64		     divu64_large
#define divux_r128		     divu128_large
#define divux_r256		     divu256_large

#define divsx_r8		     divi8_large
#define divsx_r16		     divi16_large
#define divsx_r32		     divi32_large
#define divsx_r64		     divi64_large
#define divsx_r128		     divi128_large
#define divsx_r256		     divi256_large

#define divu_r8				 divu8
#define divu_r16			 divu16
#define divu_r32			 divu32
#define divu_r64			 divu64
#define divu_r128			 divu128
#define divu_r256			 divu256
//TODO: vec integral

#define divs_r8              divi8
#define divs_r16             divi16
#define divs_r32             divi32
#define divs_r64             divi64
#define divs_r128            divi128
#define divs_r256            divi256
//TODO: FP, vec integral, vec FP

#define remu_r8				 remu8
#define remu_r16			 remu16
#define remu_r32			 remu32
#define remu_r64			 remu64
#define remu_r128			 remu128
#define remu_r256			 remu256
//TODO: vec integral

#define rems_r8              remi8
#define rems_r16             remi16
#define rems_r32             remi32
#define rems_r64             remi64
#define rems_r128            remi128
#define rems_r256            remi256
//TODO: FP, vec integral, vec FP

#define mod_r8               mod8
#define mod_r16              mod16
#define mod_r32              mod32
#define mod_r64              mod64
#define mod_r128             mod128
#define mod_r256             mod256
//TODO: FP, vec integral, vec FP

#define bitnot_r8            bitNot8
#define bitnot_r16           bitNot16
#define bitnot_r32           bitNot32
#define bitnot_r64           bitNot64
#define bitnot_r128          bitNot128
#define bitnot_r256          bitNot256
#define bitnot_r512          bitNot512
#define bitnot_r1024         bitNot1024

#define bitand_r8            bitand8
#define bitand_r16           bitand16
#define bitand_r32           bitand32
#define bitand_r64           bitand64
#define bitand_r128          bitand128
#define bitand_r256          bitand256
#define bitand_r512          bitand512
#define bitand_r1024         bitand1024

#define bitor_r8             bitor8
#define bitor_r16            bitor16
#define bitor_r32            bitor32
#define bitor_r64            bitor64
#define bitor_r128           bitor128
#define bitor_r256           bitor256
#define bitor_r512           bitor512
#define bitor_r1024          bitor1024

#define bitxor_r8            bitXor8
#define bitxor_r16           bitXor16
#define bitxor_r32           bitXor32
#define bitxor_r64           bitXor64
#define bitxor_r128          bitXor128
#define bitxor_r256          bitXor256
#define bitxor_r512          bitXor512
#define bitxor_r1024         bitXor1024

#define eq_r8                are_eq8
#define eq_r16               are_eq16
#define eq_r32               are_eq32
#define eq_r64               are_eq64
#define eq_r128              are_eq128
#define eq_r256              are_eq256
#define eq_r512              are_eq512
#define eq_r1024             are_eq1024
//TODO: FP?, vec integral, vec FP?

#define ltu_r8               is_strict_less_u8
#define ltu_r16              is_strict_less_u16
#define ltu_r32              is_strict_less_u32
#define ltu_r64              is_strict_less_u64
#define ltu_r128             is_strict_less_u128
#define ltu_r256             is_strict_less_u256
//TODO: vec integral

#define lts_r8               is_strict_less_i8
#define lts_r16              is_strict_less_i16
#define lts_r32              is_strict_less_i32
#define lts_r64              is_strict_less_i64
#define lts_r128             is_strict_less_i128
#define lts_r256             is_strict_less_i256
//TODO: vec integral

#define bool_not_r8          boolNot8
#define bool_not_r16         boolNot16
#define bool_not_r32         boolNot32
#define bool_not_r64         boolNot64
#define bool_not_r128		 boolNot128
#define bool_not_r256		 boolNot256

#define pow_n_r8             powi8
#define pow_n_r16            powi16
#define pow_n_r32            powi32
#define pow_n_r64            powi64
#define pow_n_r128           powi128
#define pow_n_r256           powi256
//TODO: FP (vec integral, vec FP ?)

#define pow_un_r8            powu8
#define pow_un_r16           powu16
#define pow_un_r32           powu32
#define pow_un_r64           powu64
#define pow_un_r128          powu128
#define pow_un_r256          powu256
//TODO: vec integral ?

#include <stdio.h>
#include <memory.h>

local_func_inl void print_i32(r32 rVal) {
    printf("%d", i32(rVal));
}
local_func_inl void print_i64(r64 rVal) {
    printf("%lld", i64(rVal));
}
local_func_inl void print_u32(r32 rVal) {
    printf("%u", u32(rVal));
}
local_func_inl void print_u64(r64 rVal) {
    printf("%llu", u64(rVal));
}
local_func_inl void print_raw32(r32 rVal) {
    printf("0x%x", u32(rVal));
}
local_func_inl void print_raw64(r64 rVal) {
    printf("0x%llx", u64(rVal));
}
local_func_inl void print_raw128(r128 rVal) {
    printf("0x%llx%llx", rVal.tLegs[1], rVal.tLegs[0]);
}
local_func_inl void print_raw256(r256 rVal) {
    printf("0x%llx%llx%llx%llx", rVal.tLegs[3], rVal.tLegs[2], rVal.tLegs[1], rVal.tLegs[0]);
}
local_func_inl void print_rawptr(r64 rVal) {
    printf(">%016llx", u64(rVal));
}
local_func_inl void print_codepoint(r32 rVal) {
    if (rVal < 128u)
        printf("%c", char(rVal));
    else if (rVal < 0x10000u)
        printf("u%04x", u32(rVal));
    else
        printf("U%06x", u32(rVal));
}
local_func_inl void print_bool(r8 rVal) {
    if (rVal == 0)
        printf("false");
    else if (rVal == 1)
        printf("true");
    else
        printf("<non-1b-truth?>");
}
local_func_inl void print_eol(r32 rVal) {
    printf("\n");
}
local_func_inl void print_cstring(r64 rAdr) {
    printf(reinterpret_cast<char*>(rAdr));
}
local_func void print_bytes(r64 rAdr, r32 rCnt) {
    char szBuffer[8200];
    char* pAdr = reinterpret_cast<char*>(rAdr);
    while (rCnt > 8192u) {
        memcpy(szBuffer, pAdr, 8192u);
        szBuffer[8192u] = '\0';
        printf(szBuffer);
        rCnt -= 8192u;
        pAdr += 8192u;
    }
    if (rCnt) {
        memcpy(szBuffer, pAdr, rCnt);
        szBuffer[rCnt] = '\0';
        printf(szBuffer);
    }
}

#define print_sint_r8       print_i32
#define print_sint_r16      print_i32
#define print_sint_r32      print_i32
#define print_sint_r64      print_i64
#define print_sint_r128     print_i128
#define print_sint_r256     print_i256

#define print_uint_r8       print_i32
#define print_uint_r16      print_i32
#define print_uint_r32      print_i32
#define print_uint_r64      print_i64
#define print_uint_r128     print_i128
#define print_uint_r256     print_i256

#define print_raw_r8        print_raw32
#define print_raw_r16       print_raw32
#define print_raw_r32       print_raw32
#define print_raw_r64       print_raw64
#define print_raw_r128      print_raw128
#define print_raw_r256      print_raw256

#define print_rawptr_r64    print_rawptr
#define print_codepoint_r32 print_codepoint
#define print_bool_r8       print_bool
#define print_eol_r32       print_eol

//
// TODO: maybe put 'on_error__' and 'on_error_report__' here ?
// TODO: allow a version of 'on_error__' for gracious termination when in a 'testing' context
//      -> maybe simply make a thread for each test, and check for abnormal termination function in the main test handler
// TODO: allow 'defers' (and errdefer ?) to be flaged (eg '@alsofatal') to be called on hard-errors, before 'on_error__'
//      -> the current 'goto on_err_check_failed__N' could setup err then goto deferred code, then goto on_err_check_failed__
//      -> fall back to an hard error skipping defers if double-fault during processing of such defers.
//