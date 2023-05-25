// Part of LocLang/HighPerfTools
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

// ************************************************************
// 'UNUSED' macro : explicitely flags some variable (typically a function parameter) as 'unused', and incidentally removes the associated warning.
// ************************************************************

#define UNUSED(var) ((void)(var))

// ************************************************************
// explicit sized typedefs
// ************************************************************
// define short identifiers for numeric types with a definite bitsize, currently working for MSVC
//   (intended to be #define'd as required for same effects under other compilers... TODO).
// ************************************************************

// floating-point: not much of a compiler dependency here

typedef float               f32;
typedef double              f64;

#if defined(_MSC_VER)  // Visual Studio Compiler

    // We have enough info about MSVC compiler to work out the explicit types without stdint.h

    typedef unsigned char        u8;
    typedef unsigned short      u16;
    typedef unsigned int        u32;
    typedef unsigned __int64    u64;

    typedef signed char          i8;
    typedef signed short        i16;
    typedef signed int          i32;
    typedef signed __int64      i64;

#  ifdef _WIN64     // 64b
    static_assert(sizeof(void*) == 8, "We assumed a 64b platform, which does not seem to be the case");
    typedef __int64          IParam; // a signed integer which is of same size as a pointer for the target architecture
    typedef unsigned __int64 UParam; // an unsigned integer which is of same size as a pointer for the target architecture
#  else             // Assumed 32b
    static_assert(sizeof(void*) == 4, "We assumed a 32b platform, which does not seem to be the case");
    typedef int              IParam; // a signed integer which is of same size as a pointer for the target architecture
    typedef unsigned int     UParam; // an unsigned integer which is of same size as a pointer for the target architecture
#  endif

#else               // Other compilers...

#  if defined(__GNUC__) || defined(__clang__)     // GCC or Clang compiler
     // TODO: we have enough info about GCC or Clang implementation choices already to work out the explicit types without stdint.h, as we did for MSVC
#    warning "under GCC or Clang, we could have defined basic types without a dependency to <stdint.h>... It remains as TODO."
#  endif 

#  include <stdint.h>     // resort to stdint.h header to ensure of the actual types.

    typedef uint8_t         u8;
    typedef uint16_t        u16;
    typedef uint32_t        u32;
    typedef uint64_t        u64;

    typedef int8_t          i8;
    typedef int16_t         i16;
    typedef int32_t         i32;
    typedef int64_t         i64;

    typedef intptr_t        IParam; // a signed integer which is of same size as a pointer for the target architecture
    typedef uintptr_t       UParam; // an unsigned integer which is of same size as a pointer for the target architecture

    #include <limits.h>  // for CHAR_BIT

    static_assert(CHAR_BIT == 8, "we require that target platform has 8-bit char, so that that bit count in any 'a' equals 8 * sizeof(a)");

#endif

static_assert(sizeof( u8) == 1, "u8 should be 8b, thus one 8b byte");
static_assert(sizeof(u16) == 2, "u16 should be 16b, thus two 8b bytes");
static_assert(sizeof(u32) == 4, "u32 should be 32b, thus four 8b bytes");
static_assert(sizeof(f32) == 4, "f32 should be 32b, thus four 8b bytes");
static_assert(sizeof(u64) == 8, "u64 should be 64b, thus eight 8b bytes");
static_assert(sizeof(f64) == 8, "f64 should be 64b, thus eight 8b bytes");
static_assert(sizeof( u8) == sizeof( i8), "u8 and i8 shall be of same size");
static_assert(sizeof(u16) == sizeof(i16), "u16 and i16 shall be of same size");
static_assert(sizeof(u32) == sizeof(i32), "u32 and i32 shall be of same size");
static_assert(sizeof(u64) == sizeof(i64), "u64 and i64 shall be of same size");
static_assert(sizeof(UParam) == sizeof(IParam), "UParam and IParam shall be of same size");
static_assert(sizeof(UParam) == sizeof(void*),  "UParam shall be the size of a pointer");
static_assert(sizeof(UParam) == sizeof(size_t), "UParam and size_t shall be of same size");

// ************************************************************
// compiler-specific FORCE_INLINE and NO_INLINE macros
// ************************************************************
// define a FORCE_INLINE macro to add before your functions (as well as a NO_INLINE macro) to give you "some" control over the compiler decision to
//   perform an actual *inlining* of their implementation into caller code.
// (Please note that the standard C++ 'inline' keyword is designed to sort out some function-redefinition concerns, and does *NOT* by itself guarantee
//    such "inlining" in any way, nor are there any way to express the opposite idea and forbid inlining in the C++ standard)
// ************************************************************

#if defined(__GNUC__) || defined(__clang__)     // GCC or Clang compiler
#  define FORCE_INLINE __inline__ __attribute__((always_inline, unused))
#  define NO_INLINE __attribute__((noinline))
#  if !defined(__cplusplus) || (__cplusplus < 201703L)
#    error "your GCC compatible compiler does not seem to be configured for C++17 support. Please use -std:c++17 compiler option (or higher)."
#  endif
    [[noreturn]] static FORCE_INLINE void _unreachable() { __builtin_unreachable(); }
#elif defined(_MSC_VER)                         // Visual Studio Compiler
#  define FORCE_INLINE __forceinline
#  define NO_INLINE __declspec(noinline)
#  if !defined(__cplusplus) || !defined(__has_include)
#    error "your MSVC compiler does not seem to be configured for C++17 support. Please use /std:c++17 compiler option (or higher)."
#  endif
    [[noreturn]] static FORCE_INLINE void _unreachable() { __assume(false); }
#else                                           // unknown other
#  warning "did not define semi-mandatory inlining directives for your current compiler"
#  define FORCE_INLINE
#  define NO_INLINE
#  if !defined(__cplusplus) || !defined(__has_include)
#    error "your compiler does not seem to be configured for C++17 support."
#  endif
    // Support function for 'UNREACHABLE' and 'Assume' in non-check-asserts compilation mode.
    static FORCE_INLINE i32 _IntentionallyTriggerUB(i32 iSetToMaxIntForUB) {
        i32 iOverflow = iSetToMaxIntForUB + 1;      // signed int overflow is in fact UB :x yeah well...
        i32 iDivByZero = 1 / iOverflow;             // in the likely case it otherwise simply wrapped... int div by zero is your best guess
        return *reinterpret_cast<i32*>(IParam(iOverflow)) - iDivByZero;     // Noes? What about dereferencing null ptr, then?
    }
    static FORCE_INLINE void _unreachable() { i32 _ub_ = _IntentionallyTriggerUB(); UNUSED(_ub_); }
#endif

// ************************************************************
// _TO_STRING and _CODE_CONCAT macros
// ************************************************************
// Utility macros.

// _TO_STRING converts some code value to its representation as a string (for example stringifying the __LINE__ macro)
#define   _TO_STRING(code)            #code
// _CODE_CONCAT concatenates two code values as some unified code (for example fusing two tokens into an identifier)
#define   _CODE_CONCAT(code1, code2)  code1 ## code2


// ************************************************************
// by-prefix rewordings of the nature of different entities... wrt the 'static' keyword in particular
// ************************************************************

#define local_func        static

#define local_func_inl    static FORCE_INLINE

#define exported_func_decl(libname)     extern "C" _CODE_CONCAT(DECL_EXPORTED_, libname) 
#define exported_func_impl          

#define public_func_decl
#define public_func_inl_decl    inline FORCE_INLINE

#define global_var

#define global_const      static const


// ************************************************************
// _min() and _max() functions
// ************************************************************
// Defines _min and _max tools as typed functions. Their usage avoids conflict between horrific
//   macro definition in windows.h and/or std::min/max
// ************************************************************

public_func_inl_decl constexpr u8 _min(u8 uA, u8 uB) { return uA > uB ? uB : uA; }
public_func_inl_decl constexpr u8 _max(u8 uA, u8 uB) { return uA > uB ? uA : uB; }

public_func_inl_decl constexpr i8 _min(i8 iA, i8 iB) { return iA > iB ? iB : iA; }
public_func_inl_decl constexpr i8 _max(i8 iA, i8 iB) { return iA > iB ? iA : iB; }

public_func_inl_decl constexpr u16 _min(u16 uA, u16 uB) { return uA > uB ? uB : uA; }
public_func_inl_decl constexpr u16 _max(u16 uA, u16 uB) { return uA > uB ? uA : uB; }

public_func_inl_decl constexpr i16 _min(i16 iA, i16 iB) { return iA > iB ? iB : iA; }
public_func_inl_decl constexpr i16 _max(i16 iA, i16 iB) { return iA > iB ? iA : iB; }

public_func_inl_decl constexpr u32 _min(u32 uA, u32 uB) { return uA > uB ? uB : uA; }
public_func_inl_decl constexpr u32 _max(u32 uA, u32 uB) { return uA > uB ? uA : uB; }

public_func_inl_decl constexpr i32 _min(i32 iA, i32 iB) { return iA > iB ? iB : iA; }
public_func_inl_decl constexpr i32 _max(i32 iA, i32 iB) { return iA > iB ? iA : iB; }

public_func_inl_decl constexpr f32 _min(f32 fA, f32 fB) { return fA > fB ? fB : fA; }
public_func_inl_decl constexpr f32 _max(f32 fA, f32 fB) { return fA > fB ? fA : fB; }

public_func_inl_decl constexpr u64 _min(u64 uA, u64 uB) { return uA > uB ? uB : uA; }
public_func_inl_decl constexpr u64 _max(u64 uA, u64 uB) { return uA > uB ? uA : uB; }

public_func_inl_decl constexpr i64 _min(i64 iA, i64 iB) { return iA > iB ? iB : iA; }
public_func_inl_decl constexpr i64 _max(i64 iA, i64 iB) { return iA > iB ? iA : iB; }

public_func_inl_decl constexpr f64 _min(f64 fA, f64 fB) { return fA > fB ? fB : fA; }
public_func_inl_decl constexpr f64 _max(f64 fA, f64 fB) { return fA > fB ? fA : fB; }

// ************************************************************
// ensuring safe-truncation from 64b to 32b functions
// ************************************************************

public_func_inl_decl constexpr bool does_fit_in_i32(i64 iVal64b) { return iVal64b <= 0x7FFFFFFFLL && iVal64b >= -0x100000000LL; }
public_func_inl_decl constexpr bool does_fit_in_u32(u64 uVal64b) { return uVal64b <= 0xFFFFFFFFuLL; }

// ************************************************************
// type_pun_to[/from]_double[/float]() functions
// ************************************************************
// define an explicit type-punning mechanism between int types and IEEE754 floats (to expose their bits),
//   and use a workaround against C++ aliasing rule problems arising from that, since there are no 'standard'
//   way for doing so (barring memcpy, which in turn is not guaranteed fast...).
// That workaround is currently implemented using the documented GCC convention for doing that (unions).
//
// TODO: ensure also always safe under MSVC at least, since we currently define 'ALLOW_TYPE_PUN_THROUGH_UNIONS_BESIDES_GCC'
//
// ************************************************************

#define ALLOW_TYPE_PUN_THROUGH_UNIONS_BESIDES_GCC

#if defined(__GNUC__) || defined(__clang__) || defined(ALLOW_TYPE_PUN_THROUGH_UNIONS_BESIDES_GCC) // GCC or Clang Compiler, or explicit allowance of pun-through-union technique

union Double_Puns_u64 {
    u64 asU64;
    double asDouble;
};
// converts a non-opaque 64b field to a 'double'
public_func_inl_decl double type_pun_to_double(u64 uBits) {
    Double_Puns_u64 pun;
    pun.asU64 = uBits;
    return pun.asDouble;
}
// converts a 'double' to a non-opaque 64b field
public_func_inl_decl u64 type_pun_from_double(double d) {
    Double_Puns_u64 pun;
    pun.asDouble = d;
    return pun.asU64;
}

union Float_Puns_u32 {
    u32 asU32;
    float asFloat;
};
// converts a non-opaque 32b field to a 'float'
public_func_inl_decl float type_pun_to_float(u32 uBits) {
    Float_Puns_u32 pun;
    pun.asU32 = uBits;
    return pun.asFloat;
}
// converts a 'float' to a non-opaque 32b field
public_func_inl_decl u32 type_pun_from_float(float f) {
    Float_Puns_u32 pun;
    pun.asFloat = f;
    return pun.asU32;
}

#else // fallback to the safe and standard way otherwise, using memcpy
      //   (any semi-recent MSVC should be clever enough to optimize out the actual call to memcpy)

#include <string.h>  // for memcpy

// converts a non-opaque 64b field to a 'double'
public_func_inl_decl double type_pun_to_double(u64 uBits) {
    double dResult;
    memcpy(&dResult, &uBits, 8u);
    return dResult;
}
// converts a 'double' to a non-opaque 64b field
public_func_inl_decl u64 type_pun_from_double(double d) {
    u64 uResult;
    memcpy(&uResult, &d, 8u);
    return uResult;
}
// converts a non-opaque 32b field to a 'float'
public_func_inl_decl float type_pun_to_float(u32 uBits) {
    float fResult;
    memcpy(&fResult, &uBits, 4u);
    return fResult;
}
// converts a 'float' to a non-opaque 32b field
public_func_inl_decl u32 type_pun_from_float(float f) {
    u32 uResult;
    memcpy(&uResult, &f, 4u);
    return uResult;
}
#endif

// ************************************************************
// defer macro
// ************************************************************
// Note: This version is probably derived from a clever implementation by gingerBill (c) 2015-2018... (gb.h library)
//                                             also Marek Rusinowski (c) 2015 for the operator avoiding parens and leaving block-braces syntax

// Note: we could include <utility> here just for 'std::forward'... but we'd rather not do that
// => define those my_* template black-magic here, paraphrasing them ourselves.

// STRUCT TEMPLATE my_is_lvalue_reference (excerpt of std::is_lvalue_reference, not requiring adding that dependency)
template<class _Ty> struct my_is_lvalue_reference { static const bool value = false; };
template<class _Ty> struct my_is_lvalue_reference<_Ty&> { static const bool value = true; };

// STRUCT TEMPLATE my_remove_reference and type alias my_remove_reference_t (excerpt of std::remove_reference and std::remove_reference_t, not requiring adding that dependency)
template<class _Ty>	struct my_remove_reference	{ using type = _Ty; };
template<class _Ty>	struct my_remove_reference<_Ty&> { using type = _Ty; };
template<class _Ty>	struct my_remove_reference<_Ty&&> { using type = _Ty; };
template<class _Ty>	using my_remove_reference_t = typename my_remove_reference<_Ty>::type;

// FUNCTION TEMPLATE my_forward (excerpt of std::forward, not requiring adding that dependency)
template<class _Ty>	[[nodiscard]] constexpr _Ty&& my_forward(my_remove_reference_t<_Ty>& _Arg) noexcept {
    return (static_cast<_Ty&&>(_Arg));
};
template<class _Ty> [[nodiscard]] constexpr _Ty&& my_forward(my_remove_reference_t<_Ty>&& _Arg) noexcept {
    static_assert(!my_is_lvalue_reference_v<_Ty>, "bad my_forward call");
    return (static_cast<_Ty&&>(_Arg));
};

// 'Defer' struct initialises some func-like property on construction, and 'invokes' it on destruction.
// Yes, this means that we *do* use RAII and destructors here, in the hope that we'll never have to use them anywhere else.
// That func-like property is intended to be in fact a lambda, whose body will come after our final 'defer' macro.
template <typename F>
struct Defer {
    // The lambda which is kept for invocation upon destruction
    F _f;
    FORCE_INLINE Defer(F f) : _f(f) {}   // Keeps track of that lambda on construction
    FORCE_INLINE ~Defer() { _f(); }      // Invokes that lambda on destruction
};

// returns an instance of a 'Defer' struct, given some func-like parameter f of type F (designed to be typically a lambda).
template <typename F>
public_func_inl_decl Defer<F> makeDefer(F f) {
    return Defer<F>(f);                   // Is this really required as a separate function from the constructor of Defer<F> itself ?
};

// defer_dummy is an empty type, used to syntacticlly allow the definition of some lambda to, directly, without requiring additional parens,
//   trigger a semantic effect (such as declaring an instance of something 'storing' it), by overloading an 'infix' 2ops function, such as operator+.
struct defer_dummy { };
template<typename F>
public_func_inl_decl Defer<F> operator+(defer_dummy, F&& f) { // operator+ 'constructs' a new instance of the 'Defer<F>' struct, with our lambda as the second operand
    // F&& 'move semantics' and std::forward are C++11 or C++17 good ideas, required here to simply manage the sheer complexity of all those C++11 'good ideas'
    return makeDefer<F>(my_forward<F>(f));
};

// _defer_id(__LINE__) will "make" an identifier which should avoid any 'redefinition' issue, using line number macro to differentiate between
//    several distinct uses of macro 'defer'.
#define _defer_id(line) _CODE_CONCAT(defer_, line)

// finally, we can now define 'defer' as a macro declaring some distinct RAII 'Defer<T>' instance within local scope, tooled to accept as its
//   parameter some lambda (defined with '[&]' to capture all locals, to be able to reference them in the lambda's body), which then only awaits
//   for a user-provided, curly-braced block right after the 'defer' macro, as its body definition.

// the 'defer' macro expects some code-block in the form '{' ... '};' afterwards the macro, which will be executed at current scope exit.
#define defer auto _defer_id(__LINE__) = defer_dummy() + [&]()


// ************************************************************
// LIKELY/UNLIKELY, NOMINAL/EDGECASE
// ************************************************************
// These macros are hints to the compiler which (when supported), but even when unsupported, they also have the benefit of decorating your code with clear *intent*

// LIKELY(cond) vs UNLIKELY(cond) : will tell the compiler that some condition 'cond' is *very* likely (resp. *very* unlikely) to be true.
// NOMINAL(cond) : will tell the compiler that some condition 'cond' is to be expected true on every nominal path, and that only error-handling path will see it false.
// vs EDGECASE(cond) : will tell the compiler that some condition 'cond' is to be expected nominally false, since it will be true only when handling some error,
//     (error handling may be assumed less performance sensitive => better to 'predict' nominal for a smooth regular experience, regardless of actual branching history)
//
// Based on these hints, compiler may chose to:
//   - issue arch-specific CPU hints for branch prediction, when available, or
//   - reorder the code blocks for helping defaults of branch-predictor or for setting actual machine code addresses more "remote" for cold paths.
//   - make different optimization decisions (priviledging size over speed for colder branches maybe).

#if defined(__GNUC__) || defined(__clang__) // GCC or Clang Compiler.
// Atm LIKELY and NOMINAL are declared the same, as are UNLIKELY and EDGE.

#define LIKELY(cond)    (__builtin_expect(!!(cond), 1))     // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that it is very likely
#define UNLIKELY(cond)  (__builtin_expect(!!(cond), 0))     // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that it is very unlikely
#define NOMINAL(cond)   (__builtin_expect(!!(cond), 1))     // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that cond being true is the nominal path
#define EDGECASE(cond)  (__builtin_expect(!!(cond), 0))     // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that cond being true is only in case of some error handling path
#else // Other compilers
#  ifdef _MSC_VER
#    pragma message("Cannot define meaningful hints for LIKELY/UNLIKELY and NOMINAL/EDGECASE macros for MSVC compiler")
#  else
#    warning "Cannot define meaningful hints for LIKELY/UNLIKELY and NOMINAL/EDGECASE macros for your compiler"
#  endif
#define LIKELY(cond)    (cond)                              // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that it is very likely
#define UNLIKELY(cond)  (cond)                              // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that it has very low probability
#define NOMINAL(cond)   (cond)                              // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that cond being true is the nominal path
#define EDGECASE(cond)  (cond)                              // evaluating to 'cond', but additionnaly decorates it with compiler (and reader) hint that cond being true is only in case of some error handling path
#endif

// ************************************************************
// Simple bit tools
// ************************************************************

// a mask of 'uSetBitCount' ones, rest 0, starting at the lowest bit of a 8b unisgned integer
#define _MASK_8_LSB(uSetBitCount)      u8(1u << (uSetBitCount))-1u)
// a mask of 'uSetBitCount' ones, rest 0, starting at the lowest bit of a 16b unisgned integer
#define _MASK_16_LSB(uSetBitCount)      u16(1u << (uSetBitCount))-1u)
// a mask of 'uSetBitCount' ones, rest 0, starting at the lowest bit of a 32b unisgned integer
#define _MASK_32_LSB(uSetBitCount)      ((1u << (uSetBitCount))-1u)
// a mask of 'uSetBitCount' ones, rest 0, starting at the lowest bit of a 64b unisgned integer
#define _MASK_64_LSB(uSetBitCount)      ((1uLL << (uSetBitCount))-1uLL)

// a mask of 'uSetBitCount' ones, rest 0, starting at a given position 'uLowPos' from the lowest bit of a 8b unisgned integer
#define _MASK_8(uSetBitCount, uLowPos)      u8(_MASK_8_MSB(uSetBitCount) << (uLowPos))
// a mask of 'uSetBitCount' ones, rest 0, starting at a given position 'uLowPos' from the lowest bit of a 16b unisgned integer
#define _MASK_16(uSetBitCount, uLowPos)     u16(_MASK_16_MSB(uSetBitCount) << (uLowPos))
// a mask of 'uSetBitCount' ones, rest 0, starting at a given position 'uLowPos' from the lowest bit of a 32b unisgned integer
#define _MASK_32(uSetBitCount, uLowPos)     (_MASK_32_MSB(uSetBitCount) << (uLowPos))
// a mask of 'uSetBitCount' ones, rest 0, starting at a given position 'uLowPos' from the lowest bit of a 64b unisgned integer
#define _MASK_64(uSetBitCount, uLowPos)     (_MASK_32_MSB(uSetBitCount) << (uLowPos))

// a mask of 'uSetBitCount' ones from the highest bit of a 8b unsigned integer, rest 0. Undefined if uSetBitCount > 8
#define _MASK_8_MSB(uSetBitCount)       _MASK_8(uSetBitCount, 8-uSetBitCount)
// a mask of 'uSetBitCount' ones from the highest bit of a 16b unsigned integer, rest 0. Undefined if uSetBitCount > 16
#define _MASK_16_MSB(uSetBitCount)      _MASK_16(uSetBitCount, 16-uSetBitCount)
// a mask of 'uSetBitCount' ones from the highest bit of a 32b unsigned integer, rest 0. Undefined if uSetBitCount > 32
#define _MASK_32_MSB(uSetBitCount)      _MASK_32(uSetBitCount, 32-uSetBitCount)
// a mask of 'uSetBitCount' ones from the highest bit of a 64b unsigned integer, rest 0. Undefined if uSetBitCount > 64
#define _MASK_64_MSB(uSetBitCount)      _MASK_64(uSetBitCount, 64-uSetBitCount)

// branchless fills 8b with ones if 8b signed iVal < 0, otherwise results in zero.
#define _FULL_IFF_NEG_8B(iVal)   u8(i8(iVal) >> 63)
// branchless fills 8b with ones if 8b val is non-zero, otherwise results in zero.
#define _FULL_IFF_NONZERO_8B(val)   (_FULL_IFF_NEG_8B(val) | _FULL_IFF_NEG_8B(-val))

// branchless fills 16b with ones if 16b signed iVal < 0, otherwise results in zero.
#define _FULL_IFF_NEG_16B(iVal)  u16(i16(iVal) >> 15)
// branchless fills 16b with ones if 16b val is non-zero, otherwise results in zero.
#define _FULL_IFF_NONZERO_16B(val)   (_FULL_IFF_NEG_16B(val) | _FULL_IFF_NEG_16B(-val))

// branchless fills 32b with ones if 32b signed iVal < 0, otherwise results in zero.
#define _FULL_IFF_NEG_32B(iVal)  u32(i32(iVal) >> 31)
// branchless fills 16b with ones if 32b val is non-zero, otherwise results in zero.
#define _FULL_IFF_NONZERO_32B(val)   (_FULL_IFF_NEG_32B(val) | _FULL_IFF_NEG_32B(-val))

// branchless fills 64b with ones if 64b signed iVal < 0, otherwise results in zero.
#define _FULL_IFF_NEG_64B(iVal)  u64(i64(iVal) >> 63)
// branchless fills 64b with ones if 64b val is non-zero, otherwise results in zero.
#define _FULL_IFF_NONZERO_64B(val)   (_FULL_IFF_NEG_64B(val) | _FULL_IFF_NEG_64B(-val))

// returns true if 32b uVal is zero or an integer power of two
static FORCE_INLINE constexpr bool isPow2_32b(u32 uVal) { return 0 == (uVal & (uVal - 1u)); }

// returns true if 64b uVal is zero or an integer power of two
static FORCE_INLINE constexpr bool isPow2_64b(u64 uVal) { return 0 == (uVal & (uVal - 1uLL)); }

// ************************************************************
// Advanced compiler dependent tools and intrinsics
// ************************************************************

#if defined(_MSC_VER)  // Visual Studio Compiler
#  include "compiler_dependent_msvc.h"
#  ifdef _WIN64
#    define ARCH_64
#  endif
#else
#  error "compiler-dependent tools unknown for your compiler."
#endif

// ************************************************************
// Assert macro utilities
// ************************************************************

#if CHECK_ASSERTS
    bool platform_on_report_error_to_user_should_break(const char* szMsg);
    void platform_exit_process();
#  define _OnAssertFailed(szMsg) do { if(platform_on_report_error_to_user_should_break(szMsg)) DEBUG_BREAK(); else platform_exit_process(); } while(0)
#  define _ASSERT_MSG_HEADER(cond, msg)          "Assert failed : " _TO_STRING(cond) " #reason = " msg
#  define _ASSERT_MSG_FOOTER(file, line)         " in " file " at line " _TO_STRING(line)
#  define _ON_ASSERT_FAILED(cond, msg, file, line)    _OnAssertFailed(_ASSERT_MSG_HEADER(cond, msg) _ASSERT_MSG_FOOTER(file, line))
#else
#  define _ON_ASSERT_FAILED(cond, msg, file, line)
#endif

// ************************************************************
// UNREACHABLE()
// ************************************************************
// This macro identifies some piece of code as unreachable.
//   * Reaching it should be detected as an error in debug mode (where 'debug mode' means same compiling condition as for having Assertions checked)
//   * Or, in non-debug mode, should hint of some state or block that could be optimized-out 
//   * In any case, it also serves to decorate the code with explicit intent, for the benefit of the reader...
//
#if CHECK_ASSERTS
#  define _ON_UNREACHABLE_DEBUG(file, line)   _OnAssertFailed("Reached unreachable" _ASSERT_MSG_FOOTER(file, line))
   // This macro identifies some piece of code as unreachable. It should be a clear hint that getting there is not intended, and provides some compiler tooling in that case.
#  define UNREACHABLE()                       do { _ON_UNREACHABLE_DEBUG(__FILE__, __LINE__); _unreachable(); } while(0)
#else
   // This macro identifies some piece of code as unreachable. It should be a clear hint that getting there is not intended, and provides some compiler tooling in that case.
#  define UNREACHABLE()                       _unreachable()
#endif

// ************************************************************
// ASSUME() macro
// ************************************************************

// For correct results in optimized builds, please make sure your condition code never reaches non-inline and non-constexpr calls.

#if defined(_MSC_VER)  // Visual Studio Compiler
#  define _ASSUME(cond)  __assume(cond)
#else
#  define _ASSUME(cond)  do { if (!(cond)) _unreachable(); } while(0)
#endif

static FORCE_INLINE void NOOP() {}

// ************************************************************
// Assert() macro
// ************************************************************
// Defines an assumption 'cond' of current code, which should be taken as a programming error if it is ever found false.
// Note that the full assert and condition code shall vanish in an optimized, release build, and incur no runtime penalty.
//   For this very reason, please do *not* rely on any side effect in the cond expression.

#if CHECK_ASSERTS

// Defines an assumption 'cond' of current code, which should be taken as a programming error if it is ever found false.
//   Note that the full assert and condition code shall vanish in an optimized, release build, and incur no runtime penalty.
//   For this very reason, please do *not* rely on any side effect in the cond expression.
#define Assert(cond, msg) do { if (! NOMINAL(cond)) { _ON_ASSERT_FAILED(cond, msg, __FILE__, __LINE__); _unreachable(); } } while(0)

// This macro is similar to 'Assert' in that it would trigger a programmer error in debug mode.
//   In release mode, however, it provides compiler tooling as if we declared the opposite condition as an 'UNREACHABLE' state (which Assert does not)
#define Assume(cond, msg) Assert(cond, msg)

#else

// Defines an assumption 'cond' of current code, which should be taken as a programming error if it is ever found false.
//   Note that the full assert and condition code shall vanish in an optimized, release build, and incur no runtime penalty.
//   For this very reason, please do *not* rely on any side effect in the cond expression.
#define Assert(cond, msg) do { if (false && ! NOMINAL(cond)) { NOOP(); } } while(0)
    // Note that in release mode, we keep the condition behind an 'if (false &&' so that it should be optimized away, but still syntax-checked

// This macro is similar to 'Assert' in that it would trigger a programmer error in debug mode.
//   In release mode, however, it provides compiler tooling as if we declared the opposite condition as an 'UNREACHABLE' state
//   (which Assert does not), hopefully without emitting any runtime-eval of its condition... please make sure your condition
//   code never reaches non-inline and non-constexpr calls, however.
#define Assume(cond, msg) _ASSUME(cond)

#endif

// 'Assert' without an explanation message... when condition-code (which would be emitted in debug builds anyway) is deemed self-explanatory enough
#define Assert_(cond)   Assert(cond, "")
// 'Assume' without an explanation message... when condition-code (which would be emitted in debug builds anyway) is deemed self-explanatory enough
#define Assume_(cond)   Assume(cond, "")


static FORCE_INLINE void spinlock_acquire32(u32 volatile *pLock) {
    u32 uLoopCount = 0;
    u32 uValueBefore;
    do {
        uLoopCount++;
        Assert_(uLoopCount < 0x0100'0000u);
        uValueBefore = InterlockedCmpEx32(pLock, 1u, 0u);
        Assert_(uValueBefore <= 1u);
    } while (uValueBefore != 0u);
}

static FORCE_INLINE void spinlock_release32(u32 volatile *pLock) {
    u32 uValueBefore = InterlockedExch32(pLock, 0u);
    Assert_(uValueBefore == 1u);
}

#define SWAP(a, b) do { \
    auto tmp = a; \
    a = b; \
    b = tmp; \
} while(0)

local_func_inl constexpr u32 align_to(u32 uAlign, u32 uBytes) { return ((uBytes-1u) | (uAlign-1u)) + 1u; } // also works with size 0 given modulo arithmetics

#ifdef ARCH_64
    static FORCE_INLINE void spinlock_acquire64(u64 volatile *pLock) {
        u32 uLoopCount = 0;
        u64 uValueBefore;
        do {
            uLoopCount++;
            Assert_(uLoopCount < 0x0100'0000u);
            uValueBefore = InterlockedCmpEx64(pLock, 1uLL, 0uLL);
            Assert_(uValueBefore <= 1uLL);
        } while (uValueBefore != 0uLL);
    }

    static FORCE_INLINE void spinlock_release64(u64 volatile *pLock) {
        u64 uValueBefore = InterlockedExch64(pLock, 0uLL);
        Assert_(uValueBefore == 1uLL);
    }
#endif

// macro which defines default constructor, copy constructor, assignment operator, and destructor as 'default', so that a given struct (or even class)
//   is seen as trivially constructible, copyable, assignable and destructible, whatever the other explicit constructor methods are present :
//   struct MyStruct {
//      DECL_TRIVIAL_STRUCT_OPS(MyStruct);
//      // ... your other members or properties there
//   };
// Note that for the struct to really be trivial, none of its methods should be virtual, and any superclass and all members should be trivial also.
#define DECL_TRIVIAL_STRUCT_OPS(name) \
    name() = default; \
    name(const name&) = default; \
    name& operator=(const name&) = default; \
    ~name() = default   // <- we could (and may) leave the destructor as not specified, instead. But that way it is also a guard against another dtor definition.

// similar to DECL_TRIVIAL_STRUCT_OPS, for when you have a templatized struct (or even class) :
//   template<typename A, int B>
//   struct MyStruct {
//      DECL_TRIVIAL_T_STRUCT_OPS(MyStruct, A, B);
//      // ... your other members or properties there
//   };
#define DECL_TRIVIAL_T_STRUCT_OPS(name, ...) \
    name() = default; \
    name(const name<__VA_ARGS__>&) = default; \
    name<__VA_ARGS__>& operator=(const name<__VA_ARGS__>&) = default; \
    ~name() = default    // <- we could (and may) leave the destructor as not specified, instead. But that way it is also a guard against another dtor definition.


// Treats some warnings as hard errors

#if defined(_MSC_VER)  // Visual Studio
#  pragma warning (error: 4005)   // error on macro redefinition
#  pragma warning (error: 4244)   // error on conversion warnings
#  pragma warning (error: 4302)   // error on truncation
#  pragma warning (error: 4311)   // error on Pointer truncation
#else
#  warning "did not define warning-as-errors for your current compiler"
#endif

