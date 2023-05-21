#pragma once

#include "BaseDecls.h"
#include "legs64_core.h"

struct /*alignas(16)*/ u128 {
    u64 tLegs[2];
};
struct /*alignas(32)*/ u256 {
    u64 tLegs[4];
};
struct _f16 {
    u16 uBits;
};
union _f32 {
    u32 uBits;
    float _f;
};
union _f64 {
    u64 uBits;
    double _d;
};
struct /*alignas(16)*/ _f128 {
    u64 tBits[2];
};
struct /*alignas(32)*/ _f256 {
    u64 tBits[4];
};
struct /*alignas(64)*/ /*alignas(32)*/ r512 {
    u64 tBits[8];
};
struct /*alignas(128)*/ /*alignas(32)*/ r1024 {
    u64 tBits[16];
};

// 8x64b
struct _xfloat {
    u32 uSignAndFlags; 
    i32 iExpAtLSB;
    u64 tLegs[6];
    double dAsDouble;
};


// *****************************************************
// Comparisons
// *****************************************************
#define are_eq8(a, b)   ((u8(a) == u8(b)) ? u8(1u) : u8(0u)) 
#define are_eq16(a, b)  ((u16(a) == u16(b)) ? u8(1u) : u8(0u)) 
#define are_eq32(a, b)  ((u32(a) == u32(b)) ? u8(1u) : u8(0u)) 
#define are_eq64(a, b)  ((u64(a) == u64(b)) ? u8(1u) : u8(0u)) 
static FORCE_INLINE u8 are_eq128(u128 a, u128 b) {
    u8 leg0 = are_eq64(a.tLegs[0], b.tLegs[0]);
    u8 leg1 = are_eq64(a.tLegs[1], b.tLegs[1]);
    return leg0 & leg1;
}
static FORCE_INLINE u8 are_eq256(u256 a, u256 b) {
    u8 leg0 = are_eq64(a.tLegs[0], b.tLegs[0]);
    u8 leg1 = are_eq64(a.tLegs[1], b.tLegs[1]);
    u8 leg2 = are_eq64(a.tLegs[2], b.tLegs[2]);
    u8 leg3 = are_eq64(a.tLegs[3], b.tLegs[3]);
    return leg0 & leg1 & leg2 & leg3;
}
static u8 are_eq512(r512 a, r512 b) {
    for (int i=0; i<8; i++)
        if (a.tBits[i] != b.tBits[i])
            return 0u;
    return 1u;
}
static u8 are_eq1024(r1024 a, r1024 b) {
    for (int i=0; i<8; i++)
        if (a.tBits[i] != b.tBits[i])
            return 0u;
    return 1u;
}

#define sign8(a)  u8(u8(a) >> 7)
#define sign16(a) u8(u16(a) >> 15)
#define sign32(a) u8(u32(a) >> 31)
#define sign64(a) u8(u64(a) >> 63)
static FORCE_INLINE u8 sign128(u128 a) { return sign64(a.tLegs[1]); }
static FORCE_INLINE u8 sign256(u256 a) { return sign64(a.tLegs[3]); }

#define sign8_(a)  u8((a) & (1u << 7))
#define sign16_(a) u16((a) & (1u << 15))
#define sign32_(a) u32((a) & (1u << 31))
#define sign64_(a) u64((a) & (1uLL << 63))
static FORCE_INLINE u64 sign128_(u128 a) { return sign64_(a.tLegs[1]); }
static FORCE_INLINE u64 sign256_(u256 a) { return sign64_(a.tLegs[3]); }

#define is_strict_less_u8(a, b)  ((u8(a) < u8(b)) ? u8(1u) : u8(0u))
#define is_strict_less_u16(a, b) ((u16(a) < u16(b)) ? u8(1u) : u8(0u))
#define is_strict_less_u32(a, b) ((u32(a) < u32(b)) ? u8(1u) : u8(0u))
#define is_strict_less_u64(a, b) ((u64(a) < u64(b)) ? u8(1u) : u8(0u))
static FORCE_INLINE u8 is_strict_less_u128(u128 a, u128 b) {
    if (a.tLegs[1] == b.tLegs[1])
        return is_strict_less_u64(a.tLegs[0], b.tLegs[0]);
    else
        return is_strict_less_u64(a.tLegs[1], b.tLegs[1]);
}
static FORCE_INLINE u8 is_strict_less_u256(u256 a, u256 b) {
    if (a.tLegs[3] == b.tLegs[3]) {
        if (a.tLegs[2] == b.tLegs[2]) {
            if (a.tLegs[1] == b.tLegs[1])
                return is_strict_less_u64(a.tLegs[0], b.tLegs[0]);
            else
                return is_strict_less_u64(a.tLegs[1], b.tLegs[1]);
        } else
            return is_strict_less_u64(a.tLegs[2], b.tLegs[2]);
    } else
        return is_strict_less_u64(a.tLegs[3], b.tLegs[3]);
}

#define is_strict_less_i8(a, b)  ((i8(a) < i8(b)) ? u8(1u) : u8(0u)) 
#define is_strict_less_i16(a, b) ((i16(a) < i16(b)) ? u8(1u) : u8(0u)) 
#define is_strict_less_i32(a, b) ((i32(a) < i32(b)) ? u8(1u) : u8(0u)) 
#define is_strict_less_i64(a, b) ((i64(a) < i64(b)) ? u8(1u) : u8(0u)) 
static FORCE_INLINE u8 is_strict_less_i128(u128 a, u128 b) {
    if (a.tLegs[1] == b.tLegs[1]) {
        return is_strict_less_u64(a.tLegs[0], b.tLegs[0]); // unsigned cmp on lower leg
    } else
        return is_strict_less_i64(a.tLegs[1], b.tLegs[1]); // signed cmp on higher leg
}

static FORCE_INLINE u8 is_strict_less_i256(u256 a, u256 b) {
    if (a.tLegs[3] == b.tLegs[3]) {
        // unsigned cmp on lower legs
        if (a.tLegs[2] == b.tLegs[2]) {
            if (a.tLegs[1] == b.tLegs[1])
                return is_strict_less_u64(a.tLegs[0], b.tLegs[0]);
            else
                return is_strict_less_u64(a.tLegs[1], b.tLegs[1]);
        } else
            return is_strict_less_u64(a.tLegs[2], b.tLegs[2]);
    } else
        return is_strict_less_i64(a.tLegs[3], b.tLegs[3]); // signed cmp higher leg
}

#define boolNot8(a)     u8(u8(a) ^ 1u)
#define boolNot16(a)    u16(u16(a) ^ 1u)
#define boolNot32(a)    (u32(a) ^ 1u)
#define boolNot64(a)    (u64(a) ^ 1uLL)
static FORCE_INLINE u128 boolNot128(u128 a) {
    return u128{ boolNot64(a.tLegs[0]), 0uLL };
}
static FORCE_INLINE u256 boolNot256(u256 a) {
    return u256{ boolNot64(a.tLegs[0]), 0uLL, 0uLL, 0uLL };
}

#define bitNot8(a)   u8(~u8(a))
#define bitNot16(a)  u16(~u16(a))
#define bitNot32(a)  (~u32(a))
#define bitNot64(a)  (~u64(a))
static FORCE_INLINE u128 bitNot128(u128 a) {
    return u128{ bitNot64(a.tLegs[0]), bitNot64(a.tLegs[1]) };
}
static FORCE_INLINE u256 bitNot256(u256 a) {
    return u256{ bitNot64(a.tLegs[0]), bitNot64(a.tLegs[1]), bitNot64(a.tLegs[2]), bitNot64(a.tLegs[3]) };
}
static r512 bitNot512(r512 a) {
    r512 result;
    for (int i=0; i<8; i++)
        result.tBits[i] = bitNot64(a.tBits[i]);
    return result;
}
static r1024 bitNot1024(r1024 a) {
    r1024 result;
    for (int i=0; i<16; i++)
        result.tBits[i] = bitNot64(a.tBits[i]);
    return result;
}

// *****************************************************
// Bitwise ops
// *****************************************************
#define bitand8(a, b)   u8(u8(a) & u8(b))
#define bitand16(a, b)  u16(u16(a) & u16(b))
#define bitand32(a, b)  (u32(a) & u32(b))
#define bitand64(a, b)  (u64(a) & u64(b))
static FORCE_INLINE u128 bitand128(u128 a, u128 b) {
    u128 result;
    result.tLegs[0] = bitand64(a.tLegs[0], b.tLegs[0]);
    result.tLegs[1] = bitand64(a.tLegs[1], b.tLegs[1]);
    return result;
}
static FORCE_INLINE u256 bitand256(u256 a, u256 b) {
    u256 result;
    result.tLegs[0] = bitand64(a.tLegs[0], b.tLegs[0]);
    result.tLegs[1] = bitand64(a.tLegs[1], b.tLegs[1]);
    result.tLegs[2] = bitand64(a.tLegs[2], b.tLegs[2]);
    result.tLegs[3] = bitand64(a.tLegs[3], b.tLegs[3]);
    return result;
}
static r512 bitand512(r512 a, r512 b) {
    r512 result;
    for (int i=0; i<8; i++)
        result.tBits[i] = bitand64(a.tBits[i], b.tBits[i]);
    return result;
}
static r1024 bitand1024(r1024 a, r1024 b) {
    r1024 result;
    for (int i=0; i<16; i++)
        result.tBits[i] = bitand64(a.tBits[i], b.tBits[i]);
    return result;
}

#define bitor8(a, b)    u8(u8(a) | u8(b))
#define bitor16(a, b)   u16(u16(a) | u16(b))
#define bitor32(a, b)   (u32(a) | u32(b))
#define bitor64(a, b)   (u64(a) | u64(b))
static FORCE_INLINE u128 bitor128(u128 a, u128 b) {
    u128 result;
    result.tLegs[0] = bitor64(a.tLegs[0], b.tLegs[0]);
    result.tLegs[1] = bitor64(a.tLegs[1], b.tLegs[1]);
    return result;
}
static FORCE_INLINE u256 bitor256(u256 a, u256 b) {
    u256 result;
    result.tLegs[0] = bitor64(a.tLegs[0], b.tLegs[0]);
    result.tLegs[1] = bitor64(a.tLegs[1], b.tLegs[1]);
    result.tLegs[2] = bitor64(a.tLegs[2], b.tLegs[2]);
    result.tLegs[3] = bitor64(a.tLegs[3], b.tLegs[3]);
    return result;
}
static r512 bitor512(r512 a, r512 b) {
    r512 result;
    for (int i=0; i<8; i++)
        result.tBits[i] = bitor64(a.tBits[i], b.tBits[i]);
    return result;
}
static r1024 bitor1024(r1024 a, r1024 b) {
    r1024 result;
    for (int i=0; i<16; i++)
        result.tBits[i] = bitor64(a.tBits[i], b.tBits[i]);
    return result;
}

#define bitXor8(a, b)    u8(u8(a) ^ u8(b))
#define bitXor16(a, b)   u16(u16(a) ^ u16(b))
#define bitXor32(a, b)   (u32(a) ^ u32(b))
#define bitXor64(a, b)   (u64(a) ^ u64(b))
static FORCE_INLINE u128 bitXor128(u128 a, u128 b) {
    u128 result;
    result.tLegs[0] = bitXor64(a.tLegs[0], b.tLegs[0]);
    result.tLegs[1] = bitXor64(a.tLegs[1], b.tLegs[1]);
    return result;
}
static FORCE_INLINE u256 bitXor256(u256 a, u256 b) {
    u256 result;
    result.tLegs[0] = bitXor64(a.tLegs[0], b.tLegs[0]);
    result.tLegs[1] = bitXor64(a.tLegs[1], b.tLegs[1]);
    result.tLegs[2] = bitXor64(a.tLegs[2], b.tLegs[2]);
    result.tLegs[3] = bitXor64(a.tLegs[3], b.tLegs[3]);
    return result;
}
static r512 bitXor512(r512 a, r512 b) {
    r512 result;
    for (int i=0; i<8; i++)
        result.tBits[i] = bitXor64(a.tBits[i], b.tBits[i]);
    return result;
}
static r1024 bitXor1024(r1024 a, r1024 b) {
    r1024 result;
    for (int i=0; i<16; i++)
        result.tBits[i] = bitXor64(a.tBits[i], b.tBits[i]);
    return result;
}

// **************************
// Regular integral negation
// **************************
#define neg8(a)         u8( -u8(a));
#define neg16(a)       u16(-u16(a));
#define neg32(a)       u32(-u32(a));
#define neg64(a)       u64(-u64(a));

// Negating our format is taking 2's complement
// 2's complement is inverting all bits, then adding 1.

static FORCE_INLINE u128 neg128(u128 a) {
    u128 result;
    u8 carry;
    result.tLegs[0] = AddCarry64(~(a.tLegs[0]), 1uLL, 0, &carry);
    result.tLegs[1] = AddCarry64(~(a.tLegs[1]), 0uLL, carry, &carry);
    return result;
}
static FORCE_INLINE u256 neg256(u256 a) {
    u256 result;
    u8 carry;
    result.tLegs[0] = AddCarry64(~(a.tLegs[0]), 1uLL, 0, &carry);
    result.tLegs[1] = AddCarry64(~(a.tLegs[1]), 0uLL, carry, &carry);
    result.tLegs[2] = AddCarry64(~(a.tLegs[2]), 0uLL, carry, &carry);
    result.tLegs[3] = AddCarry64(~(a.tLegs[3]), 0uLL, carry, &carry);
    return result;
}

// *****************************************************
// Regular modulo add (works for signed or unsigned)
// *****************************************************

#define add8(a, b)     u8(u8(a) + u8(b))
#define add16(a, b)    u16(u16(a) + u16(b))
#define add32(a, b)    (u32(a) + u32(b))
#define add64(a, b)    (u64(a) + u64(b))
static FORCE_INLINE u128 add128(u128 a, u128 b) {
    u128 result;
    u8 carry;
    result.tLegs[0] = AddCarry64(a.tLegs[0], b.tLegs[0], 0, &carry);
    result.tLegs[1] = AddCarry64(a.tLegs[1], b.tLegs[1], carry, &carry);
    return result;
}
static FORCE_INLINE u256 add256(u256 a, u256 b) {
    u256 result;
    u8 carry;
    result.tLegs[0] = AddCarry64(a.tLegs[0], b.tLegs[0], 0, &carry);
    result.tLegs[1] = AddCarry64(a.tLegs[1], b.tLegs[1], carry, &carry);
    result.tLegs[2] = AddCarry64(a.tLegs[2], b.tLegs[2], carry, &carry);
    result.tLegs[3] = AddCarry64(a.tLegs[3], b.tLegs[3], carry, &carry);
    return result;
}

// *****************************************************
// Regular modulo sub (works for signed or unsigned)
// *****************************************************

#define sub8(a, b)     u8(u8(a) - u8(b))
#define sub16(a, b)    u16(u16(a) - u16(b))
#define sub32(a, b)    (u32(a) - u32(b))
#define sub64(a, b)    (u64(a) - u64(b))
static FORCE_INLINE u128 sub128(u128 a, u128 b) {
    u128 result;
    u8 borrow;
    result.tLegs[0] = SubBorrow64(a.tLegs[0], b.tLegs[0], 0, &borrow);
    result.tLegs[1] = SubBorrow64(a.tLegs[1], b.tLegs[1], borrow, &borrow);
    return result;
}
static FORCE_INLINE u256 sub256(u256 a, u256 b) {
    u256 result;
    u8 borrow;
    result.tLegs[0] = SubBorrow64(a.tLegs[0], b.tLegs[0], 0, &borrow);
    result.tLegs[1] = SubBorrow64(a.tLegs[1], b.tLegs[1], borrow, &borrow);
    result.tLegs[2] = SubBorrow64(a.tLegs[2], b.tLegs[2], borrow, &borrow);
    result.tLegs[3] = SubBorrow64(a.tLegs[3], b.tLegs[3], borrow, &borrow);
    return result;
}

// **************************************************
// Regular modulo add with hardware support for carry (in and out)
// **************************************************

static FORCE_INLINE u8 add8_car(u8 a, u8 b, u8 cIn, u8* cOut) {
    return AddCarry8(a, b, cIn, cOut);
}
static FORCE_INLINE u16 add16_car(u16 a, u16 b, u8 cIn, u8* cOut) {
    return AddCarry16(a, b, cIn, cOut);
}
static FORCE_INLINE u32 add32_car(u32 a, u32 b, u8 cIn, u8* cOut) {
    return AddCarry32(a, b, cIn, cOut);
}
static FORCE_INLINE u64 add64_car(u64 a, u64 b, u8 cIn, u8* cOut) {
    return AddCarry64(a, b, cIn, cOut);
}
static FORCE_INLINE u128 add128_car(u128 a, u128 b, u8 cIn, u8* cOut) {
    u128 result;
    u8 carry;
    result.tLegs[0] = AddCarry64(a.tLegs[0], b.tLegs[0], cIn, &carry);
    result.tLegs[1] = AddCarry64(a.tLegs[1], b.tLegs[1], carry, &carry);
    *cOut = carry;
    return result;
}
static FORCE_INLINE u256 add256_car(u256 a, u256 b, u8 cIn, u8* cOut) {
    u256 result;
    u8 carry;
    result.tLegs[0] = AddCarry64(a.tLegs[0], b.tLegs[0], cIn, &carry);
    result.tLegs[1] = AddCarry64(a.tLegs[1], b.tLegs[1], carry, &carry);
    result.tLegs[2] = AddCarry64(a.tLegs[2], b.tLegs[2], carry, &carry);
    result.tLegs[3] = AddCarry64(a.tLegs[3], b.tLegs[3], carry, &carry);
    *cOut = carry;
    return result;
}

// ***************************************************
// Regular modulo sub with hardware support for borrow (in and out)
// ***************************************************

static FORCE_INLINE u8 sub8_bor(u8 a, u8 b, u8 bIn, u8* bOut) {
    return SubBorrow8(a, b, bIn, bOut);
}
static FORCE_INLINE u16 sub16_bor(u16 a, u16 b, u8 bIn, u8* bOut) {
    return SubBorrow16(a, b, bIn, bOut);
}
static FORCE_INLINE u32 sub32_bor(u32 a, u32 b, u8 bIn, u8* bOut) {
    return SubBorrow32(a, b, bIn, bOut);
}
static FORCE_INLINE u64 sub64_bor(u64 a, u64 b, u8 bIn, u8* bOut) {
    return SubBorrow64(a, b, bIn, bOut);
}
static FORCE_INLINE u128 sub128_bor(u128 a, u128 b, u8 bIn, u8* bOut) {
    u128 result;
    u8 borrow;
    result.tLegs[0] = SubBorrow64(a.tLegs[0], b.tLegs[0], bIn, &borrow);
    result.tLegs[1] = SubBorrow64(a.tLegs[1], b.tLegs[1], borrow, &borrow);
    *bOut = borrow;
    return result;
}
static FORCE_INLINE u256 sub256_bor(u256 a, u256 b, u8 bIn, u8* bOut) {
    u256 result;
    u8 borrow;
    result.tLegs[0] = SubBorrow64(a.tLegs[0], b.tLegs[0], bIn, &borrow);
    result.tLegs[1] = SubBorrow64(a.tLegs[1], b.tLegs[1], borrow, &borrow);
    result.tLegs[2] = SubBorrow64(a.tLegs[2], b.tLegs[2], borrow, &borrow);
    result.tLegs[3] = SubBorrow64(a.tLegs[3], b.tLegs[3], borrow, &borrow);
    *bOut = borrow;
    return result;
}


/*
    Adder with check for unsigned semantics
    ---------------------------------------

	A		B		COut  	R  	 SA SB SR		A	B 	 R	SemR
	                             
	00		00  	0		00	 0  0  0		0 + 0 -> 0	0
	01		00		0		01	 0  0  0		1 + 0 -> 1	1
	10		00		0		10	 1  0  1		2 + 0 -> 2	2
	11		00		0		11	 1  0  1		3 + 0 -> 3	3
	00		01  	0		01	 0  0  0		0 + 1 -> 1	1
	01		01		0		10	 0  0  1		1 + 1 -> 2	2
	10		01		0		11	 1  0  1		2 + 1 -> 3  3
	11		01		1		00	 1  0  0		3 + 1 -> 0  4	*
	00		10  	0		10	 0  1  1		0 + 2 -> 2  2
	01		10		0		11	 0  1  1		1 + 2 -> 3  3
	10		10		1		00	 1  1  0		2 + 2 -> 0  4   *
	11		10		1		01	 1  1  0		3 + 2 -> 1  5	*
	00		11  	0		11	 0  1  1		0 + 3 -> 3  3 
	01		11		1		00	 0  1  0		1 + 3 -> 0  4	* 
	10		11		1		01	 1  1  0		2 + 3 -> 1  5   *
	11		11		1		10	 1  1  1		3 + 3 -> 2  6   *

'Error' == when carry out is set
*/
static FORCE_INLINE u8 add8_chk_uns(u8 a, u8 b, u8* oor) {
    return add8_car(a, b, 0, oor);
}
static FORCE_INLINE u16 add16_chk_uns(u16 a, u16 b, u8* oor) {
    return add16_car(a, b, 0, oor);
}
static FORCE_INLINE u32 add32_chk_uns(u32 a, u32 b, u8* oor) {
    return add32_car(a, b, 0, oor);
}
static FORCE_INLINE u64 add64_chk_uns(u64 a, u64 b, u8* oor) {
    return add64_car(a, b, 0, oor);
}
static FORCE_INLINE u128 add128_chk_uns(u128 a, u128 b, u8* oor) {
    return add128_car(a, b, 0, oor);
}
static FORCE_INLINE u256 add256_chk_uns(u256 a, u256 b, u8* oor) {
    return add256_car(a, b, 0, oor);
}


/*
    Subtracter with check for unsigned semantics
    --------------------------------------------

	A		B		BOut  	R  	 SA SB SR		A	B 	 R	SemR
	                                     
	00		00  	0		00	 0  0  0		0 - 0 -> 0	 0
	01		00		0		01	 0  0  0		1 - 0 -> 1	 1
	10		00		0		10	 1  0  1		2 - 0 -> 2	 2
	11		00		0		11	 1  0  1		3 - 0 -> 3	 3
	00		01  	1		11	 0  0  1		0 - 1 -> 3	-1  *
	01		01		0		00	 0  0  0		1 - 1 -> 0	 0
	10		01		0		01	 1  0  0		2 - 1 -> 1   1
	11		01		0		10	 1  0  1		3 - 1 -> 2   2
	00		10  	1		10	 0  1  1		0 - 2 -> 2  -2  *
	01		10		1		11	 0  1  1		1 - 2 -> 3  -1  *
	10		10		0		00	 1  1  0		2 - 2 -> 0   0
	11		10		0		01	 1  1  0		3 - 2 -> 1   1
	00		11  	1		01	 0  1  0		0 - 3 -> 1  -3  *
	01		11		1		10	 0  1  1		1 - 3 -> 2  -2  *
	10		11		1		11	 1  1  1		2 - 3 -> 3  -1  *
	11		11		0		00	 1  1  0		3 - 3 -> 0   0

'Error' == when borrow out is set
*/
static FORCE_INLINE u8 sub8_chk_uns(u8 a, u8 b, u8* oor) {
    return sub8_bor(a, b, 0, oor);
}
static FORCE_INLINE u16 sub16_chk_uns(u16 a, u16 b, u8* oor) {
    return sub16_bor(a, b, 0, oor);
}
static FORCE_INLINE u32 sub32_chk_uns(u32 a, u32 b, u8* oor) {
    return sub32_bor(a, b, 0, oor);
}
static FORCE_INLINE u64 sub64_chk_uns(u64 a, u64 b, u8* oor) {
    return sub64_bor(a, b, 0, oor);
}
static FORCE_INLINE u128 sub128_chk_uns(u128 a, u128 b, u8* oor) {
    return sub128_bor(a, b, 0, oor);
}
static FORCE_INLINE u256 sub256_chk_uns(u256 a, u256 b, u8* oor) {
    return sub256_bor(a, b, 0, oor);
}

/*
    Adder with check for signed semantics
    -------------------------------------

	A		B		R  		COut  	SA SB SR	 A	   B 	 R   SemR	
	 
	00		00  	00		0		0  0  0		 0  +  0 ->  0	  0		
	01		00		01		0		0  0  0		 1  +  0 ->  1	  1		
	10		00		10		0		1  0  1	 	-2  +  0 -> -2	 -2		
	11		00		11		0		1  0  1		-1  +  0 -> -1	 -1		
	00		01  	01		0		0  0  0		 0  +  1 ->  1	  1		
	01		01		10		0		0  0  1		 1  +  1 -> -2	  2 *	
	10		01		11		0		1  0  1		-2  +  1 -> -1   -1		
	11		01		00		1		1  0  0		-1  +  1 ->  0    0		
	00		10  	10		0		0  1  1		 0  + -2 -> -2   -2		
	01		10		11		0		0  1  1		 1  + -2 -> -1   -1		
	10		10		00		1		1  1  0		-2  + -2 ->  0   -4 *	
	11		10		01		1		1  1  0		-1  + -2 ->  1   -3	*	
	00		11  	11		0		0  1  1		 0  + -1 -> -1   -1 	
	01		11		00		1		0  1  0		 1  + -1 ->  0    0		
	10		11		01		1		1  1  0		-2  + -1 ->  1   -3 *	
	11		11		10		1		1  1  1		-1  + -1 -> -2   -2		

'Error' == when (signA == signB) *And* (signR != carry out)
*/

static FORCE_INLINE u8 add8_chk_sgn(u8 a, u8 b, u8* oor) {
    u8 carry;
    u8 uResult = add8_car(a, b, 0, &carry);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u8 a_eq_b = ~(a^b); u8 r_neq_c = (uResult^(carry<<7));
    *oor = sign8(a_eq_b & r_neq_c); //'Error' == when (signA == signB) *And* (signR != carry out)
    return uResult;
}
static FORCE_INLINE u16 add16_chk_sgn(u16 a, u16 b, u8* oor) {
    u8 carry;
    u16 uResult = add16_car(a, b, 0, &carry);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u16 a_eq_b = ~(a^b); u16 r_neq_c = (uResult^(u16(carry)<<15));
    *oor = sign16(a_eq_b & r_neq_c); //'Error' == when (signA == signB) *And* (signR != carry out)
    return uResult;
}
static FORCE_INLINE u32 add32_chk_sgn(u32 a, u32 b, u8* oor) {
    u8 carry;
    u32 uResult = add32_car(a, b, 0, &carry);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u32 a_eq_b = ~(a^b); u32 r_neq_c = (uResult^(u32(carry)<<31));
    *oor = sign32(a_eq_b & r_neq_c); //'Error' == when (signA == signB) *And* (signR != carry out)
    return uResult;
}
static FORCE_INLINE u64 add64_chk_sgn(u64 a, u64 b, u8* oor) {
    u8 carry;
    u64 uResult = add64_car(a, b, 0, &carry);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_eq_b = ~(a^b); u64 r_neq_c = (uResult^(u64(carry)<<63));
    *oor = sign64(a_eq_b & r_neq_c); //'Error' == when (signA == signB) *And* (signR != carry out)
    return uResult;
}
static FORCE_INLINE u128 add128_chk_sgn(u128 a, u128 b, u8* oor) {
    u8 carry;
    u128 result = add128_car(a, b, 0, &carry);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_eq_b = ~(a.tLegs[1]^b.tLegs[1]); u64 r_neq_c = (result.tLegs[1]^(u64(carry)<<63));
    *oor = sign64(a_eq_b & r_neq_c); //'Error' == when (signA == signB) *And* (signR != carry out)
    return result;
}
static FORCE_INLINE u256 add256_chk_sgn(u256 a, u256 b, u8* oor) {
    u8 carry;
    u256 result = add256_car(a, b, 0, &carry);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_eq_b = ~(a.tLegs[3]^b.tLegs[3]); u64 r_neq_c = (result.tLegs[3]^(u64(carry)<<63));
    *oor = sign64(a_eq_b & r_neq_c); //'Error' == when (signA == signB) *And* (signR != carry out)
    return result;
}


/*
    Subtracter with check for signed semantics
    ------------------------------------------

	A		B		R  		BOut  	SA SB SR	 A	   B 	 R	TR		
	                                                                    
	00		00  	00		0		0  0  0		 0  -  0 ->  0	 0      
	01		00		01		0		0  0  0		 1  -  0 ->  1	 1      
	10		00		10		0		1  0  1	 	-2  -  0 -> -2	-2      
	11		00		11		0		1  0  1		-1  -  0 -> -1	-1      
	00		01  	11		1		0  0  1		 0  -  1 -> -1	-1      
	01		01		00		0		0  0  0		 1  -  1 ->  0	 0      
	10		01		01		0		1  0  0		-2  -  1 ->  1  -3  *   
	11		01		10		0		1  0  1		-1  -  1 -> -2  -2      
	00		10  	10		1		0  1  1		 0  - -2 -> -2   2  *   
	01		10		11		1		0  1  1		 1  - -2 -> -1   3  *   
	10		10		00		0		1  1  0		-2  - -2 ->  0   0      
	11		10		01		0		1  1  0		-1  - -2 ->  1   1      
	00		11  	01		1		0  1  0		 0  - -1 ->  1   1      
	01		11		10		1		0  1  1		 1  - -1 -> -2   2  *   
	10		11		11		1		1  1  1		-2  - -1 -> -1  -1      
	11		11		00		0		1  1  0		-1  - -1 ->  0   0      

'Error' == when (signA != signB) *And* (signR == borrow out)

'Error' also is *almost* same errors as-if signed Adder of 2's complement of B.
   BUT !! Note that '0 - minint', does here raise an error (as it should, same as unary -minint),
		     when signedadd(0, twoscompl(minint)) would not
*/

static FORCE_INLINE u8 sub8_chk_sgn(u8 a, u8 b, u8* oor) {
    u8 borrow;
    u8 uResult = sub8_bor(a, b, 0, &borrow);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u8 a_neq_b = (a^b); u8 r_eq_c = ~(uResult^(borrow<<7));
    *oor = sign8(a_neq_b & r_eq_c); //'Error' == when (signA != signB) *And* (signR == borrow out)
    return uResult;
}
static FORCE_INLINE u16 sub16_chk_sgn(u16 a, u16 b, u8* oor) {
    u8 borrow;
    u16 uResult = sub16_bor(a, b, 0, &borrow);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u16 a_neq_b = (a^b); u16 r_eq_c = ~(uResult^(u16(borrow)<<15));
    *oor = sign16(a_neq_b & r_eq_c); //'Error' == when (signA != signB) *And* (signR == borrow out)
    return uResult;
}
static FORCE_INLINE u32 sub32_chk_sgn(u32 a, u32 b, u8* oor) {
    u8 borrow;
    u32 uResult = sub32_bor(a, b, 0, &borrow);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u32 a_neq_b = (a^b); u32 r_eq_c = ~(uResult^(u32(borrow)<<31));
    *oor = sign32(a_neq_b & r_eq_c); //'Error' == when (signA != signB) *And* (signR == borrow out)
    return uResult;
}
static FORCE_INLINE u64 sub64_chk_sgn(u64 a, u64 b, u8* oor) {
    u8 borrow;
    u64 uResult = sub64_bor(a, b, 0, &borrow);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_neq_b = (a^b); u64 r_eq_c = ~(uResult^(u64(borrow)<<63));
    *oor = sign64(a_neq_b & r_eq_c); //'Error' == when (signA != signB) *And* (signR == borrow out)
    return uResult;
}
static FORCE_INLINE u128 sub128_chk_sgn(u128 a, u128 b, u8* oor) {
    u8 borrow;
    u128 result = sub128_bor(a, b, 0, &borrow);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_neq_b = (a.tLegs[1]^b.tLegs[1]); u64 r_eq_c = ~(result.tLegs[1]^(u64(borrow)<<63));
    *oor = sign64(a_neq_b & r_eq_c); //'Error' == when (signA != signB) *And* (signR == borrow out)
    return result;
}
static FORCE_INLINE u256 sub256_chk_sgn(u256 a, u256 b, u8* oor) {
    u8 borrow;
    u256 result = sub256_bor(a, b, 0, &borrow);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_neq_b = (a.tLegs[3]^b.tLegs[3]); u64 r_eq_c = ~(result.tLegs[3]^(u64(borrow)<<63));
    *oor = sign64(a_neq_b & r_eq_c); //'Error' == when (signA != signB) *And* (signR == borrow out)
    return result;
}

/*
    Adder with check for mixed semantics. (ie. A is considered signed, R too, but *not* B (allows for some add-large-constant special derogations?)
    -------------------------------------

	A		B		R  		COut  	SA SB SR	 A	   B 	 R   TR		(~SA)|SR   SB|(SR^SA)
	 
	00		00  	00		0		0  0  0		 0  +  0 ->  0	  0		  1			0
	01		00		01		0		0  0  0		 1  +  0 ->  1	  1		  1         0
	10		00		10		0		1  0  1	 	-2  +  0 -> -2	 -2		  1         0
	11		00		11		0		1  0  1		-1  +  0 -> -1	 -1		  1         0
	00		01  	01		0		0  0  0		 0  +  1 ->  1	  1		  1         0
	01		01		10		0		0  0  1		 1  +  1 -> -2	  2 *	  1         1
	10		01		11		0		1  0  1		-2  +  1 -> -1   -1		  1         0
	11		01		00		1		1  0  0		-1  +  1 ->  0    0		  0         1
	00		10  	10		0		0  1  1		 0  +  2 -> -2    2	*	  1         1
	01		10		11		0		0  1  1		 1  +  2 -> -1    3 *	  1         1
	10		10		00		1		1  1  0		-2  +  2 ->  0    0		  0         1
	11		10		01		1		1  1  0		-1  +  2 ->  1    1		  0         1
	00		11  	11		0		0  1  1		 0  +  3 -> -1    3 * 	  1         1
	01		11		00		1		0  1  0		 1  +  3 ->  0    4	*	  1         1
	10		11		01		1		1  1  0		-2  +  3 ->  1    1 	  0         1
	11		11		10		1		1  1  1		-1  +  3 -> -2    2	*	  1         1

'Error' == ((~SA)|SR) & (SB|(SR^SA))
*/

static FORCE_INLINE u8 add8_chk_mix(u8 a, u8 b, u8* oor) {
    u8 uResult = add8(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u8 na_or_r = ((~a)|uResult); u8 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign8(na_or_r & b_or_r_neq_a); //'Error' == ((~SA)|SR) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u16 add16_chk_mix(u16 a, u16 b, u8* oor) {
    u16 uResult = add16(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u16 na_or_r = ((~a)|uResult); u16 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign16(na_or_r & b_or_r_neq_a); //'Error' == ((~SA)|SR) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u32 add32_chk_mix(u32 a, u32 b, u8* oor) {
    u32 uResult = add32(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u32 na_or_r = ((~a)|uResult); u32 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign32(na_or_r & b_or_r_neq_a); //'Error' == ((~SA)|SR) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u64 add64_chk_mix(u64 a, u64 b, u8* oor) {
    u64 uResult = add64(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 na_or_r = ((~a)|uResult); u64 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign64(na_or_r & b_or_r_neq_a); //'Error' == ((~SA)|SR) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u128 add128_chk_mix(u128 a, u128 b, u8* oor) {
    u128 result = add128(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 na_or_r = ((~a.tLegs[1])|result.tLegs[1]); u64 b_or_r_neq_a = (b.tLegs[1]|(result.tLegs[1]^a.tLegs[1]));
    *oor = sign64(na_or_r & b_or_r_neq_a); //'Error' == ((~SA)|SR) & (SB|(SR^SA))
    return result;
}
static FORCE_INLINE u256 add256_chk_mix(u256 a, u256 b, u8* oor) {
    u256 result = add256(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 na_or_r = ((~a.tLegs[3])|result.tLegs[3]); u64 b_or_r_neq_a = (b.tLegs[3]|(result.tLegs[3]^a.tLegs[3]));
    *oor = sign64(na_or_r & b_or_r_neq_a); //'Error' == ((~SA)|SR) & (SB|(SR^SA))
    return result;
}

/*
    Subtracter with check for mixed semantics. (ie. A is considered signed, R too, but *not* B (allows for some sub-large-constant special derogations?)
    -------------------------------------

	A		B		R  		BR  	SA SB SR	 A	   B 	 R   TR		SA|(~SR)   SB|(SR^SA)
	
	00		00  	00		0		0  0  0		 0  -  0 ->  0	  0		  1			0
	01		00		01		0		0  0  0		 1  -  0 ->  1	  1       1         0
	10		00		10		0		1  0  1	 	-2  -  0 -> -2	 -2       1         0
	11		00		11		0		1  0  1		-1  -  0 -> -1	 -1       1         0
	00		01  	11		1		0  0  1		 0  -  1 -> -1	 -1       0         1
	01		01		00		0		0  0  0		 1  -  1 ->  0	  0       1         0
	10		01		01		0		1  0  0		-2  -  1 ->  1   -3 *     1         1
	11		01		10		0		1  0  1		-1  -  1 -> -2   -2       1         0
	00		10  	10		1		0  1  1		 0  -  2 -> -2   -2       0         1  
	01		10		11		1		0  1  1		 1  -  2 -> -1   -1       0         1
	10		10		00		0		1  1  0		-2  -  2 ->  0   -4 *     1         1
	11		10		01		0		1  1  0		-1  -  2 ->  1   -3 *     1         1
	00		11  	01		1		0  1  0		 0  -  3 -> -1   -3 *     1         1
	01		11		10		1		0  1  1		 1  -  3 -> -2   -2       0         1
	10		11		11		1		1  1  1		-2  -  3 -> -1   -5 *     1         1
	11		11		00		0		1  1  0		-1  -  3 ->  0   -4 *     1         1

'Error' == (SA|(~SR)) & (SB|(SR^SA))
*/

static FORCE_INLINE u8 sub8_chk_mix(u8 a, u8 b, u8* oor) {
    u8 uResult = sub8(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u8 a_or_nr = (a|(~uResult)); u8 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign8(a_or_nr & b_or_r_neq_a); //'Error' == (SA|(~SR)) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u16 sub16_chk_mix(u16 a, u16 b, u8* oor) {
    u16 uResult = sub16(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u16 a_or_nr = (a|(~uResult)); u16 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign16(a_or_nr & b_or_r_neq_a); //'Error' == (SA|(~SR)) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u32 sub32_chk_mix(u32 a, u32 b, u8* oor) {
    u32 uResult = sub32(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u32 a_or_nr = (a|(~uResult)); u32 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign32(a_or_nr & b_or_r_neq_a); //'Error' == (SA|(~SR)) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u64 sub64_chk_mix(u64 a, u64 b, u8* oor) {
    u64 uResult = sub64(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_or_nr = (a|(~uResult)); u64 b_or_r_neq_a = (b|(uResult^a));
    *oor = sign64(a_or_nr & b_or_r_neq_a); //'Error' == (SA|(~SR)) & (SB|(SR^SA))
    return uResult;
}
static FORCE_INLINE u128 sub128_chk_mix(u128 a, u128 b, u8* oor) {
    u128 result = sub128(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_or_nr = (~a.tLegs[1]|(~result.tLegs[1])); u64 b_or_r_neq_a = (b.tLegs[1]|(result.tLegs[1]^a.tLegs[1]));
    *oor = sign64(a_or_nr & b_or_r_neq_a); //'Error' == (SA|(~SR)) & (SB|(SR^SA))
    return result;
}
static FORCE_INLINE u256 sub256_chk_mix(u256 a, u256 b, u8* oor) {
    u256 result = sub256(a, b);
    // sign(x) bitwiseop sign(y) == sign(x bitwiseop y)  =>
    u64 a_or_nr = (~a.tLegs[3]|(~result.tLegs[3])); u64 b_or_r_neq_a = (b.tLegs[3]|(result.tLegs[3]^a.tLegs[3]));
    *oor = sign64(a_or_nr & b_or_r_neq_a); //'Error' == (SA|(~SR)) & (SB|(SR^SA))
    return result;
}

// ********************************************
// Regular integral unsigned multiply (modulo)
// ********************************************
#define mulu8(a, b)   u8(u8(a) * u8(b))
#define mulu16(a, b)  u16(u16(a) * u16(b))
#define mulu32(a, b)  (u32(a) * u32(b))
#define mulu64(a, b)  (u64(a) * u64(b))

// ***************************************************
// Regular integral unsigned multiply (with high part)
// ***************************************************
static FORCE_INLINE u8 mulu8_with_high(u8 a, u8 b, u8* outH)
{
    u64 uResult = u64(a) * u64(b);
    *outH = u8(uResult >> 8);
    return u8(uResult);
}
static FORCE_INLINE u16 mulu16_with_high(u16 a, u16 b, u16* outH)
{
    u64 uResult = u64(a) * u64(b);
    *outH = u16(uResult >> 16);
    return u16(uResult);
}
static FORCE_INLINE u32 mulu32_with_high(u32 a, u32 b, u32* outH)
{
    return MulWithHigh32(a, b, outH);
}
static FORCE_INLINE u64 mulu64_with_high(u64 a, u64 b, u64* outH)
{
    return MulWithHigh64(a, b, outH);
}

// (a0.2^0 + a1.2^64) * (b0.2^0 + b1.2^64)
// == 2^0   * ( lo(a0*b0).2^0 ) +
//    2^64  * ( hi(a0*b0) + lo(a0.b1) + lo(a1.b0) ) +
// (clamped-out)
//    2^128 * ( hi(a0.b1) + hi(a1.b0) + lo(a1.b1) ) +
//    2^192 * ( hi(a1.b1) )
static FORCE_INLINE u128 mulu128(u128 a, u128 b) {
    u128 result;
    u64 highOf00;
    result.tLegs[0] = MulWithHigh64(a.tLegs[0], b.tLegs[0], &highOf00);
    u64 lowOf01 = mulu64(a.tLegs[0], b.tLegs[1]);
    u64 lowOf10 = mulu64(a.tLegs[1], b.tLegs[0]);
    result.tLegs[1] = lowOf01 + lowOf10 + highOf00;
    return result;
}

static FORCE_INLINE u128 mulu128_with_high(u128 a, u128 b, u128* outHigh) {
    u64 tLegs[4];
    naive_mul_legs_64_to(tLegs, a.tLegs, 2, b.tLegs, 2);
    outHigh->tLegs[0] = tLegs[2];
    outHigh->tLegs[1] = tLegs[3];
    return u128{ tLegs[0], tLegs[1] };
}

#define LO128(param256)     (*reinterpret_cast<u128*>(&(param256)))
#define HI128(param256)     (*reinterpret_cast<u128*>(&(param256) + 1))

static FORCE_INLINE u256 mulu256(u256 a, u256 b) {
    u256 result;
    naive_mul_legs_64_truncated_to(result.tLegs, 4, a.tLegs, 4, b.tLegs, 4);
    return result;
}

static FORCE_INLINE u256 mulu256_with_high(u256 a, u256 b, u256* outHigh) {
    u64 tLegs[8];
    naive_mul_legs_64_to(tLegs, a.tLegs, 4, b.tLegs, 4);
    outHigh->tLegs[0] = tLegs[4];
    outHigh->tLegs[1] = tLegs[5];
    outHigh->tLegs[2] = tLegs[6];
    outHigh->tLegs[3] = tLegs[7];
    return u256{ tLegs[0], tLegs[1], tLegs[2], tLegs[3] };
}

// *****************************************
// Regular integral signed multiply (modulo)
// *****************************************
#define muli8(a, b)      u8(i8(a) * i8(b))
#define muli16(a, b)     u16(i16(a) * i16(b))
#define muli32(a, b)     u32(i32(a) * i32(b))
#define muli64(a, b)     u64(i64(a) * i64(b))
static u128 muli128(u128 a, u128 b) {
    u8 negA = sign128(a);
    u8 negB = sign128(b);
    if (negA)
        a = neg128(a);
    if (negB)
        b = neg128(b);
    u128 result = mulu128(a, b);
    if (negA ^ negB)
        result = neg128(result);
    return result;
}
static u256 muli256(u256 a, u256 b) {
    u8 negA = sign256(a);
    u8 negB = sign256(b);
    if (negA)
        a = neg256(a);
    if (negB)
        b = neg256(b);
    u256 result = mulu256(a, b);
    if (negA ^ negB)
        result = neg256(result);
    return result;
}

// ******************************************************************
// Integral negation of a value constituted from a low + a high part
// ******************************************************************
static FORCE_INLINE u8 negtwo8(u8* ioLow, u8* ioHigh) {
    u8 carry;
    *ioLow =  AddCarry8(~(*ioLow), 1u, 0, &carry);
    *ioHigh = AddCarry8(~(*ioHigh), 0u, carry, &carry);
    return carry;
}
static FORCE_INLINE u8 negtwo16(u16* ioLow, u16* ioHigh) {
    u8 carry;
    *ioLow =  AddCarry16(~(*ioLow), 1u, 0, &carry);
    *ioHigh = AddCarry16(~(*ioHigh), 0u, carry, &carry);
    return carry;
}
static FORCE_INLINE u8 negtwo32(u32* ioLow, u32* ioHigh) {
    u8 carry;
    *ioLow =  AddCarry32(~(*ioLow), 1u, 0, &carry);
    *ioHigh = AddCarry32(~(*ioHigh), 0u, carry, &carry);
    return carry;
}
static FORCE_INLINE u8 negtwo64(u64* ioLow, u64* ioHigh) {
    u8 carry;
    *ioLow =  AddCarry64(~(*ioLow), 1uLL, 0, &carry);
    *ioHigh = AddCarry64(~(*ioHigh), 0uLL, carry, &carry);
    return carry;
}
static FORCE_INLINE u8 negtwo128(u128* ioLow, u128* ioHigh) {
    u8 carry;
    ioLow->tLegs[0] =  AddCarry64(~(ioLow->tLegs[0]),  1uLL, 0, &carry);
    ioLow->tLegs[1] =  AddCarry64(~(ioLow->tLegs[1]),  0uLL, carry, &carry);
    ioHigh->tLegs[0] = AddCarry64(~(ioHigh->tLegs[0]), 0uLL, carry, &carry);
    ioHigh->tLegs[1] = AddCarry64(~(ioHigh->tLegs[1]), 0uLL, carry, &carry);
    return carry;
}
static FORCE_INLINE u8 negtwo256(u256* ioLow, u256* ioHigh) {
    u8 carry;
    ioLow->tLegs[0] =  AddCarry64(~(ioLow->tLegs[0]),  1uLL, 0, &carry);
    ioLow->tLegs[1] =  AddCarry64(~(ioLow->tLegs[1]),  0uLL, carry, &carry);
    ioLow->tLegs[2] =  AddCarry64(~(ioLow->tLegs[2]),  0uLL, carry, &carry);
    ioLow->tLegs[3] =  AddCarry64(~(ioLow->tLegs[3]),  0uLL, carry, &carry);
    ioHigh->tLegs[0] = AddCarry64(~(ioHigh->tLegs[0]), 0uLL, carry, &carry);
    ioHigh->tLegs[1] = AddCarry64(~(ioHigh->tLegs[1]), 0uLL, carry, &carry);
    ioHigh->tLegs[2] = AddCarry64(~(ioHigh->tLegs[2]), 0uLL, carry, &carry);
    ioHigh->tLegs[3] = AddCarry64(~(ioHigh->tLegs[3]), 0uLL, carry, &carry);
    return carry;
}

// **************************************************
// Regular integral signed multiply (with high part)
// **************************************************
static FORCE_INLINE u8 muli8_with_high(u8 a, u8 b, u8* outH)
{
    i32 iResult = i32(i8(a)) * i32(i8(b));
    *outH = u8(iResult>>8);
    return u8(iResult);
}
static u16 muli16_with_high(u16 a, u16 b, u16* outH)
{
    i32 iResult = i32(i16(a)) * i32(i16(b));
    *outH = u16(iResult>>16);
    return u16(iResult);
}
static u32 muli32_with_high(u32 a, u32 b, u32* outH)
{
    return (u32)MulWithHigh32_Signed(i32(a), i32(b), reinterpret_cast<i32*>(outH));
}
static u64 muli64_with_high(u64 a, u64 b, u64* outH)
{
    return (u64)MulWithHigh64_Signed(i64(a), i64(b), reinterpret_cast<i64*>(outH));
}
static u128 muli128_with_high(u128 a, u128 b, u128* outH)
{
    u8 negA = sign128(a);
    u8 negB = sign128(b);
    if (negA)
        a = neg128(a);
    if (negB)
        b = neg128(b);
    u128 result = mulu128_with_high(a, b, outH);
    if (negA ^ negB)
        negtwo128(&result, outH);
    return result;
}
static u256 muli256_with_high(u256 a, u256 b, u256* outH)
{
    u8 negA = sign256(a);
    u8 negB = sign256(b);
    if (negA)
        a = neg256(a);
    if (negB)
        b = neg256(b);
    u256 result = mulu256_with_high(a, b, outH);
    if (negA ^ negB)
        negtwo256(&result, outH);
    return result;
}

// **************************************************
// Integral unsigned multiply with check for overflow
// **************************************************
static FORCE_INLINE u8 mulu8_ovf(u8 a, u8 b, u8* ovfOut)
{
    u8 outH;
    u8 uResult = mulu8_with_high(a, b, &outH);
    *ovfOut = outH ? 1 : 0;
    return uResult;
}
static FORCE_INLINE u16 mulu16_ovf(u16 a, u16 b, u8* ovfOut)
{
    u16 outH;
    u16 uResult = mulu16_with_high(a, b, &outH);
    *ovfOut = outH ? 1 : 0;
    return uResult;
}
static FORCE_INLINE u32 mulu32_ovf(u32 a, u32 b, u8* ovfOut)
{
    u32 outH;
    u32 uResult = mulu32_with_high(a, b, &outH);
    *ovfOut = outH ? 1 : 0;
    return uResult;
}
static FORCE_INLINE u64 mulu64_ovf(u64 a, u64 b, u8* ovfOut)
{
    u64 outH;
    u64 uResult = mulu64_with_high(a, b, &outH);
    *ovfOut = outH ? 1 : 0;
    return uResult;
}
static FORCE_INLINE u128 mulu128_ovf(u128 a, u128 b, u8* ovfOut)
{
    u128 outH;
    u128 result = mulu128_with_high(a, b, &outH);
    *ovfOut = (outH.tLegs[0] || outH.tLegs[1]) ? 1 : 0;
    return result;
}
static FORCE_INLINE u256 mulu256_ovf(u256 a, u256 b, u8* ovfOut)
{
    u256 outH;
    u256 result = mulu256_with_high(a, b, &outH);
    *ovfOut = (outH.tLegs[0] || outH.tLegs[1] || outH.tLegs[2] || outH.tLegs[3]) ? 1 : 0;
    return result;
}

// ************************************************
// Integral signed multiply with check for overflow
// ************************************************
static FORCE_INLINE u8 muli8_ovf(u8 a, u8 b, u8* ovfOut)
{
    u8 outH;
    u8 uResult = muli8_with_high(a, b, &outH);
    if (sign8_(uResult)) {
        *ovfOut = u8(~outH) ? 1 : 0;
    } else {
        *ovfOut = outH ? 1 : 0;
    }
    return uResult;
}
static FORCE_INLINE u16 muli16_ovf(u16 a, u16 b, u8* ovfOut)
{
    u16 outH;
    u16 uResult = muli16_with_high(a, b, &outH);
    if (sign16_(uResult)) {
        *ovfOut = u16(~outH) ? 1 : 0;
    } else {
        *ovfOut = outH ? 1 : 0;
    }
    return uResult;
}
static FORCE_INLINE u32 muli32_ovf(u32 a, u32 b, u8* ovfOut)
{
    u32 outH;
    u32 uResult = muli32_with_high(a, b, &outH);
    if (sign32_(uResult)) {
        *ovfOut = u32(~outH) ? 1 : 0;
    } else {
        *ovfOut = outH ? 1 : 0;
    }
    return uResult;
}
static FORCE_INLINE u64 muli64_ovf(u64 a, u64 b, u8* ovfOut)
{
    u64 outH;
    u64 uResult = muli64_with_high(a, b, &outH);
    if (sign64_(uResult)) {
        *ovfOut = u64(~outH) ? 1 : 0;
    } else {
        *ovfOut = outH ? 1 : 0;
    }
    return uResult;
}
static FORCE_INLINE u128 muli128_ovf(u128 a, u128 b, u8* ovfOut)
{
    u128 outH;
    u128 result = muli128_with_high(a, b, &outH);
    if (sign128_(result)) {
        *ovfOut = (~(outH.tLegs[0]) || ~(outH.tLegs[1])) ? 1 : 0;
    } else {
        *ovfOut = (outH.tLegs[0] || outH.tLegs[1]) ? 1 : 0;
    }
    return result;
}
static FORCE_INLINE u256 muli256_ovf(u256 a, u256 b, u8* ovfOut)
{
    u256 outH;
    u256 result = muli256_with_high(a, b, &outH);
    if (sign256_(result)) {
        *ovfOut = (~(outH.tLegs[0]) || ~(outH.tLegs[1]) || ~(outH.tLegs[2]) || ~(outH.tLegs[3])) ? 1 : 0;
    } else {
        *ovfOut = (outH.tLegs[0] || outH.tLegs[1] || outH.tLegs[2] || outH.tLegs[3]) ? 1 : 0;
    }
    return result;
}

#define divi8(a, b)      u8(i8(a)/i8(b))
#define divi16(a, b)     u16(i8(a)/i8(b))
#define divi32(a, b)     u32(i32(a)/i32(b))
#define divi64(a, b)     u64(i64(a)/i64(b))

#define divu8(a, b)      u8(u8(a)/u8(b))
#define divu16(a, b)     u16(u16(a)/u16(b))
#define divu32(a, b)     (u32(a)/u32(b))
#define divu64(a, b)     (u64(a)/u64(b))

#define remi8(a, b)      u8(i8(a)%i8(b))
#define remi16(a, b)     u16(i8(a)%i8(b))
#define remi32(a, b)     u32(i32(a)%i32(b))
#define remi64(a, b)     u64(i64(a)%i64(b))

#define remu8(a, b)      u8(u8(a)%u8(b))
#define remu16(a, b)     u16(u16(a)%u16(b))
#define remu32(a, b)     (u32(a)%u32(b))
#define remu64(a, b)     (u64(a)%u64(b))

static u128 _div_and_rem_u128_whenlong(u128 a, u128 b, u128* outRem)
{
//
// Base configuration for the long_div algorithm:
//
//    D&R     Div      Quo
//   +---+   +---+
//   |A.s|   |B.1|
//   +---+   +---+    +---+
//   |A.1|   |B.0| -> |Q.1|
//   +---+   +---+    +---+
//   |A.0|            |Q.0|
//   +---+            +---+
    int iQuotientCount = 2;     // => a base configuration of 2 quotient legs
    u64 uHighA = a.tLegs[1];
    if (uHighA == 0) {
        if (a.tLegs[0] == 0) {  // edge-case of dividend 0: shortcut everything and return 0
            *outRem = {};
            return u128{};
        } else {
            Assert(b.tLegs[1], "_div_and_rem_u128_whenlong() should not have been called with no high B and no low A");
            *outRem = a;        // ensured higher-than-dividend divisor => shortcut everything and return remainder a
            return u128{};
        }
        // (in a more general case, we may have remove a quotient leg if dividend starts lower)
    }

    int iMSLegB;         // index of highest leg of B (after left-justification shift)
    int iBitShift = left_justify_legs_64(b.tLegs, 2, &iMSLegB); // left-justify B so that the msb of its MSLeg is 1
    int iStartB = b.tLegs[0] ? 0 : 1;                           // index of lowest non-zero leg of B (after shift)
    int iCountB = iMSLegB - iStartB + 1;                        // count of significant legs of B (after shift)
    u64 tDividendAndRem[3];         // max req size = dividend size + 1
    u64 tQuotientLegs[3] = {};      // max req size = same as D&R when we can have trimmable high legs in B, down to 1 remaining
    
    // copy dividend to the 'dividend and remainder' buffer
    tDividendAndRem[0] = a.tLegs[0];            
    tDividendAndRem[1] = uHighA;            
    tDividendAndRem[2] = 0;         // zeroing the additional high leg
    // leftshift dividend representation (in the buffer) by same amount that we leftshifted B (may require 1 more leg => 3)
    u64 expect0 = left_shift_legs_64_bitonly_by(iBitShift, tDividendAndRem, 3);
    Assert_(0 == expect0);

    // From a base configuration of no offset for remainder, add one if trimmed low divisor
    int iRemOffset = iStartB;
    // From the base configuration quotient legs count, add one if trimmed high divisor, remove one if trimmed low divisor
    iQuotientCount += (1-iMSLegB) - iStartB;
    // We may remove a further quotient leg if shifted dividend has finally no bits in its additional high leg
    if (0 == tDividendAndRem[2])
        iQuotientCount--;

    // => We're now ready to call our long-division algorithm:
    Assert_(iQuotientCount > 0);
    long_div_leg64_to(tQuotientLegs, tDividendAndRem + iRemOffset, iQuotientCount, b.tLegs + iStartB, iCountB);
    // after that call:
    //   in tQuotientLegs is already the desired quotient (up to two legs).
    Assert_(tQuotientLegs[2] == 0);
    //   in tDividendAndRem, we should find the remainder of the shifted representation (up to two legs)
    Assert_(tDividendAndRem[2] == 0);
    Assert_(tDividendAndRem[1] < b.tLegs[1] || (tDividendAndRem[1] == b.tLegs[1] && tDividendAndRem[0] < b.tLegs[0]));
    //   => compensate its shift, by applying this time a right-shift on it:
    expect0 = right_shift_legs_64_bitonly_by(iBitShift, tDividendAndRem, 2);
    Assert_(0 == expect0);

    u128 quotientResult;
    quotientResult.tLegs[0] = tQuotientLegs[0];
    quotientResult.tLegs[1] = tQuotientLegs[1];

    outRem->tLegs[0] = tDividendAndRem[0];
    outRem->tLegs[1] = tDividendAndRem[1];

#if CHECK_ASSERTS
    expect0 = right_shift_legs_64_bitonly_by(iBitShift, b.tLegs, 2);
    Assert_(0 == expect0);
    u128 expectNoHigh;
    u128 checkLow = mulu128_with_high(b, quotientResult, &expectNoHigh);
    Assert_(expectNoHigh.tLegs[0] == 0 && expectNoHigh.tLegs[1] == 0);
    u8 expectNoCarry;
    u128 expectA = add128_car(checkLow, *outRem, 0, &expectNoCarry);
    Assert_(0 == expectNoCarry);
    Assert_(expectA.tLegs[0] == a.tLegs[0] && expectA.tLegs[1] == a.tLegs[1]);
#endif

    return quotientResult;
}

static FORCE_INLINE u128 divu128(u128 a, u128 b) {
    u128 unusedRem;
    if (!b.tLegs[1]) { // try optimization in a single call if b is single-legged
        u64 uDivisor = b.tLegs[0];
        Assert_(uDivisor);
        u64 uHighA = a.tLegs[1];
        if (uHighA < uDivisor) {
            u128 result;
            result.tLegs[0] = DivAndRemLarge64(uHighA, a.tLegs[0], uDivisor, unusedRem.tLegs);
            result.tLegs[1] = 0;
            return result;
        }
    } // Otherwise (and fallback) : call to our generic div-in-ring algorithm
    return _div_and_rem_u128_whenlong(a, b, &unusedRem);
}

static FORCE_INLINE u128 remu128(u128 a, u128 b) {
    if (!b.tLegs[1]) { // try optimization in a single call if b is single-legged
        u64 uDivisor = b.tLegs[0];
        Assert_(uDivisor);
        u64 uHighA = a.tLegs[1];
        if (uHighA < uDivisor) {
            u128 result;
            DivAndRemLarge64(uHighA, a.tLegs[0], uDivisor, result.tLegs);
            result.tLegs[1] = 0;
            return result;
        }
    } // Otherwise (and fallback) : call to generic div-in-ring algorithm
    u128 remResult;
    _div_and_rem_u128_whenlong(a, b, &remResult);
    return remResult;
}

static FORCE_INLINE u128 div_and_rem_u128(u128 a, u128 b, u128* outRem) {
    if (!b.tLegs[1]) { // try optimization in a single call if b is single-legged
        u64 uDivisor = b.tLegs[0];
        Assert_(uDivisor);
        u64 uHighA = a.tLegs[1];
        if (uHighA < uDivisor) {
            u128 result;
            result.tLegs[0] = DivAndRemLarge64(uHighA, a.tLegs[0], uDivisor, outRem->tLegs);
            result.tLegs[1] = 0;
            outRem->tLegs[1] = 0;
            return result;
        }
    } // Otherwise (and fallback) : call to generic div-in-ring algorithm
    return _div_and_rem_u128_whenlong(a, b, outRem);
}

static u256 div_and_rem_u256(u256 a, u256 b, u256* outRem)
{
//
// Base configuration for the long_div algorithm:
//
//    D&R     Div      Quo
//   +---+   +---+
//   |A.s|   |B.3|
//   +---+   +---+    
//   |A.3|   |B.2|    
//   +---+   +---+    
//   |A.2|   |B.1|    
//   +---+   +---+    +---+
//   |A.1|   |B.0| -> |Q.1|
//   +---+   +---+    +---+
//   |A.0|            |Q.0|
//   +---+            +---+
    int iQuotientCount = 2;     // => a base configuration of 2 quotient legs

    int iMSLegA = find_highest_significant_leg64(a.tLegs, 4);
    if (iMSLegA < 0) { // edge-case of dividend 0: shortcut everything and return 0
        *outRem = {};
        return u256{};
    }
    iQuotientCount -= (3-iMSLegA); // We may remove a quotient leg per trimmed-high dividend leg
    int iCountA = iMSLegA + 1;

    int iMSLegB;         // index of highest leg of B (after left-justification shift)
    int iBitShift = left_justify_legs_64(b.tLegs, 2, &iMSLegB); // left-justify B so that the msb of its MSLeg is 1
    if (iMSLegA < iMSLegB) { // ensured higher-than-dividend divisor => shortcut everything and return remainder a
        *outRem = a;        
        return u256{};
    }
    int iStartB = find_lowest_significant_leg64(b.tLegs, iMSLegB+1);     // index of lowest non-zero leg of B (after shift)
    int iCountB = iMSLegB - iStartB + 1;                    // count of significant legs of B (after shift)
    u64 tDividendAndRem[5] = {};    // max req size = dividend size + 1
    u64 tQuotientLegs[5] = {};      // max req size = same as D&R when we can have trimmable high legs in B, down to 1 remaining

    copy_legs_to(tDividendAndRem, a.tLegs, 4); // copy dividend to the 'dividend and remainder' buffer
    tDividendAndRem[4] = 0;                    // zeroing the additional high leg
    // leftshift dividend representation (in the buffer) by same amount that we leftshifted B (may require 1 more leg => 3)
    u64 expect0 = left_shift_legs_64_bitonly_by(iBitShift, tDividendAndRem, iCountA+1);
    Assert_(0 == expect0);

    // From a base configuration of no offset for remainder, add one if trimmed low divisor
    int iRemOffset = iStartB;
    // From the base configuration quotient legs count, add one if trimmed high divisor, remove one if trimmed low divisor
    iQuotientCount += (3-iMSLegB) - iStartB;
    // We may remove a further quotient leg if shifted dividend has finally no bits in its additional high leg
    if (0 == tDividendAndRem[iCountA])
        iQuotientCount--;

    // => We're now ready to call our long-division algorithm:
    long_div_leg64_to(tQuotientLegs, tDividendAndRem + iRemOffset, iQuotientCount, b.tLegs + iStartB, iCountB);
    // after that call:
    //   in tQuotientLegs is already the desired quotient (up to four legs).
    Assert_(tQuotientLegs[4] == 0);
    //   in tDividendAndRem, we should find the remainder of the shifted representation (up to iCountA legs)
    Assert_(tDividendAndRem[4] == 0);
    Assert_(lesser_than_legs64(tDividendAndRem, b.tLegs, 4));
    Assert_(tDividendAndRem[iCountA] == 0);
    Assert_(lesser_than_legs64(tDividendAndRem, b.tLegs, iCountA));
    //   => compensate its shift, by applying this time a right-shift on it:
    expect0 = right_shift_legs_64_bitonly_by(iBitShift, tDividendAndRem, iCountA);
    Assert_(0 == expect0);

    u256 quotientResult;
    quotientResult.tLegs[0] = tQuotientLegs[0];
    quotientResult.tLegs[1] = tQuotientLegs[1];
    quotientResult.tLegs[2] = tQuotientLegs[2];
    quotientResult.tLegs[3] = tQuotientLegs[3];

    outRem->tLegs[0] = tDividendAndRem[0];
    outRem->tLegs[1] = tDividendAndRem[1];
    outRem->tLegs[2] = tDividendAndRem[2];
    outRem->tLegs[3] = tDividendAndRem[3];

#if CHECK_ASSERTS
    expect0 = right_shift_legs_64_bitonly_by(iBitShift, b.tLegs, 4);
    Assert_(0 == expect0);
    u256 expectNoHigh;
    u256 checkLow = mulu256_with_high(b, quotientResult, &expectNoHigh);
    Assert_(expectNoHigh.tLegs[0] == 0 && expectNoHigh.tLegs[1] == 0 && expectNoHigh.tLegs[2] == 0 && expectNoHigh.tLegs[3] == 0);
    u8 expectNoCarry;
    u256 expectA = add256_car(checkLow, *outRem, 0, &expectNoCarry);
    Assert_(0 == expectNoCarry);
    Assert_(expectA.tLegs[0] == a.tLegs[0] && expectA.tLegs[1] == a.tLegs[1] && expectA.tLegs[2] == a.tLegs[2] && expectA.tLegs[3] == a.tLegs[3]);
#endif

    return quotientResult;
}

static FORCE_INLINE u256 divu256(u256 a, u256 b) {
    u256 unused;
    return div_and_rem_u256(a, b, &unused);
}

static FORCE_INLINE u256 remu256(u256 a, u256 b) {
    u256 result;
    return div_and_rem_u256(a, b, &result);
    return result;
}

static u128 divi128(u128 a, u128 b) {
    u8 negA = sign128(a);
    u8 negB = sign128(b);
    if (negA)
        a = neg128(a);
    if (negB)
        b = neg128(b);
    u128 result = divu128(a, b);
    if (negA ^ negB)
        result = neg128(result);
    return result;
}

static u128 remi128(u128 a, u128 b) {
    u8 negA = sign128(a);
    u8 negB = sign128(b);
    if (negA)
        a = neg128(a);
    if (negB)
        b = neg128(b);
    u128 result = remu128(a, b);
    if (negA)                       // -21 /  4 should result in quotient = -5 ; 4 * -5 = -20 => rem = -1
        result = neg128(result);    //  21 / -4 should also result in quotient = -5 ; but -4*-5 = 20 => rem = +1
    return result;                  // -21 / -4 should have a distinct remainder as 21/4, since quotient is same as = 5 ; but -4*5 = -20 => rem = -1
}

static u128 div_and_rem_i128(u128 a, u128 b, u128* outRem) {
    u8 negA = sign128(a);
    u8 negB = sign128(b);
    if (negA)
        a = neg128(a);
    if (negB)
        b = neg128(b);
    u128 result = div_and_rem_u128(a, b, outRem);
    if (negA ^ negB)
        result = neg128(result);
    if (negB)
        *outRem = neg128(*outRem);
    return result;
}

static u256 divi256(u256 a, u256 b) {
    u8 negA = sign256(a);
    u8 negB = sign256(b);
    if (negA)
        a = neg256(a);
    if (negB)
        b = neg256(b);
    u256 result = divu256(a, b);
    if (negA ^ negB)
        result = neg256(result);
    return result;
}

static u256 remi256(u256 a, u256 b) {
    u8 negA = sign256(a);
    u8 negB = sign256(b);
    if (negA)
        a = neg256(a);
    if (negB)
        b = neg256(b);
    u256 result = remu256(a, b);
    if (negA)                       // -21 /  4 should result in quotient = -5 ; 4 * -5 = -20 => rem = -1
        result = neg256(result);    //  21 / -4 should also result in quotient = -5 ; but -4*-5 = 20 => rem = +1
    return result;                  // -21 / -4 should have a distinct remainder as 21/4, since quotient is same as = 5 ; but -4*5 = -20 => rem = -1
}

static u256 div_and_rem_i256(u256 a, u256 b, u256* outRem) {
    u8 negA = sign256(a);
    u8 negB = sign256(b);
    if (negA)
        a = neg256(a);
    if (negB)
        b = neg256(b);
    u256 result = div_and_rem_u256(a, b, outRem);
    if (negA ^ negB)
        result = neg256(result);
    if (negB)
        *outRem = neg256(*outRem);
    return result;
}

static u8 mod8(u8 a, u8 b)
{
    u8 negA = sign8(a);
    u8 negB = sign8(b);
    if (negA)
        a = neg8(a);
    if (negB)
        b = neg8(b);
    u8 uResult = remu8(a, b);
    if (uResult) {
        if (negA ^ negB)                // -21 /  4 should result in remainder 3 => equals b - rem of unsigned
            uResult = neg8(uResult);    //  21 / -4 should result in remainder -3 => equals opposite of -21 / 4
        if (negB)                       // -21 / -4 should result in remainder -1 => equals opposite of 21 / 4
            uResult = neg8(uResult);
    }
    return uResult;
}

static u16 mod16(u16 a, u16 b)
{
    u8 negA = sign16(a);
    u8 negB = sign16(b);
    if (negA)
        a = neg16(a);
    if (negB)
        b = neg16(b);
    u16 uResult = remu16(a, b);
    if (uResult) {
        if (negA ^ negB)                // -21 /  4 should result in remainder 3 => equals b - rem of unsigned
            uResult = b - uResult;      //  21 / -4 should result in remainder -3 => equals opposite of -21 / 4
        if (negB)                       // -21 / -4 should result in remainder -1 => equals opposite of 21 / 4
            uResult = neg16(uResult);
    }
    return uResult;
}

static u32 mod32(u32 a, u32 b)
{
    u8 negA = sign32(a);
    u8 negB = sign32(b);
    if (negA)
        a = neg32(a);
    if (negB)
        b = neg32(b);
    u32 uResult = remu32(a, b);
    if (uResult) {
        if (negA ^ negB)
            uResult = b - uResult;
        if (negB)
            uResult = neg32(uResult);
    }
    return uResult;
}

static u64 mod64(u64 a, u64 b)
{
    u8 negA = sign64(a);
    u8 negB = sign64(b);
    if (negA)
        a = neg64(a);
    if (negB)
        b = neg64(b);
    u64 uResult = remu64(a, b);
    if (uResult) {
        if (negA ^ negB)
            uResult = b - uResult;
        if (negB)
            uResult = neg64(uResult);
    }
    return uResult;
}

static u128 mod128(u128 a, u128 b)
{
    u8 negA = sign128(a);
    u8 negB = sign128(b);
    if (negA)
        a = neg128(a);
    if (negB)
        b = neg128(b);
    u128 uResult = remu128(a, b);
    if (uResult.tLegs[0] || uResult.tLegs[1]) {
        if (negA ^ negB)
            uResult = sub128(b, uResult);
        if (negB)
            uResult = neg128(uResult);
    }
    return uResult;
}

static u256 mod256(u256 a, u256 b)
{
    u8 negA = sign256(a);
    u8 negB = sign256(b);
    if (negA)
        a = neg256(a);
    if (negB)
        b = neg256(b);
    u256 uResult = remu256(a, b);
    if (uResult.tLegs[0] || uResult.tLegs[1] || uResult.tLegs[2] || uResult.tLegs[3]) {
        if (negA ^ negB)
            uResult = sub256(b, uResult);
        if (negB)
            uResult = neg256(uResult);
    }
    return uResult;
}

static constexpr u8 tFPexpBits[5u] = {
    5,  // fp16 -  IEEE754 'half' - 5b exp
    8,  // fp32 -  IEEE754 'single' - 8b exp
    11, // fp64 -  IEEE754 'double' - 11b exp
    15, // fp128 - IEEE754 'quadruple' - 15b exp
    19, // fp256 - IEEE754 'octuple' - 19b exp
};
static constexpr u32 tFPallBits[5u] = {
    16u,
    32u,
    64u,
    128u,
    256u,
};
static constexpr u32 tFPLastLeg[5u] = {
    0u,
    0u,
    0u,
    1u,
    3u,
};
static FORCE_INLINE constexpr u8 fp_exp_bits(u8 uPosFpAbove16) {
    return tFPexpBits[uPosFpAbove16];
}
static FORCE_INLINE constexpr i32 fp_exponent_bias(u8 uPosFpAbove16) {
    return (1 << (fp_exp_bits(uPosFpAbove16)-1u)) - 1;
}
static FORCE_INLINE constexpr u32 fp_mantissa_bits_with_implicit(u8 uPosFpAbove16) {
    return tFPallBits[uPosFpAbove16] - fp_exp_bits(uPosFpAbove16);
}
static FORCE_INLINE constexpr u32 fp_mantissa_bits(u8 uPosFpAbove16) {
    return fp_mantissa_bits_with_implicit(uPosFpAbove16) - 1u;
}
static FORCE_INLINE constexpr u32 fp_rsh_exponent_mask(u8 uPosFpAbove16) {
    return (1u << fp_exp_bits(uPosFpAbove16)) - 1u;
}
static FORCE_INLINE constexpr u64 fp_inlastleg_exponent_mask(u8 uPosFpAbove16) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    return u64(fp_rsh_exponent_mask(uPosFpAbove16)) << (fp_mantissa_bits(uPosFpAbove16) - 64u*uLastLeg);
}
static FORCE_INLINE constexpr u32 fp_inlastleg_sign_pos(u8 uPosFpAbove16) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    return tFPallBits[uPosFpAbove16] - 1u - 64u*uLastLeg;
}
static FORCE_INLINE constexpr u64 fp_inlastleg_sign_mask(u8 uPosFpAbove16) {
    return 1uLL << fp_inlastleg_sign_pos(uPosFpAbove16);
}
static FORCE_INLINE constexpr u32 fp_inlastleg_mantissa_bits(u8 uPosFpAbove16) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    return fp_mantissa_bits(uPosFpAbove16) - 64u*uLastLeg;
}
static FORCE_INLINE constexpr u64 fp_inlastleg_qnan_mask(u8 uPosFpAbove16) {
    return 1uLL << (fp_inlastleg_mantissa_bits(uPosFpAbove16) - 1u);
}
static FORCE_INLINE constexpr u64 fp_inlastleg_mantissa_mask(u8 uPosFpAbove16) {
    return (1uLL << fp_inlastleg_mantissa_bits(uPosFpAbove16)) - 1uLL;
}

static constexpr u64 fp16_infinity[1u] = { fp_inlastleg_exponent_mask(0u) };
static constexpr u64 fp32_infinity[1u] = { fp_inlastleg_exponent_mask(1u) };
static constexpr u64 fp64_infinity[1u] = { fp_inlastleg_exponent_mask(2u) };
static constexpr u64 fp128_infinity[2u] = { 0uLL, fp_inlastleg_exponent_mask(3u) };
static constexpr u64 fp256_infinity[4u] = { 0uLL, 0uLL, 0uLL, fp_inlastleg_exponent_mask(3u) };

static constexpr u64 fp16_qnan[1u] = { fp_inlastleg_exponent_mask(0u)|fp_inlastleg_qnan_mask(0u) };
static constexpr u64 fp32_qnan[1u] = { fp_inlastleg_exponent_mask(1u)|fp_inlastleg_qnan_mask(0u) };
static constexpr u64 fp64_qnan[1u] = { fp_inlastleg_exponent_mask(2u)|fp_inlastleg_qnan_mask(0u) };
static constexpr u64 fp128_qnan[2u] = { 0uLL, fp_inlastleg_exponent_mask(3u)|fp_inlastleg_qnan_mask(0u) };
static constexpr u64 fp256_qnan[4u] = { 0uLL, 0uLL, 0uLL, fp_inlastleg_exponent_mask(3u)|fp_inlastleg_qnan_mask(0u) };

static constexpr u64 fp16_snan[1u] = { fp_inlastleg_exponent_mask(0u)|1uLL };
static constexpr u64 fp32_snan[1u] = { fp_inlastleg_exponent_mask(1u)|1uLL };
static constexpr u64 fp64_snan[1u] = { fp_inlastleg_exponent_mask(2u)|1uLL };
static constexpr u64 fp128_snan[2u] = { 1uLL, fp_inlastleg_exponent_mask(3u) };
static constexpr u64 fp256_snan[4u] = { 1uLL, 0uLL, 0uLL, fp_inlastleg_exponent_mask(3u) };

static constexpr u64 const* tFPStandardInfinity[5u] = {
    fp16_infinity,
    fp32_infinity,
    fp64_infinity,
    fp128_infinity,
    fp256_infinity,
};
static constexpr u64 const* tFPStandardQNan[5u] = {
    fp16_qnan,
    fp32_qnan,
    fp64_qnan,
    fp128_qnan,
    fp256_qnan,
};
static constexpr u64 const* tFPStandardSNan[5u] = {
    fp16_snan,
    fp32_snan,
    fp64_snan,
    fp128_snan,
    fp256_snan,
};

static FORCE_INLINE u64 fp_get_sign_rsh(u8 uPosFpAbove16, const u64* tLegs) {
    return tLegs[tFPLastLeg[uPosFpAbove16]] >> fp_inlastleg_sign_pos(uPosFpAbove16);
}

static FORCE_INLINE bool fp_is_finite(u8 uPosFpAbove16, const u64* tLegs) {
    u64 uExpMask = fp_inlastleg_exponent_mask(uPosFpAbove16);
    u64 uLastLegPayload = tLegs[tFPLastLeg[uPosFpAbove16]];
    return (uLastLegPayload & uExpMask) != uExpMask;
}

static FORCE_INLINE bool fp_is_infinity(u8 uPosFpAbove16, const u64* tLegs) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    if (!fp_is_finite(uPosFpAbove16, tLegs) && 0 == (tLegs[uLastLeg] & fp_inlastleg_mantissa_mask(uPosFpAbove16))) {
        for (u32 uLegBefore = 0; uLegBefore < uLastLeg; uLegBefore++) {
            if (tLegs[uLegBefore])
                return false;
        }
        return true;
    }
    return false;
}

static FORCE_INLINE bool fp_is_nan(u8 uPosFpAbove16, const u64* tLegs) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    if (!fp_is_finite(uPosFpAbove16, tLegs)) {
        if (0 != (tLegs[uLastLeg] & fp_inlastleg_mantissa_mask(uPosFpAbove16)))
            return true;
        for (u32 uLegBefore = 0; uLegBefore < uLastLeg; uLegBefore++) {
            if (tLegs[uLegBefore])
                return true;
        }
    }
    return false;
}

static FORCE_INLINE bool fp_is_qnan(u8 uPosFpAbove16, const u64* tLegs) {
    return !fp_is_finite(uPosFpAbove16, tLegs) && 0 != (tLegs[tFPLastLeg[uPosFpAbove16]] & fp_inlastleg_qnan_mask(uPosFpAbove16));
}

static FORCE_INLINE bool fp_is_zero(u8 uPosFpAbove16, const u64* tLegs) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    if (0 == (tLegs[uLastLeg] & ~fp_inlastleg_sign_mask(uPosFpAbove16))) {
        for (u32 uLegBefore = 0; uLegBefore < uLastLeg; uLegBefore++) {
            if (tLegs[uLegBefore])
                return false;
        }
        return true;
    }
    return false;
}

static FORCE_INLINE bool fp_is_subnormal_or_zero(u8 uPosFpAbove16, const u64* tLegs) {
    return 0 == (tLegs[tFPLastLeg[uPosFpAbove16]] & fp_inlastleg_exponent_mask(uPosFpAbove16));
}

static FORCE_INLINE u32 fp_get_biased_exponent(u8 uPosFpAbove16, const u64* tLegs) {
    return u32(tLegs[tFPLastLeg[uPosFpAbove16]] >> fp_inlastleg_mantissa_bits(uPosFpAbove16)) & fp_rsh_exponent_mask(uPosFpAbove16);
}
static FORCE_INLINE i32 fp_get_exponent(u8 uPosFpAbove16, const u64* tLegs) {
    return i32(fp_get_biased_exponent(uPosFpAbove16, tLegs)) + fp_exponent_bias(uPosFpAbove16);
}

static FORCE_INLINE void fp_copy_legs_to(u64* outLegs, u8 uPosFpAbove16, const u64* tLegs) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    for (u32 uLeg = 0; uLeg <= uLastLeg; uLeg++)
        outLegs[uLeg] = tLegs[uLeg];
}

template<u8 uPosFpAbove16>
static void fp_static_copy_legs_to(u64* outLegs, const u64* tLegs) {
    u32 uLastLeg = tFPLastLeg[uPosFpAbove16];
    for (u32 uLeg = 0; uLeg <= uLastLeg; uLeg++)
        outLegs[uLeg] = tLegs[uLeg];
}
; // template termination

// 'static' paths on the following use proper signed shifts, depending on the case ;
//          compiler doesn't know about those paths being static => opposite paths have negative shifts => unnecessary warnings are issued
#pragma warning(disable:4293) 

template<u8 uDestFpAbove16, u8 uSrcFpAbove16>
static void fp_static_upcast_to(u64* outLegs, const u64* tSrcLegs)
{
    Assert_(uDestFpAbove16 > uSrcFpAbove16);
    constexpr u32 uLastLeg = tFPLastLeg[uDestFpAbove16];
    if (!fp_is_finite(uSrcFpAbove16, tSrcLegs)) {
        if (fp_is_qnan(uSrcFpAbove16, tSrcLegs)) {
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardQNan[uDestFpAbove16]);
        } else if (fp_is_nan(uSrcFpAbove16, tSrcLegs)) {
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardSNan[uDestFpAbove16]);
        } else {
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardInfinity[uDestFpAbove16]);
        }
    } else if (fp_is_zero(uSrcFpAbove16, tSrcLegs)) {
        for (u32 uLeg = 0; uLeg <= uLastLeg; uLeg++)
            outLegs[uLeg] = 0uLL;
    } else {
        i32 iExponent = fp_get_exponent(uSrcFpAbove16, tSrcLegs);
        Assert_(iExponent <= fp_exponent_bias(uSrcFpAbove16));   // otherwise not finite
        Assert_(iExponent <= fp_exponent_bias(uDestFpAbove16));  // smaller source exponent range cannot be beyond larger source exponent max
        Assert_(iExponent > -fp_exponent_bias(uDestFpAbove16));  // smaller source exponent range cannot be below larger source exponent min

        if (iExponent > -fp_exponent_bias(uSrcFpAbove16)) { // nominal range on source
            u32 uBiasedExponentDest = u32(iExponent + fp_exponent_bias(uDestFpAbove16));
            outLegs[uLastLeg] = u64(uBiasedExponentDest) << fp_inlastleg_mantissa_bits(uDestFpAbove16);
            // Note: Mantissa bits in last leg
            // f16: 10 ; f32 : 23 ; f64 : 52 ; f128 : 48 ; f256 : 44
            if (fp_inlastleg_mantissa_bits(uDestFpAbove16) < fp_inlastleg_mantissa_bits(uSrcFpAbove16)) {
                // f64 -> f128+ ; f128 -> f256, 
                constexpr u32 uDiff = fp_inlastleg_mantissa_bits(uSrcFpAbove16) - fp_inlastleg_mantissa_bits(uDestFpAbove16);
                constexpr u32 uSrcLastLeg = tFPLastLeg[uSrcFpAbove16];
                constexpr u32 uLegDiff = uLastLeg - uSrcLastLeg;
                constexpr u32 uInvDiff = 64u - uDiff;
                Assert_(uLegDiff);
                for (u32 uLeg = 0u; uLeg < uLegDiff; uLeg++)
                    outLegs[uLeg] = 0uLL;
                for (u32 uLeg = uLegDiff; uLeg < uLastLeg; uLeg++) {
                    outLegs[uLeg] = (tSrcLegs[uLeg-uLegDiff] >> uDiff) | (tSrcLegs[uLeg+1u-uLegDiff] << uInvDiff);
                }
                outLegs[uLastLeg] |= (tSrcLegs[uSrcLastLeg] >> uDiff) & fp_inlastleg_mantissa_mask(uDestFpAbove16);
            } else {
                // f16 -> f32+ ; f32 -> f64+
                Assert_(tFPLastLeg[uSrcFpAbove16] == 0u);
                constexpr u32 uDiff = fp_inlastleg_mantissa_bits(uDestFpAbove16) - fp_inlastleg_mantissa_bits(uSrcFpAbove16);
                outLegs[uLastLeg] |= (tSrcLegs[0u] << uDiff) & fp_inlastleg_mantissa_mask(uDestFpAbove16);
                for (u32 uLeg = 0u; uLeg < uLastLeg; uLeg++)
                    outLegs[uLeg] = 0uLL;
            }

        } else {                                            // subnormal range on source
            // TODO
            Assert(false, "fp_static_upcast_to() : subnormal source not yet implemented");
        }
    }
    outLegs[uLastLeg] |= fp_get_sign_rsh(uSrcFpAbove16, tSrcLegs) << fp_inlastleg_sign_pos(uDestFpAbove16);
}

template<u8 uDestFpAbove16, u8 uSrcFpAbove16>
static void fp_static_downcast_to(u64* outLegs, const u64* tSrcLegs, u8* outLostPrec)
{
    Assert_(uDestFpAbove16 < uSrcFpAbove16);
    constexpr u32 uLastLeg = tFPLastLeg[uDestFpAbove16];
    *outLostPrec = 0u;
    if (!fp_is_finite(uSrcFpAbove16, tSrcLegs)) {
        if (fp_is_qnan(uSrcFpAbove16, tSrcLegs)) {
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardQNan[uDestFpAbove16]);
        } else if (fp_is_nan(uSrcFpAbove16, tSrcLegs)) {
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardSNan[uDestFpAbove16]);
        } else {
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardInfinity[uDestFpAbove16]);
        }
    } else if (fp_is_zero(uSrcFpAbove16, tSrcLegs)) {
        for (u32 uLeg = 0; uLeg <= uLastLeg; uLeg++)
            outLegs[uLeg] = 0uLL;
    } else {
        i32 iExponent = fp_get_exponent(uSrcFpAbove16, tSrcLegs);
        Assert_(iExponent <= fp_exponent_bias(uSrcFpAbove16)); // otherwise not finite

        if (iExponent <= fp_exponent_bias(uDestFpAbove16)) {           // can fit in dest ?

            if (iExponent > -fp_exponent_bias(uDestFpAbove16)) {       // can fit nominal in dest ?
                Assert_(iExponent > -fp_exponent_bias(uSrcFpAbove16)); // smaller dest exponent range cannot be below larger source exponent min
                u32 uBiasedExponentDest = u32(iExponent + fp_exponent_bias(uDestFpAbove16));
                outLegs[uLastLeg] = u64(uBiasedExponentDest) << fp_inlastleg_mantissa_bits(uDestFpAbove16);
                // Note: Mantissa bits in last leg
                // f16: 10 ; f32 : 23 ; f64 : 52 ; f128 : 48 ; f256 : 44
                if (fp_inlastleg_mantissa_bits(uDestFpAbove16) < fp_inlastleg_mantissa_bits(uSrcFpAbove16)) {
                    // f32+ -> f16 ; f64+ -> f32
                    Assert_(uLastLeg == 0u);
                    constexpr u32 uDiff = fp_inlastleg_mantissa_bits(uSrcFpAbove16) - fp_inlastleg_mantissa_bits(uDestFpAbove16);
                    constexpr u32 uSrcLastLeg = tFPLastLeg[uSrcFpAbove16];
                    outLegs[0] |= (tSrcLegs[uSrcLastLeg] >> uDiff) & fp_inlastleg_mantissa_mask(uDestFpAbove16);
                    constexpr u32 uInvDiff = 64u - uDiff;
                    if (tSrcLegs[uSrcLastLeg] << uInvDiff)
                        *outLostPrec = 1u;
                    else {
                        for (u32 uLeg = 0u; uLeg < uSrcLastLeg; uLeg++) {
                            if (tSrcLegs[uLeg]) {
                                *outLostPrec = 1u;
                                break;
                            }
                        }
                    }

                } else {
                    // f128+ -> f64 ; f256 -> f128
                    constexpr u32 uDiff = fp_inlastleg_mantissa_bits(uDestFpAbove16) - fp_inlastleg_mantissa_bits(uSrcFpAbove16);
                    constexpr u32 uSrcLastLeg = tFPLastLeg[uSrcFpAbove16];
                    constexpr u32 uLegDiff = uSrcLastLeg - uLastLeg;
                    constexpr u32 uInvDiff = 64u - uDiff;
                    Assert_(uLegDiff);
                    for (u32 uLeg = 0u; uLeg < uLastLeg; uLeg++)
                        outLegs[uLeg] = (tSrcLegs[uLeg+uLegDiff] << uDiff) | (tSrcLegs[uLeg+uLegDiff-1u] >> uInvDiff);
                    u64 uLastMant = (tSrcLegs[uLastLeg+uLegDiff] << uDiff) | (tSrcLegs[uLastLeg+uLegDiff-1u] >> uInvDiff);
                    outLegs[uLastLeg] |= uLastMant & fp_inlastleg_mantissa_mask(uDestFpAbove16);
                    if (tSrcLegs[uLegDiff-1u] << uDiff)
                        *outLostPrec = 1u;
                    else {
                        for (u32 uLeg = 0u; uLeg < uLegDiff-1u; uLeg++) {
                            if (tSrcLegs[uLeg]) {
                                *outLostPrec = 1u;
                                break;
                            }
                        }
                    }
                }

            } else if (iExponent >= -fp_exponent_bias(uDestFpAbove16) - fp_mantissa_bits(uDestFpAbove16)) { // subnormal range on dest
                Assert_(iExponent > -fp_exponent_bias(uSrcFpAbove16)); // smaller dest exponent range cannot be below larger source exponent min
                // TODO
                Assert(false, "fp_static_downcast_to() : subnormal dest not yet implemented");

            } else {
                // underflow dest => results in 'zero' as dest type (necessarily having 'lost' precision since source wasn't zero)
                *outLostPrec = 1u;
                for (u32 uLeg = 0; uLeg <= uLastLeg; uLeg++)
                    outLegs[uLeg] = 0uLL;
            }

        } else {
            // overflow dest => results in 'infinity' as dest type (necessarily having 'lost' precision since source wasn't zero)
            *outLostPrec = 1u;
            fp_static_copy_legs_to<uDestFpAbove16>(outLegs, tFPStandardInfinity[uDestFpAbove16]);
        }
    }
    outLegs[uLastLeg] |= fp_get_sign_rsh(uSrcFpAbove16, tSrcLegs) << fp_inlastleg_sign_pos(uDestFpAbove16);
}

#pragma warning(default:4293) 







