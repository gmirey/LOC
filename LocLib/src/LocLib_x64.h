#pragma once 

#ifndef LOCLIB_X64_H_
#define LOCLIB_X64_H_


#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_Cmd_API.h"

enum eRegCategory {

    REG_X64_HIGHBYTE_IN_WORD_BASE,

    REG_X64_AH = REG_X64_HIGHBYTE_IN_WORD_BASE,     // specifically targets bits 8..15 of the RAX register => mandatory 8b operand width, at id 100 ; often cannot be used in conjunction with a REX-prefixed op (since would target SPL instead)
    REG_X64_CH,     // specifically targets bits 8..15 of the RCX register => mandatory 8b operand width, at id 101 ; often cannot be used in conjunction with a REX-prefixed op (since would target BPL instead)
    REG_X64_DH,     // specifically targets bits 8..15 of the RDX register => mandatory 8b operand width, at id 110 ; often cannot be used in conjunction with a REX-prefixed op (since would target SIL instead)
    REG_X64_BH,     // specifically targets bits 8..15 of the RBX register => mandatory 8b operand width, at id 111 ; often cannot be used in conjunction with a REX-prefixed op (since would target DIL instead)

    REG_X64_HIGHBYTE_IN_WORD_END,
    REG_X64_LEGACY_X87_BASE = REG_X64_HIGHBYTE_IN_WORD_END,

    REG_X64_ST0 = REG_X64_LEGACY_X87_BASE,
    REG_X64_ST1,
    REG_X64_ST2,
    REG_X64_ST3,
    REG_X64_ST4,
    REG_X64_ST5,
    REG_X64_ST6,
    REG_X64_ST7,

    REG_X64_LEGACY_X87_END,
    REG_X64_LEGACY_MMX_BASE = REG_X64_LEGACY_X87_END,

    REG_X64_MM0 = REG_X64_LEGACY_MMX_BASE,
    REG_X64_MM1,
    REG_X64_MM2,
    REG_X64_MM3,
    REG_X64_MM4,
    REG_X64_MM5,
    REG_X64_MM6,
    REG_X64_MM7,

    REG_X64_LEGACY_MMX_END,
    REG_X64_STD_INTEGRAL_BASE = REG_X64_LEGACY_MMX_END,

    REG_X64_xAX = REG_X64_STD_INTEGRAL_BASE,    // represents the set of AL, AX, EAX, RAX registers, depending on operand width - common id 000
    REG_X64_xCX,    // represents the set of CL, CX, ECX, RCX registers, depending on operand width                             - common id 001
    REG_X64_xDX,    // represents the set of DL, DX, EDX, RDX registers, depending on operand width                             - common id 010
    REG_X64_xBX,    // represents the set of BL, BX, EBX, RBX registers, depending on operand width                             - common id 011

    REG_X64_xSP,    // represents the set of SPL, SP, ESP, RSP registers, depending on operand width - common id 100, unless 8b ; Note: bare REX prefix targets SPL at id 100
    REG_X64_xBP,    // represents the set of BPL, BP, EBP, RBP registers, depending on operand width - common id 101, unless 8b ; Note: bare REX prefix targets BPL at id 101
    REG_X64_xSI,    // represents the set of SIL, SI, ESI, RSI registers, depending on operand width - common id 110, unless 8b ; Note: bare REX prefix targets SIL at id 110
    REG_X64_xDI,    // represents the set of DIL, DI, EDI, RDI registers, depending on operand width - common id 111, unless 8b ; Note: bare REX prefix targets DIL at id 111

    REG_X64_STD_INTEGRAL_END,
    REG_X64_E64_INTEGRAL_BASE = REG_X64_STD_INTEGRAL_END,

    REG_X64_R8x = REG_X64_E64_INTEGRAL_BASE,    // represents the set of R8B, R8W, R8D, R8 registers, depending on operand width
    REG_X64_R9x,    // represents the set of R9B, R9W, R9D, R9 registers, depending on operand width
    REG_X64_R10x,   // represents the set of R10B, R10W, R10D, R10 registers, depending on operand width
    REG_X64_R11x,   // represents the set of R11B, R11W, R11D, R11 registers, depending on operand width
    REG_X64_R12x,   // represents the set of R12B, R12W, R12D, R12 registers, depending on operand width
    REG_X64_R13x,   // represents the set of R13B, R13W, R13D, R13 registers, depending on operand width
    REG_X64_R14x,   // represents the set of R14B, R14W, R14D, R14 registers, depending on operand width
    REG_X64_R15x,   // represents the set of R15B, R15W, R15D, R15 registers, depending on operand width

    REG_X64_E64_INTEGRAL_END,
    REG_X64_STD_xMM_BASE = REG_X64_E64_INTEGRAL_END,

    REG_X64_xMM0 = REG_X64_STD_xMM_BASE,   // represents the set of XMM0, YMM0, ZMM0 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM1,   // represents the set of XMM1, YMM1, ZMM1 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM2,   // represents the set of XMM2, YMM2, ZMM2 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM3,   // represents the set of XMM3, YMM3, ZMM3 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM4,   // represents the set of XMM4, YMM4, ZMM4 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM5,   // represents the set of XMM5, YMM5, ZMM5 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM6,   // represents the set of XMM6, YMM6, ZMM6 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM7,   // represents the set of XMM7, YMM7, ZMM7 registers - or the first slot of those when scalar - depending on operand width

    REG_X64_STD_xMM_END,
    REG_X64_AVX_xMM_BASE = REG_X64_STD_xMM_END,

    REG_X64_xMM8 = REG_X64_AVX_xMM_BASE,   // represents the set of XMM8, YMM8, ZMM8 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM9,   // represents the set of XMM9, YMM9, ZMM9 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM10,  // represents the set of XMM10, YMM10, ZMM10 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM11,  // represents the set of XMM11, YMM11, ZMM11 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM12,  // represents the set of XMM12, YMM12, ZMM12 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM13,  // represents the set of XMM13, YMM13, ZMM13 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM14,  // represents the set of XMM14, YMM14, ZMM14 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM15,  // represents the set of XMM15, YMM15, ZMM15 registers - or the first slot of those when scalar - depending on operand width

    REG_X64_AVX_xMM_END,
    REG_X64_AVX512_xMM_BASE = REG_X64_AVX_xMM_END,

    REG_X64_xMM16 = REG_X64_AVX512_xMM_BASE,   // represents the set of XMM16, YMM16, ZMM16 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM17,  // represents the set of XMM17, YMM17, ZMM17 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM18,  // represents the set of XMM18, YMM18, ZMM18 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM19,  // represents the set of XMM19, YMM19, ZMM19 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM20,  // represents the set of XMM20, YMM20, ZMM20 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM21,  // represents the set of XMM21, YMM21, ZMM21 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM22,  // represents the set of XMM22, YMM22, ZMM22 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM23,  // represents the set of XMM23, YMM23, ZMM23 registers - or the first slot of those when scalar - depending on operand width

    REG_X64_xMM24,  // represents the set of XMM24, YMM24, ZMM24 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM25,  // represents the set of XMM25, YMM25, ZMM25 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM26,  // represents the set of XMM26, YMM26, ZMM26 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM27,  // represents the set of XMM27, YMM27, ZMM27 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM28,  // represents the set of XMM28, YMM28, ZMM28 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM29,  // represents the set of XMM29, YMM29, ZMM29 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM30,  // represents the set of XMM30, YMM30, ZMM30 registers - or the first slot of those when scalar - depending on operand width
    REG_X64_xMM31,  // represents the set of XMM31, YMM31, ZMM31 registers - or the first slot of those when scalar - depending on operand width

    REG_X64_AVX512_xMM_END,

    REG_X64_SPECIAL_NONE,
};

enum eModRM : u8 {
    // Mod 00
    X64_MODRM_ADDR_NO_OFFSET_BASE = 0x00u,
    X64_MODRM_ADDR_IN_RAX = X64_MODRM_ADDR_NO_OFFSET_BASE,
    X64_MODRM_ADDR_IN_RCX,
    X64_MODRM_ADDR_IN_RDX,
    X64_MODRM_ADDR_IN_RBX,
    X64_MODRM_ADDR_BY_SIB,
    X64_MODRM_ADDR_DISP32_FROM_IP,
    X64_MODRM_ADDR_IN_RSI,
    X64_MODRM_ADDR_IN_RDI,

    // Mod 01
    X64_MODRM_ADDR_8b_OFFSET_BASE = 0x40u,
    X64_MODRM_ADDR_IN_RAX_AND_DISP8 = X64_MODRM_ADDR_8b_OFFSET_BASE,
    X64_MODRM_ADDR_IN_RCX_AND_DISP8,
    X64_MODRM_ADDR_IN_RDX_AND_DISP8,
    X64_MODRM_ADDR_IN_RBX_AND_DISP8,
    X64_MODRM_ADDR_BY_SIB_AND_DISP8,
    X64_MODRM_ADDR_IN_RBP_AND_DISP8,
    X64_MODRM_ADDR_IN_RSI_AND_DISP8,
    X64_MODRM_ADDR_IN_RDI_AND_DISP8,

    // Mod 10
    X64_MODRM_ADDR_32b_OFFSET_BASE = 0x80u,
    X64_MODRM_ADDR_IN_RAX_AND_DISP32 = X64_MODRM_ADDR_32b_OFFSET_BASE,
    X64_MODRM_ADDR_IN_RCX_AND_DISP32,
    X64_MODRM_ADDR_IN_RDX_AND_DISP32,
    X64_MODRM_ADDR_IN_RBX_AND_DISP32,
    X64_MODRM_ADDR_BY_SIB_AND_DISP32,
    X64_MODRM_ADDR_IN_RBP_AND_DISP32,
    X64_MODRM_ADDR_IN_RSI_AND_DISP32,
    X64_MODRM_ADDR_IN_RDI_AND_DISP32,

    // Mod 11
    X64_MODRM_DIRECT_REG_BASE = 0xC0u,
    X64_MODRM_DIRECT_RAX_OR_XMM0 = X64_MODRM_DIRECT_REG_BASE,
    X64_MODRM_DIRECT_RCX_OR_XMM1,
    X64_MODRM_DIRECT_RDX_OR_XMM2,
    X64_MODRM_DIRECT_RBX_OR_XMM3,
    X64_MODRM_DIRECT_RSP_OR_XMM4,
    X64_MODRM_DIRECT_RBP_OR_XMM5,
    X64_MODRM_DIRECT_RSI_OR_XMM6,
    X64_MODRM_DIRECT_RDI_OR_XMM7,

};

#define X64_MODRM_RMBITS_WHEN_REQ_SIB  u8(4u)

struct X64DestSrcOp {
    DECL_TRIVIAL_STRUCT_OPS(X64DestSrcOp);
    u8 uOpCodeDestRMandSrcReg_8b;
    u8 uOpCodeDestRMandSrcImm_8b;
    u8 uOpCodeDestRMandSrcSreg;
    u8 uOpCodeDestHCandSrcImm_8b;
    u8 _reserved0;
    u8 _reserved1;
    u8 _reserved2;
    u8 uModRMregInCaseDestRMSrcImm;
    u64 uFlags; 
};

struct X64SingleRMOp {
    DECL_TRIVIAL_STRUCT_OPS(X64SingleRMOp);
    u8 uOpCodeRM_8b;
    u8 uModRMreg;
    u16 _reserved0;
    u32 _reserved1;
    u64 uFlags; 
};

#define X64_OPCODE_MODIFIER_8b_TO_16_32_64b                 u8(0x01u)       // default modifier to OR with an opcode in order to go from 8b (base) to 32b (bare), or 16b (66h-prefix), or 64b (REX.W-prefix)
#define X64_OPCODE_MODIFIER_REVERT_DEST_SRC                 u8(0x02u)       // default modifier to OR with an opcode in order to revert source & dest, typically in the ModRM (base r/m<-r ; with this modifier : r<-r/m)
#define X64_OPCODE_DESTRM_IMM_MODIFIER_RESTRICT_8b          u8(0x02u)       // modifier to OR with an opcode with an immediate source in order to force it as a 8b, sign-extended immediate if operand size is greater.
#define X64_OPCODE_MODIFIER_WHENPLUS_8b_TO_16_32_64b        u8(0x08u)       // modifier to OR with an opcode in order to go from 8b (base) to 32b (bare), or 16b (66h-prefix), or 64b (REX.W-prefix) in case of Op being +RegCode

#define X64_OPCODE_PREFIX_16b                               u8(0x66u)       // prefix to prepend to default-32b opcodes in order to use them with 16b operands
#define X64_OPCODE_PREFIX_REX_W                             u8(0x48u)       // REX prefix to prepend to default-32b opcodes in order to use them with 32b operands. Can also be prefixed to opcodes working with 8b immediates.
#define X64_OPCODE_PREFIX_REX_R                             u8(0x44u)       // REX prefix to extend the ModRM:reg field to target registers 8..15
#define X64_OPCODE_PREFIX_REX_X                             u8(0x42u)       // REX prefix to extend the SIB index field to target registers 8..15
#define X64_OPCODE_PREFIX_REX_B                             u8(0x41u)       // REX prefix to extend the ModRM:r/m field to target registers 8..15 (or SIB base, or OpCode+)
#define X64_OPCODE_PREFIX_REX_ONLY                          u8(0x40u)       // Bare REX prefix, may change interpretation of AH, CH, DH, BH as reg of ModRM to SPL, BPL, SIL, SIL.

#define X64FLAG_CODE_NO_8b                                  0x01uLL
static_assert(u8(X64FLAG_CODE_NO_8b) == X64_OPCODE_MODIFIER_8b_TO_16_32_64b, "X64FLAG_CODE_NO_8b must equal opcode modifier 8b to 16/32/64b");
#define X64FLAG_CODE_NO_REVERT_RM                           0x02uLL
#define X64FLAG_CODE_REVERT_ONLY_RM                         0x04uLL

#define X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED                0x010uLL         // Indicates that this instruction has an opcode for the 'r/m <- imm' form ; in this case, the ModRM:reg field shall contain the 'uModRMregInCaseDestRMSrcImm', shifted at bitpos 3
#define X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32                  0x020uLL         // Indicates that in case the 'r/m <- imm' form is used, the width of immediates in case operand is 64b stays 32b (sign-extended)
#define X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED    0x040uLL         // Indicates that in case the 'r/m <- imm' form is used, we can OR the opcode with 'X64_OPCODE_DESTRM_IMM_MODIFIER_RESTRICT_8b' to only specify immediate on 8b whatever the operand width (sign-extended) 

#define X64FLAG_CODES_SREG_ALLOWED                          0x080uLL         // Indicates that this instruction has opcodes for the 'r/m <- sreg' and 'sreg <- r/m' forms

#define X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED             0x100uLL         // Indicates that this instruction has an opcode for the 'hcreg <- imm' form ; defaults to xAX as the hardcoded register. 
#define X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32               0x200uLL         // Indicates that in case the 'hcreg <- imm' form is used, the width of immediates in case operand is 64b stays 32b (sign-extended)
#define X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALL8R_PLUS          0x400uLL         // Indicates that in case the 'hcreg <- imm' form is used, the opcode can be or'ed with a reg code to another reg. Note, in this case, extension to 16b;32b;64b opcodes shall OR 0x08 instead of 0x02

#define X64FLAG_CODE_DEST_HCREG_SRC_RM_ALLOWED              0x800uLL         // Indicates that this instruction has an opcode for the 'hcreg <- r/m' form ; defaults to xAX as the hardcoded register. 

#define X64FLAG_CODE_USES_XMM                               0x01000uLL       // Indicates that this instruction works with fp or vector registers instead of scalar integrals


// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define X64_DEST_SRC_OP(name, uOpCodeDestRMandSrcReg_8b, uOpCodeDestRMandSrcImm_8b, uOpCodeDestRMandSrcSreg, uOpCodeDestHCandSrcImm_8b, \
//                                                      _reserved0, _reserved1, _reserved2, uModRMregInCaseDestRMSrcImm, uFlags)
#define X64_DEST_SRC_OP_EMITTER_ \
        \
    X64_DEST_SRC_OP(MOV, /* dest := src [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x88u,  0xC6u,  0x8Cu,  0xB0u,  0x00u,  0x00u,  0x00u,  (0x00u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|\
                    X64FLAG_CODES_SREG_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALL8R_PLUS) \
        \
    X64_DEST_SRC_OP(ADD, /* dest := dest + src [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x00u,  0x80u,  0x00u,  0x04u,  0x00u,  0x00u,  0x00u,  (0x00u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(ADC, /* dest := dest + src + carry-in [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x10u,  0x80u,  0x00u,  0x14u,  0x00u,  0x00u,  0x00u,  (0x02u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(SUB, /* dest := dest - src [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x28u,  0x80u,  0x00u,  0x2Cu,  0x00u,  0x00u,  0x00u,  (0x05u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(SBB, /* dest := dest - src - borrow-in [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x18u,  0x80u,  0x00u,  0x1Cu,  0x00u,  0x00u,  0x00u,  (0x03u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(IMUL, /* dest := dest * src [scalar-integral regs, signed semantics] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x0Fu,  0x00u,  0x00u,  0x00u,  0x00u,  0x00u,  0x00u,  0u, \
                    X64FLAG_CODE_NO_8b|X64FLAG_CODE_NO_REVERT_RM) \
        \
    X64_DEST_SRC_OP(CMP, /* compares "dest" and src by setting the appropriate flags as-if we did "SUB dest, src" [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x38u,  0x80u,  0x00u,  0x3Cu,  0x00u,  0x00u,  0x00u,  (0x07u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(LEA, /* computes the absolute address of src (r/m form only) and stores it to dest -- format is format of resulting address => 0x03u for 64b !! */ \
                 /* r<-rm (only) ! */ \
                    0x8Du,          0x00u,  0x00u,  0x00u,  0x00u,  0x00u,  0x00u,  0u, \
                    X64FLAG_CODE_NO_8b|X64FLAG_CODE_REVERT_ONLY_RM) \
        \
    X64_DEST_SRC_OP(AND, /* dest &= src [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x20u,  0x80u,  0x00u,  0x24u,  0x00u,  0x00u,  0x00u,  (0x04u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(OR, /* dest |= src [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x08u,  0x80u,  0x00u,  0x0Cu,  0x00u,  0x00u,  0x00u,  (0x01u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \
    X64_DEST_SRC_OP(XOR, /* dest ^= src [scalar-integral regs] */ \
                 /* rm<-r   rm<-im  rm<-sr  hc<-im  rsrvd0  rsrvd1  rsrvd2  modrmi  */ \
                    0x30u,  0x80u,  0x00u,  0x34u,  0x00u,  0x00u,  0x00u,  (0x06u<<3u), \
                    X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32|X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED|\
                    X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED|X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) \
        \


constexpr X64DestSrcOp x64_tDestSrcOps[] = {
    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    #define X64_DEST_SRC_OP(name, uOpCodeDestRMandSrcReg_8b, uOpCodeDestRMandSrcImm_8b, uOpCodeDestRMandSrcSreg, uOpCodeDestHCandSrcImm_8b, \
                                                      _reserved0, _reserved1, _reserved2, uModRMregInCaseDestRMSrcImm, uFlags) \
                            X64DestSrcOp{uOpCodeDestRMandSrcReg_8b, uOpCodeDestRMandSrcImm_8b, uOpCodeDestRMandSrcSreg, uOpCodeDestHCandSrcImm_8b, 0u, 0u, 0u, uModRMregInCaseDestRMSrcImm, uFlags}, 
        X64_DEST_SRC_OP_EMITTER_
    #undef X64_DEST_SRC_OP
};

enum EX64DestSrcOp {
    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    #define X64_DEST_SRC_OP(name, uOpCodeDestRMandSrcReg_8b, uOpCodeDestRMandSrcImm_8b, uOpCodeDestRMandSrcSreg, uOpCodeDestHCandSrcImm_8b, \
                                                      _reserved0, _reserved1, _reserved2, uModRMregInCaseDestRMSrcImm, uFlags) \
                             EX64_DEST_SRC_OP_ ## name, 
        X64_DEST_SRC_OP_EMITTER_
    #undef X64_DEST_SRC_OP
};

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
// use together with    #define X64_SINGLERM_OP(name, uOpCodeRM_8b, uModRMreg, uFlags)
#define X64_SINGLERM_OP_EMITTER_ \
        \
    X64_SINGLERM_OP(MUL, /* AX|xAX:xDX := xAX * src [scalar-integral, unsigned semantics] */ \
                 /* rm      rmreg       */ \
                    0xF6u,  (0x04u<<3u), \
                    0uLL) \
        \
    X64_SINGLERM_OP(IMUL, /* AX|xAX:xDX := xAX * src [scalar-integral, signed semantics] */ \
                 /* rm      rmreg       */ \
                    0xF6u,  (0x05u<<3u), \
                    0uLL) \
        \
    X64_SINGLERM_OP(NOT, /* dest := ~dest [scalar-integrals] */ \
                 /* rm      rmreg       */ \
                    0xF6u,  (0x02u<<3u), \
                    0uLL) \
        \
    X64_SINGLERM_OP(NEG, /* dest := -dest [scalar-integrals] */ \
                 /* rm      rmreg       */ \
                    0xF6u,  (0x03u<<3u), \
                    0uLL) \
        \
    X64_SINGLERM_OP(DIV, /* xAX := AX|xAX:xDX / src ; AH|xDX := AX|xAX:xDX % src [scalar-integrals, unsigned semantics] */ \
                 /* rm      rmreg       */ \
                    0xF6u,  (0x06u<<3u), \
                    0uLL) \
        \
    X64_SINGLERM_OP(IDIV, /* xAX := AX|xAX:xDX / src ; AH|xDX := AX|xAX:xDX % src [scalar-integrals, signed semantics] */ \
                 /* rm      rmreg       */ \
                    0xF6u,  (0x07u<<3u), \
                    0uLL) \


constexpr X64SingleRMOp x64_tSingleRMOps[] = {
    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    #define X64_SINGLERM_OP(name, uOpCodeRM_8b, uModRMreg, uFlags) \
                            X64SingleRMOp{uOpCodeRM_8b, uModRMreg, 0u, 0u, uFlags}, 
        X64_SINGLERM_OP_EMITTER_
    #undef X64_SINGLERM_OP
};

enum EX64SingleRMOp {
    // Macro-magic... cf "A Note on enums-and-structs synchronization" in LocLib_TokenizerEnums.h
    #define X64_SINGLERM_OP(name, uOpCodeRM_8b, uModRMreg, uFlags) \
                             EX64_SINGLERM_OP_ ## name, 
        X64_SINGLERM_OP_EMITTER_
    #undef X64_SINGLERM_OP
};

struct X64OperandRef {
    DECL_TRIVIAL_STRUCT_OPS(X64OperandRef);
    u32 uCodeBase;
    u32 uFlags;
    union {
        u64 uWhenImmediate;
        u64 uAdditionalPayload;
        struct {
            u32 uCodeIndex;
            i32 iOffset30MsbAndLog2Scale;
        };
    };
};

#define X64OPERAND_FLAG_IS_REG              0x01u // when operand is value in a register (register code in uCodeBase)
#define X64OPERAND_FLAG_IS_IMMEDIATE        0x02u // when operand is an immediate value
#define X64OPERAND_FLAG_ADDR_IS_IP_BASED    0x04u // when operand is at address specified by a single 32b displacement from Instruction pointer at next instruction
                                                  // in this case, uAdditionalPayload may carry additional info (such as PESection for winx64 backend)
#define X64OPERAND_FLAG_IP_BASED_IS_NYEP    0x08u // when operand is at address specified by a single 32b displacement from Instruction pointer at next instruction, to a not-yet emitted proc
                                                  // in this case, uAdditionalPayload contains full IR code for the not-yet-emitted proc

// when none of the above flags are set:
//   operand is at address computed by "reg[uCodeBase] + scale*reg[uCodeIndex] + iOffset"
//   where uCodeBase can be 0 (none) or one of standard (or R8+) integral registers,
//   and uCodeIndex can be 0 (none) or one of the standard (or R8+) integral registers (barring RSP).

local_func_inl X64OperandRef make_operand_reg(u32 uRegCategoryCode) {
    X64OperandRef result;
    result.uCodeBase = uRegCategoryCode;
    result.uWhenImmediate = 0;
    result.uFlags = X64OPERAND_FLAG_IS_REG;
    return result;
}
local_func_inl u32 reg_code_from_operand_reg(const X64OperandRef& opd) {
    Assert_(opd.uFlags & X64OPERAND_FLAG_IS_REG);
    return opd.uCodeBase;
}

local_func_inl X64OperandRef make_operand_immediate(i64 iValue64b) {
    X64OperandRef result;
    result.uCodeBase = 0;
    result.uWhenImmediate = u64(iValue64b);
    result.uFlags = X64OPERAND_FLAG_IS_IMMEDIATE;
    return result;
}
local_func_inl u64 immu64_from_operand_imm(const X64OperandRef& opd) {
    Assert_(opd.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE);
    return opd.uWhenImmediate;
}

local_func_inl X64OperandRef make_operand_offset_from_ip(i32 iDisplacementApartFromIp, u64 uAdditionalPayload) {
    X64OperandRef result;
    result.uCodeBase = u32(iDisplacementApartFromIp);
    result.uAdditionalPayload = uAdditionalPayload;
    result.uFlags = X64OPERAND_FLAG_ADDR_IS_IP_BASED;
    return result;
}
local_func_inl u32 base_offset32_from_operand_ip_based_address(const X64OperandRef& opd) {
    Assert_(opd.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED);
    return opd.uCodeBase;
}
local_func_inl u64 additional_payload_from_operand_ip_based_address(const X64OperandRef& opd) {
    Assert_(opd.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED);
    return opd.uAdditionalPayload;
}

local_func_inl X64OperandRef make_operand_at_address_in_reg(u32 uRegCategoryCode, i32 iConstOffset = 0) {
    X64OperandRef result;
    result.uCodeBase = uRegCategoryCode;
    result.uCodeIndex = 0;
    result.iOffset30MsbAndLog2Scale = iConstOffset << 2;
    result.uFlags = 0;
    return result;
}
local_func_inl X64OperandRef make_operand_at_address_in_reg_indexed(u32 uRegBaseCategoryCode, u32 uRegIndexCategoryCode, u32 uLog2OfIndexScale = 0, i32 iConstOffset = 0) {
    Assert_(uLog2OfIndexScale < 4u);
    X64OperandRef result;
    result.uCodeBase = uRegBaseCategoryCode;
    result.uCodeIndex = uRegIndexCategoryCode;
    result.iOffset30MsbAndLog2Scale = (iConstOffset << 2) | uLog2OfIndexScale;
    result.uFlags = 0;
    return result;
}
local_func_inl u32 base_regcode_from_operand_regular_mem(const X64OperandRef& opd) {
    Assert_(0u == (opd.uFlags & (X64OPERAND_FLAG_ADDR_IS_IP_BASED|X64OPERAND_FLAG_IS_REG|X64OPERAND_FLAG_IS_IMMEDIATE)));
    return opd.uCodeBase;
}
local_func_inl u32 index_regcode_from_operand_regular_mem(const X64OperandRef& opd) {
    Assert_(0u == (opd.uFlags & (X64OPERAND_FLAG_ADDR_IS_IP_BASED|X64OPERAND_FLAG_IS_REG|X64OPERAND_FLAG_IS_IMMEDIATE)));
    return opd.uCodeIndex;
}
local_func_inl i32 displacement_from_operand_regular_mem(const X64OperandRef& opd) {
    Assert_(0u == (opd.uFlags & (X64OPERAND_FLAG_ADDR_IS_IP_BASED|X64OPERAND_FLAG_IS_REG|X64OPERAND_FLAG_IS_IMMEDIATE)));
    return opd.iOffset30MsbAndLog2Scale >> 2u;
}
local_func_inl u8 log2scale_from_operand_regular_mem(const X64OperandRef& opd) {
    Assert_(0u == (opd.uFlags & (X64OPERAND_FLAG_ADDR_IS_IP_BASED|X64OPERAND_FLAG_IS_REG|X64OPERAND_FLAG_IS_IMMEDIATE)));
    return u8(opd.iOffset30MsbAndLog2Scale & 0x03u);
}
local_func_inl u8 inplace_sib_scale_from_operand_regular_mem(const X64OperandRef& opd) {
    Assert_(0u == (opd.uFlags & (X64OPERAND_FLAG_ADDR_IS_IP_BASED|X64OPERAND_FLAG_IS_REG|X64OPERAND_FLAG_IS_IMMEDIATE)));
    return u8(opd.iOffset30MsbAndLog2Scale << 6);
}

enum EX64JumpCodeRelative : u8 {

    EX64JUMPCODE_RELATIVE_JMP = 0xE9u,                      // unconditional jump

    EX64JUMPCODE_NONE = 0,                                  // represents unconditional *no-jump* (for our backend only) => to check before emission

    EX64JUMPCODE_RELATIVE_JO = 0x80u,                       // Overflow         (err-result of signed add or sub ; or muls)

    EX64JUMPCODE_RELATIVE_JNO = 0x81u,                      // No Overflow

    EX64JUMPCODE_RELATIVE_JB = 0x82u,                       // Below            (result of unsigned A<B ; also err-result of unsigned add or sub)
    EX64JUMPCODE_RELATIVE_JNAE = EX64JUMPCODE_RELATIVE_JB,

    EX64JUMPCODE_RELATIVE_JAE = 0x83u,                      // Above or Equal   (result of unsigned A>=B)
    EX64JUMPCODE_RELATIVE_JNB = EX64JUMPCODE_RELATIVE_JAE,

    EX64JUMPCODE_RELATIVE_JE = 0x84u,                       // Equal            (result of A==B)
    EX64JUMPCODE_RELATIVE_JZ = EX64JUMPCODE_RELATIVE_JE,

    EX64JUMPCODE_RELATIVE_JNE = 0x85u,                      // Not Equal        (result of A!=B)
    EX64JUMPCODE_RELATIVE_JNZ = EX64JUMPCODE_RELATIVE_JNE,

    EX64JUMPCODE_RELATIVE_JBE = 0x86u,                      // Below or Equal   (result of unsigned A<=B)
    EX64JUMPCODE_RELATIVE_JNA = EX64JUMPCODE_RELATIVE_JBE,

    EX64JUMPCODE_RELATIVE_JA = 0x87u,                       // Above            (result of unsigned A>B)
    EX64JUMPCODE_RELATIVE_JNBE = EX64JUMPCODE_RELATIVE_JA,

    EX64JUMPCODE_RELATIVE_JS = 0x88u,                       // Sign             (ie. is result negative)

    EX64JUMPCODE_RELATIVE_JNS = 0x89u,                      // No Sign          (ie. is result non-negative)

    EX64JUMPCODE_RELATIVE_JPE = 0x8Au,                      // Parity even
    EX64JUMPCODE_RELATIVE_JP = EX64JUMPCODE_RELATIVE_JPE,

    EX64JUMPCODE_RELATIVE_JPO = 0x8Bu,                      // Parity odd
    EX64JUMPCODE_RELATIVE_JNP = EX64JUMPCODE_RELATIVE_JPO,

    EX64JUMPCODE_RELATIVE_JL = 0x8Cu,                       // Less             (result of signed A<B)
    EX64JUMPCODE_RELATIVE_JNGE = EX64JUMPCODE_RELATIVE_JL,

    EX64JUMPCODE_RELATIVE_JGE = 0x8Du,                      // Greater or Equal (result of signed A>=B)
    EX64JUMPCODE_RELATIVE_JNL = EX64JUMPCODE_RELATIVE_JGE,

    EX64JUMPCODE_RELATIVE_JLE = 0x8Eu,                      // Less or Equal    (result of signed A<=B)
    EX64JUMPCODE_RELATIVE_JNG = EX64JUMPCODE_RELATIVE_JLE,

    EX64JUMPCODE_RELATIVE_JG = 0x8Fu,                       // Greater          (result of signed A>B)
    EX64JUMPCODE_RELATIVE_JNLE = EX64JUMPCODE_RELATIVE_JG,

};

local_func u8 x64_encode_jmp_relative(u8* tData, u32 uPosOfInstruction, u8* outBytesToOffset)
{
    tData[0] = EX64JUMPCODE_RELATIVE_JMP;
    u32 uPosOfNextInstruction = uPosOfInstruction + 5u;
    u32 uTmpOffset = 0u - uPosOfNextInstruction;
    memcpy(tData + 1u, &uTmpOffset, 4u);
    *outBytesToOffset = 1u;
    return 5u;
}

local_func u8 x64_encode_jcc_relative(EX64JumpCodeRelative eJumpCode, u8* tData, u32 uPosOfInstruction, u8* outBytesToOffset)
{
    Assert_((u8(eJumpCode) & 0xF0u) == 0x80u);
    tData[0] = 0x0Fu;
    tData[1] = eJumpCode;
    u32 uPosOfNextInstruction = uPosOfInstruction + 6u;
    u32 uTmpOffset = 0u - uPosOfNextInstruction;
    memcpy(tData + 2u, &uTmpOffset, 4u);
    *outBytesToOffset = 2u;
    return 6u;
}

#define HIGHER_THAN_SIGNED8b_MASK       0xFFFF'FFFF'FFFF'FF80uLL
#define HIGHER_THAN_SIGNED16b_MASK      0xFFFF'FFFF'FFFF'8000uLL
#define HIGHER_THAN_SIGNED32b_MASK      0xFFFF'FFFF'8000'0000uLL

local_func_inl bool can_encode_8b_signed(u64 uValue64b) {
    return (uValue64b & HIGHER_THAN_SIGNED8b_MASK) == 0uLL || (uValue64b & HIGHER_THAN_SIGNED8b_MASK) == HIGHER_THAN_SIGNED8b_MASK;
}
local_func_inl bool can_encode_16b_signed(u64 uValue64b) {
    return (uValue64b & HIGHER_THAN_SIGNED16b_MASK) == 0uLL || (uValue64b & HIGHER_THAN_SIGNED16b_MASK) == HIGHER_THAN_SIGNED16b_MASK;
}
local_func_inl bool can_encode_32b_signed(u64 uValue64b) {
    return (uValue64b & HIGHER_THAN_SIGNED32b_MASK) == 0uLL || (uValue64b & HIGHER_THAN_SIGNED32b_MASK) == HIGHER_THAN_SIGNED32b_MASK;
}

// outputs the encoding of the modRM+offset in case the r/m part is an offset from instruction pointer (of next instruction)
// inputs:
//      iDisplacementFromIp: the base displacement to the referenced thing, as-if also from 0 in code section ;
//      uStartPos: the position of the modrm byte in code section
//      uBytesAfterThat: the number of bytes, if any, remaining to encode after offset (in case there is, eg, an immediate coming before next instruction)
//      uReg3bInPlace: the 'reg' bits to OR with modRM
// outputs:
//      the encoding, as an array of bytes written from 'pOutByes' (assumed pointing to the desired position for the modrm byte itself), including possible SIB byte
//          and actual displacement from next instruction to the offset.
// Note: The offset will always be written 1 byte after the position of the modrm base itself and always span 4 bytes => this call will always write 5 bytes to 'pOutByes'
local_func void x64_encode_ip_based_modRM_and_offset(u32 uBaseDisplacement, u32 uStartPos, u8 uBytesAfterThat, u8 uReg3bInPlace, u8* pOutByes)
{
    pOutByes[0u] = X64_MODRM_ADDR_DISP32_FROM_IP | uReg3bInPlace;
    u32 uStartPosOfNextInstruction = 5u + uStartPos + uBytesAfterThat;
    u32 uOffsetToEncode = uBaseDisplacement - uStartPosOfNextInstruction;
    memcpy(pOutByes + 1u, &uOffsetToEncode, 4u);
}

local_func u8 x64_encode_non_ip_modRM_and_SIB(const X64OperandRef& rmOpd, u8 uReg3bInPlace, bool bIs8bOp, u8* pOutBytes, u8* ioREX, u8* ioDoesForbidREX)
{
    Assert_(0u == (rmOpd.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));     // r/m cannot be immediate, ofc...
    Assert_(0u == (rmOpd.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED)); // should have called 'x64_encode_ip_based_modRM_and_offset' instead
    if (rmOpd.uFlags & X64OPERAND_FLAG_IS_REG) {
        u32 uRegInRM = reg_code_from_operand_reg(rmOpd);
        u8 uRegInRM4b;
        if (uRegInRM >= REG_X64_HIGHBYTE_IN_WORD_BASE && uRegInRM < REG_X64_HIGHBYTE_IN_WORD_END) {
            uRegInRM4b = u8(uRegInRM - REG_X64_HIGHBYTE_IN_WORD_BASE) + 4u;
            *ioDoesForbidREX = 1u;
        } else if (uRegInRM >= REG_X64_STD_INTEGRAL_BASE && uRegInRM <= REG_X64_E64_INTEGRAL_END) {
            uRegInRM4b = u8(uRegInRM - REG_X64_STD_INTEGRAL_BASE);
            if(bIs8bOp && uRegInRM >= REG_X64_xSP && uRegInRM <= REG_X64_xDI)
                *ioREX |= X64_OPCODE_PREFIX_REX_ONLY;
        } else if (uRegInRM >= REG_X64_STD_xMM_BASE && uRegInRM <= REG_X64_AVX_xMM_END) {
            uRegInRM4b = u8(uRegInRM - REG_X64_STD_xMM_BASE);
        } else {
            Assert_(false);
        }
        if (uRegInRM4b > 7u)
            *ioREX |= X64_OPCODE_PREFIX_REX_B;
        pOutBytes[0u] = X64_MODRM_DIRECT_REG_BASE | (uRegInRM4b & 0x07u) | uReg3bInPlace;
        return 1u;

    } else {
        i32 iDisplacement32b = displacement_from_operand_regular_mem(rmOpd);
        u8 uReqDisplacementBytes = 0u;
        u8 uModRMMod = X64_MODRM_ADDR_NO_OFFSET_BASE;
        u8 uSIBCount = 0u;
        if (iDisplacement32b) {
            if (can_encode_8b_signed(u64(i64(iDisplacement32b)))) {
                uReqDisplacementBytes = 1u;
                uModRMMod = X64_MODRM_ADDR_8b_OFFSET_BASE;
            } else {
                Assert_(can_encode_32b_signed(u64(i64(iDisplacement32b))));
                uReqDisplacementBytes = 4u;
                uModRMMod = X64_MODRM_ADDR_32b_OFFSET_BASE;
            }
        }
        u8 uRMBaseCode = 0u;
        u8 uSIBBaseCode = 0u;
        u8 uSIBIndexCode = 4u; // by default: none
        static_assert(REG_X64_STD_INTEGRAL_BASE > 0u, "our REG-Code 0 should not be part of standard integral ranges for testing addressing base or index == none...");
        u32 uCodeBase = base_regcode_from_operand_regular_mem(rmOpd);
        u32 uCodeIndex = index_regcode_from_operand_regular_mem(rmOpd);
        if (uCodeBase) {
            Assert_(uCodeBase >= REG_X64_STD_INTEGRAL_BASE && uCodeBase < REG_X64_E64_INTEGRAL_END);
            uRMBaseCode = u8(uCodeBase - REG_X64_STD_INTEGRAL_BASE);
            if (uRMBaseCode > 7u)
                *ioREX |= X64_OPCODE_PREFIX_REX_B;
            uSIBBaseCode = uRMBaseCode;
            if (uCodeBase == REG_X64_xSP) // SP-slot is SIB-byte encoding => SP-based requires SIB-byte
                goto on_require_sib;
            // Otherwise fallthrough: if (uCodeIndex)

        } else { // Special encoding by SIB byte with no base.
            Assert_(uModRMMod == X64_MODRM_ADDR_NO_OFFSET_BASE);
            uSIBBaseCode = 5u;            // 'none'
            goto on_require_sib;          // No base => require SIB...
        }

        if (uCodeIndex) {
            Assert_(uCodeIndex >= REG_X64_STD_INTEGRAL_BASE && uCodeIndex < REG_X64_E64_INTEGRAL_END);
            Assert_(uCodeIndex != REG_X64_xSP); // Stack pointer cannot be used as index
            uSIBIndexCode = u8(uCodeIndex - REG_X64_STD_INTEGRAL_BASE);
            if (uSIBIndexCode > 7u)
                *ioREX |= X64_OPCODE_PREFIX_REX_X;
            if (uCodeBase == REG_X64_xBP && uReqDisplacementBytes == 0u) { // Cannot encode RBP as base within an SIB byte without an offset...
                Assert_(uSIBBaseCode == 5u);
                uModRMMod = X64_MODRM_ADDR_8b_OFFSET_BASE;
                uReqDisplacementBytes = 1u;                     // => we'll encode offset 0 on 8b
            }
            { on_require_sib:
                uSIBCount = 1u;
                pOutBytes[0u] = uModRMMod | X64_MODRM_RMBITS_WHEN_REQ_SIB | uReg3bInPlace;
                pOutBytes[1u] = inplace_sib_scale_from_operand_regular_mem(rmOpd) | ((uSIBIndexCode & 0x07u) << 3u) | (uSIBBaseCode & 0x07u);
                goto when_done;
            }
        }

        { on_no_sib:
            Assert_(uSIBCount == 0u);
            pOutBytes[0u] = uModRMMod | (uRMBaseCode & 0x07u) | uReg3bInPlace;
        }

        { when_done:
            if (uReqDisplacementBytes)
                memcpy(pOutBytes + 1u + uSIBCount, &iDisplacement32b, uReqDisplacementBytes);
            return 1u + uSIBCount + uReqDisplacementBytes;
        }
    }
}

/*
local_func u8 x64_encode_2operands_mem_and_reg(u8 uOpCode, const X64OperandRef& memOpd, u32 uRegCode, u8 uFormat, u8* pOutBytes,
                                               u32 uStartingOffset, u8* outPatchableOffsetMem)
{
    Assert_(0u == (memOpd.uFlags & X64OPERAND_FLAG_IS_REG));
    Assert_(0u == (memOpd.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    Assert(uFormat <= 0x03u, "x64_encode_2operands_mem_and_reg() : only scalar integral format implemented atm");
    Assert_(uRegCode < REG_X64_STD_xMM_BASE);
    u8 uPrefixCount = 0u;
    u8 uRexCount = 0u;
    constexpr u8 uOpAndModRMCount = 2u;
    i32 iDisplacement = memOpd.iOffset30MsbAndLog2Scale >> 2u;
    u8 uEncodedRegCode;
    if (uRegCode >= REG_X64_E64_INTEGRAL_BASE) {
        // TODO
        Assert(false, "x64_encode_2operands_mem_and_reg() : access to registers 8..15 not yet implemented");
    }
    if (uFormat) { // more than 8b
        uOpCode |= X64_OPCODE_MODIFIER_8b_TO_16_32_64b;
        Assert(uRegCode >= REG_X64_STD_INTEGRAL_BASE, "x64_encode_2operands_mem_and_reg() : xH registers cannot be used when operand size >8b");
        if (uFormat == 0x01u) { // 16b
            uPrefixCount = 1u;
            pOutBytes[0u] = X64_OPCODE_PREFIX_16b;
        } else if (uFormat == 0x03u) { // 64b
            uRexCount = 1u;
            pOutBytes[0u] = X64_OPCODE_PREFIX_REX_W;
        } else { Assert_(uFormat == 0x02u); // 32b
            // NOOP
        }
        uEncodedRegCode = u8(uRegCode - REG_X64_STD_INTEGRAL_BASE);
    } else { // 8b
        if (uRegCode < REG_X64_STD_INTEGRAL_BASE) {
            uEncodedRegCode = u8(uRegCode + 4u);
        } else {
            Assert(uRegCode < REG_X64_xSP || uRegCode > REG_X64_xDI, "x64_encode_2operands_mem_and_reg() : 8b access to SPL..SIL not yet implemented");
            uEncodedRegCode = u8(uRegCode - REG_X64_STD_INTEGRAL_BASE);
        }
    }
    u8 uRegCodeInRM = uEncodedRegCode << 3u;
    u8 uByteLengthToOp = uPrefixCount + uRexCount;
    pOutBytes[uByteLengthToOp] = uOpCode;

    u8 uResultingLength;
    if (memOpd.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
        Assert_(0u == (memOpd.uFlags & X64OPERAND_FLAG_ADDR_USE_INDEX));
        pOutBytes[uByteLengthToOp+1u] = X64_MODRM_ADDR_DISP32_FROM_IP | uRegCodeInRM;
        u8 uByteLengthToOffset = uByteLengthToOp + uOpAndModRMCount;
        uResultingLength = uByteLengthToOffset + 4u;
        u32 uFinalOffset = u32(iDisplacement) - (uStartingOffset + u32(uResultingLength));
        memcpy(pOutBytes + uByteLengthToOffset, &uFinalOffset, 4u);
        *outPatchableOffsetMem = uByteLengthToOffset;
    } else {
        *outPatchableOffsetMem = 0;
        Assert_(memOpd.uCodeBase >= REG_X64_STD_INTEGRAL_BASE);
        Assert_(memOpd.uCodeBase < REG_X64_STD_INTEGRAL_END);
        Assert_(memOpd.uCodeBase < REG_X64_STD_INTEGRAL_END, "x64_encode_2operands_mem_and_reg() : base at reg >= R8 not yet implemented");
        if (memOpd.uCodeBase == REG_X64_xSP) { // xSP code in modrm is reserved for SIB... => requires SIB no matter what

        } else {

        }
        Assert_(memOpd.uCodeBase != REG_X64_xSP); // xSP code in modrm is reserved for SIB...
        if (memOpd.uFlags & X64OPERAND_FLAG_ADDR_USE_INDEX) {
            Assert_(memOpd.uCodeIndex >= REG_X64_STD_INTEGRAL_BASE);
            Assert_(memOpd.uCodeIndex < REG_X64_STD_INTEGRAL_END);
            Assert_(memOpd.uCodeIndex < REG_X64_STD_INTEGRAL_END, "x64_encode_2operands_mem_and_reg() : index at reg >= R8 not yet implemented");
            u8 uBaseOfModRM = X64_MODRM_ADDR_BY_SIB;
            u8 uBytesForOffset = 0u;
            if (iDisplacement) {
                if (can_encode_8b_signed(u64(i64(iDisplacement)))) {
                    uBytesForOffset = 1u;
                    uBaseOfModRM = X64_MODRM_ADDR_BY_SIB_AND_DISP8;
                } else {
                    Assert_(can_encode_32b_signed(u64(i64(iDisplacement))));
                    uBytesForOffset = 4u;
                    uBaseOfModRM = X64_MODRM_ADDR_BY_SIB_AND_DISP32;
                }
            }
            // TODO
            uResultingLength = 0;
            Assert(false, "x64_encode_2operands_mem_and_reg() : indexed mem addressing not yet implemented");
        } else {
            u8 uBaseOfModRM = X64_MODRM_ADDR_IN_RAX;
            u8 uBytesForOffset = 0u;
            if (iOffsetAs64b) {
                if (can_encode_8b_signed(u64(iOffsetAs64b))) {
                    uBytesForOffset = 1u;
                    uBaseOfModRM = X64_MODRM_ADDR_IN_EAX_AND_DISP8;
                } else {
                    Assert_(can_encode_32b_signed(u64(iOffsetAs64b)));
                    uBytesForOffset = 4u;
                    uBaseOfModRM = X64_MODRM_ADDR_IN_EAX_AND_DISP32;
                }
            }
            uBaseOfModRM |= (memOpd.uCodeBase - REG_X64_STD_INTEGRAL_BASE);
            pOutBytes[uByteLengthToOp+1u] = uBaseOfModRM | uRegCodeInRM;
            if (uBytesForOffset)
                memcpy(pOutBytes[uByteLengthToOp+2u], (const u8*)&iOffsetAs64b, uBytesForOffset);
            uResultingLength = uByteLengthToOp + 2u + uBytesForOffset;
        }
    }

    return uResultingLength;
}

local_func u8 x64_encode_2operands_reg_immdata(u8 uOpCodeWith8bAlreadySortedOut, u8 uModRMregInCaseDestRMSrcImm, u32 uRegCode, const u8* immData, u8 uImmDataByteCount,
                                               u8 uFormat, u8* pOutBytes)
{
    u8 uNextPos = 0;
    u8 uModRM;
    Assert_(uRegCode < REG_X64_E64_INTEGRAL_END);
    Assert(uRegCode < REG_X64_STD_INTEGRAL_END, "x64_encode_2operands_reg_immdata() : regs at or beyond R8 not yet implemented"); // TODO: allow extended Regs
    if (uFormat) {
        Assert_(uFormat <= 0x03u);
        Assert_(uRegCode > REG_X64_STD_INTEGRAL_BASE);
        uModRM = X64_MODRM_DIRECT_RAX_OR_XMM0 | u8(uRegCode-REG_X64_STD_INTEGRAL_BASE) | u8(uModRMregInCaseDestRMSrcImm<<3u);
        if (uFormat == 0x03u) {
            pOutBytes[0u] = X64_OPCODE_PREFIX_REX_W;
            uNextPos = 1u;
        } else if (uFormat == 0x01u) {
            pOutBytes[0u] = X64_OPCODE_PREFIX_16b;
            uNextPos = 1u;
        }
    } else { // 8b
        if (uRegCode < REX_X64_HIGHBYTE_IN_WORD_END) {
            uModRM = X64_MODRM_DIRECT_RAX_OR_XMM0 | u8(uRegCode+4u) | u8(uModRMregInCaseDestRMSrcImm<<3u);
        } else {
            uModRM = X64_MODRM_DIRECT_RAX_OR_XMM0 | u8(uRegCode-REG_X64_STD_INTEGRAL_BASE) | u8(uModRMregInCaseDestRMSrcImm<<3u);
        }
    }
    pOutBytes[uNextPos++] = uOpCodeWith8bAlreadySortedOut;
    pOutBytes[uNextPos++] = uModRM;
    memcpy(pOutBytes + uNextPos, immData, uImmDataByteCount);
    return uNextPos + uImmDataByteCount;
}

local_func u8 x64_encode_2operands_reg_and_imm(u8 uOpCodeStd, u8 uOpCodeHC, u8 uModRMregInCaseDestRMSrcImm, u64 uOpFlags, u32 uRegCode, u64 uImmediate64b, u8 uFormat, u8* pOutBytes)
{
    if ((uOpFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED) && (uOpFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED)) {
        if (uFormat > 0x00u && can_encode_8b_signed(uImmediate64b)) {
            return x64_encode_2operands_reg_immdata(uOpCodeStd|X64_OPCODE_DESTRM_IMM_MODIFIER_RESTRICT_8b, uModRMregInCaseDestRMSrcImm, uRegCode,
                (const u8*)&uImmediate64b, 1u, uFormat, pOutBytes);
        }
    }

    if (uOpFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED && (uRegCode == REG_X64_xAX ||  // by default, hardcoded to xAX
            ((uRegCode < REG_X64_STD_INTEGRAL_END && (uOpFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALL8R_PLUS)) && // but for some ops, possibly hardcodable as all 8 std regs
                ((uRegCode < REG_X64_xSP) || (uFormat > 0x00u))))) { // ... with a provision for not targetting SPL..DIL in 8b
        // TODO
        Assert(false, "x64_encode_2operands_reg_and_imm() : not yet implemented when possibly HC");
        return 0;
    } else {
        Assert_(uFormat <= 0x03u);
        Assert_(uOpFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED);
        u8 uBytesCount = u8(1u << uFormat);
        if (uFormat == 0x03u && (uOpFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32))
            uBytesCount = 4u;
        Assert_(uBytesCount > 4u || can_encode_32b_signed(uImmediate64b));
        Assert_(uBytesCount > 2u || can_encode_16b_signed(uImmediate64b));
        Assert_(uBytesCount > 1u || can_encode_8b_signed(uImmediate64b));
        u8 uOpCodeWith8bAlreadySortedOut = uOpCodeStd;
        if (uFormat > 0x00u)
            uOpCodeWith8bAlreadySortedOut |= X64_OPCODE_MODIFIER_8b_TO_16_32_64b;
        return x64_encode_2operands_reg_immdata(uOpCodeWith8bAlreadySortedOut, uModRMregInCaseDestRMSrcImm, uRegCode,
                (const u8*)&uImmediate64b, uBytesCount, uFormat, pOutBytes);
    }
}

local_func u8 x64_encode_2operands_mem_and_imm(u8 uOpCode, u8 uModRMregInCaseDestRMSrcImm, u64 uOpFlags, const X64OperandRef& dest, u64 uImmediate64b, u8 uFormat, u8* pOutBytes,
                                               u32 uStartingOffset, u8* outPatchableOffsetMem)
{
    Assert(uOpFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED, "x64_encode_2operands_mem_and_imm() : not encodable for this op");
    // TODO
    Assert(false, "x64_encode_2operands_mem_and_imm() : not yet implemented");
    return 0;
}
*/

local_func u8 x64_when_reg_get_3b(u32 uRegCode, bool uIsOp8b, u8 uREXWhen3rdBitSet, u8* ioREXByte, u8* ioDoesForbidREX)
{
    u8 uReg4b;
    if (uRegCode >= REG_X64_HIGHBYTE_IN_WORD_BASE && uRegCode < REG_X64_HIGHBYTE_IN_WORD_END) {
        uReg4b = u8(uRegCode - REG_X64_HIGHBYTE_IN_WORD_BASE) + 4u;
        *ioDoesForbidREX = 1u;
    } else if (uRegCode >= REG_X64_STD_INTEGRAL_BASE && uRegCode <= REG_X64_E64_INTEGRAL_END) {
        uReg4b = u8(uRegCode - REG_X64_STD_INTEGRAL_BASE);
        if (uIsOp8b && uRegCode >= REG_X64_xSP && uRegCode <= REG_X64_xDI)
            *ioREXByte |= X64_OPCODE_PREFIX_REX_ONLY;
    } else if (uRegCode >= REG_X64_STD_xMM_BASE && uRegCode <= REG_X64_AVX_xMM_END) {
        uReg4b = u8(uRegCode - REG_X64_STD_xMM_BASE);
    } else {
        Assert_(false);
    }
    if (uReg4b > 7u) {
        *ioREXByte |= uREXWhen3rdBitSet;
        return uReg4b & 0x07u;
    } else
        return uReg4b;
}

local_func u8 x64_encode_ip_based_dest(u8 uOpCodeKnowingWhether8b, u8 uFormat, i32 iRawDisplacementFromIP, const u8* pImmData, u8 uImmByteCount, u8 uCurrentREX,
                                       u8 uRMReg3bInPlace, u8* pOutBytes, u32 uStartingOffset, u8* outPatchableOffsetSrc)
{
    u8 uOpAndPrefixCount = 0u;
    if (uFormat == 0x03u) { // 64b => REX.W
        pOutBytes[uOpAndPrefixCount++] = uCurrentREX | X64_OPCODE_PREFIX_REX_W;
    } else {
        if (uFormat == 0x01u) // 16b => 0x66 (before REX)
            pOutBytes[uOpAndPrefixCount++] = X64_OPCODE_PREFIX_16b;
        if (uCurrentREX)
            pOutBytes[uOpAndPrefixCount++] = uCurrentREX;
    }
    pOutBytes[uOpAndPrefixCount++] = uOpCodeKnowingWhether8b;

    x64_encode_ip_based_modRM_and_offset(iRawDisplacementFromIP, uStartingOffset+uOpAndPrefixCount, uImmByteCount,
                                         uRMReg3bInPlace, pOutBytes + uOpAndPrefixCount);
    u8 uCountToOffset = uOpAndPrefixCount+1u;
    *outPatchableOffsetSrc = uCountToOffset;

    u8 uTotalCount = uCountToOffset + 4u;
    if (uImmByteCount) {
        memcpy(pOutBytes + uTotalCount, pImmData, uImmByteCount);
        uTotalCount += uImmByteCount;
    }
    return uTotalCount;
}

local_func u8 x64_encode_non_ip_based_modrm_with_immediate(u8 uOpCodeKnowingWhether8b, u8 uFormat, const X64OperandRef& destRM, const u8* pImmData, u8 uImmByteCount,
                                                           u8 uRMReg3bInPlace, u8* pOutBytes)
{
    Assert_(0u == (destRM.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));
    Assert_(0u == (destRM.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED));
    
    u8 tRMAndSIBBytes[6u];
    u8 uREX = 0u;
    u8 uForbidREX = 0u;
    u8 uRMAndSIBCount = x64_encode_non_ip_modRM_and_SIB(destRM, uRMReg3bInPlace, uFormat == 0x00u, tRMAndSIBBytes, &uREX, &uForbidREX);
    
    u8 uOpAndPrefixCount = 0u;
    if (uFormat == 0x03u) { // 64b => REX.W
        Assert_(0u == uForbidREX);
        pOutBytes[uOpAndPrefixCount++] = uREX | X64_OPCODE_PREFIX_REX_W;
    } else {
        if (uFormat == 0x01u) // 16b => 0x66 (before REX)
            pOutBytes[uOpAndPrefixCount++] = X64_OPCODE_PREFIX_16b;
        if (uREX) {
            Assert_(0u == uForbidREX);
            pOutBytes[uOpAndPrefixCount++] = uREX;
        }
    }
    pOutBytes[uOpAndPrefixCount++] = uOpCodeKnowingWhether8b;

    memcpy(pOutBytes + uOpAndPrefixCount, tRMAndSIBBytes, uRMAndSIBCount);
    u8 uTotalCount = uOpAndPrefixCount + uRMAndSIBCount;

    memcpy(pOutBytes + uTotalCount, pImmData, uImmByteCount);
    uTotalCount += uImmByteCount;

    return uTotalCount;
}

local_func u8 x64_encode_dest_src_op_as_standard_modrm(u8 uOpWhen8bOrDefault, const X64OperandRef& rmOpd, u32 uRegCode, u8 uFormat, u8 uNo8b, u8* pOutBytes,
                                                       u32 uStartingOffset, u8* outPatchableOffsetRM)
{
    Assert_(uNo8b == 0u || uNo8b == X64_OPCODE_MODIFIER_8b_TO_16_32_64b);
    u8 uREX = 0u;
    u8 bForbidREX = 0u;
    bool bFormat8b = true;
    u8 uOpCode = uOpWhen8bOrDefault;
    if (uFormat) {
        uOpCode |= (X64_OPCODE_MODIFIER_8b_TO_16_32_64b ^ uNo8b);
        bFormat8b = false;
    } else {
        Assert_(0u == uNo8b);
    }
    u8 uReg3b = x64_when_reg_get_3b(uRegCode, bFormat8b, X64_OPCODE_PREFIX_REX_R, &uREX, &bForbidREX);
    u8 uRegInModRM = uReg3b << 3u;

    u8 uOpAndPrefixCount = 0u;
    if (rmOpd.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {

        if (uFormat == 0x03u) { // 64b => REX.W
            Assert_(0u == bForbidREX);
            pOutBytes[uOpAndPrefixCount++] = uREX | X64_OPCODE_PREFIX_REX_W;
        } else {
            if (uFormat == 0x01u) // 16b => 0x66 (before REX)
                pOutBytes[uOpAndPrefixCount++] = X64_OPCODE_PREFIX_16b;
            if (uREX) {
                Assert_(0u == bForbidREX);
                pOutBytes[uOpAndPrefixCount++] = uREX;
            }
        }
        pOutBytes[uOpAndPrefixCount++] = uOpCode;

        x64_encode_ip_based_modRM_and_offset(base_offset32_from_operand_ip_based_address(rmOpd), uStartingOffset+uOpAndPrefixCount, 0u,
                                             uRegInModRM, pOutBytes + uOpAndPrefixCount);
        *outPatchableOffsetRM = uOpAndPrefixCount + 1u;
        return uOpAndPrefixCount + 5u;

    } else {
        u8 tRMAndSIBBytes[6u];
        u8 uRMAndSIBCount = x64_encode_non_ip_modRM_and_SIB(rmOpd, uRegInModRM, bFormat8b, tRMAndSIBBytes, &uREX, &bForbidREX);
        if (uFormat == 0x03u) { // 64b => REX.W
            Assert_(0u == bForbidREX);
            pOutBytes[uOpAndPrefixCount++] = uREX | X64_OPCODE_PREFIX_REX_W;
        } else {
            if (uFormat == 0x01u) // 16b => 0x66 (before REX)
                pOutBytes[uOpAndPrefixCount++] = X64_OPCODE_PREFIX_16b;
            if (uREX) {
                Assert_(0u == bForbidREX);
                pOutBytes[uOpAndPrefixCount++] = uREX;
            }
        }
        pOutBytes[uOpAndPrefixCount++] = uOpCode;

        memcpy(pOutBytes + uOpAndPrefixCount, tRMAndSIBBytes, uRMAndSIBCount);

        *outPatchableOffsetRM = 0u;
        return uOpAndPrefixCount + uRMAndSIBCount;
    }
}

local_func u8 x64_encode_dest_src_op(EX64DestSrcOp opIndex, const X64OperandRef& dest, const X64OperandRef& src, u8 uFormat, u8* pOutBytes,
                                     u32 uStartingOffset, u8* outPatchableOffsetDest, u8* outPatchableOffsetSrc)
{
    const X64DestSrcOp& op = x64_tDestSrcOps[opIndex];
    if (0 == (uFormat & 0xF8u)) { // scalar integrals
        Assert(uFormat <= 0x03u, "x64_encode_dest_src_op() : max scalar integral format for this arch is 64b");
        Assert(0 == (op.uFlags & X64FLAG_CODE_USES_XMM), "x64_encode_dest_src_op() : cannot use scalar integral format with instruction working with vector registers");
        
        if (src.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE) {
            Assert_(0 == (op.uFlags & X64FLAG_CODE_REVERT_ONLY_RM));
            u64 uImm64 = immu64_from_operand_imm(src);
            *outPatchableOffsetSrc = 0u;
            if ((op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED) && (op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_8B_RESTRICT_ALLOWED)) {
                Assert_(0u == (op.uFlags & X64FLAG_CODE_NO_8b));
                if (uFormat > 0x00u && can_encode_8b_signed(uImm64)) {
                    u8 uOpCode = op.uOpCodeDestRMandSrcImm_8b | X64_OPCODE_DESTRM_IMM_MODIFIER_RESTRICT_8b | X64_OPCODE_MODIFIER_8b_TO_16_32_64b;
                    if (dest.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
                        return x64_encode_ip_based_dest(uOpCode, uFormat, base_offset32_from_operand_ip_based_address(dest),
                                                        (const u8*)&uImm64, 1u, 0u, op.uModRMregInCaseDestRMSrcImm,
                                                        pOutBytes, uStartingOffset, outPatchableOffsetDest);
                    } else {
                        *outPatchableOffsetDest = 0u;
                        return x64_encode_non_ip_based_modrm_with_immediate(uOpCode, uFormat, dest,
                                                                            (const u8*)&uImm64, 1u, op.uModRMregInCaseDestRMSrcImm,
                                                                            pOutBytes);
                    }
                }
            }
            u8 uImmBytes = 1u << uFormat;
            Assert_(uImmBytes > 4u || can_encode_32b_signed(uImm64));
            Assert_(uImmBytes > 2u || can_encode_16b_signed(uImm64));
            Assert_(uImmBytes > 1u || can_encode_8b_signed(uImm64));
            if ((dest.uFlags & X64OPERAND_FLAG_IS_REG) && (op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALLOWED)) {
                u32 uDestRegCode = reg_code_from_operand_reg(dest);
                if (uDestRegCode == REG_X64_xAX || (op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALL8R_PLUS)) {
                    if (uFormat == 0x03u) { // 64b is quite large for an immediates. Try to manage cases where it doesn't fit with op, or where we can encode shorter.
                        if (can_encode_32b_signed(uImm64)) {
                            if (op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) {
                                uImmBytes = 4u;                 // shrink immediate back to 32b if possible
                            } else if (uImm64 < 0x8000'0000uLL) { // immediate is positive => use the fact that we can move to eg, EAX to zero extend to RAX
                                uFormat = 0x02u;
                                uImmBytes = 4u;
                            } else if ((op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED) && (op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32)) {
                                goto try_encode_with_rm_imm;    // use r/m if r/m can shrink to 32b
                            }
                        } else if (op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_MAX32) {
                            goto try_encode_with_rm_imm;        // try to use r/m if not possible to encode 64b to hcreg
                        }
                    }
                    u8 uOpCode = op.uOpCodeDestHCandSrcImm_8b;
                    bool bFormat8b = true;
                    if (uFormat > 0x00u) {
                        uOpCode |= (op.uFlags & X64FLAG_CODE_DEST_HCREG_SRC_IMM_ALL8R_PLUS) ? X64_OPCODE_MODIFIER_WHENPLUS_8b_TO_16_32_64b :
                            (X64_OPCODE_MODIFIER_8b_TO_16_32_64b ^ u8(op.uFlags & X64FLAG_CODE_NO_8b));
                        bFormat8b = false;
                    } else {
                        Assert_(0uLL == (op.uFlags & X64FLAG_CODE_NO_8b));
                    }
                    u8 uREX = 0u;
                    u8 bForbidREX = 0u;
                    uOpCode |= x64_when_reg_get_3b(uDestRegCode, bFormat8b, X64_OPCODE_PREFIX_REX_B, &uREX, &bForbidREX);
                    u8 uTotalBytes = 0u;
                    if (uFormat == 0x03u) {
                        Assert_(0u == bForbidREX);
                        pOutBytes[uTotalBytes++] = uREX|X64_OPCODE_PREFIX_REX_W;
                    } else {
                        if (uFormat == 0x01u)
                            pOutBytes[uTotalBytes++] = X64_OPCODE_PREFIX_REX_W;
                        if (uREX) {
                            Assert_(0u == bForbidREX);
                            pOutBytes[uTotalBytes++] = uREX;
                        }
                    }
                    pOutBytes[uTotalBytes++] = uOpCode;
                    memcpy(pOutBytes + uTotalBytes, &uImm64, uImmBytes);
                    uTotalBytes += uImmBytes;
                    return uTotalBytes;
                }
            }

            { try_encode_with_rm_imm:
                Assert_(op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_ALLOWED);
                if (uFormat == 0x03u && (op.uFlags & X64FLAG_CODE_DEST_RM_SRC_IMM_MAX32)) {
                    Assert_(can_encode_32b_signed(uImm64));
                    uImmBytes = 4u;
                }
                u8 uOpCode = op.uOpCodeDestRMandSrcImm_8b;
                if (uFormat > 0x00u) {
                    uOpCode |= (X64_OPCODE_MODIFIER_8b_TO_16_32_64b ^ u8(op.uFlags & X64FLAG_CODE_NO_8b));
                } else {
                    Assert_(0uLL == (op.uFlags & X64FLAG_CODE_NO_8b));
                }
                if (dest.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {
                    return x64_encode_ip_based_dest(uOpCode, uFormat, base_offset32_from_operand_ip_based_address(dest),
                                                    (const u8*)&uImm64, uImmBytes, 0u, op.uModRMregInCaseDestRMSrcImm,
                                                    pOutBytes, uStartingOffset, outPatchableOffsetDest);
                } else {
                    *outPatchableOffsetDest = 0u;
                    return x64_encode_non_ip_based_modrm_with_immediate(uOpCode, uFormat, dest,
                                                                        (const u8*)&uImm64, uImmBytes, op.uModRMregInCaseDestRMSrcImm,
                                                                        pOutBytes);
                }
            }

        } else if (src.uFlags & X64OPERAND_FLAG_IS_REG) {
            Assert_(0 == (op.uFlags & X64FLAG_CODE_REVERT_ONLY_RM));
            // r/m <- r
            *outPatchableOffsetSrc = 0u;
            return x64_encode_dest_src_op_as_standard_modrm(op.uOpCodeDestRMandSrcReg_8b,
                                                            dest, reg_code_from_operand_reg(src), uFormat, u8(op.uFlags & X64FLAG_CODE_NO_8b),
                                                            pOutBytes, uStartingOffset, outPatchableOffsetDest);

        } else {
            Assert_(dest.uFlags & X64OPERAND_FLAG_IS_REG); // cannot do mem to mem...
            Assert_(0uLL == (src.uFlags & X64FLAG_CODE_NO_REVERT_RM));
            // r <- r/m => reverted from standard r/m <- r
            *outPatchableOffsetDest = 0u;
            u8 uOpCode = op.uOpCodeDestRMandSrcReg_8b;
            if (0 == (op.uFlags & X64FLAG_CODE_REVERT_ONLY_RM))
                uOpCode |= X64_OPCODE_MODIFIER_REVERT_DEST_SRC;
            return x64_encode_dest_src_op_as_standard_modrm(uOpCode,
                                                            src, reg_code_from_operand_reg(dest), uFormat, u8(op.uFlags & X64FLAG_CODE_NO_8b),
                                                            pOutBytes, uStartingOffset, outPatchableOffsetSrc);
        }

    } else {
        Assert(false, "x64_encode_dest_src_op() : non-scalar integral formats not yet implemented");
    }

    return 0;
}

local_func u8 x64_encode_singleRM_op(EX64SingleRMOp opIndex, const X64OperandRef& operand, u8 uFormat, u8* pOutBytes,
                                     u32 uStartingOffset, u8* outPatchableOffsetRM)
{
    const X64SingleRMOp& op = x64_tSingleRMOps[opIndex];
    if (0 == (uFormat & 0xF8u)) { // scalar integrals
        Assert(uFormat <= 0x03u, "x64_encode_singleRM_op() : max scalar integral format for this arch is 64b");
        Assert(0 == (op.uFlags & X64FLAG_CODE_USES_XMM), "x64_encode_singleRM_op() : cannot use scalar integral format with instruction working with vector registers");
    
        Assert_(0u == (operand.uFlags & X64OPERAND_FLAG_IS_IMMEDIATE));

        u8 uREX = 0u;
        u8 bForbidREX = 0u;
        bool bFormat8b = true;
        u8 uOpCode = op.uOpCodeRM_8b;
        if (uFormat) {
            uOpCode |= X64_OPCODE_MODIFIER_8b_TO_16_32_64b ^ u8(op.uFlags & X64FLAG_CODE_NO_8b);
            bFormat8b = false;
        } else {
            Assert_(0uLL == (op.uFlags & X64FLAG_CODE_NO_8b));
        }

        u8 uOpAndPrefixCount = 0u;
        if (operand.uFlags & X64OPERAND_FLAG_ADDR_IS_IP_BASED) {

            if (uFormat == 0x03u) { // 64b => REX.W
                Assert_(0u == bForbidREX);
                pOutBytes[uOpAndPrefixCount++] = uREX | X64_OPCODE_PREFIX_REX_W;
            } else {
                if (uFormat == 0x01u) // 16b => 0x66 (before REX)
                    pOutBytes[uOpAndPrefixCount++] = X64_OPCODE_PREFIX_16b;
                if (uREX) {
                    Assert_(0u == bForbidREX);
                    pOutBytes[uOpAndPrefixCount++] = uREX;
                }
            }
            pOutBytes[uOpAndPrefixCount++] = uOpCode;

            x64_encode_ip_based_modRM_and_offset(base_offset32_from_operand_ip_based_address(operand), uStartingOffset+uOpAndPrefixCount, 0u,
                                                 op.uModRMreg, pOutBytes + uOpAndPrefixCount);
            *outPatchableOffsetRM = uOpAndPrefixCount + 1u;
            return uOpAndPrefixCount + 5u;

        } else {
            u8 tRMAndSIBBytes[6u];
            u8 uRMAndSIBCount = x64_encode_non_ip_modRM_and_SIB(operand, op.uModRMreg, bFormat8b, tRMAndSIBBytes, &uREX, &bForbidREX);
            if (uFormat == 0x03u) { // 64b => REX.W
                Assert_(0u == bForbidREX);
                pOutBytes[uOpAndPrefixCount++] = uREX | X64_OPCODE_PREFIX_REX_W;
            } else {
                if (uFormat == 0x01u) // 16b => 0x66 (before REX)
                    pOutBytes[uOpAndPrefixCount++] = X64_OPCODE_PREFIX_16b;
                if (uREX) {
                    Assert_(0u == bForbidREX);
                    pOutBytes[uOpAndPrefixCount++] = uREX;
                }
            }
            pOutBytes[uOpAndPrefixCount++] = uOpCode;

            memcpy(pOutBytes + uOpAndPrefixCount, tRMAndSIBBytes, uRMAndSIBCount);

            *outPatchableOffsetRM = 0u;
            return uOpAndPrefixCount + uRMAndSIBCount;
        }
    } else {
        Assert(false, "x64_encode_singleRM_op() : non-scalar integral formats not yet implemented");
    }

    return 0;
}


#endif // LOCLIB_X64_H_
