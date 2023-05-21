#pragma once

#ifndef LOCLIB_TYPE_INFO_DECLS_H_
#define LOCLIB_TYPE_INFO_DECLS_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"

#include "LocLib_Cmd_API.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"

struct DisplayableCompilerEntity {
};
static const DisplayableCompilerEntity DCE_NONE = {};

/*
// predecl

// emits an error directly on compilation context, not tied to a specific node
void emit_ctx_error(CompilationContext* pEvalContext, u16 uErrCode,
    const char* formatMsg, DisplayableCompilerEntity param1 = DCE_NONE,
    DisplayableCompilerEntity param2 = DCE_NONE, DisplayableCompilerEntity param3 = DCE_NONE);
*/

enum ECoreType : u8 {
#define RESERVED_WORD_TYPE(name)                        ECORETYPE_ ## name,
#define RESERVED_WORD_TYPE_ALIAS(name)
#define RESERVED_WORD_VALUE(type, name)
#define RESERVED_WORD_SPECIAL_VALUE(type, name)
#define RESERVED_WORD(syntaxName, name, specialExpand)  specialExpand
    RESERVED_WORDS_EMITTER_
#undef RESERVED_WORD
#undef RESERVED_WORD_SPECIAL_VALUE
#undef RESERVED_WORD_VALUE
#undef RESERVED_WORD_TYPE_ALIAS
#undef RESERVED_WORD_TYPE

    ECORETYPE_COMPACT_STRING,
    ECORETYPE_COMPACT_OWNEDSTRING,
    
//    ECORETYPE_NOTYETTYPED_ADDR,

    ECORETYPE_BUILTIN,
};

constexpr char const* tCoreTypesStr[] = {
#define RESERVED_WORD_TYPE(name)                        #name,
#define RESERVED_WORD_TYPE_ALIAS(name)
#define RESERVED_WORD_VALUE(type, name)
#define RESERVED_WORD_SPECIAL_VALUE(type, name)
#define RESERVED_WORD(syntaxName, name, specialExpand)  specialExpand
    RESERVED_WORDS_EMITTER_
#undef RESERVED_WORD
#undef RESERVED_WORD_SPECIAL_VALUE
#undef RESERVED_WORD_VALUE
#undef RESERVED_WORD_TYPE_ALIAS
#undef RESERVED_WORD_TYPE

    "#compact_string",
    "#compact_ownedstring",

//    "#not_yet_typed_address",

    "#builtin"
};
static const size_t COUNT_ECORETYPES = sizeof(tCoreTypesStr) / sizeof(char*);
static_assert(COUNT_ECORETYPES == ECORETYPE_BUILTIN + 1u);

// pre-decls
struct TypeInfo_CoreBase;       // Core. Has ECoreType

struct TypeInfo_Integral;       // Core, either sign-agnostic, signed or unsigned. Has max. Has min iff signed. Includes compint, bool, codepoint, rawptr, and not yet typed address
struct TypeInfo_FloatingPoint;  // Core, always signed. Includes float literal and XFloat. Has mantissa bits, max and min exponent... precisions...
struct TypeInfo_HwVector;       // Core, always embeds full contents => Eq on contents. Special comptime-known overloads on other operators.
struct TypeInfo_OtherCore;      // Core, can have a backing structlike impl. can be string related (either compact or not, either with alloc or not)

struct TypeInfo_Array;          // User defined elt type, static or dynamic count... includes slices. No-eq overload (always ref eq).
struct TypeInfo_Set;            // User defined key type. Specialization for string keys.
struct TypeInfo_Map;            // User defined key type and value type. Specialization for string keys.
struct TypeInfo_DistinctAlias;  // User defined distinct alias of other type. Direct ptr to other type info.
struct TypeInfo_ProcLike;       // User defined signature of a proc (including foreigns). Const-Valued with handle to typechecked body.
            // Note that the equivalent of a c 'function-pointer' will here also be a pointer to a TypeInfo_ProcLike of that type,
            // and here also implicit-deref when used in invocation forms.
struct TypeInfo_Pointer;        // User defined typed pointer, including pointer to procs. Direct ptr to value type info.
struct TypeInfo_StructLike;     // User defined list of members. Allows empty structs. Includes union types ; May include struct views.
            // Note: a structlike may have comptime-only members... but *only if decl is flagged as such*
struct TypeInfo_Enum;           // User defined list of values.
struct PseudoTypeInfo_ProcLikeOverload;   // Special representation of an overloaded proc-like as the "type" bound to its identifier
struct PseudoTypeInfo_ProcLikePolymorph;  // Special representation of a polymorphic proc-like as the "type" bound to its identifier.
struct PseudoTypeInfo_StructPolymorph;    // Special representation of a polymorphic struct-like as the "type" bound to its identifier.
struct PseudoTypeInfo_EnumPolymorph;      // Special representation of a polymorphic enum-like as the "type" bound to its identifier.

enum ETypeKind {
    ETYPEKIND_INTEGRAL,             // Can cast TypeInfo to TypeInfo_Integral
    ETYPEKIND_FLOATINGPOINT,        // Can cast TypeInfo to TypeInfo_FloatingPoint
    ETYPEKIND_HWVECTOR,             // Can cast TypeInfo to TypeInfo_HwVector
    ETYPEKIND_OTHERCORE,            // Can cast TypeInfo to TypeInfo_OtherCore
    ETYPEKIND_ARRAY,                // Can cast TypeInfo to TypeInfo_Array
    ETYPEKIND_SET,                  // Can cast TypeInfo to TypeInfo_Set
    ETYPEKIND_MAP,                  // Can cast TypeInfo to TypeInfo_Map
    ETYPEKIND_DISTINCTALIAS,        // Can cast TypeInfo to TypeInfo_DistinctAlias
    ETYPEKIND_POINTER,              // Can cast TypeInfo to TypeInfo_Pointer
    ETYPEKIND_STRUCTLIKE,           // Can cast TypeInfo to TypeInfo_StructLike (includes 'union' types)
    ETYPEKIND_ENUM,                 // Can cast TypeInfo to TypeInfo_Enum
    ETYPEKIND_PROCLIKEBODY,         // Can cast TypeInfo to TypeInfo_ProcLikeBody
    ETYPEKIND_PROCLIKEOVERLOAD,     // Can cast TypeInfo to PseudoTypeInfo_ProcLikeOverload
    ETYPEKIND_PROCLIKEPOLYMORPH,    // Can cast TypeInfo to PseudoTypeInfo_ProcLikePolymorph
    ETYPEKIND_STRUCTPOLYMORPH,      // Can cast TypeInfo to PseudoTypeInfo_StructPolymorph
    ETYPEKIND_ENUMPOLYMORPH,        // Can cast TypeInfo to PseudoTypeInfo_EnumPolymorph

    COUNT_TYPE_KINDS,
};

enum EBuiltin {

    EBUILTIN_FOREIGN,
    EBUILTIN_FOREIGN_SOURCE,

    EBUILTIN_MEMCPY,
    EBUILTIN_REV_MEMCPY,
    EBUILTIN_ZEROMEM,
    EBUILTIN_MEMSET,
    EBUILTIN_MEMCMP,
    EBUILTIN_RDTSC,

    COUNT_BUILTINS,
};

#define UNICODE_CODEPOINT_MAX       0x0010'FFFFu

#define RESERVED_WORD_SPECIAL_VALUE_SPECIAL_TRANSMUTE_CAST  1uLL
#define RESERVED_WORD_SPECIAL_VALUE_SPECIAL_TRUNCATING_CAST 2uLL
#define RESERVED_WORD_SPECIAL_VALUE_SPECIAL_SATURATING_CAST 3uLL
#define RESERVED_WORD_SPECIAL_VALUE_SPECIAL_POINTER_CAST    4uLL

#define COMPOUNDFLAG_HAS_PADDING_wITHIN     0x40u     // For structs or unions having padding bytes within values (of importance for equality checks)
#define COMPOUNDFLAG_BODY_IN_ERROR          0x20u     // For any error during typechecking of the compound body
#define COMPOUNDFLAG_BODY_IN_ERROR_RUNTIME  0x10u     // For an error on a *runtime* member, preventing finalizeation of runtime footprint and offsets
#define COMPOUNDFLAG_HAS_OVERLAPS           0x08u     // For unions (or structs with 'using' union members), indicates the presence of overlapped definitions indeed. 

#define COMPOUNDTYPE_IS_COMPTIME_ONLY       0x80u     // For structs or unions having non-const members of a non-runtime type
#define COMPOUNDTYPE_IS_STRUCT              0x08u     // Indicates that the compound is in fact a plain struct
#define COMPOUNDTYPE_IS_ENUM                0x04u     // Indicates that the compound is in fact an enum
#define COMPOUNDTYPE_IS_UNION               0x02u     // Indicates that the compound is in fact a union
#define COMPOUNDTYPE_IS_VIEW                0x01u     // Indicates that the compound is in fact a struct *view*

// Base of all TypeInfo
__declspec(align(16))
struct TypeInfo {

    DECL_TRIVIAL_STRUCT_OPS(TypeInfo);

    // 'As Node' (including cross-referencing IR): allows nodes having that TypeInfo as a *value* to directly point to that without spawning an new NodeValue.
    // Will have, as 'knownValue' meta, a pointer to 'this' TypeInfo. May additionnally carry the IR of the associated RTTI declaration,
    // if was already spawned (it may not yet be, but can eventually be resolved whenever 'typeinfo' or 'typeid' are required in code).
    NodeValue asNode;

    // ETypeKind
    u8 uTypeKind;

    // for core: ECoreType ; for proclike: proc-kind ; for compounds: compound-type-flags ; for some others: type-specific
    u8 _coreType;

    // for integrals: 2LSB = ESignedness ; 0x80 if target-reg-size dependent 
    // for floating point: Exponent bit count
    // for string-related : see STRINGFLAG_*
    // for others core: type-specific
    u8 _coreFlags;
    
    // 3LSB: log2 of scalar format (=> 0x00 for bytes, 0x01 for u16, 0x02 for u32, 0x03 for u64...)
    // 1b :  is ensured floating-point (and if compound type: must be the same fp format overall)
    // 4MSB: log2 of max hardware vector count (=> 0x00 for scalar, 0x20 for x4..., 0x30 for x8...)
    // Note that this corresponds directly to our encoding for IR formats...
    u8 uIRFormat;

    // 4MSB: log2 of alignment (max 12 for 4096), then 28b for slot count (even if max 20b).
    // Note that 0 slots is possible for 'void' type, empty structlike, or zero-count-static arrays (marker for 'trailing')
    u32 uSlotsAndAlign;
};

// "only" up to 6M bytes, and incidentally 6M slots... this serves two purposes:
//  * we're ensured to always fit IR slot counts or byte counts in IR immediates
//  * we're ensured to be able to target any known offset within an IR declaration using the 24b (signed) offset in a NYKA value.
#define MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE    0x0060'0000u

local_func_inl ETypeKind get_type_kind(const TypeInfo* pType) { return ETypeKind(pType->uTypeKind); }
local_func_inl bool is_core_type(ETypeKind eKind) { return eKind <= ETYPEKIND_OTHERCORE; }
local_func_inl bool is_core_type(const TypeInfo* pType) { return is_core_type(get_type_kind(pType)); }

enum ESignedness {
    ESIGN_AGNOSTIC = 0,
    EUNSIGNED = 1,
    ESIGNED = 2,
};

struct TypeInfo_CoreBase : public TypeInfo {
    DECL_TRIVIAL_STRUCT_OPS(TypeInfo_CoreBase);
};

struct TypeInfo_Integral : public TypeInfo_CoreBase {
    DECL_TRIVIAL_STRUCT_OPS(TypeInfo_Integral);
    u64 uNykaOfMax;
    u64 uNykaOfMin;
};

struct TypeInfo_UserBase : public TypeInfo {
};

struct TypeInfo_DistinctAlias : public TypeInfo_UserBase {
    const TypeInfo* pAliasedType;
};
local_func_inl const TypeInfo* get_aliased_type(const TypeInfo_DistinctAlias* pDistinctType) { return pDistinctType->pAliasedType; }
local_func_inl void init_distinct_alias_type(TypeInfo_DistinctAlias* ioToInit, const TypeInfo* pAliasedType, CompilationContext* pEvalContext) {
    *ioToInit = {};
    ioToInit->uTypeKind = ETypeKind::ETYPEKIND_DISTINCTALIAS;
    ioToInit->uIRFormat = pAliasedType->uIRFormat;
    ioToInit->uSlotsAndAlign = pAliasedType->uSlotsAndAlign;
    ioToInit->pAliasedType = pAliasedType;
}

// predecls
struct TypeInfo_CompoundBase;
struct TCStatement;
struct TCDeclSourceBlock;
struct TCContext;

// 4x64b
struct TCCompoundRegistration {
    TypeInfo_CompoundBase* pCompoundType;
    TCStatement* pStatementWithSignature;
    TCDeclSourceBlock* pRootTcBlock;
    int iPrimaryIdentifier;
    u32 volatile uTCProgress;
    TmpSet<TCContext*> setWaitingConstOnly;
    TmpSet<TCContext*> setWaitingPossiblyRuntime;
};

struct TypeInfo_CompoundBase : public TypeInfo_UserBase {
    TCCompoundRegistration* pRegistration;
    u16 uRegistrationIndex;  // 64k max compound types per source file
    u16 uRuntimeMemberCount; // 64k max final runtime member count (only relevant for structs or unions)
    u32 uUnalignedByteSize;  // Compound base size, without align (only relevant for structs or unions)
};
local_func_inl bool is_compound_type(const TypeInfo* pTypeInfo) {
    ETypeKind eKind = get_type_kind(pTypeInfo);
    return (eKind == ETypeKind::ETYPEKIND_STRUCTLIKE || eKind == ETypeKind::ETYPEKIND_ENUM);
}

/*
struct EnumEntry {
    int iIdentifier;
    u32 uValueFlags;
    u64 _valuePayload;
};
*/
struct TypeInfo_Enum : public TypeInfo_CompoundBase {
    const TypeInfo_Integral* pBaseType;
    TmpMap<int, u32> mapAllMembers;
    TmpArray<ValueBinding*> vecAllMembers;
    TmpArray<const TypeInfo_Enum*> vecUsed;
    ValueBinding* pLastValue;
};
local_func_inl const TypeInfo* get_base_type(const TypeInfo_Enum* pEnumType) { return pEnumType->pBaseType; }

local_func_inl void init_enum_type_before_tc(TypeInfo_Enum* ioToInit, const TypeInfo_Integral* pBaseType, u8 uTypeFlags, u8 uStartingFlags, Arena arena) {
    *ioToInit = {};
    ioToInit->uTypeKind = ETypeKind::ETYPEKIND_ENUM;
    ioToInit->_coreType = uTypeFlags;
    ioToInit->_coreFlags = uStartingFlags;
    ioToInit->pBaseType = pBaseType;
    ioToInit->mapAllMembers.init(FireAndForgetArenaAlloc(arena));
    ioToInit->vecAllMembers.init(arena);
    ioToInit->vecUsed.init(arena);
    ioToInit->pLastValue = 0;
}

local_func_inl const TypeInfo* unalias_ext(const TypeInfo* pType, ETypeKind* outKind) {
    ETypeKind eKind = get_type_kind(pType);
    if (eKind == ETypeKind::ETYPEKIND_DISTINCTALIAS) {
        pType = ((const TypeInfo_DistinctAlias*)pType)->pAliasedType;
        eKind = get_type_kind(pType);
        Assert_(eKind != ETypeKind::ETYPEKIND_DISTINCTALIAS);
    } else if (eKind == ETypeKind::ETYPEKIND_ENUM) {
        pType = ((const TypeInfo_Enum*)pType)->pBaseType;
        Assert_(get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL);
        eKind = ETypeKind::ETYPEKIND_INTEGRAL;
    }
    *outKind = eKind;
    return pType;
}
local_func_inl const TypeInfo* unalias(const TypeInfo* pType) {
    ETypeKind unused;
    return unalias_ext(pType, &unused);
}

enum ECompoundTCProgress : u8 {
    ECOMPOUND_NOT_YET_STARTED = 0,
    ECOMPOUND_IN_PROGRESS,
    ECOMPOUND_DONE_RUNTIME,
    ECOMPOUND_DONE_ALL,
};

// 2x64b + TypeInfo
struct TypeInfo_StructLike : public TypeInfo_CompoundBase {
    TmpMap<int, u32> mapAllMembers;
    TmpArray<ValueBinding*> vecAllMembers;  // we'll salvage the 'uIR' member from the ValueHolder there for 'uOffsetFromStart'.
    TmpArray<const TypeInfo_StructLike*> vecIncluded;
};

local_func_inl bool is_structlike_type_footprint_available(const TypeInfo_StructLike* pStructType)
{
    bool bReady = pStructType->pRegistration->uTCProgress >= ECOMPOUND_DONE_RUNTIME;
    READ_FENCE();
    return bReady;
}

local_func_inl bool is_compound_type_full_typechecked(const TypeInfo_CompoundBase* pCompoundType)
{
    bool bReady = pCompoundType->pRegistration->uTCProgress == ECOMPOUND_DONE_ALL;
    READ_FENCE();
    return bReady;
}

local_func_inl void init_structlike_type_before_tc(TypeInfo_StructLike* ioToInit, u8 uTypeFlags, u8 uStartingFlags, Arena arena) {
    *ioToInit = {};
    ioToInit->uTypeKind = ETypeKind::ETYPEKIND_STRUCTLIKE;
    ioToInit->_coreType = uTypeFlags;
    ioToInit->_coreFlags = uStartingFlags;
    ioToInit->mapAllMembers.init(FireAndForgetArenaAlloc(arena));
    ioToInit->vecAllMembers.init(arena);
    ioToInit->vecIncluded.init(arena);
}

local_func_inl bool is_type_info_available(const TypeInfo* pType) {
    if (get_type_kind(pType) != ETypeKind::ETYPEKIND_STRUCTLIKE)
        return true;
    else
        return is_structlike_type_footprint_available((const TypeInfo_StructLike*)pType);
}

local_func_inl bool is_target_reg_size_dependent_(const TypeInfo* pType) {
    Assert_(get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    return 0 != (pType->_coreFlags & 0x80);
}
local_func_inl u8 get_log2_of_scalar_bytes_from_format(u8 uIrFormat) {
    return uIrFormat & 0x07;
}
local_func_inl u8 get_log2_of_vector_count_from_format(u8 uIrFormat) {
    return uIrFormat >> 4u;
}

local_func_inl u8 get_log2_of_scalar_bytes(const TypeInfo* pType) {
    Assert_(is_type_info_available(pType));
    return get_log2_of_scalar_bytes_from_format(pType->uIRFormat);
}
local_func_inl u8 get_log2_of_vector_count(const TypeInfo* pType) {
    Assert_(is_type_info_available(pType));
    return get_log2_of_vector_count_from_format(pType->uIRFormat);
}
local_func_inl u32 get_log2_of_align_bytes(const TypeInfo* pType) {
    Assert_(is_type_info_available(pType));
    return pType->uSlotsAndAlign >> 28;
}
local_func_inl u32 get_byte_count_of_scalar_elem(const TypeInfo* pType) { return 1u << get_log2_of_scalar_bytes(pType); }
local_func_inl u32 get_vector_elem_count(const TypeInfo* pType) { return 1u << get_log2_of_vector_count(pType); }
local_func_inl u32 get_byte_count_of_align(const TypeInfo* pType) { return 1u << get_log2_of_align_bytes(pType); }
local_func_inl u32 get_slots_count(const TypeInfo* pType) {
    Assert_(is_type_info_available(pType));
    return pType->uSlotsAndAlign & 0x0FFF'FFFFu;
}
local_func_inl bool is_ensured_fp_elem_type(const TypeInfo* pType) {
    Assert_(is_type_info_available(pType));
    return 0 != (pType->uIRFormat & 0x08u);
}
local_func_inl bool is_a_void_type(const TypeInfo* pType) { return get_slots_count(pType) == 0u; }
local_func_inl bool is_single_slot(const TypeInfo* pType) { return get_slots_count(pType) == 1u; }
local_func_inl bool has_scalar_slots(const TypeInfo* pType) { return get_log2_of_vector_count(pType) == 0u; }
local_func_inl bool is_simple_scalar(const TypeInfo* pType) {return has_scalar_slots(pType) && is_single_slot(pType); }
local_func_inl u8 get_ir_format(const TypeInfo* pType) {
    Assert_(is_type_info_available(pType));
    return pType->uIRFormat;
}

local_func_inl u32 get_log2_of_slot_size_from_format(u8 uIRFormat) {
    return get_log2_of_scalar_bytes_from_format(uIRFormat) + get_log2_of_vector_count_from_format(uIRFormat);
}
local_func_inl u32 get_log2_of_natural_align_from_format(u8 uIRFormat) {
    return _min(4u, get_log2_of_slot_size_from_format(uIRFormat)); // CLEANUP: currently max 16-bytes aligned for base formats
}

local_func u32 get_runtime_unaligned_size(const TypeInfo* pType) {
    ETypeKind eKind = get_type_kind(pType);
    if (eKind != ETypeKind::ETYPEKIND_STRUCTLIKE) {
        u32 uSlotCount = get_slots_count(pType);
        Assert_(uSlotCount <= MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE);
        u32 uLog2OfSlotSize = get_log2_of_slot_size_from_format(pType->uIRFormat);
        u32 uSizeOfSlot = 1u << uLog2OfSlotSize;
        Assert_(u64(uSlotCount)*u64(uSizeOfSlot) <= u64(MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE));
        return uSizeOfSlot*uSlotCount;
    } else {
        const TypeInfo_StructLike* pAsStructLike = (const TypeInfo_StructLike*)pType;
        Assert_(is_structlike_type_footprint_available(pAsStructLike));
        Assert_(pAsStructLike->uUnalignedByteSize <= MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE);
        return pAsStructLike->uUnalignedByteSize;
    }
}

local_func u32 get_runtime_sizeof_ext(const TypeInfo* pType, u32* outAlign, u32* outUnalignedSize) {
    u32 uUnalignedByteSize = get_runtime_unaligned_size(pType);
    u32 uMaskIfSizeZero = u32(i32(uUnalignedByteSize - 1u) >> 31);
    u32 uLog2OfAlign = get_log2_of_align_bytes(pType);
    Assert_(uLog2OfAlign <= 12u); // 4K max align for any type (common "ensured" OS page align)
    u32 uAlignment = 1u << uLog2OfAlign;
    u32 uMaskOfAlignment = uAlignment - 1u;
    u32 uAlignedSize = (((uUnalignedByteSize - 1u) | uMaskOfAlignment) + 1u) & ~uMaskIfSizeZero;
    Assert_(uAlignedSize <= MAX_SLOT_AND_BYTE_COUNT_OF_USER_TYPE); // < 1M max size for any type (incl. structs and static arrays)
    *outUnalignedSize = uUnalignedByteSize;
    *outAlign = uAlignment;
    return uAlignedSize;
}

local_func_inl u32 get_runtime_sizeof(const TypeInfo* pType, u32* outAlign) {
    u32 unused; return get_runtime_sizeof_ext(pType, outAlign, &unused);
}
local_func_inl u32 get_runtime_sizeof(const TypeInfo* pType) {
    u32 unused; return get_runtime_sizeof(pType, &unused);
}

local_func_inl ECoreType get_core_type(const TypeInfo_CoreBase* pTypeInfo) { return ECoreType(pTypeInfo->_coreType); }
local_func_inl ECoreType get_core_type_(const TypeInfo* pTypeInfo) { return ECoreType(pTypeInfo->_coreType); }

local_func_inl const TypeInfo* type_from_type_node(const NodeValue* pValue) {
    Assert_(pValue->pType);
    ETypeKind eTypeKind;
    const TypeInfo* pUnaliasedType = unalias_ext(pValue->pType, &eTypeKind);
    Assert_(eTypeKind == ETypeKind::ETYPEKIND_OTHERCORE);
    Assert_(get_core_type_(pUnaliasedType) == ECoreType::ECORETYPE_TYPE);
    Assert_(is_value_tc_only(pValue));
    return pValue->info.metaValue.knownValue.pType;
}

local_func_inl NodeValue* type_node_from_type(const TypeInfo* pType) {
    return const_cast<NodeValue*>(&(pType->asNode));
}

local_func_inl bool is_compint(const TypeInfo_CoreBase* pType) { return get_core_type(pType) == ECoreType::ECORETYPE_COMPINT; }
local_func_inl bool is_compint_(const TypeInfo* pType) {
    return get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL && get_core_type_(pType) == ECoreType::ECORETYPE_COMPINT;
}
local_func_inl bool is_special_reg_sized_on_compute(const TypeInfo_Integral* pType) {
    return get_core_type(pType) == ECoreType::ECORETYPE_INT || get_core_type(pType) == ECoreType::ECORETYPE_NAT;
}
local_func_inl ESignedness get_signedness(const TypeInfo_Integral* pType) { return ESignedness(pType->_coreFlags & 0x03); }
local_func_inl bool is_signed(const TypeInfo_Integral* pType) { return get_signedness(pType) == ESignedness::ESIGNED; }
local_func_inl bool is_unsigned(const TypeInfo_Integral* pType) { return get_signedness(pType) == ESignedness::EUNSIGNED; }
local_func_inl bool is_raw_integral(const TypeInfo_Integral* pType) { return get_signedness(pType) == ESignedness::ESIGN_AGNOSTIC; }
local_func_inl bool is_raw_integral_(const TypeInfo* pType) {
    Assert_(get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    return is_raw_integral((const TypeInfo_Integral*)pType);
}
local_func_inl bool is_signed_(const TypeInfo* pType) {
    Assert_(get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    return is_signed((const TypeInfo_Integral*)pType);
}
local_func_inl bool is_unsigned_(const TypeInfo* pType) {
    Assert_(get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL);
    return is_unsigned((const TypeInfo_Integral*)pType);
}

struct TypeInfo_FloatingPoint : public TypeInfo_CoreBase {
};

local_func_inl bool is_xfloat(const TypeInfo_CoreBase* pType) { return get_core_type(pType) == ECoreType::ECORETYPE_XFLOAT; }
local_func_inl bool is_xfloat_(const TypeInfo* pType) {
    return get_type_kind(pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT && get_core_type_(pType) == ECoreType::ECORETYPE_XFLOAT;
}
local_func_inl bool is_float_literal(const TypeInfo_CoreBase* pType) { return get_core_type(pType) == ECoreType::ECORETYPE_FLOAT_LIT; }
local_func_inl bool is_float_literal_(const TypeInfo* pType) {
    return get_type_kind(pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT && get_core_type_(pType) == ECoreType::ECORETYPE_FLOAT_LIT;
}

local_func_inl u8 get_exponent_bitcount(const TypeInfo_FloatingPoint* pType) { return pType->_coreFlags & 0x7Fu; }
local_func_inl u32 get_mantissa_bitcount_with_implicit(const TypeInfo_FloatingPoint* pType) {
    return (get_byte_count_of_scalar_elem(pType) << 3) - u32(get_exponent_bitcount(pType));
}
local_func_inl u32 get_mantissa_bitcount_impl(const TypeInfo_FloatingPoint* pType) {
    return get_mantissa_bitcount_with_implicit(pType) - 1u;
}
local_func_inl i32 get_exponent_bias(const TypeInfo_FloatingPoint* pType) { return (1 << (get_exponent_bitcount(pType)-1u)) - 1; }
local_func_inl i32 get_exponent_min_normal(const TypeInfo_FloatingPoint* pType) { return 1 - get_exponent_bias(pType); }
local_func_inl i32 get_exponent_max_finite(const TypeInfo_FloatingPoint* pType) { return get_exponent_bias(pType); }
local_func_inl i32 get_exponent_of_lsb_smallest_denorm(const TypeInfo_FloatingPoint* pType) {
    return get_exponent_min_normal(pType) - i32(get_mantissa_bitcount_with_implicit(pType));
}
local_func_inl i32 get_exponent_of_lsb_when_abs_one(const TypeInfo_FloatingPoint* pType) {
    return -i32(get_mantissa_bitcount_impl(pType));
}
local_func_inl i32 get_exponent_of_lsb_when_largest_exp(const TypeInfo_FloatingPoint* pType) {
    return get_exponent_max_finite(pType) - i32(get_mantissa_bitcount_impl(pType));
}

#define FP_EMBD64_STANDARD_SNAN         0x7FF'0'0000'0000'0001uLL
#define FP_EMBD64_STANDARD_QNAN         0x7FF'8'0000'0000'0001uLL
#define FP_EMBD64_POSITIVE_INFINITY     0x7FF'0'0000'0000'0000uLL
#define FP_EMBD64_NEGATIVE_INFINITY     0xFFF'0'0000'0000'0000uLL
#define FP_EMBD64_NEGATIVE_ZERO         0x800'0'0000'0000'0000uLL

// any NaN
local_func_inl constexpr bool is_embd_nan_fp(u64 uEmbedded) {
    return (uEmbedded & 0x7FF'0'0000'0000'0000uLL) == 0x7FF'0'0000'0000'0000uLL &&
           (uEmbedded & 0x000'F'FFFF'FFFF'FFFFuLL) != 0;
}
// any signaling NaN
local_func_inl constexpr bool is_embd_signaling_nan_fp(u64 uEmbedded) {
    return (uEmbedded & 0x7FF'8'0000'0000'0000uLL) == 0x7FF'0'0000'0000'0000uLL &&
           (uEmbedded & 0x000'F'FFFF'FFFF'FFFFuLL) != 0;
}
// any quiet NaN
local_func_inl constexpr bool is_embd_quiet_nan_fp(u64 uEmbedded) {
    return (uEmbedded & 0x7FF'8'0000'0000'0000uLL) == 0x7FF'8'0000'0000'0000uLL;
}
// + or - infinity
local_func_inl constexpr bool is_embd_infinite_fp(u64 uEmbedded) {
    return (uEmbedded & 0x7FF'F'FFFF'FFFF'FFFFuLL) == 0x7FF'0'0000'0000'0000uLL;
}
// +infinity
local_func_inl constexpr bool is_embd_pos_infinity_fp(u64 uEmbedded) {
    return uEmbedded == 0x7FF'0'0000'0000'0000uLL;
}
// -infinity
local_func_inl constexpr bool is_embd_neg_infinity_fp(u64 uEmbedded) {
    return uEmbedded == 0xFFF'0'0000'0000'0000uLL;
}
// + or- zero
local_func_inl constexpr bool is_embd_pos_or_neg_zero_fp(u64 uEmbedded) {
    return (uEmbedded & 0x7FF'F'FFFF'FFFF'FFFFuLL) == 0x000'0'0000'0000'0000uLL;
}
// +zero
local_func_inl constexpr bool is_embd_pos_zero_fp(u64 uEmbedded) {
    return uEmbedded == 0x000'0'0000'0000'0000uLL;
}
// -zero
local_func_inl constexpr bool is_embd_neg_zero_fp(u64 uEmbedded) {
    return uEmbedded == 0x800'0'0000'0000'0000uLL;
}
// + or- one
local_func_inl constexpr bool is_embd_pos_or_neg_one_fp(u64 uEmbedded) {
    return (uEmbedded & 0x7FF'F'FFFF'FFFF'FFFFuLL) == 0x3FF'0'0000'0000'0000uLL;
}
// +one
local_func_inl constexpr bool is_embd_pos_one_fp(u64 uEmbedded) {
    return uEmbedded == 0x3FF'0'0000'0000'0000uLL;
}
// -one
local_func_inl constexpr bool is_embd_neg_one_fp(u64 uEmbedded) {
    return uEmbedded == 0xBFF'0'0000'0000'0000uLL;
}

// NaN FP *always* get embedded as double whatever the actual footprint. (LOC does *not* handle NaN payloads - or sign, for that matter)
local_func_inl constexpr bool is_nan_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_nan_fp(constValue.uEmbeddedValue);
}
// NaN FP *always* get embedded as double whatever the footprint. (LOC does *not* handle NaN payloads - or sign, for that matter)
local_func_inl constexpr bool is_signaling_nan_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_signaling_nan_fp(constValue.uEmbeddedValue);
}
// NaN FP *always* get embedded as double whatever the footprint. (LOC does *not* handle NaN payloads - or sign, for that matter)
local_func_inl constexpr bool is_quiet_nan_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_quiet_nan_fp(constValue.uEmbeddedValue);
}
// infinite FP *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_infinite_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_infinite_fp(constValue.uEmbeddedValue);
}
// infinite FP *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_pos_infinity_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_pos_infinity_fp(constValue.uEmbeddedValue);
}
// infinite FP *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_neg_infinity_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_neg_infinity_fp(constValue.uEmbeddedValue);
}
// strict +-0 *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_pos_or_neg_zero_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_pos_or_neg_zero_fp(constValue.uEmbeddedValue);
}
// strict +0 *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_pos_zero_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_pos_zero_fp(constValue.uEmbeddedValue);
}
// strict -0 *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_neg_zero_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_neg_zero_fp(constValue.uEmbeddedValue);
}
// strict +-1 *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_pos_or_neg_one_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_pos_or_neg_one_fp(constValue.uEmbeddedValue);
}
// strict +-1 *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_pos_one_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_pos_one_fp(constValue.uEmbeddedValue);
}
// strict +-1 *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_neg_one_fp(u32 uConstFlags, AKnownValue constValue) {
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_neg_one_fp(constValue.uEmbeddedValue);
}

// zero, infinity or nan
local_func_inl constexpr bool is_embd_zero_or_non_finite_fp(u64 uEmbedded) {
    return is_embd_pos_or_neg_zero_fp(uEmbedded) || (uEmbedded & 0x7FF'0'0000'0000'0000uLL) == 0x7FF'0'0000'0000'0000uLL;
}

// zero or non-finite FP (infinity or nan) *always* get embedded as double whatever the footprint
local_func_inl constexpr bool is_zero_or_non_finite_fp(const TypeInfo_FloatingPoint* pType,
    u32 uConstFlags, AKnownValue constValue, CompilationContext* pEvalContext)
{
    UNUSED(pType);
    UNUSED(pEvalContext);
    return (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) ==
           (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA)) &&
           is_embd_zero_or_non_finite_fp(constValue.uEmbeddedValue);
}

// returns whether negative flag is present for any FP value (even NaN or zero)
local_func bool is_fp_negative(const TypeInfo_FloatingPoint* pType, u32 uConstFlags, AKnownValue constValue, CompilationContext* pEvalContext)
{
    if ((IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD) == (uConstFlags & (IRFLAG_IS_KNOWN|IRFLAG_IS_KNOWN_EMBD|IRFLAG_HAS_NYKA))) {
        return 0 != (constValue.uEmbeddedValue & 0x8000'0000'0000'0000uLL);
    } else {
        // TODO
        platform_log_error("*** is_fp_negative() : not yet implemented for non-embedded", true);
        Assert_(false);
    }
}

// returns whether any FP value is subnormal (incl. zero)
local_func bool is_fp_subnormal_or_zero(const TypeInfo_FloatingPoint* pType, u32 uConstFlags, AKnownValue constValue, CompilationContext* pEvalContext)
{
    // TODO
    platform_log_error("*** is_fp_subnormal() : not yet implemented", true);
    Assert_(false);
}

struct TypeInfo_HwVector : public TypeInfo_CoreBase {
    TypeInfo* pElementType;
};
local_func_inl const TypeInfo* get_element_type(const TypeInfo_HwVector* pVecType) { return pVecType->pElementType; }

struct TypeInfo_Pointer : public TypeInfo_UserBase {
    const TypeInfo* pPointedToType;
};
local_func_inl const TypeInfo* get_pointed_to_type(const TypeInfo_Pointer* pPtrType) { return pPtrType->pPointedToType; }

local_func_inl void init_ptr_type(TypeInfo_Pointer* ioToInit, const TypeInfo* pToType, CompilationContext* pEvalContext) {
    *ioToInit = {};
    ioToInit->uTypeKind = ETypeKind::ETYPEKIND_POINTER;
    ioToInit->uIRFormat = 0x03u;
    ioToInit->uSlotsAndAlign = 0x3000'0001u;
    ioToInit->pPointedToType = pToType;
}

struct TypeInfo_Array : public TypeInfo_UserBase {
    const TypeInfo* pElementType;
    u32 uElemCount;
    const TypeInfo_StructLike* pAsRuntimeStruct;
};
local_func_inl const TypeInfo* get_element_type(const TypeInfo_Array* pArrayType) { return pArrayType->pElementType; }
local_func_inl u8 get_array_category_type(const TypeInfo_Array* pArray) { return pArray->_coreType; }

#define     ARRAY_TYPE_FLAG_IS_SLICE                    0x01u
#define     ARRAY_TYPE_FLAG_IS_IMMUTABLE                0x02u
#define     ARRAY_TYPE_FLAG_IS_DYNAMIC                  0x04u
#define     ARRAY_TYPE_FLAG_IS_FF_DYNAMIC               0x08u
#define     ARRAY_TYPE_KIND_HAS_BUFFER                  0x10u
#define     ARRAY_TYPE_KIND_IS_STABLE_ON_GROW           0x20u
#define     ARRAY_TYPE_FLAG_HAS_FURTHER_COMPACTION      0x40u
#define     ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE          0x80u

#define     ARRAY_TYPE_KIND_STATIC                      0x00u
#define     ARRAY_TYPE_KIND_SLICE                       (ARRAY_TYPE_FLAG_IS_SLICE|ARRAY_TYPE_FLAG_IS_IMMUTABLE)
#define     ARRAY_TYPE_KIND_COMPACT_SLICE               (ARRAY_TYPE_FLAG_IS_SLICE|ARRAY_TYPE_FLAG_IS_IMMUTABLE|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_MUTABLE_SLICE               (ARRAY_TYPE_FLAG_IS_SLICE)
#define     ARRAY_TYPE_KIND_MUTABLE_COMPACT_SLICE       (ARRAY_TYPE_FLAG_IS_SLICE|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_DYNAMIC                     (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_COMPACT_DYNAMIC             (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE|ARRAY_TYPE_FLAG_HAS_FURTHER_COMPACTION)
#define     ARRAY_TYPE_KIND_DYNAMIC_FF                  (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_IS_FF_DYNAMIC|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_COMPACT_DYNAMIC_FF          (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_IS_FF_DYNAMIC|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE|ARRAY_TYPE_FLAG_HAS_FURTHER_COMPACTION)
#define     ARRAY_TYPE_KIND_BUFFER                      (ARRAY_TYPE_KIND_HAS_BUFFER|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_DYNAMIC_WITH_BUFFER         (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_IS_FF_DYNAMIC|ARRAY_TYPE_KIND_HAS_BUFFER|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_STABLE_GROWING              (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_KIND_IS_STABLE_ON_GROW)
#define     ARRAY_TYPE_KIND_STABLE_GROWING_FF           (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_IS_FF_DYNAMIC|ARRAY_TYPE_KIND_IS_STABLE_ON_GROW)
#define     ARRAY_TYPE_KIND_COMPACT_STABLE_GROWING      (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_KIND_IS_STABLE_ON_GROW|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)
#define     ARRAY_TYPE_KIND_COMPACT_STABLE_GROWING_FF   (ARRAY_TYPE_FLAG_IS_DYNAMIC|ARRAY_TYPE_FLAG_IS_FF_DYNAMIC|ARRAY_TYPE_KIND_IS_STABLE_ON_GROW|ARRAY_TYPE_FLAG_HAS_COMPACTED_SIZE)

local_func_inl void init_array_type(TypeInfo_Array* ioToInit, const TypeInfo* pElemType,
    u32 uElemCountAndCategory8Msb, CompilationContext* pEvalContext)
{
    *ioToInit = {};
    ioToInit->uTypeKind = ETypeKind::ETYPEKIND_ARRAY;
    u8 uArrayCategory = u8(uElemCountAndCategory8Msb >> 24);
    u32 uElemCount = uElemCountAndCategory8Msb & 0x00FF'FFFFu;
    if (uArrayCategory == ARRAY_TYPE_KIND_STATIC) {
        ioToInit->uIRFormat = pElemType->uIRFormat;
        ioToInit->uSlotsAndAlign = (0xF000'0000u & pElemType->uSlotsAndAlign) | (get_slots_count(pElemType) * uElemCount);
        ioToInit->pAsRuntimeStruct = 0;
    } else if (uArrayCategory == ARRAY_TYPE_KIND_BUFFER) {
        ioToInit->uIRFormat = pElemType->uIRFormat;
        u32 uAlignLog2 = _max(get_log2_of_align_bytes(pElemType), 2u);
        u32 uRuntimeSizeOfElem = get_runtime_sizeof(pElemType);
        Assert_(uRuntimeSizeOfElem); // void types todo ???
        u32 uBytesForSize = align_to(1u << uAlignLog2, 4u);
        u32 uLog2OfSlotSize = get_log2_of_slot_size_from_format(get_ir_format(pElemType));
        Assert_(uBytesForSize >= (1u << uLog2OfSlotSize));
        u32 uSlotCountForSizeMember = uBytesForSize >> uLog2OfSlotSize;
        ioToInit->uSlotsAndAlign = (uAlignLog2 << 28) | (get_slots_count(pElemType) * (uElemCount + uSlotCountForSizeMember));
        ioToInit->pAsRuntimeStruct = 0;
    } else if (uArrayCategory == ARRAY_TYPE_KIND_SLICE) {
        ioToInit->uIRFormat = 0x03u;
        ioToInit->uSlotsAndAlign = 0x3000'0002u;        // always 2x64b slots for slices (1x ptr, 1x u64 size)
        ioToInit->pAsRuntimeStruct = 0; // TODO ???
    } else {
        // TODO
        Assert(false, "init_array_type(): not yet implemented for other than static-arrays, buffers, or slices");
    }
    ioToInit->_coreType = uArrayCategory;
    ioToInit->pElementType = pElemType;
    ioToInit->uElemCount = uElemCount;
}

local_func_inl u32 get_elem_count_and_category_for_array_type(TypeInfo_Array* pType)
{
    return pType->uElemCount | (u32(pType->_coreType) << 24);
}

#define NO_DEFAULT_VALUE    0xFFFFFFFFu

struct ProcLikeParam {
    int iIdentifier;
    u32 uOptDefaultValueNodeIndex;
    ValueBinding* pBinding;
};
struct TypeInfo_ProcLike : public TypeInfo_UserBase {
    TmpArray<ProcLikeParam> params;
};
local_func_inl u8 get_proc_kind(const TypeInfo_ProcLike* pProcSign) { return pProcSign->_coreType; }
local_func_inl u8 get_total_param_count(const TypeInfo_ProcLike* pProcSign) { return u8(pProcSign->params.size()); }
local_func_inl u8 get_input_param_count(const TypeInfo_ProcLike* pProcSign) { return pProcSign->_coreFlags; }
local_func_inl u8 get_output_param_count(const TypeInfo_ProcLike* pProcSign) { return get_total_param_count(pProcSign) - get_input_param_count(pProcSign); }

local_func_inl void init_proc_like(TypeInfo_ProcLike* ioProcLike, u8 uProcKind, Arena arena)
{
    *ioProcLike = {};
    ioProcLike->uTypeKind = ETypeKind::ETYPEKIND_PROCLIKEBODY;
    ioProcLike->uIRFormat = 0x03u;                   // TODO: CLEANUP: think about those formats some more...
    ioProcLike->uSlotsAndAlign = 0x3000'0001u;
    ioProcLike->_coreType = uProcKind;
    ioProcLike->params.init(FireAndForgetArenaAlloc(arena));
}

local_func_inl void set_proc_like_input_param_count(TypeInfo_ProcLike* ioProcLike, u8 uInParamsCount)
{
    ioProcLike->_coreFlags = uInParamsCount;
}

TypeInfo* g_pCoreTypesInfo[COUNT_ECORETYPES];

local_func_inl const TypeInfo_Integral* get_unsigned_type_of_same_width_from(const TypeInfo_Integral* pType)
{
    Assert_(pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    if (pType == g_pCoreTypesInfo[ECORETYPE_INT] || pType == g_pCoreTypesInfo[ECORETYPE_NAT])
        return (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_NAT];
    u8 uWidthLog2 = get_log2_of_scalar_bytes_from_format(get_ir_format(pType));
    return (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_U8 + uWidthLog2];
}

local_func_inl const TypeInfo_Integral* get_signed_type_of_same_width_from(const TypeInfo_Integral* pType)
{
    Assert_(pType != g_pCoreTypesInfo[ECORETYPE_COMPINT]);
    if (pType == g_pCoreTypesInfo[ECORETYPE_INT] || pType == g_pCoreTypesInfo[ECORETYPE_NAT])
        return (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_INT];
    u8 uWidthLog2 = get_log2_of_scalar_bytes_from_format(get_ir_format(pType));
    return (const TypeInfo_Integral*)g_pCoreTypesInfo[ECORETYPE_I8 + uWidthLog2];
}

struct CTViewOfRTTI {
    const TypeInfo* pCTMergedTypeInfo;
};

struct RuntimeTypeId {
    const CTViewOfRTTI* rttiHandle;
};

struct TypeInfo_Set : public TypeInfo_UserBase {
    // TODO !!!
    const TypeInfo_StructLike* pAsRuntimeStruct;
};

struct TypeInfo_Map : public TypeInfo_UserBase {
    // TODO !!!
    const TypeInfo_StructLike* pAsRuntimeStruct;
};

#define OTHERCOREFLAG_IS_STRING     0x08u
#define STRINGFLAG_IS_COMPACT       0x01u
#define STRINGFLAG_HAS_ALLOC        0x02u

struct TypeInfo_OtherCore : public TypeInfo_CoreBase {
    const TypeInfo_StructLike* pAsRuntimeStruct;
};

local_func_inl bool is_string_related(const TypeInfo* pType) {
    return get_type_kind(pType) == ETypeKind::ETYPEKIND_OTHERCORE && 0 != (pType->_coreFlags & OTHERCOREFLAG_IS_STRING);
}

local_func const TypeInfo_StructLike* get_possible_runtime_structlike_(const TypeInfo* pType, ETypeKind eKind) {
    switch (eKind) {
        case ETypeKind::ETYPEKIND_STRUCTLIKE:       // structs returns themselves...
            return (const TypeInfo_StructLike*)pType; 

        case ETypeKind::ETYPEKIND_OTHERCORE:        // non-compact-strings return inrinsic structs with appropriate members ; any returns its impl
            return ((const TypeInfo_OtherCore*)pType)->pAsRuntimeStruct;

        case ETypeKind::ETYPEKIND_ARRAY:            // non-static and non-fully-compact-arrays return appropriate members
            return ((const TypeInfo_Array*)pType)->pAsRuntimeStruct;

        case ETypeKind::ETYPEKIND_SET:              // non-compact sets return appropriate members
            return ((const TypeInfo_Set*)pType)->pAsRuntimeStruct;

        case ETypeKind::ETYPEKIND_MAP:              // non-compact maps return appropriate members
            return ((const TypeInfo_Map*)pType)->pAsRuntimeStruct;
    }

    return 0; // the default is: no struct-like...
}


local_func_inl const TypeInfo_StructLike* get_possible_runtime_structlike(const TypeInfo* pType) {
    ETypeKind eKind;
    pType = unalias_ext(pType, &eKind);             // aliases shall not change what a struct is internally...
    return get_possible_runtime_structlike_(pType, eKind);
}

// returns only the flags indicating TC progress from a TCNode.
local_func_inl u32 get_tc_phase_node_flags(const TCNode* pNode) {
    return pNode->ast.uNodeKindAndFlags & (ENODEKINDFLAG_IS_TYPECHECKED_PHASE1|ENODEKINDFLAG_IS_TYPECHECKED_PHASE2);
}
// indicates whether a node is flaggued in-error, which should never happen on the nominal typechecker paths
//   (errors are found as we typecheck, and once we find it, we shall never 'resume' a typecheck attempt from same statement)
local_func_inl bool is_node_tc_in_error(const TCNode* pNode) {
    return get_tc_phase_node_flags(pNode) == ENODEKINDFLAG_IS_TYPECHECKED_PHASE2; // tc phase 2 without phase 1 indicates error
}
// indicates whether a node has already passed typechecking phase 1, which is typechecking it in isolation
local_func_inl bool is_node_already_typechecked(const TCNode* pNode) {
    return 0 != (pNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_IS_TYPECHECKED_PHASE1);
}
// indicates whether a node has already passed typechecking phase 2, which is typechecking it in context,
//   ie type-casting to its contextual usage type
local_func_inl bool is_node_already_type_casted(const TCNode* pNode) {
    // we're only checking for phase2 flag in this implementation => we suppose it is not in error.
    Assert_(!is_node_tc_in_error(pNode));
    return 0 != (pNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_IS_TYPECHECKED_PHASE2);
}
// indicates whether a node has not yet been typechecked (and is not in error either)
local_func_inl bool is_node_tc_not_started(const TCNode* pNode) {
    return get_tc_phase_node_flags(pNode) == 0;
}

// returns contents of a 'uNodeKindAndFlags' on an ast node, except any of the typechecker-specific flags
local_func_inl u32 get_node_kind_and_flags_except_tc_from(const TCNode* pNode) {
    return pNode->ast.uNodeKindAndFlags &
        ~(ENODEKINDFLAG_IS_TYPECHECKED_PHASE1|ENODEKINDFLAG_IS_TYPECHECKED_PHASE2|ENODEKINDFLAG_HAS_NO_TC_EXPR_VALUE);
}
// returns contents of a 'uNodeKindAndFlags' on an ast node, except any of the typechecker-specific or bool inversion flags

// returned by most typechecking functions:
//   nominally "success", somethimes "waiting" if typechecking task requires info not-available yet to complete
enum ETCResult
{
    ETCR_SUCCESS = 0,   // Typecheck succeeded (and taking into account expectations)
    ETCR_WAITING,       // Completing typecheck would require additional information to continue. pEvalContext should have been updated with which info is missing.
    ETCR_ERROR,         // Typecheck failed (including when expectations could not be met)
};

union UpwardsInference {
//    EBoolTransform ifBoolTransform;  // if eq or less to 3 (including 'none')
    const TypeInfo* pIfType;
    u64 _payload;
};

local_func_inl UpwardsInference infer_type(const TypeInfo* pType) { UpwardsInference result; result.pIfType = pType; return result; }

local_func_inl bool is_inferred_none(UpwardsInference inferred) { return inferred._payload == 0uLL; }

local_func_inl ETCResult set_node_typecheck_expr_success(TCNode* pNode) {
    pNode->ast.uNodeKindAndFlags |= ENODEKINDFLAG_IS_TYPECHECKED_PHASE1;
    return ETCResult::ETCR_SUCCESS;
}

// raises the appropriate flags to indicate success on typechecking phase 2 on a TCNode (typecheked in context => typecasted)
local_func_inl ETCResult set_node_type_cast_expr_success(TCNode* pNode) {
    pNode->ast.uNodeKindAndFlags |= (ENODEKINDFLAG_IS_TYPECHECKED_PHASE1|ENODEKINDFLAG_IS_TYPECHECKED_PHASE2);
    return ETCResult::ETCR_SUCCESS;
}
// raises the appropriate flag to indicate success on typechecking phase 1 on a TCNode (typecheked in isolation)
local_func_inl ETCResult set_node_typecheck_notanexpr_success(TCNode* pNode) {
    pNode->ast.uNodeKindAndFlags |= ENODEKINDFLAG_IS_TYPECHECKED_PHASE1|ENODEKINDFLAG_HAS_NO_TC_EXPR_VALUE;
    return ETCResult::ETCR_SUCCESS;
}
// raises the appropriate flags to indicate a typechecking error on a TCNode
local_func_inl ETCResult set_node_tc_error(TCNode* pNode, u16 uErrCode) {
    UNUSED(uErrCode); // TODO ? use that ?
    pNode->ast.uNodeKindAndFlags = get_node_kind_and_flags_except_tc_from(pNode) | ENODEKINDFLAG_IS_TYPECHECKED_PHASE2; // tc phase 2 without phase 1 indicates error
    return ETCResult::ETCR_ERROR;
}

local_func_inl bool has_node_no_tc_expr_value(TCNode* pNode) {
    return pNode->ast.uNodeKindAndFlags & ENODEKINDFLAG_HAS_NO_TC_EXPR_VALUE;
}

// predecl
bool are_user_types_same(const TypeInfo_UserBase* pTypeA, const TypeInfo_UserBase* pTypeB, CompilationContext* pEvalContext);

local_func_inl bool are_types_same(const TypeInfo* pTypeA, const TypeInfo* pTypeB, CompilationContext* pEvalContext) {
    if (pTypeA == pTypeB)
        return true;
    else if (is_core_type(pTypeA) || is_core_type(pTypeB))
        return false; // core types would have had ptr equality
    else
        return are_user_types_same((const TypeInfo_UserBase*)pTypeA, (const TypeInfo_UserBase*)pTypeB, pEvalContext);
}

local_func_inl bool are_types_same_when_unaliased(const TypeInfo* pTypeA, const TypeInfo* pTypeB, CompilationContext* pEvalContext) {
    if (pTypeA == pTypeB)
        return true;
    else {
        const TypeInfo* pUnaliasedTypeA = unalias(pTypeA);
        const TypeInfo* pUnaliasedTypeB = unalias(pTypeB);
        return are_types_same(pUnaliasedTypeA, pUnaliasedTypeB, pEvalContext);
    }
}

local_func bool are_user_types_same(const TypeInfo_UserBase* pTypeA, const TypeInfo_UserBase* pTypeB, CompilationContext* pEvalContext)
{
    ETypeKind kind = get_type_kind(pTypeA);
    if (kind != get_type_kind(pTypeB))
        return false;
    else {
        u64 uTypeIdA = pTypeA->asNode.info.uIRandMetaFlags & IR_STD_PARAM_MASK;
        u64 uTypeIdB = pTypeB->asNode.info.uIRandMetaFlags & IR_STD_PARAM_MASK;
        if (ir_is_valid_param(uTypeIdA) && ir_is_valid_param(uTypeIdB))
            // both types have their typeid computed already => return id equality
            return uTypeIdA == uTypeIdB;
        else
        {
            switch (kind) {
                case ETypeKind::ETYPEKIND_DISTINCTALIAS:
                case ETypeKind::ETYPEKIND_ENUM:
                case ETypeKind::ETYPEKIND_STRUCTLIKE:
                    return pTypeA == pTypeB;        // each of those types are distinct on declare => only true for pointer equality

                case ETYPEKIND_POINTER: {
                    const TypeInfo_Pointer* pPtrTypeA = (const TypeInfo_Pointer*)pTypeA;
                    const TypeInfo_Pointer* pPtrTypeB = (const TypeInfo_Pointer*)pTypeB;
                    return are_types_same(get_pointed_to_type(pPtrTypeA), get_pointed_to_type(pPtrTypeB), pEvalContext);
                } break;

                case ETYPEKIND_ARRAY: {
                    const TypeInfo_Array* pArrayTypeA = (const TypeInfo_Array*)pTypeA;
                    const TypeInfo_Array* pArrayTypeB = (const TypeInfo_Array*)pTypeB;
                    if (pArrayTypeA->_coreType != pArrayTypeB->_coreType) // array 'category' flags : 0 for static, etc.
                        return false;
                    if (pArrayTypeA->uElemCount != pArrayTypeB->uElemCount)
                        return false;
                    return are_types_same(get_element_type(pArrayTypeA), get_element_type(pArrayTypeB), pEvalContext);
                } break;

                case ETYPEKIND_SET:
                case ETYPEKIND_MAP:
                case ETYPEKIND_PROCLIKEBODY:
                case ETYPEKIND_PROCLIKEOVERLOAD:
                case ETYPEKIND_PROCLIKEPOLYMORPH:
                case ETYPEKIND_STRUCTPOLYMORPH:
                    // TODO
                    platform_log_error("*** are_types_same() : not implemented for those user types");
                    Assert_(false);
                    return false;

                default:
                    Assert(false, "are_user_types_same() : unexpected type kind");
                    return false;
            }
        }
    }
}


#endif // LOCLIB_TYPE_INFO_DECLS_H_

