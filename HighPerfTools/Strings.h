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

#include "BaseDecls.h"
#include "Platform.h"
#include "Arenas.h"
#include "AllocTraits.h"
#include "StringView.h"

struct FFString {
    u8* pStart;
    FORCE_INLINE bool is_null() const { return pStart == 0; }
    FORCE_INLINE bool is_null_or_empty() const { return pStart == 0 || byte_length() == 0; }
    FORCE_INLINE bool is_empty() const { return byte_length() == 0 && pStart != 0; }
    FORCE_INLINE u32 byte_length() const { return *reinterpret_cast<u32*>(pStart-4); }
    // Note: The implicit TZ is already taken into account in the following formula, and it is *not* the visible '+1' (more like the absence of -1 before mask with 3):
    static FORCE_INLINE u32 _as_ff_dword_count_(u32 uByteLength) { return 2u + (((uByteLength | 0x03u) + 1u) >> 2); }
    FORCE_INLINE u32 _as_ff_dword_count() const { return _as_ff_dword_count_(byte_length()); }
    FORCE_INLINE u32* _as_ff_start() { return reinterpret_cast<u32*>(pStart-8); }
    FORCE_INLINE const u8* begin() const { return pStart; }
    FORCE_INLINE const u8* end() const { return pStart + byte_length(); }
    FORCE_INLINE const char* c_str() const { return (char*)pStart; }
    FORCE_INLINE u32 get_flags() const { return *reinterpret_cast<u32*>(pStart-8); }
    FORCE_INLINE bool can_be_used_as_c_str() const { return get_flags() & STRING_VIEW_FLAG_ENDS_WITH_ZERO; }
    FORCE_INLINE bool can_be_used_as_ffstring() const { return can_be_used_as_c_str() && (get_flags() & STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS); }
    FORCE_INLINE bool is_known_7b_ascii() const { return get_flags() & STRING_VIEW_FLAG_KNOWN_7b_ASCII; }
    FORCE_INLINE bool is_known_non_7b_ascii() const { return get_flags() & STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII; }
    FORCE_INLINE bool is_known_valid_utf8() const { return get_flags() & STRING_VIEW_FLAG_KNOWN_VALID_Utf8; }
    FORCE_INLINE bool is_known_invalid_utf8() const { return get_flags() & STRING_VIEW_FLAG_KNOWN_INVALID_Utf8; }
    void check_utf8() {
        if (get_flags() & (STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII)) {
            Assert_(!is_null_or_empty());
            return;
        }
        Assert_(0 == (get_flags() & (STRING_VIEW_FLAG_KNOWN_VALID_Utf8 | STRING_VIEW_FLAG_KNOWN_INVALID_Utf8)));
        const u8* pEnd = pStart + byte_length();
        for (const u8* pByte = pStart; pByte < pEnd; pByte++) {
            if (*pByte & 0x80u) {
                *reinterpret_cast<u32*>(pStart-8) |= STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII;
                // TODO: check validity
                Assert_(false);
                return;
            }
        }
        *reinterpret_cast<u32*>(pStart-8) |= STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;
    }
    FORCE_INLINE operator StringView() const {
        StringView result;
        result.start = begin();
        result.uByteLength = byte_length();
        return result;
    }
    FORCE_INLINE void reset() {
        pStart = 0;
    }
    static FFString makeFF(StringView orig, Arena arena) {
        FFString result;
        u32 uDwordLength = _as_ff_dword_count_(orig.byte_length());
        u8* pAllocated = alloc_from(arena, uDwordLength * 4u, 4u);
        u32 uFlags = orig.flags | STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS | STRING_VIEW_FLAG_ENDS_WITH_ZERO;
        *reinterpret_cast<u32*>(pAllocated) = uFlags;
        *reinterpret_cast<u32*>(pAllocated + 4) = orig.byte_length();
        *reinterpret_cast<u32*>(pAllocated + (uDwordLength-1u)*4u) = 0; // last u32 zero, incl at least one zero-trailing byte
        result.pStart = pAllocated + 8;
        memcpy(result.pStart, orig.begin(), orig.byte_length());
        return result;
    }
    template<typename AllocT>
    static FFString fromAllocatorMakeFF(StringView orig, AllocT alloc) {
        FFString result;
        u32 uDwordLength = _as_ff_dword_count_(orig.byte_length());
        u8* pAllocated = alloc.allocate(uDwordLength * 4u, 4u);
        u32 uFlags = orig.flags | STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS | STRING_VIEW_FLAG_ENDS_WITH_ZERO;
        *reinterpret_cast<u32*>(pAllocated) = uFlags;
        *reinterpret_cast<u32*>(pAllocated + 4) = orig.byte_length();
        *reinterpret_cast<u32*>(pAllocated + (uDwordLength-1u)*4u) = 0; // last u32 zero, incl at least one zero-trailing byte
        result.pStart = pAllocated + 8;
        memcpy(result.pStart, orig.begin(), orig.byte_length());
        return result;
    };

    static FFString getOrMakeFF(StringView orig, Arena arena) {
        if ((orig.get_flags() & STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS) && (orig.get_flags() & STRING_VIEW_FLAG_ENDS_WITH_ZERO)) {
            FFString result;
            result.pStart = const_cast<u8*>(orig.start);
            return result;
        } else
            return makeFF(orig, arena);
    }
    u32 utf_length() const {
        if (is_known_7b_ascii())
            return byte_length();
        else {
            const_cast<FFString*>(this)->check_utf8();
            if (is_known_7b_ascii())
                return byte_length();
            else {
                // TODO
                Assert_(false);
                return 0;
            }
        }
    }
    const u8* get_first_byte_at_utf_index(u32 uPos) const {
        Assert_(!is_null());
        if (is_known_7b_ascii()) {
            Assert_(uPos < byte_length());
            return pStart + uPos;
        } else {
            if (uPos) {
                const_cast<FFString*>(this)->check_utf8();
                if (is_known_7b_ascii()) {
                    Assert_(uPos < byte_length());
                    return pStart + uPos;
                } else {
                    // TODO
                    Assert_(false);
                    return 0;
                }
            } else
                return pStart;
        }
    }
    CodePoint operator[](u32 uPos) {
        if (is_known_7b_ascii())
            return u32(*(pStart + uPos));
        else {
            const u8* pFirst = get_first_byte_at_utf_index(uPos);
            if (*pFirst & 0x80u) {
                // TODO
                Assert_(false);
                return 0;
            } else {
                return u32(*pFirst);
            }
        }
    }
};

struct OwnedString : public FFString {
    FORCE_INLINE AllocAndFreeFuncSign get_free_fn() { return *reinterpret_cast<AllocAndFreeFuncSign*>(pStart-24); }
    FORCE_INLINE u64 get_free_fn_param()   { return *reinterpret_cast<u64*>(pStart-16); }
    FORCE_INLINE void release() {
        Assert_(pStart);
        AllocAndFreeFuncSign pFreeFn = get_free_fn();
        pFreeFn(reinterpret_cast<u64>(pStart-24), EAllocAndFreeOp::OP_FREE, 8u, get_free_fn_param());
        pStart = 0;
    }
    template<typename AllocT> static OwnedString makeOWned(StringView orig, AllocT allocator) {
        OwnedString result;
        u8* pAllocated = allocator.allocate(orig.byte_length() + 25u, 8u);
        u32 uFlags = orig.flags | STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS | STRING_VIEW_FLAG_ENDS_WITH_ZERO;
        *reinterpret_cast<AllocAndFreeFuncSign*>(pAllocated) = allocator.getFreeFn();
        *reinterpret_cast<void**>(pAllocated+8) = allocator.getFreeParams();
        *reinterpret_cast<u32*>(pAllocated+16) = uFlags;
        *reinterpret_cast<u32*>(pAllocated+20) = orig.byte_length();
        result.pStart = pAllocated + 24;
        memcpy(result.pStart, orig.begin(), orig.byte_length());
        result.pStart[orig.byte_length()] = 0;
        return result;
    };
};
