#pragma once

#include "BaseDecls.h"


template<typename T>
struct MutSlice
{
    MutSlice() = default;
    MutSlice(const MutSlice<T>&) = default;
    MutSlice<T>& operator=(const MutSlice<T>&) = default;
    ~MutSlice() = default;

    T* start;
    u64 count;
    FORCE_INLINE bool is_null() const { return start == 0; }
    FORCE_INLINE bool is_null_or_empty() const { return start == 0 || length() == 0; }
    FORCE_INLINE bool is_empty() const { return length() == 0 && start != 0; }
    FORCE_INLINE u64 length() const { return count; }
    FORCE_INLINE u64 byte_length() const { return count * sizeof(T); }
    FORCE_INLINE const T* begin() const { return start; }
    FORCE_INLINE const T* end() const { return start + count; }
    FORCE_INLINE const T* at(u64 uPos) const { Assert_(uPos < count); return start + uPos; }
    FORCE_INLINE const T& operator[](u64 uPos) const { return *at(uPos); }
    FORCE_INLINE T* begin() { return start; }
    FORCE_INLINE T* end() { return start + count; }
    FORCE_INLINE T* at(u64 uPos) { Assert_(uPos < count); return start + uPos; }
    FORCE_INLINE T& operator[](u64 uPos) { return *at(uPos); }
    FORCE_INLINE MutSlice<T> subslice(u64 uStartPos, u64 uLength) {
        Assert_(!is_null());
        Assert_(uStartPos < count);
        Assert_(uLength <= count && uStartPos < count - uLength);
        MutSlice<T> result;
        result.start = start + uStartPos;
        result.count = uLength;
        return result;
    }
    FORCE_INLINE MutSlice<T> subslice_from(u64 uStartPos) const {
        Assert_(!is_null());
        Assert_(uStartPos < count);
        MutSlice<T> result;
        result.start = start + uStartPos;
        result.length = count - 1uLL - uStartPos;
        return result;
    }
    FORCE_INLINE MutSlice<u8> as_byte_slice() const { 
        MutSlice<u8> result; result.start = reinterpret_cast<u8*>(start); result.count = count * sizeof(T); return result;
    }
};

template<typename T>
struct Slice
{
    Slice() = default;
    Slice(const Slice<T>&) = default;
    Slice<T>& operator=(const Slice<T>&) = default;
    ~Slice() = default;

    Slice(const MutSlice<T>& mut):start(mut.start), count(mut.count) {}
    Slice<T>& operator=(const MutSlice<T>& mut) { start = mut.start; count = mut.count; return *this; }

    const T* start;
    u64 count;
    FORCE_INLINE bool is_null() const { return start == 0; }
    FORCE_INLINE bool is_null_or_empty() const { return start == 0 || length() == 0; }
    FORCE_INLINE bool is_empty() const { return length() == 0 && start != 0; }
    FORCE_INLINE u64 length() const { return count; }
    FORCE_INLINE u64 byte_length() const { return count * sizeof(T); }
    FORCE_INLINE const T* begin() const { return start; }
    FORCE_INLINE const T* end() const { return start + count; }
    FORCE_INLINE const T* at(u64 uPos) const { Assert_(uPos < count); return start + uPos; }
    FORCE_INLINE const T& operator[](u64 uPos) const { return *at(uPos); }
    FORCE_INLINE Slice<T> subslice(u64 uStartPos, u64 uLength) const {
        Assert_(!is_null());
        Assert_(uStartPos < count);
        Assert_(uLength <= count && uStartPos < count - uLength);
        Slice<T> result;
        result.start = start + uStartPos;
        result.count = uLength;
        return result;
    }
    FORCE_INLINE Slice<T> subslice_from(u64 uStartPos) const {
        Assert_(!is_null());
        Assert_(uStartPos < count);
        Slice<T> result;
        result.start = start + uStartPos;
        result.length = count - 1uLL - uStartPos;
        return result;
    }
    FORCE_INLINE Slice<u8> as_byte_slice() const { 
        Slice<u8> result; result.start = reinterpret_cast<u8*>(start); result.count = count * sizeof(T); return result;
    }
};

typedef u32 CodePoint;

#define STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS     1u
#define STRING_VIEW_FLAG_ENDS_WITH_ZERO                   2u
#define STRING_VIEW_FLAG_KNOWN_7b_ASCII                   4u
#define STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII               8u
#define STRING_VIEW_FLAG_KNOWN_VALID_Utf8                 16u
#define STRING_VIEW_FLAG_KNOWN_INVALID_Utf8               32u

#include <string.h>

struct StringView
{
    const u8* start;
    u32 uByteLength;
    u32 flags;
    StringView() = default;
    StringView(const StringView&) = default;
    StringView& operator=(const StringView&) = default;
    ~StringView() = default;
    FORCE_INLINE StringView(const char* szString) {
        *this = StringView::from_c_str(szString);
    }
    FORCE_INLINE bool is_null() const { return start == 0; }
    FORCE_INLINE bool is_null_or_empty() const { return start == 0 || byte_length() == 0; }
    FORCE_INLINE bool is_empty() const { return byte_length() == 0 && start != 0; }
    FORCE_INLINE u32 byte_length() const { return u32(uByteLength); }
    FORCE_INLINE const u8* begin() const { return start; }
    FORCE_INLINE const u8* end() const { return start + uByteLength; }
    FORCE_INLINE u32 get_flags() const { return flags; }
    FORCE_INLINE bool can_be_used_as_c_str() const { return flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO; }
    FORCE_INLINE bool can_be_used_as_ffstring() const { return can_be_used_as_c_str() && (flags & STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS); }
    FORCE_INLINE bool is_known_7b_ascii() const { return flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII; }
    FORCE_INLINE bool is_known_non_7b_ascii() const { return flags & STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII; }
    FORCE_INLINE bool is_known_valid_utf8() const { return flags & STRING_VIEW_FLAG_KNOWN_VALID_Utf8; }
    FORCE_INLINE bool is_known_invalid_utf8() const { return flags & STRING_VIEW_FLAG_KNOWN_INVALID_Utf8; }
    void check_utf8() {
        if (flags & (STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII)) {
            Assert_(!is_null_or_empty());
            return;
        }
        Assert_(0 == (flags & (STRING_VIEW_FLAG_KNOWN_VALID_Utf8 | STRING_VIEW_FLAG_KNOWN_INVALID_Utf8)));
        const u8* pEnd = start + uByteLength;
        for (const u8* pByte = start; pByte < pEnd; pByte++) {
            if (*pByte & 0x80u) {
                flags |= STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII;
                // TODO: check validity
                Assert_(false);
                return;
            }
        }
        flags |= STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;
    }
    FORCE_INLINE Slice<u8> as_byte_slice() const { 
        Slice<u8> result; result.start = start; result.count = u64(uByteLength); return result;
    }
    FORCE_INLINE Slice<u8> as_byte_slice_incl_trailing_zero() const { 
        Assert_(!is_null());
        Assert_(flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO);
        Slice<u8> result; result.start = start; result.count = u64(uByteLength) + 1uLL; return result;
    }
    StringView substr_utf8(u32 uStartPos, u32 uLength) const {
        Assert_(!is_null());
        u32 uUtfLength = utf_length();
        StringView result;
        result.start = get_first_byte_at_utf_index(uStartPos);
        if (uLength) {
            if (is_known_7b_ascii()) {
                Assert_(u64(uStartPos) + u64(uLength) <= u64(uByteLength));
                result.uByteLength = uLength;
            } else {
                // TODO
                Assert_(false);
                return result;
            }
            result.flags |= (flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII) | (flags & STRING_VIEW_FLAG_KNOWN_VALID_Utf8);
            if (uLength == uUtfLength)
                result.flags |= (flags & STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII) | (flags & STRING_VIEW_FLAG_KNOWN_INVALID_Utf8);
        } else {
            result.uByteLength = 0;
            result.flags |= STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;
        }
        if (0 == uStartPos)
            result.flags |= flags & STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS;
        if (uStartPos == uUtfLength - 1u)
            result.flags |= flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO;
        return result;
    }
    StringView substr_utf8_from(u32 uStartPos) const {
        Assert_(!is_null());
        StringView result;
        result.start = get_first_byte_at_utf_index(uStartPos);
        size_t remainingSize = (start + uByteLength) - result.start;
        if (remainingSize) {
            result.uByteLength = u32(remainingSize);
            result.flags |= (flags & STRING_VIEW_FLAG_KNOWN_7b_ASCII) | (flags & STRING_VIEW_FLAG_KNOWN_VALID_Utf8);
            if (u32(remainingSize) == uByteLength)
                result.flags |= (flags & STRING_VIEW_FLAG_KNOWN_NON_7b_ASCII) | (flags & STRING_VIEW_FLAG_KNOWN_INVALID_Utf8);
        } else {
            result.uByteLength = 0;
            result.flags |= STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;
        }
        if (0 == uStartPos)
            result.flags |= flags & STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS;
        result.flags |= flags & STRING_VIEW_FLAG_ENDS_WITH_ZERO;
        return result;
    }
    u32 utf_length() const {
        if (is_known_7b_ascii())
            return uByteLength;
        else {
            const_cast<StringView*>(this)->check_utf8();
            if (is_known_7b_ascii())
                return uByteLength;
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
            Assert_(uPos < uByteLength);
            return start + uPos;
        } else {
            if (uPos) {
                const_cast<StringView*>(this)->check_utf8();
                if (is_known_7b_ascii()) {
                    Assert_(uPos < uByteLength);
                    return start + uPos;
                } else {
                    // TODO
                    Assert_(false);
                    return 0;
                }
            } else
                return start;
        }
    }
    CodePoint operator[](u32 uPos) {
        if (is_known_7b_ascii())
            return u32(*(start + uPos));
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
    static StringView from_known_c_str(const char* szString, u32 uLength, bool bZeroAfterwards) {
        StringView result;
        if (szString) {
            result.start = (const u8*)szString;
            result.uByteLength = uLength;
            result.flags = 0;
            if (bZeroAfterwards) {
                Assert_(szString[uLength] == '\0');
                result.flags |= STRING_VIEW_FLAG_ENDS_WITH_ZERO;
            }
            if (0 == uLength)
                result.flags |= STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;
        } else {
            Assert_(uLength == 0 && bZeroAfterwards == false);
            result.start = 0;
            result.uByteLength = 0;
            result.flags = 0;
        }
        return result;
    }
    static StringView from_c_str(const char* szString, u32 uMaxLookupLength = 0x0001'0000uLL) {
        StringView result;
        if (szString) {
            u32 uLen = u32(strnlen(szString, uMaxLookupLength));
            if (uLen == uMaxLookupLength) {
                result.start = 0;
                result.uByteLength = 0;
                result.flags = STRING_VIEW_FLAG_KNOWN_INVALID_Utf8;
                return result;
            }
            result.start = (const u8*)szString;
            result.uByteLength = uLen;
            result.flags = STRING_VIEW_FLAG_ENDS_WITH_ZERO;
            if (0 == uLen)
                result.flags |= STRING_VIEW_FLAG_KNOWN_7b_ASCII | STRING_VIEW_FLAG_KNOWN_VALID_Utf8;
        } else {
            result.start = 0;
            result.uByteLength = 0;
            result.flags = 0;
        }
        return result;
    }
};

static FORCE_INLINE int cmpInt(i64 iFirst, i64 iSecond) {
    if (iFirst == iSecond)
        return 0;
    else
        return iFirst < iSecond ? -1 : +1;
}

static int cmpNonNullString(StringView first, StringView second) {
    i64 iSizeFirst = i64(u64(first.uByteLength));
    i64 iSizeSecond = i64(u64(second.uByteLength));
    i64 iMinSize = _min(iSizeFirst, iSizeSecond);
    const u8* pF = first.start;
    const u8* pS = second.start;
    for (i64 i = 0; i < iMinSize; i++, pF++, pS++) { // scanning each char (pairwise at same index) from start to end...
        u8 f = *pF;
        u8 s = *pS;
        if (f < s) // as soon as one char with greater/lesser code than the other is found, we have an ordering
            return -1;
        else if (f > s)
            return +1;
        // otherwise noop for this iteration => continue by scanning next char
    }
    // once we reach end of string on one or the other without any difference until that point...
    return cmpInt(iSizeFirst, iSizeSecond); // the greatest is the one with remaining chars... if same length, means equality
}

static int cmpNonNullStringNoCaseASCII(StringView first, StringView second) {
    i64 iSizeFirst = i64(u64(first.uByteLength));
    i64 iSizeSecond = i64(u64(second.uByteLength));
    i64 iMinSize = _min(iSizeFirst, iSizeSecond);
    const u8* pF = first.start;
    const u8* pS = second.start;
    for (i64 i = 0; i < iMinSize; i++, pF++, pS++) { // scanning each char (pairwise at same index) from start to end...
        u8 f = *pF; if (f >= 'A' && f <= 'Z') f |= 0x10; // lower-cases f (only if plain 7b ASCII, though)
        u8 s = *pS; if (s >= 'A' && s <= 'Z') s |= 0x10; // lower-cases s (only if plain 7b ASCII, though)
        if (f < s) // as soon as one char with greater/lesser code than the other is found, we have an ordering
            return -1;
        else if (f > s)
            return +1;
        // otherwise noop for this iteration => continue by scanning next char
    }
    // once we reach end of string on one or the other without any difference until that point...
    return cmpInt(iSizeFirst, iSizeSecond); // the greatest is the one with remaining chars... if same length, means equality
}

static FORCE_INLINE bool areNonNullStringEqual(StringView first, StringView second) {
    // Implemented in terms of 'true when cmpNonNullString returns 0', with the following additional shortcuts:
    if (first.uByteLength != second.uByteLength)
        return false;                   // shortcuts to false if size different (since cmpNonNullString would do uneccessary work)
    if (first.start == second.start)
        return true;                    // shortcuts to true if start ref equal (since we also already know they're same size)
    return 0 == cmpNonNullString(first, second);
}

static FORCE_INLINE bool areNonNullStringEqualNoCaseASCII(StringView first, StringView second) {
    // Implemented in terms of 'true when cmpNonNullStringNoCaseASCII returns 0', with the following additional shortcuts:
    if (first.uByteLength != second.uByteLength)
        return false;                   // shortcuts to false if size different (since cmpNonNullStringNoCaseASCII would do uneccessary work)
    if (first.start == second.start)
        return true;                    // shortcuts to true if start ref equal (since we also already know they're same size)
    return 0 == cmpNonNullStringNoCaseASCII(first, second);
}

