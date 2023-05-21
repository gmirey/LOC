// -------------------------------------------------------
// This implementation is derived from the Abseil library
// -------------------------------------------------------
//
// Copyright 2017 The Abseil Authors.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      https://www.apache.org/licenses/LICENSE-2.0
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
#include "Hash.h"
#include "Strings.h"
#include "Arrays.h"

#define MAP_HASH_SEED   0x1337'1138'd011'7110uLL

//#define SET_GROUP_SIZE      16u
//#define SET_WINDOW_SIZE     

struct SetMetaDataImpl
{
    static const i8 kEmpty = -128;   // 0b_10000000
    static const i8 kDeleted = -2;   // 0b_11111110
    static const i8 kSentinel = -1;  // 0b_11111111

    static_assert((kEmpty & kDeleted & kSentinel & 0x80) != 0,
        "Special markers need to have the MSB set to make checking for them efficient");
    static_assert(kEmpty < kSentinel && kDeleted < kSentinel,
        "kEmpty and kDeleted must be smaller than kSentinel to make the SIMD test of IsEmptyOrDeleted() efficient");
    static_assert(kSentinel == -1,
        "kSentinel must be -1 to elide loading it from memory into SIMD registers (pcmpeqd xmm, xmm)");
    static_assert(kEmpty == -128,
        "kEmpty must be -128 to make the SIMD check for its existence efficient (psignb xmm, xmm)");
    static_assert((~kEmpty & ~kDeleted & kSentinel & 0x7F) != 0,
        "ctrl_t::kEmpty and ctrl_t::kDeleted must share an unset bit that is not shared by ctrl_t::kSentinel "
        "to make the scalar test for MatchEmptyOrDeleted() efficient");
    static_assert(kDeleted == -2,
        "ctrl_t::kDeleted must be -2 to make the implementation of ConvertSpecialToEmptyAndFullToDeleted efficient");

    static FORCE_INLINE bool IsEmpty(i8 c)  { return c == kEmpty; }
    static FORCE_INLINE bool IsFull(i8 c)  { return c >= 0; }
    static FORCE_INLINE bool IsDeleted(i8 c)  { return c == kDeleted; }
    static FORCE_INLINE bool IsEmptyOrDeleted(i8 c)  { return c < kSentinel; }

    static FORCE_INLINE u32 TrailingZeroesOfNZ(u64 uValue) {
        Assume_(uValue);
        return u32(GetPosOfLeastSignificantBitSet64(uValue));
    }

    template<typename T>
    static FORCE_INLINE u32 LeadingZeroes(T uValue) {
        static_assert(sizeof(T) <= sizeof(u64), "T too large");
        if (sizeof(T) <= sizeof(u16)) {
            return CountLeadingZeroes16(u16(uValue)) - (16 - sizeof(T)*8u);
        } else {
            if (sizeof(T) <= sizeof(u32))
                return CountLeadingZeroes32(u32(uValue)) - (32 - sizeof(T)*8u);
            else
                return CountLeadingZeroes64(u64(uValue)) - (64 - sizeof(T)*8u);
        }
    }
};

const i8 kEmptyGroup[16] = {
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
};

/*
const i8 kEmptyGroup[32] = {
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
    SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty, SetMetaDataImpl::kEmpty,
};
*/

// Returns a pointer to a control byte group that can be used by empty tables.
static FORCE_INLINE i8* EmptyGroup()  {
  // Const must be cast away here; no uses of this function will actually write
  // to it, because it is only used for empty tables.
  return const_cast<i8*>(kEmptyGroup);
}

#ifndef ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2
#if defined(__SSE2__) ||  \
    (defined(_MSC_VER) && (defined(_M_X64) || (defined(_M_IX86) && _M_IX86_FP >= 2)))
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2 1
#else
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2 0
#endif
#endif

#ifndef ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3
#ifdef __SSSE3__
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3 1
#else
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3 0
#endif
#endif

/*
#ifndef ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX
#if defined(__AVX__) || defined(__AVX2__) || \
    (defined(_MSC_VER) && (defined(__AVX__) || defined(__AVX2__)))
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX 1
#else
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX 0
#endif
#endif

#ifndef ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX2
#if defined(__AVX2__) || \
    (defined(_MSC_VER) && defined(__AVX2__))
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX2 1
#else
#define ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX2 0
#endif
#endif
*/

#if ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3 && \
    !ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2
#error "Bad configuration!"
#endif

#if ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2
#include <emmintrin.h>
#endif

#if ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3
#include <tmmintrin.h>
#endif

/*
#if ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX || ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX2
#include <immintrin.h>
#endif
*/

// An abstract bitmask, such as that emitted by a SIMD instruction.
//
// Specifically, this type implements a simple bitset whose representation is
// controlled by `SignificantBits` and `Shift`. `SignificantBits` is the number
// of abstract bits in the bitset, while `Shift` is the log-base-two of the
// width of an abstract bit in the representation.
// This mask provides operations for any number of real bits set in an abstract
// bit. To add iteration on top of that, implementation must guarantee no more
// than one real bit is set in an abstract bit.
template <class T, int SignificantBits, int Shift = 0>
class NonIterableBitMask {
 public:
  explicit NonIterableBitMask(T mask) : mask_(mask) {}

  explicit operator bool() const { return this->mask_ != 0; }

  // Returns the index of the lowest *abstract* bit set in `self`.
  u32 LowestBitSet() const {
    return SetMetaDataImpl::TrailingZeroesOfNZ(mask_) >> Shift;
  }

  // Returns the index of the highest *abstract* bit set in `self`.
  u32 HighestBitSet() const {
    return static_cast<uint32_t>((bit_width(mask_) - 1) >> Shift);
  }

  // Return the number of trailing zero *abstract* bits.
  u32 TrailingZeros() const {
    return SetMetaDataImpl::TrailingZeroesOfNZ(mask_) >> Shift;
  }

  // Return the number of leading zero *abstract* bits.
  u32 LeadingZeros() const {
    constexpr int total_significant_bits = SignificantBits << Shift;
    constexpr int extra_bits = sizeof(T) * 8 - total_significant_bits;
    return static_cast<u32>(SetMetaDataImpl::LeadingZeroes<T>(mask_ << extra_bits)) >> Shift;
  }

  T mask_;
};

// Mask that can be iterable
//
// For example, when `SignificantBits` is 16 and `Shift` is zero, this is just
// an ordinary 16-bit bitset occupying the low 16 bits of `mask`. When
// `SignificantBits` is 8 and `Shift` is 3, abstract bits are represented as
// the bytes `0x00` and `0x80`, and it occupies all 64 bits of the bitmask.
//
// For example:
//   for (int i : BitMask<uint32_t, 16>(0b101)) -> yields 0, 2
//   for (int i : BitMask<uint64_t, 8, 3>(0x0000000080800000)) -> yields 2, 3
template <class T, int SignificantBits, int Shift = 0>
class BitMask : public NonIterableBitMask<T, SignificantBits, Shift> {
  using Base = NonIterableBitMask<T, SignificantBits, Shift>;
  static_assert(Shift == 0 || Shift == 3, "");

 public:
  explicit BitMask(T mask) : Base(mask) {}
  // BitMask is an iterator over the indices of its abstract bits.
  using value_type = int;
  using iterator = BitMask;
  using const_iterator = BitMask;

  BitMask& operator++() {
    this->mask_ &= (this->mask_ - 1);
    return *this;
  }

  u32 operator*() const { return Base::LowestBitSet(); }

  BitMask begin() const { return *this; }
  BitMask end() const { return BitMask(0); }

 private:
  friend bool operator==(const BitMask& a, const BitMask& b) {
    return a.mask_ == b.mask_;
  }
  friend bool operator!=(const BitMask& a, const BitMask& b) {
    return a.mask_ != b.mask_;
  }
};

/*
#if ABSL_INTERNAL_RAW_HASH_SET_HAVE_AVX2
// https://github.com/abseil/abseil-cpp/issues/209
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87853
// _mm_cmpgt_epi8 is broken under GCC with -funsigned-char
// Work around this by using the portable implementation of Group
// when using -funsigned-char under GCC.
static FORCE_INLINE __m256i _mm256_cmpgt_epi8_fixed(__m256i a, __m256i b) {
#if defined(__GNUC__) && !defined(__clang__)
  if (std::is_unsigned<char>::value) {
    const __m256i mask = _mm256_set1_epi8(0x80);
    const __m256i diff = _mm256_subs_epi8(b, a);
    return _mm256_cmpeq_epi8(_mm256_and_si256(diff, mask), mask);
  }
#endif
  return _mm256_cmpgt_epi8(a, b);
}

struct GroupAvx2Impl {
  static const u32 kWidth = 32;  // the number of slots per group
  __m256i ctrl;

  explicit GroupAvx2Impl(const i8* pos) {
    ctrl = _mm256_loadu_si256(reinterpret_cast<const __m256i*>(pos));
  }

  // Returns a bitmask representing the positions of slots that match hash.
  FORCE_INLINE u32 Match(i8 hashH2) const  {
    auto match = _mm256_set1_epi8(hashH2);
    return u32(_mm256_movemask_epi8(_mm256_cmpeq_epi8(match, ctrl)));
  }

  // Returns a bitmask representing the positions of empty slots.
  FORCE_INLINE u32 MatchEmpty() const  {
    // This only works because ctrl_t::kEmpty is -128.
    return u32(_mm256_movemask_epi8(_mm256_sign_epi8(ctrl, ctrl)));
  }

  // Returns a bitmask representing the positions of empty or deleted slots.
  FORCE_INLINE u32 MatchEmptyOrDeleted() const  {
    auto special = _mm256_set1_epi8(SetMetaDataImpl::kSentinel);
    return u32(_mm256_movemask_epi8(_mm256_cmpgt_epi8_fixed(special, ctrl)));
  }

  // Returns the number of trailing empty or deleted elements in the group.
  FORCE_INLINE u32 CountLeadingEmptyOrDeleted() const  {
    auto special = _mm256_set1_epi8(SetMetaDataImpl::kSentinel);
    return CountTrailingZeroes32(u32(_mm256_movemask_epi8(_mm256_cmpgt_epi8_fixed(special, ctrl)) + 1));
  }

  void ConvertSpecialToEmptyAndFullToDeleted(i8* dst) const {
    auto msbs = _mm256_set1_epi8(i8(-128));
    auto x126 = _mm256_set1_epi8(i8(126));
    auto res = _mm256_or_si256(_mm256_shuffle_epi8(x126, ctrl), msbs);
    _mm256_storeu_si256(reinterpret_cast<__m256i*>(dst), res);
  }
};
#endif
*/

#if ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2
// Quick reference guide for intrinsics used below:
//
// * __m128i: An XMM (128-bit) word.
//
// * _mm_setzero_si128: Returns a zero vector.
// * _mm_set1_epi8:     Returns a vector with the same i8 in each lane.
//
// * _mm_subs_epi8:    Saturating-subtracts two i8 vectors.
// * _mm_and_si128:    Ands two i128s together.
// * _mm_or_si128:     Ors two i128s together.
// * _mm_andnot_si128: And-nots two i128s together.
//
// * _mm_cmpeq_epi8: Component-wise compares two i8 vectors for equality,
//                   filling each lane with 0x00 or 0xff.
// * _mm_cmpgt_epi8: Same as above, but using > rather than ==.
//
// * _mm_loadu_si128:  Performs an unaligned load of an i128.
// * _mm_storeu_si128: Performs an unaligned store of an i128.
//
// * _mm_sign_epi8:     Retains, negates, or zeroes each i8 lane of the first
//                      argument if the corresponding lane of the second
//                      argument is positive, negative, or zero, respectively.
// * _mm_movemask_epi8: Selects the sign bit out of each i8 lane and produces a
//                      bitmask consisting of those bits.
// * _mm_shuffle_epi8:  Selects i8s from the first argument, using the low
//                      four bits of each i8 lane in the second argument as
//                      indices.

// https://github.com/abseil/abseil-cpp/issues/209
// https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87853
// _mm_cmpgt_epi8 is broken under GCC with -funsigned-char
// Work around this by using the portable implementation of Group
// when using -funsigned-char under GCC.
inline __m128i _mm_cmpgt_epi8_fixed(__m128i a, __m128i b) {
#if defined(__GNUC__) && !defined(__clang__)
  if (std::is_unsigned<char>::value) {
    const __m128i mask = _mm_set1_epi8(0x80);
    const __m128i diff = _mm_subs_epi8(b, a);
    return _mm_cmpeq_epi8(_mm_and_si128(diff, mask), mask);
  }
#endif
  return _mm_cmpgt_epi8(a, b);
}

struct GroupSse2Impl {
  static constexpr u32 kWidth = 16;  // the number of slots per group

  explicit GroupSse2Impl(const u8* pos) {
    ctrl = _mm_loadu_si128(reinterpret_cast<const __m128i*>(pos));
  }

  // Returns a bitmask representing the positions of slots that match hash.
  BitMask<u32, kWidth> Match(u8 hash) const {
    auto match = _mm_set1_epi8(i8(hash));
    return BitMask<u32, kWidth>(u32(_mm_movemask_epi8(_mm_cmpeq_epi8(match, ctrl))));
  }

  // Returns a bitmask representing the positions of empty slots.
  NonIterableBitMask<u32, kWidth> MaskEmpty() const {
#ifdef ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3
    // This only works because ctrl_t::kEmpty is -128.
    return NonIterableBitMask<u32, kWidth>(u32(_mm_movemask_epi8(_mm_sign_epi8(ctrl, ctrl))));
#else
    auto match = _mm_set1_epi8(SetMetaDataImpl::kEmpty);
    return NonIterableBitMask<u32, kWidth>(u32(_mm_movemask_epi8(_mm_cmpeq_epi8(match, ctrl))));
#endif
  }

  // Returns a bitmask representing the positions of empty or deleted slots.
  NonIterableBitMask<u32, kWidth> MaskEmptyOrDeleted() const {
    auto special = _mm_set1_epi8(SetMetaDataImpl::kSentinel);
    return NonIterableBitMask<u32, kWidth>(u32(_mm_movemask_epi8(_mm_cmpgt_epi8_fixed(special, ctrl))));
  }

  // Returns the number of trailing empty or deleted elements in the group.
  u32 CountLeadingEmptyOrDeleted() const {
    auto special = _mm_set1_epi8(SetMetaDataImpl::kSentinel);
    return SetMetaDataImpl::TrailingZeroesOfNZ(u32(_mm_movemask_epi8(_mm_cmpgt_epi8_fixed(special, ctrl)) + 1));
  }

  void ConvertSpecialToEmptyAndFullToDeleted(u8* dst) const {
    auto msbs = _mm_set1_epi8(i8(-128));
    auto x126 = _mm_set1_epi8(126);
#ifdef ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSSE3
    auto res = _mm_or_si128(_mm_shuffle_epi8(x126, ctrl), msbs);
#else
    auto zero = _mm_setzero_si128();
    auto special_mask = _mm_cmpgt_epi8_fixed(zero, ctrl);
    auto res = _mm_or_si128(msbs, _mm_andnot_si128(special_mask, x126));
#endif
    _mm_storeu_si128(reinterpret_cast<__m128i*>(dst), res);
  }

  __m128i ctrl;
};
#endif  // ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2

/*
#if defined(ABSL_INTERNAL_HAVE_ARM_NEON) && defined(ABSL_IS_LITTLE_ENDIAN)
struct GroupAArch64Impl {
  static constexpr u32 kWidth = 8;

  explicit GroupAArch64Impl(const ctrl_t* pos) {
    ctrl = vld1_u8(reinterpret_cast<const uint8_t*>(pos));
  }

  BitMask<uint64_t, kWidth, 3> Match(h2_t hash) const {
    uint8x8_t dup = vdup_n_u8(hash);
    auto mask = vceq_u8(ctrl, dup);
    constexpr uint64_t msbs = 0x8080808080808080ULL;
    return BitMask<uint64_t, kWidth, 3>(
        vget_lane_u64(vreinterpret_u64_u8(mask), 0) & msbs);
  }

  NonIterableBitMask<uint64_t, kWidth, 3> MaskEmpty() const {
    uint64_t mask =
        vget_lane_u64(vreinterpret_u64_u8(
                          vceq_s8(vdup_n_s8(static_cast<h2_t>(ctrl_t::kEmpty)),
                                  vreinterpret_s8_u8(ctrl))),
                      0);
    return NonIterableBitMask<uint64_t, kWidth, 3>(mask);
  }

  NonIterableBitMask<uint64_t, kWidth, 3> MaskEmptyOrDeleted() const {
    uint64_t mask =
        vget_lane_u64(vreinterpret_u64_u8(vcgt_s8(
                          vdup_n_s8(static_cast<int8_t>(ctrl_t::kSentinel)),
                          vreinterpret_s8_u8(ctrl))),
                      0);
    return NonIterableBitMask<uint64_t, kWidth, 3>(mask);
  }

  uint32_t CountLeadingEmptyOrDeleted() const {
    uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(ctrl), 0);
    // ctrl | ~(ctrl >> 7) will have the lowest bit set to zero for kEmpty and
    // kDeleted. We lower all other bits and count number of trailing zeros.
    // Clang and GCC optimize countr_zero to rbit+clz without any check for 0,
    // so we should be fine.
    constexpr uint64_t bits = 0x0101010101010101ULL;
    return countr_zero((mask | ~(mask >> 7)) & bits) >> 3;
  }

  void ConvertSpecialToEmptyAndFullToDeleted(ctrl_t* dst) const {
    uint64_t mask = vget_lane_u64(vreinterpret_u64_u8(ctrl), 0);
    constexpr uint64_t msbs = 0x8080808080808080ULL;
    constexpr uint64_t lsbs = 0x0101010101010101ULL;
    auto x = mask & msbs;
    auto res = (~x + (x >> 7)) & ~lsbs;
    little_endian::Store64(dst, res);
  }

  uint8x8_t ctrl;
};
#endif  // ABSL_INTERNAL_HAVE_ARM_NEON && ABSL_IS_LITTLE_ENDIAN
*/

struct GroupPortableImpl {
  static const u32 kWidth = 8;

  static const u64 msbs = 0x8080808080808080ULL;
  static const u64 lsbs = 0x0101010101010101ULL;

  explicit GroupPortableImpl(const u8* pos) {
      memcpy(&ctrl, pos, 8u);
  }

  BitMask<u64, kWidth, 3> Match(u8 hash) const {
    // For the technique, see:
    // http://graphics.stanford.edu/~seander/bithacks.html##ValueInWord
    // (Determine if a word has a byte equal to n).
    //
    // Caveat: there are false positives but:
    // - they only occur if there is a real match
    // - they never occur on ctrl_t::kEmpty, ctrl_t::kDeleted, ctrl_t::kSentinel
    // - they will be handled gracefully by subsequent checks in code
    //
    // Example:
    //   v = 0x1716151413121110
    //   hash = 0x12
    //   retval = (v - lsbs) & ~v & msbs = 0x0000000080800000
    u64 x = ctrl ^ (lsbs * hash);
    return BitMask<u64, kWidth, 3>((x - lsbs) & ~x & msbs);
  }

  NonIterableBitMask<u64, kWidth, 3> MaskEmpty() const {
    return NonIterableBitMask<u64, kWidth, 3>((ctrl & (~ctrl << 6)) & msbs);
  }

  NonIterableBitMask<u64, kWidth, 3> MaskEmptyOrDeleted() const {
    return NonIterableBitMask<u64, kWidth, 3>((ctrl & (~ctrl << 7)) & msbs);
  }

  u64 CountLeadingEmptyOrDeleted() const {
    // ctrl | ~(ctrl >> 7) will have the lowest bit set to zero for kEmpty and
    // kDeleted. We lower all other bits and count number of trailing zeros.
    return SetMetaDataImpl::TrailingZeroesOfNZ((ctrl | ~(ctrl >> 7)) & lsbs) >> 3;
  }

  void ConvertSpecialToEmptyAndFullToDeleted(u8* dst) const {
    u64 x = ctrl & msbs;
    u64 res = (~x + (x >> 7)) & ~lsbs;
    memcpy(dst, &res, 8u);
  }

  u64 ctrl;
};

#ifdef ABSL_INTERNAL_RAW_HASH_SET_HAVE_SSE2
using Group = GroupSse2Impl;

//#elif defined(ABSL_INTERNAL_HAVE_ARM_NEON) && defined(ABSL_IS_LITTLE_ENDIAN)
//using Group = GroupAArch64Impl;

#else
using Group = GroupPortableImpl;

#endif


struct HashSet_Impl {

    u8* _ctrl;

    FORCE_INLINE u32 non_null_capacity() const { return *reinterpret_cast<const u32*>(_ctrl - 4); }
    FORCE_INLINE u32 non_null_size() const { return *reinterpret_cast<const u32*>(_ctrl - 8); }
    FORCE_INLINE u32 non_null_growth_left() const { return *reinterpret_cast<const u32*>(_ctrl - 12); }
    FORCE_INLINE u8* non_null_keys() { return *reinterpret_cast<u8**>(_ctrl - 20); }
    FORCE_INLINE u8* non_null_values() { return *reinterpret_cast<u8**>(_ctrl - 28); }
    FORCE_INLINE const u8* non_null_keys() const { return *reinterpret_cast<const u8**>(_ctrl - 20); }
    FORCE_INLINE const u8* non_null_values() const { return *reinterpret_cast<const u8**>(_ctrl - 28); }

    FORCE_INLINE u32 capacity() const { if (_ctrl) return non_null_capacity(); return 0u; }
    FORCE_INLINE u32 size() const { if (_ctrl) return non_null_size(); return 0u; }
    FORCE_INLINE u32 growth_left() const { if (_ctrl) return non_null_growth_left(); return 0u; }

    FORCE_INLINE u32& capacity_lhv() { return *reinterpret_cast<u32*>(_ctrl - 4); }
    FORCE_INLINE u32& size_lhv() { return *reinterpret_cast<u32*>(_ctrl - 8); }
    FORCE_INLINE u32& growth_left_lhv() { return *reinterpret_cast<u32*>(_ctrl - 12); }
    FORCE_INLINE u8*& keys_lhv() { return *reinterpret_cast<u8**>(_ctrl - 20); }
    FORCE_INLINE u8*& values_lhv() { return *reinterpret_cast<u8**>(_ctrl - 28); }

    // Returns he number of "cloned control bytes".
    //
    // This is the number of control bytes that are present both at the beginning
    // of the control byte array and at the end, such that we can create a
    // `Group::kWidth`-width probe window starting from any control byte.
    static constexpr FORCE_INLINE size_t NumClonedBytes() { return Group::kWidth - 1; }

    // Returns whether `n` is a valid capacity (i.e., number of slots).
    //
    // A valid capacity is a non-zero integer `2^m - 1`.
    static constexpr FORCE_INLINE bool IsValidCapacity(u32 n) { return ((n + 1) & n) == 0 && n > 0; }

    // Converts `n` into the next valid capacity, per `IsValidCapacity`.
    static constexpr FORCE_INLINE u32 NormalizeCapacity(u32 n) {
        return n ? 0xFFFFFFFFu >> CountLeadingZeroes32(n) : 1u;
    }

    // Extracts the H1 portion of a hash (its 57 MSB)
    static constexpr FORCE_INLINE u64 H1(u64 hash)  {
      return (hash >> 7);
    }

    // Extracts the H2 portion of a hash (its 7 LSB)
    static constexpr FORCE_INLINE i8 H2(u64 hash)  {
      return i8(hash & 0x7F);
    }

    // The state for a probe sequence.
    //
    // Currently, the sequence is a triangular progression of the form
    //
    //   p(i) := Width * (i^2 + i)/2 + hash (mod mask + 1)
    //
    // The use of `Width` ensures that each probe step does not overlap groups;
    // the sequence effectively outputs the addresses of *groups* (although not
    // necessarily aligned to any boundary). The `Group` machinery allows us
    // to check an entire group with minimal branching.
    //
    // Wrapping around at `mask + 1` is important, but not for the obvious reason.
    // As described above, the first few entries of the control byte array
    // are mirrored at the end of the array, which `Group` will find and use
    // for selecting candidates. However, when those candidates' slots are
    // actually inspected, there are no corresponding slots for the cloned bytes,
    // so we need to make sure we've treated those offsets as "wrapping around".
    //
    // It turns out that this probe sequence visits every group exactly once if the
    // number of groups is a power of two, since (i^2+i)/2 is a bijection in
    // Z/(2^m). See https://en.wikipedia.org/wiki/Quadratic_probing
    struct probe_seq {
        // Creates a new probe sequence using `hashH1` as the initial value of the
        // sequence and `mask` (usually the capacity of the table) as the mask to
        // apply to each value in the progression.
        FORCE_INLINE probe_seq(u64 hashH1, u32 mask) {
            Assert(((mask + 1) & mask) == 0, "not a mask");
            mask_ = mask;
            offset_ = u32(hashH1) & mask_;
            index_ = 0;
        }

        // The offset within the table, i.e., the value `p(i)` above.
        FORCE_INLINE u32 offset() const { return offset_; }
        FORCE_INLINE u32 offset(u32 i) const { return (offset_ + i) & mask_; }

        FORCE_INLINE void next() {
            index_ += Group::kWidth;
            offset_ += index_;
            offset_ &= mask_;
        }

        // 0-based probe index, a multiple of `Width`.
        FORCE_INLINE u32 index() const { return index_; }

    private:
        u32 mask_;
        u32 offset_;
        u32 index_;
    };

    // General notes on capacity/growth methods below:
    // - We use 7/8th as maximum load factor. For 16-wide groups, that gives an
    //   average of two empty slots per group.
    // - For (capacity+1) >= Group::kWidth, growth is 7/8*capacity.
    // - For (capacity+1) < Group::kWidth, growth == capacity. In this case, we
    //   never need to probe (the whole table fits in one group) so we don't need a
    //   load factor less than 1.

    // Given `capacity`, applies the load factor; i.e., it returns the maximum
    // number of values we should put into the table before a resizing rehash.
    static constexpr FORCE_INLINE u32 CapacityToGrowth(u32 n)  {
        Assert_(IsValidCapacity(n));
        // `capacity*7/8`
        if (Group::kWidth == 8 && n == 7) {
            // x-x/8 does not work when x==7.
            return 6;
        }
        return n - n / 8;
    }

    // Given `growth`, "unapplies" the load factor to find how large the capacity
    // should be to stay within the load factor.
    //
    // This might not be a valid capacity and `NormalizeCapacity()` should be
    // called on this.
    static constexpr FORCE_INLINE u32 GrowthToLowerboundCapacity(u32 growth)  {
        // `growth*8/7`
        if (Group::kWidth == 8 && growth == 7) {
            // x+(x-1)/7 does not work when x==7.
            return 8;
        }
        return growth + (growth - 1) / 7;
    }

    // Begins a probing operation on `ctrl`, using `hash`.
    inline probe_seq non_null_probe(u64 hash) {
        return probe_seq(H1(hash), non_null_capacity());
    }

    // Probes an array of control bits using a probe sequence derived from `hash`,
    // and returns the offset corresponding to the first deleted or empty slot.
    //
    // Behavior when the entire table is full is undefined.
    //
    // NOTE: this function must work with tables having both empty and deleted
    // slots in the same group. Such tables appear during `erase()`.
    u32 non_null_find_first_non_full(u64 hash) const {
        return find_first_non_full(_ctrl, H1(hash), non_null_capacity());
    }

    static FORCE_INLINE u32 find_first_non_full(u8* controlBytes, u64 hashH1, u32 capacity) {
        auto seq = probe_seq(hashH1, capacity);
        while (true) {
            Group g{controlBytes + seq.offset()};
            auto mask = g.MaskEmptyOrDeleted();
            if (mask) {
                return seq.offset(mask.LowestBitSet());
            }
            seq.next();
            Assert(seq.index() <= capacity, "Full table!");
        }
    }

    template<typename T>
    FORCE_INLINE void slot_assign_iff_non_void(T* table, u32 uDest, u32 uSrc) { table[uDest] = table[uSrc]; };
    template<>
    FORCE_INLINE void slot_assign_iff_non_void<void>(void* table, u32 uDest, u32 uSrc) {};

    template<typename T>
    FORCE_INLINE void slot_swap_iff_non_void(T* table, u32 uFirst, u32 uSecond) {
        T tmp = table[uFirst];
        table[uFirst] = table[uSecond];
        table[uSecond] = tmp;
    };
    template<>
    FORCE_INLINE void slot_swap_iff_non_void<void>(void* table, u32 uDest, u32 uSrc) {};

    // Prunes control bytes to remove as many tombstones as possible.
    template<typename KeyT, typename KeyCheckT, typename ValueT = void>
    void non_null_drop_deletes_without_resize(KeyT* tKeys, ValueT* tValues = 0) {
        Assert_(_ctrl);
        u8* controlBytes = _ctrl;
        u32 capacity = non_null_capacity();
        Assert_(HashSet_Impl::IsValidCapacity(capacity));
        Assert_(capacity > Group::kWidth);
        // Algorithm:
        // - mark all DELETED slots as EMPTY
        // - mark all FULL slots as DELETED
        // - for each slot marked as DELETED
        //     hash = Hash(element)
        //     target = find_first_non_full(hash)
        //     if target is in the same group
        //       mark slot as FULL
        //     else if target is EMPTY
        //       transfer element to target
        //       mark slot as EMPTY
        //       mark target as FULL
        //     else if target is DELETED
        //       swap current element with target element
        //       mark target as FULL
        //       repeat procedure for current slot with moved from element (target)
        
        // ConvertDeletedToEmptyAndFullToDeleted :
        // Applies the following mapping to every byte in the control array:
        //   * kDeleted -> kEmpty
        //   * kEmpty -> kEmpty
        //   * _ -> kDeleted
        // PRECONDITION:
        //   IsValidCapacity(capacity)
        //   ctrl[capacity] == ctrl_t::kSentinel
        //   ctrl[i] != ctrl_t::kSentinel for all i < capacity
        Assert_(controlBytes[capacity] == u8(SetMetaDataImpl::kSentinel));
        u8* pEnd = controlBytes + capacity;
        for (u8* pCtrl = controlBytes; pCtrl < pEnd; pCtrl += Group::kWidth) {
            Group{pCtrl}.ConvertSpecialToEmptyAndFullToDeleted(pCtrl);
        }
        // Copy the cloned ctrl bytes.
        memcpy(pEnd + 1, controlBytes, NumClonedBytes());
        controlBytes[capacity] = u8(SetMetaDataImpl::kSentinel);
        
#define PROBE_INDEX(pos)   (((pos - probe_offset) & capacity) / Group::kWidth)
        for (u32 i = 0; i < capacity; ++i) {
            if (!SetMetaDataImpl::IsDeleted(controlBytes[i])) continue;
            const u64 hash = get_map_hash((KeyCheckT)tKeys[i]);
            const u64 hashH1 = H1(hash);
            const u32 probe_offset = probe_seq(hashH1, capacity).offset();
            u32 probe_length;
            u32 new_i = find_first_non_full(controlBytes, hashH1, capacity);
            const u8 h2 = H2(hash);
            if (LIKELY(PROBE_INDEX(new_i) == PROBE_INDEX(i))) {
                // if same group index as previously relative to its hash, we can in fact consider it didn't need to move at all.
                set_ctrl(controlBytes, i, h2, capacity); // => reassign it to same slot and dont bother swapping keys (and possibly values)
                continue;
            } 
            if (SetMetaDataImpl::IsEmpty(controlBytes[new_i])) {
                // Transfer element to the empty spot.
                set_ctrl(controlBytes, new_i, h2, capacity);
                tKeys[new_i] = tKeys[i];
                slot_assign_iff_non_void(tValues, new_i, i);
                set_ctrl(controlBytes, i, u8(SetMetaDataImpl::kEmpty), capacity);
            } else {
                Assert_(SetMetaDataImpl::IsDeleted(controlBytes[new_i]));
                set_ctrl(controlBytes, new_i, h2, capacity);
                // Until we are done rehashing, DELETED marks previously FULL slots
                // swap i and new_i elements
                KeyT tmpKey = tKeys[new_i];
                tKeys[new_i] = tKeys[i];
                tKeys[i] = tmpKey;
                slot_swap_iff_non_void(tValues, new_i, i);
                --i; // repeat
            }
        }
        growth_left_lhv() = CapacityToGrowth(capacity) - non_null_size();
    }

    // Sets `ctrl` to `{kEmpty, kSentinel, ..., kEmpty}`, marking the entire
    // array as marked as empty.
    FORCE_INLINE void non_null_reset() {
        const u32 capacity = non_null_capacity();
        memset(_ctrl, u8(SetMetaDataImpl::kEmpty), capacity + 1 + NumClonedBytes());
        _ctrl[capacity] = u8(SetMetaDataImpl::kSentinel);
        size_lhv() = 0u;
        growth_left_lhv() = CapacityToGrowth(capacity);
    }

    // Sets `ctrl[i]` to `h`.
    //
    // Unlike setting it directly, this function will perform bounds checks and
    // mirror the value to the cloned tail if necessary.
    static FORCE_INLINE void set_ctrl(u8* controlBytes, u32 i, u8 h, u32 capacity) {
        Assert_(i < capacity);
        controlBytes[i] = h;
        controlBytes[((i - NumClonedBytes()) & capacity) + (NumClonedBytes() & capacity)] = h;
    }

    // Sets `ctrl[i]` to `h`.
    //
    // Unlike setting it directly, this function will perform bounds checks and
    // mirror the value to the cloned tail if necessary.
    FORCE_INLINE void non_null_set_ctrl(u32 i, u8 h) {
        set_ctrl(_ctrl, i, h, non_null_capacity());
    }

    FORCE_INLINE void non_null_set_ctrl_from_hash(u32 i, u64 hash) { set_ctrl(_ctrl, i, H2(hash), non_null_capacity()); }

    /*
    // Given the capacity of a table, computes the offset (from the start of the
    //   backing allocation) at which the slots begin.
    FORCE_INLINE size_t getSlotOffset(u16 slot_align) const {
        const size_t num_control_bytes = size_t(non_null_capacity()) + 1u + NumClonedBytes();
        const size_t align_mask = size_t(slot_align) - 1u;
        return (num_control_bytes + align_mask) & align_mask;
    }
    */

    /*
    // Given the capacity of a table, computes the total size of the backing array.
    FORCE_INLINE size_t getAllocSize(size_t slot_size, u16 slot_align) {
        return getSlotOffset(slot_align) + size_t(capacity) * slot_size;
    }
    */

    template<typename KeyT, typename KeyQueryT, typename EqFn>
    FORCE_INLINE u32 non_null_find_hash_and_key_index(u64 hashOfKeyToFind, const KeyQueryT& keyToFind, EqFn areEqual, const KeyT* tKeyValues) const {
        const u32 capacity = non_null_capacity();
        auto seq = probe_seq(H1(hashOfKeyToFind), capacity);
        u8 h2 = H2(hashOfKeyToFind);
        while (true) {
            Group g{_ctrl + seq.offset()};
            for (u32 i : g.Match(h2)) {
                u32 uMatchIndex = seq.offset(i);
                if (LIKELY(areEqual(tKeyValues[uMatchIndex], keyToFind)))
                    return uMatchIndex;
            }
            if (LIKELY(g.MaskEmpty()))
                return capacity; // marker for end
            seq.next();
            Assert(seq.index() <= capacity, "Full table!");
        }
    }
};

/*
    template<typename KeyT>
    void insertNotPresent(u64 hashOfKeyToInsert, const KeyT& keyToInsert, KeyT* tKeyValues) {
        //const KeyT* keyValues = reinterpret_cast<const KeyT*>(ctrl + getSlotOffset(alignof(KeyT)));
        static_assert(std::is_trivially_copyable<KeyT>::value,
            "HashSet_Impl only allows trivially copyable keys. fall back to std::unordered_set for fancier content");
        auto seq = probe(ctrl, hashOfKeyToInsert, capacity);
        while (true) {
            Group g{ctrl + seq.offset()};
            for (u32 i : g.Match(H2(hash))) {
                u32 uMatchIndex = seq.offset(i);
                if (LIKELY(areEqual(keyValues[uMatchIndex], keyToFind)))
                    return uMatchIndex;
            }
            if (LIKELY(g.MaskEmpty()))
                return capacity; // marker for end
            seq.next();
            Assert(seq.index() <= capacity, "Full table!");
        }
    }
*/

template<typename KeyT>
FORCE_INLINE u64 get_map_hash(KeyT key) {
    if (sizeof(KeyT) == 1u) {
        return getHashForUpTo64bValue(u64(reinterpret_cast<const u8&>(key)), MAP_HASH_SEED);
    } else if (sizeof(KeyT) == 2u && alignof(KeyT) >= 2u)  {
        return getHashForUpTo64bValue(u64(reinterpret_cast<const u16&>(key)), MAP_HASH_SEED);
    } else if (sizeof(KeyT) == 4u && alignof(KeyT) >= 4u) {
        return getHashForUpTo64bValue(u64(reinterpret_cast<const u32&>(key)), MAP_HASH_SEED);
    } else if (sizeof(KeyT) == 8u && alignof(KeyT) >= 8u) {
        return getHashForUpTo64bValue(u64(reinterpret_cast<const u32&>(key)), MAP_HASH_SEED);
    } else if (sizeof(KeyT) < 32u) {
        return getHashForOtherValue(key, MAP_HASH_SEED);
    } else {
        return getFastHashFor32BytesValueOrLarger(key, MAP_HASH_SEED);
    }
}
; // template termination

template<>
FORCE_INLINE u64 get_map_hash<StringView>(StringView key) {
    return getFastHashFromString(key, MAP_HASH_SEED);
}
; // template termination

template<>
FORCE_INLINE u64 get_map_hash<FFString>(FFString key) {
    return getFastHashFromString((StringView)key, MAP_HASH_SEED);
}
; // template termination

template<>
FORCE_INLINE u64 get_map_hash<OwnedString>(OwnedString key) {
    return getFastHashFromString((StringView)key, MAP_HASH_SEED);
}
; // template termination

template<typename KeyT, typename KeyT2 = KeyT>
FORCE_INLINE bool are_equal_keys(KeyT k1, KeyT2 k2) {
    return k1 == (KeyT)k2;
}
; // template termination

template<>
FORCE_INLINE bool are_equal_keys<StringView, StringView>(StringView k1, StringView k2) {
    if (k1.is_null())
        return k2.is_null();
    if (k2.is_null())
        return false;
    return areNonNullStringEqual(k1, k2);
}
; // template termination
template<> FORCE_INLINE bool are_equal_keys<FFString, StringView>(FFString k1, StringView k2) { return are_equal_keys((StringView)k1, k2); };
template<> FORCE_INLINE bool are_equal_keys<OwnedString, StringView>(OwnedString k1, StringView k2) { return are_equal_keys((StringView)k1, k2); };
template<> FORCE_INLINE bool are_equal_keys<StringView, FFString>(StringView k1, FFString k2) { return are_equal_keys(k1, (StringView)k2); };
template<> FORCE_INLINE bool are_equal_keys<FFString, FFString>(FFString k1, FFString k2) { return are_equal_keys((StringView)k1, (StringView)k2); };
template<> FORCE_INLINE bool are_equal_keys<OwnedString, FFString>(OwnedString k1, FFString k2) { return are_equal_keys((StringView)k1, (StringView)k2); };
template<> FORCE_INLINE bool are_equal_keys<StringView, OwnedString>(StringView k1, OwnedString k2) { return are_equal_keys(k1, (StringView)k2); };
template<> FORCE_INLINE bool are_equal_keys<FFString, OwnedString>(FFString k1, OwnedString k2) { return are_equal_keys((StringView)k1, (StringView)k2); };
template<> FORCE_INLINE bool are_equal_keys<OwnedString, OwnedString>(OwnedString k1, OwnedString k2) { return are_equal_keys((StringView)k1, (StringView)k2); };

template<typename KeyT>
struct MapCheckType {
    typedef KeyT value_t;
};

template<>
struct MapCheckType<FFString> {
    typedef StringView value_t;
};

template<>
struct MapCheckType<OwnedString> {
    typedef StringView value_t;
};

template<typename KeyT>
struct SetView {
    static_assert(std::is_trivially_copyable<KeyT>::value,
                  "Set only allows trivially copyable keys. Fall back to std::unordered_set for fancy content");
    typedef typename MapCheckType<KeyT>::value_t KeyCheckT;
    static_assert(std::is_convertible<KeyT, KeyCheckT>::value,
                  "Set KeyT must be convertible to KeyCheckT");
protected:
    HashSet_Impl _impl;
public:

    SetView() = default;
    SetView(const SetView<KeyT>&) = default;
    SetView<KeyT>& operator=(const SetView<KeyT>&) = default;
    ~SetView() = default;

    FORCE_INLINE bool is_empty() const { return _impl._ctrl == 0 || _impl.non_null_size() == 0; };
    FORCE_INLINE u32 size() const { return _impl.size(); };
    FORCE_INLINE u32 current_capacity() const { return _impl.capacity(); };
    FORCE_INLINE u32 current_growth_left() const { return _impl.growth_left(); };

    struct iterator {
        iterator() = default;
        iterator(const SetView<KeyT>::iterator&) = default;
        SetView<KeyT>::iterator& operator=(const SetView<KeyT>::iterator&) = default;
        ~iterator() = default;
        FORCE_INLINE bool operator==(const SetView<KeyT>::iterator& other) const { return this->ctrl_ == other.ctrl_; }
        FORCE_INLINE bool operator!=(const SetView<KeyT>::iterator& other) const { return !operator==(other); }
        FORCE_INLINE KeyT& operator*() {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "* called on invalid set iterator");
            return *pKey_;
        }
        FORCE_INLINE const KeyT& operator*() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "* called on invalid set iterator");
            return *pKey_;
        }
        FORCE_INLINE KeyT* operator->() {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "-> called on invalid set iterator");
            return pKey_;
        }
        FORCE_INLINE const KeyT* operator->() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "-> called on invalid set iterator");
            return pKey_;
        }
        FORCE_INLINE SetView<KeyT>::iterator& operator++() {
          Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "++ called on invalid set iterator");
          ++ctrl_; ++pKey_; skip_empty_or_deleted_and_check_end();
          return *this;
        }
        FORCE_INLINE SetView<KeyT>::iterator operator++(int) { auto tmp = *this; ++*this; return tmp; }
        FORCE_INLINE u32 _getIndex_from(HashSet_Impl impl) {
          Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "_getIndex_from() called on invalid set iterator");
          Assert_(impl._ctrl);
          Assert_(ctrl_ >= impl._ctrl && ctrl_ < impl._ctrl + impl.non_null_capacity());
          return u32(ctrl_ - impl._ctrl);
        }
    private:
        const u8* ctrl_;
        KeyT* pKey_;
        FORCE_INLINE iterator(const u8* ctrl, KeyT* pKey):ctrl_(ctrl), pKey_(pKey) {}
        void skip_empty_or_deleted_and_check_end() {
            while (SetMetaDataImpl::IsEmptyOrDeleted(*ctrl_)) {
                u32 shift = Group{ctrl_}.CountLeadingEmptyOrDeleted();
                ctrl_ += shift;
                pKey_ += shift;
            }
            if (UNLIKELY(*ctrl_ == u8(SetMetaDataImpl::kSentinel))) {
                ctrl_ = 0;
                pKey_ = 0;
            }
        }
        friend struct SetView<KeyT>;
        friend struct SetView<KeyT>::const_iterator;
        template<typename KT, typename AllocT> friend struct ASet;
        template<typename AllocT> friend struct AStringSet;
    };

    struct const_iterator {
        const_iterator() = default;
        const_iterator(const SetView<KeyT>::const_iterator&) = default;
        SetView<KeyT>::const_iterator& operator=(const SetView<KeyT>::const_iterator&) = default;
        ~const_iterator() = default;
        FORCE_INLINE const_iterator(const SetView<KeyT>::iterator& fromNonConst):ctrl_(fromNonConst.ctrl_), pKey_(fromNonConst.pKey_) {}
        FORCE_INLINE SetView<KeyT>::const_iterator& operator=(const SetView<KeyT>::iterator& fromNonConst) { return operator=(SetView<KeyT>::const_iterator(fromNonConst)); }

        FORCE_INLINE bool operator==(const SetView<KeyT>::const_iterator& other) const { return this->ctrl_ == other.ctrl_; }
        FORCE_INLINE bool operator!=(const SetView<KeyT>::const_iterator& other) const { return !operator==(other); }
        FORCE_INLINE bool operator==(const SetView<KeyT>::iterator& other) const { return this->ctrl_ == other.ctrl_; }
        FORCE_INLINE bool operator!=(const SetView<KeyT>::iterator& other) const { return !operator==(other); }

        FORCE_INLINE const KeyT& operator*() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "* called on invalid set iterator");
            return *pKey_;
        }
        FORCE_INLINE const KeyT* operator->() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "-> called on invalid set iterator");
            return pKey_;
        }
        FORCE_INLINE SetView<KeyT>::const_iterator& operator++() {
          Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "++ called on invalid set iterator");
          ++ctrl_; ++pKey_; skip_empty_or_deleted_and_check_end();
          return *this;
        }
        FORCE_INLINE SetView<KeyT>::const_iterator operator++(int) { auto tmp = *this; ++*this; return tmp; }

    private:
        const u8* ctrl_;
        const KeyT* pKey_;
        FORCE_INLINE const_iterator(const u8* ctrl, const KeyT* pKey):ctrl_(ctrl), pKey_(pKey) {}
        void skip_empty_or_deleted_and_check_end() {
            while (SetMetaDataImpl::IsEmptyOrDeleted(*ctrl_)) {
                u32 shift = Group{ctrl_}.CountLeadingEmptyOrDeleted();
                ctrl_ += shift;
                pKey_ += shift;
            }
            if (UNLIKELY(*ctrl_ == u8(SetMetaDataImpl::kSentinel))) {
                ctrl_ = 0;
                pKey_ = 0;
            }
        }
        friend struct SetView<KeyT>;
        template<typename KT, typename AllocT> friend struct ASet;
        template<typename AllocT> friend struct AStringSet;
    };

    FORCE_INLINE iterator begin() {
        if (LIKELY(_impl._ctrl)) {
            iterator it(_impl._ctrl, _get_table_keys());
            it.skip_empty_or_deleted_and_check_end();
            return it;
        }
        return end();
    }
    FORCE_INLINE iterator end() const { return iterator(0, 0); }
    FORCE_INLINE const_iterator cbegin() const {
        if (LIKELY(_impl._ctrl)) {
            const_iterator it(_impl._ctrl, _get_const_table_keys());
            it.skip_empty_or_deleted_and_check_end();
            return it;
        }
        return end();
    }
    FORCE_INLINE const_iterator cend() const { return const_iterator(0, 0); }

    iterator findHashed(u64 uHashOfKeyToFind, const KeyCheckT& keyToFind) {
        if (LIKELY(_impl._ctrl)) {
            const u32 capacity = _impl.non_null_capacity();
            KeyT* tKeys = _get_table_keys();
            u32 uIndex = _impl.non_null_find_hash_and_key_index(uHashOfKeyToFind, keyToFind, are_equal_keys<KeyT, KeyCheckT>, tKeys);
            if (uIndex < capacity) {
                return iterator(_impl._ctrl + uIndex, tKeys + uIndex);
            }
        }
        return end();
    }

    const_iterator findHashed(u64 uHashOfKeyToFind, const KeyCheckT& keyToFind) const {
        if (LIKELY(_impl._ctrl)) {
            const u32 capacity = _impl.non_null_capacity();
            const KeyT* tKeys = _get_const_table_keys();
            u32 uIndex = _impl.non_null_find_hash_and_key_index(uHashOfKeyToFind, keyToFind, are_equal_keys<KeyT, KeyCheckT>, tKeys);
            if (uIndex < capacity) {
                return const_iterator(_impl.ctrl + uIndex, tKeys + uIndex);
            }
        }
        return end();
    }

    FORCE_INLINE iterator find(const KeyCheckT& keyToFind) {
        u64 uHash = get_map_hash(keyToFind);
        return findHashed(uHash, keyToFind);
    }

    FORCE_INLINE const_iterator find(const KeyCheckT& keyToFind) const {
        u64 uHash = get_map_hash(keyToFind);
        return findHashed(uHash, keyToFind);
    }

    FORCE_INLINE u32 _non_null_size() const { Assert_(_impl._ctrl); return _impl.non_null_size(); }

protected:
    FORCE_INLINE KeyT* _get_table_keys() { Assert_(_impl._ctrl); return reinterpret_cast<KeyT*>(_impl.non_null_keys()); };
    FORCE_INLINE const KeyT* _get_const_table_keys() const { Assert_(_impl._ctrl); return reinterpret_cast<const KeyT*>(_impl.non_null_keys()); };
};

template<typename KeyT, typename AllocT>
struct ASet : public SetView<KeyT> {

    typedef typename SetView<KeyT>::iterator iterator;
    typedef typename SetView<KeyT>::const_iterator const_iterator;

    typedef typename SetView<KeyT>::KeyCheckT KeyCheckT;

    ASet() = default;
    ASet(const ASet<KeyT, AllocT>&) = default;
    ASet<KeyT, AllocT>& operator=(const ASet<KeyT, AllocT>&) = default;
    ~ASet() = default;

    AllocT _alloc;

    FORCE_INLINE ASet(AllocT alloc, u32 uInitialCapacity):_impl(), _alloc(alloc) { _impl = {}; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    FORCE_INLINE ASet(AllocT alloc):_impl(), _alloc(alloc) { _impl = {}; }

    FORCE_INLINE AllocT alloc() const { return _alloc; }
    FORCE_INLINE AllocT& alloc_ref() { return _alloc; }

    void init(AllocT alloc) { _impl = {}; _alloc = alloc; }
    void init(AllocT alloc, u32 uInitialCapacity) { _impl = {}; _alloc = alloc; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    template<typename OtherKeyT> void init(AllocT alloc, SetView<OtherKeyT> viewToCopy) {
        _impl = {}; _alloc = alloc;
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._non_null_size()));
            _allocate(uRequiredCapacity);
            _deep_copy_when_sized(viewToCopy);
        }
    };

    template<typename = void> void init() {
        static_assert(AllocT::has_default_instance, "non-specific ASet::init() called with allocator not having default");
        init(AllocT::getDefaultInstance());
    };
    template<typename = void> void init(u32 uInitialCapacity) {
        static_assert(AllocT::has_default_instance, "non-specific ASet::init() called with allocator not having default");
        init(AllocT::getDefaultInstance(), uInitialCapacity);
    };

    template<typename OtherKeyT> void _deep_copy_when_sized(SetView<OtherKeyT> viewToCopy) {
        static_assert(std::is_convertible<OtherKeyT, KeyT>::value, "cannot call 'set::deep_copy()' on a set of a type not convertible to actual key type");
        static_assert(std::is_convertible<OtherKeyT, KeyCheckT>::value, "cannot call 'set::deep_copy()' on a set of a type not convertible to actual key check type");
        Assert_(!viewToCopy.is_empty());
        Assert_(_impl._ctrl);
        u32 capacity = _impl.non_null_capacity();
        Assert_(HashSet_Impl::IsValidCapacity(capacity));
        Assert_(capacity > viewToCopy.size());
        KeyT* tTableKeys = _get_table_keys();
        for (auto it = viewToCopy.cbegin(), itEnd = viewToCopy.cend(); it != itEnd; it++) {
            const OtherKeyT& key = *it;
            u64 uHash = get_map_hash((KeyCheckT)key);
            u32 uIndex = prepare_insert_not_present_nogrow(uHash);
            tTableKeys[uIndex] = (KeyT)key;
        }
    };
    template<typename OtherKeyT> void deep_copy_from(SetView<OtherKeyT> viewToCopy) {
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._non_null_size()));
            if (_impl._ctrl) {
                if (uCapacity == uRequiredCapacity || (uRequiredCapacity < uCapacity && uCapacity < 128u)) {
                    _impl.non_null_reset();
                } else {
                    release();
                    _allocate(uRequiredCapacity);
                }
            } else {
                _allocate(uRequiredCapacity);
            }
            _deep_copy_when_sized(viewToCopy);
        } else {
            clear();
        }
    };

    void _allocate(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        u32 uAllocMetaSize = uNewCapacity + 20u + 1u + HashSet_Impl::NumClonedBytes();
        size_t uAllocKeySize = uNewCapacity * sizeof(KeyT);
        Assert_(uAllocKeySize < 0x1'0000'0000uLL);
        u8* alloc_start = _alloc.allocate(uAllocMetaSize, 8u);
        u8* tKeys = _alloc.allocate(uAllocKeySize, alignof(KeyT));
        _impl._ctrl = alloc_start + 20;
        _impl.capacity_lhv() = uNewCapacity;
        _impl.keys_lhv() = tKeys;
        _impl.non_null_reset();
    }
    FORCE_INLINE u8* non_null_get_start_of_alloc() { return _impl._ctrl - 20u; }
    FORCE_INLINE void clear() {
        if (LIKELY(_impl._ctrl)) {
            if (_impl.non_null_capacity() > 127) {
                _alloc.deallocate((u8*)_get_table_keys());
                _alloc.deallocate(non_null_get_start_of_alloc());
                _impl._ctrl = 0;
            } else {
                _impl.non_null_reset();
            }
        }
    }

    FORCE_INLINE u32 prepare_insert_not_present_nogrow(u64 uHashOfKeyToInsert) {
        u32 uResult = _impl.non_null_find_first_non_full(uHashOfKeyToInsert);
        _impl.non_null_set_ctrl_from_hash(uResult, uHashOfKeyToInsert);
        _impl.growth_left_lhv()--;
        _impl.size_lhv()++;
        return uResult;
    }

    u32 prepare_insert_not_present(u64 uHashOfKeyToInsert) {
        if (LIKELY(_impl._ctrl)) {
            if (LIKELY(_impl.non_null_growth_left())) {
                // NOOP
            } else {
                u32 capacity = _impl.non_null_capacity();
                if (capacity > Group::kWidth && u64(_impl.non_null_size()) * 32uLL <= u64(capacity) * 25uLL) {
                    _impl.non_null_drop_deletes_without_resize<KeyT, KeyCheckT>(_get_table_keys());
                } else {
                    Assert(capacity < 0x8000'0000u, "trying to grow too large");
                    resize(capacity * 2u + 1u);
                }
            }
        } else {
            _allocate(Group::kWidth - 1u);
        }
        return prepare_insert_not_present_nogrow(uHashOfKeyToInsert);
    }
    FORCE_INLINE iterator insert_not_present(u64 uHashOfKeyToInsert, const KeyT& keyToInsert) {
        u32 uInsertionIndex = prepare_insert_not_present(uHashOfKeyToInsert);
        KeyT* tTableKeys = _get_table_keys();
        tTableKeys[uInsertionIndex] = keyToInsert;
        return iterator(_impl._ctrl + uInsertionIndex, tTableKeys + uInsertionIndex);
    }

    iterator get_or_insert(const KeyT& keyToInsert) {
        KeyCheckT asKeyCheckT = (KeyCheckT)keyToInsert;
        u64 uHash = get_map_hash(asKeyCheckT);
        if (LIKELY(_impl._ctrl)) {
            iterator found = findHashed(uHash, asKeyCheckT);
            if (found != end())
                return found;
        }
        return insert_not_present(uHash, keyToInsert);
    }

    FORCE_INLINE iterator get_or_insert_check_hashed(u64 uHash, const KeyCheckT& asKeyCheckT, const KeyT& keyToInsert, bool* outWasInsertedAsNew) {
        if (LIKELY(_impl._ctrl)) {
            iterator found = findHashed(uHash, asKeyCheckT);
            if (found != end()) {
                *outWasInsertedAsNew = false;
                return found;
            }
        }
        *outWasInsertedAsNew = true;
        return insert_not_present(uHash, keyToInsert);
    }

    iterator get_or_insert_check(const KeyT& keyToInsert, bool* outWasInsertedAsNew) {
        KeyCheckT asKeyCheckT = (KeyCheckT)keyToInsert;
        u64 uHash = get_map_hash(asKeyCheckT);
        return get_or_insert_check_hashed(uHash, asKeyCheckT, keyToInsert, outWasInsertedAsNew);
    }

    FORCE_INLINE bool insert(const KeyT& keyToInsert) {
        bool outWasInsertedAsNew;
        get_or_insert_check(keyToInsert, &outWasInsertedAsNew);
        return outWasInsertedAsNew;
    }

    bool insertHashed(u64 uHash, const KeyT& keyToInsert) {
        bool outWasInsertedAsNew;
        KeyCheckT asKeyCheckT = (KeyCheckT)keyToInsert;
        get_or_insert_check_hashed(uHash, asKeyCheckT, keyToInsert, &outWasInsertedAsNew);
        return outWasInsertedAsNew;
    }

    void remove_at_iter(iterator it) {
        _impl.size_lhv()--;
        _impl.non_null_set_ctrl(it._getIndex_from(_impl), u8(SetMetaDataImpl::kDeleted));
    }

    bool remove_at_key(KeyCheckT key) {
        u64 uHash = get_map_hash(key);
        iterator found = findHashed(uHash, key);
        if (found != end()) {
            remove_at_iter(found);
            return true;
        } else
            return false;
    }


    void resize(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        if (_impl._ctrl) {
            ASet<KeyT, AllocT> old = *this;
            _allocate(uNewCapacity);
            KeyT* tTableKeys = _get_table_keys();
            for (auto it = old.cbegin(), itEnd = old.cend(); it != itEnd; it++) {
                const KeyT& key = *it;
                u64 uHash = get_map_hash((KeyCheckT)key);
                u32 uIndex = prepare_insert_not_present_nogrow(uHash);
                tTableKeys[uIndex] = key;
            }
            old.release();
        } else {
            _allocate(uNewCapacity);
        }
    }

    void release() {
        if (LIKELY(_impl._ctrl)) {
            _alloc.deallocate((u8*)_get_table_keys());
            _alloc.deallocate(non_null_get_start_of_alloc());
            _impl._ctrl = 0;
        }
    }
};

template<typename KeyT> using Set = ASet<KeyT, MallocPoweredAlloc>;
template<typename KeyT> using TmpSet = ASet<KeyT, FireAndForgetArenaAlloc>;
template<typename KeyT> using DynSet = ASet<KeyT, DynGPAlloc>;

// Differs from ASet<OwnedString, AllocT> in that it always auto-allocates its owned string on key insert (using
//   same allocator)
// It is statically typed as having FFString as keys, though (since we do not want those to be thought owned from
//   somewhere else by user code, and so that we can work ok with ScratchAllocators, which will be fine since
//   we'll have sufficient knowledge to force-cast to OwnedString internally if need be).
template<typename AllocT>
struct AStringSet : public SetView<FFString> {

    typedef typename SetView<FFString>::iterator iterator;
    typedef typename SetView<FFString>::const_iterator const_iterator;

    AStringSet() = default;
    AStringSet(const AStringSet<AllocT>&) = default;
    AStringSet<AllocT>& operator=(const AStringSet<AllocT>&) = default;
    ~AStringSet() = default;

    AllocT _alloc;

    FORCE_INLINE AStringSet(AllocT alloc, u32 uInitialCapacity):_impl(), _alloc(alloc) { _impl = {}; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    FORCE_INLINE AStringSet(AllocT alloc):_impl(), _alloc(alloc) { _impl = {}; }

    FORCE_INLINE AllocT alloc() const { return _alloc; }
    FORCE_INLINE AllocT& alloc_ref() { return _alloc; }

    void init(AllocT alloc) { _impl = {}; _alloc = alloc; }
    void init(AllocT alloc, u32 uInitialCapacity) { _impl = {}; _alloc = alloc; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    
    template<typename OtherKeyT>
    void init(AllocT alloc, SetView<OtherKeyT> viewToCopy) {
        _impl = {}; _alloc = alloc;
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._non_null_size()));
            _allocate(uRequiredCapacity);
            _deep_copy_when_sized(viewToCopy);
        }
    };

    template<typename = void> void init() {
        static_assert(AllocT::has_default_instance, "non-specific ASet::init() called with allocator not having default");
        init(AllocT::getDefaultInstance());
    };
    template<typename = void> void init(u32 uInitialCapacity) {
        static_assert(AllocT::has_default_instance, "non-specific ASet::init() called with allocator not having default");
        init(AllocT::getDefaultInstance(), uInitialCapacity);
    };

    template<typename OtherKeyT>
    void _deep_copy_when_sized(SetView<OtherKeyT> viewToCopy) {
        static_assert(std::is_convertible<OtherKeyT, StringView>::value, "cannot call 'StringSet::deep_copy()' from a set of a type not convertible to StringView");
        Assert_(!viewToCopy.is_empty());
        Assert_(_impl._ctrl);
        u32 capacity = _impl.non_null_capacity();
        Assert_(HashSet_Impl::IsValidCapacity(capacity));
        Assert_(capacity > viewToCopy.size());
        FFString* tTableKeys = _get_table_keys();
        for (auto it = viewToCopy.cbegin(), itEnd = viewToCopy.cend(); it != itEnd; it++) {
            StringView key = (StringView)*it;
            u64 uHash = get_map_hash(key);
            u32 uIndex = prepare_insert_not_present_nogrow(uHash);
            tTableKeys[uIndex] = FFString::fromAllocatorMakeFF(key, _alloc);
        }
    };

    template<typename OtherKeyT>
    void deep_copy_from(SetView<OtherKeyT> viewToCopy) {
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._impl.non_null_size()));
            if (_impl._ctrl) {
                _release_allocated_strings();
                if (uCapacity == uRequiredCapacity || (uRequiredCapacity < uCapacity && uCapacity < 128u)) {
                    _impl.non_null_reset();
                } else {
                    _release_index_only();
                    _allocate(uRequiredCapacity);
                }
            } else {
                _allocate(uRequiredCapacity);
            }
            _deep_copy_when_sized(viewToCopy);
        } else {
            clear();
        }
    };

    void _allocate(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        u32 uAllocMetaSize = uNewCapacity + 20u + 1u + HashSet_Impl::NumClonedBytes();
        size_t uAllocKeySize = uNewCapacity * sizeof(FFString);
        Assert_(uAllocKeySize < 0x1'0000'0000uLL);
        u8* alloc_start = _alloc.allocate(uAllocMetaSize, 8u);
        u8* tKeys = _alloc.allocate(uAllocKeySize, alignof(FFString));
        _impl._ctrl = alloc_start + 20;
        _impl.capacity_lhv() = uNewCapacity;
        _impl.keys_lhv() = tKeys;
        _impl.non_null_reset();
    }

    FORCE_INLINE u8* non_null_get_start_of_alloc() { return _impl._ctrl - 20; }

    FORCE_INLINE void _release_allocated_strings() {
        Assert_(_impl._ctrl);
        if (!AllocT::is_ensured_fire_and_forget && !_alloc.isFireAndForget()) {
            for (auto it = begin(), itEnd = end(); it != itEnd; it++) {
                u8* str_start = (*it).pStart;
                if (str_start)
                    _alloc.deallocate(str_start - 8);
            }
        }
    }

    FORCE_INLINE void clear() {
        if (LIKELY(_impl._ctrl)) {
            _release_allocated_strings();
            if (_impl.non_null_capacity() > 127) {
                _alloc.deallocate((u8*)_get_table_keys());
                _alloc.deallocate(non_null_get_start_of_alloc());
                _impl._ctrl = 0;
            } else {
                _impl.non_null_reset();
            }
        }
    }

    FORCE_INLINE u32 prepare_insert_not_present_nogrow(u64 uHashOfKeyToInsert) {
        u32 uResult = _impl.non_null_find_first_non_full(uHashOfKeyToInsert);
        _impl.non_null_set_ctrl_from_hash(uResult, uHashOfKeyToInsert);
        _impl.growth_left_lhv()--;
        _impl.size_lhv()++;
        return uResult;
    }

    u32 prepare_insert_not_present(u64 uHashOfKeyToInsert) {
        if (LIKELY(_impl._ctrl)) {
            if (LIKELY(_impl.non_null_growth_left())) {
                // NOOP
            } else {
                u32 capacity = _impl.non_null_capacity();
                if (capacity > Group::kWidth && u64(_impl.non_null_size()) * 32uLL <= u64(capacity) * 25uLL) {
                    _impl.non_null_drop_deletes_without_resize<FFString, StringView>(_get_table_keys());
                } else {
                    Assert(capacity < 0x8000'0000u, "trying to grow too large");
                    resize(capacity * 2u + 1u);
                }
            }
        } else {
            _allocate(Group::kWidth - 1u);
        }
        return prepare_insert_not_present_nogrow(uHashOfKeyToInsert);
    }

    FORCE_INLINE iterator insert_not_present(u64 uHashOfKeyToInsert, const FFString& keyToInsert) {
        u32 uInsertionIndex = prepare_insert_not_present(uHashOfKeyToInsert);
        KeyT* tTableKeys = _get_table_keys();
        tTableKeys[uInsertionIndex] = keyToInsert;
        return iterator(_impl._ctrl + uInsertionIndex, tTableKeys + uInsertionIndex);
    }

    iterator get_or_insert(StringView keyToInsert) {
        u64 uHash = get_map_hash(keyToInsert);
        if (LIKELY(_impl._ctrl)) {
            iterator found = findHashed(uHash, keyToInsert);
            if (found != end())
                return found;
            return insert_not_present(uHash, FFString::fromAllocatorMakeFF(keyToInsert, _alloc));
        } else {
            return insert_not_present(uHash, FFString::fromAllocatorMakeFF(keyToInsert, _alloc));
        }
    }

    iterator get_or_insert_check(StringView keyToInsert, bool* outWasInsertedAsNew) {
        u64 uHash = get_map_hash(keyToInsert);
        if (LIKELY(_impl._ctrl)) {
            iterator found = findHashed(uHash, keyToInsert);
            if (found != end()) {
                *outWasInsertedAsNew = false;
                return found;
            }
            *outWasInsertedAsNew = true;
            return insert_not_present(uHash, FFString::fromAllocatorMakeFF(keyToInsert, _alloc));
        } else {
            *outWasInsertedAsNew = true;
            return insert_not_present(uHash, FFString::fromAllocatorMakeFF(keyToInsert, _alloc));
        }
    }

    FORCE_INLINE bool insert(StringView keyToInsert) {
        bool outWasInsertedAsNew;
        get_or_insert_check(keyToInsert, &outWasInsertedAsNew);
        return outWasInsertedAsNew;
    }

    void remove_at_iter(iterator it) {
        u8* str_start = (*it).pStart;
        if (str_start)
            _alloc.deallocate(str_start - 8);
        _impl.size_lhv()--;
        _impl.non_null_set_ctrl(it._getIndex_from(_impl), u8(SetMetaDataImpl::kDeleted));
    }

    bool remove_at_key(StringView key) {
        u64 uHash = get_map_hash(key);
        iterator found = findHashed(uHash, key);
        if (found != end()) {
            remove_at_iter(found);
            return true;
        } else
            return false;
    }

    void resize(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        if (_impl._ctrl) {
            AStringSet<AllocT> old = *this;
            _allocate(uNewCapacity);
            FFString* tTableKeys = _get_table_keys();
            for (auto it = old.cbegin(), itEnd = old.cend(); it != itEnd; it++) {
                FFString key = *it;
                u64 uHash = get_map_hash((StringView)key);
                u32 uIndex = prepare_insert_not_present_nogrow(uHash);
                tTableKeys[uIndex] = key;
            }
            old._release_tables();
        } else {
            _allocate(uNewCapacity);
        }
    }

    void _release_tables() {
        if (LIKELY(_impl._ctrl)) {
            _alloc.deallocate((u8*)_get_table_keys());
            _alloc.deallocate(non_null_get_start_of_alloc());
            _impl._ctrl = 0;
        }
    }

    void release() {
        _release_allocated_strings();
        _release_tables();
    }

};

typedef AStringSet<MallocPoweredAlloc> StringSet;
typedef AStringSet<FireAndForgetArenaAlloc> TmpStringSet;
typedef AStringSet<DynGPAlloc> DynStringSet;

template<typename KeyT, typename ValT>
struct MapView {
    static_assert(std::is_trivially_copyable<KeyT>::value,
                  "Map only allows trivially copyable keys. Fall back to std::unordered_map for fancy content");
    static_assert(std::is_trivially_copyable<ValT>::value,
                  "Map only allows trivially copyable values. Fall back to std::unordered_map for fancy content");
    typedef typename MapCheckType<KeyT>::value_t KeyCheckT;
    static_assert(std::is_convertible<KeyT, KeyCheckT>::value,
                  "Map KeyT must be convertible to KeyCheckT");
protected:
    HashSet_Impl _impl;
public:

    MapView() = default;
    MapView(const MapView<KeyT, ValT>&) = default;
    MapView<KeyT, ValT>& operator=(const MapView<KeyT, ValT>&) = default;
    ~MapView() = default;

    FORCE_INLINE bool is_empty() const { return _impl._ctrl == 0 || _impl.non_null_size() == 0; };
    FORCE_INLINE u32 size() const { return _impl.size(); };
    FORCE_INLINE u32 current_capacity() const { return _impl.capacity(); };
    FORCE_INLINE u32 current_growth_left() const { return _impl.growth_left(); };

    struct iterator {
        iterator() = default;
        iterator(const MapView<KeyT, ValT>::iterator&) = default;
        MapView<KeyT, ValT>::iterator& operator=(const MapView<KeyT, ValT>::iterator&) = default;
        ~iterator() = default;
        FORCE_INLINE bool operator==(const MapView<KeyT, ValT>::iterator& other) const { return this->ctrl_ == other.ctrl_; }
        FORCE_INLINE bool operator!=(const MapView<KeyT, ValT>::iterator& other) const { return !operator==(other); }
        FORCE_INLINE KeyT& key() {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "key() called on invalid map iterator");
            return *pKey_;
        }
        FORCE_INLINE const KeyT& key() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "key() called on invalid map iterator");
            return *pKey_;
        }
        FORCE_INLINE ValT& value() {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "value() called on invalid map iterator");
            return *pValue_;
        }
        FORCE_INLINE const ValT& value() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "value() called on invalid map iterator");
            return *pValue_;
        }
        FORCE_INLINE MapView<KeyT, ValT>::iterator& operator++() {
          Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "++ called on invalid map iterator");
          ++ctrl_; ++pKey_; ++pValue_; skip_empty_or_deleted_and_check_end();
          return *this;
        }
        FORCE_INLINE MapView<KeyT, ValT>::iterator operator++(int) { auto tmp = *this; ++*this; return tmp; }
        FORCE_INLINE u32 _getIndex_from(HashSet_Impl impl) {
          Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "_getIndex_from() called on invalid set iterator");
          Assert_(impl._ctrl);
          Assert_(ctrl_ >= impl._ctrl && ctrl_ < impl._ctrl + impl.non_null_capacity());
          return u32(ctrl_ - impl._ctrl);
        }
    private:
        const u8* ctrl_;
        KeyT* pKey_;
        ValT* pValue_;
        FORCE_INLINE iterator(const u8* ctrl, KeyT* pKey, ValT* pValue):ctrl_(ctrl), pKey_(pKey), pValue_(pValue) {}
        void skip_empty_or_deleted_and_check_end() {
            while (SetMetaDataImpl::IsEmptyOrDeleted(*ctrl_)) {
                u32 shift = Group{ctrl_}.CountLeadingEmptyOrDeleted();
                ctrl_ += shift;
                pKey_ += shift;
                pValue_ += shift;
            }
            if (UNLIKELY(*ctrl_ == u8(SetMetaDataImpl::kSentinel))) {
                ctrl_ = 0;
                pKey_ = 0;
                pValue_ = 0;
            }
        }
        friend struct MapView<KeyT, ValT>;
        friend struct MapView<KeyT, ValT>::const_iterator;
        template<typename KT, typename VT, typename AllocT> friend struct AMap;
        template<typename VT, typename AllocT> friend struct AStringMap;
    };

    struct const_iterator {
        const_iterator() = default;
        const_iterator(const MapView<KeyT, ValT>::const_iterator&) = default;
        MapView<KeyT, ValT>::const_iterator& operator=(const MapView<KeyT, ValT>::const_iterator&) = default;
        ~const_iterator() = default;
        FORCE_INLINE const_iterator(const MapView<KeyT, ValT>::iterator& fromNonConst):ctrl_(fromNonConst.ctrl_), pKey_(fromNonConst.pKey_), pValue_(fromNonConst.pValue_) {}
        FORCE_INLINE MapView<KeyT, ValT>::const_iterator& operator=(const MapView<KeyT, ValT>::iterator& fromNonConst) { return operator=(MapView<KeyT, ValT>::const_iterator(fromNonConst)); }

        FORCE_INLINE bool operator==(const MapView<KeyT, ValT>::const_iterator& other) const { return this->ctrl_ == other.ctrl_; }
        FORCE_INLINE bool operator!=(const MapView<KeyT, ValT>::const_iterator& other) const { return !operator==(other); }
        FORCE_INLINE bool operator==(const MapView<KeyT, ValT>::iterator& other) const { return this->ctrl_ == other.ctrl_; }
        FORCE_INLINE bool operator!=(const MapView<KeyT, ValT>::iterator& other) const { return !operator==(other); }

        FORCE_INLINE const KeyT& key() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "key() called on invalid map iterator");
            return *pKey_;
        }
        FORCE_INLINE const ValT& value() const {
            Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "value() called on invalid map iterator");
            return *pValue_;
        }
        FORCE_INLINE MapView<KeyT, ValT>::const_iterator& operator++() {
          Assert(ctrl_ != nullptr && SetMetaDataImpl::IsFull(*ctrl_), "++ called on invalid map iterator");
          ++ctrl_; ++pKey_; ++pValue_; skip_empty_or_deleted_and_check_end();
          return *this;
        }
        FORCE_INLINE MapView<KeyT, ValT>::const_iterator operator++(int) { auto tmp = *this; ++*this; return tmp; }

    private:
        const u8* ctrl_;
        const KeyT* pKey_;
        const ValT* pValue_;
        FORCE_INLINE const_iterator(const u8* ctrl, const KeyT* pKey, const ValT* pValue):ctrl_(ctrl), pKey_(pKey), pValue_(pValue) {}
        void skip_empty_or_deleted_and_check_end() {
            while (SetMetaDataImpl::IsEmptyOrDeleted(*ctrl_)) {
                u32 shift = Group{ctrl_}.CountLeadingEmptyOrDeleted();
                ctrl_ += shift;
                pKey_ += shift;
                pValue_ += shift;
            }
            if (UNLIKELY(*ctrl_ == u8(SetMetaDataImpl::kSentinel))) {
                ctrl_ = 0;
                pKey_ = 0;
                pValue_ = 0;
            }
        }
        friend struct MapView<KeyT, ValT>;
        template<typename KT, typename VT, typename AllocT> friend struct AMap;
        template<typename VT, typename AllocT> friend struct AStringMap;
    };

    FORCE_INLINE iterator begin() {
        if (LIKELY(_impl._ctrl)) {
            iterator it(_impl._ctrl, _get_table_keys(), _get_table_values());
            it.skip_empty_or_deleted_and_check_end();
            return it;
        }
        return end();
    }
    FORCE_INLINE iterator end() const { return iterator(0, 0, 0); }
    FORCE_INLINE const_iterator cbegin() const {
        if (LIKELY(_impl._ctrl)) {
            const_iterator it(_impl._ctrl, _get_const_table_keys(), _get_const_table_values());
            it.skip_empty_or_deleted_and_check_end();
            return it;
        }
        return end();
    }
    FORCE_INLINE const_iterator cend() const { return const_iterator(0, 0, 0); }

    iterator findHashed(u64 uHashOfKeyToFind, const KeyCheckT& keyToFind) {
        if (LIKELY(_impl._ctrl)) {
            const u32 capacity = _impl.non_null_capacity();
            KeyT* tKeys = _get_table_keys();
            u32 uIndex = _impl.non_null_find_hash_and_key_index(uHashOfKeyToFind, keyToFind, are_equal_keys<KeyT, KeyCheckT>, tKeys);
            if (uIndex < capacity) {
                return iterator(_impl._ctrl + uIndex, tKeys + uIndex, _get_table_values() + uIndex);
            }
        }
        return end();
    }

    const_iterator findHashed(u64 uHashOfKeyToFind, const KeyCheckT& keyToFind) const {
        if (LIKELY(_impl._ctrl)) {
            const u32 capacity = _impl.non_null_capacity();
            const KeyT* tKeys = _get_const_table_keys();
            u32 uIndex = _impl.non_null_find_hash_and_key_index(uHashOfKeyToFind, keyToFind, are_equal_keys<KeyT, KeyCheckT>, tKeys);
            if (uIndex < capacity) {
                return const_iterator(_impl._ctrl + uIndex, tKeys + uIndex, _get_const_table_values() + uIndex);
            }
        }
        return end();
    }

    FORCE_INLINE iterator find(const KeyCheckT& keyToFind) {
        u64 uHash = get_map_hash(keyToFind);
        return findHashed(uHash, keyToFind);
    }

    FORCE_INLINE const_iterator find(const KeyCheckT& keyToFind) const {
        u64 uHash = get_map_hash(keyToFind);
        return findHashed(uHash, keyToFind);
    }

    FORCE_INLINE u32 _non_null_size() const { Assert_(_impl._ctrl); return _impl.non_null_size(); }

protected:
    FORCE_INLINE KeyT* _get_table_keys() { Assert_(_impl._ctrl); return reinterpret_cast<KeyT*>(_impl.non_null_keys()); };
    FORCE_INLINE const KeyT* _get_const_table_keys() const { Assert_(_impl._ctrl); return reinterpret_cast<const KeyT*>(_impl.non_null_keys()); };
    FORCE_INLINE ValT* _get_table_values() { Assert_(_impl._ctrl); return reinterpret_cast<ValT*>(_impl.non_null_values()); };
    FORCE_INLINE const ValT* _get_const_table_values() const { Assert_(_impl._ctrl); return reinterpret_cast<const ValT*>(_impl.non_null_values()); };
};

template<typename KeyT, typename ValT, typename AllocT>
struct AMap : public MapView<KeyT, ValT> {

    typedef typename MapView<KeyT, ValT>::iterator iterator;
    typedef typename MapView<KeyT, ValT>::const_iterator const_iterator;

    typedef typename MapView<KeyT, ValT>::KeyCheckT KeyCheckT;
    AMap() = default;
    AMap(const AMap<KeyT, ValT, AllocT>&) = default;
    AMap<KeyT, ValT, AllocT>& operator=(const AMap<KeyT, ValT, AllocT>&) = default;
    ~AMap() = default;

    AllocT _alloc;

    FORCE_INLINE AMap(AllocT alloc, u32 uInitialCapacity): _alloc(alloc) { _impl = {}; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    FORCE_INLINE AMap(AllocT alloc): _alloc(alloc) { _impl = {}; }

    FORCE_INLINE AllocT alloc() const { return _alloc; }
    FORCE_INLINE AllocT& alloc_ref() { return _alloc; }

    void init(AllocT alloc) { _impl = {}; _alloc = alloc; }
    void init(AllocT alloc, u32 uInitialCapacity) { _impl = {}; _alloc = alloc; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }

    template<typename OtherKeyT, typename OtherValT>
    void init(AllocT alloc, MapView<OtherKeyT, OtherValT> viewToCopy) {
        _impl = {}; _alloc = alloc;
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._non_null_size()));
            _allocate(uRequiredCapacity);
            _deep_copy_when_sized(viewToCopy);
        }
    };

    template<typename = void> void init() {
        static_assert(AllocT::has_default_instance, "non-specific AMap::init() called with allocator not having default");
        init(AllocT::getDefaultInstance());
    };
    template<typename = void> void init(u32 uInitialCapacity) {
        static_assert(AllocT::has_default_instance, "non-specific AMap::init() called with allocator not having default");
        init(AllocT::getDefaultInstance(), uInitialCapacity);
    };

    template<typename OtherKeyT, typename OtherValT>
    void _deep_copy_when_sized(MapView<OtherKeyT, OtherValT> viewToCopy) {
        static_assert(std::is_convertible<OtherKeyT, KeyT>::value, "cannot call 'map::deep_copy()' on a map of a key type not convertible to actual key type");
        static_assert(std::is_convertible<OtherKeyT, KeyCheckT>::value, "cannot call 'map::deep_copy()' on a map of a key type not convertible to actual key check type");
        static_assert(std::is_convertible<OtherValT, ValT>::value, "cannot call 'map::deep_copy()' on a map of a value type not convertible to our value type");
        Assert_(!viewToCopy.is_empty());
        Assert_(_impl._ctrl);
        u32 capacity = _impl.non_null_capacity();
        Assert_(HashSet_Impl::IsValidCapacity(capacity));
        Assert_(capacity >= viewToCopy.size());
        KeyT* tTableKeys = _get_table_keys();
        ValT* tTableValues = _get_table_values();
        for (auto it = viewToCopy.cbegin(), itEnd = viewToCopy.cend(); it != itEnd; it++) {
            const OtherKeyT& key = it.key();
            u64 uHash = get_map_hash((KeyCheckT)key);
            u32 uIndex = prepare_insert_not_present_nogrow(uHash);
            tTableKeys[uIndex] = (KeyT)key;
            tTableValues[uIndex] = (ValT)it.value();
        }
    };

    template<typename OtherKeyT, typename OtherValT>
    void deep_copy_from(MapView<OtherKeyT, OtherValT> viewToCopy) {
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._impl.non_null_size()));
            if (_impl._ctrl) {
                if (uCapacity == uRequiredCapacity || (uRequiredCapacity < uCapacity && uCapacity < 128u)) {
                    _impl.non_null_reset();
                } else {
                    release();
                    _allocate(uRequiredCapacity);
                }
            } else {
                _allocate(uRequiredCapacity);
            }
            _deep_copy_when_sized(viewToCopy);
        } else {
            clear();
        }
    };

    void _allocate(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        u32 uAllocMetaSize = uNewCapacity + 28u + 1u + HashSet_Impl::NumClonedBytes();
        size_t uAllocKeySize = uNewCapacity * sizeof(KeyT);
        size_t uAllocValSize = uNewCapacity * sizeof(ValT);
        Assert_(uAllocKeySize < 0x1'0000'0000uLL);
        Assert_(uAllocValSize < 0x1'0000'0000uLL);
        u8* alloc_start = _alloc.allocate(uAllocMetaSize, 8u);
        u8* tKeys = _alloc.allocate(u32(uAllocKeySize), alignof(KeyT));
        u8* tValues = _alloc.allocate(u32(uAllocValSize), alignof(ValT));
        _impl._ctrl = alloc_start + 28;
        _impl.capacity_lhv() = uNewCapacity;
        _impl.keys_lhv() = tKeys;
        _impl.values_lhv() = tValues;
        _impl.non_null_reset();
    }

    FORCE_INLINE u8* non_null_get_start_of_alloc() { return _impl._ctrl - 28; }

    FORCE_INLINE void clear() {
        if (LIKELY(_impl._ctrl)) {
            if (_impl.non_null_capacity() > 127) {
                _alloc.deallocate((u8*)_get_table_keys());
                _alloc.deallocate((u8*)_get_table_values());
                _alloc.deallocate(non_null_get_start_of_alloc());
                _impl._ctrl = 0;
            } else {
                _impl.non_null_reset();
            }
        }
    }

    FORCE_INLINE u32 prepare_insert_not_present_nogrow(u64 uHashOfKeyToInsert) {
        u32 uResult = _impl.non_null_find_first_non_full(uHashOfKeyToInsert);
        _impl.non_null_set_ctrl_from_hash(uResult, uHashOfKeyToInsert);
        _impl.growth_left_lhv()--;
        _impl.size_lhv()++;
        return uResult;
    }

    u32 prepare_insert_not_present(u64 uHashOfKeyToInsert) {
        if (LIKELY(_impl._ctrl)) {
            if (LIKELY(_impl.non_null_growth_left())) {
                // NOOP
            } else {
                u32 capacity = _impl.non_null_capacity();
                if (capacity > Group::kWidth && u64(_impl.non_null_size()) * 32uLL <= u64(capacity) * 25uLL) {
                    _impl.non_null_drop_deletes_without_resize<KeyT, KeyCheckT, ValT>(_get_table_keys(), _get_table_values());
                } else {
                    Assert(capacity < 0x8000'0000u, "trying to grow too large");
                    resize(capacity * 2u + 1u);
                }
            }
        } else {
            _allocate(Group::kWidth - 1u);
        }
        return prepare_insert_not_present_nogrow(uHashOfKeyToInsert);
    }

    FORCE_INLINE iterator insert_not_present(u64 uHashOfKeyToInsert, const KeyT& keyToInsert, const ValT& valToInsert) {
        u32 uInsertionIndex = prepare_insert_not_present(uHashOfKeyToInsert);
        KeyT* tTableKeys = _get_table_keys();
        ValT* tTableVals = _get_table_values();
        tTableKeys[uInsertionIndex] = keyToInsert;
        tTableVals[uInsertionIndex] = valToInsert;
        return iterator(_impl._ctrl + uInsertionIndex, tTableKeys + uInsertionIndex, tTableVals + uInsertionIndex);
    }

    iterator insert(const KeyT& keyToInsert, const ValT& valToInsert) {
        KeyCheckT asKeyCheckT = (KeyCheckT)keyToInsert;
        u64 uHash = get_map_hash(asKeyCheckT);
        if (LIKELY(_impl._ctrl)) {
            iterator found = findHashed(uHash, asKeyCheckT);
            if (found != end()) {
                found.value() = valToInsert;
                return found;
            }
        }
        return insert_not_present(uHash, keyToInsert, valToInsert);
    }
    ; // template termination

    // Warning  : NOT same behaviour as std containers: fails on not found !!
    const ValT& operator[](const KeyCheckT& key) const {
        u64 uHash = get_map_hash(key);
        const_iterator found = findHashed(uHash, key);
        Assert_(found != end()); 
        return found.value();
    }

    // Warning  : NOT same behaviour as std containers: fails on not found !!
    ValT& operator[](const KeyCheckT& key) {
        u64 uHash = get_map_hash(key);
        iterator found = findHashed(uHash, key);
        Assert_(found != end()); 
        return found.value();
    }

    void remove_at_iter(iterator it) {
        _impl.size_lhv()--;
        _impl.non_null_set_ctrl(it._getIndex_from(_impl), u8(SetMetaDataImpl::kDeleted));
    }

    bool remove_at_key(KeyCheckT key) {
        u64 uHash = get_map_hash(key);
        iterator found = findHashed(uHash, key);
        if (found != end()) {
            remove_at_iter(found);
            return true;
        } else
            return false;
    }

    void resize(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        if (_impl._ctrl) {
            AMap<KeyT, ValT, AllocT> old = *this;
            _allocate(uNewCapacity);
            KeyT* tTableKeys = _get_table_keys();
            ValT* tTableVals = _get_table_values();
            for (auto it = old.cbegin(), itEnd = old.cend(); it != itEnd; it++) {
                const KeyT& key = it.key();
                u64 uHash = get_map_hash((KeyCheckT)key);
                u32 uIndex = prepare_insert_not_present_nogrow(uHash);
                tTableKeys[uIndex] = key;
                tTableVals[uIndex] = it.value();
            }
            old.release();
        } else {
            _allocate(uNewCapacity);
        }
    }

    void release() {
        if (LIKELY(_impl._ctrl)) {
            _alloc.deallocate((u8*)_get_table_keys());
            _alloc.deallocate((u8*)_get_table_values());
            _alloc.deallocate(non_null_get_start_of_alloc());
            _impl._ctrl = 0;
        }
    }
};

template<typename KeyT, typename ValT> using Map = AMap<KeyT, ValT, MallocPoweredAlloc>;
template<typename KeyT, typename ValT> using TmpMap = AMap<KeyT, ValT, FireAndForgetArenaAlloc>;
template<typename KeyT, typename ValT> using DynMap = AMap<KeyT, ValT, DynGPAlloc>;

// Differs from AMap<OwnedString, ValT, AllocT> in that it auto-allocates its owned string on key insert
template<typename ValT, typename AllocT>
struct AStringMap : public MapView<FFString, ValT> {

    typedef typename MapView<FFString, ValT>::iterator iterator;
    typedef typename MapView<FFString, ValT>::const_iterator const_iterator;

    AStringMap() = default;
    AStringMap(const AStringMap<ValT, AllocT>&) = default;
    AStringMap<ValT, AllocT>& operator=(const AStringMap<ValT, AllocT>&) = default;
    ~AStringMap() = default;

    AllocT _alloc;

    FORCE_INLINE AStringMap(AllocT alloc, u32 uInitialCapacity): _alloc(alloc) { _impl = {}; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    FORCE_INLINE AStringMap(AllocT alloc): _alloc(alloc) { _impl = {}; }

    FORCE_INLINE AllocT alloc() const { return _alloc; }
    FORCE_INLINE AllocT& alloc_ref() { return _alloc; }

    void init(AllocT alloc) { _impl = {}; _alloc = alloc; }
    void init(AllocT alloc, u32 uInitialCapacity) { _impl = {}; _alloc = alloc; if (uInitialCapacity) _allocate(HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(uInitialCapacity))); }
    template<typename OtherKeyT, typename OtherValT> void init(AllocT alloc, MapView<OtherKeyT, OtherValT> viewToCopy) {
        _impl = {}; _alloc = alloc;
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._non_null_size()));
            _allocate(uRequiredCapacity);
            _deep_copy_when_sized(viewToCopy);
        }
    };

    template<typename = void> void init() {
        static_assert(AllocT::has_default_instance, "non-specific AStringMap::init() called with allocator not having default");
        init(AllocT::getDefaultInstance());
    };
    template<typename = void> void init(u32 uInitialCapacity) {
        static_assert(AllocT::has_default_instance, "non-specific AStringMap::init() called with allocator not having default");
        init(AllocT::getDefaultInstance(), uInitialCapacity);
    };

    template<typename OtherKeyT, typename OtherValT>
    void _deep_copy_when_sized(MapView<OtherKeyT, OtherValT> viewToCopy) {
        static_assert(std::is_convertible<OtherKeyT, StringView>::value, "cannot call 'StringMap::deep_copy()' on a map of a key type not convertible to StringView");
        static_assert(std::is_convertible<OtherValT, ValT>::value, "cannot call 'StringMap::deep_copy()' on a map of a value type not convertible to our value type");
        Assert_(!viewToCopy.is_empty());
        Assert_(_impl._ctrl);
        u32 capacity = _impl.non_null_capacity();
        Assert_(HashSet_Impl::IsValidCapacity(capacity));
        Assert_(capacity > viewToCopy.size());
        FFString* tTableKeys = _get_table_keys();
        ValT* tTableValues = _get_table_values();
        for (auto it = viewToCopy.cbegin(), itEnd = viewToCopy.cend(); it != itEnd; it++) {
            StringView key = (StringView)it.key();
            u64 uHash = get_map_hash(key);
            u32 uIndex = prepare_insert_not_present_nogrow(uHash);
            tTableKeys[uIndex] = FFString::fromAllocatorMakeFF(key, _alloc);
            tTableValues[uIndex] = (ValT)it.value();
        }
    };

    template<typename OtherKeyT, typename OtherValT>
    void deep_copy_from(MapView<OtherKeyT, OtherValT> viewToCopy) {
        if (LIKELY(!viewToCopy.is_empty())) {
            u32 uRequiredCapacity = HashSet_Impl::NormalizeCapacity(HashSet_Impl::GrowthToLowerboundCapacity(viewToCopy._impl.non_null_size()));
            if (_impl._ctrl) {
                _release_allocated_strings();
                if (uCapacity == uRequiredCapacity || (uRequiredCapacity < uCapacity && uCapacity < 128u)) {
                    _impl.non_null_reset();
                } else {
                    _release_tables();
                    _allocate(uRequiredCapacity);
                }
            } else {
                _allocate(uRequiredCapacity);
            }
            _deep_copy_when_sized(viewToCopy);
        } else {
            clear();
        }
    };

    void _allocate(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        u32 uAllocMetaSize = uNewCapacity + 28u + 1u + HashSet_Impl::NumClonedBytes();
        size_t uAllocKeySize = uNewCapacity * sizeof(FFString);
        size_t uAllocValSize = uNewCapacity * sizeof(ValT);
        Assert_(uAllocKeySize < 0x1'0000'0000uLL);
        Assert_(uAllocValSize < 0x1'0000'0000uLL);
        #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 0
            platform_log_info("StringMap realloc:");
        #endif
        u8* alloc_start = _alloc.allocate(uAllocMetaSize, 8u);
        u8* tKeys = _alloc.allocate(u32(uAllocKeySize), alignof(FFString));
        u8* tValues = _alloc.allocate(u32(uAllocValSize), alignof(ValT));
        #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 0
        {
            char szTmp[256];
            sprintf(szTmp, "  StringMap was reallocated to 0x%llx with capacity 0x%x",
                reinterpret_cast<u64>(alloc_start), uNewCapacity);
            platform_log_info(szTmp);
        }
        #endif
        _impl._ctrl = alloc_start + 28;
        _impl.capacity_lhv() = uNewCapacity;
        _impl.keys_lhv() = tKeys;
        _impl.values_lhv() = tValues;
        _impl.non_null_reset();
    }

    FORCE_INLINE u8* non_null_get_start_of_alloc() { return _impl._ctrl - 28; }

    FORCE_INLINE void _release_allocated_strings() {
        Assert_(_impl._ctrl);
        if (!AllocT::is_ensured_fire_and_forget && !_alloc.isFireAndForget()) {
            for (auto it = begin(), itEnd = end(); it != itEnd; it++) {
                u8* str_start = it.key().pStart;
                if (str_start)
                    _alloc.deallocate(str_start - 8);
            }
        }
    }

    FORCE_INLINE void clear() {
        if (LIKELY(_impl._ctrl)) {
            _release_allocated_strings();
            if (_impl.non_null_capacity() > 127) {
                _alloc.deallocate((u8*)_get_table_keys());
                _alloc.deallocate((u8*)_get_table_values());
                _alloc.deallocate(non_null_get_start_of_alloc());
                _impl._ctrl = 0;
            } else {
                _impl.non_null_reset();
            }
        }
    }

    FORCE_INLINE u32 prepare_insert_not_present_nogrow(u64 uHashOfKeyToInsert) {
        u32 uResult = _impl.non_null_find_first_non_full(uHashOfKeyToInsert);
        _impl.non_null_set_ctrl_from_hash(uResult, uHashOfKeyToInsert);
        _impl.growth_left_lhv()--;
        _impl.size_lhv()++;
        return uResult;
    }

    u32 prepare_insert_not_present(u64 uHashOfKeyToInsert) {
        if (LIKELY(_impl._ctrl)) {
            if (LIKELY(_impl.non_null_growth_left())) {
                // NOOP
            } else {
                u32 capacity = _impl.non_null_capacity();
                if (capacity > Group::kWidth && u64(_impl.non_null_size()) * 32uLL <= u64(capacity) * 25uLL) {
                    _impl.non_null_drop_deletes_without_resize<FFString, StringView, ValT>(_get_table_keys(), _get_table_values());
                } else {
                    Assert(capacity < 0x8000'0000u, "trying to grow too large");
                    resize(capacity * 2u + 1u);
                }
            }
        } else {
            _allocate(Group::kWidth - 1u);
        }
        return prepare_insert_not_present_nogrow(uHashOfKeyToInsert);
    }

    FORCE_INLINE iterator insert_not_present(u64 uHashOfKeyToInsert, const FFString& keyToInsert, const ValT& valToInsert) {
        u32 uInsertionIndex = prepare_insert_not_present(uHashOfKeyToInsert);
        FFString* tTableKeys = _get_table_keys();
        ValT* tTableVals = _get_table_values();
        tTableKeys[uInsertionIndex] = keyToInsert;
        tTableVals[uInsertionIndex] = valToInsert;
        return iterator(_impl._ctrl + uInsertionIndex, tTableKeys + uInsertionIndex, tTableVals + uInsertionIndex);
    }

    FORCE_INLINE iterator insert_not_present(u64 uHashOfKeyToInsert, StringView keyToInsert, const ValT& valToInsert) {
        return insert_not_present(uHashOfKeyToInsert, FFString::fromAllocatorMakeFF(keyToInsert, _alloc), valToInsert);
    }

    iterator insert(StringView keyToInsert, const ValT& valToInsert) {
        u64 uHash = get_map_hash(keyToInsert);
        if (LIKELY(_impl._ctrl)) {
            iterator found = findHashed(uHash, keyToInsert);
            if (found != end()) {
                found.value() = valToInsert;
                return found;
            }
        }
        return insert_not_present(uHash, keyToInsert, valToInsert);
    }

    // Warning  : NOT same behaviour as std containers: fails on not found !!
    const ValT& operator[](StringView key) const {
        u64 uHash = get_map_hash(key);
        const_iterator found = findHashed(uHash, key);
        Assert_(found != end()); 
        return found.value();
    }

    // Warning  : NOT same behaviour as std containers: fails on not found !!
    ValT& operator[](StringView key) {
        u64 uHash = get_map_hash(key);
        iterator found = findHashed(uHash, key);
        Assert_(found != end()); 
        return found.value();
    }

    void remove_at_iter(iterator it) {
        u8* str_start = it.key().pStart;
        if (str_start)
            _alloc.deallocate(str_start - 8);
        _impl.size_lhv()--;
        _impl.non_null_set_ctrl(it._getIndex_from(_impl), u8(SetMetaDataImpl::kDeleted));
    }

    bool remove_at_key(StringView key) {
        u64 uHash = get_map_hash(key);
        iterator found = findHashed(uHash, key);
        if (found != end()) {
            remove_at_iter(found);
            return true;
        } else
            return false;
    }

    void resize(u32 uNewCapacity) {
        Assert_(HashSet_Impl::IsValidCapacity(uNewCapacity));
        if (_impl._ctrl) {
            AStringMap<ValT, AllocT> old = *this;
            _allocate(uNewCapacity);
            FFString* tTableKeys = _get_table_keys();
            ValT* tTableVals = _get_table_values();
            for (auto it = old.cbegin(), itEnd = old.cend(); it != itEnd; it++) {
                FFString key = it.key();
                u64 uHash = get_map_hash((StringView)key);
                u32 uIndex = prepare_insert_not_present_nogrow(uHash);
                tTableKeys[uIndex] = key;
                tTableVals[uIndex] = it.value();
            }
            old._release_tables();
        } else {
            _allocate(uNewCapacity);
        }
    }

    void _release_tables() {
        if (LIKELY(_impl._ctrl)) {
            _alloc.deallocate((u8*)_get_table_keys());
            _alloc.deallocate((u8*)_get_table_values());
            _alloc.deallocate(non_null_get_start_of_alloc());
            _impl._ctrl = 0;
        }
    }

    void release() {
        _release_allocated_strings();
        _release_tables();
    }
};

template<typename ValT> using StringMap = AStringMap<ValT, MallocPoweredAlloc>;
template<typename ValT> using TmpStringMap = AStringMap<ValT, FireAndForgetArenaAlloc>;
template<typename ValT> using DynStringMap = AStringMap<ValT, DynGPAlloc>;

template<typename AllocT>
struct AStringArray : public ArrayView<FFString> {

    AStringArray() = default;
    AStringArray(const AStringArray<AllocT>&) = default;
    AStringArray<AllocT>& operator=(const AStringArray<AllocT>&) = default;
    ~AStringArray() = default;

    AllocT _alloc;

    FORCE_INLINE AStringArray(AllocT alloc):ArrayView<FFString>(), _alloc(alloc) { _impl = ArrayImpl::initZero(); }

    FORCE_INLINE AllocT alloc() const { return _alloc; }
    FORCE_INLINE AllocT& alloc_ref() { return _alloc; }

    FORCE_INLINE void init(AllocT alloc) { _alloc = alloc; _impl = ArrayImpl::initZero(); }
    FORCE_INLINE void setAlloc(AllocT alloc) { _alloc = alloc; }
    FORCE_INLINE void clear() { resize_non_zeroing(0u); }
    FORCE_INLINE void release() { clear(); _impl.release(_alloc); }
    FORCE_INLINE void append(StringView elt) {
        FFString ownedElt = FFString::fromAllocatorMakeFF(elt, _alloc);
        if ((_impl._uSize + 1u) * sizeof(FFString) < _impl.uAllocSize)
            _append_no_check(ownedElt);
        else
            _append_after_grow(ownedElt);
    }
    FORCE_INLINE void insert(u32 uIndex, StringView elt) {
        FFString ownedElt = FFString::fromAllocatorMakeFF(elt, _alloc);
        if ((_impl._uSize + 1u) * sizeof(FFString) < _impl.uAllocSize)
            _insert_no_check(ownedElt);
        else
            _insert_after_grow_at(uIndex, ownedElt);
    }

    void reserve(u32 uReqEltCount, bool bReduceAllocIfLowerThanCurrent = false) {
        Assert_(uReqEltCount >= _impl._uSize);
        u64 uReqSize = u64(uReqEltCount) * sizeof(FFString);
        Assert_(uReqSize < 0x80000000uLL && "max payload of about 2 GB reached for DynArray");
        if (u32(uReqSize) > _impl.uAllocSize || (bReduceAllocIfLowerThanCurrent && u32(uReqSize) < _impl.uAllocSize && !_alloc.isFireAndForget()))
            _impl.realloc_for_append(_alloc, u32(uReqSize), _impl._uSize * sizeof(FFString), getAlign());
    }

    void resize(u32 uEltCount) {
        u32 uPrevSize = _impl._uSize;
        resize_non_zeroing(uEltCount);
        if (uPrevSize < uEltCount) {
            FFString* pStart = at(uPrevSize);
            u32 uDiff = uEltCount - uPrevSize;
            memset(pStart, 0, uDiff * sizeof(EltT));
        }
    }

    void resize_non_zeroing(u32 uEltCount) {
        if (uEltCount >= getCurrentCapacity()) {
            reserve(uEltCount);
        }
        u32 uPrevSize = _impl._uSize;
        if (!AllocT::is_ensured_fire_and_forget && !_alloc.isFireAndForget() && uEltCount < uPrevSize) {
            _free_strings_from(at(uPrevSize), uPrevSize - uEltCount);
        }
        _impl._uSize = uEltCount;
    }

protected:
    FORCE_INLINE u32 _get_alloc_for_growing_to(u32 uReqEltCount) const {
        u64 uReqSize = u64(uReqEltCount) * sizeof(FFString);
        Assert_(uReqSize < 0x80000000uLL && "max payload of about 2 GB reached for DynArray");
        u32 uPreviousAlloc = _max(_impl.uAllocSize, u32(16u * sizeof(FFString))); // first append would always grow to 16 elements
        u32 uReqAllocNow = (uPreviousAlloc * 7u) / 4u;  // growing by * 1.75 ? => 0, 16, 28, 49, 85, 148, 259, 453...
        if (uReqAllocNow < u32(uReqSize) || uReqAllocNow >= 0x80000000u)
            uReqAllocNow = u32(uReqSize); // also growing to exact size when trying to add a large amount (which does not fit the default growth scheme)
        return uReqAllocNow;
    }
    void _append_after_grow(FFString elt) {
        u32 uSize = _impl._uSize;
        u32 uReqAllocNow = _get_alloc_for_growing_to(uSize + 1u);
        _impl.realloc_for_append(_alloc, uReqAllocNow, uSize * sizeof(FFString), getAlign());
        u32 uIndex = uSize;
        _impl._uSize = uSize + 1u;
        _replace_all_nocheck_from(uIndex, 1u, &elt);
    };
    void _insert_after_grow_at(u32 uIndex, FFString elt) {
        u32 uSize = _impl._uSize;
        u32 uReqAllocNow = _get_alloc_for_growing_to(uSize + 1u);
        Assert_(uIndex < uSize);
        _impl.realloc_for_insert(_alloc, uReqAllocNow, uIndex * sizeof(FFString), (uIndex + 1u) * sizeof(FFString), (uSize - uIndex) * sizeof(FFString), getAlign());
        _impl._uSize = uSize + 1u;
        _replace_all_nocheck_from(uIndex, 1u, &elt);
    };
    void _free_strings_from(FFString* pFrom, u32 uCount) {
        for (u32 uIndex = 0; uIndex < uCount; uIndex++) {
            _alloc.deallocate(pFrom[uCount].pStart - 8);
        }
    }

};

typedef AStringArray<MallocPoweredAlloc> StringArray;
typedef AStringArray<FireAndForgetArenaAlloc> TmpStringArray;
typedef AStringArray<DynGPAlloc> DynStringArray;
