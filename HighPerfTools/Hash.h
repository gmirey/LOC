//
// This implementation is derived from xxHash
//
/*
 * xxHash - Extremely Fast Hash algorithm
 * Development source file for `xxh3`
 * Copyright (C) 2019-2021 Yann Collet
 *
 * BSD 2-Clause License (https://www.opensource.org/licenses/bsd-license.php)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above
 *      copyright notice, this list of conditions and the following disclaimer
 *      in the documentation and/or other materials provided with the
 *      distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * You can contact the author at:
 *   - xxHash homepage: https://www.xxhash.com
 *   - xxHash source repository: https://github.com/Cyan4973/xxHash
 */

#pragma once

#include "BaseDecls.h"
#include "Platform.h"
#include "Arenas.h"
#include "StringView.h"

#include <stdlib.h>
#ifdef __has_builtin
#  define HAS_BUILTIN(x) __has_builtin(x)
#else
#  define HAS_BUILTIN(x) 0
#endif

#if !defined(NO_CLANG_BUILTIN) && HAS_BUILTIN(__builtin_rotateleft32) \
                               && HAS_BUILTIN(__builtin_rotateleft64)
#  define ROTL_32 __builtin_rotateleft32
#  define ROTL_64 __builtin_rotateleft64
/* Note: although _rotl exists for minGW (GCC under windows), performance seems poor */
#elif defined(_MSC_VER)
#  define ROTL_32(x,r) _rotl(x,r)
#  define ROTL_64(x,r) _rotl64(x,r)
#else
#  define ROTL_32(x,r) (((x) << (r)) | ((x) >> (32 - (r))))
#  define ROTL_64(x,r) (((x) << (r)) | ((x) >> (64 - (r))))
#endif

#define XXH_PRIME64_1  0x9E3779B185EBCA87ULL  /*!< 0b1001111000110111011110011011000110000101111010111100101010000111 */
#define XXH_PRIME64_2  0xC2B2AE3D27D4EB4FULL  /*!< 0b1100001010110010101011100011110100100111110101001110101101001111 */
#define XXH_PRIME64_3  0x165667B19E3779F9ULL  /*!< 0b0001011001010110011001111011000110011110001101110111100111111001 */
#define XXH_PRIME64_4  0x85EBCA77C2B2AE63ULL  /*!< 0b1000010111101011110010100111011111000010101100101010111001100011 */
#define XXH_PRIME64_5  0x27D4EB2F165667C5ULL  /*!< 0b0010011111010100111010110010111100010110010101100110011111000101 */

static FORCE_INLINE u64 XXH64_round(u64 acc, u64 input)
{
    acc += input * XXH_PRIME64_2;
    acc  = ROTL_64(acc, 31);
    acc *= XXH_PRIME64_1;
    return acc;
}
// Not used for MapHash
static u64 XXH64_mergeRound(u64 acc, u64 val)
{
    val  = XXH64_round(0, val);
    acc ^= val;
    acc  = acc * XXH_PRIME64_1 + XXH_PRIME64_4;
    return acc;
}
static FORCE_INLINE u64 XXH64_avalanche(u64 hash)
{
    hash ^= hash >> 33;
    hash *= XXH_PRIME64_2;
    hash ^= hash >> 29;
    hash *= XXH_PRIME64_3;
    hash ^= hash >> 32;
    return hash;
}

#include <cstring>

static FORCE_INLINE u64 XXH64_finalize_round_64(u64 hash, u64 uValue) {
    hash ^= XXH64_round(0, uValue);
    hash  = ROTL_64(hash,27) * XXH_PRIME64_1 + XXH_PRIME64_4;
    return hash;
}
static FORCE_INLINE u64 XXH64_finalize_round_32(u64 hash, u32 uValue) {
    hash ^= uValue * XXH_PRIME64_1;
    hash = ROTL_64(hash, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
    return hash;
}
static FORCE_INLINE u64 XXH64_finalize_round_8(u64 hash, u8 uValue) {
    hash ^= uValue * XXH_PRIME64_5;
    hash = ROTL_64(hash, 23) * XXH_PRIME64_2 + XXH_PRIME64_3;
    return hash;
}

template<bool bAligned>
static u64 XXH64_finalizeLast31Bytes_(u64 hash, const u8* pValue, u64 uByteCount) {
    Assert_(pValue || uByteCount == 0u);
    Assert_(uByteCount < 32u);
    Assert_(!bAligned || 0 == (reinterpret_cast<UParam>(pValue) & 0x07u));
    u32 uTmp32;
    u64 uTmp64;

#define PROCESS_XXH64_FINALIZE_8 \
    hash = XXH64_finalize_round_8(hash, *pValue); pValue++
#define PROCESS_XXH64_FINALIZE_32 \
    if (bAligned) { uTmp32 = *reinterpret_cast<const u32*>(pValue); } else { memcpy(&uTmp32, pValue, 4u); } \
    hash = XXH64_finalize_round_32(hash, uTmp32); pValue += 4
#define PROCESS_XXH64_FINALIZE_64 \
    if (bAligned) { uTmp64 = *reinterpret_cast<const u64*>(pValue); } else { memcpy(&uTmp64, pValue, 8u); } \
    hash = XXH64_finalize_round_64(hash, uTmp64); pValue += 8

    switch (uByteCount & 31u) {
      case 24: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 16: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case  8: PROCESS_XXH64_FINALIZE_64;
               break;

      case 28: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 20: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 12: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case  4: PROCESS_XXH64_FINALIZE_32;
               break;

      case 25: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 17: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case  9: PROCESS_XXH64_FINALIZE_64;
               PROCESS_XXH64_FINALIZE_8;
               break;

      case 29: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 21: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 13: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case  5: PROCESS_XXH64_FINALIZE_32;
               PROCESS_XXH64_FINALIZE_8;
               break;

      case 26: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 18: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 10: PROCESS_XXH64_FINALIZE_64;
               PROCESS_XXH64_FINALIZE_8;
               PROCESS_XXH64_FINALIZE_8;
               break;

      case 30: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 22: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 14: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case  6: PROCESS_XXH64_FINALIZE_32;
               PROCESS_XXH64_FINALIZE_8;
               PROCESS_XXH64_FINALIZE_8;
               break;

      case 27: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 19: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 11: PROCESS_XXH64_FINALIZE_64;
               PROCESS_XXH64_FINALIZE_8;
               PROCESS_XXH64_FINALIZE_8;
               PROCESS_XXH64_FINALIZE_8;
               break;

      case 31: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 23: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case 15: PROCESS_XXH64_FINALIZE_64; // fallthrough
      case  7: PROCESS_XXH64_FINALIZE_32; // fallthrough
      case  3: PROCESS_XXH64_FINALIZE_8;  // fallthrough
      case  2: PROCESS_XXH64_FINALIZE_8;  // fallthrough
      case  1: PROCESS_XXH64_FINALIZE_8;  // fallthrough
      case  0: break;
    }

    return XXH64_avalanche(hash);
}
; // template termination

static FORCE_INLINE u64 XXH64_finalizeFour(u64 hash, u64 uValue0, u64 uValue1, u64 uValue2, u64 uValue3) {
    hash = XXH64_finalize_round_64(hash, uValue0);
    hash = XXH64_finalize_round_64(hash, uValue1);
    hash = XXH64_finalize_round_64(hash, uValue2);
    hash = XXH64_finalize_round_64(hash, uValue3);
    return XXH64_avalanche(hash);
}

static u64 XXH64_finalizeFourSamplesOf8Bytes(u64 uHash, const u8* pStart, u64 uSize) {
    Assert_(uSize >= 32u);
    u64 uLeftOver = uSize - 32u;
    u64 uMiddleLeftOver = uLeftOver >> 1u;
    u64 uRemainingLeftOver = uLeftOver - uMiddleLeftOver;
    u64 uLeftLeftOver = uRemainingLeftOver >> 1u;
    u64 uRightLeftOver = uRemainingLeftOver - uLeftLeftOver;
    u64 uValue0, uValue1, uValue2, uValue3;
    memcpy(&uValue0, pStart, 8u);
    memcpy(&uValue1, pStart + 8u + uLeftLeftOver, 8u);
    const u8* pEnd = pStart + uSize; 
    memcpy(&uValue2, pEnd - 16u - uRightLeftOver, 8u);
    memcpy(&uValue3, pEnd - 8u, 8u);
    return XXH64_finalizeFour(uHash, uValue0, uValue1, uValue2, uValue3);
}

template<bool bAligned>
static FORCE_INLINE u64 getFastHashFromBytes_(const u8* pStart, u64 uSize, u64 uHashSeed) {
    Assert_(!bAligned || 0 == (reinterpret_cast<UParam>(pStart) & 0x07u));
    u64 uInitialHash = uHashSeed + XXH_PRIME64_5 + uSize;
    if (uSize < 32u)
        return XXH64_finalizeLast31Bytes_<bAligned>(uInitialHash, pStart, uSize);
    else
        return XXH64_finalizeFourSamplesOf8Bytes(uInitialHash, pStart, uSize);
}
; // template termination

template<bool bAligned>
static u64 getFullHashFromBytes_(const u8* pStart, u64 uSize, u64 uHashSeed) {
    Assert_(!bAligned || 0 == (reinterpret_cast<UParam>(pStart) & 0x07u));
    u64 uHash;
    if (uSize < 32u)
        uHash = uHashSeed + XXH_PRIME64_5;
    else {
        u64 v1 = uHashSeed + XXH_PRIME64_1 + XXH_PRIME64_2;
        u64 v2 = uHashSeed + XXH_PRIME64_2;
        u64 v3 = uHashSeed + 0;
        u64 v4 = uHashSeed - XXH_PRIME64_1;

#define PROCESS_XXH64_ROUND_64(v) do { \
            u64 uTmpVal; if (bAligned) { uTmpVal = *reinterpret_cast<const u64*>(p); } else { memcpy(&uTmpVal, p, 8); } \
            v = XXH64_round(v, uTmpVal); p += 8; \
        } while (0)

        const u8* bEnd = pStart + uSize;
        const u8* const limit = bEnd - 32;
        const u8* p = pStart;
        do {
            PROCESS_XXH64_ROUND_64(v1);
            PROCESS_XXH64_ROUND_64(v2);
            PROCESS_XXH64_ROUND_64(v3);
            PROCESS_XXH64_ROUND_64(v4);
        } while (p<=limit);

        uHash = ROTL_64(v1, 1) + ROTL_64(v2, 7) + ROTL_64(v3, 12) + ROTL_64(v4, 18);
        uHash = XXH64_mergeRound(uHash, v1);
        uHash = XXH64_mergeRound(uHash, v2);
        uHash = XXH64_mergeRound(uHash, v3);
        uHash = XXH64_mergeRound(uHash, v4);
    }
    uHash += uSize;
    return XXH64_finalizeLast31Bytes_<bAligned>(uHash, pStart, uSize);
}
; // template termination

static FORCE_INLINE u64 getFastHashFromNonNullSlice(Slice<u8> value, u64 uHashSeed) {
    const u8* pStart = value.start;
    u64 uSize = value.count;
    if (reinterpret_cast<UParam>(pStart) & 0x07u)
        return getFastHashFromBytes_<false>(pStart, uSize, uHashSeed);
    else
        return getFastHashFromBytes_<true>(pStart, uSize, uHashSeed);
}

static FORCE_INLINE u64 getFastHashFromSlice(Slice<u8> value, u64 uHashSeed) {
    if (value.is_null())
        return 0x4A54deadbeef4A54 + uHashSeed;   // Special hash for null slices (and de facto, null strings)
    else
        return getFastHashFromNonNullSlice(value, uHashSeed);
}

static FORCE_INLINE u64 getFastHashFromNonNullString(StringView value, u64 uHashSeed) {
    const u8* pStart = value.start;
    u64 uSize = u64(value.uByteLength);
    if (reinterpret_cast<UParam>(pStart) & 0x07u)
        return getFastHashFromBytes_<false>(pStart, uSize, uHashSeed);
    else
        return getFastHashFromBytes_<true>(pStart, uSize, uHashSeed);
}

static FORCE_INLINE u64 getFastHashFromString(StringView value, u64 uHashSeed) {
    if (value.is_null())
        return 0x4A54deadbeef4A54 + uHashSeed;   // Special hash for null slices (and de facto, null strings)
    else
        return getFastHashFromNonNullString(value, uHashSeed);
}

static FORCE_INLINE u64 getFullHashFromNonNullSlice(Slice<u8> value, u64 uHashSeed) {
    const u8* pStart = value.start;
    u64 uSize = value.count;
    if (reinterpret_cast<UParam>(pStart) & 0x07u)
        return getFullHashFromBytes_<false>(pStart, uSize, uHashSeed);
    else
        return getFullHashFromBytes_<true>(pStart, uSize, uHashSeed);
}

static FORCE_INLINE u64 getFullHashFromSlice(Slice<u8> value, u64 uHashSeed) {
    if (value.is_null())
        return 0x4A54deadbeef4A54 + uHashSeed;   // Special hash for null slices (and de facto, null strings)
    else
        return getFullHashFromNonNullSlice(value, uHashSeed);
}

static FORCE_INLINE u64 getFullHashFromNonNullString(StringView value, u64 uHashSeed) {
    const u8* pStart = value.start;
    u64 uSize = u64(value.uByteLength);
    if (reinterpret_cast<UParam>(pStart) & 0x07u)
        return getFullHashFromBytes_<false>(pStart, uSize, uHashSeed);
    else
        return getFullHashFromBytes_<true>(pStart, uSize, uHashSeed);
}

static FORCE_INLINE u64 getFullHashFromString(StringView value, u64 uHashSeed) {
    if (value.is_null())
        return 0x4A54deadbeef4A54 + uHashSeed;   // Special hash for null slices (and de facto, null strings)
    else
        return getFullHashFromNonNullString(value, uHashSeed);
}

static FORCE_INLINE u64 getHashForUpTo64bValue(u64 value, u64 uHashSeed) {
    u64 uInitialHash = uHashSeed + XXH_PRIME64_5 + 8u;
    u64 hash = XXH64_finalize_round_64(uInitialHash, value);
    return XXH64_avalanche(hash);
}

template<typename T>
static FORCE_INLINE u64 getHashForOtherValue(const T& value, u64 uHashSeed) {
    const u8* pStart = reinterpret_cast<const u8*>(&value);
    if (alignof(T) >= 8 || alignof(T) == sizeof(T) || 0 == (reinterpret_cast<UParam>(pStart) & 0x07u))
        return getFullHashFromBytes_<true>(pStart, sizeof(T), uHashSeed);
    else
        return getFullHashFromBytes_<false>(pStart, sizeof(T), uHashSeed);
}
; // template termination

template<typename T>
static FORCE_INLINE u64 getFastHashFor32BytesValueOrLarger(const T& value, u64 uHashSeed) {
    u64 uInitialHash = uHashSeed + XXH_PRIME64_5 + sizeof(T);
    const u8* pStart = reinterpret_cast<const u8*>(&value);
    return XXH64_finalizeFourSamplesOf8Bytes(uInitialHash, pStart, sizeof(T));
}
; // template termination

