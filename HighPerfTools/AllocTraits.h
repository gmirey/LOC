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

// Required traits for an "Allocator":
// An AllocType abiding to Allocator should be a struct or class, possibly carrying some additional properties,
//   and defining the following:
//
// - an allocation method for a given number of bytes, with a given alignment requirement
//      u8* allocate(u64 uSize, u64 uAlign)
// - a query method for the fire-and-forget or malloc/free-like nature of the allocation strategy
//      bool isFireAndForget() const
// - a deallocation method for any previously returned pointer from an 'allocate' call on that same allocator
//      (typically NOOP for fire-and-forget strategies).
//      void deallocate(u8*)
// - a default instance retrieval method (not required to have a working implementation, but must be present syntactically)
//      static AllocType getDefaultInstance() 
// - a default-method retrieval method 'getFreeFn' for the allocation method to use in case of free, and 'getFreeParams' for its associated parameter
// - static const booleans has_default_instance and is_ensured_fire_and_forget
// - a to-default conversion method 'toDefaultDyn' for being seen as a runtime-polymorphic-equivalent
// - a to-default conversion method 'toFFADyn' for being seen as a runtime-polymorphic-equivalent, in case is_ensured_fire_and_forget is true

// the following allocators are, by default, provided:
struct MallocPoweredAlloc;        // behaves like traditional C or C++ strategies. For when a better plan wasn't thought about at all.
struct FireAndForgetArenaAlloc;   // when our problem has reasonable memory usage and a well defined lifecycle and sharing scheme.
struct GeneralPurposeArenaAlloc;  // when we require generic-alloc capability, but know precisely about memory isolation. (single-thread in particular).
struct MTGeneralPurposeAlloc;     // when we require generic-alloc capability, but cannot guarantee structural thread safety and need mutexes on each alloc.
struct DynScratchAlloc;           // when we could do with FireAndForgetArenaAlloc capabilities, but also want to allow distinct implementations at runtime.
struct DynGPAlloc;                // when we want to allow any allocation strategy at runtime.
struct DummyNoAlloc;              // when an Allocator template argument is syntactically required, but actually not used.

// Detailed explanations:
//
// 'MallocPoweredAlloc' is intendend to behave like "traditional" C or C++, by calling the standard library malloc() and free() for allocation or deallocation.
//   it thus brings the slight benefit from the no-data-overhead of this approach (since we can call malloc() without any additional info), however, using this scheme
//   is usually discouraged: malloc is indeed overly generic, and despite the often discussed pitfalls of C memory management, is intended for a kind of
//   convenience where we do not want to think that hard about memory, be it usage, allocation, or the question of 'where it resides'.
//   When we *do* know anything about a problem context (and, when trying to solve any problem, we should know a great deal about that), we also usually have enough
//   information that we could opt for much faster, and sometimes even much "simpler" (and less wasteful) allocation strategies, if we cared at all about either
//   performance or simplicity. So, please, consider one of the other choices below:
//
// 'FireAndForgetArenaAlloc' will statically tie the allocation calls to that of an associated Arena (and thus statically NOOP on deallocation).
//   One should definitely read Arena documentation for more info, but to sum up, there are many cases where we can know that a given problem
//   requires reasonnable amount of memory, and that a simple memory arena could be used for that purpose (assuming, of course, good knowledge of who else has
//   usage of that arena, and of whether and when that arena may get partially or entirely reset). This allocator will bring, by far, the fastest performance,
//   and the additionnal benefit that having thought about that arena usage and resetting-scheme and our specific problem (at least enough to establish
//   that it does indeed fit well in that scheme), is generally good for our health. And sanity. And that of whole projects and software applications.
//
// 'GeneralPurposeArenaAlloc' will provide "general-purpose" allocation capability, as malloc does. However, it will tie the allocation call to that of an associated
//   ChunkProvider or even Arena, and will thus typically be faster than malloc. This performance gain is due to the fact that, first, there is no automatic thread safety mechanism
//   overhead, since it is assumed we could establish that our solution *does* manage memory in isolation from other threads. Next, we use a convenient (and stable across
//   platforms) separated-bucket-based implementation, solving most fragmentation issues, and ensure that its performance and memory usage is kept regular in case of prolonged
//   use. We also get the additional benefit, as with all arenas, of being able to segregate our usage of such allocations between distinct groups, which we can organize
//   by functionnality, or segregate per thread, and more generally manage independently (including global "resets" facility much like Arena provides)
//
// 'MTGeneralPurposeAlloc' is similar to the above, but somewhat slower, as it will enforce the usage of mutexes upon each allocation or deallocation operation.
//   It is provided as convenience in such cases where we cannot solve those issues better, but it is strongly advised to simply make sure that we segregate our
//   allocation concerns between threads, and then use 'GeneralPurposeAlloc' instead (as this will ALSO usually improve the simplicity, clarity, and overall architecture
//   of an application). As a last argument against it, note that segregated thread allocations required by the preferred 'GeneralPurposeAlloc' does not even forbid
//   one's code to allow other threads to "access" the data allocated by another (provided we protect this shared usage in an independent manner)
//
// 'DynScratchAlloc' is similar to 'FireAndForgetArenaAlloc', with the additional flexibility that it is runtime-polymorphic to the actual implementation
//   of any fire and forget allocator. One can thus provide different fire and forget schemes if we require something other than the default FireAndForgetArena,
//   or, from the standpoint of an API, allow this same flexibility as a non-templatized version of this polymorphism would. It comes at the cost of
//   some additional property to be carried on each structure using those alloctors (containers in particular), and a slight runtime speed hit.
//
// 'DynGPAlloc', like 'DynScratchAlloc', also provides runtime polymorphism to allocators, but this time also including all general purpose schemes,
//   requiring an actual deallocation call. It *does* actually also support fire and forget strategies (those strategies simply take the slight overhead of an actual
//   call to a non-inlinable deallocation method, returning after their typical NOOP). It is thus the 'ultimate' runtime-polymorphic allocator, and is thus advised to
//   use at API boundaries for unspecified use-cases. Also note that, anticipating the likeliness of this scenario, the various types using other allocators (containers in
//   particular) provide a convenient auto-cast mechanism to their 'DynGPAlloc' version.
//
// Important additional option to these choices:
//   In a large number of cases, one would also find that a given problem can be solved using stack-local containers (ie, the callers down the stack will
//   never need to have a direct ref to that data), and that the most common use case typically requires low amounts of memory. In that case, nothing would beat
//   a few hundreds bytes allocated on the stack itself. Most of the default containers will thus propose an extended version of themselves, allowing us to get
//   that ultimate performance gain, with a slight additional runtime mechanism on the base container to account for that situation (in particular,
//   preventing that a deallocation call would ever be performed on such stack-allocated data). Note that, in the case such stack-allocated space is not sufficient,
//   all of those containers would still benefit from their own, regular "allocators" as backup. Although it is also possible to use them in combination with
//   a 'DummyNoAlloc' a backup, if we're absolutely positive than no more memory will ever be needed (and would prefer to clearly assert on breaking that law,
//   instead of seeing performance degrade unexpectedly). 
//   Those "start-with-small-stack-prealloc" versions are also designed to be polymorphically usable at API boundaries requiring the base version.
//   

// ------------------------
// Definition of a runtime-polymorphic fire and forget allocator function
typedef u8* (*FireAndForgetAllocSign)	(u64 uSize, u64 uAlign, u64 uAllocParametersOrPtr);
// ------------------------

//pre-declaration of default implementations provided for the fire and forget allocator function
u8* fire_and_forget_arena_alloc(u64 uSize, u64 uAlign, u64 uAllocParams);
u8* dummy_fire_and_forget_no_alloc(u64 uSize, u64 uAlign, u64 uAllocParams);

// ------------------------
// Definition of a runtime-polymorphic general allocator function
enum EAllocAndFreeOp {
    OP_ALLOC,
    OP_FREE
};
typedef u8* (*AllocAndFreeFuncSign)	(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParams);
// ------------------------

//pre-declaration of default implementations provided for the general allocator functions
u8* stdlib_alloc_and_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParametersOrPtr);
u8* general_purpose_arena_alloc_and_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParametersOrPtr);
u8* mt_general_purpose_arena_alloc_and_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParametersOrPtr);
u8* fire_and_forget_arena_alloc_and_noopfree(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParametersOrPtr);
u8* dummy_no_alloc_no_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParametersOrPtr);

// TODO: isolate that dependency to stdlib somewhere else ?
#include <stdlib.h> // for aligned_alloc and free
// 'MallocPoweredStaticAlloc' : abides to required traits for an "Allocator", and can provide a static instance
// behaves like traditional C or C++. ie, one *could* use it when a better plan for memory allocation wasn't thought about.
struct MallocPoweredAlloc
{
    MallocPoweredAlloc() = default;
    MallocPoweredAlloc(const MallocPoweredAlloc&) = default;
    MallocPoweredAlloc& operator=(const MallocPoweredAlloc&) = default;
    ~MallocPoweredAlloc() = default;

#if defined(_MSC_VER)  // Visual Studio
#include <malloc.h>    // for _aligned_alloc and _aligned_free
    FORCE_INLINE u8* allocate(u64 uSize, u64 uAlign) const  { return reinterpret_cast<u8*>(_aligned_malloc(uSize, uAlign)); }
    FORCE_INLINE void deallocate(u8* pPtr) const  { _aligned_free(pPtr); }
#else
    FORCE_INLINE u8* allocate(u64 uSize, u64 uAlign) const  { return reinterpret_cast<u8*>(aligned_malloc(uSize, uAlign)); }
    FORCE_INLINE void deallocate(u8* pPtr) const  { free(pPtr); }
#endif
    bool isFireAndForget() const { return false; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return stdlib_alloc_and_free; }
    FORCE_INLINE u64 getFreeParams() const { return 0u; }
    static FORCE_INLINE MallocPoweredAlloc getDefaultInstance()  { return MallocPoweredAlloc(); }
    DynGPAlloc toDefaultDyn(); // conversion to DynGPAlloc
    static const bool has_default_instance = true;
    static const bool is_ensured_fire_and_forget = false;
};

// 'FireAndForgetStaticAlloc' : abides to required traits for an "Allocator"
// when our problem has reasonable memory usage and a well defined lifecycle and sharing scheme.
struct FireAndForgetArenaAlloc
{
    FireAndForgetArenaAlloc() = default;
    FireAndForgetArenaAlloc(const FireAndForgetArenaAlloc&) = default;
    FireAndForgetArenaAlloc& operator=(const FireAndForgetArenaAlloc&) = default;
    ~FireAndForgetArenaAlloc() = default;
    
    FORCE_INLINE FireAndForgetArenaAlloc(Arena theArena):arena(theArena) {}

    Arena arena;
    FORCE_INLINE u8* allocate(u64 uSize, u64 uAlign)  {
        Assert_(0 == (uSize & 0xFFFFFFFF00000000uLL));
        Assert_(0 == (uAlign & 0xFFFFFFFFFFFF0000uLL));
        return alloc_from(arena, u32(uSize), u16(uAlign));
    }
    FORCE_INLINE void deallocate(u8* pPtr) const  { } // NOOP
    FORCE_INLINE bool isFireAndForget() const  { return true; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return fire_and_forget_arena_alloc_and_noopfree; }
    FORCE_INLINE u64 getFreeParams() const { return 0u; }
    static FORCE_INLINE FireAndForgetArenaAlloc getDefaultInstance()  { Assert_(false); return FireAndForgetArenaAlloc(); }
    DynScratchAlloc toFFADyn(); // conversion to DynScratchAlloc
    DynGPAlloc toDefaultDyn(); // conversion to DynGPAlloc
    static const bool has_default_instance = false;
    static const bool is_ensured_fire_and_forget = true;
};

// 'GeneralStaticAlloc' : abides to required traits for an "Allocator"
// when we require generic-alloc capability, but know precisely about memory isolation. (single-thread in particular).
struct GeneralPurposeArenaAlloc
{
    GeneralPurposeArenaAlloc() = default;
    GeneralPurposeArenaAlloc(const GeneralPurposeArenaAlloc&) = default;
    GeneralPurposeArenaAlloc& operator=(const GeneralPurposeArenaAlloc&) = default;
    ~GeneralPurposeArenaAlloc() = default;
    
    // TODO, when GeneralPurposeArena becomes available
    FORCE_INLINE bool isFireAndForget() const  { return false; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return general_purpose_arena_alloc_and_free; }
    FORCE_INLINE u64 getFreeParams() const { return 0; /* TODO */ }
    static FORCE_INLINE GeneralPurposeArenaAlloc getDefaultInstance()  { Assert_(false); return GeneralPurposeArenaAlloc(); }
    DynGPAlloc toDefaultDyn(); // conversion to DynGPAlloc
    static const bool has_default_instance = false;
    static const bool is_ensured_fire_and_forget = false;
};

// 'ThreadSafeGeneralStaticAlloc' : abides to required traits for an "Allocator"
// when we require generic-alloc capability, but cannot guarantee structural thread safety and need mutexes.
struct MTGeneralPurposeArenaAlloc
{
    MTGeneralPurposeArenaAlloc() = default;
    MTGeneralPurposeArenaAlloc(const MTGeneralPurposeArenaAlloc&) = default;
    MTGeneralPurposeArenaAlloc& operator=(const MTGeneralPurposeArenaAlloc&) = default;
    ~MTGeneralPurposeArenaAlloc() = default;

    // TODO, when GeneralPurposeArena becomes available
    FORCE_INLINE bool isFireAndForget() const  { return false; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return mt_general_purpose_arena_alloc_and_free; }
    FORCE_INLINE u64 getFreeParams() const { return 0u; /* TODO */ }
    static FORCE_INLINE MTGeneralPurposeArenaAlloc getDefaultInstance()  { Assert_(false); return MTGeneralPurposeArenaAlloc(); }
    DynGPAlloc toDefaultDyn(); // conversion to DynGPAlloc
    static const bool has_default_instance = false;
    static const bool is_ensured_fire_and_forget = false;
};

// 'DummyNoAlloc' : abides to required traits for an "Allocator", and can provide a static instance
// when an Allocator template argument is syntactically required, but actually not used.
struct DummyNoAlloc
{
    DummyNoAlloc() = default;
    DummyNoAlloc(const DummyNoAlloc&) = default;
    DummyNoAlloc& operator=(const DummyNoAlloc&) = default;
    ~DummyNoAlloc() = default;

    FORCE_INLINE u8* allocate(u64 uSize, u64 uAlign) const  { Assert_(false); return 0; }
    FORCE_INLINE void deallocate(u8* pPtr) const  { } // NOOP
    FORCE_INLINE bool isFireAndForget() const  { return true; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return dummy_no_alloc_no_free; }
    FORCE_INLINE u64 getFreeParams() const { return 0u; }
    static FORCE_INLINE DummyNoAlloc getDefaultInstance()  { return DummyNoAlloc(); }
    DynScratchAlloc toFFADyn(); // conversion to DynScratchAlloc
    DynGPAlloc toDefaultDyn(); // conversion to DynGPAlloc
    static const bool has_default_instance = true;
    static const bool is_ensured_fire_and_forget = true;
};

// 'DynScratchAlloc' : abides to required traits for an "Allocator"
// when we could use FireAndForgetStaticAlloc, but also want to allow distinct implementations at runtime.
struct DynScratchAlloc
{
    DynScratchAlloc() = default;
    DynScratchAlloc(const DynScratchAlloc&) = default;
    DynScratchAlloc& operator=(const DynScratchAlloc&) = default;
    ~DynScratchAlloc() = default;

    FORCE_INLINE DynScratchAlloc(FireAndForgetAllocSign theAllocFn, u64 theAllocParams):pAllocFn(theAllocFn), uAllocParams(theAllocParams) {}

    FireAndForgetAllocSign pAllocFn;
    u64 uAllocParams;
    FORCE_INLINE u8* allocate(u64 uSize, u64 uAlign)  {
        return pAllocFn(uSize, uAlign, uAllocParams);
    }
    FORCE_INLINE void deallocate(u8* pPtr) const  { } // NOOP
    FORCE_INLINE bool isFireAndForget() const { return true; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return dummy_no_alloc_no_free; }
    FORCE_INLINE u64 getFreeParams() const { return 0u; }
    static FORCE_INLINE DynScratchAlloc getDefaultInstance()  { Assert_(false); return DynScratchAlloc(); }
    FORCE_INLINE DynScratchAlloc toFFADyn() { return *this; }
    DynGPAlloc toDefaultDyn(); // conversion to DynGPAlloc
    static const bool has_default_instance = false;
    static const bool is_ensured_fire_and_forget = true;
};

// 'DynGPAlloc' : abides to required traits for an "Allocator"
// when we want to allow any allocation strategy at runtime.
struct DynGPAlloc
{
    DynGPAlloc() = default;
    DynGPAlloc(const DynGPAlloc&) = default;
    DynGPAlloc& operator=(const DynGPAlloc&) = default;
    ~DynGPAlloc() = default;

    FORCE_INLINE DynGPAlloc(AllocAndFreeFuncSign theAllocFn, u64 theAllocParams):pAllocAndFreeFn(theAllocFn), uAllocParams(theAllocParams) {}

    AllocAndFreeFuncSign pAllocAndFreeFn;
    u64 uAllocParams;
    FORCE_INLINE u8* allocate(u64 uSize, u64 uAlign)  {
        return pAllocAndFreeFn(uSize, EAllocAndFreeOp::OP_ALLOC, uAlign, uAllocParams);
    }
    FORCE_INLINE u8* deallocate(u8* pPtr)  {
        return pAllocAndFreeFn(u64(pPtr), EAllocAndFreeOp::OP_FREE, 0u, uAllocParams);
    }
    FORCE_INLINE bool isFireAndForget() const  { return false; }
    FORCE_INLINE AllocAndFreeFuncSign getFreeFn() const { return pAllocAndFreeFn; }
    FORCE_INLINE u64 getFreeParams() const { return uAllocParams; }
    static FORCE_INLINE DynGPAlloc getDefaultInstance()  { Assert_(false); return DynGPAlloc(); }
    FORCE_INLINE DynGPAlloc toDefaultDyn() { return *this; }
    static const bool has_default_instance = false;
    static const bool is_ensured_fire_and_forget = false;
    static FORCE_INLINE DynGPAlloc dummyNoAlloc() {
        DynGPAlloc result;
        result.pAllocAndFreeFn = dummy_no_alloc_no_free;
        result.uAllocParams = 0u;
        return result;
    }
};

// implementation of the fire-and-forget allocator function with our fire and forget arenas
static u8* fire_and_forget_arena_alloc(u64 uSize, u64 uAlign, u64 uAllocParams) {
    FireAndForgetArenaAlloc ffaPoweredAllocator;
    ffaPoweredAllocator.arena.root_chunk_handle.uTagged = uAllocParams;
    return ffaPoweredAllocator.allocate(uSize, uAlign);
}

// dummy assert-on-call implementation of the fire-and-forget allocator function
static u8* dummy_fire_and_forget_no_alloc(u64 uSize, u64 uAlign, u64 uAllocParams) {
    UNUSED(uAllocParams);
    UNUSED(uSize);
    UNUSED(uAlign);
    Assert_(false);
    return 0;
}

// implementation of the general allocator function with the legacy malloc/free approach
static u8* stdlib_alloc_and_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParams) {
    UNUSED(uAllocParams);
    MallocPoweredAlloc mallocPoweredAllocator;
    if (eOp == EAllocAndFreeOp::OP_ALLOC)
        return mallocPoweredAllocator.allocate(uSizeOrPtrToFree, uAlign);
    else {
        UNUSED(uAlign);
        Assert_(eOp == EAllocAndFreeOp::OP_FREE);
        mallocPoweredAllocator.deallocate((u8*)uSizeOrPtrToFree);
    }
    return 0;
}

// implementation of the general allocator function with our general purpose arenas
static u8* general_purpose_arena_alloc_and_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParams) {
    // TODO, when GeneralPurposeArena becomes available
    UNUSED(uSizeOrPtrToFree);
    UNUSED(eOp);
    UNUSED(uAlign);
    UNUSED(uAllocParams);
    Assert_(false);
    return 0;
}

// implementation of the general allocator function with our MultiThreaded general purpose arenas
static u8* mt_general_purpose_arena_alloc_and_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParams) {
    // TODO, when GeneralPurposeArena becomes available
    UNUSED(uSizeOrPtrToFree);
    UNUSED(eOp);
    UNUSED(uAlign);
    UNUSED(uAllocParams);
    Assert_(false);
    return 0;
}

// implementation of the general allocator function with our fire and forget arenas
static u8* fire_and_forget_arena_alloc_and_noopfree(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParams) {
    if (eOp == EAllocAndFreeOp::OP_ALLOC) {
        FireAndForgetArenaAlloc ffaPoweredAllocator;
        ffaPoweredAllocator.arena.root_chunk_handle.uTagged = uAllocParams;
        return ffaPoweredAllocator.allocate(uSizeOrPtrToFree, uAlign);
    } else {
        UNUSED(uAlign);
        Assert_(eOp == EAllocAndFreeOp::OP_FREE);
        UNUSED(uSizeOrPtrToFree);
        // NOOP
    }
    return 0;
}

// dummy assert-on-alloc implementation of the general allocator function, and noop on free
static u8* dummy_no_alloc_no_free(u64 uSizeOrPtrToFree, EAllocAndFreeOp eOp, u64 uAlign, u64 uAllocParams) {
    UNUSED(uAllocParams);
    UNUSED(uSizeOrPtrToFree);
    UNUSED(uAlign);
    if (eOp == EAllocAndFreeOp::OP_ALLOC)
        Assert_(false);
    else
        Assert_(eOp == EAllocAndFreeOp::OP_FREE);
    return 0;
}

// conversion from MallocPoweredAlloc to DynGPAlloc
FORCE_INLINE DynGPAlloc MallocPoweredAlloc::toDefaultDyn() {
    DynGPAlloc result;
    result.pAllocAndFreeFn = stdlib_alloc_and_free;
    result.uAllocParams = 0u;
    return result;
};

// conversion from FireAndForgetArenaAlloc to DynScratchAlloc
FORCE_INLINE DynScratchAlloc FireAndForgetArenaAlloc::toFFADyn() {
    DynScratchAlloc result;
    result.pAllocFn = fire_and_forget_arena_alloc;
    result.uAllocParams = this->arena.root_chunk_handle.uTagged;
    return result;
};

// conversion from FireAndForgetArenaAlloc to DynGPAlloc
FORCE_INLINE DynGPAlloc FireAndForgetArenaAlloc::toDefaultDyn() {
    DynGPAlloc result;
    result.pAllocAndFreeFn = fire_and_forget_arena_alloc_and_noopfree;
    result.uAllocParams = this->arena.root_chunk_handle.uTagged;
    return result;
};

// conversion from GeneralPurposeArenaAlloc to DynGPAlloc
FORCE_INLINE DynGPAlloc GeneralPurposeArenaAlloc::toDefaultDyn() {
    Assert_(false); // TODO when GeneralPurposeArena becomes available
    return DynGPAlloc();
}

// conversion from MTGeneralPurposeArenaAlloc to DynGPAlloc
FORCE_INLINE DynGPAlloc MTGeneralPurposeArenaAlloc::toDefaultDyn() {
    Assert_(false); // TODO when GeneralPurposeArena becomes available
    return DynGPAlloc();
}

// conversion from DynScratchAlloc to DynGPAlloc
FORCE_INLINE DynGPAlloc DynScratchAlloc::toDefaultDyn() {
    DynGPAlloc result;
    result.pAllocAndFreeFn = fire_and_forget_arena_alloc_and_noopfree;
    result.uAllocParams = this->uAllocParams;
    return result;
}

// conversion from DummyNoAlloc to DynScratchAlloc
FORCE_INLINE DynScratchAlloc DummyNoAlloc::toFFADyn() {
    DynScratchAlloc result;
    result.pAllocFn = dummy_fire_and_forget_no_alloc;
    result.uAllocParams = 0u;
    return result;
};

// conversion from DummyNoAlloc to DynGPAlloc
FORCE_INLINE DynGPAlloc DummyNoAlloc::toDefaultDyn() {
    return DynGPAlloc::dummyNoAlloc();
};


