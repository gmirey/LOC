#pragma once

#include "BaseDecls.h"
#include "Platform.h"
#include "Arenas.h"
#include "AllocTraits.h"

// TODO: do without this ?
#include <type_traits>

struct ArrayImpl {
    u32 uAllocSize;
    u32 _uSize;
    u8* pCurrentBase;

    ArrayImpl() = default;
    ArrayImpl(const ArrayImpl&) = default;
    ArrayImpl& operator=(const ArrayImpl&) = default;
    ~ArrayImpl() = default;

    static FORCE_INLINE ArrayImpl initZero() {
        ArrayImpl result;
        result.uAllocSize = 0u;
        result._uSize = 0;
        result.pCurrentBase = 0;
        return result;
    }
    static FORCE_INLINE ArrayImpl initStatic(u8* pStaticAlloc, u32 uStaticAllocSize) {
        ArrayImpl result;
        result.uAllocSize = uStaticAllocSize;
        result._uSize = 0;
        Assert(0 == (reinterpret_cast<uintptr_t>(pStaticAlloc) & uintptr_t(1u)),
            "ArrayImpl::initStatic(): pointer should be 16b aligned at least, otherwise fails with tags"); 
        result.pCurrentBase = reinterpret_cast<u8*>(reinterpret_cast<uintptr_t>(pStaticAlloc) | uintptr_t(1u)); // tags our pointer with static flag
        return result;
    }
    FORCE_INLINE bool isPointingToStackInit() const {
        return 0u != reinterpret_cast<uintptr_t>(pCurrentBase) & uintptr_t(1u);                             // queries our pointer tag
    }
    FORCE_INLINE u8* getPtr() {
        return reinterpret_cast<u8*>(reinterpret_cast<uintptr_t>(pCurrentBase) & ~uintptr_t(1u));           // untags our pointer
    }
    FORCE_INLINE const u8* getConstPtr() const {
        return reinterpret_cast<const u8*>(reinterpret_cast<uintptr_t>(pCurrentBase) & ~uintptr_t(1u));     // untags our pointer
    }

    template<typename AllocT>
    u8* realloc_for_append(AllocT allocator, u32 uNewAlloc, u32 uPreviousUsedBytes, u16 uAlign)
    {
        Assert_(uAlign >= 2u);
        Assert_(uNewAlloc);
        u8* pNewBase = allocator.allocate(uNewAlloc, uAlign);               // allocates required amount
        Assert_(pNewBase);
        uintptr_t uLastBase = reinterpret_cast<uintptr_t>(pCurrentBase);
        u8* pLastBase = reinterpret_cast<u8*>(uLastBase & ~uintptr_t(1u));  // untags our pointer to data
        if (uPreviousUsedBytes) {
            Assert_(uPreviousUsedBytes <= uAllocSize);
            Assert_(uPreviousUsedBytes <= uNewAlloc);
            Assert_(pLastBase);
            memcpy(pNewBase, pLastBase, uPreviousUsedBytes);                // copies previous contents to new alloc if need be
        }
        if (pLastBase && 0uLL == (uLastBase & 0x1uLL))                      // deallocates previous alloc IFF non null and not tagged as pointing to static
            allocator.deallocate(pLastBase);
        uAllocSize = uNewAlloc;                                             // updates our state with this new stuff, and returns new pointer
        pCurrentBase = pNewBase;
        return pNewBase;
    };

    template<typename AllocT>
    u8* realloc_for_insert(AllocT allocator, u32 uNewAlloc, u32 uPreviousStartBytes,
        u32 uOffsetTail, u32 uPreviousTailBytes, u16 uAlign)
    {
        Assert_(uAlign >= 2u);
        Assert_(uNewAlloc);
        u8* pNewBase = allocator.allocate(uNewAlloc, uAlign);               // allocates required amount
        Assert_(pNewBase);
        uintptr_t uLastBase = reinterpret_cast<uintptr_t>(pCurrentBase);
        u8* pLastBase = reinterpret_cast<u8*>(uLastBase & ~uintptr_t(1u));  // untags our pointer to data
        Assert_(pLastBase && uPreviousTailBytes);
        Assert_(uOffsetTail > uPreviousStartBytes);
        Assert_(uPreviousStartBytes + uPreviousTailBytes <= uAllocSize);
        Assert_(uOffsetTail + uPreviousTailBytes <= uNewAlloc);
        if (uPreviousStartBytes) {
            memcpy(pNewBase, pLastBase, uPreviousStartBytes);                // copies previous contents to new alloc if need be
        }
        memcpy(pNewBase + uOffsetTail, pLastBase + uPreviousStartBytes, uPreviousTailBytes);
        if (pLastBase && 0uLL == (uLastBase & 0x1uLL))                      // deallocates previous alloc IFF non null and not tagged as pointing to static
            allocator.deallocate(pLastBase);
        uAllocSize = uNewAlloc;                                             // updates our state with this new stuff, and returns new pointer
        pCurrentBase = pNewBase;
        return pNewBase;
    };

    template<typename AllocT>
    ArrayImpl initByDeepCopy(AllocT allocator, u32 uUsedBytes, u32 uSize, u16 uAlign, bool stripAllocToSize = true)
    {
        Assert_(uAlign >= 2u);
        Assert_(uUsedBytes <= uAllocSize);
        u32 uNewAlloc = stripAllocToSize ? uUsedBytes : uAllocSize;             // we'll only alloc current used size unless stripAllocToSize is false
        if (uNewAlloc) {
            Assert_(uSize);
            u8* pNewBase = allocator.allocate(uNewAlloc, uAlign);               // allocates required amount
            Assert_(pNewBase);
            uintptr_t uOurBase = reinterpret_cast<uintptr_t>(pCurrentBase);
            u8* pOurBase = reinterpret_cast<u8*>(uOurBase & ~uintptr_t(1u));    // untags our pointer to data
            if (uUsedBytes) {
                Assert_(pOurBase);
                memcpy(pNewBase, pOurBase, uUsedBytes);                         // copies our contents to new alloc if need be
            }
            ArrayImpl result;                                                // instanciates result with this new stuff, and returns it
            result.uAllocSize = uNewAlloc;
            result._uSize = uSize;
            result.pCurrentBase = pNewBase;
            return result;
        } else {
            Assert_(uSize == 0);
            return initZero();                                            // returns an instnce pointing to null if zero alloc size.
        }
    };

    template<typename AllocT>
    FORCE_INLINE void release(AllocT allocator) {
        uintptr_t uCurrentBase = reinterpret_cast<uintptr_t>(pCurrentBase);
        if (pCurrentBase && (0uLL == (uCurrentBase & 0x1uLL))) {
            allocator.deallocate(pCurrentBase);
            pCurrentBase = 0;
            uAllocSize = 0u;
        } // otherwise NOOP: keep null as null, and even keep static alloc.
        _uSize = 0;
    };
};

// An array of 'EltT' with arbitrary length. Base implementation for 'DynArray', however this one has no dynamic resizing ability
template<typename EltT>
struct ArrayView {
    static_assert(std::is_trivially_copyable<EltT>::value,
                  "Array only allows trivially copyable elements. Fall back to std::vector for fancy content");
    static_assert(alignof(EltT) < 0x10000, "ArrayView requires max elem align 64K");
    static_assert(sizeof(EltT) < 0x10000, "ArrayView requires max elem size 64K");

    ArrayImpl _impl;
public:
    ArrayView() = default;
    ArrayView(const ArrayView<EltT>&) = default;
    ArrayView<EltT>& operator=(const ArrayView<EltT>&) = default;
    ~ArrayView() = default;

    FORCE_INLINE u32 size() const { return _impl._uSize; }
    FORCE_INLINE u32 isEmpty() const { return 0 == _impl._uSize; }

    FORCE_INLINE u32 getCurrentCapacity() const { return _impl.uAllocSize / sizeof(EltT); }
    FORCE_INLINE static constexpr u16 getAlign() { return _max(u16(alignof(EltT)), u16(2u)); }

    FORCE_INLINE EltT* begin()              { return reinterpret_cast<EltT*>(_impl.getPtr()); }
    FORCE_INLINE EltT* end()                { return begin() + _impl._uSize; }
    FORCE_INLINE EltT* at(u32 uIndex)       { Assert_(uIndex < _impl._uSize); return begin() + uIndex; }

    FORCE_INLINE const EltT* cbegin() const { return reinterpret_cast<const EltT*>(_impl.getConstPtr()); }
    FORCE_INLINE const EltT* cend() const   { return cbegin() + _impl._uSize; }
    FORCE_INLINE const EltT* cat(u32 uIndex) const    { Assert_(uIndex < _impl._uSize); return cbegin() + uIndex; }

    FORCE_INLINE EltT& operator[](u32 uIndex)  { return *at(uIndex); }
    FORCE_INLINE const EltT& operator[](u32 uIndex) const { return *cat(uIndex); }
    FORCE_INLINE EltT& first() { Assert_(_impl._uSize); return *begin(); }
    FORCE_INLINE EltT& last()  { Assert_(_impl._uSize); return *at(_impl._uSize - 1u); }
    FORCE_INLINE const EltT& cfirst() const { Assert_(_impl._uSize); return *begin(); }
    FORCE_INLINE const EltT& clast()  const { Assert_(_impl._uSize); return *cat(_impl._uSize - 1u); }

    FORCE_INLINE Slice<EltT> asSlice() const { Slice<EltT> result; result.start = cbegin(); result.count = u64(size()); return result; }

    FORCE_INLINE void clear() { _impl._uSize = 0u; }

    FORCE_INLINE void remove_last() { Assert_(_impl._uSize); _impl._uSize--; }
    FORCE_INLINE EltT pop_last() {
        Assert_(_impl._uSize);
        EltT lastElt = clast();
        _impl._uSize--;
        return lastElt;
    }
    void remove_by_endswap_at(u32 uIndex) {
        u32 uSize = _impl._uSize;
        Assert_(uSize);
        if (uIndex < uSize - 1u) {
            EltT* slots = begin();
            slots[uIndex] = slots[uSize - 1u];
        } else {
            Assert_(uIndex == uSize-1u);
        }
        _impl._uSize = uSize - 1u;
    }
    EltT pop_by_endswap_at(u32 uIndex) {
        u32 uSize = _impl._uSize;
        Assert_(uSize);
        if (uIndex < uSize - 1u) {
            EltT* slots = begin();
            EltT removed = slots[uIndex];
            slots[uIndex] = slots[uSize - 1u];
            _impl._uSize = uSize - 1u;
            return removed;
        } else {
            Assert_(uIndex == uSize - 1u);
            return pop_last();
        }
    }
    void remove_at(u32 uIndex) {
        u32 uSize = _impl._uSize;
        Assert_(uSize);
        if (uIndex < uSize - 1u) {
            EltT* slots = begin();
            memmove(slots + uIndex, slots + uIndex + 1u, sizeof(EltT) * (uSize - uIndex - 1u));
        } else {
            Assert_(uIndex == uSize - 1u);
        }
        _impl._uSize = uSize - 1u;
    }
    EltT pop_at(u32 uIndex) {
        u32 uSize = _impl._uSize;
        Assert_(uSize);
        if (uIndex < uSize - 1u) {
            EltT* slots = begin();
            EltT removed = slots[uIndex];
            memmove(slots + uIndex, slots + uIndex + 1u, sizeof(EltT) * (uSize - uIndex - 1u));
            _impl._uSize = uSize - 1u;
            return removed;
        } else {
            Assert_(uIndex == uSize - 1u);
            return pop_last();
        }
    }
    void remove_range(u32 uStart, u32 uCount) {
        if (LIKELY(uCount)) {
            u32 uSize = _impl._uSize;
            Assert_(uStart < uSize);
            if (uStart + uCount >= uSize)
                _impl._uSize = uStart;
            else {
                u32 uStartOfRemaining = uStart + uCount;
                memmove(slots + uStart, slots + uStartOfRemaining, sizeof(EltT) * (uSize - uStart - uCount));
                _impl._uSize = uSize - uCount;
            }
        }
    }

protected:
    FORCE_INLINE void _append_no_check(EltT elt) {
        *end() = elt;
        _impl._uSize++;
    }
    void _insert_nocheck_at(u32 uIndex, EltT elt) {
        u32 uSize = _impl._uSize;
        if (uIndex < uSize) {
            for (u32 i = uSize+1u; i > uIndex; i--)
                *at(i) = *cat(i-1);
            *at(uIndex) = elt;
            _impl._uSize = uSize + 1u;
        } else {
            Assert_(uIndex == uSize);
            _append_no_check(elt);
        }
    }
    // overwrites some of our range with a range of contiguous and convertible-to-EltT elements (implementation helper without most checks)
    template<typename CompatibleElemT> FORCE_INLINE void _replace_all_nocheck_from(u32 uIndex, u32 uCount, const CompatibleElemT* eltBegin) {
        Assert_(uIndex + uCount <= _impl._uSize);
        EltT* slots = at(i);
        for (const CompatibleElemT* source = eltBegin, const CompatibleElemT* sourceEnd = eltBegin + uCount; source < sourceEnd; source++, slots++)
            *slots = eltBegin[i];
    };
    // specialization of the above when the elements are precisely of type EltT, preferentially using memcpy for same result
    template<> FORCE_INLINE void _replace_all_nocheck_from<EltT>(u32 uIndex, u32 uCount, const EltT* eltBegin) {
        Assert_(uIndex + uCount <= _impl._uSize);
        memcpy(at(uIndex), eltBegin, uCount * sizeof(EltT));
    };
    // appends a range of contiguous and convertible-to-EltT elements
    template<typename CompatibleElemT> void _append_all_nocheck(const CompatibleElemT* eltBegin, u32 uCount) {
        if (uCount) {
            u32 uIndex = _impl._uSize;
            _impl._uSize += uCount;
            _replace_all_nocheck_from(uIndex, uCount, eltBegin);
        }
    };

    /*
    // inserts a range of contiguous and convertible-to-EltT elements
    template<typename CompatibleElemT> void _insert_all_nocheck_at(u32 uIndex, const CompatibleElemT* eltBegin, u32 uCount) {
        u32 uSize = _impl._uSize;
        if (uIndex < uSize) {
            if (uCount) {

                for (u32 i = _uSize+uCount; i >= uIndex + uCount; i--)
                    *at(i) = *cat(i-uCount);
                _uSize += uCount;
                _replace_all_nocheck_from(uIndex, uCount, eltBegin);
            }
        } else {
            Assert_(uIndex == _uSize);
            _append_all_nocheck(eltBegin, uCount);
        }
    };
    */
};

// An array of 'EltT' which can grow to arbitrary lengths, by resizing itself, according to its 'AllocT' allocator
template<typename EltT, typename AllocT>
struct AArray : public ArrayView<EltT> {
    AllocT _alloc;
    
    AArray() = default;
    AArray(const AArray<EltT, AllocT>&) = default;
    AArray<EltT, AllocT>& operator=(const AArray<EltT, AllocT>&) = default;
    ~AArray() = default;

    FORCE_INLINE AArray(AllocT alloc):ArrayView<EltT>(), _alloc(alloc) { _impl = ArrayImpl::initZero(); }
    
    FORCE_INLINE AllocT alloc() const { return _alloc; }
    FORCE_INLINE AllocT& alloc_ref() { return _alloc; }

    FORCE_INLINE void init(AllocT alloc) { _alloc = alloc; _impl = ArrayImpl::initZero(); }
    FORCE_INLINE void setAlloc(AllocT alloc) { _alloc = alloc; }
    FORCE_INLINE void release() { _impl.release(_alloc); }
    FORCE_INLINE void append(EltT elt) {
        if ((_impl._uSize + 1u) * sizeof(EltT) < _impl.uAllocSize)
            _append_no_check(elt);
        else
            _append_after_grow(&elt, 1u);
    }
    FORCE_INLINE void insert(u32 uIndex, EltT elt) {
        if ((_impl._uSize + 1u) * sizeof(EltT) < _impl.uAllocSize)
            _insert_no_check(elt);
        else
            _insert_after_grow_at(uIndex, &elt, 1u);
    }

    template<typename CompatibleElemT>
    FORCE_INLINE void append_all(const CompatibleElemT* startElt, u32 uCount) {
        if (uCount) {
            Assert_(uCount < 0x10000000u);
            if ((_impl._uSize + uCount) * sizeof(EltT) <= _impl.uAllocSize)
                _append_all_nocheck(startElt, uCount);
            else
                _append_after_grow(startElt, uCount);
        }
    };

    template<typename CompatibleElemT>
    FORCE_INLINE void append_all(const CompatibleElemT* startElt, const CompatibleElemT* endElt) {
        if (endElt > startElt) {
            u64 uCount = u64(endElt - startElt);
            Assert_(uCount < 0x10000000uLL)
            append_all(startElt, u32(uCount));
        }
    };

    template<typename CompatibleElemT>
    FORCE_INLINE void append_all(const ArrayView<CompatibleElemT>& other) {
        u32 uCount = other.size();
        if (uCount)
            append_all(other.cbegin(), uCount);
    };

    template<typename CompatibleElemT>
    FORCE_INLINE void insert_all_at(u32 uIndex, const CompatibleElemT* startElt, u32 uCount) {
        u32 uSize = _impl._uSize;
        if (uCount) {
            Assert_(uCount < 0x80000000u);
            if ((uSize + uCount) * sizeof(EltT) <= _impl.uAllocSize)
                _insert_all_nocheck_at(startElt, uCount);
            else {
                if (uIndex < uSize) {
                    _insert_after_grow_at(uIndex, startElt, uCount);
                } else {
                    Assert_(uIndex == uSize);
                    _append_after_grow(startElt, uCount);
                }
            }
        }
    };

    template<typename CompatibleElemT>
    FORCE_INLINE void insert_all_at(u32 uIndex, const CompatibleElemT* startElt, const CompatibleElemT* endElt) {
        if (endElt > startElt) {
            u64 uCount = u64(endElt - startElt);
            Assert_(uCount < 0x80000000u)
            insert_all_at(uIndex, startElt, u32(uCount));
        }
    };

    template<typename CompatibleElemT>
    FORCE_INLINE void insert_all_at(u32 uIndex, const ArrayView<CompatibleElemT>& other) {
        u32 uCount = other.getSize();
        if (uCount)
            insert_all_at(uIndex, other.cbegin(), uCount);
    };

    FORCE_INLINE AArray<EltT, AllocT> get_deep_copy(bool stripAllocToSize = true) {
        AArray<EltT, AllocT> result(_alloc);
        result._impl = _impl.initByDeepCopy(_alloc, _uSize * sizeof(EltT), _uSize, getAlign(), stripAllocToSize);
        return result;
    }
    FORCE_INLINE AArray<EltT, AllocT> get_self_or_copy_if_on_stack(bool stripAllocToSize = true) {
        if (_impl.isPointingToStackInit())
            return get_deep_copy(stripAllocToSize);
        else
            return *this;
    }

    template<typename OtherAllocT>
    FORCE_INLINE AArray<EltT, OtherAllocT> get_deep_copy_with(OtherAllocT otherAlloc, bool stripAllocToSize = true) {
        AArray<EltT, OtherAllocT> result(otherAlloc);
        result._impl = _impl.initByDeepCopy(otherAlloc, _uSize * sizeof(EltT), _uSize, getAlign(), stripAllocToSize);
        return result;
    };

    void reserve(u32 uReqEltCount, bool bReduceAllocIfLowerThanCurrent = false) {
        Assert_(uReqEltCount >= _impl._uSize);
        u64 uReqSize = u64(uReqEltCount) * sizeof(EltT);
        Assert_(uReqSize < 0x80000000uLL && "max payload of about 2 GB reached for DynArray");
        if (u32(uReqSize) > _impl.uAllocSize || (bReduceAllocIfLowerThanCurrent && u32(uReqSize) < _impl.uAllocSize && !_alloc.isFireAndForget()))
            _impl.realloc_for_append(_alloc, u32(uReqSize), _impl._uSize * sizeof(EltT), getAlign());
    }

    void resize(u32 uEltCount) {
        u32 uPrevSize = _impl._uSize;
        resize_non_zeroing(uEltCount);
        if (uPrevSize < uEltCount) {
            EltT* pStart = at(uPrevSize);
            u32 uDiff = uEltCount - uPrevSize;
            memset(pStart, 0, uDiff * sizeof(EltT));
        }
    }

    void resize_non_zeroing(u32 uEltCount) {
        if (uEltCount >= getCurrentCapacity()) {
            reserve(uEltCount);
        }
        _impl._uSize = uEltCount;
    }

protected:
    FORCE_INLINE u32 _get_alloc_for_growing_to(u32 uReqEltCount) const {
        u64 uReqSize = u64(uReqEltCount) * sizeof(EltT);
        Assert_(uReqSize < 0x80000000uLL && "max payload of about 2 GB reached for DynArray");
        u32 uPreviousAlloc = _max(_impl.uAllocSize, u32(16u * sizeof(EltT))); // first append would always grow to 16 elements
        u32 uReqAllocNow = (uPreviousAlloc * 7u) / 4u;  // growing by * 1.75 ? => 0, 16, 28, 49, 85, 148, 259, 453...
        if (uReqAllocNow < u32(uReqSize) || uReqAllocNow >= 0x80000000u)
            uReqAllocNow = u32(uReqSize); // also growing to exact size when trying to add a large amount (which does not fit the default growth scheme)
        return uReqAllocNow;
    }
    template<typename CompatibleElemT> void _append_after_grow(const CompatibleElemT* startElt, u32 uCount) {
        u32 uSize = _impl._uSize;
        u32 uReqAllocNow = _get_alloc_for_growing_to(uSize + uCount);
        _impl.realloc_for_append(_alloc, uReqAllocNow, uSize * sizeof(EltT), getAlign());
        u32 uIndex = uSize;
        _impl._uSize = uSize + uCount;
        _replace_all_nocheck_from(uIndex, uCount, startElt);
    };
    template<typename CompatibleElemT> void _insert_after_grow_at(u32 uIndex, const CompatibleElemT* startElt, u32 uCount) {
        u32 uSize = _impl._uSize;
        u32 uReqAllocNow = _get_alloc_for_growing_to(uSize + uCount);
        Assert_(uIndex < uSize);
        _impl.realloc_for_insert(_alloc, uReqAllocNow, uIndex * sizeof(EltT), (uIndex + uCount) * sizeof(EltT), (uSize - uIndex) * sizeof(EltT), getAlign());
        _impl._uSize = uSize + uCount;
        _replace_all_nocheck_from(uIndex, uCount, startElt);
    };
};

template<typename EltT> using Array = AArray<EltT, MallocPoweredAlloc>;
template<typename EltT> using TmpArray = AArray<EltT, FireAndForgetArenaAlloc>;
template<typename EltT> using DynArray = AArray<EltT, DynGPAlloc>;

// helper definitions, to ensure at least 16b alignement on our buffers on stack
template<typename ElemT, u8 uIsLessThanTwoBytes> struct RequalifyToWORDHelper { typedef ElemT elem_t; };
template<typename ElemT> struct RequalifyToWORDHelper<ElemT, 1u>              { typedef u16 elem_t; };

template<typename EltT, u16 uStackCount, typename AllocT>
struct AStackOptiArray : public AArray<EltT, AllocT> {
    static const u8 uIsLessThanTwoBytes = sizeof(EltT) < 2u ? 1u : 0u;
    static const u16 STACK_BUFFER_SLOTS = uIsLessThanTwoBytes ? (uStackCount + 1u) / 2u : uStackCount;
    typedef typename RequalifyToWORDHelper<EltT, uIsLessThanTwoBytes>::elem_t stack_buffer_elem_t;
    stack_buffer_elem_t STACK_BUFFER[STACK_BUFFER_SLOTS]; // should be ensured aligned to at least EltT align, and also at least 16b.
    FORCE_INLINE AStackOptiArray():AArray<EltT, AllocT>() {
        _init_with_static_buffer();
    }
    FORCE_INLINE AStackOptiArray(AllocT alloc):AArray<EltT, AllocT>(alloc) {
        _init_with_static_buffer();
    }
    AStackOptiArray(const AStackOptiArray<EltT, uStackCount, AllocT>&) = delete;
    AStackOptiArray<EltT, uStackCount, AllocT>& operator= (const AStackOptiArray<EltT, uStackCount, AllocT>&) = delete;
    ~AStackOptiArray() = default;
private:
    void _init_with_static_buffer() {
        _impl = ArrayImpl::initStatic(reinterpret_cast<u8*>(STACK_BUFFER), STACK_BUFFER_SLOTS * sizeof(stack_buffer_elem_t));
    }
};

template<typename EltT, u16 uStackCount> using StackOptiArray = AStackOptiArray<EltT, uStackCount, MallocPoweredAlloc>;
template<typename EltT, u16 uStackCount> using TmpStackOptiArray = AStackOptiArray<EltT, uStackCount, FireAndForgetArenaAlloc>;
template<typename EltT, u16 uStackCount> using DynStackOptiArray = AStackOptiArray<EltT, uStackCount, DynGPAlloc>;

#define STABLE_GROWING_VECTOR_SIZE_SHIFT            8u  // 256x size
#define STABLE_GROWING_VECTOR_SIZE_PER_STEP         (1u << STABLE_GROWING_VECTOR_SIZE_SHIFT)
#define STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK    (STABLE_GROWING_VECTOR_SIZE_PER_STEP - 1u)

struct StableGrowingVector_LeafBlock {             // 0 at init, or fixed 64x elements (max 16M of those from root)
    u8 tData[0];
    static FORCE_INLINE StableGrowingVector_LeafBlock* create(u16 uElemSize, u16 uElemAlign, Arena arena) {
        return (StableGrowingVector_LeafBlock*)alloc_from(arena, STABLE_GROWING_VECTOR_SIZE_PER_STEP * u32(uElemSize), uElemAlign);
    }
};

struct StableGrowingVector_InterBlock {             // 0 at init, or fixed 64x elements (max 16M of those from root)
    StableGrowingVector_LeafBlock* tLeafBlocks[STABLE_GROWING_VECTOR_SIZE_PER_STEP];
    static FORCE_INLINE StableGrowingVector_InterBlock* create(Arena arena) {
        return (StableGrowingVector_InterBlock*)alloc_from(arena, sizeof(StableGrowingVector_InterBlock), alignof(StableGrowingVector_InterBlock));
    }
    void alloc_below(u16 uElemSize, u16 uElemAlign, u32 uPrevLeafCount, u32 uReqLeafCount, Arena arena)
    {
        Assert_(uPrevLeafCount <= STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        Assert_(uReqLeafCount <= STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        Assert_(uPrevLeafCount < uReqLeafCount);
        for (u32 uLeaf = uPrevLeafCount; uLeaf < uReqLeafCount; uLeaf++)
            tLeafBlocks[uLeaf] = StableGrowingVector_LeafBlock::create(uElemSize, uElemAlign, arena);
    }
};

struct StableGrowingVector_Impl {
    Arena arena;
    StableGrowingVector_InterBlock* tInterBlocks[STABLE_GROWING_VECTOR_SIZE_PER_STEP];
    static FORCE_INLINE StableGrowingVector_Impl* create(Arena theArena) {
        StableGrowingVector_Impl* pResult = (StableGrowingVector_Impl*)alloc_from(theArena, sizeof(StableGrowingVector_Impl), alignof(StableGrowingVector_Impl));
        pResult->arena = theArena;
        return pResult;
    }
    void alloc_below(u16 uElemSize, u16 uElemAlign,
        u32 uPrevInterCount, u32 uReqInterCount,
        u32 uPrevLeafCountInPrevLastInter, u32 uReqLeafCountInLastInter)
    {
        Assert_(uPrevInterCount <= STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        Assert_(uReqInterCount <= STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        Assert_(uPrevInterCount < uReqInterCount);

        Assert_(uPrevLeafCountInPrevLastInter <= STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        Assert_(uReqLeafCountInLastInter <= STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        Assert_(uReqLeafCountInLastInter);

        if (uPrevInterCount) {
            Assert_(uPrevLeafCountInPrevLastInter);
            if (uPrevLeafCountInPrevLastInter < STABLE_GROWING_VECTOR_SIZE_PER_STEP) {
                StableGrowingVector_InterBlock* pPrevLastInter = tInterBlocks[uPrevInterCount - 1u];
                pPrevLastInter->alloc_below(uElemSize, uElemAlign, uPrevLeafCountInPrevLastInter, STABLE_GROWING_VECTOR_SIZE_PER_STEP, arena);
            }
        }

        StableGrowingVector_InterBlock* pInterToAllocFull = tInterBlocks[uPrevInterCount];
        for (u32 uInterToAllocFull = uPrevInterCount; uInterToAllocFull < uReqInterCount - 1u; uInterToAllocFull++) {
            StableGrowingVector_InterBlock* pInterToAllocFull = StableGrowingVector_InterBlock::create(arena);
            pInterToAllocFull->alloc_below(uElemSize, uElemAlign, 0, STABLE_GROWING_VECTOR_SIZE_PER_STEP, arena);
            tInterBlocks[uInterToAllocFull] = pInterToAllocFull;
        }
        StableGrowingVector_InterBlock* pLastInterToAlloc = StableGrowingVector_InterBlock::create(arena);
        pLastInterToAlloc->alloc_below(uElemSize, uElemAlign, 0, uReqLeafCountInLastInter, arena);
        tInterBlocks[uReqInterCount - 1u] = pLastInterToAlloc;
    }
};

// Less efficient access than a vector, and unable to remove elements...
// but ensures pointer stability and allows lock-free-reads of all elements once pushed.
// Current implementation only support arena allocators. TODO: a version supporting all allocator traits ???
template<typename EltT>
struct StableGrowingVector {
    static_assert(std::is_trivially_copyable<EltT>::value,
                  "StableGrowingVector only allows trivially copyable elements.");
    static_assert(alignof(EltT) < 0x10000, "StableGrowingVector requires max elem align 64K");
    static_assert(sizeof(EltT) < 0x10000, "StableGrowingVector requires max elem size 64K");

    u32 _uSize;
    u32 _pad0;
    StableGrowingVector_Impl* _pImplData;

    StableGrowingVector() = default;
    StableGrowingVector(const StableGrowingVector<EltT>&) = default;
    StableGrowingVector<EltT>& operator=(const StableGrowingVector<EltT>&) = default;
    ~StableGrowingVector() = default;

    FORCE_INLINE StableGrowingVector(Arena arena) { init(arena); }
    StableGrowingVector(Arena arena, u32 uInitialSize) {
        init(arena);
        Assert_(uInitialSize <= STABLE_GROWING_VECTOR_SIZE_PER_STEP * STABLE_GROWING_VECTOR_SIZE_PER_STEP * STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        resize_requiring_new_leaf_blocks(0, (uInitialSize + STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK) >> STABLE_GROWING_VECTOR_SIZE_SHIFT);
        _uSize = uInitialSize;
    }

    FORCE_INLINE void init(Arena arena) {
        _uSize = 0;
        _pad0 = 0;
        _pImplData = StableGrowingVector_Impl::create(arena);
    }

    void resize_requiring_new_leaf_blocks(u32 uCurrentLeafBlocks, u32 uReqLeafBlocks) {
        Assert_(uReqLeafBlocks > uCurrentLeafBlocks);
        u32 uCurrentLeafInCurrentLastInter = uCurrentLeafBlocks & STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK;
        u32 uReqLeafInReqLastInter = uReqLeafBlocks & STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK;
        u32 uCurrentInterBlocks = (uCurrentLeafBlocks + STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK) >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
        u32 uReqInterBlocks = (uReqLeafBlocks + STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK) >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
        if (uReqInterBlocks > uCurrentInterBlocks) {
            _pImplData->alloc_below(sizeof(EltT), alignof(EltT), uCurrentInterBlocks, uReqInterBlocks,
                uCurrentLeafInCurrentLastInter, uReqLeafInReqLastInter);
        } else {
            StableGrowingVector_InterBlock* pLastInter = _pImplData->tInterBlocks[uCurrentInterBlocks - 1u];
            Assert_(uCurrentLeafInCurrentLastInter);
            Assert_(uReqLeafInReqLastInter > uCurrentLeafInCurrentLastInter);
            pLastInter->alloc_below(sizeof(EltT), alignof(EltT), uCurrentLeafInCurrentLastInter,
                uReqLeafInReqLastInter, _pImplData->arena);
        }
    }

    void resize(u32 uNewSize) {
        Assert_(uNewSize <= STABLE_GROWING_VECTOR_SIZE_PER_STEP * STABLE_GROWING_VECTOR_SIZE_PER_STEP * STABLE_GROWING_VECTOR_SIZE_PER_STEP);
        u32 uCurrentSize = _uSize;
        if (uNewSize > uCurrentSize) {
            u32 uCurrentLeafBlocks = (uCurrentSize + STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK) >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
            u32 uReqLeafBlocks = (uNewSize + STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK) >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
            if (uReqLeafBlocks > uCurrentLeafBlocks) { // We'll require at least one alloc iff we require more leaf blocks than current
                resize_requiring_new_leaf_blocks(uCurrentLeafBlocks, uReqLeafBlocks);
                // forces to complete all ptr reorgs above before assigning new size, below
                WRITE_FENCE();
            } else
                Assert_(uReqLeafBlocks == uCurrentLeafBlocks);
        }
        // Then unconditionnaly write the new size, knowing that if any pointer reorg happened above,
        //   we asked for a fence to complete these before this one write.
        // Note that it MAY also shrink the current size, but since we are using FF alloc, we can live with this without care...
        _uSize = uNewSize;
    }

    FORCE_INLINE EltT& operator[](u32 uIndex) {
        return *at(uIndex);
    }
    FORCE_INLINE const EltT& operator[](u32 uIndex) const {
        return *at(uIndex);
    }
    FORCE_INLINE EltT& last()  {
        Assert_(_uSize);
        return *at(_uSize - 1u);
    }
    FORCE_INLINE const EltT& constlast() const  {
        Assert_(_uSize);
        return *at(_uSize - 1u);
    }
    FORCE_INLINE void append(EltT elem) {
        u32 uPrevSize = _uSize;
        resize(uPrevSize + 1u);
        EltT* pPtr = at(uPrevSize);
        *pPtr = elem;
    }
    FORCE_INLINE u32 size() const  { return _uSize; }
    FORCE_INLINE void clear()  { resize(0u); }

    u8* get_ptr_at(u32 uIndex) {
        u32 uLeafBlock = uIndex >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
        u32 uPosInLeaf = uIndex & STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK;

        u32 uInterBlock = uLeafBlock >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
        u32 uLeafInInter = uLeafBlock & STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK;

        u8* tBase = _pImplData->tInterBlocks[uInterBlock]->tLeafBlocks[uLeafInInter]->tData;
        return tBase + (uPosInLeaf * sizeof(EltT));
    }
    FORCE_INLINE EltT* at(u32 uIndex) { return reinterpret_cast<EltT*>(get_ptr_at(uIndex)); }

    const u8* get_const_ptr_at(u32 uIndex) const {
        u32 uLeafBlock = uIndex >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
        u32 uPosInLeaf = uIndex & STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK;

        u32 uInterBlock = uLeafBlock >> STABLE_GROWING_VECTOR_SIZE_SHIFT;
        u32 uLeafInInter = uLeafBlock & STABLE_GROWING_VECTOR_SIZE_PER_STEP_MASK;

        const u8* tBase = _pImplData->tInterBlocks[uInterBlock]->tLeafBlocks[uLeafInInter]->tData;
        return tBase + (uPosInLeaf * sizeof(EltT));
    }
    FORCE_INLINE const EltT* at(u32 uIndex) const { return reinterpret_cast<const EltT*>(get_const_ptr_at(uIndex)); }

};

