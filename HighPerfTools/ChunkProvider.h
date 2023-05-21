#pragma once

#include "BaseDecls.h"  // basic stuff for everything
#include "Platform.h"   // to get pages from the OS

#define ASSERT_FALSE_ON_OOM  true

#define CHUNK_PROVIDER_HANDLE_H_
#ifdef BASIC_CHUNK_PROVIDER_HANDLE_H_
#  error "Should not include both 'BasicChunkProvider.h' and 'ChunkProvider.h'"
#endif

// ChunkProvider manages allocations of large chunks of memory,
//   by being able to call OS func for memory pages, but also avoiding to do that very frequently.
//
// It has 3 allocations strategies:
//   - 1) "Regular" chunks of CHUNK_PROVIDER_REGULAR_CHUNK_SIZE each, which is the minimum one may ask for.
//        Each of thoses are piecewise taken from "regular" pages, containing 64 of them to fast test for availability
//        against some bitfield. Note that it is only "regular" in that derived allocators such as FireAndForgetAlloc
//        know about it, and will preferentially chose this particular 128K size to manage their own allocations.
//              Per-Page alloc (OS call) : 8M
//              Per-Chunk given (user-available partition) : 128K
//              Chunks per Page : 64
//   - 2) "Larger" chunks of over CHUNK_PROVIDER_REGULAR_CHUNK_SIZE, but less than MAX_ALLOC_IN_LARGE_PAGE.
//        Each of those are taken from "larger" pages, which remember a list of allocated chunks.
//        Since all those allocs are at least CHUNK_PROVIDER_REGULAR_CHUNK_SIZE, and the size of the page
//        is known, the max count of free nodes in one of those pages is reduced, thus allowing to
//        alloc a small table of them within the larger chunk itself.
//              Per-Page alloc (OS call) : 32M (on top of a 4K header)
//              Per-Chunk given (user-available partition) : [132K..4M]
//              Chunks per Page : Variable, [8..248]
//   - 3) "Larger still" chunks, this time allocated in a single OS-call for each.
//              Per-Page alloc (OS call) : > 4M, as per user request
//              Per-Chunk given (user-available partition) : > 4M, as per user request
//              Chunks per Page : 1
//
// Note : requested chunk sizes should always be at least a multiple of 4K.
//        All chunks will also be returned with that minimum alignment (no provision to increase this).
//
// With its significant minimum alloc size, ChunkProvider is ill-suited for being the
//   "end-user" allocator, and its allocations are typically wrapped by ScratchMem, BucketMem, or GeneralPurpose allocators
// ChunkProvider keeps its own index (up to 128KB meta info) of currently allocated chunks, and manages deallocation on demand.

// Note: Originally, ChunkProvider would return a pointer to the starting location of a requested chunk on allocation, and work its
//   way to find it in its meta-info on deallocation. This scheme was deemed suboptimal, so ChunkProvider now returns a handle,
//   which is basically a tagged pointer. Since all chunks are at least 4K-aligned, the bottom 12 bits of the pointer are ensured 0
//     => They are now used to encode the position in our 4k-max-per kind page indices.
#define CHUNK_HANDLE_TAG_MASK               0x00000FFFu // 4096-1
struct ChunkHandle {
    u64 uTagged;
    FORCE_INLINE u8* get_ptr_to_data() const { return reinterpret_cast<u8*>(uTagged & (~u64(CHUNK_HANDLE_TAG_MASK))); }
    FORCE_INLINE u16 get_page_index_for_kind() const { return u16(uTagged) & CHUNK_HANDLE_TAG_MASK; }
};

//
// How it works:
//   Well, as described above, it provides somewhat large "chunks" of one to several hundreds of thousands of bytes, carved out
//      from OS-allocated (and larger still) "pages" of several megabytes.
//   The only complexity here is to keep track of those allocated pages and chunks. 
//   On initialization, it default-requests from OS one regular page (8M) and allocates one regular chunk (128K) inside it for
//      its own tracking. Currently, all this meta-info *has* to fit in these 128K, otherwise you should get an assert.
//   Organization of the tracking chunk:
//      a 'HeaderInRootRegularChunk' is the full organization of these 128K.
//      In this header, there are, in particular:
//          - 4K slots for describing regular pages:
//              each such slot describes a page, with pointer to start, and 64b bitfield of 'allocated' status for the 64 possible chunks in it
//          - 4K slots for describing large pages:
//              each slot is a pointer to the 4K header specific to such pages
//          - a little less than 4K slots for huge pages:
//              each slot is here simply a pointer to start
//      In the 'HeaderInRootRegularChunk', there are also a few helpers to fast-target positions of potentially-available pages in
//          these ~4K possibilities: a 64b bitfield, where a bit is set to 1 to indicate "full" status of any of the 64 existing
//          "64x page group", and each "64x page group" is itself represented as a 64b bitfield, where a bit is set to 1 to indicate
//          "full" status of the corresponding page (64x64 => 4096 such pages for each kind).
//      Note on the header of the large pages themselves (the 4KB before the 32M alloc dedicated to chunks):
//          Such a header is composed of up to 256 'nodes', each node simply describing start and end of each allocation. It is always
//          kept sorted, for fast dichotomical search of an empty spot in them. The corresponding "full" flag for a page is set whenever
//          it is detected that there are no places in there with at least 132K between them (which is the minimum chunk size in a large
//          page). TODO: maybe find some better strategy for describing remaining space in large pages (and try to allocate on least-wasting)

struct LargeAllocNode {
    u8* pStart; 
    u8* pEnd;
};

static const u32 CHUNK_PROVIDER_REGULAR_CHUNK_SIZE = 0x00020000; // 128K
static const u32 REGULAR_CHUNKS_PER_PAGE = 64;                   // to fit in u64 bitfield
static const u32 REGULAR_PAGE_SIZE = CHUNK_PROVIDER_REGULAR_CHUNK_SIZE * REGULAR_CHUNKS_PER_PAGE; // 8M
static const u32 LARGE_PAGE_TOTAL_SIZE = 0x0201000;      // 32M + 64K
static const u32 MAX_FREE_NODES_IN_LARGE_PAGE = 256;     // LARGE_PAGE_SIZE / CHUNK_PROVIDER_REGULAR_CHUNK_SIZE
struct LargePageHeader {
    LargeAllocNode tNodes[MAX_FREE_NODES_IN_LARGE_PAGE]; // *16B = 4K
};
static_assert(sizeof(LargePageHeader) == 0x1000); // 4K

static const u32 LARGE_PAGE_SIZE = LARGE_PAGE_TOTAL_SIZE - sizeof(LargePageHeader);     // 32M + 60K
static const u32 MAX_ALLOC_IN_LARGE_PAGE = 0x00400000;   // 4M ; over that limit, allocate in dedicated page

// 128K (size of smaller, "regular" chunks) + 4K (min allowed divisor of requested chunk sizes past that)
static const u32 CHUNK_PROVIDER_MIN_LARGE_CHUNK_SIZE = 0x00020000 | 0x00001000; 

static const u64 FULL_64_B = 0xFFFFFFFFFFFFFFFFuLL;

struct MemChunkPage {
    u64 uAllocated;             // This is a 64b bitfield, holding 1 iff corresponding regular chunk is allocated. Obviously only 64 regular chunks per regular page.
    u8* pPageMemoryStart;
};

static const u16 MAX_REGULAR_PAGES = 4096;
static const u16 MAX_LARGE_PAGES = 4096;
static const u16 MAX_HUGE_PAGES = 4096 - (2 + 4 + 4*64);  // 3834

/*
static const u32 OFFSET_INDEX_REGULAR_ADDRESSES = 0;
static const u32 OFFSET_INDEX_REGULAR_PAGEINDEX = OFFSET_INDEX_REGULAR_ADDRESSES + MAX_REGULAR_PAGES * sizeof(u8*);
static const u32 OFFSET_INDEX_LARGE_ADDRESSES = OFFSET_INDEX_REGULAR_PAGEINDEX + MAX_REGULAR_PAGES * sizeof(u16);
static const u32 OFFSET_INDEX_LARGE_PAGEINDEX = OFFSET_INDEX_LARGE_ADDRESSES + MAX_LARGE_PAGES * sizeof(u8*);
static const u32 OFFSET_INDEX_HUGE_ADDRESSES = OFFSET_INDEX_LARGE_PAGEINDEX + MAX_LARGE_PAGES * sizeof(u16);
static const u32 OFFSET_INDEX_HUGE_PAGEINDEX = OFFSET_INDEX_HUGE_ADDRESSES + MAX_HUGE_PAGES * sizeof(u8*);
static const u32 SIZEOF_INDEX_FULL = OFFSET_INDEX_HUGE_PAGEINDEX + MAX_HUGE_PAGES * sizeof(u16);
static_assert(SIZEOF_INDEX_FULL <= CHUNK_PROVIDER_REGULAR_CHUNK_SIZE);
*/

struct HeaderInRootRegularChunk {
    // 64b => 8 bytes
    u16 uRegularChunkPagesCount;
    u16 uLargePagesCount;
    u16 uHugePagesCount;
    u16 _pad0;

    // 64b => 8 bytes
    u64 uLockableMaybe;

    // Remaining 128K - 16 bytes... let's say:
    //  - 64K max for MemChunkPages (size 16 => 4K of them)
    //  - 32K max for LargePages (size 8 => 4K of them also)
    //  - remainder ~32K for HugePages => a little less than 4K of them (4096 - 262 (ptr size) = 3834)

    // --- provision for further indexing if those counts get huge ??? (TODO)
    MemChunkPage tRegularPages[MAX_REGULAR_PAGES]; // Around 32GB possible to allocate in those...
    LargePageHeader* tLargePages[MAX_LARGE_PAGES]; // Around 128GB possible to allocate in those...
    u8* tHugePages[MAX_HUGE_PAGES];                // Between 16GB to maybe around 15 TerraBytes possible to allocate in those...

    // 4 x 64b following

    u64 regular_page_bits_if_64group_full;      // bit set to 1 if no more room in whole 64b 'regular_page_bits_if_full' at that position

    u64 large_page_bits_if_64group_full;        // bit set to 1 if no more room in whole 64b 'large_page_bits_if_full' at that position
    u64 large_page_bits_if_64group_no_med;      // bit set to 1 if no more room in whole 64b 'large_page_bits_if_no_med' at that position
    u64 large_page_bits_if_64group_no_large;    // bit set to 1 if no more room in whole 64b 'large_page_bits_if_no_large' at that position

    // 256 x 64b below

    u64 regular_page_bits_if_full[64];          // bit set to 1 if no more chunk in whole regular page at that position
    
    u64 large_page_bits_if_full[64];            // bit set to 1 if no more room for a 132K chunk in whole large page at that position
    u64 large_page_bits_if_no_med[64];          // bit set to 1 if no more room for a 512K chunk in whole large page at that position
    u64 large_page_bits_if_no_large[64];        // bit set to 1 if no more room for a 2M chunk in whole large page at that position
   
};
static_assert(sizeof(HeaderInRootRegularChunk) == CHUNK_PROVIDER_REGULAR_CHUNK_SIZE);

struct ChunkProvider {
    HeaderInRootRegularChunk* pRoot;

    ChunkHandle allocate_regular_chunk();
    ChunkHandle allocate_large_chunk(u32 uChunkSize);
    ChunkHandle allocate_huge_chunk(u64 uChunkSize);
    ChunkHandle allocate_chunk(u64 uChunkSize);

    void release_regular_chunk(ChunkHandle handle);
    void release_large_chunk(ChunkHandle handle, u32 uChunkSize);
    void release_huge_chunk(ChunkHandle handle, u64 uChunkSize);
    void release_chunk(ChunkHandle handle, u64 uChunkSize);
};

// Note: first init of a chunk provider will use one full regular slot for its own bookkeeping purposes
//   => each time you use an independent ChunkProvider, it reserves at least 8M (and consuming 128K)
void init_chunk_provider(ChunkProvider* outProvider, bool bThreadSafe = false) {
    // There is one regular page already allocated at init
    u8* pRootPageMem = platform_page_alloc(REGULAR_PAGE_SIZE);
    // Starting bytes of this 'root' allocated page hold the 'header'
    HeaderInRootRegularChunk* pHeaderAtStart = (HeaderInRootRegularChunk*)pRootPageMem;
    memset(pHeaderAtStart, 0, sizeof(HeaderInRootRegularChunk));
    pHeaderAtStart->uRegularChunkPagesCount = 1;
    // The header itself is sized in a regular 'chunk', and is thus 'first' chunk in the root allocated page => register this as such...
    pHeaderAtStart->tRegularPages[0].uAllocated = 1uLL;
    pHeaderAtStart->tRegularPages[0].pPageMemoryStart = pRootPageMem;
    // If we asked for thread-safety,
    if (bThreadSafe) {
        // TODO
        Assert_(false);
        pHeaderAtStart->uLockableMaybe = 1u; // TODO: init mutex by os call, or something
    }
    // Finally, our 'provider' is internally nothing more than a pointer to this header
    outProvider->pRoot = pHeaderAtStart;
}

void release_chunk_provider(ChunkProvider* outProvider) {
    HeaderInRootRegularChunk* pRoot = outProvider->pRoot;
    if (pRoot) {
        if (pRoot->uLockableMaybe) {
            // TODO: thread-safety
            Assert_(false);
        }
        u16 uRegularPageCount = pRoot->uRegularChunkPagesCount;
        MemChunkPage* pRegularPageHeader = pRoot->tRegularPages;
        for (u16 uRegularPage = 0; uRegularPage < uRegularPageCount; uRegularPage++, pRegularPageHeader++) {
            if (pRegularPageHeader->pPageMemoryStart) {
                Assert_(pRegularPageHeader->uAllocated);
                platform_page_free(pRegularPageHeader->pPageMemoryStart);
            }
        }
        u16 uLargePageCount = pRoot->uLargePagesCount;
        LargePageHeader** pLargePageStartPointer = pRoot->tLargePages;
        for (u16 uLargePage = 0; uLargePage < uLargePageCount; uLargePage++, pLargePageStartPointer++) {
            u8* pLargePageStart = (u8*)(*pLargePageStartPointer);
            if (pLargePageStart) {
                platform_page_free(pLargePageStart);
            }
        }
        u16 uHugePageCount = pRoot->uHugePagesCount;
        u8** pHugePageStartPointer = pRoot->tHugePages;
        for (u16 uHugePage = 0; uHugePage < uHugePageCount; uHugePage++, pHugePageStartPointer++) {
            u8* pHugePageStart = (u8*)(*pHugePageStartPointer);
            if (pHugePageStart) {
                platform_page_free(pHugePageStart);
            }
        }
    }
    outProvider->pRoot = 0;
}

// Allocating regular-sized chunks should be one of the overall fastest strategies to quickly get some working memory...
// OS calls for reserving pages should happen roughly 1 time out of 64 calls to this, and regular chunks are otherwise fast allocated
//   (by full, fast user-space indexing) out of these, each chuck with a respectable size of 128 KB.
ChunkHandle ChunkProvider::allocate_regular_chunk()
{
    if (pRoot->uLockableMaybe) {
        // TODO: thread-safety
        Assert_(false);
    }
    // Searches for a regular page with remaining free slots for regular chunks
    //
    u16 uNonFullRegularPageIndex = 0;
    u16 uCurrentRegularPageCount = pRoot->uRegularChunkPagesCount;
    u16 uCountRegularPageGroup = (uCurrentRegularPageCount + 63u) >> 6;
    u64 uRegularFullGroupBits = pRoot->regular_page_bits_if_64group_full;
    // These loop may seem scary, but should not generally cost more than a handful of cycles... (max 2 x 64 x (loop + bitwise checks) in the very worst case)
    for (u16 uRegularPageGroup = 0; uRegularPageGroup < 64u; uRegularPageGroup++) {
        if (0 == (uRegularFullGroupBits & (1uLL << uRegularPageGroup))) {
            u64 uRegularFullPageBits = pRoot->regular_page_bits_if_full[uRegularPageGroup];
            Assert_(uRegularFullPageBits != 0xFFFF'FFFF'FFFF'FFFFuLL);
            u16 uMaxThere = _max(u16(uNonFullRegularPageIndex + 64u), uCurrentRegularPageCount);
            for (u16 uIndexThere = 0; uNonFullRegularPageIndex < uMaxThere; uIndexThere++, uNonFullRegularPageIndex++) {
                if (0 == (uRegularFullPageBits & (1uLL << uIndexThere)))
                    break;
            }
            break;
        } else {
            if (uNonFullRegularPageIndex + 64u < uCurrentRegularPageCount)
                uNonFullRegularPageIndex++;
            else {
                uNonFullRegularPageIndex = uCurrentRegularPageCount;
                break;
            }
        }
    }

    // Updating regular page count if necessary, and while doing so, checking if we did not totally run out of regular pages...
    //
    ChunkHandle result;
    if (uNonFullRegularPageIndex < uCurrentRegularPageCount) { 
        // usually NOOP... (in the 63 times out of 64 ballpark, but depending on fragmentation)
    } else {
        if (uNonFullRegularPageIndex < MAX_REGULAR_PAGES) {
            uCurrentRegularPageCount++;
            pRoot->uRegularChunkPagesCount++;
            Assert_(0 == pRoot->tRegularPages[uNonFullRegularPageIndex].pPageMemoryStart);
        } else {
#if ASSERT_FALSE_ON_OOM
            Assert_(false && "Reached Max Regular Pages Count for this ChunkProvider");
#endif
            result.uTagged = 0;
            return result;
        }
    }

    // retrieve header for page with a free slot, in which we'll allocate a chunk ; allocating it as a page at OS level if not yet allocated.
    //
    Assert_(uNonFullRegularPageIndex < pRoot->uRegularChunkPagesCount);
    MemChunkPage* pRegularPageHeader = pRoot->tRegularPages + uNonFullRegularPageIndex;
    u8* pPageStart = pRegularPageHeader->pPageMemoryStart;
    if (pPageStart != 0) {
        // usually NOOP also... (in the 63 times out of 64 ballpark, but depending on fragmentation)
    } else {
        Assert_(0 == pRegularPageHeader->uAllocated);
        pPageStart = platform_page_alloc(REGULAR_PAGE_SIZE);
        Assert_(0 == (reinterpret_cast<u64>(pPageStart) & CHUNK_HANDLE_TAG_MASK));
        pRegularPageHeader->pPageMemoryStart = pPageStart;
    }

    // Searches for a non allocated chunk in this page (and assuming there is one)
    //
    u64 uAllocatedSlotsBits = pRegularPageHeader->uAllocated;
    Assert_(uAllocatedSlotsBits != 0xFFFF'FFFF'FFFF'FFFFuLL);
    u16 uFreeSlotIndexInPage = 0;
    // This loop should not generally cost more than a handful of cycles... (max 64 x (loop + bitwise checks) in the worst case)
    for (; uFreeSlotIndexInPage < 64u; uFreeSlotIndexInPage++) {
        if (0 == (uAllocatedSlotsBits & (1uLL << uFreeSlotIndexInPage)))
            break;
    }
    Assert_(uFreeSlotIndexInPage < 64u);

    // Prepares result once we found the chunk position
    //
    u8* uChunkStart = pPageStart + size_t(uFreeSlotIndexInPage)*CHUNK_PROVIDER_REGULAR_CHUNK_SIZE;
    u64 uTagged = reinterpret_cast<u64>(uChunkStart);
    Assert_(0 == (uTagged & CHUNK_HANDLE_TAG_MASK));
    result.uTagged = uTagged | u64(uNonFullRegularPageIndex);

    // updates allocation flag for this chunk, then the 'page full' and 'group full' flags as necessary
    //
    uAllocatedSlotsBits |= (1uLL << uFreeSlotIndexInPage);
    pRegularPageHeader->uAllocated = uAllocatedSlotsBits;
    if (uAllocatedSlotsBits < 0xFFFF'FFFF'FFFF'FFFFuLL) {
        // usually NOOP also.. (in the 63 times out of 64 ballpark, but depending on fragmentation)
    } else {
        u16 uPageGroup = uNonFullRegularPageIndex >> 6;
        u16 uIndexInPageGroup = uNonFullRegularPageIndex & 0x3F;
        pRoot->regular_page_bits_if_full[uPageGroup] |= (1uLL << uIndexInPageGroup);
        if (pRoot->regular_page_bits_if_full[uPageGroup] == 0xFFFF'FFFF'FFFF'FFFFuLL) {
            pRoot->regular_page_bits_if_64group_full |= (1uLL << uPageGroup);
        }
    }

    return result;
}

ChunkHandle ChunkProvider::allocate_large_chunk(u32 uByteSize)
{
    if (pRoot->uLockableMaybe) {
        // TODO: thread-safety
        Assert_(false);
    }
    Assert_(uByteSize > CHUNK_PROVIDER_REGULAR_CHUNK_SIZE && "Min alloc as 'large' Chunk is 132 KB");
    Assert_(uByteSize < MAX_ALLOC_IN_LARGE_PAGE && "Max alloc as 'large' Chunk is 4 MB");
    Assert_(0 == (uByteSize & 0x0FFFu) && "Chunk size should be multiple of 4 KB");
    // TODO
    Assert_(false);
    ChunkHandle result;
    return result;
}

ChunkHandle ChunkProvider::allocate_huge_chunk(u64 uByteSize)
{
    if (pRoot->uLockableMaybe) {
        // TODO: thread-safety
        Assert_(false);
    }
    Assert_(uByteSize > MAX_ALLOC_IN_LARGE_PAGE && "Min alloc as 'huge' Chunk is over 4 MB");
    Assert_(0 == (uByteSize & 0x0FFFu) && "Chunk size should be multiple of 4 KB");
    // TODO
    Assert_(false);
    ChunkHandle result;
    return result;
}

ChunkHandle ChunkProvider::allocate_chunk(u64 uByteSize)
{
    if (uByteSize < MAX_ALLOC_IN_LARGE_PAGE) {
        if (uByteSize > CHUNK_PROVIDER_REGULAR_CHUNK_SIZE) {
            return allocate_large_chunk(u32(uByteSize));
        } else {
            Assert_(uByteSize == CHUNK_PROVIDER_REGULAR_CHUNK_SIZE && "Min alloc for Chunk is 128 KB");
            return allocate_regular_chunk();
        }
    } else {
        return allocate_huge_chunk(uByteSize);
    }
}

void ChunkProvider::release_regular_chunk(ChunkHandle handle)
{
    if (pRoot->uLockableMaybe) {
        // TODO: thread-safety
        Assert_(false);
    }
    // handle holds both the actual ptr to start of allocated chunk, and a tag which allows us to fast-retrieve page index.
    u8* pChunkStart = handle.get_ptr_to_data();
    u16 uRegularPageIndex = handle.get_page_index_for_kind();

    // from that, we'll find info about the corresponding regular page in which the chunk was allocated
    Assert_(uRegularPageIndex < pRoot->uRegularChunkPagesCount);
    MemChunkPage* pRegularPageHeader = pRoot->tRegularPages + uRegularPageIndex;
    u8* pPageStart = pRegularPageHeader->pPageMemoryStart;
    
    // assuming the page was allocated indeed... and that our ptr does indeed lie within the address range for that page...
    Assert_(pPageStart);
    size_t offset = pChunkStart - pPageStart;
    Assert_(offset < REGULAR_PAGE_SIZE);
    Assert_(0 == (offset & (CHUNK_PROVIDER_REGULAR_CHUNK_SIZE - 1u)));
    // ... retrieves actual position of the chunk within the page
    size_t uChunkIndex = offset / CHUNK_PROVIDER_REGULAR_CHUNK_SIZE;
    Assert_(uChunkIndex < 64u);

    // assuming chunk index is indeed flaggued as allocated, unflag it
    u64 uChunkIndexMask = 1uLL << uChunkIndex;
    Assert_(pRegularPageHeader->uAllocated & uChunkIndexMask);
    pRegularPageHeader->uAllocated &= ~uChunkIndexMask;
    
    // if no more allocated chunk in this page, release whole page at OS level
    if (0 == pRegularPageHeader->uAllocated) {
        platform_page_free(pRegularPageHeader->pPageMemoryStart);
        pRegularPageHeader->pPageMemoryStart = 0;
        // furthermore, if we just released the 'last' regular page , we can decrease regular page count
        if (uRegularPageIndex == pRoot->uRegularChunkPagesCount - 1u) {
            MemChunkPage* pPageHeaderToRelease = pRegularPageHeader;
            // and while we're at it, we'll do so for all trailing unallocated pages, checking each in turn, in reverse:
            while (pPageHeaderToRelease->pPageMemoryStart == 0) {
                Assert_(0 == pPageHeaderToRelease->uAllocated);
                if (pRoot->uRegularChunkPagesCount) {
                    pRoot->uRegularChunkPagesCount--;
                    pPageHeaderToRelease--;
                } else
                    break;
            }
        }
    }

    // always unflag page as 'full' when we release a regular chunk from a page... (and do not bother checking if it was not already 'full')
    u16 uRegularPageGroup = uRegularPageIndex >> 6;
    u16 uRegularPageIndexInGroup = uRegularPageIndex & 0x3Fu;
    pRoot->regular_page_bits_if_full[uRegularPageGroup] &= ~(1uLL << uRegularPageIndexInGroup);
    pRoot->regular_page_bits_if_64group_full &= ~(1uLL << uRegularPageGroup);
}

void ChunkProvider::release_large_chunk(ChunkHandle handle, u32 uChunkSize)
{
    if (pRoot->uLockableMaybe) {
        // TODO: thread-safety
        Assert_(false);
    }
    Assert_(uChunkSize > CHUNK_PROVIDER_REGULAR_CHUNK_SIZE && "Min alloc as 'large' Chunk is 132 KB");
    Assert_(uChunkSize < MAX_ALLOC_IN_LARGE_PAGE && "Max alloc as 'large' Chunk is 4 MB");
    Assert_(0 == (uChunkSize & 0x0FFFu) && "Chunk size should be multiple of 4 KB");
    // TODO
    Assert_(false);
}

void ChunkProvider::release_huge_chunk(ChunkHandle handle, u64 uChunkSize)
{
    if (pRoot->uLockableMaybe) {
        // TODO: thread-safety
        Assert_(false);
    }
    Assert_(uChunkSize > MAX_ALLOC_IN_LARGE_PAGE && "Min alloc as 'huge' Chunk is over 4 MB");
    Assert_(0 == (uChunkSize & 0x0FFFu) && "Chunk size should be multiple of 4 KB");
    // TODO
    Assert_(false);
}

void ChunkProvider::release_chunk(ChunkHandle handle, u64 uChunkSize)
{
    if (uChunkSize < MAX_ALLOC_IN_LARGE_PAGE) {
        if (uChunkSize > CHUNK_PROVIDER_REGULAR_CHUNK_SIZE) {
            return release_large_chunk(handle, u32(uChunkSize));
        } else {
            Assert_(uChunkSize == CHUNK_PROVIDER_REGULAR_CHUNK_SIZE && "Min alloc for Chunk is 128 KB");
            return release_regular_chunk(handle);
        }
    } else {
        return release_huge_chunk(handle, uChunkSize);
    }
}

#if 0

u16 _dichotomous_search_sorted_insertion_pos(u8** tAllStartAddressesSorted, u16 uCountAddresses, u8* pAddressToInsert)
{
    u16 uStart = 0;
    u16 uEnd = uCountAddresses;
    while (uStart < uEnd) {
        u16 uDelta = (uEnd - uStart) >> 1;
        u16 uMidPoint = uStart + uDelta;
        u8* pAddressAtMidPoint = tAllStartAddressesSorted[uMidPoint];
        if (pAddressAtMidPoint > pAddressToInsert) {
            uEnd = uMidPoint;
        } else {
            if (pAddressAtMidPoint == pAddressToInsert) {
                return uMidPoint;
            } else {
                uStart = uMidPoint+1;
            }
        }
    }
    return uStart;
}

void _on_add_one_update_sorted_index_impl(u8** tAllStartAddressesSorted, u16* tAllCorrespondingPageIndices,
    u8* pStartOfNewPage, u16 uPage)
{
    // look for a valid insertion point
    u16 uInsertionIndex = _dichotomous_search_sorted_insertion_pos(tAllStartAddressesSorted,
        uPage, pStartOfNewPage);
    assert(uInsertionIndex == uPage ||     // either append, or...
           (uInsertionIndex < uPage &&     // sanity check not a known address !!
            tAllStartAddressesSorted[uInsertionIndex] != pStartOfNewPage)); 
    // first, copies every address (and page index) past 'uInsertionIndex' forward one slot
    for (u16 uIndex = uPage; uIndex > uInsertionIndex; uIndex--) {
        tAllStartAddressesSorted[uIndex] = tAllStartAddressesSorted[uIndex-1];
        tAllCorrespondingPageIndices[uIndex] = tAllCorrespondingPageIndices[uIndex-1];
    }
    // then simply add ptr to new page (and page index) at uInsertionIndex
    tAllStartAddressesSorted[uInsertionIndex] = pStartOfNewPage;
    tAllCorrespondingPageIndices[uInsertionIndex] = uPage;
}

// ---------------------------------------------------------
// Notes about the following methods:
//      _on_add_one_update_sorted_index_regular,
//      _on_add_one_update_sorted_index_large,
//      _on_add_one_update_sorted_index_huge
// ---------------------------------------------------------
// About _on_add_one_update_sorted_index_regular :
//
// We could get 'uRegularChunkPagesCount' to know the actual count to lookup against, here...
//   but using the 'uPage' parameter instead allows us to simply recursively call ourselves to
//   handle the 'uPage == 16' case... since we're otherwise always called with
//   uPage == uRegularChunkPagesCount-1, this should work perfectly...
// ---------------------------------------------------------
// And same rationale applies for, respectively,
//   large pages with uLargePagesCount, or
//   huge pages with uHugePagesCount.
// ---------------------------------------------------------


void _on_add_one_update_sorted_index_regular(HeaderInRootRegularChunk* pRoot, u8* pStartOfNewPage, u16 uPage)
{
    u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;
    u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_REGULAR_ADDRESSES);
    u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_REGULAR_PAGEINDEX);
    
    if (uPage == 16) { // the index was just used for the first time... => we need to fill the first 16 also!
        for (u16 uPrevPageToAdd = 0; uPrevPageToAdd < 16; uPrevPageToAdd++)
        {
            u8* pStartOfPrevPage = pRoot->tRegularPages[uPrevPageToAdd].pPageMemoryStart;
            _on_add_one_update_sorted_index_regular(pRoot, pStartOfPrevPage, uPrevPageToAdd); // see notes above
        }
    } else {
        assert(uPage < 16 || uPage == pRoot->uRegularChunkPagesCount - 1); // see notes above
    }

    // actually call the common "update index" method with tables specific to regular pages, and 'uPage' as index
    _on_add_one_update_sorted_index_impl(tAllStartAddressesSorted, tAllCorrespondingPageIndices,
        pStartOfNewPage, uPage);
}

void _on_add_one_update_sorted_index_large(HeaderInRootRegularChunk* pRoot, LargePageHeader* pStartOfNewPageInclHeader, u16 uPage)
{
    u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;
    u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_LARGE_ADDRESSES);
    u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_LARGE_PAGEINDEX);
    
    if (uPage == 16) { // the index was just used for the first time... => we need to fill the first 16 also!
        for (u16 uPrevPageToAdd = 0; uPrevPageToAdd < 16; uPrevPageToAdd++)
        {
            LargePageHeader* pStartOfPrevPageInclHeader = pRoot->tLargePages[uPrevPageToAdd];
            _on_add_one_update_sorted_index_large(pRoot, pStartOfPrevPageInclHeader, uPrevPageToAdd); // see notes above
        }
    } else {
        assert(uPage < 16 || uPage == pRoot->uLargePagesCount - 1); // see notes above
    }

    u8* pStartOfNewPage = pStartOfNewPageInclHeader->tNodes[0].pStart;
    // actually call the common "update index" method with tables specific to large pages, and 'uPage' as index
    _on_add_one_update_sorted_index_impl(tAllStartAddressesSorted, tAllCorrespondingPageIndices,
        pStartOfNewPage, uPage);
}

void _on_add_one_update_sorted_index_huge(HeaderInRootRegularChunk* pRoot, u8* pStartOfNewPage, u16 uPage)
{
    u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;
    u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_HUGE_ADDRESSES);
    u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_HUGE_PAGEINDEX);
    
    if (uPage == 16) { // the index was just used for the first time... => we need to fill the first 16 also!
        for (u16 uPrevPageToAdd = 0; uPrevPageToAdd < 16; uPrevPageToAdd++)
        {
            u8* pStartOfPrevPage = pRoot->tHugePages[uPrevPageToAdd];
            _on_add_one_update_sorted_index_huge(pRoot, pStartOfPrevPage, uPrevPageToAdd); // see notes above
        }
    } else {
        assert(uPage < 16 || uPage == pRoot->uHugePagesCount - 1); // see notes above
    }

    // actually call the common "update index" method with tables specific to huge pages, and 'uPage' as index
    _on_add_one_update_sorted_index_impl(tAllStartAddressesSorted, tAllCorrespondingPageIndices,
        pStartOfNewPage, uPage);
}

u8* allocate_regular_chunk_from_thread_unsafe_provider(LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u16 uCurrentCountRegular = pRoot->uRegularChunkPagesCount;
    MemChunkPage* pTableRegular = pRoot->tRegularPages;
    u64 uRegularPagesLargeHeader = pRoot->uRegularPagesLargeHeader;
    u64* tMedHeaders = pRoot->tRegularPagesMedHeader;
    if (uRegularPagesLargeHeader != FULL_64_B) {
        u16 uMed;
        if (0 == (uRegularPagesLargeHeader & 0x01uLL)) {
            uMed = 0;
        } else {
            unsigned long uAvailablePosInLarge;
            _BitScanForward64(&uAvailablePosInLarge, ~uRegularPagesLargeHeader);
            uMed = u16(uAvailablePosInLarge);
        }
        u64 uMedHeader = tMedHeaders[uMed];
        assert(uMedHeader != FULL_64_B);
        u16 uStartLeaf = uMed*64;
        u16 uLeaf;
        if (0 == (uMedHeader & 0x01uLL)) {
            uLeaf = uStartLeaf;
        } else {
            unsigned long uAvailablePosInMed;
            _BitScanForward64(&uAvailablePosInMed, ~uMedHeader);
            uLeaf = uStartLeaf + u16(uAvailablePosInMed);
        }
        if (uLeaf < uCurrentCountRegular) {
            assert(pTableRegular[uLeaf].pPageMemoryStart);
            assert(pTableRegular[uLeaf].uAllocated != FULL_64_B);
            u8* pResult = pTableRegular[uLeaf].pPageMemoryStart;
            if (0 == (pTableRegular[uLeaf].uAllocated & 0x01)) {
                pTableRegular[uLeaf].uAllocated |= 0x01;
            } else {
                unsigned long uAvailablePos;
                _BitScanForward64(&uAvailablePos, ~(pTableRegular[uLeaf].uAllocated));
                pTableRegular[uLeaf].uAllocated |= (0x01uLL << uAvailablePos);
                pResult += size_t(uAvailablePos) * CHUNK_PROVIDER_REGULAR_CHUNK_SIZE;
            }
            if (pTableRegular[uLeaf].uAllocated == FULL_64_B)
            {
                tMedHeaders[uMed] |= (0x01uLL << (uLeaf - uStartLeaf));
                if (tMedHeaders[uMed] == FULL_64_B)
                    pRoot->uRegularPagesLargeHeader |= (0x01uLL << uMed);
            }
            return pResult;
        } else {
            assert(uLeaf == uCurrentCountRegular); // only expects increment by 1
            u8* pStartOfNewPage = pProvider->pOsFuncs->pPageAllocFn(REGULAR_PAGE_SIZE);
            if (pStartOfNewPage)
            {
                pTableRegular[uCurrentCountRegular].pPageMemoryStart = pStartOfNewPage;
                pTableRegular[uCurrentCountRegular].uAllocated = 0x0000000000000001uLL;
                pRoot->uRegularChunkPagesCount = uCurrentCountRegular + 1;
                if (uCurrentCountRegular >= 16) {
                    u8* pSortedIndexEachIfMoreThan16 = pRoot->pSortedIndexEachIfMoreThan16;
                    if (!pSortedIndexEachIfMoreThan16) {
                        // We need a new regular chunk for storing the index, and we just allocated a new page for regulars...
                        // => avoid the mess of calling alloc recursively while in the middle of
                        //    updating our counters, and simply also flag the second chunk in that new page as allocated...
                        pTableRegular[uCurrentCountRegular].uAllocated = 0x0000000000000003uLL;
                        pSortedIndexEachIfMoreThan16 = pStartOfNewPage + CHUNK_PROVIDER_REGULAR_CHUNK_SIZE;
                        pRoot->pSortedIndexEachIfMoreThan16 = pSortedIndexEachIfMoreThan16;
                    }
                    _on_add_one_update_sorted_index_regular(pRoot, pStartOfNewPage, uCurrentCountRegular);
                }
                return pStartOfNewPage;
            }
            else
            {
                // OS did not provide required memory...
#               if ASSERT_FALSE_ON_OOM
                    assert(false);  // uh oh...
#               endif
                return 0;
            }
        }
    } else {
        assert(MAX_REGULAR_PAGES == uCurrentCountRegular);
        // No more room for a regular chunk in this allocator...
#       if ASSERT_FALSE_ON_OOM
            assert(false);  // uh oh...
#       endif
        return 0;
    }
}

void _on_found_regular_page_for_releasing_chunk(HeaderInRootRegularChunk* pRoot, u16 uPage, u8* pRegularAllocatedPtr)
{
    MemChunkPage& refPage = pRoot->tRegularPages[uPage];

    // we compute the actual chunk slot in page by some simple math
    size_t uBytePosInPage = pRegularAllocatedPtr - refPage.pPageMemoryStart;
    size_t uChunkInPage = uBytePosInPage / CHUNK_PROVIDER_REGULAR_CHUNK_SIZE;
    assert(0 == uBytePosInPage % CHUNK_PROVIDER_REGULAR_CHUNK_SIZE);    // end-user should have ensured to ask for release from a valid chunk address !!
    
    // now simply 'unflag' that chunk slot in that page... and we're done releasing :)
    u64 uPrevPageAllocField = refPage.uAllocated;
    u64 uMaskChunkSlot = 0x01uLL << uChunkInPage;
    assert(uPrevPageAllocField & uMaskChunkSlot);               // end-user should have ensured to ask for release of something actually allocated !!
    refPage.uAllocated = uPrevPageAllocField & (~uMaskChunkSlot);

    if (uPrevPageAllocField == FULL_64_B) { // but wait, if we just went from full to... **not full**
        // also unflag our 'header' bitfields accordingly
        u16 uMed = uPage >> 6;
        u16 uPosInMed = uPage & 0x003F;
        u64 uMaskInMed = (0x01uLL << uPosInMed);
        assert(pRoot->tRegularPagesMedHeader[uMed] & uMaskInMed); // sanity check
        pRoot->tRegularPagesMedHeader[uMed] &= ~uMaskInMed;
        u16 uPosInLarge = uMed & 0x003F;
        u64 uMaskInLarge = (0x01uLL << uPosInLarge);
        pRoot->uRegularPagesLargeHeader &= ~uMaskInLarge;
    }
}

void release_regular_chunk_from_thread_unsafe_provider(u8* pRegularAllocatedPtr, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u16 uCurrentCountRegular = pRoot->uRegularChunkPagesCount;
    MemChunkPage* pTableRegular = pRoot->tRegularPages;

    if (uCurrentCountRegular > 16)
    {
        // Past 16 "regular" pages, we should have instanciated a sorted index to
        //    help pinpoint the page holding a given allocated pointer, quite faster
        //    (up to the max 4K pages to lookup !!)
        u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;
        assert(pSortedIndex); // sanity check
        u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_REGULAR_ADDRESSES);
        // and here we ask the index to give us back the actual page...
        u16 uIndex = _dichotomous_search_sorted_insertion_pos(tAllStartAddressesSorted, uCurrentCountRegular,
            pRegularAllocatedPtr);
        assert(uIndex <= uCurrentCountRegular);
        // if 'pRegularAllocatedPtr' was precisely at start of a page, we got directly the final 'uIndex', having
        //    salvaged the '_dichotomous_search_sorted_insertion_pos' implementation used for insertion...
        if (uIndex == uCurrentCountRegular || tAllStartAddressesSorted[uIndex] > pRegularAllocatedPtr) {
            assert(uIndex > 0); // otherwise, the address at previous index is instead the correct one
            uIndex -= 1;
        }
        // given that index, we now get the page...
        u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_REGULAR_PAGEINDEX);
        u16 uPage = tAllCorrespondingPageIndices[uIndex];
        u8* pPageStart = pTableRegular[uPage].pPageMemoryStart;
        assert(tAllStartAddressesSorted[uIndex] == pPageStart);         // sanity check
        assert(pRegularAllocatedPtr >= pPageStart);                     // sanity check
        assert(pRegularAllocatedPtr < pPageStart + REGULAR_PAGE_SIZE);  // end-user should have ensured to ask for release from a valid chunk address !!
        // now actually release from that page...
        _on_found_regular_page_for_releasing_chunk(pRoot, uPage, pRegularAllocatedPtr);
    }
    else
    {
        // Up to (including) 16 pages, we'd simply iterate against each of them, instead...
        for (u16 uPage = 0; uPage < uCurrentCountRegular; uPage++) {
            u8* pPageStart = pTableRegular[uPage].pPageMemoryStart;
            if (pRegularAllocatedPtr >= pPageStart && pRegularAllocatedPtr < pPageStart + REGULAR_PAGE_SIZE) {
                // we found the page within which lies our allocated pointer
                // => now actually release from that page...
                _on_found_regular_page_for_releasing_chunk(pRoot, uPage, pRegularAllocatedPtr);
                return; // and stop searching, obviously
            }
        }
        // when falling to that part of the code without having found a matching page... 
        assert(false); // end-user should have ensured we would find that chunk among our pages eventually...
    }
}


u8* _try_alloc_large_chunk_given_nodes(LargeAllocNode* tNodes, u32 uChunkSize)
{
    LargeAllocNode& nodeFreeSpaceAtStart = tNodes[0];
    u8* pPageStart = nodeFreeSpaceAtStart.pStart;
    if (nodeFreeSpaceAtStart.pEnd - pPageStart >= uChunkSize) {
        // enough space available right at the start, identified by the "available free space at start" node
        // => we'll alloc as a "first" allocated node, starting from the **top** of the available space.
        //    (to keep remaining space easily identifiable in the "free space at start" node)
        
        // first, copies every currently allocated nodes forward one slot (including end of nodes guard)
        LargeAllocNode* pLastKnownNode = tNodes + 1;
        while (pLastKnownNode->pEnd > pLastKnownNode->pStart) // otherwise denotes end of allocated nodes
            pLastKnownNode++;
        assert((pLastKnownNode->pStart == pLastKnownNode->pEnd) && (pLastKnownNode->pEnd == pPageStart + LARGE_PAGE_SIZE));
        assert(size_t(pLastKnownNode - tNodes) < 255);
        for (LargeAllocNode* pCurrentNode = pLastKnownNode+1; pCurrentNode > tNodes + 1; pCurrentNode--) {
            pCurrentNode->pStart = (pCurrentNode - 1)->pStart;
            pCurrentNode->pEnd = (pCurrentNode - 1)->pEnd;
        }
        // then updates the "free space at start" node and inits the new "first" allocated node accordingly
        u8* pEndOfFreeSpaceAtStart = nodeFreeSpaceAtStart.pEnd;
        tNodes[1].pEnd = pEndOfFreeSpaceAtStart;
        pEndOfFreeSpaceAtStart -= uChunkSize;
        nodeFreeSpaceAtStart.pEnd = pEndOfFreeSpaceAtStart;
        tNodes[1].pStart = pEndOfFreeSpaceAtStart;
        // that's it...
        return pEndOfFreeSpaceAtStart;
    } else {
        // not enough space available right at the start of the page, but still possible to find space inbetween
        //   allocated nodes... (including case of "past last allocated node" which is algorithmically equivalent)
        LargeAllocNode* pLookingForNode = tNodes + 1;
        while (pLookingForNode->pEnd > pLookingForNode->pStart) { // otherwise denotes end of allocated nodes
            u8* pEndOfAllocatedSpaceBefore = pLookingForNode->pEnd;
            if ((pLookingForNode + 1)->pStart - pEndOfAllocatedSpaceBefore >= uChunkSize) {
                // enough space available inbetween two allocated nodes (between pLookingForNode and pLookingForNode+1)
                // => we'll insert a new node right after 'pLookingForNode'

                // first, copies every currently allocated nodes past 'pLookingForNode' forward one slot
                //    (including end of nodes guard)
                LargeAllocNode* pLastKnownNode = pLookingForNode + 1;
                while (pLastKnownNode->pEnd > pLastKnownNode->pStart) // otherwise denotes end of allocated nodes
                    pLastKnownNode++;
                assert((pLastKnownNode->pStart == pLastKnownNode->pEnd) && (pLastKnownNode->pEnd == pPageStart + LARGE_PAGE_SIZE));
                assert(size_t(pLastKnownNode - tNodes) < 255);
                for (LargeAllocNode* pCurrentNode = pLastKnownNode+1; pCurrentNode > pLookingForNode + 1; pCurrentNode--) {
                    pCurrentNode->pStart = (pCurrentNode - 1)->pStart;
                    pCurrentNode->pEnd = (pCurrentNode - 1)->pEnd;
                }
                // then inits the new allocated node (situated at pLookingForNode+1) accordingly
                pLookingForNode[1].pStart = pEndOfAllocatedSpaceBefore;
                pLookingForNode[1].pEnd = pEndOfAllocatedSpaceBefore + uChunkSize;
                // that's it...
                return pEndOfAllocatedSpaceBefore;
            }
        }
        // if we reach that point, we could not find enough space...
        return 0;
    }
}

bool _has_min_space_remaining_within_nodes(LargeAllocNode* tNodes)
{
    LargeAllocNode& nodeFreeSpaceAtStart = tNodes[0];
    u8* pPageStart = nodeFreeSpaceAtStart.pStart;
    if (nodeFreeSpaceAtStart.pEnd - pPageStart >= CHUNK_PROVIDER_MIN_LARGE_CHUNK_SIZE) {
        // enough space available right at the "available free space at start"
        return true;
    } else {
        // not enough space available right at the start of the page, but still possible to find space inbetween
        //   allocated nodes... (including case of "past last allocated node" which is algorithmically equivalent)
        LargeAllocNode* pLookingForNode = tNodes + 1;
        while (pLookingForNode->pEnd > pLookingForNode->pStart) { // otherwise denotes end of allocated nodes
            if ((pLookingForNode + 1)->pStart - pLookingForNode->pEnd >= CHUNK_PROVIDER_MIN_LARGE_CHUNK_SIZE) {
                // enough space available inbetween two allocated nodes (between pLookingForNode and pLookingForNode+1)
                return true;
            }
        }
        // if we reach that point, we could not find enough space...
        return false;
    }
}

u8* _allocate_large_chunk_from_thread_unsafe_provider(u32 uChunkSize, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    assert((uChunkSize & 0x0FFF) == 0); // req multiple of 4K
    assert((uChunkSize > CHUNK_PROVIDER_REGULAR_CHUNK_SIZE) && (uChunkSize <= MAX_ALLOC_IN_LARGE_PAGE));
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u16 uCurrentCountLarge = pRoot->uLargePagesCount;
    LargePageHeader** pTableLarge = pRoot->tLargePages;
    u64 uLargePagesLargeHeader = pRoot->uLargePagesLargeHeader;
    u64* tMedHeaders = pRoot->tLargePagesMedHeader;
    // we'll actually possibly update a local copy of uLargePagesLargeHeader with more '1s' than
    //   actually stored, when not found enough room for *that* request
    while (uLargePagesLargeHeader != FULL_64_B) { 
        u16 uMed;
        if (0 == (uLargePagesLargeHeader & 0x01uLL)) {
            uMed = 0;
        } else {
            unsigned long uAvailablePosInLarge;
            _BitScanForward64(&uAvailablePosInLarge, ~uLargePagesLargeHeader);
            uMed = u16(uAvailablePosInLarge);
        }
        u64 uMedHeader = tMedHeaders[uMed];
        assert(uMedHeader != FULL_64_B);
        // we'll actually possibly update a local copy of uMedHeader with more '1s' than
        //   actually stored, when not found enough room for *that* request
        while (uMedHeader != FULL_64_B) {
            u16 uStartLeaf = uMed*64;
            u16 uLeaf;
            if (0 == (uMedHeader & 0x01uLL)) {
                uLeaf = uStartLeaf;
            } else {
                unsigned long uAvailablePosInMed;
                _BitScanForward64(&uAvailablePosInMed, ~uMedHeader);
                uLeaf = uStartLeaf + u16(uAvailablePosInMed);
            }
            if (uLeaf < uCurrentCountLarge) {
                LargeAllocNode* tNodes = pTableLarge[uLeaf]->tNodes;
                u8* pResult = _try_alloc_large_chunk_given_nodes(tNodes, uChunkSize);
                if (pResult) {
                    if (!_has_min_space_remaining_within_nodes(tNodes)) {
                        tMedHeaders[uMed] |= (0x01uLL << (uLeaf - uStartLeaf));
                        if (tMedHeaders[uMed] == FULL_64_B)
                            pRoot->uLargePagesLargeHeader |= (0x01uLL << uMed);
                    }
                    return pResult;
                } else
                    // this page was flaggued with some space remaining, but did not have "enough" space
                    //   for our current uChunkSize requirement => temporarily flag this one as
                    //   a no-go for us, to try another one on next iteration
                    uMedHeader |= (0x01uLL << (uLeaf - uStartLeaf));
            } else {
                // The leaf index which was returned to us is actually past the end of current allocated pages
                assert(uLeaf == uCurrentCountLarge); // only expects increment by 1
                LargePageHeader* pNewLargePage = (LargePageHeader*)pProvider->pOsFuncs->pPageAllocFn(LARGE_PAGE_TOTAL_SIZE);
                pTableLarge[uCurrentCountLarge] = pNewLargePage;
                LargeAllocNode* pFirstNode = pNewLargePage->tNodes;
                pFirstNode->pStart = ((u8*)pNewLargePage) + sizeof(LargePageHeader);
                pFirstNode->pEnd = pFirstNode->pStart + LARGE_PAGE_SIZE - uChunkSize;
                pFirstNode[1].pStart = pFirstNode->pEnd;
                pFirstNode[1].pEnd = pFirstNode[1].pStart + uChunkSize;
                pFirstNode[2].pStart = pFirstNode[2].pEnd = pFirstNode[1].pEnd;
                pRoot->uLargePagesCount = uCurrentCountLarge + 1;
                if (uCurrentCountLarge >= 16) {
                    u8* pSortedIndexEachIfMoreThan16 = pRoot->pSortedIndexEachIfMoreThan16;
                    if (!pSortedIndexEachIfMoreThan16) {
                        pSortedIndexEachIfMoreThan16 = allocate_regular_chunk_from_thread_unsafe_provider(pProvider);
                        pRoot->pSortedIndexEachIfMoreThan16 = pSortedIndexEachIfMoreThan16;
                    }
                    _on_add_one_update_sorted_index_large(pRoot, pNewLargePage, uCurrentCountLarge);
                }
                return pFirstNode[1].pStart;
            }
        }
        // this bunch of 64 pages had some pages flaggued with some space remaining, but none of them
        //   had "enough" space for our current uChunkSize requirement => temporarily flag this bunch as
        //   a no-go for us, to try another one on next iteration
        uLargePagesLargeHeader |= (0x01uLL << uMed);
    }

    // No more room for a regular chunk in this allocator...
#   if ASSERT_FALSE_ON_OOM
        assert(false);  // uh oh...
#   endif
    return 0;
}

void _release_alloc_from_node(LargeAllocNode* tNodes, u8* pAllocatedPtr, u32 uChunkSize)
{
    LargeAllocNode& nodeFreeSpaceAtStart = tNodes[0];
    LargeAllocNode* pCurrent = tNodes + 1;
    while (pCurrent->pEnd > pCurrent->pStart) { // otherwise denotes end of allocated nodes
        if (pCurrent->pStart == pAllocatedPtr) {
            assert (pCurrent->pEnd - pCurrent->pStart == uChunkSize);
            if (pCurrent == tNodes + 1) { // if releasing first node...
                // reclaims into "free space at start" (up to start of next alloc, or end)
                nodeFreeSpaceAtStart.pEnd = (pCurrent + 1)->pStart;
            }
            while (pCurrent->pEnd > pCurrent->pStart) { // otherwise denotes end of allocated nodes
                // copies everything back one slot, after released node (including end of nodes guard)
                pCurrent->pStart = (pCurrent+1)->pStart;
                pCurrent->pEnd = (pCurrent+1)->pEnd;
            }
            return;
        }
    }
    // caller or end-user should have ensured we would find that alloc among our nodes eventually...
    assert(false);
}

void _on_found_large_page_for_releasing_chunk(HeaderInRootRegularChunk* pRoot, u16 uPage, u8* pAllocatedPtr, u32 uChunkSize)
{
    LargeAllocNode* tNodes = pRoot->tLargePages[uPage]->tNodes;
    _release_alloc_from_node(tNodes, pAllocatedPtr, uChunkSize);
    u16 uMed = uPage >> 6;
    u16 uPosInMed = uPage & 0x003F;
    u64 uMaskInMed = (0x01uLL << uPosInMed);
    pRoot->tLargePagesMedHeader[uMed] &= ~uMaskInMed;
    u16 uPosInLarge = uMed & 0x003F;
    u64 uMaskInLarge = (0x01uLL << uPosInLarge);
    pRoot->uLargePagesLargeHeader &= ~uMaskInLarge;
}

void _release_large_chunk_from_thread_unsafe_provider(u8* pAllocatedPtr, u32 uChunkSize, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    assert((uChunkSize & 0x0FFF) == 0); // req multiple of 4K
    assert((uChunkSize > CHUNK_PROVIDER_REGULAR_CHUNK_SIZE) && (uChunkSize <= MAX_ALLOC_IN_LARGE_PAGE));
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u16 uCurrentCountLarge = pRoot->uLargePagesCount;
    LargePageHeader** pTableLargePtrToHeader = pRoot->tLargePages;
    if (uCurrentCountLarge > 16)
    {
        // Past 16 "large" pages, we should have instanciated a sorted index to
        //    help pinpoint the page holding a given allocated pointer, quite faster
        //    (up to the max 4K pages to lookup !!)
        u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;
        assert(pSortedIndex);
        u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_LARGE_ADDRESSES);
        // and here we ask the index to give us back the actual page...
        u16 uIndex = _dichotomous_search_sorted_insertion_pos(tAllStartAddressesSorted, uCurrentCountLarge,
            pAllocatedPtr);
        assert(uIndex <= uCurrentCountLarge);
        // if 'pAllocatedPtr' was precisely at start of a page, we got directly the final 'uIndex', having
        //    salvaged the '_dichotomous_search_sorted_insertion_pos' implementation used for insertion...
        if (uIndex == uCurrentCountLarge || tAllStartAddressesSorted[uIndex] > pAllocatedPtr) {
            assert(uIndex > 0); // otherwise, the address at previous index is instead the correct one
            uIndex -= 1;
        }
        // given that index, we now get the page...
        u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_LARGE_PAGEINDEX);
        u16 uPage = tAllCorrespondingPageIndices[uIndex];
        LargeAllocNode* tNodes = pTableLargePtrToHeader[uPage]->tNodes;
        u8* pPageStart = tNodes[0].pStart;
        assert(tAllStartAddressesSorted[uIndex] == pPageStart);  // sanity check
        assert(pAllocatedPtr >= pPageStart);                     // sanity check
        assert(pAllocatedPtr < pPageStart + LARGE_PAGE_SIZE);    // end-user should have ensured to ask for release from a valid chunk address !!
        // now actually release from that page...
        _on_found_large_page_for_releasing_chunk(pRoot, uPage, pAllocatedPtr, uChunkSize);
    }
    else
    {
        // Up to (and including) 16 pages, we'd simply iterate over each of them, instead
        for (u16 uPage = 0; uPage < uCurrentCountLarge; uPage++) {
            LargeAllocNode* tNodes = pTableLargePtrToHeader[uPage]->tNodes;
            u8* pPageStart = tNodes[0].pStart;
            if (pAllocatedPtr >= pPageStart && pAllocatedPtr < pPageStart + LARGE_PAGE_SIZE) {
                // we found the page within which lies our allocated pointer
                // => now actually release from that page...
                _on_found_large_page_for_releasing_chunk(pRoot, uPage, pAllocatedPtr, uChunkSize);
                return; // and stop searching, obviously
            }
        }
        // when falling to that part of the code without having found a matching page... 
        assert(false); // end-user should have ensured we would find that chunk among our pages eventually...
    }
}

u8* _allocate_huge_chunk_from_thread_unsafe_provider(u32 uChunkSize, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    assert((uChunkSize & 0x0FFF) == 0); // req multiple of 4K
    assert(uChunkSize > MAX_ALLOC_IN_LARGE_PAGE); // otherwise should have called the '_allocate_large...' version
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u16 uCurrentCountHuge = pRoot->uHugePagesCount;
    if (uCurrentCountHuge < MAX_HUGE_PAGES) {
        u8** pTableHugePtr = pRoot->tHugePages;
        u8* pNewHugePage = pProvider->pOsFuncs->pPageAllocFn(uChunkSize);
        pTableHugePtr[uCurrentCountHuge] = pNewHugePage;
        pRoot->uHugePagesCount = uCurrentCountHuge + 1;
        if (uCurrentCountHuge >= 16) {
            u8* pSortedIndexEachIfMoreThan16 = pRoot->pSortedIndexEachIfMoreThan16;
            if (!pSortedIndexEachIfMoreThan16) {
                pSortedIndexEachIfMoreThan16 = allocate_regular_chunk_from_thread_unsafe_provider(pProvider);
                pRoot->pSortedIndexEachIfMoreThan16 = pSortedIndexEachIfMoreThan16;
            }
            _on_add_one_update_sorted_index_huge(pRoot, pNewHugePage, uCurrentCountHuge);
        }
        return pNewHugePage;
    } else {
#       if ASSERT_FALSE_ON_OOM
            assert(false);  // uh oh...
#       endif
        return 0;
    }
}

void _release_huge_chunk_from_thread_unsafe_provider(u8* pAllocatedPtr, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u16 uCurrentCountHuge = pRoot->uHugePagesCount;
    u8** pTableHuge = pRoot->tHugePages;
    if (uCurrentCountHuge > 16)
    {
        // Past 16 "huge" pages, we should have instanciated a sorted index to
        //    help pinpoint the page holding a given allocated pointer, quite faster
        //    (up to the max almost 4K pages to lookup !!)
        u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;
        assert(pSortedIndex);
        u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_HUGE_ADDRESSES);
        // and here we ask the index to give us back the actual page...
        u16 uIndex = _dichotomous_search_sorted_insertion_pos(tAllStartAddressesSorted, uCurrentCountHuge,
            pAllocatedPtr);
        assert(uIndex < uCurrentCountHuge);
        // 'pAllocatedPtr' **should always** be precisely at start of a huge page...
        // end-user should have ensured to ask for release from a valid chunk address !!
        assert(tAllStartAddressesSorted[uIndex] == pAllocatedPtr);

        // given that index, we now get the page...
        u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_HUGE_PAGEINDEX);
        u16 uPage = tAllCorrespondingPageIndices[uIndex];
        u8* pPageStart = pTableHuge[uPage];
        assert(pAllocatedPtr == pPageStart);    // sanity check at this point
        // now actually release from that page by unallocating OS-wise !
        pProvider->pOsFuncs->pPageFreeFn(pAllocatedPtr);
        // swap-with-last-remove
        uCurrentCountHuge -= 1;
        if (uPage < uCurrentCountHuge) {
            pTableHuge[uPage] = pTableHuge[uCurrentCountHuge];
        }
        pProvider->pRoot->uHugePagesCount = uCurrentCountHuge;
        if (uCurrentCountHuge > 16) {
            // and also remove that page from our index... (if we're not back to at-or-below 16 total)
            for (u16 uCount = uIndex; uCount < uCurrentCountHuge; uCount++) {
                tAllStartAddressesSorted[uCount] = tAllStartAddressesSorted[uCount+1];
                tAllCorrespondingPageIndices[uCount] = tAllCorrespondingPageIndices[uCount+1];
            }
        }
    }
    else
    {
        // Up to (and including) 16 pages, we'd simply iterate over each of them, instead
        for (u16 uPage = 0; uPage < uCurrentCountHuge; uPage++) {
            u8* pPageStart = pTableHuge[uPage];
            if (pAllocatedPtr == pPageStart) {
                // we found the page which corresponds to our allocated pointer
                // => now actually release from that page by unallocating OS-wise !
                pProvider->pOsFuncs->pPageFreeFn(pAllocatedPtr);
                // swap-with-last-remove
                uCurrentCountHuge -= 1;
                if (uPage < uCurrentCountHuge) {
                    pTableHuge[uPage] = pTableHuge[uCurrentCountHuge];
                }
                pProvider->pRoot->uHugePagesCount = uCurrentCountHuge;
                return; // and stop searching, obviously
            }
        }
        // when falling to that part of the code without having found a matching page... 
        assert(false); // end-user should have ensured we would find that chunk among our pages eventually...
    }
}

u8* allocate_chunk_from_thread_unsafe_provider(u32 uChunkSize, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    if (uChunkSize < CHUNK_PROVIDER_REGULAR_CHUNK_SIZE) {
        assert(false);
        return 0;
    } else {
        assert((uChunkSize & 0x0FFF) == 0); // req multiple of 4K
        if (uChunkSize == CHUNK_PROVIDER_REGULAR_CHUNK_SIZE)
            return allocate_regular_chunk_from_thread_unsafe_provider(pProvider);
        else if (uChunkSize <= MAX_ALLOC_IN_LARGE_PAGE)
            return _allocate_large_chunk_from_thread_unsafe_provider(uChunkSize, pProvider);
        else
            return _allocate_huge_chunk_from_thread_unsafe_provider(uChunkSize, pProvider);
    }
}

void release_chunk_from_thread_unsafe_provider(u8* pAllocatedPtr, u32 uChunkSize, LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    if (uChunkSize < CHUNK_PROVIDER_REGULAR_CHUNK_SIZE)
        assert(false);
    else {
        assert((uChunkSize & 0x0FFF) == 0); // req multiple of 4K
        if (uChunkSize == CHUNK_PROVIDER_REGULAR_CHUNK_SIZE)
            return release_regular_chunk_from_thread_unsafe_provider(pAllocatedPtr, pProvider);
        else if (uChunkSize <= MAX_ALLOC_IN_LARGE_PAGE)
            return _release_large_chunk_from_thread_unsafe_provider(pAllocatedPtr, uChunkSize, pProvider);
        else
            return _release_huge_chunk_from_thread_unsafe_provider(pAllocatedPtr, pProvider);
    }
}

void _remove_from_regular_index(u8* pSortedIndex, u8* pPageStart, u16 uPage, u16 uCount)
{
    u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_REGULAR_ADDRESSES);
    u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_REGULAR_PAGEINDEX);
    u16 uIndex = _dichotomous_search_sorted_insertion_pos(tAllStartAddressesSorted, uCount, pPageStart);
    assert(uIndex < uCount);
    assert(tAllStartAddressesSorted[uIndex] == pPageStart);
    assert(tAllCorrespondingPageIndices[uIndex] == uPage);
    for (u16 uIndexPastThat = uIndex; uIndexPastThat < uCount-1; uIndexPastThat++) {
        tAllStartAddressesSorted[uIndexPastThat] = tAllStartAddressesSorted[uIndexPastThat+1];
        tAllCorrespondingPageIndices[uIndexPastThat] = tAllCorrespondingPageIndices[uIndexPastThat+1];
    }
}

void _remove_from_large_index(u8* pSortedIndex, LargePageHeader* pPageHeader, u16 uPage, u16 uCount)
{
    u8** tAllStartAddressesSorted = reinterpret_cast<u8**>(pSortedIndex + OFFSET_INDEX_LARGE_ADDRESSES);
    u16* tAllCorrespondingPageIndices = reinterpret_cast<u16*>(pSortedIndex + OFFSET_INDEX_LARGE_PAGEINDEX);
    u8* pPageStart = pPageHeader->tNodes[0].pStart;
    u16 uIndex = _dichotomous_search_sorted_insertion_pos(tAllStartAddressesSorted, uCount, pPageStart);
    assert(uIndex < uCount);
    assert(tAllStartAddressesSorted[uIndex] == pPageStart);
    assert(tAllCorrespondingPageIndices[uIndex] == uPage);
    for (u16 uIndexPastThat = uIndex; uIndexPastThat < uCount-1; uIndexPastThat++) {
        tAllStartAddressesSorted[uIndexPastThat] = tAllStartAddressesSorted[uIndexPastThat+1];
        tAllCorrespondingPageIndices[uIndexPastThat] = tAllCorrespondingPageIndices[uIndexPastThat+1];
    }
}

// this will actually return memory from no-longer-in-use pages, directly to OS...
void reclaim_all_empty_pages_from_thread_unsafe_provider(LocLib_ThreadUnsafeMemChunkProvider* pProvider)
{
    HeaderInRootRegularChunk* pRoot = pProvider->pRoot;
    u8* pSortedIndex = pRoot->pSortedIndexEachIfMoreThan16;

    // actually unalloc regular pages with their chunk alloc bitfield to all 0
    u16 uCurrentCountRegular = pRoot->uRegularChunkPagesCount;
    MemChunkPage* pTableRegular = pRoot->tRegularPages;
    // we'll just iterate here in all cases and not bother about bitfields,
    //   which would maybe be slightly helpful but not best suited to indicate full 0s anyways
    for (u16 uCount = 0; uCount < uCurrentCountRegular; uCount++) {
        while (pTableRegular[uCount].uAllocated == 0) {
            pProvider->pOsFuncs->pPageFreeFn(pTableRegular[uCount].pPageMemoryStart);
            if (pSortedIndex)
                _remove_from_regular_index(pSortedIndex, pTableRegular[uCount].pPageMemoryStart, uCount, uCurrentCountRegular);
            uCurrentCountRegular -= 1;
            // swap-with-last-remove
            if (uCount < uCurrentCountRegular) {
                pTableRegular[uCount].pPageMemoryStart = pTableRegular[uCurrentCountRegular].pPageMemoryStart;
                pTableRegular[uCount].uAllocated = pTableRegular[uCurrentCountRegular].uAllocated;
                // ... and retries same 'count' with the 'while' iteration
            } else
                break; // otherwise stop for this 'count'
        }
    }
    pProvider->pRoot->uRegularChunkPagesCount = uCurrentCountRegular;

    // actually unalloc large pages with their first, "free space" node spanning the entire large page size
    u16 uCurrentCountLarge = pRoot->uLargePagesCount;
    LargePageHeader** pTableLarge = pRoot->tLargePages;
    // we'll just iterate here in all cases and not bother about bitfields,
    //   which would maybe be slightly helpful but not best suited to indicate full availability anyways
    for (u16 uCount = 0; uCount < uCurrentCountLarge; uCount++) {
        LargeAllocNode* freeSpaceNodeThere = pTableLarge[uCount]->tNodes;
        while (freeSpaceNodeThere->pEnd == freeSpaceNodeThere->pStart + LARGE_PAGE_SIZE) {
            pProvider->pOsFuncs->pPageFreeFn(reinterpret_cast<u8*>(pTableLarge[uCount]));
            if (pSortedIndex)
                _remove_from_large_index(pSortedIndex, pTableLarge[uCount], uCount, uCurrentCountLarge);
            uCurrentCountLarge -= 1;
            // swap-with-last-remove
            if (uCount < uCurrentCountLarge) {
                pTableLarge[uCount] = pTableLarge[uCurrentCountLarge];
                // ... and retries same 'count' with the 'while' iteration
                freeSpaceNodeThere = pTableLarge[uCount]->tNodes;
            } else
                break; // otherwise stop for this 'count'
        }
    }
    pProvider->pRoot->uLargePagesCount = uCurrentCountLarge;

    // Note: NOOP on huge pages which were directly unallocated when released 

    // If an index is allocated, check if counts of regular, large and huge pages are all at most 16.
    //   If so, also release the index chunk... (and check & unalloc regular page holding it if need be...)
    if (pSortedIndex &&
        uCurrentCountRegular <= 16 && uCurrentCountLarge <= 16 && pRoot->uHugePagesCount <= 16)
    {
        release_regular_chunk_from_thread_unsafe_provider(pSortedIndex, pProvider);
        for (u16 uCount = 0; uCount < uCurrentCountRegular; uCount++) {
            if (pTableRegular[uCount].uAllocated == 0) {
                pProvider->pOsFuncs->pPageFreeFn(pTableRegular[uCount].pPageMemoryStart);
                uCurrentCountRegular -= 1;
                // swap-with-last-remove
                if (uCount < uCurrentCountRegular) {
                    pTableRegular[uCount].pPageMemoryStart = pTableRegular[uCurrentCountRegular].pPageMemoryStart;
                    pTableRegular[uCount].uAllocated = pTableRegular[uCurrentCountRegular].uAllocated;
                }
                pProvider->pRoot->uRegularChunkPagesCount = uCurrentCountRegular;
                break;
            }
        }

    }
}

#endif

