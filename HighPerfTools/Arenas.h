#pragma once

#include "BaseDecls.h"     // basic stuff for everything
#include "Platform.h"
#include "ChunkProvider.h"     // Chunk provider for arenas. TODO: replace with "ChunkProvider.h" when ready

#define ARENA_FLAG_ALLOWED_TO_GROW              0x10000000u     // indicates whether we can require more chunks
#define ARENA_FLAG_ALLOWED_TO_RESIZE_ON_GROW    0x20000000u     // indicates whether the additional chunks on growth can be of greater size than the first
struct ArenaChunkHeader {
    u32 used_byte_size;
    u32 reserved_byte_size;
    ChunkHandle next_chunk_handle;
    FORCE_INLINE ArenaChunkHeader* next_chunk() const { return (ArenaChunkHeader*)next_chunk_handle.get_ptr_to_data(); }
};
struct RootArenaChunkHeader : ArenaChunkHeader {
    ChunkHandle last_chunk_handle;
    FORCE_INLINE ArenaChunkHeader* last_chunk() const { return (ArenaChunkHeader*)last_chunk_handle.get_ptr_to_data(); }
    ChunkProvider chunk_provider;
    u32 uFlags;
};

// Arenas typically provide 'fire-and-forget' memory allocation schemes, where we simply ask for some amount of memory,
//   and we intend to use it without thinking about piecewise-releasing it afterwards.
// Of course, memory can still be freed at the application level, by asking that arena to 'reset' globally (reclaiming all memory),
//   or even asking it to reset 'to a previously recorded state', giving it some stack-like capability, able to grow and shrink sequentially, on demand.
// This arena implementation thus supports:
//   - Fixed-size arenas (ensured to perform no OS-alloc whatsoever once initialized, and will fail on OOM)
//   - Growing arenas (allowed to require more chunks from chunk provider)
struct Arena {
    ChunkHandle root_chunk_handle;
    FORCE_INLINE RootArenaChunkHeader* root_chunk() const { return (RootArenaChunkHeader*)root_chunk_handle.get_ptr_to_data(); }
};

#if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 0
#include <stdio.h>
#endif

// Creates and initializes an Arena, given a chunk provider
static void init_arena(Arena* outArena, ChunkProvider provider, u32 byte_size = CHUNK_PROVIDER_REGULAR_CHUNK_SIZE,
    u32 growth_flags = ARENA_FLAG_ALLOWED_TO_GROW | ARENA_FLAG_ALLOWED_TO_RESIZE_ON_GROW)
{
    ChunkHandle root_handle = provider.allocate_chunk(byte_size);
    #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 0
        char szBuffer[1024];
        sprintf(szBuffer, "initializing arena at root_handle = %llu (@0x%llx ; tag %u) - byte size %u",
            root_handle.uTagged, root_handle.uTagged & (~u64(CHUNK_HANDLE_TAG_MASK)),
            u32(root_handle.uTagged) & CHUNK_HANDLE_TAG_MASK, byte_size);
        platform_log_info(szBuffer, true);
    #endif
    RootArenaChunkHeader* root_header = (RootArenaChunkHeader*)root_handle.get_ptr_to_data();
    #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 3
        sprintf(szBuffer, "(root_header at 0x%llx)", (u64)root_header);
        platform_log_info(szBuffer, true);
    #endif
    root_header->used_byte_size = sizeof(RootArenaChunkHeader);
    root_header->reserved_byte_size = byte_size;
    root_header->next_chunk_handle.uTagged = 0;
    root_header->last_chunk_handle = root_handle;
    root_header->chunk_provider = provider;
    root_header->uFlags = growth_flags;
    outArena->root_chunk_handle = root_handle;
}

namespace ArenaImpl {

    static ChunkHandle _create_arena_chunk(ChunkProvider provider, u32 byte_size) {
        ChunkHandle handle = provider.allocate_chunk(byte_size);
        #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 1
            char szBuffer[1024];
            sprintf(szBuffer, "creating new arena chunk at chunk_handle = %llu (@0x%llx ; tag %u) - byte size %u",
                handle.uTagged, handle.uTagged & (~u64(CHUNK_HANDLE_TAG_MASK)),
                u32(handle.uTagged) & CHUNK_HANDLE_TAG_MASK, byte_size);
            platform_log_info(szBuffer, true);
        #endif
        ArenaChunkHeader* header = (ArenaChunkHeader*)handle.get_ptr_to_data();
        #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 3
            sprintf(szBuffer, "(header at 0x%llx)", (u64)header);
            platform_log_info(szBuffer, true);
        #endif
        header->used_byte_size = sizeof(ArenaChunkHeader);
        header->reserved_byte_size = byte_size;
        header->next_chunk_handle.uTagged = 0;
        return handle;
    }

    static void _release_arena_chunk(ChunkHandle chunk_handle, ChunkProvider provider)
    {
        ChunkHandle chunk_to_release = chunk_handle;
        while (chunk_to_release.uTagged) {
            #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 1
                char szBuffer[1024];
                sprintf(szBuffer, "releasing arena chunk at chunk_handle = %llu (@0x%llx ; tag %u)",
                    chunk_to_release.uTagged, chunk_to_release.uTagged & (~u64(CHUNK_HANDLE_TAG_MASK)),
                    u32(chunk_to_release.uTagged) & CHUNK_HANDLE_TAG_MASK);
                platform_log_info(szBuffer, true);
            #endif
            ArenaChunkHeader* header_of_to_release = (ArenaChunkHeader*)chunk_to_release.get_ptr_to_data();
            #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 3
                sprintf(szBuffer, "(header at 0x%llx, with reserved byte size = %u)", (u64)header_of_to_release, header_of_to_release->reserved_byte_size);
                platform_log_info(szBuffer, true);
            #endif
            ChunkHandle chunk_to_release_after = header_of_to_release->next_chunk_handle;
            provider.release_chunk(chunk_to_release, header_of_to_release->reserved_byte_size);
            chunk_to_release = chunk_to_release_after;
        }
    }

    static u8* _try_allocate_in_chunk(ArenaChunkHeader* current_chunk, u32 byte_size, u32 align_bytes)
    {
        // We compute position of result, taking alignment into account
        //
        UParam next_pos = UParam(current_chunk) + UParam(current_chunk->used_byte_size);
        u32 size_increase_from_align = 0;
        if (next_pos & (align_bytes - 1u)) { // If position at current size is not aligned, we've got a little more work to do (but not that much)
            UParam next_pos_aligned = (next_pos & ~UParam(align_bytes - 1u)) + align_bytes;
            Assume(0 == (next_pos_aligned & (align_bytes - 1u)), "expected valid arithmetic");
            UParam size_increase = next_pos_aligned - next_pos;
            Assume(does_fit_in_u32(size_increase + byte_size), "expected used size given alignement is u32");
            size_increase_from_align = u32(size_increase);
            next_pos = next_pos_aligned;
        }
        // then we prepare to simply return that now-aligned position as our result...
        u8* ptr = (u8*)next_pos;

        // But we check we do indeed have enough room in current chunk to fit that allocation
        //
        if (NOMINAL(ptr + byte_size <= (u8*)current_chunk + current_chunk->reserved_byte_size)) {
            // if so, we bump our usage byte count by that amount (and any padding required for alignment), and return that position.
            current_chunk->used_byte_size += byte_size + size_increase_from_align;
            return ptr;
        } else {
            // otherwise we fail that allocation attempt, returning 0
            return 0;
        }
    }

    u8* _try_allocate_in_new_chunk(Arena arena, u32 byte_size, u32 align_bytes)
    {
        RootArenaChunkHeader* root_header = arena.root_chunk();
        if (root_header->uFlags & ARENA_FLAG_ALLOWED_TO_GROW) {
            // NOOP : We may assume we're called only in case where we're allowed to grow...
        } else {
            return 0; // otherwise someone will have to do some error handling, there...
        }

        // If we're allowed to grow, we prepare to create a new chunk.
        ChunkHandle new_chunk_handle = {};
        // Nominally, allocs which are requested from arenas would be small, and smaller in particular than a chunk size... but let's check that anyway.
        if (LIKELY(sizeof(ArenaChunkHeader) + byte_size + align_bytes < root_header->reserved_byte_size))
            new_chunk_handle = _create_arena_chunk(root_header->chunk_provider, root_header->reserved_byte_size);
        else {
            // Here, requested alloc is potentially greater than the initial size for the root chunk... if we're NOT allowed to resize on grow, we fail...
            if (root_header->uFlags & ARENA_FLAG_ALLOWED_TO_RESIZE_ON_GROW) {
                // if we're allowed to resize on grow, we compute a new alloc size sufficient to hold that large alloc request.
                u32 byte_size_to_reserve = root_header->reserved_byte_size * 2u;
                while (sizeof(ArenaChunkHeader) + byte_size + align_bytes > byte_size_to_reserve) {
                    byte_size_to_reserve <<= 1;
                }
                // And we create a new chunk with those uncommon parameters...
                new_chunk_handle = _create_arena_chunk(root_header->chunk_provider, byte_size_to_reserve);
            } else {
                return 0; // if we're NOT allowed to resize on grow, we fail...
            }
        }
        Assume(new_chunk_handle.uTagged, "expected valid new chunk on this code path");

        // We link up the new chunk to the chunk chain. Both as 'next' in previous chunk and as 'current' in the root chunk.
        //
        root_header->last_chunk()->next_chunk_handle = new_chunk_handle;
        root_header->last_chunk_handle = new_chunk_handle;
        // And we simply ask that new chunk to perform the allocation.
        u8* ptr = _try_allocate_in_chunk((ArenaChunkHeader*)new_chunk_handle.get_ptr_to_data(), byte_size, align_bytes);
        Assert(ptr, "Allocation in purposefully created new chunk should succeed");
        return ptr;
    }

    static u8* _allocate_after_first_try(Arena arena, u32 byte_size, u32 align_bytes) {
        #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 1
            char szBuffer[1024];
            sprintf(szBuffer, "_allocate_after_first_try() for arena with root_handle = %llu (@0x%llx ; tag %u)",
                arena.root_chunk_handle.uTagged, arena.root_chunk_handle.uTagged & (~u64(CHUNK_HANDLE_TAG_MASK)),
                u32(arena.root_chunk_handle.uTagged) & CHUNK_HANDLE_TAG_MASK);
            platform_log_info(szBuffer, true);
        #endif
        // We're called when we do not have enough room in current chunk to comply with some allocation demand.
        u8* ptr = 0;
        // if someone called 'resetArenaNoRelease', we may have a non-null 'next_chunk' chain from 'current_chunk'
        ArenaChunkHeader* last_chunk = arena.root_chunk()->last_chunk();
        #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 1
            sprintf(szBuffer, "(last_chunk at 0x%llx, with reserved byte size = %u)", (u64)last_chunk, last_chunk->reserved_byte_size);
            platform_log_info(szBuffer, true);
        #endif
        if (last_chunk->next_chunk()) {
            Assert(last_chunk->next_chunk()->used_byte_size == sizeof(ArenaChunkHeader), "Expected any dangling next-chunk to be empty");
            // In which case, we can try to allocate from there without spawning another chunk...
            ptr = _try_allocate_in_chunk(last_chunk->next_chunk(), byte_size, align_bytes);
            if (NOMINAL(ptr)) { // we can reasonnably expect allocations from dangling next empty chunk to succeed.
                arena.root_chunk()->last_chunk_handle = last_chunk->next_chunk_handle;
                return ptr;
            } else { // However, it may be the case that we fail to allocate in an empty page.
                // It can happen if we'd need some larger chunk for that alloc, than current next chunk.
                // If so, we'll chose to force deallocation of the non-null 'next_chunk' chain, before falling back to the nominal path
                _release_arena_chunk(last_chunk->next_chunk_handle, arena.root_chunk()->chunk_provider);
            }
        }
        // Nominal path is to try to allocate a new chunk for that allocation
        ptr = _try_allocate_in_new_chunk(arena, byte_size, align_bytes);
        #if ASSERT_FALSE_ON_OOM
            // We did everything we could do, we can expect the allocation in new chunk to have been successful, or we assert false on OOM.
            Assert(ptr, "Arena OOM");
        #endif
        return ptr;
    }

    static void _reset_from(ArenaChunkHeader* arena_chunk) {
        while (arena_chunk) {
            arena_chunk->used_byte_size = sizeof(ArenaChunkHeader);
            arena_chunk = arena_chunk->next_chunk();
        }
    }

} // namespace ArenaImpl

// Main usage function for arenas : ask the arena to provide some amount of memory, possibly with some alignment requirement
static FORCE_INLINE u8* alloc_from(Arena arena, u32 byte_size, u16 align_bytes = 1u)
{
    Assert(align_bytes > 0 && isPow2_32b(align_bytes), "alignment shall be a non-zero power of two");
    Assert_(align_bytes <= 0x1000u);
    u8* ptr = ArenaImpl::_try_allocate_in_chunk(arena.root_chunk()->last_chunk(), byte_size, align_bytes);
    if (NOMINAL(ptr)) // we can reasonnably expect allocations from an arena to nominally succeed in current chunk.
        return ptr;
    else              // otherwise we'll try to allocate a new chunk (or maybe reuse some existing but hanging one after a resetNoRelease() call)
        return  ArenaImpl::_allocate_after_first_try(arena, byte_size, align_bytes);
}

// Fully releases all memory from an arena, making it unusable until another init_arena() call
static void release_arena(Arena* outArena)
{
    ChunkHandle root_chunk_handle = outArena->root_chunk_handle;
    #if DEBUG_ARENAS_ALLOC_PRINTLEVEL > 0
        char szBuffer[1024];
        sprintf(szBuffer, "releasing arena at root_handle = %llu (@0x%llx ; tag %u)",
            root_chunk_handle.uTagged, root_chunk_handle.uTagged & (~u64(CHUNK_HANDLE_TAG_MASK)),
            u32(root_chunk_handle.uTagged) & CHUNK_HANDLE_TAG_MASK);
        platform_log_info(szBuffer, true);
    #endif
    RootArenaChunkHeader* root_header = (RootArenaChunkHeader*)root_chunk_handle.get_ptr_to_data();
    if (root_header) {
        ArenaImpl::_release_arena_chunk(outArena->root_chunk_handle, root_header->chunk_provider);
        outArena->root_chunk_handle.uTagged = 0;
    }
}

// Resets the arena to its 'empty' state, like it was just after initialization,
//   also releasing any additional chunk after the first if it had grown to several.
static void reset_arena(Arena arena)
{
    ChunkHandle root_chunk_handle = arena.root_chunk_handle;
    RootArenaChunkHeader* root_header = (RootArenaChunkHeader*)root_chunk_handle.get_ptr_to_data();
    if (root_header) {
        ArenaImpl::_release_arena_chunk(root_header->next_chunk_handle, root_header->chunk_provider);
        root_header->used_byte_size = sizeof(RootArenaChunkHeader);
        root_header->next_chunk_handle.uTagged = 0;
        root_header->last_chunk_handle = root_chunk_handle;
    }
}

// Resets the arena to its 'empty' state, like it was just after initialization,
//   similar to 'reset_arena' but keeping any additional chunk after the first (if it had grown to several)
//   as dangling and immediately reusable, without releasing them.
static void reset_arena_no_release(Arena arena)
{
    ChunkHandle root_chunk_handle = arena.root_chunk_handle;
    RootArenaChunkHeader* root_header = (RootArenaChunkHeader*)root_chunk_handle.get_ptr_to_data();
    if (root_header) {
        ArenaImpl::_reset_from(root_header->next_chunk());
        root_header->used_byte_size = sizeof(RootArenaChunkHeader);
        root_header->last_chunk_handle = root_chunk_handle;
    }
}

// Stores information about the alloc-state of an arena, so that its actual size can be reverted to this very state.
//   In effect, this gives any Arena a 'stack-like' capability, able to grow and shrink sequentially, on demand.
//   @see 'get_arena_ref_point', 'resetArenaTo', 'resetArenaNoReleaseTo'
struct ArenaRefPoint {
    ChunkHandle current_chunk_handle;
    u32 used_byte_size;
    FORCE_INLINE ArenaChunkHeader* current_chunk() const { return (ArenaChunkHeader*)current_chunk_handle.get_ptr_to_data(); }
};

// Retrieves information about current state of the arena, so that its actual size can be reverted to this very state.
static FORCE_INLINE ArenaRefPoint get_arena_ref_point(Arena arena) {
    ArenaRefPoint ref_point;
    RootArenaChunkHeader* root_header = arena.root_chunk();
    Assert_(root_header);
    ref_point.current_chunk_handle = root_header->last_chunk_handle;
    ref_point.used_byte_size = root_header->used_byte_size;
    return ref_point;
}

// Resets an arena to a previously remembered alloc-state,
//   also releasing any additional chunk after the ref point if it had grown further.
static FORCE_INLINE void reset_arena_to(ArenaRefPoint ref_point, Arena arena)
{
    RootArenaChunkHeader* root_header = arena.root_chunk();
    Assert_(root_header);
    ArenaImpl::_release_arena_chunk(ref_point.current_chunk()->next_chunk_handle, root_header->chunk_provider);
    ref_point.current_chunk()->next_chunk_handle.uTagged = 0;
    ref_point.current_chunk()->used_byte_size = ref_point.used_byte_size;
    root_header->last_chunk_handle = ref_point.current_chunk_handle;
}

// Resets an arena to a previously remembered alloc-state,
//   similar to 'reset_arena_to' but keeping any additional chunk after the ref point (if it had grown further)
//   as dangling and immediately reusable, without releasing them.
static FORCE_INLINE void reset_arena_no_release_to(ArenaRefPoint ref_point, Arena arena) {
    RootArenaChunkHeader* root_header = arena.root_chunk();
    Assert_(root_header);
    ArenaImpl::_reset_from(ref_point.current_chunk()->next_chunk());
    ref_point.current_chunk()->used_byte_size = ref_point.used_byte_size;
    root_header->last_chunk_handle = ref_point.current_chunk_handle;
}
