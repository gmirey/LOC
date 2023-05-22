// Part of LocLang/Compiler
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

#ifndef LOCLIB_SOURCE_FILE_DESC_AND_STATE_H_
#define LOCLIB_SOURCE_FILE_DESC_AND_STATE_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/HashSets.h"
#include "../../HighPerfTools/Strings.h"
#include "LocLib_Cmd_API.h"
#include "LocLib_TokenizerEnums.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_PostParserTypes.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_Namespace.h"

struct CompTimeNatural {
    u16 uLegsCount;
    u16 tLegs[0];
};

enum ETaskWaitingReason {
    EWR_GLOBAL_IDENTIFIER_RESOLUTION,       // waiting for a global identifier to be found somewhere
    EWR_PROC_BODY_TYPECHECK_SUCCESS,        // waiting for a proc body to successfully typecheck
    EWR_COMPOUND_BODY_TYPECHECK_SUCCESS,    // waiting for a compound type body to successfully typecheck
    EWR_OVERLOAD_GATHER_MORE,               // waiting for an overloaded-proclike identifier to gather more results
    EWR_BINDING_TYPE_FINALIZATION,          // waiting for a global identifier binding typing to be finalized
    EWR_BINDING_CONST_VALUE_FINALIZATION,   // waiting for a global identifier const binding to be evaluated
};

struct TCWaitingReason {

    DECL_TRIVIAL_STRUCT_OPS(TCWaitingReason);
    FORCE_INLINE bool operator==(const TCWaitingReason& other) const { return other._packed == _packed; }

    u64 _packed;
    FORCE_INLINE ETaskWaitingReason getWaitingReason() const { return ETaskWaitingReason(u8(_packed) & 0x0Fu); } // 4b
    FORCE_INLINE int getSourceFileIndex() const { return int(_packed >> 4) & 0x0FFFFFFFu; } // 28b
    FORCE_INLINE u32 getAwaitedId() const { return u32(_packed >> 32); } // 32b : identifier or procbody or ...
};

#define WAITING_REASON_NO_SPECIFIC_INDEX  (-5)

local_func_inl TCWaitingReason make_waiting_reason(ETaskWaitingReason eReason, int iSourceFileIndex, u32 uAwaitedId) {
    TCWaitingReason result;
    result._packed = u64(eReason) | (u64(iSourceFileIndex) << 4) | (u64(uAwaitedId) << 32);
    return result;
}

struct ForeignSourceDecl {
    StringView staticLibName;
    StringView dynamicLibName;
};

struct SourceFileDescAndState {
	
    FFString sourceFileName;    // allocated from pScratchMemForFinalAst. also stores length as u16 at ptr-2

    Arena parseOnlyArena;       // ensured single-accessed at all times by virtue of hard requirement of only at most one parsing task in flight

    AstBlock* tBlocks;          // allocated from parseOnlyArena
    i32 iBlockCount;            // -1 when not init
    int iRegistrationIndex;
    int iLoadedFromFileIndex;
    int iLoadedFromLineIndex;
    
    TCBaseSourceBlock* pRootTCBlock;

    u32 uNextVirtualBlock;
    u32 uIdentifierNamesOffset;                               // setup at sourcefile-parsing startup
    TmpArray<FFString> locallyFoundIdentifierNamesByOffsetId; // filled up during parsing, pushed in one go to global after parsing
    WholeProgramCompilationState* programState;               // holding mapAllIdentifierIdsByName and final vecAllIdentifierNamesById

    // arena specific to *this* source-file
    //   (ensured single-accessed at all times by virtue of hard requirement of only at most one typechecking task in flight per source file)
    Arena localArena;

    Arena preparsingArenaKeepingErrors; // TODO: CLEANUP: could be organied such as we use the single localArena instead...

    //
    // The following containers are allocated from localArena
    //

    TmpArray<LocLib_Error> vecErrors;                         // All errors emitted from parsing or typechecking from this source
    u32 uWarningCount;                                        // TODO: replace that with an array of warnings

    IRRepo filewiseConstRepo;
    IRRepo filewiseGlobalVarRepo;
    TmpMap<int, TmpMap<u32, ReferencedNamespace*>> mapReferencedNamespaces; // All namespaces fom *other sources*
    TmpMap<u64, TmpSet<int>> mapSetLocalUnshadowingByNamespaceUID;    // These are the sets of all local identifiers which shall not shadow a name within a namespace, by namespace uid
    StableGrowingVector<TCNamespace*> vecNamespaces;          // All namespaces *actually* defined in this file (not just referenced)
    TCNamespace* pRootNamespace;                              // always points to pos 0 of the above vec, but repeated here for fast access.

    TmpMap<u64, const TypeInfo*> mapRuntimeTypeIdToTCTypeInfo;
    StableGrowingVector<ValueBinding*> vecAllGlobalBindings;
    StableGrowingVector<TCProcBodyRegistration*> vecAllProcBodies;
    StableGrowingVector<TCCompoundRegistration*> vecAllCompoundDef;
    TmpArray<ForeignSourceDecl> vecForeignSources;

    TmpArray<u32> vecProcsIndicesDeclaredAsForeign;

    // TODO: locks once identifiers can be found in another context
    TmpArray<TCContext*> vecTCTasksToLaunch;
    TmpMap<TCWaitingReason, TmpArray<TCContext*>> mapWaitingTasksByReason; // TODO: multimap!!!

    TmpMap<const TypeInfo*, TypeInfo_Pointer*> mapAllPointersByToType;
    TmpMap<const TypeInfo*, TmpMap<u32, TypeInfo_Array*> > mapAllArraysByToType;

    void init_and_register_namespace(TCNamespace* ioToInit, SourceFileDescAndState* pOriginSourceFile, TCNamespace* pParent) {
        
        ioToInit->pParent = pParent;
        ioToInit->pOriginalSourceFile = pOriginSourceFile;
        
        ioToInit->uRegistrationIndex = pOriginSourceFile->vecNamespaces.size();
        pOriginSourceFile->vecNamespaces.append(ioToInit);
        ioToInit->asRef.pOrigNamespace = ioToInit;
        
        Arena localArena = pOriginSourceFile->localArena;
        FireAndForgetArenaAlloc sfsFFAlloc(localArena);
        
        ioToInit->asRef.laggedState.uDiscoveryProgress = ESOURCE_COMP_STATE_NOT_STARTED;
        ioToInit->asRef.laggedState.uVersion = 0u;
        //ioToInit->asRef.laggedState.mapKnownReferencedNamespaces.init(sfsFFAlloc);
        ioToInit->asRef.laggedState.mapKnownPublicDeclarationsById.init(sfsFFAlloc);
        ioToInit->asRef.laggedState.mapKnownAccessibleDeclarationsById.init(sfsFFAlloc);
        ioToInit->asRef.laggedState.vecKnownUsedNamespaces.init(localArena);
        ioToInit->asRef.laggedState.vecKnownUsedEnums.init(localArena);
        ioToInit->asRef.laggedState.mapKnownOthersUsingThis.init(sfsFFAlloc);

        //ioToInit->setLocalUnshadowing.init(sfsFFAlloc);
        //ioToInit->mapReferencedNamespaces.init(sfsFFAlloc);
        ioToInit->mapPublicDeclarationsById.init(sfsFFAlloc);
        ioToInit->mapAccessibleDeclarationsById.init(sfsFFAlloc);
        ioToInit->mapAllGlobalDeclarationsById.init(sfsFFAlloc);
        ioToInit->vecUsedNamespaces.init(localArena);
        ioToInit->vecUsedEnums.init(localArena);
        ioToInit->setOfNewlyDeclaredGlobalIdentifiers.init(sfsFFAlloc);
        ioToInit->mapOthersUsingThis.init(sfsFFAlloc);
        ioToInit->uCountGlobalTasksInTasksToLaunch = 0u;
        ioToInit->uCountGlobalTasksInWaitingTasks = 0u;
    }

	void init(int iIndex, int iLoadedFromFile, int iLoadedFromLine, Arena arenaForParseOnly, Arena sourceFileSpecificArena,
              FFString fileName, WholeProgramCompilationState* pProgState)
    {
        iRegistrationIndex = iIndex;
        iLoadedFromFileIndex = iLoadedFromFile;
        iLoadedFromLineIndex = iLoadedFromLine;
        sourceFileName = fileName;

        parseOnlyArena = arenaForParseOnly;
        uNextVirtualBlock = 0;
        uIdentifierNamesOffset = 0;                                                             // will be setup at sourcefile-parsing startup
        locallyFoundIdentifierNamesByOffsetId.init(FireAndForgetArenaAlloc(arenaForParseOnly)); // filled up during parsing, pushed in one go to global after parsing
        iBlockCount = -1;
        tBlocks = 0;
        pRootTCBlock = 0;
        programState = pProgState;
        
        localArena = sourceFileSpecificArena;
        FireAndForgetArenaAlloc sfsFFAlloc(sourceFileSpecificArena);
        preparsingArenaKeepingErrors = Arena{};
        vecErrors.init(sfsFFAlloc);
        uWarningCount = 0;

        Assert_(iIndex >= 0 && iIndex < IR_MAX_FILE_COUNT);
        init_ir_repo(&filewiseConstRepo, u16(u32(iIndex) + IR_REPO_ID_FIRST_FILE), localArena);
        init_ir_repo(&filewiseGlobalVarRepo, u16(u32(iIndex) + IR_REPO_ID_FIRST_FILE), localArena);

        vecAllGlobalBindings.init(sourceFileSpecificArena);
        vecAllProcBodies.init(sourceFileSpecificArena);
        vecAllCompoundDef.init(sourceFileSpecificArena);
        vecForeignSources.init(sourceFileSpecificArena);
        vecProcsIndicesDeclaredAsForeign.init(sourceFileSpecificArena);
        //setOtherFilesWithTasksWaitingForMe.init(sfsFFAlloc);
        //mapReferencedNamespaces.init(sfsFFAlloc);
        mapSetLocalUnshadowingByNamespaceUID.init(sfsFFAlloc);
        vecNamespaces.init(sourceFileSpecificArena);
        pRootNamespace = (TCNamespace*)alloc_from(sourceFileSpecificArena, sizeof(TCNamespace), alignof(TCNamespace));
        init_and_register_namespace(pRootNamespace, this, 0);

        vecTCTasksToLaunch.init(sfsFFAlloc);
        mapWaitingTasksByReason.init(sfsFFAlloc);

        mapAllPointersByToType.init(sfsFFAlloc);
        mapAllArraysByToType.init(sfsFFAlloc);
        mapRuntimeTypeIdToTCTypeInfo.init(sfsFFAlloc);
	}

};

// Note: blocking call
local_func_inl void acquire_type_def_lock(SourceFileDescAndState* pSourceFile, CompilationContext* pEvalContext) {
    // TODO!!!
}

local_func_inl void release_type_def_lock(SourceFileDescAndState* pSourceFile, CompilationContext* pEvalContext) {
    // TODO!!!
}


// Note: blocking call
local_func_inl void acquire_write_laggued_state_lock(SourceFileDescAndState* pSourceFile, CompilationContext* pEvalContext) {
    // TODO!!!
}

local_func_inl void release_write_laggued_state_lock(SourceFileDescAndState* pSourceFile, CompilationContext* pEvalContext) {
    // TODO!!!
}

// Note: blocking call
local_func_inl void acquire_read_laggued_state_lock(SourceFileDescAndState* pSourceFile, CompilationContext* pEvalContext) {
    // TODO!!!
}

local_func_inl void release_read_laggued_state_lock(SourceFileDescAndState* pSourceFile, CompilationContext* pEvalContext) {
    // TODO!!!
}

// Note: blocking call
local_func_inl void acquire_source_file_specific_task_lock(SourceFileDescAndState* pSourceFile, WorkerDesc* pWorker) {
    // TODO!!!
}

local_func_inl void release_source_file_specific_task_lock(SourceFileDescAndState* pSourceFile, WorkerDesc* pWorker) {
    // TODO!!!
}


local_func void write_namespace_to_self_lagged_state_while_under_lock(TCNamespace* pNamespace, bool bWriteGlobals, bool bWriteRefs, Arena laggedArena)
{
    // Note: All the following "copying" of current state to lagged state containers use *re-init* (and append all) instead of a possibly less
    //   wasteful *clear* (and append all), or even an update of just-the-new data. This way of doing things however is preserved, for the
    //   time being, since it could allows us (even if we do not do that, in the current version) to shallow-copy those laggued states to
    //   anywhere listening to them (only passing the implementation "pointers" of the resulting laggued state) with confidence that they became
    //   essentially immutable data (and since we're using fire and forget alloc with arenas, this holds true as long as we do not reset
    //   those arenas) => very cheap copy indeed for a potential 'listener', even if maybe vastly more wasteful to the laggued state now ?
    //   (TODO: CLEANUP: profile this)
    //
    // Note : if we're prepared to be *that* wasteful... we could also simply devise an atomic-ptr to current lagged state, with fencing.
    // This could peharps allow us to in fact be *less* wasteful : we could have an atomic counter of current accessors, and purely release
    //   the whole arenas containing such states.
    // => TODO!!!
    pNamespace->asRef.laggedState.uDiscoveryProgress = pNamespace->eCompState;
    pNamespace->asRef.laggedState.uVersion += 1u;
    if (bWriteGlobals) {
        Assert_(pNamespace->mapPublicDeclarationsById.size() >= pNamespace->asRef.laggedState.mapKnownPublicDeclarationsById.size());
        if (pNamespace->mapPublicDeclarationsById.size() > pNamespace->asRef.laggedState.mapKnownPublicDeclarationsById.size()) {
            pNamespace->asRef.laggedState.mapKnownPublicDeclarationsById.init(laggedArena,
                pNamespace->mapPublicDeclarationsById);
        }
        Assert_(pNamespace->mapAccessibleDeclarationsById.size() >= pNamespace->asRef.laggedState.mapKnownAccessibleDeclarationsById.size());
        if (pNamespace->mapAccessibleDeclarationsById.size() > pNamespace->asRef.laggedState.mapKnownAccessibleDeclarationsById.size()) {
            pNamespace->asRef.laggedState.mapKnownAccessibleDeclarationsById.init(laggedArena,
                pNamespace->mapAccessibleDeclarationsById);
        }
    } else {
        Assert_(pNamespace->mapPublicDeclarationsById.size() == pNamespace->asRef.laggedState.mapKnownPublicDeclarationsById.size());
        Assert_(pNamespace->mapAccessibleDeclarationsById.size() == pNamespace->asRef.laggedState.mapKnownAccessibleDeclarationsById.size());
    }

    if (bWriteRefs) {
        Assert_(pNamespace->vecUsedNamespaces.size() >= pNamespace->asRef.laggedState.vecKnownUsedNamespaces.size());
        if (pNamespace->vecUsedNamespaces.size() > pNamespace->asRef.laggedState.vecKnownUsedNamespaces.size()) {
            pNamespace->asRef.laggedState.vecKnownUsedNamespaces.init(laggedArena);
            pNamespace->asRef.laggedState.vecKnownUsedNamespaces.append_all(pNamespace->vecUsedNamespaces);
            /*
            u32 uCount = pNamespace->vecUsedNamespaces.size();
            pNamespace->asRef.laggedState.vecKnownUsedNamespaces.reserve(uCount);
            for (u32 uIndex = 0u; uIndex < uCount; uIndex++) {
                u64 uNamespaceUID = get_namespace_id(pNamespace->vecUsedNamespaces[uIndex]);
                pNamespace->asRef.laggedState.vecKnownUsedNamespaces.append(uNamespaceUID);
            }
            */
        }
        Assert_(pNamespace->vecUsedEnums.size() >= pNamespace->asRef.laggedState.vecKnownUsedEnums.size());
        if (pNamespace->vecUsedEnums.size() > pNamespace->asRef.laggedState.vecKnownUsedEnums.size()) {
            pNamespace->asRef.laggedState.vecKnownUsedEnums.init(laggedArena);
            pNamespace->asRef.laggedState.vecKnownUsedEnums.append_all(pNamespace->vecUsedEnums);
        }
        Assert_(pNamespace->mapOthersUsingThis.size() >= pNamespace->asRef.laggedState.mapKnownOthersUsingThis.size());
        if (pNamespace->mapOthersUsingThis.size() > pNamespace->asRef.laggedState.mapKnownOthersUsingThis.size()) {
            pNamespace->asRef.laggedState.mapKnownOthersUsingThis.init(laggedArena,
                pNamespace->mapOthersUsingThis);
        }
    } else {
        //Assert_(pNamespace->mapReferencedNamespaces.size() == pNamespace->asRef.laggedState.mapKnownReferencedNamespaces.size());
        Assert_(pNamespace->vecUsedNamespaces.size() == pNamespace->asRef.laggedState.vecKnownUsedNamespaces.size());
        Assert_(pNamespace->vecUsedEnums.size() == pNamespace->asRef.laggedState.vecKnownUsedEnums.size());
        Assert_(pNamespace->mapOthersUsingThis.size() == pNamespace->asRef.laggedState.mapKnownOthersUsingThis.size());
    }
}

#if 0

local_func TmpMap<u32, ReferencedNamespace*>& get_or_insert_map_of_referenced_namespaces(SourceFileDescAndState* pReferencingSourceFile, int iReferencedSourceFile)
{
    auto itFound = pReferencingSourceFile->mapReferencedNamespaces.find(iReferencedSourceFile);
    if (itFound == pReferencingSourceFile->mapReferencedNamespaces.end()) {
        TmpMap<u32, ReferencedNamespace*> newMapForThisFile;
        newMapForThisFile.init(pReferencingSourceFile->localArena);
        itFound = pReferencingSourceFile->mapReferencedNamespaces.insert_not_present(
            get_map_hash(iReferencedSourceFile), iReferencedSourceFile, newMapForThisFile);
    }
    return itFound.value();
}

local_func ReferencedNamespace* get_or_insert_referenced_namespace(SourceFileDescAndState* pReferencingSourceFile,
    int iReferencedSourceFile, u32 uNamespaceIndex, CompilationContext* pContext, bool bHasLockLaggedStateForReferencedFile)
{
    if (pReferencingSourceFile->iRegistrationIndex == iReferencedSourceFile) {
        return (ReferencedNamespace*)pReferencingSourceFile->vecNamespaces[uNamespaceIndex];
    } else {
        TmpMap<u32, ReferencedNamespace*>& mapNamespaces = get_or_insert_map_of_referenced_namespaces(pReferencingSourceFile, iReferencedSourceFile);
        auto itFound = mapNamespaces.find(uNamespaceIndex);
        if (itFound == mapNamespaces.end()) {
            ReferencedNamespace* pNewRef = (ReferencedNamespace*)alloc_from(pReferencingSourceFile->localArena,
                sizeof(ReferencedNamespace), alignof(ReferencedNamespace));
            SourceFileDescAndState* pReferencedFile = pContext->pProgCompilationState->vecSourceFiles[u32(iReferencedSourceFile)];
            pNewRef->pOrigNamespace = pReferencedFile->vecNamespaces[uNamespaceIndex];

            if (!bHasLockLaggedStateForReferencedFile)
                acquire_read_laggued_state_lock(pReferencedFile, pContext);
            
            // Note this "copy" is not deep at all, but we're ensured that the containers from the lagged states will *not* change same allocs.
            pNewRef->laggedState = pNewRef->pOrigNamespace->asRef.laggedState;
            
            if (!bHasLockLaggedStateForReferencedFile)
                release_read_laggued_state_lock(pReferencedFile, pContext);
        }
        return itFound.value();
    }
}

#endif

#if 0
local_func void copy_namespace_lagged_state_to_ref_while_src_under_lock(SourceFileDescAndState* pSourceFile,
    ReferencedNamespace* pRefNamespace, Arena laggedArena, TmpArray<TmpMap<u64, ReferencedNamespace*>*>* outVecMapRefsToReevaluate)
{
    LaggedNamespaceState& laggedSrc = pRefNamespace->pOrigNamespace->asRef.laggedState;
    LaggedNamespaceState& laggedDst = pRefNamespace->laggedState;
    if (laggedDst.uDiscoveryProgress != laggedSrc.uDiscoveryProgress || laggedDst.uVersion != laggedSrc.uVersion) {
        Assert_(laggedDst.uDiscoveryProgress >= laggedSrc.uDiscoveryProgress);
        Assert_(laggedDst.uVersion > laggedSrc.uVersion);
        Assert_(laggedSrc.mapKnownPublicDeclarationsById.size() >= laggedDst.mapKnownPublicDeclarationsById.size());
        if (laggedSrc.mapKnownPublicDeclarationsById.size() > laggedDst.mapKnownPublicDeclarationsById.size()) {
            laggedDst.mapKnownPublicDeclarationsById.init(laggedArena,
                laggedSrc.mapKnownPublicDeclarationsById);
        }
        Assert_(laggedSrc.mapKnownAccessibleDeclarationsById.size() >= laggedDst.mapKnownAccessibleDeclarationsById.size());
        if (laggedSrc.mapKnownAccessibleDeclarationsById.size() > laggedDst.mapKnownAccessibleDeclarationsById.size()) {
            laggedDst.mapKnownAccessibleDeclarationsById.init(laggedArena,
                laggedSrc.mapKnownAccessibleDeclarationsById);
        }
        Assert_(laggedSrc.mapKnownReferencedNamespaces.size() >= laggedDst.mapKnownReferencedNamespaces.size());
        if (laggedSrc.mapKnownReferencedNamespaces.size() > laggedDst.mapKnownReferencedNamespaces.size()) {
            laggedDst.mapKnownAccessibleDeclarationsById.init(laggedArena,
                laggedSrc.mapKnownAccessibleDeclarationsById);
            outVecMapRefsToReevaluate->append(&(laggedDst.mapKnownAccessibleDeclarationsById));
        }
        Assert_(laggedSrc.vecKnownUsedNamespaces.size() >= laggedDst.vecKnownUsedNamespaces.size());
        if (laggedSrc.vecKnownUsedNamespaces.size() > laggedDst.vecKnownUsedNamespaces.size()) {
            laggedDst.vecKnownUsedNamespaces.init(laggedArena,
                laggedSrc.vecKnownUsedNamespaces);
        }
        Assert_(laggedSrc.vecKnownUsedEnums.size() >= laggedDst.vecKnownUsedEnums.size());
        if (laggedSrc.vecKnownUsedEnums.size() > laggedDst.vecKnownUsedEnums.size()) {
            laggedDst.vecKnownUsedEnums.init(laggedArena,
                laggedSrc.vecKnownUsedEnums);
        }
    }
}
#endif


exported_func_impl const char* get_filename_from_filedesc(SourceFileDescAndState* pSourceFileDesc)
{
    return pSourceFileDesc->sourceFileName.c_str();
}

exported_func_impl const LocLib_Error* get_errors_on_sourcefile(SourceFileDescAndState* pSourceFileDesc, u64* outErrCount)
{
    *outErrCount = u64(pSourceFileDesc->vecErrors.size());
    return pSourceFileDesc->vecErrors.cbegin();
}

local_func u32 register_identifier_during_parsing(StringView strId, SourceFileDescAndState* pSourceFileDesc, u16* outErrCodeOpt = 0); // defined in LocLib_ProgramState

local_func void get_line_and_col_from(AstNode* pNode, AstStatement* pStatement, AstBlock* pBlock, SourceFileDescAndState* pSourceFileDesc,
    int* outByteOffsetOfLineStart, int* outLine, int* outCol)
{
    int iLineOfStatementFromFileStart = 0;
    int iByteOffsetOfStatementFromFileStart = 0;
    int iCol = 0;
    do {
        if (pStatement) {
            iLineOfStatementFromFileStart += int(pStatement->uFirstLineRelToLineOfParentStatementInParentBlock);
            iByteOffsetOfStatementFromFileStart += int(pStatement->uStartOfLineByteOffsetRelToStartOfParentStatement);
        }
        if (pBlock && pBlock > pSourceFileDesc->tBlocks) {
            u32 uIndexOfStatementInParent = pBlock->uIndexOfParentStatementInParentBlock;
            pBlock = pSourceFileDesc->tBlocks + pBlock->uParentBlock;
            pStatement = uIndexOfStatementInParent < pBlock->uStatementCount ? pBlock->tStatements + uIndexOfStatementInParent : 0;
        } else
            break;
    } while (true);
    if (pNode) {
        *outCol = int(pNode->uPivotalTokenPackedRef & 0x3FF);
        *outLine = iLineOfStatementFromFileStart + int((pNode->uPivotalTokenPackedRef) >> 10);
    } else {
        if (pStatement->uFlags & IN_ERROR_STATEMENT_MASK_EXCEPT_NOCHILD)
        *outCol = 0;
        *outLine = iLineOfStatementFromFileStart;
    }
    *outByteOffsetOfLineStart = iByteOffsetOfStatementFromFileStart;
}

exported_func_impl void get_non_scan_error_info(const SourceFileDescAndState* pSourceFile, const LocLib_Error* pError, LocLib_ErrorInfo* outInfo)
{
    Assert_(pSourceFile->iBlockCount > 0);
    Assert_(pError);
    Assert_(pError->errCode >= BASE_OF_TOKENIZER_ERRORS);
    u32 uLineOfStatementFromFileStart = 0;
    u32 uByteOffsetOfStatementFromFileStart = 0;
    i32 iBlock = i32(pError->uBlockOrLineIfScanErr);
    u32 uStatement = pError->uStatement;
    do {
        Assert_(iBlock < pSourceFile->iBlockCount);
        AstBlock* pBlock = pSourceFile->tBlocks + iBlock;
        if (uStatement < pBlock->uStatementCount) {
            AstStatement* pStatement = pBlock->tStatements + uStatement;
            uLineOfStatementFromFileStart += pStatement->uFirstLineRelToLineOfParentStatementInParentBlock;
            uByteOffsetOfStatementFromFileStart += pStatement->uStartOfLineByteOffsetRelToStartOfParentStatement;
        }
        if (pBlock > pSourceFile->tBlocks) {
            iBlock = i32(pBlock->uParentBlock);
            uStatement = pBlock->uIndexOfParentStatementInParentBlock;
        } else
            break;
    } while (true);

    outInfo->uLineOfStatementFromFileStart = uLineOfStatementFromFileStart;
    outInfo->uByteOffsetOfStatementFromFileStart = uByteOffsetOfStatementFromFileStart;
    outInfo->uLineOffsetOfTokenFromStatement = pError->uTokenRef >> 10;
    outInfo->uColumnOfTokenStart = pError->uTokenRef & 0x03FF;
}


/*
local_fund u32 add_global_scope_variable_slot(int iIdHandle, u32 uBindingBlock, u32 uStatementIndexInBlock, EScopeKind eScopeKind, SourceFileDescAndState* pSourceFileDesc)
{
    u32 uAvailableSlot = pSourceFileDesc->vecGlobalVar.size();
    Assert_(uAvailableSlot <= 0x007FFFFFu); // TODO: replace with error
    u64 uHash = get_map_hash(iIdHandle);
    u32 uSlotWithFlags = uAvailableSlot;
    switch (eScopeKind) {
        case EScopeKind::SCOPEKIND_GLOBAL_PUBLIC: {
            Assert_(pSourceFileDesc->mapGlobalPublicDeclarationsById.findHashed(uHash, iIdHandle) == pSourceFileDesc->mapGlobalPublicDeclarationsById.end());
            pSourceFileDesc->mapGlobalPublicDeclarationsById.insert_not_present(uHash, iIdHandle, uSlotWithFlags);
            uSlotWithFlags |= BINDINGID_LOCATION_KIND_PUBLIC;
        } break;
        case EScopeKind::SCOPEKIND_GLOBAL_PACKAGE: {
            Assert_(pSourceFileDesc->mapGlobalPackageDeclarationsById.findHashed(uHash, iIdHandle) == pSourceFileDesc->mapGlobalPackageDeclarationsById.end());
            pSourceFileDesc->mapGlobalPackageDeclarationsById.insert_not_present(uHash, iIdHandle, uSlotWithFlags);
            uSlotWithFlags |= BINDINGID_LOCATION_KIND_PACKAGE;
        } break;
        case EScopeKind::SCOPEKIND_GLOBAL_PRIVATE: {
            Assert_(pSourceFileDesc->mapGlobalPrivateDeclarationsById.findHashed(uHash, iIdHandle) == pSourceFileDesc->mapGlobalPrivateDeclarationsById.end());
            pSourceFileDesc->mapGlobalPrivateDeclarationsById.insert_not_present(uHash, iIdHandle, uSlotWithFlags);
            uSlotWithFlags |= BINDINGID_LOCATION_KIND_PRIVATE;
        } break;
        default:
            Assume_(false);
    }
    GlobalVarIdentifierBinding newBinding;
    newBinding.iIdentifierHandle = iIdHandle;
    newBinding.uBindingBlock = uBindingBlock;
    newBinding.uBindingStatementIndexInBlock = uStatementIndexInBlock;
    newBinding.uInitialValueFlags = VALUE_FLAGS_UNSOLVED_BINDING;
    newBinding.pType = 0;
    newBinding.uInitialRawValue = 0;
    pSourceFileDesc->vecGlobalVar.append(newBinding);
    return uSlotWithFlags;
};

local_fund u32 add_global_scope_constant_slot(int iIdHandle, u32 uBindingAstBlock, u32 uStatementAstIndexInBlock, EScopeKind eScopeKind, SourceFileDescAndState* pSourceFileDesc)
{
    u32 uAvailableSlot = pSourceFileDesc->vecGlobalConst.size();
    Assert_(uAvailableSlot <= 0x007FFFFFu); // TODO: replace with error
    u64 uHash = get_map_hash(iIdHandle);
    u32 uSlotWithFlags = uAvailableSlot | BINDINGID_FLAG_BINDING_IS_CONSTANT;
    switch (eScopeKind) {
        case EScopeKind::SCOPEKIND_GLOBAL_PUBLIC: {
            Assert_(pSourceFileDesc->mapGlobalPublicDeclarationsById.findHashed(uHash, iIdHandle) == pSourceFileDesc->mapGlobalPublicDeclarationsById.end());
            pSourceFileDesc->mapGlobalPublicDeclarationsById.insert_not_present(uHash, iIdHandle, uSlotWithFlags);
            uSlotWithFlags |= BINDINGID_LOCATION_KIND_PUBLIC;
        } break;
        case EScopeKind::SCOPEKIND_GLOBAL_PACKAGE: {
            Assert_(pSourceFileDesc->mapGlobalPackageDeclarationsById.findHashed(uHash, iIdHandle) == pSourceFileDesc->mapGlobalPackageDeclarationsById.end());
            pSourceFileDesc->mapGlobalPackageDeclarationsById.insert_not_present(uHash, iIdHandle, uSlotWithFlags);
            uSlotWithFlags |= BINDINGID_LOCATION_KIND_PACKAGE;
        } break;
        case EScopeKind::SCOPEKIND_GLOBAL_PRIVATE: {
            Assert_(pSourceFileDesc->mapGlobalPrivateDeclarationsById.findHashed(uHash, iIdHandle) == pSourceFileDesc->mapGlobalPrivateDeclarationsById.end());
            pSourceFileDesc->mapGlobalPrivateDeclarationsById.insert_not_present(uHash, iIdHandle, uSlotWithFlags);
            uSlotWithFlags |= BINDINGID_LOCATION_KIND_PRIVATE;
        } break;
        default:
            Assume_(false);
    }
    ConstIdentifierBinding newBinding;
    newBinding.iIdentifierHandle = iIdHandle;
    newBinding.uBindingAstBlock = uBindingAstBlock;
    newBinding.uBindingAstStatementIndexInBlock = uStatementAstIndexInBlock;
    // TODO: two-level Ast reference for macro expansions ?
    newBinding.uConstValueFlags = VALUE_FLAGS_UNSOLVED_BINDING;
    newBinding.pType = 0;
    newBinding.uCurrentBoundRawValue = 0;
    pSourceFileDesc->vecGlobalConst.append(newBinding);
    return uSlotWithFlags;
}

local_func bool check_no_global_scope_redefinition(SourceFileDescAndState* pSourceFileDesc, int iIdHandle)
{
    u64 uHash = get_map_hash(iIdHandle);
    auto itFound = pSourceFileDesc->mapAllGlobalDeclarationsById.findHashed(uHash, iIdHandle);
    return itFound == pSourceFileDesc->mapAllGlobalDeclarationsById.end();
}

local_func ValueBinding* try_get_global_binding(SourceFileDescAndState* pSourceFileDesc, int iIdHandle)
{
    u64 uHash = get_map_hash(iIdHandle);
    auto itFound = pSourceFileDesc->mapAllGlobalDeclarationsById.findHashed(uHash, iIdHandle);
    if (itFound != pSourceFileDesc->mapAllGlobalDeclarationsById.end()) {
        return itFound.value();
    }
    return 0;
}

*/

#endif // LOCLIB_SOURCE_FILE_DESC_AND_STATE_H_
