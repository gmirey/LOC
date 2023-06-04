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

    // Currently not used
    //TmpMap<int, TmpMap<u32, ReferencedNamespace*>> mapReferencedNamespaces; // All namespaces fom *other sources*

    // Note: idea of disallowance of global shadowing from local values currently put aside...
    // to reimplement: either uncomment this one, or the more direct set in tcnamespace
    //TmpMap<u64, TmpSet<int>> mapSetLocalUnshadowingByNamespaceUID;    // These are the sets of all local identifiers which shall not shadow a name within a namespace, by namespace uid
    
    StableGrowingVector<TCNamespace*> vecNamespaces;          // All namespaces *actually* defined in this file (not just referenced)
    TCNamespace* pRootNamespace;                              // always points to pos 0 of the above vec, but repeated here for fast access.

    TmpMap<u64, const TypeInfo*> mapRuntimeTypeIdToTCTypeInfo;
    StableGrowingVector<ValueBinding*> vecAllGlobalBindings;
    StableGrowingVector<TCProcBodyRegistration*> vecAllProcBodies;
    StableGrowingVector<TCCompoundRegistration*> vecAllCompoundDef;
    TmpArray<ForeignSourceDecl> vecForeignSources;

    TmpArray<u32> vecProcsIndicesDeclaredAsForeign;

    // TODO: locks once identifiers can be found in another context
    TmpArray<TCContext*> tvecTCTasksToLaunchByPrio[ETaskPriority::E_COUNT_TASKPRIO];
    TmpMap<TCWaitingReason, TmpArray<TCContext*>> mapNonIdWaitingTasksByReason; // TODO: multimap!!!

    TmpMap<const TypeInfo*, TypeInfo_Pointer*> mapAllPointersByToType;
    TmpMap<const TypeInfo*, TmpMap<u32, TypeInfo_Array*> > mapAllArraysByToType;

    void init_and_register_namespace(TCNamespace* ioToInit, SourceFileDescAndState* pOriginSourceFile, TCNamespace* pParent) {
        
        ioToInit->pParent = pParent;
        ioToInit->pOriginalSourceFile = pOriginSourceFile;
        
        ioToInit->uRegistrationIndex = pOriginSourceFile->vecNamespaces.size();
        pOriginSourceFile->vecNamespaces.append(ioToInit);
        
        Arena localArena = pOriginSourceFile->localArena;
        FireAndForgetArenaAlloc sfsFFAlloc(localArena);

        ioToInit->setLocalUnshadowing.init(sfsFFAlloc);

        ioToInit->mapAccessibleDeclarationsById.init(sfsFFAlloc);
        ioToInit->mapAllGlobalDeclarationsById.init(sfsFFAlloc);

        ioToInit->vecAccessibleUsedNamespaces.init(localArena);
        ioToInit->vecAllUsedNamespaces.init(localArena);

        ioToInit->vecAccessibleUsedEnums.init(localArena);
        ioToInit->vecAllUsedEnums.init(localArena);

        ioToInit->vecChildrenNamespaces.init(localArena);
        ioToInit->vecAllNamespacesInSameFileUsingSelf.init(localArena);

        ioToInit->mapAccessibleBindingsInclUsing.init(sfsFFAlloc);
        ioToInit->mapAllBindingsInclUsing.init(sfsFFAlloc);

        ioToInit->mapTasksWaitingForGlobalIds.init(sfsFFAlloc);

        ioToInit->uCountGlobalAccessibleTasks = 0u;
        ioToInit->uCountGlobalPrivateTasks = 0u;

        ioToInit->vecTasksWaitingForCompletion.init(localArena);

        ioToInit->iPrimaryIdentifier = -1;
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
        
        vecNamespaces.init(sourceFileSpecificArena);
        pRootNamespace = (TCNamespace*)alloc_from(sourceFileSpecificArena, sizeof(TCNamespace), alignof(TCNamespace));
        init_and_register_namespace(pRootNamespace, this, 0);

        for (ETaskPriority ePrio = ETaskPriority::ETASKPRIO_HIGHEST; ePrio <= ETaskPriority::ETASKPRIO_LOWEST; ePrio = ETaskPriority(ePrio + 1u)) {
            tvecTCTasksToLaunchByPrio[ePrio].init(sfsFFAlloc);
        }
        mapNonIdWaitingTasksByReason.init(sfsFFAlloc);

        mapAllPointersByToType.init(sfsFFAlloc);
        mapAllArraysByToType.init(sfsFFAlloc);
        mapRuntimeTypeIdToTCTypeInfo.init(sfsFFAlloc);
	}

};

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
