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

#ifndef LOCLIB_PROGRAM_STATE_H_
#define LOCLIB_PROGRAM_STATE_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"
#include "../../HighPerfTools/HashSets.h"

#include "LocLib_ScanAndTok.h"
//#include "LocLib_PreParserTypes.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_SourceFileDescAndState.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_TypecheckerTypes.h"
#include "LocLib_IR_Types.h"

local_func_inl TracableEvent EventEXPR_TC_NON_INVOC() {
    TracableEvent result = {};
    result.uEventType = EEvtEXPR::EEVT_EXPR_TC_NON_INVOC;
}
local_func void print_event_EXPR_TC_NON_INVOC_to(char* szBuffer, const TracableEvent& eventEntry) {
    Assert_(eventEntry.eLocPhase == ELOCPHASE_TC_EXPR);
    Assert_(eventEntry.uEventType == EEVT_EXPR_TC_NON_INVOC);
    sprintf(szBuffer, "just testing printing TC NON INVOC EXPR");
}

enum ETaskType {
    ETASK_TYPECHECK_SOURCE_FILE = 0x10000000u,
    ETASK_EVAL_TYPE_DECL        = 0x20000000u,
    ETASK_EVAL_FULL_TYPE_DEF    = 0x30000000u,
};

#define ETASK_TYPE_MASK           0xF0000000u

struct ParseFileTaskDescription {
    int iIndexOfDemandingSourceFile;
    u32 uBlockInDemandingSourceFile;
    u32 uStatementInDemandingBlock;
    u32 uNodeInDemandingStatement;
};

// predecl
struct WholeProgramCompilationState;
void init_reserved_type_and_value(u8 uReservedIndex, const TypeInfo* pType, u32 uValuePayload, WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker);
void init_reserved_type_and_value(u8 uReservedIndex, const TypeInfo* pType, u64 uIRAndFlags, u64 uMetaPayload, WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker);
void init_reserved_type_and_value(u8 uReservedIndex, const TypeInfo* pType, const TypeInfo* pTypeValue, WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker);

struct WholeProgramCompilationState {
	
    ChunkProvider parseOnlyProvider;
    Arena parseOnlyArena;
    StableGrowingVector<FFString> vecAllIdentifierNamesById;
    TmpStringMap<u32> mapAllIdentifierIdsByName;

    TmpMap<int, ParseFileTaskDescription> mapOfParseSourceFileTasksToLaunch;
    TmpSet<int> setOfFilesWithTCTasksToLaunch;
    TmpSet<int> setOfFilesWithTCWaitingTasks;
    TmpSet<int> setOfFilesWithActiveTasksBeingRun;

    volatile u64 uCountOfActivelyWorking; //  Atomic !

    ChunkProvider globalProvider; // TODO: possibly separate global provider for parser-only,
                                  //   from a global-provider behind locks.

    Arena globalArena;
    // volatile u64 vecSourceFileMutexEmplacement;          // TODO
    StableGrowingVector<SourceFileDescAndState*> vecSourceFiles; // TODO: locks on write if other than parser can ever emit registration ??
    // volatile u64 vecBackendErrorsMutexEmplacement;       // TODO
    StableGrowingVector<LocLib_Error> vecBackendErrors;     // TODO: locks on write if MT ? or switch to simpler TmpVector and have one per each thread
    // volatile u64 mapRuntimeTypeIdMutexEmplacement;       // TODO
    TmpMap<u64, const TypeInfo*> mapRuntimeTypeIdToTCTypeInfo;    // TODO: locks on access

    ChunkProvider typecheckingProvider; // special provider with thread-safetly enabled, for initializing per-source-file arenas.

    // TODO: one provider and Arena per worker thread ?

    IRRepo programwiseRepo;

    //u8* pBaseOfCoreTypeInfo;
    // volatile u64 vecTypesMutexEmplacement;               // TODO
    // StableGrowingVector<PseudoTypeInfo_ProcLikeSign*> vecProcLikeSignTypes;    // TODO: locks on write
    // StableGrowingVector<TypeInfo_ProcLikeAddress*> vecProcLikeAddressTypes;    // TODO: locks on write
    // StableGrowingVector<TypeInfo_Pointer*> vecPointerTypes;                    // TODO: locks on write
    // StableGrowingVector<TypeInfo_DistinctAlias*> vecAliasedTypes;              // TODO: locks on write
    // StableGrowingVector<TCProcBodyResult*> vecTypecheckedProcLikeBodies;       // TODO: locks on write

    LocLib_OS_WrapperFunctions* pOsFuncs;
    LocLib_CompilationParams* pCompilationParams;
    LocLib_CompilationResults* pCompilationResults;
    
    u32 uUniqueIdIncreasing; // no need for 64b since an 'id' is a 32b index here anyway...

    void init_reserved_words_and_implicit_members_ids()
    {
        #define RESERVED_WORD(syntaxName, name, specialExpand) mapAllIdentifierIdsByName.insert(syntaxName, ERES_ ## name);
            RESERVED_WORDS_EMITTER_
        #undef RESERVED_WORD

        static_assert(EIMPLICIT_MEMBER_LENGTH_IN_BYTES == COUNT_RESERVED_WORDS, "hola ?");

        #define IMPLICIT_MEMBER(syntaxName, name)              mapAllIdentifierIdsByName.insert(syntaxName, EIMPLICIT_MEMBER_ ## name);
            IMPLICIT_MEMBERS_EMITTER_
        #undef IMPLICIT_MEMBER

        Assert_(mapAllIdentifierIdsByName.size() == COUNT_RESERVED_WORDS + COUNT_IMPLICIT_MEMBERS);
        vecAllIdentifierNamesById.resize(COUNT_RESERVED_WORDS + COUNT_IMPLICIT_MEMBERS);
        for (auto it = mapAllIdentifierIdsByName.begin(), itEnd = mapAllIdentifierIdsByName.end(); it != itEnd; it++) {
            vecAllIdentifierNamesById[it.value()] = it.key();
        }
    }

    void init_reserved_words_values(WorkerDesc* pWorker)
    {
        BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL6_SIGNIFICANT_INFO, EventREPT_CUSTOM_HARDCODED(
            "Initializing Reserved Word Values"), pWorker);

        g_tReservedWordValues = (NodeValue*)alloc_from(globalArena, sizeof(NodeValue) * COUNT_RESERVED_WORDS, alignof(NodeValue));
        
        const TypeInfo* pTypeOfTypes = g_pCoreTypesInfo[ECORETYPE_TYPE];
        constexpr u32 RESERVED_WORD_VALUE_TRUTH = 0000'0001u;
        constexpr u32 RESERVED_WORD_VALUE_FALSITY = 0000'0000u;
        constexpr u32 RESERVED_WORD_VALUE_INF = u32(fp32_infinity[0]);
        constexpr u32 RESERVED_WORD_VALUE_SNAN = u32(fp32_snan[0]);
        constexpr u32 RESERVED_WORD_VALUE_QNAN = u32(fp32_qnan[0]);

        const u64 RESERVED_WORD_SPECIAL_VALUE_FOREIGN = u64(EBuiltin::EBUILTIN_FOREIGN) << 4;
        const u64 RESERVED_WORD_SPECIAL_VALUE_FOREIGN_SOURCE = u64(EBuiltin::EBUILTIN_FOREIGN_SOURCE) << 4;

        const u64 RESERVED_WORD_SPECIAL_VALUE_MEMCPY = u64(EBuiltin::EBUILTIN_MEMCPY) << 4;
        const u64 RESERVED_WORD_SPECIAL_VALUE_REV_MEMCPY = u64(EBuiltin::EBUILTIN_REV_MEMCPY) << 4;
        const u64 RESERVED_WORD_SPECIAL_VALUE_MEMSET = u64(EBuiltin::EBUILTIN_MEMSET) << 4;
        const u64 RESERVED_WORD_SPECIAL_VALUE_ZEROMEM = u64(EBuiltin::EBUILTIN_ZEROMEM) << 4;
        const u64 RESERVED_WORD_SPECIAL_VALUE_MEMCMP = u64(EBuiltin::EBUILTIN_MEMCMP) << 4;
        const u64 RESERVED_WORD_SPECIAL_VALUE_RDTSC = u64(EBuiltin::EBUILTIN_RDTSC) << 4;

        const int RESERVED_WORD_ALIAS_BYTE = ECoreType::ECORETYPE_U8;
        const int RESERVED_WORD_ALIAS_FLOAT = ECoreType::ECORETYPE_F64;
        Assert(pCompilationParams->bRegSizeIs64, "TODO: change alias for other reg sizes");
        int RESERVED_WORD_ALIAS_REG = ECoreType::ECORETYPE_R64;
        int RESERVED_WORD_ALIAS_RPTRMIN32 = ECoreType::ECORETYPE_R64;
        int RESERVED_WORD_ALIAS_UPTRMIN32 = ECoreType::ECORETYPE_U64;
        int RESERVED_WORD_ALIAS_IPTRMIN32 = ECoreType::ECORETYPE_I64;
        int RESERVED_WORD_ALIAS_RPTRMIN64 = ECoreType::ECORETYPE_R64;
        int RESERVED_WORD_ALIAS_UPTRMIN64 = ECoreType::ECORETYPE_U64;
        int RESERVED_WORD_ALIAS_IPTRMIN64 = ECoreType::ECORETYPE_I64;

        #define RESERVED_WORD_TYPE(name)                    pTypeOfTypes, g_pCoreTypesInfo[ECORETYPE_ ## name]
        #define RESERVED_WORD_TYPE_ALIAS(name)              pTypeOfTypes, g_pCoreTypesInfo[RESERVED_WORD_ALIAS_ ## name]
        #define RESERVED_WORD_VALUE(type, name)             g_pCoreTypesInfo[ECORETYPE_ ## type], RESERVED_WORD_VALUE_ ## name
        #define RESERVED_WORD_SPECIAL_VALUE(type, name)     g_pCoreTypesInfo[ECORETYPE_ ## type], IRFLAG_IS_KNOWN|IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST, RESERVED_WORD_SPECIAL_VALUE_ ## name
        #define RESERVED_WORD(syntaxName, name, specialExpand) init_reserved_type_and_value(ERES_ ## name, specialExpand, this, pWorker);
            RESERVED_WORDS_EMITTER_
        #undef RESERVED_WORD
        #undef RESERVED_WORD_SPECIAL_VALUE
        #undef RESERVED_WORD_VALUE
        #undef RESERVED_WORD_TYPE_ALIAS
        #undef RESERVED_WORD_TYPE

        // Special null type on sink and 'invalid id'
        g_tReservedWordValues[ERES_INVALID_ID].pType = 0;
        g_tReservedWordValues[ERES_SINK].pType = 0;

        Assert_(g_tReservedWordValues[ERES_RDTSC].pType == g_pCoreTypesInfo[ECORETYPE_BUILTIN]);
        Assert_(g_tReservedWordValues[ERES_RDTSC].info.uIRandMetaFlags & IRFLAG_TC_ONLY);
        Assert_(g_tReservedWordValues[ERES_RDTSC].info.metaValue._payload == u64(EBuiltin::EBUILTIN_RDTSC) << 4);
        Assert_(g_pCoreTypesInfo[ECORETYPE_BUILTIN]->_coreType == ECORETYPE_BUILTIN);
    }

    // can be triggered by comptime builtin (=> some form of macro...), or used by temporaries
    u32 make_unique_id(u16* outErrCodeOpt = 0) {
        // TODO: make that a task !!! (should not if files are currently being parsed)
        uUniqueIdIncreasing += 1u;
        char szTmp[1024];
        sprintf(szTmp, "_unique__id__%x_", uUniqueIdIncreasing);
        StringView uniqueIdView(szTmp);
        u64 uHash = get_map_hash(uniqueIdView);
        Assert_(mapAllIdentifierIdsByName.findHashed(uHash, uniqueIdView) == mapAllIdentifierIdsByName.end());
        u32 uIdIndex = vecAllIdentifierNamesById.size();
        if (NOMINAL(0 == (uIdIndex & 0x8000'0000u))) {
            auto it = mapAllIdentifierIdsByName.insert_not_present(uHash, uniqueIdView, uIdIndex);
            // pushing it.key() means we now push the allocated string from the map itself !
            vecAllIdentifierNamesById.append(it.key());
            return uIdIndex;
        } else {
            if (outErrCodeOpt) {
                *outErrCodeOpt = CERR_TOO_MANY_IDENTIFIERS;
            } else {
                Assert_(false);
            }
            return ERES_INVALID_ID;
        }
    }

    // helper method for some comptime builtins (=> some form of macros...) (such as 'get_or_make_concat_id', below)
    u32 get_or_make_id(StringView idView, u16* outErrCodeOpt = 0) {
        // TODO: make that a task !!! (should not run if files are currently being parsed)
        u64 uHash = get_map_hash(idView);
        auto it = mapAllIdentifierIdsByName.findHashed(uHash, idView);
        if (it != mapAllIdentifierIdsByName.end()) {
            return it.value();
        } else {
            u32 uIdIndex = vecAllIdentifierNamesById.size();
            if (NOMINAL(0 == (uIdIndex & 0x8000'0000u))) {
                auto it = mapAllIdentifierIdsByName.insert_not_present(uHash, idView, uIdIndex);
                // pushing it.key() means we now push the allocated string from the map itself !
                vecAllIdentifierNamesById.append(it.key());
                return uIdIndex;
            } else {
                if (outErrCodeOpt) {
                    *outErrCodeOpt = CERR_TOO_MANY_IDENTIFIERS;
                } else {
                    Assert_(false);
                }
                return ERES_INVALID_ID;
            }
        }
    }

    // can be triggered by comptime builtin (=> some form of macro...), or used by temporaries
    // --> implements the '#concat_id(id1, id2)' macro
    u32 get_or_make_concat_id(StringView first, StringView second, u16* outErrCodeOpt = 0) {
        u32 uTotalSize = first.byte_length() + second.byte_length();
        if (NOMINAL(uTotalSize < 4096u)) {
            char szTmp[4096];
            memcpy(szTmp, first.begin(), first.byte_length());
            memcpy(szTmp + first.byte_length(), second.begin(), second.byte_length());
            szTmp[uTotalSize] = '\0';
            StringView resultView = StringView::from_known_c_str(szTmp, uTotalSize, true);
            return get_or_make_id(resultView);
        } else {
            if (outErrCodeOpt) {
                *outErrCodeOpt = CERR_RESULTING_IDENTIFIER_TOO_LONG;
            } else {
                Assert_(false);
            }
            return ERES_INVALID_ID;
        }
    }

};


local_func void ir_decode_non_imm(u64 uParam, IRAwareContext* pIRCtx, IRRepo** outRepo, u32* outIndex, SourceFileDescAndState** outSourceFile, EEntryKind* outKind) {
    Assert_(!ir_is_immediate(uParam));
    u32 uPayload32 = u32(uParam >> IR_STD_PARAM_SHIFT);
    if (0uLL == (uParam & IR_STD_PARAM_HIGHMASK)) {
        *outRepo = &(pIRCtx->pProgCompilationState->programwiseRepo);
        *outIndex = uPayload32;
        *outSourceFile = 0;
        *outKind = EEK_PROGRAMWISE_ENTRY;
    } else {
        u16 uRepoIndex = u16(uParam >> IR_STD_PARAM_REPO_ID_SHIFT);
        if (uRepoIndex >= IR_REPO_ID_FIRST_FILE) {
            u32 uFileIndex = uRepoIndex - IR_REPO_ID_FIRST_FILE;
            *outSourceFile = pIRCtx->pProgCompilationState->vecSourceFiles[uRepoIndex - IR_REPO_ID_FIRST_FILE];
            *outIndex = uPayload32 & 0x003F'FFFFu;
            u32 uHigh22and23 = uPayload32 & 0x00C0'0000u;
            switch (uHigh22and23) {
                case 0x0000'0000u:
                    *outRepo = &((*outSourceFile)->filewiseConstRepo);
                    *outKind = EEK_FILEWISE_CONST;
                    break;
                case 0x0040'0000u:
                    *outRepo = &((*outSourceFile)->filewiseGlobalVarRepo);
                    *outKind = EEK_FILEWISE_VAR;
                    break;
                case 0x0080'0000u:
                    *outRepo = 0;
                    *outKind = EEK_IS_PROCBODY_REF;
                    break;
                case 0x00C0'0000u: // reserved for future use
                    *outRepo = 0;
                    *outKind = EEK_NOT_AN_ENTRY;
                    break;
                default:
                    Assume_(false);
            }
        } else {
            Assert_(pIRCtx->pIsolatedSourceFile);
            *outSourceFile = pIRCtx->pIsolatedSourceFile;
            *outIndex = uPayload32 & 0x00FF'FFFFu;
            if (uRepoIndex == IR_REPO_ID_CURRENT_PROC) {
                Assert_(pIRCtx->pRepo);
                Assert_(pIRCtx->pProcResult);
                Assert_(pIRCtx->pRepo == &(pIRCtx->pProcResult->procwiseRepo));
                *outRepo = pIRCtx->pRepo;
                *outKind = EEK_CURRENT_PROC_LOCAL;
            } else {
                Assert_(uRepoIndex == IR_REPO_ID_TEMPORARY);
                Assert_(pIRCtx->pTmpRepo);
                *outRepo = pIRCtx->pTmpRepo;
                *outKind = EEK_CURRENT_TEMPORARY;
            }
        }        
    }
}

local_func u32 register_identifier_during_parsing(StringView strId, SourceFileDescAndState* pSourceFileDesc, u16* outErrCodeOpt)
{
    TmpStringMap<u32>& mapAllIdentifierIdsByName = pSourceFileDesc->programState->mapAllIdentifierIdsByName;
    u64 uHash = get_map_hash(strId);
    auto it = mapAllIdentifierIdsByName.findHashed(uHash, strId);
    if (it != mapAllIdentifierIdsByName.end()) {
        return int(it.value());
    } else {
        u32 uId = pSourceFileDesc->locallyFoundIdentifierNamesByOffsetId.size() + pSourceFileDesc->uIdentifierNamesOffset;
        if (NOMINAL(0 == (uId & 0x8000'0000u))) {
            it = mapAllIdentifierIdsByName.insert_not_present(uHash, strId, uId);
            // pushing it.key() means we now push the allocated string from the map itself !
            pSourceFileDesc->locallyFoundIdentifierNamesByOffsetId.append(it.key());
            Assert_(uId == it.value());
            return uId;
        } else {
            if (outErrCodeOpt) {
                *outErrCodeOpt = CERR_RESULTING_IDENTIFIER_TOO_LONG;
            } else {
                Assert_(false);
            }
            return ERES_INVALID_ID;
        }
    }
}

constexpr u32 tUnnamedPayload[5] = {
    STRING_VIEW_FLAG_ENDS_WITH_ZERO|STRING_VIEW_FLAG_PREFIXED_WITH_SIZE_AND_FLAGS|STRING_VIEW_FLAG_KNOWN_7b_ASCII|STRING_VIEW_FLAG_KNOWN_VALID_Utf8,
    9u,
    u32(u8('<'))|u32(u8('u')<<8)|u32(u8('n')<<16)|u32(u8('n')<<24),
    u32(u8('a'))|u32(u8('m')<<8)|u32(u8('e')<<16)|u32(u8('d')<<24),
    u32(u8('>')),
};

local_func_inl FFString get_identifier_string(WholeProgramCompilationState* pProgramState, i32 iIdentifierId) {
    if (iIdentifierId > 0) {
        return pProgramState->vecAllIdentifierNamesById[u32(iIdentifierId)];
    } else {
        FFString result;
        result.pStart = const_cast<u8*>(reinterpret_cast<const u8*>(tUnnamedPayload + 2));
        return result;
    }
}

local_func_inl StringView get_identifier_stringview(WholeProgramCompilationState* pProgramState, i32 iIdentifierId) {
    return StringView(get_identifier_string(pProgramState, iIdentifierId));
}

local_func StringView get_identifier_stringview_when_parsing(SourceFileDescAndState* pSourceFileDesc, u32 uIdentifierId) {
    if (uIdentifierId >= pSourceFileDesc->uIdentifierNamesOffset) {
        return pSourceFileDesc->locallyFoundIdentifierNamesByOffsetId[uIdentifierId - pSourceFileDesc->uIdentifierNamesOffset];
    } else {
        return pSourceFileDesc->programState->vecAllIdentifierNamesById[uIdentifierId];
    }
}

local_func bool init_program_compilation_state(WholeProgramCompilationState* pProgramCompState,
                                    LocLib_OS_WrapperFunctions* pOsFuncs,
                                    LocLib_CompilationParams* pCompilationParams,
                                    LocLib_CompilationResults* oCompilationResults)
{
    pProgramCompState->pOsFuncs = pOsFuncs;
    pProgramCompState->pCompilationParams = pCompilationParams;
    pProgramCompState->pCompilationResults = oCompilationResults;

    init_chunk_provider(&pProgramCompState->globalProvider);
    init_chunk_provider(&pProgramCompState->parseOnlyProvider);
    init_chunk_provider(&pProgramCompState->typecheckingProvider /*, true */); // TODO: uncomment for enabling thread-safety

    init_arena(&pProgramCompState->globalArena, pProgramCompState->globalProvider);
    init_arena(&pProgramCompState->parseOnlyArena, pProgramCompState->parseOnlyProvider);

    pProgramCompState->mapOfParseSourceFileTasksToLaunch.init(pProgramCompState->globalArena);
    pProgramCompState->setOfFilesWithTCTasksToLaunch.init(pProgramCompState->globalArena);
    pProgramCompState->setOfFilesWithTCWaitingTasks.init(pProgramCompState->globalArena);
    pProgramCompState->setOfFilesWithActiveTasksBeingRun.init(pProgramCompState->globalArena);
    pProgramCompState->uCountOfActivelyWorking = 0;

    pProgramCompState->vecSourceFiles.init(pProgramCompState->globalArena);
    pProgramCompState->vecBackendErrors.init(pProgramCompState->globalArena);
    pProgramCompState->mapRuntimeTypeIdToTCTypeInfo.init(pProgramCompState->globalArena);
    
    //pProgramCompState->vecProcLikeSignTypes.init(pProgramCompState->globalArena);
    //pProgramCompState->vecProcLikeAddressTypes.init(pProgramCompState->globalArena);
    //pProgramCompState->vecPointerTypes.init(pProgramCompState->globalArena);
    //pProgramCompState->vecAliasedTypes.init(pProgramCompState->globalArena);

    pProgramCompState->vecAllIdentifierNamesById.init(pProgramCompState->parseOnlyArena);
    pProgramCompState->mapAllIdentifierIdsByName.init(pProgramCompState->parseOnlyArena);

    init_keywords();

    // We init reserved words as "identifiers" before initializing types (type init can now also emit identifiers, eg for fields of core struct impl)
    pProgramCompState->init_reserved_words_and_implicit_members_ids();

    // We also push our special implicit members identifiers


    //
    // TODO: the following is no longer needed in current scheme. What about the whole concept of a struct_like_impl for core types ?
    //    eg. string related types do not need that struct_like_impl type any more
    // CLEANUP: think about that again once we actuall implemented "any", dynamic arrays, and hash-containers.
    //
    
    /*
    u32 idFlags = pProgramCompState->get_or_make_id("flags");
    u32 idBytes = pProgramCompState->get_or_make_id("bytes");
    u32 idByteLength = pProgramCompState->get_or_make_id("byte_length");
    u32 idData = pProgramCompState->get_or_make_id("data");
    u32 idCount = pProgramCompState->get_or_make_id("count");
    u32 idAllocFn = pProgramCompState->get_or_make_id("alloc_fn");
    u32 idAllocData = pProgramCompState->get_or_make_id("alloc_data");
    u32 idTypeId = pProgramCompState->get_or_make_id("type_id");

    for (int iDummyStruct = 0; iDummyStruct < 3; iDummyStruct++) {
        TypeInfo_StructLike* pDummyStruct = g_tDummyStructLikeTypes + iDummyStruct;
        TCCompoundRegistration* pDummyRegistration = g_tDummyStructRegistrations + iDummyStruct;
        pDummyStruct->pRegistration = pDummyRegistration;
        pDummyStruct->uRegistrationIndex = u16(-1);
        pDummyStruct->uTypeKind = ETypeKind::ETYPEKIND_STRUCTLIKE;
        pDummyStruct->_coreType = COMPOUNDTYPE_IS_STRUCT; 
        pDummyStruct->_coreFlags = 0; 
        pDummyStruct->uIRFormat = 0x03u;
        pDummyStruct->mapAllMembers.init(pProgramCompState->globalArena);
        pDummyStruct->vecAllMembers.init(pProgramCompState->globalArena);

        pDummyRegistration->pCompoundType = pDummyStruct;
        pDummyRegistration->pRootTcBlock = 0;
        pDummyRegistration->pStatementWithSignature = 0;
        pDummyRegistration->setWaitingConstOnly.init(pProgramCompState->globalArena);
        pDummyRegistration->setWaitingPossiblyRuntime.init(pProgramCompState->globalArena);
        pDummyRegistration->uTCProgress = ECOMPOUND_DONE_ALL;
    }

    for (int iDummyBinding = 0; iDummyBinding < 7; iDummyBinding++) {
        ValueBinding* pDummyBinding = g_tDummyStructBindings + iDummyBinding;
        *pDummyBinding = {};
        pDummyBinding->uFlags = VALUEHOLDER_FLAG_IS_UNKNOWN;
        pDummyBinding->uScopeAndWhenConstFlags = EScopeKind::SCOPEKIND_COMPOUND;
    }

    ValueBinding* pStringRelatedBytesBinding = g_tDummyStructBindings + 0;
    ValueBinding* pStringRelatedByteLengthBinding = g_tDummyStructBindings + 1;
    ValueBinding* pStringRelatedFlagsBinding = g_tDummyStructBindings + 2;
    ValueBinding* pStringRelatedAllocFnBinding = g_tDummyStructBindings + 3;
    ValueBinding* pStringRelatedAllocDataBinding = g_tDummyStructBindings + 4;
    ValueBinding* pAnyDataBinding = g_tDummyStructBindings + 5;
    ValueBinding* pAnyTypeIdBinding = g_tDummyStructBindings + 6;

    pStringRelatedBytesBinding->iIdentifierHandle = idBytes;
    pStringRelatedBytesBinding->pType = g_pCoreTypesInfo[ECORETYPE_RAWPTR];
    pStringRelatedBytesBinding->uIR = 0u; // salvaged for offset from base in structlikes

    pStringRelatedByteLengthBinding->iIdentifierHandle = idByteLength;
    pStringRelatedByteLengthBinding->pType = g_pCoreTypesInfo[ECORETYPE_U32];
    pStringRelatedByteLengthBinding->uIR = 4u; // salvaged for offset from base in structlikes

    pStringRelatedFlagsBinding->iIdentifierHandle = idFlags;
    pStringRelatedFlagsBinding->pType = g_pCoreTypesInfo[ECORETYPE_U32];
    pStringRelatedFlagsBinding->uIR = 8u; // salvaged for offset from base in structlikes

    pStringRelatedAllocFnBinding->iIdentifierHandle = idAllocFn;
    pStringRelatedAllocFnBinding->pType = g_pCoreTypesInfo[ECORETYPE_RAWPTR];
    pStringRelatedAllocFnBinding->uIR = 16u; // salvaged for offset from base in structlikes

    pStringRelatedAllocDataBinding->iIdentifierHandle = idAllocData;
    pStringRelatedAllocDataBinding->pType = g_pCoreTypesInfo[ECORETYPE_RAWPTR];
    pStringRelatedAllocDataBinding->uIR = 24u; // salvaged for offset from base in structlikes

    pAnyDataBinding->iIdentifierHandle = idData;
    pAnyDataBinding->pType = g_pCoreTypesInfo[ECORETYPE_R64];
    pAnyDataBinding->uIR = 0u; // salvaged for offset from base in structlikes

    pAnyTypeIdBinding->iIdentifierHandle = idTypeId;
    pAnyTypeIdBinding->pType = g_pCoreTypesInfo[ECORETYPE_TYPE_ID];
    pAnyTypeIdBinding->uIR = 8u; // salvaged for offset from base in structlikes

    TypeInfo_StructLike* pStringViewStr = g_tDummyStructLikeTypes + 0;
    pStringViewStr->pRegistration->iPrimaryIdentifier = pProgramCompState->get_or_make_id("#string_view_struct");
    pStringViewStr->uUnalignedByteSize = 16u;
    pStringViewStr->uSlotsAndAlign = 0x3000'0002u;
    pStringViewStr->uRuntimeMemberCount = 3u;
    pStringViewStr->mapAllMembers.insert(idBytes, 0u);
    pStringViewStr->mapAllMembers.insert(idByteLength, 1u);
    pStringViewStr->mapAllMembers.insert(idFlags, 2u);
    pStringViewStr->vecAllMembers.append(pStringRelatedBytesBinding);
    pStringViewStr->vecAllMembers.append(pStringRelatedByteLengthBinding);
    pStringViewStr->vecAllMembers.append(pStringRelatedFlagsBinding);

    TypeInfo_StructLike* pOwnedStringStr = g_tDummyStructLikeTypes + 1;
    pOwnedStringStr->pRegistration->iPrimaryIdentifier = pProgramCompState->get_or_make_id("#owned_string_struct");
    pOwnedStringStr->uUnalignedByteSize = 32u;
    pOwnedStringStr->uSlotsAndAlign = 0x3000'0004u;
    pOwnedStringStr->uRuntimeMemberCount = 5u;
    pOwnedStringStr->mapAllMembers.insert(idBytes, 0u);
    pOwnedStringStr->mapAllMembers.insert(idByteLength, 1u);
    pOwnedStringStr->mapAllMembers.insert(idFlags, 2u);
    pOwnedStringStr->mapAllMembers.insert(idAllocFn, 3u);
    pOwnedStringStr->mapAllMembers.insert(idAllocData, 4u);
    pOwnedStringStr->vecAllMembers.append(pStringRelatedBytesBinding);
    pOwnedStringStr->vecAllMembers.append(pStringRelatedByteLengthBinding);
    pOwnedStringStr->vecAllMembers.append(pStringRelatedFlagsBinding);
    pOwnedStringStr->vecAllMembers.append(pStringRelatedAllocFnBinding);
    pOwnedStringStr->vecAllMembers.append(pStringRelatedAllocDataBinding);

    TypeInfo_StructLike* pAnyStr = g_tDummyStructLikeTypes + 2;
    pAnyStr->pRegistration->iPrimaryIdentifier = pProgramCompState->get_or_make_id("#any");
    pAnyStr->uUnalignedByteSize = 16u;
    pAnyStr->uSlotsAndAlign = 0x3000'0002u;
    pAnyStr->uRuntimeMemberCount = 2u;
    pAnyStr->mapAllMembers.insert(idData, 0u);
    pAnyStr->mapAllMembers.insert(idTypeId, 1u);
    pAnyStr->vecAllMembers.append(pAnyDataBinding);
    pAnyStr->vecAllMembers.append(pAnyTypeIdBinding);
    */

    return true;
}

local_func bool register_source_file(int iFileToRegisterIndex, int iLoadedFromFileIndex, int iLoadedFromLineIndex,
                    WholeProgramCompilationState* pProgramCompState,
                    LocLib_OS_WrapperFunctions* pOsFuncs,
                    LocLib_CompilationParams* pCompilationParams,
                    LocLib_CompilationResults* oCompilationResults)
{
    // TODO : locks

    Assert_(iFileToRegisterIndex >= 0);
    int iFileNameByteCount = pOsFuncs->pGetSourceFileFullNameByteCountFn(iFileToRegisterIndex);
    Assert_(iFileNameByteCount > 1);
    Assert_(iFileNameByteCount <= 4096);
    char dummy[4096];
    FFString fileName = FFString::makeFF(StringView::from_known_c_str(dummy, u32(iFileNameByteCount), false), pProgramCompState->globalArena);
    pOsFuncs->pGetSourceFileFullNameFn(iFileToRegisterIndex, (char*)fileName.pStart);

    SourceFileDescAndState* pSourceFileDesc = reinterpret_cast<SourceFileDescAndState*>(
        alloc_from(pProgramCompState->globalArena, sizeof(SourceFileDescAndState), alignof(SourceFileDescAndState)));
    Arena sourceFileSpecificArena;
    init_arena(&sourceFileSpecificArena, pProgramCompState->typecheckingProvider);
    pSourceFileDesc->init(iFileToRegisterIndex, iLoadedFromFileIndex, iLoadedFromLineIndex,
        pProgramCompState->parseOnlyArena, sourceFileSpecificArena, fileName, pProgramCompState);

    u32 uCurrentSizeOfVecSourceFiles = pProgramCompState->vecSourceFiles.size();
    Assert_(iFileToRegisterIndex >= 0);
    if (u32(iFileToRegisterIndex) == uCurrentSizeOfVecSourceFiles) {
        pProgramCompState->vecSourceFiles.append(pSourceFileDesc);
    } else {
        if (u32(iFileToRegisterIndex) > uCurrentSizeOfVecSourceFiles) {
            pProgramCompState->vecSourceFiles.resize(u32(iFileToRegisterIndex) + 1u);
            for (u32 uIndex = uCurrentSizeOfVecSourceFiles; uIndex < u32(iFileToRegisterIndex); uIndex++) {
                pProgramCompState->vecSourceFiles[uIndex] = 0;
            }
        }
        pProgramCompState->vecSourceFiles[iFileToRegisterIndex] = pSourceFileDesc;
    }
    return true;
}

local_func bool _check_file_name(const u8* pFileName, u16 uFileNameBytes, int iLoadedFromFileIndex, int iLoadedFromLineIndex,
    WholeProgramCompilationState* pProgramCompState,
    LocLib_OS_WrapperFunctions* pOsFuncs,
    LocLib_CompilationParams* pCompilationParams,
    LocLib_CompilationResults* oCompilationResults)
{
    if (uFileNameBytes && uFileNameBytes < 4096) {
        // TODO: also check input format
        return true;
    } else {
        platform_log_info("*** _check_file_name() : invalid string size", true);
        // TODO: log error
        return false;
    }
}

local_func int get_or_register_source_file(const u8* pFileName, u16 uFileNameBytes, int iLoadedFromFileIndex, int iLoadedFromLineIndex,
    WholeProgramCompilationState* pProgramCompState,
    LocLib_OS_WrapperFunctions* pOsFuncs,
    LocLib_CompilationParams* pCompilationParams,
    LocLib_CompilationResults* oCompilationResults)
{
    Assert_(pFileName);
    if (_check_file_name(pFileName, uFileNameBytes, iLoadedFromFileIndex, iLoadedFromLineIndex,
                         pProgramCompState, pOsFuncs, pCompilationParams, oCompilationResults)) {
        u16 uError = 0;
        int iRetrieveSourceFileIndexResult = pOsFuncs->pGetSourceFileIndexFn(iLoadedFromFileIndex,
            reinterpret_cast<const char*>(pFileName), i16(uFileNameBytes), &uError);
        if (0 == uError) {
            Assert_(iRetrieveSourceFileIndexResult >= 0);
            // TODO : locks
            if (iRetrieveSourceFileIndexResult < pProgramCompState->vecSourceFiles.size() &&
                    pProgramCompState->vecSourceFiles[iRetrieveSourceFileIndexResult]) {
                return iRetrieveSourceFileIndexResult;
            } else {
                if (register_source_file(iRetrieveSourceFileIndexResult, iLoadedFromFileIndex, iLoadedFromLineIndex,
                        pProgramCompState, pOsFuncs, pCompilationParams, oCompilationResults)) {
                    return iRetrieveSourceFileIndexResult;
                } else {
                    return -1;
                }
            }
        } else {
            // TODO: log error
            return -1;
        }
    } else {
        return -1;
    }
}

local_func ESourceCompState is_source_file_compiled_otherwise_spawn_task(int iSourceFileIndex,
    int iDemandingFileIndex, u32 uBlockInDemandingFile, u32 uStatementInDemandingBlock, u32 uNodeInDemandingStatement,
    WholeProgramCompilationState* pProgCompilationState, LocLib_CompilationParams* pCompilationParams, LocLib_CompilationResults* oCompilationResults)
{
    // TODO: put behind lock for multithread
    Assert_(iSourceFileIndex >= 0 && iSourceFileIndex < (int)pProgCompilationState->vecSourceFiles.size());
    SourceFileDescAndState* pSourceFileDesc = pProgCompilationState->vecSourceFiles[iSourceFileIndex];
    ESourceCompState uCurrentCompState = pSourceFileDesc->pRootNamespace->eCompState;
    if (uCurrentCompState == ESourceCompState::ESOURCE_COMP_STATE_NOT_STARTED) {
        if (pProgCompilationState->mapOfParseSourceFileTasksToLaunch.find(iSourceFileIndex) ==
            pProgCompilationState->mapOfParseSourceFileTasksToLaunch.end())
        {
            // TODO: find some worker to trace that...
            /*
            BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED("spawning new task for parsing source file '%s'",
                reinterpret_cast<u32>(pSourceFileDesc->sourceFileName.c_str())), pTCContext->pWorker);
            */
            ParseFileTaskDescription parseTask;
            parseTask.iIndexOfDemandingSourceFile = iDemandingFileIndex;
            parseTask.uBlockInDemandingSourceFile = uBlockInDemandingFile;
            parseTask.uStatementInDemandingBlock = uStatementInDemandingBlock;
            parseTask.uNodeInDemandingStatement = uNodeInDemandingStatement;
            pProgCompilationState->mapOfParseSourceFileTasksToLaunch.insert(iSourceFileIndex, parseTask);
        }
    }
    return uCurrentCompState;
}

local_func_inl void init_reserved_type_and_value(u8 uReservedIndex, const TypeInfo* pType, u32 uValuePayload, WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Init reserved type and value for reserved-word '%s' (u32 value payload version)",
        reinterpret_cast<u64>(get_identifier_string(pProgramState, uReservedIndex).c_str())), pWorker);

    if (get_type_kind(pType) == ETypeKind::ETYPEKIND_INTEGRAL) {
        g_tReservedWordValues[uReservedIndex].info = ir_make_info_for_int_immediate(i32(uValuePayload), get_ir_format(pType));
    } else { Assert_(get_type_kind(pType) == ETypeKind::ETYPEKIND_FLOATINGPOINT); 
        u8 uFormat = get_ir_format(pType);
        if (uFormat == XFLOAT_PSEUDO_IR_FORMAT) {
            Assert_(get_core_type_(pType) == ECoreType::ECORETYPE_FLOAT_LIT);
            uFormat = 0x08u|0x03u; // TODO: to update when float lit gets to other than current 64b tmp... and really is an xfloat.
        }
        g_tReservedWordValues[uReservedIndex].info = ir_make_info_for_float_immediate(type_pun_to_float(uValuePayload), uFormat);
    } // TODO: other possibilities ?
    g_tReservedWordValues[uReservedIndex].pType = pType;
}

local_func_inl void init_reserved_type_and_value(u8 uReservedIndex, const TypeInfo* pType, u64 uIRAndFlags, u64 uMetaPayload, WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Init reserved type and value for reserved-word '%s' (IR + meta version)",
        reinterpret_cast<u64>(get_identifier_string(pProgramState, uReservedIndex).c_str())), pWorker);

    Assert_(uIRAndFlags & IRFLAG_TC_ONLY);
    Assert_(uIRAndFlags & IRFLAG_TC_SEMANTIC_CONST);
    g_tReservedWordValues[uReservedIndex].pType = pType;
    g_tReservedWordValues[uReservedIndex].info.uIRandMetaFlags = uIRAndFlags;
    g_tReservedWordValues[uReservedIndex].info.metaValue._payload = uMetaPayload;
}

local_func_inl void init_reserved_type_and_value(u8 uReservedIndex, const TypeInfo* pType, const TypeInfo* pTypeValue, WholeProgramCompilationState* pProgramState, WorkerDesc* pWorker)
{
    BLOCK_TRACE(ELOCPHASE_REPORT, _LLVL7_REGULAR_STEP, EventREPT_CUSTOM_HARDCODED(
        "Init reserved type and value for reserved-word '%s' (typevalue version)",
        reinterpret_cast<u64>(get_identifier_string(pProgramState, uReservedIndex).c_str())), pWorker);

    Assert_(pType == g_pCoreTypesInfo[ECORETYPE_TYPE]);
    g_tReservedWordValues[uReservedIndex].pType = pType;
    g_tReservedWordValues[uReservedIndex].info.uIRandMetaFlags = IRFLAG_TC_ONLY|IRFLAG_TC_SEMANTIC_CONST|IRFLAG_IS_KNOWN;
    g_tReservedWordValues[uReservedIndex].info.metaValue.knownValue.pType = pTypeValue;
}


local_func_inl void acquire_global_using_graph_lock(WholeProgramCompilationState* pCompState, WorkerDesc* pWorkerDesc)
{
    // TODO !!
}

local_func_inl void release_global_using_graph_lock(WholeProgramCompilationState* pCompState, WorkerDesc* pWorkerDesc)
{
    // TODO !!
}

#endif //LOCLIB_PROGRAM_STATE_H_

