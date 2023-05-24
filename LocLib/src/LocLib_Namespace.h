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

#ifndef LOCLIB_NAMESPACE_H_
#define LOCLIB_NAMESPACE_H_

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

enum ESourceCompState : u32 {
    ESOURCE_COMP_STATE_NOT_STARTED,                // state of the root namespace of a file, when not yet parsed
    ESOURCE_COMP_STATE_DONE_PARSED,                // starting state of any namespace declared in an already parsed file
    ESOURCE_COMP_STATE_DONE_LOCAL_DISCOVERY,       // state of a namespace where all locally defined stuff are known gathered and named (but barring other referenced namespace, even those with *using*)
    ESOURCE_COMP_STATE_DONE_DISCOVERY_USING,       // state of a namespace where all named stuff are known (including those behind *using*)
    ESOURCE_COMP_STATE_DONE_TYPECHECKED,           // state of the root namespace of a file, when no remaining TC tasks are set upon it
    ESOURCE_COMP_STATE_SCANNER_ERROR,              // state of the root namespace of a file, when a scanner error occurred during parsing.
};

struct TCNamespace;
struct ReferencedNamespace;
// A laggued state stores the known bindings declared on a namespace, as seen by other source files.
// There is one lagguedState for each TCNamespace.
//      At start, it was designed so that there is also one distinct lagguedState for each different source file referencing it.
//      Currently, though, all namespaces having knowledge of another go through that same, unique 'lagguedState', foreplanned behind
//         a read/write lock (when we implement MultiThread).
// Usage: 
//    Once a source file has finished a TC-task which has found at least one new declaration within a namespace, it
//       takes a lock on its laggued state, updates it, then release the lock (and scheduler can notify of its update to all interested)
// Initial Plan:
//    Once scheduler knows that a file has no currently working TC tasks, it can take a lock on referenced laggued states for each other file,
//       update its own local copy of the laggued state, then release the lock (and scheduler can maybe wake tasks waiting on identifiers)
// Current Plan:
//    All files accessing namespaces from other files take a read lock on referenced laggued state *at the time of each access*
//
struct LaggedNamespaceState {
    ESourceCompState uDiscoveryProgress;                                        // for recording discovery progress
    u32 uVersion;                                                               // additional counter of "abritrary"-length for recording and *discriminating* discovery progress
    //TmpMap<u64, ReferencedNamespace*> mapKnownReferencedNamespaces;           // This is the map of all namespaces having been directly bound to a namespace binding, or declared as 'using', within this namespace.
    TmpMap<int, u32> mapKnownPublicDeclarationsById;                            // this map points to the binding repository of the *original source file*
    TmpMap<int, u32> mapKnownAccessibleDeclarationsById;                        // this map points to the binding repository of the *original source file*, 
    //TmpArray<u64> vecKnownUsedNamespaces;                                     // These are all the namespaces which are known declared as *using*
    TmpArray<ReferencedNamespace*> vecKnownUsedNamespaces;                      // These are all the namespaces which are known declared as *using*
    TmpArray<const TypeInfo_Enum*> vecKnownUsedEnums;                           // These are all the *enums* which are known declared as *using*

    // TODO: CLEANUP: Currently not really used
    TmpMap<u64, ReferencedNamespace*> mapKnownOthersUsingThis;                  // These are all the namespaces which are known *directly* 'using' this one
};

__declspec(align(8))
struct ReferencedNamespace {
    TCNamespace* pOrigNamespace;            // The TCNamespace, directly in the file having defined it (can also be the root namespace for said file). Can also point to 'itself' in case the referencing is file-to-same-file.
    LaggedNamespaceState laggedState;       // The "referenced" state of the namespace, either updated locally to the file, or as a copy of some version of it for other files.
};
static_assert(alignof(ReferencedNamespace) >= 4, "ReferencedNamespace alignment shall be at least 4 for usage as ScopedEntityHandle");

struct SourceFileDescAndState;
struct TCNamespace {
    ReferencedNamespace asRef; // with pOrigNamespace pointing to 'this'. TODO: a specific lock on its lagged state.
    TCNamespace* pParent;                           // Namespaces can be nested. This thus points is the parent for current namespace. Can be null for root within a file.
    SourceFileDescAndState* pOriginalSourceFile;    // This is the sourcefile in which this namespace was *really* declared, not merely referenced
    u32 uRegistrationIndex;                         // This is the registration index of this namespace *within its original source file*
    ESourceCompState eCompState;                    // ...

    // Note: currently replaced by mapSetLocalUnshadowingByNamespaceUID, directly in source file.
    //TmpSet<int> setLocalUnshadowing;                    // This is the set of all locals found while parsing procbodies within this direct namespace, which should be checked afterwards for name collisions
    
    // Note: currently replaced by a simple vecUsedNamespaces (and vecKnownUsedNamespaces when laggued), which we'll walk recursively through namespace references if needed
    //TmpMap<u64, ReferencedNamespace*> mapReferencedNamespaces;   // This is the map of all namespaces having been directly bound to a namespace binding, or declared as 'using', within this namespace.
    
    TmpMap<int, u32> mapPublicDeclarationsById;         // this map points to the binding repository of the *original source file*
    TmpMap<int, u32> mapAccessibleDeclarationsById;     // this map points to the binding repository of the *original source file*, and includes public, and package.
    TmpMap<int, u32> mapAllGlobalDeclarationsById;      // this map points to the binding repository of the *original source file*, and includes public, package and privates.
    TmpArray<ReferencedNamespace*> vecAllUsedNamespaces;   // These are all the namespaces which are known declared as *using*
    TmpArray<const TypeInfo_Enum*> vecAllUsedEnums;        // These are all the *enums* which are known declared as *using*
    TmpArray<ReferencedNamespace*> vecAccessibleUsedNamespaces;   // These are the namespaces which are known declared as *using* outside of a private scope
    TmpArray<const TypeInfo_Enum*> vecAccessibleUsedEnums;        // These are the *enums* which are known declared as *using* outside of a private scope

    TmpSet<int> setOfNewlyDeclaredGlobalIdentifiers;    // global identifiers, newly declared on a TC pass, for fast-iteration to a lagged state (and waking up tasks).

    // TODO: CLEANUP: Currently not really used
    TmpMap<u64, ReferencedNamespace*> mapOthersUsingThis;    // These are all the namespaces which are *directly* 'using' this one

    u32 uCountGlobalTasksInTasksToLaunch;
    u32 uCountGlobalTasksInWaitingTasks;
};

#endif // LOCLIB_NAMESPACE_H_
