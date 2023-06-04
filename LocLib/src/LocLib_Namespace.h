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
    ESOURCE_COMP_STATE_DONE_ACCESSIBLE_DISCOVERY,  // state of a namespace where all known non-private defined stuff are known gathered and named (incl. 'using')
    ESOURCE_COMP_STATE_DONE_ALL_DISCOVERY,         // state of a namespace where all defined stuff are known gathered and named (incl. 'using')
    ESOURCE_COMP_STATE_DONE_TYPECHECKED,           // state of the root namespace of a file, when no remaining TC tasks are set upon it
    ESOURCE_COMP_STATE_SCANNER_ERROR,              // state of the root namespace of a file, when a scanner error occurred during parsing.

//    ESOURCECOMPSTATEFLAG_BEING_LOCKED = 0x4000'0000u,     // temporary flag (at interplay with atomic handling of changes) while we're adding tasks waiting on its completion
};

struct SourceFileDescAndState;
__declspec(align(8))
struct TCNamespace {
    TCNamespace* pParent;                           // Namespaces can be nested. This thus points is the parent for current namespace. Can be null for root within a file.
    SourceFileDescAndState* pOriginalSourceFile;    // This is the sourcefile in which this namespace was *really* declared, not merely referenced
    u32 uRegistrationIndex;                         // This is the registration index of this namespace *within its original source file*
    u32 volatile uTCProgress;                       // ESourceCompState

    // 'setLocalUnshadowing' : This is the set of all locals found while parsing subordinate entities within this namespace,
    //   which, although not part of the namespace proper, could result in name-collisions *after-the-fact* :
    // Case 1: We check for a name within that subordinate entity. It is not present locally, but present in encompassing namespace:
    //              we take the namespace.
    // Case 2: We check for a name within that subordinate entity. It is present locally:
    //              we take the local one.
    // Case 3: We add a name to the subordinate entity, but it is present in the encompassing namespace already.
    //              => allow ? disallow ?
    // 
    // If we have out of order discovery of the encompassing namespace, and any possible out of order discovery also within the subordinate entity
    //    (directly or through non-blocking 'using' chains of other out-of-order discoveries from namespace or other entities), then we need to
    //    disallow Case 3 : declaring a local identifier which is already present in the encompassing namespace. Otherwise, it could result in
    //    already-successfully-typechecked expressions having taken the case-1 path, and similar expressions after local discovery taking
    //    the case-2 path => NOGOOD !!
    // But, to disallow for case 3 in that fully out-of-order environment, we need to also remember that any names in the subordinate were
    //   forbidden to collide with the encompassing namespace in the first place... otherwise, there will be times where that 'shadowing' of
    //   globals would be refused by the compiler (if it discovered the namespace-one first) while at other times it would be accepted 
    //   (if the namespace one was not yet discovered at time of local declaration) => NOGOOD either !! (and to make things worse,
    //   one behaviour or the other could be totally random in the presence of multithread...).
    // 
    // Note: we dropped out the idea of that "after-the-fact" local unshadowing for proc scopes and enums:
    //   we indeed have sequential discovery within those, and thus by *allowing* shadowing of namespace globals, we just *solve* Case 3
    //   by *not complaining, and overriding the binding afterwards in the sequence*. Which can never behave differently between runs.
    //      Moreover, we can't declare 'using' a namespace within an enum...
    //      And local scope are now blocking on namespace usage (even within same file)... so this prevents different used namespaces to
    //        have to check against each other constantly if used locally in different procs.
    //   
    // However, structlike still have out of order decl 'requiring' something like that:
    TmpSet<int> setLocalUnshadowing;
    
    TmpMap<int, u32> mapAccessibleDeclarationsById;     // this map points to the binding repository of the *original source file*, and includes public, and package.
    TmpMap<int, u32> mapAllGlobalDeclarationsById;      // this map points to the binding repository of the *original source file*, and includes public, package and privates.

    TmpArray<TCNamespace*> vecAccessibleUsedNamespaces; // These are the namespaces which are known declared as *using* outside of a private scope
    TmpArray<TCNamespace*> vecAllUsedNamespaces;        // These are all the namespaces which are known declared as *using*

    TmpArray<const TypeInfo_Enum*> vecAccessibleUsedEnums;      // These are the *enums* which are known declared as *using* outside of a private scope
    TmpArray<const TypeInfo_Enum*> vecAllUsedEnums;             // These are all the *enums* which are known declared as *using*

    TmpArray<u32> vecChildrenNamespaces;                        // All nested (children) namespaces, by index in file local Namespace repo
    TmpArray<u32> vecAllNamespacesInSameFileUsingSelf;          // All namespaces *in same file* currently using this one, by index in file local Namespace repo

    TmpMap<int, ValueBinding*> mapAccessibleBindingsInclUsing;  // this map gathers all accessible bindings, incl. those from used namespaces and enums.
    TmpMap<int, ValueBinding*> mapAllBindingsInclUsing;         // this map gathers all bindings, incl. those from used namespaces and enums, also incl. our privates.

    TmpMap<TCWaitingReason, TmpArray<TCContext*>> mapTasksWaitingForGlobalIds;    // all tasks waiting for a global id to be found from that particular context (or one of its parents)

    // Tasks remaining to be done before we can flag ourselves (and notify) as being done with either private or non-private declaration gathering
    // Note: Those are indifferently waiting, or ready to lauch, or being TC'd. Will be decremented on task completion.
    // => Can only be incremented when registering another wait => from a TC on same file => No lock needed !
    u32 uCountGlobalAccessibleTasks;
    u32 uCountGlobalPrivateTasks;

    TmpArray<TCContext*> vecTasksWaitingForCompletion;     // All tasks in same file waiting for this namespace's completion (assumed of private + accessible)

    i32 iPrimaryIdentifier;
    u32 _pad0;
};
static_assert(alignof(TCNamespace) >= 4, "TCNamespace alignment shall be at least 4 for usage as ScopedEntityHandle");


#endif // LOCLIB_NAMESPACE_H_
