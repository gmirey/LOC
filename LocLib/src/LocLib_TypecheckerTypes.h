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

#ifndef LOCLIB_TYPECHECKER_TYPES_H_
#define LOCLIB_TYPECHECKER_TYPES_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "../../HighPerfTools/Arrays.h"

#include "LocLib_Token.h"
#include "LocLib_PostParserTypes.h"
#include "LocLib_IR_Info.h"
#include "LocLib_NodeValue.h"
#include "LocLib_TypeInfoDecls.h"
#include "LocLib_Worker.h"

NodeValue* g_tReservedWordValues;

//#define MAX_IR_INSTRUCTIONS_COUNT   0x007FFFFFu   // 8M-1 ; leaving 'immediate' range untouched

enum ETypecheckContextKind : u8 {
    ECTXKIND_GLOBAL_PUBLIC,            // currently typechecking a root level at public scope ;
    ECTXKIND_GLOBAL_PACKAGE,           // currently typechecking a root level at package scope (default) ;
    ECTXKIND_GLOBAL_PRIVATE,           // currently typechecking a root level at private scope ;
    ECTXKIND_COMPOUND,                 // currently typechecking contents of a compound type declaration such as struct, union, enum...
    ECTXKIND_PROCBODY,                 // currently typechecking a regular proclike body
    ECTXKIND_INLINEEXP,                // currently typechecking an inlined proclike body (in the context of another, regular or inlined)
    ECTXKIND_MACROEXP,                 // currently typechecking a macro expansion (in any other context)
};

enum EScopeKind : u8 {
    SCOPEKIND_GLOBAL_PUBLIC,            // registration at global level, with public scope
    SCOPEKIND_GLOBAL_PACKAGE,           // registration at global level, with package scope
    SCOPEKIND_GLOBAL_PRIVATE,           // registration at global level, with private scope
    SCOPEKIND_COMPOUND,                 // registration as member of a compound type such as struct, union, enum...
    SCOPEKIND_PROC_PARAM,               // registration as proc param
    SCOPEKIND_PROC_BLOCK_LOCAL,         // registration as local var or const
};

// The current status of typechecking a proc-like body
enum EProcBodyTypechekingStatus : u32 {
    EPBTC_NOT_STARTED,
    EPBTC_BEING_EVALUATED,
    EPBTC_ON_HOLD,
    EPBTC_ON_ERROR,
    EPBTC_DONE,
    EPBTC_IS_FOREIGN,
};

// predecl
struct TCBaseSourceBlock;

// the equivalent of an 'AST' statement, as seen through the eyes of the typechecker...
struct TCStatement {

    // the following pointers are not *required* in the general case where we'd be able to retrieve statement
    // (resp child block) from indices, knowing AstBlock (resp source file block table)
    // ... yet, this being here will allow us to specify additional statements and blocks from macros, or from
    // desugaring, inlining, defers... all that good stuff

    AstStatement* pAstStatement;
    TCBaseSourceBlock* pChildBlock;

    // Similarily, "vecNodes" is here extensible, both in case of macros AND importantly, also in case
    //   of multi-ret param invocations, where a single invocation node turns into several result slots
    //   (each represented as a single 'node')

    TmpArray<TCNode*> vecNodes;
    TmpArray<NodeValue*> vecNodeValues;

    // cross-indexing for the purpose of error-messages reporting (and auto-completion helpers later ?).
    u32 uStatementIndexInBlock;
    u32 uLastIRorGlobalTCResult; // last IR position of this statement ; or if global: 0 not yet checked ; or 1+ETCResult;

    // Possibly used also for cross indexing... TODO: check if really in use
    u32 uBlockIndexInSourceFile;
    u32 iSourceFileIndex; // TODO: check if really in use
};

enum ETypecheckBlockKind : u8 {
    EBLOCKKIND_BASE,
    EBLOCKKIND_DECL,
    EBLOCKKIND_SEQ,
};

// the equivalent of an 'AST' block, as seen through the eyes of the typechecker...
struct TCNamespace;
struct TCBaseSourceBlock {
    int iSourceFileIndex;
    u32 uAstBlockIndex;
    AstBlock* pAstBlock;
    TCBaseSourceBlock* pParentBlock;
    TCBaseSourceBlock* pNextTcBlockAfterCurrentStatement;
    TCNamespace* pBlockIsDirectChildOfNamespace;
    TmpArray<TCStatement*> vecStatements;
    u32 uStatementBeingTypechecked;
    union {
        u32 _uBlockOpeningIRIffSeq;     // some IR position for a marker_jump_label at scope start
        u32 _uScopeLevelIffGlobal;      // the 'ETypecheckContextKind', assumed one of the 3 GLOBAL_* within this block
    };
};

// a specialization of an 'AST' block, for when it is known to be typechecked in a context allowing local declarations
struct TCDeclSourceBlock : public TCBaseSourceBlock {
    TmpMap<int, u32>* pMapBlockDeclarationsById;
};

#define BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND     0x0001u // positionned on 'uKindFlagsOfParentStatement' of the TCSeqSourceBlock instanciated as the child of an else (or elif) block.

// a specialization of an 'AST' block, for when it is known to be typechecked in the context of typechecking a proc-like body
struct TCSeqSourceBlock : public TCDeclSourceBlock {
    TmpArray<TCSeqSourceBlock*>* pVecDeferredBlocksInDeclOrder;
    TmpArray<u32>* pVecPlaceholdersToAfterBlockAndAfterElses; // general block exit ; and exit after when true in case block is an if (or elif)
    TmpArray<u32>* pVecPlaceholdersToElse;                  // in case block is child of an if (or elif), this is the vector to false
    u32 uIROfAfterBlock;                                    // after block emission, position of the next marker-jump target (considered as part of parent block)
    u32 uIRBeforeLoopConditionIfBlockIsLoop;                // in case block is child of a while (or for), this is the ir of the jump target to goto at block end
    u32 uIndexOfParentStatementInParentBlock;               // in case block is child, this is the index of its parent statement in parent TCSeqSourceBlock
    u32 uKindFlagsOfParentStatement;                        // in case block is child, holds flags for its *parent* statement, such as BLOCKFLAG_PARENT_STATEMENT_IS_ELSE_KIND
};

// the structure representing a whole proc-like body (only as a 'source' here => for IR emission and stuff, see 'TCProcBodyResult')
struct TCProcBodySource {
    const TypeInfo_ProcLike* procSign;      // may have parametric poly params declared in here, or even Ast refs if macro
    TmpArray<AstNode*> vecStaticInParams;   // sized as in-params from proc sign. null elems for common params. non-null elems for parapoly or Ast refs.
    TCSeqSourceBlock* pRootTcBlock;
    TCStatement* pStatementWithSignature;
};

// Runtime auto err-check kind
enum EErrCheckKind : u8 {
    ERR_CHECK_NONE = 0,

    ERR_CHECK_SIGNED_RESULT_OUT_OF_BOUNDS,
    ERR_CHECK_UNSIGNED_RESULT_OUT_OF_BOUNDS,
    ERR_CHECK_FP_RESULT_IN_INFINITY_FROM_FINITE,
    ERR_CHECK_FP_RESULT_IN_NAN_FROM_NON_NAN,
    ERR_CHECK_FP_RESULT_IN_UNDERFLOW_FROM_REGULAR,
    ERR_CHECK_NON_INTEGRAL_FP_TO_INTEGRAL,
    ERR_CHECK_FP_PARAM_SIGNALLING_NAN,
    ERR_CHECK_INT_DIVISION_BY_ZERO,
    ERR_CHECK_OPERAND_CANNOT_BE_NEGATIVE,
    ERR_CHECK_OPERAND_VALUE_TOO_LARGE,
    ERR_CHECK_INDEX_OUT_OF_RANGE,
    ERR_CHECK_UNKOWN_HASH_KEY,
    ERR_CHECK_UNGUARDED_OUT_OF_MEMORY,
    ERR_CHECK_UNGUARDED_OUT_OF_CAPACITY,
    ERR_CHECK_UNGUARDED_IO_FAILURE,
    ERR_CHECK_EXPLICIT_ASSERT,
    ERR_CHECK_REACHED_UNREACHABLE,
    ERR_CHECK_CALLER_CONTRACT_FAILED,
    ERR_CHECK_CALLEE_PROMISES_FAILED,
    ERR_CHECK_UNIT_TEST_FAILURE,
    ERR_CHECK_NULLPTR_ACCESS,
    ERR_CHECK_SANITIZER_UNALLOCATED_ACCESS,
    ERR_CHECK_SANITIZER_AFTER_FREE_ACCESS,
    ERR_CHECK_SANITIZER_READ_UNINITIALIZED,
    ERR_CHECK_PROC_SIGNALLED,
    ERR_CHECK_CAST_FORBIDS_NEGATIVE,
    ERR_CHECK_CAST_OUT_OF_BOUNDS,
    ERR_CHECK_PTR_CAST_MISALIGNED,
    ERR_CHECK_PTR_OFFSET_MISALIGNED,
};

// Represents a runtime auto err-check
struct LocalErrCheck {
    u32 uPosOfCheck;        // position of the IRIT_ERRCHK instruction (assumed in proc-local repo)
    u32 uPosOfInstr;        // (opt) position of the instruction which is being checked (assumed in proc-local repo)
    
    u32 uTokenRef;          // pivot token of node of expression which would be reported as in-error
    u32 uStatement;         // statement index in block where error occurred
    
    u32 uBlockIndex;        // source block index in file where error occurred
    int iSourceFile;        // source file index of code where error occurred
    
    u32 uFlagsAndKind;      // is active, is taken when non-zero, uKind on 8lsb
};

struct GraphResult;
// the structure representing a whole proc-like body (as a 'result' : for the typechecked nodes, see 'TCProcBodySource')
struct TCProcBodyResult {
    const TypeInfo_ProcLike* procSign;
    IRRepo procwiseRepo;
    TmpArray<ValueBinding*> vecBindings;    // all encountered block-local declarations are gathered here
    TmpArray<LocalErrCheck> vecErrChecks;   // keeps track of all IR indices and corresponding Ast where auto error-checks were inserted
    int iPrimaryIdentifier;
    u32 volatile uProcBodyTypechekingStatus;  // can also indicate 'foreign' status, in which case most of the other stuff is unused
    u32 uRegistrationIndex;         // cross indexing its own storage pos - in sourcefile scope until done and to global storage
    int iSourceFileIndex;
    u64 uIRofProcDecl;
    u64 uIsForeignSource;
    FFString foreignSymbolName;
    GraphResult* pGraphResult;
};

struct TCProcBodyRegistration {
    TCProcBodySource procSource;
    TCProcBodyResult procResult;
};

#define CTXFLAG_HALT_ON_NON_SUCCESS         0x0000'0001u  // typically false on root context, true on sequential
#define CTXFLAG_DECLARATIONS_ARE_LOCAL      0x0000'0002u  // typically false on root context, true on sequential
#define CTXFLAG_RESOLUTIONS_START_LOCAL     0x0000'0004u  // typically false on root context, true on sequential
#define CTXFLAG_IS_ENUM_BODY                0x0000'0008u  // typically false in all but enum bodies
#define CTXFLAG_ALLOW_RUNTIME               0x0000'0010u  // typically false on root context, true on sequential

struct LocalNodeInfoForResumedTask {
    TmpArray<u32> jumpsToTrue;
    TmpArray<u32> jumpsToFalse;
    u32 uIRofLocalDecl;
    u32 uIRofStoreToFalseForTernaryIf;
    bool bTCboolBinopReachedOperandB;
    bool bTCternaryIfReachedExprIfTrue;
    bool bTCternaryIfReachedExprIfFalse;
    u8 _pad0;
    u32 _pad1;
};

struct TCContext : public IRAwareContext {
    ETypecheckContextKind eKind;    // context kind, driving the additional data of current context instance.
    ETypecheckBlockKind eBlockKind; // block kind, to safely cast pCurrentBlock (and all of its parents, ensured in same context)
    EScopeKind eGlobalDeclScope;    // scope to use for globals declarations
    u8 uNestedExpansionLevel;       // should be non-zero if is_proc_expansion_context... and obviously checks if not too large
    u32 uFlags;                     // for fast-check of common properties of current context
    TCBaseSourceBlock* pCurrentBlock;   // current block to typecheck, may be a TCDeclSourceBlock or TCSeqSourceBlock depending on block kind
    TCNamespace* pNamespace;        // the active namespace, where globals are defined.

    //
    // the following if has_ctx_decl_blocks
    //

    // this is where local bindings are indexed, from map of id-to-index in decl-block (usually pointing to array within pProcResult)
    TmpArray<ValueBinding*>* pVecLocalBindings;

    // 
    // the following if is_ctx_with_proc_source
    //

    TCProcBodySource* pProcSource;
    // and for blocks with emitted IR (will serve as iteration for solving banches after the fact):
    TmpArray<TCSeqSourceBlock*> vecTypecheckedBlocks;

    //
    // the following if is_ctx_proc_expansion
    //

    TCContext* pParentContext;
    TmpArray<u32>* pVecOfGotoPlaceholdersToReturnPoint;
    // TODO: something to reference table of multi-expr for when expanding ?

    u32 uGlobalStatementOnHold;
    u8 bResumingCurrentStatement;
    u8 _pad0;
    u16 _pad1;
    TmpMap<u32, LocalNodeInfoForResumedTask*> mapLocalNodeInfoIfResumingCurrentStatement;

    //
    // the following if is_ctx_compound
    //

    TCCompoundRegistration* pCompoundToTC;

    u32 uNodeIndexWithWait;
    // TODO ? 
    //TCProcExpansionContext* pResumingToExpansion; // to use in the somewhat-edgey-case of resuming an inline or macro expansion task
};

local_func_inl bool is_ctx_global(const TCContext* pCtx) { return pCtx->eKind <= ECTXKIND_GLOBAL_PRIVATE; }
local_func_inl bool is_ctx_compound(const TCContext* pCtx) { return pCtx->eKind == ECTXKIND_COMPOUND; }
local_func_inl bool is_ctx_with_proc_source(const TCContext* pCtx) { return pCtx->eKind >= ECTXKIND_PROCBODY; }
local_func_inl bool is_ctx_proc_expansion(const TCContext* pCtx) { return pCtx->eKind >= ECTXKIND_INLINEEXP; }
local_func_inl bool is_ctx_regular_proc_body(const TCContext* pCtx) { return pCtx->eKind == ECTXKIND_PROCBODY; }

local_func_inl bool has_ctx_decl_blocks(const TCContext* pCtx) { return pCtx->eBlockKind >= EBLOCKKIND_DECL; }
local_func_inl bool has_ctx_seq_blocks(const TCContext* pCtx) { return pCtx->eBlockKind == EBLOCKKIND_SEQ; }

local_func_inl SourceFileDescAndState* get_tc_ctx_ensured_isolated_file(TCContext* pCtx) {
    return pCtx->pIsolatedSourceFile;
}

local_func_inl WorkerDesc* get_tc_ctx_worker(TCContext* pCtx) {
    return pCtx->pWorker;
}

local_func_inl bool does_tc_ctx_allow_runtime(TCContext* pCtx) {
    return pCtx->uFlags & CTXFLAG_ALLOW_RUNTIME;
}

enum EDeclAttributes {
    EDECLATTR_REGULAR_VAR,
    EDECLATTR_REGULAR_CONST,
    /*
    EDECLATTR_ENUM_CONST,
    EDECLATTR_STRUCT_VAR,
    EDECLATTR_STRUCT_CONST,
    */
};

local_func_inl bool does_tc_ctx_require_declare_as_local(TCContext* pCtx, EDeclAttributes attr) {
    Assert(attr == EDeclAttributes::EDECLATTR_REGULAR_CONST || attr == EDeclAttributes::EDECLATTR_REGULAR_VAR,
        "decl attribute kind not yet implemented"); // TODO
    return pCtx->uFlags & CTXFLAG_DECLARATIONS_ARE_LOCAL;
}

enum EIdentAttributes {
    EIDENTATTR_REGULAR,
};

local_func_inl bool does_tc_ctx_resolve_identifiers_locally(TCContext* pCtx, EIdentAttributes attr) {
    Assert(attr == EIdentAttributes::EIDENTATTR_REGULAR,
        "ident attribute kind not yet implemented"); // TODO
    return pCtx->uFlags & CTXFLAG_RESOLUTIONS_START_LOCAL;
}

local_func_inl SourceFileDescAndState* get_tc_global_resolution_file(TCContext* pCtx, EIdentAttributes attr) {
    Assert(attr == EIdentAttributes::EIDENTATTR_REGULAR,
        "ident attribute kind not yet implemented"); // TODO
    return pCtx->pIsolatedSourceFile; // TODO: check if always the case ?
}

local_func_inl TmpArray<ValueBinding*>* get_tc_ctx_local_resolution_registry(TCContext* pCtx, EIdentAttributes attr) {
    Assert(attr == EIdentAttributes::EIDENTATTR_REGULAR,
        "ident attribute kind not yet implemented"); // TODO
    Assert_(has_ctx_decl_blocks(pCtx));
    return pCtx->pVecLocalBindings;
}

local_func_inl TCProcBodyResult* get_tc_ctx_proc_result(TCContext* pCtx) {
    Assert_(pCtx->pProcResult);
    return pCtx->pProcResult;
}

local_func_inl TCDeclSourceBlock* get_tc_ctx_local_declaration_block(TCContext* pCtx, EDeclAttributes attr) {
    Assert(attr == EDeclAttributes::EDECLATTR_REGULAR_CONST || attr == EDeclAttributes::EDECLATTR_REGULAR_VAR,
        "decl attribute kind not yet implemented"); // TODO
    Assert_(has_ctx_decl_blocks(pCtx));
    return (TCDeclSourceBlock*)pCtx->pCurrentBlock;
}

local_func_inl TmpArray<ValueBinding*>* get_tc_ctx_local_declaration_registry(TCContext* pCtx, EDeclAttributes attr) {
    Assert(attr == EDeclAttributes::EDECLATTR_REGULAR_CONST || attr == EDeclAttributes::EDECLATTR_REGULAR_VAR,
        "decl attribute kind not yet implemented"); // TODO
    Assert_(has_ctx_decl_blocks(pCtx));
    return pCtx->pVecLocalBindings;
}

local_func_inl SourceFileDescAndState* get_tc_global_declaration_file(TCContext* pCtx, EDeclAttributes attr) {
    Assert(attr == EDeclAttributes::EDECLATTR_REGULAR_CONST || attr == EDeclAttributes::EDECLATTR_REGULAR_VAR,
        "decl attribute kind not yet implemented"); // TODO
    return pCtx->pIsolatedSourceFile; // TODO: check if always the case ?
}

local_func_inl bool does_tc_ctx_allow_shadowing_globals(TCContext* pCtx, EDeclAttributes attr) {
    Assert(attr == EDeclAttributes::EDECLATTR_REGULAR_CONST || attr == EDeclAttributes::EDECLATTR_REGULAR_VAR,
        "decl attribute kind not yet implemented"); // TODO
    return false;
}

local_func_inl bool does_tc_ctx_allow_shadowing_locals(TCContext* pCtx, EDeclAttributes attr) {
    Assert(attr == EDeclAttributes::EDECLATTR_REGULAR_CONST || attr == EDeclAttributes::EDECLATTR_REGULAR_VAR,
        "decl attribute kind not yet implemented"); // TODO
    return false;
}

local_func_inl bool should_tc_ctx_halt_on_non_success(TCContext* pCtx) {
    return pCtx->uFlags & CTXFLAG_HALT_ON_NON_SUCCESS;
}

// Macro-magic-emitter... cf "A Note on enums-and-structs synchronization"
// use together with    #define IMPLICIT_MEMBER(syntaxName, name)
#define IMPLICIT_MEMBERS_EMITTER_ \
        \
    IMPLICIT_MEMBER("length_in_bytes", LENGTH_IN_BYTES)     /* length_in_bytes of an instance of a string type */ \
    IMPLICIT_MEMBER("ptr_to_bytes", PTR_TO_BYTES)           /* ptr_to_bytes of an instance of a string type */ \
    IMPLICIT_MEMBER("flags", FLAGS)                         /* u32 flags of an instance of a string type */ \
    IMPLICIT_MEMBER("bytes", BYTES)                         /* bytes slice of an instance of a string type */ \
        \
    IMPLICIT_MEMBER("alloc", ALLOC)                         /* allocator of an instance of a non-FF string type, or other types ith alloc */ \
    IMPLICIT_MEMBER("size", SIZE)                           /* size of a container such as slice, array, map or set */ \
    IMPLICIT_MEMBER("cur_alloc_size", CUR_ALLOC_SIZE)       /* current capacity of a resizable container such as array, map or set */ \
    IMPLICIT_MEMBER("data_ptr", DATA_PTR)                   /* pointer to data for an array */ \


enum EImplicitMembers {
    
    INDEX_BEFORE_IMPLICIT_MEMBERS_ = COUNT_RESERVED_WORDS - 1u,

    #define IMPLICIT_MEMBER(syntaxName, name)   EIMPLICIT_MEMBER_ ## name ,
        IMPLICIT_MEMBERS_EMITTER_
    #undef IMPLICIT_MEMBER
};

// Macro-magic... cf "A Note on enums-and-structs synchronization"
constexpr char const* tImplicitMembersStr[] = {
#define IMPLICIT_MEMBER(syntaxName, name)       syntaxName ,
    IMPLICIT_MEMBERS_EMITTER_
#undef IMPLICIT_MEMBER
};
static const size_t COUNT_IMPLICIT_MEMBERS = sizeof(tImplicitMembersStr) / sizeof(char*);


#endif // LOCLIB_TYPECHECKER_TYPES_H_

