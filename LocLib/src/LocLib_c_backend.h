#pragma once 

#ifndef LOCLIB_C_BACKEND_H_
#define LOCLIB_C_BACKEND_H_

#include "../../HighPerfTools/BaseDecls.h"
#include "../../HighPerfTools/Arenas.h"
#include "LocLib_Cmd_API.h"

#include "LocLib_TokenizerEnums.h"
#include "LocLib_Token.h"
#include "LocLib_ScanAndTok.h"
#include "LocLib_ErrorEnums.h"
#include "LocLib_ProgramState.h"
#include "LocLib_PreParserTypes.h"
#include "LocLib_DebugPrint.h"
#include "LocLib_PreParser.h"
#include "LocLib_PostParserTypes.h"
#include "LocLib_Postparser.h"
#include "LocLib_TypeChecker.h"
#include "LocLib_IR.h"

#include "../../HighPerfTools/arithmetic_operations.h"

//#define FUNC_RET_VALUE_NAME                 "retValue__"
#define ERR_CHECK_SPECIAL_VAR_NAME          "lastErrChecked__"
#define ERR_CHECK_FUNCTION_NAME             "on_error__"
#define ERR_REPORT_FUNC_NAME                "on_error_report__"
//#define ERR_REPORT_FUNC_NAME_NO_AST_INFO    "on_error_report_without_ast_info__"
#define ERR_CHECK_FAILED_LABEL_NAME         "on_err_check_failed__"

/*
__declspec(align(8)) struct CBckProcSignDef {
    u32 tableValues[32];
};

struct CBckProcSignDefHandle {
    CBckProcSignDef* pDef;
};

template<>
local_func u64 get_map_hash<CBckProcSignDefHandle>(CBckProcSignDefHandle signDef)
{
    u32 uParamsCount = signDef.pDef->tableValues[0] & 0x0000'001Fu;
    u32 uCount64 = (2u + uParamsCount) >> 1; // +1 from first slot, +1 for odd before shift
    return getFullHashFromBytes_<true>((const u8*)signDef.pDef->tableValues, uCount64 << 3, MAP_HASH_SEED);
};

template<>
local_func bool are_equal_keys<CBckProcSignDefHandle, CBckProcSignDefHandle>(CBckProcSignDefHandle signDefA, CBckProcSignDefHandle signDefB)
{
    if (signDefA.pDef == signDefB.pDef)
        return true;
    const u32* pA = signDefA.pDef->tableValues;
    const u32* pB = signDefB.pDef->tableValues;
    for (u32 i = 0; i < 32; i++, pA++, pB++)
        if ((*pA) != (*pB))
            return false;
    return true;
};

#define INVALID_FORMAT32 0xFFFF'FFFFu
*/

struct CBckProcBody {
    TCProcBodyResult* pProcResult;
    //CBckProcSignDefHandle procSignDef;
    StringView strProcName;
    TmpArray<u32> vecOfLocalConstDeclToEmit;
    u32 uTotalParamsCount;
    u32 uCurrentInstruction;
};

struct CBckContext : public CompilationContext {
    TmpArray<u64> vecOfDeclarationsToEmit;
    TmpArray<u64> vecOfDeclarationsOnHold;
    TmpArray<u64> vecOfProcBodiesToEmit;
    PlatformFileHandle fileForGlobalDecls;
    PlatformFileHandle fileForProcBodies;
    Arena secondaryTmpArena;
};

const char* tTypeIdentifiers[128u] = {
    // scalars, integral
    "r8",
    "r16",
    "r32",
    "r64",
    "r128",
    "r256",
    "r512",
    "r1024",

    // scalars, fp
    "_invalid_f8",
    "_f16",
    "_f32",
    "_f64",
    "_f128",
    "_f256",
    "_xfloat",              // not yet implemented this way... TODO ?
    "_invalid_f1024",

    // vec2, integral
    "r8x2",
    "r16x2",
    "r32x2",
    "r64x2",
    "r128x2",
    "r256x2",
    "r512x2",
    "_invalid_r1024x2",

    // vec2, fp
    "_invalid_f8x2",
    "_f16x2",
    "_f32x2",
    "_f64x2",
    "_f128x2",
    "_f256x2",
    "_invalid_xfloatx2",
    "_invalid_f1024x2",

    // vec4, integral
    "r8x4",
    "r16x4",
    "r32x4",
    "r64x4",
    "r128x4",
    "r256x4",
    "_invalid_r512x4",
    "_invalid_r1024x4",

    // vec4, fp
    "_invalid_f8x4",
    "_f16x4",
    "_f32x4",
    "_f64x4",
    "_f128x4",
    "_f256x4",
    "_invalid_xfloatx4",
    "_invalid_f1024x4",

    // vec8, integral
    "r8x8",
    "r16x8",
    "r32x8",
    "r64x8",
    "r128x8",
    "_invalid_r256x8",
    "_invalid_r512x8",
    "_invalid_r1024x8",

    // vec8, fp
    "_invalid_f8x8_compint",    // compint should not appear in backend
    "_f16x8",
    "_f32x8",
    "_f64x8",
    "_f128x8",
    "_invalid_f256x8",
    "_invalid_xfloatx8",
    "_invalid_f1024x8",

    // vec16, integral
    "r8x16",
    "r16x16",
    "r32x16",
    "r64x16",
    "_invalid_r128x16",
    "_invalid_r256x8",
    "_invalid_r512x8",
    "_invalid_r1024x8",

    // vec16, fp
    "_invalid_f8x16",
    "_f16x16",
    "_f32x16",
    "_f64x16",
    "_invalid_f128x16",
    "_invalid_f256x16",
    "_invalid_xfloatx16",
    "_invalid_f1024x16",

    // vec32, integral
    "r8x32",
    "r16x32",
    "r32x32",
    "_invalid_r64x32",
    "_invalid_r128x16",
    "_invalid_r256x8",
    "_invalid_r512x8",
    "_invalid_r1024x8",

    // vec32, fp
    "_invalid_f8x32",
    "_f16x32",
    "_f32x32",
    "_invalid_f64x32",
    "_invalid_f128x32",
    "_invalid_f256x32",
    "_invalid_xfloatx32",
    "_invalid_f1024x32",

    // vec64, integral
    "r8x64",
    "r16x64",
    "_invalid_r32x64",
    "_invalid_r64x64",
    "_invalid_r128x16",
    "_invalid_r256x8",
    "_invalid_r512x8",
    "_invalid_r1024x8",

    // vec64, fp
    "_invalid_f8x64",
    "_f16x64",
    "_invalid_f32x64",
    "_invalid_f64x64",
    "_invalid_f128x64",
    "_invalid_f256x64",
    "_invalid_xfloatx64",
    "_invalid_f1024x64",

    // vec128, integral
    "r8x128",
    "_invalid_r16x128",
    "_invalid_r32x64",
    "_invalid_r64x64",
    "_invalid_r128x16",
    "_invalid_r256x8",
    "_invalid_r512x8",
    "_invalid_r1024x8",

    // vec128, fp
    "_invalid_f8x128",
    "_invalid_f16x128",
    "_invalid_f32x128",
    "_invalid_f64x128",
    "_invalid_f128x128",
    "_invalid_f256x128",
    "_invalid_xfloatx128",
    "_invalid_f1024x128",

};

local_func_inl const char* c_backend_get_type_identifier_for_format(u8 uFormat)
{
    Assert(uFormat < 0x80u, "higher than 128-slots-vec not supposed to exist");
    return tTypeIdentifiers[uFormat];
}

const char* tTmpPrintKindIdentifiers[COUNT_PRINT_KINDS] = {
    "uint", //EPRINTKIND_UNSIGNED,
    "sint", //EPRINTKIND_SIGNED,
    "raw", //EPRINTKIND_RAW,
    "rawptr", //EPRINTKIND_RAWPTR,
    "codepoint", //EPRINTKIND_CODEPOINT,
    "bool", //EPRINTKIND_BOOL,
    "eol", //EPRINTKIND_EOL,
};

local_func_inl const char* c_backend_get_tmp_print_kind(u8 uTmpPrintKind)
{
    Assert(uTmpPrintKind < COUNT_PRINT_KINDS, "unexpected 'print kind' value");
    return tTmpPrintKindIdentifiers[uTmpPrintKind];
}

local_func_inl const char* c_backend_get_op_name(u8 uOp)
{
    Assert_(uOp < COUNT_IRIT_INSTRUCTIONS);
    return tIRITStr[uOp];
}

local_func_inl const TypeInfo* std_type_from_ir_format(u8 uFormat) {
    Assert(uFormat < 0x80u, "higher than 128-slots-vec not supposed to exist");
    if (uFormat <= 0x05u)
        return g_pCoreTypesInfo[ECORETYPE_U8 + uFormat];
    else if (uFormat <= 0x07u)
        return g_pCoreTypesInfo[ECORETYPE_R8 + uFormat];
    else if (uFormat <= 0x0Fu) {
        Assert(uFormat != 0x08u, "f8 does not exist");
        Assert(uFormat <= 0x0Du, "fp formats higher than f256 do not exist");
        return g_pCoreTypesInfo[ECORETYPE_F16 + uFormat - 9u];
    } else {
        Assert(false, "std_type_from_ir_format : non-scalars not yet implemented");
    }
}

local_func void c_backend_emit_name_of_proc_to_buffer(char* szBuffer, TCProcBodyResult* pProcResult,
    CompilationContext* pCompilationContext)
{
    int iPrimaryId = pProcResult->iPrimaryIdentifier;
    sprintf(szBuffer, "file%u__proc%u__%s",
        u32(pProcResult->iSourceFileIndex), pProcResult->uRegistrationIndex, iPrimaryId > 0 ?
            get_identifier_string(pCompilationContext->pProgCompilationState, iPrimaryId).c_str() : "_noname_");
}

local_func bool c_backend_on_reference_append_or_return_is_already_marked(u64 uIRofDecl, TCProcBodyResult* pIfProc, CBckContext* pBckContext)
{
    Assert_(ir_is_valid_param(uIRofDecl));
    Assert_(!ir_is_immediate(uIRofDecl));
    u16 uLocation = ir_get_repo_id(uIRofDecl);
    IRRepo* pRepo;
    if (uLocation < IR_REPO_FILE_OFFSET) {
        pRepo = pBckContext->pProgCompilationState->tPrograwiseRepos + uLocation;
    } else if (uLocation < IR_REPO_CURRENT_PROC) {
        u32 uFileIndex = u32(uLocation - IR_REPO_FILE_OFFSET);
        pRepo = &(pBckContext->pProgCompilationState->vecSourceFiles[uFileIndex]->filewiseRepo);
    } else if (uLocation == IR_REPO_CURRENT_PROC) {
        Assert_(pIfProc);
        pRepo = &(pIfProc->procwiseRepo);
    } else { Assert_(uLocation == IR_REPO_TEMPORARY);
        Assert(false, "temporary repo not supported here");
    }
    u32 uPos = ir_get_pos_in_repo(uIRofDecl);
    IREntry& entry = ir_access_repo_instr(pRepo, uPos);
    if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION) {
        if (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT) {
            return true;
        } else {
            pBckContext->vecOfDeclarationsToEmit.append(uIRofDecl);
            entry.uInstrMetaFlagsAndSecondParam |= IRFLAG_BCK_TO_EMIT;
            return false;
        }
    } else {
        // TODO ??
        Assert(false, "non-declaration not supported here");
    }
    return false;
}

#define BCK_FLAG_DECL_IS_LOCAL_CONST 0x0080'0000uLL
#define BCK_MASK_PROC_REGISTRATION_WHEN_LOCAL_CONST 0x007F'FFFFu

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_try_emit_proc_sign_to(char* szBuffer, const TypeInfo_ProcLike* procSign,
    const char* szName, CBckContext* pBckContext)
{
    u32 uTotalParamsCount = procSign->params.size();
    u8 uInParamCount = get_input_param_count(procSign);
    Assert_(uTotalParamsCount < 32);
    Assert_(uInParamCount <= uTotalParamsCount);
    u8 uProcKind = get_proc_kind(procSign);
    u32 uRetParamCount = uTotalParamsCount - uInParamCount;
    u32 uFirstAdditionalRet = uInParamCount + 1u;
    u32 uRemainingRetParamCount = 0;
    if (uRetParamCount) {
        uRemainingRetParamCount = uRetParamCount-1u;
        const TypeInfo* pFirstRetParamType = procSign->params[uInParamCount].pBinding->pType;
        u32 uFirstRetParamSlotCount = get_slots_count(pFirstRetParamType);
        if (LIKELY(uFirstRetParamSlotCount)) {
            u32 uFirstRetParamAlignPow2 = get_log2_of_align_bytes(pFirstRetParamType);
            if (uFirstRetParamAlignPow2 <= 3u) {
                u8 uFirstRetParamFormat = get_ir_format(pFirstRetParamType);
                if (uFirstRetParamSlotCount == 1u) { // nominal retparam: declared as the 'return type' of a c function
                    sprintf(szBuffer, "%s %s(", c_backend_get_type_identifier_for_format(uFirstRetParamFormat), szName);

                } else { // more than 1 slot => retparam converted to additional in-ptr
                    uFirstAdditionalRet = -1u;
                    uRemainingRetParamCount += 1u;
                    sprintf(szBuffer, "void %s(", szName);
                }
            } else { // greater than 64b align => retparam converted to additional in-ptr
                uFirstAdditionalRet = -1u;
                uRemainingRetParamCount += 1u;
                sprintf(szBuffer, "void %s(", szName);
            }
        } else { // no-slots : special '_DummyEmptyStruct' representing any void type
            sprintf(szBuffer, "_DummyEmptyStruct %s(", szName);
        }

    } else { // no retparam: this is our equivalent of the c 'void' (unless our void-types)
        sprintf(szBuffer, "void %s(", szName);
    }

    for (u32 uInParam = 0; uInParam < uInParamCount; uInParam++) {
        const TypeInfo* pInParamType = procSign->params[uInParam].pBinding->pType;
        u32 uInParamSlotsCount = get_slots_count(pInParamType);
        if (LIKELY(uInParamSlotsCount)) {
            u32 uInParamAlignPow2 = get_log2_of_align_bytes(pInParamType);
            if (uInParamAlignPow2 <= 3u) {
                u8 uInParamIRformat = get_ir_format(pInParamType);
                if (uInParamSlotsCount == 1u) { // nominal inparam : declared as a standard c function param
                    sprintf(szBuffer + strlen(szBuffer), "%s inP%u%s",
                        c_backend_get_type_identifier_for_format(uInParamIRformat), uInParam,
                        uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
                } else { // more than 1 slot => inparam forced to pass-by-address
                    // TODO: declare and use some struture instead... mind alignment ?
                    sprintf(szBuffer + strlen(szBuffer), "%s* inRP%u%s",
                        c_backend_get_type_identifier_for_format(uInParamIRformat), uInParam,
                        uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
                }
            } else { // greater than 64b align => inparam forced to pass-by-address
            sprintf(szBuffer + strlen(szBuffer), "_DummyEmptyStruct inP%u%s", uInParam,
                uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
            }
        } else {
            sprintf(szBuffer + strlen(szBuffer), "_DummyEmptyStruct inP%u%s", uInParam,
                uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
        }
    }

    for (u32 uAddRetParam = uFirstAdditionalRet; uAddRetParam < uTotalParamsCount; uAddRetParam++) {
        const TypeInfo* pRetParamType = procSign->params[uAddRetParam].pBinding->pType;
        u32 uRetParamSlotsCount = get_slots_count(pRetParamType);
        if (LIKELY(uRetParamSlotsCount)) {
            u8 uRetParamFormat = get_ir_format(pRetParamType);
            sprintf(szBuffer + strlen(szBuffer), "%s* outRP%u%s",
                c_backend_get_type_identifier_for_format(uRetParamFormat), uAddRetParam,
                uAddRetParam < uTotalParamsCount-1u ? ", ":"");
        } else {
            sprintf(szBuffer + strlen(szBuffer), "_DummyEmptyStruct* outRP%u%s", uAddRetParam,
                uAddRetParam < uTotalParamsCount-1u ? ", ":"");
        } 
    }

    sprintf(szBuffer + strlen(szBuffer), ")");
    return true;
}

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_emit_simple_meta_value_to(char* pBuffer, u8 uFormat, u32 uAlignPow2,
    u32 uConstFlags, MetaValueIR metaValue, TCProcBodyResult* pIfProcBody, CBckContext* pCBckContext)
{
    if (0 == (uConstFlags & IRFLAG_IS_NYKA)) {
        Assert_(uConstFlags & IRFLAG_IS_CONST);
        if (uFormat == COMPINT_PSEUDO_IR_FORMAT) {
            Assert(false, "compint not supported ?");
        } else if (0 == (uFormat & 0xF0u)) {
            if (0 == (uFormat & 0x08u)) {
                Assert_(uAlignPow2 <= 3u);
                Assert_(uFormat < 0x03u);
                Assert_(uConstFlags & IRFLAG_IS_CONST_EMBEDDED);
                u64 uEmbedded = metaValue.knownValue.uEmbeddedValue;
                switch (uFormat) {
                    case 0x00u: { // r8
                        sprintf(pBuffer, "0x%02Xu", u8(uEmbedded));
                    } break;
                    case 0x01u: { // r16
                        sprintf(pBuffer, "0x%04Xu", u16(uEmbedded));
                    } break;
                    case 0x02u: { // r32
                        sprintf(pBuffer, "0x%08Xu", u32(uEmbedded));
                    } break;
                    case 0x03u: { // r64
                        sprintf(pBuffer, "0x%X'%08XuLL", u32(uEmbedded >> 32), u32(uEmbedded));
                    } break;
                    default:
                        Assume_(false);
                }
            } else {
                // TODO
                sprintf(pBuffer, "non_integral_value_not_yet_impl");
            }
        } else {
            // TODO
            sprintf(pBuffer, "vector_value_not_yet_impl");
        }
    } else {
        // TODO
        sprintf(pBuffer, "nyka_not_yet_impl");
    }
    return true;
}

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_emit_table_meta_value_to(char* pBuffer, u8 uFormat, u32 uAlignPow2, u32 uSlotsCount,
    u32 uConstFlags, MetaValueIR metaValue, TCProcBodyResult* pIfProcBody, CBckContext* pCBckContext)
{
    Assert_(uSlotsCount);
    // TODO
    sprintf(pBuffer, "table_value_not_yet_impl");
    return true;
}

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_try_emit_local_const_decl(u32 uPos, TCProcBodyResult* pProcResult, CBckContext* pCBckContext)
{
    IREntry& entry = ir_access_repo_instr(&(pProcResult->procwiseRepo), uPos);

    Assert_(entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT);
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_EMITTED));

    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION);
    Assert_(entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST);

    u8 uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 8);
    u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);

    char szTmp[2048];
    if (uSlotsCount) {
        const char* szType = c_backend_get_type_identifier_for_format(uFormat);
        if (uSlotsCount == 1u && uAlignPow2 <= 3u) {
            sprintf(szTmp, "constexpr %s file%u_proc%u_const%u = ", szType,
                u32(pProcResult->iSourceFileIndex), pProcResult->uRegistrationIndex, uPos);
            c_backend_emit_simple_meta_value_to(szTmp + strlen(szTmp), uFormat, uAlignPow2,
                u32(entry.uInstrMetaFlagsAndSecondParam), entry.metaValue, pProcResult, pCBckContext);
        } else {
            sprintf(szTmp, "constexpr %s file%u_proc%u_constR%u[] = ", szType,
                u32(pProcResult->iSourceFileIndex), pProcResult->uRegistrationIndex, uPos);
            c_backend_emit_table_meta_value_to(szTmp + strlen(szTmp), uFormat, uAlignPow2, uSlotsCount,
                u32(entry.uInstrMetaFlagsAndSecondParam), entry.metaValue, pProcResult, pCBckContext);
        }
        sprintf(szTmp + strlen(szTmp), ";\n");
    } else {
        sprintf(szTmp, "constexpr _DummyEmptyStruct file%u_proc%u_const%u = {};\n",
            u32(pProcResult->iSourceFileIndex), pProcResult->uRegistrationIndex, uPos);
    }
    sprintf(szTmp, "constexpr _not_yet_impl_local_decl_ file%u_proc%u_const%u;\n",
        u32(pProcResult->iSourceFileIndex), pProcResult->uRegistrationIndex, uPos);
    platform_write_to_file(pCBckContext->fileForGlobalDecls, (const u8*)szTmp, strlen(szTmp));

    return true;
}

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_try_emit_filewise_decl(u32 uPos, SourceFileDescAndState* pSourceFile, CBckContext* pCBckContext)
{
    IREntry& entry = ir_access_repo_instr(&(pSourceFile->filewiseRepo), uPos);

    Assert_(entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT);
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_EMITTED));

    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION);

    u8 uFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 8);
    u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);

    // TODO !!!
    char szTmp[2048];
    if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST) {
        if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_PROCBODY) {
            Assert_(uSlotsCount);
            u32 uProcRegistration = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
            TCProcBodyRegistration* pProcRegistration = pSourceFile->vecAllProcBodies[uProcRegistration];
            char szName[1024];
            c_backend_emit_name_of_proc_to_buffer(szName, &(pProcRegistration->procResult), pCBckContext);
            sprintf(szTmp, "static ");
            if (!c_backend_try_emit_proc_sign_to(szTmp + strlen(szTmp), pProcRegistration->procResult.procSign, szName, pCBckContext))
                return false;
            sprintf(szTmp + strlen(szTmp), ";\n");
            pCBckContext->vecOfProcBodiesToEmit.append(pProcRegistration->procResult.uIRofProcDecl);

        } else if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_RTTI) {
            Assert_(uSlotsCount);
            sprintf(szTmp, "constexpr _not_yet_impl_filewise_rtti_decl_ file%u_const_%u;\n",
                u32(pSourceFile->iRegistrationIndex), uPos);
        } else {
            if (uSlotsCount) {
                const char* szType = c_backend_get_type_identifier_for_format(uFormat);
                if (uSlotsCount == 1u && uAlignPow2 <= 3u) {
                    sprintf(szTmp, "constexpr %s file%u_const%u = ", szType,
                        u32(pSourceFile->iRegistrationIndex), uPos);
                    c_backend_emit_simple_meta_value_to(szTmp + strlen(szTmp), uFormat, uAlignPow2,
                        u32(entry.uInstrMetaFlagsAndSecondParam), entry.metaValue, 0, pCBckContext);
                } else {
                    sprintf(szTmp, "constexpr %s file%u_constR%u[] = ", szType,
                        u32(pSourceFile->iRegistrationIndex), uPos);
                    c_backend_emit_table_meta_value_to(szTmp + strlen(szTmp), uFormat, uAlignPow2, uSlotsCount,
                        u32(entry.uInstrMetaFlagsAndSecondParam), entry.metaValue, 0, pCBckContext);
                }
                sprintf(szTmp + strlen(szTmp), ";\n");
            } else {
                sprintf(szTmp, "constexpr _DummyEmptyStruct file%u_const%u = {};\n",
                    u32(pSourceFile->iRegistrationIndex), uPos);
            }
        }
    } else {
        if (uSlotsCount) {
            const char* szType = c_backend_get_type_identifier_for_format(uFormat);
            if (uSlotsCount == 1u && uAlignPow2 <= 3u) {
                sprintf(szTmp, "constexpr %s file%u_var%u = ", szType,
                    u32(pSourceFile->iRegistrationIndex), uPos);
                c_backend_emit_simple_meta_value_to(szTmp + strlen(szTmp), uFormat, uAlignPow2,
                    u32(entry.uInstrMetaFlagsAndSecondParam), entry.metaValue, 0, pCBckContext);
            } else {
                sprintf(szTmp, "constexpr %s file%u_varR%u[] = ", szType,
                    u32(pSourceFile->iRegistrationIndex), uPos);
                c_backend_emit_table_meta_value_to(szTmp + strlen(szTmp), uFormat, uAlignPow2, uSlotsCount,
                    u32(entry.uInstrMetaFlagsAndSecondParam), entry.metaValue, 0, pCBckContext);
            }
            sprintf(szTmp + strlen(szTmp), ";\n");
        } else {
            sprintf(szTmp, "static _DummyEmptyStruct file%u_var%u;\n",
                u32(pSourceFile->iRegistrationIndex), uPos);
        }
    }

    platform_write_to_file(pCBckContext->fileForGlobalDecls, (const u8*)szTmp, strlen(szTmp));
    return true;
}

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_try_emit_programwise_decl(u32 uPos, u16 uLocation, CBckContext* pCBckContext)
{
    Assert_(uLocation < IR_REPO_FILE_OFFSET);
    IREntry& entry = ir_access_repo_instr(pCBckContext->pProgCompilationState->tPrograwiseRepos + uLocation, uPos);

    Assert_(entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT);
    Assert_(0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_EMITTED));

    Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION);
    Assert_(entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST);

    u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);

    // TODO !!!
    char szTmp[2048];
    if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_RTTI) {
        sprintf(szTmp, "constexpr _not_yet_impl_programwise_rtti_decl_ programwise_const_%u;\n", uPos | (u32(uLocation) << 24));
    } else {
        Assert_(0 == (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_PROCBODY));
        sprintf(szTmp, "constexpr _not_yet_impl_programwise_const_decl_ programwise_const_%u;\n", uPos | (u32(uLocation) << 24));
    }
    platform_write_to_file(pCBckContext->fileForGlobalDecls, (const u8*)szTmp, strlen(szTmp));

    return true;
}

// returns false when try failed **for the reason of requiring another declaration beforehand** (thus when should be placed 'on hold')
local_func bool c_backend_try_emit_declaration(u64 uIROfDecltoEmit, CBckContext* pCBckContext)
{
    Assert_(!ir_is_immediate(uIROfDecltoEmit & IR_STD_PARAM_MASK));
    u16 uLocation = ir_get_repo_id(uIROfDecltoEmit);
    u32 uPos = ir_get_pos_in_repo(uIROfDecltoEmit);
    Assert_(uLocation < IR_REPO_CURRENT_PROC);
    if (uIROfDecltoEmit & BCK_FLAG_DECL_IS_LOCAL_CONST) {
        Assert_(uLocation >= IR_REPO_FILE_OFFSET);
        u32 uFileIndex = u32(uLocation - IR_REPO_FILE_OFFSET);
        u32 uProcRegistration = u32(uIROfDecltoEmit) & BCK_MASK_PROC_REGISTRATION_WHEN_LOCAL_CONST;
        SourceFileDescAndState* pSourceFile = pCBckContext->pProgCompilationState->vecSourceFiles[uFileIndex];
        TCProcBodyRegistration* pRegistration = pSourceFile->vecAllProcBodies[uProcRegistration];
        return c_backend_try_emit_local_const_decl(uPos, &(pRegistration->procResult), pCBckContext);
    } else {
        if (uLocation >= IR_REPO_FILE_OFFSET) {
            u32 uFileIndex = u32(uLocation - IR_REPO_FILE_OFFSET);
            SourceFileDescAndState* pSourceFile = pCBckContext->pProgCompilationState->vecSourceFiles[uFileIndex];
            return c_backend_try_emit_filewise_decl(uPos, pSourceFile, pCBckContext);
        } else {
            return c_backend_try_emit_programwise_decl(uPos, uLocation, pCBckContext);
        }
    }
}

#if 0 // TMP TMP TMP

local_func void c_backend_emit_proc_body(CBckProcBody* pProcBody, CBckContext* pCBckContext)
{
    // TODO !!!
    char szTmp[2048];
    sprintf(szTmp, "static _not_yet_impl_proc_decl_ ");
    c_backend_emit_name_of_proc_to_buffer(szTmp + strlen(szTmp), pProcBody->pProcResult, pCBckContext);
    sprintf(szTmp + strlen(szTmp), "(_proc_params_not_yet_impl_either_) {\n}\n\n");
    platform_write_to_file(pCBckContext->fileForProcBodies, (const u8*)szTmp, strlen(szTmp));
}


local_func_inl u32 c_backend_get_format32_from_data(u32 uAlignPow2, u32 uSlotsCount, u8 uIRFormat, CBckContext* pBckContext)
{
    Assert_(uAlignPow2 <= 12u);
    Assert_(uSlotsCount < (1u << 21));
    Assert_(uIRFormat < 0x80u);
    if (uSlotsCount)
        return uIRFormat | (uAlignPow2 << 7) | (uSlotsCount << 11);
    else
        return uAlignPow2 << 7;
}

local_func u32 c_backend_get_format32_from_type(const TypeInfo* pType, CBckContext* pBckContext)
{
    u32 uAlignPow2 = get_log2_of_align_bytes(pType);
    u32 uSlotsCount = get_slots_count(pType);
    u8 uFormat = get_ir_format(pType);
    return c_backend_get_format32_from_data(uAlignPow2, uSlotsCount, uFormat, pBckContext);
}

local_func_inl void c_backend_format32_unpack(u32 uFormat32,
    u32* outAlignPow2, u32* outSlotsCount, u8* outIRFormat)
{
    *outAlignPow2 = (uFormat32 >> 7) & 0x0000'000Fu;
    *outSlotsCount = uFormat32 >> 11;
    *outIRFormat = u8(uFormat32) & 0x7Fu;
}

local_func CBckProcSignDefHandle c_backend_create_proc_representative_from(Arena arena, const TypeInfo_ProcLike* pProcSign,
    CBckContext* pBckContext)
{
    CBckProcSignDefHandle result;
    result.pDef = (CBckProcSignDef*)alloc_from(arena, sizeof(CBckProcSignDef), alignof(CBckProcSignDef));
    u32 uTotalParamsCount = pProcSign->params.size();
    Assert_(uTotalParamsCount<32u);
    u32 uInParamsCount = u32(get_input_param_count(pProcSign));
    u32 uProcKind = get_proc_kind(pProcSign);
    result.pDef->tableValues[0] = uTotalParamsCount | (uInParamsCount << 8) | (uProcKind << 16);
    u32* pParam = result.pDef->tableValues + 1;
    for (u32 uActualParam = 0u; uActualParam < uTotalParamsCount; uActualParam++, pParam++)
        *pParam = c_backend_get_format32_from_type(pProcSign->params[uActualParam].pBinding->pType, pBckContext);
    for (u32 uZeroedAfter = uTotalParamsCount; uZeroedAfter < 31; uZeroedAfter++)
        *pParam = 0;
    return result;
}

local_func_inl void c_backend_proc_sign_unpack_header(CBckProcSignDefHandle procSign,
    u32* outProcKind, u32* outTotalParamCount, u32* outInParamCount)
{
    u32 uHeader = procSign.pDef->tableValues[0];
    *outProcKind = uHeader >> 16;
    *outTotalParamCount = uHeader & 0x0000'001Fu;
    *outInParamCount = (uHeader >> 8) & 0x0000'001Fu;
}

local_func_inl u32 c_backend_get_format32_from_param(CBckProcSignDefHandle procSign, u32 uParamIndex) {
    return procSign.pDef->tableValues[uParamIndex+1u];
}

local_func void c_backend_emit_proc_sign_to(char* szBuffer, CBckProcSignDefHandle procSign, const char* szName, CBckContext* pBckContext)
{
    u32 uProcKind, uTotalParamsCount, uInParamCount;
    c_backend_proc_sign_unpack_header(procSign, &uProcKind, &uTotalParamsCount, &uInParamCount);
    Assert_(uTotalParamsCount < 32);
    Assert_(uInParamCount <= uTotalParamsCount);
    u32 uRetParamCount = uTotalParamsCount - uInParamCount;
    u32 uFirstAdditionalRet = uInParamCount + 1u;
    u32 uRemainingRetParamCount = 0;
    if (uRetParamCount) {
        uRemainingRetParamCount = uRetParamCount-1u;
        u32 uFirstRetParamFormat32 = c_backend_get_format32_from_param(procSign, uInParamCount);
        u32 uFirstRetParamAlignPow2, uFirstRetParamSlotsCount; u8 uFirstRetParamIRformat;
        c_backend_format32_unpack(uFirstRetParamFormat32, &uFirstRetParamAlignPow2, &uFirstRetParamSlotsCount, &uFirstRetParamIRformat);
        if (LIKELY(uFirstRetParamSlotsCount)) {
            if (uFirstRetParamSlotsCount > 1u || (uFirstRetParamAlignPow2 > 4u && uFirstRetParamAlignPow2 > get_log2_of_natural_align_from_format(uFirstRetParamIRformat))) {
                uFirstAdditionalRet -= 1u;
                uRemainingRetParamCount += 1u;
                sprintf(szBuffer, "void %s(", szName);
            } else {
                sprintf(szBuffer, "%s %s(", c_backend_get_type_identifier_for_format(uFirstRetParamIRformat), szName);
            }
        } else {
            sprintf(szBuffer, "_DummyEmptyStruct %s(", szName);
        }
    } else {
        sprintf(szBuffer, "void %s(", szName);
    }

    for (u32 uInParam = 0; uInParam < uInParamCount; uInParam++) {
        u32 uInParamFormat32 = c_backend_get_format32_from_param(procSign, uInParam);
        u32 uInParamAlignPow2, uInParamSlotsCount; u8 uInParamIRformat;
        c_backend_format32_unpack(uInParamFormat32, &uInParamAlignPow2, &uInParamSlotsCount, &uInParamIRformat);
        if (LIKELY(uInParamSlotsCount)) {
            if (uInParamSlotsCount > 1u || (uInParamAlignPow2 > 4u && uInParamAlignPow2 > get_log2_of_natural_align_from_format(uInParamIRformat))) {
                sprintf(szBuffer + strlen(szBuffer), "%s* inRP%u%s",
                    c_backend_get_type_identifier_for_format(uInParamIRformat), uInParam,
                    uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
            } else {
                sprintf(szBuffer + strlen(szBuffer), "%s inP%u%s",
                    c_backend_get_type_identifier_for_format(uInParamIRformat), uInParam,
                    uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
            }
        } else {
            sprintf(szBuffer + strlen(szBuffer), "_DummyEmptyStruct inP%u%s", uInParam,
                uInParam < uInParamCount-1u+uRemainingRetParamCount ? ", ":"");
        }
    }

    for (u32 uAddRetParam = uFirstAdditionalRet; uAddRetParam < uTotalParamsCount; uAddRetParam++) {
        u32 uRetParamFormat32 = c_backend_get_format32_from_param(procSign, uAddRetParam);
        u32 uRetParamAlignPow2, uRetParamSlotsCount; u8 uRetParamIRformat;
        c_backend_format32_unpack(uRetParamFormat32, &uRetParamAlignPow2, &uRetParamSlotsCount, &uRetParamIRformat);
        if (LIKELY(uRetParamSlotsCount)) {
            sprintf(szBuffer + strlen(szBuffer), "%s* outP%u%s",
                c_backend_get_type_identifier_for_format(uRetParamIRformat), uAddRetParam,
                uAddRetParam < uTotalParamsCount-1u ? ", ":"");
        } else {
            sprintf(szBuffer + strlen(szBuffer), "_DummyEmptyStruct* outP%u%s", uAddRetParam,
                uAddRetParam < uTotalParamsCount-1u ? ", ":"");
        }
    }

    sprintf(szBuffer + strlen(szBuffer), ")");
}

local_func void c_backend_emit_proc_sign_def(CBckProcSignDefHandle procSign, const char* szName, CBckContext* pBckContext)
{
    char szBuffer[4096];
    sprintf(szBuffer, "typedef ");
    c_backend_emit_proc_sign_to(szBuffer + strlen(szBuffer), procSign, szName, pBckContext);
    sprintf(szBuffer + strlen(szBuffer), ";\n");
    platform_write_to_file(pBckContext->fileForGlobalDecls, (const u8*)szBuffer, u32(strlen(szBuffer)));
}

local_func void c_backend_emit_proc_sign_def_id_to(char* szBuffer, CBckProcSignDefHandle procSign, CBckContext* pBckContext,
    Arena tmpArena, ArenaRefPoint beforeAllocIfPresent)
{
    u32 uProcSignIndex;
    auto itFound = pBckContext->mapOfAlreadyEmittedProcSignDefs.find(procSign);
    if (itFound != pBckContext->mapOfAlreadyEmittedProcSignDefs.end()) {
        uProcSignIndex = itFound.value();
        sprintf(szBuffer, "_proc_sign_%u_", uProcSignIndex);
        reset_arena_no_release_to(beforeAllocIfPresent, tmpArena);
    } else {
        uProcSignIndex = pBckContext->uCountProcSigns;
        sprintf(szBuffer, "_proc_sign_%u_", uProcSignIndex);
        c_backend_emit_proc_sign_def(procSign, szBuffer, pBckContext);
        pBckContext->uCountProcSigns++;
        pBckContext->mapOfAlreadyEmittedProcSignDefs.insert(procSign, uProcSignIndex);
    }
}

local_func bool c_backend_emit_procresult_value_to(char* szBuffer, const TypeInfo_ProcLike* pType,
    TCProcBodyResult* pProcResult, CBckContext* pBckContext)
{
    auto itFoundDecl = pBckContext->setOfAlreadyEmittedProcs.find(pProcResult);
    if (itFoundDecl == pBckContext->setOfAlreadyEmittedProcs.end()) {
        pBckContext->setOfProcsToEmit.insert(pProcResult);
        return false;
    }
    c_backend_emit_name_of_proc_to_buffer(szBuffer, pProcResult, pBckContext);
    return true;
}

// predecls
bool c_backend_emit_known_value_to(char* szBuffer, const TypeInfo* pType,
    AConstOrBoundValue constValue, u32 uConstFlags, CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext);
bool c_backend_emit_value_to(char* szBuffer, ValueBinding* pBinding,
    CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext);
void c_backend_emit_binding_id_to(char* szBuffer, ValueBinding* pBinding, CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext);

local_func bool c_backend_emit_value_from_holder_to(char* szBuffer, ValueHolder* pHolder,
    CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext)
{
    Assert_(is_value_const(pHolder));
    if (is_value_by_address(pHolder)) {
        Assert_(!is_value_by_indexed_ref(pHolder));
        ValueBinding* pResultBinding = get_deepest_primary_binding(pHolder->whenConstOrBound.pRefOther, pBckContext, false);
        if (!pResultBinding) {
            platform_log_error("*** unknown referencing a non-bound by address !!!", true);
            sprintf(szBuffer, "_err_unk_addressing_unbound_");
            return true;
        }
        if (u8(pResultBinding->uScopeAndWhenConstFlags) <= EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
            if (pBckContext->setOfAlreadyEmittedGlobalBindings.find(pResultBinding) != pBckContext->setOfAlreadyEmittedGlobalBindings.end()) {
                sprintf(szBuffer, "reinterpret_cast<r64>(&");
                c_backend_emit_binding_id_to(szBuffer+strlen(szBuffer), pResultBinding, pLocalProcIfLocal, pBckContext);
                sprintf(szBuffer+strlen(szBuffer), ")");
            } else {
                pBckContext->setOfGlobalBindingsToEmit.insert(pResultBinding);
                return false;
            }
        } else {
            Assert(is_value_const(pResultBinding), "cannot const-ref a local, non-const binding");
            Assert_(pLocalProcIfLocal);
            if (pLocalProcIfLocal->setOfAlreadyEmittedLocalBindings.find(pResultBinding) != pLocalProcIfLocal->setOfAlreadyEmittedLocalBindings.end()) {
                sprintf(szBuffer, "&");
                c_backend_emit_binding_id_to(szBuffer+1, pResultBinding, pLocalProcIfLocal, pBckContext);
            } else {
                pLocalProcIfLocal->setOfLocalBindingsToEmit.insert(pResultBinding);
                return false;
            }
        }
    } else {
        AConstOrBoundValue constValue; u32 uConstFlags;
        if (try_retrieve_final_known_const_value_from(pHolder, pBckContext, 0, AConstOrBoundValue{}, 0, &constValue, &uConstFlags)) {
            return c_backend_emit_known_value_to(szBuffer, pHolder->pType, constValue, uConstFlags, pLocalProcIfLocal, pBckContext);
        } else {
            platform_log_error("*** non-address, yet unknown const value emission from a ValueHolder !!!", true);
            sprintf(szBuffer, "_err_unk_non_address_");
            return true;
        }
    }
    return true;
}

local_func bool c_backend_emit_known_value_to(char* szBuffer, const TypeInfo* pType,
    AConstOrBoundValue constValue, u32 uConstFlags, CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext)
{
    ETypeKind eKind;
    pType = unalias_ext(pType, &eKind);
    if (get_slots_count(pType) == 0) {
        sprintf(szBuffer, "{ /*dummy-for-zero-sized-type*/ }");
        return true;
    }

    bool bEmbedded = 0 != (uConstFlags & CONSTFLAG_IS_EMBEDDED);
    if (bEmbedded && constValue.uEmbeddedValue == 0) {
        if (eKind == ETypeKind::ETYPEKIND_INTEGRAL) {
            if (get_log2_of_scalar_bytes(pType) <= 3) {
                sprintf(szBuffer, "0");
                return true;
            }
        } else if (eKind == ETypeKind::ETYPEKIND_FLOATINGPOINT) {
            if (get_log2_of_scalar_bytes(pType) == 3) {
                sprintf(szBuffer, "0.0");
                return true;
            } else if (get_log2_of_scalar_bytes(pType) == 2) {
                sprintf(szBuffer, "0.0f");
                return true;
            }
        } // otherwise (and fallthrough:)
        sprintf(szBuffer, "{ /*zeroed*/ }");
        return true;
    }

    switch (eKind) {
        case ETypeKind::ETYPEKIND_INTEGRAL: {
            u8 uFormat = get_log2_of_scalar_bytes(pType);
            switch (uFormat) {
                case 0: {
                    Assert_(bEmbedded);
                    u8 uAbs = u8(constValue.uEmbeddedValue);
                    if (uConstFlags & CONSTFLAG_INT_IS_NEGATIVE) {
                        u8 uResult = u8(~uAbs + 1u);
                        sprintf(szBuffer, "(0x%02Xu /* -%u */)", uResult, uAbs);
                    } else {
                        sprintf(szBuffer, "(0x%02Xu /* %u */)", uAbs, uAbs);
                    }
                } break;// u8

                case 1: {
                    Assert_(bEmbedded);
                    u16 uAbs = u16(constValue.uEmbeddedValue);
                    if (uConstFlags & CONSTFLAG_INT_IS_NEGATIVE) {
                        u16 uResult = u16(~uAbs + 1u);
                        sprintf(szBuffer, "(0x%04Xu /* -%u */)", uResult, uAbs);
                    } else {
                        sprintf(szBuffer, "(0x%04Xu /* %u */)", uAbs, uAbs);
                    }
                } break;// u16

                case 2: {
                    Assert_(bEmbedded);
                    u32 uAbs = u32(constValue.uEmbeddedValue);
                    if (uConstFlags & CONSTFLAG_INT_IS_NEGATIVE) {
                        u32 uResult = u32(~uAbs + 1u);
                        sprintf(szBuffer, "(0x%08Xu /* -%u */)", uResult, uAbs);
                    } else {
                        sprintf(szBuffer, "(0x%08Xu /* %u */)", uAbs, uAbs);
                    }
                } break;// u32

                case 3: {
                    Assert_(bEmbedded);
                    u64 uAbs = constValue.uEmbeddedValue;
                    if (uConstFlags & CONSTFLAG_INT_IS_NEGATIVE) {
                        u64 uResult = ~uAbs + 1uLL;
                        sprintf(szBuffer, "(0x%08llXuLL /* -%llu */)", uResult, uAbs);
                    } else {
                        sprintf(szBuffer, "(0x%08llXuLL /* %llu */)", uAbs, uAbs);
                    }
                } break;// u64

                case 4: {
                    if (bEmbedded) {
                        u128 uAbs = { constValue.uEmbeddedValue, 0uLL };
                        if (uConstFlags & CONSTFLAG_INT_IS_NEGATIVE) {
                            u128 uResult = neg128(uAbs);
                            sprintf(szBuffer, "r128{\n\t\t\t{ 0x%08llXuLL, 0x%08llXuLL } /* -%llu */ }",
                                uResult.tLegs[0], uResult.tLegs[1], constValue.uEmbeddedValue);
                        } else {
                            sprintf(szBuffer, "r128{\n\t\t\t{ 0x%08llXuLL, 0x%08llXuLL } /* %llu */ }",
                                uAbs.tLegs[0], uAbs.tLegs[1], constValue.uEmbeddedValue);
                        }
                    } else {
                        // TODO
                        sprintf(szBuffer, "_non_embd_integral_not_yet_impl");
                    }
                } break;// u128

                case 5: {
                    if (bEmbedded) {
                        u256 uAbs = { constValue.uEmbeddedValue, 0uLL, 0uLL, 0uLL };
                        if (uConstFlags & CONSTFLAG_INT_IS_NEGATIVE) {
                            u256 uResult = neg256(uAbs);
                            sprintf(szBuffer, "r256{\n\t\t\t{ 0x%08llXuLL, 0x%08llXuLL, 0x%08llXuLL, 0x%08llXuLL } /* -%llu */ }",
                                uResult.tLegs[0], uResult.tLegs[1], uResult.tLegs[2], uResult.tLegs[3], constValue.uEmbeddedValue);
                        } else {
                            sprintf(szBuffer, "r256{\n\t\t\t{ 0x%08llXuLL, 0x%08llXuLL, 0x%08llXuLL, 0x%08llXuLL } /* %llu */ }",
                                uAbs.tLegs[0], uAbs.tLegs[1], uAbs.tLegs[2], uAbs.tLegs[3], constValue.uEmbeddedValue);
                        }
                    } else {
                        // TODO
                        sprintf(szBuffer, "_non_embd_integral_not_yet_impl");
                    }
                } break;// u256

                case 6: {
                    // TODO
                    sprintf(szBuffer, "_r512_values_not_yet_impl");
                } break;// r512

                case 7: {
                    // TODO
                    sprintf(szBuffer, "_r1024_values_not_yet_impl");
                } break;// r1024

                default:
                    Assert_(false);
            }
        } break;

        case ETypeKind::ETYPEKIND_FLOATINGPOINT: {
            u8 uFormat = get_log2_of_scalar_bytes(pType);
            switch (uFormat) {
                case 2: {
                    Assert_(bEmbedded);
                    sprintf(szBuffer, "%.17G", constValue.dEmbeddedFP);
                } break;
                case 3: {
                    Assert_(bEmbedded);
                    sprintf(szBuffer, "%.9Gf", float(constValue.dEmbeddedFP));
                } break;
                default: {
                    // TODO
                    sprintf(szBuffer, "_non_C_compliant_fp_formats_not_yet_impl");
                } break;// u8
            }
        } break;

        case ETypeKind::ETYPEKIND_PROCLIKEBODY: { // Hmmmm...
            sprintf(szBuffer, "_non_ref_proclike_body_as_runtime_value_error");
            /*
            sprintf(szBuffer, "&");
            return c_backend_emit_procresult_value_to(szBuffer + strlen(szBuffer),
                (const TypeInfo_ProcLike*)pType, constValue.pIfProcBody, pBckContext);
            */
        } break;

        case ETypeKind::ETYPEKIND_POINTER: { // A non-zero *known* ptr value ??? oh, well...
            Assert_(bEmbedded);
            sprintf(szBuffer, "reinterpret_cast<%s*>(0x%016llx)",
                c_backend_get_type_identifier_for_format(get_ir_format(pType)), constValue.uEmbeddedValue);
        } break;

        case ETypeKind::ETYPEKIND_ARRAY: {
            const TypeInfo_Array* asArray = (const TypeInfo_Array*)pType;
            const TypeInfo* pElemType = asArray->pElementType;
            if (get_array_category_type(asArray) == ARRAY_TYPE_KIND_STATIC) {
                sprintf(szBuffer, "{\n");
                u32 uElemCount = asArray->uElemCount;
                if (uConstFlags & CONSTFLAG_ARRAY_IS_STORED_AS_RUNTIME) {
                    u8* pRawData = constValue.pPtrToRawData;
                    u32 unused;
                    u32 uRuntimeSize = get_runtime_sizeof(pElemType, &unused);
                    AConstOrBoundValue constValElem; u32 constFlagsElem;
                    for (u32 uElem = 0; uElem < uElemCount; uElem++, pRawData += uRuntimeSize) {
                        reinterp_from_runtime_representation_at(pRawData,
                            pElemType, pBckContext, &constValElem, &constFlagsElem);
                        if (!c_backend_emit_known_value_to(szBuffer+strlen(szBuffer), pElemType,
                            constValElem, constFlagsElem, pLocalProcIfLocal, pBckContext))
                            return false;
                        sprintf(szBuffer+strlen(szBuffer), ",\n");
                    }
                } else {
                    ConstArrayElem* tElems = (ConstArrayElem*)constValue.pPtrToRawData;
                    ValueHolder hackHolder;
                    hackHolder.pType = pElemType;
                    hackHolder.uIR = INVALID_IR_CODE;
                    for (u32 uElem = 0; uElem < uElemCount; uElem++) {
                        hackHolder.uFlags = tElems[uElem].uAsValueHolderFlags;
                        hackHolder.whenConstOrBound = tElems[uElem].asValueHolderValue;
                        if (!c_backend_emit_value_from_holder_to(szBuffer+strlen(szBuffer), &hackHolder, pLocalProcIfLocal, pBckContext))
                            return false;
                        sprintf(szBuffer+strlen(szBuffer), ",\n");
                    }
                }
                sprintf(szBuffer+strlen(szBuffer), "}");
            } else {
                platform_log_error("*** known-const value emission not yet implemented for non-static arrays", true);
            }
        } break;

        case ETypeKind::ETYPEKIND_STRINGRELATED: {
            const TypeInfo_String* pAsString = (const TypeInfo_String*)pType;
            if (pAsString->_coreFlags & STRINGFLAG_IS_COMPACT) {
                platform_log_error("*** a compact string shall not be a primary ***", true);
                sprintf(szBuffer, "_value_emission_unexpected_for_compact_strings_");
            } else {
                platform_log_error("*** value emission not yet implemented for non-compact strings", true);
                sprintf(szBuffer, "_value_emission_not_yet_impl_for_this_kind_");
            }
        } break;

        default: {
            platform_log_error("*** value emission not yet implemented for this kind", true);
            sprintf(szBuffer, "_value_emission_not_yet_impl_for_this_kind_");
        }
    }

    return true;
}

local_func_inl bool c_backend_emit_value_to(char* szBuffer, ValueBinding* pBinding,
    CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext)
{
    if (pBinding->uFlags & VALUEHOLDER_FLAG_IS_UNKNOWN) {
        // unknown global consts, and unknown initial values for global go there
        ValueHolder hackAsConst = *((ValueHolder*)pBinding);
        hackAsConst.uFlags |= VALUEHOLDER_FLAG_IS_CONST;
        return c_backend_emit_value_from_holder_to(szBuffer, &hackAsConst, pLocalProcIfLocal, pBckContext);
    } else {
        // known global consts, and known initial values for global go there
        return c_backend_emit_known_value_to(szBuffer, pBinding->pType, pBinding->finalConstValue, pBinding->uScopeAndWhenConstFlags, 
            pLocalProcIfLocal, pBckContext);
    }
}

local_func void c_backend_emit_binding_id_to(char* szBuffer, ValueBinding* pBinding, CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext)
{
    Assert_(u8(pBinding->uScopeAndWhenConstFlags) <= EScopeKind::SCOPEKIND_GLOBAL_PRIVATE || pLocalProcIfLocal);
    if (u8(pBinding->uScopeAndWhenConstFlags) <= EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
        sprintf(szBuffer, "file%d_%s%u_", pBinding->iSourceFileIndex,
            is_value_const(pBinding) ? "gcon" : "gvar", pBinding->uLocation);
    } else {
        Assert_(is_value_const(pBinding));
        sprintf(szBuffer, "file%d_local_con%u_in_proc%u_", pLocalProcIfLocal->pProcResult->iSourceFileIndex,
            pBinding->uLocation, pLocalProcIfLocal->pProcResult->uRegistrationIndex);
    }

    if (pBinding->iIdentifierHandle > 0) {
        sprintf(szBuffer + strlen(szBuffer), "%s_",
            get_identifier_string(pBckContext->pProgCompilationState, u32(pBinding->iIdentifierHandle)).c_str());
    }
}

local_func bool c_backend_is_table_type(const TypeInfo* pType, CompilationContext* pEvalContext)
{
    ETypeKind eKind;
    pType = unalias_ext(pType, &eKind);
    return eKind == ETypeKind::ETYPEKIND_ARRAY;
}

local_func void c_backend_emit_regular_from_immediate_to(char* szBuffer, u32 uIRParam, u8 uFormat,
    CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    Assert_(ir_is_immediate(uIRParam));
    if (ir_is_int_immediate(uIRParam)) {
        i32 iIntValue = ir_get_as_int_immediate(uIRParam);
        if (iIntValue) {
            Assert_(uFormat <= 0x07u);
            AConstOrBoundValue asConstOrBound;
            if (iIntValue < 0) {
                asConstOrBound.uEmbeddedValue = u64(-iIntValue);
                c_backend_emit_known_value_to(szBuffer, std_type_from_ir_format(uFormat),
                    asConstOrBound, CONSTFLAG_IS_EMBEDDED|CONSTFLAG_INT_IS_NEGATIVE, pProcBody, pBckContext);
            } else {
                asConstOrBound.uEmbeddedValue = u64(iIntValue);
                c_backend_emit_known_value_to(szBuffer, std_type_from_ir_format(uFormat),
                    asConstOrBound, CONSTFLAG_IS_EMBEDDED, pProcBody, pBckContext);
            }
        } else {
            if ((uFormat & 0x07u) <= 3 && (uFormat & 0xF0u) == 0)
                sprintf(szBuffer, "0");
            else
                sprintf(szBuffer, "%s{}", c_backend_get_type_identifier_for_format(uFormat));
        }
    } else {
        sprintf(szBuffer, "_non_int_immediates_not_yet_impl_");
    }
}

enum ECBckInterp {
    ECBKINTERP_RAW,
    ECBKINTERP_REGULAR,
    ECBKINTERP_REFERENCE,
    ECBKINTERP_DEREFERENCE,
};

#define _interp_prefix(isRef, req)  (req == ECBKINTERP_RAW ? "" :                     \
                                    (req == ECBKINTERP_REGULAR ? (isRef ? "*":"") :   \
                                    (req == ECBKINTERP_REFERENCE ? (isRef ? "":"&") : \
                                    (/*req == ECBKINTERP_DEREFERENCE ?*/ isRef ? "**":"*"))))

local_func u8 c_backend_interpret_standard_ir_parameter_to(char* szId, u32 uIRParam, CBckProcBody* pProcBody, CBckContext* pBckContext,
    bool* outIsRef, u32* outAlignPow2, u32* outSlotCount, ECBckInterp eInterp)
{
    Assert_(!ir_is_immediate(uIRParam));

    u32 uFormat32 = pProcBody->tInstrFormats[uIRParam];
    u32 uAlignPow2, uSlotCount; u8 uIRFormat;
    c_backend_format32_unpack(uFormat32, &uAlignPow2, &uSlotCount, &uIRFormat);
    bool bIsRef = uSlotCount > 1u;

    Assert_(pProcBody->uTotalParamsCount < 32u);
    if (uIRParam < pProcBody->uTotalParamsCount) {
        Assert_(u8(pProcBody->pProcResult->vecInstructions[uIRParam]) == IRIT_RESERVE_LOCAL_VAR);
        Assert_(uFormat32 == c_backend_get_format32_from_param(pProcBody->procSignDef, uIRParam));
        Assert_(uFormat32 == c_backend_get_format32_from_type(pProcBody->pProcResult->procSign->params[uIRParam].pBinding->pType, pBckContext));
        u32 uInParamsCount = get_input_param_count(pProcBody->pProcResult->procSign);
        Assert_(uInParamsCount < pProcBody->uTotalParamsCount);
        u32 uRetParamsCount = pProcBody->uTotalParamsCount - uInParamsCount;
        if (uIRParam > uInParamsCount || (uSlotCount && uAlignPow2 > 4u && uAlignPow2 >= get_log2_of_natural_align_from_format(uIRFormat)))
            bIsRef = true;
        if (uIRParam >= uInParamsCount) {
            sprintf(szId, "%soutP%u", _interp_prefix(bIsRef, eInterp), uIRParam);
        } else if (bIsRef) {
            sprintf(szId, "%sinRP%u", _interp_prefix(bIsRef, eInterp), uIRParam);
        } else {
            sprintf(szId, "%sinP%u", _interp_prefix(bIsRef, eInterp), uIRParam);
        }
    } else {
        u64 uInstructionEncoding = pProcBody->pProcResult->vecInstructions[uIRParam];
        u64 uFourLSB = uInstructionEncoding & 0x000FuLL;
        if (uFourLSB == 0) {            // a binding => we use the name of the binding
            ValueBinding* pAsBinding = reinterpret_cast<ValueBinding*>(uInstructionEncoding);
            Assert_(uFormat32 == c_backend_get_format32_from_type(pAsBinding->pType, pBckContext));
            bIsRef |= uSlotCount && c_backend_is_table_type(pAsBinding->pType, pBckContext);
            if (is_value_const(pAsBinding) && (eInterp == ECBKINTERP_REFERENCE || (bIsRef && eInterp == ECBKINTERP_RAW))) {
                sprintf(szId, "const_cast<%s*>(%s",
                    c_backend_get_type_identifier_for_format(get_ir_format(pAsBinding->pType)),
                    _interp_prefix(bIsRef, eInterp));
                    c_backend_emit_binding_id_to(szId + strlen(szId), pAsBinding, pProcBody, pBckContext);
                sprintf(szId + strlen(szId), ")"); 
            } else {
                sprintf(szId, "%s", _interp_prefix(bIsRef, eInterp));
                c_backend_emit_binding_id_to(szId + strlen(szId), pAsBinding, pProcBody, pBckContext);
            }
        } else if (uFourLSB == 1) {     // an 'extended code' instruction => just as-if-regular for referencing it
            sprintf(szId, "%stmp%u", _interp_prefix(bIsRef, eInterp), uIRParam);

        } else {                        // regular instructions, but depending on what it is...
            u32 uParamFormat32 = pProcBody->tInstrFormats[uIRParam];
            c_backend_format32_unpack(uParamFormat32, &uAlignPow2, &uSlotCount, &uIRFormat);
            u8 uInstrType = u8(uInstructionEncoding);
            if (uInstrType == IRIT_RESERVE_LOCAL_VAR) {             // ...a local var slot.
                sprintf(szId, "%svar%u", _interp_prefix(bIsRef, eInterp), uIRParam);

            } else if (uInstrType == IRIT_CALLER_PROC_RESULT) {     // ...a proc result
                u32 uIRofCall = u32(uInstructionEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                Assert_(uIRParam > uIRofCall);
                u32 uRelativeParamPos = uIRParam - uIRofCall - 1u;
                u64 uCallInstruction = pProcBody->pProcResult->vecInstructions[uIRofCall];
                if (u8(uCallInstruction) == IRIT_PROC_CALL) {
                    // TODO: call conventions, default trailing params...
                    u32 uInParamsCountOfCall = (uCallInstruction >> 40) & 0x01F;
                    Assert_(uRelativeParamPos >= uInParamsCountOfCall);
                    u32 uOutParamsCountOfCall = (uCallInstruction >> 45) & 0x01F;
                    Assert_(uInParamsCountOfCall + uOutParamsCountOfCall < 32u);
                    Assert_(uRelativeParamPos < uInParamsCountOfCall + uOutParamsCountOfCall);
                    sprintf(szId, "%sres%u", _interp_prefix(bIsRef, eInterp), uIRParam);
                } else {
                    sprintf(szId, "%ssup%u", _interp_prefix(bIsRef, eInterp), uIRParam);
                }

            } else if (uInstrType == IRIT_OFFSET) {                 // ...a regular offset into something else
                u32 uBaseIR = u32(uInstructionEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                Assert_(!ir_is_immediate(uBaseIR));
                u32 uIRofOffset = u32(uInstructionEncoding >> 40);
                char szRawBase[1024]; bool bBaseIsRef, unused1; u32 unused2, unused3;
                c_backend_interpret_standard_ir_parameter_to(szRawBase, uBaseIR,
                    pProcBody, pBckContext, &bBaseIsRef, &unused2, &unused3, ECBckInterp::ECBKINTERP_REFERENCE);
                //Assert_(bBaseIsRef);
                bIsRef = true;
                sprintf(szId, "%s(%s + ", _interp_prefix(bIsRef, eInterp), szRawBase);
                if (ir_is_immediate(uIRofOffset)) {
                    c_backend_emit_regular_from_immediate_to(szId + strlen(szId), uIRofOffset, 0x02u, pProcBody, pBckContext);
                } else {
                    c_backend_interpret_standard_ir_parameter_to(szId + strlen(szId), uIRofOffset,
                        pProcBody, pBckContext, &unused1, &unused2, &unused3, ECBckInterp::ECBKINTERP_REGULAR);
                }
                sprintf(szId + strlen(szId), ")");

            } else if (uInstrType == IRIT_OFFSET_EXT) {        // ...an extended or reinterpreted offset
                u32 uBaseIR = u32(uInstructionEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                Assert_(!ir_is_immediate(uBaseIR));
                u8 uElemFormat = u8(uInstructionEncoding >> 8);
                u8 uBaseFormat = u8(uInstructionEncoding >> 40);
                u8 uIsKnownRange = u8(uInstructionEncoding >> 48) & 0x01u;
                u8 uIsBytewise = u8(uInstructionEncoding >> 49) & 0x01u;
                u8 uEnsuredAlignLog2 = u8(uInstructionEncoding >> 50) & 0x0Fu;
                u64 uNextInstructionEncoding = pProcBody->pProcResult->vecInstructions[uIRParam+1u];
                Assert_(u8(uNextInstructionEncoding) == IRIT_CALLER_PROC_PARAM);
                u8 uOffsetFormat = u8(uNextInstructionEncoding >> 8);
                Assert_(uOffsetFormat <= 0x05u); // or 64b max ??? currently 256
                u32 uIRofOffset = u32(uNextInstructionEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                Assert_(u32(uNextInstructionEncoding >> 40) == 1u);
                char szRawBase[1024]; bool bBaseIsRef, unused1; u32 unused2, unused3;
                c_backend_interpret_standard_ir_parameter_to(szRawBase, uBaseIR,
                    pProcBody, pBckContext, &bBaseIsRef, &unused2, &unused3, ECBckInterp::ECBKINTERP_REFERENCE);
                bIsRef = true;
                if (uIsBytewise && (uBaseFormat != 0x00u || uElemFormat != 0x00u)) {
                    sprintf(szId, "(%s*)%s((r8*)(%s) + i64(", c_backend_get_type_identifier_for_format(uElemFormat),
                        _interp_prefix(bIsRef, eInterp), szRawBase);
                } else {
                    sprintf(szId, "%s(%s + i64(", _interp_prefix(bIsRef, eInterp), szRawBase);
                }
                if (ir_is_immediate(uIRofOffset)) {
                    c_backend_emit_regular_from_immediate_to(szId + strlen(szId), uIRofOffset, 0x03u, pProcBody, pBckContext);
                } else {
                    c_backend_interpret_standard_ir_parameter_to(szId + strlen(szId), uIRofOffset,
                        pProcBody, pBckContext, &unused1, &unused2, &unused3, ECBckInterp::ECBKINTERP_REGULAR);
                }
                sprintf(szId + strlen(szId), ") )");

            } else if (uInstrType == IRIT_DEREF_LONG_ADDRESS) {     // ...the dereferencing of a long-address
                bIsRef = true;   // those in fact still are 'refs' (pointers) for the c backend... however, they are now typed.
                sprintf(szId, "%sref%u", _interp_prefix(bIsRef, eInterp), uIRParam);
            } /*else if (uInstrType == IRIT_LONG_ADDRESS) {
                u32 uEnsuredAlignPow2 = u32(uInstructionEncoding >> 16) & 0x0Fu;
                u32 uBaseIR = u32(uInstructionEncoding >> 40);
                Assert_(!ir_is_immediate(uBaseIR));
                bIsRef = false;
                bool unused1; u32 unused2, unused3;
                if (eInterp == ECBckInterp::ECBKINTERP_REGULAR) {
                    c_backend_interpret_standard_ir_parameter_to(szId + strlen(szId), uBaseIR,
                        pProcBody, pBckContext, &unused1, &unused2, &unused3, ECBckInterp::ECBKINTERP_REFERENCE);
                } else {
                    sprintf(szId, "%s(&(", _interp_prefix(bIsRef, eInterp));
                    c_backend_interpret_standard_ir_parameter_to(szId + strlen(szId), uBaseIR,
                        pProcBody, pBckContext, &unused1, &unused2, &unused3, ECBckInterp::ECBKINTERP_REGULAR);
                    sprintf(szId + strlen(szId), "))");
                }
            } */else if (uInstrType == IRIT_REINTERP) {
                u32 uBaseIR = u32(uInstructionEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                u8 uBaseFormat = u8(uInstructionEncoding >> 40);
                u8 uDestFormat = u8(uInstructionEncoding >> 8);
                if (ir_is_immediate(uBaseIR)) {
                    bIsRef = false;
                    Assert_(eInterp <= ECBKINTERP_REGULAR);
                    if (ir_is_int_immediate(uBaseIR)) {
                        if (uDestFormat <= 0x05u) {
                            Assert_(uDestFormat <= 0x02u);
                            sprintf(szId, "%s(", c_backend_get_type_identifier_for_format(uDestFormat));
                            c_backend_emit_regular_from_immediate_to(szId + strlen(szId), uBaseIR,
                                0x02u, pProcBody, pBckContext);
                            sprintf(szId + strlen(szId), ")");
                        } else {
                            sprintf(szId, "_reinterp_int_immediate_to_non_int_not_yet_impl_%u", uIRParam);
                        }
                    } else {
                        sprintf(szId, "_reinterp_non_int_immediate_not_yet_impl_%u", uIRParam);
                    }
                } else {
                    bIsRef = true;
                    sprintf(szId, "%sreinterpret_cast<%s*>(", _interp_prefix(bIsRef, eInterp),
                        c_backend_get_type_identifier_for_format(uDestFormat));
                    bool unused1; u32 unused2, unused3;
                    c_backend_interpret_standard_ir_parameter_to(szId + strlen(szId), uBaseIR,
                        pProcBody, pBckContext, &unused1, &unused2, &unused3, ECBckInterp::ECBKINTERP_REFERENCE);
                    sprintf(szId + strlen(szId), ")");
                }
            } else if (uInstrType == IRIT_CAST_SCALAR_ELEMS) {
                sprintf(szId, "_cast_not_yet_impl_%u", uIRParam);
            } else if (uInstrType == IRIT_CAST_FP_TO_INT_SCALAR_ELEMS) {
                sprintf(szId, "_cast_fp_to_int_not_yet_impl_%u", uIRParam);
            } else {
                sprintf(szId, "%stmp%u", _interp_prefix(bIsRef, eInterp), uIRParam);
            }
        }
    }

    *outIsRef = bIsRef;
    *outAlignPow2 = uAlignPow2;
    *outSlotCount = uSlotCount;
    return uIRFormat;
}

local_func bool c_backend_emit_binding(ValueBinding* pBinding, CBckProcBody* pLocalProcIfLocal, CBckContext* pBckContext)
{
    Assert_(pBinding);
    Assert_(u8(pBinding->uScopeAndWhenConstFlags) <= EScopeKind::SCOPEKIND_GLOBAL_PRIVATE || pLocalProcIfLocal);
    pBckContext->pIsolatedSourceFile = pBckContext->pProgCompilationState->vecSourceFiles[u32(pBinding->iSourceFileIndex)];

    char szBuffer[2048];
    #if TRACE_BACKEND_PRINTLEVEL > 3
    sprintf(szBuffer, "==== starting emission of %s binding %s from file %s", pLocalProcIfLocal ? "local" : "global",
        pBinding->iIdentifierHandle > 0 ? get_identifier_string(pBckContext->pProgCompilationState, pBinding->iIdentifierHandle).c_str() :
            "<unnamed>", pBckContext->pIsolatedSourceFile->sourceFileName.c_str());
    platform_log_info(szBuffer, true);
    #endif

    char szId[2048];
    c_backend_emit_binding_id_to(szId, pBinding, pLocalProcIfLocal, pBckContext);

    if (get_type_kind(pBinding->pType) == ETypeKind::ETYPEKIND_PROCLIKEBODY) {
        const TypeInfo_ProcLike* pAsProcType = (const TypeInfo_ProcLike*)pBinding->pType;
        ArenaRefPoint beforeAllocProcSign = get_arena_ref_point(pBckContext->pWorker->tmpArena);
        if (is_value_const(pBinding)) {
            sprintf(szBuffer, "#define %s\t\t", szId);
            TCProcBodyResult* pProcBody = pBinding->finalConstValue.pIfProcBody;
            if (pBckContext->setOfAlreadyEmittedProcs.find(pProcBody) == pBckContext->setOfAlreadyEmittedProcs.end())
                pBckContext->setOfProcsToEmit.insert(pProcBody);
            c_backend_emit_name_of_proc_to_buffer(szBuffer + strlen(szBuffer), pProcBody, pBckContext);
            sprintf(szBuffer + strlen(szBuffer), "\n");
        } else {
            CBckProcSignDefHandle procSign = c_backend_create_proc_representative_from(
                pBckContext->pWorker->tmpArena, pAsProcType, pBckContext);
            char szSignDefId[256];
            c_backend_emit_proc_sign_def_id_to(szSignDefId, procSign, pBckContext,
                pBckContext->pWorker->tmpArena, beforeAllocProcSign);
            sprintf(szBuffer, "static %s* %s = &", szSignDefId, szId);
            if (c_backend_emit_procresult_value_to(szBuffer + strlen(szBuffer), pAsProcType,
                pBinding->finalConstValue.pIfProcBody, pBckContext)) {
                sprintf(szBuffer + strlen(szBuffer), ";\n");
            } else {
                return false;
            }
        }

    } else {
        u32 uAlignPow2 = get_log2_of_align_bytes(pBinding->pType);
        u32 uSlotsCount = get_slots_count(pBinding->pType);
        if (LIKELY(uSlotsCount)) {
            u8 uFormat = get_ir_format(pBinding->pType);
            u32 uSlotPow2 = get_log2_of_scalar_bytes(pBinding->pType) + get_log2_of_vector_count(pBinding->pType);

            if (uAlignPow2 > 3u || uAlignPow2 > uSlotPow2) // TODO: '3u' here assumes 64b platforms
                sprintf(szBuffer, "__declspec(align(%u)) static %s%s %s%s", 1u << uAlignPow2, 
                    is_value_const(pBinding) ? "const ":"",
                    c_backend_get_type_identifier_for_format(uFormat), 
                    szId, c_backend_is_table_type(pBinding->pType, pBckContext) ? "[]":"");
            else // TODO: try to reestablish 'constexpr' here, WRT our reinterpret cast as r64 of addresses of other bindings.
                sprintf(szBuffer, "%s%s %s%s", is_value_const(pBinding) ? "static const ":"static ",
                    c_backend_get_type_identifier_for_format(uFormat), 
                    szId, c_backend_is_table_type(pBinding->pType, pBckContext) ? "[]":"");
                /* 
                sprintf(szBuffer, "%s%s %s%s", is_value_const(pBinding) ? "constexpr ":"static ",
                    c_backend_get_type_identifier_for_format(uFormat), 
                    szId, c_backend_is_table_type(pBinding->pType, pBckContext) ? "[]":"");
                */

            if (uSlotsCount > 1u && !c_backend_is_table_type(pBinding->pType, pBckContext))
                sprintf(szBuffer + strlen(szBuffer), "[%u] = ", uSlotsCount);
            else
                sprintf(szBuffer + strlen(szBuffer), " = ");

            if (c_backend_emit_value_to(szBuffer + strlen(szBuffer), pBinding, pLocalProcIfLocal, pBckContext)) {
                sprintf(szBuffer + strlen(szBuffer), ";\n");
            } else {
                return false;
            }

        } else {
            if (uAlignPow2)
                sprintf(szBuffer, "__declspec(align(%u)) static const _DummyEmptyStruct %s = {};\n", 1u << uAlignPow2, szId);
            else
                sprintf(szBuffer, "static const _DummyEmptyStruct %s = {};\n", szId);
        }
    }

    platform_write_to_file(pBckContext->fileForGlobalDecls, (u8*)szBuffer, u32(strlen(szBuffer)));
    return true;
}

local_func bool c_backend_emit_locals_for_proc_body(CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    while (pProcBody->setOfLocalBindingsToEmit.size()) {
        auto itEmitOne = pProcBody->setOfLocalBindingsToEmit.begin();
        ValueBinding* pBinding = *itEmitOne;
        pProcBody->setOfLocalBindingsToEmit.remove_at_iter(itEmitOne);
        pProcBody->setOfAlreadyEmittedLocalBindings.insert(pBinding);
        if (!c_backend_emit_binding(pBinding, pProcBody, pBckContext)) {
            pProcBody->vecOfLocalBindingsOnHold.append(pBinding);
        }
    }

    u32 uOnHoldLocalsCount = pProcBody->vecOfLocalBindingsOnHold.size();
    if (uOnHoldLocalsCount) {
        ArenaRefPoint beforeTmpSecond = get_arena_ref_point(pBckContext->secondaryTmpArena);
        ValueBinding** tStillOnHold = (ValueBinding**)alloc_from(pBckContext->secondaryTmpArena,
            uOnHoldLocalsCount * sizeof(ValueBinding*), alignof(ValueBinding*));
        u32 uStillOnHoldCount = 0;
        for (u32 uOnHold = 0; uOnHold < uOnHoldLocalsCount; uOnHold++) {
            ValueBinding* pBinding = pProcBody->vecOfLocalBindingsOnHold[uOnHold];
            if (!c_backend_emit_binding(pBinding, pProcBody, pBckContext)) {
                // TODO: handle case of cyclic references ? allow them ?
                tStillOnHold[uStillOnHoldCount] = pBinding;
                uStillOnHoldCount++;
            }
        }
        pProcBody->vecOfLocalBindingsOnHold.clear();
        if (uStillOnHoldCount) {
            pProcBody->vecOfLocalBindingsOnHold.append_all(tStillOnHold, uStillOnHoldCount);
            reset_arena_no_release_to(beforeTmpSecond, pBckContext->secondaryTmpArena);
            return false;
        } else {
            reset_arena_no_release_to(beforeTmpSecond, pBckContext->secondaryTmpArena);
            return true;
        }

    } else 
        return true;
}

local_func void c_backend_emit_regular_from_non_immediate_param_to(char* szBuffer, u32 uIRParam, u8 uFormat,
    CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    Assert_(!ir_is_immediate(uIRParam));
    bool bIsRef; u32 uAlignPow2, uSlotsCount;
    u8 uFormatOfParam = c_backend_interpret_standard_ir_parameter_to(szBuffer, uIRParam, pProcBody, pBckContext,
        &bIsRef, &uAlignPow2, &uSlotsCount, ECBckInterp::ECBKINTERP_REGULAR);
    Assert_(uSlotsCount);
    Assert_(uFormatOfParam == uFormat);
}

local_func void c_backend_emit_ref_from_non_immediate_param_to(char* szBuffer, u32 uIRParam, u8 uFormat,
    CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    Assert_(!ir_is_immediate(uIRParam));
    bool bIsRef; u32 uAlignPow2, uSlotsCount;
    u8 uFormatOfParam = c_backend_interpret_standard_ir_parameter_to(szBuffer, uIRParam, pProcBody, pBckContext,
        &bIsRef, &uAlignPow2, &uSlotsCount, ECBckInterp::ECBKINTERP_REGULAR);
    Assert_(uSlotsCount);
    Assert_(uFormatOfParam == uFormat);
}

local_func_inl void c_backend_emit_regular_from_std_param_to(char* szBuffer, u32 uIRParam, u8 uFormat,
    CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    if (ir_is_immediate(uIRParam))
        c_backend_emit_regular_from_immediate_to(szBuffer, uIRParam, uFormat, pProcBody, pBckContext);
    else
        c_backend_emit_regular_from_non_immediate_param_to(szBuffer, uIRParam, uFormat, pProcBody, pBckContext);
}

local_func_inl void c_backend_emit_proc_call_from_procresult_param_to(char* szBuffer, u32 uIRParam, u8 uFormat,
    CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    c_backend_emit_regular_from_non_immediate_param_to(szBuffer, uIRParam, uFormat, pProcBody, pBckContext);
    sprintf(szBuffer + strlen(szBuffer), "(");
}

local_func bool c_backend_declare_local_var_or_tmp(const char* szVarOrTmp, u32 uInstrIndex, u32 uAlignPow2, u32 uSlotsCount, u8 uFormat,
    CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    u32 uFormat32 = c_backend_get_format32_from_data(uAlignPow2, uSlotsCount, uFormat, pBckContext);
    pProcBody->tInstrFormats[uInstrIndex] = uFormat32;

    char szBuffer[2048];
    bool bIsRef = false;
    if (LIKELY(uSlotsCount)) {
        if (uAlignPow2 > 4u && uAlignPow2 > get_log2_of_natural_align_from_format(uFormat)) // TODO: '4u' here assumes align 16 by default for all proc params
            sprintf(szBuffer, "\t__declspec(align(%u)) %s %s%u", 1u << uAlignPow2, 
                c_backend_get_type_identifier_for_format(uFormat), szVarOrTmp, uInstrIndex);
        else
            sprintf(szBuffer, "\t%s %s%u", c_backend_get_type_identifier_for_format(uFormat), szVarOrTmp, uInstrIndex);

        if (uSlotsCount > 1u) {
            sprintf(szBuffer + strlen(szBuffer), "[%u];\n", uSlotsCount);
            bIsRef = true;
        } else
            sprintf(szBuffer + strlen(szBuffer), ";\n");
    } else {
        if (uAlignPow2)
            sprintf(szBuffer, "\t__declspec(align(%u)) _DummyEmptyStruct %s%u;\n", 1u << uAlignPow2, szVarOrTmp, uInstrIndex);
        else
            sprintf(szBuffer, "\t_DummyEmptyStruct %s%u;\n", szVarOrTmp, uInstrIndex);
    }

    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
    return bIsRef;
}

#endif // TMP TMP TMP

local_func void c_backend_emit_std_assignable_to(char* pBuffer, u64 uStdIR, u8 uFormat, CBckProcBody* pIfProcBody, CBckContext* pBckContext)
{
    Assert_(ir_is_valid_param(uStdIR));
    Assert_(!ir_is_immediate(uStdIR));
    u16 uLocation = ir_get_repo_id(uStdIR);
    u32 uPos = ir_get_pos_in_repo(uStdIR);
    if (uLocation < IR_REPO_FILE_OFFSET) {
        Assert(false, "assignable in programwise globals not supported");

    } else if (uLocation < IR_REPO_CURRENT_PROC) {
        u32 uFileIndex = uLocation - IR_REPO_FILE_OFFSET;
        IRRepo* pRepo = &(pBckContext->pProgCompilationState->vecSourceFiles[uFileIndex]->filewiseRepo);
        IREntry& entry = ir_access_repo_instr(pRepo, uPos);
        Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION);
        Assert_(0 == (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST));
        Assert_(entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_ASSIGNABLE);
        if (0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT)) {
            entry.uInstrMetaFlagsAndSecondParam |= IRFLAG_BCK_TO_EMIT;
            pBckContext->vecOfDeclarationsToEmit.append(uStdIR);
        }
        u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
        u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
        Assert_(uSlotsCount); // otherwise not "assignable" as far as IR is concerned
        Assert_(uFormat == u8(entry.uInstrCodeAndFormatAndFirstParam >> 8));
        if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like var
            sprintf(pBuffer, "file%u_var%u", uFileIndex, uPos);
        } else { // table-like var
            sprintf(pBuffer, "*file%u_varR%u", uFileIndex, uPos);
        }

    } else if (uLocation == IR_REPO_CURRENT_PROC) {
        Assert_(pIfProcBody);
        const TypeInfo_ProcLike* procSign = pIfProcBody->pProcResult->procSign;

        if (uPos < procSign->params.size()) {
            u8 uInCount = get_input_param_count(procSign);
            Assert(uPos >= uInCount, "cannot assign to input param");
            const TypeInfo* pParamType = procSign->params[uPos].pBinding->pType;
            u32 uSlotsCount = get_slots_count(pParamType);
            Assert_(uSlotsCount); // otherwise not "assignable" as far as IR is concerned
            u32 uAlignPow2 = get_log2_of_align_bytes(pParamType);
            Assert_(uFormat == get_ir_format(pParamType));
            if (uPos == uInCount && uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like retparam
                sprintf(pBuffer, "outP%u", uPos);
            } else { // by-pointer out-param
                sprintf(pBuffer, "*outRP%u", uPos);
            }

        } else {
            IRRepo* pRepo = &(pIfProcBody->pProcResult->procwiseRepo);
            IREntry& entry = ir_access_repo_instr(pRepo, uPos);
            Assert_(u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION);
            Assert_(0 == (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST));
            Assert_(entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_ASSIGNABLE);
            u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
            u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
            Assert_(uSlotsCount); // otherwise not "assignable" as far as IR is concerned
            Assert_(uFormat == u8(entry.uInstrCodeAndFormatAndFirstParam >> 8));
            if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like var
                sprintf(pBuffer, "locV%u", uPos);
            } else { // table-like var
                sprintf(pBuffer, "*locRV%u", uPos);
            }
        }

    } else { Assert_(uLocation == IR_REPO_TEMPORARY);
        Assert(false, "assignable in temporary repo not supported");
    }
}

local_func void c_backend_emit_std_value_to(char* pBuffer, u64 uStdIR, u8 uFormat, CBckProcBody* pIfProcBody, CBckContext* pBckContext)
{
    Assert_(ir_is_valid_param(uStdIR));
    if (!ir_is_immediate(uStdIR)) {
        u16 uLocation = ir_get_repo_id(uStdIR);
        u32 uPos = ir_get_pos_in_repo(uStdIR);
        if (uLocation < IR_REPO_CURRENT_PROC) {
            IRRepo* pRepo = uLocation < IR_REPO_FILE_OFFSET ? 
                pBckContext->pProgCompilationState->tPrograwiseRepos + uLocation :
                &(pBckContext->pProgCompilationState->vecSourceFiles[uLocation - IR_REPO_FILE_OFFSET]->filewiseRepo);
            IREntry& entry = ir_access_repo_instr(pRepo, uPos);
            if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION) {
                u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
                u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                Assert_(uSlotsCount); // otherwise not really a value as far as IR is concerned
                Assert_(uFormat == u8(entry.uInstrCodeAndFormatAndFirstParam >> 8));
                if (0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT)) {
                    entry.uInstrMetaFlagsAndSecondParam |= IRFLAG_BCK_TO_EMIT;
                    pBckContext->vecOfDeclarationsToEmit.append(uStdIR);
                }
                if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST) {
                    if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_PROCBODY) {
                        // TODO
                        sprintf(pBuffer, "_procbody_value_not_yet_impl");
                    } else if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_RTTI) {
                        // TODO
                        sprintf(pBuffer, "_rtti_value_not_yet_impl");
                    } else {
                        if (uLocation < IR_REPO_FILE_OFFSET) {
                            // TODO
                            sprintf(pBuffer, "_filewise_global_const_not_yet_impl");
                        } else {
                            u32 uFileIndex = uLocation - IR_REPO_FILE_OFFSET;
                            if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like const
                                sprintf(pBuffer, "file%u_const%u", uFileIndex, uPos);
                            } else { // table-like const
                                sprintf(pBuffer, "*file%u_constR%u", uFileIndex, uPos);
                            }
                        }
                    }
                } else {
                    Assert(uLocation >= IR_REPO_FILE_OFFSET, "var in programwise globals not supported");
                    u32 uFileIndex = uLocation - IR_REPO_FILE_OFFSET;
                    if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like var
                        sprintf(pBuffer, "file%u_var%u", uFileIndex, uPos);
                    } else { // table-like var
                        sprintf(pBuffer, "*file%u_varR%u", uFileIndex, uPos);
                    }
                }
            } else {
                // TODO
                sprintf(pBuffer, "_non_decl_global_value_not_yet_impl");
            }
        } else if (uLocation == IR_REPO_CURRENT_PROC) {
            Assert_(pIfProcBody);
            const TypeInfo_ProcLike* procSign = pIfProcBody->pProcResult->procSign;

            if (uPos < procSign->params.size()) {
                u8 uInCount = get_input_param_count(procSign);
                const TypeInfo* pParamType = procSign->params[uPos].pBinding->pType;
                u32 uSlotsCount = get_slots_count(pParamType);
                Assert_(uSlotsCount); // otherwise not really a value as far as IR is concerned
                u32 uAlignPow2 = get_log2_of_align_bytes(pParamType);
                Assert_(uFormat == get_ir_format(pParamType));
                if (uPos < uInCount) {
                    if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like in-param
                        sprintf(pBuffer, "inP%u", uPos);
                    } else { // by-pointer in-param
                        sprintf(pBuffer, "*inRP%u", uPos);
                    }
                } else {
                    if (uPos == uInCount && uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like retparam
                        sprintf(pBuffer, "outP%u", uPos);
                    } else { // by-pointer out-param
                        sprintf(pBuffer, "*outRP%u", uPos);
                    }
                }
            } else {
                IRRepo* pRepo = &(pIfProcBody->pProcResult->procwiseRepo);
                IREntry& entry = ir_access_repo_instr(pRepo, uPos);
                if (u8(entry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION) {
                    u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
                    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    Assert_(uSlotsCount); // otherwise not really a value as far as IR is concerned
                    Assert_(uFormat == u8(entry.uInstrCodeAndFormatAndFirstParam >> 8));
                    if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST) {
                        if (0 == (entry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT)) {
                            entry.uInstrMetaFlagsAndSecondParam |= IRFLAG_BCK_TO_EMIT;
                            pIfProcBody->vecOfLocalConstDeclToEmit.append(uPos);
                        }
                        if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like const
                            sprintf(pBuffer, "file%u_proc%u_const%u",
                                pIfProcBody->pProcResult->iSourceFileIndex, pIfProcBody->pProcResult->uRegistrationIndex, uPos);
                        } else { // table-like const
                            sprintf(pBuffer, "*file%u_proc%u_constR%u",
                                pIfProcBody->pProcResult->iSourceFileIndex, pIfProcBody->pProcResult->uRegistrationIndex, uPos);
                        }
                    } else {
                        if (uSlotsCount == 1u && uAlignPow2 <= 3u) { // single C-like var
                            sprintf(pBuffer, "locV%u", uPos);
                        } else { // table-like var
                            sprintf(pBuffer, "*locRV%u", uPos);
                        }
                    }
                } else {
                    // TODO
                    sprintf(pBuffer, "_temporary_local_value_not_yet_impl");
                }
            }

        } else { Assert_(uLocation == IR_REPO_TEMPORARY);
            Assert(false, "value in temporary repo not supported");
        }

    } else {
        // TODO
        sprintf(pBuffer, "_immediate_not_yet_impl");
    }
}

local_func void c_backend_emit_proc_body(CBckProcBody* pProcBody, CBckContext* pBckContext)
{
    Assert_(pProcBody);
    Assert_(pProcBody->pProcResult);
    Assert_(pProcBody->pProcResult->iSourceFileIndex >= 0);
    pBckContext->pIsolatedSourceFile = pBckContext->pProgCompilationState->vecSourceFiles[u32(pProcBody->pProcResult->iSourceFileIndex)];

#if CHECK_ASSERTS
    {
        u64 uIRofProcDecl = pProcBody->pProcResult->uIRofProcDecl;
        Assert_(ir_is_valid_param(uIRofProcDecl));
        Assert_(!ir_is_immediate(uIRofProcDecl));
        u16 uLocationOfProcDecl = ir_get_repo_id(uIRofProcDecl);
        Assert_(uLocationOfProcDecl >= IR_REPO_FILE_OFFSET && uLocationOfProcDecl < IR_REPO_CURRENT_PROC);
        u32 uProcDeclFileIndex = u32(uLocationOfProcDecl - IR_REPO_FILE_OFFSET);
        SourceFileDescAndState* pProcDeclFile = pBckContext->pProgCompilationState->vecSourceFiles[uProcDeclFileIndex];
        u32 uPosOfProcDecl = ir_get_pos_in_repo(uIRofProcDecl);
        IREntry& procDeclEntry = ir_access_repo_instr(&(pProcDeclFile->filewiseRepo), uPosOfProcDecl);
        Assert_(u8(procDeclEntry.uInstrCodeAndFormatAndFirstParam) == IRIT_DECLARATION);
        Assert_(procDeclEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST);
        Assert_(procDeclEntry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_PROCBODY);
        Assert_(procDeclEntry.uInstrMetaFlagsAndSecondParam & IRFLAG_BCK_TO_EMIT);
    }
#endif

    char szBuffer[4096];

    #if TRACE_BACKEND_PRINTLEVEL > 1
    if (pProcBody->pProcResult->uIsForeignSource) {
        sprintf(szBuffer, "==== starting emission of #foreign proc %s from file %s ; symbol '%s'",
            (const char*)pProcBody->strProcName.begin(), pBckContext->pIsolatedSourceFile->sourceFileName.c_str(),
            pProcBody->pProcResult->foreignSymbolName.c_str());
        platform_log_info(szBuffer, true);
    } else {
        sprintf(szBuffer, "==== starting emission of proc %s from file %s", (const char*)pProcBody->strProcName.begin(),
            pBckContext->pIsolatedSourceFile->sourceFileName.c_str());
        platform_log_info(szBuffer, true);
    }
    #endif

    IRRepo* pRepo = &(pProcBody->pProcResult->procwiseRepo);
    u32 uInstructionsCount = pRepo->uSize;

    //TmpArray<u64> vecInstructions = pProcBody->pProcResult->vecInstructions;
    //u32 uInstructionsCount = vecInstructions.size();

    //pProcBody->procSignDef = c_backend_create_proc_representative_from(pBckContext->pWorker->tmpArena,
    //    pProcBody->pProcResult->procSign, pBckContext);
    pProcBody->uTotalParamsCount = pProcBody->pProcResult->procSign->params.size();

    char szProcBodyId[1024];
    c_backend_emit_name_of_proc_to_buffer(szProcBodyId, pProcBody->pProcResult, pBckContext);

    u32 uErrCheckCount = pProcBody->pProcResult->vecErrChecks.size();
    if (uErrCheckCount) {
        sprintf(szBuffer, "\nconstexpr u32 %s_err_table_[%u] = {\n", szProcBodyId, 7u * pProcBody->pProcResult->vecErrChecks.size());
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
        for(u32 uErrCheck = 0; uErrCheck < uErrCheckCount; uErrCheck++) {
            LocalErrCheck& errCheck = pProcBody->pProcResult->vecErrChecks[uErrCheck];
            Assert_(errCheck.iSourceFile >= 0 && u32(errCheck.iSourceFile) < pBckContext->pProgCompilationState->vecSourceFiles.size());
            sprintf(szBuffer, "\t%uu, %uu, %uu, %uu, %uu, %uu, %uu,\n",
                u32(errCheck.uFlagsAndKind & 0x00FFu),
                u32(errCheck.iSourceFile), errCheck.uBlockIndex, errCheck.uStatement, errCheck.uTokenRef, 
                errCheck.uPosOfCheck, errCheck.uPosOfInstr);
            platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
        }
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)"};", u32(sizeof("};")-1u));
    }

    bool assumedTrue = c_backend_try_emit_proc_sign_to(szBuffer, pProcBody->pProcResult->procSign, szProcBodyId, pBckContext);
    Assert_(assumedTrue);
    char szStaticKey[] = "\nstatic ";
    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szStaticKey, u32(sizeof(szStaticKey)-1u));
    if (pProcBody->pProcResult->uIsForeignSource) {
        char szInlineKey[] = "FORCE_INLINE ";
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szInlineKey, u32(sizeof(szInlineKey)-1u));
    }
    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
    //platform_write_to_file(pBckContext->fileForGlobalDecls, (const u8*)szBuffer, u32(strlen(szBuffer)));
    //char szDeclEnd[] = ";\n";
    //platform_write_to_file(pBckContext->fileForGlobalDecls, (const u8*)szDeclEnd, u32(sizeof(szDeclEnd)-1u));
    char szDefIntro[] = "\n{\n";
    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szDefIntro, u32(sizeof(szDefIntro)-1u));
    
    const TypeInfo_ProcLike* procSign = pProcBody->pProcResult->procSign;
    u32 uInParamsCount = u32(get_input_param_count(procSign));
    bool bHasFirstRetParam = false;
    if (uInParamsCount < pProcBody->uTotalParamsCount) {
        bHasFirstRetParam = true;
        const TypeInfo* pFirstRetParamType = procSign->params[uInParamsCount].pBinding->pType;
        u32 uFirstRetParamSlotCount = get_slots_count(pFirstRetParamType);
        if (LIKELY(uFirstRetParamSlotCount)) {
            u32 uFirstRetParamAlignPow2 = get_log2_of_align_bytes(pFirstRetParamType);
            if (uFirstRetParamAlignPow2 <= 3u) {
                u8 uFirstRetParamFormat = get_ir_format(pFirstRetParamType);
                if (uFirstRetParamSlotCount == 1u) { // nominal retparam: declared as the 'return type' of a c function
                    if (0 == pProcBody->pProcResult->uIsForeignSource) {
                        sprintf(szBuffer, "\t%s outP%u;\n", c_backend_get_type_identifier_for_format(uFirstRetParamFormat), uInParamsCount);
                        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
                    }
                } else { // more than 1 slot => first retparam name already in proc signature
                    bHasFirstRetParam = false;
                }
            } else { // more than 1 slot => first retparam name already in proc signature
                bHasFirstRetParam = false;
            }
        } else {
            if (0 == pProcBody->pProcResult->uIsForeignSource) {
                sprintf(szBuffer, "\t_DummyEmptyStruct outP%u;\n", uInParamsCount);
                platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
            }
        }
    }

    if (pProcBody->pProcResult->uIsForeignSource) {
        sprintf(szBuffer, "\tforeign_source_call_not_yet_implemented();\n}\n");
        return;
        #if 0
        const TypeInfo_ProcLike* pProcSign = pProcBody->pProcResult->procSign;
        char szForeignDecl[2048u];
        if (bHasFirstRetParam) {
            Assert_(uInParamsCount < pProcBody->uTotalParamsCount);
            u32 uFirstParamFormat32 = c_backend_get_format32_from_param(pProcBody->procSignDef, uInParamsCount);
            u32 uAlignPow2, uSlotCount; u8 uIRFormat;
            c_backend_format32_unpack(uFirstParamFormat32, &uAlignPow2, &uSlotCount, &uIRFormat);
            Assert_(uSlotCount == 1u);
            sprintf(szBuffer, "\treturn reinterpret_cast<%s>( ", c_backend_get_type_identifier_for_format(uIRFormat));

            const TypeInfo* pTypeSource = unalias(pProcSign->params[uInParamsCount].pBinding->pType);
            int iIndirectionCount = 0;
            while (get_type_kind(pTypeSource) == ETYPEKIND_POINTER) {
                iIndirectionCount++;
                pTypeSource = unalias(((const TypeInfo_Pointer*)pTypeSource)->pPointedToType);
            }
            if (pTypeSource == g_pCoreTypesInfo[ECORETYPE_RAWPTR])
                iIndirectionCount++;
            if (iIndirectionCount) {
                sprintf(szForeignDecl, "FOREIGN_PROC void");
                for (int i=0; i<iIndirectionCount; i++)
                    sprintf(szForeignDecl + strlen(szForeignDecl), "* ");
            } else {
                sprintf(szForeignDecl, "FOREIGN_PROC %s ", c_backend_get_type_identifier_for_format(uIRFormat));
            }
        } else {
            if (uInParamsCount < pProcBody->uTotalParamsCount)
                Assert(false, "non-standard ret value on foreign source not yet implemented");
            sprintf(szBuffer, "\t");
            sprintf(szForeignDecl, "FOREIGN_PROC void ");
        }

        sprintf(szBuffer + strlen(szBuffer), "%s(", pProcBody->pProcResult->foreignSymbolName.c_str());
        sprintf(szForeignDecl + strlen(szForeignDecl), "%s(", pProcBody->pProcResult->foreignSymbolName.c_str());

        for (u8 uInput = 0; uInput < uInParamsCount; uInput++) {
            const TypeInfo* pTypeSource = unalias(pProcSign->params[uInput].pBinding->pType);
            u8 uIRFormat = get_ir_format(pTypeSource);
            Assert_(uIRFormat == get_ir_format(pProcSign->params[uInput].pBinding->pType));
            int iIndirectionCount = 0;
            while (get_type_kind(pTypeSource) == ETYPEKIND_POINTER) {
                iIndirectionCount++;
                pTypeSource = unalias(((const TypeInfo_Pointer*)pTypeSource)->pPointedToType);
            }
            if (pTypeSource == g_pCoreTypesInfo[ECORETYPE_RAWPTR])
                iIndirectionCount++;
            if (iIndirectionCount) {
                Assert_(pBckContext->pCompilationParams->bPtrSizeIs64); // TODO: other target archs
                Assert_(uIRFormat == 0x03u);
                sprintf(szBuffer + strlen(szBuffer), "reinterpret_cast<void");
                sprintf(szForeignDecl + strlen(szForeignDecl), "void");
                for (int i = 0; i < iIndirectionCount; i++) {
                    sprintf(szBuffer + strlen(szBuffer), "*");
                    sprintf(szForeignDecl + strlen(szForeignDecl), "*");
                }
                if (uInput < uInParamsCount-1u) {
                    sprintf(szBuffer + strlen(szBuffer), ">(inP%u), ", uInput);
                    sprintf(szForeignDecl + strlen(szForeignDecl), ", ");
                } else
                    sprintf(szBuffer + strlen(szBuffer), ">(inP%u)", uInput);
            } else {
                if (uInput < uInParamsCount-1u) {
                    sprintf(szBuffer + strlen(szBuffer), "inP%u, ", uInput);
                    sprintf(szForeignDecl + strlen(szForeignDecl), "%s, ", c_backend_get_type_identifier_for_format(uIRFormat));
                } else {
                    sprintf(szBuffer + strlen(szBuffer), "inP%u", uInput);
                    sprintf(szForeignDecl + strlen(szForeignDecl), "%s", c_backend_get_type_identifier_for_format(uIRFormat));
                }
            }
        }

        if (bHasFirstRetParam)
            sprintf(szBuffer + strlen(szBuffer), ") );\n}\n");
        else
            sprintf(szBuffer + strlen(szBuffer), ");\n}\n");
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));

        sprintf(szForeignDecl + strlen(szForeignDecl), ");\n");
        platform_write_to_file(pBckContext->fileForGlobalDecls, (const u8*)szForeignDecl, u32(strlen(szForeignDecl)));

        return true;
        #endif
    }

    if (uErrCheckCount) {
        sprintf(szBuffer, "\tu32 " ERR_CHECK_SPECIAL_VAR_NAME ";\n\n");
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
    }

    ArenaRefPoint beforeTmp = get_arena_ref_point(pBckContext->secondaryTmpArena);
    TmpStringArray dumbassVecOfAllLines(pBckContext->secondaryTmpArena);
    for (u32 uInstrIndex = pProcBody->uTotalParamsCount; uInstrIndex < uInstructionsCount; uInstrIndex++) {
        IREntry& entry = ir_access_repo_instr(pRepo, uInstrIndex);
        u8 uIRIT = u8(entry.uInstrCodeAndFormatAndFirstParam);
        u8 uStdFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 8);
        u64 uFirstStdIR = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
        u64 uSecondStdIR = entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_MASK;
        switch (uIRIT) {
            
            case IRIT_MARKER_JUMP_TARGET: {
                sprintf(szBuffer, "\tlabel_%u:\n", uInstrIndex);
                dumbassVecOfAllLines.append(szBuffer);
            } break;

            case IRIT_NO_OP:
            case IRIT_MARKER_START_SOURCE_SCOPE:
            case IRIT_MARKER_POP_TO_SCOPE:
            {
                // NOOP for C Backend
            } break;

            case IRIT_RET: {
                if (bHasFirstRetParam)
                    sprintf(szBuffer, "\t\treturn outP%u;\n", uInParamsCount);
                else
                    sprintf(szBuffer, "\t\treturn;\n");
                dumbassVecOfAllLines.append(szBuffer);
            } break;

            case IRIT_DECLARATION: {
                if (0 == (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_DECL_IS_CONST)) {
                    u32 uAlignPow2 = u32(entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT);
                    u32 uSlotsCount = u32(entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT);
                    // TODO
                    sprintf(szBuffer, "\t\tlocal_declaration_noy_yet_impl;\n");
                    dumbassVecOfAllLines.append(szBuffer);
                    //c_backend_declare_local_var_or_tmp("var", uInstrIndex, uAlignPow2, uSlotsCount, uStdFormat, pProcBody, pBckContext);
                } // otherwise NOOP : consts will get emitted as global on-demand.
            } break;

            case IRIT_STORE: {
                sprintf(szBuffer, "\t\t");
                c_backend_emit_std_assignable_to(szBuffer + strlen(szBuffer), uFirstStdIR, uStdFormat, pProcBody, pBckContext);
                sprintf(szBuffer + strlen(szBuffer), " = ");
                c_backend_emit_std_value_to(szBuffer + strlen(szBuffer), uSecondStdIR, uStdFormat, pProcBody, pBckContext);
                sprintf(szBuffer + strlen(szBuffer), ";\n");
                dumbassVecOfAllLines.append(szBuffer);
            } break;

            case IRIT_STORE_EXT: {
                sprintf(szBuffer, "\t\t_store_ext_not_yet_impl;\n");
                dumbassVecOfAllLines.append(szBuffer);
            } break;

            default:
                sprintf(szBuffer, "\t\t%s_not_handled;\n", c_backend_get_op_name(uIRIT));
                dumbassVecOfAllLines.append(szBuffer);
        }

        #if 0
        u64 uInstrEncoding = vecInstructions[uInstrIndex];
        u64 uFourMsbs = uInstrEncoding & 0x0Fu;
        if (uFourMsbs == 0) { // binding
            ValueBinding* pAsBinding = reinterpret_cast<ValueBinding*>(uInstrEncoding);
            if (u8(pAsBinding->uScopeAndWhenConstFlags) <= EScopeKind::SCOPEKIND_GLOBAL_PRIVATE) {
                u64 uHash = get_map_hash(pAsBinding);
                auto itAlready = pBckContext->setOfAlreadyEmittedGlobalBindings.findHashed(uHash, pAsBinding);
                if (itAlready == pBckContext->setOfAlreadyEmittedGlobalBindings.end())
                    pBckContext->setOfGlobalBindingsToEmit.insertHashed(uHash, pAsBinding);

            } else {
                Assert_(is_value_const(pAsBinding));
                u64 uHash = get_map_hash(pAsBinding);
                auto itAlready = pProcBody->setOfAlreadyEmittedLocalBindings.findHashed(uHash, pAsBinding);
                if (itAlready == pProcBody->setOfAlreadyEmittedLocalBindings.end())
                    pProcBody->setOfLocalBindingsToEmit.insertHashed(uHash, pAsBinding);
            }
            pProcBody->tInstrFormats[uInstrIndex] = c_backend_get_format32_from_type(pAsBinding->pType, pBckContext);

        } else if (uFourMsbs == 1) { // extended instruction
            platform_log_error("*** extended instructions not yet implemented", true);
            pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
        } else {
            u8 uInstr = u8(uInstrEncoding);
            u8 uStdFormat = u8(uInstrEncoding>>8);
            u32 uStdFirstOp = u32(uInstrEncoding>>16) & IR_INSTRUCTION_PARAM_MASK;
            u32 uStdSecondOp = u32(uInstrEncoding>>40);
            switch (uInstr) {

                case IRIT_MARKER_START_SOURCE_SCOPE: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    sprintf(szBuffer, "\t\t// <--- Starting-Scope-Marker for Block in Source-Ast, #%u\n", uInstrIndex);
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_MARKER_POP_TO_SCOPE: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    sprintf(szBuffer, "\t\t// ---> Ending-Scope-Maker for Block in Source-Ast, #%u\n", uStdFirstOp);
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_MARKER_JUMP_TARGET: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    sprintf(szBuffer, "\tlabel_%u:\n", uInstrIndex);
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_RESERVE_LOCAL_VAR: {
                    u32 uAlignPow2 = uStdFirstOp;
                    u32 uSlotsCount = uStdSecondOp;
                    if (uInstrIndex < pProcBody->uTotalParamsCount) { // already defined as proc param
                        u32 uFormat32 = c_backend_get_format32_from_data(uAlignPow2, uSlotsCount, uStdFormat, pBckContext);
                        Assert_(uFormat32 == c_backend_get_format32_from_param(pProcBody->procSignDef, uInstrIndex));
                        pProcBody->tInstrFormats[uInstrIndex] = uFormat32;
                    } else {
                        // note: will also setup pProcBody->tInstrFormats[uInstrIndex]
                        c_backend_declare_local_var_or_tmp("var", uInstrIndex, uAlignPow2, uSlotsCount, uStdFormat, pProcBody, pBckContext);
                    }
                } break;

                case IRIT_STORE: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    sprintf(szBuffer, "\t\t");
                    c_backend_emit_regular_from_non_immediate_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), " = ");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdSecondOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ";\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_STORE_MULTI: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    u32 uNextInstruction = uInstrIndex + 1u;
                    u64 uAdditionalParamEncoding = vecInstructions[uNextInstruction];
                    Assert_(u8(uAdditionalParamEncoding) == IRIT_CALLER_PROC_PARAM);
                    Assert_(u8(uAdditionalParamEncoding >> 8) == 0x02u);
                    Assert_(u32(uAdditionalParamEncoding >> 40) == 1u);
                    u32 uSlotsCountIR = u32(uAdditionalParamEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                    Assert_(ir_is_uint_immediate(uSlotsCountIR));
                    u32 uSlotsCount = ir_get_as_uint_immediate(uSlotsCountIR);
                    Assert_(uSlotsCount > 1u);
                    sprintf(szBuffer, "\t\tfor(int iToStore=0; iToStore<%d; iToStore++)\n", int(uSlotsCount));
                    dumbassVecOfAllLines.append(szBuffer);
                    
                    char szDest[1024];
                    bool bIsRefDest; u32 uAlignPow2Dest, uSlotsCountDest;
                    u8 uFormatOfDest = c_backend_interpret_standard_ir_parameter_to(szDest, uStdFirstOp, pProcBody, pBckContext,
                        &bIsRefDest, &uAlignPow2Dest, &uSlotsCountDest, ECBckInterp::ECBKINTERP_REGULAR);
                    Assert_(uFormatOfDest == uStdFormat);
                    char szSrc[1024];
                    bool bIsRefSrc; u32 uAlignPow2Src, uSlotsCountSrc;
                    u8 uFormatOfSrc = c_backend_interpret_standard_ir_parameter_to(szSrc, uStdSecondOp, pProcBody, pBckContext,
                        &bIsRefSrc, &uAlignPow2Src, &uSlotsCountSrc, ECBckInterp::ECBKINTERP_REGULAR);
                    Assert_(uFormatOfSrc == uStdFormat);

                    if (bIsRefDest)
                        sprintf(szBuffer, "\t\t\t%s[iToStore] = ", szDest);
                    else
                        sprintf(szBuffer, "\t\t\t*(&%s + iToStore) = ", szDest);

                    if (bIsRefSrc)
                        sprintf(szBuffer + strlen(szBuffer), "%s[iToStore];\n", szSrc);
                    else
                        sprintf(szBuffer + strlen(szBuffer), "*(&%s + iToStore);\n", szSrc);

                    dumbassVecOfAllLines.append(szBuffer);
                    uInstrIndex = uNextInstruction;
                } break;

                case IRIT_RESET_TO_ZERO: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    if (ir_is_immediate(uStdSecondOp)) {
                        Assert_(ir_is_uint_immediate(uStdSecondOp));
                        u32 uConstSlotCount = ir_get_as_uint_immediate(uStdSecondOp);
                        Assert_(uConstSlotCount);
                        if (uConstSlotCount == 1u) {
                            sprintf(szBuffer, "\t\t");
                            c_backend_emit_regular_from_non_immediate_param_to(szBuffer + strlen(szBuffer),
                                uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                            sprintf(szBuffer + strlen(szBuffer), " = ");
                            c_backend_emit_regular_from_immediate_to(szBuffer + strlen(szBuffer),
                                IR_REF_ZEROING_ANY, uStdFormat, pProcBody, pBckContext);
                            sprintf(szBuffer + strlen(szBuffer), ";\n");
                            dumbassVecOfAllLines.append(szBuffer);
                            break;
                        }
                    }

                    sprintf(szBuffer, "\t\tfor(u32 uToReset=0; uToReset < ");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdSecondOp, 0x02u, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), "; uToReset++)\n");
                    dumbassVecOfAllLines.append(szBuffer);
                    char szDest[1024];
                    bool bIsRefDest; u32 uAlignPow2Dest, uSlotsCountDest;
                    u8 uFormatOfDest = c_backend_interpret_standard_ir_parameter_to(szDest, uStdFirstOp, pProcBody, pBckContext,
                        &bIsRefDest, &uAlignPow2Dest, &uSlotsCountDest, ECBckInterp::ECBKINTERP_RAW);
                    Assert_(uFormatOfDest == uStdFormat);
                    if (bIsRefDest)
                        sprintf(szBuffer, "\t\t\t%s[uToReset] = ", szDest);
                    else
                        sprintf(szBuffer, "\t\t\t*(&%s + uToReset) = ", szDest);
                    c_backend_emit_regular_from_immediate_to(szBuffer + strlen(szBuffer),
                        IR_REF_ZEROING_ANY, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ";\n");
                    dumbassVecOfAllLines.append(szBuffer);

                } break;

                case IRIT_BIT_AND:
                case IRIT_BIT_OR:
                case IRIT_BIT_XOR:
                case IRIT_ADD:
                case IRIT_SUB:
                case IRIT_MUL:
                case IRIT_MUL_UINT:
                case IRIT_DIV:
                case IRIT_MOD:
                case IRIT_DIV_UINT:
                case IRIT_REM_SINT:
                case IRIT_REM_UINT:
                case IRIT_POW_FP:
                case IRIT_LEFT_SHIFT:
                case IRIT_RIGHT_SHIFT_SIGNED:
                case IRIT_RIGHT_SHIFT:
                case IRIT_LEFT_ROTATE:
                case IRIT_RIGHT_ROTATE:
                    // also, extended:
                case IRIT_ADD_EXT:
                case IRIT_SUB_EXT:
                case IRIT_MUL_EXT:
                case IRIT_MUL_UINT_EXT:
                case IRIT_POW_N:
                case IRIT_POW_N_UINT:
                {
                    u32 uAlignPow2 = get_log2_of_natural_align_from_format(uStdFormat);
                    // note: will also setup pProcBody->tInstrFormats[uInstrIndex]
                    bool bIsRef = c_backend_declare_local_var_or_tmp("tmp", uInstrIndex, uAlignPow2, 1u, uStdFormat, pProcBody, pBckContext);
                    Assert_(!bIsRef);
                    sprintf(szBuffer, "\t\ttmp%u = %s%s(", uInstrIndex,
                        c_backend_get_op_name(uInstr), c_backend_get_type_identifier_for_format(uStdFormat));
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ", ");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdSecondOp, uStdFormat, pProcBody, pBckContext);
                    if (uInstr == IRIT_ADD_EXT || uInstr == IRIT_SUB_EXT) {
                        u64 uOvfEncoding = vecInstructions[uInstrIndex + 1u];
                        Assert_(u8(uOvfEncoding) == IRIT_CALLER_PROC_RESULT);
                        Assert_(u8(uOvfEncoding >> 8) == 0x00u);
                        Assert_(u32(uOvfEncoding >> 40) == 1u);
                        Assert_((u32(uOvfEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK) == uInstrIndex);
                        c_backend_declare_local_var_or_tmp("sup", uInstrIndex + 1u, 0u, 1u, 0x00u, pProcBody, pBckContext);
                        sprintf(szBuffer + strlen(szBuffer), ",\n\t\t\t&sup%u", uInstrIndex + 1u);

                        u64 uCarOrUnderflowFPEncoding = vecInstructions[uInstrIndex + 2u];
                        Assert_(u8(uCarOrUnderflowFPEncoding) == IRIT_CALLER_PROC_RESULT);
                        Assert_(u8(uCarOrUnderflowFPEncoding >> 8) == 0x00u);
                        Assert_(u32(uCarOrUnderflowFPEncoding >> 40) == 1u);
                        Assert_((u32(uCarOrUnderflowFPEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK) == uInstrIndex);
                        c_backend_declare_local_var_or_tmp("sup", uInstrIndex + 2u, 0u, 1u, 0x00u, pProcBody, pBckContext);
                        sprintf(szBuffer + strlen(szBuffer), ", &sup%u", uInstrIndex + 2u);

                        uInstrIndex += 2u;

                    } else if (uInstr == IRIT_MUL_EXT || uInstr == IRIT_MUL_UINT_EXT || uInstr == IRIT_POW_N_UINT ||
                            (uInstr == IRIT_POW_N && uStdFormat <= 0x05u)) {
                        u64 uOvfEncoding = vecInstructions[uInstrIndex + 1u];
                        Assert_(u8(uOvfEncoding) == IRIT_CALLER_PROC_RESULT);
                        Assert_(u8(uOvfEncoding >> 8) == 0x00u);
                        Assert_(u32(uOvfEncoding >> 40) == 1u);
                        Assert_((u32(uOvfEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK) == uInstrIndex);
                        c_backend_declare_local_var_or_tmp("sup", uInstrIndex + 1u, 0u, 1u, 0x00u, pProcBody, pBckContext);
                        sprintf(szBuffer + strlen(szBuffer), ",\n\t\t\t&sup%u", uInstrIndex + 1u);
                    
                        uInstrIndex += 1u;
                    }
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_BOOLEAN_NOT:
                case IRIT_ABS:
                case IRIT_NEG:
                case IRIT_BIT_NOT:
                {
                    u32 uAlignPow2 = get_log2_of_natural_align_from_format(uStdFormat);
                    // note: will also setup pProcBody->tInstrFormats[uInstrIndex]
                    bool bIsRef = c_backend_declare_local_var_or_tmp("tmp", uInstrIndex, uAlignPow2, 1u, uStdFormat, pProcBody, pBckContext);
                    Assert_(!bIsRef);
                    sprintf(szBuffer, "\t\ttmp%u = %s%s(", uInstrIndex,
                        c_backend_get_op_name(uInstr), c_backend_get_type_identifier_for_format(uStdFormat));
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_ARE_EQUAL:
                case IRIT_IS_LESS_SINT:
                case IRIT_IS_LESS_UINT:
                {
                    // note: will also setup pProcBody->tInstrFormats[uInstrIndex]
                    bool bIsRef = c_backend_declare_local_var_or_tmp("tmp", uInstrIndex, 0u, 1u, 0x00u, pProcBody, pBckContext);
                    Assert_(!bIsRef);
                    sprintf(szBuffer, "\t\ttmp%u = %s%s(", uInstrIndex,
                        c_backend_get_op_name(uInstr), c_backend_get_type_identifier_for_format(uStdFormat));
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ", ");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdSecondOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_PROC_CALL: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    u8 uCallConvention = uStdFormat;
                    u8 uInParamsCount = u8(uInstrEncoding >> 40) & 0x1Fu;
                    u8 uOutParamsCount = u8(uInstrEncoding >> 45) & 0x1Fu;
                    u8 uDefaultTrail = u8(uInstrEncoding >> 50) & 0x1Fu;
                    u8 bIsComptimeBindingToProc = u8(uInstrEncoding >> 55) & 0x1u;
                    u8 bIsProcConstexpr = u8(uInstrEncoding >> 56) & 0x1u;
                    u8 bIsTailCall = u8(uInstrEncoding >> 57) & 0x1u;
                    sprintf(szBuffer, "\t\t");
                    if (bIsComptimeBindingToProc) {
                        c_backend_emit_proc_call_from_procresult_param_to(szBuffer + strlen(szBuffer),
                            uStdFirstOp, 0x03u, pProcBody, pBckContext);
                    } else {
                        sprintf(szBuffer + strlen(szBuffer), "_call_from_procptr_not_yet_impl_(");
                    }
                    if (uDefaultTrail) {
                        sprintf(szBuffer + strlen(szBuffer), "_args_with_default_trail_not_yet_impl_");
                    } else {
                        char szParamBuffer[1024];
                        u32 uInstrIndexParam = uInstrIndex;
                        for (u32 uInParam = 0; uInParam < uInParamsCount; uInParam++) {
                            uInstrIndexParam++;
                            u64 uParamPushInstrEncoding = vecInstructions[uInstrIndexParam];
                            Assert_(u8(uParamPushInstrEncoding) == IRIT_CALLER_PROC_PARAM);
                            pProcBody->tInstrFormats[uInstrIndexParam] = INVALID_FORMAT32;
                            u8 uEncodedFormat = u8(uParamPushInstrEncoding >> 8);
                            u32 uParamIR = u32(uParamPushInstrEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                            u32 uEncodedSlotsCount =  u32(uParamPushInstrEncoding >> 40);
                            if (ir_is_immediate(uParamIR)) {
                                Assert_(uEncodedSlotsCount == 1u);
                                c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                                    uParamIR, uEncodedFormat, pProcBody, pBckContext);
                                if (uInParam < uInParamsCount-1u)
                                    sprintf(szBuffer + strlen(szBuffer), ", ");
                            } else {
                                bool bIsRef; u32 uAlignPow2, uSlotsCount;
                                u8 uFormat = c_backend_interpret_standard_ir_parameter_to(szParamBuffer, uParamIR, pProcBody, pBckContext,
                                    &bIsRef, &uAlignPow2, &uSlotsCount, ECBckInterp::ECBKINTERP_RAW);
                                Assert_(uFormat == uEncodedFormat || uEncodedSlotsCount == 0);
                                // TODO: we need align for IRIT_CALLER_PROC_PARAM !!! 
                                if (uEncodedSlotsCount > 1u /*|| (uEncodedSlotsCount && uEncodedAlignPow2 > 4u && uEncodedAlignPow2 > get_log2_of_natural_align_from_format(uFormat))*/) {
                                    sprintf(szBuffer + strlen(szBuffer), "%s%s%s", bIsRef ? "":"&", szParamBuffer,
                                        uInParam < uInParamsCount-1u ? ", ":"");
                                } else {
                                    sprintf(szBuffer + strlen(szBuffer), "%s%s%s", bIsRef ? "*":"", szParamBuffer,
                                        uInParam < uInParamsCount-1u ? ", ":"");
                                }
                            }
                        }
                        bool bAlreadyDividedOutput = false;
                        for (u32 uOutParam = 0; uOutParam < uOutParamsCount; uOutParam++) {
                            u32 uParamIndex = uOutParam + uInParamsCount;
                            uInstrIndexParam++;
                            u64 uParamPopInstrEncoding = vecInstructions[uInstrIndexParam];
                            Assert_(u8(uParamPopInstrEncoding) == IRIT_CALLER_PROC_RESULT); // TODO: also direct results to var
                            u8 uEncodedFormat = u8(uParamPopInstrEncoding >> 8);
                            u32 uAssociatedCall = u32(uParamPopInstrEncoding >> 16) & IR_INSTRUCTION_PARAM_MASK;
                            Assert_(uAssociatedCall == uInstrIndex);
                            u32 uEncodedSlotsCount =  u32(uParamPopInstrEncoding >> 40);
                            u32 uAlignPow2 = get_log2_of_natural_align_from_format(uEncodedFormat); // TODO: we need align for IRIT_CALLER_PROC_RESULT !!! 
                            // note: will also setup pProcBody->tInstrFormats[uInstrIndexParam]
                            bool bIsRef = c_backend_declare_local_var_or_tmp("res", uInstrIndexParam,
                                uAlignPow2, uEncodedSlotsCount, uEncodedFormat, pProcBody, pBckContext);
                            // TODO: we need align for IRIT_CALLER_PROC_RESULT !!!                             
                            if (uOutParam || uEncodedSlotsCount > 1u /*|| (uEncodedSlotsCount && uEncodedAlignPow2 > 4u && uEncodedAlignPow2 > get_log2_of_natural_align_from_format(uFormat))*/) {
                                if (!bAlreadyDividedOutput) {
                                    sprintf(szBuffer + strlen(szBuffer), ",\n\t\t\t\t");
                                    bAlreadyDividedOutput = true;
                                }
                                sprintf(szBuffer + strlen(szBuffer), "%sres%u%s", bIsRef ? "":"&", uInstrIndexParam,
                                    uOutParam < uOutParamsCount-1u ? ", ":"");
                            } else {
                                char szBufferHackLineBeforeForReturnValue[1024];
                                sprintf(szBufferHackLineBeforeForReturnValue, "\t\t%sres%u = \n\t", bIsRef ? "*":"", uInstrIndexParam);
                                dumbassVecOfAllLines.append(szBufferHackLineBeforeForReturnValue);
                            }
                        }
                        uInstrIndex = uInstrIndexParam;
                    }
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);

                } break;

                case IRIT_OFFSET:
                case IRIT_OFFSET_EXT:
                case IRIT_REINTERP:
                {
                    // Those instructions are not directly handled when encountered: they are handled when referenced...
                    u32 uAlignPow2 = get_log2_of_natural_align_from_format(uStdFormat);
                    u32 uFormat32 = c_backend_get_format32_from_data(uAlignPow2, 1u, uStdFormat, pBckContext);
                    pProcBody->tInstrFormats[uInstrIndex] = uFormat32;
                    if (uInstr == IRIT_OFFSET_EXT) {
                        // IRIT_OFFSET_EXT awaits an additional param (which is not handled here: also handled when referenced)
                        uInstrIndex += 1u;
                    }
                } break;

                case IRIT_RET: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    if (bHasFirstRetParam) {
                        sprintf(szBuffer, "\t\treturn outP%u;\n", uInParamsCount);
                        dumbassVecOfAllLines.append(szBuffer);
                    } else {
                        dumbassVecOfAllLines.append("\t\treturn;\n");
                    }
                } break;

                case IRIT_ERR_BRANCH: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    u8 bErrWhenNonZero = u8(uInstrEncoding >> 40) & 0x01u;
                    u8 bIsActive = u8(uInstrEncoding >> 41) & 0x01u;
                    if (bIsActive) 
                        sprintf(szBuffer, "\t\t");
                    else
                        sprintf(szBuffer, "\t\t// ");

                    u32 uErrCheckIndex = u32(uInstrEncoding >> 42);
                    sprintf(szBuffer + strlen(szBuffer), "{ if (%s", bErrWhenNonZero ? "":" ! (");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), "%s) goto " ERR_CHECK_FAILED_LABEL_NAME "%u; }\n",
                        bErrWhenNonZero ? "":") ", uErrCheckIndex);
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_CALLER_PROC_PARAM:
                case IRIT_CALLER_PROC_RESULT:
                {
                    Assert(false, "proc param or proc result instruction found outside the decoding of a caller");
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    dumbassVecOfAllLines.append("\t\t__unexpected_instruction__;\n");
                } break;

                case IRIT_GOTO: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    u32 uInstructionTo = uStdFirstOp;
                    sprintf(szBuffer, "\t\tgoto label_%u;\n", uInstructionTo);
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_BRANCH: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    u32 uInstructionTo = uStdFirstOp;
                    u8 uFormatOfArg = uStdFormat & 0x07u;
                    u8 bTakeBranchIfNonZero = (uStdFormat >> 3) & 0x01u;
                    if (bTakeBranchIfNonZero) {
                        sprintf(szBuffer, "\t\tif (");
                        c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                            uStdSecondOp, uFormatOfArg, pProcBody, pBckContext);
                        sprintf(szBuffer + strlen(szBuffer), ") goto label_%u;\n", uInstructionTo);
                    } else {
                        sprintf(szBuffer, "\t\tif (0 == (");
                        c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                            uStdSecondOp, uFormatOfArg, pProcBody, pBckContext);
                        sprintf(szBuffer + strlen(szBuffer), ")) goto label_%u;\n", uInstructionTo);
                    }
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_LONG_ADDRESS: {
                    Assert_(!ir_is_immediate(uStdSecondOp));
                    u32 uFormatOfReferenced = pProcBody->tInstrFormats[uStdSecondOp];
                    Assert_(uFormatOfReferenced != INVALID_FORMAT32);

                    u32 uFormat32 = c_backend_get_format32_from_data(0x03u, 1u, 0x03u, pBckContext);
                    pProcBody->tInstrFormats[uInstrIndex] = uFormat32;
                    sprintf(szBuffer, "\tr64 tmp%u;\n", uInstrIndex);
                    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));

                    sprintf(szBuffer, "\t\ttmp%u = reinterpret_cast<r64>(", uInstrIndex);
                    bool bIsRef; u32 uAlignPow2, uSlotsCount;
                    u8 uFormatOfParam = c_backend_interpret_standard_ir_parameter_to(szBuffer + strlen(szBuffer),
                        uStdSecondOp, pProcBody, pBckContext, &bIsRef, &uAlignPow2, &uSlotsCount, ECBckInterp::ECBKINTERP_REFERENCE);
                    Assert_(uSlotsCount);
                    Assert_(uFormatOfParam == u8(uFormatOfReferenced));
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_DEREF_LONG_ADDRESS: {
                    u32 uFormat32 = c_backend_get_format32_from_data(uStdFirstOp, 1u, uStdFormat, pBckContext);
                    pProcBody->tInstrFormats[uInstrIndex] = uFormat32;
                    sprintf(szBuffer, "\t%s* ref%u;\n", c_backend_get_type_identifier_for_format(uStdFormat), uInstrIndex);
                    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));

                    sprintf(szBuffer, "\t\tref%u = reinterpret_cast<%s*>(", uInstrIndex,
                        c_backend_get_type_identifier_for_format(uStdFormat));
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer), uStdSecondOp, 0x03u,
                        pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_RECALL: {
                    platform_log_error("*** RECALL not yet implemented", true);
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    dumbassVecOfAllLines.append("\t\t__unknown_instruction__;\n");
                } break;

                case IRIT_SNAPSHOT: {
                    platform_log_error("*** SNAPSHOT not yet implemented", true);
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    dumbassVecOfAllLines.append("\t\t__unknown_instruction__;\n");
                } break;

                case IRIT_CAST_SCALAR_ELEMS: {
                    platform_log_error("*** CAST_SCALAR_ELEMS not yet implemented", true);
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    dumbassVecOfAllLines.append("\t\t__unknown_instruction__;\n");
                } break;

                case IRIT_CAST_FP_TO_INT_SCALAR_ELEMS: {
                    platform_log_error("*** CAST_FP_TO_INT_SCALAR_ELEMS not yet implemented", true);
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    dumbassVecOfAllLines.append("\t\t__unknown_instruction__;\n");
                } break;

                case IRIT_TMP_BUILTIN_PRINT_REG: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    u8 uTmpPrintKind = u8(uStdSecondOp);
                    sprintf(szBuffer, "\t\tprint_%s_%s(",
                        c_backend_get_tmp_print_kind(uTmpPrintKind),
                        c_backend_get_type_identifier_for_format(uStdFormat));
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, uStdFormat, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_TMP_BUILTIN_PRINT_CSTRING: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    sprintf(szBuffer, "\t\tprint_cstring(");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, 0x03u, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                case IRIT_TMP_BUILTIN_PRINT_BYTES: {
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    sprintf(szBuffer, "\t\tprint_bytes(");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdFirstOp, 0x03u, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ", ");
                    c_backend_emit_regular_from_std_param_to(szBuffer + strlen(szBuffer),
                        uStdSecondOp, 0x02u, pProcBody, pBckContext);
                    sprintf(szBuffer + strlen(szBuffer), ");\n");
                    dumbassVecOfAllLines.append(szBuffer);
                } break;

                default:
                    platform_log_error("*** this instruction not yet implemented", true);
                    pProcBody->tInstrFormats[uInstrIndex] = INVALID_FORMAT32;
                    dumbassVecOfAllLines.append("\t\t__unknown_instruction__;\n");
            }
        }
        #endif
    }

    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)"\n", 1u);

    u32 uLineCount = dumbassVecOfAllLines.size();
    for (u32 uLine = 0; uLine < uLineCount; uLine++) {
        FFString strLine = dumbassVecOfAllLines[uLine];
        platform_write_to_file(pBckContext->fileForProcBodies, strLine.begin(), strLine.byte_length());
    }
    reset_arena_no_release_to(beforeTmp, pBckContext->secondaryTmpArena);

    if (uErrCheckCount) {
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)"\n", u32(sizeof("\n") - 1u));
        for (u32 uErrIndex = 0; uErrIndex < uErrCheckCount; uErrIndex++) {
            sprintf(szBuffer, "\t" ERR_CHECK_FAILED_LABEL_NAME "%u: " ERR_CHECK_SPECIAL_VAR_NAME " = %uu; goto "
                ERR_CHECK_FAILED_LABEL_NAME ";\n" , uErrIndex, uErrIndex);
            platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
        }
        sprintf(szBuffer, "\n\t" ERR_CHECK_FAILED_LABEL_NAME ":\n"
                          "\t\t" ERR_CHECK_FUNCTION_NAME "(" ERR_CHECK_SPECIAL_VAR_NAME ", %s_err_table_);\n", szProcBodyId);
        platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szBuffer, u32(strlen(szBuffer)));
    }

    char szDefEnd[] = "}\n";
    platform_write_to_file(pBckContext->fileForProcBodies, (const u8*)szDefEnd, u32(sizeof(szDefEnd)-1u));
}

#endif // LOCLIB_C_BACKEND_H_

