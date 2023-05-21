#pragma once 

#ifndef LOCLIB_IR_DUMP_H_
#define LOCLIB_IR_DUMP_H_

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


local_func void append_id_to_buffer(int iIdentifier, char* ioBuffer, IRAwareContext* pCompilationContext) {
    if (iIdentifier > 0) {
        StringView idStr = get_identifier_stringview(pCompilationContext->pProgCompilationState, iIdentifier);
        memcpy(ioBuffer, idStr.begin(), idStr.byte_length());
        ioBuffer[idStr.byte_length()] = '\0';
    } else {
        sprintf(ioBuffer, "<unnamed>");
    }
}

// predecl
void append_proc_sign_to_buffer(const TypeInfo_ProcLike* pProcSign, char* ioBuffer, IRAwareContext* pCompilationContext);

local_func void append_type_to_buffer(const TypeInfo* pType, char* ioBuffer, IRAwareContext* pCompilationContext) {
    if (pType) {
        ETypeKind eTypeKind = get_type_kind(pType);
        switch(eTypeKind) {
            case ETypeKind::ETYPEKIND_INTEGRAL: {
                const TypeInfo_Integral* pAsIntegral = (const TypeInfo_Integral*)pType;
                switch (get_core_type(pAsIntegral)) {
                    case ECoreType::ECORETYPE_COMPINT:
                        sprintf(ioBuffer, "COMPINT");
                        break;
                    case ECoreType::ECORETYPE_INT:
                        sprintf(ioBuffer, "INT");
                        break;
                    case ECoreType::ECORETYPE_NAT:
                        sprintf(ioBuffer, "NAT");
                        break;
                    case ECoreType::ECORETYPE_CODEPOINT:
                        sprintf(ioBuffer, "CODEPOINT");
                        break;
                    case ECoreType::ECORETYPE_BOOL:
                        sprintf(ioBuffer, "BOOL");
                        break;
                    default:
                        sprintf(ioBuffer, "%c%u",
                            is_signed(pAsIntegral) ? 'i' : (is_unsigned(pAsIntegral) ? 'u' : 'r'),
                            get_byte_count_of_scalar_elem(pAsIntegral) * 8u);
                }
            } break;

            case ETypeKind::ETYPEKIND_FLOATINGPOINT: {
                const TypeInfo_FloatingPoint* pAsFP = (const TypeInfo_FloatingPoint*)pType;
                switch (get_core_type(pAsFP)) {
                    case ECoreType::ECORETYPE_FLOAT_LIT:
                        sprintf(ioBuffer, "FLOAT_LIT");
                        break;
                    case ECoreType::ECORETYPE_XFLOAT:
                        sprintf(ioBuffer, "XFLOAT");
                        break;
                    default:
                        sprintf(ioBuffer, "f%u", get_byte_count_of_scalar_elem(pAsFP) * 8u);
                }
            } break;

            case ETypeKind::ETYPEKIND_OTHERCORE: {
                sprintf(ioBuffer, "Oth<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_HWVECTOR: {
                sprintf(ioBuffer, "HWV<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_ARRAY: {
                sprintf(ioBuffer, "Arr<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_SET: {
                sprintf(ioBuffer, "Set<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_MAP: {
                sprintf(ioBuffer, "Map<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_DISTINCTALIAS: {
                sprintf(ioBuffer, "Alias of ");
                append_type_to_buffer(unalias(pType), ioBuffer+strlen(ioBuffer), pCompilationContext);
            } break;

            case ETypeKind::ETYPEKIND_POINTER: {
                sprintf(ioBuffer, "Ptr<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_STRUCTLIKE: {
                sprintf(ioBuffer, "StructLike<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_ENUM: {
                sprintf(ioBuffer, "Enum<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_PROCLIKEBODY: {
                sprintf(ioBuffer, "ProcLikeBody { ");
                append_proc_sign_to_buffer((const TypeInfo_ProcLike*)pType, ioBuffer + strlen(ioBuffer), pCompilationContext);
                sprintf(ioBuffer + strlen(ioBuffer), " }");
            } break;

            case ETypeKind::ETYPEKIND_PROCLIKEOVERLOAD: {
                sprintf(ioBuffer, "POvld<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_PROCLIKEPOLYMORPH: {
                sprintf(ioBuffer, "PPoly<TODO>");
            } break;

            case ETypeKind::ETYPEKIND_STRUCTPOLYMORPH: {
                sprintf(ioBuffer, "SPoly<TODO>");
            } break;

            default:
                Assume_(false);
        }
    }
}

local_func void append_tc_only_info_to_buffer(NodeValue* pValue, char* ioBuffer, IRAwareContext* pCompilationContext) {
    Assert_(is_value_tc_only(pValue));
    if (pValue->pType == g_pCoreTypesInfo[ECORETYPE_COMPINT]) {
        u64 uPayload = pValue->info.metaValue.knownValue.uEmbeddedValue;
        u8 uIsNeg = u8(uPayload) & COMPINT_FLAG_IS_NEGATIVE;
        switch (uPayload & COMPINT_SIZE_MASK) {
            case COMPINT_SIZE_SMALL_EMBD: {
                u64 uAbsValue = uPayload >> COMPINT_VALUE_SHIFT_WHENSMALL;
                if (uIsNeg) {
                    sprintf(ioBuffer, "-%llu (-0x%llx)", uAbsValue, uAbsValue);
                } else {
                    sprintf(ioBuffer, "%llu (0x%llx)", uAbsValue, uAbsValue);
                }
            } break;

            case COMPINT_SIZE_2LEGS:
            case COMPINT_SIZE_NLEGS:

            case COMPINT_SIZE_1LEG: {
                if (uIsNeg) {
                    // TODO
                    sprintf(ioBuffer, "Large (-)compint TODO");
                } else {
                    // TODO
                    sprintf(ioBuffer, "Large (+)compint TODO");
                }
            } break;
            /*
            case COMPINT_SIZE_2LEGS: {
            } break;
            case COMPINT_SIZE_NLEGS: {
            } break;
            */
            default:
                Assume_(false);
        }
    } else if (pValue->pType == g_pCoreTypesInfo[ECORETYPE_TYPE]) {
        append_type_to_buffer(type_from_type_node(pValue), ioBuffer, pCompilationContext);
    } else {
        sprintf(ioBuffer, "<not-yet-handled tc-only representation>");
    }
}

local_func void append_ir_non_imm_param_to_buffer(u64 uIRParam, char* ioBuffer, IRAwareContext* pCompilationContext)
{
    Assert_(!ir_is_immediate(uIRParam));
    IRRepo* pRepo;
    u32 uIndex;
    SourceFileDescAndState* pSourceFile;
    EEntryKind eKind;
    ir_decode_non_imm(uIRParam, pCompilationContext, &pRepo, &uIndex, &pSourceFile, &eKind);
    switch (eKind) {

        case EEntryKind::EEK_PROGRAMWISE_ENTRY: {
            sprintf(ioBuffer, "Pg%08u     ", uIndex);
        } break;

        case EEntryKind::EEK_FILEWISE_CONST: {
            Assert_(pSourceFile);
            if (pSourceFile == pCompilationContext->pIsolatedSourceFile)
                sprintf(ioBuffer, "Cs%07u@thisf", uIndex);
            else
                sprintf(ioBuffer, "Cs%07u@%05u", uIndex, u32(pSourceFile->iRegistrationIndex));
        } break;

        case EEntryKind::EEK_FILEWISE_VAR: {
            Assert_(pSourceFile);
            if (pSourceFile == pCompilationContext->pIsolatedSourceFile)
                sprintf(ioBuffer, "Vr%07u@thisf", uIndex);
            else
                sprintf(ioBuffer, "Vr%07u@%05u", uIndex, u32(pSourceFile->iRegistrationIndex));
        } break;

        case EEntryKind::EEK_IS_PROCBODY_REF: {
            Assert_(pSourceFile);
            TCProcBodyRegistration* reg = pSourceFile->vecAllProcBodies[uIndex];
            if (pSourceFile == pCompilationContext->pIsolatedSourceFile)
                sprintf(ioBuffer, "Rp%07u@thisf", uIndex);
            else
                sprintf(ioBuffer, "Rp%07u@F%05u", uIndex, u32(pSourceFile->iRegistrationIndex));
        } break;

        case EEntryKind::EEK_CURRENT_PROC_LOCAL: {
            Assert_(pCompilationContext->pProcResult);
            if (uIndex < pCompilationContext->pProcResult->procSign->params.size()) {
                if (uIndex < get_input_param_count(pCompilationContext->pProcResult->procSign)) {
                    sprintf(ioBuffer, "Pi%07u      ", uIndex);
                } else {
                    sprintf(ioBuffer, "Po%07u      ", uIndex);
                }
            } else {
                sprintf(ioBuffer, "Ir%07u      ", uIndex);
            }
        } break;

        case EEntryKind::EEK_CURRENT_TEMPORARY: {
            sprintf(ioBuffer, "Tm%07u", uIndex);
        } break;

        case EEntryKind::EEK_NOT_AN_ENTRY:
            Assert_(false);
            break;

        default:
            Assume_(false);
    }
}

local_func void append_known_value_to_buffer(u64 uMetaFlags, AKnownValue knownValue, u8 uFormat, u32 uSlotsCount, char* ioBuffer, IRAwareContext* pCompilationContext)
{
    Assert_(irflag_is_known_or_nyka(uMetaFlags));
    if (irflag_is_known_embd(uMetaFlags)) {
        if (irflag_is_or_has_nyka(uMetaFlags)) {
            i32 iOffset;
            u64 uNykaToWhat = ir_decode_nyka_value(knownValue.uEmbeddedValue, &iOffset);
            Assert_(!ir_is_immediate(uNykaToWhat));
            sprintf(ioBuffer, "KnownValue [64b-embd] = NYKA to ");
            append_ir_non_imm_param_to_buffer(uNykaToWhat, ioBuffer + strlen(ioBuffer), pCompilationContext);
            if (iOffset) {
                sprintf(ioBuffer + strlen(ioBuffer), " %+d bytes", iOffset);
            }
        } else {
            if (uFormat & 0x08u) { // Floats
                sprintf(ioBuffer, "KnownValue [64b-embd] = <FP not yet implemented>");
            } else {
                if (uSlotsCount == 1u) {
                    switch(uFormat) {
                        case 0x00u:
                            sprintf(ioBuffer, "KnownValue [64b-embd] = 0x%02x as r8", u8(knownValue.uEmbeddedValue));
                            break;
                        case 0x01u:
                            sprintf(ioBuffer, "KnownValue [64b-embd] = 0x%04x as r16", u16(knownValue.uEmbeddedValue));
                            break;
                        case 0x02u:
                            sprintf(ioBuffer, "KnownValue [64b-embd] = 0x%08x as r32", u32(knownValue.uEmbeddedValue));
                            break;
                        case 0x03u:
                            sprintf(ioBuffer, "KnownValue [64b-embd] = 0x%08x'%08x as r64", u32(knownValue.uEmbeddedValue >> 32u), u32(knownValue.uEmbeddedValue));
                            break;
                        default:
                            if (uFormat & 0xF0u) {
                                sprintf(ioBuffer, "KnownValue [64b-embd] = <vector integral not yet implemented>");
                            } else {
                                sprintf(ioBuffer, "KnownValue [64b-embd] = 0x%08x'%08x", u32(knownValue.uEmbeddedValue >> 32u), u32(knownValue.uEmbeddedValue));
                            }
                    }
                } else {
                    sprintf(ioBuffer, "KnownValue [64b-embd] = <multi-slot integral not yet implemented>");
                }
            }
        }
    } else {
        if (irflag_is_or_has_nyka(uMetaFlags)) {
            sprintf(ioBuffer, "KnownValue = <non-embd not yet implemented (with nykas)>");
        } else {
            sprintf(ioBuffer, "KnownValue = <non-embd not yet implemented (without nykas)>");
        }
    }
}

local_func void append_ir_info_to_buffer(IRInfo* pInfo, u8 uFormat, u32 uSlotsCount, char* ioBufferLine1, char* ioBufferLine2, IRAwareContext* pCompilationContext)
{
    Assert_(!irflag_is_tc_only(pInfo->uIRandMetaFlags));
    if (ir_is_numeric_immediate(pInfo->uIRandMetaFlags)) {
        Assert_(irflag_is_known_non_nyka(pInfo->uIRandMetaFlags));
        Assert_(irflag_is_known_embd(pInfo->uIRandMetaFlags));
        if (uFormat & 0x08u) { // Floats
            // TODO: may assert that 'ir_get_value_from_f32_immediate(pInfo->uIRandMetaFlags)' matches with knownValue
            sprintf(ioBufferLine1, "[FP-immediate]");
        } else {
            // TODO: may assert that 'ir_get_large_integral_from_immediate(pInfo->uIRandMetaFlags)' matches with knownValue
            sprintf(ioBufferLine1, "[INT-immediate]");
        }
        goto whenknown;
    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(pInfo->uIRandMetaFlags)) {
        Assert_(irflag_is_known_or_nyka(pInfo->uIRandMetaFlags));
        Assert_(irflag_is_or_has_nyka(pInfo->uIRandMetaFlags));
        Assert_(irflag_is_known_embd(pInfo->uIRandMetaFlags));
        // TODO: may assert that 'ir_get_param_from_nyka_immediate(pInfo->uIRandMetaFlags)' matches with decode of knownValue NYKA
        sprintf(ioBufferLine1, "[NYKA-immediate]");
        goto whenknown;
    } else {
        IRRepo* pRepo;
        u32 uIndex;
        SourceFileDescAndState* pSourceFile;
        EEntryKind eKind;
        ir_decode_non_imm(pInfo->uIRandMetaFlags, pCompilationContext, &pRepo, &uIndex, &pSourceFile, &eKind);
        switch (eKind) {

            case EEntryKind::EEK_PROGRAMWISE_ENTRY: {
                sprintf(ioBufferLine1, "PROG-GLOBAL-IR #%u", uIndex);
                Assert_(irflag_is_known_or_nyka(pInfo->uIRandMetaFlags));
                goto whenknown;
            } break;

            case EEntryKind::EEK_FILEWISE_CONST: {
                Assert_(pSourceFile);
                sprintf(ioBufferLine1, "FILEWISE-IR-CONST-DECL #%u in file %d (%s)", uIndex, pSourceFile->iRegistrationIndex, pSourceFile->sourceFileName.begin());
                Assert_(irflag_is_known_or_nyka(pInfo->uIRandMetaFlags));
                goto whenknown;
            } break;

            case EEntryKind::EEK_FILEWISE_VAR: {
                Assert_(pSourceFile);
                sprintf(ioBufferLine1, "FILEWISE-IR-VAR-DECL #%u in file %d (%s)", uIndex, pSourceFile->iRegistrationIndex, pSourceFile->sourceFileName.begin());
                goto nomorelines;
            } break;

            case EEntryKind::EEK_IS_PROCBODY_REF: {
                Assert_(pSourceFile);
                TCProcBodyRegistration* reg = pSourceFile->vecAllProcBodies[uIndex];
                if (reg->procResult.iPrimaryIdentifier > 0) {
                    sprintf(ioBufferLine1, "FILEWISE-PROC-REF #%u (%s) in file %d (%s)", uIndex,
                        get_identifier_string(pCompilationContext->pProgCompilationState, reg->procResult.iPrimaryIdentifier).begin(),
                        pSourceFile->iRegistrationIndex, pSourceFile->sourceFileName.begin());
                } else {
                    sprintf(ioBufferLine1, "FILEWISE-PROC-REF #%u (<unnamed>) in file %d (%s)", uIndex, pSourceFile->iRegistrationIndex, pSourceFile->sourceFileName.begin());
                }
                goto nomorelines;
            } break;

            case EEntryKind::EEK_CURRENT_PROC_LOCAL: {
                sprintf(ioBufferLine1, "CURRENT-PROC-IR #%u", uIndex);
                if (irflag_is_known_or_nyka(pInfo->uIRandMetaFlags))
                    goto whenknown;
                else
                    goto nomorelines;
            } break;

            case EEntryKind::EEK_CURRENT_TEMPORARY: {
                sprintf(ioBufferLine1, "TEMPORARY-IR #%u", uIndex);
                if (irflag_is_known_or_nyka(pInfo->uIRandMetaFlags))
                    goto whenknown;
                else
                    goto nomorelines;
            } break;

            case EEntryKind::EEK_NOT_AN_ENTRY:
                Assert_(false);
                break;

            default:
                Assume_(false);
        }
    }

    { nomorelines:
        sprintf(ioBufferLine2, "");
        return;
    }

    { whenknown:
        append_known_value_to_buffer(pInfo->uIRandMetaFlags, pInfo->metaValue.knownValue, uFormat, uSlotsCount, ioBufferLine2, pCompilationContext);
        return;
    }
}

local_func void append_binding_to_buffer(ValueBinding* pBinding, char* ioBufferLine1, char* ioBufferLine2, char* ioBufferLine3, IRAwareContext* pCompilationContext)
{
    append_id_to_buffer(pBinding->iIdentifierHandle, ioBufferLine1, pCompilationContext);
    sprintf(ioBufferLine1+strlen(ioBufferLine1), ":\t");
    append_type_to_buffer(pBinding->pType, ioBufferLine1 + strlen(ioBufferLine1), pCompilationContext);
    const TypeInfo* pUnaliasedType = unalias(pBinding->pType);
    if (is_value_tc_const(pBinding)) {
        if (is_value_tc_only(pBinding)) {
            sprintf(ioBufferLine2, "TC-ONLY, Value = ");
            append_tc_only_info_to_buffer(pBinding, ioBufferLine2 + strlen(ioBufferLine2), pCompilationContext);
            sprintf(ioBufferLine3, "");
        } else {
            sprintf(ioBufferLine2, "CONSTANT, IR-REF = ");
            Assert_(ir_is_valid_param_(pBinding->info.uIRandMetaFlags));
            append_ir_info_to_buffer(&(pBinding->info), get_ir_format(pUnaliasedType), get_slots_count(pUnaliasedType), ioBufferLine2 + strlen(ioBufferLine2), ioBufferLine3, pCompilationContext);
        }
    } else {
        sprintf(ioBufferLine2, "VARIABLE, IR-REF = ");
        Assert_(ir_is_valid_param_(pBinding->info.uIRandMetaFlags));
        Assert_(!ir_is_immediate(pBinding->info.uIRandMetaFlags));
        append_ir_info_to_buffer(&(pBinding->info), get_ir_format(pUnaliasedType), get_slots_count(pUnaliasedType), ioBufferLine2 + strlen(ioBufferLine2), ioBufferLine3, pCompilationContext);
    }
}

local_func void dump_global_bindings(PlatformFileHandle file, IRAwareContext* pCompilationContext)
{
    SourceFileDescAndState* pSourceFile = pCompilationContext->pIsolatedSourceFile;

    char szLine1[2048];
    u32 uGlobalBindingCount = u32(pSourceFile->vecAllGlobalBindings.size());
    sprintf(szLine1, "\n\n----------------\nDeclared as filewise global bindings (any scope and constness), total count %u:\n\n", uGlobalBindingCount);
    platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

    char szLine2[2048];
    char szLine3[2048];
    for (u32 uIndex = 0; uIndex < uGlobalBindingCount; uIndex++) {
        ValueBinding* pBinding = pSourceFile->vecAllGlobalBindings[uIndex];
        append_binding_to_buffer(pBinding, szLine1, szLine2, szLine3, pCompilationContext);
        char szTmp[256];
        sprintf(szTmp, "\t%07u\t", uIndex);
        platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
        if (szLine2[0]) {
            sprintf(szTmp, "\n\t\t");
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
            platform_write_to_file(file, (u8*)szLine2, u32(strlen(szLine2)));
            if (szLine3[0]) {
                sprintf(szTmp, "\n\t\t");
                platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
                platform_write_to_file(file, (u8*)szLine3, u32(strlen(szLine3)));
            }
        }
        sprintf(szTmp, "\n");
        platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
    }
}

local_func void print_ir_format_to(char tBuffer[64], u8 uFormat)
{
    sprintf(tBuffer, "%c%4db", uFormat & 0x08u ? 'F':'R', 8u << (uFormat & 0x07u));
    u8 uVecCountPow = uFormat >> 4;
    if (uVecCountPow) {
        sprintf(tBuffer+strlen(tBuffer), " x%5u", 1u << uVecCountPow);
    } else {
        sprintf(tBuffer+strlen(tBuffer), " scalar");
    }
}

local_func const char* get_op_name(u8 op)
{
    Assert_(op < COUNT_IRIT_INSTRUCTIONS);
    return tIRITStr[op];
}

local_func const TypeInfo* default_type_from_ir_format(u8 uFormat)
{
    if (uFormat == COMPINT_PSEUDO_IR_FORMAT)
        return g_pCoreTypesInfo[ECORETYPE_COMPINT];
    else if (uFormat == XFLOAT_PSEUDO_IR_FORMAT)
        return g_pCoreTypesInfo[ECORETYPE_XFLOAT];
    else if (0 == (uFormat & 0xF0u)) {
        if (uFormat & 0x08u) {
            switch (uFormat & 0x07u) {
                case 0x01u: return g_pCoreTypesInfo[ECORETYPE_F16];
                case 0x02u: return g_pCoreTypesInfo[ECORETYPE_F32];
                case 0x03u: return g_pCoreTypesInfo[ECORETYPE_F64];
                case 0x04u: return g_pCoreTypesInfo[ECORETYPE_F128];
                case 0x05u: return g_pCoreTypesInfo[ECORETYPE_F256];
                default: Assume_(false); return 0;
            }
        } else {
            return g_pCoreTypesInfo[ECORETYPE_R8 + uFormat];
        }
    } else {
        // TODO: vector formats
        return 0;
    }
}

local_func void append_proc_sign_to_buffer(const TypeInfo_ProcLike* pProcSign, char* ioBuffer, IRAwareContext* pCompilationContext)
{
    if (pProcSign) {
        sprintf(ioBuffer, "kind %u (", get_proc_kind(pProcSign));
        u8 uInParamCount = get_input_param_count(pProcSign);
        u8 uOutParamCount = get_output_param_count(pProcSign);
        for (u8 uIn = 0; uIn < uInParamCount; uIn++) {
            append_id_to_buffer(pProcSign->params[uIn].iIdentifier, ioBuffer + strlen(ioBuffer), pCompilationContext);
            const TypeInfo* pType = pProcSign->params[uIn].pBinding->pType;
            sprintf(ioBuffer + strlen(ioBuffer), " : ");
            append_type_to_buffer(pType, ioBuffer + strlen(ioBuffer), pCompilationContext);
            if (uIn < uInParamCount - 1u)
                sprintf(ioBuffer + strlen(ioBuffer), ", ");
        }
        if (uOutParamCount) {
            sprintf(ioBuffer + strlen(ioBuffer), ") -> (");
            u8 uTotalParamCount = uInParamCount + uOutParamCount;
            for (u8 uOut = uInParamCount; uOut < uTotalParamCount; uOut++) {
                append_id_to_buffer(pProcSign->params[uOut].iIdentifier, ioBuffer + strlen(ioBuffer), pCompilationContext);
                const TypeInfo* pType = pProcSign->params[uOut].pBinding->pType;
                sprintf(ioBuffer + strlen(ioBuffer), " : ");
                append_type_to_buffer(pType, ioBuffer + strlen(ioBuffer), pCompilationContext);
                if (uOut < uTotalParamCount - 1u)
                    sprintf(ioBuffer + strlen(ioBuffer), ", ");
            }
        }
        sprintf(ioBuffer + strlen(ioBuffer), ")");
    }
}

local_func void print_ir_param_to(char* ioBuffer, u64 uIRParam, u8 uFormat, IRAwareContext* pCompilationContext)
{
    if (ir_is_numeric_immediate(uIRParam)) {
        switch (uFormat) {
            case 0x00u: sprintf(ioBuffer, " 8b:        0x%02x", u8(ir_get_value_from_int_immediate(uIRParam))); break;
            case 0x01u: sprintf(ioBuffer, " 16b:     0x%04x", u16(ir_get_value_from_int_immediate(uIRParam))); break;
            case 0x02u: sprintf(ioBuffer, " 32b: 0x%08x", ir_get_value_from_int_immediate(uIRParam)); break;
            case 0x03u: {
                i32 iValue = ir_get_value_from_int_immediate(uIRParam);
                if (iValue < 0)
                    sprintf(ioBuffer, " 0xFFF..%08x", iValue);
                else
                    sprintf(ioBuffer, " 0x000..%08x", iValue);
            } break;
            default: {
                sprintf(ioBuffer, " FP-IMMED-TODO");
            } break;
        }
    } else if (ir_is_known_other_than_numeric_imm_a_nyka_imm(uIRParam)) {
        Assert_(uFormat == 0x03u);
        u64 uBaseIR = ir_get_param_from_nyka_immediate(uIRParam);
        Assert_(!ir_is_immediate(uBaseIR));
        sprintf(ioBuffer, "@");
        append_ir_non_imm_param_to_buffer(uBaseIR, ioBuffer + 1, pCompilationContext);
    } else {
        sprintf(ioBuffer, " ");
        append_ir_non_imm_param_to_buffer(uIRParam, ioBuffer + 1, pCompilationContext);
    }
}

local_func void append_ir_entry_to_buffer(IREntry& entry, u32 uEntryPos, char* ioBufferLine1, char* ioBufferLine2, IRAwareContext* pCompilationContext)
{
    #define no_format "             "
    u8 uInstructionType = u8(entry.uInstrCodeAndFormatAndFirstParam);
    Assert_(uInstructionType < COUNT_IRIT_INSTRUCTIONS);
    u8 uInstructionFormatCode = tIRITFormatSlot[uInstructionType];
    u8 uStdFormat = u8(entry.uInstrCodeAndFormatAndFirstParam >> 16);
    u8 uResultFormat = IR_INSTR_NOVALUE;
    u32 uSlotsCount = 1u; // by default
    char szStdFormat[64];
    print_ir_format_to(szStdFormat, uStdFormat);
    if (uInstructionFormatCode == IR_INSTR_NOVALUE) {
        sprintf(ioBufferLine1, "-------------");
    } else if (uInstructionFormatCode == IR_INSTR_STDFORMAT) {
        uResultFormat = uStdFormat;
        sprintf(ioBufferLine1, "%s", szStdFormat);
    } else if (uInstructionFormatCode == IR_INSTR_STDINT) {
        uResultFormat = uStdFormat & 0x07u;
        char szIntFormat[64];
        print_ir_format_to(szIntFormat, uResultFormat);
        sprintf(ioBufferLine1, "%s", szIntFormat);
    } else {
        Assert_(uInstructionFormatCode < 0x80u);
        uResultFormat = uInstructionFormatCode;
        char szFixFormat[64];
        print_ir_format_to(szFixFormat, uResultFormat);
        sprintf(ioBufferLine1, "%s", szFixFormat);
    }

    if (uInstructionType == IRIT_MARKER_JUMP_TARGET) {
        sprintf(ioBufferLine1 + strlen(ioBufferLine1), " ");
    } else if (uInstructionType == IRIT_GOTO || uInstructionType == IRIT_BRANCH || uInstructionType == IRIT_RET) {
        sprintf(ioBufferLine1 + strlen(ioBufferLine1), "     ");
    } else if (uInstructionType == IRIT_CALLER_IN_PARAM || uInstructionType == IRIT_CALLER_RET_PARAM) {
        sprintf(ioBufferLine1 + strlen(ioBufferLine1), "       ");
    } else {
        sprintf(ioBufferLine1 + strlen(ioBufferLine1), "   ");
    }
    sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s", get_op_name(uInstructionType));
    u32 uSize = u32(strlen(ioBufferLine1));
    Assert_(uSize < 50u);
    memset(ioBufferLine1+uSize, 0x20, 50u - uSize);
    ioBufferLine1[50] = '\0';

    #define no_param  "                "
    char szFirstParam[64];
    char szSecondParam[64];
    u64 uFirstParamIRValue = entry.uInstrCodeAndFormatAndFirstParam & IR_STD_PARAM_MASK;
    u64 uSecondParamIRValue = entry.uInstrMetaFlagsAndSecondParam & IR_STD_PARAM_MASK;
    u64 uFirstParamAsStatic = entry.uInstrCodeAndFormatAndFirstParam >> IR_STD_PARAM_SHIFT;
    u64 uSecondParamAsStatic = entry.uInstrMetaFlagsAndSecondParam >> IR_STD_PARAM_SHIFT;

    const TypeInfo* pTypeRepr = 0;
    if (uResultFormat != IR_INSTR_NOVALUE)
        pTypeRepr = default_type_from_ir_format(uResultFormat);

    switch (uInstructionType) {

        // declarations
        case IRIT_DECLARATION: {
            uSlotsCount = u32(uSecondParamAsStatic);
            u32 uAlignLog2 = u32(uSecondParamAsStatic>>32);
            sprintf(szSecondParam, "%8u", uSlotsCount);
            if (entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_CONST_IS_RTTI) {
                sprintf(ioBufferLine1 + strlen(ioBufferLine1), "(--RTTI--)      ---:%s ; aln:%4u ; cnt:%s\t", no_param, uAlignLog2, szSecondParam);
                pTypeRepr = 0; // TODO
            } else {
                sprintf(ioBufferLine1 + strlen(ioBufferLine1), "(CONSTANT)      ---:%s ; aln:%4u ; cnt:%s\t", no_param, uAlignLog2, szSecondParam);
                if (uSlotsCount != 1u)
                    pTypeRepr = 0;
            }
        } break;

        case IRIT_GLOBAL_VAR_DECL: {
            uSlotsCount = u32(uSecondParamAsStatic);
            u32 uAlignLog2 = u32(uSecondParamAsStatic>>32);
            sprintf(szSecondParam, "%8u", uSlotsCount);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1),     "%s   ---:%s ; aln:%4u ; cnt:%s\t", no_format, no_param, uAlignLog2, szSecondParam);
        } break;

        case IRIT_LOCAL_VAR_DECL: {
            Assert_(pCompilationContext->pProcResult);
            uSlotsCount = u32(uSecondParamAsStatic);
            u32 uAlignLog2 = u32(uSecondParamAsStatic>>32);
            sprintf(szSecondParam, "%8u", uSlotsCount);
            if (uEntryPos < pCompilationContext->pProcResult->procSign->params.size()) {
                if (uEntryPos < get_input_param_count(pCompilationContext->pProcResult->procSign)) {
                    sprintf(ioBufferLine1 + strlen(ioBufferLine1),     "(INP-PARAM)     ---:%s ; aln:%4u ; cnt:%s\t", no_param, uAlignLog2, szSecondParam);
                } else {
                    sprintf(ioBufferLine1 + strlen(ioBufferLine1),     "(OUT-PARAM)     ---:%s ; aln:%4u ; cnt:%s\t", no_param, uAlignLog2, szSecondParam);
                }
            } else {
                sprintf(ioBufferLine1 + strlen(ioBufferLine1),         "%s   ---:%s ; aln:%4u ; cnt:%s\t", no_format, no_param, uAlignLog2, szSecondParam);
            }
        } break;

        // ops with no param and no value
        case IRIT_NO_OP:
        case IRIT_MARKER_JUMP_TARGET:
        case IRIT_MARKER_START_SOURCE_SCOPE:
        case IRIT_RET:
        {
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   ---:%s ; ---:%s\t", no_format, no_param, no_param);
        } break;

        // special ops
        case IRIT_MARKER_END_SOURCE_SCOPE:
        {
            sprintf(szFirstParam, "%16u", u32(uFirstParamAsStatic));
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   ---:%s ; ---:%s\t", no_format, szFirstParam, no_param);
        } break;

        // unary ops
        case IRIT_ISNAN:
        case IRIT_ISNEG:
        case IRIT_ABS:
        case IRIT_BIT_NOT:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, uStdFormat, pCompilationContext);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   opA:%s ; ---:%s\t", szStdFormat, szFirstParam, no_param);
        } break;

        case IRIT_BOOL_NOT:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, 0x00u, pCompilationContext);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   opA:%s ; ---:%s\t", szStdFormat, szFirstParam, no_param);
        } break;

        // binary ops
        case IRIT_ADD:
        case IRIT_ADD_CAR:

        case IRIT_SUB:
        case IRIT_SUB_BOR:

        case IRIT_MUL:
        case IRIT_MUL_U:
        case IRIT_MUL_HIGH:
        case IRIT_MUL_HIGH_U:

        case IRIT_DIV:
        case IRIT_QUO:
        case IRIT_EXACT_QUO:
        case IRIT_QUO_LARGE:

        case IRIT_MOD:
        case IRIT_REM:
        case IRIT_REM_LARGE:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, uResultFormat, pCompilationContext);
            print_ir_param_to(szSecondParam, uSecondParamIRValue, uResultFormat, pCompilationContext);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   opA:%s ; opB:%s\t", szStdFormat, szFirstParam, szSecondParam);
        } break;

        case IRIT_STORE:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, uStdFormat, pCompilationContext);
            print_ir_param_to(szSecondParam, uSecondParamIRValue, uStdFormat, pCompilationContext);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   dst:%s ; src:%s\t", szStdFormat, szFirstParam, szSecondParam);
        } break;

        case IRIT_DEREF:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, 0x03u, pCompilationContext);
            uSlotsCount = u32(uSecondParamAsStatic);
            u32 uAlignLog2 = u32(uSecondParamAsStatic>>32);
            sprintf(szSecondParam, "%8u", uSlotsCount);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   adr:%s ; aln:%4u ; cnt:%s\t", szStdFormat, szFirstParam, uAlignLog2, szSecondParam);
        } break;

        case IRIT_PTR_OFFSET:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, 0x03u, pCompilationContext);
            print_ir_param_to(szSecondParam, uSecondParamIRValue, 0x02u, pCompilationContext);
            u32 uScale = u32(uStdFormat) + 1u; // in format slot is index scale x1..x256.
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   bas:%s ; idx:%s (scale x%u bytes)\t", no_format, szFirstParam, szSecondParam, uScale);
        } break;

        case IRIT_CMP_EQ:
        case IRIT_CMP_ORD:
        {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, uStdFormat, pCompilationContext);
            print_ir_param_to(szSecondParam, uSecondParamIRValue, uStdFormat, pCompilationContext);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   opA:%s ; opB:%s\t", szStdFormat, szFirstParam, szSecondParam);
        } break;

        case IRIT_CALL: {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, 0x03u, pCompilationContext);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   prc:%s ; ---:%s\t", no_format, szFirstParam, no_param);
        } break;

        case IRIT_CALLER_IN_PARAM: {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, uStdFormat, pCompilationContext);
            uSlotsCount = u32(uSecondParamAsStatic);
            u32 uAlignLog2 = u32(uSecondParamAsStatic>>32);
            sprintf(szSecondParam, "%8u", uSlotsCount);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   val:%s ; aln:%4u ; cnt:%s\t", szStdFormat, szFirstParam, uAlignLog2, szSecondParam);
        } break;

        case IRIT_CALLER_RET_PARAM: {
            sprintf(szFirstParam, " from Ir%07u", u32(uFirstParamAsStatic));
            uSlotsCount = u32(uSecondParamAsStatic);
            u32 uAlignLog2 = u32(uSecondParamAsStatic>>32);
            sprintf(szSecondParam, "%8u", uSlotsCount);
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   retrn%s ; aln:%4u ; cnt:%s\t", szStdFormat, szFirstParam, uAlignLog2, szSecondParam);
        } break;

        case IRIT_GOTO: {
            sprintf(szSecondParam, "jmpto Ir%07u", u32(uSecondParamAsStatic));
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   ---:%s ; dst:%s\t", no_format, no_param, szSecondParam);
        } break;

        case IRIT_BRANCH:
        case IRIT_ERRCHK: {
            print_ir_param_to(szFirstParam, uFirstParamIRValue, uStdFormat, pCompilationContext);
            u32 bIfNonZero = entry.uInstrCodeAndFormatAndFirstParam & IR_INSTRFLAG_BRANCH_ON_NONZERO;
            if (uInstructionType == IRIT_BRANCH) {
                sprintf(szSecondParam, "%s Ir%07u", (bIfNonZero ? "nz =>" : "=0 =>"), u32(uSecondParamAsStatic));
            } else {
                sprintf(szSecondParam, "%s ErrHandler", (bIfNonZero ? "nz =>" : "=0 =>"));
            }
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   tst:%s ; dst:%s\t", szStdFormat, szFirstParam, szSecondParam);
        } break;

        default:
            sprintf(ioBufferLine1 + strlen(ioBufferLine1), "%s   <decoding TODO>                              \t", no_format);
    }

    if (irflag_is_known_or_nyka(entry.uInstrMetaFlagsAndSecondParam)) {
        append_known_value_to_buffer(entry.uInstrMetaFlagsAndSecondParam, entry.metaValue.knownValue, uStdFormat, uSlotsCount, ioBufferLine2, pCompilationContext);
    } else {
        sprintf(ioBufferLine2, "");
    }
}

local_func void dump_ir_standard_consts(PlatformFileHandle file, IRAwareContext* pCompilationContext)
{
    SourceFileDescAndState* pSourceFile = pCompilationContext->pIsolatedSourceFile;

    char szLine1[2048];
    IRRepo* pRepo = &(pSourceFile->filewiseConstRepo);
    u32 uStandardIRConst = pRepo->uSize;
    sprintf(szLine1, "\n\n----------------\nDeclared as filewise standard (const) IR declarations, total count %u:\n\n", uStandardIRConst);
    platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

    char szLine2[2048];
    for (u32 uIndex = 0; uIndex < uStandardIRConst; uIndex++) {
        IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
        append_ir_entry_to_buffer(entry, uIndex, szLine1, szLine2, pCompilationContext);
        char szTmp[256];
        sprintf(szTmp, "\tCs%07u\t", uIndex);
        platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
        if (szLine2[0]) {
            sprintf(szTmp, "\n\t\t");
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
            platform_write_to_file(file, (u8*)szLine2, u32(strlen(szLine2)));
        }
        sprintf(szTmp, "\n");
        platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
    }
}

local_func void dump_ir_global_vars(PlatformFileHandle file, IRAwareContext* pCompilationContext)
{
    SourceFileDescAndState* pSourceFile = pCompilationContext->pIsolatedSourceFile;

    char szLine1[2048];
    IRRepo* pRepo = &(pSourceFile->filewiseGlobalVarRepo);
    u32 uStandardIRVars = pRepo->uSize;
    sprintf(szLine1, "\n\n----------------\nDeclared as filewise global var IR declarations (showing initial value), total count %u:\n\n", uStandardIRVars);
    platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

    char szLine2[2048];
    for (u32 uIndex = 0; uIndex < uStandardIRVars; uIndex++) {
        IREntry& entry = ir_access_repo_instr(pRepo, uIndex);
        append_ir_entry_to_buffer(entry, uIndex, szLine1, szLine2, pCompilationContext);
        char szTmp[256];
        sprintf(szTmp, "\tVr%07u\t", uIndex);
        platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
        if (szLine2[0]) {
            sprintf(szTmp, "\n\t\t");
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
            platform_write_to_file(file, (u8*)szLine2, u32(strlen(szLine2)));
        }
        sprintf(szTmp, "\n");
        platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
    }
}

local_func void dump_ir_proc_entries(PlatformFileHandle file, IRAwareContext* pCompilationContext)
{
    SourceFileDescAndState* pSourceFile = pCompilationContext->pIsolatedSourceFile;

    char szLine1[2048];
    u32 uProcBodies = pSourceFile->vecAllProcBodies.size();
    sprintf(szLine1, "\n\n----------------\nDeclared as filewise registered proc-bodies, total count %u:\n\n", uProcBodies);
    platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

    for (u32 uIndex = 0; uIndex < uProcBodies; uIndex++) {
        TCProcBodyRegistration* pRegistration = pSourceFile->vecAllProcBodies[uIndex];
        sprintf(szLine1, "\tRp%07u", uIndex);
        if (pRegistration->procResult.iPrimaryIdentifier > 0) {
            sprintf(szLine1 + strlen(szLine1), "\t'%s', Signature = ",
                get_identifier_string(pCompilationContext->pProgCompilationState, u32(pRegistration->procResult.iPrimaryIdentifier)).begin());
        } else {
            sprintf(szLine1 + strlen(szLine1), "\t<unnamed>, Signature = ");
        }
        append_proc_sign_to_buffer(pRegistration->procResult.procSign, szLine1 + strlen(szLine1), pCompilationContext);
        sprintf(szLine1 + strlen(szLine1), "\n");
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
    }
}

local_func void dump_procbodies(PlatformFileHandle file, IRAwareContext* pCompilationContext)
{
    SourceFileDescAndState* pSourceFile = pCompilationContext->pIsolatedSourceFile;

    char szLine1[2048];
    u32 uProcBodies = pSourceFile->vecAllProcBodies.size();
    for (u32 uProc = 0u; uProc < uProcBodies; uProc++) {
        sprintf(szLine1, "\n\n----------------\nProc-Body Dump of proc Rp%07u:\n\n", uProc);
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
        TCProcBodyRegistration* pRegistration = pSourceFile->vecAllProcBodies[uProc];

        TCProcBodyResult* pProcResult = &(pRegistration->procResult);
        pCompilationContext->pProcResult = pProcResult;
        pCompilationContext->pRepo = &(pProcResult->procwiseRepo);

        if (pProcResult->iPrimaryIdentifier > 0) {
            sprintf(szLine1, "\tPrimary-identifier : '%s'\n",
                get_identifier_string(pCompilationContext->pProgCompilationState, u32(pProcResult->iPrimaryIdentifier)).begin());
            platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
        }
        sprintf(szLine1, "\tSignature = ");
        append_proc_sign_to_buffer(pProcResult->procSign, szLine1 + strlen(szLine1), pCompilationContext);
        sprintf(szLine1 + strlen(szLine1), "\n\n");
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

        u32 uLocalBindingCount = u32(pProcResult->vecBindings.size());
        sprintf(szLine1, "\t---------------- Declared as procwise binding (any constness), total count %u:\n\n", uLocalBindingCount);
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

        char szLine2[2048];
        char szLine3[2048];
        for (u32 uIndex = 0; uIndex < uLocalBindingCount; uIndex++) {
            ValueBinding* pBinding = pProcResult->vecBindings[uIndex];
            append_binding_to_buffer(pBinding, szLine1, szLine2, szLine3, pCompilationContext);
            char szTmp[256];
            sprintf(szTmp, "\t%07u\t", uIndex);
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
            platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
            if (szLine2[0]) {
                sprintf(szTmp, "\n\t\t");
                platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
                platform_write_to_file(file, (u8*)szLine2, u32(strlen(szLine2)));
                if (szLine3[0]) {
                    sprintf(szTmp, "\n\t\t");
                    platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
                    platform_write_to_file(file, (u8*)szLine3, u32(strlen(szLine3)));
                }
            }
            sprintf(szTmp, "\n");
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
        }

        u32 uIRCount = pCompilationContext->pRepo->uSize;
        sprintf(szLine1, "\n\t---------------- ProcBody IR-Listing, total count %u:\n\n", uIRCount);
        platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));

        for (u32 uIndex = 0; uIndex < uIRCount; uIndex++) {
            IREntry& entry = ir_access_repo_instr(pCompilationContext->pRepo, uIndex);
            append_ir_entry_to_buffer(entry, uIndex, szLine1, szLine2, pCompilationContext);
            char szTmp[256];
            if (uIndex < pProcResult->procSign->params.size()) {
                if (uIndex < get_input_param_count(pProcResult->procSign)) {
                    sprintf(szTmp, "\t? Pi%07u\t", uIndex);
                } else {
                    sprintf(szTmp, "\t? Po%07u\t", uIndex);
                }
            } else {
                if (tIRITFormatSlot[u8(entry.uInstrCodeAndFormatAndFirstParam)] == IR_INSTR_NOVALUE) {
                    sprintf(szTmp, "\t**Ir%07u\t", uIndex);
                } else if (IRFLAG_IS_KNOWN == (u32(entry.uInstrMetaFlagsAndSecondParam) & (IRFLAG_IS_KNOWN|IRFLAG_HAS_LOCAL_NYKA))) {
                    sprintf(szTmp, "\t  Ir%07u\t", uIndex);
                } else {
                    sprintf(szTmp, "\t? Ir%07u\t", uIndex);
                }
            }
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
            platform_write_to_file(file, (u8*)szLine1, u32(strlen(szLine1)));
            if (szLine2[0]) {
                sprintf(szTmp, "\n\t\t");
                platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
                platform_write_to_file(file, (u8*)szLine2, u32(strlen(szLine2)));
            }
            sprintf(szTmp, "\n");
            platform_write_to_file(file, (u8*)szTmp, u32(strlen(szTmp)));
        }

    }
}

local_func void ir_dump_to_file(PlatformFileHandle file, IRAwareContext* pCompilationContext)
{
    SourceFileDescAndState* pSourceFile = pCompilationContext->pIsolatedSourceFile;
    char szBuffer[2048];
    sprintf(szBuffer, "\n\n****************\n****************\nFile #%d : %s\n****************\n****************\n",
        pSourceFile->iRegistrationIndex, pSourceFile->sourceFileName.c_str());
    platform_write_to_file(file, (u8*)szBuffer, u32(strlen(szBuffer)));

    dump_ir_standard_consts(file, pCompilationContext);
    dump_ir_global_vars(file, pCompilationContext);
    dump_ir_proc_entries(file, pCompilationContext);
    dump_global_bindings(file, pCompilationContext);
    dump_procbodies(file, pCompilationContext);
}


#endif // LOCLIB_IR_DUMP_H_

