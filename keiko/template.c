/* Template file -- generated by iset.tcl */

#include "oblink.h"
#include "keiko.h"

struct _template templates[NTEMPLATES] = {
{"PUSH",      "N",    -1,  6,  1,  1,  1, K_PUSH_x1, { NULL }},
{   NULL,     "1",     0,  0,  0,  2,  1, K_PUSH_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_PUSH_2, { NULL }},
{"LDKW",      "1",     0,  0,  0,  2,  1, K_LDKW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDKW_2, { NULL }},
{"LDKF",      "1",     0,  0,  0,  2,  1, K_LDKF_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDKF_2, { NULL }},
{"LOCAL",     "1",     0,  0,  0,  2,  1, K_LOCAL_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LOCAL_2, { NULL }},
{"PLUSA",     "",      0,  0,  0,  0,  0, 0, { "INDEXC" }},
{"INDEXC",    "",      0,  0,  0,  1,  1, K_INDEXC, { NULL }},
{"INDEXS",    "",      0,  0,  0,  1,  1, K_INDEXS, { NULL }},
{"INDEXW",    "",      0,  0,  0,  1,  1, K_INDEXW, { NULL }},
{"INDEXD",    "",      0,  0,  0,  1,  1, K_INDEXD, { NULL }},
{"LOADW",     "",      0,  0,  0,  1,  1, K_LOADW, { NULL }},
{"LOADS",     "",      0,  0,  0,  1,  1, K_LOADS, { NULL }},
{"LOADC",     "",      0,  0,  0,  1,  1, K_LOADC, { NULL }},
{"LOADF",     "",      0,  0,  0,  1,  1, K_LOADF, { NULL }},
{"STOREW",    "",      0,  0,  0,  1,  1, K_STOREW, { NULL }},
{"STORES",    "",      0,  0,  0,  1,  1, K_STORES, { NULL }},
{"STOREC",    "",      0,  0,  0,  1,  1, K_STOREC, { NULL }},
{"STOREF",    "",      0,  0,  0,  1,  1, K_STOREF, { NULL }},
{"LDLW",      "N",   -24, -4,  4,  1,  1, K_LDLW_x1, { NULL }},
{   NULL,     "N",    12, 32,  4,  1,  1, K_LDLW_x2, { NULL }},
{   NULL,     "1",     0,  0,  0,  2,  1, K_LDLW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDLW_2, { NULL }},
{"LDLS",      "1",     0,  0,  0,  2,  1, K_LDLS_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDLS_2, { NULL }},
{"LDLC",      "1",     0,  0,  0,  2,  1, K_LDLC_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDLC_2, { NULL }},
{"LDLF",      "1",     0,  0,  0,  2,  1, K_LDLF_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDLF_2, { NULL }},
{"STLW",      "N",   -24, -4,  4,  1,  1, K_STLW_x1, { NULL }},
{   NULL,     "N",    12, 32,  4,  1,  1, K_STLW_x2, { NULL }},
{   NULL,     "1",     0,  0,  0,  2,  1, K_STLW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_STLW_2, { NULL }},
{"STLS",      "1",     0,  0,  0,  2,  1, K_STLS_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_STLS_2, { NULL }},
{"STLC",      "1",     0,  0,  0,  2,  1, K_STLC_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_STLC_2, { NULL }},
{"STLF",      "1",     0,  0,  0,  2,  1, K_STLF_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_STLF_2, { NULL }},
{"LDGW",      "K",     0,  0,  0,  2,  1, K_LDGW_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_LDGW_L, { NULL }},
{"LDGS",      "K",     0,  0,  0,  2,  1, K_LDGS_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_LDGS_L, { NULL }},
{"LDGC",      "K",     0,  0,  0,  2,  1, K_LDGC_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_LDGC_L, { NULL }},
{"LDGF",      "K",     0,  0,  0,  2,  1, K_LDGF_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_LDGF_L, { NULL }},
{"STGW",      "K",     0,  0,  0,  2,  1, K_STGW_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_STGW_L, { NULL }},
{"STGS",      "K",     0,  0,  0,  2,  1, K_STGS_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_STGS_L, { NULL }},
{"STGC",      "K",     0,  0,  0,  2,  1, K_STGC_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_STGC_L, { NULL }},
{"STGF",      "K",     0,  0,  0,  2,  1, K_STGF_K, { NULL }},
{   NULL,     "L",     0,  0,  0,  3,  1, K_STGF_L, { NULL }},
{"LDNW",      "N",   -16, 40,  4,  1,  1, K_LDNW_x1, { NULL }},
{   NULL,     "1",     0,  0,  0,  2,  1, K_LDNW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDNW_2, { NULL }},
{"STNW",      "N",   -16, 20,  4,  1,  1, K_STNW_x1, { NULL }},
{   NULL,     "1",     0,  0,  0,  2,  1, K_STNW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_STNW_2, { NULL }},
{"LDIW",      "",      0,  0,  0,  1,  1, K_LDIW, { NULL }},
{"LDIF",      "",      0,  0,  0,  1,  1, K_LDIF, { NULL }},
{"LDIS",      "",      0,  0,  0,  1,  1, K_LDIS, { NULL }},
{"LDIC",      "",      0,  0,  0,  1,  1, K_LDIC, { NULL }},
{"STIW",      "",      0,  0,  0,  1,  1, K_STIW, { NULL }},
{"STIF",      "",      0,  0,  0,  1,  1, K_STIF, { NULL }},
{"STIS",      "",      0,  0,  0,  1,  1, K_STIS, { NULL }},
{"STIC",      "",      0,  0,  0,  1,  1, K_STIC, { NULL }},
{"LDEW",      "1",     0,  0,  0,  2,  1, K_LDEW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDEW_2, { NULL }},
{"STEW",      "1",     0,  0,  0,  2,  1, K_STEW_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_STEW_2, { NULL }},
{"LOADD",     "",      0,  0,  0,  1,  1, K_LOADD, { NULL }},
{"STORED",    "",      0,  0,  0,  1,  1, K_STORED, { NULL }},
{"LDKD",      "1",     0,  0,  0,  2,  1, K_LDKD_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDKD_2, { NULL }},
{"LOADQ",     "",      0,  0,  0,  1,  1, K_LOADQ, { NULL }},
{"STOREQ",    "",      0,  0,  0,  1,  1, K_STOREQ, { NULL }},
{"LDKQ",      "1",     0,  0,  0,  2,  1, K_LDKQ_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  3,  1, K_LDKQ_2, { NULL }},
{"LDID",      "",      0,  0,  0,  0,  0, 0, { "INDEXD", "LOADD" }},
{"STID",      "",      0,  0,  0,  0,  0, 0, { "INDEXD", "STORED" }},
{"LDLD",      "1",     0,  0,  0,  0,  0, 0, { "LOCAL $a", "LOADD" }},
{"STLD",      "1",     0,  0,  0,  0,  0, 0, { "LOCAL $a", "STORED" }},
{"LDGD",      "K",     0,  0,  0,  0,  0, 0, { "LDKW $a", "LOADD" }},
{"STGD",      "K",     0,  0,  0,  0,  0, 0, { "LDKW $a", "STORED" }},
{"LDIQ",      "",      0,  0,  0,  0,  0, 0, { "INDEXD", "LOADQ" }},
{"STIQ",      "",      0,  0,  0,  0,  0, 0, { "INDEXD", "STOREQ" }},
{"LDLQ",      "1",     0,  0,  0,  0,  0, 0, { "LOCAL $a", "LOADQ" }},
{"STLQ",      "1",     0,  0,  0,  0,  0, 0, { "LOCAL $a", "STOREQ" }},
{"LDGQ",      "K",     0,  0,  0,  0,  0, 0, { "LDKW $a", "LOADQ" }},
{"STGQ",      "K",     0,  0,  0,  0,  0, 0, { "LDKW $a", "STOREQ" }},
{"INCL",      "1",     0,  0,  0,  2,  1, K_INCL_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  0,  0, 0, { "LDLW $a", "INC", "STLW $a" }},
{"DECL",      "1",     0,  0,  0,  2,  1, K_DECL_1, { NULL }},
{   NULL,     "2",     0,  0,  0,  0,  0, 0, { "LDLW $a", "DEC", "STLW $a" }},
{"DUP",       "N",     0,  2,  1,  1,  1, K_DUP, { NULL }},
{"SWAP",      "",      0,  0,  0,  1,  1, K_SWAP, { NULL }},
{"POP",       "1",     0,  0,  0,  2,  1, K_POP_1, { NULL }},
{"PLUS",      "",      0,  0,  0,  1,  1, K_PLUS, { NULL }},
{"MINUS",     "",      0,  0,  0,  1,  1, K_MINUS, { NULL }},
{"TIMES",     "",      0,  0,  0,  1,  1, K_TIMES, { NULL }},
{"UMINUS",    "",      0,  0,  0,  1,  1, K_UMINUS, { NULL }},
{"AND",       "",      0,  0,  0,  1,  1, K_AND, { NULL }},
{"OR",        "",      0,  0,  0,  1,  1, K_OR, { NULL }},
{"NOT",       "",      0,  0,  0,  1,  1, K_NOT, { NULL }},
{"INC",       "",      0,  0,  0,  1,  1, K_INC, { NULL }},
{"DEC",       "",      0,  0,  0,  1,  1, K_DEC, { NULL }},
{"BITAND",    "",      0,  0,  0,  1,  1, K_BITAND, { NULL }},
{"BITOR",     "",      0,  0,  0,  1,  1, K_BITOR, { NULL }},
{"BITXOR",    "",      0,  0,  0,  1,  1, K_BITXOR, { NULL }},
{"BITNOT",    "",      0,  0,  0,  1,  1, K_BITNOT, { NULL }},
{"BITSUB",    "",      0,  0,  0,  1,  1, K_BITSUB, { NULL }},
{"BIT",       "",      0,  0,  0,  1,  1, K_BIT, { NULL }},
{"LSL",       "",      0,  0,  0,  1,  1, K_LSL, { NULL }},
{"LSR",       "",      0,  0,  0,  1,  1, K_LSR, { NULL }},
{"ASR",       "",      0,  0,  0,  1,  1, K_ASR, { NULL }},
{"DIV",       "",      0,  0,  0,  1,  1, K_DIV, { NULL }},
{"MOD",       "",      0,  0,  0,  1,  1, K_MOD, { NULL }},
{"EQ",        "",      0,  0,  0,  1,  1, K_EQ, { NULL }},
{"LT",        "",      0,  0,  0,  1,  1, K_LT, { NULL }},
{"GT",        "",      0,  0,  0,  1,  1, K_GT, { NULL }},
{"LEQ",       "",      0,  0,  0,  1,  1, K_LEQ, { NULL }},
{"GEQ",       "",      0,  0,  0,  1,  1, K_GEQ, { NULL }},
{"NEQ",       "",      0,  0,  0,  1,  1, K_NEQ, { NULL }},
{"JEQ",       "S",     0,  0,  0,  2,  1, K_JEQ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JEQ_R, { NULL }},
{"JLT",       "S",     0,  0,  0,  2,  1, K_JLT_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JLT_R, { NULL }},
{"JGT",       "S",     0,  0,  0,  2,  1, K_JGT_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JGT_R, { NULL }},
{"JLEQ",      "S",     0,  0,  0,  2,  1, K_JLEQ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JLEQ_R, { NULL }},
{"JGEQ",      "S",     0,  0,  0,  2,  1, K_JGEQ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JGEQ_R, { NULL }},
{"JNEQ",      "S",     0,  0,  0,  2,  1, K_JNEQ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JNEQ_R, { NULL }},
{"JLTZ",      "S",     0,  0,  0,  2,  1, K_JLTZ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "PUSH 0", "JLT $a" }},
{"JGTZ",      "S",     0,  0,  0,  2,  1, K_JGTZ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "PUSH 0", "JGT $a" }},
{"JLEQZ",     "S",     0,  0,  0,  2,  1, K_JLEQZ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "PUSH 0", "JLEQ $a" }},
{"JGEQZ",     "S",     0,  0,  0,  2,  1, K_JGEQZ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "PUSH 0", "JGEQ $a" }},
{"JUMPF",     "R",     0,  0,  0,  0,  0, 0, { "JEQZ $a" }},
{"JUMPT",     "R",     0,  0,  0,  0,  0, 0, { "JNEQZ $a" }},
{"JNEQZ",     "S",     0,  0,  0,  2,  1, K_JNEQZ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JNEQZ_R, { NULL }},
{"JEQZ",      "S",     0,  0,  0,  2,  1, K_JEQZ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JEQZ_R, { NULL }},
{"JUMP",      "S",     0,  0,  0,  2,  1, K_JUMP_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JUMP_R, { NULL }},
{"QPLUS",     "",      0,  0,  0,  1,  1, K_QPLUS, { NULL }},
{"QMINUS",    "",      0,  0,  0,  1,  1, K_QMINUS, { NULL }},
{"QTIMES",    "",      0,  0,  0,  1,  1, K_QTIMES, { NULL }},
{"QUMINUS",   "",      0,  0,  0,  1,  1, K_QUMINUS, { NULL }},
{"QDIV",      "",      0,  0,  0,  1,  1, K_QDIV, { NULL }},
{"QMOD",      "",      0,  0,  0,  1,  1, K_QMOD, { NULL }},
{"QINC",      "",      0,  0,  0,  0,  0, 0, { "PUSH 1", "CONVNQ", "QPLUS" }},
{"QDEC",      "",      0,  0,  0,  0,  0, 0, { "PUSH 1", "CONVNQ", "QMINUS" }},
{"JCASE",     "1",     0,  0,  0,  2,  1, K_JCASE_1, { NULL }},
{"CASEL",     "R",     0,  0,  0,  2,  0, 0, { NULL }},
{"JRANGE",    "S",     0,  0,  0,  2,  1, K_JRANGE_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_JRANGE_R, { NULL }},
{"TESTGEQ",   "S",     0,  0,  0,  2,  1, K_TESTGEQ_S, { NULL }},
{   NULL,     "R",     0,  0,  0,  3,  1, K_TESTGEQ_R, { NULL }},
{"FPLUS",     "",      0,  0,  0,  1,  1, K_FPLUS, { NULL }},
{"FMINUS",    "",      0,  0,  0,  1,  1, K_FMINUS, { NULL }},
{"FTIMES",    "",      0,  0,  0,  1,  1, K_FTIMES, { NULL }},
{"FDIV",      "",      0,  0,  0,  1,  1, K_FDIV, { NULL }},
{"FUMINUS",   "",      0,  0,  0,  1,  1, K_FUMINUS, { NULL }},
{"FCMP",      "",      0,  0,  0,  1,  1, K_FCMP, { NULL }},
{"FEQ",       "",      0,  0,  0,  0,  0, 0, { "FCMP", "PUSH 0", "EQ" }},
{"FLT",       "",      0,  0,  0,  0,  0, 0, { "FCMP", "PUSH 0", "LT" }},
{"FGT",       "",      0,  0,  0,  0,  0, 0, { "FCMP", "PUSH 0", "GT" }},
{"FLEQ",      "",      0,  0,  0,  0,  0, 0, { "FCMP", "PUSH 0", "LEQ" }},
{"FGEQ",      "",      0,  0,  0,  0,  0, 0, { "FCMP", "PUSH 0", "GEQ" }},
{"FNEQ",      "",      0,  0,  0,  0,  0, 0, { "FCMP", "PUSH 0", "NEQ" }},
{"FJEQ",      "R",     0,  0,  0,  0,  0, 0, { "FCMP", "JEQZ $a" }},
{"FJLT",      "R",     0,  0,  0,  0,  0, 0, { "FCMP", "JLTZ $a" }},
{"FJGT",      "R",     0,  0,  0,  0,  0, 0, { "FCMP", "JGTZ $a" }},
{"FJLEQ",     "R",     0,  0,  0,  0,  0, 0, { "FCMP", "JLEQZ $a" }},
{"FJGEQ",     "R",     0,  0,  0,  0,  0, 0, { "FCMP", "JGEQZ $a" }},
{"FJNEQ",     "R",     0,  0,  0,  0,  0, 0, { "FCMP", "JNEQZ $a" }},
{"DPLUS",     "",      0,  0,  0,  1,  1, K_DPLUS, { NULL }},
{"DMINUS",    "",      0,  0,  0,  1,  1, K_DMINUS, { NULL }},
{"DTIMES",    "",      0,  0,  0,  1,  1, K_DTIMES, { NULL }},
{"DDIV",      "",      0,  0,  0,  1,  1, K_DDIV, { NULL }},
{"DUMINUS",   "",      0,  0,  0,  1,  1, K_DUMINUS, { NULL }},
{"DCMP",      "",      0,  0,  0,  1,  1, K_DCMP, { NULL }},
{"DEQ",       "",      0,  0,  0,  0,  0, 0, { "DCMP", "PUSH 0", "EQ" }},
{"DLT",       "",      0,  0,  0,  0,  0, 0, { "DCMP", "PUSH 0", "LT" }},
{"DGT",       "",      0,  0,  0,  0,  0, 0, { "DCMP", "PUSH 0", "GT" }},
{"DLEQ",      "",      0,  0,  0,  0,  0, 0, { "DCMP", "PUSH 0", "LEQ" }},
{"DGEQ",      "",      0,  0,  0,  0,  0, 0, { "DCMP", "PUSH 0", "GEQ" }},
{"DNEQ",      "",      0,  0,  0,  0,  0, 0, { "DCMP", "PUSH 0", "NEQ" }},
{"DJEQ",      "S",     0,  0,  0,  0,  0, 0, { "DCMP", "JEQZ $a" }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "DEQ", "JUMPT $a" }},
{"DJLT",      "S",     0,  0,  0,  0,  0, 0, { "DCMP", "JLTZ $a" }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "DLT", "JUMPT $a" }},
{"DJGT",      "S",     0,  0,  0,  0,  0, 0, { "DCMP", "JGTZ $a" }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "DGT", "JUMPT $a" }},
{"DJLEQ",     "S",     0,  0,  0,  0,  0, 0, { "DCMP", "JLEQZ $a" }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "DLEQ", "JUMPT $a" }},
{"DJGEQ",     "S",     0,  0,  0,  0,  0, 0, { "DCMP", "JGEQZ $a" }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "DGEQ", "JUMPT $a" }},
{"DJNEQ",     "S",     0,  0,  0,  0,  0, 0, { "DCMP", "JNEQZ $a" }},
{   NULL,     "R",     0,  0,  0,  0,  0, 0, { "DNEQ", "JUMPT $a" }},
{"QCMP",      "",      0,  0,  0,  1,  1, K_QCMP, { NULL }},
{"QEQ",       "",      0,  0,  0,  0,  0, 0, { "QCMP", "PUSH 0", "EQ" }},
{"QLT",       "",      0,  0,  0,  0,  0, 0, { "QCMP", "PUSH 0", "LT" }},
{"QGT",       "",      0,  0,  0,  0,  0, 0, { "QCMP", "PUSH 0", "GT" }},
{"QLEQ",      "",      0,  0,  0,  0,  0, 0, { "QCMP", "PUSH 0", "LEQ" }},
{"QGEQ",      "",      0,  0,  0,  0,  0, 0, { "QCMP", "PUSH 0", "GEQ" }},
{"QNEQ",      "",      0,  0,  0,  0,  0, 0, { "QCMP", "PUSH 0", "NEQ" }},
{"QJEQ",      "R",     0,  0,  0,  0,  0, 0, { "QCMP", "JEQZ $a" }},
{"QJLT",      "R",     0,  0,  0,  0,  0, 0, { "QCMP", "JLTZ $a" }},
{"QJGT",      "R",     0,  0,  0,  0,  0, 0, { "QCMP", "JGTZ $a" }},
{"QJLEQ",     "R",     0,  0,  0,  0,  0, 0, { "QCMP", "JLEQZ $a" }},
{"QJGEQ",     "R",     0,  0,  0,  0,  0, 0, { "QCMP", "JGEQZ $a" }},
{"QJNEQ",     "R",     0,  0,  0,  0,  0, 0, { "QCMP", "JNEQZ $a" }},
{"CONVNF",    "",      0,  0,  0,  1,  1, K_CONVNF, { NULL }},
{"CONVND",    "",      0,  0,  0,  1,  1, K_CONVND, { NULL }},
{"CONVFD",    "",      0,  0,  0,  1,  1, K_CONVFD, { NULL }},
{"CONVDF",    "",      0,  0,  0,  1,  1, K_CONVDF, { NULL }},
{"CONVNC",    "",      0,  0,  0,  1,  1, K_CONVNC, { NULL }},
{"CONVNS",    "",      0,  0,  0,  1,  1, K_CONVNS, { NULL }},
{"CONVNQ",    "",      0,  0,  0,  1,  1, K_CONVNQ, { NULL }},
{"CONVQN",    "",      0,  0,  0,  1,  1, K_CONVQN, { NULL }},
{"CONVQD",    "",      0,  0,  0,  1,  1, K_CONVQD, { NULL }},
{"BOUND",     "2",     0,  0,  0,  3,  1, K_BOUND_2, { NULL }},
{"NCHECK",    "2",     0,  0,  0,  3,  1, K_NCHECK_2, { NULL }},
{"GCHECK",    "2",     0,  0,  0,  3,  1, K_GCHECK_2, { NULL }},
{"ZCHECK",    "2",     0,  0,  0,  3,  1, K_ZCHECK_2, { NULL }},
{"FZCHECK",   "2",     0,  0,  0,  3,  1, K_FZCHECK_2, { NULL }},
{"DZCHECK",   "2",     0,  0,  0,  3,  1, K_DZCHECK_2, { NULL }},
{"QZCHECK",   "2",     0,  0,  0,  3,  1, K_QZCHECK_2, { NULL }},
{"ERROR",     "12",    0,  0,  0,  4,  1, K_ERROR_12, { NULL }},
{"EASSERT",   "2",     0,  0,  0,  0,  0, 0, { "RESULTW", "ERROR E_ASSERT $a" }},
{"ALIGNC",    "",      0,  0,  0,  1,  1, K_ALIGNC, { NULL }},
{"ALIGNS",    "",      0,  0,  0,  1,  1, K_ALIGNS, { NULL }},
{"FIXCOPY",   "",      0,  0,  0,  1,  1, K_FIXCOPY, { NULL }},
{"FLEXCOPY",  "",      0,  0,  0,  1,  1, K_FLEXCOPY, { NULL }},
{"TYPETEST",  "1",     0,  0,  0,  2,  1, K_TYPETEST_1, { NULL }},
{"CALL",      "1",     0,  0,  0,  0,  0, 0, { "JPROC", "SLIDE $a" }},
{"CALLW",     "1",     0,  0,  0,  0,  0, 0, { "JPROC", "SLIDEW $a" }},
{"CALLQ",     "1",     0,  0,  0,  0,  0, 0, { "JPROC", "SLIDEQ $a" }},
{"CALLF",     "1",     0,  0,  0,  0,  0, 0, { "JPROC", "SLIDEF $a" }},
{"CALLD",     "1",     0,  0,  0,  0,  0, 0, { "JPROC", "SLIDED $a" }},
{"LINK",      "",      0,  0,  0,  1,  1, K_LINK, { NULL }},
{"SAVELINK",  "",      0,  0,  0,  1,  1, K_SAVELINK, { NULL }},
{"JPROC",     "",      0,  0,  0,  1,  1, K_JPROC, { NULL }},
{"SLIDE",     "1",     0,  0,  0,  2,  1, K_SLIDE_1, { NULL }},
{"SLIDEW",    "1",     0,  0,  0,  2,  1, K_SLIDEW_1, { NULL }},
{"SLIDED",    "1",     0,  0,  0,  2,  1, K_SLIDED_1, { NULL }},
{"SLIDEF",    "1",     0,  0,  0,  2,  1, K_SLIDEF_1, { NULL }},
{"SLIDEQ",    "1",     0,  0,  0,  2,  1, K_SLIDEQ_1, { NULL }},
{"RETURNW",   "",      0,  0,  0,  0,  0, 0, { "RESULTW", "RETURN" }},
{"RETURNF",   "",      0,  0,  0,  0,  0, 0, { "RESULTF", "RETURN" }},
{"RETURND",   "",      0,  0,  0,  0,  0, 0, { "RESULTD", "RETURN" }},
{"RETURNQ",   "",      0,  0,  0,  0,  0, 0, { "RESULTQ", "RETURN" }},
{"RESULTW",   "",      0,  0,  0,  1,  1, K_RESULTW, { NULL }},
{"RESULTD",   "",      0,  0,  0,  1,  1, K_RESULTD, { NULL }},
{"RESULTF",   "",      0,  0,  0,  1,  1, K_RESULTF, { NULL }},
{"RESULTQ",   "",      0,  0,  0,  1,  1, K_RESULTQ, { NULL }},
{"RETURN",    "",      0,  0,  0,  1,  1, K_RETURN, { NULL }},
{"LNUM",      "2",     0,  0,  0,  3,  1, K_LNUM_2, { NULL }},
{"BREAK",     "2",     0,  0,  0,  3,  1, K_BREAK_2, { NULL }},
{"CONST",     "?",     0,  0,  0, -1,  1, D_CONST, { NULL }},
{"GLOBAL",    "?",     0,  0,  0, -1,  1, D_GLOBAL, { NULL }},
{"FCONST",    "?",     0,  0,  0, -1,  1, D_FCONST, { NULL }},
{"DCONST",    "?",     0,  0,  0, -1,  1, D_DCONST, { NULL }},
{"QCONST",    "?",     0,  0,  0, -1,  1, D_QCONST, { NULL }},
{"LABEL",     "?",     0,  0,  0, -1,  1, D_LABEL, { NULL }},
{"PROC",      "????",  0,  0,  0, -1,  1, D_PROC, { NULL }},
{"END",       "",      0,  0,  0, -1,  1, D_END, { NULL }},
{"PRIMDEF",   "????",  0,  0,  0, -1,  1, D_PRIMDEF, { NULL }},
{"DEFINE",    "?",     0,  0,  0, -1,  1, D_DEFINE, { NULL }},
{"STRING",    "?",     0,  0,  0, -1,  1, D_STRING, { NULL }},
{"GLOVAR",    "??",    0,  0,  0, -1,  1, D_GLOVAR, { NULL }},
{"WORD",      "?",     0,  0,  0, -1,  1, D_WORD, { NULL }},
{"MODULE",    "???",   0,  0,  0, -1,  1, D_MODULE, { NULL }},
{"ENDHDR",    "",      0,  0,  0, -1,  1, D_ENDHDR, { NULL }},
{"IMPORT",    "??",    0,  0,  0, -1,  1, D_IMPORT, { NULL }},
{"PRIM",      "?",     0,  0,  0, -1,  1, D_PRIM, { NULL }},
{"STKMAP",    "?",     0,  0,  0, -1,  1, D_STKMAP, { NULL }},
{"FLOAT",     "?",     0,  0,  0, -1,  1, D_FLOAT, { NULL }},
{"DOUBLE",    "?",     0,  0,  0, -1,  1, D_DOUBLE, { NULL }},
{"LONG",      "?",     0,  0,  0, -1,  1, D_LONG, { NULL }},
{"LINE",      "?",     0,  0,  0, -1,  1, D_LINE, { NULL }},
};

short templ_trie[NTRIE] = {
 -67,  -80,    3,  273,  -66,  -72,  -60,    9,  -69,  244, 
 -56,  245,   13,  107,  120,  -65,  -64,   14,   24,  -50, 
  21,  117,  -48,   11,  112,  -28,  -55,  115,   30,   27, 
 113,   32,  116,  -35,  -47,   36,  114,  -40,  -29,   40, 
 235,  -23,  -32,   44,  272,  -30,  -27,  249,  253,   47, 
 252,  251,  250,  -15,  -21,   56,  166,  273,   61,  -18, 
  57,  273,   -7,   88,   23,  -68,  -57,  -20,   68,  152, 
 173,  273,   25,  283,  320,   34,  365,  464,  484,  488, 
 489,  521,  612,  641,  741,  762,  -59,  767,  229,  228, 
 768,   89,   94,   96,  230,   97,  227,  226,  232,  -53, 
 -54,  231,  103,  234,  -52,  233,   98,  194,  101,  -45, 
 118,  107,  105,   38,   31,   48,   35,   50,  276,   39, 
 192,   26,  111,   98,  129,  120,   49,   55,   51,  282, 
 195,  199,  197,  121,   52,   37,   46,   84,  201,   80, 
 -26,   70,   90,   -6,  108,  109,  114,  111,  119,   69, 
 131,  122,  124,  127,   53,  138,  133,  162,  144,   74, 
  76,   91,  209,  205,  132,  130,  100,  170,  104,   86, 
 207,  203,  211,   93,  172,  163,  178,   95,  198,  196, 
 106,  190,  102,  113,  171,  200,  189,  112,  117,  292, 
 189,  110,  179,  181,  185,  115,  121,  125,  123,  190, 
 116,  191,  134,  126,  135,  193,  136,  210,  128,  201, 
 100,  140,  145,  141,  146,  240,  215,  139,  147,  243, 
 205,  137,  142,  224,  280,  149,  150,  219,  287,  143, 
 155,  123,  228,  231,  153,  157,  156,  242,  237,  248, 
 166,  182,  183,  159,  193,  174,  186,  204,  176,  225, 
 238,  247,  176,  243,  275,  180,  174,  250,  251,  177, 
 177,  254,  184,  266,  260,  267,  256,  181,  179,  191, 
 192,  246,  187,  197,  202,  214,  271,  268,  279,  183, 
 217,  284,  221,  200,  187,  185,  206,  288,  186,  184, 
 211,  188,  291,  295,  212,  180,  158,  220,  285,  222, 
 230,  289,  216,  247,  241,  303,  223,  308,  291,  310, 
 178,  234,  229,  172,  232,  313,  236,  318,  182,  235, 
 239,  171,  321,  249,  242,  327,  255,  173,  257,  252, 
 253,  244,  175,  262,  269,  332,  339,  261,  270,  239, 
 272,  237,  265,  278,  274,  275,  347,  127,  286,  276, 
 341,  277,  274,  352,  284,  282,  125,  356,  280,  288, 
 287,  297,  354,  292,  366,  302,  110,  290,  293,  281, 
  96,  294,  374,  375,   10,   13,  359,   11,   12,  298, 
 383,  311,  305,  165,  129,  384,  153,  314,  377,  304, 
 394,  324,  378,  309,  137,  147,  334,  133,  342,  143, 
 333,  135,  355,  323,  401,  350,  145,  131,  397,  141, 
 412,  329,  139,  151,  349,  335,  417,  256,  407,  353, 
 343,  348,  423,  167,  425,  155,  149,  344,  150,  362, 
 363,  360,  278,  381,  408,   72,  432,   46,  415,   88, 
  48,   94,  370,  416,  445,  435,   44,   42,  456,  535, 
 358,   67,  396,   84,  405,   65,  430,  438,   90,  398, 
  66,   64,   78,  437,  439,    5,  440,   82,    3,   28, 
  86,   30,  451,  453,  386,  455,   92,  441,   26,  446, 
  22,   58,  126,  447,  395,  481,  458,  399,  460,  482, 
 494,  406,  461,  421,  294,  426,  499,  409,  462,  254, 
 465,  427,  413,  271,  503,  469,  470,   16,  471,  428, 
 448,  467,  457,   76,   17,   80,  459,  468,   15,  476, 
  14,  478,  452,  463,    7,  480,  507,  513,  524,  514, 
 293,  118,  531,  119,  530,  124,  104,  466,  533,  472, 
 515,  545,  518,  473,  454,  122,  520,  286,  547,  483, 
 479,  477,  487,  486,  485,  536,  236,  128,  109,  558, 
 556,  108,  103,  475,    9,  490,  102,  557,  492,  289, 
 561,  500,  566,  501,  502,  493,  562,  510,  569,  516, 
 581,  281,  279,  582,  586,  512,    0,  213,  523,  539, 
 534,  587,  548,  596,  542,  555,  277,  576,  577,  590, 
 511,  588,  525,  526,  607,  593,  594,  164,  537,  509, 
 161,  609,  527,  610,  214,  614,  218,  538,  216,  616, 
 554,  622,  163,  220,  544,  623,  559,  564,  553,  224, 
 474,  567,  618,  571,  629,  222,  560,  508,  223,  221, 
 563,  638,  225,  635,  642,  565,  647,  217,  215,  158, 
 574,  639,  568,  572,  162,  649,  589,  654,  219,  580, 
 648,  658,  665,  579,  578,  157,  591,  592,  598,  159, 
 595,  597,  160,  599,  608,  669,  602,  611,  604,  680, 
 241,  615,  273,  672,  600,  624,  682,  267,  613,  601, 
 268,  269,  687,  266,  690,  270,  264,  263,  603,  617, 
 265,  625,  619,  695,  262,  691,  621,  640,  637,  632, 
 634,  693,  643,  255,  715,  257,  259,  646,  713,  652, 
 645,  260,  261,  258,  656,  661,  657,  273,  739,   74, 
 651,  727,  674,   54,  683,   89,  685,  705,  729,  694, 
 707,  733,  735,  726,  745,   56,   95,   52,   50,   71, 
 749,  752,   85,  754,   69,  746,   91,  747,   70,   68, 
 290,  748,  702,  696,  756,  697,  758,  688,  760,   38, 
 759,   87,  769,  771,   40,  774,  700,   93,   36,   32, 
  61,  780,  704,  716,   20,  721,  777,  730,  778,  718, 
  77,   21,  779,   81,   19,   18,  722,  784,  790,  725, 
 791,  283,  723,  734,  728,  801,  101,  731,  806,  743, 
 724,  793,  737,  794,  742,  738,  169,  795,  816,  751, 
 740,  105,  750,  821,  753,  248,  755,  106,  285,  238, 
 744,    0,    0,    0,  757,  775,  764,    0,  761,  763, 
 828,    0,  765,  773,    0,  825,  770,  782,  829,  827, 
 766,  776,  772, 
};

uchar templ_check[NTRIE] = {
  68,   68,   82,    0,   73,   71,   78,   67,   76,    0, 
  78,    0,   68,    0,    0,   83,   73,   82,   68,   78, 
  84,    0,   79,   83,    0,   82,   79,    0,   82,   84, 
   0,   66,    0,   85,   79,   82,    0,   85,   78,   68, 
   0,   69,   65,   75,    0,   65,   76,    0,    0,   76, 
   0,    0,    0,   83,   69,   76,    0,    0,   84,   79, 
  78,    0,   68,   70,   70,   65,   66,   67,   68,   69, 
  70,   71,   78,   73,   74,   81,   76,   77,   78,   79, 
  80,   81,   82,   83,   84,   85,   65,   87,    0,    0, 
  90,   68,   67,   68,    0,   70,    0,    0,    0,   78, 
  79,    0,   68,    0,   83,    0,   81,    0,   83,   88, 
  84,   80,   78,   78,   77,   68,   79,   70,    0,   73, 
   0,   83,    0,    0,   69,   86,   73,   78,   81,    0, 
   0,    0,    0,    0,   87,   67,   68,   69,    0,   71, 
  83,   73,   74,   86,   76,   77,   78,   79,   80,   69, 
  81,   67,   84,   85,   70,   81,   86,   81,   90,   69, 
  69,   71,    0,    0,   84,   81,   76,   81,   78,   69, 
   0,    0,    0,   69,   81,   84,   81,   69,    0,    0, 
  78,    0,   73,   69,   84,    0,   69,   66,   76,    0, 
   0,   85,   84,   83,   81,   76,   85,   73,   76,   83, 
  85,    0,   77,   69,   77,    0,   78,   80,   73,   83, 
   0,   67,   72,   67,   69,    0,   75,   65,   69,    0, 
  83,   85,   83,   68,    0,   83,   68,   84,    0,   82, 
  78,    0,   82,   81,   82,   82,   79,    0,   82,   80, 
  67,   68,   69,   77,   71,   79,   73,   74,    0,   76, 
  77,   78,   78,   80,    0,   73,    0,   84,   85,   83, 
   0,   84,   69,   90,   81,   81,   86,    0,    0,   67, 
  79,    0,   80,   69,   88,   71,   89,   84,   81,    0, 
  76,   81,   78,   69,    0,    0,   69,   81,    0,    0, 
  69,    0,   81,   81,   69,    0,   72,   67,   84,   79, 
  88,   84,   80,    0,   79,   89,   65,   84,    0,   84, 
   0,   73,   78,    0,   85,   83,   69,   81,    0,   76, 
  85,    0,   83,   73,   69,   83,   77,    0,   77,   85, 
  73,   78,    0,   67,   72,   83,   75,   67,   69,    0, 
  67,    0,   69,   69,   72,   67,   81,    0,   66,   76, 
  75,   65,    0,   76,    0,   79,    0,   84,   65,    0, 
  77,   78,   82,   82,   67,   68,    0,   80,   86,   79, 
   0,   69,   67,   68,    0,    0,   84,    0,    0,   65, 
  69,   83,   88,    0,    0,   81,    0,   67,   83,   69, 
  81,   71,   87,   69,    0,    0,   76,    0,   78,    0, 
  80,    0,   82,   69,   81,   85,    0,    0,   84,    0, 
  81,   69,    0,    0,   79,   82,   67,    0,   84,   71, 
  65,   78,   69,    0,   80,    0,    0,   77,    0,   66, 
  65,   69,    0,   68,   69,    0,   76,    0,   73,    0, 
   0,    0,   76,   78,   79,   87,    0,    0,   83,   84, 
  69,    0,   71,    0,   73,    0,   75,   76,    0,   78, 
   0,    0,    0,   67,   68,    0,   70,    0,    0,    0, 
   0,    0,   67,   68,   90,   70,    0,   81,    0,   83, 
   0,    0,    0,   87,   90,   87,   81,   90,   83,   81, 
  69,   90,   87,   78,    0,   70,   75,   90,   68,    0, 
  70,   85,   90,    0,   77,   67,   68,    0,   70,   84, 
  65,   81,   67,    0,    0,    0,   68,   87,    0,   81, 
   0,   83,   65,   78,    0,   87,   67,   68,   76,   70, 
   0,    0,   76,    0,   71,    0,    0,   73,   82,   85, 
  81,   68,   83,   79,   78,    0,   87,    0,   69,   72, 
  76,   67,   69,   69,   67,   83,    0,    0,    0,   84, 
  75,    0,    0,   79,    0,   76,    0,   81,   79,    0, 
  82,   82,   80,   73,   85,   85,   83,   69,   77,   79, 
  70,    0,    0,   67,   72,   83,    0,    0,   67,   68, 
  69,   80,   71,   84,   73,   74,    0,   76,   77,   78, 
  77,   80,   79,   78,   67,   84,   85,    0,   69,   83, 
   0,   90,   73,   86,    0,   81,    0,   69,    0,   81, 
  78,   67,    0,    0,   69,   81,   71,   65,   69,    0, 
  85,   76,   84,   78,   81,    0,   69,   68,    0,    0, 
  69,   81,    0,   84,   81,   69,   81,    0,    0,    0, 
  73,   84,   78,   85,    0,   83,   79,   68,    0,   69, 
  84,   81,   83,   85,   76,    0,   73,   69,   77,    0, 
  73,   77,    0,   78,   72,   83,   67,   69,   67,   75, 
   0,   69,    0,   83,   85,   84,   71,    0,   85,   76, 
   0,    0,   68,    0,   70,    0,    0,    0,   83,   84, 
   0,   82,   85,   78,    0,   81,   65,   86,   76,   69, 
  73,   87,   78,    0,   69,    0,    0,   76,   75,   73, 
  68,    0,    0,    0,   67,   84,   65,    0,   87,    0, 
  69,   75,   71,    0,   73,    0,   75,   76,   87,   78, 
  79,   67,   68,   82,   70,    0,    0,    0,    0,    0, 
  67,   68,    0,   70,    0,   81,    0,   83,    0,    0, 
   0,   87,   77,   68,   81,   70,   83,   65,   80,    0, 
  87,    0,   67,   68,    0,   70,   81,    0,    0,    0, 
   0,   87,   87,   68,    0,   70,   81,   69,   83,   82, 
   0,    0,   87,    0,    0,    0,   81,   67,   68,   73, 
  70,    0,   87,   78,   65,   71,    0,   83,   80,   71, 
  69,   81,   69,   83,   73,   84,    0,   87,   81,   77, 
  69,    0,   69,   83,   80,    0,   69,    0,    0,    0, 
  89,  128,  128,  128,   84,   67,   73,  128,   83,   77, 
  68,  128,   78,   67,  128,   84,   79,   72,   75,   83, 
  85,   69,   82, 
};