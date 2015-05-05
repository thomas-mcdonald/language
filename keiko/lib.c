#include "obx.h"
#include <stdio.h>

unsigned prim_check = 0;

#define args (fp + HEAD)

static void Lib_Print(value *sp) {
     value *fp = sp;
     printf(" %d", args[0].i);
}

static void Lib_Newline(value *sp) {
     printf("\n");
}

static void Lib_New(value *sp) {
    value *fp = sp;
    value *bp = sp;
    FPINIT;
    value *desc = args[1].p, *block;
    int size = args[2].i;
    block = (value * ) scratch_alloc(size, TRUE);
    *block = (value) desc;
    ( *args[0].p ).p = block;
}

void dltrap(value *sp) {
     fprintf(stderr, "Oops: dltrap called!\n");
     exit(2);
}

primitive *primtab[] = {
     interp, dltrap, Lib_Print, Lib_Newline, Lib_New,
     NULL
};

char *primname[] = {
     "INTERP", "DLTRAP", "Lib_Print", "Lib_Newline", "Lib_New"
};

