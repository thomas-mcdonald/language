#include "obx.h"

#include "gc.c"

#define args (bp + HEAD)


PRIMDEF void new(value *sp) {
  value UNUSED *bp = sp;
  value UNUSED *cp = bp[CP].p;
  FPINIT;
  value *desc = args[1].p, *block;
  int size = args[2].i;
  //
  // ( *args[0].p ).p = NULL;    /* Free old storage */
  block = (value * ) gc_alloc(desc, size, bp);
  ( *args[0].p ).p = block;
}
