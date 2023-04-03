#ifndef KILO_STDLIB
#define KILO_STDLIB

#include "string.h"

int print(KiloString*);
int panic(int);

KiloString* input(KiloString*);

KiloString* int_to_string(int);

#endif
