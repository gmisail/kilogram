#include <stdio.h>

#include "stdlib.h"
#include "string.h"

KiloString* int_to_string(int num) {
	char buffer[32];
	sprintf(buffer, "%d", num); 

	return string_create(buffer);
}

int print(KiloString* str) {
	printf("%s\n", str->content);

	return 0;
}
