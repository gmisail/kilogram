#include "string.h"

#include <stdlib.h>
#include <string.h>

KiloString* string_create(char* data) {
	KiloString* string = malloc(sizeof(KiloString));
	string->content = (char*) calloc(sizeof(data) + 1, sizeof(char));

	// Copy data from given string to heap-allocated string.
	strcpy(string->content, data);

	return string;
}

void string_destroy(KiloString* string) {
	free(string->content);
	free(string);
}
