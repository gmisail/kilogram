#ifndef K_STRING
#define K_STRING

typedef struct {
	char* content;
	int size;
} KiloString;

KiloString* string_create(char*);
void string_destroy(KiloString*);

#endif
