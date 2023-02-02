#ifndef K_OBJECT
#define K_OBJECT

#include "type.h"

/*
 * Represents heap-allocated data. 
 * */
typedef struct {
	void* data;
	KiloType type;
} KiloObject;

KiloObject* object_create(void*, KiloType);
void* object_get_data(KiloObject*);
void object_destroy(KiloObject*);

#endif
