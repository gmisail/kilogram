#include <stdlib.h>

#include "object.h"
#include "type.h"

/*
 * Creates a new heap-allocated object.
 * */
KiloObject* object_create(void* data, KiloType type) {
	KiloObject* object = (KiloObject*) malloc(sizeof(KiloObject));
	object->data = data;
	object->type = type;
	return object;
}

/*
 * Gets the data that this object is pointing to.
 * */
void* object_get_data(KiloObject* object) {
	return object->data;
}

/*
 * Frees the data pointed to by this object and then frees itself.
 * */
void object_destroy(KiloObject* object) {
	free(object->data);
	free(object);
}
