#include "object.h"
#include "string.h"
#include "type.h"

#include <stdio.h>
#include <stdlib.h>

int main() {
	int* number = (int*) malloc(sizeof(int));
	*number = 150;

	KiloObject* obj = object_create(number, TYPE_INT);
				  
				// this is inferred by the compiler
	printf("%d\n", *((int*) object_get_data(obj)));

	KiloString* str = string_create("Hello world!");
	printf("%s\n", str->content);

	object_destroy(obj);
	string_destroy(str);

	return 0;
}
