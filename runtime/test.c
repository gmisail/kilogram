#include "object.h"
#include "type.h"
#include <stdio.h>
#include <stdlib.h>

int main() {
	int* number = (int*) malloc(sizeof(int));
	*number = 150;

	KiloObject* obj = object_create(number, TYPE_INT);
	printf("%d\n", *((int*) object_get_data(obj)));
	object_destroy(obj);

	return 0;
}
