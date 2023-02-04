#include <stdlib.h>

#include "function.h"

KiloFunction* function_create(void* body, void* captured) {
	KiloFunction* func = malloc(sizeof(KiloFunction));
	func->body = body;
	func->captured = captured;
	return func; 
}
