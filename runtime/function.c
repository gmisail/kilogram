#include <stdlib.h>

#include "function.h"

KiloFunction* function_create(void* body) {
	KiloFunction* func = malloc(sizeof(KiloFunction));
	func->body = body;
	func->env = NULL;
	return func; 
}


