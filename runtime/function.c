#include <stdlib.h>

#include "function.h"

KiloFunction* function_create(void* body, void* env) {
	KiloFunction* func = malloc(sizeof(KiloFunction));
	func->body = body;
	func->env = env;
	return func; 
}
