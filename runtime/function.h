#ifndef K_FUNCTION
#define K_FUNCTION

typedef struct {
	// Function pointer to the function's implementation.
	void* body;

	// Structure of variables captured by the function.
	void* captured;
} KiloFunction;

KiloFunction* function_create(void*, void*);

#endif

