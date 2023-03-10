#ifndef K_FUNCTION
#define K_FUNCTION

typedef struct
{
	// Function pointer to the function's implementation.
	void (*body)(void);

	// Structure of variables captured by the function.
	void *env;
} KiloFunction;

KiloFunction *function_create(void *);

#endif
