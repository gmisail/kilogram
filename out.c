#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "runtime/string.h"
#include "runtime/object.h"
#include "runtime/function.h"
#include "runtime/stdlib.h"

// Record header

// Enum header

// Box constructor header
void* _kg_box_int(int data){
int* tmp = (int*) malloc(sizeof(int));
*tmp = data;
return (void*) tmp;
}
void* _kg_box_KiloString_ptr(KiloString* data){
KiloString** tmp = (KiloString**) malloc(sizeof(KiloString*));
*tmp = data;
return (void*) tmp;
}

// Branch header

// Function header
typedef struct {
	
} _kg_function_0_env;

_kg_function_0_env* _create__kg_function_0_env(){
return NULL;
}

void* _kg_function_0 (void* val, void* val2, void* val3, _kg_function_0_env* env){

return val;
}
KiloFunction* create__kg_function_0(){
KiloFunction* tmp = function_create(_kg_function_0);
tmp->env = _create__kg_function_0_env();
return tmp;
}


// Program
int main(int argc, char** argv){
KiloFunction* identity = create__kg_function_0();
int int_type = *((int*) ((void* (*)(void*, void*, void*, void*)) identity->body)(_kg_box_int(10), _kg_box_int(20), _kg_box_KiloString_ptr(string_create("hello")), identity->env));
return int_type;
}