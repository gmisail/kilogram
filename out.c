#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "runtime/string.h"
#include "runtime/object.h"
#include "runtime/function.h"
#include "runtime/stdlib.h"

// Record header
typedef struct Pair_float_string Pair_float_string;
typedef struct Pair_int_bool Pair_int_bool;
typedef struct Pair_string_int Pair_string_int;
typedef struct Pair_int_int Pair_int_int;
typedef struct Pair_bool_string Pair_bool_string;
typedef struct Pair_int_float Pair_int_float;
struct Pair_float_string {
	float first;
	KiloString* second;
};
Pair_float_string* _create_Pair_float_string(float first, KiloString* second){
Pair_float_string* tmp = malloc(sizeof(Pair_float_string));
(*tmp).first = first;
(*tmp).second = second;
return tmp;
}
struct Pair_int_bool {
	int first;
	bool second;
};
Pair_int_bool* _create_Pair_int_bool(int first, bool second){
Pair_int_bool* tmp = malloc(sizeof(Pair_int_bool));
(*tmp).first = first;
(*tmp).second = second;
return tmp;
}
struct Pair_string_int {
	KiloString* first;
	int second;
};
Pair_string_int* _create_Pair_string_int(KiloString* first, int second){
Pair_string_int* tmp = malloc(sizeof(Pair_string_int));
(*tmp).first = first;
(*tmp).second = second;
return tmp;
}
struct Pair_int_int {
	int first;
	int second;
};
Pair_int_int* _create_Pair_int_int(int first, int second){
Pair_int_int* tmp = malloc(sizeof(Pair_int_int));
(*tmp).first = first;
(*tmp).second = second;
return tmp;
}
struct Pair_bool_string {
	bool first;
	KiloString* second;
};
Pair_bool_string* _create_Pair_bool_string(bool first, KiloString* second){
Pair_bool_string* tmp = malloc(sizeof(Pair_bool_string));
(*tmp).first = first;
(*tmp).second = second;
return tmp;
}
struct Pair_int_float {
	int first;
	float second;
};
Pair_int_float* _create_Pair_int_float(int first, float second){
Pair_int_float* tmp = malloc(sizeof(Pair_int_float));
(*tmp).first = first;
(*tmp).second = second;
return tmp;
}

// Enum header

// Box constructor header

// Branch header

// Function header
typedef struct {
	
} _kg_function_0_env;

_kg_function_0_env* _create__kg_function_0_env(){
return NULL;
}

Pair_int_int* _kg_function_0 (int first, int second, _kg_function_0_env* env){

return _create_Pair_int_int(first, second);
}
KiloFunction* create__kg_function_0(){
KiloFunction* tmp = function_create(_kg_function_0);
tmp->env = _create__kg_function_0_env();
return tmp;
}

typedef struct {
	
} _kg_function_1_env;

_kg_function_1_env* _create__kg_function_1_env(){
return NULL;
}

Pair_int_float* _kg_function_1 (int first, float second, _kg_function_1_env* env){

return _create_Pair_int_float(first, second);
}
KiloFunction* create__kg_function_1(){
KiloFunction* tmp = function_create(_kg_function_1);
tmp->env = _create__kg_function_1_env();
return tmp;
}

typedef struct {
	
} _kg_function_2_env;

_kg_function_2_env* _create__kg_function_2_env(){
return NULL;
}

Pair_float_string* _kg_function_2 (float first, KiloString* second, _kg_function_2_env* env){

return _create_Pair_float_string(first, second);
}
KiloFunction* create__kg_function_2(){
KiloFunction* tmp = function_create(_kg_function_2);
tmp->env = _create__kg_function_2_env();
return tmp;
}

typedef struct {
	
} _kg_function_3_env;

_kg_function_3_env* _create__kg_function_3_env(){
return NULL;
}

Pair_string_int* _kg_function_3 (KiloString* first, int second, _kg_function_3_env* env){

return _create_Pair_string_int(first, second);
}
KiloFunction* create__kg_function_3(){
KiloFunction* tmp = function_create(_kg_function_3);
tmp->env = _create__kg_function_3_env();
return tmp;
}

typedef struct {
	
} _kg_function_4_env;

_kg_function_4_env* _create__kg_function_4_env(){
return NULL;
}

Pair_bool_string* _kg_function_4 (bool first, KiloString* second, _kg_function_4_env* env){

return _create_Pair_bool_string(first, second);
}
KiloFunction* create__kg_function_4(){
KiloFunction* tmp = function_create(_kg_function_4);
tmp->env = _create__kg_function_4_env();
return tmp;
}

typedef struct {
	
} _kg_function_5_env;

_kg_function_5_env* _create__kg_function_5_env(){
return NULL;
}

Pair_int_bool* _kg_function_5 (int first, bool second, _kg_function_5_env* env){

return _create_Pair_int_bool(first, second);
}
KiloFunction* create__kg_function_5(){
KiloFunction* tmp = function_create(_kg_function_5);
tmp->env = _create__kg_function_5_env();
return tmp;
}

typedef struct {
	
} _kg_function_6_env;

_kg_function_6_env* _create__kg_function_6_env(){
return NULL;
}

Pair_int_float* _kg_function_6 (Pair_int_float* first, Pair_int_float* second, _kg_function_6_env* env){

return _create_Pair_int_float(first->first, second->second);
}
KiloFunction* create__kg_function_6(){
KiloFunction* tmp = function_create(_kg_function_6);
tmp->env = _create__kg_function_6_env();
return tmp;
}

typedef struct {
	
} _kg_function_8_env;

_kg_function_8_env* _create__kg_function_8_env(){
return NULL;
}

int _kg_function_8 (int inner_value, _kg_function_8_env* env){

return inner_value*inner_value;
}
KiloFunction* create__kg_function_8(){
KiloFunction* tmp = function_create(_kg_function_8);
tmp->env = _create__kg_function_8_env();
return tmp;
}

typedef struct {
	
} _kg_function_7_env;

_kg_function_7_env* _create__kg_function_7_env(){
return NULL;
}

int _kg_function_7 (int value, _kg_function_7_env* env){

KiloFunction* square = create__kg_function_8();
return ((int (*)(int, void*)) square->body)(value, square->env);
}
KiloFunction* create__kg_function_7(){
KiloFunction* tmp = function_create(_kg_function_7);
tmp->env = _create__kg_function_7_env();
return tmp;
}


// Program
int main(int argc, char** argv){
KiloFunction* make_pair_int_int = create__kg_function_0();
KiloFunction* make_pair_int_float = create__kg_function_1();
KiloFunction* make_pair_float_string = create__kg_function_2();
KiloFunction* make_pair_string_int = create__kg_function_3();
KiloFunction* make_pair_bool_string = create__kg_function_4();
KiloFunction* make_pair_int_bool = create__kg_function_5();
KiloFunction* combine_pairs_int_float = create__kg_function_6();
KiloFunction* pipeline_int = create__kg_function_7();
Pair_int_int* temp_a = ((Pair_int_int* (*)(int, int, void*)) make_pair_int_int->body)(10, 20, make_pair_int_int->env);
Pair_int_float* temp_b = ((Pair_int_float* (*)(int, float, void*)) make_pair_int_float->body)(10, 3.14, make_pair_int_float->env);
Pair_float_string* temp_c = ((Pair_float_string* (*)(float, KiloString*, void*)) make_pair_float_string->body)(3.14, string_create("<--- this number"), make_pair_float_string->env);
Pair_string_int* temp_d = ((Pair_string_int* (*)(KiloString*, int, void*)) make_pair_string_int->body)(string_create("PI"), 20, make_pair_string_int->env);
Pair_bool_string* temp_e = ((Pair_bool_string* (*)(bool, KiloString*, void*)) make_pair_bool_string->body)(true, string_create("TRUE"), make_pair_bool_string->env);
Pair_int_bool* temp_f = ((Pair_int_bool* (*)(int, bool, void*)) make_pair_int_bool->body)(10, false, make_pair_int_bool->env);
Pair_int_float* before_combination = ((Pair_int_float* (*)(int, float, void*)) make_pair_int_float->body)(59, 2.14, make_pair_int_float->env);
Pair_int_float* combined = ((Pair_int_float* (*)(Pair_int_float*, Pair_int_float*, void*)) combine_pairs_int_float->body)(temp_b, before_combination, combine_pairs_int_float->env);
return print(int_to_string(((int (*)(int, void*)) pipeline_int->body)(11, pipeline_int->env)));
}