# kilogram

Functional programming language inspired by OCaml and Lua. For high performance and maximum portability, it compiles into C11 and does not use any external dependencies. 

```lua
enum IntList
    None,
    Cons(int, IntList)
end

let rec make_list(initial_value: int, len: int): IntList
    if len == 0 then
    	None
    else
    	Cons(initial_value, make_list(inital_value, len - 1))
end

let rec factorial = function(x: int): int 
	if x == 0 then 1
	else x * factorial(x - 1)
end

let iter = 10

print(int_to_string(factorial(iter)))
```
