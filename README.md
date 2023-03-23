# kilogram

Functional programming language inspired by OCaml and Lua. For high performance and maximum portability, it compiles into C11 and does not use any external dependencies. 

```lua
enum IntList
    None,
    Cons(int, IntList)
end

let rec length(list: IntList): int
    case list of 
        Cons(num, next) -> 1 + length(next),
	Nil -> 0
    end
end

let rec factorial = function(x: int): int 
    if x == 0 then 
    	1
    else 
    	x * factorial(x - 1)
end

let iter = 10

print(int_to_string(factorial(iter)))
```
