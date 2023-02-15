# kilogram

Functional programming language inspired by OCaml and Lua. 

```lua
let rec factorial = function(x: int): int 
	if x == 0 then 1
	else x * factorial(x - 1)
end

let iter = 10

print(int_to_string(factorial(iter)))
```
