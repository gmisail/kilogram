# kilogram

Functional programming language inspired by OCaml and Lua. For high performance and maximum portability, it compiles
into C11 and does not use any external dependencies.

### Example

```lua
enum Sequence['T]
    None,
    Cons('T, Sequence['T])
end

record Pair['T, 'S]
    first: 'T,
    second: 'S
end

let rec length = function(seq: Sequence): int
    case seq of 
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
let user = Pair[string, int] { first: "gmisail", second: 22 }

let list = Sequence[int].Cons(
    0, 
    Sequence[int].Cons(
        1, 
        Sequence[int].Nil
    )
)

print(int_to_string(factorial(iter)))
```

## Goals

- Simplicity without sacrificing correctness
- Good (enough) performance
- Support for all major platforms (macOS, Linux, Windows)
- Seamless interoperability with C

## Installation

**TODO**
