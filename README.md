# lisexp
A REPL Lisp evaluator using Jane-Street's S-Expression library in OCaml.

# Installation
```bash
git clone https://github.com/MustafaAamir/lisexp
cd lisexp
dune build
# opam install parsexp sexplib core base dune
dnf install rlwrap
rlwrap -f completion dune exec sexp
```

`rlwrap -f completion` is optional for auto-completion and a better repl experience

### Grammar

Operators:
```
( + ) → addition
( - ) → subtraction
( * ) → multiplication
( / ) → division
( ^ ) → exponentiation

( <> ) → NEq
( <= ) → LTE
( >= ) → GTE
( =  ) → Eq
```

Expressions:
```
λ> (1)
=> 1

λ> 1
Not a valid expression. All expressions must be wrapped in parentheses

λ> (+ 1 1)
=> 2

λ> (define x 42)
=> 42

λ> (define x (+ (^ 3 2) 1))
=> 10
```

Control flow:
```
λ> (if (<expression>) <true block> <false block>)

λ> (if (= 1 1) 1 0)
=> 1

λ> (if (<> 1 1) 1 0)
=> 0
```

Lambda functions:
```
λ> (define <identifier> (lambda (<parameters>) (<body>)))
```

Recursion:
```
λ> (define fact ($ (n) (if (= n 0) 1 (* n (fact (- n 1))))))
=> <function λ>

λ> (factorial 20)
=> 2432902008176640000

λ> (define fib ($ (n) (if (<= n 1) n (+ (fib(- n 1)) (fib(- n 2))))))
=> <function λ>

λ> (fib 10)
=> 55
```

## Data Structures:
- Integers: Whole numbers (e.g., 42)
- Floats: Real numbers (e.g., 3.14)
- Symbols: Unquoted strings representing variables or functions (e.g., x, define)
- Strings: Quoted strings (e.g., "hello world")
- Booleans: True or false values (true, false)
- Lists: Ordered collections of other values enclosed in parentheses (e.g., (1 2 3), (x y "hello"))
- Functions: Anonymous functions defined using the \$ keyword (e.g., (\$ (x) (+ x 1)))
- List: The first element is treated as a function, and the remaining elements are evaluated as arguments. The function is then applied to the evaluated arguments.
- if Expression: Evaluate the condition (first element). If true, evaluate the second element (then-expr); otherwise, evaluate the third element (else-expr).
- define expression: Bind the second element (symbol) to the evaluated value of the third element (expr) within the environment.
- Anonymous functions: Create a function object with the parameters (list following $) and the body expression.

# Built-in Functions
- Arithmetic: +, -, *, /, ^
- Comparison: >, <, >=, <=, = (for numbers and strings)
- Type Predicates: integer?, float?, symbol?, string?, list?, nil?
- List Operations: cons (conses a new list), car (returns the first element), cdr (returns the rest of the list after the first element), append (alias @) (concatenates lists or an element to a list), length (returns the length of a list), map, filter, foldl, foldr, sort_asc, sort_desc, get (retrieves an element by index), set (modifies an element at a specific index)

# Commands
Environment ':env <optional>': access the current environment using :env and optionally specify a variable name to retrieve its value.
Signatures for built-in functions `:signature <optional>`: display the type signatures using :signature and optionally specify a function name to view its associated types.
Quit: `:wq`
Help: `:help`

# TODO:
1. ~Fix inane type error in `add_cmp`~
2. Infinite repl when multi-line is triggered. Paren-equality isn't enough
3. function signatures and recursive signatures
4. Finish implementation of lists (should I use cons n ??)
5. <then> block in if statement doesn't work w parens
6. write unit tests using ounit2
7. Delete disgusting parser hack to return "\"" ^ symbol ^ "\""

