# lisexp
> REPL for a lisp dialect written in ocaml

## Installation
```bash
git clone https://github.com/MustafaAamir/lisexp
cd lisexp
opam switch create .
opam install stdio base dune
dune build
# opam install parsexp sexplib core base dune
dnf install rlwrap
rlwrap -f completion dune exec sexp
```

`rlwrap -f completion` is optional for auto-completion and a better repl experience

### Grammar

#### Boolean
```
real -> true
cap -> false
```

#### String
```
"<contents>"
```

## Expressions:
- Arithmetic: +, -, *, /, ^
- Comparison: >, <, >=, <=, = (for numbers and strings)
- Type Predicates: integer?, float?, symbol?, string?, list?, nil?

```
λ> (1)
=> Error: Not a function
# because 1 isn't a function. (+ 1 1) is a function call ( + ) (1) (1)

λ> 1
=> 1

λ> (+ 1 1)
=> 2

λ> (define x 42)
=> 42

λ> (define x (+ (^ 3 2) 1))
=> 10

λ> (integer? x)
=> real

λ> (string? x)
=> cap

λ> (bool? real)
=> real

λ> (integer? real)
=> cap

```



Control flow:
```
λ> (if (<expression>) <real block> <cap block>)

λ> (if (= 1 1) 1 0)
=> 1

λ> (if (<> 1 1) 1 0)
=> 0
```

Lambda functions:
```
λ> (define <identifier> ($ (<parameters>) (<body>)))
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

## Builtins list functions:
- List Operations: cons (conses a new list), car (returns the first element), cdr (returns the rest of the list after the first element), append (alias &) (concatenates lists or an element to a list), length (returns the length of a list), map, filter, foldl, foldr, sort_asc, sort_desc, get (retrieves an element by index), set (modifies an element at a specific index)

```
λ> (@ 1 2 3) # list constructor
=> (1 2 3)

λ> (list 1 2 3) # list constructor
=> (1 2 3)

λ> (define x (cons 1 (cons 2 (cons 3 nil))))
=> (1 2 3)

λ> (car x)
=> 1

λ> (cdr x)
=> (2 3)

λ> (append 4 x) # append 4 to the END of x
=> (1 2 3 4)

λ> (& 4 x) # append 4 to the END of x
=> (1 2 3 4)

λ> (append (4 5 6) x)
=> (1 2 3 4 5 6)

λ> (length x)
=> 3

λ> (map ($ (x) (+ x 1)) (@ 1 2 3))
=> (2 3 4)

λ> (filter ($ (x) (< x 10)) (@ 8 9 10 11 12))
=> (8 9)

# 0 + (1+(2+3))
λ> (foldl 0 (@ 1 2 3) ($ (x y) (+ x y))) #sum
=> 6

# 10 + (1+(2+3))
λ> (foldl 10 (@ 1 2 3) ($ (x y) (+ x y))) #sum + 10
=> 16

# ((1+2)+3) + 0
λ> (foldr (@ 1 2 3) 0 ($ (x y) (+ x y)))
=> 6

λ> (foldr (@ 1 2 3) 10 ($ (x y) (+ x y)))
=> 16

λ> (sort_asc (@ 3 2 1))
=> (1 2 3)

λ> (sort_desc (@ 1 2 3))
=> (3 2 1)

λ> (get 0 (@ 1 2 3)) # get 0th element from list
=> 1

λ> (set 0 (@ 1 2 3) 100) # set 0th element in list to 100
=> (100 2 3)

```

## Commands

Environment ':env <optional>': access the current environment using :env and optionally specify a variable name to retrieve its value.
Signatures for built-in functions `:signature <optional>`: display the type signatures using :signature and optionally specify a function name to view its associated types.
Quit: `:wq`
Help: `:help`

```
λ> :env
# prints all builtin AND user-defined functions and variables

λ> (define x "hi")
=> 'hi'

λ> :env x
=> x: 'hi'

λ> :signature
# prints type signatures for all builtin functions

λ> :signature foldl
=> type signature(s) for 'foldl':
'a -> list 'a -> ('a -> 'a) -> 'a

λ> :signature foldr
=> type signature(s) for 'foldr':
list 'a -> 'a -> ('a -> 'a) -> 'a

λ> :signature set
=> type signature(s) for 'set':
integer a -> list 'a -> 'a -> list 'a

λ> :help # prints help

λ> :wq #exit
```

# Examples

```lisp
;; List constructor from s to e inclusive
 λ> (define p ($ (s e)
... >      (if (> s e)
... >        nil
... >        (cons s (p (+ s 1) e)))))
=> <function λ>

;; Sieve of Eratosthenes
 λ> (define sv ($ (nlist)
... >      (if (nil? nlist)
... >        nil
... >        (cons
... >          (car nlist)
... >          (sv
... >            (filter
... >              ($ (x) (<> 0 (% x (car nlist))))
... >              (cdr nlist)))))))
=> <function λ>

;; main function
 λ> (define primes
... >   ($ (n)
... >      (sv (p 2 n))))
=> <function λ>

 λ> (primes 100)
=> (2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97)
```
---

### TODO:
1. ~Fix inane type error in `add_cmp`~
2. Infinite repl when multi-line is triggered. Paren-equality isn't enough
3. function signatures and recursive signatures
4. Finish implementation of lists (should I use cons n ??)
5. <then> block in if statement doesn't work w parens
6. write unit tests using ounit2
7. Delete disgusting parser hack to return "\"" ^ symbol ^ "\""
8. Implement Hindley-Milner Type System

