# lisexp
A REPL Lisp evaluator using Jane-Street's S-Expression library in OCaml.

# Installation
```bash
git clone https://github.com/MustafaAamir/lisexp
cd lisexp
dune build
# opam install parsexp sexplib core base dune
sudo dnf install rlwrap
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
λ> (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
=> <function λ>

λ> (factorial 20)
=> 2432902008176640000

λ> (define fib (lambda (n) (if (<= n 1) n (+ (fib(- n 1)) (fib(- n 2))))))
=> <function λ>

λ> (fib 10)
=> 55
```


# TODO:
1. ~Fix inane type error in `add_cmp`~
2. Infinite repl when multi-line is triggered. Paren-equality isn't enough
3. function signatures and recursive signatures
4. Finish implementation of lists (should I use cons n ??)
5. <then> block in if statement doesn't work w parens
6. write unit tests using ounit2

