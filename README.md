# lisexp
A REPL Lisp evaluator using Jane-Street's S-Expression library in OCaml.

# Grammar

Operators:
```
| Operator | Function | Usage |
| ------------- | ------------- | ----- |
| + | Addition | `(+ x y)` |
| - | Subtraction | `(- x y)` |
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
λ> (define factorial (lambda (n) (if (= n 0) (1) (* n factorial (- n 1)))))
=> <function λ>

λ> (factorial 20)
=> 2432902008176640000

λ> (define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ fib(n - 1) + fib(n - 2))))))
=> <function λ>

λ> (fib 10)
=> 55
```


# TODO:
Fix inane type error in `add_cmp`

