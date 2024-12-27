(define p ($ (s e)
     (if (> s e)
       nil
       (cons s (p (+ s 1) e)))))

(define sv ($ (nlist)
     (if (nil? nlist)
       nil
       (cons
         (car nlist)
         (sv
           (filter
             ($ (x) (<> 0 (% x (car nlist))))
             (cdr nlist)))))))

(define primes
  ($ (n)
     (sv (p 2 n))))

