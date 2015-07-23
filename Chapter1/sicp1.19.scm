(define (double x) (+ x x))
(define (halve n) (if (= (remainder n 2) 0) (/ n 2) "error"))

(define (expt x n)
    (define (even? m) (= (remainder m 2) 0 ))
    (define (expt-iter a b n)
        ;(print a " " b " " n)
        (if (= n 0)
            a
            (if (even? n)
                (expt-iter a (* b b) (/ n 2))
                (expt-iter (* a b) b (- n 1))
            )
        )
    )
    (expt-iter 1 x n)
)

(define (fib n)
    (define (fib-iter a b p q count)
    (print a " " b " " p " " q " " count)
        (cond 
            ((= count 0)
                b
            )
            ((even? count)
                (fib-iter 
                    a
                    b
                    (+ (* p p) (* q q))
                    (* q (+ (* 2 p) q))
                    (/ count 2)
                )
            )
            (else
                (fib-iter
                    (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- count 1)
                )
            )
        )
    )
    (fib-iter 1 0 0 1 n)
)

(print (fib 10))