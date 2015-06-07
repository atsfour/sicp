(define (rec-f n)
    (if (< n 3) n
        (+ (rec-f (- n 1)) (* 2 (rec-f (- n 2))) (* 3 (rec-f (- n 3))))
        ))

(define (f n)
    (define (iter acc1 acc2 acc3 cnt)
        (define ans (+ acc1 (* 2 acc2) (* 3 acc3 ))
        )
        (if (= cnt n)
            ans
            (iter ans acc1 acc2 (+ cnt 1))
        )
    )
    (if (< n 3)
        n 
        (iter 2 1 0 3)
    )
)
(define (test n)
    (print "f-rec(" n ")=" (rec-f n))
    (print "    f(" n ")=" (f n))
)
(test 10)