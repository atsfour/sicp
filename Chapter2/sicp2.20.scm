;Exercise 2.20
(define (even? x)
  (= (remainder x 2) 0))

(define (my-filter f x)
  (if (null? x)
      `()
      (if (f (car x))
          (cons (car x) (my-filter f (cdr x)))
          (my-filter f (cdr x)))))

(define (same-parity n . x)
  (if (even? n)
      (cons n (my-filter even? x))
      (cons n (my-filter (lambda (x) (not (even? x))) x))))