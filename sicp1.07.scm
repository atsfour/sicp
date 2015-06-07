(define (abx x)
    if (< x 0) (- x) x)

(define (square x) (* x x))

(define (new-if pred then-clause else-clause)
    (cond (pred then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
    (print guess)
    (if (good-enough guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2.0))

(define (good-enough guess x )
    (< (abs (- 1 (/ (square guess) x))) 0.0001))

(sqrt-iter 1.0 3000)