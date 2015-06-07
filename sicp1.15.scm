(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (abs x) (if (< x 0) (- x) x))

(define (sine angle)
    (define radian (* angle (/ 3.14159 180)))
    (if (< (abs radian) 0.01)
        radian
        (p (sine (/ angle 3.0)))
    )
)

(print (sine 90))