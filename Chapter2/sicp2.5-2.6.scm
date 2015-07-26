(define (?even x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

(define (pow x y)
  (cond ((= y 0) 1)
        ((?even y) (square (pow x (/ y 2))))
        (else (* x (pow x (- y 1))))))

; Exercise 2.5

(define (cons x y)
  (* (pow 2 x) (pow 3 y)))
