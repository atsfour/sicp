(define (?even x)
  (= 0 (remainder x 2)))

(define (square x)
  (* x x))

(define (pow x y)
  (cond ((= y 0) 1)
        ((?even y) (square (pow x (/ y 2))))
        (else (* x (pow x (- y 1))))))

(define (index-of z x)
  (define (index-of-iter z x acc)
    (if (= (remainder z x) 0)
        (index-of-iter (/ z x) x (+ acc 1))
        acc))
  (index-of-iter z x 0))

; Exercise 2.5

(define (cons x y)
  (* (pow 2 x) (pow 3 y)))

(define (car z)
  (index-of z 2))

(define (cdr z)
  (index-of z 3))
