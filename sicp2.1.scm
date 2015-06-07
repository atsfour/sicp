(define (make-rat n d)
    (if (< d 0)
        (make-rat (- n) (- d))
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g)))))
        
(define (denom x) (cdr x))
(define (numer x) (car x))

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x))
    (newline))

(define (gcd a b)
    (if (= (remainder a b) 0)
        (abs b)
        (gcd b (remainder a b))))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
              
(print-rat (add-rat (make-rat -16 5) (make-rat 3 -2)))
(print-rat (make-rat 6 -3))