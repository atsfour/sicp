; 汎用関数
(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2.0))

; Exercise 1.34
(define (f g) (g 2))

;fixed-point書き写し
(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (display "guess = ")
        (display guess)
        (newline)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(define (average-damp f) (lambda (x) (average x (f x))))
(define (fixed-point-ad f first-guess)
    (fixed-point (average-damp f) first-guess))

;Exercise 1.35
(define golden-ratio-fun (lambda (x) (+ 1 (/ 1.0 x))))

;Exercise 1.36
(define (x-pow-x-is-n n)
    (fixed-point (lambda (x) (/ (log n) (log x))) 2.0))
    

(display (x-pow-x-is-n 1000))
(newline)