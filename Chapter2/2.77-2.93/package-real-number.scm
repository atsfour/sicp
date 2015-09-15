
(define (install-real-package)
  (define (square x) (* x x))
  (define (tag x) (attach-tag 'real x))
  (define (project x)
    (make-rational (floor->exact (* x 1000)) 1000))

  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'minus '(real) (lambda (x) (tag (- x))))
  (put 'my-equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise-type 'real
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project 'real project)
  (put 'generic-square '(real) (lambda (x) (tag (square x))))
  (put 'generic-sqrt '(real) (lambda (x) (tag (sqrt x))))
  (put 'generic-sin '(real) (lambda (x) (tag (sin x))))
  (put 'generic-cos '(real) (lambda (x) (tag (cos x))))
  (put 'generic-atan '(real) (lambda (x) (tag (atan x))))
  (put 'make 'real (lambda (x) (tag x)))
  '(done real))

(define (make-real x)
  ((get 'make 'real) x))
