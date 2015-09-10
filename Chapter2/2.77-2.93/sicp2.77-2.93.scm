;;install packages

(use file.util)
(add-load-path "." :relative)
(load "operation-table.scm")
(load "attach-tags.scm")
(load "type-coercions.scm")
(load "package-integer.scm")
(load "package-rational.scm")
(load "package-real-number.scm")
(load "package-complex-number.scm")
(load "package-scheme-number.scm")
(load "package-polynomial.scm")
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-scheme-number-package)
(install-polynomial-package)

;apply generic
(define (apply-generic op . args)
  (define (try-apply op args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            '() ))))
  (define (try-raise op args)
    (if (every can-raise? args)
        (let ((raised-args (map raise-type args)))
          (let ((ans (try-apply op raised-args)))
            (if (not (null? ans))
                ans
                (try-raise op raised-args))))
        '() ))
  (define (try-coerce op args)
    (if (can-coerce? args)
        (let ((raised-args
                (map (lambda (x) (raise-to x (highest-type args)))
                     args)))
          (let ((ans (try-apply op raised-args)))
            (if (not (null? ans))
                ans
                (try-raise op raised-args))))
        '()))
  (define (result)
    (let ((res1 (try-apply op args)))
      (if (not (null? res1))
          res1
          (let ((res2 (try-coerce op args)))
            (if (not (null? res2))
                res2
                (error "No method for these types: APPLY-GENERIC"
                       op args))))))
  (drop (result)))


;generic operations

(define (generic-square x) (apply-generic 'generic-square x))
(define (generic-sqrt x) (apply-generic 'generic-sqrt x))
(define (generic-sin x) (apply-generic 'generic-sin x))
(define (generic-cos x) (apply-generic 'generic-cos x))
(define (generic-atan x) (apply-generic 'generic-atan x))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (minus x) (apply-generic 'minus x))

(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

;complex package
;Exercise 2.77
;(magnitude (make-complex-from-real-imag 3 4))
;
;(apply-generic 'magnitude (make-complex-from-real-imag 3 4))
;
;(apply-generic 'magnitude
;               ((get 'make-from-real-imag 'rectangular) 3 4))
;
;(apply-generic 'magnitude
;               (cons 'complex
;                     (cons 'rectangular (cons 3 4))))
;apply-generic 1

;Exercise 2.79
(define (my-equ? x y) (apply-generic 'my-equ? x y))

;Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;Exercise 2.83
(define (raise-type x)
  (if (can-raise? x)
      ((get 'raise-type (type-tag x)) (contents x))
      #f))

;Exercise 2.85
(define (project x)
  (let ((proc (get 'project (type-tag x))))
    (if proc
        (apply proc (list (contents x)))
        #f)))

(define (drop x)
  (let ((projected (project x)))
    (if projected
        (if (my-equ? x (raise-type projected))
            (drop projected)
            x)
        x)))