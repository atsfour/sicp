;install packages

(use file.util)
(add-load-path "./Chapter2/2.77-2.93")
(require "operation-table.scm")
(require "attach-tags.scm")
(require "type-coercions.scm")
(require "package-integer.scm")
(require "package-rational.scm")
(require "package-real-number.scm")
(require "package-complex-number.scm")
(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)

;apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (can-coerce? args)
              (let ((type-to (highest-type type-tags)))
                (let ((raised-args
                        (map (lambda (x) (raise-to x type-to)) args)))
                  (let ((raised-type-tags
                          (map type-tag raised-args)))
                    (apply (get op raised-type-tags)
                           (map contents raised-args)))))
          (error "No method for these types: APPLY-GENERIC"
                 (list op raised-type-tags)))))))

;generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


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
(define (raise x) (apply-generic 'raise x))
