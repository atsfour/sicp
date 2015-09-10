(use gauche.test)

(test-start "sicp2.77-2.93")
(add-load-path "." :relative)
(load "sicp2.77-2.93.scm")

(define-syntax test**
  (syntax-rules ()
    ((_ a b) (test* (quote b) a b))))

(test-section "number-packages")

(define int1 (make-integer 3))
(define int2 (make-integer 4))
(define rat1 (make-rational 2 3))
(define rat2 (make-rational 5 7))
(define real1 (make-real 4.55))
(define cmpx1 (make-complex-from-real-imag 1 2))
(define cmpx2 (make-complex-from-real-imag rat1 real1))
(define cmpx3 (make-complex-from-mag-ang 1 1))

(test** 7 (add int1 int2))
(test** '(rational 2 . 3) rat1)
(test** (make-rational 10 21) (mul rat1 rat2))
(test** (make-integer 2) (mul rat1 int1))
(test** (make-rational 3 1) (raise-type int1))
(test** (make-rational 91 5) (mul real1 int2))
(test** 4 (sub real1 (make-real 0.55)))
(test** (make-complex-from-real-imag 4 2)
        (add cmpx1 int1))
(test** (generic-sin 1) (imag-part cmpx3))

(test-section "type-tagging")
(test** 'integer (type-tag 1))
(test** 'integer (type-tag (make-integer 1)))
(test** 'complex (type-tag cmpx3))
(test** 'polar (type-tag (contents cmpx3)))
(test** '(real . 1) (attach-tag 'real 1))
(test** 1 (attach-tag 'integer 1))

(test-section "polynomials")
(define p1 (make-polynomial-dence 'x '(1 -1)))
(define p2 (make-polynomial-dence 'x '(1 0 0 -1)))
(define p3 (make-polynomial-sparce 'x '((3 3) (2 -1) (1 1) (0 -3))))

(test** (make-polynomial-dence 'x '(1 0 1 -2)) (add p1 p2))
(test** (make-polynomial-dence 'x '(1 -1 0 -1 1)) (mul p1 p2))
(test** (list (make-polynomial-sparce 'x '((2 3) (1 -4) (0 -3)))
              (make-polynomial-sparce 'x '()))
        (div p3 p1))

(test-end)