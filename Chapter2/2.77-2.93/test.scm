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
(test** 1 (greatest-common-divisor int1 int2))
(test** '(rational 2 . 3) rat1)
(test** (make-rational 10 21) (mul rat1 rat2))
(test** (make-integer 2) (mul rat1 int1))
(test** (make-rational 3 1) (raise-type int1))
(test** (make-rational 91 5) (mul real1 int2))
(test** 4 (sub real1 (make-real 0.55)))
(test** (make-complex-from-real-imag 4 2)
        (add cmpx1 int1))
(test** (make-real (sin 1)) (imag-part cmpx3))

(test-section "type-tagging")
(test** 'integer (type-tag 1))
(test** 'integer (type-tag (make-integer 1)))
(test** 'complex (type-tag cmpx3))
(test** 'polar (type-tag (contents cmpx3)))
(test** '(real . 1) (attach-tag 'real 1))
(test** 1 (attach-tag 'integer 1))

(test-section "polynomials")
(define p1 (make-polynomial-dence 'x '(1 -1)))
(define p2 (make-polynomial-dence 'x (list 1 0 0 (make-rational -1 1))))
(define p3 (make-polynomial-sparce 'x '((3 3) (2 -1) (1 1) (0 -3))))

(test** #t (polynomial? p1))
(test** '(polynomial x sparce (0 0)) (make-polynomial-sparce 'x '((0 0))))
(test** (make-polynomial-dence 'x '(1 0 1 -2)) (add p1 p2))
(test** (make-polynomial-dence 'x '(1 -1 0 -1 1)) (mul p1 p2))
(test** (make-polynomial-sparce 'x '((6 3) (5 -1) (4 1) (3 -6) (2 1) (1 -1) (0 3)))
        (mul p2 p3))
(test** (list (make-polynomial-sparce 'x '((2 3) (1 2) (0 3)))
              (make-polynomial-sparce 'x '()))
        (div p3 p1))

(test-section "rational plynomials")
(define p4 (make-polynomial-sparce 'x '((3 1) (0 1))))
(define p5 (make-polynomial-sparce 'x '((2 1) (0 1))))
(define rf (make-rational p4 p5))

(test** (make-rational (make-polynomial-sparce 'x '((3 2) (0 2)))
                       (make-polynomial-sparce 'x '((2 1) (0 1))))
        (add rf rf))
(test** (make-polynomial-sparce 'x '((2 1) (1 -1)))
        (greatest-common-divisor
          (make-polynomial-sparce 'x '((4 1) (3 -1) (2 -2) (1 2)))
          (make-polynomial-sparce 'x '((3 1) (1 -1)))))

(test-section "sicp-2.95")
(define p6 (make-polynomial-sparce 'x '((2 1) (1 -2) (0 1))))
(define p7 (make-polynomial-sparce 'x '((2 11) (0 7))))
(define p8 (make-polynomial-sparce 'x '((1 13) (0 5))))

(test** p6 (greatest-common-divisor (mul p6 p7)
                                    (mul p6 p8)))


(test-end)