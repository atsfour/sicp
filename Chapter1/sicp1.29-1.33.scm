(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; 実験用に使う関数たち
(define (identity x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (prime? x)
  (= x (smallest-divisor x)))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (next x) (if (= x 2) 3 (+ x 2)))
    (define (divides? a b) (= (remainder b a) 0))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

(define (disjoint? a b)
  (= 1 (GCD a b)))

(define pi 3.14)

; Exercise 1.29
(define (simpson f a b n)
  (define dx (/ (- b a) n))
  (define (add-2dx x)
    (+ x (* 2 dx)))
  (define (term x)
    (+ (f x)
       (* 4 (f (+ x dx)))
       (f (+ x (* 2 dx))))
    )
  (* (sum term a add-2dx (- b dx))
     (/ dx 3.0)))

; Exercise 1.30
(define (sum-liner term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result ))))
  (iter a 0))

(define (sum-integers-liner a b)
  (sum-liner identity a inc b))

; Exercise 1.31
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1)
  )

(define (factorial x)
  (product identity 1 inc x))
;πの近似値を求める手続き
(define (prod-pi n)
  (define (term a)
    (/ (* a (+ a 2)) (* (+ a 1) (+ a 1))))
  (define (next a) (+ a 2))
  (* 4 (product term 2.0 next n))
  )

; Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

(define (sum-integers-acc a b)
  (sum-acc identity a inc b))

(define (fact-acc n)
  (prod-acc identity 1 inc n))

; Exercise 1.33
(define (filtered-accumulate combiner
                             null-value
                             term
                             a
                             next
                             b
                             filter)
  (define (iter a result)
    (display a)
    (display "  result = ")
    (display result)
    (newline)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))

(define (sum-square-prime a b)
  (filtered-accumulate +
                       0
                       square
                       a
                       inc
                       b
                       prime?))

(define (prod-disjoint-n n)
  (define (disjoint-n? a)
    (= 1 (GCD a n)))
  (filtered-accumulate *
                       1
                       identity
                       1
                       inc
                       (- n 1)
                       disjoint-n?))


(define a 3)
(define b 27)
(define n 2)
(define ans (string-append
              "a = "
              (x->string a)
              ", b = "
              (x->string b)
              ", n = "
              (x->string n)
              ", ans ="
              (x->string (prod-disjoint-n 15))))
(display ans)
(newline)