;prepareing
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      ()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap f seq)
  (accumulate append () (map f seq)))

(define (make-sum-pair p)
  (if (null? p)
      p
      (list (car p) (cadr p) (+ (car p) (cadr p)))))

(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

; difinision of prime
(define (prime? x)
  (= x (smallest-divisor x)))
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (define (next x) (if (= x 2) 3 (+ x 2)))
    (define (divides? a b) (= (remainder b a) 0))
    (define (square x) (* x x))
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor n 2))

;Exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (reverse (enumerate-interval 1 (- i 1)))))
           (reverse (enumerate-interval 1 n))))

(define (prime-sum-pairs n)
  (filter (lambda (x) (prime? (caddr x)))
          (map make-sum-pair (unique-pairs n))))

;Exercise 2.41
(define (unique-n-pairs n num)
  (if (= n 1)
      (map (lambda (x) (list x)) (reverse (enumerate-interval 1 num)))
      (flatmap (lambda (i) (map (lambda (j) (cons i j))
                                (unique-n-pairs (- n 1) (- i 1))))
               (reverse (enumerate-interval 1 num)))))

(define (split-to-n-pairs s n upper-bound)
  (filter (lambda (x) (= s (accumulate + 0 x)))
          (unique-n-pairs n upper-bound)))
