;ランダム値を扱うライブラリをインポート
(use srfi-27)
(use gauche.time)

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

(define (prime? n) (= (smallest-divisor n) n))

(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (next x) (if (= x 2) 3 (+ x 2)))

(define (even? x) (= (remainder x 2) 0))

(define (expmod base exp m)
;baseのexp乗をmで割った余りを求める
    (cond ((= exp 0)
           1)
          ((even? exp)
           (remainder
            (square
             (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base
               (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
    (define (try-it a)
        ;aのn乗をnで割った余りが再びaならnは素数の可能性が高い
        (= (expmod a n n) a))
    (define test-num
        ;random-integerは0からn-1までのランダム値を返す。
        ;1からnまでが欲しい場合はrandam-integerにn-1を渡して1を足す
        (+ 1 (random-integer (- n 1))))
    ;(print test-num)
    (try-it test-num))

(define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))

(define (timed-test sub-routine)
    (let1 t (make <real-time-counter>)
    (with-time-counter t sub-routine)
    (format #t "elspased time: ~a~%" (time-counter-value t))))

(define (search-for-primes lower-bound upper-bound num-primes)
    (cond ((= num-primes 0) #t)
          ((> lower-bound upper-bound) #f)
          ((prime? lower-bound)
            (begin (print lower-bound)
                   (search-for-primes
                     (+ lower-bound 2) upper-bound (- num-primes 1))))
          (else
            (search-for-primes (+ lower-bound 2) upper-bound num-primes))
    )
)

(timed-test (print (search-for-primes 100001 110000 3)))
