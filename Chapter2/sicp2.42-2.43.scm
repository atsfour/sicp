; prepareing
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

(define (for-all proc seq)
  (accumulate (lambda (x y) (and (proc x) y)) #t seq))

(define (length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

;Exercise 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define (safe? k positions)
  (let ((head (car positions))
        (tail (cdr positions)))
    (for-all (lambda (x) (and (not (= (cadr head) (cadr x)))
                              (not (= (abs (- (car head) (car x)))
                                      (abs (- (cadr head) (cadr x)))))))
             tail)))

(define (print-queens q)
  (for-each (lambda (x) (print x))
            (map (lambda (x) (map (lambda (y) (cadr y))
                                  x))
                 q)))

(define (print-queens2 q)
  (for-each (lambda (x) (print x))
            q))

(define (num-of-solution q)
  (length q))

;Exercise 2.43
(define (wrong-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row
                                      k
                                      rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))
