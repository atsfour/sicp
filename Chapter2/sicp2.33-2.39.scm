;Exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;Exercese 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;Exercise 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (cond ((null? x) 0)
                                     ((not (pair? x)) 1)
                                     (else (count-leaves x))))
                   t)))

;Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;Exersise 2.37
(define (dot-product v1 v2)
  (accumulate + 0 (map * v1 v2)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix-*-matrix m1 m2)
  (let ((cols (transpose m2)))
    (map (lambda (x) (matrix-*-vector cols x)) m1)))

;Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;Exercise 2.39
(define (reverse1 sequence)
  (fold-left (lambda (x y) (append x (list y))) () sequence))
(define (reverse2 sequence)
  (accumulate (lambda (x y) (append y (list x))) () sequence))

(define sample-list1 (list 1 2 3 4 5))
(define vector1 (list 1 2 3))
(define vector2 (list 4 5 6))
(define matrix1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define matrix2 (list (list 1 1 1) (list 1 1 1)))
(define matrix3 (list (list 1 2) (list 3 4) (list 5 6)))