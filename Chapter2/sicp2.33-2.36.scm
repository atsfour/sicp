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

(define sample-list1 (list 1 2 3 4 5))
