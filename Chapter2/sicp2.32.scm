;Exercise 2.32
(define (subset s)
  (if (null? s)
      (list ())
      (let ((rest (subset (cdr s))))
        (append (map (lambda (x) (cons (car s) x)) rest) rest))))

(define sample-list (list 1 2 3 4))