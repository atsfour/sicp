(define (memq item x)
  (cond ((null? item) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;Exercise 2.54
(define (new-equal? a b)
  (cond ((not (pair? a)) (eq? a b))
        (else (and (new-equal? (car a) (car b))
                   (new-equal? (cdr a) (cdr b))))))