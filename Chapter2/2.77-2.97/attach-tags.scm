;Exercise 2.78
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'integer)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'integer))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))
