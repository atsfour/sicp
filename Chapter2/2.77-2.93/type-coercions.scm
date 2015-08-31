
;Exercise 2.84
(define type-hyerarchy-table
  (hash-table 'equal?
              '(integer . 1)
              '(rational . 2)
              '(real . 3)
              '(complex . 4)))

(define (highest-type type-list)
  (define (highest-type-iter l height type)
    (if (null? l)
        type
        (let ((this-type (car l))
              (this-height
                (hash-table-get type-hyerarchy-table (car l) 0)))
          (if (> this-height height)
              (highest-type-iter (cdr l) this-height this-type)
              (highest-type-iter (cdr l) height type)))))
  (highest-type-iter type-list 0 'null))

(define (raise-to obj type)
  (if (equal? (type-tag obj) type)
      obj
      (raise-to (raise obj) type)))

(define (can-coerce? list)
  (cond ((null? list) #t)
        ((hash-table-exists? type-hyerarchy-table
                             (type-tag (car list)))
         (can-coerce? (cdr list)))
        (else #f)))
