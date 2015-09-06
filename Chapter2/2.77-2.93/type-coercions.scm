
;Exercise 2.84
(define type-hyerarchy-table
  (hash-table 'equal?
              '(integer . 1)
              '(rational . 2)
              '(real . 3)
              '(complex . 4)))

(define (highest-type args)
  (define (highest-type-iter args height type)
    (if (null? args)
        type
        (let ((this-type (type-tag (car args))))
          (let ((this-height
                  (hash-table-get type-hyerarchy-table this-type 0)))
            (if (> this-height height)
                (highest-type-iter (cdr args) this-height this-type)
                (highest-type-iter (cdr args) height type))))))
  (highest-type-iter args 0 'miss))

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

(define (can-raise? x)
  (get 'raise (type-tag x)))
