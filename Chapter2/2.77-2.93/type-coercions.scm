
;Exercise 2.84
(define type-hyerarchy-table
  (hash-table 'equal?
              '(integer number . 1)
              '(rational number . 2)
              '(real number . 3)
              '(complex number . 4)
              '(dence poly-terms . 1)
              '(sparce poly-terms . 2)))

(define (group tag)
  (if (hash-table-exists? type-hyerarchy-table tag)
      (car (hash-table-get type-hyerarchy-table tag))
      #f))

(define (height tag)
  (if (hash-table-exists? type-hyerarchy-table tag)
      (cdr (hash-table-get type-hyerarchy-table tag))
      #f))

(define (highest-type args)
  (define (highest-type-iter args highest type)
    (if (null? args)
        type
        (let ((this-type (type-tag (car args))))
          (let ((this-height (height this-type)))
            (if (> this-height highest)
                (highest-type-iter (cdr args) this-height this-type)
                (highest-type-iter (cdr args) highest type))))))
  (highest-type-iter args 0 'miss))

(define (raise-to obj type)
  (if (equal? (type-tag obj) type)
      obj
      (raise-to (raise obj) type)))

(define (can-coerce? list)
  (let ((first-group (group (type-tag (car list)))))
    (every (lambda (obj) 
             (and (hash-table-exists? 
                    type-hyerarchy-table 
                    (type-tag obj))
                  (equal? (group (type-tag obj)) first-group)))
             list)))

(define (can-raise? x)
  (get 'raise (type-tag x)))
