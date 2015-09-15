;definision of put and get
(define operation-table (make-hash-table 'equal?))

(define (put op type item)
  (if (not (hash-table-exists? operation-table op))
      (hash-table-put! operation-table op (make-hash-table 'equal?)))
  (let ((type-table (hash-table-get operation-table op)))
    (hash-table-put! type-table type item)))

(define (get op type)
  (if (hash-table-exists? operation-table op)
      (let ((type-table (hash-table-get operation-table op #f)))
        (hash-table-get type-table type #f))
      (error "NOT EXISTS" op type)))
