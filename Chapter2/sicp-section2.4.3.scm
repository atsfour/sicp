;definision of put and get
(define operation-table (make-hash-table 'equal?))

(define (put op type item)
  (if (not (hash-table-exists? operation-table op))
      (hash-table-put! operation-table op (make-hash-table 'equal?)))
  (let ((type-table (hash-table-get operation-table op)))
    (hash-table-put! type-table type item)))

(define (get op type)
  (if (hash-table-exists? operation-table op)
      (let ((type-table (hash-table-get operation-table op)))
        (hash-table-get type-table type))
      (error "NOT EXISTS" op type)))

;2.4.2 tagged data
(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: APPLY-GENERIC"
            (list op type-tags))))))

;2.4.3 data-directed programming

(define (square x) (* x x))

(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  (define this-type-tag 'rectangular)
  (define (tag x) (attach-tag this-type-tag x))
  (put 'real-part (list this-type-tag) real-part)
  (put 'imag-part (list this-type-tag) imag-part)
  (put 'magnitude (list this-type-tag) magnitude)
  (put 'angle (list this-type-tag) angle)
  (put 'make-from-real-imag this-type-tag
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang this-type-tag
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan (/ y x))))

  (define this-type-tag 'polar)
  (define (tag x) (attach-tag this-type-tag x))
  (put 'real-part (list this-type-tag) real-part)
  (put 'imag-part (list this-type-tag) imag-part)
  (put 'magnitude (list this-type-tag) magnitude)
  (put 'angle (list this-type-tag) angle)
  (put 'make-from-real-imag this-type-tag
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang this-type-tag
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))