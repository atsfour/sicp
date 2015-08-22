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

;Exercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;Exercise 2.73 b
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (addend s) (car s))
(define (augend s)
  (if (null? (cddr s))
      (cadr s)
      (cons '+ (cdr s))))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else
          (list '+ a1 a2))))
(define (deriv-sum exp var)
  (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var)))

(define (multiplier p) (car p))
(define (multiplicand m)
  (if (null? (cddr m))
      (cadr m)
      (cons '* (cdr m))))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (deriv-product exp var)
  (make-sum (make-product (deriv (multiplier exp) var)
                          (multiplicand exp))
            (make-product (multiplier exp)
                          (deriv (multiplicand exp) var))))

;Exercise 2.73 c
(define (base e) (car e))
(define (exponent e) (cadr e))
(define (make-exponention base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        (else
          (list '** base exponent))))
(define (deriv-exponention exp var)
  (make-product (exponent exp)
                (make-product (deriv (base exp) var)
                              (make-exponention (base exp)
                                                (- (exponent exp) 1)))))

;installs
(put 'deriv '+ deriv-sum)
(put 'deriv '* deriv-product)
(put 'deriv '** deriv-exponention)