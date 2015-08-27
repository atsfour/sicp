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

;2.4.2 tagged data new version at Exercise 2.78
(define (old-attach-tag type-tag contents)
  (cons type-tag contents))
(define (old-type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (old-contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;apply generic
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (print "try type coercion from " type1 " to " type2)
                  (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                        (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                        (else (error
                                "No method for these types"
                                (list op type-tags))))))
              (error
                "No method for these types: APPLY-GENERIC"
                (list op type-tags)))))))

;generic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;scheme number package
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'my-equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  '(done scheme-number))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;rational package
(define (install-rational-package)
  (define (denom x) (cdr x))
  (define (numer x) (car x))
  (define (gcd a b)
    (if (= (remainder a b) 0)
        (abs b)
        (gcd b (remainder a b))))
  (define (make-rat n d)
    (if (< d 0)
        (make-rat (- n) (- d))
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (denom x) (numer y)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (my-equ? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero? x) (= (numer x) 0))
  (define (raise x) (make-real (/ (numer x) (denom x))))
  
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'my-equ? '(rational rational) my-equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) raise)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  '(done rational))

(define (make-rational n d)
  ((get 'make 'rational) n d))

;complex package

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
  '(done rectangular))

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
  '(done polar))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  (define (my-equ? z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (= (real-part z) 0) (= (imag-part z) 0)))
  
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (add-complex z1 z2)))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (sub-complex z1 z2)))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (mul-complex z1 z2)))
  (put 'div '(complex complex)
       (lambda (z1 z2) (div-complex z1 z2)))
  (put 'my-equ? '(complex complex) my-equ?)
  (put '=zero? '(complex) =zero?)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (install-rectangular-package)
  (install-polar-package)
  '(done complex))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;Exercise 2.77
;(magnitude (make-complex-from-real-imag 3 4))
;
;(apply-generic 'magnitude (make-complex-from-real-imag 3 4))
;
;(apply-generic 'magnitude 
;               ((get 'make-from-real-imag 'rectangular) 3 4))
;
;(apply-generic 'magnitude 
;               (cons 'complex 
;                     (cons 'rectangular (cons 3 4))))
;apply-generic 1

;Exercise 2.78
(define (attach-tag type-tag contents)
  (if (equal? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))

;Exercise 2.79
(define (my-equ? x y) (apply-generic 'my-equ? x y))

;Exercise 2.80
(define (=zero? x) (apply-generic '=zero? x))

;section 2.5.2 coercions
(define coercion-table (make-hash-table 'equal?))

(define (put-coercion type1 type2 coercion-func)
  (if (not (hash-table-exists? coercion-table type1))
      (hash-table-put! coercion-table type1
                       (make-hash-table 'equal?)))
  (let ((coercion-result-table
          (hash-table-get coercion-table type1)))
    (hash-table-put! coercion-result-table
                     type2
                     coercion-func)))

(define (get-coercion type1 type2)
  (let ((coercion-result-table
          (hash-table-get coercion-table type1 #f)))
    (if (hash-table? coercion-result-table)
        (hash-table-get coercion-result-table type2 #f)
        #f)))

(define (install-type-coersion-package)
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  
  (put-coercion 'scheme-number
                'complex
                scheme-number->complex)
  'done)

;Exercise 2.83
;integer and real package (same as scheme number, only tagged)
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer)
       (lambda (x y) (tag (/ x y))))
  (put 'my-equ? '(integer integer) =)
  (put '=zero? '(integer)
       (lambda (x) (= x 0)))
  (put 'raise '(integer)
       (lambda (x) (make-rational x 1)))
  (put 'make 'integer (lambda (x) (tag x)))
  '(done integer))

(define (make-integer x)
  ((get 'make 'integer) x))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'my-equ? '(real real) =)
  (put '=zero? '(real)
       (lambda (x) (= x 0)))
  (put 'raise '(real)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'make 'real (lambda (x) (tag x)))
  '(done real))

(define (make-real x)
  ((get 'make 'real) x))

(define (raise x) (apply-generic 'raise x))

(define (install-all)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-complex-package)
  (install-type-coersion-package)
  )
