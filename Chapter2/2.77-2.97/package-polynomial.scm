(define (install-dence-polynomial-package)

  (define (add-dence-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((adjusuted-terms (adjusut-terms L1 L2)))
              (let ((new-L1 (car adjusuted-terms))
                    (new-L2 (cadr adjusuted-terms)))
                (add-element-lists new-L1 new-L2))))))
  (define (adjusut-terms L1 L2)
    (let ((diff-length (- (length L1) (length L2))))
      (if (= diff-length 0)
          (list L1 L2)
          (if (> diff-length 0)
              (list L1 (append (make-list diff-length 0) L2))
              (list (append (make-list (- diff-length) 0) L1) L2)))))
  (define (add-element-lists L1 L2)
    (define (iter acc L1 L2)
      (cond ((and (null? L1) (null? L2) (reverse acc)))
            ((null? L1) (iter (cons (car L2) acc) '() (cdr L2)))
            ((null? L2) (iter (cons (car L1) acc) (cdr L1) '()))
            (else (iter (cons (add (car L1) (car L2)) acc) (cdr L1) (cdr L2)))))
    (iter '() L1 L2))

  (define (minus-dence-terms L)
    (map (lambda (x) (minus x)) L))

  (define (mul-dence-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-dence-terms (mul-term-by-all-terms (first-term L1) L2)
                         (mul-dence-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (map (lambda (x) (mul (coeff t1) x))
             (append L (make-list (order t1) 0)))))

  (define (dence->sparce term-list)
    (define (iter order term-list)
      (cond ((null? term-list) '())
            ((=zero? (car term-list))
             (iter (- order 1) (cdr term-list)))
            (else (cons (list order (car term-list))
                        (iter (- order 1) (cdr term-list))))))
    (iter (- (length term-list) 1) term-list))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (list (- (length term-list) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (tag x) (attach-tag 'dence x))
  (put 'add-terms '(dence dence)
       (lambda (L1 L2) (tag (add-dence-terms L1 L2))))
  (put 'mul-terms '(dence dence)
       (lambda (L1 L2) (tag (mul-dence-terms L1 L2))))
  (put 'minus-terms '(dence)
       (lambda (term-list) (tag (minus-dence-terms term-list))))
  (put 'raise-type 'dence
       (lambda (term-list)
         (attach-tag 'sparce (dence->sparce term-list))))
  '(done dence poly))


(define (install-sparce-polynomial-package)
  (define (make-poly-sparce variable term-list)
    (cons variable term-list))

  (define (add-sparce-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-sparce-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-sparce-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-sparce-terms (rest-terms L1)
                                          (rest-terms L2)))))))))
  (define (minus-sparce-terms L)
    (map (lambda (t) (make-term (order t) (minus (coeff t))))
         L))

  (define (mul-sparce-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-sparce-terms (mul-term-by-all-terms (first-term L1) L2)
                          (mul-sparce-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-sparce-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                        (add-sparce-terms
                          L1
                          (minus-sparce-terms
                            (mul-term-by-all-terms
                              (make-term new-o new-c)
                              L2)))))
                  (let ((result (div-sparce-terms rest-of-result L2)))
                    (list (adjoin-term (make-term new-o new-c)
                                       (car result))
                          (cadr result)))))))))

  (define (remainder-sparce-terms L1 L2)
    (cadr (div-sparce-terms L1 L2)))

  (define (pseudo-remainder-sparce-terms L1 L2)
    (define (power int n)
      (if (= n 0)
          1
          (* int (power int (- n 1)))))
    (let ((integerizing-factor
            (power (coeff (first-term L2))
                   (+ 1 (order (first-term L1)) (minus (order (first-term L2)))))))
      (let ((rem (cadr (div-sparce-terms
                         (mul-coeff-by-number L1 integerizing-factor)
                         L2))))
        (div-coeff-by-number rem (gcd-coeff rem)))))

  (define (gcd-coeff L)
    (if (null? L)
        0
        (gcd (coeff (first-term L)) (gcd-coeff (cdr L)))))
  (define (mul-coeff-by-number L num)
    (map (lambda (t) (make-term (order t) (mul (coeff t) num))) L))
  (define (div-coeff-by-number L num)
    (map (lambda (t) (make-term (order t) (div (coeff t) num))) L))

  (define (gcd-sparce-terms L1 L2)
    (let ((rem (pseudo-remainder-sparce-terms L1 L2)))
      (if (empty-termlist? rem)
          (if (< (coeff (first-term L2)) 0)
              (mul-coeff-by-number L2 -1)
              L2)
          (gcd-sparce-terms L2 rem))))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))

  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list)
    (every (lambda (t) (my-equ? (coeff t) 0)) term-list))

  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (tag x) (attach-tag 'sparce x))
  (put 'add-terms '(sparce sparce)
       (lambda (L1 L2) (tag (add-sparce-terms L1 L2))))
  (put 'mul-terms '(sparce sparce)
       (lambda (L1 L2) (tag (mul-sparce-terms L1 L2))))
  (put 'div-terms '(sparce sparce)
       (lambda (L1 L2) (map tag (div-sparce-terms L1 L2))))
  (put 'gcd-terms '(sparce sparce)
       (lambda (L1 L2) (tag (gcd-sparce-terms L1 L2))))
  (put 'minus-terms '(sparce)
       (lambda (term-list) (tag (minus-sparce-terms term-list))))
  '(done sparce poly))


(define (install-polynomial-package)

  (define (make-poly variable term-list)
    (cons variable term-list))

  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (variable p) (apply-generic 'variable p))
  (define (term-list p) (apply-generic 'term-list p))

  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (equal? v1 v2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: add-poly"
               (list p1 p2))))
  (define (sub-poly p1 p2)
    (add-poly p1 (minus-poly p2)))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var: mul-poly"
               (list p1 p2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result-term-lists
                (div-terms (term-list p1) (term-list p2))))
          (map (lambda (t) (make-poly (variable p1) t))
               result-term-lists))
        (error "Polys not in same var: div-poly"
               (list p1 p2))))

  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: div-poly"
               (list p1 p2))))
  (define (quatient-poly p1 p2)
    (car (div-poly p1 p2)))

  (define (minus-poly p)
    (make-poly (variable p)
               (minus-terms (term-list p))))

  (define (add-terms L1 L2)
    (apply-generic 'add-terms L1 L2))

  (define (minus-terms L)
    (apply-generic 'minus-terms L))

  (define (mul-terms L1 L2)
    (apply-generic 'mul-terms L1 L2))

  (define (div-terms L1 L2)
    (apply-generic 'div-terms L1 L2))

  (define (gcd-terms L1 L2)
    (apply-generic 'gcd-terms L1 L2))

  (define (poly=zero? p)
    (define (term-list=zero? term-list)
      (print "try" term-list)
      (cond ((empty-termlist? term-list) #t)
            ((=zero? (coeff (first-term term-list)))
             (term-list=zero? (cdr term-list)))
            (else #f)))
    (term-list=zero? (term-list p)))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'quatient '(polynomial polynomial)
       (lambda (p1 p2) (tag (quatient-poly p1 p2))))
  (put 'minus '(polynomial)
       (lambda (p) (tag (minus-poly p))))
  (put 'make-poly-dence 'polynomial
       (lambda (var terms)
         (tag (make-poly var (attach-tag 'dence terms)))))
  (put 'make-poly-sparce 'polynomial
       (lambda (var terms)
         (tag (make-poly var (attach-tag 'sparce terms)))))
  (put '=zero? '(polynomial) poly=zero?)

  (install-dence-polynomial-package)
  (install-sparce-polynomial-package)
  'done)

(define (make-polynomial-dence var terms)
  ((get 'make-poly-dence 'polynomial) var terms))
(define (make-polynomial-sparce var terms)
  ((get 'make-poly-sparce 'polynomial) var terms))
(define (polynomial? x)
  (equal? (type-tag x) 'polynomial))