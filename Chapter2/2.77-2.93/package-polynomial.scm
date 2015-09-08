(define (install-dence-polynomial-package)
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (adjoin-term term term-list)
    (define (convert-nth n obj list)
      (if (= n 0)
          (cons obj (cdr list))
          (cons (car list) (convert-nth (- n 1) obj (cdr list)))))
    (if (null? term-list)
        (cons (coeff term) (make-list (order term) 0))
        (cons (coeff term) term-list)))
  
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
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul-terms '(dence dence) 
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'raise 'dence
       (lambda (term-list)
         (attach-tag 'sparce (dence->sparce term-list))))
  '(done dence poly))

(define (install-sparce-polynomial-package)
  (define (make-poly-sparce variable term-list)
    (cons variable term-list))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                    (else
                      (adjoin-term
                        (make-term (order t1)
                                   (add (coeff t1) (coeff t2)))
                        (add-terms (rest-terms L1)
                                   (rest-terms L2)))))))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
            (make-term (+ (order t1) (order t2))
                       (mul (coeff t1) (coeff t2)))
            (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  (define (tag x) (attach-tag 'sparce x))
  (put 'add-terms '(sparce sparce) 
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'mul-terms '(sparce sparce) 
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
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
  (define (minus-poly p)
    (make-poly (variable p)
               (map (lambda (t) (make-term (order t)
                                           (minus (coeff t))))
                    (term-list p))))
  
  (define (add-terms L1 L2)
    (apply-generic 'add-terms L1 L2))
  
  (define (mul-terms L1 L2)
    (apply-generic 'mul-terms L1 L2))
  
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