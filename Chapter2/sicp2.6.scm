(define church-0 (lambda (f) (lambda (x) x)))
(define church-1 (lambda (f) (lambda (x) (f x))))
(define church-2 (lambda (f) (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define (display-church c)
  ((c inc) 0))

(define (inc n)
  (+ n 1))

(define (disp1 sign)
  (display sign)
  sign)
