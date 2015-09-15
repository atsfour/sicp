; Exercise 2.7
(define (make-interval a b) (cons (min a b) (max a b)))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Exercise 2.8
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (lower-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exerxise 2.10
(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (raise (condition (<error> (message "divisor spans 0"))))
      (mul-interval
        x
        (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y))))))

; Exercise 2.11
(define (positive? x) (> (lower-bound x) 0))
(define (negative? x) (< (upper-bound x) 0))
(define (spans-0? x) (not (or (positive? x) (negative? x))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (cond ((positive? x)
         (cond ((positive? y) (make-interval p1 p4))
               ((negative? y) (make-interval p3 p2))
               (else (make-interval p3 p4))))
        ((negative? x)
         (cond ((positive? y) (make-interval p2 p3))
               ((negative? y) (make-interval p4 p1))
               (else (make-interval p2 p1))))
        (else
        (cond ((positive? y) (make-interval p2 p4))
              ((negative? y) (make-interval p3 p1))
              (else (make-interval (min p2 p3) (max p1 p4)))))
        )))

;Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

(define (make-center-percent c p)
  (make-center-width c (* c p 0.01)))

;Exercise 2.13
(define (width-percent i)
  (if (= (width i) 0)
      0
      (* (/ (width i) (center i)) 100)))

(define (approximate-mul-interval x y)
  (if (or (not (positive? x)) (not (positive? y)))
      (raise (condition (<error> (message "cannot use approximation if interval is not positive"))))
      (make-center-percent
        (* (center x) (center y))
        (+ (width-percent x) (width-percent y)))))

;Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
      one (add-interval (div-interval one r1)
                        (div-interval one r2)))))

; Examples
(define interval-1 (make-interval 1.0 3.0))
(define interval-2 (make-interval -5.0 -7.0))
(define interval-3 (make-interval -2.0 4.0))
(define interval-4 (make-interval -4.0 0))
(define narrow-interval-1 (make-center-percent 120 0.7))
(define narrow-interval-2 (make-center-percent 150 1.0))
