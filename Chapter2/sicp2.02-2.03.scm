;Exercise 2.2
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (average a b)
  (/ (+ a b) 2.0))
(define (midpoint p1 p2)
  (make-point
    (average (x-point p1) (x-point p2))
    (average (y-point p1) (y-point p2))))

(define (midpoint-segment seg)
  (midpoint (start-segment seg) (end-segment seg)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;Exercise 2.3
(define (make-rectangle corner1 corner2)
  ;input 2 point which make a diagonal line
  (define top-right
    (make-point
      (max (x-point corner1) (x-point corner2))
      (max (y-point corner1) (y-point corner2))))
  (define bottom-left
    (make-point
      (min (x-point corner1) (x-point corner2))
      (min (y-point corner1) (y-point corner2))))
  (cons top-right bottom-left))

(define (height-rectangle rect)
  (- (y-point (car rect)) (y-point (cdr rect))))
(define (width-rectangle rect)
  (- (x-point (car rect)) (x-point (cdr rect))))

(define (perimeter-rectangle rect)
  (* 2 (+ (height-rectangle rect) (width-rectangle rect))))
(define (area-rectangle rect)
  (* (height-rectangle rect) (width-rectangle rect)))

;Usage
(define p0 (make-point 0 0))
(define p1 (make-point 2 4))
(define p2 (make-point 2.5 6.7))
(define s1 (make-segment p0 p1))
(define s2 (make-segment p1 p2))
(define m1 (midpoint-segment s1))
(define rect1 (make-rectangle p0 p2))
