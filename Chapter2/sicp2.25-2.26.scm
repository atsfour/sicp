;Exercise 2.25

(define list1 (list 1 3 (list 5 7) 9))
(define list2 (list (list 7)))
(define list3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (solution1 x)
  (car (cdr (car (cdr (cdr x))))))
(define (solution2 x)
  (car (car x)))
(define (solution3 x)
  (car (cdr (cadr (cadr (cadr (cadr (cadr x))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

;(append x y) = (1 2 3 4 5 6)
;(cons x y) = ((1 2 3) 4 5 6)
;(list x y) = ((1 2 3) (4 5 6))