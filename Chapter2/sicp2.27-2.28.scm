;Exercise 2.27
(define (deep-reverse x)
  (if (pair? x)
	  (append (deep-reverse (cdr x)) (list (deep-reverse (car x))))
	  x))

;Exercise 2.28
(define (fringe x)
  (cond ((null? x) x)
		((pair? x)
		 (append (fringe (car x)) (fringe (cdr x))))
		(else (list x))))

(define sample-1 (list (list 1 2) 3))
(define sample-2 (list 1 (list 2 3 (list 4 5))))
;((7 6) 5 4)