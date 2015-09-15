; Exercise 2.21
(define (square x) (* x x))

(define (square-list-1 items)
	(if (null? items)
			`()
			(cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list items)
	(map square items))

;Exercise 2.22
(define (append list1 list2)
	(if (null? list1)
			list2
			(cons (car list1) (append (cdr list1) list2))))

; use append (bad performance)
(define (square-list-2 items)
	(define (iter things answer)
		(if (null? things)
				answer
				(iter (cdr things)
							(append answer (list (square (car things)))))))
	(iter items `() ))

;Exercise 2.23
(define (for-each-1 f items)
	(if (null? items)
			#f
			(and (f (car items))
					 (for-each-1 f (cdr items)))))