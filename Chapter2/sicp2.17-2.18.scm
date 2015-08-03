(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; Exercise 2.17
(define (last-pair x)
  (if (null? (cdr x))
      (car x)
      (last-pair (cdr x))))

; Exercise 2.18
(define (reverse x)
  (if (null? x)
      ()
      (append (reverse (cdr x)) (list (car x)))))

(define sample (list 1 2 3))