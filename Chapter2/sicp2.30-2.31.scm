;Exercise 2.30

(define (square x) (* x x))

(define (square-tree-direct tree)
  (cond ((null? tree) `())
        ((pair? tree)
         (cons (square-tree-direct (car tree))
               (square-tree-direct (cdr tree))))
        (else (square tree))))

(define (square-tree-map1 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map1 sub-tree)
             (square sub-tree)))
       tree))

;Exercise 2.31
(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

(define sample-list (list 1 2 (list 3 (list 4 5) 6)))