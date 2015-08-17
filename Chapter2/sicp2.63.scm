; binary tree
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

; set operation
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((>x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-set x (right-branch set))))))

;Exercise 2.63
(define (tree-to-list-1 tree)
  (if (null? tree)
      '()
      (append (tree-to-list-1 (left-branch tree))
              (cons (entry tree)
                    (tree-to-list-1 (right-branch tree))))))

(define (tree-to-list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;Exercise 2.64
(define (list-to-tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
                (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                    (partial-tree
                      (cdr non-left-elts)
                      right-size)))
              (let ((right-tree (car right-result))
                    (remainding-elts
                      (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remainding-elts))))))))

;partial-treeはリストeltsのうちn個の要素をバランスのとれた木構造とし、
;残りの部分をそのままリストとして返す手続きである。
;partial-treeは1回あたりで長さが約半分になり、自身を2回呼び出すため、
;list-to-treeは全体としてO(2)のオーダーを持つ

;samples
(define sample-tree-1
  (make-tree 7 
             (make-tree 3
                        (make-tree 1
                                   '()
                                   '())
                        (make-tree 5
                                   '()
                                   '()))
             (make-tree 9
                        '()
                        (make-tree 11
                                   '()
                                   '()))))

;Exercise 2.65
(define (intersection-set set1 set2)
  )
(define (union-set set1 set2)
  )
