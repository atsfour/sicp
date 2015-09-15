;Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;problem a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

;problem b
(define (is-mobile? branch)
  (pair? (branch-structure branch)))

(define (structure-weight branch)
  (if (is-mobile? branch)
      (+ (structure-weight (left-branch (branch-structure branch)))
         (structure-weight (right-branch (branch-structure branch))))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (structure-weight (left-branch mobile))
     (structure-weight (right-branch mobile))))

;problem c
(define (branch-torque branch)
  (* (branch-length branch) (structure-weight branch)))

(define (branch-balanced branch)
  (if (is-mobile? branch)
      #t
      (= (branch-torque))))

(define (balanced mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (if (not (is-mobile? (left-branch mobile)))
           #t
           (balanced (branch-structure (left-branch mobile))))
       (if (not (is-mobile? (right-branch mobile)))
           #t
           (balanced (branch-structure (right-branch mobile))))))

;problem d
(define (new-make-mobile left right)
  (cons left right))

(define (new-make-branch length structure)
  (cons length structure))

; only right-branch and branch-structure are needed to change
;(define (right-branch mobile) (cdr mobile))
;(define (branch-structure branch) (cdr branch))

;sample
(define sample-mobile
  (make-mobile
    (make-branch 5 6)
    (make-branch 3 (make-mobile
                     (make-branch 3 6)
                     (make-branch 3 (make-mobile
                                      (make-branch 1 3)
                                      (make-branch 3 1)))))))

(define new-sample-mobile
  (new-make-mobile
    (new-make-branch 5 6)
    (new-make-branch 3 (new-make-mobile
                         (new-make-branch 3 6)
                         (new-make-branch 3 (new-make-mobile
                                              (new-make-branch 1 3)
                                              (new-make-branch 3 1)))))))