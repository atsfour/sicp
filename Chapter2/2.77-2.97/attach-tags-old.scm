;2.4.2 tagged data new version at Exercise 2.78
(define (old-attach-tag type-tag contents)
  (cons type-tag contents))
(define (old-type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))
(define (old-contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))