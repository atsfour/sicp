(define (make-accumulator num)
  (let ((result num))
    (lambda (x) (begin (set! result (+ result x))
                  result))))