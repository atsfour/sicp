(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)      
  (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set2 set1))

;adjoin-set と union-set から element-of-set?を取り除けるので、この２つは速くなる。
;その反面、要素数が増えるのでelement-of-set自体は遅くなる。
;ここでは必要ないがsetの内容を表示する時に、重複を除く手続きが必要になる。
