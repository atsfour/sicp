;leaf constructor and selector
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf leaf) (cadr leaf))
(define (weight-leaf leaf) (caddr leaf))

;code-tree constructor and selector

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree) 
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;decode
(define (decode bits tree)
  (define (decode1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode1 (cdr bits) tree))
              (decode1 (cdr bits) next-branch)))))
  (decode1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;samples
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-bits
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Exercise 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (exists? symbol symbol-list)
  (cond ((null? symbol-list) #f)
        ((eq? symbol (car symbol-list)) #t)
        (else (exists? symbol (cdr symbol-list)))))

(define (encode-symbol symbol tree) 
  (define (encode-iter symbol tree codelist)
    (if (leaf? tree)
        codelist
        (if (exists? symbol (symbols (left-branch tree)))
            (encode-iter symbol (left-branch tree) (append codelist (list 0)))
            (encode-iter symbol (right-branch tree) (append codelist (list 1))))))
  (if (exists? symbol (symbols tree))
      (encode-iter symbol tree '())
      (error "symbol dosen't exist in tree: ENCODE-SYMBOL" symbol)))

;Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-marge (make-leaf-set pairs)))

(define (successive-marge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-marge
        (adjoin-set (make-code-tree (car leaf-set)
                                    (cadr leaf-set))
                    (cddr leaf-set)))))

;Exercise 2.70
(define rock-list '((A 2) (GET 2) (SHA 3) (WAH 1)
                    (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
(define song '(GET A JOB
               SHA NA NA NA NA NA NA NA NA
               GET A JOB
               SHA NA NA NA NA NA NA NA NA
               WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
               SHA BOOM))

(define rock-tree (generate-huffman-tree rock-list))
;huffman -> 84 bits
;fixed-length -> 38words * 3bits/word = 114 bits

;Exercise 2.71
(define n-5-tree (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16))))
(define n-10-tree (generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16)
                                           (F 32) (G 64) (H 128) (I 256) (J 512))))

;Exercise 2.72

;n個のシンボルがあるとする。
;最大頻度のシンボルはencodeを１ステップ、exists?でn-1ステップを要する。
;最小頻度のシンボルはencodeをlog_2(n)ステップ、各ステップでexists?を1ステップを要する。
;exists?の段階では、right-branchに存在する可能性のほうが高いので、
;(exists? (left-branch tree))ではなく
;(exists? (right-branch tree))にしたほうが全体としてステップ数が少なくて済む。
