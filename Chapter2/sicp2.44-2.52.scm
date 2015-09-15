#!/usr/local/bin/gosh
(use gl)
(use gl.glut)

; draw-line procedure with gauche-gl
(define (draw-line v1 v2)
  (gl-vertex (xcor-vect v1) (ycor-vect v1))
  (gl-vertex (xcor-vect v2) (ycor-vect v2)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode GLUT_RGBA)
  (glut-init-window-size 600 600)
  (glut-create-window "SICP Picture Langage Sample")
  (glut-display-func display)
  (init)
  (glut-main-loop))

(define (display)
  (gl-color 0.0 0.2 1.0)
  (gl-begin GL_LINES)
  (X-painter frame-basic)
  (gl-end)
  (gl-flush)
  )

(define (init)
  (gl-clear-color 0.0 0.0 1.0 0.0)
  (gl-line-width 4)
  )

; prepareing

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs2 painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit2 painter)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;Exercise 2.45
(define (split op1 op2)
  (define (result painter n)
    (let ((smaller (result painter (- n 1))))
      (op1 painter (op2 smaller smaller))))
  result)

(define (right-split2 (split beside below)))
(define (up-split2 (split below beside)))

;frame-coord-map1
(define (frame-coord-map1 frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v)
                            (edge1-frame frame))
                (scale-vect (ycor-vect v)
                            (edge2-frame frame))))))

;Exercise 2.46
(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (sub-vect v1 v2)
  (add-vect v1 (scale-vect -1 v2)))

;Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))

;painter
(define (segments-to-painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map1 frame)
           (start-segment segment))
          ((frame-coord-map1 frame)
           (end-segment segment))))
      segment-list)))

;Exercise 2.48
(define (make-segment v1 v2)
  (list v1 v2))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cadr segment))

;Exercise 2.49

(define (frame-painter frame)
  ((segments-to-painter
     (list (make-segment (make-vect 0 0) (make-vect 0 1))
           (make-segment (make-vect 0 1) (make-vect 1 1))
           (make-segment (make-vect 1 1) (make-vect 1 0))
           (make-segment (make-vect 1 0) (make-vect 0 0))))
   frame))

(define (X-painter frame)
  ((segments-to-painter
     (list (make-segment (make-vect 0 0) (make-vect 1 1))
           (make-segment (make-vect 1 0) (make-vect 0 1))))
   frame))

(define (diamond-painter frame)
  ((segments-to-painter
     (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
           (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
           (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
           (make-segment (make-vect 1 0.5) (make-vect 0.5 0))))
   frame))

(define (mirror-vect v)
  (make-vect (- 1.0 (xcor-vect v)) (ycor-vect v)))
(define (mirror-segment seg)
  (make-segment (mirror-vect (start-segment seg))
                (mirror-vect (end-segment seg))))

(define wave-segments
  (let ((left-hand-top (make-vect 0 0.7))
        (left-hand-bottom (make-vect 0 0.6))
        (left-elbow-top (make-vect 0.2 0.6))
        (left-elbow-bottom (make-vect 0.2 0.5))
        (left-shoulder-top (make-vect 0.4 0.7))
        (left-shoulder-bottom (make-vect 0.4 0.6))
        (left-ear (make-vect 0.3 0.8))
        (left-head (make-segment 0.4 1.0))
        (left-hip (make-segment 0.4 0.3))
        (left-leg-left (make-segment 0.3 0.0))
        (left-leg-right (make-segment 0.4 0.0))
        (crotch (make-segment 0.5 0.3))
        (right-hand-top (make-segment 1.0 0.4))
        (right-hand-bottom (make-segment 1.0 0.3)))
    (let ((body-lines
            (list (make-segment left-shoulder-top left-ear)
                  (make-segment left-ear left-head)
                  (make-segment left-shoulder-bottom left-hip)
                  (make-segment left-hip left-leg-left)
                  (make-segment left-leg-right crotch))))
      (append body-lines
              (map mirror-segment
                   body-lines)
              (list (make-segment left-hand-top left-elbow-top)
                    (make-segment left-elbow-top left-shoulder-top)
                    (make-segment left-hand-bottom left-elbow-bottom)
                    (make-segment left-elbow-bottom left-shoulder-bottom)
                    (make-segment (mirror-vect left-shoulder-top) right-hand-top)
                    (make-segment (mirror-vect left-shoulder-bottom) right-hand-bottom))))))

(define (wave-painter frame)
  ((segments-to-painter
     wave-segments)
   frame))

; transform-painter

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map1 frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))

;Exercise 2.50
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

;Exercise 2.51

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              split-point
              (make-vect 0.0 1.0)))
          (paint-right
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.0)
              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter
              painter1
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point))
          (paint-top
            (transform-painter
              painter2
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

;Exercise 2.52

(define (wave-painter-with-smile frame)
  ((segments-to-painter
     (append wave-segments
             (list (make-segment (make-vect 0.35 0.85) (make-vect 0.4 0.9))
                   (make-segment (make-vect 0.4 0.9) (make-vect 0.45 0.85))
                   (make-segment (make-vect 0.65 0.85) (make-vect 0.6 0.9))
                   (make-segment (make-vect 0.6 0.9) (make-vect 0.55 0.85))
                   (make-segment (make-vect 0.4 0.8) (make-vect 0.5 0.75))
                   (make-segment (make-vect 0.6 0.8) (make-vect 0.5 0.75)))))
   frame))

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      ((square-of-four identity
                       (lambda (x) (corner-split2 x (- n 1)))
                       identity
                       identity)
       painter)))

;exmaple
(define v1 (make-vect 0.5 0.5))
(define v2 (make-vect 0 0.7))
(define v3 (make-vect 0.3 0))
(define seg1 (make-segment v1 v2))
(define seg2 (make-segment v2 v3))
(define frame1 (make-frame (make-vect 2 2) (make-vect 2 0) (make-vect 0 1)))
(define frame-basic (make-frame (make-vect -1 -1) (make-vect 2 0) (make-vect 0 2)))