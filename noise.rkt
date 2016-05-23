#lang racket

(require "vec.rkt")

(provide noise% interpolate)

(define (smooth noise x y)
  (define-syntax-rule (ng x y)
    (send noise 2d x y))
  (let*
      ([corners (/ (+
                    (ng (sub1 x) (sub1 y))
                    (ng (add1 x) (sub1 y))
                    (ng (sub1 x) (add1 y))
                    (ng (add1 x) (add1 y))) 16)]
       [sides (/ (+
                  (ng (sub1 x) y)
                  (ng (add1 x) y)
                  (ng x (sub1 y))
                  (ng x (add1 y))) 8)]
       [center (/ (ng x  y) 4)])
    (+ corners sides center)))  

(define (interpolate noise vc)
  (define (inter a b x)
    (let*
        ([ft (* x pi)]
         [f (* (- 1 (cos ft)) 0.5)]
         [l (* a (- 1 f))]
         [r (* b x)])
      (+ l r)))  
  (define-syntax-rule (v-x)
    (vec-x vc))
  (define-syntax-rule (v-y)
    (vec-y vc))
  (let*
      ([xe (truncate (v-x))]
       [xf (- (v-x) xe)]
       [ye (truncate (v-y))]
       [yf (- (v-y) ye)]
       [v1 (smooth noise xe ye)]
       [v2 (smooth noise (add1 xe) ye)]
       [v3 (smooth noise xe (add1 ye))]
       [v4 (smooth noise (add1 xe) (add1 ye))]
       [i1 (inter v1 v2 xf)]
       [i2 (inter v3 v4 xf)])
    (inter i1 i2 yf)))

(define noise%
  (class object%
    (super-new)
    (init-field [v '#(15731 789221 1376312589)])
    (define/public (1d x)
      (let*
          ([x1 (bitwise-xor (arithmetic-shift 13 x) x)]
           [x2 (* x x (vector-ref v 0))]
           [x3 (+ x2 (vector-ref v 1))]
           [x4 (* x x3)]
           [x5 (+ x4 (vector-ref v 2))]
           [x6 (bitwise-and x5 #x7fffffff)]
           [x7 (/ x6 1073741824.0)]
           [x8 (- 1.0 x7)])
        x8))
    (define/public (2d x y)
      (let*
          ([n (+ (* y 57) x)]
           [x1 (bitwise-xor (arithmetic-shift 13 n) n)]
           [x2 (* x1 x1 (vector-ref v 0))]
           [x3 (+ x2 (vector-ref v 1))]
           [x4 (* x1 x3)]
           [x5 (+ x4 (vector-ref v 2))]
           [x6 (bitwise-and x5 #x7fffffff)]
           [x7 (/ x6 1073741824.0)]
           [x8 (- 1.0 x7)])
        x8))))
