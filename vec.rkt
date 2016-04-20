#lang racket

(require json)

(provide (all-defined-out))
(provide (struct-out vec))

(struct vec (x y) #:transparent)
(define (vec-add v0 v1)
  (vec (+ (vec-x v0) (vec-x v1)) (+ (vec-y v0) (vec-y v1)))) 
(define (vec-sub v0 v1)
  (vec (- (vec-x v0) (vec-x v1)) (- (vec-y v0) (vec-y v1)))) 
(define (vec-mul v0 m)
  (vec (* (vec-x v0) m) (* (vec-y v0) m)))
(define (vec-div v0 d)
  (with-handlers ([exn:fail:contract:divide-by-zero?
                    (lambda (v) (vec +inf.0 +inf.0))])
    (vec (/ (vec-x v0) d) (/ (vec-y v0) d))))
(define (vec-round v0)
  (vec (round (vec-x v0)) (round (vec-y v0))))
(define (vec-floor v0)
  (vec (floor (vec-x v0)) (floor (vec-y v0))))
(define (vec-ceiling v0)
  (vec (ceiling (vec-x v0)) (ceiling (vec-y v0))))
(define (vec-distance v0 v1)
  (sqrt (+ (expt (- (vec-x v1) (vec-x v0)) 2) (expt (- (vec-y v1) (vec-y v0)) 2))))
(define (vec-normal v0 v1)
  (define dist (vec-distance v0 v1))
  (vec-div v0 dist))
(define (vec-d v)
  (vec-distance v (vec 0 0)))
(define (vec-n v)
  (vec-normal v (vec 0 0)))
(define (vec-equals? v0 v1)
  (and
   (= (vec-x v0) (vec-x v1))
   (= (vec-y v0) (vec-y v1))))

(define ((veclist/a vec-x vec-y vec) v0 v1)
  (let* ([x0 (vec-x v0)]
         [y0 (vec-y v0)]
         [x1 (vec-x v1)]
         [y1 (vec-y v1)]
         [n (+ 1 (abs (- x0 x1)))]
         [x-min (min x0 x1)]
         [y-min (if (= x0 x-min) y0 y1)])
    (if (vec-equals? v0 v1)
        (list (vec x0 y0))
        (build-list n (lambda (i)
                        (let* ([x (+ i x-min)]
                               [y (round (+ y-min (* (- x x-min)
                                                     (/ (- y1 y0)
                                                        (- x1 x0)))))])
                          (vec x y)))))))
 
(define veclist/x (veclist/a vec-x vec-y vec))
(define veclist/y (veclist/a vec-y vec-x (lambda (y x) (vec x y))))
 
(define (veclist v0 v1)
  (let* ([x0 (vec-x v0)]
         [y0 (vec-y v0)]
         [x1 (vec-x v1)]
         [y1 (vec-y v1)]
         [dx (abs (- x0 x1))]
         [dy (abs (- y0 y1))])
    ((if (> dx dy) veclist/x veclist/y) v0 v1)))

(define (veclist->jsexpr vl)
  (map vec->jsexpr vl))
(define (veclist->string vl)
  (jsexpr->string (veclist->jsexpr vl)))

(define (vec->jsexpr v)
  (hasheq 'x (vec-x v) 'y (vec-y v)))
(define (vec->string v)
  (jsexpr->string (vec->jsexpr v)))
