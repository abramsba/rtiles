#lang racket

(require
  racket/draw
  "../vec.rkt"
  "../noise.rkt")

(define (perlin noise v persistence octaves)
  (apply +
         (for/list ([i octaves])
           (let*
               ([freq (expt 2 i)]
                [amp (expt persistence i)]
                [xfq (* (vec-x v) freq)]
                [yfq (* (vec-y v) freq)])
             (* (interpolate noise (vec xfq yfq)) amp)))))

(define (perlin->bitmap noise persistence octaves tilesize)
  (define (nm po)
    (define o (/ (+ po 1) 2))
    (if (> o 1) 1 o))
  (let*
      ([ts (* octaves tilesize)]
       [bmp (make-bitmap ts ts)]
       [dc (new bitmap-dc% [bitmap bmp])])
    (for* ([x octaves][y octaves])
      (let*
          ([pval (perlin noise (vec x y) persistence octaves)]
           [aval (nm pval)]
           [xcur (* x tilesize)]
           [ycur (* y tilesize)])
        (send dc set-pen "black" 0 'transparent)
        (send dc set-brush "white" 'solid)
        (send dc set-alpha aval)
        (send dc draw-rectangle xcur ycur tilesize tilesize)))
    bmp))
      
                   