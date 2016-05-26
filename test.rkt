#lang racket

(require
  racket/draw
  "vec.rkt"
  "line.rkt"
  "chunk.rkt"
  "layer.rkt"
  "zone.rkt"
  "utils.rkt")

(define (random-color)
  (car (shuffle (send the-color-database get-names))))

<<<<<<< HEAD
(define (random-unicode-char)
  (integer->char (random #x0400 #x06FF)))

(define (random-line-noise size density)
  (flatten (for/list ([i (random (/ size density))])
    (line->veclist (line (vec (random size) (random size)) (vec (random size) (random size)))))))
=======
(define (random-layer id size)
  (let*
      ([maxp (* size size)]
       [rndp (random (/ maxp 8))]
       [colors (send the-color-database get-names)]
       [chars ascii-chars]
       [fgc (get-random colors)]
       [bgc (get-random colors)]
       [fga (exact->inexact (/ (random 100) 100))]
       [bga (exact->inexact (/ (random 100) 100))]
       [chr (get-random chars)]
       [rs (λ (rs) (random (sub1 size)))]
       [plst (for/list ([i rndp]) (vec (random size) (random size)))]
       [chk (vec->chunk size #t plst)]
       [lyr (new layer% [id id][chunk chk][zindex 0])])
    (send lyr fg-color! fgc)
    (send lyr fg-alpha! fga)
    (send lyr bg-color! bgc)
    (send lyr bg-alpha! bga)
    (send lyr character! chr)
    lyr))
>>>>>>> 9e10fca8de6e6fe2e6bbd7549d3f16cd6076c596

(define (random-layer size density id)
  (let*
      ([chunk (new chunk-mutable%[size size])]
       [layer (new layer% [id id][chunk chunk])])
    (send layer <-vec (random-line-noise size density))
    (send layer fg-alpha! (random))
    (send layer bg-alpha! (random))
    (send layer fg-color! (random-color))
    (send layer bg-color! (random-color))
    (send (send layer get-font) offset-x! (random 16))
    (send layer character! (random-unicode-char))
    layer))

(define size 32)
(define layers 20)
(define density 4)
(define tilesize 16)

(let*
    ([zone (new zone%[id 'test])])
  (for ([i layers])
    (let*
        ([id (string->symbol (format "zl_~a" i))]
         [ly (random-layer size density id)])
      (send (send ly get-font) weight! 'bold)
      (send zone add-layer ly)))
  (send zone ->zone.bmp tilesize))
    



<<<<<<< HEAD
=======
(random-seed 0)
(define size 64)
(define layers 200)
>>>>>>> 9e10fca8de6e6fe2e6bbd7549d3f16cd6076c596


<<<<<<< HEAD
=======
(define start-bitmap-render (current-milliseconds))
(send rnd-zone ->bitmap 14)
(define end-bitmap-render (current-milliseconds))
>>>>>>> 9e10fca8de6e6fe2e6bbd7549d3f16cd6076c596


    

