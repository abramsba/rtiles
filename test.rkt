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

(define (random-unicode-char)
  (integer->char (random #x0400 #x06FF)))

(define (random-line-noise size density)
  (flatten (for/list ([i (random (/ size density))])
    (line->veclist (line (vec (random size) (random size)) (vec (random size) (random size)))))))

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

    

