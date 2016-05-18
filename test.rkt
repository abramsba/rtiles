#lang racket

(require
  racket/draw
  "vec.rkt"
  "chunk.rkt"
  "layer.rkt"
  "zone.rkt"
  "utils.rkt")

(define (random-layer id size)
  (let*
      ([maxp (* size size)]
       [rndp (random (/ maxp 32))]
       [colors (send the-color-database get-names)]
       ;[chars (list #\u0414 #\u0416 #\u042e #\u04f8 #\u0394 #\u03a9 #\u03a3)]
       [chars (list #\+ #\/ #\\ #\x #\@ #\P )]
       [fgc (get-random colors)]
       [bgc (get-random colors)]
       [fga (exact->inexact (/ (random 100) 100))]
       [bga (exact->inexact (/ (random 100) 100))]
       [chr (get-random chars)]
       [plst (for/list ([i rndp]) (vec (random size) (random size)))]
       [chk (vec->chunk size #t plst)]
       [lyr (new layer% [id id][chunk chk][zindex 0])])
    (send lyr fg-color! fgc)
    (send lyr fg-alpha! fga)
    (send lyr bg-color! bgc)
    (send lyr bg-alpha! bga)
    (send lyr character! chr)
    lyr))

(define (random-zone id size layers)
  (let*
      ([zone (new zone% [id id])]
       [lyrs (for/list ([l layers]) (random-layer 'ok size))])
    (for ([l lyrs])
      (send zone add-layer l))
    zone))

(define rnd-zone (random-zone 'whatever 64 26))
;(send rnd-zone ->bitmap 128)
(define lyrs (send rnd-zone ->list))
(send rnd-zone ->bitmap 8)
(for/list ([l lyrs])
  (let*
      ([sz 2]
       [bitmap (make-bitmap (* sz (send l size?)) (* sz (send l size?)))]
       [dc (new bitmap-dc% [bitmap bitmap])])
    (send dc set-brush "black" 'solid)
    (send dc draw-rectangle 0 0 (* sz (send l size?)) (* sz (send l size?)))
    (send dc draw-bitmap (send l ->bitmap sz) 0 0)
    bitmap))