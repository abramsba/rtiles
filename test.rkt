#lang racket

(require
  racket/draw
  "vec.rkt"
  "chunk.rkt"
  "layer.rkt"
  "zone.rkt"
  "utils.rkt")

(define ascii-chars
  (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
        #\a #\b #\c #\d #\e #\f #\g #\w #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
        #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
        #\  #\` #\~ #\! #\@ #\$ #\# #\%))

(define (random-layer id size)
  (let*
      ([maxp (* size size)]
       [rndp (random (/ maxp 4))]
       [colors (send the-color-database get-names)]
       [chars ascii-chars]
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

(define rnd-zone (random-zone 'whatever 128 3))
(define lyrs (send rnd-zone ->list))
(send rnd-zone ->bitmap 20)

