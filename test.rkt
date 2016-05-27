#lang racket

(require
  racket/draw
  "vec.rkt"
  "line.rkt"
  "chunk.rkt"
  "layer.rkt"
  "zone.rkt"
  "utils.rkt"
  "editorfuncs.rkt")

(define zones (for/list ([i 5])
                (random-zone 32 35 2 48)))


