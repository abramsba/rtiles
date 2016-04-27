#lang racket

(require "chunk.rkt" "utils.rkt")

(define zone-layer%
  (class object%
    (init-field id chunk zindex)
    (inspect (make-inspector))
    (field
     [move-block #f]
     [los-block #f]
     [color "Black"])
    (super-new)
    (define/public (id?) id)
    (define/public (chunk?) chunk)
    (define/public (move-block?) move-block)
    (define/public (move-block! b)
      (set! move-block b))
    (define/public (los-block?) los-block)
    (define/public (los-block! b)
      (set! los-block b))
    (define/public (color?) color)
    (define/public (color! c)
      (set! color c))
    (define/public (zindex?) zindex)
    (define/public (zindex! z)
      (set! zindex z))
    (define/public (locked?)
      (let-values ([(cls skp) (object-info chunk)])
        (equal? cls chunk-immutable%)))
    (define/public (unlock)
      (when (send this locked?)
        (set! chunk (send chunk ->mutable))))
    (define/public (lock)
      (unless (send this locked?)
        (set! chunk (send chunk ->immutable))))
    (define/public (->jsexpr)
      (hasheq 'id id
              'chunk (send chunk ->jsexpr)
              'zindex zindex
              'locked (send this locked?)
              'move-block move-block
              'los-block los-block
              'color color))))
      
(define zone%
  (class object%
    (init-field 'id)
    (field [layers (vector)])
    (inspect (make-inspector))
    (super-new)
    
    
    
