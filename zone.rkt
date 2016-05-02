#lang racket

(require "chunk.rkt" "utils.rkt")

(provide (all-defined-out) zone-layer% zone%)

(define zone-layer%
  (class object%
    (init-field id chunk zindex)
    (inspect (make-inspector))
    (field
     [move-block #f]
     [los-block #f]
     [character #\ ]
     [bg-color "Black"]
     [fg-color "White"]
     [offset-x 0]
     [offset-y 0])
    (super-new)
    (define/public (id?) id)
    (define/public (chunk?) chunk)
    (define/public (move-block?) move-block)
    (define/public (move-block! b)
      (set! move-block b))
    (define/public (los-block?) los-block)
    (define/public (los-block! b)
      (set! los-block b))
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
    (define/public (fg-color?) fg-color)
    (define/public (fg-color! c)
      (set! fg-color c))
    (define/public (bg-color?) bg-color)
    (define/public (bg-color! c)
      (set! bg-color c))
    (define/public (offset-x?) offset-x)
    (define/public (offset-x! x)
      (set! offset-x x))
    (define/public (offset-y?) offset-y)
    (define/public (offset-y! y)
      (set! offset-y y))
    (define/public (->jsexpr)
      (hasheq 'id (symbol->string id)
              'chunk (send chunk ->jsexpr)
              'zindex zindex
              'locked (send this locked?)
              'move-block move-block
              'los-block los-block
              'bg-color bg-color
              'fg-color fg-color
              'offset-x offset-x
              'offset-y offset-y))))
      
(define zone%
  (class object%
    (init-field id)
    (field [layers (list)])
    (inspect (make-inspector))
    (super-new)
    (define/public (add-layer ly)
      (unless (eq? #t (send this has-layer? ly))
        (set! layers (append layers (list ly)))))
    (define/public (has-layer? ly)
      (not (equal? #f (member ly layers))))
    (define/public (get-layer ly-id)
      (let loop([idx 0])
        (cond ([empty? layers] #f)
              ([equal? idx (length layers)] #f)
              ([equal? ly-id (send (list-ref layers idx) id?)] (list-ref layers idx))
              (else (loop (+ 1 idx))))))
    (define/public (remove-layer ly)
      (set! layers (remove ly layers)))
    (define/public (->jsexpr)
      (let
          ([json_layers (for/list ([l layers]) (send l ->jsexpr))])
        (hasheq 'id (symbol->string id)
                'layers json_layers)))))
    

      

