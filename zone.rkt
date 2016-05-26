#lang racket

(require 
  racket/draw)

(provide (all-defined-out) zone%)

(define zone%
  (class object%
    (init-field id)
    (field [layers (list)])
    (inspect (make-inspector))
    (super-new)
    
    (define/public (add-layer ly)
      (unless (eq? #t (send this has-layer? ly))
        (set! layers (append layers (list ly)))))
    
    (define/public (get-size)
      (apply max (for/list ([l (send this ->list)]) (send l get-size))))
    
    (define/public (get-count)
      (length layers))
    
    (define/public (has-layer? ly)
      (not (equal? #f (member ly layers))))
    
    (define/public (get-layer ly-id)
      (let loop([idx 0])
        (cond ([empty? layers] #f)
              ([equal? idx (length layers)] #f)
              ([equal? ly-id (send (list-ref layers idx) get-id)] (list-ref layers idx))
              (else (loop (+ 1 idx))))))
    
    (define/public (remove-layer ly)
      (set! layers (remove ly layers)))
    
    (define/public (->list)
      (sort (flatten layers)
            (lambda (x y)
              (< (send x get-zindex) (send y get-zindex)))))
    
    (define/public (->zone.bmp ts)
      (let*
          ([sz (* (send this get-size) ts)]
           [bitmap (make-bitmap sz sz)]
           [dc (new bitmap-dc% [bitmap bitmap])])
        (send dc set-brush "black" 'solid)
        (send dc draw-rectangle 0 0 sz sz)
        (for ([i (send this ->list)])
          (send dc draw-bitmap (send i ->layer.bmp ts) 0 0))
        bitmap))
    
    (define/public (->jsexpr)
      (let
          ([json_layers (for/list ([l layers]) (send l ->jsexpr))])
        (hasheq 'id (symbol->string id)
                'layers json_layers)))))

(define (add-layers-to-zone zone . layers)
  (let*
      ([lyrs (flatten layers)])
    (send zone add-layer lyrs)))


