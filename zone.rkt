#lang racket

(require 
  racket/draw)

(provide (all-defined-out) zone%)

(require "layer.rkt" "chunk.rkt" "vec.rkt" "utils.rkt")

(define zone%
  (class object%
    (init-field id)
    (field [layers (list)])
    (inspect (make-inspector))
    (super-new)
    
    (define/public (add-layer ly)
      (unless (eq? #t (send this has-layer? ly))
        (set! layers (append layers (list ly)))))
    
    (define/public (size?)
      (apply max (for/list ([l (send this ->list)]) (send l size?))))
    
    (define/public (count?)
      (length layers))
    
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
    
    (define/public (->list)
      (sort (flatten layers)
            (lambda (x y)
              (< (send x zindex?) (send y zindex?)))))

    (define/public (->tiles-bitmap ts [font (make-font #:size (/ ts 1.8) #:family 'modern)])
      (let*
          ([t_layers (send this ->list)]
           [t_bmp (make-bitmap (* ts (length t_layers)) ts)]
           [t_dc (new bitmap-dc% [bitmap t_bmp])])
        (for ([l t_layers][i (length t_layers)])
          (let*
              ([l_bmp (send l ->tile-bitmap ts)])
            (send t_dc draw-bitmap l_bmp (* ts i) 0)))
        t_bmp))

    (define/public (->layers-bitmap ts [font (make-font #:size (/ ts 1.8) #:family 'modern)])
      (for/list ([l (send this ->list)])
        (send l ->bitmap ts)))
    
    (define/public (->bitmap ts)
      (let*
          ([sz (* (send this size?) ts)]
           [bitmap (make-bitmap sz sz)]
           [dc (new bitmap-dc% [bitmap bitmap])])
        (send dc set-brush "black" 'solid)
        (send dc draw-rectangle 0 0 sz sz)
        (for ([i (send this ->list)])
          (send dc draw-bitmap (send i ->bitmap ts) 0 0))
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

(define zone (new zone% [id 'zone]))
(define layer1 (new layer% [id 'layer1]))
(define layer2 (new layer% [id 'layer2]))
(define layer3 (new layer% [id 'layer3]))
(define layer4 (new layer% [id 'layer3]))
(define layer5 (new layer% [id 'layer3]))

(add-layers-to-zone zone layer1 layer2 layer3 layer4 layer5)

(define chrs (list #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\_ #\= #\+ #\~ #\`))

(send layer1 bg-color! (get-random (send the-color-database get-names)))
(send layer1 fg-color! (get-random (send the-color-database get-names)))
(send layer1 character! (get-random chrs))
(vec->chunk! (send layer1 chunk?) (vec 0 0) (vec 1 0) (vec 7 7))

(send layer2 bg-color! (get-random (send the-color-database get-names)))
(send layer2 fg-color! (get-random (send the-color-database get-names)))
(send layer2 character! (get-random chrs))
(vec->chunk! (send layer2 chunk?) (vec 1 1) (vec 0 1) (vec 6 0) (vec 0 6))

(send layer3 bg-color! (get-random (send the-color-database get-names)))
(send layer3 fg-color! (get-random (send the-color-database get-names)))
(send layer3 character! (get-random chrs))
(vec->chunk! (send layer3 chunk?) (vec 2 2) (vec 2 0) (vec 5 5))

(send layer4 character! (get-random chrs))
(vec->chunk! (send layer4 chunk?) (vec 2 3) (vec 2 1) (vec 2 7))

(send layer5 bg-color! (get-random (send the-color-database get-names)))
(send layer5 fg-color! (get-random (send the-color-database get-names)))
(send layer5 character! (get-random chrs))
(vec->chunk! (send layer5 chunk?) (vec 3 3) (vec 2 5))


(printf "The zone as a whole\n----\n")
(send zone ->bitmap 16)

(printf "The tiles used in the zone\n----\n")
(send zone ->tiles-bitmap 16)

(printf "The layers used in the zone\n----\n")
(send zone ->layers-bitmap 16)




