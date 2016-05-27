#lang racket/gui

(require json "vec.rkt" "layer.rkt" "zone.rkt" "editorfuncs.rkt")

(define editor-zone (random-zone 96 15 16))
(define zone-bitmap (send editor-zone ->zone.bmp 20))

(define main-frame (new frame% [label "Editor"]))
(define main-menu-bar (new menu-bar% [parent main-frame]))
(new menu%
     (label "File")
     (parent main-menu-bar))

(define main-panel (new horizontal-panel%
                          [parent main-frame]))
(define tool-panel (new vertical-panel%
                        [parent main-panel]
                        [min-width 400]
                        [stretchable-width #f]
                        ))

(define (paint-callback canvas dc)
  (send dc draw-bitmap zone-bitmap 0 0))
(define zone-terminal (new editor-canvas%
                           [parent tool-panel]
                           [style '(no-hscroll no-border auto-vscroll)]))
(define layer-panel (new vertical-panel%
                         [parent tool-panel]))
(define layer-listbox (new list-box%
                           [label #f]
                           [parent layer-panel]
                           [choices '()]
                           [style '(single column-headers multiple)]
                           [columns (list "ID" "CH")]))
(define (set-zone-choices)
  (let*
      ([layers (send editor-zone ->list)]
       [charlist
        (map
         (λ (l) (string (send l get-character)))
         layers)]
       [idlist
        (map
         (λ (l) (symbol->string (send l get-id)))
         layers)])
    (send layer-listbox set idlist charlist)))
(set-zone-choices)


(define text (new text%))
(send text insert "# ")
(send text auto-wrap)
(send zone-terminal set-editor text)

(define zone-canvas (new canvas%
                           [parent main-panel]
                           [paint-callback paint-callback]))

(send main-frame maximize #t)
(send main-frame show #t)

