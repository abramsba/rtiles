#lang racket/gui

(require json "vec.rkt" "layer.rkt" "zone.rkt" "editorfuncs.rkt")

(define editor-zone (random-zone 96 15 16))
(send (send editor-zone get-layer 'zl_0) lock)
(send (send editor-zone get-layer 'zl_1) lock)
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
                        [min-width 300]
                        [stretchable-width #f]
                        ))

(define zone-terminal (new editor-canvas%
                           [parent tool-panel]
                           [style '(no-hscroll no-border auto-vscroll)]))
(define text (new text%[auto-wrap #t]))
(define style-delta (make-object style-delta% 'change-normal-color))
(send style-delta set-delta-background "black")
(send style-delta set-delta-foreground "white")
(send text change-style style-delta)
(send text insert " > ")
(send zone-terminal set-editor text)
(define zone-terminal-dc (send zone-terminal get-dc))
(send zone-terminal-dc draw-line 0 30 400 30)

(define layer-panel (new vertical-panel%
                         [parent tool-panel]))
(define layer-listbox (new list-box%
                           [label #f]
                           [parent layer-panel]
                           [choices '()]
                           [style '(single column-headers multiple)]
                           [columns (list "L" "ID" "CH")]))
(send layer-listbox set-column-width 1 150 150 150)
(define (set-zone-choices)
  (let*
      ([layers (send editor-zone ->list)]
       [charlist
        (map
         (λ (l) (string (send l get-character)))
         layers)]
       [locklist
        (map
         (λ (l)
           (if (send l locked?) "X" "")) layers)]
       [idlist
        (map
         (λ (l) (symbol->string (send l get-id)))
         layers)])
    (send layer-listbox set locklist idlist charlist)))
(set-zone-choices)


(define (paint-callback canvas dc)
  (send dc draw-bitmap zone-bitmap 0 0))
(define zone-canvas (new canvas%
                           [parent main-panel]
                           [paint-callback paint-callback]))

(send main-frame maximize #t)
(send main-frame show #t)
