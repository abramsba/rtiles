#lang racket/gui

(require json "vec.rkt" "layer.rkt" "zone.rkt" "editorfuncs.rkt")

(define editor-zone (random-zone 96 2 16))
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
(define terminal-panel (new vertical-panel%
                            [parent tool-panel]))

(define terminal-buttons (new horizontal-panel%
                              [parent tool-panel]
                              [min-height 32]
                              [stretchable-height #f]))

(define zone-terminal (new editor-canvas%
                           [parent terminal-panel]
                           [style '(no-hscroll no-border auto-vscroll)]))
(define zone-output-term (new editor-canvas%
                              [parent terminal-panel]
                              [style '(no-hscroll no-border auto-vscroll)]
                              [min-height 100]
                              [stretchable-height #f]))
(define zone-output-text (new text%[auto-wrap #t]))
(send zone-output-term set-editor zone-output-text)

(define zone-terminal-run (new button%
                               [parent terminal-buttons]
                               [callback
                                (λ (evt c)
                                  (let*
                                      ([code (format "(let () ~a)" (send text get-text))]
                                       [res (eval (read (open-input-string code)))])
                                    (send zone-output-text erase)
                                    (send zone-output-text insert (format "~a" res))))]
                               [label "Run"]))

(define text (new text%[auto-wrap #t]))
(define style-delta (make-object style-delta% 'change-normal-color))
(send style-delta set-delta-face "Fixed")
(send style-delta set-delta 'change-size 10)
(send text change-style style-delta)
(send zone-terminal set-editor text)

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
