#lang racket/unit

(require racket/gui
         drracket/tool
         (only-in framework
                  preferences:set-default preferences:get preferences:set)
         "private/class.rkt"
         "private/sexp.rkt"
         "private/keybinding.rkt")

(import drracket:tool^)
(export drracket:tool-exports^)

(define (phase1) (void))
(define (phase2) (void))

; Collapse/expand the s-expression touching the cursor, or if a selection,
; collapse all the selected s-expressions together as a unit.
(define (collapse/expand-handler text event)
  (when (text%? text)
    ; If this is invoked on a selection, start-pos and end-pos are the start and end
    ; positions of the selection. Otherwise, start-pos and end-pos are the same and
    ; correspond to the position of the cursor.
    (let* ([start-pos (send text get-start-position)]
           [start-sexp (containing-sexp text start-pos)]
           [end-sexp (send/#f text get-forward-sexp start-sexp)]
           [end-pos (send text get-end-position)])
      (define-values (start end)
        (cond
          ; Something went wrong?
          [(not (and start-sexp start-pos end-pos end-sexp)) (values #f #f)]

          ; For selections, assume that start-pos and end-pos are located in
          ; sibling s-expressions. Expand the selection to include both those
          ; s-expressions and collapse it.
          [(not (= start-pos end-pos)) (expand-range text start-pos end-pos)]

          ; Collapse the atomic s-expression containing the cursor.
          [else (values start-sexp end-sexp)]))

      ; Check if this was invoked on an already-collapsed s-expression.
      ; If so, expand it instead.
      (when (and start end)
        (if (collapsed? text start end)
            (expand-from text (send text find-snip end 'before))
            (collapse-from text start end))))))



(define (add-key-bindings keymap)
  (send keymap add-function "collapse atomic s-expression" collapse-handler)
  (send keymap map-function "c:s:space" "collapse atomic s-expression"))

(add-key-bindings (drracket:rep:get-drs-bindings-keymap))
