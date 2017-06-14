#lang racket/unit

(require framework
         (only-in racket/gui text%)
         (only-in racket/class send is-a? instantiate)
         (only-in racket/match match)
         drracket/tool)

(import drracket:tool^)
(export drracket:tool-exports^)

(define (bracket? char) (memq char '(#\( #\{ #\[ #\) #\} #\])))
(define (matching-delimiter char) (match char [#\) #\(] [#\] #\[] [#\} #\{] [_ #\nul]))

; Mostly copied from gui-lib/framework/private/racket.rkt.
(define (collapse-from text left-pos right-pos)
  (when (and left-pos right-pos)
    (let* ([right (send text get-character (- right-pos 1))]
           ; If the selection is a compound s-expression, frame the collapsed snip with
           ; its brackets.
           ; NOTE: Decide if a selection is a compound s-expression by looking
           ; at the right bracket only, because delimiters like #; are counted as
           ; parts of s-expressions.
           [left-bracket (matching-delimiter right)]
           [right-bracket (if (bracket? right) right #\nul)])
      (send text begin-edit-sequence)
      (send text split-snip left-pos)
      (send text split-snip right-pos)
      (let ([snips (let loop ([snip (send text find-snip left-pos 'after)])
                     (cond
                       [(not snip) null]
                       [((send text get-snip-position snip) . >= . right-pos)
                        null]
                       [else (cons (send snip copy) (loop (send snip next)))]))])
        (send text delete left-pos right-pos)
        (send text insert (instantiate racket:sexp-snip% ()
                            (left-bracket left-bracket)
                            (right-bracket right-bracket)
                            (saved-snips snips))
              left-pos left-pos)
        (send text end-edit-sequence)))))

(define (expand-from text snip)
  (let ([snips (send snip get-saved-snips)])
    (send text begin-edit-sequence)
    (let ([pos (send text get-snip-position snip)])
      (send text delete pos (+ pos 1))
      (let loop ([snips (reverse snips)])
        (cond
          [(null? snips) (void)]
          [else (send text insert (send (car snips) copy) pos pos)
                (loop (cdr snips))])))
    (send text end-edit-sequence)))

; For more easily chaining together text accessors.
(define-syntax-rule (send-unless-false text command arg ...)
  (if (and arg ...)
      (send text command arg ...)
      #f))

#| Return the position of the start of the closest s-expression to position in text.

   Algorithm:
   • Need to find out if the start position is at the front of an s-expression, at the end
     of an s-expression, or in the middle of an s-expression.
   • If the start position is at the end or middle of an s-expression, get-backward-sexp
     will return the desired position. If the start position is at the front of an s-expression,
     we want to return the start position.
   • To determine if the start position is at the front of an s-expression:
     ∘ get-backward-sexp followed by get-forward-sexp will return a position greater than or
       equal to the start position if the start position is at the end or in the middle of
       an s-expression. Otherwise it will return either #f (if the start position immediately
       follows a parenthesis) or a position strictly less than the start position.

   NOTE: Inside the empty s-expression, this just returns the position of the cursor. |#
(define (nearest-sexp text position)
  (let* ([backward (send text get-backward-sexp position)]
         [backward-forward (send-unless-false text get-forward-sexp backward)])
    (if (and backward-forward (backward-forward . >= . position))
        backward
        ; position could be at the head of an s-expression, or located in whitespace or
        ; a comment preceding one. In the latter case, move position ahead to the
        ; actual s-expression; in the former case these two operations should cancel out.
        (send-unless-false text get-backward-sexp
                           (send-unless-false text get-forward-sexp position)))))

; Return two positions: the start of the nearest s-expression to start, and the end of
; the nearest s-expression to end. Either of these values may be #f if not applicable.
(define (expand-selection text start end)
  (let ([start-sexp (nearest-sexp text start)]
        [end-sexp (send-unless-false text get-forward-sexp (nearest-sexp text end))])
    (values start-sexp end-sexp)))

; Return the position of the start of the smallest s-expression in text containing
; both pos1 or pos2, or #f is there is none.
; This isn't used anymore at the moment, but it might be in the future.
(define (smallest-containing-sexp text pos1 pos2)
  (let loop ([pos1 pos1]
             [pos2 pos2])
    (cond
      ; No more s-expressions to back out of: no common containing s-expression.
      [(or (not pos1) (not pos2)) #f]
      ; Found the smallest containing s-expression: return its start position.
      [(= pos1 pos2) pos1]
      ; If pos1 occurs before pos2, back out of the s-expression containing pos2.
      [(< pos1 pos2) (loop pos1 (send text find-up-sexp pos2))]
      ; Otherwise, back out of the s-expression containing pos1.
      [else (loop (send text find-up-sexp pos1) pos2)])))

(define (collapse-handler text event)
  (when (is-a? text text%)
    ; If this is invoked on a selection, start-pos and end-pos are the start and end
    ; positions of the selection. Otherwise, start-pos and end-pos are the same and
    ; correspond to the position of the cursor.
    (let* ([start-pos (send text get-start-position)]
           [start-sexp (nearest-sexp text start-pos)]
           [end-sexp (send-unless-false text get-forward-sexp start-sexp)]
           [end-pos (send text get-end-position)])
      (define-values (start end)
        (cond
          ; Something went wrong?
          [(not (and start-sexp start-pos end-pos end-sexp)) (values #f #f)]

          ; For selections, assume that start-pos and end-pos are located in
          ; sibling s-expressions. Expand the selection to include both those
          ; s-expressions and collapse it.
          [(not (= start-pos end-pos)) (expand-selection text start-pos end-pos)]
            
          ; Collapse the atomic s-expression containing the cursor.
          [else (values start-sexp end-sexp)]))

      ; Check if this was invoked on an already-collapsed s-expression.
      ; If so, expand it instead.
      (let* ([snip-before (send text find-snip end 'before)]
             [snip-after (send text find-snip start 'after)]
             [collapsed? (and snip-before
                              snip-after
                              (eq? snip-before snip-after)
                              (is-a? snip-before racket:sexp-snip%))])
        (if collapsed?
            (expand-from text snip-before)
            (collapse-from text start end))))))


(define (phase1) (void))
(define (phase2) (void))

(define (add-key-bindings keymap)
  (send keymap add-function "collapse atomic s-expression" collapse-handler)
  (send keymap map-function "c:s:space" "collapse atomic s-expression"))

(add-key-bindings (drracket:rep:get-drs-bindings-keymap))
