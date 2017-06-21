#lang racket/base

(require (only-in racket/class send is-a? instantiate)
         (only-in framework racket:sexp-snip%)
         (only-in racket/match match)
         (only-in racket/function negate)
         (only-in "class.rkt" send/#f send/#f/∘))

(provide bracket? matching-delimiter
         atomic-sexp? compound-sexp? collapsed?
         get-forward*-sexp
         collapse-from expand-from
         containing-sexp
         expand-range
         collapse-backward)

; NOTE: works fine if char isn't actually a character.
(define (bracket? char) (memq char '(#\( #\{ #\[ #\) #\} #\])))

; If char is a right bracket, return the matching left bracket; otherwise return #\nul.
(define (matching-delimiter char) (match char [#\) #\(] [#\] #\[] [#\} #\{] [_ #\nul]))

; text% number number -> boolean
; Returns #t if start and end are the start and end positions of a collapsed
; snip, otherwise #f. Assumes start and end are valid positions.
(define (collapsed? text start end)
  (let* ([snip-before (send text find-snip end 'before)]
         [snip-after (send text find-snip start 'after)])
    (and snip-before
         snip-after
         (eq? snip-before snip-after)
         (is-a? snip-before racket:sexp-snip%))))

; Return the end position of the last S-expression within the S-expression
; containing pos, i.e. the position you would get from repeatedly calling
; forward-sexp until there is nowhere left to move.
; If pos is not provided, it is initialized to the start position of text.
(define (get-forward*-sexp text [pos #f] [prev-pos pos])
  (define (helper current-pos [prev-pos current-pos])
    (if current-pos
        (helper (send text get-forward-sexp current-pos) current-pos)
        prev-pos))

  (helper (or pos (send text get-start-position))))

; Assuming that pos is located at the start of an S-expression, returns #t
; if that S-expression is atomic, #f if it is compound.
; NOTE: Can't just check if find-down-sexp is non-#f, because find-down-sexp
; moves forward until it finds a compound S-expression to descend into.
(define (atomic-sexp? text pos)
  (define right (send text get-forward-sexp pos))
  (define down (send text find-down-sexp pos))
  (or
   ; No compound S-expressions in the entire containing S-expression
   (not down)
   ; NOTE: by assumption, right can never be #f.
   ; When both down and right are non-#f:
   ; down < right iff a compound S-expression occurs before the end of
   ; the current S-expression,
   ; iff the current S-expression is compound.
   (down . >= . right)))

(define compound-sexp? (negate atomic-sexp?))

; Precondition: pos is at the front of an s-expression
#;
(define (collapse-down text pos)
  (when (and pos (compound-sexp? text pos))
    (collapse-backward
     (let loop ([end-pos (send text find-down-sexp pos)])
       (cond [(not end-pos) #f]
             [(not (send text get-forward-sexp end-pos))
              (send text get-backward-sexp end-pos)]
             [else (loop (send text get-forward-sexp end-pos))])))
    (collapse-from text pos (send text get-forward-sexp pos))))

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

#| Return the position of the start of the s-expression in text such that either
   that s-expression contains position, or position is at the front or end of it.
   If there are two such s-expressions, return the start position of the earlier one.
   If position is not touching any s-expressions, return #f.

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
(define (containing-sexp text position)
  (let* ([backward (send text get-backward-sexp position)]
         [backward-forward (send/#f text get-forward-sexp backward)]
         [start-pos
          (if (and backward-forward (backward-forward . >= . position))
              backward
              ; position could be at the head of an s-expression, or located in whitespace or
              ; a comment preceding one. In the latter case, move position ahead to the
              ; actual s-expression; in the former case these two operations should cancel out.
              (send/#f/∘ text (get-backward-sexp (get-forward-sexp position))))])
    (cond
      [(not start-pos) #f]
      ; Since start-pos is the head of an s-expression, get-forward-sexp can't return #f.
      [(<= start-pos position (send text get-forward-sexp start-pos)) start-pos]
      ; position is not touching any s-expressions.
      [else #f])))

; Return two positions: the start of the s-expression containing or touching start, and the
; end of the s-expression containing or touching end.
; If the first value is not applicable, start will be returned in its place, and if the second
; value is not applicable, end will be returned in its place.
; (See documentation for containing-sexp to see when a value might not be applicable.)
(define (expand-range text start end)
  (let ([start-sexp (containing-sexp text start)]
        [end-sexp (send/#f text get-forward-sexp (containing-sexp text end))])
    (values (or start-sexp start) (or end-sexp end))))

; Return the position of the start of the smallest s-expression in text containing
; both pos1 or pos2, or #f is there is none.
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

; text% number number -> void
; Collapse every compound s-expression backward from position, in the compound
; s-expression containing position, until start-pos is reached.
; Precondition: pos is at the front of an s-expression
(define (collapse-backward text pos start-pos)
  (when (and pos (pos . >= . start-pos))
    (let ([end (send text get-forward-sexp pos)])
      (unless (or (collapsed? text pos end) (atomic-sexp? text pos))
        (collapse-from text pos end)))
    (collapse-backward text (send text get-backward-sexp pos) start-pos)))