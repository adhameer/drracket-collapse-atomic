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

; Expand the s-expression touching the cursor by one level, i.e. expand it and
; collapse all its immediate children.
(define (expand-one-handler text event)
  (when (text%? text)
    (let* ([start-pos (send text get-start-position)]
           [start-sexp (containing-sexp text start-pos)]
           [end-sexp (send/#f text get-forward-sexp start-sexp)])
      (define-values (start end)
        (cond
          [(not (and start-sexp end-sexp)) (values #f #f)]
          [else (values start-sexp end-sexp)]))

      (when (and start end (collapsed? text start end))
        (send text begin-edit-sequence)

        ; Expand the indicated s-expression.
        (expand-from text (send text find-snip end 'before))

        ; Collapse all immediate children.
        ; Need to collapse them backwards because all later positions
        ; shift once an earlier s-expression is collapsed.
        (collapse-backward
         text
         (send/#f text get-backward-sexp
                  (get-forward*-sexp text
                                     (send text find-down-sexp start)))
         start)

        ; Move the cursor to the end of the last s-expression within
        ; the newly-expanded expression.
        ; Note: expanding an s-expression moves the cursor to the end of it,
        ; so we need to move backward, then in, and then all the way forward.
        (send/#f/∘ text (backward-sexp (get-start-position)))
        (send/#f/∘ text (down-sexp (get-start-position)))
        (send text set-position (get-forward*-sexp text))

        (send text end-edit-sequence)))))

; Collapse all s-expressions in the selection separately.
(define (collapse-all-handler text event)
  (when (text%? text)
    (define-values (start-pos end-pos)
      (expand-range text
                    (send text get-start-position)
                    (send text get-end-position)))

    (send text begin-edit-sequence)
    (collapse-backward text (send text get-backward-sexp end-pos) start-pos)
    (send text end-edit-sequence)))


; ------------------------------------------------------------------------------------------
; Keybinding preferences

(define keybinding-pref 'drracket:collapse-atomic-keybinding)
(define fname "collapse atomic s-expression")
(define default-keybinding "c:s:space")
; worth it to use valid-keybinding? instead of string?
(preferences:set-default keybinding-pref default-keybinding string?)


; Note: will no longer be necessary after racket/gui update.
(define (handle-bad-keybinding keybinding)
  (message-box "Error" (format "Invalid keybinding: \"~a\"" keybinding) #f '(stop ok)))

(define update-keybinding
  (let ([replace-keybinding (make-replaceable-keybinding (global-keymap)
                                                         (preferences:get keybinding-pref)
                                                         fname
                                                         collapse/expand-handler)])
    (λ (new-keybinding)
      (replace-keybinding new-keybinding)
      (preferences:set keybinding-pref new-keybinding))))

(define (update-preference)
  (define choice (get-text-from-user "Collapse Atomic Keybinding"
                                     "Please enter keybinding:"
                                     #f
                                     (preferences:get keybinding-pref)
                                     '(disallow-invalid)
                                     #:validate valid-keybinding?))
  ; Even though we set disallow-invalid, still need to check the entry
  ; because the user could just press Enter. (until racket/gui update)
  (when choice
    ((if (valid-keybinding? choice) update-keybinding handle-bad-keybinding)
     choice)))

; Add a button to the Edit menu for changing the keybinding
(drracket:get/extend:extend-unit-frame
 (mixin (drracket:unit:frame<%>) ()
   (super-new)
   (define/override (edit-menu:between-find-and-preferences edit-menu)
     (super edit-menu:between-find-and-preferences edit-menu)
     (new separator-menu-item% [parent edit-menu])
     (new menu-item%
          [label "Collapse Atomic Keybinding"]
          [parent edit-menu]
          [callback (λ (self _) (update-preference))])
     (new separator-menu-item% [parent edit-menu]))))

(add-keybinding (global-keymap) "c:s:$" "collapse all selected" collapse-all-handler)
(add-keybinding (global-keymap) "c:s:%" "expand one" expand-one-handler)