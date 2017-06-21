#lang racket/base

(require (only-in racket/class send new)
         (only-in racket/gui keymap%)
         (only-in framework keymap:get-global keymap:aug-keymap%))

(provide valid-keybinding?
         global-keymap
         add-keybinding
         make-replaceable-keybinding)

; Returns #t if v is a valid keybinding string for the map-function method
; of keymap%.
(define (valid-keybinding? v)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (send (new keymap%) map-function v "")
    #t))

; A nicer alias.
(define global-keymap keymap:get-global)

; Add a keybinding for handler with the function name fname to the given keymap.
(define (add-keybinding keymap keybinding fname handler)
  (send keymap add-function fname handler)
  (send keymap map-function keybinding fname))

; For creating keybindings that can be replaced.
; The keymap classes provide ways of adding keybindings, but not of removing or
;  replacing existing ones. This gets around that by creating a new keymap with
;  only one keybinding that is chained to the provided keymap. To replace the
;  keybinding, the current chained keymap is removed and replaced with a fresh
;  keymap with the new keybinding.
; Returns a function that can be used to replace the current keybinding for
;  function name fname and handling procedure handler.
(define (make-replaceable-keybinding keymap keybinding fname handler)
  (let ([current-keymap (new keymap:aug-keymap%)])
    (add-keybinding current-keymap keybinding fname handler)
    (send keymap chain-to-keymap current-keymap #t)

    (λ (new-keybinding)
      (let ([new-keymap (new keymap:aug-keymap%)])
        (add-keybinding new-keymap new-keybinding fname handler)
        (send keymap remove-chained-keymap current-keymap)
        (send keymap chain-to-keymap new-keymap #t)
        (set! current-keymap new-keymap)))))