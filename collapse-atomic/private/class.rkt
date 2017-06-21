#lang racket/base

(require syntax/parse/define
         (for-syntax (only-in racket/base syntax))
         (only-in racket/class send is-a?)
         (only-in racket/gui text%)
         (only-in racket/function identity))

(provide send/#f send/#f/∘ text%?)

; If any of the args evaluate to #f, return #f. Otherwise, call method of object with
; arguments args.
; The rewritten expression is almost the same as (and arg ... (send object method arg ...)),
; except that the args are evaluated only once.
; Typically used to chain together text methods relating to positions, which take a
; number as an argument and return either a number or #f.
(define-syntax-rule (send/#f object method arg ...)
  ((λ (arg-list)
     (and (andmap identity arg-list) (send object method . arg-list)))
   (list arg ...)))

; Typically used for chaining text methods that take positions as arguments, e.g.:
; (send/#f/∘ text (get-forward-sexp (get-forward-sexp (get-forward-sexp position))))
; is almost the same as:
; (send/#f text get-forward-sexp
;         (send/#f text get-forward-sexp
;                  (send/#f text get-forward-sexp position))
; The object is computed only once.
; The innermost method call can take any number of arguments.
(define-syntax-parser send/#f/∘
  [(_ object:expr (method:id arg:id ...)) #'(send/#f object method arg ...)]
  [(_ object:expr (method:id (method′:id arg ...)))
   #'(let ([o object])
       (send/#f o method (send/#f/∘ o (method′ arg ...))))])


(define (text%? v) (is-a? v text%))