#lang typed/racket/base

(provide unpack-error)

;; ---------------------------------------------------------------------------------------------------
;; Helper macros

(define-syntax unpack-error
  (syntax-rules ()
    [(unpack-error func type val)
     (error (format "~a: expected value of type ~a, received: ~a" func (quote type) val))]))
