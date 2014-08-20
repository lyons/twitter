#lang typed/racket/base

(provide (all-defined-out))

(require (only-in "oauth/oauth.rkt"
                  Client
                  Token))
(require "types/types.rkt"
         "types/json.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Parameters

(: current-client (Parameterof Client))
(define current-client
  (make-parameter (Client "" "")))

(: current-token (Parameterof Token))
(define current-token
  (make-parameter (Token "" "")))

(: current-include-entities? (Parameterof (Perhaps Boolean)))
(define current-include-entities?
  (make-parameter (Nothing)))

(: current-skip-status? (Parameterof (Perhaps Boolean)))
(define current-skip-status?
  (make-parameter (Nothing)))

(: current-trim-user? (Parameterof (Perhaps Boolean)))
(define current-trim-user?
  (make-parameter (Nothing)))

;; ---------------------------------------------------------------------------------------------------
;; Response errors

(: twitter-error/has-code? (-> Twitter-Error Integer Boolean))
(define (twitter-error/has-code? err code)
  (if (assq code (Twitter-Error-errors err)) ;; assq does not return a boolean
      #t
      #f))
