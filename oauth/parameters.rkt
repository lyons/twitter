#lang typed/racket/base

(provide (all-defined-out))

;; default value of Accept header
(: oauth-accept (Parameterof String))
(define oauth-accept
  (make-parameter "*/*"))

;; default value of User-Agent header
(: oauth-user-agent (Parameterof String))
(define oauth-user-agent
  (make-parameter "Racket OAuth Client"))

;; PNRG used for generating nonce
(: oauth-pseudo-random-generator (Parameterof Pseudo-Random-Generator))
(define oauth-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)))
