#lang typed/racket/base

(provide (all-defined-out))

(require (only-in racket/file
                  file->bytes)
         (only-in racket/path
                  filename-extension))
(require/typed net/base64
               [base64-encode (->* [Bytes] [Bytes] Bytes)])
(require "../../oauth/oauth.rkt")
(require "../../types/types.rkt"
         "../../types/json.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Helper macros

(define-syntax response-error
  (syntax-rules ()
    [(unpack-error func result)
     (error (format "~a: response does not match any format for this endpoint: ~a"
                    func
                    result))]))

;; ---------------------------------------------------------------------------------------------------
;; Helper functions

(: enforce-range (-> (Perhaps Integer) (U Integer '-inf) (U Integer '+inf) (Perhaps Integer)))
(define (enforce-range n lower upper)
  (cond
    [(Nothing? n) (Nothing)]
    [else (cond
            [(and (not (eq? '-inf lower)) (< n lower)) lower]
            [(and (not (eq? '+inf upper)) (> n upper)) upper]
            [else n])]))

(: perhaps-param
   (All (α) (-> String (Perhaps α) (-> α String) (U HTTP-Parameter-List Null))))
(define (perhaps-param key value f)
  (cond
    [(Nothing? value) '()]
    [else (list (Param key (f value)))]))

;; ---------------------------------------------------------------------------------------------------
;; Request URLs

(: twitter/url
   (->* [String]
        [HTTP-Parameter-List]
        OAuth-URL))
(define (twitter/url path [query '()])
  (OAuth-URL 'https "api.twitter.com" path query))

;; ---------------------------------------------------------------------------------------------------
;; Response errors

(: twitter/error (-> Bytes Input-Port Twitter-Error))
(define (twitter/error http-response body)
  (define errors
    (let ([x (read-json body)])
      (cond
        [(hash? x) (let ([y (hash-ref (json-hash x) 'errors JSON-Null)])
                     (cond
                       [(list-of-hash? y) (map (λ ([err : (HashTable Any Any)])
                                                 (let* ([hash (json-hash err)]
                                                        [code (hash-ref hash 'code JSON-Null)]
                                                        [msg (hash-ref hash 'message JSON-Null)])
                                                   (cond
                                                     [(and (exact-integer? code) (string? msg))
                                                      `(,code ,msg)]
                                                     [else '(0 "Unknown error response")])))
                                               y)]
                       [else '((0 "Unknown error response"))]))]
        [else '((0 "Unknown error response"))])))
  (Twitter-Error http-response errors))

;; ---------------------------------------------------------------------------------------------------
;; Image uploads

(define-predicate valid-image-format? (U #"gif" #"jpg" #"jpeg" #"png"))

; Twitter uses 1024 kilobyte
(: below-filesize-in-kb? (-> Integer (-> Integer Boolean)))
(define (below-filesize-in-kb? size-in-kb)
  (λ (size-in-bytes)
    (< size-in-bytes (* 1024 size-in-kb))))

(: base64-string-from-path (-> Path String))
(define (base64-string-from-path path)
  (bytes->string/utf-8 (base64-encode (file->bytes path))))