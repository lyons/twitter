#lang typed/racket/base

(provide help/configuration
         help/supported-languages
         help/privacy-policy
         help/terms-of-service
         application/rate-limit-status)

(require racket/match)
(require oauth/client)
(require "../types/types.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Help functions

(: help/configuration
   (->* []
        [#:client Client #:token Token]
        [U (HashTable Symbol JSON) Twitter-Error]))
(define help/configuration
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/help/configuration.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (json-hash x)]
           [else (response-error 'help/configuration x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: help/supported-languages
   (->* []
        [#:client Client #:token Token]
        [U (Listof (HashTable Symbol JSON)) Twitter-Error]))
(define help/supported-languages
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/help/languages.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map json-hash x)]
           [else (response-error 'help/supported-languages x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: help/privacy-policy
   (->* []
        [#:client Client #:token Token]
        [U String Twitter-Error]))
(define help/privacy-policy
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/help/privacy.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([y (hash-ref (json-hash x) 'privacy JSON-Null)])
                        (cond
                          [(string? y) y]
                          [else (response-error 'help/privacy-policy x)]))]
           [else (response-error 'help/privacy-policy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: help/terms-of-service
   (->* []
        [#:client Client #:token Token]
        [U String Twitter-Error]))
(define help/terms-of-service
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/help/tos.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([y (hash-ref (json-hash x) 'tos JSON-Null)])
                        (cond
                          [(string? y) y]
                          [else (response-error 'help/terms-of-service x)]))]
           [else (response-error 'help/terms-of-service x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: application/rate-limit-status
   (->* []
        [#:client Client #:token Token #:resources (Perhaps (Listof String))]
        [U (HashTable Symbol JSON) Twitter-Error]))
(define application/rate-limit-status
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:resources [resources (Nothing)])
    (define response
      (oauth/get (twitter/url "/1.1/application/rate_limit_status.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (json-hash x)]
           [else (response-error 'help/rate-limit-status x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
