#lang typed/racket/base

(provide (all-defined-out))

(require "../types.rkt")

(require racket/match
         (only-in racket/string
                  string-join))
(require/typed net/uri-codec
               [uri-unreserved-encode (-> String String)])

;; HTTP-Parameter helper methods -------
(: parameter<? (-> HTTP-Parameter HTTP-Parameter Boolean))
(define (parameter<? p1 p2)
  (match-let ([(HTTP-Parameter key1 value1) p1]
              [(HTTP-Parameter key2 value2) p2])
    (cond
      [(string<? key1 key2) #t]
      [(string=? key1 key2) (string<? value1 value2)]
      [else #f])))

(: parameter-percent-encode (-> HTTP-Parameter HTTP-Parameter))
(define (parameter-percent-encode p)
  (Param (uri-unreserved-encode (HTTP-Parameter-key p))
         (uri-unreserved-encode (HTTP-Parameter-value p))))

(: parameter->string (-> HTTP-Parameter String))
(define (parameter->string p)
  (match p
    [(HTTP-Parameter key "") key]
    [(HTTP-Parameter key value) (string-append key "=" value)]))

(: parameter->quoted-string (-> HTTP-Parameter String))
(define (parameter->quoted-string p)
  (match p
    [(HTTP-Parameter key "") key]
    [(HTTP-Parameter key value) (string-append key "=\"" value "\"")]))

;; HTTP-Parameter-List helper methods --
(: parameterlist->string
   (->* [HTTP-Parameter-List]
        [#:transformer (-> HTTP-Parameter String) #:joiner String #:percent-encode? Boolean]
        String))
(define parameterlist->string
  (λ (plist
      #:transformer [transformer parameter->string]
      #:joiner [joiner "&"]
      #:percent-encode? [percent-encode? #t])
    (define f (if percent-encode?
                  (compose transformer parameter-percent-encode)
                  transformer))
    (string-join (map f plist) joiner)))

;; URL helper methods ------------------
(: url-ssl? (-> OAuth-URL Boolean))
(define (url-ssl? url)
  (eq? (OAuth-URL-protocol url) 'https))

(: url->path+query (-> OAuth-URL String))
(define (url->path+query url)
  (if (null? (OAuth-URL-query url))
      (OAuth-URL-path url)
      (string-append (OAuth-URL-path url) "?" (parameterlist->string (OAuth-URL-query url)))))

(: url->base-url (-> OAuth-URL String))
(define (url->base-url url)
  (let ([protocol (case (OAuth-URL-protocol url)
                    ['https "https://"]
                    ['http "http://"])])
    (string-append protocol
                   (OAuth-URL-host url)
                   (OAuth-URL-path url))))

;; Token helper methods ----------------
(: ~token->spliceable-oauth-parameter (-> (Option Token) HTTP-Parameter-List))
(define (~token->spliceable-oauth-parameter t)
  (if t
      `(,(Param "oauth_token" (Token-key t)))
      '()))
