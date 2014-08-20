#lang typed/racket/base

(provide (all-defined-out))

(require "../types.rkt"
         "type-helpers.rkt")

(require (only-in racket/string
                  string-join))
(require/typed net/base64
               [base64-encode (->* [Bytes] [Bytes] Bytes)])
(require/typed net/uri-codec
               [uri-unreserved-encode (-> String String)])
(require/typed web-server/stuffers/hmac-sha1
               [HMAC-SHA1 (-> Bytes Bytes Bytes)])

;; OAuth signature methods -------------
(define-type SigningKey String)
(define-type Signature String)

(: signature-base-string (-> HTTP-Method OAuth-URL HTTP-Parameter-List HTTP-Parameter-List String))
(define signature-base-string
  (λ (http-method url oauth-params data)
    (define param-list (append (OAuth-URL-query url) oauth-params data))
    (define http-parameters
      (string-join (map parameter->string
                        (sort (map parameter-percent-encode param-list) parameter<?))
                   "&"))
    (string-join (map uri-unreserved-encode `(,(symbol->string http-method)
                                              ,(url->base-url url)
                                              ,http-parameters))
                 "&")))

(: signing-key (-> Client (Option Token) SigningKey))
(define (signing-key client token)
  (string-append (uri-unreserved-encode (Client-secret client))
                 "&"
                 (if token
                     (uri-unreserved-encode (Token-secret token))
                     "")))

(: sign-HMAC-SHA1 (-> SigningKey String Signature))
(define (sign-HMAC-SHA1 key base-string)
  (bytes->string/utf-8
   (base64-encode (HMAC-SHA1 (string->bytes/utf-8 key)
                             (string->bytes/utf-8 base-string))
                  #"")))

(: sign-plaintext (-> SigningKey Signature))
(define (sign-plaintext key)
  key)

