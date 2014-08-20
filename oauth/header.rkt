#lang typed/racket/base

(provide oauth-header
         oauth-multipart-data)

(require "types.rkt"
         "parameters.rkt"
         "private/header-helpers.rkt"
         "private/multipart.rkt"
         "private/signature.rkt"
         "private/type-helpers.rkt")
(provide (all-from-out "types.rkt")
         (all-from-out "parameters.rkt"))

(require racket/match
         (only-in racket/string
                  string-join
                  string-replace))
(require/typed net/uri-codec
               [uri-unreserved-encode (-> String String)])

(: oauth-header
   (->* [HTTP-Method OAuth-URL #:client Client #:token (Option Token) #:signing-method Signing-Method
                    #:data HTTP-Parameter-List #:other-oauth-fields HTTP-Parameter-List]
        [#:oauth-realm (Option String) #:timestamp-method (-> String) #:nonce-method (-> String)]
        String))
(define oauth-header
  (λ (http-method
      url
      #:client client
      #:token token
      #:signing-method signing-method
      #:data data
      #:other-oauth-fields other-fields
      #:oauth-realm [realm #f]
      #:timestamp-method [timestamp oauth-timestamp]
      #:nonce-method [nonce oauth-nonce])
    (define base-params
      (append `(,(Param "oauth_consumer_key" (Client-key client))
                ,(Param "oauth_nonce"  (nonce))
                ,(Param "oauth_signature_method" (symbol->string signing-method))
                ,(Param "oauth_timestamp" (timestamp))
                ,(Param "oauth_version" "1.0"))
              (~token->spliceable-oauth-parameter token)
              other-fields))
    (define signature
      (Param "oauth_signature"
             (match signing-method
               ['HMAC-SHA1 (sign-HMAC-SHA1 (signing-key client token)
                                           (signature-base-string http-method
                                                                  url
                                                                  base-params
                                                                  data))]
               ['plaintext (sign-plaintext (signing-key client token))])))
    (define params (sort (cons signature base-params) parameter<?))
    (define header (string-append "Authorization: OAuth "
                                  (if realm
                                      (string-append "realm=\"" (uri-unreserved-encode realm) "\", ")
                                      "")
                                  (parameterlist->string params
                                                         #:transformer parameter->quoted-string
                                                         #:joiner ", ")))
    header))

(: oauth-multipart-data (-> (Listof HTTP-Parameter) (Listof Path) Bytes Bytes))
(define (oauth-multipart-data params paths boundary)
  (apply bytes-append
         `(,@(map (λ ([param : HTTP-Parameter])
                    (param->multipart-data param boundary))
                  params)
           ,@(map (λ ([path : Path])
                    (file->multipart-data path boundary))
                  paths)
           ,(bytes-append #"--" boundary #"--"))))
