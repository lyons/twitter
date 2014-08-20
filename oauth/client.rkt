#lang typed/racket/base

(provide (all-defined-out))

(require "header.rkt"
         "types.rkt"
         "parameters.rkt"
         "private/http-client.rkt"
         "private/multipart.rkt"
         "private/type-helpers.rkt")
(provide (all-from-out "types.rkt")
         (all-from-out "parameters.rkt"))

(require racket/match)

(: oauth/get (->* [OAuth-URL Client Token]
                  [#:signing-method Signing-Method #:oauth-realm (Option String) #:accept String]
                  HTTP-Response))
(define oauth/get
  (λ (url
      client
      token
      #:signing-method [signing-method 'HMAC-SHA1]
      #:oauth-realm [oauth-realm #f]
      #:accept [accept (oauth-accept)])
    (oauth/request 'GET
                   url
                   client
                   #:token token
                   #:signing-method signing-method
                   #:oauth-realm oauth-realm
                   #:accept accept)))

(: oauth/post (->* [OAuth-URL Client Token]
                   [#:data HTTP-Parameter-List #:signing-method Signing-Method
                           #:oauth-realm (Option String) #:accept String]
                   HTTP-Response))
(define oauth/post
  (λ (url
      client
      token
      #:data [data '()]
      #:signing-method [signing-method 'HMAC-SHA1]
      #:oauth-realm [oauth-realm #f]
      #:accept [accept (oauth-accept)])
    (oauth/request 'POST
                   url
                   client
                   #:token token
                   #:data data
                   #:signing-method signing-method
                   #:oauth-realm oauth-realm
                   #:accept accept)))

(: oauth/put (->* [OAuth-URL Client Token]
                  [#:data HTTP-Parameter-List #:signing-method Signing-Method
                          #:oauth-realm (Option String) #:accept String]
                  HTTP-Response))
(define oauth/put
  (λ (url
      client
      token
      #:data [data '()]
      #:signing-method [signing-method 'HMAC-SHA1]
      #:oauth-realm [oauth-realm #f]
      #:accept [accept (oauth-accept)])
    (oauth/request 'PUT
                   url
                   client
                   #:token token
                   #:data data
                   #:signing-method signing-method
                   #:oauth-realm oauth-realm
                   #:accept accept)))

(: oauth/delete (->* [OAuth-URL Client Token]
                     [#:signing-method Signing-Method #:oauth-realm (Option String) #:accept String]
                     HTTP-Response))
(define oauth/delete
  (λ (url
      client
      token
      #:signing-method [signing-method 'HMAC-SHA1]
      #:oauth-realm [oauth-realm #f]
      #:accept [accept (oauth-accept)])
    (oauth/request 'DELETE
                   url
                   client
                   #:token token
                   #:signing-method signing-method
                   #:oauth-realm oauth-realm
                   #:accept accept)))

(: oauth/request (->* [HTTP-Method OAuth-URL Client]
                      [#:token (Option Token) #:signing-method Signing-Method
                               #:data HTTP-Parameter-List #:other-oauth-fields HTTP-Parameter-List
                               #:oauth-realm (Option String) #:accept String]
                      HTTP-Response))
(define oauth/request
  (λ (http-method
      url
      client
      #:token [token #f]
      #:signing-method [signing-method 'HMAC-SHA1]
      #:data [data '()]
      #:other-oauth-fields [other-fields '()]
      #:oauth-realm [oauth-realm #f]
      #:accept [accept (oauth-accept)])
    (define auth
      (oauth-header http-method
                    url
                    #:client client
                    #:token token
                    #:signing-method signing-method
                    #:data data
                    #:other-oauth-fields other-fields
                    #:oauth-realm oauth-realm))
    (define headers `(,(string-append "Accept: " accept)
                      ,(string-append "Twitter-User-Agent: " (oauth-user-agent))
                      ,@(match http-method
                          [(or 'POST 'PUT) '("Content-Type: application/x-www-form-urlencoded")]
                          [_ '()])
                      ,auth))
    (define-values (status response-headers response-body)
      (http-sendrecv (OAuth-URL-host url)
                     (url->path+query url)
                     #:ssl? (url-ssl? url)
                     #:method http-method
                     #:headers headers
                     #:data (parameterlist->string data)))
    (HTTP-Response status response-headers response-body)))

(: oauth/post-multipart-data
   (->* [OAuth-URL Client Token #:data HTTP-Parameter-List #:files (Listof Path)]
        [#:signing-method Signing-Method #:ssl? Boolean #:accept String]
        HTTP-Response))
(define oauth/post-multipart-data
  (λ (url
      client
      token
      #:data data
      #:files files
      #:signing-method [signing-method 'HMAC-SHA1]
      #:ssl? [ssl? #t]
      #:oauth-realm [oauth-realm #f]
      #:accept [accept (oauth-accept)])
    (define auth
      (oauth-header 'POST
                    url
                    #:client client
                    #:token token
                    #:signing-method signing-method
                    #:data '()
                    #:other-oauth-fields '()
                    #:oauth-realm oauth-realm))
    (define boundary (multipart-boundary))
    (define headers `(,(string-append "Accept: " accept)
                      ,(string-append "Twitter-User-Agent: " (oauth-user-agent))
                      ,(string-append "Content-Type: multipart/form-data;boundary="
                                      (bytes->string/utf-8 boundary))
                      ,auth))
    (define body (oauth-multipart-data data files boundary))
    (define-values (status response-headers response-body)
      (http-sendrecv (OAuth-URL-host url)
                     (url->path+query url)
                     #:ssl? (url-ssl? url)
                     #:method 'POST
                     #:headers headers
                     #:data body))
    (HTTP-Response status response-headers response-body)))