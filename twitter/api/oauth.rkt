#lang typed/racket/base

(provide oauth/request-token
         oauth/authorise-url
         oauth/authenticate-url
         oauth/access-token
         oauth/access-token-x-auth)

(require racket/match
         (only-in racket/port
                  port->string)
         (only-in racket/string
                  string-split))
(require (only-in typed/net/url
                  [url URL]
                  url?
                  string->url
                  url->string))
(require oauth/client)
(require "../types/types.rkt"
         "../types/type-helpers.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; OAuth 1.0A

(: oauth/request-token
   (->* [(U URL 'oob)]
        [#:client Client #:x-auth-access-type (Perhaps (U 'read 'write))]
        [U Token Twitter-Error]))
(define oauth/request-token
  (λ (callback
      #:client [client (current-client)]
      #:x-auth-access-type [x-auth (Nothing)])
    (define response
      (oauth/request 'POST
                     (twitter/url "/oauth/request_token")
                     client
                     #:other-oauth-fields `(,(Param "oauth_callback" (if (url? callback)
                                                                         (url->string callback)
                                                                         (symbol->string callback)))
                                            ,@(perhaps-param "x_auth_access_type"
                                                                x-auth
                                                                symbol->string))))
     (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let* ([x (port->string body)]
              [y (string-split x "&")]
              [z (map (λ ([s : String]) (string-split s "=")) y)])
         (match z
           [(list-no-order `("oauth_token" ,token) `("oauth_token_secret" ,secret)
                           `("oauth_callback_confirmed" "true"))
            #:when (and (string? token) (string? secret))
            (Token token secret)]
           [_ (response-error 'oauth/request-token x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: oauth/authorise-url
   (->* [Token]
        [#:force-login? (Perhaps Boolean) #:username (Perhaps Twitter-Username)]
        URL))
(define oauth/authorise-url
  (λ (token
      #:force-login? [force-login? (Nothing)]
      #:username [username (Nothing)])
    (string->url
     (apply string-append
            `("https://api.twitter.com/oauth/authorize?oauth_token="
              ,(Token-key token)
              ,(if (boolean? force-login?)
                   (string-append "&force_login=" (bool->string force-login?))
                   "")
              ,(if (Twitter-Username? username)
                   (string-append "&screen_name=" (user*->string username))
                   ""))))))

(: oauth/authenticate-url
   (->* [Token]
        [#:force-login? (Perhaps Boolean) #:username (Perhaps Twitter-Username)]
        URL))
(define oauth/authenticate-url
  (λ (token
      #:force-login? [force-login? (Nothing)]
      #:username [username (Nothing)])
    (string->url
     (apply string-append
            `("https://api.twitter.com/oauth/authenticate?oauth_token="
              ,(Token-key token)
              ,(if (boolean? force-login?)
                   (string-append "&force_login=" (bool->string force-login?))
                   "")
              ,(if (Twitter-Username? username)
                   (string-append "&screen_name=" (user*->string username))
                   ""))))))

(: oauth/access-token
   (->* [Token String]
        [#:client Client]
        [U (List Token Twitter-Username Twitter-UserID) Twitter-Error]))
(define oauth/access-token
  (λ (token
      verifier
      #:client [client (current-client)])
    (define response
      (oauth/request 'POST
                     (twitter/url "/oauth/access_token")
                     client
                     #:token token
                     #:other-oauth-fields `(,(Param "oauth_verifier" verifier))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let* ([x (port->string body)]
              [y (string-split x "&")]
              [z (map (λ ([s : String]) (string-split s "=")) y)])
         (match z
           [(list-no-order `("oauth_token" ,token) `("oauth_token_secret" ,secret)
                           `("user_id" ,id) `("screen_name" ,name))
            #:when (and (string? token) (string? secret) (string? id) (string? name))
            (let ([id-num (string->number id)])
              (begin (assert id-num exact-integer?)
                     `(,(Token token secret)
                       ,(Twitter-Username name)
                       ,(Twitter-UserID id-num))))]
           [_ (response-error 'oauth/access-token x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: oauth/access-token-x-auth
   (->* [Token #:x-auth-username Twitter-Username #:x-auth-password String]
        [#:client Client]
        [U (List Token Twitter-Username Twitter-UserID) Twitter-Error]))
(define oauth/access-token-x-auth
  (λ (token
      #:x-auth-username username
      #:x-auth-password password
      #:client [client (current-client)])
    (define response
      (oauth/request 'POST
                     (twitter/url "/oauth/access_token")
                     client
                     #:token token
                     #:other-oauth-fields `(,(Param "x_auth_username" (user*->string username))
                                            ,(Param "x_auth_password" password)
                                            ,(Param "x_auth_mode" "client_auth"))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let* ([x (port->string body)]
              [y (string-split x "&")]
              [z (map (λ ([s : String]) (string-split s "=")) y)])
         (match z
           [(list-no-order `("oauth_token" ,token) `("oauth_token_secret" ,secret)
                           `("user_id" ,id) `("screen_name" ,name))
            #:when (and (string? token) (string? secret) (string? id) (string? name))
            (let ([id-num (string->number id)])
              (begin (assert id-num exact-integer?)
                     `(,(Token token secret)
                       ,(Twitter-Username name)
                       ,(Twitter-UserID id-num))))]
           [_ (response-error 'oauth/access-token-x-auth x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
