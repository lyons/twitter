#lang typed/racket/base

(provide (struct-out Client) (struct-out Token)
         (struct-out OAuth-URL) (struct-out HTTP-Response)
         Param
         Signing-Method HTTP-Method HTTP-Parameter HTTP-Parameter-List
         oauth/get oauth/post oauth/put oauth/delete oauth/request
         oauth/post-multipart-data
         oauth-header)

(require racket/match
         (only-in racket/file
                  file->bytes)
         (only-in racket/path
                  file-name-from-path)
         (only-in racket/string
                  string-join
                  string-replace))
(require/typed net/base64
               [base64-encode (->* [Bytes] [Bytes] Bytes)])
(require/typed net/uri-codec
               [uri-unreserved-encode (-> String String)])
(require/typed web-server/stuffers/hmac-sha1
               [HMAC-SHA1 (-> Bytes Bytes Bytes)])
(require "http-client.rkt")

(module+ test
  (require typed/rackunit
           (only-in racket/file
                    file->string)))

;; ---------------------------------------------------------------------------------------------------
;; Parameters

(: oauth-pseudo-random-generator (Parameterof Pseudo-Random-Generator))
(define oauth-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)))

(: oauth-accept (Parameterof String))
(define oauth-accept
  (make-parameter "*/*"))

(: oauth-user-agent (Parameterof String))
(define oauth-user-agent
  (make-parameter "Racket OAuth Client"))

;; ---------------------------------------------------------------------------------------------------
;; Type Definitions

(struct Client
  ([key : String]
   [secret : String]))

(struct Token
  ([key : String]
   [secret : String]))

(struct OAuth-URL
  ([protocol : (U 'http 'https)]
   [host : String]
   [path : String]
   [query : HTTP-Parameter-List]))

(struct HTTP-Parameter
  ([key : String]
   [value : String]))
(define-type HTTP-Parameter-List (Listof HTTP-Parameter))

(: Param (->* [String] [String] HTTP-Parameter))
(define (Param key [value ""])
  (HTTP-Parameter key value))

(define-type Signing-Method (U 'HMAC-SHA1 'PLAINTEXT))
(define-type HTTP-Method (U 'GET 'POST 'PUT 'DELETE))

(struct HTTP-Response
  ([status : Bytes]
   [headers : (Listof Bytes)]
   [body : Input-Port]))

;; ---------------------------------------------------------------------------------------------------
;; Request API

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
    (define body (multipart-data data files boundary))
    (define-values (status response-headers response-body)
      (http-sendrecv (OAuth-URL-host url)
                     (url->path+query url)
                     #:ssl? (url-ssl? url)
                     #:method 'POST
                     #:headers headers
                     #:data body))
    (HTTP-Response status response-headers response-body)))

;; ---------------------------------------------------------------------------------------------------
;; Header generation

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

(module+ test
  (check-equal? (oauth-header 'POST
                              (OAuth-URL 'https "photos.example.net" "/initiate" '())
                              #:client (Client "dpf43f3p2l4k3l03" "kd94hf93k423kf44")
                              #:token #f
                              #:signing-method 'HMAC-SHA1
                              #:data '()
                              #:other-oauth-fields `(,(Param "oauth_callback"
                                                             "http://printer.example.com/ready"))
                              #:oauth-realm "Photos"
                              #:timestamp-method (λ () "137131200")
                              #:nonce-method (λ () "wIjqoS"))
                (file->string "test/rfc-example-1.txt"))
  (check-equal? (oauth-header 'POST
                              (OAuth-URL 'https "photos.example.net" "/token" '())
                              #:client (Client "dpf43f3p2l4k3l03" "kd94hf93k423kf44")
                              #:token (Token "hh5s93j4hdidpola" "")
                              #:signing-method 'HMAC-SHA1
                              #:data '()
                              #:other-oauth-fields `(,(Param "oauth_verifier" "hfdp7dh39dks9884"))
                              #:oauth-realm "Photos"
                              #:timestamp-method (λ () "137131201")
                              #:nonce-method (λ () "walatlh"))
                (file->string "test/rfc-example-2.txt"))
  (check-equal? (oauth-header 'GET
                              (OAuth-URL 'https
                                         "photos.example.net"
                                         "/photos?file=vacation.jpg&size=original"
                                         '())
                              #:client (Client "dpf43f3p2l4k3l03" "kd94hf93k423kf44")
                              #:token (Token "nnch734d00sl2jdk" "pfkkdhi9sl3r4s00")
                              #:signing-method 'HMAC-SHA1
                              #:data '()
                              #:other-oauth-fields '()
                              #:oauth-realm "Photos"
                              #:timestamp-method (λ () "137131202")
                              #:nonce-method (λ () "chapoH"))
                (file->string "test/rfc-example-3.txt"))
  (check-equal? (oauth-header 'POST
                              (OAuth-URL 'https
                                         "api.twitter.com"
                                         "/1/statuses/update.json"
                                         `(,(Param "include_entities" "true")))
                              #:client (Client "xvz1evFS4wEEPTGEFPHBog"
                                               "kAcSOqF21Fu85e7zjz7ZN2U4ZRhfV3WpwPAoE3Z7kBw")
                              #:token (Token "370773112-GmHxMAgYyLbNEtIKZeRNFsMKPR9EyMZeS9weJAEb"
                                             "LswwdoUaIvS8ltyTt5jkRh4J50vUPVVHtR2YPi5kE")
                              #:signing-method 'HMAC-SHA1
                              #:data `(,(Param "status"
                                               "Hello Ladies + Gentlemen, a signed OAuth request!"))
                              #:other-oauth-fields '()
                              #:timestamp-method (λ () "1318622958")
                              #:nonce-method (λ () "kYjzVBB8Y0ZFabxSWbWovY3uYSQ2pTgmZeNu2VS4cg"))
                (file->string "test/twitter-docs-example.txt")))

;; ---------------------------------------------------------------------------------------------------
;; Private definitions

(: n-random-bytes (-> Natural Bytes))
(define (n-random-bytes n)
  (list->bytes
   ; typed list->bytes expects (Listof Integer) rather than (Listof Bytes), which feels dodgy
   (let loop : (Listof Integer) ([count : Natural 0]
                                 [acc : (Listof Integer) '()])
     (if (= count n)
         acc
         (loop (+ count 1)
               (cons (random 256 (oauth-pseudo-random-generator)) acc))))))

(: oauth-nonce (-> String))
(define (oauth-nonce)
  (string-replace (bytes->string/utf-8 (base64-encode (n-random-bytes 32)))
                  #px"[^[:alnum:]]"
                  ""))

(module+ test
  ;; (Of course, from this it might simply be periodic with a period >= 2)
  (check-not-equal? (oauth-nonce)
                    (oauth-nonce)
                    "oauth-nonce should produce a unique value each time it is called"))

(: oauth-timestamp (-> String))
(define (oauth-timestamp)
  (number->string (current-seconds)))

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

;; Multipart data ----------------------
(: multipart-boundary (-> Bytes))
(define (multipart-boundary)
  (string->bytes/utf-8 (string-append "multipart_boundary_" (oauth-timestamp) (oauth-nonce))))

(: multipart-boundary-header (-> Bytes Bytes))
(define (multipart-boundary-header boundary)
  (bytes-append #"--" boundary #"\r\n"))

(: file->multipart-data (-> Path Bytes Bytes))
(define (file->multipart-data path boundary)
  (define filename (file-name-from-path path))
  (assert filename path-for-some-system?)
  (apply bytes-append
         `(,(multipart-boundary-header boundary)
           #"Content-Type: application/octet-stream\r\n"
           #"Content-Disposition: form-data; name=\"media[]\"; filename=\""
           ,(path->bytes filename) #"\"\r\n\r\n"
           ,(file->bytes path) #"\r\n")))

(: param->multipart-data (-> HTTP-Parameter Bytes Bytes))
(define (param->multipart-data param boundary)
  (define key   (string->bytes/utf-8 (HTTP-Parameter-key param)))
  (define value (string->bytes/utf-8 (HTTP-Parameter-value param)))
  (apply bytes-append
         `(,(multipart-boundary-header boundary)
           #"Content-Disposition: form-data; name=\"" ,key #"\"\r\n\r\n"
           ,value #"\r\n")))

(: multipart-data (-> (Listof HTTP-Parameter) (Listof Path) Bytes Bytes))
(define (multipart-data params paths boundary)
  (apply bytes-append
         `(,@(map (λ ([param : HTTP-Parameter])
                    (param->multipart-data param boundary))
                  params)
           ,@(map (λ ([path : Path])
                    (file->multipart-data path boundary))
                  paths)
           ,(bytes-append #"--" boundary #"--"))))

(module+ test
  (check-equal?
   (multipart-data `(,(Param "greeting" "hello, world")
                     ,(Param "purpose" "This is just test data"))
                   `(,(string->path "./test/file1.dat")
                     ,(string->path "./test/file2.dat")
                     ,(string->path "./test/file3.dat"))
                   #"multipart_boundary_1408336976Urvbyj8FlBbn8syJyUdshXEdejz3qYPVJf5ePfuRXtg")
   (file->bytes "test/multipart-data.txt")))

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
