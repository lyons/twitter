#lang typed/racket/base

(require typed/rackunit
         (only-in racket/file
                  file->bytes
                  file->string))
(require "../header.rkt")

;; OAuth header ------------------------
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
              (file->string "data/rfc-example-1.txt"))

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
              (file->string "data/rfc-example-2.txt"))

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
              (file->string "data/rfc-example-3.txt"))

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
              (file->string "data/twitter-docs-example.txt"))

;; Multipart data ----------------------
(check-equal?
 (oauth-multipart-data `(,(Param "greeting" "hello, world")
                         ,(Param "purpose" "This is just test data"))
                       `(,(string->path "data/file1.dat")
                         ,(string->path "data/file2.dat")
                         ,(string->path "data/file3.dat"))
                       #"multipart_boundary_1408336976Urvbyj8FlBbn8syJyUdshXEdejz3qYPVJf5ePfuRXtg")
 (file->bytes "data/multipart-data.txt"))