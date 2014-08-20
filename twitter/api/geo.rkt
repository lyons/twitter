#lang typed/racket/base

(provide geo/show-place
         geo/reverse-geocode
         geo/search
         geo/similar-places)

(require racket/match)
(require "../oauth/oauth.rkt")
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Geolocation

(: geo/show-place
   (->* [Twitter-PlaceID]
        [#:client Client #:token Token]
        [U Twitter-Place Twitter-Error]))
(define geo/show-place
  (λ (place
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url (string-append "/1.1/geo/id/" (placeid->string place) ".json"))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-Place (json-hash x))]
           [else (response-error 'geo/show-place x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: geo/reverse-geocode
 (->* [Coördinates]
      [#:client Client #:token Token #:radius (Perhaps Natural)
                #:granularity (Perhaps (U 'poi 'neighbourhood 'city 'admin 'country))
                #:max-results (Perhaps Integer)  #:callback (Perhaps String)]
      [U (Listof Twitter-Place) Twitter-Error]))
(define geo/reverse-geocode
  (λ (coördinates
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:radius [radius (Nothing)]
      #:granularity [granularity (Nothing)]
      #:max-results [max-results (Nothing)]
      #:callback [callback (Nothing)])
    (define ~granularity (if (eq? granularity 'neighbourhood) 'neighborhood granularity))
    (define ~max-results (enforce-range max-results 1 20))
    (define response
      (oauth/get (twitter/url "/1.1/geo/reverse_geocode.json"
                              `(,@(coördinates->perhaps-params coördinates)
                                ,@(perhaps-param "accuracy" radius number->string)
                                ,@(perhaps-param "granularity" ~granularity symbol->string)
                                ,@(perhaps-param "max_results" ~max-results number->string)
                                ,@(perhaps-param "callback" callback string-identity)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body) ;; This ugly chunk of code omits the search metadata
       (define x (read-json body))
       (assert x hash?)
       (define y (hash-ref (json-hash x) 'result JSON-Null))
       (assert y hash?)
       (define z (hash-ref (json-hash y) 'places JSON-Null))
       (assert z list-of-hash?)
       (map (compose Twitter-Place json-hash) z)]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: geo/search
   (->* [(U Coördinates String)]
        [#:client Client #:token Token
                  #:granularity (Perhaps (U 'poi 'neighbourhood 'city 'admin 'country))
                  #:accuracy (Perhaps Natural) #:max-results (Perhaps Integer)
                  #:contained-within (Perhaps Twitter-PlaceID) #:street-address (Perhaps String)
                  #:callback (Perhaps String)]
        [U (Listof Twitter-Place) Twitter-Error]))
(define geo/search
  (λ (search
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:granularity [granularity (Nothing)]
      #:accuracy [accuracy (Nothing)]
      #:max-results [max-results (Nothing)]
      #:contained-within [within (Nothing)]
      #:street-address [address (Nothing)]
      #:callback [callback (Nothing)])
    (define search-params
      (cond
        [(Coördinates? search) (coördinates->perhaps-params search)]
        [else (if (ipv4? search)
                  `(,(Param "ip" search))
                  `(,(Param "query" search)))]))
    (define ~granularity (if (eq? granularity 'neighbourhood) 'neighborhood granularity))
    (define ~max-results (enforce-range max-results 1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/geo/search.json"
                              `(,@search-params
                                ,@(perhaps-param "granularity" ~granularity symbol->string)
                                ,@(perhaps-param "accuracy" accuracy number->string)
                                ,@(perhaps-param "max_results" ~max-results number->string)
                                ,@(perhaps-param "contained_within" within placeid->string)
                                ,@(perhaps-param "attribute:street_address" address string-identity)
                                ,@(perhaps-param "callback" callback string-identity)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body) ;; This ugly chunk of code omits the search metadata
       (define x (read-json body))
       (assert x hash?)
       (define y (hash-ref (json-hash x) 'result JSON-Null))
       (assert y hash?)
       (define z (hash-ref (json-hash y) 'places JSON-Null))
       (assert z list-of-hash?)
       (map (compose Twitter-Place json-hash) z)]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: geo/similar-places
   (->* [Coördinates String]
        [#:client Client #:token Token #:contained-within (Perhaps Twitter-PlaceID)
                  #:street-address (Perhaps String) #:callback (Perhaps String)]
        [U (Listof Twitter-Place) Twitter-Error]))
(define geo/similar-places
  (λ (coördinates
      name
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:contained-within [within (Nothing)]
      #:street-address [address (Nothing)]
      #:callback [callback (Nothing)])
    (define coörds-params
      (match coördinates
        [(Coördinates lat long) `(,(Param "lat" (number->string lat))
                                  ,(Param "long" (number->string long)))]))
    (define response
      (oauth/get (twitter/url "/1.1/geo/similar_places.json"
                              `(,@(coördinates->perhaps-params coördinates)
                                ,(Param "name" name)
                                ,@(perhaps-param "contained_within" within placeid->string)
                                ,@(perhaps-param "attribute:street_address" address string-identity)
                                ,@(perhaps-param "callback" callback string-identity)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body) ;; This ugly chunk of code omits the search metadata
       (define x (read-json body))
       (assert x hash?)
       (define y (hash-ref (json-hash x) 'result JSON-Null))
       (assert y hash?)
       (define z (hash-ref (json-hash y) 'places JSON-Null))
       (assert z list-of-hash?)
       (map (compose Twitter-Place json-hash) z)]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; Internal helpers

(: ipv4? (-> String Boolean))
(define (ipv4? str)
  ;; IP matching regexes borrowed from http://docs.racket-lang.org/guide/An_Extended_Example.html
  (define n0-255
    (string-append
     "(?:"
     "\\d|"        ;   0 through 9
     "\\d\\d|"     ;  00 through 99
     "[01]\\d\\d|" ; 000 through 199
     "2[0-4]\\d|"  ; 200 through 249
     "25[0-5]"     ; 250 through 255
     ")"))
  (define ip-re1
    (string-append
     "^"        ; nothing before
     n0-255     ; the first n0-255,
     "(?:"      ; then the subpattern of
     "\\."      ; a dot followed by
     n0-255     ; an n0-255,
     ")"        ; which is
     "{3}"      ; repeated exactly 3 times
     "$"))
  (define ip-re
    (pregexp
     (string-append
       "(?![0.]*$)" ; not just zeros and dots
                    ; (note: . is not metachar inside [...])
       ip-re1)))

  (if (regexp-match ip-re str) ; this looks silly
      #t                       ; but regexp-match returns matched string rather than #t
      #f))
