#lang typed/racket/base

(provide (except-out (all-defined-out)
                     list-list-of-pair-of-float?))

(require racket/match
         (only-in typed/net/url
                  [url URL]
                  string->url))
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Places

(: place/attributes (-> Twitter-Place
                        (U 'iso3 'locality 'phone 'postal_code 'region 'street_address 'twitter 'url)
                        (Perhaps String)))
(define (place/attributes place attribute)
  (let ([x (hash-ref (Twitter-Place-data place) 'attributes JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref (json-hash x) attribute JSON-Null)])
                   (cond
                     [(string? y) y]
                     [else (Nothing)]))]
      [else (Nothing)])))

(: place/bounding-box (-> Twitter-Place (Listof Coördinates)))
(define (place/bounding-box place)
  (let ([x (hash-ref (Twitter-Place-data place) 'bounding-box JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref (json-hash x) 'coordinates JSON-Null)])
                   (cond
                     [(list-list-of-pair-of-float? y)
                      (map (λ ([ls : (List Inexact-Real Inexact-Real)])
                             (match ls [`(,lat ,long) (Coördinates lat long)]))
                           (car y))]
                     [else (unpack-error 'place/bounding-box (Listof Coördinates) y)]))]
      [else (unpack-error 'place/bounding-box (HashTable Symbol JSON) x)])))

(: place/country (-> Twitter-Place String))
(define (place/country place)
  (let ([x (hash-ref (Twitter-Place-data place) 'country JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'place/country String x)])))

(: place/country-code (-> Twitter-Place String))
(define (place/country-code place)
  (let ([x (hash-ref (Twitter-Place-data place) 'country_code JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'place/country-code String x)])))

(: place/full-name (-> Twitter-Place String))
(define (place/full-name place)
  (let ([x (hash-ref (Twitter-Place-data place) 'full_name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'place/full-name String x)])))

(: place/id (-> Twitter-Place Twitter-PlaceID))
(define (place/id place)
  (let ([x (hash-ref (Twitter-Place-data place) 'id JSON-Null)])
    (cond
      [(string? x) (Twitter-PlaceID x)]
      [else (unpack-error 'place/id Twitter-PlaceID x)])))

(: place/name (-> Twitter-Place String))
(define (place/name place)
  (let ([x (hash-ref (Twitter-Place-data place) 'name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'place/name String x)])))

(: place/type (-> Twitter-Place String))
(define (place/type place)
  (let ([x (hash-ref (Twitter-Place-data place) 'type JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'place/type String x)])))

(: place/url (-> Twitter-Place URL))
(define (place/url place)
  (let ([x (hash-ref (Twitter-Place-data place) 'url JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (unpack-error 'place/url URL x)])))

;; ---------------------------------------------------------------------------------------------------
;; Helper functions

;; (╯°□°)╯︵ ┻━┻
(define-predicate list-list-of-pair-of-float? (List (Listof (List Inexact-Real Inexact-Real))))