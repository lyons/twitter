#lang typed/racket/base

(provide (except-out (all-defined-out)
                     indices?))

(require racket/match
         (only-in racket/function
                  identity)
         (only-in typed/net/url
                  [url URL]
                  string->url))
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Hashtags

(: hashtag-entity/indices (-> Twitter-Hashtag-Entity (List Integer Integer)))
(define (hashtag-entity/indices hashtag)
  (let ([x (hash-ref (Twitter-Hashtag-Entity-data hashtag) 'indices JSON-Null)])
       (cond
         [(indices? x) x]
         [else (unpack-error 'hashtag-entity/indices (List Integer Integer) x)])))

(: hashtag-entity/text (-> Twitter-Hashtag-Entity String))
(define (hashtag-entity/text hashtag)
  (let ([x (hash-ref (Twitter-Hashtag-Entity-data hashtag) 'text JSON-Null)])
       (cond
         [(string? x) x]
         [else (unpack-error 'hashtag-entity/text String x)])))

;; ---------------------------------------------------------------------------------------------------
;; Media

(: media-entity/display-url (-> Twitter-Media-Entity URL))
(define (media-entity/display-url media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'display_url JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'media-entity/display-url URL x)])))

(: media-entity/expanded-url (-> Twitter-Media-Entity URL))
(define (media-entity/expanded-url media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'expanded_url JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'media-entity/expanded-url URL x)])))

(: media-entity/id (-> Twitter-Media-Entity String))
(define (media-entity/id media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'id JSON-Null)])
       (cond
         [(string? x) x]
         [else (unpack-error 'media-entity/id String x)])))

(: media-entity/media-url (->* [Twitter-Media-Entity] [#:ssl? Boolean] URL))
(define media-entity/media-url
  (λ (media
      #:ssl? [ssl? #t])
    (let* ([key (if ssl? 'media_url_https 'media_url)]
           [x (hash-ref (Twitter-Media-Entity-data media) key JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'media-entity/media-url URL x)]))))

(: media-entity/sizes (-> Twitter-Media-Entity (HashTable Symbol JSON)))
(define (media-entity/sizes media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'sizes JSON-Null)])
       (cond
         [(hash? x) (json-hash x)]
         [else (unpack-error 'media-entity/sizes (HashTable Symbol JSON) x)])))

(: media-entity/source-id (-> Twitter-Media-Entity (Perhaps TweetID)))
(define (media-entity/source-id media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'source_status_id JSON-Null)])
       (cond
         [(exact-integer? x) (TweetID x)]
         [else (unpack-error 'media-entity/source-id TweetID x)])))

(: media-entity/type (-> Twitter-Media-Entity String))
(define (media-entity/type media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'type JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'media-entity/type String x)])))

(: media-entity/url (-> Twitter-Media-Entity URL))
(define (media-entity/url media)
  (let ([x (hash-ref (Twitter-Media-Entity-data media) 'url JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'media-entity/url URL x)])))

;; ---------------------------------------------------------------------------------------------------
;; Mentions

(: mention-entity/display-name (-> Twitter-Mention-Entity String))
(define (mention-entity/display-name mention)
  (let ([x (hash-ref (Twitter-Mention-Entity-data mention) 'name JSON-Null)])
       (cond
         [(string? x) x]
         [else (unpack-error 'mention-entity/display-name String x)])))

(: mention-entity/id (-> Twitter-Mention-Entity Twitter-UserID))
(define (mention-entity/id mention)
  (let ([x (hash-ref (Twitter-Mention-Entity-data mention) 'id JSON-Null)])
       (cond
         [(exact-integer? x) (Twitter-UserID x)]
         [else (unpack-error 'mention-entity/id Twitter-UserID x)])))

(: mention-entity/indices (-> Twitter-Mention-Entity (List Integer Integer)))
(define (mention-entity/indices mention)
  (let ([x (hash-ref (Twitter-Mention-Entity-data mention) 'indices JSON-Null)])
       (cond
         [(indices? x) x]
         [else (unpack-error 'mention-entity/indices (List Integer Integer) x)])))

(: mention-entity/name (-> Twitter-Mention-Entity Twitter-Username))
(define (mention-entity/name mention)
  (let ([x (hash-ref (Twitter-Mention-Entity-data mention) 'screen_name JSON-Null)])
       (cond
         [(string? x) (Twitter-Username x)]
         [else (unpack-error 'mention-entity/name Twitter-Username x)])))

;; ---------------------------------------------------------------------------------------------------
;; Symbols

(: symbol-entity/indices (-> Twitter-Symbol-Entity (List Integer Integer)))
(define (symbol-entity/indices sym)
  (let ([x (hash-ref (Twitter-Symbol-Entity-data sym) 'indices JSON-Null)])
       (cond
         [(indices? x) x]
         [else (unpack-error 'symbol-entity/indices (List Integer Integer) x)])))

(: symbol-entity/text (-> Twitter-Symbol-Entity String))
(define (symbol-entity/text sym)
  (let ([x (hash-ref (Twitter-Symbol-Entity-data sym) 'text JSON-Null)])
       (cond
         [(string? x) x]
         [else (unpack-error 'symbol-entity/text String x)])))

;; ---------------------------------------------------------------------------------------------------
;; URLs

(: url-entity/display-url (-> Twitter-URL-Entity URL))
(define (url-entity/display-url url)
  (let ([x (hash-ref (Twitter-URL-Entity-data url) 'display_url JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'url-entity/display-url URL x)])))

(: url-entity/expanded-url (-> Twitter-URL-Entity URL))
(define (url-entity/expanded-url url)
  (let ([x (hash-ref (Twitter-URL-Entity-data url) 'expanded_url JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'url-entity/expanded-url URL x)])))

(: url-entity/indices (-> Twitter-URL-Entity (List Integer Integer)))
(define (url-entity/indices url)
  (let ([x (hash-ref (Twitter-URL-Entity-data url) 'indices JSON-Null)])
       (cond
         [(indices? x) x]
         [else (unpack-error 'url-entity/indices (List Integer Integer) x)])))

(: url-entity/url (-> Twitter-URL-Entity URL))
(define (url-entity/url url)
  (let ([x (hash-ref (Twitter-URL-Entity-data url) 'url JSON-Null)])
       (cond
         [(string? x) (string->url x)]
         [else (unpack-error 'url-entity/url URL x)])))

;; ---------------------------------------------------------------------------------------------------
;; Helper functions

(define-predicate indices? (List Integer Integer))