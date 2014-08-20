#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Tweets

;(: tweet/contributors (-> Tweet (Perhaps Contributors)))
(: tweet/media-entities (-> Tweet (Listof Twitter-Media-Entity)))
(define (tweet/media-entities tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'media JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Media-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: tweet/mention-entities (-> Tweet (Listof Twitter-Mention-Entity)))
(define (tweet/mention-entities tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'user_mentions JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Mention-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: tweet/url-entities (-> Tweet (Listof Twitter-URL-Entity)))
(define (tweet/url-entities tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'urls JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-URL-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: tweet/symbol-entities (-> Tweet (Listof Twitter-Symbol-Entity)))
(define (tweet/symbol-entities tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'symbols JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Symbol-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: tweet/hashtag-entities (-> Tweet (Listof Twitter-Hashtag-Entity)))
(define (tweet/hashtag-entities tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'hashtags JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Hashtag-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: tweet/coördinates (-> Tweet (Perhaps Coördinates)))
(define (tweet/coördinates tweet)
  (match (hash-ref (Tweet-data tweet) 'coordinates JSON-Null)
    [(list long lat) #:when (and (inexact-real? long) (inexact-real? lat))
                     (Coördinates lat long)]
    [_ (Nothing)]))

(: tweet/created-at (-> Tweet String))
(define (tweet/created-at tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'created_at JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'tweet/created-at String x)])))

(: tweet/current-user-retweet (-> Tweet (Perhaps TweetID)))
(define (tweet/current-user-retweet tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'current_user_retweet JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'id JSON-Null)])
                   (cond
                     [(exact-integer? y) (TweetID y)]
                     [else (Nothing)]))]
      [else (Nothing)])))

(: tweet/favourite-count (-> Tweet (Perhaps Integer)))
(define (tweet/favourite-count tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'favorite_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (Nothing)])))

(: tweet/favourited? (-> Tweet (Perhaps Boolean)))
(define (tweet/favourited? tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'favorited JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (Nothing)])))

(: tweet/filter-level (-> Tweet (Perhaps String)))
(define (tweet/filter-level tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'filter_level JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: tweet/id (-> Tweet TweetID))
(define (tweet/id tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (TweetID x)]
      [else (unpack-error 'tweet/id TweetID x)])))

(: tweet/in-reply-to-status-id (-> Tweet (Perhaps TweetID)))
(define (tweet/in-reply-to-status-id tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'in_reply_to_status_id JSON-Null)])
    (cond
      [(exact-integer? x) (TweetID x)]
      [else (Nothing)])))

(: tweet/in-reply-to-username (-> Tweet (Perhaps Twitter-Username)))
(define (tweet/in-reply-to-username tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'in_reply_to_screen_name JSON-Null)])
    (cond
      [(string? x) (Twitter-Username x)]
      [else (Nothing)])))

(: tweet/in-reply-to-user-id (-> Tweet (Perhaps Twitter-UserID)))
(define (tweet/in-reply-to-user-id tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'in_reply_to_user_id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-UserID x)]
      [else (Nothing)])))

(: tweet/lang (-> Tweet (Perhaps String)))
(define (tweet/lang tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'lang JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: tweet/place (-> Tweet (Perhaps Twitter-Place)))
(define (tweet/place tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'place JSON-Null)])
    (cond
      [(hash? x) (Twitter-Place (json-hash x))]
      [else (Nothing)])))

(: tweet/possibly-sensitive? (-> Tweet (Perhaps Boolean)))
(define (tweet/possibly-sensitive? tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'possibly_sensitive JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (Nothing)])))

(: tweet/retweet-count (-> Tweet Integer))
(define (tweet/retweet-count tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'retweet_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'tweet/retweet-count Integer x)])))

(: tweet/retweeted? (-> Tweet (Perhaps Boolean)))
(define (tweet/retweeted? tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'retweeted JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (Nothing)])))

(: tweet/retweeted-status (-> Tweet (Perhaps Tweet)))
(define (tweet/retweeted-status tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'retweeted_status JSON-Null)])
    (cond
      [(hash? x) (Tweet (json-hash x))]
      [else (Nothing)])))

(: tweet/source (-> Tweet String))
(define (tweet/source tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'source JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'tweet/source String x)])))

(: tweet/text (-> Tweet String))
(define (tweet/text tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'text JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'tweet/text String x)])))

(: tweet/truncated? (-> Tweet Boolean))
(define (tweet/truncated? tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'truncated JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'tweet/truncated? Boolean x)])))

(: tweet/user (-> Tweet (U Twitter-User Twitter-UserID)))
(define (tweet/user tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'user JSON-Null)])
    (cond [(hash? x)
           (let ([y (hash-ref x 'screen_name JSON-Null)]
                 [z (hash-ref x 'id JSON-Null)])
             (cond
               [(string? y) (Twitter-User (json-hash x))] ; if x has username, must be full user
               [(exact-integer? z) (Twitter-UserID z)]    ; otherwise trim-user? was true, just an id
               [else (unpack-error 'tweet/user
                                   (U Twitter-User Twitter-UserID)
                                   x)]))]
          [else (unpack-error 'tweet/user (U Twitter-User Twitter-UserID) x)])))

(: tweet/withheld-copyright? (-> Tweet (Perhaps Boolean)))
(define (tweet/withheld-copyright? tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'withheld_copyright JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (Nothing)])))

(: tweet/withheld-in-countries (-> Tweet (Perhaps (Listof String))))
(define (tweet/withheld-in-countries tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'withheld_in_countries JSON-Null)])
    (cond
      [(list-of-string? x) x]
      [else (Nothing)])))

(: tweet/withheld-scope (-> Tweet (Perhaps String)))
(define (tweet/withheld-scope tweet)
  (let ([x (hash-ref (Tweet-data tweet) 'withheld_scope JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))
