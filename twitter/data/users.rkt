#lang typed/racket/base

(provide (all-defined-out))

(require (only-in typed/net/url
                  [url URL]
                  string->url))
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Users

(: user/contributors-enabled? (-> Twitter-User Boolean))
(define (user/contributors-enabled? user)
  (let ([x (hash-ref (Twitter-User-data user) 'contributors_enabled JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'user/contributors-enabled? Boolean x)])))

(: user/created-at (-> Twitter-User String))
(define (user/created-at user)
  (let ([x (hash-ref (Twitter-User-data user) 'created_at JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'user/created-at String x)])))

(: user/description (-> Twitter-User (Perhaps String)))
(define (user/description user)
  (let ([x (hash-ref (Twitter-User-data user) 'description JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: user/description-url-entities (-> Twitter-User (Listof Twitter-URL-Entity)))
(define (user/description-url-entities user)
  (let ([x (hash-ref (Twitter-User-data user) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'description JSON-Null)])
                   (cond
                     [(hash? y) (let ([z (hash-ref y 'urls JSON-Null)])
                                  (cond
                                    [(list-of-hash? y) (map (compose Twitter-URL-Entity json-hash) y)]
                                    [else '()]))]
                     [else '()]))]
      [else '()])))

(: user/display-name (-> Twitter-User String))
(define (user/display-name user)
  (let ([x (hash-ref (Twitter-User-data user) 'name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'user/display-name String x)])))

(: user/egg? (-> Twitter-User Boolean))
(define (user/egg? user)
  (let ([x (hash-ref (Twitter-User-data user) 'default_profile_image JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'user/egg? Boolean x)])))

(: user/favourites-count (-> Twitter-User Integer))
(define (user/favourites-count user)
  (let ([x (hash-ref (Twitter-User-data user) 'favourites_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'user/favourites-count Integer x)])))

(: user/follow-request-sent? (-> Twitter-User (Perhaps Boolean)))
(define (user/follow-request-sent? user)
  (let ([x (hash-ref (Twitter-User-data user) 'follow_request_sent JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (Nothing)])))

(: user/follower-count (-> Twitter-User Integer))
(define (user/follower-count user)
  (let ([x (hash-ref (Twitter-User-data user) 'followers_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'user/followers-count Integer x)])))

(: user/friend-count (-> Twitter-User Integer))
(define (user/friend-count user)
  (let ([x (hash-ref (Twitter-User-data user) 'friend_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'user/friend-count Integer x)])))

(: user/geo-enabled? (-> Twitter-User Boolean))
(define (user/geo-enabled? user)
  (let ([x (hash-ref (Twitter-User-data user) 'geo_enabled JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'user/geo-enabled? Boolean x)])))

(: user/id (-> Twitter-User Twitter-UserID))
(define (user/id user)
  (let ([x (hash-ref (Twitter-User-data user) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-UserID x)]
      [else (unpack-error 'user/id Twitter-UserID x)])))

(: user/lang (-> Twitter-User String))
(define (user/lang user)
  (let ([x (hash-ref (Twitter-User-data user) 'lang JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'user/lang String x)])))

(: user/listed-count (-> Twitter-User Integer))
(define (user/listed-count user)
  (let ([x (hash-ref (Twitter-User-data user) 'listed_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'user/listed-count Integer x)])))

(: user/location (-> Twitter-User (Perhaps String)))
(define (user/location user)
  (let ([x (hash-ref (Twitter-User-data user) 'location JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: user/name (-> Twitter-User Twitter-Username))
(define (user/name user)
  (let ([x (hash-ref (Twitter-User-data user) 'screen_name JSON-Null)])
    (cond
      [(string? x) (Twitter-Username x)]
      [else (unpack-error 'user/name Twitter-Username x)])))

(: user/protected? (-> Twitter-User Boolean))
(define (user/protected? user)
  (let ([x (hash-ref (Twitter-User-data user) 'protected JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'user/protected? Boolean x)])))

(: user/show-inline-media? (-> Twitter-User (Perhaps Boolean)))
(define (user/show-inline-media? user)
  (let ([x (hash-ref (Twitter-User-data user) 'show_all_inline_media JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (Nothing)])))

(: user/status (-> Twitter-User (Perhaps Tweet)))
(define (user/status user)
  (let ([x (hash-ref (Twitter-User-data user) 'status JSON-Null)])
    (cond
      [(hash? x) (Tweet (json-hash x))]
      [else (Nothing)])))

(: user/status-count (-> Twitter-User Integer))
(define (user/status-count user)
  (let ([x (hash-ref (Twitter-User-data user) 'statuses_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'user/status-count Integer x)])))

(: user/time-zone (-> Twitter-User (Perhaps String)))
(define (user/time-zone user)
  (let ([x (hash-ref (Twitter-User-data user) 'time_zone JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: user/translator? (-> Twitter-User Boolean))
(define (user/translator? user)
  (let ([x (hash-ref (Twitter-User-data user) 'is_translator JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'user/is-translator? Boolean x)])))

(: user/url (-> Twitter-User (Perhaps URL)))
(define (user/url user)
  (let ([x (hash-ref (Twitter-User-data user) 'url JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (Nothing)])))

(: user/url-entities (-> Twitter-User (Listof Twitter-URL-Entity)))
(define (user/url-entities user)
  (let ([x (hash-ref (Twitter-User-data user) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'url JSON-Null)])
                   (cond
                     [(hash? y) (let ([z (hash-ref y 'urls JSON-Null)])
                                  (cond
                                    [(list-of-hash? y) (map (compose Twitter-URL-Entity json-hash) y)]
                                    [else '()]))]
                     [else '()]))]
      [else '()])))

(: user/utc-offset (-> Twitter-User (Perhaps String)))
(define (user/utc-offset user)
  (let ([x (hash-ref (Twitter-User-data user) 'utc_offset JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: user/verified? (-> Twitter-User Boolean))
(define (user/verified? user)
  (let ([x (hash-ref (Twitter-User-data user) 'verified JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'user/verified? Boolean x)])))

(: user/withheld-in-countries (-> Twitter-User (Perhaps String)))
(define (user/withheld-in-countries user)
  (let ([x (hash-ref (Twitter-User-data user) 'withheld-in-countries JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

(: user/withheld-scope (-> Twitter-User (Perhaps String)))
(define (user/withheld-scope user)
  (let ([x (hash-ref (Twitter-User-data user) 'withheld_scope JSON-Null)])
    (cond
      [(string? x) x]
      [else (Nothing)])))

;; ---------------------------------------------------------------------------------------------------
;; User profiles

(: profile/avatar-url (-> Twitter-User [#:ssl? Boolean] URL))
(define profile/avatar-url
  (λ (user
      #:ssl? [ssl? #t])
    (let* ([key (if ssl? 'profile_image_url_https 'profile_image_url)]
           [x (hash-ref (Twitter-User-data user) key JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (unpack-error 'profile/avatar-url URL x)]))))

(: profile/background-colour (-> Twitter-User Colour))
(define (profile/background-colour user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_background_color JSON-Null)])
    (cond
      [(string? x) (hexcode->colour x)]
      [else (unpack-error 'profile/background-colour Colour x)])))

(: profile/background-image-url (-> Twitter-User [#:ssl? Boolean] URL))
(define profile/background-image-url
  (λ (user
      #:ssl? [ssl? #t])
    (let* ([key (if ssl? 'profile_background_image_url_https 'profile_background_image_url)]
           [x (hash-ref (Twitter-User-data user) key JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (unpack-error 'profile/background-image-url URL x)]))))

(: profile/background-tile? (-> Twitter-User Boolean))
(define (profile/background-tile? user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_background_tile JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'profile/background-tile? Boolean x)])))

(: profile/banner-url (-> Twitter-User URL))
(define (profile/banner-url user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_banner_url JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (unpack-error 'profile/banner_url Colour x)])))

(: profile/default? (-> Twitter-User Boolean))
(define (profile/default? user)
  (let ([x (hash-ref (Twitter-User-data user) 'default_profile JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'profile/default? Boolean x)])))

(: profile/link-colour (-> Twitter-User Colour))
(define (profile/link-colour user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_link_color JSON-Null)])
    (cond
      [(string? x) (hexcode->colour x)]
      [else (unpack-error 'profile/link-colour Colour x)])))

(: profile/sidebar-border-colour (-> Twitter-User Colour))
(define (profile/sidebar-border-colour user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_sidebar_border_color JSON-Null)])
    (cond
      [(string? x) (hexcode->colour x)]
      [else (unpack-error 'profile/sidebar-border-colour Colour x)])))

(: profile/sidebar-fill-colour (-> Twitter-User Colour))
(define (profile/sidebar-fill-colour user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_sidebar_fill_color JSON-Null)])
    (cond
      [(string? x) (hexcode->colour x)]
      [else (unpack-error 'profile/sidebar-fill-colour Colour x)])))

(: profile/text-colour (-> Twitter-User Colour))
(define (profile/text-colour user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_text_color JSON-Null)])
    (cond
      [(string? x) (hexcode->colour x)]
      [else (unpack-error 'profile/text-colour Colour x)])))

(: profile/use-background-image? (-> Twitter-User Boolean))
(define (profile/use-background-image? user)
  (let ([x (hash-ref (Twitter-User-data user) 'profile_user_background_image JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'profile/use-background-image? Boolean x)])))
