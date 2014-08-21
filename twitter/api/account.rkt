#lang typed/racket/base

(provide account/settings account/update-settings
         account/verify-credentials
         account/update-profile
         account/update-profile-colours
         account/update-profile-image
         account/update-profile-background
         account/update-profile-banner account/remove-profile-banner)

(require racket/match
         (only-in racket/path
                  filename-extension)
         (only-in typed/net/url
                  [url URL]
                  url->string))
(require oauth/client)
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; User accounts

(: account/settings
   (->* []
        [#:client Client #:token Token]
        [U (HashTable Symbol JSON) Twitter-Error]))
(define account/settings
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/account/settings.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x)(json-hash x)]
           [else (response-error 'account/settings x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-settings
   (->* []
        [#:client Client #:token Token #:trend-location (Perhaps WOE-ID)
                  #:sleep-time? (Perhaps Boolean) #:sleep-start (Perhaps Integer)
                  #:sleep-end (Perhaps Integer) #:time-zone (Perhaps Twitter-Time-Zone)
                  #:language (Perhaps Twitter-Language-Code)]
        [U (HashTable Symbol JSON) Twitter-Error]))
(define account/update-settings
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:trend-location [trend-location (Nothing)]
               #:sleep-time? [sleep-time? (Nothing)]
               #:sleep-start [sleep-start (Nothing)]
               #:sleep-end [sleep-end (Nothing)]
               #:time-zone [time-zone (Nothing)]
               #:language [language (Nothing)])
    (define ~sleep-start (enforce-range sleep-start 0 23))
    (define ~sleep-end (enforce-range sleep-end 0 23))
    (define response
      (oauth/post (twitter/url "/1.1/account/settings.json")
                  client
                  token
                  #:data `(,@(perhaps-param "trend_location_woeid" trend-location woe-id->string)
                           ,@(perhaps-param "sleep_time_enabled" sleep-time? bool->string)
                           ,@(perhaps-param "start_sleep_time" ~sleep-start number->string)
                           ,@(perhaps-param "end_sleep_time" ~sleep-end number->string)
                           ,@(perhaps-param "time_zone" time-zone symbol->string)
                           ,@(perhaps-param "lang" language symbol->string))))
     (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x)(json-hash x)]
           [else (response-error 'account/update-settings x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/verify-credentials
   (->* []
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define account/verify-credentials
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/get (twitter/url "/1.1/account/verify_credentials.json"
                              `(,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'account/verify-credentials x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-profile
   (->* []
        [#:client Client #:token Token #:name (Perhaps String) #:url (Perhaps URL)
                  #:location (Perhaps String)  #:description (Perhaps String)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define account/update-profile
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:name [name (Nothing)]
               #:url [url (Nothing)]
               #:location [location (Nothing)]
               #:description [description (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/post (twitter/url "/1.1/account/update_profile.json")
                  client
                  token
                  #:data `(,@(perhaps-param "name" name string-identity)
                           ,@(perhaps-param "url" url url->string)
                           ,@(perhaps-param "location" location string-identity)
                           ,@(perhaps-param "description" description string-identity)
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "skip_status" skip-status? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'account/update-profile x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-profile-colours
   (->* []
        [#:client Client #:token Token #:background-colour (Perhaps Colour)
                  #:link-colour (Perhaps Colour) #:sidebar-border-colour (Perhaps Colour)
                  #:sidebar-fill-colour (Perhaps Colour) #:text-colour (Perhaps Colour)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define account/update-profile-colours
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:background-colour [background-colour (Nothing)]
               #:link-colour [link-colour (Nothing)]
               #:sidebar-border-colour [sidebar-border-colour (Nothing)]
               #:sidebar-fill-colour [sidebar-fill-colour (Nothing)]
               #:text-colour [text-colour (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/post (twitter/url "/1.1/account/update_profile_colors.json")
                  client
                  token
                  #:data `(,@(perhaps-param "profile_background_color"
                                               background-colour
                                               colour->hexcode)
                           ,@(perhaps-param "profile_link_color"
                                               link-colour
                                               colour->hexcode)
                           ,@(perhaps-param "profile_sidebar_border_color"
                                               sidebar-border-colour
                                               colour->hexcode)
                           ,@(perhaps-param "profile_sidebar_fill_color"
                                               sidebar-fill-colour
                                               colour->hexcode)
                           ,@(perhaps-param "profile_text_color"
                                               text-colour
                                               colour->hexcode)
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "skip_status" skip-status? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'account/update-profile-colours x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-profile-image
   (->* [Path]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define account/update-profile-image
  (λ (path
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (assert path file-exists?)
    (assert (filename-extension path) valid-image-format?)
    (assert (file-size path) (below-filesize-in-kb? 700))
    (define response
      (oauth/post (twitter/url "/1.1/account/update_profile_image.json")
                  client
                  token
                  #:data `(,(Param "image" (base64-string-from-path path))
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "skip_status" skip-status? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'account/update-profile-image x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-profile-background
   (->* []
        [#:client Client #:token Token #:image-path (Perhaps Path) #:tile? (Perhaps Boolean)
                  #:use? (Perhaps Boolean) #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define account/update-profile-background
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:image-path [path (Nothing)]
               #:tile? [tile? (Nothing)]
               #:use? [use? (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (when (path? path)
      (assert path file-exists?)
      (assert (filename-extension path) valid-image-format?)
      (assert (file-size path) (below-filesize-in-kb? 800)))
    (define response
      (oauth/post (twitter/url "/1.1/account/update_profile_background_image.json")
                  client
                  token
                  #:data `(,@(perhaps-param "image" path base64-string-from-path)
                           ,@(perhaps-param "tile" tile? bool->string)
                           ,@(perhaps-param "use" use? bool->string)
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "skip_status" skip-status? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'account/update-profile-background x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-profile-banner
   (->* [Path]
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define account/update-profile-banner
  (λ (path
      #:client [client (current-client)]
      #:token [token (current-token)])
    (assert path file-exists?)
    (assert (filename-extension path) valid-image-format?)
    (assert (file-size path) (below-filesize-in-kb? 3072))
    (define response
      (oauth/post (twitter/url "/1.1/account/update_profile_banner.json")
                  client
                  token
                  #:data `(,(Param "image" (base64-string-from-path path)))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body) 'success]
      [(HTTP-Response #"HTTP/1.1 201 Created" _ body) 'success]
      [(HTTP-Response #"HTTP/1.1 202 Accepted" _ body) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/update-profile-banner-cropped
   (->* [Path #:width Integer #:height Integer #:offset-left Integer #:offset-top Integer]
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define account/update-profile-banner-cropped
  (λ (path
      #:width width
      #:height height
      #:offset-left offset-left
      #:offset-top offset-top
      #:client [client (current-client)]
      #:token [token (current-token)])
    (assert path file-exists?)
    (assert (filename-extension path) valid-image-format?)
    (assert (file-size path) (below-filesize-in-kb? 3072))
    (define ~width (enforce-range width 1 1500))
    (define ~height (enforce-range height 1 500))
    (define ~offset-left (enforce-range offset-left 0 '+inf))
    (define ~offset-top (enforce-range offset-top 0 '+inf))
    (define response
      (oauth/post (twitter/url "/1.1/account/update_profile_banner.json")
                  client
                  token
                  #:data `(,(Param "image" (base64-string-from-path path))
                           ,@(perhaps-param "width" ~width number->string)
                           ,@(perhaps-param "height" ~height number->string)
                           ,@(perhaps-param "offset_left" ~offset-left number->string)
                           ,@(perhaps-param "offset_top" ~offset-top number->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body) 'success]
      [(HTTP-Response #"HTTP/1.1 201 Created" _ body) 'success]
      [(HTTP-Response #"HTTP/1.1 202 Accepted" _ body) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: account/remove-profile-banner
   (->* []
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define account/remove-profile-banner
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/account/remove_profile_banner.json")
                  client
                  token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))
