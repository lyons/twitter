Timelines
===

Endpoint                       | Method
------------------------------ | -----------------
GET statuses/mentions_timeline | timeline/mentions
GET statuses/user_timeline     | timeline/user
GET statuses/home_timeline     | timeline/home
GET statuses/retweets_of_me    | timeline/retweets


Tweets
===

Endpoint                        | Method
------------------------------- | ------------------------
GET  statuses/retweets/:id      | status/retweets
GET  statuses/show/:id          | status/show
POST statuses/destroy/:id       | status/destroy
POST statuses/update            | status/create
POST statuses/retweet/:id       | status/retweet
POST statuses/update_with_media | status/create-with-media
GET  statuses/oembed            | status/oembed
GET  statuses/retweeters/ids    | status/retweeters
GET  statuses/lookup            | status/lookup


Search
===

Endpoint          | Method
----------------- | -------------
GET search/tweets | search/tweets <br /> (Currently discards search metadata)


Streaming
===

Endpoint               | Method
---------------------- | -------------------
POST statuses/filter   | Not yet implemented
GET  statuses/sample   | Not yet implemented
GET  statuses/firehose | Not yet implemented
GET  user              | Not yet implemented
GET  site              | Not yet implemented


Direct Messages
===

Endpoint                     | Method
---------------------------- | -------------------------
GET  direct_messages         | direct-messages/list
GET  direct_messages/sent    | direct-messages/list-sent
GET  direct_messages/show    | direct-messages/show
POST direct_messages/destroy | direct-messages/destroy
POST direct_messages/new     | direct-messages/create


Friends & Followers
===

Endpoint                         | Method
-------------------------------- | -------------------------
GET  friendships/no_retweets/ids | friends/no-retweets
GET  friends/ids                 | friends/ids
GET  followers/ids               | followers/ids
GET  friendships/incoming        | friendships/incoming
GET  friendships/outgoing        | friendships/outgoing
POST friendships/create          | friendship/create
POST friendships/destroy         | friendship/destroy
POST friendships/update          | friendship/update
GET  friendships/show            | friendship/show
GET  friends/list                | friends/list
GET  followers/list              | followers/list
GET  friendships/lookup          | friendships/lookup-by-id <br /> friendships/lookup-by-name


Users
===

Endpoint                                     | Method
-------------------------------------------- | ---------------------------------
GET  account/settings                        | account/settings
GET  account/verify_credentials              | account/verify-credentials
POST account/settings                        | account/update-settings
POST account/update_delivery_device          | Retired?
POST account/update_profile                  | account/update-profile
POST account/update_profile_background_image | account/update-profile-background
POST account/update_profile_colors           | account/update-profile-colours
POST account/update_profile_image            | account/update-profile-image
POST account/remove_profile_banner           | account/remove-profile-banner
POST account/update_profile_banner           | account/update-profile-banner <br /> account/update-profile-banner-cropped
GET  blocks/list                             | blocks/list
GET  blocks/ids                              | blocks/list-ids
POST blocks/create                           | block/create
POST blocks/destroy                          | block/destroy
GET  users/lookup                            | users/lookup-by-name <br /> users/lookup-by-id
GET  users/show                              | user/show
GET  users/search                            | search/users
GET  users/contributees                      | user/contributees
GET  users/contributors                      | user/contributors
GET  users/profile_banner                    | user/profile-banner
POST mutes/users/create                      | mute/create
POST mutes/users/destroy                     | mute/destroy
GET  mutes/users/ids                         | mutes/list-ids
GET  mutes/users/list                        | mutes/list


Suggested Users
===

Endpoint                            | Method
----------------------------------- | -------------------
GET users/suggestions/:slug         | Not yet implemented
GET users/suggestions               | Not yet implemented
GET users/suggestions/:slug/members | Not yet implemented


Favourites
===

Endpoint               | Method
---------------------- | ------------------
GET  favorites/list    | favourites/list
POST favorites/destroy | favourites/destroy
POST favorites/create  | favourites/create


Lists
===

Endpoint                       | Method
------------------------------ | -------------------------------
GET  lists/list                | lists/list
GET  lists/statuses            | lists/statuses
POST lists/members/destroy     | lists/members/destroy
GET  lists/memberships         | lists/memberships
GET  lists/subscribers         | lists/subscribers
POST lists/subscribers/create  | lists/subscribe
GET  lists/subscribers/show    | (lists/subscribed?)
POST lists/subscribers/destroy | lists/unsubscribe
POST lists/members/create_all  | lists/members/create-all-by-id <br /> lists/members/create-all-by-name
GET  lists/members/show        | lists/members/show
GET  lists/members             | lists/members
POST lists/members/create      | lists/members/create
POST lists/destroy             | lists/destroy
POST lists/update              | lists/update
POST lists/create              | lists/create
GET  lists/show                | lists/show
GET  lists/subscriptions       | lists/subscriptions
POST lists/members/destroy_all | lists/members/destroy-all-by-id <br /> lists/members/destroy-all-by-name
GET  lists/ownerships          | lists/ownerships


Saved Searches
===

Endpoint                        | Method
------------------------------- | -------------------
GET  saved_searches/list        | Not yet implemented
GET  saved_searches/show/:id    | Not yet implemented
POST saved_searches/create      | Not yet implemented
POST saved_searches/destroy/:id | Not yet implemented


Places & Geo
===

Endpoint                 | Method
------------------------ | -------------------
GET  geo/id/:place_id    | geo/show-place
GET  geo/reverse_geocode | geo/reverse-geocode <br /> (Currently discards search metadata)
GET  geo/search          | geo/search          <br /> (Currently discards search metadata)
GET  geo/similar_places  | geo/similar-places  <br /> (Currently discards search metadata)
POST geo/place           | Retired


Trends
===

Endpoint             | Method
-------------------- | ----------------
GET trends/place     | trends/place
GET trends/available | trends/available
GET trends/closest   | trends/closest


Spam Reporting
===

Endpoint               | Method
---------------------- | -----------------
POST users/report_spam | users/report-spam


OAuth
===

Endpoint                     | Method
---------------------------- | ----------------------
GET  oauth/authenticate      | oauth/authenticate-url
GET  oauth/authorize         | oauth/authorise-url
POST oauth/access_token      | oauth/access-token <br /> oauth/access-token-x-auth
POST oauth/request_token     | oauth/request-token
POST oauth2/token            | Not implemented
POST oauth2/invalidate_token | Not implemented


Help
===

Endpoint                          | Method
--------------------------------- | -----------------------------
GET help/configuration            | help/configuration
GET help/languages                | help/supported-languages
GET help/privacy                  | help/privacy-policy
GET help/tos                      | help/terms-of-service
GET application/rate_limit_status | application/rate-limit-status
