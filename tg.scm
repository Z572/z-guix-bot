(library (tg)
  (export
   %api.telegram.org
   scm->tg-user

   tg-entities?
   tg-entities->scm
   scm->tg-entities
   json->tg-entities
   tg-entities-type
   tg-entities-length
   tg-entities-offset
   tg-entities-url
   tg-entities-language

   json->tg-from
   tg-from-first-name
   tg-from-user-name
   tg-from-language)
  (import
    (srfi srfi-71)
    (srfi srfi-1)
    (srfi srfi-18)
    (srfi srfi-189)
    (srfi srfi-171)
    (srfi srfi-2)
    (srfi srfi-26)
    (srfi srfi-35)
    (srfi srfi-71)
    (srfi srfi-9)
    (guile)
    (ice-9 binary-ports)
    (rnrs bytevectors)
    (web client)
    (web request)
    (web response)
    (web server)
    (web uri)
    (json))
  (define %api.telegram.org
    (make-parameter "api.telegram.org"))
  (define-json-type <tg-user>
    (id)
    (bot? "is_bot")
    (first-name "first_name")
    (last-name "last_name")
    (username)
    (language-code "language_code"))
  (define-json-type <tg-entities>
    (type)
    (length)
    (offset)
    ;;(user "user" <tg-user>)
    (url)
    (language))
  (define-json-type <tg-from>
    (id)
    (bot? "is_bot")
    (first-name "first_name")
    (last-name "last_name")
    (user-name "username")
    (language "language_code")
    (premium? "is_premium")
    (can-join-groups? "can_join_groups")
    (can-read-all-group-messages? "can_read_all_group_messages")
    (supports-inline-queries? "supports_inline_queries")))
