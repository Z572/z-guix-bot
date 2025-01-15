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
   tg-from-language

   json->tg-chat
   tg-chat-id
   tg-chat-title
   tg-chat-user-name

   scm->tg-message
   tg-message?
   tg-message-id
   tg-message-chat
   tg-message-from
   tg-message-edit-date
   tg-message-date
   tg-message-entities
   tg-message-text)
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
    (supports-inline-queries? "supports_inline_queries"))

  (define-json-type <tg-chat>
    (id "id")
    (type "type")
    ;; option
    (title tg-chat-title)
    (user-name tg-chat-user-name)
    (first-name "first_name")
    (last-name "last_name")
    (forum "is_forum"))

  (define* (unspecified->maybe x #:key (conv identity))
    (if (unspecified? x)
        (nothing)
        (just (conv x))))

  (define-json-mapping <tg-message> make-tg-message
    tg-message?
    json->tg-message
    <=> tg-message->json
    <=> scm->tg-message
    <=> tg-message->scm
    (id tg-message-id "message_id")
    (from tg-message-from "from" json->tg-from)
    (sender-chat tg-message-sender-chat "sender_chat"
                 (cut unspecified->maybe <> #:conv json->tg-chat))
    (date tg-message-date)
    (edit-date tg-message-edit-date "edit_date")
    (chat tg-message-chat "chat" json->tg-chat)
    ;;(forward-from tg-message-formward-from "forward_from" json->tg-user)
    (entities tg-message-entities "entities"
              (lambda (a)
                (if (vector? a)
                    (vector-transduce (tmap json->tg-entities) rcons a)
                    (list))))
    (text tg-message-text))

  (define-json-mapping <tg-update> make-tg-update
    tg-update?
    json->tg-update
    <=> tg-update->json
    <=> scm->tg-update
    <=> tg-update->scm
    (id tg-update-id "update_id")
    (message tg-update-message "message"
             (cut unspecified->maybe <>
                  #:conv json->tg-message))
    (edited_message tg-update-edited-message "edited_message"
                    (cut unspecified->maybe <>
                         #:conv json->tg-message))))
