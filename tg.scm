(library (tg)
  (export
   %tg-debug?
   %tg-test?
   %tg-request
   %tg-token
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
   scm->tg-chat
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
   tg-message-text

   tg-request
   get-me
   get-webhook-info
   set-webhook!
   send-message
   send-reply)
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
  (define-once %tg-debug? (make-parameter #t))
  (define-once %tg-test? (make-parameter #f))
  (define-once %tg-request (make-parameter #f))
  (define-once %tg-token (make-parameter #f))
  (define-once %api.telegram.org
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
                         #:conv json->tg-message)))

  (define* (tg-request method
                       #:optional query
                       #:key
                       (token (%tg-token))
                       (proc identity)
                       (user-agent "z-bot"))
    (either-join
     (exception->either
      (const #t)
      (if (%tg-test?)
          (lambda ()
            (right (%tg-request)))
          (lambda ()
            (let* ((uri (build-uri
                         'https
                         #:host (%api.telegram.org)
                         #:path
                         (string-append "/bot" token "/"
                                        (if (symbol? method)
                                            (symbol->string method)
                                            method))))
                   (response r-body
                             (http-post uri
                                        #:body (scm->json-string query #:unicode #t)
                                        #:headers `((Content-Type . "application/json")
                                                    (User-Agent . ,user-agent))))
                   (json (call-with-input-bytevector r-body json->scm)))
              (if (assoc-ref json "ok")
                  (right (proc (assoc-ref json "result")))
                  (left (assoc-ref json "description")))))))))
  (define (maybe-field name value)
    (if (unspecified? value)
        '()
        (list (cons name value))))
  (define* (send-message #:key
                         (token (%tg-token))
                         chat-id
                         text
                         reply-to-message-id
                         disable-notification
                         entities)
    (let ((o (tg-request
              "sendMessage"
              `(("chat_id" . ,chat-id)
                ("text" . ,text)
                ,@(maybe-field "disable_notification" disable-notification)
                ,@(if disable-notification
                      `(("disable_notification" . ,(->bool disable-notification)))
                      '())
                ("reply_to_message_id"
                 . ,reply-to-message-id)
                ,@(if entities
                      `(("entities" .
                         ,(list->vector
                           (map tg-entities->scm entities))))
                      '()))
              #:token token
              #:proc scm->tg-message)))
      o))
  (define* (send-reply message text
                       #:key (token (%tg-token))
                       entities)
    (send-message
     #:token token
     #:chat-id (tg-chat-id (tg-message-chat message))
     #:reply-to-message-id (tg-message-id message)
     #:text text
     #:entities entities))

  (define* (get-me #:key (token (%tg-token)))
    (tg-request 'getMe #:token token #:proc scm->tg-user))

  (define* (get-webhook-info #:key (token (%tg-token)))
    (tg-request 'getWebhookInfo #:token token))

  (define* (set-webhook!
            url
            #:key
            (max-connections 40)
            secret-token
            (token (%tg-token)))
    (tg-request 'setWebhook
                `(("url" . ,(or url ""))
                  ("max_connections" . ,max-connections))
                #:token token)))
