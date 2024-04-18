(define-module (tg)
  #:use-module (ice-9 session)
  #:use-module (ice-9 sandbox)
  #:use-module (system repl server)
  #:use-module (guix describe)
  #:use-module (guix packages)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (web server)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (json)
  #:use-module (gnu)
  #:use-module (guix ui)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (guix records)
  #:use-module (guix import json)
  #:use-module (guix scripts search)
  #:use-module (srfi srfi-9)
  #:use-module (sxml simple)
  #:export (main))

(define %api-base-url
  "https://api.telegram.org")

(define %debug? #f)

(define %tg-token
  (make-parameter
   *unspecified*
   (lambda (x)
     (unless (or (string? x) (unspecified? x))
       (error "token must string"))
     x)))

;; (define-record-type* <tg-string> tg-string
;;   make-tg-string
;;   tg-string?
;;   this-tg-string
;;   (content tg-string-content
;;            (default "")))

(define-json-type <tg-user>
  (id)
  (bot? "is_bot")
  (first-name "first_name")
  (last-name "last_name")
  (username)
  (language-code "language_code"))

(define-json-type <tg-chat>
  (id "id")
  (type "type")
  ;; option
  (title tg-chat-title)
  (username tg-chat-username)
  (first-name "first_name")
  (last-name "last_name")
  ;;(photo "photo" <tg-chatphoto>)
  ;; (bio)
  ;; (description)
  ;; (invite-link)
  ;; (pinned-message "pinned_message" <tg-message>)
  ;; (permissions "permissions" <tg-chat-permissions>)
  ;; (slow-mode-delay "slow_mode_delay")
  ;; (message-auto-delete-time "message_auto_delete_time")
  ;; (sticker-set-name "sticker_set_name")
  ;; (can-set-sticker-set "can_set_sticker_set")
  ;; (linked-chat-id "linked_chat_id")
  ;;(location "location" <tg-chat-location>)
  )

(define-json-mapping <tg-message> make-tg-message
  tg-message?
  json->tg-message
  <=> tg-message->json
  <=> scm->tg-message
  <=> tg-message->scm
  (message-id tg-message-message-id "message_id")
  (from tg-message-from "from" json->tg-from)
  ;;(sender-chat "sender_chat" <tg-chat>)
  (date tg-message-date)
  (chat tg-message-chat "chat" json->tg-chat)
  ;;(forward-from tg-message-formward-from "forward_from" json->tg-user)
  (entities tg-message-entities "entities"
            (lambda (a)
              (and (vector? a)
                   (map json->tg-entities
                        (vector->list a)))))
  (text tg-message-text))

(define-json-type <tg-entities>
  (type)
  (length)
  (offset)
  ;;(user "user" <tg-user>)
  (url)
  (language))

(define-json-type <tg-from>
  (id)
  (language "language_code")
  (username)
  (first-name "first_name")
  (bot? "is_bot"))

(define (tg-lookup url)
  (and=>
   (false-if-exception (assoc-ref (json-fetch url) "result"))
   (match-lambda
     ;; (a a)
     (#((pat_1 update-id)) (cons (scm->tg-message (cdr pat_1)) (cdr update-id)
                                 ;; (car pat_1)
                                 ))
     (#() #f)
     (_ #f))))
(define %last-update-id #f)

(define (return-packages-info name)
  (if (string= "" name)
      "???"
      (let ((packages (apply find-packages-by-name (string-split name #\@))))
        (format #f "~{~a~%~%~}" (if (null? packages)
                                    (list (format #f "~a 查不到" name))
                                    (map
                                     (lambda (p)
                                       (call-with-output-string
                                         (cut package->recutils p <> 30)))
                                     packages))))))

(define* (get-method-url method . arg)
  (apply string-append %api-base-url "/bot" (%tg-token) "/" method
         arg))

(define (get-command-name text offset length)
  (apply values (string-split (substring text offset (+ length offset)) #\@)))

(define* (send-message #:key
                       (toke (%tg-token))
                       chat-id
                       (allow-sending-without-reply #f)
                       reply-to-message-id
                       text)
  (json-fetch (get-method-url
               "sendMessage"
               "?"
               (format #f "~:{~a=~a&~}"
                       `(("chat_id" ,(number->string
                                      chat-id))
                         ("allow_sending_without_reply"
                          ,(scm->json-string allow-sending-without-reply))
                         ("reply_to_message_id"
                          ,(number->string reply-to-message-id))
                         ("text" ,(uri-encode
                                   text)))))))

(define-once %commands (make-hash-table))
(define-syntax-rule (define-command (s args ...)
                      body ...)
  (hash-set! %commands
             (string-append "/" (symbol->string 's))
             (lambda (args ...) body ...)))

(define-command (show comm)
  (return-packages-info comm))

(define-command (info comm)
  (with-output-to-string
    (lambda ()
      ((macro-transformer (module-ref (resolve-interface '(ice-9 session)) 'help))
       (datum->syntax #f `(help ,(call-with-input-string comm read)))))))

(define-command (channels comm)
  "Show current channels"
  (object->string (current-channels)))

(define-command (help comm)
  (apply string-append
         (hash-map->list (lambda (x p) (string-append
                                        x
                                        "\n"
                                        (or (procedure-documentation p)
                                            "[No doc]")
                                        "\n"))

                         %commands)))

(define-command (eval comm)
  (eval-in-sandbox->string comm))

(define (eval-in-sandbox->string s)
  (pk 's s)
  (let ((s (string-append "(values\n" s "\n)")))
    (call-with-values
        (lambda ()
          (let ((exp (catch #t
                       (lambda ()
                         (call-with-input-string s read))
                       (lambda args
                         (format #f (G_ "failed to read expression ~s: ~s~%")
                                 s args)))))
            (if (string? exp)
                exp
                (catch #t
                  (lambda ()
                    (eval-in-sandbox
                     exp
                     #:time-limit 100))
                  (lambda args
                    (format #f (G_ "failed to evaluate expression '~a':~%") exp)
                    (match args
                      (('syntax-error proc message properties form . rest)
                       (format #f (G_ "syntax error: ~a~%") message))
                      ((error args ...)
                       (apply format #f "error-> ~S" args))
                      (what? (apply format #f "unknow: ~{~S~}" args))))))))
      (lambda a (format #f "=> ~{~S~^ ~}"  a)))))

(define (get-command str)
  (or (hash-ref %commands str)
      (lambda (str)
        (format #f "未知指令: ~a" str))))

(define* (main1)
  (and-let* ((out (tg-lookup (get-method-url "getUpdates" "?offset=-1")))
             (message (car out))
             (update-id (cdr out)))
    (match-let* ((($ <tg-message> message-id from date chat entities text) message))
      (unless (equal? %last-update-id update-id)
        (when (number? %last-update-id)
          (and=> entities
                 (cut for-each (match-lambda
                                 (($ <tg-entities> type length offset url language)
                                  (define command-value (string-trim-both
                                                         (string-drop
                                                          text
                                                          (+ length offset))))
                                  (pk command-value)
                                  (match type
                                    ("bot_command"
                                     (send-message
                                      #:chat-id (tg-chat-id chat)
                                      #:allow-sending-without-reply #t
                                      #:reply-to-message-id message-id
                                      #:text
                                      ((get-command (get-command-name text offset length))
                                       command-value
                                       )))
                                    (_ #f))
                                  (format #t "[update-id:~a] chat-id:~a user: ~S(~S) type: ~S~%"
                                          update-id
                                          (tg-chat-id chat)
                                          (tg-from-first-name from)
                                          (tg-from-username from)
                                          text)
                                  (when %debug?
                                    (format #t "\n---\n\n~@{~S\n~}\n---\n"
                                            message-id
                                            from
                                            date
                                            chat
                                            text
                                            type
                                            length
                                            offset
                                            url
                                            language
                                            message
                                            (string-split command #\@)
                                            command-value)))) <>)))
        (set! %last-update-id update-id)))))

(define (main . _)
  (spawn-server)
  (parameterize ((%tg-token (second (program-arguments))))
    (while #t
      (false-if-exception (main1))
      (sleep 1)))
  (stop-server-and-clients!))


;; Local Variables:
;; mode: scheme
;; End:
