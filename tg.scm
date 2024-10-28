#!/usr/bin/env -S guile -e main
!#
(use-modules ;(tg)
 (ice-9 atomic)
 (goblins actor-lib cell)
 (rnrs bytevectors)
 (ice-9 threads)
 (ice-9 iconv)
 (logging logger)
 (logging rotating-log)
 (logging port-log)
 (scheme documentation)
 (oop goops)
 (fibers)
 (fibers conditions)
 (goblins actor-lib methods)
 (srfi srfi-71)
 (ice-9 binary-ports)
 (web client)
 (goblins)
 (guix inferior)
 (guix store)
 (guix channels)
 (ice-9 session)
 (ice-9 sandbox)
 (system repl coop-server)
 (system repl server)
 (guix describe)
 (guix packages)
 (ice-9 format)
 (ice-9 match)
 (ice-9 pretty-print)
 (srfi srfi-1)
 (srfi srfi-2)
 (srfi srfi-26)
 (srfi srfi-189)
 (web server)
 (web response)
 (web request)
 (web client)
 (web uri)
 (json)
 (gnu)
 (guix ui)
 (guix packages)
 (gnu packages)
 (guix records)
 (guix import json)
 (guix scripts search)
 (srfi srfi-9)
 (sxml simple))

(define %api-base-url
  (make-parameter "api.telegram.org"))

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


(define* (tg-request token method #:optional query)
                                        ;(log-msg 'INFO "tg-request!")
  (let* ((uri (build-uri
               'https
               #:host (%api-base-url)
               #:path
               (string-append "/bot" token "/"
                              (if (symbol? method)
                                  (symbol->string method)
                                  method))))
         (body (call-with-output-bytevector (lambda (x) (scm->json query x))))
         (headers `((Content-Type . "application/json")
                    (User-Agent . "z-bot")
                    (Content-Length . ,(number->string
                                        (bytevector-length body)))))
         (port (open-socket-for-uri uri))
         (request (build-request
                   uri
                   #:headers headers
                   #:version '(1 . 1)
                   #:port port))
         (request (write-request request port)))
    (write-request-body request body)
    (force-output (request-port request))
    (let* ((response (read-response port))
           (body (read-response-body response))
           (scm (call-with-input-bytevector body json->scm)))
      (close-port port)
      scm)))

(define (tg-lookup json)
  (if (assoc-ref json "ok")
      (right (match (assoc-ref json "result")
               (#((pat_1 update-id)) (cons (scm->tg-message (cdr pat_1)) (cdr update-id)))
               (#() #f)
               (a (pk 'unk a) #f)))
      (left (assoc-ref json "description"))))
(define (show-inferior-package pkg) 1)
(define (return-packages-info name)
  (if (string= "" name)
      "???"
      (let ((packages
             ($ %guix-bot 'look-package name)
             ;; (apply find-packages-by-name (string-split name #\@))
             ))
        (maybe-ref packages
                   (lambda _ "no init!")
                   (lambda (x)
                     (object->string x))
                   ;; (if (null? packages)
                   ;;     (list (format #f "~a not found!" name))
                   ;;     ;; (map
                   ;;     ;;  (lambda (p)
                   ;;     ;;    (call-with-output-string
                   ;;     ;;      (cut package->recutils p <> 30)))
                   ;;     ;;  packages)
                   ;;     (map object->string packages)
                   ;;     )
                   ))))

(define (get-command-name text offset length)
  (apply values (string-split (substring text offset (+ length offset)) #\@)))

(define (maybe-field name value)
  (if (unspecified? value)
      '()
      (list (cons name value))))
(define* (send-message token
                       #:key
                       chat-id
                       text
                       reply-to-message-id
                       disable-notification)
  (log-msg 'INFO "send-message" 'chat-id chat-id 'text text)
  (tg-request
   token
   "sendMessage"
   `(("chat_id" . ,chat-id)
     ("text" . ,text)
     ,@(maybe-field "disable_notification" disable-notification)
     ,@(if disable-notification
           `(("disable_notification" . ,(->bool disable-notification)))
           '())
     ("reply_to_message_id"
      . ,reply-to-message-id))))

(define-once %commands (make-hash-table))
(define-syntax-rule (define-command (s args ...)
                      body ...)
  (hash-set! %commands
             (string-append "/" (symbol->string 's))
             (lambda (args ...)
               (with-throw-handler #t
                 (lambda ()
                   (begin body ...))
                 (lambda _
                   (backtrace))))))

(define-command (show comm)
  (return-packages-info comm))

(define-command (info comm)
  (with-output-to-string
    (lambda ()
      ((macro-transformer (module-ref (resolve-interface '(ice-9 session)) 'help))
       (datum->syntax #f `(help ,(call-with-input-string comm read)))))))

(define-command (channels comm)
  "Show current channels"
  (maybe-ref (pk 'channels ($ %guix-bot 'current-channel))
             (lambda _ "NO init!")
             (lambda (x)
               (with-output-to-string
                 (lambda ()
                   (pretty-print x))))))

(define-once tg-vat (make-parameter #f))

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
  (let (;; (s (string-append "(values\n" s "\n)"))
        )
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
                    (string-append
                     (format #f (G_ "failed to evaluate expression '~a':~%") exp)
                     (match args
                       (('syntax-error proc message properties form . rest)
                        (format #f (G_ "syntax error: ~a~%") message))
                       ((error args ...)
                        (apply format #f "error-> ~S" args))
                       (what? (apply format #f "unknow: ~{~S~}" args)))))))))
      (lambda a (format #f "=> ~{~S~^ ~}"  a)))))

(define (get-command str)
  (or (hash-ref %commands str)
      (lambda (str)
        (format #f "未知指令: ~a" str))))


(define (call-with-inferior s proc)
  (let* ((i (open-inferior s))
         (out (proc i)))
    (close-inferior i)
    out))

(define-actor (^guix bcom)
  #:self self
  (define inferior-d (make-atomic-box #f))
  (define channels
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git"))))
  (call-with-new-thread
   (lambda ()
     (let loop ()
       (with-throw-handler #t
         (lambda ()
           (define i
             (with-store store
               (cached-channel-instance
                store
                channels
                #:cache-directory (%inferior-cache-directory)
                #:ttl (* 3600 24 30))))
           (and=> (atomic-box-swap! inferior-d i)
                  (lambda (x)
                    (unless (equal? x i)
                      (log-msg 'INFO "inferior different, update it ~a ~a" x i inferior-d)))))
         (lambda _
           (backtrace)))
       (loop))))
  (methods
   ((get-inferior)
    (truth->maybe (atomic-box-ref inferior-d)))
   ((look-package pkg)
    (maybe-let* ((d ($ self 'get-inferior)))
      (pk 'look-package
          (call-with-inferior d
            (lambda (i)
              (apply lookup-inferior-packages i (string-split pkg #\@)))))))
   ((current-channel)
    (maybe-let* ((i ($ self 'get-inferior)))
      (call-with-inferior i
        (lambda (b)
          (inferior-eval
           '(begin
              (use-modules (guix describe)
                           (guix channels))
              (let ((guix-channel (car (current-channels))))
                (channel->code guix-channel)))
           b)))))))

(define-actor (^bot bcom #:key token)
  (define-cell %last-update-id #f)
  (methods
   ((run!)
    (either-let* ((out (tg-lookup (tg-get-updates token -1))))
      (let* ((message (car out))
             (update-id (cdr out))
             (message-id (tg-message-message-id message))
             (from (tg-message-from message))
             (chat (tg-message-chat message))
             (text (tg-message-text message))
             (entities (tg-message-entities message)))
        (unless (equal? ($ %last-update-id) update-id)
          (when (number? ($ %last-update-id))
            (and=> entities
                   (cut for-each
                        (lambda (m)
                          (define type (tg-entities-type m))
                          (define length (tg-entities-length m))
                          (define offset (tg-entities-offset m))
                          (define url (tg-entities-url m))
                          (define language (tg-entities-language m))

                          (define command-value (string-trim-both
                                                 (string-drop
                                                  text
                                                  (+ length offset))))

                          (pk command-value)
                          (match type
                            ("bot_command"
                             (send-message
                              token
                              #:chat-id (tg-chat-id chat)
                              #:reply-to-message-id message-id
                              #:text
                              ((get-command (get-command-name text offset length))
                               command-value
                               )))
                            (o #f))
                          (log-msg 'INFO
                                   "[update-id:~a] chat-id:~a user: ~S(~S) type: ~S~%"
                                   update-id
                                   (tg-chat-id chat)
                                   (tg-from-first-name from)
                                   (tg-from-username from)
                                   text)) <>)))
          ($ %last-update-id update-id))
        #f)))))

(define* (tg-get-updates token #:optional (offset -1))
  (tg-request token 'getUpdates `((offset . ,offset))))

(define (setup-logging)
  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 3
                     #:size-limit 1024
                     #:file-name "test-log-file"))
        (err       (make <port-log> #:port (current-error-port))))

    ;; don't want to see warnings or info on the screen!!
    ;; (disable-log-level! err 'WARN)
    ;; (disable-log-level! err 'INFO)

    ;; add the handlers to our logger
    (add-handler! lgr rotating)
    (add-handler! lgr err)

    (set-default-logger! lgr)
    (open-log! lgr)))

(define (shutdown-logging)
  (flush-log)
  (close-log!)
  (set-default-logger! #f))


(define %guix-bot #f)
(define %bot #f)
(define (main . _)
  (tg-vat (spawn-vat))
  (setup-logging)
  (spawn-server)
  (with-vat (tg-vat)
    (set! %guix-bot (spawn ^guix)))
  (with-vat (tg-vat)
    (let* ((bot (spawn ^bot #:token (second (program-arguments)))))
      (set! %bot bot)
      (log-msg 'INFO "start!")
      (let loop ()
        (on (<- bot 'run!)
            (lambda (out)
              (unless (either->truth out)
                                        ;(log-msg 'INFO "do agent")
                (loop)))))))
  (shutdown-logging))

;; Local Variables:
;; mode: scheme
;; eval: (put 'maybe-let* 'scheme-indent-function 1)
;; eval: (put 'either-let* 'scheme-indent-function 1)
;; eval: (put 'call-with-inferior 'scheme-indent-function 1)
;; End:
