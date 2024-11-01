#!/usr/bin/env -S guile -e main
!#
(use-modules
 (srfi srfi-35)
 (gnutls)
 (goblins ghash)
 (goblins actor-lib common)
 (guix git)
 (git)
 (ice-9 iconv)
 (ice-9 atomic)
 (goblins actor-lib cell)
 (rnrs bytevectors)
 (srfi srfi-18)
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


(define* (tg-request method
                     #:optional query
                     #:key
                     (token (%token))
                     (proc identity))
  (either-join
   (exception->either
    (const #t)
    (lambda ()
      (let* ((uri (build-uri
                   'https
                   #:host (%api-base-url)
                   #:path
                   (string-append "/bot" token "/"
                                  (if (symbol? method)
                                      (symbol->string method)
                                      method))))
             (response r-body
                       (http-post uri
                                  #:body (scm->json-string query #:unicode #t)
                                  #:headers `((Content-Type . "application/json")
                                              (User-Agent . "z-bot"))))
             (json (call-with-input-bytevector r-body json->scm)))
        (if (assoc-ref json "ok")
            (right (proc (assoc-ref json "result")))
            (left (assoc-ref json "description"))))))))

(define (get-command-name text offset length)
  (apply values (string-split (substring text offset (+ length offset)) #\@)))

(define (maybe-field name value)
  (if (unspecified? value)
      '()
      (list (cons name value))))
(define* (send-message #:key
                       (token (%token))
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
    (log-msg 'INFO "send-message" 'chat-id chat-id 'text text)
    o))

(define* (get-me #:key (token (%token)))
  (tg-request 'getMe #:token token #:proc scm->tg-user))

(define commands-vat (spawn-vat #:name 'commands))
(define-once %commands
  (with-vat commands-vat
    (spawn ^ghash)))
(define-syntax-rule (define-command (s args ...)
                      body ...)
  (with-vat commands-vat
    (<-np %commands
          'set (string-append "/" (symbol->string 's))
          (lambda* (args ...)
            (with-throw-handler #t
              (lambda ()
                (begin body ...))
              (lambda _
                (backtrace)))))))

(define-command (show comm message)
  (on (<- %guix-bot 'look-package comm)
      (lambda (x)
        (maybe-let* ((o x))
          (send-reply message o)))))

(define-command (info comm message)
  (send-reply
   message
   (with-output-to-string
     (lambda ()
       ((macro-transformer (module-ref (resolve-interface '(ice-9 session)) 'help))
        (datum->syntax #f `(help ,(call-with-input-string comm read))))))))

(define (source->offset str source)
  (define line (assoc-ref source 'line))
  (define column (assoc-ref source 'column))
  (pk 'str str
      'line line
      'column column
      'offset
      (let ((s (string-split str #\nl)))
        (let loop ((line* 0)
                   (offset column))
          (if (>=  line* line)
              offset
              (loop (1+ line*)
                    (+ offset
                       1 ;; \n
                       (string-length (list-ref s line*)))))))))

(define (get-channel-o x)
  (syntax-case x (channel
                  name
                  url
                  branch
                  commit
                  introduction
                  make-channel-introduction
                  openpgp-fingerprint)
    ((channel (name guix)
              (url url*)
              (branch branch*)
              (commit commit*)
              (introduction
               (make-channel-introduction
                before-commit
                (openpgp-fingerprint
                 fingerprint))))
     (cons
      (syntax->datum #'commit*)
      (syntax-source #'commit*)))
    ((channel (name guix)
              (url url*)
              (branch branch*)
              (commit commit*))
     (cons
      (syntax->datum #'commit*)
      (syntax-source #'commit*)))))
(define-command (channels comm message)
  "Show current channels"
  (maybe-ref
   (pk 'channels ($ %guix-bot 'current-channel))
   (lambda _ (send-reply message "No init!"))
   (lambda (x)
     (let ((str (call-with-output-string (cut pretty-print x <>))))
       (match-let* (((commit . locate)
                     (get-channel-o (call-with-input-string str read-syntax))))

         (let* ((checkout commit2 _ (update-cached-checkout
                                     "https://git.savannah.gnu.org/git/guix.git"
                                     #:ref `(tag-or-commit . ,commit)))
                (repo (repository-open checkout))
                (mes (commit-message
                      (commit-lookup
                       repo
                       (reference-name->oid repo "HEAD")))))

           (send-reply message
                       (string-append mes "\n"
                                      str)
                       #:entities
                       (list (make-tg-entities
                              "text_link"
                              (string-length commit)
                              (+ (1+ (string-length mes))
                                 (1+ (source->offset str locate)))
                              (string-append "https://git.savannah.gnu.org/cgit/guix.git/commit/?id=" commit)
                              *unspecified*))))
         )))))

(define-once tg-vat (make-parameter #f))

(define-command (help comm message)
  (on (<- %commands 'data)
      (lambda (x)
        (send-reply
         message
         (ghash-fold
          (lambda (x p prev) (string-append
                              prev "\n"
                              x
                              "\n"
                              (or (procedure-documentation p)
                                  "[No doc]")))
          ""
          x)))))

(define-command (eval comm message)
  (send-reply message (eval-in-sandbox->string comm)))

(define (eval-in-sandbox->string s)
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
  (with-vat commands-vat
    ($ %commands 'ref
                 str
                 (lambda (_str message)
                   (send-reply message (format #f "未知指令: ~a" str))))))

(define-actor (^guix bcom)
  #:self self
  (define inferior+instance (make-atomic-box #f))
  (define channels
    (list (channel
           (name 'guix)
           (url "https://git.savannah.gnu.org/git/guix.git"))))
  (define inferior-updater
    (make-thread
     (lambda ()
       (let loop ()
         (with-throw-handler #t
           (lambda ()
             (define (update! d)
               (atomic-box-swap! inferior+instance
                                 (cons d (open-inferior d))))
             (let ((inf (atomic-box-ref inferior+instance))
                   (new-d (with-store store
                            (cached-channel-instance
                             store
                             channels
                             #:cache-directory (%inferior-cache-directory)
                             #:ttl (* 3600 24 30)))))
               (if inf
                   (let ((d (car inf))
                         (i* (cdr inf)))
                     (unless (string= new-d d)
                       (and=> (update! new-d)
                              (lambda (x)
                                (close-inferior (cdr x ))
                                (log-msg 'INFO "close-inferior" (cdr x))))))
                   (update! new-d))))
           (lambda _
             (backtrace)))
         (thread-sleep! 3600)
                                        ;(sleep 5)
         (loop)))
     "tg inferior updater"))
  (thread-start! inferior-updater)
  (methods
   ((get-inferior)
    (truth->maybe (and-let* ((o (atomic-box-ref inferior+instance)))
                    (cdr o))))
   ((look-package pkg)
    (maybe-let* ((d ($ self 'get-inferior)))
      (inferior-eval
       `(begin
          (use-modules (gnu packages)
                       (guix diagnostics)
                       (guix read-print))
          (let ((o (find-package-locations ,@(string-split pkg #\@))))
            (if (null? o)
                "No found this package!"
                (let ((loc (cdar o)))
                  (call-with-input-file (%search-load-path (location-file loc))
                    (lambda (p)
                      (go-to-location
                       p
                       (location-line loc)
                       (location-column loc))
                      (call-with-output-string
                        (lambda (out)
                          (pretty-print-with-comments
                           out
                           (read-with-comments p))))))))))
       d)))
   ((current-channel)
    (maybe-let* ((i ($ self 'get-inferior)))
      (inferior-eval
       '(begin
          (use-modules (guix describe)
                       (guix channels))
          (let ((guix-channel (car (current-channels))))
            (channel->code guix-channel)))
       i)))))

(define* (send-reply message text
                     #:key (token (%token))
                     entities)
  (send-message
   #:token token
   #:chat-id (tg-chat-id (tg-message-chat message))
   #:reply-to-message-id (tg-message-message-id message)
   #:text text
   #:entities entities))

(define-actor (^bot bcom)
  #:self self
  (define-cell %last-update-id #f)
  (define-cell %should-shutdown? #f)
  (methods
   ((get-me)
    (get-me))

   ((run!)
    (either-let*-values (((message update-id) (tg-get-updates -1)))
      (let* ((from (tg-message-from message))
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
                          (define command-value (string-trim-both
                                                 (string-drop
                                                  text
                                                  (+ length offset))))

                          (pk command-value)
                          (match type
                            ("bot_command"
                             ((get-command (get-command-name text offset length))
                              command-value
                              message))
                            (o (log-msg 'WARN "unknow type"o)#f))
                          (log-msg 'INFO
                                   "[update-id:~a] chat-id:~a user: ~S(~S) type: ~S~%"
                                   update-id
                                   (tg-chat-id (tg-message-chat message))
                                   (tg-from-first-name from)
                                   (tg-from-username from)
                                   text)) <>)))
          ($ %last-update-id update-id))))
    (unless ($ %should-shutdown?)
      (<- self 'run!)))))

(define* (tg-get-updates #:optional (offset -1) #:key (token (%token)))
  (either-let* ((updates (tg-request 'getUpdates `((offset . ,offset))
                                     #:token token)))
    (let ((m&u (vector-ref updates 0)))
      (values (scm->tg-message (assoc-ref m&u "message"))
              (assoc-ref m&u "update_id")))))

(define (setup-logging)
  (let ((lgr       (make <logger>))
        (rotating  (make <rotating-log>
                     #:num-files 3
                     #:size-limit 102400
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
(define %token (make-parameter #f))
(define (setup-env)
  (tg-vat (spawn-vat #:name 'tg #:log? #t))
  (with-vat (tg-vat)
    (set! %guix-bot (spawn ^guix))))
(define (main . _)
  (%token (second (program-arguments)))
  (setup-env)
  (setup-logging)
  (spawn-server)

  (with-vat (tg-vat)
    (let* ((bot (spawn ^bot)))
      (on (<- bot 'get-me) (cut log-msg 'INFO <>))
      (log-msg 'INFO (getpid))
      (set! %bot bot)
      (log-msg 'INFO "start!")
      (<- bot 'run!)))
  (shutdown-logging))

;; Local Variables:
;; mode: scheme
;; eval: (put 'with-vat 'scheme-indent-function 1)
;; eval: (put 'maybe-let* 'scheme-indent-function 1)
;; eval: (put 'either-let* 'scheme-indent-function 1)
;; eval: (put 'either-let*-values 'scheme-indent-function 1)
;; End:
