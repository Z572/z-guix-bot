#!/usr/bin/env -S guile -e main
!#
(add-to-load-path (dirname (current-filename)))
(use-modules
 (tg)
 ((system repl server) #:hide (run-server))
 (fibers conditions)
 (fibers)
 (git)
 (gnu packages)
 (gnu)
 (gnutls)
 (goblins actor-lib cell)
 (goblins actor-lib common)
 (goblins actor-lib methods)
 (goblins ghash)
 (goblins)
 (guix channels)
 (guix describe)
 (guix git)
 (guix import json)
 (guix inferior)
 (guix packages)
 (guix packages)
 (guix records)
 (guix scripts search)
 (guix store)
 (guix ui)
 (ice-9 atomic)
 (ice-9 binary-ports)
 (ice-9 format)
 (ice-9 iconv)
 (ice-9 match)
 (ice-9 pretty-print)
 (ice-9 sandbox)
 (ice-9 session)
 (ice-9 textual-ports)
 (json)
 (logging logger)
 (logging port-log)
 (logging rotating-log)
 (oop goops)
 (rnrs bytevectors)
 (scheme documentation)
 (srfi srfi-1)
 (srfi srfi-18)
 (srfi srfi-171)
 (srfi srfi-189)
 (srfi srfi-2)
 (srfi srfi-26)
 (srfi srfi-35)
 (srfi srfi-71)
 (srfi srfi-9)
 (system repl coop-server)
 (web client)
 (web request)
 (web response)
 (web server)
 (web uri)
 (sxml simple))

(define (get-command-name text offset length)
  (apply values (string-split (substring text offset (+ length offset)) #\@)))

(define-once commands-vat (spawn-vat #:name 'commands))
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

;; (define-command (show comm message)
;;   (on (<- %guix-bot 'look-package comm)
;;       (lambda (x)
;;         (maybe-let* ((o x))
;;           (send-reply message o)))))

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
;; (define-command (channels comm message)
;;   "Show current channels"
;;   (maybe-ref
;;    (pk 'channels ($ %guix-bot 'current-channel))
;;    (lambda _ (send-reply message "No init!"))
;;    (lambda (x)
;;      (let ((str (call-with-output-string (cut pretty-print x <>))))
;;        (match-let* (((commit . locate)
;;                      (get-channel-o (call-with-input-string str read-syntax))))

;;          (let* ((checkout commit2 _ (update-cached-checkout
;;                                      "https://git.savannah.gnu.org/git/guix.git"
;;                                      #:ref `(tag-or-commit . ,commit)))
;;                 (repo (repository-open checkout))
;;                 (mes (commit-message
;;                       (commit-lookup
;;                        repo
;;                        (reference-name->oid repo "HEAD")))))

;;            (send-reply message
;;                        (string-append mes "\n"
;;                                       str)
;;                        #:entities
;;                        (list (make-tg-entities
;;                               "text_link"
;;                               (string-length commit)
;;                               (+ (1+ (string-length mes))
;;                                  (1+ (source->offset str locate)))
;;                               (string-append "https://git.savannah.gnu.org/cgit/guix.git/commit/?id=" commit)
;;                               *unspecified*))))
;;          )))))

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

(define-actor (^bot bcom)
  #:self self
  (methods
   ((get-me) (get-me))
   ((update! update)
    (let* ((message (or (maybe->truth (tg-update-message update))
                        (maybe->truth (tg-update-edited-message update))))
           (update-id (tg-update-id update))
           (from (tg-message-from message))
           (text (tg-message-text message))
           (entities (tg-message-entities message)))
      (for-each
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
                  (tg-from-user-name from)
                  text)) entities)
      (pk 'update! from  text entities)))))

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


(define-once %guix-bot #f)
(define-once %bot #f)
(define (setup-env)
  (tg-vat (spawn-vat #:name 'tg #:log? #t))
  ;; (with-vat (tg-vat)
  ;;   (set! %guix-bot (spawn ^guix)))
  )
;; (define (main . _)
;;   (%tg-token (second (program-arguments)))
;;   (setup-env)
;;   (setup-logging)
;;   (spawn-server)

;;   (with-vat (tg-vat)
;;     (let* ((bot (spawn ^bot)))
;;       (on (<- bot 'get-me) (cut log-msg 'INFO <>))
;;       (log-msg 'INFO (getpid))
;;       (set! %bot bot)
;;       (log-msg 'INFO "start!")
;;       (<- bot 'run!)))
;;   (shutdown-logging))


(define (handler request body)
  (let ((update (call-with-input-bytevector body json->tg-update)))
    (when (%tg-debug?)
      (log-msg 'INFO "get" update))
    ($ %bot 'update! update)
    (values '((content-type . (text/plain)))
            "ok!\n")))

(define (main . _)
  (let ((tg-token-file (string-append (getcwd) "/.tg-token")))
    (%tg-token
     (if (file-exists? tg-token-file)
         (call-with-input-file tg-token-file get-line)
         (second (program-arguments)))))

  (setup-env)
  (setup-logging)
  (spawn-server)

  (with-vat (tg-vat)
    (let* ((bot (spawn ^bot)))
      (on (<- bot 'get-me) (cut log-msg 'INFO <>))
      (log-msg 'INFO (getpid))
      (set! %bot bot)
      (log-msg 'INFO "start!"))
    (run-server (lambda (request body)
                  (handler request body))
                'http (list #:port 10000)))
  (shutdown-logging))

;; Local Variables:
;; mode: scheme
;; eval: (put 'with-vat 'scheme-indent-function 1)
;; eval: (put 'maybe-let* 'scheme-indent-function 1)
;; eval: (put 'either-let* 'scheme-indent-function 1)
;; eval: (put 'either-let*-values 'scheme-indent-function 1)
;; End:
