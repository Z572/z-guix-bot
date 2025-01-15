(library (tg)
  (export
   tg-entities?
   tg-entities->scm
   scm->tg-entities
   tg-entities-type
   tg-entities-length
   tg-entities-offset
   tg-entities-url
   tg-entities-language)
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
  (define-json-type <tg-entities>
    (type)
    (length)
    (offset)
    ;;(user "user" <tg-user>)
    (url)
    (language)))
