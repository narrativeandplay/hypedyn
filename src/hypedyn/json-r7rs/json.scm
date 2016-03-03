
(define-library (macduffie json)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme inexact)
          (scheme read)
          (scheme write)
          (srfi 69))
  (export json-read  json-read-string  json-read-file
          json-write json-write-string json-write-file)
  (include "json.body.scm"))
