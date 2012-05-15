; handle debugging information from the server

; extract parameter from request
(define (request-parameter param)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-parameter"):getRequestParameter param))

; for now, print out to console
(format #t "event_type: ~a, event_data: ~a.~%~!" 
        (request-parameter "event_type")
        (request-parameter "event_data"))