; kawa-specific network code

; export
(module-export gethostname hostname->string 
               tcp-listen tcp-accept-ready? tcp-accept tcp-connect 
               socket-write socket-read
               client-socket)
(module-static 'init-run)

; get hostname
(define (gethostname)
  (<java.net.InetAddress>:getLocalHost))

; convert hostname to string
(define (hostname->string in-hostname :: <java.net.InetAddress>)
  (if in-hostname
      (invoke in-hostname 'toString)))

; listen for connections on given port
; returns a TCP listener value
(define (tcp-listen in-port)
;  (try-catch
      (<java.net.ServerSocket> in-port)
;    (ex <java.net.SocketException>
;        (begin
;          (display (*:toString ex))(newline)
;          (*:printStackTrace ex)
;          )))
    ; now need to listen?
    )

; is there a client waiting?
(define (tcp-accept-ready? in-listener)
  ; need to check if a client is waiting to be accepted
  #f)

;; new java way of accepting connection
;; returning a socket object 
; accept an incoming client connection on given listener
(define (tcp-accept in-listener :: <java.net.ServerSocket>) ;; :: <java.net.ServerSocket>
  (invoke in-listener 'accept))

;; new 
(define (client-socket host :: <java.lang.String> inport :: <int>) ;;  
  (<java.net.Socket> (as <java.lang.String> host) (as <int> inport)))
  ; need to accept the client

; connect to given host (by hostname) and port
; returns two values: input stream and output stream
(define (tcp-connect in-host :: <java.lang.String> in-port :: <int>)
  (let ((inet-address :: <java.net.InetSocketAddress>
                      (<java.net.InetSocketAddress> in-host in-port))
        (socket :: <java.net.Socket> (<java.net.Socket>)))
    (display "tcp-connect ")(display inet-address)(newline)
    ; connect to server
;    (try-catch (invoke socket 'connect inet-address)
;      (ex <java.lang.Throwable>
;          (begin
;            (display (*:toString ex))(newline)
;            (*:printStackTrace ex)
;            )))
    (try-catch 
        (set! socket (<java.net.Socket> in-host in-port))
      (ex <java.lang.Throwable>
          (begin
            (if (equal? (invoke ex 'get-class) <java.net.ConnectException>)
              (begin
                (display "ex is an connect exception")(display ex)(newline)
                ))
            (display "socket ")(display socket)(newline)
            (display (*:toString ex))(newline)
            (*:printStackTrace ex)
            )))
    
    ; get input/output streams, and return
    
    (let ((input-stream :: <java.io.InputStream>
                        (invoke socket 'getInputStream))
          (output-stream :: <java.io.OutputStream>
                         (invoke socket 'getOutputStream)))
      (values input-stream output-stream))))

; write to a socketOutputStream (standard write in Kawa doesn't work for network)
(define (socket-write in-msg
                      in-stream :: <java.io.OutputStream>)
  ;(display "outstream in sock write ")(display in-stream)(newline)
  (define socket-print-writer
    (<java.io.PrintWriter> in-stream)); 'true1))
  (invoke socket-print-writer 'println in-msg)
  (invoke socket-print-writer 'flush)
  )

; read socketInputStream
;; NOTE: make sure to read faster than you write or your 
;; data will be lost.
(define (socket-read from-stream)
  (let ((br (<java.io.BufferedReader> 
             (<java.io.InputStreamReader> from-stream))))
    (if (invoke br 'ready)
        (begin
          ;(display "br ready reading.. ")
        (invoke br 'read-line))
        )
  ))
