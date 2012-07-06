; network stuff

;; network.scm defines servers and clients.
;; it is assumed that every instance of a software doesnt run a client AND a server at the same time 
;; (which is a bad assumption to be fixed later)
;; a server has a read thread and a send thread.
;; a client too has a read thread and a send thread

;; the connection to the server is a special variable called server-connection 
;; any message received from there is assumed to be from the server and handled by handle-server-msg

;; the server has three procedure that determines it's behaviors 
;; namely handle-client-joined handle-client-left handle-client-message

;; the client has two procedure 
;; handle-server-disconnect handle-server-msg


; to do:
; - handle client disconnection
; - server as a thread in same app as client
; - catch exceptions 

; bugs
; - currently doesn't seem to always recognize closed connection, then later tried to write to it
; leading to an exception.

(begin
  ; use data table from datastructure.scm
  (require "objects.scm")

  ; kawa-specific network code
  (require "../kawa/network-kawa.scm")

  ; threads
  (require "thread.scm")

  )

; export
(module-export start-network-client ;stop-network-client
               connect-to-server disconnect-from-server
               send-to-server
               
               start-server ;stop-server
               send-to-client send-to-others send-to-all
               server-connection set-server-loop
               set-client-send-proc
               server-pause server-continue
               client-pause client-continue
               
               set-handle-server-msg! set-handle-server-disconnect!
               )
(module-static 'init-run)
;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; client
;;
;;;;;;;;;;;;;;;;;;;;;;;;

; server connection
(define server-connection '())

; listening thread
(define-variable listening-thread '())

;; Note start-network-client no longer requires handle-server-msg and handle-server-disconnect
;; to be passed to it. instead it an empty procedure is used until a proper handler server msg is set
;; to the network

; start a network client: create a thread to listen to server;
; at this point just start the thread and store callback
(define (start-network-client)
;  (set-handle-server-msg! new-handle-server-msg)
;  (set-handle-server-disconnect! new-handle-server-disconnect)
  (set! client-send-thread
        (make-thread (lambda ()
                       (if client-send-proc
                           (try-catch
                               (client-send-proc)
                             (ex <java.lang.Throwable>
                                 (begin
                                   (display "error in client-send-proc")(newline)
                                   (display (*:toString ex))(newline)
                                   (*:printStackTrace ex)
                                   ))
                             )))
                     50)) ;; setting to 110 was 50 (sync with offline ctg simulation-loop rate) (TOFIX)
  (ask client-send-thread 'wakeup)
  (ask client-send-thread 'start)
  
  (set! client-read-thread (make-thread client-loop 1))
  (ask client-read-thread 'wakeup)
  (ask client-read-thread 'start)
  )

; stop the client
;(define (stop-network-client)
;  ; stop listening thread
;  (if (not (eq? '() listening-thread))
;      (begin
;        (display "killing listening thread...")
;        ;; this the Dr Scheme way of killing thread?
;        ;(kill-thread listening-thread)
;        ;(set! listening-thread '())
;        ))

;  ; shut down the connection
;  (if (not (eq? '() server-connection))
;      (begin
;        (display "closing server connection...")
;        (delete-server-connection))))

; handle-server-msg: callback to handle messages from the server;
; takes one parameter, the message to be handled (a list).
; first item of message is message-type (symbol)
; the rest of the message is dependent on the type of message
(define handle-server-msg '())
(define (set-handle-server-msg! new-handle-server-msg)
                (set! handle-server-msg new-handle-server-msg))

; handle-server-disconnect: callback to handle disconnection;
; takes no parameters.
(define-variable handle-server-disconnect '())
(define (set-handle-server-disconnect! new-handle-server-disconnect)
                (set! handle-server-disconnect new-handle-server-disconnect))


(define client-send-proc #f)
(define (set-client-send-proc act)
  (set! client-send-proc act))

(define client-send-thread #f)
(define (client-pause)
  (ask client-send-thread 'sleep))
(define (client-continue)
  (ask client-send-thread 'wakeup))

; main client loop (client read thread)
(define-private (client-loop)
                ; is there a server connection?
                (if (and (not (eq? '() server-connection)) (ask server-connection 'connected?))
                    ; check for message from server, '() means no message pending
                    (let ((reply (ask server-connection 'read-message)))
                      ; got message, so process the message
                      (if (not (equal? reply #!void))
                          (begin
                            (try-catch
                                (handle-server-msg reply)
                              (ex <java.lang.Throwable>
                                  (begin
                                    (display "error in client-loop (read)")(newline)
                                    (display (*:toString ex))(newline)
                                    (*:printStackTrace ex)
                                    )))))
                      ;; need to know when the connection is broken and close the connection
                      )))

;;new client loop implementation (client read thread)
(define client-read-thread #f)


; connect to the server
(define (connect-to-server myname server-host service-port)
  (create-server-connection server-host service-port)
  (display "server-connection created? ")(display server-connection)(newline)
  (if (not (eq? '() server-connection))
      (begin
        (ask server-connection 'connect myname))))

; disconnect from the server (but don't stop the listening thread)
(define (disconnect-from-server)
  (delete-server-connection))

; send a message to the server
;(define (send-to-server message-type message-body)
;  (if (not (eq? '() server-connection))
;      (ask server-connection 'send-message (list message-type message-body))))

(define (send-to-server msg)
  (if (not (eq? '() server-connection))
      (ask server-connection 'send-message msg)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Server
;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define server-read-thread #f)
(define server-send-thread #f)
(define (server-pause)
  (ask server-send-thread 'sleep))
(define (server-continue)
  (ask server-send-thread 'wakeup))
; server thread
(define-variable connection-request-thread #f)
(define server-loop #f)
(define (set-server-loop new-server-loop)
  (set! server-loop new-server-loop))

; the main server process: start listening for clients;
; - check if anyone is waiting to be accepted
; - if there is, accept them (create client record, add to list)
; - cycle through all clients and check for incoming messages
; - for each incoming message, process the message
; (also, if any clients died, handle this)
; handle-client-joined callback takes 1 paramenter: clientID
; handle-client-left callback takes 1 paramenter: clientID
; Note: client connection-obj is deleted from the data table AFTER this callback
; returns, so server can still access the connection-obj
; handle-client-message callback takes 3 parameters: sourceID, message-type and message-body
; Also note that this procedure starts a thread for the server, and then returns...
; - need to trap failure
(define start-server
  (lambda (service-port handle-client-joined handle-client-message) ;; handle-client-left not done yet
    ; create a listener

    (let ((listener
           (try-catch
               ;; this creates a serversocket 
               (tcp-listen service-port)
             (ex <java.lang.Throwable>
                 (begin
                   (display (*:toString ex))(newline)
                   (display "if it says 
'Unrecognized Windows Sockets error: 0: JVM_Bind' 
choose another service port number")(newline)
                   ;(*:printStackTrace ex)
                   ))))
          ;; new after porting to kawa
          (connection-list '())
          )

      (display "listener ")(display listener)(newline)
      ; start server thread

      (if listener
          (begin
            ;; this thread keeps listening for connection
;            (set! connection-request-thread
;                  (future ; starts a new thread
;                   ;(lambda ()
;                     ; main server loop
;                     (let server-connection-loop ()
;                       (begin
;                         ;(sleep 0.05)
;                         (begin
;                           (accept-client listener handle-client-joined))

;                         ; and loop
;                         (server-connection-loop)))))
            (set! connection-request-thread
                  (make-thread
                   (lambda ()
                     (accept-client listener handle-client-joined)) 50))
            (ask connection-request-thread 'wakeup)
            (ask connection-request-thread 'start)


            ;;equivalent of process client
            (set! server-read-thread
                  (make-thread
                   (lambda ()
                     (begin
                       (try-catch
                           (begin
                             (process-clients handle-client-message) ;;  handle-client-left taken out for the moment
                             )
                         (ex <java.lang.Throwable>
                             (begin
                               (display "server-read-thread error ")(newline)
                               (display (*:toString ex))(newline)
                               (*:printStackTrace ex)
                               ))))
                     ) 1))
            (ask server-read-thread 'wakeup)
            (ask server-read-thread 'start)

            (set! server-send-thread
                  (make-thread
                   (lambda ()
                     (try-catch
                         (if server-loop
                             (server-loop))
                       (ex <java.lang.Throwable>
                           (begin
                             (display "server-read-thread error ")(newline)
                             (display (*:toString ex))(newline)
                             (*:printStackTrace ex)
                             ))))
                   15))
            (ask server-send-thread 'wakeup)
            (ask server-send-thread 'start)
            ;(gethostname)
            )) ;; end of if listener
      )))

; stop the server
; - should I do a proper tcp-close on the listener?
; - what if the server thread is in the middle of something?
; - will there be synchronization issues here?
;;(define (stop-server)
;;  (if server-thread
;;      (begin
;;        (display "killing server thread...")
;;        (kill-thread server-thread)
;;        (set! server-thread #f))))

;;used by server
; accept a new client - note that here it waits for a name from the client ***
(define-private (accept-client listener handle-client-joined)
                (let* ((socket (tcp-accept listener))
                       (client->me (invoke socket 'getInputStream))
                       (me->client (invoke socket 'getOutputStream)))
                  (try-catch
                      (begin
                        (define name #!void)
                        ;; read from the socket till there is a name received
                        (do ()
                            ((not (equal? name #!void))
                             (begin (display "client name received: ")(display name)(newline)))
                            (begin
                              (set! name (socket-read client->me))))

                        (define clientID (create-connection client->me me->client name))
                        (if (not (eq? '() handle-client-joined))
                            (handle-client-joined clientID)))
                    (ex <java.lang.Throwable>
                        (begin
                          (display "error in accept-client")(newline) 
                          (display (*:toString ex))(newline)
                          (*:printStackTrace ex)
                          )))
                  ))

; client left
(define-private (client-left clientID handle-client-left)
                (if (not (eq? '() handle-client-left))
                    (handle-client-left clientID))
                (delete-connection clientID))

; process clients:
; run through the list of clients, and process any messages from them
(define-private (process-clients handle-client-message) ; handle-client-left taken out since not ready

                (let ((the-connections (get-list 'connections)))
                  (if the-connections
                      (map (lambda (c)
                             (let* ((thisconnection (cdr c))
                                    (thisconnection-inport (ask thisconnection 'in-port))
                                    (thisconnection-ID (ask thisconnection 'ID))
                                    (themessage (ask thisconnection 'read-message)))

                               ;; reading from the connection when there is no message sets themessage to #!void

                               (if (and (not (equal? themessage #!void))
                                        (not (equal? handle-client-message '())))
                                   (handle-client-message thisconnection-ID themessage))
                               ))
                           the-connections)
                      )))

; send to a specific client
;(define (send-to-client client-ID message-type message-body)
;  (let ((the-connection (get 'connections client-ID)))
;    (display "connectino ")(display the-connection)(newline)
;    (if the-connection
;        (ask the-connection 'send-message (list message-type message-body)))))

(define (send-to-client client-ID message)
  (let ((the-connection (get 'connections client-ID)))
    ;(display "client connectino ")(display the-connection)(newline)
    (if the-connection
        (ask the-connection 'send-message message))))

; send to all clients
;(define (send-to-all source-ID message-type message-body)
;  (let ((the-connections (get-list 'connections)))
;    (if the-connections
;        (map (lambda (c)
;               (let ((thisconnection (cdr c)))
;                 (ask thisconnection 'send-message (list message-type message-body))))
;             the-connections))))

(define (send-to-all msg)
  (let ((the-connections (get-list 'connections)))
    (if the-connections
        (map (lambda (c)
               (let ((thisconnection (cdr c)))
                 ;(display "this connection ")(display thisconnection)(newline)
                 (ask thisconnection 'send-message msg)))
             the-connections))))

; send to all clients expect specified client
(define (send-to-others source-ID message-type message-body)
  (display source-ID)
  (let ((the-connections (get-list 'connections))
        (themessage (list message-type message-body)))
    (if the-connections
        (map (lambda (c)
               (let* ((thisconnection (cdr c))
                      (thisconnection-ID (ask thisconnection 'ID)))
                 (if (not (= source-ID thisconnection-ID))
                     (ask thisconnection 'send-message themessage))))
             the-connections))))
;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; connection objects
;;
;;;;;;;;;;;;;;;;;;;;;;;;

; note: these objects make use of the object hierarchy and data table
; in objects.scm

; create a connection: make a connection object, and add it to the data table
(define-private (create-connection in-port out-port name)
                (let* ((new-connection (make-connection name in-port out-port #t))
                       (new-connectionID (ask new-connection 'ID)))
                  (put 'connections new-connectionID new-connection)
                  ;(display (string-append "Added new connection, ID=" (number->string new-connectionID) ", name=" name "\n"))
                  new-connectionID))

; delete a connection: shut down the connection and delete from the data table
(define-private (delete-connection connectionID)
                (display "delete connection called ")(newline)
                (let ((theconnection (get 'connections connectionID)))
                  (if theconnection
                      (begin
                        ; close the ports
                        (ask theconnection 'shutdown)

                        ; delete the connection record
                        (del 'connections connectionID)
                        (display (string-append "Deleted connection, ID=" (number->string connectionID)))))))

; create a server connection: create a server-connection object and store in server-connection
(define-private (create-server-connection server-host service-port)
                ; store connection
                (set! server-connection (make-server-connection '() '() server-host service-port)))

; delete a server connection: delete the connection and clear server-connection
(define-private (delete-server-connection)
                (if (not (eq? '() server-connection))
                    (begin
                      ; shut down the connection
                      (delete-connection server-connection)
                      (set! server-connection '()))))

;; connection: create the connection object;
;; this inherits from uniqueID-obj in datastructure.scm
(define-private (make-connection name in-port out-port connected . args)
                (let ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args)))))
                  (lambda (message)
                    (cond ((eq? message 'in-port) (lambda (self) in-port))
                          ((eq? message 'set-in-port!) (lambda (self new-in-port)
                                                         (set! in-port new-in-port)))
                          ((eq? message 'out-port) (lambda (self) out-port))
                          ((eq? message 'set-out-port!) (lambda (self new-out-port)
                                                          (set! out-port new-out-port)))
                          ((eq? message 'name) (lambda (self) name))
                          ((eq? message 'connected?) (lambda (self) connected))
                          ((eq? message 'set-connected!) (lambda (self new-connected)
                                                           (display "set-connected ")(display new-connected)(newline)
                                                           (set! connected new-connected)))
                          ((eq? message 'send-message) (lambda (self the-message)
                                                         ;; gotta check for missing outport too
                                                         (try-catch
                                                             (begin
                                                             ;(display "outport ")(display out-port)(newline)
                                                             (if (not (equal? out-port '()))
                                                                 (begin
                                                                   (socket-write the-message out-port))))
                                                           (ex <java.lang.Throwable>
                                                               (begin
                                                                 (display (*:toString ex))(newline)
                                                                 (*:printStackTrace ex)
                                                                 )))))
                          ((eq? message 'read-message) (lambda (self)
                                                         (try-catch
                                                             (socket-read in-port)
                                                           (ex <java.lang.Throwable>
                                                               (begin
                                                                 (display (*:toString ex))(newline)
                                                                 (*:printStackTrace ex)
                                                                 )))))
                          ((eq? message 'shutdown) (lambda (self)
                                                     (begin
                                                       (close-output-port out-port)
                                                       (close-input-port in-port))))
                          (else (get-method uniqueID-obj message))))))

;; this is used by client?
;; server connection: start a connection to a server
;; inherits from connection-obj
(define-private (make-server-connection in-port out-port server-host service-port . args)
                (let ((connection-obj (make-connection "server" in-port out-port #f (if (pair? args) (car args)))))
                  (lambda (message)
                    (cond ((eq? message 'server-host) (lambda (self) server-host))
                          ((eq? message 'service-port) (lambda (self) service-port))
                          ((eq? message 'connect)
                           (lambda (self myname)
                             ; connect to the server
                             (let-values (((server->me me->server)
                                           (tcp-connect server-host service-port)))
                               ; remember ports
                               (ask self 'set-in-port! server->me)
                               (ask self 'set-out-port! me->server)
                               (ask self 'set-connected! #t)

                               ; send ID to server
                               (ask self 'send-message myname)
                               )))
                          (else (get-method connection-obj message))))))