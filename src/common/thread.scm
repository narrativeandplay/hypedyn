; simple threaded object
(begin
  (require "objects.scm")
  (require "timer.scm") ;; for do-on-the-side
  (require "../kawa/miscutils.scm")
  )

;; mydelay is in milliseconds
;; make-thread will run mycallback repeatedly pausing for mydelay in between each execution
;; the thread object need to be issued both a start and wakeup message

(module-export make-thread
               thread-set-interval
               thread-start
               thread-stop
               thread-sleep
               thread-wakeup
               )
(module-static 'init-run)

(define (make-thread mycallback mydelay) ;;would it be more understandable by calling it interval?
  (let ((named-obj (make-named-object "thread"))
        (wake-flag #f)
        (kill-flag #f)
        (started-flag #f)
        (mythread #f))
    
    (define (myloop)
      ; check if we should wake up
      (if (and wake-flag (procedure? mycallback))
          ;(easy-try-catch mycallback)
          (mycallback)
          )
      (if (not kill-flag)
          (begin
            (sleep (* mydelay 0.001)) ;;convert mydelay in ms to secs
            (myloop)
            )
          ;; mythread set to false is a sure sign that it has terminated
          (set! mythread #f)
          ))
    
    (lambda (message)
      (cond ((eq? message 'start)
             (lambda (self)
               ;(display "start in thread")(newline)
               (set! kill-flag #f)
               (set! started-flag #t)
               ;; changed to only spawn new future thread when previous thread 
               ;; has terminated
               (if (not mythread)
                   (set! mythread
                         (future
                          (myloop))))
               ))
            ((eq? message 'stop)
             (lambda (self)
               (set! kill-flag #t)
               ))
            ((eq? message 'wakeup)
             (lambda (self)
               (set! wake-flag #t)))
            ((eq? message 'sleep)
             (lambda (self)
               (set! wake-flag #f)))
            ((eq? message 'set-delay)
             (lambda (self newdelay)
               (set! mydelay newdelay)))
            ((eq? message 'set-callback)
             (lambda (self new-callback)
               (set! mycallback new-callback)))
            
            ((eq? message 'get-wake-flag)
             (lambda (self)
               wake-flag))
            ((eq? message 'get-kill-flag)
             (lambda (self)
               kill-flag))
            ((eq? message 'get-started-flag)
             (lambda (self)
               started-flag))
            ((eq? message 'get-delay)
             (lambda (self)
               mydelay))
            ((eq? message 'get-thread)
             (lambda (self)
               (if mythread mythread)))

            (else (get-method named-obj message))))))

(define (thread-start thread)
  (ask thread 'start))

(define (thread-wakeup thread)
  (ask thread 'wakeup))

(define (thread-stop thread)
  (ask thread 'stop))

(define (thread-sleep thread)
  (ask thread 'sleep))

(define (thread-set-interval thread intv)
  (ask thread 'set-delay intv))
  

 ;testing
;(define time 0)
;(define (doit)
;  (if (< time 100000000000000000)
;      (begin
;        (set! time (+ time 1))
;        ;(format #t "time: ~a~%~!" time)
;        )
;  ))

;(define (time-him)
;  (sleep 1)
;  (display time)(display " time's up ")(newline)
;  (set! time 0))

;(define my-thread #f)
;(define my-thread2 #f)
;(define (test)
;  (set! my-thread (make-thread doit 0))
;  (set! my-thread2 (make-thread time-him 0))
;  (ask my-thread2 'start)
;  (ask my-thread 'start)
;  (ask my-thread2 'wakeup)
;  (ask my-thread 'wakeup)
;  ;(ask my-thread 'stop)
;  
;  ;(ask my-thread 'start)
;  )
;(test)