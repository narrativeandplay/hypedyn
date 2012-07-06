(require "thread.scm")
(require "objects.scm")
(require 'hash-table)

;; just for truncate decimal
(require "math.scm")

;; TODO: change the timer-get-time ugly name to something 
;; more understandable like time-stamp 

(module-export start-timer pause-timer resume-timer reset-timer stop-timer
               get-min-sec-string
               get-elapsed-sec get-elapsed-min
               do-on-the-side
               delayed-fire
               side-delayed-fire
               timer-get-time
               reset-interval
               )
(module-static 'init-run)

;; the longest amount of time we can get delta time (time passed)
;; with timer-get-time
(define reset-interval 60000)

;; accurate up to 1 second
(define (create-time-keeper)
  (let ((ms-elapsed 0)
        (sec-elapsed 0)
        (min-elapsed 0)
        (hour-elapsed 0))
    
    (lambda (message)
      (cond ((equal? message 'tick)
             (lambda (self)
               (set! ms-elapsed (+ ms-elapsed 1))
               
               
               ;; everytime ms-elapsed hit 1000 
               ;; increment sec-elapsed
               (if (equal? (mod ms-elapsed 1000) 0);(equal? ms-elapsed 1000)
                   (begin
                     (set! sec-elapsed (+ sec-elapsed 1))
                     ))
               ;; we don't want ms-elapsed to get too big (reset every 60 secs)
               (if (equal? ms-elapsed reset-interval)
                   (set! ms-elapsed 0))
               
               (if (equal? sec-elapsed 60) ;; was 60000
                   (begin
                     (set! min-elapsed (+ min-elapsed 1))
                     (set! sec-elapsed 0)))
               (if (equal? min-elapsed 60)
                   (begin
                     (set! hour-elapsed (+ hour-elapsed 1))
                     (set! min-elapsed (+ min-elapsed 0))
                     ))
               
               ;(display "ms-elapsed ")(display ms-elapsed)(newline)
               ))
            ((equal? message 'reset)
             (lambda (self)
               (set! ms-elapsed 0)
               (set! sec-elapsed 0)
               (set! min-elapsed 0)
               (set! hour-elapsed 0)
               ))

            ;; should not give the internal ms-elapsed that goes up to 60000
            ;; just give remainder when divided by 1000
            ((or (equal? message 'get-time)
                 (equal? message 'get-ms-elapsed))
             (lambda (self)
               (remainder ms-elapsed 1000)))
            ((equal? message 'get-sec-elapsed)
             (lambda (self)
               sec-elapsed))
            ((equal? message 'get-min-elapsed)
             (lambda (self)
               min-elapsed))
            ((equal? message 'get-hour-elapsed)
             (lambda (self)
               hour-elapsed))
            ))
    ))

(define time-keeper (create-time-keeper))

;; tick the counter in time-keeper every 1 ms
(define time-ticker 
  (make-thread (lambda ()
                 (ask time-keeper 'tick)) 1)) ;; was 1000

(define (start-timer)
  (ask time-ticker 'wakeup)
  ;; should only start once in a thread
  (if (not (ask time-ticker 'get-started-flag))
      (ask time-ticker 'start))
  (ask time-keeper 'reset)
  )

(define (pause-timer)
  (ask time-ticker 'sleep)
  )

(define (timer-get-time)
  (ask time-keeper 'get-time))

(define (resume-timer)
  (ask time-ticker 'wakeup)
  (display "resume timer")(newline)
  ;(ask time-print-thread 'wakeup)
  )

(define (stop-timer)
  (ask time-ticker 'sleep)
  (ask time-ticker 'stop))

(define (reset-timer)
  (ask time-ticker 'sleep)
  (ask time-keeper 'reset)
  )

;; in seconds
(define (get-elapsed-sec)
  ;(quotient (ask time-keeper 'get-time) 1000)
  ;(ask time-keeper 'get-time)
  (ask time-keeper 'get-sec-elapsed)
  )

(define (get-elapsed-min)
  (ask time-keeper 'get-min-elapsed)
  )

(define (get-min-sec-string)
  (string-append (invoke (ask time-keeper 'get-min-elapsed) 'to-string) ":"
                 (invoke (ask time-keeper 'get-sec-elapsed) 'to-string) "."
                 (invoke (ask time-keeper 'get-ms-elapsed) 'to-string)))

;(define (time-printer)
;  (display "timer-time ")
;  (display (get-min-sec-string))(newline)
;  )

;(define time-print-thread (make-thread time-printer 1000))

(define self #f)
(define (delayed-fire proc delay-time . proc-args)
  (sleep delay-time)
;  (if (null? proc-args)
;      (proc)
;      (proc proc-args)
;      )
  (apply proc proc-args)
  )

(define (side-delayed-fire proc delay-time . proc-args)
  (do-on-the-side 
   (lambda ()
     ;(delayed-fire proc delay-time)
     (apply delayed-fire (append (list proc delay-time) proc-args)) 
     ))
  )

;; note i assume the proc finish after 99 seconds, optionally pass in delay
(define (do-on-the-side proc . args)
  (define self #f)
  (set! self
        (make-thread 
         (lambda () 
           (proc)
           ;; terminate the thread after we're done with proc
           (ask self 'stop)
           (ask self 'sleep)
           (set! self #f))
           (if (pair? args) 
               (car args) 
               99000))) ;; one time only
  (ask self 'start)
  (ask self 'wakeup)
  ;; in case someome wants a reference to this
  self)