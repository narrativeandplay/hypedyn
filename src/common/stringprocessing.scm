(require "../kawa/strings.scm")

(module-export quote-nest preserve-newline preserve-quotes preserve-forward-slash escape-special)

;; ============================
;;;; string processing helper
;; ============================

;; add "\"" to start and end
(define (quote-nest str)
  (string-append "\"" str "\""))

;; preserves \n when we print out the string,
;; replaces "\n" with "\\n"
;; replaces "\r" with ""
(define (preserve-newline str)
  ;(define r-replaced (replace-char str #\x0D ""))
  ;(replace-char r-replaced #\newline "\\n")
  (define r-replaced (string-replace-all "\r" "" str))
  (string-replace-all "\n" "\\n" r-replaced))

(define (preserve-quotes str)
  ;; escaping both \ and " here so that 
  ;; \" shows up in the printed display
  ;(replace-char str #\" "\\\"")
  
  ;(display "preserve quotes ")(newline)
  ;(display "convert1 ")(display (to-string #\"))(newline)
  ;(display "convert2 ")(display (substring (to-string #\") 1 2))(newline)
  ;(display "str ")(display str)(newline)
  (string-replace-all "\"" "\\\"" str)
  )

(define (preserve-forward-slash str)
  (string-replace-all "\\" "\\\\" str))

(define (escape-special str)
  (define retval str)
  (set! retval (preserve-forward-slash retval))
  (set! retval (preserve-quotes retval))
  (set! retval (preserve-newline retval))
  retval)