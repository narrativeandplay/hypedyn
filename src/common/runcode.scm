;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2012
;; National University of Singapore
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

; execute some code
(begin
  (require "evaluator.scm")
  (require "graphics.scm")
  (require "inspector.scm")
  (require "../kawa/miscutils.scm") ;; for easy-try-catch
  )

; export
(module-export runcode runcode-exp runcode-just-sexpr parsecode-sexpr string->sexpr)
(module-static 'init-run)

; need to get the text, convert to s-expression using read,
; and then feed it into our eval;
; then have to convert the resulting s-expression into a
; string and send to the text editor... phew!

;
; execute the contents of a string:
; opens an input string from the string and passes to helper fn
(define (runcode txt result-callback status-callback)
  (let ((explist (open-input-string txt)))
    (status-callback txt)
    (set-end-of-code #f)
    (runcode-exp explist result-callback status-callback)))

; helper fn to recursively execute s-expressions read
; from the given input-string
(define (runcode-exp explist result-callback status-callback)
  (define input 
    (custom-try-catch 
     (lambda () (read explist))
     #t
     (lambda (ex)
       (display "error from runcode-exp ")(newline)
       (display (*:toString ex)))))
    
  (cond
   ((eof-object? input)
    ; reached then end-of-file,, so refresh the graphical display
    ;(status-callback "eof...")
    (if (eq? #t (usegraphics?))
        (begin
          (refresh)
          ))
    (set-end-of-code #t)
    )
   ;((eq? #f (is-void? input))
   (else
    ; run this expression
    ;(status-callback "before myeval...")
    (define output
      (custom-try-catch
       (lambda ()
         (myeval input)
         )
       #f ;; dont do standard print stack
       ;; catch errors
       ;; should actually stop here and show error line to user
       (lambda (ex ::  <java.lang.Throwable>)
         (display "error2 from runcode-exp ")(newline)
         (display (*:toString ex))
         (*:printStackTrace ex)
         )
       ))

    ; display non-void results
    (if (not (equal? output #!void))
        (result-callback output))

    ; update inspectors - should this also be inside the REPL?
    (update-inspectors)

    (runcode-exp explist result-callback status-callback))))

;; open-input-file
(define (string->sexpr code-str :: <string>)
  (define input-port (open-input-string code-str))
  (define (one-element? port) ;; check whether the whole code is one closure example (begin (dothis) (dothat)) or not (dothis)(dothat)
    ;; read one time
    (define first-read (custom-try-catch
                        (lambda () (read port))
                        #t
                        (lambda (ex)
                          (display "[ERROR] from string->sexpr ")(newline)
                          (display (*:toString ex)))))
    (define second-read (read port)) ;; assuming if first read went ok second read should be fine
    ;; read second time
    (and (not (eof-object? first-read))
         (eof-object? second-read)))
  
  ;; create a list of all the sexpr (converted from string via read)
  (define (helper port)
    (define input
      (custom-try-catch
       (lambda () (read port))
       #t
       (lambda (ex)
         (display "[ERROR] from string->sexpr ")(newline)
         (display (*:toString ex)))))
    (cond
     ((eof-object? input)
      '())
     (else 
      (append (list input) (helper port))))
    )
  
    (if (one-element? input-port)
        (begin
          ;; if in one closure get that single element from the list returned by helper
          (car (helper (open-input-string code-str)))) ;; note input-port has been read and need to load it again
          
        (begin
          ;; if not in one closure add a begin in from of the enclosing list returned by helper
          (cons 'begin (helper (open-input-string code-str))))
      )
  )

(define runcode-just-sexpr myeval)

;; run the proc on the whole code (used to find a certain line in the code to retrieve information)
(define (parsecode-sexpr input-port proc)
  (define input 
    (custom-try-catch 
     (lambda () (read input-port))
     #t
     (lambda (ex)
       (display "error from runcode-exp ")(newline)
       (display (*:toString ex)))))
  
  (define (parse-helper explist)
    (if (pair? explist)
        (begin
          ;(display "proc ")(display (procedure? proc))(newline)
          (if (procedure? proc)
              (proc explist))
          (map (lambda (e)
                 (parse-helper e)
                 ) explist)
          ))
    )
  
  (cond
   ((eof-object? input)
    (display "end of code ")(newline)
    (set-end-of-code #t)
    )
   (else
    (parse-helper input)
    (parsecode-sexpr input-port (if (pair? proc) (car proc)))))
  )