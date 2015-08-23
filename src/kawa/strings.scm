;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2015
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

(require "miscutils.scm") ; for is-null?
(require "arrays.scm")
(require 'list-lib)

;; scheme string 
;(require 'srfi-13) string-contains

(module-export string-starts-with? string-ends-with? list-is-numeric? string-is-numeric? string-is-double?
               number->java-string double->java-string java-string->double URL->string string->URL 
               string-indexof string-lastindexof
               jstring->fstring ;fstring->string 
               to-fstring to-string
               string-contains string-replace string-replace-all)

;;
;; strings
;; 

; starts with?
(define (string-starts-with? string :: <java.lang.String> starting :: <java.lang.String>)
  (invoke string 'startsWith starting))

; ends with?
(define (string-ends-with? string :: <java.lang.String> ending :: <java.lang.String>)
  (invoke string 'endsWith ending))

; list contains all numeric string
(define (list-is-numeric? list)
  (let ((return-boolean #t))
    (if (null-list? list)
        (set! return-boolean #t)
        (let loop ((lst list))
          (let ((current (invoke-static <java.lang.Character> 'to-string (as <char> (car lst))))
                (rest (cdr lst)))
            (if (or (equal? current "0") (equal? current "1") (equal? current "2")
                    (equal? current "3") (equal? current "4") (equal? current "5")
                    (equal? current "6") (equal? current "7")(equal? current "8")
                    (equal? current "9")
                    )
                #t ;;do nothing
                (set! return-boolean #f))
            (if (not (null-list? rest))
                (loop rest)
                ))) ;; end of let loop
        ) ;;end of if null-list?
    return-boolean
    ))

(define (string-is-numeric? string :: <String>)
;  (let* (;(length (invoke-static <java.lang.String> 'length string))
;         ;(char-array (invoke-static <java.lang.String> 'toCharArray string))
;         (list (string->list string));(array-to-list char-array length))
;         (return-val #t))
;    (set! return-val (list-is-numeric? list))
;    return-val)
  
  ;; at least one digit - added handling of a leading "-" sign - alex
  (invoke string 'matches "-?[0-9]+") 
  )

(define (string-is-double? string :: <String>)
  (let ((list (string->list string)))
    (if (equal? (car list) #\-)
        (begin (display "minus ")(newline) (set! list (cdr list))))
    (let*((return-val #t)
          (temp (member #\. list)) ;#\.
          (temp-length (if temp (length temp) 0))
          (tail (if (and (> temp-length 0) ;if theres a decimal place
                         temp)             ;temp-length is 0 and tail is empty list
                    (take-right temp (- temp-length 1))
                    '()))
          (head (if (and (> temp-length 0)
                         temp)
                    (drop-right list temp-length)
                    list)))
      (set! list (map (lambda (element)
                        (invoke-static <gnu.text.Char> 'to-scm-readable-string (invoke element 'hash-code))) list))
      (if (and (list-is-numeric? tail)
               (list-is-numeric? head)
               (not (equal? temp-length 1));;if temp-length is 1, string ended with "." which gives a false true as empty list tail is true
               (not (null-list? head)))    ;;if head is empty list means string started with "." which gives a false true as empty list head is true
          #t #f)
      )))

(define (number->java-string num :: <int>)
  (invoke-static <java.lang.Integer> 'to-string num))

(define (double->java-string double :: <double>)
  (invoke-static <java.lang.Double> 'to-string double))

(define (java-string->double double-string <java.lang.String>)
  (display "java-string->double called")(newline)
  (invoke-static <java.lang.Double> 'parse-double double-string))

(define (URL->string in-URL :: <java.net.URL>)
  (invoke in-URL 'to-string))

(define (string->URL in-string)
  (<java.net.URL> in-string))

;; string contains already implemented in srfi 13 (it is not working in srfi 13)
(define (string-contains mystring :: <java.lang.String> substring :: <java.lang.String>)
  (let ((retval (string-indexof mystring substring)))
    (if (>= retval 0)
        #t
        #f)))

(define (string-indexof mystring :: <String> 
                        substring :: <String>
                        #!optional from-index)
  
  (if (and from-index (int? from-index))
      (invoke (as <java.lang.String> mystring) 'indexOf (as <java.lang.String> substring) from-index)
      (invoke (as <java.lang.String> mystring) 'indexOf (as <java.lang.String> substring)))
  
  ;(display "returning index ")(display to-ret)(newline)
  )

(define (string-lastindexof mystring :: <String> substring :: <String>)
  (invoke (as <java.lang.String> mystring) 'lastIndexOf (as <java.lang.String> substring)))
  
(define (to-string object :: <java.lang.Object>) :: <java.lang.String>
  (invoke object 'to-string))

(define (to-fstring object :: <java.lang.Object>) :: <gnu.lists.FString>
  (jstring->fstring (invoke object 'to-string)))

; get a java string from an gnu.lists.FString
;;(define (fstring->string in-string :: <gnu.lists.FString>) :: <java.lang.String>
;;  (invoke in-string 'toString))

(define (jstring->fstring jstring :: <java.lang.String>) :: <gnu.lists.FString>
  (<gnu.lists.FString> jstring))

(define (string-replace str inserted start-index end-index)
  (string-append (substring str 0 start-index) inserted (substring str end-index (string-length str))))

;(define (string-replace-with search-str replace-str in-str)
;  (define find (string-indexof in-str search-str index))
;  (if (not (= find -1))
;      (string-replace in-str replace-str find (+ find (string-length search-str))))
;  )

(define (string-replace-all search-str replace-str in-str)
  (define (helper index str)
    (let ((find (string-indexof str search-str index)))
      (if (not (= find -1))
          (helper (+ find (string-length replace-str))
                  (string-replace str replace-str find (+ find (string-length search-str))))
          str
          )))
  (helper 0 in-str)
  )

;;(define (string-replace2 main-str :: <string> 
;;                        match-str :: <string> 
;;                        replace-str :: <string>)
;;  (define main-len (string-length main-str))
;;  (define match-len (string-length match-str))
;;  (cond ((> match-len main-len) ;; won't find match-str
;;         main-str)              ;; so just replace main-str unchanged
;;        (else
;;         (define (helper main-str start-index end-index) :: <string>
;;           (display "helper ")(display (list start-index end-index))(newline)
;;           (if (<= end-index main-len)
;;               (begin
;;                 (define str-frag (substring main-str start-index end-index))
;;                 (if (equal? str-frag match-str)
;;                     (begin
;;                       (set! main-str
;;                             (string-append (substring main-str 0 start-index)
;;                                            replace-str
;;                                            (substring main-str end-index main-len)))
;;                       (set! main-len (string-length main-str))
;;                     ))
;;                 (helper main-str (+ start-index 1) (+ end-index 1)))
;;               main-str))
;;         (helper main-str 0 match-len)
;;         ))
;;  )

;; (define test-str "a.b.c.")
;; (display "replace test ")(newline)
;;  ;(display "string replace exists? ")(display string-replace)(newline)
;; (display "string before ")(display test-str)(newline)
;; (set! test-str (string-replace2 test-str "." ""))
;; (display "string after ")(display test-str)(newline)