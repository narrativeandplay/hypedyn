;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2014
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

(module-export get-class instantiate-class
               is-null? not-null? is-void? 
               easy-try-catch custom-try-catch try-catch-ignore
               set-mouse-position)

;;
;; misc utils
;; 

;; classes

; get a class at runtime
(define (get-class in-classname)
  (<java.lang.Class>:forName in-classname))

; instantiate a class
(define (instantiate-class in-class)
  (invoke (as <java.lang.Class> in-class) 'newInstance))


;; types

;; note: null? in kawa does not match the java #!null so use is-null? in place of null?
;; note about this note: I think it would be safer to use '() as null, rather than #!null, unless
;; you specifically want a java null - alex
(define (is-null? var)
  (if (eq? var #!null) #t #f))

(define (not-null? var)
  (if (is-null? var) #f #t))

;; check for a java void, ie. #!void
(define (is-void? var)
  (if (equal? #!void var)
      #t #f))


;; for debugging purposes


;; can do anything on exception
;; just pass in a procedure as extra argument 
;; (most common use is debug to print where the exception is from)
(define (custom-try-catch proc printstack? . extra)
   (try-catch
      (proc)
    (ex <java.lang.Throwable>
        (begin
          ;; run extra procedure on exception if given
          (if (not (null? extra))
              (let ((extra-content (car extra)))
                   (if (procedure? extra-content)
                       (extra-content ex))))
          (if printstack?
              (begin
                (display (*:toString ex))(newline)
                (*:printStackTrace ex)))
          ))))

;; easy try catch does something easy - just dump stacktrace on exception
(define (easy-try-catch proc)
  (custom-try-catch proc #t))

;; we don't print the stacktrace hence ignoring the red errors
(define (try-catch-ignore proc)
  (custom-try-catch proc #f))

; control the mouse position
(define (set-mouse-position x y)
  (let ((robot (<java.awt.Robot>)))
    (invoke robot 'mouse-move x y)))

;(define (set-mouse-position x y a)
;  (invoke-static <java.awt.Robot> 'mouseMove x y a))