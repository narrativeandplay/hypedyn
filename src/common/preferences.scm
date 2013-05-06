;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2013
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

; preferences

(require "fileio.scm")
(require "objects.scm") ;; obj-get
(require "datatable.scm") ;; make-table
(require "../kawa/file.scm")

; export
(module-export get-pref-languageinfo getprefs putprefs
               init-prefs get-pref-list get-pref put-pref! del-pref!)
; (module-static 'init-run)
 
(define pref-file #f)
(define-constant pref-filename "./myprefs")

; get the pref file, returns #f if not found
(define (find-pref-file) 
  (let ((the-file (make-file pref-filename)))
    (if (check-file-exists the-file)
        the-file
        #f)))

; preferences
(define pref-languageinfo #f)
(define (get-pref-languageinfo)
  (if (procedure? get-pref)
      (get-pref 'lang)))

; app-specific prefs can be stored here
(define pref-table #f)
(define get-pref-list #f)
(define get-pref #f)
(define put-pref! #f)
(define del-pref! #f)

; create preferences table to store app-specific prefs
(define (init-prefs)
  (set! pref-table (make-table))
  
  ;; retrieving the procedure defined inside pref-table obj
  (define get-list-proc (obj-get pref-table 'get-list))
  (define get-pref-proc (obj-get pref-table 'get))
  (define put-pref-proc (obj-get pref-table 'put))
  (define del-pref-proc (obj-get pref-table 'del))
  
  ;; only change is that i have to pass the pref-table to the self arg because the proc were 
  ;; originally meant for doing ask
  (set! get-pref-list (lambda ()
                        (get-list-proc pref-table 'prefs)))
  (set! get-pref (lambda (in-pref-id)
                   (get-pref-proc pref-table 'prefs in-pref-id)))
  (set! put-pref! (lambda (in-pref-id in-pref)
                    (put-pref-proc pref-table 'prefs in-pref-id in-pref)))
  (set! del-pref! (lambda (in-pref-id)
                    (del-pref-proc pref-table 'prefs in-pref-id))))

; get preferences
(define (getprefs)
  ; create the preferences table
  (init-prefs)

  ; and load the pref file into it
  (try-catch
      (let ((the-pref-file (find-pref-file)))
        (if the-pref-file
            (let* ((input-port (open-input-file the-pref-file))
                   (the-prefs (read input-port)))
              (set! pref-file the-pref-file)
              (if (not (eof-object? the-prefs))
                  ; run through explist and put into table
                  (map (lambda (this-entry)
                         (let ((this-entry-id (car this-entry))
                               (this-entry-data (cdr this-entry)))
                           (put-pref! this-entry-id this-entry-data)))
                       the-prefs))
              (close-input-port input-port))
            (display "no prefs file.\n")))
    (ex <java.lang.Throwable>
        (begin
          (display (*:toString ex))(newline)
          ;(*:printStackTrace ex)
          ))))

; save preferences:
; lists out the preferences in the pref-table and saves to file
(define (putprefs)
  (if (procedure? get-pref-list)
      (try-catch
          (let ((the-pref-list (get-pref-list))
                (the-pref-file (if (not pref-file)
                                   (let ((the-file (make-file pref-filename)))
                                     (display "no pref file, make one...\n")
                                     (display (create-new-file the-file))
                                     the-file)
                                   (begin
                                     (display "use existing pref file")(display pref-file)(newline)
                                     pref-file))))
            (if (and the-pref-list the-pref-file)
                (let ((output-port (open-output-file the-pref-file)))
                  (write the-pref-list output-port)
                  (close-output-port output-port))))
        (ex <java.lang.Throwable>
            (begin
              (display (*:toString ex))(newline)
              ;(*:printStackTrace ex)
              )))))