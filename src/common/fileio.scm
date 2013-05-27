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

;; 
;; file I/O: provides loading and saving of data in object.scm format to/from a file as an s-expression
;;

(begin
  (require "datatable.scm") ;; clear-dirty!
  (require "runcode.scm")
  (require "preferences.scm")
  (require "evaluator.scm")
  (require "../kawa/miscutils.scm")
  (require "../kawa/file.scm")
  (require "../kawa/system.scm")
  (require "../kawa/ui/dialog.scm")
  )

(module-export get-fileformat-version set-fileformat-version!
               get-fileformat-type set-fileformat-type!
               get-saved-filename get-saved-filename-string set-saved-filename!
               set-default-save-dir! get-last-saved-dir set-last-saved-dir!
               get-file-prefs put-file-prefs!
               load-from-file ;load-from-file2
               importing? import-from-file
               set-build-sexpr-callback!
               save-to-file save-to-sexpr
               get-safe-new-filename
               parse-sexpr-file
               write-sexpr-file
               get-content-file)
(module-static 'init-run)

; keep track of file format version number and file type
(define fileformat-version 1)
(define (get-fileformat-version)
  fileformat-version)
(define (set-fileformat-version! new-version)
  (set! fileformat-version new-version))

(define loaded-fileformat-version 0)
(define loaded-fileformat-type #f)
(define fileformat-type 'generic)
(define (get-fileformat-type)
  fileformat-type)
(define (set-fileformat-type! new-type)
  (set! fileformat-type new-type))
(define saved-filename #f)
(define (set-saved-filename! newfilename)
  (set! saved-filename newfilename))
(define (get-saved-filename)
  saved-filename)
(define (get-saved-filename-string)
  (if (not (eq? saved-filename #f))
      (get-file-name saved-filename) #f))

; allow application to specify a default save directory
(define default-save-dir #!null)
(define (set-default-save-dir! in-dir)
  (set! default-save-dir in-dir))

; last saved directory, a <java.io.File>, or #!null if none
(define last-saved-dir #!null)
(define (get-last-saved-dir)
  (if (not-null? last-saved-dir)
      last-saved-dir
      ; if no last saved dir, use default, and if no default, use the current directory
      (if (not-null? default-save-dir)
          default-save-dir
          (make-file (get-user-directory)))))
(define (set-last-saved-dir! newdir)
  (set! last-saved-dir newdir))
(define (update-last-saved-dir)
  (set! last-saved-dir (if (not (eq? saved-filename #f))
                           (make-file (path-directory saved-filename))
                           #!null)))

; prefs
(define (get-file-prefs)
  (let ((dirinfo (get-pref 'dirinfo)))
    (if (and dirinfo (not (is-null? dirinfo)))
        (set-last-saved-dir! (make-file dirinfo)))))
(define (put-file-prefs!)
  (let ((the-dir (get-last-saved-dir)))
    (if (not (is-null? the-dir))
        (put-pref! 'dirinfo (path-file the-dir)))))

;Reading sexpr from a file; returns true on success, false on failure
(define (read-sexpr-file filename)
  ; first check for existence
  (let* ((safetoproceed #t)
         (filereallyexists
          (try-catch (file-exists? filename)
            (ex <java.lang.Throwable>
                (begin
                  (display (*:toString ex))(newline)
                  ;(*:printStackTrace ex)
                  )))))
    (if (and safetoproceed filereallyexists)
        (try-catch
            (let
                ((explist (open-input-file filename)))
              ; run the file
              (runcode-exp explist display display)
              ; close input port
              (close-input-port explist)
              
              #t)
          (ex <java.lang.Throwable>
              (begin
                (display (*:toString ex))(newline)
                (*:printStackTrace ex)
                #f
                )))
        #f)))

;; same as read-sexpr-file just that this one 
;; only parse instead of running it
(define (parse-sexpr-file filename proc)
  ; first check for existence
  (let* ((safetoproceed #t)
         (filereallyexists
          (try-catch (file-exists? filename)
            (ex <java.lang.Throwable>
                (begin
                  (display (*:toString ex))(newline)
                  ;(*:printStackTrace ex)
                  )))))
    (if (and safetoproceed filereallyexists)
        (try-catch
            (let
                ((explist (open-input-file filename)))
              ; parse  the file
              ;(parsecode-sexpr explist (lambda (e) (display "[code] ")(display e)(newline)))
              (parsecode-sexpr explist proc)

              ; close input port
              (close-input-port explist)
              
              #t)
          (ex <java.lang.Throwable>
              (begin
                (display (*:toString ex))(newline)
                (*:printStackTrace ex)
                #f
                )))
        #f)))

; load data structure from file
(define (load-from-file filename . reset?)
  
  ; reset environment
  ;; if #f to reset? then don't reset environment
  (if (or (null? reset?)
          (car reset?))
      (reset-environment))

  ; read sexpr file into environment
  (if (read-sexpr-file filename)
      (begin
        ; remember filename and directory
        (set-saved-filename! filename)
        (update-last-saved-dir)

        ; mark newly loaded file as clean
        (clear-dirty!)))
  )

; import flag:
; languages can use this to check whether to offset object IDs when importing
(define-private import-flag #f)

; set import flag
(define-private (set-import-flag! in-flag)
  (set! import-flag in-flag))

; check import flag
(define (importing?)
  import-flag)

; import data structure from file
(define (import-from-file filename)
  (let ((return-value #f))
    ; set importing flag
    (set-import-flag! #t)

    ; read sexpr file into environment without resetting environment
    ; note: also don't reset dirty flag as file has changed
    (set! return-value (read-sexpr-file filename))

    ; unset importing flag
    (set-import-flag! #f)
    
    ; return whether or not read succeeded
    return-value))

;Writing an sexpr to a file, returns #t if successful, or #f if failed:
(define (write-sexpr-file filename mysexpr #!optional write-permit-error-callback)
  ;;(format #t "writing to file: ~a~b~%~!" filename mysexpr)
  ; first check for overwrite
  (let* ((safetoproceed #t)
         (overwrite
          (try-catch
              (file-exists? filename)
            (ex <java.lang.Throwable>
                (begin
                  (display (*:toString ex))(newline)
                  (set! safetoproceed #f)
                  ;(*:printStackTrace ex)
                  )))))
    (begin
      ; if necessary, delete the existing file
      (format #t "overwrite: ~a, safetoproceed: ~a~%~!" overwrite safetoproceed)
      (if safetoproceed
          (begin
            (if overwrite
                (try-catch
                    (delete-file filename)
                  (ex <java.lang.Throwable>
                      (begin
                        (display (*:toString ex))(newline)
                        (set! safetoproceed #f)
                        (display "DELETE FAILED ")(newline)
                        
                        ;; error handling if any
                        (if (procedure? write-permit-error-callback)
                            (write-permit-error-callback))
                        
                        ;(*:printStackTrace ex)
                        ))))

            ; now go ahead and write
            (if safetoproceed
                (begin
                  (format #t "safetoproceed: ~a~%~!" safetoproceed)
                  (let ((output-port
                         (try-catch
                             (open-output-file filename)
                           (ex <java.lang.Throwable>
                               (begin
                                 (display (*:toString ex))(newline)
                                 ;(*:printStackTrace ex)
                                 (set! safetoproceed #f)
                                 
                                 ;; error handling if any
                                 (if (procedure? write-permit-error-callback)
                                     (write-permit-error-callback))
                                 )))))
                    
                    (display "before put ")(newline)
                    (format #t "output-port: ~a~a~%~!" output-port (is-void? output-port))
                    
                    (if safetoproceed
                        (begin
                          (format #t "writing~%~!")
                          (write mysexpr output-port)
                          (close-output-port output-port)
                          #t)
                        #f)))
                #f))))))

; install build-sexpr-callback for implementation-specific tags
(define build-sexpr-callback #f)
(define (set-build-sexpr-callback! new-callback)
  (set! build-sexpr-callback new-callback))

; build an sexpression from data structure
(define (build-sexpr)
  (append (list 'begin
                (list 'make-hypertext (list 'quote fileformat-type) fileformat-version))
          ; implementation-specific data
          (if build-sexpr-callback
              (build-sexpr-callback)
              '())))

; save data structure to file
(define (save-to-file filename silent? #!optional write-permit-error-callback)
  (if (write-sexpr-file filename (build-sexpr) (if silent? #f write-permit-error-callback))
      (if (not silent?)
          (begin
            (set-saved-filename! filename)
            (update-last-saved-dir)
            (clear-dirty!))
          #f)
      #f))

; save data to sexpr
(define (save-to-sexpr)
  (build-sexpr))

; save arbitrary sexpr to file
(define (save-sexpr-to-file filename lst-expr)
  (write-sexpr-file filename lst-expr))

;;
;; utilities
;; 

; get new filename, checking for overwrite;
; Note: only need to do this if NOT MacOS, since MacOS uses java.awt.FileDialog, which does this automatically
; in-dir: default directory where dialogue opens
; select-dir: #t if user is selecting a directory, #f if selecting a file
; filetype: extension for 
; returns filename if can proceed, #f otherwise

;; TODO; choosing dir to save to does not seem to work. highlighting the dir you would like to overwrite 
;; does not bring the name of that dir up in the name
;; cant overwrite dir that way
(define (get-safe-new-filename in-dir select-dir filterlist #!optional default-name default-extension)
  (let ((newfilename (get-file-to-save in-dir select-dir filterlist default-name default-extension)))
    
    (if (is-mac-os?)
        ; mac, so assume java.awt.FileDialog has already checked, don't check twice
        newfilename
        ; otherwise must check
        (if (not (eq? #f newfilename))
            (let* ((safetoproceed #t)
                   (overwrite
                    (try-catch
                        (file-exists? newfilename)
                      (ex <java.lang.Throwable>
                          (begin
                            (display (*:toString ex))(newline)
                            (set! safetoproceed #f)
                            ;(*:printStackTrace ex)
                            )))))
              (if safetoproceed
                  (if overwrite
                      ; duplicate filename, so check first
                      (let ((reply (make-confirm-dialogbox
                                    #!null 3
                                    (string-append "A file named \"" (get-file-name newfilename)
                                                   "\" already exists,\ndo you want to replace it?")
                                    "Warning: file exists")))
                        (cond ((eq? 1 reply) newfilename) ; yes, so return the new filename
                              ((eq? 2 reply) #f) ; cancel, so return #f
                              (else (get-safe-new-filename in-dir select-dir filterlist default-name default-extension)))) ; no, so ask again
                      ; not duplicate, so return filename
                      newfilename)
                  ; not safe, so an exception occurred
                  #f))
            ; no filename, so cancelled
            #f))))


; helper, adapted from Processing, for getting file relative to the app
; http://code.google.com/p/processing/source/browse/trunk/processing/app/src/processing/app/Base.java?r=7522
(define (get-content-file in-name)
  (let ((the-path (get-system-property "user.dir")))
    ; Get a path to somewhere inside the .app folder
    (if (is-mac-os?) 
;      <key>javaroot</key>
;      <string>$JAVAROOT</string>
        (let ((javaroot (get-system-property "javaroot")))
          (if (not (is-null? javaroot))
              (set! the-path javaroot))))
    (make-file (string-append the-path "/" in-name))))
