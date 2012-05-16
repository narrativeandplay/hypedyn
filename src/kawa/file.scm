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

;;;;;;;;;;;;;;;
;; file io
;;;;;;;;;;;;;;;

(require "miscutils.scm") ; for (is-null? x)
(require "arrays.scm")
(require "../kawa/system.scm") ; for get-system-property
(require "../kawa/strings.scm") ; for string-ends-with?, string-contains
;(require 'srfi-13) ;; string-contains

(module-export make-file create-new-file make-directory set-file-data append-file-data get-file-to-save get-file-to-open
               get-file-separator get-user-directory get-file-data
               lines-in-file get-file-from-directory get-directory-listing recursively-delete-directory recursively-copy-directory
               get-file-name get-file-absolutepath check-file-exists check-file-content
               copy-file-nio filename-extension-check)
 

;make file (open existing file with name filename OR 
;; create a file with name filename)
(define (make-file filename :: <String>)
  (<java.io.File> (as <java.lang.String> filename)))

; actually create file in filesystem
(define (create-new-file file :: <java.io.File>)
  (invoke file 'createNewFile))

;make directory
(define (make-directory dirname :: <String>)
  (let(( dir (invoke (<java.io.File> (as <java.lang.String> dirname)) 'mkdir)))
    ;(format #t "dir: ~a~%~!\n" dir)
    dir))

;write data into file
(define (set-file-data file :: <java.io.File> datalines)
  (let(( dos (<java.io.DataOutputStream> (<java.io.FileOutputStream>
                                          file #f)))); set to true if appending required
    (invoke dos 'writeBytes datalines)
    (invoke dos 'close)))

(define (append-file-data file :: <java.io.File> datalines)
  (let(( dos (<java.io.DataOutputStream> (<java.io.FileOutputStream>
                                          file #t)))); set to true if appending required
    (invoke dos 'writeBytes datalines)
    (invoke dos 'close)))

; Note: for file dialogs, use java.awt.FileDialog for MacOS, as it looks better, but
; use javax.swing.JFileChooser for Windows, as FileDialog on Windows doesn't allow 
; for directory selection

; open file dialog to save a file
; select-dir: #t if user is selecting a directory, #f if user is selecting a file
; filterlist: list of filetype filter(s)
(define (get-file-to-save dir :: <java.io.File> select-dir filterlist #!optional default-name default-extension)
  (if (is-mac-os?)
      (show-file-dialog dir "Save..." <java.awt.FileDialog>:SAVE select-dir filterlist default-name default-extension)
      (show-jfilechooser dir 'showSaveDialog select-dir filterlist default-name default-extension)))

; open file dialog to load a file
; select-dir: #t if user is selecting a directory, #f if user is selecting a file
; filterlist: list of filetype filter(s)
(define (get-file-to-open dir :: <java.io.File> select-dir filterlist)
  (if (is-mac-os?)
      (show-file-dialog dir "Load..." <java.awt.FileDialog>:LOAD select-dir filterlist)
      (show-jfilechooser dir 'showOpenDialog select-dir filterlist)))

; common code for file dialog, flag to select directories, and list of filetype filter(s)
; note that for Windows, only the first filetype filter is used
(define (show-file-dialog dir :: <java.io.File> title mode select-dir filterlist #!optional default-name default-extension)
  ; now create the jfilechooser
  (let ((fchooser :: <java.awt.FileDialog> (<java.awt.FileDialog> (<javax.swing.JFrame>) title mode)))
    (invoke fchooser 'setDirectory (if (is-null? dir) (get-user-directory) dir))
    
    ; select directory? (only works on macos)
    (if (and select-dir (is-mac-os?))
        (begin
          (format #t "setting apple.awt.fileDialogForDirectories~%~!")
          (set-system-property "apple.awt.fileDialogForDirectories" "true")))

    ; file filter
    (if (not (null? filterlist))
        ;(if (is-windows?)
        ;    ; FilenameFilter is broken in Windows so use a hack instead
        ;    (invoke fchooser 'setFile (string-append "*" (car filterlist)))
            ; on other platforms, use FilenameFilter
            (invoke fchooser 'setFilenameFilter
                    ; anonymous class to filter file list
                    (object (<java.io.FilenameFilter>)
                      ((accept dir :: <java.io.File> name :: <java.lang.String>) ::boolean
                       (let ((result #f))
                         ; check whether the file ends with any of the permitted file extensions
                         (map (lambda (this-entry)
                                (set! result (or result (string-ends-with? name this-entry))))
                              filterlist)
                         result))
                      ((get-description) :: <java.lang.String>
                       (let ((the-string ""))
                         (map (lambda (this-entry)
                                (set! the-string (string-append the-string
                                                                " "
                                                                "*"
                                                                this-entry)))
                              filterlist)
                         (invoke the-string 'to-string)))
                      )))
        ;)
    
   (if default-name 
       (invoke fchooser 'setFile default-name))
    
    ; show the dialog
    (invoke fchooser 'show)
    
    ; put this back the way we found it
    (if (and select-dir (is-mac-os?))
        (set-system-property "apple.awt.fileDialogForDirectories" "false"))
    
    ; extract the response
    (let ((chosenFile (invoke fchooser 'getFile))
          (chosenDir (invoke fchooser 'getDirectory)))
      
      (if (is-null? chosenFile)
          #f
          (begin
            ;; if filename does not contain . (ie no extension given) append with extension
            (if default-extension
                (set! chosenFile (filename-extension-check chosenFile default-extension)))
            
            (make-file (string-append chosenDir (get-file-separator) chosenFile))
            ))
      )))

(define (filename-extension-check file-name :: <string>
                                  extension :: <string>)
  ;; make sure file-name is a java string
;;  (if (and (not (boolean? file-name))
;;           (not (java.lang.String? file-name)))
;;      (set! file-name (to-string file-name)))
;;  ;; make sure extension is a java string
;;  (if (and (not (boolean? extension))
;;           (not (java.lang.String? extension)))
;;      (set! extension (to-string extension)))
  (display "file extension check ")(display file-name)(newline)
  (display "extension ")(display extension)(newline)
  (if (or (not (string-contains file-name "."))
          (string-ends-with? file-name ".")  ;; freak case of . in the end
          (string-starts-with? file-name ".")) ;; freak case of . in front 
      (if (or (string? extension)
              (java.lang.String? extension))
          (begin
            ;; make sure extension is a java string
            (set! extension (to-string extension))

            ;; make sure extension is in the correct form (starts with a ".")
            (if (not (string-starts-with? extension "."))
                (set! extension (string-append "." extension)))

            ;; make sure new file ends with default extension
            (set! file-name (string-append file-name extension))
            ))
      )
  file-name)
  
; common code for jfilechooser, flag to select directories, and list of filetype filter(s)
; get file to save from file chooser, starting in dir,
; or in user's default directory if dir is #!null
(define (show-jfilechooser dir :: <java.io.File> mode select-dir filterlist 
                           #!optional 
                           default-name
                           default-extension)
  
  (display "show jfilechooser ")(display dir)(newline)
  
  (let* ((fchooser :: <javax.swing.JFileChooser>
                   (if (is-null? dir)
                       (<javax.swing.JFileChooser>)
                       ;(<Bar> 'right)
                       (<javax.swing.JFileChooser> dir)
                       ;(custom-jfilechooser2 dir)
                       ))
         (local-frame (<javax.swing.JFrame>)))
    
    ;; this should be called before setFileSelectionMode
    ;; or it will not work
    (if (or (string? default-name)
            (java.lang.String? default-name))
        (invoke fchooser 'setSelectedFile (make-file default-name)))
    
    ; select directory?
    (if select-dir
        (begin
          (invoke fchooser 'setFileSelectionMode <javax.swing.JFileChooser>:DIRECTORIES_ONLY)
          ;; dont use file filter when selecting directory (show all files and folders)
          (invoke fchooser 'setAcceptAllFileFilterUsed #f)
          )
        )
        
    ; file filter
    (if (not (null? filterlist))
        (invoke fchooser 'setFileFilter
                ; anonymous class to filter file list
                (object (<javax.swing.filechooser.FileFilter>)
                  ((accept the-file :: <java.io.File>) :: boolean
                   (or 
                    ; if its a directory, let it through
                    (file-directory? the-file)
                    ; otherwise check the extension
                    (let ((result #f)
                          (name (get-file-name the-file)))
                      ; check whether the file ends with any of the permitted file extensions
                      (map (lambda (this-entry)
                             (set! result (or result (string-ends-with? name this-entry))))
                           filterlist)
                      result)))
                      ((get-description) :: <java.lang.String>
                       (let ((the-string ""))
                         (map (lambda (this-entry)
                                (display this-entry) (newline)
                                (set! the-string (string-append the-string
                                                                " "
                                                                "*"
                                                                this-entry)))
                              filterlist)
                         (invoke the-string 'to-string)))
                  )))
    
    (display "entering show dialog")(newline)
    ; show the dialog
    (let ((returnVal (invoke fchooser mode local-frame)))
      (display "here returnVal ")(display returnVal)(newline)
      (if (= returnVal <javax.swing.JFileChooser>:APPROVE_OPTION)
          (begin
            (display "clicked on ok in file chooser ");
            ; select directory?
            ;; setting this disallows the default-name to be set to the textfield
            ;; so we set it after clicking on ok
            (if select-dir
                (invoke fchooser 'setFileSelectionMode <javax.swing.JFileChooser>:DIRECTORIES_ONLY))
            
            (define tmp-file (invoke fchooser 'getSelectedFile))
            (define dir-str (invoke tmp-file 'get-parent))
            (define file-name (invoke tmp-file 'get-name))
            
            ;; duplicate code in show-file-dialog
            ;; if filename does not contain . (ie no extension given) append with extension
            (if default-extension
                (set! file-name (filename-extension-check file-name default-extension)))
            
            (make-file (string-append dir-str (get-file-separator) file-name))
            )
          #f))))

; helper function to get file separator
(define (get-file-separator)
  (get-system-property "file.separator"))

; helper function to get user director
(define (get-user-directory)
  (get-system-property "user.dir"))

(define (get-file-data file :: <java.io.File>)
  (let* ((br (<java.io.BufferedReader> (<java.io.InputStreamReader>
                                        (<java.io.FileInputStream> file))))
         (datalines (lines-in-file file br "")))
    (invoke br 'close)
    datalines))

(define (lines-in-file file :: <java.io.File>
                       buf :: <java.io.BufferedReader>
                       str)
  (if (invoke buf 'ready)
      (let ((line (invoke buf 'readLine)))
        (set! line (string-append line "\n"))
        (set! str (string-append line (lines-in-file file buf str)))))
  str)

;get all files having a particular name in a directory and its subdirectories
;@args: filename, directory
(define (get-file-from-directory name :: <java.lang.String>
                                 dir :: <java.io.File>)
  (let*(( results (list))
        (child-list (get-directory-listing dir)))
    (map (lambda (this-child)
           (if (invoke (as <java.io.File> this-child) 'isFile)
               (if (invoke (invoke (as <java.io.File> this-child) 'getName) 'equals name)
                   (set! results (append results (list this-child))))
               (set! results (append results (get-file-from-directory
                                              name this-child)))))
         child-list)
    results))

; get a list of files from a directory, returns a list
(define (get-directory-listing dir :: <java.io.File>)
  (if (and (file-exists? dir)
           (file-directory? dir))
      (let ((children (invoke dir 'listFiles)))
        (if (not (is-null? children))
            (let ((lsize (invoke-static <java.lang.reflect.Array>
                                        'getLength children)))
              (array-to-list children lsize))
            '()))
      '()))

; recursively deletes directory contents
(define (recursively-delete-directory dir :: <java.io.File>)
  (if (and (file-exists? dir)
           (file-directory? dir))
      (let ((dir-list (get-directory-listing dir)))
        ; delete all the files in the directory first
        (map (lambda (this-file)
               (if (file-exists? this-file)
                   (if (file-directory? this-file)
                       ; if its a directory, recursively delete
                       (recursively-delete-directory this-file)
                       ; otherwise, just delete the file
                       (try-catch
                           (begin
                             (format #t "deleting file: ~a~%~!" this-file)
                             (delete-file this-file))
                         (ex <java.lang.Throwable>
                             (begin
                               (display (*:toString ex))(newline)
                               ;(*:printStackTrace ex)
                               ))))))
             dir-list)
        ; then delete the directory itself (now that its empty)
        (try-catch
            (begin
              (format #t "deleting directory: ~a~%~!" dir)
              (delete-file dir))
          (ex <java.lang.Throwable>
              (begin
                (display (*:toString ex))(newline)
                ;(*:printStackTrace ex)
                ))))))

; recursively copies directory contents
(define (recursively-copy-directory source-dir :: <java.io.File>
                                    dest-dir :: <java.io.File>)
  (format #t "copying directory: ~a to ~a~%~!" source-dir dest-dir)
  (if (and (file-exists? source-dir)
           (file-directory? source-dir)
           (not (equal? source-dir dest-dir)))
      (let ((dir-list (get-directory-listing source-dir))
            (dest-dir-path (path-file dest-dir))
            (file-separator (get-file-separator)))
        (format #t "inside~%~!")

        ; first copy the directory itself
        (try-catch
            (begin
              (format #t "first create dest directory: ~a~%~!" dest-dir)
              (make-directory dest-dir))
          (ex <java.lang.Throwable>
              (begin
                (display (*:toString ex))(newline)
                (*:printStackTrace ex)
                )))

        ; then copy all the files in the directory
        (format #t "then copy the files in ~a~%~!" source-dir)
        (map (lambda (this-file)
               (if (file-exists? this-file)
                   (let ((dest-file (make-file
                                     (string-append dest-dir-path
                                                    file-separator
                                                    (path-last this-file)))))
                     (if (file-directory? this-file)
                         ; if its a directory, recursively copy
                         (recursively-copy-directory this-file dest-file)
                         ; otherwise, just copy the file
                         (try-catch
                             (begin
                               (format #t "copying file: ~a to ~a~%~!" this-file dest-file)
                               (copy-file-nio this-file dest-file))
                           (ex <java.lang.Throwable>
                               (begin
                                 (display (*:toString ex))(newline)
                                 (*:printStackTrace ex)
                                 )))))))
             dir-list))))

;get all files in a directory and its subdirectories
;@args: filename, directory
;;(define (get-files-in-directory dir :: <java.io.File>)
;;  (let*(( results (list))
;;        ( children (invoke dir 'listFiles)))
;;    (if (not (is-null? children))
;;        (let* ((lsize (invoke-static <java.lang.reflect.Array>
;;                                     'getLength children))
;;               (child-list (array-to-list children lsize)))

;;          (map (lambda (this-child)
;;                 (if (invoke (as <java.io.File> this-child) 'isFile)
;;                     (set! results (append results (list this-child)))
;;                     (set! results (append results (get-files-in-directory
;;                                                    name this-child)))))
;;               child-list)))
;;    results))

;get the name of the file
(define (get-file-name in-file :: <java.io.File>)
  (invoke in-file 'getName))

;get the absolute path of the file
(define (get-file-absolutepath in-file :: <java.io.File>)
  (invoke in-file 'getAbsolutePath))

; check if a file exists
(define (check-file-exists file :: <java.io.File>)
  (invoke file 'exists))

; check if string matches content of file
(define (check-file-content file :: <java.io.File> str)
  (let ((filedata (get-file-data file)))
    string=? filedata str))

; copy a file using NIO (should be able to handle large files)
; based on http://www.javalobby.org/java/forums/t17036.html
(define (copy-file-nio (from :: <java.io.File>) (to :: <java.io.File>)) :: <void>
  ; create dest file if it doesn't already exist
  (if (not (check-file-exists to))
      (create-new-file to))

  (let ((source :: <java.nio.channels.FileChannel> #!null)
        (dest :: <java.nio.channels.FileChannel> #!null))
    (try-finally
        (begin
          (set! source (invoke (<java.io.FileInputStream> from) 'getChannel))
          (set! dest (invoke (<java.io.FileOutputStream> to) 'getChannel))
          (invoke dest 'transferFrom source 0 (invoke source 'size)))
      (begin
        (if (not (is-null? source))
            (invoke source 'close))
        (if (not (is-null? dest))
            (invoke dest 'close))))))