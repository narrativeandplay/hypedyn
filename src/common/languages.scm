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

; support for multiple languages
(begin
  (require "../kawa/ui/menu.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/checkbox.scm")
  (require "preferences.scm")
  (require "main-ui.scm")
  (require "../kawa/file.scm")
  (require "../kawa/ui/events.scm")
  )
  
; export
(module-export set-language-menu!
               get-curr-langmenuitem set-curr-langmenuitem!
               get-curr-languagename set-curr-languagename!
               get-curr-languageinfo set-curr-languageinfo
               get-language-info
               get-languages add-languages
               load-preferred-language)
(module-static 'init-run)

; remember the languages menu
  
(define language-menu #f)

(define (set-language-menu! in-lang-menu)
  (set! language-menu in-lang-menu))

; remember currently checked language menu item and language name
(define curr-langmenuitem #f)
(define (get-curr-langmenuitem)
  curr-langmenuitem)
(define (set-curr-langmenuitem! in-item)
  (set! curr-langmenuitem in-item))

(define curr-languagename "<no language chosen>")
(define (get-curr-languagename)
  curr-languagename)
(define (set-curr-languagename! in-name)
  (set! curr-languagename in-name))

(define curr-languageinfo #f)
(define (set-curr-languageinfo in-info)
  (set! curr-languageinfo in-info))
(define (get-curr-languageinfo)
  curr-languageinfo)

(define language-info (list))
(define (get-language-info)
  language-info)

; figure out which languages are installed: returns a list of lists in the
; format (list name path-to-langfile path-to-helpfile)
;"../../../langs"
(define (get-languages)
  (let*(( directory (make-file "langs"))
        ( file-list (get-file-from-directory "info.scm" directory)))
    (map (lambda (this-file)
           (let*(( explist (open-input-file this-file))
                 ( langlist (read explist)))
             (set! language-info (append language-info (list langlist)))))
         file-list))
  language-info)
 
; add all the installed languages to the languages menu: takes
; a list of languages, plus the menu to add the languages to, and
; a callback to be called when the menu item is selected.
; note that if an info file is empty, there will be an "eof" in the
; lang-info list, so need to check for this
(define (add-languages lang-info set-language)
  ;(format #t "add-languages: ~a, ~a~%~!" lang-info set-language)
  (map (lambda (this-language)
         (if (not (eq? this-language #!eof))
             (let* (( langname (list-ref this-language 0))
                    ( langfilename (list-ref this-language 1))
                    ( helpfilename (list-ref this-language 2))
                    ( initproc (list-ref this-language 3))
                    ( menuitem (make-checkbox-menu-item langname)))
               (add-component language-menu menuitem)
               (add-actionlistener menuitem (make-actionlistener
                                             (lambda (source)
                                               (begin
                                                 (set-language! menuitem langname
                                                                langfilename helpfilename initproc))))))))
       lang-info))

; load the language specified in the preferences file, if any
(define (load-preferred-language)
  (display "load pref lang that call set-language!")(newline)
  ; load preferred language, if any - need to catch this?
  (let ((pref-langinfo (get-pref 'lang)))
    (if pref-langinfo
        (let*(( langname (list-ref pref-langinfo 0))
              ( langfilename (list-ref pref-langinfo 1))
              ( helpfilename (list-ref pref-langinfo 2))
              ( initproc (list-ref pref-langinfo 3)))
          (set-language! #f langname langfilename helpfilename initproc))
        (begin
          (display "no preferred language found.\n")
          (no-language-set)))))


; figure out which examples are available for a language 
;;(define (get-examples lang-name)
;;  (let*(( directory (make-file lang-name))
;;        ( file-list (get-files-in-directory directory))
;;        ( sub-directory (get-directory-from-directory)))
;;    (if (is-not-null? subdirectory)
;;        (begin
;;          (newline)
;;          (newline)
;;          (
;;           ;(format #t "filelist: ~a~%~!" file-list)
;;           (map (lambda (this-file)
;;                  ;(format #t "this-file: ~a~%~!" this-file)
;;                  (let*(( explist (open-input-file this-file))
;;                        ( langlist (read explist)))
;;                    ;(format #t "explist: ~a~%~!" explist)  
;;                    ;(format #t "langlist: ~a~%~!" langlist)       
;;                    (set! language-info (append language-info (list langlist)))))
;;                file-list))
;;          ;(format #t "lang-nfo: ~a~%~!" language-info)
;;          language-info)