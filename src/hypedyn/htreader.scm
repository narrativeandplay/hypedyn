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

;; standalone reader
(begin
  (require "../common/evaluator.scm")
  (require "../kawa/ui/menu.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/frame.scm")
  (require "../kawa/file.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/dialog.scm")
  (require "../kawa/system.scm")
  (require "../common/fileio.scm")
  (require "../common/inspector.scm")
  (require "../common/macos.scm")
  (require "../common/datatable.scm")
  (require "htlanguage.scm")
  (require "reader.scm")
  (require "datastructure.scm"))

(define use-fullscreen #f)
(define demo-mode #f)
(define demo-filename "main.dyn")

; shutdown procedure
(define (close-htreader-subwindows)
  (exit 1))

; test if can close main frame or not
(define (mycanclose?)
  ; check if data has changed
  (if (dirty?)
      ; yes, so ask first
      ; (later save reading position)
      (let ((reply (make-confirm-dialogbox
                    f 2 "Do you really want to stop reading?")))
        (eq? reply 1))

      ; otherwise go ahead
      #t))

; are we fully initialized?
(define initialized #f)
(define (initialized?)
  initialized)
(define (set-initialized!)
  (set! initialized #t))

; UI components
(define f #f)
(define mb #f)
(define m-file #f)
(define m-file-open #f)
(define m-file-exit #f)

; macos-specific application events

; about event
(define (macos-application-about e)
  (display "about")(newline))

; preferences event
(define (macos-application-preferences e)
  (display "preferences")(newline))

; open application event
(define (macos-application-open-application e)
  (display "open application")(newline))

(define double-clicked-filename #f)
(define (get-double-clicked-filename)
  double-clicked-filename)

; open file event, happens on double-click
(define (macos-application-open-file the-filename)
  (if (initialized?)
      ; already initialized, so go ahead and load the file
      (doreaderopen-file (make-file (path-file the-filename)))
      ; otherwise save the name and load when initialization is finished
      (set! double-clicked-filename the-filename)))

; print file event
(define (macos-application-print-file e)
  (display "print file")(newline))

; re-open application event
(define (macos-application-re-open-application e)
  (display "re-open-application")(newline))

; create the standalone reader UI
(define (create-standalone-reader-UI)
  ; mac-os specific stuff here
  (if (is-mac-os?)
      (begin
        ; handle application events
        (add-application-listener window-closing
                                  macos-application-about
                                  macos-application-preferences
                                  macos-application-open-application
                                  macos-application-open-file
                                  macos-application-print-file
                                  macos-application-re-open-application)
        
        ; enable mac menus
        (enable-mac-menus)))
                                
  ; create frame
  (set! f (make-window "HypeDyn Reader"))

  ; if using fullscreen mode, set undecorated flag
  (if use-fullscreen
      (set-window-undecorated f #t))

  ; add menu bar
  (set! mb (make-menu-bar))
  (if (not use-fullscreen)
      (add-menu-bar f mb))

  ; menus
  (set! m-file (make-menu "File"))
  (add-component mb m-file)

  ;
  ; menu items
  ;

  ; file

  ; open
  (set! m-file-open (make-menu-item "Open..."))
  (add-component m-file m-file-open)
  (add-actionlistener m-file-open (make-actionlistener
                                   (lambda (source)
                                     (doreaderopen))))

  ; exit
  (if (not (is-mac-os?))
      (begin
        (set! m-file-exit (make-menu-item "Exit"))
        (add-component m-file m-file-exit)
        (add-actionlistener m-file-exit (make-actionlistener
                                         (lambda (source)
                                           (doexit))))))

  ; link in nodereader-editor
  (create-nodereader f #f demo-mode demo-filename)

  ; handle window closing
  (add-windowlistener f (make-windowlistener window-opened
                                             window-closing
                                             window-closed
                                             window-iconified
                                             window-deiconified
                                             window-activated
                                             window-deactivated))

; pack
  (pack-frame f))

;;
;; callbacks
;;

; open
(define (doreaderopen)
  ; if using fullscreen mode, need to disable to allow file dialog to show
  (if use-fullscreen
      (set-window-fullscreen #!null))
  ; first check if we should save
  (if (mycanclose?)
      ; safe, so proceed
      (let ((newfile (get-file-to-open (get-last-saved-dir) #f (list ".dyn"))))
        (if (not (doreaderopen-file newfile))
            (make-confirm-dialogbox #!null 1 "Sorry, no start node defined."))))
  ; if using fullscreen, reset after finished with file dialog
  (if use-fullscreen
    (set-window-fullscreen f)))

; exit
(define (doexit)
  ; first check if we should save
  (if (mycanclose?)
      ; safe, so proceed  
      (close-htreader-subwindows)))

;window callbacks ; future work
(define (window-closing exititem)
  (doexit))
(define (window-opened o)
  ())
(define (window-closed o)
  ())
(define (window-iconified o)
  ())
(define (window-deiconified o)
  ())
(define (window-activated o)
  ())
(define (window-deactivated o)
  ())

;;
;; start
;;

(define (run)
  ; create the UI
  (create-standalone-reader-UI)

  ; start adding language
  (add-language-start)

  ; add htlanguage to evaluator
  (add-htlanguage)

  ; finish adding language
  (add-language-end)

  ; if using fullscreen, set window size to screen size
  (if use-fullscreen
      (set-bounds f (get-screen-size)))

  ; show the frame
  (set-component-visible f #t)
  
  ; remember that we're fully initialized
  (set-initialized!)

  ; if using fullscreen, switch to fullscreen now
  (if use-fullscreen
      (set-window-fullscreen f))

  ; handle command-line arguments, if any
  (format #t "args: ~a~%~!" command-line-arguments)
  (if (> (vector-length command-line-arguments) 0)
      (let ((arg (vector-ref command-line-arguments 0)))
        (remove-component m-file m-file-open)
        (doreaderopen-filename arg)))
      
  ; make sure frame has focus
  ;(send f focus)
  
  (if demo-mode 
      (doreaderopen-filename demo-filename))

  
  ; and if there was a double-clicked file, load it now
  (if double-clicked-filename
      (begin
        (doreaderopen-file (make-file (path-file double-clicked-filename)))
        (set! double-clicked-filename #f))))

(run)
