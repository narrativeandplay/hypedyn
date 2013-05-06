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

; help window

; this will contain a help document browser;
; content will be loaded dynamically based on
; the current language.

; language definition file should call
; (set-helpfile! filename)

 
(require "../kawa/ui/component.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/file.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/scrollpane.scm")

; export
(module-export create-help-window show-help-window close-help-window load-helpfile)
  (module-static 'init-run)
  
; help window frame
(define help-window-frame #f)

; help text
(define help-editor #f)

; parent
(define help-window-parent #f)

; create the help window
(define (create-help-window in-parent)
  ; remember parent
  (set! help-window-parent in-parent)
  
  ; top-level frame
  (set! help-window-frame (make-window "Help"))
  ; text editor
  (set! help-editor (make-textarea "" 20 25))
  (set-text-component help-editor #f #t)
  (let(( help-editor-scroll (make-scrollpane help-editor)))
    (add-component help-window-frame help-editor-scroll)
    ;(set-component-visible help-window-frame #t)
    (pack-frame help-window-frame)))
  
; show/hide the help window
(define (show-help-window flag)
  (if (and help-window-frame help-window-parent)
      (begin
        (center-frame-in-parent help-window-frame help-window-parent)
        (set-component-visible help-window-frame flag))))
  
; close the help window
(define (close-help-window)
  (if help-window-frame
      (set-component-visible help-window-frame #f)
      (dispose-frame help-window-frame)))
  
; load the help file
(define (load-helpfile help-filename)
  (let ((file (make-file help-filename)))
    (if (and help-editor (check-file-exists file))
        (let ((str (get-file-data file)))
          (set-text help-editor str)))))