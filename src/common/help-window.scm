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

 
(require "../kawa/file.scm")
(require "fileio.scm") ; get-content-file

; export
(module-export show-help-window set-helpfile!)
  (module-static 'init-run)
  
; help file URI
(define help-file-URI "")

; show the help window
(define (show-help-window flag)
  (format #t "show-help-window: ~a ~%~!" help-file-URI)
  (invoke (java.awt.Desktop:getDesktop) 'browse help-file-URI))

; set the help file
(define (set-helpfile! in-help-filename in-mac-testing)
  (format #t "set-helpfile!: ~a ~%~!" in-help-filename)
  (let ((file (get-content-file in-help-filename in-mac-testing)))
    (if (check-file-exists file)
        (set! help-file-URI (get-file-URI file)))))
