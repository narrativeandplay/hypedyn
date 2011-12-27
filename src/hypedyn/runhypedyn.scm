;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2011
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

;; starting the main app with only one language
(begin
  (require "../common/main-ui.scm")
  (require "../common/preferences.scm")
  (require "../common/fileio.scm")
  (require "../common/vector-processing.scm")
  (require "../kawa/file.scm")
  (require "config-options.scm")
  (require "hypedyn.scm")
  (require "htfileio.scm"))

; command-line options
(format #t "args: ~a~%~!" command-line-arguments)
(if (> (vector-length command-line-arguments) 0)
    (vector-map
     (lambda (arg)
       (cond
        ((equal? "-basic" arg)
         (set-basic-version!))
        ((equal? "-normal" arg)
         (set-normal-version!))
        ((equal? "-sculptural" arg)
         (set-sculptural-version!))
        ((equal? "-undo" arg)
         (set-undo-enabled! #t))
        ((equal? "-full" arg)
         (set-full-version!))))
     command-line-arguments))

; create the main UI
(create-main-UI)

; load preferences, purely for the last-saved-dir
(getprefs)
(get-file-prefs)

; load only a specific language
(set-language! #f 
               "HypeDyn" ; name of language to appear in main window titlebar
               "hypedyn.scm" ; name of main file to run language
               "hypedyn.txt" ; name of help text file
               starthypedyn)

; remove language, control and examples menus
(remove-menu (get-language-menu))
(if (is-basic-mode?)
    (remove-menu (get-control-menu)))
(remove-menu (get-examples-menu))
;;(remove-menu (get-help-menu))

;; main tseting

; show the frame
(show-main-frame)

; open file from command line for windows
; (format #t "args len: ~a~%~!" (vector-length command-line-arguments))
(if (> (vector-length command-line-arguments) 0)
    ; grab the final argument
    (let* ((args-len (vector-length command-line-arguments))
           (arg (vector-ref command-line-arguments (- args-len 1)))
           (arg-len (string-length arg)))
      ; make sure it isn't a flag, and ends in .dyn
      (if (and
           (> arg-len 4) ; must have more than 4 characters long
           (not (equal? "-" (substring arg 0 1))) ; first character must not be -
           (equal? ".dyn" (substring arg (- arg-len 4) arg-len))) ; and must end in .dyn
          (open-file-by-name (make-file (path-file arg))))))

