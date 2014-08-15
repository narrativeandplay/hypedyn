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

; contains procedure to start hypedyn
(begin
  (require "hteditor.scm")
  (require "htlanguage.scm")
  (require "config-options.scm")
  (require "../common/main-ui.scm") ;; get-main-ui-frame
  (require "../kawa/system.scm")
  (require "../kawa/file.scm")
  (require "../kawa/ui/dialog.scm") ; make-error-dialog
  )

; export
(module-export starthypedyn)

; start hypedyn
(define (starthypedyn)
  ; add htlanguage to evaluator
  (add-htlanguage)

  ; register the ui
  (use-hteditor-ui)
  
  ; start server
  (if (not (java-reader?))
      (try-catch
          (begin
            ; create a temporary directory for server
            (display "system-tmpdir ")(display (system-tmpdir))(newline)

            (set-temp-dir! (string-append (system-tmpdir) "/hypedyn" (number->string (get-current-time))))
            (make-directory (get-temp-dir))

            ; start the server - disabled as doesn't seem to work in app bundle with Java 7 on MacOS X (not needed yet anyway)
            (if (not (is-mac-os?))
                (begin
                    (gnu.kawa.servlet.KawaHttpHandler:addAutoHandler "/" (get-temp-dir))
                    (gnu.kawa.servlet.KawaHttpHandler:startServer (get-local-port))))
        )
        (ex <java.lang.Throwable>
            (begin
              (display (*:toString ex))(newline)
              ;(*:printStackTrace ex)
              (make-error-dialog (get-main-ui-frame)
                                 "Hypedyn Error"
                                 (string-append
                                  "Unable to start HypeDyn.\nQuit any other copies of HypeDyn\nthat may be running and try again."
                                  (*:toString ex)))
              (exit)
              )))
      )
  )
