;; Part of the HypeDyn project - http://www.narrativeandplay.org/hypedyn
;; 
;; Copyright (C) 2008-2016
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

; actually run the applet
; note: we need this additional file to run the applet since top-level code in modules 
; required in the actual applet class didn't seem to get executed...

(require "../common/evaluator.scm")
(require "../kawa/applet-support.scm")
(require "../kawa/strings.scm")
(require "htlanguage.scm")
(require "reader.scm")
(module-static 'init-run)
(module-export run-applet)

; run reader applet
(define (run-applet in-applet in-filename)
  ; create nodereader
  (create-nodereader in-applet #t #f #f)
  
  ; start adding language
  (add-language-start)

  ; add htlanguage to evaluator
  (add-htlanguage)

  ; finish adding language
  (add-language-end)
  
  ; loading from .jar file
;  (let* ((the-URL ((<geturl>):geturl))
;         (the-jar-path (path-parent the-URL)) ; path into the jar file
;         (the-file-path (path (string-append the-jar-path in-filename)))) ; the file, assuming its in the .jar
;    (doreaderopen-file the-file-path)) ; contains code to open the file using open-input-file
  
  ; loading from directory
  (let* ((the-docbase (get-document-base in-applet)) ; get the document base for applet
         (the-docbase-string (URL->string the-docbase)) ; convert from path to string
         (the-docbase-dir (substring the-docbase-string
                                     0 (- (string-length the-docbase-string) 
                                          (string-length (path-last the-docbase))))) ; hack to get directory
         (the-file-path (path (string-append the-docbase-dir in-filename)))) ; the file
    (format #t "path-last: ~a.~%~!" (path-last the-docbase))
    (doreaderopen-file the-file-path)))