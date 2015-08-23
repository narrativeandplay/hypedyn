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

;; applet version of standalone reader
(require "htappletwrapper.scm")
(require "../kawa/miscutils.scm")

;
; applet code
;

; note: I've left java-specific code here as an applet is java-specific anyway... :)

; init the applet
(define (init) :: void
    (format #t "called init.~%~!"))

(define (start) <void>
  (let ((applet :: <java.applet.Applet> (this)))
    (format #t "called start.~%~!")
    ; required! as applets cannot compile to java
    (set! gnu.expr.ModuleExp:alwaysCompile #f)
    (invoke applet 'setLayout (<java.awt.BorderLayout>))
    (let ((the-filename (invoke applet 'getParameter "STORYFILE")))
      (if (not (is-null? the-filename))
          (run-applet applet the-filename)
          (run-applet applet "story.dyn")))))

(define (stop) :: void (format #t "called stop.~%~!"))
(define (destroy) :: void (format #t "called destroy.~%~!"))