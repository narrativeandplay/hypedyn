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

(module-export make-label make-label-with-title bold-label)

;;
;; labels
;; 

; make a label
(define (make-label)
  (<javax.swing.JLabel>))

; make a label with title 
(define (make-label-with-title title :: <java.lang.String> )
  (<javax.swing.JLabel> title))
;;(define (make-label-with-title title :: <gnu.lists.FString>) ;:: <java.lang.String>
;;  (<javax.swing.JLabel> title))

;;JLabel has bold as default already (so only useful for unbolding it)
(define (bold-label in-label :: <javax.swing.JLabel>
                    #!optional bold? :: <boolean>)
  (if (not bold?) (set! bold? #t)) ;; if bold? not given then bold it
  (let* ((font (invoke in-label 'get-font))
         (font-style (invoke font 'get-style))
         (new-font (invoke font 'derive-font 
                           ((if bold? bitwise-and bitwise-xor) font-style <java.awt.Font>:BOLD))))
    (invoke in-label 'set-font new-font)))
