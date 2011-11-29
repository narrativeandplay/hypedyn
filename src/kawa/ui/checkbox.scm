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

(module-export make-checkbox set-checkbox-selected 
               get-checkbox-value set-checkbox-value)
               
;;
;; check box
;; 

; make a check box
(define (make-checkbox in-title :: <String>)
  (<javax.swing.JCheckBox> (as <java.lang.String> in-title)))

; set selected a checkbox.
(define (set-checkbox-selected checkbox :: <javax.swing.JToggleButton> boolean :: <java.lang.Boolean>)
  (invoke checkbox 'setSelected boolean))

; get current value of checkbox
(define (get-checkbox-value in-checkbox :: <javax.swing.JCheckBox>)
  (not (equal? #!null (invoke in-checkbox 'getSelectedObjects))))

; set value of checkbox
(define (set-checkbox-value in-checkbox :: <javax.swing.JCheckBox> in-flag :: <boolean>)
  (invoke in-checkbox 'setSelected in-flag))


