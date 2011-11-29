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

(module-export make-panel clear-panel)

;;
;; panel
;; 

; make a panel
(define (make-panel)
  (<javax.swing.JPanel>))

;; remove all children component from a panel
(define (clear-panel container :: <java.awt.Container>)
  (invoke container 'remove-all))

(define (set-component-opaque jcomp :: <javax.swing.JComponent>
                              opaque? :: <boolean>)
  (invoke jcomp 'set-opaque opaque?))

