;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2014
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

(module-export make-popup
               show-popup hide-popup)

(define-private (get-popup-factory) (invoke-static <javax.swing.PopupFactory> 'get-shared-instance))
(define (make-popup parent :: <java.awt.Component>
                    contents :: <java.awt.Component>
                    x :: <int> 
                    y :: <int>)
  (invoke (get-popup-factory) 'get-popup parent contents x y))

(define (show-popup popup :: <javax.swing.Popup>)
  (invoke popup 'show))

(define (hide-popup popup :: <javax.swing.Popup>)
  (invoke popup 'hide))