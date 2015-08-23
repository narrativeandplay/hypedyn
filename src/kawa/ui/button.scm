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

(module-export make-button set-button 
               set-button-label get-button-label 
               set-button-selected is-button-selected)

;;
;; button
;; 

;make a button
(define (make-button title :: <java.lang.String> 
                     #!optional action <javax.swing.AbstractButton>
                     )
  (define this-button (<javax.swing.JButton> title))
  (if action
      (invoke this-button 'set-action action))
  this-button
  )

;set button properties
;@param: button, enabled(boolean)
(define (set-button button :: <javax.swing.JButton>
                    enabled)
  (invoke (as <javax.swing.JButton> button) 'setEnabled enabled))

;set button's text label
; @param: button, name(java string)
(define (set-button-label button :: <javax.swing.JButton>
                          name :: <java.lang.String>)
  (invoke button 'set-text name))

;; return button's text
(define (get-button-label button :: <javax.swing.JButton>)
  (invoke button 'get-text))

(define (set-button-selected comp :: <javax.swing.JButton>
                             in-flag)
  (invoke comp 'setSelected in-flag))

;is a button selected
(define (is-button-selected btn :: <javax.swing.AbstractButton>)
  (invoke btn 'isSelected))
