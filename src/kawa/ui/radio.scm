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

(module-export make-radio-button make-button-group
               add-to-button-group
               radio-button-selected?
               radio-button-set-selected)

;(define (make-radio . label-lst)
;  (define button-grp (<javax.swing.ButtonGroup>))
;  (map (lambda (str)
;         (if (string? str)
;             (begin
;               (display "its a string ")(display str)(newline)
;               (define r-button (<javax.swing.JRadioButton> str))
;               (invoke r-button 'set-action-command str)
;               (invoke button-grp 'add (r-button))
;               )
;             (begin
;               (display "not a string in make-radio")(newline)
;               ))
;         ) label-lst)
;  )

(define (make-radio-button str :: <string>) (<javax.swing.JRadioButton> str))
(define (make-button-group) (<javax.swing.ButtonGroup>))

(define (add-to-button-group button-grp :: <javax.swing.ButtonGroup>
                             . button-lst)
  (map (lambda (button)
         (if (javax.swing.AbstractButton? button)
             (invoke button-grp 'add button))
         ) button-lst))

(define (radio-button-selected? radio :: <javax.swing.JRadioButton>)
  (invoke radio 'is-selected))

(define (radio-button-set-selected (radio :: <javax.swing.JRadioButton>)
                                   (value :: <boolean>))
  (invoke radio 'set-selected value))