;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2012
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

(module-export make-spinner set-spinner-number-model set-spinner-tip-text set-spinner-back-color set-spinner-fore-color
               set-spinner-value get-number-spinner-value)
               
;;
;; spinner
;;

;make a spinner
(define (make-spinner)
  (<javax.swing.JSpinner>))

;set a number model for the spinner
(define (set-spinner-number-model spinner :: <javax.swing.JSpinner>
                                  init :: <int> min :: <int> max :: <int> step :: <int>)
  (invoke spinner 'setModel (<javax.swing.SpinnerNumberModel> init min max step)))

;set tip text for spinner
(define (set-spinner-tip-text spinner :: <javax.swing.JSpinner> text)
  (invoke spinner 'setToolTipText text))

;set background color for spinner
(define (set-spinner-back-color spinner :: <javax.swing.JSpinner> color)
  (let* ((editor (invoke spinner 'get-editor))
         (textfield (invoke (as <javax.swing.JSpinner$DefaultEditor> editor) 'get-text-field)))
    (invoke textfield 'set-background color)))

;set foreground color for spinner
(define (set-spinner-fore-color spinner :: <javax.swing.JSpinner> color)
  (invoke (invoke (as <javax.swing.JSpinner$DefaultEditor> (invoke spinner 'getEditor)) 'getTextField) 'setForeground color))

;sets value in the spinner
(define (set-spinner-value spinner :: <javax.swing.JSpinner> value)
  (invoke spinner 'setValue value))

;returns the value in the number spinner
(define (get-number-spinner-value spinner :: <javax.swing.JSpinner>)
  (let* ((model (as <javax.swing.SpinnerNumberModel> (invoke spinner 'getModel)))
         (number (as <java.lang.Number> (invoke model 'getNumber)))
         (integer (invoke number 'intValue)))
    integer))

