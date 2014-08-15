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

(module-export make-splitpane set-splitpane-divider-location 
               set-splitpane-resize-weight set-splitpane-continuous-layout 
               set-splitpane-onetouchexpandable add-splitpane-component)
               
;;
;; splitpane
;; 

;make split pane
;true for horizontal and false for vertical split
(define (make-splitpane flag)
  (<javax.swing.JSplitPane> (cond ((eq? flag #t) <javax.swing.JSplitPane>:VERTICAL_SPLIT)
                                  (else <javax.swing.JSplitPane>:HORIZONTAL_SPLIT))))

; set splitpane divider location
(define (set-splitpane-divider-location in-pane :: <javax.swing.JSplitPane>
                                        in-pos :: <int>)
  (invoke in-pane 'setDividerLocation in-pos))

; set splitpane resize weights
(define (set-splitpane-resize-weight in-pane :: <javax.swing.JSplitPane>
                                     in-weight :: <double>)
  (invoke in-pane 'setResizeWeight in-weight))

; set splitpane to continuously update layout as divider is dragged (can impact performance)
(define (set-splitpane-continuous-layout in-pane :: <javax.swing.JSplitPane>
                                         in-flag :: <boolean>)
  (invoke in-pane 'setContinuousLayout in-flag))

; set splitpane to have "one-touch expandable" triangle for collapsing
(define (set-splitpane-onetouchexpandable in-pane :: <javax.swing.JSplitPane>
                                          in-flag :: <boolean>)
  (invoke in-pane 'setOneTouchExpandable in-flag))

; add component to split pane
; flag = true then add in top/left; else in right/bottom
(define (add-splitpane-component splitpane :: <javax.swing.JSplitPane>
                                 component :: <javax.swing.JComponent> flag)
  (cond ((eq? flag #f) (invoke splitpane 'setRightComponent component))
        (else (invoke splitpane 'setTopComponent component))))


