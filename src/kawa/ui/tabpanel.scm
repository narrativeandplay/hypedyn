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

(module-export make-tabpanel make-tab-panel add-tabpanel-tab insert-tabpanel-tab remove-tabpanel-tab
               get-tabpanel-selection-index get-tabpanel-selected-component set-tabpanel-selected-index 
               get-tabpanel-label-at set-tabpanel-label-at)
 
;;
;; tabbed panel
;;

; why are there 2 of these? - alex

;make a tab panel
(define (make-tabpanel)
  (<javax.swing.JTabbedPane>))

;make tab panel
(define (make-tab-panel)
  (let* ((new-tab-panel (<javax.swing.JTabbedPane>))
         (new-select-model (<javax.swing.DefaultSingleSelectionModel>)))
    (invoke new-tab-panel 'setModel new-select-model)
    new-tab-panel))

;adding a new tab to tab-panel
(define (add-tabpanel-tab in-tabpanel :: <javax.swing.JTabbedPane>
                          tabname :: <java.lang.String>
                          tabcontent :: <javax.swing.JComponent>)
  (invoke in-tabpanel 'addTab tabname tabcontent))

; insert tab at a given index
(define (insert-tabpanel-tab in-tabpanel :: <javax.swing.JTabbedPane>
                             tabname :: <java.lang.String>
                             tabcontent :: <javax.swing.JComponent>
                             n :: <int>)
  (invoke in-tabpanel 'insertTab tabname #!null tabcontent tabname n))

;removing a tab from tab-panel
(define (remove-tabpanel-tab in-tabpanel :: <javax.swing.JTabbedPane> component :: <javax.swing.JComponent>)
  (invoke in-tabpanel 'remove component))

;get selection index from tab-panel
(define (get-tabpanel-selection-index tabpanel :: <javax.swing.JTabbedPane>)
  (invoke tabpanel 'getSelectedIndex))

;get selected component from tab-panel
(define (get-tabpanel-selected-component tabpanel :: <javax.swing.JTabbedPane>)
  (invoke tabpanel 'getSelectedComponent))

; set selected tab on tabpanel
(define (set-tabpanel-selected-index tabpanel :: <javax.swing.JTabbedPane>
                                     n :: <int>)
  (invoke tabpanel 'setSelectedIndex n))

; get the title of the tab
(define (get-tabpanel-label-at tabpanel :: <javax.swing.JTabbedPane>
                               index :: <int>)
  (invoke tabpanel 'getTitleAt index))

; set the title of the tab
(define (set-tabpanel-label-at tabpanel :: <javax.swing.JTabbedPane>
                               index :: <int>
                               tabname :: <java.lang.String>)
  (invoke tabpanel 'setTitleAt index tabname))


