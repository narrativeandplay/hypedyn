;; Part of the HypeDyn project - http://www.narrativeandplay.org/hypedyn
;; 
;; Copyright (C) 2008-2016
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

(module-export make-toolbar set-toolbar-floatable 
               set-toolbar-rollover add-toolbar-separator)

;;
;; toolbar
;; 

; make a toolbar
(define (make-toolbar title :: <String>)
  (<javax.swing.JToolBar> title))

; set toolbar floatable
(define (set-toolbar-floatable in-toolbar :: <javax.swing.JToolBar> in-flag)
  (invoke in-toolbar 'setFloatable in-flag))

; set toolbar rollover (whether buttons change state on rollover)
(define (set-toolbar-rollover in-toolbar :: <javax.swing.JToolBar> in-flag)
  (invoke in-toolbar 'setRollover in-flag))

; add a separator to toolbar
(define (add-toolbar-separator in-toolbar :: <javax.swing.JToolBar>)
  (invoke in-toolbar 'addSeparator))



