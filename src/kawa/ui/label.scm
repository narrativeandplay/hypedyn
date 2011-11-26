;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2011 National University of Singapore and
;; Singapore-MIT GAMBIT Game Lab c/o Media Development Authority of Singapore
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

(module-export make-label make-label-with-title)

;;
;; labels
;; 

; make a label
(define (make-label)
  (<javax.swing.JLabel>))

; make a label with title 
(define (make-label-with-title title :: <java.lang.String>)
  (<javax.swing.JLabel> title))
