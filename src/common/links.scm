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

; code related to links used in both hypertextpane.scm and reader.scm

(begin
  (require "../kawa/ui/text.scm"))

; export
(module-export style-link style-disabled-link style-followed-link style-nolink)
(module-static 'init-run)

; text styles

; style for links
(define style-link (make-attribute-set))
(set-style-attributes style-link 'bold)
(set-style-attributes style-link 'underline)

; style for disabled links
(define style-disabled-link (make-attribute-set))
(set-style-attributes style-disabled-link 'bold)

; style delta for followed links
(define style-followed-link (make-attribute-set))
(set-style-attributes style-followed-link 'underline)

; style to reset text
(define style-nolink (make-attribute-set))