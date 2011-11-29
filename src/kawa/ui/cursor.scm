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

(module-export check-cursor-type set-cursor-type set-custom-cursor!)

;;
;; cursor
;; 

; check cursor type:
; 'hand, 'default and 'custom for now
(define (check-cursor-type in-component :: <java.awt.Component> cursorType)
  (let* ((theCursor :: <java.awt.Cursor> (invoke in-component 'getCursor))
         (theCursorType (invoke theCursor 'getType)))
    (cond
     ((eq? cursorType 'hand)
      (eq? theCursorType <java.awt.Cursor>:HAND_CURSOR))
     ((eq? cursorType 'wait)
      (eq? theCursorType <java.awt.Cursor>:WAIT_CURSOR))
     ((eq? cursorType 'default)
      (eq? theCursorType <java.awt.Cursor>:DEFAULT_CURSOR))
     ((eq? cursorType 'custom)
      (eq? theCursorType <java.awt.Cursor>:CUSTOM_CURSOR))
     (else #f))))

; set cursor:
; 'hand and 'default for now
(define (set-cursor-type in-component :: <java.awt.Component> cursorType)
  (cond
   ((eq? cursorType 'wait)
    (invoke in-component 'setCursor
            (<java.awt.Cursor>:getPredefinedCursor <java.awt.Cursor>:WAIT_CURSOR)))
   ((eq? cursorType 'hand)
    (invoke in-component 'setCursor
            (<java.awt.Cursor>:getPredefinedCursor <java.awt.Cursor>:HAND_CURSOR)))
   ((eq? cursorType 'default)
    (invoke in-component 'setCursor
            (<java.awt.Cursor>:getPredefinedCursor <java.awt.Cursor>:DEFAULT_CURSOR)))
   (else #f)))

; create a custom cursor
(define (set-custom-cursor! in-component :: <java.awt.Component>
                            in-filename :: <java.lang.String>
                            in-label :: <java.lang.String>)
  (let* ((toolkit :: <java.awt.Toolkit> (<java.awt.Toolkit>:getDefaultToolkit))
         (image :: <java.awt.Image> (invoke toolkit 'getImage in-filename))
         (hotspot :: <java.awt.Point> (<java.awt.Point> 0 0))
         (cursor :: <java.awt.Cursor> (invoke toolkit 'createCustomCursor image hotspot in-label)))
    (invoke in-component 'setCursor cursor)))


               