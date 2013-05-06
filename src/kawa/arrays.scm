;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2013
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

(module-export array-to-list array-to-list2 list-to-array)

;;
;; array utilities
;;

(define (array-to-list array :: <java.lang.Object> size)
  (if (> size 0)
      (let*(( newsize (- size 1))
            ( value (invoke-static <java.lang.reflect.Array> 'get array newsize))
            ( result (list value)))
        (append (array-to-list array newsize) (list value)))
      '())
  )

;; this is an improved version of array-to-list that does not 
;; require a size to be fed in before hand
(define (array-to-list2 array :: <java.lang.Object>)
  (define array-size (array-length array))
  (define (helper array index)
    (if (< index array-size)
        (let ((value (invoke-static <java.lang.reflect.Array> 'get array index)))
          (append (list value) (helper array (+ index 1))))
        '()
        ))
  (helper array 0))

;; Note: Throws: IllegalArgumentException - if the object argument is not an array
(define (array-length array :: <java.lang.Object>)
  (invoke-static <java.lang.reflect.Array> 'get-length array))

;; unused? - alex
;;(define (array-to-list2 array :: <java.lang.Object>)
;;  (array-to-list array array:length))

(define (list-to-array objlist)
  (let*(( list-length (length objlist))
        ( array (make <object[]> length: list-length))
        ( index (- list-length 1)))
    (map (lambda (o)
           (set! (array index) o)
           (set! index (- index 1)))
         objlist)
    array))
