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

(module-export get-rectangle-x get-rectangle-y get-rectangle-width get-rectangle-height
               make-dimension get-dimension-width get-dimension-height make-java-point
               get-point-x get-point-y)

;;
;; geometry
;; 

;getX of rectangle
(define (get-rectangle-x comp :: <java.awt.Rectangle>)
  (invoke comp 'getX))

;getY of rectangle
(define (get-rectangle-y comp :: <java.awt.Rectangle>)
  (invoke comp 'getY))

; get width of rectangle
(define (get-rectangle-width dim :: <java.awt.Rectangle>)
  (invoke dim 'getWidth))

; get height of rectangle
(define (get-rectangle-height dim :: <java.awt.Rectangle>)
  (invoke dim 'getHeight))

; make dimension
(define (make-dimension x y)
  (<java.awt.Dimension> x y))

; get width of dimension
(define (get-dimension-width dim :: <java.awt.Dimension>)
  (invoke dim 'getWidth))

; get height of dimension
(define (get-dimension-height dim :: <java.awt.Dimension>)
  (invoke dim 'getHeight))

(define (make-java-point x y)
  (<java.awt.Point> x y))

; get x component of point
(define (get-point-x in-point :: <java.awt.Point>) :: int
  (invoke in-point 'getX))

; get y component of point
(define (get-point-y in-point :: <java.awt.Point>) :: int
  (invoke in-point 'getY))



 