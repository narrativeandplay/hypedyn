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

(module-export make-slider get-slider-value set-slider-value set-slider-min set-slider-max)
 
;;
;; slider
;; 

; make a slider
;(define (make-slider)
;  (<javax.swing.JSlider>))

; makie a slider with min, max, init values
(define (make-slider min :: <int> max :: <int> init :: <int>)
  (<javax.swing.JSlider> min max init))

; get value from slider
(define (get-slider-value slider :: <javax.swing.JSlider>)
  (invoke slider 'getValue))

; set value for slider
(define (set-slider-value slider :: <javax.swing.JSlider>
                          val :: <int>)
  (invoke slider 'setValue val))

;; set minimum value of the slider
(define (set-slider-min slider :: <javax.swing.JSlider>
                        min :: <int>)
  (invoke slider 'set-minimum min))

;; set maximum value of the slider
(define (set-slider-max slider :: <javax.swing.JSlider>
                        max :: <int>)
  (invoke slider 'set-maximum max))


               