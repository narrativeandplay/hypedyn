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

(module-export get-color-red get-color-green get-color-blue
               get-colorjava-red get-colorjava-green get-colorjava-blue
               colorjava->colorlist
               get-red-javacolor get-green-javacolor get-blue-javacolor get-gray-javacolor
               get-darkgray-javacolor get-lightgray-javacolor
               make-colour-from-list ;make-colour-from-list4 
               make-colour-rgb
               white-color black-color grey-color light-grey-color dark-grey-color bg-color 
               white-smoke-color floral-white-color
               red-color yellow-color green-color blue-color light-blue-color tan-color dark-green-color
               make-greyscale-colour)

;;
;; colour - should clean up the redundant versions of colour representation
;; 

;get red from the colour made from the (colour r g b) function
; the colour resulting from the function is just a list of 3 number betw 0 to 255
(define (get-color-red color)
  (car color))

;get green
(define (get-color-green color)
  (cadr color))

;get blue 
(define (get-color-blue color)
  (caddr color))


;; this is totally different from the above
;; here we are retrieving components from actual java color
(define (get-colorjava-red color :: <java.awt.Color>)
  (invoke color 'get-red))

(define (get-colorjava-green color :: <java.awt.Color>)
  (invoke color 'get-green))

(define (get-colorjava-blue color :: <java.awt.Color>)
  (invoke color 'get-blue))

;; convert java color to a color component list
(define (colorjava->colorlist color :: <java.awt.Color>)
  (list (get-colorjava-red color)
        (get-colorjava-green color)
        (get-colorjava-blue color)))

;; yet another totally different but looks the same set of functions
;; here we are getting RED, GREEN or BLUE java colours
(define (get-red-javacolor) :: <java.awt.Color>
  (<java.awt.Color>:.red))

(define (get-green-javacolor) :: <java.awt.Color>
  (<java.awt.Color>:.green))

(define (get-blue-javacolor) :: <java.awt.Color>
  (<java.awt.Color>:.blue))

(define (get-gray-javacolor) :: <java.awt.Color>
  ;(<java.awt.Color>:gray)
  <java.awt.Color>:gray
  )
(define (get-darkgray-javacolor) :: <java.awt.Color>
  <java.awt.Color>:darkGray)
(define (get-lightgray-javacolor) :: <java.awt.Color>
  <java.awt.Color>:lightGray)

;; note all these colours are just a list of 3 numbers
;; you need to call (make-colour-from-list color) to make the actual java color
;; should move to a separate file
(define white-color (list 255 255 255 255))
(define black-color (list 0 0 0 255))
(define grey-color (list 180 180 180 255))
(define light-grey-color (list 220 220 220 255))
(define white-smoke-color (list 245 245 245))
(define floral-white-color (list 255 250 240))
(define dark-grey-color (list 100 100 100 255))
(define bg-color (list 230 230 230 255))
(define red-color (list 255 0 0 255))
(define yellow-color (list 255 255 51 255))
(define green-color (list 0 255 0 255))
(define dark-green-color (list 0 120 0 255))
(define blue-color (list 51 51 255 255))
(define light-blue-color (list 120 254 254 255))
(define tan-color (list 210 180 140 255))

; make a java colour object: takes a list of rgb values
(define (make-colour-from-list col)
  ;(display "make color from list check ")(display col)(newline)
  (let* ((red :: <int> (car col))
         (green :: <int> (cadr col))
         (blue :: <int> (caddr col))

         ;; if alpha specified use it, else use 255
         (alpha :: <int> (if (> (length col) 3) (cadddr col) 255))
         (the-colour :: <java.awt.Color>
                     (<java.awt.Color> red green blue alpha))
         )

    the-colour))

;; with tranlucency alpha (make-color-from-list can take alpha values too now)
;(define (make-colour-from-list4 col)
;  (let* ((red :: <int> (car col))
;         (green :: <int> (cadr col))
;         (blue :: <int> (caddr col))
;         (alpha :: <int> (cadddr col))
;         (the-colour :: <java.awt.Color> (<java.awt.Color> red green blue alpha)))
;    the-colour))

; make colour by rgb
(define (make-colour-rgb red :: <int>  green :: <int> blue :: <int>)
  (begin
    (if (< red 0) (display "red invalid number : negative"))
    (if (> red 255) (display "red invalid number : more than 255"))
    (if (< blue 0) (display "blue invalid number : negative"))
    (if (> blue 255) (display "blue invalid number : more than 255"))
    (if (< green 0) (display "green invalid number : negative"))
    (if (> green 255) (display "green invalid number : more than 255"))
    (<java.awt.Color> red green blue)))

; make a java colour object: takes a single value for grey
(define (make-greyscale-colour col :: <int> . alpha )
  ;; if alpha specified use it, else use 255
  (let* ((alpha :: <int> (if (pair? alpha) (car alpha) 255))
         (the-colour :: <java.awt.Color>
                     (<java.awt.Color> col col col alpha))
         )

    the-colour))

 