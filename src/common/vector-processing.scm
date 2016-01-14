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

;; temp common utils 
(module-export vector-map 
               vector->string
               vector-clone
               my-vector-selector)
(module-static 'init-run)

(define (vector-map map-lambda vect)
  (let ((vect-length (vector-length vect)))
    (do ((pos 0 (+ pos 1)))
        ((>= pos vect-length))
        (begin
          ;(display (vector-ref vect pos))(display " ")
          ;(display (invoke (vector-ref vect pos) 'get-class))(newline)
          (map-lambda (vector-ref vect pos))))))

;; should be rewritten to have better cloning method
(define (vector-clone vect)
  (let ((new-vect (list->vector (vector->list vect)))
        (count 0))
;    (vector-map (lambda (element)
;                  (vector-set! new-vect 0 element)
;                  (set! count (+ count 1)))
;                vect)
    new-vect
    ))

;; vect is a vector of string
(define (vector->string vect)
  (let ((string-to-return ""))
    
    ;; convert char element in the vect to string element
;    (define pos 0)
;    (my-vect-map 
;     (lambda (char)
;       (display "curr char ")(display char)(newline)
;       (vector-set! vect pos (char->string char))
;       (set! pos (+ pos 1)))
;     vect)
    
    ;; piece together all the char-string (string of one character)
    (vector-map 
     (lambda (char-str)
       (set! string-to-return (string-append string-to-return char-str)))
     vect)
    string-to-return))

;selector for vectors
; returns a new vector with elements of vect from 
; start(inclusive) to end(exclusive)
(define (my-vector-selector vect start end)
  (if (< start end)
      (let(( value (vector-ref vect start)))
        (vector-append (vector value) (my-vector-selector vect (+ start 1) end)))
      (if (= start end)
          (vector (vector-ref vect start))
          (vector))))