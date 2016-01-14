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

(module-export make-random generate-random-float generate-random-int
               get-next-double get-next-int)

;;
;; random numbers
;; 

(define (make-random)
  (<java.util.Random>))

;(as <java.lang.Integer>
;; teongleong: just added a floor so that it produces integer
;; |-----|
;; 0     1
;;    |------|
;;   0.5    1.5
;; we add 0.5 both the lower and higher range to compensate for 
;; the bias introduced by floor
;; randomed below 1 will floor to 0 randomed above 1 will random to 1 fairly
(define (generate-random-int lower higher) :: int

  (if (> lower higher)
      (let ((temp #f))
        (set! temp lower)
        (set! lower higher)
        (set! higher temp)))

  (floor
   (+ lower
      0.5
      (* (invoke-static <java.lang.Math> 'random)
         (- higher lower)))))

;  (display "higher ")(display higher)(newline)
;  (display "lower ")(display lower)(newline)
;  
;  (set! lower (+ (floor lower) 1))
;  (set! higher (floor higher))
;  (let ((rand-generator (make-random))
;        (diff (- higher lower)))
;    (display "diff ")(display diff)(newline)
;    (+ lower (invoke rand-generator 'next-int diff)))
;  )

;; if -1.1 and 0.1 

;; 

;;  (let* (( ran (invoke r 'nextInt))
;;         ( x (* (/ ran <java.lang.Integer>:MAX_VALUE) higher)))
;;    (format #t "ran: ~a~%~!" ran)
;;    (format #t "x: ~a~%~!" (/ ran <java.lang.Integer>:MAX_VALUE))
;;    (+ x lower)))

(define shared-rand-gen (make-random))

(define (generate-random-float lower higher)
  (let* ((random-generator shared-rand-gen)
         (random1 (invoke random-generator 'next-float))
         (interval (abs (- higher lower)))
         (return-val (* random1 interval)))
    ;; swap them around if it is wrong
    (if (> lower higher)
        (begin
          (let ((temp #f))
            (set! temp lower)
            (set! lower higher)
            (set! higher temp))))
    (+ return-val lower)
    )
  ;(* (invoke (make-random) 'next-float) (abs (- higher lower)))
  )

;Returns the next pseudorandom, uniformly distributed double value between 0.0 and 1.0 
;from this random number generator's sequence.
(define (get-next-double)
  (invoke shared-rand-gen 'nextDouble))

;Returns a pseudorandom, uniformly distributed int value between 0 (inclusive) 
;and the specified value (exclusive)
(define (get-next-int n)
  (invoke shared-rand-gen 'nextInt n))
