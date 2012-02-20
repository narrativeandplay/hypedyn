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


;; where point and origin are cons-pair
(require "graphics.scm") ; for Cosine and Sine
(require "../kawa/strings.scm") ;string-contains
;(require 'srfi-13) ; string-contains

(module-export kawa-float->kawa-int
               rotate-clockwise
               ;;normalize-2d-vect
               vector2d-normalize
               scale-vector
               max-integer
               
               distance-2d distance vector-magnitude
               distance-less-than distance-more-than
               midpoint deg-to-rad rad-to-deg set-to-180 set-to-360 angular-dist angular-diff
               compute-angle my-slope truncate-decimal
               
               angle-greater-in-180? angle-lesser-in-180?
               basic-angle angle-in-which-quadrant
               angle-speed-to-velocity
               vector2d-diff vector2d-sum vector2d-to-from
               angle-to-direction
               direction-to-angle
               find-nearest-point-in-circle
               lerp inverse-lerp
               pi halfpi twopi three-quad-pi
               clamp-vector-magnitude
               dot-product
               
               my-more-anticlock
               my-more-clock
               weighted-sum
               angle-between-vector2d
               vector2d-average
               vector2d-right-angle-to
               concave-function
               convex-function
               
               angle-from-x-axis
               sigmoid-function convex-function concave-function linear-function
               )
;(module-static 'init-run)


; hack definition of PI
(define pi (atan 0.0 -1.0))
(define twopi (* pi 2))
(define halfpi (* pi 0.5))
(define three-quad-pi (* halfpi 3))

;; duplicated form ui-kawa
;(define (Cosine x) (cos (* pi (/ x 180))))
;(define (Sine x) (sin (* pi (/ x 180))))

;; angle in degree
;; note in the inverted y (java canvas y increase as we go down)
;; this does a rotate-clockwise
;; where point and origin are cons-pair
(define (rotate-clockwise 
         point origin angle
         )
  
  (if (and (pair? point)
           (pair? origin))
      (let ((x (car point))
            (y (cdr point))
            (x0 (car origin))
            (y0 (cdr origin))
            (rotated (cons 0 0)))
        (set-car! rotated (+ (* (- x x0) (Cosine angle))
                             (* (- y y0) (Sine angle))
                             x0))
        (set-cdr! rotated (+ (* (- y y0) (Cosine angle))
                             (* (- x x0) (Sine angle) -1)
                             y0))
        rotated)
      (begin
        (display "Warning: rotate-point invalid arguments.")(newline)
        (cons 0 0)
        )
      ))

;; 2d-vect-pair is a vector
(define (vector2d-normalize vect2d)
  (let* ((length (vector-magnitude vect2d))
         (x-comp (/ (vector-ref vect2d 0) length))
         (y-comp (/ (vector-ref vect2d 1) length))
         (to-return (vector x-comp y-comp)))
    to-return))

;; assuming we have a purely numerical vector
;; changes the vector too
(define (scale-vector vect amt)
  (do ((i 0 (+ i 1)))
       ((> i (- (vector-length vect) 1)) )
       (begin
         (vector-set! vect i (* amt (vector-ref vect i)))
         ))
  vect
  )

(define (kawa-float->kawa-int myfloat :: <float>)
  (invoke myfloat 'toExactInt <gnu.math.Numeric>:ROUND))
;;       
(define (max-integer) <java.lang.Integer>:MAX_VALUE)

;;;;;;;;;;;;;;;;;;;;;
;; MATH FUNCTIONS ;;;
;;;;;;;;;;;;;;;;;;;;;

;returns the euclidian distance between two 2d points
(define (distance-2d x1 y1 x2 y2)
  (sqrt (+ (expt (- x2 x1) 2.0) (expt (- y2 y1) 2.0))))

; returns the euclidian distance between two vectors
;;(define (distance pos1 pos2)
;;  (sqrt (+ (expt (- (vector-ref pos2 0) (vector-ref pos1 0)) 2.0) 
;;           (expt (- (vector-ref pos2 1) (vector-ref pos1 1)) 2.0))))

(define (vector-magnitude vect)
  (sqrt (+ (* (vector-ref vect 0) (vector-ref vect 0))
           (* (vector-ref vect 1) (vector-ref vect 1))))
  )
(define (vector-op operation vect vect2)
  (vector (operation (vector-ref vect 0) (vector-ref vect2 0))
          (operation (vector-ref vect 1) (vector-ref vect2 1)))
  )

(define (vector2d-diff vect vect2)
  (vector-op - vect vect2)
  )
(define vector2d-to-from vector2d-diff)

(define (vector2d-sum vect vect2)
   (vector-op + vect vect2)
  )

;; does vector of any length
;; 
(define (dot-product vect1 vect2)
  (define vl1 (vector-length vect1))
  (define vl2 (vector-length vect2))
  (if (not (equal? vl1 vl2))
      (begin
        (error "Warning dot product with vectors of different length")
        ))
  (define length-min (min vl1 vl2))
  (define length-max (max vl1 vl2))
  (define to-return 0)
  (do ((i 0 (+ i 1)))
      ((equal? i length-min))
      (begin
        (set! to-return
              (+ to-return
                      (* (vector-ref vect1 i) (vector-ref vect2 i))))
        
        ))
  to-return
  )

(define (midpoint pos1 pos2)
  (let (( pos1-x (vector-ref pos1 0))
        ( pos1-y (vector-ref pos1 1))
        ( pos2-x (vector-ref pos2 0))
        ( pos2-y (vector-ref pos2 1)))
    (vector (/ (+ pos2-x pos1-x) 2) (/ (+ pos2-y pos1-y) 2))))

(define (distance pos1 pos2)
  (let* (( pos1-x (vector-ref pos1 0))
         ( pos1-y (vector-ref pos1 1))
         ( pos2-x (vector-ref pos2 0))
         ( pos2-y (vector-ref pos2 1))
         ( x-diff (- pos2-x pos1-x))
         ( y-diff (- pos2-y pos1-y))
         )
    ;(display "distance num test ")(display (invoke pos1-x 'get-class))(newline)
    (sqrt (+ (* x-diff x-diff)
             (* y-diff y-diff)))
    ))

(define print-moderator 0) 
(define (distance-compare pos1 pos2 comparator threshold)
  (let* (( pos1-x (vector-ref pos1 0))
         ( pos1-y (vector-ref pos1 1))
         ( pos2-x (vector-ref pos2 0))
         ( pos2-y (vector-ref pos2 1))
         ( x-diff (- pos2-x pos1-x))
         ( y-diff (- pos2-y pos1-y))
         )
    (set! print-moderator (+ print-moderator 1))
;    (if (equal? (mod print-moderator 2) 0)
;        (begin
;          (display "distance compare ")(display comparator)(display " ")
;          (display (list (* x-diff x-diff) (* y-diff y-diff) (* threshold threshold)))(newline)
;          ))
;;    (if (> print-moderator 1000000)
;;        (set! print-moderator 0))
    (define to-return
      (comparator
       (+ (* x-diff x-diff) (* y-diff y-diff))
       (* threshold threshold)))
    ;(display "compare ")(display to-return)(newline)
    to-return
    ))

(define (distance-less-than pos1 pos2 threshold)
  (distance-compare pos1 pos2 < threshold)
  )

(define (distance-more-than pos1 pos2 threshold)
  (distance-compare pos1 pos2 > threshold)
  )

;degree to radians
(define (deg-to-rad deg)
  (/ (* deg 3.14159) 180))

;radians to degree
(define (rad-to-deg rad)
  (/ (* rad 180) 3.14159))

;; setting to range [-pi, pi] ( [-180, 180] in degrees )
(define (set-to-180 angle)
  (if (> angle pi)
      (- angle twopi)
      (if (<= angle (* pi -1))
          (+ angle twopi)
          (* angle 1))))

;; setting to range [0, 2*pi] ( [0, 360] in degrees) 
(define (set-to-360 angle)
  (if (>= angle twopi)
      (- angle twopi)
      (if (< angle 0)
          (+ angle twopi)
          (* angle 1))))

;takes angles as input
(define (angular-dist a b)
  (set! a (set-to-180 a))
  (set! b (set-to-180 b))
  (let (( diff (abs (- a b))))
    (if (> diff pi)
        (begin
          (set! a (set-to-360 a))
          (set! b (set-to-360 b))
          ))
    (abs (- a b))))

;; negative means angle a to b is reached faster going clockwise
 ;;positive means angle a to b is reached faster going anticlock
(define (angular-diff a b)
  (set! a (set-to-180 a))
  (set! b (set-to-180 b))
  (let (( diff (abs (- a b))))
    (if (> diff (deg-to-rad 180))
        (begin
          (set! a (set-to-360 a))
          (set! b (set-to-360 b))))
    (- a b)))

;takes vectors as input
(define (compute-angle to-pos from-pos)
  (set-to-180 (atan (- (vector-ref to-pos 1) (vector-ref from-pos 1))
                    (- (vector-ref to-pos 0) (vector-ref from-pos 0)))))

;; returns a normalized directional vector
(define (angle-to-direction angle)
  (let* ((which-quad (angle-in-which-quadrant angle))
         (base-angle (basic-angle angle))
         (to-return (vector (cos base-angle) (sin base-angle))))
    (cond ((equal? which-quad 1) () );(vector-set! to-return 1 (* -1 (vector-ref to-return 1))) )
          ((equal? which-quad 2) (vector-set! to-return 0 (* -1 (vector-ref to-return 0))))
          ((equal? which-quad 3) 
           (vector-set! to-return 0 (* -1 (vector-ref to-return 0)))
           (vector-set! to-return 1 (* -1 (vector-ref to-return 1))))
          ((equal? which-quad 4) (vector-set! to-return 1 (* -1 (vector-ref to-return 1))) ) ;; do nothing
          )
    ;; from the computation above, to-return is garanteed to be normalized
    to-return))

;; returns a normalized velocity
(define (angle-speed-to-velocity angle speed)
;;  (display "angle speed to vel test ")(newline)
;;  (display "angle ")(display (deg-to-rad angle))(newline)
;;  (display "speed ")(display speed)(newline)
  
;;  (display "angle to direction tset ")(display (angle-to-direction (deg-to-rad 45)))(newline)
;;  (display "angle here ")(display (rad-to-deg angle))(newline)
  (define to-return (angle-to-direction angle)) ;; angle zero defined to be facing north instead of east
;;  (display "direction ")(display to-return)(newline)
  (set! to-return (scale-vector to-return speed))
  to-return
  )

;; inverse of the above 2 (can take velocity or direction)
(define (direction-to-angle dir-vect)
  ;(atan (vector-ref dir-vect 0) (* -1 (vector-ref dir-vect 1)))
  (define to-return (atan (abs (vector-ref dir-vect 1)) (abs (vector-ref dir-vect 0))))
  (cond ((and (>= (vector-ref dir-vect 0) 0)
              (>= (vector-ref dir-vect 1) 0))
         () ;; first quad do nothing
         )
        ((and (>= (vector-ref dir-vect 0) 0)
              (< (vector-ref dir-vect 1) 0))
         ;; fourth quad
         (set! to-return (- twopi to-return))
         )
        ((and (< (vector-ref dir-vect 0) 0)
              (< (vector-ref dir-vect 1) 0))
         ;; third quad
         (set! to-return (+ pi to-return))
         )
        ((and (< (vector-ref dir-vect 0) 0)
              (>= (vector-ref dir-vect 1) 0))
         ;; second quad 
         (set! to-return (- pi to-return))
         )
        )
  to-return
  )

;; DUPLICATE FROM sheep-flocking.scm turn-towards-direction
;; negative angle means its anti clock
(define (angle-between-vector2d target-dir curr-dir)

  (set! target-dir (vector2d-normalize target-dir))
  (set! curr-dir (vector2d-normalize curr-dir))
  (define local-steer-angle 0)
  
  (if (or (equal? target-dir curr-dir)
          (equal? target-dir (scale-vector curr-dir -1)))
      (set! local-steer-angle pi)
      (begin
        (set! local-steer-angle (acos (dot-product target-dir curr-dir)))
        (define curr-facing (direction-to-angle curr-dir))
        (define target-facing (direction-to-angle target-dir))
        (if (my-more-anticlock target-facing curr-facing)
            (set! local-steer-angle (* local-steer-angle -1))
            )
        )
      )
  
  local-steer-angle
  )

;; limit the magnitude, if exceeded then set the magnitude as the limit
(define (clamp-vector-magnitude vect mag)
  (if (> (vector-magnitude vect) mag)
      (begin
        (set! vect (vector2d-normalize vect))
        (scale-vector vect mag)
        ))
  vect
  )

;pass n as a large value
(define (find-nearest-point-in-circle n x y new-x new-y dist-to-circle center radius)
  (let* (( temp-x (+ (vector-ref center 0) (* radius (cos (deg-to-rad (- 90 (* 6 n)))))))
         ( temp-y (+ (vector-ref center 1) (* radius (sin (deg-to-rad (- 90 (* 6 n)))))))
         ;(vector 0 0) represents pen
         ( temp-dist (distance (vector temp-x temp-y) (vector x y))))
    (if (< temp-dist dist-to-circle)
        (begin
          (set! new-x temp-x)
          (set! new-y temp-y)
          (set! dist-to-circle temp-dist)))
    (set! n (+ n 1))
    (if (< n 60)
        (find-nearest-point-in-circle n x y new-x new-y dist-to-circle center radius)
        (vector new-x new-y))))

;;TODO-- some bug
;returns #t if ang1 > ang2 and viceversa
;; top hemi is negative, btm hemi is positive, right is 0
(define (my-more-anticlock ang1 ang2)
  (set! ang1 (set-to-180 ang1))
  (set! ang2 (set-to-180 ang2))
  (cond ((or (and (< ang1 0)
                  (< ang2 0))
             (and (>= ang1 0)
                  (>= ang2 0)))
         (< ang1 ang2))
        ((and (< ang1 0)
              (>= ang2 0))
         (set! ang2 (+ ang2 (abs ang1)))
         (set! ang1 (+ ang1 (abs ang1))) ;; to 0
         
         (set! ang1 (set-to-180 ang1))
         (set! ang2 (set-to-180 ang2))
         ;; if the difference is less than 180 then 
         ;; ang1 is more anti
         ;; if more than diff the diff is 
         ;(if 
         ;    (< ang2 (deg-to-rad 180))
         ;    #t #f)
         (< ang1 ang2)
         )
        ((and (>= ang1 0)
              (< ang2 0))
         (set! ang1 (+ ang1 (abs ang2)))
         (set! ang2 (+ ang2 (abs ang2))) ;; to 0
         
         (set! ang1 (set-to-180 ang1))
         (set! ang2 (set-to-180 ang2))
;;         (if (< ang1 (* -1 (deg-to-rad 180)))
;;             #t #f)
         (< ang1 ang2)
         )
  ))

(define (my-more-clock ang1 ang2)
  (not (my-more-anticlock ang1 ang2))
  )

(define (angle-greater-in-180? ang1 ang2)
  (let (( diff (abs (- ang1 ang2))))
    (if (> diff pi)
        (begin
          (set! ang1 (set-to-360 ang1))
          (set! ang2 (- twopi ang2))))
    (>= ang1 ang2)))

;; take in ang in rad 
(define (angle-in-which-quadrant ang)
  ;; make sure ang is in the range [0, 2*pi]
  (set! ang (set-to-360 ang))
  (cond ((< ang (rad-to-deg 90))  1)
        ((< ang (rad-to-deg 180)) 2)
        ((< ang (rad-to-deg 270)) 3)
        ((< ang (rad-to-deg 360)) 4)))

(define (angle-to-from angle1 angle2)
  (- angle1 angle2)
  )

;; take in ang in rad
(define (basic-angle ang)
  (define which-quad (angle-in-which-quadrant ang))
  ;; make sure ang is in the range [0, 2*pi]
  (set! ang (set-to-360 ang))
  (cond ((equal? which-quad 1) ang)
        ((equal? which-quad 2) (- pi ang))
        ((equal? which-quad 3) (+ pi ang))
        ((equal? which-quad 4) (- twopi ang))))

;;TODO-- some bug
;returns #t if ang1 < ang2 and viceversa
(define (angle-lesser-in-180? ang1 ang2)
  (let (( diff (abs (- ang1 ang2))))
    (if (> diff (deg-to-rad 180))
        (begin
          (set! ang1 (set-to-360 ang1))
          (set! ang2 (- (deg-to-rad 360) ang2))))
    (<= ang1 ang2)))

;for undefined case, it just returns max value
(define (my-slope x1 y1 x2 y2)
  (if (= x2 x1)
      <java.lang.Integer>:MAX_VALUE
      (/ (- y2 y1) (- x2 x1))))

;; used to format double/float to remove very small decimals due to inaccuracy
;; to round to 2 decimal place just put 2 to the decimal-place argument
;; Current problem: not sure to use format string 0.## or 0.##E0 and when
;; case 0.##: process number without E correctly, once E appears the desired 2.23E3(2 decimal) is not correct
;; case 0.##E0: process number with E correctly, but number without E with decimal has decimals thrown away
;; solution is to check for E before choosing the correct format to use, this is done through converting 
;; the number to string first to have the E appear 
(define (truncate-decimal our-number decimal-place)
  (cond ((equal? decimal-place 0)
         (if (>= our-number 0)
             (ceiling our-number)
             (- (ceiling our-number) 1))
         )
        ((> decimal-place 0)
         (define format-string "0.#")
         ;; get the correct number of # after 0. indicated by the decimal-place to keep
         (do
          ((i 1 (+ i 1)))
          ((= i decimal-place))
          (set! format-string (string-append format-string "#"))
          )

         ;; check for E 
         (define Echeck-string (to-string our-number))
         ;(display "Echeckstring ")(display Echeck-string)(newline)
         ;(display "ecs class ")(display (invoke Echeck-string 'get-class))(newline)
         ;; add E to format-string if number contains E
         (if (string-contains Echeck-string "E")
             (set! format-string (string-append format-string "E0")))

         (define df (<java.text.DecimalFormat> format-string))
         (define number-string (invoke df 'format our-number))

         (define our-double (<java.lang.Double> number-string))
         (invoke our-double 'double-value)
         )
        (else (display "1st argument specifying decimal place isnt a proper positive value")
              (newline))))

;; linear interpolation
;; pos take value from 0 to 1
(define (lerp min-val max-val pos)
  (+ min-val 
     (* (- max-val min-val) 
        pos))
  )

;; return the position of current value on a linearly-spaced-valued line 
;; with min-val and max-val on the two far ends of the line
(define (inverse-lerp min-val max-val curr-val)
  (let ((ret-val (/ (- curr-val min-val)
                    (- max-val min-val))))
    ret-val
    )
  )

;; TODO: make weighted sum take in as many arguments as possible
(define (weighted-sum w1 v1 w2 v2 w3 v3 w4 v4 w5 v5 . args)
  (define total-sum
    (+ (* w1 v1)
       (* w2 v2)
       (* w3 v3)
       (* w4 v4) 
       (* w5 v5)
       ;(* w6 v6)
       ))
  (define total-weight
    (+ w1 w2 w3 w4 w5))
  
  ;; normalize if total-sum not zero, if zero just return total-sum
  (define normalized-sum total-sum)
  (if (not (equal? total-weight 0))
      (set! normalized-sum (/ total-sum total-weight)))
  normalized-sum
  )

;; TODO: need testing to replace the above
(define (weighted-sum2 . weight-value-pairs)
  (define total-sum 0)
  (define total-weight 0)
  (define (helper wv-lst)
    (if (> (length wv-lst) 2)
        (begin
          (set! total-sum (+ total-sum (* (car wv-lst) (cadr wv-lst))))
          (set! total-weight (+ total-weight (car wv-lst)))))
    (helper (cddr wv-lst)))
  (helper weight-value-pairs)
  
  ;; prevent division by 0
  (if (not (= total-weight 0))
      (/ total-sum total-weight)
      0) ;; all weight 0 just return 0
  )

;; vec-list is a list of 2d vector 
(define (vector2d-average vec-list)
  (define avg-vector (vector 0 0))
  (define vec-count (length vec-list))
  (map
   (lambda (curr-vec)
     (set! avg-vector (vector2d-sum avg-vector curr-vec))
     )
   vec-list
   )
  
  (if (> vec-count 1)
      (set! avg-vector (scale-vector avg-vector (/ 1 vec-count))))
  
  avg-vector
  )

;; applying rotational matrix 90 clockwise
(define (vector2d-right-angle-to vect)
  (define curr-x (vector-ref vect 0))
  (define curr-y (vector-ref vect 1))
  (vector curr-y (* -1 curr-x))
  )

(define (concave-function x concave-curviness)
  (let ((un-normalized-val (- (exp (<gnu.math.DFloNum> (as <double> (* x concave-curviness)) )) 1))
        (norm-factor (- (exp (<gnu.math.DFloNum> (as <double> (* 1 concave-curviness)))) 1)))
    (/ un-normalized-val norm-factor))
  )

;;(define (convex-function x convex-curviness)
;;  (/ (- 1 (exp (* -1 (* x convex-curviness))))))

(define (convex-function x convex-curviness)
  (/ (- 1 (exp (* -1 (* x convex-curviness))))
     (- 1 (exp (* -1 (* 1 convex-curviness))))))

;; these functions give a value from 0 to 1
;; the input is in the range of 1 to 30

;;(define (convex-function x convex-curviness)
;;  (/ (- 1 (exp (* -1 (* (/ x max-level) convex-curviness))))
;;     (- 1 (exp (* -1 (* 1 convex-curviness))))))

(define (linear-function x lin-gradient)
  (* x lin-gradient)
  )

;; x = 0 is a bad value
(define (sigmoid-function x sigmoid-sharpness)
  (let ((un-normalized-val
         (- (/ 1 (+ 1 (exp (* -1
                              (* sigmoid-sharpness (- x 0.5))
                              )))) 0.5))
        (norm-factor
         (- (/ 1 (+ 1 (exp (* -1
                              (* sigmoid-sharpness (- 1 0.5))
                              )))) 0.5))
        )
    (* (+ (/ un-normalized-val norm-factor) 1) 0.5)
    )
  )

;;(define (concave-function x concave-curviness)
;;  (let ((un-normalized-val (- (exp (* (/ x max-level) concave-curviness)) 1))
;;        (norm-factor (- (exp (* 1 concave-curviness)) 1)))
;;    (/ un-normalized-val norm-factor)
;;    ;un-normalized-val
;;    )
;;  )

; calculate angle of a line from x-axis (in degrees?)
(define (angle-from-x-axis sx sy tx ty)
  (let* ((rad-angle (if (= sx tx) 
                        (if (< sy ty) (/ pi 2.0) (/ (* 3 pi) 2))
                        (atan (/ (- ty sy) (- tx sx)))))
         (deg-angle (* rad-angle (/ 360.0 (* 2.0 pi)))))
    (if (< tx sx) (+ 180.0 deg-angle) deg-angle)))

