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

; this module implements a simple graphics API, which could be supported by any
; underlying graphics toolkit
; require the relevant graphics-x.scm file depending on implementation used

(begin
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/frame.scm")
  (require "../kawa/color.scm")
  (require "../kawa/graphics-kawa.scm")
  (require "math.scm") ;; for pi
  )

; export
(module-export Cosine Sine colour round* number-clamp
               get-screen-width get-screen-height get-colourdepth
               make-point createseg createstamp 
               z-coord orientation
               segment-intersection? point-in-triangle? triangle-intersection?
               set-bgcolour! clearlines clearstamps
               set-graphics-size!
               set-mydraw-callback! refresh set-usegraphics! usegraphics?
               set-init-callback! set-reset-callback! set-exit-callback! set-mouse-event-callback!
               reset-graphics init-graphics exit-graphics )
(module-static 'init-run)
               
(define font-scale   0.09)
; wrappers for some gl functions

; angle in radian to degree
;(define (rad-to-deg x-in-rad) 
;  (* pi (/ x-in-rad 180)))

;angle in degree to radian
;(define (deg-to-rad x-in-deg)
;  (* (/ x-in-deg pi) 180))

(define (Cosine x) (cos (* pi (/ x 180))))
(define (Sine x) (sin (* pi (/ x 180))))
(define round* (lambda (x) (round x)))

;; keep the number value within the specified min and max
(define (number-clamp value min max)
  ;; make sure the min is smaller than max
  (cond ((> min max)
         (let ((temp max))
           (set! max min)
           (set! min temp)))
        ((< value min)
         (set! value min))
        ((> value max)
         (set! value max)))
  value)

(define (colour r g b) (list r g b))

; convert between allegro and scheme representation of angles
(define (allegroToSchemeAngle angle)
  (* angle (/ 360 256))
  )
(define (schemeToAllegroAngle angle)
  (* angle (/ 256 360))
  )

; screen size and colour depth

; tengfei made some changes to the default value of screen-width and screen-height to achieve a better performance
; for ioScheme. The default values are 400 and 800 for width and height respectively.
(define screen-width 200)
;(provide get-screen-width)
(define (get-screen-width)
  screen-width)
(define screen-height 200)
;(provide get-screen-height)
(define (get-screen-height)
  screen-height)
(define colourdepth 16)
;(provide get-colourdepth)
(define (get-colourdepth)
  colourdepth)

;; line segments and points, borrowed from SICP;
;; these functions are used to provide a persistent object
;; model for the lines drawn on the screen.

; list of line segments
(define line-segments '())
(define stamps '())

(define (make-segment point1 point2 col wid)
  (list point1 point2 col wid))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cadr seg))
(define (colour-segment seg)
  (caddr seg))
(define (width-segment seg)
  (cadddr seg))
;(provide make-point)
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (print-point p)
  (newline)
  (display "print point ")(newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (average a b)
  (/ (+ b a) 2.0))
(define (mid-point seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg)))
    (make-point (average (x-point p2) (x-point p1))
                (average (y-point p2) (y-point p1)))))

; create line segment - take out use of append?
; use this to draw a line; note that line width is
; stored but is currently not implemented in the drawing
; functions below.
;(provide createseg)
(define (createseg x1 y1 x2 y2 color width)
  (set! line-segments (append line-segments
                              (list (make-segment
                                     (make-point x1 y1)
                                     (make-point x2 y2)
                                     color
                                     width)))))

;stamps is different from line segment
;two types
;-normal turtle shap
;-bitmap
;(provide createstamp)
(define (createstamp stamp)
  (set! stamps (append stamps (list stamp)))
  ;(display "stamps created: ") (display stamps) (newline))
  )

; 2D geometry stuff, ref:
; http://www.mochima.com/articles/cuj_geometry_article/cuj_geometry_article.html

; given 3 points p1, p2 and p3, calculate the z-coordiante of
; the vector product of p2-p1 and p3-p2
;(provide z-coord)
(define (z-coord p1 p2 p3)
  (let ((x1 (x-point p1))
        (y1 (y-point p1))
        (x2 (x-point p2))
        (y2 (y-point p2))
        (x3 (x-point p3))
        (y3 (y-point p3)))
    (+ (* x1 (- y2 y3)) (* x2 (- y3 y1)) (* x3 (- y1 y2)))))

; return -1, 0 or 1 depending on orientation of z-coordinate
;(provide orientation)
(define (orientation p1 p2 p3)
  (let ((z (z-coord p1 p2 p3)))
    (if (= 0 z)
        z
        (/ z (abs z)))))

; test if line segments p and q intersect
;(provide segment-intersection?)
(define (segment-intersection? p1 p2 q1 q2)
  (and
   (not
    (=
     (orientation p1 p2 q1)
     (orientation p1 p2 q2)))
   (not
    (=
     (orientation q1 q2 p1)
     (orientation q1 q2 p2)))))

; point p inclusion in a triangle (v1,v2,v3)
;(provide point-in-triangle?)
(define (point-in-triangle? v1 v2 v3 p)
  (=
   (orientation v1 v2 p)
   (orientation v2 v3 p)
   (orientation v3 v1 p)))

; check triangle intersection for triangles (v1,v2,v3) and (w1,w2,w3)
; triangle intersect if either
; a) any points from one are inside the other, or
; b) any lines intersect
; (this is probably highly inefficient!)
;(provide triangle-intersection?)
(define (triangle-intersection? v1 v2 v3 w1 w2 w3)
  (or
   (or
    (point-in-triangle? v1 v2 v3 w1)
    (point-in-triangle? v1 v2 v3 w2)
    (point-in-triangle? v1 v2 v3 w3)
    (point-in-triangle? w1 w2 w3 v1)
    (point-in-triangle? w1 w2 w3 v2)
    (point-in-triangle? w1 w2 w3 v3))
   (or
    (segment-intersection? v1 v2 w1 w2)
    (segment-intersection? v1 v2 w2 w3)
    (segment-intersection? v1 v2 w3 w1)
    (segment-intersection? v2 v3 w1 w2)
    (segment-intersection? v2 v3 w2 w3)
    (segment-intersection? v2 v3 w3 w1)
    (segment-intersection? v3 v1 w1 w2)
    (segment-intersection? v3 v1 w2 w3)
    (segment-intersection? v3 v1 w3 w1))))

;; drawing

; background colour
(define bgcolour (colour 255 255 255))
;(provide set-bgcolour!)
(define (set-bgcolour! in-colour)
  ;(display in-colour)
  (set! bgcolour in-colour))

; moved mydraw up to platform-specific code section

; draw the lines
(define (draw-segs in-canvas segs)
  (if (null? segs)
      segs
      (begin
        (draw-seg in-canvas (car segs))
        (draw-segs in-canvas (cdr segs)))))

; draw the stamps
(define (draw-stamps in-canvas stamps)
  (if (null? stamps)
      stamps
      (begin
        (draw-stamp in-canvas (car stamps))
        (draw-stamps in-canvas (cdr stamps)))))

(define (draw-stamp in-canvas stamp)
  (if (eq? (car stamp) 'bitmap)
      (begin
        ; not implemented - alex
        ;(apply drawbitmap (append (list in-canvas) (cdr stamp)))
        'ok)
      (if (eq? (car stamp) 'normal)
          (apply drawtriangle (append (list in-canvas) (cdr stamp))))))


; draw a given line segment
(define (draw-seg in-canvas seg)
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg))
        (col (colour-segment seg))
        (wid (width-segment seg)))
    (drawline in-canvas (x-point p1) (y-point p1) (x-point p2) (y-point p2) col 'solid)))

; clear lines
;(provide clearlines)
(define (clearlines)
  (set! line-segments '())
  )

; clear stamps
;(provide clearstamps)
(define (clearstamps)
  (set! stamps '())
  )

; set draw callback
(define mydraw-callback '())
;(provide set-mydraw-callback!)
(define (set-mydraw-callback! incallback)
  (set! mydraw-callback incallback))

; refresh
;(provide refresh)
(define (refresh)
  (mydraw mydraw-callback))

; redraw screen:
; will redraw the current set of line segments;
; specify a callback for your own drawing procedures, ie. turtle
(define (mydraw callback)
  (if graphics-canvas
      (begin
        
        ; set background
        (set-background-color graphics-canvas (make-colour-from-list bgcolour))
        
        ; clear the buffer
        (clear-buffer graphics-canvas)

        ; draw line segments
        (draw-segs (get-buffer graphics-canvas) line-segments)

        ; draw stamps
        (draw-stamps (get-buffer graphics-canvas) stamps)

        ; customized drawing (if any)
        (if (procedure? callback)
            (callback (get-buffer graphics-canvas)))

        ; update canvas
        (update-canvas graphics-canvas)
        )))

; change size of graphics window
(define (set-graphics-size! in-w in-h)
  (if graphics-frame
      (begin
        (set! screen-width in-w)
        (set! screen-height in-h)
        (set-component-size graphics-frame in-w in-h))))

; using graphics?
(define usegraphics #f)
;(provide set-usegraphics!)
(define (set-usegraphics! flag)
  (set! usegraphics flag))
;(provide usegraphics?)
(define (usegraphics?)
  usegraphics)

; set init callback
(define init-callback '())
;(provide set-init-callback!)
(define (set-init-callback! incallback)
  (set! init-callback incallback))

; set reset callback
(define reset-callback '())
;(provide set-reset-callback!)
(define (set-reset-callback! incallback)
  (set! reset-callback incallback))

; set exit callback
(define exit-callback '())
;(provide set-exit-callback!)
(define (set-exit-callback! incallback)
  (set! exit-callback incallback))

; set mouse event callback - alex xxx
(define mouse-event-callback '())
;(provide set-mouse-event-callback!)
(define (set-mouse-event-callback! incallback)
  (set! mouse-event-callback incallback))

; adding mouse handling - alex xxx
(define (my-on-event event)
  (if (procedure? mouse-event-callback)
      (mouse-event-callback event)))

; handle resize: forces canvas to match size of window
(define (my-resize e)
  (let-values (((w h) (get-component-size graphics-canvas)))
    (set! screen-width w)
    (set! screen-height h)
    (create-buffer graphics-canvas)
    (refresh)))

; reset graphics
;(provide reset-graphics)
(define (reset-graphics)
;    (set-bgcolour! (colour 0 0 0))
  (clearlines)
  (clearstamps)
  (display "clear stamps")
  (if (procedure? reset-callback)
      (reset-callback)))

; hack for now
(define graphics-frame #f)
(define graphics-canvas #f)

; init and shutdown graphics
;(provide init-graphics)
(define (init-graphics)
  (let ((frame (make-window "Graphics")))
    ; set window size and show window
    (set-component-size frame screen-width screen-height)
    (set-component-visible frame #t)

    ; create the canvas
    (let ((glcanvas (create-canvas screen-width screen-height my-on-event my-resize refresh)))
      (add-component frame
                     glcanvas)

      ; remember canvas
      (set! graphics-canvas glcanvas)
      
      ; set background colour
      (set-background-color graphics-canvas (make-colour-from-list (colour 255 255 255)))
      
      ; remember frame
      (set! graphics-frame frame))
    
    ; call init-callback, if any
      (if (procedure? init-callback)
          (init-callback))))

; should the exit callback be called first?
;(provide exit-graphics)
(define (exit-graphics)
  (set-component-visible graphics-frame #f)
  (if (procedure? exit-callback)
      (exit-callback)))
;) ; for drscheme