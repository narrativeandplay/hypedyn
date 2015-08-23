;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2015
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

;; note the mouse event behavior is specific to hypedyn app
;  every other app using graph-editor should have it's own 

(begin
  (require "../kawa/color.scm")
  (require "../common/objects.scm")
  (require "../common/graph-editor.scm")
  (require "../kawa/graphics-kawa.scm"))

; export
(module-export set-hypedyn-graph-editor-mouse-event)

(define (set-hypedyn-graph-editor-mouse-event graph-ed-obj max-w max-h)
  
  ;; mhm stands for mouse handler mould
  ;; this object is used to change the mouse-event function embeded inside the object
  ;; the mouse-event-function is in turn the default mouse-event listener for 
  ;; the canvas of graph editor
  
  (define mhm-instant (ask graph-ed-obj 'get-mhm))
  
  ;; TODO: need further work 
  ;; by right there is no need to get callback from the graph-editor
  ;; since the code is on the application we could have it just calling the actual function 
  ;; from the app's code
  (define callback (ask graph-ed-obj 'get-callback))
  
;; for node drag #t 
  (define (node-drag-mouse-event x y click-count clicked)
    (ask graph-ed-obj 'begin-paint)
    (if (ask clicked 'is-shown?)
        (ask clicked 'hide #f))
    (ask graph-ed-obj 'end-paint)
    (let-values
        (((sw sh) (ask clicked 'get-size)))
      (let
          ((dc (ask graph-ed-obj 'get-buffer))
           (px (ask mhm-instant 'get-prev-x))
           (py (ask mhm-instant 'get-prev-y)))
        ; dragging effect, need to fiddle with this - alex
        (ask graph-ed-obj 'begin-paint)
        (if (and px py)
            (rectangle-fill dc
                            (- px (* sw 0.5)) (- py (* sh 0.5))
                            (+ px (* sw 0.5)) (+ py (* sh 0.5))
                            black-color 'xor
                            (make-colour-from-list bg-color)))
        (rectangle-fill dc
                        (- x (* sw 0.5)) (- y (* sh 0.5))
                        (+ x (* sw 0.5)) (+ y (* sh 0.5))
                        black-color 'xor
                        (make-colour-from-list bg-color))
        ; we have to refresh only the bitmap
        (ask graph-ed-obj 'end-paint)
        (ask graph-ed-obj 'my-refresh)
        (ask mhm-instant 'set-prev-x x)
        (ask mhm-instant 'set-prev-y y)
        )))

; node 'left-up #t
  (define (node-after-drag-mouse-event x y click-count clicked)
    (let
        ((dc (ask graph-ed-obj 'get-buffer))
         (px (ask mhm-instant 'get-prev-x))
         (py (ask mhm-instant 'get-prev-y)))

;      ; hiding the dragged square is not necessary, we will do a full redraw
;      ; do a layout if there was a dragging
      (if (and px py)
          (let-values
              (((w h) (ask clicked 'get-size)))
            ; ensure that no negative coordinates are allowed
            (if (< x 0)
                (ask clicked 'x-set! (* w 0.5))
                (if (> x max-w)
                    (ask clicked 'x-set! (- max-w (* w 0.5)))
                    (ask clicked 'x-set! x)
                    )
                )
            (if (< y 0)
                (ask clicked 'y-set! (* 0.5 (+ h tab-height tab-height)))
                (if (> y max-h)
                    (ask clicked 'y-set! (- max-h (* h 0.5)))
                    (ask clicked 'y-set! y)
                    ))

            (ask graph-ed-obj 'layout (ask clicked 'get-id))
            ) ;; end of let-values
          )
      
      
      (ask graph-ed-obj 'begin-paint)
      (ask clicked 'show #t)
;      (ask graph-ed-obj 'set-selected! (ask clicked 'get-id))
      (ask graph-ed-obj 'end-paint)
      
      ;; call node selecting event
      (node-left-clicked-mouse-event x y click-count clicked)
      
      (ask graph-ed-obj 'my-on-paint)
      
      ;(ask graph-ed-obj 'my-refresh)
      
      ;reset dragging previous marker 
      (ask mhm-instant 'set-prev-x #f)
      (ask mhm-instant 'set-prev-y #f)
      )
    )

;; 'line 'left-up #t (line selecting)
  (define (line-onclick-mouse-event x y click-count clicked)
    (let ((selected-node (ask graph-ed-obj 'get-selected)))

      (if selected-node
          (begin
            (if callback
                (callback 'deselect selected-node))
            (ask graph-ed-obj 'begin-paint)
            (ask selected-node 'show #f)
            ; we have to refresh only one node
            (ask graph-ed-obj 'end-paint)
            (ask graph-ed-obj 'my-refresh)
            (ask graph-ed-obj 'set-selected-node #f))
          )

      (ask graph-ed-obj 'begin-paint)
      (ask graph-ed-obj 'set-selected-line! clicked)
      (ask graph-ed-obj 'end-paint)

      (if callback (callback 'select-line clicked))
      (ask graph-ed-obj 'begin-paint)
      (ask clicked 'select)
      (ask graph-ed-obj 'end-paint)
      (ask graph-ed-obj 'my-refresh)))

;; deselecting (nothing clicked)
;; #f 'left-down #f

  (define (deselecting-mouse-event x y click-count clicked)
    (let ((selected-node (ask graph-ed-obj 'get-selected))
          (selected-line (ask graph-ed-obj 'get-selected-line)))
      (begin
        (if selected-node
            (begin
              (if callback
                 (callback 'deselect selected-node))
              (ask graph-ed-obj 'begin-paint)
              (ask selected-node 'show #f)
              ; we have to refresh only one node
              (ask graph-ed-obj 'end-paint)
              (ask graph-ed-obj 'my-refresh)
              (ask graph-ed-obj 'set-selected-node #f))
            )
        (if selected-line
            (begin (display "deselect line disable delete connection")
              (display selected-line)(newline)
              (if callback
                 (callback 'deselect-line selected-line))
              ;(ask c 'set-selected-line! #f)
              (display "LINNEE DESELECTING LINE")(newline)
              (ask selected-line 'deselect)
              (ask graph-ed-obj 'set-selected-line! #f)
              ;(ask graph-ed-obj 'my-refresh)
              (ask graph-ed-obj 'my-on-paint)
              (ask graph-ed-obj 'set-selected-line! #f)
              ))
        )))

;; selecting node (left clicked on node)
; node left-clicked #t
  ;; TODO: fix mouse-handler-mould seem to fire twice on left click
  ;;        when dragging node, left up also fire left click once
  ;;        so i suspect left click is fired when going down and coming up
  (define (node-left-clicked-mouse-event x y click-count clicked)
    (let ((px (ask mhm-instant 'get-prev-x))
          (py (ask mhm-instant 'get-prev-y))
          (selected-line (ask graph-ed-obj 'get-selected-line)))

      (ask graph-ed-obj 'begin-paint)
      (ask clicked 'show #t)
      (ask graph-ed-obj 'end-paint)

      ;; if previously a line selected deselect it
      (if selected-line
          (begin
            (if callback
                 (callback 'deselect-line selected-line))
            (ask selected-line 'deselect)
            ))
      
      (ask graph-ed-obj 'set-selected-node clicked)
      
      ; trigger enable buttons on clicked
      (if callback
          (cond 
           ((<= click-count 1)
            (callback 'select clicked)) ;selected-node
           ((= click-count 2)
            (callback 'editnode clicked))) ;edit-node
      )))

  (ask mhm-instant 'set-event-func 'node 'drag #t node-drag-mouse-event)
  (ask mhm-instant 'set-event-func 'node 'left-up #t node-after-drag-mouse-event) ;; end the drag
  ;(ask mhm-instant 'set-event-func 'line 'left-up #t line-onclick-mouse-event) ; this is handled by 'left-clicked
  (ask mhm-instant 'set-event-func #f 'left-down #f deselecting-mouse-event)
  (ask mhm-instant 'set-event-func 'node 'left-clicked #t node-left-clicked-mouse-event)
  )