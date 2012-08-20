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
;; 
;; This code is based on:
;; 
;; ##################################################################################
;; # ============================================================================== #
;; # Graph Editor                                                                   #
;; # http://www.hexahedron.hu/private/peteri/                                       #
;; # Copyright (C) Peter Ivanyi, 2007                                               #
;; # ============================================================================== #
;; #                                                                                #
;; # This program is free software; you can redistribute it and/or                  #
;; # modify it under the terms of the GNU General Public License                    #
;; # as published by the Free Software Foundation; either version 2                 #
;; # of the License, or (at your option) any later version.                         #
;; #                                                                                #
;; # This program is distributed in the hope that it will be useful,                #
;; # but WITHOUT ANY WARRANTY; without even the implied warranty of                 #
;; # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                  #
;; # GNU General Public License for more details.                                   #
;; #                                                                                #
;; # You should have received a copy of the GNU General Public License              #
;; # along with this program; if not, write to the Free Software                    #
;; # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.    #
;; #                                                                                #
;; ##################################################################################


;; TODO suggestions in the future::
;  1) allow for object overlap on the canvas (eg node partially overlapping)
;  2) keep track of which area belongs to which (eg clicking on overlapping areas, 
;     we should be able to know that we are clicking on the node on top)
;  3) noticed that some attributes are actually shared nodes, links, tabs
;     object system should have encapsulated them and have it inherited if possible
     
;(begin
  (require "objects.scm")
  (require "graphics.scm")
  (require "../kawa/graphics-kawa.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/color.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/panel.scm")
  (require "../kawa/ui/scrollpane.scm")
  (require "myhashtable.scm")
  (require 'hash-table)
  (require 'list-lib)
  ;)

; export
(module-export default-node-color
               create-graph-editor
               tab-width tab-height node-width node-height node-buffer)
(module-static 'init-run)


;;;; global var
; the default node colour
(define default-node-color  (list 91 91 184 255))

(define tab-width 16.0) ;changed to be bigger (previously 10) for ioblocks
(define tab-height 16.0) ;ioblocks
(define node-width 60.0)
(define node-height 25.0)
(define node-buffer 5.0)
(define text-height 12.0)

; use our object system, replace send with ask
; adding "typed object" as superclass for line, tab, node
; 
;; typed object
;(define-private (make-typed-object name obj-type)
;                (let ((named-obj (make-named-object name)))
;                  (lambda (message)
;                    (cond ((eq? message 'obj-type) (lambda (self) obj-type))
;                          (else (get-method named-obj message))))))

; ----------------------------------------------------------------------------
;;;; line
; ----------------------------------------------------------------------------
(define-private (create-line source target editor)
                (let ((the-line (make-line source target editor)))
                  the-line))
(define-private (make-line source target editor)
                (let* ((typed-obj (make-typed-object 'line))
                       (named-obj (new-make-named-object "line"))
                       (this-obj (new-object typed-obj named-obj)) ;; multiple inheritance
                       ; user defined data structure
                       ; if the user wants to store anything in the node
                       (data #f)
                       (visible? #f)
                       (selected? #f)
                       (custom-line-draw #f)
                       (ID #f))

                  ;; default function to draw a line from tab to tab (source to dest)
                  (define (default-line-draw dc source target selected? show? data)
                    (begin
                      (let-values
                          (((sx sy) (ask source 'get-line-position))
                           ((tx ty) (ask target 'get-line-position))
                           ;((sw sh) (ask src-node 'get-size))
                           ;((tw th) (ask dst-node 'get-size))
                           )
                        (if selected?
                            (begin
                              ;(display "drawing red line ")(display selected?)(newline)
                              (drawline dc sx sy tx ty red-color 'solid))
                            (begin
                              ;(display "drawing black line ")(display selected?)(newline)
                              (drawline dc sx sy tx ty black-color 'solid)))
                        )
                      
                      ;;drawing the data--name of line
                      (let-values
                          (((sx sy) (ask source 'get-position))
                           ((tx ty) (ask target 'get-position))
                           ((tw th td ta) (get-text-extent dc data text-height)))

                        ; should get panel background somehow? - alex
                        (rectangle-fill dc
                                        (- (/ (abs (+ tx sx)) 2.0) (/ tw 2.0))
                                        (- (/ (abs (+ ty sy)) 2.0) (/ th 2.0) )
                                        (+ (/ (abs (+ tx sx)) 2.0) (/ tw 2.0))
                                        (+ (/ (abs (+ ty sy)) 2.0) (/ th 2.0) )
                                        bg-color 'solid)
                        (drawtext dc
                                  (- (/ (abs (+ tx sx)) 2) (* tw 0.5)) (+ (/ (abs (+ ty sy)) 2) (* th 0.5))
                                  red-color bg-color data))
                      ))
                      
                  ; draw the line  
                  (define (draw show?)
                    (set! visible? show?)

                    ;; common things needed for both ioblocks and hypedyn
                    (let
                        ((dc (ask editor 'get-buffer)))
                      (if custom-line-draw
                          (custom-line-draw dc source target selected? show? data)
                          (default-line-draw dc source target selected? show? data))
                      )) ;; end of draw

                  ;; line's on-mouse callback (used by find-clicked)
                  (define (line-on-mouse self event-type cx cy)
                    (cond ((member event-type '(left-down))
                           (let
                               ((dc (ask editor 'get-buffer)))
                             (let-values
                                 (((sx sy) (ask source 'get-position))
                                  ((tx ty) (ask target 'get-position))
                                  ((tw th td ta) (get-text-extent dc data text-height)))
                               ;(display "cx ")(display cx)(newline)
                               ;(display "cy ")(display cy)(newline)


                               (if (and (<= (- (/ (abs (+ tx sx)) 2.0) (/ tw 2.0)) cx (+ tw (- (/ (abs (+ tx sx)) 2.0) (/ tw 2.0))))
                                        (<= (- (/ (abs (+ ty sy)) 2.0) (/ th 2.0) ) cy (+ th (- (/ (abs (+ ty sy)) 2) (/ th 2.0) ))))
                                   (begin (ask self 'select) self) #f))
                             ))
                          (else #f))
                    )

                  (obj-put this-obj 'get-data
                           (lambda (self) data))
                  (obj-put this-obj 'set-data
                           (lambda (self dat) (set! data dat)))
                  (obj-put this-obj 'is-shown?
                           (lambda (self) visible?))
                  (obj-put this-obj 'is-selected?
                           (lambda (self) selected?))
                  (obj-put this-obj 'select
                           (lambda (self)
                             (set! selected? #t)
                             (display "line select message")(newline)
                           ))
                  (obj-put this-obj 'deselect
                           (lambda (self)
                             (set! selected? #f)
                             (display "line deselect message")(newline)
                             ))
                  (obj-put this-obj 'get-source
                           (lambda (self) source))
                  (obj-put this-obj 'get-target
                           (lambda (self) target))
                  (obj-put this-obj 'set-target!
                           (lambda (self new-target)
                             (set! target new-target)))
                  (obj-put this-obj 'show
                           (lambda (self) (draw #t)))
                  (obj-put this-obj 'hide
                           (lambda (self) (draw #f)))
                  (obj-put this-obj 'on-mouse
                           (lambda (self event-type cx cy)
                             (line-on-mouse self event-type cx cy)))
                  (obj-put this-obj 'set-custom-line-draw
                           (lambda (self drawfunc)
                             (set! custom-line-draw drawfunc)))
                  (obj-put this-obj 'set-ID
                           (lambda (self in-ID)
                             (set! ID in-ID)))
                  (obj-put this-obj 'get-ID
                           (lambda (self) ID))
                  this-obj))

; ----------------------------------------------------------------------------
;;;; tab
; ----------------------------------------------------------------------------
(define-private (create-tab x y type node editor . name)
                (if (not (null? name))
                    (let ((the-tab (make-tab x y type node editor (car name))))
                      the-tab)
                    (let ((the-tab (make-tab x y type node editor)))
                      the-tab)
                    ))
(define-private (make-tab x y type node editor . name)
                (let* ((typed-obj (make-typed-object 'tab))
                       (named-obj (new-make-named-object "tab"))
                       (this-obj (new-object typed-obj named-obj))
                       (data #f)
                       (visible? #f)
                       ; all lines connected to this tab
                       (lines '())
                       (name (if (and name
                                      (not (null? name)))
                                 (car name)))
                       (custom-tab-draw #f))

                  ;(define (init self)

                  ;; default way of drawing tabs
                  (define (default-tab-draw dc type show?)
                    (cond
                     ;for out port tabs
                     ((equal? type 'out)
                      (drawtabsquare dc show? x y)
                      ; draw the text          
                      (let-values
                          (((tw th td ta) (get-text-extent dc "O" text-height))) ; previously data was name in hypedyn version
                        (drawtext dc (- x (* tw 0.5)) (+ y (* th 0.5)) ; center text around center of x and y
                                  white-color bg-color "O")))

                     ;for in port tabs
                     ((equal? type 'in)
                      (drawtabsquare dc show? x y)
                      ;(- x (* tab-width 0.5)) (- y (* tab-width 0.5)))
                      (let-values
                          (((tw th td ta) (get-text-extent dc "I" text-height)))
                        (drawtext dc (- x (* tw 0.5)) (+ y (* th 0.5)) ; center text around center of x and y
                                  white-color bg-color "I")
                        ;(drawtext dc x y white-color bg-color "I")
                        )
                      )
                     (else (begin
                             (display "else reached in draw show? of tab")(newline)))))
                  
                  ;; draw the tab
                  (define (draw show?)
                    (set! visible? show?)
                    (let* ((dc (ask editor 'get-buffer))
                           (parent-node node)
                           ;;                         (ioblocks-node? (is-ioblocks-node? parent-node));(member 'ioblocks-node (ask parent-node 'get-style) ))
                           ;;                         (hidden-tabs? (member 'hidden-tabs (ask parent-node 'get-style) ))
                           )
                      
                      ;; use custom draw if it is set
                      (if custom-tab-draw 
                          (custom-tab-draw dc type show?)
                          (default-tab-draw dc type show?))

                      ;; ask all lines from this tab to draw
                      (for-each
                       (lambda (lin)
                         (if show?
                             (ask lin 'show)
                             (ask lin 'hide)))
                       lines)
                      
                      ))

                  (define (drawtabsquare dc show? bx by)
                    ; draw the tab square ;derived from drawnodesquare
                    ; draw boundary border
                    (if show?
                        (rectangle-fill dc
                                        (- bx (/ tab-width 2.0))
                                        (- by (/ tab-height 2.0))
                                        (+ (- bx (/ tab-width 2.0)) tab-width)
                                        (+ (- by (/ tab-height 2.0)) tab-height)
                                        default-node-color 'solid)
                        (rectangle-fill dc
                                        (- bx (/ tab-width 2.0))
                                        (- by (/ tab-height 2.0))
                                        (+ (- bx (/ tab-width 2.0)) tab-width)
                                        (+ (- by (/ tab-height 2.0)) tab-height)
                                        white-color 'solid))

                    ; draw a white line on top and left
                    (drawline dc
                              (- bx (/ tab-width 2.0))
                              (- by (/ tab-height 2.0))
                              (- bx (/ tab-width 2.0))
                              (+ (- by (/ tab-height 2.0)) tab-height)
                              white-color 'solid)
                    (drawline dc
                              (- bx (/ tab-width 2.0))
                              (- by (/ tab-height 2.0))
                              (+ (- bx (/ tab-width 2.0)) tab-width)
                              (- by (/ tab-height 2.0))
                              white-color 'solid))

                  ;; tab on mouse
                  (define (on-mouse self etype cx cy)
                    (cond
                     ((member etype '(left-down left-up))
                      (cond
                       ((and 
                         (<= (- x (* tab-width  0.5)) cx (+ x (* tab-width  0.5)))
                         (<= (- y (* tab-height 0.5)) cy (+ y (* tab-height 0.5)))
                         ; x y now corresponse to the middle of the tab
                         )
                        self)
                       (else
;;                        (if (is-ioblocks-tab? self) ;; do ioblocks thing (ask whether line is clicked on)
                            (let ((click-line-list (map (lambda (line) (ask line 'on-mouse etype cx cy)) lines)))
                              (if (null-list? click-line-list)
                                  #f
                                  (car click-line-list)))
;;                            #f)

                        ) ;; end of else
                       
                       )) ;;end of left-down left-up

                     ;; mouse over only for ioblocks
                     ((member etype '(motion))
                      ;; issit ok for mouse over tabs to be there for all apps?
;                      (and (member etype '(motion))
;                           (is-ioblocks-tab? self))
                      (cond
                       ((if (and (<= (- x (* tab-width  0.5)) cx (+ x (* tab-width  0.5)))
                                 (<= (- y (* tab-height 0.5)) cy (+ y (* tab-height 0.5)))) ; x y now corresponse to the middle of the tab  
                            self
                            #f))))

                     (else #f)))

                  (obj-put this-obj 'get-data
                           (lambda (self) data))
                  (obj-put this-obj 'clear-lines
                           (lambda (self) (set! lines '())))
                  (obj-put this-obj 'line-add
                           (lambda (self line)
                             (set! lines (cons line lines))))
                  (obj-put this-obj 'line-del
                           (lambda (self line)
                             (if line
                                 ; need to pass in a pred for list-lib version of remove - alex
                                 (set! lines (remove (lambda (x) (eq? line x)) lines)))))
                  (obj-put this-obj 'get-lines
                           (lambda (self) lines))
                  (obj-put this-obj 'connected?
                           (lambda (self) (> (length lines) 0)))
                  (obj-put this-obj 'is-shown?
                           (lambda (self) visible?))
                  (obj-put this-obj 'get-type
                           (lambda (self) type))
                  (obj-put this-obj 'get-node 
                           (lambda (self) node))
                  (obj-put this-obj 'x-set!
                           (lambda (self cx) (set! x cx)))
                  (obj-put this-obj 'y-set!
                           (lambda (self cy) (set! y cy)))
                  (obj-put this-obj 'get-position
                           (lambda (self) (values x y)))
                  (obj-put this-obj 'get-line-position
                           (lambda (self)
                             (begin
                               (if (equal? type 'in)
                                   ;(values x (- y tab-height))
                                   ;(values x (+ y tab-height -1))))))
                                   (values x (- y (/ tab-height 2.0)))
                                   (values x (+ y (/ tab-height 2.0) -1.0)))))) ;ioblocks change cos x y is center of tab
                  (obj-put this-obj 'show
                           (lambda (self) (draw #t)))
                  (obj-put this-obj 'hide
                           (lambda (self) (draw #f)))
                  (obj-put this-obj 'on-mouse
                           (lambda (self etype cx cy)
                             (on-mouse self etype cx cy)))
                  (obj-put this-obj 'get-name
                           (lambda (self) name))
                  (obj-put this-obj 'set-custom-tab-draw
                           (lambda (self drawfunc)
                             (set! custom-tab-draw drawfunc)))
                  this-obj))

; ----------------------------------------------------------------------------
;;;; node
; ----------------------------------------------------------------------------

(define-private (create-node id data name x y editor style)
                (let ((the-node (make-node id data name x y editor style)))
                  (ask the-node 'init)
                  the-node))
(define-private (make-node id data name x y editor style)
                (let* ((typed-obj (make-typed-object 'node))
                       (named-obj (new-make-named-object name))
                       (this-obj (new-object typed-obj named-obj))
                      ; note: make-hash-table defaults to equal? for kawa
                       (tab-in (make-hash-table)) ;'equal))
                       (tab-out (make-hash-table)) ;'equal))
                       (width 0.0)
                       (height 0.0)
                       (emph-height 0.0) ; add a height to show emphasis, for ht2009 paper - alex
                       (offset 5)
                       (visible? #f)
                       (is-selected? #f) ; remember if we're selected so can redraw correctly - alex
                      
                      ;; for drawing
                       (custom-draw #f) ;custom-draw provides a way to put custom drawings for the node
                       (custom-node-text-draw #f)
                       (custom-offset (list)) ;; offset from the node's centre
                       (node-color default-node-color)
                       (text-color white-color)
                       (node-type #f))
                  
                  ;; private methods

                  (define (get-style)
                    style)

                  (define (get-real-width)
;                    (display "in get real width ")(newline)
;                    (display "dc ")(display (ask editor 'get-buffer))(newline)
;                    (display "name ")(display name)(newline)
;                    (display "text-height ")(display text-height)(newline)
                    (let
                        ((dc (ask editor 'get-buffer)))
                      (let-values
                          (((tw th td ta) (get-text-extent dc name text-height)))
                        (max 70.0 (+ (* offset 2.0) tw)))))

                  (define (get-real-height)
                    (let
                        ((dc (ask editor 'get-buffer)))
                      (let-values
                          (((tw th td ta) (get-text-extent dc name text-height)))
                        ;(max 30 (+ (* offset 2) ta td th))
                        (max emph-height (+ (* offset 2.0) ta td th)))))

                  (define (x-set! cx)
                    (if (ask editor 'snap-to-grid?)
                        ; snap to grid
                        (let* ((xgrid (+ node-width (* 4 node-buffer)))
                               (xgridnum (round (div cx xgrid))))
                          (set! x (* xgrid xgridnum)))
                        (set! x cx))

                    ; update tab positions
                    (try-catch
                        (begin
                          (let*
                              ((n (- (hash-table-count tab-in) 1))
                               (req-width (* (+ n n 3.0) tab-width))
                               (left (* (- width req-width) 0.5))
                               (sx (+ (- x (* width 0.5)) left (* 1.5 tab-width))))
                            (do ((i 0 (+ i 1))) ((= i (hash-table-count tab-in)))
                                (let
                                    ((tab (hash-table-get tab-in i #f)))
                                  (ask tab 'x-set! sx)
                                  (set! sx (+ sx (* 2.0 tab-width))))))
                          (let*
                              ((n (- (hash-table-count tab-out) 1))
                               (req-width (* (+ n n 3.0) tab-width))
                               (left (* (- width req-width) 0.5))
                               (sx (+ (- x (* width 0.5)) left (* 1.5 tab-width)))
                               )
                            (do ((i 0 (+ i 1))) ((= i (hash-table-count tab-out)))
                                (let
                                    ((tab (hash-table-get tab-out i #f)))
                                  (ask tab 'x-set! sx)
                                  (set! sx (+ sx (* 2.0 tab-width)))))))
                      (ex <java.lang.Throwable>
                          (begin
                            (display (*:toString ex))(newline)
                            (*:printStackTrace ex)
                            )))
                    )

                  (define (y-set! cy)
                    (if (ask editor 'snap-to-grid?)
                        ; snap to grid
                        (let* ((ygrid (+ node-height (* 4 node-buffer)))
                               (ygridnum (round (div cy ygrid))))
                          (set! y (* ygrid ygridnum )))
                        (set! y cy))
                    
                    ; update tab positions
                    (do ((i 0 (+ i 1))) ((= i (hash-table-count tab-in)))
                        (let
                            ((tab (hash-table-get tab-in i #f)))
                          (ask tab 'y-set! (- (- y (/ height 2.0)) (* tab-height 0.5))))) ;changed by ioblocks cos x y now center of tab
                    (do ((i 0 (+ i 1))) ((= i (hash-table-count tab-out)))
                        (let
                            ((tab (hash-table-get tab-out i #f)))
                          (ask tab 'y-set! (+ (+ y (/ height 2.0)) (* tab-height 0.5))))))

                  (define (set-emph-height in-height)
                    (ask self 'hide self )
                    (set! emph-height in-height)
                    (set! height (get-real-height))
                    (ask self 'show #f)
                    (ask editor 'layout id))

                  ; check whether any of the tabs is connected
                  (define (tab-connected? tab-list)
                    (let
                        ((connected? #f)
                         (n (hash-table-count tab-list)))
                      (do ((i 0 (+ i 1))) ((or connected? (= i n)))
                          (set! connected? (or connected?
                                               (ask (hash-table-get tab-list i #f) 'connected?))))
                      connected?))

                  ; default tab-decr
                  (define (tab-decr self tab-list type)
                    (let*
                        ((n (hash-table-count tab-list)))
                      (if (> n 0)
                          (let
                              ((last (hash-table-get tab-list (- n 1) #f)))
                            (if (not (ask last 'connected?))
                                (let
                                    ((n (- n 1))
                                     (ni (hash-table-count tab-in))
                                     (no (hash-table-count tab-out))
                                     (max-width #f)
                                     (req-width (* (+ n n 0.0) tab-width)))
                                  (ask self 'hide #f)
                                  (hash-table-remove! tab-list n)
                                  (if (equal? type 'in)
                                      (set! max-width (max (* (- (+ ni ni) 1.0) tab-width)
                                                           (* (+ (+ no no) 1.0) tab-width)))
                                      (set! max-width (max (* (+ (+ ni ni) 1.0) tab-width)
                                                           (* (- (+ no no) 1.0) tab-width))))
                                  (set! width (max (get-real-width) max-width))
                                  (let*
                                      ((left (* (- width req-width) 0.5))
                                       (sx (+ (- x (* width 0.5)) left (* 2.0 tab-width))))
                                    (do ((i 0 (+ i 1))) ((= i n))
                                        (let
                                            ((tab (hash-table-get tab-list i #f)))
                                          (ask tab 'x-set! sx)
                                          (set! sx (+ sx (* 2.0 tab-width))))))
                                  (ask self 'show #f)
                                  (ask editor 'my-on-paint)))))))

                  ;; the default tab-incr
                  (define (tab-incr self tab-list type . name)
                    (ask self hide #f)
                    (let*((n (hash-table-count tab-list))
                          (ni (hash-table-count tab-in))
                          (no (hash-table-count tab-out))
                          (max-width #f)
                          (req-width (* (+ n n 3.0) tab-width))
                          (tab-name (car name)))
                      
                      ;; expand accordingly if tab cant fit the width of node
                      (if (equal? type 'in)
                          (set! max-width (max (* (+ ni ni 3.0) tab-width)
                                               (* (+ no no 1.0) tab-width)))
                          (set! max-width (max (* (+ ni ni 1.0) tab-width)
                                               (* (+ no no 3.0) tab-width))))
                      (set! width (max (get-real-width) max-width))
                      (let*
                          ((left (* (- width req-width) 0.5))
                           (sx (+ (- x (* width 0.5)) left (* 1.5 tab-width))))
                        (do ((i 0 (+ i 1))) ((= i n))
                            (let
                                ((tab (hash-table-get tab-list i #f)))
                              (ask tab 'x-set! sx)
                              (set! sx (+ sx (* 2.0 tab-width)))))
                        (let
                            ((tab (create-tab
                                   sx
                                   (if (equal? type 'in)
                                       (- (- y (/ height 2.0)) (* tab-height 0.5)) ; set at bottom of node AND shift to center of tab
                                       (+ (+ y (/ height 2.0)) (* tab-height 0.5)) ; set at top of node AND shift to center of tab
                                       )
                                   type self editor tab-name)))
                          (hash-table-put! tab-list n tab)
                          tab))
                      (ask self 'show #f)
                      (ask editor 'layout id)
                      ))
                  
                  ;; custom tab-incr with control over the 
                  ;; offset with respect to the node
                  (define (custom-tab-incr self x y type custom-tab-draw)
                    (let ((which-tab-hash #f)
                          (newtab (create-tab
                                          x y 
                                          type self editor)))
                      ;; determine which tab hash-table to add to
                      ;; in or out
                      (if (eq? type 'in)
                          (set! which-tab-hash tab-in)
                          (set! which-tab-hash tab-out))
                      (ask newtab 'set-custom-tab-draw custom-tab-draw)
                        (hash-table-put! which-tab-hash
                                         (hash-table-count which-tab-hash)
                                         newtab
                                         )))

                  ; draw node square helper - move this to a subclass? - alex
                  (define (drawnodesquare dc show? bx by)
                    ; draw the node square
                    ; draw boundary border
                    (if show?
                        (rectangle-fill dc
                                        (- bx (/ width 2.0))
                                        (- by (/ height 2.0))
                                        (+ bx (/ width 2.0))
                                        (+ by (/ height 2.0))
                                        node-color 'solid)
                        (rectangle-fill dc
                                        (- bx (/ width 2.0))
                                        (- by (/ height 2.0))
                                        (+ bx (/ width 2.0))
                                        (+ by (/ height 2.0))
                                        white-color 'solid))

                    ; draw a white line on top and left
                    (drawline dc
                              (- bx (/ width 2.0))
                              (- by (/ height 2.0))
                              (- bx (/ width 2.0))
                              (+ by (/ height 2.0))
                              white-color 'solid)
                    (drawline dc
                              (- bx (/ width 2.0))
                              (- by (/ height 2.0))
                              (+ bx (/ width 2.0))
                              (- by (/ height 2.0))
                              white-color 'solid)

                    ; draw a black line on bottom and right
                    (drawline dc
                              (- bx (/ width 2.0))
                              (- (+ by (/ height 2.0)) 1.0)
                              (+ bx (/ width 2.0))
                              (- (+ by (/ height 2.0)) 1.0)
                              black-color 'solid)
                    (drawline dc
                              (+ bx (/ width 2.0))
                              (- by (/ height 2.0))
                              (+ bx (/ width 2.0))
                              (- (+ by (/ height 2.0)) 1.0)
                              black-color 'solid)
                    )
                  
                  (define (default-node-draw show? selected?)
                    (let
                        ((dc (ask editor 'get-buffer))
                         (color #f))
                    (begin ;; else draw default style
                            ; draw or undraw selected square

                            (if (and show? selected? )
                                (set! color grey-color)
                                (set! color bg-color))
                            (rectangle-fill dc
                                            (- x (/ width 2.0))
                                            (- y (/ height 2.0) tab-height)
                                            (if (member 'alt (get-style)) (+ 4.0 x (/ width 2.0)) (+ x (/ width 2.0)))
                                            (+ y (/ height 2.0) tab-height)
                                            color 'solid)
                            ; draw node square
                            (if (member 'alt (get-style))
                                (begin
                                  (drawnodesquare dc show? (+ x 4.0) (- y 4.0))
                                  (drawnodesquare dc show? (+ x 2.0) (- y 2.0))))
                            (drawnodesquare dc show? x y))))
                  
                  ;; draw the node (overriden in hypedyn)
                  (define (draw self show? selected?)
                    (set! visible? show?)
                    (set! is-selected? selected?)
                    (let ((dc (ask editor 'get-buffer))
                          (color #f))

                      (if (procedure? custom-draw)
                          ;; if there is a custom draw then draw using that
                          (begin
                            (custom-draw dc x y bg-color selected? data)
                            
                            )
                          ;; if not draw default drawing
                          (default-node-draw show? selected?)
                          )

                      ;; custom draw text if any
                      (let-values
                          (((tw th td ta) (get-text-extent dc name text-height)))
                        (if (procedure? custom-node-text-draw)
                            (custom-node-text-draw dc x y tw th td ta text-color bg-color (ask self 'name) text-height)
                            (drawtext dc (- x (* tw 0.5)) (+ y (* th 0.5))
                                      text-color bg-color (ask self 'name)))))

                    ;; go through all in tab of this node and draw them
                    (do ((i 0 (+ i 1))) ((= i (hash-table-count tab-in)))
                        (let
                            ((tab (hash-table-get tab-in i #f)))
                          (if show?
                              (ask tab 'show)
                              (ask tab 'hide))))

                    ;; go through all out tab of this node and draw them
                    (do ((i 0 (+ i 1))) ((= i (hash-table-count tab-out)))
                        (let
                            ((tab (hash-table-get tab-out i)))
                          (if show?
                              (ask tab 'show)
                              (ask tab 'hide))))
                    )

                  (define (show self selected?)
                    (ask self 'draw #t selected?)
                    )

                  (define (hide self selected?)
                    (ask self 'draw #f selected?)
                    )

                  ;; node on mouse
                  (define (on-mouse self type cx cy)
                    (cond
                     ((member type '(left-down left-up motion right-down right-up))
                      (let-values (((width height) (ask self 'get-size)))
                        (cond
                         ((and (<= (- x (* width  0.5)) cx (+ x (* width  0.5)))
                               (<= (- y (* height 0.5)) cy (+ y (* height 0.5))))
                          self)
                         (else
                          (let
                              ((found? #f)
                               (n-in  (hash-table-count tab-in))
                               (n-out (hash-table-count tab-out)))
                            (do ((i 0 (+ i 1))) ((or found? (= i n-in)))
                                (set! found? (ask (hash-table-get tab-in i #f) 'on-mouse type cx cy)))
                            (do ((i 0 (+ i 1))) ((or found? (= i n-out)))
                                (set! found? (ask (hash-table-get tab-out i #f) 'on-mouse type cx cy)))
                            found?)
                          ))))
                     (else #f)))

                  ; set node colour
                  (define (set-node-color self in-color)
                    (begin
                      (set! node-color in-color)
                      (ask self 'draw visible? is-selected?)
                      (ask editor 'my-refresh)))

                  ; set text colour
                  (define (set-text-color self in-color)
                    (begin
                      (set! text-color in-color)
                      (ask self 'draw visible? is-selected?)
                      (ask editor 'my-refresh)))

                  ;; initialize

                  (define (init self)
                    ; determine the size
                    (set! width  (get-real-width))
                    (set! height (get-real-height))

                    ;; if ioblocks-node is in style it just means the node is not 
                    ;; initiated with any input or output tabs

                    ;; lets just not put any tabs for nodes 
                    ;; and have the person using graphed add the tabs manually like we do
                   
                    ; create input tabs 
                    ;; note there is no app using no-input or no-output style
;                    (if (and (not (member 'no-input style))
;                             (not (is-ioblocks-node? self)))

;                        (hash-table-put! tab-in
;                                         (hash-table-count tab-in)
;                                         (create-tab
;                                          x (- y (/ height 2.0))
;                                          'in  self editor)))
;                    ; create output tabs
;                    (if (and (not (member 'no-output style))
;                             (not (is-ioblocks-node? self)))
;                        (hash-table-put! tab-out
;                                         (hash-table-count tab-out)
;                                         (create-tab
;                                          x (+ y (/ height 2.0))
;                                          'out self editor)))
                    )

                  ;; NODE public methods
                  (obj-put this-obj 'init
                           (lambda (self) (init self)))
                  (obj-put this-obj 'get-data
                           (lambda (self) data))
                  (obj-put this-obj 'set-data
                           (lambda (self dat) (set! data dat)))
                  (obj-put this-obj 'get-id
                           (lambda (self) id))
                  (obj-put this-obj 'get-name
                           (lambda (self) name))
                  (obj-put this-obj 'set-name
                           (lambda (self str)
                             (begin
                               (ask self 'hide #f)
                               (ask self 'set-name! str)
                               (set! width (get-real-width))
                               (ask self 'show #f)
                               (ask editor 'layout id))))
                  (obj-put this-obj 'get-x
                           (lambda (self) x))
                  (obj-put this-obj 'get-y
                           (lambda (self) y))
                  (obj-put this-obj 'get-style
                           (lambda (self) style))
                  (obj-put this-obj 'get-visible?
                           (lambda (self) visible?))
                  (obj-put this-obj 'get-selected?
                           (lambda (self) is-selected?))
                  (obj-put this-obj 'set-style
                           (lambda (self in-style)
                             (begin
                               (ask self 'hide #f)
                               (set! style in-style)
                               (ask self 'show #f)
                               (ask editor 'layout id))))
                  (obj-put this-obj 'is-shown?
                           (lambda (self) visible?))
                  (obj-put this-obj 'x-set!
                           (lambda (self cx)
                             (x-set! cx)))
                  (obj-put this-obj 'y-set!
                           (lambda (self cy)
                             (y-set! cy)))
                  (obj-put this-obj 'set-emph-height
                           (lambda (self in-height)
                             (set-emph-height in-height)))
                  (obj-put this-obj 'get-position
                           (lambda (self) (values x y)))
                  (obj-put this-obj 'get-size
                           (lambda (self)
                             (values width height)))
                  (obj-put this-obj 'tab-in-count
                           (lambda (self)
                             (hash-table-count tab-in)))
                  (obj-put this-obj 'tab-out-count
                           (lambda (self)
                             (hash-table-count tab-out)))
                  (obj-put this-obj 'tab-in-ref
                           (lambda (self i)
                             (begin
                               (if (>= i 0)
                                   (hash-table-get tab-in i #f)))))
                  (obj-put this-obj 'tab-out-ref
                           (lambda (self i)
                             (begin
                               (if (>= i 0)
                                   (hash-table-get tab-out i #f)))))
                          ;; get in tab by name
                  (obj-put this-obj 'tab-in-get
                           (lambda (self tabname)
                             ;(hash-table-get tab-in tabname)
                             (let ((tab-to-return #f))
                               (hash-table-walk tab-in (lambda (key tab) (if (equal? (ask tab 'get-name) tabname)
                                                                             (set! tab-to-return tab))))
                               tab-to-return)))
                          ;; get out tab by name
                  (obj-put this-obj 'tab-out-get
                           (lambda (self tabname)
                             ;(hash-table-get tab-in tabname)
                             (let ((tab-to-return #f))
                               (hash-table-walk tab-out (lambda (key tab) (if (equal? (ask tab 'get-name) tabname)
                                                                              (set! tab-to-return tab))))
                               tab-to-return)))
                  (obj-put this-obj 'get-tab-in-list
                           (lambda (self) tab-in))
                  (obj-put this-obj 'get-tab-out-list
                           (lambda (self) tab-out))
                  (obj-put this-obj 'tab-in-connected?
                           (lambda (self)
                             (tab-connected? tab-in)))
                  (obj-put this-obj 'tab-out-connected?
                           (lambda (self)
                             (tab-connected? tab-out)))
                  (obj-put this-obj 'tab-in-decr
                           (lambda (self)
                             (tab-decr self tab-in 'in)))
                  (obj-put this-obj 'tab-out-decr
                           (lambda (self)
                             (tab-decr self tab-out 'out)))
                  (obj-put this-obj 'tab-in-incr
                           (lambda (self . portname)                 ;ioblocks need portname
                             (if (not (null? portname))
                                 (tab-incr self tab-in 'in (car portname))
                                 (tab-incr self tab-in 'in ))))
                  (obj-put this-obj 'tab-out-incr
                           (lambda (self . portname)                 ;ioblocks need portname
                             (if (not (null? portname))
                                 (tab-incr self tab-out 'out (car portname))
                                 (tab-incr self tab-out 'out ))))
                  (obj-put this-obj 'show show)
                  ;; for hide shouldn't we just don't draw?
                  (obj-put this-obj 'hide hide)
                  (obj-put this-obj 'draw draw)
                  (obj-put this-obj 'on-mouse
                           (lambda (self type cx cy)
                             (on-mouse self type cx cy)))
                  (obj-put this-obj 'set-custom-node-draw
                           (lambda (self drawfunc)
                             (set! custom-draw drawfunc)))
                  (obj-put this-obj 'set-custom-node-text-draw
                           (lambda (self func)
                             (set! custom-node-text-draw func)
                             ))
                  (obj-put this-obj 'set-node-color set-node-color)
                  (obj-put this-obj 'set-text-color set-text-color)
                  (obj-put this-obj 'set-size
                           (lambda (self newwidth newheight)
                             (set! width newwidth)
                             (set! height newheight)))
                  (obj-put this-obj 'custom-tab-incr
                           (lambda (self x y type custom-tab-draw)
                             (custom-tab-incr self x y type custom-tab-draw)))
                  this-obj))


; ----------------------------------------------------------------------------
;;;; graph editor
; ----------------------------------------------------------------------------

(define (create-graph-editor width height callback user-app)
  (let ((the-editor (make-graph-editor width height callback user-app)))
    (ask the-editor 'init)
    the-editor))
(define-private (make-graph-editor width height callback user-app)
                (let* ((typed-obj (make-typed-object 'editor))
                       (named-obj (new-make-named-object "editor"))
                       (this-obj (new-object typed-obj named-obj))
                       ; some settings
                       (allow-overlap #f) ; determines whether nodes can overlap or not
                       (snap-to-grid #f) ; determines whether nodes snap to grid or not
                       
                       ;; for drawing
                       ; the canvas
                       (glcanvas #f)
                       (zoomfactor 1.0)
                       (allow-repaint #t) ; determines whether a call to my-on-paint actually repaints, for optimization
                       
                       ; the selected node
                       (selected-node #f)
                       (selected-line #f)
                       
                       (x-offset 0)
                       (y-offset 0)
                       (px #f)
                       (py #f)
                       
                       ; the nodes in the graph, nodes are identified by an integer number
                       (nodes (make-hash-table)) ; 'equal))
                       (links (list (list 'ID 'line)))
                       (custom-nodes-hash (make-hash-table))
                       
                       ; the maximum id number in the hash table
                       (max-id 0)
                       (clicked #f)
                       (mouse-overed "Ready")
                       
                       (self-reference #f)
                       (application-symbol #f) ;;denotes which program is using graph-editor
                       (bgdraw #f) ;; a function to do some drawing before drawing the actual nodes
                       (mhm #f) ;; mouse handler mould
                       )

                  ; initialization
                  (define (init self)
                    ; create canvas
                    (set! glcanvas
                          (create-canvas
                           width
                           height
                           ;my-on-event ; mouse event handling
                           (ask mhm 'get-mouse-handler)
                           on-size   ; resize handling
                           #f ;my-refresh ; redraw handling - not necessary since we draw to offscreen buffer
                           ))
                    
                    ; set background colour
                    (set-gpanel-bgcolour glcanvas (make-colour-from-list bg-color))
                    
                    ; reference to ourself
                    (set! self-reference self)
                    (set! application-symbol user-app)
                    )

                  ; private methods

                  (define (get-selected)
                    selected-node)

                  ; added testing that node exists
                  ; TL: ID is not equivalent to the data we put in 
                  ; I hope nobody is using this
                  ; I am, seems to work :) - alex
                  (define (set-selected! ID)
;;                    (display "setting selecting in ge")(newline)
;;                    (display "ID ")(display ID)(newline)
                    (let ((node (node-get-by-data ID)))
                      (if node
                          (ask node 'show #t))
                      (set! selected-node node)
                      (my-on-paint)))
                  
                  ; scroll to make the specified node visible
                  (define (scroll-to-node ID)
                    (let ((node (node-get-by-data ID)))
                      (if node
                          (let-values (((x y) (ask node 'get-position))
                                       ((w h) (ask node 'get-size)))
                            (scroll-rect-to-visible glcanvas
                                                    (make-rectangle (graph->canvas (- x (/ w 2)))
                                                                    (graph->canvas (- y (/ h 2) tab-height))
                                                                    (graph->canvas w)
                                                                    (graph->canvas (+ h (* 2 tab-height)))))))))

                  (define (distance x1 y1 x2 y2)
                    (let
                        ((dx (- x2 x1))
                         (dy (- y2 y1)))
                      (sqrt (+ (* dx dx) (* dy dy)))))

                  (define (is-inside? x1 y1 x2 y2 x y)
                    (and (<= (min x1 x2) x (max x1 x2))
                         (<= (min y1 y2) y (max y1 y2))))

                  (define (overlap? x1 y1 w1 h1
                                    x2 y2 w2 h2)
                    (let
                        ((halfw1 (* 0.5 w1))
                         (halfh1 (* 0.5 h1))
                         (halfw2 (* 0.5 w2))
                         (halfh2 (* 0.5 h2)))
                      (or (is-inside? (- x2 halfw2) (- y2 halfh2) (+ x2 halfw2) (+ y2 halfh2)
                                      (- x1 halfw1) (- y1 halfh1))
                          (is-inside? (- x2 halfw2) (- y2 halfh2) (+ x2 halfw2) (+ y2 halfh2)
                                      (+ x1 halfw1) (- y1 halfh1))
                          (is-inside? (- x2 halfw2) (- y2 halfh2) (+ x2 halfw2) (+ y2 halfh2)
                                      (+ x1 halfw1) (+ y1 halfh1))
                          (is-inside? (- x2 halfw2) (- y2 halfh2) (+ x2 halfw2) (+ y2 halfh2)
                                      (- x1 halfw1) (+ y1 halfh1))
                          ;;;;;
                          (is-inside? (- x1 halfw1) (- y1 halfh1) (+ x1 halfw1) (+ y1 halfh1)
                                      (- x2 halfw2) (- y2 halfh2))
                          (is-inside? (- x1 halfw1) (- y1 halfh1) (+ x1 halfw1) (+ y1 halfh1)
                                      (+ x2 halfw2) (- y2 halfh2))
                          (is-inside? (- x1 halfw1) (- y1 halfh1) (+ x1 halfw1) (+ y1 halfh1)
                                      (+ x2 halfw2) (+ y2 halfh2))
                          (is-inside? (- x1 halfw1) (- y1 halfh1) (+ x1 halfw1) (+ y1 halfh1)
                                      (- x2 halfw2) (+ y2 halfh2)))))

                  ; this function enforces the node placement strategy, so
                  ; the nodes cannot overlap
                  (define (layout-aux id)
                    (if id
                        (let ((ok? #t)
                              (n max-id)
                              (anode (hash-table-get nodes id #f)))
                          ; maybe anode does not exist
                          (do ((i 0 (+ i 1))) ((or anode (= i n)))
                              (set! id i)
                              (set! anode (hash-table-get nodes id #f))
                              )
                          (if anode
                              (let-values
                                  (((ax ay) (ask anode 'get-position))
                                   ((aw ah) (ask anode 'get-size)))
                                (do ((j 0 (+ j 1))) ((= j n))
                                    (if (not (= j id))
                                        (let
                                            ((bnode (hash-table-get nodes j #f)))
                                          (if bnode
                                              (let-values
                                                  (((bx by) (ask bnode 'get-position))
                                                   ((bw bh) (ask bnode 'get-size))
                                                   )
                                                (if (overlap? ax ay aw (+ ah tab-height tab-height)
                                                              bx by bw (+ bh tab-height tab-height))
                                                    (begin
                                                      (ask bnode 'x-set! (+ ax (+ (* aw 0.55) (* bw 0.55)) node-buffer))
                                                      (layout-aux j)))))))))))))
                  (define (layout id)
                    (if (not allow-overlap)
                        (layout-aux id))
                    (update-bitmap))

                  ; go through and layout all the nodes (cleanup)
                  (define (layout-all)
                    (do ((j 0 (+ j 1))) ((= j max-id))
                        (if snap-to-grid
                            ; snap all to grid
                            (let ((anode (hash-table-get nodes j #f)))
                              (if anode
                                  (let-values (((ax ay) (ask anode 'get-position)))
                                    (ask anode 'x-set! ax)
                                    (ask anode 'y-set! ay))))
                            ; otherwise, if not allowing overlap, remove overlaps
                            (if (not allow-overlap)
                                (layout-aux j))))
                    (update-bitmap))

                  (define (clear)
                    (let
                        ;; no before-clear in ioblocks's callback
                        ((allowed? (if callback (callback 'before-clear #f) #t)))
                      (if allowed?
                          (begin
                            (set! nodes (make-hash-table)) ; 'equal))
                            (set! links (list (list 'ID 'line))) ; also need to clear links - alex
                            (layout #f)
                            ;; no after-clear in ioblocks's callback
                            (if callback
                                (callback 'after-clear #f))))))

                  (define (node-add g-editor data name x y style)
                    (let
                        ;; no before-node-add in ioblocks's callback
                        ((allowed? (if callback (callback 'before-node-add name) #t)))
                      (if allowed?
                          ; store new node
                          (let*
                              ((id max-id)
                               (node (create-node
                                      id
                                      data
                                      name
                                      x y
                                      g-editor style)))
                            
                            ; add to the hash table
                            (hash-table-put! nodes id node)
                            ; increment the maximum id number
                            (set! max-id (+ max-id 1))
                            
                            ; ensure that the node is not outside of screen
                            (let-values
                                (((w h) (ask node 'get-size)))
                              (if (< x 0)
                                  (ask node 'x-set! (* w 0.5)))
                              (if (< y 0)
                                  (ask node 'y-set! (* 0.5 (+ h tab-height tab-height)))))
                            
                            ; do a full layout
                            (layout id)
                            ;; no after-node-add in ioblocks's callback
                            (if callback
                                (callback 'after-node-add node))
                            
                            node)
                          #f)))

                  (define (node-del node)
                    (if (and (not (ask node 'tab-in-connected?))
                             (not (ask node 'tab-out-connected?)))
                        (let
                            ;; no before-node-del in ioblocks's callback
                            ((allowed? (if callback (callback 'before-node-del node) #t)))
                          (if allowed?
                              ; delete the node
                              (let*
                                  ((id (ask node 'get-id)))
                                (hash-table-remove! nodes id)
                                ; ensure that it all of them are unselected
                                (set! selected-node #f)
                                ; do a full layout
                                (layout #f)
                                ;; no after-node-del in ioblocks's callback
                                (if callback
                                    (callback 'after-node-del id)))))))

                  (define (node-get-by-name name)
                    (let ((ok? #f)
                          (n max-id))
                      (do ((i 0 (+ i 1))) ((or ok? (= i n)))
                          (let ((node (hash-table-get nodes i #f)))
                            (if (and node
                                     (equal? (ask node 'get-name) name))
                                (set! ok? node))))
                      ok?))


                  ;; not searching inside custom-node-hash at the moment
                  (define (node-get-by-data data)
                    (let ((ok? #f)
                          (n max-id))
                      (do ((i 0 (+ i 1))) ((or ok? (= i n)))
                          (let ((node (hash-table-get nodes i #f)))
                            (if (and node
                                     (equal? (ask node 'get-data) data))
                                (set! ok? node))))
                      ok?))

                  ; func has two arguments
                  ; key : (an integer number)
                  ; value : a node object
                  (define (node-for-each func)
                    (hash-table-walk
                     nodes
                     func))

                  (define (node-del! node)
                    (let
                        ((ni (ask node 'tab-in-count))
                         (no (ask node 'tab-out-count)))
                      ; go through the input tabs and delete connected lines
                      (do ((i 0 (+ i 1))) ((= i ni))
                          (let ((tab (ask node 'tab-in-ref i)))
                            (if (ask tab 'connected?)
                                ; if lines are conencected to tab then delete the lines
                                (for-each
                                 (lambda (line)
                                   (let
                                       ((src (ask line 'get-source))
                                        (dst (ask line 'get-target)))
                                     (line-del line src dst)))
                                 (ask tab 'get-lines)))))
                      ; go through the output tabs and delete connected lines
                      (do ((i 0 (+ i 1))) ((= i no))
                          (let ((tab (ask node 'tab-out-ref i)))
                            (if (ask tab 'connected?)
                                ; if lines are conencected to tab then delete the lines
                                (for-each
                                 (lambda (line)
                                   (let
                                       ((src (ask line 'get-source))
                                        (dst (ask line 'get-target)))
                                     (line-del line src dst)))
                                 (ask tab 'get-lines)))))
                      ; delete the node itself
                      (node-del node)))

                  ;; add new link/connection to the list of links called links in graph-editor
                  (define (add-to-linklist ID line)
                    (set! links (append links (list(list ID line)))))

                  ; it returns the line object
                  ; the only different between line-add and line-add-with-name-and-ID 
                  ; is that line-add does not add the new line to the links list on graph-editor
                  (define (line-add g-editor src-tab dst-tab in-custom-draw)
                    (let
                        ;; allowed? is also the name of the new line - Note that this is a hack so that ioblocks
                        ;; can pass the name of the line back to line-add after asking user for name
                        ((allowed? (if callback (callback 'before-line-add (list src-tab dst-tab)) #t)))
                      (if allowed?
                          (let
                              ((line (create-line src-tab dst-tab g-editor)))
                            (if (and in-custom-draw
                                     (procedure? in-custom-draw))
                                (ask line 'set-custom-line-draw in-custom-draw))
                            (ask src-tab 'line-add line)
                            (ask dst-tab 'line-add line)
                            (ask line 'show)
                            (if callback
                                (callback 'after-line-add (list src-tab dst-tab line allowed?)))
                            (my-on-paint)
                            line)
                          #f)))

                  ; it returns the line object
                  ;; node ID-str must be a string 
                  ;; TODO: get rid of the annoying (ID line) pair format (use data table or hashtable)
                  ;; TODO: get rid of the first ('ID line) entry in links, it was a hack or some sort for something that was long forgotten 
                  (define (line-add-with-name-and-ID g-editor in-name ID-str src-tab dst-tab in-custom-draw)
                    
                    (let
                        ((allowed? (if callback (callback 'before-line-add (list src-tab dst-tab)) #t)))
                      ;; there is no before-line-add in graph-callback of ioblocks
                      (if allowed?
                          (let*
                              ((line (create-line src-tab dst-tab g-editor)))
                            (if (and in-custom-draw
                                     (procedure? in-custom-draw))
                                (ask line 'set-custom-line-draw in-custom-draw))
                            (set! links (append links (list (list ID-str line))))
                            (ask line 'set-ID ID-str)
                            (ask src-tab 'line-add line)
                            (ask dst-tab 'line-add line)
                            (ask line 'set-data in-name)
                            (ask line 'show)
                            
                            (if callback
                                (callback 'after-line-add (list src-tab dst-tab line allowed?)))
                            (my-on-paint)
                            line)
                          #f)))

                  ;; need to skip the first (ID line) entry in links, why is it there in the first place?
                  ; note: ID is a string, so shouldn't be converted to a number
                  (define (get-line-by-ID ID)
                    (define (search link ID)
                      (if (null-list? link)
                          #f
                          (let ((curr-link (car link)))
                            (if (equal? (car curr-link) ID)
                                (cadr curr-link)
                                (search (cdr link) ID)))))
                    (search (cdr links) ID))

                  (define (get-line-by-name name)
                    (let ((line-to-return #f))
                      (map (lambda (id-line-pair)
                             (let ((line (cadr id-line-pair)))
                               (if (equal? (ask line 'get-data) name)
                                   (set! line-to-return line))))
                           (cdr links))
                      line-to-return))

                  ; rename a line - alex
                  (define (line-rename line newname)
                    (ask line 'set-data newname)
                    (my-on-paint))

                  ; change a line's destination
                  (define (line-change-dest line old-dst-tab new-dst-tab lineID)
                    (ask old-dst-tab 'line-del line)
                    (ask new-dst-tab 'line-add line)
                    (ask line 'set-target! new-dst-tab)
                    (my-on-paint))

                  ;; set selected lines
                  (define (set-selected-line! line)
                    ;; if there is a previously selected line 
                    ;; deselect it and paint it accordingly
                    (if selected-line
                        (begin
                          (ask selected-line 'deselect) ;;ask line to deselect itself to paint it black
                          (ask selected-line 'show))) ; paint it

                    ;; set to a new line or #f 
                    (set! selected-line line)

                    ;; only do this when selected-line is not #f
                    (if selected-line
                        (begin
                          (ask selected-line 'select) ;;ask line to select itself to paint it red
                          (ask selected-line 'show))) ; paint it
                    )

                  (define (line-del line src-tab dst-tab #!optional ID) ; added args to allow passing in of ID when deleting line
;                    (display "line-del in graph-editor")(newline)
;                    (display "line ")(display line)(newline)
                    (let
                        ;; no before-line-del in ioblocks's callback
                        ((allowed? (if callback (callback 'before-line-del (list src-tab dst-tab)) #t)))
                      (if allowed?
                          (begin
                            (ask src-tab 'line-del line)
                            (ask dst-tab 'line-del line)
                            ;; need to remove from links list as well - alex
                            ;(display "links before del ")(display links)(newline) 
                            (display "ID ")(display ID)(newline)
                            (display "line name ")(display (ask line 'get-data))(newline)
                            (display (map (lambda (link)
                                            (car link)) links))(newline)
                            
                            ;(display "testing name ")(display (ask (cadr (car links)) 'get-data))(newline)
                            (display (map (lambda (link)
                                            (ask (cadr link) 'get-data)) (cdr links)))(newline)
                                   
                            (if ID
                                (set! links (remove (lambda (a) (equal? (car a) ID)) links)))
                            (display (map (lambda (link)
                                            (car link)) links))(newline)
                            
                            ;(display "links after aft ")(display links)(newline)
                            (ask line 'hide)
                            ;; no after-line-del in ioblocks's callback
                            (if callback
                                (callback 'after-line-del #f))
                            (my-on-paint))))
                    )
                  
                  ;; create a subclass of node without putting it in the node hash
;                  (define (custom-wrap-anode editor data name x y style superclass-wrapper)
;                    ;(node-add self data name x y style)
;                    ;; id is name for now
;                    ;                    (let* ((node (create-node name data name x y editor 'style-notapplicable))
;                    ;                           (new-custom (superclass-wrapper node)))
;                    
;                    (let ((node (create-node max-id data name x y editor style)))
;                      (let ((new-custom (superclass-wrapper node)))
;                      ;; return the custom-node
;                      new-custom
;                    )))
                  
                  (define (custom-node-add editor data name x y style subclass-wrapper) ;; obj is the ac

                    ;; wrap the node
                    (let ((node (create-node max-id data name x y editor style)))
                      (let ((custom-node (subclass-wrapper node)))

                        ;; put into nodes hash
                        (hash-table-put! nodes max-id custom-node)
                        (set! max-id (+ max-id 1))

                        ;; ensure that the node is not outside of screen
                        (let-values
                            (((w h) (ask custom-node 'get-size)))
                          (if (< x 0)
                              (ask custom-node 'x-set! (* w 0.5)))
                          (if (< y 0)
                              (ask custom-node 'y-set! (* 0.5 (+ h tab-height tab-height)))))
                        ))
                    )
                  
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; drawing the graph
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  
                  ; get the real width of the network
                  (define (get-real-width)
                    (let
                        ((width 0))
                      (hash-table-walk
                       nodes
                       (lambda (idx node)
                         (let-values
                             (((x y) (ask node 'get-position))
                              ((w h) (ask node 'get-size)))
                           (set! width (max width (+ x (* w 0.5)))))))
                      (inexact->exact (round width))))

                  (define (get-real-height)
                    (let
                        ((height 0))
                      (hash-table-walk
                       nodes
                       (lambda (idx node)
                         (let-values
                             (((x y) (ask node 'get-position))
                              ((w h) (ask node 'get-size)))
                           (set! height (max height (+ y (* (+ h tab-height tab-height) 0.5)))))))
                      (inexact->exact (round height))))

                  ; update the bitmap size after layout change
                  (define (update-bitmap)
                    ;(display "died here ?")(newline)
                    ;(display "get parent glcanvas ")(display (get-parent glcanvas))(newline)
                    (let-values
                        (((viewport-width viewport-height) (get-component-size (get-parent glcanvas))))
                      ; calculate size of graph editor (can't be smaller than 1)
                      (let ((buffer-width (get-buffer-width glcanvas))
                            (buffer-height (get-buffer-height glcanvas))
                            (new-width (max viewport-width 1 (graph->canvas (+ 10 (get-real-width)))))
                            (new-height (max viewport-height 1 (graph->canvas (+ 10 (get-real-height))))))
;;                        (format #t "update-bitmap: buffer-w=~a, buffer-h=~a, viewport-w=~a, viewport-h=~a, new-width=~a, new-height=~a~%~!"
;;                                buffer-width buffer-height viewport-width viewport-height new-width new-height)
                        (if (not (and (= buffer-width new-width)
                                      (= buffer-height new-height)))
                            (begin
                              (set! width new-width)
                              (set! height new-height)

                              ; set size of actual canvas
                              (set-component-preferred-size glcanvas width height)

                              ; recreate buffer
                              (create-buffer glcanvas)))))

                        ; and repaint
                        (component-revalidate glcanvas)
                        (my-on-paint))

                  ; translating from graph coordinates to canvas coordinates
                  (define (graph->canvas in-value)
                    (round (* in-value zoomfactor)))
                  
                  ; translating from canvas coordinates to graph coordinated
                  (define (canvas->graph in-value)
                    (round (/ in-value zoomfactor)))
                  
                  ; is the canvas zoomed?
                  (define (zoomed?)
                    (not (= 1.0 zoomfactor)))
                  
                  ;;
                  ;; removed scrollbar stuff - alex

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;;; on-size
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  ; canvas was resized
                  (define (on-size . args)
                    ;; We just update the bitmap...
                    ;(format #t "on-size~%~!")
                    (update-bitmap))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;;; my-on-paint
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  ; refresh the bitmap onto canvas
                  (define (my-refresh)
                    ;(format #t "my-refresh~%~!")
                    (update-canvas glcanvas))

                  ; store previous transform
                  (define oldtx #f)
                  
                  ; begin paint: apply zoom transform
                  (define (begin-paint)
                    (let ((tx (make-affinetransform)))
                      ; store old transform
                      (set! oldtx (get-affinetransform (get-buffer glcanvas)))
                      ; set scale transform
                      (set-affinetransform-scale tx zoomfactor zoomfactor)
                      (compose-transform (get-buffer glcanvas) tx)))
                  
                  ; end paint: restore stored transform
                  (define (end-paint)
                    ; reset the transform
                    (if oldtx
                        (begin
                          (set-affinetransform (get-buffer glcanvas) oldtx)
                          (set! oldtx #f))))

                    ; redraw everything and then refresh bitmap to canvas
                    (define (my-on-paint)
                      (if allow-repaint
                          (begin
                            ;(format #t "my-on-paint~%~!")
                            ; clear the buffer
                            (clear-buffer glcanvas)

                            ; begin painting
                            (begin-paint)

                            ; if theres a background to draw 
                            ; draw it first
                            (if bgdraw
                                (bgdraw))

                            (if (not (equal? (hash-table-size custom-nodes-hash) 0))
                                (begin
                                  (hash-table-walk
                                   custom-nodes-hash
                                   (lambda (key node)
                                     ;(display "hash walk in ge node: ")(display node)(newline)
                                     (if (equal? node selected-node)
                                         (ask node 'show #t)
                                         (ask node 'show #f)))
                                   )))

                            ; draw all nodes with tabs and lines
                            ;; newest node (ie largest id) are drawn last 
                            (do ((i 0 (+ i 1))) ((= i max-id))
                                (let ((node (hash-table-get nodes i #f)))
                                  (if node
                                      (if (equal? node selected-node)
                                          (ask node 'show #t)
                                          (ask node 'show #f)))))
                            
                            ; end painting
                            (end-paint)

                            ; draw the bitmap
                            (my-refresh))))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;;; on-char
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  ; deleted - alex

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;;; on-event
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  ;; not just clicked, any type of mouse event can use 
                  ;; this to activate the on-mouse callback in the objects
                  (define (find-clicked type x y)
                    (let
                        ((obj #f)
                         ;(ioblocks? (is-ioblocks-editor? self-reference))
                         ) ;;use the first node to determine whether this is in ioblocks
                      
                      (define (select-node name node)
                        (let ((selected (ask node 'on-mouse type x y)))
                          (if selected
                              (let ((selected-type (ask selected 'obj-type)))
                                ;; select the first object we come across (also the most recent one)
                                (if (not obj)
                                    (cond
                                     ((equal? selected-type 'node)
                                      (set! obj selected))
                                     ((equal? selected-type 'tab)
                                      (set! obj selected))
                                     ;;                                    ((and (equal? selected-type 'line)
                                     ;;                                          ioblocks?)
                                     ;;                                     (set! obj selected))
                                     ((equal? selected-type 'line)
                                      ;; issit ok for lines from other app to be selectable?
                                      (set! obj selected))
                                     ((equal? selected-type 'place-node)
                                      (set! obj selected))
                                 ))))))
                      
                      ; go through all nodes
                      (hash-table-walk
                       nodes
                       select-node)
                      
                      ;; hash walk custom-nodes-hash as well
                      (hash-table-walk
                       custom-nodes-hash
                       select-node)
                      
                      obj))
                  
                  ;; mouse-handler-mould is the object that keeps track 
                  ;; and mould/change the behavior of the mouse-handler on
                  ;; different obj-type which can be 'node 'tab or 'line
                  (define (mouse-handler-mould)
                    (let ((internal-clicked #f)
                          (clicked-type #f)
                          (prev-x #f)
                          (prev-y #f)
                          (prev-mouse-event #f)

                          ;; keeps track of the assignment of mouse event and function
                          ;; key is list of combination (look at comment below)
                          ;; object is a function
                          (func-hashtable (make-hash-table))

                          ;; keeps track of which combination of 
                          ;; mouse event has a function assigned
                          ;; an example of a combination ( 'node 'left-up #t ) 
                          (mouse-func-slot-set (list))

                          (this-obj (new-object))
                          )
                     
                      ;; the actual mouse-event listener callback to attach to the 
                      ;; canvas of graph-editor's canvas
                      ;; it is attached to graph-editor by default
                      (define (mouse-handler event)
                        (let ((mouse-event-type (get-mouseevent-type event))
                              (x    (canvas->graph (+ (get-mouseevent-x event) x-offset)))
                              (y    (canvas->graph (+ (get-mouseevent-y event) y-offset)))
                              (click-count (get-mouseevent-click-count event))
                              (find-clicked-trigger-list (list 'left-down 'right-down 'motion)))
                          
                          ;; only check what is clicked when a left 
                          ;; or right down is registered
                          (if (member mouse-event-type find-clicked-trigger-list)
                              (begin
                                ;; set internal-clicked so we know what is clicked
                                (set! internal-clicked (find-clicked mouse-event-type x y))
                                        ;(display "internal-clicked set to : ")(display internal-clicked)(newline)

                                (if internal-clicked
                                    (set! clicked-type (ask internal-clicked 'obj-type)) ; can be 'node 'tab 'line
                                    (set! clicked-type #f))
                                ))
                          
                          ;; actual mouse event handling
                          ;; retrieve and carry out mouse-event behavior if it's existent
                          (if (member (list clicked-type mouse-event-type (if internal-clicked #t #f)) mouse-func-slot-set)
                              (begin ;(display "carrying out mouse behavior")(newline)
                                ((hash-table-get func-hashtable (list clicked-type mouse-event-type (if internal-clicked #t #f)) ) 
                                 x y click-count internal-clicked)) ; added click-count - alex
                              )
                          
                          ; used to tell the difference between a drag and a clicked event
                          ; drag event has a drag right before a left-up
                          (set! prev-mouse-event mouse-event-type)

                          ));; end of mouse handler event
                      
                    
                      ;; Graph editor messages
                      (obj-put this-obj 'set-event-func
                          (lambda (self obj-type event-slot clicked func)
                            (if (and func-hashtable
                                     (or (symbol? obj-type)
                                         (boolean? obj-type))
                                     (symbol? event-slot)
                                     (boolean? clicked)
                                     (procedure? func))
                                (begin 
                                  (let ((combi-list (list obj-type event-slot clicked)))
                                    (hash-table-put! func-hashtable combi-list func)
                                    (if (not (member combi-list mouse-func-slot-set))
                                        (set! mouse-func-slot-set (append mouse-func-slot-set (list combi-list)))))
                                  )
                                (begin
                                  (display "error in adding mouse-event-func to mouse-handler-mould")(newline))
                                  )))

                      (obj-put this-obj 'get-mouse-handler
                          (lambda (self) mouse-handler))
                      (obj-put this-obj 'get-prev-x
                          (lambda (self) prev-x))
                      (obj-put this-obj 'get-prev-y
                          (lambda (self) prev-y))
                      (obj-put this-obj 'set-prev-x
                          (lambda (self new-x)
                            (set! prev-x new-x)))
                      (obj-put this-obj 'set-prev-y
                          (lambda (self new-y)
                            (set! prev-y new-y)))
                      (obj-put this-obj 'reset-clicked
                          (lambda (self)
                            (set! internal-clicked #f)
                            (set! clicked-type #f)))
                      (obj-put this-obj 'get-prev-mouse-event
                          (lambda (self)
                            prev-mouse-event))

                      this-obj));end of mouse-handler-mould
                  
                  ;; maybe this should be called inside the let
                  (set! mhm (mouse-handler-mould))

                  ;; public methods
                  (obj-put this-obj 'init
                           (lambda (self) (init self)))
                  (obj-put this-obj 'get-selected
                           (lambda (self)
                             (get-selected)))
                  (obj-put this-obj 'set-selected!
                           (lambda (self ID)
                             (set-selected! ID)))
                  (obj-put this-obj 'set-selected-node
                           (lambda (self node)
                             (begin
                               (begin-paint)
                               (if node
                                   (ask node 'show #t))
                               (end-paint)
                               (set! selected-node node))))
                  (obj-put this-obj 'scroll-to-node
                           (lambda (self ID)
                             (scroll-to-node ID)))
                  (obj-put this-obj 'layout 
                           (lambda (self id)
                             (layout id)))
                  (obj-put this-obj 'layout-all
                           (lambda (self)
                             (layout-all)))
                  (obj-put this-obj 'clear
                           (lambda (self) (clear)))
                  (obj-put this-obj 'node-add
                           (lambda (self data name x y style)
                             (node-add self data name x y style)))
                  (obj-put this-obj 'node-del
                           (lambda (self in-node)
                             (node-del in-node)))
                  (obj-put this-obj 'node-get-by-name
                           (lambda (self in-name)
                             (node-get-by-name in-name)))
                  (obj-put this-obj 'node-get-by-data
                           (lambda (self in-data)
                             (node-get-by-data in-data)))
                  (obj-put this-obj 'node-for-each
                           (lambda (self func)
                             (node-for-each func)))
                  (obj-put this-obj 'node-del!
                           (lambda (self in-node)
                             (node-del! in-node)))
                  (obj-put this-obj 'line-add
                           (lambda (self src-tab dst-tab in-custom-draw)
                             (line-add self src-tab dst-tab in-custom-draw)))
                  (obj-put this-obj 'line-add-with-name-and-ID
                           (lambda (self in-name ID src-tab dst-tab in-custom-draw)
                             (line-add-with-name-and-ID self in-name ID src-tab dst-tab in-custom-draw)))
                  (obj-put this-obj 'get-line-by-ID
                           (lambda (self ID)
                             (get-line-by-ID ID)))
                  (obj-put this-obj 'get-line-by-name
                           (lambda (self name)
                             (get-line-by-name name)))
                  (obj-put this-obj 'line-rename
                           (lambda (self line newname)
                             (line-rename line newname)))
                  (obj-put this-obj 'line-change-dest
                           (lambda (self line old-dst-tab new-dst-tab lineID)
                             (line-change-dest line old-dst-tab new-dst-tab lineID)))
                  (obj-put this-obj 'line-del
                           (lambda (self line src-tab dst-tab #!rest args)
                             (apply line-del (append (list line src-tab dst-tab) args))))
                  (obj-put this-obj 'set-selected-line!
                           (lambda (self line)
                             (set-selected-line! line)))
                  (obj-put this-obj 'get-selected-line
                           (lambda (self)
                             selected-line))
                  (obj-put this-obj 'begin-paint
                           (lambda (self)
                             (begin-paint)))
                  (obj-put this-obj 'end-paint
                           (lambda (self)
                             (end-paint)))
                  (obj-put this-obj 'my-on-paint
                           (lambda (self)
                             (my-on-paint)))
                  (obj-put this-obj 'get-buffer
                           (lambda (self)
                             (get-buffer glcanvas)))
                  (obj-put this-obj 'get-canvas
                           (lambda (self)
                             glcanvas))
                  (obj-put this-obj 'my-refresh
                           (lambda (self)
                             (my-refresh)))
                  (obj-put this-obj 'add-to-linklist
                           (lambda (self ID line)
                             (add-to-linklist ID line)))
                  (obj-put this-obj 'get-mouse-overed
                           (lambda (self)
                             mouse-overed))
                  (obj-put this-obj 'set-mouse-overed ;;mouse overed is used to update status bar
                           (lambda (self mouseovered-name)
                             (set! mouse-overed mouseovered-name)))
                  (obj-put this-obj 'get-application-symbol
                           (lambda (self)
                             application-symbol))
                  (obj-put this-obj 'get-links
                           (lambda (self)
                             links))
                  (obj-put this-obj 'on-size
                           (lambda (self)
                             (on-size)))
                  (obj-put this-obj 'set-bgdraw
                           (lambda (self drawfunc)
                             (set! bgdraw drawfunc)))
                  (obj-put this-obj 'set-zoomfactor!
                           (lambda (self in-zoomfactor)
                             (set! zoomfactor in-zoomfactor)))
                  (obj-put this-obj 'get-zoomfactor
                           (lambda (self)
                             zoomfactor))
                  (obj-put this-obj 'graph->canvas
                           (lambda (self in-value)
                             (graph->canvas in-value)))
                  (obj-put this-obj 'canvas->graph
                           (lambda (self in-value)
                             (canvas->graph in-value)))
                  (obj-put this-obj 'zoomed?
                           (lambda (self)
                             (zoomed?)))
                  (obj-put this-obj 'set-allow-overlap!
                           (lambda (self in-flag)
                             (set! allow-overlap in-flag)))
                  (obj-put this-obj 'allow-overlap?
                           (lambda (self)
                             allow-overlap))
                  (obj-put this-obj 'set-snap-to-grid!
                           (lambda (self in-flag)
                             (set! snap-to-grid in-flag)))
                  (obj-put this-obj 'snap-to-grid?
                           (lambda (self)
                             snap-to-grid))
                  (obj-put this-obj 'set-allow-repaint!
                           (lambda (self in-flag)
                             (set! allow-repaint in-flag)))
                  (obj-put this-obj 'get-mhm
                           (lambda (self)
                             mhm))
                  (obj-put this-obj 'find-clicked
                           (lambda (self type x y)
                             (find-clicked type x y)))
                  (obj-put this-obj 'get-callback
                           (lambda (self)
                             (if callback
                                 callback
                                 (begin 
                                   (display "callback of graph-editor does not exist")(newline)
                                   #f))))
;                  (obj-put this-obj 'custom-wrap-anode
;                           (lambda (self data name x y style subclass-wrapper)
;                             (custom-wrap-anode self data name x y style subclass-wrapper)
;                             ))
                  
                  ;; add subclass of the node to the nodes hash
                  ;; subclass-wrapper is an object/function that takes a node as its only argument
                  (obj-put this-obj 'custom-node-add custom-node-add)
                           
                          ; set text height
                  (obj-put this-obj 'set-text-height!
                           (lambda (self in-text-height)
                             (set! text-height in-text-height)))
                  this-obj))
;        ); end of module