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

(module-export make-scrollpane make-scrollpane-with-policy
               scroll-rect-to-visible
               scroll-set-vertical-unit-increment scroll-set-horizontal-unit-increment
               scroll-viewport-width scroll-viewport-height  
               scroll-get-scrollbar
               )

;;
;; scrollpane
;; 

(define-alias vert-sb-always <javax.swing.ScrollPaneConstants>:VERTICAL_SCROLLBAR_ALWAYS)  
(define-alias vert-sb-needed <javax.swing.ScrollPaneConstants>:VERTICAL_SCROLLBAR_AS_NEEDED)  
(define-alias vert-sb-never <javax.swing.ScrollPaneConstants>:VERTICAL_SCROLLBAR_NEVER) 

(define-alias hori-sb-always <javax.swing.ScrollPaneConstants>:HORIZONTAL_SCROLLBAR_ALWAYS)
(define-alias hori-sb-needed <javax.swing.ScrollPaneConstants>:HORIZONTAL_SCROLLBAR_AS_NEEDED)  
(define-alias hori-sb-never <javax.swing.ScrollPaneConstants>:HORIZONTAL_SCROLLBAR_NEVER) 

;make a scrollpane
(define (make-scrollpane in-component :: <javax.swing.JComponent>)
  (<javax.swing.JScrollPane> in-component))

;; make a scrollpane with policy
(define (make-scrollpane-with-policy in-component :: <javax.swing.JComponent> 
                                     vp-sym :: <symbol> 
                                     hp-sym :: <symbol> )
;  (define vpolicy
;    (case vp-sym
;      ((always) vert-sb-always)
;      ((needed) vert-sb-needed)
;      ((never) vert-sb-never)
;      (else (display "Error unknown policy ")(display vp-sym)
;            vert-sb-needed)))
;  
;   (define hpolicy
;    (case hp-sym
;      ((always) hori-sb-always)
;      ((needed) hori-sb-needed)
;      ((never) hori-sb-never)
;      (else (display "Error unknown policy ")(display hp-sym)
;            hori-sb-needed)))
;  
;  (display "hori sb needed ")(display hori-sb-needed)(newline)
;  (display "vert sb needed ")(display vert-sb-needed)(newline)
;  
;  (display "hori sb always ")(display hori-sb-always)(newline)
;  (display "vert sb always ")(display vert-sb-always)(newline)
;  
;  (display "hori sb never ")(display hori-sb-never)(newline)
;  (display "vert sb never ")(display vert-sb-never)(newline)
  
  ;;(display "vpolicy ")(display vpolicy)(newline)
  ;;(display "hpolicy ")(display hpolicy)(newline)
  
   (<javax.swing.JScrollPane> in-component 
                              (case vp-sym
                                ((always) vert-sb-always)
                                ((needed) vert-sb-needed)
                                ((never) vert-sb-never)
                                (else (display "Error unknown policy ")(display vp-sym)
                                      vert-sb-needed))
                              (case hp-sym
                                ((always) hori-sb-always)
                                ((needed) hori-sb-needed)
                                ((never) hori-sb-never)
                                (else (display "Error unknown policy ")(display hp-sym)
                                      hori-sb-needed))
                              )
  )

; scroll containing scrollpane, if any, to given rect; must be JComponent
(define (scroll-rect-to-visible component :: <javax.swing.JComponent> rect :: <java.awt.Rectangle>)
  (invoke component 'scrollRectToVisible rect))
               
; set scrollbar vertical unit increment
(define (scroll-set-vertical-unit-increment in-scrollpane :: <javax.swing.JScrollPane> inc :: <int>)
  (invoke (as <javax.swing.JScrollBar> (invoke in-scrollpane 'getVerticalScrollBar)) 'setUnitIncrement inc))

; set scrollbar horizontal unit increment
(define (scroll-set-horizontal-unit-increment in-scrollpane :: <javax.swing.JScrollPane> inc :: <int>)
  (invoke (as <javax.swing.JScrollBar> (invoke in-scrollpane 'getHorizontalScrollBar)) 'setUnitIncrement inc))

;;getViewport
(define (scroll-viewport-width scrollpane)
  (let ((vp-dim (invoke (invoke scrollpane 'getViewport) 'get-view-size)))
    (invoke vp-dim 'get-width)))

(define (scroll-viewport-height scrollpane)
  (let ((vp-dim (invoke (invoke scrollpane 'getViewport) 'get-view-size)))
    (invoke vp-dim 'get-height)))

;; get scrollbar
(define (scroll-get-scrollbar sp vert-hori-sym)
  (case vert-hori-sym
    ((vert) (invoke sp 'get-vertical-scroll-bar))
    ((hori) (invoke sp 'get-horizontal-scroll-bar))))
