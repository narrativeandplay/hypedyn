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
               scroll-set-vertical-unit-increment scroll-set-horizontal-unit-increment)

;;
;; scrollpane
;; 

(define-alias vert-sb-always <javax.swing.ScrollPaneConstants>:VERTICAL_SCROLLBAR_ALWAYS)  
(define-alias vert-sb-needed <javax.swing.ScrollPaneConstants>:VERTICAL_SCROLLBAR_AS_NEEDED)  
(define-alias vert-sb-never <javax.swing.ScrollPaneConstants>:VERTICAL_SCROLLBAR_NEVER) 

(define-alias hori-sb-always <javax.swing.ScrollPaneConstants>:HORIZONTAL_SCROLLBAR_ALWAYS)
(define-alias hori-sb-needed <javax.swing.ScrollPaneConstants>:HORIZONTAL_SCROLLBAR_AS_NEEDED)  
(define-alias hori-sb-never <javax.swing.ScrollPaneConstants>:HORIZONTAL_SCROLLBAR_NEVER) 



;(define vert-sb-always <javax.swing.JScrollPane>:VERTICAL_SCROLLBAR_ALWAYS)  
;(define vert-sb-needed <javax.swing.JScrollPane>:VERTICAL_SCROLLBAR_AS_NEEDED)  
;(define vert-sb-never <javax.swing.JScrollPane>:VERTICAL_SCROLLBAR_NEVER) 

;(define hori-sb-always <javax.swing.JScrollPane>:HORIZONTAL_SCROLLBAR_ALWAYS)
;(define hori-sb-needed <javax.swing.JScrollPane>:HORIZONTAL_SCROLLBAR_AS_NEEDED)  
;(define hori-sb-never <javax.swing.JScrollPane>:HORIZONTAL_SCROLLBAR_NEVER) 
;make a scrollpane
(define (make-scrollpane in-component :: <javax.swing.JComponent>)
  (<javax.swing.JScrollPane> in-component))

;; make a scrollpane with policy
(define (make-scrollpane-with-policy in-component :: <javax.swing.JComponent> 
                                     vp-sym :: <symbol> 
                                     hp-sym :: <symbol> )
  (define vpolicy
    (case vp-sym
      ((always) vert-sb-always)
      ((needed) vert-sb-needed)
      ((never) vert-sb-never)
      (else (display "Error unknown policy ")(display vp-sym)
            vert-sb-needed)))
  
   (define hpolicy
    (case hp-sym
      ((always) hori-sb-always)
      ((needed) hori-sb-needed)
      ((never) hori-sb-never)
      (else (display "Error unknown policy ")(display hp-sym)
            hori-sb-needed)))
  (display "vpolicy")(display vert-sb-needed)(newline)
   (<javax.swing.JScrollPane> in-component vert-sb-needed hori-sb-needed)
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

