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

; export
(module-export add-component remove-component
               set-component-size set-component-preferred-size set-component-minimum-size set-component-maximum-size
               set-component-non-resizable-size
               set-opaque set-border set-background-color get-background-color 
               get-component-size set-component-visible get-component-visible set-component-enabled set-location
               get-width get-height
               get-bounds request-focus transfer-focus remove-self-from-parent
               
               get-location-on-screen
               get-parent
               component-update set-align-x set-align-y
               black-border bevel-in-border bevel-out-border
               component-revalidate
               get-component-root-pane
               )

;;
;; Components
;; 

;; predefined borders // to use do (set-border <component> <border>)
(define black-border (invoke-static <javax.swing.BorderFactory> 'createLineBorder (<java.awt.Color> 0 0 0 0)))
(define bevel-in-border (invoke-static <javax.swing.BorderFactory> 'createLoweredBevelBorder))
(define bevel-out-border (invoke-static <javax.swing.BorderFactory> 'createRaisedBevelBorder))

;;QUESTION: the syntax (invoke container 'add component layout) seems wrong
;; add (Component comp, int index)  this is the matching layout i found and it doesn't seem right
;; Answer myself: now i get it, the index correspond to the position when we're in the border layout
;; the index have no correspondence if it is in another layout

; add a component to a container
(define (add-component container :: <java.awt.Container>
                       component :: <java.awt.Component> . args)
  (if (pair? args)
      (let* ((align (car args)))
        (invoke (as <java.awt.Container> container)
                'add component
                (cond
                 ((eq? align 'border-north) <java.awt.BorderLayout>:NORTH)
                 ((eq? align 'border-south) <java.awt.BorderLayout>:SOUTH)
                 ((eq? align 'border-east) <java.awt.BorderLayout>:EAST)
                 ((eq? align 'border-west) <java.awt.BorderLayout>:WEST)
                 ((eq? align 'border-center) <java.awt.BorderLayout>:CENTER)
                 ((eq? align 'box-vertical) <javax.swing.BoxLayout>:Y_AXIS)
                 ((eq? align 'box-horizontal) <javax.swing.BoxLayout>:X_AXIS)
                 ((number? align) align)
                 (else ;(error "add-component: unknown alignment")
                  (display "add-component: unknown alignment")(newline))
                 )
                ))
      (invoke (as <java.awt.Container> container)
              'add component)))

;; adding component to jframe
;frame.getContentPane().add(child);
;(define (add-component-to-window jframe :: <javax.swing.JFrame>
;                                 component :: <java.awt.Component>)
;  (invoke (invoke jframe 'get-content-pane) 'add component))


;remove component
(define (remove-component container :: <java.awt.Container>
                          component :: <java.awt.Component>)
  (invoke (as <java.awt.Container> container) 'remove component))

; set component size
(define (set-component-size component :: <java.awt.Component>
                            width :: <int>
                            height :: <int>)
  (invoke (as <java.awt.Component> component) 'setSize width height))

; set component preferred size - note: are we using all JComponents?
(define (set-component-preferred-size component :: <javax.swing.JComponent>
                                      width :: <int>
                                      height :: <int>)
  (invoke (as <javax.swing.JComponent> component) 'setPreferredSize
          (<java.awt.Dimension> width height)))

; doesn't this duplicate the above function? only used in 
; langs/vehicles/gui/unused/color-dialog.scm, so removed. - alex
;;(define (set-preferred-size comp :: <javax.swing.JComponent>
;;                            width height)
;;  (invoke comp 'setPreferredSize
;;          (<java.awt.Dimension> width height)))

; set component minimum size
(define (set-component-minimum-size component :: <java.awt.Component>
                                    width :: <int>
                                    height :: <int>)
  (invoke (as <java.awt.Component> component) 'setMinimumSize
          (<java.awt.Dimension> width height)))

; set component maximum size
(define (set-component-maximum-size component :: <java.awt.Component>
                                    width :: <int>
                                    height :: <int>)
  (invoke (as <java.awt.Component> component) 'setMaximumSize
          (<java.awt.Dimension> width height)))

; set component non resizable by setting minimum maximum and preferred size of component
(define (set-component-non-resizable-size component :: <java.awt.Component>
                                          width :: <int>
                                          height :: <int>)
  (set-component-minimum-size component width height)
  (set-component-maximum-size component width height)
  (set-component-preferred-size component width height))

; set component's opaque flag
(define (set-opaque comp :: <javax.swing.JComponent> flag)
  (invoke comp 'setOpaque flag))

; set a border of a component 
(define (set-border component :: <javax.swing.JComponent>
                    border)
  (invoke component 'setBorder border))

; set background color of any component
(define (set-background-color component :: <javax.swing.JComponent>
                              color :: <java.awt.Color>)
  (invoke component 'setBackground color))

; get background color of a component
(define (get-background-color component :: <javax.swing.JComponent>)
  (invoke component 'getBackground))

; get component's current size
(define (get-component-size component :: <java.awt.Component>)

  (let ((the-size :: <java.awt.Dimension>
                  (invoke (as <java.awt.Component> component) 'getSize
                          #!null)))
    (values (invoke the-size 'getWidth)
            (invoke the-size 'getHeight))))

; get width
(define (get-width component :: <java.awt.Component>)
  (invoke component 'getWidth))

; get height
(define (get-height component :: <java.awt.Component>)
  (invoke component 'getHeight))

; set component's visible state
(define (set-component-visible component :: <java.awt.Component>
                               in-flag :: <boolean>)
  (invoke (as <java.awt.Component> component) 'setVisible in-flag))

; get component's visible state
(define (get-component-visible component :: <java.awt.Component>)
  (invoke (as <java.awt.Component> component) 'isVisible))

; enable/disable a component
(define (set-component-enabled in-item :: <javax.swing.JComponent> enable)
  (invoke in-item 'setEnabled enable))

(define (set-location comp :: <java.awt.Component> loc-x loc-y)
  (invoke comp 'setLocation loc-x loc-y)
  )

;get bound size of a component
(define (get-bounds comp :: <java.awt.Component>)
  (invoke comp 'getBounds))


(define (request-focus component :: <java.awt.Component>)
  (invoke component 'requestFocus))

(define (transfer-focus component :: <java.awt.Component>)
  (invoke component 'transferFocus))

; remove a component from its parent (in other words destroy itself)
(define (remove-self-from-parent self)
  (let ((parent (get-parent self)))
    (remove-component parent self)))

;; extra stuff,  not sure if still used
;; 
; get parent container of component
(define (get-parent component :: <javax.swing.JComponent>)
  (invoke component 'getParent))

; set X position
(define (set-posX component :: <java.awt.Component>
                  x :: <int>)
  (let ((y (invoke (invoke component 'getLocation) 'getY)))
    (invoke component 'setLocation x y)))

; set Y position
(define (set-posY component :: <java.awt.Component>
                  y :: <int>)
  (let ((x (invoke (invoke component 'getLocation) 'getX)))
    (invoke component 'setLocation x y)))

; set height
(define (set-width component :: <java.awt.Component>
                   width :: <int>)
  (let ((height (invoke component 'getHeight)))
    (set-component-size component width height)))

(define (get-location-on-screen comp :: <java.awt.Component>)
  (let (( loc (invoke comp 'getLocationOnScreen)))
    (vector (invoke (as <java.awt.Point> loc) 'getX) (invoke (as <java.awt.Point> loc) 'getY))))

; set width
(define (set-height component :: <java.awt.Component>
                    height :: <int>)
  (let ((width (invoke component 'getWidth)))
    (set-component-size component width height)))

;get minimum size
(define (get-minimum-size component :: <java.awt.Component>)
  (invoke component 'getMinimumSize))

; set min height
(define (set-min-height component :: <javax.swing.JComponent>
                        height :: <int>)
  (let ((width (invoke (invoke component 'getMinimumSize) 'getWidth)))
    (invoke component 'setMinimumSize (<java.awt.Dimension> width height))))

; set min width
(define (set-min-width component :: <java.awt.Component>
                       width :: <int>)
  (let ((height (invoke (invoke component 'getMinimumSize) 'getHeight)))
    (invoke component 'setMinimumSize (<java.awt.Dimension> width height))))

; add me (a component) to the parent component 
(define (parent-add-me parent :: <java.awt.Container>
                       me :: <java.awt.Component>)
  (invoke parent 'add me))

; set a component visible
(define (show component :: <java.awt.Component>)
  (set-component-visible component #t))

; set a component invisible
(define (hide component :: <java.awt.Component>)
  (set-component-visible component #f))

; set alignment x  attribute
(define (set-align-x component :: <javax.swing.JComponent> xalign)
  (cond ((equal? xalign 'left) (invoke component 'setAlignmentX <java.awt.Component>:LEFT_ALIGNMENT))
        ((equal? xalign 'center) (invoke component 'setAlignmentX <java.awt.Component>:CENTER_ALIGNMENT))
        ((equal? xalign 'right) (invoke component 'setAlignmentX <java.awt.Component>:RIGHT_ALIGNMENT))))

; set alignment y attribute
(define (set-align-y component :: <javax.swing.JComponent> yalign)
  (cond ((equal? yalign 'top) (invoke component 'setAlignmentY <java.awt.Component>:TOP_ALIGNMENT))
        ((equal? yalign 'center) (invoke component 'setAlignmentY <java.awt.Component>:CENTER_ALIGNMENT))
        ((equal? yalign 'bottom) (invoke component 'setAlignmentY <java.awt.Component>:BOTTOM_ALIGNMENT))))

; get the parent of parent recursively until we get a parent without parent
(define (get-top-parent component :: <java.awt.Component>)
  (let ((parent (invoke component 'getParent)))
    (if (eqv? #!null parent)
        component
        (begin
          ;(display (get-top-parent parent))(newline)
          (get-top-parent parent)))))

(define (component-update component :: <java.awt.Component>)
  (let ((component-graphics (invoke component 'get-graphics)))
    (invoke component 'update component-graphics)))

; revalidate a component
(define (component-revalidate component :: <javax.swing.JComponent>)
  (invoke (as <javax.swing.JComponent> component) 'revalidate))

; get the root pane
(define (get-component-root-pane component :: <javax.swing.JComponent>) :: <javax.swing.JRootPane>
  (invoke component 'getRootPane))