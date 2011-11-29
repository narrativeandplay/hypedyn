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

(require "../geometry.scm") ; for dimensions
(require "component.scm") ; for get-component-size

; export
(module-export make-window set-window-fullscreen bring-to-front set-window-undecorated set-bounds get-screen-size
               pack
               
               set-frame-location center-frame-in-parent 
               set-frame-title pack-frame get-frame-location
               get-active-frame dispose-frame
               set-frame-on-exit-operation set-frame-dont-exit set-focusable
               set-frame-iconified set-frame-maximized toggle-frame-maximized set-frame-normal-state)
               
;;
;; window
;; 

; make a top-level window (ie. a Java frame)
(define (make-window title :: <java.lang.String>)
  (<javax.swing.JFrame> title))

; set undecorated
(define (set-window-undecorated in-frame :: <javax.swing.JFrame> in-flag)
  (invoke in-frame 'setUndecorated in-flag))

; switch to full-screen mode (null to switch out of full-screen mode)
(define (set-window-fullscreen in-frame :: <javax.swing.JFrame>)
  (invoke (invoke (<java.awt.GraphicsEnvironment>:getLocalGraphicsEnvironment)
                  'getDefaultScreenDevice) 'setFullScreenWindow (as <java.awt.Window> in-frame)))

; bring window to the front
(define (bring-to-front in-frame :: <javax.swing.JFrame>)
  (invoke in-frame 'toFront))

; set bounds
(define (set-bounds in-frame :: <javax.swing.JFrame> in-size :: <java.awt.Dimension>)
  (invoke in-frame 'setBounds 0 0 (get-dimension-width in-size) (get-dimension-height in-size)))

; get screen size
(define (get-screen-size) :: <java.awt.Dimension>
  (invoke (<java.awt.Toolkit>:getDefaultToolkit) 'getScreenSize))

; pack
(define (pack comp :: <java.awt.Window>)
  (invoke comp 'pack))

;;
;; frame
;; 

;set frame property
;@param size x,y location x,y 
(define (set-frame-location frame :: <java.awt.Window> ;previously <javax.swing.JFrame>
                            loc-x loc-y)
  (invoke frame 'setLocation loc-x loc-y))

(define (center-frame-in-parent in-frame :: <java.awt.Window>
                                in-parent :: <java.awt.Window>)
  (let-values (((parent-w parent-h) (get-component-size in-parent))
               ((about-w about-h) (get-component-size in-frame)))
    (let* ((parent-pos (get-frame-location in-parent))
           (parent-x (get-point-x parent-pos))
           (parent-y (get-point-y parent-pos)))
      (set-frame-location in-frame
                          (+ parent-x
                             (/ parent-w 2)
                             (- (/ about-w 2)))
                          (+ parent-y
                             (/ parent-h 2)
                             (- (/ about-h 2)))))))

; set title
; Note: in-title is a <String> not a <java.lang.String> so that we
; can pass in a (mutable) Scheme string, such as a series of strings
; appended together
(define (set-frame-title frame :: <java.awt.Frame> ;previously <javax.swing.JFrame>
                         in-title :: <String>)
  (invoke (as <java.awt.Frame> frame) 'setTitle (as <java.lang.String> in-title)))

; pack frame
(define (pack-frame frame :: <java.awt.Window>)
  (invoke frame 'pack))

; get frame location
(define (get-frame-location frame :: <java.awt.Window>) :: <java.awt.Point>
  (invoke frame 'getLocation))

;needs to add handling of cases when more than one frame is available
;check for isVisible() for active frame
(define (get-active-frame)
  ((invoke-static <java.awt.Frame> 'getFrames) 0))

;dispose a frame
(define (dispose-frame frame :: <javax.swing.JFrame>)
  (invoke (as <java.awt.Window> frame) 'dispose))

(define (set-frame-on-exit-operation frame :: <javax.swing.JFrame>)
  (invoke frame 'setDefaultCloseOperation <javax.swing.JFrame>:EXIT_ON_CLOSE))

(define (set-frame-dont-exit frame :: <javax.swing.JFrame>)
  (invoke frame 'setDefaultCloseOperation <javax.swing.JFrame>:DO_NOTHING_ON_CLOSE))

;set focus
(define (set-focusable comp :: <java.awt.Component> 
                       bool :: <boolean>)
  (invoke comp 'setFocusable bool))

; iconify a window
(define (set-frame-iconified frame :: <javax.swing.JFrame>)
  (invoke frame 'setExtendedState(bitwise-ior (invoke frame 'getExtendedState) <javax.swing.JFrame>:ICONIFIED)))

; maximize a window
(define (set-frame-maximized frame :: <javax.swing.JFrame>)
  (invoke frame 'setExtendedState(bitwise-ior (invoke frame 'getExtendedState) <javax.swing.JFrame>:MAXIMIZED_BOTH)))

; toggle a window's maximized state
(define (toggle-frame-maximized frame :: <javax.swing.JFrame>)
  (invoke frame 'setExtendedState(bitwise-xor (invoke frame 'getExtendedState) <javax.swing.JFrame>:MAXIMIZED_BOTH)))

; restore a window to normal state
(define (set-frame-normal-state frame :: <javax.swing.JFrame>)
  (invoke frame 'setExtendedState(bitwise-ior (invoke frame 'getExtendedState) <javax.swing.JFrame>:NORMAL)))
