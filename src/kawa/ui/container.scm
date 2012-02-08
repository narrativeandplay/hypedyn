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

; require
(require "../arrays.scm") ; for array-to-list
(require "component.scm") ;; remove-component (not sure if clear-container should be calling this)

; export
(module-export set-container-layout 
               get-container-children
               validate-container
               get-nth-component
               clear-container
               clear-container-from-index-onwards
               )

;;
;; container
;; 

; set container layout
; currently supports border and flow layout
; for grid layout row and col is passed as args in that order
(define (set-container-layout container :: <java.awt.Container> layout . args)
  (let* (
         (the-layout
          (cond
           ((eq? layout 'border) (<java.awt.BorderLayout>))
           ((eq? layout 'flow) (<java.awt.FlowLayout>
                                (if (pair? args)
                                    (begin (define align (car args))
                                      (cond
                                       ((eq? align 'left) <java.awt.FlowLayout>:LEFT)
                                       ((eq? align 'right) <java.awt.FlowLayout>:RIGHT)
                                       ((eq? align 'center) <java.awt.FlowLayout>:CENTER)
                                       ((eq? align 'leading) <java.awt.FlowLayout>:LEADING)
                                       ((eq? align 'trailing) <java.awt.FlowLayout>:TRAILING)
                                       (else (error "set-container-layout: unknown alignment")))))))
           ((eq? layout 'horizontal) (<javax.swing.BoxLayout>
                                      container
                                      <javax.swing.BoxLayout>:X_AXIS))
           ((eq? layout 'vertical) (<javax.swing.BoxLayout>
                                    container
                                    <javax.swing.BoxLayout>:Y_AXIS))
           ((eq? layout 'grid) (<java.awt.GridLayout> (if (pair? args) 
                                                          (begin 
                                                            ;(display "first arg ")(display (car args))(newline) 
                                                            (car args))
                                                          1)
                                                      (if (and (pair? args) (pair? (cdr args)))
                                                          (begin
                                                            ;(display "second arg ")(display (cadr args))(newline)
                                                            (cadr args)
                                                            )
                                                          1)))
           (else (error "set-container-layout: unknown layout")))))
    (if the-layout
        (invoke container 'setLayout the-layout))))

; get children in a container
(define (get-container-children container :: <java.awt.Container>)
  (let ((children (invoke container 'getComponents)))
    (array-to-list children children:length)))

;; called after changes are made to components inside (but dont need pack-frame ie container size doesnt change)
(define (validate-container container :: <java.awt.Container>)
  (invoke container 'validate))

(define (get-nth-component container :: <java.awt.Container>
                           n :: <int>)
  (invoke container 'get-component n))

;; empty the container of all children component
;; TODO : implement clear-container with get-nth-component instead
(define (clear-container in-container)
  (let ((children (get-container-children in-container)))
    (if (not (null? children))
        (begin
          (remove-component in-container (car children))
          (clear-container in-container)))))

;; clear all children component except the first n=index components
(define (clear-container-from-index-onwards in-container index)
  (let ((children (get-container-children in-container)))
    (if (> (length children) index)
        (begin
          (remove-component in-container (list-ref children index))
          (clear-container-from-index-onwards in-container index)))))

