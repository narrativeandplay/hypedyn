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

; simple evaluator

(module-export myeval 
               add-language-start add-language-end reset-environment
               primitive-procedure-add! add-primitive-procedures-start add-primitive-procedures-end
               primitive-procedure-names
               set-end-of-code get-end-of-code)

;(module-static 'init-run)

;; when we reached end of code
(define end-of-code #f)
(define (set-end-of-code new-state :: <boolean>)
  (set! end-of-code new-state))
(define (get-end-of-code)
  end-of-code)

; evaluator which only uses our primitives
; note: currently doesn't handle expressions, and variable bindings are not persistent
(define (myeval exp)
  ; special case: if its a "begin" at the top level, then run through and eval the expressions in sequence
  ; this is because java cannot handle very long expressions, see http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4262078.
  (if (eq? 'begin (car exp))
      (map (lambda (this-exp)
             (do-myeval this-exp))
           (cdr exp))
      (do-myeval exp)))
  
(define (do-myeval exp)
  (eval (list 'let primitive-procedures exp) (null-environment 5)))

; our environment
(define the-global-environment #f)

; reset the global environment
; this is currently not used, experimenting with persistent bindings
(define (reset-environment)
  (set! the-global-environment (null-environment 5)))

; start adding a language
(define (add-language-start)
  ;(add-expressions-start)
  (add-primitive-procedures-start))

; finish adding a language
(define (add-language-end)
  (add-primitive-procedures-end)
  ;(add-expressions-end)
  )

;; defining primitive procedures in terms of underlying
;; Scheme implementation

; list of any primitives in the underlying Scheme
; implementation that we're providing; first entry is the
; primitive in our language, second is underlying
; implementation
(define primitive-procedures '())

; install new primitive
(define (primitive-procedure-add! name object)
  (set! primitive-procedures
        (append primitive-procedures
                (list (list name object)))))

; start adding primitive procedures
(define (add-primitive-procedures-start)
  (set! primitive-procedures '())
  (primitive-procedure-add! 'false #f)
  (primitive-procedure-add! 'true #t)
  
  ; experimental stuff
  (primitive-procedure-add! 'make-global! make-global!))
  
; end adding primitive procedures
(define (add-primitive-procedures-end)
  'PRIMITIVE-PROCEDURES-LOADED)

; gets list of primitive procedure names
(define (primitive-procedure-names)
  (map car
       primitive-procedures))

;;
;; experimental stuff
;; 

; basic global bindings:
; making a procedure global allows it to be called anywhere
; making a variable global is a bit messy: the binding becomes
; available everywhere, but the value can only be updated from
; a global procedure which was defined in the same context
(define (make-global! in-symbol in-value)
  (primitive-procedure-add! in-symbol in-value))
