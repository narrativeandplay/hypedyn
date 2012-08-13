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

;; require on kawa side should be removed
(require "myhashtable.scm") ;; hash-table-get
(require 'hash-table)

(define provide module-export)

;; module-export should be replaced with provide in the preprocessor
;; of the kawa to racket convertor
(module-export new-object 
               derive 
               obj-put    
               obj-get
               ask
               
               make-typed-object
               new-make-named-object
               make-uniqueID-object
               generate-uniqueID
               reset-uniqueID
               )
  ;(print-hash-table #t)
  
  ; Objects. 
  ;
  ; Since we need maximum flexibility for the object system
  ; and have full reflection support, we use a very basic
  ; and general object representation - a hashtable that
  ; maps properties to values. We can then query the hashtable
  ; to get properties, rules, actions, etc. that are associated
  ; with a turtle. It is also trivial to set up an inheritance
  ; behaviour hierarchy between such "objects". We also
  ; dissolve the difference between "class" and "object".
  
  ;; ====================================================================||
  ;;
  ;; Notes on the new objects system relative to the old lambda based one||
  ;;
  ;; ====================================================================||

  ;; in the old system we always keep any values within the make-object definition
  ;;  ;; old system
  ;;  eg  (define (make-node ...)
  ;;           (let ((var #f))
  ;;     
  ;;            (lambda (message)
  ;;               (cond ((equal? message 'get-var)
  ;;                      (lambda (self) var)))
  ;;  ;; new system there is a new possibility of just keeping all the variable 
  ;;  ;; inside the hashtable inside this-obj
  ;;
  ;;  eg (define (make-node ...)
  ;;         (let ((this-obj (new-object))
  ;;               (var1 #f))
  ;;           (obj-put 'var1 (lambda (self) var1))   ;; var1 is using old style object
  ;;           (obj-put 'var2 var2)                   ;; var2 is new style 
  ;;           (obj-put 'method1 (lambda (self) ...))
  ;; 
  ;; in the new system we have the dilemma of where a var should be placed
  ;;
  ;; choice one, we can really just put the var inside our object hashtable
  ;; then any get and set will be done to that value inside and
  ;; this seemed the right and natural way of using this

  ;; choice two, keep the old way of keeping variables.
  ;; i chose this in order not to change too many things during the "porting"
  ;; between the old to new object system but i intend to switch to this when I have time
  ;;
  ;; one pro i can see from doing the old way is whatever variable definitions are seen
  ;; in the let, whereas in the new system, the coder must know that the definitions and 
  ;; setting in the object is done through obj-put.

  ;; one con is that getter and setter messages require longer code when we do it the old way
  ;; when given the message to get, we are actually just returning (lambda (self) var)
  ;; because we need var to be a referencing the var in the scope and not a value in the hashtable.
  ;; if we did it the new way, we can simply return the value from hashtable because the real var 
  ;; value is inside. the new way also hides all the let definitions that sometimes make the object 
  ;; longer than it should be. one line of obj-put should be enough to reveal that such a variable exists.

  ;; future todo (1) 
  ;; inside new-object 
  ;; differentiate between methods and variables (keep two hashtables)
  ;; idea is we want to do the following too

  ;; next time we would like to be able to do this
  ;; to any variables by default when we have a definition
  ;; (ask obj 'get 'var1)
  ;; (ask obj 'set 'var1 value)
  ;; or perhaps 
  ;; (ask obj 'get-var1)
  ;; (ask obj 'set-var1 value)

  ;; currently define and set variable is done like this
  ;; (obj-put 'var1 #f) ;; defining to initial value of #f
  ;; (obj-put 'var1 #t) ;; setting to true

  ;; currently is possible to do something like this for getting
  ;; (obj-get obj 'var1)
  ;; and this for setting
  ;; (obj-put obj 'var1 #t)
  ;; but it isn't really looking homogenous with the ask framework

  ;; we can do something addition to make this work
  ;; (obj-put obj 'get-var1 (lambda (self) (obj-get self 'var1))
  ;; (obj-put obj 'set-var1 (lambda (self val) (obj-put self 'var1 val))
  ;; but this generate extra code
  
  ;; future todo (2)
  ;; consider the following case
  ;; (obj-put this-obj 'is-shown?
  ;;            (lambda (self) visible?))
  ;;
  ;; if we did todo (1) it would look like this
  ;; (obj-put this-obj 'is-shown?
  ;;            (lambda (self) (ask this-obj 'get-visible?)))
  ;; 
  ;; there should be a more convenient way of setting equivalent messages
  ;; (obj-equivalent this-obj 'get-visible? 'is-shown?)
  ;; so that we can do
  ;; (ask this-obj 'get-visible?) and still return is-shown? variable


  ; An object is simply a hash-table
(define (new-object . super-objs )
  (let ((obj (make-hash-table)))
    (obj-put obj 'super-lst super-objs)
    obj))
  
  ; Gets an overridden or inherited property of the object.
  ; Inherited properties are expected to be immutable.
  ; The inheritance hierarchy is specified in the 'super
  ; property as a list, so you can get and modify the
  ; hierarchy at runtime.
(define (obj-get obj prop)
  (if (not obj)
      (begin
        (display obj)(newline)
        ))
  (hash-table-get obj prop
                  ;; do when cant get in this hash-table (in this case get from super objects)
                  (lambda ()
                    ; Get the supers list if available.
                    (let ((supers (hash-table-get obj 'super-lst '())))
                      ; Run through the supers to get the value.
                      (let loop ((cs supers))
                        ;(display "supers ")(display supers)(newline)
                        (if (equal? cs '())
                            ;; return null when value not found
                            #!null
                            (let ((val (obj-get (car cs) prop)))
                              (if ;(is-null? val)
                                  (equal? #!null val)
                                  (loop (cdr cs))
                                  val))))))))
  
  ; Set the given property of the object to the
  ; given value. All inherited objects that
  ; don't override the given property automatically
  ; gain the property via getprop.
  ;
  ; Returns the object itself, so you can chain
  ; property edits.
(define (obj-put obj prop value)
  (hash-table-put! obj prop value)
  obj)

  ; Makes an object defive properties from another.
(define (derive obj prototype)
  (obj-put obj 'super-lst (cons prototype (obj-get obj 'super-lst))))

;; typing system

(define (make-typed-object type-sym)
  (define typed-object (new-object))
  (obj-put typed-object 'obj-type (lambda (self) type-sym))
  typed-object)

(define (new-make-named-object name-str)
  (define new-named-object (new-object))
  (obj-put new-named-object 'name (lambda (self) name-str))
  (obj-put new-named-object 'set-name!
           (lambda (self newname)
             (begin
               (set! name-str newname)
               )))
  new-named-object)

; generate a unique ID
(define nextID 0)

(define (reset-uniqueID)
  (set! nextID 0))

;; uniqueID-object - each has a unique ID
;; if an ID is passed in, this means that we're creating from a save file,
;; so instead of generating the unique ID, use the one that was passed in
(define (make-uniqueID-object name . args)
  (let* ((ID (generate-uniqueID (if (pair? args) (car args))))
         (named-obj (new-make-named-object name))
         (this-obj (new-object named-obj))
         )
    (obj-put this-obj 'ID (lambda (self) ID))
    this-obj))

(define (generate-uniqueID . args)
  ; check if there's an arg, and if its a number
  (if (and (not (eq? '() args)) 
           (number? (car args)))
      (let ((arg (car args)))
        ; using an existing ID, so use it and update nextID
        (set! nextID (max arg nextID))
        arg)
      (begin
        ; no ID passed in, so just generate a new one
        (set! nextID (+ nextID 1))
        nextID)))

(define (ask obj prop . argv)
  (if (hash-table? obj)
      (begin
        (define gotten-proc (obj-get obj prop))
        (if (procedure? gotten-proc)
            (begin
              ;; argument count check 
              ;; add 1 because we always pass self as a mandatory argument
              (define (arg-num-check proc)
                (and (>= (+ (length argv) 1) (invoke proc 'min-args))
                     (<= (+ (length argv) 1) (invoke proc 'max-args)))
                
                )
              
              (if (arg-num-check gotten-proc)
                  (apply gotten-proc (append (list obj) argv))
                  (begin
                    (display "[objects.scm](ask) argument count does not match ")(newline)
                    (display (length argv))(display " arguments given")(newline)
                    (display (invoke gotten-proc 'min-args)) (display " to ")(display (invoke gotten-proc 'max-args))(display " expected")(newline)
                    
                    (display "ask prop ")(display prop)(newline)
                    (display "argv ")(display argv)(newline)
                    )))
            ;; Display Error
            (begin
              (display "asked ")(display prop)(newline)
              (display "[objects.scm](ask) no such message ")(newline)
              ;;(display "NOT PROC ")(display gotten-proc)(newline)
                                        ;(display "obj ")(display obj)(newline)
              ;;(display "prop ")(display prop)(newline)
              ;;(display "argv ")(display argv)(newline)
              
;              (if (and (not (void? gotten-proc))
;                       (not (null? gotten-proc)))
;                  (begin
;                    (display "class ")(display (invoke gotten-proc 'get-class))(newline)
;                    ))
;              gotten-proc
              )
            ))
      (begin
        (display "asked ")(display prop)(newline)
        (display "[objects.scm](ask) obj is not a hashtable object ")(newline)
        (display "obj ")(display obj)(newline)
        )
      ))

