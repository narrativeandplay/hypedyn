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

(require 'list-lib)
(require "myhashtable.scm") ;; hash-table-get
(require 'hash-table)
(require "../kawa/miscutils.scm") ;; is-void?
(require "datatable.scm") ;; set-dirty!

(module-export new-object derive 
               obj-put    obj-get
               make-typed-object
               new-make-named-object
               make-uniqueID-object
               generate-uniqueID
               ask
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
    
;    (display "this super class ")(display (hash-table-get obj 'super-lst '()))(newline)
    
    ;; so i would expect every one of them has to have init
    ;; one problem if we have multiple inheritance, we cant use the same argv for all of them
;    (obj-put obj 'super-init (lambda (self . argv)
;                           (for-each (lambda (base) (apply (obj-get base 'init) argv)) (obj-get obj 'super-lst))
;                           (first argv)))
    ;; should i write this init? init method is doing nothing in the beginning
    ;(obj-put obj 'init (lambda () '()))
    obj))
  
  
  ; Gets an overridden or inherited property of the object.
  ; Inherited properties are expected to be immutable.
  ; The inheritance hierarchy is specified in the 'super
  ; property as a list, so you can get and modify the
  ; hierarchy at runtime.
(define (obj-get obj prop)
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
                            (let ((val (obj-get (first cs) prop)))
                              (if (is-null? val)
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
  
  ; Sets multiple properties on the object.
  ; Useful for initialization.
  ;
  ; Returns the object itself, so you can chain property
  ; edits.
;(define (putprops obj prop-value-pairs)
;  (for-each (lambda (pv) (put obj (first pv) (rest pv)))
;            prop-value-pairs)
;  obj)

  ; Makes an object defive properties from another.
(define (derive obj prototype)
  (obj-put obj 'super-lst (cons prototype (obj-get obj 'super-lst))))
   
  ; Dispatch based on first argument.
;(define (<- obj method . argv)
;  (newline)(display "obj ")(display obj)(newline)
;  (display "method ")(display method)(newline)
;  (display "argv ")(display argv)(newline)
;  (newline)(display "dispatch ")(newline)
;  (define gotten-method (obj-get obj method))
;  (newline)(display "method here")(newline)
;  (if (not (is-null? gotten-method)) ;; check if the method exists
;      (apply gotten-method (cons obj argv))
;      (begin
;        (display "[ERROR doing <-] method not found")(newline)
;        #!null)))

  ; Apply a sequence of methods on the object.
;(define (do-with obj . methods)
;  (for-each (lambda (method) (method obj)) methods))
  
  ; Chains a sequence of method invocations.
  ; The result of the first method invocation becomes
  ; the subject of the second and so on.
;(define (chain-with obj . methods)
;  (if (null? methods)
;      obj
;      (chain-with ((first methods) obj) (rest methods))))

;  ; Query inheritance
;(define (super? a b)
;  (let ((inh (obj-get a 'super-lst)))
;    (display "inh ")(display inh)(newline)
;    (cond
;     ((eq? a b) #t)
;     ((null? inh) #f)
;     ;; goes through inh list to check for super? condition
;     (else (any (lambda (base) (super? base b)) inh))
;     )))

  ; (with ((obj1 (prop1 var1a) (prop2 var2a) ..)
  ;        (obj2 (prop1 var1b) (prop2 var2b) ..))
  ;    ..expr using var1a, var2b etc.
  ; )
;(define-macro (with bindings . body)
;  (if (cons? (second (first bindings)))
;      (let ((vars-list (apply append (map (lambda (bs)
;                                            (map (lambda (b) (second b)) (rest bs)))
;                                          bindings)))
;            (vals-list (cons 'list (apply append (map (lambda (bs)
;                                                        (map (lambda (b)
;                                                               (list 'get (first bs) (list 'quote (first b))))
;                                                             (rest bs)))
;                                                      bindings)))))
;        `(apply (lambda ,vars-list ,@body) ,vals-list))
;      (let ((vars-list (apply append (map (lambda (bs) (rest bs)) bindings)))
;            (vals-list (cons 'list (apply append (map (lambda (bs)
;                                                        (map (lambda (b)
;                                                               (list 'get (first bs) (list 'quote b)))
;                                                             (rest bs)))
;                                                      bindings)))))
;        `(apply (lambda ,vars-list ,@body) ,vals-list))))


;; typing system

(define (make-typed-object type-sym)
  (define typed-object (new-object))
  (obj-put typed-object 'obj-type (lambda (self) type-sym))
  typed-object)

;(define (make-named-object name)
;  (lambda (message)
;    (cond ((eq? message 'name) (lambda (self) name))
;          ((eq? message 'set-name!) (lambda (self newname)
;                                      (begin
;                                        (set! name newname)
;                                        (set-dirty!))))
;          (else (no-method name)))))

(define (new-make-named-object name-str)
  (define new-named-object (new-object))
  (obj-put new-named-object 'name (lambda (self) name-str))
  (obj-put new-named-object 'set-name!
           (lambda (self newname)
             (begin
               (set! name-str newname)
               ;(set-dirty!)
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
;    (lambda (message)
;      (cond ((eq? message 'ID)  (lambda (self) ID))
;            (else (get-method named-obj message))))
    (obj-put this-obj 'ID (lambda (self) ID))
    this-obj))

(define (generate-uniqueID . args)
  ; check if there's an arg, and if its a number
  (if (and (not (eq? '() args)) (number? (car args)))
      (let ((arg (car args)))
        ; using an existing ID, so use it and update nextID
        (set! nextID (max arg nextID))
        arg)
      (begin
        ; no ID passed in, so just generate a new one
        (set! nextID (+ nextID 1))
        nextID)))

;; i wish i had names of the argument i pass in
;; i wish i had the number of arguments passed into the procedure

(define (ask obj prop . argv)
  (define gotten-attr (obj-get obj prop))
  (if (procedure? gotten-attr)
      (begin
        ;(apply <- (append (list obj argv)))
        (apply gotten-attr (append (list obj) argv))
        )
      (begin
        (newline)
        (display "NOT PROC ")(display gotten-attr)(newline)
        (display "obj ")(display obj)(newline)
        (display "prop ")(display prop)(newline)
        (display "argv ")(display argv)(newline)
        (display "class ")(display (invoke gotten-attr 'get-class))(newline)
        gotten-attr
        )
      ))

