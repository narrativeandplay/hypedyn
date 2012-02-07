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

;;
;; data structures to hold the text nodes and links
;;
(begin
  (require "../common/objects.scm")
  (require "../common/datatable.scm") ;;get, del, put
  (require "../common/fileio.scm")
  ;(require "../common/inspector.scm")
  (require "hteditor.scm")
  (require 'list-lib))

; export
(module-export set-start-node! get-start-node reset-start-nodeID
               set-document-ruleID! get-document-ruleID reset-document-ruleID has-document-rule?
               set-nodecount-display-callback! reset-node-count inc-node-count dec-node-count get-node-count
               set-import-offsets!
               create-node deletenode
               create-link create-rule create-typed-rule create-typed-rule2
               create-condition create-typed-condition create-action create-fact)

;; debug var 
(define debug-node #f)

;; importing

; remember last object ID
(define-private import-offset-ID 0)
(define-private import-offset-x 0)
(define-private import-offset-anywhere-x 0)
(define (set-import-offsets! max-x max-y
                             max-anywhere-x max-anywhere-y)
  (set! import-offset-ID (generate-uniqueID))
  (set! import-offset-x max-x)
  (set! import-offset-anywhere-x max-anywhere-x))

; our version of set-dirty! which updates window display
;(define (ht-set-dirty!)
;  (set-dirty!)
;  (update-dirty-state))

;; node;
;; overloaded to allow passing in of a predetermined uniqueID, for loading from file
(define-private (make-node name content x y anywhere . args)
                (let* ((links '()) ; just a list of IDs, actual data is in data-table
                       (uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj))
                       (rule 'not-set)
                       (rule-lst '())
                       (visited? 0)
                       (inspectable-fields (list (list 'visited? 'number "visited: "))))
                  (obj-put this-obj 'content
                           (lambda (self)
                             content))
                  (obj-put this-obj 'set-content!
                           (lambda (self newcontent)
                             (set! content newcontent)
                             ;;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'size
                           (lambda (self)
                             (string-length content)))
                  (obj-put this-obj 'links (lambda (self) links))
                  (obj-put this-obj 'visited? (lambda (self) visited?))
                  (obj-put this-obj 'set-visited! (lambda (self val) (set! visited? val)))
                  (obj-put this-obj 'set-x! (lambda (self val) (set! x val)))
                  (obj-put this-obj 'set-y! (lambda (self val) (set! y val)))
                  (obj-put this-obj 'get-x (lambda (self) x))
                  (obj-put this-obj 'get-y (lambda (self) y))
                  (obj-put this-obj 'anywhere? (lambda (self) anywhere))
                  (obj-put this-obj 'set-anywhere! (lambda (self val) (set! anywhere val)))
                  (obj-put this-obj 'addlink
                           ; need to set dirty bit, but not if loading - add a flag?
                           (lambda (self link)
                             (set! links (cons link links))
                             ))
                  (obj-put this-obj 'dellink
                           (lambda (self link)
                             (set! links (delete! link links))))
                  (obj-put this-obj 'rule (lambda (self) rule))
                  (obj-put this-obj 'rule-lst (lambda (self) rule-lst))
                  (obj-put this-obj 'set-rule!
                           (lambda (self new-rule)
                             (set! rule new-rule)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'add-rule
                           (lambda (self new-rule-ID)
                             (set! rule-lst (append rule-lst (list new-rule-ID)))))
                  (obj-put this-obj 'remove-rule
                           (lambda (self ruleID)
                             (set! rule-lst (remove (lambda (this-ruleID) (= this-ruleID ruleID)) rule-lst))))
                  (obj-put this-obj 'set-rule-lst
                           (lambda (self new-rule-lst)
                             (set! rule-lst new-rule-lst)
                             ))
                  
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-node
                                   (ask self 'name)
                                   content
                                   x
                                   y
                                   (if anywhere 'true 'false)
                                   'false
                                   (ask self 'ID))))
                  ;; printable version, for export to text file
                  (obj-put this-obj 'to-printable
                      (lambda (self)
                        (string-append
                         ; name and ID
                         "* " (ask self 'name) " * (" (number->string (ask self 'ID)) ")\n\n"
;;                         ; content
                         content "\n\n -- \n")))
                  (obj-put this-obj 'get-inspectable-fields
                      (lambda (self) inspectable-fields))
                  this-obj))

;; link
;; overloaded to allow passing in of a predetermined uniqueID, for loading from file
;; need to set dirty bit for set-*, perhaps also need flag?
(define-private (make-link name source destination start-index end-index use-destination
                           use-alt-destination use-alt-text alt-destination alt-text . args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj))
                       (rule 'not-set) ; 'not-set or the ruleID
                       (rule-lst '())
                       (followed? 0) ; 0 = not followed, 1 = followed
                       (link-type 'default) ; 'default or 'hover
                       (custom-cursor-image #f) ; filename of custom cursor for this link, if any
                       (user-data #f) ; arbitrary end-user specified data
                       (inspectable-fields (list (list 'followed? 'number "followed: "))))
                  (obj-put this-obj 'source (lambda (self) source))
                  (obj-put this-obj 'destination (lambda (self) destination))
                  (obj-put this-obj 'set-destination!
                           (lambda (self new-destination)
                             (set! destination new-destination)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'start-index (lambda (self) start-index))
                  (obj-put this-obj 'set-start-index!
                           (lambda (self new-start-index)
                             (display "[setting start index] ")(display new-start-index)(newline)
                             (set! start-index new-start-index)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'end-index (lambda (self) end-index))
                  (obj-put this-obj 'set-end-index!
                           (lambda (self new-end-index)
                             (display "[setting end index] ")(display new-end-index)(newline)
                             (set! end-index new-end-index)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'use-destination (lambda (self) use-destination))
                  (obj-put this-obj 'set-use-destination!
                           (lambda (self new-value)
                             (set! use-destination new-value)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'use-alt-destination (lambda (self) use-alt-destination))
                  (obj-put this-obj 'set-use-alt-destination!
                           (lambda (self new-value)
                             (set! use-alt-destination new-value)
                             ;(ht-set-dirty!)
                             ))
                          ; use alternate text: 'true/'false for plain text, 'fact for fact
                  (obj-put this-obj 'use-alt-text (lambda (self) use-alt-text))
                  (obj-put this-obj 'set-use-alt-text!
                           (lambda (self new-value)
                             (set! use-alt-text new-value)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'alt-destination (lambda (self) alt-destination))
                  (obj-put this-obj 'set-alt-destination!
                           (lambda (self new-destination)
                             (set! alt-destination new-destination)
                             ;(ht-set-dirty!)
                             ))
                  ;; alt text: either a string, or a factID if use-alt-text is 'fact
                  (obj-put this-obj 'alt-text (lambda (self) alt-text))
                  (obj-put this-obj 'set-alt-text!
                           (lambda (self new-text)
                             (set! alt-text new-text)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'rule (lambda (self) rule))
                  (obj-put this-obj 'rule-lst (lambda (self) rule-lst))
                  
                  (obj-put this-obj 'set-rule!
                           (lambda (self new-rule)
                             (set! rule new-rule)
                             ;(ht-set-dirty!)
                             ))
                  
                  (obj-put this-obj 'add-rule
                           (lambda (self new-rule-ID)
                             (set! rule-lst (append rule-lst (list new-rule-ID)))
                             ))
                  (obj-put this-obj 'remove-rule
                           (lambda (self ruleID)
                             (set! rule-lst (remove (lambda (this-ruleID) (= this-ruleID ruleID)) rule-lst))))
                  (obj-put this-obj 'set-rule-lst
                           (lambda (self new-rule-lst)
                             (set! rule-lst new-rule-lst)
                             ))
                  
                  (obj-put this-obj 'followed?
                           (lambda (self)
                             followed?))
                  (obj-put this-obj 'set-followed!
                           (lambda (self in-followed?)
                             (set! followed? in-followed?)))
                  (obj-put this-obj 'get-link-type
                           (lambda (self)
                             link-type))
                  (obj-put this-obj 'set-link-type!
                           (lambda (self in-link-type)
                             (set! link-type in-link-type)))
                  (obj-put this-obj 'get-custom-cursor-image
                           (lambda (self)
                             custom-cursor-image))
                  (obj-put this-obj 'set-custom-cursor-image!
                           (lambda (self in-custom-cursor-image)
                             (set! custom-cursor-image in-custom-cursor-image)))
                  (obj-put this-obj 'get-user-data
                           (lambda (self)
                             user-data))
                  (obj-put this-obj 'set-user-data!
                           (lambda (self in-user-data)
                             (set! user-data in-user-data)))
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-link
                                   (ask self 'name)                            ; name (string)
                                   source                                      ; source nodeID (int)
                                   destination                                 ; destination nodeID (int)
                                   start-index                                 ; start index (int)
                                   end-index                                   ; end index (int
                                   (if use-destination 'true 'false)           ; is destination active? (#t/#f)
                                   (if use-alt-destination 'true 'false)       ; is alt destination active? (#t/#f)
                                   (cond                                       ; is alt text active? (#t/#f/'fact)
                                    ((eq? #t use-alt-text) 'true)
                                    ((eq? #f use-alt-text) 'false)
                                    ((eq? 'fact use-alt-text) (quote 'fact)))
                                   alt-destination                             ; alt destination nodeID (int)
                                   alt-text                                    ; alt text (string)
                                   'false                                      ; update procedure callback, must be #f for saving
                                   (ask self 'ID))))                           ; linkID (int)
                  (obj-put this-obj 'get-inspectable-fields
                           (lambda (self) inspectable-fields))
                  this-obj))


;; make-rule2 provides negate? on top of what we already have
;; the old files uses make-rule that does not have the negate arg hence the make-rule2

(define-private (make-rule2 name type and-or negate? parentID #!rest args)
                
                ;; inherit make-rule
                (define parent-rule (apply make-rule (append (list name type and-or parentID) args)))
                (define this-obj (new-object parent-rule))
                
                ;; fall through (triggering this rule will stop other rules below it from getting checked and fired)
                (define fall-through? #t)
                
                (obj-put this-obj 'fall-through? (lambda (self) fall-through?))
                (obj-put this-obj 'set-fall-through? (lambda (self new-ft) (set! fall-through? new-ft)))
                
                ;; add new features and override rule-expr and to-save-sexpr
                (obj-put this-obj 'negate? (lambda (self) negate?)) 
                (obj-put this-obj 'rule-expr
                           (lambda (self)
                             (if negate?
                                 (list 'not
                                       (ask parent-rule 'rule-expr))
                                 (ask parent-rule 'rule-expr))))
                (obj-put this-obj 'type (lambda (self) type))
                
                (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-typed-rule2
                                   (ask self 'name)                        ; name (string)
                                   (list 'quote type)                      ; type ('link/'node)
                                   (list 'quote and-or)                ; expression ('and/'or)
                                   negate?
                                   parentID                                  ; used to be linkID (int)
                                   (ask self 'ID))))
                this-obj)

;; rule
; type: 'link or 'node
(define-private (make-rule name type expression parentID #!rest args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj))
                       ;; note: these actions are actually "before" and "after" for nodes, and
                       ;; "init" and "step" for document rules, but I'm leaving the names as 
                       ;; they are for now, until I generalize the rule structure
                       (then-action #f) ; action to take when rule is satisfied
                       (else-action #f) ; action to take when rule is not satisfied
                       (conditions '()) ; list of conditions which must be satisfied
                       (actions '())   ; generalized actions, currently used for updating facts in node rules
                       (negate? #f))   ;; not the whole condition
                  
                  
                  (obj-put this-obj 'parentID (lambda (self) parentID))
                  (obj-put this-obj 'expression (lambda (self) expression))

                  ;; conditions
                  (obj-put this-obj 'conditions (lambda (self) conditions))
                  (obj-put this-obj 'add-condition!
                           (lambda (self new-condition)
                             (set! conditions (cons new-condition conditions))
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'delcondition
                           (lambda (self condition)
                             (set! conditions (delete! condition conditions))))

                          ; then and else actions
                  (obj-put this-obj 'then-action
                           (lambda (self) then-action))
                  (obj-put this-obj 'set-then-action!
                           (lambda (self new-action)
                             (set! then-action new-action)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'else-action
                           (lambda (self) else-action))
                  (obj-put this-obj 'set-else-action!
                           (lambda (self new-action)
                             (set! else-action new-action)
                             ;(ht-set-dirty!)
                             ))

                  ;; generalized actions - for now, this is just for then facts
                  (obj-put this-obj 'actions    
                           (lambda (self) actions))
                  (obj-put this-obj 'add-action!
                           (lambda (self new-action)
                             ;(set! actions (cons new-action actions))
                             (set! actions (append actions (list new-action)))
                             ;(display "add-action, new actions ")(display actions)(newline)
                             ;(ht-set-dirty!)
                             ))
                  (obj-put this-obj 'delaction
                           (lambda (self action)
                             (set! s (delete! action actions))))

                  ;; convert rule into an s-expr that can be eval-ed by our evaluator
                  (obj-put this-obj 'rule-expr
                           (lambda (self)
                             (define to-return
                               (cons expression
                                   (map (lambda (c)
                                          (let* ((thiscondition (get 'conditions c))
                                                 (targetID (ask thiscondition 'targetID))
                                                 (operator (ask thiscondition 'operator))
                                                 (type (ask thiscondition 'type)))
                                            (cond 
                                             ((eq? type 0)
                                                ; node
                                                (cond ((eq? operator 0) (list 'not (list 'visited? targetID)))
                                                      ((eq? operator 1) (list 'visited? targetID))
                                                      ((eq? operator 2) (list 'previous? targetID))))
                                             ((eq? type 1)
                                                ; link
                                                (cond ((eq? operator 0) (list 'not (list 'followed? targetID)))
                                                      ((eq? operator 1) (list 'followed? targetID))))
                                             ((eq? type 2)
                                                ; fact
                                                (cond ((eq? operator 0) (list 'not (list 'holds? targetID)))
                                                      ((eq? operator 1) (list 'holds? targetID)))))))
                                        conditions)))
                             ;(display "rule-expr returning ")(newline)
                             ;(display to-return)(newline)
                             to-return
                             ))
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-typed-rule
                                   (ask self 'name)                        ; name (string)
                                   (list 'quote type)                      ; type ('link/'node)
                                   (list 'quote expression)                ; expression ('and/'or)
                                   parentID                                ; parent obj's ID (int)
                                   (ask self 'ID))))                       ; ruleID
                  this-obj))

;; condition
;; type: either node (0), link (1), or fact (2)
;; targetID: either nodeID or linkID depending on type
;; operator: the condition operator: 
;;     for nodes: not visited (0), visited (1), or previous (2)
;;     for links: not followed (0) or followed (1)
;;     for facts: false (0) or true (1)
;; ruleID: the ID of the containing rule
(define-private (make-condition name type targetID operator ruleID . args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj)))
                  (obj-put this-obj 'type (lambda (self) type))
                  (obj-put this-obj 'targetID (lambda (self) targetID))
                  (obj-put this-obj 'ruleID (lambda (self) ruleID))
                  (obj-put this-obj 'operator (lambda (self) operator))
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-typed-condition
                                   (ask self 'name)                       ; name (string)
                                   type                                   ; type (see above)
                                   targetID                               ; target nodeID or linkID (int)
                                   operator                               ; operator (see above)
                                   ruleID                                 ; parent ruleID (int)
                                   (ask self 'ID))))                      ; conditionID (int)
                  this-obj))

;; create an sexpr that creates the sexpr
;; example (list 'do-action (quote text) "\""string"\"" 1)
;; when written to file it becomes (do-action 'text "string" 1)
;; however if we want the original form to be kept in data structure 
;;  it needs to be written as (list 'list 'do-action (list 'list 'quote 'text) "\"\""string"\"\"" 1)

;; general rule
;; 1) add 'list in from of all list
;; 2) add "\"" to front and back of strings
(define (sexpr-recreate sexpr)
  (if (pair? sexpr)
      (cons
       'list
       (map (lambda (element)
              (cond ((pair? element)
                     (sexpr-recreate element))
                    ((string? element)
                     ;(string-append "\"" element "\"")
                     ;(escape-quotes element)
                     element
                     )
                    ((symbol? element)
                     (list 'quote element))
                    (else element))
              ) sexpr))))

(define (escape-quotes str :: <string>)
  (map (lambda (char-str)
         (display char-str)(display " ")
         ) (string->list str))
  (newline)
  str
  )



;; action
;; an expression to be evaluated, usually when rule is/isn't satisfied
;; type: 'then, 'else, 'before, 'after, 'init or 'step
;; expr: an s-expression to be evaluated when action is triggered
;; ruleID: the rule that this action is attached to
;; type is now the type of event that is relevant to this action
;; eg. follow link action has type 'clicked-link 
;;     replace-link-text has type 'displayed-node
(define-private (make-action name type expr ruleID . args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj)))
                  (obj-put this-obj 'type (lambda (self) type))
                  (obj-put this-obj 'expr (lambda (self) expr))
                  (obj-put this-obj 'set-expr! 
                           (lambda (self new-expr) (if (pair? new-expr)
                                                       (set! expr new-expr))))
                  (obj-put this-obj 'ruleID (lambda (self) ruleID))
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-action
                                   (ask self 'name)                      ; name (string)
                                   (list 'quote type)                    ; type ('then, 'else, 'before, 'after, 'init or 'step)
                                   ;(list 'quote expr) ; NOTE: not sure if expr should be stored as string or s-expr - alex
                                   ;; note: trying s-expr approach, might be more convenient 
                                   ;; to load out the actions into the action panels (teongleong)
                                   (sexpr-recreate expr)                                  ; expression to be evaluated
                                   ruleID                                ; parent ruleID (int)
                                   (ask self 'ID))))                     ; actionID (int)
                  this-obj))

;; fact
;; stores a value of a given type ('boolean, 'string or 'number)
;; checking if a non-boolean holds basically checks if its #f (unset)
(define-private (make-fact name type . args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj))
                       (value #f)
                       (inspectable-fields (list (list 'get-value type "value: "))))
                  (obj-put this-obj 'type (lambda (self) type))
                  (obj-put this-obj 'holds? (lambda (self) (not (eq? value #f))))
                  (obj-put this-obj 'assert
                           (lambda (self) 
                             (set! value #t)))
                  (obj-put this-obj 'retract
                           (lambda (self) 
                             (set! value #f)))
                  (obj-put this-obj 'get-value   
                           (lambda (self) 
                             value))
                  (obj-put this-obj 'set-value!
                           (lambda (self in-value)
                             (set! value in-value)))
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-fact
                                   (ask self 'name)                        ; name (string)
                                   (list 'quote type)                      ; type ('boolean/'string/'number)
                                   (ask self 'ID))))                       ; factID (int)
                  (obj-put this-obj 'get-inspectable-fields
                           (lambda (self) inspectable-fields))
                  this-obj))

; keep track of document-level rule (OLD)
(define-private document-ruleID 'not-set)
(define (set-document-ruleID! ruleID)
  (set! document-ruleID (if (importing?)
                            (+ import-offset-ID ruleID)
                            ruleID))
  ;(ht-set-dirty!)
  )
(define (get-document-ruleID)
  document-ruleID)
(define (reset-document-ruleID)
  (set! document-ruleID 'not-set))
;(define (has-document-rule?)
;  (not (eq? document-ruleID 'not-set)))

;; new rule list at document level
(define-private document-ruleID-lst '())
(define-private (add-document-ruleID ruleID)
    (set! document-ruleID-lst (append document-ruleID-lst (list ruleID))))
(define (get-document-ruleID-lst) document-ruleID-lst)
(define (reset-document-ruleID-lst)
  (set! document-ruleID-lst '()))
(define (has-document-rule?)
  (not (null? document-ruleID-lst)))

; keep track of start node
(define-private start-nodeID #f)
(define (set-start-node! nodeID)
  (set! start-nodeID (if (importing?)
                         (+ import-offset-ID nodeID)
                         nodeID))
  ;(ht-set-dirty!)
  )
(define (get-start-node)
  start-nodeID)

; reset the start node
(define (reset-start-nodeID)
  (set! start-nodeID #f))

; track number of nodes
(define-private node-count 0)

; provide callback to update a display of node count
(define-private nodecount-display-callback #f)
(define (set-nodecount-display-callback! in-callback)
  (set! nodecount-display-callback in-callback))

; reset node count
(define (reset-node-count)
  (set! node-count 0)
  (if nodecount-display-callback
      (nodecount-display-callback node-count)))

; increment node count
(define (inc-node-count)
  (set! node-count (+ node-count 1))
  (if nodecount-display-callback
      (nodecount-display-callback node-count)))

; decrement node count
(define (dec-node-count)
  (set! node-count (- node-count 1))
  (if nodecount-display-callback
      (nodecount-display-callback node-count)))

; check node count
(define (get-node-count)
  node-count)

; add node to node-list and data-table, and to graph-editor
(define (create-node name content x y anywhere update-display . args)
  ;(format #t "Creating node: ~a~%~!" name)
  (let* ((actual-x (if (importing?)
                       (if anywhere
                           (+ x import-offset-anywhere-x)
                           (+ x import-offset-x))
                       x))
         (new-node (make-node name content
                              actual-x y
                              anywhere
                              (if (pair? args)
                                  (if (importing?)
                                      (+ (car args) import-offset-ID) ; if importing, shift by offset
                                      (car args))))))
    (set! debug-node new-node)
    
    (let ((new-nodeID (ask new-node 'ID)))
      
      ; add to table
      (put 'nodes new-nodeID new-node)

      ; update node count
      (inc-node-count)
      
      ; update display if necessary
      (if (procedure? update-display)
          (update-display new-nodeID name
                          actual-x y
                          anywhere))

      ; return the new node ID
      new-nodeID)))

; delete  a node from data table
(define (deletenode nodeID)
  ; delete from table
  (del 'nodes nodeID)

  ; update node count
  (dec-node-count))

; add link to link-list, selected-node's list of links, and data-table
; if update-graph is true, add to the graph display
(define (create-link name fromnodeID tonodeID start-index end-index use-destination
                     use-alt-destination use-alt-text alt-destination alt-text
                     update-display . args)
  ;(format #t "Creating link: ~a~%~!" name)
;  (display "create link args name ")(newline)
;  (display " ")(display "create-link name fromnodeID tonodeID start-index end-index use-destination
;                     use-alt-destination use-alt-text alt-destination alt-text
;                     update-display . args")(newline)
;  (display "create link ")(display (list name fromnodeID tonodeID start-index end-index
;                                         use-destination use-alt-destination))(newline)
;  (display "args2 ")(display (list use-alt-text alt-destination alt-text))(newline)
;  (display "name type ")(display (invoke name 'get-class))(newline) 
;  (display "fromnodeID ")(display (invoke fromnodeID 'get-class))(newline)
;  (display "tonodeID ")(display (invoke tonodeID 'get-class))(newline)
  (let* ((actual-fromnodeID (if (importing?)
                                (+ fromnodeID import-offset-ID)
                                fromnodeID))
         (actual-tonodeID (if (importing?)
                              (+ tonodeID import-offset-ID)
                              tonodeID))
         (actual-alt-destination (if (importing?)
                                     (+ alt-destination import-offset-ID)
                                     alt-destination))
         (new-link (make-link name
                              actual-fromnodeID
                              actual-tonodeID
                              start-index end-index
                              use-destination use-alt-destination use-alt-text
                              actual-alt-destination alt-text
                              (if (pair? args)
                                  (if (importing?)
                                      (+ (car args) import-offset-ID)
                                      (car args)))))
         (from-node (get 'nodes actual-fromnodeID))
         (to-node (get 'nodes actual-tonodeID))
         (new-linkID (ask new-link 'ID)))
    
    (if from-node
        (ask from-node 'addlink new-linkID))
    (put 'links new-linkID new-link)

    ; return the new link's ID
    new-linkID))

; create a rule - for links, retained for backwards compatibility
(define (create-rule name expression linkID . args)
  (create-typed-rule name 'link expression linkID (if (pair? args) (car args))))

; create a typed rule
; type: 'link or 'node
;; this is preserved to load older file format of hypedyn
(define (create-typed-rule name type expression parentID . args)
  ;(format #t "Creating rule: ~a~%~!" name)
  (let* ((actual-parentID (if (importing?)
                              (+ parentID import-offset-ID)
                              parentID))
         (new-rule (make-rule name type expression actual-parentID
                              (if (pair? args)
                                  (if (importing?)
                                      (+ (car args) import-offset-ID)
                                      (car args)))))
         (rule-ID (ask new-rule 'ID)))

    ; add to rule list
    (put 'rules rule-ID new-rule)
    ;(format #t "New rule: ~a~%~!" new-rule)

    ; set rule in parent
    (if (eq? type 'doc)
        ; document rule, so just set
        (set-document-ruleID! rule-ID)
        ; otherwise, get the parent and set
        (let* ((the-get-symbol (if (eq? type 'link)
                                   'links
                                   'nodes))
               (the-parent (get the-get-symbol actual-parentID)))
          (if the-parent
              (ask the-parent 'set-rule! rule-ID))))
    
    ; return the rule ID
    rule-ID))

;; version 2 of create-typed-rule 
;; expression from version 1 takes either 'and or 'or (thus i renamed it as so and-or)
;; version 2 has a new argument negate? which takes in a boolean 
;; it duplicates most of the code except that it uses make-rule2 
;; TODO: combine create-typed-rule with this
(define (create-typed-rule2 name type and-or negate? parentID . args)
  (let* ((actual-parentID (if (importing?)
                              (+ parentID import-offset-ID)
                              parentID))
         (new-rule (make-rule2 name type and-or negate? actual-parentID
                               (if (pair? args)
                                   (if (importing?)
                                       (+ (car args) import-offset-ID)
                                       (car args)))))
         (rule-ID (ask new-rule 'ID)))
    
    ; add to rule list
    (put 'rules rule-ID new-rule)
    ;(format #t "New rule: ~a~%~!" new-rule)

    ; set rule in parent
    (if (eq? type 'doc)
        
        ; document rule, so just set
        ;(set-document-ruleID! rule-ID)
        (add-document-ruleID rule-ID)
        
        ; otherwise, get the parent and set
        (let* ((the-get-symbol (if (eq? type 'link) 
                                   'links
                                   'nodes))
               (the-parent (get the-get-symbol actual-parentID)))
          (if the-parent
              ;(ask the-parent 'set-rule! rule-ID)
              (begin
                (ask the-parent 'add-rule rule-ID))
              )))
    
    ; return the rule ID
    rule-ID))

; create a condition - for nodes, retained for backward compatability
(define (create-condition name nodeID operator ruleID . args)
  (create-typed-condition name 0 nodeID operator ruleID (if (pair? args) (car args))))

; create a condition
(define (create-typed-condition name type targetID operator ruleID . args)
  (let* ((actual-ruleID (if (importing?)
                            (+ ruleID import-offset-ID)
                            ruleID))
         (actual-targetID (if (importing?)
                              (+ targetID import-offset-ID)
                              targetID))
         (new-condition (make-condition name type actual-targetID operator actual-ruleID
                                        (if (pair? args)
                                            (if (importing?)
                                                (+ (car args) import-offset-ID)
                                                (car args)))))
         (the-rule (get 'rules actual-ruleID)))
    
    ;(format #t "Creating condition: ~a(~a), targetID:~a~%~!" name (ask new-condition 'ID) actual-targetID)
    
    ; add to condition list
    (put 'conditions (ask new-condition 'ID) new-condition)

    ; add condition to rule
    (if the-rule
        (ask the-rule 'add-condition! (ask new-condition 'ID)))))

; create an action
(define (create-action name type expr ruleID . args)
  
  ;(display "[create-action] expr ")(display expr)(newline)
  (let* ((actual-ruleID (if (importing?)
                            (+ ruleID import-offset-ID)
                            ruleID))
         (new-action (make-action name type expr actual-ruleID
                                  (if (pair? args)
                                      (if (importing?)
                                          (+ (car args) import-offset-ID)
                                          (car args)))))
         (the-rule (get 'rules actual-ruleID))
         (new-action-ID (ask new-action 'ID)) 
         )
    ; add to action list
    (put 'actions new-action-ID new-action)

    ; add action to rule
    ; note: for nodes, before=then=step and after=else=init for now
    ;; TODO: need to clean up all these
    ;;       action type is now the event types that trigger the action 
    ;;       so far there is 'clicked-link 'entered-node 'displayed-node
    
    ;; TODO: need to reactivate the before after step init actions somehow
    ;;       it they are still useful
    (if the-rule
;        (cond
;         ((eq? type 'fact) ; hack until generalize actions
;          (ask the-rule 'add-action! (ask new-action 'ID)))
;         ((eq? type 'then)
;          (ask the-rule 'set-then-action! (ask new-action 'ID)))
;         ((eq? type 'else)
;          (ask the-rule 'set-else-action! (ask new-action 'ID)))
;         ((eq? type 'before)
;          (ask the-rule 'set-then-action! (ask new-action 'ID)))
;         ((eq? type 'after)
;          (ask the-rule 'set-else-action! (ask new-action 'ID)))
;         ((eq? type 'step)
;          (ask the-rule 'set-then-action! (ask new-action 'ID)))
;         ((eq? type 'init)
;          (ask the-rule 'set-else-action! (ask new-action 'ID)))
;         
;         )
          (ask the-rule 'add-action! (ask new-action 'ID))
        )
    
    new-action-ID))


; create an fact
(define (create-fact name type . args)
  (let* ((new-fact (make-fact name type
                              (if (pair? args)
                                  (if (importing?)
                                      (+ (car args) import-offset-ID)
                                      (car args)))))
         (fact-ID (ask new-fact 'ID)))
    
    
    (put 'facts fact-ID new-fact) ;; add to facts list
    fact-ID))                     ;; and return the ID