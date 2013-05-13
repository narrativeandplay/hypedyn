;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2013
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
  (require "../common/myhashtable.scm")
  (require 'hash-table)
  (require "../common/fileio.scm")
  (require "../common/list-helpers.scm") ;; list-replace
  ;(require "../common/inspector.scm")
  (require "hteditor.scm")
  (require 'list-lib))

; export
(module-export set-start-node! get-start-node reset-start-nodeID
               set-document-ruleID! get-document-ruleID reset-document-ruleID has-document-rule?
               set-nodecount-display-callback! reset-node-count inc-node-count dec-node-count get-node-count
               set-import-offsets!
               create-node deletenode
               create-link create-action create-fact
               create-rule create-typed-rule create-typed-rule2 create-typed-rule3
               create-condition create-typed-condition  create-typed-condition2
               ;;dup-offset-ID dup-offset-x dup-offset-anywhere-x 
               get-duplicate-offsets
               conversion-flag set-conversion-flag!
               )


;; mark for conversion
;; when this flag is on, any new node and link created would be marked for conversion
(define conversion-flag #f)
(define (set-conversion-flag! new-flag)
  (set! conversion-flag new-flag))

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

;; duplicate obj (not sure whether import-offset-ID should be public, should we just use import offset and make it public?)

;(define dup-offset-ID 0)
;(define dup-offset-x 0)
;(define dup-offset-anywhere-x 0)

(define (get-duplicate-offsets max-x max-y
                               max-anywhere-x max-anywhere-y)
  (values (generate-uniqueID) max-x max-anywhere-x))


; our version of set-dirty! which updates window display
;(define (ht-set-dirty!)
;  (set-dirty!)
;  (update-dirty-state))

;; inherited by both make-link and make node
;; just keeps track of a list of ruleIDs
(define-private (rule-containing-object)
                (let ((rule 'not-set) ;; legacy attribute (dont think we use it anymore)
                      (rule-lst '())
                      (this-obj (new-object)))
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
                  (obj-put this-obj 'replace-rule
                           (lambda (self ruleID new-rule-ID)
                             (display "replace rule ")(display rule-lst)(newline)
                             (set! rule-lst (list-replace rule-lst (list-index (lambda (this-ruleID) (= ruleID this-ruleID)) rule-lst) new-rule-ID))
                             (display "after replace rule ")(display rule-lst)(newline)
                             ))
                  ;; remove the last rule on the lst (right most)
                  (obj-put this-obj 'remove-last-rule
                           (lambda (self)
                             ;; this changes the lst getting passed in
                             (define (remove-last! lst)
                               (if (not (= (length (cdr lst)) 1))
                                   (remove-last! (cdr lst))
                                   (set-cdr! lst '())))
                             (remove-last! rule-lst))
                           )

                  (obj-put this-obj 'set-rule-lst
                           (lambda (self new-rule-lst)
                             (set! rule-lst new-rule-lst)
                             ))
                  this-obj))

;; node;
;; overloaded to allow passing in of a predetermined uniqueID, for loading from file
(define-private (make-node name content x y anywhere . args)
                (let* ((links '()) ; just a list of IDs, actual data is in data-table
                       (uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (rule-container (rule-containing-object))
                       (this-obj (new-object uniqueID-obj rule-container))
                       (visited? 0)
                       (inspectable-fields (list (list 'visited? 'number "visited: ")))
                       (start-indices (make-hash-table))
                       (end-indices (make-hash-table))
                       (convert-flag #f) ;; marked for conversion
                       )
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

                  (obj-put this-obj 'start-indices (lambda (self) start-indices))
                  (obj-put this-obj 'end-indices (lambda (self) end-indices))

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

                  ;; mark for conversion
                  (obj-put this-obj 'convert-flag
                           (lambda (self) convert-flag))
                  (obj-put this-obj 'set-convert-flag!
                           (lambda (self new-flag)
                             (set! convert-flag new-flag)))
                  this-obj))

;; link
;; overloaded to allow passing in of a predetermined uniqueID, for loading from file
;; need to set dirty bit for set-*, perhaps also need flag?
(define-private (make-link name source destination start-index end-index use-destination
                           use-alt-destination use-alt-text alt-destination alt-text . args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (rule-container (rule-containing-object))
                       (this-obj (new-object uniqueID-obj rule-container))
                       (followed? 0) ; 0 = not followed, 1 = followed
                       (link-type 'default) ; 'default or 'hover
                       (custom-cursor-image #f) ; filename of custom cursor for this link, if any
                       (user-data #f) ; arbitrary end-user specified data
                       (inspectable-fields (list (list 'followed? 'number "followed: ")))
                       (convert-flag #f) ;; marked for conversion
                       )
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

                  ;; marked for conversion
                  (obj-put this-obj 'convert-flag
                           (lambda (self) convert-flag))
                  (obj-put this-obj 'set-convert-flag!
                           (lambda (self new-flag)
                             (set! convert-flag new-flag)))

                  this-obj))

;; this has the ability to initialize fall-through? on rule creation
(define-private (make-rule3 name type and-or negate? parentID #!key fixedID fall-through?)
                (define parent-rule (make-rule2 name type and-or negate? parentID fixedID))
                (define this-obj (new-object parent-rule))

                (obj-put this-obj 'fall-through? (lambda (self) fall-through?))
                (obj-put this-obj 'set-fall-through? (lambda (self new-ft)
                                                       (set! fall-through? new-ft)))

                (obj-put this-obj 'to-save-sexpr
                         (lambda (self)
                           (list 'create-typed-rule3
                                 (ask self 'name)                        ; name (string)
                                 (list 'quote (ask self 'type))                      ; type ('link/'node)
                                 (list 'quote (ask self 'and-or))        ; and-or ('and/'or)
                                 negate?
                                 (ask self 'parentID)             ; used to be linkID (int)
                                 fixedID: (ask self 'ID)
                                 fall-through?: fall-through?
                                 )))
                this-obj)

;; make-rule2 provides negate? on top of what we already have
;; the old files uses make-rule that does not have the negate arg hence the make-rule2

(define-private (make-rule2 name type and-or negate? parentID #!optional fixedID)
                ;; inherit make-rule
                ;(define parent-rule (apply make-rule (append (list name type and-or parentID) args)))
                (define parent-rule (make-rule name type and-or parentID fixedID))
                (define this-obj (new-object parent-rule))


                ;(define fall-through? #t)

                ;; fall through (triggering this rule will stop other rules below it from getting checked and fired)
                ;; kept to ensure those hypedyn story that was written and saved using this version of the rule
                ;; does not crash when we check the fall-through checkbox
                ;; that version however did not save the fall through value in the save file anyway 
                (obj-put this-obj 'fall-through? (lambda (self) fall-through?))
                (obj-put this-obj 'set-fall-through? (lambda (self new-ft)
                                                       (set! fall-through? new-ft)))

                ;; add new features and override rule-expr and to-save-sexpr
                (obj-put this-obj 'negate? (lambda (self) negate?))
                (obj-put this-obj 'set-negate!
                         (lambda (self in-negate)
                           (set! negate? in-negate)))
                (obj-put this-obj 'rule-expr
                         (lambda (self)
                           (if negate?
                               (list 'not
                                     (ask parent-rule 'rule-expr))
                               (ask parent-rule 'rule-expr))))
                (obj-put this-obj 'type (lambda (self) type))

                ;; overridden 
                (obj-put this-obj 'to-save-sexpr
                         (lambda (self)
                           (list 'create-typed-rule2
                                 (ask self 'name)                        ; name (string)
                                 (list 'quote (ask self 'type))                      ; type ('link/'node)
                                 (list 'quote (ask self 'and-or))        ; and-or ('and/'or)
                                 negate?
                                 (ask self 'parentID)             ; used to be linkID (int)
                                 (ask self 'ID)
                                 )))
                this-obj)

;; rule
; type: 'link or 'node
(define-private (make-rule name type and-or parentID #!rest args)
                (let* ((uniqueID-obj (make-uniqueID-object name (if (pair? args) (car args))))
                       (this-obj (new-object uniqueID-obj))
                       ;; note: these actions are actually "before" and "after" for nodes, and
                       ;; "init" and "step" for document rules, but I'm leaving the names as 
                       ;; they are for now, until I generalize the rule structure
                       (then-action #f) ; action to take when rule is satisfied
                       (else-action #f) ; action to take when rule is not satisfied
                       (conditions '()) ; list of conditions which must be satisfied
                       (actions '()))   ; generalized actions, currently used for updating facts in node rules

                  (obj-put this-obj 'parentID (lambda (self) parentID))
                  (obj-put this-obj 'set-parentID! (lambda (self new-parentID) (set! parentID new-parentID)))
                  (obj-put this-obj 'and-or (lambda (self) and-or))
                  (obj-put this-obj 'set-and-or!
                           (lambda (self in-and-or)
                             (set! and-or in-and-or)))
                  (obj-put this-obj 'type (lambda (self) type))

                  ;; conditions
                  (obj-put this-obj 'conditions (lambda (self) conditions))
                  (obj-put this-obj 'add-condition!
                           (lambda (self new-condition)
                             (set! conditions (append conditions (list new-condition)))
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
                           (lambda (self actionID)
                             (set! actions (delete actionID actions))
                             ))

                  ;; convert rule into an s-expr that can be eval-ed by our evaluator
                  (obj-put this-obj 'rule-expr
                           (lambda (self)
                             (cons and-or
                                   (map (lambda (c)
                                          (let* ((thiscondition (get 'conditions c))
                                                 (targetID (ask thiscondition 'targetID))
                                                 (operator (ask thiscondition 'operator))
                                                 (type (ask thiscondition 'type)))

                                            (ask thiscondition 'expr)
;                                            (cond
;                                             ((eq? type 0)
;                                              ;; node
;                                              (cond ((eq? operator 0) (list 'not (list 'visited? targetID)))
;                                                    ((eq? operator 1) (list 'visited? targetID))
;                                                    ((eq? operator 2) (list 'previous? targetID))))
;                                             ((eq? type 1)
;                                              ;; link
;                                              (cond ((eq? operator 0) (list 'not (list 'followed? targetID)))
;                                                    ((eq? operator 1) (list 'followed? targetID))))
;                                             ((eq? type 2)
;                                              ;; fact
;                                              (cond ((eq? operator 0) (list 'not (list 'holds? targetID)))
;                                                    ((eq? operator 1) (list 'holds? targetID)))))
                                            ))
                                        conditions))
                             ))
                  (obj-put this-obj 'empty-rule
                           (lambda (self)
                             ;; TODO: should remove conditions and
                             ;;       actions that were in there as well
                             (set! actions '())
                             (set! conditions '())
                             ))

                  (obj-put this-obj 'empty-rule?
                           (lambda (self)
                             (and (null? actions)
                                  (null? conditions))
                             ))

                  ;; overridden 
                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-typed-rule
                                   (ask self 'name)                        ; name (string)
                                   (list 'quote type)                      ; type ('link/'node)
                                   (list 'quote and-or)                    ; and-or ('and/'or)
                                   parentID                                ; parent obj's ID (int)
                                   (ask self 'ID)                          ; ruleID
                                   )))
                  this-obj))

;; condition
;; type: either node (0), link (1), boolean fact (2) or number fact (3)
;; targetID: either nodeID or linkID depending on type
;; operator: the condition operator: 
;;     for nodes: not visited (0), visited (1), or previous (2)
;;     for links: not followed (0) or followed (1)
;;     for facts: false (0) or true (1)
;; ruleID: the ID of the containing rule
;; TODO: when free, we should convert this to sexpr.
;;       the operator and type is too troublesome to process mentally
(define-private (make-condition name type targetID operator ruleID #!optional fixedID)
                (let* ((uniqueID-obj (make-uniqueID-object name fixedID))
                       (this-obj (new-object uniqueID-obj)))
                  (obj-put this-obj 'type (lambda (self) type))
                  (obj-put this-obj 'targetID (lambda (self) targetID))
                  (obj-put this-obj 'ruleID (lambda (self) ruleID))
                  (obj-put this-obj 'operator (lambda (self) operator))

                  ;; new needs testing TODO in progress
                  ;; this is for java reader
                  (obj-put this-obj 'expr (lambda (self)
                                            (cond
                                             ((eq? type 0)
                                              ;; node
                                              (cond ((eq? operator 0) (list 'not (list 'visited? targetID)))
                                                    ((eq? operator 1) (list 'visited? targetID))
                                                    ((eq? operator 2) (list 'previous? targetID))))
                                             ((eq? type 1)
                                              ;; link
                                              (cond ((eq? operator 0) (list 'not (list 'followed? targetID)))
                                                    ((eq? operator 1) (list 'followed? targetID))))
                                             ((eq? type 2)
                                              ;; bool fact 
                                              (cond ((eq? operator 0) (list 'not (list 'holds? targetID)))
                                                    ((eq? operator 1) (list 'holds? targetID))))
                                             ((eq? type 3)
                                              ;; num fact
                                              (let ((comparator (string->symbol (car operator)))
                                                    (operand-type (cadr operator))
                                                    (operand-choice (caddr operator))
                                                    (operand-expr
                                                     (case operand-type
                                                       (("Input") (string->number operand-choice))
                                                       (("Fact") (list 'get-value (string->number operand-choice))))))
                                                ;; comparator is one of these ( '= '< '> '<= '>= )
                                                (list comparator (list 'get-value targetID) operand-expr)
                                                ))
                                             )))

                  (obj-put this-obj 'to-save-sexpr
                           (lambda (self)
                             (list 'create-typed-condition
                                   (ask self 'name)                       ; name (string)
                                   type                                   ; type (string)
                                   targetID                               ; target nodeID or linkID (int)
                                   operator                               ; operator (int)
                                   ruleID                                 ; parent ruleID (int)
                                   (ask self 'ID))))                      ; conditionID (int)
                  this-obj))

;; make-condition2 adds support to number fact comparing
(define-private (make-condition2 name type targetID operator ruleID #!key fixedID numfact-args)
                (define parent-rule (make-condition name type targetID operator ruleID fixedID))
                (define this-obj (new-object parent-rule))

                (define local-numfact-args
                  (if (pair? numfact-args)
                      (cons 'list numfact-args)
                      #f))

                (obj-put this-obj 'numfact-args (lambda (self) numfact-args))
                (obj-put this-obj 'to-save-sexpr
                         (lambda (self)
                           (list 'create-typed-condition2
                                 (ask self 'name)                       ; name (string)
                                 type                                   ; type (string)
                                 targetID                               ; target nodeID or linkID (int)
                                 operator                               ; operator (int)
                                 ruleID                                 ; parent ruleID (int)
                                 fixedID: (ask self 'ID)                ; conditionID (int)
                                 numfact-args: local-numfact-args     ; list of arguments
                                 )))
                )

;; create an sexpr that creates the sexpr
;; example (list 'do-action (quote text) "\""string"\"" 1)
;; when written to file it becomes (do-action 'text "string" 1)
;; however if we want the original form to be kept in data structure 
;;  it needs to be written as (list 'list 'do-action (list 'list 'quote 'text) "\"\""string"\"\"" 1)

;; reason we need this is because we are suppose to create the sexpr in actions ( and whatever stores sexpr) when we are loading 
;; from a saved file

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
                       (this-obj (new-object uniqueID-obj))
                       (imported? (importing?))
                       )

                  ;(display "make-action imported? ")(display imported?)(newline) 

                  (obj-put this-obj 'imported? (lambda (self) imported?))
                  (obj-put this-obj 'set-imported! (lambda (self flag)
                                                     (display "setting import ")(display flag)(newline)
                                                     (set! imported? flag)))

                  ;; TODO: Identify more actions that need their target's ID offset during import

                  ;; importing actions needs special treatments for follow link targets etc
                  (obj-put this-obj 'after-import
                           (lambda (self)
                             (if (pair? expr)
                                 (cond ((or (equal? (car expr) 'follow-link)
                                            (equal? (car expr) 'show-in-popup))
                                        ;; import offset for the dest node ID of follow-link action
                                        ;; (follow-link2 linkID parent-ruleID link-type dest-nodeID)
                                        (ask self 'set-expr!
                                             (list-replace expr 4 (+ (list-ref expr 4) import-offset-ID))))
                                       ((equal? (car expr) 'replace-link-text)
                                        ;; import offset for target linkID's for replace-link-text action
                                        ;;(replace-link-text text-type value linkID)
                                        (ask self 'set-expr!
                                             (list-replace expr 3 (+ (list-ref expr 3) import-offset-ID))))
                                       ((or (equal? (car expr) 'set-value!)
                                            (equal? (car expr) 'assert)
                                            (equal? (car expr) 'retract)
                                            (equal? (car expr) 'add-anywhere-link))
                                        ;; import offset for target linkID's for replace-link-text action
                                        ;;(replace-link-text text-type value linkID)
                                        (ask self 'set-expr!
                                             (list-replace expr 1 (+ (list-ref expr 1) import-offset-ID)))
                                        )
                                       ((equal? (car expr) 'set-number-fact)
                                        ;; replace the first factID
                                        (ask self 'set-expr!
                                             (list-replace expr 1 (+ (list-ref expr 1) import-offset-ID)))

                                        ;; the third argument's format
                                        ;; fact  factID 

                                        ;; math (list operator
                                        ;;            operand1 operand1-type
                                        ;;            operand2 operand2-type)

                                        ;; random (list operand1 operand1-type
                                        ;;            operand2 operand2-type)

                                        ;; operand1 and operand2 are strings for both math and random

;                                          (list 'set-number-fact
;                                              factID
;                                              num-fact-mode
;                                              new-fact-value)

                                        (define third-arg (list-ref expr 3))

                                        (display "third arg ")(display third-arg)(newline)

                                        (define corrected-third-arg
                                          (case (list-ref expr 2)
                                            (("Fact") (+ third-arg import-offset-ID))
                                            (("Math")
                                             (define opr1-str (list-ref third-arg 1))
                                             (define opr2-str (list-ref third-arg 3))
                                             (define opr1 (string->number opr1-str))
                                             (define opr2 (string->number opr2-str))
                                             (define opr1-corrected (+ opr1 import-offset-ID))
                                             (define opr2-corrected (+ opr2 import-offset-ID))
                                             (define opr1-corrected-str (number->string opr1-corrected))
                                             (define opr2-corrected-str (number->string opr2-corrected))
                                             (list (list-ref third-arg 0)
                                                   opr1-corrected-str
                                                   (list-ref third-arg 2)
                                                   opr2-corrected-str
                                                   (list-ref third-arg 4))
                                             )
                                            (("Random")
                                             (define opr1-str (list-ref third-arg 0))
                                             (define opr2-str (list-ref third-arg 2))
                                             (define opr1 (string->number opr1-str))
                                             (define opr2 (string->number opr2-str))
                                             (define opr1-corrected (+ opr1 import-offset-ID))
                                             (define opr2-corrected (+ opr2 import-offset-ID))
                                             (define opr1-corrected-str (number->string opr1-corrected))
                                             (define opr2-corrected-str (number->string opr2-corrected))
                                             (list opr1-corrected-str
                                                   (list-ref third-arg 1)
                                                   opr2-corrected-str
                                                   (list-ref third-arg 3))
                                             )
                                            ))
                                        ;; replace the value 
                                        (ask self 'set-expr!
                                             (list-replace expr 3 corrected-third-arg))
                                        ))) ;; end of if
                             ))

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
  (display "set start node! ")(display nodeID)(newline)
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

  ;;(format #t "Creating node: ~a~%~!" name)
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

    ;; marking conversion
    (if conversion-flag
        (ask new-node 'set-convert-flag! #t))

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

  ;;(format #t "Creating link: ~a~%~!" name)
  (let* ((actual-fromnodeID (if (importing?)
                                (if (not (= fromnodeID -1))
                                    (+ fromnodeID import-offset-ID)
                                    -1)
                                fromnodeID))
         ;; Note: tonodeID is not used now
         (actual-tonodeID (if (importing?)
                              (if (not (= tonodeID -1))
                                  (+ tonodeID import-offset-ID)
                                  -1)
                              tonodeID))
         (actual-alt-destination (if (importing?)
                                     (if (not (= alt-destination -1))
                                         (+ alt-destination import-offset-ID)
                                         -1)
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

    ;; marking conversion
    (if conversion-flag
        (ask new-link 'set-convert-flag! #t))

    (if from-node
        (ask from-node 'addlink new-linkID))

    (put 'links new-linkID new-link)

    ; return the new link's ID
    new-linkID))

; create a rule - for links, retained for backwards compatibility
(define (create-rule name and-or linkID . args)
  (create-typed-rule name 'link and-or linkID (if (pair? args) (car args))))

; create a typed rule
; type: 'link or 'node
;; this is preserved to load older file format of hypedyn
(define (create-typed-rule name type and-or parentID . args)
  ;(format #t "Creating rule: ~a~%~!" name)
  (let* ((actual-parentID (if (importing?)
                              (+ parentID import-offset-ID)
                              parentID))
         (new-rule (make-rule name type and-or actual-parentID
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
        ;; Note: this is where it is different from create-typed-rule-common (so we cant use it)
        ;;       not sure if the old stories written in this version of rule even work
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

(define (create-typed-rule-common new-rule type parentID)

  ; add to rule list
  (define ruleID (ask new-rule 'ID))
  (put 'rules ruleID new-rule)

; add rule in parent
  (if (eq? type 'doc)
      ; document rule, so just add it
      (add-document-ruleID ruleID)

      ;; otherwise, get the parent and set
      (let* ((get-symbol (case type
                           ((link) 'links)
                           ((node) 'nodes)))
             (the-parent (get get-symbol parentID)))
        (if the-parent
            (ask the-parent 'add-rule ruleID))
        )))

;; version 2 of create-typed-rule 
;; expression from version 1 takes either 'and or 'or (thus i renamed it as so and-or)
;; version 2 has a new argument negate? which takes in a boolean 
;; it duplicates most of the code except that it uses make-rule2 
;; also it adds to the rule-lst instead of setting the one and only rule
;; NOTE: this cannot replace create-typed-rule because we need to keep the first version for compatibility
(define (create-typed-rule2 name type and-or negate? parentID #!optional fixedID)
  (let* ((actual-parentID (if (importing?)
                              (+ parentID import-offset-ID)
                              parentID))
         ;; was using make-rule2 before
         ;; now calling make-rule3 so that it is using the version which supports fall-through? saved and loaded
         (new-rule (make-rule3 name type and-or negate? actual-parentID
                               fixedID: (if fixedID
                                            (if (importing?)
                                                (+ fixedID import-offset-ID)
                                                fixedID)
                                            #f)
                               fall-through?: #t ;; defaults to true
                               ))
         (rule-ID (ask new-rule 'ID)))

    (create-typed-rule-common new-rule type actual-parentID)

    ; return the rule ID
    rule-ID))

;; this adds the ability to save fall-through? value 
;; from here on, we shouldn't need to create a new create-typed-rule number for every additional
;; new arguments can be put it at the back with a keyword
(define (create-typed-rule3 name type and-or negate? parentID #!key fixedID fall-through?)
  (let* ((actual-parentID (if (importing?)
                              (+ parentID import-offset-ID)
                              parentID))
         (new-rule (make-rule3 name type and-or negate? actual-parentID
                               fixedID: (if fixedID
                                            (if (importing?)
                                                (+ fixedID import-offset-ID)
                                                fixedID)
                                            #f)
                               fall-through?: fall-through?))
         (rule-ID (ask new-rule 'ID)))

    (create-typed-rule-common new-rule type actual-parentID)

    ; return the rule ID
    rule-ID))


; create a condition - for nodes, retained for backward compatability
(define (create-condition name nodeID operator ruleID #!optional fixedID)
  (create-typed-condition name 0 nodeID operator ruleID fixedID))

; create a condition
(define (create-typed-condition name type targetID operator ruleID #!optional fixedID)
  (let* ((actual-ruleID (if (importing?)
                            (+ ruleID import-offset-ID)
                            ruleID))
         (actual-targetID (if (importing?)
                              (+ targetID import-offset-ID)
                              targetID))
         (new-condition (make-condition2 name type actual-targetID operator actual-ruleID
                                         fixedID: (if fixedID
                                                      (if (importing?)
                                                          (+ fixedID import-offset-ID)
                                                          fixedID)
                                                      #f)))
         (the-rule (get 'rules actual-ruleID)))

    ; add to condition list
    (put 'conditions (ask new-condition 'ID) new-condition)

    ; add condition to rule
    (if the-rule
        (ask the-rule 'add-condition! (ask new-condition 'ID)))))

(define (create-typed-condition2 name type targetID operator ruleID #!key fixedID numfact-args)
  (let* ((actual-ruleID (if (importing?)
                            (+ ruleID import-offset-ID)
                            ruleID))
         (actual-targetID (if (importing?)
                              (+ targetID import-offset-ID)
                              targetID))
         (new-condition (make-condition2 name type actual-targetID operator actual-ruleID
                                         fixedID: (if fixedID
                                                      (if (importing?)
                                                          (+ fixedID import-offset-ID)
                                                          fixedID)
                                                      #f)
                                         numfact-args: numfact-args
                                         ))
         (the-rule (get 'rules actual-ruleID)))

    ; add to condition list
    (put 'conditions (ask new-condition 'ID) new-condition)

    ; add condition to rule
    (if the-rule
        (ask the-rule 'add-condition! (ask new-condition 'ID))))
  )

; create an action
(define (create-action name type expr ruleID #!key fixedID)
  ;;(display "[create-action] expr ")(newline)
  ;(display "expr class ")(display (invoke expr 'get-class))(newline)

  (let* ((actual-ruleID (if (importing?)
                            (+ ruleID import-offset-ID)
                            ruleID))
         (new-action (make-action name type expr actual-ruleID
                                  (if fixedID
                                      (if (importing?)
                                          (+ fixedID import-offset-ID)
                                          fixedID))))
         (the-rule (get 'rules actual-ruleID))
         (new-action-ID (ask new-action 'ID))
         )

    ; add to action list
    (put 'actions new-action-ID new-action)

    ;; debugging import 
;    (if (not (pair? expr))
;        (begin
;          (display "ERROR NOT PAIR!! ")(display ruleID)(newline)
;          (display "expr in create action ")(display expr)(newline)
;          (display "expr class ")(display (invoke expr 'get-class))(newline)
;          ))

    ;; this is for importing actions that need no version conversion 
    ;; those that needs conversion already has the import offset added before conversion
    ;; Note: still need to check whether it is pair for version 2.1
    ;;       all the expr in v2.1 is still in string
;    (if (and (pair? expr)
;             (importing?))
;        (begin
;          (cond ((equal? (car expr) 'follow-link)
;                 ;; import offset for the dest node ID of follow-link action
;                 ;; (follow-link2 linkID parent-ruleID link-type dest-nodeID)
;                 (ask new-action 'set-expr!
;                      (list-replace expr 4 (+ (list-ref expr 4) import-offset-ID))))
;                ((equal? (car expr) 'replace-link-text)
;                 ;; import offset for target linkID's for replace-link-text action
;                 ;;(replace-link-text text-type value linkID)
;                 (ask new-action 'set-expr!
;                      (list-replace expr 3 (+ (list-ref expr 3) import-offset-ID))))
;                )))

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
;         )
        (ask the-rule 'add-action! (ask new-action 'ID))
        )

    new-action-ID))


; create an fact
(define (create-fact name type . args)
  ;(display "create fact ")(display name)(newline)
  (let* ((new-fact (make-fact name type
                              (if (pair? args)
                                  (if (importing?)
                                      (+ (car args) import-offset-ID)
                                      (car args)))))
         (fact-ID (ask new-fact 'ID)))


    (put 'facts fact-ID new-fact) ;; add to facts list
    fact-ID))                     ;; and return the ID

