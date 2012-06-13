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

(require "../kawa/ui/component.scm")
(require "../kawa/ui/container.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/ui/events.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/scrollpane.scm")
(require "../kawa/ui/label.scm")
(require "../kawa/ui/button.scm")
(require "../kawa/ui/checkbox.scm")
(require "../kawa/ui/combobox.scm")
(require "../kawa/ui/tabpanel.scm")
(require "../kawa/ui/undo.scm") ;; debug compoundundomanager-updatelevel 

(require "../kawa/system.scm") ; is-windows?
(require "../kawa/strings.scm") ;; to-string
(require "../kawa/color.scm") ;; make-colour-rgb
(require "../kawa/geometry.scm") ;; ;; debug (get-dimension-width)

(require "../common/objects.scm") ;; ask
(require "../common/datatable.scm") ;; get, table-map
(require "../common/main-ui.scm") ; for get-main-ui-frame
(require "../common/list-helpers.scm") ;; list-replace, list-insert

(require "config-options.scm")
(require "datastructure.scm")
(require "hteditor.scm")
(require "hypedyn-undo.scm")
(require "nodeeditor.scm") ;; nodeeditor-save
(require "htfileio.scm") ;; loaded-file-version
(require "rules-manager.scm")
(require 'list-lib) ;; list-index

; export
(module-export doeditlink doeditnoderule doeditdocrule
               create-editlink-dialog
               ;create-if-condition-panel
               ;create-actions-main-panel
               create-update-text-action-panel
               
               add-follow-link-rule-display remove-follow-link-rule-display
               add-link-display remove-link-display
               
               edit-mode ;; used by rules-manager so far
               delete-action ;; just for ht-editor.scm's dodelnode
               
               edited-linkID
               )
               

; remember which link we're editing
(define edited-linkID '())
;;(define (get-edited-linkID) edited-linkID)

;; keep track of which rule we're editing
(define edited-ruleID '())

; remember currently edited node
;(define-private edited-nodeID '()) ;; now using nodeeditor.scm's edited-nodeID

; are we editing a link (default) or just a rule?
(define edit-mode 'link)

;;;; doedit

; edit a link - called from hteditor.scm
;; in-default-link-text not used any more
;; NOTE: this should be renamed as doeditrule now
;;       since it targets one rule inside the link
(define (doeditlink selected-linkID in-edited-nodeID in-ruleID);in-default-link-text)
  
  ;; cache a version of the unedited link for creation of undo
  (before-edit-rule in-ruleID)
  
  ; set titles
  ;; Note: not used at the moment
;  (set-tabpanel-label-at editlink-dialog-tabpanel 0 "Link")
;  (set-tabpanel-label-at editlink-dialog-tabpanel 1 "Then action")
;  (set-tabpanel-label-at editlink-dialog-tabpanel 2 "Else action")
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  
  ; remember what we're editing
  (set! edited-linkID selected-linkID)
  (set! edited-nodeID in-edited-nodeID)
  (set! edited-ruleID in-ruleID)
  
  ; get all the details from the link object
  (let* ((link-obj (get 'links edited-linkID))
         (link-name (ask link-obj 'name))
         )
    
    ; show link name
    (set-dialog-title 
     editlink-dialog
     (string-append
      "Edit link: "
      link-name
      (if (show-IDs?)
          (string-append " (" (number->string edited-linkID) ")")
          "")))
    
    ;; add appropriate panels to editlink-dialog
    (clear-container editlink-panel-top)
    (add-component editlink-panel-top (create-rules-rename-panel in-ruleID))
    (add-component editlink-panel-top (create-if-condition-panel))
    (add-component editlink-panel-top (create-actions-main-panel))
    
    ;; TODO: convert this condition to be meaningful in the new framework
    ;;       so we should not allow follow-link action to have none?
    ; set "ok" button state - cannot allow a "use" checkbox to be checked and a link to be "none"
;    (set-button editlink-panel-buttons-ok
;          (not
;           (or
;            (and link-uselink (eq? link-dest1 -1))
;            (and link-usealtlink (eq? link-dest2 -1)))))
    
    ;; reset combobox to contain all action-type-list
    (reset-action-type-choice 'link)
    (populate-rule-editor in-ruleID)
    )
    
  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog editlink-dialog-parent)
  (set-component-visible editlink-dialog #t)
  )

; edit a node rule - removes the link-specific portions of the editor
(define (doeditnoderule in-edited-nodeID in-ruleID)
  
  (nodeeditor-save) ;; save text content before create sexpr in before-editnode
  (before-edit-rule in-ruleID)
  
  ; set titles
;  (set-tabpanel-label-at editlink-dialog-tabpanel 0 "Rule")
;  (set-tabpanel-label-at editlink-dialog-tabpanel 1 "Before action")
;  (set-tabpanel-label-at editlink-dialog-tabpanel 2 "After action")
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  
  ; remember which link we're editing ie. none
  (set! edited-linkID '())
  
  ; remember which node we're editing
  (set! edited-nodeID in-edited-nodeID)
  
  ;; remember the rule we're editing
  (set! edited-ruleID in-ruleID)
  
  ; get the rule from the node
  (let* ((edited-node (get 'nodes edited-nodeID))
         ;;(selected-rule (ask edited-node 'rule))
         (node-name (ask edited-node 'name))
         ;;(anywhere (ask edited-node 'anywhere?))
         )

    ;; show node name
    (set-dialog-title editlink-dialog (string-append "Edit rule for node: " node-name))
    )

  ;; add if panel to editlink-dialog
  (clear-container editlink-panel-top)
  (add-component editlink-panel-top (create-rules-rename-panel in-ruleID))
  (add-component editlink-panel-top (create-if-condition-panel))
  (add-component editlink-panel-top (create-actions-main-panel))

  ;; reset combobox to contain all action-type-list
  (reset-action-type-choice 'node)

  (populate-rule-editor in-ruleID)

  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog editlink-dialog-parent)
  (set-component-visible editlink-dialog #t) ;; we're using the same dialog as editlink
  )

; edit document rule
; for now, just one action that is run at start
(define (doeditdocrule)
  (format #t "doeditdocrule~%~!")
  
  ; set titles
  (set-tabpanel-label-at editlink-dialog-tabpanel 0 "Step rule")
  (set-tabpanel-label-at editlink-dialog-tabpanel 1 "Step action")
  (set-tabpanel-label-at editlink-dialog-tabpanel 2 "Init action")
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  (display "[reset-rule-editor] inside doeditdocrule ")(newline)
  
  ; remember which link we're editing ie. none
  (set! edited-linkID '())
  
  ; remember which node we're editing ie. none
  (set! edited-nodeID '())
  
  ; get the rule
  (let ((selected-rule (get-document-ruleID)))
    ; show node name
    (set-dialog-title editlink-dialog "Edit document rule")

    ; reconstruct rule if necessary
    (populate-rule-editor selected-rule))

  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog (get-main-ui-frame))
  (set-component-visible editlink-dialog #t))

;;
;;;; rule edit dialog components
;;
(define editlink-dialog-parent #f)
(define editlink-dialog #f)
(define editlink-dialog-pane #f)

(define editlink-panel-top #f)
;(define editlink-panel-if #f)
(define editlink-panel-follow #f)

(define editlink-panel-main-buttons #f)
(define add-condition-button #f) ;; add conditions
(define delete-condition-button #f) ;; delete conditions

(define editlink-panel-buttons #f)
(define editlink-panel-buttons-cancel #f)
(define editlink-panel-buttons-ok #f)

(define condition-scrollpane-vp-width #f)
(define action-scrollpane-vp-width #f)


;; if condition panel (init editlink-panel-if)
(define (create-if-condition-panel)

  ;; start of IF conditions

  ; panel for "if" part of rule
  (set! editlink-panel-if (make-panel))
  (set-container-layout editlink-panel-if 'vertical)
  
  ;; will add when we need it
  ;(add-component editlink-panel-top editlink-panel-if)

  ; panel for "all"/"any" selection
  (set! editlink-panel-follow (make-panel))
  (set-container-layout editlink-panel-follow 'flow 'left)
  (add-component editlink-panel-if editlink-panel-follow)

  ;; obsolete ("IF" becomes a combobox that has UNLESS as the opposite of IF) 
;  (set! editlink-dialog-operater-label (make-label-with-title "IF"))
;  (add-component editlink-panel-follow editlink-dialog-operater-label)
  
  ;; "if" / "unless" selection
  (set! editlink-dialog-negate-operator (make-combobox "If" "Unless"))
  (add-component editlink-panel-follow editlink-dialog-negate-operator)
  
  ; "all"/"any" selection
  (set! editlink-dialog-andor-operator (make-combobox "All" "Any"))
  (add-component editlink-panel-follow editlink-dialog-andor-operator)
  
  ; label for "of the following conditions are true:"
  (set! editlink-dialog-label (make-label))
  (set-text editlink-dialog-label "of the following conditions are true:")
  (add-component editlink-panel-follow editlink-dialog-label)

  ;;condition panel
  (set! condition-list-panel (make-panel))
  (set-container-layout condition-list-panel 'vertical)
  ;(add-component editlink-panel-if condition-list-panel)
  
  ;; container scrollpane for condition panel
  (define condition-scrollpane (make-scrollpane-with-policy condition-list-panel 'needed 'needed))
  (set-component-preferred-size condition-scrollpane 500 140)
  
  (set! condition-scrollpane-vp-width (scroll-viewport-width condition-scrollpane))
  (add-component editlink-panel-if condition-scrollpane)
  
  ;; Add a horizontal panel to the dialog, with centering for buttons
  (set! editlink-panel-main-buttons (make-panel))
  (set-container-layout editlink-panel-main-buttons 'horizontal)
  (add-component editlink-panel-if editlink-panel-main-buttons)

  ;; Add ADD button
  (set! add-condition-button (make-button "Add condition"))
  (add-actionlistener add-condition-button
                      (make-actionlistener (lambda (source)
                                             (add-condition-callback))))
  (add-component editlink-panel-main-buttons add-condition-button)

  ;; Add DELETE button
  (set! delete-condition-button (make-button "Delete selected"))
  (add-actionlistener delete-condition-button
                      (make-actionlistener (lambda (source)
                                             (delete-condition-callback))))
  (set-component-enabled delete-condition-button #f)
  (add-component editlink-panel-main-buttons delete-condition-button)
  
  editlink-panel-if) ;; end of IF conditions

; user clicked "ok" button on the edit rule UI
;; NOTE: we now recycle the rule object
;; TODO: Recycle objects (we don't recycle condition and action objects atm)
;;       Action: since the 'expr is just a sexpr we can just set it, 
;;               however the action object might have no relations to the new action 
;;               there is no easy way to look at the UI and say which actions aren't changed atm
;;       Condition : mechanics need changing since it stores the information in different variable (perhaps switch to sexpr)
;;                   same as actions, there is no way to check which conditions are same compared to the previous
;;                   so that we can reuse it (not sure if it is worth the trouble)

;;       with abstraction I can see how one condition, action, rule can be assigned to many places
;;       but it create some programming problem because each of these rule object have different parents
;;       and condition and action have reference to a parent ruleID

(define (edit-rule-confirm)
  (let* ((rule-parent-obj (cond ((eq? edit-mode 'link) (get 'links edited-linkID))
                                ((eq? edit-mode 'node) (get 'nodes edited-nodeID))
                                ((eq? edit-mode 'doc) #f)))
         (new-rulename (get-new-rule-name))
         
         ; get boolean operator
         (new-rule-and-or-pos (get-combobox-selectedindex editlink-dialog-andor-operator))
         (new-rule-and-or (get-rule-exp new-rule-and-or-pos))
         
         (negate-pos (get-combobox-selectedindex editlink-dialog-negate-operator))
         (negate? (case negate-pos
                    ((0) #f)
                    ((1) #t)))
         ; create the rule
;         (new-ruleID (create-typed-rule2 new-rulename edit-mode new-rule-and-or negate?
;                                       (cond ((eq? edit-mode 'link) edited-linkID)
;                                             ((eq? edit-mode 'node) edited-nodeID)
;                                             ((eq? edit-mode 'doc) -1))))
         
         ; get the actions, if any
         ;(then-action-string (get-text editlink-dialog-then-actiontext))
         ;(else-action-string (get-text editlink-dialog-else-actiontext))
         )

    (display "and-or ")(display new-rule-and-or)(newline)
    (display "negate? ")(display negate?)(newline)
    
    ;; remove the line associated with this rule before we edit it
    (if (eq? edit-mode 'link)
        (remove-follow-link-rule-display edited-ruleID))  ;; remove the line display from the previous edited-ruleID
    (if (eq? edit-mode 'link)
        (remove-show-popup-rule-display edited-ruleID))  ;; remove the line display from the previous edited-ruleID
    
    
    (define the-rule (get 'rules edited-ruleID))
    
    ;; empty current rule's contents before making the changes
    (if the-rule
        (ask the-rule 'empty-rule))
    
    ;; set the and-or and negate? properties
    (if the-rule
        (begin
          (ask the-rule 'set-and-or! new-rule-and-or)
          (ask the-rule 'set-negate! negate?)))
    
    ;; set the new name of the rule
    (if the-rule
        (ask the-rule 'set-name! new-rulename))
    
    ;; Conditions
    ; run through conditions and add to rule
    (map (lambda (panel) 
;           (let* ((children (get-container-children panel))
;                  (select-type (list-ref children 0))  ;; link node fact combobox
;                  (select-target (list-ref children 1)) ;; list of existent objects of select-type
;                  (select-operator (list-ref children 2)) ;; options specific to select-type (ie if type is link then it is followed/not followed)
;                  (the-type (if (not (is-basic-mode?)) (get-combobox-selectedindex select-type) 0))
;                  (targetID (get-comboboxwithdata-selecteddata select-target))
;                  (operator (get-combobox-selectedindex select-operator)))
             ;(create-typed-condition new-rulename the-type targetID operator edited-ruleID)
             (create-typed-condition new-rulename
                                     (condition-panel-target-type panel)
                                     (condition-panel-target-id panel)
                                     (condition-panel-operator panel)
                                     edited-ruleID)
             )
         (condition-panel-list))
    
    ;; then-action-string, else-action-string
    
    ; if there's an action, add the action to the rule
    ; need to read the string and break in to s-expressions; should eventually
    ; be able to plug in an improved version of definition-editor here
    ; NOTE: I haven't decided if actions should be stored as strings (currently)
    ; or as s-expressions (commented out) - alex
    ; strings: can store formatting
    ; s-expr: seems more appropriate, but some problems with string values?
;    (if (not (equal? then-action-string ""))
;        (create-action new-rulename 
;                       (cond ((eq? edit-mode 'link) 'then)
;                             ((eq? edit-mode 'node) 'before)
;                             ((eq? edit-mode 'doc) 'step))
;                       then-action-string new-ruleID))
;    (if (not (equal? else-action-string ""))
;        (create-action new-rulename 
;                       (cond ((eq? edit-mode 'link) 'else)
;                             ((eq? edit-mode 'node) 'after)
;                             ((eq? edit-mode 'doc) 'init))
;                       else-action-string new-ruleID))
;;        (let ((explist (open-input-string then-action-string)))
;;          (read-action-expr 'then explist new-rulename new-ruleID)))
;;        (let ((explist (open-input-string else-action-string)))
;;          (read-action-expr 'else explist new-rulename new-ruleID)))

    ;;=========
    ;; Actions
    ;;=========
    (define obj-name (ask rule-parent-obj 'name))

    ;; map through the list of action panel and add the actions to the rule
    (map (lambda (action-panel)
           (define action-type (get-action-panel-type action-panel))
           ;; Update Text Action
           (display "action type in confirm ")(display action-type)(newline)
           (cond ((equal? action-type "update text using")
                  ;; text of fact?
                  (define text-or-fact (get-combobox-selecteditem action-type-combobox))
                  (define text-type #f)
                  (define text-value #f)
                  (case (to-string text-or-fact)
                    (("alternative text")
                     (display "came into alt ")(newline)
                     (set! text-type "alternative text")
                     (set! text-value (get-text alt-text-textfield)))
                    (("string fact")
                     (display "came into str fact ")(newline)
                     (set! text-type "string fact")
                     ;; TOFIX: get fact string during runtime instead of from the start
                     ;; (might not need to) just need to store factID
                     ;; problem is fact is not accessible to our interpreter since it is in a different environment
                     (define factID (get-comboboxwithdata-selecteddata fact-string-choice-combobox))
                     (set! text-value factID)
                     )
                    (("number fact")
                     (display "came into num fact ")(newline)
                     (set! text-type "number fact")
                     (define factID (get-comboboxwithdata-selecteddata fact-number-choice-combobox))
                     (set! text-value factID)
                     )
                    )
                  (create-action obj-name 'displayed-node
                                 (list 'replace-link-text
                                       ;;(list 'quote text-type)
                                       (to-string text-or-fact)
                                       text-value
                                       edited-linkID)
                                 edited-ruleID))
                 ;; Follow Link action
                 ((equal? action-type "follow link to")
                  (define dest-nodeID (get-comboboxwithdata-selecteddata node-choice-combobox))

                  (create-action obj-name 'clicked-link
                                 (list 'follow-link
                                       edited-linkID
                                       edited-ruleID
                                       (list 'quote 'default)
                                       dest-nodeID)
                                 edited-ruleID))
                 ;; Update Fact action
                 ((equal? action-type "update fact")

                  (define fact-panel-children (get-container-children action-panel))
                  ;; the components that follows the update fact label in the action panel
                  (define component-list (get-container-children (list-ref fact-panel-children 1)))  ;; was caddr

                  ;; dd - dropdown
                  (define dd1 (car component-list))
                  (define dd2 (list-ref component-list 1))

;                  (define fact-type
;                    (case (to-string (get-combobox-selecteditem dd1))
;                      (("True/False") 'boolean)
;                      (("Text") 'text)
;                      (("Number") 'number)
;                      (else 'error)))
                  (define fact-type (to-string (get-combobox-selecteditem dd1)))

                  (define factID (get-comboboxwithdata-selecteddata dd2))
                  (display "factID ")(display factID)(newline)

                  ;; node rule and link rule share only update fact action for now
                  ;; update fact is triggered by different event in the two kinds of rules
                  (define event-type (case edit-mode
                                       ((link) 'clicked-link)
                                       ((node) 'entered-node)))

                  ;; TODO: move away from these action names. use more understandable names like
                  ;; set-boolean-fact, set-string-fact, set-number-fact
                  (cond ((equal? fact-type "True/False")
                         (define value-dd (list-ref component-list 2))
                         (define bool-val-selected (to-string (get-combobox-selecteditem value-dd)))

                         (define bool-operator
                           (cond ((equal? bool-val-selected "True") 'assert)
                                 ((equal? bool-val-selected "False") 'retract)))

                         (create-action obj-name event-type
                                        (list bool-operator
                                              factID)
                                        edited-ruleID
                                        ))
                        ((equal? fact-type "Text")
                         (define fact-tf (list-ref component-list 2))
                         (create-action obj-name event-type
                                        (list 'set-value!
                                              factID
                                              (get-text fact-tf))
                                        edited-ruleID)
                         )
                        ((equal? fact-type "Number")
                         (define num-fact-mode-dd (list-ref component-list 3))
                         
                          ;; component 3 can be a textfield/dropdown/math-panel depending on what is selected in dd2
                         (define comp3 (list-ref component-list 4))
                         (define num-fact-mode (to-string (get-combobox-selecteditem num-fact-mode-dd)))
                         
                         (define new-fact-value
                           (case num-fact-mode
                             (("Input") (get-text comp3))
                             (("Fact") (get-comboboxwithdata-selecteddata comp3))
                             (("Math")  ;; hardcoded math op
                              (display "[new fact value MATH]")(newline)
                              
                              ;; layout of a math panel
                              ;; [[operand1-type-dd operand1-choice] operator-dd [operand2-type-dd operand2-choice] ]
                              ;; operand choice can be a text field or drop down depending on the current selection in operand type dd 
                              (define math-panel-lst (get-container-children comp3))
                              (define operand1-panel (car math-panel-lst))
                              (define operator-dd (cadr math-panel-lst))
                              (define operand2-panel (caddr math-panel-lst))
                              
                              (define operand1-panel-lst (get-container-children operand1-panel))
                              (define operand2-panel-lst (get-container-children operand2-panel))
                              
                              (define operand1-type (to-string (get-combobox-selecteditem (list-ref operand1-panel-lst 0))))
                              (define operand2-type (to-string (get-combobox-selecteditem (list-ref operand2-panel-lst 0))))
                              (define operator (to-string (get-combobox-selecteditem operator-dd)))
                              
                              (define operand1 
                                (case operand1-type
                                  (("Input") (get-text (list-ref operand1-panel-lst 1)))
                                  (("Fact") (to-string (get-comboboxwithdata-selecteddata (list-ref operand1-panel-lst 1))))))
                              
                              (define operand2
                                (case operand2-type
                                  (("Input") (get-text (list-ref operand2-panel-lst 1)))
                                  (("Fact") (to-string (get-comboboxwithdata-selecteddata (list-ref operand2-panel-lst 1))))))
                              
                              (display "operand1-type ")(display operand1-type)(newline)
                              (display "operand2-type ")(display operand2-type)(newline)
                              
                              (display "operand1 ")(display operand1)(newline)
                              (display "operand2 ")(display operand2)(newline)
                              (display "operator ")(display operator)(newline)
                              
                              ;; conversion of visual to actual programming function
                              (if (equal? operator "x")
                                  (set! operator "*"))

                              (define new-fact-value-expr
                                (list operator
                                      operand1 operand1-type
                                      operand2 operand2-type))
                              new-fact-value-expr)
                             ))
                         
                         ;; operand1 operator operand2
                         ;; operand can be num or fact
                         ;; operator can be + - *
                         ;; javascript form shd look like 
                         ;; 4 + factlist[factID].value
                         
                         (create-action obj-name event-type
                                        (list 'set-number-fact
                                              factID
                                              num-fact-mode
                                              new-fact-value)
                                        edited-ruleID)
                         )
                        ) ;; end of fact-type cond
                  )
;                 
                 ;; no parameter from the ui
                 ((equal? action-type "enable links to this node from anywhere")
                  (create-action "Enable Link" 'anywhere-check
                               (list 'add-anywhere-link edited-nodeID)
                               edited-ruleID))
                 ((equal? action-type "show in popup")
                  (define target-nodeID (get-comboboxwithdata-selecteddata node-choice-combobox2))
                  (display "edited linkID on confirm ")(display edited-linkID)(newline)
                  (create-action "Show in Popup" 'clicked-link
                               (list 'show-in-popup target-nodeID)
                               edited-ruleID)
                  )
                 ) ;; end of action-type cond
           ) (action-panel-list))

    ;; cache the information of the edited link in the form of a lambda object
    (after-edit-rule edited-ruleID)

    ;; added an undoable event 
    (post-edit-rule-undoable-event
     edit-mode
     (case edit-mode
       ((link) edited-linkID)
       ((node) edited-nodeID))
     edited-ruleID)

    ;; reflect changes in node-graph 
    ;; only need to do for links that have follow link actions
    (if (eq? edit-mode 'link)
        (add-follow-link-rule-display edited-ruleID))
    (if (eq? edit-mode 'link)
        (add-show-popup-rule-display edited-ruleID))

    ;;===============
    ;; End of Actions
    ;;===============
    
    ;; reflect changes in rule manager
    (rmgr-update)
    
    ; hide link editor, and reset (for next time)
    (set-component-visible editlink-dialog #f)
    (reset-rule-editor)
    ))

; reset the rule editor
(define (reset-rule-editor)
  
  (if condition-list-panel
      (clear-container condition-list-panel)) ;; empty conditions panel
  
  (if action-list-panel
      (clear-container action-list-panel))    ;; empty actions panel
  
  ; if facts are showing, reset the alt text to text, not fact
;  (if (show-facts?)
;      (set-combobox-selection editlink-panel-else-text-typechoice 0))

  (if editlink-dialog-andor-operator
      (set-combobox-selection editlink-dialog-andor-operator 0))
  )

;; check whether rule is valid or not then
;; enable the ok button accordingly

(define (validate-rule #!optional e)
  (set-component-enabled 
   editlink-panel-buttons-ok 
   (and (all-conditions-valid?)
        (all-actions-valid?))))

;;;; action and condition panels
;(define actions-main-panel #f)
(define action-list-panel #f)
(define action-type-choice #f)

(define condition-list-panel #f)
(define editlink-dialog-negate-operator #f) ;; If/Unless combobox
(define editlink-dialog-andor-operator #f) ;; Any/All combobox
(define editlink-dialog-label #f) ;; "of the following conditions are true:" message label

(define node-choice-combobox #f) ;; for follow link
(define node-choice-combobox2 #f) ;; for popup

;; remove all the condition and action panels from the condition action editing panel
;; obj-type : 'node 'link 'anywhere 'doc
(define (reset-action-type-choice obj-type)
  (set-combobox-clear action-type-choice)
  
  (define action-type-list '())
  
  (cond ((equal? obj-type 'link)
         (set! action-type-list action-type-list-link))
        
        ((equal? obj-type 'node)
         (set! action-type-list action-type-list-node))
         )
  
  ;; reset the available actions type in the combobox
  (display "RESETTING ACTION TYPE CHOICE ")(newline)
  (map (lambda (action-type)
         (add-combobox-string action-type-choice action-type))
       action-type-list)
  )

;; returns a list of the action panels in action-list-panel
(define (action-panel-list)
  (get-container-children action-list-panel))

(define (get-selected-action-panel)
  (filter panel-selected? (action-panel-list)))

(define (condition-panel-list)
  (get-container-children condition-list-panel))

(define (get-selected-condition-panel)
  (filter panel-selected? (condition-panel-list)))

(define (action-panel-restrict)
  ;; enable add button when 0 or 1 action panel selected 
  (set-component-enabled add-action-button (not (> (length (get-selected-action-panel)) 1)))
  
  ;; enabled delete when at least 1 action panel selected
  (set-component-enabled delete-action-button (> (length (get-selected-action-panel)) 0))
  )

;;;; action panel operations

(define (get-action-panel-type panel)
  (let* ((children (get-container-children panel))
         (action-name-label (list-ref children 0)))
    (get-text action-name-label)
    ))

(define (update-fact-panel-valid? panel)
  (let* ((children (get-container-children panel))
         (fact-panel (list-ref children 1))
         (fp-children (get-container-children fact-panel))
         ;; in the middle of implementing number facts
         (fact-type-cb (list-ref fp-children 0))

         (target-cb
          (case (to-string (get-combobox-selecteditem fact-type-cb))
            (("True/False") (list-ref fp-children 1))
            (("Text") (list-ref fp-children 1))
            (("Number") (list-ref fp-children 1))
            )))
    
    (case (to-string (get-combobox-selecteditem fact-type-cb))
      (("True/False") (not (= (get-comboboxwithdata-selecteddata target-cb) -1)))
      (("Text") (not (= (get-comboboxwithdata-selecteddata target-cb) -1)))
      (("Number")
       (let* ((num-fact-mode-cb (list-ref fp-children 3))
              (num-fact-mode (get-combobox-selecteditem num-fact-mode-cb)))
         (case (to-string num-fact-mode)
           (("Input")
            (and (not (= (get-comboboxwithdata-selecteddata target-cb) -1))
                 (string-is-numeric? (get-text (list-ref fp-children 4)))
                 ))
           (("Fact")
            (and (not (= (get-comboboxwithdata-selecteddata target-cb) -1))
                 ;; source fact (value from which we're getting from) is selected (not none)
                 (not (= (get-comboboxwithdata-selecteddata (list-ref fp-children 4)) -1)))
            )
           (("Math")
            (define (operand-panel-valid? panel)
              (let ((comp-lst (get-container-children panel)))
                (case (to-string (get-combobox-selecteditem (car comp-lst)))
                  (("Input") (string-is-numeric? (get-text (cadr comp-lst))))
                  (("Fact") (not (= (get-comboboxwithdata-selecteddata (cadr comp-lst)) -1)))
                )))
            
            (let* ((math-panel (list-ref fp-children 4))
                   (math-panel-lst (get-container-children math-panel))
                   (operand1-panel (list-ref math-panel-lst 0))
                   (operand2-panel (list-ref math-panel-lst 2)))
              (and (operand-panel-valid? operand1-panel)
                   (operand-panel-valid? operand2-panel))))
           )
       ))) ;; end of case
    ))

(define (update-text-using-panel-valid? panel)
  (let ((selected-item (to-string (get-combobox-selecteditem action-type-combobox))))
    (case selected-item
      (("alternative text") #t) ;; no checking required
      (("string fact") 
       (not (= (get-comboboxwithdata-selecteddata fact-string-choice-combobox) -1)))
      (("number fact") 
       (not (= (get-comboboxwithdata-selecteddata fact-number-choice-combobox) -1)))
      )))

;; show in popup and follow link checks does the same thing
(define follow-link-to-panel-valid? show-in-popup-panel-valid?)
(define (show-in-popup-panel-valid? panel)
  (let* ((children (get-container-children panel))
         (target-cb (list-ref children 1)))
    (not (= (get-comboboxwithdata-selecteddata target-cb) -1))
    ))

(define (action-panel-valid? panel)
  (case (get-action-panel-type panel)
    (("update fact") (update-fact-panel-valid? panel))
    (("follow link to") (follow-link-to-panel-valid? panel))
    (("update text using") (update-text-using-panel-valid? panel))
    (("show in popup") (show-in-popup-panel-valid? panel))
    (else (display "[action panel valid?] new type ")(display (get-action-panel-type panel))(newline) #f))
  )

(define (all-actions-valid?)
  (define (my-and lst)
    (if (null? lst) 
        #t
        (and (car lst) 
             (my-and (cdr lst)))))
  
  (my-and (map action-panel-valid?
               (action-panel-list)))
  )

;; add an action to the action list of type action-type ["update text using", "follow link to", "update fact"]
;; args-lst null means we're adding a new empty action panel,
;; if args-lst not null load the information provided (present existing action) 
(define (add-specific-action action-type . args-lst)
  (display "[add-specific-action] ")(newline)
  (display "  action type ")(display action-type)(newline)
  (display "  args-lst ")(display args-lst)(newline)
  
  (define panel-to-return (create-action-panel action-type))
  (set-container-layout panel-to-return 'horizontal)
  (set-component-align-x panel-to-return 'left)
  
  ;; alter the configuration of the ui objects if args-lst given
  (cond ((equal? action-type "update text using")
         
         (if (= (length args-lst) 2)
             (let ((using-type (car args-lst))
                   (alt-text (cadr args-lst)))
               (display "args lst ")(display using-type)(newline)
               (set-combobox-selection-object action-type-combobox (create-combobox-string-item using-type))

               (cond ((equal? using-type "alternative text")
                      (set-text alt-text-textfield alt-text)
                      )
                     ((equal? using-type "string fact")
                      (define target-fact (get 'facts alt-text))
                      (define fact-name (ask target-fact 'name))

                      (define for-selection (create-combobox-string-item fact-name))
                      (set-combobox-selection-object fact-string-choice-combobox for-selection)
                      )
                     ((equal? using-type "number fact")
                      (define target-fact (get 'facts alt-text))
                      (define fact-name (ask target-fact 'name))

                      (define for-selection (create-combobox-string-item fact-name))
                      (set-combobox-selection-object fact-number-choice-combobox for-selection)
                      )
                     )
               )
             ;; if this is a new action, just reset fact type selection to alternative text
             (begin
               (set-combobox-selection-object action-type-combobox (create-combobox-string-item "alternative text"))
               ))
         (pack-component update-text-action-panel)
         (add-component panel-to-return update-text-action-panel)
         )
        ((equal? action-type "follow link to")
         (if (= (length args-lst) 1)
             (let* ((link-dest1 (car args-lst))
;                    (dest-node-name (if link-dest1
;                                        (ask (get 'nodes link-dest1) 'name)
;                                        ))
                    )
               (set! node-choice-combobox (create-node-choice link-dest1 #t -1))
               
               ;;
               ;(set-combobox-selection-object node-choice-combobox (create-combobox-string-item dest-node-name))
               )
             (set! node-choice-combobox (create-node-choice #f #t -1)))
         
         (add-actionlistener
          node-choice-combobox
          (make-actionlistener
           validate-rule))
          
         (pack-component node-choice-combobox)
         (add-component panel-to-return node-choice-combobox)
         )
        ((equal? action-type "update fact")
         (cond ((= (length args-lst) 3)
                (let ((the-action (car args-lst))
                      (targetID (cadr args-lst))
                      (the-value (caddr args-lst)))
                  (add-component panel-to-return (create-fact-panel the-action targetID the-value))))
               ((= (length args-lst) 4)
                (let ((the-action (car args-lst))
                      (targetID (cadr args-lst))
                      (num-fact-mode (caddr args-lst))
                      (the-value (cadddr args-lst)))
                  (add-component panel-to-return (create-fact-panel the-action targetID the-value num-fact-mode: num-fact-mode)))
                )
             (else (add-component panel-to-return (create-fact-panel #f #f #f)))
         ))
        ((equal? action-type "enable links to this node from anywhere")
         ;; disable the checkbox of this action (so it can't be deleted)
         (set-component-enabled (car (get-container-children panel-to-return)) #f)
         )
        ((equal? action-type "show in popup")
         (if (= (length args-lst) 1)
             (set! node-choice-combobox2 (create-node-choice (car args-lst) #t -1))
             (set! node-choice-combobox2 (create-node-choice #f #t -1)))
         (add-actionlistener
          node-choice-combobox2
          (make-actionlistener
           validate-rule))
         (pack-component node-choice-combobox2)
         (add-component panel-to-return node-choice-combobox2))
        )
  
  (set-border panel-to-return bevel-in-border)
  (set-component-non-resizable-size panel-to-return 650 50) ;;action-scrollpane-vp-width was 480 for width
  
  (add-mouselistener
     panel-to-return
     (make-mouselistener
      (lambda (e)
        (if (equal? (get-mouseevent-type e) 'left-clicked)
            (begin
              ;; if control key not held down unselect the others
              (if (not (ctrl-key-down? (get-mouseevent-rawevent e)))
                  (map (lambda (pnl)
                         (if (not (equal? pnl panel-to-return))
                             (select-condition-panel pnl #f)))
                       (action-panel-list))
                  )

              (select-condition-panel panel-to-return (not (panel-selected? panel-to-return)))
              (action-panel-restrict)
              ))
        )))
  
  panel-to-return)

;; called when add action button is pressed
(define (add-action-callback source)
  (display "add action! ")(newline)
  
    ;; get the selected type
  (define selected-action-type (get-combobox-selecteditem action-type-choice))
  (define new-action-panel (add-specific-action selected-action-type))
  
  (if (= (length (get-selected-action-panel)) 0)
     (begin
      (add-component action-list-panel new-action-panel)
       ))

  (if (= (length (get-selected-action-panel)) 1)
      (begin
        (define index (list-index (lambda (o) (equal? o (car (get-selected-action-panel)))) (action-panel-list)))
        (add-component-at action-list-panel new-action-panel (+ index 1))
        ))
  
  ;; check whether rule is valid and enable ok button
  (validate-rule)
  
  ;(add-component action-list-panel new-action-panel)
  (pack-frame editlink-dialog))

;; called when delete action button is pressed
;; NOTE: this just remove the action-panel in the ui
;;       the changes only comes when you press ok on the edit rule dialog
(define (delete-action-callback source)
  (display "delete action callback ")(newline)
  (map (lambda (action-panel)
         (define comp-lst (get-container-children action-panel))
         ;(define action-checkbox (car comp-lst))
         (define action-label (list-ref comp-lst 0))
         (if (panel-selected? action-panel);(get-checkbox-value action-checkbox)
             (begin
               (define action-type (get-text action-label))
               ;; add the choice back if we're deleting a unique action (only a copy of the action should exist)
               (if (member (get-text action-label) unique-choices)
                   (begin
                     (add-combobox-string action-type-choice action-type)
                     
                     ;; these two actions "show in popup" and "follow link to" cannot exists in the same rule
                     ;; add back the action when the action excluding it had been removed
                     (if (equal? action-type "show in popup")
                         (add-combobox-string action-type-choice "follow link to"))
                     (if (equal? action-type "follow link to")
                         (add-combobox-string action-type-choice "show in popup"))
                     ))
               
               ;; remove that panel from action-list-panel
               (remove-component action-list-panel action-panel)
               
               ;; the panel does not disappear even after we do remove-component thus we do this
               (component-update action-list-panel)
               ))
         ) (action-panel-list))
  (action-panel-restrict)
  (validate-rule)
  (pack-frame editlink-dialog))

;; remove an action from the parent rule so that it is never 
;; used anymore. 
;; NOTE: this is used by dodelnode to remove any follow link 
;;       action that has the deleted node as dest node
(define (delete-action actionID)
  
  (define action (get 'actions actionID))
  (define ruleID (ask action 'ruleID))
  (define rule (get 'rules ruleID))
  (define action-sexpr (ask action 'to-save-sexpr))
  
  ;; in case it is a follow link action, we need to update the link display
  (remove-follow-link-rule-display ruleID)
  
  (ask rule 'delaction actionID)
  (del 'actions actionID)
  
  ;; in case it is NOT a follow link action, we need to add the link back
  (add-follow-link-rule-display ruleID)
  
  (compoundundomanager-postedit 
   undo-manager
   (make-undoable-edit 
    "Delete Action"
    (lambda () ;;undo
      (remove-follow-link-rule-display ruleID)
      (eval-sexpr action-sexpr)
      (add-follow-link-rule-display ruleID)
      )
    (lambda () ;; redo
      (define action (get 'actions actionID))
      (define ruleID (ask action 'ruleID))
      (define rule (get 'rules ruleID))
      (define action-sexpr (ask action 'to-save-sexpr))
      (remove-follow-link-rule-display ruleID)
      (ask rule 'delaction actionID)
      (del 'actions actionID)
      (add-follow-link-rule-display ruleID)
      )))
  )

      
(define add-action-button #f)
(define delete-action-button #f)
(define action-scrollpane #f)
(define (create-actions-main-panel)
  
  (set! actions-main-panel (make-panel))
  (set-container-layout actions-main-panel 'border)
  
  (set! add-action-button (make-button "Add Action"))
  (set! delete-action-button (make-button "Delete Selected"))
  (set-component-enabled delete-action-button #f)
  
  ;; label
  (define action-label (make-label-with-title "THEN perform the following actions:"))
  (define action-label-panel (make-panel))
  (set-container-layout action-label-panel 'vertical)
  (add-component action-label-panel action-label)
  (add-component actions-main-panel action-label-panel 'border-north)
  
  ;; list of actions
  (set! action-list-panel (make-panel))
  (set-container-layout action-list-panel 'vertical)
  ;(add-component actions-main-panel action-list-panel 'border-center)
  
    ;; scrollpane for condition panel
  (set! action-scrollpane (make-scrollpane-with-policy action-list-panel 'needed 'needed))
  (set-component-preferred-size action-scrollpane 500 140)
  (set! action-scrollpane-vp-width (scroll-viewport-width action-scrollpane))
  (add-component actions-main-panel action-scrollpane 'border-center)
  ;;(add-component actions-main-panel action-list-panel 'border-center)
  
  ;; action type list contains the remaining available types left (after previous adding of actions)
  (set! action-type-choice (make-combobox))
  
  (define action-type-choice-container (make-panel))
  (add-component action-type-choice-container action-type-choice)

  ;; button panel
  (define actions-buttons-panel (make-panel))
  (set-container-layout actions-buttons-panel 'flow 'center)
  (add-component actions-main-panel actions-buttons-panel 'border-south)
  
  ;; Add action choice
  (add-component actions-buttons-panel add-action-button)
  (add-component actions-buttons-panel action-type-choice-container)  
  (add-component actions-buttons-panel delete-action-button)
  
  ;; action listeners
  (add-actionlistener add-action-button 
                      (make-actionlistener add-action-callback))
  
  (add-actionlistener delete-action-button 
                      (make-actionlistener delete-action-callback))
  actions-main-panel)

;;;; rule rename textfield
(define rule-rename-tf #f)
(define (get-new-rule-name)
  (if rule-rename-tf (get-text rule-rename-tf) "rule-rename-tf not init'd"))
(define (create-rules-rename-panel ruleID)
  (define name-panel (make-panel))
  (add-component name-panel (make-label-with-title "Rule name: "))
  (define rule (get 'rules ruleID))
  (set! rule-rename-tf
        (if rule
            (make-textfield (ask rule 'name) 15)
            (make-textfield "!!rule not found!!" 15)))
  (add-component name-panel rule-rename-tf)
  name-panel)

;;;; action panels
;; kept to ensure some actions are only added once
(define action-type-list-link (list "update text using" "follow link to" "update fact" "show in popup"))
(define action-type-list-node (list "update fact"))
(define-constant unique-choices (list "update text using" "follow link to" "show in popup")) ;; choices that shouldnt be duplicated


;; an instance of the action type selector panel
;; updates the action choice combobox and returns a panel with a name label
(define (create-action-panel action-type) ; the-type )
  (if (not (eq? action-type #!null))
      (let* ((top-panel (make-panel))
             (the-checkbox (make-checkbox "")))
        ;; add top-panel
        (set-container-layout top-panel 'flow 'left)

        ;; Add checkbox
        ;(add-component top-panel the-checkbox)

        ;; add the action label display
        (define action-label (make-label-with-title (to-string action-type)))
        (add-component top-panel action-label)

        (define action-type-str (to-string action-type))
                                        ;"update text using" "follow link to" "update fact"))

        ;; update action-type-choice combobox
        ;; ensure actions that should only have one instance is not available as a choice in the combobox
        (if (member (to-string action-type) unique-choices)
            (begin
              (remove-combobox-string action-type-choice action-type)
              
              ;; these two actions "show in popup" and "follow link to" cannot exists in the same rule
              ;; so restrict adding the other action when one is added
              (if (equal? action-type "show in popup")
                  (remove-combobox-string action-type-choice "follow link to"))
              (if (equal? action-type "follow link to")
                  (remove-combobox-string action-type-choice "show in popup"))
              ))

        top-panel)
      (make-panel)))

; reconstruct a rule
; this is called from the doeditlink/doeditnoderule/doeditdocrule procedures above
; to finish building the GUI representation of the link/rule
(define (populate-rule-editor edited-ruleID)
  (display "edited rule ")(display edited-ruleID)(newline)
  (define rule-obj (get 'rules edited-ruleID))
  (display "rule-obj ")(display edited-ruleID)(newline)
  (if rule-obj
      (begin
        (define conditions (ask rule-obj 'conditions))
        (define actions (ask rule-obj 'actions))
        (define facts (ask rule-obj 'actions))
        (define rule-name (ask rule-obj 'name))
        ;; TODO refresh name when show ID selected
        (if (show-IDs?)
            (string-append rule-name "(" (to-string (ask rule-obj 'ID)) ")"))
        (define expr (ask rule-obj 'and-or))
        
        ;;(define then-action-ID (ask rule-obj 'then-action))
        ;;(define else-action-ID (ask rule-obj 'else-action))
        
        ; set actions in code editor
;        (if then-action-ID
;            (let ((the-action (get 'actions then-action-ID)))
;              (if the-action
;                  (begin
;                    (set-text editlink-dialog-then-actiontext (ask the-action 'expr))
;                    (set-cursor-pos editlink-dialog-then-actiontext 0)))))
;        (if else-action-ID
;            (let ((the-action (get 'actions else-action-ID)))
;              (if the-action
;                  (begin
;                    (set-text editlink-dialog-else-actiontext (ask the-action 'expr))
;                    (set-cursor-pos editlink-dialog-else-actiontext 0)))))

                                        ; set boolean operator "all/any"
        (set-combobox-selection editlink-dialog-andor-operator
                                (get-rule-pos expr)) ; add setting of operator

        ;; added in version 2.2 (pre 2.2 rules would be converted to support this on load)
        (define negate? (ask rule-obj 'negate?)) ;; we can't be sure whether this is old rule or new rule obj
        ;; set negate operator "if/unless"
        (set-combobox-selection editlink-dialog-negate-operator
                                (if negate? 1 0))

                                        ; build conditions
        (map (lambda (mycond)
               (let* ((cond-obj (get 'conditions mycond))
                      (the-type (ask cond-obj 'type))
                      (targetID (ask cond-obj 'targetID))
                      (operator (ask cond-obj 'operator)))
                 (add-component condition-list-panel (create-condition-panel the-type targetID operator -1)))) ;; should allow edited note to be a choice in condition?
             conditions)

                                        ; build actions (show them in the ui)
        (map (lambda (actionID)
               (define action (get 'actions actionID))
               (define action-sexpr (ask action 'expr))

               (cond
                ((equal? 'follow-link (car action-sexpr))
                 (define dest-nodeID (list-ref action-sexpr 4))
                 (add-component action-list-panel (add-specific-action "follow link to" dest-nodeID)))

                ((equal? 'replace-link-text (car action-sexpr))  ;(replace-link-text text-type value linkID)
                 (display "inside replace link text ")(newline)
                 (display "action sexpr ")(display action-sexpr)(newline)
                 (define text-type (list-ref action-sexpr 1)) ;(using-type (car args-lst)) (alt-text (cadr args-lst)))
                 (define text-value (list-ref action-sexpr 2))
                 (display "text type ")(display text-type)(newline)
                 (display "text value ")(display text-value)(newline)

                 ;; 'text maps to "alternative text", 'fact maps to "string fact" 
                 (define text-type-string #f)
;                 (cond ((equal? text-type 'text)
;                        (set! text-type-string "alternative text")
;                        )
;                       ((equal? text-type 'fact)
;                        (set! text-type-string "string fact")
;                        )
;                       ((equal? text-type 'num-fact)
;                        (set! text-type-string "number fact")
;                        )
;                       )
                 (add-component action-list-panel (add-specific-action "update text using" text-type text-value))
                 )
                ((or (equal? 'retract (car action-sexpr))
                     (equal? 'assert (car action-sexpr))
                     (equal? 'set-value! (car action-sexpr))
                     )

                 (define the-action (car action-sexpr))
                 (define targetID (cadr action-sexpr)) ;; factID
                 (define the-value (if (or (eq? the-action 'set-value!)
                                           (eq? the-action 'set-number-fact))
                                       (caddr action-sexpr)
                                       'NA))

                 (add-component action-list-panel (add-specific-action "update fact" the-action targetID the-value))
                 )
                ((equal? 'set-number-fact (car action-sexpr))
                 (define the-action (list-ref action-sexpr 0))
                 (define targetID (list-ref action-sexpr 1)) ;; factID
                 (define num-fact-mode (list-ref action-sexpr 2))
                 (define the-value (list-ref action-sexpr 3))
                 (add-component action-list-panel (add-specific-action "update fact" the-action targetID num-fact-mode the-value))
                 )
                ((equal? 'add-anywhere-link (car action-sexpr))
                 (add-component action-list-panel (add-specific-action "enable links to this node from anywhere"))
                 )
                ((equal? 'show-in-popup (car action-sexpr))
                 (define target-nodeID (list-ref action-sexpr 1))
                 (add-component action-list-panel (add-specific-action "show in popup" target-nodeID))
                 )
                )
               ) actions)
        )))

;;  ===========================
;;;; Update text action panels
;;  ===========================

(define update-text-action-panel #f)
(define alt-text-textfield #f)
(define action-type-combobox #f)
(define fact-string-choice-combobox #f)
(define fact-boolean-choice-combobox #f)
(define fact-number-choice-combobox #f)

;; update the panel UI state for "update text using" action
(define (action-update-text-combobox-callback)

  (define link-obj (get 'links edited-linkID))
  (define link-alttext (ask link-obj 'alt-text))

  (define selected-item (to-string (get-combobox-selecteditem action-type-combobox)))

  (cond ((equal? selected-item "alternative text")
         (clear-container update-text-action-panel)
         (add-component update-text-action-panel action-type-combobox)
         (add-component update-text-action-panel alt-text-textfield)
         )
        ((or (equal? selected-item "string fact")
             (equal? selected-item "number fact"))
         (clear-container update-text-action-panel)
         (add-component update-text-action-panel action-type-combobox)
         
         (display "selected-item class ")(display (invoke selected-item 'get-class))(newline)
         (display "equal? still correct? ")(display (equal? selected-item "string fact"))(newline)
         (display "equal? still correct? ")(display (equal? selected-item "number fact"))(newline)
         ;; string/number fact differentiation
         (define fact-choice
           (case selected-item
             (("string fact") (location fact-string-choice-combobox))
             (("number fact") (location fact-number-choice-combobox))
             (else "HUH")
             ))
         (define fact-type
           (case selected-item
             (("string fact") 'string)
             (("number fact") 'number)))
         
         ;; **** Explanation of how location and setter works 
         ;; location just gets a "pointer" to the variable, it is a getter function of the variable
         ;; eg 
         ;; (define x 1)
         ;; (define lx (location x))
         ;; (display (lx)) ==> 1
         ;; setter when given a location gets the setter function of that 
         ;;   particular variable associated with the location
         ;; ((setter lx) 4)
         ;; (display x) ==> 4
         ;; the pair of function therefore gives a means to pass a variable by reference

         ;; get selection on fact-string-choice-combobox  
         ;; Note: this preserves our fact selection when we toggle between text and fact
         (define fact-selection #f)
         (display "fact-choice ")(display fact-choice)(newline)
         (if (fact-choice)
             (set! fact-selection (get-comboboxwithdata-selecteddata (fact-choice))))
         
         ;; create and add the combobox containing the string facts
         ((setter fact-choice) (create-fact-choice fact-type fact-selection))
         (pack-component (fact-choice))
         
         (add-actionlistener
          (fact-choice)
          (make-actionlistener
           validate-rule
           ))
         
         ;; add to a container panel and add to the update-text-action-panel
         (add-component update-text-action-panel (fact-choice))
         )
        )
  
  ;;(component-update (get-parent update-text-action-panel))
  ;;(component-revalidate (get-parent update-text-action-panel))
  
  (pack-component update-text-action-panel)
  
  (pack-frame editlink-dialog)
  )

;; the panel that comes behind the combobox selecting actions type
(define (create-update-text-action-panel)
  (set! update-text-action-panel (make-panel))
  (set! action-type-combobox (make-combobox "alternative text" "string fact" "number fact"))
  (set! alt-text-textfield (make-textfield "" 20))
  
  (set! fact-string-choice-combobox
    (create-fact-choice 'string -1))
  
  ;; TODO: we should be able to show boolean as well 
  (set! fact-boolean-choice-combobox
    (create-fact-choice 'boolean -1))
  
  (set! fact-number-choice-combobox
    (create-fact-choice 'number -1))
  
  ;; layout
  (set-container-layout update-text-action-panel 'horizontal)
  (pack-component action-type-combobox)
  
  (add-component update-text-action-panel action-type-combobox)
  (add-component update-text-action-panel alt-text-textfield)
  
  ;(set! update-text-combobox-callback (lambda (
  
  (add-actionlistener action-type-combobox 
                      (make-actionlistener 
                       (lambda (e)
                         (validate-rule)
                         (action-update-text-combobox-callback))))
  )

(define (create-editlink-dialog parent)
  ; remember parent
  (set! editlink-dialog-parent parent)
  
  ; create edit link dialog
  (set! editlink-dialog (make-dialog-noparent-modal "Edit link"))
  (set-component-size editlink-dialog 400 100)
  (set-dialog-resizable editlink-dialog (show-actions?))
  (set! editlink-dialog-pane (get-dialog-content-pane editlink-dialog))
  (set-container-layout editlink-dialog-pane 'border)
  
;  (set! editlink-dialog-tabpanel (make-tab-panel))
;  (if (show-actions?)
;      (add-component editlink-dialog-pane editlink-dialog-tabpanel 'border-center))
  
  ;; link
;  (set! editlink-dialog-linkpanel (make-panel))
;  (add-tabpanel-tab editlink-dialog-tabpanel "Link" editlink-dialog-linkpanel)
;  (set-container-layout editlink-dialog-linkpanel 'border)
  
  ;; Add a horizontal panel for conditions and condition buttons
  (set! editlink-panel-top (make-panel))
  (set-container-layout editlink-panel-top 'vertical)
  (if (show-actions?)
      (add-component editlink-dialog-linkpanel editlink-panel-top 'border-center)
      (add-component editlink-dialog-pane editlink-panel-top 'border-center))

  ;;
  ;; Ok/Cancel buttons
  ;;

  ;; Add a horizontal panel to the dialog, with centering for buttons
  (set! editlink-panel-buttons (make-panel))
  (set-container-layout editlink-panel-buttons 'flow 'right)
  (add-component editlink-dialog-pane editlink-panel-buttons 'border-south)
  
  ;; Add Cancel and Ok buttons to the horizontal panel
  (set! editlink-panel-buttons-cancel (make-button " Cancel "))
  (add-actionlistener editlink-panel-buttons-cancel
                      (make-actionlistener (lambda (source)
                                             (edit-rule-cancel))))
  (set! editlink-panel-buttons-ok (make-button "   OK   "))
  (add-actionlistener editlink-panel-buttons-ok
                      (make-actionlistener (lambda (source)
                                             (edit-rule-confirm))))
  (if (is-windows?)
      (begin
        (add-component editlink-panel-buttons editlink-panel-buttons-ok)
        (add-component editlink-panel-buttons editlink-panel-buttons-cancel))
      (begin
        (add-component editlink-panel-buttons editlink-panel-buttons-cancel)
        (add-component editlink-panel-buttons editlink-panel-buttons-ok)))
  
  ; pack
  ;(pack-frame editlink-dialog)
  )

;;  =============
;;;; Node/Link/Fact Choice (combobox)
;;  =============
; add a node choice
; callback will be called when choice changes, and nodeID will be selected
; if excludeAnywhere, will exclude anywhere nodes
; will also exclude edited-nodeID
(define (create-node-choice nodeID excludeAnywhere in-edited-nodeID)
  (let ((node-list (make-sortedcomboboxwithdata)))
;    (format #t "Creating node-choice: nodeID:~a, in-edited-nodeID:~a~%~!" nodeID in-edited-nodeID)

    ; add entries to node choice
    (set-comboboxwithdata-clear node-list)
    (let ((the-nodes (get-list 'nodes)))
      (if the-nodes
          (map (lambda (n)
                 (let* ((thisnodeID (car n))
                        (thisnode (cdr n))
                        (name (ask thisnode 'name))
                        (display-name (if (show-IDs?)
                                          (string-append name
                                                         " ("
                                                         (number->string thisnodeID)
                                                         ")")
                                          name))
                        (anywhere (ask thisnode 'anywhere?)))
                   (if (and (not (eq? in-edited-nodeID thisnodeID))
                            (not (and excludeAnywhere anywhere)))
                       (add-comboboxwithdata-string node-list display-name thisnodeID))))
               the-nodes)))

    ; make sure the "None" entry is explicitly inserted at the start of the list
    (insert-comboboxwithdata-string-at node-list "<None>" -1 0)
    
    ; select the chosen node
    (if nodeID
        (set-comboboxwithdata-selection-bydata node-list nodeID)
        (set-combobox-selection node-list 0))

    ; return the list
    node-list))

; add a link choice
; callback will be called when choice changes, and linkID will be selected
(define (create-link-choice linkID)
  (let ((link-list (make-sortedcomboboxwithdata)))

    ; add entries to link choice
    (set-comboboxwithdata-clear link-list)
    (let ((the-links (get-list 'links)))
      (if the-links
          (map (lambda (l)
                 (let* ((thislinkID (car l))
                        (thislink (cdr l))
                        (name (ask thislink 'name))
                        (display-name (if (show-IDs?)
                                          (string-append name
                                                         " ("
                                                         (number->string thislinkID)
                                                         ")")
                                          name)))
                   (add-comboboxwithdata-string link-list display-name thislinkID)))
               the-links)))

    ; make sure the "None" entry is explicitly inserted at the start of the list
    (insert-comboboxwithdata-string-at link-list "<None>" -1 0)
    
    ; select the chosen link
    (set-comboboxwithdata-selection-bydata link-list linkID)

    ; return the list
    link-list))

; add a fact choice - same as above, refactor! - alex
; creates a choice containing facts of the given type
; callback will be called when choice changes, and factID will be selected
;; if factID is #f select <None>
(define (create-fact-choice the-type factID)
  (let ((fact-list (make-sortedcomboboxwithdata)))

    ; add entries to fact choice - moved this afterwards to allow for adding of data
    (set-comboboxwithdata-clear fact-list)
    (let ((the-facts (get-list 'facts)))
      (if the-facts
          (map (lambda (a)
                 (let* ((thisfactID (car a))
                        (thisfact (cdr a))
                        (name (ask thisfact 'name))
                        (display-name 
                         (if (show-IDs?)
                             (string-append name
                                            " ("
                                            (number->string thisfactID)
                                            ")")
                             name))
                        (thistype (ask thisfact 'type)))
                   (if (or (eq? thistype 'any)
                           (eq? thistype the-type))
                       (add-comboboxwithdata-string fact-list display-name thisfactID))
                   ))
               the-facts)))

    ; make sure the "None" entry is explicitly inserted at the start of the list
    (insert-comboboxwithdata-string-at fact-list "<None>" -1 0)
    (set-combobox-selection fact-list 0) ;; default selection
    
    ;; select the chosen fact (if factID isn't one of the entry, selection isn't changed)
    (if factID
        (set-comboboxwithdata-selection-bydata fact-list factID))
    
    ; return the list
    fact-list))

;;  =============
;;;; conditions
;;  =============

; create a condition panel
; the-type: the type of condition (0=node, 1=link, 2=fact)
; targetID: the currently selected node/link, if any (pass in -1 if none selected)
; selectedOperator: the operator for this condition (visited, not visited, or previous)
(define (create-condition-panel the-type targetID selectedOperator in-edited-nodeID)
  (let* ((top-panel (make-panel))
         ;(the-checkbox (make-checkbox ""))
         (the-type-choice (if (is-basic-mode?)
                              (make-combobox "Node")
                              (if (show-facts?)
                                  (make-combobox "Node" "Link" "Fact")
                                  (make-combobox "Node" "Link"))))
         (the-node-list (create-node-choice targetID #f in-edited-nodeID))
         (the-link-list (create-link-choice targetID))
         (the-fact-list (create-fact-choice 'boolean targetID))
         (the-node-operator-choice (if (not (is-basic-mode?))
                                       (make-combobox "Not Visited" "Visited" "Previous Node")
                                       (make-combobox "Not Visited" "Visited")))
         (the-link-operator-choice (make-combobox "Not Followed" "Followed"))
         (the-fact-operator-choice (make-combobox "False" "True")))

    (format #t "Creating condition-panel: targetID:~a, in-edited-nodeID:~a~%~!" targetID in-edited-nodeID)
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;(add-component condition-list-panel top-panel)
    
    ;; Add checkbox
    ;(add-component top-panel the-checkbox)

    ;; Add type choice
    (if (not (is-basic-mode?))
        (begin
          (add-component top-panel the-type-choice)
          (set-combobox-selection the-type-choice the-type)
          (add-actionlistener the-type-choice
                              (make-actionlistener (lambda (source)
                                                     (selected-type-in-condition source top-panel
                                                                                 the-node-list the-node-operator-choice
                                                                                 the-link-list the-link-operator-choice
                                                                                 the-fact-list the-fact-operator-choice)
                                                     (component-update top-panel)
                                                     ))))
        (add-component top-panel (make-label-with-title "Node")))
    
    
    ;; Check whether the condition is valid and
    ;; target choice action listener
    (add-actionlistener 
     the-node-list
     (make-actionlistener
      validate-rule))
    
    (add-actionlistener 
     the-link-list
     (make-actionlistener
      validate-rule))
    
    (add-actionlistener 
     the-fact-list
     (make-actionlistener
      validate-rule))

    ;; Add target choice - may have problems if loading a file that doesn't match chosen version
    (cond 
     ((= 0 the-type) (add-component top-panel the-node-list))
     ((= 1 the-type) (add-component top-panel the-link-list))
     ((= 2 the-type) (add-component top-panel the-fact-list)))

    ;; add condition
    (let ((the-choice (cond 
                       ((= 0 the-type) the-node-operator-choice)
                       ((= 1 the-type) the-link-operator-choice)
                       ((= 2 the-type) the-fact-operator-choice))))
      (add-component top-panel the-choice)
      (set-combobox-selection the-choice selectedOperator))
    
    ;; specify a fixed size
    (set-component-non-resizable-size top-panel 480 50) ;;condition-scrollpane-vp-width
    (set-border top-panel bevel-in-border)
    
    ;; prevent the comboboxes from expanding to fill the panel 
    (pack-component the-node-list)
    (pack-component the-link-list)
    (pack-component the-fact-list)
    
    (pack-component the-type-choice)
    
    (pack-component the-node-operator-choice)
    (pack-component the-link-operator-choice)
    (pack-component the-fact-operator-choice)
    
    (set-component-align-x top-panel 'left)
    
    (add-mouselistener
     top-panel
     (make-mouselistener
      (lambda (e)
        (if (equal? (get-mouseevent-type e) 'left-clicked)
            (begin
              ;; if control key not held down unselect the others
              (if (not (ctrl-key-down? (get-mouseevent-rawevent e)))
;                  (map (lambda (rID)
;                         (if (not (= rID ruleID))
;                             (select-rule-panel rID #f))
;                         ) (rmgr-rule-lst))
                  (map (lambda (pnl)
                         (if (not (equal? pnl top-panel))
                             (select-condition-panel pnl #f)))
                       (get-container-children condition-list-panel))
                  )

              ;(select-rule-panel ruleID (not (panel-selected? top-panel)))
              (select-condition-panel top-panel (not (panel-selected? top-panel)))
              (condition-panel-restrict)
              ))
        ;(action-restrict-check)
        )))

    ; return the panel
    top-panel))

;; used to enable and disable buttons depending on the condition panels state
(define (condition-panel-restrict)
  
  ;; enable add button when 0 or 1 condition panel selected 
  (set-component-enabled add-condition-button (not (> (length (get-selected-condition-panel)) 1)))
  
  ;; enabled delete when at least 1 condition panel selected
  (set-component-enabled delete-condition-button (> (length (get-selected-condition-panel)) 0))
  )


;;
;;;; callbacks from UI
;;

; set "ok" button state - cannot allow a "use" checkbox to be checked and a link to be "none"
;; TODO : outdated (used to do the above, now it should check 
;;        for new conditions where we don't want them to hit the ok button)
(define (update-ok-button-state) #f)

; selection of condition type changed, so change the node/link/fact list and operator list
(define (selected-type-in-condition c top-panel
                                    the-node-list the-node-operator-choice
                                    the-link-list the-link-operator-choice
                                    the-fact-list the-fact-operator-choice)
  
  ; remove the current target and operator lists
  (clear-container top-panel)
  (add-component top-panel c)
         
  ;; add new target and operator lists
  (let* ((new-type (get-combobox-selectedindex c))
         (new-target (cond
                      ((= 0 new-type) the-node-list)
                      ((= 1 new-type) the-link-list)
                      ((= 2 new-type) the-fact-list)))
         (new-operator (cond 
                        ((= 0 new-type) the-node-operator-choice)
                        ((= 1 new-type) the-link-operator-choice)
                        ((= 2 new-type) the-fact-operator-choice))))
    (add-component top-panel new-target)
    (add-component top-panel new-operator)
    
    (set-combobox-selection new-target 0)
    (set-combobox-selection new-operator 0))
  
  (pack-frame editlink-dialog))

; cancelled, so hide newnode-dialog
(define (edit-rule-cancel)
  ;; if updatelevel is more than 0 it doeditlink was called right after donewlink
  ;; in that case a beginupdate had been called so close the compound undoable edit
  ;; by calling endupdate to close it
  (if (> (compoundundomanager-updatelevel undo-manager) 0)
      (compoundundomanager-endupdate undo-manager undo-action redo-action))
  (set-component-visible editlink-dialog #f)
  (reset-rule-editor)
  (display "[reset-rule-editor] inside editlink-dialog-cancel ")(newline)
  )

; add a condition
(define (add-condition-callback)
  
  (define new-cond-panel (create-condition-panel 0 -1 0 -1))
  
  (if (= (length (get-selected-condition-panel)) 0)
     (begin
      (add-component condition-list-panel new-cond-panel)
       ))

  (if (= (length (get-selected-condition-panel)) 1)
      (begin
        (define index (list-index (lambda (o) (equal? o (car (get-selected-condition-panel)))) (condition-panel-list)))
        (add-component-at condition-list-panel new-cond-panel (+ index 1))
        ))
  
  ;; check whether rule is valid and enable ok button
  (validate-rule)
  
  ;(add-component condition-list-panel (create-condition-panel 0 -1 0 -1))
  (pack-frame editlink-dialog))

; delete selected condition
(define (delete-condition-callback)
  (let ((all-children
         (if (java.awt.Container? condition-list-panel)
             (get-container-children condition-list-panel)
             '())))

    (map (lambda (panel)
           (define this-checkbox (car (get-container-children panel)))
           (if (panel-selected? panel) ;(get-checkbox-value this-checkbox)
               (begin
                 (remove-component condition-list-panel panel)
                 ;; the panel does not disappear even after we do remove-component thus we do this
                 (component-update condition-list-panel)
                 )))
         all-children)
    
    ;; no more selected conditions, disable button
    (condition-panel-restrict)
    (validate-rule)
    (pack-frame editlink-dialog)))

; get expression from position
(define (get-rule-exp pos)
  (case pos
    ((0) 'and)
    ((1) 'or)))

; get position from expression
(define (get-rule-pos exp)
  (case exp
    ('and 0)
    ('or 1))) ; ('or exp 1))) alex xxx

;;;; update node graph lines
;; remove or add the line display associated with the follow link action of this rule
;; will check whether the rule has follow link first
(define (add-follow-link-rule-display ruleID)
  (update-rule-line-display ruleID 'add 'follow-link))
(define (remove-follow-link-rule-display ruleID)
  (update-rule-line-display ruleID 'remove 'follow-link))

;; the popup analogous of follow link
(define (add-show-popup-rule-display ruleID)
  (update-rule-line-display ruleID 'add 'show-in-popup))
(define (remove-show-popup-rule-display ruleID)
  (update-rule-line-display ruleID 'remove 'show-in-popup))

(define-private (update-rule-line-display ruleID add-or-remove action-type)
  (define rule (get 'rules ruleID))
  (if rule
      (begin
        ;; assumes its a rule of a link
        (define parent-link-ID (ask rule 'parentID))
        (define parent-link (get 'links parent-link-ID))
        (if parent-link
            (begin
              (define source-nodeID (ask parent-link 'source))
              ;; go through every action in this rule and look for follow-link action
              (map (lambda (actionID)
                     (define action (get 'actions actionID))
                     (define expr (ask action 'expr))
                     (if (eq? (car expr) action-type);; was 'follow-link
                         (begin
                           (define dest-nodeID 
                             (case action-type
                               ((follow-link) (list-ref expr 4))
                               ((show-in-popup) (list-ref expr 1))))
                           
                           ;; Question: should we not allow the user to press ok
                           ;; when the dest node is set to none?
                           
                           (if (not (= dest-nodeID -1))
                               (case add-or-remove
                                 ((add) ;; draw a line in the node-graph from the edited-node to the dest-node
                                  (ask node-graph 'create-line
                                       (ask rule 'name)
                                       source-nodeID
                                       dest-nodeID
                                       ruleID ;; use ruleID as the display ID of line for the moment))
                                       (case action-type
                                         ((follow-link) 'default)
                                         ((show-in-popup) 'dashed))
                                       )
                                  ;(ask node-graph 'refresh-display) ;; check show-IDs? and display id accordingly for the new line
                                  )
                                 ((remove) ;; remove the line in the node-graph 
                                  (ask node-graph 'del-line
                                        ;                                   (ask node-graph 'get-line-by-ID
                                        ;                                        (number->string ruleID))
                                       (number->string ruleID)
                                       source-nodeID
                                       dest-nodeID)
                                  )))
                           
                           )))
                   (ask rule 'actions)) ;; end of map
              ))
        ))
  )

;; add or remove the line display from the rules of linkID
(define (remove-link-display linkID)
  (update-link-display2 linkID 'remove))
(define (add-link-display linkID)
  (update-link-display2 linkID 'add))

;; if add-remove is 'add we draw all the lines in linkID
;; if add-remove is 'remove we remove all the lines in linkID
;; there is an update-link-display in object-graphview, therefore the 2 at the back
(define-private (update-link-display2 linkID add-or-remove)
    (define the-link (get 'links linkID))
    (if the-link
        (begin
          (define rule-lst (ask the-link 'rule-lst))
          
          (case add-or-remove
            ((add) 
             (map add-follow-link-rule-display rule-lst)
             (map add-show-popup-rule-display rule-lst))
            ((remove)
             (map remove-follow-link-rule-display rule-lst)
             (map remove-show-popup-rule-display rule-lst)))
          )))

;;;; Condition panel operations

;; getting the components in the condition panel
(define (condition-panel-component panel index)
  (let ((children (get-container-children panel)))
    ;;(display "condition panel component length ")(display (length children))(newline)
    ;;(display "children ")(display children)(newline)
    (if (< index (length children))
        (list-ref children index))))
(define (condition-panel-target-type-cb panel)
  (condition-panel-component panel 0))
(define (condition-panel-target-choice-cb panel)
  (condition-panel-component panel 1))
(define (condition-panel-operator-cb panel)
  (condition-panel-component panel 2))

(define (condition-panel-target-type panel)
  (if (not (is-basic-mode?))
      (get-combobox-selectedindex (condition-panel-target-type-cb panel))
      0))

(define (condition-panel-target-id panel)
    (get-comboboxwithdata-selecteddata (condition-panel-target-choice-cb panel)))

(define (condition-panel-operator panel)
  (display "condition panel operator ")(newline)
  (display (condition-panel-operator-cb panel))(newline)
  (get-combobox-selectedindex (condition-panel-operator-cb panel)))

(define (condition-panel-valid? panel)
  ;; target id cannot be -1 
  (not (= (condition-panel-target-id panel) -1)))

(define (all-conditions-valid?)
  
  (define (my-and lst)
    (if (null? lst) 
        #t
        (and (car lst) 
             (my-and (cdr lst)))))
  
  (my-and (map condition-panel-valid?
               (get-container-children condition-list-panel)))
  )

;; a duplicate of how selectable rule panel

(define (panel-selected? pnl)
  (equal? (get-background-color pnl) selected-color))

(define selected-color (make-colour-rgb 135 206 250))  ;; sky blue
(define unselected-color (make-colour-rgb 238 238 238))

(define (select-condition-panel pnl selected?)
  (if selected?
      (set-background-color pnl selected-color)
      (set-background-color pnl unselected-color)
      ))

;; similar to how rule panel is handled

;(define (get-condition-panel-fall-checkbox rule-panel)
;  (list-ref (get-container-children rule-panel) 2))
;(define (get-condition-panel-name-label rule-panel)
;  (list-ref (get-container-children rule-panel) 0))

;;  =============
;;;; fact panel
;;  =============

; create an fact panel
; the-action: the type of action 'assert, 'retract or 'set-value!
; factID: the currently selected fact, if any (pass in -1 if none selected)
; the-value: value that will be set (for 'set-value! only)
;; fact type is only needed if a factID is selected

(define (create-fact-panel fact-type factID the-value #!key num-fact-mode)
  (let* ((top-panel (make-panel))
         ;(the-checkbox (make-checkbox ""))
         (the-type-choice (make-combobox "True/False" "Text" "Number"))
         (the-boolean-choice (make-combobox "True" "False"))
         (the-string-entry (make-textfield "[the fact text...........]" 20))
         (the-number-entry (make-textfield "0" 4))
         (the-fact-list-boolean (create-fact-choice 'boolean factID))
         (the-fact-list-string (create-fact-choice 'string factID))
         (the-fact-list-number (create-fact-choice 'number factID))
         (number-fact-target-cb (create-fact-choice 'number #f))
         (num-fact-mode-choice (make-combobox "Input" "Fact" "Math"))
         )
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;; add type choice
    (add-component top-panel the-type-choice)
    
    ;; create math UI
    (define (make-math-panel #!optional op opr1 opr1-type opr2 opr2-type)
      (define math-panel (make-panel))
      
      (display "op ")(display op)(newline)
      ;; operand choice returns a panel for inputing number facts value
      ;; 2 modes are provided now "Input" and "Fact" 
      (define (make-operand-choice opr opr-type)
        (let ((operand-panel (make-panel))
              (mode-choice (make-combobox "Input" "Fact"))
              (number-entry (make-textfield "0" 4))
              (number-choice (create-fact-choice 'number #f))
              )
          (set-container-layout operand-panel 'horizontal)
          (add-component operand-panel mode-choice)
          ;;(add-component operand-panel number-entry)
          
          ;; set the value in the UI if provided
          (if (and opr-type opr)
              (begin
                (set-combobox-selection-object mode-choice (create-combobox-string-item opr-type))
                (case opr-type
                  (("Input")
                   (set-text number-entry opr)
                   (add-component operand-panel number-entry))
                  (("Fact")
                   (set-comboboxwithdata-selection-bydata number-choice (string->number opr))
                   (add-component operand-panel number-choice)
                   ))
                )
              (begin
                (set-text number-entry "0")
                (add-component operand-panel number-entry)
                (set-comboboxwithdata-selection-bydata number-choice -1)
                ;;(add-component operand-panel number-choice)
                )
              )
          
          (add-actionlistener mode-choice
                              (make-actionlistener
                               (lambda (e)
                                 (remove-component operand-panel number-entry)
                                 (remove-component operand-panel number-choice)
                                 
                                 ;; add appropriate component
                                 (case (to-string (get-combobox-selecteditem mode-choice))
                                   (("Input") (add-component operand-panel number-entry))
                                   (("Fact") (add-component operand-panel number-choice))
                                   )
                                 
                                 (validate-rule)
                                 
                                 ;; update thie panel and parent panels
                                 (component-revalidate operand-panel)
                                 (component-update operand-panel)
                                 (pack-component operand-panel)
                                 (pack-component math-panel)
                                 (pack-component top-panel)
                                 )))
          (add-actionlistener
           number-choice
           (make-actionlistener
            validate-rule
            ))
          
          (add-documentlistener
           number-entry
           (make-documentlistener
            validate-rule
            validate-rule
            validate-rule
            ))
          
          (pack-component operand-panel)
          operand-panel))
      
      (define operator-choice (make-combobox "+" "-" "x"))
      (if op
          (set-combobox-selection-object operator-choice (create-combobox-string-item op)))
      
      (set-container-layout math-panel 'horizontal)
      (add-components math-panel 
                      (make-operand-choice opr1 opr1-type)
                      operator-choice
                      (make-operand-choice opr2 opr2-type))
;      (component-revalidate math-panel)
;      (component-update math-panel)
      (pack-component math-panel)
      math-panel)
  
    ;; choose the fact type index
    (define (fact-type-index)
      (cond ((or (equal? 'assert fact-type)
                 (equal? 'retract fact-type)) "True/False")
            ((equal? 'set-value! fact-type) "Text")
            ((equal? 'set-number-fact fact-type) "Number")
            ((equal? #f fact-type) "True/False")
            (else (display "wrong type in create-fact-panel")(display fact-type)(newline)
                  "HUH")
            ))
    
    ; set type choice
    ;(set-combobox-selection the-type-choice (fact-type-index))
    (set-combobox-selection-object the-type-choice (create-combobox-string-item (fact-type-index)))
    
    ;; add the appropriate components
;    (set-fact-panel-components top-panel (fact-type-index)
;                               the-fact-list-string the-string-entry
;                               the-fact-list-boolean the-boolean-choice
;                               the-fact-list-number the-number-entry num-fact-mode-choice)
    (case (fact-type-index)
      (("True/False")
       (add-component top-panel the-fact-list-boolean)
       (add-component top-panel the-boolean-choice))
      (("Text")
       (add-component top-panel the-fact-list-string)
       (add-component top-panel the-string-entry))
      (("Number")
       (add-component top-panel the-fact-list-number)
       (add-component top-panel (make-label-with-title " using "))
       (add-component top-panel num-fact-mode-choice)
       (add-component top-panel the-number-entry)))
    
    ; add type callback
    (add-actionlistener the-type-choice
                        (make-actionlistener (lambda (source)
;                                               (selected-type-in-action source top-panel
;                                                                        the-fact-list-boolean the-boolean-choice
;                                                                        the-fact-list-string the-string-entry
;                                                                        the-fact-list-number the-number-entry num-fact-mode-choice)
                                                ; remove the current target and operator lists
                                               (let* ((children (get-container-children top-panel))
                                                      (fact-type-choice (car children))
                                                      )
                                                 (clear-container top-panel)
                                                 (add-component top-panel fact-type-choice))

                                               ;; add the appropriate components and set value
;                                               (set-fact-panel-components top-panel (to-string (get-combobox-selecteditem c));(get-combobox-selectedindex c)
;                                                                          the-fact-list-string the-string-entry
;                                                                          the-fact-list-boolean the-boolean-choice
;                                                                          the-fact-list-number the-number-entry num-fact-mode-choice)
                                               (case (to-string (get-combobox-selecteditem source))
                                                 (("True/False")
                                                  (add-component top-panel the-fact-list-boolean)
                                                  (add-component top-panel the-boolean-choice))
                                                 (("Text")
                                                  (add-component top-panel the-fact-list-string)
                                                  (add-component top-panel the-string-entry))
                                                 (("Number")
                                                  (add-component top-panel the-fact-list-number)
                                                  (add-component top-panel (make-label-with-title " using "))
                                                  (add-component top-panel num-fact-mode-choice)
                                                  (add-component top-panel the-number-entry)))
                                               
                                               
                                               (pack-component top-panel)
                                               (pack-frame editlink-dialog)
                                             
                                               (validate-rule)
                                               )))

    (add-actionlistener
     the-fact-list-boolean
     (make-actionlistener
      validate-rule))
    
    (add-actionlistener
     the-fact-list-string
     (make-actionlistener
      validate-rule))
    
    (add-actionlistener
     the-fact-list-number
     (make-actionlistener
      validate-rule))
    
    (add-actionlistener
     number-fact-target-cb
     (make-actionlistener
      (lambda (e)
        (display "num fact mode change")(newline)
        (validate-rule))))
    
    ;; check content of number entry to be numeric
;    (let ((callback 
;           (lambda (e)
;             ;; disable ok button is non numeric
;             (set-component-enabled
;              editlink-panel-buttons-ok
;              (string-is-numeric? (get-text the-number-entry)))
;             )))
    (add-documentlistener
     the-number-entry
     (make-documentlistener
      validate-rule
      validate-rule
      validate-rule
      ))
    
    ;; number facts has different mode
    (add-actionlistener
     num-fact-mode-choice
     (make-actionlistener
      (lambda (e)
        ;; remove fourth component
        (remove-component top-panel (list-ref (get-container-children top-panel) 4))
        ;(remove-component top-panel the-number-entry)
        ;(remove-component top-panel number-fact-target-cb)
        
        
        ;;(display "top-panel ")(display top-panel)(newline)
        ;;(display "number-fact-target-cb ")(display number-fact-target-cb)(newline)
        
        (case (to-string (get-combobox-selecteditem num-fact-mode-choice))
          (("Input") (add-component top-panel the-number-entry))
          (("Fact") (add-component top-panel number-fact-target-cb))
          (("Math") 
           (if (pair? the-value)
               (add-component top-panel (apply make-math-panel the-value))
               (add-component top-panel (make-math-panel))
               ))
          )
        (component-revalidate top-panel)
        (pack-component top-panel)
        )))
    
    ;; set current value of fact
    ;; call after all the action listener to fire them when needed
    (if factID ;; if factID false
        (cond ((equal? fact-type 'boolean)
               (display "boolean fact ")(display the-value)(newline)
               (cond ((equal? the-value #t) (set-combobox-selection the-boolean-choice 0))
                     ((equal? the-value #f) (set-combobox-selection the-boolean-choice 1))))
              ((equal? fact-type 'assert)
                (display "boolean fact assert")(newline)
               (set-combobox-selection the-boolean-choice 0))
              ((equal? fact-type 'retract)
               (display "boolean fact retract")(newline)
               (set-combobox-selection the-boolean-choice 1))
              ;;  ;; is 'string ever used? -teongleong
              ((or (equal? fact-type 'string)
                   (equal? fact-type 'set-value!))
               (display "string fact")(display the-value)(newline)
               (set-text the-string-entry the-value))
              ((equal? fact-type 'set-number-fact)
               (set-combobox-selection-object num-fact-mode-choice (create-combobox-string-item num-fact-mode))
               (case num-fact-mode
                 (("Input") (set-text the-number-entry (to-string the-value)))
                 (("Fact") (set-comboboxwithdata-selection-bydata number-fact-target-cb the-value))
                 (("Math") #f)
                 )
               )
              ))
    
    (pack-component top-panel)
    ; return the panel
    top-panel))

; set fact panel components
(define (set-fact-panel-components top-panel the-type
                                   the-fact-list-string the-string-entry
                                   the-fact-list-boolean the-boolean-choice
                                   the-fact-list-number the-number-entry num-fact-mode-choice)
  (display "[set fact panel components]")(newline)
  (case the-type
    (("True/False") 
     (add-component top-panel the-fact-list-boolean)
     (add-component top-panel the-boolean-choice))
    (("Text")
     (add-component top-panel the-fact-list-string)
     (add-component top-panel the-string-entry))
    (("Number")
     (add-component top-panel the-fact-list-number)
     (add-component top-panel (make-label-with-title " using "))
     (add-component top-panel num-fact-mode-choice)
     (add-component top-panel the-number-entry)))
  )