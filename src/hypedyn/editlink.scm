;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2014
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
(require "../kawa/ui/menu.scm") ;; make-separator

(require "../kawa/system.scm") ; is-windows?
(require "../kawa/strings.scm") ;; to-string
(require "../kawa/color.scm") ;; make-colour-rgb
(require "../kawa/geometry.scm") ;; ;; debug (get-dimension-width)
(require "../kawa/graphics-kawa.scm") ;; make-rectangle

(require "../common/objects.scm") ;; ask
(require "../common/datatable.scm") ;; get, table-map
(require "../common/main-ui.scm") ; for get-main-ui-frame
(require "../common/list-helpers.scm") ;; list-replace, list-insert

(require "config-options.scm") ; choice-name-limit
(require "datastructure.scm")
(require "hteditor.scm")
(require "hypedyn-undo.scm") ;; hd-postedit, hd-begin-update, hd-end-update
(require "nodeeditor.scm") ;; nodeeditor-save
(require "htfileio.scm") ;; loaded-file-version
(require "rules-manager.scm")
(require 'list-lib) ;; list-index

; export
(module-export doeditlink doeditnoderule doeditdocrule
               create-editlink-dialog
               ;create-if-condition-panel
               ;create-actions-main-panel
               ;create-update-text-action-panel
               
               add-follow-link-rule-display remove-follow-link-rule-display
               add-link-display remove-link-display
               
               ;; used by rules-manager so far
               edit-mode 
               remove-follow-link-rule-display 
               remove-show-popup-rule-display
               add-follow-link-rule-display
               add-show-popup-rule-display
               update-anywhere-flag
               
               delete-action ;; just for ht-editor.scm's dodelnode
               
               edited-linkID
               edited-ruleID
               )

(define-constant cond-panel-width 500) ;; not used
(define-constant cond-panel-height 50)

(define-constant action-panel-width 500) ;; not used
(define-constant action-panel-height 50)

(define-constant action-scrollpane-width 500)
(define-constant action-scrollpane-height 140)

(define-constant condition-scrollpane-width 500)
(define-constant condition-scrollpane-height 140)
               

; remember which link we're editing
(define edited-linkID '())
;;(define (get-edited-linkID) edited-linkID)

;; keep track of which rule we're editing
(define edited-ruleID '())

; remember currently edited node
;(define-private edited-nodeID '()) ;; now using nodeeditor.scm's edited-nodeID

; rule editing mode, can be 'link or 'node
(define edit-mode 'link)

(define (edited-rule)
  (if (not (null? edited-ruleID))
      (get 'rules edited-ruleID)
      #f))

;;;; doedit

; edit a link - called from hteditor.scm
;; in-default-link-text not used any more
;; NOTE: this should be renamed as doeditrule now
;;       since it targets one rule inside the link
(define (doeditlink selected-linkID in-edited-nodeID in-ruleID);in-default-link-text)
  
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
      "Edit rule for link: "
      link-name
      (if (show-IDs?)
          (string-append " (" (number->string edited-linkID) ")")
          "")))
    
    ;; add appropriate panels to editlink-dialog
    (clear-container editlink-panel-top)
    (add-component editlink-panel-top (create-rules-rename-panel in-ruleID))
    
    ;;(Box.createHorizontalStrut(5));
    ;; separator is invisible by default due to it having no width or height in the direction 
    ;; it is separating

    (define separator (make-separator))
    (set-component-preferred-size separator 200 10)
    (add-component editlink-panel-top separator)
    
    (add-component editlink-panel-top (create-if-condition-panel))
    (add-component editlink-panel-top (create-actions-main-panel))
    
    ;; reset combobox to contain all action-type-list
;;    (let ((edited-node (get 'nodes in-edited-nodeID)))
;;      (if (ask edited-node 'anywhere?)
;;          (reset-action-type-choice 'anywhere-node-link)
          (reset-action-type-choice 'link)
;;          )
;;      )
    
    (populate-rule-editor in-ruleID))

  ; set "ok" button state
  (validate-rule)

  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog editlink-dialog-parent)
  (set-component-visible editlink-dialog #t)
  )

; edit a node rule - removes the link-specific portions of the editor
(define (doeditnoderule in-edited-nodeID in-ruleID)
  (display "edit node rule ")(newline)
  (nodeeditor-save) ;; save text content before create sexpr in before-editnode
  
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
  
  ;; add if panel to editlink-dialog
  (clear-container editlink-panel-top)
  (add-component editlink-panel-top (create-rules-rename-panel in-ruleID))
  (add-component editlink-panel-top (create-if-condition-panel))
  (add-component editlink-panel-top (create-actions-main-panel))
  
    ; get the rule from the node
  (let* ((edited-node (get 'nodes edited-nodeID))
         ;;(selected-rule (ask edited-node 'rule))
         (node-name (ask edited-node 'name))
         ;;(anywhere (ask edited-node 'anywhere?))
         )

    ;; show node name
    (set-dialog-title editlink-dialog (string-append "Edit rule for node: " node-name))

    ;; reset combobox to contain all action-type-list
;;    (if (ask edited-node 'anywhere?)
        (reset-action-type-choice 'anywhere-node)
;;        (reset-action-type-choice 'node)
;;        )
    )

  (populate-rule-editor in-ruleID)

  ; set "ok" button state
  (validate-rule)

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

  ; set "ok" button state
  (validate-rule)

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

(define max-action-panel-width 0)
(define max-cond-panel-width 0)

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
  (set-container-layout editlink-panel-follow 'horizontal)
  (add-component editlink-panel-if editlink-panel-follow)

  ;; obsolete ("IF" becomes a combobox that has UNLESS as the opposite of IF) 
;  (set! editlink-dialog-operater-label (make-label-with-title "IF"))
;  (add-component editlink-panel-follow editlink-dialog-operater-label)
  
  ;; "If" label
  (add-component editlink-panel-follow (make-label-with-title " If "))
  
  ; "all"/"any" selection
  (set! editlink-dialog-andor-operator (make-combobox "All" "Any"))
  (add-component editlink-panel-follow editlink-dialog-andor-operator)
  (pack-component editlink-dialog-andor-operator)
  
  ; label for "of the following conditions are true:"
  (set! editlink-dialog-label (make-label))
  (set-text editlink-dialog-label " of the following conditions are true:")
  (add-component editlink-panel-follow editlink-dialog-label)
  
  ;; up down button
  (define cond-up-button (make-button "Up"))
  (define cond-down-button (make-button "Down"))
  (add-component editlink-panel-follow (create-horizontal-glue))
  (add-component editlink-panel-follow cond-up-button)
  (add-component editlink-panel-follow cond-down-button)

  ;;condition panel
  (set! condition-list-panel (make-panel))
  (set-container-layout condition-list-panel 'vertical)
  ;(add-component editlink-panel-if condition-list-panel)
  
  ;; container scrollpane for condition panel
  (define condition-scrollpane (make-scrollpane-with-policy condition-list-panel 'needed 'needed))
  (set-component-preferred-size condition-scrollpane 
                                condition-scrollpane-width condition-scrollpane-height)
  
  (define vert-scrollbar (scroll-get-scrollbar condition-scrollpane 'vert))
  (define scrollbar-width (get-preferred-width vert-scrollbar))
  (set! condition-scrollpane-vp-width (- condition-scrollpane-width scrollbar-width))
  (display "condition scroll-vp-width ")(display condition-scrollpane-vp-width)(newline)
  
  ;; reset the max width of condition
  (set! max-cond-panel-width condition-scrollpane-vp-width)
  
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
  
  ;; Action listeners
  
  (add-actionlistener cond-down-button 
                      (make-actionlistener (shift-callback 'cond 'down)))
  
  (add-actionlistener cond-up-button 
                      (make-actionlistener (shift-callback 'cond 'up)))
  
  editlink-panel-if) ;; end of IF conditions

;; get the position of the children panel which are selected
(define (get-selected-pos-lst container-panel)
  (define (helper lst pos) 
    (if (null? lst)
        '()
        (begin
          (define curr-panel (car lst))
          (append 
           ;; condition append
           (if (panel-selected? curr-panel)
               (list pos)
               '())
           (helper (cdr lst) (+ pos 1)))
          )))
  ;; traverse through the list of rule panels
  (helper (get-container-children container-panel) 0))

;; used by condition's and action's up down buttons for callbacks
;; type ['cond 'action]
;; dir ['up 'down]
(define (shift-callback type dir)
  (lambda (e)
    (let ((container-panel (case type
                             ((cond) condition-list-panel)
                             ((action) action-list-panel))))
      (define selected-lst (get-selected-pos-lst container-panel))
      (if (= (length selected-lst) 1)
          (let ((pos (car selected-lst)))
            (case dir
              ((up) (shift-panel container-panel pos (- pos 1)))
              ((down) (shift-panel container-panel pos (+ pos 1))))
            (component-revalidate container-panel)
            )))
  ))

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
         ;; get boolean operator
         ( new-rule-and-or-pos (get-combobox-selectedindex editlink-dialog-andor-operator))
         ( new-rule-and-or (get-rule-exp new-rule-and-or-pos)))

    ;; remove the line associated with this rule before we edit it
    (if (eq? edit-mode 'link)
        (remove-follow-link-rule-display edited-ruleID))  ;; remove the line display from the previous edited-ruleID
    (if (eq? edit-mode 'link)
        (remove-show-popup-rule-display edited-ruleID))  ;; remove the line display from the previous edited-ruleID
    
    
    (define the-rule (get 'rules edited-ruleID))
    
    ;; cache a version of the unedited link for creation of undo
    (before-edit-rule edited-ruleID)
    
    ;; empty the rule and delte the old actions and conditions
    ;; since we're not recycling the conditions and actions, delete them from data table
    (display "recycling in confirm ")(display (ask the-rule 'actions))(newline)
    (map (lambda (actionID)
           (del 'actions actionID)
           ) (ask the-rule 'actions))
    (map (lambda (condID)
           (del 'conditions condID)
           ) (ask the-rule 'conditions))
    ;; empty current rule's contents before making the changes
    (if the-rule
        (ask the-rule 'empty-rule))

    (display "after emptying ")(display (ask the-rule 'actions))(newline)

    (define action-lst (get-list 'actions))
    (display "action lst ")(display action-lst)(newline)
    
    ;; debug
    (if (pair? action-lst)
        (begin
          (display "action list ")(display (map (lambda (o) (car o)) (get-list 'actions)))(newline))
        (begin
          (display "action lst false ")(newline)
          ))
    
    ;; set the and-or and negate? properties
    (if the-rule
        (ask the-rule 'set-and-or! new-rule-and-or))
    
    ;; set the new name of the rule
    (if the-rule
        (ask the-rule 'set-name! new-rulename))
    
    ;; Conditions
    ; run through conditions and add to rule
    (map (lambda (panel)
           (if (or (= (condition-panel-target-type panel) 0)
                   (= (condition-panel-target-type panel) 1)
                   (= (condition-panel-target-type panel) 2))
               (create-typed-condition2 new-rulename
                                       (condition-panel-target-type panel)
                                       (condition-panel-target-id panel)
                                       (condition-panel-operator panel)
                                       edited-ruleID)
               ;; if target type index is 3 it is number fact comparing conditions
               ;; [ [comparator-operator-cb] [[operand-type-cb] [operand-choice]] ] - comparator panel layout
               (let* ((comparator-panel (condition-panel-operator-cb panel))
                      (cmpr-lst (get-container-children comparator-panel))
                      (comparator-cb (car cmpr-lst))

                      (operand-panel (cadr cmpr-lst))
                      (oprd-lst (get-container-children operand-panel))
                      (operand-type-cb (car oprd-lst))
                      (operand-choice (cadr oprd-lst))

                      (comparator (to-string (get-combobox-selecteditem comparator-cb)))
                      (operand-type (to-string (get-combobox-selecteditem operand-type-cb)))
                      (operand-choice
                       (case operand-type
                         (("Input") (get-text operand-choice))
                         (("Fact") (to-string (get-comboboxwithdata-selecteddata operand-choice)))
                         ))
                      )
                 (create-typed-condition2 new-rulename
                                         (condition-panel-target-type panel)
                                         (condition-panel-target-id panel)
                                         #f
                                         edited-ruleID
                                         numfact-args: (list comparator operand-type operand-choice)
                                         )
                 )))
         (condition-panel-list))
    
    ;; then-action-string, else-action-string
    
    ; if there's an action, add the action to the rule
    ; need to read the string and break in to s-expressions; should eventually
    ; be able to plug in an improved version of definition-editor here
    ; NOTE: I haven't decided if actions cashould be stored as strings (currently)
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
                  (define update-text-using-panel (list-ref (get-container-children action-panel) 1))
                  (define action-type-combobox (utap-get-action-type-combobox update-text-using-panel))
                  (define second-comp (utap-get-second-comp update-text-using-panel))
                  
                  ;; text of fact?
                  (define text-or-fact (get-combobox-selecteditem action-type-combobox))
                  (define text-type #f)
                  (define text-value #f)
                  (case (to-string text-or-fact)
                    (("alternative text")
                     (display "came into alt ")(newline)
                     (set! text-type "alternative text")
                     (set! text-value (get-text second-comp)))
                    (("text fact" "number fact")
                     (set! text-type "text fact")
                     (define factID (get-comboboxwithdata-selecteddata second-comp))
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
                 ;; Show in popup
                 ((equal? action-type "show in popup")
                  (define target-nodeID (get-comboboxwithdata-selecteddata node-choice-combobox2))
                  (display "edited linkID on confirm ")(display edited-linkID)(newline)
                  (create-action "Show in Popup" 'clicked-link
                               (list 'show-in-popup target-nodeID)
                               edited-ruleID)
                  )
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
                                  (begin
                                  (set! operator "*")
                                  (display "x -> * \n")))

                              (define new-fact-value-expr
                                (list operator
                                      operand1 operand1-type
                                      operand2 operand2-type))
                              new-fact-value-expr)
                             (("Random")
                              (define random-panel-lst (get-container-children comp3))
                              (define operand1-panel (list-ref random-panel-lst 1))
                              (define operand2-panel (list-ref random-panel-lst 3))
                              
                              ;; similar to the code on top
                              (define operand1-panel-lst (get-container-children operand1-panel))
                              (define operand2-panel-lst (get-container-children operand2-panel))
                              
                              (define operand1-type (to-string (get-combobox-selecteditem (list-ref operand1-panel-lst 0))))
                              (define operand2-type (to-string (get-combobox-selecteditem (list-ref operand2-panel-lst 0))))
                              
                              (define operand1 
                                (case operand1-type
                                  (("Input") (get-text (list-ref operand1-panel-lst 1)))
                                  (("Fact") (to-string (get-comboboxwithdata-selecteddata (list-ref operand1-panel-lst 1))))))
                              
                              (define operand2
                                (case operand2-type
                                  (("Input") (get-text (list-ref operand2-panel-lst 1)))
                                  (("Fact") (to-string (get-comboboxwithdata-selecteddata (list-ref operand2-panel-lst 1))))))
                              
                              (define new-fact-value-expr
                                (list operand1 operand1-type
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
                 
                 ;; no parameter from the ui
                 ((equal? action-type "show disabled anywhere links")
                  (create-action "Show Disabled Link" 'disabled-anywhere-check
                               (list 'show-disabled-anywhere-link edited-nodeID)
                               edited-ruleID))
                 
                 ) ;; end of action-type cond
           ) (action-panel-list))

    ; check whether node is an anywhere node after editing the rule
    (if (eq? edit-mode 'node)
        (update-anywhere-flag edited-nodeID))

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

; helper to check whether a node is an anywhere node after editing a rule
(define (update-anywhere-flag thisnode-ID)
  (let* ((thisnode-obj (get 'nodes thisnode-ID))
         (thisnode-rules (ask thisnode-obj 'rule-lst))
         (has-anywhere-link #f))
    
    ; go through the rules and see if any have anywhere link actions
    (map (lambda (thisrule)
           (let* ((thisrule-obj (get 'rules thisrule))
                  (thisrule-actions (ask thisrule-obj 'actions)))
             (map (lambda (thisaction)
                    (let* ((thisaction-obj (get 'actions thisaction))
                           (thisaction-expr (ask thisaction-obj 'expr))
                           (thisaction-type (car thisaction-expr)))
                      (set! has-anywhere-link 
                            (or has-anywhere-link
                                (equal? 'add-anywhere-link thisaction-type)))))
                  thisrule-actions)))
           thisnode-rules)
    (format #t "After editing rule, nodeID:~a, anywhere:~a~%~!" thisnode-ID has-anywhere-link)
    (ask thisnode-obj 'set-anywhere! has-anywhere-link)
    ;(ask node-graph 'refresh-node thisnode-ID)
    (ask node-graph 'refresh)) ; is this going to lag eventually? - alex
  )

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
        
        ((equal? obj-type 'anywhere-node)
         (set! action-type-list action-type-list-anywhere-node))
        
        ((equal? obj-type 'anywhere-node-link)
         (set! action-type-list action-type-list-anywhere-node-link))
         )
  
  ;; reset the available actions type in the combobox
  (display "RESETTING ACTION TYPE CHOICE ")(newline)
  
  (map (lambda (action-type)
         (add-combobox-string action-type-choice action-type))
       action-type-list)
  
  ;; select "follow link to" option if its a link (assuming follow link is the first option) 
  (if (equal? obj-type 'link)
      (set-combobox-selection-object action-type-choice (create-combobox-string-item "follow link to")))
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
            (if (>= (length fp-children) 5)
                (let* ((math-panel (list-ref fp-children 4))
                       (math-panel-lst (get-container-children math-panel)))
                  ;(display "math-panel class type ")(display (invoke math-panel 'get-class))(newline)
                  
                       (define operand1-panel (list-ref math-panel-lst 0))
                       (define operand2-panel (list-ref math-panel-lst 2))
                  
                  (and (operand-panel-valid? operand1-panel)
                       (operand-panel-valid? operand2-panel))))
            )
           (("Random")
            (if (>= (length fp-children) 5)
                (let* ((random-panel (list-ref fp-children 4))
                       (random-panel-lst (get-container-children random-panel))
                       (opr1-panel (list-ref random-panel-lst 1))
                       (opr2-panel (list-ref random-panel-lst 3)))
                  (and (operand-panel-valid? opr1-panel)
                       (operand-panel-valid? opr2-panel))))
            ))
       ))) ;; end of case
    ))

(define (update-text-using-panel-valid? panel)
  (define update-text-using-panel (list-ref (get-container-children panel) 1))
  (define action-type-combobox (utap-get-action-type-combobox update-text-using-panel))
  (define second-comp (utap-get-second-comp update-text-using-panel))
  (let ((selected-item (to-string (get-combobox-selecteditem action-type-combobox))))
    (case selected-item
      (("alternative text") #t) ;; no checking required
      (("text fact" "number fact") ;; make sure combobox does not have none selected
       (not (= (get-comboboxwithdata-selecteddata second-comp) -1)))
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
    (("enable links to this node from anywhere") #t)
    (("show disabled anywhere links") #t)
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

;; add an action to the action list of type action-type ["update text using", "follow link to", "update fact", "show in popup"]
;; args-lst null means we're adding a new empty action panel,
;; if args-lst not null load the information provided (present existing action) 
(define (create-action-panel action-type . args-lst)
;  (display "[create-action-panel] ")(newline)
;  (display "  action type ")(display action-type)(newline)
;  (display "  args-lst ")(display args-lst)(newline)
  
  (define panel-to-return (create-action-panel-common action-type))
  (set-container-layout panel-to-return 'horizontal)
  (set-component-align-x panel-to-return 'left)
  
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
  
  
  ;; alter the configuration of the ui objects if args-lst given
  (cond ((equal? action-type "update text using")
         (define update-text-action-panel (create-update-text-action-panel))
         (define action-type-combobox (utap-get-action-type-combobox update-text-action-panel))
         (if (= (length args-lst) 2)
             (let* ((using-type (car args-lst))
                    (alt-text (cadr args-lst)))
               
               ;; set the mode of update text
               (set-combobox-selection-object action-type-combobox
                                              (create-combobox-string-item using-type))
               
               ;; the second component in update-text-action-panel is switched in depending on what is selected in action-type-combobox 
               (define second-comp (utap-get-second-comp update-text-action-panel))
               
               (cond ((equal? using-type "alternative text")
                      (set-text second-comp alt-text)
                      )
                     ((or (equal? using-type "text fact")
                          (equal? using-type "number fact"))
                      (define target-fact (get 'facts alt-text))
                      (define fact-name (ask target-fact 'name))

                      (define for-selection (create-combobox-string-item fact-name))
                      (set-combobox-selection-object second-comp for-selection)
                      ))
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
               (set! node-choice-combobox (create-node-choice link-dest1 #f -1))
               
               )
             (set! node-choice-combobox (create-node-choice #f #f -1)))
         
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
               ;; number fact
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
         #f)
        
        ((equal? action-type "show disabled anywhere links")
         #f)
        
        ((equal? action-type "show in popup")
         (if (= (length args-lst) 1)
             (set! node-choice-combobox2 (create-node-choice (car args-lst) #f -1))
             (set! node-choice-combobox2 (create-node-choice #f #f -1)))
         (add-actionlistener
          node-choice-combobox2
          (make-actionlistener
           validate-rule))
         (pack-component node-choice-combobox2)
         (add-component panel-to-return node-choice-combobox2))
        )
  
  (set-border panel-to-return bevel-in-border)
  ;;(set-component-non-resizable-size panel-to-return cond-panel-width cond-panel-height) ;;action-scrollpane-vp-width was 480 for width
  ;;(pack-component panel-to-return)
  
  (define preferred-width (get-preferred-width panel-to-return))
  (display "preferred width ")(display preferred-width)(newline)
  
  (set! max-action-panel-width 
        (max max-action-panel-width 
             (max preferred-width action-scrollpane-vp-width)))
  
  ;;(display "max-action-panel-width ")(display max-action-panel-width)(newline)
  
  (set-component-non-resizable-size panel-to-return max-action-panel-width action-panel-height)
  
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
                             (select-panel pnl #f 'action)))
                       (action-panel-list))
                  )

              (select-panel panel-to-return (not (panel-selected? panel-to-return)) 'action)
              (action-panel-restrict)
              ))
        )))
  
  panel-to-return)

;; called when add action button is pressed
(define (add-action-callback source)
  (display "add action! ")(newline)
  
    ;; get the selected type
  (define selected-action-type (to-string (get-combobox-selecteditem action-type-choice)))
  (define new-action-panel (create-action-panel selected-action-type))
  
  (if (= (length (get-selected-action-panel)) 0)
     (begin
      (add-component action-list-panel new-action-panel)
       ))

  (if (= (length (get-selected-action-panel)) 1)
      (begin
        (define index (list-index (lambda (o) (equal? o (car (get-selected-action-panel)))) (action-panel-list)))
        (add-component-at action-list-panel new-action-panel (+ index 1))
        ))
  
  ;; unselect all conditions
  (map (lambda (pnl)
         (select-panel pnl #f 'action)
         ) (get-selected-action-panel))
  ;; select new condition panel
  (select-panel new-action-panel #t 'action)
  
  ;; enable delete button since we there is now a selected panel
  (set-component-enabled delete-action-button #t)
  
  ;; need to do this to give new-panel a position
  (validate-container editlink-dialog)

  ;; scroll to newly added panel
  ;; top left point of new-panel relative to scrollpane
;  (define new-panel-tl-point (get-component-location new-action-panel))
;  (define tl-x (invoke new-panel-tl-point 'get-x))
;  (define tl-y (invoke new-panel-tl-point 'get-y))
;  (scroll-rect-to-visible action-list-panel (make-rectangle tl-x tl-y action-panel-width action-panel-height))
  
  ;; check whether rule is valid and enable ok button
  (validate-rule)
  
  ; resize if necessary
  (resize-all-action-panels)
  
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
                         (begin
                           (add-combobox-string action-type-choice "follow link to")
                           ;; select follow link when available
                           (set-combobox-selection-object action-type-choice (create-combobox-string-item "follow link to"))
                         ))
                     (if (equal? action-type "follow link to")
                         (begin
                           (add-combobox-string action-type-choice "show in popup")
                           ;; select follow link when available
                           (set-combobox-selection-object action-type-choice (create-combobox-string-item "follow link to"))
                           ))
                     ))
               
               ;; remove that panel from action-list-panel
               (remove-component action-list-panel action-panel)
               
               ;; the panel does not disappear even after we do remove-component thus we do this
               (component-update action-list-panel)
               ))
         ) (action-panel-list))
  (action-panel-restrict)
  (validate-rule)
  
  ; resize if necessary
  (resize-all-action-panels)
  
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
  (remove-show-popup-rule-display ruleID)
  
  (ask rule 'delaction actionID)
  (del 'actions actionID)
  
  ;; in case it is NOT a follow link action, we need to add the link back
  (add-follow-link-rule-display ruleID)
  (add-show-popup-rule-display ruleID)
  
  (hd-postedit 
   undo-manager
   (make-undoable-edit 
    "Delete Action"
    (lambda () ;;undo
      (remove-follow-link-rule-display ruleID)
      (remove-show-popup-rule-display ruleID)
      (eval-sexpr action-sexpr)
      (add-follow-link-rule-display ruleID)
      (add-show-popup-rule-display ruleID)
      )
    (lambda () ;; redo
      (define action (get 'actions actionID))
      (define ruleID (ask action 'ruleID))
      (define rule (get 'rules ruleID))
      (define action-sexpr (ask action 'to-save-sexpr))
      
      (remove-follow-link-rule-display ruleID)
      (remove-show-popup-rule-display ruleID)
      (ask rule 'delaction actionID)
      (del 'actions actionID)
      (add-follow-link-rule-display ruleID)
      (add-show-popup-rule-display ruleID)
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
  
  ;; up down button
  (define action-up-button (make-button "Up"))
  (define action-down-button (make-button "Down"))
  
  ;; label
  (define action-label (make-label-with-title " THEN perform the following actions:"))
  (define action-label-panel (make-panel))
  (set-container-layout action-label-panel 'horizontal)
  (add-component action-label-panel action-label)
  (add-component action-label-panel (create-horizontal-glue))
  (add-component action-label-panel action-up-button)
  (add-component action-label-panel action-down-button)
  
  ;;(set-align-x action-label 'left)
  ;;(set-align-x action-up-button 'right)
  ;;(set-align-x action-down-button 'right)
  
  (add-component actions-main-panel action-label-panel 'border-north)
  
  ;; list of actions
  (set! action-list-panel (make-panel))
  (set-container-layout action-list-panel 'vertical)
  ;(add-component actions-main-panel action-list-panel 'border-center)
  
    ;; scrollpane for condition panel
  (set! action-scrollpane (make-scrollpane-with-policy action-list-panel 'needed 'needed))
  (set-component-preferred-size action-scrollpane 
                                action-scrollpane-width action-scrollpane-height)
  (add-component actions-main-panel action-scrollpane 'border-center)
  
  (define vert-scrollbar (scroll-get-scrollbar action-scrollpane 'vert))
  (define scrollbar-width (get-preferred-width vert-scrollbar))
  (set! action-scrollpane-vp-width (- action-scrollpane-width scrollbar-width))
  (display "action scroll-vp-width ")(display action-scrollpane-vp-width)(newline)
  
  ;; reset the max width of condition
  (set! max-action-panel-width action-scrollpane-vp-width)
  
  ;; action type list contains the remaining available types left (after previous adding of actions)
  (set! action-type-choice (make-sorted-combobox))
  
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
  
  (add-actionlistener action-down-button 
                      (make-actionlistener (shift-callback 'action 'down)))
  
  (add-actionlistener action-up-button 
                      (make-actionlistener (shift-callback 'action 'up)))
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
(define action-type-list-anywhere-node (list "update fact" "enable links to this node from anywhere" "show disabled anywhere links"))
(define action-type-list-anywhere-node-link (list "update text using" "update fact"))

(define-constant unique-choices (list "update text using" "follow link to" "show in popup" "enable links to this node from anywhere" "show disabled anywhere links")) ;; choices that shouldnt be duplicated


;; an instance of the action type selector panel
;; updates the action choice combobox and returns a panel with a name label
(define (create-action-panel-common action-type) ; the-type )
  (if (not (eq? action-type #!null))
      (let* ((top-panel (make-panel))
             (the-checkbox (make-checkbox "")))
        ;; add top-panel
        (set-container-layout top-panel 'flow 'left)

        ;; add the action label display
        (define action-label (make-label-with-title (to-string action-type)))
        (add-component top-panel action-label)

        top-panel)
      (make-panel)))

(define (populate-condition-panel condID-lst)
  (map (lambda (mycond)
         (let* ((cond-obj (get 'conditions mycond))
                (the-type (ask cond-obj 'type))
                (targetID (ask cond-obj 'targetID))
                (operator (ask cond-obj 'operator))
                (numfact-args (ask cond-obj 'numfact-args)))

           ;; should allow edited note to be a choice in condition?
           (add-component condition-list-panel (create-condition-panel the-type targetID operator -1 numfact-args: numfact-args))))
       condID-lst)
  )

; reconstruct a rule
; this is called from the doeditlink/doeditnoderule/doeditdocrule procedures above
; to finish building the GUI representation of the link/rule
(define (populate-rule-editor edited-ruleID)
  (display "POPULATE RULE EDITOR ")(newline)
  (define rule-obj (get 'rules edited-ruleID))
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

        (display "AND OR expr ")(display expr)(newline)
        ;; set boolean operator "all/any"
        (set-combobox-selection editlink-dialog-andor-operator
                                (get-rule-pos expr)) ; add setting of operator

        ;; build conditions
        (populate-condition-panel conditions)

        (display "POPULATE ACTIONS ")(display actions)(newline)
        
        ;; build actions (show them in the ui)
        (map (lambda (actionID)
               (define action (get 'actions actionID))
               (define action-sexpr (ask action 'expr))

               (cond
                ((equal? 'follow-link (car action-sexpr))
                 (define dest-nodeID (list-ref action-sexpr 4))
                 (add-component action-list-panel (create-action-panel "follow link to" dest-nodeID)))

                ((equal? 'replace-link-text (car action-sexpr))  ;(replace-link-text text-type value linkID)
                 (display "inside replace link text ")(newline)
                 (display "action sexpr ")(display action-sexpr)(newline)
                 (define text-type (list-ref action-sexpr 1)) ;(using-type (car args-lst)) (alt-text (cadr args-lst)))
                 (define text-value (list-ref action-sexpr 2))
                 (display "text type ")(display text-type)(newline)
                 (display "text value ")(display text-value)(newline)

                 ;; 'text maps to "alternative text", 'fact maps to "text fact" 
                 (define text-type-string #f)
                 (add-component action-list-panel (create-action-panel "update text using" text-type text-value))
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

                 (add-component action-list-panel (create-action-panel "update fact" the-action targetID the-value))
                 )
                ((equal? 'set-number-fact (car action-sexpr))
                 (define the-action (list-ref action-sexpr 0))
                 (define targetID (list-ref action-sexpr 1)) ;; factID
                 (define num-fact-mode (list-ref action-sexpr 2))
                 (define the-value (list-ref action-sexpr 3))
                 
                 (display "ADDING update NUMBER fact ")(display the-value)(newline)
                 (add-component action-list-panel (create-action-panel "update fact" the-action targetID num-fact-mode the-value))
                 )
                ((equal? 'add-anywhere-link (car action-sexpr))
                 (add-component action-list-panel (create-action-panel "enable links to this node from anywhere"))
                 )
                ((equal? 'show-disabled-anywhere-link (car action-sexpr))
                 (add-component action-list-panel (create-action-panel "show disabled anywhere links"))
                 )
                ((equal? 'show-in-popup (car action-sexpr))
                 (define target-nodeID (list-ref action-sexpr 1))
                 (add-component action-list-panel (create-action-panel "show in popup" target-nodeID))
                 )
                )
               ) actions)
        (resize-all-action-panels)
        (resize-all-condition-panels)
        )))

;;  ===========================
;;;; Update text action panels (utap)
;;  ===========================

;(define update-text-action-panel #f)
;(define action-type-combobox #f)

;(define alt-text-textfield #f)
;(define fact-string-choice-combobox #f)
;(define fact-boolean-choice-combobox #f)
;(define fact-number-choice-combobox #f)

(define (utap-get-action-type-combobox utap)
  (let ((children (get-container-children utap)))
    (list-ref children 0)))

;; depending on whether "alternate text" "text fact" "number fact" is selected
;; this will return alt-text-textfield fact-string-choice-combobox fact-number-choice-combobox respectively
(define (utap-get-second-comp utap)
  (let ((children (get-container-children utap)))
    (list-ref children 1)))

;; the panel that comes behind the combobox selecting actions type
(define (create-update-text-action-panel)
  
  ;; new code
  (let ((top-panel (make-panel))
        (action-type-combobox (make-combobox "alternative text" "text fact" "number fact"))
        (alt-text-textfield (make-textfield "" 20))
        (fact-string-choice-combobox (create-fact-choice 'string -1))
        ;; TODO: we should be able to show boolean as well 
        (fact-boolean-choice-combobox (create-fact-choice 'boolean -1))
        (fact-number-choice-combobox (create-fact-choice 'number -1)))
    
    ;; layout
    (set-container-layout top-panel 'horizontal)
    (pack-component action-type-combobox)

    (add-component top-panel action-type-combobox)
    (add-component top-panel alt-text-textfield)
  
  ;; update the panel UI state for "update text using" action
  (define (action-update-text-combobox-callback)

    (define link-obj (get 'links edited-linkID))
    (define selected-item (to-string (get-combobox-selecteditem action-type-combobox)))

    (cond ((equal? selected-item "alternative text")
           (clear-container top-panel)
           (add-component top-panel action-type-combobox)
           (add-component top-panel alt-text-textfield)
           )
          ((or (equal? selected-item "text fact")
               (equal? selected-item "number fact"))
           (clear-container top-panel)
           (add-component top-panel action-type-combobox)

           ;; **** Explanation of how location and setter works 
           ;; location just gets a "pointer" to the variable, it is a getter function of the variable
           ;; (define x 1)
           ;; (define lx (location x))
           ;; (display (lx)) ==> 1
           ;; setter when given a location gets the setter function of that 
           ;;   particular variable associated with the location
           ;; ((setter lx) 4)
           ;; (display x) ==> 4
           ;; the pair of function therefore gives a means to pass a variable by reference

           ;; string/number fact differentiation
           (define fact-choice
             (case selected-item
               (("text fact") fact-string-choice-combobox)
               (("number fact") fact-number-choice-combobox)
               (else "HUH")
               ))

           (define fact-type
             (case selected-item
               (("text fact") 'string)
               (("number fact") 'number)))

           ;; get selection on fact-string-choice-combobox  
           ;; Note: this preserves our fact selection when we toggle between text and fact
           (define fact-selection #f)

           (if fact-choice
               (set! fact-selection (get-comboboxwithdata-selecteddata fact-choice)))

           ;; create and add the combobox containing the string facts
           ;;((setter fact-choice) (create-fact-choice fact-type fact-selection))
           (pack-component fact-choice)

           (add-actionlistener
            fact-choice
            (make-actionlistener
             (lambda (e)
               (display "fact-choice ")(newline)
               (validate-rule)
               )))

           ;; add to a container panel and add to the top-panel
           (add-component top-panel fact-choice)
           )
          ) ;; end of cond

    (pack-component top-panel)
    (pack-frame editlink-dialog)
    )
    
    (add-actionlistener action-type-combobox
                        (make-actionlistener
                         (lambda (e)
                           (display "action type combobox")(newline)
                           (action-update-text-combobox-callback)
                           (validate-rule)
                           )))
    top-panel))

(define (create-editlink-dialog parent)
  ; remember parent
  (set! editlink-dialog-parent parent)
  
  ; create edit link dialog
  (set! editlink-dialog (make-dialog-noparent-modal "Edit link"))
  (set-component-size editlink-dialog 400 100)
  (set-dialog-resizable editlink-dialog (show-actions?))
  (set! editlink-dialog-pane (get-dialog-content-pane editlink-dialog))
  (set-container-layout editlink-dialog-pane 'border)
  
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
                        (truncated-name
                         ;; truncate fact name at choice-name-limit
                         (if (> (string-length name) choice-name-limit)
                             (string-append (substring name 0 choice-name-limit) "...")
                             name))
                        (display-name (if (show-IDs?)
                                          (string-append truncated-name
                                                         " ("
                                                         (number->string thisnodeID)
                                                         ")")
                                          truncated-name))
                        (anywhere (ask thisnode 'anywhere?)))
                   (if (and (not (eq? in-edited-nodeID thisnodeID))
                            (not (and excludeAnywhere anywhere)))
                       (add-comboboxwithdata-string node-list display-name thisnodeID))))
               the-nodes)))

    ; make sure the "None" entry is explicitly inserted at the start of the list
    (insert-comboboxwithdata-string-at node-list "<None>" -1 0)
    
    ; select the chosen node
    (if nodeID
        ; note: set-comboboxwithdata-selection-bydata returns 'ok if failed, so check this
        (let ((returnval (set-comboboxwithdata-selection-bydata node-list nodeID)))
          (if (eq? 'ok returnval) (set-combobox-selection node-list 0)))
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
                        (truncated-name
                         ;; truncate fact name at choice-name-limit
                         (if (> (string-length name) choice-name-limit)
                             (string-append (substring name 0 choice-name-limit) "...")
                             name))
                        (display-name (if (show-IDs?)
                                          (string-append truncated-name
                                                         " ("
                                                         (number->string thislinkID)
                                                         ")")
                                          truncated-name)))
                   (add-comboboxwithdata-string link-list display-name thislinkID)))
               the-links)))

    ; make sure the "None" entry is explicitly inserted at the start of the list
    (insert-comboboxwithdata-string-at link-list "<None>" -1 0)
    
    ; select the chosen link
    (if linkID
        ; note: set-comboboxwithdata-selection-bydata returns 'ok if failed, so check this
        (let ((returnval (set-comboboxwithdata-selection-bydata link-list linkID)))
          (if (eq? 'ok returnval) (set-combobox-selection link-list 0)))
        (set-combobox-selection link-list 0))
    
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
                        
                        (truncated-name
                         ;; truncate fact name at 20
                         (if (> (string-length name) choice-name-limit)
                             (string-append (substring name 0 choice-name-limit) "...")
                             name))
                        
                        ;; name with ID
                        (display-name 
                         (if (show-IDs?)
                             (string-append truncated-name
                                            " ("
                                            (number->string thisfactID)
                                            ")")
                             truncated-name))
                        
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
        ; note: set-comboboxwithdata-selection-bydata returns 'ok if failed, so check this
        (let ((returnval (set-comboboxwithdata-selection-bydata fact-list factID)))
          (if (eq? 'ok returnval) (set-combobox-selection fact-list 0)))
        (set-combobox-selection fact-list 0))
    
    ; return the list
    fact-list))

;;  =============
;;;; conditions
;;  =============

; create a condition panel
; the-type: the type of condition (0=node, 1=link, 2=fact)
; targetID: the currently selected node/link, if any (pass in -1 if none selected)
; selected-operator: the operator for this condition (visited, not visited, or previous)
(define (create-condition-panel the-type targetID selected-operator in-edited-nodeID #!key numfact-args)
  
  ;; [condition panel [comparator-panel [ operand-choice ]  [ operand-choice ] ]]
  ;; [action panel [ fact-panel [math-panel [operand-choice]]
  
  ;; number fact comparator
  (define (make-comparator-panel top-panel)
    (let ((comparator-panel (make-panel))
          (comparator-cb (make-combobox "=" "<" ">" "<=" ">=")))
      (add-component comparator-panel comparator-cb)
      (add-component comparator-panel (make-operand-choice #f #f 'cond top-panel))
      (pack-component comparator-panel)
      comparator-panel))
  
  ;; operations on comparator panel
  (define (comparator-panel-sign-cb panel)
    (car (get-container-children panel))
    )
  (define (comparator-panel-operand-type-cb panel)
    (car (get-container-children
          (cadr (get-container-children panel)))))
  (define (comparator-panel-choice-comp panel)
    (cadr (get-container-children
          (cadr (get-container-children panel)))))
  
  (let* ((top-panel (make-panel))
         ;(the-checkbox (make-checkbox ""))
         (the-type-choice (if (is-basic-mode?)
                              (make-combobox "Node")
                              (if (show-facts?)
                                  (make-combobox "Node" "Link" "True/False Fact" "Number Fact")
                                  (make-combobox "Node" "Link"))))
         (the-node-list (create-node-choice targetID #f in-edited-nodeID))
         (the-link-list (create-link-choice targetID))
         (bool-fact-list (create-fact-choice 'boolean targetID))
         (num-fact-list (create-fact-choice 'number targetID))
         
         (the-node-operator-choice (if (not (is-basic-mode?))
                                       (make-combobox "Not Visited" "Visited" "Previous Node" "Not Previous Node")
                                       (make-combobox "Not Visited" "Visited")))
         (the-link-operator-choice (make-combobox "Not Followed" "Followed"))
         (the-fact-operator-choice (make-combobox "False" "True"))
         (comparator-panel (make-comparator-panel top-panel))
         )

    (format #t "Creating condition-panel: targetID:~a, in-edited-nodeID:~a~%~!" targetID in-edited-nodeID)
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;; Add type choice
    (if (not (is-basic-mode?))
        (begin
          (add-component top-panel the-type-choice)
          (set-combobox-selection the-type-choice the-type)
          (add-actionlistener the-type-choice
                              (make-actionlistener (lambda (source)
                                                     ;; remove the current target and operator lists
                                                     (clear-container top-panel)
                                                     (add-component top-panel the-type-choice)

                                                     ;; add new target and operator lists
                                                     (let* ((new-type (get-combobox-selectedindex the-type-choice))
                                                            (new-target (cond
                                                                         ((= 0 new-type) the-node-list)
                                                                         ((= 1 new-type) the-link-list)
                                                                         ((= 2 new-type) bool-fact-list)
                                                                         ((= 3 new-type) num-fact-list)
                                                                         ))
                                                            (new-operator (cond
                                                                           ((= 0 new-type) the-node-operator-choice)
                                                                           ((= 1 new-type) the-link-operator-choice)
                                                                           ((= 2 new-type) the-fact-operator-choice)
                                                                           ((= 3 new-type) comparator-panel)
                                                                           )))
                                                             (add-component top-panel new-target)
                                                             (add-component top-panel new-operator)
                                                       
                                                       ;; not the same for type 3 (num fact conditions)
                                                       (cond ((or (= 0 new-type)
                                                                  (= 1 new-type)
                                                                  (= 2 new-type))
                                                              (set-combobox-selection new-target 0)
                                                              (set-combobox-selection new-operator 0))
                                                             ((= 3 new-type)
                                                              ;; set color of parent condition panel
                                                              (set-background-color comparator-panel (get-background-color top-panel))
                                                              (recursively-colour-panels comparator-panel (get-background-color top-panel)))
                                                           )
                                                       )

                                                     (validate-rule)
                                                     
                                                     ; resize if necessary
                                                     (resize-all-condition-panels)

                                                     (component-update top-panel)
                                                     (pack-frame editlink-dialog)                                                     
                                                     ))))
        (add-component top-panel (make-label-with-title "Node")))
    
    
    ;; Check whether the condition is valid and
    ;; target choice action listener
    (add-actionlistener 
     the-node-list
     (make-actionlistener
      (lambda (e)
        ;(display "node list ")(newline)
        (validate-rule))))
    
    (add-actionlistener 
     the-link-list
     (make-actionlistener
      (lambda (e)
        ;(display "link list ")(newline)
        (validate-rule))))
    
    (add-actionlistener 
     bool-fact-list
     (make-actionlistener
      (lambda (e)
        ;(display "bool fact list ")(newline)
        (validate-rule))))
    
    (add-actionlistener 
     num-fact-list
     (make-actionlistener
      (lambda (e)
        ;(display "num fact list ")(newline)
        (validate-rule))))

    ;; Add target choice - may have problems if loading a file that doesn't match chosen version
    (cond 
     ((= 0 the-type) (add-component top-panel the-node-list))
     ((= 1 the-type) (add-component top-panel the-link-list))
     ((= 2 the-type) (add-component top-panel bool-fact-list))
     ((= 3 the-type) (add-component top-panel num-fact-list))
     )

    ;; add condition
    (let ((the-choice (cond 
                       ((= 0 the-type) the-node-operator-choice)
                       ((= 1 the-type) the-link-operator-choice)
                       ((= 2 the-type) the-fact-operator-choice)
                       ((= 3 the-type) comparator-panel))))
      (add-component top-panel the-choice)
      
      (if (not (= 3 the-type))
          (set-combobox-selection the-choice selected-operator)
          (if (pair? numfact-args)
              (begin
                (set-combobox-selection-object
                 (comparator-panel-sign-cb comparator-panel)
                 (create-combobox-string-item (car numfact-args)))
                
                (set-combobox-selection-object
                 (comparator-panel-operand-type-cb comparator-panel)
                 (create-combobox-string-item (cadr numfact-args)))
                
                (case (cadr numfact-args)
                  (("Input") (set-text (comparator-panel-choice-comp comparator-panel) (caddr numfact-args)))
                  (("Fact") 
                   (set-comboboxwithdata-selection-bydata
                             (comparator-panel-choice-comp comparator-panel)
                             (string->number (caddr numfact-args))))))
              )))
    
    ;; specify a fixed size
    (set! max-cond-panel-width
          (max condition-scrollpane-vp-width condition-scrollpane-vp-width))
    
    (set-component-non-resizable-size top-panel
                                      max-cond-panel-width
                                      cond-panel-height)
    
    (set-border top-panel bevel-in-border)
    
    ;; prevent the comboboxes from expanding to fill the panel 
    (pack-component the-node-list)
    (pack-component the-link-list)
    (pack-component bool-fact-list)
    (pack-component num-fact-list)
    
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
                  (map (lambda (pnl)
                         (if (not (equal? pnl top-panel))
                             (select-panel pnl #f 'cond)))
                       (get-container-children condition-list-panel))
                  )

              ;(select-rule-panel ruleID (not (panel-selected? top-panel)))
              (select-panel top-panel (not (panel-selected? top-panel)) 'cond)
              (condition-panel-restrict)
              ))
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

; cancelled, so hide newnode-dialog
(define (edit-rule-cancel)
  ;; if updatelevel is more than 0 it doeditlink was called right after donewlink
  ;; in that case a beginupdate had been called so close the compound undoable edit
  ;; by calling endupdate to close it
  (if (> (compoundundomanager-updatelevel undo-manager) 0)
      (hd-end-update undo-manager undo-action redo-action))
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
  
  ;; unselect all conditions
  (map (lambda (pnl)
         (select-panel pnl #f 'cond)
         ) (get-selected-condition-panel))
  ;; select new condition panel
  (select-panel new-cond-panel #t 'cond)
  
  ;; enable delete button since we there is now a selected panel
  (set-component-enabled delete-condition-button #t)
  
  ;; check whether rule is valid and enable ok button
  (display "edit rule cancel ")(newline)
  (validate-rule)
  
  ; resize if necessary
  (resize-all-condition-panels)
  
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
    (display "delete condition ")(newline)
    (validate-rule)

  ; resize if necessary
  (resize-all-condition-panels)
  
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
  (get-combobox-selectedindex (condition-panel-operator-cb panel)))

(define (condition-panel-valid? panel)
  ;; target id cannot be -1 
  ;;(make-combobox "Node" "Link" "Fact(#t/#f)" "Fact(num)")
  (let ((cond-type (condition-panel-target-type panel)))
    (cond ((<= cond-type 2)
           (not (= (condition-panel-target-id panel) -1)))
          ((= cond-type 3)
           (and (not (= (condition-panel-target-id panel) -1))
                (comparator-panel-valid? (condition-panel-operator-cb panel)))
           ))
    ))

(define (operand-panel-valid? panel)
  (let ((comp-lst (get-container-children panel)))
    (case (to-string (get-combobox-selecteditem (car comp-lst)))
      (("Input") (string-is-numeric? (get-text (cadr comp-lst))))
      (("Fact") (not (= (get-comboboxwithdata-selecteddata (cadr comp-lst)) -1)))
      )))

(define (comparator-panel-valid? panel)
  (define child-lst (get-container-children panel))
  (let ((operand-choice (cadr child-lst)))
    (operand-panel-valid? operand-choice)))

(define (all-conditions-valid?)
  (define (my-and lst)
    (if (null? lst) 
        #t
        (and (car lst) 
             (my-and (cdr lst)))))
  
  (my-and (map condition-panel-valid?
               (get-container-children condition-list-panel)))
  )

;; a duplicate of how selectable rule panel is implemented
;; this is used for both action and condition panels

(define (panel-selected? pnl)
  (equal? (get-background-color pnl) selected-color))

(define selected-color (make-colour-rgb 135 206 250))  ;; sky blue
(define unselected-color (make-colour-rgb 238 238 238))

; recursively colour the panels
(define (recursively-colour-panels the-panel the-colour)
  ;; color the child if it is a panel
  (map (lambda (child-comp)
         (if (javax.swing.JPanel? child-comp)
             (begin
               (set-background-color child-comp the-colour)
               (recursively-colour-panels child-comp the-colour)))
         ) (get-container-children the-panel)))


;; used by both action panel and condition panel
(define (select-panel pnl selected? type)
  (if selected?
      (begin
        (set-background-color pnl selected-color)
        (recursively-colour-panels pnl selected-color)

        ;; need to do this to give new-panel a position
        (validate-container editlink-dialog)

        ;; scroll to newly added panel
        ;; top left point of new-panel relative to scrollpane
        (define new-panel-tl-point (get-component-location pnl))
        (define tl-x (invoke new-panel-tl-point 'get-x))
        (define tl-y (invoke new-panel-tl-point 'get-y))
        
        ;; depending on the type, scroll to the panel on the correct list-panel
        (case type
          ((cond)
           (scroll-rect-to-visible
            condition-list-panel
            (make-rectangle tl-x tl-y cond-panel-width cond-panel-height)))
          ((action)
           (scroll-rect-to-visible
            action-list-panel
            (make-rectangle tl-x tl-y action-panel-width action-panel-height))
           )))
      (begin
        (set-background-color pnl unselected-color)
        (recursively-colour-panels pnl unselected-color)
        )))

;; TODO: shift this to component.scm or container.scm somewhere
;; problem is it belongs to component.scm but needs get-container-children from container.scm
;; skip-top-level allows us to skip packing for top most level
(define (recursive-pack-component comp :: <java.awt.Component>
                                  #!optional skip-top-level ) 
  (let ((children (get-container-children comp)))
    (map 
     (lambda (child-comp)
       (recursive-pack-component child-comp))
       children)
    (if (not skip-top-level)
        (pack-component comp))
    ))


(define (resize-selectable-panel ap)
  ;; pack all its children component
  (recursive-pack-component ap #t)
  
  ;; return the preferred width
  (get-preferred-width ap)
  )

;; type can be 'action or 'cond
(define (resize-all-selectable-panel type)
  
  (define all-selectable-panels 
    (get-container-children 
     (case type
       ((action) action-list-panel)
       ((cond) condition-list-panel))))
  
  (define width-lst 
    (map (lambda (selectable-panel)
           (resize-selectable-panel selectable-panel))
         all-selectable-panels))
  
  ;; update the maximum width for either action or condition panels
  (define max-panel-width-loc
    (case type
      ((action) (location max-action-panel-width))
      ((cond) (location max-cond-panel-width))))
  
  (define panel-height
    (case type
      ((action) action-panel-height)
      ((cond) cond-panel-height)))
        
  ;; update the maximum width 
  ((setter max-panel-width-loc)
   (apply max (cons (max-panel-width-loc) width-lst)))

  (map (lambda (selectable-panel)
         (set-component-non-resizable-size
          selectable-panel
          (max-panel-width-loc)
          panel-height)
         (component-revalidate selectable-panel))
       all-selectable-panels) ;; end of let
  )

(define (resize-all-action-panels)
  (resize-all-selectable-panel 'action))

(define (resize-all-condition-panels)
  (resize-all-selectable-panel 'cond))

;; resize parent action or condition panels with this
;; resizing is done by altering the maximum width or either action or cond panels
;; then set the max to that particular action/cond panel
(define (resize-parent comp levels action-or-cond)
  (if (> levels 0)
      (begin
        ;;(display "resize level ")(display levels)(newline)
        ;;(display "panel size here ")(display (get-preferred-width comp))(newline)
        
        (pack-component comp)
        
        (component-update comp)
        (component-revalidate comp)
        
        (let ((parent (get-parent comp)))
          (if (not (equal? #!null parent))
              (resize-parent parent (- levels 1) action-or-cond))))
      (begin
        (define parent-panel-width #f)
        (case action-or-cond
          ((action)
           (set! max-action-panel-width
                 (max max-action-panel-width
                      (max (get-preferred-width comp) action-scrollpane-vp-width)))
           (set! parent-panel-width max-action-panel-width))
          ((cond)
           (set! max-cond-panel-width
                 (max max-cond-panel-width
                      (max (get-preferred-width comp) condition-scrollpane-vp-width)))
           (set! parent-panel-width max-cond-panel-width)
           ))

        (set-component-non-resizable-size comp
                                          parent-panel-width
                                          action-panel-height)
        )))

;; operand choice returns a panel for inputing number facts value
;; 2 modes are provided now "Input" and "Fact" 
;; levels-to-parent is an int that gives the number of levels to go up before you hit the panel 
;;   we need to resize when  
;; top-panel can be the parent action panel or condition panel both are not direct parent
(define (make-operand-choice opr opr-type action-or-cond top-panel)
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
             )))
        (begin
          (set-text number-entry "0")
          (add-component operand-panel number-entry)
          (set-comboboxwithdata-selection-bydata number-choice -1)
          ;;(add-component operand-panel number-choice)
          )
        )
    
    (add-actionlistener
     mode-choice
     (make-actionlistener
      (lambda (e)
        ;;(display "mode choice trigger ")(newline)
        ;;(display "mode choice operand panel ")(newline)(display operand-panel)(newline)
        (remove-component operand-panel number-entry)
        (remove-component operand-panel number-choice)

        ;; add appropriate component
        (case (to-string (get-combobox-selecteditem mode-choice))
          (("Input") (add-component operand-panel number-entry))
          (("Fact") (add-component operand-panel number-choice)))

        (validate-rule)

        ;; update thie panel and parent panels
        ;(component-revalidate operand-panel)
        ;(component-update operand-panel)
        ;(pack-component operand-panel)
        ;(pack-component (get-parent operand-panel)) ;;math panel
        
        ;;(pack-component (get-parent (get-parent operand-panel))) ;; action panel

        ;; resize the panel on top
        (define levels-to-parent
          (case action-or-cond
            ((action) 3)
            ((cond) 2)))

        ;(resize-parent operand-panel levels-to-parent action-or-cond)
        (case action-or-cond
          ((action) (resize-all-action-panels))  ;(resize-all-action-panel))
          ((cond) (resize-all-condition-panels)))     ;(resize-all-condition-panel)))
        )))
    
    (add-actionlistener
     number-choice
     (make-actionlistener
      (lambda (e)
        ;;(display "number choice ")(newline)
        (validate-rule))
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

;; set the value in the operand choice panel
(define (set-operand-choice opr-panel opr opr-type action-or-cond)
  
  (define children-lst (get-container-children opr-panel))
  (define opr-type-cb (car children-lst))
  
  ;; set the type ("Input" or "Fact")
  (set-combobox-selection-object opr-type-cb (create-combobox-string-item opr-type))
  
  ;; get it again since setting opr-type-cb would change which component comes after it
  ;; a textfield for Input and another combobox for Fact
  (set! children-lst (get-container-children opr-panel))
  
  (define opr-component (cadr children-lst))
  
  (case opr-type
    (("Input")
     (set-text opr-component opr))
    (("Fact")
     (set-comboboxwithdata-selection-bydata opr-component (string->number opr))
     ))
  
  ;; an actionlistener resize the parent panels size to the correct with
  
  (set-combobox-selection-object opr-type-cb (create-combobox-string-item opr-type))
  
  (component-revalidate opr-panel)
  (component-update opr-panel)
  (pack-component opr-panel)
  ;;(pack-component (get-parent opr-panel))
  
  (display "operand hax ")(display(get-preferred-width opr-panel))(newline)
  )

;;  =============
;;;; fact panel
;;  =============

;; create math UI
;; action-panel the parent action-panel
(define (make-math-panel action-panel #!optional op opr1 opr1-type opr2 opr2-type)
  ;;(display "make math panel ")(display (list op opr1 opr1-type opr2 opr2-type))(newline)
  (define math-panel (make-panel))

  (define operator-choice (make-combobox "+" "-" "x" "/" "%"))

  (if (equal? "*" op)
      (set! op "x"))

  (if op
      (set-combobox-selection-object operator-choice (create-combobox-string-item op)))
  (set-container-layout math-panel 'horizontal)
  (add-components math-panel
                  (make-operand-choice opr1 opr1-type 'action action-panel)
                  operator-choice
                  (make-operand-choice opr2 opr2-type 'action action-panel))
  ;; to trigger the panel resize
  
  (pack-component math-panel)
  math-panel)

(define (set-math-panel math-panel val-lst)
  ;;(display "set math panel ")(display val-lst)(newline)
  (let ((op (list-ref val-lst 0))
        (opr1      (list-ref val-lst 1))
        (opr1-type (list-ref val-lst 2))
        (opr2      (list-ref val-lst 3))
        (opr2-type (list-ref val-lst 4))
        (comp-lst  (get-container-children math-panel)))

    (let ((opr1-cb (list-ref comp-lst 0))
          (op-cb   (list-ref comp-lst 1))
          (opr2-cb (list-ref comp-lst 2)))

      (if (equal? "*" op)
          (set! op "x"))

      (set-combobox-selection-object op-cb (create-combobox-string-item op))
      (set-operand-choice opr1-cb opr1 opr1-type 'action)
      (set-operand-choice opr2-cb opr2 opr2-type 'action)
      )))

(define (set-random-panel random-panel val-lst)
  (display "set random panel ")(display val-lst)(newline)
  (let ((opr1      (list-ref val-lst 0))
        (opr1-type (list-ref val-lst 1))
        (opr2      (list-ref val-lst 2))
        (opr2-type (list-ref val-lst 3))
        (comp-lst  (get-container-children random-panel)))

    (let ((opr1-cb (list-ref comp-lst 1))
          (opr2-cb (list-ref comp-lst 3)))
      (set-operand-choice opr1-cb opr1 opr1-type 'action)
      (set-operand-choice opr2-cb opr2 opr2-type 'action)
      )))

(define (make-random-panel action-panel #!optional opr1 opr1-type opr2 opr2-type)
  (define random-panel (make-panel))
  (set-container-layout random-panel 'horizontal)
  (define betw-label (make-label-with-title " between "))
  (define and-label (make-label-with-title " and "))
  (define opr-choice1 (make-operand-choice opr1 opr1-type 'action action-panel))
  (define opr-choice2 (make-operand-choice opr2 opr2-type 'action action-panel))
  (add-components random-panel
                  betw-label
                  opr-choice1
                  and-label
                  opr-choice2)
  
  (pack-component random-panel)
  random-panel)

; create an fact panel
; the-action: the type of action 'assert, 'retract or 'set-value!
; factID: the currently selected fact, if any (pass in -1 if none selected)
; the-value: value that will be set (for 'set-value! only)
;; fact type is only needed if a factID is selected
;; num-fact-mode is a string it can be one of the following ("Input" "Fact" "Math" "Random")
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
         (num-fact-mode-choice (make-combobox "Input" "Fact" "Math" "Random"))
         (math-panel #f)
         (random-panel #f)
         )
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;; add type choice
    (add-component top-panel the-type-choice)
    
    ;; choose the fact type index
    (define fact-type-index
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
    (set-combobox-selection-object the-type-choice (create-combobox-string-item fact-type-index)) ; alex is this the problem?
    
    (case fact-type-index
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
    (add-actionlistener 
     the-type-choice
     (make-actionlistener 
      (lambda (source)
        ;; remove the current target and operator lists
        (let* ((children (get-container-children top-panel))
               (fact-type-choice (car children))
               )
          (clear-container top-panel)
          (add-component top-panel fact-type-choice))

        ;; add the appropriate components and set value
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
           (add-component top-panel the-number-entry)
           
           ;; change the slot at the-number-entry based on what is 
           ;; appropriate through actionlistener on num-fact-mode-choice
           (set-combobox-selection-object 
            num-fact-mode-choice 
            (get-combobox-selecteditem num-fact-mode-choice))
           )) 

        ; resize if necessary
        (resize-all-action-panels)
        (pack-component top-panel)
        (pack-frame editlink-dialog)

        (validate-rule)
        )))

    (add-actionlistener
     the-fact-list-boolean
     (make-actionlistener
      (lambda (e)
        ;;(display "bool fact mode change ")(newline)
        (validate-rule))))
    
    (add-actionlistener
     the-fact-list-string
     (make-actionlistener
      (lambda (e)
        ;;(display "string fact mode change ")(newline)
        (validate-rule))))
    
    (add-actionlistener
     the-fact-list-number
     (make-actionlistener
      (lambda (e)
        ;;(display "num fact mode chosen ")(newline)
        (validate-rule))))
    
    (add-actionlistener
     number-fact-target-cb
     (make-actionlistener
      (lambda (e)
        ;;(display "num fact mode change")(newline)
        (validate-rule))))
    
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
        ;; remove fifth component
        (if (>= (length (get-container-children top-panel)) 5)
            (remove-component top-panel (list-ref (get-container-children top-panel) 4)))
        
        (case (to-string (get-combobox-selecteditem num-fact-mode-choice))
          (("Input") (add-component top-panel the-number-entry))
          (("Fact") (add-component top-panel number-fact-target-cb))
          (("Math")
           ;; only create math-panel once (to values when switching between modes)
           (if (not math-panel)
               (set! math-panel (make-math-panel top-panel)))
           (add-component top-panel math-panel)
           ;; set color of parent action panel
           (set-background-color math-panel (get-background-color top-panel))
           (recursively-colour-panels math-panel (get-background-color top-panel))
          )
          (("Random")
           (if (not random-panel)
               (set! random-panel (make-random-panel top-panel)))
           (add-component top-panel random-panel)
           ;; set color of parent action panel
           (set-background-color random-panel (get-background-color top-panel))
           (recursively-colour-panels random-panel (get-background-color top-panel))
           ))
        
        (resize-all-action-panels)
        
        (component-revalidate top-panel)
        (pack-component top-panel)
        
        (define parent-action-panel (get-parent top-panel))
        
        (if (not (equal? parent-action-panel #!null))
            (begin
              (set! max-action-panel-width
                    (max max-action-panel-width
                         (max (get-preferred-width parent-action-panel) action-scrollpane-vp-width)))

              (set-component-non-resizable-size parent-action-panel
                                                max-action-panel-width
                                                action-panel-height)))
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
               (display "text fact")(display the-value)(newline)
               (set-text the-string-entry the-value))
              
              ((equal? fact-type 'set-number-fact)
               (set-combobox-selection-object num-fact-mode-choice (create-combobox-string-item num-fact-mode))
               (case num-fact-mode
                 (("Input") (set-text the-number-entry (to-string the-value)))
                 (("Fact") (set-comboboxwithdata-selection-bydata number-fact-target-cb the-value))
                 (("Math")
                  (if math-panel 
                      (begin
                        (display "setting math panel values ")(newline)
                        (set-math-panel math-panel the-value)
                        ;; used to resize action panel 
                        (set-combobox-selection-object 
                         num-fact-mode-choice
                         (get-combobox-selecteditem num-fact-mode-choice))
                        )))
                 (("Random") 
                  (if random-panel 
                      (begin
                        (display "setting random panel values ")(newline)
                        (set-random-panel random-panel the-value)
                        ;; used to resize action panel
                        (set-combobox-selection-object 
                         num-fact-mode-choice
                         (get-combobox-selecteditem num-fact-mode-choice))
                      ))
                  ))
              )))
    
    (pack-component top-panel)
    ; return the panel
    top-panel))