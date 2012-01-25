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

; edit link dialogue, with rules and conditions
(begin
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
  (require "../common/objects.scm") ;; ask
  (require "../common/datatable.scm") ;; get, table-map
  (require "../common/main-ui.scm") ; for get-main-ui-frame
  (require "config-options.scm")
  (require "datastructure.scm")
  (require "hteditor.scm")
  (require "hypedyn-undo.scm")
  (require "nodeeditor.scm") ;; nodeeditor-save
  (require "htfileio.scm") ;; loaded-file-version
  
  (require "node-graphview.scm") ;update-link-display
  
  (require 'srfi-1) ;; remove
  )

; export
(module-export doeditlink doeditnoderule doeditdocrule
               create-editlink-dialog
               create-editnode-dialog
               
               create-facts-main-panel
               create-if-condition-panel
               create-then-action-panel
               create-actions-main-panel
               
               create-update-text-action-panel
               create-rules-manager
               
               obj-convertion-2.2)
               

; remember which link we're editing
(define-private edited-linkID '())

; remember currently edited node
;(define-private edited-nodeID '()) ;; now using nodeeditor.scm's edited-nodeID

; callback for updating links in graph
(define-private update-callback #f)

; are we editing a link (default) or just a rule?
(define-private edit-mode 'link)

(define (obj-convertion-2.2)
  (table-map 'links convert-pre-2.2-links)
  (table-map 'nodes convert-pre-2.2-nodes)
  )

(define (convert-pre-2.2-nodes nodeID node-obj)
  #f
  )

;; convertion of pre 2.2 links to 2.2 format
;; need to create 2 rules one for(if) and against(else) the condition in rule
(define (convert-pre-2.2-links linkID link-obj)
  (display "CONVERT LINKS pre 2.2")(newline)
  (let* (;(link-obj (get 'links linkID))
         (selected-rule-ID (ask link-obj 'rule))
         (selected-rule (get 'rules selected-rule-ID))
         ;(rule-lst (ask link-obj 'rule-lst))
         (link-name (ask link-obj 'name))
         (link-dest1 (ask link-obj 'destination))
         (link-uselink (ask link-obj 'use-destination))
         (link-usealtlink (ask link-obj 'use-alt-destination))
         (link-usealttext (ask link-obj 'use-alt-text))
         (link-dest2 (ask link-obj 'alt-destination))
         (link-alttext (ask link-obj 'alt-text))
         (link-start-index (ask link-obj 'start-index))
         (link-end-index (ask link-obj 'end-index))
;         (edited-node (get 'nodes edited-nodeID))
;         (anywhere (ask edited-node 'anywhere?))
         
;         (new-ruleexpression-pos (get-combobox-selectedindex editlink-dialog-andor-operator))
;         (new-ruleexpression (get-rule-exp new-ruleexpression-pos))
         
         ;(create-typed-rule2 name type and-or negate? parentID . args)
         (if-rule-ID (create-typed-rule2 link-name 'link (ask selected-rule 'expression) #f linkID))
         (else-rule-ID (create-typed-rule2 link-name 'link (ask selected-rule 'expression) #t linkID))
         (if-rule (get 'rules if-rule-ID))
         (else-rule (get 'rules else-rule-ID))
         )
    
    (if (<= loaded-file-version 2.1)
        (begin
          ;; alt-text (text and fact)
          (if link-usealttext
              (if (eq? link-usealttext #t)
                  (begin ;; show alt text
                    (display "[adding alt text action]")(newline)
                    (display "the actino ")(display (string-append "(replace-link-text " 
                                        "(quote text) "
                                        "\"" link-alttext "\"" " "
                                        (to-string linkID) ")"))(newline)
                    ;; add a new action panel
                    ;(add-specific-action "update text using" "alternative text" link-alttext)
                    (ask else-rule 'add-action!
                         (create-action link-name 'displayed-node
                                        (string-append "(replace-link-text "
                                                       "(quote text) "
                                                       "\"" link-alttext "\"" " "
                                                       (to-string linkID) ")")
                                        else-rule-ID
                                        ))
                    )
                  (begin ;; show fact text
                    (display "[adding alt text fact action] ")(newline)
                    (display "link-alttext ")(display link-alttext)(newline)
                    (display "the action ")(display (string-append "(replace-link-text " 
                                        "(quote text) "
                                        (to-string link-alttext) " " ;; here link-alttext is factID of the fact
                                        (to-string linkID) ")"))(newline)
                    ;(add-specific-action "update text using" "string fact" link-alttext)
                    (ask else-rule 'add-action! 
                         (create-action link-name 'displayed-node 
                                        (string-append "(replace-link-text "
                                                       "(quote text) "
                                                       (to-string link-alttext) " " ;; here link-alttext is factID of the fact
                                                       (to-string linkID) ")")
                                        else-rule-ID)
                         )
                    )))

                                        ;(define action-type-list (list "update text using" "follow link to" "update fact"))

          (display "link dest 1 ")(display link-dest1)(newline)
          (display "link dest 2 ")(display link-dest2)(newline)

          ;; default destination
          (if (not (equal? link-dest1 -1))
              (ask if-rule 'add-action!
                   (create-action link-name 'clicked-link
                                  (string-append "(follow-link "
                                                 (to-string linkID) " "
                                                 (to-string (ask if-rule 'ID)) " "
                                                 "(quote default) "
                                                 (to-string link-dest1) ")")
                                  if-rule-ID
                                  ))
              )

          ;; link-dest2 is used in the else so it should be in the not rules instead
          (if (not (equal? link-dest2 -1))
              (ask if-rule 'add-action!
                   (create-action link-name 'clicked-link
                                  (string-append "(follow-link "
                                                 (to-string linkID) " "
                                                 (to-string (ask if-rule 'ID)) " "
                                                 "(quote default) "
                                                 (to-string link-dest2) ")")
                                  else-rule-ID
                                  ))
              )

          (display "selected rule ID ")(display selected-rule-ID)(newline)
          ;; transfer the condition from the original old rule to the new rules
          ;(create-typed-condition name type targetID operator ruleID . args)
          (define old-conditions (ask selected-rule 'conditions))
          (map (lambda (condition)
                 (display "this cond ID ")(display condition)(newline)
                 ;(display "this cond ")(display (get 'conditions condition))(newline)
                 (define this-cond (get 'conditions condition))
;                 (display "name ")(display (ask this-cond 'name ))(newline)
;                 (display "type ")(display (ask this-cond 'type ))(newline)
;                  (display "targetID ")(display (ask this-cond 'targetID ))(newline)
;                  (display "ruleID ")(display (ask this-cond 'ruleID))(newline)
;                  (display "operator ")(display (ask this-cond 'operator))(newline)
                 (let ((type (ask this-cond 'type))
                       (targetID (ask this-cond 'targetID))
                       (operator (ask this-cond 'operator)))
                   (create-typed-condition "If-rule" type targetID operator if-rule-ID )
                   (create-typed-condition "else-rule" type targetID operator else-rule-ID ))
                 ) old-conditions)

          ))) ;; end of let
    )

; edit a link - called from hteditor.scm
;; in-default-link-text not used any more
(define (doeditlink selected-linkID in-edited-nodeID in-callback in-default-link-text)
  
  ;; cache a version of the unedited link for creation of undo
  (before-editlink selected-linkID)
  
  ; set editing mode
  (set! edit-mode 'link)
  
  ; set titles
  (set-tabpanel-label-at editlink-dialog-tabpanel 0 "Link")
  (set-tabpanel-label-at editlink-dialog-tabpanel 1 "Then action")
  (set-tabpanel-label-at editlink-dialog-tabpanel 2 "Else action")
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  
  ; remember which link we're editing
  (set! edited-linkID selected-linkID)
  
  ; remember which node we're editing
  (set! edited-nodeID in-edited-nodeID)
  
  ; remember callback
  (set! update-callback in-callback)
  
  ; get all the details from the link object
  (let* ((link-obj (get 'links edited-linkID))
         (selected-rule-ID (ask link-obj 'rule))
         (rule-lst (ask link-obj 'rule-lst))
         (link-name (ask link-obj 'name))
         (link-dest1 (ask link-obj 'destination))
         (link-uselink (ask link-obj 'use-destination))
         (link-usealtlink (ask link-obj 'use-alt-destination))
         (link-usealttext (ask link-obj 'use-alt-text))
         (link-dest2 (ask link-obj 'alt-destination))
         (link-alttext (ask link-obj 'alt-text))
         (link-start-index (ask link-obj 'start-index))
         (link-end-index (ask link-obj 'end-index))
         (edited-node (get 'nodes edited-nodeID))
         (anywhere (ask edited-node 'anywhere?)))
    
    ;; add appropriate panels to editlink-dialog
    (add-component editlink-panel-top editlink-panel-if)
;    (add-component editlink-panel-top editlink-panel-then)
    (add-component editlink-panel-top actions-main-panel)
    
    ; show link name
    (set-dialog-title editlink-dialog (string-append "Edit link: " 
                                                     link-name
                                                     (if (show-IDs?) (string-append " (" (number->string edited-linkID) ")") "")))

;    ; show destination node
;    (set! editlink-panel-then-link-choice
;          (create-node-choice editlink-panel-then-choicepanel
;                              selected-node-in-destination link-dest1
;                              #t -1)) ; allow link to self
;    (add-component editlink-panel-then-choicepanel editlink-panel-then-link-choice)

;    ; show alternative destination, if any
;    (set! editlink-panel-else-link-choice
;          (create-node-choice editlink-panel-else-choicepanel 
;                              selected-node-in-destination link-dest2
;                              #t -1)) ; allow link to self
;    (add-component editlink-panel-else-choicepanel editlink-panel-else-link-choice)
;    
;    ; show alternative text, if any: recreate the type choice, then determine which to show, fact or text
;    (set! editlink-panel-else-text-factchoice 
;          (create-fact-choice 'string editlink-panel-else-text 
;                              selected-fact-in-link -1))
;    (if link-usealttext
;        ; using alt text
;        (if (eq? link-usealttext #t)
;            ; set to true, so its plain text
;            (begin
;              ; if showing facts, set the type choice to text, which also shows correct component
;              (if (show-facts?)
;                  (set-combobox-selection editlink-panel-else-text-typechoice 0))
;              ; and set the text
;              (set-text editlink-panel-else-text-message1 link-alttext))
;            ; not true, so using a factID
;            (begin
;              ; and then recreate the fact choice
;              (set! editlink-panel-else-text-factchoice (create-fact-choice 'string editlink-panel-else-text 
;                                                                  selected-fact-in-link link-alttext))
;              ; set the type choice to fact, which also shows the correct component
;              (set-combobox-selection editlink-panel-else-text-typechoice 1))))


    ;; new code for new ui
    
    ;; empty the panel
    (clear-container action-list-panel)
    
    ;; reset combobox to contain all action-type-list
    ;(set! action-type-choice (apply make-combobox action-type-list))
    (reset-action-type-choice)
    
    (display "DOING NEW CODE ")(newline)
    
    ;; new code to update action list
    ;; this should be in the not part of the condition
    
    
    ;; end of new code
    
    ; set check boxes
;    (display "set check boxes")(newline)
;    (set-checkbox-value editlink-panel-then-link-check link-uselink)
;    (set-checkbox-value editlink-panel-else-link-check link-usealtlink)
;    (set-checkbox-value editlink-panel-else-text-check link-usealttext)

    ; set "ok" button state - cannot allow a "use" checkbox to be checked and a link to be "none"
    (set-button editlink-panel-buttons-ok
          (not
           (or
            (and link-uselink (eq? link-dest1 -1))
            (and link-usealtlink (eq? link-dest2 -1)))))

    ; enable if portion of editor
    ;(set-component-visible editlink-panel-if #t)
    
    ; enable then and else portions of editor except facts, and make sure its all set correctly
    ; also disable links if its an anywhere node
    ;(set-component-visible editlink-panel-then #t)
    
    ;; default text not needed anymore
    ;(set-component-visible editlink-panel-then-text #f)
    
    ;; default text not needed anymore
    ;(set-component-visible editlink-panel-then-link-message1-panel #t)
    ;(set-text editlink-panel-then-text-message1 "show default text: ")  ;; the label
    ;(set-component-visible editlink-panel-then-text-message1 #t)
    
    ;(set-text editlink-panel-then-text-message2 in-default-link-text) ;; the actual text
    ;(set-text-selection editlink-panel-then-text-message2 0 0)
    ;(set-component-visible editlink-panel-then-text-message2 #t)
    
    
    
    ; no links if its an anywhere node or in sculptural mode (no longer used)
;    (set-component-visible editlink-panel-then-link (and (not anywhere) (not (sculptural?))))
;    (set-component-visible editlink-panel-then-facts-labelpanel #f)
;    (set-component-visible editlink-panel-then-facts #f)
;    (set-component-visible editlink-panel-then-buttons #f)
    
    ;; else panel not used anymore
    ;(set-component-visible editlink-panel-else #f) ;; was #t
    
    ; no links if its an anywhere node or in sculptural mode
    ;(set-component-visible editlink-panel-else-link (and (not anywhere) (not (sculptural?))))
    
    ; reconstruct rule if necessary
    (populate-rule-editor selected-rule-ID in-edited-nodeID))
    
  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog editlink-dialog-parent)
  (set-component-visible editlink-dialog #t)
  )

;; edit-mode is either 'link 'node 'doc
(define (edit-rule edit-mode in-callback) ;;selected-ruleID 
  
  ;; cache a version of the unedited link for creation of undo
  (before-editlink selected-linkID)
  
  ; set editing mode
  ;(set! edit-mode 'link)
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  
  ; remember which link we're editing
  ;(set! edited-linkID selected-linkID)
  
  ; remember which node we're editing
  ;(set! edited-nodeID selected-nodeID)
  
  
  (if (equal? edit-mode 'link)
  ; get all the details from the link object
      (let* ((link-obj (get 'links edited-linkID))
             (selected-rule-ID (ask link-obj 'rule))
             (rule-lst (ask link-obj 'rule-lst))
             (link-name (ask link-obj 'name))
             (link-dest1 (ask link-obj 'destination))
             (link-uselink (ask link-obj 'use-destination))
             (link-usealtlink (ask link-obj 'use-alt-destination))
             (link-usealttext (ask link-obj 'use-alt-text))
             (link-dest2 (ask link-obj 'alt-destination))
             (link-alttext (ask link-obj 'alt-text))
             (link-start-index (ask link-obj 'start-index))
             (link-end-index (ask link-obj 'end-index))
             (edited-node (get 'nodes edited-nodeID))
             (anywhere (ask edited-node 'anywhere?)))

                                        ; remember callback
        (set! update-callback in-callback)

        ;; add appropriate panels to editlink-dialog
        (add-component editlink-panel-top editlink-panel-if)
        (add-component editlink-panel-top editlink-panel-then)
        (add-component editlink-panel-top actions-main-panel)

                                        ; show link name
        (set-dialog-title editlink-dialog (string-append "Edit link: "
                                                         link-name
                                                         (if (show-IDs?) (string-append " (" (number->string edited-linkID) ")") "")))

                                        ; show destination node
        (set! editlink-panel-then-link-choice
              (create-node-choice editlink-panel-then-choicepanel
                                  selected-node-in-destination link-dest1
                                  #t -1)) ; allow link to self
        (add-component editlink-panel-then-choicepanel editlink-panel-then-link-choice)

                                        ; show alternative destination, if any
        (set! editlink-panel-else-link-choice
              (create-node-choice editlink-panel-else-choicepanel
                                  selected-node-in-destination link-dest2
                                  #t -1)) ; allow link to self
        (add-component editlink-panel-else-choicepanel editlink-panel-else-link-choice)

                                        ; show alternative text, if any: recreate the type choice, then determine which to show, fact or text
        (set! editlink-panel-else-text-factchoice
              (create-fact-choice 'string editlink-panel-else-text
                                  selected-fact-in-link -1))

        (if link-usealttext
                                        ; using alt text
            (if (eq? link-usealttext #t)
                                        ; set to true, so its plain text
                (begin
                                        ; if showing facts, set the type choice to text, which also shows correct component
                  (if (show-facts?)
                      (set-combobox-selection editlink-panel-else-text-typechoice 0))
                                        ; and set the text
                  (set-text editlink-panel-else-text-message1 link-alttext))
                                        ; not true, so using a factID
                (begin
                                        ; and then recreate the fact choice
                  (set! editlink-panel-else-text-factchoice (create-fact-choice 'string editlink-panel-else-text
                                                                                selected-fact-in-link link-alttext))
                                        ; set the type choice to fact, which also shows the correct component
                  (set-combobox-selection editlink-panel-else-text-typechoice 1))))


        ;; new code for new ui

        ;; empty the panel
        (clear-container action-list-panel)

        ;; reset combobox to contain all action-type-list
        ;(set! action-type-choice (apply make-combobox action-type-list))
        (reset-action-type-choice)

        (display "DOING NEW CODE ")(newline)

        ;; new code to update action list
        ;; this should be in the not part of the condition
        (if link-usealttext
            (if (eq? link-usealttext #t)
                (begin ;; show alt text
                  ;; add a new action panel
                  (add-specific-action "update text using" "alternative text" link-alttext)
                  )
                (begin ;; show fact text
                  (add-specific-action "update text using" "string fact" link-alttext)
                  )))

                                        ;(define action-type-list (list "update text using" "follow link to" "update fact"))

        (if (not (equal? link-dest1 -1))
            (begin
              (add-specific-action "follow link to" link-dest1)
                                        ;(define dest-node-name (ask (get 'nodes link-dest1) 'name))
                                        ;(set-combobox-selection-object node-choice-combobox (create-combobox-string-item dest-node-name))
              ))

        ;; link-dest1 is used in the else so it should be in the not rules instead
        ;; do later (alt dest (not))
        ;    (if link-dest2
        ;        (begin
        ;          (add-specific-action "follow link to")
        ;          ))

        (display "selected rule ID ")(display selected-rule-ID)(newline)

        (if (not (equal? selected-rule-ID 'not-set))
            (begin
              (define rule-obj (get 'rules selected-rule-ID))
                                        ;(display "rule obj ")(display rule-obj)(newline)
              (define facts (ask rule-obj 'actions))
                                        ;(display "facts ")(display facts)(newline)
              (format #t "building facts: ~a~%~!" facts)
              (map (lambda (myaction)
                     (let* ((action-obj (get 'actions myaction))
                            (the-action-string (ask action-obj 'expr))
                            (the-action-expr (read (open-input-string the-action-string))) ; hack, should change to saving as sexpr
                            (the-action (car the-action-expr))
                            (targetID (cadr the-action-expr))
                            (the-value (if (eq? the-action 'set-value!)
                                           (caddr the-action-expr)
                                           "")))
                       (format #t "fact: ~a (~a, ~a, ~a)~%~!" the-action-expr the-action the-value targetID)
                       (add-specific-action "update fact" (list the-action targetID the-value))

                                        ;(define the-panel (add-specific-action "update fact" (list the-action targetID the-value)))
                                        ;(add-component the-panel (create-fact-panel2 the-action targetID the-value))
                       ))
                   facts)
              ))


        ;; end of new code

        ; reconstruct rule if necessary
        (populate-rule-editor selected-rule-ID in-edited-nodeID)))
  
  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog editlink-dialog-parent)
  (set-component-visible editlink-dialog #t)
  )

; edit a node rule - removes the link-specific portions of the editor
(define (doeditnoderule in-edited-nodeID)
  (format #t "doeditrule: in-edited-nodeID: ~a~%~!" in-edited-nodeID)
  
  ; set edit mode to rule
  (set! edit-mode 'node)
  (nodeeditor-save) ;; save text content before create sexpr in before-editnode
  (before-editnode in-edited-nodeID)
  
  ; set titles
  (set-tabpanel-label-at editlink-dialog-tabpanel 0 "Rule")
  (set-tabpanel-label-at editlink-dialog-tabpanel 1 "Before action")
  (set-tabpanel-label-at editlink-dialog-tabpanel 2 "After action")
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  
  ; remember which link we're editing ie. none
  (set! edited-linkID '())
  
  ; remember which node we're editing
  (set! edited-nodeID in-edited-nodeID)
  
  ; remember callback ie. none
  ;; why do update-callback #f?
  (set! update-callback #f)
  
  ; get the rule from the node
  (let* ((selected-rule (ask (get 'nodes edited-nodeID) 'rule))
         (edited-node (get 'nodes edited-nodeID))
         (node-name (ask edited-node 'name))
         (anywhere (ask edited-node 'anywhere?)))
    
    ; show node name
    (set-dialog-title editlink-dialog (string-append "Edit rule for node: " node-name))

    ;; add if panel to editnode-dialog
    (add-component editlink-panel-top editlink-panel-if)
    (add-component editlink-panel-top editlink-panel-then)
    
    ;; build 'then' panel
    (add-component editlink-panel-then editlink-panel-then-link-message1-panel)
    (add-component editlink-panel-then editlink-panel-then-text)
    ;(add-component editlink-panel-then editlink-panel-then-link)
    
    (add-component editlink-panel-then editnode-panel-then-facts-labelpanel)
    (add-component editlink-panel-then editnode-panel-then-facts)
    (add-component editlink-panel-then editnode-panel-then-buttons)
    
    ;; nothing in then panel except the "enable links to this node" label editlink-panel-then-text-message1
    ; no links if its an anywhere node or in sculptural mode 
;    (set-component-visible editlink-panel-else-link (and (not anywhere) (not (sculptural?))))
    ;(set-component-visible editlink-panel-else-link #f)
    
    ; enable if portion of editor
    ;(set-component-visible editlink-panel-if anywhere)
    
    ; enable then portion of editor if its an anywhere node, or if its not but facts are showing,
    ; but change label and disable link
;    (set-component-visible editlink-panel-then (or 
;                                                anywhere
;                                                (show-facts?)))
;    (set-component-visible editlink-panel-then-link-message1-panel anywhere)
    (set-text editlink-panel-then-text-message1 "enable links to this node")
;    (set-component-visible editlink-panel-then-text-message1 anywhere)
    (set-component-visible editlink-panel-then-text-message2 #f)
;    (set-component-visible editlink-panel-then-link #f)
    
    ;; not in editlink anymore
;    (set-component-visible editlink-panel-then-facts-labelpanel (show-facts?))
;    (set-component-visible editlink-panel-then-facts (show-facts?))
;    (set-component-visible editlink-panel-then-buttons (show-facts?))
    
    ; disable else portion of editor
    ;; else panel not used anymore
;    (set-component-visible editlink-panel-else #f)  was #f
    
    ; reconstruct rule if necessary
    (populate-rule-editor selected-rule in-edited-nodeID)
    )

  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog editlink-dialog-parent)
  (set-component-visible editlink-dialog #t)
  ;(set-component-visible editnode-dialog #t)
  )

; edit document rule
; for now, just one action that is run at start
(define (doeditdocrule)
  (format #t "doeditdocrule~%~!")
  
  ; set edit mode to rule
  (set! edit-mode 'doc)
  
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
  
  ; remember callback ie. none
  (set! update-callback #f)
  
  ; get the rule
  (let ((selected-rule (get-document-ruleID)))
    ; show node name
    (set-dialog-title editlink-dialog "Edit document rule")

    ; enable if portion of editor
    (set-component-visible editlink-panel-if #t)
    
    ; disable then and else portions of editor
    (set-component-visible editlink-panel-then #f)
    ;; else panel not used anymore
    (set-component-visible editlink-panel-else #f) ;; was #f
    
    ; reconstruct rule if necessary
    (populate-rule-editor selected-rule -1))

  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog (get-main-ui-frame))
  (set-component-visible editlink-dialog #t))

;(doeditlink selected-linkID in-edited-nodeID in-callback in-default-link-text)
;(doeditnoderule in-edited-nodeID)
;(doeditdocrule)

(define rules-manager-main-dialog #f)
(define rules-list-panel #f)

;; one instance of the rule panel (one entry on the rule list)
(define (make-rule-panel #!optional rule-name)
  (define top-panel (make-panel))
  (set-container-layout top-panel 'flow 'left)

  (define rule-checkbox (make-checkbox ""))

  (define rule-name-label 
    (if rule-name
        (make-label-with-title rule-name)
        (make-label-with-title "New Rule")
        ))
  
  ;(define rule-name-label (make-label-with-title "SIGH"))
  
  (define rule-edit-button (make-button "Edit Rule"))
  (add-actionlistener rule-edit-button 
                      (make-actionlistener
                       (lambda (e)
                         (define sibling-lst (get-container-children (get-parent top-panel)))
                         (define pos (list-index (lambda (panel) (eq? top-panel panel)) sibling-lst))
                         (cond ((equal? edit-mode 'link)
                                (doeditlink edited-linkID edited-nodeID update-link-display "")
                                )
                               ((equal? edit-mode 'node)
                                #f
                                )
                               ((equal? edit-mode 'doc)
                                #f
                                ))
                         )))

  (add-component top-panel rule-checkbox)
  (add-component top-panel rule-name-label)
  (add-component top-panel rule-edit-button)

  top-panel)

;; add rule (makes a rule panel inside rules-list-panel)
(define (add-rule-panel ;. rule-name) ;; strangely using #!optional here for rulename just crashes kawa (SIGH)
                  #!optional rule-name)
  (if (and rules-list-panel
           rules-manager-main-dialog)
      (begin
;        (if rule-name
;            (add-component rules-list-panel (make-rule-panel rule-name))
;            (add-component rules-list-panel (make-rule-panel)))
        (add-component rules-list-panel (make-rule-panel rule-name))
        (pack-frame rules-manager-main-dialog)
        ))
  )

(define mgr-rule-lst (list))



;; target type might be 'link 'node 'doc
;; display the dialog with the list of rules attached to the object (which can be any of the target-type)
(define (create-rules-manager target-type . args) ;; target-type not used yet
  (set! rules-manager-main-dialog (make-dialog (get-nodeeditor-frame) "Rule Editor" #t))
  
  (define rules-manager-main-panel (make-panel))
  (set-container-layout rules-manager-main-panel 'border)
  (add-component rules-manager-main-dialog rules-manager-main-panel)
  
  ;; label display
  (define rules-label (make-label-with-title "Rules"))
  (define rules-label-panel (make-panel))
  (add-component rules-label-panel rules-label)
  (add-component rules-manager-main-panel rules-label-panel 'border-north)
  
  (define center-panel (make-panel))
  (set-container-layout center-panel 'vertical)
  (add-component rules-manager-main-panel center-panel 'border-center)
  
  ;; list of rules
  (set! rules-list-panel (make-panel))
  (set-container-layout rules-list-panel 'vertical)
  (add-component center-panel rules-list-panel)
  
  ;;rule list buttons
  (define add-rule-button (make-button "Add Rule"))
  (define delete-rule-button (make-button "Delete Selected"))
  (define rule-list-button-panel (make-panel))
  (add-component rule-list-button-panel add-rule-button)
  (add-component rule-list-button-panel delete-rule-button)
  (add-component center-panel rule-list-button-panel)
  
  (define rules-dialog-button-panel (make-panel))
  (set-container-layout rules-dialog-button-panel 'flow 'right)
  (add-component rules-manager-main-panel rules-dialog-button-panel 'border-south)
  
  ;; dialog button
  (define rules-dialog-close (make-button "Close"))
  ;(define rules-dialog-cancel (make-button "Cancel"))
  (add-component rules-dialog-button-panel rules-dialog-close)
  ;(add-component rules-dialog-button-panel rules-dialog-cancel)
  
  (add-actionlistener add-rule-button 
                      (make-actionlistener
                       (lambda (e)
                         (add-rule-panel #f)
;                         (add-component rules-list-panel (make-rule-panel))
;                         (pack-frame rules-manager-main-dialog)
                         )))
  
  (add-actionlistener rules-dialog-close
                      (make-actionlistener
                       (lambda (e)
                         (set-component-visible rules-manager-main-dialog #f)
                         )))
  
  (pack-frame rules-manager-main-dialog)
  (set-component-visible rules-manager-main-dialog #t)
  )

; reconstruct a rule
; this is called from the doeditlink/doeditnoderule/doeditdocrule procedures above
; to finish building the GUI representation of the link/rule
(define (populate-rule-editor edited-ruleID in-edited-nodeID)
  ;(format #t "populate-rule-editor, edited-ruleID: ~a, in-edited-nodeID: ~a~%~!" edited-ruleID in-edited-nodeID)
  (if (not (eq? 'not-set edited-ruleID))
      (let* ((rule-obj (get 'rules edited-ruleID))
             (conditions (ask rule-obj 'conditions))
             (facts (ask rule-obj 'actions))
             (rule-name (ask rule-obj 'name))
             (expr (ask rule-obj 'expression))
             (then-action-ID (ask rule-obj 'then-action))
             (else-action-ID (ask rule-obj 'else-action)))
        ; set actions in code editor
        (if then-action-ID
            (let ((the-action (get 'actions then-action-ID)))
              (if the-action
                  (begin
                    (set-text editlink-dialog-then-actiontext (ask the-action 'expr))
                    (set-cursor-pos editlink-dialog-then-actiontext 0)))))
        (if else-action-ID
            (let ((the-action (get 'actions else-action-ID)))
              (if the-action
                  (begin
                    (set-text editlink-dialog-else-actiontext (ask the-action 'expr))
                    (set-cursor-pos editlink-dialog-else-actiontext 0)))))

        ; set boolean operator "all/any"
        (set-combobox-selection editlink-dialog-andor-operator
                                (get-rule-pos expr)) ; add setting of operator
        
        ;; added in version 2.2
        (if (>= loaded-file-version 2.2)
            (begin
              (define negate? (ask rule-obj 'negate?)) ;; we can't be sure whether this is old rule or new rule obj
              ;; set negate operator "if/unless"
              (set-combobox-selection editlink-dialog-negate-operator
                                      (if negate? 1 0))))

        ; build conditions
        (map (lambda (mycond)
               (let* ((cond-obj (get 'conditions mycond))
                      (the-type (ask cond-obj 'type))
                      (targetID (ask cond-obj 'targetID))
                      (operator (ask cond-obj 'operator)))
                 (create-condition-panel the-type targetID operator -1))) ;in-edited-nodeID))) ; should allow edited note to be a condition?
             conditions)

        ; build facts
;        (format #t "building facts: ~a~%~!" facts)
;        (map (lambda (myaction)
;               (let* ((action-obj (get 'actions myaction))
;                      (the-action-string (ask action-obj 'expr))
;                      (the-action-expr (read (open-input-string the-action-string))) ; hack, should change to saving as sexpr
;                      (the-action (car the-action-expr))
;                      (targetID (cadr the-action-expr))
;                      (the-value (if (eq? the-action 'set-value!)
;                                     (caddr the-action-expr)
;                                     "")))
;                 (format #t "fact: ~a (~a, ~a, ~a)~%~!" the-action-expr the-action the-value targetID)
;                 (create-fact-panel the-action targetID the-value)))
;             facts)
        )))

;;
;; build the UI
;;
(define editlink-dialog-parent #f)
(define editlink-dialog #f)
(define editlink-dialog-pane #f)

;;not used atm
(define editlink-dialog-tabpanel #f)
(define editlink-dialog-linkpanel #f)
(define editlink-dialog-then-actiontext #f)
(define editlink-dialog-else-actiontext #f)
(define editlink-panel-top #f)
(define editlink-panel-if #f)
(define editlink-panel-follow2 #f)
;(define editlink-dialog-operater-label #f)
(define editlink-dialog-negate-operator #f)
(define editlink-dialog-andor-operator #f)
(define editlink-dialog-message3 #f)
(define editlink-panel-conditions #f)
(define editlink-panel-main-buttons #f)
(define editlink-panel-main-buttons-add #f)
(define editlink-panel-main-buttons-delete #f)
(define editlink-panel-then #f)
(define editlink-panel-then-link-message1 #f)
(define editlink-panel-then-link-message1-panel #f)
(define editlink-panel-then-text #f)
(define editlink-panel-then-text-message1 #f)
(define editlink-panel-then-text-message2 #f)
(define editlink-panel-then-link #f)
(define editlink-panel-then-link-check #f)
(define editlink-panel-then-choicepanel #f)
(define editlink-panel-then-link-choice #f)
(define editlink-panel-then-facts-labelpanel #f)
(define editlink-panel-then-facts-label #f)
(define editlink-panel-then-facts #f)
(define editlink-panel-then-buttons #f)
(define editlink-panel-then-buttons-add #f)
(define editlink-panel-then-buttons-delete #f)
(define editlink-panel-else #f)
(define editlink-panel-else-message1-panel #f)
(define editlink-panel-else-message1 #f)
(define editlink-panel-else-text #f)
(define editlink-panel-else-text-check #f)
(define editlink-panel-else-text-typechoice #f)
(define editlink-panel-else-text-factchoice #f)
(define editlink-panel-else-text-message1 #f)
(define editlink-panel-else-link #f)
(define editlink-panel-else-link-check #f)
(define editlink-panel-else-choicepanel #f)
(define editlink-panel-else-link-choice #f)
(define editlink-panel-buttons #f)
(define editlink-panel-buttons-cancel #f)
(define editlink-panel-buttons-ok #f)

;; if condition panel (init editlink-panel-if)
(define (create-if-condition-panel)

  ;; start of IF conditions

  ; panel for "if" part of rule
  (set! editlink-panel-if (make-panel))
  (set-container-layout editlink-panel-if 'vertical)
  
  ;; will add when we need it
  ;(add-component editlink-panel-top editlink-panel-if)

  ; panel for "all"/"any" selection
  (set! editlink-panel-follow2 (make-panel))
  (set-container-layout editlink-panel-follow2 'flow 'left)
  (add-component editlink-panel-if editlink-panel-follow2)

  ;; obsolete ("IF" becomes a combobox that has UNLESS as the opposite of IF) 
;  (set! editlink-dialog-operater-label (make-label-with-title "IF"))
;  (add-component editlink-panel-follow2 editlink-dialog-operater-label)
  
  ;; "if" / "unless" selection
  (set! editlink-dialog-negate-operator (make-combobox "If" "Unless"))
  (add-component editlink-panel-follow2 editlink-dialog-negate-operator)
  
  ; "all"/"any" selection
  (set! editlink-dialog-andor-operator (make-combobox "All" "Any"))
  (add-component editlink-panel-follow2 editlink-dialog-andor-operator)
  
  ; 

  ; label for "of the following conditions are true:"
  (set! editlink-dialog-message3 (make-label))
  (set-text editlink-dialog-message3 "of the following conditions are true:")
  (add-component editlink-panel-follow2 editlink-dialog-message3)

  ;;condition panel
  (set! editlink-panel-conditions (make-panel))
  (set-container-layout editlink-panel-conditions 'vertical)
  (add-component editlink-panel-if editlink-panel-conditions)

  ;; Add a horizontal panel to the dialog, with centering for buttons
  (set! editlink-panel-main-buttons (make-panel))
  (set-container-layout editlink-panel-main-buttons 'horizontal)
  (add-component editlink-panel-if editlink-panel-main-buttons)

  ;; Add ADD button
  (set! editlink-panel-main-buttons-add (make-button "Add condition"))
  (add-actionlistener editlink-panel-main-buttons-add
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-add))))
  (add-component editlink-panel-main-buttons editlink-panel-main-buttons-add)

  ;; Add DELETE button
  (set! editlink-panel-main-buttons-delete (make-button "Delete selected"))
  (add-actionlistener editlink-panel-main-buttons-delete
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-delete-conditions))))
  (add-component editlink-panel-main-buttons editlink-panel-main-buttons-delete)
  ) ;; end of IF conditions

;; panel for setting actions (init editlink-panel-then)
(define (create-then-action-panel)
  ;; start of THEN action

  ; panel to hold all the "then" information
  (set! editlink-panel-then (make-panel))
  (set-container-layout editlink-panel-then 'vertical)
  
  ;; add when we building the dialog
  ;(add-component editlink-panel-top editlink-panel-then)

  ;the THEN label
  (set! editlink-panel-then-link-message1-panel (make-panel))
  (set-container-layout editlink-panel-then-link-message1-panel 'flow 'left)
;  (add-component editlink-panel-then editlink-panel-then-link-message1-panel)
  (set! editlink-panel-then-link-message1 (make-label))
  (set-text editlink-panel-then-link-message1 "THEN") ;; was "THEN"
  (add-component editlink-panel-then-link-message1-panel editlink-panel-then-link-message1)

  ; panel to hold the "if" text
  (set! editlink-panel-then-text (make-panel))
  (set-container-layout editlink-panel-then-text 'flow 'left)
;  (add-component editlink-panel-then editlink-panel-then-text)

  ; default link text label
  ; TODO: fix layout when editing anywhere node rule
  ; note: this is only problematic because I've hacked the dialog to resize for 
  ; editing code, so this isn't worth fixing for now - alex
  (set! editlink-panel-then-text-message1 (make-label))
  (set-text editlink-panel-then-text-message1 "show default text: ")
  (if (not (is-basic-mode?))
      (add-component editlink-panel-then-text editlink-panel-then-text-message1))

  ; default link text content
  (set! editlink-panel-then-text-message2 (make-textfield "[default text...........]" 20))
  (set-text-component editlink-panel-then-text-message2 #f #t)
  ;(set-text editlink-panel-then-text-message2 "[default text...........]")
  (add-component editlink-panel-then-text editlink-panel-then-text-message2)

  ; panel to hold the "if" link
  (set! editlink-panel-then-link (make-panel))
  (set-container-layout editlink-panel-then-link 'horizontal)
  ;(add-component editlink-panel-then editlink-panel-then-link)

  ;; Add checkbox
  (set! editlink-panel-then-link-check (make-checkbox "follow link to"))
  (if (not (is-basic-mode?))
      (begin
        (add-itemlistener editlink-panel-then-link-check
                          (make-itemlistener (lambda (source newstate)
                                               (link-check-changed source newstate))))
        (add-component editlink-panel-then-link editlink-panel-then-link-check))
      (add-component editlink-panel-then-link (make-label-with-title "follow link to")))

  ; panel to hold the destination choice
  (set! editlink-panel-then-choicepanel (make-panel))
  (set-container-layout editlink-panel-then-choicepanel 'horizontal)
  (add-component editlink-panel-then-link editlink-panel-then-choicepanel)

  ; choice for destination - created dynamically when rule editor is loaded
  (set! editlink-panel-then-link-choice #f)

  ;; end of THEN action
  )

(define actions-main-panel #f)
(define action-list-panel #f)
(define action-type-choice #f)

(define (init-action-type-choice)
  (display "init action type choice ")(newline)
  (set! action-type-choice (apply make-combobox action-type-list))
  (display "action-type-list ")(display action-type-list)(newline)
  
  )

(define (reset-action-type-choice)
  (display "b4 set combobox clear")(newline) 
  (set-combobox-clear action-type-choice)
  
  (define (clear-combobox-hack combobox)
    (define this-item (get-combobox-item-at combobox 0))
    (display "clear combobox hack ")(display this-item)(newline)
    ;; check on the first time whether combobox has anything inside
    (display "combo count ")(display (get-combobox-item-count combobox))(newline)
    (display "first ele ")(display (get-combobox-item-at combobox 0))(newline)
    (if (> (get-combobox-item-count combobox) 0)
        (remove-combobox-item combobox (get-combobox-item-at combobox 0)))
    
    (display "aft clear combobox hack ")(display this-item)(newline)
    ;; if after remove still more than 
    (if (> (get-combobox-item-count combobox) 0)
        (clear-combobox-hack combobox))
    )
  
  (define (test-combobox cb)
    (define cb-len (get-combobox-item-count cb))
    (define (helper cb index)
      (display index)(display " ")(display (get-combobox-item-at cb index))(newline)
      (if (not (= (- cb-len 1) index))
          (helper cb (+ index 1)))
      )
    (helper cb 0)
    )
  
;  (map (lambda (action-type)
;         (display "action type ")(display action-type)(newline)
;        )
;       action-type-list)
  
  ;(test-combobox action-type-choice)
  ;(clear-combobox-hack action-type-choice)
  
  (display "aft set combobox clear ")(newline)
  (map (lambda (action-type)
         (add-combobox-string action-type-choice action-type))
       action-type-list)
  )

;; returns a list of the action panels in action-list-panel
(define (action-panel-list)
  (get-container-children action-list-panel))

;; add an alt text action to the action list
(define (add-specific-action action-type #!rest args-lst)
  ;; cache the selection to set back at the end
  (define selected-action-type (get-combobox-selecteditem action-type-choice))
  (display "add-specific-action ")(newline)
  (display "action tyep choice ")(display action-type-choice)(newline)
  
  ;; choose our desired type and 
  (set-combobox-selection-object action-type-choice action-type)
  (define panel-to-return (add-action-callback #f)) 
  
  (display "selection? ")(display (get-combobox-selecteditem action-type-choice))(newline)
  
  ;; alter the configuration of the ui objects if args-lst given
  (if (and args-lst (not (null? args-lst)))
      (cond ((equal? action-type "update text using")
             (if (= (length args-lst) 2)
                 (let ((using-type (car args-lst))
                       (alt-text (cadr args-lst)))

                   (set-combobox-selection-object action-type-combobox using-type)

                   (cond ((equal? using-type "alternative text")
                          (set-text alt-text-textfield alt-text)
                          )
                         ((equal? using-type "string fact")
                                        ;fact-string-choice-combobox
                          (define target-fact (get 'facts alt-text))
                          (define fact-name (ask target-fact 'name))

                          (define for-selection (create-combobox-string-item fact-name))
                          (set-combobox-selection-object fact-string-choice-combobox for-selection)
                          )
                         )
                   ))
             )
            ((equal? action-type "follow link to")
             (display "args lst in follow link to ")(display args-lst)(newline)
             (if (= (length args-lst) 1)
                 (let* ((link-dest1 (car args-lst))
                        (dest-node-name (ask (get 'nodes link-dest1) 'name)))
                   (display "dest node name  ")(display dest-node-name)(newline)
                   (set-combobox-selection-object node-choice-combobox (create-combobox-string-item dest-node-name))))
             )
            ((equal? action-type "update fact")
             (if (= (length args-lst) 3)
                 (let ((the-action (car args-lst))
                       (targetID (cadr args-lst))
                       (the-value (caddr args-lst)))
                   (add-component panel-to-return (create-fact-panel2 the-action targetID the-value))))
             ))
      )
  
  ;; set back original selection
  (set-combobox-selection-object action-type-choice selected-action-type)
  panel-to-return)

;; called when add action button is pressed
(define (add-action-callback source)
  (display "add action! ")(newline)
                                        ;(add-component actions-main-panel update-text-action-panel 'border-center)
  ;; get the selected type
  (define selected-action-type (get-combobox-selecteditem action-type-choice))
  (display "selected ")(display selected-action-type)(newline)
  (define new-action-panel (create-action-panel selected-action-type))
  (display "adding panel for add action panel ")(newline)
  (add-component action-list-panel new-action-panel)
  
  ;; remove unavailable (taken/existent) choices
  ;(update-action-type-choice2 new-action-panel)
  
  (display "new action panel ")(display new-action-panel)(newline)
  
  (pack-frame editlink-dialog)
  new-action-panel)

;; called when delete action button is pressed
(define (delete-action-callback source)
  (map (lambda (action-panel)
         (define comp-lst (get-container-children action-panel))
         (define action-checkbox (car comp-lst))
         (define action-label (cadr comp-lst))
         (if (get-checkbox-value action-checkbox)
             (begin
               (define action-type (get-text action-label))
               ;; add the choice back if we're deleting a unique action (only a copy of the action should exist)
               (if (member (get-text action-label) unique-choices)
                   (add-combobox-string action-type-choice action-type))
               
               ;; remove that panel from action-list-panel
               (remove-component action-list-panel action-panel)
               ))
         ) (action-panel-list)))

(define (create-actions-main-panel)
  
  (set! actions-main-panel (make-panel))
  (set-container-layout actions-main-panel 'border)
  
  (define add-action-button (make-button "Add Action"))
  (define delete-action-button (make-button "Delete Selected"))
  
  ;; label
  (define action-label (make-label-with-title "THEN perform the following actions:"))
  (define action-label-panel (make-panel))
  (set-container-layout action-label-panel 'vertical)
  (add-component action-label-panel action-label)
  (add-component actions-main-panel action-label-panel 'border-north)
  
  ;; list of actions
  (set! action-list-panel (make-panel))
  (set-container-layout action-list-panel 'vertical)
  (add-component actions-main-panel action-list-panel 'border-center)
  
  ;; action type list contains the remaining available types left (after previous adding of actions)
  ;(set! action-type-choice (apply make-combobox action-type-list))
  (init-action-type-choice)
  
  (define action-type-choice-container (make-panel))
  (add-component action-type-choice-container action-type-choice)

  ;; button panel
  (define actions-buttons-panel (make-panel))
  (set-container-layout actions-buttons-panel 'flow 'center)
  (add-component actions-main-panel actions-buttons-panel 'border-south)
  
  ;; Add action choice
  (add-component actions-buttons-panel action-type-choice-container)
  (add-component actions-buttons-panel add-action-button)
  (add-component actions-buttons-panel delete-action-button)
  
  ;; action listeners
  (add-actionlistener add-action-button 
                      (make-actionlistener add-action-callback))
  
  (add-actionlistener delete-action-button 
                      (make-actionlistener delete-action-callback))
  )

;; kept to ensure some actions are only added once
(define action-type-list (list "update text using" "follow link to" "update fact"))
(define-constant unique-choices (list "update text using" "follow link to")) ;; choices that shouldnt be duplicated

(define node-choice-combobox #f)

;; an instance of the action type selector panel
(define (create-action-panel action-type) ; the-type )
  (if (not (eq? action-type #!null))
      (let* ((top-panel (make-panel))
             (the-checkbox (make-checkbox "")))
                                        ; add top-panel
        (set-container-layout top-panel 'flow 'left)

                                        ;(add-component editlink-panel-conditions top-panel)

        ;; Add checkbox
        (add-component top-panel the-checkbox)

                                        ;    (cond ((equal? selected-item "update text using")
                                        ;         (display "equal update text ")(newline)
                                        ;         (clear-container-from-index-onwards action-panel 2)
                                        ;         ;(update-action-type-choice "update text using")
                                        ;         (update-action-type-choice2)
                                        ;         (add-component action-panel update-text-action-panel)
                                        ;         )
                                        ;        ((equal? selected-item "follow link to")
                                        ;         (display "eq text from fact ")(newline)
                                        ;         (clear-container-from-index-onwards action-panel 2)
                                        ;         ;(update-action-type-choice "follow link to")
                                        ;         (update-action-type-choice2)
                                        ;         (display "add follow link t ")(newline)
                                        ;         (add-component action-panel (create-node-choice #f (lambda (e) #f) #f #t #f)) ;; edited-nodeID last arg
                                        ;         (display "aft follow link t ")(newline)
                                        ;         )
                                        ;        ((equal? selected-item "update fact") ;; update fact can have many copy
                                        ;         (display "eq text from fact ")(newline)
                                        ;         (clear-container-from-index-onwards action-panel 2)
                                        ;         (update-action-type-choice2)
                                        ;         (add-component action-panel (create-fact-panel2 #f #f #f))
                                        ;         )
                                        ;        ((equal? selected-item "<Choose action>")
                                        ;         (clear-container-from-index-onwards action-panel 2)
                                        ;         (update-action-type-choice2)
                                        ;         ))

        (display "action type ")(display action-type)(newline)

        ;; add the action label display
        (define action-label (make-label-with-title (to-string action-type)))
        (add-component top-panel action-label)

        (define action-type-str (to-string action-type))
                                        ;"update text using" "follow link to" "update fact"))
        
        (cond ((equal? action-type-str "update text using")
               (add-component top-panel update-text-action-panel)
               )
              ((equal? action-type-str "follow link to")
               (set! node-choice-combobox (create-node-choice #f (lambda (e) #f) #f #t #f))
               (add-component top-panel node-choice-combobox)
               )
              ((equal? action-type-str "update fact")
               (add-component top-panel (create-fact-panel2 #f #f #f))
               )
              (else
               (display "create-action-panel error ")(display action-type)(newline)
               (display "action type class type ")(display (invoke action-type 'get-class))(newline)
               )
              )

        (display "check to remove TYPES ")(newline)
        (display "action type ")(display action-type)(newline)
        (display "action type class type ")(display (invoke action-type 'get-class))(newline)
        (display "member check ")(display (member (to-string action-type) unique-choices))(newline)
                                        ;    (display (to-string action-type))(newline)
                                        ;    (display (invoke (to-string action-type) 'get-class))(newline)
                                        ;    (display unique-choices)(newline)
                                        ;    (display (invoke (car unique-choices) 'get-class))(newline)
                                        ;    (display (member (to-string action-type) unique-choices))(newline)

        ;; update action-type-choice combobox
        ;; remove from available
        (if (member (to-string action-type) unique-choices)
            (begin
              (display "removing cos it is inside uc")(newline)
              (remove-combobox-item action-type-choice action-type)
              ))

        top-panel)
      (make-panel)))

;;
;; Update text panels
;;

(define update-text-action-panel #f)
(define alt-text-textfield #f)
(define action-type-combobox #f)
(define fact-string-choice-combobox #f)
(define fact-boolean-choice-combobox #f)
(define update-text-fact-selection-panel #f)

(define (action-update-text-combobox-callback)
  (define (clear-all-except-first)
    (display "clear all ")(newline)
    (remove-component update-text-action-panel alt-text-textfield)
    (remove-component update-text-action-panel update-text-fact-selection-panel)
    (display "clear all done ")(newline))
  
  (define link-obj (get 'links edited-linkID))
;         (link-name (ask link-obj 'name))
;         (link-dest1 (ask link-obj 'destination))
;         (link-uselink (ask link-obj 'use-destination))
;         (link-usealtlink (ask link-obj 'use-alt-destination))
;         (link-usealttext (ask link-obj 'use-alt-text))
;         (link-dest2 (ask link-obj 'alt-destination))
  (define link-alttext (ask link-obj 'alt-text))
;         (link-start-index (ask link-obj 'start-index))
;         (link-end-index (ask link-obj 'end-index))
;         (edited-node (get 'nodes edited-nodeID))
;         (anywhere (ask edited-node 'anywhere?)))
  
    (display "combobox type selected ")
    (define selected-item (get-combobox-selecteditem action-type-combobox))
    (display selected-item)(newline)

    (cond ((equal? selected-item "alternative text")
           (display "equal alt text ")(newline)
           (clear-all-except-first)
           (add-component update-text-action-panel alt-text-textfield)
           (display "[link-alttext] ")(display link-alttext) 
           (set-text alt-text-textfield link-alttext)
           )
          ((equal? selected-item "string fact")
           (display "eq text from fact ")(newline)
           (clear-all-except-first)
           ;(add-component update-text-action-panel fact-choice-combobox)
           
;           (set! fact-string-choice-combobox ;editlink-panel-else-text-factchoice 
;                 (create-fact-choice 'string update-text-action-panel
;                                     selected-fact-in-link -1))
           
           ;; create and add the combobox containing the string facts
           (set! fact-string-choice-combobox (create-fact-choice 'string #f selected-fact-in-action #f))
           ;(define string-facts-combobox (create-fact-choice 'string #f selected-fact-in-action #f))
           
           ;; add to a container panel and add to the update-text-action-panel
           (add-component update-text-fact-selection-panel fact-string-choice-combobox)
           (add-component update-text-action-panel update-text-fact-selection-panel)
           
           ;(add-component update-text-action-panel fact-string-choice-combobox)
           ))
    ;(set-component-visible editlink-dialog #t)
  (pack-frame editlink-dialog)
    )

;; update update-text-fact-selection-panel
(define (populate-facts)
  
  ;; empty update-text-fact-selection-panel
  (clear-container update-text-fact-selection-panel)
  (display "finished clearing populate facts ")(newline)
  
;  (define link-obj (get 'links edited-linkID))
;  ;(display "link obj ")(display link-obj)(newline)
;  (define selected-ruleID (ask link-obj 'rule))
;  (define rule-obj (get 'rules selected-ruleID))
;  (define facts (ask rule-obj 'actions))
  
  (define the-facts (get-list 'facts))
  (display "facts before populate ")(display the-facts)(newline)
  
   ;; go through the facts list and add a fact panel for every fact
;  (if the-facts
;      (map (lambda (myaction)
;             (let* ((action-obj (get 'actions myaction))
;                    (the-action-string (ask action-obj 'expr))
;                    (the-action-expr (read (open-input-string the-action-string))) ; hack, should change to saving as sexpr
;                    (the-action (car the-action-expr))
;                    (targetID (cadr the-action-expr))
;                    (the-value (cond ((eq? the-action 'set-value!)
;                                      (caddr the-action-expr))
;                                     ((eq? the-action 'assert) #t)
;                                     ((eq? the-action 'retract) #f)
;                                     ))
;                    (fact-type (cond ((or (eq? the-action 'assert)
;                                          (eq? the-action 'retract))
;                                      "boolean")
;                                     ((eq? the-action 'set-value!)
;                                      "string")))

;                    )
;               (format #t "fact: ~a (~a, ~a, ~a)~%~!" the-action-expr the-action the-value targetID)
;               (define new-fact-panel (create-fact-panel2 fact-type targetID the-value))
;               (display "new fact panel ")(display new-fact-panel)(newline)

;               (add-component update-text-fact-selection-panel new-fact-panel)))
;           the-facts))
;  (if the-facts 
;      (map (lambda (fact)
;             (display "fact ")(display fact)(newline)
;             (let* ((fact-obj (cdr fact))
;                    (factID (car fact))
;                    (the-value (ask fact-obj 'get-value))
;                    (fact-type (ask fact-obj 'type))
;                    )
;               (display "FACT TYPE ")(display fact-type)(newline)
;               (display "fact type class ")(display (invoke fact-type 'get-class))(newline)
;               (define new-fact-panel (create-fact-panel2 fact-type factID the-value))
;               (add-component update-text-fact-selection-panel new-fact-panel)
;               )
;             ) the-facts))
  ;(add-component update-text-fact-selection-panel (create-fact-panel2 'boolean #f #f))
;  (add-component update-text-fact-selection-panel (create-fact-choice 'string #f selected-fact-in-action #f))
  
  (display "end of populate facts ")(newline)
      )

;; the panel that comes behind the combobox selecting actions type
(define (create-update-text-action-panel)
  (set! update-text-action-panel (make-panel))
  (set! action-type-combobox (make-combobox "alternative text" "string fact"))
  (set! alt-text-textfield (make-textfield "" 20))
  
  (set! update-text-fact-selection-panel (make-panel))
  
  (set! fact-string-choice-combobox ;editlink-panel-else-text-factchoice 
    (create-fact-choice 'string update-text-action-panel
                        selected-fact-in-link -1))
  
  (set! fact-boolean-choice-combobox ;editlink-panel-else-text-factchoice 
    (create-fact-choice 'boolean update-text-action-panel
                        selected-fact-in-link -1))
  
  (add-component update-text-action-panel action-type-combobox)
  (add-component update-text-action-panel alt-text-textfield)
  
  ;(set! update-text-combobox-callback (lambda (
  
  (add-actionlistener action-type-combobox 
                      (make-actionlistener 
                       (lambda (e)
                         (action-update-text-combobox-callback))))
  )

;; 
;; Update Fact panels
;;
;(define update-fact-action-panel #f)

;; create an instance of update fact action panel
(define (create-update-fact-action-panel)
  ;(set! update-fact-action-panel (make-panel))
  (define top-panel (make-panel))
  (add-component top-panel (create-fact-panel2 'boolean #f #f))
  
  top-panel)

(define (create-editlink-dialog parent)
  ; remember parent
  (set! editlink-dialog-parent parent)
  
  ; create edit link dialog
  (set! editlink-dialog (make-dialog-noparent-modal "Edit link"))
  (set-component-size editlink-dialog 400 100)
  (set-dialog-resizable editlink-dialog (show-actions?))
  (set! editlink-dialog-pane (get-dialog-content-pane editlink-dialog))
  (set-container-layout editlink-dialog-pane 'border)
  
  ;; tabbed panel for link and actions
  (set! editlink-dialog-tabpanel (make-tab-panel))
  (if (show-actions?)
      (add-component editlink-dialog-pane editlink-dialog-tabpanel 'border-center))
  
  ;; link
  (set! editlink-dialog-linkpanel (make-panel))
  (add-tabpanel-tab editlink-dialog-tabpanel "Link" editlink-dialog-linkpanel)
  (set-container-layout editlink-dialog-linkpanel 'border)
  
  ;;
  ;; actions

  ; THEN action
  (set! editlink-dialog-then-actiontext (make-textpane)) ;(make-editor-pane 300 100))
  (set-component-preferred-size editlink-dialog-then-actiontext 300 100)
  (add-tabpanel-tab editlink-dialog-tabpanel "Then action" (make-scrollpane editlink-dialog-then-actiontext))
  
  ; ELSE action
  (set! editlink-dialog-else-actiontext (make-textpane)) ;(make-editor-pane 300 100))
  (set-component-preferred-size editlink-dialog-else-actiontext 300 100)
  (add-tabpanel-tab editlink-dialog-tabpanel "Else action" (make-scrollpane editlink-dialog-else-actiontext))
  
  ;; Add a horizontal panel for conditions and condition buttons
  (set! editlink-panel-top (make-panel))
  (set-container-layout editlink-panel-top 'vertical)
  (if (show-actions?)
      (add-component editlink-dialog-linkpanel editlink-panel-top 'border-center)
      (add-component editlink-dialog-pane editlink-panel-top 'border-center))

  ;;
  ;; ELSE
  ;;

  ;; start of ELSE action

  ; panel to hold all the "else" information
  (set! editlink-panel-else (make-panel))
  (set-container-layout editlink-panel-else 'vertical)
  
  ;; should not have else panel in the new version
  ;; shifted this code to where we do set-component-visible for dialog (which is doeditnode doeditlink)
;  (if (not (is-basic-mode?))
;      (begin
;        (add-component editlink-panel-top editlink-panel-else)))

  ; the "ELSE" label
  (set! editlink-panel-else-message1-panel (make-panel))
  (set-container-layout editlink-panel-else-message1-panel 'flow 'left)
  (add-component editlink-panel-else editlink-panel-else-message1-panel)
  (set! editlink-panel-else-message1 (make-label))
  (set-text editlink-panel-else-message1 "ELSE")
  (add-component editlink-panel-else-message1-panel editlink-panel-else-message1)

  ; panel to hold the "else" text
  (set! editlink-panel-else-text (make-panel))
  (set-container-layout editlink-panel-else-text 'horizontal)
  ;(add-component editlink-panel-else editlink-panel-else-text) ;; used to be in else 
  ;(add-component editlink-panel-then editlink-panel-else-text)

  ;; Add checkbox
  ; need to be able to disable fact option with show-facts
  (set! editlink-panel-else-text-check (make-checkbox 
                                        (if (show-facts?)
                                            "show"
                                            "show alternative text")))
  (add-itemlistener editlink-panel-else-text-check
                    (make-itemlistener (lambda (source newstate)
                                         (text-check-changed source newstate))))
  (add-component editlink-panel-else-text editlink-panel-else-text-check)
  
  ; add text choice
  (set! editlink-panel-else-text-typechoice (make-combobox "alternative text" "text from Fact"))
  (if (show-facts?)
      (add-component editlink-panel-else-text editlink-panel-else-text-typechoice))

  ; set callback for type choice
  (add-actionlistener editlink-panel-else-text-typechoice
                      (make-actionlistener (lambda (source)
                                             (selected-type-in-link source))))

  ; alternative link text
  (set! editlink-panel-else-text-message1 (make-textfield "[alternative text...........]" 20))
  (add-component editlink-panel-else-text editlink-panel-else-text-message1)
  
  ; panel to hold the "else" link
  (set! editlink-panel-else-link (make-panel))
  (set-container-layout editlink-panel-else-link 'horizontal)
  (add-component editlink-panel-else editlink-panel-else-link)

  ;; Add checkbox
  (set! editlink-panel-else-link-check (make-checkbox "follow link to"))
  (add-itemlistener editlink-panel-else-link-check
                    (make-itemlistener (lambda (source newstate)
                                         (link-check-changed source newstate))))
  (add-component editlink-panel-else-link editlink-panel-else-link-check)

  ; panel to hold the alternative destination choice
  (set! editlink-panel-else-choicepanel (make-panel))
  (set-container-layout editlink-panel-else-choicepanel 'horizontal)
  (add-component editlink-panel-else-link editlink-panel-else-choicepanel)

  ; choice for alternative destination - created dynamically when rule editor is loaded
  (set! editlink-panel-else-link-choice #f)

  ;; end of ELSE action

  ;; start of new NEW THEN action
  
  (define editlink-panel-action-buttons (make-panel))
;  (set-container-layout editlink-panel-then-buttons 'horizontal)
;  (add-component editlink-panel-then editlink-panel-action-buttons)
  
  (define editlink-panel-action-buttons-add (make-button "Add Action"))
;  (add-actionlistener editlink-panel-then-buttons-add
;                      (make-actionlistener (lambda (source)
;                                             (editlink-dialog-add-fact))))
;  (add-component editlink-panel-action-buttons editlink-panel-action-buttons-add)

  ;; Add DELETE button
  (set! editlink-panel-action-buttons-delete (make-button "Delete selected"))
;  (add-actionlistener editlink-panel-then-buttons-delete
;                      (make-actionlistener (lambda (source)
;                                             (editlink-dialog-delete-facts))))
;  (add-component editlink-panel-action-buttons editlink-panel-action-buttons-delete)
  
  
  ;; end of NEW THEN action
  
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
                                             (editlink-dialog-cancel))))
  (set! editlink-panel-buttons-ok (make-button "   OK   "))
  (add-actionlistener editlink-panel-buttons-ok
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-ok))))
  (if (is-windows?)
      (begin
        (add-component editlink-panel-buttons editlink-panel-buttons-ok)
        (add-component editlink-panel-buttons editlink-panel-buttons-cancel))
      (begin
        (add-component editlink-panel-buttons editlink-panel-buttons-cancel)
        (add-component editlink-panel-buttons editlink-panel-buttons-ok)))
  
;  (create-dialog-ok-cancel-buttons 
;   editlink-dialog-pane 
;                    (lambda (source)
;                      (editlink-dialog-ok))
;                    (lambda (source)
;                      (editlink-dialog-cancel)))
      
  ; pack
  (pack-frame editlink-dialog))

(define editnode-dialog #f)
(define editnode-panel-then-facts #f)
(define editnode-panel-top #f)
(define editnode-panel-then #f)
(define editnode-dialog-parent #f)
(define editnode-dialog-pane #f)
(define editnode-panel-then-facts-labelpanel #f)
(define editnode-panel-then-buttons #f)

(define (create-facts-main-panel)
  ; panel for facts label
  (set! editnode-panel-then-facts-labelpanel (make-panel))
  (set-container-layout editnode-panel-then-facts-labelpanel 'flow 'left)
  ;(add-component editnode-panel-then editnode-panel-then-facts-labelpanel)

  ;; facts label
  (define editnode-panel-then-facts-label (make-label))
  (set-text editnode-panel-then-facts-label "when visited, update the following facts:")
  (add-component editnode-panel-then-facts-labelpanel editnode-panel-then-facts-label)
  
  ;; facts panel
  (set! editnode-panel-then-facts (make-panel))
  
  (set-container-layout editnode-panel-then-facts 'vertical)
  
  ;(add-component editnode-panel-then editnode-panel-then-facts)

  ;; Add a horizontal panel to the dialog, with centering for buttons
  (set! editnode-panel-then-buttons (make-panel))
  (set-container-layout editnode-panel-then-buttons 'horizontal)
  
;  (add-component editnode-panel-then editnode-panel-then-buttons)

  ;; Add ADD button
  (define editnode-panel-then-buttons-add (make-button "Add fact"))
  (add-actionlistener editnode-panel-then-buttons-add
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-add-fact))))
  (add-component editnode-panel-then-buttons editnode-panel-then-buttons-add)

  ;; Add DELETE button
  (define editnode-panel-then-buttons-delete (make-button "Delete selected"))
  (add-actionlistener editnode-panel-then-buttons-delete
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-delete-facts))))
  (add-component editnode-panel-then-buttons editnode-panel-then-buttons-delete)
  )

(define (create-editnode-dialog parent)
  ; remember parent
  (set! editnode-dialog-parent parent)
  
  ; create edit link dialog
  (set! editnode-dialog (make-dialog-noparent-modal "Edit link"))
  (set-component-size editnode-dialog 400 100)
  (set-dialog-resizable editnode-dialog #f) ;(show-actions?)) 
  (set! editnode-dialog-pane (get-dialog-content-pane editnode-dialog))
  (set-container-layout editnode-dialog-pane 'border)
  
  ;; tabbed panel for link and actions
;  (define editnode-dialog-tabpanel (make-tab-panel))
;  (if (show-actions?)
;      (add-component editnode-dialog-pane editnode-dialog-tabpanel 'border-center))
  
    ; facts - eventually change this to actions? - alex
  
    ;; start of THEN action

  (set! editnode-panel-top (make-panel))
  (set-container-layout editnode-panel-top 'vertical)
;  (if (show-actions?)
;      (add-component editlink-dialog-linkpanel editlink-panel-top 'border-center)
;      (add-component editlink-dialog-pane editlink-panel-top 'border-center))
  (add-component editnode-dialog-pane editnode-panel-top 'border-center)
  
  ; panel to hold all the "then" information
  (set! editnode-panel-then (make-panel))
  (set-container-layout editnode-panel-then 'vertical)
  
  ;(add-component editnode-panel-top editnode-panel-then)
  
  
;  (create-dialog-ok-cancel-buttons editnode-dialog-pane
;                                   (lambda (source)
;                                     (editnode-dialog-ok))
;                                   (lambda (source)
;                                     (editnode-dialog-cancel)))

  (pack-frame editnode-dialog)
  )

(define (create-ok-cancel-buttons-panel parent-dialog-pane ok-action cancel-action)
  ;; OK cancel buttons
  (define editnode-panel-buttons (make-panel))
  (set-container-layout editnode-panel-buttons 'flow 'right)
  ;(add-component parent-dialog-pane editnode-panel-buttons 'border-south)
  
  ;; Add Cancel and Ok buttons to the horizontal panel
  (define editnode-panel-buttons-cancel (make-button " Cancel "))
  (add-actionlistener editnode-panel-buttons-cancel
                      (make-actionlistener cancel-action))
  (define editnode-panel-buttons-ok (make-button "   OK   "))
  (add-actionlistener editnode-panel-buttons-ok
                      (make-actionlistener ok-action))
  (if (is-windows?)
      (begin
        (add-component editnode-panel-buttons editnode-panel-buttons-ok)
        (add-component editnode-panel-buttons editnode-panel-buttons-cancel))
      (begin
        (add-component editnode-panel-buttons editnode-panel-buttons-cancel)
        (add-component editnode-panel-buttons editnode-panel-buttons-ok)))
  
  editnode-panel-button
  )

;;
;; conditions
;;

; add a node choice
; callback will be called when choice changes, and nodeID will be selected
; if excludeAnywhere, will exclude anywhere nodes
; will also exclude edited-nodeID
(define (create-node-choice parent-panel in-callback nodeID excludeAnywhere in-edited-nodeID)
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
    
    ; add action listener
    (add-actionlistener node-list
                        (make-actionlistener (lambda (source)
                                               (in-callback source))))
    
    ; select the chosen node
    (if nodeID
        (set-comboboxwithdata-selection-bydata node-list nodeID)
        (set-combobox-selection node-list 0))

    ; return the list
    node-list))

; add a link choice
; callback will be called when choice changes, and linkID will be selected
(define (create-link-choice parent-panel in-callback linkID)
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
    
    ; add action listener
    (add-actionlistener link-list
                        (make-actionlistener (lambda (source)
                                               (in-callback source))))
    
    ; select the chosen link
    (set-comboboxwithdata-selection-bydata link-list linkID)

    ; return the list
    link-list))

; add a fact choice - same as above, refactor! - alex
; creates a choice containing facts of the given type
; callback will be called when choice changes, and factID will be selected
;; if factID is #f select <None>
;; TODO: remove parent-panel from all those create-fact-choice, create-link-choice, create-node-choice since we're not using it anymore
(define (create-fact-choice the-type parent-panel in-callback factID)
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
    
    
    ; add action listener
    (add-actionlistener fact-list
                        (make-actionlistener (lambda (source)
                                               (in-callback source the-type))))

    ; select the chosen fact
    (if factID
        (set-comboboxwithdata-selection-bydata fact-list factID)
        (set-combobox-selection fact-list 0)) ;; no fact selected
    
    ; return the list
    fact-list))

; create a condition panel
; the-type: the type of condition (0=node, 1=link, 2=fact)
; targetID: the currently selected node/link, if any (pass in -1 if none selected)
; selectedOperator: the operator for this condition (visited, not visited, or previous)
(define (create-condition-panel the-type targetID selectedOperator in-edited-nodeID)
  (let* ((top-panel (make-panel))
         (the-checkbox (make-checkbox ""))
         (the-type-choice (if (is-basic-mode?)
                              (make-combobox "Node")
                              (if (show-facts?)
                                  (make-combobox "Node" "Link" "Fact")
                                  (make-combobox "Node" "Link"))))
         (the-node-list (create-node-choice top-panel selected-node-in-condition targetID #f in-edited-nodeID))
         (the-link-list (create-link-choice top-panel selected-link-in-condition targetID))
         (the-fact-list (create-fact-choice 'boolean top-panel selected-fact-in-condition targetID))
         (the-node-operator-choice (if (not (is-basic-mode?))
                                       (make-combobox "Not Visited" "Visited" "Previous Node")
                                       (make-combobox "Not Visited" "Visited")))
         (the-link-operator-choice (make-combobox "Not Followed" "Followed"))
         (the-fact-operator-choice (make-combobox "False" "True")))

    (format #t "Creating condition-panel: targetID:~a, in-edited-nodeID:~a~%~!" targetID in-edited-nodeID)
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    (add-component editlink-panel-conditions top-panel)
    
    ;; Add checkbox
    (add-component top-panel the-checkbox)

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
                                                                                 the-fact-list the-fact-operator-choice)))))
        (add-component top-panel (make-label-with-title "Node")))

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

    ; return the panel
    top-panel))

;;
;; callbacks from UI
;;

; set "ok" button state - cannot allow a "use" checkbox to be checked and a link to be "none"
(define (update-ok-button-state)
  (if (and editlink-panel-buttons-ok
           editlink-panel-then-link-check
           editlink-panel-then-link-choice
           editlink-panel-else-link-check
           editlink-panel-else-link-choice
           editlink-panel-else-text-check)
      (set-button editlink-panel-buttons-ok
                  (not
                   (or
                    ; else text checked, fact type selected, but "none" selected for fact
                    (and (get-checkbox-value editlink-panel-else-text-check)
                         editlink-panel-else-text-typechoice ; make sure these exist
                         editlink-panel-else-text-factchoice
                         (eq? (get-combobox-selectedindex editlink-panel-else-text-typechoice) 1)
                         (eq? (get-comboboxwithdata-selecteddata editlink-panel-else-text-factchoice) -1))
                         
                    ; then link checked but "none" selected
                    (and (get-checkbox-value editlink-panel-then-link-check)
                         (eq? (get-comboboxwithdata-selecteddata editlink-panel-then-link-choice) -1))
                    
                    ; else link checked but "none" selected
                    (and (get-checkbox-value editlink-panel-else-link-check)
                         (eq? (get-comboboxwithdata-selecteddata editlink-panel-else-link-choice) -1)))))))

; link checkbox was changed, so update OK button state
(define (link-check-changed c e)
  (update-ok-button-state))

; text checkbox was changed, so update OK button state
(define (text-check-changed c e)
  (update-ok-button-state))

; selection of condition type changed, so change the node/link list and operator list
(define (selected-type-in-condition c top-panel
                                    the-node-list the-node-operator-choice
                                    the-link-list the-link-operator-choice
                                    the-fact-list the-fact-operator-choice)
  (format #t "selected-type-in-condition~%~!")
  
  ; remove the current target and operator lists
  (let* ((children (get-container-children top-panel))
         (old-target (caddr children))
         (old-operator (cadddr children)))
    (remove-component top-panel old-target)
    (remove-component top-panel old-operator))
         
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
    (set-combobox-selection new-target 0)
    (add-component top-panel new-operator)
    (set-combobox-selection new-operator 0))
  
  (pack-frame editlink-dialog))

; selection of condition node changed: do nothing for now
(define (selected-node-in-condition c)
  (format #t "selected-node-in-condition~%~!"))

; selection of condition link changed: do nothing for now
(define (selected-link-in-condition c)
  (format #t "selected-link-in-condition~%~!"))

; selection of condition fact changed: do nothing for now
(define (selected-fact-in-condition c type)
  (format #t "selected-fact-in-condition: ~a~%~!" type))

  ; selection of destination node changed, so update checkboxes if necessary, and update OK button state
(define (selected-node-in-destination c)
;  (format #t "selected-node-in-destination~%~!")
  ; if "none" destination selected, uncheck the checkbox
  (if (and (eq? c editlink-panel-then-link-choice)
           editlink-panel-then-link-check 
           editlink-panel-then-link-choice)
      (set-checkbox-value editlink-panel-then-link-check
                      (not (eq? (get-comboboxwithdata-selecteddata editlink-panel-then-link-choice) -1))))
  (if (and (eq? c editlink-panel-else-link-choice)
           editlink-panel-else-link-check 
           editlink-panel-else-link-choice)
      (set-checkbox-value editlink-panel-else-link-check
                      (not (eq? (get-comboboxwithdata-selecteddata editlink-panel-else-link-choice) -1))))
  (update-ok-button-state))

; cancelled, so hide newnode-dialog
(define (editlink-dialog-cancel)
  ;; if updatelevel is more than 0 it doeditlink was called right after donewlink
  ;; in that case a beginupdate had been called so close the compound undoable edit
  ;; by calling endupdate to close it
  (if (> (compoundundomanager-updatelevel undo-manager) 0)
      (compoundundomanager-endupdate undo-manager undo-action redo-action))
  (set-component-visible editlink-dialog #f)
  (reset-rule-editor)
  (display "[reset-rule-editor] inside editlink-dialog-cancel ")(newline)
  )

(define (editnode-dialog-cancel)
  ;; if updatelevel is more than 0 it doeditlink was called right after donewlink
  ;; in that case a beginupdate had been called so close the compound undoable edit
  ;; by calling endupdate to close it
  (if (> (compoundundomanager-updatelevel undo-manager) 0)
      (compoundundomanager-endupdate undo-manager undo-action redo-action))
  (set-component-visible editnode-dialog #f) ;; just differs in this line with editlink-dialog-cancel
  (reset-rule-editor)
  (display "[reset-rule-editor] inside editlink-dialog-cancel ")(newline)
  )

; add a condition
(define (editlink-dialog-add)
  (create-condition-panel 0 -1 0 -1)
  (pack-frame editlink-dialog))

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

; user clicked "ok" button
;; used by both editlink dialog and editnoderule dialog
(define (editlink-dialog-ok)
  (format #t "editlink-dialog-ok: ~a~%~!" edit-mode)
  (let* ((the-rule-object (cond ((eq? edit-mode 'link) (get 'links edited-linkID))
                                ((eq? edit-mode 'node) (get 'nodes edited-nodeID))
                                ((eq? edit-mode 'doc) #f)))
         (new-rulename (if (eq? edit-mode 'doc)
                           "document"
                           (ask the-rule-object 'name)))

         ; get boolean expression
         (new-ruleexpression-pos (get-combobox-selectedindex editlink-dialog-andor-operator))
         (new-ruleexpression (get-rule-exp new-ruleexpression-pos))
         
         ; get conditions
         (all-conditions (get-container-children editlink-panel-conditions))
         
         (negate-pos (get-combobox-selectedindex editlink-dialog-negate-operator))
         (negate? (case negate-pos
                    ((0) #f)
                    ((1) #t)))
         ; get facts
         ;(all-facts (get-container-children editlink-panel-then-facts))
         
         ; create the rule
         (new-ruleID (create-typed-rule2 new-rulename edit-mode new-ruleexpression negate?
                                       (cond ((eq? edit-mode 'link) edited-linkID)
                                             ((eq? edit-mode 'node) edited-nodeID)
                                             ((eq? edit-mode 'doc) -1))))
         
         ; get the actions, if any
         (then-action-string (get-text editlink-dialog-then-actiontext))
         (else-action-string (get-text editlink-dialog-else-actiontext)))
    
    (display "and-or ")(display new-ruleexpression)(newline)
    (display "negate? ")(display negate?)(newline)
    
    ; run through conditions and add to rule
    (map (lambda (panel) (let* ((children (get-container-children panel))
                                (select-type (cadr children))  ;; link node fact combobox
                                (select-target (caddr children)) ;; list of existent objects of select-type
                                (select-operator (cadddr children)) ;; options specific to select-type (ie if type is link then it is followed/not followed)
                                (the-type (if (not (is-basic-mode?)) (get-combobox-selectedindex select-type) 0))
                                (targetID (get-comboboxwithdata-selecteddata select-target))
                                (operator (get-combobox-selectedindex select-operator)))
                           (create-typed-condition new-rulename the-type targetID operator new-ruleID)))
         all-conditions)
    
    ; run through facts and add to rule - this will eventually generalize to actions
    (format #t "saving facts~%~!")
;    (map (lambda (panel) (let* ((children (get-container-children panel))
;                                (select-type (cadr children))
;                                (select-target (caddr children))
;                                (set-value (cadddr children))
;                                (the-type (get-combobox-selectedindex select-type))
;                                (the-value 
;                                 (if (= the-type 0)
;                                     (get-combobox-selectedindex set-value)
;                                     (get-text set-value)))
;                                (targetID (get-comboboxwithdata-selecteddata select-target))
;                                ; build the expression: 
;                                ; (assert targetID) or
;                                ; (retract targetID) or
;                                ; (set-value! targetID the-value)
;                                (the-action-expr 
;                                 (string-append
;                                  "("
;                                  (if (= the-type 0)
;                                      (if (= the-value 0)
;                                          "assert"
;                                          "retract")
;                                      "set-value!")
;                                  " "
;                                  (number->string targetID)
;                                  (if (= the-type 1)
;                                      (string-append " \"" the-value "\"")
;                                      "")
;                                  ")")))
;                           (format #t "fact: ~a~%~!" the-action-expr)
;                           (create-action new-rulename 'fact the-action-expr new-ruleID)))
;         all-facts)
    (format #t "after saving facts, actions: ~a~%~!" (ask (get 'rules new-ruleID) 'actions))
    
    ; if there's an action, add the action to the rule
    ; need to read the string and break in to s-expressions; should eventually
    ; be able to plug in an improved version of definition-editor here
    ; NOTE: I haven't decided if actions should be stored as strings (currently)
    ; or as s-expressions (commented out) - alex
    ; strings: can store formatting
    ; s-expr: seems more appropriate, but some problems with string values?
    (if (not (equal? then-action-string ""))
        (create-action new-rulename 
                       (cond ((eq? edit-mode 'link) 'then)
                             ((eq? edit-mode 'node) 'before)
                             ((eq? edit-mode 'doc) 'step))
                       then-action-string new-ruleID))
    (if (not (equal? else-action-string ""))
        (create-action new-rulename 
                       (cond ((eq? edit-mode 'link) 'else)
                             ((eq? edit-mode 'node) 'after)
                             ((eq? edit-mode 'doc) 'init))
                       else-action-string new-ruleID))
;;        (let ((explist (open-input-string then-action-string)))
;;          (read-action-expr 'then explist new-rulename new-ruleID)))
;;        (let ((explist (open-input-string else-action-string)))
;;          (read-action-expr 'else explist new-rulename new-ruleID)))

    ; if we're editing a link, then retrieve the link details
    (cond ((eq? edit-mode 'link)
                                        ; update use-alt, alt-destination and alt-text
           (define the-link the-rule-object)
           (define uselink (get-checkbox-value editlink-panel-then-link-check))
           (define destID (get-comboboxwithdata-selecteddata editlink-panel-then-link-choice))
           (define olduselink (ask the-link 'use-destination))
           (define olddestID (ask the-link 'destination))
           (define usealtlink (get-checkbox-value editlink-panel-else-link-check))
           (define usealttext (get-checkbox-value editlink-panel-else-text-check))
           (define altdestID (get-comboboxwithdata-selecteddata editlink-panel-else-link-choice))
           (define oldusealtlink (ask the-link 'use-alt-destination))
           (define oldaltdestID (ask the-link 'alt-destination))
           (define alttext (get-text editlink-panel-else-text-message1))
           (define alttext-fact (if (show-facts?)
                                    (eq? (get-combobox-selectedindex editlink-panel-else-text-typechoice) 1)
                                    #f))
           
           ;; an action panel always has a checkbox as first component then a label with the name
           ;; which identifies which kind of action it represents
           (define (get-action-panel-type action-panel)
             (define children (get-container-children action-panel))
             (define action-label (cadr children))
             (get-text action-label))
           
           ;; for processing "update text using" action-panel
           (define (get-update-text-using-type uts-action-panel)
             (define children (get-container-children uts-action-panel))
             
             )
           
           ;; map through the list of action panel
           (map (lambda (action-panel)
                  (define action-type (get-action-panel-type action-panel))
                  (cond ((equal? action-type "update text using")
                         (display "I see a UPDATE TEXT ")(newline)
                         ;; text of fact?
                         (define text-or-fact (get-combobox-selecteditem action-type-combobox))
                         
                         (define text-type #f)
                         (define text-value #f)
                         (case (to-string text-or-fact)
                           (("alternative text") 
                            (set! text-type "text")
                            (set! text-value (get-text alt-text-textfield)))
                           (("string fact") 
                            (set! text-type "fact")
                            ;; TOFIX: get fact string during runtime instead of from the start
                            ;; (might not need to) just need to store factID
                            ;; problem is fact is not accessible to our interpreter since it is in a different environment
                            (define factID (get-comboboxwithdata-selecteddata fact-string-choice-combobox))
                            (display "factID ")(display factID)(newline)
                            ;(define fact-text (ask (get 'facts factID) 'get-value))
                            (set! text-value (to-string factID))
                            )
                           )
                         
                         (display "SETTING ACTION ")(newline)
                         (display "text type ")(display text-type)(newline)
                         (display "text value ")(display text-value)(newline)
                         
                         ;(replace-link-text text-type value linkID)
                         (ask (get 'rules new-ruleID) 'add-action! 
                              (string-append "(replace-link-text " 
                                             text-type " "
                                             text-value " "
                                             (to-string edited-linkID) ")"))
                         
                         )
                        ((equal? action-type "follow link to")
                         ;(display "I see FOLLOW LINK ")(newline)
                         ;node-choice-combobox
                         (define dest-nodeID (get-comboboxwithdata-selecteddata node-choice-combobox))
                         ;(follow-link2 linkID parent-ruleID link-type dest-nodeID)
                         (ask (get 'rules new-ruleID) 'add-action! 
                              (string-append "(follow-link "
                                              (to-string edited-linkID) " "
                                              (to-string new-ruleID) " "
                                              "(quote default)" 
                                              (to-string dest-nodeID) ")"))
                         )
                        ((equal? action-type "update fact")
                         (display "I see UPDATE FACT ")(newline)
                         )
                        )
                  ) (get-container-children action-list-panel))

           (ask the-link 'set-use-destination! uselink)
           (ask the-link 'set-destination! destID)
           (ask the-link 'set-use-alt-destination! usealtlink)
           (ask the-link 'set-alt-destination! altdestID)
           ;; if using alt text, and alttext-fact is true, then set use-alt-text to 'fact,
           ;; otherwise just set to usealttext
           (ask the-link 'set-use-alt-text!
                (if (and usealttext
                         alttext-fact)
                    'fact
                    usealttext))
           (format #t "*** use-alt-text: ~a ***~%~!" (ask the-link 'use-alt-text))

           ;; if alttext-fact is true, store the factID in alt text otherwise store text
           (if alttext-fact
               (ask the-link 'set-alt-text! (get-comboboxwithdata-selecteddata editlink-panel-else-text-factchoice))
               (ask the-link 'set-alt-text! alttext))

           ;; update links in graph if callback provided
           ;; note: if uselink/usealtlink is false, then pass -1 ie. don't draw line
           ;; similar for old links
           (if update-callback
               (update-callback new-rulename edited-nodeID
                                olduselink olddestID
                                oldusealtlink oldaltdestID
                                uselink destID
                                usealtlink altdestID
                                edited-linkID))

           ;; cache the information of the edited link in the form of a lambda object
           (after-editlink edited-linkID)
           
           ;; added an undoable event 
           (post-editlink-undoable-event)
                                        ;(display "[editlink-dialog-ok] ")(display (compoundundomanager-updatelevel undo-manager))(newline)

           ;; if updatelevel is more than 0 it doeditlink was called right after donewlink
           ;; in that case a beginupdate had been called so close the compound undoable edit
           ;; by calling endupdate to close it
           (if (> (compoundundomanager-updatelevel undo-manager) 0)
               (compoundundomanager-endupdate undo-manager undo-action redo-action))
                                        ;(display "[editlink-dialog-ok] ")(display (compoundundomanager-updatelevel undo-manager))(newline)
           )
          ((eq? edit-mode 'node)
           (define the-node the-rule-object)
           (define nodeID (ask the-node 'ID))
           (after-editnode nodeID)
           (post-edit-noderule-undoable-event)
           )
          )

    ; hide link editor, and reset (for next time)
    (set-component-visible editlink-dialog #f)
    (reset-rule-editor)
    ))

;; TODO merge editnode-dialog-ok and editlink-dialog-ok's code because they do things 
;; pretty similar
(define (editnode-dialog-ok)
  (format #t "editnode-dialog-ok: ~a~%~!" edit-mode)
  (let* ((the-rule-object (cond ((eq? edit-mode 'link) (get 'links edited-linkID))
                                ((eq? edit-mode 'node) (get 'nodes edited-nodeID))
                                ((eq? edit-mode 'doc) #f)))
         (new-rulename (if (eq? edit-mode 'doc)
                           "document"
                           (ask the-rule-object 'name)))

;         ; get boolean expression
         (new-ruleexpression-pos (get-combobox-selectedindex editlink-dialog-andor-operator))
         (new-ruleexpression (get-rule-exp new-ruleexpression-pos))
         
         (negate-pos (get-combobox-selectedindex editlink-dialog-negate-operator))
         (negate? (case negate-pos
                    ((0) #f)
                    ((1) #t)))
         
         ; get conditions
         ;(all-conditions (get-container-children editlink-panel-conditions))
         
         ; get facts
         (all-facts (get-container-children editnode-panel-then-facts))
         
         ; create the rule
         (new-rule (create-typed-rule2 new-rulename edit-mode new-ruleexpression negate?
                                       (cond ((eq? edit-mode 'link) edited-linkID)
                                             ((eq? edit-mode 'node) edited-nodeID)
                                             ((eq? edit-mode 'doc) -1))))
         
         ; get the actions, if any
         (then-action-string (get-text editlink-dialog-then-actiontext))
         (else-action-string (get-text editlink-dialog-else-actiontext)))
    
    ; run through conditions and add to rule
;    (map (lambda (panel) (let* ((children (get-container-children panel))
;                                (select-type (cadr children))
;                                (select-target (caddr children))
;                                (select-operator (cadddr children))
;                                (the-type (if (not (is-basic-mode?)) (get-combobox-selectedindex select-type) 0))
;                                (targetID (get-comboboxwithdata-selecteddata select-target))
;                                (operator (get-combobox-selectedindex select-operator)))
;                           (create-typed-condition new-rulename the-type targetID operator new-rule)))
;         all-conditions)
    
    ; run through facts and add to rule - this will eventually generalize to actions
    (format #t "saving facts~%~!")
    (map (lambda (panel) (let* ((children (get-container-children panel))
                                (select-type (cadr children))
                                (select-target (caddr children))
                                (set-value (cadddr children))
                                (the-type (get-combobox-selectedindex select-type))
                                (the-value 
                                 (if (= the-type 0)
                                     (get-combobox-selectedindex set-value)
                                     (get-text set-value)))
                                (targetID (get-comboboxwithdata-selecteddata select-target))
                                ; build the expression: 
                                ; (assert targetID) or
                                ; (retract targetID) or
                                ; (set-value! targetID the-value)
                                (the-action-expr 
                                 (string-append
                                  "("
                                  (if (= the-type 0)
                                      (if (= the-value 0)
                                          "assert"
                                          "retract")
                                      "set-value!")
                                  " "
                                  (number->string targetID)
                                  (if (= the-type 1)
                                      (string-append " \"" the-value "\"")
                                      "")
                                  ")")))
                           (format #t "fact: ~a~%~!" the-action-expr)
                           (create-action new-rulename 'fact the-action-expr new-rule)))
         all-facts)
    (format #t "after saving facts, actions: ~a~%~!" (ask (get 'rules new-rule) 'actions))
    
    ; if there's an action, add the action to the rule
    ; need to read the string and break in to s-expressions; should eventually
    ; be able to plug in an improved version of definition-editor here
    ; NOTE: I haven't decided if actions should be stored as strings (currently)
    ; or as s-expressions (commented out) - alex
    ; strings: can store formatting
    ; s-expr: seems more appropriate, but some problems with string values?
    (if (not (equal? then-action-string ""))
        (create-action new-rulename 
                       (cond ((eq? edit-mode 'link) 'then)
                             ((eq? edit-mode 'node) 'before)
                             ((eq? edit-mode 'doc) 'step))
                       then-action-string new-rule))
    (if (not (equal? else-action-string ""))
        (create-action new-rulename 
                       (cond ((eq? edit-mode 'link) 'else)
                             ((eq? edit-mode 'node) 'after)
                             ((eq? edit-mode 'doc) 'init))
                       else-action-string new-rule))
;;        (let ((explist (open-input-string then-action-string)))
;;          (read-action-expr 'then explist new-rulename new-rule)))
;;        (let ((explist (open-input-string else-action-string)))
;;          (read-action-expr 'else explist new-rulename new-rule)))

    ; if we're editing a link, then retrieve the link details
    (cond 
;     ((eq? edit-mode 'link)
;                                        ; update use-alt, alt-destination and alt-text
;           (define the-link the-rule-object)
;           (define uselink (get-checkbox-value editlink-panel-then-link-check))
;           (define destID (get-comboboxwithdata-selecteddata editlink-panel-then-link-choice))
;           (define olduselink (ask the-link 'use-destination))
;           (define olddestID (ask the-link 'destination))
;           (define usealtlink (get-checkbox-value editlink-panel-else-link-check))
;           (define usealttext (get-checkbox-value editlink-panel-else-text-check))
;           (define altdestID (get-comboboxwithdata-selecteddata editlink-panel-else-link-choice))
;           (define oldusealtlink (ask the-link 'use-alt-destination))
;           (define oldaltdestID (ask the-link 'alt-destination))
;           (define alttext (get-text editlink-panel-else-text-message1))
;           (define alttext-fact (if (show-facts?)
;                                    (eq? (get-combobox-selectedindex editlink-panel-else-text-typechoice) 1)
;                                    #f))

;           (ask the-link 'set-use-destination! uselink)
;           (ask the-link 'set-destination! destID)
;           (ask the-link 'set-use-alt-destination! usealtlink)
;           (ask the-link 'set-alt-destination! altdestID)
;           ;; if using alt text, and alttext-fact is true, then set use-alt-text to 'fact,
;           ;; otherwise just set to usealttext
;           (ask the-link 'set-use-alt-text!
;                (if (and usealttext
;                         alttext-fact)
;                    'fact
;                    usealttext))
;           (format #t "*** use-alt-text: ~a ***~%~!" (ask the-link 'use-alt-text))

;           ;; if alttext-fact is true, store the factID in alt text otherwise store text
;           (if alttext-fact
;               (ask the-link 'set-alt-text! (get-comboboxwithdata-selecteddata editlink-panel-else-text-factchoice))
;               (ask the-link 'set-alt-text! alttext))

;           ;; update links in graph if callback provided
;           ;; note: if uselink/usealtlink is false, then pass -1 ie. don't draw line
;           ;; similar for old links
;           (if update-callback
;               (update-callback new-rulename edited-nodeID
;                                olduselink olddestID
;                                oldusealtlink oldaltdestID
;                                uselink destID
;                                usealtlink altdestID
;                                edited-linkID))

;           ;; cache the information of the edited link in the form of a lambda object
;           (after-editlink edited-linkID)
;           
;           ;; added an undoable event 
;           (post-editlink-undoable-event)
;                                        ;(display "[editlink-dialog-ok] ")(display (compoundundomanager-updatelevel undo-manager))(newline)

;           ;; if updatelevel is more than 0 it doeditlink was called right after donewlink
;           ;; in that case a beginupdate had been called so close the compound undoable edit
;           ;; by calling endupdate to close it
;           (if (> (compoundundomanager-updatelevel undo-manager) 0)
;               (compoundundomanager-endupdate undo-manager undo-action redo-action))
;                                        ;(display "[editlink-dialog-ok] ")(display (compoundundomanager-updatelevel undo-manager))(newline)
;           )
          ((eq? edit-mode 'node)
           (define the-node the-rule-object)
           (define nodeID (ask the-node 'ID))
           (after-editnode nodeID)
           (post-edit-noderule-undoable-event)
           )
          )

    ; hide link editor, and reset (for next time)
    (set-component-visible editnode-dialog #f)
    (reset-rule-editor)
    ))

; currently this isn't needed as the action is stored as a string - alex
;;; read the action-expression from the link editor
;;(define (read-action-expr action-type explist new-rulename new-rule)
;;  (let ((input
;;         (try-catch
;;             (begin
;;               (read explist))
;;           (ex <java.lang.Throwable>
;;               (begin
;;                 (display "error reading expressions from editlink")(newline)
;;                 (status-callback (*:toString ex))
;;                 ;(*:printStackTrace ex)
;;                 )))))
;;    (display "read-action-expr: input ")(display input)(newline)
;;    (cond
;;     ((eof-object? input)
;;      ; reached then end-of-file, so done
;;      'ok)
;;     (else
;;      (begin
;;        ; create an action from this expression
;;        (create-action new-rulename action-type input new-rule)

;;        ; and recursively continue to read the expressions
;;        (read-action-expr action-type explist new-rulename new-rule))))))
  

;; empty the container of any children component
;; can consider moving this elsewhere as this is useful
(define (clear-container in-container)
  (let ((children (get-container-children in-container)))
    (if (not (null? children))
        (begin
          (remove-component in-container (car children))
          (clear-container in-container)))))

(define (clear-container-from-index-onwards in-container index)
  (let ((children (get-container-children in-container)))
    (if (> (length children) index)
        (begin
          (remove-component in-container (list-ref children index))
          (clear-container-from-index-onwards in-container index)))))
  
  
; reset the rule editor
(define (reset-rule-editor)
  ; runs through all conditions or facts and removes them
  (define reset-helper clear-container)
  
  ;; empty the two dialogs
  
  ;(reset-helper editlink-panel-top)
  ;(reset-helper editnode-panel-top)
  
  ; remove conditions
  (reset-helper editlink-panel-conditions)
  
  ; remove facts
  ;; not even sure what this does
  ;(reset-helper editnode-panel-then-facts)
  
  ; remove link destination choicebox - these are recreated when link editor is opened
  (if editlink-panel-then-link-choice
      (remove-component editlink-panel-then-choicepanel editlink-panel-then-link-choice))
  (set! editlink-panel-then-link-choice #f)
  (if editlink-panel-else-link-choice
      (remove-component editlink-panel-else-choicepanel editlink-panel-else-link-choice))
  (set! editlink-panel-else-link-choice #f)
  (display "RESET removed editlink-panel-else-link-choice")(newline)

  ; if facts are showing, reset the alt text to text, not fact
  (if (show-facts?)
      (set-combobox-selection editlink-panel-else-text-typechoice 0))

  ; clear all checkboxes and entry fields
  (set-checkbox-value editlink-panel-then-link-check #f)
  (set-checkbox-value editlink-panel-else-link-check #f)
  (set-checkbox-value editlink-panel-else-text-check #f)
  (set-text editlink-panel-else-text-message1 "")
  (set-combobox-selection editlink-dialog-andor-operator 0)
  (set-text editlink-dialog-then-actiontext "")
  (set-text editlink-dialog-else-actiontext ""))

; delete selected condition
(define (editlink-dialog-delete-conditions)
  (editlink-dialog-delete editlink-panel-conditions))

; delete selected entries in a container, used by conditions and facts
(define (editlink-dialog-delete in-container)
  (define all-children 
    (if in-container
        (get-container-children in-container)))
  (define (delete-recur panels)
    (if (null? panels) #t
        (begin
          (if (eq? #t (get-checkbox-value (car (get-container-children (car panels)))))
              (begin
                (remove-component in-container (car panels))
                (pack-frame editlink-dialog))
              #t)
          (delete-recur (cdr panels)))))
  (if all-children
      (delete-recur all-children)))

;;
;; facts
;; 

; add an fact
(define (editlink-dialog-add-fact)
  (create-fact-panel 'assert -1 "")
  (pack-frame editlink-dialog))

; create an fact panel
; the-action: the type of action 'assert, 'retract or 'set-value!
; targetID: the currently selected fact, if any (pass in -1 if none selected)
; the-value: value that will be set (for 'set-value! only)
(define (create-fact-panel the-action targetID the-value)
  (let* ((top-panel (make-panel))
         (the-checkbox (make-checkbox ""))
         (the-type-choice (make-combobox "True/False" "Text"))
         (the-boolean-choice (make-combobox "True" "False"))
         (the-string-entry (make-textfield "[the fact text...........]" 20))
         (the-fact-list-boolean (create-fact-choice 'boolean top-panel selected-fact-in-action targetID))
         (the-fact-list-string (create-fact-choice 'string top-panel selected-fact-in-action targetID)))
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;; nolong add to the facts panel
    ;(add-component editnode-panel-then-facts top-panel)
    
    ;; add checkbox
    ;(add-component top-panel the-checkbox)

    ;; add type choice
    (add-component top-panel the-type-choice)
  
    ; set type choice
    (set-combobox-selection the-type-choice (if (eq? the-action 'set-value!) 1 0))
    
    ;; add the appropriate components
    (set-fact-panel-components top-panel (if (eq? the-action 'set-value!) 1 0)
                               the-fact-list-string the-string-entry
                               the-fact-list-boolean the-boolean-choice)
    
    ; set value
    (if (eq? the-action 'set-value!)
        (set-text the-string-entry the-value)
        (set-combobox-selection the-boolean-choice (if (eq? the-action 'assert) 0 1)))

    ; add type callback
    (add-actionlistener the-type-choice
                        (make-actionlistener (lambda (source)
                                               (selected-type-in-action source top-panel
                                                                        the-fact-list-boolean the-boolean-choice
                                                                        the-fact-list-string the-string-entry))))

    ; return the panel
    top-panel))

;; new version of the above (will eventually replace the above)
;; fact type is only needed if a factID is selected
(define (create-fact-panel2 fact-type factID the-value)
  (display "create fact panel 2")(newline)
  
  (let* ((top-panel (make-panel))
         ;(the-checkbox (make-checkbox ""))
         (the-type-choice (make-combobox "True/False" "Text"))
         (the-boolean-choice (make-combobox "True" "False"))
         (the-string-entry (make-textfield "[the fact text...........]" 20))
         (the-fact-list-boolean (create-fact-choice 'boolean top-panel selected-fact-in-action factID))
         (the-fact-list-string (create-fact-choice 'string top-panel selected-fact-in-action factID)))
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;; nolong add to the facts panel
    ;(add-component editnode-panel-then-facts top-panel)
    
    ;; add checkbox
    ;(add-component top-panel the-checkbox)

    ;; add type choice
    (add-component top-panel the-type-choice)
  
    ;; choose the fact type index
;    (define (fact-type-index)
;      (cond ((equal? "boolean" fact-type) 0)
;            ((equal? "string" fact-type) 1)))
    (define (fact-type-index)
      (cond ((equal? 'boolean fact-type) 0)
            ((equal? 'string fact-type) 1)
            ((or (equal? 'assert fact-type)
                 (equal? 'retract fact-type)) 0)
            ((equal? 'set-value! fact-type) 1)
            (else 0)
            ))
    ;(set-combobox-selection the-type-choice (if (eq? the-action 'set-value!) 1 0))
    
;    (if (eq? the-action 'set-value!)
;        (set-text the-string-entry the-value)
;        (set-combobox-selection the-boolean-choice (if (eq? the-action 'assert) 0 1)))
    
    (display "fact type index ")(display fact-type)(newline)
    
    ; set type choice
;    (set-combobox-selection the-type-choice (if (eq? the-action 'set-value!) 1 0))
    (set-combobox-selection the-type-choice (fact-type-index))
    
    ;; add the appropriate components
    (set-fact-panel-components top-panel (fact-type-index)
                               the-fact-list-string the-string-entry
                               the-fact-list-boolean the-boolean-choice)
    
    ; set value
;    (if (eq? the-action 'set-value!)
;        (set-text the-string-entry the-value)
;        (set-combobox-selection the-boolean-choice (if (eq? the-action 'assert) 0 1)))
    
    
    (display "BOOLEAN VALUE ")(display the-value)(display "\"")(newline)
    (display "the value ")(display (equal? the-value #!void))(newline)
    
    ;; set current value of fact
    (if factID ;; if factID false
        (cond ((equal? fact-type 'boolean)
               (cond ((equal? the-value #t) (set-combobox-selection the-boolean-choice 0))
                     ((equal? the-value #f) (set-combobox-selection the-boolean-choice 1))))
              ((equal? fact-type 'assert)
               (set-combobox-selection the-boolean-choice 0))
              ((equal? fact-type 'retract)
               (set-combobox-selection the-boolean-choice 1))
              ((or (equal? fact-type 'string)
                   (equal? fact-type 'set-value!))
               (set-text the-string-entry the-value))))
    
    ; add type callback
    (add-actionlistener the-type-choice
                        (make-actionlistener (lambda (source)
                                               (selected-type-in-action source top-panel
                                                                        the-fact-list-boolean the-boolean-choice
                                                                        the-fact-list-string the-string-entry))))

    ; return the panel
    top-panel))

; set fact panel components
(define (set-fact-panel-components top-panel the-type
                                   the-fact-list-string the-string-entry
                                   the-fact-list-boolean the-boolean-choice)
  (if (eq? the-type 1)
      ; string
      (begin
        (add-component top-panel the-fact-list-string)
        (add-component top-panel the-string-entry))
      ; boolean
      (begin
        (add-component top-panel the-fact-list-boolean)
        (add-component top-panel the-boolean-choice))))

; selection of assertion type changed, so change the fact list and value entry/choice
(define (selected-type-in-action c top-panel
                                    the-fact-list-boolean the-boolean-choice
                                    the-fact-list-string the-string-entry)
  (format #t "selected-type-in-assertion~%~!")
  
  ; remove the current target and operator lists
;  (let* ((children (get-container-children top-panel))
;         (old-target (caddr children))
;         (old-operator (cadddr children)))
;    (remove-component top-panel old-target)
;    (remove-component top-panel old-operator))
  
  (let* ((children (get-container-children top-panel))
         (old-target (cadr children))
         (old-operator (caddr children)))
    (remove-component top-panel old-target)
    (remove-component top-panel old-operator))
  
  ; add the appropriate components and set value
  (set-fact-panel-components top-panel (get-combobox-selectedindex c)
                             the-fact-list-string the-string-entry
                             the-fact-list-boolean the-boolean-choice)
  
  (pack-frame editlink-dialog))

  ; selection of action changed: do nothing for now
(define (selected-action-in-action c)
  (format #t "selected-fact-in-action~%~!"))

; selection of fact changed: do nothing for now
(define (selected-fact-in-action c type)
  (format #t "selected-fact-in-action: ~a~%~!" type))

; delete selected fact
(define (editlink-dialog-delete-facts)
  (editlink-dialog-delete editlink-panel-then-facts))

;;
;; facts as alternative text
;; 

; selection of alternative type changed, so change the entry field/choice
(define (selected-type-in-link c)
  ;(format #t "selected-type-in-link~%~!")

  ; remove the current target and operator lists
  (let* ((children (get-container-children editlink-panel-else-text))
         (old-type (caddr children)))
    (remove-component editlink-panel-else-text old-type))

  ; add the appropriate component
  (let ((the-type (get-combobox-selectedindex c)))
    (if (eq? the-type 0)
        (add-component editlink-panel-else-text editlink-panel-else-text-message1)
        (begin
          (add-component editlink-panel-else-text editlink-panel-else-text-factchoice)
          ; if fact selected, uncheck the checkbox
          (if (and editlink-panel-else-text-check
                   editlink-panel-else-text-factchoice)
              (set-checkbox-value editlink-panel-else-text-check
                                  (not (eq? (get-comboboxwithdata-selecteddata editlink-panel-else-text-factchoice) -1)))))))

  (update-ok-button-state)
  (pack-frame editlink-dialog))

  ; selection of fact changed: do nothing for now
(define (selected-fact-in-link c type)
  (format #t "selected-fact-in-link: ~a, data: ~a~%~!" type (get-comboboxwithdata-selecteddata c))
    ; if "none" fact selected, uncheck the checkbox
  (if (and (eq? c editlink-panel-else-text-factchoice)
           editlink-panel-else-text-check 
           editlink-panel-else-text-factchoice)
      (set-checkbox-value editlink-panel-else-text-check
                      (not (eq? (get-comboboxwithdata-selecteddata editlink-panel-else-text-factchoice) -1))))
  (update-ok-button-state))
  