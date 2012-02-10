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
  (require "../kawa/ui/menu.scm") ;; make-menu, make-menu-bar
  
  (require "../kawa/system.scm") ; is-windows?
  (require "../kawa/strings.scm") ;; to-string
  
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
  (require "node-graphview.scm") ;update-link-display
  (require 'srfi-1) ;; remove
  )

; export
(module-export doeditlink doeditnoderule doeditdocrule
               create-editlink-dialog
               create-editnode-dialog
               create-if-condition-panel
               create-actions-main-panel
               create-update-text-action-panel
               create-rules-manager
               
               ;create-facts-main-panel
               ;create-then-action-panel
               
               get-edited-linkID
               obj-convertion-2.2)
               

; remember which link we're editing
(define-private edited-linkID '())
(define (get-edited-linkID) edited-linkID)

;; keep track of which rule we're editing
(define edited-ruleID '())

; remember currently edited node
;(define-private edited-nodeID '()) ;; now using nodeeditor.scm's edited-nodeID

; callback for updating links in graph
(define-private update-callback #f)

; are we editing a link (default) or just a rule?
(define-private edit-mode 'link)

;;  =====================
;;;; pre 2.2 conversion
;;  =====================
(define (obj-convertion-2.2)
  (if (<= loaded-file-version 2.1)
      (begin
        (table-map 'links convert-pre-2.2-links)
        (table-map 'nodes convert-pre-2.2-nodes)
        ;(table-map 'actions convert-pre-2.2-actions)
        )))

(define (convert-pre-2.2-nodes nodeID node-obj)
  ;; convert standard node
  (let* ((ruleID (ask node-obj 'rule))
         (rule-obj (get 'rules ruleID))
         (node-name (ask node-obj 'name))
         (anywhere? (ask node-obj 'anywhere?))
         (new-ruleID (create-typed-rule2 "Set fact" 'node 'and #f nodeID))
         ;(conditions (ask rule-obj 'conditions))
         ;(actions (ask rule-obj 'actions))
         )
    
    ;; add the dummy add-anywhere-link action
    ;; (does nothing, just a place holder to let it show up)
    (if anywhere?
        (create-action "Enable Link" 'entered-node
                       (list 'add-anywhere-link)
                       new-ruleID))
    
    ;; add all actions to the new rule (only set fact actions are on pre 2.2 nodes)
    (if rule-obj
        (begin
          (define actions (ask rule-obj 'actions))
          (map (lambda (actionID)
                 (define action (get 'actions actionID))
                 (define action-string (ask action 'expr))
                 
                 (define action-input-port (open-input-string action-string))
                 (define action-sexpr (read action-input-port))
                 
                 (if (not (eof-object? action-sexpr))
                     (begin
                       (display "SEXPR ")(display action-sexpr)(newline)
                       (display "pair? ")(display (pair? action-sexpr))(newline)
                       (define new-rule (get 'rules new-ruleID))
                       (create-action node-name 'entered-node
                                      action-sexpr
                                      new-ruleID)
                       
                       ;;(create-condition name nodeID operator ruleID . args)
                       ;; add the conditions to the rules
                       ))
                 ) actions)
          
          ;; transfer the condition from the original old rule to the new rules
        ;(create-typed-condition name type targetID operator ruleID . args)
        (define old-conditions (ask rule-obj 'conditions))
        (map (lambda (condition)
               (define this-cond (get 'conditions condition))
               (let ((type (ask this-cond 'type))
                     (targetID (ask this-cond 'targetID))
                     (operator (ask this-cond 'operator)))
                 (create-typed-condition "Enable link" type targetID operator new-ruleID))
               ) old-conditions)
          )
          ;; no rule for node so no need to transfer anything to new rule
        (begin
          (display "no rule for node ")(display nodeID)(newline)
          ))
  ))

;; convertion of pre 2.2 links to 2.2 format
;; need to create 2 rules one for(if) and against(else) the condition in rule
;; note old rules are still around in the datatable, just not invoked
(define (convert-pre-2.2-links linkID link-obj)
  ;(display "CONVERT LINKS pre 2.2")(newline)

  (define selected-rule-ID (ask link-obj 'rule))
  (if (not (eq? selected-rule-ID 'not-set))
      (begin

        (define selected-rule (get 'rules selected-rule-ID))
        (define link-name (ask link-obj 'name))
        (define link-dest1 (ask link-obj 'destination))
        (define link-uselink (ask link-obj 'use-destination))
        (define link-usealtlink (ask link-obj 'use-alt-destination))
        (define link-usealttext (ask link-obj 'use-alt-text))
        (define link-dest2 (ask link-obj 'alt-destination))
        (define link-alttext (ask link-obj 'alt-text))
        (define link-start-index (ask link-obj 'start-index))
        (define link-end-index (ask link-obj 'end-index))
        
        (define if-rule-ID (create-typed-rule2 "THEN" 'link (ask selected-rule 'expression) #f linkID))
        (define else-rule-ID (create-typed-rule2 "ELSE" 'link (ask selected-rule 'expression) #t linkID))
        (define if-rule (get 'rules if-rule-ID))
        (define else-rule (get 'rules else-rule-ID))
                                        ;)
        ;; alt-text (text and fact)
        (if link-usealttext
            (if (eq? link-usealttext #t)
                (begin ;; show alt text
                  ;; add a new action panel
                  (create-action link-name 'displayed-node
                                 (list 'replace-link-text
                                       (list 'quote 'text)
                                       link-alttext
                                       ;(string-append "\"" link-alttext "\"")
                                       linkID)
                                 else-rule-ID
                                 )
                  )
                (begin ;; show fact text
                  (create-action link-name 'displayed-node
                                 (list 'replace-link-text
                                       (list 'quote 'text)
                                        ;(string-append "\"" link-alttext "\"")
                                       link-alttext ;; this is actually factID
                                       linkID)
                                 else-rule-ID)
                  )))

        ;; default destination
        (if (not (equal? link-dest1 -1))
            (create-action link-name 'clicked-link
                           (list 'follow-link
                                 linkID
                                 (ask if-rule 'ID)
                                 (list 'quote 'default)
                                 link-dest1)
                           if-rule-ID))

        ;; link-dest2 is used in the else so it should be in the not rules instead
        (if (not (equal? link-dest2 -1))
            (create-action link-name 'clicked-link
                           (list 'follow-link
                                 linkID
                                 (ask else-rule 'ID)
                                 (list 'quote 'default)
                                 link-dest2)
                           else-rule-ID))

        ;; transfer the condition from the original old rule to the new rules
        ;(create-typed-condition name type targetID operator ruleID . args)
        (define old-conditions (ask selected-rule 'conditions))
        (map (lambda (condition)
               (define this-cond (get 'conditions condition))
               (let ((type (ask this-cond 'type))
                     (targetID (ask this-cond 'targetID))
                     (operator (ask this-cond 'operator)))
                 (create-typed-condition "If-rule" type targetID operator if-rule-ID )
                 (create-typed-condition "else-rule" type targetID operator else-rule-ID ))
               ) old-conditions)
        )))

;;;; doedit

; edit a link - called from hteditor.scm
;; in-default-link-text not used any more
;; NOTE: this should be renamed as doeditrule now
;;       since it targets one rule inside the link
(define (doeditlink selected-linkID in-edited-nodeID in-callback in-ruleID);in-default-link-text)
  
  ;; cache a version of the unedited link for creation of undo
  (before-editlink selected-linkID)
  
  ; set editing mode
  ;; NOTE: done in create-rules-manager now
  ;(set! edit-mode 'link)
  
  ; set titles
  ;; Note: not used at the moment
;  (set-tabpanel-label-at editlink-dialog-tabpanel 0 "Link")
;  (set-tabpanel-label-at editlink-dialog-tabpanel 1 "Then action")
;  (set-tabpanel-label-at editlink-dialog-tabpanel 2 "Else action")
  
  ; reset the editor, in case this failed when we closed last time
  (reset-rule-editor)
  
  ; remember which link we're editing
  (set! edited-linkID selected-linkID)
  
  ; remember which node we're editing
  (set! edited-nodeID in-edited-nodeID)
  
  ;; remember which rule we're editing
  (set! edited-ruleID in-ruleID)
  
  ; remember callback
  (set! update-callback in-callback)
  
  ; get all the details from the link object
  (let* ((link-obj (get 'links edited-linkID))
         ;(selected-rule-ID (ask link-obj 'rule))
         ;(rule-lst (ask link-obj 'rule-lst))
         (link-name (ask link-obj 'name))
         ;(link-dest1 (ask link-obj 'destination))
         ;(link-uselink (ask link-obj 'use-destination))
         ;(link-usealtlink (ask link-obj 'use-alt-destination))
         ;(link-dest2 (ask link-obj 'alt-destination))
         )
    
    ; show link name
    (set-dialog-title editlink-dialog (string-append "Edit link: "
                                                     link-name
                                                     (if (show-IDs?) (string-append " (" (number->string edited-linkID) ")") "")))
    
    ;; add appropriate panels to editlink-dialog
    (add-component editlink-panel-top editlink-panel-if)
    (add-component editlink-panel-top actions-main-panel)
    
        ;; TODO conver this to be using the new stuff
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
  (format #t "doeditrule: in-edited-nodeID: ~a~%~!" in-edited-nodeID)

  ; set edit mode to rule
  ;; NOTE: done in create-rules-manager now
  ;(set! edit-mode 'node)
  
  (nodeeditor-save) ;; save text content before create sexpr in before-editnode
  (before-editnode in-edited-nodeID)
  
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
  
  ; remember callback ie. none
  ;; why do update-callback #f?
  (set! update-callback #f)
  
  ; get the rule from the node
  (let* ((edited-node (get 'nodes edited-nodeID))
         ;;(selected-rule (ask edited-node 'rule))
         (node-name (ask edited-node 'name))
         ;;(anywhere (ask edited-node 'anywhere?))
         )

    ;; show node name
    (set-dialog-title editlink-dialog (string-append "Edit rule for node: " node-name))
    )

  ;; add if panel to editnode-dialog
  (add-component editlink-panel-top editlink-panel-if)
  (add-component editlink-panel-top actions-main-panel)

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
  
  ; set edit mode to rule
  ;; NOTE: done in create-rules-manager now
  ;(set! edit-mode 'doc)
  
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

    ; reconstruct rule if necessary
    (populate-rule-editor selected-rule))

  ; pack the UI and show
  (pack-frame editlink-dialog)
  (center-frame-in-parent editlink-dialog (get-main-ui-frame))
  (set-component-visible editlink-dialog #t))

;(doeditlink selected-linkID in-edited-nodeID in-callback in-default-link-text)
;(doeditnoderule in-edited-nodeID)
;(doeditdocrule)

;;  ===============
;;;; rule manager
;;  ===============

;; rmgr stands for rule manager
(define rules-manager-main-dialog #f)
(define rmgr-rules-list-panel #f) ;; the panel that contain all the rule panels
;(define rmgr-rules-panel-list #f) ;; a list of all the rule panels (sigh sorry for the confusing names)
;(define rmgr-rule-lst (list))

;; get rule-lst from currently edited object
(define (rmgr-rule-lst)
  (ask (rmgr-get-currently-edited) 'rule-lst))

;; update rule position in object's rule-lst
(define (rmgr-set-rule-lst new-lst)
  (ask (rmgr-get-currently-edited) 'set-rule-lst new-lst))

;; get the child of rmgr-rules-list-panel and see which position
;; this rule-panel is in
(define (rule-panel-position rule-panel)
  (list-index (lambda (obj) (eq? rule-panel obj)) (get-container-children rmgr-rules-list-panel) ))

;; one instance of the rule panel (one entry on the rule list)
;; only called by add-rule-panel which makes sure we are passed a valid ruleID (gotten from actual rule-lst)
(define (make-rule-panel ruleID)
  (define top-panel (make-panel))
  (set-container-layout top-panel 'flow 'left)

  (define rule-checkbox (make-checkbox ""))

  (define rule-name-label #f)

  ;; assume ruleID valid   
  (define rule-obj (get 'rules ruleID))
  (define rule-name (ask rule-obj 'name))
  (set! rule-name-label (make-label-with-title rule-name))
  
  ;; make buttons
  (define rule-edit-button (make-button "Edit Rule"))
  (define fall-through-button (make-button "Fall"))
  (define shift-up-button (make-button "Up"))
  (define shift-down-button (make-button "Down"))
  
  ;; edit button goes through all the rules panel and 
  ;; figure out which position is it in the parent rules-list-panel
  ;; then look at rmgr-rule-lst which keeps the ruleID of all the rules in that order
  
  (add-actionlistener rule-edit-button 
                      (make-actionlistener
                       (lambda (e)
                         ;(display "EDIT RULE ")(display edit-mode)(newline)
                         (define sibling-lst (get-container-children (get-parent top-panel)))
                         (define pos (list-index (lambda (panel) (eq? top-panel panel)) sibling-lst))
                         (cond ((equal? edit-mode 'link)
                                (define rule-position (rule-panel-position top-panel))
                                (define ruleID (list-ref (rmgr-rule-lst) rule-position))
                                
                                (doeditlink selected-linkID edited-nodeID update-link-display ruleID)
                                )
                               ((equal? edit-mode 'node)
                                (define rule-position (rule-panel-position top-panel))
                                (define ruleID (list-ref (rmgr-rule-lst) rule-position))
                                (doeditnoderule edited-nodeID ruleID)
                                )
                               ((equal? edit-mode 'doc)
                                #f
                                ))
                         )))

  (define (fall-through-button-callback e)
    ;; alternate button display between "Fall" and "Stop"
    (if (equal? (get-button-label fall-through-button) "Fall")
        (begin
          (ask rule-obj 'set-fall-through? #f)
          (set-button-label fall-through-button "Stop")
          )
        (begin
          (ask rule-obj 'set-fall-through? #t)
          (set-button-label fall-through-button "Fall")
          ))
    (pack-frame rules-manager-main-dialog))
  
  (add-actionlistener fall-through-button 
                      (make-actionlistener
                       fall-through-button-callback))
  
  (define rule-fall-through? (ask rule-obj 'fall-through?))
  (if (not rule-fall-through?)
      (set-button-label fall-through-button "Fall"))
  
  ;; TODO: MISC (non hypedyn) write a better swap that does not need two element 
  ;; to be side by side
  (define (swap-first-two lst)
    (list-insert (cdr lst) (car lst) 1))
  
  ;; swap object at index with the object right of it
  (define (swap-right lst index)
    (if (>= index 0) 
        (append (take lst index) ;; take first (index) number of object in front (left) 
                (swap-first-two (drop lst index)))
        lst)) ;; if already left most then do nothing
  
  ;; the same thing just a positional difference
  (define (swap-left lst index)
    (swap-right lst (- index 1)))
  
  (add-actionlistener
   shift-up-button
   (make-actionlistener
    (lambda (e)
      (display "UP ")(newline)
      (define position (list-index (lambda (o) (equal? o ruleID)) (rmgr-rule-lst)))
      ;; swap this rule panel up 
      ;; swap ruleID in rmgr-rule-lst leftwards
      (display "b4 shift up ")(display (rmgr-rule-lst))(newline)
;      (set! rmgr-rule-lst
;            (swap-left rmgr-rule-lst position))

      ;; set the entire rule-lst in object 
;      (define parentID (ask rule-obj 'parentID))
;      (define parent-type (ask rule-obj 'type))
;      (define parent-obj
;        (case parent-type
;          ((link) (get 'links parentID))
;          ((node) (get 'nodes parentID))))
;      (ask parent-obj 'set-rule-lst (list-copy rmgr-rule-lst))
      
      ;; update rule position in object's rule-lst
      (rmgr-set-rule-lst (swap-left (rmgr-rule-lst) position))

      (shift-panel rmgr-rules-list-panel position (- position 1))
      
      (display "after shift up ")(display (rmgr-rule-lst))(newline)
                                          
      ;; need to pack-frame for update?
      ;(pack-frame rules-manager-main-dialog)
      (validate-container rules-manager-main-dialog)
      )
    ))
  
  (add-actionlistener 
   shift-down-button
   (make-actionlistener
    (lambda (e)
      (display "DOWN ")(newline)
      (define position (list-index (lambda (o) (equal? o ruleID)) (rmgr-rule-lst)))
      (display "position ")(display position)(newline)
      ;; swap this rule panel up 
      ;; swap ruleID in rmgr-rule-lst leftwards
;      (set! rmgr-rule-lst
;            (swap-right rmgr-rule-lst position))

      ;; set the entire rule-lst in object 
;      (define parentID (ask rule-obj 'parentID))
;      (define parent-type (ask rule-obj 'type))
;      (define parent-obj
;        (case parent-type
;          ((link) (get 'links parentID))
;          ((node) (get 'nodes parentID))))
;      (ask parent-obj 'set-rule-lst (list-copy rmgr-rule-lst))

      (shift-panel rmgr-rules-list-panel position (+ position 1))
      
      (rmgr-set-rule-lst (swap-right (rmgr-rule-lst) position))
      
      (display "after shift down ")(display rmgr-rule-lst)(newline)
      
      ;; need to pack-frame for update?
      ;(pack-frame rules-manager-main-dialog)
      (validate-container rules-manager-main-dialog)
      )
    ))
  
  (add-component top-panel rule-checkbox)
  (add-component top-panel rule-name-label)
  
  (add-component top-panel rule-edit-button)
  (add-component top-panel fall-through-button)
  (add-component top-panel shift-up-button)
  (add-component top-panel shift-down-button)
  
  ;; make rule panel
  top-panel)


;; TODO: add-rule and remove-rule needs to have an undo event
;;       that updates the datastructure of what is added/removed

;; TODO: we need to update the rule manager after editing a rule because we are 
;;       creating new rules and adding it to the object

;; add rule (makes a rule panel inside rmgr-rules-list-panel)
;; when pos is added it adds the panel and the ruleID in that particular position
;; NOTE: don't think i have any use for pos yet (thought i did)
(define (add-rule-panel ruleID #!optional pos)
  (if (and rmgr-rules-list-panel
           rules-manager-main-dialog)
      (begin
        ;; create new rule if 'new passed in
        (if (equal? ruleID 'new)
            (begin
              (set! ruleID (create-typed-rule2 "new rule" edit-mode 'and #f
                                                (rmgr-get-currently-edited-ID)))
              ;; only need to add it when its a new ruleID
              ;; TOFIX: when calling add-rule-panel to redo, we need to add to the rule-lst
              ;;        whereas when we're 
              (rmgr-set-rule-lst (append (rmgr-rule-lst) (list ruleID)))
              ))
        
        ;; if position given use it (disabled for now)
;        (if pos
;            (begin
;              (set! rmgr-rule-lst (list-insert rmgr-rule-lst ruleID pos))
;              (add-component-at rmgr-rules-list-panel (make-rule-panel ruleID) pos))
;            (begin
;              (set! rmgr-rule-lst (append rmgr-rule-lst (list ruleID)))
;              (add-component rmgr-rules-list-panel (make-rule-panel ruleID))))
         
        (add-component rmgr-rules-list-panel (make-rule-panel ruleID))
        
        (pack-frame rules-manager-main-dialog)
        
        ;; return ruleID
        ruleID)
      ;;
      (begin
        (display "ERROR: in add-rule-panel")(newline)
      -1)))

;; go through all the rule panels in rmgr-rules-list-panel
;; remove those with checkbox checked
;; remove the corresponding ruleID in rmgr-rule-list
;; remove the rule in the object
(define (remove-selected-rule-panel)
  (define rule-panel-lst (get-container-children rmgr-rules-list-panel))
  
  (display "remove-selected-rule-panel")(newline)
  (display "edit-mode ")(display edit-mode)(newline)
  
  (define rule-parent
    (case edit-mode
      ;; note: link and node are read as 'link 'node in the special case syntax 
      ((link) (display "came in link ")(newline) (get 'links edited-linkID))
      ((node) (display "came in node ")(newline) (get 'nodes edited-nodeID))))
  
  (map (lambda (rule-panel ruleID)
         ;(display "rule panel ")(display rule-panel)(newline)
         ;(display "rule ID ")(display ruleID)(newline)
         (define component-lst (get-container-children rule-panel))
         (define this-checkbox (car component-lst))
         ;(display "this checkbox? ")(display (invoke this-checkbox 'get-class))(newline)
         ;(display "checked? ")(display (get-checkbox-value this-checkbox))(newline)

         ;; if checkbox checked remove
         (if (get-checkbox-value this-checkbox)
             (begin
               (remove-component rmgr-rules-list-panel rule-panel)
               (pack-frame rules-manager-main-dialog)
               ;(set! rmgr-rule-lst (remove (lambda (thisruleID) (= ruleID thisruleID)) rmgr-rule-lst))
               (rmgr-set-rule-lst (remove (lambda (thisruleID) (= ruleID thisruleID)) (rmgr-rule-lst)))
               (ask rule-parent 'remove-rule ruleID)
               ))
         ) rule-panel-lst (rmgr-rule-lst))
  )

;; assume rule manager is correctly loaded (rule-panel-lst correspond to rmgr-rule-lst)
;; we're deleting a rule within rule manager 
(define (check-rule-panel ruleID)
  (define rule-panel-lst (get-container-children rmgr-rules-list-panel))
  (map (lambda (rule-panel this-ruleID)
         (define component-lst (get-container-children rule-panel))
         (define this-checkbox (car component-lst))
         ;; if checkbox checked remove
         (if (= this-ruleID ruleID)
             (set-checkbox-value this-checkbox #t))
         ) rule-panel-lst (rmgr-rule-lst)))

;; remove rule assumes that obj is the currently 
;; edited object in the rule manager
;; TODO: remove rules from 'rules list when deleting
;;       but this would mean when we delete and undo, we have to recreate
;;       the same rule (which should be the right thing to do)
(define (rmgr-remove-rule ruleID)
  (define index (list-index (lambda (rid) (= rid ruleID)) (rmgr-rule-lst)))    ;; get the index in rmgr-rule-lst and rmgr-rules-list-panel
;  (display "find ")(display ruleID)(newline)
;  (display "rmgr-rule-lst ")(display (rmgr-rule-lst))(newline)
;  (display "edited ID ")(display (rmgr-get-currently-edited-ID))(newline)
  
;  (set! rmgr-rule-lst (remove (lambda (rid) (= rid ruleID)) rmgr-rule-lst))
  (rmgr-set-rule-lst (remove (lambda (rid) (= rid ruleID)) (rmgr-rule-lst)))
  (define comp-lst (get-container-children rmgr-rules-list-panel))
  (define panel-to-remove (list-ref comp-lst index))
  (remove-component rmgr-rules-list-panel panel-to-remove)
  (pack-frame rules-manager-main-dialog)
  ;(ask (rmgr-get-currently-edited) 'set-rule-lst rmgr-rule-lst)
  )

;; get the object current edited by rule manager
(define (rmgr-get-currently-edited)
  (case edit-mode
    ((link) (get 'links edited-linkID))
    ((node) (get 'nodes edited-nodeID))
    ((doc) -1)))

(define (rmgr-get-currently-edited-ID)
  (case edit-mode
    ((link) edited-linkID)
    ((node) edited-nodeID)
    ((doc) -1))
  )

;; target type might be 'link 'node 'doc
;; display the dialog with the list of rules attached to the object (which can be any of the target-type)
(define (create-rules-manager target-type obj-ID . args)
  (display "CREATE RULES MAN")(newline)
  
  ;;set edit-mode
  (display "target-type ")(display target-type)(newline)
  (set! edit-mode target-type)
  
  (set! rules-manager-main-dialog (make-dialog (get-nodeeditor-frame) "Rule Editor" #t))
  
  (define rules-manager-main-panel (make-panel))
  (set-container-layout rules-manager-main-panel 'border)
  (add-component rules-manager-main-dialog rules-manager-main-panel)
  
  (define rules-menu-bar (make-menu-bar))
  (define rules-menu (make-menu "Edit"))
  (if (is-undo-enabled?)
      (begin
        (add-menu-action rules-menu undo-action)
        (add-menu-action rules-menu redo-action)

        ;; adding edit menu to rules manager but it is not visible
        ;; this is the only way I know to get the undo working on rules manager
        (add-component rules-manager-main-panel rules-menu-bar)
        (add-component rules-menu-bar rules-menu)
        ))
  
  ;; label display
  (define rules-label (make-label-with-title "Rules"))
  (define rules-label-panel (make-panel))
  (add-component rules-label-panel rules-label)
  (add-component rules-manager-main-panel rules-label-panel 'border-north)
  
  (define center-panel (make-panel))
  (set-container-layout center-panel 'vertical)
  (add-component rules-manager-main-panel center-panel 'border-center)
  
  ;; list of rules
  (set! rmgr-rules-list-panel (make-panel))
  (set-container-layout rmgr-rules-list-panel 'vertical)
  (add-component center-panel rmgr-rules-list-panel)
  
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
  
  
  ;; TODO: rules manager and edit node pane should pop up
  ;;       when we undo and redo
  
  ;; TOFIX: look into why the name of the new rule changes after undo redo to the name of the node/link
  ;;        from its original "new rule"
  (add-actionlistener add-rule-button 
                      (make-actionlistener
                       (lambda (e)
                         (define new-rule-ID (add-rule-panel 'new))
                         
                         ;; post undo
                         (compoundundomanager-postedit
                          undo-manager
                          (make-undoable-edit "Add Rule"
                                              (lambda () ;; undo
                                                (rmgr-remove-rule new-rule-ID)
                                                )
                                              (lambda () ;; redo
                                                (add-rule-panel new-rule-ID)
                                                (rmgr-set-rule-lst (append (rmgr-rule-lst) (list new-rule-ID)))
                                                ;(ask (rmgr-get-currently-edited) 'set-rule-lst rmgr-rule-lst)
                                                )))
                         )))
  
  (add-actionlistener delete-rule-button 
                      (make-actionlistener
                       (lambda (e)
                         (remove-selected-rule-panel)
                         )))
  
  (add-actionlistener rules-dialog-close
                      (make-actionlistener
                       (lambda (e)
                         (set-component-visible rules-manager-main-dialog #f)
                         )))
  
  ;; empty the rule ID list (corresponds to the rule list in rmgr) 
  ;; NOTE: rmgr-rule-lst is not longer a separate list caching the object's rule-lst
  ;;       but it is now a function that returns the object's rule-lst
  ;(set! rmgr-rule-lst '())
  
  ;; populate the rule manager
  (cond ((equal? target-type 'link)
         (set! edited-linkID obj-ID)
         (define in-link (get 'links obj-ID))
         (define rule-lst (ask in-link 'rule-lst))
         (display "rule lst ")(display rule-lst)(newline)
         (map (lambda (ruleID)
                (add-rule-panel ruleID)
                ) rule-lst)
         )
        ((equal? target-type 'node)
         (set! edited-nodeID obj-ID)
         (define in-node (get 'nodes obj-ID))
         (define rule-lst (ask in-node 'rule-lst))
         (display "rule lst ")(display rule-lst)(newline)
         (map (lambda (ruleID)
                (add-rule-panel ruleID)
                ) rule-lst)
         )
        )
  
  (pack-frame rules-manager-main-dialog)
  (set-component-visible rules-manager-main-dialog #t)
  )

; reconstruct a rule
; this is called from the doeditlink/doeditnoderule/doeditdocrule procedures above
; to finish building the GUI representation of the link/rule
(define (populate-rule-editor edited-ruleID)
  (display "edited rule ")(display edited-ruleID)(newline)
  (if (not (eq? 'not-set edited-ruleID))
      (let* ((rule-obj (get 'rules edited-ruleID))
             (conditions (ask rule-obj 'conditions))
             (actions (ask rule-obj 'actions))
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
                 (create-condition-panel the-type targetID operator -1))) ;; should allow edited note to be a choice in condition?
             conditions)
        
        ; build actions (show them in the ui)
        (map (lambda (actionID)
               (define action (get 'actions actionID))
               (define action-sexpr (ask action 'expr))
               
               (cond  
                 ((equal? 'follow-link (car action-sexpr)) 
                  (display "inside follow link")(newline)
                  (define dest-nodeID (list-ref action-sexpr 4))
                  (add-specific-action "follow link to" dest-nodeID))
                 
                 ((equal? 'replace-link-text (car action-sexpr))  ;(replace-link-text text-type value linkID)
                  (display "inside replace link text ")(newline)
                  (define text-type (list-ref action-sexpr 1)) ;(using-type (car args-lst)) (alt-text (cadr args-lst)))
                  (define text-value (list-ref action-sexpr 2))
                  (display "text type ")(display text-type)(newline)
                  (display "text value ")(display text-value)(newline)
                  
                  ;; 'text maps to "alternative text", 'fact maps to "string fact" 
                  (define text-type-string #f)
                  (cond ((equal? (cadr text-type) 'text)
                         (set! text-type-string "alternative text")
                         )
                        ((equal? (cadr text-type) 'fact)
                         (set! text-type-string "string fact")
                         ))
                  (add-specific-action "update text using" text-type-string text-value)
                  )
                 ((or (equal? 'retract (car action-sexpr))
                      (equal? 'assert (car action-sexpr))
                      (equal? 'set-value! (car action-sexpr))
                      )
                  ;(define factID (list-ref action-sexpr 1))
                  
                  ;(the-action-expr (ask action-obj 'expr))
                  (define the-action (car action-sexpr))
                  (define targetID (cadr action-sexpr)) ;; factID
                  (define the-value (if (eq? the-action 'set-value!)
                                 (caddr action-sexpr)
                                 'NA))
                  
                  (add-specific-action "update fact" the-action targetID the-value)
                  )
                 ((equal? 'add-anywhere-link (car action-sexpr))
                  (add-specific-action "enable links to this node from anywhere")
                  )
                 )
               
               ) actions)
        )))

;;
;;;; build the UI
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

(define editlink-panel-main-buttons #f)
(define editlink-panel-main-buttons-add #f) ;; add conditions
(define editlink-panel-main-buttons-delete #f) ;; delete conditions

(define editlink-panel-buttons #f)
(define editlink-panel-buttons-cancel #f)
(define editlink-panel-buttons-ok #f)

;(define editlink-panel-then #f)
;(define editlink-panel-then-link-message1 #f)
;(define editlink-panel-then-link-message1-panel #f)
;(define editlink-panel-then-text #f)
;(define editlink-panel-then-text-message1 #f)
;(define editlink-panel-then-text-message2 #f)
;(define editlink-panel-then-link #f)
;(define editlink-panel-then-link-check #f)
;(define editlink-panel-then-choicepanel #f)
;(define editlink-panel-then-link-choice #f)
;(define editlink-panel-then-facts-labelpanel #f)
;(define editlink-panel-then-facts-label #f)
;(define editlink-panel-then-facts #f)
;(define editlink-panel-then-buttons #f)
;(define editlink-panel-then-buttons-add #f)
;(define editlink-panel-then-buttons-delete #f)

;(define editlink-panel-else #f)
;(define editlink-panel-else-message1-panel #f)
;(define editlink-panel-else-message1 #f)
;(define editlink-panel-else-text #f)
;(define editlink-panel-else-text-check #f)
;(define editlink-panel-else-text-typechoice #f)
;(define editlink-panel-else-text-factchoice #f)
;(define editlink-panel-else-text-message1 #f)
;(define editlink-panel-else-link #f)
;(define editlink-panel-else-link-check #f)
;(define editlink-panel-else-choicepanel #f)
;(define editlink-panel-else-link-choice #f)



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
  (set! condition-list-panel (make-panel))
  (set-container-layout condition-list-panel 'vertical)
  (add-component editlink-panel-if condition-list-panel)

  ;; Add a horizontal panel to the dialog, with centering for buttons
  (set! editlink-panel-main-buttons (make-panel))
  (set-container-layout editlink-panel-main-buttons 'horizontal)
  (add-component editlink-panel-if editlink-panel-main-buttons)

  ;; Add ADD button
  (set! editlink-panel-main-buttons-add (make-button "Add condition"))
  (add-actionlistener editlink-panel-main-buttons-add
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-add-condition))))
  (add-component editlink-panel-main-buttons editlink-panel-main-buttons-add)

  ;; Add DELETE button
  (set! editlink-panel-main-buttons-delete (make-button "Delete selected"))
  (add-actionlistener editlink-panel-main-buttons-delete
                      (make-actionlistener (lambda (source)
                                             (editlink-dialog-delete-conditions))))
  (add-component editlink-panel-main-buttons editlink-panel-main-buttons-delete)
  ) ;; end of IF conditions

;; panel adding actions (init editlink-panel-then)
;; TODO: remove
;(define (create-then-action-panel)
;  ;; start of THEN action

;  ; panel to hold all the "then" information
;  (set! editlink-panel-then (make-panel))
;  (set-container-layout editlink-panel-then 'vertical)
;  
;  ;; add when we building the dialog
;  ;(add-component editlink-panel-top editlink-panel-then)

;  ;the THEN label
;  (set! editlink-panel-then-link-message1-panel (make-panel))
;  (set-container-layout editlink-panel-then-link-message1-panel 'flow 'left)
;  (set! editlink-panel-then-link-message1 (make-label))
;  (set-text editlink-panel-then-link-message1 "THEN") ;; was "THEN"
;  (add-component editlink-panel-then-link-message1-panel editlink-panel-then-link-message1)

;  ; panel to hold the "if" text
;  (set! editlink-panel-then-text (make-panel))
;  (set-container-layout editlink-panel-then-text 'flow 'left)

;  ; default link text label
;  ; TODO: fix layout when editing anywhere node rule
;  ; note: this is only problematic because I've hacked the dialog to resize for 
;  ; editing code, so this isn't worth fixing for now - alex
;  (set! editlink-panel-then-text-message1 (make-label))
;  (set-text editlink-panel-then-text-message1 "show default text: ")
;  (if (not (is-basic-mode?))
;      (add-component editlink-panel-then-text editlink-panel-then-text-message1))

;  ; default link text content
;  (set! editlink-panel-then-text-message2 (make-textfield "[default text...........]" 20))
;  (set-text-component editlink-panel-then-text-message2 #f #t)
;  ;(set-text editlink-panel-then-text-message2 "[default text...........]")
;  (add-component editlink-panel-then-text editlink-panel-then-text-message2)

;  ; panel to hold the "if" link
;  (set! editlink-panel-then-link (make-panel))
;  (set-container-layout editlink-panel-then-link 'horizontal)
;  ;(add-component editlink-panel-then editlink-panel-then-link)

;  ;; Add checkbox
;  (set! editlink-panel-then-link-check (make-checkbox "follow link to"))
;  (if (not (is-basic-mode?))
;      (begin
;        (add-itemlistener editlink-panel-then-link-check
;                          (make-itemlistener (lambda (source newstate)
;                                               (link-check-changed source newstate))))
;        (add-component editlink-panel-then-link editlink-panel-then-link-check))
;      (add-component editlink-panel-then-link (make-label-with-title "follow link to")))

;  ; panel to hold the destination choice
;  (set! editlink-panel-then-choicepanel (make-panel))
;  (set-container-layout editlink-panel-then-choicepanel 'horizontal)
;  (add-component editlink-panel-then-link editlink-panel-then-choicepanel)

;  ; choice for destination - created dynamically when rule editor is loaded
;  (set! editlink-panel-then-link-choice #f)

;  ;; end of THEN action
;  )

;;;; rule edit UI
(define actions-main-panel #f)
(define action-list-panel #f) 
(define action-type-choice #f)

(define condition-list-panel #f)
(define editlink-dialog-negate-operator #f) ;; If/Unless combobox
(define editlink-dialog-andor-operator #f) ;; Any/All combobox
(define editlink-dialog-message3 #f) ;; "of the following conditions are true:" message label

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

;; add an action to the action list of type action-type ["update text using", "follow link to", "update fact"]
;; args-lst null means we're adding a new empty action panel,
;; if args-lst not null load the information provided (present existing action) 
(define (add-specific-action action-type . args-lst)
  (display "[add-specific-action] ")(newline)
  (display "  action type ")(display action-type)(newline)
  (display "  args-lst ")(display args-lst)(newline)
  
  (define panel-to-return (create-action-panel action-type))
  (add-component action-list-panel panel-to-return)
  
  (display "selection? ")(display (get-combobox-selecteditem action-type-choice))(newline)
  
  ;; alter the configuration of the ui objects if args-lst given
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
         (add-component panel-to-return update-text-action-panel)
         )
        ((equal? action-type "follow link to")
         (display "args lst in follow link to ")(display args-lst)(newline)
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
         (add-component panel-to-return node-choice-combobox)
         )
        ((equal? action-type "update fact")
         (if (= (length args-lst) 3)
             (let ((the-action (car args-lst))
                   (targetID (cadr args-lst))
                   (the-value (caddr args-lst)))
               (add-component panel-to-return (create-fact-panel the-action targetID the-value))
               )
             (add-component panel-to-return (create-fact-panel #f #f #f)))
         )
        ((equal? action-type "enable links to this node from anywhere")
         ;; disable the checkbox of this action (so it can't be deleted)
         (set-component-enabled (car (get-container-children panel-to-return)) #f)
         )
        )
  
  panel-to-return)

;; called when add action button is pressed
(define (add-action-callback source)
  (display "add action! ")(newline)
  
    ;; get the selected type
  (define selected-action-type (get-combobox-selecteditem action-type-choice))
  (define new-action-panel (add-specific-action selected-action-type)) 
  
  (add-component action-list-panel new-action-panel)
  (pack-frame editlink-dialog)
  )

;; called when delete action button is pressed
;; TODO: does not remove the action at all from the rule object
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
  (set! action-type-choice (make-combobox))
  
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
(define action-type-list-link (list "update text using" "follow link to" "update fact"))
(define action-type-list-node (list "update fact"))
(define-constant unique-choices (list "update text using" "follow link to")) ;; choices that shouldnt be duplicated

(define node-choice-combobox #f)

;; an instance of the action type selector panel
;; updates the combobox and return a panel with just a checkbox and label
(define (create-action-panel action-type) ; the-type )
  (if (not (eq? action-type #!null))
      (let* ((top-panel (make-panel))
             (the-checkbox (make-checkbox "")))
                                        ; add top-panel
        (set-container-layout top-panel 'flow 'left)

                                        ;(add-component condition-list-panel top-panel)

        ;; Add checkbox
        (add-component top-panel the-checkbox)

        ;; add the action label display
        (define action-label (make-label-with-title (to-string action-type)))
        (add-component top-panel action-label)

        (define action-type-str (to-string action-type))
                                        ;"update text using" "follow link to" "update fact"))

        ;; update action-type-choice combobox
        ;; ensure actions that should only have one instance is not available as a choice in the combobox
        (if (member (to-string action-type) unique-choices)
            (remove-combobox-string action-type-choice action-type))

        top-panel)
      (make-panel)))

;;  ====================
;;;; Update text panels
;;  ====================

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
           
           ;; create and add the combobox containing the string facts
           (set! fact-string-choice-combobox (create-fact-choice 'string #f))
           
           ;; add to a container panel and add to the update-text-action-panel
           (add-component update-text-fact-selection-panel fact-string-choice-combobox)
           (add-component update-text-action-panel update-text-fact-selection-panel)
           
           ;(add-component update-text-action-panel fact-string-choice-combobox)
           ))
    ;(set-component-visible editlink-dialog #t)
  (pack-frame editlink-dialog)
    )

;; the panel that comes behind the combobox selecting actions type
(define (create-update-text-action-panel)
  (set! update-text-action-panel (make-panel))
  (set! action-type-combobox (make-combobox "alternative text" "string fact"))
  (set! alt-text-textfield (make-textfield "" 20))
  
  (set! update-text-fact-selection-panel (make-panel))
  
  (set! fact-string-choice-combobox ;editlink-panel-else-text-factchoice 
    (create-fact-choice 'string
                        -1))
  
  (set! fact-boolean-choice-combobox ;editlink-panel-else-text-factchoice 
    (create-fact-choice 'boolean
                        -1))
  
  (add-component update-text-action-panel action-type-combobox)
  (add-component update-text-action-panel alt-text-textfield)
  
  ;(set! update-text-combobox-callback (lambda (
  
  (add-actionlistener action-type-combobox 
                      (make-actionlistener 
                       (lambda (e)
                         (action-update-text-combobox-callback))))
  )

;;;; create-editlink
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
  
  ; pack
  (pack-frame editlink-dialog))

;;;; editnode UI

(define editnode-dialog #f)
(define editnode-panel-then-facts #f)
(define editnode-panel-top #f)
(define editnode-panel-then #f)
(define editnode-dialog-parent #f)
(define editnode-dialog-pane #f)
(define editnode-panel-then-facts-labelpanel #f)
(define editnode-panel-then-buttons #f)

;; TODO: remove
;(define (create-facts-main-panel)
;  ; panel for facts label
;  (set! editnode-panel-then-facts-labelpanel (make-panel))
;  (set-container-layout editnode-panel-then-facts-labelpanel 'flow 'left)
;  ;(add-component editnode-panel-then editnode-panel-then-facts-labelpanel)

;  ;; facts label
;  (define editnode-panel-then-facts-label (make-label))
;  (set-text editnode-panel-then-facts-label "when visited, update the following facts:")
;  (add-component editnode-panel-then-facts-labelpanel editnode-panel-then-facts-label)
;  
;  ;; facts panel
;  (set! editnode-panel-then-facts (make-panel))
;  
;  (set-container-layout editnode-panel-then-facts 'vertical)
;  
;  ;; Add a horizontal panel to the dialog, with centering for buttons
;  (set! editnode-panel-then-buttons (make-panel))
;  (set-container-layout editnode-panel-then-buttons 'horizontal)
;  
;  ;; Add ADD button
;  (define editnode-panel-then-buttons-add (make-button "Add fact"))
;  (add-actionlistener editnode-panel-then-buttons-add
;                      (make-actionlistener (lambda (source)
;                                             (editlink-dialog-add-fact))))
;  (add-component editnode-panel-then-buttons editnode-panel-then-buttons-add)

;  ;; Add DELETE button
;  (define editnode-panel-then-buttons-delete (make-button "Delete selected"))
;  (add-actionlistener editnode-panel-then-buttons-delete
;                      (make-actionlistener (lambda (source)
;                                             (editlink-dialog-delete-facts))))
;  (add-component editnode-panel-then-buttons editnode-panel-then-buttons-delete)
;  )

;; not used anymore
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
    (add-component condition-list-panel top-panel)
    
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
;;;; callbacks from UI
;;

; set "ok" button state - cannot allow a "use" checkbox to be checked and a link to be "none"
;; TODO : outdated (used to do the above, now it should check 
;;        for new conditions where we don't want them to hit the ok button)
(define (update-ok-button-state)
  #f
  )

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
(define (editlink-dialog-add-condition)
  (create-condition-panel 0 -1 0 -1)
  (pack-frame editlink-dialog))

; delete selected condition
(define (editlink-dialog-delete-conditions)
  (editlink-dialog-delete condition-list-panel))

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
;; Note: it seems we don't recycle condition, rule and action objects
;;       perhaps a TODO might be to check whether the objects exists. (might be too troublesome)
;;       also with abstraction I can see how one condition, action, rule can be assigned to many places
;;       but it create some programming problem because each of these rule object have different parents
;;       and condition and action have reference to a parent ruleID

;; TODO: Recycle objects
;;       Rule: one solution is to just set the condition and action list in rules so that it is still the same object
;;       Action: since the 'expr is just a sexpr we can just set it 
;;       Condition : mechanics need changing since it stores the information in different variable (perhaps switch to sexpr) 
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
         (all-conditions (get-container-children condition-list-panel))
         
         ; get actions
         (actions (action-panel-list))
         
         (negate-pos (get-combobox-selectedindex editlink-dialog-negate-operator))
         (negate? (case negate-pos
                    ((0) #f)
                    ((1) #t)))
         ; create the rule
         (new-ruleID (create-typed-rule2 new-rulename edit-mode new-ruleexpression negate?
                                       (cond ((eq? edit-mode 'link) edited-linkID)
                                             ((eq? edit-mode 'node) edited-nodeID)
                                             ((eq? edit-mode 'doc) -1))))
         
         ; get the actions, if any
         ;(then-action-string (get-text editlink-dialog-then-actiontext))
         ;(else-action-string (get-text editlink-dialog-else-actiontext))
         )
    
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
    
    ;; run through all the actions and add to rule
    
    
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

    ; if we're editing a link, then retrieve the link details
    (cond ((eq? edit-mode 'link)
                                        ; update use-alt, alt-destination and alt-text
           (define the-link the-rule-object)
           (define link-name (ask the-rule-object 'name))
           
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
           
           ;; an action panel always has a checkbox as first component then a label with the name
           ;; which identifies which kind of action it represents
           (define (get-action-panel-type action-panel)
             (define children (get-container-children action-panel))
             (define action-label (cadr children))
             (get-text action-label))
           
           ;; for processing "update text using" action-panel
;           (define (get-update-text-using-type uts-action-panel)
;             (define children (get-container-children uts-action-panel))
;             
;             )
           
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
                            ;(set! text-value (to-string factID))
                            (set! text-value factID)
                            )
                           )
                         
                         (display "SETTING ACTION ")(newline)
                         (display "text type ")(display text-type)(newline)
                         (display "text value ")(display text-value)(newline)
                         
                         ;(replace-link-text text-type value linkID)
;                         (ask (get 'rules new-ruleID) 'add-action! 
;                              (string-append "(replace-link-text " 
;                                             text-type " "
;                                             text-value " "
;                                             (to-string edited-linkID) ")"))
                         (create-action link-name 'displayed-node
                                        (list 'replace-link-text
                                              (list 'quote 'text)
                                              text-value ;; this is actually factID
                                              edited-linkID)
                                        new-ruleID)
                         )
                        ((equal? action-type "follow link to")
                         ;(display "I see FOLLOW LINK ")(newline)
                         ;node-choice-combobox
                         (define dest-nodeID (get-comboboxwithdata-selecteddata node-choice-combobox))
                         ;(follow-link2 linkID parent-ruleID link-type dest-nodeID)
;                         (ask (get 'rules new-ruleID) 'add-action! 
;                              (string-append "(follow-link "
;                                              (to-string edited-linkID) " "
;                                              (to-string new-ruleID) " "
;                                              "(quote default)" 
;                                              (to-string dest-nodeID) ")"))
                         (create-action link-name 'clicked-link
                                        (list 'follow-link
                                              edited-linkID
                                              new-ruleID
                                              (list 'quote 'default)
                                              dest-nodeID)
                                        new-ruleID
                                        )
                         )
                        ((equal? action-type "update fact")
                         (display "I see UPDATE FACT ")(newline)
                         
                         (define fact-panel-children (get-container-children action-panel))
                         ;; the components that follows the update fact label in the action panel
                         (define component-list (get-container-children (caddr fact-panel-children)))
                         
                         (define dd1 (car component-list))
                         (define dd2 (cadr component-list))
                         ;; component 3 can be a textfield or dropdown depending on what is selected in dd2
                         (define comp3 (caddr component-list))
                         
                         (define fact-type 
                           (case (to-string (get-combobox-selecteditem dd1))
                             (("True/False") 'boolean)
                             (("Text") 'text)
                             (else 'error)))
                         
                         (define factID (get-comboboxwithdata-selecteddata dd2))
                         (display "factID ")(display factID)(newline)
                         
                         (cond ((equal? fact-type 'boolean)
                                (define bool-val-selected (to-string (get-combobox-selecteditem comp3)))

                                (define bool-operator
                                  (cond ((equal? bool-val-selected "True") 'assert)
                                        ((equal? bool-val-selected "True") 'retract)))

                                (create-action link-name 'clicked-link
                                               (list bool-operator
                                                     factID)
                                               new-ruleID
                                               ))
                               ((equal? fact-type 'text)
                                (create-action link-name 'clicked-link
                                               (list 'set-value!
                                                     factID
                                                     (get-text comp3))
                                               new-ruleID)
                                )) ;; end of fact-type cond
                         )) ;; end of action-type cond
                  ) (get-container-children action-list-panel))
           
           ;; now we replace the old rule with a completely new one
           ;; clean up old rule ID in manager and in object
           ;; create-typed-rule2 has already added new rule to the-link
           ;; TODO: if it was the first rule attached to this link, it will no longer be in that order
           ;;       need to remove the implicit add rule from within create-typed-rule2
           ;;       and implement a extra replace rule command for links and node objects
           (ask the-link 'remove-rule edited-ruleID)
           (del 'rules edited-ruleID)
           
           ;; no need to remove it rmgr-rule-lst
           ;(define old-ruleID-index (list-index (lambda (obj) (= edited-ruleID obj)) (rmgr-rule-lst)))
           ;(set! rmgr-rule-lst (list-replace rmgr-rule-lst old-ruleID-index new-ruleID))
           ;(rmgr-set-rule-lst (list-replace (rmgr-rule-lst) old-ruleID-index new-ruleID))
           
           ;; now we remove the old action ID
           ;(create-action name type expr ruleID . args)

;           (ask the-link 'set-use-destination! uselink)
;           (ask the-link 'set-destination! destID)
;           (ask the-link 'set-use-alt-destination! usealtlink)
;           (ask the-link 'set-alt-destination! altdestID)
           ;; if using alt text, and alttext-fact is true, then set use-alt-text to 'fact,
           ;; otherwise just set to usealttext
;           (ask the-link 'set-use-alt-text!
;                (if (and usealttext
;                         alttext-fact)
;                    'fact
;                    usealttext))
           ;(format #t "*** use-alt-text: ~a ***~%~!" (ask the-link 'use-alt-text))

           ;; if alttext-fact is true, store the factID in alt text otherwise store text
;           (if alttext-fact
;               (ask the-link 'set-alt-text! (get-comboboxwithdata-selecteddata editlink-panel-else-text-factchoice))
;               (ask the-link 'set-alt-text! alttext))

           ;; update links in graph if callback provided
           ;; note: if uselink/usealtlink is false, then pass -1 ie. don't draw line
           ;; similar for old links
           ;; TODO: get the information from the action needed to update the link drawing 
;           (if update-callback
;               (update-callback new-rulename edited-nodeID
;                                olduselink olddestID
;                                oldusealtlink oldaltdestID
;                                uselink destID
;                                usealtlink altdestID
;                                edited-linkID))

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
         ;(all-conditions (get-container-children condition-list-panel))
         
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
                                 (cond ((= the-type 0) ;; boolean fact
                                        (list (if (= the-value 0)
                                                  'assert
                                                  'retract)
                                              targetID))
                                       ((= the-type 1)
                                        (list 'set-value! targetID (string-append "\"" the-value "\"")))
                                       (else '()))
                                 ))
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
  
; reset the rule editor
(define (reset-rule-editor)
  
  (clear-container condition-list-panel) ;; empty conditions panel
  (clear-container action-list-panel)         ;; empty actions panel

  ; if facts are showing, reset the alt text to text, not fact
;  (if (show-facts?)
;      (set-combobox-selection editlink-panel-else-text-typechoice 0))

  (set-combobox-selection editlink-dialog-andor-operator 0))

; delete selected entries in a container, used by conditions and facts
;; TODO take this out as misc or replace with a suitable procedure from container
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
;;;; fact panel
;; 

; create an fact panel
; the-action: the type of action 'assert, 'retract or 'set-value!
; targetID: the currently selected fact, if any (pass in -1 if none selected)
; the-value: value that will be set (for 'set-value! only)
;; fact type is only needed if a factID is selected

(define (create-fact-panel fact-type factID the-value)
  
  (let* ((top-panel (make-panel))
         ;(the-checkbox (make-checkbox ""))
         (the-type-choice (make-combobox "True/False" "Text"))
         (the-boolean-choice (make-combobox "True" "False"))
         (the-string-entry (make-textfield "[the fact text...........]" 20))
         (the-fact-list-boolean (create-fact-choice 'boolean factID))
         (the-fact-list-string (create-fact-choice 'string factID)))
    
    ; add top-panel
    (set-container-layout top-panel 'horizontal)
    
    ;; add type choice
    (add-component top-panel the-type-choice)
  
    ;; choose the fact type index
    (define (fact-type-index)
      (cond ((or (equal? 'assert fact-type)
                 (equal? 'retract fact-type)) 0)
            ((equal? 'set-value! fact-type) 1)
            (else 0)
            ))
    (display "fact type index ")(display fact-type)(newline)
    
    ; set type choice
    (set-combobox-selection the-type-choice (fact-type-index))
    
    ;; add the appropriate components
    (set-fact-panel-components top-panel (fact-type-index)
                               the-fact-list-string the-string-entry
                               the-fact-list-boolean the-boolean-choice)
    
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