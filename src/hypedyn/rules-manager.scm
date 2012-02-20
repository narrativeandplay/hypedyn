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

(require "../kawa/ui/component.scm")
(require "../kawa/ui/container.scm")
(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/menu.scm") ;; make-menu, make-menu-bar
(require "../kawa/ui/label.scm") ;; make-label-with-title
(require "../kawa/ui/button.scm") ;; make-button
(require "../kawa/ui/events.scm") ;; add-actionlistener
(require "../kawa/ui/checkbox.scm") ;; make-checkbox
(require "../kawa/ui/frame.scm") ;; pack-frame 
(require "../kawa/ui/undo.scm") ;; compoundundomanager-postedit

(require "../common/datatable.scm") ;; get
(require "../common/objects.scm") ;; ask

(require "nodeeditor.scm") ;get-nodeeditor-frame
(require "config-options.scm") ;; is-undo-enabled?
(require "hypedyn-undo.scm") ;; undo-action
(require "editlink.scm") ;; doeditlink, set-edit-mode
(require "datastructure.scm") ;; create-typed-rule2
(require 'srfi-1) ;; remove

(module-export rmgr-init
               rmgr-edit
               rmgr-close)

;;  ===============
;;;; rule manager
;;  ===============

;; rmgr stands for rule manager
(define rules-manager-main-dialog #f)
(define rmgr-rules-list-panel #f) ;; the panel that contain all the rule panels

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
                                
                                (doeditlink selected-linkID edited-nodeID ruleID)
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

  ;; set fall through or not for this rule 
  (define (set-fall value :: <boolean>)
    (if value
        (begin
          (ask rule-obj 'set-fall-through? #t)
          (set-button-label fall-through-button "Fall")
          )
        (begin
          (ask rule-obj 'set-fall-through? #f)
          (set-button-label fall-through-button "Stop")
          ))
    
    ;; pop the rules manager out again
    (set-component-visible rules-manager-main-dialog #t)
    
    ;; if this was a link then bring up the nodeeditor and 
    ;; set the edited-linkID and edited-nodeID
    
    ;; if this was a node then bring up the nodeeditor and 
    ;; set the edited-nodeID
    (define parentID (ask rule-obj 'parentID))
    (case edit-mode
      ((link) 
       (set! edited-linkID parentID)
       (define parent-nodeID (ask (get 'links edited-linkID) 'source))
       (nodeeditor-edit parent-nodeID)  ;; pop nodeeditor out again
       )
      ((node)
       (nodeeditor-edit parentID) ;; pop nodeeditor out again
       )
      )
    
    ;; right most button of dialog box is getting pushed out
    (pack-frame rules-manager-main-dialog)
    )
  
  (define (fall-through-button-callback e)
    ;; alternate button display between "Fall" and "Stop"
    (if (equal? (get-button-label fall-through-button) "Fall")
        (begin
          (set-fall #f)
          (compoundundomanager-postedit
           undo-manager
           (make-undoable-edit "Set Stop"
                               (lambda () ;; undo
                                 (display "STOP undo")(newline)
                                 (set-fall #t))
                               (lambda () ;; redo
                                 (display "STOP redo")(newline)
                                 (set-fall #f)))))
        (begin
          (set-fall #t)
          (compoundundomanager-postedit
           undo-manager
           (make-undoable-edit "Set Fall"
                               (lambda () ;; undo
                                 (display "FALL undo")(newline)
                                 (set-fall #f))
                               (lambda () ;; redo
                                 (display "FALL redo")(newline)
                                 (set-fall #t))))
          )))
  
  (add-actionlistener fall-through-button 
                      (make-actionlistener
                       fall-through-button-callback))
  
  (define rule-fall-through? (ask rule-obj 'fall-through?))
  (if (not rule-fall-through?)
      (set-button-label fall-through-button "Fall"))
  
  ;; to be side by side
  (define (swap-first-two lst)
    (list-insert (cdr lst) (car lst) 1))
  
  ;; swap object at index with the object right of it
  (define (swap-right lst index)
    (append (take lst index) ;; take first (index) number of object in front (left) 
            (swap-first-two (drop lst index)))
    ) ;; if already left most then do nothing
  
  ;; the same thing just a positional difference
  (define (swap-left lst index)
    (if (>= index 0)
        (swap-right lst (- index 1))
        lst))
  
  ;; shift this rule panel up, shift the rule as well
  (define (shift-rule-up)
    (display "UP ")(newline)
    (define position (list-index (lambda (o) (equal? o ruleID)) (rmgr-rule-lst)))

    (shift-panel rmgr-rules-list-panel position (- position 1))

    ;; update rule position in object's rule-lst
    (rmgr-set-rule-lst (swap-left (rmgr-rule-lst) position))

    ;; need to pack-frame for update?
    ;(pack-frame rules-manager-main-dialog)
    (validate-container rules-manager-main-dialog)
    )
  
  ;; shift this rule panel down, shift the rule as well
  (define (shift-rule-down)
    (display "DOWN ")(newline)
    (define position (list-index (lambda (o) (equal? o ruleID)) (rmgr-rule-lst)))

    (shift-panel rmgr-rules-list-panel position (+ position 1))

    ;; update rule position in object's rule-lst
    (rmgr-set-rule-lst (swap-right (rmgr-rule-lst) position))

    ;; need to pack-frame for update?
    ;(pack-frame rules-manager-main-dialog)
    (validate-container rules-manager-main-dialog))
  
  (add-actionlistener
   shift-up-button
   (make-actionlistener
    (lambda (e)
      (shift-rule-up)
      (compoundundomanager-postedit
       undo-manager
       (make-undoable-edit "Shift Rule Up"
                           (lambda () ;; undo
                             (shift-rule-down))
                           (lambda () ;; undo
                             (shift-rule-up))))
      )))
  
  (add-actionlistener 
   shift-down-button
   (make-actionlistener
    (lambda (e)
      (shift-rule-down)
      (compoundundomanager-postedit
       undo-manager
       (make-undoable-edit "Shift Rule Down"
                           (lambda () ;; undo
                             (shift-rule-up))
                           (lambda () ;; undo
                             (shift-rule-down))))
      )))
  
  (add-component top-panel rule-checkbox)
  (add-component top-panel rule-name-label)
  
  (add-component top-panel rule-edit-button)
  (add-component top-panel fall-through-button)
  (add-component top-panel shift-up-button)
  (add-component top-panel shift-down-button)
  
  ;; make rule panel
  top-panel)

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
      ((link) (get 'links edited-linkID))
      ((node) (get 'nodes edited-nodeID))))
  
  (define deleted-ID-lst '())
  
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
               ;(ask rule-parent 'remove-rule ruleID)
               (set! deleted-ID-lst (append deleted-ID-lst (list ruleID)))
               ))
         ) rule-panel-lst (rmgr-rule-lst))
  
  ;; return a list of the deleted ID
  deleted-ID-lst)

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
(define (rmgr-init)
  (set! rules-manager-main-dialog (make-dialog (get-nodeeditor-frame) "Rule Editor" #f)) ;; debug used to be #t
  
  (define rules-manager-main-panel (make-panel))
  (set-container-layout rules-manager-main-panel 'border)
  (add-component rules-manager-main-dialog rules-manager-main-panel)
  
  ;; adding menu bar just to activate undo in this dialog box
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
  (add-component rules-dialog-button-panel rules-dialog-close)
  
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
                         (define deleted-ID-lst (remove-selected-rule-panel))
                         
                         ;; post undo
                         (compoundundomanager-postedit
                          undo-manager
                          (make-undoable-edit "Delete Rule"
                                              (lambda () ;; undo
                                                ;(rmgr-remove-rule new-rule-ID)
                                                (map (lambda (ruleID)
                                                       (add-rule-panel ruleID)
                                                       (rmgr-set-rule-lst (append (rmgr-rule-lst) (list ruleID)))
                                                       ) deleted-ID-lst)
                                                )
                                              (lambda () ;; redo
                                                ;(add-rule-panel new-rule-ID)
                                                ;(rmgr-set-rule-lst (append (rmgr-rule-lst) (list new-rule-ID)))
                                                ;(ask (rmgr-get-currently-edited) 'set-rule-lst rmgr-rule-lst)
                                                (map (lambda (ruleID)
                                                       (rmgr-remove-rule ruleID)
                                                       ) deleted-ID-lst)
                                                )))
                         )))
  
  (add-actionlistener rules-dialog-close
                      (make-actionlistener
                       (lambda (e)
                         (set-component-visible rules-manager-main-dialog #f)
                         )))
  
  ;(populate-rules-manager target-type obj-ID)
  ;(set-component-visible rules-manager-main-dialog #t)
  )

(define (populate-rules-manager target-type obj-ID)
  
  (clear-container rmgr-rules-list-panel)
  
  (cond ((equal? target-type 'link)
         (set! edited-linkID obj-ID)
         (display "edited-linkID set ")(display obj-ID)(newline)
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
         ))
  
  (pack-frame rules-manager-main-dialog))

(define (rmgr-edit target-type obj-ID)
  (set! edit-mode target-type)
  (display "[SETTING editmode] ")(display target-type)(newline)
  (populate-rules-manager target-type obj-ID)
  (set-component-visible rules-manager-main-dialog #t)
  (display "edit mode here ")(display target-type)(newline)
  )

(define (rmgr-close)
  (if rules-manager-main-dialog
      (set-component-visible rules-manager-main-dialog #f)))
