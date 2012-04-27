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
(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/menu.scm") ;; make-menu, make-menu-bar
(require "../kawa/ui/label.scm") ;; make-label-with-title
(require "../kawa/ui/button.scm") ;; make-button
(require "../kawa/ui/events.scm") ;; add-actionlistener
(require "../kawa/ui/checkbox.scm") ;; make-checkbox
(require "../kawa/ui/frame.scm") ;; pack-frame 
(require "../kawa/ui/undo.scm") ;; compoundundomanager-postedit
(require "../kawa/strings.scm") ;; to-string

(require "../common/datatable.scm") ;; get
(require "../common/objects.scm") ;; ask
(require "../common/list-helpers.scm") ;; swap-left swap-right  
 
(require "nodeeditor.scm") ;get-nodeeditor-frame
(require "config-options.scm") ;; is-undo-enabled?
(require "hypedyn-undo.scm") ;; undo-action, cache-rule
(require "editlink.scm") ;; doeditlink, set-edit-mode
(require "datastructure.scm") ;; create-typed-rule2
(require 'srfi-1) ;; remove


(module-export rmgr-init
               rmgr-edit
               rmgr-close)
;; rmgr stands for rule manager
(define rules-manager-main-dialog #f)
(define rmgr-rules-list-panel #f) ;; the panel that contain all the rule panels


;;;; helper functions
;; NOTE: rmgr-get-currently-edited depends on edited-linkID/edited-nodeID and edit-mode being correctly set
;;       so make sure rmgr-edit is called to update it

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

(define (get-rule-panel-for ruleID)
  (display "get-rule-panel ")(display ruleID)(newline)
  (display "rmrg-rule-lst ")(display (rmgr-rule-lst))(newline)
  (define index (list-index (lambda (rid) (= rid ruleID)) (rmgr-rule-lst)))
  (if index
      (list-ref (get-container-children rmgr-rules-list-panel) index)
      #f))

;;;; components of rule-panel 
;(define (rule-panel-checkbox rule-panel)
;  (list-ref (get-container-children rule-panel) 0))
(define (rule-panel-fall-button rule-panel)
  (list-ref (get-container-children rule-panel) 3))

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
    ((doc) -1)))

;;;; make-rule-panel
;; one instance of the rule panel (one entry on the rule list)
;; only called by rmgr-add-rule-panel which makes sure we are passed a valid ruleID (gotten from actual rule-lst)
(define (make-rule-panel ruleID)
  
  ;; make components
  (define top-panel (make-panel))
  (define rule-checkbox (make-checkbox ""))
  (define rule-name-label #f)
  ;; make buttons
  (define rule-edit-button (make-button "Edit Rule"))
  (define fall-through-button (make-button "Fall"))
  (define shift-up-button (make-button "Up"))
  (define shift-down-button (make-button "Down"))
  
  ;; TODO: Make a rename button or have some method to rename the rule.
  ;;       Be sure to update rule-panel-fall-button to use the correct position
  ;;       if you add new components to the rule panel.

  (set-container-layout top-panel 'flow 'left)
  
  ;; assume ruleID valid
  (define rule-obj (get 'rules ruleID))
  
  (if rule-obj
      (let ((rule-name (ask rule-obj 'name))
            (rule-fall-through? (ask rule-obj 'fall-through?)))
        
        ;; TODO: this should refresh when show id just got selected
        (if (show-IDs?)
            (set! rule-name (to-string (string-append rule-name "(" (to-string (ask rule-obj 'ID)) ")"))))
        
        (set! rule-name-label (make-label-with-title rule-name))
  
        ;; set initial state of the fall-through-button (its label)
        (if rule-fall-through?
            (set-button-label fall-through-button "Fall")
            (set-button-label fall-through-button "Stop")))
      (begin
        (display "ERROR make-rule-panel given invalid ruleID")(newline)))
  
  ;; buttons listeners
  (add-actionlistener 
   rule-edit-button
   (make-actionlistener
    (edit-rule-button-callback ruleID)))

  (add-actionlistener 
   fall-through-button
   (make-actionlistener
    (fall-through-button-callback ruleID)))
  
  (add-actionlistener
   shift-up-button
   (make-actionlistener
    (shift-rule-button-callback ruleID 'up)))
  
  (add-actionlistener 
   shift-down-button
   (make-actionlistener
    (shift-rule-button-callback ruleID 'down)))
  
  (add-component top-panel rule-checkbox)
  (add-component top-panel rule-name-label)
  
  (add-component top-panel rule-edit-button)
  (add-component top-panel fall-through-button)
  (add-component top-panel shift-up-button)
  (add-component top-panel shift-down-button)
  
  ;; make rule panel
  top-panel)

;;;; add/remove rule panel

;; add rule (makes a rule panel inside rmgr-rules-list-panel)
;; when pos is added it adds the panel and the ruleID in that particular position
(define (rmgr-add-rule-panel ruleID)
  (if (and rmgr-rules-list-panel
           rules-manager-main-dialog)
      (begin
        (add-component rmgr-rules-list-panel (make-rule-panel ruleID))
        (pack-frame rules-manager-main-dialog))
      (begin
        (display "ERROR: in rmgr-add-rule-panel")(newline))))

;; go through all the rule panels in rmgr-rules-list-panel
;; remove the panels with checkbox checked
(define (remove-selected-rule-panel)
  (define rule-panel-lst (get-container-children rmgr-rules-list-panel))
  
  (define rule-parent
    (case edit-mode
      ;; note: link and node are read as 'link 'node in the special case syntax 
      ((link) (get 'links edited-linkID))
      ((node) (get 'nodes edited-nodeID))))
  
  (define deleted-ID-lst '())
  
  (map (lambda (rule-panel ruleID)
         (define component-lst (get-container-children rule-panel))
         (define this-checkbox (car component-lst))

         ;; if checkbox checked remove
         (if (get-checkbox-value this-checkbox)
             (begin
               (remove-component rmgr-rules-list-panel rule-panel)
               (pack-frame rules-manager-main-dialog)
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
;; NOTE: relies on the rmgr-rule-lst to tell us where 
;;       is the panel, so important to do this first before 
;;       removing ruleID from the rule-lst
(define (rmgr-remove-rule-panel ruleID)
  (define index (list-index (lambda (rid) (= rid ruleID)) (rmgr-rule-lst)))    ;; get the index in rmgr-rule-lst and rmgr-rules-list-panel
  (display "remove rule panel ")(display index )(newline)
  ;; remove corresponding panel
  (define comp-lst (get-container-children rmgr-rules-list-panel))
  (define panel-to-remove (list-ref comp-lst index))
  (remove-component rmgr-rules-list-panel panel-to-remove)
  (pack-frame rules-manager-main-dialog)
  )

;;;; button callback
(define (add-rule-button-callback e)
  (define new-rule-ID (create-typed-rule2 "new rule" edit-mode 'and #f
                                          (rmgr-get-currently-edited-ID)))
  (rmgr-add-rule-panel new-rule-ID)
  (define new-rule-sexpr (cache-rule new-rule-ID))

  ;; cache these edited obj ID and edit-mode
  (define edited-obj-ID (rmgr-get-currently-edited-ID))
  (define curr-edit-mode edit-mode)

  ;; post undo
  (compoundundomanager-postedit
   undo-manager
   (make-undoable-edit
    "Add Rule"
    (lambda () ;; undo
      ;; pops up rmgr and make sure we're editing that obj's 
      (rmgr-edit curr-edit-mode edited-obj-ID)
      (rmgr-remove-rule-panel new-rule-ID)
      (rmgr-set-rule-lst (remove (lambda (rid) (= rid new-rule-ID)) (rmgr-rule-lst)))
      (del 'rules new-rule-ID)
      )
    (lambda () ;; redo
      (rmgr-edit curr-edit-mode edited-obj-ID)
      (eval-sexpr new-rule-sexpr)                                                     ;; recreate the new rule 
      (rmgr-add-rule-panel new-rule-ID)
      ))))

(define (delete-selected-rule-button-callback e)
  ;; because create-typed-rule2 adds the ruleID to the obj 
  ;; it messes up the previous order, so cache it and set it back later
  (define rule-lst-before-deletion (list-copy (rmgr-rule-lst)))
  (define deleted-ID-lst (remove-selected-rule-panel))

  (if (not (null? deleted-ID-lst))
      (begin
        ;; cache these edited obj ID and edit-mode
        (define edited-obj-ID (rmgr-get-currently-edited-ID))
        (define curr-edit-mode edit-mode) ;; cache the value of edit-mode at this moment

        (define deleted-sexpr-lst
          (map cache-rule deleted-ID-lst))

        (map (lambda (ruleID)
               ;; update rule-lst
               (rmgr-set-rule-lst (remove (lambda (thisruleID) (= ruleID thisruleID)) (rmgr-rule-lst)))
               ;; delete from 'rules 
               (del 'rules ruleID)
               ) deleted-ID-lst)

        ;; post undo
        (compoundundomanager-postedit
         undo-manager
         (make-undoable-edit
          "Delete Rule"
          (lambda () ;; undo

            (rmgr-edit curr-edit-mode edited-obj-ID)

            ;; restore the rules
            (map (lambda (rule-sexpr)
                   (eval-sexpr rule-sexpr)
                   ) deleted-sexpr-lst)

            ;; make sure the order of rule-lst is the same as before deletion
            (rmgr-set-rule-lst rule-lst-before-deletion)

            ;; reorder the panels according to the rule-lst
            (populate-rules-manager curr-edit-mode edited-obj-ID)
            )
          (lambda () ;; redo
            (rmgr-edit curr-edit-mode edited-obj-ID)

            (display "deleted-ID-lst ")(display deleted-ID-lst)(newline)
            ;; remove panel first (important to do before remove rule-lst)
            (map (lambda (ruleID)
                   (rmgr-remove-rule-panel ruleID)
                   (rmgr-set-rule-lst (remove (lambda (thisruleID) (= ruleID thisruleID)) (rmgr-rule-lst)))
                   ) deleted-ID-lst)
            ;; remove from rule-lst and 'rules 
            (map (lambda (ruleID)
                   ;; delete from 'rules 
                   (del 'rules ruleID)
                   ) deleted-ID-lst)
            ))))))

;;;; rule swapping 

;; returns a callback lambda for the use of the button
;; because ruleID is a parameter that differs for every button
;; up-or-down ('up 'down) depending on whether its a shift up or shift down button 
(define (shift-rule-button-callback ruleID up-or-down)
    ;; shift this rule panel up, shift the rule as well
  (define (shift-rule-up)
    
    (display "[shift up]")(newline)
    (display "ruleID ")(display ruleID)(newline)
    (display "rmgr-rule-lst ")(display (rmgr-rule-lst))(newline)
    
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
    
    (display "[shift up]")(newline)
    (display "ruleID ")(display ruleID)(newline)
    (display "rmgr-rule-lst ")(display (rmgr-rule-lst))(newline)
    
    (define position (list-index (lambda (o) (equal? o ruleID)) (rmgr-rule-lst)))

    (shift-panel rmgr-rules-list-panel position (+ position 1))

    ;; update rule position in object's rule-lst
    (rmgr-set-rule-lst (swap-right (rmgr-rule-lst) position))

    ;; need to pack-frame for update?
    ;(pack-frame rules-manager-main-dialog)
    (validate-container rules-manager-main-dialog))
  
  (define (shift-up-callback e)
    (shift-rule-up)
    ;; cache these edited obj ID and edit-mode
    (define edited-obj-ID (rmgr-get-currently-edited-ID))
    (define curr-edit-mode edit-mode) ;; cache the value of edit-mode at this moment
    (compoundundomanager-postedit
     undo-manager
     (make-undoable-edit
      "Shift Rule Up"
      (lambda () ;; undo
        (rmgr-edit curr-edit-mode edited-obj-ID)
        (shift-rule-down))
      (lambda () ;; undo
        (rmgr-edit curr-edit-mode edited-obj-ID)
        (shift-rule-up)))))
  
  (define (shift-down-callback e)
    (shift-rule-down)
     ;; cache these edited obj ID and edit-mode
  (define edited-obj-ID (rmgr-get-currently-edited-ID))
  (define curr-edit-mode edit-mode) ;; cache the value of edit-mode at this moment
    (compoundundomanager-postedit
     undo-manager
     (make-undoable-edit
      "Shift Rule Down"
      (lambda () ;; undo
        (rmgr-edit curr-edit-mode edited-obj-ID) ;; pop up
        (shift-rule-up))
      (lambda () ;; undo
        (rmgr-edit curr-edit-mode edited-obj-ID)
        (shift-rule-down)))))

  (case up-or-down
    ((up) shift-up-callback)
    ((down) shift-down-callback)
    (else (display "ERROR: up-or-down to shift-rule-button-callback wrong")(newline)))
  )

;;;; fall through
;; returns the callback that is has the parameter ruleID and ft-button (these differs for different button)
(define (fall-through-button-callback ruleID)
  
  ;; set fall through or not for this rule 
  (define (set-fall value :: <boolean>)
    ;; must get the correct fall through button associated with this ruleID
    (define ft-button (rule-panel-fall-button (get-rule-panel-for ruleID)))
    (define rule-obj (get 'rules ruleID))
    (if (and rule-obj ft-button)
        (if value
            (begin
              (ask rule-obj 'set-fall-through? #t)
              (set-button-label ft-button "Fall")
              )
            (begin
              (ask rule-obj 'set-fall-through? #f)
              (set-button-label ft-button "Stop")
              )))

    ;; pop the rules manager out again
    ;(set-component-visible rules-manager-main-dialog #t)
    (rmgr-edit edit-mode (rmgr-get-currently-edited-ID))

    ;; right most button of dialog box is getting pushed out
    (pack-frame rules-manager-main-dialog))
  
  (define (ft-button-callback e)
    (define ft-button (rule-panel-fall-button (get-rule-panel-for ruleID)))
    ;; alternate button display between "Fall" and "Stop"
    (if (equal? (get-button-label ft-button) "Fall")
        (begin
          (set-fall #f)
          (compoundundomanager-postedit
           undo-manager
           (make-undoable-edit "Set Stop"
                               (lambda () ;; undo
                                 (set-fall #t))
                               (lambda () ;; redo
                                 (set-fall #f)))))
        (begin
          (set-fall #t)
          (compoundundomanager-postedit
           undo-manager
           (make-undoable-edit "Set Fall"
                               (lambda () ;; undo
                                 (set-fall #f))
                               (lambda () ;; redo
                                 (set-fall #t))))
          )))
  
  ft-button-callback)

;; edit rule button
;; returns a callback parameterized by ruleID
(define (edit-rule-button-callback ruleID)
  (display "selected link, edited-nodeID, ruleID ")(newline)
  (display (list selected-linkID edited-nodeID ruleID))(newline)
  ;; return this callback
  (lambda (e)
    (cond ((equal? edit-mode 'link)
           (doeditlink edited-linkID edited-nodeID ruleID)
           )
          ((equal? edit-mode 'node)
           (doeditnoderule edited-nodeID ruleID)
           )
          ((equal? edit-mode 'doc)
           #f
           ))))

;;  =================
;;;; main operation
;;  =================

;; populate rule manager with the rule panels of the edited object 
;; target-type 'link/'node
;; obj-ID is linkID or nodeID
(define (populate-rules-manager target-type obj-ID)
  
  (clear-container rmgr-rules-list-panel)
  
  (cond ((equal? target-type 'link)
         (set! edited-linkID obj-ID)
         (define in-link (get 'links obj-ID))
         (define rule-lst (ask in-link 'rule-lst))
         (map (lambda (ruleID)
                (rmgr-add-rule-panel ruleID)
                ) rule-lst)
         )
        ((equal? target-type 'node)
         (set! edited-nodeID obj-ID)
         (define in-node (get 'nodes obj-ID))
         (define rule-lst (ask in-node 'rule-lst))
         (map (lambda (ruleID)
                (rmgr-add-rule-panel ruleID)
                ) rule-lst)
         ))
  
  (pack-frame rules-manager-main-dialog))

;; edit the rules of this object, target-type ('link 'node)
(define (rmgr-edit target-type obj-ID)
  (set! edit-mode target-type)
  (populate-rules-manager target-type obj-ID)
  
  ;; make sure nodeeditor is open when rule manager is editing
  ;; NOTE: depends on populate-rules-manager to set edited-linkID and edited-nodeID properly
  (case edit-mode
    ((link)
     (define parent-nodeID (ask (get 'links edited-linkID) 'source))
     
     ;; pop nodeeditor out again if not already
     (nodeeditor-edit parent-nodeID)
     
     ;; make sure link is selected in nodeeditor
     (do-selectlink edited-linkID)
     ) 
    ((node)
     (nodeeditor-edit obj-ID)))
  
  (set-component-visible rules-manager-main-dialog #t))

(define (rmgr-close)
  (if rules-manager-main-dialog
      (set-component-visible rules-manager-main-dialog #f)))

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
  
  ;; rule list buttons
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
  
  (add-actionlistener add-rule-button 
                      (make-actionlistener add-rule-button-callback))
  
  (add-actionlistener delete-rule-button 
                      (make-actionlistener delete-selected-rule-button-callback))
  
  (add-actionlistener rules-dialog-close
                      (make-actionlistener
                       (lambda (e)
                         (rmgr-close))))
  )
