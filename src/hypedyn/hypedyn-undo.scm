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

(require "../common/objects.scm") ;; ask
(require "../common/datatable.scm") ;; set-dirty!, clear-dirty!, del, get
(require "../kawa/ui/undo.scm")
(require "nodeeditor.scm") ;; nodeeditor-set-dirty! nodeeditor-clear-dirty!
(require "hteditor.scm") ;; update-dirty-state, update-node-style
(require "htfileio.scm") ;; ht-build-sexpr-from-object-with-rule, update-dirty-state, hd-autosave
(require "editlink.scm") ;; update-nodegraph-display, remove-link-display, add-link-display
(require "rules-manager.scm") ;; rmgr-close
(require 'list-lib) ;; list-copy

(module-export delete-link-action
               delete-link-undo
               
               before-edit-rule
               after-edit-rule
               cache-rule
               
               post-edit-rule-undoable-event
               
               undo-manager init-undo-system
               undo-action redo-action
               hd-postedit hd-begin-update hd-end-update
               )

; undo
(define undo-manager #f)
(define undo-action #f)
(define redo-action #f)

(define (init-undo-system)
  (set! undo-manager (make-compoundundomanager))
  (set! undo-action (make-undo-action undo-manager))
  (set! redo-action (make-redo-action undo-manager))
  (invoke (as <compoundundomanager> undo-manager) 'set-undo-redo-actions undo-action redo-action)
  (set-associated-redoaction! undo-action redo-action)
  (set-associated-undoaction! redo-action undo-action)
  
  ;; give reference of set-dirty! and clear-dirty! to use to undo.scm
  (save-point-tracking-init 
   (lambda () ;; same as ht-set-dirty!
     (set-dirty!)
     ;;(nodeeditor-set-dirty!) ;; no need to set as actions inside nodeeditor triggers this
     (update-dirty-state))
   (lambda ()
     (clear-dirty!)
     (nodeeditor-clear-dirty!)
     (update-dirty-state)))
  )

; delete link action (do and redo)
(define (delete-link-action 
         linkID from-nodeID del-in-nodeeditor update-node-style-callback)
  
  ;; need to remove display before delete-link-action which removes the rules with follow link action
  (remove-link-display linkID)
  
  (define from-node (get 'nodes from-nodeID))
  (set! thelink (get 'links linkID))
  
  ; delete link from the from-node and from links-list
  (if from-node
      (begin
        ; delete from from-node
        (ask from-node 'dellink linkID)

        ; remove from UI - cheat by just repopulating the list
        (if (= from-nodeID edited-nodeID)
            (ask link-list 'populate-list from-node #f))

        ; remove from nodeeditor (remove underlining)
        (if del-in-nodeeditor
            (ask node-editor 'removelink thelink))

        ; TODO: should check if there's a selection, and if so, enable newlink button
        ; disable link-related buttons in node editor
        (enable-link-buttons #f)

        ; update node style, in case this was an alt-text link
        (update-node-style-callback from-nodeID)
        ))

  ; delete link from data-table
  (del 'links linkID)
  
  ;; hide rule-manager if current edited-linkID is linkID
  (if (= edited-linkID linkID)
      (begin
        (set! edited-linkID '())
        (rmgr-close)))

  ; update main window label to show file is dirty
  (update-dirty-state))

; delete link undo action
(define (delete-link-undo redo-sexpr linkID from-nodeID update-node-style-callback del-in-nodeeditor)
  ;; first need to recreate the link in data structure: this recreates the link
  ;; and any contained rules, conditions and actions, and adds them to the data table
  (eval-sexpr redo-sexpr)

  ;; retrieve the newly recreated link
  (set! thelink (get 'links linkID))
  
  ; and the link list - cheat by just repopulating the list (this updates the left list in the node editor)
  (if (= from-nodeID edited-nodeID)
      (begin
        (define from-node (get 'nodes from-nodeID))
        ;; all these only makes sense if we are editing the node in the node editor
        (ask link-list 'populate-list from-node #f)
        (ask node-editor 'addlink thelink)
        ;; when should we not do this? when should del-in-nodeeditor be false? and why?
        ;; undoing a deletion of link text (whole link) seem to not underline the text properly
        ;; now need to update the node editor
        ;;  (if del-in-nodeeditor
        ;;      (ask node-editor 'addlink thelink))
        )
      (begin
        (display "in delete-link-undo ")(display (list from-nodeID edited-nodeID))(newline)
        (display "from-nodeID edited-nodeID different so did not add to link-list ")(newline)
        ))
  
  ; TODO: link button states?

  ; update node style, in case this was an alt-text link
  (update-node-style-callback from-nodeID)
  
  ;; draw the link in the node graph (must do after redo-sexpr run) 
  (add-link-display linkID))

;;;; new caching for edit rule

;; rule cache
(define unedited-rule-sexpr #f)
(define edited-rule-sexpr #f)

(define (before-edit-rule ruleID)
  (set! unedited-rule-sexpr (cache-rule ruleID))
  (display "before edit rule ")(newline)
  (display unedited-rule-sexpr)(newline)
  )
(define (after-edit-rule ruleID)
  (set! edited-rule-sexpr (cache-rule ruleID))
  (display "after edit rule ")(newline)
  (display edited-rule-sexpr)(newline)
  )

;; create the sexpr of the rule complete with actions and conditions
(define (cache-rule rule-ID)
  (display "[cache rule] ")(newline)
  (define rule-obj (get 'rules rule-ID))
  (if rule-obj
      (begin
        (define actions (ask rule-obj 'actions))
        (define conditions (ask rule-obj 'conditions))
        (define rule-sexpr (ask rule-obj 'to-save-sexpr))
        (display "cache rule actions ")(display actions)(newline)
        (define combined-sexpr
          (append (list 'begin
                        rule-sexpr)
                  ;; a list of all the sexpr from the actions
                  (map (lambda (actionID)
                         (define action (get 'actions actionID))
                         (if action
                             (begin
                               (ask action 'to-save-sexpr)
                               ))
                         ) actions)
                  (map (lambda (conditionID)
                         (define condition (get 'conditions conditionID))
                         (if condition
                             (begin
                               (ask condition 'to-save-sexpr)
                               ))
                         ) conditions)
                  ))
        combined-sexpr)))

;; NOTE: undoing/redoing of rule edit does not bring up the rule editor UI
(define (post-edit-rule-undoable-event type obj-ID ruleID)
  
;  (display "posting edit rule undoable ")(newline)
  ;; copy the cache and store in this context (the cache would be reused for other edits)
  (define unedited-sexpr-copy (list-copy unedited-rule-sexpr))
  (define edited-sexpr-copy (list-copy edited-rule-sexpr))
  
;  (display "unedited-sexpr-copy ")(newline)
;  (display unedited-sexpr-copy)(newline)
;  
;  (display "edited-sexpr-copy ")(newline)
;  (display edited-sexpr-copy)(newline) 
  
  (define (empty-rule)
    (define the-rule (get 'rules ruleID))
    ;; empty the rule and delte the old actions and conditions
    ;; since we're not recycling the conditions and actions, delete them from data table
    (display "recycling ")(display (ask the-rule 'actions))(newline)
    (map (lambda (actionID)
           (del 'actions actionID)
           ) (ask the-rule 'actions))
    (map (lambda (condID)
           (del 'conditions condID)
           ) (ask the-rule 'conditions))
    ;; empty current rule's contents before making the changes
    (if the-rule
        (ask the-rule 'empty-rule))
    )
  
  (hd-begin-update undo-manager)
  
  (hd-postedit
   undo-manager
   (make-undoable-edit
    "Edit Rule"
    (lambda ()  ; undo
      ;; recreate the unedited version of the rule and all its actions
      (case type
        ((link) ;; link need to update node graph display (follow link action)
         (remove-follow-link-rule-display ruleID)
         (remove-show-popup-rule-display ruleID)
         ;(display "UNDOING edit rule ")(display unedited-sexpr-copy)(newline)
         (empty-rule)
         (eval-sexpr unedited-sexpr-copy)
         (add-follow-link-rule-display ruleID)
         (add-show-popup-rule-display ruleID)
         )
        ((node)
         (eval-sexpr unedited-sexpr-copy)))
      
      ;; put old ruleID in the right place
      (let ((the-obj (case type
                       ((link) (get 'links obj-ID))
                       ((node) (get 'nodes obj-ID)))))
        ;; remove the extra ruleID that was added to the-obj 
        ;; when the create-typed-rule2 in sexpr was run 
        (ask the-obj 'remove-last-rule))
      )
    (lambda () ;; redo
      (case type
        ((link)
         (remove-follow-link-rule-display ruleID)
         (remove-show-popup-rule-display ruleID)
         (empty-rule)
         (eval-sexpr edited-sexpr-copy)
         ;(display "REDOING edit rule ")(display edited-sexpr-copy)(newline)
         (add-follow-link-rule-display ruleID)
         (add-show-popup-rule-display ruleID)
         )
        ((node)
         (eval-sexpr edited-sexpr-copy)))
     
      ;; put new-ruleID in the right place
      (let ((the-obj (case type
                       ((link) (get 'links obj-ID))
                       ((node) (get 'nodes obj-ID)))))
        (ask the-obj 'remove-last-rule))
      )
    ))
  
  (hd-end-update undo-manager undo-action redo-action)
  )

;;;; Hypedyn undo wrapper

;; in addition to posting undoable actions, hypedyn needs to auto save every x actions

(define autosave-thresh 10)

;; hd stands for hypedyn
(define (hd-postedit undo-mgr :: <compoundundomanager>
                     in-edit :: <javax.swing.undo.UndoableEdit>)
  ;; wrapped operation
  (compoundundomanager-postedit undo-mgr in-edit)
  (auto-save-check undo-mgr)
  )

(define (hd-begin-update undo-mgr :: <compoundundomanager>)
  ;; wrapped operation
  (compoundundomanager-beginupdate undo-mgr))

(define (hd-end-update undo-mgr undo-action redo-action)
  ;; wrapped operation
  (compoundundomanager-endupdate undo-mgr undo-action redo-action)
  (auto-save-check undo-mgr)
  )

;; check whether it is time to autosave, do if if it is
(define (auto-save-check undo-mgr)
  ;; make sure only the single encapsulating compound undoable edit posted is considered
  ;;   we might have postedit and compound edits (with start-update, end-update) within packed 
  ;;   as a single edit using a start-update/end-update pair on the outer most level
  ;; every x number of compound postedit, auto save 
  (if (and (= (compoundundomanager-updatelevel undo-mgr) 0)
           (= (modulo (get-save-point-offset) 10) 0))
      (hd-autosave))
  )