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

(require "../common/objects.scm") ;; ask
(require "../common/datatable.scm") ;; set-dirty!, clear-dirty!, del, get
(require "../kawa/ui/undo.scm")
(require "nodeeditor.scm") ;; nodeeditor-set-dirty! nodeeditor-clear-dirty!
(require "hteditor.scm") ;; update-dirty-state, update-node-style, update-link-display
(require "htfileio.scm") ;; ht-build-sexpr-from-object-with-rule, update-dirty-state
(require "editlink.scm") ;; update-nodegraph-display
(require 'list-lib) ;; list-copy

(module-export delete-link-action
               delete-link-undo
               
               before-editlink
               after-editlink
               post-editlink-undoable-event
               
               before-editnode
               after-editnode
               post-edit-noderule-undoable-event
               
               before-edit-rule
               after-edit-rule
               post-edit-rule-undoable-event
               
               undo-manager init-undo-system
               undo-action redo-action
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
                                        ;         linkID thelink from-nodeID to-nodeID to-alt-nodeID
                                        ;                            name usedest usealtdest
                                        ;                            del-in-nodeeditor node-graph update-node-style-callback)
  
  (define from-node (get 'nodes from-nodeID))
  (set! thelink (get 'links linkID))
  
  ; delete link from the from-node and from links-list
  (if from-node
      (begin
        ; delete from from-node
        (ask from-node 'dellink linkID)

        ; remove from UI - cheat by just repopulating the list
        (if (= from-nodeID edited-nodeID)
            (ask link-list 'populate-list from-node #f)
            (begin
              (display "in delete-link-action ")(newline)
              (display "from-nodeID edited-nodeID different so did not add to link-list ")(newline)
              ))

        ; remove from nodeeditor (remove underlining)
        (if del-in-nodeeditor
            (ask node-editor 'removelink thelink))

        ; disable link-related buttons in node editor
        (enable-link-buttons #f)

        ; TODO: should check if there's a selection, and if so, enable newlink button

        ; update node style, in case this was an alt-text link
        (update-node-style-callback from-nodeID)
        ))

  ; delete link from data-table
  (del 'links linkID)

  ; update main window label to show file is dirty
  (update-dirty-state))

; delete link undo action
(define (delete-link-undo redo-sexpr linkID from-nodeID update-node-style-callback del-in-nodeeditor)
  ;         redo-sexpr
;         linkID thelink from-nodeID to-nodeID to-alt-nodeID
;         name usedest usealtdest
;         del-in-nodeeditor node-graph update-node-style-callback)
  ; first need to recreate the link in data structure: this recreates the link
  ; and any contained rules, conditions and actions, and adds them to the data table
  (eval-sexpr redo-sexpr)

  (define from-node (get 'nodes from-nodeID)) 
  
  ;; retrieve the newly recreated link
  (set! thelink (get 'links linkID))
  (display " del-in-nodeeditor ")(display del-in-nodeeditor)(newline)
  
  ;; now need to update the node editor
;;  (if del-in-nodeeditor
;;      (ask node-editor 'addlink thelink))
  
  ;; when should we not do this? when should del-in-nodeeditor be false? and why?
  ;; undoing a deletion of link text (whole link) seem to not underline the text properly
  
  ; and the link list - cheat by just repopulating the list (this updates the left list in the node editor)
  (if (= from-nodeID edited-nodeID)
      (begin
        ;; all these only makes sense if we are editing the node in the node editor
        (ask link-list 'populate-list from-node #f)
        (ask node-editor 'addlink thelink)
        )
      (begin
        (display "in delete-link-undo ")(display (list from-nodeID edited-nodeID))(newline)
        (display "from-nodeID edited-nodeID different so did not add to link-list ")(newline)
        )
      )
  
  ; TODO: link button states?

  ; update node style, in case this was an alt-text link
  (update-node-style-callback from-nodeID)

  )

;;;; new caching for edit rule

;; rule cache
(define unedited-rule-sexpr #f)
(define edited-rule-sexpr #f)

(define (before-edit-rule ruleID)
  (cache-rule ruleID #f))
(define (after-edit-rule ruleID)
  (cache-rule ruleID #t))

(define (cache-rule rule-ID edited?)
  (define rule-obj (get 'rules rule-ID))
  (if rule-obj
      (begin
        (define actions (ask rule-obj 'actions))
        (define conditions (ask rule-obj 'conditions))
        (define rule-sexpr (ask rule-obj 'to-save-sexpr))
        (define combine-sexpr
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
        (display "cache rule ")(display combine-sexpr)(newline)
        (if edited?
            (set! edited-rule-sexpr combine-sexpr)
            (set! unedited-rule-sexpr combine-sexpr))
        )))

;; reason I can get away with using unedited-rule-sexpr and edited-rule-sepxr
;; is because list are passed by reference
;; when i created a pair of unedited-rule-sexpr and edited-rule-sexpr 
;; it is only ever referenced by one undoable-event
;; no matter how many time unedited-rule-sexpr and edited-rule-sexpr had been re-set!
(define (post-edit-rule-undoable-event linkID old-ruleID new-ruleID)
  
  (compoundundomanager-postedit
   undo-manager
   (make-undoable-edit
    "Edit Rule"
    (lambda ()  ; undo
      ;; recreate the unedited version of the rule and all its actions
      (display "undo ")(display unedited-rule-sexpr)(newline)
      
;      (let ((thelink (get 'links linkID)))
;        (ask thelink 'remove-rule ruleID))
      
      (remove-follow-link-rule-display new-ruleID)
      (eval-sexpr unedited-rule-sexpr)
      (add-follow-link-rule-display old-ruleID)
      
      ;; put old ruleID in the right place
      (let ((the-link (get 'links linkID)))
        ;; make sure new-ruleID is in the position where edited-ruleID used to be
        (ask the-link 'remove-rule old-ruleID)  ;; remove the old-ruleID create-type-rule2 added
        (ask the-link 'replace-rule new-ruleID old-ruleID))  ;; place new-ruleID in the spot of edited-ruleID
      
      ;; delete new-ruleID
      (del 'rules new-ruleID)
      ;(update-nodegraph-display)
      )
    (lambda () ;; redo
       (display "redo ")(display edited-rule-sexpr)(newline)
      
;      (let ((thelink (get 'links linkID)))
;        (ask thelink 'remove-rule old-ruleID))
      
      (remove-follow-link-rule-display old-ruleID)
      (eval-sexpr edited-rule-sexpr)
      (add-follow-link-rule-display new-ruleID)
     
      ;; put new-ruleID in the right place
      (let ((the-link (get 'links linkID)))
        ;; make sure new-ruleID is in the position where edited-ruleID used to be
        (ask the-link 'remove-rule new-ruleID)  ;; remove the new-ruleID create-type-rule2 added
        (ask the-link 'replace-rule old-ruleID new-ruleID))  ;; place new-ruleID in the spot of edited-ruleID
      ;(update-nodegraph-display)
      ;; delete old-ruleID
      (del 'rules old-ruleID)
      )
    ))
  )

;;;; OLD caching for editlink/editnoderule
;; set by before-editlink
(define unedited-link-data (list))
(define unedited-link-data2 (list))
(define edited-link-data (list))
(define edited-link-data2 (list))

(define editlink-common-data (list))

(define (before-editlink link-ID)
  (cache-link link-ID #f))

(define (after-editlink link-ID)
  (cache-link link-ID #t))


;; cache the information of the link to 2 lambda object
;; unedited-link-lambda restores the link to unedited state
;; edited-link-lambda does otherwise
(define (cache-link link-ID edited?)
  (define thelink (get 'links link-ID))
  (if thelink
      (begin
        ; store the action for redoing
        (define redo-sexpr (ht-build-sexpr-from-object-with-rule thelink))
        (define from-nodeID (ask thelink 'source))
        (define to-nodeID (ask thelink 'destination))
        (define to-alt-nodeID (ask thelink 'alt-destination))
        (define name (ask thelink 'name))
        (define from-node (get 'nodes from-nodeID))
        (define usedest (ask thelink 'use-destination))
        (define usealtdest (ask thelink 'use-alt-destination))
        
;;        (display "[CACHE LINK] ")(display (list redo-sexpr from-nodeID to-nodeID))(newline)
;;        (display (list to-alt-nodeID name from-node))(newline)
;;        (display (list usedest usealtdest))(newline)
        
        (if (not edited?)
            (begin
              ;; used for both delete-link-action and delete-link-undo
              (set! unedited-link-data
                    (list redo-sexpr
                          link-ID thelink from-nodeID to-nodeID to-alt-nodeID
                          name usedest usealtdest
                          #t node-graph update-node-style))
              (set! unedited-link-data2
                    (list usedest to-nodeID usealtdest to-alt-nodeID))
              (set! editlink-common-data (list name from-nodeID link-ID))
              )
            (begin
              ;; used for both delete-link-action and delete-link-undo
              (set! edited-link-data
                    (list redo-sexpr
                          link-ID thelink from-nodeID to-nodeID to-alt-nodeID
                          name usedest usealtdest
                          #t node-graph update-node-style))
              (set! edited-link-data2
                    (list usedest to-nodeID usealtdest to-alt-nodeID))
              ))
        ))
  )

;; TOFIX: in rule edit, action deletion and undo does not bring up the rule editor (previously called editlink)
;; TOFIX: undoing edits to the rule would change the name of the rule to the link/node obj's name
(define (post-editlink-undoable-event)
  (define unedited-link-cache (list-copy unedited-link-data))
  (define edited-link-cache (list-copy edited-link-data))
  
  (define unedited-link-cache2 (list-copy unedited-link-data2))
  (define edited-link-cache2 (list-copy edited-link-data2))
  
  (define editlink-common-cache (list-copy editlink-common-data))
  
;;  (if (and unedited-link-lambda
;;           edited-link-lambda)
;;      (begin
  (compoundundomanager-postedit
   undo-manager
   (make-undoable-edit
    ;; if compound edit on means that it is a new link operation
    (if (> (compoundundomanager-updatelevel undo-manager) 0)
        "New Link"
        "Edit Link")
    ;; undo
    (lambda () ;; restore the link before edit
               ;(unedited-link-lambda)
      (apply delete-link-action
             (cdr unedited-link-cache))
      (apply delete-link-undo unedited-link-cache)
      (apply update-link-display
             (append
              (list (list-ref editlink-common-cache 0)
                    (list-ref editlink-common-cache 1))
              edited-link-cache2
              unedited-link-cache2
              (list (list-ref editlink-common-cache 2)))))
    (lambda () ;; restore the link before edit
               ;(edited-link-lambda)
      (apply delete-link-action
             (cdr edited-link-cache))
      (apply delete-link-undo edited-link-cache)
      (apply update-link-display
             (append
              (list (list-ref editlink-common-cache 0)
                    (list-ref editlink-common-cache 1))
              unedited-link-cache2
              edited-link-cache2
              (list (list-ref editlink-common-cache 2)))))
    ))
  )

(define (before-editnode node-ID)
  (cache-node node-ID #f))
(define (after-editnode node-ID)
  (cache-node node-ID #t))

(define unedited-node-sexpr #f)
(define edited-node-sexpr #f)

(define (cache-node node-ID edited?)
  (define thenode (get 'nodes node-ID))
  (if thenode
      (begin
        (display "cached node sexpr ")(display (ht-build-sexpr-from-object-with-rule thenode))(newline)
        ; store the sexpr for redoing
        (if (not edited?)
            (set! unedited-node-sexpr (ht-build-sexpr-from-object-with-rule thenode))
            (set! edited-node-sexpr (ht-build-sexpr-from-object-with-rule thenode)))
        )))

(define (post-edit-noderule-undoable-event)
  (define unedited-node-sexpr-cache (list-copy unedited-node-sexpr))
  (define edited-node-sexpr-cache (list-copy edited-node-sexpr))
  (display "POSTING EDIT NODE RULE ")(newline)
  (compoundundomanager-postedit
   undo-manager
   (make-undoable-edit
    "Edit Node Rule"
    (lambda () ;; restore the node before edit (undo)
      (eval-sexpr unedited-node-sexpr-cache))
    (lambda () ;; restore the node before edit (redo)
      (eval-sexpr edited-node-sexpr-cache))
    ))
  )

; hypedyn's wrapper around compoundundomanager-postedit
;(define (ht-undoable-postedit in-undo-manager :: <compoundundomanager>
;                     in-edit :: <javax.swing.undo.UndoableEdit>)
;  
;  ;; do the actual posting
;  (compoundundomanager-postedit in-undo-manager in-edit)
;  )


