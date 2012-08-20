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
(require "../kawa/ui/undo.scm") ;; make-undoable-edit
(require "../kawa/ui/scrollpane.scm") ;; make-scrollpane
(require "../kawa/ui/text.scm") ;; set-text

(require "../kawa/strings.scm") ;; to-string
(require "../kawa/geometry.scm") ;; make-dimension
(require "../kawa/color.scm") ;; make-colour-rgb
(require "../kawa/graphics-kawa.scm") ;; make-rectangle

(require "../common/datatable.scm") ;; get
(require "../common/objects.scm") ;; ask
(require "../common/list-helpers.scm") ;; swap-left swap-right  
 
(require "nodeeditor.scm") ;get-nodeeditor-frame
(require "config-options.scm") ;; is-undo-enabled? rule-name-limit
(require "hypedyn-undo.scm") ;; undo-action, cache-rule
(require "editlink.scm") ;; doeditlink, set-edit-mode
(require "datastructure.scm") ;; create-typed-rule2
(require 'srfi-1) ;; remove


(module-export rmgr-init
               rmgr-edit
               rmgr-close
               rmgr-update
               delete-rule-from-obj
               )
;; rmgr stands for rule manager
(define rules-manager-main-dialog #f)
(define rmgr-rules-list-panel #f) ;; the panel that contain all the rule panels
(define center-panel #f) ;; the scrollpane rmgr-rules-list-panel is in

(define add-rule-button #f)
(define delete-rule-button #f)
(define rule-edit-button #f)
(define up-button #f)
(define down-button #f)

(define rule-panel-width 0)
(define-constant rule-panel-height 40)
;;;; helper functions
;; NOTE: rmgr-get-currently-edited depends on edited-linkID/edited-nodeID and edit-mode being correctly set
;;       so make sure rmgr-edit is called to update it

;; get rule-lst from currently edited object
(define (rmgr-rule-lst)
  (let ((edited-obj (rmgr-get-currently-edited)))
    (if edited-obj
        (ask edited-obj 'rule-lst)
        '()))
  )

;; update rule position in object's rule-lst
(define (rmgr-set-rule-lst new-lst)
  (ask (rmgr-get-currently-edited) 'set-rule-lst new-lst))

;; get the child of rmgr-rules-list-panel and see which position
;; this rule-panel is in
(define (rule-panel-position rule-panel)
  (list-index (lambda (obj) (eq? rule-panel obj)) (get-container-children rmgr-rules-list-panel) ))

(define (get-nth-rule-panel n)
  (set! n (quotient n 2))
  (get-container-children rmgr-rules-list-panel)
  )

(define (get-rule-panel-for ruleID)
  (define index (list-index (lambda (rid) (= rid ruleID)) (rmgr-rule-lst)))
  (if index
      (list-ref (get-container-children rmgr-rules-list-panel) index)
      #f))

(define (get-ruleID-for-rule-panel pnl)
  (define index (list-index (lambda (this-pnl) (equal? pnl this-pnl)) (get-container-children rmgr-rules-list-panel)))
  (if index
      (list-ref (rmgr-rule-lst) index)
      #f)
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
    ((doc) -1)))

(define (get-rule-name ruleID)
  (let* ((rule-obj (get 'rules ruleID))
         (rule-name (ask rule-obj 'name)))
    
    ;; truncate rule name display
    (if (> (string-length rule-name) rule-name-limit)
        (set! rule-name (string-append (substring rule-name 0 rule-name-limit) "...")))
    
    (if (show-IDs?)
        (to-string (string-append rule-name "(" (to-string ruleID) ")"))
        rule-name))
  )

;;;; make-rule-panel
;; one instance of the rule panel (one entry on the rule list)
;; only called by rmgr-add-rule-panel which makes sure we are passed a valid ruleID (gotten from actual rule-lst)
(define (make-rule-panel ruleID)
  
  ;; make components
  (define top-panel (make-panel))
  (define rule-name-label #f)
  
  ;; make buttons
  ;; new replacement for the previous "Fall" button for fall through
  (define fall-checkbox (make-checkbox "Stop if true"))
  (set-checkbox-text-alignment fall-checkbox 'left)

  (set-container-layout top-panel 'horizontal)
  
  (if (= rule-panel-width 0)
      (begin
        ;(define vert-scrollbar (scroll-get-scrollbar center-panel 'vert))
        ;(define scrollbar-width (get-preferred-width vert-scrollbar))
        ;(set! rule-panel-width (- (scroll-viewport-width center-panel) scrollbar-width))
        (display (scroll-viewport-width center-panel))(newline)
        (set! rule-panel-width (scroll-viewport-width center-panel))
        ))
  
  (set-component-non-resizable-size top-panel rule-panel-width rule-panel-height)
  
  ;; assume ruleID valid
  (define rule-obj (get 'rules ruleID))
  (if rule-obj
      (let ((rule-name (ask rule-obj 'name))
            (rule-fall-through? (ask rule-obj 'fall-through?)))
        
        ;; TODO: this should refresh when show id just got selected
        (set! rule-name (get-rule-name ruleID))
        
        (set! rule-name-label (make-label-with-title rule-name))
  
        ;; set initial state of the fall-checkbox
        (set-checkbox-value fall-checkbox (not rule-fall-through?))
        )
      (begin
        (display "ERROR make-rule-panel given invalid ruleID")(newline)))
    
  
  ;; for selection of panels
  (add-mouselistener 
   top-panel
   (make-mouselistener
    (lambda (e)
      (if (equal? (get-mouseevent-type e) 'left-clicked)
          (begin
            (display "number of clicks ")(display (get-mouseevent-click-count e))(newline)
            (if (>= (get-mouseevent-click-count e) 1)
                (begin
                  ;; if control key not held down unselect the others
                  (if (not (ctrl-key-down? (get-mouseevent-rawevent e)))
                      (map (lambda (rID)
                             (if (not (= rID ruleID))
                                 (select-rule-panel rID #f))
                             ) (rmgr-rule-lst)))

                  (select-rule-panel ruleID (not (panel-selected? top-panel)))
                  (action-restrict-check)
                  ))
            (if (= (get-mouseevent-click-count e) 2)
                (begin
                  (display "click count 2, editing rule ")(newline)
                  ((edit-rule-button-callback ruleID) #f)
                  ))
              ))
            )))
    
  (add-itemlistener
   fall-checkbox
   (make-itemlistener
    (fall-cb-callback ruleID)
    ))
  
  ;; buttons listeners
  (add-component top-panel rule-name-label)
  ;; pushes the right elements to the right and left to the left (does the left and right justify)
  (add-component top-panel (create-horizontal-glue))
  (add-component top-panel fall-checkbox)
  
  (set-border top-panel bevel-in-border)
  
  ;; make rule panel
  top-panel)

(define (update-rule-panel ruleID)
  (define pnl (get-rule-panel-for ruleID))
  
  ;; update the name of the rule
  (define name-label (get-rule-panel-name-label pnl))
  (define fall-checkbox (get-rule-panel-fall-checkbox pnl))
  (define rule (get 'rules ruleID))
  
  (if rule
      (begin
        (set-text name-label (get-rule-name ruleID))
        (display "updating rule panel with name ")(display (ask rule 'name))(newline)
        (display "SETTTING fall check box FALLTHROUGH? ")(display (ask rule 'fall-through?))(newline)
        ;; check means DONT fall through
        (set-checkbox-value fall-checkbox (not (ask rule 'fall-through?)))
        (component-update pnl)
        )
      (begin (display "[update-rule-panel] invalid ruleID")(newline)))
  )



;;;; add/remove rule panel

;; add rule (makes a rule panel inside rmgr-rules-list-panel)
;; when pos is added it adds the panel and the ruleID in that particular position
(define (rmgr-add-rule-panel ruleID #!optional index)
  (if (and rmgr-rules-list-panel
           rules-manager-main-dialog)
      (begin
        (define new-panel (make-rule-panel ruleID))
        
        (set-component-align-x new-panel 'left)
        (if index
            (add-component-at rmgr-rules-list-panel new-panel index)
            (add-component rmgr-rules-list-panel new-panel))
        
        (component-revalidate center-panel) ;; revalidate the scrollpane
        new-panel)
      (begin
        (display "ERROR: in rmgr-add-rule-panel")(newline)
        #f)))

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
         ;(define component-lst (get-container-children rule-panel))
         ;(define this-checkbox (car component-lst))

         ;; if checkbox checked remove
         (if (panel-selected? rule-panel);(get-checkbox-value this-checkbox)
             (begin
               (remove-component rmgr-rules-list-panel rule-panel)
               (pack-frame rules-manager-main-dialog)
               (set! deleted-ID-lst (append deleted-ID-lst (list ruleID)))
               ))
         ) rule-panel-lst (rmgr-rule-lst))
  
  ;; return a list of the deleted ID
  deleted-ID-lst)

;;;; components of rule-panel 
;(define (get-rule-panel-checkbox rule-panel)
;  (list-ref (get-container-children rule-panel) 0))
;(define (rule-panel-fall-button rule-panel)
;  (list-ref (get-container-children rule-panel) 3))
(define (get-rule-panel-fall-checkbox rule-panel)
  (list-ref (get-container-children rule-panel) 2))
(define (get-rule-panel-name-label rule-panel)
  (list-ref (get-container-children rule-panel) 0))

;;=========================
;;;; rule panel selection
;;=========================

;; get the ruleID of the selected panel
(define (selected-rule-lst)
  (map (lambda (index)
         (list-ref (rmgr-rule-lst) index))
         (get-selected-pos-lst)))

(define (panel-selected? pnl)
  (equal? (get-background-color pnl) selected-color))

(define selected-color (make-colour-rgb 135 206 250))  ;; sky blue
(define unselected-color (make-colour-rgb 238 238 238))

;; return a list of the position of the selected rule panels
(define (get-selected-pos-lst)
  (define (helper lst pos) 
    (if (null? lst)
        '()
        (begin
          (define rule-panel (car lst))
          (append 
           ;; condition append
           (if (panel-selected? rule-panel)
               (list pos)
               '())
           (helper (cdr lst) (+ pos 1)))
          )))
  ;; traverse through the list of rule panels
  (helper (get-container-children rmgr-rules-list-panel) 0))

  ;; some actions are disabled depending on the number of rules selected
(define (action-restrict-check)
  
  ;; add button
  (set-component-enabled add-rule-button (<= (length (selected-rule-lst)) 1)) ;; 0 or 1 rule selected
  
  ;; only activate these buttons when only one is selected
  (set-component-enabled rule-edit-button (= (length (selected-rule-lst)) 1))
  
  ;; at least one selected to delete
  (set-component-enabled delete-rule-button (> (length (selected-rule-lst)) 0))
  
  ;; up button enable criteria
  (if (and (= (length (selected-rule-lst)) 1) ;; only one selected
           (not (= (car (selected-rule-lst)) (car (rmgr-rule-lst))))) ;; not first/top rule 
      (set-component-enabled up-button #t)
      (set-component-enabled up-button #f))
  
  ;; down button
  (if (and (= (length (selected-rule-lst)) 1) ;; only one selected
           (not (= (car (selected-rule-lst)) 
                   (last (rmgr-rule-lst))))) ;; not last/btm rule 
      (set-component-enabled down-button #t)
      (set-component-enabled down-button #f))
  )

;; assume rule manager is correctly loaded (rule-panel-lst correspond to rmgr-rule-lst)
;; check the checkbox in the panel this ruleID
(define (select-rule-panel ruleID selected?)
  (define rule-panel-lst (get-container-children rmgr-rules-list-panel))
  (map (lambda (rule-panel this-ruleID)
         (define component-lst (get-container-children rule-panel))
         (define rule-name-label (get-rule-panel-name-label rule-panel))
         (define fall-checkbox (get-rule-panel-fall-checkbox rule-panel))
         (if (= this-ruleID ruleID)
             (if selected?
                 (begin
                   (set-background-color rule-panel selected-color)
                   (set-background-color rule-name-label selected-color)
                   (set-background-color fall-checkbox selected-color)
                   
                   ;; need to do this to give new-panel a position
                   (validate-container rules-manager-main-dialog)

                   ;; scroll to newly added panel
                   ;; top left point of new-panel relative to scrollpane
                   (define new-panel-tl-point (get-component-location rule-panel))
                   (define tl-x (invoke new-panel-tl-point 'get-x))
                   (define tl-y (invoke new-panel-tl-point 'get-y))
                   (scroll-rect-to-visible rmgr-rules-list-panel (make-rectangle tl-x tl-y rule-panel-width rule-panel-height))
                   )
                 (begin
                   (set-background-color rule-panel unselected-color)
                   (set-background-color rule-name-label unselected-color)
                   (set-background-color fall-checkbox unselected-color))
                 ))
         ) rule-panel-lst (rmgr-rule-lst)))

;; remove rule assumes that obj is the currently 
;; edited object in the rule manager
;; NOTE: relies on the rmgr-rule-lst to tell us where 
;;       is the panel, so important to do this first before 
;;       removing ruleID from the rule-lst
(define (rmgr-remove-rule-panel ruleID)
  (define index (list-index (lambda (rid) (= rid ruleID)) (rmgr-rule-lst)))    ;; get the index in rmgr-rule-lst and rmgr-rules-list-panel
  
  ;; remove corresponding panel
  (define comp-lst (get-container-children rmgr-rules-list-panel))
  (define panel-to-remove (list-ref comp-lst index))
  (remove-component rmgr-rules-list-panel panel-to-remove)
  (pack-frame rules-manager-main-dialog)
  )

;;;; button callback
(define (add-rule-button-callback e)
  
  ;; cache these edited obj ID and edit-mode
  (define edited-obj-ID (rmgr-get-currently-edited-ID))
  (define curr-edit-mode edit-mode)
  (define num-selected (length (selected-rule-lst)))
  (define index #f)
  
  ;; leave out the parent object ID to create-typed-rule2
  ;; we'll add it ourselves in the right position through rmgr-set-rule-lst
  ;; fall through true by default
  (define new-rule-ID (create-typed-rule3 "new rule" edit-mode 'and #f #f fall-through?: #t))
  
  ;; adds a rule panel and the ruleID in the correct positions
  ;; assumes create-typed-rule2 already called
  (define (do-add-rule #!optional index)
    ;; if index given means we're inserting a rule in between (not at the end)
    
    (define new-panel #f)
    (if index
        (begin 
          (set! new-panel (rmgr-add-rule-panel new-rule-ID index))
          (define new-rule-lst (list-insert (rmgr-rule-lst) new-rule-ID index))
          (rmgr-set-rule-lst new-rule-lst))
        (begin
          (set! new-panel (rmgr-add-rule-panel new-rule-ID))
          (rmgr-set-rule-lst (append (rmgr-rule-lst) (list new-rule-ID)))
          ))
    
    ;; deselect all rule panel first
    (map (lambda (ruleID)
           (select-rule-panel ruleID #f))
         (rmgr-rule-lst))
    
    ;; then select the newly added panel
    (select-rule-panel new-rule-ID #t)
    
    ;; enable delete button since we there is now a selected panel
    (set-component-enabled delete-rule-button #t)
    
    (define new-rule (get 'rules new-rule-ID))
    (ask new-rule 'set-parentID! edited-obj-ID)
    )
  
  ;; if a rule is selected, add the rule right after it
  (if (= num-selected 0)
      (do-add-rule)
      (if (= num-selected 1)
          (begin 
            (set! index (+ (car (get-selected-pos-lst)) 1))
            (do-add-rule index))))
  
  (action-restrict-check)
  
  ;; post undo
  (hd-postedit
   undo-manager
   (make-undoable-edit
    "Add Rule"
    (lambda () ;; undo
      ;; pops up rmgr and make sure we're editing that obj's 
      (rmgr-edit curr-edit-mode edited-obj-ID)
      (display "undoing add rule ")(newline)
      (rmgr-remove-rule-panel new-rule-ID)
      (rmgr-set-rule-lst (remove (lambda (rid) (= rid new-rule-ID)) (rmgr-rule-lst)))
      (del 'rules new-rule-ID)
      ;; the last panel does not disappear even after we do remove-component thus we do this
      (component-update rmgr-rules-list-panel)
      (action-restrict-check)
      )
    (lambda () ;; redo
      (rmgr-edit curr-edit-mode edited-obj-ID)
      ;(eval-sexpr new-rule-sexpr)   ;; recreate the new rule
      
      ;; create the rule with the same ID
      (create-typed-rule2 "new rule" edit-mode 'and #f #f new-rule-ID)
      (do-add-rule index)
      (action-restrict-check)
      ))))

;; obj can be a link or node 
(define (delete-rule-from-obj ruleID obj)
  ;; actually only applicable to link objects does nothing on a node obj
  (remove-follow-link-rule-display ruleID)  ;; remove the line display from the ruleID (if any)
  (remove-show-popup-rule-display ruleID)  ;; remove the line display from ruleID

  ;; remove the rule from the obj
  (ask obj 'set-rule-lst 
       (remove (lambda (thisruleID) 
                 (= ruleID thisruleID)) 
               (ask obj 'rule-lst)))
  
  ;; delete the rule from our datatable
  (del 'rules ruleID)
  )

(define (delete-selected-rule-button-callback e)
  ;; because create-typed-rule2 adds the ruleID to the obj 
  ;; it messes up the previous order, so cache it and set it back later
  (define rule-lst-before-deletion (list-copy (rmgr-rule-lst)))
  (define deleted-ID-lst (remove-selected-rule-panel))
  (action-restrict-check)

  (if (not (null? deleted-ID-lst))
      (begin
        ;; cache these edited obj ID and edit-mode
        (define edited-obj-ID (rmgr-get-currently-edited-ID))
        (define curr-edit-mode edit-mode) ;; cache the value of edit-mode at this moment

        (define deleted-sexpr-lst
          (map cache-rule deleted-ID-lst))
        (display "delete selected ")(display deleted-ID-lst)(newline)
        

        (map (lambda (ruleID)
               (delete-rule-from-obj ruleID
                                     (get
                                      (case edit-mode
                                        ((link) 'links)
                                        ((node) 'nodes))
                                      edited-obj-ID))
               ) deleted-ID-lst)
        
        ;; the last panel does not disappear even after we do remove-component thus we do this
        (component-update rmgr-rules-list-panel)

        ;; post undo
        (hd-postedit
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
            
            ;; restore the lines in the graph associated to the rule's action
            (map (lambda (ruleID)
                   (if (eq? edit-mode 'link)
                       (begin
                         (add-follow-link-rule-display ruleID)
                         (add-show-popup-rule-display ruleID)
                         ))
                   ) deleted-ID-lst)
            )
          (lambda () ;; redo
            (rmgr-edit curr-edit-mode edited-obj-ID)

            ;; remove panel first (important to do before remove rule-lst)
            (map (lambda (ruleID)
                   (rmgr-remove-rule-panel ruleID)
                   (delete-rule-from-obj ruleID
                                     (get
                                      (case edit-mode
                                        ((link) 'links)
                                        ((node) 'nodes))
                                      edited-obj-ID))
                   ) deleted-ID-lst)
            (component-update rmgr-rules-list-panel)
            ))))))

;; used by the external rule button (as opposed to the rule button on the panel)
(define (edit-rule-button-callback2 e)
  ;; reusing the old code for now
  (define selected-ruleID-lst (selected-rule-lst))
  (if (= (length selected-ruleID-lst) 1)
      (begin
        ;; get the lambda object and run it on the spot
        ((edit-rule-button-callback (car selected-ruleID-lst)) #f)
        )))

(define (shift-up-callback2 e)
  (define selected-ruleID-lst (selected-rule-lst))
  (if (= (length selected-ruleID-lst) 1)
      (begin
        ;; get the lambda object and run it on the spot
        ((shift-rule-button-callback (car selected-ruleID-lst) 'up) #f)
        ))
  ;(shift-rule-button-callback ruleID up-or-down)
  )

(define (shift-down-callback2 e)
  (define selected-ruleID-lst (selected-rule-lst))
  (if (= (length selected-ruleID-lst) 1)
      (begin
        ;; get the lambda object and run it on the spot
        ((shift-rule-button-callback (car selected-ruleID-lst) 'down) #f)
        ))
  ;(shift-rule-button-callback ruleID up-or-down)
  )

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
    (action-restrict-check)
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
    (validate-container rules-manager-main-dialog)
    (action-restrict-check))
  
  (define (shift-up-callback e)
    (shift-rule-up)
    ;; cache these edited obj ID and edit-mode
    (define edited-obj-ID (rmgr-get-currently-edited-ID))
    (define curr-edit-mode edit-mode) ;; cache the value of edit-mode at this moment
    (hd-postedit
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
    (hd-postedit
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

;;;; fall through (not used anymore)
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
          (hd-postedit
           undo-manager
           (make-undoable-edit "Set Stop"
                               (lambda () ;; undo
                                 (set-fall #t))
                               (lambda () ;; redo
                                 (set-fall #f)))))
        (begin
          (set-fall #t)
          (hd-postedit
           undo-manager
           (make-undoable-edit "Set Fall"
                               (lambda () ;; undo
                                 (set-fall #f))
                               (lambda () ;; redo
                                 (set-fall #t))))
          )))
  
  ft-button-callback)

(define (fall-cb-callback ruleID)

  ;; set fall through or not for this rule (negation of checkbox value)
  (define (set-fall value :: <boolean>)
    (define rule-obj (get 'rules ruleID))

    (if rule-obj
        (ask rule-obj 'set-fall-through? value))

    ;; pop the rules manager out again
    (rmgr-edit edit-mode (rmgr-get-currently-edited-ID))

    ;; right most button of dialog box is getting pushed out
    ;;(pack-frame rules-manager-main-dialog)
    )
  
  
  (define (ft-checkbox-callback e stop?)
    ;;(define ft-checkbox (get-rule-panel-fall-checkbox (get-rule-panel-for ruleID)))
    ;; alternate button display between "Fall" and "Stop"

    (if stop?
        (begin
          (set-fall #f)
          (hd-postedit
           undo-manager
           (make-undoable-edit "Set Stop"
                               (lambda () ;; undo
                                 (set-fall #t))
                               (lambda () ;; redo
                                 (set-fall #f)))))
        (begin
          (set-fall #t)
          (hd-postedit
           undo-manager
           (make-undoable-edit "Set Fall"
                               (lambda () ;; undo
                                 (set-fall #f))
                               (lambda () ;; redo
                                 (set-fall #t)))))
        ))

  ft-checkbox-callback)

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
  
  (define target-obj #f)
  (cond ((equal? target-type 'link)
         (set! edited-linkID obj-ID)
         (set! target-obj (get 'links obj-ID))
         (define rule-lst (ask target-obj 'rule-lst))
         (map (lambda (ruleID)
                (rmgr-add-rule-panel ruleID)
                ) rule-lst)
         )
        ((equal? target-type 'node)
         (set! edited-nodeID obj-ID)
         (set! target-obj (get 'nodes obj-ID))
         (define rule-lst (ask target-obj 'rule-lst))
         (map (lambda (ruleID)
                (rmgr-add-rule-panel ruleID)
                ) rule-lst)
         ))
  
  (set-dialog-title rules-manager-main-dialog
                   (string-append 
                    (case target-type
                      ((link) "Edit link: ")
                      ((node) "Edit node rules: ")) 
                    (ask target-obj 'name)))
  
  ;; update the display so deleted rule panel's graphics 
  ;; which are not drawn over do not linger on
  (component-update rmgr-rules-list-panel)
  
  (pack-frame rules-manager-main-dialog))

;; edit the rules of this object, target-type ('link 'node)
(define (rmgr-edit target-type obj-ID)
  (display "rmgr-edit ")(newline)
  (set! edit-mode target-type)
  
  (set-component-enabled delete-rule-button #f)
  (set-component-enabled rule-edit-button #f)
  (set-component-enabled up-button #f)
  (set-component-enabled down-button #f)
  
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

(define (rmgr-update)
  (map update-rule-panel (rmgr-rule-lst)))

;; target type might be 'link 'node 'doc
;; display the dialog with the list of rules attached to the object (which can be any of the target-type)
(define (rmgr-init)
  (set! rules-manager-main-dialog (make-dialog (get-nodeeditor-frame) "Rule Editor" #f)) ;; debug used to be #t
  
  (set-dialog-resizable rules-manager-main-dialog #f)
  
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
  
  ;; top panel
  (define rules-label (make-label-with-title "Rules (applied in order shown) "))
  (define up-down-label (make-label-with-title "Change rule order: "))
  (define rules-label-panel (make-panel))
  (set! up-button (make-button "Up"))
  (set! down-button (make-button "Down"))
  (add-component rules-label-panel rules-label)
  (add-component rules-label-panel up-down-label)
  (add-component rules-label-panel up-button)
  (add-component rules-label-panel down-button)
  (add-component rules-manager-main-panel rules-label-panel 'border-north)
  
  ;; list of rules
  (set! rmgr-rules-list-panel (make-panel))
  
  (set-container-layout rmgr-rules-list-panel 'vertical)
  
  (set! center-panel (make-scrollpane-with-policy rmgr-rules-list-panel 'always 'needed))
  (set-component-preferred-size center-panel 400 200)
  
  (add-component rules-manager-main-panel center-panel 'border-center)
  
  (component-update rules-manager-main-panel)
  (component-revalidate rules-manager-main-panel)
  
  (component-update (scroll-get-viewport center-panel))
  (component-revalidate (scroll-get-viewport center-panel))
  
  (define rules-dialog-button-panel (make-panel))
  (set-container-layout rules-dialog-button-panel 'horizontal)
  (add-component rules-manager-main-panel rules-dialog-button-panel 'border-south)
  
    ;; rule list buttons
  (set! add-rule-button (make-button "Add Rule"))
  (set! delete-rule-button (make-button "Delete Selected"))
  (set! rule-edit-button (make-button "Edit Rule"))
  (add-component rules-dialog-button-panel add-rule-button)
  (add-component rules-dialog-button-panel delete-rule-button)
  (add-component rules-dialog-button-panel rule-edit-button)
  
  ;; dialog button
  (define rules-dialog-close (make-button "Close"))
  
  ;; pushes the right elements to the right and left to the left (does the left and right justify)
  (add-component rules-dialog-button-panel (create-horizontal-glue))
  (add-component rules-dialog-button-panel rules-dialog-close)
  
  (add-actionlistener up-button 
                      (make-actionlistener shift-up-callback2))
  
  (add-actionlistener down-button 
                      (make-actionlistener shift-down-callback2))
  
  (add-actionlistener add-rule-button 
                      (make-actionlistener add-rule-button-callback))
  
  (add-actionlistener delete-rule-button 
                      (make-actionlistener delete-selected-rule-button-callback))
  
   (add-actionlistener rule-edit-button
                       (make-actionlistener edit-rule-button-callback2))
  
  (add-actionlistener rules-dialog-close
                      (make-actionlistener
                       (lambda (e)
                         (rmgr-close))))
  
  (pack-frame rules-manager-main-dialog)
  )
