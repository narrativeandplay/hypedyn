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

(require "../miscutils.scm") ;; custom-try-catch

(module-export make-undo-manager make-undoableeditlistener add-undoableeditlistener
               undo-manager-add-edit undo-manager-discard-all-edits
               make-compoundundomanager compoundundomanager-beginupdate 
               compoundundomanager-endupdate compoundundomanager-postedit
               compoundundomanager-updatelevel
               compoundundomanager-locked?
               undoable-edit?
               make-undo-action set-associated-redoaction! update-undo-action
               make-redo-action set-associated-undoaction! update-redo-action
               make-undoable-edit 
               ;add-undoable-edit save-point-offset save-point-newedit save-point-undo save-point-redo 
               save-point-reset
               save-point-tracking-init
               get-save-point-offset
               
               <compoundundomanager> ;; export our own simple class
               )

;;
;; undo for jtextpanes, based on Notepad.java example from JDK
;; 

;;
;; generic undo manager
;; 

; make an undo manager
(define (make-undo-manager)
  (<javax.swing.undo.UndoManager>))

; make an undoableEditListener
(define (make-undoableeditlistener mycallback)
  (object (<javax.swing.event.UndoableEditListener>)
    ((undoableEditHappened e :: <javax.swing.event.UndoableEditEvent>) :: <void>
     (mycallback (invoke e 'getEdit)))))

; add an undoableEditListener: currently only for documents
(define (add-undoableeditlistener undoableedit-producer :: <javax.swing.text.AbstractDocument>
                                  undoableedit-listener :: <javax.swing.event.UndoableEditListener>)
  (invoke undoableedit-producer 'addUndoableEditListener undoableedit-listener))

; add an edit to undo manager: adds an edit directly to undo manager, don't use this if
; you're using a compound undo manager
(define (undo-manager-add-edit in-undo-manager :: <javax.swing.undo.UndoManager>
                               in-edit :: <javax.swing.undo.UndoableEdit>)
  (invoke in-undo-manager 'addEdit in-edit))

; discard all edits
(define (undo-manager-discard-all-edits in-undo-manager :: <javax.swing.undo.UndoManager>)
  (invoke in-undo-manager 'discardAllEdits))

;;
;; compound undo manager:
;; keeps track of a series of edits, combines them into a compound edit, and, 
;; when a given condition is satisfied, passes them up to the undo manager.
;; Very loosely based on http://tips4java.wordpress.com/2008/10/27/compound-undo-manager/
;;

(define (make-compoundundomanager)
  (<compoundundomanager>))

; begin a compound edit
(define (compoundundomanager-beginupdate in-undo-manager :: <compoundundomanager>)
  (invoke in-undo-manager 'beginUpdate)
  )

; end a compound edit
;; TODO undo-action and redo-action is updated internally since undo-mgr keeps track of it
;; remove undo-action and redo-action arguments

(define (compoundundomanager-endupdate in-undo-manager :: <compoundundomanager>
                                       undo-action redo-action)
  (invoke in-undo-manager 'endUpdate))

(define (compoundundomanager-updatelevel in-undo-manager :: <compoundundomanager>)
  (invoke in-undo-manager 'getUpdateLevel)
  )

; post an edit: if a compound edit is in progress, will be batched until
; endupdate is called, otherwise will be posted immediately

;; TODO the app (hypedyn) should not set dirty by itself anymore
;; it should just provide the procedure for set dirty and clear dirty
;; this is to prevent an action setting dirty flag after compound
(define (compoundundomanager-postedit in-undo-manager :: <compoundundomanager>
                                      in-edit :: <javax.swing.undo.UndoableEdit>)
  (if (undoable-edit? in-edit)
      (begin
        (invoke in-undo-manager 'postEdit in-edit)
        (update-undo-action (invoke in-undo-manager 'get-undo-action))
        (update-redo-action (invoke in-undo-manager 'get-redo-action))
        ))
  )

(define (compoundundomanager-locked? in-undo-manager :: <compoundundomanager>) :: <boolean>
  (invoke in-undo-manager 'get-lock))

; check if a given event is an undoable edit
(define (undoable-edit? e)
  (javax.swing.undo.UndoableEdit? e))
                        
; class to handle compound edits
(define-simple-class <compoundundomanager> (<javax.swing.undo.UndoManager>)
  ; undoable edit support to keep track of compound edits
  (undo-edit-support :: <javax.swing.undo.UndoableEditSupport>)
  (undo-action :: <undoAction>)
  (redo-action :: <redoAction>)
  (undoing-redoing-lock :: boolean)
  (save-point-offset :: <int>)
  
  ; constructor
  ((*init*)
   ; call superclass constructor
   (invoke-special <javax.swing.undo.UndoManager> (this) '*init*)
    
   ; create an instance of undoable edit support to handle listeners
   (set! undo-edit-support (<javax.swing.undo.UndoableEditSupport>))
   
   (set! undoing-redoing-lock #f)
   
   (set! save-point-offset 0)
   
   ; add ourselves as a listener to our undo edit support class
   (invoke undo-edit-support 'addUndoableEditListener (this)))
  
  ; manual posting of edits, allows tracking of compound edits
  ((postEdit (e :: <javax.swing.undo.UndoableEdit>)) :: <void>

   ;;(format #t "compoundundomanager postEdit: ~a~%~!" e)
   (if (and (undoable-edit? e)
            (not undoing-redoing-lock))
       (begin
         (invoke undo-edit-support 'postEdit e)
         (save-point-newedit)
         (update-actions)
         )
;         (begin
;           (display "[postedit ignored] '")(display (invoke e 'getPresentationName))(display "'")(newline)
;           (display "  Either not undoable-edit? ")(display (not (undoable-edit? e)))(newline)
;           (display "  Or undoing-redoing-lock ")(display undoing-redoing-lock)(newline)
;           )
         ))
     
  ; begin batching of edits
  ((beginUpdate) :: <void>
   ;; can only beginUpdate if not undoing and redoing, if not it is ignored
   (if (not undoing-redoing-lock)
       (invoke undo-edit-support 'beginUpdate)))
  
  ; end batching of edits
  ((endUpdate) :: <void>
   ;; can only endUpdate if not undoing and redoing, if not it is ignored
   (if (not undoing-redoing-lock)
       (begin
         (invoke undo-edit-support 'endUpdate)
         (save-point-newedit)
         (update-actions))))
  
  ((getUpdateLevel)
   (invoke undo-edit-support 'getUpdateLevel))
  
  ((set-undo-redo-actions in-undo-action in-redo-action)
   (set! undo-action in-undo-action)
   (set! redo-action in-redo-action)
   )
  
  ((update-actions) :: <void>
   (if (not (void? undo-action))
       (update-undo-action undo-action))
           
   (if (not (void? redo-action))
       (update-redo-action redo-action))
   )
  
  ;; check locks to make sure that you do not post new undoableedit
  ;; in the middle of an undo or redo
  ((get-lock) :: <boolean>
   undoing-redoing-lock)
  
  ((lock) 
   (set! undoing-redoing-lock #t))
  ((unlock) 
   (set! undoing-redoing-lock #f))
  
  ((undo)
   (lock)
   (display "undoing LOCK")(newline)
   (invoke-special <javax.swing.undo.UndoManager> (this) 'undo)
   (display "undoing UNLOCK")(newline)
   (unlock)
   )
  ((redo)
   (lock)
   (display "redoing LOCK")(newline)
   (invoke-special <javax.swing.undo.UndoManager> (this) 'redo)
   (display "redoing UNLOCK")(newline)
   (unlock)
   )
  
  ((get-undo-action) :: <undoAction>
   undo-action)
  
  ((get-redo-action) :: <redoAction>
   redo-action)
  
  ((get-save-point-offset) :: <int>
   save-point-offset)
  
  ((save-point-newedit) :: <void>
   ;; if simple postedit or end of compound then increment
   (if (= (invoke undo-edit-support 'getUpdateLevel) 0)
       (set! save-point-offset (+ save-point-offset 1)))

   ;; always set dirty since it is a new edit
   (set-dirty!))
  
  ((save-point-redo) :: <void>
   (set! save-point-offset (+ save-point-offset 1))
   (if (= save-point-offset 0)
       (clear-dirty!)
       (set-dirty!))
   )
  
  ((save-point-undo) :: <void>
   (set! save-point-offset (- save-point-offset 1))
   (if (= save-point-offset 0)
       (clear-dirty!)
       (set-dirty!))
   )
  
  ((save-point-reset) :: <void>
   (set! save-point-offset 0))
  )

;;
;; undo actions
;; 

; make an undo action
(define (make-undo-action u :: <javax.swing.undo.UndoManager>)
  (<undoAction> u))

; set undo action's associated redo action
(define (set-associated-redoaction! in-undo-action :: <undoAction> in-associated-redo-action :: <redoAction>)
  (invoke in-undo-action 'setRedoAction in-associated-redo-action))

; update undo action
(define (update-undo-action in-undo-action :: <undoAction>)
  (invoke in-undo-action 'update))

; undo action class
(define-simple-class <undoAction> (<javax.swing.AbstractAction>)
  ; undo manager
  (undo-mgr :: <javax.swing.undo.UndoManager>)
  
  ; associated redo action
  (associatedRedoAction :: <redoAction>)
  ;(associatedRedoAction); :: <redoAction>)
  
  ; constructor - create the action
  ((*init* (u :: <javax.swing.undo.UndoManager>))
   (begin
     ; call superclass constructor
     (invoke-special <javax.swing.AbstractAction> (this) '*init* "Undo")
     
     ; set accelerator key
     (let ((the-char (as <int> (char->integer #\Z))))
       (invoke (this) 'putValue
               <javax.swing.Action>:ACCELERATOR_KEY
               (<javax.swing.KeyStroke>:getKeyStroke the-char
                                                     (invoke (<java.awt.Toolkit>:getDefaultToolkit)
                                                             'getMenuShortcutKeyMask))))
     
     ; remember undo manager
     (set! undo-mgr u)
     
     ; and enable
     (invoke (this) 'setEnabled #f)))
  
  ; set redo action
  ;((setRedoAction in-redoAction :: <redoAction>) :: <void>
  ((setRedoAction in-redoAction) :: <void>
   (set! associatedRedoAction in-redoAction))
  
  ; perform the undo
  ((actionPerformed e :: <java.awt.event.ActionEvent>) :: <void>
   (define cached-presentation-name (invoke undo-mgr 'getUndoPresentationName))
   (try-catch
       (begin
         (invoke undo-mgr 'undo)
         (display "undoing ")(newline)
         (save-point-undo undo-mgr))
     (ex <javax.swing.undo.CannotUndoException>
         (begin
           (display cached-presentation-name)(display " [undo failed]")(newline)
           (display (*:toString ex))(newline)
           (*:printStackTrace ex))))
   (invoke (this) 'update)
   (if associatedRedoAction
       (invoke associatedRedoAction 'update)))

  ; update the action
  ((update) access: 'protected :: <void>
   (begin
     (if (invoke undo-mgr 'canUndo)
         (begin
           (invoke (this) 'setEnabled #t)
           (invoke (this) 'putValue <javax.swing.Action>:NAME (invoke undo-mgr 'getUndoPresentationName)))
         (begin
           (invoke (this) 'setEnabled #f)
           (invoke (this) 'putValue <javax.swing.Action>:NAME "Undo")))))
)

; make an redo action
(define (make-redo-action r :: <javax.swing.undo.UndoManager>)
  (<redoAction> r))

; set redo action's associated undo action
(define (set-associated-undoaction! in-redo-action :: <redoAction> in-associated-undo-action :: <undoAction>)
  (invoke in-redo-action 'setUndoAction in-associated-undo-action))

; update redo action
(define (update-redo-action in-redo-action :: <redoAction>)
  (invoke in-redo-action 'update))

; redo action class
(define-simple-class <redoAction> (<javax.swing.AbstractAction>)
  ; undo manager
  (undo-mgr :: <javax.swing.undo.UndoManager>)
  
  ; associated undo action
  (associatedUndoAction :: <undoAction>)
  
  ; constructor - create the action
  ((*init* (u :: <javax.swing.undo.UndoManager>))
   (begin
     ; call superclass constructor
     (invoke-special <javax.swing.AbstractAction> (this) '*init* "Redo")
     
     ; set accelerator key
     (let ((the-char (as <int> (char->integer #\Y))))
       (invoke (this) 'putValue
               <javax.swing.Action>:ACCELERATOR_KEY
               (<javax.swing.KeyStroke>:getKeyStroke the-char
                                                     (invoke (<java.awt.Toolkit>:getDefaultToolkit)
                                                             'getMenuShortcutKeyMask))))

   ; remember undo manager
     (set! undo-mgr u)
     
     ; and disable
     (invoke (this) 'setEnabled #f)))
  
  ; set undo action
  ((setUndoAction in-undoAction :: <undoAction>) :: <void>
   (set! associatedUndoAction in-undoAction))
  
  ; perform the redo
  ((actionPerformed e :: <java.awt.event.ActionEvent>) :: <void>
   (define cached-presentation-name (invoke undo-mgr 'getUndoPresentationName))
   (begin
     (try-catch
         (begin
           ;(save-point-redo)
           (invoke undo-mgr 'redo)
           (display "redoing ")(newline)
           (save-point-redo undo-mgr)
           )
       (ex <javax.swing.undo.CannotRedoException>
           (begin
             (display cached-presentation-name)(display " [redo failed]")(newline)
             (display (*:toString ex))(newline)
             (*:printStackTrace ex))))
     (invoke (this) 'update)
     (if associatedUndoAction
         (invoke associatedUndoAction 'update))))

  ; update the action
  ((update) access: 'protected :: <void>
   (begin
     (if (invoke undo-mgr 'canRedo)
         (begin
           (invoke (this) 'setEnabled #t)
           (invoke (this) 'putValue <javax.swing.Action>:NAME (invoke undo-mgr 'getRedoPresentationName)))
         (begin
           (invoke (this) 'setEnabled #f)
           (invoke (this) 'putValue <javax.swing.Action>:NAME "Redo")))))
)

; make an undoableedit
(define (make-undoable-edit in-presentation-name :: <string>
                            in-undo-action :: <procedure>
                            in-redo-action :: <procedure>)
  (<genericundoableedit> in-presentation-name in-undo-action in-redo-action))

; a generic undoable edit:
; consists of 
; 1) a human-readable name for the action, to be shown in the "undo" menu item
; 2) an "undo" procedure, which reverts the system to the state before the action, and
; 3) a "redo" procedure, which repeats the action when called after an "undo"
(define-simple-class <genericundoableedit> (<javax.swing.undo.AbstractUndoableEdit>)
  ; presentation name
  (presentation-name :: <string>)
  
  ; undo procedure
  (undo-procedure :: <procedure>)
  
  ; redo procedure
  (redo-procedure :: <procedure>)
  
  ; constructor - create the undoable edit
  ((*init* (in-presentation-name :: <string>) (in-undo-procedure :: <procedure>) (in-redo-procedure :: <procedure>))
   (begin
     ; call superclass constructor
     (invoke-special <javax.swing.undo.AbstractUndoableEdit> (this) '*init*)
     
     ; store the presentation name
     (set! presentation-name in-presentation-name)
     
     ; store the undo and redo procedures
     (set! undo-procedure in-undo-procedure)
     (set! redo-procedure in-redo-procedure)))
  
  ; get presentation name
  ((getPresentationName) :: <java.lang.String>
   presentation-name)
  
  ; undo
  ((undo) throws: (<javax.swing.undo.CannotUndoException>) :: <void>
   (begin
     ;(format #t "undo:~a ~a~%~!" presentation-name undo-procedure)
     (invoke-special <javax.swing.undo.AbstractUndoableEdit> (this) 'undo)
     
     ; call the undo procedure
     (if (procedure? undo-procedure)
         (begin
           (display "[undo] ")(display presentation-name)(newline)
           (undo-procedure))
         (begin
           (display "[undo] not procedure not undoing ")(newline)
           )
         )
     ))

  ; redo
  ((redo) :: <void>
   (begin
     ;(format #t "redo:~a ~a~%~!" presentation-name redo-procedure)
     
     (invoke-special <javax.swing.undo.AbstractUndoableEdit> (this) 'redo)
     
     ; call the redo procedure
     (if (procedure? redo-procedure)
         (begin
           (display "[redo] ")(display presentation-name)(newline)
           (redo-procedure))
         (begin
           (display "[redo] not procedure not redoing ")(newline)
           )
         )
     ))
  )

;;;; Save point tracking 

(define local-set-dirty! #f)
(define local-clear-dirty! #f)


;; since undo manager does changes to the other component which keeps dirty flags
;; it has to set them too, procedures for setting and clearing are kept here and called
(define-private (set-dirty!) 
  ;(display "set dirty! ")(newline)
  (if (procedure? local-set-dirty!)
      (local-set-dirty!)))

(define-private (clear-dirty!)
  ;(display "clear dirty! ")(newline)
  (if (procedure? local-clear-dirty!)
      (local-clear-dirty!)))

(define (save-point-tracking-init set-dirty-proc clear-dirty-proc)
  (set! local-set-dirty! set-dirty-proc)
  (set! local-clear-dirty! clear-dirty-proc)
  )

;; count number of edits from save-point to determine whether document is dirty (needs saving)
;; this should be called everytime an undoable action is posted so it is called inside every postedit call

;; handling for compound edits
;; if it is a compound edit, update level should be more than 0 when doing postedit
;; in that case only do save-point-outwards when endupdate of compound edits are called

;; assumption: assume that compound edits are not empty so every pair of compound edit beginupdate
;; and endupdate are counted as an edit 


;; TODO save point offset should be an attribute of the undo manager

(define (get-save-point-offset undo-mgr)
  (invoke undo-mgr 'get-save-point-offset))

(define (save-point-newedit undo-mgr) ;; new edits (compound or simple)
  (invoke undo-mgr 'save-point-newedit))

;; only time to clear dirty is when save-point-offset is 0 
;; both redo and undo can trigger it
(define (save-point-redo undo-mgr)
  (invoke undo-mgr 'save-point-redo))

(define (save-point-undo undo-mgr)
  (invoke undo-mgr 'save-point-undo))

;; called by clear-dirty! as save/load fileio needs
;; to start a new save point offset count
(define (save-point-reset undo-mgr)
  (invoke undo-mgr 'save-point-reset))


