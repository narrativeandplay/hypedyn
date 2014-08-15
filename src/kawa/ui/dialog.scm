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

(require "../arrays.scm") ; for list-to-array

; make-input-dialogbox-custom needed to construct the dialog
(require "button.scm")
(require "panel.scm")
(require "component.scm") ;; add-component, set-component-enabled
(require "frame.scm") ;; pack-frame
(require "text.scm") ;;make-textfield
(require "label.scm") ;; make-label
(require "container.scm") ;; set-container-layout
(require "events.scm") ;; make-actionlistener
(require "../system.scm") ;; is-windows?

(module-export make-confirm-dialogbox make-input-dialogbox make-message-box make-input-dialogbox-multiple make-dialog
               get-dialog-content-pane make-dialog-noparent make-dialog-noparent-modal make-error-dialog
               set-dialog-title set-dialog-resizable dispose-dialog set-dialog-dont-exit
               set-dialog-content-pane make-message-optionpane
               
               make-input-dialogbox-custom
               make-input-dialogbox-check-empty
               get-dialog-root-pane
               )
               
;;
;; dialog box
;; 

;make a dialog box
;takes the number of choices (1, 2, or 3), message to show, and optional window title
;returns 1 on yes and 3 on no and 2 on cancel
(define (make-confirm-dialogbox frame :: <javax.swing.JFrame> choices dialog . args)
  (let ((option (invoke-static <javax.swing.JOptionPane>
                               'showConfirmDialog frame dialog (if (pair? args) (car args) "Message")
                               (cond ((eq? choices 4) <javax.swing.JOptionPane>:OK_CANCEL_OPTION)
                                     ((eq? choices 3) <javax.swing.JOptionPane>:YES_NO_CANCEL_OPTION)
                                     ((eq? choices 2) <javax.swing.JOptionPane>:YES_NO_OPTION)
                                     ((eq? choices 1) <javax.swing.JOptionPane>:DEFAULT_OPTION)))))
    (define retval
      (cond ((= option <javax.swing.JOptionPane>:YES_OPTION) 1)
            ((= option <javax.swing.JOptionPane>:CANCEL_OPTION) 2)
            ((= option <javax.swing.JOptionPane>:NO_OPTION) 3)
            ;; these arent used anywhere yet
            ((= option <javax.swing.JOptionPane>:CLOSED_OPTION) 4)
            ((= option <javax.swing.JOptionPane>:OK_OPTION) 5)
            ))
    (display "make-confirm-dialogbox returning ")(display retval)(newline)
    retval
    ))

;make an input dialog box
;takes some default text, the message to show, and the window title
;returns the string entered, or null if cancelled
(define (make-input-dialogbox frame :: <javax.swing.JFrame> deftxt msg title)
  (invoke-static <javax.swing.JOptionPane>
                 'showInputDialog frame msg
                 title
                 <javax.swing.JOptionPane>:PLAIN_MESSAGE
                 #!null
                 #!null
                 deftxt
                 ))

;; checks for empty text
;; returns !#null on cancel
;; returns text if pressed ok with non empty text
;; pop error dialog if pressed ok with empty text
(define (make-input-dialogbox-check-empty frame :: <javax.swing.JFrame> deftxt msg title)
  (define input-txt #f)
  ;; create a dialog box to get input
  (set! input-txt
        (invoke-static <javax.swing.JOptionPane>
                       'showInputDialog frame msg
                       title
                       <javax.swing.JOptionPane>:PLAIN_MESSAGE
                       #!null
                       #!null
                       deftxt
                       ))
  ;; check return
  (cond ((equal? input-txt "")
         ;; show error message
         (invoke-static <javax.swing.JOptionPane>
                        'showMessageDialog
                        #!null
                        "Input text cannot be empty")
         ;; recurse this function (until we get non empty string)
         (set! input-txt (make-input-dialogbox-check-empty frame deftxt msg title)))
        (else ;; return the text if its not "" (even if it is #!null means cancel)
         input-txt))
  input-txt
  )

; show a message
(define (make-message-box frame :: <javax.swing.JFrame> msg title)
  (invoke-static <javax.swing.JOptionPane>
                 'showMessageDialog frame msg title
                 <javax.swing.JOptionPane>:INFORMATION_MESSAGE))

;input dialog box with multiple input list
(define (make-input-dialogbox-multiple frame :: <javax.swing.JFrame> msg title choices)
  (let(( choice-array (list-to-array choices)))
    ;(format #t "choice-array: ~a~%~!" choice-array)
    (invoke-static <javax.swing.JOptionPane>
                   'showInputDialog frame msg title
                   <javax.swing.JOptionPane>:PLAIN_MESSAGE
                   (<javax.swing.ImageIcon>)
                   choice-array
                   (choice-array 0))))

;; disable ok button if text field is empty
;; returns #!null if cancel pressed
;; returns text field content on ok pressed (garanteed non empty string)
(define (make-input-dialogbox-custom parent :: <java.awt.Frame>
                                     deftxt :: <string>
                                     title :: <String>
                                     msg :: <String>) :: <java.lang.String>
  (define input-txt #f)
  (define main-dialog
    (make-dialog parent title #t))

  ;; main panel
  (define dialog-panel (make-panel))
  ;; containing panels for components
  (define dialog-label-panel (make-panel))
  (define dialog-tf-panel (make-panel))
  (define dialog-buttons-panel (make-panel))
  
  ;; components
  (define dialog-label (make-label-with-title msg))
  (define dialog-textfield (make-textfield deftxt 16))
  (define dialog-ok (make-button "   OK   "))
  (define dialog-cancel (make-button " Cancel "))

  ;; listeners procedures
  (define (ok-button-proc e)
    (set-component-visible main-dialog #f) ;; hide dialog
    (set! input-txt (get-text dialog-textfield)))
  
  (define (cancel-button-proc e)
    (set-component-visible main-dialog #f)
    (set! input-txt #!null)) ;; return #!null
  
  (define (tf-empty-disable-ok e)
    (if (equal? (get-text dialog-textfield) "")
        (set-component-enabled dialog-ok #f)
        (set-component-enabled dialog-ok #t)))
  
  (set-container-layout dialog-panel 'vertical)
  
  ;; add the respective listeners
  (add-actionlistener dialog-ok (make-actionlistener ok-button-proc))
  (add-actionlistener dialog-cancel (make-actionlistener cancel-button-proc))
  (add-caretlistener dialog-textfield (make-caretlistener tf-empty-disable-ok))
  (let ((root-pane (get-dialog-root-pane main-dialog)))
    (invoke (as <javax.swing.JRootPane> root-pane) 'setDefaultButton dialog-ok))
  
  ;; add main panel into content panel
  (add-component (get-dialog-content-pane main-dialog) dialog-panel)

  ;; add all the ui into their respective panel
  (add-component dialog-tf-panel dialog-textfield)
  (set-container-layout dialog-buttons-panel 'flow 'right)
  (if (is-windows?)
      (begin
        (add-component dialog-buttons-panel dialog-ok)
        (add-component dialog-buttons-panel dialog-cancel))
      (begin
        (add-component dialog-buttons-panel dialog-cancel)
        (add-component dialog-buttons-panel dialog-ok)))
  (set-container-layout dialog-label-panel 'flow 'left)
  (add-component dialog-label-panel dialog-label)

  ;; add all the panels into main panel 
  (add-component dialog-panel dialog-label-panel)
  (add-component dialog-panel dialog-tf-panel)
  (add-component dialog-panel dialog-buttons-panel)
  
  ;; ok button initial enabled depends on dialog-textfield's content
  (tf-empty-disable-ok #f)
  
  ; interesting setting resizable after set-component-visible actually does nothing
  ; it must be done before which means such properties are initialized during the 
  ; set-component-visible phase
  (pack-frame main-dialog)
  (set-dialog-resizable main-dialog #f)
  (center-frame-in-parent main-dialog parent)
  (set-component-visible main-dialog #t)
  
  ;; most importantly return input-txt
  input-txt
  )

; make a generic main-dialogwith a parent
(define (make-dialog parent :: <java.awt.Frame>;:: <java.awt.Frame>
                     title :: <java.lang.String>
                     modal :: <boolean>) :: <javax.swing.JDialog>
  (<javax.swing.JDialog> parent title modal))

; get content pane of a dialog
(define (get-dialog-content-pane the-dialog :: <javax.swing.JDialog>)
  (invoke the-dialog 'getContentPane))

; make a generic modeless dialog with no parent
(define (make-dialog-noparent title :: <String>)
  (let ((mydialog :: <javax.swing.JDialog> (<javax.swing.JDialog>)))
    (invoke mydialog 'setTitle title)
    mydialog))

; make a generic modal dialog with no parent
(define (make-dialog-noparent-modal title :: <String>)
  (<javax.swing.JDialog> (as <java.awt.Window> #!null) title <java.awt.Dialog$ModalityType>:APPLICATION_MODAL))

; message or error popup
(define (make-error-dialog parent :: <javax.swing.JFrame>
                           title :: <String>
                           message :: <String>)
  (invoke-static <javax.swing.JOptionPane> 'showMessageDialog parent message title <javax.swing.JOptionPane>:ERROR_MESSAGE))

; set a dialog title
(define (set-dialog-title dialog :: <javax.swing.JDialog>
                          in-title :: <String>)
  (invoke (as <java.awt.Dialog> dialog) 'setTitle (as <java.lang.String> in-title)))

; set whether a dialog is resizable
(define (set-dialog-resizable dialog :: <javax.swing.JDialog>
                              in-flag :: <boolean>)
  (invoke (as <java.awt.Dialog> dialog) 'setResizable in-flag))

;dispose a dialog
(define (dispose-dialog dlg :: <javax.swing.JDialog>)
  (invoke dlg 'dispose))

; don't close when user tries to close
(define (set-dialog-dont-exit frame :: <javax.swing.JDialog>)
  (invoke frame 'setDefaultCloseOperation <javax.swing.JDialog>:DO_NOTHING_ON_CLOSE))

; set content pane
(define (set-dialog-content-pane dialog :: <javax.swing.JDialog>
                                 content-pane :: <java.awt.Container>)
  (invoke dialog 'setContentPane content-pane))

;;
;; separate option panes to allow for customization of dialogs
;; 

; make a message option pane
(define (make-message-optionpane msg) :: <javax.swing.JOptionPane>
    (<javax.swing.JOptionPane> msg <javax.swing.JOptionPane>:INFORMATION_MESSAGE))
  
; get the root pane
(define (get-dialog-root-pane dialog :: <javax.swing.JDialog>) :: <javax.swing.JRootPane>
  (invoke dialog 'getRootPane))