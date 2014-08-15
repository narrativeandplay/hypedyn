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

(module-export make-menu-bar make-menu make-menu-item make-menu-item-from-action
               set-menu-item-accelerator set-menu-mnemonic set-menu-item-mnemonic
               set-menu-item-text
               make-checkbox-menu-item set-checkbox-menu-item get-checkbox-menu-item make-separator make-popup-menu
               set-menuitem-component add-menu-bar add-menuitem add-menuitem-at remove-menuitem-at get-menu-component-count
               add-menu-action make-cut-menuitem make-copy-menuitem make-paste-menuitem
               make-custom-cut-action make-custom-copy-action make-custom-paste-action)

;;
;; menus
;; 

; make a menu bar
(define (make-menu-bar)
  (<javax.swing.JMenuBar>))

; make a menu
(define (make-menu title :: <java.lang.String>)
  (<javax.swing.JMenu> title))

; make a menu item
(define (make-menu-item title :: <java.lang.String>)
  (<javax.swing.JMenuItem> title))

; make a menu item from action
(define (make-menu-item-from-action in-action :: <javax.swing.AbstractAction>)
  (<javax.swing.JMenuItem> in-action))

; set menuitem accelerator
; parameters:
; item: the menu item
; accelerator-key: char, the accelerator key
; args: additional modifiers (shift, ctrl, etc), if any
; (note - additional modifiers not yet implemented)
(define (set-menu-item-accelerator item :: <javax.swing.JMenuItem>
                                   accelerator-key
                                   . args)
  (let ((the-char (as <int> (char->integer accelerator-key))))
    (invoke item
            'setAccelerator
            (<javax.swing.KeyStroke>:getKeyStroke the-char
                                                  (invoke (<java.awt.Toolkit>:getDefaultToolkit)
                                                          'getMenuShortcutKeyMask)))))

; set menu mnemonic (windows)
(define (set-menu-mnemonic menu :: <javax.swing.JMenu>
                           mnemonic-key)
  (let ((the-char (as <int> (char->integer mnemonic-key))))
    (invoke menu
            'setMnemonic
            the-char)))

; set menuitem mnemonic (windows)
(define (set-menu-item-mnemonic item :: <javax.swing.JMenuItem>
                                mnemonic-key)
  (let ((the-char (as <int> (char->integer mnemonic-key))))
    (invoke item
            'setMnemonic
            the-char)))

; set the text on a menu item
(define (set-menu-item-text item :: <javax.swing.JMenuItem>
                            title :: <string>)
  (invoke item 'setText title))

;make checkbox menuitem
(define (make-checkbox-menu-item title :: <java.lang.String>)
  (<javax.swing.JCheckBoxMenuItem> title))

;set state of check box menu item
(define (set-checkbox-menu-item item :: <javax.swing.JCheckBoxMenuItem> flag)
  (invoke item 'setState flag))

;get state of check box menu item
(define (get-checkbox-menu-item item :: <javax.swing.JCheckBoxMenuItem>)
  (invoke item 'getState))

;make seperator
(define (make-separator)
  (<javax.swing.JSeparator>))

; make a popup menu
(define (make-popup-menu)
  (<javax.swing.JPopupMenu>))

;set properties for the menuitem component
; enable flag
(define (set-menuitem-component in-item :: <javax.swing.JComponent> enable)
  (invoke in-item 'setEnabled enable))

(define (add-menu-bar frame :: <javax.swing.JFrame>
                      bar :: <javax.swing.JMenuBar>)
  (invoke (as <javax.swing.JFrame> frame) 'setJMenuBar bar))

; add a menu item
(define (add-menuitem in-menu :: <javax.swing.JMenu>
                      in-item :: <javax.swing.JComponent>)
  (invoke in-menu 'add in-item))

; add a menu item at a specific index
(define (add-menuitem-at in-menu :: <javax.swing.JMenu>
                         in-item :: <javax.swing.JComponent>
                         in-index :: <int>)
  (invoke in-menu 'add in-item in-index))

; remove a menu item from a specific index
(define (remove-menuitem-at in-menu :: <javax.swing.JMenu>
                            in-index :: <int>)
  (if (and (>= in-index 0)
           (< in-index (get-menu-component-count in-menu)))
      (invoke in-menu 'remove in-index)))

; get number of components in a menu
(define (get-menu-component-count in-menu :: <javax.swing.JMenu>) :: <int>
  (invoke in-menu 'get-menu-component-count))

; add an action to a menu
(define (add-menu-action in-menu :: <javax.swing.JMenu> in-action :: <javax.swing.AbstractAction>)
  (invoke in-menu 'add in-action))

;; 
;; cut/copy/paste
;; 

;; TODO: need to hook into the jtextcomponent's keymappings

(define (make-cut-menuitem #!optional pre-action post-action)
  (define cut-menuitem (make-menu-item-from-action
                        (if pre-action
                            (make-custom-cut-action pre-action post-action)
                            (<javax.swing.text.DefaultEditorKit$CutAction>))))
  (set-menu-item-text cut-menuitem "Cut")
  (set-menu-item-mnemonic cut-menuitem #\X)
  (set-menu-item-accelerator cut-menuitem #\X)
  cut-menuitem)
  
(define (make-copy-menuitem #!optional pre-action post-action)
  (define copy-menuitem (make-menu-item-from-action
                         (if pre-action
                             (make-custom-copy-action pre-action post-action)
                             (<javax.swing.text.DefaultEditorKit$CopyAction>))))
  (set-menu-item-text copy-menuitem "Copy")
  (set-menu-item-mnemonic copy-menuitem #\C)
  (set-menu-item-accelerator copy-menuitem #\C)
  copy-menuitem)
  
(define (make-paste-menuitem #!optional pre-action post-action)
  (define paste-menuitem (make-menu-item-from-action
                          (if pre-action
                              (make-custom-paste-action pre-action post-action)
                              (<javax.swing.text.DefaultEditorKit$PasteAction>))))
  (set-menu-item-text paste-menuitem "Paste")
  (set-menu-item-mnemonic paste-menuitem #\V)
  (set-menu-item-accelerator paste-menuitem #\V)
  paste-menuitem)

;;
;; custom cut/copy/paste action handlers
;; 

; note: if in-action returns #t, then perform the default action

(define (make-custom-cut-action pre-action post-action)
  (object (<javax.swing.text.DefaultEditorKit$CutAction>)
    ((actionPerformed e :: <java.awt.event.ActionEvent>) :: <void>
     (format #t "*** custom cut action~%~!")
     (let ((do-default #t))
       (if (procedure? pre-action)
           (set! do-default (pre-action e)))
       (if do-default
           (invoke-special <javax.swing.text.DefaultEditorKit$CutAction> (this) 'actionPerformed e))
       (if (procedure? post-action)
           (post-action e))))))

(define (make-custom-copy-action pre-action post-action)
  (object (<javax.swing.text.DefaultEditorKit$CopyAction>)
    ((actionPerformed e :: <java.awt.event.ActionEvent>) :: <void>
     (format #t "*** custom copy action~%~!")
     (let ((do-default #t))
       (if (procedure? pre-action)
           (set! do-default (pre-action e)))
       (if do-default
           (invoke-special <javax.swing.text.DefaultEditorKit$CopyAction> (this) 'actionPerformed e))
       (if (procedure? post-action)
           (post-action e))))))

(define (make-custom-paste-action pre-action post-action)
  (object (<javax.swing.text.DefaultEditorKit$PasteAction>)
    ((actionPerformed e :: <java.awt.event.ActionEvent>) :: <void>
     (format #t "*** custom paste action~%~!")
     (let ((do-default #t))
       (if (procedure? pre-action)
           (set! do-default (pre-action e)))
       (if do-default
           (invoke-special <javax.swing.text.DefaultEditorKit$PasteAction> (this) 'actionPerformed e))
       (if (procedure? post-action)
           (post-action e))))))


