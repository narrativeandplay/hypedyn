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

; inspector: tool to inspect "objects" in the "world"
; intended to be a general tool, initially implemented for logo+turtle

; inspectors expect an "inspectable" object to be an object created using
; objects.scm, and containing a 'get-inspectable-fields message which
; returns a list of lists in the format (list field-name field-type field-label)

; objects can be registered as "inspectable" by calling register-as-inspectable

(begin
  (require "objects.scm")
  (require "datatable.scm") ;; get-list, dirty?
  (require "window-menu.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/container.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/frame.scm")
  (require "../kawa/ui/menu.scm")
  (require "../kawa/strings.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/ui/dialog.scm")
  (require "../kawa/ui/panel.scm")
  (require "../kawa/ui/label.scm")
  (require "../kawa/ui/scrollpane.scm")
  (require "../kawa/ui/toolbar.scm")
  (require "../kawa/ui/button.scm")
  (require 'regex)
  )

;; export
(module-export create-inspectors-window
               show-inspectors-window
               create-inspector
               update-inspectors
               close-inspectors
               register-as-inspectable
               get-inspectable-objects
               ask-add-inspector)
(module-static 'init-run)

;; inspector frame, holds all inspectors
(define inspectors-window #f)
(define inspectors-panel #f)

; menus
(define inspector-menubar #f)
(define inspector-menu #f)
(define inspector-menuitem #f)

; show inspectors menu item in parent UI (if any)
(define show-inspectors-menuitem #f)

; toolbar
(define inspector-toolbar-panel #f)
(define inspector-toolbar-button-back #f)

; create inspectors window
(define (create-inspectors-window in-show-inspectors-menuitem)
  (set! inspectors-window (make-window "Inspectors"))
  (set! inspectors-panel (make-panel))
  (add-component inspectors-window (make-scrollpane inspectors-panel))
  (set-container-layout inspectors-panel 'vertical)
  (add-windowlistener inspectors-window (make-windowlistener
                                        inspector-window-opened
                                        inspector-window-closing
                                        inspector-window-closed
                                        inspector-window-iconified
                                        inspector-window-deiconified
                                        inspector-window-activated
                                        inspector-window-deactivated))

  ; add menu items
  
  ; menu bar
  (set! inspector-menubar (make-menu-bar))
  (add-menu-bar inspectors-window inspector-menubar)

  ; inspector menu and menu item
  (set! inspector-menu (make-menu "Inspectors"))
  (add-component inspector-menubar inspector-menu)
  (set! inspector-menuitem (make-menu-item "Add inspector"))
  (add-component inspector-menu inspector-menuitem)
  (add-actionlistener inspector-menuitem 
                      (make-actionlistener (lambda (a)
                                             (ask-add-inspector inspectors-window))))

  ; window menu
  (add-component inspector-menubar (add-window-menu inspectors-window))
  
  ; remember associated menu item
  (set! show-inspectors-menuitem in-show-inspectors-menuitem)

  ;; Add a horizontal panel to the frame, with centering, to hold toolbar buttons
  (set! inspector-toolbar-panel (make-toolbar "Toolbar"))
  (set-toolbar-floatable inspector-toolbar-panel #f)
  (add-component inspectors-window inspector-toolbar-panel 'border-north)

  ;; button to add an inspector
  (set! inspector-toolbar-button-back (make-button  "Add inspector"))
  (add-component inspector-toolbar-panel inspector-toolbar-button-back)
  (add-actionlistener inspector-toolbar-button-back
                      (make-actionlistener
                       (lambda (a) (ask-add-inspector inspectors-window))))
  
  (pack-frame inspectors-window))

; show/hide inspectors window
(define (show-inspectors-window flag)
  (if (and
       inspectors-window
       (not (equal? (get-component-visible inspectors-window) flag)))
      (begin
        (set-component-visible inspectors-window flag)
        (if flag
            (add-to-window-menu inspectors-window "Inspectors")
            (on-close-cleanup)))))

; close the window
(define (close-inspectors-window)
  (if inspectors-window
      (begin
        (set-component-visible inspectors-window #f)
        (dispose-frame inspectors-window)
        (set! inspectors-window #f))))

; on-close cleanup
(define (on-close-cleanup)
  (remove-from-window-menu inspectors-window)
  (if show-inspectors-menuitem
      (set-checkbox-menu-item show-inspectors-menuitem #f)))

; window callback
(define (inspector-window-opened o)
  (format #t "inspector-window-opened~%~!"))
(define (inspector-window-closing o)
  (format #t "inspector-window-closing~%~!")
  (on-close-cleanup))
(define (inspector-window-closed o)
  (format #t "inspector-window-closed~%~!")
  (on-close-cleanup))
(define (inspector-window-iconified o)
  (format #t "inspector-window-iconified~%~!"))
(define (inspector-window-deiconified o)
  (format #t "inspector-window-deiconified~%~!"))
(define (inspector-window-activated o)
  (format #t "inspector-window-activated~%~!"))
(define (inspector-window-deactivated o)
  (format #t "inspector-window-deactivated~%~!"))

; make an inspector
; currently supports inspection of strings, numbers and booleans
(define (make-object-inspector name object parent-frame)
  (let* ((named-obj (new-make-named-object name))
         (this-obj (new-object named-obj))
         (the-inspector-panel (make-panel))
         (the-inspector-label (make-label))
         (fields '()))
    ; handle messages
;    (lambda (message)
;      (cond
       ; initialize the inspector
;       ((eq? message 'init)
;        (lambda (self)
;          (begin
;            ; set layout
;            (set-container-layout the-inspector-panel 'flow 'left)
;            ; add this inspector's panel to the inspectors window
;            (add-component the-inspector-panel the-inspector-label)
;            (set-text the-inspector-label (fstring->string name))
;            (add-component inspectors-panel the-inspector-panel)
;            (pack-frame inspectors-window))))
       (obj-put this-obj 'init
        (lambda (self)
          (begin
            ; set layout
            (set-container-layout the-inspector-panel 'flow 'left)
            ; add this inspector's panel to the inspectors window
            (add-component the-inspector-panel the-inspector-label)
            (set-text the-inspector-label (to-string name))
            (add-component inspectors-panel the-inspector-panel)
            (pack-frame inspectors-window))))
       ; show the inspector
;       ((eq? message 'show)  (lambda (self flag)
;                               ;(set-component-visible local-frame flag)
;                               (show-inspectors-window #t)))
    (obj-put this-obj 'show (lambda (self flag)
                               (show-inspectors-window #t)))
       ; add a field to the inspector
;       ((eq? message 'add-field)
;        (lambda (self new-field-name new-field-type new-field-label)
;          (let* ((thepanel (make-panel))
;                 (the-field (make-textfield "" 15))
;                 (the-field-name (make-label)))
;            (add-component thepanel the-field-name 'border-west)
;            (set-text the-field-name new-field-label)
;            (add-component thepanel the-field 'border-east)
;            (add-component the-inspector-panel thepanel)
;            (set! fields
;                  (append fields
;                          (list (list new-field-name
;                                      new-field-type
;                                      the-field))))
;            ;(set-frame-location local-frame 350 150)
;            ;(set-component-visible local-frame #t)
;            ;(pack-frame local-frame)
;            (pack-frame inspectors-window)
;            (ask self 'update-field new-field-name))))
    (obj-put this-obj 'add-field
        (lambda (self new-field-name new-field-type new-field-label)
          (let* ((thepanel (make-panel))
                 (the-field (make-textfield "" 15))
                 (the-field-name (make-label)))
            (add-component thepanel the-field-name 'border-west)
            (set-text the-field-name new-field-label)
            (add-component thepanel the-field 'border-east)
            (add-component the-inspector-panel thepanel)
            (set! fields
                  (append fields
                          (list (list new-field-name
                                      new-field-type
                                      the-field))))
            ;(set-frame-location local-frame 350 150)
            ;(set-component-visible local-frame #t)
            ;(pack-frame local-frame)
            (pack-frame inspectors-window)
            (ask self 'update-field new-field-name))))
    ; update a field in the inspector
    (obj-put this-obj 'update-field
        (lambda (self this-field-name)
          (if (not (null? object))
              (let* ((the-value (ask object this-field-name))
                     (the-field (assoc this-field-name fields)))
                (if (not (null? the-field))
                    (let ((the-field-type (cadr the-field))
                          (the-field-text (caddr the-field)))
                      (set-text the-field-text
                                (cond ((eq? the-field-type 'number)
                                       (number->string the-value))
                                      ((eq? the-field-type 'boolean)
                                       (if the-value
                                           "#t"
                                           "#f"))
                                      ((eq? the-field-type 'string)
                                       the-value)))))))))
    ; update all fields
    (obj-put this-obj 'update-all-fields
        (lambda (self)
          (map (lambda (f)
                 (ask self 'update-field (car f)))
               fields)))
    ; close the inspector
    (obj-put this-obj 'close
        (lambda (self)
          (if the-inspector-panel
              (begin
                (remove-component inspectors-panel the-inspector-panel)
                (pack-frame inspectors-window)))))
    ; pass to super-class
    this-obj))

;create an inspector
(define (create-inspector name object parent-frame)
  (let ((new-inspector (make-object-inspector name object parent-frame))
        (inspectable-fields (ask object 'get-inspectable-fields)))
    (if inspectable-fields
        (map (lambda (f)
               (let ((field-name (car f))
                     (field-type (cadr f))
                     (field-label (caddr f)))
                 (ask new-inspector 'init)
                 (ask new-inspector 'add-field field-name field-type field-label)));
             inspectable-fields))
        (put 'inspectors name new-inspector)))


; update inspectors
(define (update-inspectors)
  (if (dirty?)
      (let ((the-inspectors (get-list 'inspectors)))
        (if the-inspectors
            (map (lambda (i)
                   (let ((insp (cdr i)))
                     (ask insp 'update-all-fields)))
                 the-inspectors)))))

; close all inspectors
(define (close-inspectors)
  ; close all the inspectors
  (let ((the-inspectors (get-list 'inspectors)))
    (if the-inspectors
        (map (lambda (i)
               (let ((insp (cdr i)))
                 (ask insp 'close)))
             the-inspectors)))
  
  ; and hide inspector window
  ;(show-inspectors-window #f)
  )

; register an object as inspectable;
; the type passed in here should be the symbol used to store the 
; objects in the data table in objects.scm
(define (register-as-inspectable object-type)
  ;(format #t "register-as-inspectable ~a~%~!" object-type)
  (put 'inspectable-objects object-type (symbol->string object-type)))

; get list of inspectable objects
(define (get-inspectable-objects)
  (get-list 'inspectable-objects))
         
;;
;; UI for creating an inspector
;;

; list of inspectable objects
(define inspectable-objects #f)

; ask the user for an inspectable object and add an inspector for that object
(define (ask-add-inspector parent-frame)
  ; get list of inspectable objects
  (set! inspectable-objects (get-inspectable-objects))
  (if inspectable-objects
      ; ask user to choose the type of object
      (let*((inspectable-objects-strings
             (map (lambda (o) (cdr o)) inspectable-objects))
            (object-type
             (make-input-dialogbox-multiple parent-frame 
                                            "Object type" "Add inspector" 
                                            inspectable-objects-strings)))
        (if (not (eq? #!null object-type))
            ; ask user to choose specific object
            (let* ((object-type-symbol (string->symbol object-type))
                   (object-type-string object-type)
                   (object-list (get-list object-type-symbol)))
              (if object-list
                  (let* ((object-list-IDs
                          (map (lambda (o)
                                 (car o))
                               object-list))
                         (object-list-strings
                          (map (lambda (o)
                                 (ask (cdr o) 'name))
                               object-list))
                         (object-list-displaylist
                          (map (lambda (o)
                                 (string-append (number->string (car o)) ":" (ask (cdr o) 'name)))
                               object-list))
                         (object-ID (make-input-dialogbox-multiple 
                                     parent-frame
                                     "Object ID" "Add inspector" object-list-displaylist)))
                    (if (not (eq? #!null object-ID))
                        ; now create the inspector
                        (add-inspector object-ID object-type-symbol parent-frame)))))))
  (make-confirm-dialogbox parent-frame 1 "Sorry, no inspectable objects")))

; create and add the UI for an inspector
(define (add-inspector object-ID object-type-symbol parent-frame)
  (let*(( split-object-ID (regex-split ":" object-ID))
        ( object-ID-string (cadr split-object-ID))
        ( object-ID-num (string->number (car split-object-ID)))
        ( the-object (get object-type-symbol object-ID-num))
        ( inspector-name (string-append (symbol->string object-type-symbol) ": " object-ID-string
                                        " (" (number->string object-ID-num) ")")))
    (if the-object
        (begin
          (create-inspector inspector-name the-object parent-frame)
          (let(( the-inspector (get 'inspectors inspector-name)))
            (ask the-inspector 'show #t))))))