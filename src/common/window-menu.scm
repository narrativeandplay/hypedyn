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

;;
;; window menu
;; 

; requires
(require "../kawa/ui/menu.scm")
(require "../kawa/ui/component.scm")
(require "../kawa/ui/events.scm")
(require "../kawa/ui/frame.scm")
(require 'list-lib)

; exports
(module-export add-window-menu remove-window-menu add-to-window-menu remove-from-window-menu)

; keep track of all windows that are using a window menu
; each entry consists of (cons this-window (cons the-window-menu list-of-remove-functions))
; where list-of-remove-functions is a list of (this-window the-remove-function)
(define window-menu-window-list '())

; when you want a window menu in your window, call add-window-menu
; this creates a menu, and populates it with all the current windows
; also remembers the menu in an update list
; returns the menu, to be added to your window's menu bar
(define (add-window-menu in-window)
  (let ((the-window-menu (make-menu "Window"))
        (the-window-menu-minimize (make-menu-item "Minimize")) ; minimize - mac only?
        (the-window-menu-zoom (make-menu-item "Zoom")) ; zoom - mac only?
        (the-window-menu-separator (make-separator))) ; separator

    ; add menu items to menu
    (add-component the-window-menu the-window-menu-minimize)
    (add-component the-window-menu the-window-menu-zoom)
    (add-component the-window-menu the-window-menu-separator)

    ; window menu actions
    (add-actionlistener the-window-menu-minimize (make-actionlistener (lambda (source)
                                                                        (set-frame-iconified in-window))))
    (add-actionlistener the-window-menu-zoom (make-actionlistener (lambda (source)
                                                                    (toggle-frame-maximized in-window))))

    ; add all the current windows to this window menu
    (let ((list-of-remove-functions '()))
      (map (lambda (w)
             (let ((this-window (car w))
                   (this-title (cdr w)))
               ; create the menu item - returns the remove function
               (let ((the-remove-function (add-window-menu-item in-window the-window-menu this-window this-title)))
                 ; now store the remove function so can use it later
                 (set! list-of-remove-functions (cons (cons this-window the-remove-function) list-of-remove-functions)))))
           window-menu-list)

      ; remember the window and the list of remove functions
      (set! window-menu-window-list (cons (cons in-window (cons the-window-menu list-of-remove-functions)) window-menu-window-list)))

    ; and return the menu
    the-window-menu))
  
; when you destroy the window, should remove the window menu;
; this removes the menu from the update list
(define (remove-window-menu in-window)
  (alist-delete! in-window window-menu-window-list))

; keep track of all windows in the window menu
(define window-menu-list '())

; add the given window to the window menu, using in-title as the label
(define (add-to-window-menu in-window in-title)
  ; add to window list
  (set! window-menu-list (cons (cons in-window in-title) window-menu-list))
  ;;(format #t "add-to-window-menu: ~a~%~!" in-title)

  ; then, for each registered window menu, add to the menu
  (let ((new-window-menu-window-list '()))
    (map (lambda (w)
           ;(format #t "w: ~a~%~!" w)
           (let ((this-window (car w))
                 (this-window-menu (cadr w))
                 (this-list-of-remove-functions (cddr w)))
             ; create the menu item - returns the remove function
             (let ((the-remove-function (add-window-menu-item this-window this-window-menu in-window in-title)))
               ; now need to store the remove function so can use it later - assume no duplicates
               (set! new-window-menu-window-list
                     (cons (cons this-window 
                                 (cons this-window-menu
                                       (cons (cons in-window the-remove-function)
                                             this-list-of-remove-functions)))
                           new-window-menu-window-list)))))
         window-menu-window-list)
    
    ; and remember the new window-menu-window list
    (set! window-menu-window-list new-window-menu-window-list)))

; add window menu item
(define (add-window-menu-item this-window this-window-menu in-window in-title)
  (let ((this-menu-item (make-checkbox-menu-item in-title)))
    ; add the menu item to window menu
    (add-component this-window-menu this-menu-item)

    ; add action to show this window, and make sure the menu stays selected
    (add-actionlistener this-menu-item (make-actionlistener
                                        (lambda (source)
                                          (bring-to-front in-window)
                                          (set-checkbox-menu-item source #t))))

    ; add a window listener to update menu item
    (let ((the-window-listener (make-windowlistener
                                (lambda (o) 'ok) ; opened
                                (lambda (o) 'ok) ;closing
                                (lambda (o) 'ok) ; closed
                                (lambda (o) 'ok) ; iconified
                                (lambda (o) 'ok) ; deiconified
                                (lambda (o)
                                  ; window activated, so check the menu item
                                  (set-checkbox-menu-item this-menu-item #t))
                                (lambda (o)
                                  ; window deactivated, so uncheck the menu item
                                  (set-checkbox-menu-item this-menu-item #f)))))
      (add-windowlistener in-window the-window-listener)

      ; return the remove function
      (lambda () (remove-window-menu-item in-window this-window-menu this-menu-item the-window-listener)))))

; remove a window menu item from the window menu
(define (remove-window-menu-item in-window in-window-menu in-menu-item in-windowlistener)
  (remove-component in-window-menu in-menu-item)
  (remove-windowlistener in-window in-windowlistener))

; When the window is removed from the window menus, need to cycle through all the window menus,
; find the remove function for the window being removed, and call it
; this suggests that the list of functions be indexed first by windows with window menu (map through this), and
; then by window (assoc this and call it, then delete the entry)
; when a window menu is removed (rare), then the entire list entry is removed
(define (remove-from-window-menu in-window)
  (let ((new-window-menu-window-list '()))
    (map (lambda (w)
           ;(format #t "w: ~a~%~!" w)
           (let ((this-window (car w))
                 (this-window-menu (cadr w))
                 (this-remove-list (cddr w)))
             ; now get the remove function
             (let* ((this-entry (assoc in-window this-remove-list))
                    (the-remove-function (if this-entry (cdr this-entry) #f)))
               ;(format #t "the-remove-function: ~a~%~!" the-remove-function)
               ; call it
               (if (procedure? the-remove-function)
                   (the-remove-function))
               ; and remove it
               (alist-delete! in-window this-remove-list)
               ; and reconstruct the list of window menus
               (set! new-window-menu-window-list
                     (cons (cons this-window 
                                 (cons this-window-menu
                                       this-remove-list))
                           new-window-menu-window-list)))))
         window-menu-window-list)
    (set! window-menu-window-list new-window-menu-window-list)))