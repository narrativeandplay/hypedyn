;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2015
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

; list view consisting of objects

(require "datatable.scm") ;; get
(require "objects.scm") ;; ask obj-put
(require "../kawa/ui/listbox.scm")
(require "../kawa/ui/scrollpane.scm")
(require "../kawa/color.scm")

(module-export make-object-listview)
(module-static 'init-run)

; make an object list view, displays a list of "named object" objects
; parameters:
; selectobject-callback: procedure to call on selection, takes ID of selected object, null if unselecting
; is-sorted: boolean, if true then list will be sorted by sort-key
; tableID: symbol, the ID of the table that stores the objects in the data table
; sort-key: symbol, the ID of the field to be sorted on
; sort-proc: procedure, returns true if first argument is >= second argument
(define (make-object-listview selectobject-callback is-sorted tableID sort-key sort-proc)
  (let* ((parent-obj (new-make-named-object "object-listview"))
         (this-obj (new-object parent-obj))
         (thelist #f)
         (thecomponent #f))

    ; initialize
    (define (init)
      (set! thelist (make-listbox))
      (add-listbox-selectListener thelist
                                  (make-listselectionlistener handle-selectedobject))
      (set! thecomponent (make-scrollpane thelist))
      (set-listbox-single-selection-mode thelist)
      ;(display "listview init called ")(newline)
      ;(display "thecomponent ")(display thecomponent)(newline)
      )

    ; handle selection
    (define (handle-selectedobject e)
      (if thelist
          (let ((objectID (get-listbox-selected-data thelist)))
            (if (procedure? selectobject-callback)
                (selectobject-callback objectID)))))

    ; select object in list
    (define (select-object objectID)
      (if (not (eq? '() objectID))
          (begin
            (show-selection objectID 0 (get-listbox-size thelist))
            (make-selection-visible))))
    
    ; deselect object
    (define (deselect-object)
      (clear-listbox-selection thelist))

    ; make the current selection visible in object list
    (define (make-selection-visible)
      (let* ((selindex (get-listbox-selected-index thelist))
             (firstvis (get-listbox-first-visible-index thelist))
             (lastvis (get-listbox-last-visible-index thelist)))
        (if (or (<= selindex firstvis)
                (>= selindex lastvis))
            (set-listbox-visible-index thelist
                                       (get-listbox-selected-index thelist)))))

    ; show selection in object list based on ID
    (define (show-selection objectID thisindex listlen)
      (if (< thisindex listlen)
          (let ((thisID (get-listbox-data-at-index thelist thisindex)))
            (if (= thisID objectID)
                (set-listbox-selected-index thelist thisindex)
                (show-selection objectID (+ thisindex 1) listlen)))))

    ; add an object to the list
    (define (add-object new-objectID in-displayname)
      ; helper to search through and add in correct position, ordered by sort-key
      (define (add-object-helper in-index in-size in-displayname in-ID in-sort-value)
        (if (= in-index in-size)
            ; at end, so add
            (begin
              (add-listbox-element thelist in-displayname in-ID))
            ; otherwise, see if this is where to insert
            (let* ((this-objectID (get-listbox-data-at-index thelist in-index))
                   (this-object (get tableID this-objectID)))
              (if this-object
                  ; got the object successfully
                  (let ((this-sort-value (ask this-object sort-key)))
                    ; see if this is the correct position
                    (if (sort-proc this-sort-value in-sort-value)
                        ; this is the position, so insert
                        (begin
                          (insert-listbox-element-at thelist in-displayname in-ID in-index))
                        ; otherwise recursively call ourselves with next index
                        (add-object-helper (+ in-index 1) in-size in-displayname in-ID in-sort-value)))))))

      ; retrieve the object
      (let ((this-object (get tableID new-objectID)))
        (if this-object
            (if (and is-sorted (procedure? sort-proc))
                ; get the object sort value
                (let ((this-sort-value (ask this-object sort-key)))
                  ; add the object - use helper to add in correct order
                  (add-object-helper 0 (get-listbox-size thelist) in-displayname new-objectID this-sort-value))
                ; otherwise add at the end
                (add-listbox-element thelist in-displayname new-objectID)))))
    
    ; enable/disable all objects
    (define (set-all-objects-enabled in-flag)
      (define (set-all-objects-enabled-helper in-index in-size in-flag)
        (if (< in-index in-size)
            ; not at end, so set and recurse
            (begin
              (set-listbox-element-enabled thelist in-index in-flag)
              (set-all-objects-enabled-helper (+ 1 in-index) in-size in-flag))))

      ; recursively set all objects
      (set-all-objects-enabled-helper 0
                                      (get-listbox-size thelist)
                                      in-flag))

    ; enable/disable given object by ID - assume there's only one, sets first
    (define (set-object-enabled in-objectID in-flag)
      (define (set-object-enabled-helper in-index in-size in-objectID in-flag)
        (if (< in-index in-size)
            ; not at end, so check if this is it
            (if (eq? in-objectID (get-listbox-data-at-index thelist in-index))
                ; this is it, so set and stop
                (set-listbox-element-enabled thelist in-index in-flag)
                ; otherwise keep going
                (set-object-enabled-helper (+ 1 in-index) in-size in-objectID in-flag))))

      ; recursively scan through all objects until we find ours
      (set-object-enabled-helper 0
                                 (get-listbox-size thelist)
                                 in-objectID
                                 in-flag))

    ; set colour for given object by ID - assume there's only one, sets first
    ; colour here is a colour list as defined in ui-kawa, or #f to clear
    (define (set-object-colour in-objectID in-colour)
      (define (set-object-colour-helper in-index in-size in-objectID in-colour)
        (if (< in-index in-size)
            ; not at end, so check if this is it
            (if (eq? in-objectID (get-listbox-data-at-index thelist in-index))
                ; this is it, so set and stop
                (if in-colour
                    (set-listbox-element-colour thelist in-index (make-colour-from-list in-colour))
                    (clear-listbox-element-colour thelist in-index))
                ; otherwise keep going
                (set-object-colour-helper (+ 1 in-index) in-size in-objectID in-colour))))

      ; recursively scan through all objects until we find ours
      (set-object-colour-helper 0
                                  (get-listbox-size thelist)
                                  in-objectID
                                  in-colour))
    
    ; message handling
;    (lambda (message)
;      (cond ((eq? message 'init)
;             (lambda (self)
;               (init)))
    (obj-put this-obj 'init 
             (lambda (self) (init)))
;            ((eq? message 'get-component)
;             (lambda (self)
;               thecomponent))
    (obj-put this-obj 'get-component
             (lambda (self) thecomponent))
;            ((eq? message 'get-list)
;             (lambda (self)
;               thelist))
    (obj-put this-obj 'get-list
             (lambda (self) thelist))
;            ((eq? message 'get-listlen)
;             (lambda (self)
;               (get-listbox-size thelist)))
    (obj-put this-obj 'get-listlen
             (lambda (self)
               (get-listbox-size thelist)))
;            ((eq? message 'select-object)
;             (lambda (self objectID)
;               (select-object objectID)))
    (obj-put this-obj 'select-object
             (lambda (self objectID)
               (select-object objectID)))
;            ((eq? message 'deselect-object)
;             (lambda (self)
;               (deselect-object)))
    (obj-put this-obj 'deselect-object
             (lambda (self)
               (deselect-object)))
;            ((eq? message 'add-object)
;             (lambda (self new-objectID name)
;               (add-object new-objectID name)))
    (obj-put this-obj 'add-object
             (lambda (self new-objectID name)
               (add-object new-objectID name)))
;            ((eq? message 'set-all-objects-enabled)
;             (lambda (self in-flag)
;               (set-all-objects-enabled in-flag)))
    (obj-put this-obj 'set-all-objects-enabled
             (lambda (self in-flag)
               (set-all-objects-enabled in-flag)))
;            ((eq? message 'set-object-enabled)
;             (lambda (self in-objectID in-flag)
;               (set-object-enabled in-objectID in-flag)))
    (obj-put this-obj 'set-object-enabled
             (lambda (self in-objectID in-flag)
               (set-object-enabled in-objectID in-flag)))
;            ((eq? message 'set-object-colour)
;             (lambda (self in-objectID in-colour)
;               (set-object-colour in-objectID in-colour)))
    (obj-put this-obj 'set-object-colour
             (lambda (self in-objectID in-colour)
               (set-object-colour in-objectID in-colour)))
;            ((eq? message 'clear)
;             (lambda (self)
;               (if thelist
;                   (set-listbox-clear thelist))))
    (obj-put this-obj 'clear
             (lambda (self)
               (if thelist
                   (set-listbox-clear thelist))))
;            (else (get-method parent-obj message))))
    this-obj))