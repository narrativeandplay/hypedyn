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

;(require "../arrays.scm") ;; array-to-list

(module-export make-combobox make-choices make-comboboxwithdata make-sortedcomboboxwithdata make-sorted-combobox
               get-combobox-selecteditem get-combobox-selectedindex get-combobox-item-at get-comboboxwithdata-selecteddata get-comboboxwithdata-data-at
               set-combobox-clear set-comboboxwithdata-clear set-combobox-selection set-combobox-selection-object set-comboboxwithdata-selection-bydata
               add-combobox-item add-combobox-string 
               add-comboboxwithdata-item insert-comboboxwithdata-item-at 
               add-comboboxwithdata-string insert-comboboxwithdata-string-at
               remove-combobox-item
               remove-combobox-string
               get-combobox-item-count
               create-combobox-string-item)
               
;;
;; combo box
;; 

; make a combo box
; pass in parameters for each item to appear in the combobox
; ie. (make-combobox "first" "second" "third")
;; need to apply the fix that comes from create-combobox-string-item here
(define (make-combobox #!rest args) ;; just have args as list   ;(args :: <Object[]>))
  (define combobox (<javax.swing.JComboBox>))
  (map (lambda (item)
         (add-combobox-string combobox item))
       args)
  ;(<javax.swing.JComboBox> args)
  combobox)

; think this is a leftover from drscheme - alex
(define (make-choices)
  (<javax.swing.JComboBox>))

; combobox with data
(define-simple-class <comboboxwithdata> (<javax.swing.JComboBox>)
  (user-data-list :: <java.util.Vector>)
  ((*init*) #!void
   (begin
     (set! user-data-list (<java.util.Vector>))))
  ((addItem txt :: <object> data) :: <void>
   (begin
     (invoke (as <javax.swing.JComboBox> (this)) 'addItem txt)
     (invoke user-data-list 'add data)))
  ((insertItemAt txt:: <object> data in-index :: <int>)
   (begin
     (invoke (this) 'insertItemAt txt in-index)
     (invoke user-data-list 'insertElementAt data in-index)))
  ((get-data-at in-index :: <int>)
   (if (< in-index (invoke user-data-list 'size))
       (invoke user-data-list 'elementAt in-index)
       '())) ; returns null if out of range, should it throw an exception instead?
  ((get-selected-data)
   (invoke (this) 'get-data-at (invoke (this) 'getSelectedIndex)))
  ((set-selected-data data :: <object>)
   ; helper to recursively search for position to add at
   (define (set-selection-helper data i size)
     ; are we at end?
     (if (>= i size)
         ; at end, so not found (throw exception?)
         'ok
         ; otherwise, if equal, insert here
         (if (equal? data (invoke (this) 'get-data-at i))
             ; equal, so select
             (invoke (this) 'setSelectedIndex i)
             ; otherwise, continue recursively
             (set-selection-helper data (+ i 1) size))))

   ; get the size of the combobox, and call helper function
   (let ((size (invoke (as <javax.swing.ComboBoxModel>
                           (invoke (as <javax.swing.JComboBox> (this)) 'getModel))
                       'getSize)))
     (set-selection-helper data 0 size)))
  ((clear)
   (begin
     (invoke (as <javax.swing.JComboBox> (this)) 'removeAllItems)
     (invoke user-data-list 'clear))))

;; uses the string to sort the entries so that we're ensured a certain order
;; disadvantage i can see is we cannot specify that order
(define-simple-class <sortedcomboboxwithdata> (<comboboxwithdata>)
  ; add an item in order
  ((addItem txt :: <object> data) :: <void>
   ; helper to recursively search for position to add at
   (define (add-item-helper txt :: <object> data i size)
     ; are we at end?
     (if (>= i size)
         ; at end, so just add to end
         (invoke-special <comboboxwithdata> (this) 'addItem txt data)
         ; otherwise, if txt <= list(i), insert here
         (if (<= (invoke (invoke (as <java.lang.Object> txt) 'toString)
                         'compareToIgnoreCase
                         (invoke (as <java.lang.Object>
                                     (invoke (as <javax.swing.JComboBox> (this)) 'getItemAt i)) 'toString))
                 0)
             ; smaller, so insert here
             (invoke (this) 'insertItemAt txt data i)
             ; otherwise, continue recursively
             (add-item-helper txt data (+ i 1) size))))

   ; get the size of the combobox, and call helper function
   (let ((size (invoke (as <javax.swing.ComboBoxModel>
                           (invoke (as <javax.swing.JComboBox> (this)) 'getModel))
                       'getSize)))
     (add-item-helper txt data 0 size))))

;; analogous to <sortedcomboboxwithdata>
;; TODO: possibly borth sortedcombobox and sortedcomboboxwithdata 
;; can share code since they are almost identical
(define-simple-class <sortedcombobox> (<javax.swing.JComboBox>)
  ; add an item in order
  ((addItem txt :: <object>) :: <void>
   ; helper to recursively search for position to add at
   (define (add-item-helper txt :: <object> i size)
     ; are we at end?
     (if (>= i size)
         ; at end, so just add to end
         (invoke-special <javax.swing.JComboBox> (this) 'addItem txt)
         ; otherwise, if txt <= list(i), insert here
         (if (<= (invoke (invoke (as <java.lang.Object> txt) 'toString)
                         'compareToIgnoreCase
                         (invoke (as <java.lang.Object>
                                     (invoke (as <javax.swing.JComboBox> (this)) 'getItemAt i)) 'toString))
                 0)
             ; smaller, so insert here
             (invoke (this) 'insertItemAt txt i)
             ; otherwise, continue recursively
             (add-item-helper txt (+ i 1) size))))

   ; get the size of the combobox, and call helper function
   (let ((size (invoke (as <javax.swing.ComboBoxModel>
                           (invoke (as <javax.swing.JComboBox> (this)) 'getModel))
                       'getSize)))
     (add-item-helper txt 0 size))))

;; make sorted combobox 
(define (make-sorted-combobox)
  (<sortedcombobox>))

; make a combo box with data
(define (make-comboboxwithdata)
  (<comboboxwithdata>))

; make a sorted combo box with data
(define (make-sortedcomboboxwithdata)
  (<sortedcomboboxwithdata>))

; get currently selected item in combobox
; Note: can also be used for comboboxwithdata
(define (get-combobox-selecteditem in-combo :: <javax.swing.JComboBox>)
  (invoke in-combo 'getSelectedItem))

; get currently selected index in combobox
; Note: can also be used for comboboxwithdata
(define (get-combobox-selectedindex in-combo :: <javax.swing.JComboBox>) :: <integer>
  (invoke in-combo 'getSelectedIndex))

; get specified item in combobox
; Note: can also be used for comboboxwithdata
(define (get-combobox-item-at in-combo :: <javax.swing.JComboBox>
                              in-index :: <int>)
  (invoke in-combo 'getItemAt in-index))

; get currently selected data in comboboxwithdata
(define (get-comboboxwithdata-selecteddata in-combo :: <comboboxwithdata>)
  (invoke in-combo 'getSelectedData))

; get specified data in comboboxwithdata
(define (get-comboboxwithdata-data-at in-combo :: <comboboxwithdata>
                                      in-index :: <int>)
  (invoke in-combo 'getDataAt in-index))

; clear combobox
;; TOFIX bug when using it with add-combobox-string
;; the equals object seems to be bringing in a null object when it deletes using this
(define (set-combobox-clear in-combo :: <javax.swing.JComboBox>)
  (invoke in-combo 'removeAllItems))

; clear comboboxwithdata
(define (set-comboboxwithdata-clear in-combo :: <comboboxwithdata>)
  (invoke in-combo 'clear))

; set combobox selection
(define (set-combobox-selection  in-combo :: <javax.swing.JComboBox>
                                 in-sel :: <int>)
  (invoke in-combo 'setSelectedIndex in-sel))

; set combobox selection by object
(define (set-combobox-selection-object  in-combo :: <javax.swing.JComboBox>
                                        in-sel :: <java.lang.Object>)
  (invoke in-combo 'setSelectedItem in-sel))

; set comboboxwithdata selection by data
(define (set-comboboxwithdata-selection-bydata  in-combo :: <comboboxwithdata>
                                                in-data :: <object>)
  (invoke in-combo 'setSelectedData in-data))

;; remove a selected object from the combobox
(define (remove-combobox-item in-combo :: <javax.swing.JComboBox>
                              to-remove :: <java.lang.Object>)
  (invoke in-combo 'remove-item to-remove)
  )

; add an item to a combobox 
; (should we stop using this? since add-combobox-string solves duplicate object problem?)
(define (add-combobox-item in-combo :: <javax.swing.JComboBox>
                           in-item :: <object>)
  (invoke in-combo 'addItem in-item))

;; need this because to do a set-combobox-selection-object 
;; on a combobox that does add-combobox-string 
;; we need to create such an object
;; for the selction to work
(define (create-combobox-string-item str-obj :: <java.lang.Object>)
  
  (define-class combobox-string-item (java.lang.Object)
    ((toString) :: <java.lang.String>
     str-obj)
    ; this seems to be the cause of bug #22, why was this added? (13 Jan 2012 by Teong Leong) - alex
    ;((equals obj :: <java.lang.Object>)
    ; (display "equals ")(newline)
    ; (display "obj ")(display obj)(newline)
    ; (display "str-obj ")(display str-obj)(newline)
    ; (if (equal? #!null obj)
    ;     #f
    ;     (equal? (invoke obj 'to-string) (invoke str-obj 'to-string))))
    )
  
  (make combobox-string-item))
  
; add a string to a combobox
; this is a workaround to avoid duplicate display of item
; selections, see http://download.oracle.com/javase/1,5.0/docs/api/javax/swing/JComboBox.html#addItem%28java.lang.Object%29
; TOFIX: perhaps this is just the different types of string causing the bug
(define (add-combobox-string in-combo :: <javax.swing.JComboBox>
                           in-item :: <object>)
  (invoke in-combo 'addItem 
;          (begin
;            (define-class myObject (java.lang.Object)
;              ((toString) :: <java.lang.String>
;               in-item))
;            (make myObject))
          (create-combobox-string-item in-item)))

(define (remove-combobox-string in-combobox :: <javax.swing.JComboBox>
                                in-item :: <object>)
  (invoke in-combobox 'remove-item (create-combobox-string-item in-item)))

; add an item to a combobox with data
(define (add-comboboxwithdata-item in-combo :: <comboboxwithdata>
                                   in-item :: <object>
                                   in-data :: <object>)
  (invoke in-combo 'addItem in-item in-data))

; insert an item to a combobox with data at a specific index
(define (insert-comboboxwithdata-item-at in-combo :: <comboboxwithdata>
                                         in-item :: <object>
                                         in-data :: <object>
                                         in-index :: <int>)
  (invoke in-combo 'insertItemAt in-item in-data in-index))

; add a string to a combobox with data
; this is a workaround to avoid duplicate display of item
; selections, see http://download.oracle.com/javase/1,5.0/docs/api/javax/swing/JComboBox.html#addItem%28java.lang.Object%29
(define (add-comboboxwithdata-string in-combo :: <comboboxwithdata>
                                     in-item :: <String>
                                     in-data :: <object>)
  (invoke in-combo 'addItem
;          (begin
;            (define-class myObject (java.lang.Object)
;              ((toString) :: <java.lang.String>
;               in-item))
;            (make myObject))
          (create-combobox-string-item in-item)
          in-data))

; as above but insert at a specific index
(define (insert-comboboxwithdata-string-at in-combo :: <comboboxwithdata>
                                           in-item :: <String>
                                           in-data :: <object>
                                           in-index :: <int>)
  (invoke in-combo 'insertItemAt
;          (begin
;            (define-class myObject2 (java.lang.Object)
;              ((toString) :: <java.lang.String>
;               in-item))
;            (make myObject2))
          (create-combobox-string-item in-item)
          in-data
          in-index))

; call clear from combobox
; duplicates set-comboboxwithdata-clear, removed - alex
;;(define (clear-comboboxwithdata cb :: <comboboxwithdata>)
;;  (invoke cb 'clear))

(define (get-combobox-item-count in-combo :: <javax.swing.JComboBox>)
  (invoke in-combo 'getItemCount))
