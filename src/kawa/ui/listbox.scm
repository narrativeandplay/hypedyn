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

; listbox
(module-export make-listbox make-listselectionlistener
               get-listbox-selected-index set-listbox-selected-index
               clear-listbox-selection
               set-listbox-single-selection-mode set-listbox-single-interval-selection-mode set-listbox-multiple-interval-selection-mode
               get-listbox-selected-value get-listbox-selected-data
               get-listbox-data-at-index
               set-listbox-visible-index get-listbox-first-visible-index get-listbox-last-visible-index
               get-listbox-size
               set-listbox-clear
               get-listbox-event-is-adjusting
               add-listbox-element
               insert-listbox-element-at
               add-listbox-selectListener
               remove-listbox-element
               set-listbox-selected-value
               set-listbox-element-enabled get-listbox-element-enabled
               set-listbox-element-colour clear-listbox-element-colour get-listbox-element-colour)
(module-static 'init-run)
;
; subclass DefaultListSelectionModel to enable us to disable individual elements in the list
; (should be an inner class? - alex)
;

(define-simple-class <disableLSM> (<javax.swing.DefaultListSelectionModel>)
  ; vector to keep track of enabled state of each element
  (enabled-list :: <java.util.Vector>)
  
  ; constructor - create the vector
  ((*init*) #!void
   (begin
     (set! enabled-list (<java.util.Vector>))))
  
  ; could the next three be done by overriding insertIndexInterval and removeIndexInterval?

  ; an element was added to end of list, so add element to vector
  ((addElement in-flag :: <boolean>) :: <void>
   (begin
     (invoke enabled-list 'add in-flag)))
  
  ; an element was inserted in list, so add element to vector
  ((insertElementAt in-flag :: <boolean> in-index :: <int>) :: <void>
   (begin
     (invoke enabled-list 'add in-index in-flag)))
  
  ; an element was removed from list, so remove element from vector
  ((removeElementAt in-index :: <int>) :: <void>
   (begin
     (invoke enabled-list 'removeElementAt in-index)))
  
  ; return whether element is enabled
  ((get-element-enabled in-index :: <int>) :: <boolean>
   (begin
     (if (< in-index (invoke enabled-list 'size))
         (invoke enabled-list 'elementAt in-index)
         #f)))
  
  ; set enabled state of element
  ((set-element-enabled in-index :: <int> in-flag :: <boolean>) :: <void>
   (begin
     (if (< in-index (invoke enabled-list 'size))
         (invoke enabled-list 'setElementAt in-flag in-index))))

  ; run through all elements in range - if any are disabled then can't set selection
  ((can-set? index0 index1) :: <boolean>
   access: 'private
    (let ((can-set #t)
          (i0 (min index0 index1))
          (i1 (max index0 index1)))
      (do ((i :: <int> i0 (+ i 1)))
          ((> i i1) can-set)
          (set! can-set (and can-set (invoke (this) 'get-element-enabled i))))
      
      ; return whether can set or not
      can-set))
  
  ; clear the enabled list
  ((clear)
   (invoke enabled-list 'clear))
  
  ; override any methods that try to set selection, and check state first
  ; before passing along to superclass to handle in normal fashion
  ; add-selection-interval
  ; set-selection-interval
  ; (what about setAnchorSelectionIndex and setLeadSelectionIndex?)
  ((set-selection-interval index0 :: <int>
                           index1 :: <int>)
     ; set selection if all in range are enabled
     (if (invoke (this) 'can-set? index0 index1)
         (invoke-special <javax.swing.DefaultListSelectionModel> (this) 'setSelectionInterval index0 index1)))

   ((add-selection-interval index0 :: <int>
                           index1 :: <int>)
     ; add selection if all in range are enabled
     (if (invoke (this) 'can-set? index0 index1)
         (invoke-special <javax.swing.DefaultListSelectionModel> (this) 'addSelectionInterval index0 index1)))
  )

; make a listbox
; note: has optional data associated with each entry
; (there may be problems if you selectively add data? - alex)
; note: need to add scrollbars by yourself, as they aren't included
; note: elements can be disabled, but are enabled by default
(define (make-listbox)
  (<mylistbox>))
(define-simple-class <mylistbox> (<javax.swing.JList>)
  (listModel :: <javax.swing.DefaultListModel>) ; the list model - do we need to do this? could we put data in here? - alex
  (selectionModel :: <disableLSM>) ; the selection model, contains info on enabled state of elements
  (user-data-list :: <java.util.Vector>) ; the user data
  (cell-colour-list :: <java.util.Vector>) ; the colour for this cell, by default entries will be #f
  (cell-renderer :: <javax.swing.ListCellRenderer>) ; the custom cell renderer, for coloured text

  ; constructor
  ((*init*) #!void
   (begin
     (set! user-data-list (<java.util.Vector>))
     (set! cell-colour-list (<java.util.Vector>))
     (set! selectionModel (<disableLSM>))
     (set! listModel (<javax.swing.DefaultListModel>))
     (set! cellRenderer (<colourCellRenderer>))
     (invoke (this) 'setModel listModel)
     (invoke (this) 'setSelectionModel selectionModel)
     (invoke (this) 'setCellRenderer cellRenderer)))
  
  ; add/remove elements
  ((addElement txt :: <java.lang.String> data) :: <void>
   (begin
     ; order matters, need the cell-colour-list set before 
     ; adding to listModel or cellRenderer won't be able to find
     ; the colour information
     (invoke cell-colour-list 'add #f)
     (invoke user-data-list 'add data)
     (invoke selectionModel 'addElement #t)
     (invoke listModel 'addElement txt)))
  ((insertElementAt txt :: <java.lang.String> data index) :: <void>
   (begin
     (invoke cell-colour-list 'add index #f)
     (invoke user-data-list 'add index data)
     (invoke selectionModel 'insertElementAt #t index)
     (invoke listModel 'insertElementAt txt index)))
  ((removeElement txt :: <java.lang.String>) :: <void>
   (let ((index (invoke listModel 'indexOf txt)))
     (begin
       (invoke listModel 'removeElement txt)
       (invoke selectionModel 'removeElementAt index)
       (invoke cell-colour-list 'removeElementAt index)
       (invoke user-data-list 'removeElementAt index))))
  
  ; add listener
  ((addListener in-listener :: <javax.swing.event.ListSelectionListener>) :: <void>
   (invoke (this) 'addListSelectionListener in-listener))
  
  ; get
  ((get-list-selected-index) :: <int>
   (invoke (as <javax.swing.JList> (this)) 'getSelectedIndex))
  ((get-list-selected-value) :: <java.lang.String>
   (invoke (as <javax.swing.JList> (this)) 'getSelectedValue))
  ((get-list-selected-data)
   (let ((the-selected-index (get-list-selected-index)))
     (if (> the-selected-index -1)
         (invoke user-data-list 'elementAt the-selected-index)
         '())))
  ((get-list-data-at-index in-index :: <int>)
   (if (< in-index (invoke user-data-list 'size))
       (invoke user-data-list 'elementAt in-index)
       '())) ; returns null if out of range, should it throw an exception instead?

  ; visibility
  ((set-list-visible-index the-index :: <int>)
   (invoke (as <javax.swing.JList> (this)) 'ensureIndexIsVisible the-index))
  ((get-list-first-visible-index) :: <int>
   (invoke (as <javax.swing.JList> (this)) 'getFirstVisibleIndex))
  ((get-list-last-visible-index) :: <int>
   (invoke (as <javax.swing.JList> (this)) 'getLastVisibleIndex))
  
  ; selection
  ((set-list-selected-index in-index :: <int>)
   (invoke (as <javax.swing.JList> (this)) 'setSelectedIndex in-index))
  ((set-list-selected-value txt :: <java.lang.String>
                            scrollThere? :: <boolean>)
   (invoke (as <javax.swing.JList> (this)) 'setSelectedValue txt scrollThere?))
;;  ((set-list-single-selection-mode)
;;   (invoke (as <javax.swing.JList> (this)) 'setSelectionMode selectionModel:SINGLE_SELECTION))
  
  ; clear the listbox
  ((set-list-clear)
   (begin
     (invoke listModel 'clear)
     (invoke selectionModel 'clear)
     (invoke cell-colour-list 'clear)
     (invoke user-data-list 'clear)))
  
  ; get size of listbox
  ((get-list-size) :: <int>
   (invoke user-data-list 'size))
  
  ; selection enabling/disabling
  ((get-list-element-enabled in-index :: <int>) :: <boolean>
   (invoke selectionModel 'get-element-enabled in-index))
  ((set-list-element-enabled in-index :: <int> in-flag :: <boolean>)
   (invoke selectionModel 'set-element-enabled in-index in-flag)
   (invoke (this) 'repaint (invoke (this) 'getBounds)))

  ; coloured elements
  ((get-list-element-colour in-index :: <int>)
   (invoke cell-colour-list 'elementAt in-index))
  ((set-list-element-colour in-index :: <int> in-colour :: <java.awt.Color>)
   (if (< in-index (invoke cell-colour-list 'size))
       (begin
         (invoke cell-colour-list 'setElementAt in-colour in-index)
         (invoke (this) 'repaint (invoke (this) 'getBounds)))))
  ((clear-list-element-colour in-index :: <int>)
   (if (< in-index (invoke cell-colour-list 'size))
       (begin
         (invoke cell-colour-list 'setElementAt #f in-index)
         (invoke (this) 'repaint (invoke (this) 'getBounds))))))
  
; make a list selection listener
(define (make-listselectionlistener in-callback)
  (object (<javax.swing.event.ListSelectionListener>)
    ((valueChanged e :: <javax.swing.event.ListSelectionEvent>) :: <void>
     (in-callback e))))

; get selection index from listbox
; not sure whether im doing the right thing for listbox or not -teong leong
; should we define a simple class for listbox?
(define (get-listbox-selected-index in-list :: <mylistbox>)
  (invoke in-list 'get-list-selected-index))

; set selection index in listbox
(define (set-listbox-selected-index in-list :: <mylistbox> in-index)
  (invoke in-list 'set-list-selected-index in-index))

; clear selection in listbox
(define (clear-listbox-selection in-list :: <mylistbox>)
  (invoke in-list 'clear-selection))

; set selection mode to single selection
(define (set-listbox-single-selection-mode in-list :: <mylistbox>)
  (invoke in-list 'setSelectionMode <javax.swing.DefaultListSelectionModel>:SINGLE_SELECTION))

; set selection mode to single interval selection
(define (set-listbox-single-interval-selection-mode in-list :: <mylistbox>)
  (invoke in-list 'setSelectionMode <javax.swing.DefaultListSelectionModel>:SINGLE_INTERVAL_SELECTION))

; set selection mode to multiple interval selection
(define (set-listbox-multiple-interval-selection-mode in-list :: <mylistbox>)
  (invoke in-list 'setSelectionMode <javax.swing.DefaultListSelectionModel>:MULTIPLE_INTERVAL_SELECTION))

; get selection value from listbox
(define (get-listbox-selected-value in-list :: <mylistbox>)
  (invoke in-list 'get-list-selected-value))

; get selection data from listbox
(define (get-listbox-selected-data in-list :: <mylistbox>)
  (invoke in-list 'get-list-selected-data))

; get data at index from listbox
(define (get-listbox-data-at-index in-list :: <mylistbox> in-index)
  (invoke in-list 'get-list-data-at-index in-index))

; set visible index in listbox
(define (set-listbox-visible-index in-list  :: <mylistbox> in-index)
  (invoke in-list 'set-list-visible-index in-index))

; get first visible index in listbox
(define (get-listbox-first-visible-index in-list :: <mylistbox>)
  (invoke in-list 'get-list-first-visible-index))

; get last visible index in listbox
(define (get-listbox-last-visible-index in-list :: <mylistbox>)
  (invoke in-list 'get-list-last-visible-index))

; get number of entries in listbox
(define (get-listbox-size in-list :: <mylistbox>)
  (invoke in-list 'get-list-size))

; clear listbox
(define (set-listbox-clear in-list :: <mylistbox>)
  (invoke in-list 'set-list-clear))

; still adjusting?
(define (get-listbox-event-is-adjusting e)
  (invoke (as <javax.swing.event.ListSelectionEvent> e) 'getValueIsAdjusting))

; add an element to a listbox
(define (add-listbox-element in-list :: <mylistbox> txt :: <String> data)
  (invoke in-list 'addElement txt data))

; add an element to a listbox at a given index
(define (insert-listbox-element-at in-list :: <mylistbox> 
                                txt :: <String> 
                                data 
                                index :: <int>)
  (invoke in-list 'insertElementAt txt data index))

; add a selection listener to a listbox
(define (add-listbox-selectListener in-list :: <mylistbox>
                                    in-listener :: <javax.swing.event.ListSelectionListener>)
  (invoke in-list 'addListener in-listener))

; remove an element to a listbox 
(define (remove-listbox-element in-list :: <mylistbox> txt :: <String>)
  (invoke in-list 'removeElement txt))

; set selected element in a listbox
; scrolls there by default (specified by the #t at the back of the function call)
(define (set-listbox-selected-value in-list :: <mylistbox> txt :: <String>)
  (invoke in-list 'setListSelectedValue txt #t))

; set enabled state of a listbox element
(define (set-listbox-element-enabled in-list :: <mylistbox> in-index :: <int> in-flag :: <boolean>)
  (invoke in-list 'set-list-element-enabled in-index in-flag))

; get enabled state of a listbox element
(define (get-listbox-element-enabled in-list :: <mylistbox> in-index :: <int>) :: <boolean>
  (invoke in-list 'get-list-element-enabled in-index))

; set colour of a listbox element
(define (set-listbox-element-colour in-list :: <mylistbox> 
                                    in-index :: <int>
                                    in-colour :: <java.awt.Color>)
  (invoke in-list 'set-list-element-colour in-index in-colour))

; clear (to #f) colour of a listbox element
(define (clear-listbox-element-colour in-list :: <mylistbox> in-index :: <int>)
  (invoke in-list 'clear-list-element-colour in-index))

; get colour of a listbox element
(define (get-listbox-element-colour in-list :: <mylistbox> in-index :: <int>)
  (invoke in-list 'get-list-element-colour in-index))


;
; custom cell renderer to allow for coloured text, based on example in javadocs
; (should be an inner class? - alex)
;

(define-simple-class <colourCellRenderer> (<javax.swing.JLabel> <javax.swing.ListCellRenderer>)
  ((getListCellRendererComponent list :: <javax.swing.JList>
                                 value :: <java.lang.Object>
                                 index :: <int>
                                 isSelected :: <boolean>
                                 cellHasFocus :: <boolean>) :: <java.awt.Component>

   (let ((cell-colour (invoke (as <mylistbox> list) 'get-list-element-colour index)))
     ; set the text
     (invoke (this) 'setText (invoke value 'toString))
     (if isSelected
         ; draw selected
         (begin
           (invoke (this) 'setBackground (invoke list 'getSelectionBackground))
           (if cell-colour
               (invoke (this) 'setForeground cell-colour)
               (invoke (this) 'setForeground (invoke list 'getSelectionForeground))))
         ; draw unselected
         (begin
           (invoke (this) 'setBackground (invoke list 'getBackground))
           (if cell-colour
               (invoke (this) 'setForeground cell-colour)
               (invoke (this) 'setForeground (invoke list 'getForeground)))))

     ; enabled state
     (invoke (this) 'setEnabled (and
                                 (invoke list 'isEnabled)
                                 (invoke  (as <mylistbox> list) 'get-list-element-enabled index)))
     
     ; font and opacity
     (invoke (this) 'setFont (invoke list 'getFont))
     (invoke (this) 'setOpaque #t)
     (this))))
