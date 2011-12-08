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

(require "cursor.scm")
(require "component.scm")
(require "../miscutils.scm")

(module-export make-textfield make-textpane 
               make-textpane-with-background-image 
               set-textpane-background-image clear-textpane-background-image
               make-textarea
               set-textpane-tooltip set-tooltip-initial-delay set-tooltip-dismiss-delay
               make-attribute-set set-text set-text-insert set-text-insert-attr set-text-delete
               set-text-component set-editable
               set-text-append
               set-textpane-page
               get-textpane-doc set-text-default-style set-text-style
               get-text get-text-section get-text-selstart get-text-selend get-text-length get-doc-text
               set-text-selection
               get-text-position-from-point check-has-attributes get-attributes-pos set-style-attributes
               set-attribute-data get-attribute-data get-attribute-data-pos
               get-cursor-pos get-selection-end set-cursor-pos
               get-change-length get-change-offset get-font-height
               textpane-insert textpane-remove)
               
;;
;; text
;; 

; make a text field
(define (make-textfield initial-text :: <java.lang.String> width :: <int>)
  (<javax.swing.JTextField> initial-text width))

; make a jtextpane
(define (make-textpane)
  (<javax.swing.JTextPane>))

; make a jtextpane with background image
(define (make-textpane-with-background-image)
  (<textpanewithbackgroundimage>))

; set the background image
(define (set-textpane-background-image the-textpane :: <textpanewithbackgroundimage>
                                       the-image :: <java.awt.image.BufferedImage>)
  (set-opaque the-textpane #f)
  (invoke the-textpane 'set-background-image the-image))

; clear the background image
(define (clear-textpane-background-image the-textpane :: <textpanewithbackgroundimage>)
  (set-opaque the-textpane #t)
  (invoke the-textpane 'set-background-image #!null))
  
; jtextpane with background image
(define-simple-class <textpanewithbackgroundimage> (<javax.swing.JTextPane>)
  ; the background image
  (the-image :: <java.awt.image.BufferedImage>)
  
  ; set image
  ((set-background-image in-image :: <java.awt.image.BufferedImage>)
   (set! the-image in-image))

  ; override paint to draw the image on the background first
  ((paintComponent g :: <java.awt.Graphics>) :: <void>
   (let ((theGraphics (invoke (this) 'getComponentGraphics g)))
     ; draw our image
     (if (and 
          (not-null? theGraphics)
          (not-null? the-image))
         (invoke theGraphics 'drawImage the-image 0 0 (this)))

     ; call paintComponent in parent last
     (invoke-special <javax.swing.JComponent> (this) 'paintComponent theGraphics))))
  
;make a text area
(define (make-textarea title :: <java.lang.String> row :: <int> column :: <int>)
  (<javax.swing.JTextArea> title row column))

; set the tooltip on a textpane
(define (set-textpane-tooltip the-textpane :: <javax.swing.JTextPane>
                              the-text :: <String>)
  (invoke the-textpane 'setToolTipText (as <java.lang.String> the-text)))

; set delay for tooltip to show
(define (set-tooltip-initial-delay in-delay)
  ((<javax.swing.ToolTipManager>:sharedInstance):setInitialDelay in-delay))

; set delay for tooltip to be dismissed
(define (set-tooltip-dismiss-delay in-delay)
  ((<javax.swing.ToolTipManager>:sharedInstance):setDismissDelay in-delay))


; make an attribute set for text formatting
(define (make-attribute-set)
  (<javax.swing.text.SimpleAttributeSet>))

; set the text
; need to break this down based on class
(define (set-text in-field in-txt)
  (cond
   ((instance? in-field <javax.swing.text.JTextComponent>)
    (invoke (as <javax.swing.text.JTextComponent> in-field) 'setText in-txt))
   ((instance? in-field <javax.swing.JLabel>)
    (invoke (as <javax.swing.JLabel> in-field) 'setText in-txt))
   ((instance? in-field <javax.swing.JButton>)
    (invoke (as <javax.swing.JButton> in-field) 'setText in-txt))
   ))

(define (get-doc-text doc start-offset end-offset)
  (define length (- end-offset start-offset))
  (define doc-length (invoke doc 'get-length))
  (if (and (<= length doc-length)
           (>= start-offset 0))
      (invoke doc 'get-text start-offset (- end-offset start-offset))
      (begin
        (display "[ERROR][get-doc-text] failed ")(newline)
        (display "  Either start-offset less than 0 ")(display start-offset)(newline)
        (display "  Or end-offset ")(display end-offset)(display " > doc length ")(display doc-length)(newline)  
        )))

; insert text at given position into a Document
(define (set-text-insert in-doc :: <javax.swing.text.Document> in-txt in-position)
  (invoke in-doc 'insertString in-position in-txt #!null))

; insert text at given position into a Document with a given attribute set
(define (set-text-insert-attr in-doc :: <javax.swing.text.Document> in-txt in-position
                              in-attr :: <javax.swing.text.AttributeSet>)
  (invoke in-doc 'insertString in-position in-txt in-attr))

; delete text at given position in a Document
(define (set-text-delete in-doc :: <javax.swing.text.Document> in-offset in-length)
  (invoke in-doc 'remove in-offset in-length))

;set properties for the text component
; editable and enable flag
(define (set-text-component in-field editable enable)
  (cond
   ((instance? in-field <javax.swing.text.JTextComponent>)
    (invoke (as <javax.swing.text.JTextComponent> in-field) 'setEditable editable)
    (invoke (as <javax.swing.text.JTextComponent> in-field) 'setEnabled enable))
   ))

(define (set-editable comp :: <java.awt.TextComponent> flag)
  (invoke comp 'setEditable flag))

; append data to textarea or Document, depending on type of first param
;@args: text
; wanted to cast in-textcomponent as <java.awt.TextComponent> and have problems in interaction-panel.scm
(define (set-text-append in-textcomponent in-txt)
  (cond ((instance? in-textcomponent <javax.swing.JTextArea>)
         (invoke (as <javax.swing.JTextArea> in-textcomponent) 'append in-txt))
        ((instance? in-textcomponent <javax.swing.text.Document>)
         (set-text-insert in-textcomponent
                          in-txt
                          (get-text-length in-textcomponent)))))

; set the page for a textpane - for HTML
(define (set-textpane-page in-pane :: <javax.swing.JTextPane> in-URL :: <java.net.URL>)
  (invoke in-pane 'setPage in-URL))

; get doc for a textpane (returns a javax.swing.text.DefaultStyledDocument i think)
(define (get-textpane-doc in-pane :: <javax.swing.JTextPane>)
  (invoke in-pane 'getStyledDocument))

; set the default style: takes a JTextPane
(define (set-text-default-style in-pane :: <javax.swing.JTextPane>
                                in-style in-overwrite)
  (invoke in-pane 'setCharacterAttributes in-style in-overwrite))

; set the style: takes a document
(define (set-text-style in-doc :: <javax.swing.text.DefaultStyledDocument>
                        in-style in-offset in-length in-overwrite)
  (invoke in-doc 'setCharacterAttributes in-offset in-length in-style in-overwrite))

; get the text
(define (get-text in-field)
  (cond
   ((instance? in-field <javax.swing.text.JTextComponent>)
    (invoke (as <javax.swing.text.JTextComponent> in-field) 'getText))
   ((instance? in-field <javax.swing.JLabel>)
    (invoke (as <javax.swing.JLabel> in-field) 'getText))
   ((instance? in-field <javax.swing.JButton>)
    (invoke (as <javax.swing.JButton> in-field) 'getText))
   ))

; get a section of the text
(define (get-text-section in-text :: <javax.swing.text.JTextComponent>
                          offset :: <int>
                          len :: <int>)
  (invoke in-text 'getText offset len))

; get current selection start position in text
(define (get-text-selstart in-text :: <javax.swing.text.JTextComponent>)
  (let ((the-caret :: <javax.swing.text.Caret> (invoke in-text 'getCaret)))
    (min (invoke the-caret 'getDot)
         (invoke the-caret 'getMark))))

; get current selection end position in text
(define (get-text-selend in-text :: <javax.swing.text.JTextComponent>)
  (let ((the-caret :: <javax.swing.text.Caret> (invoke in-text 'getCaret)))
    (max (invoke the-caret 'getDot)
         (invoke the-caret 'getMark))))

; get text length for a document
(define (get-text-length in-text :: <javax.swing.text.Document>)
  (invoke in-text 'getLength))

; set text selection
(define (set-text-selection in-text :: <javax.swing.text.JTextComponent>
                            in-selstart in-selend)
  (let ((the-caret :: <javax.swing.text.Caret> (invoke in-text 'getCaret)))
    (invoke the-caret 'setDot in-selstart)
    (invoke the-caret 'moveDot in-selend)))

;; if there's no text select then it is an insert
(define-private (replace-selection in-pane :: <javax.swing.JTextPane>
                           str)
  (invoke in-pane 'replace-selection str))

(define (textpane-insert in-pane :: <javax.swing.JTextPane>
                         str
                         offset :: <int>
                         #!optional attr ;; :: <javax.swing.text.AttributeSet>
                         )
  (set-text-selection in-pane offset offset) ;; no selection
  (replace-selection in-pane str))

(define (textpane-remove in-pane :: <javax.swing.JTextPane>
                         offset :: <int>
                         length :: <int>)
  (set-text-selection in-pane offset (+ offset length)) ;; no selection
  (replace-selection in-pane ""))

; translate from view x,y position to position in model
(define (get-text-position-from-point in-text :: <javax.swing.text.JTextComponent> x :: <int> y :: <int>)
  (invoke in-text 'viewToModel (<java.awt.Point> x y)))

; get attribute set at given position
(define (get-attributes-pos in-doc :: <javax.swing.text.DefaultStyledDocument>
                            pos :: <int>) :: <javax.swing.text.AttributeSet>
  (let ((charElement :: <javax.swing.text.Element>
                     (invoke in-doc 'getCharacterElement pos)))
    (invoke charElement 'getAttributes)))

; check if given position in a document has the given style
(define (check-has-attributes in-doc :: <javax.swing.text.DefaultStyledDocument>
                              pos :: <int>
                              in-attribute-set :: <javax.swing.text.AttributeSet>)
  (let ((theAttribs :: <javax.swing.text.AttributeSet>
                    (get-attributes-pos in-doc pos)))
    (invoke theAttribs 'containsAttributes in-attribute-set)))

; set attributes:
; 'bold and 'underline for now
(define (set-style-attributes in-attribute-set :: <javax.swing.text.AttributeSet>
                              in-attribute)
  (cond
   ((eq? in-attribute 'bold)
    (<javax.swing.text.StyleConstants>:setBold in-attribute-set #t))
   ((eq? in-attribute 'underline)
    (<javax.swing.text.StyleConstants>:setUnderline in-attribute-set #t))))

; storing data in attributes

; store user-defined data in an attribute
; takes the attribute set, the name of the attribute, and the data to store
(define (set-attribute-data in-attribute-set :: <javax.swing.text.SimpleAttributeSet>
                            in-attribute-name :: <string>
                            in-user-data)
  (invoke in-attribute-set 'addAttribute in-attribute-name in-user-data))

; retrieve user-defined data from attributes
; takes the attribute set and the name of the attribute, and returns the data
(define (get-attribute-data in-attribute-set :: <javax.swing.text.AttributeSet>
                            in-attribute-name :: <string>)
  (invoke in-attribute-set 'getAttribute in-attribute-name))

; retrieve user-defined data from attributes by position
; takes the attribute set, the name of the attribute, and the position, and returns the data
(define (get-attribute-data-pos in-doc :: <javax.swing.text.DefaultStyledDocument>
                                in-attribute-name :: <string>
                                pos :: <int>)
  (let ((theAttribs :: <javax.swing.text.AttributeSet>
                    (get-attributes-pos in-doc pos)))
    (get-attribute-data theAttribs in-attribute-name)))


; this is text-related, not sure where to put

;get cursor position
(define (get-cursor-pos e :: <javax.swing.event.CaretEvent>)
  (invoke e 'getDot))

;get selection end
(define (get-selection-end e :: <javax.swing.event.CaretEvent>)
  (invoke e 'getMark))

;set cursor position 
(define (set-cursor-pos component :: <javax.swing.text.JTextComponent> pos)
  (invoke component 'setCaretPosition pos))

; get change length
(define (get-change-length e :: <javax.swing.event.DocumentEvent>)
  (invoke e 'getLength))

; get change offset
(define (get-change-offset e :: <javax.swing.event.DocumentEvent>)
  (invoke e 'getOffset))

; get height of font
(define (get-font-height fm :: <java.awt.FontMetrics>)
  (invoke fm 'getHeight))