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

(module-export make-changelistener add-changelistener
               make-actionlistener add-actionlistener
               make-caretlistener add-caretlistener
               make-keylistener add-keylistener
               get-key-event
               enter-key-event? escape-key-event? up-key-event? down-key-event? left-key-event? right-key-event?
               i-key-event? j-key-event? k-key-event? l-key-event?
               w-key-event? s-key-event? a-key-event? d-key-event? space-key-event? tab-key-event? backspace-key-event?
               p-key-event? e-key-event? forward-slash-key-event?
               ctrl-key-modifier?
               set-keystroke-listener
               make-documentlistener add-documentlistener
               make-documentfilter add-documentfilter filter-bypass-insert filter-bypass-remove filter-bypass-replace
               make-windowlistener add-windowlistener remove-windowlistener
               make-itemlistener add-itemlistener
               make-componentlistener add-componentlistener
               dispatch-mouseevent translate-event 
               get-mousebutton-type get-mouseevent-type get-mouseevent-x get-mouseevent-y get-mouseevent-click-count get-mouseevent-rawevent
               make-mouselistener make-mousemotionlistener 
               add-mouselistener add-mousemotionlistener 
               remove-mouselistener remove-mousemotionlistener
               make-propertychangelistener add-propertychangelistener)

;;;;;;;;;;;;;;
;; listeners
;;;;;;;;;;;;;;

;;
;; change listener
;; 

; make a change listener
(define (make-changelistener mycallback)
  (object (<javax.swing.event.ChangeListener>)
    ((stateChanged e :: <javax.swing.event.ChangeEvent>) :: <void>
     (mycallback (invoke e 'getSource)))))

;add a change listener
(define (add-changelistener action-producer
                            action-listener :: <javax.swing.event.ChangeListener>)
  (cond
   ((instance? action-producer <javax.swing.JTabbedPane>)
    (invoke (as <javax.swing.JTabbedPane> action-producer) 'addChangeListener action-listener))
   ((instance? action-producer <javax.swing.JSlider>)
    (invoke (as <javax.swing.JSlider> action-producer) 'addChangeListener action-listener))
   ((instance? action-producer <javax.swing.JSpinner>)
    (invoke (as <javax.swing.JSpinner> action-producer) 'addChangeListener action-listener))
   ))


;;
;; action listener
;; 

; make an action listener
(define (make-actionlistener mycallback)
  (object (<java.awt.event.ActionListener>)
    ((action-performed e :: <java.awt.event.ActionEvent>) :: <void>
     (mycallback (invoke e 'getSource)))))

;add an action listener
(define (add-actionlistener action-producer
                            action-listener :: <java.awt.event.ActionListener>)
  (cond
   ((instance? action-producer <javax.swing.AbstractButton>)
    (invoke (as <javax.swing.AbstractButton> action-producer) 'addActionListener action-listener))
   ((instance? action-producer <javax.swing.JComboBox>)
    (invoke (as <javax.swing.JComboBox> action-producer) 'addActionListener action-listener))
   ((instance? action-producer <javax.swing.JTextField>)
    (invoke (as <javax.swing.JTextField> action-producer) 'addActionListener action-listener))
   (else (format #t "*********** unknown action producer: ~a~%~!" action-producer))
   ))

;;
;; caret listener
;; 

; make an caret listener
(define (make-caretlistener mycallback)
  (object (<javax.swing.event.CaretListener>)
    ((caret-update e :: <javax.swing.event.CaretEvent>) :: <void>
     (mycallback e))))

;add an caret listener
(define (add-caretlistener action-producer :: <javax.swing.text.JTextComponent>
                           action-listener :: <javax.swing.event.CaretListener>)
  (invoke action-producer 'add-caret-listener action-listener))


;;
;; key listener
;; 

; make a key listener
(define (make-keylistener press-callback typed-callback release-callback)
  (object (<java.awt.event.KeyListener>)
    ((key-pressed e :: <java.awt.event.KeyEvent>) :: <void>
     (press-callback e))
    ((key-typed f :: <java.awt.event.KeyEvent>) :: <void>
     (typed-callback f))
    ((key-released g :: <java.awt.event.KeyEvent>) :: <void>
     (release-callback g))))

;add a key listener
(define (add-keylistener action-producer :: <java.awt.Component>
                         action-listener :: <java.awt.event.KeyListener>)
  (invoke action-producer 'add-key-listener action-listener))

; get keycode from key event
(define (get-key-event e :: <java.awt.event.KeyEvent>)
  (invoke e 'getKeyCode))

(define (enter-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_ENTER))

(define (escape-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_ESCAPE))

(define (up-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_UP))

(define (down-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_DOWN))

(define (left-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_LEFT))

(define (right-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_RIGHT))

(define (i-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_I))
(define (j-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_J))

(define (k-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_K))

(define (l-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_L))

(define (w-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_W))

(define (s-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_S))

(define (a-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_A))

(define (d-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_D))

(define (p-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_P))

(define (e-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_E))

(define (space-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_SPACE))

(define (tab-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_TAB))

(define (ctrl-key-modifier? e :: <java.awt.event.KeyEvent>)
;  (format #t "modifier:~a~%~!" (invoke e 'getModifiersEx))
  (= (invoke e 'getModifiersEx) java.awt.event.KeyEvent:CTRL_DOWN_MASK))

(define (backspace-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_BACK_SPACE))

;; leftward slash "/"
(define (forward-slash-key-event? e :: <java.awt.event.KeyEvent>)
  (eq? (invoke e 'getKeyCode) java.awt.event.KeyEvent:VK_SLASH))

(define (set-keystroke-listener component :: <javax.swing.JComponent>)
  (let(( input-map (invoke component 'getInputMap))
       ( key1 (invoke-static <javax.swing.KeyStroke> 'getKeyStroke
                             <java.awt.event.KeyEvent>:VK_B
                             <java.awt.Event>:CTRL_MASK))
       ( key2 (invoke-static <javax.swing.KeyStroke> 'getKeyStroke
                             <java.awt.event.KeyEvent>:VK_F
                             <java.awt.Event>:CTRL_MASK))
       ( key3 (invoke-static <javax.swing.KeyStroke> 'getKeyStroke
                             <java.awt.event.KeyEvent>:VK_P
                             <java.awt.Event>:CTRL_MASK))
       ( key4 (invoke-static <javax.swing.KeyStroke> 'getKeyStroke
                             <java.awt.event.KeyEvent>:VK_N
                             <java.awt.Event>:CTRL_MASK)))
    ;Ctrl-b to go backward one character
    (invoke input-map 'put key1
            <javax.swing.text.DefaultEditorKit>:backwardAction)
    ;Ctrl-f to go forward one character
    (invoke input-map 'put key2
            <javax.swing.text.DefaultEditorKit>:forwardAction)
    ;Ctrl-p to go up one line
    (invoke input-map 'put key3
            <javax.swing.text.DefaultEditorKit>:upAction)
    ;Ctrl-n to go down one line
    (invoke input-map 'put key4
            <javax.swing.text.DefaultEditorKit>:downAction)))



;;
;; document listener
;; 

; make a document listener
; note: document listener cannot make changes to the document, as this will
; likely cause a deadlock - alex
(define (make-documentlistener insert-callback remove-callback changed-callback)
  (object (<javax.swing.event.DocumentListener>)
    ((insert-update e :: <javax.swing.event.DocumentEvent>) :: <void>
     (insert-callback e))
    ((remove-update f :: <javax.swing.event.DocumentEvent>) :: <void>
     (remove-callback f))
    ((changed-update g :: <javax.swing.event.DocumentEvent>) :: <void>
     (changed-callback g))))

;add a document listener
(define (add-documentlistener action-producer :: <javax.swing.text.JTextComponent>
                              action-listener :: <javax.swing.event.DocumentListener>)
  (let ((the-doc :: <javax.swing.text.AbstractDocument> (invoke action-producer 'getDocument)))
    (invoke the-doc 'add-document-listener action-listener)))


;;
;; document filter
;; 

; make a document filter
; gets called BEFORE a document is changed
(define (make-documentfilter insert-string-callback remove-callback replace-callback)
  (object (<javax.swing.text.DocumentFilter>)
    ((insertString fb :: <javax.swing.text.DocumentFilter$FilterBypass>
                   offset :: <int>
                   string :: <java.lang.String>
                   attr :: <javax.swing.text.AttributeSet>) :: <void>
     (if (insert-string-callback fb offset string attr)
         (invoke-special <javax.swing.text.DocumentFilter> (this) 'insertString fb offset string attr)))
    ((remove fb :: <javax.swing.text.DocumentFilter$FilterBypass>
             offset :: <int>
             len :: <int>) :: <void>
     (if (remove-callback fb offset len)
         (invoke-special <javax.swing.text.DocumentFilter> (this) 'remove fb offset len)))
    ((replace fb :: <javax.swing.text.DocumentFilter$FilterBypass>
              offset :: <int>
              len :: <int>
              string :: <java.lang.String>
              attr :: <javax.swing.text.AttributeSet>) :: <void>
     (if (replace-callback fb offset len string attr)
         (invoke-special <javax.swing.text.DocumentFilter> (this) 'replace fb offset len string attr)))
    ))

;add a document filter to a text component
(define (add-documentfilter the-textcomponent :: <javax.swing.text.JTextComponent>
                            the-filter :: <javax.swing.text.DocumentFilter>)
  (let ((the-doc :: <javax.swing.text.AbstractDocument> (invoke the-textcomponent 'getDocument)))
    (invoke the-doc 'set-document-filter the-filter)))

; call insert on document filter's filter bypass
(define (filter-bypass-insert fb :: <javax.swing.text.DocumentFilter$FilterBypass>
                              offset string attr)
  (invoke fb 'insertString offset string attr))

; call remove on document filter's filter bypass
(define (filter-bypass-remove fb :: <javax.swing.text.DocumentFilter$FilterBypass>
                              offset len)
  (invoke fb 'remove offset len))

; call replace on document filter's filter bypass
(define (filter-bypass-replace fb :: <javax.swing.text.DocumentFilter$FilterBypass>
                               offset len string attr)
  (invoke fb 'replace offset len string attr))


;;
;; window listener
;; 

; make a window listener
(define (make-windowlistener opened close closed coni deconi act deact)
  (object (<java.awt.event.WindowListener>)
    ((window-opened e :: <java.awt.event.WindowEvent>) :: <void>
     (opened e))
    ((window-closing e :: <java.awt.event.WindowEvent>) :: <void>
     (close e))
    ((window-closed e :: <java.awt.event.WindowEvent>) :: <void>
     (closed e))
    ((window-iconified e :: <java.awt.event.WindowEvent>) :: <void>
     (coni e))
    ((window-deiconified e :: <java.awt.event.WindowEvent>) :: <void>
     (deconi e))
    ((window-activated e :: <java.awt.event.WindowEvent>) :: <void>
     (act e))
    ((window-deactivated e :: <java.awt.event.WindowEvent>) :: <void>
     (deact e))))

;add a window listener
(define (add-windowlistener action-producer :: <javax.swing.JFrame>
                            action-listener :: <java.awt.event.WindowListener>)
  (invoke action-producer 'add-window-listener action-listener))

; remove a window listener
(define (remove-windowlistener action-producer :: <javax.swing.JFrame>
                            action-listener :: <java.awt.event.WindowListener>)
  (invoke action-producer 'remove-window-listener action-listener))

;;
;; item listener
;; 

; make an item listener
; calls callback with reference to source, and boolean (#t if selected)
(define (make-itemlistener in-item-callback)
  (object (<java.awt.event.ItemListener>)
    ((itemStateChanged e :: <java.awt.event.ItemEvent>) :: <void>
     (in-item-callback (invoke e 'getItemSelectable)
                       (eq? (invoke e 'getStateChange) <java.awt.event.ItemEvent>:SELECTED)))))

; add an item listener
(define (add-itemlistener action-producer :: <javax.swing.AbstractButton>
                          action-listener :: <java.awt.event.ItemListener>)
  (invoke action-producer 'addItemListener action-listener))


;;
;; component listener
;; 

; make a component listener
(define (make-componentlistener hidden-callback moved-callback resized-callback shown-callback)
  (object (<java.awt.event.ComponentListener>)
    ((componentHidden e :: <java.awt.event.ComponentEvent>) :: <void>
     (hidden-callback e))
    ((componentMoved e :: <java.awt.event.ComponentEvent>) :: <void>
     (moved-callback e))
    ((componentResized e :: <java.awt.event.ComponentEvent>) :: <void>
     (resized-callback e))
    ((componentShown e :: <java.awt.event.ComponentEvent>) :: <void>
     (shown-callback e))))

; add a component listener
(define (add-componentlistener the-component :: <java.awt.Component>
                               the-component-listener :: <java.awt.event.ComponentListener>)
  (invoke the-component 'addComponentListener the-component-listener))


;;
;; mouse events
;; 

; dispatch a mouse event
(define (dispatch-mouseevent the-component :: <java.awt.Component>
                               e :: <java.awt.event.MouseEvent>)
  (invoke the-component 'dispatchEvent e))
         
; helper to translate from java events
(define (translate-event event-type e :: <java.awt.event.MouseEvent>)
  (let ((event-x (invoke e 'getX))
        (event-y (invoke e 'getY))
        (event-click-count (invoke e 'getClickCount)))
    (list event-type event-x event-y event-click-count e)))

; getting mouse button type from mouse event
(define (get-mousebutton-type event :: <java.awt.event.MouseEvent>)
  (let ((click-type (invoke event 'get-button)))
    (cond
     ((equal? click-type <java.awt.event.MouseEvent>:BUTTON1) 'left)
     ((equal? click-type <java.awt.event.MouseEvent>:BUTTON2) 'middle)
     ((equal? click-type <java.awt.event.MouseEvent>:BUTTON3) 'right)
     (else (begin (display "not a click in get-mousebutton-type")(newline) 'not-click)))))

; get mouse event details
(define (get-mouseevent-type e)
  (car e))
(define (get-mouseevent-x e)
  (cadr e))
(define (get-mouseevent-y e)
  (caddr e))
(define (get-mouseevent-click-count e)
  (cadddr e))
(define (get-mouseevent-rawevent e)
  (cadddr (cdr e)))

; make a mouse listener
; calls the provided callback with a list consisting of
; event-type, x and y
; note left click
(define (make-mouselistener in-mouse-callback)
  (object (<java.awt.event.MouseListener>)
    ((mouseClicked e :: <java.awt.event.MouseEvent>) :: <void>
     (cond ((equal? (get-mousebutton-type e) 'left)
            (in-mouse-callback (translate-event 'left-clicked e)))
           ((equal? (get-mousebutton-type e) 'middle)
            (in-mouse-callback (translate-event 'middle-clicked e)))
           ((equal? (get-mousebutton-type e) 'right)
            (in-mouse-callback (translate-event 'right-clicked e)))))

    ((mouseEntered e :: <java.awt.event.MouseEvent>) :: <void>
     (in-mouse-callback (translate-event 'entered e)))
    ((mouseExited e :: <java.awt.event.MouseEvent>) :: <void>
     (in-mouse-callback (translate-event 'exited e)))

    ((mousePressed e :: <java.awt.event.MouseEvent>) :: <void>
     (cond ((equal? (get-mousebutton-type e) 'left)
            (in-mouse-callback (translate-event 'left-down e)))
           ((equal? (get-mousebutton-type e) 'middle)
            (in-mouse-callback (translate-event 'middle-down e)))
           ((equal? (get-mousebutton-type e) 'right)
            (in-mouse-callback (translate-event 'right-down e)))))

    ((mouseReleased e :: <java.awt.event.MouseEvent>) :: <void>
     (cond ((equal? (get-mousebutton-type e) 'left)
            (in-mouse-callback (translate-event 'left-up e)))
           ((equal? (get-mousebutton-type e) 'middle)
            (in-mouse-callback (translate-event 'middle-up e)))
           ((equal? (get-mousebutton-type e) 'right)
            (in-mouse-callback (translate-event 'right-up e)))))))

; make a mouse motion listener
; calls the provided callback with a list consisting of
; event-type, x and y
(define (make-mousemotionlistener in-mouse-callback)
  (object (<java.awt.event.MouseMotionListener>)
    ((mouseMoved e :: <java.awt.event.MouseEvent>) :: <void>
     (in-mouse-callback (translate-event 'motion e)))
    ((mouseDragged e :: <java.awt.event.MouseEvent>) :: <void>
     (in-mouse-callback (translate-event 'drag e)))))

; add a mouse listener
(define (add-mouselistener action-producer :: <java.awt.Component>
                           action-listener :: <java.awt.event.MouseListener>)
  (invoke action-producer 'addMouseListener action-listener))

(define (remove-mouselistener action-producer :: <java.awt.Component>
                              action-listener :: <java.awt.event.MouseListener>)
  (invoke action-producer 'removeMouseListener action-listener))

; add a mouse motion listener
(define (add-mousemotionlistener action-producer :: <java.awt.Component>
                                 action-listener :: <java.awt.event.MouseMotionListener>)
  (invoke action-producer 'addMouseMotionListener action-listener))

(define (remove-mousemotionlistener action-producer :: <java.awt.Component>
                                    action-listener :: <java.awt.event.MouseMotionListener>)
  (invoke action-producer 'removeMouseMotionListener action-listener))


;;
;; property change listeners
;; 

; make a property change listener;
; callback takes property name and source
(define (make-propertychangelistener in-callback)
  (object (<java.beans.PropertyChangeListener>)
    ((propertyChange e :: <java.beans.PropertyChangeEvent>) :: <void>
     (in-callback (invoke e 'getPropertyName) (invoke e 'getSource)))))

; add property change listener
(define (add-propertychangelistener propertychange-producer :: <java.awt.Component>
                                    propertychange-listener :: <java.beans.PropertyChangeListener>)
  (invoke propertychange-producer 'addPropertyChangeListener propertychange-listener))
