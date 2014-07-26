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

;;
;;text window frame: editing of hypertext nodes, with links
;;

(require "../kawa/ui/menu.scm")
(require "../kawa/ui/component.scm")
(require "../kawa/ui/container.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/miscutils.scm")
(require "../kawa/ui/events.scm")
(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/label.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/scrollpane.scm")
(require "../kawa/ui/button.scm")
(require "../kawa/ui/splitpane.scm")
(require "../kawa/ui/undo.scm")
(require "../kawa/ui/toolbar.scm")
(require "../kawa/ui/undo.scm")
(require "../kawa/strings.scm")
(require "../common/datatable.scm") ;; get
(require "../common/objects.scm") ;; ask
(require "../common/object-listview.scm")
(require "../common/hypertextpane.scm")
(require "../common/links.scm")
(require "../common/evaluator.scm")
(require "../common/window-menu.scm")
(require "../common/main-ui.scm")
(require "../common/list-helpers.scm") ;list-replace
(require "config-options.scm")
(require "datastructure.scm")
(require "hteditor.scm")
(require "rules-manager.scm") ;; rmgr-close rmgr-edit
(require "htfileio.scm")
;(load-relative "highlighter.scm")
(require "hypedyn-undo.scm") ;; hd-postedit
(require "node-graphview.scm") ;; generate-link-name
(require "hypedyn-undo.scm") ;; hd-postedit, hd-begin-update, hd-end-update
(require "editlink.scm") ;; remove-link-display, get-edited-linkID

; export
(module-export update-nodeeditor-frame-title
               nodeeditor-close create-nodeeditor
               get-nodeeditor-frame
               nodeeditor-edit nodeeditor-save
               get-edited-nodeID set-edited-nodeID!
               delete-link refresh-link-list
               nodeeditor-dirty?
               selected-linkID
               do-selectlink ;; rules-manager.scm
               
               ;; needed by hypedyn-undo.scm
               enable-link-buttons
               node-editor
               edited-nodeID
               link-list
               eval-sexpr
               get-link-text
               nodeeditor-set-dirty!
               nodeeditor-clear-dirty!
               )

; currently edited node
(define edited-nodeID '())
(define (set-edited-nodeID! in-ID)
  (set! edited-nodeID in-ID))
(define (get-edited-nodeID)
  edited-nodeID)

; define frame subclass to save contents to node
; shutdown procedure (called as closing)
; should test whether contents have changed before saving to node
; should also have an "ok" button?

; close the node editor
;; this seems to be called when deleting node
(define (nodeeditor-close)
  (rmgr-close)
  (nodeeditor-save)
  (set-edited-nodeID! '())
  (display "set edited-nodeID to '() ")(newline)
  (remove-from-window-menu nodeeditor-frame)
  (set-component-visible nodeeditor-frame #f))

; edit the node; checks if we are already editing it first (for undo)
(define (nodeeditor-edit in-nodeID)
  ; make sure node is selected
  (do-selectnode-list in-nodeID)
  (do-selectnode-graph in-nodeID)

  ; edit if necessary
  ;; Question: this assumes that closing node-editor should reset edited-nodeID?
  ;;           because I had to make it visible in the case where i closed the node 
  ;;           editor and open the same previous node
  (if (not (equal? in-nodeID (get-edited-nodeID)))
      ; not editing this node, so edit it
      (let* ((selected-node (get 'nodes in-nodeID))
             (anywhere (ask selected-node 'anywhere?)))
        
        (display "[nodeeditor-edit] node editing changed to other node ")(display in-nodeID)(newline)
        ; is it already open?
        (if (not (eq? '() (get-edited-nodeID)))
            ; yes, so save any content in nodeeditor
            (nodeeditor-save)
            ; no, so need to add window to window menu
            (add-to-window-menu nodeeditor-frame "Node Editor")
            )

        ; set label
        (set-nodeeditor-frame-title in-nodeID)

        ; set contents
        (nodeeditor-setcontents selected-node)

        ; initially disable link buttons/menu items
        (enable-link-buttons #f)
        (enable-newlink-button #f)

        ; if this is the start node or an anywhere node, disable "set start node" button and menu item
        (if (= in-nodeID (get-start-node))
            (begin
              (set-button nodeeditor-toolbar-button-setstartnode #f)
              (set-menuitem-component m-edit1-setstartnode #f))
            (begin
              (set-button nodeeditor-toolbar-button-setstartnode #t)
              (set-menuitem-component m-edit1-setstartnode #t))
            )

        ; if its not an anywhere node, and facts aren't showing, disable edit node rule button
        (if (show-noderule?)
            (set-component-visible m-edit1-editnoderule (or anywhere (show-facts?))))

        ; clear selected link 
        (set! selected-linkID '())
        
        ;; hide rules-manager (if visible, it isn't relevant anymore)
        (rmgr-close)

        ; clear then populate links list
        (ask link-list 'populate-list selected-node #t)

        ; show the nodeeditor window
        (center-frame-in-parent nodeeditor-frame (get-main-ui-frame))
        (set-component-visible nodeeditor-frame #t)

        ; grab focus
        (request-focus (ask node-editor 'getcomponent))

        ; set selection at start of text
        (ask node-editor 'setselection 0 0)

        ; remember edited node    
        (set-edited-nodeID! in-nodeID)

        ; and reset the undo manager
        ;(undo-manager-discard-all-edits undo-manager)
        (update-undo-action undo-action)
        (update-redo-action redo-action))
      (begin
        (display "editing same node ")(newline)
        ; already editing, so just bring to front and get focus
        (set-component-visible nodeeditor-frame #t)
        (bring-to-front nodeeditor-frame)
        (deiconify nodeeditor-frame)
        (request-focus (ask node-editor 'getcomponent)))))

; set nodeeditor frame title
(define (set-nodeeditor-frame-title in-nodeID)
  (set-frame-title nodeeditor-frame (string-append "Editing: "
                                                   (ask (get 'nodes in-nodeID) 'name)
                                                   (if (show-IDs?) (string-append " (" (number->string in-nodeID) ")") "")
                                                   (if (= in-nodeID (get-start-node)) " (start node)" ""))))

; update nodeeditor frame title
(define (update-nodeeditor-frame-title)
  (let ((the-nodeID (get-edited-nodeID)))
    (if (not (eq? '() the-nodeID))
        (set-nodeeditor-frame-title the-nodeID))))

; save nodeeditor contents (text)
(define (nodeeditor-save)
  (let ((edited-node (get 'nodes edited-nodeID))
        (content (ask node-editor 'gettext))
        (dirty (nodeeditor-dirty?))
        )
;    (display "content len in nodeeditor-save ")(display (string-length content))(newline)
;    (display "content during nodeeditor-save ")(display content)(newline)
      (if (and ;dirty 
               (not (eq? '() content)) 
               (not (eq? '() (get-edited-nodeID))))
          (begin
            (ask edited-node 'set-content! content)
            (nodeeditor-clear-dirty!)
            ; update node emphasis height - need to make this a callback - alex xxx
            (update-node-emphasis edited-nodeID)))
    ))

;window callback ; future work
(define (nodeeditor-window-opened o)
  (format #t "nodeeditor-window-opened~%~!"))
(define (nodeeditor-window-closing o)
  (format #t "nodeeditor-window-closing~%~!")
  (nodeeditor-close))
(define (nodeeditor-window-closed o)
  (format #t "nodeeditor-window-closed~%~!"))

(define (nodeeditor-window-iconified o)
  (format #t "nodeeditor-window-iconified~%~!"))
(define (nodeeditor-window-deiconified o)
  (format #t "nodeeditor-window-deiconified~%~!"))
(define (nodeeditor-window-activated o)
  (format #t "nodeeditor-window-activated~%~!")
  ; update undo menu items in case anything added when we didn't have the focus
  (update-undo-action undo-action)
  (update-redo-action redo-action))

(define (nodeeditor-window-deactivated o)
  (format #t "nodeeditor-window-deactivated~%~!"))

; variables to store node editor components
(define nodeeditor-frame #!null)
(define m-bar1 #f)
(define m-edit1 #f)
(define m-edit1-cut #f)
(define m-edit1-copy #f)
(define m-edit1-paste #f)
(define m-edit1-copylink #f)
(define m-edit1-pastelink #f)
(define m-link #f)
(define m-edit1-newlink #f)
(define m-edit1-editlink #f)
(define m-edit1-renamelink #f)
(define m-edit1-dellink #f)
(define m-edit1-setstartnode #f)
(define m-edit1-editnoderule #f)
(define nodeeditor-toolbar-panel #f)
(define nodeeditor-toolbar-button-close #f)
(define nodeeditor-toolbar-button-newlink #f)
(define nodeeditor-toolbar-button-editlink #f)
(define nodeeditor-toolbar-button-renamelink #f)
(define nodeeditor-toolbar-button-dellink #f)
(define nodeeditor-toolbar-button-setstartnode #f)
(define nodeeditor-toolbar-button-editnoderule #f)
(define nodeeditor-frame-panel #f)
(define nodeeditor-lists-panel #f)
(define link-list-label #f)
(define link-list #f)
(define node-editor #f)
(define node-highlighter #f)
(define node-highlightpainter #f)
;(define undo-manager #f)
;(define undo-action #f)
;(define redo-action #f)

; get the nodeeditor frame
(define (get-nodeeditor-frame)
  nodeeditor-frame)

; create the node editor
; takes callbacks to update links in graph, populate nodes list,
; rename a line, and (node graph to delete a line), update the node style; and
; a reference to the global undo manager
(define (create-nodeeditor populate-nodes-list-callback
                           rename-line-callback
                           node-graph
                           update-node-style-callback
                           in-undo-manager)
  (set! nodeeditor-frame (make-window "Text editor"))
  (set-component-size nodeeditor-frame 450 300)
  (set-container-layout nodeeditor-frame 'border)
  (add-windowlistener nodeeditor-frame (make-windowlistener
                                        nodeeditor-window-opened
                                        nodeeditor-window-closing
                                        nodeeditor-window-closed
                                        nodeeditor-window-iconified
                                        nodeeditor-window-deiconified
                                        nodeeditor-window-activated
                                        nodeeditor-window-deactivated))

  ; menubar
  (set! m-bar1 (make-menu-bar))
  (add-menu-bar nodeeditor-frame m-bar1)

  ; menus
  (set! m-edit1 (make-menu "Edit"))
  (add-component m-bar1 m-edit1)

  ; undo menu items
  (if (is-undo-enabled?)
      (begin
        (add-menu-action m-edit1 undo-action)
        (add-menu-action m-edit1 redo-action)
        (add-component m-edit1 (make-separator))))
  
  ; default edit actions
  (set! m-edit1-cut (make-cut-menuitem))
  (add-component m-edit1 m-edit1-cut)
  (set! m-edit1-copy (make-copy-menuitem))
  (add-component m-edit1 m-edit1-copy)
  (set! m-edit1-paste (make-paste-menuitem))
  (add-component m-edit1 m-edit1-paste)
  
  (set! m-link (make-menu "Link"))
  (add-component m-bar1 m-link)

  ; new link menu item
  (set! m-edit1-newlink (make-menu-item "New link"))
  (add-component m-link m-edit1-newlink)
  (add-actionlistener m-edit1-newlink
                      (make-actionlistener 
                       (lambda (source) (donewlink node-graph update-node-style-callback))))
  (set-menu-item-accelerator m-edit1-newlink #\L)
  (set-menuitem-component m-edit1-newlink  #f)

  ; edit link menu item
  (set! m-edit1-editlink (make-menu-item "Edit link"))
  (add-component m-link m-edit1-editlink)
  (add-actionlistener m-edit1-editlink
                      (make-actionlistener (lambda (source)
                                             (rmgr-edit 'link selected-linkID)
                                             )))
  (set-menu-item-accelerator m-edit1-editlink #\E)
  (set-menuitem-component m-edit1-editlink #f)

  ; rename link menu item
  (set! m-edit1-renamelink (make-menu-item "Rename link"))
  (add-component m-link m-edit1-renamelink)
  (add-actionlistener m-edit1-renamelink
                      (make-actionlistener (lambda (source) (dorenamelink rename-line-callback))))
  (set-menu-item-accelerator m-edit1-renamelink #\R)
  (set-menuitem-component m-edit1-renamelink #f)

  ; delete link menu item
  (set! m-edit1-dellink (make-menu-item "Delete link"))
  (add-component m-link m-edit1-dellink)
  (add-actionlistener m-edit1-dellink
                      (make-actionlistener (lambda (source) (dodellink node-graph
                                                                       update-node-style-callback))))
  (set-menu-item-accelerator m-edit1-dellink #\D)
  (set-menuitem-component m-edit1-dellink #f)
  
  (add-component m-link (make-separator))
  (set! m-edit1-copylink (make-copy-menuitem copy-link #f))
  (set-menu-item-text m-edit1-copylink "Copy link")
  (set-menu-item-mnemonic m-edit1-copylink #\J)
  (set-menu-item-accelerator m-edit1-copylink #\J)
  (add-component m-link m-edit1-copylink)
  (set! m-edit1-pastelink (make-paste-menuitem paste-link-pre paste-link-post))
  (add-component m-link m-edit1-pastelink)
  (set-menu-item-text m-edit1-pastelink "Paste link")
  (set-menu-item-mnemonic m-edit1-pastelink #\K)
  (set-menu-item-accelerator m-edit1-pastelink #\K)
  
  ; set start node menu item
  (set! m-edit1-setstartnode (make-menu-item "Set start node"))
  (add-component m-edit1 m-edit1-setstartnode)
  (add-actionlistener m-edit1-setstartnode
                      (make-actionlistener (lambda (source) (dosetstartnode (get-edited-nodeID) populate-nodes-list-callback))))
  (set-menuitem-component m-edit1-setstartnode #f)

  ; set edit rule menu item
  (set! m-edit1-editnoderule (make-menu-item "Edit node rules"))
  (if (show-noderule?)
      (begin
        (add-component m-edit1 m-edit1-editnoderule)
        (add-actionlistener m-edit1-editnoderule
                            (make-actionlistener (lambda (source) 
                                                   (rmgr-edit 'node (get-edited-nodeID))
                                                   )))
        (set-menuitem-component m-edit1-editnoderule #t)))

  ;; TODO: what does this do?
  ;(append-editor-operation-menu-items m-edit1)
  ; what is equivalent? - alex xxx
  
  ; window menu
  (add-component m-bar1 (add-window-menu nodeeditor-frame))
  
  ;; Add a horizontal panel to the frame, with centering, to hold toolbar buttons
  (set! nodeeditor-toolbar-panel (make-toolbar "Toolbar"))
  (set-toolbar-floatable nodeeditor-toolbar-panel #f)
  (add-component nodeeditor-frame nodeeditor-toolbar-panel 'border-north)

  ;; button to create a new link
  (set! nodeeditor-toolbar-button-newlink (make-button "New link"))
  (add-component nodeeditor-toolbar-panel nodeeditor-toolbar-button-newlink )
  (add-actionlistener nodeeditor-toolbar-button-newlink
                      (make-actionlistener (lambda (source) (donewlink node-graph update-node-style-callback))))

  ; button to edit link rule/conditions
  (set! nodeeditor-toolbar-button-editlink (make-button "Edit Link"))
  (add-component nodeeditor-toolbar-panel nodeeditor-toolbar-button-editlink )
  (add-actionlistener nodeeditor-toolbar-button-editlink
                      (make-actionlistener (lambda (source)
;                                             (doeditlink selected-linkID
;                                                         (get-edited-nodeID)
;                                                         (get-link-text selected-linkID))
                                             (rmgr-edit 'link selected-linkID)
                                             )))

  ; button to rename link
  (set! nodeeditor-toolbar-button-renamelink (make-button "Rename Link"))
  (add-component nodeeditor-toolbar-panel nodeeditor-toolbar-button-renamelink )
  (add-actionlistener nodeeditor-toolbar-button-renamelink
                      (make-actionlistener (lambda (source) (dorenamelink rename-line-callback))))

  ;; button to delete a link
  (set! nodeeditor-toolbar-button-dellink (make-button "Delete link"))
  (add-component nodeeditor-toolbar-panel nodeeditor-toolbar-button-dellink )
  (add-actionlistener nodeeditor-toolbar-button-dellink
                      (make-actionlistener (lambda (source) (dodellink node-graph
                                                                       update-node-style-callback))))

  ;; button to set this node as the start node
  (set! nodeeditor-toolbar-button-setstartnode (make-button "Set start node"))
  (add-component nodeeditor-toolbar-panel nodeeditor-toolbar-button-setstartnode)
  (add-actionlistener nodeeditor-toolbar-button-setstartnode
                      (make-actionlistener (lambda (source) (dosetstartnode (get-edited-nodeID) populate-nodes-list-callback))))

  ;; button to edit node rule
  (set! nodeeditor-toolbar-button-editnoderule (make-button "Edit node rules"))
  (if (show-noderule?)
      (begin
        (add-component nodeeditor-toolbar-panel nodeeditor-toolbar-button-editnoderule)
        (add-actionlistener nodeeditor-toolbar-button-editnoderule
                            (make-actionlistener (lambda (source) 
                                                   ;(doeditnoderule (get-edited-nodeID))
                                                   (rmgr-edit 'node (get-edited-nodeID))
                                                   )))))
  
  ;; add a splitpanel for link list and text editor
  (set! nodeeditor-frame-panel (make-splitpane #f))
  (add-component nodeeditor-frame nodeeditor-frame-panel 'border-center)

  ; set resize weight and layout options
  (set-splitpane-resize-weight nodeeditor-frame-panel 0.25)
  (set-splitpane-continuous-layout nodeeditor-frame-panel #t)
  (set-splitpane-onetouchexpandable nodeeditor-frame-panel #t)

  ;; panel to hold lists of links
  (set! nodeeditor-lists-panel (make-panel))
  (set-container-layout nodeeditor-lists-panel 'vertical)
  (set-component-minimum-size nodeeditor-lists-panel 100 200)
  (set-component-preferred-size nodeeditor-lists-panel 100 200)
  (add-splitpane-component nodeeditor-frame-panel nodeeditor-lists-panel #t)

  ;; make a list for links
  (set! link-list-label (make-label))
  (set-text link-list-label "Links")
  (add-component nodeeditor-lists-panel link-list-label)
  (set! link-list (make-link-listview do-selectlink 
                                      (lambda (e)
                                        (linklist-onmouse e))))
  (ask link-list 'init)
  (add-component nodeeditor-lists-panel (ask link-list 'get-component))

  ; create editor (with links)
  ; pass in callback for when a link is selected and deleted, to enable the new
  ; link button, and the method for getting links from the associated pattern
  (set! node-editor
        (make-hypertextpane 400 300
                            (lambda (linkID)
                              (if linkID
                                  (ask link-list 'select-link linkID)
                                  (begin
                                        ; handle deselecting a link
                                    (ask link-list 'deselect-link)

                                        ; forget selected link
                                    (set! selected-linkID '())

                                    ; disable delete link buttons and menu items
                                    (enable-link-buttons #f)
                                    )))
                            (lambda (del-linkID)
                              (delete-link del-linkID
                                           #f
                                           node-graph
                                           update-node-style-callback))
                            enable-newlink-button
                            'content
                            'links
                            'nodes))
  (ask node-editor 'init)
  (ask node-editor 'set-update-dirty-callback update-dirty-state)
  (ask node-editor 'set-postedit-proc! hd-postedit)
  (ask node-editor 'set-beginupdate-proc! hd-begin-update)
  (ask node-editor 'set-endupdate-proc! hd-end-update)
  
  (add-splitpane-component nodeeditor-frame-panel
                           (make-scrollpane
                            (ask node-editor 'getcomponent)) #f)
  
  ; working on keymapping, not sure why its not working... - alex
;;  (define the-text-component (ask node-editor 'getcomponent))
;;  (define the-keymap
;;    (invoke (as <javax.swing.text.JTextComponent> the-text-component)
;;            'addKeymap "mykeymap"
;;            (invoke (as <javax.swing.text.JTextComponent> the-text-component)
;;                    'getKeymap)))
;;  (invoke (as <javax.swing.text.Keymap> the-keymap)
;;          'addActionForKeyStroke
;;          (<javax.swing.KeyStroke>:getKeyStroke (as <int> (char->integer #\C))
;;                                                (invoke (<java.awt.Toolkit>:getDefaultToolkit)
;;                                                        'getMenuShortcutKeyMask))
;;          (as <javax.swing.AbstractAction> custom-copy-action))
          

  
  ; tell the editor about our undo manager
  (ask node-editor 'set-undo-manager! undo-manager undo-action redo-action nodeeditor-edit)
  
  ; experimental, not in use
  
  ; add a highlighter - move inside
  ;(set-text-highlighter n-editor n-highlighter)

  ; move inside
  ; (set! node-highlighter (make-underlinehighlighter))
  ;(set! node-highlightpainter (make-underlinehighlight-painter (make-colour-rgb 255 0 0)))
  )

; enable/disable link-related buttons
(define (enable-link-buttons newstate)
  (set-button nodeeditor-toolbar-button-editlink newstate)
  (set-button nodeeditor-toolbar-button-dellink newstate)
  (set-button nodeeditor-toolbar-button-renamelink newstate)
;;  (set-menuitem-component m-edit1-copylink newstate)
  (set-menuitem-component m-edit1-editlink newstate)
  (set-menuitem-component m-edit1-dellink newstate)
  (set-menuitem-component m-edit1-renamelink newstate))

; enable/disable new link button
(define (enable-newlink-button newstate)
  (set-button nodeeditor-toolbar-button-newlink newstate)
;;  (set-menuitem-component m-edit1-pastelink newstate)
  (set-menuitem-component m-edit1-newlink newstate))

; enable/disable setstartnode button and menu item state
(define (enable-setstartnode-button newstate)
  (set-button nodeeditor-toolbar-button-setstartnode newstate)
  (set-menuitem-component m-edit1-setstartnode newstate))

;
; callbacks for nodeeditor menu items/buttons
;

; set currently edited node as the start node
(define (dosetstartnode in-startnodeID populate-nodes-list-callback)
  (let ((old-startnodeID (get-start-node)))
    ; store the action for undoing
    (hd-postedit
     undo-manager
     (make-undoable-edit
      "Set start node"
      (lambda () ; undo
        (dosetstartnode-action old-startnodeID populate-nodes-list-callback))
                                        
      (lambda () ; redo
        (dosetstartnode-action in-startnodeID populate-nodes-list-callback))))

    ; and do the action
    (dosetstartnode-action in-startnodeID populate-nodes-list-callback)))

; set start node action: this actually performs the action, used for undo/redo
(define (dosetstartnode-action in-startnodeID populate-nodes-list-callback)
  (let ((old-start-nodeID (get-start-node))
        (current-edited-nodeID (get-edited-nodeID)))
    ; set the start node
    (set-start-node! in-startnodeID)

    ; update the UI
    (if (not (null? current-edited-nodeID))
        ; we are currently editing a node, so update title and button/menu item states
        (begin
          (update-nodeeditor-frame-title)
          (if (and (not (null? in-startnodeID))
                   (= current-edited-nodeID in-startnodeID))
              ; new start node is the currently edited node
              (enable-setstartnode-button #f)
              ; otherwise, another node is the start node
              (enable-setstartnode-button #t))))
    (populate-nodes-list-callback)))

; create a new link
(define (donewlink node-graph update-node-style-callback)
  
  ;(define newlink-name (make-input-dialogbox-check-empty nodeeditor-frame "" "Link name" "New link"))
  (define newlink-name (make-input-dialogbox-custom nodeeditor-frame "" "New link" "Link name"))
  
  (define (newlink-undoable-postedit newlink-name newlink-ID)
    (hd-begin-update undo-manager)
    (define thelink (get 'links newlink-ID))
    (if thelink
        (begin
          ; store the action for redoing
          (define redo-sexpr (ht-build-sexpr-from-object-with-rule thelink))
          (define from-nodeID (ask thelink 'source))
          ;; (define to-nodeID (ask thelink 'destination))
          ;; (define to-alt-nodeID (ask thelink 'alt-destination))
          ;; (define name (ask thelink 'name))
          ;; (define from-node (get 'nodes from-nodeID))
          ;; (define usedest (ask thelink 'use-destination))
          ;; (define usealtdest (ask thelink 'use-alt-destination))
          (hd-postedit 
           undo-manager
           (make-undoable-edit
            "New Link"
            
            (lambda () ; undo
              (display "[undo add new link]")(newline)
              ; delete the link - not sure about del-in-nodeeditor?
              (delete-link-action newlink-ID from-nodeID #t update-node-style-callback)
              
              ;; enable/disable newlink button based on whether text is selected
              (ask node-editor 'selection-newlink-check)
              )
            
            (lambda () ;;redo
              (display "[REDO add new link]")(newline)
               ; restore the link
              (delete-link-undo redo-sexpr newlink-ID from-nodeID update-node-style-callback #t)
              
              ;; enable/disable newlink button based on whether text is selected
              (ask node-editor 'selection-newlink-check)
              )))
            ))
    (hd-end-update undo-manager undo-action redo-action)
    
    ;; hack to save content of node everytime link is created
    (nodeeditor-save)
    )
  
  (define (create-newlink newlink-name)
    (if (not (equal? newlink-name ""))
        ; create the link
        (let ((new-fromnode (get-edited-nodeID))
              (new-startindex (ask node-editor 'getselstart))
              (new-endindex (ask node-editor 'getselend))
              (new-tonode -1))
          ; create the link
          (let ((newlink-ID (create-link
                             newlink-name
                             new-fromnode new-tonode
                             new-startindex new-endindex
                             #f #f #f -1 ""
                             '() ; don't display the link yet, as we don't know destination
                             )))

            ; add link
            (ask node-editor 'addlink (get 'links newlink-ID))

            ; add to link list
            (ask link-list 'add-link newlink-ID newlink-name)
            (ask link-list 'select-link newlink-ID)
            
            ; disable new link button and menu item (until selection changes)
            ;; seem like now it disables correctly without this
            ;(enable-newlink-button #f)

            ;; disables the newlink button because node editor sees a link in the selection
            (ask node-editor 'selection-newlink-check)
            
            ; update main window label to show file is dirty
            (update-dirty-state)

            ;; used to need to add the editlink part as well to this action
            (newlink-undoable-postedit newlink-name newlink-ID)
            
            ; show edit rule dialog
            ;(doeditlink newlink-ID (get-edited-nodeID) (get-link-text newlink-ID))
            (rmgr-edit 'link newlink-ID)
            ))))
  
  ;; cancel return #!null (dont do anything if null)
  (if (and (procedure? create-newlink)
           (not (is-null? newlink-name)))
        (create-newlink newlink-name))
  )

; delete a link
(define (dodellink node-graph update-node-style-callback)
  (delete-link selected-linkID #t node-graph update-node-style-callback))

;; delete a link
;; Note: used of del-in-nodeeditor helps avoid deadlocks after deleting the link text 
;;      deletes from nodeeditor if del-in-nodeeditor is #t
;; 
(define (delete-link linkID del-in-nodeeditor node-graph update-node-style-callback)
  (let ((thelink (get 'links linkID)))
    (if thelink
        (begin
          (define from-nodeID (ask thelink 'source))
          (define redo-sexpr (ht-build-sexpr-from-object-with-rule thelink)) ; store the action for undoing
          
          (hd-postedit
           undo-manager
           (make-undoable-edit
            "Delete Link"
            ;; undo
            (lambda () ;; restore the link
              (delete-link-undo redo-sexpr linkID from-nodeID update-node-style-callback del-in-nodeeditor)
              )
            ;; redo
            (lambda () ;; delete the link - not sure about del-in-nodeeditor?
              (delete-link-action linkID from-nodeID del-in-nodeeditor update-node-style-callback)
              )))

            ; and actually delete the link
            (delete-link-action linkID from-nodeID del-in-nodeeditor update-node-style-callback)
          
            )))
  )

; evaluate an s-expression
(define (eval-sexpr this-expr)
  (try-catch
      (myeval this-expr)
    (ex <java.lang.Throwable>
        (begin
          (display (*:toString ex))(newline)
          (*:printStackTrace ex)))))

; rename a link
(define (dorenamelink rename-line-callback)
  ; show dialogue box asking for new name of link
  (let* ((selected-link (get 'links selected-linkID))
         (this-linkID selected-linkID)
         (this-nodeID edited-nodeID)
         (oldname (ask selected-link 'name))
         (newname (make-input-dialogbox-custom nodeeditor-frame oldname "Rename link" "Enter the new name for this link"))
         )
    ;; cancel on dialogbox returns #!null
    (if (not (is-null? newname))
        (begin
          ; store the action for undoing
          (hd-postedit
             undo-manager
             (make-undoable-edit
              "Rename Link"
               ; undo
              (lambda ()
                (begin
                  (do-selectlink this-linkID)
                  (dorenamelink-action rename-line-callback oldname this-linkID this-nodeID)))
              ; redo
              (lambda ()
                (begin
                  (do-selectlink this-linkID)
                  (dorenamelink-action rename-line-callback newname this-linkID this-nodeID)))))

          ; update link name
          (dorenamelink-action rename-line-callback newname selected-linkID edited-nodeID)))))

; rename link action: this actually performs the rename, also used for undo/redo
(define (dorenamelink-action rename-line-callback newname in-linkID in-nodeID)
  (let ((selected-link (get 'links in-linkID))
        (this-node (get 'nodes in-nodeID)))
    ; update in data structure
    (ask selected-link 'set-name! newname)

    ; update main window label to show file is dirty
    (update-dirty-state)

    ; in link list (cheat!)
    (ask link-list 'populate-list this-node #t)
    (ask link-list 'select-link in-linkID)

    ; and in graph
    (rename-line-callback (number->string in-linkID)
                          (if (show-IDs?)
                              (string-append newname "(" (number->string in-linkID) ")")
                              newname))
    (rename-line-callback (string-append "~" (number->string in-linkID))
                          (string-append "~" newname
                                         (if (show-IDs?)
                                             (string-append "(" (number->string in-linkID) ")")
                                             "")))))


;; links list in nodeeditor

; currently selected link
(define selected-linkID '())

(define (make-link-listview select-callback onmouse-callback)
  (let ((parent-obj (make-object-listview select-callback #t 'links 'name string>=?))
        (this-obj (new-object)))

    ;; inherit parent-obj
    (derive this-obj parent-obj)
    
    ; initialize
    (define (init)
      (ask parent-obj 'init)
      (if (procedure? onmouse-callback)
          (add-mouselistener (ask parent-obj 'get-list)
                             (make-mouselistener onmouse-callback))))
    
    ; populate the links list
    ; note: only update highlight links in node if add-in-nodeeditor is #t
    (define (populate-list thisnode add-in-nodeeditor)
      (ask parent-obj 'clear)
      ;(display "[Populate list] ")(display (ask thisnode 'links))(newline)
      ;(format #t "links: ~a thisnode links:~a~%~!" (get-list 'links) (ask thisnode 'links))
      (map (lambda (l)
             (let ((thislink (get 'links l)))
               ;(format #t "l: ~a thislink:~a~%~!" l thislink)
               ; add link to list
               (add-link l (ask thislink 'name))

               ; highlight text
               (if add-in-nodeeditor
                   (ask node-editor 'addlink thislink))))
           (ask thisnode 'links)))
    
    ; add a link
    (define (add-link new-linkID name)
      (let ((display-name (if (show-IDs?)
                              (string-append name
                                             " ("
                                             (number->string new-linkID)
                                             ")")
                              name)))
        
        ;; DEBUG
        (define thislink (get 'links new-linkID))
        (define old-start (ask thislink 'start-index))
        (define old-end (ask thislink 'end-index))
        ;(display "[add-link] ")(display (list old-start old-end))(newline)
        (ask parent-obj 'add-object new-linkID display-name)))

    ; message handling
    (obj-put this-obj 'init
             (lambda (self)
               (init)))
    (obj-put this-obj 'populate-list
             (lambda (self in-node in-flag)
               (populate-list in-node in-flag)))
    (obj-put this-obj 'add-link
             (lambda (self new-linkID name)
               (add-link new-linkID name)))
    (obj-put this-obj 'select-link
             (lambda (self in-linkID)
               (ask parent-obj 'select-object in-linkID)))
    (obj-put this-obj 'deselect-link
             (lambda (self)
               (ask parent-obj 'deselect-object)))
    this-obj))


; selected a link in the link list, invoked when selection changes in the link list
(define (do-selectlink linkID)
  (if (not (eq? '() linkID))
      (let ((link (get 'links linkID)))
        (if link
            (let ((name (ask link 'name))
                  (destination (ask link 'destination)))
              ; remember selected link
              (set! selected-linkID linkID)

              ; show selection in text
              (ask node-editor 'setselection (ask link 'start-index) (ask link 'end-index))

              ; enable delete link buttons and menu items
              (enable-link-buttons #t))))))

; mouse event in link list - for double-clicking
;; QUESTION how does this work? (newlink-ID 'link selected-linkID)
(define (linklist-onmouse e)
  (let ((event-type (get-mouseevent-type e)))
    (if (eq? event-type 'left-clicked)
        (begin
          ;; fix for windows where we need focus to show the hightlight
          (request-focus (ask node-editor 'getcomponent))
          
          (let ((event-click-count (get-mouseevent-click-count e)))
            (if (= 2 event-click-count)
                (rmgr-edit 'link selected-linkID)
                ))))))

; get the text of the specified link, used by editlink
(define (get-link-text in-linkID)
  (if (not (eq? '() in-linkID))
      (let ((link (get 'links in-linkID)))
        (if link
            (let ((link-start (ask link 'start-index))
                  (link-end (ask link 'end-index)))
              (ask node-editor 'gettextsection link-start link-end))
            ""))
      ""))

; refresh the link list; used when showIDs is toggled
(define (refresh-link-list)
  (let ((edited-node (get 'nodes edited-nodeID)))
    (if edited-node
        (ask link-list 'populate-list edited-node #f))))

; flag to track if links are being tracked - unused?
(define track-links #f)
(define (set-track-links! m)
  (set! track-links m))

; flag to track if there have been any edits
(define nodeeditor-dirty #f)
(define (nodeeditor-dirty?)
  (ask node-editor 'dirty?))
(define (nodeeditor-clear-dirty!)
  (ask node-editor 'clear-dirty!)
  (update-dirty-state))
(define (nodeeditor-set-dirty!) ;; no longer used
  (ask node-editor 'set-dirty!)
  (update-dirty-state))

; set nodeeditor contents
(define (nodeeditor-setcontents selected-node)
  ; tell hypertextnode which node we're editing; will add text
  ; and links from the node
  (display "node editor set contents ")(newline)
  (ask node-editor 'clear-content!)
  (ask node-editor 'set-node! selected-node))


;;
;; handle copy and paste of links
;; 

(define (copy-link e)
  (format #t "copy text~%~!")
  (copy-with-links)
  
  ; return true so that normal copy action is executed afterwards
  #t)
  
(define copied-text #f) ; plain text
(define copied-links '()) ; ((link (rule (conditions ...) (actions ...)) ...) ...)
  
  ; need to 1) check if there are any links in the text, and 
  ; 2) store those links so they can be duplicated/pasted
  
  ; strategy for duplicating:
  ; 1) make a copy but don't add to data structure
  ; 2) on paste, duplicate then shift the copy
  
(define (copy-with-links)
  ; remember copied text
  (set! copied-text (ask node-editor 'getselectedtext))
  (format #t "copied text: ~a~%~!" copied-text)
  
  ; copy all the links
  (define selstart (ask node-editor 'getselstart)) 
  (define selend (ask node-editor 'getselend)) 
  (define contained-links (ask node-editor 'get-contained-links selstart selend))
  (format #t "contained links: ~a~%~!" contained-links)
  (set! copied-links '())
  (map (lambda (linkID)
         (let* ((link-obj (get 'links linkID))
                (name (ask link-obj 'name))
                (start-index (- (ask link-obj 'start-index) selstart))
                (end-index (- (ask link-obj 'end-index) selstart))
                (rule-lst (ask link-obj 'rule-lst)))
           ; copy one link
           ; note: don't want to actually create yet, instead get the s-expr - use make-link
           (define copied-link
;             (make-link (string-append name " copy") -1 -1 start-index end-index
             (make-link name -1 -1 start-index end-index
                        #f -1 #f -1
                        "" linkID))
           (format #t "copied link: ~a ~a~%~!" linkID (ask copied-link 'to-save-sexpr))
           
           ;; copy the rules on this link
           (define copied-rules '())
           (map (lambda (ruleID)
                  (let* ((rule-obj (get 'rules ruleID))
                         (name (ask rule-obj 'name))
                         (type (ask rule-obj 'type))
                         (and-or (ask rule-obj 'and-or))
                         (negate? (ask rule-obj 'negate?))
                         (fall-through? (ask rule-obj 'fall-through?)))
                    ; copy this rule
                    (define copied-rule
                      (make-rule3 name type and-or negate? linkID
                                  fixedID: ruleID
                                  fall-through?: fall-through?))
                    (format #t "copied rule: ~a, sexpr: ~a~%~!" ruleID (ask copied-rule 'to-save-sexpr))
                    
                    ;; duplicate the conditions 
                    (define copied-conditions '())
                    (map (lambda (condID)
                           (let* ((cond-obj (get 'conditions condID))
                                  (name (ask cond-obj 'name))
                                  (type (ask cond-obj 'type))
                                  (targetID (ask cond-obj 'targetID))
                                  (operator (ask cond-obj 'operator))
                                  (numfact-args (ask cond-obj 'numfact-args)))
                             (define copied-condition
                               (make-condition2 name type targetID operator ruleID fixedID: condID
                                                numfact-args: numfact-args))
                             (format #t "copied condition: ~a, sexpr: ~a~%~!" condID (ask copied-condition 'to-save-sexpr))
                             (set! copied-conditions (append copied-conditions (list copied-condition)))))
                         (ask rule-obj 'conditions))
                    
                    ;; duplicate the actions 
                    (define copied-actions '())
                    (map (lambda (actionID)
                           (let* ((action-obj (get 'actions actionID))
                                  (name (ask action-obj 'name))
                                  (type (ask action-obj 'type))
                                  (expr (ask action-obj 'expr)))

                             (define copied-action
                               (make-action name type expr ruleID actionID))
                             (format #t "copied action: ~a, sexpr: ~a~%~!" actionID (ask copied-action 'to-save-sexpr))
                             (set! copied-actions (append copied-actions (list copied-action))))) 
                         (ask rule-obj 'actions))
                    
                    (set! copied-rules (append copied-rules (list (list copied-rule copied-conditions copied-actions))))))
                rule-lst)
         
         (set! copied-links (append copied-links (list (list copied-link copied-rules))))))
       contained-links))

(define paste-selstart 0)

(define (paste-link-pre e)
  (format #t "paste link pre~%~!")
  
  ; remember start of selection before pasting
  (set! paste-selstart (ask node-editor 'getselstart))  
  
  ;; wrap paste link in one operation
  (hd-begin-update undo-manager)

  ; return true so that normal paste action is executed afterwards
  #t)
  

(define (paste-link-post e)
  (format #t "paste link post~%~!")

  ; need to check if there are any stored links and duplicate them
  ; note: need to do this AFTER the default paste action!

  (let-values (((max-x max-y max-anywhere-x max-anywhere-y)
                (get-max-node-positions)))
    (let-values (((dup-offset-ID dup-offset-x dup-offset-anywhere-x)
                  (get-duplicate-offsets max-x max-y
                                         max-anywhere-x max-anywhere-y)))
      (let ((this-paste-selstart paste-selstart)
            (this-copied-links copied-links)
            (this-nodeID (get-edited-nodeID))) 
        (define (actual-paste-links)

          (format #t "actual-paste-links: paste-selstart=~a, this-paste-selstart=~a~%~!" paste-selstart this-paste-selstart)

          ; run through the links
          (map (lambda (c)
                 (let ((copied-link (car c))
                       (copied-rules (cadr c)))
                   ; paste the link
                   (define new-linkID (+ dup-offset-ID (ask copied-link 'ID)))
                   (format #t "pasting link: ~a~%~!" (ask copied-link 'to-save-sexpr))
                   (paste-link this-paste-selstart copied-link new-linkID edited-nodeID)

                   ; run through the rules
                   (map (lambda (r)
                          (let ((copied-rule (car r))
                                (copied-conditions (cadr r))
                                (copied-actions (caddr r)))
                            ; paste the rule
                            (define new-ruleID (+ dup-offset-ID (ask copied-rule 'ID)))
                            (format #t "pasting rule: ~a~%~!" (ask copied-rule 'to-save-sexpr))
                            (paste-rule copied-rule new-ruleID new-linkID)

                            ; run through the conditions
                            (map (lambda (copied-condition)
                                   ; paste the condition
                                   (format #t "pasting condition: ~a~%~!" (ask copied-condition 'to-save-sexpr))
                                   (paste-condition copied-condition (+ dup-offset-ID (ask copied-condition 'ID)) new-ruleID))
                                 copied-conditions)

                            ; run through the actions
                            (map (lambda (copied-action)
                                   ; paste the actions
                                   (format #t "pasting action: ~a~%~!" (ask copied-action 'to-save-sexpr))
                                   (paste-action copied-action (+ dup-offset-ID (ask copied-action 'ID)) new-ruleID new-linkID))
                                 copied-actions)

                            ;; update graph view 
                            (add-follow-link-rule-display new-ruleID)
                            (add-show-popup-rule-display new-ruleID)
                            ))
                        copied-rules)

                   ; add link to editor
                   (define new-link (get 'links new-linkID))
                   (ask node-editor 'addlink new-link)

                   ; add to link list
                   (ask link-list 'add-link new-linkID (ask new-link 'name))))

               this-copied-links))

        (define (undo-paste-links)
          (format #t "undo-paste-links~%~!")
          
          ; make sure we're editing the correct node
          (nodeeditor-edit this-nodeID)
          
          ; run through the links and delete them (undoing the paste)
          (map (lambda (c)
                 (let ((copied-link (car c))
                       (copied-rules (cadr c)))
                   ; delete the link
                   (define new-linkID (+ dup-offset-ID (ask copied-link 'ID)))
                   (format #t "deleting link: ~a~%~!" (ask copied-link 'to-save-sexpr))
                   ; delete the link; rules, conditions and actions should be deleted automatically
                   (delete-link-action new-linkID edited-nodeID #f #f)))
               this-copied-links))

        ; actually paste the links
        (actual-paste-links)

        ;; add the undoable edit
        (hd-postedit
         undo-manager
         (make-undoable-edit
          "Paste Link"
          (lambda () ;; undo
            ;; here we need to delete the text (and hopefully automatically any links)
            ;; in the range where we pasted (selstart and selend *after* pasting*)
            (display "undoing paste link")(newline)
            (undo-paste-links)
            )
          (lambda () ;; redo
            (display "redoing paste link")(newline)

            ;; here we need to redo the paste, by wrapping all the above code in a function
            (actual-paste-links)
            )))

        ;; end the undoable paste links action
        (hd-end-update undo-manager undo-action redo-action)))))


(define (paste-link in-paste-selstart link-obj linkID parentID)
  (let* ((name (ask link-obj 'name))
         (start-index (+ in-paste-selstart (ask link-obj 'start-index)))
         (end-index (+ in-paste-selstart (ask link-obj 'end-index))))
    (create-link name parentID -1
                 start-index end-index
                 #f -1 #f -1
                 "" #f linkID)))

(define (paste-rule rule-obj ruleID parentID)
  (let* ((name (ask rule-obj 'name))
         (type (ask rule-obj 'type))
         (and-or (ask rule-obj 'and-or))
         (negate? (ask rule-obj 'negate?))
         (fall-through? (ask rule-obj 'fall-through?)))

    (create-typed-rule3 name type and-or negate? parentID 
                        fixedID: ruleID 
                        fall-through?: fall-through?)
))

(define (paste-condition cond-obj condID parentID)
  (let* ((name (ask cond-obj 'name))
         (type (ask cond-obj 'type))
         (targetID (ask cond-obj 'targetID))
         (operator (ask cond-obj 'operator))
         (numfact-args (ask cond-obj 'numfact-args)))
    (create-typed-condition2 name type targetID operator parentID 
                             fixedID: condID
                             numfact-args: numfact-args)
    ))

(define (paste-action action-obj actionID parentID new-linkID)
  (let* ((name (ask action-obj 'name))
         (type (ask action-obj 'type))
         (expr (ask action-obj 'expr)))
    
    ;; replace text action should point to newly pasted link
    (if (equal? (car expr) 'replace-link-text)
        (begin
          (set! expr (list-replace expr 3 new-linkID))
          ))
         
    ;; add anywhere link action should point to current node
    (if (equal? (car expr) 'add-anywhere-link)
        (begin
          (set! expr (list-replace expr 1 edited-nodeID))
          ))
         
    (create-action name type expr parentID actionID)
    ))



