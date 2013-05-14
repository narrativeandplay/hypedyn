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

;; hypertext fiction editor

(require "../common/inspector.scm")
(require "../common/evaluator.scm")
(require "../kawa/ui/menu.scm")
(require "../kawa/ui/component.scm")
(require "../kawa/ui/container.scm")
(require "../kawa/ui/listbox.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/file.scm")
(require "../kawa/miscutils.scm")
(require "../kawa/ui/events.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/toolbar.scm")
(require "../kawa/ui/scrollpane.scm")
(require "../kawa/ui/label.scm")
(require "../kawa/ui/button.scm")
(require "../kawa/ui/splitpane.scm")
(require "../kawa/ui/undo.scm")

(require "../common/objects.scm") ;; ask
(require "../common/datatable.scm") ;; dirty?, reset-table, del, get, clear-dirty!
(require "../common/object-listview.scm")
(require "../common/fileio.scm")
(require "../common/graphics.scm")
(require "../common/window-menu.scm")
(require "../common/main-ui.scm")
(require "../common/preferences.scm")
(require "../common/list-helpers.scm") ;list-replace
(require "export.scm")
(require "config-options.scm")
(require "datastructure.scm")
(require "editlink.scm")
(require "reader.scm")
(require "nodeeditor.scm")
(require "htfileio.scm")
(require "node-graphview.scm")
(require 'srfi-1)
(require "hypedyn-undo.scm") ;; hd-postedit, hd-begin-update, hd-end-update
(require "about-hypedyn.scm")
(require "rules-manager.scm") ;; rmgr-init
(require "properties-ui.scm") ;; make-properties-ui, show-properties
(require "stats.scm") ;; display-stats

; export
(module-export close-hteditor-subwindows update-node-emphasis do-selectnode-list do-selectnode-graph
               clear-data clear-display populate-display add-recent-file update-dirty-state
               exceeded-node-limit?
               store-node-positions get-max-node-positions use-hteditor-ui
               check-init-sculptural
               
               ;; needed by hypedyn-undo.scm
               node-graph 
               update-node-style
               
               ;; for datastructure.scm (create-node hack)
               update-display-nodes
               
               ;; for import in htfileio.scm
               dodelnode
               delete-fact
               )

;;;; some global variables

; hteditor ui
(define hteditor-ui-panel #f)

; edit menu
(define m-edit-menu #f)

; node menu
(define m-node-menu #f)

; fact menu
(define m-fact-menu #f)

; view menu
(define m-view-menu #f)

; graph editors
(define node-graph #f)
(define anywhere-graph #f)

; node list
(define node-list #f)

; fact list
(define fact-list #f)

; menu items
(define m-file-openrecent #f)
(define m-file-import #f)
(define m-file-export-web #f)
(define m-file-export-standalone #f)
(define m-file-export-text #f)
(define m-file-export-js #f)
(define m-file-separator #f)
(define m-edit-docrule #f)
(define m-view-zoomin #f)
(define m-view-zoomout #f)
(define m-view-zoomreset #f)
(define m-view-showIDs #f)
(define m-view-allow-overlap #f)
(define m-view-snap-to-grid #f)
(define m-view-layout #f)
(define m-node-newnode #f)
(define m-node-newanywherenode #f)
(define m-node-editnode #f)
(define m-node-renamenode #f)
(define m-node-delnode #f)
(define m-node-dup #f)
(define m-fact-newfact #f)
(define m-fact-newfact-boolean #f)
(define m-fact-newfact-string #f)
(define m-fact-renamefact #f)
(define m-fact-delfact #f)

; buttons
(define toolbar-button-newnode #f)
(define toolbar-button-newanywherenode #f)
(define toolbar-button-editnode #f)
(define toolbar-button-renamenode #f)
(define toolbar-button-delnode #f)
(define toolbar-button-readnode #f)

; node count display
(define toolbar-nodecount-label #f)
(define toolbar-nodecount-textarea #f)

;;
;;;; build the UI
;;
; shutdown procedure, called in close-hteditor-ui
(define (close-hteditor-subwindows)
  ; need to close node editor and any other dialogs if necessary
  (nodeeditor-close)
  (nodereader-close)
  (close-inspectors))

(define (start-hteditor)
  (let ((main-menu (get-main-ui-menu))
        ; file menu
        (m-file (get-file-menu))
        (mf-separator (make-separator))
        (mf-separator2 (make-separator))
        (mf-openrecent (make-menu "Open Recent"))
        (mf-import (make-menu-item "Import..."))
        (mf-export-web (make-menu-item "Export as Applet..."))
        (mf-export-standalone (make-menu-item "Export Standalone..."))
        (mf-export-js (make-menu-item "Export for Web..."))
        (mf-export-text (make-menu-item "Export as Text..."))
        
        (mf-properties (make-menu-item "Properties"))
        
        ; edit menu
        (m-edit (make-menu "Edit"))
        (me-docrule (make-menu-item "Document rule"))

        ; view menu
        (m-view (make-menu "View"))
        (mv-zoomin (make-menu-item "Zoom In"))
        (mv-zoomout (make-menu-item "Zoom Out"))
        (mv-zoomreset (make-menu-item "Reset Zoom"))
        (mv-showIDs (make-checkbox-menu-item "Show IDs"))
        (mv-allow-overlap (make-checkbox-menu-item "Allow Overlap"))
        (mv-snap-to-grid (make-checkbox-menu-item "Snap to Grid"))
        (mv-layout (make-menu-item "Layout"))
        (mv-display-stats (make-menu-item "Display Stats"))
        
        ; node menu
        (m-node (make-menu "Node"))
        (mn-newnode (make-menu-item "New node"))
        (mn-newanywherenode (make-menu-item "New anywhere node"))
        (mn-editnode (make-menu-item "Edit node"))
        (mn-renamenode (make-menu-item "Rename node"))
        (mn-delnode (make-menu-item "Delete node"))
        (dup-node (make-menu-item "Duplicate Node"))
        
        ; facts menu
        (m-fact (make-menu "Fact"))
        (ma-newfact (make-menu "New"))
        (ma-newfact-boolean (make-menu-item "True/False"))
        (ma-newfact-string (make-menu-item "Text"))
        (ma-newfact-number (make-menu-item "Number"))
        (ma-renamefact (make-menu-item "Rename"))
        (ma-delfact (make-menu-item "Delete"))
        
        ; ui-panel
        (title-label "HypeDyn Editor")
        (ui-panel (make-panel))
        
        ; toolbar
        (toolbar-panel (get-main-ui-button-panel))
        
        ; buttons
        (tb-button-newnode (make-button "New node"))
        (tb-button-newanywherenode (make-button "New anywhere node"))
        (tb-button-editnode (make-button "Edit node"))
        (tb-button-renamenode (make-button "Rename node"))
        (tb-button-readnode (make-button "Read node"))
        (tb-button-delnode (make-button "Delete node"))
        (toolbar-label-nodecount-label (make-label))
        (toolbar-label-nodecount (make-textfield "" 12))
        
        ; panel to hold lists and graphs
        (f-panel (make-splitpane #f))
        
        ; panel to hold lists
        (lists-panel (make-splitpane #t))
        (lists-panel-upper (make-panel))
        (lists-panel-lower (make-panel))
        
        ; the node list
        (n-list (make-node-listview do-selectnode-list nodelist-onmouse))
        
        ; the fact list
        (a-list (make-fact-listview do-selectfact-list factlist-onmouse))
        
        ; panel to hold graphs
        (maps-panel (make-splitpane #t))
        (maps-panel-upper (make-panel))
        (maps-panel-lower (make-panel)))
    
    ; file menu
    (add-menuitem-at m-file mf-openrecent 2)
    (set-menuitem-component mf-openrecent #f)
    (add-component m-file mf-separator)
    (if (not (is-basic-mode?))
        (begin
          (add-component m-file mf-properties)
          (add-component m-file mf-separator2)
          (add-actionlistener mf-properties
                              (make-actionlistener
                               (lambda (source)
                                 (show-properties))))
          ))
    (add-component m-file mf-import)
    (add-actionlistener mf-import (make-actionlistener
                                   (lambda (source)
                                     (doimport))))
    (if (not (is-basic-mode?))
        (begin
          (if (java-reader?)
              (begin
                (add-component m-file mf-export-web)
                (add-actionlistener mf-export-web (make-actionlistener
                                                   (lambda (source)
                                                     (doexport-hypedyn-web))))
                (add-component m-file mf-export-standalone)
                (add-actionlistener mf-export-standalone (make-actionlistener
                                                          (lambda (source)
                                                            (doexport-standalone))))))
          (add-component m-file mf-export-js)
          (add-actionlistener mf-export-js (make-actionlistener
                                            (lambda (source)
                                              (doexport-js))))
    (add-component m-file mf-export-text)
    (add-actionlistener mf-export-text (make-actionlistener
                                       (lambda (source)
                                         (doexport-text))))
          ))
    
    ; edit menu
    (add-component main-menu m-edit)
    (if (show-docrule?)
        (begin
          (add-component m-edit me-docrule)
          (add-actionlistener me-docrule (make-actionlistener
                                          (lambda (source)
                                            (doeditdocrule))))))
    
    ;; undo
    (init-undo-system)
    (if (is-undo-enabled?)
        (begin
          (add-menu-action m-edit undo-action)
          (add-menu-action m-edit redo-action))
        )

    ; view menu
    (add-component main-menu m-view)
    (add-component m-view mv-zoomin)
    (add-actionlistener mv-zoomin (make-actionlistener
                                   (lambda (source)
                                     (dozoomin))))
    (set-menu-item-accelerator mv-zoomin #\1)
    (add-component m-view mv-zoomout)
    (add-actionlistener mv-zoomout (make-actionlistener
                                    (lambda (source)
                                      (dozoomout))))
    (set-menu-item-accelerator mv-zoomout #\2)
    (add-component m-view mv-zoomreset)
    (add-actionlistener mv-zoomreset (make-actionlistener
                                      (lambda (source)
                                        (dozoomreset))))
    (set-menu-item-accelerator mv-zoomreset #\0)
    (if (not (is-basic-mode?))
        (begin
          (add-component m-view mv-showIDs)
          (add-actionlistener mv-showIDs (make-actionlistener
                                          (lambda (source)
                                            (doshowIDs))))
          (set-checkbox-menu-item mv-showIDs (show-IDs?))))
    ;(add-component m-view mv-allow-overlap)
    (add-actionlistener mv-allow-overlap (make-actionlistener
                                          (lambda (source)
                                            (doallowoverlap))))
    (set-checkbox-menu-item mv-allow-overlap (allow-overlap?))
    (add-component m-view mv-snap-to-grid)
    (add-actionlistener mv-snap-to-grid (make-actionlistener
                                         (lambda (source)
                                           (dosnaptogrid))))
    (set-checkbox-menu-item mv-snap-to-grid (snap-to-grid?))
    (add-component m-view mv-layout)
    (add-actionlistener mv-layout (make-actionlistener
                                   (lambda (source)
                                     (dolayout))))
    (set-component-enabled mv-layout (snap-to-grid?))
    
    ; display stats for story analysis
    (if (display-stats?)
        (begin
          (add-component m-view mv-display-stats)
          (add-actionlistener mv-display-stats (make-actionlistener
                                          (lambda (source)
                                            (display-stats))))))
    
    ; node menu
    (add-component main-menu m-node)
    (if (not (sculptural?))
        (begin
          (add-component m-node mn-newnode)
          (add-actionlistener mn-newnode (make-actionlistener
                                          (lambda (source)
                                            (donewnode #f))))
          (set-menu-item-accelerator mn-newnode #\M)))
    (if (not (is-basic-mode?))
        (begin
          (add-component m-node mn-newanywherenode)
          (add-actionlistener mn-newanywherenode (make-actionlistener
                                                  (lambda (source)
                                                    (donewnode #t))))))
    (add-component m-node mn-editnode)
    (add-actionlistener mn-editnode 
                        (make-actionlistener (lambda (source) (doeditnode))))
    (set-menu-item-accelerator mn-editnode #\E)
    (set-menuitem-component mn-editnode #f)
    (add-component m-node mn-renamenode)
    (add-actionlistener mn-renamenode 
                        (make-actionlistener (lambda (source) (dorenamenode))))
    (set-menu-item-accelerator mn-renamenode #\R)
    (set-menuitem-component mn-renamenode #f)
    (add-component m-node mn-delnode)
    (add-actionlistener mn-delnode 
                        (make-actionlistener (lambda (source) (dodelnode))))
    (set-menu-item-accelerator mn-delnode #\D)
    (set-menuitem-component mn-delnode #f)
    (add-component m-node dup-node)
    (add-actionlistener dup-node 
                        (make-actionlistener (lambda (e) (do-dup-node))))
    (set-menuitem-component dup-node #f)
    
    ; fact menu
    (if (show-facts?)
        (begin
          (add-component main-menu m-fact)
          (add-component m-fact ma-newfact)
          (add-component ma-newfact ma-newfact-boolean)
          (add-actionlistener ma-newfact-boolean (make-actionlistener
                                               (lambda (source)
                                                 (donewfact 'boolean "True/False"))))
          (add-component ma-newfact ma-newfact-string)
          (add-actionlistener ma-newfact-string (make-actionlistener
                                               (lambda (source)
                                                 (donewfact 'string "Text"))))
          (add-component ma-newfact ma-newfact-number)
          (add-actionlistener ma-newfact-number (make-actionlistener
                                               (lambda (source)
                                                 (donewfact 'number "Number"))))
          
          (add-component m-fact ma-renamefact)
          (add-actionlistener ma-renamefact (make-actionlistener (lambda (source) (dorenamefact))))
          (set-menuitem-component ma-renamefact #f)
          (add-component m-fact ma-delfact)
          (add-actionlistener ma-delfact (make-actionlistener (lambda (source) (dodelfact))))
          (set-menuitem-component ma-delfact #f)))
    
    ; ui-panel
    (set-container-layout ui-panel 'border)
    
    ; buttons
    
    ; first a separator
    ;(add-toolbar-separator toolbar-panel)
    
    ; node menu buttons
    (if (not (sculptural?))
        ; sculptural mode has only one non-anywhere node, which is created automatically
        (begin
          (add-component toolbar-panel tb-button-newnode)
          (add-actionlistener tb-button-newnode (make-actionlistener
                                                 (lambda (source)
                                                   (donewnode #f))))))
    (if (not (is-basic-mode?))
        ; basic mode has no anywhere nodes
        (begin
          (add-component toolbar-panel tb-button-newanywherenode)
          (add-actionlistener tb-button-newanywherenode (make-actionlistener
                                                         (lambda (source)
                                                           (donewnode #t))))))
    (add-component toolbar-panel tb-button-editnode)
    (add-actionlistener tb-button-editnode (make-actionlistener (lambda (source) (doeditnode))))
    (add-component toolbar-panel tb-button-renamenode)
    (add-actionlistener tb-button-renamenode (make-actionlistener (lambda (source) (dorenamenode))))
    (add-component toolbar-panel tb-button-delnode)
    (add-actionlistener tb-button-delnode (make-actionlistener (lambda (source) (dodelnode))))

    ; separator
    ;(add-toolbar-separator toolbar-panel)
    
    ; read node - not sure if this is a good idea or not, temporarily remove - alex
    ;(add-component toolbar-panel tb-button-readnode)
    ;(add-actionlistener tb-button-readnode (make-actionlistener (lambda (source) (doreadnode))))
    
    ; separator
    ;(add-toolbar-separator toolbar-panel)
    
    ; node count
    (add-component toolbar-panel toolbar-label-nodecount-label)
    (set-text toolbar-label-nodecount-label " Node count:")
    (add-component toolbar-panel toolbar-label-nodecount)
    (set-text-component toolbar-label-nodecount #f #t)
    
    ; set resize weight and layout options for f-panel
    ;(set-splitpane-resize-weight f-panel 0.75) ; leaving as 0 lets left-hand panel stay same size
    (set-splitpane-continuous-layout f-panel #t)
    (set-splitpane-onetouchexpandable f-panel #t)
    ; for basic mode, set the size of the f-panel
    (if (is-basic-mode?)
        (set-component-preferred-size f-panel 400 300))
    
    ; add a splitpane to the left side of f-panel for lists - need to be able to disable this
    (if (show-facts?)
        (add-splitpane-component f-panel lists-panel #t))

    ; set the size, divider location and resize weight for lists-panel
    (set-component-preferred-size lists-panel 120 430)
    (set-splitpane-divider-location lists-panel 300)
    ;(set-splitpane-resize-weight lists-panel 0.75) ; this lets the two panels resize together
    (set-splitpane-continuous-layout lists-panel #t)
    (set-splitpane-onetouchexpandable lists-panel #t)
    
    ; panel to hold lists of nodes
    (set-container-layout lists-panel-upper 'vertical)
    (set-component-preferred-size lists-panel-upper 120 100) ; was 120x200
    (if (show-facts?)
        (add-splitpane-component lists-panel lists-panel-upper #t)
        (add-splitpane-component f-panel lists-panel-upper #t))
        
    ; make a list for nodes
    (ask n-list 'init)
    (add-component lists-panel-upper (ask n-list 'get-component))

    ; panel to hold lists of facts - need to be able to disable this
    (set-container-layout lists-panel-lower 'vertical)
    (set-component-preferred-size lists-panel-lower 120 100) ; was 120x200
    (add-splitpane-component lists-panel lists-panel-lower #f)
    
    ; make a list for facts
    (ask a-list 'init)
    (add-component lists-panel-lower (ask a-list 'get-component))

    ;; add a splitpane to the frame, with centering, to hold map views
    (if (not (is-basic-mode?))
          (add-splitpane-component f-panel maps-panel #f))

    ; set the size, divider location and resize weight
    (set-component-preferred-size maps-panel 400 430)
    (set-splitpane-divider-location maps-panel (if (sculptural?) 100 300))
    (set-splitpane-resize-weight maps-panel (if (sculptural?) 0 1)) ; 0/1 lets the top/bottom panel stay the same size
    (set-splitpane-continuous-layout maps-panel #t)
    (set-splitpane-onetouchexpandable maps-panel #t)

    ; create graph editor for nodes
    (set! node-graph (make-node-graphview 400 (if (sculptural?) 100 300) 4000 (if (sculptural?) 1000 3000)  graph-callback #f use-emph))
    (ask node-graph 'init)

    (let ((the-scrollpane (make-scrollpane (ask node-graph 'get-component))))
      (if (not (is-basic-mode?))
          ; for advanced version, add the anywhere nodes graph using a splitpane
          (add-splitpane-component maps-panel the-scrollpane #t)
          ; for basic version, just add the regular nodes graph
          (add-splitpane-component f-panel the-scrollpane #f))
      (scroll-set-horizontal-unit-increment the-scrollpane 20)
      (scroll-set-vertical-unit-increment the-scrollpane 20))

    ; create graph editor for anywhere nodes
    (set! anywhere-graph (make-node-graphview 400 (if (sculptural?) 300 100) 4000 (if (sculptural?) 3000 1000) graph-callback #t use-emph))
    (ask anywhere-graph 'init)
    (let ((the-scrollpane (make-scrollpane (ask anywhere-graph 'get-component))))
      (add-splitpane-component maps-panel the-scrollpane #f)
      (scroll-set-horizontal-unit-increment the-scrollpane 20)
      (scroll-set-vertical-unit-increment the-scrollpane 20))

    ; set node display callback
    (set-nodecount-display-callback!
     (lambda (newcount)
       (begin
         (set-text toolbar-label-nodecount
                   (string-append
                    (number->string newcount)
                    (if has-node-limit
                        (string-append " of " (number->string max-nodes))
                        "")
                    (if (exceeded-node-limit?)
                        ": over limit!"
                        "")))
         (check-node-limit newcount))))

    ; create node editor
    ; callbacks for updating link display in graph, populating nodes list,
    ; renaming a line, (the node graph for deleting), and updating node style
    ; should be safe to assume rename-line and del-line are only called for node-graph?
    (create-nodeeditor (lambda () (ask node-list 'populate-nodes-list))
                       (lambda (line-ID newname) (ask node-graph 'rename-line line-ID newname))
                       node-graph
                       update-node-style
                       undo-manager)

    ; create node reader window
    (create-nodereader-window)
    
    ;; create the panels (the components inside the editlink editnode dialogs)
    ;(create-then-action-panel)
    ;(create-facts-main-panel)
    
    ;(create-if-condition-panel)
    ;(create-actions-main-panel)
    (rmgr-init)
    
    ;(create-update-text-action-panel)
    
    ; create link editor
    (create-editlink-dialog (get-nodeeditor-frame))
    ;(create-editnode-dialog (get-nodeeditor-frame))
    
    (make-properties-ui)

    ; store any references that are needed
    (set! hteditor-ui-panel f-panel)
    (set! node-list n-list)
    (set! fact-list a-list)
    (set! m-file-import mf-import)
    (set! m-file-export-text mf-export-text)
    (set! m-file-export-web mf-export-web)
    (set! m-file-export-standalone mf-export-standalone)
    (set! m-file-export-js mf-export-js)
    (set! m-file-separator mf-separator)
    (set! m-file-openrecent mf-openrecent)
    (set! m-edit-menu m-edit)
    (set! m-node-menu m-node)
    (set! m-node-newnode mn-newnode)
    (set! m-node-newanywherenode mn-newanywherenode)
    (set! m-node-editnode mn-editnode)
    (set! m-node-renamenode mn-renamenode)
    (set! m-node-delnode mn-delnode)
    (set! m-fact-menu m-fact)
    (set! m-fact-newfact ma-newfact)
    (set! m-fact-newfact-boolean ma-newfact-boolean)
    (set! m-fact-newfact-string ma-newfact-string)
    (set! m-fact-renamefact ma-renamefact)
    (set! m-fact-delfact ma-delfact)
    (set! m-view-menu m-view)
    (set! m-view-zoomin mv-zoomin)
    (set! m-view-zoomout mv-zoomout)
    (set! m-view-zoomreset mv-zoomreset)
    (set! m-view-showIDs mv-showIDs)
    (set! m-view-allow-overlap mv-allow-overlap)
    (set! m-view-snap-to-grid mv-snap-to-grid)
    (set! m-view-layout mv-layout)
    (set! toolbar-button-newnode tb-button-newnode)
    (set! toolbar-button-newanywherenode tb-button-newanywherenode)
    (set! toolbar-button-editnode tb-button-editnode)
    (set! toolbar-button-renamenode tb-button-renamenode)
    (set! toolbar-button-delnode tb-button-delnode)
    (set! toolbar-button-readnode tb-button-readnode)
    (set! toolbar-nodecount-label toolbar-label-nodecount-label)
    (set! toolbar-nodecount-textarea toolbar-label-nodecount)
    (set! m-node-dup dup-node)
    
    (enable-zoom)
    
    ; create the about window
    (create-about-window (get-main-ui-frame))))

;;
;;;; recent files menu
;;

; open recent file
(define (doopenrecent newfilename)
  (if (confirm-save)                     ;; check if data has changed
      (open-file-by-name newfilename)))  ;; safe, so proceed

; add items to the "recent files" menu
; takes a full filepath
(define-constant max-recent-files 10)
(define recent-files '())
(define (add-recent-file in-filename)
  (if (file-exists? in-filename)
      (let* ((the-filename-string (get-file-name in-filename))
             (mf-openrecent-newitem (make-menu-item the-filename-string))
             (the-file-path (path-file in-filename))
             (is-the-file? (lambda (this-file)
                             (equal? this-file the-file-path))))
        ; add the menu item
        (add-menuitem-at m-file-openrecent mf-openrecent-newitem 0)
        (add-actionlistener mf-openrecent-newitem (make-actionlistener
                                                   (lambda (source)
                                                     (doopenrecent in-filename))))

        ; first remove any duplicate (will be at most 1)
        (let ((the-index (list-index is-the-file? recent-files)))
          (if the-index
              (begin
                (remove-menuitem-at m-file-openrecent (- (get-menu-component-count m-file-openrecent) the-index 1))
                (set! recent-files (remove is-the-file? recent-files)))))
        
        ; add to the list
        (set! recent-files (append recent-files (list the-file-path)))
        
        ; enable menu
        (set-menuitem-component m-file-openrecent #t)
        
        ; make sure there aren't too many, if so remove the last
        (if (> (get-menu-component-count m-file-openrecent) max-recent-files)
            (begin
              (remove-menuitem-at m-file-openrecent max-recent-files)
              (set! recent-files (drop recent-files 1)))))))

;;
;; preferences
;;

; get preferences
(define (get-hteditor-prefs)
  (let ((the-filelist (get-pref 'recent-files))
        (the-last-exported-dir (get-pref 'last-exported-dir)))
    (if the-filelist
        (map (lambda (this-file)
               (if (and this-file (not (is-null? this-file)))
                   (add-recent-file (make-file this-file))))
             the-filelist))
    (if (and the-last-exported-dir (not (is-null? the-last-exported-dir)))
        (set-last-exported-dir! (make-file the-last-exported-dir)))))

; save preferences
(define (set-hteditor-prefs!)
  (put-pref! 'recent-files recent-files)
  (let ((the-last-exported-dir (get-last-exported-dir)))
    (if (not (is-null? the-last-exported-dir))
        (put-pref! 'last-exported-dir (path-file the-last-exported-dir)))))


;;
;; nodes
;;

; check if we've hit the node limit
(define-private has-node-limit #f)
(define max-nodes 13)
(define (check-node-limit in-count)
  (if has-node-limit
      (begin
        ; if we're reached the limit, disable "new node" button
        (if (>= in-count max-nodes)
            (begin
              (set-button toolbar-button-newnode #f)
              (set-menuitem-component m-node-newnode #f)
              (if (not (is-basic-mode?))
                  (begin
                    (set-button toolbar-button-newanywherenode #f)
                    (set-menuitem-component m-node-newanywherenode #f))))
            (begin
              (set-button toolbar-button-newnode #t)
              (set-menuitem-component m-node-newnode #t)
              (if (not (is-basic-mode?))
                  (begin
                    (set-button toolbar-button-newanywherenode #t)
                    (set-menuitem-component m-node-newanywherenode #t)))))
        ; if we've exceeded the limit (ie. by importing), disable "save" and "saveas" menu items
        (if (> in-count max-nodes)
            (begin
              (enable-save #f)
              (enable-saveas #f))
            (begin
              (enable-save #t)
              (enable-saveas #t))))))

; check if we've exceeded the limit
(define (exceeded-node-limit?)
  (and has-node-limit (> (get-node-count) max-nodes)))

(define (newnode-redo node-name anywhere? nodeID node-sexpr)
  (display "redo sexpr ")(display node-sexpr)(newline)
  (eval-sexpr node-sexpr)
  (update-dirty-state))

(define (newnode-undo nodeID)
  (delete-node nodeID))

;;;; Duplicate objects

(define (duplicate-action actionID parentID dup-offset-ID #!key post-undo?)
  (let* ((action-obj (get 'actions actionID))
         (name (ask action-obj 'name))
         (type (ask action-obj 'type))
         (expr (ask action-obj 'expr))
         (new-actionID (+ actionID dup-offset-ID)))
    
    ;; replace text action should point to new duplicated link
    (if (equal? (car expr) 'replace-link-text)
        (begin
          (set! expr (list-replace expr 3 (+ (list-ref expr 3) dup-offset-ID)))
          ))
         
    ;; add anywhere link action should point to new duplicated node
    (if (equal? (car expr) 'add-anywhere-link)
        (begin
          (set! expr (list-replace expr 1 (+ (list-ref expr 1) dup-offset-ID)))
          ))
;;         
    (create-action name type expr parentID new-actionID)
    ))

(define (duplicate-condition condID parentID dup-offset-ID #!key post-undo?)
  (let* ((cond-obj (get 'conditions condID))
         (name (ask cond-obj 'name))
         (type (ask cond-obj 'type))
         (targetID (ask cond-obj 'targetID))
         (operator (ask cond-obj 'operator))
         (numfact-args (ask cond-obj 'numfact-args))
         (new-condID (+ condID dup-offset-ID)))
    (create-typed-condition2 name type targetID operator parentID 
                             fixedID: new-condID
                             numfact-args: numfact-args)
    ))

(define (duplicate-rule ruleID parentID dup-offset-ID #!key post-undo?)
  
  (let* ((rule-obj (get 'rules ruleID))
         (name (ask rule-obj 'name))
         (type (ask rule-obj 'type))
         (and-or (ask rule-obj 'and-or))
         (negate? (ask rule-obj 'negate?))
         (fall-through? (ask rule-obj 'fall-through?))
         (new-ruleID (+ ruleID dup-offset-ID)))

    (create-typed-rule3 name type and-or negate? parentID 
                        fixedID: new-ruleID 
                        fall-through?: fall-through?)
    
    ;; duplicate the conditions 
    (map (lambda (condID)
           (duplicate-condition condID new-ruleID dup-offset-ID post-undo?: #f)
           ) (ask rule-obj 'conditions))
    
    ;; duplicate the actions 
    (map (lambda (actionID)
           (duplicate-action actionID new-ruleID dup-offset-ID post-undo?: #f)
           ) (ask rule-obj 'actions))
    
    ;; update graph view 
    (add-follow-link-rule-display new-ruleID)
    (add-show-popup-rule-display new-ruleID)
    ))

(define (duplicate-link linkID parentID dup-offset-ID #!key post-undo?)

  (let* ((link-obj (get 'links linkID))
         (name (ask link-obj 'name))
         (start-index (ask link-obj 'start-index))
         (end-index (ask link-obj 'end-index))
         (new-linkID (+ linkID dup-offset-ID))
         (rule-lst (ask link-obj 'rule-lst))
         )
    (create-link name parentID -1
                 start-index end-index
                 #f -1 #f -1
                 "" #f new-linkID)

    ;; duplicate the rules on this link
    (map (lambda (ruleID)
           (duplicate-rule ruleID new-linkID dup-offset-ID post-undo?: #f)
           ) rule-lst)
    ))

(define (duplicate-node nodeID)
  
  (let-values (((max-x max-y max-anywhere-x max-anywhere-y)
                (get-max-node-positions)))
    ;; set import offsets (uniqueID and positions)
    (let-values (((dup-offset-ID dup-offset-x dup-offset-anywhere-x)
                  (get-duplicate-offsets max-x max-y
                                         max-anywhere-x max-anywhere-y)))
      
      (let* ((node-obj (get 'nodes nodeID))
             (name (ask node-obj 'name))
             (content (ask node-obj 'content))
             (anywhere (ask node-obj 'anywhere?))
             (links (ask node-obj 'links))
             (new-nodeID (+ nodeID dup-offset-ID))
             (rule-lst (ask node-obj 'rule-lst))
             )

        (define (actual-duplicate)
          (create-node name content
                       (calc-new-node-x anywhere)
                       initial-y anywhere update-display-nodes new-nodeID)

          ;; duplicate links in this node
          (map (lambda (linkID)
                 (duplicate-link linkID new-nodeID dup-offset-ID post-undo?: #f)
                 ) links)

          ;; duplicate rules on this node
          (map (lambda (ruleID)
                 (duplicate-rule ruleID new-nodeID dup-offset-ID post-undo?: #f))
               rule-lst)
          )
        
        (actual-duplicate)

        (ask node-list 'select-node new-nodeID)
          
        (hd-postedit 
         undo-manager
         (make-undoable-edit 
          "Duplicate Node"
          (lambda () ;; undo
            ;; NOTE: the postedit called within dodelnode would not be triggered 
            ;; since we do not allow postedit when undoing
            (display "new nodeID ")(display new-nodeID)(newline)
            (dodelnode new-nodeID)
            (display "undoing duplicate ")(newline)
            )
          (lambda () ;; redo
            (display "redoing duplicate ")(newline)
            
            ;; this would not post undo edit since we don't allow that 
            (actual-duplicate)
            )
          ))
                                     
        ))
    )
  )

;; called by menu item Duplicate Node
(define (do-dup-node)
  (if (not (= selected-nodeID -1))
      (duplicate-node selected-nodeID)))
            

; starting position for first node in the story
(define-constant initial-x 80)
(define-constant initial-y 45)

; calculate the new x position
(define (calc-new-node-x in-anywhere)
  ; get the max positions
  (let-values (((max-x max-y max-anywhere-x max-anywhere-y)
                (get-max-node-positions)))
    ; clamp to grid if necessary
    (let ((raw-x (if in-anywhere
                     (+ max-anywhere-x initial-x)
                     (+ max-x initial-x))))
      (if (snap-to-grid?)
          (* initial-x (round (div raw-x initial-x)))
          raw-x))))

; create a new node
(define (donewnode anywhere)
  (let ((newmsg (if anywhere "New anywhere node" "New node"))
        (logmsg (if anywhere "NewAnywhereNode" "NewNode")))
    (define new-nodename 
      (make-input-dialogbox-custom (get-main-ui-frame) "" newmsg "Node name"))
    
    ;; if canceled in dialogbox, #!null is returned
    (if (not (is-null? new-nodename))
        (begin
          ; create the node
          (define newnode-x (calc-new-node-x anywhere))
          (define newnode-y initial-y) ;(if anywhere (+ max-anywhere-y initial-y) (+ max-y initial-y)))
          (define newnode-ID
            (create-node new-nodename ""
                         newnode-x newnode-y
                         anywhere update-display-nodes))
          
          (ask node-list 'select-node newnode-ID)
          
          ;; if anywhere add a rule with add-anywhere-link action
          (if anywhere
              (begin
                (define new-ruleID (create-typed-rule2 "Add Anywhere Link" 'node 'and #f newnode-ID))
                (create-action "Enable Link" 'anywhere-check
                               (list 'add-anywhere-link newnode-ID)
                               new-ruleID)))
          
          (define newnode (get 'nodes newnode-ID))
          (define newnode-sexpr (ask newnode 'to-save-sexpr))

          ; update main window label to show file is dirty
          (update-dirty-state)

          ;; add the undoable edit
          (hd-postedit
           undo-manager
           (make-undoable-edit
            "Add Node"
            (lambda () ;; undo
              (newnode-undo newnode-ID)
              )
            (lambda () ;; redo
              (newnode-redo new-nodename anywhere newnode-ID newnode-sexpr)
              (update-display-nodes newnode-ID new-nodename
                                    newnode-x newnode-y ;actual-x y
                                    anywhere)
              )
            ))
          )) ;; end of if
    ))

; update node displays
(define (update-display-nodes new-nodeID name x y anywhere)
  (ask node-list 'add-node new-nodeID name)
  (if anywhere
      (begin
        (ask anywhere-graph 'add-node new-nodeID name x y)
        (ask anywhere-graph 'handle-resize))
      (begin
        (ask node-graph 'add-node new-nodeID name x y)
        (ask node-graph 'handle-resize)))) ;; anywhere)))

; edit a node
(define (doeditnode)
  (nodeeditor-edit selected-nodeID))

; read a node
(define (doreadnode)
  (forget-history)
  (reset-environment)
  (goto-node selected-nodeID #f)
  (ask (get 'nodes selected-nodeID) 'set-visited! 1))

(define (rename-node nodeID newname)
  (define remember-selected-nodeID selected-nodeID)
  (define target-node (get 'nodes nodeID))
  
  ; in data structure
  (ask target-node 'set-name! newname)

  ; update main window label to show file is dirty
  (update-dirty-state)

  ; in nodeeditor, if this is the node being edited
  (if (eq? nodeID (get-edited-nodeID))
      (update-nodeeditor-frame-title))

  ; in node list (cheat!)
  (ask node-list 'populate-nodes-list)
  (ask node-list 'select-node remember-selected-nodeID) ; make sure we don't lose the selection

  ; and in graph
  (if (ask target-node 'anywhere?)
      (ask anywhere-graph 'rename-node nodeID)
      (ask node-graph 'rename-node nodeID)))

;; add the undoable edit for rename node
(define (post-rename-node-undoable-event nodeID oldname newname)
  (hd-postedit 
   undo-manager
   (make-undoable-edit 
    "Rename Node"
    (lambda () (rename-node nodeID oldname)) ;; undo rename (set to oldname)
    (lambda () (rename-node nodeID newname)) ;; redo rename (set to newname)
    )))

; rename a node
(define (dorenamenode)
  ; show dialogue box asking for new name of node
  (let* ((remember-selected-nodeID selected-nodeID)
         (selected-node (get 'nodes selected-nodeID))
         (oldname (ask selected-node 'name))
         (newname (make-input-dialogbox-custom (get-main-ui-frame) oldname "Rename node" "Enter the new name for this node"))
         )
    
    ;; make-input-dialogbox-check-empty ensure no "" empty string 
    (if (not (is-null? newname)) ;; null when canceled
        (begin
          (rename-node selected-nodeID newname)
          (post-rename-node-undoable-event selected-nodeID oldname newname)
          ))
    ))

; currently selected node
(define selected-nodeID '())

(define (get-selected-nodeID) selected-nodeID)

; select the node
(define (selectnode nodeID)
  ; remember selected node
  (set! selected-nodeID nodeID)

  ; a node is now selected, so enable node buttons/menu items
  (enable-node-functions #t)
  )

; deselect node
(define (deselectnode)
  ; forget selected node
  (set! selected-nodeID '())

  ; no nodes are selected, so disable node buttons/menu items
  (enable-node-functions #f)
  )

; enable/disable node-related buttons and menu items
(define (enable-node-functions flag)
  (set-button toolbar-button-editnode flag)
  (set-menuitem-component m-node-editnode flag)
  (set-button toolbar-button-renamenode flag)
  (set-menuitem-component m-node-renamenode flag)
  (set-menuitem-component m-node-dup flag)
  
  ; enable delete button/menu only if the flag is true,
  ; and either we're not in sculptural mode, or
  ; this is an anywhere node (ie. can't delete non-anywhere
  ; nodes in sculptural mode)
  (let ((can-delete (and flag
                         (or (not (sculptural?))
                             (ask (get 'nodes selected-nodeID) 'anywhere?)))))
    (set-button toolbar-button-delnode can-delete)
    (set-menuitem-component m-node-delnode can-delete))
  (set-button toolbar-button-readnode flag)
  )

; delete currently selected node
;; node-to-del if specified would delete that node
(define (dodelnode #!optional node-to-del-ID)
  
  ;; if node-to-del-ID not specified, we delete the selected node
  (if (not node-to-del-ID)
      (set! node-to-del-ID (get-selected-nodeID)))
  
  ; first save the node contents if its being edited
  (if (= (get-edited-nodeID) node-to-del-ID)
      (nodeeditor-save))
  
  (define cached-nodeID node-to-del-ID)
  (define node-to-del (get 'nodes cached-nodeID))
  (define nodename (ask node-to-del 'name))
  (define node-anywhere (ask node-to-del 'anywhere?))
  (define node-sexpr (ask node-to-del 'to-save-sexpr))
  
  ; store the position
  (ask (if node-anywhere anywhere-graph node-graph) 'store-node-position cached-nodeID node-to-del)
  (define actual-x (ask node-to-del 'get-x))
  (define actual-y (ask node-to-del 'get-y))
  
  ; remember if this was the start node
  (define was-start-node (= (get-start-node) cached-nodeID))
  
  ;; get the list of follow-link actions going to this node
  (define actionID-lst
    (let ((action-lst (get-list 'actions)))
      (if action-lst
          (filter (lambda (actionID) ;;find action in this rule
                    (define action (get 'actions actionID))
                    (define sexpr (ask action 'expr))
                    (and (equal? (car sexpr) 'follow-link)
                         (equal? (list-ref sexpr 4) node-to-del-ID))
                    )
                  ;; get list of action
                  (map (lambda (o) (car o)) action-lst))
          '())))
  
  ;; wrap delete link and delete node in one operation
  ;; delete-node invokes delete-link which has its own hd-postedit
  (hd-begin-update undo-manager)
  
  ;; delete follow link actions to this node
  (map delete-action actionID-lst)
  
  ;; does not contain any undo postedit
  (delete-node node-to-del-ID)
  
      ;; add the undoable edit
  (hd-postedit
   undo-manager
   (make-undoable-edit
    "Delete Node"
    (lambda () ;; redo new node is undo for delnode - not quite, also need to restore the start node if necessary
      (display "undoing delete node")(newline)
      (newnode-redo nodename node-anywhere cached-nodeID node-sexpr)
      (if was-start-node (set-start-node! cached-nodeID))
      (update-display-nodes cached-nodeID nodename
                            actual-x actual-y
                            node-anywhere))
    (lambda () ;; undo new node is redo for delnode
      (display "redoing delete node")(newline)
      (newnode-undo cached-nodeID))
    ))
  
  (hd-end-update undo-manager undo-action redo-action)) ;; end of dodelnode

; delete a node
(define (delete-node nodeID)
  (let ((thenode (get 'nodes nodeID)))
    (if thenode
        (begin
          ; if node is open in editor or reader, close them
          (if (= (get-edited-nodeID) nodeID)
              (begin
                (display "nodeID match so close it ")(display (get-edited-nodeID))(newline)
                (nodeeditor-close)
                (set-edited-nodeID! '()))
;;              (begin
;;                (display "nodeID DOESNOT match so dont close it ")(display (get-edited-nodeID))(newline)
;;                )
              )
          (if (= (get-read-nodeID) nodeID)
              (begin
                (nodereader-stop)
                (forget-history)))

          ; if this is the start node, clear start node
          (if (= (get-start-node) nodeID)
              (set-start-node! #f))

          ; if this is the selected node, clear selected node and disable node buttons/menu items
          (if (= selected-nodeID nodeID)
              (begin
                (set! selected-nodeID '())
                (enable-node-functions #f)))

          ; delete all links in this node
          (map (lambda (l)
                 (let ((thislink (get 'links l)))
                   (delete-link l #t node-graph
                                update-node-style)))
               (ask thenode 'links))

          ; delete node from data-table
          (deletenode nodeID)

          ; update main window label to show file is dirty
          (update-dirty-state)

          ; remove from UI - cheat by just repopulating the list
          (ask node-list 'populate-nodes-list)

          ; also delete from graph - node name in graph is now nodeName not nodeID
          (let ((the-graph (if (ask thenode 'anywhere?) anywhere-graph node-graph)))
            (ask the-graph 'del-node nodeID))))))

;;
;; node list
;; 

(define (make-node-listview selectnode-callback onmouse-callback)
  (let* ((parent-obj (make-object-listview selectnode-callback #t 'nodes 'name string>=?))
         (this-obj (new-object parent-obj)))

    ; initialize
    (define (init)
      (ask parent-obj 'init)
      (if (procedure? onmouse-callback)
          (add-mouselistener (ask parent-obj 'get-list)
                             (make-mouselistener onmouse-callback))))
    
    ; update node display in list view
    (define (add-node new-nodeID name)
      (let ((display-name (if (show-IDs?)
                              (string-append name
                                             " ("
                                             (number->string new-nodeID)
                                             ")")
                              name)))
        (if (= new-nodeID (get-start-node))
            (ask parent-obj 'add-object new-nodeID (string-append display-name " (start node)"))
            (ask parent-obj 'add-object new-nodeID display-name))))

    ; populate the nodes list
    (define (populate-nodes-list)
      (let ((the-nodes (get-list 'nodes)))
        (ask parent-obj 'clear)
        (if the-nodes
            (map (lambda (n)
                   (let* ((thisnode (cdr n))
                          (thisnode-name (ask thisnode 'name))
                          (thisnode-ID (ask thisnode 'ID)))
                     (add-node thisnode-ID thisnode-name)))
                 the-nodes)))
      (if (not (eq? '() selected-nodeID))
          (ask parent-obj 'select-object selected-nodeID)))

    ; message handling
    (obj-put this-obj 'init
             (lambda (self)
               (init)))
    (obj-put this-obj 'populate-nodes-list
             (lambda (self)
               (populate-nodes-list)))
    (obj-put this-obj 'add-node
             (lambda (self new-nodeID name)
               (add-node new-nodeID name)))
    (obj-put this-obj 'select-node
             (lambda (self in-nodeID)
               (ask parent-obj 'select-object in-nodeID)))
    (obj-put this-obj 'deselect-node
             (lambda (self)
               (ask parent-obj 'deselect-object)))
    this-obj))

; selected a node in the node list
(define (do-selectnode-list nodeID)
  (if (not (null? nodeID))
      (let* ((thenode (get 'nodes nodeID))
             (anywhere (ask thenode 'anywhere?))
             (the-graph (if anywhere anywhere-graph node-graph))
             (the-othergraph (if anywhere node-graph anywhere-graph)))
        (selectnode nodeID)
        ; make sure no nodes are selected in the other graph view
        (if (not (is-basic-mode?))
            (ask the-othergraph 'select-node -1))
        (ask the-graph 'select-node nodeID))
      (begin
        ; deselected a node in the list
        (deselectnode)
        ; make sure graph views are also deselected
        (if (not (is-basic-mode?))
            (ask anywhere-graph 'select-node -1))
        (ask node-graph 'select-node -1))))

; mouse event in node list - for double-clicking
(define (nodelist-onmouse e)
  (let ((event-type (get-mouseevent-type e)))
    (if (eq? event-type 'left-clicked)
        (let ((event-click-count (get-mouseevent-click-count e)))
          (if (= 2 event-click-count)
              (begin
                (doeditnode)
                )
              )))))


;;
;; facts
;;

; create a new fact
(define (donewfact type type-string)
  (let ((new-factname (make-input-dialogbox-custom (get-main-ui-frame) "" (string-append "New " type-string " fact") "Fact name")))
    (if (not (is-null? new-factname))
        (begin
          ;; create the fact
          (define new-factID (create-fact new-factname type)) ; create the fact
          (ask fact-list 'add-fact new-factID new-factname) ; and add to fact list
          (update-dirty-state) ; update main window label to show file is dirty
          (define new-fact (get 'facts new-factID))
          (define newfact-sexpr (ask new-fact 'to-save-sexpr))
          
          ;; add the undoable edit
          (hd-postedit
           undo-manager
           (make-undoable-edit
            "Add Fact"
            (lambda () ;; undo
              (newfact-undo new-factID))
            (lambda () ;; redo
              (newfact-redo newfact-sexpr new-factID new-factname))
            ))
          
          ))))

(define (newfact-redo fact-sexpr factID factname)
  (eval-sexpr fact-sexpr)
  ;; factID debug check
  (display "same factID created? ")(display (get 'fact factID))(newline)
  (display "fact-sexpr ")(display fact-sexpr)(newline)
  (ask fact-list 'add-fact factID factname)
  (update-dirty-state)
  )

(define (newfact-undo factID)
  (selectfact factID) ;; make sure selected-factID is new-factID
  (delete-fact selected-factID) ;; deletes the link pointed to by selected-factID
  )

(define (rename-fact-undo-redo factID newname)
  (let* ((remember-selected-factID selected-factID)
         (renamed-fact (get 'facts factID)))
    ; update fact name
    ; in data structure
    (ask renamed-fact 'set-name! newname)

    ; update main window label to show file is dirty
    (update-dirty-state)

    ; in fact list (cheat!)
    (ask fact-list 'populate-facts-list)
    (ask fact-list 'select-fact remember-selected-factID)))

; rename an fact
(define (dorenamefact)
  ; show dialogue box asking for new name of fact
  (let* ((remember-selected-factID selected-factID)
         (selected-fact (get 'facts selected-factID))
         (oldname (ask selected-fact 'name))
         (newname (make-input-dialogbox-custom (get-main-ui-frame) oldname "Rename fact" "Enter the new name for this fact")))
    (if (not (is-null? newname))
        (begin
          ; update fact name

          ; in data structure`
          (ask selected-fact 'set-name! newname)

          ; update main window label to show file is dirty
          (update-dirty-state)

          ; in fact list (cheat!)
          (ask fact-list 'populate-facts-list)
          (ask fact-list 'select-fact remember-selected-factID)
          
          ;; add undoable actions
          (define cache-factID selected-factID) ;; copy the factID
          (hd-postedit
           undo-manager
           (make-undoable-edit
            "Rename Fact"
            (lambda () ;; undo
              (rename-fact-undo-redo cache-factID oldname))
            (lambda () ;; redo
              (rename-fact-undo-redo cache-factID newname))
            ))
          
          )))) ; make sure we don't lose the selection

; currently selected fact
(define selected-factID '())

; select the fact
(define (selectfact factID)
  ; remember selected fact
  (set! selected-factID factID)

  ; a fact is now selected, so enable fact buttons/menu items
  (enable-fact-functions #t))

; deselect fact
(define (deselectfact)
  ; forget selected fact
  (set! selected-factID '())

  ; no facts are selected, so disable fact buttons/menu items
  (enable-fact-functions #f))

; enable/disable fact-related buttons and menu items
(define (enable-fact-functions flag)
  (set-menuitem-component m-fact-renamefact flag)
  (set-menuitem-component m-fact-delfact flag))

; delete currently selected fact
(define (dodelfact)
  
  (define fact-to-del (get 'facts selected-factID))
  (define factID selected-factID) ;; important to cache this because selected-factID gets set
  (define fact-sexpr (ask fact-to-del 'to-save-sexpr))
  (define factname (ask fact-to-del 'name))
  
  ;; add the undoable edit
  (hd-postedit
   undo-manager
   (make-undoable-edit
    "Delete Fact"
    ;; redo make new fact is the undo of delete fact
    (lambda ()
      (newfact-redo fact-sexpr factID factname))
    ;; undo make new fact is the redo of delete fact
    (lambda ()
      (newfact-undo factID))
    ))
  (delete-fact selected-factID))

; delete an fact
(define (delete-fact factID)
  (let ((thefact (get 'facts factID)))
    (if thefact
        (begin
          ; if this is the selected fact, clear selected fact and disable fact buttons/menu items
          (if (eq? selected-factID factID)
              (begin
                (set! selected-factID '())
                (enable-fact-functions #f)))

          ; delete node from data-table
          (del 'facts factID)

          ; update main window label to show file is dirty
          (update-dirty-state)

          ; remove from UI - cheat by just repopulating the list
          (ask fact-list 'populate-facts-list)))))


;;
;;;; zooming
;; 

; zoom delta: how much it zooms in/out each time
(define-constant zoom-delta 1.5)
(define-constant default-zoom 1.0)
(define-constant max-zoom (/ 1.0 (* 10.0 zoom-delta)))
(define-constant min-zoom 1.0)

; zoom in
(define (dozoomin)
  (let* ((current-zoom (ask node-graph 'get-zoomfactor))
         (new-zoom (min min-zoom (* current-zoom zoom-delta))))
    (ask node-graph 'set-zoomfactor! new-zoom)
    (ask anywhere-graph 'set-zoomfactor! new-zoom)
    (enable-zoom)))
    
; zoom out
(define (dozoomout)
  (let* ((current-zoom (ask node-graph 'get-zoomfactor))
         (new-zoom (max max-zoom (/ current-zoom zoom-delta))))
    (ask node-graph 'set-zoomfactor! new-zoom)
    (ask anywhere-graph 'set-zoomfactor! new-zoom)
    (enable-zoom)))

; zoom reset
(define (dozoomreset)
  (ask node-graph 'set-zoomfactor! default-zoom)
  (ask anywhere-graph 'set-zoomfactor! default-zoom)
  (enable-zoom))

; update zoom menuitem state
(define (enable-zoom)
  (let ((current-zoom (ask node-graph 'get-zoomfactor)))
    (set-menuitem-component m-view-zoomin (< current-zoom min-zoom))
    (set-menuitem-component m-view-zoomout (> current-zoom max-zoom))
    (set-menuitem-component m-view-zoomreset (not (= current-zoom default-zoom)))))

;;;; operation

; toggle show IDs
(define (doshowIDs)
  (set-show-IDs! (not (show-IDs?)))
  
  ; refresh main display
  (populate-lists)
  (refresh-graphs)
  
  (rmgr-update)
  
  ; refresh node editor display
  (refresh-link-list))

; toggle overlapping nodes in graph view
(define (doallowoverlap)
  (let ((new-state (not (allow-overlap?))))
    ; remember in config options
    (set-allow-overlap! new-state)
    
    ; and set in graphs
    (ask node-graph 'set-allow-overlap! new-state)
    (ask node-graph 'layout-all)
    (ask anywhere-graph 'set-allow-overlap! new-state)
    (ask anywhere-graph 'layout-all)
    
    ; update menu item
    (set-checkbox-menu-item m-view-allow-overlap new-state)))
    
; toggle snap to grid in graph view
(define (dosnaptogrid)
  (let ((new-state (not (snap-to-grid?))))
    ; remember in config options
    (set-snap-to-grid! new-state)
    
    ; and set in graphs
    (ask node-graph 'set-snap-to-grid! new-state)
    (ask anywhere-graph 'set-snap-to-grid! new-state)
    
    ; enable layout menu item
    (set-component-enabled m-view-layout new-state)
    
    ; update menu item
    (set-checkbox-menu-item m-view-snap-to-grid new-state)))

; force layout
(define (dolayout)
  (ask node-graph 'layout-all)
  (ask anywhere-graph 'layout-all))

;;
;;;; fact list
;; 

(define (make-fact-listview selectfact-callback onmouse-callback)
  (let* ((parent-obj (make-object-listview selectfact-callback #t 'facts 'name string>=?))
         (this-obj (new-object parent-obj)))

    ; initialize
    (define (init)
      (ask parent-obj 'init)
      (if (procedure? onmouse-callback)
          (add-mouselistener (ask parent-obj 'get-list)
                             (make-mouselistener onmouse-callback))))

    ; add fact to the list
    (define (add-fact new-factID name)
      (let ((the-fact (get 'facts new-factID)))
        (if the-fact
            (let ((the-type (ask the-fact 'type)))
              (ask parent-obj 'add-object new-factID
                   (string-append name 
                                  (if (show-IDs?)
                                      (string-append " ("
                                                     (number->string new-factID)
                                                     ")")
                                      "")
                                  " ("
                                  (case the-type
                                    ((string) "text")
                                    ((boolean) "true/false")
                                    ((number) "number")
                                    (else "unknown type"))
                                  ")")))
            (ask parent-obj 'add-object new-factID name))))
    
    ; populate the fact list
    (define (populate-facts-list)
      (let ((the-facts (get-list 'facts)))
        (ask parent-obj 'clear)
        (if the-facts
            (map (lambda (a)
                   (let* ((thisfact (cdr a))
                          (thisfact-name (ask thisfact 'name))
                          (thisfact-ID (ask thisfact 'ID)))
                     (add-fact thisfact-ID thisfact-name)))
                 the-facts)))
      (if (not (eq? '() selected-factID))
          (ask parent-obj 'select-object selected-factID)))

    ; message handling
    (obj-put this-obj 'init
             (lambda (self) (init)))
    (obj-put this-obj 'populate-facts-list
             (lambda (self) (populate-facts-list)))
    (obj-put this-obj 'add-fact
             (lambda (self new-factID name)
               (add-fact new-factID name)))
    (obj-put this-obj 'select-fact
             (lambda (self in-factID)
               (ask parent-obj 'select-object in-factID)))
    (obj-put this-obj 'deselect-node
             (lambda (self)
               (ask parent-obj 'deselect-object)))
    this-obj))

; selected an fact in the fact list
(define (do-selectfact-list factID)
  (if (not (null? factID))
      (let* ((thefact (get 'facts factID)))
        (selectfact factID))
      (begin
        ; deselected a fact in the list
        (deselectfact))))

; mouse event in fact list - for double-clicking
(define (factlist-onmouse e)
  (let ((event-type (get-mouseevent-type e)))
    (if (eq? event-type 'left-clicked)
        (let ((event-click-count (get-mouseevent-click-count e)))
          (if (= 2 event-click-count)
              (dorenamefact))))))

;;
;;;; graph editor
;;

; callback from graph editor
(define (graph-callback action object)
  (cond ((eq? action 'select)
         (do-selectnode-graph (string->number (ask object 'get-data))))
        ((eq? action 'deselect)
         (do-deselectnode-graph (string->number (ask object 'get-data))))
        ((eq? action 'editnode)
         (doeditnode))))

; selected a node in the graph
(define (do-selectnode-graph nodeID)
  (let* ((thenode (get 'nodes nodeID))
         (anywhere (ask thenode 'anywhere?))
         (the-othergraph (if anywhere node-graph anywhere-graph)))
    (selectnode nodeID)
    (ask node-list 'select-node nodeID)

    ; make sure no nodes are selected in the other graph view
    (if (not (is-basic-mode?))
        (ask the-othergraph 'select-node -1))))

; deselected a node in the graph
(define (do-deselectnode-graph nodeID)
  (deselectnode)
  (ask node-list 'deselect-node))

; store the node positions for saving
(define (store-node-positions)
  (ask node-graph 'store-node-positions)
  (ask anywhere-graph 'store-node-positions))

; update the emphasis height for a node
(define use-emph #f) ; disable emphasis for NM3222
(define (update-node-emphasis thisnodeID)
  (let ((thisnode (get 'nodes thisnodeID)))
    (if thisnode
        (let ((anywhere (ask thisnode 'anywhere?)))
          (if anywhere
              (ask anywhere-graph 'update-node-emphasis thisnodeID)
              (ask node-graph 'update-node-emphasis thisnodeID))))))

; update the style for a node
(define (update-node-style thisnodeID)
  (let ((thisnode (get 'nodes thisnodeID)))
    (if thisnode
        (let ((anywhere (ask thisnode 'anywhere?)))
          (if anywhere
              (ask anywhere-graph 'update-node-style thisnodeID)
              (ask node-graph 'update-node-style thisnodeID))))))

; get maximum node positions, used for importing
(define (get-max-node-positions)
  (let-values (((max-x max-y) (ask node-graph 'get-max-node-positions))
               ((max-anywhere-x max-anywhere-y) (ask anywhere-graph 'get-max-node-positions)))
    (values max-x max-y max-anywhere-x max-anywhere-y)))

;; 
;;;;  file I/O
;;


;; clear the data
(define (clear-data)
  (reset-table)
  (reset-uniqueID)
  (reset-document-ruleID)
  (reset-start-nodeID)
  (reset-card-shark)
  (reset-back-button)
  (reset-restart-button)
  (reset-node-count)
  (reset-properties)
  (set-saved-filename! #f)
  (clear-loaded-file-version)
  (register-as-inspectable 'nodes)
  (register-as-inspectable 'links)
  (register-as-inspectable 'facts)
  (clear-dirty!)
  (undo-manager-discard-all-edits undo-manager)
  )

; clear the UI (lists and graph)
(define (clear-display)
  ; clear lists
  (clear-lists)
  
  ; clear graphs
  (clear-graphs)

  ; clear any selections
  (set! selected-nodeID '())
  (if (show-facts?)
      (set! selected-factID '()))

  ; reset button and menu states
  (enable-node-functions #f)
  (if (show-facts?)
      (enable-fact-functions #f))
  
  ; reset the undo/redo menu items
  (update-undo-action undo-action)
  (update-redo-action redo-action)

  ; finally, update frame label
  (update-dirty-state)
  )

; clear graphs
(define (clear-graphs)
  (ask node-graph 'clear)
  (if (not (is-basic-mode?))
      (ask anywhere-graph 'clear)))

; clear lists
(define (clear-lists)
    (ask node-list 'clear)
  (if (show-facts?)
      (ask fact-list 'clear)))

; refresh graphs: just regenerates the names
(define (refresh-graphs)
  (ask node-graph 'refresh-display)
  (if (not (is-basic-mode?))
      (ask anywhere-graph 'refresh-display)))

;;;; populate  

; populate lists
(define (populate-lists)
    (ask node-list 'populate-nodes-list)
  (if (show-facts?)
      (ask fact-list 'populate-facts-list)))

; populate graphs
(define (populate-graphs)
  (ask node-graph 'populate-graph)
  (if (not (is-basic-mode?))
      (ask anywhere-graph 'populate-graph)))
  
; populate the display
(define (populate-display)
  ; now that everything is loaded, repopulate nodes list (a hack to get start node to show)
  (populate-lists)
  
  ; and fill in nodes and lines in graph
  (populate-graphs)
  
  ; finally, update frame label 
  (update-dirty-state)
  )

; update whether or not the file is dirty ie. needs saving
(define (update-dirty-state)
  ; update main window label
  (update-label)
  
  ; and set save menu item enabled/disabled
  (enable-save (or (dirty?) (nodeeditor-dirty?))))

; append "*" to filename if its dirty, used only for main window label
(define (build-display-filename)
  (let ((the-filename (get-saved-filename-string)))
    
    (string-append
     (if the-filename the-filename "Untitled")
     (if (or
          (dirty?)
          (nodeeditor-dirty?))
         "*" "")
     )
    ))

;;
;; reader window - implementation is in reader.scm
;; allows reading of text nodes, and following of links
;;

;;text window frame

;;;; window callback ; future work
(define (nodereader-window-opened o)
  (format #t "nodereader-window-opened~%~!")
  ())
(define (nodereader-window-closing o)
  (format #t "nodereader-window-closing~%~!")
  (nodereader-close)
  )
(define (nodereader-window-closed o)
  (format #t "nodereader-window-closed~%~!")
  (remove-from-window-menu nrf)
  ())
(define (nodereader-window-iconified o)
  (format #t "nodereader-window-iconified~%~!")
  ())                                                 
(define (nodereader-window-deiconified o)
  (format #t "nodereader-window-deiconified~%~!")
  ())                                                    
(define (nodereader-window-activated o)
  (format #t "nodereader-window-activated~%~!")
  ())                                                   
(define (nodereader-window-deactivated o)
  (format #t "nodereader-window-deactivated~%~!")
  ())                                                    

;;;; nodereader operation
; shutdown procedure - called as closing
(define (nodereader-close)
  (remove-from-window-menu nrf)
  (close-nodereader)
  (set-runstate #f))

; start reading when start button in main-ui is pressed
(define file-suffix 0) ; save each temporary story file in a different subfolder in the temp folder
(define (nodereader-start)
  (nodeeditor-save) ;; save the edited node's content
  (if (java-reader?)
      (if (not (doreadstartnode (get-saved-filename-string)))
          (begin
            (make-confirm-dialogbox #!null 1 "Sorry, no start node defined.")
            (set-runstate #f))
          (add-to-window-menu nrf "Reader"))
      (begin
        (set! file-suffix (+ file-suffix 1))
        (let ((the-tmp-filename (string-append (get-temp-dir) "/temp" (number->string file-suffix))))
          (copy-js-framework-to-folder (make-file the-tmp-filename))
          (invoke (java.awt.Desktop:getDesktop) 'browse
                  (<java.net.URI> (string-append "http://localhost:" (number->string (get-local-port))
                                                 "/temp" (number->string file-suffix) "/index.html")))
          (set-runstate #f)))))

; stop reading when stop button in main-ui is pressed
(define (nodereader-stop)
  (nodereader-close))

; link in nodereader-editor
(define nrf #f)
(define (create-nodereader-window)
  (set! nrf (make-window "Reader"))
  (set-component-size nrf 400 300)
;  (set-container-layout nrf 'flow 'left)
  (add-windowlistener nrf (make-windowlistener
                           nodereader-window-opened
                           nodereader-window-closing
                           nodereader-window-closed
                           nodereader-window-iconified
                           nodereader-window-deiconified
                           nodereader-window-activated
                           nodereader-window-deactivated))
  (create-nodereader nrf #f #f #f)
  
  ; add window menu
  (let ((mb (make-menu-bar)))
    (add-menu-bar nrf mb)
    (add-component mb (add-window-menu nrf))))

;;
;;;; main ui hook 
;; 
;; hook into main-ui.scm
;;

; initialize the hteditor UI
(define (init-hteditor-ui)
  ; temporarily remove built-in menus, to preserve ordering of menus
  (remove-menu (get-control-menu))
  (remove-menu (get-language-menu))
  (remove-menu (get-examples-menu))
  (remove-menu (get-window-menu))
  (remove-menu (get-help-menu))
  
  ; restore menus at the end of the menubar
  (restore-menu (get-control-menu))
  (restore-menu (get-language-menu))
  (restore-menu (get-examples-menu))
  (restore-menu (get-window-menu))
  (restore-menu (get-help-menu))
 
  ; disable step button
  (enable-step #f)
  
  (if (not (java-reader?))
      (begin
        (hide-step)
        (hide-stop)))
  
  ; intialize data
  (clear-data)
  (clear-display)
  
  ; special init for sculptural mode
  (check-init-sculptural))

; close the hteditor UI
(define (close-hteditor-ui)
  ; close editor and reader window if necessary
  (close-hteditor-subwindows)

  ; re-enable step button
  (enable-step #t)
  
  ; remove our menus
  (remove-menu m-view-menu)
  (remove-menu m-edit-menu)
  (if (show-facts?)
      (remove-menu m-fact-menu))
  (remove-menu m-node-menu)
  
  ; remove our menu items
  (remove-component (get-file-menu) m-file-openrecent)
  (remove-component (get-file-menu) m-file-separator)
  (remove-component (get-file-menu) m-file-import)
  (remove-component (get-file-menu) m-file-export-web)
  (remove-component (get-file-menu) m-file-export-standalone)
  
  ; remove our buttons from toolbar
  (remove-component (get-main-ui-button-panel) toolbar-button-newnode)
  (remove-component (get-main-ui-button-panel) toolbar-button-newanywherenode)
  (remove-component (get-main-ui-button-panel) toolbar-button-editnode)
  (remove-component (get-main-ui-button-panel) toolbar-button-renamenode)
  (remove-component (get-main-ui-button-panel) toolbar-button-delnode)
  (remove-component (get-main-ui-button-panel) toolbar-button-readnode)
  (remove-component (get-main-ui-button-panel) toolbar-nodecount-label)
  (remove-component (get-main-ui-button-panel) toolbar-nodecount-textarea))

; cleanup on exit
(define (cleanup-on-exit)
  (if (not (java-reader?))
      (export-remove-folder (make-file (get-temp-dir)))))

; set callbacks
(define (use-hteditor-ui)
  (start-hteditor)
  (set-ht-build-sexpr-callback!)
  (register-ui hteditor-ui-panel
               nodereader-start
               #f
               nodereader-stop
               build-display-filename
               donew
               doopen
               dosave-wrapper
               confirm-save
               init-hteditor-ui
               close-hteditor-ui
               #f
               #f
               #f
               #f)
  
  (set-fileformat-version! 2.2)
  (set-fileformat-type! 'htfe)
  
  (set-get-prefs-callback! get-hteditor-prefs)
  (set-set-prefs-callback! set-hteditor-prefs!)
  (set-window-activated-callback! window-activated-callback)
  ;(set-window-iconified-callback! window-minimize-callback)
  ;(set-window-deiconified-callback! window-restore-callback)
  
  (set-about-callback! show-about-window)
  (set-appexit-callback! cleanup-on-exit))

; callback when window is activated
(define (window-activated-callback)
  (format #t "window-activated-callback~%~!")
  (update-undo-action undo-action)
  (update-redo-action redo-action))

(define (window-minimize-callback)
  (display "hypedyn minimized")(newline))

(define (window-restore-callback)
  (display "hypedyn restore")(newline))

; special init for sculptural mode
(define (check-init-sculptural)
  ; if we're in sculptural mode, create a single non-anywhere node, and make it the start node
  (if (sculptural?)
      (let ((new-nodeID (create-node "Start" "" (calc-new-node-x #t) initial-y #f #f)))
        (set-start-node! new-nodeID)
        (populate-display))))