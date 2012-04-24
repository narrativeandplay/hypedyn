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

(require "../common/graph-editor.scm")
(require "../common/object-graphview.scm")
(require "../common/objects.scm")
(require "../common/datatable.scm") ;; get 
(require "../kawa/graphics-kawa.scm")
(require "../kawa/color.scm")
(require "config-options.scm")
(require "datastructure.scm")
(require "hypedyn-graphed-mouse.scm")

;; temp debug
(require "editlink.scm") ;; add-link-display

(module-export make-node-graphview
               generate-link-name)

; generate the link name
(define (generate-link-name in-name in-linkID)
  (string-append
   ;(if in-alt? "~" "")
   in-name
   (if (show-IDs?) (string-append "(" (number->string in-linkID) ")") "")))

; make a node graph view
; parameters:
; w: width of graph editor
; h: height of graph editor
; graph-callback: procedure to call when action occurs in graph, takes action and object
; is-anywhere: boolean, #t means its the anywhere graph view, # is the main graph view
; use-emph: should nodes use emphasis?
(define (make-node-graphview w h max-w max-h graph-callback is-anywhere use-emph)
  (let* ((parent-obj (make-object-graphview w h graph-callback 
                                           ; want to pass in the max-w and max-h to hypedyn's version of mouse handler
                                           (lambda (c)
                                             (set-hypedyn-graph-editor-mouse-event c max-w max-h))
                                           'nodes))
         (this-obj (new-object parent-obj))
         (the-graph-editor #f)
         (emph-scale 10)) ; this is the ratio of text length to pixels, should be dynamic?
    
    ; init
    (define (init)
      (ask parent-obj 'init)
      ;(ask parent-obj 'set-line-draw-proc! #f)
      (ask parent-obj 'set-make-displayname-callback! make-displayname)
      (set! the-graph-editor (ask parent-obj 'get-graph-editor))
      (ask parent-obj 'set-allow-overlap! (allow-overlap?))
      (ask parent-obj 'set-snap-to-grid! (snap-to-grid?))
      (ask the-graph-editor 'set-text-height! 12)
      (ask the-graph-editor 'set-bgdraw bgdraw))
    
    ; draw background
    (define (bgdraw)
      (let ((buffer (ask the-graph-editor 'get-buffer)))
        (drawline buffer 0 max-h max-w max-h white-color 'solid)
        (drawline buffer max-w 0 max-w max-h white-color 'solid)))
    
    ; populate nodes and links in graph
    (define (populate-graph)
      (let ((the-nodes (get-list 'nodes))
            (the-links (get-list 'links)))
        ; stop repaint (for performance)
        (ask parent-obj 'set-allow-repaint! #f)
        
        ; go through all nodes and create
        (if the-nodes
            (map (lambda (n)
                   (let* ((thisnode (cdr n))
                          (thisnode-name (ask thisnode 'name))
                          (thisnode-ID (ask thisnode 'ID))
                          (thisnode-anywhere (ask thisnode 'anywhere?)))
                     ;(display "drawing nodes ")(display nodeID)(newline)
                     (if (eq? thisnode-anywhere is-anywhere)
                         (add-node thisnode-ID (make-displayname thisnode)
                                   (ask thisnode 'get-x) (ask thisnode 'get-y)))))
                 the-nodes))

        ; go through all links and create the lines
        (if (not is-anywhere)
            (if the-links
                (map (lambda (l)
                       (let ((linkID (car l)))
                         (display "drawing links ")(display linkID)(newline)
                         (add-link-display linkID)))
                     the-links)))
            
        
        ; allow repaint
        (ask parent-obj 'set-allow-repaint! #t)
        (ask parent-obj 'refresh)))
    
    
    ; make display name from number and ID
    (define (make-displayname in-node)
      (if in-node
          (let ((thisnode-name (ask in-node 'name))
                (thisnode-ID (ask in-node 'ID)))
            (if (show-IDs?)
                (string-append thisnode-name
                               " ("
                               (number->string thisnode-ID)
                               ")")
                thisnode-name))
          ""))

    ;; dont draw anything for tab
    (define (custom-tab-draw dc type show?)
      #t)
    
    ; update node display in graph view
    (define (add-node new-nodeID name x y)
      (let* ((the-style (if (has-alt-text? new-nodeID)
                            '(alt hidden-tabs)
                            '(hidden-tabs)))
             (the-node (get 'nodes new-nodeID))
             (editor (ask parent-obj 'get-graph-editor)))

        ; add the node
        (ask editor 'node-add
             (number->string new-nodeID)
             (make-displayname the-node)
             x y the-style)

        (let ((new-node (ask editor 'node-get-by-data (number->string new-nodeID))))
          ; why do I set tabs?
          (let-values (((width height) (ask new-node 'get-size)))
            (set! tab-in (ask new-node 'custom-tab-incr x y 'in custom-tab-draw))
            (set! tab-out (ask new-node 'custom-tab-incr x y 'out custom-tab-draw)))

          ; set custom drawing
          (ask new-node 'set-custom-node-draw
               (lambda (dc x y bg-color selected? data)
                 (draw-node new-node dc x y bg-color selected? data))))

        ; set node emphasis
        (update-node-emphasis new-nodeID)))

        ; custom drawing of nodes
    (define (draw-node this-node dc x y bg-color selected? data)
      (let ((color #f))
        (let-values (((width height) (ask this-node 'get-size)))
          ; draw or undraw selected square
          (if selected?
              (set! color red-color)
              (set! color bg-color))
          (rectangle-fill dc
                          (- x (/ width 2.0) 4)
                          (- y (/ height 2.0) 4)
                          (+ x (/ width 2.0) 5)
                          (+ y (/ height 2.0) 5)
                          color 'solid)

          ; draw node square
          (drawnodesquare dc #t x y width height))))

    ; draw node square helper 
    (define (drawnodesquare dc show? bx by width height)
      ; draw the node square
      ; draw boundary border
      (if show?
          (rectangle-fill dc
                          (- bx (/ width 2.0))
                          (- by (/ height 2.0))
                          (+ bx (/ width 2.0))
                          (+ by (/ height 2.0))
                          default-node-color 'solid) ;dark-grey-color 'solid)
          (rectangle-fill dc
                          (- bx (/ width 2.0))
                          (- by (/ height 2.0))
                          (+ bx (/ width 2.0))
                          (+ by (/ height 2.0))
                          white-color 'solid))

      ; draw a white line on top and left
      (drawline dc
                (- bx (/ width 2.0))
                (- by (/ height 2.0))
                (- bx (/ width 2.0))
                (+ by (/ height 2.0))
                white-color 'solid)
      (drawline dc
                (- bx (/ width 2.0))
                (- by (/ height 2.0))
                (+ bx (/ width 2.0))
                (- by (/ height 2.0))
                white-color 'solid)

      ; draw a black line on bottom and right
      (drawline dc
                (- bx (/ width 2.0))
                (- (+ by (/ height 2.0)) 1.0)
                (+ bx (/ width 2.0))
                (- (+ by (/ height 2.0)) 1.0)
                black-color 'solid)
      (drawline dc
                (+ bx (/ width 2.0))
                (- by (/ height 2.0))
                (+ bx (/ width 2.0))
                (- (+ by (/ height 2.0)) 1.0)
                black-color 'solid)
      )

    ; helper to check if node has any alt text
    ;; TODO: remove outdated
    (define (has-alt-text? thisnodeID)
      (let ((thisnode (get 'nodes thisnodeID))
            (has-alt #f))
        (if thisnode
            (begin
              (map (lambda (l)
                     (let ((thislink (get 'links l)))
                       (set! has-alt
                             (or has-alt (ask thislink 'use-alt-text)))))
                   (ask thisnode 'links))
              has-alt)
            #f)))

    ; update node style
    ;; TODO: to get back to this, unsure what this is doing
    ;;       however this is definitely outdated since has-alt-text? isnt working
    (define (update-node-style nodeID)
      (let ((graph-node (ask the-graph-editor 'node-get-by-data (number->string nodeID))))
        (if graph-node
            (ask graph-node 'set-style (if (has-alt-text? nodeID) '(alt hidden-tabs) '(hidden-tabs))))))

    ; update the emphasis height for a node
    (define (update-node-emphasis thisnodeID)
      (if use-emph
          (let ((thisnode (get 'nodes thisnodeID)))
            (if thisnode
                (let* ((content (ask thisnode 'content))
                       (emph-height (string-length content))
                       (graph-node (ask (ask parent-obj 'get-graph-editor) 'node-get-by-data (number->string thisnodeID))))
                  (ask graph-node 'set-emph-height (/ emph-height emph-scale)))))))

    ; get maximum node positions, used for importing
    (define (get-max-node-positions)
      (let ((the-nodes (get-list 'nodes))
            (max-x 0)
            (max-y 0))
        (if the-nodes
            (map (lambda (n)
                   (let* ((thisnodeID (car n))
                          (thisnode (cdr n))
                          (anywhere (ask thisnode 'anywhere?)))
                     (if (eq? anywhere is-anywhere)
                         (let ((graph-node (ask (ask parent-obj 'get-graph-editor) 'node-get-by-data (number->string thisnodeID))))
                           (set! max-x (max max-x (ask graph-node 'get-x)))
                           (set! max-y (max max-y (ask graph-node 'get-y)))))))
                 the-nodes))
        (values (+ max-x node-width)
                (+ max-y node-height))))
    
    ; refresh node and link names, used after toggling showIDs
    (define (refresh-display)
      (let ((the-nodes (get-list 'nodes))
            (the-links (get-list 'links)))
        ; stop repaint
        ;(ask parent-obj 'set-allow-repaint! #f)
        
        ; go through all nodes and refresh name
        (if the-nodes
            (map (lambda (n)
                   (let* ((thisnode (cdr n))
                          (thisnode-ID (ask thisnode 'ID))
                          (thisnode-anywhere (ask thisnode 'anywhere?)))
                     (if (eq? thisnode-anywhere is-anywhere)
                         ; force the node to refresh its name
                         (ask parent-obj 'rename-node thisnode-ID))))
                 the-nodes))

        ; go through all links and refresh name
        (if (not is-anywhere)
            (if the-links
                (map (lambda (l)
                       (let* ((this-link (cdr l))
                              ;(use-destination (ask this-link 'use-destination))
                              ;(use-alt-destination (ask this-link 'use-alt-destination))
                              (this-link-ID(ask this-link 'ID))
                              (this-link-name (ask this-link 'name)))
;                         ; if using destination, then refresh to link line's label
;                         (if use-destination
;                             (ask parent-obj 'rename-line 
;                                  (number->string this-link-ID)
;                                  (generate-link-name this-link-name this-link-ID #f)))
;                         ; if using alt-destination, then refresh alt link line's label
;                         (if use-alt-destination
;                             (ask parent-obj 'rename-line 
;                                  (string-append "~" (number->string this-link-ID))
;                                  (generate-link-name this-link-name this-link-ID #t))))
                         
                          ;debug does rename line do the right thing?
                         (ask parent-obj 'rename-line
                              (number->string this-link-ID)
                              ;(generate-link-name this-link-name this-link-ID)
                              "DOH"
                              )
                         
                       ))
                     the-links)))
        
        ; allow repaint
        (ask parent-obj 'set-allow-repaint! #t)
        (ask parent-obj 'refresh)))
    
    ; message handling                  
    (obj-put this-obj 'init
             (lambda (self) (init)))
    (obj-put this-obj 'populate-graph
             (lambda (self) (populate-graph)))
    (obj-put this-obj 'add-node
             (lambda (self new-nodeID name x y)
               (add-node new-nodeID name x y)))
    (obj-put this-obj 'update-node-style
             (lambda (self nodeID)
               (update-node-style nodeID)))
    (obj-put this-obj 'update-node-emphasis
             (lambda (self nodeID)
               (update-node-emphasis nodeID)))
             
    (obj-put this-obj 'get-max-node-positions
             (lambda (self)
               (get-max-node-positions)))
    (obj-put this-obj 'refresh-display
             (lambda (self) (refresh-display)))
    
;    (define (del-line line-ID fromnodeID tonodeID)
;      (let* ((c-fromnode (ask c 'node-get-by-data (number->string fromnodeID)))
;             (c-tonode (ask c 'node-get-by-data (number->string tonodeID)))
;             (c-fromtab (ask c-fromnode 'tab-out-ref 0))
;             (c-totab (ask c-tonode 'tab-in-ref 0))
;             ;(link (ask c 'get-line-by-ID line-ID))
;             )
;        (define link (ask c 'get-line-by-ID line-ID))
;        (ask c 'line-del link c-fromtab c-totab line-ID)))
    
    ;; remove all lines coming out of this node (give this a better name next time)
    (obj-put this-obj 'clear-node-outlinks
             (lambda (self nodeID)
               (define graph-ed (ask parent-obj 'get-graph-editor))
               (define fromnode (ask graph-ed 'node-get-by-data (number->string nodeID)))
               (define fromtab (ask fromnode 'tab-out-ref 0))
               (define line-lst (ask fromtab 'get-lines))
               (map (lambda (line)
                      (define src-tab (ask line 'get-source))
                      (define target-tab (ask line 'get-target))
                      (define line-name (ask line 'get-ID))
                      (ask graph-ed 'line-del line src-tab target-tab line-name)
                      ) line-lst)
               ))
    this-obj))
