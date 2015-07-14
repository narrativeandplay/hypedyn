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

(require "../common/graph-editor.scm")
(require "../common/object-graphview.scm")
(require "../common/objects.scm")
(require "../common/datatable.scm") ;; get 
(require "../common/list-helpers.scm") ;; flatten, take, map-with-index
(require "../kawa/strings.scm") ;; string-lastindexof
(require "../kawa/graphics-kawa.scm")
(require "../kawa/color.scm")
(require "config-options.scm") ;; node-name-limit
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
                          (thisnode-ID (ask thisnode 'ID)))
                     (add-node thisnode-ID (make-displayname thisnode)
                               (ask thisnode 'get-x) (ask thisnode 'get-y))
                     ))
                 the-nodes))

        ; go through all links and create the lines
        (if (not is-anywhere)
            (if the-links
                (map (lambda (l)
                       (let ((linkID (car l)))
                         (add-link-display linkID)))
                     the-links)))
            
        
        ; allow repaint
        (ask parent-obj 'set-allow-repaint! #t)
        
        ; handle resize, forces resize of the canvas which updates scrollbars
        (ask parent-obj 'handle-resize)
        ))
    
    
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

    ; because kawa doesn't have a string-split
    ; from http://schemecookbook.org/Cookbook/StringSplit
    (define (str-split str ch)
      (let ((len (string-length str)))
        (letrec
            ((split
              (lambda (a b)
                (cond
                 ((>= b len) (if (= a b) '("") (cons (substring str a b) '())))
                 ((char=? ch (string-ref str b)) (if (= a b)
                                                     (cons "" (split (+ 1 a) (+ 1 b)))
                                                     (cons (substring str a b) (split (+ 1 b) (+ 1 b)))))
                 (else (split a (+ 1 b)))))))
          (split 0 0))))

    ; update node display in graph view
    (define (add-node new-nodeID name x y)
      (let* ((the-style (if (has-alt-text? new-nodeID)
                            '(alt hidden-tabs)
                            '(hidden-tabs)))
             (the-node (get 'nodes new-nodeID))
             (editor (ask parent-obj 'get-graph-editor)))

        ;; our custom create-node
        (create-graph-node (number->string new-nodeID) (make-displayname the-node) x y the-style)

        (let ((new-node (ask editor 'node-get-by-data (number->string new-nodeID))))
          ; why do I set tabs?
          (let-values (((width height) (ask new-node 'get-size)))
            (set! tab-in (ask new-node 'custom-tab-incr x y 'in custom-tab-draw))
            (set! tab-out (ask new-node 'custom-tab-incr x y 'out custom-tab-draw)))
          
          ; set custom drawing
          (ask new-node 'set-custom-node-draw
               (lambda (dc x y bg-color selected? data)
                 (draw-node new-node dc x y bg-color selected? data (ask the-node 'anywhere?))))
        
          ;; override the text drawing behavior of graph-editor
          (ask new-node 'set-custom-node-text-draw
               (lambda (dc x y tw th td ta text-color bg-color name text-height)
                 (draw-node-text new-node the-node dc x y tw th td ta text-color bg-color name text-height)))
          
          ;; draw the new node selected
          (ask new-node 'show #t)
          
          ) ;; end of let
        
        
        ; set node emphasis
        (update-node-emphasis new-nodeID)))
    
    ;; custom drawing of nodes
    (define (draw-node this-node dc x y bg-color selected? data is-anywhere)
      (let ((color #f))
        (let-values (((width height) (ask this-node 'get-size)))

          ;; draw or undraw selected square
          (if selected?
              (set! color red-color)
              (set! color bg-color))
          (rectangle-fill dc
                          (- x (/ width 2.0) 4)
                          (- y (/ height 2.0) 4)
                          (+ x (/ width 2.0) 5)
                          (+ y (/ height 2.0) 5)
                          color 'solid)

          ;; draw node square
          (drawnodesquare dc #t x y width height
                          (if is-anywhere dark-grey-color grey-color))
          ))
      )

    ; draw node square helper 
    (define (drawnodesquare dc show? bx by width height in-colour)
      ; draw the node square
      ; draw boundary border
      (if show?
          (begin
          (rectangle-fill dc
                          (- bx (/ width 2.0))
                          (- by (/ height 2.0))
                          (+ bx (/ width 2.0))
                          (- by (/ height 3.5))
                          in-colour 'solid)
          (rectangle-fill dc
                          (- bx (/ width 2.0))
                          (- by (/ height 3.5))
                          (+ bx (/ width 2.0))
                          (+ by (/ height 2.0))
                          white-smoke-color 'solid)
            )
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

      ; Draw line under node title
      (drawline dc
                (- bx (/ width 2.0))
                (- by (/ height 3.5))
                (+ bx (/ width 2.0))
                (- by (/ height 3.5))
                black-color 'solid)
      )
    
    ; custom drawing of node text
    (define (draw-node-text new-node the-node dc x y tw th td ta text-color bg-color name text-height)
      (set! name
            (if (> (string-length name) node-name-limit)
                (string-append (substring name 0 node-name-limit) "...")
                name))
      (let-values
          (((tw1 th1 td1 ta1) (get-text-extent dc name text-height)))
        (drawtext dc (- x (* tw1 0.5)) (+ (- y 55.0) (* th1 0.5)) text-color bg-color name))
      (let-values
          (((tw1 th1 td1 ta1) (get-text-extent dc name text-height)))
        (let-values (((width height) (ask new-node 'get-size)))
          (define line-length-limit 20)
          (define line-count-limit 8)
          (define (word-wrap string n)
            (cond ((<= n 0) '(""))
                  ((< (string-length string) n) (list string))
                  (else
                   (let* ((raw-line (substring string 0 n))
                          (end-index (string-lastindexof raw-line " "))
                          (line-end (if (= -1 end-index)
                                        n
                                        end-index))
                          (next-start (if (= -1 end-index)
                                          n
                                          (+ 1 end-index))))
                     (cons (substring string 0 line-end)
                           (word-wrap (substring string next-start (string-length string)) n))))))
          (map-with-index (lambda (line i)
                            (drawtext dc
                                      (- x (* (/ width 2.0) 0.9))
                                      (+ (- y 30.0) (* th1 0.5) (* i ta1))
                                      dark-grey-color
                                      bg-color
                                      line))
                          (safe-take (flatten (map (lambda (line)
                                                     (word-wrap line line-length-limit))
                                                   (str-split (ask the-node 'content) #\newline)))
                                     line-count-limit)))
        )
      )

     ;; TODO: subclass of node should be able to override 
    ;; 1) truncating name behavior without putting in callbacks
    ;; 2) drawing function without putting in callbacks
    ;;  the main obstacle now is that within the old node, it is still calling the draw inside the old node.
    ;;  in order for subclass to work, all the calls to internal functions should use (ask node 'message) instead of calling directly
    (define (create-graph-node ID name x y style)
      
      (define (create-node-wrapper in-node)
        (let ((this-obj (new-object in-node))
              (width 0.0)
              (height 0.0))

          (define (local-draw self show? selected?)
            
                               ;(draw show? selected?)
                     ;(display "graph editor ")(display the-graph-editor)(newline)
                     ;#f
                     ;(let ((dc (ask (ask parent-obj 'get-graph-editor) 'get-buffer)))
                     ;  (display "DC ")(display dc)(newline)
                       (draw-node self
                                   (ask (ask parent-obj 'get-graph-editor) 'get-buffer)
                     ;             #f #f #f #f #f #f);dc
                                  (ask self 'get-x)
                                  (ask self 'get-y)
                                  bg-color
                                  (ask self 'get-selected?)
                                  (ask self 'get-data))
            
            ;(draw-node self #f #f #f #f #f #f)
            ;(local-draw self show? selected?)
            (display "local draw ")(newline)
            )
          
          ;; use underlying draw for the moment
          ;;(obj-put this-obj 'draw local-draw)
          
          ;; override get-size
          (obj-put this-obj 'get-size
                   (lambda (self)
                     ;; get the default width and height and override it
                     (let-values (((old-width old-height) (ask in-node 'get-size)))
                         (let ((adjusted-width (+ old-width 70))
                               (adjusted-height (+ old-height 100)))
                             (values (if (> (string-length (ask in-node 'name)) node-name-limit)
                                         150
                                         adjusted-width)
                                     adjusted-height)))))
                     )) ;; end of wrapper
;      
;      (define create-node-wrapper 
;        (lambda (a) 
;          (ask parent-obj 'get-graph-editor)
;          a)
;        )
      
      ;; TODO DEBUG THIS there is no way of passing a create-node-wrapper that has a reference to anything outside this function scope
      ;; this the one that throws exception
      (ask (ask parent-obj 'get-graph-editor) 'custom-node-add
           ID name x y style create-node-wrapper)
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
                          (thisnode (cdr n)))
                         (let ((graph-node (ask (ask parent-obj 'get-graph-editor) 'node-get-by-data (number->string thisnodeID))))
                           (set! max-x (max max-x (ask graph-node 'get-x)))
                           (set! max-y (max max-y (ask graph-node 'get-y))))))
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
                          (thisnode-ID (ask thisnode 'ID)))
                     (ask parent-obj 'rename-node thisnode-ID)))
                 the-nodes))

        ; go through all links and refresh name (mainly for updating show ID)
        (if (not is-anywhere)
            (if the-links
                (map (lambda (l)
                       (let* ((this-link (cdr l))
                              (this-link-ID (ask this-link 'ID)))
                         ;; go through all the rules on this link
                         ;; if it exists, there would be a line sharing the same ID with that rule
                         (map (lambda (ruleID)
                                (ask parent-obj 'rename-line
                                     (number->string ruleID)
                                     (generate-link-name (ask (get 'rules ruleID) 'name) ruleID))
                                ) (ask this-link 'rule-lst))
                       ))
                     the-links)))
        
        ; allow repaint
        (ask parent-obj 'set-allow-repaint! #t)
        (ask parent-obj 'refresh)))
    
    (define (create-line name fromnodeID tonodeID ID #!optional style)
      (ask parent-obj 'create-line
           (generate-link-name name ID)
           fromnodeID
           tonodeID
           (number->string ID)
           style))
    
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
    (obj-put this-obj 'create-line
             (lambda (self name fromnodeID tonodeID ID #!optional style) 
               (create-line name fromnodeID tonodeID ID style)))
    
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
