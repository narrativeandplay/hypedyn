;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2011 National University of Singapore and
;; Singapore-MIT GAMBIT Game Lab c/o Media Development Authority of Singapore
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

;; graph view for objects: generic graph-editor view of a collection of objects connected by line

(require "graph-editor.scm")
(require "objects.scm")
(require "datatable.scm") ;;get
(require "graphics.scm")
(require "math.scm")
(require "../kawa/graphics-kawa.scm")
(require "../kawa/strings.scm") ;; to-string
(require "../kawa/color.scm")

(module-export make-object-graphview)
(module-static 'init-run)

; make an object graph view
; parameters:
; w: width of graph editor
; h: height of graph editor
; graph-callback: procedure to call when action occurs on graph editor, takes action and object
; set-mouse-event-callback: procedure that will set the mouse event callback, takes graph editor
; tableID: the ID of the table in the data table holding the nodes
(define (make-object-graphview w h graph-callback set-mouse-event-callback tableID)
  (let* ((named-obj (new-make-named-object "object-graphview"))
         (this-obj (new-object named-obj))
         (c #f)
         (make-displayname #f)
         (line-draw-proc #f)
         (text-height 12.0))
    
    
    ; initialize
    (define (init)
      (set! line-draw-proc line-draw)
      (set! c (create-graph-editor w h graph-callback 'generic)) ; last param no longer used?
      (if (procedure? set-mouse-event-callback)
          (set-mouse-event-callback c))
      )
    
    ; populate graph - should be implemented by subclass
    (define (populate-graph)
      'ok)

    ;; custom draw for (dont draw anything for tab) - should pass this in? alex
    (define (custom-tab-draw dc type show?)
      #t)
    
    ; update node display in graph view
    (define (add-node new-nodeID name x y)
      (let ((new-node (ask c 'node-add (number->string new-nodeID) name x y '(hidden-tabs))))
        (if new-node
            (let-values (((width height) (ask new-node 'get-size)))
              (set! tab-in (ask new-node 'custom-tab-incr x y 'in custom-tab-draw))
              (set! tab-out (ask new-node 'custom-tab-incr x y 'out custom-tab-draw))
              ))
        ))

    ; update a link in the graph; used as a callback from doeditlink
    (define (update-link-display name
                                 fromnodeID
                                 tonodeID
                                 new-linkID)
      (create-line name (number->string new-linkID) fromnodeID tonodeID))

    ;; custom line drawing
    (define (line-draw dc source target selected? show? data)
      (let ((src-node (ask source 'get-node))
            (dst-node (ask target 'get-node)))
        (let-values
            (((sx sy) (ask src-node 'get-position))
             ((tx ty) (ask dst-node 'get-position))
             ((sw sh) (ask src-node 'get-size))
             ((tw th) (ask dst-node 'get-size))
             )

          ; offset based on relative positions of nodes
          (let ((src-halfwidth (/ sw 2.0))
                (src-halfheight (/ sh 2.0))
                (dst-halfwidth (/ tw 2.0))
                (dst-halfheight (/ th 2.0))
                (dx (abs (- sx tx)))
                (dy (abs (- sy ty))))
            (if (> dx dy)
                (begin
                  ; x offset
                  (if (< (+ sx src-halfwidth) (- tx dst-halfwidth))
                      (begin
                        (set! sx (+ sx src-halfwidth))
                        (set! tx (- tx dst-halfwidth))))
                  (if (> (- sx src-halfwidth) (+ tx dst-halfwidth))
                      (begin
                        (set! sx (- sx src-halfwidth))
                        (set! tx (+ tx dst-halfwidth)))))
                (begin
                  ; y offset
                  (if (< (+ sy src-halfheight) (- ty dst-halfheight))
                      (begin
                        (set! sy (+ sy src-halfheight))
                        (set! ty (- ty dst-halfheight))))
                  (if (> (- sy src-halfheight) (+ ty dst-halfheight))
                      (begin
                        (set! sy (- sy src-halfheight))
                        (set! ty (+ ty dst-halfheight)))))))

          ; draw the line
          (if show?
              (drawline dc sx sy tx ty black-color 'solid)
              (drawline dc sx sy tx ty white-color 'solid))

          ; arrowhead
          (let* ((width 4.0)
                 (height (* width 2.5))
                 (angle (angle-from-x-axis sx sy tx ty))
                 (basex (round (- tx (* height (Cosine angle)))))
                 (basey (round (- ty (* height (Sine angle)))))
                 (x1 (round (+ basex (* width (Cosine (- angle 90.0))))))
                 (y1 (round (+ basey (* width (Sine (- angle 90.0))))))
                 (x2 tx)
                 (y2 ty)
                 (x3 (round (+ basex (* width (Cosine (+ angle 90.0))))))
                 (y3 (round (+ basey (* width (Sine (+ angle 90.0)))))))
            (drawfilledtriangle dc x1 y1 x2 y2 x3 y3 black-color))))

      ;;drawing the data--name of line
      (let-values
          (((sx sy) (ask source 'get-position))
           ((tx ty) (ask target 'get-position))
           ((tw th td ta) (get-text-extent dc data text-height)))

        ; should get panel background somehow? - alex
        (rectangle-fill dc
                        (- (/ (abs (+ tx sx)) 2.0) (/ tw 2.0))
                        (- (/ (abs (+ ty sy)) 2.0) (/ th 2.0) )
                        (+ (/ (abs (+ tx sx)) 2.0) (/ tw 2.0))
                        (+ (/ (abs (+ ty sy)) 2.0) (/ th 2.0) )
                        bg-color 'solid)
        (drawtext dc
                  (- (/ (abs (+ tx sx)) 2) (* tw 0.5)) (+ (/ (abs (+ ty sy)) 2) (* th 0.5))
                  red-color bg-color data)))

    ; create the line in graph
    ; name and ID should be strings, ID is generally the linkID (plus "~" for "else")
    (define (create-line name ID fromnodeID tonodeID)
      ;; only create line if tonodeID not -1
      (if (not (= tonodeID -1))
          (begin
            (define c-fromnode (ask c 'node-get-by-data (number->string fromnodeID)))
            (define c-tonode (ask c 'node-get-by-data (number->string tonodeID)))
            (define c-fromtab (ask c-fromnode 'tab-out-ref 0))
            (define c-totab (ask c-tonode 'tab-in-ref 0))

            (ask c 'line-add-with-name-and-ID name (to-string ID) c-fromtab c-totab line-draw-proc)
            )))

    ; delete the line in graph
    ; line-ID should be a string corresponding to the linkID for "if" case, and
    ; "~" + linkID for "else" case
    (define (del-line line-ID fromnodeID tonodeID)
      (let* ((c-fromnode (ask c 'node-get-by-data (number->string fromnodeID)))
             (c-tonode (ask c 'node-get-by-data (number->string tonodeID)))
             (c-fromtab (ask c-fromnode 'tab-out-ref 0))
             (c-totab (ask c-tonode 'tab-in-ref 0))
             ;(link (ask c 'get-line-by-ID line-ID))
             )
        
        (define link (ask c 'get-line-by-ID line-ID))
        
        
;;        (display "in del-line ")(display c)(newline)
;;        (display "link ")(display link)(newline)
;;        (display "c-fromtab ")(display c-fromtab)(newline)
;;        (display "c-totab ")(display c-totab)(newline)
;;        (display "line-ID ")(display line-ID)(newline)
;;        
;;        (define data (car (cadr (ask c 'get-links))))
;;        (display "data class type ")(display (invoke line-ID 'get-class))(newline)
;;        
;;        (display "links ")(display (ask c 'get-links))(newline)
;;        (display "line-ID class type ")(display (invoke line-ID 'get-class))(newline)
        
;        (display "c-fromnode ")(display c-fromnode)(newline)
;        (display "c-tonode ")(display c-tonode)(newline)
;        (display "c-fromtab ")(display c-fromtab)(newline)
;        (display "c-totab ")(display c-totab)(newline)
        (newline)
        (display "line-ID ")(display line-ID)(newline)
        (display "line ID type ")(display (invoke line-ID 'get-class))(newline)
        
        (display "fromnodeID ")(display fromnodeID)(newline)
        (display "tonodeID ")(display tonodeID)(newline)
                                                    
        (display "[line passed to line-del] ")(display link)(newline)
        (ask c 'line-del link c-fromtab c-totab line-ID)))

    ; rename a line in graph
    ; line-ID should be a string corresponding to the linkID for "if" case, and
    ; "~" + linkID for "else" case
    (define (rename-line line-ID newname)
      (let ((link (ask c 'get-line-by-ID line-ID)))
        (if link (ask c 'line-rename link newname))))
    
    ; rename node in graph
    (define (rename-node in-nodeID)
      (let* ((graph-node
              (ask c 'node-get-by-data (number->string in-nodeID)))
             (the-node (get tableID in-nodeID))
             (the-name (if (procedure? make-displayname)
                           (make-displayname the-node)
                           (ask the-node 'name))))
        (ask graph-node 'set-name the-name)))

    ; delete node in graph
    (define (del-node in-nodeID)
;      (display "in del-node ")(newline)
;      (display "c ")(display c)(newline)
;      (display "node ")(display (ask c 'node-get-by-data (number->string in-nodeID)))(newline)
      (ask c 'node-del!
           (ask c 'node-get-by-data (number->string in-nodeID))))

    ; select node in graph
    (define (select-node in-nodeID)
      (if (eq? '() in-nodeID)
          ; passed in '() so deselect
          (ask c 'set-selected! (number->string -1))
          (begin
            (ask c 'set-selected! (number->string in-nodeID))
            (ask c 'scroll-to-node (number->string in-nodeID)))))

    ; set node colour
    (define (set-node-colour in-nodeID in-colour)
      (let ((this-node (ask c 'node-get-by-data (number->string in-nodeID))))
        (if this-node
            (ask this-node 'set-node-color in-colour))))
    
    ; store the node positions for saving
    (define (store-node-positions)
      (let ((the-nodes (get-list tableID)))
        (if the-nodes
            (map (lambda (n)
                   (let ((thisnodeID (car n))
                         (thisnode (cdr n)))
                     (store-node-position thisnodeID thisnode)))
                 the-nodes))))

    ; store the position for a given node
    (define (store-node-position in-nodeID in-node)
      (let ((graph-node (ask c 'node-get-by-data (number->string in-nodeID))))
        (if graph-node
            (begin
              (ask in-node 'set-x! (ask graph-node 'get-x))
              (ask in-node 'set-y! (ask graph-node 'get-y))))))

    
    ; zoom
    (define (get-zoomfactor)
      (if c
          (ask c 'get-zoomfactor)))
    (define (set-zoomfactor! in-zoom)
      (if c
          (begin
            (ask c 'set-zoomfactor! in-zoom)
            (ask c 'on-size))))

    ; message handling                  
;    (lambda (message)
;      (cond ((eq? message 'init)
;             (lambda (self)
;               (init)))
    (obj-put this-obj 'init
             (lambda (self) (init)))
;            ((eq? message 'get-component)
;             (lambda (self)
;               (if c
;                   (ask c 'get-canvas)
;                   #f)))
    (obj-put this-obj 'get-component
             (lambda (self)
               (if c
                   (ask c 'get-canvas)
                   #f)))
;            ((eq? message 'get-graph-editor)
;             (lambda (self)
;               c))
    (obj-put this-obj 'get-graph-editor 
             (lambda (self) c))
;            ((eq? message 'update-link-display)
;             (lambda (self name
;                           fromnodeID
;                           tonodeID
;                           new-linkID)
;               (update-link-display name
;                                    fromnodeID
;                                    tonodeID
;                                    new-linkID)))
    (obj-put this-obj 'update-link-display
             (lambda (self name
                           fromnodeID
                           tonodeID
                           new-linkID)
               (update-link-display name
                                    fromnodeID
                                    tonodeID
                                    new-linkID)))
;            ((eq? message 'create-line)
;             (lambda (self name
;                           fromnodeID
;                           tonodeID
;                           new-linkID-string)
;               (create-line name
;                            fromnodeID
;                            tonodeID
;                            new-linkID-string)))
    (obj-put this-obj 'create-line
             (lambda (self name
                           fromnodeID
                           tonodeID
                           new-linkID-string)
               (create-line name
                            fromnodeID
                            tonodeID
                            new-linkID-string)))
;            ((eq? message 'rename-line)
;             (lambda (self line-ID newname)
;               (rename-line line-ID newname)))
    (obj-put this-obj 'rename-line
             (lambda (self line-ID newname)
               (rename-line line-ID newname)))
;            ((eq? message 'del-line)
;             (lambda (self line-ID fromnodeID tonodeID)
;               (del-line line-ID fromnodeID tonodeID)))
    (obj-put this-obj 'del-line
             (lambda (self line-ID fromnodeID tonodeID)
               (del-line line-ID fromnodeID tonodeID)))
;            ((eq? message 'add-node)
;             (lambda (self new-nodeID name x y)
;               (add-node new-nodeID name x y)))
    (obj-put this-obj 'add-node
             (lambda (self new-nodeID name x y)
               (add-node new-nodeID name x y)))
;            ((eq? message 'rename-node)
;             (lambda (self in-nodeID)
;               (rename-node in-nodeID)))
    (obj-put this-obj 'rename-node
             (lambda (self in-nodeID)
               (rename-node in-nodeID)))
;            ((eq? message 'set-make-displayname-callback!)
;             (lambda (self in-callback)
;               (set! make-displayname in-callback)))
    (obj-put this-obj 'set-make-displayname-callback!
             (lambda (self in-callback)
               (set! make-displayname in-callback)))
;            ((eq? message 'del-node)
;             (lambda (self in-nodeID)
;               (del-node in-nodeID)))
    (obj-put this-obj 'del-node
             (lambda (self in-nodeID)
               (del-node in-nodeID)))
;            ((eq? message 'select-node)
;             (lambda (self in-nodeID)
;               (select-node in-nodeID)))
    (obj-put this-obj 'select-node
             (lambda (self in-nodeID)
               (select-node in-nodeID)))
;            ((eq? message 'set-node-colour)
;             (lambda (self in-nodeID in-colour)
;               (set-node-colour in-nodeID in-colour)))
    (obj-put this-obj 'set-node-colour
             (lambda (self in-nodeID in-colour)
               (set-node-colour in-nodeID in-colour)))
;            ((eq? message 'clear)
;             (lambda (self)
;               (if c (ask c 'clear))))
    (obj-put this-obj 'clear
             (lambda (self) (if c (ask c 'clear))))
;            ((eq? message 'populate-graph)
;             (lambda (self)
;               (populate-graph)))
    (obj-put this-obj 'populate-graph
             (lambda (self) (populate-graph)))
;            ((eq? message 'store-node-positions)
;             (lambda (self)
;               (store-node-positions)))
    (obj-put this-obj 'store-node-positions
             (lambda (self) (store-node-positions)))
;            ((eq? message 'store-node-position)
;             (lambda (self in-nodeID in-node)
;               (store-node-position in-nodeID in-node)))
    (obj-put this-obj 'store-node-position
             (lambda (self in-nodeID in-node)
               (store-node-position in-nodeID in-node)))
;            ((eq? message 'allow-overlap?)
;             (lambda (self)
;               (and c (ask c 'allow-overlap?))))
    (obj-put this-obj 'allow-overlap?
             (lambda (self)
               (and c (ask c 'allow-overlap?))))
;            ((eq? message 'set-allow-overlap!)
;             (lambda (self in-flag)
;               (if c (ask c 'set-allow-overlap! in-flag))))
    (obj-put this-obj 'set-allow-overlap!
             (lambda (self in-flag)
               (if c (ask c 'set-allow-overlap! in-flag))))
;            ((eq? message 'snap-to-grid?)
;             (lambda (self)
;               (and c (ask c 'snap-to-grid?))))
    (obj-put this-obj 'snap-to-grid?
             (lambda (self)
               (and c (ask c 'snap-to-grid?))))
;            ((eq? message 'set-snap-to-grid!)
;             (lambda (self in-flag)
;               (if c (ask c 'set-snap-to-grid! in-flag))))
    (obj-put this-obj 'set-snap-to-grid!
             (lambda (self in-flag)
               (if c (ask c 'set-snap-to-grid! in-flag))))
;            ((eq? message 'get-zoomfactor)
;             (lambda (self)
;               (get-zoomfactor)))
    (obj-put this-obj 'get-zoomfactor
             (lambda (self) (get-zoomfactor)))
;            ((eq? message 'set-zoomfactor!)
;             (lambda (self in-zoom)
;               (set-zoomfactor! in-zoom)))
    (obj-put this-obj 'set-zoomfactor!
             (lambda (self in-zoom)
               (set-zoomfactor! in-zoom)))
;            ((eq? message 'set-allow-repaint!)
;             (lambda (self in-flag)
;               (if c (ask c 'set-allow-repaint! in-flag))))
    (obj-put this-obj 'set-allow-repaint!
             (lambda (self in-flag)
               (if c (ask c 'set-allow-repaint! in-flag))))
;            ((eq? message 'refresh)
;             (lambda (self)
;               (if c (ask c 'my-on-paint))))
    (obj-put this-obj 'refresh
             (lambda (self)
               (if c (ask c 'my-on-paint))))
;            ((eq? message 'layout-all)
;             (lambda (self)
;               (if c (ask c 'layout-all))))
    (obj-put this-obj 'layout-all
             (lambda (self)
               (if c (ask c 'layout-all))))
;            ((eq? message 'set-line-draw-proc!)
;             (lambda (self in-proc)
;               (set! line-draw-proc in-proc)))
    (obj-put this-obj 'set-line-draw-proc!
             (lambda (self in-proc)
               (set! line-draw-proc in-proc)))
;            ((eq? message 'get-line-draw-proc)
;             (lambda (self)
;               line-draw-proc))
    (obj-put this-obj 'get-line-draw-proc
             (lambda (self) line-draw-proc))
;            (else (get-method named-obj message))))
    this-obj))