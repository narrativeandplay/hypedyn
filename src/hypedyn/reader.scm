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

;;
;; reader window
;; allows reading of text nodes, and following of links
;; 

(begin
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/container.scm")
  (require "../kawa/ui/frame.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/ui/panel.scm")
  (require "../kawa/ui/scrollpane.scm")
  (require "../kawa/ui/label.scm")
  (require "../kawa/ui/button.scm")
  (require "../kawa/ui/toolbar.scm")
  
  (require "../kawa/file.scm")
  (require "../kawa/strings.scm") ;;to-string
  (require "../kawa/system.scm")
  (require "../kawa/miscutils.scm")
  (require "../kawa/audio-kawa.scm")
  
  (require "../common/evaluator.scm")
  (require "../common/objects.scm") ;; ask
  (require "../common/datatable.scm") ;; get-list, get, reset-table 
  (require "../common/inspector.scm")
  (require "../common/fileio.scm")
  (require "../common/list-helpers.scm")
  (require "../common/runcode.scm")
  (require "../common/myhashtable.scm") ;;hash-table-for-each
  (require "../common/links.scm") ;; for style-link 
  (require "../common/hypertextpane.scm") ;; set-attribute-linkAction
  
  (require "config-options.scm")
  (require "datastructure.scm")
  (require "reader-pane.scm")
  
  (require 'list-lib)
  ;(require 'hash-table)
  )

; exports
(module-export close-nodereader
               doreaderopen-filename doreaderopen-file
               get-step-count
               forget-history goto-node doback
               get-read-nodeID get-prev-read-nodeID
               dorestart doreadstartnode create-nodereader
               visited? followed? previous? 
               ;dest-visited? alt-dest-visited? 
               holds? get-fact-value
               set-visited! set-followed! assert retract set-fact-value!
               get-link-type set-link-type!
               get-custom-cursor-image set-custom-cursor-image!
               get-user-data set-user-data!
               do-action get-action-from-rule-by-ID get-action-from-rule
               check-rule-condition ;eval-rule-expr
               set-custom-cursors! set-hover-links! set-hover-click!
               set-available-node-count! get-available-node-count
               set-visited-node-count! get-visited-node-count
               update-node-counter toggle-show-node-counter
               set-reader-background-color!
               set-reader-background-image! clear-reader-background-image!
               play-audio loop-audio loop-audio-forever stop-audio
               
               get-anywhere-nodes
               
               replace-link-text follow-link2 add-anywhere-link)

;;
;; config flags
;; 

;;;; Reader states

(define demo-mode #f)          ;; true if we're running in demo mode
(define demo-filename #f)      ;; demo filename
(define applet-mode #f)        ;; true if we're running in an applet

; keep track of number of nodes read, and number currently available to be read
; note that this doesn't really make sense, but is being used for the user study
(define visited-node-count 0)
(define available-node-count 0)
(define showing-node-count #f)


;;
;;;; UI objects
;; 

; variables to hold components of nodereader
(define nodereader-frame #f)
(define nodereader-toolbar-panel #f)
(define nodereader-toolbar-button-back #f)
(define nodereader-toolbar-button-restart #f)
(define nodereader-toolbar-button-mainmenu #f)
(define nodereader-toolbar-label-nodecount-label #f)
(define nodereader-toolbar-label-nodecount #f)
(define nodereader-canvas #f)
(define nodereader-pane #f)

; create the nodereader (init ui)
(define (create-nodereader in-frame in-applet-mode in-demo-mode in-demo-filename)
  ; remember the frame
  (set! nodereader-frame in-frame)

  ; remember if we're in applet mode
  (set! applet-mode in-applet-mode)

  ; remember if we're in demo mode
  (set! demo-mode in-demo-mode)
  (set! demo-filename in-demo-filename)

  ;; Add a horizontal panel to the frame, with centering, to hold toolbar buttons
  (set! nodereader-toolbar-panel (make-toolbar "Toolbar"))
  (set-toolbar-floatable nodereader-toolbar-panel #f)
  (add-component nodereader-frame nodereader-toolbar-panel 'border-north)

  ;; button to go back to previous node
  (set! nodereader-toolbar-button-back (make-button  "Back"))
  (if (not (sculptural?))
      (begin
        (add-component nodereader-toolbar-panel nodereader-toolbar-button-back)
        (add-actionlistener nodereader-toolbar-button-back
                            (make-actionlistener
                             (lambda (source) (doback))))
        (set-button nodereader-toolbar-button-back #f)))

  ;; button to restart reading
  (set! nodereader-toolbar-button-restart (make-button  "Restart"))
  (add-component nodereader-toolbar-panel nodereader-toolbar-button-restart)
  (add-actionlistener nodereader-toolbar-button-restart
                      (make-actionlistener
                       (lambda (source) (dorestart))))

  ;; button to return to main menu -for demo mode only
  (if (and demo-mode
           demo-filename)
      (begin
        (set! nodereader-toolbar-button-mainmenu (make-button  "Main menu"))
        (add-component nodereader-toolbar-panel nodereader-toolbar-button-mainmenu)
        (add-actionlistener nodereader-toolbar-button-mainmenu
                            (make-actionlistener
                             (lambda (source) (doreaderopen-filename demo-filename))))))

  (set! nodereader-toolbar-label-nodecount-label (make-label))
  (add-component nodereader-toolbar-panel nodereader-toolbar-label-nodecount-label)
  (set-text nodereader-toolbar-label-nodecount-label "Visited:")
  (set! nodereader-toolbar-label-nodecount (make-textfield "" 12))
  (add-component nodereader-toolbar-panel nodereader-toolbar-label-nodecount)
  (set-text-component nodereader-toolbar-label-nodecount #f #t)
  (set-component-visible nodereader-toolbar-label-nodecount-label #f)
  (set-component-visible nodereader-toolbar-label-nodecount #f)
  
  ; create editor
  (set! nodereader-pane (make-reader-pane 400 300 (is-basic-mode?)))
  (ask nodereader-pane 'init)
  (add-component in-frame (make-scrollpane (ask nodereader-pane 'getcomponent)) 'border-center))

; close the nodereader
(define (close-nodereader)
  (format #t "close-nodereader~%~!")
  (set-component-visible nodereader-frame #f)
  (stop-audio)
  ;(dispose-frame nodereader-frame)
  ;(set! nodereader-frame #f)
  )

;;;; show node counter
; hide/show label
(define (toggle-show-node-counter)
  (set! showing-node-count (not showing-node-count))
  (set-component-visible nodereader-toolbar-label-nodecount-label showing-node-count)
  (set-component-visible nodereader-toolbar-label-nodecount showing-node-count))

; reset showing node counter to false
(define (reset-show-node-counter)
  (set! showing-node-count #f)
  (set-component-visible nodereader-toolbar-label-nodecount-label showing-node-count)
  (set-component-visible nodereader-toolbar-label-nodecount showing-node-count))

; update node count
(define (update-node-counter)
  (set-text nodereader-toolbar-label-nodecount
            (string-append
             (number->string visited-node-count)
             (string-append " of " (number->string available-node-count)))))

;;
;;;; file i/o
;;

; name of file being read
(define nodereader-filename "")

; open given a filename
(define (doreaderopen-filename in-filename)
  (doreaderopen-file (make-file in-filename)))

; open given a file
(define (doreaderopen-file in-file)
  (if (not (eq? #f in-file))
      (begin
        ; forget history
        (forget-history)

        (display in-file)(newline)

        ; clear previous data
        (reset-table)
        (reset-uniqueID)
        (reset-document-ruleID)
        (reset-start-nodeID)
        (reset-card-shark)
        (reset-back-button)
        (reset-restart-button)
        
        ; load the file
        (if (load-from-file in-file)
            ; and start reading
            (doreadstartnode (path-last in-file))
            #t))
      #t))


;;;; node traversal
;;
;; overall document
;; 

;;; step count

; remember how many steps
; note: starts at 0, and is incremented when leaving first node,
; after the first node's after action, but before the document step
(define-private step-count 0)
(define (get-step-count)
  step-count)
(define (increment-step-count!)
  (set! step-count (+ step-count 1)))
(define (reset-step-count)
  (set! step-count 0))

; go back to previous node
(define (doback)
  (let ((prev-read-nodes (get-prev-read-nodes)))
    (if (not (null? prev-read-nodes))
        (begin
          (let* ((newlen (- (length prev-read-nodes) 1))
                 (prev-read-nodeID (car (list-tail prev-read-nodes newlen))))
            ; go to previous node, first rolling back history list xxx
            (set-prev-read-nodes! (take prev-read-nodes newlen))
            (goto-node prev-read-nodeID #f))))))

; currently read node and previously read node
(define read-nodeID #f)
(define (get-read-nodeID)
  read-nodeID)

; remember history of nodes visited
(define prev-read-nodes '())
(define (get-prev-read-nodes)
  prev-read-nodes)
(define (set-prev-read-nodes! newlist)
  (set! prev-read-nodes newlist))

; helper - get previous node, returns #f if none
(define (get-prev-read-nodeID)
  (let ((prev-read-nodes (get-prev-read-nodes)))
    (if (not (eq? '() prev-read-nodes))
        (begin
          (let* ((newlen (- (length prev-read-nodes) 1))
                 (prev-read-nodeID (car (list-tail prev-read-nodes newlen))))
            prev-read-nodeID))
        #f)))

; show history list
(define (doshowhistory)
  #t)

; forget history
(define (forget-history)
  ; disable back and restart buttons
  (set-button nodereader-toolbar-button-back #f)
  (set-button nodereader-toolbar-button-restart #f)

  ; forget previous nodes and current node
  (set-prev-read-nodes! '())
  (set! read-nodeID #f)

  ; clear visited flags in nodes
  (let ((node-list (get-list 'nodes)))
    (if node-list
        (map (lambda (n)
               (let ((thisnodeID (car n))
                     (thisnode (cdr n)))
                 (ask thisnode 'set-visited! 0)))
             node-list)))

  ; and clear followed flags in links
  (let ((link-list (get-list 'links)))
    (if link-list
        (map (lambda (l)
               (let ((thislinkID (car l))
                     (thislink (cdr l)))
                 (ask thislink 'set-followed! 0)))
             link-list)))

  ; and clear facts
  (let ((fact-list (get-list 'facts)))
    (if fact-list
        (map (lambda (a)
               (let ((thisfactID (car a)))
                 (retract thisfactID)))
             fact-list))))

; restart reading
(define (dorestart)
  (let ((current-nodeID read-nodeID))
    ; go to the start node; if that fails, go to current node
    (if (not (doreadstartnode nodereader-filename))
        (goto-node current-nodeID #f))))

; read from the start node
(define (doreadstartnode in-filename)
  ; remember filename
  (if (not in-filename)
      (set! nodereader-filename "Untitled")
      (set! nodereader-filename in-filename))

  ; forget history
  (forget-history)

  ; reset environment
  (reset-environment)
  
  ; reset step count
  (reset-step-count)
  
  ; reset custom cursors
  (set-custom-cursors! #f)
  
  ; reset hover links
  (set-hover-links! #f)
  
  ; reset hover click
  (set-hover-click! #f)
  
  ; stop any audio
  (stop-audio)
  
  ; reset background color
  (set-reader-background-color! (list 255 255 255))
  
  ; reset background image
  (clear-reader-background-image!)
  
  ; reset node counts
  (set! visited-node-count 0)
  (set! available-node-count 0)
  (reset-show-node-counter)

  ; set label
  (if (not applet-mode)
      (set-frame-title nodereader-frame (string-append "Reading: " nodereader-filename)))

  ; run document init action, if any
  ; note: init is currently stored as else-action
  (if (has-document-rule?)
      (let* ((doc-ruleID (get-document-ruleID))
             (doc-init-action (get-action-from-rule-by-ID 'else-action doc-ruleID)))
        (if doc-init-action
            (do-action doc-init-action))))

  (let ((snID (get-start-node)))
    (if snID
        (begin
          ; shuffle and deal if necessary
          (format #t "doreadstartnode: card-shark=~a~%~!\n" (card-shark?))
          (if (card-shark?)
              (shuffle-and-deal))

          ; and go to the first node
          (goto-node snID #f))
        #f)))

;;
;;;; card shark stuff
;;

; nodes currently in hand
(define nodes-in-hand '())
(define nodes-in-deck '())
(define-constant cards-dealt 7)

; extract anywhere nodes
(define (get-anywhere-nodes)
  (let ((node-list (get-list 'nodes))
        (anywhere-list '()))
    (if node-list
        (map
         (lambda (n)
           (let* ((this-nodeID (car n))
                  (this-node (cdr n)))
             (if (ask this-node 'anywhere?)
                 (set! anywhere-list (append anywhere-list (list this-nodeID))))))
         node-list))
    anywhere-list))

; shuffle the cards and deal
(define (shuffle-and-deal)
  (format #t "shuffling: ~a~%~!\n" (get-anywhere-nodes))
  ; shuffle the deck
  (let ((shuffled-anywhere-nodes (shuffle-list (get-anywhere-nodes))))
    (if (>= (length shuffled-anywhere-nodes) cards-dealt)
        (begin
          (format #t "after shuffling: ~a~%~!\n" shuffled-anywhere-nodes)
          
          ; deal 7 nodes into hand
          (set! nodes-in-hand (take shuffled-anywhere-nodes cards-dealt))
          (format #t "hand: ~a~%~!\n" nodes-in-hand)
          
          ; remaining nodes are the deck
          (set! nodes-in-deck (drop shuffled-anywhere-nodes cards-dealt))
          (format #t "deck: ~a~%~!\n" nodes-in-deck))
        (begin
          ; not enough cards, so all go into hand
          (set! nodes-in-hand (take shuffled-anywhere-nodes (length shuffled-anywhere-nodes)))
          
          ; and deck is empty
          (set! nodes-in-deck '())))))

; draw next card from deck
(define (draw-next-node)
  (if (not (eq? nodes-in-deck '()))
      (let ((next-node (take nodes-in-deck 1)))
        (format #t "draw node: deck: ~a, next node: ~a~%~!\n" nodes-in-deck next-node)

        (set! nodes-in-deck (drop nodes-in-deck 1))
        (set! nodes-in-hand (append nodes-in-hand next-node)))))

;; end card shark stuff

;;;; story states
; save the initial state, in case running the story messes up the nodes/links
(define saved-state #f)

; store initial state if not yet stored
(define (save-state)
  (if (not saved-state)
      ; nothing saved, so save the state
      (set! saved-state (save-to-sexpr))))

; restore saved state
(define (restore-saved-state)
  (if saved-state
      (runcode-just-sexpr saved-state)))

; goto given node in reader
(define (goto-node next-nodeID remember)
  (let ((prev-node (get 'nodes read-nodeID))
        (next-node (get 'nodes next-nodeID)))
    (if (not (eq? #f next-node))
        (begin
          ; check if there was a previous node
          (if prev-node
              (begin
                ; run after action for previous node
                ; note: after action is currently stored as else action
                (let* ((the-ruleID (ask prev-node 'rule))
                       (after-action (get-action-from-rule-by-ID 'else-action the-ruleID)))
                  (if after-action
                      (do-action after-action)))

                ; increment step count
                (increment-step-count!)))

          
          ; run document step action, if any
          ; note: step is currently stored as then-action
          (if (has-document-rule?)
              (let* ((doc-ruleID (get-document-ruleID))
                     (doc-step-action (get-action-from-rule-by-ID 'then-action doc-ruleID)))
                (if (and
                     doc-step-action
                     (check-rule-condition doc-ruleID))
                    (do-action doc-step-action))))

          ; run before action for next node (not sure about exact sequence for this)
          ; note: before action is currently stored as then action
          (if next-node
              (let* ((the-ruleID (ask next-node 'rule))
                     (before-action (get-action-from-rule-by-ID 'then-action the-ruleID)))
                (if before-action
                    (do-action before-action))))

          ;; trigger the rules with entered-node event 
          (if next-node
              (map (lambda (ruleID)
                     (do-rule-action 'entered-node ruleID)
                     ) (ask next-node 'rule-lst)))

          ; remember this node has been visited
          (if (not (visited? next-nodeID))
              (begin
                (set! visited-node-count (+ visited-node-count 1))
                (update-node-counter)))
          (ask next-node 'set-visited!
               (+ (ask next-node 'visited?) 1))
          
          ; if there was a previous node, need to take next card out
          ; of hand - card shark stuff
          ;(format #t "goto-node: card-shark=~a~%~!\n" (card-shark?))
          (if (and (card-shark?) 
                   prev-node)
              (begin
                ; if next node is an anywhere node, remove next node from hand - card shark stuff
                (if (ask next-node 'anywhere?)
                    (set! nodes-in-hand (delete next-nodeID nodes-in-hand)))

                ; take next node from deck and put in hand - card shark stuff
                (draw-next-node)))

          ; set contents - need to enable editing temporarily
          (ask nodereader-pane 'set-node! next-node)

          ; remember previous node
          (if (and remember read-nodeID)
              (set-prev-read-nodes! (append (get-prev-read-nodes) (list read-nodeID))))

          ; enable back and restart buttons
          (if (not (eq? '() (get-prev-read-nodes)))
              (begin
                ; have previous nodes, so enable both

                ; unless the author has disabled the restart button
                (if (not (disable-restart-button?))
                    (set-button nodereader-toolbar-button-restart #t)
                    (set-button nodereader-toolbar-button-restart #f))

                ; unless the author has disabled the back button
                (if (not (disable-back-button?))
                    (set-button nodereader-toolbar-button-back #t)
                    (set-button nodereader-toolbar-button-back #f)))
              (begin
                ; otherwise disable back button
                (set-button nodereader-toolbar-button-back #f)))

          ; remember read node    
          (set! read-nodeID next-nodeID)
          
          ;; cache link bounds before the replace link text is triggered by displayed-node
          (cache-link-bounds)
          
          ;; trigger rules
          (rule-check-trigger-links 'displayed-node next-nodeID) ;; trigger links text display
          (rule-check-trigger 'entered-node 'nodes next-nodeID) ;; trigger the node 
          ;(ask nodereader-pane 'rule-check-trigger-links next-nodeID)
          
          ; highlight links
          (ask nodereader-pane 'highlight-links)

          ; add anywhere nodes
          ;(format #t "goto-node: card-shark=~a~%~!\n" (card-shark?))
          (if (card-shark?)
              (ask nodereader-pane 'add-nodes-in-hand-links nodes-in-hand)
              (ask nodereader-pane 'add-anywherenode-links))
          
          ; show the nodereader window
          (set-component-visible nodereader-frame #t)

          #t)
        #f))
  (update-inspectors))

;; called when we set fact
;; a subset of goto-node (refresh the display of the node)
;; TODO: the mechanism to refresh a node's display after fact is update is problematic
;;       because if we replace text of a link first then set fact, we would replace that newly replaced text with old content
;;       this is commented out of set-fact procedures for now
(define (refresh-node)
  (define current-node (get 'nodes (get-read-nodeID)))
  
  (if current-node
      (begin
        ;; set contents - need to enable editing temporarily
        (ask nodereader-pane 'set-node! current-node)

        ;; trigger rules
        (rule-check-trigger-links 'displayed-node (get-read-nodeID)) ;; trigger links text display

        ;; highlight links
        (ask nodereader-pane 'highlight-links)

        ;; add anywhere nodes
        (ask nodereader-pane 'add-anywherenode-links)
        )))


;;;; conditions

; was a node visited?
(define (visited? dest-nodeID)
  (let ((dest-node (get 'nodes dest-nodeID)))
    (if dest-node
        (> (ask dest-node 'visited?) 0)
        #f)))

; check if given nodeID was the previous node
(define (previous? in-nodeID)
  (let ((prev-read-nodeID (get-prev-read-nodeID)))
    (display "Prev node: ") (display prev-read-nodeID) (newline)
    (and prev-read-nodeID (= prev-read-nodeID in-nodeID))))

;;; check if a link's destination has been visited
;(define (dest-visited? l)
;  (let* ((dest-nodeID (ask l 'destination))
;         (dest-node (get 'nodes dest-nodeID)))
;    (> (ask dest-node 'visited?) 0)))

;;; check if a link's alt destination has been visited
;(define (alt-dest-visited? l)
;  (let* ((dest-nodeID (ask l 'alt-destination))
;         (dest-node (get 'nodes dest-nodeID)))
;    (> (ask dest-node 'visited?) 0)))

;; operations

; set visited count on a node
(define (set-visited! in-nodeID in-value)
  (let ((the-node (get 'nodes in-nodeID)))
    (if the-node
        (ask the-node 'set-visited! in-value))))

; set followed count on a node
(define (set-followed! in-linkID in-value)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'set-followed! in-value))))

;;;; link information

; get the link type
(define (get-link-type in-linkID)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'get-link-type)
        'default)))

; set the type of a link
(define (set-link-type! in-linkID in-type)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'set-link-type! in-type))))
  
; get the user data in a link
(define (get-user-data in-linkID)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'get-user-data)
        '#f)))

; set the user data from a link
(define (set-user-data! in-linkID in-data)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'set-user-data! in-data))))

; check if a link has been followed
(define (followed? target-linkID)
  (let ((target-link (get 'links target-linkID)))
    (if target-link
        (> (ask target-link 'followed?) 0)
        #f)))

;;;; fact operations

; assert an fact
(define (assert in-factID)
  (let ((target-fact (get 'facts in-factID)))
    (if target-fact
        (begin
          (ask target-fact 'assert)
          ;(refresh-node)
          ))))

; retract an fact
(define (retract in-factID)
  (let ((target-fact (get 'facts in-factID)))
    (if target-fact
        (begin
          (ask target-fact 'retract)
          ;(refresh-node)
          ))))

; set the value of an fact
(define (set-fact-value! in-factID in-value)
  (let ((target-fact (get 'facts in-factID)))
;    (format #t "set-fact-value!: fact: ~a, value: ~a, target fact: ~a~%~!\n" 
;            in-factID in-value target-fact)
    (if target-fact
        (begin
          (ask target-fact 'set-value! in-value)
          ;(refresh-node)
          ))))

; check if an fact holds
(define (holds? target-factID)
  (let ((target-fact (get 'facts target-factID)))
    (if target-fact
        (ask target-fact 'holds?)
        #f)))

; get the value of an fact
(define (get-fact-value target-factID)
  (let ((target-fact (get 'facts target-factID)))
    (if target-fact
        (ask target-fact 'get-value)
        #f)))

;;;; rules operation
;; actions

; perform an action
; this version assumes stored as s-expressions
;;    (define (do-action in-action)
;;      (if in-action
;;          (let ((this-expr (ask in-action 'expr)))
;;            (try-catch
;;                (myeval this-expr the-global-environment)
;;              (ex <java.lang.Throwable>
;;                  (begin
;;                    (display (*:toString ex))(newline)
;;                    (*:printStackTrace ex)
;;                    #f))))))

; perform an action
; this version assumes stored as a string
(define (do-action actionID)
  (define in-action (get 'actions actionID))
  
  ;; debug code block
  (define ruleID (ask in-action 'ruleID))
  (define rule (get 'rules ruleID))
  
  (if in-action
      (let ((this-expr (ask in-action 'expr)))
        ; wrap the expression in a "begin" so its all one expression
;        (runcode (string-append "(begin\n" (to-string this-expr) "\n)") 
;                 display-results display-status)
        (runcode-just-sexpr this-expr) 
        )))

; get action from rule by ID
(define (get-action-from-rule-by-ID in-get-method in-ruleID)
  (if (not (eq? 'not-set in-ruleID))
      (get-action-from-rule in-get-method (get 'rules in-ruleID))
      #f))

; get action from rule
(define (get-action-from-rule in-get-method in-rule)
  (let ((the-action-ID (ask in-rule in-get-method)))
    (get 'actions the-action-ID)))

; evaluate a rule expression by ID
;; new name should be check-rule-condition
(define (check-rule-condition in-ruleID)
;  (if (not (eq? in-ruleID 'not-set))
;      (eval-rule-expr (get 'rules in-ruleID))
;      #t)
  (if (not (eq? in-ruleID 'not-set))
      (let ((rule-expr (ask (get 'rules in-ruleID) 'rule-expr)))
        ;; evaluate the expression in our evaluator
        (try-catch
            (myeval rule-expr)
          (ex <java.lang.Throwable>
              (begin
                (display (*:toString ex))(newline)
                (*:printStackTrace ex)
                #f))))
      #t)
  )

;;;; evaluator debugging
; display the results after running an action (debugging)
(define (display-results txt)
  (format #t "Results: ~a~%~!\n" txt))

; display the status after running an action (debugging)
(define (display-status txt)
  (format #t "Status: ~a~%~!\n" txt))

;;;; cursor over display options

; not sure where this should go - turn custom cursors on/off
(define (set-custom-cursors! in-flag)
  (if nodereader-pane
      (ask nodereader-pane 'set-custom-cursors! in-flag)))

; not sure where this should go - turn hover links on/off
(define (set-hover-links! in-flag)
  (if nodereader-pane
      (ask nodereader-pane 'set-hover-links! in-flag)))

; not sure where this should go - turn hover click on/off
(define (set-hover-click! in-flag)
  (if nodereader-pane
      (ask nodereader-pane 'set-hover-click! in-flag)))

; get the custom cursor image for a link
(define (get-custom-cursor-image in-linkID)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'get-custom-cursor-image)
        '#f)))

; set the custom cursor image for a link
(define (set-custom-cursor-image! in-linkID in-image)
  (let ((the-link (get 'links in-linkID)))
    (if the-link
        (ask the-link 'set-custom-cursor-image! in-image))))

;;;; node counts
; set number of available nodes, for user study
(define (set-available-node-count! in-count)
  (set! available-node-count in-count)
  (update-node-counter))
(define (get-available-node-count)
  available-node-count)
(define (set-visited-node-count! in-count)
  (set! visited-node-count in-count))
(define (get-visited-node-count)
  visited-node-count)

;;;; reader background 
; set the background colour: takes a list as defined in ui-kawa.scm
(define (set-reader-background-color! in-color)
  (if nodereader-pane
      (ask nodereader-pane 'set-background-color in-color)))

; set the background image: takes a filename
(define (set-reader-background-image! in-filename)
  (if nodereader-pane
      (ask nodereader-pane 'set-background-image in-filename)))

; clear the background image
(define (clear-reader-background-image!)
  (if nodereader-pane
      (ask nodereader-pane 'clear-background-image)))

;;
;;;; audio
;; 

; audio clip
(define the-clip #!null)

; play an audio file (in-filename is path to file)
(define (play-audio in-filename)
  (display "play audio ")(newline)
  
  ; stop current audio, if any
  (stop-audio)
  
  ;; assumes aplay program is available on linux (ubuntu's alsa player)
  ;; if using icedtea, play the audio through aplay, else the normal way is fine
  (set! the-clip
        (if (is-linux-os?)
            (play-audio-file-linux in-filename)
            (play-audio-file (make-file in-filename)))))

; loop an audio clip (already playing)
(define (loop-audio in-count)
  (display "loop audio") (newline)
  (if (not-null? the-clip)
      (loop-audio-clip the-clip in-count)))

; loop an audio clip forever (already playing)
(define (loop-audio-forever)
  (display "loop audio") (newline)
  (if (not-null? the-clip)
      (loop-audio-clip-forever the-clip)))

; stop current clip
; should this be different for linux?
(define (stop-audio)
  (if (not-null? the-clip)
      (begin
        (stop-audio-clip the-clip)
        (close-audio-clip the-clip)
        (set! the-clip #!null))))

;;;; replace text / highlight
(define (update-indices this-hashtable this-key this-index insert-index offset)
  (if (>= this-index insert-index)
      (hash-table-put! this-hashtable this-key (+ this-index offset))))

(define (cache-link-bounds)
  ; first pass - make copy of link positions, to be offset when
  ; text is changed by alternate links
  ;; moved to reader.scm from reader-pane's highlight-links
  (map (lambda (l)
         (let* ((thislink (get 'links l))
                (start-index (ask thislink 'start-index))
                (end-index (ask thislink 'end-index)))
           (hash-table-put! start-indices l start-index)
           (hash-table-put! end-indices l end-index)
           ))
       (ask (get 'nodes (ask nodereader-pane 'get-nodeID)) 'links)))

;; new addition to htlanguage

    ;; text-type is either 'text 'fact 
    ;; value would be a string when it is text-type 
    ;;       or a factID if it is a fact (num)
    ;; part of htlanguage
(define (replace-link-text text-type value linkID)
  (if nodereader-pane
      (let* ((link-obj (get 'links linkID))
             ;(start-index (ask link-obj 'start-index))
             ;(end-index (ask link-obj 'end-index))
             (start-index (hash-table-get start-indices linkID))
             (end-index (hash-table-get end-indices linkID))
             (newtext (cond ((eq? text-type 'text)
                             value)
                            ((eq? text-type 'fact)
                             (define fact-obj (get 'facts value))
                             (if fact-obj
                                 (begin
                                   (define fact-text (ask fact-obj 'get-value))
                                   (if fact-text
                                       fact-text
                                       "[fact not set]")
                                   )
                                 "[fact not found]"
                                 ))))
             (new-end-index (+ start-index (string-length newtext)))
             ;(use-alt-text (alttext? thislink))
             )

        ;; do the replacement in the textpane
        (ask nodereader-pane 'set-track-links! #f)
        (let ((nodereader-doc (ask nodereader-pane 'getdocument)))
;          (display "[delete from] ")(display start-index) (newline)
;          (display "  to ")(display end-index)(newline)
;          (display "  deleting these - ")(display (substring (ask nodereader-pane 'gettext) start-index end-index))(newline) 
          (set-text-delete nodereader-doc
                           start-index
                           (- end-index start-index))
;          (display "[insert] ")(display newtext)(newline)
;          (display "  from ")(display start-index)(newline) 
          (set-text-insert nodereader-doc
                           newtext
                           start-index)
          )
       (ask nodereader-pane 'set-track-links! #t) ;; htpane-obj (nodereader-pane)
        
        (define newtext-len (string-length newtext))
        (define oldtext-len (- end-index start-index))
        (define offset (- newtext-len oldtext-len))
        
        ; update link positions (shift the link bounds that comes after this link)
        (define new-end-index (+ end-index offset))
        (hash-table-for-each start-indices
                             (lambda (k v)
                               (update-indices start-indices k v end-index offset))) ;; was end-index
        (hash-table-for-each end-indices
                             (lambda (k v)
                               (update-indices end-indices k v end-index offset)))
        )))

;; part of htlanguage
;; check whether condition in ruleID is met then carry out follow link
;; a copy of follow-link
(define (follow-link2 linkID parent-ruleID link-type dest-nodeID) ;; the-action, use-link taken out
  (display "follow-link2 ")(newline)
  (if nodereader-pane 
      (begin
        (define the-link (get 'links linkID))
        ; perform action, if any, before going to destination
;        (if the-action
;            (do-action the-action))

        ; and go to destination node if link is active
        ;(if use-link
;            (if (and (or (not hover-links)
;                         (equal? link-type 'default))
;                     (check-rule-condition parent-ruleID)) ; Note: this check was only for hover-links - Alex
        (if (check-rule-condition parent-ruleID) ; I don't think this check is necessary - Alex
            (begin
              ; increment followed count
              (ask the-link 'set-followed!
                   (+ (ask the-link 'followed?) 1))

              ; goto node
              (goto-node dest-nodeID #t))

;                ;; else
;                ;(if hover-links
;                 ; show the hover link 
;                (let ((e (ask nodereader-pane 'get-lastmousemove)))
;                  (set-tooltip-text (ask the-link 'ID))
;                                        ; dispatch a mouseevent to trick tooltipmanager into displaying tooltip
;                  (if e
;                      (begin ;; QUESTION: why do it twice here?
;                        (dispatch-mouseevent (ask nodereader-pane 'getcomponent) e)
;                        (dispatch-mouseevent (ask nodereader-pane 'getcomponent) e)
;                        )))
            )))
  )

;; dummy for anywhere link adding action
;; not sure if we're going to use this or not
(define (add-anywhere-link nodeID)

  (let* ((thisnode (get 'nodes nodeID))
         (thisnodeName (ask thisnode 'name))
         (nodereader-doc (ask nodereader-pane 'getdocument))
         (thisStartPos (get-text-length nodereader-doc)))
    ;; add text
    (set-text-insert nodereader-doc
                     (string-append "\n" thisnodeName)
                     thisStartPos)

    ;; no format for \n char
    ;; Note: this fixed the clickable area beyond anywhere links to the right
    (set-text-style nodereader-doc
                        style-nolink
                        thisStartPos 1
                        #t)
    
    ;; highlight
    (if (> (ask thisnode 'visited?) 0)
        ;; already followed, so just underline
        (set-text-style nodereader-doc
                        style-followed-link
                        (+ 1 thisStartPos)
                        (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                        #t)
        ;; otherwise underline and bold
        (set-text-style nodereader-doc
                        style-link
                        (+ 1 thisStartPos)
                        (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                        #t))

    ;; and set clickback
    (let ((link-attribute-set (make-attribute-set)))
      (set-attribute-linkAction link-attribute-set
                                (lambda ()
                                  (goto-node nodeID #t)))
      (set-attribute-linkID link-attribute-set nodeID)
      (set-text-style nodereader-doc
                      link-attribute-set
                      (+ 1 thisStartPos)
                      (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                      #f)))
)
