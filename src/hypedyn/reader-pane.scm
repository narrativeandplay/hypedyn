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

; reader pane
; subclass of hypertextpane with hypedyn-specific functionality

(require "../kawa/ui/events.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/cursor.scm")

(require "../kawa/strings.scm")

(require "../common/math.scm")
(require "../common/links.scm")
(require "../common/myhashtable.scm")
(require "../common/evaluator.scm")
(require "../common/objects.scm") 
(require "../common/datatable.scm") ;; get-list, get, 
(require "../common/hypertextpane.scm") ;; make-hypertextpane-readonly
(require "../common/runcode.scm")

(require "config-options.scm")
(require "reader.scm")
(require 'hash-table)
(require 'srfi-1) ;; find 

;; TODO: ticket #143: remove CHI study hacks:
;; - hover-links - requires global link mouseover + click action and get-active-linkID
;; - custom cursors - requires global link mouseover action and get-active-linkID

; exports
(module-export make-reader-pane
               rule-check-trigger-links
               rule-check-trigger
               start-indices
               end-indices
               
               do-rule-action
               find-action2
               )

(define start-indices (make-hash-table))
(define end-indices (make-hash-table))


;; action name is the symbol assigned to the primitive procedure (eg 'follow-link 'retract)
(define (find-action-helper in-rule-lst in-action-name)
  (if (null? in-rule-lst)
      #f
      (begin
        (define condition-satisfied? (check-rule-condition (car in-rule-lst)))
        (define fall-through? (ask (get 'rules (car in-rule-lst)) 'fall-through?))
        (define found-action?
          (find (lambda (actionID) ;;find action in this rule
                  (define action (get 'actions actionID))
                  (define sexpr (ask action 'expr))
                  (equal? (car sexpr) in-action-name)
                  ) (ask (get 'rules (car in-rule-lst)) 'actions)))

        ;; if checking condition need to check take note not to fall through when condition satisfied
        (if check-condition?
            (if (and found-action? condition-satisfied?)
                #t
                (if (or (and condition-satisfied?
                             fall-through?)
                        (and (not condition-satisfied?)))
                    (find-action-helper (cdr in-rule-lst) in-action-name)
                    #f))
            ;; if not checking just need to look through the whole in-rule-lst
            (if found-action?
                #t
                (find-action-helper (cdr in-rule-lst) in-action-name))
            )
        ))) ;; end of helper

    ;; if check-condition? true, we check whether the 
    ;;   condition for the follow link action is satisfied
    ;; if check-condition? false, we only check the existence 
(define (find-action obj-ID obj-type check-condition? action-name)
  (define this-obj
    (case obj-type
      ((link) (get 'links obj-ID))
      ((node) (get 'nodes obj-ID))))
  
  ;; for debug (delete later)
  (define obj-name (ask this-obj 'name))
  
  (define rule-lst (ask this-obj 'rule-lst))

  ;; start the helper
  (find-action-helper rule-lst action-name)
  )

;; a more powerful generic version of find-action
;; returns a list of actionID that satisfy this pred
;; NOTE: pred should work on actionID
(define (find-action2 pred)
  (filter pred
        ;; get list of action
        (map (lambda (o) (car o)) (get-list 'actions))))

; read-only hypertextpane
(define (make-reader-pane
         w h
         basic-mode)
  (let* ((htpane-obj (make-hypertextpane-readonly
                      w h
                      #f
                      #f
                      #f
                      'content
                      'links
                      'nodes))
         (this-obj (new-object htpane-obj))
         (bg-change #f)
         
         ;; for studies
         (custom-cursors #f)
         (hover-links #f) ;; for tooltips
         (hover-click #f) ;; for tooltips
         (current-cursor-image #f)) ;; cursor image

    ; initialize the reader-pane
    (define (init)
      ; init the parent object
      (ask htpane-obj 'init)

      ; set mouseover callback
      (ask htpane-obj 'set-mouseover-callback! mouseover-callback)
      
      ; hack: make tooltips show immediately and remain forever, move this to be set only when window activated
      (set-tooltip-initial-delay 0)
      (set-tooltip-dismiss-delay (max-integer))
      
      ; key listener for user studies
      (add-keylistener (ask htpane-obj 'getcomponent)
                       (make-keylistener handle-keypressed handle-keytyped handle-keyreleased)))
    
    ; handle keys for user studies
    (define (handle-keypressed e)
      (if (user-study?)
          ; assume that for user study stories, handleKeyPressed is defined in document rule
          (myeval (list 'handleKeyPressed (get-key-event e)))))

    (define (handle-keytyped e)
      'ok)
    (define (handle-keyreleased e)
      'ok)

    (define (is-basic-mode?)
      basic-mode)

    ; which node is being edited
    ; note: override hypertextpane as we don't want to use setlinks
    (define (set-node! in-node)
      (ask htpane-obj 'set-nodeID! (ask in-node 'ID))
      (settext (ask in-node 'content))
      (set-cursor-pos (ask htpane-obj 'getcomponent) 0))

    ; define clickback to handle selection of link
    (define (nodereader-clickback clicked-linkID)
      ; trigger the clicked link
      (trigger-link clicked-linkID))

    ; trigger a link
    (define (trigger-link clicked-linkID)
      ; trigger the link
      (if clicked-linkID
          (let* ((this-link (get 'links clicked-linkID))
                 (then-action (get-then-action this-link))
                 (else-action (get-else-action this-link))
                 (link-type (get-link-type clicked-linkID)))
            (rule-check-trigger 'clicked-link 'links clicked-linkID)
            )))
    
    ;; check if a link can be followed (check-condition #t)
    ;; check if a link is available (check-condition #f)
    (define (follow-link-available? linkID check-condition?)
      (find-action linkID 'link check-condition? 'follow-link))
    
    ;; link actions

    ; get action from a link
    (define (get-action-from-link in-get-method in-link)
      (if in-link
          (get-action-from-rule-by-ID in-get-method (ask in-link 'rule))
          #f))

    ; get then action from a link
    (define (get-then-action in-link)
      (get-action-from-link 'then-action in-link))

    ; get else action from a link
    (define (get-else-action in-link)
      (get-action-from-link 'else-action in-link))

    ; check if link has actions
    (define (has-actions? in-link)
      (or (has-then-action? in-link)
          (has-else-action? in-link)))

    ; check if link has then action
    (define (has-then-action? in-link)
      (get-then-action in-link))

    ; check if link has else action
    (define (has-else-action? in-link)
      (get-else-action in-link))

    ; highlight links in reader
    (define (highlight-links)
      (display "[HIGHLIGHT]")(newline)
      (let ((thisnode (get 'nodes (ask htpane-obj 'get-nodeID))))

        ; second pass - highlight links, update text, and set clickbacks
        (map (lambda (l)
               (let* ((nodereader-doc (ask htpane-obj 'getdocument))
                      (thislink (get 'links l))
                      (this-linkID (ask thislink 'ID))
                      (start-index (hash-table-get start-indices l #f))
                      (end-index (hash-table-get end-indices l #f)))
                 
                 ;; check whether follow link action exists and its condition satisfied
                 (if (follow-link-available? l #t) ;; check whether I should underline it
                     (begin ;; (substring (ask htpane-obj 'gettext) (ask thislink 'start-index) (ask thislink 'end-index))

                       ;; check whether we should bold it
                       (if (followed? this-linkID) ; changed to followed? - alex
                                                   ; already followed, so just underline
                           (set-text-style nodereader-doc
                                           style-followed-link
                                           start-index
                                           (- end-index start-index)
                                           #t)
                                        ; otherwise underline and bold
                           (set-text-style nodereader-doc
                                           style-link
                                           start-index
                                           (- end-index start-index)
                                           #t))

                                        ; and set clickback
                       (let ((link-attribute-set (make-attribute-set)))
                         (set-attribute-linkAction link-attribute-set
                                                   (lambda ()
                                                     (nodereader-clickback l)))
                         (set-attribute-linkID link-attribute-set this-linkID)
                         (set-text-style nodereader-doc
                                         link-attribute-set
                                         start-index
                                         (- end-index start-index)
                                         #f)))
                     ;; check for follow link action available but with conditions not satisfied
                     (if (follow-link-available? l #f)
                         (begin
                           (if (followed? this-linkID)
                               ;; strange to be followed and yet condition unmet?
                               (set-text-style nodereader-doc
                                               style-nolink
                                               start-index
                                               (- end-index start-index)
                                               #t)
                               ; not followed, but follow link action is there (bold it)
                               (set-text-style nodereader-doc
                                               style-disabled-link
                                               start-index
                                               (- end-index start-index)
                                               #t)
                               ))))
                     ))
             (ask thisnode 'links))))

    
    ; check if link has alternate link, and if it was enabled
    ;; TODO: altlink? alttext? uselink? all outdated
    (define (altlink? l)
      (and (ask l 'use-alt-destination)
           (not (= -1 (ask l 'alt-destination)))))
    ; check if link has alternate text, and if it was enabled
    (define (alttext? l)
      (ask l 'use-alt-text))
    ; check if link is enabled
    (define (uselink? l)
      (ask l 'use-destination))
    
    

    ; add links to anywhere nodes
    ;; NOTE : this is not called as an action like the others (just saving time because its troublesome but can be done)
    ;;        however, find-action looks for add-anywhere-link action in the rules of 
    ;;        the anywhere node and check its condition before it adds the anywhere link 
    ;;        therefore deleting the action/rule containing add-anywhere-link action would cause the link to not be ever added
    (define (add-anywherenode-links)
      ;; add in the divider
      (define (add-anywherenode-divider nodereader-doc)
        ; leave a line before anywhere nodes
        (set-text-insert nodereader-doc "\n-----" (get-text-length nodereader-doc))

        ; make sure line isn't formatted by an adjacent link
        (set-text-style nodereader-doc
                        style-nolink
                        (- (get-text-length nodereader-doc) 5)
                        5
                        #t))
        
      ; add in anywhere node links, if any (and if condition for it is satisfied)
      (let ((node-list (get-list 'nodes))
            (nodereader-doc (ask htpane-obj 'getdocument))
            (first-link-added #f))
        (if node-list
            (begin
              ; disable link tracking
              (ask htpane-obj 'set-track-links! #f)
              
              ; for each node, run through and check if it should be added to the anywhere node links
              (map (lambda (n)
                     (let* ((thisnodeID (car n))
                            (thisnode (cdr n))
                            (rule-lst (ask thisnode 'rule-lst)))
                       ; if its an anywhere node, not the current node, and its rule is satisfied
                       (map (lambda (ruleID)
                              (if (and (ask thisnode 'anywhere?)
                                       (not (= thisnodeID (get-read-nodeID)))
                                       (find-action thisnodeID 'node #t 'add-anywhere-link)
                                       ;(check-rule-condition ruleID) ;(ask thisnode 'rule)))
                                       )
                                  (begin
                                        ; if its the first anywhere node, add divider
                                    (if (not first-link-added)
                                        (begin
                                          (add-anywherenode-divider nodereader-doc)
                                          (set! first-link-added #t)))

                                        ; add the link
                                    (let ((thisnodeName (ask thisnode 'name))
                                          (thisStartPos (get-text-length nodereader-doc)))
                                        ; add text
                                      (set-text-insert nodereader-doc
                                                       (string-append "\n" thisnodeName)
                                                       thisStartPos)

                                        ; highlight
                                      (if (> (ask thisnode 'visited?) 0)
                                        ; already followed, so just underline
                                          (set-text-style nodereader-doc
                                                          style-followed-link
                                                          (+ 1 thisStartPos)
                                                          (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                                                          #t)
                                        ; otherwise underline and bold
                                          (set-text-style nodereader-doc
                                                          style-link
                                                          (+ 1 thisStartPos)
                                                          (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                                                          #t))

                                        ; and set clickback
                                      (let ((link-attribute-set (make-attribute-set)))
                                        (set-attribute-linkAction link-attribute-set
                                                                  (lambda ()
                                                                    (goto-node thisnodeID #t)))
                                        (set-attribute-linkID link-attribute-set thisnodeID)
                                        (set-text-style nodereader-doc
                                                        link-attribute-set
                                                        (+ 1 thisStartPos)
                                                        (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                                                        #f)))))
                              ) rule-lst)
                       ))
                   node-list)
              (ask htpane-obj 'set-track-links! #t)))))

    ; add "nodes-in-hand" links - card shark stuff
    (define (add-nodes-in-hand-links nodes-in-hand)
      ; add in anywhere node links, if any
      (let ((nodereader-doc (ask htpane-obj 'getdocument)))
        ; disable link tracking
        (ask htpane-obj 'set-track-links! #f)

        ; leave a line before anywhere nodes
        (set-text-insert nodereader-doc "\n-----" (get-text-length nodereader-doc))

        ; make sure line isn't formatted by an adjacent link
        (set-text-style nodereader-doc
                        style-nolink
                        (- (get-text-length nodereader-doc) 5)
                        5
                        #t)

        ; for each node, run through and check if it should be added to the node-in-hand links
        (map (lambda (thisnodeID)
               (let ((thisnode (get 'nodes thisnodeID)))
                 (let ((thisnodeName (ask thisnode 'name))
                       (thisStartPos (get-text-length nodereader-doc)))
                   ; add text
                   (set-text-insert nodereader-doc
                                    (string-append "\n" thisnodeName)
                                    thisStartPos)

                   ; highlight - is its rule satisfied?
                   (if (check-rule-condition (ask thisnode 'rule))
                       ; yes, so enable
                       (begin
                         ; underline and bold
                         (set-text-style nodereader-doc
                                         style-link
                                         (+ 1 thisStartPos)
                                         (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                                         #t)

                         ; and set clickback
                         (let ((link-attribute-set (make-attribute-set)))
                           (set-attribute-linkAction link-attribute-set
                                                     (lambda ()
                                                       (goto-node thisnodeID #t)))
                           (set-attribute-linkID link-attribute-set thisnodeID)
                           (set-text-style nodereader-doc
                                           link-attribute-set
                                           (+ 1 thisStartPos)
                                           (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                                           #f)))
                       (begin
                         ; otherwise just set bold
                         (set-text-style nodereader-doc
                                         style-disabled-link
                                         (+ 1 thisStartPos)
                                         (- (get-text-length nodereader-doc) (+ 1 thisStartPos))
                                         #t))))))
             nodes-in-hand)
        (ask htpane-obj 'set-track-links! #t)))

    ; turn context cursors on/off
    (define (set-custom-cursors! in-flag)
      (set! custom-cursors in-flag))

    ; turn hover links on/off
    (define (set-hover-links! in-flag)
      (set! hover-links in-flag))

    ; turn hover click on/off
    (define (set-hover-click! in-flag)
      (set! hover-click in-flag))

    ; turn bg change on/off
    (define (set-bg-change! in-flag)
      (set! bg-change in-flag))

    ; set text - override to reset cursor
    (define (settext in-text)
      (ask htpane-obj 'settext in-text)
      (set-cursor-type (ask htpane-obj 'getcomponent) 'default))

    ; mouseover callback
    (define (mouseover-callback is-link linkID)
      (let ((nodereader-text (ask htpane-obj 'getcomponent)))
        (if is-link
            (begin
              (if custom-cursors
                  ; if using custom cursors, set based on link's custom cursor, if any
                  (let ((cursor-image (get-custom-cursor-image linkID)))
                    ; over link, so change to custom cursor
                    (if cursor-image
                        ; set the cursor
                        (if (or (not (check-cursor-type nodereader-text 'custom))
                                (not (eq? current-cursor-image cursor-image)))
                            (begin
                              (set-custom-cursor! nodereader-text cursor-image "custom")
                              (set! current-cursor-image cursor-image)))
                        ; image missing, so just set to hand
                        (if (not (check-cursor-type nodereader-text 'hand))
                            (set-cursor-type nodereader-text 'hand))))
                  ; otherwise, just show hand
                  (if (not (check-cursor-type nodereader-text 'hand))
                      (set-cursor-type nodereader-text 'hand)))
              (if (and hover-links (not hover-click))
                  ; if using hover-links, show based on link
                  (let ((link-type (get-link-type linkID)))
                    ; over link, so show hover-text
                    (if (equal? link-type 'hover)
                        ; show the hover link
                        (show-hover-link linkID)
                        ; otherwise no tooltip
                        (set-tooltip-text -1)))
                  ; otherwise not using hover-links, so no tooltip
                  (if (not hover-links)
                      (set-tooltip-text -1))))
            ; otherwise change to default cursor
            (begin
              (if (not (check-cursor-type nodereader-text 'default))
                  (set-cursor-type nodereader-text 'default))
              (set-tooltip-text -1)))))
    
    ; show a hover link
    (define (show-hover-link linkID)
      (let* ((the-link (get 'links linkID))
             (then-action (get-then-action the-link))
             (else-action (get-else-action the-link)))
        ; set text
        (set-tooltip-text linkID)

        ; increment followed count - should only do this when first mouse-over xxx
        (ask the-link 'set-followed!
             (+ (ask the-link 'followed?) 1))
        
        ; and execute action, if any
        ;; TODO: follow-link? outdated, then-action else action not working
;        (if (follow-link? linkID)
;            (do-action then-action)
;            (do-action else-action))
        ))
      
    ; set the tooltip text on rollover for links which only show tooltips
    ;; TODO: get-destnode-text and get-destnode-text is outdated
    ;;       use of follow-link?,  uselink?, ask 'destination, altlink?, alt-destination 
    ;;       are all from the old framework
    ;;       set-tooltip-text not used at the moment so no problems yet
    (define (set-tooltip-text linkID)
      (let ((the-destnode-text (get-destnode-text linkID))
            (nodereader-text (ask htpane-obj 'getcomponent)))
        (if the-destnode-text
            ; there's text to show, so set as the tooltip
            (set-textpane-tooltip nodereader-text
                                  (format-node-text the-destnode-text))
            ; otherwise set to null (to hide the tooltip)
            (set-textpane-tooltip nodereader-text
                                  #!null))))

    ; get the text of destination node for a given link
    ; also need to apply any alt-text and actions for the node?
    (define (get-destnode-text linkID)
      ; get the link
      (let ((the-link (get 'links linkID)))
        (if the-link
            ; got the link, now get the destination node
            (let ((the-nodeID
                   (if (and (follow-link? linkID) (uselink? the-link))
                       ; its a normal destination, and can follow
                       (ask the-link 'destination)
                       ; otherwise, check if there's an alt link
                       (if (altlink? the-link)
                           ; yes, use this instead
                           (ask the-link 'alt-destination)
                           ; otherwise no destination node available
                           -1))))
              (if (= the-nodeID -1)
                  ; no destination, so return empty string
                  ""
                  ; otherwise get the text from the dest node
                  (let ((the-node (get 'nodes the-nodeID)))
                    ; update node counter and visited status
                    (if (not (visited? the-nodeID))
                        (begin
                          (set-visited-node-count! (+ (get-visited-node-count) 1))
                          (update-node-counter)))
                    
                    ; and update visited count - should only do this when first mouse-over xxx
                    (ask the-node 'set-visited!
                         (+ (ask the-node 'visited?) 1))
                    
                    ; and get the text
                    (ask the-node 'content))))
            ; no link, so return false
            #f)))
    
    ; format text for tooltip: add <html></html> tags around text, and
    ; put in <br> every 50 characters
    (define (format-node-text in-text)
      ; recursively add in <br> every 50 characters
      (define (format-node-text-helper in-sub-text)
        (let ((slashn-index (string-indexof in-sub-text "\n"))
              (len (string-length in-sub-text)))
          ; first check if there's a \n before character 50
          (if (and (> slashn-index -1)
                   (< slashn-index 50))
              ; break at the \n
              (string-append (substring in-sub-text 0 slashn-index)
                             "<br>"
                             (format-node-text-helper (substring in-sub-text (+ slashn-index 1) len)))
              ; otherwise, check the length
              (if (> len 50)
                  (let* ((this-line (substring in-sub-text 0 50))
                         (last-space-index (string-lastindexof this-line " "))
                         ; be careful in case there is no " "
                         (break-index (if (= last-space-index -1) 50 (min last-space-index 50))))
                    (string-append (substring in-sub-text 0 break-index)
                                   "<br>"
                                   (format-node-text-helper (substring in-sub-text break-index len))))
                  in-sub-text))))
      ; return the text broken by <br>s, bracketed by html tags
      (string-append "<html>" (format-node-text-helper in-text) "</html>"))

    ; message handling                  
    (obj-put this-obj 'init
             (lambda (self) (init)))
    (obj-put this-obj 'set-node!
             (lambda (self in-node)
               (set-node! in-node)))
    (obj-put this-obj 'highlight-links
             (lambda (self)
               (highlight-links)))
    
    (obj-put this-obj 'add-anywherenode-links
             (lambda (self)
               (add-anywherenode-links)))
    (obj-put this-obj 'add-nodes-in-hand-links
             (lambda (self the-nodes)
               (add-nodes-in-hand-links the-nodes)))
    (obj-put this-obj 'settext
             (lambda (self in-text)
               (settext in-text)))
    (obj-put this-obj 'set-custom-cursors!
             (lambda (self in-flag)
               (set-custom-cursors! in-flag)))
    (obj-put this-obj 'set-hover-links!
             (lambda (self in-flag)
               (set-hover-links! in-flag)))
    (obj-put this-obj 'set-hover-click!
             (lambda (self in-flag)
               (set-hover-click! in-flag)))
    (obj-put this-obj 'set-bg-change!
             (lambda (self in-flag)
               (set-bg-change! in-flag)))
    this-obj))

;; RULE HANDLING

;; carry out the action from this rule
;; only do the actions relevant to the event-type
;; Note: do-action should really be triggered through this function
;;       if we want to govern the triggering of actions with the right event-type
;; TODO: clean up do-action to all go through this do-rule-action
(define (do-rule-action event-type ruleID)
  (display "do-rule-action ")(display event-type)(newline)
  (define action-lst (ask (get 'rules ruleID) 'actions))

  (map (lambda (actionID)
         ;; if relevant to this event type, do action
         (define action-event-type (ask (get 'actions actionID) 'type))
         ;(display "action and event ")(display action-event-type)(display " ")(display event-type)(newline)
         ;(display "action expr ")(display (ask (get 'actions actionID) 'expr))(newline)
         (if (equal? action-event-type event-type)
                 ;(equal? action-event-type 'goto-node)) ;; testing goto-node event type
             (begin
               ;(display "event type matched ")(display event-type)(newline)
             (do-action actionID)))
         ) (ask (get 'rules ruleID) 'actions))
  )

;; obj can be a link or node..
;; TODO: doc rule should fit into this somehow
;; obj-type is either 'links or 'nodes for now
(define (rule-check-trigger event-type obj-type obj-ID)
  
  (define obj (get obj-type obj-ID))
  (define rule-lst (ask obj 'rule-lst))
 
  ;; enforce rule fall through checking
  (define (traverse-rule rule-lst)
    (if (not (null? rule-lst))
        (begin
          (if (check-rule-condition (car rule-lst))
              (begin
                (do-rule-action event-type (car rule-lst))
                ;; if this rule triggered, check whether we allow fall through
                (if (ask (get 'rules (car rule-lst)) 'fall-through?)
                    (traverse-rule (cdr rule-lst))))
              ;; check next ruleID
              (traverse-rule (cdr rule-lst)))
          )))
  
  ;; call the local helper function
  (traverse-rule rule-lst))

;; goes through all the links of this node and 
;; check for rule triggers
(define (rule-check-trigger-links event-type nodeID)
  (map (lambda (linkID)
         (rule-check-trigger event-type 'links linkID))
       (ask (get 'nodes nodeID) 'links))
  )
