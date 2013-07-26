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

; editor pane
; subclass of hypertextpane implementing hypedyn 2.3 changes
(begin
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/container.scm")
  (require "../kawa/ui/panel.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/ui/button.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/color.scm")
  (require "../kawa/strings.scm")
  (require "../kawa/miscutils.scm")
  (require "../common/objects.scm")
  (require "../common/datatable.scm") ;; get
  (require "../common/hypertextpane.scm") ;; parent class
  (require "../common/stringprocessing.scm")
  (require "../common/links.scm")
  
  (require "datastructure.scm")
  (require "config-options.scm")
  )

; export
(module-export make-editor-pane)

(module-static 'init-run)

; make an editor pane
(define (make-editor-pane w h
                          selectlink-callback
                          deletelink-callback
                          enable-newlink-button-callback
                          gettext-method
                          getlinks-method
                          nodelist-name)

  (let* ((htpane-obj (make-hypertextpane w h
                                         selectlink-callback
                                         deletelink-callback
                                         enable-newlink-button-callback
                                         gettext-method
                                         getlinks-method
                                         nodelist-name))
         (this-obj (new-object htpane-obj))
         )

    ; initialize the reader-pane
    (define (init)
      ; init the parent object
      (ask htpane-obj 'init)
      )
    
    ; override hypertextpane functions
    
    ; add a link as an expandable list of alternative texts
    ; TODO make sure it doesn't delete the links
    (define (addlink thislink)
      ;; cache the value of track-undoable-edits and set it back later
      (define original-track-undoable-edits (ask this-obj 'track-undoable-edits?))
      (ask this-obj 'set-track-undoable-edits! #f)
      
      ;(ask this-obj 'set-track-links! #f)
      (ask this-obj 'set-track-dirty! #f)

      (let* ((the-editor (ask this-obj 'getcomponent))
             (the-doc (ask this-obj 'getdocument))
             (start-index (ask thislink 'start-index))
             (end-index (ask thislink 'end-index))
             (len (- end-index start-index))
             (old-selstart (ask this-obj 'getselstart))
             (old-selend (ask this-obj 'getselend))
             (this-linkID (ask thislink 'ID))
             (text-panel (make-panel))
             (new-pane #f)
             (expand-button (make-button "+"))
             (first-panel (make-panel))
             (rest-panel (make-panel))
             (this-link (get 'links this-linkID))
             (button-action #f))
        (ask this-obj 'setselection start-index end-index)

        ; formatting the panel
        (set-align-y text-panel 0.75)
        (set-border text-panel black-border)
        (set-container-layout text-panel 'vertical)

        ; add default text
        (set-container-layout first-panel 'horizontal)
        ;(set! new-pane (make-embedded-pane (ask this-obj 'gettextselection start-index end-index)))
        (set! new-pane (make-embedded-pane (get-doc-text the-doc start-index end-index)))
        (add-component first-panel new-pane)
        (add-component text-panel first-panel)
        ;(set-textpane-tooltip new-pane "default")
        
        ; expand action
        (define (expand-button-action)
          (set-button-label expand-button "-")
          (add-component text-panel rest-panel)
          (set! button-action collapse-button-action))
        ; collapse action
        (define (collapse-button-action)
          (set-button-label expand-button "+")
          (remove-component text-panel rest-panel)
          (set! button-action expand-button-action))
        ; add actions
        (add-actionlistener expand-button
                            (make-actionlistener (lambda (source)
                                                   (button-action))))
        (set! button-action expand-button-action)
        (set-component-non-resizable-size expand-button 20 20)
        
        ; panel for the rest of the text
        (set-container-layout rest-panel 'vertical)
        
        ; go through the rules and extract the conditions and actions
        (let ((the-rules (ask this-link 'rule-lst)))
          (map (lambda (thisrule)
                 (let* ((has-alt-text? #f)
                        (thisrule-obj (get 'rules thisrule))
                        (thisrule-actions (ask thisrule-obj 'actions))
                        (thisrule-conditions (ask thisrule-obj 'conditions))
                        (if-not (if (ask thisrule-obj 'negate?) "not if" "if"))
                        (and-or (cond ((equal? (ask thisrule-obj 'and-or) 'and) "all")
                                      ((equal? (ask thisrule-obj 'and-or) 'or) "any")))
                        (fall-through  (if (ask thisrule-obj 'fall-through?) "continue" "stop"))
                        (thisrule-tooltip (string-append "<html>" if-not " " and-or 
                                                         " of the following conditions are true:<br>"))
                        (thisrule-text "[no alternative text]"))
                   
                   ; conditions
                   (map (lambda (thiscondition)
                          (let* ((thiscondition-obj (get 'conditions thiscondition))
                                 (condition-type (ask thiscondition-obj 'type))
                                 (condition-operator (ask thiscondition-obj 'operator))
                                 (func-target-id (ask thiscondition-obj 'targetID))
                                 (func-target-name (ask (get (case (ask thiscondition-obj 'type)
                                                               ((0) 'nodes)
                                                               ((1) 'links)
                                                               ((2) 'facts)
                                                               ((3) 'facts))
                                                             func-target-id)
                                                        'name)))
                            (set! thisrule-tooltip (string-append thisrule-tooltip "&nbsp;&nbsp;"))
                            (case condition-type
                              ; node
                              ((0) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "[node \"" func-target-name "\"] is "
                                                                         (case condition-operator
                                                                           ((0) "not visited")
                                                                           ((1) "visited") 
                                                                           ((2) "previous")
                                                                           ((3) "not previous")
                                                                           (else ""))
                                                                         "<br>"
                                                                         )))
                              ; link
                              ((1) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "[link \"" func-target-name "\"] is "
                                                                         (case condition-operator
                                                                           ((0) "not followed")
                                                                           ((1) "followed")
                                                                           (else ""))
                                                                         "<br>")))
                              ; true/false fact
                              ((2) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "[true/false fact \"" func-target-name "\"] is "
                                                                         (case condition-operator
                                                                           ((0) "false")
                                                                           ((1) "true")
                                                                           (else ""))
                                                                         "<br>")))
                              ; number fact
                              ((3) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "[number fact \"" func-target-name "\"] "
                                                                         (let* ((args-lst (ask thiscondition-obj 'numfact-args))
                                                                                (comparator (car args-lst))
                                                                                (operand-type (cadr args-lst))
                                                                                (operand-choice (caddr args-lst)))
                                                                           (string-append (cond
                                                                                           ((equal? comparator "<") "&lt")
                                                                                           ((equal? comparator ">") "&gt")
                                                                                           ((equal? comparator "<=") "&le")
                                                                                           ((equal? comparator ">=") "&ge")
                                                                                           (else comparator))
                                                                                          " "
                                                                                          (if (equal? operand-type "Input")
                                                                                              operand-choice
                                                                                              (string-append "[number fact \""
                                                                                                             (ask (get 'facts (string->number operand-choice)) 'name)
                                                                                                             "\"]"))))
                                                                         "<br>"))))))
                        thisrule-conditions)
                   
                   ; actions
                   (set! thisrule-tooltip (string-append thisrule-tooltip "then perform the following actions:<br>"))
                   (map (lambda (thisaction)
                          (let* ((thisaction-obj (get 'actions thisaction))
                                 (thisaction-expr (ask thisaction-obj 'expr))
                                 (thisaction-type (car thisaction-expr)))
                            (set! thisrule-tooltip (string-append thisrule-tooltip "&nbsp;&nbsp;"))
                            
                            (case thisaction-type
                              ; follow link
                              ((follow-link)
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Follow link to [node "
                                                                     (quote-nest (ask (get 'nodes (list-ref thisaction-expr 4)) 'name))
                                                                     "]<br>")))
                              ; assert
                              ((assert)
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Set [true/false fact "
                                                                     (quote-nest (ask (get 'facts (list-ref thisaction-expr 1)) 'name))
                                                                     "] to true<br>")))
                              ; retract
                              ((retract)
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Set [true/false fact ["
                                                                     (quote-nest (ask (get 'facts (list-ref thisaction-expr 1)) 'name))
                                                                     "] to false<br>")))
                              ; set value
                              ((set-value!)
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Set [text fact "
                                                                     (quote-nest (ask (get 'facts (list-ref thisaction-expr 1)) 'name))
                                                                     "] to "
                                                                     (quote-nest (list-ref thisaction-expr 2)) "<br>")))
                              ; add anywhere link
                              ((add-anywhere-link)
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Add anywhere link to node ["
                                                                     (to-string (list-ref thisaction-expr 1)) "]<br>"))) ;; TODO add anywhere link not implemented
                              ; replace link text
                              ((replace-link-text)
                               (let ((replace-type (list-ref thisaction-expr 1))
                                     (replace-value (list-ref thisaction-expr 2)))
                                 (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                       "Update text using "
                                                                       replace-type
                                                                       "<br>"))

                                 ;; differentiate between fact text or just alt text
                                 (if (eq? "alternative text" replace-type)
                                     ; alternative text
                                     (set! thisrule-text replace-value)
                                     ; text (or number?) fact replacement
                                     (set! thisrule-text (string-append "["
                                                                        (list-ref thisaction-expr 1)
                                                                        " "
                                                                        (quote-nest (ask (get 'facts
                                                                                              (list-ref thisaction-expr 2))
                                                                                         'name))
                                                                        "]"))))

                               ; remember we have alt text
                               (set! has-alt-text? #t))
                              ((show-in-popup)
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Show in popup [node " 
                                                                     (quote-nest (ask (get 'nodes (list-ref thisaction-expr 1)) 'name))
                                                                     "]<br>")))
                              ; set number fact
                              ((set-number-fact)
                               (define target-factID (list-ref thisaction-expr 1))
                               (define num-fact-mode (list-ref thisaction-expr 2))
                               (define fact-value
                                 (case num-fact-mode
                                   (("Input") (to-string (list-ref thisaction-expr 3)))
                                   (("Fact") (string-append "[number fact "
                                                            (quote-nest (ask (get 'facts (list-ref thisaction-expr 3)) 'name))
                                                            "]" ))
                                   (("Math")
                                    ;; (list op opr1 opr1-type opr2 opr2-type)
                                    (let* ((op            (list-ref (list-ref thisaction-expr 3) 0))
                                           (operand1      (list-ref (list-ref thisaction-expr 3) 1))
                                           (operand1-type (list-ref (list-ref thisaction-expr 3) 2))
                                           (operand2      (list-ref (list-ref thisaction-expr 3) 3))
                                           (operand2-type (list-ref (list-ref thisaction-expr 3) 4)))

                                      (string-append (if (eq? operand1-type "Input")
                                                         operand1
                                                         (string-append "[number fact "
                                                                        (quote-nest (ask (get 'facts (string->number operand1)) 'name))
                                                                        "]"))
                                                     " " op " "
                                                     (if (eq? operand2-type "Input")
                                                         operand2
                                                         (string-append "[number fact "
                                                                        (quote-nest (ask (get 'facts (string->number operand2)) 'name))
                                                                        "]")))))
                                   (("Random")
                                    ;; (list opr1 opr1-type opr2 opr2-type)
                                    (let* ((operand1      (list-ref (list-ref thisaction-expr 3) 0))
                                           (operand1-type (list-ref (list-ref thisaction-expr 3) 1))
                                           (operand2      (list-ref (list-ref thisaction-expr 3) 2))
                                           (operand2-type (list-ref (list-ref thisaction-expr 3) 3)))

                                      (string-append "a random number between "
                                                     (if (eq? operand1-type "Input")
                                                         operand1
                                                         (string-append "[number fact "
                                                                        (quote-nest (ask (get 'facts (string->number operand1)) 'name))
                                                                        "]"))
                                                     " and "
                                                     (if (eq? operand2-type "Input")
                                                         operand2
                                                         (string-append "[number fact "
                                                                        (quote-nest (ask (get 'facts (string->number operand2)) 'name))
                                                                        "]"))
                                                     )))))
                               (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                     "Set [number fact "
                                                                     (quote-nest (ask (get 'facts target-factID) 'name))
                                                                     "] to "
                                                                     fact-value
                                                                     "<br>"))))))
                        thisrule-actions)
                   
                   ; close the tooltip
                   (set! thisrule-tooltip (string-append thisrule-tooltip "&nbsp;&nbsp;" fall-through "</html>"))
                   
                   ; now set the text - only show if there's a replace text action
                   (if (or has-alt-text? (show-all-rules?))
                       (begin
                         (set! new-pane (make-embedded-pane thisrule-text))
                         (set-text-component new-pane #f #f)
                         (add-component rest-panel new-pane)
                         ; and add the tooltip
                         (set-textpane-tooltip new-pane thisrule-tooltip)))))
               the-rules))

        ; if there are any alternative text panels in the rest panel, then add the expand button
        (if (not (null? (get-container-children rest-panel)))
            (add-component first-panel expand-button))

        ; insert the component
        (textpane-insert-component the-editor text-panel)

        ; and set clickback: this is used for the ID, not the action, maybe remove the action?
        (ask this-obj 'set-clickback this-linkID start-index 1) ; len is 1
        )

      (ask this-obj 'set-track-links! #t)
      (ask this-obj 'set-track-dirty! #t)
      
      ;; set back original value
      (ask this-obj 'set-track-undoable-edits! original-track-undoable-edits))

    (define (quote-nest str)
      (string-append "\"" str "\""))
    
    (define (make-embedded-pane the-text)
      (let ((new-pane (make-textpane)))
        (textpane-replace-selection new-pane the-text)
        (set-border new-pane (make-line-border (get-gray-javacolor) 1))
        (set-background-color new-pane (get-lightgray-javacolor))
        new-pane))

    ; message handling                  
    (obj-put this-obj 'init
             (lambda (self)
               (init)))
    (obj-put this-obj 'addlink
             (lambda (self in-link)
               (addlink in-link)))
    this-obj))

