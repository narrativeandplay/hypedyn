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
; subclass of hypertextpane (for now), implementing hypedyn 2.3 changes
(begin
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/container.scm")
  (require "../kawa/ui/panel.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/color.scm")
  (require "../kawa/strings.scm")
  (require "../common/objects.scm")
  (require "../common/datatable.scm") ;; get
  (require "../common/hypertextpane.scm") ;; parent class

  (require "datastructure.scm")
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

         (inserting-component #f) ; alex - rename this?
         )

    ; initialize the reader-pane
    (define (init)
      ; init the parent object
      (ask htpane-obj 'init)
      )


    ; flags

    ; enable/disable inserting component
    (define (set-inserting-component! m)
      (set! inserting-component m))

    ; override
    ; add a link as an expandable list of alternative texts
    ; TODO make sure it doesn't delete the links
    (define (addlink thislink)
      (format #t "**** addlink in editor-pane ****~%~!")
      ;; cache the value of track-undoable-edits and set it back later
      (define original-track-undoable-edits (ask this-obj 'track-undoable-edits?))
      (ask this-obj 'set-track-undoable-edits! #f)
      ;(ask this-obj 'set-track-links! #f)
      (ask this-obj 'set-track-dirty! #f)
      (set-inserting-component! #t)

      (let* ((the-editor (ask this-obj 'getcomponent))
             (the-doc (ask this-obj 'getdocument))
             (start-index (ask thislink 'start-index))
             (end-index (ask thislink 'end-index))
             (len (- end-index start-index))
             (old-selstart (ask this-obj 'getselstart))
             (old-selend (ask this-obj 'getselend))
             (this-linkID (ask thislink 'ID))
             (new-panel (make-panel))
             (new-pane #f)
             (this-link (get 'links this-linkID)))
        (ask this-obj 'setselection start-index end-index)

        ; formatting the panel
        (set-align-y new-panel 0.75)
        (set-border new-panel black-border)
        (set-container-layout new-panel 'vertical)

        ; add default text
        (set! new-pane (make-embedded-pane (get-doc-text the-doc start-index end-index)))
        (add-component new-panel new-pane)
        (set-textpane-tooltip new-pane "default")

        ; get the alternative text from the rules
        (let ((the-rules (ask this-link 'rule-lst)))
          (map (lambda (thisrule)
                 (let* ((has-alt-text? #f)
                        (thisrule-obj (get 'rules thisrule))
                        (thisrule-actions (ask thisrule-obj 'actions))
                        (thisrule-conditions (ask thisrule-obj 'conditions))
                        (if-not (if (ask thisrule-obj 'negate?) "not if" "if"))
                        (and-or (cond ((equal? (ask thisrule-obj 'and-or) 'and) "all are true:")
                                      ((equal? (ask thisrule-obj 'and-or) 'or) "any are true:")))
                        (fall-through  (if (ask thisrule-obj 'fall-through?) "continue" "stop"))
                        (thisrule-tooltip (string-append "<html>" if-not " " and-or "<br>"))
                        (thisrule-text "[no alternative text]"))
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
                                                        'name))
                                 (negate (case condition-operator
                                           ((0) "true")
                                           ((1) "false")
                                           ((2) "false")
                                           ((3) "true")
                                           (else "false")
                                           )))
                            (case condition-type
                              ((0) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "node \"" func-target-name "\" "
                                                                         (case condition-operator
                                                                           ((0) "visited")
                                                                           ((1) "not visited")
                                                                           ((2) "is previous")
                                                                           ((3) "is not previous")
                                                                           (else ""))
                                                                         "<br>"
                                                                         )))
                              ((1) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "link \"" func-target-name "\" "
                                                                         (case condition-operator
                                                                           ((0) "followed")
                                                                           ((1) "not followed")
                                                                           (else ""))
                                                                         "<br>")))
                              ((2) (set! thisrule-tooltip (string-append thisrule-tooltip
                                                                         "[true/false fact \"" func-target-name "\"] "
                                                                         (case condition-operator
                                                                           ((0) "true")
                                                                           ((1) "false")
                                                                           (else ""))
                                                                         "<br>")))
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
                   
                   (set! thisrule-tooltip (string-append thisrule-tooltip "then do:<br>"))
                   (map (lambda (thisaction)
                          (let* ((thisaction-obj (get 'actions thisaction))
                                 (thisaction-expr (ask thisaction-obj 'expr))
                                 (thisaction-type (car thisaction-expr)))
                            (cond
                             ((equal? 'replace-link-text thisaction-type)
                              ; text replacement, so extract the text
                              (if (eq? "alternative text" (list-ref thisaction-expr 1))
                                  (set! thisrule-text (list-ref thisaction-expr 2))
                                  (set! thisrule-text (string-append "[text fact \""
                                                                     (ask (get 'facts
                                                                               (list-ref thisaction-expr 2))
                                                                          'name)
                                                                     "\"]")))
                              (set! thisrule-tooltip (string-append thisrule-tooltip (symbol->string thisaction-type) "<br> "))
                              (set! has-alt-text? #t))
                             (else
                              ; TODO need to fill in details for other actions
                              (set! thisrule-tooltip (string-append thisrule-tooltip (symbol->string thisaction-type) "<br> ")))
                             )))
                        thisrule-actions)
                   
                   ; close the tooltip
                   (set! thisrule-tooltip (string-append thisrule-tooltip fall-through "</html>"))
                   
                   ; now set the text - only show if there's a replace text action
                   (if has-alt-text?
                       (begin
                         (set! new-pane (make-embedded-pane thisrule-text))
                         (add-component new-panel new-pane)
                         ; and add the tooltip
                         (set-textpane-tooltip new-pane thisrule-tooltip)))
                   ))
               the-rules))

        ; insert the component
        (textpane-insert-component the-editor new-panel)

        ;(setselection old-selstart old-selend)
        )

      (set-inserting-component! #f)
      (ask this-obj 'set-track-links! #t)
      (ask this-obj 'set-track-dirty! #t)
      ;; set back original value
      (ask this-obj 'set-track-undoable-edits! original-track-undoable-edits))

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

