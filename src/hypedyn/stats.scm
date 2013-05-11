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
;; display stats for story analysis
;;

(begin
  (require "../common/objects.scm")
  (require "../common/datatable.scm") ;;get, del, put
  (require "../common/myhashtable.scm")
  (require 'hash-table)
  (require "datastructure.scm")
  (require 'list-lib))

; export
(module-export display-stats)


;;
;; display stats for analysis of stories
;; 

(define (display-stats)
  (format #t "\n******* begin stats *******~%~!")

  (let ((the-nodes (get-list 'nodes))
        (the-links (get-list 'links))
        (the-facts (get-list 'facts))
        (the-rules (get-list 'rules))
        (the-conditions (get-list 'conditions))
        (the-actions (get-list 'actions))
        (total-anywhere 0)
        (total-boolean 0)
        (total-string 0)
        (total-number 0)
        (max-node-rules 0)
        (min-node-rules -1)
        (total-node-rules 0)
        (max-node-conditions 0)
        (min-node-conditions -1)
        (total-node-conditions 0)
        (max-node-actions 0)
        (min-node-actions -1)
        (total-node-actions 0)
        (max-node-linkrules 0)
        (min-node-linkrules -1)
        (total-node-linkrules 0)
        (max-node-linkconditions 0)
        (min-node-linkconditions -1)
        (total-node-linkconditions 0)
        (max-node-linkactions 0)
        (min-node-linkactions -1)
        (total-node-linkactions 0)
        (max-node-destinations 0)
        (min-node-destinations -1)
        (total-node-destinations 0)
        (max-node-destinationscond 0)
        (min-node-destinationscond -1)
        (total-node-destinationscond 0)
        (max-node-destinationsnocond 0)
        (min-node-destinationsnocond -1)
        (total-node-destinationsnocond 0)
        (max-node-entries 0)
        (min-node-entries -1)
        (total-node-entries 0)
        (max-node-factupdates 0)
        (min-node-factupdates -1)
        (total-node-factupdates 0)
        (max-node-factconditions 0)
        (min-node-factconditions -1)
        (total-node-factconditions 0)
        (max-node-alttexttyped 0)
        (min-node-alttexttyped -1)
        (total-node-alttexttyped 0)
        (max-node-alttextfact 0)
        (min-node-alttextfact -1)
        (total-node-alttextfact 0)
        (max-link-destinations 0)
        (min-link-destinations -1)
        (total-link-destinations 0)
        (total-regularnode-links 0)
        (max-fact-updates 0)
        (min-fact-updates -1)
        (total-fact-updates 0)
        (max-combinedrules 0)
        (min-combinedrules -1)
        (max-combinedconditions 0)
        (min-combinedconditions -1)
        (max-combinedactions 0)
        (min-combinedactions -1)
        )

    ; overall stats
    (format #t "*** Overall stats ***~%~!")
    (if the-nodes (format #t "Total nodes\t~a~%~!" (length the-nodes)))
    (if the-links (format #t "Total links\t~a~%~!" (length the-links)))
    (if the-facts (format #t "Total facts\t~a~%~!" (length the-facts)))
    (if the-rules (format #t "Total rules\t~a~%~!" (length the-rules)))
    (if the-conditions (format #t "Total conditions\t~a~%~!" (length the-conditions)))
    (if the-actions (format #t "Total actions\t~a~%~!" (length the-actions)))

    ; go through all the facts
    (if the-facts
        (begin
          ; for each fact, count based on type
          (map (lambda (thisfact)
                 (let* ((thisfact-obj (cdr thisfact))
                        (thisfact-type (ask thisfact-obj 'type))
                        (thisfact-updatecount 0)
                        (thisfact-ID (ask thisfact-obj 'ID)))

                   ; count totals
                   (if (eq? thisfact-type 'boolean) (set! total-boolean (+ total-boolean 1)))
                   (if (eq? thisfact-type 'string) (set! total-string (+ total-string 1)))
                   (if (eq? thisfact-type 'number) (set! total-number (+ total-number 1)))

                   ; count updates
                   (map (lambda (thisaction)
                          (let* ((thisaction-obj (cdr thisaction))
                                 (thisaction-expr (ask thisaction-obj 'expr))
                                 (thisaction-type (car thisaction-expr)) ; action type (follow link, replace link text, etc.)
                                 )
                            (cond
                             ((or (equal? 'retract thisaction-type)
                                  (equal? 'assert thisaction-type)
                                  (equal? 'set-value! thisaction-type))
                              (define targetID (cadr thisaction-expr)) ;; factID
                              (if (= targetID thisfact-ID)
                                  (set! thisfact-updatecount (+ thisfact-updatecount 1))))
                             ((equal? 'set-number-fact thisaction-type)
                              (define targetID (list-ref thisaction-expr 1)) ;; factID
                              (if (= targetID thisfact-ID)
                                  (set! thisfact-updatecount (+ thisfact-updatecount 1)))))))
                        the-actions)

                   ; calculate stats
                   (if (> thisfact-updatecount max-fact-updates) (set! max-fact-updates thisfact-updatecount))
                   (if (or
                        (= -1 min-fact-updates)
                        (< thisfact-updatecount min-fact-updates)) (set! min-fact-updates thisfact-updatecount))
                   (set! total-fact-updates (+ total-fact-updates thisfact-updatecount))))
               the-facts)

          ; display fact stats
          (format #t "Total true/false facts\t~a~%~!" total-boolean)
          (format #t "Total text facts\t~a~%~!" total-string)
          (format #t "Total number facts\t~a~%~!" total-number)
          ))

    ; go through all the nodes
    (if the-nodes
        (begin
          ; for each node, calculate the stats
          (map (lambda (thisnode)
                 (let* ((thisnode-obj (cdr thisnode))
                        (thisnode-ID (ask thisnode-obj 'ID))
                        (thisnode-rules (ask thisnode-obj 'rule-lst))
                        (thisnode-links (ask thisnode-obj 'links))
                        (thisnode-noderulecount (length thisnode-rules))
                        (thisnode-linkrulecount 0)
                        (thisnode-linkconditioncount 0)
                        (thisnode-linkactioncount 0)
                        (thisnode-destinationcount 0)
                        (thisnode-destinationcondcount 0)
                        (thisnode-destinationnocondcount 0)
                        (thisnode-entrycount 0)
                        (thisnode-factupdatecount 0)
                        (thisnode-alttexttypedcount 0)
                        (thisnode-alttextfactcount 0)
                        (thisnode-factconditionscount 0)
                        (thisnode-combinedrulecount 0)
                        (thisnode-combinedconditioncount 0)
                        (thisnode-combinedactioncount 0))

                   ; count anywhere nodes
                   (if (ask thisnode-obj 'anywhere?) (set! total-anywhere (+ total-anywhere 1)))

                   ; max/min/average node rules
                   (if (> thisnode-noderulecount max-node-rules) (set! max-node-rules thisnode-noderulecount))
                   (if (or
                        (= -1 min-node-rules)
                        (< thisnode-noderulecount min-node-rules)) (set! min-node-rules thisnode-noderulecount))
                   (set! total-node-rules (+ total-node-rules thisnode-noderulecount))

                   ; helper for counting various elements of rules
                   (define (count-conditions-and-actions in-rules)
                     (let ((the-conditioncount 0)
                           (the-actioncount 0)
                           (the-destinationcount 0)
                           (the-destinationcondcount 0)
                           (the-destinationnocondcount 0)
                           (the-factupdatecount 0)
                           (the-factconditionscount 0)
                           (the-alttexttypedcount 0)
                           (the-alttextfactcount 0))
                       (map (lambda (thisrule)
                              (let* ((thisrule-obj (get 'rules thisrule))
                                     (thisrule-conditions (ask thisrule-obj 'conditions))
                                     (thisrule-actions (ask thisrule-obj 'actions)))
                                ; check for and count various types of conditions
                                (set! the-conditioncount (+ the-conditioncount (length thisrule-conditions)))
                                (map (lambda (thiscondition)
                                       (let* ((thiscondition-obj (get 'conditions thiscondition))
                                              (thiscondition-type (ask thiscondition-obj 'type)))
                                         (cond
                                          ((eq? thiscondition-type 0)
                                           ;; node
                                           'node)
                                          ((eq? thiscondition-type 1)
                                           ;; link
                                           'link)
                                          ((eq? thiscondition-type 2)
                                           ;; bool fact 
                                           (set! the-factconditionscount (+ the-factconditionscount 1)))
                                          ((eq? thiscondition-type 3)
                                           ;; num fact
                                           (set! the-factconditionscount (+ the-factconditionscount 1))))
                                         ))
                                     thisrule-conditions)

                                ; check for and count various types of actions
                                (set! the-actioncount (+ the-actioncount (length thisrule-actions)))
                                (map (lambda (thisaction)
                                       (let* ((thisaction-obj (get 'actions thisaction))
                                              (thisaction-expr (ask thisaction-obj 'expr))
                                              (thisaction-type (car thisaction-expr)) ; action type (follow link, replace link text, etc.)
                                              )
                                         (cond
                                          ((equal? 'follow-link thisaction-type)
                                           ; count destinations
                                           (set! the-destinationcount (+ the-destinationcount 1))
                                           ; count destinations with conditions and without conditions
                                           (if (> (length thisrule-conditions) 0)
                                               (set! the-destinationcondcount (+ the-destinationcondcount 1))
                                               (set! the-destinationnocondcount (+ the-destinationnocondcount 1))))
                                          ((equal? 'replace-link-text thisaction-type)
                                           ; count typed vs. fact text updates
                                           (if (eq? "alternative text" (list-ref thisaction-expr 1))
                                               (set! the-alttexttypedcount (+ the-alttexttypedcount 1))
                                               (set! the-alttextfactcount (+ the-alttextfactcount 1))))
                                          ((or (equal? 'retract thisaction-type)
                                               (equal? 'assert thisaction-type)
                                               (equal? 'set-value! thisaction-type))
                                           ; count fact updates
                                           (set! the-factupdatecount (+ the-factupdatecount 1)))
                                          ((equal? 'set-number-fact thisaction-type)
                                           ; count fact updates
                                           (set! the-factupdatecount (+ the-factupdatecount 1)))
                                          ((equal? 'add-anywhere-link thisaction-type)
                                           'add-anywhere-link)
                                          ((equal? 'show-in-popup thisaction-type)
                                           ; what about popups?
                                           'show-in-popup)
                                          )))
                                     thisrule-actions)))
                            in-rules)
                       (values the-conditioncount the-actioncount the-destinationcount the-destinationcondcount the-destinationnocondcount
                               the-factupdatecount the-alttexttypedcount the-alttextfactcount the-factconditionscount)))

                   ; max/min/average node conditions and actions 
                   ; Note: thisnode-destinationcount, thisnode-destinationcondcount, thisnode-destinationnocondcount, thisnode-alttexttypedcount and thisnode-alttextfactcount 
                   ; are ignored here as they are irrelevant
                   (let-values (((thisnode-nodeconditioncount thisnode-nodeactioncount thisnode-destinationcount
                                                              thisnode-destinationcondcount thisnode-destinationnocondcount
                                                              thisnode-nodefactupdatecount thisnode-alttexttypedcount
                                                              thisnode-alttextfactcount thisnode-nodefactconditionscount)
                                 (count-conditions-and-actions thisnode-rules)))
                     (begin
                       ; conditions
                       (if (> thisnode-nodeconditioncount max-node-conditions) (set! max-node-conditions thisnode-nodeconditioncount))
                       (if (or
                            (= -1 min-node-conditions)
                            (< thisnode-nodeconditioncount min-node-conditions)) (set! min-node-conditions thisnode-nodeconditioncount))
                       (set! total-node-conditions (+ total-node-conditions thisnode-nodeconditioncount))
                       ; actions
                       (if (> thisnode-nodeactioncount max-node-actions) (set! max-node-actions thisnode-nodeactioncount))
                       (if (or
                            (= -1 min-node-actions)
                            (< thisnode-nodeactioncount min-node-actions)) (set! min-node-actions thisnode-nodeactioncount))
                       (set! total-node-actions (+ total-node-actions thisnode-nodeactioncount))
                       ; fact updates - note: just combine node rule and link rule count for now
                       (set! thisnode-factupdatecount thisnode-nodefactupdatecount)
                       ; fact-based conditions - note: just combine node rule and link rule count for now
                       (set! thisnode-factconditionscount thisnode-nodefactconditionscount)
                       ; combined rule count
                       (set! thisnode-combinedrulecount thisnode-noderulecount)
                       ; combined condition count
                       (set! thisnode-combinedconditioncount thisnode-nodeconditioncount)
                       ; combined action count
                       (set! thisnode-combinedactioncount thisnode-nodeactioncount)
                       ))

                   ; for each link in the node
                   (map (lambda (thislink)
                          (let* ((thislink-obj (get 'links thislink))
                                 (thislink-rules (ask thislink-obj 'rule-lst)))
                            ; max/min/total link rules
                            (set! thisnode-linkrulecount (+ thisnode-linkrulecount (length thislink-rules)))
                            ; max/min/total link conditions and actions
                            (let-values (((thislink-conditioncount thislink-actioncount thislink-destinationcount
                                                                   thislink-destinationcondcount thislink-destinationnocondcount
                                                                   thislink-factupdatecount
                                                                   thislink-alttexttypedcount thislink-alttextfactcount
                                                                   thislink-factconditionscount)
                                          (count-conditions-and-actions thislink-rules)))
                              ; counts
                              (set! thisnode-linkconditioncount (+ thisnode-linkconditioncount thislink-conditioncount))
                              (set! thisnode-linkactioncount (+ thisnode-linkactioncount thislink-actioncount))
                              (set! thisnode-destinationcount (+ thisnode-destinationcount thislink-destinationcount))
                              (set! thisnode-destinationcondcount (+ thisnode-destinationcondcount thislink-destinationcondcount))
                              (set! thisnode-destinationnocondcount (+ thisnode-destinationnocondcount thislink-destinationnocondcount))
                              (set! thisnode-factupdatecount (+ thisnode-factupdatecount thislink-factupdatecount))
                              (set! thisnode-alttexttypedcount (+ thisnode-alttexttypedcount thislink-alttexttypedcount))
                              (set! thisnode-alttextfactcount (+ thisnode-alttextfactcount thislink-alttextfactcount))
                              (set! thisnode-factconditionscount (+ thisnode-factconditionscount thislink-factconditionscount))
                              ; max/min/total link destinations (only for regular nodes)
                              (if (not (ask thisnode-obj 'anywhere?))
                                  (begin
                                    (set! total-regularnode-links (+ total-regularnode-links 1))
                                    (if (> thislink-destinationcount max-link-destinations) (set! max-link-destinations thislink-destinationcount))
                                    (if (or
                                         (= -1 min-link-destinations)
                                         (< thislink-destinationcount min-link-destinations)) (set! min-link-destinations thislink-destinationcount))
                                    (set! total-link-destinations (+ total-link-destinations thislink-destinationcount))
                                    )))))
                        thisnode-links)

                   ; count entry points
                   (map (lambda (thislink)
                          (let* ((thislink-obj (cdr thislink))
                                 (thislink-nodeID (ask thislink-obj 'source))
                                 (thislink-rules (ask thislink-obj 'rule-lst)))
                            (if (not (= thisnode-ID thislink-nodeID))
                                (map (lambda (thisrule)
                                       (let* ((thisrule-obj (get 'rules thisrule))
                                              (thisrule-actions (ask thisrule-obj 'actions)))
                                         (map (lambda (thisaction)
                                                (let* ((thisaction-obj (get 'actions thisaction))
                                                       (thisaction-expr (ask thisaction-obj 'expr))
                                                       (thisaction-type (car thisaction-expr)))
                                                  (if (equal? thisaction-type 'follow-link)
                                                      (let ((thisaction-destID (list-ref thisaction-expr 4)))
                                                        (if (equal? thisaction-destID thisnode-ID)
                                                            (set! thisnode-entrycount (+ thisnode-entrycount 1)))))))
                                              thisrule-actions)))
                                     thislink-rules))))
                        the-links)

                   ; now tabulate the link stats for this node - maybe should track the IDs of max nodes/links?
                   ; rules
                   (if (> thisnode-linkrulecount max-node-linkrules) (set! max-node-linkrules thisnode-linkrulecount))
                   (if (or
                        (= -1 min-node-linkrules)
                        (< thisnode-linkrulecount min-node-linkrules)) (set! min-node-linkrules thisnode-linkrulecount))
                   (set! total-node-linkrules (+ total-node-linkrules thisnode-linkrulecount))
                   ; conditions
                   (if (> thisnode-linkconditioncount max-node-linkconditions) (set! max-node-linkconditions thisnode-linkconditioncount))
                   (if (or
                        (= -1 min-node-linkconditions)
                        (< thisnode-linkconditioncount min-node-linkconditions)) (set! min-node-linkconditions thisnode-linkconditioncount))
                   (set! total-node-linkconditions (+ total-node-linkconditions thisnode-linkconditioncount))
                   ; actions
                   (if (> thisnode-linkactioncount max-node-linkactions) (set! max-node-linkactions thisnode-linkactioncount))
                   (if (or
                        (= -1 min-node-linkactions)
                        (< thisnode-linkactioncount min-node-linkactions)) (set! min-node-linkactions thisnode-linkactioncount))
                   (set! total-node-linkactions (+ total-node-linkactions thisnode-linkactioncount))
                   ; destinations
                   (if (> thisnode-destinationcount max-node-destinations) (set! max-node-destinations thisnode-destinationcount))
                   (if (or
                        (= -1 min-node-destinations)
                        (< thisnode-destinationcount min-node-destinations)) (set! min-node-destinations thisnode-destinationcount))
                   (set! total-node-destinations (+ total-node-destinations thisnode-destinationcount))
                   ; destinations with conditions
                   (if (> thisnode-destinationcondcount max-node-destinationscond) (set! max-node-destinationscond thisnode-destinationcondcount))
                   (if (or
                        (= -1 min-node-destinationscond)
                        (< thisnode-destinationcondcount min-node-destinationscond)) (set! min-node-destinationscond thisnode-destinationcondcount))
                   (set! total-node-destinationscond (+ total-node-destinationscond thisnode-destinationcondcount))
                   ; destinations with no conditions
                   (if (> thisnode-destinationnocondcount max-node-destinationsnocond) (set! max-node-destinationsnocond thisnode-destinationnocondcount))
                   (if (or
                        (= -1 min-node-destinationsnocond)
                        (< thisnode-destinationnocondcount min-node-destinationsnocond)) (set! min-node-destinationsnocond thisnode-destinationnocondcount))
                   (set! total-node-destinationsnocond (+ total-node-destinationsnocond thisnode-destinationnocondcount))
                   ; entries
                   (if (> thisnode-entrycount max-node-entries) (set! max-node-entries thisnode-entrycount))
                   (if (or
                        (= -1 min-node-entries)
                        (< thisnode-entrycount min-node-entries)) (set! min-node-entries thisnode-entrycount))
                   (set! total-node-entries (+ total-node-entries thisnode-entrycount))
                   ; fact updates
                   (if (> thisnode-factupdatecount max-node-factupdates) (set! max-node-factupdates thisnode-factupdatecount))
                   (if (or
                        (= -1 min-node-factupdates)
                        (< thisnode-factupdatecount min-node-factupdates)) (set! min-node-factupdates thisnode-factupdatecount))
                   (set! total-node-factupdates (+ total-node-factupdates thisnode-factupdatecount))
                   ; text updates (typed)
                   (if (> thisnode-alttexttypedcount max-node-alttexttyped) (set! max-node-alttexttyped thisnode-alttexttypedcount))
                   (if (or
                        (= -1 min-node-alttexttyped)
                        (< thisnode-alttexttypedcount min-node-alttexttyped)) (set! min-node-alttexttyped thisnode-alttexttypedcount))
                   (set! total-node-alttexttyped (+ total-node-alttexttyped thisnode-alttexttypedcount))
                   ; text updates (fact)
                   (if (> thisnode-alttextfactcount max-node-alttextfact) (set! max-node-alttextfact thisnode-alttextfactcount))
                   (if (or
                        (= -1 min-node-alttextfact)
                        (< thisnode-alttextfactcount min-node-alttextfact)) (set! min-node-alttextfact thisnode-alttextfactcount))
                   (set! total-node-alttextfact (+ total-node-alttextfact thisnode-alttextfactcount))
                   ; fact-based conditions
                   (if (> thisnode-factconditionscount max-node-factconditions) (set! max-node-factconditions thisnode-factconditionscount))
                   (if (or
                        (= -1 min-node-factconditions)
                        (< thisnode-factconditionscount min-node-factconditions)) (set! min-node-factconditions thisnode-factconditionscount))
                   (set! total-node-factconditions (+ total-node-factconditions thisnode-factconditionscount))
                   ; combined rules
                   (set! thinode-combinedrulecount (+ thisnode-combinedrulecount thisnode-linkrulecount))
                   (if (> thinode-combinedrulecount max-combinedrules) (set! max-combinedrules thinode-combinedrulecount))
                   (if (or
                        (= -1 min-combinedrules)
                        (< thinode-combinedrulecount min-combinedrules)) (set! min-combinedrules thinode-combinedrulecount))
                   ; combined conditions
                   (set! thinode-combinedconditioncount (+ thisnode-combinedconditioncount thisnode-linkconditioncount))
                   (if (> thinode-combinedconditioncount max-combinedconditions) (set! max-combinedconditions thinode-combinedconditioncount))
                   (if (or
                        (= -1 min-combinedconditions)
                        (< thinode-combinedconditioncount min-combinedconditions)) (set! min-combinedconditions thinode-combinedconditioncount))
                   ; combined actions
                   (set! thinode-combinedactioncount (+ thisnode-combinedactioncount thisnode-linkactioncount))
                   (if (> thinode-combinedactioncount max-combinedactions) (set! max-combinedactions thinode-combinedactioncount))
                   (if (or
                        (= -1 min-combinedactions)
                        (< thinode-combinedactioncount min-combinedactions)) (set! min-combinedactions thinode-combinedactioncount))
                   ))
               the-nodes)

          ; display node stats
          (format #t "Total anywhere nodes\t~a~%~!" total-anywhere)
          (format #t "Total regular nodes\t~a~%~!" (- (length the-nodes) total-anywhere))
          (format #t "Total node rules\t~a~%~!" total-node-rules)
          (if the-rules (format #t "Total link rules\t~a~%~!" (- (length the-rules) total-node-rules)))
          (format #t "Total fact-based conditions\t~a~%~!" total-node-factconditions)
          (format #t "Total fact updates\t~a~%~!" total-node-factupdates)
          (format #t "Total alt text (typed)\t~a~%~!" total-node-alttexttyped)
          (format #t "Total alt text (facts)\t~a~%~!" total-node-alttextfact)
          
          ; feature usage
          (format #t "\n*** Feature usage ***~%~!")
          (format #t "Max node rules per node\t~a~%~!Min node rules per node\t~a~%~!Average node rules per node\t~a~%~!"
                  max-node-rules (min min-node-rules 0) (if (> (length the-nodes) 0)
                                                            (/ (round (* (exact->inexact (/ total-node-rules (length the-nodes))) 100)) 100)
                                                            "N/A"))
          (format #t "Max node conditions per node\t~a~%~!Min node conditions per node\t~a~%~!Average node conditions per node\t~a~%~!"
                  max-node-conditions (min min-node-conditions 0) (if (> (length the-nodes) 0)
                                                                      (/ (round (* (exact->inexact (/ total-node-conditions (length the-nodes))) 100)) 100)
                                                                      "N/A"))
          (format #t "Max node actions per node\t~a~%~!Min node actions per node\t~a~%~!Average node actions per node\t~a~%~!"
                  max-node-actions (min min-node-actions 0) (if (> (length the-nodes) 0)
                                                                (/ (round (* (exact->inexact (/ total-node-actions (length the-nodes))) 100)) 100)
                                                                "N/A"))
          (format #t "Max link rules per node\t~a~%~!Min link rules per node\t~a~%~!Average link rules per node\t~a~%~!"
                  max-node-linkrules (min min-node-linkrules 0) (if (> (length the-nodes) 0)
                                                                    (/ (round (* (exact->inexact (/ total-node-linkrules (length the-nodes))) 100)) 100)
                                                                    "N/A"))
          (format #t "Max link conditions per node\t~a~%~!Min link conditions per node\t~a~%~!Average link conditions per node\t~a~%~!"
                  max-node-linkconditions (min min-node-linkconditions 0) (if (> (length the-nodes) 0)
                                                                              (/ (round (* (exact->inexact (/ total-node-linkconditions (length the-nodes))) 100)) 100)
                                                                              "N/A"))
          (format #t "Max link actions per node\t~a~%~!Min link actions per node\t~a~%~!Average link actions per node\t~a~%~!"
                  max-node-linkactions (min min-node-linkactions 0) (if (> (length the-nodes) 0)
                                                                        (/ (round (* (exact->inexact (/ total-node-linkactions (length the-nodes))) 100)) 100)
                                                                        "N/A"))
          (format #t "Max destinations per node\t~a~%~!Min destinations per node\t~a~%~!Average destinations per node (regular nodes only)\t~a~%~!"
                  max-node-destinations (min min-node-destinations 0) (if (> (- (length the-nodes) total-anywhere) 0)
                                                                          (/ (round (* (exact->inexact (/ total-node-destinations (- (length the-nodes) total-anywhere))) 100)) 100)
                                                                          "N/A"))
          (format #t "Max entries per node\t~a~%~!Min entries per node\t~a~%~!Average entries per node (regular nodes only)\t~a~%~!"
                  max-node-entries (min min-node-entries 0) (if (> (- (length the-nodes) total-anywhere) 0)
                                                                (/ (round (* (exact->inexact (/ total-node-entries (- (length the-nodes) total-anywhere))) 100)) 100)
                                                                "N/A"))
          (format #t "Max rules per node\t~a~%~!Min rules per node\t~a~%~!Average rules per node (disregard node/link distinction)\t~a~%~!"
                  max-combinedrules (min min-combinedrules 0) (if (and (> (length the-nodes) 0) the-rules)
                                                                (/ (round (* (exact->inexact (/ (length the-rules) (length the-nodes))) 100)) 100)
                                                                "N/A"))
          (format #t "Max conditions per node\t~a~%~!Min conditions per node\t~a~%~!Average conditions per node (disregard node/link distinction)\t~a~%~!"
                  max-combinedconditions (min min-combinedconditions 0) (if (and (> (length the-nodes) 0) the-conditions)
                                                                (/ (round (* (exact->inexact (/ (length the-conditions) (length the-nodes))) 100)) 100)
                                                                "N/A"))
          (format #t "Max actions per node\t~a~%~!Min actions per node\t~a~%~!Average actions per node (disregard node/link distinction)\t~a~%~!"
                  max-combinedactions (min min-combinedactions 0) (if (and (> (length the-nodes) 0) the-actions)
                                                                (/ (round (* (exact->inexact (/ (length the-actions) (length the-nodes))) 100)) 100)
                                                                "N/A"))
          
          ; procedurality
          (format #t "\n*** Procedurality ***~%~!")
          (format #t "Max fact updates per node\t~a~%~!Min fact updates per node\t~a~%~!Average fact updates per node\t~a~%~!"
                  max-node-factupdates (min min-node-factupdates 0) (if (> (length the-nodes) 0)
                                                                        (/ (round (* (exact->inexact (/ total-node-factupdates (length the-nodes))) 100)) 100)
                                                                        "N/A"))
          (format #t "Max alt text (typed) replacements per node\t~a~%~!Min alt text (typed) replacements per node\t~a~%~!Average alt text (typed) replacements per node\t~a~%~!"
                  max-node-alttexttyped (min min-node-alttexttyped 0) (if (> (length the-nodes) 0)
                                                                          (/ (round (* (exact->inexact (/ total-node-alttexttyped (length the-nodes))) 100)) 100)
                                                                          "N/A"))
          (format #t "Max alt text (fact) replacements per node\t~a~%~!Min alt text (fact) replacements per node\t~a~%~!Average alt text (fact) replacements per node\t~a~%~!"
                  max-node-alttextfact (min min-node-alttextfact 0) (if (> (length the-nodes) 0)
                                                                        (/ (round (* (exact->inexact (/ total-node-alttextfact (length the-nodes))) 100)) 100)
                                                                        "N/A"))
          (format #t "Max fact-based conditions per node\t~a~%~!Min fact-based conditions per node\t~a~%~!Average fact-based conditions per node\t~a~%~!"
                  max-node-factconditions (min min-node-factconditions 0) (if (> (length the-nodes) 0)
                                                                              (/ (round (* (exact->inexact (/ total-node-factconditions (length the-nodes))) 100)) 100)
                                                                              "N/A"))
          (format #t "Max destinations with conditions per node\t~a~%~!Min destinations with conditions per node\t~a~%~!Average destinations with conditions per node (regular nodes only)\t~a~%~!"
                  max-node-destinationscond (min min-node-destinationscond 0) (if (> (- (length the-nodes) total-anywhere) 0)
                                                                                  (/ (round (* (exact->inexact (/ total-node-destinationscond (- (length the-nodes) total-anywhere))) 100)) 100)
                                                                                  "N/A"))
          (format #t "Max destinations with no conditions per node\t~a~%~!Min destinations with no conditions per node\t~a~%~!Average destinations with no conditions per node (regular nodes only)\t~a~%~!"
                  max-node-destinationsnocond (min min-node-destinationsnocond 0) (if (> (- (length the-nodes) total-anywhere) 0)
                                                                                      (/ (round (* (exact->inexact (/ total-node-destinationsnocond (- (length the-nodes) total-anywhere))) 100)) 100)
                                                                                      "N/A"))
          (format #t "Max destinations per link\t~a~%~!Min destinations per link\t~a~%~!Average destinations per link (regular nodes only)\t~a~%~!"
                  max-link-destinations (min min-link-destinations 0) (if (> total-regularnode-links 0)
                                                                          (/ (round (* (exact->inexact (/ total-link-destinations total-regularnode-links)) 100)) 100)
                                                                          "N/A"))
          ))

    (if the-facts
        (format #t "Max updates per fact\t~a~%~!Min updates per fact\t~a~%~!Average updates per fact\t~a~%~!"
                max-fact-updates (min min-fact-updates 0) (if (> (length the-facts) 0)
                                                              (/ (round (* (exact->inexact (/ total-fact-updates (length the-facts))) 100)) 100)
                                                              "N/A")))

    (format #t "******* end stats *******~%~!")
    ))