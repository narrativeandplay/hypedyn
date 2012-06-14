;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2012
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

; simple language for conditions in if-hyper
(begin
  (require "../common/evaluator.scm")
  (require "../kawa/random.scm")
  (require "config-options.scm")
  (require "datastructure.scm")
  (require "reader.scm")
  (require "reader-pane.scm"))

; export
(module-export add-hypedyn-primitive-procedures add-htlanguage)

; add the hypedyn language
(define (add-htlanguage)
  ; use the minischeme language
  (add-minischeme-primitive-procedures)

  ; and add hypedyn-specific primitives
  (add-hypedyn-primitive-procedures))

; add basic scheme primitives
(define (add-minischeme-primitive-procedures)
  (primitive-procedure-add! 'car car)
  (primitive-procedure-add! 'cdr cdr)
  (primitive-procedure-add! 'cons cons )
  (primitive-procedure-add! 'list list )
  (primitive-procedure-add! 'null?  null?)
  (primitive-procedure-add! 'not not)
  (primitive-procedure-add! '=  =)
  (primitive-procedure-add! '>  >)
  (primitive-procedure-add! '< <)
  (primitive-procedure-add! '>= >=)
  (primitive-procedure-add! '<= <=)
  (primitive-procedure-add! 'eq? eq?)
  (primitive-procedure-add! 'equal? equal?)

  ;; added math operators
  (primitive-procedure-add! '+ +)
  (primitive-procedure-add! '- -)
  (primitive-procedure-add! '* *)
  (primitive-procedure-add! '/ /)
  (primitive-procedure-add! '^ expt))

; add primitives
(define (add-hypedyn-primitive-procedures)
  ; creating the data structure, for loading/saving
  (primitive-procedure-add! 'xml make-hypertext)
  (primitive-procedure-add! 'make-hypertext make-hypertext)
  (primitive-procedure-add! 'create-node create-node)                                ; (create-node content x y anywhere update-display . args): create a new node, args is optional nodeID
  (primitive-procedure-add! 'create-link create-link)                                ; (create-link name fromnodeID tonodeID start-index end-index use-destination use-alt-destination use-alt-text alt-destination alt-text update-display . args)
  (primitive-procedure-add! 'create-rule create-rule)                                ; (create-rule name expression linkID . args)
  (primitive-procedure-add! 'create-typed-rule create-typed-rule)                    ; (create-typed-rule name type expression parentID . args)
  (primitive-procedure-add! 'create-typed-rule2 create-typed-rule2)                  ; (create-typed-rule2 name type and-or negate? parentID . args)
  (primitive-procedure-add! 'create-typed-rule3 create-typed-rule3)                  ; (create-typed-rule3 name type and-or negate? parentID #!key fixedID fall-through?)
  (primitive-procedure-add! 'create-condition create-condition)                      ; (create-condition name nodeID operator ruleID . args)
  (primitive-procedure-add! 'create-typed-condition create-typed-condition)          ; (create-typed-condition name type targetID operator ruleID #!optional fixedID)
  (primitive-procedure-add! 'create-typed-condition2 create-typed-condition2)
  (primitive-procedure-add! 'create-action create-action)                            ; (create-action name type expr ruleID . args)
  (primitive-procedure-add! 'create-fact create-fact)                                ; (create-fact name type . args)
  (primitive-procedure-add! 'set-start-node! set-start-node!)                        ; (set-start-node! nodeID)
  (primitive-procedure-add! 'set-basic-mode! set-basic-mode!)                        ; (set-basic-mode! #t/#f)
  (primitive-procedure-add! 'set-card-shark! set-card-shark!)                        ; (set-card-shark! #t/#f)
  (primitive-procedure-add! 'set-user-study! set-user-study!)                        ; (set-user-study! #t/#f)
  
  ; used by actions
  (primitive-procedure-add! 'restart dorestart)                                      ; (restart): restart reading, same as restart button
  (primitive-procedure-add! 'read-file doreaderopen-filename)                        ; (read-file "filename.dyn"): open the given file and jump to its start node
  (primitive-procedure-add! 'goto-node goto-node)                                    ; (goto-node nodeID #t/#f): jump to a node, flag determines if added to history
  (primitive-procedure-add! 'back doback)                                            ; (back): go to previous node, same as back button
  (primitive-procedure-add! 'forget-history forget-history)                          ; (forget-history): clears all history
  (primitive-procedure-add! 'set-visited! set-visited!)                              ; (set-visited! nodeID int): sets visited count for a node
  (primitive-procedure-add! 'set-followed! set-followed!)                            ; (set-followed! linkID int): sets followed count for a link
  (primitive-procedure-add! 'assert assert)                                          ; (assert factID): sets a true/false fact to #t
  (primitive-procedure-add! 'retract retract)                                        ; (retract factID): sets a true/false fact to #f
  (primitive-procedure-add! 'set-value! set-fact-value!)                             ; (set-value! factID "string"): sets value of a text fact
  (primitive-procedure-add! 'set-number-fact set-fact-value!)
  (primitive-procedure-add! 'set-link-type! set-link-type!)                          ; (set-link-type! linkID 'symbol): sets link type: 'hover or 'default, hover links show dest text if hover links turned on
  (primitive-procedure-add! 'set-custom-cursor-image! set-custom-cursor-image!)      ; (set-custom-cursor-image! linkID "filename.gif"): sets image for given link, shows on roll-over if custom cursors turned on
  (primitive-procedure-add! 'set-user-data! set-user-data!)                          ; (set-link-type! linkID data): sets user data for a link
  (primitive-procedure-add! 'set-available-node-count! set-available-node-count!)    ; (set-available-node-count! nodeID int): sets the current available node count, displayed in remaining node counter
  (primitive-procedure-add! 'get-available-node-count get-available-node-count)      ; (get-available-node-count nodeID): gets the current available node count
  (primitive-procedure-add! 'set-background-color! set-reader-background-color!)     ; (set-background-color! (list r g b a)): sets background colour
  (primitive-procedure-add! 'set-background-image! set-reader-background-image!)     ; (set-background-image! "filename.jpg"): sets background image
  (primitive-procedure-add! 'clear-background-image! clear-reader-background-image!) ; (clear-background-image!): clears background image
  (primitive-procedure-add! 'play-audio play-audio)                                  ; (play-audio "audio.wav") * check formats *
  (primitive-procedure-add! 'loop-audio loop-audio)                                  ; (loop-audio int): loops audio int times
  (primitive-procedure-add! 'loop-audio-forever loop-audio-forever)                  ; (loop-audio-forever): loops audio until stopped
  (primitive-procedure-add! 'stop-audio stop-audio)                                  ; (stop-audio): stops audio immediately
  (primitive-procedure-add! 'set-disable-back-button! set-disable-back-button!)      ; (set-disable-back-button! #t/#f): enable/disable back button
  (primitive-procedure-add! 'set-disable-restart-button! set-disable-restart-button!) ; (set-disable-restart-button! #t/#f): enable/disable restart button
  (primitive-procedure-add! 'set-custom-cursors! set-custom-cursors!)                ; (set-custom-cursors! #t/#f): turn custom cursors on/off
  (primitive-procedure-add! 'set-hover-links! set-hover-links!)                      ; (set-hover-links! #t/#f): turn hover links on/off
  (primitive-procedure-add! 'set-hover-click! set-hover-click!)                      ; (set-hover-clicks! #t/#f): turn hover clicks on/off
  (primitive-procedure-add! 'toggle-show-node-counter toggle-show-node-counter)      ; (toggle-show-node-counter): toggle show/hide remaining node counter
  
  ; used by conditions
  (primitive-procedure-add! 'visited? visited?)                                      ; (visited? nodeID): returns #t/#t if node visited
  (primitive-procedure-add! 'previous? previous?)                                    ; (previous? nodeID): returns #t/#f if node is prev
  (primitive-procedure-add! 'followed? followed?)                                    ; (followed? linkID): returns #t/#f if link followed
  (primitive-procedure-add! 'holds? holds?)                                          ; (holds? factID): returns #t/#f if fact holds
  (primitive-procedure-add! 'get-value get-fact-value)                               ; (get-value factID): returns a text fact's value
  (primitive-procedure-add! 'get-link-type get-link-type)                            ; (get-link-type linkID): returns link type: 'hover or 'default
  (primitive-procedure-add! 'get-custom-cursor-image get-custom-cursor-image)        ; (get-custom-cursor-image): returns current image filenane
  (primitive-procedure-add! 'get-user-data get-user-data)                            ; (get-user-data linkID): returns arbitrary user data stored in a link
  (primitive-procedure-add! 'get-step-count get-step-count)                          ; (get-step-count): gets current step count
  (primitive-procedure-add! 'get-current-nodeID get-read-nodeID)                     ; (get-current-nodeID): returns current nodeID
  
  ;; rule actions
  (primitive-procedure-add! 'follow-link follow-link2)
  (primitive-procedure-add! 'replace-link-text replace-link-text)
  (primitive-procedure-add! 'goto-node goto-node)
  (primitive-procedure-add! 'add-anywhere-link add-anywhere-link)
  (primitive-procedure-add! 'show-in-popup show-in-popup)
  
  ;; config options
  (primitive-procedure-add! 'set-disable-restart-button! set-disable-restart-button!)
  (primitive-procedure-add! 'set-disable-back-button! set-disable-back-button!)
  (primitive-procedure-add! 'set-disable-pagebreak! set-disable-pagebreak!)
  (primitive-procedure-add! 'set-disable-page-resize! set-disable-page-resize!)
  (primitive-procedure-add! 'set-fixed-page-width! set-fixed-page-width!)
  (primitive-procedure-add! 'set-fixed-page-height! set-fixed-page-height!)
  (primitive-procedure-add! 'set-css-type! set-css-type!)
  (primitive-procedure-add! 'set-custom-css-location! set-custom-css-location!)
  (primitive-procedure-add! 'set-custom-css-location2! set-custom-css-location2!)
  
  ; some additional useful primitives
  (primitive-procedure-add! 'random generate-random-int)
  
  ; debugging
  (primitive-procedure-add! 'display display)
  
  ; not sure if this is necessary
  (primitive-procedure-add! 'null '()))

; make a new hypertext
(define (make-hypertext in-filetype in-fileversion)
  (display "make-hypertext: ")(display in-filetype)(display ",")(display in-fileversion)(newline))

; end of primitives
