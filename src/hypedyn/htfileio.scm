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
;; if-hyper-specific fileio code
;;

(begin
  (require "config-options.scm")
  (require "datastructure.scm")
  (require "hteditor.scm")
  (require "nodeeditor.scm")
  (require "../common/objects.scm")
  (require "../common/datatable.scm") ;; get
  (require "../common/fileio.scm")
  (require "../common/main-ui.scm") ;; set-runstate
  (require "../kawa/file.scm")
  (require "../kawa/system.scm")
  (require "../kawa/strings.scm")
  (require "../kawa/miscutils.scm") ;; easy-try-catch, custom-try-catch
  (require "../kawa/ui/dialog.scm")
  (require "../kawa/ui/cursor.scm")
  (require "../common/main-ui.scm")
  (require "export.scm")
  (require "editlink.scm") ;; obj-convertion-2.2
  (require 'list-lib) ;; list-ref
  )

; export
(module-export set-ht-build-sexpr-callback!
               donew doopen open-file-by-name 
               doimport doexport-hypedyn-web doexport-standalone doexport-text
               dosave-wrapper confirm-save ht-save-to-file
               ht-build-sexpr-from-object-with-rule ht-build-sexpr-from-rule
               clear-loaded-file-version ;; used by clear-data in hteditor.scm
               loaded-file-version obj-convertion-2.2
               )

; set fileformat version and type
; Note: file format version numbers only use the major and minor rev numbers; if you change the file format, be sure to
; increment at least the minor rev number (the third sequence is for bug fixes or small changes that don't affect file format)
; Note: hypedyn files are marked as type 'htfe (HyperTextFiction Editor, the original name of the app way back)
(set-fileformat-version! 2.2) 
(set-fileformat-type! 'htfe) 

;; for keep track of a loaded old version file
(define loaded-file-version 1)
(define (clear-loaded-file-version)
  (set! loaded-file-version (get-fileformat-version)))

; new
(define (donew)
  ; first check if we should save
  (if (confirm-save)  ;; check if data has changed
      ; safe, so proceed
      (begin
        ; close any open editor/reader windows
        (close-hteditor-subwindows)

        ; reset everything
        (clear-data)
        (clear-display)

        ; special init for sculptural mode
        (check-init-sculptural))))

; open: optional arg for filename, if no arg passed then show file dialogue
(define (doopen . args)
  (if
      ; check if data has changed
      (confirm-save)
      ; safe, so proceed
      (let ((newfilename (if (pair? args) (car args) (get-file-to-open (get-last-saved-dir) #f (list ".dyn")))))
        (open-file-by-name newfilename)
        (set-runstate #f))))

; actually open the file given a path, used by doopen and doopenrecent
(define (open-file-by-name newfilename)
  ; show wait cursor
  (set-cursor-type (get-main-ui-frame) 'wait)
  
  ; open the file
  (try-catch
     (if (not (eq? #f newfilename))
         (begin
           ; close any open editor/reader windows
           (close-hteditor-subwindows)

           ; clear previous data
           (clear-data)

           ; clear the UI
           (clear-display)

           ;; the version of hypedyn that saved this file
           (define file-version-number 1)
           (define file-type #f)
           (define (get-version-number lst)
             (if (and (pair? lst)
                      (equal? (car lst) 'make-hypertext))
                 (begin
                   (display "[make ht found!] ")(display lst)(newline)
                   (set! file-version-number (list-ref lst 2))
                   (set! file-type (list-ref (list-ref lst 1) 1))
                   (display "file-version num ")(display file-version-number)(newline)
                   (display "file type ")(display file-type)(newline)
                   ))
             )
           (parse-sexpr-file newfilename get-version-number)
           
           ;; check loading conditions
           (cond ((not (equal? file-type (get-fileformat-type))) ;; file format does not match htfe
                  ;; error dialog
                  (make-error-dialog (get-main-ui-frame)
                                     "Hypedyn Open Error"
                                     (string-append
                                      "Error: this is not a HypeDyn file"
                                      (if (symbol? file-type)
                                          (string-append " (type: " (symbol->string file-type) ").")
                                          "."))))
                 ;; opening file saved in older versions of hypedyn (or same version if equal)
                 ((>= (get-fileformat-version) file-version-number) 
                  
                  (define open-choice #f)
                  (define diff-version? (not (= (get-fileformat-version) file-version-number)))
                  
                  ;; newer app opening older file
                  ;; warn here when opening 
                  (if diff-version?
                      ;(make-confirm-dialogbox #!null 1 "Sorry, no start node defined.")
                      (begin
                        (set! open-choice (make-confirm-dialogbox
                                           (get-main-ui-frame)
                                           4
                                           (string-append
                                            "Warning: Opening a file saved in an older version ("
                                            (to-string file-version-number)
                                            ").\nFile will be saved as version "
                                            (to-string (get-fileformat-version))
                                            ".")
                                           ))
                        )
                      )
                  
                  ;; user decides to open the file anyway 
                  ;; or it is of the same version 
                  (if (or (and diff-version?
                               (equal? open-choice 1))
                          (not diff-version?))
                      (begin
                        
                        (set! loaded-file-version file-version-number) 
                        
                        ; load from file
                        (if (load-from-file newfilename)
                            (begin
                              (add-recent-file newfilename)  ;; add to recent menu
                              (obj-convertion-2.2)           ;; if loading pre 2.2 objects convert to post 2.2 format
                              (populate-display)             ;; populate the display (important to convert first)
                              ))
                        )))
                 ;; older app opening newer file
                 ((< (get-fileformat-version) file-version-number)
                  ;; error dialog
                  (make-error-dialog (get-main-ui-frame)
                                     "Hypedyn Open Error"
                                     (string-append
                                      "Error: Trying to open a file saved in a newer version ("
                                      (to-string file-version-number)
                                      ").\nPlease try again using HypeDyn version "
                                      (to-string file-version-number)
                                      " or later.")
                                     )
                  ))
           ))
   (ex <java.lang.Throwable>
   ;; extra to do after exception
;     (make-message-box (get-main-ui-frame)
;                       (*:toString ex)
;                       "")
       
    ;(display (*:toString ex))(newline)
    (*:printStackTrace ex)
     ))
  
  ; reset cursor
  (set-cursor-type (get-main-ui-frame) 'default))

; import into existing file
(define (doimport)
  (easy-try-catch
   (lambda ()
      (let ((newfilename (get-file-to-open (get-last-saved-dir) #f (list ".dyn"))))
        (if (not (eq? #f newfilename))
            (let-values (((max-x max-y max-anywhere-x max-anywhere-y) (get-max-node-positions)))
              ; set import offsets (uniqueID and positions)
              (set-import-offsets! max-x max-y
                                   max-anywhere-x max-anywhere-y)

              ; clear the UI
              (clear-display)

              ; import from file
              (if (import-from-file newfilename)
                  (begin
                    ; populate the display
                    (populate-display)))))))))

; export to web (applet)
(define (doexport-hypedyn-web)
  (export-web export-standalone-folder
              "htapplet.jar"
              "htapplet.html"
              export-hypedyn-web-callback))

; callback for saving to web
(define (export-hypedyn-web-callback in-export-folder in-source-folder)
  (let ((export-file (path (string-append
                            in-export-folder
                            "/story.dyn"))))
    (display "ARGH export-file ")(display export-file)(newline)
    (ht-save-to-file export-file #t)))

; export to standalone
(define (doexport-standalone)
  (export-standalone))

; export to text file
(define (doexport-text)
  (easy-try-catch
   (lambda ()
     ;; assuming .dyn is always appended to the filename get-filename-callback returns
     (define file-name
       (let ((tmp (get-saved-filename-string)))
         (if (not tmp)
             (set! tmp "Untitled.dyn"))
         ;; removes the extension
         (string-append (substring tmp 0 (- (string-length tmp) 4)) ".txt")
         ))
      (let ((exportfilename (get-safe-new-filename (get-last-saved-dir) #f (list ".txt") file-name "txt")))
        (if (not (eq? #f exportfilename))
            (begin
              (if (file-exists? exportfilename)
                  (delete-file exportfilename))
              (create-new-file exportfilename)
              (let ((the-nodes (get-list 'nodes)))
                ; run through all nodes and output the text
                (if the-nodes
                    (map (lambda (thisnode)
                           (append-file-data exportfilename
                                             (string-append 
                                              (ask (cdr thisnode) 'to-printable)
                                              "\n\n")))
                         the-nodes)))))))
   ))

; save a file - specifying "" as filename will open file dialog,
; specifying #f will use existing filename, if any, 
; otherwise will open file dialog
(define (dosave-wrapper filename)
  (if filename
      (dosaveas (to-string filename))
      (dosave)))

; check file version before saving
(define (check-file-version)
  (let ((save-choice #f)
        (diff-version? (not (= (get-fileformat-version) loaded-file-version))))
    ;; check save condition
    (if diff-version?
        (set! save-choice (make-confirm-dialogbox
                           (get-main-ui-frame)
                           4
                           (string-append
                            "Warning: Saving old file format Version:"
                            (to-string loaded-file-version)
                            " to new file format Version:"
                            (to-string (get-fileformat-version))))))
    
    ; return whether or not to proceed
    (or (and diff-version?
             (equal? save-choice 1))
        (not diff-version?))))

; save - will attempt to use existing filename, if any, otherwise will "save as"
(define (dosave)
  (if (not (eq? #f (get-saved-filename)))
      (if (check-file-version)
          (begin
            (ht-save-to-file (get-saved-filename) #f)
            (update-dirty-state)))
      (dosaveas (to-string "Untitled"))))

; save-as - will ask user for a new filename, regardless of whether 
; file has been saved before

(define (dosaveas filename :: <string>)
  (if (check-file-version)
      (let* ((filename (filename-extension-check filename ".dyn")) ;; make sure filename has .dyn at the end
             (newfile (get-safe-new-filename (get-last-saved-dir) #f (list ".dyn") filename "dyn")))
        (if (not (eq? #f newfile))
            (begin
              (ht-save-to-file newfile #f)
              (set-saved-filename! newfile)
              (add-recent-file newfile)
              (update-dirty-state)
              #t)
            #f))))

; ht-save to file:
; do any pre-save preparation then calls fileio.scm's save-to-file
(define (ht-save-to-file in-filename in-flag)
  (nodeeditor-save) ; save any content changes in the node editor
  (store-node-positions) ; store the node positions in the graph
  (save-to-file in-filename in-flag))

; prompt for save before close/new - returns #t if its safe to proceed, #f otherwise
(define (confirm-save)
  (if (or (dirty?) (nodeeditor-dirty?))
      ; dirty, so ask first
      (if (exceeded-node-limit?)
          ; can't save, ask if they want to quit anyway
          (let ((reply (make-confirm-dialogbox
                        (get-main-ui-frame) 2 "Current project is not saved, and node limit is exceeded.\nDo you want to quit anyway?")))
            (cond ((eq? 1 reply) #t)
                  (else #f)))
          ; ask if they want to save
          (let ((reply (make-confirm-dialogbox
                        (get-main-ui-frame) 3 "Current project is not saved.\nDo you want to save the current project?")))
            (cond ((eq? 1 reply) (dosave-wrapper #f))
                  ((eq? 2 reply) #f)
                  (else #t))))
      ; not dirty so go ahead
      #t))


;
; callbacks for common/fileio.scm
;

; set the build-sexpr-callback in common/fileio.scm
(define (set-ht-build-sexpr-callback!)
  (set-build-sexpr-callback!  ht-build-sexpr-callback))

; build sexpressions from a list of objects
(define (ht-build-sexpr-from-objectlist the-object-list)
  (if the-object-list
      (map (lambda (thisobject)
             (ask (cdr thisobject) 'to-save-sexpr))
           the-object-list)
      '()))

; build sexpressions from a list of object IDs
(define (ht-build-sexpr-from-IDlist the-object-list object-type)
  (if the-object-list
      (map (lambda (thisobjectID)
             (let ((thisobject (get object-type thisobjectID)))
               (if thisobject
                   (ask thisobject 'to-save-sexpr))))
           the-object-list)
      '()))

; build sexpression from list of objects containing rules:
; build sexpr from each object, and also extract the attached rule,
; if any, and generate sexpr from the rule
(define (ht-build-sexpr-from-objectlist-with-rules the-object-list)
  (if the-object-list
      (map (lambda (thisobject)
             (ht-build-sexpr-from-object-with-rule (cdr thisobject)))
           the-object-list)
      '()))

; build sexpression for an object and the sexpr of all the attached rules 
; will build both the object's s-expr and the rule's s-expr, plus any
; conditions and actions
(define (ht-build-sexpr-from-object-with-rule the-object)
;  (let ((theruleID (ask the-object 'rule)))
;    (if (eq? theruleID 'not-set)
;        (ask the-object 'to-save-sexpr)
;        (list 'begin
;              (ask the-object 'to-save-sexpr)
;              (ht-build-sexpr-from-rule theruleID))))
  (define rule-lst (ask the-object 'rule-lst))
  
  (append
   (list 'begin
         (ask the-object 'to-save-sexpr))
   (map ht-build-sexpr-from-rule rule-lst))
  )

; build sexpression for a rule and associated conditions and actions
(define (ht-build-sexpr-from-rule theruleID)
  (if (not (eq? theruleID 'not-set))
      (let* ((therule (get 'rules theruleID))
             (the-conditions (ask therule 'conditions))
             (the-actions (ask therule 'actions))
             (then-action-ID (ask therule 'then-action))
             (else-action-ID (ask therule 'else-action)))
        (append (list 'begin
                      (ask therule 'to-save-sexpr))
                ; conditions
                (if (not (null? the-conditions))
                    (ht-build-sexpr-from-IDlist the-conditions 'conditions)
                    '())
                ; actions
                (if (not (null? the-actions))
                    (ht-build-sexpr-from-IDlist the-actions 'actions)
                    '())
                ; then action
                (if then-action-ID
                    (let ((then-action (get 'actions then-action-ID)))
                      (list (ask then-action 'to-save-sexpr)))
                    '())
                ; else action
                (if else-action-ID
                    (let ((else-action (get 'actions else-action-ID)))
                      (list (ask else-action 'to-save-sexpr)))
                    '())))))

; callback to build the sexpression
(define (ht-build-sexpr-callback)
  (let ((the-nodes (get-list 'nodes))
        (the-links (get-list 'links))
        (the-facts (get-list 'facts)))
    (append
     ; record card shark mode
     (list 
      (list 'set-card-shark! (list 'quote (card-shark?))))
     
     ; record if basic or advanced mode
     (if (is-basic-mode?)
         (list
          (list 'set-basic-mode! 'true))
         '())
     
     ; record if back button is disabled
     (if (disable-back-button?)
         (list
          (list 'set-disable-back-button! 'true))
         '())
     
     ; save document rule, if any
     (if (has-document-rule?)
         (list (ht-build-sexpr-from-rule (get-document-ruleID)))
         '())
     
     ; run through all nodes and generate sexpr
     (ht-build-sexpr-from-objectlist-with-rules the-nodes)
     
     ; then run through all the links and generate sexpr
     (ht-build-sexpr-from-objectlist-with-rules the-links)

     ; record start node, if any
     (if (get-start-node)
         (list
          (list 'set-start-node! (get-start-node)))
         '())
     
     ; run through all facts and generate sexpr
     (ht-build-sexpr-from-objectlist the-facts))))

;;  ==============================
;;;; pre 2.2 save file conversion
;;  ==============================
(define (obj-convertion-2.2)
  (if (<= loaded-file-version 2.1)
      (begin
        (table-map 'links convert-pre-2.2-links)
        (table-map 'nodes convert-pre-2.2-nodes)
        ;(table-map 'actions convert-pre-2.2-actions)
        )))

(define (convert-pre-2.2-nodes nodeID node-obj)
  ;; convert standard node
  (let* ((ruleID (ask node-obj 'rule))
         (rule-obj (get 'rules ruleID))
         (node-name (ask node-obj 'name))
         (anywhere? (ask node-obj 'anywhere?))
         (new-ruleID (create-typed-rule2 "Set fact" 'node 'and #f nodeID))
         ;(conditions (ask rule-obj 'conditions))
         ;(actions (ask rule-obj 'actions))
         )
    
    ;; add the dummy add-anywhere-link action
    ;; (does nothing, just a place holder to let it show up)
    (if anywhere?
        (create-action "Enable Link" 'entered-node
                       (list 'add-anywhere-link)
                       new-ruleID))
    
    ;; add all actions to the new rule (only set fact actions are on pre 2.2 nodes)
    (if rule-obj
        (begin
          (define actions (ask rule-obj 'actions))
          (map (lambda (actionID)
                 (define action (get 'actions actionID))
                 (define action-string (ask action 'expr))
                 
                 (define action-input-port (open-input-string action-string))
                 (define action-sexpr (read action-input-port))
                 
                 (if (not (eof-object? action-sexpr))
                     (begin
                       (display "SEXPR ")(display action-sexpr)(newline)
                       (display "pair? ")(display (pair? action-sexpr))(newline)
                       (define new-rule (get 'rules new-ruleID))
                       (create-action node-name 'entered-node
                                      action-sexpr
                                      new-ruleID)
                       
                       ;;(create-condition name nodeID operator ruleID . args)
                       ;; add the conditions to the rules
                       ))
                 ) actions)
          
          ;; transfer the condition from the original old rule to the new rules
        ;(create-typed-condition name type targetID operator ruleID . args)
        (define old-conditions (ask rule-obj 'conditions))
        (map (lambda (condition)
               (define this-cond (get 'conditions condition))
               (let ((type (ask this-cond 'type))
                     (targetID (ask this-cond 'targetID))
                     (operator (ask this-cond 'operator)))
                 (create-typed-condition "Enable link" type targetID operator new-ruleID))
               ) old-conditions)
          )
          ;; no rule for node so no need to transfer anything to new rule
        (begin
          ;(display "no rule for node ")(display nodeID)(newline)
          #f
          ))
  ))

;; convertion of pre 2.2 links to 2.2 format
;; need to create 2 rules one for(if) and against(else) the condition in rule
;; note old rules are still around in the datatable, just not invoked
(define (convert-pre-2.2-links linkID link-obj)
  ;(display "CONVERT LINKS pre 2.2")(newline)

  (define selected-rule-ID (ask link-obj 'rule))
  (if (not (eq? selected-rule-ID 'not-set))
      (begin

        (define selected-rule (get 'rules selected-rule-ID))
        (define link-name (ask link-obj 'name))
        (define link-dest1 (ask link-obj 'destination))
        (define link-uselink (ask link-obj 'use-destination))
        (define link-usealtlink (ask link-obj 'use-alt-destination))
        (define link-usealttext (ask link-obj 'use-alt-text))
        (define link-dest2 (ask link-obj 'alt-destination))
        (define link-alttext (ask link-obj 'alt-text))
        (define link-start-index (ask link-obj 'start-index))
        (define link-end-index (ask link-obj 'end-index))
        
        (define if-rule-ID (create-typed-rule2 "THEN" 'link (ask selected-rule 'expression) #f linkID))
        (define else-rule-ID (create-typed-rule2 "ELSE" 'link (ask selected-rule 'expression) #t linkID))
        (define if-rule (get 'rules if-rule-ID))
        (define else-rule (get 'rules else-rule-ID))
                                        ;)
        ;; alt-text (text and fact)
        (if link-usealttext
            (if (eq? link-usealttext #t)
                (begin ;; show alt text
                  ;; add a new action panel
                  (create-action link-name 'displayed-node
                                 (list 'replace-link-text
                                       (list 'quote 'text)
                                       link-alttext
                                       ;(string-append "\"" link-alttext "\"")
                                       linkID)
                                 else-rule-ID
                                 )
                  )
                (begin ;; show fact text
                  (create-action link-name 'displayed-node
                                 (list 'replace-link-text
                                       (list 'quote 'text)
                                        ;(string-append "\"" link-alttext "\"")
                                       link-alttext ;; this is actually factID
                                       linkID)
                                 else-rule-ID)
                  )))

        ;; default destination
        (if (not (equal? link-dest1 -1))
            (create-action link-name 'clicked-link
                           (list 'follow-link
                                 linkID
                                 (ask if-rule 'ID)
                                 (list 'quote 'default)
                                 link-dest1)
                           if-rule-ID))

        ;; link-dest2 is used in the else so it should be in the not rules instead
        (if (not (equal? link-dest2 -1))
            (create-action link-name 'clicked-link
                           (list 'follow-link
                                 linkID
                                 (ask else-rule 'ID)
                                 (list 'quote 'default)
                                 link-dest2)
                           else-rule-ID))

        ;; transfer the condition from the original old rule to the new rules
        ;(create-typed-condition name type targetID operator ruleID . args)
        (define old-conditions (ask selected-rule 'conditions))
        (map (lambda (condition)
               (define this-cond (get 'conditions condition))
               (let ((type (ask this-cond 'type))
                     (targetID (ask this-cond 'targetID))
                     (operator (ask this-cond 'operator)))
                 (create-typed-condition "If-rule" type targetID operator if-rule-ID )
                 (create-typed-condition "else-rule" type targetID operator else-rule-ID ))
               ) old-conditions)
        )))