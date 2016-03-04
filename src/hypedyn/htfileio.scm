;; Part of the HypeDyn project - http://www.narrativeandplay.org/hypedyn
;; 
;; Copyright (C) 2008-2016
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
  (require "properties-ui.scm")
  (require "nodeeditor.scm")
  (require "rules-manager.scm") ;; delete-rule-from-obj
  (require "hypedyn-undo.scm") ;; hd-postedit
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
  (require "../kawa/ui/undo.scm")
  (require "../common/main-ui.scm")
  (require "../common/runcode.scm") ;; runcode-exp
  (require "export.scm")
  ;(require "editlink.scm") ;; obj-convertion-2.2
  (require 'list-lib) ;; list-ref

  (require "json-r7rs/json.scm") ;; for export-to-hypedyn2
  (import (macduffie json))
  (require 'hash-table) ;; for export-to-hypedyn2
  )

; export
(module-export set-ht-build-sexpr-callback!
               donew doopen open-file-by-name 
               doimport doexport-hypedyn-web doexport-standalone doexport-text doexport-js
               copy-js-framework-to-folder
               dosave-wrapper confirm-save ht-save-to-file
               ht-build-sexpr-from-object-with-rule ht-build-sexpr-from-rule
               clear-loaded-file-version ;; used by clear-data in hteditor.scm
               loaded-file-version
               get-hypedyn-folder-file get-hypedyn-folder-string check-hypedyn-folder hd-autosave
               export-hypedyn2
               )

; set fileformat version and type
; Note: file format version numbers only use the major and minor rev numbers; if you change the file format, be sure to
; increment at least the minor rev number (the third sequence is for bug fixes or small changes that don't affect file format)
; Note: hypedyn files are marked as type 'htfe (HyperTextFiction Editor, the original name of the app way back)

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
  ; check if data has changed
  (if (confirm-save)
      ; safe, so proceed
      (let ((newfilename (if (pair? args) (car args) (get-file-to-open (get-last-saved-dir) #f (list ".dyn")))))
        (open-file-by-name newfilename)
        (set-runstate #f))))

;; used when opening and importing files
;; return file-version-number when accepted
;; return #f when incompatible or canceled
(define (open-file-version-check! newfilename)
  
  ;; the version of hypedyn that saved this file
  (define file-version-number 1)
  (define file-type #f)
  (define (get-version-number lst)
    (if (and (pair? lst)
             (equal? (car lst) 'make-hypertext))
        (begin
          ;(display "[make ht found!] ")(display lst)(newline)
          (set! file-version-number (list-ref lst 2))
          (set! file-type (list-ref (list-ref lst 1) 1))
          ;(display "file-version num ")(display file-version-number)(newline)
          ;(display "file type ")(display file-type)(newline)
          ))
    )
  (parse-sexpr-file newfilename get-version-number)
  
  ;; incompatible conditions or conditions that need notifications
  (cond ((not (equal? file-type (get-fileformat-type))) ;; file format does not match htfe
         ;; error dialog
         (make-error-dialog (get-main-ui-frame)
                            "Hypedyn Open Error"
                            (string-append
                             "Error: this is not a HypeDyn file"
                             (if (symbol? file-type)
                                 (string-append " (type: " (symbol->string file-type) ").")
                                 ".")))
         #f
         )
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
         ;; these are the only condition to go ahead with loading
         (if (or (and diff-version?
                      (equal? open-choice 1))
                 (not diff-version?))
             file-version-number
             #f)
         )
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
         #f
         ))
  )

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

           ;; version-check checks whether the version number on the file is compatible
           ;; if older versions ask for confirmation
           ;; if incompatible file-version-number is false
           (let ((file-version-number (open-file-version-check! newfilename)))
             (if file-version-number
                 (begin
                   (set! loaded-file-version file-version-number)
                   
                   ;; if different version then mark the new objects for conversion
                   (if (not (= (get-fileformat-version) file-version-number))
                       (set-conversion-flag! #t))
                   
                   ;; load from file
                   (define load-file-result (load-from-file newfilename))
                   
                   (if (not (= (get-fileformat-version) file-version-number))
                       (set-conversion-flag! #f))
                   
                   (if load-file-result
                       (begin
                         (add-recent-file newfilename)  ;; add to recent menu
                         (if (not (= (get-fileformat-version) file-version-number)) ; convert file format if necessary
                             (obj-conversion-2.3))
                         (populate-display)             ;; populate the display (important to convert first)
                         ))
                   )))
           ))
   (ex <java.lang.Throwable>
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

              ;; also asks for confirmations when file version is not the same
              (define version-number (open-file-version-check! newfilename))
              
              ; clear the UI
              (clear-display)
              
              ;; get the list of ID 
              (define (datatable-ID-lst name-sym)
                ;;(display "datatable ID ")(display name-sym)(newline);(display (get-list name-sym))(newline)
                (let ((lst (get-list name-sym)))
                  (if (pair? lst)
                      (map (lambda (pair)
                             (car pair)
                             ) lst)
                      '())
                  ))
              
              (define (after-import)
                (define action-lst
                  (let ((lst (get-list 'actions)))
                    (if (pair? lst)
                        (map (lambda (pair)
                               (cdr pair)
                               ) lst)
                        '())))
                
                (display "AFTER IMPORT ")(newline)
                (map (lambda (action)
                       (if (ask action 'imported?)
                           (begin
                             
                             (display "AI 2 ")(newline)
                             ;; do post import processing on the actions
                             (ask action 'after-import)
                             

                             ;; remove the flag after that 
                             (ask action 'set-imported! #f)
                             ))
                       ) action-lst)
                )

              ;; caching of rules, actions and conditions are done through caching of node and link
              ;; list of ID before import
              (define node-lst-b4 (datatable-ID-lst 'nodes))
              (define link-lst-b4 (datatable-ID-lst 'links))
              (define fact-lst-b4 (datatable-ID-lst 'facts))
              (define start-node-b4 (get-start-node))
              
              (if version-number
                  (begin
                    (add-recent-file newfilename)  ;; add to recent menu

                    ;; do the import 
                    (define import-result #f)
                    (display "IMPORTING")(newline)
                    (display "fileformat ")(display (get-fileformat-version))(newline)
                    (display "version number ")(display version-number)(newline)
                    
                    (if (not (= (get-fileformat-version) version-number))
                        (begin
                          ;; mark the imported object for conversion
                          (set-conversion-flag! #t)
                          (set! import-result (import-from-file newfilename))
                          (set-conversion-flag! #f)
                          
                          ;; convert the imported data
                          (obj-conversion-2.3)
                          )
                        (begin
                          (set! import-result (import-from-file newfilename))))

                    ;; if import success
                    (if import-result
                        (begin
                          ;; post an undoable edit
                          (post-import-undo-edit node-lst-b4 link-lst-b4 fact-lst-b4)

                          ;; process the imported actions
                          (after-import)
                          )
                        (begin
                          (display "[Error]: doimport failure when doing import-from-file")(newline))
                        )
                    ))

              ;; post an undo action for import
              ;; compares the lists of node, link and fact IDs to see which are the imported objects
              
              (define (post-import-undo-edit node-lst-b4 link-lst-b4 fact-lst-b4)
                 ;; list of ID after import
                (define node-lst-aft (datatable-ID-lst 'nodes))
                (define link-lst-aft (datatable-ID-lst 'links))
                (define fact-lst-aft (datatable-ID-lst 'facts))

                ;; list of ID that are added during import
                (define imported-nodes (lset-difference = node-lst-aft node-lst-b4))
                (define imported-links (lset-difference = link-lst-aft link-lst-b4))
                (define imported-facts (lset-difference = fact-lst-aft fact-lst-b4))

                ;; make them into alist again
                (define imported-nodes-alist
                  (map (lambda (ID)
                         (cons ID (get 'nodes ID)))
                       imported-nodes))
                (define imported-links-alist
                  (map (lambda (ID)
                         (cons ID (get 'links ID)))
                       imported-links))
                (define imported-facts-alist
                  (map (lambda (ID)
                         (cons ID (get 'facts ID)))
                       imported-facts))

                (define imported-sexpr
                  (append
                   (list 'begin)

                   ;; run through all nodes and generate sexpr
                   (ht-build-sexpr-from-objectlist-with-rules imported-nodes-alist)

                   ;; then run through all the links and generate sexpr
                   (ht-build-sexpr-from-objectlist-with-rules imported-links-alist)

                   ;; record start node, if any
                   (if (get-start-node)
                       (list
                        (list 'set-start-node! (get-start-node)))
                       '())

                   ;; run through all facts and generate sexpr
                   (ht-build-sexpr-from-objectlist imported-facts-alist)))

                (hd-begin-update undo-manager)

                ;; undoable event
                (hd-postedit
                 undo-manager
                 (make-undoable-edit
                  "Import File"
                  (lambda () ;; undo

                    ;; undo deletes the newly imported nodes and facts (links are tied to nodes)
                    (map (lambda (node-keypair)
                           (dodelnode (car node-keypair)))
                         imported-nodes-alist)
                    (map (lambda (fact-keypair)
                           (delete-fact (car fact-keypair)))
                         imported-facts-alist)

                    ;; restore start node
                    ;; technically we just need to repopulate the node-list but that is not exported at the moment
                    (clear-display)
                    (if start-node-b4
                        (set-start-node! start-node-b4))
                    ;; update start node display

                    (populate-display)
                    )
                  (lambda () ;; redo
                    ;; just run the expr to recreate the imported objects
                    (clear-display)
                    (runcode-just-sexpr imported-sexpr)
                    (populate-display)
                    )))

                (hd-end-update undo-manager undo-action redo-action)) ;; end of post-import-undo-edit
              
              
              ;; have to populate since we cleared before
              (populate-display)
              
              ; import from file
;              (if (import-from-file newfilename)
;                  (begin
;                    ;; TODO needs work and testing
;                    ;; I would assume we need to do a object conversion in import as well
;                    ;; the below two lines is to be put in and tested
;                    ;; (add-recent-file newfilename)  ;; add to recent menu
;                    ;;(obj-conversion-2.2)           ;; if loading pre 2.2 objects convert to post 2.2 format
;                    
;                    ; populate the display
;                    (populate-display)))
              
              ))))))

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
              (set! loaded-file-version (get-fileformat-version))
              
              (if (ht-save-to-file newfile #f)
                  (begin
                    (display "ht save to file SUCCESS ")(newline)
                    (set-saved-filename! newfile)
                    (add-recent-file newfile)
                    (update-dirty-state)
                    #t)
                  #f)
              )
            #f))))

(define (ht-write-permit-error-dialog)
  (make-error-dialog 
   (get-main-ui-frame)
   "Error"
   "Unable to save as you don't have permission\nto write to this location.")
  )

; ht-save to file:
; do any pre-save preparation then calls fileio.scm's save-to-file
(define (ht-save-to-file in-filename silent?)
  (nodeeditor-save) ; save any content changes in the node editor
  (store-node-positions) ; store the node positions in the graph
  (save-to-file in-filename silent? ht-write-permit-error-dialog))

(define (hd-dirty?)
  (or (dirty?) (nodeeditor-dirty?)))

; prompt for save before close/new - returns #t if its safe to proceed, #f otherwise
(define (confirm-save)
  (if (hd-dirty?)
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
            (display "reply ")(display reply)(newline)
            (cond ((eq? 1 reply) (dosave-wrapper #f))
                  ;; cancel or close dialog
                  ((or (eq? 2 reply)
                       (eq? 4 reply)) #f)
                  ((= 3 reply) #t)
                  (else #f))))
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
     (ht-build-sexpr-from-objectlist the-facts)
     
     ;; config options
     (list 
      (list 'set-disable-restart-button! (disable-restart-button?))
      (list 'set-disable-back-button! (disable-back-button?))
      (list 'set-disable-pagebreak! (disable-pagebreak?))
      (list 'set-disable-page-resize! (disable-page-resize?))
      (list 'set-fixed-page-width! (get-fixed-page-width))
      (list 'set-fixed-page-height! (get-fixed-page-height))
      (list 'set-css-type! (list 'quote (get-css-type)))
      (list 'set-custom-css-location! (get-custom-css-location))
      (list 'set-custom-css-location2! (get-custom-css-location2))
      (list 'set-author-name! (get-author-name))
      (list 'set-story-title! (get-story-title))
      (list 'set-story-comment! (get-story-comment))
      )
         
     )))

;;  ==============================
;;;; pre 2.2 save file conversion
;;  ==============================

(define (obj-conversion-2.3)
  ;; if loading pre 2.2 objects convert to 2.2 format
  (if (< (get-fileformat-version) 2.2)
      (obj-conversion-2.2))
  
  ; then do any conversion necessary from 2.2 to 2.3
  (table-map 'links convert-2.2-to-2.3-links)
  (let ((y-offset (get-max-node-y)))
    (table-map 'nodes (lambda (nodeID node-obj)
                        (convert-2.2-to-2.3-nodes nodeID node-obj y-offset)))))

;; use with care
;; assumes whatever is inside the data table are all pre 2.2
;; since there is no clear way to differentiate between pre 2.2 and post 2.2 
(define (obj-conversion-2.2)
  ;;(newline)
  ;;(display "STARTed v2.2 conversion ")
  ;;(newline)
  ;;(newline)
  
  (table-map 'links convert-pre-2.2-links)
  (table-map 'nodes convert-pre-2.2-nodes)
  )

; calculate max y position from data table (can't use get-max-node-positions as its in graph editor and nodes aren't drawn yet)
(define (get-max-node-y)
  (let ((max-y 0))
    (table-map 'nodes
               (lambda (nodeID node-obj)
                 (let ((this-node-y (ask node-obj 'get-y))
                       (is-anywhere (ask node-obj 'anywhere?)))
                   (if (and (> this-node-y max-y)
                            (not is-anywhere))
                       (set! max-y this-node-y)))))
    max-y))

; calculate the new y position
(define (calc-new-node-y old-y y-offset)
  (let ((raw-y (+ y-offset old-y initial-y)))
    (if (snap-to-grid?)
        (* initial-y (round (div raw-y initial-y)))
        raw-y)))

(define (convert-2.2-to-2.3-nodes nodeID node-obj y-offset)
  (let* ((ruleID (ask node-obj 'rule))
         (anywhere? (ask node-obj 'anywhere?)))
    ; shift anywhere nodes down below the rest of the nodes
    (if anywhere?
        (ask node-obj 'set-y! (calc-new-node-y (ask node-obj 'get-y) y-offset)))))
  
(define (convert-2.2-to-2.3-links linkID link-obj)
  ; nothing yet
  'continue
)
  
(define (convert-pre-2.2-nodes nodeID node-obj)

  ;; convert standard node
  (let* ((ruleID (ask node-obj 'rule))
         (rule-obj (get 'rules ruleID))
         (node-name (ask node-obj 'name))
         (anywhere? (ask node-obj 'anywhere?))
                                        ;(conditions (ask rule-obj 'conditions))
                                        ;(actions (ask rule-obj 'actions))
         )


    ;; add all actions to the new rule (only set fact actions are on pre 2.2 nodes)
    (if (ask node-obj 'convert-flag)
        (if rule-obj
            (begin
              
              ;; conversion underway for this one so the flag has done its job
              (ask node-obj 'set-convert-flag! #f)
              
              (define new-ruleID (create-typed-rule2 "Set fact" 'node 'and #f nodeID))
              
              ;; add the dummy add-anywhere-link action
              ;; (does nothing, just a place holder to let it show up in the rule editor)
              ;; Note: this however is important in the export to js 
              (if anywhere?
                  (create-action "Enable Link" 'anywhere-check
                                 (list 'add-anywhere-link nodeID)
                                 new-ruleID))

              ;; only version 2.1 has actions
              ;; the expr of the actions has to be converted to list from string
              (define actions (ask rule-obj 'actions))
              ;;(display "actions count in CONVERSION ")(display (length actions))(newline)
              (map (lambda (actionID)
                     (define action (get 'actions actionID))
                     (define action-string (ask action 'expr))

                     (define action-input-port (open-input-string action-string))
                     
                     ;; just need to read once because action sexpr are one liners
                     (define action-sexpr (read action-input-port))

                     (if (not (eof-object? action-sexpr))
                         (begin
                           ;;(display "SEXPR ")(display action-sexpr)(newline)
                           ;;(display "pair? ")(display (pair? action-sexpr))(newline)
                           (define new-rule (get 'rules new-ruleID))
                           
                           ;; set fact actions
                           (define new-action-ID
                             (create-action node-name 'entered-node
                                            action-sexpr
                                            new-ruleID))
                           
                           (define new-action (get 'actions new-action-ID))
                           
                           ;; transfer the imported? flag
                           (ask new-action 'set-imported! (ask action 'imported?))
                           
                           ;; remove the old actions 
                           ;; that has expr in the string form
                           (display "deleting old action ")(display actionID)(newline)
                           (del 'actions actionID)

                           ;;(create-condition name nodeID operator ruleID . args)
                           ;; add the conditions to the rules
                           ))
                     ) actions)

;;              (display "actions count2 in CONVERSION ")(display (length (ask rule-obj 'actions)))(newline)
;;              (display "actions ")(display (ask rule-obj 'actions))(newline)
;;              (display "new-ruleID ")(display new-ruleID)(newline)
;;              (display "new actions ")(display (ask (get 'rules new-ruleID) 'actions))(newline)
              
              ;; transfer the condition from the original old rule to the new rules
              ;(create-typed-condition2 name type targetID operator ruleID #!key fixedID comparator-args)
              (define old-conditions (ask rule-obj 'conditions))
              (map (lambda (condition)
                     (define this-cond (get 'conditions condition))
                     (let ((type (ask this-cond 'type))
                           (targetID (ask this-cond 'targetID))
                           (operator (ask this-cond 'operator)))
                       (create-typed-condition2 "Enable link" type targetID operator new-ruleID))
                     ) old-conditions)
              )
            ;; no rule for node so no need to transfer anything to new rule
            (begin
              (ask node-obj 'set-convert-flag! #f)
                                        ;;(display "no rule for node ")(display nodeID)(newline)
              #f
              )))
    ))

;; convertion of pre 2.2 links to 2.2 format
;; need to create 2 rules: one (if) with the conditions in the old rule and a "stop", the other (else) with no conditions
;; note old rules are still around in the datatable, just not invoked
(define (convert-pre-2.2-links linkID link-obj)
  
  (if ;(or (not (eq? selected-rule-ID 'not-set))
      ;    version-one?)
      (ask link-obj 'convert-flag)
      (begin
        ;; conversion underway for this one so the flag has done its job
        (ask link-obj 'set-convert-flag! #f)
        
        ;(display "REALLY converting link ")(display linkID)(newline)
        
        (define version-one? (= loaded-file-version 1))

        (define selected-rule-ID (ask link-obj 'rule))
        
        ;; NOTE: rule, destination, use-destination, use-alt-destination,
        ;;       use-alt-text, alt-destination, alt-text
        ;;       should not be removed but should be kept as a compatibility layer so that the 
        ;;       older version of the object still works
        ;; TODO: move these attributes out to a separate object type inherited by the new link
        (define selected-rule (get 'rules selected-rule-ID))
        (define link-name (ask link-obj 'name))

        ;; old attributes
        (define link-dest1 (ask link-obj 'destination))
        (define link-uselink (ask link-obj 'use-destination))
        (define link-usealtlink (ask link-obj 'use-alt-destination))
        (define link-usealttext (ask link-obj 'use-alt-text))
        (define link-dest2 (ask link-obj 'alt-destination))
        (define link-alttext (ask link-obj 'alt-text))

        (define link-start-index (ask link-obj 'start-index))
        (define link-end-index (ask link-obj 'end-index))

;        (define and-or
;          (if version-one?
;              'and
;              (ask selected-rule 'and-or)))
        (define and-or (ask selected-rule 'and-or))

        (define if-rule-ID (create-typed-rule3 "IF" 'link and-or #f linkID fall-through?: #f))
        (define else-rule-ID (create-typed-rule2 "ELSE" 'link and-or #f linkID))
        (define if-rule (get 'rules if-rule-ID))
        (define else-rule (get 'rules else-rule-ID))
                                        ;)
        ;; alt-text (text and fact)
        (if link-usealttext
            (if (eq? link-usealttext #t) ;; if use-alt-text is just #t use alt-text
                (begin ;; show alt text
                  ;; add a new action panel
                  (create-action link-name 'displayed-node
                                 (list 'replace-link-text
                                       "alternative text"
                                       link-alttext
                                        ;(string-append "\"" link-alttext "\"")
                                       linkID)
                                 else-rule-ID))
                ;; if use-alt-text is a factID use fact text
                (begin ;; show fact text 
                  (create-action link-name 'displayed-node
                                 (list 'replace-link-text
                                       "text fact"
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

        ;; transfer the condition from the original old rule to the new (if) rule
        (if selected-rule
            (begin
              (define old-conditions (ask selected-rule 'conditions))
              (map (lambda (condition)
                     (define this-cond (get 'conditions condition))
                     (let ((type (ask this-cond 'type))
                           (targetID (ask this-cond 'targetID))
                           (operator (ask this-cond 'operator)))
                       
                       (create-typed-condition2 "if-rule" type targetID operator if-rule-ID ))
                     ) old-conditions)
              ))
        
        ;; remove empty IF and ELSE rule
        (if (ask if-rule 'empty-rule?)
            (delete-rule-from-obj if-rule-ID link-obj))
        
        (if (ask else-rule 'empty-rule?)
            (delete-rule-from-obj else-rule-ID link-obj))
        
        )))

;; ============================
;;;; string processing helper
;; ============================

;; add "\"" to start and end
(define (quote-nest str)
  (string-append "\"" str "\""))

;; preserves \n when we print out the string,
;; replaces "\n" with "\\n"
;; replaces "\r" with ""
(define (preserve-newline str)
  ;(define r-replaced (replace-char str #\x0D ""))
  ;(replace-char r-replaced #\newline "\\n")
  (define r-replaced (string-replace-all "\r" "" str))
  (string-replace-all "\n" "\\n" r-replaced))

(define (preserve-quotes str)
  ;; escaping both \ and " here so that 
  ;; \" shows up in the printed display
  ;(replace-char str #\" "\\\"")
  
  ;(display "preserve quotes ")(newline)
  ;(display "convert1 ")(display (to-string #\"))(newline)
  ;(display "convert2 ")(display (substring (to-string #\") 1 2))(newline)
  ;(display "str ")(display str)(newline)
  (string-replace-all "\"" "\\\"" str)
  )

(define (preserve-forward-slash str)
  (string-replace-all "\\" "\\\\" str))

(define (escape-special str)
  (define retval str)
  (set! retval (preserve-forward-slash retval))
  (set! retval (preserve-quotes retval))
  (set! retval (preserve-newline retval))
  retval)

;(define debug-count 0)
;(define debug-cache #f)

;(define debug-cache2 #f)

;; can consider changing this to use (<string> #\a #\p #\p #\l #\e) to form the final string
;; need a helper to get the list of char
;; then apply <string> to the list of char
;; idea is to replace the char we are looking for with two char #\\  and original char 
;; so that the char we want to preserve is escaped
;(define (replace-char str :: <string>
;                      char :: <char>
;                      char-str :: <string>)
;  
;  (if (equal? debug-cache str)
;      (set! debug-count (+ debug-count 1))
;      (set! debug-count 0)
;      )
;  (if (> debug-count 1)
;      (begin
;        (display "debug str ")(display debug-cache)(newline)
;        (display "str ")(display str)(newline)
;        (display "len same? ")(display (= (string-length str) (string-length debug-cache)))(newline)
;        (display "infinite loop in replace-char")(newline)
;        (sleep 1000)
;        ))
;  (set! debug-cache str)
;  
;  
;  (if (= (string-length str) 0)
;      "" ;; terminate
;      (begin
;        (string-append
;         (if (equal? (string-ref str 0) char)
;             char-str
;             ;; cast original char to string
;             (<string> (string-ref str 0)))
;         (replace-char (substring str 1 (string-length str)) char char-str))
;        )))

;(define (replace-char str :: <string>
;                      char :: <char>
;                      char-str :: <string>)
;  (define (helper index)
;    (define find (string-indexof str (to-string char) index))
;    (if (not (= find -1))
;        (begin
;          (set! str (string-replace str char-str find (+ find 1)))
;          (display "replacing ")(newline)
;          (helper (+ find 1))))
;    )
;  (helper 0)
;  str
;  )

;;=====================
;;;; javascript export
;;=====================


(define (copy-js-framework)
  ;; assuming .dyn is always appended to the filename get-filename-callback returns

  (display "before folder name")(newline)
  (define folder-name
    (let ((tmp (get-saved-filename-string)))
      (if (not tmp)
          (set! tmp "Untitled.dyn"))

      (display "folder name ")(display tmp)(newline)
      ;; removes the extension
      (string-append (substring tmp 0 (- (string-length tmp) 4)) "-JS")
      ))
  (display "after folder name ")(display folder-name)(newline)

  (display "last exported dir ")(display (get-last-exported-dir))(newline)
  
  (display "cwd display ")(display (make-file "."))(newline)
  (let ((export-folder (get-safe-new-filename (make-file (get-last-exported-dir)) #t '() folder-name))) ;; third arg was #t
    (if (not (eq? #f export-folder))
        (begin
          (display "export folder ")(display export-folder)(newline)
          (update-last-exported-dir! export-folder)

          ;; export to the export folder
          (copy-js-framework-to-folder export-folder))
        #f)))


(define (copy-js-framework-to-folder export-folder)
  (define safetoproceed #t)

  ;; create folder, first deleting if it already exists
  (try-catch
      (export-create-folder export-folder)
    (ex <java.lang.Throwable>
        (begin
          (display (*:toString ex))(newline)
          (set! safetoproceed #f)
          (display "export-create-folder FAILED")(newline)

          (make-error-dialog
           (get-main-ui-frame)
           "Error"
           "Unable to export: failed to create export folder.")

          ;(*:printStackTrace ex)
          )))

  ;; Note: put try-catch around this and cleanup on failure
  (if safetoproceed
  (try-catch
      (begin
        (define source-folder (get-content-file "js" (mac-testing?)))
        (recursively-copy-directory source-folder
                                    export-folder)

        ;; copy css files
        (define styling-css-file
          (case (get-css-type) ;(get-stylesheet-choice)
            ((default) (get-content-file "css/styling.css" (mac-testing?)))
            ((fancy) (get-content-file "css/styling2.css" (mac-testing?)))
            ((custom) (make-file (get-custom-css-location) ))))

        (copy-file-nio styling-css-file (make-file (string-append (to-string export-folder) "/styling.css")))

        (write-jscode-to
         (string-append (path-file export-folder) "/dynfile.js")
         (generate-jscode))
        #t)
    (ex <java.lang.Throwable>
        (begin
          (display (*:toString ex))(newline)
          (display "export FAILED")(newline)

          (make-error-dialog
           (get-main-ui-frame)
           "Error"
           (*:toString ex))

                                        ;(*:printStackTrace ex)
          )))))

(define (doexport-js) 
  (copy-js-framework))

;; createNode (content, id)
(define (js-node-code nodeID)
  (define node (get 'nodes nodeID))  
  (define name (ask node 'name))
  (define links (ask node 'links))
  (define rule-lst (ask node 'rule-lst))
  (define anywhere? (ask node 'anywhere?))
  
    (apply string-append
           (append
            (list "\tcreateNode("
                  (quote-nest (escape-special name)) ", " ;; not safe to assume no funny characters in these names
                  (quote-nest 
;                   (preserve-newline 
;                    (preserve-quotes (ask node 'content))
;                    ;(ask node 'content)
;                    )
                   (escape-special (ask node 'content))
                   ) ", "
                  (to-string anywhere?) ", "
                  (to-string (ask node 'ID)) ");\n")
            (map js-link-code links)     ;; convert its links
            (map js-rule-code rule-lst)   ;; convert its rules
            (list "\n") ;; leave a blank line between each node's code block
            )))

;; createLink(nodeID, start, end, id)
(define (js-link-code linkID)
  (let* ((link (get 'links linkID))
         (rule-lst (ask link 'rule-lst)))
    (apply string-append 
           (append 
            (list "\tcreateLink("
                  (to-string (ask link 'source)) ", "
                  (to-string (ask link 'start-index)) ", "
                  (to-string (ask link 'end-index)) ", "
                  (to-string (ask link 'ID)) ");\n")
            (map js-rule-code rule-lst)
            ))))

;; createRule(parentID, parentType, if_not, and_or, fall_through, id)
(define (js-rule-code ruleID)
  (let* ((rule (get 'rules ruleID))
         (actions (ask rule 'actions))
         (conditions (ask rule 'conditions))
         (parent-type (case (ask rule 'type)
                        ((link) "link")
                        ((node) "node")))
         (if-not (if (ask rule 'negate?) "not" "if"))
         (and-or (cond ((equal? (ask rule 'and-or) 'and) "and")
                       ((equal? (ask rule 'and-or) 'or) "or")))
         (fall-through  (if (ask rule 'fall-through?) "true" "false")))
    
    ;; skip rules with no actions (commenting out skipping null actions for now) 
    ;;(if (not (null? actions))
        (apply string-append
               (append
                (list "\t\tcreateRule("
                      (to-string (ask rule 'parentID)) ", "
                      (quote-nest parent-type) ", "
                      (quote-nest if-not) ", "
                      (quote-nest and-or) ", "
                      fall-through ", "
                      (to-string (ask rule 'ID)) ");\n")
                (map js-condition-code conditions)
                (map js-action-code actions)
                ))
     ;;   "")
    ))

;; createAction(eventType, parentRuleID, func, args, id)
(define (js-action-code actionID)
  (let* ((action (get 'actions actionID)) ; xxx
         (expr (ask action 'expr))
         (event-type (case (ask action 'type)
                       ((entered-node displayed-node) "enteredNode")
                       ((clicked-link) "clickedLink")
                       ((anywhere-check) "anywhereCheck")
                       ((disabled-anywhere-check) "disabledAnywhereCheck")
                       (else "ERROR EVENT")))
         (parent-rule-id (ask action 'ruleID))
         
         ;; follow-link (follow-link2 linkID parent-ruleID link-type dest-nodeID) , gotoNode(nodeID)
         ;; (replace-link-text text-type value linkID) replaceText(linkID, altcontent)
         ;; set-value! (set-fact-value! in-factID in-value), (assert factID), (retract factID) setFact( id, value)
         (func (case (car expr) 
                 ((follow-link) "gotoNode")
                 ((replace-link-text) "replaceText")
                 ((set-value! assert retract) "setFact")
                 ((set-number-fact) "setNumberFact")
                 ((add-anywhere-link) "addAnywhereLink")
                 ((show-disabled-anywhere-link) "addInactiveAnywhereLink")
                 ((show-in-popup) "popup")
                 ))
         
         ;; a javascript array of argument
         (args (case (car expr)
                 ((follow-link)
                  (string-append "[" (to-string (list-ref expr 4)) "]"))
                 ((assert)
                  (string-append "[" (to-string (list-ref expr 1)) ", "
                                 "true]"))
                 ((retract)
                  (string-append "[" (to-string (list-ref expr 1)) ", "
                                 "false]"))
                 ((set-value!)
                  (string-append "["
                                 (to-string (list-ref expr 1)) ", "
                                 (quote-nest (escape-special (list-ref expr 2))) ;; remember to escape the text
                                 "]"))
                 ((add-anywhere-link)
                  (string-append "[" (to-string (list-ref expr 1)) "]")) ;; TODO add anywhere link not implemented
                 ((show-disabled-anywhere-link)
                  (string-append "[" (to-string (list-ref expr 1)) "]")) ;; TODO add anywhere link not implemented
                 ((replace-link-text)
                  (string-append
                   "[" (to-string (list-ref expr 3)) ", "
                   (quote-nest (list-ref expr 1)) ", " ;; content_type
                   
                   ;; differentiate between fact text or just alt text
                   (let ((val (list-ref expr 2)))
                     (cond ((string? val)
                            (quote-nest
                             (escape-special val)
                             ))
                           ((number? val)
                            (to-string val))))
                   "]"))
                 ((show-in-popup)
                  (string-append "[" (to-string (list-ref expr 1)) "]")
                  )
                 ((set-number-fact)
;                  (string-append "[" (to-string (list-ref expr 1)) ", "
;                                 ;; give in number form
;                                 (list-ref expr 2) 
;                                 "]")
                  
                  (define target-factID (list-ref expr 1))
                  (define num-fact-mode (list-ref expr 2))
                  (define fact-value
                    (case num-fact-mode
                      (("Input" "Fact") (string-append "[" (to-string (list-ref expr 3)) "]" ))
                      (("Math")
                       ;; (list op opr1 opr1-type opr2 opr2-type)
                       (let* ((op            (list-ref (list-ref expr 3) 0))
                              (operand1      (list-ref (list-ref expr 3) 1))
                              (operand1-type (list-ref (list-ref expr 3) 2))
                              (operand2      (list-ref (list-ref expr 3) 3))
                              (operand2-type (list-ref (list-ref expr 3) 4)))

                         (string-append "[" (quote-nest op) ", "
                                        operand1 ", "
                                        (quote-nest operand1-type) ", "
                                        operand2 ", "
                                        (quote-nest operand2-type) "]"
                                        ))
                         )
                      (("Random")
                       ;; (list opr1 opr1-type opr2 opr2-type)
                       (let* ((operand1      (list-ref (list-ref expr 3) 0))
                              (operand1-type (list-ref (list-ref expr 3) 1))
                              (operand2      (list-ref (list-ref expr 3) 2))
                              (operand2-type (list-ref (list-ref expr 3) 3)))

                         (string-append "[" 
                                        operand1 ", "
                                        (quote-nest operand1-type) ", "
                                        operand2 ", "
                                        (quote-nest operand2-type) "]"
                                        ))
                         )
                      ))
                  (string-append "[" (to-string target-factID) ", "
                                 (quote-nest num-fact-mode) ", "
                                 fact-value
                                 "]")
                  )
                 )))
    
    ;; return string
    (if (and (not (equal? func #!void))
             (not (equal? args #!void)))

        (string-append "\t\t\tcreateAction("
                       (quote-nest event-type) ", "
                       (to-string parent-rule-id) ", "
                       func ", "
                       args ", "
                       (to-string actionID) ");\n")
        (begin
          (display "UNKNOWN ACTION ENCOUNTERED (in JS ACTION EXPORT) ")(display expr)(newline)
        "")) ;; ignore if func and args not recognised
  ))

;;(make-condition name type targetID operator ruleID . args)
;; createCondition(func, func_target_ID, ruleID, not, id)

;(cond
; ((eq? type 0)
;  ;; node
;  (cond ((eq? operator 0) (list 'not (list 'visited? targetID)))
;        ((eq? operator 1) (list 'visited? targetID))
;        ((eq? operator 2) (list 'previous? targetID))))
; ((eq? type 1)
;  ;; link
;  (cond ((eq? operator 0) (list 'not (list 'followed? targetID)))
;        ((eq? operator 1) (list 'followed? targetID))))
; ((eq? type 2)
;  ;; fact
;  (cond ((eq? operator 0) (list 'not (list 'holds? targetID)))
;        ((eq? operator 1) (list 'holds? targetID)))))

(define (js-condition-code conditionID)
  
  (let* ((condition (get 'conditions conditionID))
         (ruleID (ask condition 'ruleID))
         (func (case (ask condition 'type)
                 ((0) "nodeVisited");; node TODO: previous not done in js
                 ((1) "linkFollowed") ;; link
                 ((2) "checkBoolFact") ;; boolean fact 
                 ((3) "compareNumFact")
                 ))
         (func-target-id (ask condition 'targetID))
         (negate (case (ask condition 'operator)
                   ((0) "true")
                   ((1) "false")
                   ((2) 
                    (set! func "nodeIsPrevious")
                    "false")
                   ((3) 
                    (set! func "nodeIsPrevious")
                    "true")
                   (else "false")
                   ))
         (func-args 
          (if (not (= (ask condition 'type) 3))
              (string-append "[" (to-string func-target-id) "]")
              ;; (list comparator operand-type operand-choice)
              (let* ((args-lst (ask condition 'numfact-args))
                     (comparator (car args-lst))
                     (operand-type (cadr args-lst))
                     (operand-choice (caddr args-lst)))
                (string-append "[" (to-string func-target-id) ", "
                               "'" comparator "'" ", "
                               "'" operand-type "'" ", "
                               operand-choice
                               "]"))
              )))
    
    ;; return string
    (string-append "\t\t\t"
                   "createCondition("
                   func ", "
                   func-args ", "
                   (to-string ruleID) ", "
                   negate ", "
                   (to-string conditionID) ");\n")
    ))

;createFact(name, type, id)
(define (js-fact-code factID)
  (display "factID ")(display factID)(newline)
  (let* ((fact (get 'facts factID))
         (value (ask fact 'get-value))
         (name (ask fact 'name))
         (type (ask fact 'type)))
    
    (display "name ")(display name)(newline)
    (display "type ")(display type)(newline)
    (string-append "\tcreateFact("
                   (quote-nest (escape-special name)) ", "
                   (quote-nest (to-string type)) ", "
                   (to-string factID) ");\n")
  ))

(define (generate-jscode)
  
  (define node-lst (get-list 'nodes))
  (define fact-lst (get-list 'facts))
  
  ;; get-list returns a #f when not found
  (if (not node-lst)
      (set! node-lst '()))
  (if (not fact-lst)
      (set! fact-lst '()))
           
  (display "generating js code ")(newline)
  ;; go through all the nodes
  (apply string-append
         (append 
          (list "function loadStory() {\n")
          
          ;; Set start node
          ;; add this only if start node is set
          (if (get-start-node)
              (list "\tsetStartNode(" (to-string (get-start-node)) ");\n")
              '())
          
          
          ;; KAWA 112 fixes (2 here)
          ;; generate createNode code that would in turn generate all the children
          ;; object associated with it
;          (map js-node-code
;               ;; get a list of the node ID
;               (map (lambda (e) (car e)) node-lst))
          (let ((real-node-lst (map (lambda (e) (car e)) node-lst)))
            (map js-node-code real-node-lst))
          
          ;(list "\n") ;; leave a space before fact code
          
          ;; generate createFact code
;          (map js-fact-code
;               (map (lambda (e) (car e)) fact-lst))
          (let ((real-fact-lst (map (lambda (e) (car e)) fact-lst)))
            (map js-fact-code real-fact-lst))
               
          
          ;; config from properties menu
          
          (list
           "\n"
           (if (disable-back-button?)
               "write_config_flag( 'back_button_flag', false );\n"
               "write_config_flag( 'back_button_flag', true );\n" )

           (if (disable-restart-button?)
               "write_config_flag( 'restart_button_flag', false );\n"
               "write_config_flag( 'restart_button_flag', true );\n" )
           
           (if (disable-pagebreak?)
               "write_config_flag( 'page_flipping_mode', false );\n"
               "write_config_flag( 'page_flipping_mode', true );\n" )
           )
           
          ;; resize
           (if (disable-page-resize?)
               (list 
                "write_config_flag( 'window_resize_flag', false );\n"
                ;; TODO check whether string is numeric
                (string-append "fixed_page_width = " (get-fixed-page-width) ";\n")
                (string-append "fixed_page_height = " (get-fixed-page-height) ";\n")
               )
               (list "write_config_flag( 'window_resize_flag', true );\n" ))
          
          (list "}")
          )))

(define (write-jscode-to filename jscode)
  (display "write jscode to ")(display filename)(newline);
  ; first check for overwrite
  (define safetoproceed #t)
  (define overwrite
    (try-catch
        (file-exists? filename)
      (ex <java.lang.Throwable>
          (begin
            (display (*:toString ex))(newline)
            (set! safetoproceed #f)
            ))))
  ;; if necessary, delete the existing file
      (format #t "overwrite: ~a, safetoproceed: ~a~%~!" overwrite safetoproceed)
  (if safetoproceed
      (begin
        (if overwrite
            (try-catch
                (delete-file filename)
              (ex <java.lang.Throwable>
                  (begin
                    (display (*:toString ex))(newline)
                    (set! safetoproceed #f)
                    ))))

        ;; now go ahead and write
        (if safetoproceed
            (begin
              (format #t "safetoproceed: ~a~%~!" safetoproceed)
              (let ((output-port
                     (try-catch
                         (open-output-file filename)
                       (ex <java.lang.Throwable>
                           (begin
                             (display (*:toString ex))(newline)
                                        ;(*:printStackTrace ex)
                             (set! safetoproceed #f)
                             )))))
                ;(format #t "output-port: ~a~a~%~!" output-port (is-void? output-port))
                (display "output port ")(display output-port)(newline)
                (if safetoproceed
                    (begin
                      (format #t "writing~%~!")
                      ;(write jscode output-port)
                      (with-output-to-file filename (lambda () (display jscode)))
                      (close-output-port output-port)
                      #t)
                    #f)))))))

;;;; Autosave

(define (get-hypedyn-folder-string)
  (string-append (get-system-property "user.home") "/Documents/HypeDyn"))
(define (get-hypedyn-folder-file)
  (make-file (get-hypedyn-folder-string)))

(define (check-hypedyn-folder)
  (if (not (file-exists? (get-hypedyn-folder-file)))
      (create-directory (get-hypedyn-folder-file))))

(define (hd-autosave)
  ;; make sure we have an autosave folder in our hypedyn folder
  (let* ((autosave-folder-string (string-append (get-hypedyn-folder-string) "/autosave"))
         (autosave-folder-file (make-file autosave-folder-string)))
    (check-hypedyn-folder)
    (if (not (file-exists? autosave-folder-file))
        (create-directory autosave-folder-file))

    (ht-save-to-file (path (string-append autosave-folder-string "/autosave.dyn")) #t))
  )

;;
;; export to HypeDyn 2 - put this here for now, later make it a standalone app
;;

; first run through the data structure and create a hashtable that corresponds to the HypeDyn 2 format, then
; write to a json file using https://notabug.org/PangolinTurtle/json-r7rs

(define (build-node-hash the-node)
    (let ((the-node-hash (make-hash-table)))
        (hash-table-set! the-node-hash 'id (ask the-node 'ID))
        (hash-table-set! the-node-hash 'name (ask the-node 'name))

        ; content (text and links)
        (hash-table-set! the-node-hash 'content
                         (let ((the-content-hash (make-hash-table)))
                             (hash-table-set! the-content-hash 'text (ask the-node 'content)) ;(escape-special (ask the-node 'content))) ; does this need to be escaped?
                             (hash-table-set! the-content-hash 'rulesets
                                              (let ((the-links (ask the-node 'links)))
                                                  (if the-links
                                                      (map (lambda (this-linkID)
                                                               (build-ruleset-hash this-linkID))
                                                           the-links)
                                                      '())))
                             the-content-hash))

        ; node rules
        (hash-table-set! the-node-hash 'rules (build-rules-hashlist the-node))

        ; is it the start node?
        (hash-table-set! the-node-hash 'isStart (eq? (get-start-node) (ask the-node 'ID)))

        the-node-hash))

(define (build-ruleset-hash the-linkID)
    (let ((the-link (get 'links the-linkID))
          (the-ruleset-hash (make-hash-table)))
        (hash-table-set! the-ruleset-hash 'id the-linkID)
        (hash-table-set! the-ruleset-hash 'name (ask the-link 'name))
        (hash-table-set! the-ruleset-hash 'start (ask the-link 'start-index))
        (hash-table-set! the-ruleset-hash 'end (ask the-link 'end-index))
        (hash-table-set! the-ruleset-hash 'rules (build-rules-hashlist the-link))

        the-ruleset-hash))

(define (build-rules-hashlist the-rule-container)
    (let ((the-rules (ask the-rule-container 'rule-lst)))
        (if the-rules
            (map (lambda (this-ruleID)
                     (let ((the-rule (get 'rules this-ruleID))
                           (the-rule-hash (make-hash-table)))
                         (hash-table-set! the-rule-hash 'id this-ruleID)
                         (hash-table-set! the-rule-hash 'name (ask the-rule 'name))
                         (hash-table-set! the-rule-hash 'stopIfTrue (ask the-rule 'fall-through?))
                         (hash-table-set! the-rule-hash 'conditionsOp (symbol->string (ask the-rule 'and-or)))
                         (hash-table-set! the-rule-hash 'conditions (build-conditions-hashlist the-rule))
                         (hash-table-set! the-rule-hash 'actions (build-actions-hashlist the-rule))
                         the-rule-hash))
                 the-rules)
            '())))

(define (build-conditions-hashlist the-rule)
    (let ((the-conditions (ask the-rule 'conditions)))
        (if the-conditions
            (map (lambda (this-conditionID)
                     (let* ((this-condition (get 'conditions this-conditionID))
                            (ruleID (ask this-condition 'ruleID))
                            (cond-type (ask this-condition 'type))
                            (cont-type-string
                                (case cond-type
                                    ((0) "NodeCondition") ;; node
                                    ((1) "LinkCondition") ;; link
                                    ((2) "BooleanFactValue") ;; boolean fact
                                    ((3) "IntegerFactComparison") ;; number fact
                                    ))
                            (target-type-symbol
                                (case cond-type
                                    ((0) 'node) ;; node
                                    ((1) 'link) ;; link
                                    ((2) 'fact) ;; boolean fact
                                    ((3) 'fact) ;; number fact
                                    ))
                            (target-type-string
                                (case cond-type
                                    ((0) "node") ;; node
                                    ((1) "link") ;; link
                                    ((2) "booleanFact") ;; boolean fact
                                    ((3) "integerFact") ;; number fact
                                    ))
                            (func-target-id (ask this-condition 'targetID))
                            (func "") ; set below
                            (negate (case (ask this-condition 'operator)
                                      ((0)
                                       (cond
                                        ((= cond-type 0)
                                            (set! func "not visited"))
                                        ((= cond-type 1)
                                            (set! func "not followed"))
                                        ((= cond-type 2)
                                         (set! func "false")))
                                        "true")
                                      ((1)
                                       (cond
                                           ((= cond-type 0)
                                            (set! func "visited"))
                                           ((= cond-type 1)
                                            (set! func "followed"))
                                           ((= cond-type 2)
                                            (set! func "true")))
                                       "false")
                                      ((2)
                                       (set! func "is previous")
                                       "false")
                                      ((3)
                                       (set! func "is not previous")
                                       "true")
                                      (else "false")
                                      ))
                            (the-condition-hash (make-hash-table)))

                         (hash-table-set! the-condition-hash 'conditionType cont-type-string)
                         (hash-table-set! the-condition-hash 'params
                                          (let ((the-param-hash (make-hash-table)))
                                              ; "target" param
                                              (let ((target-param (make-hash-table)))
                                                    (hash-table-set! target-param 'value func-target-id)
                                                    (hash-table-set! target-param 'type target-type-string)

                                                  (hash-table-set! the-param-hash target-type-symbol target-param))

                                              ; "status"/"state"/value param
                                              (if (not (= cond-type 3))
                                                  (let ((status-param (make-hash-table)))
                                                        (hash-table-set! status-param 'type "selectedListValue")
                                                        (hash-table-set! status-param 'value func)

                                                      (hash-table-set! the-param-hash (if (= cond-type 2) 'state 'status) status-param))

                                                  ;integer fact here
                                                  (let* ((comparison-param (make-hash-table))
                                                         (operator-param (make-hash-table))
                                                         (value-param (make-hash-table))
                                                         (args-lst (ask this-condition 'numfact-args))
                                                         (comparator (car args-lst))
                                                         (operand-type (cadr args-lst))
                                                         (operand-choice (caddr args-lst)))
                                                      ; comparisonValue
                                                      (hash-table-set! comparison-param 'type "union")
                                                      (hash-table-set! comparison-param 'value (if (equal? operand-type "Input")
                                                                                                   "input"
                                                                                                   "otherFact"))
                                                      (hash-table-set! the-param-hash 'comparisonValue comparison-param)

                                                      ; operator (need to do a bit of adjustment first...)
                                                      (if (equal? comparator "=")
                                                          (if (equal? negate "true")
                                                              (set! comparator "!=")
                                                              (set! comparator "==")))
                                                      (hash-table-set! operator-param 'type "selectedListValue")
                                                      (hash-table-set! operator-param 'value comparator)
                                                      (hash-table-set! the-param-hash 'operator operator-param)

                                                      (hash-table-set! value-param 'type (if (equal? operand-type "Input")
                                                                                             "integer"
                                                                                             "integerFact"))
                                                      (hash-table-set! value-param 'value (string->number operand-choice))
                                                      (hash-table-set! the-param-hash
                                                                       (if (equal? operand-type "Input")
                                                                                           'input
                                                                                           'otherFact)
                                                                       value-param)))
                                              the-param-hash))
                         the-condition-hash))
                 the-conditions)
            '())))

(define (build-actions-hashlist the-rule) ; xxx
    (let ((the-actions (ask the-rule 'actions)))
        (if the-actions
            (map (lambda (this-actionID)
                     (let* ((this-action (get 'actions this-actionID))
                            (expr (ask this-action 'expr))
                            (func-symbol (car expr))
                            (func-string (case func-symbol
                                ((follow-link) "LinkTo")
                                ((show-in-popup) "ShowPopupNode")
                                ((assert retract) "UpdateBooleanFact")
                                ((set-value!) "UpdateStringFact")
                                ((add-anywhere-link) "EnableAnywhereLinkToHere")
                                ((show-disabled-anywhere-link) "ShowDisabledAnywhereLink")
                                ((replace-link-text) "UpdateText")
                                ((set-number-fact) "UpdateIntegerFacts")))
                            (the-action-hash (make-hash-table)))
                         (hash-table-set! the-action-hash 'actionType func-string)
                         (hash-table-set! the-action-hash 'params
                                          (let ((the-param-hash (make-hash-table)))

                                              (format #t "*********** actions expr: ~a  ~%~!" expr)

                                              (case func-symbol
                                                  ((follow-link)
                                                   (let ((value-hash (make-hash-table)))
                                                       (hash-table-set! value-hash 'type "node")
                                                       (hash-table-set! value-hash 'value (list-ref expr 4))
                                                       (hash-table-set! the-param-hash 'node value-hash)))
                                                  ((show-in-popup)
                                                   (let ((value-hash (make-hash-table)))
                                                       (hash-table-set! value-hash 'type "node")
                                                       (hash-table-set! value-hash 'value (list-ref expr 1))
                                                       (hash-table-set! the-param-hash 'node value-hash)))
                                                  ((assert)
                                                   (let ((target-hash (make-hash-table))
                                                         (value-hash (make-hash-table)))
                                                       (hash-table-set! target-hash 'type "booleanFact")
                                                       (hash-table-set! target-hash 'value (list-ref expr 1))
                                                       (hash-table-set! the-param-hash 'fact target-hash)

                                                       (hash-table-set! value-hash 'type "selectedListValue")
                                                       (hash-table-set! value-hash 'value "true")
                                                       (hash-table-set! the-param-hash 'value value-hash)))
                                                  ((retract)
                                                   (let ((target-hash (make-hash-table))
                                                         (value-hash (make-hash-table)))
                                                       (hash-table-set! target-hash 'type "booleanFact")
                                                       (hash-table-set! target-hash 'value (list-ref expr 1))
                                                       (hash-table-set! the-param-hash 'fact target-hash)

                                                       (hash-table-set! value-hash 'type "selectedListValue")
                                                       (hash-table-set! value-hash 'value "false")
                                                       (hash-table-set! the-param-hash 'value value-hash)))
                                                  ((set-value!)
                                                   (let ((target-hash (make-hash-table))
                                                         (value-hash (make-hash-table)))
                                                       (hash-table-set! target-hash 'type "stringFact")
                                                       (hash-table-set! target-hash 'value (list-ref expr 1))
                                                       (hash-table-set! the-param-hash 'fact target-hash)

                                                       (hash-table-set! value-hash 'type "string")
                                                       (hash-table-set! value-hash 'value (escape-special (list-ref expr 2)))
                                                       (hash-table-set! the-param-hash 'value value-hash)))
                                                  ((replace-link-text)
                                                   (let* ((type-hash (make-hash-table))
                                                          (value-hash (make-hash-table))
                                                          (type-string (list-ref expr 1)))
                                                       (cond
                                                           ((equal? type-string "alternative text")
                                                            (hash-table-set! type-hash 'type "string")
                                                            (hash-table-set! type-hash 'value (list-ref expr 2)) ; (escape-special (list-ref expr 2))) ; don't escape this?
                                                            (hash-table-set! the-param-hash 'textInput type-hash)

                                                            (hash-table-set! value-hash 'type "union")
                                                            (hash-table-set! value-hash 'value "textInput")
                                                            (hash-table-set! the-param-hash 'text value-hash))
                                                           ((equal? type-string "text fact")
                                                            (hash-table-set! type-hash 'type "stringFact")
                                                            (hash-table-set! type-hash 'value (list-ref expr 2))
                                                            (hash-table-set! the-param-hash 'stringFactValue type-hash)

                                                            (hash-table-set! value-hash 'type "union")
                                                            (hash-table-set! value-hash 'value "stringFactValue")
                                                            (hash-table-set! the-param-hash 'text value-hash))
                                                           ((equal? type-string "number fact")
                                                            (hash-table-set! type-hash 'type "numberFact")
                                                            (hash-table-set! type-hash 'value (list-ref expr 2))
                                                            (hash-table-set! the-param-hash 'NumberFactValue type-hash)

                                                            (hash-table-set! value-hash 'type "union")
                                                            (hash-table-set! value-hash 'value "NumberFactValue")
                                                            (hash-table-set! the-param-hash 'text value-hash)))))

                                                  ;                                                  ((set-number-fact)
                                                  ;                                                   ;                  (string-append "[" (to-string (list-ref expr 1)) ", "
                                                  ;                                                   ;                                 ;; give in number form
                                                  ;                                                   ;                                 (list-ref expr 2)
                                                  ;                                                   ;                                 "]")
                                                  ;
                                                  ;                                                   (define target-factID (list-ref expr 1))
                                                  ;                                                   (define num-fact-mode (list-ref expr 2))
                                                  ;                                                   (define fact-value
                                                  ;                                                       (case num-fact-mode
                                                  ;                                                           (("Input" "Fact") (string-append "[" (to-string (list-ref expr 3)) "]" ))
                                                  ;                                                           (("Math")
                                                  ;                                                            ;; (list op opr1 opr1-type opr2 opr2-type)
                                                  ;                                                            (let* ((op            (list-ref (list-ref expr 3) 0))
                                                  ;                                                                   (operand1      (list-ref (list-ref expr 3) 1))
                                                  ;                                                                   (operand1-type (list-ref (list-ref expr 3) 2))
                                                  ;                                                                   (operand2      (list-ref (list-ref expr 3) 3))
                                                  ;                                                                   (operand2-type (list-ref (list-ref expr 3) 4)))
                                                  ;
                                                  ;                                                                (string-append "[" (quote-nest op) ", "
                                                  ;                                                                               operand1 ", "
                                                  ;                                                                               (quote-nest operand1-type) ", "
                                                  ;                                                                               operand2 ", "
                                                  ;                                                                               (quote-nest operand2-type) "]"
                                                  ;                                                                               ))
                                                  ;                                                           )
                                                  ;                                                           (("Random")
                                                  ;                                                            ;; (list opr1 opr1-type opr2 opr2-type)
                                                  ;                                                            (let* ((operand1      (list-ref (list-ref expr 3) 0))
                                                  ;                                                                   (operand1-type (list-ref (list-ref expr 3) 1))
                                                  ;                                                                   (operand2      (list-ref (list-ref expr 3) 2))
                                                  ;                                                                   (operand2-type (list-ref (list-ref expr 3) 3)))
                                                  ;
                                                  ;                                                                (string-append "["
                                                  ;                                                                               operand1 ", "
                                                  ;                                                                               (quote-nest operand1-type) ", "
                                                  ;                                                                               operand2 ", "
                                                  ;                                                                               (quote-nest operand2-type) "]"
                                                  ;                                                                               ))
                                                  ;                                                           )
                                                  ;                                                           ))
                                                  ;                                                   (string-append "[" (to-string target-factID) ", "
                                                  ;                                                                  (quote-nest num-fact-mode) ", "
                                                  ;                                                                  fact-value
                                                  ;                                                                  "]")
                                                  ;                                                  )
                                                  ;                                                  )
                                                  )
                                              the-param-hash))

                         the-action-hash))
                 the-actions)
            '())))

(define (build-fact-hash the-fact)
    (let ((the-fact-hash (make-hash-table)))
        (hash-table-set! the-fact-hash 'id (ask the-fact 'ID))
        (hash-table-set! the-fact-hash 'name (ask the-fact 'name))
        (let ((fact-type (ask the-fact 'type)))
            (cond
                ((eq? fact-type 'boolean)
                 (begin
                     (hash-table-set! the-fact-hash 'type "bool")
                     (hash-table-set! the-fact-hash 'initialValue #f)))
                ((eq? fact-type 'string)
                 (begin
                     (hash-table-set! the-fact-hash 'type "string")
                     (hash-table-set! the-fact-hash 'initialValue "")))
                ((eq? fact-type 'number)
                 (begin
                     (hash-table-set! the-fact-hash 'type "int")
                     (hash-table-set! the-fact-hash 'initialValue 0)))))

        the-fact-hash))

(define (build-node-position-hash the-node)
    (store-node-positions) ; store the node positions in the graph

    (let ((the-node-hash (make-hash-table)))
        (hash-table-set! the-node-hash 'id (ask the-node 'ID))
        (hash-table-set! the-node-hash 'x (/ (ask the-node 'get-x) 1.0))
        (hash-table-set! the-node-hash 'y (/ (ask the-node 'get-y) 1.0))

        the-node-hash))

(define (export-hypedyn2)
    (easy-try-catch
        (lambda ()
            ;; assuming .dyn is always appended to the filename get-filename-callback returns
            (define file-name
                (let ((tmp (get-saved-filename-string)))
                    (if (not tmp)
                        (set! tmp "Untitled.dyn"))
                    ;; add "2" to the extension
                    (string-append tmp "2")
                    ))
            (let ((exportfilename (get-safe-new-filename (get-last-saved-dir) #f (list ".dyn2") file-name "dyn2")))
                (if (not (eq? #f exportfilename))
                    (begin
                        (if (file-exists? exportfilename)
                            (delete-file exportfilename))

                        ; build the json
                        (let ((the-nodes (get-list 'nodes))
                              (the-links (get-list 'links))
                              (the-facts (get-list 'facts))
                              (the-hash (make-hash-table)))
                            ; story
                            (hash-table-set! the-hash 'story
                                             (let ((the-story (make-hash-table)))
                                                 (hash-table-set! the-story 'title (get-story-title))
                                                 (hash-table-set! the-story 'author (get-author-name))
                                                 (hash-table-set! the-story 'description "")
                                                 (hash-table-set! the-story 'metadata
                                                                  (let ((the-metadata (make-hash-table)))
                                                                      (hash-table-set! the-metadata 'comments (get-story-comment))
                                                                      (let ((css-type (get-css-type)))
                                                                          (cond
                                                                              ((eq? css-type 'default)
                                                                               (hash-table-set! the-metadata 'readerStyle "standard"))
                                                                              ))
                                                                      ; also need to put in the location of the css file, if required
                                                                      (hash-table-set! the-metadata 'backDisabled (disable-back-button?))
                                                                      (hash-table-set! the-metadata 'restartDisabled (disable-restart-button?))
                                                                      the-metadata))

                                                 ; nodes
                                                 (hash-table-set! the-story 'nodes
                                                                  (if the-nodes
                                                                      (map (lambda (this-node)
                                                                               (build-node-hash (cdr this-node)))
                                                                           the-nodes)
                                                                      '()))

                                                 ; facts
                                                 (hash-table-set! the-story 'facts
                                                                  (if the-facts
                                                                     (map (lambda (this-fact)
                                                                              (build-fact-hash (cdr this-fact)))
                                                                          the-facts)
                                                                     '()))

                                                 ; rules - there aren't any document rules in HypeDyn 1 so this will always be empty
                                                 (hash-table-set! the-story 'rules '())

                                                 the-story))

                            ; plugins
                            (hash-table-set! the-hash 'plugins
                                             (let ((the-plugins (make-hash-table)))
                                                 (hash-table-set! the-plugins (string->symbol "Default Story Viewer")
                                                                  (let ((default-story-viewer (make-hash-table)))
                                                                      (hash-table-set! default-story-viewer 'nodes
                                                                                       (if the-nodes
                                                                                               (map (lambda (this-node)
                                                                                                        (build-node-position-hash (cdr this-node)))
                                                                                                    the-nodes)
                                                                                               '()))
                                                                      (hash-table-set! default-story-viewer 'zoomLevel 1.0)
                                                                      default-story-viewer))
                                                 the-plugins))

                            (format #t "export-to-hypedyn2 hashtable: ~a  ~%~!" the-hash)
                            (define jstring (json-write-string the-hash #t))
                            (format #t "export-to-hypedyn2 json: ~a  ~%~!" jstring)

                            ; write to file
                            (json-write-file the-hash exportfilename #t))))))))



;
;
;
;            ; run through all nodes and generate sexpr
;            (ht-build-sexpr-from-objectlist-with-rules the-nodes)
;
;            ; then run through all the links and generate sexpr
;            (ht-build-sexpr-from-objectlist-with-rules the-links)
;
;            ; record start node, if any
;            (if (get-start-node)
;                (list
;                    (list 'set-start-node! (get-start-node)))
;                '())
;
;            ; run through all facts and generate sexpr
;            (ht-build-sexpr-from-objectlist the-facts)
;
;            ; save document rule, if any
;        (if (has-document-rule?)
;            (list (ht-build-sexpr-from-rule (get-document-ruleID)))
;            '())
;
;        ; plugin data
