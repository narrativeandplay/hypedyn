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

; UI for a hypertext node
(begin
  (require "../kawa/miscutils.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/color.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/ui/cursor.scm")
  (require "../kawa/ui/splitpane.scm")
  (require "../kawa/ui/undo.scm")
  (require "../kawa/graphics-kawa.scm") ; for open-image-file
  (require "objects.scm")
  (require "datatable.scm") ;; get
  (require "links.scm")
  (require 'srfi-1))

; export
(module-export make-hypertextpane make-hypertextpane-readonly
               set-attribute-linkAction get-attribute-linkAction get-attribute-linkAction-pos
               set-attribute-linkID get-attribute-linkID get-attribute-linkID-pos)
(module-static 'init-run)

; make a hypertext pane
; selectlink-callback is called when a link is selected, takes a linkID
; deletelink-callback is called when a link is deleted, takes a linkID
; enable-newlink-button-callback is called when a link is able to be 
;   created (or not) based on the overlap of selections, takes a boolean
; gettext-method is the name of the method to call in the 
;   associated node to get its text, takes a symbol
; getlinks-method is the name of the method to call in the 
;   associated node to get its links, takes a symbol
; nodelist-name is the name of the list in the data table containing the
;   list of nodes, ie. 'patterns or 'nodes, used to retrieve the associated node
(define (make-hypertextpane w h
                            selectlink-callback 
                            deletelink-callback
                            enable-newlink-button-callback
                            gettext-method
                            getlinks-method
                            nodelist-name)
  (let* ((named-obj (new-make-named-object "hpane"))
         (this-obj (new-object named-obj))
         ;; UI elements
         (the-editor #f)
         (the-doc #f)

         ;; flags
         (dirty #f)
         (track-dirty #f)
         (track-links #f)

         ;; data
         (the-nodeID #f)

         ;; mouseover callback
         (mouseover-callback #f)

         ;; dirty display update callback: used to indicate that text has changed
         (update-dirty-callback #f)

         ;; last mouse event, hack for user study to trigger tooltipmanager to show tooltip
         (lastmousemove #f)

         ;; undo handling
         (track-undoable-edits #f)
         (undo-manager #f)
         (undo-action #f)
         (redo-action #f)
         (re-edit-node-callback #f)

         ;;        ;; compound action tracking
         ;;        ( (list))
         )
    
    ; initialize the hypertexteditor
    (define (init)
      ; build the ui
      (buildui))

    ; build the UI
    (define (buildui)
      ;; add a splitpanel for link list and text editor

      ; make text editor for content
      (set! the-editor (make-textpane-with-background-image))
      (add-caretlistener the-editor
                         (make-caretlistener cursor-handler))
      (add-documentlistener the-editor
                            (make-documentlistener document-insert-handler
                                                   document-remove-handler
                                                   document-changed-handler))
      (add-documentfilter the-editor
                          (make-documentfilter document-filter-insert-string-handler
                                               (lambda (fb offset len)
                                                 (document-filter-remove-handler fb 
                                                                                 offset 
                                                                                 len))
                                               (lambda (fb offset len string attr)
                                                 (document-filter-replace-handler fb 
                                                                                  offset 
                                                                                  len 
                                                                                  string 
                                                                                  attr))))
      (add-keylistener the-editor (make-keylistener key-press-handler
                                                    key-type-handler
                                                    key-release-handler))

      ; get the doc for editor
      (set! the-doc
            (get-textpane-doc the-editor))

      ; set size for editor - don't hard-code this in the long term
      (set-component-preferred-size the-editor w h)

      ; add mouse listener to editor, for clicking on hyperlinks
      (add-mouselistener the-editor
                         (make-mouselistener the-mousecallback))
      (add-mousemotionlistener the-editor
                               (make-mousemotionlistener the-mousecallback)))
    
    ; flags
    
    ; enable/disable tracking of links
    (define (set-track-links! m)
      (set! track-links m))
    
    ; enable/disable tracking of edits
    (define (set-track-dirty! m)
      (set! track-dirty m))
    
    ; enable/disable tracking of undoable edits
    (define (set-track-undoable-edits! m)
      (set! track-undoable-edits m))
    
    ; track if there have been any edits
    ; need to set this if ANY of the contents have changed
    (define (dirty?)
      dirty)
    (define (clear-dirty!)
      (set! dirty #f)
      (if update-dirty-callback (update-dirty-callback)))
    (define (set-dirty!)
      (if track-dirty
          (begin
            (set! dirty #t)
            (if update-dirty-callback (update-dirty-callback)))))

    ; callback to update display when dirty
    (define (set-update-dirty-callback! in-callback)
      (set! update-dirty-callback in-callback))
    
    ; data
    
    ; set the node ID
    (define (set-nodeID! in-nodeID)
      (set! the-nodeID in-nodeID))
    
    ; which node is being edited
    (define (set-node! in-node)
      (set-nodeID! (ask in-node 'ID))
      
      ;; hack to ensure the pressing enter in textpane does not add "\r\n"
      ;; but just a "\n"
      (settext " ") ;; this seem to do the job
      (settext (ask in-node gettext-method))
      
      (setlinks (ask in-node getlinks-method))
      (set-cursor-pos the-editor 0))
    
    ; clear content
    (define (clear-content!)
      (set! the-nodeID #f)
      (settext "")
      (set-cursor-pos the-editor 0)
      (clear-dirty!))
        
    ;; links in editor text
    ;; 
    
    ; clickback for links
    (define (clickback this-linkID)
      (if (procedure? selectlink-callback)
          (selectlink-callback this-linkID)))

    ; add a link in the editor as an underline
    (define (addlink thislink)
      
      ;; cache the value of track-undoable-edits and set it back later
      (define original-track-undoable-edits track-undoable-edits)
      (set-track-undoable-edits! #f)
      
      (let* ((start-index (ask thislink 'start-index))
             (end-index (ask thislink 'end-index))
             (len (- end-index start-index))
             (this-linkID (ask thislink 'ID)))
        
        ;;TODO should check whether we are setting beyond the document length
        ;; 0 < start-index < doc-len
        ;; len <= doc-len 
        (set-text-style the-doc 
                        style-link ;;DEBUG was style-link
                        start-index
                        len
                        #t)

        ; and set clickback ;;DEBUG! (commented out)
        (let ((link-attribute-set (make-attribute-set)))
          (set-attribute-linkAction link-attribute-set
                                    (lambda ()
                                      (clickback this-linkID)))
          (set-attribute-linkID link-attribute-set this-linkID)
          (set-text-style the-doc 
                          link-attribute-set
                          start-index
                          len
                          #f)
          )
        )
      ;; set back original value
      (set-track-undoable-edits! original-track-undoable-edits)
      )
    
    ; remove a link (underline) from the editor
    (define (removelink thislink)
      ;; cache the value of track-undoable-edits and set it back later
      (define original-track-undoable-edits track-undoable-edits)
      (set-track-undoable-edits! #f)
      
      (let* ((start-index (ask thislink 'start-index))
             (end-index (ask thislink 'end-index))
             (len (- end-index start-index))
             (this-linkID (ask thislink 'ID)))
        
        ;; for debug only
        (define (get-attributes-pos2 in-doc :: <javax.swing.text.DefaultStyledDocument>
                                     pos :: <int>
                                     ) ;:: <javax.swing.text.AttributeSet>
          (let ((charElement :: <javax.swing.text.Element>
                             (invoke in-doc 'getCharacterElement pos)))
            (define gotten-attr (invoke charElement 'getAttributes))
;;            (display "doh ")(display (invoke charElement 'getStartOffset))(newline)
;;            (display "doh2 ")(display (invoke charElement 'getEndOffset))(newline)
;;            (display "doh3 ")(display (invoke charElement 'getAttributes))(newline)
;;            (display "isequal style-link? ")(display (invoke gotten-attr 'isEqual style-link))(newline)
;;            (display "isequal style-nolink? ")(display (invoke gotten-attr 'isEqual style-nolink))(newline)
;;            (display "name ")(display (invoke charElement 'getName))(newline)
            ))
        
;        (display "removelink start index STYLE ")(display (get-attributes-pos2 the-doc start-index))(newline)
;        (display "removelink END index STYLE ")(display (get-attributes-pos2 the-doc (+ start-index len)))(newline)
        (get-attributes-pos2 the-doc start-index)
        (get-attributes-pos2 the-doc (+ start-index len))
        
        (set-text-style the-doc style-nolink
                        start-index
                        len
                        #t)
        
;;        (display "[start index] ")(display start-index)(newline)
;;        (display "[end-index] ")(display end-index)(newline)
;;        (display "[len] ")(display len)(newline)
;;        (display "[this-linkID]")(display this-linkID)(newline)
;;        
;;        (display "[delete link] ")(display (list start-index len (get-text-length the-doc)))(newline)
;;        (display "cond check ")(display (equal? (+ start-index len) (get-text-length the-doc)))(newline)
        
;        (display "removelink start index STYLE ")(display (get-attributes-pos2 the-doc start-index))(newline)
;        (display "removelink END index STYLE ")(display (get-attributes-pos2 the-doc (+ start-index len)))(newline)
        )
      (set-track-undoable-edits! original-track-undoable-edits))
    
    ; rename a link
    (define (renamelink in-linkID in-newname)
      'ok)
    
    ; adjust links after deleting
    ; returns #t if need to manually clean up after link deletion
    (define (adjust-links-delete start len)
      ;(display "adjust-links-delete ")(newline)
      (let ((link-deleted #f)
            (edited-node (get nodelist-name the-nodeID)))
        ; run through each link and adjust
        (map (lambda (l)
               (set! link-deleted (or (adjust-one-link-delete start len l)
                                      link-deleted)))
             (ask edited-node getlinks-method))
        link-deleted))

    ; adjust one link after deletion
    ; returns #t if need to manually clean up after link deletion
    (define (adjust-one-link-delete del-start del-len linkID)
      (let ((link-deleted #f)
            (thislink (get 'links linkID)))
        (if thislink
            (let ((link-start (ask thislink 'start-index))
                  (link-end (ask thislink 'end-index))
                  (del-end (+ del-start del-len)))
              (display "adjust one link delete ")(display (list del-start del-end link-start link-end))(newline)

              ;; how to read the examples aaBBaa  
              ;; a - non link text,  b - link text
              ;; [ - start of deletion, ] - end of deletion
              (cond ((<= del-end link-start) ;; Case 1; del-start del-end link-start link-end (eg [a]aBBaa, [aa]BBaa)
                     ; Entire deletion is before link (shift link)
                     (display "delete case 1 ")(newline)
                     (ask thislink 'set-start-index! (- link-start del-len))
                     (ask thislink 'set-end-index! (- link-end del-len))
                     (set! link-deleted 0))
                    ((<= link-end del-start) ;; Case 2; link-start link-end del-start del-end (eg aaBB[a]a, aaBBa[a]) 
                     ;; Entire deletion after link (DO NOTHING to this link)
                     (display "delete case 2 ")(newline)
                     (set! link-deleted 0))
                    ((and (<= del-start link-start) ;; Case 3; del-start link-start link-end del-end (eg a[aBB]aa, a[aBBa]a, aa[BBa]a, aa[BB]aa)
                          (<= link-end del-end))
                     (display "delete case 3 ")(newline)
                     ;; Entire link is inside deletion, (delete link)
                     
                     ;; link-end = link-start (only for the purpose of \r\n check)
                     ;; normally we dont bother shifting the link since we delete it anyway
                     (ask thislink 'set-end-index! link-start)
                     
                     (set! link-deleted (- link-end link-start))  
                     ; Delete the link
                     (deletelink-callback linkID))
                    ((and (<= del-end link-end)         ;; Case 4; link-start del-start del-end link-end (eg aaB[B]a, aa[B]Baa, aaB[B]Baa) 
                          (<= link-start del-start))    ;; makes sure not the whole link 
                     ;; Entire deletion is inside link but not encompassing it, (shorten link by length of deletion)
                     (display "delete case 4 ")(newline)
                     (display "link end ")(display link-end)(newline)
                     (display "del-len ")(display del-len)(newline)
                     (ask thislink 'set-end-index! (- link-end del-len))
                     (display "new link end ")(display (ask thislink 'end-index))(newline)
                     (set! link-deleted del-len))
                    ((and (< del-start link-start)  ;; Case 5; del-start link-start del-end link-end (eg a[aB]Baa)
                          (< link-start del-end)    ;; link boundaries does not coincides with deletion boundary    
                          (< del-end link-end))
                     (display "delete case 5 ")(newline)
                     ; a portion of the link at the head is deleted (shift link and shorten)
                     (ask thislink 'set-start-index! del-start) ;; move link-start to del-start
                     (ask thislink 'set-end-index! (+ del-start (- link-end del-end))) ; count remaining length of link and 
                                                                                       ; offset that from new link-start (del-start) 
                     (set! link-deleted (- del-end link-start))) ;; the length of head deleted
                    ((and (< link-start del-start)   ;; Case 6; link-start del-start link-end del-end  (eg aaB[Ba]a)
                          (< del-start link-end)     ;; link boundaries does not coincides with deletion boundary
                          (< link-end del-end))
                     (display "delete case 6 ")(newline)
                     ;; cut off an end portion of link (shift link-end)
                     (ask thislink 'set-end-index! del-start) ;; link-end moved to del-start
                     (set! link-deleted (- link-end del-start)))
                    )
              
;              ;; after deletion check whether we're left with only \r and \n in the link text
;              (set! link-start (ask thislink 'start-index))
;              (set! link-end (ask thislink 'end-index))
;              (define link-len (- link-end link-start))
;              
;              (define (delete-string str del-start del-end)
;                (string-append (substring str 0 del-start) 
;                               (substring str del-end (string-length str))))
;              
;              (define content (invoke the-editor 'get-text))
;              (define trunc-content (delete-string content del-start (+ del-start del-len)))
;              (display "content ")(display content)(newline)
;              (display "trunc content ")(display trunc-content)(newline)
;              (display "link start end ")(display (list link-start link-end))(newline)
;              (define link-text (substring trunc-content link-start link-end))
;              
;              ;; assumes newline characters has the most 2 char (ie   text\r\n and not text\r\r\n)
;              
;              (display "sanity check! ")(newline)
;              (display "link len ")(display link-len)(newline)
;              (display "link-text ")(display link-text)(newline)
;              (cond
;               ((= link-len 1)
;                (define char1 (substring link-text 0 1))
;                (if (or (equal? char1 "\r")
;                        (equal? char1 "\n"))
;                    (deletelink-callback linkID)))
;               ((= link-len 2)
;                (define char1 (substring link-text 0 1))
;                (define char2 (substring link-text 1 2))
;                (if (and (or (equal? char1 "\r")
;                             (equal? char1 "\n"))
;                         (or (equal? char2 "\r")
;                             (equal? char2 "\n")))
;                    (deletelink-callback linkID))
;                ))
              
              ))
        (display "end of adjust one link delete ")(newline)
        link-deleted))

    ; adjust links after inserting
    (define (adjust-links-insert start len is-undo-action? extend-len)
      (display "adjust-links-insert ")(newline)
      (let ((edited-node (get nodelist-name the-nodeID)))
        ; run through each link and adjust
        (map (lambda (l)
               (adjust-one-link-insert start len l is-undo-action? extend-len))
             (ask edited-node getlinks-method))))

    ; adjust one link after insertion
    ; Note: if extend-len is more than 0, extend the end of link 
    (define (adjust-one-link-insert ins-start ins-len linkID is-undo-action? extend-len)
      (display "adjust one link insert ")(newline)
      (let ((thislink (get 'links linkID)))
        (if thislink
            (let ((link-start (ask thislink 'start-index))
                  (link-end (ask thislink 'end-index))
                  (ins-end (+ ins-start ins-len)))
              (display "ins len ")(display ins-len)(newline)
              (display "in adjust one link insert ")(display (list link-start link-end))(newline)
              (display "ins-start ins-end ")(display (list ins-start ins-end))(newline)
  
              (if (not is-undo-action?)
                  (cond ((and (<= ins-start link-start) ) ;; Case 1 : ins before link (eg aa[i]LLLaa, a[i]aLLLaa)
                         (display "insert case 1 insert before link (just shift link) ")(newline)
                         (display "link start end ")(display (list link-start link-end))(newline)
                         ;; just shift link without changing length
                         (ask thislink 'set-start-index! (+ link-start ins-len))
                         (ask thislink 'set-end-index! (+ link-end ins-len)))
                        ((and (<= link-start ins-start) (<= ins-start link-end) ) ;; Case 2 : ins within link (eg aaL[i]LLaa)
                         (display "insert case 2 (within link) ")(newline)
                         ;; lengthen link no matter what (do i? seems like this commenting extend-len check was commented out
                         ;; to fix a bug
                         (if (> extend-len 0)
                             (ask thislink 'set-end-index! (+ link-end ins-len))
                             )
                         )
                        ((and (= link-end ins-start) )   ;; Case 3b : ins RIGHT after link end (eg aaLLL[i]aa)
                         (display "insert case 3 (ins right after link) ")(newline)
                         (display "extend-len ")(display extend-len)(newline)
                         (if (> extend-len 0) ;; used by insert-blank-space (custom call that does not extend link len ie extend-len is 0)
                             (ask thislink 'set-end-index! (+ link-end ins-len)))) ;; lengthen link by ins-len
                        ((= link-end ins-start) ;; Case 3a: ins after link (do nothing) (eg aaLLLa[i]a)
                         (display "INSERT AFTER LINK")(newline)
                         #f))
                  ;; undo-actions 
                  ;; if extend-len is 0 means the insertion makes no extension to this link
                  (cond ((and (<= ins-start link-start) ) ;; insert before link (undo deletion case 1 and 5)
                         (display "insert case 1 insert before link version UNDO ")(newline)
                         ;; do the same for non undo case 1
                         (ask thislink 'set-start-index! (+ link-start ins-len))
                         (ask thislink 'set-end-index! (+ link-end ins-len))
                         (if (> extend-len 0) ;; Case 1 special : undoing deletion of head of link
                             (begin           ;; adding the length of deleted (extend-len) to the start
                               (set! link-start (ask thislink 'start-index))
                               (ask thislink 'set-start-index! (- link-start extend-len)))
                             (begin
                               #f
                               )
                             ))
                        ((and (<= link-start ins-start) (<= ins-start link-end) ) ;; Case 2 ins within link (same as non undo)
                         (display "insert (delete undo) case 2 ")(newline)
                         (if (> extend-len 0) ;; redundant check since deletion of link within will always return positive deletion length
                             (ask thislink 'set-end-index! (+ link-end extend-len))))
                        ((and (<= link-end ins-start) (> extend-len 0)) ;; Case 3 ins after link : (undo deletion case 2 and 6)
                         (display "delete undo case 3 ")(display extend-len)(newline)
                         (if (> extend-len 0) ;; undo deletion of tail of link
                             (ask thislink 'set-end-index! (+ link-end extend-len)))) ;; lengthen link by extend-len (instead of ins-len in normal case)
                        ))
              ))))

    ; after editing position is set, determine whether new link is allowed
    ; (may need to check this after insert/delete separately)
    (define (check-links-overlap sel-start sel-end)
      (let ((edited-node (get nodelist-name the-nodeID)))
        ; run through each link and adjust
        (let ((overlaps (any (lambda (l)
                               (check-link-overlap sel-start sel-end l))
                             (ask edited-node getlinks-method))))
          (if (or overlaps (= sel-start sel-end))
              (begin
                (if (procedure? enable-newlink-button-callback)
                    (enable-newlink-button-callback #f)))
              (begin
                (if (procedure? enable-newlink-button-callback)
                    (enable-newlink-button-callback #t)))))))

    ; check if selection overlaps with given link, returns #t if overlaps
    ; doesn't overlap:  if (sel-end < link-start or sel-start > link-end))
    (define (check-link-overlap sel-start sel-end linkID)
      (let ((thislink (get 'links linkID)))
        (if thislink
            (let ((link-start (ask thislink 'start-index))
                  (link-end (ask thislink 'end-index)))
              (let ((overlaps (not (or (<= sel-end link-start) (<= link-end sel-start)))))
                overlaps))
            #f)))

    ; check if we're at the end of a link:
    ; not a zero-length document, not at the first position,
    ; previous position is a link, and either at the
    ; end, or next position is not the same link
    (define (end-of-link? pos)
      (let ((text-length (get-text-length the-doc)))
        (and (> text-length 0)
             (> pos 0)
             (is-link? (- pos 1))
             (or (= pos text-length)
                 (not (eq? (get-attribute-linkAction-pos the-doc (- pos 1))
                           (get-attribute-linkAction-pos the-doc pos)))))))

    ; check if we're at the start of a link:
    ; not a zero-length document, not at the end,
    ; next position is a link, and either at the
    ; start, or previous position is not the same link
    (define (start-of-link? pos)
      (let ((text-length (get-text-length the-doc)))
        (and (> text-length 0)
             (< pos text-length)
             (is-link? pos)
             (or (= pos 0)
                 (not (eq? (get-attribute-linkAction-pos the-doc (- pos 1))
                           (get-attribute-linkAction-pos the-doc pos)))))))

    ; insert a blank character with no link attached
    (define (insert-blank-space pos)
      ; note: for some reason need a hack here, insert 2 characters and then
      ; delete the second one, otherwise the style doesn't go away - alex
      (set-track-links! #f)
      
      (start-compound-undoable-event "End Linktext Extend")
      
      ;DEBUG
      ;(set-text-insert-attr the-doc "  " pos style-nolink)
        ;(set-text-insert-attr the-doc "  " pos style-link)
      ;(set-text-delete the-doc (+ pos 1) 1)
      
      ;; no need to do the above hack anymore for some unknown reason
      (set-text-insert-attr the-doc " " pos style-nolink)
      
      (adjust-links-insert pos 1 #f 0)
      (end-compound-undoable-event "End Linktext Extend")
      
      (set-track-links! #t))
    
    ; get selection start
    (define (getselstart)
      (get-text-selstart the-editor))

     ; get selection end
    (define (getselend)
      (get-text-selend the-editor))

     ; get the hypertextnode's text
    (define (gettext)
      (get-text the-editor))
    
    ; get a section of the text
    (define (gettextsection in-start in-end)
      (get-text-section the-editor
                        in-start
                        (- in-end in-start)))

    ; get the selected text
    (define (getselectedtext)
      (get-text-section the-editor 
                        (getselstart) 
                        (- (getselend) (getselstart))))

    ; need to add set content
    (define (settext in-text)
      (set-track-links! #f)
      (set-track-dirty! #f)
      (set-track-undoable-edits! #f)
      (set-text-default-style the-editor style-nolink #t) ;;DEBUG
      ;(set-text-default-style the-editor style-link #t)
      
      (set-text the-editor in-text)
      (set-text-selection the-editor 0 0)
;      (set-text-style the-doc style-nolink 0
;                      (+ (get-text-length the-doc) 1) #t)
      
      (set-track-undoable-edits! #t)
      (set-track-links! #t)
      (set-track-dirty! #t)
      (clear-dirty!))

    ; set selection
    (define (setselection in-selstart in-selend)
      (set-text-selection the-editor in-selstart in-selend))
    
    ; set links
    (define (setlinks in-linklist)
      (map (lambda (l)
             (let ((thislink (get 'links l)))

               ; highlight text
               (addlink thislink)))
           in-linklist))

    ; handle cursor change
    ; note: cursor position is the moving cursor position
    ; and selection end is the other, anchored end, so 
    ; cursor position may be > selection end (unlike mred)
    (define (cursor-handler e)
      (begin
        (let((selstart (get-cursor-pos e))
             (selend (get-selection-end e)))
          (after-set-position (min selstart selend) (max selstart selend)))))

    ; handle key press
    (define (key-press-handler e)
      ; if ctrl-space pressed, and nothing selected, check if its at end of a link,
      ; and if it is, insert a blank space with no link
      (if (and (ctrl-key-modifier? e)
               (or (space-key-event? e)
                   (forward-slash-key-event? e)))
          (let ((sel-start (get-text-selstart the-editor))
                (sel-end (get-text-selend the-editor)))
            ; is nothing selected?
            (if (= sel-start sel-end)
                  ; are we at end of a link?
                  (if (end-of-link? sel-start)
                      ; insert a blank character at the selection point
                      (insert-blank-space sel-start))))))
    
    ; handle key typed
    (define (key-type-handler e)
      'ok)

    ; handle key released
    (define (key-release-handler e)
      'ok)

    ;
    ; define document filter handlers
    ;

    ; note: these are called BEFORE the document is changed, and its safe to
    ; use the filter bypass to make changes to the document

    ; handle insert string
    (define (document-filter-insert-string-handler fb offset string attr)
      ;(format #t "document-filter-insert-string-handler~%~!")
      (display "INSERT FILTER ")(newline)
      (display "inside insert filter ")(display (list offset string))(newline)
      (set! replace-event #f)
      ; start compound event
      
      (start-compound-undoable-event "Typing(insert)")
      #t)

    ; handle remove
    ; note: extra parameter (callback) is added when doc filter is created
    (define (document-filter-remove-handler fb offset len)
      ;(format #t "document-filter-remove-handler~%~!")
      (display "REMOVE FILTER ")(newline)
      (display "inside remove filter ")(display (list offset len))(newline)
      ; start compound event
      (start-compound-undoable-event "Typing(remove)")
      (set! replace-event #f)
      
      ;; special handling for new line (line feed) deletion
;      (define content (invoke the-editor 'get-text))
;      (if (<= (+ offset 2) (string-length content)) ;; make sure we dont overshoot in case of only \n
;          (begin
;            (define possible-rn (substring content offset (+ offset 2)))
;            (display "[LAST 2 CHAR after DELETION] ")(display possible-rn)(newline)
;            (if (equal? possible-rn "\r\n")
;                (set! len (+ len 1)))
;            ))
      
;      (display "removed newline? ")(newline)
;      (define frag (substring content offset (+ offset len)))
;      (display "frag ")(display frag)(newline)
;      (display "return? ")(display (equal? frag "\r"))(newline)
;      (display "newline? ")(display (equal? frag "\n"))(newline)

;      ; recalculate link positions
      (after-delete offset len)
      
      (display "after after-delete")(newline)
      #t)

    ; handle replace (called when we do inserts or replaces operations)
    ; note: extra parameter (callback) is added when doc filter is created
    (define (document-filter-replace-handler fb offset len string attr)
      ;(format #t "document-filter-replace-handler~%~!")
      (display "REPLACE FILTER ")(newline)
;      (display "string leng ")(display (string-length string))(newline)
;      (display "newline? ")(display (equal? string "\n"))(newline)
;      (display "return? ")(display (equal? string "\r"))(newline)
;      (display "nr? ")(display (equal? string "\n\r"))(newline)
;      (display "rn? ")(display (equal? string "\r\n"))(newline)
;      (newline)
;      
;      (define content (invoke the-editor 'get-text))
;      (display "string len ")(display (string-length content))(newline)
;      (display "content ")(display content)(newline)
;      (display "rn check ")(display (equal? "\r\n" content))(newline)
;      (display "rnrn check ")(display (equal? "\r\na\r\n" content))(newline)
;      (display "rnn check ")(display (equal? "\r\n\n" content))(newline)
;      (display "rnr check ")(display (equal? "\r\n\r" content))(newline)
;      (newline)
      ; start compound event
      (start-compound-undoable-event "Typing(replace)")
      (set! replace-event #t) ;; hack to get replace to work properly
      (after-delete offset len)
      
;      ;; debug
;      (display "inside replace filter ")(display (list offset len string))(newline)
;      (display "txt len b4 inserting ")(display (string-length (invoke the-editor 'get-text)))(newline)
      
      ; handle formatting ourselves: (bolding and unlining of link text)
      ; if its the END of a link, or (its a LINK and it isn't the START of a link), use the
      ; formatting from the previous position, otherwise use no formatting
      
      ;; NOTE there is a way to differentiate between replace and non replace
      ;; replace has positive len variable while insert has 0 len
      ;; differentiate between replacing and insert
      (if (> len 0) ;; replace
          (begin
            (display "filter bypass replacing ")(newline)
            (cond ((and (or (end-of-link? offset)
                            (and
                             (is-link? offset)
                             (not (start-of-link? offset))))
                        (not (= offset 0)))
                   (filter-bypass-replace fb offset len string
                                         (get-attributes-pos the-doc (- offset 1))
                                         ))
                  (else (filter-bypass-replace fb offset len string style-nolink))
                  )
            )
          ;; insert
          (begin
            (display "filter bypass inserting ")(newline)
            (cond ((and (or (end-of-link? offset)
                            (and
                             (is-link? offset)
                             (not (start-of-link? offset))))
                        (not (= offset 0)))
                   (filter-bypass-insert fb offset string
                                         (get-attributes-pos the-doc (- offset 1))
                                         ))
                  (else (filter-bypass-insert fb offset string style-nolink))
                  )
                ))
      ;(display "txt len aft inserting ")(display (string-length (invoke the-editor 'get-text)))(newline)
      #f)

    ;; for debug
    (define (print-update-level txt)
      (display (string-append txt " "))
      (display (compoundundomanager-updatelevel undo-manager))
      (newline)
      )
    
    ; define document listeners
    ; note: DON'T make changes to the document from these handlers!

    ; handle insert
    (define (document-insert-handler e)
      ;(format #t "document-insert-handler~%~!")
      
      ; recalculate link positions
      (let ((change-length (get-change-length e))
            (change-offset (get-change-offset e)))
        (display "[Handler insert] ")(newline)
        (display "  change-length ")(display change-length)(newline)
        (display "  change-offset ")(display change-offset)(newline)
        
        (define content (invoke the-editor 'get-text))
        (define new-str-frag (substring content
                                        change-offset (+ change-offset change-length)))
        
;        (display "new frag ")(display new-str-frag)(newline)
;        (display "return? ")(display (equal? new-str-frag "\r"))(newline)
;        (display "newline? ")(display (equal? new-str-frag "\n"))(newline)
        
        ;; special case for line feed (pressed enter)
        ;; change-length will return one but in actual fact we are inserting 2 character
        ;; "\r\n"
        
        ;; sometimes it is just \n sometimes \r\n we have to check
;        (if (<= (+ change-offset 2) (string-length content)) ;; make sure we dont overshoot in case of only \n
;            (begin
;              (define possible-rn (substring content change-offset (+ change-offset 2)))
;              (display "[LAST 2 CHAR after insertion] ")(display possible-rn)(newline)
;              (if (equal? possible-rn "\r\n")
;                  (begin
;                    (set! change-length (+ change-length 1))
;                    )
;                  )
;              ))

        
        (after-insert change-offset change-length (undoable-edit? e))
        )

      ; end compound event
      (finalize-compound-undoable-event e "Typing(insert)" #t)
      )

    ;; fix for replace events
    ;; problem is as follows
    ;; insert event - replace-filter, insert-handler
    ;; remove event - remove-filter, remove-handler
    ;; replace event - replace-filter, remove-handler, insert-handler
    ;; for insert events and replace events replace filter is called and it 
    ;; calls start-compound-undoable-event once
    ;; in insert-handler and remove-handler both calls end-compound-undoable-event
    ;; means when replace event occurs we start once and end twice
    ;; to prevent that we flag a replace-event when replace-filter is called and stop 
    ;; remove-handler from doing end-compound-undoable-event (done in finalize-compound-undoable-event)
    (define replace-event #f)
    
    ; handle delete
    (define (document-remove-handler e)
      ;(format #t "document-remove-handler~%~!")
      (display "HANDLER REMOVE ")(newline)
      (display "content len ")(display (string-length (invoke the-editor 'get-text)))(newline)
      ; end compound event
      ;; if replace-event, means that remove-handler was invoked by replace filter 
      ;; dont end compound 
      ;; third arg to finalize-compound-undable-event determines whether it ends compound 
      (finalize-compound-undoable-event e "Typing(remove)" (not replace-event))
      )
    
    ; handle style change
    (define (document-changed-handler e)
      ;(format #t "document-changed-handler~%~!")
      (display "CHANGED HANDLER ")(newline)
      
      ; end compound event
      (finalize-compound-undoable-event e "CHANGED" #t)
;      (end-compound-undoable-event "changed debug ")
;      (print-update-level "changed handler after debug test ")
      )

    ;
    ; called by document and caret handlers
    ;

    ; after-delete
    ; returns #t if need to manually clean up after link deletion
    (define (after-delete start len)
      (set-dirty!)
;      (display " [after delete] ")(newline)
;      (display "   start ")(display start)(newline)
;      (display "   len ")(display len)(newline)
      (if track-links 
          (begin
            ;actually do it
            (define link-len-deleted (adjust-links-delete start len))
            
            ; post the link adjustment actions for undoing
            (compoundundomanager-postedit undo-manager
                                          (make-undoable-edit "after-delete"
                                                              (lambda () 
                                                                ;(display "[after-delete undo] adjust-links-insert ")(newline)
                                                                ;(display "args ")(display (list start len #t link-len-deleted))(newline)
                                                                (adjust-links-insert start len #t link-len-deleted)
                                                                )
                                                              (lambda () 
                                                                ;(display "[after-delete redo] adjust-links-delete ")(newline)
                                                                ;(display "args ")(display (list start len))(newline)
                                                                (adjust-links-delete start len))))
            )
          #f))

    ; after-insert
    (define (after-insert start len can-undo)
;      (display "after insert can undo? ")(display can-undo)(newline)
;      (display "[after insert] ")(newline)
;      (display "  start ")(display start)(newline)
;      (display "  len ")(display len)(newline)
      
      (set-dirty!)
      (if (and 
           track-links
           can-undo)
          (begin
            ; post the link adjustment actions for undoing
            (define insert-undoable-edit
              (make-undoable-edit "after-insert"
                                  (lambda () 
                                    ;(display "[after-insert undo] adjust links delete ")(newline)
                                    ;(display "args ")(display (list start len))(newline)
                                    (adjust-links-delete start len))
                                  (lambda () 
                                    ;(display "[after-insert redo] adjust links delete ")(newline)
                                    ;(display "args ")(display (list start len #f 0))(newline)
                                    (adjust-links-insert start len #f 0))))
            ;(display "here goes ")(display insert-undoable-edit)(newline)
            (compoundundomanager-postedit undo-manager insert-undoable-edit)
            ;(display "after insert here2 ")(newline)

            ; and actually do it
            (adjust-links-insert start len #f len))))

    ; after-set-position
    (define (after-set-position sel-start sel-end)
      (if track-links (check-links-overlap sel-start sel-end)))

    ;
    ; define mouse listener
    ;
    
    ; is there a link at the given position?
    (define (is-link? pos)
      (or
       (check-has-attributes the-doc pos style-followed-link)
       (check-has-attributes the-doc pos style-link)))

    ; handle mouse events for hypertextpane
    (define (the-mousecallback e)
      (let* ((pos (get-text-position-from-point the-editor
                                                (get-mouseevent-x e)
                                                (get-mouseevent-y e)))
             (event-type (get-mouseevent-type e))
             (isLink (is-link? pos)))
        (cond
         ((eq? event-type 'left-clicked)
          (if isLink
              ; link selected, so retrieve link action
              (let ((the-linkAction (get-attribute-linkAction-pos
                                     the-doc pos)))
                ;(display "action: ")(display the-linkAction)(newline)
                (if (not (is-null? the-linkAction))
                    (the-linkAction)))
              ; no link selected, so call selectlink-callback with #f
              (if (procedure? selectlink-callback)
                  (selectlink-callback #f))))
         ((eq? event-type 'motion)
          ; call mouseover action, if any
          (if (procedure? mouseover-callback)
              (begin
                (if isLink (set! lastmousemove (get-mouseevent-rawevent e)))
                (mouseover-callback isLink
                                    (get-attribute-linkID-pos the-doc pos))))))))
    
    ; set mouseover callback
    ; takes 2 parameters: 
    ;   boolean, true if over link
    ;   int, the ID of the link (if any)
    (define (set-mouseover-callback! in-mouseover-callback)
      (set! mouseover-callback in-mouseover-callback))

    ; set background colour: takes a colour list (as defined in ui-kawa.scm)
    (define (set-editor-background-color in-color)
      (set-background-color the-editor (make-colour-from-list in-color)))
    
    ; set background image: takes a filename
    (define (set-editor-background-image in-filename)
      (set-textpane-background-image the-editor
                                     (open-image-file in-filename)))

    ; clear background image
    (define (clear-editor-background-image)
      (clear-textpane-background-image the-editor))
    
    ; undo handling: takes an undo manager, the associated undo/redo actions, and a callback to
    ; make sure that the node being undone is currently being edited
    (define (set-undo-manager! in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)
      (set! undo-manager in-undo-manager)
      (set! undo-action in-undo-action)
      (set! redo-action in-redo-action)
      (set! re-edit-node-callback in-re-edit-node-callback))

    ; tell undo manager to start a compound undoable event
    (define (start-compound-undoable-event in-undo-label)
      ;(display (string-append "[OPEN] start compound undoable event from " in-undo-label))(newline)
      (if (and
           undo-manager
           track-undoable-edits)
          (begin
            ; start the compound edit
            (compoundundomanager-beginupdate undo-manager)
            
            ; make sure that the correct node is being edited for redo
            (let ((undo-nodeID the-nodeID))
              (compoundundomanager-postedit undo-manager
                                            (make-undoable-edit in-undo-label
                                                                (lambda ()
                                                                  (format #t "compound-undoable-edit end of undo~%~!"))
                                                                (lambda () ;; start of redo
                                                                  (format #t "compound-undoable-edit start of redo~%~!")
                                                                  
                                                                  ;; selects the right node and brings up the node editor if need be
                                                                  (re-edit-node undo-nodeID)
                                                                  
                                                                  ;; hack to solve the hanging issue with first character 
                                                                  (if (= (get-text-length the-doc) 0)
                                                                      (begin
                                                                        (set! track-undoable-edits #f)
                                                                        
                                                                        (set-text-style the-doc style-nolink 0 1 #t) ;; correct one
                                                                        (set! track-undoable-edits #t)
                                                                        ))
                                                                  ))))
            
            )))
    
    ; tell the undo manager to end a compound undoable event
    (define (end-compound-undoable-event in-undo-label)
      ;(display (string-append "[CLOSE] start compound undoable event from " in-undo-label))(newline)
      ; make sure that the correct node is being edited for undo
      (let ((undo-nodeID the-nodeID))
        (compoundundomanager-postedit 
         undo-manager
         (make-undoable-edit in-undo-label
                             (lambda ()
                               (format #t "compound-undoable-edit start of undo~%~!")
                               ;; selects the right node and brings up the node editor if need be
                               (re-edit-node undo-nodeID)
                               (display "END OF START UNDO typing ")(newline)
                               )
                             (lambda ()
                               (format #t "compound-undoable-edit end of redo~%~!")))))
      (compoundundomanager-endupdate undo-manager undo-action redo-action)
      )
    
    ;; called by document-insert-handler, document-remove-handler, document-change-handler
    ;; to finalize and add compound typing undoable event
    ;; event-str is the name of the undoable event
    (define (finalize-compound-undoable-event e event-str end-compound?)
;      (display "[finalize compound] ")(newline)
;      (display "  undoable? ")(display (undoable-edit? e))(newline)
;      (display "  track undoable edits ")(display track-undoable-edits)(newline)
;      (display "  event str ")(display event-str)(newline)
      (if (and undo-manager
               (undoable-edit? e) ; this is to avoid posting undo/redo events
               track-undoable-edits)
          (begin
;            (display "posting in finalize compound ")(newline)
            (compoundundomanager-postedit undo-manager e) ; post the compound edit
;            (display "posted the compound edit ")(newline)
            (if end-compound?
                (begin
                  ;(display (string-append "[close] from " event-str))(newline)
                (end-compound-undoable-event event-str)
                  ;(display "ended compound ")(newline)
                  ))
            )))
      
    ; re-edit a node: need to make sure that the hypertextpane is currently being edited
    (define (re-edit-node in-nodeID)
      ; first, make sure we have a procedure to do this from the containing editor
      (if (procedure? re-edit-node-callback)
          (begin
            ; now get the edit back
            (format #t "trying to re-edit node, nodeID=~a~%~!" in-nodeID)
            (re-edit-node-callback in-nodeID))))
    
    ; message handling                  
;    (lambda (message)
;      (cond ((eq? message 'init)
;             (lambda (self)
;               (init)))
    (obj-put this-obj 'init
             (lambda (self) (init)))
;            ((eq? message 'dirty?)
;             (lambda (self)
;               (dirty?)))
    (obj-put this-obj 'dirty?
             (lambda (self) (dirty?)))
;            ((eq? message 'set-dirty!)
;             (lambda (self)
;               (set-dirty!)))
    (obj-put this-obj 'set-dirty!
             (lambda (self) (set-dirty!)))
               
;            ((eq? message 'clear-dirty!)
;             (lambda (self)
;               (clear-dirty!)))
    (obj-put this-obj 'clear-dirty!
             (lambda (self) (clear-dirty!)))
               
;            ((eq? message 'set-track-links!)
;             (lambda (self m)
;               (set-track-links! m)))
    (obj-put this-obj 'set-track-links!
             (lambda (self m) (set-track-links! m)))
;            ((eq? message 'getselstart)
;             (lambda (self)
;               (getselstart)))
    (obj-put this-obj 'getselstart
             (lambda (self) (getselstart)))
;            ((eq? message 'getselend)
;             (lambda (self)
;               (getselend)))
    (obj-put this-obj 'getselend
             (lambda (self) (getselend)))
;            ((eq? message 'gettext)
;             (lambda (self)
;               (gettext)))
    (obj-put this-obj 'gettext
             (lambda (self) (gettext)))
;            ((eq? message 'getselectedtext)
;             (lambda (self)
;               (getselectedtext)))
    (obj-put this-obj 'getselectedtext
             (lambda (self) (getselectedtext)))
;            ((eq? message 'gettextsection)
;             (lambda (self in-start in-end)
;               (gettextsection in-start in-end)))
    (obj-put this-obj 'gettextsection
             (lambda (self in-start in-end)
               (gettextsection in-start in-end)))
;            ((eq? message 'settext)
;             (lambda (self in-text)
;               (settext in-text)))
    (obj-put this-obj 'settext
             (lambda (self in-text)
               (settext in-text)))
;            ((eq? message 'setselection)
;             (lambda (self in-selstart in-selend)
;               (setselection in-selstart in-selend)))
    (obj-put this-obj 'setselection
             (lambda (self in-selstart in-selend)
               (setselection in-selstart in-selend)))
;            ((eq? message 'setlinks)
;             (lambda (self in-linklist)
;               (setlinks in-linklist)))
    (obj-put this-obj 'setlinks
             (lambda (self in-linklist)
               (setlinks in-linklist)))
;            ((eq? message 'addlink)
;             (lambda (self in-link)
;               (addlink in-link)))
    (obj-put this-obj 'addlink
             (lambda (self in-link)
               (addlink in-link)))
;            ((eq? message 'removelink)
;             (lambda (self in-link)
;               (removelink in-link)))
    (obj-put this-obj 'removelink
             (lambda (self in-link)
               (removelink in-link)))
;            ((eq? message 'renamelink)
;             (lambda (self in-linkID in-newname)
;               (renamelink in-linkID in-newname)))
    (obj-put this-obj 'renamelink
             (lambda (self in-linkID in-newname)
               (renamelink in-linkID in-newname)))
;            ((eq? message 'getcomponent)
;             (lambda (self)
;               the-editor))
    (obj-put this-obj 'getcomponent
             (lambda (self) the-editor))
;            ((eq? message 'getdocument)
;             (lambda (self)
;               the-doc))
    (obj-put this-obj 'getdocument
             (lambda (self) the-doc))
;            ((eq? message 'set-nodeID!)
;             (lambda (self in-nodeID)
;               (set-nodeID! in-nodeID)))
    (obj-put this-obj 'set-nodeID!
             (lambda (self in-nodeID)
               (set-nodeID! in-nodeID)))
;            ((eq? message 'get-nodeID)
;             (lambda (self)
;               the-nodeID))
    (obj-put this-obj 'get-nodeID
             (lambda (self)
               the-nodeID))
;            ((eq? message 'set-node!)
;             (lambda (self in-node)
;               (set-node! in-node)))
    (obj-put this-obj 'set-node!
             (lambda (self in-node)
               (set-node! in-node)))
;            ((eq? message 'clear-content!)
;             (lambda (self)
;               (clear-content!)))
    (obj-put this-obj 'clear-content!
             (lambda (self) (clear-content!)))
;            ((eq? message 'set-mouseover-callback!)
;             (lambda (self in-mouseover-callback)
;               (set-mouseover-callback! in-mouseover-callback)))
    (obj-put this-obj 'set-mouseover-callback!
             (lambda (self in-mouseover-callback)
               (set-mouseover-callback! in-mouseover-callback)))
;            ((eq? message 'set-update-dirty-callback)
;             (lambda (self in-update-dirty-callback)
;               (set-update-dirty-callback! in-update-dirty-callback)))
    (obj-put this-obj 'set-update-dirty-callback
             (lambda (self in-update-dirty-callback)
               (set-update-dirty-callback! in-update-dirty-callback)))
;            ((eq? message 'get-lastmousemove)
;             (lambda (self)
;               lastmousemove))
    (obj-put this-obj 'get-lastmousemove
             (lambda (self) lastmousemove))
;            ((eq? message 'set-background-color)
;             (lambda (self in-color)
;               (set-editor-background-color in-color)))
    (obj-put this-obj 'set-background-color
             (lambda (self in-color)
               (set-editor-background-color in-color)))
;            ((eq? message 'set-background-image)
;             (lambda (self in-filename)
;               (set-editor-background-image in-filename)))
    (obj-put this-obj 'set-background-image
             (lambda (self in-filename)
               (set-editor-background-image in-filename)))
;            ((eq? message 'set-undo-manager!)
;             (lambda (self in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)
;               (set-undo-manager! in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)))
    (obj-put this-obj 'set-undo-manager!
             (lambda (self in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)
               (set-undo-manager! in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)))
;            ((eq? message 'clear-background-image)
;             (lambda (self)
;               (clear-editor-background-image)))
    (obj-put this-obj 'clear-background-image
             (lambda (self)
               (clear-editor-background-image)))
;            (else (get-method named-obj message))))
    this-obj))

; read-only hypertextpane
(define (make-hypertextpane-readonly
         w h
         selectlink-callback 
         deletelink-callback
         enable-newlink-button-callback
         gettext-method
         getlinks-method
         nodelist-name)
  (let* ((htpane-obj (make-hypertextpane
                      w h
                      selectlink-callback
                      deletelink-callback
                      enable-newlink-button-callback
                      gettext-method
                      getlinks-method
                      nodelist-name))
         (this-obj (new-object htpane-obj)))
    
    ; initialize the hypertexteditor
    (define (init)
      ; init the parent object
      (ask htpane-obj 'init)
      
      ; set editor to read-only
      (set-text-component (ask htpane-obj 'getcomponent) #f #t) ; parameters are editable and enabled
      
      ; set mouseover callback
      (ask htpane-obj 'set-mouseover-callback! mouseover-callback))

    ; mouseover callback
    (define (mouseover-callback is-link linkID)
      (let ((nodereader-text (ask htpane-obj 'getcomponent)))
        (if is-link
            ; over link, so change to hand cursor
            (if (not (check-cursor-type nodereader-text 'hand))
                (set-cursor-type nodereader-text 'hand))
            ; otherwise change to default cursor
            (if (not (check-cursor-type nodereader-text 'default))
                (set-cursor-type nodereader-text 'default)))))
    
    ; message handling                  
;    (lambda (message)
;      (cond ((eq? message 'init)
;             (lambda (self)
;               (init)))
;            (else (get-method htpane-obj message))))
    (obj-put this-obj 'init
             (lambda (self) (init)))
    this-obj))


;;
;; helper functions for storing linkIDs and linkActions in attributes
;; 

; link actions

; name of linkAction attribute
(define linkAction-attribute "linkAction")

; store link action in attributes 
(define (set-attribute-linkAction in-attribute-set :: <javax.swing.text.SimpleAttributeSet>
                                  in-linkAction)
  (set-attribute-data in-attribute-set linkAction-attribute in-linkAction))

; retrieve link action from attributes
(define (get-attribute-linkAction in-attribute-set :: <javax.swing.text.AttributeSet>)
  (get-attribute-data in-attribute-set linkAction-attribute))

; retrieve link action from attributes by position
(define (get-attribute-linkAction-pos in-doc :: <javax.swing.text.DefaultStyledDocument>
                                      pos :: <int>)
  (get-attribute-data-pos in-doc linkAction-attribute pos))

; link IDs

; name of linkID attribute
(define linkID-attribute "linkID")

; store link ID in attributes 
(define (set-attribute-linkID in-attribute-set :: <javax.swing.text.SimpleAttributeSet>
                              in-linkID)
  (set-attribute-data in-attribute-set linkID-attribute in-linkID))

; retrieve link ID from attributes
(define (get-attribute-linkID in-attribute-set :: <javax.swing.text.AttributeSet>)
  (get-attribute-data in-attribute-set linkID-attribute))

; retrieve link ID from attributes by position
(define (get-attribute-linkID-pos in-doc :: <javax.swing.text.DefaultStyledDocument>
                                  pos :: <int>)
  (get-attribute-data-pos in-doc linkID-attribute pos))
