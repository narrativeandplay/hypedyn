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

; UI for a hypertext node
(begin
  (require "../kawa/miscutils.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/ui/cursor.scm")
  ;(require "../kawa/ui/splitpane.scm")
  (require "../kawa/ui/undo.scm")
  (require "../kawa/color.scm")
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
         ;; === UI elements ===
         (the-editor #f)
         (the-doc #f)
         ;; === flags ===
         (dirty #f)
         (track-dirty #f)
         (track-links #f)
         ;; === data ===
         (the-nodeID #f)
         ;; mouseover callback
         (mouseover-callback #f)
         ;; dirty display update callback: used to indicate that text has changed
         (update-dirty-callback #f)
         ;; last mouse event, hack for user study to trigger tooltipmanager to show tooltip
         (lastmousemove #f)

         ;; undo handling
         (track-undoable-edits #t)
         (undo-manager #f)
         (undo-action #f)
         (redo-action #f)
         (re-edit-node-callback #f)
         
         ;; compound action tracking
         ;; ( (list))
         )
    
    ; initialize the hypertexteditor
    (define (init)
      (buildui))
    
    ; build the UI
    (define (buildui)
      ; make text editor for content
      (set! the-editor (make-textpane-with-background-image))
      ; get the doc for editor
      (set! the-doc (get-textpane-doc the-editor))
      
      ;; force all newline insertions to be of the form "\n" in the textpane
      ;; an issue might be when it is written out to a file "\n" is still used
      ;; the lines won't be broken properly when opened in other text editing programs
      (set-text the-editor "\n")
      (set-text the-editor "")
      
      (add-caretlistener
       the-editor
       (make-caretlistener cursor-handler))
      (add-documentlistener
       the-editor
       (make-documentlistener document-insert-handler
                              document-remove-handler
                              document-changed-handler))
      (add-documentfilter
       the-editor
       (make-documentfilter document-filter-insert-string-handler
                            document-filter-remove-handler
                            document-filter-replace-handler))
      (add-keylistener
       the-editor
       (make-keylistener key-press-handler
                         key-type-handler
                         key-release-handler))
      
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
    
    ; clickback for links
    (define (clickback this-linkID)
      (if (procedure? selectlink-callback)
          (selectlink-callback this-linkID)))

    ; add a link in the editor as an underline
    (define (addlink thislink)
      (display "[addlink]")(newline)
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
                        style-link
                        start-index
                        len
                        #t)

        ; and set clickback
        (set-clickback this-linkID start-index len))
      
      ;; set back original value
      (set-track-undoable-edits! original-track-undoable-edits))
    
    ; set clickback on a range of text
    (define (set-clickback this-linkID start-index len)
      (display "set-clicback! ***********")(display (list this-linkID start-index len))(newline)
      (let ((link-attribute-set (make-attribute-set)))
        (set-attribute-linkAction link-attribute-set
                                  (lambda ()
                                    (display "clickback ***********")(display this-linkID)(newline)
                                    (clickback this-linkID)))
        (set-attribute-linkID link-attribute-set this-linkID)
        (set-text-style the-doc
                        link-attribute-set
                        start-index
                        len
                        #f)))
    
    ; remove a link (underline) from the editor
    (define (removelink thislink)
      ;; cache the value of track-undoable-edits and set it back later
      (define original-track-undoable-edits track-undoable-edits)
      (set-track-undoable-edits! #f)
      
      (let* ((start-index (ask thislink 'start-index))
             (end-index (ask thislink 'end-index))
             (len (- end-index start-index))
             (this-linkID (ask thislink 'ID)))
        
        (set-text-style the-doc style-nolink
                        start-index
                        len
                        #t))
      (set-track-undoable-edits! original-track-undoable-edits))
    
    ; rename a link
    (define (renamelink in-linkID in-newname)
      'ok)
    
    ;; shift the boundaries of the link and
    ;; trimming flag differentiate between a link shifting and a trimming when boundary shifts
    (define (shift-link-boundary linkID side new-val #!optional trimming)
      (let* ((thislink (get 'links linkID))
             (old-start (ask thislink 'start-index))
             (old-end (ask thislink 'end-index)))
        
        ;; takes care of clickback setting when resizing 
        ;; boundaries (new-val is the one you're setting to now)
;        (define (clickback-resize old-val new-val side)
;          (display "clickback resize linkID ")(display linkID)(newline)
;          (display "old-val new-val ")(display (list old-val new-val trimming))(newline)
;          (if (and (equal? side 'start)
;                   (> old-val new-val)
;                   trimming)
;              (begin
;                (display "shifting start ")(newline)
;                (set-clickback linkID new-val (- old-val new-val))))
;          (if (and (equal? side 'end)
;                   (> new-val old-val)
;                   trimming)
;              (begin
;                (display "shifting end ")(newline)
;                (set-clickback linkID old-val (- new-val old-val)))
;              ))
        
      (if thislink
          (begin
            (if (equal? side 'start)
                (begin
                  (ask thislink 'set-start-index! new-val)
                  (compoundundomanager-postedit
                   undo-manager
                   (make-undoable-edit "Shift link start"
                                       (lambda () ;; undo
                                         ;; important to get the link again
                                         ;; the previous reference to the link is outdated
                                         (set! thislink (get 'links linkID))
                                         ;(display " setting start to ")(display old-start)(newline)
                                         (ask thislink 'set-start-index! old-start)
                                         ;(clickback-resize new-val old-start side)
                                         ) ;; undo
                                       (lambda () ;; redo
                                         (set! thislink (get 'links linkID))
                                         ;(display " setting start to ")(display new-val)(newline)
                                         (ask thislink 'set-start-index! new-val)
                                         ;(clickback-resize old-start new-val side)
                                         )))
                  ))
            (if (equal? side 'end)
                (begin
                  (ask thislink 'set-end-index! new-val)
                  (compoundundomanager-postedit
                   undo-manager
                   (make-undoable-edit "Shift link end"
                                       (lambda () ;; undo 
                                         (set! thislink (get 'links linkID))
                                         ;(display " setting end to ")(display old-end)(newline)
                                         (ask thislink 'set-end-index! old-end)
                                         ;(clickback-resize new-val old-end side)
                                         ) 
                                       (lambda () ;; redo
                                         (set! thislink (get 'links linkID))
                                         ;(display " setting end to ")(display new-val)(newline)
                                         (ask thislink 'set-end-index! new-val)
                                         ;(clickback-resize old-end new-val side)
                                         )))
                  ))
            ))
      ))
    
    ;; to be called before doing link boundary shifting
    ;; updates the clickback boundaries of the link
    (define (post-clickback-resize-undoable linkID)
      (display "[posting clickback] ")(newline)
      (compoundundomanager-postedit
       undo-manager
       (make-undoable-edit "link clickback resize"
                           (lambda () ;; undo 
                             (let* ((this-link (get 'links linkID))
                                    (link-start (ask this-link 'start-index))
                                    (link-end (ask this-link 'end-index))
                                    (link-len (- link-end link-start)))
                               (set-clickback linkID link-start link-len)
                               ))
                           (lambda () ;; redo
                             #f
                             ))))
    
    ;; bound lst is a two element list with start and end position of the link
        ;; fragment that we are suppose to restore underline/bold formatting to
        (define (format-link-text bound-lst)
          (display "   bound-lst ")(display bound-lst)(newline)
          (if (not (null? bound-lst)) 
              (set-text-style the-doc style-link ;; format new extension
                              (car bound-lst) ;; start
                              (- (cadr bound-lst) (car bound-lst)) #t)))
    
    ; adjust links after deleting
    ; returns #t if need to manually clean up after link deletion
    (define (adjust-links-delete start len)
      (display "[adjust-links-delete] ")(newline)
      (let ((edited-node (get nodelist-name the-nodeID))
            (deleted-link-bound '()))
        ; run through each link and adjust
        (map (lambda (l)
               (set! deleted-link-bound 
                     (append deleted-link-bound (list (adjust-one-link-delete start len l))))
               )
             (ask edited-node getlinks-method))
        
        (display "  deleted link-bound ")(display deleted-link-bound)(newline)
        
        (compoundundomanager-postedit
         undo-manager
         (make-undoable-edit "link text formating"
                             (lambda () ;; undo
                               (display "link text formating ")(newline)
                               (set-text-style the-doc style-nolink ;; clear formatting for whole of deletion
                                               start
                                               len #t)
                               (map format-link-text deleted-link-bound) ;; then put in formatting for link fragments
                               )
                             (lambda () ;; redo
                               #f
                               )))
        ))
    
    

    ; adjust one link after deletion
    ; returns #t if need to manually clean up after link deletion
    (define (adjust-one-link-delete del-start del-len linkID)
      (let ((link-deleted '())
            (thislink (get 'links linkID)))
        (if thislink
            (let ((link-start (ask thislink 'start-index))
                  (link-end (ask thislink 'end-index))
                  (del-end (+ del-start del-len)))
              (display "adjust one link delete ")(display (list del-start del-end link-start link-end))(newline)

              ;; making sure I dont leave out any conditions and 
              ;; there is not conflicts/overlap between conditions
;              del-start 0
;              del-end 1
;              link-start 2
;              link-end 3
;              permutate the position of 0123
;              get rid of permutations with 3 in front of 2, and 1 in front of 0
;              32 (get rid impossible) (3 > 2)
;              10 (get rid impossible) (1 > 0)
;              we're left with case 1 to 6
;              
;              0 = 1 case 0a ignored
;              2 = 3 case 0b (0 length link shouldnt exist)
;              special is cases that might have conflicts
              
;              0123 case 1 ;; deletion in front of link
;                special 1 = 2 ;; deleting character just left of link
;              
;              2301 case 2 ;; deletion behind the link
;                special 3 = 0 ;; deleting character just right of link
;              
;              0213 case 5 ;; part of the deletion contains head potion of link
;                no equal
;              2031 case 6 ;; part of the deletion contains tail portion of link
;                no equal
;              
;              0231 case 3 ;; delete whole link
;                ;; does no formatting 
;                special 0 = 2
;                special 3 = 1
;              
;              2013 case 4 ;; whole deletion within link
;                special 2 = 0  
;                special 1 = 3  
              ;; possible conflict with case 3 is when 
              ;; (and (= 0 2) (= 1 3)) but would not have reached case 4 since case 3 is on top
              ;; so we're safe
              
              ;; how to read the examples aaBBaa  
              ;; a - non link text,  b - link text
              ;; [ - start of deletion, ] - end of deletion
              (cond ((= del-start del-end) ;; Case 0a
                     (set! link-deleted '()) ;; empty deletion (nothing deleted)
                     )
                    ((= link-start link-end) ;; Case 0b
                     (set! link-deleted '())
                     (display "[ZERO LEN LINK BUG ALERT!!]")(newline)
                     )
                    ((<= del-end link-start) ;; Case 1; del-start del-end link-start link-end (eg [a]aBBaa, [aa]BBaa) 0123
                     ; Entire deletion is before link (shift link)
                     (display " delete case 1 ")(newline)
                     (shift-link-boundary linkID 'start (- link-start del-len))
                     (shift-link-boundary linkID 'end (- link-end del-len))
                     ;(set! link-deleted 0)
                     (set! link-deleted '())
                     )
                    ((<= link-end del-start) ;; Case 2; link-start link-end del-start del-end (eg aaBB[a]a, aaBBa[a]) 2301
                     ;; Entire deletion after link (DO NOTHING to this link)
                     (display " delete case 2 ")(newline)
                     (set! link-deleted '())
                     ;(set! link-deleted 0)
                     )
                    ((and (<= del-start link-start) ;; Case 3; del-start link-start link-end del-end (eg a[aBB]aa, a[aBBa]a, aa[BBa]a, aa[BB]aa)
                          (<= link-end del-end))
                     (display " delete case 3 ")(newline)
                     ;; Entire link is inside deletion, (delete link)
                     ;(set! link-deleted (- link-end link-start))
                     (set! link-deleted (list link-start link-end))
                     ; Delete the link
                     (deletelink-callback linkID))
                    ((and (<= del-end link-end)         ;; Case 4; link-start del-start del-end link-end (eg aaB[B]a, aa[B]Baa, aaB[B]Baa) 
                          (<= link-start del-start))    ;; makes sure not the whole link 
                     ;; Entire deletion is inside link but not encompassing it, (shorten link by length of deletion)
                     (display " delete case 4 ")(newline)
                     (post-clickback-resize-undoable linkID)
                     (shift-link-boundary linkID 'end (- link-end del-len) #t)
                     (display "link start ")(display link-start)(newline)
                     (display "new link end ")(display (- link-end del-len))(newline)
                     ;(set! link-deleted del-len)
                     (set! link-deleted (list del-start del-end))
                     )
                    ((and (< del-start link-start)  ;; Case 5; del-start link-start del-end link-end (eg a[aB]Baa)
                          (< link-start del-end)    ;; link boundaries does not coincides with deletion boundary    
                          (< del-end link-end))
                     (display " delete case 5 ")(newline)
                     ; a portion of the link at the head is deleted (shift link and shorten)
                     (post-clickback-resize-undoable linkID)
                     (shift-link-boundary linkID 'start del-start #t)
                     (shift-link-boundary linkID 'end (+ del-start (- link-end del-end)))
                     ;(set! link-deleted (- del-end link-start)) ;; the length of head deleted
                     (set! link-deleted (list link-start del-end)))
                    ((and (< link-start del-start)   ;; Case 6; link-start del-start link-end del-end  (eg aaB[Ba]a)
                          (< del-start link-end)     ;; link boundaries does not coincides with deletion boundary
                          (< link-end del-end))
                     (display " delete case 6 ")(newline)
                     ;; cut off an end portion of link (shift link-end)
                     (post-clickback-resize-undoable linkID)
                     (shift-link-boundary linkID 'end del-start #t)
                     ;(set! link-deleted (- link-end del-start))
                     (set! link-deleted (list del-start link-end))
                     ))
              ))
        (display "end of adjust one link delete ")(newline)
        link-deleted))
    
    ; adjust links after inserting
    (define (adjust-links-insert start len #!optional break-link)
      (display "adjust-links-insert ")(display (list start len break-link))(newline)
      (display "start len undo? extend-len ")(newline)
      (let ((edited-node (get nodelist-name the-nodeID)))
        ; run through each link and adjust
        (map (lambda (l)
               (adjust-one-link-insert start len l break-link))
             (ask edited-node getlinks-method))))

    ; adjust one link after insertion
    ; Note: if extend-len is more than 0, extend the end of link 
    (define (adjust-one-link-insert ins-start ins-len linkID break-link)
      (display "adjust one link insert ")(newline)
      (let ((thislink (get 'links linkID)))
        (if thislink
            (let ((link-start (ask thislink 'start-index))
                  (link-end (ask thislink 'end-index))
                  (ins-end (+ ins-start ins-len)))
;              (display "ins len ")(display ins-len)(newline)
;              (display "in adjust one link insert ")(display (list link-start link-end))(newline)
              (display "ins-start ins-end ")(display (list ins-start ins-end))(newline)
              (display "link-start link-end ")(display (list link-start link-end))(newline)
  
              (cond ((and (<= ins-start link-start) ) ;; Case 1 : ins before link (eg aa[i]LLLaa, a[i]aLLLaa)
                     (display "insert case 1 insert before link (just shift link) ")(newline)
                     (display "link start end ")(display (list link-start link-end))(newline)
                     ;; just shift link without changing length
                     ;(ask thislink 'set-start-index! (+ link-start ins-len))
                     ;(ask thislink 'set-end-index! (+ link-end ins-len))
                     (shift-link-boundary linkID 'start (+ link-start ins-len))
                     (shift-link-boundary linkID 'end (+ link-end ins-len))
                     )
                    ((and (< link-start ins-start) (< ins-start link-end) ) ;; Case 2 : ins within link (eg aaL[i]LLaa)
                     (display "insert case 2 (within link) ")(newline)
                     (shift-link-boundary linkID 'end (+ link-end ins-len))
                     )
                    ((= link-end ins-start)   ;; Case 3b : ins RIGHT after link end (eg aaLLL[i]aa)
                     (display "insert case 3 (ins right after link) ")(newline)
                     (if (not break-link) ;; used by insert-blank-space (custom call that does not extend link len ie extend-len is 0)
                                          ;(ask thislink 'set-end-index! (+ link-end ins-len))
                         (shift-link-boundary linkID 'end (+ link-end ins-len))
                         )) ;; lengthen link by ins-len

                    ((< link-end ins-start) ;; Case 3a: ins after link (do nothing) (eg aaLLLa[i]a)
                     (display "INSERT AFTER LINK")(newline)
                     #f)))
              )))

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
        (display "end of link test ")(display pos)(newline)
        (display " (> text-length 0) ")(display (> text-length 0))(newline)
        (display " (> pos 0) ")(display (> pos 0))(newline)
        (display " (is-link? (- pos 1)) ")(display (is-link? (- pos 1)))(newline)
        (display " or (= pos text-length) ")(display (= pos text-length))(newline)
        (display " or (not (eq? (get-attribute-linkAction-pos the-doc (- pos 1))
                           (get-attribute-linkAction-pos the-doc pos)))")
        (display (not (eq? (get-attribute-linkAction-pos the-doc (- pos 1))
                           (get-attribute-linkAction-pos the-doc pos))))(newline)
        (and (> text-length 0)
             (> pos 0)
             (is-link? (- pos 1))
             (or (= pos text-length)
                 (not (equal? (get-attribute-linkAction-pos the-doc (- pos 1))
                           (get-attribute-linkAction-pos the-doc pos)))))
        ))

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

    ;; set to true would inform insert-filter to bypass normal 
    ;; get style method and use style-nolink by default
    (define break-link #f)
    
    ; insert a blank character with no link attached
    (define (insert-blank-space pos)
      
      (define (before-break-link)
        (set-track-links! #f)
        (set! break-link #t)
        
        
        (compoundundomanager-postedit
         undo-manager
         (make-undoable-edit "Before Break Link"
                             (lambda () #f
                               (display "track?1 ")(display track-undoable-edits)(newline)
                               ) ;; undo
                             (lambda () ;; redo
                               (set-track-links! #f)
                               (set! break-link #t)
                               ))))
      
      (define (after-break-link)
        (set! break-link #f)
        (set-track-links! #t)
        (adjust-links-insert pos 1 #t)  ;; since we not tracking link, we do the shifting ourselves here
        
        (compoundundomanager-postedit
         undo-manager
         (make-undoable-edit "After Break Link"
                             (lambda () 
                               #f
                               (display "track?2 ")(display track-undoable-edits)(newline)
                               ) ;; undo
                             (lambda () ;; redo
                               (set-track-links! #t)
                               (set! break-link #f)
                               ;; does not extend link len in this case
                               (adjust-links-insert pos 1 #t)
                               ))))
      
      (start-compound-undoable-event "End Linktext Extend")
      (display "track undoable flag1 ")(display track-undoable-edits)(newline)
      (before-break-link)
      (display "track undoable flag2 ")(display track-undoable-edits)(newline)
      ; note: for some reason need a hack here, insert 2 characters and then
      ; delete the second one, otherwise the style doesn't go away - alex
      ;(set-text-insert-attr the-doc "  " pos style-nolink)
      ;(set-text-delete the-doc (+ pos 1) 1)
      ;; no need to do the above hack anymore for some unknown reason
      ;(set-text-insert-attr the-doc " " pos style-nolink)
      (set-text-insert the-doc " " pos)
      (display "track undoable flag3 ")(display track-undoable-edits)(newline)
      (after-break-link)
      (display "track undoable flag4 ")(display track-undoable-edits)(newline)
      (end-compound-undoable-event "End Linktext Extend")
      )
    
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
      (define old-track-undoable-edits track-undoable-edits)
      (set-track-undoable-edits! #f)
      (set-text-default-style the-editor style-nolink #t) ;;DEBUG
      ;(set-text-default-style the-editor style-link #t)
      
      (set-text the-editor in-text)
      
      (set-text-selection the-editor 0 0)
;      (set-text-style the-doc style-nolink 0
;                      (+ (get-text-length the-doc) 1) #t)
      
      (set-track-undoable-edits! old-track-undoable-edits)
      (set-track-links! #t)
      (set-track-dirty! #t)
      (clear-dirty!))

    ; set selection
    (define (setselection in-selstart in-selend)
      (set-text-selection the-editor in-selstart in-selend))
    
    ; set links (no need for this anymore since when doing filter-bypass
    ;; we always check which style we should use for the text)
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
    
    ;; used by our custom document edit undo/redo mechanism
    (define insert-cache (list))
    (define remove-cache (list))
      
    ;;=============================
    ;; Document filter handlers
    ;;=============================
    ; note: these are called BEFORE the document is changed, and its safe to
    ; use the filter bypass to make changes to the document

    ; handle formatting ourselves: (bolding and unlining of link text)
    ; if its the END of a link, or (its a LINK and it isn't the START of a link), use the
    ; formatting from the previous position, otherwise use no formatting
    (define (style-to-use offset)
      (if break-link 
          style-nolink
          (if (and (or (end-of-link? offset)
                       (and (is-link? offset)
                            (not (start-of-link? offset))))
                   (not (= offset 0)))
              (get-attributes-pos the-doc (- offset 1))
              style-nolink)))
    
    ; handle insert string
    (define (document-filter-insert-string-handler fb offset string attr)
      (display "INSERT FILTER ")(display (list offset string))(newline)(newline)
      (set! replace-event #f)
      (set! insert-cache (list offset string (string-length string) attr))
      (start-compound-undoable-event "Typing") ;(insert) compound") ; start compound event
      
      ;; used by insert-blank-space atm
      (display "style to use ")(display (style-to-use offset))(newline)
      (display "end of link? ")(display (end-of-link? offset))(newline)
      (filter-bypass-insert fb offset string (style-to-use offset))
      
      #f)

    ; handle remove
    ; note: extra parameter (callback) is added when doc filter is created
    (define (document-filter-remove-handler fb offset len)
      (display "REMOVE FILTER ")(display (list offset len))(newline)
      (start-compound-undoable-event "Typing") ;(remove) compound") ; start compound event
      (define string (substring (get-text the-editor) offset (+ offset len)))
      ;; if locked then we are in the middle of an undo
      (if (or (not undo-manager) 
              (not (compoundundomanager-locked? undo-manager)))
          (after-delete offset len)) ; recalculate link positions
      
      ;; since after-delete does not post any undoable on its own
      ;; it can be called
      ;(after-delete offset len)
      
      (set! replace-event #f)
      (set! remove-cache (list offset string len))
      #t)

    ; handle replace (called when we do inserts or replaces operations)
    ; note: extra parameter (callback) is added when doc filter is created
    (define (document-filter-replace-handler fb offset len string attr)
      (display "REPLACE FILTER ")(display (list offset len string))(newline)
      (start-compound-undoable-event "Typing") ;(replace) compound") ; start compound event
      (if (or (not undo-manager) 
              (not (compoundundomanager-locked? undo-manager)))
          (after-delete offset len))
      (set! replace-event #t) ;; hack to get replace to work properly
      (set! insert-cache (list offset string (string-length string)))
      (define string-removed (substring (get-text the-editor) offset (+ offset len)))
      (set! remove-cache (list offset string-removed len))
      
      ;; replace has positive len variable while insert has 0 len
      (if (> len 0)
          (filter-bypass-replace fb offset len string (style-to-use offset))
          (begin
            (display "style to use replace ")(display (style-to-use offset))(newline)
            (display "this returns .. ")(display (get-attributes-pos the-doc (- offset 1)))(newline)
            (display "this is class tyep ")(display (invoke (get-attributes-pos the-doc (- offset 1)) 'get-class))(newline)
            (display "class type ")(display (invoke (style-to-use offset) 'get-class))(newline)
            
            (define test-cast (as <javax.swing.text.AttributeSet> (get-attributes-pos the-doc (- offset 1))))
            (display "test-cast ")(display test-cast)(newline)
            (display "test cast class ")(display (invoke test-cast 'get-class))(newline)
            
            (display "offset ")(display (- offset 1))(newline)
            
            (display "is this attribute set ")
            (display (javax.swing.text.AttributeSet? (get-attributes-pos the-doc (- offset 1))))
            (newline)
            
            (display "doc len ")(display (invoke (as <javax.swing.text.Document> the-doc) 'get-length))(newline)
;;            (set-text-style the-doc test-cast
;;                            ;(- (invoke the-doc 'get-length) 6)'
;;                            0
;;                            (invoke the-doc 'get-length) #t)
            
            (display "is style-link ")(display (equal? (style-to-use offset) style-link))(newline)
            (display "is style-nolink ")(display (equal? (style-to-use offset) style-nolink))(newline)
            (display "!!!end of link?!! ")(display (end-of-link? offset))(newline)
            (filter-bypass-insert fb offset string (style-to-use offset))
            )
          )
      #f)

    ;; =========================
    ;;  Document listeners
    ;; =========================
    ; note: DON'T make changes to the document from these handlers!

    (define (insert-undo event-offset event-string event-len)
      (display "insert-undo")(newline)
      (display "track undo here ")(display track-undoable-edits)(newline)
      (set-text-delete the-doc event-offset event-len)
      ;;(textpane-remove the-editor event-offset event-len)
      (set-cursor-pos the-editor event-offset)
      )
    
    (define (remove-undo event-offset event-string event-len)
      (set-text-insert the-doc event-string event-offset)
      ;;(textpane-insert the-editor event-string event-offset)
      (set-cursor-pos the-editor (+ event-offset (string-length event-string)))
      )
    
    ; handle insert
    (define (document-insert-handler e)
      (display "[Handler insert] ")(newline)
;      (display "undoable? ")(display (undoable-edit? e))(newline)
;      (display "redoing ")(display (compoundundomanager-locked? undo-manager))(newline)
      
      ; recalculate link positions
      (let ((change-length (get-change-length e))
            (change-offset (get-change-offset e)))
        ;; if locked then we are in the middle of an undo
        (if (or (not undo-manager) 
                (not (compoundundomanager-locked? undo-manager)))
            (after-insert change-offset change-length (undoable-edit? e))))
      
      (define (post-insert-undoable-edit)
        
        (define (post-it event-offset event-string event-len)
          (compoundundomanager-postedit
           undo-manager
           (make-undoable-edit "Typing" ;(insert)"
                               (lambda () ;; undo
                                 (insert-undo event-offset event-string event-len))
                               (lambda () ;; redo
                                 (remove-undo event-offset event-string event-len))
                               )))
          
          (if (not (null? insert-cache))
              (post-it (car insert-cache)   ;; event offset 
                       (cadr insert-cache)  ;; event string
                       (caddr insert-cache)))
          (set! insert-cache '()) ;; clear insert-cache after using
          )
      
;      (display "undoable-edit? ")(display (undoable-edit? e))(newline)
;      (display "track edit ")(display track-undoable-edits)(newline)
      (if (and undo-manager
               (undoable-edit? e) ; this is to avoid posting undo/redo events
               track-undoable-edits)
          (begin
            (post-insert-undoable-edit)  ;; post our own undoable edit
            
            ;; (finalize-compound-undoable-event e "Typing(insert)" #t)
            (end-compound-undoable-event "Typing"))) ;(insert) close compound"))) ;; end compound event
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
    ;; to prevent that we flag replace-event when replace-filter is called and stop 
    ;; remove-handler from doing end-compound-undoable-event (done in finalize-compound-undoable-event)
    (define replace-event #f)
    
    ; handle delete
    (define (document-remove-handler e)
      (display "[HANDLER REMOVE] ")(newline)
      
      (define (post-remove-undoable-edit)

        (define (post-it event-offset event-string event-len)
          (compoundundomanager-postedit
           undo-manager
           (make-undoable-edit "Typing" ;(remove)"
                               (lambda () ;; undo
                                 (remove-undo event-offset event-string event-len)
                                 )
                               (lambda () ;; redo
                                 (insert-undo event-offset event-string event-len))
                               )))
        
        (if (not (null? remove-cache))
            (post-it (car remove-cache)     ;; event-offset
                     (cadr remove-cache)    ;; event-string
                     (caddr remove-cache))) ;; event-len

        (set! remove-cache '()) ;; clear remove-cache after using
        )

;      (display "undoable-edit? ")(display (undoable-edit? e))(newline)
;      (display "track edit ")(display track-undoable-edits)(newline)
      (if (and undo-manager
               (undoable-edit? e) ; this is to avoid posting undo/redo events
               track-undoable-edits)
          (begin
            (post-remove-undoable-edit)

            ;; if replace-event, means that remove-handler was invoked by replace filter 
            ;; dont end compound 
            ;(finalize-compound-undoable-event e "Typing(remove)" (not replace-event))
            (if (not replace-event)
                (end-compound-undoable-event "Typing")) ;(remove) close compound")) ;; end compound event
            ))
      )
    
    ; handle style change
    (define (document-changed-handler e)
      (display "CHANGED HANDLER ")(newline)
      
       (if (and undo-manager
               (undoable-edit? e) ; this is to avoid posting undo/redo events
               track-undoable-edits)
          (begin ;; change does not normally do that
;            (display "!!====================================================!!")(newline)
;            (display "!!!!!!!!!!  Change handler posting undoables !!!!!!!!!!!")(newline)
;            (display "!!====================================================!!")(newline)
            (compoundundomanager-postedit undo-manager e)
            
            ;(if end-compound? 
            ;; for now we don't close compound undoable 
            ;(end-compound-undoable-event "Doc Changed  close compound")
            ))
      ;(finalize-compound-undoable-event e "CHANGED" #t)
      ) ;; end compound event

    ;
    ; called by document and caret handlers
    ;

    ; after-delete
    ; returns #t if need to manually clean up after link deletion
    (define (after-delete start len)
      (set-dirty!)
      (display " [after delete] ")(newline)
      (if track-links 
          (begin
            ;actually do it
            (adjust-links-delete start len)
;            (define link-len-deleted (adjust-links-delete start len))
;            (display "link len deleted ")(display link-len-deleted)(newline)
            
            ; post the link adjustment actions for undoing
            ;; need this to keep link-len-deleted data 
            ;; to determine whether we've deleted link text
;            (compoundundomanager-postedit undo-manager
;                                          (make-undoable-edit "after-delete"
;                                                              (lambda () ;; undo
;                                                                (display "undoing delete here ")(newline)
;                                                                (display "link len deleted ")(display link-len-deleted)(newline)
;                                                                (adjust-links-insert start len #t link-len-deleted)
;                                                                )
;                                                              (lambda () ;; redo
;                                                                (adjust-links-delete start len))))
            )
          #f))

    ; after-insert
    (define (after-insert start len can-undo)
      (display "[after insert] ")(newline)
      (set-dirty!)
      (if (and track-links
               can-undo)
          (begin
            ; and actually do it
            (adjust-links-insert start len)
            
            ;; no need to post anymore since when undoing/redoing we do not bypass the handler events
            ;; after insert would be called from handler
            ;; after insert is the same for real inserts and undo/redo
;            (define insert-undoable-edit
;              (make-undoable-edit "after-insert"
;                                  (lambda () ;; undo
;                                    (adjust-links-delete start len))
;                                  (lambda () ;; redo
;                                    (adjust-links-insert start len #f 0))))
;            (compoundundomanager-postedit undo-manager insert-undoable-edit)
            )))

    ; after-set-position
    (define (after-set-position sel-start sel-end)
      (if track-links (check-links-overlap sel-start sel-end)))

    ;;
    ;; define mouse listener
    ;;
    
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
      (if (and undo-manager
               track-undoable-edits)
          (begin
            ; start the compound edit
            (display "start compound undoable event ")(display in-undo-label)(newline)
            (compoundundomanager-beginupdate undo-manager)
            
            ; make sure that the correct node is being edited for redo
            (let ((undo-nodeID the-nodeID))
              (compoundundomanager-postedit
               undo-manager
               (make-undoable-edit in-undo-label
                                   (lambda () ;; end of undo
                                     (format #t "compound-undoable-edit end of undo~%~!")
                                     (set-track-undoable-edits! #t)
                                     )
                                   (lambda () ;; start of redo
                                     (format #t "compound-undoable-edit start of redo~%~!")
                                     (set-track-undoable-edits! #f)

                                     ;; selects the right node and brings up the node editor if need be
                                     (re-edit-node undo-nodeID)

                                     ;; hack to solve the hanging issue with first character 
                                     (if (= (get-text-length the-doc) 0)
                                         (begin
                                           ;(set! track-undoable-edits #f)
                                           (set-text-style the-doc style-nolink 0 1 #t) ;; correct one
                                           ;(set! track-undoable-edits #t)
                                           ))
                                     ))))
            )))
    
    ; tell the undo manager to end a compound undoable event
    (define (end-compound-undoable-event in-undo-label)
      ; make sure that the correct node is being edited for undo
      (let ((undo-nodeID the-nodeID))
        (display "end compound-undoable-event ")(newline)
        (display "posting last event for ")(display in-undo-label)(newline)
        (compoundundomanager-postedit 
         undo-manager
         (make-undoable-edit in-undo-label
                             (lambda () ;; start of undo
                               (format #t "compound-undoable-edit start of undo~%~!")
                               (set-track-undoable-edits! #f)
                               
                               ;; selects the right node and brings up the node editor if need be
                               (re-edit-node undo-nodeID)
                               )
                             (lambda () ;; end of redo
                               (format #t "compound-undoable-edit end of redo~%~!")
                               (set-track-undoable-edits! #t)
                               ))))
      (display "end compound success ")(newline)
      (compoundundomanager-endupdate undo-manager undo-action redo-action)
      )
    
    
    ;; called by document-insert-handler, document-remove-handler, document-change-handler
    ;; to finalize and add compound typing undoable event
    ;; event-name is the name of the undoable event
    ;; NOTE: decommissioned because insert-handler, remove-handler and change handler 
    ;; does not share common code anymore
;    (define (finalize-compound-undoable-event e event-name end-compound?)
;      (if (and undo-manager
;               (undoable-edit? e) ; this is to avoid posting undo/redo events
;               track-undoable-edits)
;          (begin
;            (if end-compound?
;                (end-compound-undoable-event event-name))
;            )))
      
    ; re-edit a node: need to make sure that the hypertextpane is currently being edited
    (define (re-edit-node in-nodeID)
      ; first, make sure we have a procedure to do this from the containing editor
      (if (procedure? re-edit-node-callback)
          (begin
            ; now get the edit back
            (format #t "trying to re-edit node, nodeID=~a~%~!" in-nodeID)
            (re-edit-node-callback in-nodeID))))
    
    ; message handling                  
    (obj-put this-obj 'init
             (lambda (self) 
               (init)))
    (obj-put this-obj 'dirty?
             (lambda (self) (dirty?)))
    (obj-put this-obj 'set-dirty!
             (lambda (self) (set-dirty!)))
    (obj-put this-obj 'clear-dirty!
             (lambda (self) (clear-dirty!)))
    (obj-put this-obj 'set-track-links!
             (lambda (self m) (set-track-links! m)))
    (obj-put this-obj 'set-track-undoable-edits!
             (lambda (self m) (set-track-undoable-edits! m)))
    (obj-put this-obj 'getselstart
             (lambda (self) (getselstart)))
    (obj-put this-obj 'getselend
             (lambda (self) (getselend)))
    (obj-put this-obj 'gettext
             (lambda (self) (gettext)))
    (obj-put this-obj 'getselectedtext
             (lambda (self) (getselectedtext)))
    (obj-put this-obj 'gettextsection
             (lambda (self in-start in-end)
               (gettextsection in-start in-end)))
    (obj-put this-obj 'settext
             (lambda (self in-text)
               (settext in-text)))
    (obj-put this-obj 'setselection
             (lambda (self in-selstart in-selend)
               (setselection in-selstart in-selend)))
    (obj-put this-obj 'setlinks
             (lambda (self in-linklist)
               (setlinks in-linklist)))
    (obj-put this-obj 'addlink
             (lambda (self in-link)
               (addlink in-link)))
    (obj-put this-obj 'removelink
             (lambda (self in-link)
               (removelink in-link)))
    (obj-put this-obj 'renamelink
             (lambda (self in-linkID in-newname)
               (renamelink in-linkID in-newname)))
    (obj-put this-obj 'getcomponent
             (lambda (self) the-editor))
    (obj-put this-obj 'getdocument
             (lambda (self) the-doc))
    (obj-put this-obj 'set-nodeID!
             (lambda (self in-nodeID)
               (set-nodeID! in-nodeID)))
    (obj-put this-obj 'get-nodeID
             (lambda (self)
               the-nodeID))
    (obj-put this-obj 'set-node!
             (lambda (self in-node)
               (set-node! in-node)))
    (obj-put this-obj 'clear-content!
             (lambda (self) (clear-content!)))
    (obj-put this-obj 'set-mouseover-callback!
             (lambda (self in-mouseover-callback)
               (set-mouseover-callback! in-mouseover-callback)))
    (obj-put this-obj 'set-update-dirty-callback
             (lambda (self in-update-dirty-callback)
               (set-update-dirty-callback! in-update-dirty-callback)))
    (obj-put this-obj 'get-lastmousemove
             (lambda (self) lastmousemove))
    (obj-put this-obj 'set-background-color
             (lambda (self in-color)
               (set-editor-background-color in-color)))
    (obj-put this-obj 'set-background-image
             (lambda (self in-filename)
               (set-editor-background-image in-filename)))
    (obj-put this-obj 'set-undo-manager!
             (lambda (self in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)
               (set-undo-manager! in-undo-manager in-undo-action in-redo-action in-re-edit-node-callback)))
    (obj-put this-obj 'clear-background-image
             (lambda (self)
               (clear-editor-background-image)))
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
      
      ; don't track undoable edits
      (ask htpane-obj 'set-track-undoable-edits! #f)
      
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
    
    ;; message handling
    (obj-put this-obj 'init
             (lambda (self) 
               (init)))
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
