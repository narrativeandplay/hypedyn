(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/label.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/container.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/ui/component.scm")
(require "../kawa/ui/events.scm")
(require "../kawa/ui/menu.scm")
(require "../kawa/ui/button.scm")
(require "../kawa/ui/tabpanel.scm")
(require "../kawa/ui/radio.scm")
(require "../kawa/ui/checkbox.scm")
(require "../kawa/ui/scrollpane.scm")
(require "../kawa/ui/undo.scm") ; make-undoable-edit
(require "../common/main-ui.scm") ;;get-main-ui-frame
(require "../common/fileio.scm") ;; get-last-saved-dir
(require "../kawa/file.scm") ;; get-file-to-open
(require "hypedyn-undo.scm") ;; hd-postedit undo-manager
(require "config-options.scm")

(module-export get-stylesheet-choice
               
               make-properties-ui
               show-properties)


(define propt-dialog #f)

(define disable-back-cb #f)
(define disable-restart-cb #f)
(define disable-resize-cb #f)
(define disable-pagebreak-cb #f)

(define width-tf #f)
(define height-tf #f)

;; radio button for stylesheet
(define rbutton-1 #f)
(define rbutton-2 #f)
(define rbutton-3 #f)
(define (get-stylesheet-choice)
  (define to-return #f)
  (if (radio-button-selected? rbutton-1)
      (set! to-return 'default))
  (if (radio-button-selected? rbutton-2)
      (set! to-return 'fancy))
  (if (radio-button-selected? rbutton-3)
      (set! to-return 'custom))
  to-return)

(define confirm-button #f)

(define browse-css-tf #f)
;(define browse-css-tf2 #f)
;(define (get-browse-css-tf)
;  (get-text browse-css-tf))
;(define (get-browse-css-tf2)
;  (get-text browse-css-tf2))

(define author-name-tf #f)
(define story-title-tf #f)
(define story-comment-tf #f)

(define (make-general-tab)
  (define general-tab (make-panel))

  (define tf-panel-1 (make-panel))
  (define tf-panel-2 (make-panel))
  (define tf-panel-3 (make-panel))
  
  (define label-1 (make-label-with-title "Author"))
  (define label-2 (make-label-with-title "Title"))
  (define label-3 (make-label-with-title "Comments"))

  (set-component-non-resizable-size label-1 70 150)
  (set-component-non-resizable-size label-2 70 150)
  (set-component-non-resizable-size label-3 70 150)
  
  (set! author-name-tf (make-textpane))
  (set! story-title-tf (make-textpane))
  (set! story-comment-tf (make-textpane))
  
  (set-container-layout tf-panel-1 'horizontal)
  (set-container-layout tf-panel-2 'horizontal)
  (set-container-layout tf-panel-3 'horizontal)
  
  (add-component tf-panel-1 label-1)
  (add-component tf-panel-2 label-2)
  (add-component tf-panel-3 label-3)
  
  (add-component tf-panel-1 (make-scrollpane-with-policy author-name-tf 'always 'never))
  (add-component tf-panel-2 (make-scrollpane-with-policy story-title-tf 'always 'never))
  (add-component tf-panel-3 (make-scrollpane-with-policy story-comment-tf 'always 'never))

  (define (pack-panel panel)
    (invoke panel 'set-preferred-size
            (invoke panel 'get-preferred-size)))

  (pack-panel tf-panel-1)
  (pack-panel tf-panel-2)
  (pack-panel tf-panel-3)

  (set-container-layout general-tab 'vertical)

  (add-components general-tab
                  tf-panel-1
                  tf-panel-2
                  tf-panel-3
                  )
  (pack-panel general-tab)
  
  ;;return it
  general-tab)

(define (make-reader-tab)
  
  (define reader-tab (make-panel))
  
  (define sep-1 (make-separator))
  (define sep-2 (make-separator))
  (define sep-3 (make-separator))
  (define sep-4 (make-separator))
  ;; styles control web-reader mobile-reader
  (define label-4 (make-label-with-title "Style"))
  (define label-5 (make-label-with-title "Control"))
  (define label-6 (make-label-with-title "Web Reader"))
  (define label-7 (make-label-with-title "Mobile Reader"))

  (define label-panel-4 (make-panel))
  (define label-panel-5 (make-panel))
  (define label-panel-6 (make-panel))
  (define label-panel-7 (make-panel))

  ;; style
  (define button-grp (make-button-group))
  (set! rbutton-1 (make-radio-button "default"))
  (set! rbutton-2 (make-radio-button "fancy"))
  (set! rbutton-3 (make-radio-button "custom"))
  (add-to-button-group button-grp
                       rbutton-1
                       rbutton-2
                       rbutton-3)
  (radio-button-set-selected rbutton-1 #t)
  (define rbutton-group-panel (make-panel))

  (define browse-css-panel (make-panel))
  (define browse-css-button (make-button "Choose..."))
  (set! browse-css-tf (make-textfield "" 16))
  (set-component-enabled browse-css-button #f)
  (set-component-enabled browse-css-tf #f)
  (add-components browse-css-panel browse-css-tf browse-css-button)

  (set-container-layout rbutton-group-panel 'vertical)
  (add-components rbutton-group-panel
                  rbutton-1
                  rbutton-2
                  rbutton-3
                  browse-css-panel
                  )

  (set-container-layout label-panel-4 'flow 'left)
  (set-container-layout label-panel-5 'flow 'left)
  (set-container-layout label-panel-6 'flow 'left)
  (set-container-layout label-panel-7 'flow 'left)
  (add-component label-panel-4 label-4)
  (add-component label-panel-5 label-5)
  (add-component label-panel-6 label-6)
  (add-component label-panel-7 label-7)

  ;; control
  (set! disable-back-cb (make-checkbox "Disable Back Button"))
  (set! disable-restart-cb (make-checkbox "Disable Restart Button"))

  ;; web-reader
  (set! disable-resize-cb (make-checkbox "Disable Resize"))

  (define width-tf-panel (make-panel))
  (define width-label (make-label-with-title "Width"))
  (set! width-tf (make-textfield "800" 5))
  (set-component-enabled width-tf #f)
  (add-components width-tf-panel
                  width-label
                  width-tf)

  (define height-tf-panel (make-panel))
  (define height-label (make-label-with-title "Height"))
  (set! height-tf (make-textfield "600" 5))
  (set-component-enabled height-tf #f)
  (add-components height-tf-panel
                  height-label
                  height-tf)

  ;; mobile reader
  (set! disable-pagebreak-cb (make-checkbox "Disable Page Breaks"))

  (set-container-layout reader-tab 'vertical)
  (add-components reader-tab
                  sep-1 label-panel-4 rbutton-group-panel
                  sep-2 label-panel-5 disable-back-cb disable-restart-cb
                  sep-3 label-panel-6 disable-resize-cb width-tf-panel height-tf-panel
                  sep-4 label-panel-7 disable-pagebreak-cb)
  
  ;; setup action listeners

  ;; callback used by all 3 radio button in css selection
  (define (css-radio-callback e selected?)
    ;; only enable when rbutton3 (custom) is selected
    (set-component-enabled browse-css-button (radio-button-selected? rbutton-3))
    (set-component-enabled confirm-button (or (not (radio-button-selected? rbutton-3))
                                              (and (radio-button-selected? rbutton-3)
                                                   (not (equal? (get-text browse-css-tf) "")))
                                              ))
    ;(set-component-enabled browse-css-button2 (radio-button-selected? rbutton-3))
    )
  
  ;; 3 css radio buttons 
  (add-itemlistener rbutton-1 (make-itemlistener css-radio-callback))
  (add-itemlistener rbutton-2 (make-itemlistener css-radio-callback))
  (add-itemlistener rbutton-3 (make-itemlistener css-radio-callback))
  
  (add-actionlistener browse-css-button
                      (make-actionlistener
                       (lambda (e)
                         (define filename (get-file-to-open (get-last-saved-dir) #f (list ".css")))
                         (if filename
                             (begin
                               (set-component-enabled confirm-button #t)
                               (set-text browse-css-tf filename)))
                         )))
  
  reader-tab)

;; setting ui with internal states
(define (properties-ui-refresh)
  ;; load the value if set
  (set-checkbox-value disable-back-cb (disable-back-button?))
  (set-checkbox-value disable-restart-cb (disable-restart-button?))
  (set-checkbox-value disable-resize-cb (disable-page-resize?))
  (set-checkbox-value disable-pagebreak-cb (disable-pagebreak?))
  (set-text width-tf (get-fixed-page-width))
  (set-text height-tf (get-fixed-page-height))
  (case (get-css-type)
    ((default) (radio-button-set-selected rbutton-1 #t))
    ((fancy) (radio-button-set-selected rbutton-2 #t))
    ((custom) (radio-button-set-selected rbutton-3 #t)))
  (set-text browse-css-tf (get-custom-css-location))
  (set-text author-name-tf (get-author-name))
  (set-cursor-pos author-name-tf 0)
  (set-text story-title-tf (get-story-title))
  (set-cursor-pos story-title-tf 0)
  (set-text story-comment-tf (get-story-comment))
  (set-cursor-pos story-comment-tf 0)
  )

(define (set-properties back restart break resize width height csstype css-loc1 
                        author-name story-title story-comment)
  (set-disable-back-button! back)
  (set-disable-restart-button! restart)
  (set-disable-pagebreak! break)
  (set-disable-page-resize! resize)
  (set-fixed-page-width! width)
  (set-fixed-page-height! height)
  (set-css-type! csstype)
  (set-custom-css-location! css-loc1)
  (set-author-name! author-name)
  (set-story-title! story-title)
  (set-story-comment! story-comment)
  )

(define (make-properties-ui)
  (define propt-dialog (make-dialog (get-main-ui-frame) "Properties" #t))
  (set-dialog-resizable propt-dialog #f)
  (define propt-tabpanel (make-tab-panel))
  (add-tabpanel-tab propt-tabpanel "General" (make-general-tab))
  (add-tabpanel-tab propt-tabpanel "Reader" (make-reader-tab))

  (set! confirm-button (make-button "Ok"))
  (define cancel-button (make-button "Cancel"))
  (define button-panel (make-panel))
  (add-components button-panel
                  confirm-button cancel-button)
  (set-component-enabled confirm-button #t)
  
  (set-container-layout (get-dialog-content-pane propt-dialog) 'vertical)
  (add-components (get-dialog-content-pane propt-dialog) 
                  propt-tabpanel button-panel)
  
  (add-actionlistener cancel-button
                      (make-actionlistener hide-properties))
  
  (add-actionlistener confirm-button 
                      (make-actionlistener 
                       (lambda (e)
                         ;; gotten from config-options
                         (define old-properties 
                           (list (disable-back-button?)
                                 (disable-restart-button?)
                                 (disable-pagebreak?)
                                 (disable-page-resize?)
                                 (get-fixed-page-width)
                                 (get-fixed-page-height)
                                 (get-css-type)
                                 (get-custom-css-location)
                                 (get-author-name)
                                 (get-story-title)
                                 (get-story-comment)
                                 ))
                         
                         ;; gotten from ui
                         (define new-properties
                           (list (get-checkbox-value disable-back-cb)
                                 (get-checkbox-value disable-restart-cb)
                                 (get-checkbox-value disable-pagebreak-cb)
                                 (get-checkbox-value disable-resize-cb)
                                 (get-text width-tf)
                                 (get-text height-tf)
                                 (get-stylesheet-choice)
                                 (get-text browse-css-tf)
                                 (get-text author-name-tf)
                                 (get-text story-title-tf)
                                 (get-text story-comment-tf)
                                 ))
                         (apply set-properties new-properties)
                         (hd-postedit
                          undo-manager
                          (make-undoable-edit "Edit Properties"
                                           (lambda () ;; undo
                                             (apply set-properties old-properties))
                                           (lambda () ;; redo
                                             (apply set-properties new-properties))))
                         
                         (hide-properties)
                         )))
  
  (add-itemlistener disable-resize-cb
                      (make-itemlistener
                       (lambda (event selected?)
                         (set-component-enabled width-tf selected?)
                         (set-component-enabled height-tf selected?)
                         )))
  (add-actionlistener browse-css-tf
                      (make-actionlistener
                       (lambda (e)
                         ;; enable confirm button only when there are text inside
                         (set-component-enabled confirm-button 
                                                (equal? (get-text browse-css-tf) ""))
                         )))
  
  (properties-ui-refresh)
  
  propt-dialog)

(define (show-properties)
  (set! propt-dialog (make-properties-ui))
  (pack-frame propt-dialog)
  (set-component-visible propt-dialog #t))
(define (hide-properties #!optional e)
  (set-component-visible propt-dialog #f))