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
(require "../kawa/ui/undo.scm") ;compoundundomanager-postedit
(require "../common/main-ui.scm") ;;get-main-ui-frame
(require "../common/fileio.scm") ;; get-last-saved-dir
(require "../kawa/file.scm") ;; get-file-to-open
(require "hypedyn-undo.scm") ;; compoundundomanager-postedit undo-manager
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

(define browse-css-tf #f)
(define browse-css-tf2 #f)
;(define (get-browse-css-tf)
;  (get-text browse-css-tf))
;(define (get-browse-css-tf2)
;  (get-text browse-css-tf2))

(define (make-general-tab)
  (define general-tab (make-panel))
  (define label-group-panel (make-panel))
  (define label-panel-1 (make-panel))
  (define label-panel-2 (make-panel))
  (define label-panel-3 (make-panel))

  (define tf-group-panel (make-panel))
  (define tf-panel-1 (make-panel))
  (define tf-panel-2 (make-panel))
  (define tf-panel-3 (make-panel))

  (define label-1 (make-label-with-title "Author"))
  (define label-2 (make-label-with-title "Title"))
  (define label-3 (make-label-with-title "Comments"))

  (define tf-1 (make-textfield "" 20))
  (define tf-2 (make-textfield "" 20))
  (define tf-3 (make-textfield "" 20))

  (set-container-layout label-group-panel 'grid 3 1)
  (set-container-layout tf-group-panel 'grid 3 1)

  (add-component label-panel-1 label-1)
  (add-component label-panel-2 label-2)
  (add-component label-panel-3 label-3)

  (add-component tf-panel-1 tf-1)
  (add-component tf-panel-2 tf-2)
  (add-component tf-panel-3 tf-3)


  (add-component label-group-panel label-panel-1)
                                        ;(add-component label-group-panel (invoke-static <javax.swing.Box> 'create-vertical-glue))
  (add-component label-group-panel label-panel-2)
                                        ;(add-component label-group-panel (invoke-static <javax.swing.Box> 'create-vertical-glue))
  (add-component label-group-panel label-panel-3)
                                        ;(add-component label-group-panel (invoke-static <javax.swing.Box> 'create-horizontal-glue))


  (add-component tf-group-panel tf-panel-1)
                                        ;(add-component tf-group-panel (invoke-static <javax.swing.Box> 'create-vertical-glue))
  (add-component tf-group-panel tf-panel-2)
                                        ;(add-component tf-group-panel (invoke-static <javax.swing.Box> 'create-vertical-glue))
  (add-component tf-group-panel tf-panel-3)
                                        ; (add-component tf-group-panel (invoke-static <javax.swing.Box> 'create-vertical-glue))

                                        ;panel.setPreferredSize(panel.getPreferredSize());
  (define (pack-panel panel)
    (invoke panel 'set-preferred-size
            (invoke panel 'get-preferred-size)))

  (pack-panel label-panel-1)
  (pack-panel label-panel-2)
  (pack-panel label-panel-3)
  (pack-panel tf-panel-1)
  (pack-panel tf-panel-2)
  (pack-panel tf-panel-3)
  (pack-panel tf-group-panel)
  (pack-panel label-group-panel)

  (set-border tf-panel-1 black-border)
  (set-border tf-panel-2 black-border)
  (set-border tf-panel-3 black-border)
  (set-border label-panel-1 black-border)
  (set-border label-panel-2 black-border)
  (set-border label-panel-3 black-border)

  (set-container-layout general-tab 'horizontal)
                                        ;(define general-group-panel (make-panel))
                                        ;(set-container-layout general-group-panel 'horizontal)

                                        ;(add-component general-tab label-group-panel)
                                        ;(add-component general-tab tf-group-panel)
                                        ;(add-component general-tab general-group-panel)

  (add-components general-tab
                  label-group-panel
                  tf-group-panel
                                        ;(invoke-static <javax.swing.Box> 'create-horizontal-glue)
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
  (define browse-css-button (make-button "Find styling"))
  (set! browse-css-tf (make-textfield "" 16))
  (set-component-enabled browse-css-button #f)
  (set-component-enabled browse-css-tf #f)
  (add-components browse-css-panel browse-css-button browse-css-tf)

  (define browse-css-panel2 (make-panel))
  (define browse-css-button2 (make-button "Find dimension"))
  (set! browse-css-tf2 (make-textfield "" 16))
  (set-component-enabled browse-css-button2 #f)
  (set-component-enabled browse-css-tf2 #f)
  (add-components browse-css-panel2 browse-css-button2 browse-css-tf2)

  (set-container-layout rbutton-group-panel 'vertical)
  (add-components rbutton-group-panel
                  rbutton-1
                  rbutton-2
                  rbutton-3
                  browse-css-panel
                  browse-css-panel2)

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
  (define (css-radio-callback e)
    ;; only enable when rbutton3 (custom) is selected
    (set-component-enabled browse-css-button (radio-button-selected? rbutton-3))
    (set-component-enabled browse-css-button2 (radio-button-selected? rbutton-3)))
  
  ;; 3 css radio buttons 
  (add-actionlistener rbutton-1 (make-actionlistener css-radio-callback))
  (add-actionlistener rbutton-2 (make-actionlistener css-radio-callback))
  (add-actionlistener rbutton-3 (make-actionlistener css-radio-callback))
  
  (add-actionlistener browse-css-button
                      (make-actionlistener
                       (lambda (e)
                         (define filename (get-file-to-open (get-last-saved-dir) #f (list ".css")))
                         (if filename
                             (set-text browse-css-tf filename))
                         )))
  
  (add-actionlistener browse-css-button2
                      (make-actionlistener
                       (lambda (e)
                         (define filename (get-file-to-open (get-last-saved-dir) #f (list ".css")))
                         (if filename
                             (set-text browse-css-tf2 filename))
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
  (set-text browse-css-tf2 (get-custom-css-location2))
  )

(define (set-properties back restart break resize width height csstype css-loc1 css-loc2)
  (set-disable-back-button! back)
  (set-disable-restart-button! restart)
  (set-disable-pagebreak! break)
  (set-disable-page-resize! resize)
  (set-fixed-page-width! width)
  (set-fixed-page-height! height)
  (set-css-type! csstype)
  (set-custom-css-location! css-loc1)
  (set-custom-css-location2! css-loc2))

(define (make-properties-ui)
  (define propt-dialog (make-dialog (get-main-ui-frame) "Properties" #t))
  (define propt-tabpanel (make-tab-panel))
  (add-tabpanel-tab propt-tabpanel "General" (make-general-tab))
  (add-tabpanel-tab propt-tabpanel "Reader" (make-reader-tab))

  (define confirm-button (make-button "Ok"))
  (define cancel-button (make-button "Cancel"))
  (define button-panel (make-panel))
  (add-components button-panel
                  confirm-button cancel-button)
  
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
                                 (get-custom-css-location2)
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
                                 (get-text browse-css-tf2)
                                 ))
                         
                         (apply set-properties new-properties)
                         (compoundundomanager-postedit
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
                         ;(set-component-enabled width-tf (get-checkbox-value disable-resize-cb))
                         ;(set-component-enabled height-tf (get-checkbox-value disable-resize-cb))
                         (set-component-enabled width-tf selected?)
                         (set-component-enabled height-tf selected?)
                         )))
  
  (properties-ui-refresh)
  
  propt-dialog)

(define (show-properties)
  (set! propt-dialog (make-properties-ui))
  (pack-frame propt-dialog)
  (set-component-visible propt-dialog #t))
(define (hide-properties #!optional e)
  (set-component-visible propt-dialog #f))