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

; main UI for framework

(begin
  (require "runcode.scm")
  (require "evaluator.scm")
  (require "graphics.scm")
  (require "inspector.scm")
  (require "window-menu.scm")
  (require "../kawa/ui/menu.scm")
  (require "../kawa/ui/component.scm")
  (require "../kawa/ui/container.scm")
  (require "../kawa/ui/frame.scm")
  (require "../kawa/system.scm")
  (require "../kawa/ui/events.scm")
  (require "../kawa/ui/text.scm")
  (require "../kawa/ui/panel.scm")
  (require "../kawa/ui/toolbar.scm")
  (require "../kawa/ui/label.scm")
  (require "../kawa/ui/button.scm")
  (require "../kawa/ui/dialog.scm")
  (require "../kawa/file.scm")
  (require "fileio.scm")
  (require "help-window.scm")
  (require "languages.scm")
  (require "preferences.scm")
  (require "macos.scm")
  (require "datatable.scm") ;; dirty?
  )

; export
(module-export get-main-ui-frame get-main-ui-menu 
               get-file-menu get-control-menu get-help-menu get-language-menu get-examples-menu get-window-menu get-main-ui-button-panel
               set-window-activated-callback!
               set-about-callback!
               get-double-clicked-filename
               create-main-UI
               set-language!
               no-language-set
               set-runstate enable-run enable-step enable-stop
               set-run-label! set-step-label! set-stop-label!
               enable-save enable-saveas
               show-language-help
               remove-menu restore-menu
               update-label
               set-run-callback! set-step-callback! set-stop-callback!
               set-get-filename-callback! set-new-callback! set-load-callback! set-save-callback! set-confirm-save-callback!
               set-init-ui-callback! set-close-ui-callback! set-ui-welcomemsg-callback! set-prompt-for-user-input-callback!
               set-ui-display-callback! set-ui-newline-callback!
               set-get-prefs-callback! set-set-prefs-callback!
               clear-ui-callbacks! register-ui
               show-main-frame
               ui-display ui-newline showstatus)
(module-static 'init-run)

; track whether language is running or not
(define started? #f)
(define (set!started? t)
  (set! started? t))

; are we fully initialized?
(define initialized #f)
(define (initialized?)
  initialized)
(define (set-initialized!)
  (set! initialized #t))

; UI elements
(define frame #f) ; main frame
(define main-panel #f) ; main panel
(define m-control #f) ; control menu
(define m-language #f) ; lanuages menu
(define button-run #f) ; run button
(define button-step #f) ; step button
(define button-stop #f) ; stop button
(define status-bar #f) ; status bar         
(define m-file-new #f) ; new menu item
(define m-file-open #f) ; open file menu item
(define m-file-save #f) ; save menu item
(define m-file-saveas #f) ; saveas menu item
(define m-file-exit #f) ; exit menu item
(define m-file #f) ; file menu
(define m-examples #f) ; examples menu
(define m-window #f)       ; window menu
(define m-help #f)       ; help menu
(define m-help-language #f) ; language help
(define m-help-about #f) ; about for non-mac
(define m-control-run #f) ; run
(define m-control-step #f) ; step
(define m-control-stop #f) ; stop
(define m-control-inspector #f) ; language help
(define menu-bar #f) ; menu bar
(define button-panel #f) ; panel for button
(define status-panel #f) ; panel for status bar    

; accessor to top-level frame for coding-ui.scm and other editors
(define (get-main-ui-frame)
  frame)
(define (get-main-ui-menu)
  menu-bar)
(define (get-file-menu)
  m-file)
(define (get-control-menu)
  m-control)
(define (get-window-menu)
  m-window)
(define (get-help-menu)
  m-help)
(define (get-language-menu)
  m-language)
(define (get-examples-menu)
  m-examples)
(define (get-main-ui-button-panel)
  button-panel)

; remember the ui that has been registered
(define registered-ui-panel #f)
(define run-callback #f)
(define step-callback #f)
(define stop-callback #f)
(define get-filename-callback #f)
(define new-callback #f)
(define load-callback #f)
(define save-callback #f)
(define confirm-save-callback #f)
(define init-ui-callback #f)
(define close-ui-callback #f)

; used to display current language in coding ui
(define ui-welcomemsg-callback #f)

; prompt callback:
; used to prompt for user input in coding-ui,
; shown at start and after running
(define prompt-for-user-input-callback #f)

; ui-display callback:
; debugging, provides "display" in coding ui
(define ui-display-callback #f)

; ui-newline callback:
; debugging, provides "newline" in coding ui
(define ui-newline-callback #f)

; callback for getting and setting app-specific preferences
(define get-prefs-callback #f)
(define set-prefs-callback #f)

; exit
(define (window-closing exititem)
  (display "window-closing called ")(display exititem)(newline)
  ; exit either if there's no confirm-save callback, or 
  ; if user confirms that its safe to exit
  (if (or (not (procedure? confirm-save-callback))
          (confirm-save-callback))
      (begin
        ; save prefs
        (init-prefs)
        ; only save language info if its NOT standalone
        (let ((curr-languageinfo (get-curr-languageinfo)))
          (if (and curr-languageinfo
                   (symbol? (list-ref curr-languageinfo 3)))
              (begin
                (display "checked ")(newline)
                (put-pref! 'lang (get-curr-languageinfo)))))
        ; store file preferences (last dir and recent files)
        (put-file-prefs!)
        ; app-specific prefs
        (if (procedure? set-prefs-callback)
            (set-prefs-callback))
        ; write to file
        (putprefs)
        
        ; remove from window menu
        (remove-from-window-menu frame)

        (exit 1))
      (begin
        (display "don't exit")(newline))))

(define window-activated-callback #f)
(define (set-window-activated-callback! in-callback)
  (set! window-activated-callback in-callback))

;window callback ; future work
(define (window-opened o)
  (display "window-opened")(newline))
(define (window-closed o)
  (display "window-closed")(newline)
  (remove-from-window-menu frame))
(define (window-iconified o)
  (display "window-incon")(newline))
(define (window-deiconified o)
  (display "window-deinco")(newline))
(define (window-activated o)
  ;(display "window-act")(newline)
  (if (procedure? window-activated-callback)
      (window-activated-callback)))
(define (window-deactivated o)
  ;(display "window-deact")(newline)
  'do-nothing
  )

; macos-specific application events

(define about-callback #f)
(define (set-about-callback! in-callback)
  (set! about-callback in-callback))

; about event
(define (macos-application-about e)
  (display "about")(newline)
  (if (procedure? about-callback)
      (begin
        (set-event-handled e)
        (about-callback))))

; preferences event
(define (macos-application-preferences e)
  (display "preferences")(newline))

; open application event
(define (macos-application-open-application e)
  (display "open application")(newline))

(define double-clicked-filename #f)
(define (get-double-clicked-filename)
  double-clicked-filename)

; open file event, happens on double-click
(define (macos-application-open-file the-filename)
  (if (initialized?)
      ; already initialized, so go ahead and load the file
      (if load-callback (load-callback (make-file (path-file the-filename))))
      ; otherwise save the name and load when initialization is finished
      (set! double-clicked-filename the-filename)))

; print file event
(define (macos-application-print-file e)
  (display "print file")(newline))

; re-open application event
(define (macos-application-re-open-application e)
  (display "re-open-application")(newline))

; create the main UI
(define (create-main-UI)
  ; mac-os specific stuff here
  (if (is-mac-os?)
      (begin
        ; handle application events
        (add-application-listener window-closing
                                  macos-application-about
                                  macos-application-preferences
                                  macos-application-open-application
                                  macos-application-open-file
                                  macos-application-print-file
                                  macos-application-re-open-application)
        
        ; enable mac menus
        (enable-mac-menus)))
                                
  (set! frame (make-window ""))
  (set-frame-dont-exit frame) ; make sure it doesn't exit automatically so we can catch
  
  ; top level panel
  (set! main-panel (make-panel))

  (set! m-control (make-menu "Control")) ; control menu
  (set! m-language (make-menu "Language")) ; languages menu
  (set-language-menu! m-language)
  ;(set! m-window (make-menu "Window")) ; window menu
  (set! m-window (add-window-menu frame)) ; window menu
  
  ; toolbar buttons
  (set! button-panel (make-toolbar "Control")) ; panel for button
  (set! button-run (make-button "Run")) ; run button
  (set! button-step (make-button "Step")) ; step button
  (set! button-stop (make-button "Stop")) ; stop button

  (set! status-bar (make-label)) ; status bar         
  (set! status-panel (make-panel)) ; panel for status bar    
  
  ; menu bar
  (set! menu-bar (make-menu-bar)) ; menu bar

  ; file menu
  (set! m-file-new (make-menu-item "New")) ; new menu item
  (set! m-file-open (make-menu-item "Open...")) ; open file menu item
  (set! m-file-save (make-menu-item "Save")) ; save menu item
  (set! m-file-saveas (make-menu-item "Save as...")) ; saveas menu item
  (if (not (is-mac-os?))
      (set! m-file-exit (make-menu-item "Exit"))) ; exit menu item
  (set! m-file (make-menu "File")) ; file menu
  (set! m-examples (make-menu "Examples")) ; examples menu

  ; control menu
  (set! m-control-run (make-menu-item "Run")) ; run
  (set! m-control-step (make-menu-item "Step")) ; step
  (set! m-control-stop (make-menu-item "Stop")) ; stop
  (set! m-control-inspector (make-checkbox-menu-item "Show inspectors")) ; inspector

  ; window menu
  ;(set! m-window-minimize (make-menu-item "Minimize")) ; minimize - mac only?
  ;(set! m-window-zoom (make-menu-item "Zoom")) ; zoom - mac only?
  ;(set! m-window-separator (make-separator)) ; separator
  
  ; help menu
  (set! m-help (make-menu "Help"))       ; help menu
  (set! m-help-language (make-menu-item "Language help")) ; language help
  (set! m-help-about (make-menu-item "About")) ; about for non-mac
  
  ; add main panel and set layout
  (add-component frame main-panel 'border-center)
  (set-container-layout main-panel 'border)
  
  ;add menubar to frame
  (add-menu-bar frame menu-bar)

  ; add menus to menubar
  (add-component menu-bar m-file)
  (add-component menu-bar m-control)
  (add-component menu-bar m-language)
  (add-component menu-bar m-examples)
  ;(add-component menu-bar m-window)
  (add-component menu-bar m-help)
  
  ; file menu
  (add-component m-file m-file-new)
  (add-component m-file m-file-open)
  (add-component m-file m-file-save)
  (add-component m-file m-file-saveas)
  (if (not (is-mac-os?))
      (add-component m-file m-file-exit))
  
  ; help menu
  (add-component m-help m-help-language)
  (if (not (is-mac-os?))
      ; about menu item for non-mac
      (begin
        (add-component m-help (make-separator))
          (add-component m-help m-help-about)))
  
  ; control menu
  (add-component m-control m-control-run)
  (add-component m-control m-control-step)
  (add-component m-control m-control-stop)
  (set-menuitem-component m-control-stop #f)
  (add-component m-control (make-separator))
  (add-component m-control m-control-inspector)
  
  ; window menu
  ;(add-component m-window m-window-minimize)
  ;(add-component m-window m-window-zoom)
  ;(add-component m-window m-window-separator)
  
  ;add status bar to main panel
  (add-component status-panel status-bar)
  (add-component main-panel status-panel 'border-south)
  ;(set-text status-bar "Ready\n")

  ; add buttons to frame
  (add-component frame button-panel 'border-north)
  (add-component button-panel button-run)
  (add-component button-panel button-step)
  (add-component button-panel button-stop)
  (set-button button-stop #f)

  ; create help window
  (create-help-window frame)

  ; set callbacks for buttons and menu items
  (add-actionlistener button-run (make-actionlistener runit))
  (add-actionlistener button-step (make-actionlistener stepit))
  (add-actionlistener button-stop (make-actionlistener stopit))
  (add-actionlistener m-file-new (make-actionlistener new-file))
  (add-actionlistener m-file-open (make-actionlistener loadit))
  (add-actionlistener m-file-save (make-actionlistener (lambda (source)
                                                         (begin
                                                           (display "b4 saveit")(newline)
                                                           (saveit #f)))))
  (add-actionlistener m-file-saveas (make-actionlistener (lambda (source)
                                                           (begin
                                                             ;; just in case get-filename-callback not set still return a "Untitled"
                                                             (define curr-filename (if get-filename-callback (get-filename-callback) "Untitled"))
                                                             (saveit curr-filename)))))
  (add-actionlistener m-file-exit (make-actionlistener exitit))

  ; window listener for the main UI
  (add-windowlistener frame (make-windowlistener window-opened
                                                 window-closing
                                                 window-closed
                                                 window-iconified
                                                 window-deiconified
                                                 window-activated
                                                 window-deactivated))

  ; window menu actions
  ;(add-actionlistener m-window-minimize (make-actionlistener (lambda (source)
  ;                                                             (minimize-main-window))))
  ;(add-actionlistener m-window-zoom (make-actionlistener (lambda (source)
  ;                                                             (maximize-main-window))))
  
  ; help menu actions
  (add-actionlistener m-help-language (make-actionlistener show-language-help))
  (if (not (is-mac-os?))
      (add-actionlistener m-help-about (make-actionlistener
                                        (lambda (a)
                                          (show-about-window)))))

  ; control menu actions
  (add-actionlistener m-control-run (make-actionlistener runit))
  (add-actionlistener m-control-step (make-actionlistener stepit))
  (add-actionlistener m-control-stop (make-actionlistener stopit))
  (add-actionlistener m-control-inspector (make-actionlistener (lambda (a)
                                                                 ; show/hide inspectors based on menu state; note that
                                                                 ; state is already updated when the actionlistener is called
                                                                 (show-inspectors-window
                                                                  (get-checkbox-menu-item m-control-inspector)))))

  ; create inspectors window
  (create-inspectors-window m-control-inspector)

  ; add keyboard accelerators
  (set-menu-item-accelerator m-file-new #\N)
  (set-menu-item-accelerator m-file-open #\O)
  (set-menu-item-accelerator m-file-save #\S)
  ;(set-menu-item-accelerator m-window-minimize #\M) ; oops, this is used by HypeDyn
  ;  (set-menu-item-accelerator m-file-saveas #\S 'shift) ; put this in once additional modifiers implemented - alex
  (if (is-windows?)
      (begin
        (set-menu-mnemonic m-file #\F)
        (set-menu-item-mnemonic m-file-new #\N)
        (set-menu-item-mnemonic m-file-open #\O)
        (set-menu-item-mnemonic m-file-save #\S)
        (set-menu-item-mnemonic m-file-exit #\X)))
  
  ; load the languages menu items
  (let((lang-info (get-languages)))
    (add-languages lang-info set-language!))

  ; add window to window menu
  (add-to-window-menu frame "Main")
  )

; minimize the main window
(define (minimize-main-window)
  (set-frame-iconified frame))

; maximize/demaximize the main window
(define (maximize-main-window)
  (toggle-frame-maximized frame))

; set current language
(define (set-language! langmenuitem langname langfilename helpfilename initproc)
;  (with-handlers 
;  ([exn:fail? (lambda (exn) (showstatus (exn-message exn)))])
  ; close any inspectors
  (close-inspectors)

  ; shutdown graphics if necessary
  (if (eq? #t (usegraphics?))
      (begin
        (reset-graphics)
        (exit-graphics)))

  ; clear any graphics callbacks
  (set-init-callback! '())
  (set-mydraw-callback! '())
  (set-reset-callback! '())

  ; reset graphics flag
  (set-usegraphics! #f)

  ; call close ui callback if any
  (close-ui)

  ; clear ui callbacks
  (clear-ui-callbacks!)

  ; un-check previous language menu item and check new menu item
  (if langmenuitem
      (begin
        (if (get-curr-langmenuitem) (set-checkbox-menu-item (get-curr-langmenuitem) #f))
        (set-curr-langmenuitem! langmenuitem)
        (set-checkbox-menu-item langmenuitem #t)))

  ; set language name
  (set-curr-languagename! langname)

  ;set language info
  (set-curr-languageinfo (list langname langfilename helpfilename initproc))

  ;(set-component-visible frame #f)
  ;frame title
  (set-frame-title frame langname)

  ; set up expressions and procedures
  (add-language-start)
  ; if there's an init procedure specified, then we're running from a 
  ; language-specific main, not main.scm, so call the init procedure,
  ; otherwise load the specified language file
  (if (procedure? initproc)
      (initproc)
      (load langfilename))
  
  (add-language-end)

  ; create the environment for the evaluator, and start REPL
  (reset-environment)

  ; init main ui
  (init-ui)
  
  ; load language-specific preferences, if any
  (if (procedure? get-prefs-callback)
      (get-prefs-callback))

  ; load the examples menu items
;  (let(( example-info (get-examples langname)))
;    (add-languages lang-info set-language!))

  ; load help file
  (load-helpfile helpfilename)
  
  ; update help and about menu item labels
  (set-menu-item-text m-help-language (string-append langname " Help"))
  (if (not (is-mac-os?))
      (set-menu-item-text m-help-about (string-append "About " langname)))
  
  ; update UI window label
  (update-label)

  ; and wait for input
  (ui-welcomemsg (get-curr-languagename))
  (prompt-for-user-input)
  (showstatus "")

  ; create the graphics panel
  (if (eq? #t (usegraphics?))
      (begin (display "use graphics?")
        (init-graphics)))

  ;display the frame
  (set-frame-location frame 200 100)
  (pack-frame frame)

  ; return the frame
  frame)

; if no preferred language, let user know that they must select one
(define (no-language-set)
  ; tell user
  (showstatus "Please choose a language")

  ;display the frame
  (set-frame-location frame 200 100)
  (pack-frame frame))

; newfile : check for changes and save if necessary, then clear code window
(define (new-file new)
  (if new-callback (new-callback))
  (update-label)
  )

; save a file - specifying #t as filename will open file dialog,
; specifying #f will use existing filename, if any, 
(define (saveit filename)
  ;; remove * at the back of the filename if it exists
  (if filename
      (let* ((namelen (string-length filename))
             (lastchar (substring filename (- namelen 1) namelen)))
        (if (equal? lastchar "*")
            (set! filename (substring filename 0 (- namelen 1))))))
  
  (if save-callback (save-callback filename))
  (update-label))

; load a file - specifying "" as filename will open file dialog
(define (loadit load)
  (if load-callback (load-callback))
  (update-label))

; update the main window label
(define (update-label)
  (if frame
      (let ((filename (if get-filename-callback (get-filename-callback) #f)))
                                        ;(format #t "filename: ~a~%~!" filename)
        (if (not (eq? filename #f))
            (begin
;              (set! filename
;                    (string-append
;                     filename
;                     (if (or
;                          (dirty?)
;                          (nodeeditor-dirty?))
;                         "*" "")))
              (set-frame-title frame (string-append filename " - " (get-curr-languagename)))
              )
            (set-frame-title frame (get-curr-languagename)))
        )))

; exit
(define (exitit exititem)
  (window-closing exititem))

; run the code 
(define (runit run)
  (display "Run!\n")
  (if run-callback
      (begin
        (set-runstate #t)
        (run-callback))))

(define (stopit stop)
  (display "Stop!\n")
  (if stop-callback
      (begin
        (set-runstate #f)
        (stop-callback))))

; step the programme - for now it just runs
(define (stepit step)
  (display "Step!\n")
  (if step-callback (step-callback)))

; set run state: #t=running, #f=not running
; will set the started? flag, and run/stop buttons
(define (set-runstate flag)
  (set!started? flag)
  (enable-run (not flag))
  (enable-stop flag))

; enable/disable run button/menu item
(define (enable-run flag)
  (if button-run
      (set-button button-run flag))
  (if m-control-run
      (set-menuitem-component m-control-run flag)))
; enable/disable step button/menu item
(define (enable-step flag)
  (if button-step
      (set-button button-step flag))
  (if m-control-step
      (set-menuitem-component m-control-step flag)))
  ; enable/disable stop button/menu item
(define (enable-stop flag)
  (if button-stop
      (set-button button-stop flag))
  (if m-control-stop
      (set-menuitem-component m-control-stop flag)))
  
; change labels on run/step/stop buttons
(define (set-run-label! in-label)
  (set-text button-run in-label))
(define (set-step-label! in-label)
  (set-text button-step in-label))
(define (set-stop-label! in-label)
  (set-text button-stop in-label))

; enable/disable save
(define (enable-save flag)
  (set-menuitem-component m-file-save flag))

; enable/disable saveas
(define (enable-saveas flag)
  (set-menuitem-component m-file-saveas flag))

; show the help window
(define (show-language-help l)
  (show-help-window #t))

; remove a menu, useful for standalone
(define (remove-menu in-menu)
  (remove-component (get-main-ui-menu) in-menu))

; restore a menu
(define (restore-menu in-menu)
  (add-component (get-main-ui-menu) in-menu))

;;
;; init and shutdown
;; 

; init the ui
(define (init-ui)
  ; init the ui
  (if init-ui-callback (init-ui-callback))

  ; and connect the ui panel to main panel
  (if registered-ui-panel (add-component main-panel registered-ui-panel 'border-center)))

; close ui
(define (close-ui)
  ; remove ui panel from main panel
  (if registered-ui-panel (remove-component main-panel registered-ui-panel))

  ; and call any close callback
  (if close-ui-callback (close-ui-callback)))


;; welcome and prompt

; ui-welcomemsg: display a welcome message, usually name of language
(define (ui-welcomemsg msg)
  (if ui-welcomemsg-callback (ui-welcomemsg-callback msg)))

; prompt-for-user-input
(define (prompt-for-user-input)
  (if prompt-for-user-input-callback (prompt-for-user-input-callback)))

; about window

(define (show-about-window)
  (format #t "show-about-window~%~!")
  (if (procedure? about-callback)
      (about-callback)))

;; debugging

; ui-display
(define (ui-display output)
  (if ui-display-callback (ui-display-callback output)))

; ui-newline
(define (ui-newline)
  (if ui-newline-callback (ui-newline-callback)))

; callback set procedures
(define (set-run-callback! in-callback)
  (set! run-callback in-callback))
(define (set-step-callback! in-callback)
  (set! step-callback in-callback))
(define (set-stop-callback! in-callback)
  (set! stop-callback in-callback))
(define (set-get-filename-callback! in-callback)
  (set! get-filename-callback in-callback))
(define (set-new-callback! in-callback)
  (set! new-callback in-callback))
(define (set-load-callback! in-callback)
  (set! load-callback in-callback))
(define (set-save-callback! in-callback)
  (set! save-callback in-callback))
(define (set-confirm-save-callback! in-callback)
  (set! confirm-save-callback in-callback))
(define (set-init-ui-callback! in-callback)
  (set! init-ui-callback in-callback))
(define (set-close-ui-callback! in-callback)
  (set! close-ui-callback in-callback))
(define (set-ui-welcomemsg-callback! in-callback)
  (set! ui-welcomemsg-callback in-callback))
(define (set-prompt-for-user-input-callback! in-callback)
  (set! prompt-for-user-input-callback in-callback))
(define (set-ui-display-callback! in-callback)
  (set! ui-display-callback in-callback))
(define (set-ui-newline-callback! in-callback)
  (set! ui-newline-callback in-callback))
(define (set-get-prefs-callback! in-callback)
  (set! get-prefs-callback in-callback))
(define (set-set-prefs-callback! in-callback)
  (set! set-prefs-callback in-callback))

; clear all callbacks
(define (clear-ui-callbacks!)
  (set-run-callback! #f)
  (set-step-callback! #f)
  (set-stop-callback! #f)
  (set-get-filename-callback! #f)
  (set-new-callback! #f)
  (set-load-callback! #f)
  (set-save-callback! #f)
  (set-confirm-save-callback! #f)
  (set-init-ui-callback! #f)
  (set-close-ui-callback! #f)
  (set-ui-welcomemsg-callback! #f)
  (set-prompt-for-user-input-callback! #f)
  (set-ui-display-callback! #f)
  (set-ui-newline-callback! #f)
  (set-get-prefs-callback! #f)
  (set-set-prefs-callback! #f))

; register ui
(define (register-ui in-panel
                     in-run-callback
                     in-step-callback
                     in-stop-callback
                     in-get-filename-callback
                     in-new-callback
                     in-load-callback
                     in-save-callback
                     in-confirm-save-callback
                     in-init-ui-callback
                     in-close-ui-callback
                     in-ui-welcomemsg-callback
                     in-prompt-for-user-input-callback
                     in-ui-display-callback
                     in-ui-newline-callback)
  ; remember the panel
  (set! registered-ui-panel in-panel)

  ; set the callbacks
  (set-run-callback! in-run-callback)
  (set-step-callback! in-step-callback)
  (set-stop-callback! in-stop-callback)
  (set-get-filename-callback! in-get-filename-callback)
  (set-new-callback! in-new-callback)
  (set-load-callback! in-load-callback)
  (set-save-callback! in-save-callback)
  (set-confirm-save-callback! in-confirm-save-callback)
  (set-init-ui-callback! in-init-ui-callback)
  (set-close-ui-callback! in-close-ui-callback)
  (set-ui-welcomemsg-callback! in-ui-welcomemsg-callback)
  (set-prompt-for-user-input-callback! in-prompt-for-user-input-callback)
  (set-ui-display-callback! in-ui-display-callback)
  (set-ui-newline-callback! in-ui-newline-callback))

(define (showstatus output)
  (set-text status-bar output))

; show top-level frame
(define (show-main-frame)
  ; show the main frame
  (set-component-visible frame #t)

  ; remember that we're fully initialized
  (set-initialized!)

  ; and if there was a double-clicked file, load it now
  (if (and
       double-clicked-filename
       load-callback)
      (begin
        (load-callback (make-file (path-file double-clicked-filename)))
        (set! double-clicked-filename #f))))
