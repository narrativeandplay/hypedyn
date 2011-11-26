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

;;
;; export
;; General approach is based on Processing's export code
;; see http://code.google.com/p/processing/source/browse/trunk/processing/app/src/processing/app/Sketch.java?r=7522 for reference
;; 

(require "../kawa/file.scm")
(require "../kawa/system.scm")
(require "../kawa/miscutils.scm")
(require "../kawa/strings.scm")
(require "../kawa/ui/dialog.scm")
(require "../kawa/ui/panel.scm")
(require "../kawa/ui/label.scm")
(require "../kawa/ui/button.scm")
(require "../kawa/ui/checkbox.scm")
(require "../kawa/ui/component.scm")
(require "../kawa/ui/container.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/ui/events.scm")
(require "../kawa/ui/text.scm")
(require "../common/fileio.scm")
(require "../common/main-ui.scm")
(require "htfileio.scm")

(module-export get-last-exported-dir set-last-exported-dir! update-last-exported-dir!
               get-content-file export-web export-standalone
               export-create-folder)

; last exported directory, a <java.io.File>, or #!null if none
(define last-exported-dir #!null)
(define (get-last-exported-dir)
  (if (not-null? last-exported-dir)
      last-exported-dir
      (make-file (get-user-directory))))
(define (set-last-exported-dir! newdir)
  (set! last-exported-dir newdir))
(define (update-last-exported-dir! newdir)
  (set! last-exported-dir (if (and newdir (not (is-null? newdir)))
                               (make-file (path-parent newdir))
                               #!null)))

; export to applet for web
;;	- export-web-folder (export-web-folder) must contain:
;;		- the kawa.jar file
;;      - the AppleJavaExtensions.jar file
;;		- the reader jar file (export-web-reader-jar-filename)
;;		- the html file that embeds the reader jar file (export-web-html-filename)
;; Note: the compile-and-package-hypedyn-* scripts build the export folder for HypeDyn
;;	- ask for name of folder to save to?
;;	- create folder
;;	- save file to a specific name in new folder - done by save-callback, which may do additional work such as copy images or data files
;;	- copy html and jar files from export-web-folder to new folder
;; 
;; Parameters: 
;; export-web-folder: path the folder containing the export files, relative to application
;; export-web-reader-jar-filename: name of reader jar file
;; export-web-html-filename: name of html file
;; export-web-data-filename: name of exported file
;; save-callback: procedure to save data to a file, takes 2 strings (export folder and source folder)
(define (export-web export-web-folder
                    export-web-reader-jar-filename
                    export-web-html-filename
                    save-callback)
  (let ((export-folder (get-safe-new-filename (get-last-exported-dir) #t '())))
    (if (not (eq? #f export-folder))
        (begin
          ; create folder, first deleting if it already exists
          (export-create-folder export-folder)
          (update-last-exported-dir! export-folder)

          ; Note: put try-catch around this and cleanup on failure
          (let* ((source-folder-string (path-file (get-content-file export-web-folder))))
            ; copy relevant files into folder
            ; note, copy-file-nio requires java.lang.File
            (copy-file-nio (make-file (string-append source-folder-string "/" export-web-html-filename))
                           (make-file (string-append (path-file export-folder) "/" export-web-html-filename)))
            (copy-file-nio (make-file (string-append source-folder-string "/" export-web-reader-jar-filename))
                           (make-file (string-append (path-file export-folder) "/" export-web-reader-jar-filename)))
            (copy-file-nio (make-file (string-append source-folder-string "/kawa-applet.jar"))
                           (make-file (string-append (path-file export-folder) "/kawa-applet.jar")))
            (copy-file-nio (make-file (string-append source-folder-string "/AppleJavaExtensions.jar"))
                           (make-file (string-append (path-file export-folder) "/AppleJavaExtensions.jar")))

            ; do app-specific saving
            (if (procedure? save-callback)
                (save-callback (path-file export-folder) source-folder-string)))
          #t)
        #f)))

;;
;; export to standalone
;; note: this is currently very hypedyn-specific
;; 

; constants to define relative locations of files for export
(define-constant export-standalone-folder "lib/export")
(define-constant export-standalone-story-filename "/story.dyn")
(define-constant export-standalone-windows-app-filename "/hypedyn-reader.exe")
(define-constant export-standalone-macos-app-filename "/HypeDynReader.app")
(define-constant export-standalone-linux-app-filename "/hypedyn-reader")
(define-constant export-standalone-macos-lib-folder "/Contents/Resources/Java")
(define-constant export-standalone-nonmac-lib-folder "/lib")
(define-constant export-standalone-jar-filename "/hypedyn-reader.jar")
(define-constant export-standalone-kawa-jar-filename "/kawa.jar")
(define-constant export-standalone-macos-folder "/standalone-macos")
(define-constant export-standalone-windows-folder "/standalone-windows")
(define-constant export-standalone-linux-folder "/standalone-linux")
(define-constant export-standalone-macos 0)
(define-constant export-standalone-windows 1)
(define-constant export-standalone-linux 2)

; export to standalone
(define (export-standalone)
  ; first check what platform they want, and other options
  (let* ((export-dialog (make-dialog (get-main-ui-frame) 
                                     "Export" #t))
         (export-label (make-label))
         (export-platform-panel (make-panel))
         (export-platform-label (make-label))
         (export-platform-checkboxpanel (make-panel))
         (export-platform-checkbox-macos (make-checkbox "MacOS"))
         (export-platform-checkbox-windows (make-checkbox "Windows"))
         (export-platform-checkbox-linux (make-checkbox "Linux"))
         (export-platform-buttonpanel (make-panel))
         (export-platform-button-ok (make-button "Ok"))
         (export-platform-button-cancel (make-button "Cancel")))
    ; build the dialog box
    (set-container-layout export-dialog 'border)
    (set-dialog-resizable export-dialog #f)
    
    ; label
    (set-text export-label "Create a standalone, double-clickable application.")
    (add-component export-dialog export-label 'border-north)
    
    ; panel for platform selection
    (set-container-layout export-platform-panel 'border)
    (add-component export-dialog export-platform-panel 'border-center)
    
    ; label for platform selection
    (set-text export-platform-label "Platform")
    (add-component export-platform-panel export-platform-label 'border-north)
    
    ; panel for checkboxes
    (set-container-layout export-platform-checkboxpanel 'flow 'left)
    (add-component export-platform-panel export-platform-checkboxpanel 'border-center)
    
    ; checkboxes
    (set-checkbox-value export-platform-checkbox-macos #t)
    (add-component export-platform-checkboxpanel export-platform-checkbox-macos)
    (set-checkbox-value export-platform-checkbox-windows #t)
    (add-component export-platform-checkboxpanel export-platform-checkbox-windows)
    (set-checkbox-value export-platform-checkbox-linux #t)
    (add-component export-platform-checkboxpanel export-platform-checkbox-linux)
    
    ; ok/cancel buttons
    (set-container-layout export-platform-buttonpanel 'flow 'left)
    (add-component export-dialog export-platform-buttonpanel 'border-south)
    (add-component export-platform-buttonpanel export-platform-button-ok)
    (add-actionlistener export-platform-button-ok
                        (make-actionlistener (lambda (source)
                                               (export-dialog-ok export-dialog
                                                                 export-platform-checkbox-macos
                                                                 export-platform-checkbox-windows
                                                                 export-platform-checkbox-linux))))
    (add-component export-platform-buttonpanel export-platform-button-cancel)
    (add-actionlistener export-platform-button-cancel
                        (make-actionlistener (lambda (source)
                                               (export-dialog-cancel export-dialog))))
    
    ; pack
    (pack-frame export-dialog)
    
    ; center in parent
    (center-frame-in-parent export-dialog (get-main-ui-frame))
    
    ; and show
    (set-component-visible export-dialog #t)))

; pressed cancel
(define (export-dialog-cancel in-dialog)
  (set-component-visible in-dialog #f)
  (dispose-dialog in-dialog))

; pressed ok
(define (export-dialog-ok in-dialog
                          in-checkbox-macos
                          in-checkbox-windows
                          in-checkbox-linux)
  ; hide dialog
  (set-component-visible in-dialog #f)

  ; get folder to export to (should check if no platforms selected first)
  (let ((export-folder (get-safe-new-filename (get-last-exported-dir) #t '())))
    (if (not (eq? #f export-folder))
        (begin
          ; create the folder
          (export-create-folder export-folder)
          
          ; check which platforms are chosen, and export
          (if (get-checkbox-value in-checkbox-macos)
              (doexport-standalone-platform (make-file 
                                             (string-append
                                              (path-file export-folder)
                                              export-standalone-macos-folder))
                                            export-standalone-macos))
          (if (get-checkbox-value in-checkbox-windows)
              (doexport-standalone-platform (make-file 
                                             (string-append
                                              (path-file export-folder)
                                              export-standalone-windows-folder))
                                            export-standalone-windows))
          (if (get-checkbox-value in-checkbox-linux)
              (doexport-standalone-platform (make-file 
                                             (string-append
                                              (path-file export-folder)
                                              export-standalone-linux-folder))
                                            export-standalone-linux))
          
          ; remember the export folder for preferences
          (update-last-exported-dir! export-folder))))
          
    ; dispose the dialog
  (dispose-dialog in-dialog))
   
;; export to a given platform
;;	- standalone folder contains:
;;		- the hypedyn-reader.exe file (windows)
;;		- the hypedyn-reader.jar file (windows and ubuntu)
;;		- the hypedyn-reader shell script (ubuntu)
;;		- the hypedyn-reader.app file (macos)
;;	- ask for name of folder to save to?
;;	- create new folder
;;	- copy relevant files into new folder
;;		- windows and linux: copy app and jar
;;		- linux: copy app and jar
;;		- mac: copy app structure, and copy jar inside app
;;	- save file to a specific name
;;		- windows and linux: story.dyn
;;		- mac: app/Contents/Resources/Java/story.dyn
(define (doexport-standalone-platform export-folder export-platform)
  ; create the export folder
  (export-create-folder export-folder)
  
  ; copy relevant files into folder
  ; try my own copy proc as the kawa one chokes on big files
  ; Note: put try-catch around this and cleanup on failure
  (let* ((source-folder-string (path-file (get-content-file export-standalone-folder)))
         ; note, copy-file-nio requires java.lang.File
         (app-filename (cond ((= export-platform export-standalone-macos)
                              export-standalone-macos-app-filename)
                             ((= export-platform export-standalone-windows)
                              export-standalone-windows-app-filename)
                             ((= export-platform export-standalone-linux)
                              export-standalone-linux-app-filename)))
         (lib-folder (if (= export-platform export-standalone-macos)
                         (make-file (string-append (path-file export-folder) 
                                                   export-standalone-macos-lib-folder))
                         (make-file (string-append (path-file export-folder) 
                                                   export-standalone-nonmac-lib-folder))))
         (source-standalone-app-file (make-file (string-append source-folder-string
                                                               app-filename)))
         (dest-standalone-app-file (make-file (string-append (path-file export-folder)
                                                             app-filename)))
         (source-standalone-jar-file (make-file (string-append source-folder-string
                                                               export-standalone-jar-filename)))
         (dest-standalone-jar-file (if (= export-platform export-standalone-macos)
                                       (make-file (string-append (path-file export-folder)
                                                                 export-standalone-macos-app-filename
                                                                 export-standalone-macos-lib-folder
                                                                 export-standalone-jar-filename))
                                       (make-file (string-append (path-file lib-folder)
                                                                 export-standalone-jar-filename))))
         (source-standalone-kawa-jar-file (make-file (string-append source-folder-string
                                                                    export-standalone-kawa-jar-filename)))
         (dest-standalone-kawa-jar-file (if (= export-platform export-standalone-macos)
                                            (make-file (string-append (path-file export-folder)
                                                                      export-standalone-macos-app-filename
                                                                      export-standalone-macos-lib-folder
                                                                      export-standalone-kawa-jar-filename))
                                            (make-file (string-append (path-file lib-folder)
                                                                      export-standalone-kawa-jar-filename)))))
    ; copy the app file
    (cond ((= export-platform export-standalone-macos)
           (begin
             ; for macos, need to copy the entire .app structure
             (format #t "MacOS: need to copy the .app structure: ~a to ~a.~%~!" source-standalone-app-file dest-standalone-app-file)
             (recursively-copy-directory source-standalone-app-file
                                         dest-standalone-app-file)
             ; also need to set the application stub to be executable
             (if (not (is-windows?))
                 (runtime-exec (fstring->string (string-append "chmod +x "
                                                               (path-file dest-standalone-app-file)
                                                               "/Contents/MacOS/JavaApplicationStub"))))))
          (( = export-platform export-standalone-linux)
           (begin
             ; for linux, copy the script and set it to be executable
             (format #t "copying ~a to ~a.~%~!" source-standalone-app-file dest-standalone-app-file)
             (copy-file-nio source-standalone-app-file dest-standalone-app-file)
             (if (not (is-windows?))
                 (runtime-exec (fstring->string (string-append "chmod +x " (path-file dest-standalone-app-file)))))))
          (( = export-platform export-standalone-windows)
           (begin
             ; for windows, just copy the .exe file
             (format #t "copying ~a to ~a.~%~!" source-standalone-app-file dest-standalone-app-file)
             (copy-file-nio source-standalone-app-file dest-standalone-app-file))))

    ; create the lib folder
    (export-create-folder lib-folder)
    
    ; copy the jar files
    (format #t "copying ~a to ~a.~%~!" source-standalone-jar-file dest-standalone-jar-file)
    (copy-file-nio source-standalone-jar-file dest-standalone-jar-file)
    (format #t "copying ~a to ~a.~%~!" source-standalone-kawa-jar-file dest-standalone-kawa-jar-file)
    (copy-file-nio source-standalone-kawa-jar-file dest-standalone-kawa-jar-file)
    
    (format #t "done copying~%~!"))

  ; save file to folder/story.dyn
  (let ((export-file (path (string-append
                            (path-file export-folder)
                            (if (= export-platform export-standalone-macos)
                                (string-append export-standalone-macos-app-filename
                                               export-standalone-macos-lib-folder)
                                export-standalone-nonmac-lib-folder)
                            "/"
                            export-standalone-story-filename))))
    (format #t "saving story file to ~a.~%~!" export-file)
    (ht-save-to-file export-file #t)))

; helper, adapted from Processing, for getting file relative to the app
; http://code.google.com/p/processing/source/browse/trunk/processing/app/src/processing/app/Base.java?r=7522
(define (get-content-file in-name)
  (let ((the-path (get-system-property "user.dir")))
    ; Get a path to somewhere inside the .app folder
    (if (is-mac-os?) 
;      <key>javaroot</key>
;      <string>$JAVAROOT</string>
        (let ((javaroot (get-system-property "javaroot")))
          (if (not (is-null? javaroot))
              (set! the-path javaroot))))
    (make-file (string-append the-path "/" in-name))))


; helper fn to create folder, first deleting if it already exists
(define (export-create-folder export-folder)
  ; create folder, first deleting if it already exists
  (if (file-exists? export-folder)
      (if (file-directory? export-folder)
          (recursively-delete-directory export-folder)
          (try-catch
              (delete-file export-folder)
            (ex <java.lang.Throwable>
                (begin
                  (display (*:toString ex))(newline)
                  ;(*:printStackTrace ex)
                  )))))
  (create-directory export-folder))

