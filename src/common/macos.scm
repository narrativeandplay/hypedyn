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

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Mac-specific stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: requires that AppleJavaExtensions.jar be in the classpath on non-mac platforms

(require "../kawa/system.scm")

(module-export add-application-listener enable-mac-menus set-event-handled)
(module-static 'init-run)

; get mac application object
(define (get-application) :: <com.apple.eawt.Application>
  (<com.apple.eawt.Application>:get-application))

; extend applicationAdapter to handle quit, about and preferences on a mac 
;(define-simple-class <my-application-adapter> ((<java.lang.Class>:forName "com.apple.eawt.ApplicationAdapter"))
;(define-simple-class <my-application-adapter> ((get-class "com.apple.eawt.ApplicationAdapter"))
(define-simple-class <my-application-adapter> (<com.apple.eawt.ApplicationAdapter>)
  (quit-callback)
  (about-callback)
  (preferences-callback)
  (open-application-callback)
  (open-file-callback)
  (print-file-callback)
  (re-open-application-callback)
  ((*init* in-quit-callback in-about-callback in-preferences-callback in-open-application-callback in-open-file-callback
           in-print-file-callback in-re-open-application-callback)
   (set! quit-callback in-quit-callback)
   (set! about-callback in-about-callback)
   (set! preferences-callback in-preferences-callback)
   (set! open-application-callback in-open-application-callback)
   (set! open-file-callback in-open-file-callback)
   (set! print-file-callback in-print-file-callback)
   (set! re-open-application-callback in-re-open-application-callback))
  ((handle-quit e) :: <void>
   ; handle quit event
   (format #t "handle quit~%~!")
   (quit-callback e))
  ((handle-about e) :: <void>
   ; handle about event
   (format #t "handle about~%~!")
   (about-callback e))
  ((handle-preferences e) :: <void>
   ; handle preference event
   (format #t "handle preferences~%~!")
   (preferences-callback e))
  ((handle-open-application e) :: <void>
   ; handle open application event
   (format #t "handle open application~%~!")
   (open-application-callback e))
  ((handle-open-file e) :: <void>
   ; handle open file event
   (let ((the-filename (invoke (as <com.apple.eawt.ApplicationEvent> e) 'getFilename)))
     (format #t "handle open file: ~a~%~!" the-filename)
     (open-file-callback the-filename)))
  ((handle-print-file e) :: <void>
   ; handle print event
   (format #t "handle print file~%~!")
   (print-file-callback e))
  ((handle-re-open-application e) :: <void>
   ; handle re-open application (from dock) event
   (format #t "handle re-open application~%~!")
   (re-open-application-callback e))
  )

; add application listener
(define (add-application-listener quit-callback about-callback preferences-callback open-application-callback
                                  open-file-callback print-file-callback re-open-application-callback)
  (let ((the-application :: <com.apple.eawt.Application> (get-application)))
    (invoke the-application 'add-application-listener 
            (<my-application-adapter> quit-callback about-callback preferences-callback open-application-callback
                                      open-file-callback print-file-callback re-open-application-callback))))

; enable mac menus
(define (enable-mac-menus) ; in-default-menubar :: <javax.swing.JMenuBar>)
  (let ((the-application :: <com.apple.eawt.Application> (get-application)))
    ; enable about and preferences menus
    (invoke the-application 'setEnabledAboutMenu #t)
    (invoke the-application 'setEnabledPreferencesMenu #t)

    ; set look and feel
;;    (set-system-property "java.library.path" "../../../lib/libquaqua64.jnilib")
;;    (<javax.swing.UIManager>:setLookAndFeel "ch.randelshofer.quaqua.QuaquaLookAndFeel")

    (set-system-property "apple.laf.useScreenMenuBar" "true")
    
    ; set a default menu bar
    ; tricky since only available on certain versions
;;    (invoke the-application 'setDefaultMenuBar in-default-menubar)
;;      Class<?> appClass = Application.class;
;;      Method method = 
;;        appClass.getMethod("setDefaultMenuBar", new Class[] { JMenuBar.class });
;;      if (method != null) {
;;        JMenuBar defaultMenuBar = new JMenuBar();
;;        JMenu fileMenu = buildFileMenu(base);
;;        defaultMenuBar.add(fileMenu);
;;        method.invoke(application, new Object[] { defaultMenuBar });
;;        // This is kind of a gross way to do this, but the alternatives? Hrm.
;;        Base.defaultFileMenu = fileMenu;
;;      }
;; 
    ; also need to figure out how to allow the menu to stay alive after the main window is closed...
  ))

; set an event as handled
(define (set-event-handled e :: <com.apple.eawt.ApplicationEvent>)
  (invoke e 'setHandled #t))