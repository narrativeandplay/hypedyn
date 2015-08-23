;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2015
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

; configuration options
;
;; TODO: break this into editor config and reader config

(module-export set-undo-enabled! is-undo-enabled?
               is-basic-mode? set-basic-mode!
               card-shark? set-card-shark! reset-card-shark
               sculptural? set-sculptural!
               disable-back-button? set-disable-back-button! reset-back-button
               disable-restart-button? set-disable-restart-button! reset-restart-button
               disable-pagebreak? set-disable-pagebreak! reset-pagebreak
               disable-page-resize? set-disable-page-resize! reset-page-resize
               show-actions? set-show-actions!
               show-facts? set-show-facts!
               show-docrule? set-show-docrule!
               show-noderule? set-show-noderule!
               show-IDs? set-show-IDs!
               allow-overlap? set-allow-overlap!
               snap-to-grid? set-snap-to-grid!
               user-study? set-user-study!
               display-stats? set-display-stats!
               set-basic-version! set-normal-version! set-sculptural-version! set-full-version!
               
               reset-properties
               get-fixed-page-width set-fixed-page-width! reset-fixed-page-width
               get-fixed-page-height set-fixed-page-height! reset-fixed-page-height
               get-css-type set-css-type! reset-css-type
               get-custom-css-location set-custom-css-location! reset-custom-css-location
               get-custom-css-location2 set-custom-css-location2! reset-custom-css-location2
               get-author-name set-author-name! reset-author-name
               get-story-title set-story-title! reset-story-title
               get-story-comment set-story-comment! reset-story-comment

               java-reader? set-java-reader!
               get-temp-dir set-temp-dir! get-local-port set-local-port!
               node-name-limit choice-name-limit rule-name-limit
               enable-autosave? set-enable-autosave!
               mac-testing? set-mac-testing!
               )

; enable/disable undo
(define-private undo-enabled #t)
(define (is-undo-enabled?)
  undo-enabled)
(define (set-undo-enabled! in-flag)
  (set! undo-enabled in-flag))

; basic version?
(define-private basic-mode #f)
(define (is-basic-mode?)
  basic-mode)
(define (set-basic-mode! in-flag)
  (set! basic-mode in-flag))

; card shark mode: anywhere nodes are stored in a "deck", and a fixed
; number of nodes are available in the player's "hand" to be "played"
; each round
; see http://www.markbernstein.org/talks/HT01.html
(define-private card-shark #f)
(define-private default-card-shark #f)
(define (card-shark?)
  card-shark)
(define (set-card-shark! in-flag)
  (set! card-shark in-flag))
(define (set-card-shark-default! in-flag)
  (set! card-shark-default in-flag))
(define (reset-card-shark)
  (set! card-shark default-card-shark))

; sculptural mode: one predefined start node, otherwise only anywhere nodes
; this is an editor mode, doesn't affect reader
(define-private sculptural #f)
(define (sculptural?)
  sculptural)
(define (set-sculptural! in-flag)
  (set! sculptural in-flag))

; disable the back button:
; disable-back-button-default is the default setting for the editor,
; whereas disable-back-button can be changed on a file-by-file basis,
; and will be set in editor and reader when file is loaded, ensuring
; that files will be run correctly
(define-private disable-back-button #f)
(define-private disable-back-button-default #f)
(define (disable-back-button?)
  disable-back-button)
(define (set-disable-back-button! in-flag)
  (set! disable-back-button in-flag))
(define (set-disable-back-button-default! in-flag)
  (set! disable-back-button-default in-flag))
(define (reset-back-button)
  (set-disable-back-button! disable-back-button-default))

(define-private disable-restart-button #f)
(define-private disable-restart-button-default #f)
(define (disable-restart-button?)
  disable-restart-button)
(define (set-disable-restart-button! in-flag)
  (set! disable-restart-button in-flag))
(define (set-disable-restart-button-default! in-flag)
  (set! disable-restart-button-default in-flag))
(define (reset-restart-button)
  (set-disable-restart-button! disable-restart-button-default))

; show actions
(define-private show-actions #f)
(define (show-actions?)
  show-actions)
(define (set-show-actions! in-flag)
  (set! show-actions in-flag))

; show facts
(define-private show-facts #t)
(define (show-facts?)
  show-facts)
(define (set-show-facts! in-flag)
  (set! show-facts in-flag))

; show document rule
(define-private show-docrule #f)
(define (show-docrule?)
  show-docrule)
(define (set-show-docrule! in-flag)
  (set! show-docrule in-flag))

; show node rule
(define-private show-noderule #t)
(define (show-noderule?)
  show-noderule)
(define (set-show-noderule! in-flag)
  (set! show-noderule in-flag))

; show IDs
(define-private show-IDs #f)
(define (show-IDs?)
  show-IDs)
(define (set-show-IDs! in-flag)
  (set! show-IDs in-flag))

; allow overlap
(define-private allow-overlap #t)
(define (allow-overlap?)
  allow-overlap)
(define (set-allow-overlap! in-flag)
  (set! allow-overlap in-flag))

; snap to grid
(define-private snap-to-grid #t)
(define (snap-to-grid?)
  snap-to-grid)
(define (set-snap-to-grid! in-flag)
  (set! snap-to-grid in-flag))

; magic keypresses for user studies
(define-private user-study #f)
(define (user-study?)
  user-study)
(define (set-user-study! in-flag)
  (set! user-study in-flag))

; display stats for story analysis
(define-private display-stats #f)
(define (display-stats?)
  display-stats)
(define (set-display-stats! in-flag)
  (set! display-stats in-flag))

; some convenience fns to set common sets of options

; basic version (used for NM2217):
; basic-mode #t, card-shark #f, sculptural #f, show-actions #f, show-facts #f, show-docrule #f, show-noderule #f, show-IDs #f,
; disable-back-button-default #f, disable-back-button #f
(define (set-basic-version!)
  (set-basic-mode! #t)
  (set-card-shark! #f)
  (set-sculptural! #f)
  (set-show-actions! #f)
  (set-show-facts! #f)
  (set-show-docrule! #f)
  (set-show-noderule! #f)
  (set-show-IDs! #f)
  (set-disable-back-button-default! #f)
  (set-disable-back-button! #f))

; normal version (used for NM3222 project 1, covered in tutorials 1 and 2):
; basic-mode #f, card-shark #f, sculptural #f, show-actions #f, show-facts #f, show-docrule #f, show-noderule #f, show-IDs #f,
; disable-back-button-default #f, disable-back-button #f
(define (set-normal-version!)
  (set-basic-mode! #f)
  (set-card-shark! #f)
  (set-sculptural! #f)
  (set-show-actions! #f)
  (set-show-facts! #f)
  (set-show-docrule! #f)
  (set-show-noderule! #f)
  (set-show-IDs! #f)
  (set-disable-back-button-default! #f)
  (set-disable-back-button! #f))

; sculptural version (used for NM3222 project 2, covered in tutorial 3):
; basic-mode #f, card-shark #f, sculptural #t, show-actions #f, show-facts #t, show-docrule #f, show-noderule #t, show-IDs #f,
; disable-back-button-default #t, disable-back-button #t
(define (set-sculptural-version!)
  (set-basic-mode! #f)
  (set-card-shark! #f)
  (set-sculptural! #t)
  (set-show-actions! #f)
  (set-show-facts! #t)
  (set-show-docrule! #f)
  (set-show-noderule! #t)
  (set-show-IDs! #f)
  (set-disable-back-button-default! #t)
  (set-disable-back-button! #t))

; full version (with coding, used for research studies):
; basic-mode #f, card-shark #f, sculptural #f, show-actions #t, show-facts #t, show-docrule #t, show-noderule #t, show-IDs #t,
; disable-back-button-default #f, disable-back-button #f
(define (set-full-version!)
  (set-basic-mode! #f)
  (set-card-shark! #f)
  (set-sculptural! #f)
  (set-show-actions! #t)
  (set-show-facts! #t)
  (set-show-docrule! #t)
  (set-show-noderule! #t)
  (set-show-IDs! #t)
  (set-disable-back-button-default! #f)
  (set-disable-back-button! #f))

;; Javascript reader options
(define-private disable-pagebreak #f) ;; its really pageflip  (mobile only)
(define-private disable-pagebreak-default #f)
(define (disable-pagebreak?)
  disable-pagebreak)
(define (set-disable-pagebreak! in-flag)
  (set! disable-pagebreak in-flag))
(define (set-disable-pagebreak-default! in-flag)
  (set! disable-pagebreak-default in-flag))
(define (reset-pagebreak)
  (set-disable-pagebreak! disable-pagebreak-default))

;; a thought can we create a generic definition manager
;; where we generate the get, set, reset functions with the correct name 
;; to prevent these duplicate code - teong leong
(define-private disable-page-resize #f) ;; word wrap changes with resize (browser only)
(define-private disable-page-resize-default #f)
(define (disable-page-resize?)
  disable-page-resize)
(define (set-disable-page-resize! in-flag)
  (set! disable-page-resize in-flag))
(define (set-disable-page-resize-default! in-flag)
  (set! disable-page-resize-default in-flag))
(define (reset-page-resize)
  (set-disable-page-resize! disable-page-resize-default))

(define fixed-page-width-default 800)
(define fixed-page-height-default 600)
(define fixed-page-width fixed-page-width-default)
(define fixed-page-height fixed-page-height-default)

(define (get-fixed-page-width) fixed-page-width)
(define (set-fixed-page-width! w) (set! fixed-page-width w))
(define (reset-fixed-page-width) (set! fixed-page-width fixed-page-width-default))

(define (get-fixed-page-height) fixed-page-height)
(define (set-fixed-page-height! h) (set! fixed-page-height h))
(define (reset-fixed-page-height) (set! fixed-page-height fixed-page-height-default))

;; css selection
(define css-type-default 'default)
(define css-type 'default) ;; can be default, fancy, custom
(define (get-css-type) css-type)
(define (set-css-type! new-css-type)
  (if (member new-css-type (list 'default 'fancy 'custom))
      (set! css-type new-css-type)))
(define (reset-css-type)
  (set! css-type css-type-default))

;; style.css
(define custom-css-location "")
(define (get-custom-css-location) custom-css-location)
(define (set-custom-css-location! new-loc) (set! custom-css-location new-loc))
(define (reset-custom-css-location) (set! custom-css-location ""))

;; dimension.css
(define custom-css-location2 "")
(define (get-custom-css-location2) custom-css-location2)
(define (set-custom-css-location2! new-loc) (set! custom-css-location2 new-loc))
(define (reset-custom-css-location2) (set! custom-css-location2 ""))

;;
;; this stuff should probably go in datastructure.scm, as it is story-specific
;; 

; author name
(define author-name "")
(define (get-author-name) author-name)
(define (set-author-name! new-name) (set! author-name new-name))
(define (reset-author-name) (set! author-name ""))

; story title
(define story-title "")
(define (get-story-title) story-title)
(define (set-story-title! new-title) (set! story-title new-title))
(define (reset-story-title) (set! story-title ""))

; story comments
(define story-comment "")
(define (get-story-comment) story-comment)
(define (set-story-comment! new-comment) (set! story-comment new-comment))
(define (reset-story-comment) (set! story-comment ""))


;; do this when we start a new story
(define (reset-properties)
  (reset-back-button)
  (reset-restart-button)
  (reset-pagebreak)
  (reset-page-resize)
  (reset-fixed-page-width)
  (reset-fixed-page-height)
  (reset-css-type)
  (reset-custom-css-location)
  (reset-custom-css-location2)
  (reset-author-name)
  (reset-story-title)
  (reset-story-comment))

;; my attempt at a generic getter setter generator
#|
(define (make-var val #!rest default-val) 
  (append (list val) default-val))
(define (make-setter var) 
  (lambda (new-var) (set-car! var new-var)))
(define (make-getter var)
  (lambda () (car var)))
(define (make-resetter var)
  (lambda () 
    (if (> (length var) 2)
        (set-car! var (cadr var)))))

(set! fixed-page-width (make-var fixed-page-width-default))
(set! fixed-page-height (make-var fixed-page-height-default))

(set! set-fixed-page-width! (make-setter fixed-page-width))
(set! set-fixed-page-height! (make-setter fixed-page-height))

(set! get-fixed-page-width! (make-getter fixed-page-width))
(set! get-fixed-page-height! (make-getter fixed-page-height))
|#


; enable/disable java reader
(define-private java-reader #f)
(define (java-reader?)
  java-reader)
(define (set-java-reader! in-flag)
  (set! java-reader in-flag))

; settings for launching js reader from editor

; temp directory
(define-private temp-dir "./temp-js")
(define (get-temp-dir)
  temp-dir)
(define (set-temp-dir! in-dir)
  (set! temp-dir in-dir))

; port number
(define-private local-port 8888)
(define (get-local-port)
  local-port)
(define (set-local-port! in-port)
  (set! local-port in-port))

;; truncation
(define node-name-limit 20)
(define choice-name-limit 20)
(define rule-name-limit 40)

; enable/disable autoasve
(define-private enable-autosave #f)
(define (enable-autosave?)
  enable-autosave)
(define (set-enable-autosave! in-flag)
  (set! enable-autosave in-flag))

; hack for testing on mac
(define-private mac-testing #f)
(define (mac-testing?)
    mac-testing)
(define (set-mac-testing! in-flag)
    (set! mac-testing in-flag))
