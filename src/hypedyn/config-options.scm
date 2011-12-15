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

; configuration options
;
;; TODO: break this into editor config and reader config

(module-export set-undo-enabled! is-undo-enabled?
               is-basic-mode? set-basic-mode!
               card-shark? set-card-shark! reset-card-shark
               sculptural? set-sculptural!
               disable-back-button? set-disable-back-button! reset-back-button
               disable-restart-button? set-disable-restart-button! reset-restart-button
               show-actions? set-show-actions!
               show-facts? set-show-facts!
               show-docrule? set-show-docrule!
               show-noderule? set-show-noderule!
               show-IDs? set-show-IDs!
               allow-overlap? set-allow-overlap!
               snap-to-grid? set-snap-to-grid!
               user-study? set-user-study!
               set-basic-version! set-normal-version! set-sculptural-version! set-full-version!)

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
