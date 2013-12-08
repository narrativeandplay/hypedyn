;; Part of the HypeDyn project - http://www.partechgroup.org/hypedyn
;; 
;; Copyright (C) 2008-2013
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

; about window

(require "../kawa/ui/component.scm")
(require "../kawa/ui/frame.scm")
(require "../kawa/ui/text.scm")
(require "../kawa/ui/scrollpane.scm")
(require "datastructure.scm")

; export
(module-export create-about-window show-about-window close-about-window)
  (module-static 'init-run)
  
; about window frame
(define about-window-frame #f)

; about text
(define about-editor #f)

; parent
(define about-window-parent #f)

; create the about window
(define (create-about-window in-parent)
  ; remember parent
  (set! about-window-parent in-parent)
  
  ; top-level frame
  (set! about-window-frame (make-window "About HypeDyn"))
  ; text editor
  (set! about-editor (make-textarea "" 15 40))
  (set-text-component about-editor #f #t)
  (set-text about-editor 
            "HypeDyn hypertext fiction editor 
Version 2.3b rev 333, 28 November 2013\n
Copyright (C) 2008-2013
National Univeristy of Singapore\n
This program is free software; you can redistribute it and/or 
modify it under the terms of the GNU General Public License as 
published by the Free Software Foundation; either version 2 of 
the License, or (at your option) any later version.\n
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.\n
You should have received a copy of the GNU General Public 
License along with this program; if not, write to the Free 
Software Foundation, Inc., 51 Franklin Street, Fifth Floor, 
Boston, MA 02110-1301 USA.\n
Built using Kawa:
http://www.gnu.org/software/kawa/
Copyright (C) Per Bothner, 1996-2011
Used under X11/MIT license.\n
Some portions of this application are based on:\n
Graph Editor
http://www.hexahedron.hu/private/peteri/
Copyright (C) Peter Ivanyi, 2007
Used under GNU GPL.\n
Fisher-Yates shuffling algorithm
http://osdir.com/ml/plt-scheme/2009-08/msg00134.html
Public domain.\n
Export code from Processing
from Sketch.java and Base.java
Used under GNU GPL.\n
iScroll v4.1.9 ~ Copyright (c) 2011 Matteo Spinelli, http://cubiq.org
Released under MIT license, http://cubiq.org/license\n
Page flipping based on 
http://www.html5rocks.com/en/tutorials/casestudies/20things_pageflip/
used with permission of the author\n
The following libraries are bundled with the software:
AppleJavaExtensions\n
Credits\n
Programming:
Ruchi Bajoria, Chuah Teong Leong, Alex Mitchell and
Zeng Qiang\n
Testing:
Ruchi Bajoria, Zeng Qiang, Alex Mitchell,
Kevin McGee, Kow Wei Man, Tim Merritt, Lim Teng Chek,
Lim Teng Howe, Shane Hua, Praveen Sudalaimuthu Namasivayam,
and the students of NM2217 and NM3222.\n
This work was funded in part under a Singapore Ministry of 
Education Academic Research Fund Faculty Research Committee 
(Tier 1) research grant, \"Understanding Interactivity: 
Experimental Studies contributing to an applied formal 
theory of interactive digital media\", NUS AcRF Grant 
R-124-000-024-112.\n
This work was funded in part under a Singapore-MIT GAMBIT 
Game Lab research grant, \"Tools for Telling: How Game 
Development Systems Shape Interactive Storytelling.\"")
  (let(( about-editor-scroll (make-scrollpane about-editor)))
    (add-component about-window-frame about-editor-scroll))
  (pack-frame about-window-frame))
  
; show/hide the about window
(define (show-about-window)
  (format #t "show-about-window~%~!")
  (if (and about-window-frame about-window-parent)
      ; show, centered
      (begin
        (center-frame-in-parent about-window-frame about-window-parent)
        (set-component-visible about-window-frame #t))))
  
; close the about window
(define (close-about-window)
  (if about-window-frame
      (set-component-visible about-window-frame #f)
      (dispose-frame about-window-frame)))
