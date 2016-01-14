;; Part of the HypeDyn project - http://www.narrativeandplay.org/hypedyn
;; 
;; Copyright (C) 2008-2016
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

(module-export load-native-library get-system-property set-system-property get-current-time runtime-exec
               get-os-name is-mac-os? is-windows? is-linux-os?)

;;;;;;;;;;;;;;;;;;;;;;
;; misc non-UI stuff, should move into a separate file?
;;;;;;;;;;;;;;;;;;;;;; 

; nobody using this anymore
(define (load-native-library lib-name :: <java.lang.String>)
  (invoke-static <java.lang.System> 'load-library lib-name))

;; system-related

; get a system property
(define (get-system-property in-property :: <java.lang.String>)
  (<java.lang.System>:getProperty in-property))

; set a system property
(define (set-system-property in-property :: <java.lang.String> in-value :: <java.lang.String>)
  (<java.lang.System>:setProperty in-property in-value))

; get time in milliseconds
(define (get-current-time)
  (<java.lang.System>:currentTimeMillis))

; exec a shell command
(define (runtime-exec in-string :: <java.lang.String>)
  ((<java.lang.Runtime>:getRuntime):exec in-string))

; get os name, useful for checking if its a mac
(define (get-os-name)
  (invoke (<java.lang.System>:getProperty "os.name") 'toLowerCase))

; is it a mac?
(define (is-mac-os?)
  (invoke (as <java.lang.String> (get-os-name)) 'startsWith "mac os x"))

; is it windows?
(define (is-windows?)
  (invoke (as <java.lang.String> (get-os-name)) 'startsWith "windows"))

;; is it linux?
(define (is-linux-os?)
  (invoke (as <java.lang.String> (get-os-name)) 'startsWith "linux"))

