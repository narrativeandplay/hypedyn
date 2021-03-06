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

; for kawa - map to drscheme naming
(begin
  (require 'hash-table))

(module-export hash-table-put! hash-table-get hash-table-remove! hash-table-count hash-table-for-each)
(module-static 'init-run)

(define hash-table-put! hash-table-set!)
; allow for drscheme [failure-thunk-or-value] for 3rd arg
(define (hash-table-get table key . args)
  (if (pair? args)
      (let ((tv (car args)))
        (if (procedure? tv)
            (hash-table-ref table key tv)
            (try-catch (hash-table-ref table key tv)
              (ex <java.lang.Throwable>
                  tv))))
      (hash-table-ref table key)))
(define hash-table-remove! hash-table-delete!)
(define hash-table-count hash-table-size)
(define hash-table-for-each hash-table-walk)