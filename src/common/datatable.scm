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

(require 'list-lib)
(require "../kawa/ui/undo.scm") ;; for save-point-reset

(require "objects.scm")
(require "myhashtable.scm") ;; hash-table-get
(require 'hash-table)

(module-export make-table 
               dirty? set-dirty! clear-dirty!
               get-list 
               get put del
               reset-table
               table-map)

;; datatable.scm uses objects.scm

;; did not realize reset-table sets nextID. will provide a proc to reset that and
;; place it where all the reset-tables are

(define (make-table)
  (let ((this-obj (new-object))
        (dirty #f)
        (local-table (make-hash-table)))
    
    (obj-put this-obj 'dirty? 
             (lambda (self) dirty))
    (obj-put this-obj 'set-dirty!
             (lambda (self) (set! dirty #t)))
    (obj-put this-obj 'clear-dirty!
             (lambda (self)
               (set! dirty #f)
               (save-point-reset)))
    
    ;; Note: careful putting into the same ID would overwrite the previous value
    (obj-put this-obj 'put
             (lambda (self lst-sym-ID value-ID value)
               (define hash (hash-table-get local-table lst-sym-ID 'not-found)) 
               (if (not (equal? hash 'not-found))
                   (hash-table-put! hash value-ID value)
                   (begin
                     (hash-table-put! local-table lst-sym-ID (make-hash-table)) ;; create new hashtable and put inside local-table
                     (hash-table-put! (hash-table-get local-table lst-sym-ID) value-ID value)) ;; put content into new hashtable
                   )))
    
    ;; TODO: Fix: we shouldn't be returning #f. what if we were putting booleans into the hashtable?
    (obj-put this-obj 'get
             (lambda (self lst-sym-ID value-ID)
               (let ((hash (hash-table-get local-table lst-sym-ID 'not-found)))
                 (if (not (equal? hash 'not-found))
                     (hash-table-get hash value-ID #f) ;; return #f if not found in hash
                     #f))))
    
    (obj-put this-obj 'get-list
             (lambda (self lst-sym-ID)
               (let ((hash (hash-table-get local-table lst-sym-ID 'not-found)))
                 (if (not (equal? hash 'not-found))
                     (hash-table->alist hash) ;; returns a alist format of the hash
                     #f))))
    
    (obj-put this-obj 'del
             (lambda (self lst-sym-ID value-ID)
               (let ((hash (hash-table-get local-table lst-sym-ID 'not-found)))
                 (if (not (equal? hash 'not-found))
                     (begin
                       (hash-table-remove! hash value-ID)
                       'ok)
                     #f))))
    
    (obj-put this-obj 'local-table
             (lambda (self) local-table))
    
    (obj-put this-obj 'table-map
             (lambda (self lst-sym-ID lambda-obj)
               (let ((hash (hash-table-get local-table lst-sym-ID 'not-found)))
                 (if (not (equal? hash 'not-found))
                     (begin
                       (hash-table-for-each hash lambda-obj)
                       'ok)
                     #f))
               ))
    
    this-obj))

(define global-table (make-table))

(define (reset-table)
  (set! global-table (make-table)))
  
(define (dirty?) (ask global-table 'dirty?))
(define (set-dirty!) (ask global-table 'set-dirty!))
(define (clear-dirty!) (ask global-table 'clear-dirty!))
(define (get-list lst-sym-ID) (ask global-table 'get-list lst-sym-ID))

(define (put lst-sym-ID value-ID value)
  (ask global-table 'put lst-sym-ID value-ID value))
(define (get lst-sym-ID value-ID)
  (ask global-table 'get lst-sym-ID value-ID))
(define (del lst-sym-ID value-ID)
  (ask global-table 'del lst-sym-ID value-ID))

(define (table-map lst-sym-ID lambda-obj)
  (ask global-table 'table-map lst-sym-ID lambda-obj))

