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

; some helper functions for lists

(require "../kawa/miscutils.scm") ; for (is-void? x)
(require "../kawa/random.scm")
(require 'list-lib) ;; remove, list-copy

; export
(module-export list-head 
               list-replace 
               list-append 
               list-swap
               
               strip-void 
               shuffle-list
               
;;               filter
               list-contains?
               list-replace!
;;               list-copy
;;               list-index
               
               remove-from-list
               list-insert
               my-make-list
               remove-index-from-list
               list-count-equal
               )

(module-static 'init-run)

(define (list-head lst k)
  (take lst k))

(define (list-replace lst n data)
  (append
   (list-head lst n)
   (list data)
   (list-tail lst (+ n 1))))

(define (list-append lst1 lst2)
  (append lst1 lst2))

; helper function to remove voids from a list
(define (strip-void mylist)
  (if (not (list? mylist))
      (if (is-void? mylist)
          '()
          mylist)
      (begin
        (if (not (null? mylist))
            (if (is-void? (car mylist))
                (strip-void (cdr mylist))
                (append (list (car mylist)) (strip-void (cdr mylist))))
            mylist))))

; Fisher-Yates shuffling algorithm for lists
; based on Knuth in AoCP, Volume 2, Section 3.4.2, Algorithm P
; adapted from http://osdir.com/ml/plt-scheme/2009-08/msg00134.html
(define (shuffle-list x)
  (do ((v (list->vector x))
       (n (length x) (- n 1)))

      ((zero? n) (vector->list v))

      (let* ((r (generate-random-int 0 (- n 1)))
             (t (vector-ref v r)))
        (vector-set! v r (vector-ref v (- n 1)))
        (vector-set! v (- n 1) t))))

;; do i really need this or could i just use list-index ?
(define (list-contains? lst element . equal-op-arg)
  (define to-return #f)
  (define equal-op equal?)
  
  ;; if no equal-op specified just use equal?
  (if (not (null? equal-op-arg))
      (set! equal-op (car equal-op)))
  
  (map
   (lambda (curr-element)
     (if (equal-op curr-element element)
         (set! to-return #t)))
   lst)
  
  to-return
  )


;; same as list-replace but changes the actual llist
(define (list-replace! llist pos value)
  (define tail (drop llist pos)) ;; drop same as list-tail
  (set-car! tail value)
  llist
  )

;;(define (list-copy lst)
;;  (define to-return (list))
;;  (define tail lst)
;;  (do ((i 0 (+ i 1)))
;;      ((equal? i (length lst)))
;;      (begin
;;        ;(set! head (append head (list (car tail))))
;;        (set! to-return (append to-return (list (car tail))))
;;        (set! tail (cdr tail))
;;        ))
;;  to-return
;;  )

;; remove the need to set the list back with returned values of append
(define (append-element lst value)
  (set-cdr! lst (append (cdr lst) (list value)))
  )

(define (list-append-to-back lst tail-lst)
  (set-cdr! lst (append (cdr lst) tail-lst))
  )

(define (remove-from-list lst item)
;  (define list-to-ret (list))
;  (do ((n (length lst)))
;      ((= n 0))
;      (begin
;        (if (not (equal? item (car lst)))
;            (set! list-to-ret (append list-to-ret (list (car lst)))))
;        (set! lst (cdr lst))
;        (set! n (- n 1))
;        ))
;  list-to-ret
  (remove (lambda (ele) (equal? ele item)) lst)
  )

;; (list 1 2 3 4)
;; remove 0
;; (append (take lst 0) (drop 3)
;; remove 1
;; (append (take lst 1) (drop 2)
;; remove 2
;; (append (take lst 2) (drop 1)
;; remove 3
;; (append (take lst 3) (drop 0)

;; (list 1 2)
;; remove 0
;; (append (take lst 0) (drop 1))
;; remove 1 
;; (append (take lst 1) (drop 0))

(define (remove-index-from-list lst index)
  (append (take lst index) 
          (drop lst (+ index 1)))
  )

;; if position is more than length of lst it would just be at the end
(define (list-insert lst item position)
  (define (helper lst steps-left)
    (if (or (equal? (<integer> steps-left) 0)
            (equal? lst '()))
        (append (list item) lst)
        (append (list (car lst)) (helper (cdr lst) (- steps-left 1)))))

  (if (>= position 0)
      (helper lst position))
  )

;; count how many element in the list 
;; that is equal? to element
  (define (list-count-equal lst element)
    (define count 0)
    (do ((i 0 (+ i 1)))
        ((equal? i (length lst)))
        (begin
          (if (equal? element (list-ref lst i))
              (set! count (+ count 1)))
          ))
    count
    )

;; not satisfied with the original make-list
;; if fill is a list, i should be copying it in here
;; and not use the same referenced list all around
(define (my-make-list length fill)
  (define toret (list))
  (do ((i 0 (+ i 1)))
      ((equal? i length))
      (begin
        (set! toret (append toret (list (if (pair? fill)
                                            (list-copy fill)
                                            fill))))
        ))
  toret
  )

;; swap index n to m / m to n
(define (list-swap lst n m)
  (define nth (list-ref lst n))
  (define mth (list-ref lst m))
  ;; n to m
  (set! lst (remove (lambda (o) (eq? nth o)) lst))
  (set! lst (list-insert lst nth m))

  ;; m to n 
  (set! lst (remove (lambda (o) (eq? mth o)) lst))
  (set! lst (list-insert lst mth n))
  lst)
  
;; returns index for element of first match, #f otherwise
;;(define (list-index lst ele . equal-op)
;;  (define result #f)
;;  (if (null? equal-op)
;;      (begin
;;        (set! result (memq ele lst))
;;        (if result
;;            (- (length lst) (length result))
;;            #f)
;;        )
;;      (begin
;;        (do ((i 0 (+ i 1))
;;             (e (car lst) (car local-lst))
;;             (local-lst lst  (cdr local-lst))
;;             )
;;            ((or ((car equal-op) ele e)
;;                 (equal? i (length lst)))
;;             (begin
;;               (define to-return (- (length lst) (length local-lst)))
;;               (if (equal? to-return (length lst))
;;                   #f ;; not found
;;                   to-return ;; found
;;                   )
;;               )
;;             )
;;            #|(begin
;;              (display "equal op test ")(display ((car equal-op) ele e))(newline)
;;              (display "i ")(display i)(newline)
;;              (display "e ")(display e)(display " ele ")(display ele)(newline)
;;              (display "local-lst ")(display local-lst)(newline)
;;              ()
;;              )|#
;;            )
;;        )
;;      )
;;  )
