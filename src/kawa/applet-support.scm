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

; procedures that are useful to enable support for applets

(require "strings.scm")
(require "ui/panel.scm")

(module-export is-applet? set-is-applet!
               set-the-applet! get-the-applet
               get-docbase-filename
               get-image-applet
               get-document-base)
(module-static 'init-run)

                                        ; is it an applet?
(define is-applet #f)
(define (is-applet?)
  is-applet)
(define (set-is-applet! in-flag)
  (set! is-applet in-flag))

                                        ; ref to applet
(define the-applet #f)
(define (set-the-applet! in-applet)
  (set! the-applet in-applet)
  )
(define (get-the-applet)
  the-applet)

(define (ends-with-html? string)
  (if (or (equal? (substring string
                             (- (string-length string) 5)
                             (string-length string)) ".html")
          (equal? (substring string
                             (- (string-length string) 4)
                             (string-length string)) ".htm"))
      #t #f)
  )

                                        ; construct path to a file from the same directory as the .jar file
                                        ; returns a filename not a file
                                        ; if not an applet, just returns the same filename

;; the-docbase might end with the html
;; or just the directory the applet is in
(define (get-docbase-filename in-filename)
  (if the-applet
      (let* ((the-docbase (get-document-base the-applet)) ; get the document base for applet
             (the-docbase-string (URL->string the-docbase)) ; convert from path to string
             (the-docbase-dir #f)) ; hack to get directory
        ;; +1 for the slash that is not accounted for in path-last

        (display "get-docbase-filename in-filename ")(display in-filename)(newline)
        ;; linux/mac difference hack
        (let ((last-path-name-length (string-length (path-last the-docbase))))

          ;; fixing the condition where getting docbase of applet
          ;; does not return the html filename in the string
          (if (ends-with-html? the-docbase-string)
              (begin
                ;; remove its ok

                (set! the-docbase-dir
                      (substring the-docbase-string
                                 0 (- (string-length the-docbase-string)
                                      last-path-name-length)))
                )
              ;; if not no need to remove the last element in the directory
              (begin
                (set! the-docbase-dir
                      the-docbase-string)
                )
              )

          (display "the path last ")(display (path-last the-docbase))(newline)
          (display "base ")(display the-docbase)(newline)
          (display "string ")(display the-docbase-string)(newline)
          (display "dir ")(display the-docbase-dir)(newline)
          (string-append the-docbase-dir in-filename)
          ))
      in-filename))

                                        ; load an image; adds the docbase to filename
                                        ; with help from http://stage.itp.nyu.edu/~mjl359/netex/finalpres.html
(define (get-image-applet in-filename) :: <java.awt.image.BufferedImage>
  (format #t "get-image-applet: filename ~a~%~!" in-filename)
  ;; dont check for the-applet since we wont set it thsi early
  (if the-applet
      (try-catch
          (begin
            (let* ((the-image-filename (string->URL (get-docbase-filename in-filename)))
                   (the-image :: <java.awt.Image> (get-image the-applet the-image-filename))
                   (tracker :: <java.awt.MediaTracker> (<java.awt.MediaTracker> the-applet)))
                                        ;(format #t "get-image-applet: the-image-filename ~a~%~!" the-image-filename)
              (newline)
              (display "doc base ")(display (get-docbase-filename in-filename))(newline)
              (display "the image ")(display the-image)(newline)
              (display "image width ")(display (invoke the-image 'getWidth the-applet))(newline)
              (display "image height ")(display (invoke the-image 'getHeight the-applet))(newline)
              (invoke tracker 'addImage the-image 0)
              (invoke tracker 'waitForID 0)

              (let* ((the-image-w (invoke the-image 'getWidth the-applet))
                     (the-image-h (invoke the-image 'getHeight the-applet))
                     (the-buffered-image :: <java.awt.image.BufferedImage> (<java.awt.image.BufferedImage>
                                                                            the-image-w the-image-h
                                                                            <java.awt.image.BufferedImage>:TYPE_3BYTE_BGR))
                     (the-buffered-image-g :: <java.awt.Graphics2D> (invoke the-buffered-image 'createGraphics)))
                                        ;(format #t "get-image-applet: the-image-w ~a, the-image-h ~a, the-buffered-image ~a, the-buffered-image-g ~a~%~!" 
                                        ;        the-image-w the-image-h the-buffered-image the-buffered-image-g)

                (invoke the-buffered-image-g 'drawImage the-image 0 0 (make-panel))
                (invoke the-buffered-image-g 'dispose)
                the-buffered-image))
            )
        (ex <java.lang.Throwable>
            (begin
              (display (*:toString ex))(newline)
              (*:printStackTrace ex))))
      ;; return a small new image
      (begin
        (display "applet not set ")(newline)
        (<java.awt.image.BufferedImage> 20 20 <java.awt.image.BufferedImage>:TYPE_3BYTE_BGR)

        )))

; get image in an applet - helper fn for get-image-applet
(define (get-image in-applet :: <java.applet.Applet> in-filename) :: <java.awt.Image>
  (try-catch
      (begin
        (display "get-image in-filename ")(display in-filename)(newline)
        (invoke in-applet 'get-image in-filename)
        )
    (ex <java.lang.Throwable>
        (begin
          (display (*:toString ex))(newline)
          (*:printStackTrace ex)))))

; get document base for an applet
(define (get-document-base in-applet :: <java.applet.Applet>) :: <java.net.URL>
  (invoke in-applet 'getDocumentBase))

