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

; kawa-specific graphics implementation
; in every buffer we set font, color, stroke, 

(begin
  (require "../kawa/ui/component.scm")
  (require "../kawa/file.scm")
  (require "../kawa/color.scm")
  (require "../kawa/ui/events.scm")
  )

; export
(module-export set-colour
               set-anti-alias
               make-square
               make-polygon my-make-polygon fill-polygon draw-polygon polygon-contains
               arc-fill draw-arc
               draw-oval
               draw-oval-fill
               make-rectangle rectangle-intersect? rectangle-contains? rectangle-fill draw-rectangle
               translate-xy
               make-basic-stroke make-dashed-stroke drawline draw-dashed-line
               set-stroke get-stroke
               draw-circle draw-circle-fill
               drawtext drawtext2
               drawtriangle drawfilledtriangle
               drawbitmap
               getpixel
               create-image get-image-buffer open-image-file image-supported?
               get-text-extent
               make-affinetransform set-affinetransform-scale set-affinetransform get-affinetransform
               scale-xy compose-transform
               create-buffer get-buffer get-buffer-width get-buffer-height clear-buffer
               update-canvas repaint
               set-gpanel-bgcolour
               create-canvas
               make-buffered-image-translucent
               tint-buffered-image
               overlap-image
               resize-canvas
               GPanel
               get-image-width get-image-height
               )
(module-static 'init-run)

; set a colour; args is bg-colour if using xor mode
(define (set-colour buffer the-colour style . args)
  (invoke (as <java.awt.Graphics> buffer) 'set-color the-colour)
  (if (and (eq? style 'xor) (pair? args))
      (invoke (as <java.awt.Graphics> buffer) 'setXORMode (car args))
      (invoke (as <java.awt.Graphics> buffer) 'setPaintMode)))

;; set antialiasing on for our graphics object
(define (set-anti-alias buffer :: <java.awt.Graphics>
                        bool)
  (invoke (as <java.awt.Graphics2D> buffer)
          'set-rendering-hint
          <java.awt.RenderingHints>:KEY_ANTIALIASING
          (if bool 
               <java.awt.RenderingHints>:VALUE_ANTIALIAS_ON
              <java.awt.RenderingHints>:VALUE_ANTIALIAS_OFF)
          ))

; this seems to duplicate drawtext (below), removed - Alex
;;(define (draw-text-on-canvas buffer :: <java.awt.Graphics> text :: <String> color x y)
;;  (set-colour buffer (make-colour-from-list color) 'solid)
;;  (invoke buffer 'drawString text x y))

(define (make-polygon x1 y1 x2 y2 x3 y3)
  (<java.awt.Polygon> (<int[]> x1 x2 x3) (<int[]> y1 y2 y3) 3))

(define (make-square x1 y1 x2 y2 x3 y3 x4 y4)
  (<java.awt.Polygon> (<int[]> x1 x2 x3 x4) (<int[]> y1 y2 y3 y4) 4))

(define (my-make-polygon x1 y1 x2 y2 x3 y3 x4 y4 x5 y5)
  (<java.awt.Polygon> (<int[]> x1 x2 x3 x4 x5) (<int[]> y1 y2 y3 y4 y5) 5))

(define (fill-polygon buffer :: <java.awt.Graphics> poly color)
  (set-colour buffer (make-colour-from-list color) 'solid)
  (invoke buffer 'fillPolygon poly))

(define (draw-polygon buffer :: <java.awt.Graphics> poly color)
  (set-colour buffer (make-colour-from-list color) 'solid)
  (invoke buffer 'drawPolygon poly))

(define (polygon-contains poly :: <java.awt.Polygon> x :: <int> y :: <int>)
  (invoke poly 'contains x y))

;; x y given is the center of the arc instead of the top left corner
(define (draw-arc buffer :: <java.awt.Graphics> x y w h start-angle arc-angle color)
  (set-colour buffer (make-colour-from-list color) 'solid)
  (invoke buffer 'drawArc 
          (- x (* w 0.5)) 
          (- y (* h 0.5)) w h start-angle arc-angle))

(define (arc-fill buffer :: <java.awt.Graphics> x y w h start-angle arc-angle color)
  (set-colour buffer (make-colour-from-list color) 'solid)
  (invoke buffer 'fillArc x y w h start-angle arc-angle))

(define (make-rectangle x y width height)
  (<java.awt.Rectangle> x y width height))

(define (rectangle-intersect? rect1 :: <java.awt.Rectangle>
                              rect2 :: <java.awt.Rectangle>)
  (invoke rect1 'intersects rect2))

(define (rectangle-contains? rect :: <java.awt.Rectangle> x ::<int> y :: <int>)
  (invoke rect 'contains x y))

; draw a filled rectangle
; style: 'solid 'xor 'transparent (what else?)
; style currently unused in kawa version
(define (rectangle-fill buffer :: <java.awt.Graphics> x-p1 y-p1 x-p2 y-p2 col style . args)
  (let ((the-colour (make-colour-from-list col)))
    (set-colour buffer the-colour style (if (pair? args) (car args)))
    ; brush?
    (invoke (as <java.awt.Graphics> buffer) 'fillRect
            x-p1
            y-p1
            (- x-p2 x-p1)
            (- y-p2 y-p1))))

(define (draw-rectangle buffer :: <java.awt.Graphics> x1 y1 x2 y2 col style)
  (let ((the-colour (make-colour-from-list col)))
    (set-colour buffer the-colour style)
    (invoke (as <java.awt.Graphics> buffer) 'drawRect
            x1
            y1
            (- x2 x1)
            (- y2 y1))
    )
  )

(define (translate-xy buffer x :: <int>  y :: <int>)
  (invoke (as <java.awt.Graphics> buffer) 'translate x y))

; makes a basic stroke
(define (make-basic-stroke width)
  (<java.awt.BasicStroke> width))

; makes a basic stroke with multiple input
(define (make-dashed-stroke width)
  (<java.awt.BasicStroke> width <java.awt.BasicStroke>:CAP_BUTT
                          <java.awt.BasicStroke>:JOIN_MITER
                          10.0 (<float[]> 10.0) 0.0))

; allows specifying of style which can be 'xor or 'solid for the call to set-colour
; note col is just a list here
(define (drawline buffer :: <java.awt.Graphics> x-p1 y-p1 x-p2 y-p2 col style . args)
  (let ((the-colour (make-colour-from-list col)))
    (set-colour buffer the-colour style (if (pair? args) (car args))) ;changed from solid 
    (if (and x-p1 y-p1 x-p2 y-p2)
        (invoke (as <java.awt.Graphics> buffer) 'drawLine x-p1 y-p1 x-p2 y-p2))
    ))


;draws dashed line
(define (draw-dashed-line buffer :: <java.awt.Graphics> x-p1 y-p1 x-p2 y-p2 col . stroke-thickness)
  (let* ((g2 :: <java.awt.Graphics2D> buffer)
         (initial-stroke (invoke g2 'getStroke))
         (dashed (<java.awt.BasicStroke> (if (null? stroke-thickness) 1.0 (car stroke-thickness))
                                         <java.awt.BasicStroke>:CAP_BUTT
                                         <java.awt.BasicStroke>:JOIN_MITER
                                         10.0 (<float[]> 10) 0.0)))
    (invoke g2 'setStroke dashed)
    (set-colour buffer (make-colour-from-list col) 'solid)
    (invoke g2 'draw (<java.awt.geom.Line2D$Double> x-p1 y-p1 x-p2 y-p2))
    (invoke g2 'setStroke initial-stroke)))

; set the stroke for given buffer
(define (set-stroke buffer :: <java.awt.Graphics> stroke :: <java.awt.BasicStroke>)
  (let ((the-graphics2D :: <java.awt.Graphics2D> buffer))
    (invoke the-graphics2D 'setStroke stroke)))

; get current stroke

(define (get-stroke buffer :: <java.awt.Graphics>) :: <java.awt.BasicStroke>
  (let ((the-graphics2D :: <java.awt.Graphics2D> buffer))
    (invoke the-graphics2D 'getStroke)))

; width and height are radius
(define (draw-oval buffer :: <java.awt.Graphics2D> x-pos y-pos angle width height color)
  (let (( orig (invoke buffer 'get-transform))
        ( af (<java.awt.geom.AffineTransform>)))
    (invoke af 'rotate angle x-pos y-pos)
    
    (define e-shape (<java.awt.geom.Ellipse2D$Double> (- x-pos width) (- y-pos height) (* 2 width) (* 2 height)))
    (define e-shape-area (<java.awt.geom.Area> e-shape))
    (invoke e-shape-area 'transform af)
    ;(invoke buffer 'set-transform af)
    (set-colour buffer (make-colour-from-list color) 'solid)
    (invoke buffer 'draw e-shape-area)
    
    ;(invoke buffer 'draw-oval (- x-pos width) (- y-pos height) (* 2 width) (* 2 height))
    ;(invoke buffer 'set-transform orig)
    ))

; width and height are radius
(define (draw-oval-fill buffer :: <java.awt.Graphics2D> x-pos y-pos angle width height color . args)
  (let (( orig (invoke buffer 'get-transform))
        ( af (<java.awt.geom.AffineTransform>)))
    (invoke af 'rotate angle x-pos y-pos)
    
    (define e-shape (<java.awt.geom.Ellipse2D$Double> (- x-pos width) (- y-pos height) (* 2 width) (* 2 height)))
    (define e-shape-area (<java.awt.geom.Area> e-shape))
    (invoke e-shape-area 'transform af)
    ;(invoke buffer 'set-transform af)
    (set-colour buffer (make-colour-from-list color) 'solid)
    (if (and (pair? args) (equal? (car args) 'aalias))
        (set-anti-alias buffer #t)
        (set-anti-alias buffer #f)
        )
    ;(invoke buffer 'fill-oval (- x-pos width) (- y-pos height) (* 2 width) (* 2 height))
    (invoke buffer 'fill e-shape-area)
    ;(invoke buffer 'set-transform orig)
    )
  )

;;draw a circle black by default
(define (draw-circle buffer :: <java.awt.Graphics> x-pos y-pos radius color . args)
  (if (and (pair? args) (equal? (car args) 'xor))
      (set-colour buffer (make-colour-from-list color) 'xor)
      (set-colour buffer (make-colour-from-list color) 'solid))
  (if (and (pair? args) (equal? (car args) 'aalias))
      (set-anti-alias buffer #t)
      (set-anti-alias buffer #f)
      )
  (invoke (as <java.awt.Graphics> buffer) 'draw-oval (- x-pos radius) (- y-pos radius) (* 2 radius) (* 2 radius))
  )

(define (draw-circle-fill buffer :: <java.awt.Graphics> x-pos y-pos radius color . args)
  (if (and (pair? args) (equal? (car args) 'xor))
      (set-colour buffer (make-colour-from-list color) 'xor)
      (set-colour buffer (make-colour-from-list color) 'solid))
  (if (and (pair? args) (equal? (car args) 'aalias))
      (set-anti-alias buffer #t)
      (set-anti-alias buffer #f))
  (invoke (as <java.awt.Graphics> buffer) 'fill-oval (- x-pos radius) (- y-pos radius) (* 2 radius) (* 2 radius))
  )

; draw some text;
; parameters are: image x y colour background-color message
; provide -1 for no background colour
; Note: for some reason the call to 'drawString requires that the jar file be signed
(define (drawtext buffer :: <java.awt.Graphics> x :: <int> y :: <int> col bg-col text :: <string>)
  (let ((the-colour (make-colour-from-list col))
        (the-text :: <java.lang.String> (invoke text 'toString)))
    (set-colour buffer the-colour 'solid)
;    (display "just before crash ")(display (list buffer text x y))(newline)
    (invoke buffer 'drawString the-text x y)))

; Note: text might be immutable (java.lang.string) or mutable (gnu.lists.FString), so need to use the common interface,
; java.lang.CharSequence, and extract the string using 'toString
(define (drawtext2 buffer :: <java.awt.Graphics> x :: <int> y :: <int> col bg-col text :: <string> font)
  (let ((the-colour (make-colour-from-list col))
        (curr-font (invoke buffer 'getFont))
        (the-text :: <java.lang.String> (invoke text 'toString)))
    (set-colour buffer the-colour 'solid)

    (invoke buffer 'setFont font)
    (invoke buffer 'drawString the-text x y)
    (invoke buffer 'setFont curr-font)))

; draw a (non-filled) triangle
(define (drawtriangle buffer x1 y1 x2 y2 x3 y3 col)
  (let ((the-colour (make-colour-from-list col)))
    (set-colour buffer the-colour 'solid)
    ; brush?
    (invoke (as <java.awt.Graphics> buffer) 'drawPolygon
            (<int[]> x1 x2 x3)
            (<int[]> y1 y2 y3)
            3)))

; draw filled triangle
(define (drawfilledtriangle buffer x1 y1 x2 y2 x3 y3 col)
  (let ((the-colour (make-colour-from-list col)))
    (set-colour buffer the-colour 'solid)
    ; brush?
    (invoke (as <java.awt.Graphics> buffer) 'fillPolygon
            (<int[]> x1 x2 x3)
            (<int[]> y1 y2 y3)
            3)))

; draw a bitmap (a buffered image)
; smooth-scale? determines whether to use smooth scaling (slower)
; observer is the imageObserver that is notified when image is ready,
; such as a canvas
; note: angle added in the ioblocks version, need to merge, not done
(define (drawbitmap buffer :: <java.awt.Graphics>
                    observer
                    bitmap-img :: <java.awt.image.BufferedImage>
                    x y w h angle smooth-scale?)
  (let ((scaled-img
         (if smooth-scale?
             (invoke bitmap-img 'getScaledInstance
                     w h <java.awt.Image>:SCALE_SMOOTH)
             bitmap-img)))
    (invoke buffer 'drawImage scaled-img x y w h observer)))

; get pixel value at given position
(define (getpixel in-glcanvas x y)
;      (let ((the-colour (make-object color% 0 0 0)))
;        (send buffer get-pixel x y the-colour)
;        (colour (send the-colour red) (send the-colour green) (send the-colour blue)))
  #f)

;; given a bufferedImage, it will return a bufferedImage exactly the same except translucent
(define (make-buffered-image-translucent src-buffer-img :: <java.awt.image.BufferedImage>
                                         translucency)
  ;; need to create a buffered image of imageType TRANSLUCENT
  (define new-trans-buffer-img (<java.awt.image.BufferedImage>
                                (invoke src-buffer-img 'get-width)
                                (invoke src-buffer-img 'get-height)
                                <java.awt.image.BufferedImage>:TRANSLUCENT))
  ;; initialize the graphics (this is a Graphics2D object)
  (define new-trans-buffer-graphics (invoke new-trans-buffer-img 'create-graphics))

  ;; set a alpha composite (the method to mixes translucent color together)  
  (invoke new-trans-buffer-graphics 'set-composite
          (invoke-static <java.awt.AlphaComposite> 'get-instance <java.awt.AlphaComposite>:SRC_OVER translucency))
  (invoke (as <java.awt.Graphics2D> new-trans-buffer-graphics) 'draw-image src-buffer-img #!null 0 0)

  ;;return the new-trans-buffer-img
  new-trans-buffer-img)


;; put one layer of translucent image over the original to tint it
;; if image has transparent background it will fit the shape of it
(define (tint-buffered-image src-buffer-img :: <java.awt.image.BufferedImage>
                             colour-list)
  ;; the layer of tint (a buffered image) we are going to paint onto the current image
  (define new-tint-layer-bi (<java.awt.image.BufferedImage>
                             (invoke src-buffer-img 'get-width)
                             (invoke src-buffer-img 'get-height)
                             <java.awt.image.BufferedImage>:TRANSLUCENT))
  ;; initialize the graphics 
  (define new-tint-graphics (invoke new-tint-layer-bi 'create-graphics))

  ;; painting only where it is not transparent
  (invoke new-tint-graphics 'draw-image src-buffer-img #!null 0 0)
  ;(set-colour new-tint-graphics  (make-colour-from-list (list 0 0 200)) 'xor)
  (invoke new-tint-graphics 'setXORMode (make-colour-from-list (list (car colour-list)
                                                                     (cadr colour-list)
                                                                     (caddr colour-list))))
  (invoke new-tint-graphics 'draw-image src-buffer-img #!null 0 0)
  (invoke new-tint-graphics 'setPaintMode)

  (define ret-img (<java.awt.image.BufferedImage>
                   (invoke src-buffer-img 'get-width)
                   (invoke src-buffer-img 'get-height)
                   <java.awt.image.BufferedImage>:TRANSLUCENT))
  (define ret-graphics (invoke ret-img 'create-graphics))
  (invoke ret-graphics 'set-composite
          (invoke-static <java.awt.AlphaComposite> 'get-instance <java.awt.AlphaComposite>:SRC_OVER 0.5))
  (invoke ret-graphics 'draw-image new-tint-layer-bi #!null 0 0)

;  ;; release the memory used by this temp buffered image
  (invoke new-tint-graphics 'dispose)
  (invoke ret-graphics 'dispose)

;  ;; make src-buffer-img translucency compatible by setting the imageType to translucent
  (set! src-buffer-img (make-buffered-image-translucent src-buffer-img 1.0))
;  (invoke (as <java.awt.Graphics> (get-image-buffer src-buffer-img)) 'draw-image ret-img #!null 0 0 )
  
  ;;NOTE i changed this to fix the error that comes up when 
  ; requiring graphics.scm cause by the commented line
   (invoke (as <java.awt.Graphics2D> (get-image-buffer src-buffer-img))
           'draw-image ret-img #!null 0 0 )

  ;; return this
  src-buffer-img
  #t
  )

;; stack 2 image, image1 has alpha/translucency value of transition-state
;; image2 has alpha/translucency value of 1-transition-state
;; note transition-state should be 0 to 1

(define (overlap-image image1 :: <java.awt.image.BufferedImage>
                       image2 :: <java.awt.image.BufferedImage> transition-state)

  ;; make image1 translucent by (1 - transition-state)
  (define image-trans1 (make-buffered-image-translucent image1 (- 1 transition-state)))
  (define image-trans1-graphics (get-image-buffer image-trans1))

  ;; set composite for anything draw onto this to have translucency of transition-state
  (invoke (as <java.awt.Graphics2D> image-trans1-graphics) 'set-composite
          (invoke-static <java.awt.AlphaComposite> 'get-instance <java.awt.AlphaComposite>:SRC_OVER transition-state))

  ;; draw image2 onto image1 with scaled to the width and height of image1
  (invoke (as <java.awt.Graphics2D> image-trans1-graphics) 'draw-image image2 0 0
          (invoke image1 'get-width)
          (invoke image1 'get-height) #!null)
  ;                                                 #!null 0 0)
  ;; return image1
  image-trans1
  )


; create an image for drawing
(define (create-image w h) :: <java.awt.image.BufferedImage>
  (<java.awt.image.BufferedImage> w h <java.awt.image.BufferedImage>:TYPE_3BYTE_BGR))

(define (get-image-buffer in-image :: <java.awt.image.BufferedImage>)
  (invoke in-image 'getGraphics))

(define (get-image-width in-image :: <java.awt.image.BufferedImage>)
  (invoke in-image 'get-width))

(define (get-image-height in-image :: <java.awt.image.BufferedImage>)
  (invoke in-image 'get-height))

;; reading in an image file (example jpg files)
(define (open-image-file filename :: <String>) :: <java.awt.image.BufferedImage>
  ;(format #t " open-image-file: filename ~a~%~!" filename)
  (let ((image-file (make-file filename)))
    (try-catch
        (invoke-static <javax.imageio.ImageIO> 'read image-file)

      (ex <java.lang.Throwable>
          (begin
            (display (*:toString ex))(newline)
            (*:printStackTrace ex)
            )))))

(define (image-supported? filename :: <String>)
  (define return #f)
  (if (file-exists? filename)
      (try-catch
          (let ((image-file (make-file filename))
                (reader #f))
            (define image-input-stream
              (invoke-static <javax.imageio.ImageIO>
                             'create-image-input-stream image-file))
            (define iterator
              (invoke-static <javax.imageio.ImageIO>
                             'get-image-readers image-input-stream))
            (if (invoke iterator 'has-next)
                (begin
                  (set! reader (invoke iterator 'next))
                  (set! return #t)))
            (invoke image-input-stream 'close)

            ;; for debugging purposes, we can get the format of the image from here
;            (if reader
;                (begin
;                  (display (invoke reader 'get-format-name))
;                  (newline)))

            )
        (ex <java.lang.Throwable>
            (begin
              (display (*:toString ex))(newline)
              ;(display "the file might not be in the supported encoding format")(newline)
              )))
      (format #t " image-supported?: file at ~a does not exist~%~!" filename))

  return
  )

; new stuff for graph-editor

; get text extent
;; teongleong: I dont like the fact that the default font is being changed 
;; yet we have no idea a font of what size is used after the change to accomodate text-height

(define (get-text-extent buffer data text-height)
  (let* ((g2 :: <java.awt.Graphics2D> buffer)
         (frc :: <java.awt.font.FontRenderContext> (invoke g2 'getFontRenderContext))
         (fs :: <float> text-height)
         (font :: <java.awt.Font> (invoke (invoke g2 'getFont) 'deriveFont fs)))
    (invoke g2 'setFont font)
    (let* ((sw :: <float> (invoke (invoke font 'getStringBounds data frc) 'getWidth))
           (lm :: <java.awt.font.LineMetrics> (invoke font 'getLineMetrics data frc))
           (sa :: <float> (invoke lm 'getAscent))
           (sd :: <float> (invoke lm 'getDescent))
           (sh :: <float> (+ sa sd))
           )
      (values sw sh sd sa))))

; create an affine transform
(define (make-affinetransform)
  (<java.awt.geom.AffineTransform>))

; set scale for a transform
(define (set-affinetransform-scale tx :: <java.awt.geom.AffineTransform>
                                   in-xscale in-yscale)
  (invoke tx 'scale in-xscale in-yscale))

; set the transform for a graphics2D
(define (set-affinetransform g :: <java.awt.Graphics2D>
                                tx :: <java.awt.geom.AffineTransform>)
  (invoke g 'setTransform tx))

; get the transform from a graphics2D
(define (get-affinetransform g :: <java.awt.Graphics2D>) :: <java.awt.geom.AffineTransform>
  (invoke g 'getTransform))

; compose transform with transform in graphics2D
(define (compose-transform g :: <java.awt.Graphics2D>
                           tx :: <java.awt.geom.AffineTransform>)
  (invoke g 'transform tx))

; scale a Graphics2D
(define (scale-xy g :: <java.awt.Graphics2D>
                        sx sy)
  (invoke g 'scale sx sy))

;
; wrapper methods for canvas
;

; (re)create the offscreen buffer
(define (create-buffer in-glcanvas)
  (if in-glcanvas
      (invoke (as GPanel in-glcanvas) 'create-buffer)
      #f))

; get the offscreen buffer
(define (get-buffer in-glcanvas)
  (if in-glcanvas
      (invoke (as GPanel in-glcanvas) 'get-buffer-g)))

; get width of buffer
(define (get-buffer-width in-glcanvas)
  (if in-glcanvas
      (let ((buffer-img (invoke (as GPanel in-glcanvas) 'get-buffer-img)))
        (invoke (as <java.awt.image.BufferedImage> buffer-img) 'getWidth))))

; get height of buffer
(define (get-buffer-height in-glcanvas)
  (if in-glcanvas
      (let ((buffer-img (invoke (as GPanel in-glcanvas) 'get-buffer-img)))
        (invoke (as <java.awt.image.BufferedImage> buffer-img) 'getHeight))))

; clear the offscreen buffer
(define (clear-buffer in-glcanvas)
  (if in-glcanvas
      (invoke (as GPanel in-glcanvas) 'clear-buffer)))

; blit offscreen buffer to canvas
(define (update-canvas in-glcanvas)
  (if in-glcanvas
      (invoke (as GPanel in-glcanvas) 'update-canvas (invoke (as GPanel in-glcanvas) 'getGraphics))))

; repaint the canvas - will call paintComponent correctly
; should change to revalidate
(define (repaint in-glcanvas)
  (if in-glcanvas
      (invoke (as GPanel in-glcanvas) 'repaint)))

; get total free memory
(define (get-total-free-memory)
  (let* ((rt (java.lang.Runtime:getRuntime))
         (maxMem (/ (invoke (as <java.lang.Runtime> rt) 'maxMemory) MEGABYTES))
         (allocMem (/ (invoke (as <java.lang.Runtime> rt) 'totalMemory) MEGABYTES))
         (freeMem (/ (invoke (as <java.lang.Runtime> rt) 'freeMemory) MEGABYTES)))
    (+ freeMem (- maxMem allocMem))))

; get memory required for buffer
(define (get-required-memory w h)
  (/ (* w h 4) MEGABYTES))

(define MEGABYTES 1048576.0)

; subclass of JPanel that maintains an offscreen buffer
(define-simple-class GPanel (<javax.swing.JPanel>) ; changed to define-simple-class, gave it a class name <gpanel>
  ; buffer
  (buffer-g :: <java.awt.Graphics>)
  (buffer-img :: <java.awt.image.BufferedImage>)
  (bgcolour :: <java.awt.Color>)
  (my-refresh-callback)

  ; set refresh callback
  ((set-refresh-callback! in-callback)
   (set! my-refresh-callback in-callback))

  ; get the buffer image
  ((get-buffer-img) :: <java.awt.image.BufferedImage>
   buffer-img)

  ; get the buffer 
  ((get-buffer-g) :: <java.awt.Graphics>
   buffer-g)

  ; (re)create offscreen bitmap to match current size of GPanel
  ((create-buffer) :: <boolean>
   (let* ((w (invoke (this) 'getWidth))
          (h (invoke (this) 'getHeight))
          (totalFreeMem (get-total-free-memory))
          (requiredMem (get-required-memory w h))
          (got-memory #t))
     
     ; is there enough mem for the buffer?
     (if (> requiredMem totalFreeMem)
         ; if not, try to garbage collect and check again
         (begin
           (format #t "graphics-kawa: create-buffer: garbage collecting~%~!")
           (java.lang.System:gc)
           (if (> requiredMem (get-total-free-memory))
               (set! got-memory #f))))

     (if got-memory
         ; should be ok, but try-catch anyway
         (try-catch
             (begin
               (set! buffer-img (create-image w h))
               (set! buffer-g (get-image-buffer buffer-img))
               ;g2d.setRenderingHints(Graphics2D.ANTIALIASING,
;        Graphics2D.ANTIALIAS_ON)
;     (invoke (as <java.awt.Graphics2D> buffer-g) 
;             'set-rendering-hint
;             <java.awt.RenderingHints>:KEY_ANTIALIASING
;             <java.awt.RenderingHints>:VALUE_ANTIALIAS_ON)
               (clear-buffer)
               #t)
           (ex <java.lang.Throwable>
               (begin
                 (display (*:toString ex))(newline)
                 (*:printStackTrace ex)
                 #f)))
         (begin
           (format #t "graphics-kawa: create-buffer: not enough memory to create buffer!~%~!")
           #f))))

  ((set-bgcolour colour :: <java.awt.Color>) :: <void>
   (set! bgcolour colour)
   (set-background-color (this) bgcolour))

  ; clear the offscreen buffer
  ((clear-buffer) :: <void>
   (let* ((w (invoke (this) 'getWidth))
          (h (invoke (this) 'getHeight)))
     (invoke buffer-g 'set-color (invoke (this) 'getBackground))
     (invoke buffer-g 'fillRect 0 0 w h)
     ))

  ; update offscreen bitmap to canvas
  ; this should be called at the end of my-refresh-callback
  ((update-canvas g :: <java.awt.Graphics>) :: <void>
   (if (not (eq? #!null g))
       (invoke g 'drawImage buffer-img 0 0 (this)))
   #t)

  ; repaint component
  ((paintComponent g :: <java.awt.Graphics>) :: <void>
   (let ((theGraphics (invoke (this) 'getComponentGraphics g)))
     ;(format #t "graphics-kawa: paintComponent~%~!")
     ; call paintComponent in parent first
     (invoke-special <javax.swing.JComponent> (this) 'paintComponent theGraphics)
     ; now do our drawing (if any)
     (if (procedure? my-refresh-callback)
         (my-refresh-callback))
     
     (update-canvas theGraphics))))

(define (set-gpanel-bgcolour panel :: GPanel
                             colour :: <java.awt.Color>)
  (invoke panel 'set-bgcolour colour))

; create a GPanel and attach callbacks
; NOTE: in-mouse-callback is called when the GPanel receives mouse events, and
; in-refresh-callback gets called when the GPanel is repainted.
; in-resize-callback gets called on resize
(define (create-canvas w h in-mouse-callback in-resize-callback in-refresh-callback)
  (let ((the-canvas :: GPanel (GPanel)))
    (invoke (as <javax.swing.JPanel> the-canvas) 'setSize (<java.awt.Dimension> w h))
    (invoke (as <javax.swing.JPanel> the-canvas) 'setPreferredSize (<java.awt.Dimension> w h))
    ; getWidth, getHeight still returns 0 without this line

    ; mouse listeners
    (add-mouselistener the-canvas (make-mouselistener in-mouse-callback))
    (add-mousemotionlistener the-canvas (make-mousemotionlistener in-mouse-callback))

    ; handle resize
    (if (procedure? in-resize-callback)
        (add-componentlistener the-canvas (make-componentlistener (lambda (e) 'ok) ;hidden
                                                                  (lambda (e) 'ok) ;moved
                                                                  in-resize-callback
                                                                  (lambda (e) 'ok) ;shown
                                                                  )))

    ; handle repaint
    (invoke the-canvas 'set-refresh-callback! in-refresh-callback)

    ; create the buffer
    (create-buffer the-canvas)

    the-canvas))

(define (resize-canvas gpanel w h)
  (invoke (as <javax.swing.JPanel> gpanel) 'setSize (<java.awt.Dimension> w h))
  (invoke (as <javax.swing.JPanel> gpanel) 'setPreferredSize (<java.awt.Dimension> w h))
  (create-buffer gpanel)
  )

;;(define (make-image)
;;  <java.awt.Image>)

;;(define (make-media-tracker comp)
;;  (<java.awt.MediaTracker> comp))

;;(define (draw-image buffer img x y)
;;  (invoke buffer 'drawImage img x y,pacmanx+1,pacmany+1,<java.awt.image.ImageObserver>))

;;(define (get-image img)
;;  (invoke (invoke-static <java.awt.Toolkit> 'getDefaultToolkit) 'getImage img))