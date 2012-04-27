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

; wrapper for java sound
(require "file.scm") ; for make-file
(require "system.scm") ; for get-os-name

; export
(module-export audio-filetype-wave audio-format-pcm-signed audio-format-ulaw audio-format-alaw
               make-audio-format make-data-line-info
               audio-get-line-using-info audio-line-open audio-line-start
               make-audio-input-stream audio-get-format-sample-rate audio-get-format-frame-size
               audio-line-read audio-supported? play-audio-file 
               stop-audio-clip close-audio-clip loop-audio-clip loop-audio-clip-forever
               play-audio-file-linux
               two-bytes-to-short-little-endian)
(module-static 'init-run)

(define audio-filetype-wave <javax.sound.sampled.AudioFileFormat$Type>:WAVE)
(define audio-format-pcm-signed <javax.sound.sampled.AudioFormat$Encoding>:PCM_SIGNED) 
(define audio-format-ulaw <javax.sound.sampled.AudioFormat$Encoding>:ULAW)
(define audio-format-alaw <javax.sound.sampled.AudioFormat$Encoding>:ALAW)

(define (make-audio-format encoding
                           sample-rate :: float
                           sample-size-in-bits :: int
                           channels :: int
                           frame-size :: int
                           frame-rate :: int 
                           big-endian :: boolean) :: <javax.sound.sampled.AudioFormat>
  (<javax.sound.sampled.AudioFormat> encoding sample-rate sample-size-in-bits channels frame-size frame-rate big-endian))

(define (make-data-line-info dataline-class 
                             audio-format :: <javax.sound.sampled.AudioFormat>) :: <javax.sound.sampled.DataLine$Info>
  (<javax.sound.sampled.DataLine$Info> dataline-class audio-format))

(define (audio-get-line-using-info info :: <javax.sound.sampled.DataLine$Info>) :: <javax.sound.sampled.TargetDataLine>
  (invoke-static <javax.sound.sampled.AudioSystem> 'getLine info))

(define (audio-line-open line :: <javax.sound.sampled.TargetDataLine>
                         format :: <javax.sound.sampled.AudioFormat>)
  (display line)(newline)
  (display format)(newline)
  (invoke (as <javax.sound.sampled.TargetDataLine> line) 'open format))
(define (audio-line-start line :: <javax.sound.sampled.TargetDataLine>)
  (invoke line 'start))

(define (make-audio-input-stream line :: <javax.sound.sampled.TargetDataLine>)
  (<javax.sound.sampled.AudioInputStream> line))

(define (audio-get-format-sample-rate format :: <javax.sound.sampled.AudioFormat>)
  (invoke format 'get-sample-rate))
(define (audio-get-format-frame-size format :: <javax.sound.sampled.AudioFormat>)
  (invoke format 'get-frame-size))

(define (audio-line-read line :: <javax.sound.sampled.TargetDataLine>
                         buffer 
                         offset read-length)
  (invoke line 'read buffer offset read-length))

;; second | first     little endian arrangement (first and second are both byte)
(define (two-bytes-to-short-little-endian first second)
  (let ((two (arithmetic-shift (bitwise-and second #xFF) 8))
        (one (arithmetic-shift (bitwise-and first #xFF) 0)))
    (let ((val (+ one two)))
    ;(display four)(display three)(display two)(display one)(newline)
       (if (>=  val 32768)
           (- val 65536)
           val)
       )))

;; checking whether audio is supported and playable by procedure play-audio-file
(define (audio-supported? filename :: <String>) ;:: <javax.sound.sampled.AudioInputStream>
  (define return-is #f) 
  (define supported? #f)
  (if (file-exists? filename)
      (let ((sound-file (make-file filename)))
        (try-catch
            (begin
              (set! return-is (invoke-static <javax.sound.sampled.AudioSystem> 'getAudioInputStream sound-file))
              )

          (ex <java.lang.Throwable>
              (begin
                (display (*:toString ex))(newline)
                (display "audio-supported?: the file might not be a valid audio or not in a supported format")(newline)
                )))
        (if return-is
            (try-catch
                (begin
                  (define audio-format (invoke (as <javax.sound.sampled.AudioInputStream> return-is) 'get-format))
                  (define dataline-info (<javax.sound.sampled.DataLine$Info>
                                         <javax.sound.sampled.SourceDataLine> audio-format))
                  (set! supported? (invoke-static <javax.sound.sampled.AudioSystem>
                                                  'is-line-supported dataline-info))
                  )

              (ex <java.lang.Throwable>
                  (begin
                    (display (*:toString ex))(newline)
                    (display "the file might not be in the supported encoding format")(newline)
                    ))))
        )
      (begin (display "filename to play-audio-file do not exist, check path: ")(display filename)(newline)))
  
  ;; only true if return-is is a valid input stream and supported? is true
  (and return-is supported?)
  )

(define (get-audio-input-stream audio-file :: <java.io.File>)
  (invoke-static <javax.sound.sampled.AudioSystem> 'get-audio-input-stream audio-file))

; play an audio file
; note: this loads the entire file, which causes memory errors for
; very large files, may want to provide streaming as well
(define (play-audio-file audio-file) ;:: <javax.sound.sampled.Clip>
;  ;; test for supported audio encoding and play
  (try-catch
      (begin
        ;; assume its an audio file
        (define ais (get-audio-input-stream audio-file))
        (define audio-format (invoke (as <javax.sound.sampled.AudioInputStream> ais) 'get-format))
        (define dataline-info (<javax.sound.sampled.DataLine$Info>
                               <javax.sound.sampled.SourceDataLine> audio-format))
        (define supported? (invoke-static <javax.sound.sampled.AudioSystem>
                                          'is-line-supported dataline-info))
;        (display "ais ")(display ais)(newline)
;        (display "audio format ")(display audio-format)(newline)
;        (display "dataline-info ")(display dataline-info)(newline)
;        (display "supported? ")(display supported?)(newline)
        (if supported?
            (begin
              (define clip (invoke-static <javax.sound.sampled.AudioSystem> 'get-clip))
;              (display "got clip")(newline)
;              (display "ais class ")(display (invoke ais 'get-class))(newline)
;              (display "clip class ")(display (invoke clip 'get-class))(newline)
              (invoke clip 'open ais)
;              (display "opened ais ")(newline)
              (invoke (as <javax.sound.sampled.AudioInputStream> ais) 'close)
;              (display "closed ais ")(newline)
              (invoke clip 'start)
;              (display "ask clip to start ")(newline)
              clip
              )
            (begin
              (display "Audio encoding type not supported (file type independent)")(newline)
              #!null)))
    (ex <java.lang.Throwable>
        (begin
          (display (*:toString ex))(newline)
          )))
  )

; stop a clip
(define (stop-audio-clip  clip :: <javax.sound.sampled.Clip>)
  (invoke clip 'stop))

; close a clip - after this it can't be restarted
(define (close-audio-clip  clip :: <javax.sound.sampled.Clip>)
  (invoke clip 'close))

; loop a clip
(define (loop-audio-clip  clip :: <javax.sound.sampled.Clip> count :: <int>)
  (invoke clip 'loop count))

; loop a clip forever
(define (loop-audio-clip-forever  clip :: <javax.sound.sampled.Clip>)
  (invoke clip 'loop javax.sound.sampled.Clip:LOOP_CONTINUOUSLY))

(define (play-audio-file-linux audio-filedir)
  (define curr-rt (invoke-static <java.lang.Runtime> 'get-runtime))
  (display "get os name ")(display (get-os-name))(newline)
  ;(define cmd-str (string-append "sh -c aplay " audio-file))
  (define cmd-str (string-append "aplay " audio-filedir))
  
  ;(define cmd-str "ls")
  (display cmd-str)(newline)
  (display (invoke curr-rt 'exec cmd-str))(newline)
  )
  

;;AudioFormat audioFormat = audioInputStream.getFormat();

;;DataLine.Info info = new DataLine.Info(SourceDataLine.class,
;;audioFormat);
;;try {
;;line = (SourceDataLine) AudioSystem.getLine(info);


;;line.open(audioFormat);
;;} catch (LineUnavailableException e) {
;;e.printStackTrace();
;;System.exit(1);
;;} catch (Exception e) {
;;e.printStackTrace();
;;System.exit(1);
;;}
;;line.start();
;;int nBytesRead = 0;
;;byte[] abData = new byte[EXTERNAL_BUFFER_SIZE];
;;while (nBytesRead != -1 && play) {
;;try {
;;nBytesRead = audioInputStream.read(abData, 0, abData.length);
;;} catch (IOException e) {
;;e.printStackTrace();
;;}
;;if (nBytesRead >= 0) {
;;int nBytesWritten = line.write(abData, 0, nBytesRead);
;;}
;;}
;;line.drain();
;;line.close(); 
;;}
;;}
  
  
            
;(if once
;      (begin
;        (define filename "langs/game-scheme/beep_1.au")
;        (if (file-exists? filename)
;            (begin (display " mg file exist " )(newline))
;            (begin (display " fiel missing ")(newline)))
;        (define audio-file (make-file filename))
;        (define ais #f)
;        (try-catch
;            (begin
;              (define fileformat (invoke-static <javax.sound.sampled.AudioSystem> 'get-audio-file-format audio-file))
;              (display "file format ")(display fileformat)(newline)
;              
;              
;              
;              
;              (set! ais (invoke-static <javax.sound.sampled.AudioSystem> 'get-audio-input-stream audio-file))
;              (define myclip (invoke-static <javax.sound.sampled.AudioSystem> 'get-clip))
;              
;              
;              
;                                     
;                                     ;isLineSupported(Line.Info info)
;              
;              
;              (define audio-format (invoke ais 'get-format))
;              (display audio-format)(newline)
;              
;              ;; check for encoding and file type support
;              (define dataline-info (<javax.sound.sampled.DataLine$Info> 
;                                                   <javax.sound.sampled.SourceDataLine> audio-format))  
;              
;              (define supported? (invoke-static <javax.sound.sampled.AudioSystem> 
;                                                'is-line-supported dataline-info))
;              (display "supoprted? ")(display supported?)(newline)
;              
;              (invoke myclip 'open ais)
;              (invoke myclip 'start)
;             
;              )
;          (ex <java.lang.Throwable>
;              (begin
;                (display (*:toString ex))(newline)
;                )))

;        
;        
;        (set! once #f)
;        )
;      )