#lang racket

(require json)
(require racket/path)

(define-struct chapter (id title start end))

(define-struct infile-meta (infile chapters))

(define-struct outfile-opts (directory enum-offset))

(define (get-filename-extension infile)
  (path-get-extension infile))

;; /path/to/file.xyz -> "file"
(define (get-filename-stem infile)
  (file-name-from-path (path-replace-extension infile #"")))

(define (make-ffprobe-args infile)
  (list "-i" infile "-v" "error" "-print_format" "json" "-show_chapters"))

(define (make-ffmpeg-split-args meta ch outfile)
  (flatten
    (list
      "-nostdin"
      "-i" (infile-meta-infile meta)
      "-v" "error"
      "-map_chapters" "-1"
      "-vn"
      "-c" "copy"
      "-ss" (chapter-start ch)
      "-to" (chapter-end ch)
      (track-num-meta (chapter-id ch) (length (infile-meta-chapters meta)))
      (track-title-meta (chapter-title ch))
      "-n"
      outfile)))

(define (track-num-meta i total)
  (list "-metadata" (format "track=~a/~a" i total)))


(define (track-title-meta title)
  (if title
    (list "-metadata" (format "title=~a" title))
    (list)))

;(displayln (track-num-meta 2 9))
;(displayln (track-title-meta "Fooo"))

(define (call-ffprobe infile)
  (apply subprocess
         #f #f #f
         (find-executable-path "ffprobe")
         (make-ffprobe-args infile)))

(define (sanitize-title title-maybe)
  (if (not title-maybe)
    #f
    (let ([t (string-trim title-maybe)])
      (if (eq? "" t) #f t))))

(define (parse-chapter ht)
  (chapter
    (hash-ref ht 'id)
    (sanitize-title (hash-ref (hash-ref ht 'tags #hash()) 'title #f))
    (hash-ref ht 'start_time)
    (hash-ref ht 'end_time)))


(define (ffprobe infile)
  (let-values ([(proc stdout stdin stderr) (call-ffprobe infile)])
    (map parse-chapter (hash-ref (read-json stdout) 'chapters))))

(define (read-infile infile)
  (let* ([chapters (ffprobe infile)])
         (infile-meta infile chapters)))

(define (format-chapter-num chnum num-total-chapters)
  (let ([width (inexact->exact (ceiling (log num-total-chapters 10)))])
    (~r
      chnum
      #:min-width (max width 2)
      #:pad-string "0")))

(define (make-outfile-stem-formatter default-stem #:force-override [override #f])
  (lambda (ch)
    (let ([title (chapter-title ch)])
      (if (and title (not override))
        title
        default-stem))))

(define use-title-as-stem #t)

; we need
; - number (per chapter)
; - stem (per chapter)
; - outdir (common)
; - ext (common)
(define (chapter-outfile out-dir stem ext)
  (path-replace-extension (build-path out-dir stem) ext))

;(define (make-command ch out))

(define (make-outfile-name-formatter meta out-dir)
  (let* ([num-chapters (length (infile-meta-chapters meta))]
         [in-stem (get-filename-stem (infile-meta-infile meta))]
         [in-ext  (get-filename-extension (infile-meta-infile meta))]
         [choose-outfile-stem (make-outfile-stem-formatter in-stem #:force-override (not use-title-as-stem))])
    (lambda (ch)
      (path-replace-extension
        (build-path
            out-dir
            (format "~a - ~a"
                    (format-chapter-num (chapter-id ch) num-chapters)
                    (choose-outfile-stem ch)))
        in-ext))))

(define test-file (build-path (current-directory) "testdata" "beep.m4a"))
(define test-out-dir (build-path "_out"))

(define (main in-file out-dir)
    (let*
      ([meta (read-infile in-file)]
       [get-outfile-name (make-outfile-name-formatter meta out-dir)]
       [build (lambda (ch) (make-ffmpeg-split-args
                             meta
                             ch
                             (get-outfile-name ch)))])
      (void (map (lambda (ch) (displayln (build ch))) (infile-meta-chapters meta)))))
      ;(void (map (lambda (ch) (displayln (get-outfile-name ch))) (infile-meta-chapters meta)))))

(main test-file test-out-dir)
