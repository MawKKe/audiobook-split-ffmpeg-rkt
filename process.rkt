#lang racket

(require json)
(require racket/path)

(define test-file (build-path (current-directory) "testdata" "beep.m4a"))

(define (get-extension infile)
  (path-get-extension infile))

(struct chapter (id title start end))

(define (make-ffprobe-args infile)
  (list "-i" infile "-v" "error" "-print_format" "json" "-show_chapters"))

(define (call-ffprobe infile)
      (apply subprocess
             #f #f #f
             (find-executable-path "ffprobe")
             (make-ffprobe-args infile)))

(define (parse-chapter ht)
  (chapter
    (hash-ref ht 'id)
    (hash-ref (hash-ref ht 'tags) 'title #f)
    (hash-ref ht 'start_time)
    (hash-ref ht 'end_time)))

(define (ffprobe infile)
    (let-values ([(proc stdout stdin stderr) (call-ffprobe infile)])
      (map parse-chapter (hash-ref (read-json stdout) 'chapters))))

(define (show-chapter ch)
    (printf "~a - ~a (~a ... ~a)~%"
            (chapter-id ch)
            (chapter-title ch)
            (chapter-start ch)
            (chapter-end ch)))

(define (make-chapter-num-formatter num-total-chapters [offset 0])
  (let ([width (inexact->exact (ceiling (log num-total-chapters 10)))])
    (lambda (ch)
      (~r
        (+ (chapter-id ch) (max offset 0))
        #:min-width (max width 2)
        #:pad-string "0"))))

(let* ([chapters (ffprobe test-file)]
       [n (length chapters)]
      [ext (get-extension test-file)])
    (void (map show-chapter chapters))
    (printf "extension: ~a~%" ext)
    (printf "total: ~a chapters~%" n))

(let ([cfmt (make-chapter-num-formatter 3)])
  (displayln (cfmt (chapter 1 #f #f #f)))
  (displayln (cfmt (chapter 11 #f #f #f))))
