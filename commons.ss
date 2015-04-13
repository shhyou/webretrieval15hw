(define-module parameter
  (export
   *vocab-count*
   *file-count*
   *output-file*
   *NTCIR-prefix*
   *query-file*
   *invidx-file*
   *invidx-ss-file*
   *vocab-file*
   *doclist-file*
   *docmaxfreq-file*
   *veclen-file*
   *posidx-file*)
  (define *vocab-count* 29908)
  (define *file-count* 46972)
  (define *output-file* "./ans-train-my")
  (define *model-prefix* "./model")
  (define *NTCIR-prefix* "./model/CIRB010")
  (define *query-file* (string-append *model-prefix* "/query/query-train.xml"))
  (define *invidx-file* (string-append *model-prefix* "/inverted-file"))
  (define *invidx-ss-file* (string-append *model-prefix* "/invidx.ss"))
  (define *vocab-file* (string-append *model-prefix* "/vocab.all"))
  (define *doclist-file* (string-append *model-prefix* "/file-list"))
  (define *docmaxfreq-file* (string-append *model-prefix* "/docmaxfreq.ss"))
  (define *veclen-file* (string-append *model-prefix* "/veclen.ss"))
  (define *posidx-file* (string-append *model-prefix* "/posidx.ss")))

(define-module common
  (export
   clock
   read-int)
  (define (clock)
    (use scheme.time :only (current-jiffy jiffies-per-second))
    (exact->inexact (/ (current-jiffy) (jiffies-per-second))))
  (define read-int
    (lambda (port)
      (define ch (let skip ([ch (read-char port)])
                   (cond
                    [(eof-object? ch) (eof-object)]
                    [(or (char-numeric? ch) (eq? ch #\-)) ch]
                    [(skip (read-char port))])))
      (if (eof-object? ch)
          (eof-object)
          (let loop ([i 0] [ch ch] [negate 1])
            (cond
             [(eq? ch #\-) (loop i (read-char port) -1)]
             [(or (eof-object? ch) (not (char-numeric? ch)))
              (* negate i)]
             [else
              (loop (+ (* i 10) (- (char->integer ch) #x30)) (read-char port) negate)]))))))
