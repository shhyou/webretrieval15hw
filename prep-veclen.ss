(require "./commons.ss")

(import parameter)
(import common)

(define (veclen-create)
  (define veclen
    (make-vector *file-count* 0))
  (define invidx
    (call-with-input-file *invidx-ss-file* read))
  (define docmaxfreq
    (call-with-input-file *docmaxfreq-file* read))
  (vector-for-each-with-index
   (lambda (vocab1 w1-invidx*)
     (vector-for-each
      (lambda (w2-fileid*)
        (define fileid*
          (if (vector? w2-fileid*)
              w2-fileid*
              (cdr w2-fileid*)))
        (define vocab2
           (if (vector? w2-fileid*)
               -1
               (car w2-fileid*)))
        (vector-for-each
         (lambda (fileid-cnt)
           (define fileid
             (if (pair? fileid-cnt) (car fileid-cnt) fileid-cnt))
           (define cnt
             (if (pair? fileid-cnt) (cdr fileid-cnt) 1))
           ;(when (> cnt 1) (format #t "file ~a cnt ~a\n" fileid cnt))
           (let* ([tf (+ 0.5 (* 0.5 (/ cnt (vector-ref docmaxfreq fileid))))]
                  [idf (log (/ *file-count* (vector-length fileid*)))]
                  [tf-idf (* tf idf)])
             (vector-set! veclen fileid
                          (+ (* tf-idf tf-idf) (vector-ref veclen fileid)))))
         fileid*))
      w1-invidx*))
   invidx)
  veclen)

(define (main args)
  (call-with-output-file *veclen-file*
    (lambda (port)
      (write (veclen-create) port))))
