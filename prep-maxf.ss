(require "./commons.ss")

(import parameter)
(import common)

(define (docmaxfreq-create)
  (define docmaxfreq
    (make-vector *file-count* 0))
  (define invidx
    (call-with-input-file *invidx-ss-file* read))
  (vector-for-each-with-index
   (lambda (vocab1 w1-invidx*)
     (vector-for-each-with-index
      (lambda (w2-fileid*)
        (define fileid*
          (if (vector? w2-fileid*)
              w2-fileid*
              (cdr w2-fileid*)))
        (vector-for-each
         (lambda (fileid-cnt)
           (define fileid
             (if (pair? fileid-cnt) (car fileid-cnt) fileid-cnt))
           (define cnt
             (if (pair? fileid-cnt) (cdr fileid-cnt) 1))
           (vector-set! docmaxfreq fileid
                        (max cnt (vector-ref docmaxfreq fileid))))
         fileid*))
      w1-invidx*))
   invidx))

(define (main args)
  (call-with-output-file *docmaxfreq-file*
    (lambda (port)
      (write (docmaxfreq-create) port))))
