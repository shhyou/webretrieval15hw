(use gauche.parseopt)

(use util.match :only (match-let))

(require "./commons.ss")

(import parameter)
(import common)

(define (veclen-create)
  (define veclen
    (make-vector *file-count* 0))
  (define querylen 0)
  (define invidx
    (call-with-input-file *invidx-ss-file* read))
  (vector-for-each-with-index
   (lambda (vocab1 w1-invidx*)
     (vector-for-each
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
           ;(when (> cnt 1) (format #t "file ~a cnt ~a\n" fileid cnt))
           (let* ([tf cnt]
                  [idf (log (/ *file-count* (vector-length fileid*)))]
                  [tf-idf (* tf idf)])
             (vector-set! veclen fileid
                          (+ (* tf-idf tf-idf) (vector-ref veclen fileid)))
             (set! querylen (+ (* 0.5 0.5 idf idf) querylen))))
         fileid*))
      w1-invidx*))
   invidx)
  (vector-map! sqrt veclen)
  `(,veclen ,querylen))

(define (main1 args)
  (call-with-output-file *veclen-file*
    (lambda (port)
      (match-let ([(veclen querylen) (veclen-create)])
        (write veclen port)
        (write querylen port))))
  0)

(define main
  (lambda (args)
    (let-args (cdr args)
              ([rocchio  "r|rocchio"]
               [infile   "i|infile=s"]
               [outfile  "o|outfile=s"]
               [modeldir "m|modeldir=s"]
               [NTCIRdir "d|NTCIRdir=s"])
      (set! *query-file* infile)
      (set! *output-file* outfile)
      (set! *doclist-file* NTCIRdir)
      (init-model modeldir)
      (format #t "Preprocessing document vector...\n")
      (main1 args))))
