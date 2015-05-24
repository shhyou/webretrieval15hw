(use gauche.parseopt)

(use util.match :only (match-let))

(require "./commons.ss")

(import parameter)
(import common)

; TODO: simplify rep'n to '(,w1 . ,w2-list)
(define (positive-index-create)
  (define posidx
    (make-vector *file-count* '()))
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
        (define vocab
          (if (vector? w2-fileid*)
              `(,vocab1 . -1)
              `(,vocab1 . ,(car w2-fileid*))))
        (vector-for-each
         (lambda (fileid-cnt)
           (define fileid
             (if (pair? fileid-cnt) (car fileid-cnt) fileid-cnt))
           (vector-set! posidx fileid
                        (cons vocab (vector-ref posidx fileid))))
         fileid*))
      w1-invidx*))
   invidx)
  (vector-map! list->vector posidx)
  posidx)

(define (main1 args)
  (call-with-output-file *posidx-file*
    (lambda (port)
      (write (positive-index-create) port)))
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
      (format #t "Preprocessing document index...\n")
      (main1 args))))