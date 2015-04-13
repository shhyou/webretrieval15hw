(use gauche.parseopt)

(require "./commons.ss")

(import parameter)
(import common)

(define (inverted-index-create)
  (define invidx
    (make-vector *vocab-count* '()))
  (call-with-input-file *invidx-file*
    (lambda (port)
      (let loop ([w1 (read-int port)])
        (if (eof-object? w1)
            (begin
              (vector-map!
               (lambda (vs)
                 (sort! (list->vector vs)
                        (^[a b]
                          (cond [(and (pair? a) (pair? b))
                                 (< (car a) (car b))]
                                [else (vector? a)]))))
               invidx)
              invidx)
            (let* ([w2 (read-int port)]
                   [file-cnt (read-int port)]
                   [inv-w2 (vector-tabulate
                            file-cnt
                            (lambda (idx)
                              (let* ([fileidx (read-int port)]
                                     [cnt (read-int port)])
                                (if (> cnt 1)
                                    `(,fileidx . ,cnt)
                                    fileidx))))])
              (vector-set!
               invidx w1
               (cons (if (= -1 w2) inv-w2 `(,w2 . ,inv-w2))
                     (vector-ref invidx w1)))
              (loop (read-int port))))))))

(define (main1 args)
  (call-with-output-file *invidx-ss-file*
    (lambda (port)
      (write (inverted-index-create) port)))
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
      (format #t "Preprocessing inverted index...\n")
      (main1 args))))
