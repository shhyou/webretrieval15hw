; Some random notes:
;   - documents
;       * list of files
;       * vocabulary list
;   - inverted index
;   - queries

(use util.match  :only (match-let))
(use sxml.ssax   :only (ssax:xml->sxml))
(use sxml.sxpath :only (sxpath car-sxpath node-pos sxml:string-value))

(use scheme.time)
(use srfi-1      :only (take-while))
(use srfi-13) ; string library
(use srfi-43) ; vector library

(require "./commons.ss")

(import parameter)
(import (common :only (clock)))

(define *read-inv-idx* #t)
(define *read-vocab* #t)
(define *read-doclist* #t)
(define *read-docmaxfreq* #t)

(define *invidx* #f)
(define *vocab-all* #f)      ; vector of string
(define *vocab-all-inv* #f)  ; hash table from string to int
(define *doclist* #f)
(define *docmaxfreq* #f)

(define (read-xml file-name xml-path)
  (call-with-input-file file-name
    (lambda (port)
      ((sxpath xml-path) (ssax:xml->sxml port '())))))

(define *query-xml-path* '(xml topic))
(define *doc-xml-path* '(// doc))

; for debug
(define (vocab-list->string vocab*)
  ($ apply string-append $ map
     (lambda (w1-w2*)
       ($ apply string-append $ map
          (lambda (w2)
            (format "~a ~a\n"
                    (vector-ref *vocab-all* (car w1-w2*))
                    (if (= -1 w2) -1 (vector-ref *vocab-all* w2))))
          (cdr w1-w2*)))
     vocab*))

; current only use title
(define (query->vocab-list query)
  (define (string->vocab-list str)
    (define avail (make-vector (+ 1 (string-length str)) #f))
    (vector-set! avail 0 #t)
    (let loop ([pos 0] [lst '()])
      (cond
       [(= pos (string-length str)) (reverse lst)]
       [(vector-ref avail pos)
        (loop (+ pos 1)
              (vector-fold-right
               (lambda (vocab1 lst^ w1)
                 (define w1-len (string-length w1))
                 (define (check-w2*)
                   (vector-fold
                    (lambda (_ lst^^ vocab2-with-invidx)
                      (if (pair? vocab2-with-invidx)
                          (let* ([vocab2 (car vocab2-with-invidx)]
                                 [w2 (vector-ref *vocab-all* vocab2)]
                                 [w2-len (string-length w2)])
                            (cond [(string-prefix? w2 str 0 w2-len (+ pos w1-len))
                                   (vector-set! avail (+ pos w1-len w2-len) #t)
                                   (cons `(,vocab1 . ,vocab2) lst^^)]
                                  [else lst^^]))
                          (begin
                            (vector-set! avail (+ pos w1-len) #t)
                            (let* ([ch (string-ref (vector-ref *vocab-all* vocab1) 0)]
                                   [category (char-general-category ch)])
                              (if (memq category '(Ll Lu Nd Lt Nl No))
                                  (cons `(,vocab1 . -1) lst^^)
                                  lst^^)))))
                    lst^
                    (vector-ref *invidx* vocab1)))
                 (cond [(string-prefix? w1 str 0 w1-len pos) (check-w2*)]
                       [else lst^]))
               lst *vocab-all*))]
       [else (loop (+ pos 1) lst)])))
  (string->vocab-list (sxml:string-value ((car-sxpath '(title)) query))))

(define (inverted-index-ref vocab)
  (define (match-vocab2 w1-invidx*)
    (and (pair? w1-invidx*) (= (cdr vocab) (car w1-invidx*))))
  (let ([w1-invidx* (vector-ref *invidx* (car vocab))])
    (cond
     [(vector-empty? w1-invidx*) #f]
     [(= -1 (cdr vocab))
      (if (vector? (vector-ref w1-invidx* 0))
          (vector-ref w1-invidx* 0)
          #f)]
     [else
      (let* ([cmp (^[a b] (cond [(< (car a) (car b)) -1]
                                [(> (car a) (car b)) 1]
                                [else 0]))]
             [start-idx (if (vector? (vector-ref w1-invidx* 0)) 1 0)]
             [idx (vector-binary-search w1-invidx* `(,(cdr vocab) . ()) cmp start-idx)]
             [w2-fileid* (vector-ref w1-invidx* idx)])
        (cdr w2-fileid*))])))

(define (tf-idf docid vocab)
  (define fileid* (inverted-index-ref vocab))
  (define (tf)
    (let
        ([term-occurence
          (vector-any
           (lambda (fileid)
             (cond [(and (pair? fileid) (= docid (car fileid))) (cdr fileid)]
                   [(and (number? fileid) (= docid fileid)) 1]
                   [else #f]))
           fileid*)])
      ; try log normalization for now
      (if term-occurence (+ 1 (log term-occurence)) 0)))
  (define (idf)
    (define doc-occurence
      (vector-length fileid*))
    (log (/ *file-count* doc-occurence)))
  (* (tf) (idf)))

(define (merge-documents vocab*)
  (define (nub prev xs)
    (cond [(null? xs) '()]
          [(eq? prev (car xs)) (nub prev (cdr xs))]
          [else (cons (car xs) (nub (car xs) (cdr xs)))]))
  ($ nub #f $ vector->list $ sort $ apply vector-append ; temporary hack
     (map
      (lambda (vocab)
        (vector-map
         (lambda (_ fileid)
           (if (pair? fileid) (car fileid) fileid))
         (inverted-index-ref vocab)))
      vocab*)))

(define (cos-dist xs ys)
  (let loop ([xs xs] [ys ys] [dot 0.0] [sumx2 0.0] [sumy2 0.0])
    (if (null? xs)
        (/ dot (* (sqrt sumx2) (sqrt sumy2)))
        (let ([x (car xs)] [y (car ys)])
          (loop (cdr xs) (cdr ys)
                (+ dot (* x y))
                (+ sumx2 (* x x))
                (+ sumy2 (* y y)))))))

(define *sample-vocab*
  '((11602 . 7709) (7709 . 10635) (10635 . 10588) (10588 . 8640) (8640 . 9632) (9632 . 10877) (10877 . 11043) (11043 . 9634) (9634 . 8780)))

(define (retrieve query)
  (define (get-vocab-idf vocab)
    (log (/ *file-count* (vector-length (inverted-index-ref vocab)))))
  (define vocab* (query->vocab-list query))
  (format #t "Retrieving ~a [~a]\n"
          vocab*
          (map (lambda (vocab)
                 `(,(vector-ref *vocab-all* (car vocab))
                   . ,(if (= -1 (cdr vocab))
                          -1
                          (vector-ref *vocab-all* (cdr vocab))))) vocab*))
  (let*
      ([docs (merge-documents vocab*)]
       [vocab-idf (map get-vocab-idf vocab*)]
       [docs-tfidf (map (lambda (d)
                          (when (= (mod d 200) 0) (format #t "~a\n" d))
                          (map (lambda (vocab) (tf-idf d vocab)) vocab*))
                        docs)]
       [docs-dist (map cons docs
                       (map (lambda (d) (cos-dist d vocab-idf))
                            docs-tfidf))])
    (format #t "Total ~a document(s).\n" (length docs))
    (set! docs-dist (sort! docs-dist (^[d1 d2] (> (cdr d1) (cdr d2)))))
    docs-dist))

(define (test)
 (call-with-output-file "ans-test"
  (lambda (port)
    (do ([queries (read-xml *query-file* *query-xml-path*) (cdr queries)])
        ([null? queries] 0)
      (let* ([query (car queries)]
             [query-num (sxml:string-value ((car-sxpath '(number)) (car queries)))]
             [query-num-len (string-length query-num)]
             [query-num-digit (substring query-num (- query-num-len 3) query-num-len)]
             [doc-dist (retrieve query)])
        (for-each
         (lambda (d)
           (let* ([docfile (vector-ref *doclist* (car d))]
                  [docfile-real (substring docfile 7 (string-length docfile))]
                  [docfile-id
                   ($ sxml:string-value $ (car-sxpath '(id))
                    (read-xml (string-append *NTCIR-prefix* docfile-real)
                              *doc-xml-path*))])
             (format port "~a ~a\n" query-num-digit docfile-id)))
         (take*
          (take-while (^d (>= (cdr d) 0.6))
                      doc-dist)
          100)))))))

(define (inverted-index-read)
  (call-with-input-file *invidx-ss-file* read))

(define (vocab-read)
  (define vocab-hash
    (make-hash-table 'string=?))
  (let ([vocab-all (call-with-input-file *vocab-file*
                     (lambda (port)
                       (list->vector
                        (port->string-list port))))])
    (vector-for-each-with-index
     (lambda (idx vocab)
       (hash-table-put! vocab-hash vocab idx))
     vocab-all)
    `(,vocab-all . ,vocab-hash)))

(define (doclist-read)
  (call-with-input-file *doclist-file*
    (lambda (port)
      (list->vector (port->string-list port)))))

(define (docmaxfreq-read)
  (call-with-input-file *docmaxfreq-file* read))

(define init-values
  (lambda ()
    (when *read-inv-idx*
      (format #t "~a (inverted-index-read)\n" (clock))
      (set! *invidx* (inverted-index-read)))
    (when *read-vocab*
      (format #t "~a (vocab-read)\n" (clock))
      (match-let ([(vocab-all . vocab-hash) (vocab-read)])
        (set! *vocab-all* vocab-all)
        (set! *vocab-all-inv* vocab-hash)))
    (when *read-doclist*
      (format #t "~a (doclist-read)\n" (clock))
      (set! *doclist* (doclist-read)))
    (when *read-docmaxfreq*
      (format #t "~a (docmaxfreq-read)\n" (clock))
      (set! *docmaxfreq* (docmaxfreq-read)))
    (format #t "~a init-values done\n" (clock))))

(define main
  (lambda (args)
    (init-values)
    (test)))
