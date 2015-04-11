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
(use srfi-13) ; string library
(use srfi-43) ; vector library

(require "./commons.ss")

(import parameter)
(import (common :only (clock)))

(define *read-inv-idx* #f)
(define *read-vocab* #t)

(define *invidx* #f)
(define *vocab-all* #f)      ; vector of string
(define *vocab-all-inv* #f)  ; hash table from string to int

(define (read-xml file-name xml-path)
  (call-with-input-file file-name
    (lambda (port)
      ((sxpath xml-path) (ssax:xml->sxml port '())))))

(define *query-xml-path* '(xml topic))
(define *doc-xml-path* '(// doc))

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
                   (vector-fold-right
                    (lambda (_ lst^^ vocab2-with-invidx)
                      (if (pair? vocab2-with-invidx)
                          (let* ([vocab2 (car vocab2-with-invidx)]
                                 [w2 (vector-ref *vocab-all* vocab2)]
                                 [w2-len (string-length w2)])
                            (cond [(string-prefix? w2 str 0 w2-len (+ pos w1-len))
                                   (vector-set! avail (+ pos w1-len w2-len) #t)
                                   (cons vocab2 lst^^)]
                                  [else lst^^]))
                          (begin
                            (vector-set! avail (+ pos w1-len) #t)
                            (cons -1 lst^^))))
                    '()
                    (vector-ref *invidx* vocab1)))
                 (cond [(string-prefix? w1 str 0 w1-len pos)
                        (cons `(,vocab1 . ,(check-w2*)) lst^)]
                       [else lst^]))
               lst *vocab-all*))]
       [else (loop (+ pos 1) lst)])))
  (string->vocab-list (sxml:string-value ((car-sxpath '(title)) query))))

(define (test)
  (define queries
    (read-xml *query-file* *query-xml-path*))
  (format #t "~a\n" (query->vocab-list ((node-pos 1) queries)))
  #f)

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
    (format #t "~a init-values done\n" (clock))))

(define main
  (lambda (args)
    (init-values)
    (test)))
