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

(define *read-inv-idx* #f)
(define *read-vocab* #t)

(define *invidx* #f)
(define vocab->string #f)
(define string->vocab #f)

(define (read-xml file-name xml-path)
  (call-with-input-file file-name
    (lambda (port)
      ((sxpath xml-path) (ssax:xml->sxml port '())))))

(define *query-xml-path* '(xml topic))
(define *doc-xml-path* '(// doc))

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
      (display (format "~a (inverted-index-read)\n" (current-jiffy)))
      (set! *invidx* (inverted-index-read)))
    (when *read-vocab*
      (display (format "~a (vocab-read)\n" (current-jiffy)))
      (match-let ([(vocab-all . vocab-hash) (vocab-read)])
        (set! vocab->string
          (lambda (vocab)
            (vector-ref vocab-all vocab)))
        (set! string->vocab
          (lambda (str)
            (hash-table-get vocab-hash str)))))
    #t))

(define main
  (lambda (args)
    (init-values)
    #t))
