; Some random notes:
;   - documents
;       * list of files
;       * vocabulary list
;   - inverted index
;   - queries

(use sxml.ssax   :only (ssax:xml->sxml))
(use sxml.sxpath :only (sxpath car-sxpath node-pos sxml:string-value))

(use scheme.time)
(use srfi-13) ; string library
(use srfi-43) ; vector library

(require "./commons.ss")

(import parameter)

(define *read-inv-idx* #f)

(define *invidx* #f)

(define (read-xml file-name xml-path)
  (call-with-input-file file-name
    (lambda (port)
      ((sxpath xml-path) (ssax:xml->sxml port '())))))

(define *query-xml-path* '(xml topic))
(define *doc-xml-path* '(// doc))

(define (inverted-index-read)
  (call-with-input-file *invidx-ss-file* read))

(define init-values
  (lambda ()
    (when *read-inv-idx*
      (display (format "~a (inverted-index-read)\n" (current-jiffy)))
      (set! *invidx* (inverted-index-read)))
    #t))

(define main
  (lambda (args)
    (init-values)
    #t))
