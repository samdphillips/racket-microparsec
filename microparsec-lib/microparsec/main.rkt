#lang racket/base

(provide (all-defined-out))

(require (for-syntax racket/base)
         racket/format
         racket/generic
         racket/match
         racket/stream
         racket/string
         (rename-in racket/stream
                    [stream-cons -stream-cons]
                    [empty-stream -empty-stream])
         syntax/parse/define
         syntax/readerr
         syntax/srcloc)

#|
Parser tok res = Stream tok -> Consumed res
Consumed res = Consumed (Reply res) | Empty (Reply res)
Reply res = Ok res (Stream tok) Message | Error Message
Message tok = Message Pos tok (List String)
|#

(struct consumed (reply) #:transparent)
(struct empty (reply) #:transparent)

(struct ok (result stream message) #:transparent)
(struct err (message) #:transparent)

(struct message (srcloc token expected) #:transparent)

(define-match-expander stream-cons
  (syntax-rules ()
   [(_ apat bpat)
    (? stream?
       (and (not (empty-stream))
            (app stream-first apat)
            (app stream-rest  bpat)))])
  (make-rename-transformer #'-stream-cons))

(define-match-expander empty-stream
  (syntax-rules ()
   [(_) (? stream-empty?)])
  (make-rename-transformer #'-empty-stream))

(define-generics to-srcloc
  [token-srcloc to-srcloc]
  #:defaults
  ([(lambda (x) #t)
    (define (token-srcloc to-srcloc) #f)]))

(define (merge-message amsg bmsg)
  (match* (amsg bmsg)
   [((message loc tok alabels) (message _ _ blabels))
    (message loc tok (append alabels blabels))]))

(define (expect msg label)
  (match msg
   [(message pos tok _) (message pos tok (list label))]))

(define (merge-ok v a-stream amsg bmsg)
  (empty (ok v a-stream (merge-message amsg bmsg))))

(define (merge-error amsg bmsg)
  (empty (err (merge-message amsg bmsg))))

(define (return/p v)
  (lambda (a-stream)
    (empty (ok v a-stream (message #f "" null)))))

(define (seq/p p f)
  (lambda (a-stream)
    (match (p a-stream)
     [(empty (ok v inp _)) ((f v) inp)]
     [(empty err) (empty err)]
     [(consumed (ok v inp _))
      (match ((f v) inp)
       [(empty rpy) (consumed rpy)]
       [v v])]
     [v v])))

(define-syntax-parser do/p
  #:datum-literals (<-)
  [(_ [x:id <- expr] rest ...+)
   #'(seq/p expr (lambda (x) (do/p rest ...)))]
  [(_ expr rest ...+)
   #'(seq/p expr (lambda (ignore) (do/p rest ...)))]
  [(_ expr) #'expr])

(define (satisfy/p pred?)
  (lambda (a-stream)
    (match a-stream
     [(stream-cons (? pred? tok) rest)
      (consumed
        (ok tok rest (message (token-srcloc tok) "" null)))]
     [(stream-cons tok _)
      (empty
        (err (message (token-srcloc tok) tok null)))]
     [(empty-stream)
      (empty
        (err (message #f empty-stream null)))])))

(define (or2/p a/p b/p)
  (lambda (a-stream)
    (match (a/p a-stream)
     [(empty (err amsg))
      (match (b/p a-stream)
       [(empty (err bmsg)) (merge-error amsg bmsg)]
       [(empty (ok v inp bmsg)) (merge-ok v inp amsg bmsg)]
       [v v])]
     [(empty (ok v inp amsg))
      (match (b/p a-stream)
       [(empty (err bmsg))    (merge-ok v inp amsg bmsg)]
       [(empty (ok _ _ bmsg)) (merge-ok v inp amsg bmsg)]
       [v v])]
     [v v])))

(define or/p
  (case-lambda
   [(a/p) a/p]
   [(a/p b/p . rest) (apply or/p (or2/p a/p b/p) rest)]))

(define (label/p label p)
  (lambda (a-stream)
    (match (p a-stream)
     [(empty (err msg))      (empty (err (expect msg label)))]
     [(empty (ok v inp msg)) (empty (ok v inp (expect msg label)))]
     [v v])))

(define (try/p p)
  (lambda (a-stream)
    (match (p a-stream)
     [(consumed (err msg)) (empty (err msg))]
     [v v])))

(define (many1/p p)
  (do/p [v <- p]
        [vs <- (or/p (many1/p p) (return/p null))]
        (return/p (cons v vs))))

(define (format-expected expected-list)
  (match expected-list
   [(list a) (~a a)]
   [(list a b) (~a a " or " b)]
   [(list a ... b) (~a (string-join (map ~a a) ", ") " or " b)]))

(define (default-err-handler e)
  (match-define (err (message srcloc tok expected)) e)
  (raise-read-error
    (~a "got " tok "expected "
        (format-expected expected))
    (source-location-source srcloc)
    (source-location-line srcloc)
    (source-location-column srcloc)
    (source-location-position srcloc)
    (source-location-span srcloc)))

(define (unwrap v)
  (match v
   [(consumed rpy) rpy]
   [(empty rpy)    rpy]))

(define (parse p s #:error [err-handler default-err-handler])
  (match (unwrap (p s))
   [(ok v s _)      (values v s)]
   [(and e (err _)) (err-handler e)]))
