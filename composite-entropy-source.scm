;;;; composite-entropy-source.scm
;;;; Kon Lovett, Apr, '17

#|
=== Composite Entropy Source

==== Usage

<enscript language=scheme>
(import composite-entropy-source)
</enscript>

==== composite-entropy-source

<procedure>(composite-entropy-source-constructor [ENTROPY-SOURCE ...] [#:comb-int (COMB-INT INTEGER-COMBINE)] [#:comb-real (COMB-REAL REAL-COMBINE)]) => entropy-source</procedure>

Returns a new {{entropy-source}} that combines the behaviors of the supplied
{{ENTROPY-SOURCE ...}}.

{{INTEGER-COMBINE}} default is {{(lambda (ints bnd) (modulo (apply + ints) bnd))}}.

{{REAL-COMBINE}} default is {{(lambda (reals prec) (apply * reals))}}.

Does not register the constructed {{entropy-source}}.

Experimental at best.
|#

(module composite-entropy-source

(;export
  *composite-entropy-source-constructor
  composite-entropy-source-constructor)

(import scheme)
(import (chicken base))
(import (chicken type))
(import (chicken keyword))

(import
  (only (chicken string) conc reverse-string-append ->string)
  (only type-errors error-argument-type)
  srfi-1
  srfi-4
  entropy-source
  srfi-27-vector-support
  srfi-27)

;;;

(include "srfi-27-common-types")

;;

(define (pull-rest-argument rest0)
  (let loop ((irest rest0) (orest '()))
    (cond
      ((null? irest)
        (reverse! orest) )
      ((keyword? (car irest))
        (loop (cddr irest) orest) )
      (else
        (loop (cdr irest) (cons (car irest) orest)) ) ) ) )

;; composite-entropy-source-constructor
;;
;; returns the composite name & constructor

;FIXME - the combinators are suspect

(: composite-entropy-source-constructor (#!rest --> procedure))
;
(define (composite-entropy-source-constructor
          #!rest rest0
          #!key
          (comb-int (lambda (ints n) (modulo (reduce fx+ 0 ints) n)))
          (comb-real (lambda (reals unit) (reduce fp* 1.0 reals))))
  ;scrub keyword arguments
  (let* (
    (rest (pull-rest-argument rest0) )
    (srcs0
      (if (null? rest)
        (error 'composite-entropy-source-constructor "no entropy-sources to combine")
        (map (cut check-entropy-source 'composite-entropy-source-constructor <>) rest) ) ) )
    ;collect features
    (let loop ((srcs srcs0) (names '()) (docus '()))
      (if (null? srcs)
        ;then make composed entropy-source
        (*composite-entropy-source-constructor
          comb-int comb-real
          (string->symbol (reverse-string-append (intersperse names "+")))
          (reverse-string-append (intersperse docus " & "))
          srcs0)
        ;else collect info
        (let ((es (car srcs)))
          (loop
            (cdr srcs)
            (cons (->string (*entropy-source-name es)) names)
            (cons (*entropy-source-documentation es) docus)) ) ) ) ) )

;; *composite-entropy-source-constructor
;;
;; returns the composite constructor

(: *composite-entropy-source-constructor (procedure procedure entropy-source-name string (list-of entropy-source) --> procedure))
;
(define (*composite-entropy-source-constructor comb-int comb-real def-name def-docu srcs)
  (let (
    (u8proc
      (lambda ()
        (comb-int
          (map
            (lambda (es)
              ((@entropy-source-u8 es)) )
            srcs)
          255) ) )
    (f64proc
      (lambda ()
        (comb-real
          (map
            (lambda (es)
              ((@entropy-source-f64 es)) )
            srcs)
          #f) ) ) )
    (letrec (
        (ctor
          (lambda (#!optional (name def-name) (docu def-docu))
            (*make-entropy-source
              ;
              ctor
              ;
              name
              ;
              docu
              ;
              u8proc
              ;
              f64proc
              ;
              (lambda (u8cnt u8vec)
                (let ((u8vec (or u8vec (make-u8vector u8cnt))))
                  (u8vector-filled! u8vec u8proc 0 u8cnt)
                  u8vec ) )
              ;
              (lambda (f64cnt f64vec)
                (let ((f64vec (or f64vec (make-f64vector f64cnt))))
                  (f64vector-filled! f64vec f64proc 0 f64cnt)
                  f64vec ) ) ) ) ) )
      ctor ) ) )

) ;module composite-entropy-source
