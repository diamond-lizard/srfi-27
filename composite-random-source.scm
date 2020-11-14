;;;; composite-random-source.scm
;;;; Kon Lovett, Oct '09

#|
=== Composite Random Source

==== Usage

<enscript language=scheme>
(use composite-random-source)
</enscript>

==== composite-random-source-constructor

<procedure>(composite-random-source-constructor [RANDOM-SOURCE ...] [#:comb-int (COMB-INT INTEGER-COMBINE)] [#:comb-real (COMB-REAL REAL-COMBINE)]) => random-source</procedure>

Returns a new {{random-source}} that combines the behaviors of the supplied
{{RANDOM-SOURCE ...}}.

{{INTEGER-COMBINE}} default is {{(lambda (ints bnd) (modulo (apply + ints) bnd))}}.

{{REAL-COMBINE}} default is {{(lambda (reals prec) (apply * reals))}}.

Does not register the constructed {{random-source}}.

Experimental at best.
|#

(module composite-random-source

(;export
  *composite-random-source-constructor
  composite-random-source-constructor
  make-composite-random-state-predicate
  composite-random-state?
  composite-state-ref composite-state-set!)

(import scheme chicken)

(use
  (only data-structures conc reverse-string-append ->string intersperse)
  (only type-errors error-argument-type)
  srfi-1
  numbers
  random-source
  srfi-27-vector-support
  srfi-27)

;;;

(include "srfi-27-common-types")

(define-type composite-random-source-state (list-of *))

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

;; composite-random-source-constructor
;;
;; returns the composite name & constructor
;;
;; ((list-of (integer exact (<= 0 _ _2))) (integer exact positive)
;;  -> (integer exact (<= 0 _ _2)))
;;
;; composite-random-real
;; ((list-of (real inexact (< 0.0 _ 1.0))) #!optional (real inexact (< 0.0 _2 1.0))
;;  -> (real inexact (< 0.0 _ 1.0)))

;FIXME - the combinators are suspect

(: composite-random-source-constructor (#!rest --> procedure))
;
(define (composite-random-source-constructor
          #!rest rest0
          #!key
          (comb-int (lambda (ints n) (modulo (reduce + 0 ints) n)))
          (comb-real (lambda (reals unit) (reduce * 1.0 reals))))
  ;scrub keyword arguments
  (let* (
      (rest (pull-rest-argument rest0) )
      (srcs0
        (if (null? rest)
          (error 'composite-random-source-constructor "empty random-sources")
          (map (cut check-random-source 'composite-random-source-constructor <>) rest) ) ) )
    ;collect features
    (let loop ((srcs srcs0) (names '()) (docus '()) (log2-periods '()) (maxrngs '()))
      (if (null? srcs)
        ;then make composed random-source
        (*composite-random-source-constructor
          comb-int comb-real
          (string->symbol (reverse-string-append (intersperse names "+")))
          (reverse-string-append (intersperse docus " & "))
          ;FIXME minimum? (if this is good then apply along the way)
          (apply min log2-periods)
          (apply min maxrngs)
          srcs0)
        ;else collect info
        (let ((rs (car srcs)))
          (loop
            (cdr srcs)
            (cons (->string (*random-source-name rs)) names)
            (cons (*random-source-documentation rs) docus)
            (cons (*random-source-log2-period rs) log2-periods)
            (cons (*random-source-maximum-range rs) maxrngs)) ) ) ) ) )

;; *composite-random-source-constructor
;;
;; returns the composite constructor

(: *composite-random-source-constructor (procedure procedure random-source-name string number number (list-of random-source) --> procedure))
;
(define (*composite-random-source-constructor comb-int comb-real def-name def-docu log2-period maxrng srcs)
  (let (
    (srcs-cnt (length srcs))
    (composite-make-integers (map random-source-make-integers srcs)) )
    (letrec (
        (ctor
          (lambda (#!optional (name def-name) (docu def-docu))
            (let ((name-state-set!-id (string->symbol (conc name #\- 'state-set!))))
              (*make-random-source
                ;
                ctor
                ;
                name
                ;
                docu
                ;
                log2-period
                ;
                maxrng
                ;entropy-source
                #f
                ;state-ref
                (lambda ()
                  (cons name (map composite-state-ref srcs)))
                ;state-set!
                (lambda (state)
                  (if (composite-random-state? state name srcs-cnt)
                    (for-each composite-state-set! srcs (cdr state))
                    (error-argument-type name-state-set!-id state 'composite-random-state) ) )
                ;randomize!
                (lambda (es)
                  (for-each (lambda (rs) ((@random-source-randomize! rs) es)) srcs) )
                ;pseudo-randomize!
                (lambda (i j)
                  (for-each (lambda (rs) ((@random-source-pseudo-randomize! rs) i j)) srcs) )
                ;make-integers
                (lambda ()
                  (lambda (n)
                    (comb-int (map (cut <> n) composite-make-integers) n)))
                ;make-reals
                (lambda (unit)
                  (let (
                    (makrels (map (lambda (rs) ((@random-source-make-reals rs) unit)) srcs)) )
                    (lambda ()
                      (comb-real (map (cut <>) makrels) unit) ) ) ) ) ) ) ) )
      ctor ) ) )

;;

(: make-composite-random-state-predicate (random-source-name #!optional (or fixnum (list-of random-source)) --> procedure))
;
(define (make-composite-random-state-predicate name #!optional srcs)
  (let (
    (*n*
      (cond
        ((list? srcs)
          (length srcs))
        ((number? srcs)
          srcs)
        (else
          #f))) )
    (lambda (obj)
      (and
        (pair? obj)
        (eq? name (car obj))
        (or (not *n*) (fx= *n* (fx- (length (cdr obj)) 1)))) ) ) )

(: composite-random-state? (* random-source-name (or fixnum (list-of random-source)) --> boolean))
;
(define (composite-random-state? obj name srcs)
  ((make-composite-random-state-predicate name srcs) obj) )

(: composite-state-ref (random-source --> composite-random-source-state))
;
(define (composite-state-ref rs)
  ((@random-source-state-ref rs)) )

(: composite-state-set! (random-source composite-random-source-state -> void))
;
(define (composite-state-set! rs state)
  ((@random-source-state-set! rs) state) )

) ;module composite-random-source
