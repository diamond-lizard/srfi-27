;;;; srfi-27-vector.scm
;;;; Kon Lovett, Feb '10

; Chicken Generic Arithmetic!

(module srfi-27-vector

(;export
  ;
  make-random-permutations
  make-random-vector
  make-random-hollow-sphere
  make-random-solid-sphere
  ;
  random-permutation!
  random-vector!
  random-hollow-sphere!
  random-solid-sphere!)

(import scheme chicken)

(import
  (only type-checks check-cardinal-integer check-vector check-procedure)
  (only type-errors error-vector)
  random-source
  srfi-27-distributions
  srfi-27-vector-support
  srfi-27
  srfi-27-distributions-support)

;;;

(include "srfi-27-common-types")

;;;

#;
(define:-type (vector-iota (n fixnum)) --> (vector-of fixnum)
  (import (only vector-lib vector-unfold))
  (vector-unfold values n) )

(: vector-iota! (vector fixnum -> void))
;
(define (vector-iota! vec n)
  (do ((i 0 (fx+ i 1)))
      ((fx= i n) vec)
    (vector-set! vec i i) ) )

;;;

;;

;Knuth's "The Art of Computer Programming", Vol. II, 2nd ed., Algorithm P of
;Section 3.4.2
;
(define (*random-permutation! vec rndint)
  (let (
    (n (vector-length vec)) )
    (vector-iota! vec n)
    (do ((k n (fx- k 1)))
        ((fx= k 1) vec )
      ;random-swap
      (let* (
        (i (fx- k 1))
        (j (rndint n))
        (xi (vector-ref vec i))
        (xj (vector-ref vec j)) )
        (vector-set! vec i xj)
        (vector-set! vec j xi) ) ) ) )

(: make-random-permutations (#!rest --> random-vector-function))
;
(define (make-random-permutations #!key (randoms (current-random-integer)))
  (lambda (n)
    (*random-permutation!
      (make-vector
        (check-cardinal-integer 'make-random-permutations n 'length)
        0)
      (check-procedure 'make-random-permutations randoms 'randoms))) )

(: random-permutation! (srfi-27-vector #!rest -> void))
;
(define (random-permutation! vec #!key (randoms (current-random-integer)))
  (*random-permutation!
    (check-vector 'random-permutation! vec)
    (check-procedure 'random-permutation! randoms 'randoms)) )

;;

(: make-random-vector (#!rest --> random-vector-function))
;
(define (make-random-vector #!key (randoms (current-random-real)))
  (lambda (n)
    (vector-filled!
      (make-vector
        (check-cardinal-integer 'random-vector n 'length))
      (check-procedure 'make-random-vector randoms 'randoms))) )

(: random-vector! (srfi-27-vector #!rest -> void))
;
(define (random-vector! vec #!key (randoms (current-random-real)))
  (vector%-filled!
    (check-vector% 'random-vector! vec)
    (check-procedure 'random-vector! randoms 'randoms)) )

;;; Normal vectors

;;

; Fills vect with inexact real random numbers the sum of whose
; squares is equal to 1.0.  Thinking of vect as coordinates in space
; of dimension n = (vector-length vect), the coordinates are
; uniformly distributed over the surface of the unit n-sphere.

(define (**random-hollow-sphere! vec norms)
  (vector%-filled! vec norms)
  (vector%-scale! vec (*reciprocal (sqrt (vector%-sum-squares vec))))
  vec )

(define (*random-hollow-sphere! vec mu sigma randoms)
  (let-values (
    ((norms pl) (make-random-normals #:mu mu #:sigma sigma #:randoms randoms)) )
    (**random-hollow-sphere! vec norms) ) )

(: make-random-hollow-sphere (#!rest --> vector))
;
(define (make-random-hollow-sphere #!key (mu 0.0) (sigma 1.0) (randoms (current-random-real)))
  (let-values (
    ((norms pl) (make-random-normals #:mu mu #:sigma sigma #:randoms randoms)) )
    (lambda (n)
      (**random-hollow-sphere!
        (make-vector (check-cardinal-integer 'random-hollow-sphere n 'length))
        norms) ) ) )

(: random-hollow-sphere! (srfi-27-vector #!rest -> void))
;
(define (random-hollow-sphere! vec #!key (mu 0.0) (sigma 1.0) (randoms (current-random-real)))
  (*random-hollow-sphere!
    (check-vector% 'random-hollow-sphere! vec)
    mu sigma randoms) )

;;

; Fills vect with inexact real random numbers the sum of whose
; squares is less than 1.0.  Thinking of vect as coordinates in
; space of dimension n = (vector-length vect), the coordinates are
; uniformly distributed within the unit n-sphere.

(define (**random-solid-sphere! vec randoms norms)
  (**random-hollow-sphere! vec norms)
  (vector%-scale! vec (expt (randoms) (*reciprocal (vector%-length vec))))
  vec )

(define (*random-solid-sphere! vec mu sigma randoms)
  (let-values (
    ((norms pl) (make-random-normals #:mu mu #:sigma sigma #:randoms randoms)) )
    (**random-solid-sphere! vec randoms norms) ) )

(: make-random-solid-sphere (#!rest --> vector))
;
(define (make-random-solid-sphere #!key (mu 0.0) (sigma 1.0) (randoms (current-random-real)))
  (let-values (
    ((norms pl) (make-random-normals #:mu mu #:sigma sigma #:randoms randoms)) )
    (lambda (n)
      (**random-solid-sphere!
        (make-vector (check-cardinal-integer 'random-solid-sphere n 'length))
        randoms norms) ) ) )

(: random-solid-sphere! (srfi-27-vector #!rest -> void))
;
(define (random-solid-sphere! vec #!key (mu 0.0) (sigma 1.0) (randoms (current-random-real)))
  (*random-solid-sphere!
    (check-vector% 'random-solid-sphere! vec)
    mu sigma randoms) )

) ;module srfi-27-vector
