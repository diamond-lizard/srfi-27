;;;; srfi-27-uniform-random.scm
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, Feb '10

(module srfi-27-uniform-random

(;export
  make-uniform-random-integers
  make-uniform-random-reals
  *make-uniform-random-integers)

(import (except scheme + - * quotient = < abs))
(import chicken)

(import (only numbers + - * quotient = < abs))
(import
  data-structures
  (only miscmacros exchange!)
  vector-lib
  (only srfi-27 current-random-source)
  random-source
  (only srfi-27-numbers
    check-integer check-positive-integer
    check-real-precision))

(declare
  (not usual-integrations
    + - * quotient = <))

;;;

(include "srfi-27-common-types")

;; Uniform random integers in [low high] by precision

;; Support

(: *make-uniform-random-integers (number number number random-integer-function --> number-function))
;
(define (*make-uniform-random-integers low high prec rndint)
  (let ((dist (abs (- high low))))
    (if (< dist prec)
      (constantly low)
      (let (
        (rng (quotient (+ dist 1) prec)) )
        (cond
          ((= 0 rng)
            (constantly low) )
          ((= 0 low)
            (if (= 1 prec)
              (lambda ()
                (rndint rng) )
              (lambda ()
                (* (rndint rng) prec) ) ) )
          (else
            (lambda ()
              (+ low (* (rndint rng) prec) ) ) ) ) ) ) ) )

;;;

(: make-uniform-random-integers (#!rest --> number-function procedure))
;
(define (make-uniform-random-integers #!key high (low 0) (precision 1) (source (current-random-source)))
  (check-random-source 'make-uniform-random-integers source 'source)
  (check-integer 'make-uniform-random-integers high 'high)
  (let ((high (or high (- (*random-source-maximum-range source) 1))))
    (check-integer 'make-uniform-random-integers low 'low)
    (check-positive-integer 'make-uniform-random-integers precision 'precision)
    (values
      (*make-uniform-random-integers
        low high precision
        ((@random-source-make-integers source)))
      (lambda ()
        (values high low precision source)) ) ) )

;; Uniform random reals in (0.0 1.0) by precision

(: make-uniform-random-reals (#!rest --> number-function procedure))
;
(define (make-uniform-random-reals #!key (precision #f) (source (current-random-source)))
  (check-random-source 'make-uniform-random-reals source 'source)
  (when precision
    (check-real-precision 'make-uniform-random-reals precision 'precision) )
  (values
    ((@random-source-make-reals source) precision)
    (lambda ()
      (values precision source)) ) )

) ;module srfi-27-uniform-random
