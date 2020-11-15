;;;; srfi-27-erlangs.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-erlangs

(;export
  *make-random-erlangs
  make-random-erlangs)

(import scheme)
(import (chicken base))
(import (chicken type))

(import
  (only type-errors error-argument-type)
  (only type-checks
    define-check+error-type
    check-procedure
    check-natural-integer
    check-real
    check-open-interval
    check-closed-interval)
  srfi-27
  srfi-27-distributions-support
  srfi-27-gammas)

;;;

(include "srfi-27-common-types")

;;; Erlang distribution

(: *make-random-erlangs (number number random-real-function --> number-function))
;
(define (*make-random-erlangs alpha theta randoms)
  (*make-random-gammas (exact->inexact alpha) (exact->inexact theta) randoms) )

(: make-random-erlangs (#!rest --> number-function procedure))
;
(define (make-random-erlangs #!key (alpha 1) (theta 1.0) (randoms (current-random-real)))
  (check-positive-real 'make-random-erlangs alpha 'alpha)
  (check-positive-real 'make-random-erlangs theta 'theta)
  (check-procedure 'make-random-erlangs randoms 'randoms)
  (values
    (*make-random-erlangs alpha theta randoms)
    (lambda () (values alpha theta randoms))) )

) ;module srfi-27-erlangs
