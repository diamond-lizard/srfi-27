;;;; srfi-27-paretos.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-paretos

(;export
  *make-random-paretos
  make-random-paretos)

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
  srfi-27-gammas
  srfi-27-exponentials)

;;;

(include "srfi-27-common-types")

;;; Pareto distribution

(: *make-random-paretos (number number random-real-function --> number-function))
;
(define (*make-random-paretos alpha xmin randoms)
  (let ((gammas (*make-random-gammas alpha (*reciprocal xmin) randoms)))
    (*make-random-exponentials 1.0 (lambda () (*reciprocal (+ xmin (gammas)))))) )

(: make-random-paretos (#!rest --> number-function procedure))
;
(define (make-random-paretos #!key (alpha 1.0) (xmin 1.0) (randoms (current-random-real)))
  (check-positive-real 'make-random-paretos alpha 'alpha)
  (check-positive-real 'make-random-paretos xmin 'xmin)
  (check-procedure 'make-random-paretos randoms 'randoms)
  (values
    (*make-random-paretos alpha xmin randoms)
    (lambda () (values alpha xmin randoms))) )

) ;module srfi-27-paretos
