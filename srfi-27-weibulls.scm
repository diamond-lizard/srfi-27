;;;; srfi-27-weibulls.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-weibulls

(;export
  *make-random-weibulls
  make-random-weibulls)

(import scheme chicken)

(import
  (only type-errors error-argument-type)
  (only type-checks
    define-check+error-type
    check-procedure
    check-cardinal-integer
    check-real
    check-open-interval
    check-closed-interval)
  srfi-27
  srfi-27-distributions-support)

;;;

(include "srfi-27-common-types")

;;; Weibull distribution

(: *make-random-weibulls (number number random-real-function --> number-function))
;
(define (*make-random-weibulls shape scale randoms)
  (let (
    (invscale (*-reciprocal scale))
    (invshape (*reciprocal shape)) )
    (lambda () (expt (* invscale (log (- 1.0 (randoms)))) invshape)) ) )

(: make-random-weibulls (#!rest --> number-function procedure))
;
(define (make-random-weibulls #!key (shape 1.0) (scale 1.0) (randoms (current-random-real)))
  (check-positive-real 'make-random-weibulls shape 'shape)
  (check-positive-real 'make-random-weibulls scale 'scale)
  (check-procedure 'make-random-weibulls randoms 'randoms)
  (values
    (*make-random-weibulls shape scale randoms)
    (lambda () (values shape scale randoms))) )

) ;module srfi-27-weibulls
