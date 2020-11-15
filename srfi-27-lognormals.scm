;;;; srfi-27-lognormals.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-lognormals

(;export
  *make-random-lognormals
  make-random-lognormals)

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
  srfi-27-normals)

;;;

(include "srfi-27-common-types")

;;; Lognormal distribution

(: *make-random-lognormals (number number random-real-function --> number-function))
;
(define (*make-random-lognormals mu sigma randoms)
  (let (
    (normals (*make-random-normals 0.0 1.0 randoms) )
    (nmu (log (* mu (/ mu (sqrt (+ (* sigma sigma) (* mu mu)))))) )
    (nsigma (sqrt (log (+ 1.0 (* sigma (/ sigma mu mu))))) ) )
    ;
    (lambda ()
      (exp (+ nmu (* (normals) nsigma))))) )

(: make-random-lognormals (#!rest --> number-function procedure))
;
(define (make-random-lognormals #!key (mu 1.0) (sigma 1.0) (randoms (current-random-real)))
  (check-nonzero-real 'make-random-lognormals mu 'mu)
  (check-nonnegative-real 'make-random-lognormals sigma 'sigma)
  (check-procedure 'make-random-lognormals randoms 'randoms)
  (values
    (*make-random-lognormals mu sigma randoms)
    (lambda () (values mu sigma randoms))) )

) ;module srfi-27-lognormals
