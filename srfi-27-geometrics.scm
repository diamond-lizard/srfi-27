;;;; srfi-27-geometrics.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-geometrics

(;export
  *make-random-geometrics
  make-random-geometrics)

(import scheme chicken)

(use
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

;;; Geometric distribution

(: *make-random-geometrics  (number random-real-function --> number-function))
;
(define (*make-random-geometrics p randoms)
  (let ((log-p (log p)))
    (lambda ()
      (+ 1 (inexact->exact (floor (/ (log (- 1.0 (randoms))) log-p)))))) )

(: make-random-geometrics (#!rest --> number-function procedure))
;
(define (make-random-geometrics #!key (p 0.5) (randoms (current-random-real)))
  (check-real-unit 'make-random-geometrics p 'p)
  (check-procedure 'make-random-geometrics randoms 'randoms)
  (values
    (*make-random-geometrics p randoms)
    (lambda () (values p randoms))) )

) ;module srfi-27-geometrics
