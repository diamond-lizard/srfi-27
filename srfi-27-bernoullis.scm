;;;; srfi-27-bernoullis.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-bernoullis

(;export
  *make-random-bernoullis
  make-random-bernoullis)

(import scheme)
(import (chicken base))

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
  srfi-27-distributions-support)

;;;

(include "srfi-27-common-types")

;;; Bernoulli distribution

(: *make-random-bernoullis (number random-real-function --> boolean-function))
;
(define (*make-random-bernoullis p randoms)
  (cond
    ((= 0.0 p) (lambda () #f))
    ((= 1.0 p) (lambda () #t))
    (else      (lambda () (<= (randoms) p)))) )

(: make-random-bernoullis (#!rest --> boolean-function procedure))
;
(define (make-random-bernoullis #!key (p 0.5) (randoms (current-random-real)))
  (check-real-unit 'make-random-bernoullis p 'p)
  (check-procedure 'make-random-bernoullis randoms 'randoms)
  (values
    (*make-random-bernoullis p randoms)
    (lambda () (values p randoms))) )

) ;module srfi-27-bernoullis
