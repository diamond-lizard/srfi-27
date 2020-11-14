;;;; srfi-27-cauchys.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-cauchys

(;export
  *make-random-cauchys
  make-random-cauchys)

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

;;; Cauchy distribution

(: *make-random-cauchys (number number random-real-function --> number-function))
;
(define (*make-random-cauchys median sigma randoms)
  (lambda ()
    (+ median (* sigma (tan (* *pi* (- (randoms) 0.5)))))) )

(: make-random-cauchys (#!rest --> number-function procedure))
;
(define (make-random-cauchys #!key (median 0.0) (sigma 1.0) (randoms (current-random-real)))
  (check-real 'make-random-cauchys median 'median)
  (check-positive-real 'make-random-cauchys sigma 'sigma)
  (check-procedure 'make-random-cauchys randoms 'randoms)
  (values
    (*make-random-cauchys median sigma randoms)
    (lambda () (values median sigma randoms))) )

) ;module srfi-27-cauchys
