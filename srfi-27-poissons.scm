;;;; srfi-27-poissons.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-poissons

(;export
  *make-random-poissons
  make-random-poissons)

(import scheme)
(import (chicken base))
(import (chicken type))
(import (chicken fixnum))

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

;;; Poisson distribution

(: *make-random-poissons (number random-real-function --> number-function))
;
(define (*make-random-poissons mu randoms)
  (let ((emu (exp (- mu))))
    (lambda ()
      ;FIXME O(mu) but O(log(mu)) desired for >> mu
      (do ((m 0 (fx+ 1 m))
           (prod (randoms) (* prod (randoms))))
          ((<= prod emu) m)))) )

(: make-random-poissons (#!rest --> number-function procedure))
;
(define (make-random-poissons #!key (mu 1.0) (randoms (current-random-real)))
  (check-nonnegative-real 'make-random-poissons mu 'mu)
  (check-procedure 'make-random-poissons randoms 'randoms)
  (values
    (*make-random-poissons mu randoms)
    (lambda () (values mu randoms))) )

) ;module srfi-27-poissons
