;;;; srfi-27-exponentials.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-exponentials

(;export
  *make-random-exponentials
  make-random-exponentials)

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

;;; Exponential distribution

;; Knuth's "The Art of Computer Programming", Vol. II, 2nd ed.,
;; Section 3.4.1.D.

(: *make-random-exponentials (number random-real-function --> number-function))
;
(define (*make-random-exponentials mu randoms)
  (if (= 1.0 mu)
    (lambda () (- (log (randoms))))
    (lambda () (* mu (- (log (randoms)))))) )

(: make-random-exponentials (#!rest --> number-function procedure))
;
(define (make-random-exponentials #!key (mu 1.0) (randoms (current-random-real)))
  (check-real-unit 'make-random-exponentials mu 'mu)
  (check-procedure 'make-random-exponentials randoms 'randoms)
  (values
    (*make-random-exponentials mu randoms)
    (lambda () (values mu randoms))) )

) ;module srfi-27-exponentials
