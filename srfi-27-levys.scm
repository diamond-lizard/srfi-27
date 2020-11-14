;;;; srfi-27-levys.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-levys

(;export
  *make-random-levys
  make-random-levys)

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

;;; Levy distribution

;; See Stable Distributions - John P. Nolan, Formula 1.12

(: *make-random-levys (number number random-real-function --> number-function))
;
(define (*make-random-levys gamma delta randoms)
  (if (and (= 1.0 gamma) (= 0.0 delta))
    (lambda () (let ((r (randoms))) (*reciprocal (* r r))))
    (lambda () (let ((r (randoms))) (+ delta (* gamma (*reciprocal (* r r))))))) )

(: make-random-levys (#!rest --> number-function procedure))
;
(define (make-random-levys #!key (gamma 1.0) (delta 0.0) (randoms (current-random-real)))
  (check-nonnegative-real 'make-random-levys delta 'delta)
  (check-positive-real 'make-random-levys gamma 'gamma)
  (check-procedure 'make-random-levys randoms 'randoms)
  (values
    (*make-random-levys gamma delta randoms)
    (lambda () (values gamma delta randoms))) )

) ;module srfi-27-levys
