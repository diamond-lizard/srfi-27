;;;; srfi-27-binomials.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-binomials

(;export
  *make-random-binomials
  make-random-binomials)

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
  srfi-27-distributions-support
  srfi-27-bernoullis)

;;;

(include "srfi-27-common-types")

;;; Binomial distribution

(: *make-random-binomials (number number random-real-function --> number-function))
;
(define (*make-random-binomials t p randoms)
  (let ((bernoullis (*make-random-bernoullis p randoms)))
    ;FIXME O(t) but O(log(t)) desired for >> t
    (if (fixnum? t)
      (lambda ()
        (do ((i 0 (fx+ 1 i))
             (n 0 (if (bernoullis) (fx+ 1 n) n)))
            ((fx<= t i) n)))
      (lambda ()
        (do ((i 0 (add1 i))
             (n 0 (if (bernoullis) (add1 n) n)))
            ((<= t i) n))))) )

(: make-random-binomials (#!rest --> number-function procedure))
;
(define (make-random-binomials #!key (t 1) (p 0.5) (randoms (current-random-real)))
  (check-natural-integer 'make-random-binomials t 't)
  (check-real-unit 'make-random-binomials p 'p)
  (check-procedure 'make-random-binomials randoms 'randoms)
  (values
    (*make-random-binomials t p randoms)
    (lambda () (values t p randoms))) )

) ;module srfi-27-binomials
