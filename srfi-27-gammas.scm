;;;; srfi-27-gammas.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-gammas

(;export
  *make-random-gammas
  make-random-gammas)

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

;;; Gamma distribution

;; "A Simple Method for Generating Gamma Variables", George Marsaglia & Wai Wan Tsang,
;; ACM Transactions on Mathematical Software, Vol. 26, No. 3, September 2000, Pages 363 372.

(: *make-random-gammas (number number random-real-function --> number-function))
;
(define (*make-random-gammas alpha theta randoms)
  (if (= 1.0 alpha)
    ;then special case
    (if (= 1.0 theta)
      (lambda () (- (log (randoms))))
      (lambda () (* theta (- (log (randoms))))) )
    ;else general case
    (let (
      (normals (*make-random-normals 0.0 1.0 randoms) )
      (uniforms
        (if (< alpha 1.0)
          (let ((alpha-inv (*reciprocal alpha)))
            (lambda () (expt (randoms) alpha-inv) ) )
          randoms) ) )
      ;
      (let* (
        (d (- (if (< alpha 1.0) (+ 1.0 alpha) alpha) *one-third*) )
        (c (*reciprocal (sqrt (* 9.0 d))) ) )
        ;
        (lambda ()
          (*
            theta
            (let loop ()
              (let* (
                (x (normals) )
                (v (+ 1.0 (* c x)) ) )
                ;
                (define (gamma?)
                  (let (
                    (u (uniforms) )
                    (x^2 (* x x) )
                    (v^3 (* v v v) ) )
                    ;
                    (or
                      (< u (- 1.0 (* 0.0331 x^2 x^2)))
                      (< (log u) (+ (* 0.5 x^2) (* d (- 1.0 (+ v^3 (log v^3)))))) ) ) )
                ;
                (if (and (< 0.0 v) (gamma?))
                   (* d v)
                   (loop) ) ) ) ) ) ) ) ) )

(: make-random-gammas (#!rest --> number-function procedure))
;
(define (make-random-gammas #!key (alpha 1.0) (theta 1.0) (randoms (current-random-real)))
  (check-positive-real 'make-random-gammas alpha 'alpha)
  (check-positive-real 'make-random-gammas theta 'theta)
  (check-procedure 'make-random-gammas randoms 'randoms)
  (values
    (*make-random-gammas alpha theta randoms)
    (lambda () (values alpha theta randoms))) )

) ;module srfi-27-gammas
