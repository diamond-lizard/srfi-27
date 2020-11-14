;;;; srfi-27-distributions-support.scm
;;;; Kon Lovett, Dec '17

; Chicken Generic Arithmetic!
; (could use fp/fl routines since only floating-point arithmetic)

(module srfi-27-distributions-support

(;export
  *pi* *one-third*
  ;
  nonzero-real? check-nonzero-real error-nonzero-real
  nonnegative-real? check-nonnegative-real error-nonnegative-real
  positive-real? check-positive-real error-positive-real
  check-real-open-interval
  check-real-closed-interval
  check-real-unit
  ;
  *reciprocal *-reciprocal)

(import scheme)
(import (chicken base))

(import
  (only mathh-consts pi)
  (only type-errors error-argument-type)
  (only type-checks
    define-check+error-type
    check-procedure
    check-cardinal-integer
    check-real
    check-open-interval
    check-closed-interval)
  srfi-27)

;;; Constants

(define *pi* pi)

(define *one-third* (fp/ 1.0 3.0))

;;; Chicken Generic Arithmetic Argument Checks

(define (nonzero-real? obj)
  (and (real? obj) (not (zero? obj))) )

(define (nonnegative-real? obj)
  (and (real? obj) (not (negative? obj))) )

(define (positive-real? obj)
  (and (real? obj) (positive? obj)) )

(define-check+error-type nonzero-real)
(define-check+error-type nonnegative-real)
(define-check+error-type positive-real)

(define (check-real-open-interval loc obj mn mx #!optional argnam)
  (check-real loc obj argnam)
  (check-real loc mn argnam)
  (check-real loc mx argnam)
  (check-open-interval loc obj mn mx argnam)
  obj )

(define (check-real-closed-interval loc obj mn mx #!optional argnam)
  (check-real loc obj argnam)
  (check-real loc mn argnam)
  (check-real loc mx argnam)
  (check-closed-interval loc obj mn mx argnam)
  obj )

(define (check-real-unit loc obj #!optional argnam)
  (check-real-closed-interval loc obj 0 1 argnam) )

;;; Mathh

;TODO in case special processing needed near limits

(define-syntax *reciprocal
  (syntax-rules ()
    ((_ ?n)
      (let ((n ?n)) (/ 1.0 n)) ) ) )

(define-syntax *-reciprocal
  (syntax-rules ()
    ((_ ?n)
      (let ((n ?n)) (/ -1.0 n)) ) ) )

) ;module srfi-27-distributions-support
