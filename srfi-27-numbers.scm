;;;; srfi-27-numbers.scm
;;;; Kon Lovett, Feb '10

(module srfi-27-numbers

(;export
  ;
  check-integer
  #;check-cardinal-integer
  check-positive-integer
  #;check-real
  #;check-nonzero-real
  #;check-nonnegative-real
  #;check-positive-real
  #;check-real-open-interval
  #;check-real-closed-interval
  check-real-precision
  #;check-real-unit
  ;
  random-large-integer
  random-large-real
  ;
  native-real-precision?)

;NOTE that the module language is not Scheme so one cannot create a list of identifiers,
;respective of context, then use that list for import except or only, or declare.

(import
  (except scheme
    <= < zero? positive? negative?
    + * - / quotient expt
    integer? real?
    exact->inexact inexact->exact
    floor))
(import chicken)

(use
  (only numbers
    <= < zero? positive? negative?
    + * - / quotient expt
    integer? real?
    exact->inexact inexact->exact
    floor))
(use
  (only type-checks
    define-check+error-type
    check-real)
  (only type-errors
    error-argument-type
    error-open-interval error-closed-interval))

(declare
  (not usual-integrations
    <= < zero? positive? negative?
    + * - / quotient expt
    integer? real?
    exact->inexact inexact->exact) )

;;;

(include "srfi-27-common-types")

;;;

;;

; use 'number' conditioned checks

(define (positive-integer? obj)
  (and (integer? obj) (positive? obj)) )

#;
(define (cardinal-integer? obj)
  (and (integer? obj) (<= 0 obj)) )

#;
(define (nonzero-real? obj)
  (and (real? obj) (not (zero? obj))) )

#;
(define (nonnegative-real? obj)
  (and (real? obj) (not (negative? obj))) )

#;
(define (positive-real? obj)
  (and (real? obj) (positive? obj)) )

(define-check+error-type integer)
#;(define-check+error-type cardinal-integer)
(define-check+error-type positive-integer)
#;(define-check+error-type real)
#;(define-check+error-type nonzero-real)
#;(define-check+error-type nonnegative-real)
#;(define-check+error-type positive-real)

(define (check-real-open-interval loc obj mn mx #!optional argnam)
  (check-real loc obj argnam)
  (unless (< mn obj mx)
    (error-open-interval loc obj mn mx argnam) )
  obj )

#;
(define (check-real-closed-interval loc obj mn mx #!optional argnam)
  (check-real loc obj argnam)
  (unless (<= mn obj mx)
    (error-closed-interval loc obj mn mx argnam) )
  obj )

(define (check-real-precision loc obj #!optional argnam)
  (check-real-open-interval loc obj 0 1 argnam) )

#;
(define (check-real-unit loc obj #!optional argnam)
  (check-real-closed-interval loc obj 0 1 argnam) )

;;;

; 'prec-max - maximum range of "core" random integer generator (maybe inexact)
; 'm        - exact maximum range (passed so conversion performed by caller)

; Chicken 'numbers' egg determines precision on arg1 so
; convert result of the "base" random integer (which may return a
; flonum due to integer representation issues) into an exact integer
; to prevent infinity generation when the value of n * m is very large.
; (the conversion of the large exact integer to a flonum may result in
; +inf)

(: random-power (procedure * number full-integer fixnum --> full-number))
;
(define (random-power rndint state prec-max m k) ;n = m^k, k >= 1
  (let (
    (rnd (lambda () (inexact->exact (rndint state prec-max)))) )
    (do ((k k (fx- k 1))
         (n (rnd) (+ (rnd) (* n m))) )
        ((fx= 1 k) n) ) ) )

; Large Integers
; ==============
;
; To produce large integer random deviates, for n > m, we first
; construct large random numbers in the range {0..m^k-1} for some
; k such that m^k >= n and then use the rejection method to choose
; uniformly from the range {0..n-1}.

(: random-large-integer (procedure * number full-integer full-integer --> full-number))
;
(define (random-large-integer rndint state prec-max m n) ;n > m
  (do ((k 2 (fx+ k 1))
       (mk (* m m) (* mk m)))
      ((<= n mk)
        (let* (
          (mk-by-n (quotient mk n))
          (a (* mk-by-n n)) )
          (let loop ()
            (let (
              (x (random-power rndint state prec-max m k)) )
              (if (< x a)
                (quotient x mk-by-n)
                (loop) ) ) ) ) ) ) )

; Multiple Precision Reals
; ========================
;
; To produce multiple precision reals we produce a large integer value
; and convert it into a real value. This value is then normalized.
; The precision goal is prec <= 1/(m^k + 1), or 1/prec - 1 <= m^k.
; If you know more about the floating point number types of the
; Scheme system, this can be improved.

(: random-large-integer (procedure * number full-integer number --> full-number))
;
(define (random-large-real rndint state prec-max m prec)
  (do ((k 1 (fx+ k 1))
       (u (- (/ 1 prec) 1) (/ u m)) )
      ((<= u 1)
        (exact->inexact
          (/
            (+ 1 (random-power rndint state prec-max m k))
            (+ 1 (expt m k)))) ) ) )

;;;

(: native-real-precision? ((or boolean number) number --> boolean))
;
(define (native-real-precision? prec prec-max)
  (or
    (not prec)
    (<= (- (/ 1 prec) 1) prec-max)) )

) ;module srfi-27-numbers
