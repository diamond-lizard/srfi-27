;;;; entropy-clock.scm
;;;; Kon Lovett, Oct '09

(module entropy-clock

(;export
  make-entropy-source-system-clock)

(import scheme)
(import (chicken base))
(import (chicken foreign))

(import entropy-source entropy-support)

;;;

#>
#include <time.h>
#include <math.h>
/* linear congruential generator */
#define MULTIPLIER  25214903917.0
#define LIMITER     81474976711000.0
<#

(define f64init (foreign-lambda* double ()
  "C_return( (double) time( NULL ) );"))

(define f64rand (foreign-lambda* double ((double f64seed))
  "double x = 11.0 + (MULTIPLIER * f64seed);
  C_return( fmod( x, LIMITER ) );"))

;;;

(include "srfi-27-common-types")

;;; Entropy from system clock

(: make-entropy-source-system-clock (--> entropy-source))
;
(define (make-entropy-source-system-clock)
  (let* (
    (*f64seed* (f64init))
    (*f64rand*
      (lambda ()
        (set! *f64seed* (f64rand *f64seed*))
        *f64seed*)) )
    (*make-entropy-source
      ;
      make-entropy-source-system-clock
      ;
      'system-clock
      ;
      "Entropy from system clock"
      ;
      (lambda ()
        (make-entropic-u8/f64 *f64rand*) )
      ;
      *f64rand*
      ;
      (lambda (u8cnt u8vec)
        (entropic-u8vector-filled/f64 u8cnt u8vec *f64rand*) )
      ;
      (lambda (f64cnt f64vec)
        (entropic-f64vector-filled f64cnt f64vec *f64rand*) ) ) ) )

(register-entropy-source! 'system-clock make-entropy-source-system-clock)

) ;module entropy-clock
