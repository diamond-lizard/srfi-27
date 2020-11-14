;;;; entropy-linux.scm
;;;; Kon Lovett, Oct '17

(module entropy-linux

(;export
  make-entropy-source-getrandom)

(import scheme chicken)

(use entropy-source entropy-procedure)

;;; Entropy from getrandom

#>
#include <sys/random.h>
<#

;FIXME needs GRND_NONBLOCK loop on EAGAIN/EINTR w/ (random ?) wait & max-trys
;FIXME ? handle ENOSYS/EINVAL/EFAULT ?

(define getrandom_double
  (foreign-lambda* double ((u8vector u8vec))
    ;Chicken SRFI 4 heap allocated vectors have 8-byte alignment!
    "double *buf = (double *) u8vec;
    ssize_t res = getrandom( buf, sizeof buf, 0 );
    if (res == sizeof buf) {
      C_return( (isnormal( *buf ) ? fabs( *buf ) : -1.0) );
    }
    C_return( -1.0 );"))

(define getrandom_u8int
  (foreign-lambda* int ()
    "uint8_t buf;
    ssize_t res = getrandom( buf, sizeof buf, 0 );
    if (res == sizeof buf) {
      C_return( buf );
    }
    /* WRONG */
    C_return( buf );"))

(define getrandom_u8proc )

(define getrandom_f64proc
  )

(define (make-entropy-source-getrandom)
  (make-entropy-source/procedure
    getrandom_u8proc
    getrandom_f64proc
    "getrandom"
    'getrandom
    "Entropy from getrandom") )

(register-entropy-source! 'getrandom make-entropy-source-getrandom)

) ;module entropy-linux
