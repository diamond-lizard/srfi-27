;;;; mwc.scm
;;;; Kon Lovett, May '06

#>
#include <math.h>

/* bitsizeof is C++, sigh */
#define bitsizeof( t ) (CHAR_BIT * sizeof( t ))

/*
 * This is the MWC (Multiply With Carry) random number generator
 * described by George Marsaglia at the Department of Statistics and
 * Supercomputer Computations Research Institute, The Florida State
 * University (http://stat.fsu.edu/~geo).
 *
 * It uses 64 bits, has a period of 4578426017172946943 (4.6e18 or 2^62),
 * and passes all tests in the DIEHARD test suite
 * (http://stat.fsu.edu/~geo/diehard.html)
 */

static uint8_t masktab[ 256 ] = {
  0,
  1,
  3, 3,
  7, 7, 7, 7,
  15, 15, 15, 15, 15, 15, 15, 15,
  31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31,
  63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
  63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
  127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127,
  127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127,
  127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127,
  127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
  255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255
};

#define A         2131995753UL
#define M32DOUBLE 4294967295.0   /* 2^32-1 */

#define W 0
#define C 1

#define low16( x ) ((x) & 0xffff)
#define high16( x ) (((x) >> 16) & 0xffff)

#define low32( x ) ((x) & 0xffffffffUL)
#define high32( x ) (((x) >> 32) & 0xffffffffUL)

#ifdef C_SIXTY_FOUR
static uint32_t
uniformu32( uint32_t *state )
{
  uint64_t x = (uint64_t) A * state[ W ] + state[ C ];
  uint32_t w = low32( x );
  state[ W ] = w;
  state[ C ] = high32( x );
  return w;
}
#else
static uint32_t
uniformu32( uint32_t *state )
{
  /* From Guile */
  uint32_t x1 = low16( A ) * low16( state[ W ] );
  uint32_t x2 = low16( A ) * high16( state[ W ] );
  uint32_t x3 = high16( A ) * low16( state[ W ] );
  uint32_t w = low16( x1 ) + low16( state[ C ] );
  uint32_t m = high16( x1 ) + low16( x2 ) + low16( x3 ) + high16( state[ C ] ) + high16( w );
  uint32_t x4 = high16( A ) * high16( state[ W ] );
  state[ W ] = w = (low16( m ) << 16) | low16( w );
  state[ C ] = high16( x2 ) + high16( x3 ) + x4 + high16( m );
  return w;
}
#endif

static double
uniformf64( uint32_t *state )
{
  return ((((double) uniformu32( state )) / M32DOUBLE) + ((double) uniformu32( state ))) / M32DOUBLE;
}

static uint32_t
randomu32( uint32_t *state, uint32_t m )
{
  uint32_t r, mask;
  mask = (m < 256
            ? masktab[ m ]
            : (m < 65536
                 ? masktab[ m >> 8 ] << 8 | 0xff
                 : (m < 0x1000000
                      ? masktab[ m >> 16 ] << 16 | 0xffff
                      : masktab[ m >> 24 ] << 24 | 0xffffff)));
  while( (r = uniformu32( state ) & mask) >= m );
  return r;
}

static void
init_state( uint32_t *state, uint64_t seed )
{
  uint32_t w = low32( seed );
  uint32_t c = high32( seed );

  if( (w == 0 && c == 0) || (w == -1 && c == A - 1) )
    ++c;

  state[ W ] = w;
  state[ C ] = c;
}

static void
uniformu32_ith_state( uint32_t *state, uint32_t i )
{
  for( ;i > 0; --i )
    uniformu32( state );
}

static void
uniformu32_jth_offset_state( uint32_t *state, uint32_t j )
{
  uint64_t x = (uint64_t) pow( A, j ) * state[ W ] + state[ C ];
  state[ W ] = low32( x );
  state[ C ] = high32( x );
}

#undef high32
#undef low32
#undef high16
#undef low16

#undef C
#undef W

#undef M32DOUBLE
#undef A

#undef bitsizeof
<#

(declare
  (not usual-integrations
    <=  exact->inexact inexact->exact))

(module mwc

(;export
  make-random-source-mwc)

(import scheme)
(import (chicken base))

(import (chicken foreign))
(import (chicken fixnum))
(import (chicken flonum))

(import
  srfi-4
  (only type-errors error-positive-integer)
  random-source
  entropy-source
  (only srfi-27-numbers
    check-positive-integer
    random-large-integer random-large-real
    native-real-precision?))

(define init_state (foreign-lambda void "init_state" nonnull-u32vector unsigned-integer64))

(define uniformu32_ith_state (foreign-lambda void "uniformu32_ith_state" nonnull-u32vector unsigned-integer32))
(define uniformu32_jth_offset_state (foreign-lambda void "uniformu32_jth_offset_state" nonnull-u32vector unsigned-integer32))

(define mwc-random-integer (foreign-lambda unsigned-integer32 "randomu32" nonnull-u32vector unsigned-integer32))
(define mwc-random-real (foreign-lambda double "uniformf64" nonnull-u32vector))

;;

(define-constant maximum-unsigned-integer32-flonum 4294967295.0)

(cond-expand
  (64bit
    (define-constant maximum-unsigned-integer32 4294967295) )    ;MAX
  (else ;32bit
    (define-constant maximum-unsigned-integer32 1073741823) ) )  ;32bit most-positive-fixnum

(define-constant fpMAX maximum-unsigned-integer32-flonum)  ;2^32 - 1
(define-constant LOG2-PERIOD 62)

(define eMAX (inexact->exact fpMAX)) ;Create a "bignum" if necessary

(define-constant INITIAL-SEED (inexact->exact maximum-unsigned-integer32-flonum))

(define-constant STATE-LENGTH 2)

(define-constant STATE-VALUE-MAXIMUM 4294967295)

(define-constant STATE-EXTERNAL-LENGTH 3)

(define INTERNAL-ID 'mwc)
(define EXTERNAL-ID 'mwc)

;;

(define (make-state) (make-u32vector STATE-LENGTH))

(define (mwc-initial-state)
  (let ((state (make-state)))
    (init_state state INITIAL-SEED)
    state ) )

(define (mwc-unpack-state state)
  (cons EXTERNAL-ID (u32vector->list state)) )

(define (mwc-pack-state external-state)
  (unless (mwc-external-state? external-state)
    (error 'mwc-pack-state "malformed state" external-state) )
  (let* (
    (state (make-state))
    (setter
      (lambda (i x)
        (if (and (integer? x) (<= 0 x STATE-VALUE-MAXIMUM))
          (u32vector-set! state i x)
          (error 'mwc-pack-state "illegal state value" x)))) )
    (setter 0 (cadr external-state))
    (setter 1 (caddr external-state))
    state ) )

(define (mwc-external-state? obj)
  (and
    (pair? obj)
    (eq? EXTERNAL-ID (car obj))
    (fx= STATE-EXTERNAL-LENGTH (length obj)) ) )

;; 64 bit entropy used as a bit source, not a number source!
(define (mwc-randomize-state state entropy-source)
  (init_state
    state
    (inexact->exact
      (modulo
        (fpabs (entropy-source-f64-integer entropy-source))
        fpMAX)))
  state )

(define (mwc-pseudo-randomize-state i j)
  (let ((state (make-state)))
    (init_state state 0)
    (uniformu32_ith_state state i)
    (uniformu32_jth_offset_state state j)
    state ) )

(define (mwc-random-large state n)
  (random-large-integer mwc-random-integer state fpMAX eMAX n) )

(define (mwc-random-real-mp state prec)
  (random-large-real mwc-random-integer state fpMAX eMAX prec) )

;;;

(define (make-random-source-mwc)
  (let ((state (mwc-initial-state)))
    (*make-random-source
      ;
      make-random-source-mwc
      ;
      EXTERNAL-ID
      ;
      "George Marsaglia's Multiply With Carry Generator"
      ;
      LOG2-PERIOD
      ;
      fpMAX
      ;
      #f
      ;
      (lambda ()
        (mwc-unpack-state state) )
      ;
      (lambda (new-state)
        (set! state (mwc-pack-state new-state)) )
      ;
      (lambda (entropy-source)
        (set! state (mwc-randomize-state state entropy-source)) )
      ;
      (lambda (i j)
        (set! state (mwc-pseudo-randomize-state i j)) )
      ;
      (lambda ()
        (lambda (n)
          (check-positive-integer INTERNAL-ID  n 'range)
          (cond-expand
            (64bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (mwc-random-integer state n))
                (else
                  (mwc-random-large state n) ) ) )
            (else ;32bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (mwc-random-integer state n))
                ;'n' maybe bignum - must be convertable to "unsigned-integer32"
                ((<= n eMAX)
                  (mwc-random-integer state (exact->inexact n)))
                (else
                  (mwc-random-large state n) ) ) ) ) ) )
      ;
      (lambda (prec)
        (cond
          ((native-real-precision? prec eMAX)
              (lambda ()
                (mwc-random-real state) ) )
          (else
            (lambda ()
              (mwc-random-real-mp state prec) ) ) ) ) ) ) )

;;;
;;; Module Init
;;;

(register-random-source! INTERNAL-ID make-random-source-mwc)

) ;module mwc
