;;;; moa.scm
;;;; Kon Lovett, Oct '09

(module moa

(;export
  make-random-source-moa)

(import scheme)
(import (chicken base))

(import (except scheme <= inexact->exact exact->inexact number?))
(import scheme)
(import (chicken base))
(import foreign)

(import
  srfi-4
  random-source
  entropy-source
  (only srfi-27-numbers
    check-positive-integer
    random-large-integer random-large-real
    native-real-precision?))

(declare
  (not usual-integrations
    <= exact->inexact inexact->exact))

#>
#include <math.h>

/* bitsizeof is C++, sigh */
#define bitsizeof( t ) (CHAR_BIT * sizeof( t ))

/* Mother **************************************************************
|   George Marsaglia's The mother of all random number generators
|       producing uniformly distributed pseudo random 32 bit values with
|       period about 2^250.
|
|   The arrays mother1 and mother2 store carry values in their
|       first element, and random 16 bit numbers in elements 1 to 8.
|       These random numbers are moved to elements 2 to 9 and a new
|       carry and number are generated and placed in elements 0 and 1.
|   The arrays mother1 and mother2 are filled with random 16 bit values.
|
|   Returns:
|   A 32 bit random number is obtained by combining the output of the
|       two generators
|
|   Bob Wheeler 8/8/94
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

#define M15MASK 0x7FFF           /* mask for lower 15 bits */
#define M31MASK 0x7FFFFFFF       /* mask for 31 bits */
#define M32DOUBLE 4294967295.0   /* 2^32-1 */

#define STATE_LENGTH 10

#define low16( x ) ((x) & 0xffff)
#define high16( x ) (((x) >> 16) & 0xffff)
#define make32( x, y ) (((x) << 16) | (y))

#define low32( x ) ((x) & 0xffffffff)
#define high32( x ) (((x) >> 32) & 0xffffffff)

static void
init_state( uint32_t *state, uint64_t seed )
{
  int i;

  /* Initialize mother with 9 random values */

  uint32_t nseed = low32( seed );
  uint64_t nxtval = high32( seed ) & M31MASK;

  for( i = 0; i < STATE_LENGTH; ++i ) {
    /* One line multiply-with-carry */
    nxtval = 30903 * nseed + high32( nxtval );
    state[ i ] = nseed = low32( nxtval );
  }

  /* make carry 31 bits */
  state[ 0 ] &= M31MASK;
}

static uint32_t
uniformu32( uint32_t *state )
{
# define mom_mov( m, i ) ((m)[ (i) + 1 ] = (m)[ (i) ])

  uint64_t nxtval;

  /* Move elements 1 to 8 to 2 to 9 */
  mom_mov( state, 8 );
  mom_mov( state, 7 );
  mom_mov( state, 6 );
  mom_mov( state, 5 );
  mom_mov( state, 4 );
  mom_mov( state, 3 );
  mom_mov( state, 2 );
  mom_mov( state, 1 );

  /* Form the linear combinations */
  nxtval = state[ 0 ]
          + make32( 1111, 1941) * state[ 2 ] + make32( 2222, 1860 ) * state[ 3 ]
          + make32( 3333, 1812 ) * state[ 4 ] + make32( 4444, 1776 ) * state[ 5 ]
          + make32( 5555, 1492 ) * state[ 6 ] + make32( 6666, 1215 ) * state[ 7 ]
          + make32( 7777, 1066 ) * state[ 8 ] + make32( 9272, 12013 ) * state[ 9 ];

  /* Save the high bits of nxtval as the new carry */
  state[ 0 ] = high32( nxtval );

  /* The low bits of nxtval are the new "seed" */
  state[ 1 ] = low32( nxtval );

  return state[ 1 ];

# undef mom_mov
}

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

#undef high32
#undef low32
#undef make32
#undef high16
#undef low16

#undef STATE_LENGTH
#undef M32DOUBLE
#undef M31MASK
#undef M15MASK

#undef bitsizeof
<#

(define init_state (foreign-lambda void "init_state" nonnull-u32vector unsigned-integer64))

(define moa-random-integer (foreign-lambda unsigned-integer32 "randomu32" nonnull-u32vector unsigned-integer32))
(define moa-random-real (foreign-lambda double "uniformf64" nonnull-u32vector))

;;;

;;

(define-constant maximum-unsigned-integer32-flonum 4294967295.0)

(cond-expand
  (64bit
    (define-constant maximum-unsigned-integer32 4294967295) )    ;MAX
  (else ;32bit
    (define-constant maximum-unsigned-integer32 1073741823) ) )  ;32bit most-positive-fixnum

(define-constant fpMAX maximum-unsigned-integer32-flonum)  ;2^32 - 1
(define-constant LOG2-PERIOD 250)

(define eMAX (inexact->exact fpMAX)) ;Create a "bignum" if necessary

(define-constant INITIAL-SEED maximum-unsigned-integer32-flonum)

(define-constant STATE-LENGTH 10)

(define INTERNAL-ID 'moa)
(define EXTERNAL-ID 'moa)

;;

(define (make-state) (make-u32vector STATE-LENGTH))

(define (moa-initial-state)
  (let ((state (make-state)))
    (init_state state INITIAL-SEED)
    state ) )

(define (moa-unpack-state state)
  (cons EXTERNAL-ID (u32vector->list state)) )

(define (moa-pack-state external-state)
  (unless (moa-external-state? external-state)
    (error 'moa-pack-state "malformed state" external-state) )
  (let ((state (make-state)))
    (do ((i 0 (fx+ i 1))
         (ss (cdr external-state) (cdr ss)) )
        ((null? ss) state)
      (let ((x (car ss)))
        ;(if {integer?(x) && 0 <= x <= 4294967295}
        ;(if {integer?(x) && 0 <= x && x <= 4294967295}
        (if (and (integer? x) (<= 0 x 4294967295))
          (u32vector-set! state i x)
          (error 'moa-pack-state "illegal value" x) ) ) ) ) )

(define (moa-external-state? obj)
  (and
    (pair? obj)
    (eq? EXTERNAL-ID (car obj))
    (fx= (fx+ STATE-LENGTH 1) (length obj)) ) )

(define (moa-randomize-state state entropy-source)
  (init_state
    state
    (exact->inexact
      (modulo
        (fpabs (entropy-source-f64-integer entropy-source))
        fpMAX)))
  state )

(define (moa-pseudo-randomize-state i j)
  (let ((state (make-state)))
    (init_state state (exact->inexact (+ i j)))
    state ) )

(define (moa-random-large state n)
  (random-large-integer moa-random-integer state fpMAX eMAX n) )

(define (moa-random-real-mp state prec)
  (random-large-real moa-random-integer state fpMAX eMAX prec) )

;;;

(define (make-random-source-moa)
  (let ((state (moa-initial-state)))
    (*make-random-source
      ;
      make-random-source-moa
      ;
      EXTERNAL-ID
      ;
      "George Marsaglia's Mother-Of-All Generator"
      ;
      LOG2-PERIOD
      ;
      fpMAX
      ;
      #f
      ;
      (lambda ()
        (moa-unpack-state state) )
      ;
      (lambda (new-state)
        (set! state (moa-pack-state new-state)) )
      ;
      (lambda (entropy-source)
        (set! state (moa-randomize-state state entropy-source)) )
      ;
      (lambda (i j)
        (set! state (moa-pseudo-randomize-state i j)) )
      ;
      (lambda ()
        (lambda (n)
          (check-positive-integer INTERNAL-ID n 'range)
          (cond-expand
            (64bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (moa-random-integer state n))
                (else
                  (moa-random-large state n) ) ) )
            (else ;32bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (moa-random-integer state n))
                ;'n' maybe bignum - must be convertable to "unsigned-integer32"
                ((<= n eMAX)
                  (moa-random-integer state (exact->inexact n)))
                (else
                  (moa-random-large state n) ) ) ) ) ) )
      ;
      (lambda (prec)
        (cond
          ((native-real-precision? prec eMAX)
            (lambda ()
              (moa-random-real state) ) )
          (else
            (lambda ()
              (moa-random-real-mp state prec) ) ) ) ) ) ) )

;;;
;;; Module Init
;;;

(register-random-source! INTERNAL-ID make-random-source-moa)

) ;module moa
