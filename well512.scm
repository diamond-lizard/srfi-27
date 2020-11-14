;;;; well512.scm
;;;; Kon Lovett, Nov '17

(module well512

(;export
  make-random-source-well512)

(import (except scheme <= inexact->exact exact->inexact number?))

(import chicken foreign)

(use (only numbers <= inexact->exact exact->inexact number?))
(use
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
/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#define R 16

typedef struct {
  unsigned int i;
  uint32_t *state; /*[R]*/
} WELL512aState;

void
InitWELLRNG512a( WELL512aState *well )
{
   well->i = 0;
   for (int j = 0; j < R; j++)
     well->state[j] = rand()^(rand()<<16)^(rand()<<31);
}

double
WELLRNG512a( WELL512aState *well )
{
# define W 32
# define P 0
# define M1 13
# define M2 9
# define M3 5

# define OFF( o ) ((well->i + (o)) & 0x0000000fU)

# define V0       well->state[OFF( 0 )]
# define VM1      well->state[OFF( M1 )]
# define VM2      well->state[OFF( M2 )]
# define VM3      well->state[OFF( M3 )]
# define VRm1     well->state[OFF( 15 )]
# define VRm2     well->state[OFF( 14 )]
# define newV0    well->state[OFF( 15 )]
# define newV1    well->state[OFF( 0 )]
# define newVRm1  well->state[OFF( 14 )]

# define MAT0POS( t, v )     (v^(v>>t))
# define MAT0NEG( t, v )     (v^(v<<(-(t))))
# define MAT3NEG( t, v )     (v<<(-(t)))
# define MAT4NEG( t, b, v )  (v^((v<<(-(t))) & b))

# define FACT 2.32830643653869628906e-10

  uint32_t z0 = VRm1;
  uint32_t z1 = MAT0NEG( -16, V0 ) ^ MAT0NEG( -15, VM1 );
  uint32_t z2 = MAT0POS( 11, VM2 );

  newV1 = z1 ^ z2;
  newV0 = MAT0NEG( -2, z0 ) ^ MAT0NEG( -18, z1 ) ^ MAT3NEG( -28, z2 ) ^ MAT4NEG(-5, 0xda442d24U, newV1 );

  well->i = OFF( 15 );
  C_return( ((double) well->state[OFF( 0 )]) * FACT );

# undef FACT

# undef V0
# undef VM1
# undef VM2
# undef VM3
# undef VRm1
# undef VRm2
# undef newV0
# undef newV1
# undef newVRm1

# undef MAT0POS
# undef MAT0NEG
# undef MAT3NEG
# undef MAT4NEG

# undef OFF

# undef W
# undef P
# undef M1
# undef M2
# undef M3
}

//#undef R

static void
init_state( unsigned int *i, uint32_t *state ) {
  uint32_t wstate[R];
  WELL512aState well;
  well.state = wstate;
  InitWELLRNG512a( &well );
  *i = well.i;
  memcpy( state, wstate, sizeof wstate );
}

static double
uniformf64( unsigned int *i, uint32_t *state )
{
  WELL512aState well;
  well.i = *i;
  well.state = state;
  double res = WELLRNG512a( well );
  *i = well.i;
  C_return( res );
}

static uint32_t
randomu32( unsigned int *i, uint32_t *state )
{
  C_return( (uint32_t) uniformf64( i, state ) );
}
<#

(define STATE-LENGTH (foreign-value "R" integer32))

(define init_state (foreign-lambda void "init_state" nonnull-u32vector unsigned-integer64))

(define well512-random-integer (foreign-lambda unsigned-integer32 "randomu32" nonnull-u32vector unsigned-integer32))
(define well512-random-real (foreign-lambda double "uniformf64" nonnull-u32vector))

;;;

;;

(define-constant maximum-unsigned-integer32-flonum 4294967295.0)

(cond-expand
  (64bit
    (define-constant maximum-unsigned-integer32 4294967295) )    ;MAX
  (else ;32bit
    (define-constant maximum-unsigned-integer32 1073741823) ) )  ;32bit most-positive-fixnum

;;

(define-constant fpMAX maximum-unsigned-integer32-flonum)  ;2^32 - 1
(define-constant LOG2-PERIOD 250)

(define eMAX (inexact->exact fpMAX)) ;Create a "bignum" if necessary

(define-constant INITIAL-SEED maximum-unsigned-integer32)

(define INTERNAL-ID 'well512)
(define EXTERNAL-ID 'well512)

;;

(define (make-state) (make-u32vector STATE-LENGTH))

(define (well512-initial-state)
  (let ((state (make-state)))
    (init_state state INITIAL-SEED)
    state ) )

(define (well512-unpack-state state)
  (cons EXTERNAL-ID (u32vector->list state)) )

(define (well512-pack-state external-state)
  (unless (well512-external-state? external-state)
    (error 'well512-pack-state "malformed state" external-state) )
  (let ((state (make-state)))
    (do ((i 0 (fx+ i 1))
         (ss (cdr external-state) (cdr ss)) )
        ((null? ss) state)
      (let ((x (car ss)))
        (if (and (integer? x) (<= 0 x 4294967295))
          (u32vector-set! state i x)
          (error 'well512-pack-state "illegal value" x) ) ) ) ) )

(define (well512-external-state? obj)
  (and
    (pair? obj)
    (eq? EXTERNAL-ID (car obj))
    (fx= (fx+ STATE-LENGTH 1) (length obj)) ) )

(define (well512-randomize-state state entropy-source)
  (init_state
    state
    (exact->inexact
      (modulo
        (fpabs (entropy-source-f64-integer entropy-source))
        fpMAX)))
  state )

(define (well512-pseudo-randomize-state i j)
  (let ((state (make-state)))
    (init_state state (exact->inexact (+ i j)))
    state ) )

(define (well512-random-large state n)
  (random-large-integer well512-random-integer state fpMAX eMAX n) )

(define (well512-random-real-mp state prec)
  (random-large-real well512-random-integer state fpMAX eMAX prec) )

;;;

(define (make-random-source-well512)
  (let ((state (well512-initial-state)))
    (*make-random-source
      ;
      make-random-source-well512
      ;
      EXTERNAL-ID
      ;
      "Well's 512-bit Generator"
      ;
      LOG2-PERIOD
      ;
      fpMAX
      ;
      #f
      ;
      (lambda ()
        (well512-unpack-state state) )
      ;
      (lambda (new-state)
        (set! state (well512-pack-state new-state)) )
      ;
      (lambda (entropy-source)
        (set! state (well512-randomize-state state entropy-source)) )
      ;
      (lambda (i j)
        (set! state (well512-pseudo-randomize-state i j)) )
      ;
      (lambda ()
        (lambda (n)
          (check-positive-integer INTERNAL-ID n 'range)
          (cond-expand
            (64bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (well512-random-integer state n))
                (else
                  (well512-random-large state n) ) ) )
            (else ;32bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (well512-random-integer state n))
                ;'n' maybe bignum - must be convertable to "unsigned-integer32"
                ((<= n eMAX)
                  (well512-random-integer state (exact->inexact n)))
                (else
                  (well512-random-large state n) ) ) ) ) ) )
      ;
      (lambda (prec)
        (cond
          ((native-real-precision? prec eMAX)
            (lambda ()
              (well512-random-real state) ) )
          (else
            (lambda ()
              (well512-random-real-mp state prec) ) ) ) ) ) ) )

;;;
;;; Module Init
;;;

(register-random-source! INTERNAL-ID make-random-source-well512)

) ;module well512
