;;;; mrg32k3a.scm
;;;; Kon Lovett, Oct '09

;;; From "mrg32k3a.scm" & "mrg32k3a_c.scm"
;;; Sebastian.Egner@philips.com, 2002.
;;; Brad Lucier

;; Issues
;;
;; - Should use generic arithmetic since all computations are
;; within the 64bit fixnum range. But until u64vector available need to
;; use f64vectors makes any such gains very local.

(module mrg32k3a

(;export
  make-random-source-mrg32k3a)

(import (except scheme <= inexact->exact exact->inexact number?))
(import chicken foreign)

(use
  srfi-4
  (only numbers <= inexact->exact exact->inexact number?)
  (only type-errors error-positive-integer)
  random-source
  entropy-source
  fp-utils
  (only srfi-27-numbers
    check-positive-integer
    random-large-integer random-large-real
    native-real-precision?)
  #;crunch)

(declare
  (not usual-integrations
    <= inexact->exact exact->inexact))

;*** DEBUG ***
(use extras)

#>
#include <math.h>

/* limits */
#define M1 4294967087UL
#define M2 4294944443UL

/* recursion coefficients of the components */
#define A12  1403580UL
#define A13N  810728UL
#define A21   527612UL
#define A23N 1370589UL

static uint32_t
mrg32k3a_random_m1( double *state )
{
  int64_t y, x10, x20;

  /* component 1 */
  x10 = (((int64_t) A12 * (int64_t) state[ 1 ]) - ((int64_t) A13N * (int64_t) state[ 2 ])) % (int64_t) M1;
  if( x10 < 0 ) x10 += (int64_t) M1;
  state[ 2 ] = state[ 1 ];
  state[ 1 ] = state[ 0 ];
  state[ 0 ] = (double) x10;

  /* component 2 */
  x20 = (((int64_t) A21 * (int64_t) state[ 3 ]) - ((int64_t) A23N * (int64_t) state[ 5 ])) % (int64_t) M2;
  if( x20 < 0 ) x20 += (int64_t) M2;
  state[ 5 ] = state[ 4 ];
  state[ 4 ] = state[ 3 ];
  state[ 3 ] = (double) x20;

  /* combination of component */
  y = x10 - x20;
  if( y < 0 ) y += (int64_t) M1;
  return y;
}

static uint32_t
mrg32k3a_random_integer( double *state, uint32_t range )
{
  /* generate result in {0..range-1} using the rejection method */
  if( range > M1 ) /* This shouldn't happen */
    return mrg32k3a_random_m1( state );
  else {
    uint32_t q = M1 / range;
    uint32_t qn = q * range; /* M1 modulo range */
    uint32_t x;
    for( x = mrg32k3a_random_m1( state ); x >= qn; x = mrg32k3a_random_m1( state ) );
    return x / q;
  }
}

/* normalization factor */
#define NORM (1.0 / ((double) (M1 + 1)))

static double
mrg32k3a_random_real( double *state )
{
  return NORM * (1.0 + mrg32k3a_random_m1( state ));
}

#undef NORM
#undef A23N
#undef A21
#undef A13N
#undef A12
#undef M2
#undef M1
<#

;;; fp stuff

;;;
;;; mrg32k3a specific
;;;

(define mrg32k3a-random-integer (foreign-lambda unsigned-integer32 "mrg32k3a_random_integer" nonnull-f64vector unsigned-integer32))
(define mrg32k3a-random-real (foreign-lambda double "mrg32k3a_random_real" nonnull-f64vector))

;;;
;;; mrg32k3a generic
;;;

(define-constant maximum-unsigned-integer32-flonum 4294967295.0)

(cond-expand
  (64bit
    (define-constant maximum-unsigned-integer32 4294967087) )    ;M1
  (else ;32bit
    (define-constant maximum-unsigned-integer32 1073741823) ) )  ;32bit most-positive-fixnum

(define-constant fpM1 4294967087.0)   ;modulus of component 1
(define-constant fpM1-1 4294967086.0) ;M1 - 1

(define-constant fpM2 4294944443.0)   ;modulus of component 2
(define-constant fpM2-1 4294944442.0) ;M2 - 1

(define-constant fp2^28 268435456.0)
(define-constant fp2^16 65536.0)

(define eM1 (inexact->exact fpM1))    ;Create a "bignum" if necessary

(define-constant LOG2-PERIOD 191)

(define INTERNAL-ID 'mrg32k3a)
(define EXTERNAL-ID 'mrg32k3a)

;;

; GENERIC PART OF MRG32k3a-GENERATOR FOR SRFI-27
; ==============================================
;
; Sebastian.Egner@philips.com, 2002.
;
; This is the generic R5RS-part of the implementation of the MRG32k3a
; generator to be used in SRFI-27. It is based on a separate implementation
; of the core generator (presumably in native code) and on code to
; provide essential functionality not available in R5RS (see below).
;
; compliance:
;   Scheme R5RS with integer covering at least {-2^53..2^53-1}.
;   In addition,
;     SRFI-23: error
;
; history of this file:
;   SE, 22-Mar-2002: refactored from earlier versions
;   SE, 25-Mar-2002: pack/unpack need not allocate
;   SE, 27-Mar-2002: changed interface to core generator
;   SE, 10-Apr-2002: updated spec of mrg32k3a-random-integer

; Generator
; =========
;
; Pierre L'Ecuyer's MRG32k3a generator is a Combined Multiple Recursive
; Generator. It produces the sequence {(x[1,n] - x[2,n]) mod m1 : n}
; defined by the two recursive generators
;
;   x[1,n] = (               a12 x[1,n-2] + a13 x[1,n-3]) mod m1,
;   x[2,n] = (a21 x[2,n-1] +                a23 x[2,n-3]) mod m2,
;
; where the constants are
;   m1       = 4294967087 = 2^32 - 209    modulus of 1st component
;   m2       = 4294944443 = 2^32 - 22853  modulus of 2nd component
;   a12      =  1403580                   recursion coefficients
;   a13      =  -810728
;   a21      =   527612
;   a23      = -1370589
;
; The generator passes all tests of G. Marsaglia's Diehard testsuite.
; Its period is (m1^3 - 1)(m2^3 - 1)/2 which is nearly 2^191.
; L'Ecuyer reports: "This generator is well-behaved in all dimensions
; up to at least 45: ..." [with respect to the spectral test, SE].
;
; The period is maximal for all values of the seed as long as the
; state of both recursive generators is not entirely zero.
;
; As the successor state is a linear combination of previous
; states, it is possible to advance the generator by more than one
; iteration by applying a linear transformation. The following
; publication provides detailed information on how to do that:
;
;    [1] P. L'Ecuyer, R. Simard, E. J. Chen, W. D. Kelton:
;        An Object-Oriented Random-Number Package With Many Long
;        Streams and Substreams. 2001.
;        To appear in Operations Research.
;
; Arithmetics
; ===========
;
; The MRG32k3a generator produces values in {0..2^32-209-1}. All
; subexpressions of the actual generator fit into {-2^53..2^53-1}.
; The code below assumes that Scheme's "integer" covers this range.
; In addition, it is assumed that floating point literals can be
; read and there is some arithmetics with inexact numbers.
;
; However, for advancing the state of the generator by more than
; one step at a time, the full range {0..2^32-209-1} is needed.

; Accessing the State
; ===================

(define-constant STATE-LENGTH 6)

; should be u64vector but unsupported, as is 64-bit arithmetic w/ 32-bit cpu
(define (make-state) (make-f64vector STATE-LENGTH))

(define (mrg32k3a-initial-state)
  ;0 3 6 9 12 15 of A^16, see below
  (f64vector
   1062452522.0
   2961816100.0
    342112271.0
   2854655037.0
   3321940838.0
   3542344109.0) )

#; ;for debugging
(define (check-integer-state loc state)
  (let* ((ils (f64vector->list state))
         (els (map inexact->exact ils)) )
    (for-each
      (lambda (i e)
        (unless (fpinteger? (exact->inexact e))
          (error loc "non-integer state value"
            e (exact->inexact e) i
            ils) ) )
      ils els)
    state ) )

; Use large integers in external state to comply with
; the reference implementation
(define (mrg32k3a-unpack-state state)
  (cons EXTERNAL-ID (map inexact->exact (f64vector->list state))) )

(define mrg32k3a-pack-state
  (let ((state-M (list fpM1-1 fpM1-1 fpM1-1 fpM2-1 fpM2-1 fpM2-1)))
    (lambda (external-state)
      ;
      (define (checked-set! state n i m)
        (unless (number? n)
          (error 'mrg32k3a-pack-state "not a number" n) )
        (let ((x (exact->inexact n)))
          (unless (and (fpinteger? x) (fp<= 0.0 x) (fp<= x m))
            (error 'mrg32k3a-pack-state "illegal value" x n) )
          (f64vector-set! state i x) ) )
      ;
      (define (check-m-state a b c)
        ;XXX This is a waste since cannot overflow to zero & or have mixed signs.
        (when (fpzero? (fp+ a (fp+ b c)))
          (error 'mrg32k3a-pack-state "illegal degenerate state" external-state) ) )
      ;
      (unless (mrg32k3a-external-state? external-state)
          (error 'mrg32k3a-pack-state "malformed state" external-state) )
      ;
      (let ((state (make-state)))
        (do ((i 0 (fx+ i 1))
             (ss (cdr external-state) (cdr ss))
             (ms state-M (cdr ms)) )
            ((null? ss)
              (check-m-state
                (f64vector-ref state 0)
                (f64vector-ref state 1)
                (f64vector-ref state 2))
              (check-m-state
                (f64vector-ref state 3)
                (f64vector-ref state 4)
                (f64vector-ref state 5))
              state )
          (checked-set! state (car ss) i (car ms)) ) ) ) ) )

(define (mrg32k3a-external-state? obj)
  (and
    (pair? obj)
    (eq? EXTERNAL-ID (car obj))
    (fx= STATE-LENGTH (length (cdr obj))) ) )

; Pseudo-Randomization
; ====================
;
; Reference [1] above shows how to obtain many long streams and
; substream from the backbone generator.
;
; The idea is that the generator is a linear operation on the state.
; Hence, we can express this operation as a 3x3-matrix acting on the
; three most recent states. Raising the matrix to the k-th power, we
; obtain the operation to advance the state by k steps at once. The
; virtual streams and substreams are now simply parts of the entire
; periodic sequence (which has period around 2^191).
;
; For the implementation it is necessary to compute with matrices in
; the ring (Z/(m1*m1)*Z)^(3x3). By the Chinese-Remainder Theorem, this
; is isomorphic to ((Z/m1*Z) x (Z/m2*Z))^(3x3). We represent such a pair
; of matrices
;   [ [[x00 x01 x02],
;      [x10 x11 x12],
;      [x20 x21 x22]], mod m1
;     [[y00 y01 y02],
;      [y10 y11 y12],
;      [y20 y21 y22]]  mod m2]
; as a vector of length 18 of the integers as writen above:
;   #(x00 x01 x02 x10 x11 x12 x20 x21 x22
;     y00 y01 y02 y10 y11 y12 y20 y21 y22)
;
; As the implementation should only use the range {-2^53..2^53-1}, the
; fundamental operation (x*y) mod m, where x, y, m are nearly 2^32,
; is computed by breaking up x and y as x = x1*W + x0 and y = y1*W + y0
; where w = 2^16. In this case, all operations fit the range because
; w^2 mod m is a small number. If proper multiprecision integers are
; available this is not necessary, but pseudo-randomize! is expected
; to be called only occasionally so we do not provide this implementation.

#;
(crunch
  ;
  (define (ring-lc a b i0 i1 i2 j0 j1 j2 m w-sqr) ;linear combination
    (let ((fpW 65536.0)) ;wordsize to split {0..2^32-1}
      (let (
        (m::double m)
        (w-sqr::double w-sqr)
        (a0h (quotient (f64vector-ref a i0) fpW))
        (a0l (modulo (f64vector-ref a i0) fpW))
        (a1h (quotient (f64vector-ref a i1) fpW))
        (a1l (modulo (f64vector-ref a i1) fpW))
        (a2h (quotient (f64vector-ref a i2) fpW))
        (a2l (modulo (f64vector-ref a i2) fpW))
        (b0h (quotient (f64vector-ref b j0) fpW))
        (b0l (modulo (f64vector-ref b j0) fpW))
        (b1h (quotient (f64vector-ref b j1) fpW))
        (b1l (modulo (f64vector-ref b j1) fpW))
        (b2h (quotient (f64vector-ref b j2) fpW))
        (b2l (modulo (f64vector-ref b j2) fpW)) )
        (let (
          (comb
            (+
              (+ (* (+ (* a0h b0h) (+ (* a1h b1h) (* a2h b2h))) w-sqr)
                (* fpW
                  (+ (* a0h b0l)
                    (+ (* a0l b0h)
                      (+ (* a1h b1l)
                        (+ (* a1l b1h) (+ (* a2h b2l) (* a2l b2h))))))))
              (+ (* a0l b0l) (+ (* a1l b1l) (* a2l b2l))))) )
          ;
          (modulo comb m) ) ) ) )
#|
Undefined symbols for architecture x86_64:
  "vtable for __cxxabiv1::__class_type_info", referenced from:
      typeinfo for crunch_buffer<double> in mrg32k3a.o
  NOTE: a missing vtable usually means the first non-inline virtual member function has no definition.
  "vtable for __cxxabiv1::__si_class_type_info", referenced from:
      typeinfo for crunch_vector<double> in mrg32k3a.o
  NOTE: a missing vtable usually means the first non-inline virtual member function has no definition.
  "operator delete(void*)", referenced from:
      crunch_vector<double>::~crunch_vector() in mrg32k3a.o
      crunch_buffer<double>::~crunch_buffer() in mrg32k3a.o
  "___gxx_personality_v0", referenced from:
      f229(crunch_vector<double>, crunch_vector<double>, crunch_vector<double>) in mrg32k3a.o
      stub829(long, long, long, long) in mrg32k3a.o
      Dwarf Exception Unwind Info (__eh_frame) in mrg32k3a.o
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
|#
  #; ;
  (define (ring-product! v a b) ;A*B in ((Z/m1*Z) x (Z/m2*Z))^(3x3)
    (let (
      (fpM1 4294967087.0)   ;modulus of component 1
      (fpM2 4294944443.0)   ;modulus of component 2
      (fpW-SQR1 209.0)
      (fpW-SQR2 22853.0) )
      ;
      (f64vector-set! v 0 (ring-lc a b  0  1  2   0  3  6  fpM1 fpW-SQR1)) ;(A*B)_00 mod m1
      (f64vector-set! v 1 (ring-lc a b  0  1  2   1  4  7  fpM1 fpW-SQR1)) ;(A*B)_01
      (f64vector-set! v 2 (ring-lc a b  0  1  2   2  5  8  fpM1 fpW-SQR1))
      (f64vector-set! v 3 (ring-lc a b  3  4  5   0  3  6  fpM1 fpW-SQR1)) ;(A*B)_10
      (f64vector-set! v 4 (ring-lc a b  3  4  5   1  4  7  fpM1 fpW-SQR1))
      (f64vector-set! v 5 (ring-lc a b  3  4  5   2  5  8  fpM1 fpW-SQR1))
      (f64vector-set! v 6 (ring-lc a b  6  7  8   0  3  6  fpM1 fpW-SQR1))
      (f64vector-set! v 7 (ring-lc a b  6  7  8   1  4  7  fpM1 fpW-SQR1))
      (f64vector-set! v 8 (ring-lc a b  6  7  8   2  5  8  fpM1 fpW-SQR1))
      (f64vector-set! v 9 (ring-lc a b  9 10 11   9 12 15  fpM2 fpW-SQR2)) ;(A*B)_00 mod m2
      (f64vector-set! v 10 (ring-lc a b  9 10 11  10 13 16  fpM2 fpW-SQR2))
      (f64vector-set! v 11 (ring-lc a b  9 10 11  11 14 17  fpM2 fpW-SQR2))
      (f64vector-set! v 12 (ring-lc a b 12 13 14   9 12 15  fpM2 fpW-SQR2))
      (f64vector-set! v 13 (ring-lc a b 12 13 14  10 13 16  fpM2 fpW-SQR2))
      (f64vector-set! v 14 (ring-lc a b 12 13 14  11 14 17  fpM2 fpW-SQR2))
      (f64vector-set! v 15 (ring-lc a b 15 16 17   9 12 15  fpM2 fpW-SQR2))
      (f64vector-set! v 16 (ring-lc a b 15 16 17  10 13 16  fpM2 fpW-SQR2))
      (f64vector-set! v 17 (ring-lc a b 15 16 17  11 14 17  fpM2 fpW-SQR2)) )
      ;
      (void) )
)

(define-constant fpW-SQR1 209.0)   ;w^2 mod m1
(define-constant fpW-SQR2 22853.0) ;w^2 mod m2

(define mrg32k3a-pseudo-randomize-state
  (let ((mrg32k3a-gen0 #f) (mrg32k3a-gen1 #f) (mrg32k3a-gen2 #f)
        (A^0
          '#f64(1.0 0.0 0.0
                0.0 1.0 0.0
                0.0 0.0 1.0
                1.0 0.0 0.0
                0.0 1.0 0.0
                0.0 0.0 1.0))
        (A            ;the MRG32k3a recursion
          '#f64(    0.0 1403580.0 4294156359.0
                    1.0       0.0          0.0
                    0.0       1.0          0.0
               527612.0       0.0 4293573854.0
                    1.0       0.0          0.0
                    0.0       1.0          0.0)) )
    (lambda (i j)
      ;
      (define (product a b) ;A*B in ((Z/m1*Z) x (Z/m2*Z))^(3x3)
        ;Yes, I know at toplevel
        (define-constant fpW      65536.0) ;wordsize to split {0..2^32-1}
        (define-constant fpW-SQR1 209.0)   ;w^2 mod m1
        (define-constant fpW-SQR2 22853.0) ;w^2 mod m2
        ;
        (define (lc i0 i1 i2 j0 j1 j2 m w-sqr) ;linear combination
          (let ((a0h (fpquotient  (f64vector-ref a i0) fpW))
                (a0l (fpmodulo (f64vector-ref a i0) fpW))
                (a1h (fpquotient  (f64vector-ref a i1) fpW))
                (a1l (fpmodulo (f64vector-ref a i1) fpW))
                (a2h (fpquotient  (f64vector-ref a i2) fpW))
                (a2l (fpmodulo (f64vector-ref a i2) fpW))
                (b0h (fpquotient  (f64vector-ref b j0) fpW))
                (b0l (fpmodulo (f64vector-ref b j0) fpW))
                (b1h (fpquotient  (f64vector-ref b j1) fpW))
                (b1l (fpmodulo (f64vector-ref b j1) fpW))
                (b2h (fpquotient  (f64vector-ref b j2) fpW))
                (b2l (fpmodulo (f64vector-ref b j2) fpW)))
            ;#{fp}{ modulo m}
            (fpmodulo
              (fp+
                (fp+ (fp* (fp+ (fp* a0h b0h)
                                  (fp+ (fp* a1h b1h)
                                       (fp* a2h b2h)))
                             w-sqr)
                        (fp* fpW
                             (fp+ (fp* a0h b0l)
                                  (fp+ (fp* a0l b0h)
                                       (fp+ (fp* a1h b1l)
                                            (fp+ (fp* a1l b1h)
                                                 (fp+ (fp* a2h b2l) (fp* a2l b2h))))))))
                   (fp+ (fp* a0l b0l) (fp+ (fp* a1l b1l) (fp* a2l b2l))))
              m) ) )
        ;
        (f64vector
          (lc  0  1  2   0  3  6  fpM1 fpW-SQR1) ;(A*B)_00 mod m1
          (lc  0  1  2   1  4  7  fpM1 fpW-SQR1) ;(A*B)_01
          (lc  0  1  2   2  5  8  fpM1 fpW-SQR1)
          (lc  3  4  5   0  3  6  fpM1 fpW-SQR1) ;(A*B)_10
          (lc  3  4  5   1  4  7  fpM1 fpW-SQR1)
          (lc  3  4  5   2  5  8  fpM1 fpW-SQR1)
          (lc  6  7  8   0  3  6  fpM1 fpW-SQR1)
          (lc  6  7  8   1  4  7  fpM1 fpW-SQR1)
          (lc  6  7  8   2  5  8  fpM1 fpW-SQR1)
          (lc  9 10 11   9 12 15  fpM2 fpW-SQR2) ;(A*B)_00 mod m2
          (lc  9 10 11  10 13 16  fpM2 fpW-SQR2)
          (lc  9 10 11  11 14 17  fpM2 fpW-SQR2)
          (lc 12 13 14   9 12 15  fpM2 fpW-SQR2)
          (lc 12 13 14  10 13 16  fpM2 fpW-SQR2)
          (lc 12 13 14  11 14 17  fpM2 fpW-SQR2)
          (lc 15 16 17   9 12 15  fpM2 fpW-SQR2)
          (lc 15 16 17  10 13 16  fpM2 fpW-SQR2)
          (lc 15 16 17  11 14 17  fpM2 fpW-SQR2)) )
      ;
      (define (power a e) ;A^e
        (cond
          ((fpzero? e)  A^0)
          ((fp= 1.0 e)  a)
          ((fpeven? e)  (power (product a a) (fpquotient e 2.0)))
          (else         (product (power a (fp- e 1.0)) a)) ) )
      ;precompute A^(2^127) and A^(2^76)
      ;note that A and A^0 are constant thru computation
      (unless mrg32k3a-gen0
        (letrec
            ((power-power  ;A^(2^b)
              (lambda (a b)
                (if (fpzero? b)
                  a
                  (power-power (product a a) (fp- b 1.0))))))
          (set! mrg32k3a-gen0 (power-power A 127.0))
          (set! mrg32k3a-gen1 (power-power A 76.0))
          (set! mrg32k3a-gen2 (power A 16.0)) ) )
      ;compute M = A^(16 + i*2^127 + j*2^76)
      (let (
        (M
          (product
            mrg32k3a-gen2
            (product
              (power mrg32k3a-gen0 (fpmodulo i fp2^28))
              (power mrg32k3a-gen1 (fpmodulo j fp2^28))))) )
        ;the new state
        (f64vector
          (f64vector-ref M 0)
          (f64vector-ref M 3)
          (f64vector-ref M 6)
          (f64vector-ref M 9)
          (f64vector-ref M 12)
          (f64vector-ref M 15)) ) ) ) )

; G. Marsaglia's simple 16-bit generator with carry
(define (make-gms16wc entropy-source)
  (let ((random-m
         (let ((x (fpmodulo ((@entropy-source-f64 entropy-source)) fp2^16)))
           (lambda ()
             (let ((y (fpmodulo x fp2^16)))
               (set! x (fp+ (fp* 30903.0 y) (fpquotient x fp2^16)))
               y ) ) ) ) )
    (lambda (n)
      ;m < n < m^2
      (fpmodulo (fp+ (fp* (random-m) fp2^16) (random-m)) n) ) ) )

; True Randomization
; ==================
;
; The value obtained from the system entropy is fed into a very
; simple pseudo random number generator. This in turn is used
; to obtain numbers to randomize the state of the MRG32k3a
; generator, avoiding period degeneration.

(define (mrg32k3a-randomize-state state entropy-source)
  ;; G. Marsaglia's simple 16-bit generator with carry
  (let ((random (make-gms16wc entropy-source)))
    ;the new state
    (f64vector
      (fp+ 1.0 (fpmodulo (fp+ (f64vector-ref state 0) (random fpM1-1)) fpM1-1))
      (fpmodulo (fp+ (f64vector-ref state 1) (random fpM1)) fpM1)
      (fpmodulo (fp+ (f64vector-ref state 2) (random fpM1)) fpM1)
      (fp+ 1.0 (fpmodulo (fp+ (f64vector-ref state 3) (random fpM2-1)) fpM2-1))
      (fpmodulo (fp+ (f64vector-ref state 4) (random fpM2)) fpM2)
      (fpmodulo (fp+ (f64vector-ref state 5) (random fpM2)) fpM2)) ) )

(define (mrg32k3a-random-large state n) ;n > m
  (random-large-integer mrg32k3a-random-integer state fpM1 eM1 n) )

(define (mrg32k3a-random-real-mp state prec)
  (random-large-real mrg32k3a-random-integer state fpM1 eM1 prec) )

;;;

(define (make-random-source-mrg32k3a)
  (let ((state (mrg32k3a-initial-state)))
    (*make-random-source
      ;
      make-random-source-mrg32k3a
      ;
      EXTERNAL-ID
      ;
      "Pierre L'Ecuyer's Combined Multiple Recursive Generator"
      ;
      LOG2-PERIOD
      ;
      fpM1
      ;
      #f
      ;
      (lambda ()
        (mrg32k3a-unpack-state state) )
      ;
      (lambda (new-state)
        (set! state (mrg32k3a-pack-state new-state)) )
      ;
      (lambda (entropy-source)
        (set! state (mrg32k3a-randomize-state state entropy-source)) )
      ;
      (lambda (i j)
        (set! state
          (mrg32k3a-pseudo-randomize-state (exact->inexact i) (exact->inexact j))) )
      ;
      (lambda ()
        (lambda (n)
          (check-positive-integer INTERNAL-ID n 'range)
          (cond-expand
            (64bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (mrg32k3a-random-integer state n))
                (else
                  (mrg32k3a-random-large state n) ) ) )
            (else ;32bit
              (cond
                ((and (fixnum? n) (<= n maximum-unsigned-integer32))
                  (mrg32k3a-random-integer state n))
                ;'n' maybe bignum - must be convertable to "unsigned-integer32"
                ((<= n eM1)
                  (mrg32k3a-random-integer state (exact->inexact n)))
                (else
                  (mrg32k3a-random-large state n) ) ) ) ) ) )
      ;
      (lambda (prec)
        (cond
          ((native-real-precision? prec eM1)
            (lambda ()
              (mrg32k3a-random-real state) ) )
          (else
            (lambda ()
              (mrg32k3a-random-real-mp state prec) ) ) ) ) ) ) )

;;;
;;; Module Init
;;;

(register-random-source! INTERNAL-ID make-random-source-mrg32k3a)

) ;module mrg32k3a
