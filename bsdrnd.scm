;;;; bsdrnd.scm
;;;; Kon Lovett, Mar 6, '17

;; Issues
;;
;; - No access to random state so ... ???

(module bsdrnd

(;export
  make-random-source-bsd)

(import scheme)
(import (chicken base))

(import (except scheme <= inexact->exact exact->inexact number?))

(import (chicken foreign))

(import
  (only srfi-27-numbers
    check-positive-integer
    random-large-integer random-large-real
    native-real-precision?))
(require-library
  srfi-27-numbers)

(import srfi-4)
(import random-source entropy-source)

(import (prefix random-bsd bsd::))
(require-library random-bsd)

;;;

;;

(define-constant 2^53 9007199254740992.0)
(define-constant maximum-unsigned-integer32-flonum 4294967295.0)

(cond-expand
  (64bit
    (define-constant maximum-unsigned-integer32 4294967295) )
  (else ;32bit
    (define-constant maximum-unsigned-integer32 1073741823) ) )

(cond-expand
  (64bit
    (define bsd-rand-max (- (expt 2 53) 1)) )
  (else ;32bit
    (define-constant bsd-rand-max 2147483647.0) ) )

(define-constant fpMAX maximum-unsigned-integer32-flonum)  ;2^32 - 1
(define-constant LOG2-PERIOD 250);FIXME

(define eMAX (inexact->exact fpMAX)) ;Create a "bignum" if necessary

(define-constant INITIAL-SEED maximum-unsigned-integer32-flonum)

(define-constant STATE-LENGTH 1)

(define INTERNAL-ID 'bsd)
(define EXTERNAL-ID 'bsd)

;;

(define (init_state state seed)
  (bsd::randomize seed)
  (state-seed-set! state seed)
  state )

(define (uniformu32_ith_state state i)
  (let ((seed (+ (state-seed state) i)))
    (bsd::randomize seed)
    (state-seed-set! state seed)
    state ) )

(define (uniformu32_jth_offset_state state j)
  (let ((seed (* (state-seed state) j)))
    (bsd::randomize seed)
    (state-seed-set! state seed)
    state ) )

(define-constant SEED_IDX 0)

(define (state-seed state)
  (u32vector-ref state SEED_IDX) )

(define (state-seed-set! state seed)
  (let ((old-seed (u32vector-ref state SEED_IDX)))
    (u32vector-set! state SEED_IDX seed)
    old-seed ) )

;;

(define (make-state) (make-u32vector STATE-LENGTH))

(define (bsd-initial-state)
  (let ((state (make-state)))
    (init_state state INITIAL-SEED)
    state ) )

(define (bsd-unpack-state state)
  (cons EXTERNAL-ID (u32vector->list state)) )

(define (bsd-pack-state external-state)
  (unless
    (and
      (pair? external-state)
      (eq? EXTERNAL-ID (car external-state))
      (fx= (fx+ STATE-LENGTH 1) (length external-state)))
    (error 'bsd-pack-state "malformed state" external-state) )
  (let ((state (make-state)))
    (do ((i 0 (fx+ 1 i))
         (ss (cdr external-state) (cdr ss)) )
        ((null? ss) state)
      (let ((x (car ss)))
        (if (and (integer? x) (<= 0 x maximum-unsigned-integer32))
          (u32vector-set! state i x)
          (error 'bsd-pack-state "illegal value" x) ) ) ) ) )

;; 64 bit entropy used as a bit source, not a number source!
(define (bsd-randomize-state state entropy-source)
  (init_state
    state
    (exact->inexact
      (modulo
        (fpabs (entropy-source-f64-integer entropy-source))
        bsd-rand-max)))
  state )

(define (bsd-pseudo-randomize-state i j)
  (let ((state (make-state)))
    (init_state state 0.0)
    (uniformu32_ith_state state i)
    (uniformu32_jth_offset_state state j)
    state ) )

(define (bsd-random-large state n)
  (random-large-integer
    (lambda (_ n) (bsd::random-integer n))
    state fpMAX eMAX n) )

(define (bsd-random-real-mp state prec)
  (random-large-real
    (lambda (_ n) (bsd::random-integer n))
    state fpMAX eMAX prec) )

;;;

(define (bsd-random-integer state n)
  (bsd::random-integer n) )

(define (make-random-source-bsd)
  (let ((state (bsd-initial-state)))
    (*make-random-source
      ;
      make-random-source-bsd
      ;
      INTERNAL-ID
      ;
      "BSD Generator (via random-bsd egg)"
      ;
      LOG2-PERIOD
      ;
      fpMAX
      ;
      #f
      ;
      (lambda ()
        #;(bsd-unpack-state state)
        #f )
      ;
      (lambda (new-state)
        #;(set! state (bsd-pack-state new-state))
        (void) )
      ;
      (lambda (entropy-source)
        (set! state (bsd-randomize-state state entropy-source)) )
      ;
      (lambda (i j)
        (set! state (bsd-pseudo-randomize-state i j)) )
      ;
      (lambda ()
        (lambda (n)
          (check-positive-integer INTERNAL-ID n 'range)
          (cond-expand
            (64bit
              (cond
                ((fixnum? n)
                  (bsd::random-integer n) )
                (else
                  (bsd-random-large state n) ) ) )
            (else ;32bit
              (cond
                ((fixnum? n)
                  (bsd::random-integer n) )
                ;'n' maybe bignum - must be convertable to "unsigned-integer32"
                ((<= n eMAX)
                  (bsd::random-integer (inexact->exact n)))
                (else
                  (bsd-random-large state n) ) ) ) ) ) )
      ;
      (lambda (prec)
        (cond
          ((native-real-precision? prec eMAX)
            (lambda ()
              (bsd::random-real) ) )
          (else
            (lambda ()
              (bsd-random-real-mp state prec) ) ) ) ) ) ) )

;;;
;;; Module Init
;;;

(register-random-source! INTERNAL-ID make-random-source-bsd)

) ;bsdrnd
