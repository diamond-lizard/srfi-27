; Chicken

(import numbers)
(import srfi-27)
(import random-source)
(import mwc moa mrg32k3a #;bsdrnd)  ;load them all

;; Select platform specific entropy source

(cond-expand
  (windows
    (import entropy-windows)
    (current-entropy-source (make-entropy-source-crypt)) )
  (unix
    (import entropy-unix)
    (current-entropy-source (make-entropy-source-random-device)) )
  (else) )

(newline)
(print "CONFIDENCE TESTS FOR SRFI-27 \"Sources of Random Bits\"")
(print "Entropy Source: " (entropy-source-kind (current-entropy-source)))


; CONFIDENCE TESTS FOR SRFI-27 "Sources of Random Bits"
; =====================================================
;
; Sebastian.Egner@philips.com, 2002.
;
; This file contains a small collection of checks for the
; implementation of SRFI-27. It is not meant to be complete
; or to test the actual properties of the underlying generator.
; It is merely meant to run the code and to check some of the
; assumptions made by specification. There is an interface to
; G. Marsaglia's DIEHARD battery of tests for random number
; generators, though.
;
; running this file in Scheme 48 0.57 with SRFI-27 installed:
;
;   ,open srfi-27 srfi-23 ascii
;   ,load conftest.scm
;
; speed in Scheme 48 0.57 with SRFI-27 opened:
;
;   ,time (do ((k 0 (+ k 1))) ((= k 100000)) (random-integer 2))
;   ;about  30000/s on P3@800MHz for the Scheme-impl.
;   ;about 160000/s on P3@800MHz for the C/Scheme-impl.
;
;   ,time (do ((k 0 (+ k 1))) ((= k 100000)) (random-real))
;   ;about   3000/s on P3@800MHz for the Scheme-impl.
;   ;about 180000/s on P3@800MHz for the C/Scheme-impl.
;
; running this file in Gambit 3.0 (interpreter) with SRFI-27 loaded:
;
;   (define ascii->char integer->char)
;   (load "conftest.scm")
;
; compiling a simple program in Gambit 3.0 (compiler) using this file:
;
;   1. create 'a.scm' with content;
;     (include "srfi-27-c.scm")
;     (time (do ((k 0 (+ k 1))) ((= k 1000000)) (random-integer 2)))
;     (time (do ((k 0 (+ k 1))) ((= k 1000000)) (random-real)))
;   2. compile Scheme into C (verbose paths for clarity)
;     GAMBCDIR=$GAMBIT/lib LD_LIBRARY_PATH=$GAMBIT/lib $GAMBIT/gsc/gsc a
;   3. compile and link C code into an executable
;     gcc -I$GAMBIT/lib -o a a.c a_.c -lm -lgambc -L$GAMBIT/lib
;   4. run the executable
;     GAMBCDIR=$GAMBIT/lib LD_LIBRARY_PATH=$GAMBIT/lib a
;
; speed in Gambit 3.0 with SRFI-27 loaded:
;
;   (time (do ((k 0 (+ k 1))) ((= k 100000)) (random-integer 2)))
;   ;about 5000/s on P3@800MHz, interpreted
;   ;about 200000/s on P3@800MHz, compiled
;
;   (time (do ((k 0 (+ k 1))) ((= k 100000)) (random-real)))
;   ;about 25000/s on P3@800MHz, interpreted
;   ;about 400000/s on P3@800MHz, compiled
;
; history of this file:
;   SE, 19-Mar-2002: initial version, based on earlier tests
;   SE, 22-Mar-2002: adapted to new procedure names
;   SE, 25-Mar-2002: more descriptive output
;   SE, 04-Apr-2002: some quick timings; check up

; Basic Tests of the Interface
; ============================

(define (check-basics)

  (define rndint (random-source-make-integers (current-random-source)))
  (define rnd (random-source-make-reals (current-random-source)))

  (define (checked-random-integer n)
    (let ((x (rndint n)))
      (if (and (integer? x) (<= 0 x (- n 1))) x
          (begin
            (flush-output)
            (error 'checked-random-integer "(random-integer n) returned illegal value" x)))))

  (define (checked-random-real)
    (let ((x (rnd)))
      (if (< 0 x 1) x
          (begin
            (flush-output)
            (error 'checked-random-real "(random-real) returned illegal value" x)))))

  ;generate increasingly large numbers
  (do ((k 0 (fx+ k 1))
       (n 1 (* n 2)))
      ((fx> k 1024))
    (print* "generating large numbers: " k " bits") (display #\return)
    (checked-random-integer n))
  (print "generating large numbers: [1 1024] bits - ok")

  ;generate some reals
  (display "generating reals [1000 times]: ") (flush-output)
  (do ((k 0 (fx+ k 1)))
      ((fx= k 1000))
      (checked-random-real))
  (print "ok")

  ;get/set the state
  (if (not (random-source-state-ref (current-random-source)))
    (warning 'check-basics "lacks state support")
    (begin
      (display "get/set state: ") (flush-output)
      (let* ((state1 (random-source-state-ref (current-random-source)))
             (x1 (checked-random-integer (expt 2 32)))
             (state2 (random-source-state-ref (current-random-source)))
             (x2 (checked-random-integer (expt 2 32))))
        (random-source-state-set! (current-random-source) state1)
        (let ((y1 (checked-random-integer (expt 2 32))))
          (unless (= x1 y1)
            (error 'check-basics "state 1 get/set doesn't work" x1 y1 state1)))
        (random-source-state-set! (current-random-source) state2)
        (let ((y2 (checked-random-integer (expt 2 32))))
          (unless (= x2 y2)
            (error 'check-basics "state 2 get/set doesn't work" x2 y2 state2))))
      (print "ok") ) )

  ;randomize!
  (display "randomize!: ") (flush-output)
  (let* ((state1 (random-source-state-ref (current-random-source)))
         (x1 (checked-random-integer (expt 2 32))))
    (random-source-state-set! (current-random-source) state1)
    (random-source-randomize! (current-random-source))
    (let ((y1 (checked-random-integer (expt 2 32))))
      (when (= x1 y1)
        (error 'check-basics "random-source-randomize! didn't work" x1 state1))))
  (print "ok")

  ;pseudo-randomize!
  (display "pseudo-randomize!: ") (flush-output)
  (let* ((state1 (random-source-state-ref (current-random-source)))
         (x1 (checked-random-integer (expt 2 32))))
    (random-source-state-set! (current-random-source) state1)
    (random-source-pseudo-randomize! (current-random-source) 0 1)
    (let ((y1 (checked-random-integer (expt 2 32))))
      (when (= x1 y1)
        (error 'check-basics "random-source-pseudo-randomize! 0 1 didn't work" x1 y1 state1)))
    (random-source-state-set! (current-random-source) state1)
    (random-source-pseudo-randomize! (current-random-source) 1 0)
    (let ((y1 (checked-random-integer (expt 2 32))))
      (when (= x1 y1)
        (error 'check-basics "random-source-pseudo-randomize! 1 0 didn't work" x1 y1 state1))))
  (print "ok")

  (print "passed (check-basics)") )

(define (check-basics-of s)
  (time
    (newline)
    (current-random-source (make-random-source s))
    (print "Random Source: " (random-source-documentation (current-random-source)))
    (check-basics) ) )

;;; run some tests

(for-each check-basics-of (registered-random-sources))
