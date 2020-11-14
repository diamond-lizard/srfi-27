; Chicken

(use numbers) ;For rndstate = rndstate since large integers used
(use srfi-27 mrg32k3a)

;; Select platform specific entropy source

(cond-expand
  (windows
    (use entropy-windows)
    (current-entropy-source (make-entropy-source-crypt)) )
  (unix
    (use entropy-unix)
    (current-entropy-source (make-entropy-source-random-device)) )
  (else) )

(newline)
(print "Testing the MRG32k3a Generator")
(print "Entropy Source: " (entropy-source-kind (current-entropy-source)))
(newline)

; Testing the MRG32k3a Generator
; ==============================

; (check-mrg32k3a)
;   tests if the underlying generator MRG32k3a is the MRG32k3a generator
;   as implemented in the reference implementation. This function
;   is useful to check whether the reference implementation computes
;   the right numbers.

(define (check-mrg32k3a)

  ;check if the initial state is A^16 * (1 0 0 1 0 0)
  (display "check A^16 * (1 0 0 1 0 0): ") (flush-output)
  (let* ((s (make-random-source-mrg32k3a))
         (initial-state (random-source-state-ref s))
         (rand (random-source-make-reals s)))
    (random-source-state-set! s '(mrg32k3a 1 0 0 1 0 0))
    (do ((k 0 (fx+ k 1)))
        ((fx= k 16)
         (let ((state (random-source-state-ref s)))
           (unless (equal? initial-state state)
             (error 'check-mrg32k3a "16-th state after (1 0 0 1 0 0) is wrong" initial-state state))))
      (rand) ) )
  (print "ok") (newline)

  ;check if pseudo-randomize! advances properly
  (display "checking random-source-pseudo-randomize! in [1 2] advance: ") (flush-output)
  (let ((s (make-random-source-mrg32k3a)))
    (random-source-pseudo-randomize! s 1 2)
    (let ((initial-state (random-source-state-ref s)))
      (random-source-pseudo-randomize! s 1 2)
      (let ((state (random-source-state-ref s)))
        (unless (equal? state initial-state)
          (error 'check-mrg32k3a "pseudo-randomize! gives wrong result" initial-state state)))))
  (print "ok") (newline)

  ;run the check published by Pierre L'Ecuyer:
  ;  Note that the reference implementation deals slightly different
  ;  with reals mapping m1-1 into 1-1/(m1+1) and not into 0 as in
  ;  L'Ecuyer's original proposal. However, for the first 10^7 reals
  ;  that makes no difference as m1-1 is not generated.
  (print "(Please wait. This will take awhile! 10^7 reals are generated!)")
  (display "checking random-source-pseudo-randomize! in [1 2] summation: ") (flush-output)
  (let* ((x 0.0)
         (s (make-random-source-mrg32k3a))
         (rand (random-source-make-reals s)))
    (random-source-state-set! s '(mrg32k3a 12345 12345 12345 12345 12345 12345))
    (do ((k 0 (fx+ k 1)))
        ((fx= k 10000000)
         (unless (< (abs (- x 5001090.95)) 0.01)
           (error 'check-mrg32k3a "bad sum over 10^7 reals" x)))
      (set! x (+ x (rand))) ) )
  (print "ok") (newline) )

;;; run some tests

(check-mrg32k3a)
(print "passed (check-mrg32k3a)")
