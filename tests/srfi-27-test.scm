;;;; srfi-27-test.scm
;;;; Kon Lovett, Dec '17

(import test)

;;;

;FIXME real tests

;;

(import srfi-27)

;;;

(print "*** Original Tests ***")

(import (chicken process))

(system* "csi -n -s test-mrg32k3a.scm")
(system* "csi -n -s test-confidence")
;(system* "csi -n -s test-diehard") ;errors

;;;

;;

(define-constant SRFI-27-TEST-TITLE "SRFI 27 Extensions")

(test-begin SRFI-27-TEST-TITLE)

;

(import (srfi 1) (srfi 4) (chicken string))

;

(import random-source entropy-source)

(test-group "basics entropy"
  (test-assert (entropy-source? (current-entropy-source)))
  (test-assert (->string (entropy-source-kind (current-entropy-source))) #t)
)

(test-group "basics random"
  (test-assert (random-source? default-random-source))
  (test-assert (random-source? (current-random-source)))
  (test-assert (->string (random-source-kind (current-random-source))) #t)
  (test-assert (procedure? random-integer))
  (test-assert (random-integer 5))
  (test-assert (procedure? random-real))
  (test-assert (random-real))
)

;

(test-group "SRFI-4 vector"

  (test-group "u8vector"
    ;(test-assert (procedure? random-u8vector))
    (let ((v10 (random-u8vector 10)))
      (test-assert (u8vector? v10))
      (test 10 (u8vector-length v10)) ) )

  (test-group "f64vector"
    ;(test-assert (procedure? random-f64vector))
    (let ((v10 (random-f64vector 10)))
      (test-assert (f64vector? v10))
      (test 10 (f64vector-length v10)) ) )
)

;

(import srfi-27-uniform-random)

(test-group "uniform-random"

  (test-group "integers"
    (let-values (
        ((gen init)
          (make-uniform-random-integers high: 27 low: 16 precision: 2)))
      (let-values (((high low precision source) (init)))
        (test-assert (= 27 high))
        (test-assert (= 16 low))
        (test-assert (= 2 precision))
        (do ((i 0 (add1 i))
             (rv (gen) (gen)) )
            ((= 100 i))
          (unless (<= 16 rv) (test-assert (<= 16 rv)))
          (unless (<= rv 27) (test-assert (<= rv 27)))
          (unless (zero? (modulo rv 2)) (test-assert (zero? (modulo rv 2)))) ) ) ) )

  ;FIXME needs real test
  (test-group "reals"
    (let-values (
        ((gen init)
          (make-uniform-random-reals precision: 0.000000000003)))
      (let-values (((precision source) (init)))
        (test-assert (= 0.000000000003 precision))
        ;(flonum-print-precision 53)
        (do ((i 0 (add1 i))
             (rv (gen) (gen)) )
            ((= 100 i))
            ) ) ) )
)

; Vectors

(import srfi-27-vector)

(define-constant VECTOR-LENGTH-LIMIT 10)
(define-constant VECTOR-EXAMPLES-LIMIT 3)

(define +known-vectors+  `(
  (,make-random-permutations ,integer? "permutations")
  (,make-random-vector ,real? "vector")
  (,make-random-hollow-sphere ,real? "hollow-sphere")
  (,make-random-solid-sphere ,real? "solid-sphere")
))

(test-group "vector"
  (for-each
    (lambda (vect-data)
      (let ((vect-ctor (car vect-data))
            (vect-pred (cadr vect-data))
            (vect-name (caddr vect-data)) )
        (test-group vect-name
          (let* ((ctor (vect-ctor))
                 (vec (ctor VECTOR-LENGTH-LIMIT)) )
            (test-assert "collection" (vector? vec))
            ;(test-assert "elements" (every vect-pred (vector->list vec)))
            (test "constructed length" VECTOR-LENGTH-LIMIT (vector-length vec))
            (do ((i 1 (add1 i)))
                ((> i VECTOR-EXAMPLES-LIMIT))
              (let ((res (vector-ref vec i)))
                (test-assert (->string res) (vect-pred res)) ) ) ) ) ) )
    +known-vectors+)
)

; Distributions

(import srfi-27-distributions)

(define-constant DISTRIBUTION-EXAMPLES-LIMIT 3)

(define +known-distributions+  `(
  (,make-random-normals ,real? "normals")
  (,make-random-exponentials ,real? "exponentials")
  (,make-random-triangles ,real? "triangles")
  (,make-random-poissons ,integer? "poissons")
  (,make-random-bernoullis ,boolean? "bernoullis")
  (,make-random-binomials ,integer? "binomials")
  (,make-random-geometrics ,integer? "geometrics")
  (,make-random-lognormals ,real? "lognormals")
  (,make-random-cauchys ,real? "cauchys")
  (,make-random-gammas ,real? "gammas")
  (,make-random-erlangs ,real? "erlangs")
  (,make-random-paretos ,real? "paretos")
  (,make-random-levys ,real? "levys")
  (,make-random-weibulls ,real? "weibulls")
))

(test-group "distributions"
  (for-each
    (lambda (distr-data)
      (let ((distr-ctor (car distr-data))
            (distr-pred (cadr distr-data))
            (distr-name (caddr distr-data)) )
        (test-group distr-name
          (receive (genny params) (distr-ctor)
            (test-assert "generator" (procedure? genny))
            (test-assert "parameters" (procedure? params))
            #; ;PLIST-STYLE (CLOJURE?)
            (with (param-list (call-with-values params list))
              (test-assert (->string param-list) (list? param-list)) )
            (let ((param-list (call-with-values params list)))
              (test-assert (->string param-list) (list? param-list)) )
            (do ((i 1 (add1 i)))
                ((> i DISTRIBUTION-EXAMPLES-LIMIT))
              (let ((res (genny)))
                (test-assert (->string res) (distr-pred res)) ) ) ) ) ) )
    +known-distributions+)
)

; Composite Entropy (experimental - at best)

(import composite-entropy-source)
(import entropy-clock)
(cond-expand
  (windows
    (import entropy-windows) )
  (unix
    (import entropy-unix) ) )

;FIXME use entropy name
(define-constant COMPOSITE-ENTROPY-TITLE
  (string-append
    "composite entropy : "
    (cond-expand
      (windows
        "crypt" )
      (unix
        (string-append "random-device" " + " "urandom-device")))))

(test-group COMPOSITE-ENTROPY-TITLE
  (let* ((ces-ctor
          (composite-entropy-source-constructor
            (make-entropy-source-system-clock)
            (cond-expand
              (windows
                (make-entropy-source-crypt) )
              (unix
                (make-entropy-source-random-device)
                (make-entropy-source-urandom-device) ) ) ) )
         (ces (ces-ctor) )
         (genu8 (entropy-source-u8 ces) )
         (genf64 (entropy-source-f64 ces) ) )
    (test-assert (integer? (genu8)))
    (test-assert (<= 0 (genu8)))
    (test-assert (<= (genu8) 255))
    (test-assert (flonum? (genf64)))
    (test-assert (u8vector? (entropy-source-u8vector ces 2)))
    (test-assert (= 2 (u8vector-length (entropy-source-u8vector ces 2))))
    (test-assert (f64vector? (entropy-source-f64vector ces 2)))
    (test-assert (= 2 (f64vector-length (entropy-source-f64vector ces 2))))
  )
)

; Composite Random (experimental - at best)

(import composite-random-source)
(import mwc mrg32k3a moa)

;FIXME use random name
(test-group "composite random : mwc + mrg32k3a + moa"
  (let* ((crs-ctor
          (composite-random-source-constructor
            (make-random-source-mwc)
            (make-random-source-mrg32k3a)
            (make-random-source-moa)) )
         (crs (crs-ctor) )
         (rndint (random-source-make-integers crs) )
         (rnd (random-source-make-reals crs) ) )
    (test-assert (procedure? rndint))
    (test-assert (procedure? rnd))
    (test-assert (integer? (rndint 10)))
    (test-assert (<= 0 (rndint 10)))
    (test-assert (<= (rndint 10) 10))
    (test-assert (inexact? (rnd)))
    (test-assert (random-source-randomize! crs))
    (test-assert (random-source-pseudo-randomize! crs 1 2))
  )
)

;;

(test-end SRFI-27-TEST-TITLE)

;;;

(test-exit)
