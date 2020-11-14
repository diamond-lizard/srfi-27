;;;; srfi-27.scm
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, Oct '09

(module srfi-27

(;export
  ;; SRFI 27
  default-random-source
  random-integer
  random-real
  make-random-source
  random-source? check-random-source error-random-source
  random-source-state-ref
  random-source-state-set!
  random-source-randomize!
  random-source-pseudo-randomize!
  random-source-make-integers
  random-source-make-reals
  ;
  ;; Extensions
  registered-random-sources registered-random-source ;re-export random-source
  current-random-source
  current-random-integer
  current-random-real
  new-random-source
  random-source-name random-source-kind
  random-source-documentation
  random-source-log2-period
  random-source-maximum-range
  random-source-entropy-source random-source-entropy-source-set!
  random-source-make-u8vectors
  random-source-make-f64vectors
  random-u8vector
  random-f64vector
  ;
  registered-entropy-sources registered-entropy-source ;re-export entropy-source
  current-entropy-source
  make-entropy-source new-entropy-source
  entropy-source? check-entropy-source error-entropy-source
  entropy-source-name entropy-source-kind
  entropy-source-documentation
  entropy-source-u8
  entropy-source-f64
  entropy-source-u8vector
  entropy-source-f64vector
  ;;
  random-integer/current
  random-real/current)

(import
  scheme
  (chicken base)
  (chicken type)
  (only srfi-4 make-u8vector make-f64vector)
  (only miscmacros define-parameter)
  (only type-errors error-argument-type warning-argument-type)
  (only srfi-27-numbers check-real-precision)
  (only srfi-27-vector-support u8vector-filled! f64vector-filled!)
  type-checks srfi-4-checks
  random-source mrg32k3a
  entropy-source entropy-clock)

;;;

(include "srfi-27-common-types")

;;;

;;; Entropy Source

;;

(: DEFAULT-ENTROPY-SOURCE entropy-source)
;
(define DEFAULT-ENTROPY-SOURCE (make-entropy-source-system-clock))

;;

(: current-entropy-source (#!optional entropy-source -> entropy-source))
;
(define-parameter current-entropy-source DEFAULT-ENTROPY-SOURCE
  (lambda (x)
    (cond
      ((entropy-source? x)
        x )
      (else
        (warning-argument-type 'current-entropy-source x 'entropy-source)
        (current-entropy-source) ) ) ) )

;;

(: make-entropy-source (#!optional entropy-source -> entropy-source))
;
(define (make-entropy-source #!optional (es (current-entropy-source)))
  (let (
    (ctor
      (cond
        ((entropy-source? es)
          (@entropy-source-constructor es) )
        ((symbol? es)
          (registered-entropy-source es) )
        (else
          #f ) ) ) )
    (unless ctor
      (error-argument-type
        'make-entropy-source es
        "valid entropy-source or registered entropy-source name") )
    (ctor) ) )

(: new-entropy-source (entropy-source --> entropy-source))
;
(define (new-entropy-source es)
  ((@entropy-source-constructor (check-entropy-source 'new-entropy-source es))) )

(: entropy-source-name (entropy-source --> entropy-source-name))
;
(define (entropy-source-name es)
  (*entropy-source-name (check-entropy-source 'entropy-source-name es)) )

(define entropy-source-kind entropy-source-name)

(: entropy-source-name (entropy-source --> symbol))
;
(define (entropy-source-documentation es)
  (*entropy-source-documentation
    (check-entropy-source 'entropy-source-documentation es)) )

(: entropy-source-u8vector (entropy-source fixnum #!optional u8vector -> u8vector))
;
(define (entropy-source-u8vector es n #!optional vec)
  ((@entropy-source-u8vector (check-entropy-source 'entropy-source-u8vector es))
    (check-positive-fixnum 'entropy-source-u8vector n)
    (and vec (check-u8vector 'entropy-source-u8vector vec))) )

(: entropy-source-f64vector (entropy-source fixnum #!optional f64vector -> f64vector))
;
(define (entropy-source-f64vector es n #!optional vec)
  ((@entropy-source-f64vector (check-entropy-source 'entropy-source-f64vector es))
    (check-positive-fixnum 'entropy-source-f64vector n)
    (and vec (check-f64vector 'entropy-source-f64vector vec))) )

(: entropy-source-u8 (entropy-source --> random-u8-function))
;
(define (entropy-source-u8 es)
  (@entropy-source-u8 (check-entropy-source 'entropy-source-u8 es)) )

(: entropy-source-f64 (entropy-source --> random-f64-function))
;
(define (entropy-source-f64 es)
  (@entropy-source-f64 (check-entropy-source 'entropy-source-f64 es)) )

;;; Random Source

(define (*random-source-make-u8vectors rs)
  (let (
    (*rnd* ((@random-source-make-integers rs))) )
    (lambda (n)
      (u8vector-filled!
        (make-u8vector (check-cardinal-integer 'random-source-make-u8vector n 'length))
        (lambda () (*rnd* 256))) ) ) )

(define (*random-source-make-f64vectors rs prec)
  (let (
    (*rnd* ((@random-source-make-reals rs) prec)) )
    (lambda (n)
      (f64vector-filled!
        (make-f64vector (check-cardinal-integer 'random-source-make-f64vector n 'length))
        *rnd*) ) ) )

;;

(: default-random-source random-source)
;
(define default-random-source (make-random-source-mrg32k3a))

(: random-integer random-integer-function)
;
(define random-integer ((@random-source-make-integers default-random-source)))

(: random-real random-real-function)
;
(define random-real ((@random-source-make-reals default-random-source) #f))

(: random-u8vector (fixnum -> u8vector))
;
(define random-u8vector
  (let (
    (*mkv* (*random-source-make-u8vectors default-random-source)) )
    (lambda (n) (*mkv* n) ) ) )

(: random-f64vector (fixnum -> f64vector))
;
(define random-f64vector
  (let (
    (*mkv* (*random-source-make-f64vectors default-random-source #f)) )
    (lambda (n) (*mkv* n) ) ) )

;;

(: current-random-source (#!optional random-source -> random-source))
;
(define-parameter current-random-source default-random-source
  (lambda (x)
    (cond
      ((random-source? x)
        x )
      (else
        (warning-argument-type 'current-random-source x 'random-source)
        (current-random-source) ) ) ) )

(: current-random-integer (-> fixnum))
;
(define (current-random-integer)
  ((@random-source-make-integers (current-random-source))) )

(: current-random-real (-> float))
;
(define (current-random-real)
  ((@random-source-make-reals (current-random-source)) #f) )

(: make-random-source (#!optional random-source -> random-source))
;
(define (make-random-source #!optional (rs (current-random-source)))
  (let (
    (ctor
      (cond
        ((random-source? rs)
          (@random-source-constructor rs) )
        ((symbol? rs)
          (registered-random-source rs) )
        (else
          #f ) ) ) )
    (unless ctor
      (error-argument-type
        'make-random-source rs
        "valid random-source or registered random-source name") )
    (ctor) ) )

(: new-random-source (#!optional random-source -> random-source))
;
(define (new-random-source #!optional (rs (current-random-source)))
  ((@random-source-constructor (check-random-source 'new-random-source rs))) )

(: random-source-name (random-source --> random-source-name))
;
(define (random-source-name rs)
  (*random-source-name
    (check-random-source 'random-source-name rs)) )

(define random-source-kind random-source-name)

(: random-source-documentation (random-source --> string))
;
(define (random-source-documentation rs)
  (*random-source-documentation
    (check-random-source 'random-source-documentation rs)) )

(: random-source-log2-period (random-source --> float))
;
(define (random-source-log2-period rs)
  (*random-source-log2-period
    (check-random-source 'random-source-log2-period rs)) )

(: random-source-maximum-range (random-source --> float))
;
(define (random-source-maximum-range rs)
  (*random-source-maximum-range
    (check-random-source 'random-source-maximum-range rs)) )

(: random-source-entropy-source (random-source --> random-source-entropy-source))
;
(define (random-source-entropy-source rs)
  (*random-source-entropy-source
    (check-random-source 'random-source-entropy-source rs)) )

(: random-source-entropy-source-set! (random-source (or boolean entropy-source) -> void))
;
(define (random-source-entropy-source-set! rs es)
  (*random-source-entropy-source-set!
    (check-random-source 'random-source-entropy-source-set! rs)
    ;#f indicates no set entropy-source
    (and es (check-entropy-source 'random-source-entropy-source-set! es))) )

(: random-source-state-ref (random-source -> random-source-state))
;
(define (random-source-state-ref rs)
  ((@random-source-state-ref (check-random-source 'random-source-state-ref rs))) )

(: random-source-state-set! (random-source random-source-state -> void))
;
(define (random-source-state-set! rs state)
  ((@random-source-state-set! (check-random-source 'random-source-state-set! rs))
    state) )

(: random-source-randomize! (random-source #!optional entropy-source -> void))
;
(define (random-source-randomize! rs #!optional es)
  ((@random-source-randomize! (check-random-source 'random-source-randomize! rs))
    (or
      (and es (check-entropy-source 'random-source-randomize! es))
      (*random-source-entropy-source rs)
      (current-entropy-source))) )

(: random-source-pseudo-randomize! (random-source fixnum fixnum -> void))
;
(define (random-source-pseudo-randomize! rs i j)
  ((@random-source-pseudo-randomize! (check-random-source 'random-source-pseudo-randomize! rs))
    (check-cardinal-integer 'random-source-pseudo-randomize! i)
    (check-cardinal-integer 'random-source-pseudo-randomize! j)) )

(: random-source-make-integers (random-source --> random-integer-function))
;
(define (random-source-make-integers rs)
  ((@random-source-make-integers (check-random-source 'random-source-make-integers rs))) )

(: random-source-make-reals (random-source #!optional fixnum --> random-real-function))
;
(define (random-source-make-reals rs #!optional prec)
  ((@random-source-make-reals (check-random-source 'random-source-make-reals rs))
    (and prec (check-real-precision 'random-source-make-reals prec 'precision))) )

(: random-source-make-u8vectors (random-source -> random-u8vector-function))
;
(define (random-source-make-u8vectors rs)
  (*random-source-make-u8vectors
    (check-random-source 'random-source-make-u8vectors rs)) )

(: random-source-make-u8vectors (random-source -> random-f64vector-function))
;
(define (random-source-make-f64vectors rs #!optional prec)
  (*random-source-make-f64vectors
    (check-random-source 'random-source-make-f64vectors rs)
    (and prec (check-real-precision 'random-source-make-f64vectors prec 'precision))) )

;;;

(: random-integer/current (deprecated current-random-integer))
(define random-integer/current current-random-integer)

(: random-real/current (deprecated current-random-real))
(define random-real/current current-random-real)

) ;module srfi-27
