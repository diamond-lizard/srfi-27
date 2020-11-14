;;;; entropy-source.scm
;;;; Kon Lovett, Oct '09

(module entropy-source

(;export
  *make-entropy-source
  entropy-source? check-entropy-source error-entropy-source
  *entropy-source-name
  *entropy-source-documentation
  @entropy-source-constructor
  @entropy-source-u8
  @entropy-source-f64
  @entropy-source-u8vector
  @entropy-source-f64vector
  ;
  entropy-source-integer
  entropy-source-f64-integer
  ;
  registered-entropy-sources
  registered-entropy-source
  unregister-entropy-source
  register-entropy-source!)

(import scheme)
(import (chicken base))
(import (chicken type))

(import
  (only srfi-1 alist-cons alist-delete!)
  (only type-checks define-check+error-type check-procedure check-symbol)
  source-registration)

;;;

(include "srfi-27-common-types")

;;;

;;

(: *make-entropy-source (procedure entropy-source-name string procedure procedure procedure procedure --> entropy-source))
(: entropy-source? (* -> boolean : entropy-source))
(: @entropy-source-constructor (entropy-source --> procedure))
(: *entropy-source-name (entropy-source --> entropy-source-name))
(: *entropy-source-documentation (entropy-source --> string))
(: @entropy-source-u8 (entropy-source --> procedure))
(: @entropy-source-f64 (entropy-source --> procedure))
(: @entropy-source-u8vector (entropy-source --> procedure))
(: @entropy-source-f64vector (entropy-source --> procedure))
;
(define-record-type entropy-source
  (*make-entropy-source ctor name docu u8 f64 u8vec f64vec)
  entropy-source?
  (ctor       @entropy-source-constructor)
  (name       *entropy-source-name)
  (docu       *entropy-source-documentation)
  (u8         @entropy-source-u8)
  (f64        @entropy-source-f64)
  (u8vec      @entropy-source-u8vector)
  (f64vec     @entropy-source-f64vector) )

(define-check+error-type entropy-source)

;;

(: entropy-source-integer (entropy-source --> number))
;
(define (entropy-source-integer es)
  ;ugly but ...
  (let (
    (genf64 (@entropy-source-f64 es)) )
    (let loop ((x (genf64)))
      (if (integer? x)
        x
        (loop (genf64)) ) ) ) )

(: entropy-source-f64-integer (entropy-source --> number))
;
(define entropy-source-f64-integer entropy-source-integer)

;; Entropy Source Constructor Registry

(define +reg+ (make-source-registration 'entropy-source '()))

(: registered-entropy-sources (--> list))
;
(define (registered-entropy-sources)
  ((@source-registration-key +reg+)) )

(: registered-entropy-source (entropy-source-name --> (or boolean random-source)))
;
(define (registered-entropy-source name)
  ((@source-registration-ref +reg+) name) )

(: unregister-entropy-source (entropy-source-name -> void))
;
(define (unregister-entropy-source name)
  ((@source-registration-deref! +reg+) name) )

(: register-entropy-source! (entropy-source-name procedure -> void))
;
(define (register-entropy-source! name ctor)
  ((@source-registration-register! +reg+) name ctor) )

) ;entropy-source
