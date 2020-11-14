;;;; entropy-support.scm
;;;; Kon Lovett, Oct '09

;; Issues
;;
;; - Could use ##core#inline forms for the 'double' operations
;;
;; - More could be coded in C, there is a lot of overhead
;;
;; - Assumes an entropy port will never hit EOF

(module entropy-support

(;export
  good_positive_double
  make-entropic-u8/f64
  entropic-u8vector-filled/f64
  entropic-u8vector-filled
  port-entropic-u8
  port-entropic-u8vector
  make-entropic-f64/u8
  entropic-f64vector-filled/u8
  entropic-f64vector-filled
  port-entropic-f64
  port-entropic-f64vector)

(import
  scheme
  (chicken base)
  (chicken foreign)
  (chicken type)
  (only (chicken io) read-byte)
  (only srfi-4
    u8vector-set! make-u8vector u8vector-length
    f64vector-set! make-f64vector f64vector-length
    read-u8vector read-u8vector!)
  (only (chicken memory) move-memory!)
  (only srfi-27-vector-support u8vector-filled! f64vector-filled!))

;;;

(include "srfi-27-common-types")

;;; Double stuff

#>
#include <math.h>
<#

#; ;w/ location
(define good_positive_double
  (foreign-lambda* bool ((u8vector u8vec) ((c-pointer double) d))
    ;Chicken SRFI 4 heap allocated vectors have 8-byte alignment!
    "double t = *((double *) u8vec);
    if (isnormal(t)) {
      *d = fabs(t);
      return 1;
    }
    C_return( 0 );"))

(: good_positive_double (u8vector --> float))
;
(define good_positive_double
  (foreign-lambda* double ((u8vector u8vec))
    ;Chicken SRFI 4 heap allocated vectors have 8-byte alignment!
    "double t = *((double *) u8vec);
    C_return( (isnormal( t ) ? fabs( t ) : -1.0) );"))

(define double_peek_byte
  (foreign-lambda* unsigned-byte ((double d) (int idx))
    "C_return( ((uint8_t *) &d)[idx] );"))

#;(define BYTES/F64 (foreign-value "sizeof( double )" int))
(define-constant BYTES/F64 8)

;; Entropy from procedure

(: make-entropic-u8/f64 (procedure --> procedure))
;
(define (make-entropic-u8/f64 f64gen)
  (let (
    (*idx* BYTES/F64)
    (*dbl* 0.0) )
    (lambda ()
      (if (fx= *idx* BYTES/F64)
        (begin
          (set! *dbl* (f64gen))
          (set! *idx* 0))
        (set! *idx* (fx+ *idx* 1)) )
      (double_peek_byte *dbl* *idx*) ) ) )

#; ;w/ location
(define (make-entropic-f64/u8 u8gen)
  (let ((*f64buf* (make-u8vector BYTES/F64)))
    (lambda ()
      (let-location ((tmpdbl double))
        (let loop ()
          (u8vector-filled! *f64buf* u8gen 0 BYTES/F64)
          (if (good_positive_double *f64buf* #$tmpdbl)
            tmpdbl
            (loop) ) ) ) ) ) )

(: make-entropic-f64/u8 (procedure --> procedure))
;
(define (make-entropic-f64/u8 u8gen)
  (let (
    (*f64buf* (make-u8vector BYTES/F64)) )
    (lambda ()
      (let loop ()
        (u8vector-filled! *f64buf* u8gen 0 BYTES/F64)
        (let ((tmpdbl (good_positive_double *f64buf*)))
          (if (fp= -1.0 tmpdbl)
            (loop)
            tmpdbl ) ) ) ) ) )

#; ;Slower but simpler
(define (entropic-u8vector-filled/f64 u8cnt u8vec f64gen)
  (let ((u8vec (or u8vec (make-u8vector u8cnt))))
    (u8vector-filled! u8vec (make-entropic-u8/f64 f64gen) 0 u8cnt)
    u8vec ) )

(: entropic-u8vector-filled/f64 (fixnum u8vector procedure -> u8vector))
;
(define (entropic-u8vector-filled/f64 u8cnt u8vec f64gen)
  (let* (
    (u8vec (or u8vec (make-u8vector u8cnt)))
    (f64cnt (fx/ u8cnt BYTES/F64))
    (f64vec (f64vector-filled! (make-f64vector f64cnt) f64gen))
    (u8rem (fxmod u8cnt BYTES/F64))
    (u8len (fx- u8cnt u8rem)) )
    (move-memory! f64vec u8vec u8len)               ;whole
    (when (fx< 0 u8rem)
      (let (
        ;remaining
        (u8gen (make-entropic-u8/f64 f64gen)) )
        (do ((idx u8len (fx+ idx 1)))
            ((fx>= idx u8cnt))
          (u8vector-set! u8vec idx (u8gen)) ) ) ) )
    u8vec )

(: entropic-f64vector-filled/u8 (fixnum f64vector procedure -> f64vector))
;
(define (entropic-f64vector-filled/u8 f64cnt f64vec u8gen)
  (let (
    (f64vec (or f64vec (make-f64vector f64cnt))) )
    (f64vector-filled! f64vec (make-entropic-f64/u8 u8gen) 0 f64cnt)
    f64vec ) )

(: entropic-u8vector-filled (fixnum u8vector procedure -> u8vector))
;
(define (entropic-u8vector-filled u8cnt u8vec u8gen)
  (let (
    (u8vec (or u8vec (make-u8vector u8cnt))) )
    (u8vector-filled! u8vec u8gen 0 u8cnt)
    u8vec ) )

(: entropic-f64vector-filled (fixnum f64vector procedure -> f64vector))
;
(define (entropic-f64vector-filled f64cnt f64vec f64gen)
  (let (
    (f64vec (or f64vec (make-f64vector f64cnt))) )
    (f64vector-filled! f64vec f64gen 0 f64cnt)
    f64vec ) )

;;; Entropy from port

(define port-entropic-u8 read-byte)

#; ;w/ location
(define port-entropic-f64
  (let ((*f64buf* (make-u8vector BYTES/F64)))
    (lambda (port)
      (let-location ((tmpdbl double))
        (let loop ()
          (let ((len (read-u8vector! BYTES/F64 *f64buf* port)))
            (cond
              ((< len BYTES/F64)
                0.0 )
              ((good_positive_double *f64buf* #$tmpdbl)
                tmpdbl )
              (else
                (loop) ) ) ) ) ) ) ) )

(: port-entropic-f64 (input-port --> procedure))
;
(define port-entropic-f64
  (let (
    (*f64buf* (make-u8vector BYTES/F64)) )
    (lambda (port)
      (let loop ()
        (let (
          (len (read-u8vector! BYTES/F64 *f64buf* port)) )
          (if (fx< len BYTES/F64)
            0.0
            (let ((tmpdbl (good_positive_double *f64buf*)))
              (if (fp= -1.0 tmpdbl)
                (loop)
                tmpdbl ) ) ) ) ) ) ) )

(: port-entropic-u8vector (input-port fixnum u8vector --> u8vector))
;
(define (port-entropic-u8vector port u8cnt u8vec)
  (if u8vec
    (begin
      (read-u8vector! u8cnt u8vec port)
      u8vec)
    (read-u8vector u8cnt port) ) )

(: port-entropic-f64vector (input-port fixnum f64vector #!optional procedure --> f64vector))
;
(define (port-entropic-f64vector port f64cnt f64vec #!optional (f64get port-entropic-f64))
  (entropic-f64vector-filled f64cnt f64vec (lambda () (f64get port))) )

) ;module entropy-support
