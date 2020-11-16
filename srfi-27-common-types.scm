;;;; srfi-27-common-types.scm
;;;; Kon Lovett, Apr '18

;; Issues
;;
;; - cannot restrict number domain w/ type system

;;

(define-type alist list)

(define-type pathname string)

;;

(define-type bignum (struct bignum))
(define-type ratnum (struct ratnum))
(define-type compnum (struct compnum))

;;

(define-type entropy-source (struct entropy-source))

(define-type entropy-source-name symbol)

(define-type random-source (struct random-source))

(define-type random-source-name symbol)

;FIXME random-source-state is really type *
(define-type random-source-state list)

(define-type boolean-function (-> boolean))

(define-type number-function (-> number))

(define-type random-integer-function (number -> number))

(define-type random-f64-function  (-> float))

;fortran "real"
(define-type random-real-function random-f64-function)

(define-type random-u8-function  (-> fixnum))

(define-type random-u8vector-function (fixnum -> u8vector))

(define-type random-f64vector-function (fixnum -> f64vector))

(define-type random-source-entropy-source (or boolean entropy-source))

(define-type source-registration (struct source-registration))

(define-type source-registration-name (or entropy-source-name random-source-name))

(define-type srfi-27-vector (or vector f32vector f64vector))

(define-type random-vector-function (fixnum -> vector))
