;;;; srfi-27-vector-support.scm
;;;; Kon Lovett, Feb '10

;; Issues
;;
;; - More could be coded in C, there is a lot of overhead

; Chicken Generic Arithmetic!

(module srfi-27-vector-support

(;export
  vector-filled! u8vector-filled! f32vector-filled! f64vector-filled!
  f32vector-mapi!/1 f32vector-foldi/1
  f64vector-mapi!/1 f64vector-foldi/1
  ;
  check-vector%
  vector%-length
  vector%-mapi!/1
  vector%-foldi/1
  vector%-filled!
  vector%-scale!
  vector%-sum-squares)

(import
  scheme
  (chicken base)
  (chicken type)
  (chicken fixnum)
  (only srfi-4
    u8vector-length u8vector-ref u8vector-set!
    f32vector? f32vector-length f32vector-ref f32vector-set!
    f64vector? f64vector-length f64vector-ref f64vector-set!)
  (only vector-lib vector-map! vector-fold)
  (only type-errors error-vector))

;;;

(include "srfi-27-common-types")

;;;

;;

#;
(define (make-filled! veclenf vecsetf)
  (letrec (
    (self
      (case-lambda
        ((vec gen)
          (self vec gen 0) )
        ((vec gen start)
          (self vec gen start (veclenf vec)) )
        ((vec gen start end)
          (do ((idx start (fx+ idx 1)))
              ((fx= end idx) vec)
            (vecsetf vec idx (gen)) ) ) ) ) )
    self ) )

(define (make-filled! veclenf vecsetf)
  (lambda (vec gen #!optional (start 0) (end (veclenf vec)))
    (do ((idx start (fx+ idx 1)))
        ((fx= end idx) vec)
      (vecsetf vec idx (gen)) ) ) )

(define (make-mapi!/1 veclenf vecref vecsetf)
  (lambda (proc vec)
    (let ((len (veclenf vec)))
      (do ((i 0 (fx+ i 1)))
          ((fx= i len) vec)
        (vecsetf vec i (proc i (vecref vec i))) ) ) ) )

(define (make-foldi/1 veclenf vecref)
  (lambda (proc init vec)
    (let ((len (veclenf vec)))
      (do ((i 0 (fx+ i 1) )
           (acc init (proc i acc (vecref vec i)) ) )
          ((fx= i len) acc) ) ) ) )

;;

(define u8vector-filled!
  (make-filled! u8vector-length u8vector-set!) )

(define f64vector-filled!
  (make-filled! f64vector-length f64vector-set!) )

(define f32vector-filled!
  (make-filled! f32vector-length f32vector-set!) )

(define vector-filled!
  (make-filled! vector-length vector-set!) )

(define f32vector-mapi!/1
  (make-mapi!/1 f32vector-length f32vector-ref f32vector-set!) )

(define f64vector-mapi!/1
  (make-mapi!/1 f64vector-length f64vector-ref f64vector-set!) )

(define f32vector-foldi/1
  (make-foldi/1 f32vector-length f32vector-ref) )

(define f64vector-foldi/1
  (make-foldi/1 f64vector-length f64vector-ref) )

;;; Vector% Support

(define (vector-fold/1 vec proc seed)
  (vector-fold proc seed vec) )

(define (vector-map!/1 vec proc)
  (vector-map! proc vec) )

#; ;NOT YET
(define (array-rank/1? obj)
  (and (array? obj) (fx= 1 (array-rank obj))))

(define (acceptable-vector? obj)
  (or
    (vector? obj)
    (f32vector? obj)
    (f64vector? obj)
    #; ;NOT YET
    (array-rank/1? obj) ) )

(define (check-vector% loc obj #!optional argnam)
  (unless (acceptable-vector? obj)
    (error-vector loc obj argnam) )
  obj )

(define (make-oper vec-oper f32vec-oper f64vec-oper)
  (lambda (vec . args)
    (cond
      ((vector? vec)
        (apply vec-oper vec args) )
      ((f32vector? vec)
        (apply f32vec-oper vec args) )
      ((f64vector? vec)
        (apply f64vec-oper vec args) )
      #; ;NOT YET
      ((array-rank/1? vec)
        ;arr-rnk-1-oper
        (apply arr-rnk-1 vec args) )
      (else
        (error-vector #f vec)) ) ) )

;;

(: vector%-length (srfi-27-vector --> fixnum))
;
(define vector%-length
  (make-oper vector-length f32vector-length f64vector-length) )

(: vector%-mapi!/1 (srfi-27-vector procedure -> void))
;
(define vector%-mapi!/1
  ;(lambda (vec proc) (array-map! vec (cut proc #f <>)))
  (make-oper vector-map!/1 f32vector-mapi!/1 f64vector-mapi!/1) )

(: vector%-foldi/1 (srfi-27-vector procedure * --> *))
;
(define vector%-foldi/1
  ;(lambda (vec proc seed) (array-fold (cut proc #f <> <>) seed vec))
  (make-oper vector-fold/1 f32vector-foldi/1 f64vector-foldi/1) )

(: vector%-filled! (srfi-27-vector random-real-function #!optional fixnum fixnum -> void))
;
(define vector%-filled!
  ;(lambda (vec func) (array-fold (lambda (x y) (func)) #f vec))
  (make-oper vector-filled! f32vector-filled! f64vector-filled!) )

(: vector%--scale! (srfi-27-vector number -> void))
;
(define (vector%-scale! vec factor)
  (vector%-mapi!/1 vec (lambda (i elt) (* elt factor))) )

(: vector%-sum-squares (srfi-27-vector --> number))
;
(define (vector%-sum-squares vec)
  (vector%-foldi/1 vec (lambda (i sum elt) (+ sum (* elt elt))) 0) )

) ;module srfi-27-vector-support
