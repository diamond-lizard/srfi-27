;;;; source-registration.scm
;;;; Kon Lovett, Feb '17
;;;; Kon Lovett, Oct '09

(module source-registration

(;export
  make-source-registration
  source-registration? check-source-registration error-source-registration
  @source-registration-key
  @source-registration-ref @source-registration-deref!
  @source-registration-register!)

(import scheme chicken)

(use
  (only data-structures alist-ref alist-update!)
  (only srfi-1 alist-delete!)
  (only type-checks define-check+error-type check-procedure check-symbol check-list))

;;;

(include "srfi-27-common-types")

;;; Utilities

(define (alist-keys alist)
  (map car alist) )

(define (alist-values alist)
  (map cdr alist) )

;;; Public

(: *make-source-registration (source-registration-name alist procedure procedure procedure procedure --> source-registration))
(: source-registration? (* -> boolean : source-registration))
(: *source-registration-name (source-registration --> source-registration-name))
(: *source-registration-sources (source-registration --> alist))
(: *source-registration-sources-set! (source-registration alist -> void))
(: @source-registration-key (source-registration --> procedure))
(: @source-registration-ref (source-registration --> procedure))
(: @source-registration-deref! (source-registration --> procedure))
(: @source-registration-register! (source-registration --> procedure))
;
(define-record-type source-registration
  (*make-source-registration nam srcs keys ref deref! reg!)
  source-registration?
  (nam    *source-registration-name)
  (srcs   *source-registration-sources *source-registration-sources-set!)
  (keys   @source-registration-key)
  (ref    @source-registration-ref)
  (deref! @source-registration-deref!)
  (reg!   @source-registration-register!) )

(define-check+error-type source-registration)

;;

(define (make-source-registration name sources)
  (letrec (
      (reg
        (*make-source-registration
          ;
          (check-symbol 'make-source-registration name "name")
          ;
          (check-list 'make-source-registration sources "sources")
          ;
          (lambda ()
            (alist-keys (*source-registration-sources reg)) )
          ;
          (lambda (name)
            (alist-ref
              (check-symbol 'source-registration-ref name)
              (*source-registration-sources reg)
              eq?
              #f) )
          ;
          (lambda (name)
            (*source-registration-sources-set!
              reg
              (alist-delete!
                (check-symbol 'source-registration-deref! name)
                (*source-registration-sources reg)
                eq?)) )
          ;
          (lambda (name ctor)
            (*source-registration-sources-set!
              reg
              (alist-update!
                (check-symbol 'source-registration-register!! name)
                (check-procedure 'source-registration-register! ctor)
                (*source-registration-sources reg)
                eq?)))) ) )
    reg ) )

) ;module source-registration
