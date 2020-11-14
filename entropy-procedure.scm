;;;; entropy-procedure.scm
;;;; Kon Lovett, Oct '09

(module entropy-procedure

(;export
  make-entropy-source/procedures
  make-entropy-source/f64procedure)

(import scheme chicken)

(use
  (only type-checks check-procedure check-symbol check-string)
  entropy-source
  entropy-support)

;;;

(include "srfi-27-common-types")

;;; Entropy from some procedure

(: make-entropy-source/procedures (procedure procedure #!optional random-source-name string --> entropy-source))
;
(define (make-entropy-source/procedures u8proc f64proc
          #!optional
            (name (gensym 'procedures-))
            (docu "Entropy from procedures"))
  (check-procedure 'make-entropy-source/procedures u8proc 'u8proc)
  (check-procedure 'make-entropy-source/procedures f64proc 'f64proc)
  (check-symbol 'make-entropy-source/procedures name 'name)
  (check-string 'make-entropy-source/procedures docu 'documentation)
  (*make-entropy-source
    ;
    (lambda () (make-entropy-source/procedures u8proc f64proc name docu))
    ;
    name
    ;
    docu
    ;
    u8proc
    ;
    f64proc
    ;
    (lambda (u8cnt u8vec) (entropic-u8vector-filled u8cnt u8vec u8proc))
    ;
    (lambda (f64cnt f64vec) (entropic-f64vector-filled f64cnt f64vec f64proc))) )

(: make-entropy-source/f64procedure (procedure #!optional random-source-name string --> entropy-source))
;
(define (make-entropy-source/f64procedure f64proc . args)
  (apply make-entropy-source/procedures (make-entropic-u8/f64 f64proc) f64proc args) )

) ;module entropy-procedure
