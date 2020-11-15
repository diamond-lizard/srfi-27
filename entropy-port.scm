;;;; entropy-port.scm
;;;; Kon Lovett, Oct '09

(module entropy-port

(;export
  ;
  entropy-port-lifetime
  ;
  make-entropy-source/port
  make-entropy-source/port-open make-entropy-source/port-open-timed
  make-entropy-source/file make-entropy-source/file-timed)

(import scheme)
(import (chicken base))
(import (chicken type))
(import (chicken gc))

(import
  (only type-checks check-input-port check-procedure check-symbol check-string check-number)
  (only type-errors warning-argument-type)
  entropy-source entropy-support
  timed-resource miscmacros)

;;;

(include "srfi-27-common-types")

;;;

(define (make-open-binary-input-file pn)
  (lambda ()
    (open-input-file pn #:binary)) )

;;;

;;

#; ;NOT YET
(define (*make-entropy-source/timed-resource tr-ctor u8gen f64gen name docu)
  (let ((tr (tr-ctor)))
    (*make-entropy-source
      ;
      (lambda () (*make-entropy-source/timed-resource tr-ctor u8gen f64gen name docu))
      ;
      name
      ;
      docu
      ;
      (lambda () (with-timed-resource tr u8gen))
      ;
      (lambda () (with-timed-resource tr f64gen))
      ;
      (lambda (u8cnt u8vec)
        (with-timed-resource tr (lambda (r) (entropic-u8vector-filled u8cnt u8vec (u8gen r)))))
      ;
      (lambda (f64cnt f64vec)
        (with-timed-resource tr (lambda (r) (entropic-f64vector-filled f64cnt f64vec (f64gen r))))) ) ) )

(define (*make-entropy-source/timed-port-resource tr-ctor name docu)
  (let ((tr (tr-ctor)))
    (*make-entropy-source
      ;
      (lambda () (*make-entropy-source/timed-port-resource tr-ctor name docu))
      ;
      name
      ;
      docu
      ;
      (lambda () (with-timed-resource tr port-entropic-u8))
      ;
      (lambda () (with-timed-resource tr port-entropic-f64))
      ;
      (lambda (u8cnt u8vec)
        (with-timed-resource tr (cut port-entropic-u8vector <> u8cnt u8vec)))
      ;
      (lambda (f64cnt f64vec)
        (with-timed-resource tr (cut port-entropic-f64vector <> f64cnt f64vec))) ) ) )

(define (*make-entropy-source/port-open-timed opener timeout name docu)
  (*make-entropy-source/timed-port-resource
    (lambda () (make-timed-resource opener close-input-port timeout name))
    name docu) )

(define (*make-entropy-source/port port name docu)
  (*make-entropy-source
    ;
    (lambda () (*make-entropy-source/port port name docu))
    ;
    name
    ;
    docu
    ;
    (lambda () (port-entropic-u8 port))
    ;
    (lambda () (port-entropic-f64 port))
    ;
    (lambda (u8cnt u8vec) (port-entropic-u8vector port u8cnt u8vec))
    ;
    (lambda (f64cnt f64vec) (port-entropic-f64vector port f64cnt f64vec))) )

(define (*make-entropy-source/port-open opener name docu)
  (let (
    (timeout (entropy-port-lifetime)) )
    (if timeout
      ;then auto-close on timeout
      (*make-entropy-source/port-open-timed opener timeout name docu)
      ;else keep open
      (let ((port (opener)))
        (set-finalizer! port close-input-port)
        (*make-entropy-source/port port name docu) ) ) ) )

;; Timeout Seconds

(define-constant DEFAULT-ENTROPY-PORT-CLOSE-SECONDS 60.0)

(: entropy-port-lifetime (#!optional (or boolean number) --> number))
;
(define-parameter entropy-port-lifetime DEFAULT-ENTROPY-PORT-CLOSE-SECONDS
  (lambda (x)
    (cond
      ((boolean? x)
        (and x DEFAULT-ENTROPY-PORT-CLOSE-SECONDS) )
      ((and (real? x) (positive? x))
        x )
      (else
        (warning-argument-type 'entropy-port-lifetime x 'seconds)
        (entropy-port-lifetime) ) ) ) )

;;; Entropy from some port

(: make-entropy-source/port (input-port #!optional entropy-source-name string --> entropy-source))
;
(define (make-entropy-source/port port
          #!optional
          (name (gensym 'port-))
          (docu "Entropy from an open port"))
  (*make-entropy-source/port
    (check-input-port 'make-entropy-source/port port)
    (check-symbol 'make-entropy-source/port name 'name)
    (check-string 'make-entropy-source/port docu 'documentation)) )

;;; Entropy from port, timed or fixed (parameterized by entropy-port-lifetime)

(: make-entropy-source/port-open (procedure #!optional entropy-source-name string --> entropy-source))
;
(define (make-entropy-source/port-open opener
          #!optional
          (name (gensym 'port-))
          (docu "Entropy from port"))
  (*make-entropy-source/port-open
    (check-procedure 'make-entropy-source/port-open opener 'open-procedure)
    (check-symbol 'make-entropy-source/port-open name 'name)
    (check-string 'make-entropy-source/port-open docu 'documentation)) )

;; Make TImed Port Entropy Source

(: make-entropy-source/port-open-timed (procedure number #!optional entropy-source-name string --> entropy-source))
;
(define (make-entropy-source/port-open-timed opener timeout
          #!optional
          (name (gensym 'timed-port-))
          (docu "Entropy from timed open port"))
  (*make-entropy-source/port-open-timed
    (check-procedure 'make-entropy-source/port-open-timed opener 'open-procedure)
    (check-number 'make-entropy-source/port-open-timed timeout 'timeout)
    (check-symbol 'make-entropy-source/port-open-timed name 'name)
    (check-string 'make-entropy-source/port-open-timed docu 'documentation)) )

;;

;binary mode by default (only at the moment)
(define make-entropy-open-file make-open-binary-input-file)

;; Entropy from some file (binary)

(: make-entropy-source/file (pathname #!optional entropy-source-name string --> entropy-source))
;
(define (make-entropy-source/file pn
          #!optional
          (name (gensym 'file-))
          (docu (string-append "Entropy from file \"" pn "\"")))
  (*make-entropy-source/port-open
    (make-entropy-open-file (check-string 'make-entropy-source/file pn 'filename))
    (check-symbol 'make-entropy-source/file name 'name)
    (check-string 'make-entropy-source/file docu 'documentation)) )

(: make-entropy-source/file-timed (pathname number #!optional entropy-source-name string --> entropy-source))
;
(define (make-entropy-source/file-timed pn timeout
          #!optional
          (name (gensym 'file-))
          (docu (string-append "Entropy from file \"" pn "\"")))
  (*make-entropy-source/port-open-timed
    (make-entropy-open-file (check-string 'make-entropy-source/file-timed pn 'filename))
    (check-number 'make-entropy-source/file-timed timeout 'timeout)
    (check-symbol 'make-entropy-source/file-timed name 'name)
    (check-string 'make-entropy-source/file-timed docu 'documentation)) )

) ;module entropy-port
