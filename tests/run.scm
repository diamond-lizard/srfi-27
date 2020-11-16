;;;; srfi-27 run.scm
;;;; Kon Lovett, Dec '17

;chicken-install invokes as "<csi> -s run.scm <eggnam> <eggdir>"

(define *this-egg-name* "srfi-27")

(import (chicken file))
(import (chicken process-context))

;no -disable-interrupts
(define *csc-options* "-inline-global -scrutinize -optimize-leaf-routines -local -inline -specialize -unsafe -no-trace -no-lambda-info -clustering -lfa2")

(define *args* (argv))

(define (test-name #!optional (eggnam *this-egg-name*))
  (string-append eggnam "-test") )

(define (this-egg-name #!optional (def *this-egg-name*))
  (cond
    ((<= 4 (length *args*))
      (cadddr *args*) )
    (def
      def )
    (else
      (error 'test "cannot determine egg-name") ) ) )

;;;

(set! *this-egg-name* (this-egg-name))

(define (run-test #!optional (eggnam *this-egg-name*) (cscopts *csc-options*))
  (let ((tstnam (test-name eggnam)))
    (print "*** csi ***")
    (system (string-append "csi -s " (make-pathname #f tstnam "scm")))
    (newline)
    (print "*** csc (" cscopts ") ***")
    (system (string-append "csc" " " cscopts " " (make-pathname #f tstnam "scm")))
    (system (make-pathname (cond-expand (unix "./") (else #f)) tstnam)) ) )

;;;

(run-test)
