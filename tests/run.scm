;;;; srfi-27 run.scm -*- Scheme -*-
;;;; Kon Lovett, Dec '17

(import scheme)

;;; Create Egg Const

(define EGG-NAME "srfi-27")

;chicken-install invokes as "<csi> -s run.scm <eggnam> <eggdir>"

(import (only (chicken pathname)
  make-pathname pathname-file pathname-replace-directory pathname-strip-extension))
(import (only (chicken process) system))
(import (only (chicken process-context) argv))
(import (only (chicken format) format))
(import (only (chicken file) file-exists? find-files))
(import (only (chicken irregex) irregex irregex-match?))

(define *args* (argv))

(define (egg-name args #!optional (def EGG-NAME))
  (cond
    ((<= 4 (length *args*)) (cadddr *args*) )
    (def                    def )
    (else
      (error 'run "cannot determine egg-name") ) ) )

(define *current-directory* (cond-expand (unix "./") (else #f)))
(define *egg* (egg-name *args*))

;no -disable-interrupts or -no-lambda-info
(define *csc-options* "-inline-global -local -inline \
  -specialize -optimize-leaf-routines -clustering -lfa2 \
  -no-trace -unsafe \
  -strict-types")

(define *test-files-rx* (irregex '(: (+ graph) #\- "test" #\. "scm")))
(define (test-filename name) (string-append name "-test"))
(define (test-files) (find-files "." #:test *test-files-rx* #:limit 1))

(define (ensure-test-source-name name)
  (if (irregex-match? *test-files-rx* name)
    name
    (make-pathname *current-directory* (test-filename name) "scm") ) )

(define (run-test-evaluated source)
  (format #t "*** ~A - csi ***~%" (pathname-file source))
  (system (string-append "csi -s " source)) )

(define (run-test-compiled source csc-options)
  (format #t "*** ~A - csc ~A ***~%" (pathname-file source) csc-options)
  ;csc output is in current directory
  (system (string-append "csc" " " csc-options " " source))
  (system (pathname-replace-directory (pathname-strip-extension source) *current-directory*)) )

;;;

(define (run-test #!optional (name *egg*) (csc-options *csc-options*))
  (let (
    (source (ensure-test-source-name name)) )
    (unless (file-exists? source)
      (error 'run "no such file" source) )
    (run-test-evaluated source)
    (newline)
    (run-test-compiled source csc-options) ) )

(define (run-tests #!optional (tests (test-files)) (csc-options *csc-options*))
  (for-each (cut run-test <> csc-options) tests) )

;;; Do Test

(run-tests)
