;;;; entropy-unix.scm
;;;; Kon Lovett, Oct '09

(module entropy-unix

(;export
  make-entropy-source-random-device
  make-entropy-source-urandom-device)

(import scheme chicken)

(import entropy-source entropy-port)

;;;

(include "srfi-27-common-types")

;;;

(define-constant TIMEOUT 5.0)

;;; Entropy from /dev/random

(: make-entropy-source-random-device (--> entropy-source))
;
(define (make-entropy-source-random-device)
  (make-entropy-source/file-timed
    "/dev/random"
    TIMEOUT
    'random-device
    "Entropy from random device") )

(register-entropy-source! 'random-device make-entropy-source-random-device)

;;; Entropy from /dev/urandom

(: make-entropy-source-urandom-device (--> entropy-source))
;
(define (make-entropy-source-urandom-device)
  (make-entropy-source/file-timed
    "/dev/urandom"
    TIMEOUT
    'urandom-device
    "Entropy from urandom device") )

(register-entropy-source! 'urandom-device make-entropy-source-urandom-device)

) ;module entropy-unix
