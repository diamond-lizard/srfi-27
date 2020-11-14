; Chicken

(import files ports)
(import srfi-27)
(import mrg32k3a)
(import md5) ;for Diehard

;; Select platform specific entropy source

(cond-expand
  (windows
    (import entropy-windows)
    (current-entropy-source (make-entropy-source 'entropy-crypt)) )
  (unix
    (import entropy-unix)
    (current-entropy-source (make-entropy-source 'random-device)) )
  (else) )

(newline)
(print "Entropy Source: " (entropy-source-kind (current-entropy-source)))
(newline)

; Writing Data to DIEHARD
; =======================

; (write-diehard filename s bytes-per-call calls)
;    creates a binary file to which bytes-per-call * calls bytes are
;    written. The bytes are obtained from the random source s using
;    the range n = (expt 256 bytes-per-call).
;       The intention of write-diehard is to give implementors a
;    '15 min.'-way of running their favourite random number generator
;    through a pretty tough testsuite.
;
;    try: For the reference implementation, the call
;
;       (write-diehard "outfile" (make-random-source) 4 2867200)
;
;    should create a file that looks as follows (od -A x -t x1 outfile):
;
;       0000000 92 bb 7e db 1b 14 f6 bb bb 54 a1 55 c2 3e cd ca
;       0000010 23 01 20 35 06 47 65 b0 52 4c b8 c0 21 48 af 67
;       0000020 63 a9 8c 78 50 73 29 08 62 d1 22 7f a6 89 96 77
;       0000030 98 28 65 2d 2d 8b f9 52 41 be 8e 3f c5 84 0f ca
;       0000040 c0 fa 03 d6 f0 65 9d 3a 9b ab 6f fe d1 aa 5f 92
;       0000050 0f ea f6 3b 78 b9 fe ad 63 5e 49 f1 9d c9 8e 2f
;       0000060 53 a9 5d 32 d4 20 51 1d 1c 2e 82 f0 8b 26 40 c0
;       ...total length is 11468800 bytes.
;
;    The message digest is md5sum = 4df554f56cb5ed251bd04b0d50767443.
;
;    try: For the reference implementation, the call
;
;       (write-diehard "outfile" (make-random-source) 3 3822934)
;
;    should create a file that looks as follows (od -A x -t x1 outfile):
;
;       000000 bb 7e db 30 a3 49 14 f6 bb d0 f2 d0 54 a1 55 8b
;       000010 8c 03 3e cd ca a3 88 1d 01 20 35 e8 50 c8 47 65
;       000020 b0 e7 d9 28 4c b8 c0 f2 82 35 48 af 67 42 3e 8a
;       000030 a9 8c 78 12 ef b6 73 29 08 ff e9 71 d1 22 7f 52
;       000040 b8 f0 89 96 77 dc 71 86 28 65 2d c2 82 fc 8b f9
;       000050 52 d7 23 2a be 8e 3f 61 a8 99 84 0f ca 44 83 65
;       000060 fa 03 d6 c2 11 c0 65 9d 3a c2 7a dd ab 6f fe 1c
;       ...total length is 11468802 bytes.
;
;    The message digest is md5sum = 750ac219ff40c50bb2d04ff5eff9b24c.

(import md5 message-digest-bv)
(import (only utils read-all))

(define (md5-digest port)
  (message-digest-string md5-primitive (read-all port)) )

(define (write-diehard filename s bytes-per-call calls)
  (let ((port (open-output-file filename))
        (errprt (current-error-port)) )
    (with-output-to-port port
      (lambda ()
        (let ((rand (random-source-make-integers s))
              (n (expt 256 bytes-per-call)))
          (do ((i 0 (fx+ i 1)))
              ((fx= i calls))
            (when (fx= 0 (fxmod i 1000))
              (display i errprt) (display #\return errprt))
            (do ((x (rand n) (fx/ x 256))
                 (k bytes-per-call (fx- k 1)))
                ((fx= 0 k))
                (write-char (integer->char (fxmod x 256)))) ) )
          (newline errprt) )
        (close-output-port port)) ) )

(define (check-diehard s bytes-per-call calls mdexpt)
  (let ((diehard-filename (create-temporary-file "diehard")))
    (print "(diehard :randomsource " bytes-per-call " " calls ") ...")
    (write-diehard diehard-filename s bytes-per-call calls)
    (let ((port (open-input-file diehard-filename)))
      (let ((md (md5-digest port)))
        (close-input-port port)
        (if (equal? mdexpt md)
          (print "Ok")
          (print "Expected: " mdexpt " Received: " md))))))

(print "(Please wait. This will take a long while!)")
(check-diehard (make-random-source-mrg32k3a) 4 2867200 "4DF554F56CB5ED251BD04B0D50767443")
(check-diehard (make-random-source-mrg32k3a) 3 3822934 "750AC219FF40C50BB2D04FF5EFF9B24C")
