;;;; srfi-27-normals.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-normals

(;export
  *make-random-normals
  make-random-normals)

(import scheme chicken)

(import
  (only type-errors error-argument-type)
  (only type-checks
    define-check+error-type
    check-procedure
    check-cardinal-integer
    check-real
    check-open-interval
    check-closed-interval)
  srfi-27
  srfi-27-distributions-support)

;;;

(include "srfi-27-common-types")

;;; Normal distribution

;; Knuth's "The Art of Computer Programming", Vol. II, 2nd ed.,
;; Algorithm P of Section 3.4.1.C.

(: *make-random-normals (number number random-real-function --> number-function))
;
(define (*make-random-normals mu sigma randoms)
  (let ((next #f))
    (lambda ()
      (if next
        (let ((result next))
          (set! next #f)
          (+ mu (* sigma result)))
        (let loop ()
          (let* (
            (v1 (- (* 2.0 (randoms)) 1.0) )
            (v2 (- (* 2.0 (randoms)) 1.0) )
            (s (+ (* v1 v1) (* v2 v2)) ) )
            ;
            (if (<= 1.0 s)
              (loop)
              (let ((scale (sqrt (/ (* -2.0 (log s)) s))))
                (set! next (* scale v2))
                (+ mu (* sigma scale v1))))))))) )

(: make-random-normals (#!rest --> number-function procedure))
;
(define (make-random-normals #!key (mu 0.0) (sigma 1.0) (randoms (current-random-real)))
  (check-real 'make-random-normals mu 'mu)
  (check-nonzero-real 'make-random-normals sigma 'sigma)
  (check-procedure 'make-random-normals randoms 'randoms)
  (values
    (*make-random-normals mu sigma randoms)
    (lambda () (values mu sigma randoms))) )

) ;module srfi-27-normals
