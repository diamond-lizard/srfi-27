;;;; srfi-27-triangles.scm
;;;; Kon Lovett, Dec '17
;;;; Kon Lovett, Jun '17
;;;; Kon Lovett, May '06

; Chicken Generic Arithmetic! (would use fp routines, except for the "real" constraint)

(module srfi-27-triangles

(;export
  *make-random-triangles
  make-random-triangles)

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

;;; Triangle distribution

;; s - smallest, m - most probable, l - largest

(: *make-random-triangles (number number number random-real-function --> number-function))
;
(define (*make-random-triangles s m l randoms)
  (let (
    (d1 (- m s))
    (d2 (- l s))
    (d3 (sqrt (- l m))) )
    (let (
      (q1 (/ d1 d2) )
      (p1 (sqrt (* d1 d2)) ) )
      ;
      (lambda ()
        (let ((u (randoms)))
          (if (<= u q1)
            (+ s (* p1 (sqrt u)))
            (- l (* d3 (sqrt (- (* d2 u) d1))))))))) )

(: make-random-triangles (#!rest --> number-function procedure))
;
(define (make-random-triangles #!key (s 0.0) (m 0.5) (l 1.0) (randoms (current-random-real)))
  (check-real 'make-random-triangles s 's)
  (check-real 'make-random-triangles m 'm)
  (check-real 'make-random-triangles l 'l)
  (check-real-open-interval 'make-random-triangles l s +inf.0 'l)
  (check-real-closed-interval 'make-random-triangles m s l 'm)
  (check-procedure 'make-random-triangles randoms 'randoms)
  (values
    (*make-random-triangles s m l randoms)
    (lambda () (values s m l randoms))) )

) ;module srfi-27-triangles
