;;;; random-source.scm
;;;; Kon Lovett, Oct '09

(module random-source

(;export
  *make-random-source
  random-source? check-random-source error-random-source
  *random-source-name
  *random-source-documentation
  *random-source-log2-period
  *random-source-maximum-range
  *random-source-entropy-source *random-source-entropy-source-set!
  @random-source-constructor
  @random-source-state-ref @random-source-state-set!
  @random-source-randomize! @random-source-pseudo-randomize!
  @random-source-make-integers @random-source-make-reals
  ;
  registered-random-sources
  registered-random-source
  unregister-random-source
  register-random-source!)

(import scheme chicken)

(use
  (only data-structures alist-ref alist-update!)
  (only srfi-1 alist-cons alist-delete!)
  (only type-checks define-check+error-type check-procedure check-symbol)
  source-registration)

#; ;NO YOU 'EFFING MORON
(use typed-define)

;;;

(include "srfi-27-common-types")

;;;

;;

#; ;NO YOU 'EFFING MORON
(define:-record-type random-source ...)

(: *make-random-source (procedure random-source-name string number number random-source-entropy-source procedure procedure procedure procedure procedure procedure --> random-source))
(: random-source? (* -> boolean : random-source))
(: @random-source-constructor (random-source --> procedure))
(: *random-source-name (random-source --> random-source-name))
(: *random-source-documentation (random-source --> string))
(: *random-source-log2-period (random-source --> number))
(: *random-source-maximum-range (random-source --> number))
(: *random-source-entropy-source (random-source --> random-source-entropy-source))
(: *random-source-entropy-source-set! (random-source random-source-entropy-source -> void))
(: @random-source-state-ref (random-source --> procedure))
(: @random-source-state-set! (random-source -> procedure))
(: @random-source-randomize! (random-source --> procedure))
(: @random-source-pseudo-randomize! (random-source --> procedure))
(: @random-source-make-integers (random-source --> procedure))
(: @random-source-make-reals (random-source --> procedure))
;
(define-record-type random-source
  (*make-random-source
    ctor
    name docu
    log2. maxrng
    es
    state-ref state-set!
    randomize! pseudo-randomize!
    make-integers make-reals)
  random-source?
  (ctor               @random-source-constructor)
  (name               *random-source-name)
  (docu               *random-source-documentation)
  (log2.              *random-source-log2-period)
  (maxrng             *random-source-maximum-range)
  (es                 *random-source-entropy-source *random-source-entropy-source-set!)
  (state-ref          @random-source-state-ref)
  (state-set!         @random-source-state-set!)
  (randomize!         @random-source-randomize!)
  (pseudo-randomize!  @random-source-pseudo-randomize!)
  (make-integers      @random-source-make-integers)
  (make-reals         @random-source-make-reals) )

(define-check+error-type random-source)

;; Random Source Constructor Registry

(define +reg+ (make-source-registration 'random-source '()))

(: registered-random-sources (--> list))
;
(define (registered-random-sources)
  ((@source-registration-key +reg+)) )

(: registered-random-source (random-source-name --> (or boolean random-source)))
;
(define (registered-random-source name)
  ((@source-registration-ref +reg+) name) )

(: unregister-random-source (random-source-name -> void))
;
(define (unregister-random-source name)
  ((@source-registration-deref! +reg+) name) )

(: register-random-source! (random-source-name procedure -> void))
;
(define (register-random-source! name ctor)
  ((@source-registration-register! +reg+) name ctor) )

) ;module random-source
