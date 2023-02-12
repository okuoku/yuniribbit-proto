(library (r7c heap boolean)
  (export not boolean?)
  (import (rvm-primitives)
          (rsc-core-syntax))

(define (not e) (eqv? e #f))
(define (boolean? b) (or (eqv? #t b) (eqv? #f b)))
)
