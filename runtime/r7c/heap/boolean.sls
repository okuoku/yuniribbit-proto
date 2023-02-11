(library (r7c heap boolean)
  (export not)
  (import (rvm-primitives)
          (rsc-core-syntax))

(define (not e) (eqv? e #f))
)
