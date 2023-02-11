(library (r7c heap core)
  (export eq?)
  (import (rvm-primitives)
          (rsc-core-syntax))


  ;; FIXME: Need to implement it
  (define (eq? a b) (eqv? a b))
)
