(library (r7c core error)
  (export error)
  (import (rsc-core-syntax)
          (rvm-primitives))

  (define (error . args)
    (car #f)
    (exit -1)))
