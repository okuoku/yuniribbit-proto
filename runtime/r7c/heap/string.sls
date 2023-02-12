(library (r7c heap string)
  (export string?)
  (import (rvm-primitives)
          (r7c heap listloop)
          (r7c heap pair)
          (rsc-core-syntax))

  ;; string-type = 2
  (define (string? str)
    (eqv? 2 (field2 str)))
  )
