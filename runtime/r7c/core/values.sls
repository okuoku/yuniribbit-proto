(library (r7c core values)
  (export values call-with-values)
  (import (rvm-primitives))

  (define (call-with-values producer consumer)
    (let ((x (producer)))
     (apply-values consumer x)))

  )
