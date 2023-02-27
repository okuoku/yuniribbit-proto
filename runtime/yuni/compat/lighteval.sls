;; dummy
(library (yuni compat lighteval)
  (export eval/yuni)
  (import (yuni scheme))
  
  (define (eval/yuni expr)
    (%r7c-eval/yuni expr))
  )
