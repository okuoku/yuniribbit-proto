(library (r7c heap vector)
  (export list->vector
          vector->list)
  (import (rvm-primitives)
          (r7c heap listloop)
          (r7c heap pair)
          (rsc-core-syntax))

  ;; vector-type = 4
  (define (list->vector lis)
    (if (pair? lis)
        #t
        (error "List required"))
    (rib lis ($fx-length lis) 4))
  (define (vector->list v)
    (if (eqv? 4 (field2 v))
        #t
        (error "Vector required"))
    (field0 v))
  )
