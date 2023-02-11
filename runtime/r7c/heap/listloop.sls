(library (r7c heap listloop)
  (export memv $fx-length)
  (import (rvm-primitives)
          (r7c heap pair)
          (rsc-core-syntax))

  (define (memv x lis)
    (if (pair? x)
        (if (eqv? x (car lis))
            lis
            (memv x (cdr lis)))
        #f))

  (define (length-loop acc lis)
    (if (pair? lis)
        (length-loop (+ 1 acc) (cdr lis))))

  (define ($fx-length lis)
    (if (pair? lis)
        (length-loop 0 lis)
        (error "Pair required")))
  )
