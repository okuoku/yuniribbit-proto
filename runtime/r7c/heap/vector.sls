(library (r7c heap vector)
  (export vector? vector-length vector-ref vector-set!
          $make-vector
          $vector-fill! $vector-copy!)

  (import (rvm-primitives)
          (r7c heap listloop)
          (r7c heap pair)
          (rsc-core-syntax))

  ;; vector-type = 4

  (define (vector? v)
    (and (rib? v) (eqv? 4 (field2 v))))

  (define (require-vec v)
    (if (vector? v)
      #t
      (error "Vector required" v)))

  (define (vector-length v)
    (require-vec v)
    (vec-length v))

  (define (vector-ref v o)
    (require-vec v)
    (vec-ref v o))

  (define (vector-set! v o x)
    (require-vec v)
    (vec-set! v o x))

  (define ($make-vector len)
    (vec-new 4 len))

  (define ($vector-fill! v fill from to)
    (require-vec v)
    (vec-fill! v fill from to))

  (define ($vector-copy! dest pos src offs len)
    (require-vec dest)
    (require-vec src)
    (vec-copy! dest pos src offs len))

  )
