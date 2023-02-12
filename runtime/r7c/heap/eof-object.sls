(library (r7c heap eof-object)
  (export eof-object eof-object?)
  (import (rvm-primitives)
          (rsc-core-syntax))

  (define (eof-object) _eof-object)
  (define (eof-object? x) (eqv? _eof-object x))

  )
