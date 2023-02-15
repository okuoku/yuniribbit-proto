(library (r7c heap symbol)
  (export symbol? symbol->string string->symbol
          $symbol=?)
  (import (rvm-primitives)
          (rsc-core-syntax))

  ;; symbol-type = 2
  (define (symbol? x) (and (rib? x) (eqv? 2 (field2 x))))
  (define ($symbol=? x y) (eqv? x y))

  )
