(library (r7c heap symbol)
  (export symbol? symbol->string string->symbol
          $symbol=?)
  (import (rvm-primitives)
          (rsc-core-syntax))

  (define ($symbol=? x y) (eqv? x y))

  )
