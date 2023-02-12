(library (r7c heap symbol)
  (export symbol? symbol->string string->symbol
          $symbol=?)
  (import (rvm-primitives)
          (rsc-core-syntax))

  ;; symbol-type = 2
  ;; string-type = 3
  (define (symbol? x) (eqv? 2 (field2 x)))
  (define (require-sym sym)
    (if (symbol? x)
        #t
        (error "Symbol required" x)))
  (define (symbol->string sym) 
    (require-sym sym)
    (rib (field2 sym) 0 3))

  (define ($symbol=? x y) (eqv? x y))

  )
