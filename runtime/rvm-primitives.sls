(library (rvm-primitives)
  (export rib id arg1 arg2 close rib? 
          field0 field1 field2 field0-set! field1-set! field2-set!
          eqv?
          < + - * quotient
          getchar putchar
          exit
          values list->values
          char? char->integer integer->char

          vec-copy vec-copy! vec-ref vec-set! vec-new vec-length
          )
  (import (rsc-core-syntax/primitives)))
