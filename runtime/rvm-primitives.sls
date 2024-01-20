(library (rvm-primitives)
  (export rib id arg1 arg2 close rib? 
          field0 field1 field2 field0-set! field1-set! field2-set!
          eqv?
          < + - * quotient
          getchar putchar
          exit
          values list->values
          char? char->integer integer->char

          ;; Ribbon(Scheme)
          vec-copy vec-copy! vec-ref vec-set! vec-new vec-length

          ;; Ribbon(NCCC)
          bv-ref/s8 bv-ref/u8
          bv-ref/s16 bv-ref/u16
          bv-ref/s32 bv-ref/u32
          bv-ref/s64 bv-ref/u64
          bv-set!/s8 bv-set!/u8
          bv-set!/s16 bv-set!/u16
          bv-set!/s32 bv-set!/u32
          bv-set!/s64 bv-set!/u64
          bv-ref/ptr0 bv-set!/ptr0
          address->bytevector bytevector->address
          nccc-call0 nccc-sizeof-ptr
          nccc-get-dispatch0

          ;; Ribbon(miniread)
          miniread-utf8-read)
  (import (rsc-core-syntax/primitives)))
