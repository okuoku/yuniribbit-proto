(library (r7c heap bytevector)
  (export bytevector? bytevector-length bytevector-u8-ref bytevector-u8-set!
     $make-bytevector $bytevector-fill! $bytevector-copy!)
  (import (rvm-primitives) (rsc-core-syntax)
          (r7c core error)
          )

  ;; bytevector-type = 8
  
  (define (require-bv bv)
    (if (bytevector? bv)
        #t
        (error "Bytevector required")))

  (define (bytevector-length bv)
    (require-bv bv)
    (vec-length bv))

  (define (bytevector-u8-ref bv o)
    (require-bv bv)
    (vec-ref bv o))
  
  (define (bytevector-u8-set! bv o b)
    (require-bv bv)
    (vec-set! bv o b))

  (define ($make-bytevector len)
    (vec-new 8 len))
  
  (define ($bytevector-fill! bv fill from to)
    (require-bv bv)
    (vec-fill! bv fill from to))

  (define ($bytevector-copy! dest pos src offs len)
    (require-bv dest)
    (require-bv src)
    (vec-copy! dest pos src offs len))

  )
