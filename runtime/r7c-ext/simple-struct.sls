(library (r7c-ext simple-struct)
  (export $make-simple-struct
          simple-struct? simple-struct-ref simple-struct-set!
          simple-struct-name)
  (import (rvm-primitives) (rsc-core-syntax)
          (r7c core error)
          )

  (define (require-ss x)
    (if (simple-struct? x)
        #t
        (error "Simple-struct required" x)))
  
  (define (simple-struct-ref ss offs)
    (require-ss ss)
    (vec-ref ss offs))
  (define (simple-struct-set! ss offs obj)
    (require-ss ss)
    (vec-set! ss offs obj))
  (define (simple-struct-name ss)
    (require-ss ss)
    (vec-ref ss -1))
  (define ($make-simple-struct name len)
    (let ((ss (vec-new 9 len)))
     (vec-set! ss -1 name)
     ss)))
