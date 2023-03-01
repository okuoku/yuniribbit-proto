(import (yuni scheme)
        (yuni io drypack)
        (ribbon util interp))

(define libpath
  ;; FIXME: Hardcoded
  '("/home/oku/repos/yuniribbit-proto/runtime"
    "/home/oku/repos/yuniribbit-proto"
    "/home/oku/repos/yuni/external"
    "/home/oku/repos/yuni/lib"
    "/home/oku/repos/yuni/lib-compat"
    "/home/oku/repos/yuni/lib-r7c"
    ))

(define source "/home/oku/repos/yuni/tests/scheme/core0.sps")

(define (fake-runvm obj) 
  (write (list 'FAKE-RUNVM: '...)) (newline)
  #f)

(define (fake-lookup obj)
  (write (list 'FAKE-LOOKUP: obj)) (newline)
  #f)

(define (decodehost obj)
  (let ((p (open-input-bytevector obj)))
   (drypack-get p)))

(define (encodehost obj)
  (let ((p (open-output-bytevector)))
   (drypack-put p obj)
   (let ((bv (get-output-bytevector p)))
    bv)))

(define dummy #f)

(define ($$lookup-cached-libinfo obj) 
  (write (list 'CACHEQUERY: obj)) (newline)
  dummy)

;(set-interp! fake-runvm fake-lookup)

(interp-reset!)
(interp-set-libpath! (reverse libpath))
(interp-activate!)

(write (list 'STARTING...)) (newline)

(interp-gen-bundle source)

(write (list 'DONE.)) (newline)

