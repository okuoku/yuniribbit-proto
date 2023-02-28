(library (ribbon vmglue vm)
  (export
    ;; For Ribbon runtime library
    $$lookup-cached-libinfo
    $$lookup-cached-code
    $$lookup-cached-macro
    $$runvm/interp
    ;; FIXME
    cache-runtime!
    ;; For emulator
    vm-library
    boot-library
    set-interp!
    )

  (import (yuni scheme)
          (yuniribbit heapcore)
          (yuni io drypack)
          (ribbon util libcache))

  (define (decodehost obj)
    (let ((p (open-input-bytevector obj)))
     (drypack-get p)))

  (define (encodehost obj)
    (let ((p (open-output-bytevector)))
     (drypack-put p obj)
     (let ((bv (get-output-bytevector p)))
      bv)))

  (define ($$lookup-cached-libinfo sym)
    (encodehost #f))
  (define ($$lookup-cached-macro name)
    (error "UNIMPL"))
  (define ($$lookup-cached-code sym)
    (error "UNIMPL"))

  (define interp #f)
  (define lookup #f)

  (define (call-interp obj) (interp obj))
  (define (call-lookup obj) (lookup obj))

  (define (set-interp! runvm vmlookup)
    (set! lookup vmlookup)
    (set! interp runvm))

  (define ($$runvm/interp code)
    (interp code))

  ;; VM library
  (define (encodevm obj)
    (let ((p (open-output-bytevector)))
     (drypack-put p obj)
     (let ((bv (get-output-bytevector p)))
      (_wrap-bytevector bv))))

  ;; Runtime caching
  (define rtcache (make-libcache call-interp call-lookup encodevm))

  (define (runvm/encoded arg)
    (unless (_bytevector? arg)
      (error "Invalid argument" arg))
    (let ((bv (_field0 arg)))
     (interp (decodehost bv))))

  (define (lookup-cached-libinfo/encoded arg)
    (unless (_symbol? arg)
      (error "Invalid argument" arg))
    (let ((sym (_field1 arg)))
     (libcache-lookup-libinfo rtcache sym)))

  (define (lookup-cached-macro/encoded arg)
    (unless (_symbol? arg)
      (error "invalid argument" arg))
    (let ((name (_field1 arg)))
     (libcache-lookup-macro rtcache name)))

  (define (lookup-cached-code/encoded arg)
    (unless (_symbol? arg)
      (error "invalid argument" arg))
    (let ((name (_field1 arg)))
     (libcache-lookup-global rtcache name)))

  (define (macro-runtime-mode/0 bogus) 0)
  (define (macro-runtime-mode/1 bogus) 1)

  (define (boot-library)
    (vector
      (vector '$$runvm runvm/encoded 1 1)
      (vector '$$macro-runtime-mode macro-runtime-mode/0 1 1)))

  (define (vm-library)
    (vector
      (vector '$$runvm runvm/encoded 1 1)
      (vector '$$macro-runtime-mode macro-runtime-mode/1 1 1)
      (vector '$$lookup-cached-libinfo lookup-cached-libinfo/encoded 1 1)
      (vector '$$lookup-cached-code lookup-cached-code/encoded 1 1)
      (vector '$$lookup-cached-macro lookup-cached-macro/encoded 1 1)))

  (define (cache-runtime! seq)
    ;; Scan over runtime and register for lookup table
    ;; ... will only evaluate macro transformers.
    (libcache-enter-bundle! rtcache seq))

  )
