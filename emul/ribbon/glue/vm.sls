(library (ribbon glue vm)
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
    set-interp!
    )

  (import (yuni scheme)
          (yuni hashtables)
          (yuniribbit heapcore)
          (yuni io drypack))

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

  (define (set-interp! runvm vmlookup)
    (set! lookup vmlookup)
    (set! interp runvm))

  (define ($$runvm/interp code)
    (setup-fake-libinfo)
    (interp code))

  ;; Runtime caching
  (define macroht (make-symbol-hashtable)) ;; sym => VM obj
  (define libinfoht (make-symbol-hashtable)) ;; sym => libinfo
  (define libcodeht (make-symbol-hashtable)) ;; sym => runnable

  ;; VM library
  (define (encodevm obj)
    (let ((p (open-output-bytevector)))
     (drypack-put p obj)
     (let ((bv (get-output-bytevector p)))
      (_wrap-bytevector bv))))

  (define (runvm/encoded arg)
    (unless (_bytevector? arg)
      (error "Invalid argument" arg))
    (let ((bv (_field0 arg)))
     (interp (decodehost bv))))

  (define (lookup-cached-libinfo/encoded arg)
    (unless (_symbol? arg)
      (error "Invalid argument" arg))
    (let ((sym (_field1 arg)))
     ;(write (list 'LOOKUP: sym '=> (hashtable-ref libinfoht sym #f))) (newline)
     (hashtable-ref libinfoht sym #f)))

  (define (lookup-cached-macro/encoded vals stack)
    (let ((arg (_car stack)))
     (unless (_symbol? arg)
       (error "invalid argument" arg))
     (let ((name (_field1 arg)))
      (let ((obj (hashtable-ref macroht name #f)))
       (unless obj
         (error "Could not refer macro" name))
       (_cons obj (_cdr stack))))))

  (define (lookup-cached-code/encoded arg)
    (unless (_symbol? arg)
      (error "invalid argument" arg))
    (let* ((name (_field1 arg))
           (obj (hashtable-ref libcodeht name #f)))
      (or obj 0)))

  (define (setup-fake-libinfo)
    (define fake (list '() '() '()))
    (hashtable-set! libinfoht 'rsc_45core_45syntax_47primitives 
                    (encodevm fake)))

  (define (vm-library)
    (vector
      (vector '$$runvm runvm/encoded 1 1)
      (vector '$$lookup-cached-libinfo lookup-cached-libinfo/encoded 1 1)
      (vector '$$lookup-cached-code lookup-cached-code/encoded 1 1)
      (vector '$$lookup-cached-macro lookup-cached-macro/encoded #f #f)))

  ;; Library processing
  ;; #(libname libsym import* imports exports seq macname mac)

  (define (putlib! ht v)
    ;; FIXME: Export everything for now
    ;;        Consider turn it anonymous where required...
    (let ((exports (vector->list (hashtable-keys ht))))
     (let ((alist (map (lambda (sym)
                         (cons sym
                               (hashtable-ref ht sym #f)))
                       exports))
           (libsym (vector-ref v 1)))
       ;(write (list 'EXPORTS: (map car alist))) (newline)
       (hashtable-set! libht libsym alist))))


  (define (cachebundle1 v)
    (when (vector-ref v 4)  ;; non-virtual lib?
      ;(write (list 'LOADING-MACRO: (vector-ref v 0))) (newline)
      (let ((globals (make-symbol-hashtable))
            (firstht (make-symbol-hashtable)))
        (cond
          ((eq? #t (vector-ref v 0))
           ;; program
           'done)
          (else
            ;; Register cached library info
            (let ((libsym (vector-ref v 1)))
             (hashtable-set! libcodeht libsym
                             (encodevm (vector-ref v 5)))
             (hashtable-set! libinfoht libsym
                             (encodevm
                               (list (vector-ref v 3) ;; imports
                                     (vector-ref v 4) ;; exports
                                     (vector-ref v 6) ;; macname
                                     ))))
            
            ;; Evaluate macro code
            (interp (vector-ref v 7))
            ;; Save VM closure object into host hashtable
            (for-each (lambda (sym)
                        ;(write (list 'MACADD: sym)) (newline)
                        (hashtable-set! macroht sym (lookup sym)))
                      (vector-ref v 6)))))))

  (define (cache-runtime! seq)
    ;; Scan over runtime and register for lookup table
    ;; ... will only evaluate macro transformers.
    (for-each cachebundle1 seq))

  )
