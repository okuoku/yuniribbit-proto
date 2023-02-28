(library (ribbon util libcache)
  (export make-libcache 
          libcache-lookup-macro
          libcache-lookup-global
          libcache-lookup-libinfo
          libcache-enter-bundle!)
  (import (yuni scheme)
          (yuni hashtables))


  (define (make-libcache interp lookup encoder)
    ;; Cache storage
    (define macroht (make-symbol-hashtable)) ;; sym => VM obj
    (define libinfoht (make-symbol-hashtable)) ;; sym => libinfo
    (define libcodeht (make-symbol-hashtable)) ;; sym => runnable

    ;; VM library
    (define encodevm (or encoder
                         (lambda (x) x)))


    (define (lookup-cached-libinfo sym)
      ;(write (list 'LOOKUP: sym '=> (hashtable-ref libinfoht sym #f))) (newline)
      (hashtable-ref libinfoht sym #f))

    (define (lookup-cached-macro name)
      (let ((obj (hashtable-ref macroht name #f)))
       (unless obj
         (error "Could not refer macro" name))
       obj))

    (define (lookup-cached-code name)
      (let ((obj (hashtable-ref libcodeht name #f)))
       (or obj 0)))

    (define (setup-fake-libinfo)
      (define fake (list '() '() '()))
      (hashtable-set! libinfoht 'rsc_45core_45syntax_47primitives
                      (encodevm fake)))

    ;; Library processing
    ;; #(libname libsym import* imports exports seq macname mac)

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

    (define (process arg obj)
      (cond
        ((= arg 0) ;; macro
         (lookup-cached-macro obj))
        ((= arg 1) ;; global
         (lookup-cached-code obj))
        ((= arg 2) ;; libinfo
         (lookup-cached-libinfo obj))
        ((= arg 3) ;; enter-bundle!
         (for-each cachebundle1 obj))
        (else
          (error "???"))))

    (setup-fake-libinfo)
    process)

  (define (libcache-lookup-macro cache sym)
    (cache 0 sym))

  (define (libcache-lookup-global cache sym)
    (cache 1 sym))

  (define (libcache-lookup-libinfo cache sym)
    (cache 2 sym))

  (define (libcache-enter-bundle! cache bundle)
    (cache 3 bundle))

  )
