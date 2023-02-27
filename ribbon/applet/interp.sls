(library (ribbon applet interp)
  (export run-interp)
  (import (yuni scheme)
          (yuni io drypack)
          (ribbon glue vm)
          (ribbon applet compiler))

  
  ;; FIXME: move this into separate library
  (define (interp-cache-handler libname sym cb)
    ;(write (list 'CACHELOADER libname sym)) (newline)
    ;; cb = ^[result imports exports code macro*]
    (let ((libinfobv ($$lookup-cached-libinfo sym)))
     (let* ((p (open-input-bytevector libinfobv))
            (libinfo (drypack-get p)))
       (cond
         (libinfo
           ;(write (list 'LOOKUP: libinfo)) (newline)
           (let ((imports (car libinfo))
                 (exports (cadr libinfo))
                 (macname* (caddr libinfo)))
             (let ((mac* (map (lambda (name)
                                (cons name ($$lookup-cached-macro name)))
                              macname*))
                   (code ($$lookup-cached-code sym)))
               (cb #t imports exports code mac*))))
         (else ;; not-found
           (cb #f #f #f #f #f))))))

  
  (define (run-interp libpath* prog)
    (ribbon-compiler-set-cache-handler! interp-cache-handler)
    (for-each ribbon-compiler-add-libpath! libpath*)
    (ribbon-compiler-activate!)

    (ribbon-compiler-compile-program prog)

    (let ((seq (ribbon-compiler-output-bundle #t)))
     (cache-runtime! seq)
     (for-each (lambda (v)
                 (let ((code (vector-ref v 5)))
                  ($$runvm/interp code)))
               seq))
    (exit 0))
  
  )
