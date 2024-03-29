(library (ribbon util interp)
  (export interp-reset!
          interp-reset!/bootstrap
          interp-set-libpath!
          interp-activate!
          interp-gen-bundle
          interp-gen-expanded
          interp-run
          )
  (import (yuni scheme)
          (ribbon vmglue vm)
          (ribbon util compiler))
  
  ;; FIXME: move this into separate library
  (define (interp-cache-handler libname sym cb)
    ;(write (list 'CACHELOADER libname sym)) (newline)
    ;; cb = ^[result imports exports code macro*]
    (let ((libinfo ($$lookup-cached-libinfo sym)))
      ;(write (list 'LOOKUP: sym (and libinfo #t))) (newline)
      (cond
        (libinfo
          ;(write (list 'LOOKUP: libinfo)) (newline)
          (let ((imports (car libinfo))
                (exports (cadr libinfo))
                (macname* (caddr libinfo)))
            (let ((mac* (map (lambda (name)
                               (let ((macro ($$lookup-cached-macro name)))
                                (cons name macro)))
                             macname*))
                  (code ($$lookup-cached-code sym)))
              (cb #t imports exports code mac*))))
        (else ;; not-found
          (cb #f #f #f #f #f)))))

  (define (interp-cache-handler/empty libname sym cb)
    (cb #f #f #f #f #f))

  (define (interp-reset!/bootstrap)
    (ribbon-compiler-reset!)
    (ribbon-compiler-set-cache-handler! interp-cache-handler/empty))

  (define (interp-reset!)
    (ribbon-compiler-reset!)
    (ribbon-compiler-set-cache-handler! interp-cache-handler))

  (define (interp-set-libpath! libpath*)
    (for-each ribbon-compiler-add-libpath! libpath*))

  (define (interp-activate!)
    (ribbon-compiler-activate!))
  
  (define (interp-gen-bundle online? path)
    (ribbon-compiler-compile-program path)
    (ribbon-compiler-output-bundle #t online?))

  (define (interp-gen-expanded path)
    (ribbon-compiler-compile-program path)
    (ribbon-compiler-output-bundle #f #f))
  
  (define (interp-run bundle)
    (for-each (lambda (v)
                (let ((code (vector-ref v 5)))
                 (for-each $$runvm code)))
              bundle)
    (exit 0))
  
  )
