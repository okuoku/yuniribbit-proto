(library (ribbon util compiler)
  (export
    ;; Initialize
    ribbon-compiler-set-cache-handler!
    ribbon-compiler-add-libpath!
    ribbon-compiler-reset!
    ;; Activation (call this before any actual expansion)
    ribbon-compiler-activate!
    ;; Expand/Compile
    ribbon-compiler-compile-program
    ;; Query/Generate output
    ribbon-compiler-output-bundle)
  (import (yuni scheme)
          (yunife core)
          (yuni hashtables)
          (ribbon vmglue compiler)
          (yuniribbit rsc))

  (define ribbon-compiler-fe #f)

  ;; constants
  (define ribbon-compiler-corelibs
    '((yunivm-core-syntax)
      (rsc-core-syntax)
      (rsc-core-syntax/primitives)
      (rvm-primitives)))

  (define (ensure-ribbon-compiler-fe!)
    (unless ribbon-compiler-fe
      (set! ribbon-compiler-fe (make-yunife))
      (yunife-add-primitives! ribbon-compiler-fe
                              '(rsc-core-syntax/primitives)
                              (map (lambda (e) (cons e #f))
                                   '(let let* letrec define and or cond)))))

  (define (ribbon-compiler-set-cache-handler! proc)
    (ensure-ribbon-compiler-fe!)
    (yunife-register-cache-loader!  ribbon-compiler-fe proc))

  (define (ribbon-compiler-add-libpath! pth)
    (ensure-ribbon-compiler-fe!)
    (yunife-add-path! ribbon-compiler-fe pth))

  (define (ribbon-compiler-activate!)
    (ensure-ribbon-compiler-fe!)
    (yunife-loadlib! ribbon-compiler-fe '(rvm reader-runtime)))

  (define (ribbon-compiler-compile-program pth)
    (yunife-load! ribbon-compiler-fe pth))

  (define (ribbon-compiler-reset!)
    (set! ribbon-compiler-fe #f)
    (ensure-ribbon-compiler-fe!))

  (define (ribbon-compiler-output-bundle compiled? online?)
    (define (makesym str) (rib str str 2))
    (define emu-env? (not (rib? 'bogus)))
    (define fe ribbon-compiler-fe)
    (define loaded ribbon-compiler-corelibs)
    (define outseq '())
    (define progsym (yunife-get-libsym fe #t))
    (define readersym (yunife-get-libsym fe '(rvm reader-runtime)))

    ;; exports: libsym -> hashtable(global-sym -> non-interned-sym)
    (define ht-exports (and compiled? (make-symbol-hashtable)))

    (define (filter-macro libsym alist)
      (define (filter-defmacro src)
        (if (pair? src)
            (let ((nam (car src))
                  (frm (cdr src)))
              (unless (eq? nam 'define-macro)
                (error "unrecognized macro" src))
              (cons 'define frm))
            src))
      (define (filt1 mac)
        (let ((sym (car mac))
              (libname (cadr mac))
              (code+source (cddr mac)))
          (list libname sym
                (if code+source
                    (filter-defmacro (cdr code+source))
                    #f))))
      (let loop ((acc '())
                 (q alist))
        (if (null? q)
            acc
            (let ((m (filt1 (car q))))
             (if (eq? libsym (car m))
                 (loop (cons (cdr m) acc) (cdr q))
                 (loop acc (cdr q)))))))

    (define (code libsym) (yunife-get-library-code fe libsym))
    (define (macro libsym) (filter-macro libsym 
                                         (yunife-get-library-macro fe libsym)))
    (define (macname mac) (map car mac))
    (define (maccode mac) (map cadr mac))
    (define (addloaded! libsym)
      ;(write (list 'ADD: libname)) (newline)
      (set! loaded (append loaded (list libsym))))
    (define (loaded? libsym)
      (let loop ((x loaded))
       (and (pair? x)
            (or (eq? (car x) libsym)
                (loop (cdr x))))))

    (define (loadlib! libsym libname)
      (unless (loaded? libsym)
        ;(write (list 'REALIZE: libsym)) (newline)
        (process libsym libname)))

    (define (process libsym libname)
      (define precompiled? #f)
      (define exporting-macro? #f)
      (define (rename-noop x) x)
      (define (rename-prog x)
        (let ((r (hashtable-ref ht-liblocal x #f)))
         (cond
           (r
             (write (list 'RENAME: r libname)) (newline)
             r)
           (else
             ;; Allocate new local symbol
             (let ((me (makesym (symbol->string x))))
              (write (list 'LOCAL: me libname)) (newline)
              (hashtable-set! ht-liblocal x me)
              me)))))
      (define (comp?/prog seq)
        (if compiled?
            (list (compile-program seq (if online? rename-prog rename-noop)))
            seq))
      (define (comp?/macro seq)
        (if compiled?
            (list (compile-program seq rename-noop))
            seq))
      (define ht-liblocal (and online? (make-eq-hashtable)))
      (define ht-libexports (and online? (make-eq-hashtable)))
      (define (imp-fixup imports)
        (if (eq? #t libname)
            (append imports '((rvm reader-runtime)))
            imports))
      (when online?
        (when emu-env?
          (display "WARNING: disabled online mode because we're on emu\n")
          (set! online? #f))
        (unless compiled?
          (error "online mode requires on-the-fly compilation")))
      ;(write (list 'PROCESS: libsym)) (newline)
      (let* ((seq (code libsym))
             (mac (macro libsym))
             (macnames* (macname mac))
             (imports
               (imp-fixup (or (yunife-get-library-imports fe libsym) '())))
             (exports (yunife-get-library-exports fe libsym))
             (import* (map (lambda (libname) (yunife-get-libsym fe libname))
                           imports)))
        (for-each loadlib! import* imports)
        (unless (and (pair? seq) (pair? (car seq)))
          (set! precompiled? #t))
        (when online?
          ;; Import other library exports 
          (for-each (lambda (libsym)
                      (let ((x (hashtable-ref ht-exports libsym #f)))
                       (when x
                         (call-with-values (lambda () (hashtable-entries x))
                                           (lambda (k v)
                                             (vector-for-each
                                               (lambda (symg symv)
                                                 (hashtable-set! ht-liblocal
                                                                 symg
                                                                 symv))
                                               k v))))))
                    import*)
          ;; Add export fields
          (when exports
            (for-each (lambda (e)
                        (let ((x (hashtable-ref ht-liblocal e #f)))
                         (unless x
                           (cond
                             (precompiled?
                               (hashtable-set! ht-liblocal e e))
                             (else
                               (write (list 'MAKESYM: e (pair? seq))) (newline)
                               (let ((sym (makesym (symbol->string e))))
                                (hashtable-set! ht-liblocal e sym)
                                (hashtable-set! ht-libexports sym e)))))))
                      exports)))
        (set! outseq (cons (vector libname libsym import* imports exports 
                                   (comp?/prog seq)
                                   macnames* (comp?/macro (maccode mac)))
                           outseq))
        (when online?
          ;; Check the library has macro export
          (let* ((me (car outseq))
                 (macronames* (vector-ref me 6)))
            (for-each (lambda (m)
                        (let ((r (hashtable-ref ht-liblocal m #f)))
                         (when r
                           (when (hashtable-ref ht-libexports r #f)
                             (write (list 'EXPORT-MACRO: m libsym)) (newline)
                             (set! exporting-macro? #t)))))
                      macronames*))
          ;; Construct export table
          (when (and exports (not (null? exports)))
            (let ((ht-myex (make-symbol-hashtable)))
             ;; If the library has macro export, export everything
             (for-each (lambda (e)
                         (let ((r (hashtable-ref ht-liblocal e #f)))
                          (unless r
                            (error "Exported but not found" e libsym))
                          #|
                          (cond
                            ((eq? e r)
                             (write (list 'EXPORT: e libsym)) (newline))
                            (else
                              (write (list 'EXPORT-RENAMED: e r libsym))
                              (newline)))
                          |#
                          (hashtable-set! ht-myex e r))) 
                       (if (or exporting-macro? precompiled?)
                           (vector->list (hashtable-keys ht-liblocal))
                           exports))
             (hashtable-set! ht-exports libsym ht-myex))))

        (addloaded! libsym)))

    (process readersym '(rvm reader-runtime))
    (process progsym #t)
    (reverse outseq))

  )
