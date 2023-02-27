(library (ribbon util compiler)
  (export
    ;; Initialize
    ribbon-compiler-set-cache-handler!
    ribbon-compiler-add-libpath!
    ;; Activation (call this before any actual expansion)
    ribbon-compiler-activate!
    ;; Expand/Compile
    ribbon-compiler-compile-program
    ;; Query/Generate output
    ribbon-compiler-output-bundle)
  (import (yuni scheme)
          (yunife core)
          (yuni hashtables)
          (yuniribbit rsc))

  (define ribbon-compiler-fe #f)

  ;; constants
  (define ribbon-compiler-corelibs
    '((yunivm-core-syntax)
      (rsc-core-syntax)
      (rsc-core-syntax/primitives)
      (rvm-primitives)))

  ;; Compiled code cache
  (define ribbon-compiler-code-cache (make-symbol-hashtable))
  (define ribbon-compiler-macro-cache (make-symbol-hashtable))

  (define (ensure-ribbon-compiler-fe!)
    (unless ribbon-compiler-fe
      (set! ribbon-compiler-fe (make-yunife))
      (yunife-add-alias-map! ribbon-compiler-fe
                             'yunivm 'yunife-yunivm)
      (yunife-add-primitives! ribbon-compiler-fe
                              '(rsc-core-syntax/primitives)
                              (map (lambda (e) (cons e #f))
                                   '(let let* letrec define and or cond)))

      ))

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


  (define (ribbon-compiler-output-bundle compiled?)
    (define fe ribbon-compiler-fe)
    (define loaded ribbon-compiler-corelibs)
    (define outseq '())
    (define progsym (yunife-get-libsym fe #t))
    (define readersym (yunife-get-libsym fe '(rvm reader-runtime)))

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

    (define (comp? seq)
      (if compiled?
          (compile-program seq)
          seq))
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
      (define (imp-fixup imports)
        (if (eq? #t libname)
            (append imports '((rvm reader-runtime)))
            imports))
      ;(write (list 'PROCESS: libsym)) (newline)
      (let* ((seq (code libsym))
             (mac (macro libsym))
             (imports
               (imp-fixup (or (yunife-get-library-imports fe libsym) '())))
             (exports (yunife-get-library-exports fe libsym))
             (import* (map (lambda (libname) (yunife-get-libsym fe libname))
                           imports)))
        (for-each loadlib! import* imports)
        (set! outseq (cons (vector libname libsym import* imports exports 
                                   (comp? seq)
                                   (macname mac) (comp? (maccode mac)))
                           outseq))
        (addloaded! libsym)))

    (process readersym '(rvm reader-runtime))
    (process progsym #t)
    (reverse outseq))


  )
