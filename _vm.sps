(import (yuni scheme)
        (yuni io drypack)
        (yuniribbit ext)
        (yuniribbit heapcore)
        (yuniribbit rvm)
        (yuni hashtables))

(define run #f)
(define args (command-line))

(define (consume-args)
  (when (pair? args)
    (let ((a (car args))
          (d (cdr args)))
      (cond
        ((string=? "-run" a)
         (unless (pair? d)
           (display "Error: -run requires a drypack file\n")
           (exit 1))
         (set! run (car d))
         (set! args (cdr d)))
        (else (set! args d))))
    (consume-args)))

(define libht (make-symbol-hashtable)) ;; libsym => alist
(define macroht (make-symbol-hashtable)) ;; sym => VM obj
(define libinfoht (make-symbol-hashtable)) ;; sym => libinfo
(define miniread #f)
(define evalyuni #f)
(define program-global #f)

;; VM library
(define (encodevm obj)
  (let ((p (open-output-bytevector)))
   (drypack-put p obj)
   (let ((bv (get-output-bytevector p)))
    (_wrap-bytevector bv))))

(define (lookup-cached-libinfo arg)
  (unless (_symbol? arg)
    (error "Invalid argument" arg))
  (let ((sym (_field0 arg)))
   ;(write (list 'LOOKUP: sym '=> (hashtable-ref libinfoht sym #f))) (newline)
   (encodevm (hashtable-ref libinfoht sym #f))))

(define (lookup-cached-macro vals stack)
  (let ((arg (_car stack)))
   (unless (_symbol? arg)
     (error "invalid argument" arg))
   (let ((name (_field0 arg)))
    (let ((obj (hashtable-ref macroht name #f)))
     (unless obj
       (error "Could not refer macro" name))
     (_cons obj (_cdr stack))))))

(define (runvm vals stack)
  (define output #f)
  (let ((arg (_car stack)))
   (unless (_bytevector? arg)
     (error "bytevector required" arg))
   (let* ((p (open-input-bytevector (_field0 arg)))
          (code (drypack-get p)))
     (rvm code
          program-global
          vm-library
          (lambda (mode x globals)
            (cond
              ((= mode 2)
               (set! output x))
              (else (exit x)))))))
  (_cons output (_cdr stack)))

(define (setup-fake-libinfo)
  (define fake (list '() '() '()))
  (hashtable-set! libinfoht 'rsc_45core_45syntax_47primitives fake)

  )

(define vm-library
  (vector
    (vector '$$runvm runvm #f #f)
    (vector '$$lookup-cached-libinfo lookup-cached-libinfo 1 1)
    (vector '$$lookup-cached-macro lookup-cached-macro #f #f)))

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
      (hashtable-set! libht libsym alist)
      (hashtable-set! libinfoht libsym
                      (list (vector-ref v 3) ;; imports
                            (vector-ref v 4) ;; exports
                            (vector-ref v 6) ;; macname
                            ))))
  ;; Special handling for %r7c-read/mini
  (let ((c (hashtable-ref ht '%r7c-read/mini #f)))
   (when c
     (write (list 'READ-CAPTURED: (vector-ref v 0))) (newline)
     (set! miniread c)))
  ;; Special handling for %r7c-eval/yuni
  (let ((c (hashtable-ref ht '%r7c-eval/yuni #f)))
   (when c
     (set! evalyuni c))))

(define (evalmacro! globals v)
  (let ((mac (vector-ref v 7)))
   (rvm mac
        globals
        vm-library
        (lambda (mode x globals)
          'ok))))

(define (putlibmacro! globals v)
  (for-each (lambda (sym)
              (let ((macobj (hashtable-ref globals sym #f)))
               (when (eq? none macobj)
                 (error "macro not found" (vector-ref v 0) sym))
               (hashtable-set! macroht sym macobj)))
            (vector-ref v 6)))

(define none (list 0))
(define (importlib! ht htdebug v)
  (when miniread
    ;; Inject reader
    (hashtable-set! ht 'read miniread)
    (hashtable-set! htdebug 'read 'NEVERLAND))
  (when evalyuni
    ;; Inject eval/yuni
    (hashtable-set! ht 'eval/yuni evalyuni)
    (hashtable-set! htdebug 'eval/yuni'NEVERLAND))
  (let ((imports (vector-ref v 2)))
   (for-each (lambda (libsym) 
               (for-each (lambda (e)
                           (let ((sym (car e))
                                 (obj (cdr e)))
                             (let ((prev (hashtable-ref ht sym none)))
                              (when (eq? none prev)
                                (hashtable-set! htdebug sym libsym))
                              (unless (eq? none prev)
                                (unless (equal? prev obj)
                                  (error "Unmatched export" libsym (cons (hashtable-ref htdebug sym #f) sym))))
                              ;(write (list 'IMPORT: sym 'FROM: libsym)) (newline)
                              (hashtable-set! ht sym obj))))
                         (hashtable-ref libht libsym '())))
             imports)))

(consume-args)

(unless run
  (display "No program speficied\n")
  (display "Specify input with -run <COMPILED.bin>\n")
  (exit 1))

(setup-fake-libinfo)

(call-with-port
  (open-binary-input-file run)
  (lambda (p)
    (let ((c (drypack-get p))
          (in-program? #f))
     (for-each (lambda (v)
                 (when (vector-ref v 4)  ;; non-virtual lib?
                   ;(write (list 'LOADING: (vector-ref v 0))) (newline)
                   (let ((globals (make-symbol-hashtable))
                         (firstht (make-symbol-hashtable)))
                    (importlib! globals firstht v)
                    (cond
                      ((eq? #t (vector-ref v 0))
                       ;; program
                       (set! in-program? #t)
                       (set! program-global globals))
                      (else
                        (set! in-program? #f)))
                    (rvm (vector-ref v 5)
                         globals
                         vm-library
                         (lambda (mode x globals)
                           (cond
                             (in-program?
                               (write (list 'EXIT: x)) (newline)
                               (exit x))
                             (else
                               ;(write (list 'LOADED: (vector-ref v 1))) (newline)
                               (putlib! globals v)
                               (evalmacro! globals v)
                               (putlibmacro! globals v))))))))
               c))))

(exit 1)
