(import (yuni scheme)
        (yuni io drypack)
        (yuniribbit ext)
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

;; Library processing
;; #(libname libsym imports exports seq mac)

(define (putlib! ht v)
  ;; FIXME: Export everything for now
  ;;        Consider turn it anonymous where required...
  (let ((exports (vector->list (hashtable-keys ht))))
    (let ((alist (map (lambda (sym)
                        (cons sym
                              (hashtable-ref ht sym #f)))
                      exports)))
      ;(write (list 'EXPORTS: (map car alist))) (newline)
      (hashtable-set! libht
                      (vector-ref v 1)
                      alist))))

(define none (list 0))
(define (importlib! ht htdebug v)
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

(call-with-port
  (open-binary-input-file run)
  (lambda (p)
    (let ((c (drypack-get p))
          (in-program? #f))
     (for-each (lambda (v)
                 (when (vector-ref v 3)  ;; non-virtual lib?
                   ;(write (list 'LOADING: (vector-ref v 0))) (newline)
                   (let ((globals (make-symbol-hashtable))
                         (firstht (make-symbol-hashtable)))
                    (importlib! globals firstht v)
                    (cond
                      ((eq? #t (vector-ref v 0))
                       ;; program
                       (set! in-program? #t))
                      (else
                        (set! in-program? #f)))
                    (rvm (vector-ref v 4)
                         globals
                         (ext-functions-vector)
                         (lambda (x globals)
                           (cond
                             (in-program?
                               (write (list 'EXIT: x)) (newline)
                               (exit x))
                             (else
                               ;(write (list 'LOADED: (vector-ref v 1))) (newline)
                               (putlib! globals v))))))))
               c))))

(exit 1)
