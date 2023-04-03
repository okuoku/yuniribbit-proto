(import (yuni scheme)
        (yuni io drypack)
        (yuniribbit ext)
        (yuniribbit heapcore)
        (yuniribbit rvm)
        (ribbon vmglue vm)
        (ribbon util interp)
        (ribbon util ribcode)
        (yuni hashtables))

(define libpath '())
(define source #f)
(define outbin #f)
(define bootstrapmode #f)
(define myself #f)
(define args (command-line))
(define restargs '())

(define (consume-args)
  (when (pair? args)
    (let ((a (car args))
          (d (cdr args)))
      ;(write (list 'ARG: a)) (newline)
      (cond
        ((string=? "-bootstrap" a)
         (unless (pair? d)
           (display "Error: -bootstrap requires source code\n")
           (exit 1))
         (set! source (car d))
         (set! args (cdr d))
         (set! bootstrapmode #t))
        ((string=? "-libpath" a)
         (unless (pair? d)
           (display "Error: -lib requires source directory\n")
           (exit 1))
         (set! libpath (cons (car d) libpath))
         (set! args (cdr d)))
        ((string=? "-source" a)
         (unless (pair? d)
           (display "Error: -source requires source code\n")
           (exit 1))
         (set! source (car d))
         (set! args (cdr d)))
        ((string=? "-out" a)
         (unless (pair? d)
           (display "Error: -out requires an output file\n")
           (exit 1))
         (set! outbin (car d))
         (set! args (cdr d)))
        ((string=? "--" a)
         (set! restargs d)
         (set! args '()))
        (else (set! args d))))
    (consume-args)))

(define globals (make-symbol-hashtable))
(define none (list 'none))

(define (savedump! obj)
  (define (report sym enc)
    (let ((tbl (vector-ref enc 4))
          (offs-rosym (vector-ref enc 2))
          (offs-rwsym (vector-ref enc 3)))
      (let ((rosyms (vector-copy tbl offs-rosym offs-rwsym))
            (rwsyms (vector-copy tbl offs-rwsym (vector-length tbl))))
        (write (list 'LIB: sym 'ROSYM: rosyms 'RWSYM: rwsyms)) (newline))))
  (define (testentry e)
    (let ((vmmac (vector-ref e 7))
          (vmseq (vector-ref e 5))
          (libsym (vector-ref e 0)))
      (let* ((encmac (ribcode-encode vmmac))
             (encseq (ribcode-encode vmseq))
             (decmac (ribcode-decode encmac))
             (decseq (ribcode-decode encseq)))
        (vector-set! e 7 decmac)
        (vector-set! e 5 decseq)
        (report libsym encmac)
        (report libsym encseq))))

  (for-each testentry obj)
  (let ((p (open-binary-output-file outbin)))
   (drypack-put p obj)
   (write (list 'SAVING... source '=> outbin)) (newline)
   (close-port p)))

(define (vmlookup sym)
  (let ((obj (hashtable-ref globals sym none)))
   (when (eq? obj none)
     (error "Could not resolve symbol" sym))
   ;; Return content of the symbol
   (_field0 obj)))

(define (runvm code)
  (define output #f)
  (rvm code globals 
       (if bootstrapmode
           (boot-library restargs)
           (vm-library restargs))
       (lambda (mode x globals)
         (cond
           ((= mode 1)
            (exit x))
           ((= mode 2)
            (set! output x))
           ((= mode 0)
            'done)
           (else
            (error "Unidentified exit code" x)))))
  output)

(consume-args)

(set-interp! runvm vmlookup)

(cond
  (source
    (interp-reset!)
    (interp-set-libpath! libpath)
    (interp-activate!)

    (let ((bundle (interp-gen-bundle source)))
     (cond
       (outbin ;; compile-only
         ;; Modifies bundle by encoding
         (savedump! bundle)
         (exit 0))
       (else ;; Run
         (cache-runtime! bundle)
         ;; Reset globals
         (set! globals (make-symbol-hashtable))
         (interp-reset!)
         (interp-run bundle)))))
  (else
    (error "no op")))

(exit 1)
