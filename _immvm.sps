(import (yuni scheme)
        (yuni io drypack)
        (yuniribbit ext)
        (yuniribbit heapcore)
        (yuniribbit rvm)
        (ribbon vmglue vm)
        (ribbon util interp)
        (yuni hashtables))

(define libpath '())
(define source #f)
(define outbin #f)
(define args (command-line))

(define (consume-args)
  (when (pair? args)
    (let ((a (car args))
          (d (cdr args)))
      ;(write (list 'ARG: a)) (newline)
      (cond
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
        (else (set! args d))))
    (consume-args)))

(define globals (make-symbol-hashtable))
(define vmlib (vm-library))
(define none (list 'none))

(define (vmlookup sym)
  (let ((obj (hashtable-ref globals sym none)))
   (when (eq? obj none)
     (error "Could not resolve symbol" sym))
   obj))

(define (runvm code)
  (define output #f)
  (rvm code globals vmlib
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
     (cache-runtime! bundle)
     ;; Reset globals
     (set! globals (make-symbol-hashtable))
     (interp-reset!)
     (interp-run bundle)))
  (else
    (error "no op")))

(exit 1)
