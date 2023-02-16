(import (yuni scheme)
        (yuni util files)
        (yuni io drypack)
        (yunife core))

(define libpath '())
(define source #f)
(define outbin #f)
(define args (command-line))
(define fe (make-yunife))

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

(define corelibs
  '((yunivm-core-syntax) 
    (rsc-core-syntax) 
    (rsc-core-syntax/primitives)
    (rvm-primitives)))

(define (merge-output fe)
  (define outseq '())
  (define progsym (yunife-get-libsym fe #t))
  (define loaded corelibs)
  (define (code libsym) (yunife-get-library-code fe libsym))
  (define (macro libsym) (yunife-get-library-macro fe libsym))
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
    ;(write (list 'PROCESS: libsym)) (newline)
    (let* ((seq (code libsym))
           (mac #f) ;; FIXME: Implement it
           (imports (or (yunife-get-library-imports fe libsym) '()))
           (exports (yunife-get-library-exports fe libsym))
           (import* (map (lambda (libname) (yunife-get-libsym fe libname))
                         imports)))
      (for-each loadlib! import* imports)
      (set! outseq (cons (vector libname libsym import* exports seq mac) 
                         outseq))
      (addloaded! libsym)))

  (process progsym #t)
  (reverse outseq))

;(write (list 'ARGS: args)) (newline)

(consume-args)

(unless source
  (display "No source specified\n")
  (display "Specify source with -source <SOURCE>\n")
  (exit 1))

(unless outbin
  (set! outbin
    (string-append (path-basename source) ".expand.bin")))

;; FIXME: Hardcoded alias map
(yunife-add-alias-map! fe 'yunivm 'yunife-yunivm)

(yunife-add-primitives! fe
                        '(rsc-core-syntax/primitives)
                        (map (lambda (e) (cons e #f))
                             '(let let* letrec define and or cond)))

;; Set libpath
(for-each (lambda (e) (yunife-add-path! fe e)) libpath)

;; Load source
(yunife-load! fe source)

;; Emit expanded code (in binary)
(when (file-exists? outbin)
  (delete-file outbin))

(let ((total (merge-output fe)))
 ;; Output expanded sexp
 (call-with-port 
   (open-binary-output-file outbin)
   (lambda (p)
     (drypack-put p total)))

 ;; Readback test
 (call-with-port
   (open-binary-input-file outbin)
   (lambda (p)
     (let ((x (drypack-get p)))
      (unless (equal? x total)
        (display "ERROR: Packed message did not match\n")
        (exit -1))))))

(write (list 'OUTBIN source '=> outbin)) (newline)
