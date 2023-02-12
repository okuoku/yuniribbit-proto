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
      (write (list 'ARG: a)) (newline)
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
  ;; NB: We have added yunife output of (import ...) but we should do
  ;;     inverse; add a method to query imported library of the expanded
  ;;     library and remove (import ...) from program/library.
  ;; FIXME: ... and why not move this to yunife anyway
  (define outseq '())
  (define loaded corelibs)
  (define (code libname) (yunife-get-library-code fe libname))
  (define (addloaded! libname)
    (write (list 'ADD: libname)) (newline)
    (set! loaded (append loaded (list libname))))
  (define (loaded? libname)
    (let loop ((x loaded))
     (and (pair? x)
          (or (equal? (car x) libname)
              (loop (cdr x))))))

  (define (loadlib! libname)
    (unless (loaded? libname)
      (write (list 'REALIZE: libname)) (newline)
      (process libname)))

  (define (process libname)
    (let ((seq (code libname)))
     (cond
       ((and (pair? seq) (pair? (car seq))
             (eq? 'import (caar seq)))
        (let ((libs (cdar seq)))
         (for-each loadlib! libs))
        (set! outseq (append outseq (cdr seq)))
        (when (pair? libname)
          (addloaded! libname)))

       (else
         (error "Malformed library" libname)))))

  (process #t)
  outseq)

(write (list 'ARGS: args)) (newline)

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