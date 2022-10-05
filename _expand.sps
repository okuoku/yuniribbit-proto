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
(yunife-add-alias-map! fe 'yuni 'yunife-yuni)
(yunife-add-alias-map! fe 'yunivm 'yunife-yunivm)

;; Set libpath
(for-each (lambda (e) (yunife-add-path! fe e)) libpath)

;; Load source
(yunife-load! fe source)

;; Emit expanded code (in binary)
(when (file-exists? outbin)
  (delete-file outbin))

(let ((c (yunife-get-library-code fe #t)))
 (call-with-port 
   (open-binary-output-file outbin)
   (lambda (p)
     (drypack-put p c))))

;; Readback test
(let ((c (yunife-get-library-code fe #t)))
 (call-with-port
   (open-binary-input-file outbin)
   (lambda (p)
     (let ((x (drypack-get p)))
      (unless (equal? x c)
        (display "ERROR: Packed message did not match\n")
        (exit -1))))))

(write (list 'OUTBIN source '=> outbin)) (newline)
