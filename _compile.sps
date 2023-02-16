(import (yuni scheme)
        (yuni util files)
        (yuni io drypack)
        (yuniribbit rsc))

(define in #f)
(define out #f)
(define args (command-line))

(define (consume-args)
  (when (pair? args)
    (let ((a (car args))
          (d (cdr args)))
      (cond
        ((string=? "-in" a)
         (unless (pair? d)
           (display "Error: -in requires a drypack file\n")
           (exit 1))
         (set! in (car d))
         (set! args (cdr d)))
        ((string=? "-out" a)
         (unless (pair? d)
           (display "Error: -out requires an output file\n")
           (exit 1))
         (set! out (car d))
         (set! args (cdr d)))
        (else (set! args d))))
    (consume-args)))

(define (compile-bundle x)
  (define (proc1 v)
    (let ((libname (vector-ref v 0))
          (libsym (vector-ref v 1))
          (import* (vector-ref v 2))
          (exports (vector-ref v 3))
          (seq (vector-ref v 4))
          (mac (vector-ref v 5)))
      (let ((c (compile-program seq)))
       (vector
         libname libsym import*
         exports
         c
         mac))))
  (map proc1 x))

(consume-args)

(unless in
  (display "No source speficied\n")
  (display "Specify input with -in <SOURCE>\n")
  (exit 1))

(unless out
  (set! out (string-append in ".compile.bin")))

(call-with-port
  (open-binary-input-file in)
  (lambda (p)
    (let* ((x (drypack-get p))
           (c (compile-bundle x)))
      (when (file-exists? out)
        (delete-file out))
      (call-with-port
        (open-binary-output-file out)
        (lambda (pout)
          (drypack-put pout c)))
      ;; Readback test
      (call-with-port
        (open-binary-input-file out)
        (lambda (p)
          (let ((y (drypack-get p)))
           (unless (equal? c y)
             (display "ERROR: Packed message did not match\n")
             (exit -1))))))))

(write (list 'COMPILE: in '=> out)) (newline)

