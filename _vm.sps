(import (yuni scheme)
        (yuni io drypack)
        (yuniribbit ext)
        (yuniribbit rvm))

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

(consume-args)

(unless run
  (display "No program speficied\n")
  (display "Specify input with -run <COMPILED.bin>\n")
  (exit 1))

(call-with-port
  (open-binary-input-file run)
  (lambda (p)
    (let ((c (drypack-get p)))
     (rvm c 
          (ext-functions-vector)
          ""
          (lambda (x . _)
            (write (list 'EXIT: x)) (newline)
            (exit x))))))

(exit 1)
