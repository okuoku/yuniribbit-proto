(import (yuni scheme)
        (yuniribbit rvm)
        (yuniribbit rsc)
        ;(yuni util files) ;; Cannot load on Bigloo
        (yunitest mini)
        (yuni compat ident))

;; Copied from (yuni util files)
(define (read-source pth)
  (define (file->list proc pth)
    (call-with-input-file
      pth
      (lambda (p)
        (define (itr cur)
          (let ((r (proc p)))
           (if (eof-object? r)
             (reverse cur)
             (itr (cons r cur)))))
        (itr '()))))
  (file->list read pth))

;; Roll our own writer
(define (sexp->string e)
  (cond 
    ((null? e) "()")
    ((list? e)
     (string-append
       "("
       (let ((m (map sexp->string e)))
        (let loop ((acc (car m))
                   (cur (cdr m)))
         (if (null? cur)
           acc
           (loop (string-append acc " " (car cur)) (cdr cur)))))
       ")")
     )
    ((pair? e) ;; Don't try to be cool at dotted-pair
     (string-append
       "("
       (sexp->string (car e))
       " . "
       (sexp->string (cdr e))
       ")"))
    ((number? e) (number->string e))
    ((symbol? e) (symbol->string e))
    ((string? e)
     (string-append "\"" e "\""))
    ((vector? e)
     (string-append "#" (sexp->string (vector->list e))))
    ((eq? #t e) "#t")
    ((eq? #f e) "#f")
    (else
      (error "Unknown object" e))))


(define runtime (read-source "max-tc.scm"))
(define code (read-source "repl-max.scm"))

(when (eq? (ident-impl) 'cyclone)
  (error "Cyclone: not yet ported."))


(let* ((s (compile-program 0 (append runtime code))))
  (define (test value code)
    (define codestr (sexp->string code))
    (write (list 'CODE: codestr)) (newline)
    (rvm s
         (string-append codestr "\n")
         (lambda x
           (write (list 'EXIT: x)) (newline)
           (check-equal value (car x)))))

  (test 0 '(begin (exit 0)))
  (test 39 '(exit 39))
  
  (test 2 '(exit (begin (+ 1 1))))

  (test 12345 '(exit
                 (begin
                   (let ((x '(1234)))
                    (if (equal? x '(1234))
                      12345
                      99999)))))

  (test 12345 '(exit 
                 (begin
                   (let ((a (lambda (x) (cons x '()))))
                     (let ((x (a 1234)))
                      (if (equal? x '(1234))
                        12345
                        99999))))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda (x) (cons x '())))
                   (let ((x (a 5678 1234)))
                    (if (equal? x '(1234))
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda x x))
                   (let ((x (a 5678 1234)))
                    (if (equal? x '(5678 1234))
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda (x y . z) (cons x (cons y (cons z '())))))
                   (let ((x (a 5678 1234 9999)))
                    (if (equal? x '(5678 1234 (9999)))
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (let ((x (+ 1 2 3 4)))
                    (if (equal? x 10)
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda x x))
                   (let ((x (apply-values a (values 1234 5678))))
                    (if (equal? x '(1234 5678))
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda (dummy x) x))
                   (let ((x (apply-values a (values 1234 5678))))
                    (if (equal? x 5678)
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda (dummy x) x))
                   (let ((x (apply-values a (list->values '(1234 5678)))))
                    (if (equal? x 5678)
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (define a (lambda x x))
                   (let ((x (apply-values a 1234)))
                    (if (equal? x '(1234))
                      12345
                      99999)))))
  (test 12345 '(exit
                 (begin
                   (call-with-values
                     (lambda () (values 1234 5678))
                     (lambda (x y)
                       (if (and (equal? x 1234) (equal? y 5678))
                         12345
                         99999))))))
  (test 12345 '(exit
                 (begin (write (integer->char 50))
                        12345)))
  )

(check-finish)
