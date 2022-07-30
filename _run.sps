(import (yuni scheme)
        (yuniribbit rvm)
        (yuniribbit rsc)
        ;(yuni util files) ;; Cannot load on Bigloo
        (yunitest mini)
        (yuni compat ident))

(define code-hello ");'u?>vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6y") ;; RVM code that prints HELLO!


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


(define runtime (read-source "max-tc.scm"))
(define code (read-source "repl-max.scm"))

(when (eq? (ident-impl) 'cyclone)
  (error "Cyclone: not yet ported."))

(rvm code-hello
     (lambda x
       (write (list 'EXIT: x)) (newline)
       (check-equal x (list #t "HELLO!\n"))))

(let* ((s (compile-program 0 (append runtime code)))
       (c (generate-code "rvm" 0 #f #f s)))
  (let ((in (string-append c "\n(exit 39)\n")))
   (write (list 'RUN: in)) (newline)
   (rvm in (lambda x (write (list 'EXIT: x)) (newline)
             (check-equal x (list 39 "> "))))))

(check-finish)
