(import (yuni scheme)
        (yuni util files)
        (yunife core))

(define libpath '())
(define source #f)
(define out #f)
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
        (else (set! args d))))
    (consume-args)))

(write (list 'ARGS: args)) (newline)

(consume-args)

(unless source
  (display "No source specified\n")
  (display "Specify source with -source <SOURCE>\n"))

(set! out
  (string-append (path-basename source) ".expand.scm"))

(write (list 'OUT source '=> out)) (newline)

;; FIXME: Hardcoded alias map
(yunife-add-alias-map! fe 'yuni 'yunife-yuni)
(yunife-add-alias-map! fe 'yunivm 'yunife-yunivm)

;; Set libpath
(for-each (lambda (e) (yunife-add-path! fe e)) libpath)

;; Load source
(yunife-load! fe source)

;; Emit expanded code
(when (file-exists? out)
  (delete-file out))

(let ((c (yunife-get-library-code fe #t)))
 (call-with-output-file
   out
   (lambda (p)
     (write c p))))
