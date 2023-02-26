
;; Special library to break circular dependency between
;; (yuni scheme) and (yuni miniread reader). 
(library (rvm reader-runtime)
  (export %r7c-read/mini)
  (import (yuni scheme)
          ;; read
          (r7c-yunicore yuniport)
          (yuni miniread reader)
          ;; eval
          (yunife core)
          (yuni io drypack)
          (yuniribbit rsc))

  (define (%r7c-read/mini . p)
    (let* ((port (if (null? p) (current-input-port) (car p)))
           (curcache (yuniport-reader-cache-ref port)))
      (cond
        (curcache ;; return previously-read content
          (let ((a (car curcache))
                (d (cdr curcache)))
            (yuniport-reader-cache-set! port (if (null? d) #f d))
            a))
        (else
          (let loop ((queue ""))
           (let ((r (read-string 4096 port)))
            (if (eof-object? r)
                (if (string=? "" queue)
                    (eof-object)
                    (begin
                      (yuniport-reader-cache-set! port
                        (utf8-read (string->utf8 queue)))
                      (%r7c-read/mini port)))
                (loop (string-append queue r)))))))))

  ;; lighteval runtime

  (define lighteval-fe #f)

  (define (lighteval-cache-loader libname sym cb)
    ;(write (list 'CACHELOADER libname sym)) (newline)
    ;; cb = ^[result imports exports macro*]
    (let ((libinfobv ($$lookup-cached-libinfo sym))) 
     (let* ((p (open-input-bytevector libinfobv))
            (libinfo (drypack-get p)))
       ;(write (list 'LOOKUP: libinfo)) (newline)
       (let ((imports (car libinfo))
             (exports (cadr libinfo))
             (macname* (caddr libinfo)))
         (let ((mac* (map (lambda (name)
                            (cons name ($$lookup-cached-macro name)))
                          macname*)))
           (cb #t imports exports mac*))))))

  (define (make-lighteval-fe)
    (set! lighteval-fe (make-yunife))
    (yunife-register-cache-loader! lighteval-fe lighteval-cache-loader))

  (define (lighteval-transcode obj)
    (define p (open-output-bytevector))
    (drypack-put p obj)
    (get-output-bytevector p))

  (define (%r7c-eval/yuni expr)
    (define prog `((import (yuni scheme)) ($vm-exit 2 ,expr)))
    (make-lighteval-fe)
    (yunife-load-sexp-list! lighteval-fe prog)
    (let* ((progsym (yunife-get-libsym lighteval-fe #t))
           (expanded (yunife-get-library-code lighteval-fe progsym))
           (code (compile-program expanded)))
      ;(write (list 'RUNNING: expanded)) (newline)
      ($$runvm (lighteval-transcode code))))

)

