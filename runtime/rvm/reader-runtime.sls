
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

  (define (lighteval-transcode/in obj)
    (let ((p (open-input-bytevector obj)))
     (drypack-get p)))

  (define (lighteval-transcode/out obj)
    (let ((p (open-output-bytevector)))
     (drypack-put p obj)
     (get-output-bytevector p)))

  (define lighteval-fe #f)

  (define (lighteval-cache-loader libname sym cb)
    ;(write (list 'CACHELOADER libname sym)) (newline)
    ;; cb = ^[result imports exports code macro*]
    (let ((libinfo (lighteval-transcode/in
                     ($$lookup-cached-libinfo sym)))) 
      (if libinfo
          ;(write (list 'LOOKUP: libinfo)) (newline)
          (let ((imports (car libinfo))
                (exports (cadr libinfo))
                (macname* (caddr libinfo)))
            (let ((mac* (map (lambda (name)
                               (cons name ($$lookup-cached-macro name)))
                             macname*)))
              (cb #t imports exports #f mac*)))
          (cb #f #f #f #f #f))))

  (define (make-lighteval-fe)
    (set! lighteval-fe (make-yunife))
    (let ((current-mode ($$macro-runtime-mode 0)))
     (cond
       ((= current-mode 1) ;; use VM side runtime
        (yunife-register-cache-loader! lighteval-fe lighteval-cache-loader) )
       (else ;; bootstrap
         'do-nothing))))

  (define (%r7c-eval/yuni expr)
    (let* ((current-mode ($$macro-runtime-mode 0))
           (prog (if (= current-mode 0)
                     `(($vm-exit 2 ,expr))
                     `((import (yuni scheme)) ($vm-exit 2 ,expr)))))
      (cond 
        ((= current-mode 0) ;; don't use yunife
         (let ((code (compile-program prog)))
          ($$runvm (lighteval-transcode/out code))))
        (else
          (unless lighteval-fe
            (make-lighteval-fe))
          (yunife-load-sexp-list! lighteval-fe prog)
          (let* ((progsym (yunife-get-libsym lighteval-fe #t))
                 (expanded (yunife-get-library-code lighteval-fe progsym))
                 (code (compile-program expanded)))
            ;(write (list 'RUNNING: expanded)) (newline)
            ($$runvm (lighteval-transcode/out code)))))))

)

