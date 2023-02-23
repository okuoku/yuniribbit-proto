
;; Special library to break circular dependency between
;; (yuni scheme) and (yuni miniread reader). 
(library (rvm reader-runtime)
  (export %r7c-read/mini)
  (import (yuni scheme)
          (r7c-yunicore yuniport)
          (yuni miniread reader))

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



  )

