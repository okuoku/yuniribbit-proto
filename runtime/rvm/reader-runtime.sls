
;; Special library to break circular dependency between
;; (yuni scheme) and (yuni miniread reader). 
(library (rvm reader-runtime)
  (export %r7c-read/mini)
  (import (yuni scheme)
          (yuni miniread reader))

  (define (%r7c-read/mini . p)
    (let ((port (if (null? p) (current-input-port) (car p))))
     (let loop ((queue ""))
      (let ((r (read-string 4096 port)))
       (if (eof-object? r)
           (if (string=? "" queue)
               (eof-object)
               (car (utf8-read (string->utf8 queue))))
           (loop (string-append queue r)))))))

  )

