(import (yuni scheme)
        (yuni io drypack)
        (ribbon util interp))

(define libpath
  ;; FIXME: Hardcoded
  '("/home/oku/repos/yuniribbit-proto/runtime"
    "/home/oku/repos/yuniribbit-proto"
    "/home/oku/repos/yuni/external"
    "/home/oku/repos/yuni/lib"
    "/home/oku/repos/yuni/lib-compat"
    "/home/oku/repos/yuni/lib-r7c"
    ))

(define source #f)
(define *command-line* (vector->list ($$command-line 0)))

(define (consume-argument!)
  (when (pair? *command-line*)
    (let ((a (car *command-line*))
          (d (cdr *command-line*)))
      (set! source a))))

(define (decodehost obj)
  (let ((p (open-input-bytevector obj)))
   (drypack-get p)))

(define (encodehost obj)
  (let ((p (open-output-bytevector)))
   (drypack-put p obj)
   (let ((bv (get-output-bytevector p)))
    bv)))

(define ($$lookup-cached-libinfo sym) 
  (define (conv obj)
    (cond
      ((pair? obj) (cons (conv (car obj)) (conv (cdr obj))))
      ((string? obj) (string->symbol obj))
      ((eqv? #f obj) #f)
      ((null? obj) '())
      (else (error "Unknown object" obj))))

  (let ((ret ($$lookup-cached-libinfo/encoded sym)))
   ;(write (list 'CACHED?: sym (if ret #t #f))) (newline)
   (conv ret)))

(consume-argument!)

(unless source
  (set! source "/home/oku/repos/yuni/tests/lib/lighteval0.sps"))

;; Initialize interpreter

(interp-reset!)
(interp-set-libpath! (reverse libpath))
(interp-activate!)

(write (list 'STARTING...: source)) (newline)

#|
(let ((bundle (interp-gen-expanded source)))
 (write (list 'DUMP...)) (newline)
 (let ((p (open-binary-output-file "dump.bin")))
  (for-each (lambda (v)
              (cond
                ((equal? (vector-ref v 0) '(yunitest mini))
                 (write (list 'DUMP: v)) (newline)
                 (drypack-put p v))
                (else
                  (write (list 'SKIP: v)) (newline))))
            bundle)
  (close-port p)))
|#

(let ((bundle (interp-gen-bundle source)))
 (write (list 'INTERP...)) (newline)
 (interp-run bundle))

(write (list 'DONE.)) (newline)
