(library (rvmruntime applyvalues)
  (export apply call-with-values)
  (import (rvm-primitives))

  (define (apply f . param)
    (let loop ((acc '())
               (cur param))
      (cond
        ((pair? param)
         (if (pair? (cdr cur))
             (loop (cons (car cur) acc) (cdr cur))
             (apply-values f (list->values (append (reverse acc) (car cur))))))
        (else
          (error "apply: Invalid argument")))))

  (define (call-with-values producer consumer)
    (let ((x (producer)))
     (apply-values consumer x)))

  )
