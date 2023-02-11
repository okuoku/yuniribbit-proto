(library (r7c core apply)
  (export apply)
  (import (rsc-core-syntax)
          (r7c core error)
          (rvm-primitives))

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

  )
