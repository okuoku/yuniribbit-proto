(library (r7c heap listloop)
  (export memv $fx-length $append)
  (import (rvm-primitives)
          (r7c heap pair)
          (rsc-core-syntax))

  (define (memv x lis)
    (if (pair? lis)
        (if (eqv? x (car lis))
            lis
            (memv x (cdr lis)))
        #f))

  (define (length-loop acc lis)
    (if (pair? lis)
        (length-loop (+ 1 acc) (cdr lis))
        acc))

  (define ($fx-length lis)
    (if (pair? lis)
        (length-loop 0 lis)
        (if (eqv? '() lis)
            0
            (error "Pair required" lis))))

  (define ($append/itr! cur lis) ;; => cur
    (cond
      ((null? lis) cur)
      ((pair? lis)
       (let* ((a (car lis))
              (d (cdr lis))
              (p (cons a '())))
         (set-cdr! cur p)
         ($append/itr! p d)))
      (else
        (error "List required" lis))))

  (define ($append x y)
    (cond
      ((null? x) y)
      ((pair? x)
       (let* ((a (car x))
              (d (cdr x))
              (n (cons a '())))
         (let ((m ($append/itr! n d)))
          (set-cdr! m y))
         n))
      (else
        (error "Pair required" x))))

  )
