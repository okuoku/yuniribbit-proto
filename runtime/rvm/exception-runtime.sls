(library (rvm exception-runtime)
  (export call/cc call-with-current-continuation
          dynamic-wind
          with-exception-handler
          raise
          raise-continuable
          error error-object?)

  (import (r7c-report misc do)
          (r7c-report conditional when)
          (r7c-report conditional unless))

  (define (%list->values0 x)
    (if (and (pair? x) (null? (cdr x)))
        (car x)
        (list->values x)))

  ;; call/cc for RVM
  (define (%%do-call/cc1 rec)
    (let ((cont-here (field1 (field1 (close #f)))))
     (rec (lambda (ret*)
            (let ((v (%list->values0 ret*))
                  (cont-next (field1 (field1 (close #f)))))
             ;; copy stack(0) and pc(2) from cont-here
             (field0-set! cont-next (field0 cont-here))
             (field2-set! cont-next (field2 cont-here))
             v)))))

  ;; dynamic-wind implementation based on TSPL4

  (define %winders '())

  (define (%%list-tail x n)
    (if ($fx= 0 n)
        x
        (%%list-tail (cdr x) ($fx- n 1))))

  (define (common-tail x y)
    (let ((lx (length x))
          (ly (length y)))
      (let loop ((xx (if ($fx> lx ly) (%%list-tail x ($fx- lx ly)) x))
                 (yy (if ($fx> ly lx) (%%list-tail y ($fx- ly lx)) y)))
        (if (eqv? xx yy)
            xx
            (loop (cdr xx) (cdr yy))))
      #|
      (do ((x (if ($fx> lx ly) (%%list-tail x ($fx- lx ly)) x) (cdr x))
           (y (if ($fx> ly lx) (%%list-tail y ($fx- ly lx)) y) (cdr y)))
          ((eq? x y) x))
      |#
      
      ))

  (define (do-wind next)
    (let ((tail (common-tail next %winders)))
     (let loop ((ls %winders))
      (unless (eq? ls tail)
        (set! %winders (cdr ls))
        ;; Call "after"
        ((cdar ls))
        (loop (cdr ls))))
     (let loop ((ls %winders))
      (unless (eq? ls tail)
        (loop (cdr ls))
        ;; Call "before"
        ((caar ls))
        (set! %winders (cdr ls))))))

  (define (call/cc f)
    (%%do-call/cc1
      (lambda (k)
        (f (let ((save %winders))
            (lambda x
              (unless (eq? save %winders)
                (do-wind save))
              (k x)))))))

  (define (call-with-current-continuation f) (call/cc f))

  (define (dynamic-wind before thunk after)
    (before)
    (set! %winders (cons (cons before after) %winders))
    (call-with-values thunk
                      (lambda x 
                        (set! %winders (cdr %winders))
                        (after)
                        (%list->values0 x))))

  ;; Exception runtime
  (define %raise-procs '())
  (define (with-exception-handler handler thunk)
    (let ((current-raise-procs %raise-procs))
      (define (raise-proc continuable? obj)
        ;; pop exception handler
        (set! %raise-procs current-raise-procs)
        (call-with-values (lambda () (handler obj))
                          (lambda vals
                            (cond
                              (continuable?
                                ;; push excetpion handler again
                                (set! %raise-procs (cons raise-proc
                                                         current-raise-procs))
                                (list->values vals))
                              (else
                                ;; reraise
                                (raise obj))))))
      (dynamic-wind
        (lambda () 
          (set! %raise-procs (cons raise-proc current-raise-procs)))
        (lambda () (thunk))
        (lambda ()
          (set! %raise-procs current-raise-procs)))))

  (define (raise obj)
    (unless (pair? %raise-procs)
      (error "unhanlded exception" obj))
    ((car %raise-procs) #f obj))

  (define (raise-continuable obj)
    (unless (pair? %raise-procs)
      (error "unhandled exception" obj))
    ((car %raise-procs) #t obj))


  ;; Error constructors
  (define %error-key "Error") ;; FIXME: Use some heap object
  (define (%make-generic-error msg irr)
    (list %error-key msg irr))
  ;; Error
  (define (error msg . irr)
    (cond
      ((null? %raise-procs) (apply $error/core msg irr))
      (else
        (raise (%make-generic-error msg irr)))))
  (define (error-object? obj)
    (and (pair? obj) (eq? %error-key (car obj))))

  )
