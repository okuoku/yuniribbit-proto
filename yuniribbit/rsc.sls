(library (yuniribbit rsc)
         (export compile-program)
         (import (yuni scheme)
                 (ribbon vmglue compiler))

         
(define (compile-program program rename)
;;;----------------------------------------------------------------------------

(define jump/call-op 0)
(define set-op       1)
(define get-op       2)
(define const-op     3)
(define if-op        4)
(define enter-op     5) ;; yuniribbit
(define constobj-op  6) ;; yuniribbit

(define (encode-lambda-param params) ;; yuniribbit
  (let loop ((cur params)
             (code 0))
    (cond
      ((pair? cur)
       (loop (cdr cur) (+ code 1)))
      ((null? cur) ;; (list? param) or ()
       code)
      (else
        (- -1 code)))))

(define (encode-lambda-args params) ;; yuniribbit
  (let loop ((acc '())
             (cur params))
    (cond
      ((pair? cur)
       (loop (cons (car cur) acc) (cdr cur)))
      ((null? cur)
       (reverse acc))
      (else
        (reverse (cons cur acc))))))

;;;----------------------------------------------------------------------------

(define procedure-type 1)

(define (make-procedure code env) (rib code env procedure-type))

;;;----------------------------------------------------------------------------

;; The compiler from Ribbit Scheme to RVM code.

(define (make-ctx cte) (rib cte 0 0))

(define (ctx-cte ctx) (field0 ctx))

(define (ctx-cte-set ctx x)
  (rib x (field1 ctx) (field2 ctx)))

(define (comp ctx expr cont)

  (cond ((symbol? expr)
         (let ((v (lookup expr (ctx-cte ctx) 0)))
          (rib get-op v cont)))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (rib constobj-op (vminject (cadr expr)) cont))

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (let ((val (caddr expr)))
                      (let ((v (lookup var (ctx-cte ctx) 1)))
                       (comp ctx val (gen-assign v cont))))))

                 ((eqv? first 'if)
                  (let ((cont-false (comp ctx (cadddr expr) cont)))
                    (let ((cont-true (comp ctx (caddr expr) cont)))
                      (let ((cont-test (rib if-op cont-true cont-false)))
                        (comp ctx (cadr expr) cont-test)))))

                 ((eqv? first 'lambda)
                  (let ((params (cadr expr)))
                    (rib const-op
                         (make-procedure
                          (rib (encode-lambda-param params)
                               0
                               (comp-begin (ctx-cte-set
                                            ctx
                                            (extend (encode-lambda-args params)
                                                    (cons #f
                                                          (cons #f
                                                                (ctx-cte ctx)))))
                                           (cddr expr)
                                           tail))
                          '())
                         (if (null? (ctx-cte ctx))
                             cont
                             (gen-call #f 'close cont)))))

                 ((eqv? first 'begin)
                  (comp-begin ctx (cdr expr) cont))

                 ((eqv? first 'let)
                  (let ((bindings (cadr expr)))
                    (let ((body (cddr expr)))
                      (comp-bind ctx
                                 (map car bindings)
                                 (map cadr bindings)
                                 body
                                 cont))))

                 (else
                  (let ((args (cdr expr)))
                    (if (symbol? first)
                        (comp-call 0 ctx
                                   args
                                   (lambda (argc ctx)
                                     (let ((v (lookup first (ctx-cte ctx) 0)))
                                       (gen-call argc v cont))))
                        (comp-bind ctx
                                   '(_)
                                   (cons first '())
                                   (cons (cons '_ args) '())
                                   cont)))))))

        (else
         ;; self-evaluating
         (rib constobj-op (vminject expr) cont))))

(define (gen-call argc v cont)
  (let ((g (if (eqv? cont tail)
             (rib jump/call-op v 0)      ;; jump
             (rib jump/call-op v cont)))) ;; call
    (if argc (rib enter-op argc g) g)))

(define (gen-assign v cont)
  (rib set-op v (gen-noop cont)))

(define (gen-noop cont)
  (if (and (rib? cont) ;; starts with pop?
           (eqv? (field0 cont) jump/call-op) ;; call?
           (eqv? (field1 cont) 'arg1)
           (rib? (field2 cont)))
      (field2 cont) ;; remove pop
      (rib constobj-op 0 cont))) ;; add dummy value for set!

(define (comp-bind ctx vars exprs body cont)
  (comp-bind* ctx vars exprs ctx body cont))

(define (comp-bind* ctx vars exprs body-ctx body cont)
  (if (pair? vars)
      (let ((var (car vars))
            (expr (car exprs)))
        (comp ctx
              expr
              (comp-bind* (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
                          (cdr vars)
                          (cdr exprs)
                          (ctx-cte-set body-ctx (cons var (ctx-cte body-ctx)))
                          body
                          (gen-unbind ctx cont))))
      (comp-begin body-ctx
                  body
                  cont)))

(define (gen-unbind ctx cont)
  (if (eqv? cont tail)
      cont
      (rib jump/call-op ;; call
           'arg2
           cont)))

(define (comp-begin ctx exprs cont)
  (comp ctx
        (car exprs)
        (if (pair? (cdr exprs))
            (rib jump/call-op ;; call
                 'arg1
                 (comp-begin ctx (cdr exprs) cont))
            cont)))

(define (comp-call argc ctx exprs k)
  (if (pair? exprs)
      (comp ctx
            (car exprs)
            (comp-call (+ 1 argc) (ctx-cte-set ctx (cons #f (ctx-cte ctx)))
                       (cdr exprs)
                       k))
      (k argc ctx)))

(define (lookup var cte i)
  (if (pair? cte)
      (if (eqv? (car cte) var)
          i
          (lookup var (cdr cte) (+ i 1)))
      (rename var)))

(define (extend vars cte)
  (if (pair? vars)
      (cons (car vars) (extend (cdr vars) cte))
      cte))

(define tail (rib jump/call-op 'id 0)) ;; jump

;;;----------------------------------------------------------------------------


(define (comp-exprs exprs)
  (let* ((expansion (expand-begin exprs)))
   (make-procedure
     (rib 0 ;; 0 parameters
          0
          (comp (make-ctx '())
                expansion
                tail))
     '())))

(define (compile-program0 program)
  (let* ((exprs program)
         (proc (comp-exprs (if (pair? exprs) exprs (cons #f '())))))
    proc))

;;;----------------------------------------------------------------------------

;; Expansion of derived forms, like "define", "cond", "and", "or".

(define (expand-expr expr)

  (cond ((symbol? expr)
         expr)

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (expand-constant (cadr expr)))

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (cons 'set!
                          (cons var
                                (cons (expand-expr (caddr expr))
                                      '())))))

                 ((eqv? first 'if)
                  (cons 'if
                        (cons (expand-expr (cadr expr))
                              (cons (expand-expr (caddr expr))
                                    (cons (if (pair? (cdddr expr))
                                              (expand-expr (cadddr expr))
                                              #f)
                                          '())))))

                 ((eqv? first 'lambda)
                  (let ((params (cadr expr)))
                    (cons 'lambda
                          (cons params
                                (cons (expand-body (cddr expr))
                                      '())))))

                 ((or (eqv? first 'let) (eqv? first '$let/core))
                  (let ((x (cadr expr)))
                    (if (symbol? x) ;; named let?
                        (expand-expr
                         (let ((bindings (caddr expr)))
                           (cons
                            (cons
                             'letrec
                             (cons (cons
                                    (cons x
                                          (cons (cons 'lambda
                                                      (cons (map car bindings)
                                                            (cdddr expr)))
                                                '()))
                                    '())
                                   (cons x
                                         '())))
                            (map cadr bindings))))
                        (let ((bindings x))
                          (if (pair? bindings)
                              (cons 'let
                                    (cons (map (lambda (binding)
                                                 (cons (car binding)
                                                       (cons (expand-expr
                                                              (cadr binding))
                                                             '())))
                                               bindings)
                                          (cons (expand-body (cddr expr))
                                                '())))
                              (expand-body (cddr expr)))))))

                 ((eqv? first 'let*)
                  (let ((bindings (cadr expr)))
                    (expand-expr
                     (cons 'let
                           (if (and (pair? bindings) (pair? (cdr bindings)))
                               (cons (cons (car bindings) '())
                                     (cons (cons 'let*
                                                 (cons (cdr bindings)
                                                       (cddr expr)))
                                           '()))
                               (cdr expr))))))

                 ((or (eqv? first 'letrec) (eqv? first 'letrec*))
                  (let ((bindings (cadr expr)))
                    (expand-expr
                     (cons 'let
                           (cons (map (lambda (binding)
                                        (cons (car binding) (cons #f '())))
                                      bindings)
                                 (append (map (lambda (binding)
                                                (cons 'set!
                                                      (cons (car binding)
                                                            (cons (cadr binding)
                                                                  '()))))
                                              bindings)
                                         (cddr expr)))))))

                 ((eqv? first 'begin)
                  (expand-begin (cdr expr)))

                 ((eqv? first 'define)
                  (let ((pattern (cadr expr)))
                    (if (pair? pattern)
                        (cons 'set!
                              (cons (car pattern)
                                    (cons (expand-expr
                                           (cons 'lambda
                                                 (cons (cdr pattern)
                                                       (cddr expr))))
                                          '())))
                        (cons 'set!
                              (cons pattern
                                    (cons (expand-expr (caddr expr))
                                          '()))))))

                 ((eqv? first 'and)
                  (expand-expr
                   (if (pair? (cdr expr))
                       (if (pair? (cddr expr))
                           (cons 'if
                                 (cons (cadr expr)
                                       (cons (cons 'and
                                                   (cddr expr))
                                             (cons #f
                                                   '()))))
                           (cadr expr))
                       #t)))

                 ((eqv? first 'or)
                  (expand-expr
                   (if (pair? (cdr expr))
                       (if (pair? (cddr expr))
                           (cons
                            'let
                            (cons
                             (cons (cons '_
                                         (cons (cadr expr)
                                               '()))
                                   '())
                             (cons
                              (cons 'if
                                    (cons '_
                                          (cons '_
                                                (cons (cons 'or
                                                            (cddr expr))
                                                      '()))))
                              '())))
                           (cadr expr))
                       #f)))

                 ((eqv? first 'cond)
                  (expand-expr
                   (if (pair? (cdr expr))
                       (if (eqv? 'else (car (cadr expr)))
                           (cons 'begin (cdr (cadr expr)))
                           (cons 'if
                                 (cons (car (cadr expr))
                                       (cons (cons 'begin
                                                   (cdr (cadr expr)))
                                             (cons (cons 'cond
                                                         (cddr expr))
                                                   '())))))
                       #f)))

                 (else
                  (expand-list expr)))))

        (else
         (expand-constant expr))))

(define (expand-constant x)
  (cons 'quote (cons x '())))

(define (expand-body exprs)
  (let loop ((exprs exprs) (defs '()))
    (if (pair? exprs)
        (let ((expr (car exprs)))
          (if (and (pair? expr) (eqv? 'define (car expr)) (pair? (cdr expr)))
              (let ((pattern (cadr expr)))
                (if (pair? pattern)
                    (loop (cdr exprs)
                          (cons (cons (car pattern)
                                      (cons (cons 'lambda
                                                  (cons (cdr pattern)
                                                        (cddr expr)))
                                            '()))
                                defs))
                    (loop (cdr exprs)
                          (cons (cons pattern
                                      (cddr expr))
                                defs))))
              (expand-body-done defs exprs)))
        (expand-body-done defs '(0)))))

(define (expand-body-done defs exprs)
  (if (pair? defs)
      (expand-expr
       (cons 'letrec
             (cons (reverse defs)
                   exprs)))
      (expand-begin exprs)))

(define (expand-begin exprs)
  (let ((x (expand-begin* exprs '())))
    (if (pair? x)
        (if (pair? (cdr x))
            (cons 'begin x)
            (car x))
        (expand-constant 0)))) ;; unspecified value

(define (expand-begin* exprs rest)
  (if (pair? exprs)
      (let ((expr (car exprs)))
        (let ((r (expand-begin* (cdr exprs) rest)))
          (cond ((and (pair? expr) (eqv? (car expr) 'begin))
                 (expand-begin* (cdr expr) r))
                (else
                 (cons (expand-expr expr) r)))))
      rest))

(define (expand-list exprs)
  (if (pair? exprs)
      (cons (expand-expr (car exprs))
            (expand-list (cdr exprs)))
      '()))

(compile-program0 program))
)
