(library (yuniribbit rsc)
         (export compile-program)
         (import (yuni scheme)
                 (yuni util files)
                 (yuni hashtables))

         
;;

(define (pipe-through program output)
 (display "*** Minification is not supported with this Scheme system\n")
 (display "*** so the generated code was not minified.\n")
 (display "*** You might want to try running ")
 (display program)
 (display " manually.\n")
 output)



(define (cmd-line)
 (command-line))


(define (exit-program-normally)
 (exit 0))

(define (exit-program-abnormally)
 (exit 1))


(define (make-table)
  (make-symbol-hashtable))

(define (table-ref table key default)
  (hashtable-ref table key default))

(define (table-set! table key value)
  (hashtable-set! table key value))

(define (table-length table)
  (hashtable-size table))

(define (table->list table)
  (let-values (((keys entries) (hashtable-entries table)))
              (vector->list (vector-map cons keys entries))))


(define uninterned-symbols-counter 0)
(define uninterned-symbols (make-table))

(define (str->uninterned-symbol string)
  (let* ((name
           (string-append "@@@" ;; use a "unique" prefix
                          (number->string
                            uninterned-symbols-counter)))
         (sym
           (string->symbol name)))
    (table-set! uninterned-symbols sym string) ;; remember "real" name
    (set! uninterned-symbols-counter (+ 1 uninterned-symbols-counter))
    sym))

(define (symbol->str symbol)
  (table-ref uninterned-symbols symbol (symbol->string symbol)))


(define (rsc-path-extension path)
  (let loop ((i (- (string-length path) 1)))
   (if (< i 0)
     ""
     (if (= (char->integer (string-ref path i)) 46) ;; #\.
       (substring path i (string-length path))
       (loop (- i 1))))))

(define (rsc-path-directory path)
  (let loop ((i (- (string-length path) 1)))
   (if (< i 0)
     "./"
     (if (= (char->integer (string-ref path i)) 47) ;; #\/
       (substring path 0 (+ i 1))
       (loop (- i 1))))))

(define (path-expand path dir)
  (if (= (string-length dir) 0)
    path
    (if (= (char->integer (string-ref dir (- (string-length dir) 1))) 47) ;; #\/
      (string-append dir path)
      (string-append dir (string-append "/" path)))))

(define (pp obj)
  (write obj)
  (newline))


(define (list-sort! compare list)

  ;; Stable mergesort algorithm

  (define (sort list len)
    (if (= len 1)
      (begin
        (set-cdr! list '())
        list)
      (let ((len1 (quotient len 2)))
       (let loop ((n len1) (tail list))
        (if (> n 0)
          (loop (- n 1) (cdr tail))
          (let ((x (sort tail (- len len1))))
           (merge (sort list len1) x)))))))

  (define (merge list1 list2)
    (if (pair? list1)
      (if (pair? list2)
        (let ((x1 (car list1))
              (x2 (car list2)))
          (if (compare x2 x1)
            (merge-loop list2 list2 list1 (cdr list2))
            (merge-loop list1 list1 (cdr list1) list2)))
        list1)
      list2))

  (define (merge-loop result prev list1 list2)
    (if (pair? list1)
      (if (pair? list2)
        (let ((x1 (car list1))
              (x2 (car list2)))
          (if (compare x2 x1)
            (begin
              (set-cdr! prev list2)
              (merge-loop result list2 list1 (cdr list2)))
            (begin
              (set-cdr! prev list1)
              (merge-loop result list1 (cdr list1) list2))))
        (begin
          (set-cdr! prev list1)
          result))
      (begin
        (set-cdr! prev list2)
        result)))

  (let ((len (length list)))
   (if (= 0 len)
     '()
     (sort list len))))

(define (list-sort compare list)
  (list-sort! compare (append list '())))


(define (script-file)
  "")

(define (executable-path)
  "")


(define (string-concatenate string-list separator)
  (if (pair? string-list)
    (let ((rev-string-list (reverse string-list))
          (sep (string->list separator)))
      (let loop ((lst (cdr rev-string-list))
                 (result (string->list (car rev-string-list))))
        (if (pair? lst)
          (loop (cdr lst)
                (append (string->list (car lst))
                        (append sep
                                result)))
          (list->string result))))
    ""))

;;;----------------------------------------------------------------------------

(define predefined '(rib false true nil)) ;; predefined symbols

(define primitives0 '(
(rib         0) ;; predefined by RVM (must be first and 0)
(id          1)
(arg1        2)
(arg2        3)
(close       4)
(rib?        5)
(field0      6)
(field1      7)
(field2      8)
(field0-set! 9)
(field1-set! 10)
(field2-set! 11)
(eqv?        12)
(<           13)
(+           14)
(-           15)
(*           16)
(quotient    17)
(getchar     18)
(putchar     19)
(exit        20)
))

(define jump/call-op 0)
(define set-op       1)
(define get-op       2)
(define const-op     3)
(define if-op        4)
(define enter-op     5) ;; yuniribbit

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





(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (instance? o type) (and (rib? o) (eqv? (field2 o) type)))

(define (rib field0 field1 field2)
  (let ((r (make-vector 3)))
   (vector-set! r 0 field0)
   (vector-set! r 1 field1)
   (vector-set! r 2 field2)
   r))

(define (rib? o) (vector? o))
(define (field0 o) (vector-ref o 0))
(define (field1 o) (vector-ref o 1))
(define (field2 o) (vector-ref o 2))
(define (field0-set! o x) (vector-set! o 0 x) o)
(define (field1-set! o x) (vector-set! o 1 x) o)
(define (field2-set! o x) (vector-set! o 2 x) o)

(define (procedure2? o) (instance? o procedure-type))
(define (make-procedure code env) (rib code env procedure-type))
(define (procedure-code proc) (field0 proc))
(define (procedure-env proc) (field1 proc))

(define (oper pc) (field0 pc))
(define (opnd pc) (field1 pc))
(define (next pc) (field2 pc))

;;;----------------------------------------------------------------------------

;; The compiler from Ribbit Scheme to RVM code.

(define (make-ctx cte live exports) (rib cte (cons live '()) exports))

(define (ctx-cte ctx) (field0 ctx))
(define (ctx-live ctx) (car (field1 ctx)))
(define (ctx-exports ctx) (field2 ctx))

(define (ctx-cte-set ctx x)
  (rib x (field1 ctx) (field2 ctx)))

(define (ctx-live-set! ctx x)
  (set-car! (field1 ctx) x))

(define (comp ctx expr cont)

  (cond ((symbol? expr)
         (let ((v (lookup expr (ctx-cte ctx) 0)))
           (if (eqv? v expr) ;; global?
               (let ((g (live? expr (ctx-live ctx))))
                 (if (and g (constant?0 g)) ;; constant propagated?
                     (rib const-op (cadr (cadr g)) cont)
                     (rib get-op v cont)))
               (rib get-op v cont))))

        ((pair? expr)
         (let ((first (car expr)))

           (cond ((eqv? first 'quote)
                  (rib const-op (cadr expr) cont))

                 ((eqv? first 'set!)
                  (let ((var (cadr expr)))
                    (let ((val (caddr expr)))
                      (let ((v (lookup var (ctx-cte ctx) 1)))
                        (if (eqv? v var) ;; global?
                            (let ((g (live? var (ctx-live ctx))))
                              (if g
                                  (if (and (constant?0 g)
                                           (not (assoc var (ctx-exports ctx))))
                                      (begin
;;                                        (pp `(*** constant propagation of ,var = ,(cadr g))
;;                                             (current-error-port))
                                        (gen-noop cont))
                                      (comp ctx val (gen-assign v cont)))
                                  (begin
;;                                    (pp `(*** removed dead assignment to ,var)
;;                                         (current-error-port))
                                    (gen-noop cont))))
                            (comp ctx val (gen-assign v cont)))))))

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
                             (gen-call #f (use-symbol ctx 'close) cont)))))

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
         (rib const-op expr cont))))

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
      (rib const-op 0 cont))) ;; add dummy value for set!

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
           (use-symbol ctx 'arg2)
           cont)))

(define (use-symbol ctx sym)
  (ctx-live-set! ctx (add-live sym (ctx-live ctx)))
  sym)

(define (comp-begin ctx exprs cont)
  (comp ctx
        (car exprs)
        (if (pair? (cdr exprs))
            (rib jump/call-op ;; call
                 (use-symbol ctx 'arg1)
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
      var))

(define (extend vars cte)
  (if (pair? vars)
      (cons (car vars) (extend (cdr vars) cte))
      cte))

(define tail (rib jump/call-op 'id 0)) ;; jump

;;;----------------------------------------------------------------------------

(define (extract-exports program)
  ;; By default all symbols are exported when the program contains
  ;; no (export ...) form.
  (let loop ((lst program) (rev-exprs '()) (exports #f))
    (if (pair? lst)
        (let ((first (car lst)))
          (if (and (pair? first) (eqv? (car first) 'export))
              (loop (cdr lst)
                    rev-exprs
                    (append (cdr first) (or exports '())))
              (loop (cdr lst)
                    (cons first rev-exprs)
                    exports)))
        (cons (reverse rev-exprs) exports))))

(define (exports->alist exports)
  (if (pair? exports)
      (map (lambda (x)
             (if (symbol? x)
                 (cons x x)
                 (cons (car x) (cadr x))))
           exports)
      exports))

(define (comp-exprs-with-exports exprs exports)
  (let* ((expansion
          (expand-begin exprs))
         (live
          (liveness-analysis expansion exports))
         (exports
          (or exports
              (map (lambda (v)
                     (let ((var (car v)))
                       (cons var var)))
                   live))))
    (cons
     (make-procedure
      (rib 0 ;; 0 parameters
           0
           (comp (make-ctx '() live exports)
                 expansion
                 tail))
      '())
     exports)))

(define (compile-program verbosity program)
  (let* ((exprs-and-exports
          (extract-exports program))
         (exprs
          (car exprs-and-exports))
         (exports
          (cdr exprs-and-exports))
         (proc-and-exports
          (comp-exprs-with-exports
           (if (pair? exprs) exprs (cons #f '()))
           (exports->alist exports))))
    (if (>= verbosity 2)
        (begin
          (display "*** RVM code:\n")
          (pp (car proc-and-exports))))
    (if (>= verbosity 3)
        (begin
          (display "*** exports:\n")
          (pp (cdr proc-and-exports))))
    proc-and-exports))

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

                 ((eqv? first 'let)
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

                 ((eqv? first 'letrec)
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
                ((and (pair? expr) (eqv? (car expr) 'cond-expand))
                 (expand-cond-expand-clauses (cdr expr) r))
                (else
                 (cons (expand-expr expr) r)))))
      rest))

(define (cond-expand-eval expr)
  (cond ((and (pair? expr) (eqv? (car expr) 'not))
         (not (cond-expand-eval (cadr expr))))
        ((and (pair? expr) (eqv? (car expr) 'and))
         (not (memv #f (map cond-expand-eval (cdr expr)))))
        ((and (pair? expr) (eqv? (car expr) 'or))
         (not (not (memv #t (map cond-expand-eval (cdr expr))))))
        (else
         (eqv? expr 'ribbit))))

(define (expand-cond-expand-clauses clauses rest)
  (if (pair? clauses)
      (let ((clause (car clauses)))
        (if (or (eqv? 'else (car clause))
                (cond-expand-eval (car clause)))
            (expand-begin* (cdr clause) rest)
            (expand-cond-expand-clauses (cdr clauses) rest)))
      rest))

(define (expand-list exprs)
  (if (pair? exprs)
      (cons (expand-expr (car exprs))
            (expand-list (cdr exprs)))
      '()))

;;;----------------------------------------------------------------------------

;; Global variable liveness analysis.

(define (liveness-analysis expr exports)
  (let ((live (liveness-analysis-aux expr '())))
    (if (assoc 'symtbl live)
        (liveness-analysis-aux expr exports)
        live)))

(define (liveness-analysis-aux expr exports)
  (let loop ((live-globals
              (add-live 'arg1 ;; TODO: these should not be forced live...
                        (add-live 'arg2
                                  (add-live 'close
                                            (add-live 'id
                                                      (exports->live
                                                       (or exports '()))))))))
    (reset-defs live-globals)
    (let ((x (liveness expr live-globals (not exports))))
      (if (eqv? x live-globals)
          live-globals
          (loop x)))))

(define (exports->live exports)
  (if (pair? exports)
      (cons (cons (car (car exports)) '())
            (exports->live (cdr exports)))
      '()))

(define (reset-defs lst)
  (let loop ((lst lst))
    (if (pair? lst)
        (begin
          (set-cdr! (car lst) '())
          (loop (cdr lst)))
        #f)))

(define (add-live var live-globals)
  (if (live? var live-globals)
      live-globals
      (let ((g (cons var '())))
        (cons g live-globals))))

(define (live? var lst)
  (if (pair? lst)
      (let ((x (car lst)))
        (if (eqv? var (car x))
            x
            (live? var (cdr lst))))
      #f))

(define (constant?0 g)
  (and (pair? (cdr g))
       (null? (cddr g))
       (pair? (cadr g))
       (eqv? 'quote (car (cadr g)))))

(define (in? var cte)
  (not (eqv? var (lookup var cte 0))))

(define (liveness expr live-globals export-all?)

  (define (add var)
    (set! live-globals (add-live var live-globals)))

  (define (add-val val)
    (cond ((symbol? val)
           (add val))
          ((pair? val)
           (add-val (car val))
           (add-val (cdr val)))
          ((vector? val)
           (for-each add-val (vector->list val)))))

  (define (liveness expr cte top?)

    (cond ((symbol? expr)
           (if (in? expr cte) ;; local var?
               #f
               (add expr))) ;; mark the global variable as "live"

          ((pair? expr)
           (let ((first (car expr)))

             (cond ((eqv? first 'quote)
                    (let ((val (cadr expr)))
                      (add-val val)))

                   ((eqv? first 'set!)
                    (let ((var (cadr expr)))
                      (let ((val (caddr expr)))
                        (if (in? var cte) ;; local var?
                            (liveness val cte #f)
                            (begin
                              (if export-all? (add var))
                              (let ((g (live? var live-globals))) ;; variable live?
                                (if g
                                    (begin
                                      (set-cdr! g (cons val (cdr g)))
                                      (liveness val cte #f))
                                    #f)))))))

                   ((eqv? first 'if)
                    (liveness (cadr expr) cte #f)
                    (liveness (caddr expr) cte #f)
                    (liveness (cadddr expr) cte #f))

                   ((eqv? first 'let)
                    (let ((bindings (cadr expr)))
                      (liveness-list (map cadr bindings) cte)
                      (liveness (caddr expr) (append (map car bindings) cte) #f)))

                   ((eqv? first 'begin)
                    (liveness-list (cdr expr) cte))

                   ((eqv? first 'lambda)
                    (let ((params (cadr expr)))
                      (liveness (caddr expr) (extend params cte) #f)))

                   (else
                    (liveness-list expr cte)))))

          (else
           #f)))

  (define (liveness-list exprs cte)
    (if (pair? exprs)
        (begin
          (liveness (car exprs) cte #f)
          (liveness-list (cdr exprs) cte))
        #f))

  (liveness expr '() #t)

  live-globals)

;;;----------------------------------------------------------------------------
)
