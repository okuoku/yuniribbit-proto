(library (yuniribbit rvm)
         (export rvm)
         (import (yuni scheme)
                 (yuni hashtables)
                 (yuniribbit util debug-expand))

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)

(define (_rib? x) (vector? x))
(define (_rib x y z) (vector x y z))
(define (_field0 x) (vector-ref x 0))
(define (_field1 x) (vector-ref x 1))
(define (_field2 x) (vector-ref x 2))
(define (_field0-set! x y) (vector-set! x 0 y))
(define (_field1-set! x y) (vector-set! x 1 y))
(define (_field2-set! x y) (vector-set! x 2 y))

(define (instance? type)
  (lambda (x) (and (_rib? x) (eqv? (_field2 x) type))))

(define _pair? (instance? pair-type))
(define (_cons car cdr) (_rib car cdr pair-type))
(define (_car pair) (_field0 pair))
(define (_cdr pair) (_field1 pair))
(define (_set-car! pair x) (_field0-set! pair x))

(define (_list->string lst) (_rib lst (_length lst) string-type))

(define (_string->uninterned-symbol str) (_rib _false str symbol-type))

(define _false (_rib "false" 0 singleton-type))
(define _true  (_rib "true" 0 singleton-type))
(define _nil   (_rib "nil" 0 singleton-type))

(define (_list-tail lst i)
  (if (< 0 i)
      (_list-tail (_cdr lst) (- i 1))
      lst))

(define (_length lst)
  (if (_pair? lst)
      (+ 1 (_length (_cdr lst)))
      0))


(debug-expand/define
   (define tracing #f)
   (define step-count 0)
   (define start-tracing 0)
   (define next-stamp 0)

   (define (trace-instruction name opnd)
     (display name)
     (if opnd
         (begin
           (display " ")
           (show opnd)))
     (newline))

   (define (show obj)
     (if (not (_rib? obj))
         (display obj)
         (let ((type (_field2 obj)))
           (if (= type 4)
               (begin (display "#") (show (_field0 obj)))
               (case type
                 ((0)
                  (display "(")
                  (show (_field0 obj))
                  (let ((obj
                         (let loop ((n 1) (obj (_field1 obj)))
                           (if (and (_rib? obj) (= (_field2 obj) 0))
                               (if (> n 4)
                                   (begin
                                     (display " ...")
                                     _nil)
                                   (begin
                                     (display " ")
                                     (show (_field0 obj))
                                     (loop (+ n 1) (_field1 obj))))
                               obj))))
                    (if (not (eqv? obj _nil))
                        (begin
                          (display " . ")
                          (show obj)))
                    (display ")")))
                 ((1)
                  (if (_rib? (_field0 obj))
                      (begin
                        (display "#<procedure nparams=")
                        (display (_field0 (_field0 obj)))
                        (display ">"))
                      (begin
                        (display "#<primitive ")
                        (display (_field0 obj))
                        (display ">"))))
                 ((2)
                  (let ((obj (_field1 obj)))
                    (if (and (_rib? obj)
                             (= (_field2 obj) 3)
                             (> (_field1 obj) 0))
                        (let loop ((obj (_field0 obj)))
                          (if (and (_rib? obj) (= (_field2 obj) 0))
                              (begin
                                (display (integer->char (_field0 obj)))
                                (loop (_field1 obj)))))
                        (begin
                          (display "#<symbol ")
                          (show obj)
                          (display ">")))))
                 ((3)
                  (display "\"")
                  (let loop ((obj (_field0 obj)))
                    (if (and (_rib? obj) (= (_field2 obj) 0))
                        (let ((c (_field0 obj)))
                          (case c
                            ((10) (display "\\n"))
                            ((13) (display "\\r"))
                            ((9)  (display "\\t"))
                            ((92) (display "\\\\"))
                            ((34) (display "\\\""))
                            (else (display (integer->char c))))
                          (loop (_field1 obj)))
                        (display "\""))))
                 ((5)
                  (cond ((eqv? obj _false)
                         (display "#f"))
                        ((eqv? obj _true)
                         (display "#t"))
                        ((eqv? obj _nil)
                         (display "()"))
                        (else
                         (display "[")
                         (show (_field0 obj))
                         (display ",")
                         (show (_field1 obj))
                         (display ",")
                         (show (_field2 obj))
                         (display "]"))))
                 (else
                  (display "[")
                  (show (_field0 obj))
                  (display ",")
                  (show (_field1 obj))
                  (display ",")
                  (show (_field2 obj))
                  (display "]")))))))

   (define (start-step stack)
     (set! step-count (+ step-count 1))
     (if (>= step-count start-tracing) (set! tracing #t))
     (if (not tracing)
         (if (>= step-count next-stamp)
             (begin
               (set! next-stamp (exact (floor (+ (* next-stamp 1.01) 1))))
               (display "@")
               (display step-count)
               (newline)))
         (begin
           (display "@")
           (display step-count)
           (display " STACK = (")
           (let loop ((s stack) (sep ""))
             (if (eqv? (_field2 s) 0)
                 (begin
                   (display sep)
                   (show (_field0 s))
                   (loop (_field1 s) " "))
                 (begin
                   (display ")")
                   (newline)))))))
   )

(define (get-cont stack)
  (let loop ((stack stack))
    (if (_rib? (_field2 stack)) stack (loop (_cdr stack)))))

(define (prim0 f)
  (lambda (stack)
    (_cons (f) stack)))

(define (prim1 f)
  (lambda (stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
      (_cons (f x) stack))))

(define (prim1/term f)
  (lambda (stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
     (f x)
     #f)))

(define (prim2 f)
  (lambda (stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y) stack))))

(define (prim3 f)
  (lambda (stack)
    (let* ((z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y z) stack))))

(define (boolean x)
  (if x _true _false))

(define (import-string x)
  (let loop ((acc "")
             (cur (_field0 x)))
    (if (_pair? cur)
      (loop (string-append 
              acc
              (list->string (list (integer->char (_car cur)))))
            (_cdr cur))
      acc)))

(define (rvm code+exports input done-cb)
  (define not-yet (cons 0 0))
  (define output-result not-yet)
  (define output-buf "")
  (define pos 0)
  (define code (car code+exports))
  (define exports (cdr code+exports))
  (define globals (make-symbol-hashtable))
  (define vmsym? (instance? symbol-type))
  (define symcache (make-symbol-hashtable))
  (define (symeq? sym rib) 
    (and (vmsym? rib)
         (let ((x (hashtable-ref symcache sym #f)))
          (eq? x rib))))

  (define (get-var stack opnd)
    (_field0 
      (cond
        ((_rib? opnd) 
         opnd)
        ((symbol? opnd) (hashtable-ref globals opnd "NOT-FOUND!!"))
        (else (_list-tail stack opnd)))))

  (define (set-var stack opnd val)
    (_field0-set! 
      (cond
        ((_rib? opnd) opnd)
        ((symbol? opnd) 
         (when (eq? 'symtbl opnd)
           ;; Handle delayed "code -> VM" import
           (when (_pair? val)
             (let ((sym (_car val)))
              (unless (eqv? symbol-type (_field2 sym))
                (error "Tried to add non-symbol!!"))
              (when (string=? "unspecified" (_field0 sym))
                (let ((name (string->symbol (import-string (_field1 sym)))))
                 (hashtable-set! symcache name sym)
                 (let ((v (hashtable-ref globals name #f)))
                   (when v
                     (_field0-set! sym (_field0 v)))))))))
         (let ((f (hashtable-ref globals opnd #f)))
          (cond
            (f f)
            (else 
              (hashtable-set! globals opnd
                              (_string->uninterned-symbol
                                (symbol->string opnd)))
              (hashtable-ref globals opnd #f)))))
        (else (_list-tail stack opnd)))
      val))


  (define (get-byte)
    (let ((x (char->integer (string-ref input pos))))
     (set! pos (+ pos 1))
     x))

  (define (run pc stack)
    (debug-expand (start-step stack))
    (let ((instr (_field0 pc))
          (opnd (_field1 pc))
          (next (_field2 pc)))
      (case instr

        ((0) ;; jump/call
         (debug-expand
           (if tracing
             (trace-instruction (if (eqv? 0 next) "jump" "call") opnd)))
         (let* ((proc (get-var stack opnd))
                (code (_field0 proc)))
           (if (_rib? code)

             ;; calling a lambda
             (let ((new-cont (_rib 0 proc 0)))
              (let loop ((nargs (_field0 code))
                         (new-stack new-cont)
                         (stack stack))
                (if (< 0 nargs)
                  (loop (- nargs 1)
                        (_cons (_car stack) new-stack)
                        (_cdr stack))
                  (begin
                    (if (_rib? next) ;; non-tail call?
                      (begin
                        (_field0-set! new-cont stack)
                        (_field2-set! new-cont next))
                      (let ((k (get-cont stack)))
                       (_field0-set! new-cont (_field0 k))
                       (_field2-set! new-cont (_field2 k))))
                    (run (_field2 code)
                         new-stack)))))

             ;; calling a primitive
             (let ((stack ((vector-ref primitives code) stack)))
               (and stack
                    (run (if (_rib? next) ;; non-tail call?
                           next
                           (let ((cont (get-cont stack)))
                            (_field1-set! stack (_field0 cont))
                            (_field2 cont)))
                         stack))))))

        ((1) ;; set
         (debug-expand
           (if tracing
             (trace-instruction "set" opnd)))
         (set-var stack opnd (_car stack))
         (run next
              (_cdr stack)))

        ((2) ;; get
         (debug-expand
           (if tracing
             (trace-instruction "get" opnd)))
         (run next
              (_cons (get-var stack opnd) stack)))

        ((3) ;; const
         (debug-expand
           (if tracing
             (trace-instruction "const" opnd)))

         (let ((v (cond
                    ((number? opnd) opnd)
                    ((null? opnd) _nil)
                    ((boolean? opnd) (if opnd _true _false))
                    (else opnd))))
           (run next (_cons v stack))))

        ((4) ;; if
         (debug-expand
           (if tracing
             (trace-instruction "if" #f)))
         (run (if (eqv? (_car stack) _false) next opnd)
              (_cdr stack)))
        ((5) ;; enter (yuniribbit)
         (debug-expand
           (if tracing
             (trace-instruction "enter" opnd)))
         (run next stack))
        (else ;; halt
          (debug-expand
            (if tracing
              (trace-instruction "halt" #f)))
          #f))))

  (define primitives0 '((rib         0)
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


  (define primitives
    (vector (prim3 _rib)             ;; 0
            (prim1 (lambda (x) x))   ;; 1
            _cdr                     ;; 2
            (prim2 (lambda (y x) x)) ;; 3

            (lambda (stack) ;; 4
              (let* ((x (_car stack)) (stack (_cdr stack)))
               (_cons (_rib (_field0 x) stack procedure-type) stack)))

            (prim1 (lambda (x) (boolean (_rib? x)))) ;; 5
            (prim1 _field0) ;; 6
            (prim1 _field1) ;; 7
            (prim1 _field2) ;; 8
            (prim2 (lambda (x y) (_field0-set! x y) y)) ;; 9
            (prim2 (lambda (x y) (_field1-set! x y) y)) ;; 10
            (prim2 (lambda (x y) (_field2-set! x y) y)) ;; 11
            (prim2 (lambda (x y)  ;; 12
                     (cond
                       ((symbol? x)
                        (boolean (or (symeq? x y)
                                     (eqv? x y))))
                       ((symbol? y)
                        (boolean (or (symeq? y x)
                                     (eqv? x y))))
                       (else
                         (boolean (eqv? x y))))))
            (prim2 (lambda (x y) (boolean (< x y)))) ;; 13
            (prim2 +) ;; 14
            (prim2 -) ;; 15
            (prim2 *) ;; 16
            (prim2 quotient) ;; 17

            (prim0 (lambda () ;; 18
                     (if (< pos (string-length input))
                       (get-byte)
                       -1)))

            (prim1 (lambda (x) ;; 19
                     (set! output-buf
                       (string-append
                         output-buf
                         (list->string (list (integer->char x)))))
                     x))

            (prim1/term (lambda (x) ;; 20
                          (set! output-result x)
                          (done-cb output-result output-buf)))))

  (for-each (lambda (e)
              (let ((sym (car e)))
               (hashtable-set! globals sym (_string->uninterned-symbol
                                             (symbol->string sym)))))
            exports)

  ;; Enter primitives
  (for-each (lambda (e)
              (let ((sym (car e))
                    (code (cadr e)))
                (set-var "unused" sym 
                         (_rib code _nil procedure-type))))
            primitives0)

  (hashtable-set! globals 'false _false)
  (hashtable-set! globals 'true _true)
  (hashtable-set! globals 'nil _nil)

  ;; Start
  
  (run (_field2 (_field0 code)) ;; instruction stream of main procedure
       (_rib 0 0 (_rib 6 0 0))) ;; primordial continuation = halt
  (when (eq? not-yet output-result)
    (done-cb #t output-buf))
   ) 


)
