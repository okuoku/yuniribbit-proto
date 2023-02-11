(library (yuniribbit rvm)
         (export rvm)
         (import (yuni scheme)
                 (yuni hashtables))

(define pair-type      0)
(define procedure-type 1)
(define symbol-type    2)
(define string-type    3)
(define vector-type    4)
(define singleton-type 5)
(define values-type    6) ;; yuniribbit (lis 0 TYPE)
;; 7 was char-type
(define bytevector-type 8) ;; yuniribbit
(define simple-struct-type 9) ;; yuniribbit

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
(define _eof-object (_rib "eof-object" 0 singleton-type))

(define (_list-tail lst i)
  (if (< 0 i)
      (_list-tail (_cdr lst) (- i 1))
      lst))

(define (_length lst)
  (if (_pair? lst)
      (+ 1 (_length (_cdr lst)))
      0))

(define (get-cont stack)
  (let loop ((stack stack))
    (if (_rib? (_field2 stack)) stack (loop (_cdr stack)))))

(define (prim0 f)
  (lambda (vals stack)
    (_cons (f) stack)))

(define (prim1 f)
  (lambda (vals stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
      (_cons (f x) stack))))

(define (prim1/term f)
  (lambda (vals stack)
    (let* ((x (_car stack)) (stack (_cdr stack)))
     (f x)
     #f)))

(define (prim2 f)
  (lambda (vals stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y) stack))))

(define (prim3 f)
  (lambda (vals stack)
    (let* ((z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y z) stack))))

(define (prim5 f)
  (lambda (vals stack)
    (let* ((v (_car stack)) (stack (_cdr stack))
           (u (_car stack)) (stack (_cdr stack))
           (z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y z u v) stack))))

(define (primn f)
  (lambda (vals stack)
    (let loop ((rest vals)
               (stack stack)
               (args '()))
      (if (= rest 0)
        (_cons (apply f args) stack)
        (loop (- rest 1) (_cdr stack) (cons (_car stack) args))))))

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
     (integer->char x)))

  (define (run vals pc stack)
    (let ((instr (_field0 pc))
          (opnd (_field1 pc))
          (next (_field2 pc)))
      (case instr

        ((0) ;; jump/call
         (let* ((proc (get-var stack opnd))
                (code (_field0 proc)))
           (if (_rib? code)

             ;; calling a lambda
             (let ((new-cont (_rib 0 proc 0)))
              ;; Audit and construct arguments
              (let* ((layout (_field0 code)) ;; was nargs param
                     ;; argnc: "rest" argument count
                     (argnc (if (and vals (< layout 0))
                              (+ vals 1 layout)
                              0))
                     ;; nargs: physical argument count
                     (nargs (if (< layout 0)
                              (- 0 layout)
                              layout)))
                ;; Audit nargs
                (when (and vals (< argnc 0) (< vals nargs))
                  (error "Unmatched argument count" vals nargs))
               (let aloop ((acc _nil)
                           (stack stack)
                           (res argnc))
                 (if (< 0 res)
                   (aloop (_cons (_car stack) acc) (_cdr stack) (- res 1))
                   (let loop ((nargs nargs)
                              (new-stack new-cont)
                              (stack (if (= argnc 0) 
                                       stack
                                       (_cons acc stack))))
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
                         (run #f (_field2 code)
                              new-stack))))))))

             ;; calling a primitive
             (if (< code 0) 
               ;; calling special primitive that handle multiple values
               (case code
                 ((-12) ;; (apply-values consumer v*)
                  (let* ((v*       (_car stack)) (stack (_cdr stack))
                         (consumer (_car stack)) (stack (_cdr stack))
                         (trampoline0 (_rib consumer 0 0))
                         ;; 0 = jump/call-op
                         (trampoline (_rib 0 trampoline0 next)))
                    (cond
                      ((and (_rib? v*) (eqv? values-type (_field2 v*)))
                       (let loop ((n 0)
                                  (cur (_field0 v*))
                                  (stack stack))
                         (if (eqv? _nil cur)
                           (run n trampoline stack)
                           (loop (+ n 1) 
                                 (_cdr cur) 
                                 (_cons (_car cur) stack)))))
                      (else ;; standard apply
                        (run 1 trampoline (_cons v* stack))))))
                 
                 (else (error "Invalid special primitive" vals pc stack)))

               ;; calling std primitive
               (let ((stack ((vector-ref primitives code) vals stack)))
                (and stack
                     (run #f
                          (if (_rib? next) ;; non-tail call?
                            next
                            (let ((cont (get-cont stack)))
                             (_field1-set! stack (_field0 cont))
                             (_field2 cont)))
                          stack)))))))

        ((1) ;; set
         (set-var stack opnd (_car stack))
         (run #f
              next
              (_cdr stack)))

        ((2) ;; get
         (run #f
              next
              (_cons (get-var stack opnd) stack)))

        ((3) ;; const
         (let ((v (cond
                    ((number? opnd) opnd)
                    ((null? opnd) _nil)
                    ((boolean? opnd) (if opnd _true _false))
                    (else opnd))))
           (run #f next (_cons v stack))))

        ((4) ;; if
         (run #f
              (if (eqv? (_car stack) _false) next opnd)
              (_cdr stack)))
        ((5) ;; enter (yuniribbit)
         (run opnd next stack))
        (else ;; halt
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
                        ;; yuniribbit
                        (values      21)
                        (list->values 22)
                        (char?       23)
                        (char->integer 24)
                        (integer->char 25)
                        (vec-copy    26)
                        (vec-copy!   27)
                        (vec-ref     28)
                        (vec-set!    29)
                        (vec-new     30)
                        (vec-length  31)
                        (vec-fill!   32)
                        ))


  (define primitives
    (vector (prim3 _rib)             ;; 0
            (prim1 (lambda (x) x))   ;; 1
            (lambda (vals stack) (_cdr stack))             ;; 2
            (prim2 (lambda (y x) x)) ;; 3

            (lambda (vals stack) ;; 4
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
            (primn +) ;; 14
            (primn -) ;; 15
            (primn *) ;; 16
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
                          (done-cb output-result output-buf)))
            ;; yuniribbit
            ;; 21: values
            (lambda (vals stack)
              (cond
                ((= vals 1) stack)
                (else (let loop ((n vals)
                                 (cur _nil)
                                 (stack stack))
                        (if (= n 0)
                          (_cons (_rib cur 0 values-type) stack)
                          (loop (- n 1) 
                                (_cons (_car stack) cur) 
                                (_cdr stack)))))))
            ;; 22: list->values
            (lambda (vals stack)
              (_cons (_rib (_car stack) 0 values-type) (_cdr stack)))
            ;; 23: char?
            (prim1 (lambda (c) (boolean (char? c))))
            ;; 24: char->integer
            (prim1 char->integer)
            ;; 25: integer->char
            (prim1 integer->char)
            ;; 26: (vec-copy vec start end) ;; -1 for full copy
            ;; 27: (vec-copy! tgt loc src start end)
            ;; 28: (vec-ref vec idx)
            ;; 29: (vec-set! vec idx obj)
            ;; 30: (vec-new tag k)
            ;; 31: (vec-length vec)
            ;; 32: (vec-fill! vec obj from to)
            ))

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

  (set-var "unused" 'apply-values (_rib -12 _nil procedure-type))

  (hashtable-set! globals 'false _false)
  (hashtable-set! globals 'true _true)
  (hashtable-set! globals 'nil _nil)
  (hashtable-set! globals '_eof-object _eof-object)

  ;; Start
  
  (run #f ;; No values
       (_field2 (_field0 code)) ;; instruction stream of main procedure
       (_rib 0 0 (_rib 6 0 0))) ;; primordial continuation = halt
  (when (eq? not-yet output-result)
    (done-cb #t output-buf))
   ) 


)
