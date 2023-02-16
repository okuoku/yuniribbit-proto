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
(define hashtable-type 10) ;; yuniribbit

(define (_rib? x) (vector? x))
(define (_rib x y z) (vector x y z))
(define (_field0 x) (vector-ref x 0))
(define (_field1 x) (vector-ref x 1))
(define (_field2 x) (vector-ref x 2))
(define (_field0-set! x y) (vector-set! x 0 y))
(define (_field1-set! x y) (vector-set! x 1 y))
(define (_field2-set! x y) (vector-set! x 2 y))

(define (_rib?/real x) (and (vector? x) 
                            (= 3 (vector-length x))))

(define (instance? type)
  (lambda (x) (and (_rib?/real x) (eqv? (_field2 x) type))))

(define _pair? (instance? pair-type))
(define (_cons car cdr) (_rib car cdr pair-type))
(define (_car pair) (_field0 pair))
(define (_cdr pair) (_field1 pair))
(define (_set-car! pair x) (_field0-set! pair x))

(define (_make-uninterned-symbol str) 
  ;(_rib _false str symbol-type)
  (_rib str str symbol-type) ;; debug
  )

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

(define (prim4 f)
  (lambda (vals stack)
    (let* ((u (_car stack)) (stack (_cdr stack))
           (z (_car stack)) (stack (_cdr stack))
           (y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y z u) stack))))

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

(define _symbol? (instance? symbol-type))
(define _string? (instance? string-type))
(define _vector? (instance? vector-type))
(define _bytevector? (instance? bytevector-type))
(define _simple-struct? (instance? simple-struct-type))
(define _procedure? (instance? procedure-type))

(define (import-string x) 
  (unless (_string? x)
    (error "tried to import non-string" x))
  (_field0 x))

(define (import-value x)
  (cond
    ((number? x) x)
    ((char? x) x)
    ((_symbol? x) (_field0 x))
    ((symbol? x) x)
    ((_procedure? x) x)
    ((_string? x) (import-string x))
    ((eqv? _nil x) '())
    ((eqv? _true x) #t)
    ((eqv? _false x) #f)
    ((_bytevector? x) (_field0 x))
    ((_vector? x) (_field0 x))
    ((_pair? x) (cons (import-value (_field0 x)) (import-value (_field1 x))))
    (else
      (error "Unsupported primitive import" x))))

(define (export-value x)
  (cond
    ((number? x) x)
    ((boolean? x) (boolean x))
    ((eof-object? x) _eof-object)
    ((char? x) x)
    ((null? x) _nil)
    ((string? x) (_rib x 0 string-type))
    ((bytevector? x) (_rib x 0 bytevector-type))
    (else
      (error "Unsupported primitive object" x))))

(define (realize-ext procname proc args results)
  (lambda (vals stack)
    (let loop ((rest (if (eq? args #t) vals args))
               (cur '())
               (stack stack))
      (unless (or (not (number? args)) (= vals args))
        (error "FIXME: Wrong arg count" procname vals))
      (if (= rest 0)
          (case results
            ((1)
             (let ((r (apply proc cur)))
              (_cons (export-value r) stack)))
            ((0)
             (apply proc cur)
             (_cons _false stack))
            (else
              (error "Unknown results count" procname results)))
          (loop (- rest 1)
                (cons (import-value (_car stack)) cur)
                (_cdr stack))))))

(define (encode-constant opnd intern!)
  (let ((v (cond
             ((number? opnd) opnd)
             ((char? opnd) opnd)
             ((null? opnd) _nil)
             ((boolean? opnd) (if opnd _true _false))
             ((string? opnd) (_rib opnd 0 string-type))
             ((pair? opnd) (_cons (encode-constant (car opnd) intern!)
                                  (encode-constant (cdr opnd) intern!)))
             ((_procedure? opnd) opnd)
             ((vector? opnd) (_rib opnd 0 vector-type))
             ((symbol? opnd) (intern! opnd))
             (else 
               (error "Unknown constant" opnd)
               opnd))))))

(define (bytevector-fill! bv b start end)
  (let loop ((idx start))
   (unless (= idx end)
     (bytevector-u8-set! bv idx b)
     (loop (+ idx 1)))))

(define (rvm code globals ext done-cb)
  (define not-yet (cons 0 0))
  (define output-result not-yet)
  (define externals (vector-map
                      (lambda (v) (realize-ext (vector-ref v 0)
                                               (vector-ref v 1)
                                               (vector-ref v 2)
                                               (vector-ref v 3)))
                      ext))
  (define external-names (vector-map (lambda (v) (vector-ref v 0)) ext))

  (define (intern! sym)
    (unless (symbol? sym)
      (error "Unknown symbol" sym))
    (or (hashtable-ref globals sym #f)
        (let ((r (_make-uninterned-symbol sym)))
         (hashtable-set! globals sym r)
         ;(write (list 'INTERN: sym)) (newline)
         r)))

  (define (get-var stack opnd)
    ;(write (list 'GET-VAR: opnd)) (newline)
    (_field0 
      (cond
        ((_rib? opnd) 
         opnd)
        ((symbol? opnd) (hashtable-ref globals opnd "NOT-FOUND!!"))
        (else (_list-tail stack opnd)))))

  (define (set-var stack opnd val)
    ;(write (list 'SET-VAR: opnd)) (newline)
    (_field0-set! 
      (cond
        ((_rib? opnd) opnd)
        ((symbol? opnd) (intern! opnd))
        (else (_list-tail stack opnd)))
      val))

  (define (run vals pc stack)
    (let ((instr (_field0 pc))
          (opnd (_field1 pc))
          (next (_field2 pc)))
      (case instr

        ((0) ;; jump/call
         (let* ((proc (get-var stack opnd))
                (code (_field0 proc)))
           ;(unless (_rib? code) (write (list 'CALL: code)) (newline))
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
                ;(write (list 'CALL: layout argnc nargs)) (newline)
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
                                       (if (< layout 0) 
                                           (_cons _nil stack)
                                           stack)
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
                 
                 (else 
                   ;; Calling external primitive
                   (let* ((id (- 0 code 12 1))
                          (proc (vector-ref externals id))
                          (stack (proc vals stack)))
                     (run #f
                          (if (_rib? next) ;; non-tail call?
                              next
                              (let ((cont (get-cont stack)))
                               (_field1-set! stack (_field0 cont))
                               (_field2 cont)))
                          stack))))

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
         (let ((v (encode-constant opnd intern!)))
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
                        (NEVERLAND-getchar     18)
                        (NEVERLAND-putchar     19)
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
                        (vec=        33)
                        (error       34)
                        (string->symbol 35)
                        (procedure?  36)
                        (ht-new      37)
                        (hashtable-set! 38)
                        (hashtable-entries 39)
                        (hashtable-ref 40)
                        (hashtable-keys 41)
                        (hashtable-size 42)
                        (symbol->string 43)
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
                     ;(write (list 'EQV: x y '=> (eqv? x y))) (newline)
                     (when (or (symbol? x) (symbol? y))
                       (error "Raw symbol" x y))
                     (boolean (eqv? x y))))
            (prim2 (lambda (x y) (boolean (< x y)))) ;; 13
            (primn +) ;; 14
            (primn -) ;; 15
            (primn *) ;; 16
            (prim2 quotient) ;; 17
            "NEVERLAND-getchar" ;; 18 was getchar
            "NEVERLAND-putchar" ;; 19 was putchar
            (prim1/term (lambda (x) ;; 20
                          (set! output-result x)
                          (done-cb output-result globals)))
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
            (prim3 (lambda (vec start end)
                     (cond
                       ((_bytevector? vec)
                        (let* ((bv (_field0 vec))
                               (newbv (if (= start -1)
                                          (bytevector-copy bv)
                                          (bytevector-copy bv start end))))
                          (_rib newbv 0 bytevector-type)))
                       (else
                         (error "Unimpl: vec-copy" vec)))))
            ;; 27: (vec-copy! tgt loc src start end)
            (prim5 (lambda (tgt loc src start end)
                     (cond
                       ((_string? tgt)
                        (string-copy! (_field0 tgt) loc (_field0 src)
                                      start end)
                        _true)
                       ((_vector? tgt)
                        (vector-copy! (_field0 tgt) loc (_field0 src)
                                      start end)
                        _true)
                       ((_bytevector? tgt)
                        (let ((bv1 (_field0 tgt))
                              (bv2 (_field0 src)))
                          (bytevector-copy! bv1 loc bv2 start end))
                        _true)
                       (else
                         (error "Unimpl: vec-copy!")))))
            ;; 28: (vec-ref vec idx)
            (prim2 (lambda (vec idx)
                     (cond
                       ((_string? vec)
                        (string-ref (_field0 vec) idx))
                       ((_vector? vec)
                        (vector-ref (_field0 vec) idx))
                       ((_simple-struct? vec)
                        (vector-ref (_field0 vec) (+ 1 idx)))
                       ((_bytevector? vec)
                        (let ((bv (_field0 vec)))
                         (bytevector-u8-ref bv idx)))
                       (else
                         (error "Unimpl: vec-ref")))))
            ;; 29: (vec-set! vec idx obj)
            (prim3 (lambda (vec idx obj)
                     (cond
                       ((_vector? vec)
                        (vector-set! (_field0 vec) idx obj)
                        _true)
                       ((_simple-struct? vec)
                        ;(write (list 'VEC: vec idx obj)) (newline)
                        (vector-set! (_field0 vec) (+ 1 idx) obj)
                        _true)
                       ((_bytevector? vec)
                        (let ((bv (_field0 vec)))
                         (bytevector-u8-set! bv idx obj))
                        _true)
                       (else
                         (error "Unimpl: vec-set!")))))
            ;; 30: (vec-new tag k)
            (prim2 (lambda (tag k)
                     (cond
                       ((= tag vector-type)
                        ;(write (list 'VECNEW: k)) (newline)
                        (_rib (make-vector k) 0 vector-type))
                       ((= tag simple-struct-type)
                        (_rib (make-vector (+ k 1)) 0 simple-struct-type))
                       ((= tag string-type)
                        (_rib (make-string k) 0 string-type))
                       ((= tag bytevector-type)
                        (let ((bv (make-bytevector k)))
                         (_rib bv 0 bytevector-type)))
                       (else
                         (error "Unimpl: vec-new" tag)))))
            ;; 31: (vec-length vec)
            (prim1 (lambda (vec)
                     (cond
                       ((_string? vec)
                        (string-length (_field0 vec)))
                       ((_vector? vec)
                        (vector-length (_field0 vec)))
                       ((_bytevector? vec)
                        (let ((bv (_field0 vec)))
                         (bytevector-length bv)))
                       (else
                         (error "Unimpl: vec-length")))))
            ;; 32: (vec-fill! vec obj from to)
            (prim4 (lambda (vec obj from to)
                     (cond
                       ((_string? vec)
                        (string-fill! (_field0 vec) obj from to)
                        _true)
                       ((_vector? vec)
                        (vector-fill! (_field0 vec) obj from to)
                        _true)
                       ((_bytevector? vec)
                        (let ((bv (_field0 vec)))
                         (bytevector-fill! bv obj from to)
                         _true))
                       (else
                         (error "Unimpl: vec-fill!")))))
            ;; 33: (vec= x y)
            (prim2 (lambda (x y)
                     (cond
                       ((_string? x)
                        (if (string=? (_field0 x) (_field0 y))
                            _true
                            _false))
                       (else
                         (error "Unimpl: vec=")))))
            ;; 34: (error . x)
            (primn (lambda x
                     (error "Error" x)))
            ;; 35: (string->symbol str)
            (prim1 (lambda (str)
                     (unless (= string-type (_field2 str))
                       (error "String required" str))
                     (let* ((name (_field0 str))
                            (namesym (string->symbol name)))
                       (intern! namesym))))
            ;; 36: (procedure? x)
            (prim1 (lambda (x)
                     ;(write (list 'PROCEDURE-FIXME: x)) (newline)
                     _false))
            ;; 37: (ht-new type)
            (prim1 (lambda (x)
                     (_rib ((case x
                              ((0) make-eq-hashtable)
                              ((1) make-eqv-hashtable)
                              ((2) make-integer-hashtable)
                              ((3) make-string-hashtable)
                              ((4) make-symbol-hashtable)))
                           x
                           hashtable-type)))
            ;; 38: (hashtable-set! ht key obj)
            (prim3 (lambda (ht key obj)
                     (hashtable-set! (_field0 ht) key obj)
                     _true))
            ;; 39: (hashtable-entries ht)
            (prim1 (lambda (ht)
                     (call-with-values
                       (lambda () (hashtable-entries (_field0 ht)))
                       (lambda (keys vals)
                         (let ((v1 (_rib keys 0 vector-type))
                               (v2 (_rib vals 0 vector-type)))
                           (_rib (_cons v1 (_cons v2 _nil)) 0 values-type))))))
            ;; 40: (hashtable-ref ht key default)
            (prim3 (lambda (ht key default)
                     (hashtable-ref (_field0 ht) key default)))
            ;; 41: (hashtable-keys ht)
            (prim1 (lambda (ht)
                     (_rib (hashtable-keys (_field0 ht)) 0 vector-type)))
            ;; 42: (hashtable-size ht)
            (prim1 (lambda (ht)
                     (hashtable-size (_field0 ht))))
            ;; 43: (symbol->string sym)
            (prim1 (lambda (sym)
                     (unless (= (_field2 sym) symbol-type)
                       (error "Symbol required" sym))
                     (_rib (symbol->string (_field0 sym)) 0 string-type)))))

  ;; Enter primitives
  (for-each (lambda (e)
              (let ((sym (car e))
                    (code (cadr e)))
                (set-var "unused" sym 
                         (_rib code _nil procedure-type))))
            primitives0)

  (set-var "unused" 'apply-values (_rib -12 _nil procedure-type))

  (let ((offset (+ -12 -1)))
   (let loop ((idx 0))
    (unless (= (vector-length external-names) idx)
      (let* ((name (vector-ref external-names idx))
             (id (- offset idx)))
        (set-var "unused" name (_rib id _nil procedure-type)))
      (loop (+ idx 1)))))

  (hashtable-set! globals 'false _false)
  (hashtable-set! globals 'true _true)
  (hashtable-set! globals 'nil _nil)
  (hashtable-set! globals '_eof-object _eof-object)

  ;; Start
  
  (run #f ;; No values
       (_field2 (_field0 code)) ;; instruction stream of main procedure
       (_rib 0 0 (_rib 6 0 0))) ;; primordial continuation = halt
  (when (eq? not-yet output-result)
    (done-cb #t globals))
   ) 


)
