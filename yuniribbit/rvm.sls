(library (yuniribbit rvm)
         (export rvm)
         (import (yuni scheme)
                 (yuni hashtables)
                 (yuniribbit ext)
                 (yuniribbit heapext)
                 (yuniribbit heapcore))


(define (_list-tail lst i)
  (if (< 0 i)
      (_list-tail (_cdr lst) (- i 1))
      lst))

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

(define (prim2 f)
  (lambda (vals stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (_cons (f x y) stack))))

(define (prim2/term f)
  (lambda (vals stack)
    (let* ((y (_car stack)) (stack (_cdr stack))
           (x (_car stack)) (stack (_cdr stack)))
      (f x y)
      #f)))

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

(define (import-string x) 
  (unless (_string? x)
    (error "tried to import non-string" x))
  (_unwrap-string x))

(define (import-value x)
  (cond
    ((number? x) x)
    ((char? x) x)
    ((_symbol? x) (_unwrap-symbol/name x))
    ((symbol? x) x)
    ((_procedure? x) x)
    ((_string? x) (import-string x))
    ((eqv? _nil x) '())
    ((eqv? _true x) #t)
    ((eqv? _false x) #f)
    ((_bytevector? x) (_unwrap-bytevector x))
    ((_vector? x) (_unwrap-vector x))
    ((_pair? x) (cons (import-value (_car x)) (import-value (_cdr x))))
    (else
      (error "Unsupported primitive import" x))))

(define (export-value x)
  (cond
    ((pair? x) (_cons (export-value (car x)) (export-value (cdr x))))
    ((number? x) x)
    ((boolean? x) (boolean x))
    ((eof-object? x) _eof-object)
    ((char? x) x)
    ((null? x) _nil)
    ((string? x) (_wrap-string x))
    ((bytevector? x) (_wrap-bytevector x))
    (else
      (error "Unsupported primitive object" x))))

(define (realize-local procname proc args results)
  (when (eq? args #f)
    (unless (eq? results #f)
      (error "not cooked" procname)))
  (unless (eq? args #f)
    (unless (= 1 results)
      (error "local primitive must return 1 value" procname)))

  (if (not args)
      proc
      (case args
        ((1) (prim1 proc))
        ((2) (prim2 proc))
        ((3) (prim3 proc))
        ((4) (prim4 proc))
        ((5) (prim5 proc))
        ((#t) (primn proc))
        (else (error "Unknown args count" procname args)))))

(define (realize-ext procname proc args results)
  (lambda (vals stack)
    ;(write (list 'EXT: procname)) (newline)
    (let loop ((rest (if (eq? args #t) vals args))
               (cur '())
               (stack stack))
      (unless (or (not (number? args)) (= vals args))
        (error "FIXME: Wrong arg count" procname vals))
      (if (= rest 0)
          (case results
            ((2)
             (call-with-values 
               (lambda () (apply proc cur))
               (lambda res
                 (_cons (_rib (export-value res) 0 values-type) stack))))
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


(define (bytevector-fill! bv b start end)
  (let loop ((idx start))
   (unless (= idx end)
     (bytevector-u8-set! bv idx b)
     (loop (+ idx 1)))))

(define none (list "NOT-FOUND!!"))

(define (rvm code globals vmlocal done-cb) ;; => output-mode result globals
  ;; output-mode: 0 = terminate, 1 = exit-called, 2 = eval-term
  (define not-yet (cons 0 0))
  (define output-result not-yet)
  (define externals #f)
  (define external-names #f)
  (define ext (ext-functions-vector))

  (define (intern! sym)
    (unless (symbol? sym)
      (error "Unknown symbol" sym))
    (or (hashtable-ref globals sym #f)
        (let ((r (_make-uninterned-symbol sym)))
          (hashtable-set! globals sym r)
          ;(write (list 'INTERN: sym)) (newline)
          r)))

  (define (encode-constant opnd)
    (cond
      ((number? opnd) opnd)
      ((char? opnd) opnd)
      ((null? opnd) _nil)
      ((boolean? opnd) (if opnd _true _false))
      ((string? opnd) (_wrap-string opnd))
      ((pair? opnd) (_cons (encode-constant (car opnd))
                           (encode-constant (cdr opnd))))
      ((_procedure? opnd) opnd)
      ((vector? opnd) (_wrap-vector (vector-map encode-constant opnd)))
      ((symbol? opnd) (intern! opnd))
      (else 
        (error "Unknown constant" opnd)
        opnd)))


  (define (get-var stack opnd)
    ;(when (symbol? opnd) (write (list 'GET-VAR: opnd)) (newline))
    (_field0  ;; _unwrap-symbol/value
      (cond
        ((_rib? opnd) 
         opnd)
        ((symbol? opnd) 
         (let ((r (hashtable-ref globals opnd none)))
           (when (eq? r none)
             (error "Could not resolve symbol" opnd))
           r))
        (else (_list-tail stack opnd)))))

  (define (set-var stack opnd val)
    ;(when (symbol? opnd) (write (list 'SET-VAR: opnd)) (newline))
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
                    ;(write (list 'APV: v*)) (newline)
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
                     (if stack
                         (run #f
                              (if (_rib? next) ;; non-tail call?
                                  next
                                  (let ((cont (get-cont stack)))
                                   (_field1-set! stack (_field0 cont))
                                   (_field2 cont)))
                              stack)
                         'done))))

               ;; calling std primitive
               (error "Illegal primitive code" code)))))

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
         (let ((v (encode-constant opnd)))
           (run #f next (_cons v stack))))

        ((4) ;; if
         (run #f
              (if (eqv? (_car stack) _false) next opnd)
              (_cdr stack)))
        ((5) ;; enter (yuniribbit)
         (run opnd next stack))
        (else ;; halt
          #f))))

  ;; ------------------
  ;; --- primitives ---
  ;; ------------------


  (define local-primitives ;; raw-in/raw-out
    (vector
      (vector 'rib _rib 3 1)
      (vector 'id (lambda (x) x) 1 1)
      (vector 'arg1 (lambda (vals stack) (_cdr stack)) #f #f)
      (vector 'arg2 (lambda (y x) x) 2 1) 
      (vector 'close (lambda (vals stack)
                       (let* ((x (_car stack)) (stack (_cdr stack)))
                         (_cons (_rib (_field0 x) stack procedure-type) stack)))
              #f #f)
      (vector 'rib? (lambda (x) (boolean (_rib? x))) 1 1)

      (vector 'field0 _field0 1 1)
      (vector 'field1 _field1 1 1)
      (vector 'field2 _field2 1 1)
      (vector 'field0-set! (lambda (x y) (_field0-set! x y) y) 2 1)
      (vector 'field1-set! (lambda (x y) (_field1-set! x y) y) 2 1)
      (vector 'field2-set! (lambda (x y) (_field2-set! x y) y) 2 1)
      (vector 'eqv? (lambda (x y) 
                      (when (or (symbol? x) (symbol? y))
                        (error "raw symbol" x y))
                      (boolean (eqv? x y))) 2 1)
      (vector '$vm-exit (prim2/term (lambda (mode x) 
                                      (set! output-result x)
                                      (done-cb mode output-result globals)))
              #f #f)
      (vector 'values (lambda (vals stack)
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
              #f #f)
      (vector 'list->values (lambda (vals stack)
                              (_cons (_rib (_car stack) 0 values-type)
                                     (_cdr stack))) #f #f)
      (vector '$error/core (primn (lambda x
                              (error "Error" x))) #f #f)
      (vector 'string->symbol (lambda (str)
                                (unless (_string? str)
                                  (error "String required" str))
                                (let* ((name (_unwrap-string str))
                                       (namesym (string->symbol name)))
                                  (intern! namesym))) 1 1)
      (vector 'procedure? (lambda (x) (if (_procedure? x) _true _false)) 1 1)
      (vector 'symbol->string (lambda (sym)
                                (unless (_symbol? sym)
                                  (error "Symbol required" sym))
                                (_wrap-string (symbol->string (_unwrap-symbol/name sym)))) 1 1)
      (vector 'vminject import-value 1 1)
      (vector 'vmfetch encode-constant 1 1)))

  (define raw-primitives (vector-append local-primitives vmlocal (heapext-ops)))

  ;; Enter primitives

  (set-var "unused" 'apply-values (_rib -12 _nil procedure-type))

  (set! externals
    (vector-append
      ;; raw-in/raw-out primitives
      (vector-map
        (lambda (v) (realize-local (vector-ref v 0)
                                   (vector-ref v 1)
                                   (vector-ref v 2)
                                   (vector-ref v 3)))
        raw-primitives) 
      ;; obj-in/obj-out primitives
      (vector-map
        (lambda (v) (realize-ext (vector-ref v 0)
                                 (vector-ref v 1)
                                 (vector-ref v 2)
                                 (vector-ref v 3)))
        ext)))

  (set! external-names
    (vector-map (lambda (v) (vector-ref v 0))
                (vector-append raw-primitives ext)))

  (let ((offset (+ -12 -1)))
    (let loop ((idx 0))
      (unless (= (vector-length external-names) idx)
        (let* ((name (vector-ref external-names idx))
               (id (- offset idx)))
          (set-var "unused" name (_rib id _nil procedure-type)))
        (loop (+ idx 1)))))

  (hashtable-set! globals '_eof-object _eof-object)

  ;; Start

  (run #f ;; No values
       (_field2 (_field0 code)) ;; instruction stream of main procedure
       (_rib 0 0 (_rib 6 0 0))) ;; primordial continuation = halt
  (when (eq? not-yet output-result)
    (done-cb 0 #t globals))
  ) 


)
