(library (yuniribbit heapcore)
  (export 
    ;; Type marker shared with rsc and rvm
    procedure-type
    values-type

    ;; Core ops
    _rib?
    _rib
    _field0
    _field1
    _field2
    _field0-set!
    _field1-set!
    _field2-set!

    ;; Singleton objects
    _false _true _nil _eof-object
    ;; Types
    _pair?
    _cons _car _cdr
    _symbol?
    _vector?
    _simple-struct?
    _procedure?
    
    ;; Type constructors
    _make-uninterned-symbol
    _wrap-vector
    _wrap-simple-struct
    _wrap-hashtable
    _unwrap-vector
    _unwrap-simple-struct
    _unwrap-hashtable
    _unwrap-symbol/value
    _unwrap-symbol/name
    )
  (import (yuni scheme))


  (define pair-type      0)
  (define procedure-type 1)
  (define symbol-type    2)
  ;(define string-type    3)
  (define vector-type    4)
  (define singleton-type 5)
  (define values-type    6) ;; yuniribbit (lis 0 TYPE)
  ;; 7 was char-type
  ;(define bytevector-type 8) ;; yuniribbit
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

  (define _symbol? (instance? symbol-type))
  (define _vector? (instance? vector-type))
  (define _simple-struct? (instance? simple-struct-type))
  (define _hashtable? (instance? hashtable-type))
  (define _procedure? (instance? procedure-type))


  (define (_make-uninterned-symbol str)
    ;(_rib _false str symbol-type)
    (_rib str str symbol-type) ;; debug
    )
  (define (_wrap-vector x) (_rib x 0 vector-type))
  (define (_wrap-simple-struct x) (_rib x 0 simple-struct-type))
  (define (_wrap-hashtable x type) (_rib x type hashtable-type))
  (define (_unwrap-vector x) (_field0 x))
  (define (_unwrap-simple-struct x) (_field0 x))
  (define (_unwrap-hashtable x) (_field0 x))
  (define (_unwrap-symbol/value x) (_field0 x))
  (define (_unwrap-symbol/name x) (_field1 x))


  ;; Singleton object instances
  (define _false (_rib "false" 0 singleton-type))
  (define _true  (_rib "true" 0 singleton-type))
  (define _nil   (_rib "nil" 0 singleton-type))
  (define _eof-object (_rib "eof-object" 0 singleton-type))
  )
