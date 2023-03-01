(library (yuniribbit heapext)
  (export heapext-ops)
  (import (yuni scheme)
          (yuni hashtables)
          (yuniribbit heapcore))

  (define (bytevector-fill! bv b start end)
    (let loop ((idx start))
     (unless (= idx end)
       (bytevector-u8-set! bv idx b)
       (loop (+ idx 1)))))


  (define (vec-copy vec start end) 
    (cond
      ((_bytevector? vec)
       (let* ((bv (_unwrap-bytevector vec))
              (newbv (if (= start -1)
                         (bytevector-copy bv)
                         (bytevector-copy bv start end))))
         (_wrap-bytevector newbv)))
      (else
        (error "Unimpl: vec-copy" vec))))

  (define (vec-copy! tgt loc src start end) 
    (cond
      ((_string? tgt)
       (string-copy! (_unwrap-string tgt) loc (_unwrap-string src)
                     start end)
       _true)
      ((_vector? tgt)
       (vector-copy! (_unwrap-vector tgt) loc (_unwrap-vector src)
                     start end)
       _true)
      ((_bytevector? tgt)
       (let ((bv1 (_unwrap-bytevector tgt))
             (bv2 (_unwrap-bytevector src)))
         (bytevector-copy! bv1 loc bv2 start end))
       _true)
      (else
        (error "Unimpl: vec-copy!"))))

  (define (vec-ref vec idx) 
    (cond
      ((_string? vec)
       (string-ref (_unwrap-string vec) idx))
      ((_vector? vec)
       (vector-ref (_unwrap-vector vec) idx))
      ((_simple-struct? vec)
       (vector-ref (_unwrap-simple-struct vec) (+ 1 idx)))
      ((_bytevector? vec)
       (bytevector-u8-ref (_unwrap-bytevector vec) idx))
      (else
        (error "Unimpl: vec-ref"))))

  (define (vec-set! vec idx obj) 
    (cond
      ((_vector? vec)
       (vector-set! (_unwrap-vector vec) idx obj)
       _true)
      ((_simple-struct? vec)
       (vector-set! (_unwrap-simple-struct vec) (+ 1 idx) obj)
       _true)
      ((_bytevector? vec)
       (bytevector-u8-set! (_unwrap-bytevector vec) idx obj)
       _true)
      (else
        (error "Unimpl: vec-set!"))))

  (define (vec-new tag k) 
    (cond
      ((= tag 4)
       (_wrap-vector (make-vector k)))
      ((= tag 9)
       (_wrap-simple-struct (make-vector (+ k 1))))
      ((= tag 3)
       (_wrap-string (make-string k)))
      ((= tag 8)
       (_wrap-bytevector (make-bytevector k)))
      (else
        (error "Unimpl: vec-new" tag))))

  (define (vec-length vec) 
    (cond
      ((_string? vec)
       (string-length (_unwrap-string vec)))
      ((_vector? vec)
       (vector-length (_unwrap-vector vec)))
      ((_bytevector? vec)
       (bytevector-length (_unwrap-bytevector vec)))
      (else
        (error "Unimpl: vec-length"))))

  (define (vec-fill! vec obj from to) 
    (cond
      ((_string? vec)
       (string-fill! (_unwrap-string vec) obj from to)
       _true)
      ((_vector? vec)
       (vector-fill! (_unwrap-vector vec) obj from to)
       _true)
      ((_bytevector? vec)
       (bytevector-fill! (_unwrap-bytevector vec) obj from to)
        _true)
      (else
        (error "Unimpl: vec-fill!"))))

  (define (vec= x y) 
    (cond
      ((_string? x)
       (if (string=? (_unwrap-string x) (_unwrap-string y))
           _true
           _false))
      (else
        (error "Unimpl: vec="))))

  (define (vec-append x . y)
    (cond
      ((_string? x)
       (_wrap-string
         (apply string-append (_unwrap-string x)
                (map (lambda (e)
                       (unless (_string? e)
                         (error "String required" e))
                       (_unwrap-string e))
                     y))))
      (else
        (error "Unimpl: vec-append"))))

  (define (vec-subvec vec start end)
    (cond
      ((_string? vec)
       (_wrap-string (substring (_unwrap-string vec) start end)))
      (else
        (error "Unimpl: vec-subvec"))))

  (define (ht-new x) 
    (_wrap-hashtable ((case x
                        ((0) make-eq-hashtable)
                        ((1) make-eqv-hashtable)
                        ((2) make-integer-hashtable)
                        ((3) make-string-hashtable)
                        ((4) make-symbol-hashtable)
                        (else (error "Unknown hashtable type" x))))
                     x))

  (define (ht-set! ht key obj) 
    (hashtable-set! (_unwrap-hashtable ht) key obj)
    _true)

  (define (ht-entries ht) 
    (call-with-values
      (lambda () (hashtable-entries (_unwrap-hashtable ht)))
      (lambda (keys vals)
        (let ((v1 (_wrap-vector keys))
              (v2 (_wrap-vector vals)))
          (_rib (_cons v1 (_cons v2 _nil)) 0 values-type)))))

  (define (ht-ref ht key default) 
    (hashtable-ref (_unwrap-hashtable ht) key default))

  (define (ht-keys ht) 
    (_wrap-vector (hashtable-keys (_unwrap-hashtable ht))))

  (define (ht-size ht) 
    (hashtable-size (_unwrap-hashtable ht)))

  (define (%char? c)
    (if (char? c) _true _false))

  (define ($fixnum? x)
    (if (and (exact? x) (integer? x))
        _true
        _false))

  (define ($flonum? x)
    (if (inexact? x)
        _true
        _false))

  (define (heapext-ops)
    (vector
      ;; Primitive Type checks (not expressed with ribs)
      (vector 'char? %char? 1 1)
      (vector '$fixnum? $fixnum? 1 1)
      (vector '$flonum? $flonum? 1 1)

      ;; Vectors
      (vector 'vec-copy vec-copy 3 1)
      (vector 'vec-copy! vec-copy! 5 1)
      (vector 'vec-ref vec-ref 2 1)
      (vector 'vec-set! vec-set! 3 1)
      (vector 'vec-new vec-new 2 1)
      (vector 'vec-length vec-length 1 1)
      (vector 'vec-fill! vec-fill! 4 1)
      (vector 'vec-append vec-append #t 1)
      (vector 'vec-subvec vec-subvec 3 1)
      (vector 'vec= vec= 2 1)

      ;; Hashtables
      (vector 'ht-new ht-new 1 1)
      (vector 'hashtable-set! ht-set! 3 1)
      (vector 'hashtable-entries ht-entries 1 1)
      (vector 'hashtable-ref ht-ref 3 1)
      (vector 'hashtable-keys ht-keys 1 1)
      (vector 'hashtable-size ht-size 1 1)))

  )
