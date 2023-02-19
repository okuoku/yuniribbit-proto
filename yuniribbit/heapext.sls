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
       (let* ((bv (_field0 vec))
              (newbv (if (= start -1)
                         (bytevector-copy bv)
                         (bytevector-copy bv start end))))
         (_wrap-bytevector newbv)))
      (else
        (error "Unimpl: vec-copy" vec))))

  (define (vec-copy! tgt loc src start end) 
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
        (error "Unimpl: vec-copy!"))))

  (define (vec-ref vec idx) 
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
        (error "Unimpl: vec-ref"))))

  (define (vec-set! vec idx obj) 
    (cond
      ((_vector? vec)
       (vector-set! (_field0 vec) idx obj)
       _true)
      ((_simple-struct? vec)
       (vector-set! (_field0 vec) (+ 1 idx) obj)
       _true)
      ((_bytevector? vec)
       (let ((bv (_field0 vec)))
        (bytevector-u8-set! bv idx obj))
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
       (string-length (_field0 vec)))
      ((_vector? vec)
       (vector-length (_field0 vec)))
      ((_bytevector? vec)
       (let ((bv (_field0 vec)))
        (bytevector-length bv)))
      (else
        (error "Unimpl: vec-length"))))

  (define (vec-fill! vec obj from to) 
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
        (error "Unimpl: vec-fill!"))))

  (define (vec= x y) 
    (cond
      ((_string? x)
       (if (string=? (_field0 x) (_field0 y))
           _true
           _false))
      (else
        (error "Unimpl: vec="))))

  (define (ht-new x) 
    (_wrap-hashtable ((case x
                        ((0) make-eq-hashtable)
                        ((1) make-eqv-hashtable)
                        ((2) make-integer-hashtable)
                        ((3) make-string-hashtable)
                        ((4) make-symbol-hashtable)))
                     x))

  (define (ht-set! ht key obj) 
    (hashtable-set! (_field0 ht) key obj)
    _true)

  (define (ht-entries ht) 
    (call-with-values
      (lambda () (hashtable-entries (_field0 ht)))
      (lambda (keys vals)
        (let ((v1 (_wrap-vector keys))
              (v2 (_wrap-vector vals)))
          (_rib (_cons v1 (_cons v2 _nil)) 0 values-type)))))

  (define (ht-ref ht key default) 
    (hashtable-ref (_field0 ht) key default))

  (define (ht-keys ht) 
    (_wrap-vector (hashtable-keys (_field0 ht))))

  (define (ht-size ht) 
    (hashtable-size (_field0 ht)))

  (define (heapext-ops)
    (vector
      (vector 'vec-copy vec-copy 3 1)
      (vector 'vec-copy! vec-copy! 5 1)
      (vector 'vec-ref vec-ref 2 1)
      (vector 'vec-set! vec-set! 3 1)
      (vector 'vec-new vec-new 2 1)
      (vector 'vec-length vec-length 1 1)
      (vector 'vec-fill! vec-fill! 4 1)
      (vector 'vec= vec= 2 1)
      (vector 'ht-new ht-new 1 1)
      (vector 'hashtable-set! ht-set! 3 1)
      (vector 'hashtable-entries ht-entries 1 1)
      (vector 'hashtable-ref ht-ref 3 1)
      (vector 'hashtable-keys ht-keys 1 1)
      (vector 'hashtable-size ht-size 1 1)))

  )
