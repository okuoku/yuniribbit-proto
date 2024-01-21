(library (yuni compat hashtables)
  (export
    make-eq-hashtable      ;; 0
    make-eqv-hashtable     ;; 1
    make-integer-hashtable ;; 2
    make-string-hashtable  ;; 3
    make-symbol-hashtable  ;; 4
    hashtable-set!
    hashtable-entries
    hashtable-ref
    hashtable-keys
    hashtable-size

    hashtable-update!
    hashtable-for-each
    hashtable-fold
    )
  (import (yuni scheme) (rvm-primitives))

  (define (make-eq-hashtable) (ht-new 0))
  (define (make-eqv-hashtable) (ht-new 1))
  (define (make-integer-hashtable) (ht-new 2))
  (define (make-string-hashtable) (ht-new 3))
  (define (make-symbol-hashtable) (ht-new 4))

  (define (hashtable-update! ht key proc default)
    (hashtable-set! ht key
                    (proc (hashtable-ref ht key default))))

  (define (hashtable-for-each . x) (error "Unimpl."))
  (define (hashtable-fold . x) (error "Unimpl."))
  
  )
