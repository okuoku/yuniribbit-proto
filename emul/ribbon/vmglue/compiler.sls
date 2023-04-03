(library (ribbon vmglue compiler)
  (export rib rib? field0 field1 field2 vminject
          ;; For ribcode
          field0-set! field1-set! field2-set!)
  (import (yuni scheme)
          (yuniribbit heapcore))

  (define rib _rib)
  (define rib? _rib?)
  (define field0 _field0)
  (define field1 _field1)
  (define field2 _field2)
  (define field0-set! _field0-set!)
  (define field1-set! _field1-set!)
  (define field2-set! _field2-set!)
  (define (vmfetchcode x) x)
  (define (vminject x) x))
