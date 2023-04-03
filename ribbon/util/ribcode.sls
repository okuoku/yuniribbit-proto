(library (ribbon util ribcode)
  (export ribcode-encode ribcode-decode)
  (import (yuni scheme)
          (yuni hashtables)
          (ribbon vmglue compiler))

  (define (ribcode-encode proc) ;; => #(out# rib# rosym# rwsym# tbl)
    (define None (list 'none))
    (define (makecnt)
      (vector
        ;; 0: Count
        0
        ;; 1: Offset (Unused)
        #f
        ;; 2: Hashtable(obj => Count)
        (make-eq-hashtable)))
    (define cnt-symtotal (makecnt))
    (define cnt-symrw (makecnt))
    (define cnt-misc (makecnt))
    (define cnt-rib (makecnt))

    (define (enter cnt obj)
      (let ((ht (vector-ref cnt 2))
            (curcnt (vector-ref cnt 0)))
        (let ((x (hashtable-ref ht obj None)))
         (when (eq? x None)
           (hashtable-set! ht obj curcnt)
           (vector-set! cnt 0 (+ curcnt 1))))))

    (define (ident cnt obj)
      (hashtable-ref (vector-ref cnt 2) obj #f))

    (define (total cnt)
      (vector-ref cnt 0))

    (define (objs cnt)
      (hashtable-keys (vector-ref cnt 2)))

    (define (enter-getref write? obj)
      (cond
        ((symbol? obj)
         (enter cnt-symtotal obj)
         (when write?
           (enter cnt-symrw obj)))
        (else
          (enter cnt-misc obj))))

    (define (enter-misc obj)
      (enter cnt-misc obj))

    (define (enter-op r)
      (unless (rib? r)
        (error "Invalid OP" r))
      (let ((opc (field0 r))
            (opr (field1 r))
            (next (field2 r)))
        (enter cnt-rib r)
        (enter cnt-misc opc)
        (cond
          ((= opc 0) ;; CALL/JMP
           (enter-getref #f opr))
          ((= opc 1) ;; SET
           (enter-getref #t opr))
          ((= opc 2) ;; GET
           (enter-getref #f opr))
          ((= opc 3) ;; CONST(for lambda)
           (enter-proc opr))
          ((= opc 4) ;; IF
           ;; FIXME: Turn this to tail-call?
           (unless (ident cnt-rib opr)
             (enter-op opr)))
          ((= opc 5) ;; Enter
           (enter-misc opr))
          ((= opc 6) ;; Const
           (enter-misc opr)))
        (cond
          ((number? next)
           (enter-misc next))
          (else
            (enter-op next)))))

    (define (enter-proc r)
      (unless (rib? r)
        (error "Invalid procedure" r))
      (enter cnt-rib r)
      (enter cnt-misc (field1 r)) ;; Nil
      (enter cnt-misc (field2 r)) ;; procedure-type
      (enter-code (field0 r)))

    (define (enter-code r)
      (enter cnt-rib r)
      (enter cnt-misc (field0 r)) ;; argc
      (enter-misc (field1 r)) ;; Zero(unused)
      (enter-op (field2 r)))

    ;; First, construct rib table
    (enter-proc proc)

    (let ((cnt-symro (makecnt)))
     ;; Construct symro table
     (let ((totals (objs cnt-symtotal)))
      (vector-for-each (lambda (s) 
                         (unless (ident cnt-symrw s)
                           (enter cnt-symro s)))
                       totals)
      ;; Construct total table
      ;; misc# rib# rosym# rwsym#
      (let* ((ht-final (make-eq-hashtable))
             (ht-syms (make-eq-hashtable))
             (offs-rib (total cnt-misc))
             (offs-rosym (+ offs-rib (* (total cnt-rib) 3)))
             (offs-rwsym (+ offs-rosym (total cnt-symro)))
             (tbl (make-vector (+ offs-rwsym (total cnt-symrw))))
             (idx 0)
             (xref (lambda (obj) (or (hashtable-ref ht-syms obj #f)
                                     (hashtable-ref ht-final obj #f)))))
        ;; Pass1: fill ht-final and tbl(except ribs)
        (vector-for-each (lambda (e) 
                           (vector-set! tbl idx e)
                           (hashtable-set! ht-final e idx)
                           (set! idx (+ idx 1)))
                         (objs cnt-misc))
        (vector-for-each (lambda (e)
                           (hashtable-set! ht-final e idx)
                           (set! idx (+ idx 3)))
                         (objs cnt-rib))
        ;; code symbols are stored in separate ht
        (vector-for-each (lambda (e)
                           (vector-set! tbl idx e)
                           (hashtable-set! ht-syms e idx)
                           (set! idx (+ idx 1)))
                         (objs cnt-symro))
        (vector-for-each (lambda (e)
                           (vector-set! tbl idx e)
                           (hashtable-set! ht-syms e idx)
                           (set! idx (+ idx 1)))
                         (objs cnt-symrw))
        ;; Pass2: Fill rib content
        (set! idx offs-rib)
        (vector-for-each (lambda (e)
                           (let ((f0 (field0 e))
                                 (f1 (field1 e))
                                 (f2 (field2 e)))
                             ;; Unless for constant op, filter symbol index
                             ;; if (= 6) might cause mistake for argc==6 
                             ;; lambdas but it shouldn't be a problem
                             (let ((m0 (if (and (number? f0) (= f0 6))
                                           (hashtable-ref ht-final f0 #f)
                                           (xref f0)))
                                   (m1 (xref f1))
                                   (m2 (xref f2)))
                               (vector-set! tbl idx m0)
                               (set! idx (+ idx 1))
                               (vector-set! tbl idx m1)
                               (set! idx (+ idx 1))
                               (vector-set! tbl idx m2)
                               (set! idx (+ idx 1)))))
                         (objs cnt-rib))
        ;; Final output
        (vector (hashtable-ref ht-final proc #f)
                offs-rib
                offs-rosym
                offs-rwsym
                tbl)))))

  ;; For debugging
  (define (ribcode-decode vec) ;; => rib
    ;; vec = #(out# rib# rosym# rwsym# tbl)
    (let* ((tbl (vector-ref vec 4))
           (out (vector-ref vec 0))
           (offs-rib (vector-ref vec 1))
           (offs-rosym (vector-ref vec 2))
           (ribcount (- offs-rosym offs-rib))
           (ribs (make-vector ribcount))
           (idx #f))
      (define (idx-isrib? idx)
        (and (<= offs-rib idx)
             (> offs-rosym idx)))
      (define (ribref i)
        (vector-ref ribs (truncate/ (- i offs-rib) 3)))
      (define (idxref i)
        (cond
          ((idx-isrib? i)
           (ribref i))
          (else
            (vector-ref tbl i))))
      ;; Generate output ribs
      (set! idx 0)
      (let loop ()
       (unless (= idx ribcount)
         (vector-set! ribs idx (rib #f #f #f))
         (set! idx (+ idx 1))
         (loop)))

      ;; Fill rib content
      (set! idx offs-rib)
      (let loop ()
       (unless (= idx offs-rosym)
         (let ((r (ribref idx)))
          (field0-set! r (idxref (vector-ref tbl idx)))
          (set! idx (+ idx 1))
          (field1-set! r (idxref (vector-ref tbl idx)))
          (set! idx (+ idx 1))
          (field2-set! r (idxref (vector-ref tbl idx)))
          (set! idx (+ idx 1))
          (loop))))

      (ribref out))))
