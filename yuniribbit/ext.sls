(library (yuniribbit ext)
  (export ext-functions-vector)
  (import (yuni scheme))


  ;; yuni r7c std library specific primitives

  (define (pexact proc)
    (lambda args (exact (apply proc args))))
  (define (pexact2 proc)
    (lambda args
      (call-with-values (lambda () (apply proc args))
                        (lambda (a b) (values (exact a) (exact b))))))
  (define (pinexact proc)
    (lambda args (inexact (apply proc args))))
  (define (pinexact2 proc)
    (lambda args
      (call-with-values (lambda () (apply proc args))
                        (lambda (a b) (values (inexact a) (inexact b))))))

  (define $flonum? inexact?)

  (define $fx= =)
  (define $fx<= <=)
  (define $fx>= >=)
  (define $fx< <)
  (define $fx> >)

  (define $fx+ +)
  (define $fx- -)
  (define $fx* *)
  (define ($fx/ a b) (exact (/ a b)))
  (define $fx->fl inexact)
  (define ($fx-expt a b) (exact (expt a b)))
  (define $fx-floor/ (pexact2 floor/))
  (define $fx-truncate/ (pexact2 truncate/))

  (define $fl-nan? nan?)
  (define $fl-finite? finite?)
  ;; FIXME: We should have this one in (yuni scheme)
  (define (xinfinite? val)
    (cond
      ((finite? val) #f)
      ((nan? val) #f)
      (else #t)))

  (define $fl-infinite? xinfinite?)
  (define $fl= =)
  (define $fl<= <=)
  (define $fl>= >=)
  (define $fl< <)
  (define $fl> >)
  (define $fl+ (pinexact +))
  (define $fl- (pinexact -))
  (define $fl* (pinexact *))
  (define $fl/ (pinexact /))
  (define $fl->fx exact)
  (define $fl-expt (pinexact expt))
  (define $fl-floor floor)
  (define $fl-ceiling ceiling)
  (define $fl-truncate truncate)
  (define $fl-round round)
  (define $fl-acos acos)
  (define $fl-asin asin)
  (define $fl-atan atan)
  (define $fl-atan2 atan)
  (define $fl-cos cos)
  (define $fl-sin sin)
  (define $fl-tan tan)
  (define $fl-exp exp)
  (define $fl-log log)
  (define $fl-loge log)
  (define $fl-sqrt sqrt)
  (define $fl-floor/ (pinexact2 floor/))
  (define $fl-truncate/ (pinexact2 truncate/))


  ;; filehandle (copy&pasted from (yunivm util compatlibs))
  (define fh-stdin #f)
  (define fh-stdout #f)
  (define fh-stderr #f)

  (define fh-vec #f)

  (define (fh-alloc)
    (unless fh-vec
      (set! fh-vec (make-vector 10 #f)))
    (let ((len (vector-length fh-vec)))
     (let loop ((idx 0))
      (cond
        ((= idx len)
         (set! fh-vec (vector-append fh-vec (make-vector 5 #f)))
         idx)
        ((vector-ref fh-vec idx)
         (loop (+ idx 1)))
        (else
          idx)))))

  (define (fh-free! idx)
    (vector-set! fh-vec idx #f))

  (define (fh-ref fh)
    (vector-ref fh-vec fh))

  (define (fh-set! obj)
    (let ((idx (fh-alloc)))
     (vector-set! fh-vec idx obj)
     idx))

  (define (filehandle-init!)
    (let ((stdin (fh-set! (current-input-port)))
          (stdout (fh-set! (current-output-port)))
          (stderr (fh-set! (current-error-port))))
      (set! fh-stdin stdin)
      (set! fh-stdout stdout)
      (set! fh-stderr stderr)))

  (define (filehandle-open/input filename)
    (fh-set! (open-binary-input-file filename)))

  (define (filehandle-open/output filename)
    (fh-set! (open-binary-output-file filename)))

  (define (filehandle-close fh)
    (close-port (fh-ref fh))
    (fh-free! fh)
    #t)

  (define buf-stdin #f)
  (define (filehandle-read!/stdin fh bv offs len)
    (unless buf-stdin
      (let ((str (read-string 4096 (fh-ref fh))))
       (set! buf-stdin
         (if (eof-object? str) (eof-object) (open-input-bytevector
                                              (string->utf8 str))))))
    (if (eof-object? buf-stdin)
        0
        (let ((r (read-bytevector! bv buf-stdin offs (+ offs len))))
         (cond
           ((eof-object? r)
            (set! buf-stdin #f)
            (filehandle-read!/stdin fh bv offs len))
           (else r)))))

  (define (filehandle-write/stdout fh bv offs len)
    (write-string
      (utf8->string (bytevector-copy bv offs (+ offs len)))
      (fh-ref fh)))

  (define (filehandle-read!/file fh bv offs len) ;; => len
    (let ((r (read-bytevector! bv (fh-ref fh) offs (+ offs len))))
     (if (eof-object? r) 0 r)))

  (define (filehandle-write/file fh bv offs len)
    ;; FIXME: Always success???
    (write-bytevector bv (fh-ref fh) offs (+ offs len)))

  (define (filehandle-read! fh bv offs len)
    (if (= fh fh-stdin)
        (filehandle-read!/stdin fh bv offs len)
        (filehandle-read!/file fh bv offs len)))

  (define (filehandle-write fh bv offs len)
    (if (or (= fh fh-stdout) (= fh fh-stderr))
        (filehandle-write/stdout fh bv offs len)
        (filehandle-write/file fh bv offs len))
    #t)

  (define (filehandle-flush fh)
    (flush-output-port (fh-ref fh))
    #t)

  (define (filehandle-stdin) fh-stdin)
  (define (filehandle-stdout) fh-stdout)
  (define (filehandle-stderr) fh-stderr)


  ;; Export
  (define ext-library-initialized #f)
  (define (ext-functions-vector)
    (unless ext-library-initialized
      (filehandle-init!)
      (set! ext-library-initialized #t))

    (vector
      ;$flonum?
      (vector '$flonum? $flonum? 1 1)
      ;$fx=
      (vector '$fx= $fx= #t 1)
      ;$fx<=
      (vector '$fx<= $fx<= #t 1)
      ;$fx>=
      (vector '$fx>= $fx>= #t 1)
      ;$fx<
      (vector '$fx< $fx< #t 1)
      ;$fx>
      (vector '$fx> $fx> #t 1)
      ;$fx+
      (vector '$fx+ $fx+ #t 1)
      ;$fx-
      (vector '$fx- $fx- #t 1)
      ;$fx*
      (vector '$fx* $fx* #t 1)
      ;$fx/
      (vector '$fx/ $fx/ #t 1)
      ;$fx->fl
      (vector '$fx->fl $fx->fl #t 1)
      ;$fx-expt
      (vector '$fx-expt $fx-expt #t 1)
      ;$fx-floor/
      (vector '$fx-floor/ $fx-floor/ #t 1)
      ;$fx-truncate/
      (vector '$fx-truncate/ $fx-truncate/ #t 1)
      ;$fl-nan?
      (vector '$fl-nan? $fl-nan? #t 1)
      ;$fl-finite?
      (vector '$fl-finite? $fl-finite? #t 1)
      ;$fl-infinite?
      (vector '$fl-infinite? $fl-infinite? #t 1)
      ;$fl=
      (vector '$fl= $fl= #t 1)
      ;$fl<=
      (vector '$fl<= $fl<= #t 1)
      ;$fl>=
      (vector '$fl>= $fl>= #t 1)
      ;$fl<
      (vector '$fl< $fl< #t 1)
      ;$fl>
      (vector '$fl> $fl> #t 1)
      ;$fl+
      (vector '$fl+ $fl+ #t 1)
      ;$fl-
      (vector '$fl- $fl- #t 1)
      ;$fl*
      (vector '$fl* $fl* #t 1)
      ;$fl/
      (vector '$fl/ $fl/ #t 1)
      ;$fl->fx
      (vector '$fl->fx $fl->fx #t 1)
      ;$fl-expt
      (vector '$fl-expt $fl-expt #t 1)
      ;$fl-floor
      (vector '$fl-floor $fl-floor #t 1)
      ;$fl-ceiling
      (vector '$fl-ceiling $fl-ceiling #t 1)
      ;$fl-truncate
      (vector '$fl-truncate $fl-truncate #t 1)
      ;$fl-round
      (vector '$fl-round $fl-round #t 1)
      ;$fl-acos
      (vector '$fl-acos $fl-acos #t 1)
      ;$fl-asin
      (vector '$fl-asin $fl-asin #t 1)
      ;$fl-atan
      (vector '$fl-atan $fl-atan #t 1)
      ;$fl-atan2
      (vector '$fl-atan2 $fl-atan2 #t 1)
      ;$fl-cos
      (vector '$fl-cos $fl-cos #t 1)
      ;$fl-sin
      (vector '$fl-sin $fl-sin #t 1)
      ;$fl-tan
      (vector '$fl-tan $fl-tan #t 1)
      ;$fl-exp
      (vector '$fl-exp $fl-exp #t 1)
      ;$fl-log
      (vector '$fl-log $fl-log #t 1)
      ;$fl-loge
      (vector '$fl-loge $fl-loge #t 1)
      ;$fl-sqrt
      (vector '$fl-sqrt $fl-sqrt #t 1)
      ;$fl-floor/
      (vector '$fl-floor/ $fl-floor/ #t 1)
      ;$fl-truncate/
      (vector '$fl-truncate/ $fl-truncate/ #t 1)

      ;(filehandle-open/input filename)
      (vector 'filehandle-open/input filehandle-open/input 1 1)
      ;(filehandle-open/output filename)
      (vector 'filehandle-open/output filehandle-open/output 1 1)
      ;(filehandle-close fh)
      (vector 'filehandle-close filehandle-close 1 1)
      ;(filehandle-read! fh bv offs len)
      (vector 'filehandle-read! filehandle-read! 4 1)
      ;(filehandle-write fh bv offs len)
      (vector 'filehandle-write filehandle-write 4 1)
      ;(filehandle-flush fh)
      (vector 'filehandle-flush filehandle-flush 1 1)
      ;(filehandle-stdin)
      (vector 'filehandle-stdin filehandle-stdin 0 1)
      ;(filehandle-stdout)
      (vector 'filehandle-stdout filehandle-stdout 0 1)
      ;(filehandle-stderr)
      (vector 'filehandle-stderr filehandle-stderr 0 1)))
  )
