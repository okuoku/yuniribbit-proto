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

  (define ($$number->string num . radix?) 
    (if (null? radix?)
        ($$number->string num 10)
        (number->string num (car radix?))))

  (define ($fx/ a b) (exact (/ a b)))
  (define ($fx-expt a b) (exact (expt a b)))
  (define $fx-floor/ (pexact2 floor/))
  (define $fx-truncate/ (pexact2 truncate/))

  ;; FIXME: We should have this one in (yuni scheme)
  (define (xinfinite? val)
    (cond
      ((finite? val) #f)
      ((nan? val) #f)
      (else #t)))

  (define $fl+ (pinexact +))
  (define $fl- (pinexact -))
  (define $fl* (pinexact *))
  (define $fl/ (pinexact /))
  (define $fl-expt (pinexact expt))
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
      ;; Char 
      (vector 'char->integer char->integer 1 1)
      (vector 'integer->char integer->char 1 1)

      ;; String constructors
      (vector 'utf8->string utf8->string #t 1)
      (vector 'vector->string vector->string #t 1)
      (vector 'list->string list->string 1 1)
      ;; Writer
      (vector 'number->string $$number->string #t 1)
      ;; Reader
      (vector 'string->number string->number #t 1)
      ;; String conversion
      (vector 'string->utf8 string->utf8 #t 1)

      ;; Math
      (vector '< < #t 1)
      (vector '+ + #t 1)
      (vector '- - #t 1)
      (vector '* * #t 1)
      (vector '= = #t 1)
      (vector '> > #t 1)
      (vector '<= <= #t 1)
      (vector '>= >= #t 1)
      (vector '$fx= = #t 1)
      (vector '$fx<= <= #t 1)
      (vector '$fx>= >= #t 1)
      (vector '$fx< < #t 1)
      (vector '$fx> > #t 1)
      (vector '$fx+ + #t 1)
      (vector '$fx- - #t 1)
      (vector '$fx* * #t 1)
      (vector '$fx/ $fx/ #t 1)
      (vector '$fx->fl inexact #t 1)
      (vector '$fx-expt $fx-expt #t 1)
      (vector '$fx-floor/ $fx-floor/ #t 2)
      (vector '$fx-truncate/ $fx-truncate/ #t 2)
      (vector '$fl-nan? nan? #t 1)
      (vector '$fl-finite? finite? #t 1)
      (vector '$fl-infinite? xinfinite? #t 1)
      (vector '$fl= = #t 1)
      (vector '$fl<= <= #t 1)
      (vector '$fl>= >= #t 1)
      (vector '$fl< < #t 1)
      (vector '$fl> > #t 1)
      (vector '$fl+ $fl+ #t 1)
      (vector '$fl- $fl- #t 1)
      (vector '$fl* $fl* #t 1)
      (vector '$fl/ $fl/ #t 1)
      (vector '$fl->fx exact #t 1)
      (vector '$fl-expt $fl-expt #t 1)
      (vector '$fl-floor floor #t 1)
      (vector '$fl-ceiling ceiling #t 1)
      (vector '$fl-truncate truncate #t 1)
      (vector '$fl-round round #t 1)
      (vector '$fl-acos acos #t 1)
      (vector '$fl-asin asin #t 1)
      (vector '$fl-atan atan #t 1)
      (vector '$fl-atan2 atan #t 1)
      (vector '$fl-cos cos #t 1)
      (vector '$fl-sin sin #t 1)
      (vector '$fl-tan tan #t 1)
      (vector '$fl-exp exp #t 1)
      (vector '$fl-log log #t 1)
      (vector '$fl-loge log #t 1)
      (vector '$fl-sqrt sqrt #t 1)
      (vector '$fl-floor/ $fl-floor/ #t 2)
      (vector '$fl-truncate/ $fl-truncate/ #t 2)

      ;; Files
      (vector 'file-exists? file-exists? 1 1)
      (vector 'delete-file delete-file 1 1)
      (vector 'filehandle-open/input filehandle-open/input 1 1)
      (vector 'filehandle-open/output filehandle-open/output 1 1)
      (vector 'filehandle-close filehandle-close 1 1)
      (vector 'filehandle-read! filehandle-read! 4 1)
      (vector 'filehandle-write filehandle-write 4 1)
      (vector 'filehandle-flush filehandle-flush 1 1)
      (vector 'filehandle-stdin filehandle-stdin 0 1)
      (vector 'filehandle-stdout filehandle-stdout 0 1)
      (vector 'filehandle-stderr filehandle-stderr 0 1)))
  )
