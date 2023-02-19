(library (yunife-yuni compat scheme)
         (export
           ;; Expander syntax
           define-syntax 
           syntax-rules ... => _ else
           syntax-error 
           
           ;; yunivm core syntax
           if when lambda begin 
           set! quote letrec*

           ;; Binding (yunife-yunivm runtime define)
           define 

           ;; Binding (yunife-yunivm runtime let)
           let let* letrec 

           ;; Binding (yunife-yunivm runtime let-values)
           let*-values let-values 

           ;; runtime syntax (yunife-yunivm runtime syntax)
           and or unless 
           case cond do 

           ;; runtime syntax (yunife-yunivm runtime qq)
           quasiquote unquote unquote-splicing 

           ;; Case-lambda (yunife-yunivm runtime case-lambda)
           case-lambda 

           ;; Unsupported (yunife-yunivm runtime unsupported)
           let-syntax letrec-syntax
           define-record-type define-values
           guard 
           parameterize

           ;; from R7RS draft 7
           * + - / < <= = > >= abs 
           append apply assoc assq assv 
           binary-port? boolean=? boolean? bytevector 
           bytevector-append bytevector-copy bytevector-copy! 
           bytevector-length bytevector-u8-ref bytevector-u8-set!
           bytevector? 
           caar cadr 
           call-with-current-continuation call/cc 
           call-with-port
           call-with-values 
           car cdar cddr cdr ceiling char->integer
           char<=?  char<? char=? char>=? char>? char? close-input-port
           close-output-port close-port complex? 
           cons current-error-port
           current-input-port current-output-port 
           dynamic-wind eof-object eof-object? eq?
           equal? eqv? 
           error error-object-irritants error-object-message error-object?
           even? exact exact-integer-sqrt exact-integer? exact? expt
           file-error?
           floor floor-quotient floor-remainder floor/ flush-output-port 
           for-each gcd
           get-output-bytevector get-output-string 
           inexact inexact? input-port-open? input-port? 
           integer->char integer? lcm
           length list list->string list->vector list-copy 
           list-ref list-set! list-tail list?
           make-bytevector make-list make-parameter 
           make-string make-vector map max member
           memq memv min modulo negative? newline not null? 
           number->string number? odd? open-input-bytevector 
           open-input-string open-output-bytevector
           open-output-string 
           output-port-open? output-port? pair?
           peek-char peek-u8 port? positive?  procedure? 
           quotient raise raise-continuable
           read-bytevector read-bytevector!
           read-char read-error? read-line read-string read-u8 
           real? remainder reverse
           round set-car! set-cdr! square string string->list string->number
           string->symbol string->utf8 string->vector string-append string-copy
           string-copy! string-fill! string-for-each 
           string-length string-map string-ref
           string-set! string<=? string<? 
           string=? string>=? string>? string? substring
           symbol->string symbol=? symbol? 
           textual-port? truncate
           truncate-quotient truncate-remainder truncate/
           utf8->string values vector vector->list vector->string
           vector-append vector-copy vector-copy! vector-fill! vector-for-each
           vector-length vector-map vector-ref vector-set!  vector? 
           with-exception-handler write-bytevector 
           write-char write-string write-u8 zero?


           ;; from R7RS draft 7
           caaaar caaadr caaar caadar caaddr caadr 
           cadaar cadadr cadar caddar cadddr caddr
           cdaaar cdaadr cdaar cdadar cdaddr cdadr 
           cddaar cddadr cddar cdddar cddddr cdddr

           ;; from R7RS draft 4
           call-with-input-file call-with-output-file delete-file file-exists?
           open-binary-input-file open-binary-output-file open-input-file
           open-output-file with-input-from-file with-output-to-file
 
           acos asin atan cos exp finite? log nan? sin sqrt tan
 
           ;; from R7RS draft 4
           command-line exit get-environment-variable
           read
           display write write-simple
           )
         (import 
           (rsc-core-syntax)
           (rvm-primitives)
           
           (r7c core values)
           (r7c-basic lib boolean)
           (r7c-basic lib bytevectors)
           (r7c-basic lib char)
           (r7c-basic lib cxr)
           (r7c-basic lib lists)
           (r7c-basic lib mapforeach)
           (r7c-basic lib strings)
           (r7c-basic lib vectors)
           (r7c-basic syntax define) ;; FIXME: remove -basic
           (r7c-basic syntax quasiquote)
           (r7c-equiv std equal)
           (r7c-equiv std lists)
           (r7c-io port buffers)
           (r7c-io port control)
           (r7c-io port core)
           (r7c-io port defaults)
           (r7c-io port files)
           (r7c-io port objects)
           (r7c-io writer datum)
           (rvm numeric division) ; = (r7c-numeric std division)
           (rvm numeric generic) ; = (r7c-numeric std generic)
           (rvm numeric inexact) ; = (r7c-numeric std inexact)
           (rvm numeric misc) ; = (r7c-numeric std misc)
           ;(r7c-report binding-construct define-values)
           (r7c-report binding-construct let-values)
           (r7c-report conditional case)
           (r7c-report conditional unless)
           ;(r7c-report conditional when) ;; NOT Exists..?
           ;(r7c-report misc case-lambda)
           ;(r7c-report misc do)
           ;(r7c-report misc guard)
           ;(r7c-report misc lazy)
           ;(r7c-report misc parameterize)
           
           )
         
;; Tentative

(define (string-map proc . args)
  (vector->string
    (apply vector-map proc (map string->vector args))))


#|
;; FIXME: This doesn't work as expected
(define (read . x) 
  ;; Actually defined in (rvm reader-runtime)
  (apply %r7c-read/mini x))
|#


         )
