(library (r7c syntax case)
  (export case)
  (import (r7c-system core)
          (r7c-system synrules)
          (r7c-system auxsyntax)
          (r7c syntax cond)
          (r7c syntax if))

  (define-syntax $case/clause
    (syntax-rules (else)
      ((_ arg (clauses ...) (else frm ...) clause ...)
       ($case/clause arg (clauses ... (else frm ...)) clause ...))
      ((_ arg (clauses ...) ((query ...) body ...) clause ...)
       ($case/clause arg (clauses ... ((or (eqv? 'query arg) ...) body ...)) clause ...))
      ((_ arg (clauses ...))
       (cond clauses ...))))

  (define-syntax case
    (syntax-rules ()
      ((_ arg clause ...)
       (let ((__1 arg))
        ($case/clause __1 () clause ...)))))
  )
