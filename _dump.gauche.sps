(import (yuni scheme)
        (gauche base)
        (yuni io drypack))

(define input #f)

(let ((c (command-line)))
 (set! input (car (reverse c))))


(call-with-port (open-binary-input-file input)
                (lambda (p) (pprint (drypack-get p))))
