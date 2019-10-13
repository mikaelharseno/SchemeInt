(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (cons-all first rests)
  (map (lambda (x) (cons first x)) rests))

(define (zip pairs)
  (if (null? pairs)
      (list nil nil)
      (list (cons (caar pairs)
                  (car (zip (cdr pairs))))
            (cons (car (cdar pairs))
                  (cadr (zip (cdr pairs)))))))

;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerator lst start)
    (if (null? lst)
        nil
        (cons (cons start (cons (car lst) nil))
              (enumerator (cdr lst) (+ 1 start)))))
  (enumerator s 0)
  )
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18
  (if (null? denoms)
      nil
      (if (< total (car denoms))
          (list-change total (cdr denoms))
          (if (> total (car denoms))
              (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                      (list-change total (cdr denoms)))
              (append (cons (cons total nil) nil) (list-change total (cdr denoms))))
    ))
  )
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Defines the atom? function that checks whether something is an atom (not a Scheme pair)
(define (atom? exp) (if (pair? exp) #f #t))
;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 19
         expr
         ; END PROBLEM 19
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define new_body (map let-to-lambda body))
           (if (lambda? expr)
              (append (list 'lambda params) new_body)
              (append (list 'define params) new_body))
           ; END PROBLEM 19
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 19
           (define zip_results (zip values))
           (define new_body (map let-to-lambda body))
           (define input_values (cadr zip_results))
           (define new_input_values (map let-to-lambda input_values))
           (cons (append (list 'lambda (car zip_results)) new_body) new_input_values)
           ; END PROBLEM 19
           ))
        (else
         ; BEGIN PROBLEM 19
         'replace-this-line
         (map let-to-lambda expr)
         ; END PROBLEM 19
         )))
