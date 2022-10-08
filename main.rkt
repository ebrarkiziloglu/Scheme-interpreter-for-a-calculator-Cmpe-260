; asude ebrar kiziloglu
; 2019400009
; compiling: yes
; complete: yes
#lang racket
(provide (all-defined-out))
; 3.1: 
(define := (lambda (var value) (cons var ( cons value '()))))
; 3.2: 
(define -- (lambda args   (cons 'let (cons args '() ) )     ))
; 3.3: 
(define @ (lambda (bindings expr)  (append bindings expr)  ))

; 3.4:
;;; Following function finds the first occurence of delim in lst, 
;;; and returns the partial-list that comes before this first occurence. 
(define split_first (lambda (delim lst)
        (if (or (empty? lst) (equal? (car lst) delim))
            '()
            (cons (car lst) (split_first delim (cdr lst))))) )
;;; With the help of the function above, following function recursively split lst according to delim element given.
(define split_at_delim (lambda (delim args)
    (let df ([lst args])
        (if (empty? lst)    ;; lst is finished
            '()
            (let ((frst (split_first delim lst)))   ;; first partial list is put in frst
                (cons
                    frst                            ;; cons frst with split_at_delim(rest of the lst)
                    (df (let ([nextposition (+ 1 (length frst))])       ;; find the position of the first occurence of delim
                        (if (< nextposition (length lst))
                            (list-tail lst nextposition)                ;; get the partial list after the first occurence
                            '() )))))))))
; 3.5:
;; Assignment conversion function:      ('x := 5)  ---- > (:= 'x 5)
;; Following function rearrange the order of elements in an assignment for := to handle:
;; It also checks whether the assignment is in the form of ['variable := 'variable] or ['variable := number]
;; According to that info, it takes the necessary levels of the elements (caddar or cadr(caddar))
(define rearrange_assg (lambda args  (cond
    [ (number?  (caddar args))   (cons (cadaar args) (cons (caddar args) '() )) ]
    [else  (cons (cadaar args) (cons (cadr (caddar args)) '() )) ]  )))
;;; Following 3 functions parse the expressions according to the EBNF structure constructed from the information in the description
;;; <expr> -> <term> { + <term> }
;;; <term> -> <factor> { * <factor> }
;;; <factor> -> (expr) | number | variable | (bindings) @ (expr)
(define parse_factor (lambda (factor)  
    (cond 
        [ (not (list? factor )) factor ]                        ;;; number / variable
        [ (not (equal? 1 (length (split_at_delim '@ factor))))  ;;; (bindings) @ (expr)
        (   let ([bindings (car (split_at_delim '@ factor))] [expr (cdr (split_at_delim '@ factor))]) 
            (cons 'let (cons (map rearrange_assg (split_at_delim '-- (car bindings))) (cons (parse_expr (caar expr)) '())))
        )]
        [ (list? factor) (parse_expr (car factor)) ]            ;;; (expr)
)))

(define parse_term (lambda (term) 
    (cond
        [(equal? 1 (length (split_at_delim '* term))) (parse_factor term)  ]        ;;; no *
        [else (let ([factors (split_at_delim '* term)])  (cons '* (map parse_factor factors)) )  ]   ;;; some * 
)))

(define parse_expr (lambda (expr)
    (cond
        [(not (list? expr)) (parse_factor expr)]
        [(equal? 1 (length (split_at_delim '+ expr)))   (parse_term expr) ]         ;;; no +
        [else (let ([terms (split_at_delim '+ expr)])  (cons '+ (map parse_term terms)) ) ]     ;;; some +
)))

; 3.6:
(define eval_expr (lambda (expr)  (eval (parse_expr expr)) ))
