#lang plait

;ArithC
(define-type ArithC
  (numC [n : Number])
  (plusC [a : ArithC] [b : ArithC])
  (multC [a : ArithC] [b : ArithC]))

;ArithS
(define-type ArithS
  (numS [n : Number])
  (plusS [a : ArithS] [b : ArithS])
  (multS [a : ArithS] [b : ArithS])
  (minusS [a : ArithS] [b : ArithS])
  (uminusS [n : ArithS]))

;eval
(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

;Parse
(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             [(-) (if (eq? (length ls) 2)
                            (uminusS (parse (second ls)))
                            (minusS (parse (second ls)) (parse (third ls))))]
             [(+) (plusS (parse (second ls)) (parse (third ls)))]
             [(*) (multS (parse (second ls)) (parse (third ls)))]
             [else (error 'parse "operación aritmética malformada")]))]
        [else (error 'parse "expresión aritmética malformada")]))

;desugar
(define (desugar [input : ArithS]) : ArithC
  (type-case ArithS input
    [(numS n) (numC n)]
    [(plusS first second) (plusC (desugar first) (desugar second))]
    [(multS first second) (multC (desugar first) (desugar second))]
    [(minusS first second) (plusC (desugar first) (multC (desugar second) (numC (- 0 1))))]
    [(uminusS first) (multC (desugar first) (numC (- 0 1)))]))

;Interp
(define (interp [input : ArithC]) : Number
  (type-case ArithC input
    [(numC n) n]
    [(plusC first second) (+ (interp first) (interp second))]
    [(multC first second) (* (interp first) (interp second))]))

;BUNDLE



