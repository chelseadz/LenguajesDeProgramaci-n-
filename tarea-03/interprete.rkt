#lang plait

;; ENVIRONMENT ;;

(define-type Binding
  (binding [name : Symbol]
           [value : Value]))

(define-type-alias Environment (Listof Binding))

(define empty-env empty)

(define (lookup-env name env)
  (if (empty? env)
      (unbound-identifier-error name)
      (if (eq? name (binding-name (first env)))
          (binding-value (first env))
          (lookup-env name (rest env)))))

(define (extend-env name value env)
  (cons (binding name value) env))


;; TYPES ;;

(define-type Value
  (numV [n : Number])
  (strV [s : String])
  (boolV [b : Boolean])
  (funV [id : Symbol]
        [body : ExprC]
        [env : Environment]))

(define-type Operator
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

(define-type ExprC
  (numC [value : Number])
  (strC [value : String])
  (boolC [value : Boolean])
  (idC [name : Symbol])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (binopC [op : Operator] [left : ExprC] [right : ExprC])
  (funC [param : Symbol] [body : ExprC])
  (appC [func : ExprC] [arg : ExprC]))


(define-type ExprS
  (numS [n : Number])
  (boolS [b : Boolean])
  (strS [s : String])
  (binopS [op : Operator]
          [l : ExprS]
          [r : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (idS [name : Symbol])
  (funS [name : Symbol]
        [arg : ExprS])
  (letS [name : Symbol]
        [val : ExprS]
        [body : ExprS])
  (appS [func : ExprS]
        [arg : ExprS]))

;; ERROR ;;

(define (bad-arg-to-op-error [op : Symbol] [v : Value])
  (error 'interp
         (string-append
          "bad argument to operator "
          (string-append
           (to-string op)
           (string-append
            ": "
            (to-string v))))))

(define (bad-conditional-error [v : Value])
  (error 'interp
         (string-append
          "bad conditional to if expression: "
          (to-string v))))

(define (unbound-identifier-error [name : Symbol])
  (error 'interp
         (string-append
          "unbound identifier: "
          (to-string name))))


;; EVAL ;;

(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str))))


;; INTERP ;;

(define (interp [e : ExprC]) : Value
  (interp-help e empty-env))

(define (interp-help [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    [(numC value) (numV value)]
    [(strC value) (strV value)]
    [(boolC value) (boolV value)]
    [(idC name)
     (lookup-env name env)]
    [(ifC e1 e2 e3)
     (let ([v1 (interp-help e1 env)])
       (cond [(not (boolV? v1))
              (error 'ifC "no es un valor booleano")]
             [(boolV-b v1)
              (interp-help e2 env)]
             [else
              (interp-help e3 env)]))]
    [(binopC op left right)
     (let ([left (interp-help left env)])
       (let ([right (interp-help right env)])
         (interp-binop op left right)))]
    [(funC name body) (funV name body env)]
    [(appC func arg)
     (let ([func (interp-help func env)])
       (cond [(funV? func)
              (interp-help (funV-body func)(extend-env (funV-id func) (interp-help arg env) (funV-env func)))]
             [else
              (error 'func "no es una función")]))])
     )

(define (interp-binop [op : Operator]
                      [left : Value]
                      [right : Value]) : Value
  (type-case Operator op
    [(plusO)
     (if (numV? left)
         (if (numV? right)
             (numV (+ (numV-n left)
                      (numV-n right)))
             (error 'binop "Not a number. argumento incorrecto"))
         (error 'binop "Not a number. argumento incorrecto"))]
    [(appendO)
     (if (strV? left)
         (if (strV? right)
             (strV (string-append (strV-s left)
                      (strV-s right)))
             (error 'binop "Not a string. argumento incorrecto"))
         (error 'binop "Not a string. argumento incorrecto"))]
    [(numeqO)
     (if (numV? left)
         (if (numV? right)
             (boolV (= (numV-n left)
                      (numV-n right)))
             (error 'binop "Not a number. argumento incorrecto"))
         (error 'binop "Not a number. argumento incorrecto"))]
    [(streqO)
     (if (strV? left)
         (if (strV? right)
             (boolV (string=? (strV-s left)
                      (strV-s right)))
             (error 'binop "Not a string. argumento incorrecto"))
         (error 'binop "Not a string. argumento incorrecto"))]))


;; DESUGAR ;;

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(boolS b) (boolC b)]
    [(strS s) (strC s)]
    [(binopS op l r) (binopC op (desugar l) (desugar r))]
    [(orS e1 e2) (ifC (desugar e1)
                      (boolC #t)
                      (ifC (desugar e2)
                            (boolC #t)
                            (boolC #f)))]
    [(andS e1 e2) (ifC (desugar e1)
                       (ifC (desugar e2)
                            (boolC #t)
                            (boolC #f))
                       (boolC #f))]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [(idS name) (idC name)]
    [(letS name val body) (appC (funC name (desugar body)) (desugar val))]
    [(funS name body) (funC name (desugar body))]
    [(appS func arg) (appC (desugar func) (desugar arg))]))
   


;; PARSE ;;

(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in) (parse-number in)]
    [(s-exp-string? in) (parse-string in)]
    [(s-exp-match? `true in) (boolS #t)]
    [(s-exp-match? `false in) (boolS #f)]
    [(s-exp-match? `{if ANY ...} in) (parse-if in)]
    [(s-exp-match? `{and ANY ...} in) (parse-and in)]
    [(s-exp-match? `{or ANY ...} in) (parse-or in)]
    [(s-exp-match? `{+ ANY ...} in) (parse-+ in)]
    [(s-exp-match? `{++ ANY ...} in) (parse-++ in)]
    [(s-exp-match? `{num= ANY ...} in) (parse-num= in)]
    [(s-exp-match? `{str= ANY ...} in) (parse-str= in)]
    [(s-exp-match? `{fun ANY ...} in) (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
    [(s-exp-match? `{ANY ...} in) (parse-app in)]
    [(s-exp-symbol? in) (parse-id in)]
    [else (error 'parse "expresión malformada")]))


(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
         (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de
               ↩→ funciones"))))


