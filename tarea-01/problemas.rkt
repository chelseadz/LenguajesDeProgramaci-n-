#lang racket

;; Escribe aquí tus soluciones

(define (countdown n)
  (if (eqv? n 0)
      '(0)
      (cons n (countdown (- n 1)))))

(define (insertL a b ls)
  (if (empty? ls)
      '()
      (if (eqv? a (first ls))
          (cons b (cons (first ls) (insertL a b (rest ls))))
          (cons (first ls) (insertL a b (rest ls))))))

(define (remv-1st s ls)
  (if (empty? ls)
      '()
      (if (eqv? s (first ls))
          (rest ls)
          (cons (first ls) (remv-1st s (rest ls))))))

(define (map p ls)
  (if (empty? ls)
      '()
      (cons (p (first ls)) (map p (rest ls)))))

(define (filter pd ls)
  (if (empty? ls)
      '()
      (if (pd (first ls))
          (cons (first ls) (filter pd (rest ls)))
          (filter pd (rest ls)))))

(define (zip l1 l2)
  (if (or (empty? l1) (empty? l2)) 
      '()
      (cons (cons (first l1) (first l2)) (zip (rest l1) (rest l2)))))

(define (list-index-ofv e ls)
  (define lng (+ (length ls) 1))
  (find-e e ls lng))

(define (find-e e ls lng)
     (if (empty? ls)
       (- lng)
      (if (equal? (first ls) e)
          0
          (+ 1 (find-e e (rest ls) lng)))))

(define (append ls1 ls2)
  (if (empty? ls1)
      ls2
      (cons (first ls1) (append (rest ls1) ls2))))

(define (reverse ls)
  (if (empty? ls)
      ls
      (append (reverse (rest ls)) (list (first ls)))))

(define (repeat ls t)
  (repeat1 ls t ls))

(define (repeat1 ls t lsi)
  (if (eqv? t 0)
      '()
      (if (<= t 1)
         ls
         (repeat1 (append ls lsi) (sub1 t) lsi))))

(define (same-lists* ls1 ls2)
  (equal? ls1 ls2))

(define (binary->natural bin)
  (bin-nat bin 0))

(define (bin-nat bin n)
  (if (empty? bin)
      0
      (+ (* (expt 2 n) (first bin)) (bin-nat (rest bin) (+ n 1)))))

(define (div a b)
  (div1 a b 1))

(define (div1 a b coc)
    (if (< a (* b coc))
        0
        (if (eqv? (* b coc) a)
            coc
            (div1 a b (add1 coc)))))

(define (append-map p ls)
  (if (empty? ls)
      '()
      (append (p (first ls)) (append-map p (rest ls)))))

(define (set-difference ls1 ls2)
  (if (empty? ls1)
      '()
      (if (has-e (first ls1) ls2)
          (set-difference (rest ls1) ls2)
          (cons (first ls1) (set-difference (rest ls1) ls2)))))
            
(define (has-e e ls)
  (if (empty? ls)
      false
      (if (equal? (first ls) e)
          true
          (has-e e (rest ls)))))

(define (foldr op n ls)
  (if (empty? ls)
      n
      (op (first ls) (foldr op n (rest ls)))))

(define (powerset ls)
    (if (empty? ls)
        (list ls)
        (let ([ps (powerset (rest ls))])
          (append (map-cons (first ls) ps) ps))))

(define (map-cons fs ps)
  (if (empty? ps)
      ps
      (cons (cons fs (first ps)) (map-cons fs (rest ps)))))

(define snowball
  (letrec
      ((odd-case
        (lambda (fix-odd)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (odd? x))
               (snowball (add1 (* x 3))))
              (else (fix-odd x))))))
       (even-case
        (lambda (fix-even)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (even? x))
               (snowball (/ x 2)))
              (else (fix-even x))))))
       (one-case
        (lambda (fix-one)
          (lambda (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (fix-one x))))))
       (base
        (lambda (x)
          (error "Invalid value ~s~n"))))
    (one-case (odd-case (even-case base )))))

(define (cartesian-product ls)
  (define ls1 (list-ref ls 0))
  (define ls2 (list-ref ls 1))
  (if (empty? ls1)
      ls1
      (if (empty? ls2)
          ls2
          (append (pair-each (first ls1) ls2) (cartesian-product (list (rest ls1) ls2))))))

(define (pair-each fs ls)
  (if (empty? ls)
      ls
      (append (list (cons fs (list (first ls)))) (pair-each fs (rest ls)))))

(define (insertL-fr s1 s2 ls)
  (foldr (lambda (s1* ls*)
           (if (eqv? s1 s1*)
               (cons s2 (cons s1* ls*))
               (cons s1* ls*)))
         '() ls))

(define (filter-fr p ls)
  (foldr (lambda (fs ls*)
           (if (p fs)
               (cons fs ls*)
               ls*))
           '() ls))

(define (map-fr p ls)
  (foldr (lambda (fs ls*)
           (cons (p fs) ls*))
           '() ls))

(define (quine ns)
  ns)

(provide (all-defined-out))
