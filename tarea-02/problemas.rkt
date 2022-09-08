#lang racket
(require (lib "racket/draw")
         (lib "pict"))


;BUNDLE
(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (unit-string? (first x))
           (unit-string-list? (rest x)))))

;explode : (string?) -> list
; Convierte una cadena en una lista de caracteres unitarios
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

;implode : (string?) -> list
; Convierte una lista de caracteres unitarios en una cadena
(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e"
           ls))
  (apply string-append ls))

;; take : (list) integer? -> list
; toma n elementos de la lista l
(define (take l n)
  (if (empty? l)
      l
      (if (eq? n 0)
          '()
          (cons (first l) (take (rest l) (sub1 n))))))

;; drop : (list) integer? -> list
; Devuelve una lista sin los primeros n elementos
(define (drop l n)
  (if (empty? l)
      l
      (if (eq? n 0)
          l
          (drop (rest l) (sub1 n)))))

;; list->chunks : (list) integer? -> list
; Agrupa listas de n elementos de una lista
; Devuelve una lista de listas
(define (list->chunks l n)
  (cond
    [(empty? l) l]
    [(eq? n 0) '()]
    [(not (empty? l)) (cons (take l n) (list->chunks (drop l n) n))]))

;bundle : (list?) integer? -> list
; agrupa subcadenas ordenadas de n caracteres de la cadena s en una lista
(define (bundle s n)
  (if (string? s)
      (map implode (list->chunks (explode s) n))
      (map implode (list->chunks (list-explode s) n)))
  )

;list-explode : (list?) -> list
; Convierte una lista de cadenas en una lista de caracteres unitarios
(define (list-explode l)
  (if (empty? l)
      l
      (append (explode (first l)) (list-explode (rest l)))))

;partition : (string?) integer? -> list
(define (partition s n)
  (if (string? s)
      (map implode (list->chunks (explode s) n))
      (error 'partition "esperaba una cadena, pero recibí: ~e" s))
  )


;isort : (list?) symbol? -> list
; Ordena una lista de elementos segun el orden recibido.
; 
(define (isort ls cndt)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) cndt) cndt)))

;insert : (list?) symbol? -> list
(define (insert n ls cndt)
  (cond
    [(empty? ls) (list n)]
    [(cndt n (first ls)) (cons n ls)]
    [(eq? n (first ls)) (cons n ls)]
    [else
     (cons (first ls) (insert n (rest ls) cndt))]))


;QUICKSORT

;quicksort : (list?) symbol? -> list
(define (quicksort ls cndt)
  (define (smallers ls n)
    (filter (lambda (x) (not (cndt n x))) ls))
  (define (largers ls n)
    (filter (lambda (x) (cndt n x)) ls))
  (cond
    [(empty? ls) '()]
    [else
      (define pivot (first ls))
      (append (quicksort (smallers (rest ls) pivot) cndt)
              (list pivot)
              (quicksort (largers (rest ls) pivot) cndt))]))

;rand-list : integer? integer? -> list
(define (rand-list sz num)
  (cond
    [(= sz 0) '()]
    [else
     (cons (+ 1 (random num))
           (rand-list (- sz 1) num))]))

;problema 13
(define (testing-sort it sz num)
  (if (= it 0)
      '()
      ((let ([ls (rand-list sz num)])
         (writeln "Quicksort: ")
         (time (quicksort ls <))
         (writeln "Isort: ")
         (time (isort ls <))
         (newline)
         (testing-sort (sub1 it) (+ sz 10) num)))))


;
(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

(define (testinggcd n m)
  (writeln "Structural: ")
  (time (gcd-structural n m))
  (writeln "Generative: ")
  (time (gcd-generative n m))
  (newline))

;PILÓN
(define (triangle side width color)
  (define w side)
  (define h (* side (sin (/ pi 3))))
  (define (draw-it ctx dx dy)
    (define prev-pen (send ctx get-pen))
    (define path (new dc-path%))
    (send ctx set-pen (new pen% [width width] [color color]))
    (send path move-to 0 h)
    (send path line-to w h)
    (send path line-to (/ w 2) 0)
    (send path close)
    (send ctx draw-path path dx dy)
    (send ctx set-pen prev-pen))
  (dc draw-it w h))

(define (sierpinski side)
  (cond [(<= side 4) (triangle side 1 "red")]
        [else
         (define half (sierpinski (/ side 2)))
         (vc-append half (hc-append half half))]))



(provide (all-defined-out))