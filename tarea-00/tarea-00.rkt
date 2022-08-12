#lang racket

(define pi 3.14)

(define (area-circle r)
  (* pi (* r r)))

(define (circle-properties r)
  (list (area-circle r) (* pi (* 2 r))))

(define (rectangle-properties rec)
  (define l (list-ref rec 0))
  (define a (list-ref rec 1))
  (list (* l a) (+ (+ l a) (+ l a))))

(define (find-needle l)
  (define lng (+ (length l) 1))
  (find-needle1 l lng))

(define (find-needle1 l lng)
  (if (empty? l)
      (- lng)
      (if (equal? (first l) 'needle)
          0
          (+ 1 (find-needle1 (rest l) lng)))))

(define (abs n)
  (if (< n 0)
      (- n)
      n))

(define (inclis1 l)
  (map inclis l))

(define (inclis n)
  (+ n 1))

(define (even? n)
  (define c (/ n 2))
  (eq? c (floor c)))

(define another-add 
  (lambda (n m)
    (cond
      [(zero? n) m]
      [else (+ 1 (another-add m (- n 1)))])))

(provide (all-defined-out))