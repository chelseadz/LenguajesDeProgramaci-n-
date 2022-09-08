#lang plait

(require "arithlang.rkt")

;tests

;Problema 1
(test (eval `(+ 2 2)) 4)
(test (eval `(+ (+ 1 (+ 1 0)) 1)) 3)
(test (eval `(* (+ 1 (+ 2 0)) 3)) 9)

(test (eval `(- (+ 1 (+ 2 0)) 3)) 0)
(test (eval `(- 3)) -3)
(test (eval `(* 2 (- 2))) -4)

