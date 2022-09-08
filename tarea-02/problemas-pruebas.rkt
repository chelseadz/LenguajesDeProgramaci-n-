#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas

  (test-case "bundle"
             (check-equal? (bundle '() 1) '())
             (check-equal? (bundle (explode "abcdefg") 3) (list "abc" "def" "g") )
             (check-equal? (bundle (explode "abcdefg") 8) (list "abcdefg") )
             (check-equal? (bundle (explode "abcdefg") 7) (list "abcdefg") )
             (check-equal? (bundle (explode "abcdefg") 0)  '())
             (check-equal? (bundle (explode "abcdefg") 1) (list "a" "b" "c" "d" "e" "f" "g") )
             )

  (test-case "take"
             (check-equal? (take (list "a" "b" "c") 1) '("a"))
             (check-equal? (take (list "a" "b" "c") 3) (list "a" "b" "c"))
             (check-equal? (take (list "a" "b" "c") 0) '())
             (check-equal? (take '() 1) '())
             (check-equal? (take (list "a" "b" "c") 10) (list "a" "b" "c")))
  
  (test-case "drop"  
             (check-equal? (drop (list "a" "b" "c") 1) '("b" "c"))
             (check-equal? (drop (list "a" "b" "c") 3) '())
             (check-equal? (drop (list "a" "b" "c") 0) (list "a" "b" "c"))
             (check-equal? (drop '() 1) '())
             (check-equal? (drop (list "a" "b" "c") 10) '()))

  (test-case "list->chunks"
             (check-equal? (list->chunks '() 1) '())
             (check-equal? (list->chunks (explode "abcdefg") 3) '(("a" "b" "c") ("d" "e" "f") ("g")))
             (check-equal? (list->chunks (explode "abcdefg") 8) '(("a" "b" "c" "d" "e" "f" "g") ))
             (check-equal? (list->chunks (explode "abcdefg") 7) '(("a" "b" "c" "d" "e" "f" "g") ))
             (check-equal? (list->chunks (explode "abcdefg") 0) '())
             (check-equal? (list->chunks (explode "abcdefg") 1) '(("a") ("b") ("c") ("d") ("e") ("f") ("g")) ))
             
  (test-case "partition"
             (check-exn exn:fail? (thunk (partition '() 1)))
             (check-equal? (partition "abcdefg" 3) (list "abc" "def" "g") )
             (check-equal? (partition "abcdefg" 8) (list "abcdefg") )
             (check-equal? (partition "abcdefg" 7) (list "abcdefg") )
             (check-equal? (partition "abcdefg" 0)  '())
             (check-equal? (partition "abcdefg" 1) (list "a" "b" "c" "d" "e" "f" "g") )
             )
  
  (test-case "isort"
             (check-equal? (isort '(10 5 3 6 8 5 4 5 6 2 8 10 2 7 4) <)
                           '(2 2 3 4 4 5 5 5 6 6 7 8 8 10 10) )
             (check-equal? (isort '(10 5 3 6 8 5 4 5 6 2 8 10 2 7 4) >)
                           '(10 10 8 8 7 6 6 5 5 5 4 4 3 2 2) )
             (check-equal? (isort '("Rojo" "Cuen" "Laborin") string<?)
                           '("Cuen" "Laborin" "Rojo"))
             (check-equal? (isort '() <)
                           '())
             )
  

  (test-case "quicksort"
             (check-equal? (quicksort '(10 5 3 6 8 5 4 5 6 2 8 10 2 7 4) <)
                           '(2 2 3 4 4 5 5 5 6 6 7 8 8 10 10) )
             (check-equal? (quicksort '(10 5 3 6 8 5 4 5 6 2 8 10 2 7 4) >)
                           '(10 10 8 8 7 6 6 5 5 5 4 4 3 2 2) )
             (check-equal? (quicksort '("Rojo" "Cuen" "Laborin") string<?)
                           '("Cuen" "Laborin" "Rojo"))
             (check-equal? (quicksort '() <)
                           '())
             )
  
  )
(run-tests pruebas 'verbose)