(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= i 1) 1]
          [(= (remainder n i) (remainder m i) 0) i]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

