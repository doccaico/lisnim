; (print "Hello, Lisp")

(define circle-area (lambda (r) (* pi (* r r))))
(print (circle-area 3))

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
(print (fact 10))
(print (circle-area (fact 10)))

(define first car)
(define rest cdr)
(define count (lambda (item L) (if L (+ (equal? item (first L)) (count item (rest L))) 0)))
(print (count 0 (list 0 1 2 3 0 0)))
(print (count (quote the) (quote (the more the merrier the bigger the better))))

(define twice (lambda (x) (* 2 x)))
(print (twice 5))

(define repeat (lambda (f) (lambda (x) (f (f x)))))
(print ((repeat twice) 10))
(print ((repeat (repeat twice)) 10))
(print ((repeat (repeat (repeat twice))) 10))
(print ((repeat (repeat (repeat (repeat twice)))) 10))

(print (pow 2 16))

(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))
(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))
(print (range 0 10))

(map fib (range 0 10))
