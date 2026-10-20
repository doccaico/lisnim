### lisnim
This is a very simple Lisp. It is a reimplementation in Nim of ["lispy"](https://www.norvig.com/lispy.html), written by Peter Norvig.
### Nim version
```
$ nim --version
Nim Compiler Version 2.2.12
```
### Run
```
$ nim r main.nim

lisnim> (define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))
fact
lisnim> (fact 10)
3628800
```
### Load a file
```
lisnim> (:load "demo.scm")
"Hello, Lisp"
28.274333882308138
3628800
41369087205782.695
3
4
10
40
160
2560
655360
65536
(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
(1, 1, 2, 3, 5, 8, 13, 21, 34, 55)
```
