#|Define procedure pad-front that takes a non-negative integer, an item,
and a list and adds the symbol the given number of times to the
beginning of the list.
~ (pad-front 3 'a '(b c))

(a a a b c)

~ (pad-front 5 0 '())

(0 0 0 0 0) |#

(define pad-front
  (lambda (addtimes symbol ls)
    (cond
      [(zero? addtimes) ls]
      (else
       (pad-front (- addtimes 1) symbol (cons symbol ls))))))

#| Define procedure decode that takes a list of two-element lists. Each
of these sublists consists of a number and a symbol. Decode returns a
list of the symbols given in each sublist, with each symbol appearing
as many times as specified by the number in its given sublist. Make
use of pad-front as a helper.
~ (decode '((3 a)
(2 b)
(5 c)))
~ (a a a b b c c c c c)

Assume that the numbers given will never be invalid,
that is to say they
will be non-negative integers. ***allowed to use append?***
|#
(define decode
  (lambda (ls)
    (cond
      [(null? ls) ls]
      (else
       (append (pad-front (caar ls) (cadar ls) '()) (decode (cdr ls)))))))

#|Challenge:
Define procedure encode that takes the output of a flat
list and
builds a nested list such that running it through
decode will give
back the original list. You are encouraged to use a
helper function.
> (encode '(a a a b b c))

((3 a) (2 b) (1 c))|#

(define groupcharacters
  (lambda (ls)
    (cond
      ;[(null? ls) ls]
      [(null? (cdr ls)) (cons '1 ls)]
      [(null? (cddr ls)) ls]
      [(not (integer? (car ls))) (groupcharacters (cons '1 ls))]
      (else
       (if (equal? (cadr ls) (caddr ls))
       (groupcharacters (cons (+ 1 (car ls)) (cddr ls)))
           ;(cons (car ls) (cons (cadr ls) '()))
           ls)))))

(define encode
  (lambda (ls)
    (cond
      [(null? ls) ls]
      (else
       (cons
         (cons (car (groupcharacters ls)) (cons (cadr (groupcharacters ls)) '()))
         (encode (cddr (groupcharacters ls))))))))