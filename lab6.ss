#|Write a procedure digits->number that takes a list of decimal digits (that is,
numbers between 0 and 9) and returns the number they represent. DO NOT use
length. (HINT: use a tail-recursive helper function)
Examples:

> (digits->number '(1 2 0))
120

> (digits->number '(1))
1

> (digits->number '(2 1 1))
211
|#

(define digits->number-counter
 (lambda (ls)
  (cond
   [(null? ls) 0]
    (else
     (+ 1 (digits->number-counter (cdr ls)))))))


(define iterate-digits-times-10
  (lambda (digits n)
    (if (zero? digits)
        n
        (iterate-digits-times-10 (- digits 1) (* n 10)))))


(define digits->number
  (lambda (ls)
    (cond
      [(null? ls) 0]
      (else
         (+ (iterate-digits-times-10 (- (digits->number-counter ls) 1) (car ls))
          (digits->number
           (cdr ls)))))))


#|Write a procedure called price-is-right that takes a price and a list of
guesses (represented as lists of decimal digits) and returns the closest guess
that doesn't go over the actual price. If all of the guesses go over the
price, return #f. (HINT: you may use digits->number, and you may wish to
define a tail-recursive helper function)
Examples:

> (price-is-right 100 '((1) (1 0) (9 0) (1 0 1) (9 9) (3 0 0 0)))
99

> (price-is-right 100 '((1 0 0) (1 0 0) (1 0 1) (9 9)))
100

> (price-is-right 100 '((1 0 1) (1 0 2) (1 0 3) (1 0 4)))
#f|#

(define largest
  (lambda (ls)
    (cond
      [(null? ls) #f]
      [(null? (cdr ls)) ls]
      [(>= (car ls) (cadr ls))
       (largest (cons (car ls) (cddr ls)))]
      (else
       (largest (cdr ls))))))

(define purge-excessive-prices
  (lambda (price ls)
    (cond
      [(null? ls) '()]
      [(> (digits->number (car ls)) price)
       (purge-excessive-prices price (cdr ls))]
      (else
       (cons (digits->number (car ls)) (purge-excessive-prices price (cdr ls)))))))

(define price-is-right
 (lambda (price ls)
   (largest
       (purge-excessive-prices price ls))))