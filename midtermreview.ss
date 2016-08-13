(define add3
  (lambda (x)
    (+ 3 x)))

(define subtract3
  (lambda (x)
    (- x 3)))

(define thousands
    (lambda (n) (quotient
                (remainder n 10000) 1000)))

(define left-part
  (lambda (str)
    (substring str 0 (quotient (string-length str) 2))))

(define right-part
  (lambda (str)
    (substring str
      (+ (quotient (string-length str) 2) (remainder (string-length str) 2))
      (string-length str))))

(define middle
  (lambda (str)
    (if (even? (string-length str))
        ""
        (substring str
          (quotient (string-length str) 2)
          (+ 1 (quotient (string-length str) 2))))))

(define char-at
  (lambda (s n)
    (substring s n (+ n 1))))

(define random-in-range
  (lambda (n p)
    (+ n (random (+ 1 p)))))

(define jitter
  (lambda (n dist)
    (+ (random (+ 1 (* 2 dist))) (- n dist))))

(define in-circle?
  (lambda (x y)
     (< (+ (* x x) (* y y)) 1)))

(define next-collatz
  (lambda (a)
    (if (even? a)
        (/ a 2)
        (+ 1 (* 3 a)))))

(define walking-point
  (lambda (pace)
    (if (>= pace 10)
       0
    (+ 1 (walking-point (+ pace (* .02 pace)))))))

(define addends-helper
  (lambda (sum ascend descend acc)
    (if (eq? ascend sum)
        acc
        (addends-helper
          sum (add1 ascend) (sub1 descend) (cons (list descend ascend) acc)))))


(define addends
  (lambda (sum)
    (addends-helper
      sum 1 (sub1 sum) '())))

(define addends2-helper
  (lambda (sum ascend descend)
    (if (< descend ascend)
        '()
        (cons (list ascend descend)
        (addends2-helper
          sum (add1 ascend) (sub1 descend))))))

(define addends2
  (lambda (sum)
    (addends2-helper
      sum 1 (sub1 sum))))

(define leap-frog
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [(and (equal? (car ls) 'frog) (null? (cdr ls)) '())]
      [(equal? (car ls) 'frog)
       (cons (cadr ls) (cons 'frog
                         (leap-frog (cddr ls))))]
      (else
       (cons (car ls)
         (leap-frog (cdr ls)))))))

(define evens-out
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(even? (car ls))
       (evens-out (cdr ls))]
      (else
       (cons (car ls)
       (evens-out (cdr ls)))))))

(define filter-out
  (lambda (pred? ls)
    (cond
      [(null? ls) ls]
      [(pred? (car ls))
       (filter-out pred? (cdr ls))]
      (else
       (cons (car ls)
         (filter-out pred? (cdr ls)))))))

(define no-small-words
  (lambda (ls)
    (filter-out
      (lambda (s)
         (< (string-length s) 4))
      ls)))

(define sparkline
  (lambda (ls)
    (cond
      [(null? (cdr ls)) '()]
      [(> (cadr ls) (car ls))
       (cons '^ (sparkline (cdr ls)))]
      [(< (cadr ls) (car ls))
       (cons 'v (sparkline (cdr ls)))]
      (else
       (cons '- (sparkline (cdr ls)))))))

(define on<->off
  (lambda (x)
    (if (equal? x 'on)
        'off
    'on)))


(define lights-out-once-help
  (lambda (n ls count)
    (cond
      [(null? ls) '()]
      [(equal? n 0)
       (if
         (null? (cdr ls))
           (list (on<->off (car ls)))
           (cons (on<->off (car ls)) (cons (on<->off (cadr ls)) (cddr ls))))]
      [(null? (cddr ls)) ;;if n = list length, last member of the list
       (list (on<->off (car ls)) (on<->off (cadr ls)))]
      [(equal? count (sub1 n))  ;; if n = count + 1
       (cons (on<->off (car ls))
         (cons (on<->off (cadr ls))
           (cons (on<->off (caddr ls)) (cdddr ls))))]
      (else
       (cons (car ls)
       (lights-out-once-help n (cdr ls) (add1 count)))))))

(define lights-out-once
  (lambda (pos cfgls)
    (lights-out-once-help pos cfgls 0)))

(define lights-out
  (lambda (cfgls opls)
    (cond
      [(null? opls) cfgls]
      (else
      (lights-out
       (lights-out-once (car opls) cfgls) (cdr opls))))))

(define divides?
  (lambda (x y)
    (and
      (not (zero? x))
      (integer? (/ y x)))))

(define slice-helper
  (lambda (n x y)
   (cond
     [(= x 1) n]
     [(divides? x n)
      (if (= n (* x y))
          (list x y)
      (slice-helper n x (add1 y)))]
     (else
      (slice-helper n (sub1 x) y)))))

(define slice
  (lambda (n)
    (slice-helper n (round (sqrt n)) (round (sqrt n)))))

(define factor-samurai
  (lambda (ls)
   (if
    (null? ls)
       ls
       (if (pair? (slice (car ls)))
       (cons (car (slice (car ls))) (cons (cadr (slice (car ls)))
                                      (factor-samurai (cdr ls))))
           (cons (slice (car ls)) (factor-samurai (cdr ls)))))))

(define make-list
  (lambda (n item)
    (if (zero? n)
        '()
        (cons item
          (make-list (sub1 n) item)))))

(define right-before?
  (lambda (point1 point2)
    (and
         (equal? (car point1) (car point2))
         (equal? (cadr point1) (sub1 (cadr point2))))))