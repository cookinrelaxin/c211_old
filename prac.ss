(define-syntax when
  (syntax-rules ()
    ((_ pred b1 ...)
     (if pred (begin b1 ...)))))

(define-syntax while
  (syntax-rules ()
    ((_ pred b1 ...)
     (let loop () (when pred b1 ... (loop))))))

(define-syntax for
  (syntax-rules ()
    ((_ (i start end) b1 ...)
     (let loop((i start))
       (when (< i end)
   b1 ...
   (loop (add1 i)))))))

(define (f-nil! x)
   (set! x '()))

(define-syntax nil!
  (syntax-rules ()
    ((_ x)
     (set! x '()))))

(define-syntax incr
  (syntax-rules ()
    ((_ x) (begin (set! x (add1 1)) x))
    ((_ x i) (begin (set! x (add1 i)) x))))

(define-syntax decr
  (syntax-rules ()
    ((_ x) (begin (set! x (sub1 x)) x))
    ((_ x i) (begin (set! x (- x i)) x))))

(define-syntax my-cond
  (syntax-rules (otherwise)
    ((_ (otherwise e1 ...))
     (begin e1 ...))
    ((_ (e1 e2 ...))
     (when e1 e2 ...))
    ((_ (e1 e2 ...) c1 ...)
     (if e1
  (begin e2 ...)
  (cond c1 ...)))))

(define-syntax show-vars
  (lambda (x)
    (syntax-case x ()
      [(_) #''shown]
      [(_ e1 e2 ...)
       #'(begin (display 'e1) (display "->") (display e1) (newline) (show-vars e2 ...))])))

(define-syntax show-vars1
  (syntax-rules ()
      [(_) 'shown]
      [(_ e1 e2 ...)
       (begin (display 'e1) (display "->") (display e1) (newline) (show-vars e2 ...))]))

(define-syntax show-vars2
  (lambda (x)
    (syntax-case x ()
      [(_) (syntax 'shown)]
      [(_ e1 e2 ...)
       (syntax (begin (display 'e1) (display "->") (display e1) (newline) (show-vars e2 ...)))])))

(define-syntax p.car
  (lambda (x)
    (syntax-case x ()
      [(_ . rest) #'((car p) . rest)]
      [_  #'(car p)])))

(define-syntax p.car1
  (make-variable-transformer
    (lambda (x)
      (syntax-case x (set!)
        [(set! _ e) #'(set-car! p e)]
        [(_ . rest) #'((car p) . rest)]
        [_  #'(car p)]))))

(let ()
  (define define 17)
  define)

(let-syntax ([def0
               (syntax-rules ()
                     [(_ x) (define x 0)])])
  (let ()
    (def0 z)
    (define def0 '(def 0))
    (list z def0)))

(let ()
  (define even?
    (lambda (x)
      (or (= x 0) (odd? (- x 1)))))       ; wow!
  (define-syntax odd?
    (syntax-rules ()
      [(_ x) (not (even? x))]))
  (even? 10))

(let ()
  (define even?
    (lambda (x)
      (or (= x 0) (odd? (- x 1)))))       ; wow!
  (define odd?
    (lambda (x)
     (not (even? x))))
  (even? 10))

(let ([f (lambda (x) (+ x 1))])
  (let-syntax ([f (syntax-rules ()
                    [(_ x) x])]
               [g (syntax-rules ()
                    [(_ x) (f x)])])
    (list (f 1) (g 1))))



(let ([f (lambda (x) (+ x 1))])
  (letrec-syntax ([f (syntax-rules ()
                       [(_ x) x])]
                  [g (syntax-rules ()
                       [(_ x) (f x)])])
    (list (f 1) (g 1))))

(define-syntax rec1
  (lambda (x)
    (syntax-case x ()
      [(_ x e)
       (identifier? #'x)
       #'(letrec ([x e]) x)])))

(map (rec fact
       (lambda (n)
         (if (= n 0)
             1
             (* n (fact (- n 1))))))
     '(1 2 3 4 5))

               ;;   (1 2 6 24 120)

(letrec ((fact (lambda (n)
           (if (= n 0)
               1
               (* n (fact (- n 1)))))))
    (map fact
       '(1 2 3 4 5)))
              ;;   (1 2 6 24 120)


(let-syntax ([dolet (lambda (x)
                      (syntax-case x ()
                        [(_ b)
                         #'(let ([a 3] [b 4]) (+ a b))]))])
  (dolet a))

(define symbolic-identifier?
  (lambda (x y)
    (eq? (syntax->datum x)
         (syntax->datum y))))

(define-syntax loop
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax ([break1 (datum->syntax #'k 'break1)])  ; woah
         #'(call-with-current-continuation
             (lambda (break1)
               (let f () e ... (f)))))])))

(define-syntax loop1
  (lambda (x)
    (syntax-case x ()
      [(_ e ...)
       #'(call-with-current-continuation
           (lambda (break1) (let f () e ... (f))))])))
                      ; doesnt work because here, break1 isnt bound

(define-syntax loop2
  (lambda (x)
    (syntax-case x ()
      [(k e ...)
       (with-syntax ([break1 (datum->syntax #'k 'break1)])  ; woah
         #'(lambda (break1)
               (let f () e ... (f))))])))



;  ~ (majority '((a 3) (b 5) (c 2) (a 2) (b 1) (a 5)) '(a b c))
;   a

;; use rec?


#|
(define count-elements
  (lambda (ls-to-count label-ls)
   (let ((counters (make-counters label-ls)))
    (let loop ((ls ls-to-count) (counters counters))
      (if (null? ls) counters
        (let ((a (assoc (caar ls) counters)))
         (if a
            (begin
             (set! a (list (caar a) (add1 (cadr a))))
          counters))))))))

              ; (loop (cdr ls) counters))
            ;(loop (cdr ls)))))))))
          |#

;; (most-common '(a a a b c e d j o q o p a a))

;; a

(define make-counters
  (lambda (ls)
    (let loop ((ls ls) (previously-encountered '()))
      (cond
        [(null? ls) ls]
        [(member (car ls) previously-encountered)
          (loop (cdr ls) previously-encountered)]
        (else
          (cons (list (car ls) 0)
            (loop (cdr ls) (cons (car ls) previously-encountered))))))))

(define update
  (lambda (x counter-ls)
    (let loop ((ls counter-ls))
      (cond
        [(null? ls) ls]
        [(equal? (caar ls) x)
          (cons (list x (add1 (cadar ls))) (cdr ls))]
       (else
         (cons (car ls) (loop (cdr ls))))))))

(define count-elements
  (lambda (ls-to-count label-ls)
   (let ((counter-ls (make-counters label-ls)))
    (let loop ((ls1 ls-to-count) (ls2 counter-ls))
     (if (null? ls1) ls2
        (loop (cdr ls1) (update (caar ls1) ls2)))))))

(define biggest
  (lambda (counted-ls)
    (let loop ((ls counted-ls))
      (cond
        [(null? ls) #f]
        [(null? (cdr ls)) (caar ls)]
        [(> (cadar ls) (cadadr ls))
          (loop (cons (car ls) (cddr ls)))]
        (else
         (loop (cdr ls)))))))

(define majority
  (lambda (ls-to-count label-ls)
    (biggest (count-elements ls-to-count label-ls))))

;; ~ (affix-weights '(a b c d))
;;   ((a 3) (b 2) (c 1) (d 0))


(define affix-weights
 (lambda (ls)
  (define affix-helper&co
   (lambda (ls1 co)
    (if (null? ls1)
        (co ls1 0)
       (affix-helper&co (cdr ls1) (lambda (x index)
                                   (co
                                     (cons
                                       (cons (car ls1)
                                         (cons index '())) x) (+ 1 index)))))))
(affix-helper&co ls (lambda (x index) x))))

;;;  wtf....

(define affix-weights1
 (lambda (ls)
  (let loop ((ls ls) (coll (lambda (x index) x)))
    (if (null? ls)
        (coll ls 0)
        (loop (cdr ls)
          (lambda (x index)
            (coll
               (cons (cons (car ls) (list index)) x)
               (+ 1 index))))))))

;;; ?????