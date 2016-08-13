(import (c211 tree) (c211 matrix) (c211 image))

;http://www.scheme.com/tspl4/exceptions.html#./exceptions:s5
;!!!!!!
;awesom!!!

(define tr0
  (tree 'a
    (leaf 5) (empty-tree)))

(define tr1
  (tree 4
    (empty-tree) (leaf 'b)))

(define tr2
  (tree 'd (leaf 'c) (leaf 'e)))

(define tr3
  (tree
    '+ (leaf 7)
    (tree '-
      (tree '/ (leaf 'x) (leaf 5))
      (leaf 'y))))

(define tr4
  (tree 1
    (tree 2 (empty-tree)
      (tree 3
        (tree 4
          (leaf 5) (empty-tree)) (empty-tree)))
    (tree 2
      (tree 3
        (leaf 4) (empty-tree)) (empty-tree))))

(define tree-member?
  (lambda (x tr)
   (and (not (empty-tree? tr))
    (or
      (equal? x (root-value tr))
      (tree-member? x (left-subtree tr))
      (tree-member? x (right-subtree tr))))))

(define path-to-x
  (lambda (x tr)
    (cond
      [(empty-tree? tr) #f]
      [(equal? x (root-value tr)) (list (root-value tr))]
      (else
        (let ((left-path (path-to-x x (left-subtree tr))))
                 (if left-path
                    (cons (root-value tr) left-path)
                    (let ((right-path (path-to-x x (right-subtree tr))))
                      (if right-path
                          (cons (root-value tr) right-path)
                          #f))))))))

(define count-leaves
  (lambda (tr)
    (cond
      [(empty-tree? tr) 0]
      [(leaf? tr) 1]
      (else
       (+ (count-leaves (left-subtree tr))
          (count-leaves (right-subtree tr)))))))

(define (string-first str)
   (assert (and 'string-first (string? str)))   ;; so sick!!!!! now we can know where errors occur
   (string-ref str 0))

(define subst-tree
  (lambda (a b tr)                  ;; replaces all instance of b with a
   (let loop ((tr tr))
    (if (empty-tree? tr)
        tr
        (let ((root (root-value tr)))
          (if (equal? b root)
              (tree a
                (loop (left-subtree tr))
                (loop (right-subtree tr)))
              (tree root
                (loop (left-subtree tr))
                (loop (right-subtree tr)))))))))

(define split-nth
  (lambda (ls n)
   (let loop ((ls ls) (c 0))
    (cond
      [(null? ls) #f]
      [(= c n) (values '() ls)]
      (else
       (call-with-values
         (lambda ()
           (loop (cdr ls) (add1 c)))
         (lambda (mae ato)
           (values (cons (car ls) mae) ato))))))))

(define deal          ;named "partition" by hiroi, but is essentially the same as dammer's deal
  (lambda (ls)
    (let loop ((ls ls) (c 0))
      (cond
        [(null? ls) (values '() '())]
        (else
         (call-with-values
           (lambda ()
             (loop (cdr ls) (add1 c)))
           (lambda (ls1 ls2)
             (if (even? c)
                 (values (cons (car ls) ls1) ls2)
                 (values ls1 (cons (car ls) ls2))))))))))

(define deal1         ;; list version of the above. before it seemed impossible, but now, not so bad at all
  (lambda (ls)
   (let-values (((ls1 ls2)
    (let loop ((ls ls) (c 0))
      (cond
        [(null? ls) (values '() '())]
        (else
         (call-with-values
           (lambda ()
             (loop (cdr ls) (add1 c)))
           (lambda (ls1 ls2)
             (if (even? c)
                 (values (cons (car ls) ls1) ls2)
                 (values ls1 (cons (car ls) ls2))))))))
                 ))
     (list ls1 ls2))))

(define split-find
  (lambda (x ls)
    (let loop ((ls ls))
      (cond
        [(null? ls) #f]
        [(equal? x (car ls))
           (values '() ls)]
        (else
         (call-with-values
           (lambda ()
             (loop (cdr ls)))
           (lambda (mae ato)
             (values (cons (car ls) mae) ato))))))))

(define split-ge
  (lambda (n ls)           ;; returns two lists, one of elements less than n...
   (let loop ((ls ls))
     (if (null? ls)
         (values '() '())
         (call-with-values
           (lambda ()
             (loop (cdr ls)))
           (lambda (pass fail)
             (if (> n (car ls))
                   (values (cons (car ls) pass) fail)
                   (values pass (cons (car ls) fail)))))))))

(define pack
  (lambda (ls)
    (letrec ((big-loop
               (lambda (ls)
                 (if (null? ls)
                     '()
                     (let-values (((packed the-rest) (little-loop (car ls) ls)))
                       (cons packed (big-loop the-rest))))))
             (little-loop
               (lambda (x ls)
                (let loop ((ls ls))
                 (if (or (null? ls) (not (equal? x (car ls))))
                     (values '() ls)
                     (call-with-values
                       (lambda ()
                         (loop (cdr ls)))
                       (lambda (to-pack the-rest)
                         (values (cons (car ls) to-pack) the-rest))))))))
      (big-loop ls))))

(define pack-num-list
  (lambda (ls)
    (letrec ((big-loop
               (lambda (ls)
                 (if (null? ls)
                     '()
                     (let-values (((packed the-rest) (little-loop (car ls) ls)))
                       (cons packed (big-loop the-rest))))))
             (little-loop
               (lambda (first ls)
                 (let loop ((ls ls) (c first))
                   (if (or (null? ls) (not (equal? (car ls) c)))
                       (if (equal? first (sub1 c))
                           (values first ls)
                           (values (cons first (sub1 c)) ls))
                       (call-with-values
                         (lambda ()
                            (loop (cdr ls) (add1 c)))
                         (lambda (to-pack the-rest)
                           (values to-pack the-rest))))))))
      (big-loop ls))))

(define expand-num-list
  (lambda (ls)
    (letrec ((big-loop
               (lambda (ls)
                 (if (null? ls)
                     '()
                     (unpack (car ls) ls))))
             (unpack
               (lambda (range ls)
                 (if (integer? range)
                     (cons range (big-loop (cdr ls)))
                     (let ((first (car range)) (last (cdr range)))
                        (let loop ((c first))
                          (if (= c last)
                            (cons last (big-loop (cdr ls)))
                            (cons c (loop (add1 c))))))))))
      (big-loop ls))))



#|

omg so hard!!
(trace-define rpn-simplify ;; '(1 2 +) -> 3
  (lambda (ls)
  (define saved-proc 'foo)
  (define the-rest 'bar)
  (if (null? ls) #f ;'is this supposed to happen?
   (let ((expr
            (let loop ((ls ls))
              (cond
                [(null? ls) 'oh-no]
                [(procedure? (eval (car ls)))
                 (set! saved-proc (car ls))
                 (set! the-rest (cdr ls))
                 '()]
                (else
                   (cons (car ls) (loop (cdr ls))))))))

    (cond
    ;  [(procedure? (eval (car ls)))
     ;    (values ls (cdr ls))]
      [(null? (cdr ls))
         ls]
      [(procedure? (eval saved-proc))
        (let ((value (apply (eval saved-proc) expr)))
          (if (equal? value (car ls))
              ls
             (values value the-rest)))]
      (else
        'oops))))))

(define rpn-eval
  (lambda (ls)
   (let-values (((useful worthless)
    (let loop ((ls ls))
     (cond
       [(null? ls) 'something-is-wrong]
       [(null? (cdr ls))
         ls]
       (else
         (let-values (((value the-rest) (rpn-simplify ls)))
            (if (null? the-rest)
               value
               (rpn-simplify (cons value (loop the-rest))))))))))
     useful)))


~ (rpn-eval '(1 2 + 3 4 + *))

~ (rpn-simplify (cons 3 (rpn-eval (3 4 + *))))

~ (rpn-simplify (cons 3 (rpn-simplify (cons 7 (rpn-eval (*))))))
-----------------------------------
~ (rpn '(1 2 + 3 4 + 5 6 + * *))

~ (rpn-simplify (cons 3 (loop (3 4 + 5 6 + * *))))

~ (rpn-simplify (cons 3 (cons 7 (loop (5 6 + * *)))))

~ (rpn-simplify (cons 3 (cons 7 (cons 11 (loop (* *))))))




|#





    |#





















    ;; compresses a list of integers ~ (pack-num-list '(1 2 3 5 7 8 10))
;                                    ~ ((1 . 3) 5 (7 . 8) 10)