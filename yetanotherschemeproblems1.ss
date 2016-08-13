(define single?
  (lambda (ls)
      (and (not (null? ls)) (null? (cdr ls)))))

(define double?
  (lambda (ls)
    (and (not (null? ls)) (not (null? (cdr ls))) (null? (cddr ls)))))

(define longer?
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) #f]
      [(null? ls2) #t]
      (else
       (longer? (cdr ls1) (cdr ls2))))))

(define longer?1
  (lambda (ls1 ls2)
    (or
        (and (not (null? ls1)) (null? ls2))
        (longer? (cdr ls1) (cdr ls2)))))

(define last
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [(null? (cdr ls)) ls]
      (else
       (last (cdr ls))))))

(define butlast
  (lambda (ls)
    (cond
      [(null? ls) ls]
      [(null? (cdr ls)) '()]
      (else
       (cons (car ls) (butlast (cdr ls)))))))

(define take
  (lambda (ls n)
    (if (or (null? ls) (zero? n))
        '()
        (cons (car ls) (take (cdr ls) (sub1 n))))))

(define drop
  (lambda (ls n)
    (if (or (null? ls) (zero? n))
        ls
        (drop (cdr ls) (sub1 n)))))

(define subseq
  (lambda (ls n m)
    (take (drop ls n) (- m n))))

(define group
 (lambda (ls n)
 (define rest '())
  (define loop
    (lambda (ls n)
      (cond
        [(null? ls) ls]
        [(zero? n) (set! rest ls) '()]
          (else
          (cons (car ls) (loop (cdr ls) (sub1 n)))))))
   (let ((loop (loop ls n)))
    (if (null? loop)
        loop
     (cons loop (group rest n))))))


(define group1
 (lambda (ls n)
  (if (null? ls)
      '()
    (cons (take ls n) (group (drop ls n) n)))))

(define group2
 (lambda (ls n)
  (define loop
    (lambda (ls n)
      (cond
        [(null? ls) (values '() ls)]
        [(zero? n) (values '() ls)]
        (else
          (call-with-values
           (lambda ()
              (loop (cdr ls) (sub1 n)))
           (lambda (a b)
              (values
                (cons (car ls) a)
                b
                                 )))))))
   (let-values (((a b) (loop ls n)))
     (if (null? a)
         a
         (cons a (group2 b n))))))

(define group3
 (lambda (ls n)
  (let-values (((a b)
     (let loop ((ls ls) (n n))
       (cond
         [(null? ls) (values '() ls)]
         [(zero? n) (values '() ls)]
         (else
            (call-with-values
               (lambda ()
                 (loop (cdr ls) (sub1 n)))
               (lambda (a b)
                 (values
                   (cons (car ls) a)
                   b
                                    ))))))))
     (if (null? a)
         a
         (cons a (group2 b n))))))

(define position
  (lambda (x ls)
    (let loop ((ls ls) (i 0))
      (if (null? ls) #f
          (if (equal? (car ls) x) i
              (loop (cdr ls) (add1 i)))))))

(define count
  (lambda (x ls)
    (let loop ((ls ls) (c 0))
      (if (null? ls) c
          (if (equal? (car ls) x)
              (loop (cdr ls) (add1 c))
              (loop (cdr ls) c))))))

(define sum-list
  (lambda (ls)
    (apply + ls)))

(define sum-list1
  (lambda (ls)
    (if (null? ls) 0
        (+ (car ls) (sum-list1 (cdr ls))))))

(define max-list
  (lambda (ls)
   (if (null? ls) ls
    (let loop ((ls ls) (m (car ls)))
      (if (null? (cdr ls)) m
          (if (> m (cadr ls))
              (loop (cdr ls) m)
              (loop (cdr ls) (cadr ls))))))))

(define min-list
  (lambda (ls)
   (if (null? ls) ls
    (let loop ((ls ls) (m (car ls)))
      (if (null? (cdr ls)) m
          (if (< m (cadr ls))
              (loop (cdr ls) m)
              (loop (cdr ls) (cadr ls))))))))

(define adjacent?
  (lambda (x y ls)
    (let loop ((ls ls))
      (if (null? ls) #f
          (if (null? (cdr ls)) #f
              (if (equal? (car ls) x)
                  (if (equal? (cadr ls) y)
                      #t
                      (loop (cdr ls)))
                  (if (equal? (car ls) y)
                      (if (equal? (cadr ls) x)  ;; lots of ifs, lol
                          #t
                          (loop (cdr ls)))
                      (loop (cdr ls)))))))))

(define before?
  (lambda (x y ls)
    (let loop ((ls ls))
      (if (or (null? ls) (null? (cdr ls)))
          #f
          (if (and (equal? (car ls) x) (equal? (cadr ls) y))
              (cdr ls)
              (loop (cdr ls)))))))

(define range
  (lambda (n m)
    (if (= n (add1 m)) '()
        (cons n (range (add1 n) m)))))

(define set-of-list
 (lambda (ls)
   (if (null? ls) ls
     (let ((x (set-of-list (cdr ls))))
       (if (member (car ls) x)
          x
         (cons (car ls) x))))))

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(member (car ls1) ls2)
          (union (cdr ls1) ls2)]
      (else
       (cons (car ls1) (union (cdr ls1) ls2))))))

(define intersection
  (lambda (ls1 ls2)
    (cond
      [(or (null? ls1) (null? ls2)) '()]
      [(member (car ls1) ls2)
         (cons (car ls1)
           (intersection (cdr ls1) ls2))]
      (else
       (intersection (cdr ls1) ls2)))))

(define merge-list
  (lambda (rel? ls1 ls2)
   (let loop ((ls1 ls1) (ls2 ls2))
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(rel? (car ls1) (car ls2))
        (cons (car ls1) (loop (cdr ls1) ls2))]
      (else
       (cons (car ls2) (loop ls1 (cdr ls2))))))))

(define group-sorted-sequences
  (lambda (rel? ls)
  (define save #f)
   (let outer-loop ((ls ls))
      (let ((single-group
              (let inner-loop ((ls ls))
                  (cond
                    [(null? ls) ls]
                    [(null? (cdr ls))
                      (set! save '())
                      ls]
                    [(rel? (car ls) (cadr ls))
                       (cons (car ls) (inner-loop (cdr ls)))]
                    (else
                      (set! save (cdr ls))
                      (list (car ls)))))))
          (if (null? save)
              (list single-group)
              (cons single-group (outer-loop save)))))))

(define merge-gss-ls
  (lambda (rel? ls)
   (let loop ((ls ls))
    (cond
      [(null? ls) '()]
      [(null? (cdr ls)) (car ls)]
      (else
       (merge-list rel?
         (merge-list rel? (car ls) (cadr ls))
         (loop (cddr ls))))))))

(define merge-sort
  (lambda (rel? ls)
    (let ((gss-ls (group-sorted-sequences rel? ls)))
     (merge-gss-ls rel? gss-ls))))

(define insertion-sort
  (lambda (rel? ls)
    (define insert-in-order
      (lambda (rel? x ls)
       (let loop ((ls ls))
        (cond
          [(null? ls) (list x)]
          [(rel? x (car ls))
            (cons x ls)]
          (else
           (cons (car ls) (loop (cdr ls))))))))
    (let loop ((ls ls))
      (cond
        [(null? ls) ls]
        (else
          (insert-in-order rel? (car ls) (loop (cdr ls))))))))

(define quick-sort
  (lambda (rel? ls)
    (let loop ((ls ls))
      (if (null? ls) ls
      (let ((first (car ls)))
        (let-values (((p q) (partition (lambda (x) (rel? x first)) (cdr ls))))
          (append (loop p) (list first) (loop q))))))))

(define prefix?
  (lambda (ls potential-prefix)
    (let loop ((ls1 ls) (ls2 potential-prefix))
      (or (null? ls2)
          (and
               (not (null? ls1))
               (equal? (car ls1) (car ls2))
               (loop (cdr ls1) (cdr ls2)))))))

(define suffix?
  (lambda (ls potential-suffix)
   (or (null? potential-suffix)
    (let loop ((ls ls))
      (and
           (not (null? ls))
           (or
               (equal? ls potential-suffix)
               (loop (cdr ls))))))))

(define sublist?
  (lambda (potential-sublist ls)
    (letrec ((big-loop (lambda (potential-sublist ls)
                         (or
                            (null? potential-sublist)
                            (and
                              (not (null? ls))
                              (or
                                  (and
                                    (equal? (car potential-sublist) (car ls))
                                    (little-loop potential-sublist ls))
                                  (big-loop potential-sublist (cdr ls)))))))

             (little-loop (lambda (potential-sublist ls)
                            (let loop ((ls1 potential-sublist) (ls2 ls))
                            (or
                              (null? ls1)
                              (and
                                (not (null? ls2))
                                  (or
                                    (and
                                      (equal? (car ls1) (car ls2))
                                      (little-loop (cdr ls1) (cdr ls2)))
                                  (big-loop potential-sublist (cdr ls)))))))))

   (big-loop potential-sublist ls))))

;; or the easy way..

(define sublist?1
 (lambda (ks ls)
  (or
    (prefix? ls ks)
    (sublist?1 ks (cdr ls)))))