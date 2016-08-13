(define insertion-sort
  (lambda (rel? ls)
    (define insert-in-order
      (lambda (x ls)
        (let loop ((ls ls))
          (cond
            [(null? ls) (list x)]
            [(rel? x (car ls))
             (cons x ls)]
            (else
           (cons (car ls) (loop (cdr ls))))))))
    (let loop ((ls ls))
      (if (null? ls) ls
     (insert-in-order (car ls) (loop (cdr ls)))))))
#|
(define gss
    (lambda (rel? ls)
     (define nextlist '())
      (define group
       (let loop ((ls ls))
        (cond
          [(null? ls) '()]
          [(null? (cdr ls)) (list ls)]
          [(rel? (car ls) (cadr ls))
            (cons (car ls) (loop (cdr ls)))]
          (else
           (set! nextlist ls) (cons (car ls) '())))))
        (if (null? nextlist)
            group
            (cons group (gss rel? (cdr nextlist))))))
#|
(define single-group
  (lambda (rel? ls)
    (if (null? ls) (values '() '())
      (let loop ((ls ls))
        (call-with-values
         (lambda ()
           (cond
             [(null? ls) (values '() '())]
             [(null? (cdr ls)) (values '() '())]
             [(not (rel? (car ls) (cadr ls))) (values '() (cdr ls))]
              (else
               (loop (cdr ls)))))
         (lambda (a b)
              (values (cons (car ls) a) b)))))))


(define gss
  (lambda (rel? ls)
   (let-values  (((a b) (single-group rel? ls)))
     (if (null? a) '()
     (cons a (gss rel? b))))))
  |#














#|
    (define merg
      (lambda (rel? ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(rel? (car ls1) (car ls2)) (cons (car ls1) (merg rel? (cdr ls1) ls2))]
      (else
       (cons (car ls2) (merg rel? ls1 (cdr ls2)))))))

(define merge-adjacent-sequences
   (lambda (rel? gssls)
    (if (or (null? gssls) (null? (cdr gssls)))
      gssls
       (cons (merg rel? (car gssls) (cadr gssls))
        (merge-adjacent-sequences rel? (cddr gssls))))))

(define merge-sort
  (lambda (rel? ls)
    (let loop ([ls (gss rel? ls)])
    (cond
      [(null? ls) ls]
      [(null? (cdr ls)) (car ls)]
     (else
      (loop (merge-adjacent-sequences rel? ls)))))))
  |#