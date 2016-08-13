(define (multiply-list1 lst)
  (define (multiply lst2)
    (if (null? lst2)
        1
       (* (car lst2) (multiply (cdr lst2)))))
  (multiply lst))

(define (multiply-list2 lst)
  (call/cc (lambda (cont) ; ????? cont ???????
    (define (multiply lst2)
      (cond
        [(null? lst2) 1]
        [(zero? (car lst2)) (cont 0)] ; 0 ????, ????0???
        (else
         (* (car lst2) (multiply (cdr lst2))))))
    (multiply lst))))

(define ls*
  (lambda (ls)
    (call/cc
     (lambda (cc)
       (if (null? ls)
           1
         (let ((x (car ls)))
           (if (= 0 x)
               (cc 0)
             (* x (ls* (cdr ls))))))))))

(define fact
  (lambda (x)
    (if (= x 1)
        (call/cc
         (lambda (cc)
           (set! *save* cc)
           1))
      (* x (fact (- x 1))))))

;; oh shit. i get it. call/cc takes whatever is acting upon it,
;;, whatever the stack is, and allows you to manipulate it. the stack itself.
;; holy fuck