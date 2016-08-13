;; sorted by category.. lists, trees, images, vectors, matrices, unary, numbers/ math stuff
;; not every procedure, but all the interesting ones

;; unary! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define u-lt?
  (lambda (ls1 ls2)
    (or
      (and (null? ls1) (not (null? ls2)))
      (and
         (and (not (null? ls1)) (not (null? ls2)))
         (u-lt? (cdr ls1) (cdr ls2))))))

(define u-min
  (lambda (ls1 ls2)
    (if
    (u-lt? ls1 ls2)
        ls1
        ls2)))

(define u-double
  (lambda (ls)
    (if (null? ls) '()
        (cons 'l (cons 'l (u-double (cdr ls)))))))

(define u-pow-two
  (lambda (ls)
   (if (null? ls) '(l)
    (let loop ((ls1 '(l l)) (ls2 (cdr ls)))
      (if (null? ls2)
          ls1
          (loop (u-double ls1) (cdr ls2)))))))

(define u-add
  (lambda (ls1 ls2)
    (if (null? ls1)
          ls2
         (cons (car ls1) (u-add (cdr ls1) ls2)))))

(define u-sub
  (lambda (ls1 ls2)
    (cond
      [(and (null? ls1) (not (null? ls2)))
         #f]                               ;; that is, if ls1 - ls2 is negative
      [(null? ls1) ls1]
      [(null? ls2) ls1]
       (else
          (u-sub (cdr ls1) (cdr ls2))))))

(define u-mul
  (lambda (ls1 ls2)
   (if (or (null? ls2) (null? ls1))
       '()
     (let loop ((ls2 ls2))
      (if (null? ls2)
          '()
          (u-add ls1 (loop (cdr ls2))))))))

(define u-quo
  (lambda (ls1 ls2)
    (if (null? ls2) "undefined"
     (let loop ((ls1 (cons 'l ls1)))
      (let ((next (u-sub ls1 ls2)))       ;; you should memorize this one,
       (if (or (null? next) (not next))                ;;  it's not so intuitive
           '()
           (cons 'l (loop next))))))))

(define u-rem
  (lambda (ls1 ls2)
     (let loop ((ls1 ls1))
      (let ((next (u-sub ls1 ls2)))
       (cond
         [(null? next) '()]
         [(not next) ls1]
         (else
           (loop next)))))))


(define u-log
  (lambda (ls)
   (let loop ((base '(l l)))             ;; in this procedure, the base is two
     (let ((sub (u-sub base ls)))
      (cond
       [(null? sub) '(l)]
       [(pair? sub) '()]
       (else
        (let ((next (u-mul '(l l) base)))
              (cons 'l (loop next)))))))))

(define u-max
  (lambda (ls)                           ;; takes a list of lists
   (define current-max '())
    (let loop ((ls ls))
       (cond
         [(null? ls) current-max]
         [(pair? (u-sub (car ls) current-max))
            (set! current-max (car ls))
            (loop (cdr ls))]
         (else
        (loop (cdr ls)))))))

;; lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define map-ran
  (lambda (proc1 proc2 ls p)
    (map
      (lambda (x)
        (if (<= (random 1.0) p)
            (proc1 x)
            (proc2 x)))
      ls)))

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

(define sublist?1
 (lambda (ks ls)
  (or
    (prefix? ls ks)
    (sublist?1 ks (cdr ls)))))

;;sorting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; numbers/ math stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define righten
 (lambda (z)
  (let ((p (expt 10 3)))         ;; this is cool
    (/ (round (* p z)) p))))

(define clock-distances
  (lambda (n m)
    (cond
      [(= n m) (list 0 0)]
      [(< n m) (list (- m n) (- (+ n 12) m))]
      (else (list (- (+ m 12) n) (- n m))))))

(define prime?
  (lambda (n)
    (let loop ((m 3))
     (cond
      [(= n 2) #t]
      [(even? n) #f]
      [(and (not (= (/ n m) 1)) (integer? (/ n m))) #f]
      [(> m (ceiling (sqrt n))) #t]
      (else
       (loop (+ 2 m)))))))

(define primes
  (lambda (n)
    (let loop ((i 0) (p 3))
      (cond
        [(= i n) '()]
        [(zero? i)
         (cons 2 (loop (add1 i) p))]
        [(prime? p)
         (cons p (loop (add1 i) (+ 2 p)))]
        (else
          (loop i (+ 2 p)))))))

(define clock-value
  (lambda (n clock-size)
    (if (integer? (/ n clock-size))
        clock-size
       (- n
         (* (quotient n clock-size) clock-size)))))  ;; hmm..

(define cong?
  (lambda (n m clock-size)
  (= (clock-value n clock-size) (clock-value m clock-size))))

;;returns #t if and only if the two numbers
;; are the same in a clock of size clock-size.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define exp-clock
  (lambda (b n clock-size)
    (clock-value (expt b n) clock-size)))

(define fermat-test
  (lambda (base p)
    (= base (clock-value (expt base p) p))))

(define sum-digits
  (lambda (n)
    (if (= n 0) 0
        (+ (remainder n 10) (sum-digits (/ (- n (remainder n 10)) 10))))))
;; this one is kind of hard to think up
;; it sums the digits of a number

(define sum-digits-one
  (lambda (n)
    (let ([m (sum-digits n)])
       (if (< m 10) m
                   (sum-digits-one m)))))
;; recursively sums the digits to a single digit

(define pi-leibniz
  (lambda (E)
    (let loop ((p 1) (sum 1) (tick 0))
      (cond
        [(< (/ 1 p) E) (* 4.0 sum)]
        [(zero? tick)
         (loop (add1 p) (+ (/ (expt -1 p) (+ (* 2 p) 1)) sum) 1)]
        (else
         (loop (add1 p) (+ (/ (expt 1 p) (+ (* 2 p) 1)) sum) 0))))))

(define subset-sum?
  (lambda (ls target)
    (or
        (and
             (null? ls) (zero? target))
        (= (car ls) target)
        (and
             (not (null? (cdr ls)))
             (or
                 (= (+ (car ls) (cadr ls)) target)
                 (subset-sum? (cdr ls) target)
                 (subset-sum? (cdr ls) (- target (car ls)))
                                                           )))))

;; returns #t if there is some subset
;; of elements from ls that sums to the target, and #f otherwise.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define rpn
 (lambda (ls)
  (let loop ((ls ls) (storage '()))
    (cond
      [(null? ls)
           (if (null? (cdr storage))
              (car storage)
              "invalid expression")]
      [(number? (car ls))
           (loop (cdr ls) (cons (car ls) storage))]
      [(symbol? (car ls))
           (if (or (null? storage) (null? (cdr storage)))
              "stack underflow"
              (let ((val ((eval (car ls)) (cadr storage) (car storage))))
               (loop (cdr ls) (cons val (cddr storage)))))]
      (else
           "invalid data")))))




;; trees! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define height
  (lambda (tr)
    (cond
      [(empty-tree? tr) 0]
      [(leaf? tr) 1]
      (else (max
              (add1 (height (left-subtree tr)))
              (add1 (height (right-subtree tr))))))))

(define leaf-member?
  (lambda (w tr)
    (and
         (not (empty-tree? tr))
         (or
             (and (leaf? tr) (equal? (root-value tr) w))
             (or
                (leaf-member? w (left-subtree tr))
                (leaf-member? w (right-subtree tr)))))))

(define random-walk
  (lambda (tr)
   (if (empty-tree? tr) '()
    (let ([v (root-value tr)] [r (random 2)])
    (cond
      [(leaf? tr) (list v)]
      [(= r 1)
        (cons v (random-walk (left-subtree tr)))]
      (else
        (cons v (random-walk (right-subtree tr)))))))))

(define same-shape?
  (lambda (t1 t2)
    (or
        (and (empty-tree? t1) (empty-tree? t2))
        (and (leaf? t1) (leaf? t2))
        (and
             (and (not (empty-tree? t1)) (not (empty-tree? t2)))
             (and
                (same-shape? (left-subtree t1) (left-subtree t2))
                (same-shape? (right-subtree t1) (right-subtree t2)))))))

(define longest-path
  (lambda (tr)
  (let-values
    ([(height data-list)
          (let loop ((tr tr) (height 0))
           (cond
             [(empty-tree? tr) (values height '())]
             [(leaf? tr) (values (add1 height) (list (root-value tr)))]
             (else
               (call-with-values
                  (lambda ()
                    (let-values (
                          ((p q) (loop (left-subtree tr) (add1 height)))
                          ((r s) (loop (right-subtree tr) (add1 height))))
                        (if (>= p r)
                          (values p q)
                          (values r s))))
                  (lambda (a b)
                    (values a
                            (cons (root-value tr) b)))))))])
     data-list)))

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





;; to tell if a tree is symmetrical, just run this on both subtrees of the tree

;; images! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define image-distance
  (lambda (img1 img2)
    (let loop ((ls1 (image->list img1)) (ls2 (image->list img2)) (i 0))
      (cond
       [(and (null? ls1) (null? ls2)) i]     ;; assume inputs are the same size
       [(color-equal? (car ls1) (car ls2))
        (loop (cdr ls1) (cdr ls2) i)]
        (else
         (loop (cdr ls1) (cdr ls2) (add1 i)))))))

(define image-copy
  (lambda (img)
    (make-image (image-rows img) (image-cols img)
      (lambda (r c)
        (image-ref img r c)))))

(define image-transpose
  (lambda (img)
    (make-image (image-cols img) (image-rows img)
      (lambda (r c)
        (image-ref img c r)))))

(define image-crop
  (lambda (img n)
   (let ((cols (if (< (- (image-cols img) n) 0) 0 (- (image-cols img) n))))
    (make-image
      (image-rows img)
      cols
      (lambda (r c)
        (image-ref img r c))))))

(define on-the-dark-side?
  (lambda (clr)
    (< (+
         (color-ref clr 'red)
         (color-ref clr 'green)
         (color-ref clr 'blue)) 400)))

(define b&w-subimage!
  (lambda (img start-row start-col h w)
    (let*
      ((rows (image-rows img)) (cols (image-cols img))
           (subimage-row-limit
             (let ((row-limit (+ start-row h)))
               (if (> row-limit rows)
                    rows
                    row-limit)))
           (subimage-col-limit
             (let ((col-limit (+ start-col w)))
               (if (> col-limit cols)
                    cols
                    col-limit))))

      (let loop ((r start-row) (c start-col))
       (unless (>= r subimage-row-limit)
         (if (>= c subimage-col-limit)
                (loop (add1 r) start-col)
                (begin
                  (image-set! img r c
                    (if (on-the-dark-side? (image-ref img r c))
                                       black
                                       white))
                  (loop r (add1 c)))))))))

(define (solid-region? img r c n)
    (let ([clr (image-ref img r c)]
       [rend (+ r (expt 2 n))]
       [cend (+ c (expt 2 n))])

      (let rloop ([r r])
         (or (= r rend)
             (and
                  (let cloop ([c c])
                    (or (= c cend)
                        (and (color-equal? (image-ref img r c) clr)
                             (cloop (+ c 1)))))
                  (rloop (+ r 1)))))))

(define black-or-white->str
  (lambda (img row column)
   (let ((clr (image-ref img row column)))
    (cond
      [(color-equal? clr white) "11"]
      [(color-equal? clr black) "10"]
      (else
       #f)))))

(define image->string
  (lambda (img)
      (let loop ((r 0) (c 0) (n (log2n (image-rows img))))
          (or
              (and (solid-region? img r c n)
                   (black-or-white->str img r c))
              (let ((n (sub1 n)))
                (string-append "0"
                  (loop r c n)
                  (loop r (+ c (expt 2 n)) n)
                  (loop (+ r (expt 2 n)) c n)
                  (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n)))))))

(define fill!
  (lambda (img r c n clr)
    (let ((rows (+ r (expt 2 n))) (cols (+ c (expt 2 n))))
      (let loop ((i r) (j c))
        (unless (= i rows)
          (or
              (and (= j cols)
                   (loop (add1 i) c))
              (begin
                   (image-set! img i j clr)
                   (loop i (add1 j)))))))))

(define string->image
  (lambda (quadstring n)
  (define str quadstring)
    (let* ((size (expt 2 n)) (new (make-image size size)))
      (let loop ((r 0) (c 0) (n n))
          (or
              (and (equal? (substring str 0 1) "0")
                     (let ((n (sub1 n)))
                       (begin
                         (set! str (substring str 1 (string-length str)))
                         (loop r c n)
                         (loop r (+ c (expt 2 n)) n)
                         (loop (+ r (expt 2 n)) c n)
                         (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n))))
              (and (equal? (substring str 0 2) "10")
                      (begin
                        (fill! new r c n black)
                        (set! str (substring str 2 (string-length str)))
                        (when (equal? str "") new)))
              (and (equal? (substring str 0 2) "11")
                     (begin
                        (fill! new r c n white)
                        (set! str (substring str 2 (string-length str)))
                        (when (equal? str "") new))))))))












;; strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define char-count
  (lambda (str char)
    (let ((l (string-length str)))
      (let loop ((i 0))
        (cond
          [(= i l) 0]
          [(char=? char (string-ref str i))
             (+ 1 (loop (add1 i)))]
          (else
           (loop (add1 i))))))))

;; vectors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vector-combine!
  (lambda (v1 proc v2)
   (let ((l (vector-length v1)))
    (let loop ((i 0))
      (if (= i l)
          (values)
          (begin
               (vector-set! v1 i
                           (proc (vector-ref v1 i)
                                 (vector-ref v2 i)))
               (loop (add1 i))))))))

;; matrices! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sum-rows
  (lambda (mat)
    (let ((rows (matrix-rows mat)) (cols (matrix-cols mat)))
      (let loop ((r 0) (c 0) (sum-acc 0))
        (cond
          [(= r rows) '()]
          [(= c cols)
             (cons sum-acc (loop (add1 r) 0 0))]
          (else
            (loop r (add1 c) (+ (matrix-ref mat r c) sum-acc))))))))

(define sum-cols
  (lambda (mat)
    (let ((rows (matrix-rows mat)) (cols (matrix-cols mat)))
      (let loop ((r 0) (c 0) (sum-acc 0))
        (cond
          [(= c cols) '()]
          [(= r rows)
            (cons sum-acc (loop 0 (add1 c) 0))]
          (else
            (loop (add1 r) c (+ (matrix-ref mat r c) sum-acc))))))))

(define sum-diag+
  (lambda (mat)
    (let ((cols (matrix-cols mat)))
      (let loop ((r 0) (c 0) (sum-acc 0))
        (if (= c cols)
            sum-acc
           (loop (add1 r) (add1 c) (+ (matrix-ref mat r c) sum-acc)))))))


(define sum-diag-
  (lambda (mat)
      (let loop ((r 0) (c (sub1 (matrix-cols mat))) (sum-acc 0))
        (if (negative? c)
            sum-acc
           (loop (add1 r) (sub1 c) (+ (matrix-ref mat r c) sum-acc))))))


(define magic-square?
  (lambda (mat)
   (and (= (matrix-rows mat) (matrix-cols mat))
    (let* ((a (sum-rows mat)) (a1 (apply = a))
           (b (sum-cols mat)) (b1 (apply = b)))
       (and
          (and a1 b1)
          (= (car a) (car b) (sum-diag+ mat) (sum-diag- mat)))))))

; algorithms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
Computing the Least Energy Seam
Input: Energy matrix em and positive integer n
Output: List of column indices representing a connected top to bottom seam of minimal total energy
Algorithm:

    Let cm be a copy of em.
    Let sm be a matrix of the same size as em, filled with zeroes.
    Starting with the next to last row and moving upwards:
        For each column c in the first n locations of the current row:
            Replace the cth entry in this row of sm with the column corresponding to the best neighboring path in the row directly below this one. (Make use of best-direction and cropped-matrix-ref.)
            Replace the cth entry in this row of cm with the cost of the newly computed path. This requires adding the value currently in cm at this location to the value in cm (on the row directly below this one) that corresponds to the best neighbor.
    Use seam-origin to find the start of the best seam and walk-seam to trace out and return the actual seam.

Note: For each row, the cth entry on that row in sm contains the start of the best known path emanating from that location and going to the bottom edge. The corresponding entry in cm contains the total energy of the best path emanating at that location.









Mergesort a list ls according to a binary relation rel?

    If the list is empty, then return it.
    Group the sorted sequences in ls to obtain the list seqs.
    If there is just one sequence in seqs, then return that sequence.
    Cut the number of sequences in seqs in half by merging adjacent sequences and then go to step 3.









Quicksort a list ls according to a binary relation rel?

    If ls has less than two elements, then return it.
    Pick an element from ls and call it the pivot.
    Divide the remaining elements into two sublists, left and right, such that everything in left comes before the pivot (according to rel?) and right is what's left over.
    Quicksort left and Quicksort right, and return the result of joining the sorted sublists together with the pivot in the middle.