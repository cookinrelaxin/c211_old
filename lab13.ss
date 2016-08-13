(import (c211 matrix))

(define display-matrix
  (lambda (mat)
    (let ((rows (matrix-rows mat))
          (cols (matrix-cols mat)))
      (let rloop ((r 0))
        (if (< r rows)
            (begin
              (let cloop ((c 0))
                (if (< c cols)
                    (begin
                      (display (matrix-ref mat r c))
                      (cloop (add1 c)))))
              (newline)
              (rloop (add1 r))))))))

(define display-matrix1
  (lambda (mat)
    (let ((rows (matrix-rows mat))
          (cols (matrix-cols mat)))
      (let loop ((r 0) (c 0))
        (cond
          [(= rows r) (values)]
          [(= c cols)
            (newline)
            (loop (add1 r) 0)]
          (else
           (display (matrix-ref mat r c))
           (loop r (add1 c))))))))

(define matrix-add
  (lambda (mat1 mat2)
    (let* ((rows (matrix-rows mat1))
           (cols (matrix-cols mat1))
           (new (make-matrix rows cols)))
      (let loop ((r 0) (c 0))
        (cond
          [(= r rows) new]
          [(= c cols)
            (loop (add1 r) 0)]
          (else
           (matrix-set! new r c (+ (matrix-ref mat1 r c) (matrix-ref mat2 r c)))
           (loop r (add1 c))))))))

(define matrix-add1
  (lambda (mat1 mat2)
    (let* ((rows (matrix-rows mat1))
           (cols (matrix-cols mat1))
           (new (make-matrix rows cols)))
      (let rloop ((r 0))
        (if (< r rows)
          (begin
            (let cloop ((c 0))
              (unless (= c cols)
                (matrix-set! new r c (+ (matrix-ref mat1 r c) (matrix-ref mat2 r c)))
                (cloop (add1 c))))
            (rloop (add1 r)))
         new)))))




(define matrix-add2
  (lambda (mat1 mat2)
    (let* ((rows (matrix-rows mat1))
           (cols (matrix-cols mat1))
           (new (make-matrix rows cols)))
      (letrec ((rloop
                (lambda (r)
                  (if (< r rows)
                      (cloop r 0)
                       new)))
               (cloop
                 (lambda (r c)
                   (if (< c cols)
                    (begin
                      (matrix-set! new r c (+ (matrix-ref mat1 r c) (matrix-ref mat2 r c)))
                      (cloop r (add1 c)))
                    (rloop (add1 r))))))
        (rloop 0)))))

(define matrix-add3
  (lambda (mat1 mat2)
    (let* ((rows (matrix-rows mat1))
           (cols (matrix-cols mat1))
           (new (make-matrix rows cols)))
      (do ((r 0
             (do ((c 0
                    (begin
                      (matrix-set! new r c (+ (matrix-ref mat1 r c) (matrix-ref mat2 r c)))
                      (add1 c))))
                    ([= c cols] (add1 r)))))
        ([= r rows] new)))))

;; is it possible to only use disjunctions and conjunctions in scheme,
;; without if statements?

(define positive
  (lambda (n)
    (and (positive? n) n))) ;; this is how!!!

;lets try to define matrix-add without any sort of conditionals

(define matrix-add4
  (lambda (mat1 mat2)
    (let* ((rows (matrix-rows mat1))
           (cols (matrix-cols mat1))
           (new (make-matrix rows cols)))
      (let loop ((r 0) (c 0))
        (or
          (and (= r rows) new)
          (and (= c cols) (loop (add1 r) 0))
          (begin
            (matrix-set! new r c (+ (matrix-ref mat1 r c) (matrix-ref mat2 r c)))
            (loop r (add1 c))))))))