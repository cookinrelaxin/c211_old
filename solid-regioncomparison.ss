(import (c211 matrix) (c211 image) (c211 color) (c211 tree))

(define (solid-region?1 img r c n)
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

(define solid-region?2
 (lambda (img r c n)
    (let ([clr (image-ref img r c)]
          [rend (+ r (expt 2 n))]
          [cend (+ c (expt 2 n))])

      (let rloop ([r r])
         (or (= r rend)
             (let cloop ([c c])
                (or
                    (and (= c cend)      ;; where's the false case?
                         (rloop (+ r 1)))
                    (and (color-equal? (image-ref img r c) clr) ; should be here
                         (cloop (+ c 1))))))))))

(define solid-region?3
  (lambda (img r c n)
    (let* ([clr (image-ref img r c)]
           [rows (image-rows img)]
           [cols (image-rows img)]
           [rend
             (let ((rend (+ r (expt 2 n))))
               (if (> rend rows)
                    rows
                    rend))]
           [cend
             (let ((cend (+ c (expt 2 n))))
               (if (> cend cols)
                    cols
                    cend))]
          )
      (let loop ((current-row r) (current-col c))
        (cond
          [(= current-row rend) #t]
          [(= current-col cend)
             (loop (add1 current-row) c)]
          [(color-equal? (image-ref img current-row current-col) clr)
                 (loop current-row (add1 current-col))]
          (else #f))))))

(define solid-region?4
  (lambda (img r c n)
    (let* ([clr (image-ref img r c)]
           [rows (image-rows img)]
           [cols (image-rows img)]
           [rend
             (let ((rend (+ r (expt 2 n))))
               (if (> rend rows)
                    rows
                    rend))]
           [cend
             (let ((cend (+ c (expt 2 n))))
               (if (> cend cols)
                    cols
                    cend))]
          )
      (trace-let loop ((current-row r) (current-col c))
        (or
            (= current-row rend)
            (and (= current-col cend)
                 (loop (add1 current-row) c))
            (and (< current-row rend)  ; this is it
                 (< current-col cend)
                 (color-equal? (image-ref img current-row current-col) clr)
                 (loop current-row (add1 current-col))))))))

;; both annoying and confusing
(define save #f)
(define solid-region?5
  (lambda (img r c n)
    (let* ([clr (image-ref img r c)]
           [rows (image-rows img)]
           [cols (image-rows img)]
           [rend
             (let ((rend (+ r (expt 2 n))))
               (if (> rend rows)
                    rows
                    rend))]
           [cend
             (let ((cend (+ c (expt 2 n))))
               (if (> cend cols)
                    cols
                    cend))]
          )
      (trace-let loop ((current-row r) (current-col c))
        (cond
            [(= current-row rend)
             (call/cc
               (lambda (cont)
                 (set! save cont)
                 #t))]
            [(= current-col cend)
                 (loop (add1 current-row) c)]
            [(color-equal? (image-ref img current-row current-col) clr)
                 (loop current-row (add1 current-col))]
            (else
             (call/cc
               (lambda (cont)
                 (set! save cont)
                 #f))))))))


(define log2n
  (lambda (n)
   (if (> 2 n) 0
    (let loop ((x 2) (i 1))
      (if (>= x n) i
          (loop (* x 2) (add1 i)))))))

(define black-or-white->str
  (lambda (img row column)
   (let ((clr (image-ref img row column)))
    (cond
      [(color-equal? clr white) "11"]
      [(color-equal? clr black) "10"]
      (else
       #f)))))

(define image->string1
  (lambda (img)
      (let loop ((r 0) (c 0) (n (log2n (image-rows img))))
          (or
              (and (solid-region?1 img r c n)
                  (black-or-white->str img r c))
              (let ((n (sub1 n)))
                (string-append "0"
                  (loop r c n)
                  (loop r (+ c (expt 2 n)) n)
                  (loop (+ r (expt 2 n)) c n)
                  (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n)))))))

(define image->string2
  (lambda (img)
      (let loop ((r 0) (c 0) (n (log2n (image-rows img))))
          (or
              (and (solid-region?2 img r c n)
                   (black-or-white->str img r c))
              (let ((n (sub1 n)))
                (string-append "0"
                  (loop r c n)
                  (loop r (+ c (expt 2 n)) n)
                  (loop (+ r (expt 2 n)) c n)
                  (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n)))))))

(define image->string3
  (lambda (img)
      (let loop ((r 0) (c 0) (n (log2n (image-rows img))))
          (or
              (and (solid-region?3 img r c n)
                   (black-or-white->str img r c))
              (let ((n (sub1 n)))
                (string-append "0"
                  (loop r c n)
                  (loop r (+ c (expt 2 n)) n)
                  (loop (+ r (expt 2 n)) c n)
                  (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n)))))))

(define image->string4
  (lambda (img)
      (let loop ((r 0) (c 0) (n (log2n (image-rows img))))
          (or
              (and (solid-region?4 img r c n)
                   (black-or-white->str img r c))
              (let ((n (sub1 n)))
                (string-append "0"
                  (loop r c n)
                  (loop r (+ c (expt 2 n)) n)
                  (loop (+ r (expt 2 n)) c n)
                  (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n)))))))

(define image->string5
  (lambda (img)
      (let loop ((r 0) (c 0) (n (log2n (image-rows img))))
          (or
              (and (solid-region?4 img r c n)
                   (black-or-white->str img r c))
              (let ((n (sub1 n)))
                (string-append "0"
                  (loop r c n)
                  (loop r (+ c (expt 2 n)) n)
                  (loop (+ r (expt 2 n)) c n)
                  (loop (+ r (expt 2 n)) (+ c (expt 2 n)) n)))))))