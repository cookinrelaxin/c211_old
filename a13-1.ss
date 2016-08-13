#|
Zachary Feldcamp
zfeldcam@umail.iu.edu
I worked alone.
|#

(import (c211 image) (c211 matrix))

;; 1.a. The constructor procedure infinity takes no arguments and returns the chosen representation of infinity.

(define (infinity) 'oo)

;; 1.b. The predicate infinity? takes an item x, and returns #t if x corresponds to our representation of infinity, and #f otherwise. The procedure will still work properly even if a design change is made to the internal representation of infinity.

(define infinity?
  (lambda (x)
    (equal? (infinity) x)))

;; 1.c. The procedure :+ takes two inputs and returns their sum. If one (or both) of the inputs is infinity, then the result is infinity. Otherwise, the result is the same as applying the ordinary addition procedure.

(define :+
  (lambda (x y)
    (or
      (or (and (infinity? x) x)
          (and (infinity? y) y))
      (+ x y))))

;; 1.d. The predicate :< takes two inputs and returns #t if the first is less than the second, and #f otherwise, as shown below. The inputs are either ordinary Scheme numbers or infinity.

(define :<
  (lambda (x y)
    (or
      (and (infinity? x) #f)
      (and
        (not (infinity? x))
         (or
           (infinity? y)
           (< x y))))))

;; 2. The procedure highlight-seam! takes an image, a list of column indices representing a vertical seam, and a color, and destructively mutates the image by replacing all the pixels along the seam with the specified highlight color.

(define highlight-seam!
  (lambda (img col-index-ls clr)
   (let ((rows (image-rows img)))
    (let loop ((r 0) (ls col-index-ls))
        (unless (or (= r rows) (null? ls))
          (image-set! img r (car ls) clr)
          (loop (add1 r) (cdr ls)))))))

;; 3.a