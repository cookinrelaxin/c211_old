;; Zachary Feldcamp
;; zfeldcam@umail.iu.edu
;; lab Donkey Kong
;; I worked alone


;; 1.a. this defines a procedure which adds the integer 3 to any given number x
(define add3
  (lambda (x) (+ 3 x)))

;; 1.b. this defines a procedure which subtracts the integer 3 from any given
;; number x
(define subtract3
  (lambda (x) (- x 3)))

;; 2.a. this defines a procedure which returns the thousands-place digit of any
;; given number n
(define thousands
  (lambda (n) (quotient
                (remainder n 10000) 1000)))


;; 2.b. this defines a procedure which outputs the absolute value of n x 10
(define ten-magnitude
  (lambda (n)
    (if (<= n 0) (* n -10)
        (* n 10))))

;; 3.a. this defines a procedure which returns the square of the input n
(define square
  (lambda (n) (* n n)))

;; 3.b. this defines a procedure which returns the sum of two squares x , y
(define add-squares
  (lambda (x y)
    (+ (square x) (square y))))

;; 4.a. this defines a procedure which determines the length of the diagonal of
;; one face of a cube given side length a
(define face-diagonal
  (lambda (a)
    (sqrt (* (* a a) 2))))

;; 4.b. This defines a procedure which returns the length of the diagonal of
;;the body of a cube, given any given side of the cube a
(define body-diagonal
  (lambda (a)
    (sqrt
      (+ (square (face-diagonal a)) (square a)))))

;; 5.a. This defines a procedure which converts degrees celsius to degrees
;; farenheit
(define c->f
  (lambda (degreescelsius)
    (+ (* 9/5 degreescelsius) 32)))

;; 5.b. This defines a procedure which converts degrees farenheit to degrees
;; celsius.
(define f->c
  (lambda (degreesfarenheit)
    (* (- degreesfarenheit 32) 5/9)))

;; 5.c. If applied an infinite number of times, f->c would yield -40. Perhaps
;; because farenheit and celsius represent the same temperature at this value.

;; 6.a. This defines a procedure which takes a non-negative integer representing
;; some number of yards and returns the corresponding number of inches
(define yd->in
  (lambda (yards)
    (* yards 36)))

;; 6.b. This defines a procedure which takes a non-negative integer
;; representing some number of inches and returns the corresponding number of
;; centimeters
(define in->cm
  (lambda (inches)
    (* inches 2.54)))

;; 6.c. This defines a procedure which takes a non-negative integer representing
;; some number of yards and returns the corresponding number of centimeters
(define yd->cm
  (lambda (yards)
    (in->cm (yd->in yards))))

;; 7.a. This defines a procedure which takes a string and returns the leftmost
;; half of the string
(define left-part
  (lambda (n)
    (substring n 0 (quotient (string-length n) 2))))

;; 7.b. This defines a procedure which takes a string and returns the rightmost
;; half of the string
(define right-part
  (lambda (n)
    (if (odd? (string-length n))
        (substring n (+ (quotient (string-length n) 2) 1) (string-length n))
    (substring n (quotient (string-length n) 2) (string-length n)))))

;; 7.c. This defines a procedure which  takes a string and returns a string
;; consisting of only the middle character in the given string if one exists,
;; and the empty string otherwise.
(define middle
  (lambda (n)
    (if (even? (string-length n)) ""
        (substring n
          (round (/ (string-length n) 2))
          (+ (round (/ (string-length n) 2)) 1)))))

;; 8.a. This defines a procedure that takes a non-empty string s and a
;; non-negative integer i, and returns a substring of length one consisting of
;; the character at index i in s.
(define char-at
  (lambda (s i)
    (substring s i
      (+ i 1))))

;; 8.b. This defines a procedure that takes a non-empty string and
;; randomly selects one of the characters in the given string to return as a
;; substring of length one.
(define random-char
  (lambda (n)
    (char-at n (random (string-length n)))))

;; 8.c. This defines a procedure that takes two strings and puts them together
;; into a single string with the word 'as' between them
(define make-simile
  (lambda (s1 s2)
    (string-append (string-append s1 " as ") s2)))

;; 9. This defines a procedure that takes two integers, n and p, where n is an
;; integer and p is a non-negative integer, and returns a randomly selected
;; integer in the closed interval from n to n + p. ***??????????***

(define random-in-range
  (lambda (n p)

    (random (+ n p))))))





10;; This defines a procedure that takes an integer n and a positive integer
;; dist, and returns a randomly selected integer in the range of integers that
;; are dist units away from n in either direction. ***???????????***