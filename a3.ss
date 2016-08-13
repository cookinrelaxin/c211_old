;; Zachary Feldcamp
;; zachary.feldcam@umail.iu.edu
;; lab Donkey Kong
;; I worked alone

;; 1.a. the procedure spin-roulette expects no arguments and returns an integer
;; in the range 1 to 5, inclusive, such that each of the five possible return
;; values is equally likely.
(define spin-roulette
  (lambda ()
    (+ (random 5) 1)))

;; 1.b. the procedure pick-ball simulates the picking of a ball from a box of
;; 50 red and 50 blue balls, and expects no arguments. it returns the symbol red
;; or the symbol blue.
(define pick-ball
  (lambda ()
    (cond
      [(= (random 2) 0) 'blue]
      (else 'red))))

;; 2. the procedure echo accepts any kind of value and returns that value
(define echo
  (lambda (value)
    n))

;; 3. the procedure toggle takes a bit and returns the opposite bit (o or l)
(define toggle
  (lambda (bit)
    (cond
      [(equal? bit 'l) 'o]
      [(equal? bit 'o) 'l]
      (else 'impossible))))

;; 4.a. the procedure add-bit-no-carry computes the addition of bits without the
;; carry ***explain more?***
(define add-bit-no-carry
  (lambda (bit1 bit2)
    (cond
      [(and (equal? bit1 'o) (equal? bit2 'o)) 'o]
      [(and (equal? bit1 'o) (equal? bit2 'l)) 'l]
      [(and (equal? bit1 'l) (equal? bit2 'o)) 'l]
      [(and (equal? bit1 'l) (equal? bit2 'l)) 'o]
      (else 'impossible))))

;; 4.b. the procedure add-bit-carry takes three bits where bit1 and bit2
;; represent the bits that are to be added, and bit3 represents the carry bit
;; from a previous operation. The procedure add-bit-carry returns the carry
;; resulting from the addition of bit1, bit2, and bit3. ***unsure***
(define add-bit-carry
  (lambda (bit1 bit2 bit3)
    (cond
      [(and (equal? bit1 'o) (equal? bit2 'o) (equal? bit3 'o)) 'o]
      [(and (equal? bit1 'l) (equal? bit2 'o) (equal? bit3 'o)) 'o]
      [(and (equal? bit1 'o) (equal? bit2 'l) (equal? bit3 'o)) 'o]
      [(and (equal? bit1 'o) (equal? bit2 'o) (equal? bit3 'l)) 'o]
      [(and (equal? bit1 'l) (equal? bit2 'l) (equal? bit3 'o)) 'l]
      [(and (equal? bit1 'l) (equal? bit2 'o) (equal? bit3 'l)) 'l]
      [(and (equal? bit1 'o) (equal? bit2 'l) (equal? bit3 'l)) 'l]
      [(and (equal? bit1 'l) (equal? bit2 'l) (equal? bit3 'l)) 'l]
      (else 'impossible))))

;; 5. the procedure lt-bit? takes two bits and returns #t if and only if the
;; first bit is less than the second bit.
(define lt-bit?
  (lambda (bit1 bit2)
    (cond
      [(and (equal? bit1 'o) (equal? bit2 'o)) #f]
      [(and (equal? bit1 'o) (equal? bit2 'l)) #t]
      [(and (equal? bit1 'l) (equal? bit2 'o)) #f]
      [(and (equal? bit1 'l) (equal? bit2 'l)) #f]
      (else 'impossible))))

;; 6. the predicate pythagorean? determines whether any three given integers are
;; Pythagorean in the sense that the sum of the square of two integers equals
;; the square of the third. the first integer is less than the second, which
;; is less than the third. ***possible problem in the ordering.... does the
;; first value need to be less than the second?***
(define pythagorean?
  (lambda (integer1 integer2 integer3)
    (equal?
      (* integer3 integer3) (+ (* integer1 integer1) (* integer2 integer2)))))

;; 7. the predicate in-circle? takes two numbers x and y and returns #t if and
;; only if the point ( x, y ) is inside (not on the circumference) the circle
;; with center (0, 0) and radius one.
(define in-circle?
  (lambda (x y)
    (<= (+ x y) 2)))

;; 8.a. the procedure smaller takes two numbers and returns the smaller of the
;; two
(define smaller
  (lambda (x1 x2)
    (cond
      [(< x1 x2) x1]
      [(< x2 x1) x2]
      (else x1))))

;; 8.b. the procedure smallest takes three numbers and returns the smallest
(define smallest
  (lambda (x1 x2 x3)
    (smaller (smaller x1 x2) x3)))

;; 9. the procedure median takes three numbers and returns the median value
(define median
  (lambda (x1 x2 x3)
    (cond
      [(and (<= x1 x2) (>= x1 x3)) x1]
      [(and (<= x1 x3) (>= x1 x2)) x1]
      [(and (<= x2 x1) (>= x2 x3)) x2]
      [(and (<= x2 x3) (>= x2 x1)) x2]
      [(and (<= x3 x1) (>= x3 x2)) x3]
      [(and (<= x3 x2) (>= x3 x1)) x3]
      (else x1))))

;;