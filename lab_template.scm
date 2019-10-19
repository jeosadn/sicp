(RESTART 1)

;Generic template

;Shared code that will always be used

;First lab
(+ 137 349)

(- 100 334)

(* 5 99)

(/ 10 5)

(+ 2.7 10)

;=====================================
(define (square x) (* x x))

(square 21)

(square 12 1)

;=====================================
(define (sum-of-squares x y)
  (+ (square x)
     (square y)))

(sum-of-squares 3 4)

;=====================================
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;=====================================
(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(abs -5)

;=====================================
(define (abs2 x)
  (if (< x 0)
      (- x)
      x))

(abs2 -5)

;=====================================
(define (inc x)
  (+ x 1))

(inc 4)

(define (dec x)
  (- x 1)
  )

(dec 4)


(define (nil)
  ('())
  )

;=====================================
(define size 2)


(define (mul x y) (* x y))

(mul 2 3)

