;==============================================
(define (inc x)
  (+ x 1))

(inc 4)

(define (dec x)
  (- x 1))

(dec 4)


(define (nil)
  ('())
  )

;==============================================
;e 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (* (- 6
            2)
         (- 2
            7))))
;e 1.3
(define (sum-of-squares x y)
  (+ (* x x)
     (* y y)))
(define (sum-of-larger-square x y z)
  (cond ((and (> x y) (> z y)) (sum-of-squares x z))
        ((and (> y x) (> z x)) (sum-of-squares y z))
        (else (sum-of-squares x y))
        ))

(sum-of-larger-square 1 2 3)
(sum-of-larger-square 1 3 2)
(sum-of-larger-square 2 1 3)
(sum-of-larger-square 2 3 1)
(sum-of-larger-square 3 1 2)
(sum-of-larger-square 3 2 1)
(sum-of-larger-square 2 1 1)
(sum-of-larger-square 1 2 1)
(sum-of-larger-square 1 1 2)

;e 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 2 3)
(a-plus-abs-b 2 -3)

;e 1.5
(define (p) (p))
(define (test x y) (if (= x 0) 0 y))

(test 0 (p))

;Normal order
(test 0 (p))
(if (= 0 0) 0 (p))
(if #t 0 (p))
(0)
;infinite

;Applicative order
(test 0 (p))
(test 0 (p))
;infinite, stuck evaluating (p)

;newton example
(define (average x y) (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 2)
(sqrt 2)

;e1.7
(define (btr-sqr-itr guess prev-guess x)
  (if (< (abs (- guess prev-guess)) 0.001)
    guess
    (btr-sqr-itr (improve guess x) guess x)))

(btr-sqr-itr 16.0 1 16)

;e1.8
(define (cube-rt-itr guess prev-guess x)
  (if (< (abs (- guess prev-guess)) 0.0001)
    guess
    (cube-rt-itr (cube-imprv guess x) guess x)))

(define (cube-imprv guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(cube-rt-itr 1.0 125 125)

;chp1.2
(define (factorial n)
  (if (= n 1) 1 (* n (factorial (- n 1)))))

(factorial 3)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product)
            (+ counter 1))))
  (iter 1 1))

;e1.9
(define (plus a b)
  (if (= a 0) b (inc (plus (dec a) b))))

(plus 4 5)
(inc (plus (dec 4) 5))
      (inc (plus (dec 3) 5))
           (inc (plus (dec 2) 5))
                (inc (plus (dec 1) 5))
                     (inc 5)

(define (plus a b)
  (if (= a 0) b (plus (dec a) (inc b))))

(plus 4 5)
(plus (dec 4) (inc 5))
(plus (dec 3) (inc 6))
(plus (dec 3) (inc 6))
(plus (dec 2) (inc 7))
(plus (dec 1) (inc 9))

;e1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 1)
(A 2 1)
(A 3 3)

;fibonacci
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(fib 30)

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

;change for 1, given .5, .25, .10 and .05
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (= kinds-of-coins 0))
         0)
        (else
          (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (first-denomination
                             kinds-of-coins))
                 kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

(count-change 100)

;e1.11
;recursive
(define (f11 n)
  (cond ((< n 3) n)
        (else (+ (f11 (- n 1))
                 (* 2 (f11 (- n 2)))
                 (* 3 (f11 (- n 3)))))))

(f11 -1)
(f11 0)
(f11 1)
(f11 2)
(f11 3)
(f11 4)
(f11 5)
(f11 6)
(define (f11 n)
  (define (iter a b c counter)
    (cond ((> 3 n) n)
          ((>= counter n) c)
          (else (iter b c (+ c (* 2 b) (* 3 a)) (+ counter 1)))))
  (iter 0 1 2 2))

;e1.12 pascal's triangle
(define (pascal row col)
  (cond ((and (= row 0) (= col 0) 1))
        ((= row 0) 0)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

(pascal 0 0)
(pascal 1 0)
(pascal 1 1)
(pascal 2 0)
(pascal 2 1)
(pascal 2 2)
(pascal 3 0)
(pascal 3 1)
(pascal 3 2)
(pascal 3 3)
(pascal 4 0)
(pascal 4 1)
(pascal 4 2)
(pascal 4 3)
(pascal 4 4)

;e1.14
(count-change 11)
(cc 11 5)
(+ (cc 11 4)
   (cc -39 5))
(+ (+ (cc 11 3)
      (cc -14 4))
   0)
(+ (+ (+ (cc 11 2)
         (cc 1 3))
      0)
   0)
(+ (+ (+ (+ (cc 11 1)
            (cc 6 2))
         (+ (cc 1 2)
            (cc -9 3))
         )
      0)
   0)
(+ (+ (+ (+ (+ (cc 11 0)
               (cc 10 1))
            (+ (cc 6 1)
               (cc 1 2))
         (+ (+ (cc 1 1)
               (cc -4 2))
            0))
         )
      0)
   0)
;.... demasiado largo!
;e 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.001))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

;exponentiation
(define (expt b n)
  (if (= n 0) 1 (* b (expt b (- n 1)))))
(expt 2 100000)

(define (expt num n)
  (define (iter b counter)
    (if (= counter n)
      b
      (iter (* b num) (+ counter 1))))
  (iter 1 0))


(fexp 2 100000)
(define (fexp num n)
  (cond ((= n 0) 1)
        ((even? n) (square (fexp num (/ n 2))))
        (else (* num (fexp num (- n 1))))))

;e 1.16
(define (fexp num ex)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 num ex))

;e 1.17
(define (mul a b)
  (if (= b 0)
    0
    (+ a (mul a (- b 1)))))

(mul 3 5)
(define (double a)
  (* a 2))
(define (halve a)
  (/ a 2))

(define (fmul num n)
  (cond ((= n 1) num)
        ((even? n) (fmul (double num) (halve n)))
        (else (+ num (fmul num (- n 1))))))
(fmul 3 6)

(define (fmul num n)
  (define (iter a b c)
    (cond ((= c 0) a)
          ((even? c) (iter a (double b) (halve c)))
          (else (iter (+ a b) b (- c 1)))))
  (iter 0 num n))

;e 1.18
(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(define (fib n)
  (define (iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (iter a
                 b
                 (+ (square p) (square q))
                 (+ (* 2 p q) (square q))
                 (/ count 2)))
          (else
            (iter (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- count 1)))))
  (iter 1 0 0 1 n))


(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))
(runtime)
(gcd 206 40)

;==============================================
(define (inc n) (+ 1 n))
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 10000))

(define (simp f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((even? k) 2)
             (else 4))
       (y k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))

(simp cube 0.0 1 4)


(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))


;
(define (print-line value)
  (display value)
  (newline))

(define (fixed-point f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
   (define (try guess)
     (print-line guess)
     (let ((next (f guess)))
       (if (close-enough? guess next)
         next
         (try next))))
   (try first-guess))

(define (x-to-the-x y)
  (fixed-point (lambda (x) (/ (log y) (log x)))
               10.0))

(x-to-the-x 10000)

;;excr
;(define (cont-frac n d k)
;  (/ (n k) (d k)))

(define (cont-frac n d k)
  (define (iter k-term)
    (cond ((= k-term k) (/ (n k-term) (d k-term)))
          (else (/ (n k-term)
                   (+ (d k-term)
                      (iter (+ k-term 1)))))))
  (iter 0))

(define (cont-frac n d reps)
  (define (iter k accum)
    (cond ((= k 0) accum)
          (else (iter (- k 1) (/ (n k) (+ (d k) accum))))))
    (iter reps 0))




(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100000)

(cont-frac i i 10)
(iter 10 0)


;All together now
(define tolerance 0.000001)
(define (fixed-point f first-guess)
   (define (close-enough? v1 v2)
     (< (abs (- v1 v2)) tolerance))
   (define (try guess)
     (let ((next (f guess)))
       (if (close-enough? guess next)
         next
         (try next))))
   (try first-guess))

(define (average x y) (/ (+ x y) 2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))
(sqrt 13)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))
(cube-root 8)


;Newtons method
(define dx 0.000000001)
(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(sqrt 9999999)

;blow my mind
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (/ x y))
    average-damp
    1.0))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x))
    newton-transform
    1.0))

;ex 1.41
(define (inc x) (+ x 1))
(inc 2)

(define (double g)
  (lambda (y) (g (g y))))

((double inc) 1)

(((double (double double)) inc) 5)
(((double (lambda (x) (double (double x)))) inc) 5)
