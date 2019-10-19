;==============================================
(define (nil)
  ('())
  )

(cons 1
      (cons 2
            (cons 3
                  (cons 4 '()))))
(cddr (list 1 2 3 4))
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items)
              (- n 1))))
(list-ref (list 1 2 3 4) 4)

(define (length items)
  (if (null? items)
    0
    (+ 1 (length (cdr items)))))

(define (ilength items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a)
                   (+ 1 count))))
  (length-iter items 0))


(ilength (list 1 2 3 4))

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1)
          (append (cdr list1) list2))))

(append (list 1 2 3) (list 4 5 6))
(cons 1 (append (list 2 3) (list 4 5 6)))
(cons 1 (cons  2 (append (list 3) (list 4 5 6))))
(cons 1 (cons  2 (cons 3 (append (list) (list 4 5 6)))))
(cons 1 (cons  2 (cons 3 (list 4 5 6))))

(define (last-pair items)
  (define (prev items curr)
   (if (null? items)
      curr
      (prev (cdr items) (car items))))
  (prev items '()))

(last-pair (list 1 2 3 4))

(define (reverse items)
  (define (rev items result)
    (if (null? items)
      result
      (rev (cdr items) (cons (car items) result))))
  (rev items '()))

(reverse (list 1 2 3 4))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0)
         1)
        ((or (< amount 0)
             (no-more? coin-values))
         0)
        (else
          (+ (cc
               amount
               (except-first-denomination
                 coin-values))
             (cc
               (- amount
                  (first-denomination
                    coin-values))
               coin-values)))))

(define (first-denomination coin-values) (car coin-values))
(define (except-first-denomination coin-values) (cdr coin-values))
(define (no-more? coin-values) (null? coin-values))
(cc 100 us-coins)
(cc 100 uk-coins)

;==============================================
(define (map proc items)
  (if (null? items)
    '()
    (cons (proc (car items))
          (map proc (cdr items)))))
(map abs (list -1 -2 -3))

;;==============================================
(for-each
  (lambda (x) (newline) (display x))
  (list 57 58 59))

(define (for-each proc items)
  (if (null? items)
    #t
    (begin
      (proc (car items))
      (for-each proc (cdr items)))))
(define (for-each proc items)
  (map proc items)
  #t)

;==============================================
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves (list 1 (list 2 (list 3 (list 4)))))
(list 1 3 (list 5 7) 9)
(cdaddr (list 1 3 (list 5 7) 9))
(caar(list (list 7)))

(define (reverse items)
  (define (rev items result)
    (if (null? items)
      result
      (rev (cdr items) (cons (car items) result))))
  (rev items '()))

(define (dreverse items)
  (define (drev items result)
    (cond ((null? items) result)
          ((not (pair? items)) items)
          (else (drev (cdr items) (cons (drev (car items) '()) result)))))
  (drev items '()))

(define (dreverse items)
  (if (pair? items)
    (reverse (map dreverse items))
    items))

;hand execution
(dreverse #(#(1 2) #(3 4)))
(reverse ((dreverse (list 1 2)) (dreverse (list 3 4))))
(reverse ((reverse ((dreverse (list 1)) (dreverse (list 2)))) (reverse ((dereverse (list 3)) (dreverse (list 4))))))
(reverse ((reverse ((1) (2))) (reverse ((3) (4)))))
(reverse ((2 1) (4 3)))
((4 3) (2 1))

(dreverse (list (list 1 2) (list 3 4) (list 5 6 (list 7 8 (list 9 8 7 (list 6 5 4 3 2 (list 1)))))))
(pair? (list 2 3 4 5))
(cons 1 2)

(pair? (cons 1 '()))
(pair? (list 1))
(pair? (car (list 1 2)))
(pair? (cdr (list 1 2)))
(cdr (list 1 2))
(pair? 1)

(define (fringe tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (fringe (car tree)) (fringe (cdr tree))))))

(fringe (list (list 1 2) (list 3 4)))

(fringe (cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '())))

(fringe (cons (cons 1 (cons 2 '())) (cons (cons 3 (cons 4 '())) '())))
(append (fringe (cons 1 (cons 2 '()))) (fringe (cons (cons 3 (cons 4 '())) '())))
(append (append (fringe 1) (fringe (cons 2 '()))) (append (fringe (cons 3 (cons 4 '()))) (fringe '())))
(append (append (cons 1 '()) (append (fringe 2) (fringe '()))) (append (append (fringe 3) (fringe (cons 4 '())))))
(append (append (cons 1 '()) (append (cons 2 '()) '())) (append (append (cons 3 '()) (append (fringe 4) (fringe '())))))
(append (append (cons 1 '()) (cons 2 '())) (append (append (cons 3 '()) (cons 4 '())) '()))
(append (append (cons 1 '()) (cons 2 '())) (append (cons 3 (cons 4 '())) '()))
(append (cons 1 (cons 2 '())) (cons 3 (cons 4 '())))
(cons 1 (cons 2 (cons 3 (cons 4 '()))))

;==============================================
(define (make-mobile left right)
  (list left right))
(define (make-branch len structure)
  (list len structure))

(define (left-branch branch)
  (car branch))
(define (right-branch branch)
  (cadr branch))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (structure-is-mobile? structure)
  (pair? structure))

(define (branch-weight branch)
  (if (structure-is-mobile? (branch-structure branch))
    (total-weight (branch-structure branch))
    (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (right-branch mobile))
     (branch-weight (left-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balanced? branch)
  (if (structure-is-mobile? (branch-structure branch))
    (balanced? (branch-structure branch))
    true))


(define (balanced? mobile)
  (and (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))
       (= (torque (left-branch mobile))
          (torque (right-branch mobile)))))

(define x (make-mobile (make-branch 1 (make-mobile (make-branch 2 3)
                                                    (make-branch 4 5)))
                       (make-branch 6 7)))
(total-weight x)
(torque (left-branch x))
(torque (right-branch x))
(balanced? x)


;==============================================
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

(scale-tree (list 1 2 (list 3 4 (list 5 6))) 2)
(scale-tree x 2)

(define (square-list items)
  (if (null? items)
    items
    (cons (square (car items)) (square-list (cdr items)))))

(define (square-listm items)
  (map square items))

(square-listm (list 1 2 3 4 5 6))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-treem tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (square-treem subtree)
           (square subtree)))
       tree))

(square-tree (list 1 2 (list 3 4 (list 5 6))))
(square-treem (list 1 2 (list 3 4 (list 5 6))))
(square-treem (list))
(square '())

(define (square-tree tree)
  (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (pair? subtree)
           (tree-map proc subtree)
           (proc subtree)))
       tree))

;==============================================
(define (subsets s)
   (if (null? s)
       (list '())
       (let ((rest (subsets (cdr s))))
         (append rest
                 (map (lambda (x) (cons (car s) x))
                      rest)))))

(subsets '())
(list '())

(subsets '(3))
rest = (subsets (cdr '(3)))
(append (subsets '())
        (map (lambda (x) (cons 3 x))
             (subsets '())))

(append (list '())
        (map (lambda (x) (cons 3 x))
             (list '())))


(subsets '(2 3))
(append (subsets '(3))
        (map (lambda (x) (cons 2 x))
             (subsets '(3))))

(append (append (list '()) (map (lambda (x) (cons 3 x)) (list '())))
        (map (lambda (x) (cons 2 x))
             (append (list '()) (map (lambda (x) (cons 3 x)) (list '())))))


(subsets '(1 2 3))
(append (subsets '(2 3))
        (map (lambda (x) (cons 1 x))
             (subsets '(2 3))))

(append
  (append
    (append (list '())
            (map (lambda (x) (cons 3 x)) (list '())))
    (map (lambda (x) (cons 2 x))
         (append (list '())
                 (map (lambda (x) (cons 3 x)) (list '())))))
  (map (lambda (x) (cons 1 x))
       (append
         (append (list '())
                 (map (lambda (x) (cons 3 x)) (list '())))
         (map (lambda (x) (cons 2 x))
              (append (list '())
                      (map (lambda (x) (cons 3 x)) (list '())))))))



(define (ins term ls)
   (if (null? ls)
     ls
     (append (list (append (car ls) (list term)))
             (ins term (cdr ls)))))

(define (subsets s)
   (if (null? s)
       (list '())
       (let ((rest (subsets (cdr s))))
         (append rest
                 (ins (car s) rest)))))

(ins 3 (list (list)))
(map (lambda (x) (cons 3 x)) (list (list)))

(ins 3 (list (list 1) (list 2) (list 1 2)))
(map (lambda (x) (cons 3 x)) (list (list 1) (list 2)))

(display (list '(1)))
(display '('(1)))





(append (append '() (map (lambda (x) (cons 3 x)) '())) (map (lambda (x) (cons 2 x)) (append '() (map (lambda (x) (cons 3 x)) '()))))

;==============================================
(define (filt predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filt predicate
                     (cdr sequence))))
        (else (filt predicate
                    (cdr sequence)))))

(filt odd? (list 1 2 3 4 5))

(define (accum op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accum op
               initial
               (cdr sequence)))))

(accum + 0 (list 1 2 3))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low
          (enumerate-interval (+ 1 low)
                              high))))

(enumerate-interval 3 8)

(define (enumerate-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(enumerate-tree (list 1 2 (list 3 4 (list 5) 6) (list 7 8)))

(define (sum-odd-squares tree)
  (accum
    +
    0
    (map square
         (filt odd?
               (enumerate-tree tree)))))

(sum-odd-squares (list 1 2 (list 3 4 (list 5) 6) (list 7 8)))

(define (even-fibs n)
  (accum
    cons
    '()
    (filt even?
          (map fib
               (enumerate-interval 0 n)))))

(even-fibs 80)

(define two (lambda (x) 1 3 6 x))
(two 4)

(define (ma p sequence)
  (accum (lambda (x y) (cons (p x) y))
         '()
         sequence))

(ma square (list 1 2 3 4))

(define (app seq1 seq2)
  (accum cons
         seq2
         seq1))

(append (list 1 2) (list 3 4))
(app (list 1 2) (list 3 4))

(accum cons
       (list 3 4)
       (list 1 2))
(cons 1
      (accum cons
             (list 3 4)
             (list 2)))
(cons 1
      (cons 2
            (accum cons
                   (list 3 4)
                   '())))

(cons 1
      (cons 2
            (list 3 4)))

(define (len sequence)
  (accum (lambda (x y) (+ 1 y))
         0
         sequence))


(len (list))
(len (list 1))
(len (list 1 2))
(len (list 1 2 3))
(len (list 1 2 3 (list)))

;==============================================
(define (horner-eval x coefficient-sequence)
  (accum
    (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
    0
    coefficient-sequence))

==>
(lambda (this-coeff higher-terms)) (car list) (accum

(horner-eval 2 (list 1 3 0 5 0 1));result = 79

(1 + x*(3 + x*(0 + x*(5 + x*(0 + x*(1))))))

;==============================================
(define (count-leavess t)
  (accum
    +
    0
    (map (lambda (x) 1) (enumerate-tree t))))

(count-leaves (list 1 (list 2 (list 3 (list 4)))))
(enumerate-tree (list 1 (list 2 (list 3 (list 4)))))
(count-leavess (list 1 (list 2 (list 3 (list 4) 5 6))))
==>
(argA (car (map argC argD)) (accum argA argB (cdr (map argC argD))))

;==============================================

(list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12))

(accum-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(define (accum-n op initial sequences)
  (if (null? (car sequences))
    '()
    (cons (accum op initial (first-picker sequences))
          (accum-n op initial (rest-picker sequences)))))

;argA: seq of the cars of all sequnences in sequence
;argB: seq of the cdr of all sequences in a sequence
(define (first-picker sequences)
  (if (null? sequences)
    '()
    (cons (car (car sequences))
          (first-picker (cdr sequences)))))

(define (rest-picker sequences)
  (if (null? sequences)
    '()
    (cons (cdr (car sequences))
          (rest-picker (cdr sequences)))))

;;MUCH easier solution:
(define (first-picker sequences)
  (map car sequences))
(define (rest-picker sequences)
  (map cdr sequences))

(first-picker (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(rest-picker (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;==============================================
;Matrices: WOOPS jumped ahead, this whole section is misguided
;;;(define v (list (list  1  2  3  4) (list  4  5  6  6) (list  6  7  8  9)))
;;;(define w (list (list 10 20 30 40) (list 40 50 60 60) (list 60 70 80 90)))
;;;
;;;(define (dot-product v w)
;;;  (accum + 0 (enumerate-tree (mapm * v w))))
;;;
;;;(mapm * v w)
;;;(enumerate-tree (mapm * v w))
;;;(dot-product v w)
;;;
;;;(define (map proc items)
;;;  (if (null? items)
;;;    '()
;;;    (cons (proc (car items))
;;;          (map proc (cdr items)))))
;;;
;;;(define (map2 proc seq1 seq2)
;;;  (if (null? seq1)
;;;    '()
;;;    (cons (proc (car seq1)
;;;                (car seq2))
;;;          (map2 proc (cdr seq1) (cdr seq2)))))
;;;
;;;(define (mapm proc mat1 mat2)
;;;  (if (null? mat1)
;;;    '()
;;;    (cons (map2 proc (car mat1) (car mat2))
;;;          (mapm proc (cdr mat1) (cdr mat2)))))
;;;
;;;(map2 + (list 1 2 3) (list 4 5 6))
;;;
;;;(mapm * v w)
;==============================================
(define m (list (list 1 2) (list 3 4) (list 5 6) (list 7 8)))
(define n (list (list 1 2 3 4) (list 5 6 7 8)))
(define v (list 1 2))
(define w (list 5 6))

(define (dot-product v w)
  (accum + 0 (map * v w)))
(dot-product v w)

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))
       m))
(matrix-*-vector m v)

(define (transpose m)
  (accum-n
    cons
    '()
    m))

(transpose (list (list 1 2) (list 3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
      (lambda (x) (matrix-*-vector cols x))
      m)))

(matrix-*-matrix m n)

;==============================================
(define (fld-lft op init seq)
  (define (iter rslt rst)
    (if (null? rst)
      rslt
      (iter (op rslt (car rst))
            (cdr rst))))
  (iter init seq))

(fold-left / 1 (list 1 2 3))
(iter 1 (list 1 2 3))
(iter (/ 1 1) (list 2 3))
(iter (/ 1 2) (list 3))
(iter (/ 0.5 3) (list))
(/ 0.5 3)

(fold-right / 1 (list 1 2 3))
(/ 1 (fold-right / 1 (list 2 3)))
(/ 1 (/ 2 (fold-right / 1 (list 3))))
(/ 1 (/ 2 (/ 3 (fold-right / 1 (list)))))
(/ 1 (/ 2 (/ 3 1)))

(fld-lft list '() (list 1 2 3))
(iter '() (list 1 2 3))
(iter (list '() 1) (list 2 3))
(iter (list (list '() 1) 2) (list 3))


(fold-right list '() (list 1 2 3))
(list 1
      (fold-right list '() (list 2 3)))

(list 1
      (list 2
            (fold-right list '() (list 3))))

(list 1
      (list 2
            (list 3
                  (fold-right list '() (list)))))

(list 1
      (list 2
            (list 3
                  '())))
;==============================================
(define (rvrs seq)
  (fold-right
    (lambda (x y) (append y (list x)))
    '()
    seq))

(define (rvrs seq)
  (fold-left
    (lambda (x y) (cons y x))
    '()
    seq))

(rvrs (list 1 2 3 4 5))

;==============================================
(let ((fact #f))
  (set! fact
        (lambda (n) (if (< n 2) 1
                               (* n (fact (- n 1))))))
  (fact 5))

(let ((U  (lambda (x) (x x)))
      (h  (lambda (g)
            (lambda (n)
              (if (zero? n)
                  1
                  (* n ((g g) (sub1 n))))))))
  ((U h) 5))

;==============================================
(accum
  append
  '()
  (map (lambda (i)
         (map (lambda (j)
                (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 9)))

(define (flatmap proc seq)
  (accum append '() (map proc seq)))

(flatmap (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 9))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair)
        (cadr pair)
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter
         prime-pair?
         (unique-pairs n))))

(prime-sum-pairs 10)

(prime? 2)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        (( divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)

 (define (prime? n)
  (= n (smallest-divisor n))) (= (remainder b a) 0))

;==============================================
(define (permutation s)
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
               (map (lambda (p)
                      (cons x p))
                    (permutation
                      (remove x s))))
             s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(permutation '())
(permutation '(1))
(permutation '(1 2))
(permutation '(1 2 3))
(permutation '(1 2 3 4))
(permutation '(1 2 3 4 5))

;==============================================
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j)
             (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(unique-pairs 5)

(define (triples n s)
  (

(define (unique-triplets n)
   (flatmap (lambda (i)
              (flatmap (lambda (j)
                     (map (lambda (k)
                            (list i j k))
                          (enumerate-interval 1 (- j 1))))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

(unique-triplets 5)

(define (make-triple-sum triple)
  (list (car triple)
        (cadr triple)
        (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(make-triple-sum '(1 2 3))

(define (triple-sum? total triple)
  (= total
     (+ (car triple) (cadr triple) (caddr triple))))

(triple-sum? 6 '(1 2 3))

(define (equal-sum-triplets total n)
  (map make-triple-sum
       (filter
         triple-sum? total
         (unique-triplets n))))

(equal-sum-triplets 6 3)

;==============================================

(define a 1)
(define b 2)
(list a b)
(list 'a 'b)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple '(x apple (apple sauce) y pear lob))
(memq 'apple '(x (apple sauce) y apple pear))
(list '(x (apple sauce) y apple pear))
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))

;==============================================
(variable e?)
(same-variable? v1 v2)
(sum? e)
(addend e)
(augend e)
(make-sum a1 b1)
(product? e)
(multiplier e)
(multiplicand e)
make-product m1 m2)
