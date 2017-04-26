;;; Touretsky Common Lisp
;;; Chapter 8 - Recursion

(defun any-odd-p (xs)
  "Return T if any number in list XS is odd."
  (cond ((null xs) nil)
        ((oddp (first xs)) t)
        (t (any-odd-p (cdr xs)))))

(any-odd-p '(2 4 6 7)) ; T
(any-odd-p '(2 4 6 8)) ; NIL

;; Exercise 8.2

(defun any-odd-p2 (xs)
  "Return T if any number in list XS is odd, using IF."
  (if (null xs)
      nil
      (if (oddp (first xs))
          t
          (any-odd-p2 (cdr xs)))))

(any-odd-p2 '(2 4 6 7)) ; T
(any-odd-p2 '(2 4 6 8)) ; NIL

;; Exercise 8.4

(defun laugh (n)
  "Return a list of n-number of 'ha."
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))

(laugh 4) ; (HA HA HA HA)

;; Exercise 8.5

(defun add-up (xs)
  "Sum all the numbers in list XS."
  (cond ((null xs) 0)
        (t (+ (car xs) (add-up (cdr xs))))))

(add-up '(2 3 7)) ; 12

;; Exercise 8.6

(defun all-odd-p (xs)
  "Return T if all numbers in XS are odd."
  (cond ((equal (length xs) 1) (oddp (car xs)))
        (t (and (oddp (car xs))
                (all-odd-p (cdr xs))))))

(all-odd-p '(1 3 5 7)) ; T
(all-odd-p '(1 3 6 7)) ; NIL

;; Exercise 8.7

(defun my/member (x ys)
  "Return T if X is an element in YS."
  (cond ((null ys) nil)
        ((equal x (car ys)) ys)
        (t (my/member x (cdr ys)))))

(my/member 3 '(1 2 3 4)) ; (3 4)
(my/member 5 '(1 2 3 4)) ; NIL

;; Exercise 8.8

(defun my/assoc (x ys)
  "Return the sublist in YS whose CAR is equal to X."
  (cond ((null ys) nil)
        ((equal x (caar ys)) (car ys))
        (t (my/assoc x (cdr ys)))))

(my/assoc 3 '((1 "one") (2 "two") (3 "three"))) ; (3 "three")

;; Exercise 8.9

(defun my/nth (n xs)
  "Return the Nth element of XS."
  (cond ((zerop n) (car xs))
        (t (my/nth (- n 1) (cdr xs)))))

(my/nth 1 '(a b c)) ; B

;; Exercise 8.10

(defun rec-plus (x y)
  "Recursively add X to Y."
  (cond ((zerop y) x)
        (t (rec-plus (+ x 1) (- y 1)))))

(rec-plus 10 10) ; 20

;; Exercise 8.11

(defun fib (n)
  "Return the Nth Fibonacci number."
  (cond ((equal n 0) 1)
        ((equal n 1) 1)
        (t (+ (fib (- n 1))
              (fib (- n 2))))))

(fib 4) ; 5
(fib 5) ; 8

(defun find-first-odd (xs)
  "Return the first odd number in list XS."
  (cond ((null xs) nil)
        ((oddp (car xs)) (car xs))
        (t (find-first-odd (cdr xs)))))

(find-first-odd '(2 4 6 7 8)) ; 7

(defun last-element (xs)
  "Return the last element of list XS."
  (cond ((null (cdr xs)) (car xs))
        (t (last-element (cdr xs)))))

(last-element '(1 2 3)) ; 3

;; Exercise 8.21

(defun add-nums (n)
  "Return the sum of N, N-1, N-2 ... 0."
  (cond ((zerop n) 0)
        (t (+ n
              (add-nums (- n 1))))))

(add-nums 5) ; 15

;; Exercise 8.22

(defun all-equal (xs)
  "Return T if all elements in XS are the same."
  (cond ((< (length xs) 2) t)
        (t (and (equal (car xs) (cadr xs))
                (all-equal (cdr xs))))))

(all-equal '(i i i i)) ; T
(all-equal '(i i e i)) ; NIL

;; Exercise 8.24

(defun count-down (n)
  "Return the list (N N-1 N-2 ... 1)."
  (cond ((zerop n) nil)
        (t (cons n (count-down (- n 1))))))

(count-down 5) ; (5 4 3 2 1)

;; Exercise 8.25

(defun fact-applicative (n)
  (reduce #'* (count-down n)))

(fact-applicative 5) ; 120

;; Exercise 8.26

(defun count-down-to-zero1 (n)
  "Return the list (N N-1 N-2 ... 0)."
  (cond ((equal n -1) nil)
        (t (cons n (count-down-to-zero1 (- n 1))))))

(count-down-to-zero1 5) ; (5 4 3 2 1 0)

(defun count-down-to-zero2 (n)
  "Return the list (N N-1 N-2 ... 0)."
  (cond ((zerop n) '(0))
        (t (cons n (count-down-to-zero2 (- n 1))))))

(count-down-to-zero2 5) ; (5 4 3 2 1 0)

;; Exercise 8.27

(defun square-list (xs)
  "Return the list of squares of each number in XS."
  (cond ((null xs) nil)
        (t (cons (* (car xs) (car xs))
                 (square-list (cdr xs))))))

(square-list '(3 4 5 6)) ; (9 16 25 36)

;; Exercise 8.28

(defun my/nth (n xs)
  "Return the Nth element of XS."
  (cond ((zerop n) (first xs))
        (t (my/nth (- n 1) (rest xs)))))

(defun my/nth2 (n xs)
  "Return the Nth element of XS."
  (cond ((null xs) nil)
        ((zerop n) (car xs))
        (t (my/nth2 (- n 1) (cdr xs)))))

(my/nth2 5 '(a b c)) ; NIL
(my/nth2 1000 '(a b c)) ; NIL

;; Exercise 8.31

(defun compare-lengths (xs ys)
  "Return which list is longer, or SAME-LENGTH."
  (cond ((and (null xs) (null ys)) 'same-length)
        ((null xs) 'second-is-longer)
        ((null ys) 'first-is-longer)
        (t (compare-lengths (cdr xs) (cdr ys)))))

(compare-lengths '(1 2 3) '(1 2)) ; FIRST-IS-LONGER
(compare-lengths '(1 2) '(1 2 3)) ; SECOND-IS-LONGER
(compare-lengths '(1 2) '(1 2)) ; SAME-LENGTH

;; Exercise 8.32

(defun sum-numeric-elements (xs)
  "Sum only the numeric elements in list XS."
  (cond ((null xs) 0)
        ((numberp (car xs)) (+ (car xs)
                               (sum-numeric-elements (cdr xs))))
        (t (sum-numeric-elements (cdr xs)))))

(sum-numeric-elements '(3 bears 3 bowls and 1 girl)) ; 7

;; Exercise 8.33

(defun my/remove (x xs)
  "Remove all occurrences of X in list XS."
  (cond ((null xs) nil)
        ((equal x (car xs)) (my/remove x (cdr xs)))
        (t (cons (car xs) (my/remove x (cdr xs))))))

(my/remove 3 '(1 2 3 4)) ; (1 2 4)

;; Exercise 8.34

(defun my/intersection (xs ys)
  "Recursive definition of INTERSECTION."
  (cond ((or (null xs) (null ys)) nil)
        ((member (car xs) ys) (cons (car xs) (my/intersection (cdr xs) ys)))
        (t (my/intersection (cdr xs) ys))))

(my/intersection '(1 2 3) '(3 4 5)) ; (3)

;; Exercise 8.35

(defun my/set-difference (xs ys)
  "Recursive definition of SET-DIFFERENCE."
  (cond ((null xs) nil)
        ((null ys) xs)
        ((member (car xs) ys) (my/set-difference (cdr xs) ys))
        (t (cons (car xs) (my/set-difference (cdr xs) ys)))))

(my/set-difference '(1 2 3) '(3 4)) ; (1 2)

;; Exercise 8.36

(defun count-odd1 (xs)
  "Return the number of odd numbers in XS."
  (cond ((null xs) 0)
        ((oddp (car xs)) (+ 1 (count-odd1 (cdr xs))))
        (t (count-odd1 (cdr xs)))))

(count-odd1 '(4 5 6 7 8)) ; 2

;;; CAR/CDR Recursion

(defun find-number (xs)
  (cond ((numberp xs) xs)
        ((atom xs) nil)
        (t (or (find-number (car xs))
               (find-number (cdr xs))))))

;; Exercise 8.39

(defun count-atoms (xs)
  "Return the number of atoms in list/tree XS."
  (cond ((atom xs) 1)
        ((null xs) 1)
        (t (+ (count-atoms (car xs))
              (count-atoms (cdr xs))))))

(count-atoms '(a (b) c)) ; 5

;; Exercise 8.40

(defun count-cons (xs)
  "Return the number of cons cells in list/tree XS."
  (cond ((atom xs) 0)
        (t (+ 1
              (count-cons (car xs))
              (count-cons (cdr xs))))))

(count-cons '(foo bar)) ; 2
(count-cons '((foo))) ; 2

;; Exercise 8.41

(defun sum-tree (xs)
  "Return the sum of all numbers in tree XS."
  (cond ((numberp xs) xs)
        ((atom xs) 0)
        (t (+ (sum-tree (car xs))
              (sum-tree (cdr xs))))))

(sum-tree '((3 bears) (3 bowls) (1 girl))) ; 7
