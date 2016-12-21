;;; Touretsky Common Lisp
;;; Chapter 2

(defun my-third1 (xs)
  "Return the third element of list XS; using FIRST and REST."
  (first (rest (rest xs))))

(equal 'c (my-third1 '(a b c d)))

(defun my-third2 (xs)
  "Return the third element of list XS; using SECOND."
  (second (rest xs)))

(equal 'c (my-third2 '(a b c d)))

(defun two-inputs (x y)
  "Return the list (X Y)."
  (list x y))

(equal '(a b) (two-inputs 'a 'b))

(defun four-inputs (a b c d)
  "Return the list ((A B) (C D))."
  (list (list a b) (list c d)))

(equal '((1 2) (3 4)) (four-inputs 1 2 3 4))

(defun duo-cons (a b xs)
  "Add A and B to the start of list XS."
  (cons a (cons b xs)))

(equal '(1 2 3 4 5) (duo-cons 1 2 '(3 4 5)))

(defun two-deeper1 (x)
  "Return list ((X)) using LIST."
  (list (list x)))

(equal '((moo)) (two-deeper1 'moo))

(defun two-deeper2 (x)
  "Return list ((X)) using CONS."
  (cons (cons x nil) nil))

(equal '((moo)) (two-deeper2 'moo))

(defun unary-add1 (xs)
  "Cons an 'X' to the start of list XS."
  (cons 'x xs))

(equal '(x x) (unary-add1 '(x)))

(defun unary-zerop (xs)
  "Return T if list XS is empty."
  (null xs))

(equal t (unary-zerop '()))
(equal nil (unary-zerop '(x x x)))

(defun unary-greaterp (xs ys)
  "Is list XS longer than YS?"
  (> (length xs) (length ys)))

(equal t (unary-greaterp '(x x x) '(x x)))
(equal nil (unary-greaterp '(x) '(x x)))


