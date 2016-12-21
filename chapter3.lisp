;;; Touretsky Common Lisp
;;; Chapter 3

(defun half (n)
  "Halve input N."
  (/ n 2))

(equal 5 (half 10))

(defun cube (n)
  "Cube input N."
  (* n n n))

(equal 27 (cube 3))

(defun onemorep (x y)
  "Return T if X is one greater than Y."
  (equal x (+ y 1)))

(equal t (onemorep 10 9))
(equal nil (onemorep 11 9))

(defun pythag (x y)
  "Return square root of X^2 + Y^2."
  (sqrt (+ (* x x) (* y y))))

(equal 5 (pythag 3 4))

(defun longer-than (xs ys)
  "Return T if XS is a longer list than YS."
  (> (length xs) (length ys)))

(equal t (longer-than '(x x x) '(x x)))
(equal nil (longer-than '(x x) '(x x)))

(defun addlength (xs)
  "Cons the length of list XS to the start of XS."
  (cons (length xs) xs))

(equal '(4 moo goo gai pan) (addlength '(moo goo gai pan)))

(defun myfun (a b)
  "Return the list ((A) B)."
  (list (list a) b))

(equal '((1) 2) (myfun 1 2))

(defun firstp (z xs)
  "Return T if Z is the first element in list XS."
  (equal z (first xs)))

(equal t (firstp 'a '(a b c d)))
(equal nil (firstp 'b '(a b c d)))

(defun mid-add1 (xs)
  "Add 1 to the middle element of three-element list XS."
  (list (first xs) (+ 1 (second xs)) (third xs)))

(equal '(take 3 cookies) (mid-add1 '(take 2 cookies)))

(defun f-to-c (temp)
  "Convert TEMP from Fahrenheit to Celsius."
  (/ (* (- temp 32) 5) 9))

(equal 40 (f-to-c 104))


