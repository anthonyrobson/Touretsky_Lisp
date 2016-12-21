;;; Touretsky Symbolic Lisp
;;; Chapter 1

(defun add1 (n)
  "Add 1 to input N."
  (+ n 1))

(equal 10 (add1 9))

(defun add2 (n)
  "Add 2 to input N."
  (+ n 2))

(equal 12 (add2 10))

(defun sub2 (n)
  "Subtract 2 from input N."
  (- n 2))

(equal 8 (sub2 10))

(defun twop (n)
  "Return T if input N equals 2; use ZEROP and SUB2."
  (zerop (sub2 n)))

(equal t (twop 2))
(equal nil (twop 3))

(defun half (n)
  "Halve input N."
  (/ n 2))

(equal 5 (half 10))

(defun multi-digit-p (n)
  "Return T if input N is greater than 9."
  (> n 9))

(equal t (multi-digit-p 10))
(equal nil (multi-digit-p 9))

(defun twomorep (x y)
  "Return T if X is two more than Y; use ADD2."
  (equal x (add2 y)))

(equal t (twomorep 10 8))
(equal nil (twomorep 9 8))

(defun average (x y)
  "Return the arithmetic mean of X and Y."
  (/ (+ x y) 2))

(equal 11 (average 10 12))

(defun not-onep (n)
  "Return T if input N is not 1."
  (not (equal 1 n)))

(equal t (not-onep 2))
(equal nil (not-onep 1))

(defun not-plusp (n)
  "Return T if input N is not greater than zero."
  (not (> n 0)))

(equal t (not-plusp -1))
(equal nil (not-plusp 1))

(defun my-evenp (n)
  "Return T if input N is an even number."
  (not (oddp n)))

(equal t (my-evenp 10))
(equal nil (my-evenp 9))


