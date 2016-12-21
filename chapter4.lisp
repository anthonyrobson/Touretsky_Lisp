;;; Touretsky Common Lisp
;;; Chapter 4

(defun make-even (n)
  "Make an odd number even by adding 1; else return N."
  (if (oddp n)
      (+ n 1)
      n))

(equal 4 (make-even 3))
(equal 4 (make-even 4))

(defun further (n)
  "Return positive N+1, negative N-1, else 0."
  (if (zerop n)
      0
      (if (> n 0)
          (+ n 1)
          (- n 1))))

(equal 0 (further 0))
(equal 2 (further 1))
(equal -2 (further -1))

(defun my-not (p)
  "Own definition of NOT, using only IF and constants."
  (if p
      nil
      t))

(equal t (my-not nil))
(equal nil (my-not t))

(defun ordered (x y)
  "Return X and Y in an ordered list."
  (if (< x y)
      (list x y)
      (list y x)))

(equal '(3 4) (ordered 3 4))
(equal '(3 4) (ordered 4 3))

(defun my-abs (n)
  "Return absolute value of N."
  (cond ((zerop n) n)
        ((> n 0) n)
        (t (- n))))

(equal 4 (my-abs -4))
(equal 4 (my-abs 4))
(equal 0 (my-abs 0))

(defun constrain (x min max)
  "If X > MAX, return MAX; else if X < MIN, return MIN; else return X."
  (cond ((> x max) max)
        ((< x min) min)
        (t x)))

(equal -50 (constrain -92 -50 50))
(equal 50 (constrain 92 -50 50))
(equal 5 (constrain 5 -50 50))

(defun first-zero (xs)
  "Return the position of the first-occurring zero in three-element list XS."
  (cond ((zerop (first xs)) 'first)
        ((zerop (second xs)) 'second)
        ((zerop (third xs)) 'third)
        (t 'none)))

(equal 'first (first-zero '(0 1 2)))
(equal 'second (first-zero '(1 0 2)))
(equal 'third (first-zero '(1 2 0)))
(equal 'none (first-zero '(1 2 3)))

(defun cycle (n)
  "If N is 99, return 1; else return N+1."
  (if (equal n 99)
      1
      (+ n 1)))

(equal 1 (cycle 99))
(equal 2 (cycle 1))

(defun how-compute (a b c)
  "Return how C may be computed from A and B; limited in scope."
  (cond ((equal c (* a b)) 'product-of)
        ((equal c (+ a b)) 'sum-of)
        (t 'beats-me)))

(equal 'product-of (how-compute 3 4 12))
(equal 'sum-of (how-compute 3 4 7))
(equal 'beats-me (how-compute 3 4 5))

(defun geq (x y)
  "Return T if X is greater than or equal to Y."
  (or (equal x y)
      (> x y)))

(equal t (geq 10 9))
(equal t (geq 9 9))
(equal nil (geq 9 10))

(defun square-double-divide (n)
  "If N is odd and positive, return its square; if odd and negative, doubled; else, divide by 2."
  (cond ((and (oddp n) (plusp n)) (* n n))
        ((and (oddp n) (minusp n)) (* n 2))
        (t (/ n 2))))

(equal 9 (square-double-divide 3))
(equal -6 (square-double-divide -3))
(equal 2 (square-double-divide 4))

(defun people (p1 p2)
  "Return T if P1 is BOY/GIRL and P2 is CHILD, or if P1 is MAN/WOMAN and P2 is ADULT."
  (or (and (or (equal p1 'boy)
               (equal p1 'girl))
           (equal p2 'child))
      (and (or (equal p1 'man)
               (equal p1 'woman))
           (equal p2 'adult))))

(equal t (people 'boy 'child))
(equal t (people 'woman 'adult))
(equal nil (people 'girl 'adult))

(defun rock-paper-scissors (p1 p2)
  "Play the rock, paper, scissors game with player 1 (P1) and player 2 (P2)."
  (cond ((equal p1 p2) 'tie)
        ((or (and (equal p1 'rock)
                  (equal p2 'scissors))
             (and (equal p1 'paper)
                  (equal p2 'rock))
             (and (equal p1 'scissors)
                  (equal p2 'paper)))
         'first-wins)
        (t 'second-wins)))

(equal 'tie (rock-paper-scissors 'rock 'rock))
(equal 'first-wins (rock-paper-scissors 'rock 'scissors))
(equal 'second-wins (rock-paper-scissors 'rock 'paper))

(defun boilingp (temp scale)
  "Return T if TEMP >= 100 and SCALE is CELSIUS, or if TEMP >= 212 and SCALE is FAHRENHEIT."
  (or (and (>= temp 100)
           (equal scale 'celsius))
      (and (>= temp 212)
           (equal scale 'fahrenheit))))

(equal t (boilingp 101 'celsius))
(equal t (boilingp 213 'fahrenheit))
(equal nil (boilingp 95 'celsius))



