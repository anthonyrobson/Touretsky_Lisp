;;; Touretsky Common Lisp
;;; Chapter 5

(defun throw-die ()
  "Return a random integer between 1 and 6."
  (+ 1 (random 6)))

(defun throw-dice ()
  "Return a list of two THROW-DIE."
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (th)
  "Return T if throw TH contains (1 1)."
  (and (equal (first th) 1)
       (equal (second th) 1)))

(equal t (snake-eyes-p '(1 1)))
(equal nil (snake-eyes-p '(1 2)))

(defun boxcars-p (th)
  "Return T if throw TH contains (6 6)."
  (and (equal (first th) 6)
       (equal (second th) 6)))

(equal t (boxcars-p '(6 6)))
(equal nil (boxcars-p '(6 5)))

(defun instant-win-p (th)
  "Return T if the sum of throw TH is 7 or 11."
  (let ((sum (+ (first th) (second th))))
    (or (equal sum 7)
        (equal sum 11))))

(equal t (instant-win-p '(6 1)))
(equal t (instant-win-p '(6 5)))
(equal nil (instant-win-p '(6 6)))

(defun instant-loss-p (th)
  "Return T if the sum of throw TH is 2, 3, or 12."
  (let ((sum (+ (first th) (second th))))
    (or (equal sum 2)
        (equal sum 3)
        (equal sum 12))))

(equal t (instant-loss-p '(1 1)))
(equal t (instant-loss-p '(2 1)))
(equal t (instant-loss-p '(6 6)))
(equal nil (instant-loss-p '(2 2)))

(defun say-throw (th)
  "Return SNAKE-EYES, BOXCARS, or the sum of throw TH as appropriate."
  (let ((sum (+ (first th) (second th))))
    (cond ((snake-eyes-p th) 'snake-eyes)
          ((boxcars-p th) 'boxcars)
          (t sum))))

(equal 7 (say-throw '(3 4)))
(equal 'boxcars (say-throw '(6 6)))

(defun craps ()
  "Output a string based on thrown dice."
  (let* ((th (throw-dice))
         (sum (+ (first th) (second th)))
         (str1 (list 'throw (first th) 'and (second th) '--)))
    (cond ((instant-win-p th) (append str1 (list (say-throw th) '-- 'you 'win)))
          ((instant-loss-p th) (append str1 (list (say-throw th) '-- 'you 'lose)))
          (t (append str1 (list 'your 'point 'is sum))))))

(defun try-for-point (n)
  "Win if N matches new throw's sum, lose if sum is 7, else new point is sum."
  (let* ((th (throw-dice))
         (sum (+ (first th) (second th)))
         (str1 (list 'throw (first th) 'and (second th) '--)))
    (cond ((equal sum 7) (append str1 (list sum '-- 'you 'lose)))
          ((equal sum n) (append str1 (list sum '-- 'you 'win)))
          (t (append str1 (list sum '-- 'throw 'again))))))
