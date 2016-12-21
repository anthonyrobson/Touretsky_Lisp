;;; Touretsky Common Lisp
;;; Chapter 6

(defun last-element1 (xs)
  "Return the last element in list XS, using LAST."
  (car (last xs)))

(equal 'c (last-element1 '(a b c)))

(defun last-element2 (xs)
  "Return the last element in list XS, using REVERSE."
  (car (reverse xs)))

(equal 'c (last-element2 '(a b c)))

(defun last-element3 (xs)
  "Return the last element in list XS, using NTH and LENGTH."
  (nth (- (length xs) 1) xs))

(equal 'c (last-element3 '(a b c)))

(defun next-to-last1 (xs)
  "Return the penultimate element in list XS, using REVERSE."
  (second (reverse xs)))

(equal 'c (next-to-last1 '(a b c d)))

(defun next-to-last2 (xs)
  "Return the penultimate element in list XS, using NTH."
  (nth (- (length xs) 2) xs))

(equal 'c (next-to-last2 '(a b c d)))

(defun my-butlast (xs)
  "Return all but last element of list XS."
  (reverse (cdr (reverse xs))))

(equal '(a b c) (my-butlast '(a b c d)))

(defun palindrome-p (xs)
  "Return T if list XS is palindromic."
  (equal xs (reverse xs)))

(equal t (palindrome-p '(a b a)))
(equal nil (palindrome-p '(a b c)))

(defun make-palindrome (xs)
  "Make list XS palindromic by appending reverse of it onto itself."
  (append xs (reverse xs)))

(equal '(a b c c b a) (make-palindrome '(a b c)))

(defun contains-article-p1 (xs)
  "Return T if list XS contains A, AN, or THE; using INTERSECTION."
  (intersection '(a an the) xs))

(not (equal nil (contains-article-p1 '(the monkey))))
(equal nil (contains-article-p1 '(monkey business)))

(defun contains-article-p2 (xs)
  "Return T if list XS contains A, AN, or THE; using MEMBER and OR."
  (or (member 'a xs)
      (member 'an xs)
      (member 'the xs)))

(not (equal nil (contains-article-p2 '(the monkey))))
(equal nil (contains-article-p2 '(monkey business)))

(defun add-vowels (xs)
  "Add set of vowels (A E I O U) to given list XS."
  (union '(a e i o u) xs))

(defun my-subsetp (xs ys)
  "Return T if XS is a subset of YS."
  (null (set-difference xs ys)))

(equal t (my-subsetp '(a i) '(a e i o u)))
(equal nil (my-subsetp '(a x) '(a e i o u)))

(defun set-equal (xs ys)
  "Return T if XS is an equal set to YS."
  (and (my-subsetp xs ys)
       (my-subsetp ys xs)))

(equal t (set-equal '(red blue green) '(green blue red)))
(equal nil (set-equal '(yellow blue green) '(green blue red)))

(defun proper-subset-p (xs ys)
  "Return T if XS is a proper subset of YS."
  (and (my-subsetp xs ys)
       (not (set-equal xs ys))))

(equal t (proper-subset-p '(a c) '(c a b)))
(equal nil (proper-subset-p '(a b c) '(c a b)))

(setf test-shape '(large red shiny cube -vs- small shiny red four-sided pyramid))

(defun right-side (xs)
  "Return all features to the right of '-vs-' in list XS."
  (cdr (member '-vs- xs)))

(equal '(small shiny red four-sided pyramid) (right-side test-shape))

(defun left-side (xs)
  "Return all features to the left of '-vs-' in list XS."
  (reverse (right-side (reverse xs))))

(equal '(large red shiny cube) (left-side test-shape))

(defun count-common (xs)
  "Return number of features left and right sides have in common."
  (length (intersection (left-side xs) (right-side xs))))

(equal 2 (count-common test-shape))

(defun compare (xs)
  "Output a response using COUNT-COMMON."
  (list (count-common xs) 'common 'features))

(equal '(2 common features) (compare test-shape))
(equal '(3 common features) (compare '(small red metal cube -vs- red plastic small cube)))

(setf books '((war-and-peace leo-tolstoy)
              (1984 george-orwell)
              (brave-new-world aldous-huxley)
              (das-kapital karl-marx)
              (animal-farm george-orwell)))

(defun who-wrote (book)
  "Return the author of BOOK from table BOOKS."
  (second (assoc book books)))

(equal 'leo-tolstoy (who-wrote 'war-and-peace))

(setf nerd-states '((sleeping eating)
                    (eating waiting-for-a-computer)
                    (waiting-for-a-computer programming)
                    (programming debugging)
                    (debugging sleeping)))

(defun nerdus (state)
  "Return the next state from STATE in NERD-STATES."
  (cadr (assoc state nerd-states)))

(equal 'eating (nerdus 'sleeping))

(defun sleepless-nerd (state)
  "As NERDUS, but misses SLEEPING."
  (if (equal state 'debugging)
      'eating
      (nerdus state)))

(equal 'eating (sleepless-nerd 'debugging))
(equal 'programming (sleepless-nerd 'waiting-for-a-computer))

(defun nerd-on-caffeine (state)
  "As NERDUS, but advances two states instead of one."
  (nerdus (nerdus state)))

(equal 'debugging (nerd-on-caffeine 'waiting-for-a-computer))

(defun swap-first-last (xs)
  "Return list XS with first and last elements swapped."
  (let ((middle (cdr (reverse (cdr (reverse xs))))))
    (append (last xs) middle (list (first xs)))))

(equal '(love cant buy you) (swap-first-last '(you cant buy love)))

(defun rotate-left (xs)
  "Rotate list XS, such that (A B C D E) becomes (B C D E A)."
  (append (cdr xs) (list (first xs))))

(equal '(b c d e a) (rotate-left '(a b c d e)))

(defun rotate-right (xs)
  "Rotate list XS, such that (A B C D E) becomes (E A B C D)."
  (append (last xs) (reverse (cdr (reverse xs)))))

(equal '(e a b c d) (rotate-right '(a b c d e)))

(setf rooms '((living-room
               (north front-stairs)
               (south dining-room)
               (east kitchen))
              (upstairs-bedroom
               (west library)
               (south front-stairs))
              (dining-room
               (north living-room)
               (east pantry)
               (west downstairs-bedroom))
              (kitchen
               (west living-room)
               (south pantry))
              (pantry
               (north kitchen)
               (west dining-room))
              (downstairs-bedroom
               (north back-stairs)
               (east dining-room))
              (back-stairs
               (south downstairs-bedroom)
               (north library))
              (front-stairs
               (north upstairs-bedroom)
               (south living-room))
              (library
               (east upstairs-bedroom)
               (south back-stairs))))

(defun choices (room)
  "Return list of choices of direction/room in a given ROOM."
  (cdr (assoc room rooms)))

(equal '((north kitchen) (west dining-room)) (choices 'pantry))

(defun look (direction room)
  "Return the room which lies in DIRECTION from ROOM."
  (cadr (assoc direction (choices room))))

(equal 'kitchen (look 'north 'pantry))

;; Robbie's location
(setf loc 'pantry)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting variable LOC."
  (setf loc place))

(defun how-many-choices ()
  "Return number of available directions in current LOC."
  (length (choices loc)))

(equal 2 (how-many-choices))

(defun upstairs-p (room)
  "Return T if ROOM is upstairs in house."
  (or (equal room 'upstairs-bedroom)
      (equal room 'library)))

(equal t (upstairs-p 'library))
(equal nil (upstairs-p 'pantry))

(defun onstairs-p (room)
  "Return T if ROOM is on stairs in house."
  (or (equal room 'front-stairs)
      (equal room 'back-stairs)))

(equal t (onstairs-p 'front-stairs))
(equal nil (onstairs-p 'pantry))

(defun where ()
  "Output where Robbie is located."
  (cond ((onstairs-p loc) (list 'robbie 'is 'on 'the loc))
        ((upstairs-p loc) (list 'robbie 'is 'upstairs 'in 'the loc))
        (t (list 'robbie 'is 'downstairs 'in 'the loc))))

(equal '(robbie is downstairs in the pantry) (where))

(defun move (direction)
  "Moves Robbie in DIRECTION, if possible."
  (if (assoc direction (choices loc))
      (progn
        (set-robbie-location (look direction loc))
        (where))
      (list 'ouch! 'robbie 'hit 'a 'wall)))

(defun royal-we (xs)
  "Change every 'I' in XS to 'WE'."
  (subst 'we 'i xs))

(equal '(if we learn lisp we will be pleased) (royal-we '(if i learn lisp i will be pleased)))


