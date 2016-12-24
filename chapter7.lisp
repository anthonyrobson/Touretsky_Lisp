;;; Touretsky Common Lisp
;;; Chapter 7 - Applicative programming

;;; `funcall` calls a function on some inputs
;;; (funcall #'cons 'a 'b) => (a . b)
;;; #' ('sharp quote') is way to quote a function in CL
;;; (type-of x) show the type of object x
;;; cannot quote macros/special functions with #'

;;; `mapcar` applies a function to each element of a given list
;;; (defun square (n) (* n n))
;;; (mapcar #'square '(1 2 3 4 5)) => (1 4 9 16 25)

;; Exercise 7.1
(defun add1 (n)
  "Add 1 to input N."
  (+ n 1))

(mapcar #'add1 '(1 3 5 7 9)) ; (2 4 6 8 10)

;; Exercise 7.2
(setf daily-planet '((olsen jimmy 123-76-4535 cub-reporter)
                     (kent  clark 089-52-6787 reporter)
                     (lane  lois  951-26-1438 reporter)
                     (white perry 355-16-7439 editor)))

(mapcar #'third daily-planet) ; (|123-76-4535| |089-52-6787| |951-26-1438| |355-16-7439|)

;; Exercise 7.3
(mapcar #'zerop '(2 0 3 4 0 -5 -6)) ; (NIL T NIL NIL T NIL NIL)

;; Exercise 7.4
(defun greater-than-five-p (n)
  "Return T if N is greater than 5."
  (> n 5))

(mapcar #'greater-than-five-p '(1 6 7 5 4 3 9)) ; (NIL T T NIL NIL NIL T)

;;; Lambda expressions such as (lambda (n) (* n n))

;; Exercise 7.5
((lambda (n) (- n 7)) 8) ; 1

;; Exercise 7.6
((lambda (p) (if (or (equal p t)
                     (equal p nil))
                 t
                 nil))
 t) ; T

;; Exercise 7.7
(defun up-down (xs)
  "Change each UP in list XS to DOWN and each DOWN to UP."
  (mapcar #'(lambda (e) (if (equal e 'up) 'down 'up)) xs))

(up-down '(up down up up)) ; (DOWN UP DOWN DOWN)

;;; FIND-IF returns first element in list which fulfils predicate

;; Exercise 7.8
(defun roughly-equal (xs k)
  "Return first number in list XS which is ±10 to K."
  (find-if #'(lambda (e) (and (> e (- k 10))
                              (< e (+ k 10))))
           xs))

(roughly-equal '(100 200 300) 101) ; 100

;; Exercise 7.9
(defun find-nested (xs)
  "Return first element of XS which is itself a non-nil list."
  (find-if #'(lambda (e) (consp e)) xs))

(find-nested '(1 2 3 () (4 5) 6 (7 8))) ; (4 5)

;;; Mini keyboard exercise

(setf note-table '((c 1) (c-sharp 2) (d 3) (d-sharp 4) (e 5) (f 6) (f-sharp 7)
                   (g 8) (g-sharp 9) (a 10) (a-sharp 11) (b 12)))

(defun numbers (notes-list)
  "Return the list of numbers corresponding to the notes in NOTES-LIST."
  (mapcar #'(lambda (e) (second (assoc e note-table))) notes-list))

(numbers '(e d c d e e e)) ; (5 3 1 3 5 5 5)

(defun notes (numbers-list)
  "Return the list of notes corresponding to the numbers in NUMBERS-LIST."
  (let ((reversed-note-table (mapcar #'reverse note-table)))
    (mapcar #'(lambda (e) (second (assoc e reversed-note-table))) numbers-list)))

(notes '(5 3 1 3 5 5 5)) ; (E D C D E E E)

(defun raise (n numbers-list)
  "Raise each number in NUMBERS-LIST by N."
  (mapcar #'(lambda (e) (+ e n)) numbers-list))

(raise 5 '(5 3 1 3 5 5 5)) ; (10 8 6 8 10 10 10)

(defun normalize (numbers-list)
  "To each number, if number is less than 1, add 12; if more than 12, subtract 12."
  (mapcar #'(lambda (e) (cond ((< e 1) (+ e 12))
                              ((> e 12) (- e 12))
                              (t e)))
          numbers-list))

(normalize '(6 10 13)) ; (6 10 1)

(defun transpose (n notes-list)
  "Convert song (NOTES-LIST) to numbers, raise, normalize, and return as a song."
  (notes (normalize (raise n (numbers notes-list)))))

(transpose 5 '(e d c d e e e)) ; (A G F G A A A)

;;; REMOVE-IF removes all items in list matching predicate
;;; REMOVE-IF-NOT removes all items in list not matching predicate

;; Exercise 7.11
(defun greater-than-1-less-than-5 (xs)
  "Return only those numbers in XS which are > 1 and < 5."
  (remove-if #'(lambda (e) (or (< e 1)
                               (> e 4)))
             xs))

(greater-than-1-less-than-5 '(-1 1 3 7)) ; (1 3)

;; Exercise 7.12
(defun count-the (xs)
  "Return how often THE occurs in list XS."
  (length (remove-if-not #'(lambda (e) (equal e 'the)) xs)))

(count-the '(the the the house)) ; 3

;; Exercise 7.13
(defun pick-length-two (xs)
  "Return those lists in XS which have a length of 2."
  (remove-if-not #'(lambda (e) (and (consp e)
                                    (equal (length e) 2)))
                 xs))

(pick-length-two '(1 (2 3) (4 5 6) (7 8))) ; ((2 3) (7 8))

;; Exercise 7.14
(defun my/intersection (xs ys)
  "Define INTERSECTION with REMOVE-IF or REMOVE-IF-NOT."
  (remove-if-not #'(lambda (e) (member e ys)) xs))

(my/intersection '(1 2 3) '(3 4 5)) ; (3)

(defun my/union (xs ys)
  "Define UNION with REMOVE-IF or REMOVE-IF-NOT."
  (append (remove-if #'(lambda (e) (member e ys)) xs)
          ys))

(my/union '(1 2 3) '(3 4 5)) ; (1 2 3 4 5)

;;; Mini keyboard exercise

(defun rank (card)
  "Return the rank, the 1st element, from CARD."
  (first card))

(rank '(ace spades)) ; ACE

(defun suit (card)
  "Return the suit, the 2nd element, from CARD."
  (second card))

(suit '(ace spades)) ; SPADES

(setf my-hand '((3 hearts) (5 clubs) (2 diamonds) (4 diamonds) (ace spades)))

(defun count-suit (s hand)
  "Return the number of cards in HAND which have a suit equal to S."
  (length (remove-if-not #'(lambda (card) (equal (suit card) s)) hand)))

(count-suit 'diamonds my-hand) ; 2

(setf colors '((clubs black) (diamonds red) (hearts red) (spades black)))

(defun color-of (card)
  "Return the colour of CARD using global COLORS."
  (second (assoc (suit card) colors)))

(color-of '(2 clubs)) ; BLACK
(color-of '(6 hearts)) ; RED

(defun first-red (hand)
  "Return the first red card in HAND."
  (find-if #'(lambda (card) (equal (color-of card) 'red)) hand))

(first-red my-hand) ; (3 HEARTS)

(defun black-cards (hand)
  "Return all the black cards in HAND."
  (remove-if-not #'(lambda (card) (equal (color-of card) 'black)) hand))

(black-cards my-hand) ; ((5 CLUBS) (ACE SPADES))

(defun what-ranks (s hand)
  "Return the ranks of all cards with suit S in HAND."
  (mapcar #'rank (remove-if-not #'(lambda (card) (equal (suit card) s)) hand)))

(what-ranks 'diamonds my-hand) ; (2 4)
(what-ranks 'spades my-hand) ; (ACE)

(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card1 card2)
  "Return T if CARD1 has a higher rank than CARD2."
  (member (rank card1) (member (rank card2) all-ranks)))

(higher-rank-p '(ace spades) '(2 clubs)) ; (ACE)
(higher-rank-p '(4 clubs) '(10 hearts)) ; NIL

(defun high-card (hand)
  "Return the highest-ranked card in HAND."
  (reduce #'(lambda (card1 card2) (if (higher-rank-p card1 card2)
                                      card1
                                      card2))
          hand))

(high-card my-hand) ; (ACE SPADES)

;;; REDUCE

;; Exercise 7.16
;; union

;; Exercise 7.17
(defun length-of-lists (xs)
  "Return the sum of the lengths of all lists in XS."
  (reduce #'+ (mapcar #'(lambda (e) (if (consp e)
                                        (length e)
                                        0))
                      xs)))

(length-of-lists '(1 (2 3) (4 5 6))) ; 5

;;; EVERY returns T if every element in a list matches predicate

;; Exercise 7.19
(defun all-odd (xs)
  "Return T if every element of XS is odd."
  (every #'oddp xs))

(all-odd '(1 3 5)) ; T
(all-odd '(1 4 5)) ; NIL

;; Exercise 7.20
(defun none-odd (xs)
  "Return T if no element of XS is odd."
  (every #'(lambda (e) (not (oddp e))) xs))

(none-odd '(2 4 6)) ; T
(none-odd '(2 5 6)) ; NIL

;; Exercise 7.21
(defun not-all-odd (xs)
  "Return T if not every element of XS is odd."
  (not (every #'oddp xs)))

(not-all-odd '(1 3 5)) ; NIL
(not-all-odd '(2 4 6)) ; T

;;; MINI KEYBOARD EXERCISE

(setf database '((b1 shape brick)
                 (b1 color green)
                 (b1 size small)
                 (b1 supported-by b2)
                 (b1 supported-by b3)
                 (b2 shape brick)
                 (b2 color red)
                 (b2 size small)
                 (b2 supports b1)
                 (b2 left-of b3)
                 (b3 shape brick)
                 (b3 color red)
                 (b3 size small)
                 (b3 supports b1)
                 (b3 right-of b2)
                 (b4 shape pyramid)
                 (b4 color blue)
                 (b4 size large)
                 (b4 supported-by b5)
                 (b5 shape cube)
                 (b5 color green)
                 (b5 size large)
                 (b5 supports b4)
                 (b6 shape brick)
                 (b6 color purple)
                 (b6 size large)))

(defun match-element (s1 s2)
  "Return T if S1 and S2 are equal, or if S2 is '?."
  (or (equal s1 s2)
      (equal s2 '?)))

(match-element 'red 'red) ; T
(match-element 'red '?) ; T
(match-element 'red 'blue) ; NIL

(defun match-triple (assertion pattern)
  "Return T if ASSERTION matches PATTERN."
  (reduce #'(lambda (x y) (and x y)) (mapcar #'match-element assertion pattern)))

(match-triple '(b2 color red) '(b2 color ?)) ; T
(match-triple '(b2 color red) '(b1 color green)) ; NIL

(defun fetch (pattern)
  "Return all assertions from DATABASE matching PATTERN."
  (remove-if-not #'(lambda (e) (match-triple e pattern)) database))

(fetch '(b2 color ?)) ; ((B2 COLOR RED))
(fetch '(? supports b1)) ; ((B2 SUPPORTS B1) (B3 SUPPORTS B1))

(fetch '(b4 shape ?)) ; ((B4 SHAPE PYRAMID))
(fetch '(? shape brick)) ; ((B1 SHAPE BRICK) (B2 SHAPE BRICK) (B3 SHAPE BRICK) (B6 SHAPE BRICK))
(fetch '(b2 ? b3)) ; ((B2 LEFT-OF B3))
(fetch '(? color ?))
; ((B1 COLOR GREEN) (B2 COLOR RED) (B3 COLOR RED) (B4 COLOR BLUE)
; (B5 COLOR GREEN) (B6 COLOR PURPLE))
(fetch '(b4 ? ?)) ; ((B4 SHAPE PYRAMID) (B4 COLOR BLUE) (B4 SIZE LARGE) (B4 SUPPORTED-BY B5))

(defun get-color-pattern (block-name)
  "Return a form (BLOCK-NAME COLOR ?)."
  (list block-name 'color '?))

(get-color-pattern 'b3) ; (B3 COLOR ?)

(defun supporters (block-name)
  "Return the blocks which support BLOCK-NAME."
  (let ((pattern (list block-name 'supported-by '?)))
    (mapcar #'third (fetch pattern))))

(supporters 'b1) ; (B2 B3)

(defun supported-by-cube-p (block-name)
  "Return T if BLOCK-NAME is supported by a cube."
  (reduce #'(lambda (x y) (and x y))
          (mapcar #'(lambda (e) (fetch (list e 'shape 'cube)))
                  (supporters block-name))))

(supported-by-cube-p 'b4) ; ((B5 SHAPE CUBE))
(supported-by-cube-p 'b1) ; NIL

(defun desc1 (block-name)
  "Return all assertions concerning BLOCK-NAME."
  (fetch (list block-name '? '?)))

(desc1 'b6) ; ((B6 SHAPE BRICK) (B6 COLOR PURPLE) (B6 SIZE LARGE))

(defun desc2 (block-name)
  "Strip BLOCK-NAME from each sublist from DESC1."
  (mapcar #'cdr (desc1 block-name)))

(desc2 'b6) ; ((SHAPE BRICK) (COLOR PURPLE) (SIZE LARGE))

(defun description (block-name)
  "Concatenate the output from DESC2 into one list."
  (reduce #'append (desc2 block-name)))

(description 'b6) ; (SHAPE BRICK COLOR PURPLE SIZE LARGE)
