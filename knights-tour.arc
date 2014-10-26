;;;; a program that calculates knights' tours

(deftem square row -1 col -1)

(def valid (sqr)
  "Checks whether SQR is a valid square."
  (and (<= 0 sqr!row 7)
       (<= 0 sqr!col 7)))

(defmemo pos-moves (sqr)
  "Get all of the valid squares a knight could move to from SQR."
  (keep valid
	(map (fn ((r c))
	       (inst 'square
		     'row (+ sqr!row r)
		     'col (+ sqr!col c)))
	     '((1 2) (2 1) (2 -1) (1 -2)
	       (-1 -2) (-2 -1) (-2 1) (-1 2)))))

(def score (sqr)
  "Calculate a score based on Warnsdorff's rule."
  (len:pos-moves sqr))

(def tour (sqr)
  "Calculate a knights tour starting from SQR."
  (afnwith (path (list sqr))
    (if (is (len path) 64)
        (rev path)
	(some [and (~mem _ path) (self:cons _ path)]
	      (sort (compare < score) (pos-moves:car path))))))

(def print-ans (path)
  "Given a path, print it nicely."
  (for i 0 7
    (for j 0 7
      (pr (pos (inst 'square 'row i 'col j) path) " "))
    (prn)))
