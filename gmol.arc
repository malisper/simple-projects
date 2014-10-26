;;;; a program for playing the game of life

(deftem cell row -1 col -1)

(= num-rows 20)
(= num-cols 20)

(def make-board ()
  "Makes a new board."
  (table))

(def valid (c)
  "A predicate for checking if a cell is in a valid location."
  (and (<= 0 c!row (- num-rows 1))
       (<= 0 c!col (- num-cols 1))))

(def get-neighbors (cell)
  "Returns a list of the cell C's neighbors."
  (keep valid
	(map (fn ((r c))
	       (inst 'cell 'row (+ cell!row r) 'col (+ cell!col c)))
	     '((1 0) (-1 0) (0 1) (0 -1) ; The Moore neighborhood.
	       (1 1) (1 -1) (-1 1) (-1 -1)))))

(def next-gen (board)
  "Returns the next generation of BOARD."
  (with (result (table) numbers (table))
    (each c (keys board)
      (each neighbor (get-neighbors c)
	(if numbers.neighbor
	    (++ numbers.neighbor)
	    (=  numbers.neighbor 1))))
    (each c (keys numbers)
      (when (or (is numbers.c 3)
		(and (is numbers.c 2)
		     board.c))
	(set result.c)))
    result))

(def print-board (board)
  "Prints the state of board."
  (for r 0 (- num-rows 1)
    (for c 0 (- num-cols 1)
      (let cell (inst 'cell 'row r 'col c)
	(if board.cell (pr "*") (pr "+"))))
    (prn)))
