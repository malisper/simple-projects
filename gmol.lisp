(in-package :experimental)

(defstruct (cell (:conc-name nil) (:constructor make-cell (row col)))
  row col)

(defparameter *num-rows* 20 "The number of rows in the grid.")
(defparameter *num-cols* 20 "The number of cols in the grid.")
(defparameter *neighborhood* '((-1 -1) (-1 0) (-1 1) (0 -1)
                               (0 1) (1 -1) (1 0) (1 1))
  "The delta-row and delta-col of each cell in a neighborhood.")

(def make-board (&optional cells)
  "Returns a new gmol board."
  (memtable cells :test #'equalp))

(def valid (c)
  "Is this cell valid?"
  (and (<= 0 c!row (dec *num-rows*))
       (<= 0 c!col (dec *num-cols*))))

(def neighbors (c)
  "Returns a list of all of the neighbors of the given cell."
  (keep #'valid
        (loop for (dr dc) in *neighborhood*
              collect (make-cell (+ c!row dr) (+ c!col dc)))))

(def next-gen (board)
  "Given a board, returns a board containing the next generation."
  (ado (keys board)
       (mappend #'neighbors it)
       (counts it)
       (keep [or (is it._ 3)
                 (and (is it._ 2)
                      board._)]
             (keys it))
       (make-board it)))

(def print-board (board)
  "Print the given board."
  (up r 0 *num-rows*
    (up c 0 *num-cols*
      (prf "~:[ ~;*~]" (gethash (make-cell r c) board)))
    (prn)))
