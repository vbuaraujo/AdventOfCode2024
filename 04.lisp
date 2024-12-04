;; Day 4.

(defparameter *example-words*
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
")


(defun input-to-array (f)
  (let* ((lines (uiop:slurp-stream-lines f))
         (array (make-array (list (length lines) (length (first lines)))
                            :initial-contents lines)))
    array))

(defun try-search (array word start-i start-j di dj)
  (let ((height (array-dimension array 0))
        (width (array-dimension array 1)))
    (cond ((not (and (<= -1 (+ start-i (* (length word) di)) height)
                     (<= -1 (+ start-j (* (length word) dj)) width)))
           nil)
          ((loop for i = start-i then (+ i di)
                 for j = start-j then (+ j dj)
                 for c across word
                 always (char= (aref array i j) c))
           t)
          (t nil))))

(defun search-words (array words)
  (let ((counts (make-hash-table :test 'equal))
        (height (array-dimension array 0))
        (width (array-dimension array 1)))
    (loop for i from 0 below height do
      (loop for j from 0 below width do
        (loop for word in words
              when (equal (aref array i j) (aref word 0)) do
                (loop for (di dj) in '((0 1) (1 1) (1 0) (1 -1)
                                       (0 -1) (-1 -1) (-1 0) (-1 1))
                      when (try-search array word i j di dj)
                        do (incf (gethash word counts 0))))))
    (loop for word being the hash-keys of counts
            using (hash-value count)
          collect (cons word count))))

(defun x-mas (array)
  (let ((count 0)
        (height (array-dimension array 0))
        (width (array-dimension array 1)))
    (loop for i from 0 below (- height 2) do
      (loop for j from 0 below (- width 2) do
        (when (and (or (try-search array "MAS" i j 1 1)
                       (try-search array "SAM" i j 1 1))
                   (or (try-search array "MAS" i (+ j 2) 1 -1)
                       (try-search array "SAM" i (+ j 2) 1 -1)))
          (incf count))))
    count))

(defun advent-4a (f)
  (let ((array (input-to-array f)))
    (search-words array '("XMAS"))))

(defun advent-4b (f)
  (let ((array (input-to-array f)))
    (x-mas array)))

(with-open-file (f "04_input.txt") (advent-4a f))
(with-open-file (f "04_input.txt") (advent-4b f))
