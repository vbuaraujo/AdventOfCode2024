;; (defun read-lines-from-path (path)
;;   (with-open-file (f path)
;;     (read-lines-from-stream f)))

;; (defun read-lines-from-stream (f)
;;     (loop for line = (read-line f nil nil)
;;           while line
;;           collect line))

;; (defun read-lines-from-string (text)
;;   (with-input-from-string (f text)
;;     (read-lines-from-stream f)))


;; (defun split-string (text sep)
;;   (let ((length (length text)))
;;     (loop for start = 0 then (+ end 1)
;;           while (<= start length)
;;           for end = (or (position sep text :start start) length)
;;           collect (subseq text start end))))

;; Day 1.

(defun read-items-from-stream (f)
  (let ((eof (gensym)))
    (loop for item = (read f nil eof)
          while (not (eq item eof))
          collect item)))


(defun collect-lists (items)
  (loop for (x y . rest) = items then rest
        collect x into xs
        collect y into ys
        while rest
        finally (return (values xs ys))))

(defun advent-1a (f)
  (multiple-value-bind (xs ys) (collect-lists (read-items-from-stream f))
    (let ((distances (mapcar (lambda (x y) (abs (- x y))) (sort xs #'<) (sort ys #'<))))
      (reduce #'+ distances))))


(defun count-items (items)
  (let ((counts (make-hash-table)))
    (loop for item in items do
      (incf (gethash item counts 0)))
    counts))

(defun advent-1b (f)
  (multiple-value-bind (xs ys) (collect-lists (read-items-from-stream f))
    (let ((counts (count-items ys)))
      (loop for x in xs
            sum (* x (gethash x counts 0))))))


(with-open-file (f "01_input.txt") (advent-1a f))
(with-open-file (f "01_input.txt") (advent-1b f))


;; Day 2.

(defun line-items (line)
  (let ((eof (gensym)))
    (with-input-from-string (f line)
      (loop for item = (read f nil eof)
            while (not (eq item eof))
            collect item))))

(defun report-safe-p (items)
  (and (or (apply #'< items)
           (apply #'> items))
       (every (lambda (x y)
                (<= 1 (abs (- x y)) 3))
              items
              (rest items))))

(defun advent-2a (f)
  (loop for line = (read-line f nil nil)
        while line
        count (report-safe-p (line-items line))))

;; brute force?

(defun report-kinda-safe-p (items)
  (let ((len (length items)))
    (or (report-safe-p items)
        (some #'report-safe-p
              (loop for i from 0 below len
                    collecting (append (subseq items 0 i) (subseq items (1+ i) len)))))))

(defun advent-2b (f)
  (loop for line = (read-line f nil nil)
        while line
        count (report-kinda-safe-p (line-items line))))


(with-open-file (f "02_input.txt") (advent-2a f))
(with-open-file (f "02_input.txt") (advent-2b f))


;; Day 3.

(require 'cl-ppcre)

(defun multiply-corrupted (text)
  (let ((total 0))
    (ppcre:do-register-groups (x y) ("mul\\(([0-9]+),([0-9]+)\\)" text)
      (let ((product (* (parse-integer x)
                        (parse-integer y))))
        (incf total product)))
    total))

(defun advent-3a (f)
  (loop for line = (read-line f nil nil)
        while line
        sum (multiply-corrupted line)))

(with-open-file (f "03_input.txt") (advent-3a f))

(defun multiply-corrupted-conditional (text)
  (let ((total 0)
        (enabled t))
    (ppcre:do-register-groups (mul x y enable disable) ("(mul)\\(([0-9]+),([0-9]+)\\)|(do)\\(\\)|(don't)\\(\\)" text)
      (cond (enable (setf enabled t))
            (disable (setf enabled nil))
            (mul (when enabled
                   (let ((product (* (parse-integer x)
                                     (parse-integer y))))
                     (incf total product))))))
    total))

(defun advent-3b (f)
  (multiply-corrupted-conditional (uiop:slurp-stream-string f)))

(with-open-file (f "03_input.txt") (advent-3b f))

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
                   (or (try-search array "MAS" i (+ j 2) -1 -1)
                       (try-search array "SAM" i (+ j 2) -1 -1)))
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
