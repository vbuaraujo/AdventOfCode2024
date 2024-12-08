(defparameter *example-input*
  "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")



(defun read-map (f)
  (let ((lines (loop for line = (read-line f nil nil)
                     while line
                     collecting line)))
    (make-array (list (length lines) (length (first lines)))
                :initial-contents lines
                :element-type 'base-char)))

(defun find-antennas (map)
  (let ((groups (make-hash-table :test 'equal)))
    (loop for i from 0 below (array-dimension map 0) do
      (loop for j from 0 below (array-dimension map 1) do
        (let ((thing (aref map i j)))
          (when (char/= thing #\.)
            (let ((antennas (gethash thing groups '())))
              (setf (gethash thing groups) (cons (cons i j) antennas)))))))
    groups))


(defun hash-table-to-alist (table)
  (loop for key being the hash-keys of table
          using (hash-value value)
        collect (cons key value)))

(defun each-pair (f antennas)
  (loop for p on antennas do
    (loop for q on (rest p) do
      (let ((a (first p))
            (b (first q)))
        (funcall f a b)))))

(defun find-antinodes-for-group (antennas height width)
  (let ((antinodes '()))
    (each-pair
     (lambda (a b)
       (destructuring-bind (ai . aj) a
         (destructuring-bind (bi . bj) b
           (let* ((di (- bi ai)) (dj (- bj aj))
                  (xi (- ai di)) (xj (- aj dj))  ;; 1st antinode's coordinates
                  (yi (+ bi di)) (yj (+ bj dj))) ;; 2nd antinode's coordinates
             (when (and (< -1 xi height) (< -1 xj width))
               (push (cons xi xj) antinodes))
             (when (and (< -1 yi height) (< -1 yj width))
               (push (cons yi yj) antinodes))))))
     antennas)
    antinodes))


(defun find-all-antinodes (map)
  (let* ((height (array-dimension map 0))
         (width (array-dimension map 1))
         (groups (find-antennas map))
         (antinodes
           (loop for group being the hash-values of groups
                 append (find-antinodes-for-group group height width))))
    (remove-duplicates antinodes :test 'equal)))


(defun advent-8a (f)
  (length (find-all-antinodes (read-map f))))

(with-input-from-string (f *example-input*) (advent-8a f))
(with-open-file (f "08_input.txt") (advent-8a f))
