(defparameter *example-input*
  "............
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
           (destructuring-bind (di . dj)
               (cond ((< ai bi) (cons (- bi ai) (- bj aj)))
                     ((> ai bi) (cons (- ai bi) (- aj bj)))
                     ((= ai bi) (cons 0         (abs (- aj bj)))))
             (let ((start-i (if (= di 0)
                                ai
                              (mod ai di)))
                   (start-j (if (= di 0)
                                (mod aj (abs dj))
                              (- aj (* dj (truncate ai di))))
                              ))
               (loop for i = start-i then (+ i di)
                     for j = start-j then (+ j dj)
                     while (if (> di 0) (< i height) (< j width)) ;; sadness
                     when (and (< -1 i height) (< -1 j width))
                       do (push (cons i j) antinodes)))))))
     antennas)
    (remove-duplicates antinodes :test 'equal)))


(defun find-all-antinodes (map)
  (let* ((height (array-dimension map 0))
         (width (array-dimension map 1))
         (groups (find-antennas map))
         (antinodes
           (loop for group being the hash-values of groups
                 append (find-antinodes-for-group group height width))))
    (remove-duplicates antinodes :test 'equal)))


(defun advent-8b (f)
  (length (find-all-antinodes (read-map f))))

(with-input-from-string (f *example-input*) (advent-8a f))
(with-open-file (f "08_input.txt") (advent-8a f))
