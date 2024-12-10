(defparameter *example-input*
  "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defun parse-input (f)
  (let ((lines (loop for line = (read-line f nil nil)
                     while line
                     collect (loop for char across line
                                   collect (or (digit-char-p char) -1)))))
    (make-array (list (length lines) (length (first lines)))
                :initial-contents lines)))


(defun height (map)
  (array-dimension map 0))

(defun width (map)
  (array-dimension map 1))

(defun find-in-map (item map)
  (let ((coords '()))
    (loop for i from 0 below (height map) do
      (loop for j from 0 below (width map) do
        (when (eql (aref map i j) item)
          (push (cons i j) coords))))
    coords))

(defun find-tops (map start)
  (let* ((height (height map))
         (width (width map))
         (visited (make-array (list height width)
                             :element-type 'bit
                             :initial-element 0))
         (count 0))
    (labels ((try (level i j)
               ;;(format t "Looking at ~a, ~a, level ~a~%" i j level)
               (cond ((and (< -1 i height)
                           (< -1 j width)
                           (not (= 1 (aref visited i j)))
                           (= 1 (- (aref map i j) level)))
                      (setf (aref visited i j) 1)
                      (cond ((eql (aref map i j) 9)
                             ;;(format t "Found it! ~a ~a~%" i j)
                             (incf count))
                            (t
                             (try (aref map i j) (- i 1) j)
                             (try (aref map i j) (+ i 1) j)
                             (try (aref map i j) i (- j 1))
                             (try (aref map i j) i (+ j 1))))
                      (setf (aref visited i j) 0)
                      )
                     (t nil))))
      (try -1 (car start) (cdr start)))
    count))

(defun advent-10b (f)
  (let* ((map (parse-input f))
         (starts (find-in-map 0 map)))
    (loop for start in starts
          for paths = (find-tops map start)
          sum paths)))



(with-input-from-string (f *example-input*) (advent-10b f))
(with-open-file (f "10_input.txt") (advent-10b f))
