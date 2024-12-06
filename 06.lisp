(defparameter *example-input*
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")



(defun read-map (f)
  (let ((lines (loop for line = (read-line f nil nil)
                     while line
                     collect line)))
    (make-array (list (length lines) (length (first lines)))
                :initial-contents lines
                :element-type 'character)))

(defun find-position (map)
  (loop for i from 0 below (array-dimension map 0) do
    (loop for j from 0 below (array-dimension map 1) do
      (when (char= (aref map i j) #\^)
        (return-from find-position (cons i j)))))
  (error "Could not find ^ in map"))


(defparameter *directions* #((-1 . 0) (0 . -1) (1 . 0) (0 . 1)))
(defparameter *direction-indicators* "^<v>")

(defun turn (direction) (mod (- direction 1) 4))

(defstruct state map position direction)

(defun make-initial-state (map)
  (make-state :map map :position (find-position map) :direction 0))

(defun one-move (state)
  (let* ((map (state-map state))
         (height (array-dimension map 0))
         (width (array-dimension map 1)))
    (destructuring-bind (i . j) (state-position state)
      (destructuring-bind (di . dj) (aref *directions* (state-direction state))
        (let* ((new-i (+ i di)) (new-j (+ j dj)))
          (cond ((and (< -1 new-i height) (< -1 new-j width))
                 ;; we are within the map. Can we move there?
                 (let ((thing (aref map new-i new-j)))
                   (case thing
                     ((#\. #\X) ;; Move there and leave a trail
                      (setf (aref map i j) #\X)
                      (setf (aref map new-i new-j)
                            (aref *direction-indicators* (state-direction state)))
                      (setf (state-position state) (cons new-i new-j)))
                     (otherwise ;; Can't move, let's turn and try again.
                      (setf (state-direction state) (turn (state-direction state)))
                      (setf (aref map i j)
                            (aref *direction-indicators* (state-direction state)))
                      (one-move state)))
                   state))
                (t ;; We are out!
                 (setf (aref map i j) #\X)
                 (setf (state-position state) nil)
                 nil)))))))


(defun count-trail (map)
  (let ((count 0))
    (loop for i from 0 below (array-dimension map 0) do
      (loop for j from 0 below (array-dimension map 1) do
        (when (char= (aref map i j) #\X)
          (incf count))))
    count))

(defun print-map (map)
  (loop for i from 0 below (array-dimension map 0) do
    (loop for j from 0 below (array-dimension map 1) do
      (princ (aref map i j)))
    (terpri)))

(defun advent-6a (f)
  (let* ((map (read-map f))
         (state (make-initial-state map)))
    (loop while (one-move state))
    (let ((count (count-trail (state-map state))))
      count)))

;;(print (with-input-from-string (f *example-input*) (advent-6a f)))
;;(terpri)
(with-open-file (f "06_input.txt") (advent-6a f))
