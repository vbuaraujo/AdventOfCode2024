(require 'sb-sprof)

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
                :element-type 'base-char)))

(deftype 2d-map () '(simple-array base-char (* *)))

(defun copy-map (map)
  (declare (type 2d-map map))
  (let* ((new-map (make-array (list (array-dimension map 0)
                                    (array-dimension map 1))
                              :element-type 'base-char)))
    (dotimes (i (array-total-size map))
      (setf (row-major-aref new-map i)
            (row-major-aref map i)))
    new-map))


(defun find-position (map)
  (loop for i from 0 below (array-dimension map 0) do
    (loop for j from 0 below (array-dimension map 1) do
      (when (char= (aref map i j) #\^)
        (return-from find-position (cons i j)))))
  (error "Could not find ^ in map"))


(defparameter *directions* #((-1 . 0) (0 . -1) (1 . 0) (0 . 1)))
(defparameter *direction-indicators* "^<v>")

(declaim (inline turn))
(defun turn (direction) (mod (- direction 1) 4))

(defstruct state
  (map nil :type 2d-map)
  (position nil :type (cons fixnum fixnum))
  (direction nil :type fixnum)
  (trails nil :type (vector (simple-array bit (* *)))))

(defun make-empty-trails (map)
  (let* ((dimensions (array-dimensions map)))
    (vector
     (make-array dimensions :element-type 'bit :initial-element 0)
     (make-array dimensions :element-type 'bit :initial-element 0)
     (make-array dimensions :element-type 'bit :initial-element 0)
     (make-array dimensions :element-type 'bit :initial-element 0))))


(defun make-initial-state (map)
  (let* ((position (find-position map))
         (trails (make-empty-trails map))
         (state
           (make-state :map map
                       :position position
                       :direction 0
                       :trails trails)))
    state))



(declaim (ftype (function (state) state) deep-copy-state))
(defun deep-copy-state (state)
  (with-slots (map position direction trails) state
    (make-state :map (copy-map map)
                :position position
                :direction direction
                :trails (make-empty-trails map) ;; trails don't need to be copied
                )))

(declaim (ftype (function (state cons) state) make-state-with-boulder))
(defun make-state-with-boulder (state boulder)
  (let ((state (deep-copy-state state)))
    (setf (aref (state-map state) (car boulder) (cdr boulder)) #\#)
    state))

(declaim (inline move))
(defun move (point direction)
  (destructuring-bind (i . j) point
    (destructuring-bind (di . dj) (aref *directions* direction)
      (cons (+ i di) (+ j dj)))))

(declaim (inline map-ref))
(defun map-ref (map point)
  (destructuring-bind (i . j) point
    (cond ((and (< -1 i (array-dimension map 0))
                (< -1 j (array-dimension map 1)))
           (case (aref map i j)
             (#\# :boulder)
             (t   :free)))
          (t :out))))

(defun one-move (state)
  (destructuring-bind (i . j) (state-position state)
    (with-slots (map position trails direction) state
      (when (= 1 (aref (aref trails direction) i j))
        (return-from one-move :loop))
      (setf (aref (aref trails direction) i j) 1)

      (let* ((new-position (move position direction))
             (what-i-see (map-ref map new-position)))
        (ecase what-i-see
          (:boulder
           (setf direction (turn direction))
           :continue)
          (:out
           :out)
          (:free
           (setf position new-position)
           :continue))))))

(defun all-moves (state)
  (loop for result = (one-move state)
        while (eq result :continue)
        finally (return result)))


(defun count-guard-blockers (final-state initial-state)
  (let ((blockers (make-hash-table :test 'equal)))
    (with-slots (map trails) final-state
      (loop for i from 0 below (array-dimension map 0) do
        (loop for j from 0 below (array-dimension map 1) do
          (loop for direction from 0 to 3 do
            (let ((been-here (= 1 (aref (aref trails direction) i j)))
                  (boulder (move (cons i j) direction)))
              (when been-here
                (when (and
                       (eq (map-ref map boulder) :free)
                       (eq (all-moves (make-state-with-boulder initial-state boulder)) :loop))
                  (setf (gethash boulder blockers) t))))))))
    (loop for boulder being the hash-keys of blockers sum 1)))

(defun advent-6b (f)
  (let* ((map (read-map f))
         (initial-state (make-initial-state map))
         (final-state (deep-copy-state initial-state))
         (initial-position (state-position initial-state)))
    (all-moves final-state)
    (count-guard-blockers final-state initial-state)))




;;(with-input-from-string (f *example-input*) (advent-6b f))
;;(print (with-open-file (f "06_input.txt") (advent-6b f)))

(time (with-open-file (f "06_input.txt") (advent-6b f)))

(sb-sprof:with-profiling (:report :flat)
  (with-open-file (f "06_input.txt") (advent-6b f)))
