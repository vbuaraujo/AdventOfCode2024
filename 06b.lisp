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

(defstruct state map position direction trails)

(defun make-empty-trails (map)
  (let* ((height (array-dimension map 0))
         (width (array-dimension map 1)))
    (coerce
     (loop for i across *directions*
           collect (make-array (list height width)
                               :element-type 'bit :initial-element 0))
     'vector)))


(defun update-trail (trails position direction)
  (destructuring-bind (i . j) position
    (setf (aref (aref trails direction) i j) 1)))

(defun make-initial-state (map)
  (let* ((position (find-position map))
         (trails (make-empty-trails map))
         (state
           (make-state :map map
                       :position position
                       :direction 0
                       :trails trails)))
    (update-trail trails position 0)
    state))

(defun one-move (state)
  (let* ((map (state-map state))
         (height (array-dimension map 0))
         (width (array-dimension map 1)))
    (update-trail (state-trails state)
                  (state-position state)
                  (state-direction state))
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
                      (setf (state-position state) (cons new-i new-j))

                      )
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

(defun advent-6b (f)
  (let* ((map (read-map f))
         (state (make-initial-state map))
         (initial-position (state-position state)))
    (loop while (one-move state))
    (let ((blockers (find-guard-blockers state)))
      (values state blockers))))
;;      (length (remove initial-position blockers :test 'equal)))))


(defun free-spot-p (map i j)
  (and (< -1 i (array-dimension map 0))
       (< -1 j (array-dimension map 1))
       (char/= (aref map i j) #\#)))

(defun look-for-trail (state start-i start-j direction)
  (destructuring-bind (di . dj) (aref *directions* direction)
    (with-slots (map trails) state
      (let ((height (array-dimension map 0))
            (width (array-dimension map 1)))
        (loop for i = (+ start-i di) then (+ i di)
              for j = (+ start-j dj) then (+ j dj)
              while (and (< -1 i height)
                         (< -1 j width)
                         (char/= (aref map i j) #\#))
            when (= 1 (aref (aref trails direction) i j))
              do (return-from look-for-trail (cons i j))))))
  nil)


(defun find-guard-blockers (state)
  (let ((blockers '()))
    (with-slots (map trails) state
      (loop for i from 0 below (array-dimension map 0) do
        (loop for j from 0 below (array-dimension map 1) do
          (loop for direction from 0 to 3 do
            (when (= 1 (aref (aref trails direction) i j))
              (let ((try (try-find-loop map trails i j direction)))
                (when try
                  (format t "Here at ~a ~a [dir ~a], found ~a~%" i j direction try)
                  (push try blockers))))))))
    blockers))

(defun try-find-loop (map trails i j direction)
  (let ((turn-direction (turn direction)))
    (destructuring-bind (di . dj) (aref *directions* direction)
      (destructuring-bind (turn-di . turn-dj) (aref *directions* turn-direction)
        (print (list (= 1 (aref (aref trails direction) i j))
                     (free-spot-p map (+ i di) (+ j dj))
                     (free-spot-p map (+ i turn-di) (+ j turn-dj))
                     (list 'aref turn-direction (+ i turn-di) (+ j turn-dj))
                     (= 1 (aref (aref trails turn-direction) (+ i turn-di) (+ j turn-dj)))))
        (when (and (= 1 (aref (aref trails direction) i j))
                   (free-spot-p map (+ i di) (+ j dj))
                   (free-spot-p map (+ i turn-di) (+ j turn-dj))
                   (= 1 (aref (aref trails turn-direction) (+ i turn-di) (+ j turn-dj))))
          (cons (+ i di) (+ j dj)))))))



(with-input-from-string (f *example-input*) (advent-6b f))
(with-open-file (f "06_input.txt") (advent-6b f))
