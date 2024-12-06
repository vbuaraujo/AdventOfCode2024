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

(defstruct state map position direction trails steps)

(defun make-empty-trails (map)
  (let* ((height (array-dimension map 0))
         (width (array-dimension map 1)))
    (coerce
     (loop for i across *directions*
           collect (make-array (list height width)
                               :initial-element nil))
     'vector)))


(defun update-trail (trails position direction steps)
  (destructuring-bind (i . j) position
    (setf (aref (aref trails direction) i j) steps)))

(defun make-initial-state (map)
  (let* ((position (find-position map))
         (trails (make-empty-trails map))
         (state
           (make-state :map map
                       :position position
                       :direction 0
                       :trails trails
                       :steps 0)))
    state))

(defun one-move (state)
  (let* ((map (state-map state))
         (height (array-dimension map 0))
         (width (array-dimension map 1)))
    (update-trail (state-trails state)
                  (state-position state)
                  (state-direction state)
                  (state-steps state))
    (incf (state-steps state))
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
      (values state
              blockers
              (length (remove initial-position blockers :test 'equal)))
      )))


(defun free-spot-p (map i j)
  (and (< -1 i (array-dimension map 0))
       (< -1 j (array-dimension map 1))
       (char/= (aref map i j) #\#)))

(defun look-for-trail (state start-i start-j direction steps)
  (destructuring-bind (di . dj) (aref *directions* direction)
    (with-slots (map trails) state
      (loop for i = (+ start-i di) then (+ i di)
            for j = (+ start-j dj) then (+ j dj)
            while (free-spot-p map i j)
            when (let ((trail-steps (aref (aref trails direction) i j)))
                   (and trail-steps
                        (< trail-steps steps)
                        ))
              do (return-from look-for-trail (cons i j)))))
  nil)


(defun find-guard-blockers (state)
  (let ((blockers '()))
    (with-slots (map trails) state
      (loop for i from 0 below (array-dimension map 0) do
        (loop for j from 0 below (array-dimension map 1) do
          (loop for direction from 0 to 3
                for (di . dj) = (aref *directions* direction) do
            (let ((steps (aref (aref trails direction) i j)))
              (when (and steps
                         (free-spot-p map (+ i di) (+ j dj))
                         (look-for-trail state i j (turn direction) steps))
                (push (cons (+ i di) (+ j dj)) blockers)))))))
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
