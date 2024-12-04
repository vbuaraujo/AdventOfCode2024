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
