(defparameter *example-input*
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defun parse-line (line)
  (mapcar (lambda (x) (parse-integer x :junk-allowed t))
          (uiop:split-string line)))


(defun solve (result terms)
  (destructuring-bind (first . rest) terms
    (cond ((null rest)
           (= result first))
          (t
           (or (solve (- result first) rest) ;; addition
               (solve (/ result first) rest) ;; multiplication
               )))))

(defun advent-7a (f)
  (loop for line = (read-line f nil nil)
        while line
        for (result . terms) = (parse-line line)
        when (solve result (reverse terms))
          sum result))

;; Part 2.

(defun solve-harder (target partial terms operators)
  (cond ((null terms)
         (= target partial))
        (t
         (some (lambda (operator)
                 (let ((new-partial (funcall operator partial (first terms))))
                   (solve-harder target new-partial (rest terms) operators)))
               operators))))

(declaim (ftype (function (integer integer) integer) concat-numbers))
(defun concat-numbers (x y)
  ;;(parse-integer (format nil "~a~a" x y)))
  (if (zerop x)
      y
    (loop for a = (* x 10) then (* a 10)
          for b = (truncate y 10) then (truncate b 10)
          while (/= b 0)
          finally (return (+ a y)))))

(defparameter *operators* (list #'+ #'* #'concat-numbers))

(defun advent-7b (f)
  (loop for line = (read-line f nil nil)
        while line
        for (result first . rest) = (parse-line line)
        when (solve-harder result first rest *operators*)
          sum result))


(with-input-from-string (f *example-input*) (advent-7a f))
(with-open-file (f "07_input.txt") (advent-7a f))

(with-input-from-string (f *example-input*) (advent-7b f))
(with-open-file (f "07_input.txt") (advent-7b f))
