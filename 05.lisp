(defparameter *example-input*
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defun parse-orderings (f)
  (loop for line = (read-line f nil "")
        while (not (equal line ""))
        collect (destructuring-bind (x y) (uiop:split-string line :separator "|")
                  (cons (parse-integer x) (parse-integer y)))))

(defun parse-manuals (f)
  (loop for line = (read-line f nil nil)
        while line
        collect (mapcar #'parse-integer (uiop:split-string line :separator ","))))


(defun make-hash-set (items)
  (let ((set (make-hash-table :test 'equal)))
    (loop for item in items do
      (setf (gethash item set) t))
    set))

(defun make-ordering-function (pairs)
  (let ((set (make-hash-set pairs)))
    (lambda (x y)
      (cond ((gethash (cons x y) set) t)
            ((gethash (cons y x) set) nil)
            ((equal x y) nil)
            (t (error "Ordering is not total, send help"))))))

(defun ordered-p (items ord)
  (every (lambda (x y)
           (funcall ord x y))
         items
         (rest items)))

(defun middle-page (manual)
  (elt manual (truncate (length manual) 2)))

(defun advent-5a (f)
  (let* ((pairs (parse-orderings f))
         (manuals (parse-manuals f))
         (order-function (make-ordering-function pairs)))
    (loop for manual in manuals
          when (ordered-p manual order-function)
            sum (middle-page manual))))

(defun advent-5b (f)
  (let* ((pairs (parse-orderings f))
         (manuals (parse-manuals f))
         (order-function (make-ordering-function pairs)))
    (loop for manual in manuals
          when (not (ordered-p manual order-function))
            sum (middle-page (sort manual order-function)))))

(with-open-file (f "05_input.txt") (advent-5a f))
(with-open-file (f "05_input.txt") (advent-5b f))
