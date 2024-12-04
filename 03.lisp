;; Day 3.

(require 'cl-ppcre)

(defun multiply-corrupted (text)
  (let ((total 0))
    (ppcre:do-register-groups (x y) ("mul\\(([0-9]+),([0-9]+)\\)" text)
      (let ((product (* (parse-integer x)
                        (parse-integer y))))
        (incf total product)))
    total))

(defun advent-3a (f)
  (loop for line = (read-line f nil nil)
        while line
        sum (multiply-corrupted line)))

(with-open-file (f "03_input.txt") (advent-3a f))

(defun multiply-corrupted-conditional (text)
  (let ((total 0)
        (enabled t))
    (ppcre:do-register-groups (mul x y enable disable) ("(mul)\\(([0-9]+),([0-9]+)\\)|(do)\\(\\)|(don't)\\(\\)" text)
      (cond (enable (setf enabled t))
            (disable (setf enabled nil))
            (mul (when enabled
                   (let ((product (* (parse-integer x)
                                     (parse-integer y))))
                     (incf total product))))))
    total))

(defun advent-3b (f)
  (multiply-corrupted-conditional (uiop:slurp-stream-string f)))

(with-open-file (f "03_input.txt") (advent-3b f))
