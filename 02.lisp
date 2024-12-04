;; Day 2.

(defun line-items (line)
  (let ((eof (gensym)))
    (with-input-from-string (f line)
      (loop for item = (read f nil eof)
            while (not (eq item eof))
            collect item))))

(defun report-safe-p (items)
  (and (or (apply #'< items)
           (apply #'> items))
       (every (lambda (x y)
                (<= 1 (abs (- x y)) 3))
              items
              (rest items))))

(defun advent-2a (f)
  (loop for line = (read-line f nil nil)
        while line
        count (report-safe-p (line-items line))))

;; brute force?

(defun report-kinda-safe-p (items)
  (let ((len (length items)))
    (or (report-safe-p items)
        (some #'report-safe-p
              (loop for i from 0 below len
                    collecting (append (subseq items 0 i) (subseq items (1+ i) len)))))))

(defun advent-2b (f)
  (loop for line = (read-line f nil nil)
        while line
        count (report-kinda-safe-p (line-items line))))


(with-open-file (f "02_input.txt") (advent-2a f))
(with-open-file (f "02_input.txt") (advent-2b f))
