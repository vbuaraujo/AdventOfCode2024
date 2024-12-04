;; (defun read-lines-from-path (path)
;;   (with-open-file (f path)
;;     (read-lines-from-stream f)))

;; (defun read-lines-from-stream (f)
;;     (loop for line = (read-line f nil nil)
;;           while line
;;           collect line))

;; (defun read-lines-from-string (text)
;;   (with-input-from-string (f text)
;;     (read-lines-from-stream f)))


;; (defun split-string (text sep)
;;   (let ((length (length text)))
;;     (loop for start = 0 then (+ end 1)
;;           while (<= start length)
;;           for end = (or (position sep text :start start) length)
;;           collect (subseq text start end))))
