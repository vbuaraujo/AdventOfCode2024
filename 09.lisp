(defstruct file id size free)

(defun aref* (seq i default)
  (if (< i (length seq))
      (aref seq i)
    default))

(defun parse-input (text)
  (loop for i from 0 below (length text) by 2
        for id from 0
        collect (make-file :id id
                           :size (digit-char-p (aref text i))
                           :free (digit-char-p (aref* text (+ i 1) #\0)))))


(defun files->blocks (files)
  (let ((blocks (make-array 0 :fill-pointer t)))
    (loop for file in files do
      (with-slots (id size free) file
        (loop for _ from 0 below size do (vector-push-extend id blocks))
        (loop for _ from 0 below free do (vector-push-extend nil blocks))))
    blocks))

(defun compact (blocks)
  (labels ((recur (i j)
           (cond ((<= j i)
                  blocks)
                 ((null (aref blocks j))
                  (recur i (- j 1)))
                 ((not (null (aref blocks i)))
                  (recur (+ i 1) j))
                 (t
                  (setf (aref blocks i) (aref blocks j))
                  (setf (aref blocks j) nil)
                  (recur (+ i 1) (- j 1))))))
    (recur 0 (- (length blocks) 1))))

(defun checksum (blocks)
  (loop for i from 0 below (length blocks)
        when (aref blocks i)
          sum (* i (aref blocks i))))

(defun advent-9a (f)
  (checksum (compact (files->blocks (parse-input (read-line f))))))

(with-open-file (f "09_input.txt") (advent-9a f))

(defun print-blocks (blocks)
  (loop for b across blocks do
    (princ (or b #\.)))
  (terpri))


;;

(defstruct dcons
  value
  (prev nil)
  (next nil))

(defun list->dcons (list)
  (if (null list)
      nil
    (let* ((head (make-dcons :value (first list)))
           (tail head))
      (loop for x in (rest list) do
        (let ((new (make-dcons :value x :prev tail :next nil)))
          (setf (dcons-next tail) new)
          (setf tail new)))
      (values head tail))))

(declaim (optimize (debug 3))) ;; the liest lie

(defun compact-whole (files)
  (multiple-value-bind (start tail) (list->dcons files)
    (loop for tail in (loop for node = tail then (dcons-prev node)
                            while node
                            collect node)
          do
      ;;(format t "~a~%" (dcons-value tail))
      (loop for head = start then (dcons-next head)
            while (not (eq head tail)) do
              (cond ((<= (file-size (dcons-value tail))
                         (file-free (dcons-value head)))
                     ;;(format t "~&Moving ~a to after ~a~%" (dcons-value tail) (dcons-value head))
                     ;;(format t "BEFORE: ~a~%" (dcons-value (dcons-prev tail)))
                     (incf (file-free (dcons-value (dcons-prev tail)))
                           (+ (file-size (dcons-value tail))
                              (file-free (dcons-value tail))))
                     ;;(format t " AFTER: ~a~%" (dcons-value (dcons-prev tail)))
                     ;; found our place.
                     (let ((free-space (file-free (dcons-value head))))
                       (setf (file-free (dcons-value head)) 0)
                       (setf (file-free (dcons-value tail))
                             (- free-space (file-size (dcons-value tail)))))
                     (dcons-move-after head tail)
                     (when (eq tail (dcons-next tail))
                       (break)) ;;breakpoint
                     ;; (print (loop for head = start then (dcons-next head)
                     ;;              while head
                     ;;              collect (file-id (dcons-value head))))
                     (return)))))
    (loop for head = start then (dcons-next head)
          while head
          ;;do (debug-node "ach" head)
          collect (dcons-value head))
          ))

(defun debug-node (text node)
  (format t "~a ~a (prev=~a, next=~a)~%"
          text
          (and node (file-id (dcons-value node)))
          (and node (dcons-prev node) (file-id (dcons-value (dcons-prev node))))
          (and node (dcons-next node) (file-id(dcons-value (dcons-next node))))))

(defun dcons-move-after (head tail)
  (let ((tail-prev (dcons-prev tail))
        (tail-next (dcons-next tail))
        ;;(head-prev (dcons-prev head))
        (head-next (dcons-next head)))
    (unless (eq head-next tail)
      ;; (debug-node "before: head: " head)
      ;; (debug-node "before: tail: " tail)
      ;; Remove tail out of the list...
      (when tail-prev
        (setf (dcons-next tail-prev) tail-next))
      (when tail-next
        (setf (dcons-prev tail-next) tail-prev))
      ;; ...and now insert it after head.
      (when head-next
        (setf (dcons-prev (dcons-next head)) tail))
      (setf (dcons-next head) tail)
      (setf (dcons-prev tail) head)
      (setf (dcons-next tail) head-next)
      ;; (debug-node " after: head: " head)
      ;; (debug-node " after: tail: " tail)
      ;; (debug-node " after: head: " head)
      ;; (debug-node " after: tail: " tail)
      )))

(defun advent-9b (f)
  (checksum (files->blocks (compact-whole (parse-input (read-line f))))))
;;(with-open-file (f "09_input.txt") (advent-9b f))
