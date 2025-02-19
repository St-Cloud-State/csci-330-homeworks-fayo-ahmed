(defun partition (lst)
  "Splits the list into two nearly equal halves."
  (let ((half (floor (/ (length lst) 2))))
    (list (subseq lst 0 half) (subseq lst half))))

(defun merge-lists (left right)
  "Merges two sorted lists into one sorted list."
  (cond
    ((null left) right)
    ((null right) left)
    ((<= (car left) (car right))
     (cons (car left) (merge-lists (cdr left) right)))
    (t
     (cons (car right) (merge-lists left (cdr right))))))

(defun mergesort (lst)
  "Recursively applies Mergesort to sort the list."
  (if (<= (length lst) 1)
      lst
      (let ((parts (partition lst)))
        (merge-lists (mergesort (first parts)) (mergesort (second parts))))))

(format t "Recursive Mergesort Output: ~a~%" (mergesort '(7 2 5 3 8 1 4 6)))
