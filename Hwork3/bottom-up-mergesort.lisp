(defun partition-into-pairs (lst)
  "Partitions a list into sorted pairs."
  (if (null lst)
      '()
      (if (null (cdr lst))
          (list (list (car lst)))  ;; Single element case
          (cons (sort (list (car lst) (cadr lst)) #'<)
                (partition-into-pairs (cddr lst))))))

(defun pair-up (lst)
  "Pairs up adjacent lists for merging."
  (cond
    ((null lst) '())  ;; Empty list case
    ((null (cdr lst)) (list lst))  ;; Single remaining list case
    (t (cons (list (car lst) (cadr lst))
             (pair-up (cddr lst))))))  ;; Move forward in pairs

(defun merge-lists (left right)
  "Merges two sorted lists into one sorted list."
  (cond
    ((null left) right)
    ((null right) left)
    ((<= (car left) (car right))
     (cons (car left) (merge-lists (cdr left) right)))
    (t
     (cons (car right) (merge-lists left (cdr right))))))  

(defun merge-pairs (lst)
  "Merges adjacent sublists iteratively."
  (if (null (cdr lst))
      lst
      (merge-pairs
       (mapcar #'(lambda (pair) (if (= (length pair) 2) 
                                    (apply #'merge-lists pair) 
                                    (car pair)))  ;; Keep single lists unchanged
               (pair-up lst)))))

(defun bottom-up-mergesort (lst)
  "Sorts a list using Bottom-Up Mergesort."
  (car (merge-pairs (partition-into-pairs lst))))

;; Test Cases
(format t "Bottom-Up Mergesort Output: ~a~%" (bottom-up-mergesort '(1 7 2 1 8 6 5 3 7 9 4)))

