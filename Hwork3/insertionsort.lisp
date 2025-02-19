(defun insert-sorted (element sorted-list)
  "Inserts an element into the correct position in a sorted list."
  (if (or (null sorted-list) (<= element (car sorted-list)))
      (cons element sorted-list)
      (cons (car sorted-list) (insert-sorted element (cdr sorted-list)))))

(defun insertion-sort (unsorted sorted)
  "Sorts a list using insertion sort."
  (if (null unsorted)
      sorted  ;; Base case: Return sorted list when unsorted is empty
      (insertion-sort (cdr unsorted) 
                      (insert-sorted (car unsorted) sorted))))

(defun insertion-sort-wrapper (lst)
  "Wrapper function to call insertion-sort with an empty initial sorted list."
  (insertion-sort lst '()))

  ;; Test Cases
(format t "Insertion Sort (Unsorted): ~a~%" (insertion-sort-wrapper '(4 2 7 1 5)))
