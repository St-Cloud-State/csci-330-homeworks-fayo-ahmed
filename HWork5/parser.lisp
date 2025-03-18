(defun parse-G (input)
  "Parse the non-terminal G."
  (if (and (>= (length input) 1)
           (member (subseq input 0 1) '("x" "y" "z" "w") :test #'string=))
      (subseq input 1)
      nil))

(defun parse-E (input)
  "Parse the non-terminal E."
  (cond
    ((< (length input) 1) nil)
    (t (or (parse-G input)
           (and (>= (length input) 2)
                (string= (subseq input 0 1) "o")
                (let ((rest (parse-G (subseq input 1))))
                  (if rest
                      (parse-E rest)
                      nil))))))

(defun parse-S (input)
  "Parse the non-terminal S."
  (cond
    ((< (length input) 1) nil)
    ((string= (subseq input 0 1) "s")
     (subseq input 1))
    ((string= (subseq input 0 1) "d")
     (if (>= (length input) 2)
         (let ((rest (parse-L (subseq input 1))))
           (if (and rest (>= (length rest) 1)
                    (string= (subseq rest 0 1) "b"))
               (subseq rest 1)
               nil))
         nil))
    (t nil)))

(defun parse-L (input)
  "Parse the non-terminal L."
  (if (and (>= (length input) 1) (string= (subseq input 0 1) "s"))
      (let ((rest (subseq input 1)))
        (if (and (>= (length rest) 1) (string= (subseq rest 0 1) "s"))
            (parse-L rest)
            rest))
      nil))

(defun parse-ES (input)
  "Parse the non-terminal ES."
  (let ((rest (parse-E input)))
    (if rest
        (parse-S rest)
        nil)))

(defun parse-I (input)
  "Parse the non-terminal I."
  (if (or (< (length input) 1)
          (not (string= (subseq input 0 1) "i")))
      nil
      (let ((rest (parse-ES (subseq input 1))))
        (if (and rest (>= (length rest) 2) (string= (subseq rest 0 1) "e"))
            (parse-S (subseq rest 1))
            rest))))

(defun parse (input)
  "Main function to parse the input string."
  (let ((result (parse-I input)))
    (if (and result (string= result ""))
        (format t "~a is valid.~%" input)
        (format t "~a is invalid.~%" input))))