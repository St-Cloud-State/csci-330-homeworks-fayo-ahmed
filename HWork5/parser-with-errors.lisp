(defun pd-error (msg pos)
  "Return an error structure with message and position."
  (list :error (format nil "~a at position ~a" msg pos)))

(defun parse-G (input pos)
  "Parse non-terminal G: expects x, y, z, or w."
  (if (>= pos (length input))
      (pd-error "Unexpected end of input in G" pos)
      (let ((ch (subseq input pos (1+ pos))))
        (if (member ch '("x" "y" "z" "w") :test #'string=)
            (list :ok (1+ pos))
            (pd-error (format nil "Expected one of (x y z w), found ~a" ch) pos)))))

(defun parse-E-prime (input pos)
  "Parse E': E' -> o G E' | ϵ."
  (if (>= pos (length input))
      (list :ok pos)  ; epsilon production: nothing to consume
      (let ((ch (subseq input pos (1+ pos))))
        (if (string= ch "o")
            (let ((g-result (parse-G input (1+ pos))))
              (if (getf g-result :error)
                  g-result
                  (parse-E-prime input (getf g-result :ok))))
            (list :ok pos)))))  ; epsilon production

(defun parse-E (input pos)
  "Parse non-terminal E: E -> G E'."
  (let ((g-result (parse-G input pos)))
    (if (getf g-result :error)
        g-result
        (parse-E-prime input (getf g-result :ok)))))

(defun parse-L-prime (input pos)
  "Parse L': L' -> s L' | ϵ."
  (if (>= pos (length input))
      (list :ok pos)
      (let ((ch (subseq input pos (1+ pos))))
        (if (string= ch "s")
            (parse-L-prime input (1+ pos))
            (list :ok pos)))))

(defun parse-L (input pos)
  "Parse non-terminal L: L -> s L'."
  (if (>= pos (length input))
      (pd-error "Expected 's' in L but reached end of input" pos)
      (let ((ch (subseq input pos (1+ pos))))
        (if (string= ch "s")
            (parse-L-prime input (1+ pos))
            (pd-error (format nil "Expected 's' at beginning of L, found ~a" ch) pos)))))

(defun parse-S (input pos)
  "Parse non-terminal S: S -> s | d L b."
  (if (>= pos (length input))
      (pd-error "Unexpected end of input in S" pos)
      (let ((ch (subseq input pos (1+ pos))))
        (cond ((string= ch "s")
               (list :ok (1+ pos)))
              ((string= ch "d")
               (let ((l-result (parse-L input (1+ pos))))
                 (if (getf l-result :error)
                     l-result
                     (let ((new-pos (getf l-result :ok)))
                       (if (>= new-pos (length input))
                           (pd-error "Expected 'b' after L in S, but reached end" new-pos)
                           (if (string= (subseq input new-pos (1+ new-pos)) "b")
                               (list :ok (1+ new-pos))
                               (pd-error (format nil "Expected 'b' after L in S, found ~a" 
                                                  (subseq input new-pos (1+ new-pos))) new-pos)))))))
              (t (pd-error (format nil "Unexpected symbol in S: ~a" ch) pos))))))

(defun parse-ES (input pos)
  "Parse ES: first parse E then S."
  (let ((e-result (parse-E input pos)))
    (if (getf e-result :error)
        e-result
        (parse-S input (getf e-result :ok)))))

(defun parse-I (input pos)
  "Parse non-terminal I: I -> i ES | i ES e S."
  (if (>= pos (length input))
      (pd-error "Expected 'i' at beginning of I but reached end" pos)
      (let ((ch (subseq input pos (1+ pos))))
        (if (not (string= ch "i"))
            (pd-error (format nil "Expected 'i' at beginning of I, found ~a" ch) pos)
            (let ((es-result (parse-ES input (1+ pos))))
              (if (getf es-result :error)
                  es-result
                  (let ((new-pos (getf es-result :ok)))
                    (if (and (< new-pos (length input))
                             (string= (subseq input new-pos (1+ new-pos)) "e"))
                        ;; If there's an 'e', then expect another S after the 'e'
                        (let ((s-result (parse-S input (1+ new-pos))))
                          (if (getf s-result :error)
                              s-result
                              s-result))
                        (list :ok new-pos)))))))))

(defun parse (input)
  "Top-level parse function that reports valid string or detailed error message."
  (let ((result (parse-I input 0)))
    (cond ((getf result :error)
           (format t "Input: \"~a\" -> ERROR: ~a~%" input (getf result :error)))
          ((/= (getf result :ok) (length input))
           (format t "Input: \"~a\" -> ERROR: Input not fully consumed. Leftover at position ~a~%"
                   input (getf result :ok)))
          (t (format t "Input: \"~a\" -> SUCCESS: String is valid.~%" input)))))

(defun run-tests (test-list)
  "Run parse on each string in TEST-LIST and print a clearly formatted result."
  (dolist (s test-list)
    (parse s)
    (format t "~%")))
