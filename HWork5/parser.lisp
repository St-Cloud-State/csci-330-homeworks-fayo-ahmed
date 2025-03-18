(defun parse-G (input)
  "Parse the non-terminal G: matches x, y, z, or w."
  (if (and (>= (length input) 1)
           (member (subseq input 0 1)
                   '("x" "y" "z" "w")
                   :test #'string=))
      (subseq input 1)
      nil))

(defun parse-E-prime (input)
  "Parse the non-terminal E′: E′ → o G E′ | ϵ.
If the first symbol is o, consume it along with a G and then repeat;
otherwise, return input (ϵ production)."
  (if (and (>= (length input) 1)
           (string= (subseq input 0 1) "o"))
      (let ((rest1 (parse-G (subseq input 1))))
        (if rest1
            (parse-E-prime rest1)
            nil))
      input))  ; epsilon case

(defun parse-E (input)
  "Parse the non-terminal E using the left-recursion eliminated form: E → G E′."
  (let ((rest (parse-G input)))
    (if rest
        (parse-E-prime rest)
        nil)))

(defun parse-L-prime (input)
  "Parse the non-terminal L′: L′ → s L′ | ϵ.
Consume additional s characters if present."
  (if (and (>= (length input) 1)
           (string= (subseq input 0 1) "s"))
      (parse-L-prime (subseq input 1))
      input))  ; epsilon production

(defun parse-L (input)
  "Parse the non-terminal L using left factoring: L → s L′.
Requires at least one s."
  (if (and (>= (length input) 1)
           (string= (subseq input 0 1) "s"))
      (parse-L-prime (subseq input 1))
      nil))

(defun parse-S (input)
  "Parse the non-terminal S: S → s | d L b."
  (if (< (length input) 1)
      nil
      (cond ((string= (subseq input 0 1) "s")
             ;; S → s
             (subseq input 1))
            ((string= (subseq input 0 1) "d")
             ;; S → d L b
             (let ((rest (parse-L (subseq input 1))))
               (if (and rest (>= (length rest) 1)
                        (string= (subseq rest 0 1) "b"))
                   (subseq rest 1)
                   nil)))
            (t nil))))

(defun parse-ES (input)
  "Parse the sequence E S (used in I): first parse E then S."
  (let ((rest (parse-E input)))
    (if rest
        (parse-S rest)
        nil)))

(defun parse-I (input)
  "Parse the non-terminal I: I → i E S | i E S e S.
After the initial i, parse E then S. If an 'e' is present,
consume it and then parse another S."
  (if (and (>= (length input) 1)
           (string= (subseq input 0 1) "i"))
      (let ((rest (parse-ES (subseq input 1))))
        (if rest
            (if (and (>= (length rest) 1)
                     (string= (subseq rest 0 1) "e"))
                (parse-S (subseq rest 1))  ; I → i E S e S
                rest)                     ; I → i E S
            nil))
      nil))

(defun parse (input)
  "Main function to parse the input string.
If the input is fully consumed (i.e. returns an empty string),
the string is valid."
  (let ((result (parse-I input)))
    (if (and result (string= result ""))
        (format t "~a is valid.~%" input)
        (format t "~a is invalid.~%" input))))
