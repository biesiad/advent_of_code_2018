(defun load-input (input-file)
  (let ((input '()))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push (parse-integer line) input)))
    (reverse input)))


(defun frequency (deltas current)
  (if (null (car deltas))
      current
      (frequency (cdr deltas) (+ (car deltas) current))))

(print (frequency (load-input "./01.input") 0))

(defun first-repeated-frequency (all-deltas)
  (flet ((iter (deltas current frequencies)
    (if (member current frequencies)
	current
	 (iter
	  (if (null (cdr deltas)) all-deltas (cdr deltas))
	  (+ (car deltas) current)
	  (cons current frequencies)))))
(iter all-deltas 0 '())))

(print (first-repeated-frequency '(+1 -1)))
(print (first-repeated-frequency '(+3 +3 +4 -2 -4)))
(print (first-repeated-frequency '(-6 +3 +8 +5 -6)))
(print (first-repeated-frequency '(+7 +7 -2 -7 -4)))
(print (first-repeated-frequency (load-input "./01.input")))
