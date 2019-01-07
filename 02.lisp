(defun load-input (input-file)
  (let ((input '()))
    (with-open-file (stream input-file)
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push line input)))
    (reverse input)))

(defun has-n (n string)
  (let ((result nil))
    (maphash
     (lambda (key value)
       (declare (ignore key))
       (setf result (or (= value n) result)))
     (reduce
      (lambda (acc c)
	(setf (gethash c acc) (+ 1 (gethash c acc 0)))
	acc)
      (loop for char across string collect char)
      :initial-value (make-hash-table :test 'equal)))
    result))

(defun checksum (ids sum2 sum3)
  (if (null ids)
      (* sum2 sum3)
      (checksum
       (cdr ids)
       (if (has-n 2 (car ids)) (+ sum2 1) sum2)
       (if (has-n 3 (car ids)) (+ sum3 1) sum3))))

(defun similarp (string1 string2)
  (labels ((iter (index difference-count)	
	(cond
	  ((> difference-count 1) nil)
	  ((or
	    (= index (length string1))
	    (= index (length string2)))
	   (= difference-count 1))
	  (t (iter
	      (+ index 1)
	      (if (eq (char string1 index) (char string2 index))
		  difference-count
		  (+ difference-count 1)))))))
    (iter 0 0)))

(defun find-similar (ids)
  (let ((result nil))
    (loop for id1 in ids
       do (loop for id2 in ids
	     do (if (similarp id1 id2)
		    (setf result (list id1 id2)))))
  result))

(defun solve-part-1 (input)
  (checksum input 0 0))

(defun solve-part-2 (input)
  (find-similar input))

(print (solve-part-1 (load-input "./02.input"))) ;; 4693
(print (solve-part-2 (load-input "./02.input")))



