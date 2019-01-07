(defvar *test-input* "dabAcCaCBAcCcaDA")

(defvar *input*
  (with-open-file (stream "./05.input")
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun destroy-units-p (u1 u2)
  (and (eq (char-upcase u1) (char-upcase u2))
       (not (eq u1 u2))))

(defun destroy-units (units index)
  (concatenate 'string
	       (subseq units 0 index)
	       (subseq units (+ 2 index))))

(defun remove-unit (unit units)
  (remove-if (lambda (u) (eq (char-upcase u) (char-upcase unit))) units))

(defun solve-part-1 (all-units)
  (labels ((iter (units index)
	     (cond
	       ((= (1+ index) (length units)) units)
	       ((destroy-units-p (elt units index) (elt units (1+ index)))
		(iter (destroy-units units index) (max 0 (- index 1))))
	       (t (iter units (1+ index))))))
    (length (iter all-units 0))))

(defun solve-part-2 (all-units)
  (let ((alphabet "abcdefghijklmnopqrstuvwxyz")
	(min-count (length all-units)))
    (dotimes (i (length alphabet))
      (let* ((reduced-units (remove-unit (elt alphabet i) all-units))
	     (count (solve-part-1 reduced-units)))
	(when (< count min-count) (setf min-count count))))
    min-count))

(= 10 (solve-part-1 *test-input*))
(= 10972 (solve-part-1 *input*))

(= 4 (solve-part-2 *test-input*))
(= 5278 (solve-part-2 *input*))

