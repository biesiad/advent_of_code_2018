(defvar *input*
  (with-open-file (in "./07.input")
    (loop for line = (read-line in nil)
	  while line
	  collect line)))

(defvar *test-input*
  '("Step C must be finished before step A can begin."
    "Step C must be finished before step F can begin."
    "Step A must be finished before step B can begin."
    "Step A must be finished before step D can begin."
    "Step B must be finished before step E can begin."
    "Step D must be finished before step E can begin."
    "Step F must be finished before step E can begin."))

(defun parse (input)
  (mapcar (lambda (step)
	    (list (intern (subseq step 5 6))
		  (intern (subseq step 36 37))))
	  input))

(defun make-deps-hash-table (steps)
  (reduce (lambda (acc step)
	    (setf (gethash (cadr step) acc)
		  (cons (car step)
			(gethash (cadr step) acc '())))
	    (when (null (gethash (car step) acc))
	      (setf (gethash (car step) acc) '()))
	    acc)
	  steps
	  :initial-value (make-hash-table)))

(defun print-hash-table (hash-table)
  (maphash (lambda (key value)
	     (format t "~a ~a~%" key value))
	   hash-table))

(available (make-deps-hash-table (parse *test-input*)))
(print-hash-table (make-deps-hash-table (parse *test-input*)))

(defun sort-symbols (symbols)
  (sort symbols
	(lambda (a b) (string< (symbol-name a)
			       (symbol-name b)))))

(defun available (steps)
  (let ((available-steps '()))
    (maphash (lambda (key value)
	       (when (null value)
		 (push key available-steps)))
	     steps)
    (sort-symbols available-steps)))

(defun solve-part-1 (steps)
  (let ((deps-by-step (make-deps-hash-table steps))
	(finished '()))
    (do () ((= (hash-table-count deps-by-step) 0)
	    (apply #'concatenate 'string
		   (mapcar #'symbol-name (reverse finished))))
      (let ((available-steps (available deps-by-step)))
	;;(format t "available: ~a, finished: ~a~%" available-steps finished)
	;;(print-hash-table deps-by-step)
	(setf finished (cons (first available-steps) finished))
	(maphash (lambda (key value)
		   (setf (gethash key deps-by-step)
			 (remove (first available-steps) value)))
		 deps-by-step)
	(remhash (first available-steps) deps-by-step)))))

(string= "CABDFE" (solve-part-1 (parse *test-input*)))
(string= "BFGKNRTWXIHPUMLQVZOYJACDSE" (solve-part-1 (parse *input*)))
	 
