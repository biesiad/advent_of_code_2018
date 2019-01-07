(defvar *input*
  (let ((input '()))
    (with-open-file (stream "./04.input")
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push line input)))
    (reverse input)))

(defvar *test-input*
  '("[1518-11-01 00:00] Guard #0010 begins shift"
    "[1518-11-01 00:05] falls asleep"
    "[1518-11-01 00:25] wakes up"
    "[1518-11-01 00:30] falls asleep"
    "[1518-11-01 00:55] wakes up"
    "[1518-11-01 23:58] Guard #0099 begins shift"
    "[1518-11-02 00:40] falls asleep"
    "[1518-11-02 00:50] wakes up"
    "[1518-11-03 00:05] Guard #0010 begins shift"
    "[1518-11-03 00:24] falls asleep"
    "[1518-11-03 00:29] wakes up"
    "[1518-11-04 00:02] Guard #0099 begins shift"
    "[1518-11-04 00:36] falls asleep"
    "[1518-11-04 00:46] wakes up"
    "[1518-11-05 00:03] Guard #0099 begins shift"
    "[1518-11-05 00:45] falls asleep"
    "[1518-11-05 00:55] wakes up"))

(defclass guard-shift ()
  ((id
    :initarg :id
    :accessor id)
   (timestamps
    :initarg :timestamps
    :accessor timestamps)))

(defun parse-guard-id (line)
  (parse-integer (subseq line 26 30)))

(defun parse-timestamp (line)
  (parse-integer (subseq line 15 17)))

(defun parse-type (line)
  (cond
    ((search "begins" line) :begins-shift)
    ((search "wakes" line) :wakes-up)
    ((search "falls" line) :falls-asleep)
    (t (error "can't parse type"))))

(defun push-timestamp (log shift)
  (setf (slot-value shift 'timestamps)
	(cons (parse-timestamp log)
	      (slot-value shift 'timestamps)))
  shift)

(defun parse (logs)
  (let ((shifts '()))
    (dolist (log (sort (copy-list logs) (lambda (s1 s2) (string-lessp s1 s2))))
      (let ((type (parse-type log)))	   
	(cond
	  ((eq type :begins-shift) 
	   (push
	    (make-instance 'guard-shift :id (parse-guard-id log) :timestamps '())
	    shifts))
	  ((or (eq type :falls-asleep)
	       (eq type :wakes-up))
	   (setf (first shifts) (push-timestamp log (first shifts))))
	  (t (error "not supported log type")))))
    shifts))

(defun sum-sleep (timestamps)
  (let ((sum 0))
    (do ((i 0 (+ 2 i)))
	((= i (length timestamps)))
      (setf sum
	    (+ sum
	       (- (elt timestamps i) (elt timestamps (+ i 1))))))
    sum))

(defun guard-asleep-p (minute timestamps)
  (if (null timestamps) nil
      (let ((result nil))
	(do ((i 0 (+ 2 i)))
	    ((= i (length timestamps)) result)
	  (setf result (or result
			   (and
			    (< minute (elt timestamps i))
			    (>= minute (elt timestamps (+ i 1))))))))))

(defun max-sleep-guard-id (shifts)
  (let ((sleep-by-guard-id (make-hash-table)))
    (dolist (shift shifts)
      (let ((id (id shift))
	    (timestamps (timestamps shift)))
	(when (not (null timestamps))
	  (setf (gethash id sleep-by-guard-id)
		(+ (sum-sleep timestamps)
		   (gethash id sleep-by-guard-id 0))))))
    (let ((max-sum 0) (max-id 0))
      (maphash (lambda (id sum)
		 (when (> sum max-sum)
		   (setf max-sum sum max-id id)))
	       sleep-by-guard-id)
      max-id)))

(defun max-sleep-minute (guard-id shifts)
  (let ((sleep-by-minute (make-array 60)))
    (dolist (shift (remove-if-not (lambda (shift) (= guard-id (id shift))) shifts))
      (dotimes (minute 60)
	(when (guard-asleep-p minute (timestamps shift))
	  (incf (elt sleep-by-minute minute)))))
    (values
     (position (reduce #'max sleep-by-minute) sleep-by-minute)
     (reduce #'max sleep-by-minute))))

(defun max-sleep-guard-minute (shifts)
  (let ((guard-ids (remove-duplicates (mapcar (lambda (shift) (id shift)) shifts)))
	(max-sum 0)
	(max-minute)
	(max-guard-id 0))
    (dolist (guard-id guard-ids)
      (multiple-value-bind (minute sum) (max-sleep-minute guard-id shifts)
	(when (> sum max-sum) (setf
			       max-sum sum
			       max-minute minute
			       max-guard-id guard-id))))
    (values max-guard-id max-minute)))

(defun solve-part-1 (shifts)
  (let* ((guard-id (max-sleep-guard-id shifts))
	(minute (max-sleep-minute guard-id shifts)))
    (* guard-id minute)))

(defun solve-part-2 (shifts)
  (multiple-value-bind (guard-id minute)
      (max-sleep-guard-minute shifts)
    (* guard-id minute)))

(print (solve-part-1 (parse *test-input*))) ;;240
(print (solve-part-1 (parse *input*))) ;; 26281

(print (solve-part-2 (parse *test-input*))) ;; 4455
(print (solve-part-2 (parse *input*))) ;; 73001


 
