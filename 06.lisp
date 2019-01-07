(defvar *input*
  (with-open-file (stream "./06.input")
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defvar *test-input*
  '("1, 1"
    "1, 6"
    "8, 3"
    "3, 4"
    "5, 5"
    "8, 9"))

(defun parse-coordinate (string)
  (list
   (parse-integer (subseq string 0 (position #\, string)))
   (parse-integer (subseq string (+ (position #\, string) 2)))))

(defun parse (input)
  (mapcar #'parse-coordinate input))

(defun distance (x y coordinate)
  (+ (abs (- x (car coordinate)))
     (abs (- y (cadr coordinate)))))

(defun max-x (coordinates)
  (apply #'max (mapcar (lambda (c) (first c)) coordinates)))

(defun max-y (coordinates)
  (apply #'max (mapcar (lambda (c) (cadr c)) coordinates)))

(defun area-infinite-p (coordinate grid)
  (destructuring-bind (y-count x-count) (array-dimensions grid)
    (dotimes (y y-count)
      (when (equal coordinate (aref grid y 0)) (return-from area-infinite-p t))
      (when (equal coordinate (aref grid y (1- x-count))) (return-from area-infinite-p t)))
    (dotimes (x x-count)
      (when (equal coordinate (aref grid 0 x)) (return-from area-infinite-p t))
      (when (equal coordinate (aref grid (1- y-count) x)) (return-from area-infinite-p t)))))
      
(defun initialize-grid (coordinates)
  (let* ((max-x (1+ (max-x coordinates)))
	 (max-y (1+ (max-y coordinates)))
	 (max-distance (+ max-x max-y))
	 (grid-distance (make-array (list max-x max-y) :initial-element max-distance))
    	 (grid-area (make-array (list max-x max-y) :initial-element nil)))
    (dotimes (x max-x)
      (dotimes (y max-y)
	(let ((min-distance max-distance))
	  (dolist (coordinate coordinates)
	    (let ((distance (distance x y coordinate))
		  (coordinate-x (car coordinate))
		  (coordinate-y (cadr coordinate)))
	      (setf (aref grid-distance coordinate-x coordinate-y) 'c)
	      (setf (aref grid-area coordinate-x coordinate-y) '(c c))	     
	      (when (not (and
			  (= x coordinate-x)
			  (= y coordinate-y)))
		 (cond ((= distance min-distance)
			(setf (aref grid-distance x y) '*)
			(setf (aref grid-area x y) '(* *)))
		      ((< distance min-distance)
		       (setf min-distance distance)
		       (setf (aref grid-distance x y) distance)
		       (setf (aref grid-area x y) coordinate)))))))))
    grid-area))

(defun solve-part-1 (coordinates)
  (let ((grid (initialize-grid coordinates))
	(sum-by-coordinate (make-hash-table :test 'equal))
	(max-sum 0)
	(max-coordinate nil))
    (destructuring-bind (x-count y-count) (array-dimensions grid)
      (dotimes (x x-count)
	(dotimes (y y-count)
	  (let* ((coordinate (aref grid x y))
		 (sum (incf (gethash coordinate sum-by-coordinate 0))))
	    (when (and
		   (not (equal coordinate '(* *)))
		   (not (equal coordinate '(c c)))
		   (not (area-infinite-p coordinate grid))
  		   (> sum max-sum))
	      (setf max-coordinate coordinate)
	      (setf max-sum sum))))))
    (values (1+ max-sum) max-coordinate)))

(defun solve-part-2 (coordinates max-total-distance)
  (let ((grid (initialize-grid coordinates))
	(count 0))
    (destructuring-bind (x-count y-count) (array-dimensions grid)
      (dotimes (x x-count)
	(dotimes (y y-count)
	  (let ((total-distance 0))
	    (dolist (coordinate coordinates)
	      (setf total-distance
		    (+ total-distance
		       (distance x y coordinate))))
	    (when (< total-distance max-total-distance)
	      (incf count))))))
    count))

(= 4060 (solve-part-1 (parse *input*)))
(= 36136 (solve-part-2 (parse *input*) 10000))

