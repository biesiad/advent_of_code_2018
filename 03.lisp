(defvar *input*
  (let ((input '()))
    (with-open-file (stream "./03.input")
      (loop for line = (read-line stream nil)
	 until (null line)
	 do (push line input)))
    (reverse input)))
    
(defun split-by-char (string char)
  (loop for i = 0 then (1+ j)
     as j = (position char string :start i)
     collect (subseq string i j)
     while j))

(defun parse-line (line)
  (let* ((sections (split-by-char line #\Space))
	 (id (nth 0 sections))
	 (xy (split-by-char (nth 2 sections) #\,))
	 (x (parse-integer (car xy)))
	 (y (parse-integer (remove #\: (cadr xy))))
	 (wh (split-by-char (nth 3 sections) #\x))
	 (w (parse-integer (car wh)))
	 (h (parse-integer (cadr wh))))
    `((:id . ,id)
      (:x . ,x)
      (:y . ,y)
      (:w . ,w)
      (:h . ,h))))

(defun coordinates-to-points (coordinates)
  "coordinates as alist ((:x . x) (:y . y)..."
  (let ((x (cdr (assoc :x coordinates)))
	(y (cdr (assoc :y coordinates)))
	(w (cdr (assoc :w coordinates)))
	(h (cdr (assoc :h coordinates)))
	(points '()))
    (loop for xi from x below (+ x w) do
	 (loop for yi from y below (+ y h) do
	      (push (list xi yi) points)))
    points))

(defun aggregate-overlapping-points (points)
  (let ((sums (make-hash-table :test 'equal)))
    (dolist (point points sums)
      (setf (gethash point sums) (+ (gethash point sums 0) 1)))))

(defun count-overlapping-points (hash)
  (let ((count 0))
    (maphash
     (lambda (key value)
       (declare (ignore key))
       (setf count (if (> value 1) (+ 1 count) count)))
     hash)
    count))

(defun claim-points-overlap-p(points aggregated-points)
  (reduce
   (lambda (acc point)
     (or (> (gethash point aggregated-points 0) 1) acc))
   points
   :initial-value nil))

(defun parse-fabric (input)
  (aggregate-overlapping-points
   (reduce
    (lambda (acc line) (append (coordinates-to-points (parse-line line)) acc))
    input
    :initial-value '())))

(defun parse-claims (input)
  (mapcar (lambda (line) (parse-line line)) input))

(defun solve-part-1 (input)
  (count-overlapping-points (fabric input)))

(defun solve-part-2 (input)
  (let ((fabric (parse-fabric *input*))
	(claims (parse-claims *input*)))
    (cdr (assoc :id (first (remove-if
		       (lambda (claim) (claim-points-overlap-p
					(coordinates-to-points claim)
					fabric))
		       claims))))))
 
(print (solve-part-1 *input*)) ;; 101656
(print (solve-part-2 *input*)) ;; 656

