;;; calculate the depth of a tree
;;; root is the number of the root node
;;; edges is an array e.g. (0 5 3 1 2 4) means node 1 leads to 5, node 2 to 3, node 3 to 1 etc.
;;; visited is a list of visited nodes
(defun treedepth (root edges &optional (visited nil))
  ;(print (list "calling treedepth:" root visited edges))
  (let ((nextroot (aref edges root)))
     (if (or (eq root nextroot) (member nextroot visited))
	 1
	 (+ 1 (treedepth nextroot edges (append visited (list root nextroot)))))
    )
  )

;;; calculate depths of trees from a list of their roots
(defun treedepths (roots edges)
  (mapcar (lambda (root)
	    (treedepth root edges))
	  roots)
  )

;;; return the (first) index of the max element
(defun maxindex (list)
  (position (apply 'max list) list)
  )

; read the input and calculate the max index
;(let ((in (open "students.txt")))
  (dotimes (n (read))
    ;(print n)
    (setf students (list 0))
    (let ((numstudents (read)))
      (set 'edges (make-array (+ 1 numstudents))) 
      (dotimes (i numstudents)
	(setf students (append students (list (+ 1 i))))
	(setf (aref edges (read)) (read))
	)
      ;(print (list "students and edges" students edges))
      ;(print (treedepths students edges))
      (format t "Case ~a: ~a~%" (+ 1 n) (maxindex (treedepths students edges)))
      )
    )
;  (close in)
;  )
