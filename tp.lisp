(defun simfact (x)
	   (labels ((rect (n) 
		      (if (zerop n)
			  1
			  (* n (rect (- n 1))))))
	     (rect x)))
