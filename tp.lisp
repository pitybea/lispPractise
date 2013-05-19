(defun simfact (x)
	   (labels ((rect (n) 
		      (if (zerop n)
			  1
			  (* n (rect (- n 1))))))
	     (rect x)))

(defmacro nlet (name args &rest body)
	   `(labels ((,name ,(mapcar #'car args)
		       ,@body))
	      (,name ,@(mapcar #'cadr args))))

(defun nlet-fact (x)
	   (nlet fact ((n x))
		 (if (zerop n) 
		     1
		     (* n (fact (- n 1))))))

(defun flattern (lst)
	   (labels ((rect (x acc)
		      (cond
			((null x) acc)
			((atom (car x)) (cons (car x) (rect (cdr x) acc)))
			(t (rect (car x) (rect (cdr x) acc))))))
	    (rect lst nil)))
