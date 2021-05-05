(uiop:define-package :ulet/core
    (:nicknames :ulet)
  (:use :common-lisp)
  (:export #:ulet))

(in-package :ulet/core)


(defmacro ulet (bindings &body body)
  (labels
      ((%symbol-starts-with-p (character)
	 (lambda (symbol) (eq character (char (symbol-name symbol) 0))))
       (%unified-binding-aux (bindings body)
	 (destructuring-bind (binding . bindings) bindings
	   (cond ((consp binding)
		  (cond ((symbolp (car binding))
			 (destructuring-bind (name . rest-binding) binding
			   (let ((generated-body (%unified-binding bindings body)))
			     (cond ((funcall (%symbol-starts-with-p #\%) name)
				    `(labels ((,name ,@rest-binding))
				       ,generated-body))
				   (t
				    `(let ((,name (progn ,@rest-binding)))
				       ,generated-body))))))
			((consp (car binding))
			 (destructuring-bind (destructuring . expressions) binding
			   (let ((generated-body (%unified-binding bindings body))
				 (expression `(progn ,@expressions)))
			     (cond ((every #'symbolp destructuring)
				    (cond ((some (%symbol-starts-with-p #\&) destructuring)
					   `(destructuring-bind ,destructuring ,expression
					      ,generated-body))
					  (t `(multiple-value-bind ,destructuring ,expression
						,generated-body))))
				   ((every (lambda (x)
					     (and (first x) (second x) (null (cddr x))))
					   destructuring)
				    `(with-slots ,destructuring ,expression
				       ,generated-body))
				   (t `(destructuring-bind ,destructuring ,expression
					 ,generated-body))))))
			(t `(let (,binding)
			      ,(%unified-binding bindings body)))))
		 (t `(let (,binding)
		       ,(%unified-binding bindings body))))))
       (%unified-binding (bindings body)
	 (if bindings
	     (%unified-binding-aux bindings body)
	     `(progn ,@body))))
    (%unified-binding bindings body)))
