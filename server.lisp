(require :usocket)

(defun start-server (port request-handler)
  (let ((socket (usocket:socket-listen "localhost" port)))
    (format t "~%Server Started - ~A~%~%" (usocket:get-local-name socket))
    (loop
       (let* ((accept (usocket:socket-accept socket)) (in (usocket:socket-stream accept)))
	 (funcall request-handler in)
	 (usocket:socket-close accept)))))

(defun http-request-handler (inputstream)
  (do* ((line (read-line inputstream nil nil) (read-line inputstream nil nil))
	(line-list (if (char= #\Return (elt line 0))
		       nil 
		       (list (string-trim '(#\Space #\Return) line)))
		   (if (char= #\Return (elt line 0))
		       line-list
		       (nconc line-list (list (string-trim '(#\Space #\Return) line))))))
       ((and (< 0 (length line-list)) (char= #\Return (elt line 0)))
	(format t "--Request Start---~%~{~a~%~}--Request Stop--~%~%" line-list)
	(format inputstream "HTTP/1.1 200 OK~%Content-Type: text/html; charset=ISO-8859-1")
	(format inputstream "Content-Type: text/html; charset=ISO-8859-1~%~%")
	(format inputstream "Hello World~%~%")
	(force-output inputstream)
	line-list)))

(start-server 80 #'http-request-handler)