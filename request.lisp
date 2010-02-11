(load "util.lisp")

;Constants
(defconstant +rootdir+ "http")

; Request Handling Functions

(defun http-request-method-not-imlemented (outputstream headers)
  "This function can be used to push a 501 http message down an output stream."
  (format outputstream "HTTP/1.1 501 Not Implemented~%")
  (format outputstream "Content-Type: text/html; charset=ISO-8859-1~%~%")
  (format outputstream "This method is not implemented.~%~%")
  (force-output outputstream)
  headers)

(defun http-request-bad-request (outputstream)
  "This function can be used to push a 400 http message down an output stream."
  (format outputstream "HTTP/1.1 400 Bad Request~%")
  (format outputstream "Content-Type: text/html; charset=ISO-8859-1~%~%")
  (format outputstream "Your client issued a bad request.~%~%")
  (force-output outputstream))

(defun http-request-get (outputstream headers)
  "This function can be used to handle an http GET request."
  ; Look for the requested file.
  (let ((f-string (file->text (concatenate 'string +rootdir+ (second (first headers))))))
    (if f-string ; If the file is found... 
	(progn
	  (format outputstream "HTTP/1.1 200 OK~%") ; Send an OK response...
	  (format outputstream "Content-Type: text/html; charset=ISO-8859-1~%~%")
	  (format outputstream "~A~%~%" f-string) ; ...along with the files contents.
	  (force-output outputstream))
	(progn
	  (format outputstream "HTTP/1.1 404 Not Found~%") ; Else send a Not Found response.
	  (format outputstream "Content-Type: text/html; charset=ISO-8859-1~%~%")
	  (format outputstream "Page Not Found~%~%")
	  (force-output outputstream))))
  headers) ; Lastly return the headers list.

(defun http-request-methods ()
  "This method returns a hashtable that has the valid http request methods as keys and
   there handler functions as values."
  (let ((methods (make-hash-table :size 8)))
    (setf (gethash 'OPTIONS methods) #'http-request-method-not-imlemented)
    (setf (gethash 'GET methods) #'http-request-get)
    (setf (gethash 'HEAD methods) #'http-request-method-not-imlemented)
    (setf (gethash 'POST methods) #'http-request-method-not-imlemented)
    (setf (gethash 'PUT methods) #'http-request-method-not-imlemented)
    (setf (gethash 'DELETE methods) #'http-request-method-not-imlemented)
    (setf (gethash 'TRACE methods) #'http-request-method-not-imlemented)
    (setf (gethash 'CONNECT methods) #'http-request-method-not-imlemented)
    methods))

(defun parse-header (header)
  "This function can be used to parse the header lines into an easy to use formmat."
  (list (split-string #\Space header)))

(defun retrieve-headers (inputstream)
  "This function can be used to retriee all the headers from an http request message."
  (do* ((line (ignore-ws inputstream) (read-line inputstream nil nil)) ; Read in a line and...
	;...check if it's empty then...
	(line-is-empty (char= #\Return (elt line 0)) (char= #\Return (elt line 0)))
	(header (clean-line line) (clean-line line)) ; ...clear it of special characters and...
	(line-list (parse-header header); ...lastly add it to a 2D array of headers that is...
		   (if line-is-empty
		       line-list
		       ; ...each header line broken up into a list of words.
		       (nconc line-list (parse-header header)))))
       ; When there are no more headers return the 2D header list.
       ((and (< 0 (length line-list)) line-is-empty) line-list)
    ; All the while loggin each header that is parsed.
    (logger (concatenate 'string "http-request-handler - retrieve-headers - " header))))

(defun http-request-handler (inputstream)
  "This function can be used to handle an http request."
  (logger "http-request-handler - Started") ; Log the start of the request handling.
  (let ((headers (retrieve-headers inputstream)) ; Retrieve all th headers and...
	(methods (http-request-methods))) ; ...generate the method hashtable.
    (handler-case
	; Run the method the has been requested in the http request method.
	(funcall (gethash (intern (first (first headers))) methods) inputstream headers)
      ; If the method doesn't exist return a bad request response message.
      (undefined-function () (http-request-bad-request inputstream))))
  (logger "http-request-handler - Finished")) ; Log the finished request handling.


;(defun http-method-options (uri http-version) "OPTIONS")
(defun http-method-get (uri http-version) (list "GET" uri http-version))
(defun http-method-head (uri http-version) (list "HEAD" uri http-version))
;(defun http-method-post (uri http-version) (list "POST" uri http-version))
;(defun http-method-put (uri http-version) (list "PUT" uri http-version))
;(defun http-method-delete (uri http-version) (list "DELETE" uri http-version))
;(defun http-method-trace (uri http-version) (list "TRACE" uri http-version))
;(defun http-method-connect (uri http-version) (list "CONNECT" uri http-version))
(defun http-method-undefined () "UNDEFINED")

(defmacro process-http-request ((method uri http-version))
  `(handler-case
      (,(intern (concatenate 'string "HTTP-METHOD-" method)) ,uri ,http-version)
    (undefined-function () (http-method-undefined))))