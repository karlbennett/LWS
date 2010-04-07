(load "util.lisp")
(load "globals.lisp")
(load "logger.lisp")

; Request Handling Functions
(defun parse-header (header)
  "This function can be used to parse the header lines into an easy to use formmat."
  (list (split-string #\Space header)))

(defun retrieve-headers (inputstream)
  "This function can be used to retrieve all the headers from an http request message."
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
    (logger (concatenate 'string " " header))))

(defun format-error-page (stream method page-uri default-page)
  (format stream "~A~%~A~%~%~A~%"
		method
		+header-content-type-text+
		(if page-uri
		    (file->text page-uri)
		    default-page)))

(defun http-request-handler (inputstream)
  "This function can be used to handle an http request."
  (logger "**** http-request-handler - Started") ; Log the start of the request handling.
  (let* ((headers (retrieve-headers inputstream)) ; Retrieve all the headers then...
	 ; using the first header generate the symbol for the http function that
	 ; has been requested.
	 (function-symbol (intern (concatenate 'string "HTTP-METHOD-" (first (first headers))))))
    (if (fboundp function-symbol) ; If the symbol matches a knowen function...
	; ...run the function and send it's output back down the input stream to the client.
	(format inputstream (funcall function-symbol (second (first headers))))
	(format-error-page inputstream 
			  +http-bad-request+ 
			  *bad-request-page-uri*
			  *bad-request-default-page*))); ...else send bad request page instead.
  (logger "**** http-request-handler - Finished")) ; Log the finished request handling.


;(defun http-method-options (uri http-version) "OPTIONS")
(defun http-method-get (uri)
  (let ((file-string (file->text uri)))
    (if file-string
	(format nil "~A~%~A~%~%~A~%"
		+http-ok+
		+header-content-type-text+
		file-string)
	(format-error-page nil
		+http-not-found+
		*not-found-page-uri*
		*not-found-default-page*))))
    
(defun http-method-head (uri http-version) (list "HEAD" uri http-version))
;(defun http-method-post (uri http-version) (list "POST" uri http-version))
;(defun http-method-put (uri http-version) (list "PUT" uri http-version))
;(defun http-method-delete (uri http-version) (list "DELETE" uri http-version))
;(defun http-method-trace (uri http-version) (list "TRACE" uri http-version))
;(defun http-method-connect (uri http-version) (list "CONNECT" uri http-version))
(defun http-method-undefined () "UNDEFINED")
