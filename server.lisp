(require :usocket)

;Constants
(defconstant +rootdir+ "http")

; Utility Functions
(defun split-string (d str)
  "This function can be used to split a string with any given delimiter."
  (loop for i = 0 then (1+ j) ; Start at index zero then move to...
     as j = (position d str :start i) ; ...index j which is the index of the first delimiter.
     collect (subseq str i j) while j)) ; Collect all the characters in between the indexes
					; as separate string as we go.

(defun logger (line)
  "This function can be used to send log messages to the standard output."
  (if (listp line) ; If the line proided is a list...
      (format t "LWS LOGGER:~{~10T~a~%~}" line) ; Print print log tag then an element per line.
      (format t "LWS LOGGER:~10T~A~%" line))) ; Else print log tag and the line.

(defun get-file-text (f)
  "This function can be used to read in a file from the server."
  (handler-case ; Handle any conditions where the file name given may ot be correct.
      ; Open the file using the provided path on the end off the root directory.
      (with-open-file (in (concatenate 'string +rootdir+ f) :element-type 'character)
	; Create char array that is big enough to hold the whole file.
	(let ((digits (make-array (file-length in) :element-type 'character)))
	  (read-sequence digits in) ; Read the whole file into the char array.
	  digits)) ; Return the char array taht contains the file contents.
    (sb-int:simple-file-error () nil) ; If the file does not exist return nil.
    (sb-int:simple-stream-error ()nil))) ; If the path is to a directory return nil.

; Server Functions
(defun start-server (port request-handler)
  "This function can be used to start a server. The function must be provided with a request
   handler function that will then be run over any new input stream."
  ; Create a new socket on the provided port.
  (let ((socket (usocket:socket-listen "localhost" port)))
    (format t "~%Server Started - ~A~%~%" (usocket:get-local-name socket)) ; Log start message.
    (loop named server do ; Infinitely loop to catch all new connections.
	 (let* ((accept (usocket:socket-accept socket)) ; Listen for a new connection.
		; When a connection is found create an input stream.
		(in (usocket:socket-stream accept))) 
	   (funcall request-handler in) ; Run the provided request function over the input stream.
	   (usocket:socket-close accept))))) ; Close socket after the request function returns.

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
  (let ((f-string (get-file-text (second (first headers))))) ; Look for the requested file.
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

(defun ignore-ws (inputstream)
  "This function can be used to remove any empty lines from the start of an intput stream."
  (loop named ignore 
     for line = (read-line inputstream nil nil) ; Read every line...
     when (not (char= #\Return (elt line 0))) ; ...until there is some actual text.
     do (return-from ignore line))) ; Then return the first line of text.

(defun clean-line (line)
  "This function can be used to remove any spaces and carrage returns from the start and end
   of a string."
  (string-trim '(#\Space #\Return) line))

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

(start-server 80 #'http-request-handler)