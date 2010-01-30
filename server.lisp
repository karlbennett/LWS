(require :usocket)

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