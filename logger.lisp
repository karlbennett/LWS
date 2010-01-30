(defun logger (line)
  "This function can be used to send log messages to the standard output."
  (if (listp line) ; If the line proided is a list...
      (format t "LWS LOGGER:~{~10T~a~%~}" line) ; Print print log tag then an element per line.
      (format t "LWS LOGGER:~10T~A~%" line))) ; Else print log tag and the line.