(load "constants.lisp")

; Utility Functions
(defun split-string (d str)
  "This function can be used to split a string with any given delimiter."
  (loop for i = 0 then (1+ j) ; Start at index zero then move to...
     as j = (position d str :start i) ; ...index j which is the index of the first delimiter.
     collect (subseq str i j) while j)) ; Collect all the characters in between the indexes
					; as separate string as we go.

(defun file->text (f)
  "This function can be used to read in a file from the server."
  (handler-case ; Handle any conditions where the file name given may ot be correct.
      ; Open the file using the provided path appended to the end off the root directory.
      (with-open-file (in (concatenate 'string +rootdir+ f) :element-type 'character)
	; Create char array that is big enough to hold the whole file.
	(let ((digits (make-array (file-length in) :element-type 'character)))
	  (read-sequence digits in) ; Read the whole file into the char array.
	  digits)) ; Return the char array taht contains the file contents.
    (sb-int:simple-file-error () nil) ; If the file does not exist return nil.
    (sb-int:simple-stream-error ()nil))) ; If the path is to a directory return nil.

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