(load "logger.lisp")
(load "server.lisp")
(load "request.lisp")
(load "response.lisp")

(start-server 80 #'http-request-handler)
