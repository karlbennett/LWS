;Constants
(defconstant +rootdir+ "http")

(defconstant +http-continue+ "HTTP/1.1 100 Continue")
(defconstant +http-switching-protocols+ "HTTP/1.1 101 Switching Protocols")
(defconstant +http-ok+ "HTTP/1.1 200 OK")
(defconstant +http-created+ "HTTP/1.1 201 Created")
(defconstant +http-accepted+ "HTTP/1.1 202 Accepted")
(defconstant +http-non-authoritative-information+ "HTTP/1.1 203 Non-Authoritative Information")
(defconstant +http-no-content+ "HTTP/1.1 204 No Content")
(defconstant +http-reset-content+ "HTTP/1.1 205 Reset Content")
(defconstant +http-partial-content+ "HTTP/1.1 206 Partial Content")
(defconstant +http-multiple-choices+ "HTTP/1.1 300 Multiple Choices")
(defconstant +http-moved-permanently+ "HTTP/1.1 301 Moved Permanently")
(defconstant +http-found+ "HTTP/1.1 302 Found")
(defconstant +http-see-other+ "HTTP/1.1 303 See Other")
(defconstant +http-not-modified+ "HTTP/1.1 304 Not Modified")
(defconstant +http-use-proxy+ "HTTP/1.1 305 Use Proxy")
(defconstant +http-temporary-redirect+ "HTTP/1.1 307 Temporary Redirect")
(defconstant +http-bad-request+ "HTTP/1.1 400 Bad Request")
(defconstant +http-unauthorized+ "HTTP/1.1 401 Unauthorized")
(defconstant +http-payment-required+ "HTTP/1.1 402 Payment Required")
(defconstant +http-forbidden+ "HTTP/1.1 403 Forbidden")
(defconstant +http-not-found+ "HTTP/1.1 404 Not Found")
(defconstant +http-method-not-allowed+ "HTTP/1.1 405 Method Not Allowed")
(defconstant +http-not-acceptable+ "HTTP/1.1 406 Not Acceptable")
(defconstant +http-proxy-authentication-required+ "HTTP/1.1 407 Proxy Authentication Required")
(defconstant +http-request-time-out+ "HTTP/1.1 408 Request Time-out")
(defconstant +http-conflict+ "HTTP/1.1 409 Conflict")
(defconstant +http-gone+ "HTTP/1.1 410 Gone")
(defconstant +http-length-required+ "HTTP/1.1 411 Length Required")
(defconstant +http-precondition-failed+ "HTTP/1.1 412 Precondition Failed")
(defconstant +http-request-entity-too-large+ "HTTP/1.1 413 Request Entity Too Large")
(defconstant +http-request-uri-too-large+ "HTTP/1.1 414 Request-URI Too Large")
(defconstant +http-unsupported-media-type+ "HTTP/1.1 415 Unsupported Media Type")
(defconstant +http-requested-range-not-satisfiable+ "HTTP/1.1 416 Requested range not satisfiable")
(defconstant +http-expectation-failed+ "HTTP/1.1 417 Expectation Failed")
(defconstant +http-internal-server-error+ "HTTP/1.1 500 Internal Server Error")
(defconstant +http-not-implemented+ "HTTP/1.1 501 Not Implemented")
(defconstant +http-bad-gateway+ "HTTP/1.1 502 Bad Gateway")
(defconstant +http-service-unavailable+ "HTTP/1.1 503 Service Unavailable")
(defconstant +http-gateway-time-out+ "HTTP/1.1 504 Gateway Time-out")
(defconstant +http-version-not-supported+ "HTTP/1.1 505 HTTP Version not supported")

(defconstant +header-content-type-text+ "Content-Type: text/html; charset=ISO-8859-1")