;; Sets R6RS options for GNU Guile.
(set! %load-extensions (append '(".guile.sls" ".sls") %load-extensions))
(read-enable 'r6rs-hex-escapes)
(read-enable 'hungry-eol-escapes)
