;;;;                Miscellaneous Useful Functions

(define (print . message)
  (write-line (apply string-append
		     (map string message))))