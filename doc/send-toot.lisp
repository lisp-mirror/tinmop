;; a comment starts with a semicolon like that
(in-package :scripts) ; always starts a script with this line

;; defun means 'define a function'
(defun read-stdin ()
  ;; 'let' introduce a new variable, 'data' in this case
  (let ((data (loop ; read from standard and collect character in a list
                 for char = (read-char *standard-input* nil nil)
                 while char
                 collect char)))
    (coerce data 'string))) ; transform the list in a string

(defun main ()
  (when-let* ((body (read-stdin)))
    ;; the first element  of a list (the stuff between parents is the
    ;; function name the rest of the lists are the functions parameters.
    ;; nil means false or kind of 'empty'
    (send-status body nil nil nil +status-public-visibility+)))

;; call the function to send a toot
(main)
