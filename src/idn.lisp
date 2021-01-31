(in-package :idn)

(defgeneric basic-code-point-p (object))

(defmethod basic-code-point-p ((object character))
  (basic-code-point-p (char-code object)))

(defmethod basic-code-point-p ((object integer))
  (<= 0 object #x7f))

(defun digit->code-point (digit)
 (code-char (cond
              ((<= 0 digit 25)
               (+ 97 digit))
              ((<= 26 digit 35)
               (+ (- digit 26) 48))
              (t
               (error "digit overflow")))))

(defun div (a b)
  (floor (/ a b)))

(define-constant +base+                  36               :test #'=)

(define-constant +t-min+                  1               :test #'=)

(define-constant +t-max+                 26               :test #'=)

(define-constant +skew+                  38               :test #'=)

(define-constant +damp+                 700               :test #'=)

(define-constant +initial-bias+          72               :test #'=)

(define-constant +delimiter+             #\-              :test #'char=)

(define-constant +encoded-string-prefix+ "xn--"           :test #'string=)

(define-constant +delta-overflow+        (1- (expt 2 26)) :test #'=)

(define-constant +initial-n+             #x80             :test #'=)

(defun unicode->ascii (unicode-text)
  (let* ((n            +initial-n+)
         (delta        0)
         (bias         +initial-bias+)
         (basic-code-points (remove-if-not #'basic-code-point-p unicode-text))
         (h                 (length basic-code-points))
         (b                 (length basic-code-points))
         (output            (if (> b 0)
                                (text-utils:strcat basic-code-points (string +delimiter+))
                                basic-code-points)))
    (labels ((adapt (delta num-points first-time)
               (if first-time
                   (setf delta (div delta +damp+))
                   (setf delta (div delta 2)))
               (incf delta (div delta num-points))
               (let ((k 0))
                 (loop while (> delta
                                (div (* (- +base+ +t-min+)
                                      +t-max+)
                                   2))
                       do
                          (setf delta (div delta (- +base+ +t-min+)))
                          (incf k +base+))
                 (+ k
                    (div (* (+ (- +base+ +t-min+) 1)
                          delta)
                         (+ delta +skew+)))))
             (maybe-signal-overflow (delta)
               (when (> delta +delta-overflow+)
                 (error "overflow detected"))))
      (loop while (< h (length unicode-text)) do
        (let ((minimum-code-greater-than-n (num:find-min (remove-if (lambda (a) (< a n))
                                                                    (map 'list
                                                                         #'char-code
                                                                         unicode-text)))))
          (setf delta (+ delta (* (- minimum-code-greater-than-n n)
                                  (+ h 1))))
          (maybe-signal-overflow delta)
          (setf n minimum-code-greater-than-n)
          (loop for character across unicode-text do
            (let ((code-point (char-code character)))
              (cond
                ((< code-point n)
                 (incf delta)
                 (maybe-signal-overflow delta))
                 ((= code-point n)
                  (let ((q delta))
                    (loop named inner-loop for k from +base+ by +base+ do
                      (let ((tx (cond
                                  ((<= k bias)
                                   +t-min+)
                                  ((>= k (+ bias +t-max+))
                                   +t-max+)
                                  (t
                                   (- k bias)))))
                        (when (< q tx)
                          (return-from inner-loop))
                        (let ((new-char (digit->code-point (+ tx (rem (- q tx)
                                                                      (- +base+ tx))))))
                          (setf output (text-utils:strcat output (string new-char))))
                        (setf q (div (- q tx)
                                     (- +base+ tx)))))
                    (setf output (text-utils:strcat output (string (digit->code-point q))))
                    (setf bias (adapt delta (1+ h) (= h b)))
                    (setf delta 0)
                    (incf h))))))
          (incf delta)
          (incf n)))
      (cond
        ((text-utils:string-empty-p output)
         output)
        ((char= (alexandria:last-elt output) +delimiter+)
         (misc:all-but-last-elt output))
        (t
         (text-utils:strcat +encoded-string-prefix+ output))))))

(defun host-unicode->ascii (unicode-host)
  (if (find #\. unicode-host :test #'char=)
      (let ((splitted (cl-ppcre:split "\\." unicode-host)))
        (text-utils:join-with-strings (mapcar #'unicode->ascii splitted)
                                      "."))
      (unicode->ascii unicode-host)))
