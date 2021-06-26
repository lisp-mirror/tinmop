(in-package :9p-client)

(define-condition 9p-error (error)
  ((error-value
    :initarg :error-value
    :reader error-value)
   (message-type
    :initarg :message-type
    :reader message-type)
   (tag
    :initarg :tag
    :reader tag))
  (:report (lambda (condition stream)
             (format stream
                     "message-type ~a tag ~a: ~a"
                     (message-type condition)
                     (tag condition)
                     (error-value condition))))
  (:documentation "Error for 9p protocol"))


(define-condition 9p-initialization-error (error)
  ((tag
    :initarg :tag
    :reader tag)
   (rtag
    :initarg :rtag
    :reader rtag))
  (:report (lambda (condition stream)
             (format stream "error initialization tag sent ~a, got ~a instead"
                     (tag condition) (rtag condition))))
  (:documentation "Error for 9p protocol"))
