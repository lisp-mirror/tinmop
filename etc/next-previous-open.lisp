(in-package :modules)

(defun open-next ()
  "Go to the next message and open it"
  (ui:thread-go-down)
  (ui:thread-open-selected-message))

(defun open-previous ()
  "Go to the previous message and open it"
  (ui:thread-go-up)
  (ui:thread-open-selected-message))

(define-key "right" #'open-next     *thread-keymap*)

(define-key "left"  #'open-previous *thread-keymap*)
