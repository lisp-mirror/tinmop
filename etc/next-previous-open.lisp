(in-package :modules)

(defun open-next ()
  "Go to the next message and open it"
  (ui:thread-go-down)
  (ui:thread-open-selected-message))

(defun open-previous ()
  "Go to the previous message and open it"
  (ui:thread-go-up)
  (ui:thread-open-selected-message))

(defun delete-and-move-next ()
  (ui:thread-mark-delete-selected-message)
  (ui:thread-open-selected-message))

(defun delete-and-move-previous ()
  (ui:thread-mark-delete-selected-message)
  (ui:thread-open-selected-message))

(define-key "right" #'open-next                *thread-keymap*)

(define-key "left"  #'open-previous            *thread-keymap*)

(define-key "M-d"   #'delete-and-move-next     *thread-keymap*)

(define-key "M-u"   #'delete-and-move-previous *thread-keymap*)
