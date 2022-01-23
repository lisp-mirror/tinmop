(defpackage :kami
  (:use
   :cl
   :config
   :constants
   :mtree
   :filesystem-tree-window)
  (:local-nicknames (:9p :purgatory)
                    (:a  :alexandria))
  (:export
   :+kami-scheme+
   :generate-filesystem-window-handlers
   :iri->filesystem-window-handlers))
