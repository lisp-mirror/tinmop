;; tinmop: an humble mastodon client
;; Copyright (C) 2020  cage

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :config
  (:use :cl)
  (:export
   :+sys-data-dir+
   :+sys-conf-dir+
   :+catalog-dir+
   :+text-domain+
   :+program-name+
   :+program-version+
   :+issue-tracker+
   :+home-data-dir+
   :_
   :n_))

(defpackage :constants
  (:use :cl
        :alexandria
        :config)
  (:export
   :+help-about-message+
   :+http-code-ok+
   :+mime-type-jpg+
   :+mime-type-png+
   :+mime-type-html+
   :+db-file+
   :+fps+
   :+command-window-height+
   :+starting-init-file+
   :+box-height-diff+
   :+default-command-prompt+
   :+menu-button-ok+
   :+status-public-visibility+
   :+status-direct-visibility+
   :+folder-direct-message-prefix+
   :+folder-tag-prefix+
   :+mention-prefix+))

(defpackage :conditions
  (:use :cl
        :config)
  (:export
   :text-error
   :text
   :not-implemented-error
   :null-reference
   :out-of-bounds
   :length-error
   :different-length-error
   :command-not-found
   :column-not-found
   :with-default-on-error))

(defpackage :num-utils
  (:use :cl
        :constants)
  (:nicknames :num)
  (:export
   :safe-parse-number
   :parse-number-default
   :find-min-max
   :find-min
   :find-max
   :round-all
   :fract
   :sign
   :count-digit
   :fnv-hash-32
   :string-fnv-hash-32
   :fnv-hash-256
   :string-fnv-hash-256
   :*lcg-seed*
   :lcg-set-seed
   :lcg-next
   :lcg-next01
   :lcg-next-upto
   :lcg-next-in-range
   :lcg-next-in-range*
   :shellsort
   :multisort
   :multisort*
   :gen-multisort-test
   :*default-epsilon*
   :with-epsilon
   :add-epsilon-rel
   :epsilon=
   :epsilon<=
   :epsilon>=
   :binary-search))

(defpackage :misc-utils
  (:use :cl
        :constants)
  (:nicknames :misc)
  (:export
   :when-debug
   :debug-log
   :dbg
   :dbg-and-quit
   :dbg-stdout
   :code->char
   :char->code
   :swap
   :dump-hash-table
   :with-messages-start-end
   :safe-random
   :split-into-sublist
   :group-by
   :delete@
   :return-whole
   :safe-delete@
   :safe-all-but-last-elt
   :remove-compact-remap-sequence
   :remove-if-null
   :remove-if-not-null
   :intersperse
   :do-while
   :do-while*
   :not-null-p
   :permutation
   :shuffle
   :make-fresh-list
   :seq->list
   :lcat
   :vcat
   :fresh-list-insert@
   :fresh-list-subst@
   :make-array-frame
   :make-fresh-array
   :sequence->list
   :vector-empty-p
   :sequence-empty-p
   :safe-elt
   :safe-last-elt
   :safe-subseq
   :random-num-filled-vector
   :random-elt
   :safe-random-elt
   :copy-multiply
   :all-but-last-elt
   :list->array
   :list->simple-array
   :copy-list-into-array
   :array-slice
   :2byte->word
   :2word->int
   :byte->int
   :bytes->string
   :read-ieee-float-32
   :int16->bytes
   :int32->bytes
   :define-offset-size
   :define-parse-header-chunk
   :read-list
   :read-array
   :definline
   :+cache-invalid-value+
   :defcached
   :defcached-list
   :function-name
   :fn-delay
   :unsplice
   :defalias
   :defun-inline-function
   :format-fn-symbol
   :check-body-keywords
   :format-keyword
   :a->function
   :gen-type-p
   :define-compiler-macros
   :defmethod-inline-function
   :nest-expressions
   :replace-e!
   :+nil-equiv-bag+
   :build-plist
   :build-assocs-chain
   :recursive-assoc
   :recursive-assoc-just-before
   :n-setf-path-value
   :plist-path-value
   :gen-trivial-plist-predicates
   :gen-trivial-plist-predicate
   :gen-trivial-plist-get
   :gen-trivial-plist-gets
   :gen-vec-comp
   :make-null-pointer
   :with-load-forms-in-var
   :time-unix->universal
   :time-second-of
   :time-minutes-of
   :time-hour-of
   :time-date-of
   :time-month-of
   :time-year-of
   :time-day-of
   :time-daylight-p-of
   :time-zone-of
   :year->timestamp
   :current-year
   :extract-year-from-timestamp
   :format-time
   :binary-search
   :defun-w-lock
   :with-lock
   :get-url-content))

(defpackage :box
  (:use
   :cl)
  (:export
   :box
   :boxp
   :unbox
   :dbox
   :dboxp
   :dunbox))

(defpackage :filesystem-utils
  (:use
   :cl
   :alexandria)
  (:nicknames :fs)
  (:export
   :+file-path-regex+
   :+s-irwxu+
   :+s-irusr+
   :+s-iwusr+
   :+s-ixusr+
   :+s-irwxg+
   :+s-irgrp+
   :+s-iwgrp+
   :+s-ixgrp+
   :+s-irwxo+
   :+s-iroth+
   :+s-iwoth+
   :+s-ixoth+
   :+s-isuid+
   :+s-isgid+
   :*directory-sep-regexp*
   :*directory-sep*
   :copy-a-file
   :file-size
   :slurp-file
   :dump-sequence-to-file
   :create-file
   :cat-parent-dir
   :has-extension
   :get-extension
   :strip-extension
   :add-extension
   :do-directory
   :search-matching-file
   :split-path-elements
   :path-last-element
   :path-first-element
   :path-to-hidden-file-p
   :parent-dir-path
   :strip-dirs-from-path
   :get-stat-mtime
   :get-stat-ctime
   :get-stat-atime
   :file-outdated-p
   :file-exists-p
   :directory-exists-p
   :file-length-if-exists
   :delete-file-if-exists
   :file-hash
   :home-dir
   :temporary-filename
   :with-anaphoric-temp-file
   :temp-file
   :file-can-write-p
   :set-file-permissions
   :cached-directory-files
   :directory-files
   :make-directory
   :package-path
   :file-in-package
   :link-file-path
   :file-is-link-if-else
   :pathname->namestring
   :namestring->pathname
   :read-single-form
   :eq-filename))

(defpackage :os-utils
  (:use
   :cl
   :config
   :constants)
  (:export
   :cpu-number
   :xdg-open
   :getenv
   :default-temp-dir
   :open-with-editor
   :exit-program
   :user-cache-dir
   :cached-file-path))

(defpackage :text-utils
  (:use
   :cl
   :config)
  (:import-from :misc :definline)
  (:export
   :+float-regexp+
   :+integer-regexp+
   :uchar-length
   :utf8-encoded-p
   :clean-unprintable-chars
   :to-s
   :strcat
   :strcat*
   :join-with-strings
   :join-with-strings*
   :split-words
   :split-lines
   :strip-prefix
   :strip-withespaces
   :common-prefix
   :basename
   :wrap-with
   :build-string
   :right-padding
   :right-padding-suffix
   :left-padding
   :left-padding-prefix
   :ellipsize
   :justify-monospaced-text
   :flush-left-mono-text
   :string-empty-p
   :string-not-empty-p
   :string-starts-with-p
   :find-max-line-length
   :box-fit-single-column
   :box-fit-multiple-column
   :annotated-text-symbol
   :annotated-text-value
   :box-fit-multiple-column-annotated
   :collect-links))

(defpackage :html-utils
  (:use
   :cl
   :alexandria
   :config
   :text-utils)
  (:export
   :tag 
   :attributes 
   :attribute-key 
   :attribute-value 
   :children 
   :tag= 
   :find-attribute 
   :html->text))

(defpackage :resources-utils
  (:use :cl
        :cl-ppcre
        :config
        :constants
        :filesystem-utils
        :text-utils)
  (:nicknames :res)
  (:export
   :init
   :home-datadir
   :home-confdir
   :return-home-filename
   :return-system-filename
   :get-config-file
   :get-sys-config-file
   :get-data-file))

(defpackage :crypto-utils
  (:use :cl
        :alexandria
        :cl-ppcre
        :config
        :constants)
  (:export
   :crypto-text-p
   :generate-key
   :encrypt-message
   :decrypt-message))

(defpackage :interfaces
  (:use :cl
        :alexandria
        :constants
        :misc)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :clone
   :clone-into
   :copy-flat
   :copy-flat-into
   :with-simple-clone
   :with-simple-copy-flat
   :serialize
   :serialize-to-stream
   :deserialize
   :post-deserialization-fix))

(defpackage :mtree-utils
  (:use
   :cl
   :interfaces
   :text-utils)
  (:nicknames :mtree)
  (:export
   :random-choose-leaf
   :traverse-apply-tree
   :traverse-napply-tree
   :traverse-find-if-tree
   :traverse-find-all-if-tree
   :traverse-apply-tree-cdr
   :traverse-nadd-child
   :traverse-ndelete-child
   :nappend-child
   :navigate
   :m-tree
   :*use-pprint-tree*
   :pprint-tree
   :children
   :parent
   :data
   :add-child
   :add-children
   :add-children*
   :child-data-pushnew
   :graft-branch
   :make-node
   :find-node
   :leafp
   :rootp
   :top-down-visit
   :bottom-up-visit
   :remove-all-children
   :remove-child
   :remove-child-if
   :do-children
   :do-children-from-end
   :find-child
   :find-child-if
   :count-leaves
   :count-nodes
   :mtree-equal
   :root-node
   :sorted-m-tree
   :tree->text-lines
   :tree->annotated-lines))

(defpackage :bs-tree
  (:use
   :cl
   :interfaces)
  (:shadow :search :map)
  (:export
   :node
   :parent
   :data
   :left
   :right
   :make-node
   :make-leaf
   :make-root-node
   :%key
   :+data+
   :+left+
   :+right+
   :+parent+
   :node->string
   :search
   :search-opt
   :with-insert-local-function
   :insert
   :leafp
   :all-children-leaf-p
   :map
   :map-node
   :walk
   :bstp
   :node->dot
   :reconstruct-parent
   :find-max-node
   :to-sexp
   :from-sexp))

(defpackage :rb-tree
  (:use
   :cl
   :interfaces
   :bs-tree)
  (:shadowing-import-from :bs-tree :search :map)
  (:export
   :+rb-red+
   :+rb-black+
   :+rb-color+
   :rb-node
   :color
   :make-rb-node
   :make-rb-leaf
   :make-root-rb-node
   :data
   :left
   :right
   :node->string
   :search
   :search-opt
   :with-insert-local-function
   :insert
   :remove-node
   :leafp
   :map
   :map-node
   :balancedp
   :walk
   :bstp
   :node->dot
   :reconstruct-parent
   :to-sexp
   :from-sexp))

(defpackage :priority-queue
  (:use :cl)
  (:nicknames :pq)
  (:export
   :priority-queue
   :key-function
   :compare-function
   :equal-function
   :push-element
   :pop-element
   :find-element
   :remove-element
   :count-elements-if
   :emptyp
   :with-min-queue))

(defpackage :queue
  (:use :cl)
  (:nicknames :qu)
  (:shadow :push :pop :find)
  (:export
   :*equal-function*
   :*key-function*
   :push
   :pop
   :find
   :emptyp
   :with-queue
   :simple-queue
   :container
   :q-pop
   :q-peek
   :q-push
   :q-empty-p
   :q-size
   :q-sort
   :q-dbg-print))

(defpackage :stack
  (:use
   :cl
   :alexandria)
  (:export
   :stack
   :stack-push
   :stack-pop
   :stack-find
   :stack-empty-p
   :stack-remove
   :stack-position
   :stack-raise-to-top
   :stack-empty-p
   :do-stack-element))

(defpackage :gemini-parser
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc
   :alexandria)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :parse))

(defpackage :db-utils
    (:use
     :cl
     :alexandria
     :cl-ppcre
     :sxql
     :local-time
     :config
     :constants
     :text-utils)
    (:export
     :+characters-trouble-name+
     :*connection*
     :fetch
     :fetch-all
     :close-db
     :connectedp
     :with-db-transaction
     :db-path
     :quote-symbol
     :init-connection
     :with-ready-database
     :with-disabled-foreign
     :do-rows
     :prepare-for-sql-like
     :object-exists-in-db-p
     :object-count-in-db
     :query-low-level
     :db-nil-p
     :db-not-nil-p
     :db-getf
     :db-nil->lisp
     :if-db-nil-else
     :count-all
     :query
     :query->sql
     :local-time-obj-now
     :decode-date-string
     :decode-datetime-string
     :encode-datetime-string
     :encoded-datetime-year
     :make-insert
     :make-delete
     :make-update
     :get-max-id
     :get-min-id
     :decode-blob
     :rows->tsv
     :table-exists-p
     :prepare-for-db
     :last-inserted-rowid))

(defpackage :db
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :sxql
   :sxql-composer
   :config
   :constants
   :db-utils
   :text-utils)
  (:export
   :+db-true+
   :+db-false+
   :+other-id+
   :+table-account+
   :+table-status+
   :+table-attachment+
   :+table-saved-status+
   :+table-crypto-data+
   :+federated-timeline+
   :+local-timeline+
   :+home-timeline+
   :+direct-timeline+
   :+default-status-folder+
   :+mentions-status-folder+
   :+default-tag-timeline+
   :+default-converation-timeline+
   :+message-index-start+
   :+tag-separator+
   :hidden-recipient-p
   :default-timelines
   :message-index->sequence-index
   :timeline-type->description
   :maybe-build-all-tables
   :build-all-indices
   :delete-all-tables
   :delete-all-views
   :delete-database
   :fetch-all-rows
   :table->alist
   :fetch-from-id
   :fetch-single
   :delete-by-id
   :account-ignored-p
   :user-ignored-p
   :acct->user
   :acct->id
   :username->id
   :user-exists-p
   :user-id->user
   :user-id->username
   :insert-in-history
   :previous-in-history
   :next-in-history
   :most-recent-history-id
   :purge-history
   :all-poll-options
   :find-poll
   :find-poll-option
   :find-poll-bound-to-status
   :update-db
   :message-root
   :message-children
   :message-root->tree
   :message-id->tree
   :message-from-timeline-folder-message-index
   :message-index->tree
   :find-status-id
   :find-message-id
   :data-id
   :row-id
   :row-message-visibility
   :row-message-status-id
   :row-message-index
   :row-message-timeline
   :row-message-folder
   :row-message-username
   :row-message-user-display-name
   :row-message-content
   :row-message-rendered-text
   :row-message-creation-time
   :row-message-subject
   :row-message-tags
   :row-message-reblog-id
   :row-lockedp
   :row-user-username
   :row-tag-got-new-message
   :row-conversation-folder
   :row-conversation-root-status-id
   :row-conversation-ignored-p
   :row-poll-expired-p
   :row-poll-multiple-vote-p
   :row-title
   :row-expire-date
   :row-votes-count
   :row-message-reply-to-id
   :next-status-tree
   :previous-status-tree
   :message-tree-root-equal
   :annotated-tree-line->data-plist
   :renumber-timeline-message-index
   :renumber-all-timelines
   :all-attachments-to-status
   :all-attachments-urls-to-status
   :mark-status-red-p
   :mark-status-unread
   :mark-status-deleted-p
   :mark-status-prevent-deletion
   :count-status-redp
   :count-status
   :search-message-text-body
   :search-next-message-body
   :search-previous-message-body
   :search-next-message-meta
   :search-previous-message-meta
   :search-next-unread-message
   :last-message-index-status
   :last-status-id-timeline-folder
   :first-status-id-timeline-folder
   :last-ignored-status-id-timeline-folder
   :first-ignored-status-id-timeline-folder
   :last-pagination-status-id-timeline-folder
   :first-pagination-status-id-timeline-folder
   :find-pagination-status
   :add-to-pagination-status
   :remove-pagination-status
   :count-status-marked-to-delete
   :delete-all-statuses-marked-deleted
   :tags-histogram-foreground
   :max-username-length
   :keyword->dbcolumn
   :folder-exists-p
   :conversation-root-captured-p
   :timeline-exists-p
   :move-message-to-folder
   :move-tree-to-folder
   :last-status-id-in-tree
   :all-timelines-in-folder
   :all-folders
   :all-timelines
   :ignore-status-author
   :unignore-status-author
   :unignore-author
   :all-usernames
   :all-followed-usernames
   :all-unfollowed-usernames
   :all-ignored-usernames
   :status-ignored-p
   :status-skipped-p
   :add-to-status-ignored
   :add-to-status-skipped
   :add-to-followers
   :remove-from-followers
   :forget-all-statuses-marked-deleted
   :status-id->username
   :subscribe-to-tag
   :unsubscribe-to-tag
   :tag->folder-name
   :folder-name->tag
   :all-tag-paginations-status
   :all-subscribed-tags
   :all-subscribed-tags-name
   :tag-folder-name-p
   :more-recent-tag-fetched-p
   :all-tags-with-new-message-fetched
   :update-last-seen-status-subscribed-tag
   :tag-histogram
   :mark-tag-got-new-messages
   :unmark-tag-got-new-messages
   :max-id-conversations
   :all-conversations
   :all-conversations-id
   :add-conversation
   :conversation-id->folder
   :all-conversation-folders
   :update-conversation-folder
   :update-conversation-folder-by-id
   :update-conversation-by-id
   :conversation-folder-exists-p
   :change-conversation-name
   :conversation-stats
   :conversation-id
   :messages-red
   :messages-to-read
   :conversation-name
   :conversation-read/red
   :all-conversation-stats
   :ignore-conversation
   :delete-conversation
   :import-crypto-data
   :crypto-user-key
   :cache-invalidate
   :cache-put
   :cache-get
   :cache-get-value
   :cache-expired-p))

(defpackage :date-formatter
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :misc
   :text-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :expand-date-formatter-spec))

(defpackage :emoji-shortcodes
  (:use :cl)
  (:export
   :shortcode-lookup
   :emojify))

(defpackage :software-configuration
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc
   :mtree)
  (:nicknames :swconf)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :+conf-filename+
   :+shared-conf-filename+
   :+key-background+
   :+key-foreground+
   :+key-width+
   :+key-root+
   :+key-height+
   :+key-value+
   :+key-new-message-mark+
   :+key-window+
   :+key-focus+
   :+key-mark+
   :+key-vote-vertical-bar+
   :+key-info-dialog+
   :+key-help-dialog+
   :+key-error-dialog+
   :+key-input-dialog+
   :+key-notify-window+
   :+key-notification-life+
   :+key-modeline+
   :+key-date-format+
   :+key-main-window+
   :+key-thread-window+
   :+key-message-window+
   :+key-favourite+
   :+key-sensitive+
   :+key-boosted+
   :+key-tags-window+
   :+key-open-attach-window+
   :+key-open-message-link-window+
   :+key-conversations-window+
   :+key-keybindings-window+
   :+key-suggestions-window+
   :+key-command-window+
   :+key-editor+
   :+key-username+
   :+key-server+
   :+key-message+
   :+key-selected+
   :+key-deleted+
   :+key-read+
   :+key-unread+
   :+key-color-re+
   :+key-tree+
   :+key-branch+
   :+key-arrow+
   :+key-data+
   :+key-data-leaf+
   :+key-purge-history-days-offset+
   :*allowed-status-visibility*
   :*allowed-attachment-type*
   :*software-configuration*
   :color-re-assign
   :re
   :color-name
   :color-value
   :attributes
   :parse
   :load-config-file
   :external-editor
   :vote-vertical-bar
   :crypted-mark-value
   :quick-help-header-colors
   :window-titles-ends
   :tags-histogram-foreground
   :tags-new-message-mark
   :conversation-window-read-colors
   :conversation-window-unread-colors
   :max-message-length
   :max-report-comment-length
   :quote-char
   :max-attachments-allowed
   :color-regexps
   :ignore-users-regexps
   :win-bg
   :win-fg
   :win-height
   :win-width
   :command-separator-config-values
   :command-error-message-colors
   :command-info-message-colors
   :tree-config-colors
   :tree-config-rendering-values
   :make-tree-colormap
   :config-purge-history-days-offset
   :config-purge-cage-days-offset
   :config-notification-life
   :config-server-name
   :config-username
   :config-win-focus-mark
   :thread-message-symbol
   :thread-message-read-colors
   :thread-message-unread-colors
   :thread-message-selected-colors
   :thread-message-deleted-colors
   :modeline-colors
   :modeline-fmt
   :date-fmt
   :message-window-locked-account-mark
   :message-window-unlocked-account-mark
   :message-window-account-locking-status-mark
   :message-window-line-mark-values
   :message-window-attachments-header
   :form-style
   :background
   :foreground
   :input-background
   :input-foreground
   :selected-background
   :selected-foreground))

(defpackage :tui-utils
    (:use
     :cl
     :alexandria
     :cl-ppcre
     :local-time
     :croatoan
     :config
     :constants
     :interfaces
     :text-utils)
    (:nicknames :tui)
    (:import-from :misc-utils :defalias)
    (:export
     :make-background
     :make-croatoan-window
     :make-blocking-croatoan-window
     :make-screen
     :make-tui-char
     :make-tui-string
     :tui-format
     :decode-key-event
     :colorize-tree-element
     :colorize-tree-line
     :text-width
     :text-slice
     :find-max-line-width
     :ncat-complex-string
     :cat-complex-string
     :complex-char->char
     :cat-tui-string
     :tui-string->chars-string
     :text-ellipsize
     :right-pad-text
     :text->tui-attribute
     :assemble-attributes
     :attribute-reverse
     :attribute-bold
     :attribute-underline
     :attribute-italic
     :attribute-blink
     :attribute-dim
     :attribute-invisible
     :combine-attributes
     :colorize-line
     :colorized-line->tui-string
     :with-notify-errors))

(defpackage :command-line
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :config
   :constants
   :text-utils
   :misc-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :*start-folder*
   :*start-timeline*
   :*update-timeline*
   :*script-file*
   :*check-follow-requests*
   :*reset-timeline-pagination*
   :*notify-mentions*
   :*update-timeline-climb-message-tree*
   :manage-opts))

(defpackage :specials
  (:use
   :cl
   :config
   :constants)
  (:export
   :*main-window*
   :*keybindings-suggestions-window*
   :*strings-suggestions-window*
   :*command-window*
   :*thread-window*
   :*message-window*
   :*send-message-window*
   :*follow-requests-window*
   :*tags-window*
   :*conversations-window*
   :*open-attach-window*
   :*open-message-link-window*))

(defpackage :complete
  (:use
   :cl
   :alexandria
   :osicat)
  (:export
   :*complete-function*
   :shortest-candidate
   :directory-complete
   :folder-complete
   :timeline-complete-fn
   :ignored-username-complete
   :username-complete
   :visibility-complete
   :unfollowed-user-complete
   :followed-user-complete
   :tags-complete
   :conversation-folder))

(defpackage :program-events
  (:use
   :cl
   :alexandria
   :priority-queue
   :config
   :constants
   :priority-queue
   :misc
   :box)
  (:shadowing-import-from :misc           :random-elt :shuffle)
  (:shadowing-import-from :priority-queue :emptyp)
  (:export
   :+standard-event-priority+
   :+minimum-event-priority+
   :+maximum-event-priority+
   :*process-events-immediately*
   :program-event
   :event-id
   :payload
   :condition-variable
   :priority
   :reinitialize-id
   :events-queue
   :lock
   :push-event
   :event-available-p
   :pop-event
   :remove-event
   :find-event
   :ask-user-input-string-event
   :user-input-string-event
   :notify-user-event
   :remove-notify-user-event
   :save-timeline-in-db-event
   :timeline-type
   :localp
   :min-id
   :fetch-remote-status-event
   :process-event
   :search-regex-message-content-event
   :thread-goto-message
   :thread-search-message-body-event
   :search-direction
   :thread-search-message-meta-event
   :delete-all-status-event
   :quit-program-event
   :error-message-event
   :info-message-event
   :error-dialog-event
   :info-dialog-event
   :move-selected-tree-event
   :refresh-thread-windows-event
   :favourite-status-event
   :unfavourite-status-event
   :reblog-status-event
   :unreblog-status-event
   :unignore-user-event
   :send-message-change-subject-event
   :send-message-change-visibility-event
   :open-send-message-window-event
   :send-message-add-attachment-event
   :send-message-event
   :use-ui-notification
   :follow-user-event
   :unfollow-user-event
   :open-follow-requests-window-event
   :subscribe-tags-event
   :unsubscribe-tags-event
   :update-last-refresh-subscribe-tags-event
   :notify-fetched-new-tag-messages-event
   :tag-mark-got-messages-event
   :refresh-tag-window-event
   :update-conversations-event
   :change-conversation-name-event
   :old-name
   :new-name
   :refresh-conversations-window-event
   :ignore-conversations-event
   :delete-conversations-event
   :update-mentions-event
   :expand-thread-event
   :report-status-event
   :add-crypto-data-event
   :poll-vote-event
   :function-event
   :dispatch-program-events
   :add-pagination-status-event
   :status-id
   :timeline))

(defpackage :api-pleroma
  (:use
   :cl
   :alexandria
   :config
   :constants)
  (:export
   :delete-notification))

(defpackage :api-client
  (:use
   :cl
   :alexandria
   :config
   :constants
   :db-utils
   :interfaces
   :text-utils
   :misc)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:nicknames :client)
  (:export
   :*client*
   :forget-credentials
   :authorize
   :favourite-status
   :unfavourite-status
   :reblog-status
   :unreblog-status
   :get-timeline
   :update-timeline
   :tag->folder-name
   :tag-name
   :update-subscribed-tags
   :fetch-remote-status
   :get-remote-status
   :send-status
   :get-status-context
   :follow-user
   :unfollow-user
   :follow-requests
   :accept-follow-request
   :reject-follow-request
   :conversation-tree
   :id
   :last-status
   :status-tree
   :root
   :conversation-root-id
   :conversations
   :expand-conversations-tree
   :make-report
   :delete-conversation
   :get-activity
   :application-credentials
   :bookmarks
   :bookmark
   :unbookmark
   :polls
   :poll-vote
   :get-notifications
   :delete-notification
   :all-mentions
   :update-mentions-folder
   :expand-status-thread
   :make-placeholder-tag-histogram
   :init))

(defpackage :hooks
  (:use
   :cl
   :alexandria)
  (:export
   :*hook*
   :add-hook
   :remove-hook
   :run-hooks
   :run-hook
   :run-hook
   :run-hook-until-failure
   :run-hook-until-success
   :*before-main-loop*
   :*before-quit*
   :*before-prepare-for-rendering-message*
   :*before-sending-message*))

(defpackage :keybindings
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :text-utils
   :misc
   :mtree)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :*global-keymap*
   :*thread-keymap*
   :*tags-keymap*
   :*conversations-keymap*
   :*message-keymap*
   :*send-message-keymap*
   :*follow-requests-keymap*
   :*open-attach-keymap*
   :*open-message-link-keymap*
   :define-key
   :init-keyboard-mapping
   :find-keymap-node
   :humanize-key
   :help-fields-get-function
   :help-fields-get-text
   :print-help))

(defpackage :windows
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :stack
   :text-utils
   :misc
   :mtree
   :specials
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:shadowing-import-from :stack :stack :stack-push :stack-pop :stack-empty-p)
  (:export
   :key-config-holder
   :key-config
   :tree-holder
   :tree-color-map
   :render-arrow-value
   :render-leaf-value
   :render-branch-value
   :render-spacer-value
   :render-vertical-line-value
   :refresh-config-color-map
   :wrapper-window
   :croatoan-window
   :with-croatoan-window
   :when-window-shown
   :keybindings
   :win-clear
   :win-width
   :win-height
   :win-x
   :win-y
   :win-box
   :win-bgcolor
   :win-fgcolor
   :win-refresh
   :win-close
   :win-raise-to-top
   :win-move-cursor
   :win-move-cursor-direction
   :win-move
   :win-resize
   :win-show
   :win-hide
   :win-shown-p
   :menu-select
   :win-width-no-border
   :win-height-no-border
   :with-window-width
   :with-window-height
   :with-window-sizes
   :calc-center-on-window-width
   :calc-bottom-of-window-height
   :print-text
   :refresh-config
   :refresh-config-colors
   :refresh-config-sizes
   :calculate
   :draw
   :draw-all
   :refresh-config-all
   :calculate-all
   :cursor-show
   :cursor-hide
   :add-flush-left-text
   :make-blocking-message-dialog
   :make-error-message-dialog
   :make-info-message-dialog
   :make-checklist-dialog
   :make-input-dialog
   :focus-marked-window
   :in-focus
   :in-focus-p
   :border-window
   :uses-border-p
   :title-window))

(defpackage :notify-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :notify-window
   :pending
   :notify-window-p
   :draw-pending
   :make-notification-window))

(defpackage :suggestions-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :specials
   :windows
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :suggestions-window
   :paginated-info
   :current-page
   :update-suggestions
   :draw-pagination-info))

(defpackage :complete-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :tui-utils
   :specials
   :windows
   :suggestions-window)

  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :complete-window
   :init))

(defpackage :keybindings-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :keybindings
   :specials
   :windows
   :suggestions-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :keybindings-window
   :keybindings-tree
   :update-keybindings-tree
   :init))

(defpackage :point-tracker
  (:use
   :cl
   :alexandria
   :config
   :constants
   :misc
   :text-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :point-tracker
   :point-position
   :point-fg
   :point-bg
   :prompt
   :no-prompt-point-pos
   :move-point-left
   :move-point-right
   :move-point
   :move-point-to-end
   :move-point-to-start
   :insert-at-point
   :delete-at-point))

(defpackage :modeline-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :esrap
   :config
   :constants
   :misc
   :text-utils
   :windows
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :modeline-window
   :mapping-code->fn
   :modeline-src
   :modeline-text
   :modeline-fg
   :modeline-bg
   :expand-modeline-spec
   :refresh-modeline-config
   :add-modeline-char-expander))

(defpackage :line-oriented-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :keybindings
   :specials
   :windows
   :modeline-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:import-from :keybindings-window :update-keybindings-tree)
  (:export
   :line
   :selected-fg
   :selected-bg
   :normal-fg
   :normal-bg
   :normal-text
   :selected-text
   :deleted-text
   :fields
   :index
   :selected
   :selectedp
   :row-oriented-widget
   :single-row-height
   :top-row-padding
   :rows
   :row-selected-index
   :y-current-row
   :renderizable-rows-data
   :unselect-all
   :select-row
   :selected-row
   :ignore-selecting-action
   :selected-row-fields
   :selected-row-delete
   :row-move
   :simple-line-navigation-window
   :selected-line-bg
   :selected-line-fg
   :resync-rows-db
   :make-blocking-list-dialog-window))

(defpackage :message-rendering-utils
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc-utils
   :db-utils
   :db)
  (:nicknames :msg-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :add-mention-prefix
   :strip-mention-prefix
   :crypto-message-destination-user
   :maybe-crypt-message
   :attachment-type->description
   :attachment-type->metadata
   :status-attachments->text
   :message-original->text-body
   :message-original->text-header
   :poll->text))

(defpackage :thread-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :keybindings
   :specials
   :windows
   :modeline-window
   :line-oriented-window
   :tui-utils
   :message-rendering-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:import-from :keybindings-window :update-keybindings-tree)
  (:export
   :message-line
   :thread-window
   :timeline-type
   :timeline-folder
   :grow-tree-to-fit-window
   :fit-timeline-to-window
   :go-message-down
   :go-message-up
   :search-next-message-body
   :search-previous-message-body
   :search-next-message-meta
   :search-previous-message-meta
   :search-next-unread
   :goto-message
   :goto-first-message
   :goto-last-message
   :open-message
   :mark-selected-message-to-delete
   :mark-selected-message-prevent-delete
   :init))

(defpackage :message-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :keybindings
   :specials
   :windows
   :modeline-window
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :message-window
   :source-text
   :scroll-down
   :scroll-up
   :scroll-end
   :scroll-begin
   :scroll-next-page
   :scroll-previous-page
   :search-regex
   :init))

(defpackage :open-attach-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :open-attach-window
   :status-id
   :refresh-view-links-window-config
   :resync-rows-db
   :open-attachment
   :init))

(defpackage :open-message-link-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :open-message-link-window
   :open-message-link
   :init))

(defpackage :command-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :keybindings
   :specials
   :windows
   :point-tracker
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:import-from :keybindings-window :update-keybindings-tree)
  (:export
   :print-error
   :command-window
   :command-line
   :event-to-answer
   :prompt
   :add-error-message
   :add-info-message
   :remove-messages
   :set-history-most-recent
   :manage-event
   :set-keybinding-mode
   :set-string-mode
   :init))

(defpackage :sending-message
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :keybindings
   :specials
   :windows
   :modeline-window
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :message-ready-to-send
   :subject
   :attachments
   :reply-to
   :visibility
   :body
   :confirm-sending-window
   :message-data
   :init))

(defpackage :follow-requests
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :follow-requests-window
   :init
   :process-requests))

(defpackage :tags-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :tags-window
   :resync-rows-db
   :init))

(defpackage :conversations-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :line-oriented-window
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :conversations-window
   :resync-rows-db
   :init))

(defpackage :main-window
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :tui-utils)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :focused-window
   :focused-keybindings
   :init
   :parse-subwin-w
   :parse-subwin-h))

(defpackage :ui-goodies
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :access
   :croatoan
   :config
   :constants
   :text-utils
   :misc
   :mtree
   :specials
   :windows
   :tui-utils
   :program-events)
  (:nicknames :ui)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export
   :delete-message-status-marked-to-delete
   :quit-program
   :clean-close-program
   :notify
   :notify-procedure
   :with-blocking-notify-procedure
   :info-dialog
   :info-dialog-immediate
   :error-dialog
   :error-dialog-immediate
   :input-dialog-immediate
   :error-message
   :info-message
   :confirm-file-overwrite-dialog-immediate
   :confirm-dialog-immediate
   :request-error-window
   :ask-string-input
   :thread-go-up
   :thread-go-down
   :thread-goto-message
   :thread-goto-first-message
   :thread-goto-last-message
   :thread-search-next-message-body
   :thread-search-previous-message-body
   :thread-search-next-message-meta
   :thread-search-previous-message-meta
   :thread-search-next-unread-message
   :thread-open-selected-message
   :thread-mark-delete-selected-message
   :thread-mark-prevent-delete-selected-message
   :subscribe-to-hash
   :unsubscribe-to-hash
   :message-scroll-up
   :message-scroll-down
   :message-scroll-begin
   :message-scroll-end
   :message-scroll-next-page
   :message-scroll-previous-page
   :message-search-regex
   :focus-to-message-window
   :focus-to-thread-window
   :focus-to-send-message-window
   :focus-to-follow-requests-window
   :focus-to-tags-window
   :focus-to-conversations-window
   :print-quick-help
   :move-message-tree
   :change-folder
   :change-timeline
   :update-current-timeline
   :update-current-timeline-backwards
   :refresh-thread
   :refresh-tags
   :favourite-selected-status
   :unfavourite-selected-status
   :boost-selected-status
   :unboost-selected-status
   :ignore-user
   :unignore-user
   :message-exceeds-server-limit-p
   :exceeding-characters-notify
   :send-message
   :compose-message
   :reply-message
   :open-message-attach
   :open-message-attach-go-up
   :open-message-attach-go-down
   :open-message-attach-perform-opening
   :close-open-attach-window
   :open-message-link
   :open-message-link-go-up
   :open-message-link-go-down
   :open-message-link-perform-opening
   :close-open-message-link-window
   :attach-go-up
   :attach-go-down
   :attach-delete
   :attach-add
   :follow-request-go-up
   :follow-request-go-down
   :follow-request-delete
   :start-follow-request-processing
   :cancel-follow-requests
   :process-follow-requests
   :change-subject
   :change-visibility
   :edit-message-body
   :cancel-send-message
   :close-send-message-window
   :follow-user
   :unfollow-user
   :tag-go-up
   :tag-go-down
   :open-tag-folder
   :update-conversations
   :open-conversation
   :conversation-go-up
   :conversation-go-down
   :goto-conversation
   :ignore-conversation
   :delete-conversation
   :rename-converation
   :change-conversation-name
   :report-status
   :crypto-import-key
   :crypto-export-key
   :crypto-generate-key
   :show-about-window
   :reset-timeline-pagination
   :poll-vote))

(defpackage :modules
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :config
   :constants
   :text-utils
   :resources-utils
   :specials
   :windows
   :sending-message
   :keybindings
   :program-events
   :ui-goodies)
  (:shadowing-import-from :resources-utils :init)
  (:export
   :load-sys-module
   :load-module))

(defpackage :scripts
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :config
   :constants
   :text-utils
   :misc-utils
   :api-client)
  (:shadowing-import-from :misc :random-elt :shuffle)
  (:export))

(defpackage :main
  (:use
   :cl
   :alexandria
   :cl-ppcre
   :croatoan
   :config
   :constants
   :text-utils
   :command-line)
  (:export))
