;; this is the main configuration file for tinmop.

;; This file must be a valid  common lisp program to allow the program
;; to even starts. This file is actual common lisp source code that is
;; loaded end  executed by the main  program; be careful, do  not copy
;; and paste  code from untrusted sources  as this could results  in a
;; *severe* security damage.

;; Anyway, even if you  do not know lisp you should  be able to change
;; keybindings  with no  difficult. Editing  this file  is the  way to
;; accomplish this task.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This line on  top of the file  is mandatory if you want  to write a
;; module (AKA plugin) for the program  as the form below will provide
;; you with access to many program's machinery

(in-package :modules)

;; of course you can define new  functions. Also note that the function called at
;; the end of the each command:
;;
;; (define-key "command keybinding" function)
;;                                  ^^^^^^^^
;;
;; can not be an anonymous function.

(defun quit ()        ; define a custom function named 'quit' and no parameters.
  "Quit the  program" ; This  string after the function  name and parameters is
                      ; called 'docstring' and will be presented to the
                      ; user as inline help
  (ui:clean-close-program))

;; this file can load others using the 'load-module' function the file
;; in  looked  up in  the  home  (e.g.  $HOME/.config/tinmop/)  config
;; directory or in the system wide config directory

(load-module "next-previous-open.lisp")

;; share a gemini page

(load-module "share-gemini-link.lisp")

;; abbrevation on command line

(load-module "expand-abbrev-command-window.lisp")

;; delete posts using a regeula expression

(load-module "delete-by-regex.lisp")

;; keybindings syntax:

;; a command is executed after a sequence  of one or more keys.  a key
;; can be composed by a single physical button pressed on the keyboard
;; or by a combination of a button pressed together.

;; Example of keys are:

;; "a"     -> press the button on the keyboard with the symbol 'a' printed on top
;; "a b"   -> press and release the button 'a' and then press the button 'b'
;; "f1 f2" -> press function key 1 and function key 2

;; sometimes a key is composed by  two pressed buttons. For example to
;; input the character 'C' (i.e. capital 'c') usually you should use a
;; combination of SHIFT and C but there is more: a key can be composed
;; by two button pressed, the first called a "modifier" button.

;; The "modifier" button  must  be keep pressed
;; while the second button is hit for the key to be valid.

;; There  are  only  two  legal modifier  buttons:  the  "Alt"  button
;; (indicated with the letter "M") and the "Control" button (indicated
;; with "C").

;; So said some combined keys example are given:

;; "M-1" -> press "Alt" and, while pressed, press "1"

;; Note the dash between the symbols, the key below:

;; "M 1"

;; means: "Press the combination of button to print the capital button
;; 'M'  and the  button to  print the  character '1',  so this  is not
;; equivalent to "M-1".

;; With this information in mind we can decode more keys like the ones
;; given below:

;; "C-c x"  -> Press control and  keep pressed press 'c'  then release
;; the two buttons and press 'x'.

;; "C-c c"  -> Press control and  keep pressed press 'c'  then release
;; the two buttons and press 'c' again.

;; Caveat:

;; - The single letter after control modifier are case insensitive;
;; - the enter key must be specified by "C-J";
;; - "dc" is the key Delete;

;; To define a new key just use the 'define-key' form below:

;; (define key COMMAND FUNCTION)

;; Where  COMMAND is  a  sequence of  keys and  FUNCTION  is the  name
;; (prefixed by:  #') of the function  to fire after command  has been
;; completed

;; See the command below as examples.

;; key conflict

;; Sometime two commands key may conflict, for example:

;; (define "C-x a"   #'foo)

;; (define "C-x a b" #'bar)

;; How the program  could know which way choose when  the button 'a' is
;; pressed?  Should be  executed the  function 'foo'  or should  we go
;; beyond this function and wait for the button 'b' to be pressed?

;; The convention  chosen is  that will be  executed the  shorter path
;; that lead to  a function so, in the case  above, the function 'foo'
;; will be executed.

;; Note that the two command below are *not* in conflict:

;; (define "C-x a b c d"   #'foo)

;; (define "C-x a e"       #'bar)

(defun gemini-search ()
 (gemini-viewer:load-gemini-url "gemini://geminispace.info/search"))

;; global keymap

(define-key "q"         #'confirm-and-clean-close-program)

(define-key "C-q"       #'quit)   ;  here we  are calling  the custom
                                  ; function defined above

(define-key "C-I"       #'pass-focus-next)

(define-key "C-a"       #'show-about-window)

(define-key "C d"       #'clear-cache)

(define-key "?"         #'print-quick-help)

(define-key "C-h h"     #'print-quick-help)

(define-key "C-h a"     #'apropos-help)

(define-key "C-h A"     #'apropos-help-global)

(define-key "C-h m"     #'open-manual)

(define-key "!"         #'gemini-search)

(define-key ">"         #'open-net-address)

(define-key "M-c"       #'open-chats-list-window)

(define-key "M-g s o"   #'gemini-open-gemlog-window)

(define-key "M-g s r"   #'gemlog-refresh-all)

(define-key "M-g s t a" #'gemlog-add-unread-posts-tour)

(define-key "M-g c i"   #'import-gemini-certificate)

(define-key "M-g c s"   #'gemini-open-certificates-window)

(define-key "M-g g l"   #'open-gempub-library)

(define-key "M-g g b s" #'display-bookmark)

(define-key "M-t t"     #'next-tour-link)

(define-key "M-t s"     #'show-tour-links)

(define-key "M-t S"     #'shuffle-tour)

(define-key "M-t c"     #'clean-all-tour)

(define-key "M-right"   #'pass-focus-on-right)

(define-key "M-left"    #'pass-focus-on-left)

(define-key "M-down"    #'pass-focus-on-bottom)

(define-key "M-up"      #'pass-focus-on-top)

(define-key "M-s l"     #'message-window-lock-scrolling)

(define-key "M-s u"     #'message-window-unlock-scrolling)

(define-key "M-e"       #'eval-command)

(define-key "M-l"       #'load-script-file)

;; follow requests keymap

(define-key "up"        #'follow-request-go-up                        *follow-requests-keymap*)

(define-key "down"      #'follow-request-go-down                      *follow-requests-keymap*)

(define-key "k"         #'follow-request-go-up                        *follow-requests-keymap*)

(define-key "j"         #'follow-request-go-down                      *follow-requests-keymap*)

(define-key "d"         #'follow-request-delete                       *follow-requests-keymap*)

(define-key "C-J"       #'process-follow-requests                     *follow-requests-keymap*)

(define-key "q"         #'cancel-follow-requests                      *follow-requests-keymap*)

;; send message keymap

(define-key "up"        #'attach-go-up                                *send-message-keymap*)

(define-key "down"      #'attach-go-down                              *send-message-keymap*)

(define-key "k"         #'attach-go-up                                *send-message-keymap*)

(define-key "j"         #'attach-go-down                              *send-message-keymap*)

(define-key "d"         #'attach-delete                               *send-message-keymap*)

(define-key "s"         #'change-subject                              *send-message-keymap*)

(define-key "m"         #'change-mentions                             *send-message-keymap*)

(define-key "q"         #'cancel-send-message                         *send-message-keymap*)

(define-key "v"         #'change-visibility                           *send-message-keymap*)

(define-key "e"         #'edit-message-body                           *send-message-keymap*)

(define-key "C-J"       #'send-message                                *send-message-keymap*)

;; thread window keymap

(define-key "up"        #'thread-go-up                                *thread-keymap*)

(define-key "down"      #'thread-go-down                              *thread-keymap*)

(define-key "k"         #'thread-go-up                                *thread-keymap*)

(define-key "j"         #'thread-go-down                              *thread-keymap*)

(define-key "C-J"       #'thread-open-selected-message                *thread-keymap*)

(define-key "dc"        #'thread-mark-delete-selected-message         *thread-keymap*)

(define-key "U"         #'thread-mark-prevent-delete-selected-message *thread-keymap*)

(define-key "right"     #'open-next                                   *thread-keymap*)

(define-key "left"      #'open-previous                               *thread-keymap*)

(define-key "d"         #'delete-and-move-next                        *thread-keymap*)

(define-key "M-u"       #'delete-and-move-previous                    *thread-keymap*)

(define-key "D"         #'delete-post-using-regex                     *thread-keymap*)

(define-key "g"         #'thread-goto-message                         *thread-keymap*)

(define-key "/ b"       #'thread-search-next-message-body             *thread-keymap*)

(define-key "\\\\ b"    #'thread-search-previous-message-body         *thread-keymap*)

(define-key "/ m"       #'thread-search-next-message-meta             *thread-keymap*)

(define-key "\\\\ m"    #'thread-search-previous-message-meta         *thread-keymap*)

(define-key "N"         #'repeat-search                               *thread-keymap*)

(define-key "n"         #'thread-search-next-unread-message           *thread-keymap*)

(define-key "home"      #'thread-goto-first-message                   *thread-keymap*)

(define-key "end"       #'thread-goto-last-message                    *thread-keymap*)

(define-key "c"         #'compose-message                             *thread-keymap*)

(define-key "r"         #'reply-message                               *thread-keymap*)

(define-key "x"         #'refresh-thread                              *thread-keymap*)

(define-key "X"         #'refresh-thread-totally                      *thread-keymap*)

(define-key "v"         #'open-message-attach                         *thread-keymap*)

(define-key "l"         #'open-message-link                           *thread-keymap*)

(define-key "P"         #'poll-vote                                   *thread-keymap*)

(define-key "C-c u"     #'update-conversations                        *thread-keymap*)

(define-key "C-c o"     #'open-conversation                           *thread-keymap*)

(define-key "C-c c"     #'change-conversation-name                    *thread-keymap*)

(define-key "C-f c"     #'change-folder                               *thread-keymap*)

(define-key "C-t c"     #'change-timeline                             *thread-keymap*)

(define-key "C-t u"     #'update-current-timeline                     *thread-keymap*)

(define-key "C-t U"     #'update-current-timeline-backwards           *thread-keymap*)

(define-key "C-t R"     #'reset-timeline-pagination                   *thread-keymap*)

(define-key "C-t h r"   #'refresh-tags                                *thread-keymap*)

(define-key "C-u i"     #'ignore-user                                 *thread-keymap*)

(define-key "C-u x"     #'unignore-user                               *thread-keymap*)

(define-key "C-u f"     #'follow-user                                 *thread-keymap*)

(define-key "C-u r f"   #'start-follow-request-processing             *thread-keymap*)

(define-key "C-u r r"   #'report-status                               *thread-keymap*)

(define-key "C-u u"     #'unfollow-user                               *thread-keymap*)

(define-key "C-u v a"   #'view-user-avatar                            *thread-keymap*)

(define-key "C-u c k i" #'crypto-import-key                           *thread-keymap*)

(define-key "C-u c k s" #'crypto-export-key                           *thread-keymap*)

(define-key "C-u c k g" #'crypto-generate-key                         *thread-keymap*)

(define-key "C-X m t"   #'move-message-tree                           *thread-keymap*)

(define-key "C-X m f"   #'favourite-selected-status                   *thread-keymap*)

(define-key "C-X m r f" #'unfavourite-selected-status                 *thread-keymap*)

(define-key "C-X m b"   #'boost-selected-status                       *thread-keymap*)

(define-key "C-X m r b" #'unboost-selected-status                     *thread-keymap*)

(define-key "C-X m s"   #'subscribe-to-hash                           *thread-keymap*)

(define-key "C-X m u"   #'unsubscribe-to-hash                         *thread-keymap*)

(define-key "|"         #'send-message-to-pipe                        *thread-keymap*)

;; message window keymap

(define-key "up"        #'message-scroll-up                           *message-keymap*)

(define-key "down"      #'message-scroll-down                         *message-keymap*)

(define-key "k"         #'message-scroll-up                           *message-keymap*)

(define-key "j"         #'message-scroll-down                         *message-keymap*)

(define-key "left"      #'message-scroll-left                         *message-keymap*)

(define-key "right"     #'message-scroll-right                        *message-keymap*)

(define-key "C-J"       #'message-scroll-down                         *message-keymap*)

(define-key "home"      #'message-scroll-begin                        *message-keymap*)

(define-key "end"       #'message-scroll-end                          *message-keymap*)

(define-key "/"         #'message-search-regex                        *message-keymap*)

(define-key "N"         #'repeat-search                               *message-keymap*)

(define-key "npage"     #'message-scroll-next-page                    *message-keymap*)

(define-key "ppage"     #'message-scroll-previous-page                *message-keymap*)

(define-key "|"         #'send-to-pipe                                *message-keymap*)

;; gemini viewer keymap

(define-key "up"        #'message-scroll-up                          *gemini-message-keymap*)

(define-key "down"      #'message-scroll-down                        *gemini-message-keymap*)

(define-key "k"         #'message-scroll-up                          *gemini-message-keymap*)

(define-key "j"         #'message-scroll-down                        *gemini-message-keymap*)

(define-key "left"      #'message-scroll-left                        *gemini-message-keymap*)

(define-key "right"     #'message-scroll-right                       *gemini-message-keymap*)

(define-key "C-J"       #'message-scroll-down                        *gemini-message-keymap*)

(define-key "home"      #'message-scroll-begin                       *gemini-message-keymap*)

(define-key "end"       #'message-scroll-end                         *gemini-message-keymap*)

(define-key "/"         #'message-search-regex                       *gemini-message-keymap*)

(define-key "N"         #'repeat-search                              *gemini-message-keymap*)

(define-key "npage"     #'message-scroll-next-page                   *gemini-message-keymap*)

(define-key "ppage"     #'message-scroll-previous-page               *gemini-message-keymap*)

(define-key "l"         #'open-message-link                          *gemini-message-keymap*)

(define-key "b"         #'gemini-history-back                        *gemini-message-keymap*)

(define-key "B"         #'address-go-back-in-path                    *gemini-message-keymap*)

(define-key "R"         #'address-go-root-path                       *gemini-message-keymap*)

(define-key "C-b a"     #'bookmark-gemini-page                       *gemini-message-keymap*)

(define-key "C-b s"     #'display-bookmark                           *gemini-message-keymap*)

(define-key "C-b d"     #'delete-gemini-bookmark                     *gemini-message-keymap*)

(define-key "U"         #'gemini-view-source                         *gemini-message-keymap*)

(define-key "d"         #'gemini-open-streams-window                 *gemini-message-keymap*)

(define-key "c"         #'gemini-open-certificates-window            *gemini-message-keymap*)

(define-key "r"         #'gemini-refresh-page                        *gemini-message-keymap*)

(define-key "s"         #'gemini-subscribe-gemlog                    *gemini-message-keymap*)

(define-key "p"         #'message-toggle-preformatted-block          *gemini-message-keymap*)

(define-key "t"         #'next-tour-link                             *gemini-message-keymap*)

(define-key "O"         #'open-gemini-toc                            *gemini-message-keymap*)

(define-key "T"         #'show-tour-links                            *gemini-message-keymap*)

(define-key "|"         #'send-to-pipe                               *gemini-message-keymap*)

(define-key "]"         #'open-next-visible-link                     *gemini-message-keymap*)

(define-key "C-]"       #'go-to-next-link                            *gemini-message-keymap*)

(define-key "C-["       #'go-to-previous-link                        *gemini-message-keymap*)

;; gemini page table of contents keymap

(define-key "up"        #'gemini-toc-scroll-up                       *gemini-toc-keymap*)

(define-key "down"      #'gemini-toc-scroll-down                     *gemini-toc-keymap*)

(define-key "k"         #'gemini-toc-scroll-up                       *gemini-toc-keymap*)

(define-key "j"         #'gemini-toc-scroll-down                     *gemini-toc-keymap*)

(define-key "q"         #'gemini-toc-close                           *gemini-toc-keymap*)

(define-key "n"         #'gemini-toc-scroll-down-page                *gemini-toc-keymap*)

(define-key "C-J"       #'gemini-toc-scroll-down-page                *gemini-toc-keymap*)

(define-key "p"         #'gemini-toc-scroll-up-page                  *gemini-toc-keymap*)

(define-key "home"      #'gemini-toc-scroll-begin                    *gemini-toc-keymap*)

(define-key "end"       #'gemini-toc-scroll-end                      *gemini-toc-keymap*)

(define-key "/"         #'gemini-toc-search                          *gemini-toc-keymap*)

(define-key "N"         #'repeat-search                              *gemini-toc-keymap*)

;; gemini stream window keymap

(define-key "a"         #'gemini-abort-download                      *gemini-downloads-keymap*)

(define-key "up"        #'gemini-streams-window-up                   *gemini-downloads-keymap*)

(define-key "down"      #'gemini-streams-window-down                 *gemini-downloads-keymap*)

(define-key "k"         #'gemini-streams-window-up                   *gemini-downloads-keymap*)

(define-key "j"         #'gemini-streams-window-down                 *gemini-downloads-keymap*)

(define-key "q"         #'gemini-streams-window-close                *gemini-downloads-keymap*)

(define-key "C-J"       #'gemini-streams-window-open-stream          *gemini-downloads-keymap*)

;; gemini certificates window keymap

(define-key "up"        #'gemini-certificate-window-go-up            *gemini-certificates-keymap*)

(define-key "down"      #'gemini-certificate-window-go-down          *gemini-certificates-keymap*)

(define-key "k"         #'gemini-certificate-window-go-up            *gemini-certificates-keymap*)

(define-key "j"         #'gemini-certificate-window-go-down          *gemini-certificates-keymap*)

(define-key "q"         #'gemini-close-certificate-window            *gemini-certificates-keymap*)

(define-key "C-J"       #'gemini-delete-certificate                  *gemini-certificates-keymap*)

;; gemini subscription window

(define-key "C-J"       #'show-gemlog-to-screen                      *gemlog-subscription-keymap*)

(define-key "up"        #'gemlogs-subscription-go-up                 *gemlog-subscription-keymap*)

(define-key "down"      #'gemlogs-subscription-go-down               *gemlog-subscription-keymap*)

(define-key "k"         #'gemlogs-subscription-go-up                 *gemlog-subscription-keymap*)

(define-key "j"         #'gemlogs-subscription-go-down               *gemlog-subscription-keymap*)

(define-key "q"         #'close-gemlog-window                        *gemlog-subscription-keymap*)

(define-key "d"         #'gemlog-cancel-subscription                 *gemlog-subscription-keymap*)

(define-key "l"         #'open-message-link                          *gemlog-subscription-keymap*)

;; gempub library window keymap

(define-key "up"        #'gempub-library-window-go-up                *gempub-library-keymap*)

(define-key "down"      #'gempub-library-window-go-down              *gempub-library-keymap*)

(define-key "k"         #'gempub-library-window-go-up                *gempub-library-keymap*)

(define-key "j"         #'gempub-library-window-go-down              *gempub-library-keymap*)

(define-key "q"         #'gempub-library-window-close                *gempub-library-keymap*)

(define-key "C-J"       #'gempub-open-file                           *gempub-library-keymap*)

;; tags keymap

(define-key "up"        #'tag-go-up                                   *tags-keymap*)

(define-key "down"      #'tag-go-down                                 *tags-keymap*)

(define-key "k"         #'tag-go-up                                   *tags-keymap*)

(define-key "j"         #'tag-go-down                                 *tags-keymap*)

(define-key "C-J"       #'open-tag-folder                             *tags-keymap*)

(define-key "U"         #'unsubscribe-to-hash                         *tags-keymap*)

(define-key "s"         #'subscribe-to-hash                           *tags-keymap*)

(define-key "r"         #'refresh-tags                                *tags-keymap*)

;; conversations keymap

(define-key "C-c c"     #'change-conversation-name                    *conversations-keymap*)

(define-key "C-J"       #'goto-conversation                           *conversations-keymap*)

(define-key "up"        #'conversation-go-up                          *conversations-keymap*)

(define-key "down"      #'conversation-go-down                        *conversations-keymap*)

(define-key "k"         #'conversation-go-up                          *conversations-keymap*)

(define-key "j"         #'conversation-go-down                        *conversations-keymap*)

(define-key "dc"        #'delete-conversation                         *conversations-keymap*)

(define-key "I"         #'ignore-conversation                         *conversations-keymap*)

;; attachments keymap

(defun copy-to-clipboard (window)
  (ui:copy-link-to-clipboard window))

(define-key "C-c"       (lambda ()
                          (copy-link-to-clipboard specials:*open-attach-window*))
  *open-attach-keymap*)

(define-key "C-J"       #'open-message-attach-perform-opening           *open-attach-keymap*)

(define-key "a"         #'open-all-message-attachments                  *open-attach-keymap*)

(define-key "up"        #'open-message-attach-go-up                     *open-attach-keymap*)

(define-key "down"      #'open-message-attach-go-down                   *open-attach-keymap*)

(define-key "k"          #'open-message-attach-go-up                    *open-attach-keymap*)

(define-key "j"         #'open-message-attach-go-down                   *open-attach-keymap*)

(define-key "q"         #'close-open-attach-window                      *open-attach-keymap*)

;; message links keymap

(define-key "C-J"       #'open-message-link-perform-opening             *open-message-link-keymap*)

(define-key "C-c"        (lambda ()
                          (copy-link-to-clipboard specials:*open-message-link-window*))
  *open-message-link-keymap*)

(define-key "up"        #'open-message-link-go-up                       *open-message-link-keymap*)

(define-key "down"      #'open-message-link-go-down                     *open-message-link-keymap*)

(define-key "k"         #'open-message-link-go-up                       *open-message-link-keymap*)

(define-key "j"         #'open-message-link-go-down                     *open-message-link-keymap*)

(define-key "q"         #'close-open-message-link-window                *open-message-link-keymap*)

(define-key "e"         #'open-message-link-open-enqueue                *open-message-link-keymap*)

(define-key "/"         #'search-link-window                            *open-message-link-keymap*)

(define-key "N"         #'repeat-search                                 *open-message-link-keymap*)

(define-key "t"         #'tour-mode-link                                *open-message-link-keymap*)

(define-key "T"         #'save-selected-message-in-tour                 *open-message-link-keymap*)

(define-key "home"      #'open-message-link-window-scroll-begin         *open-message-link-keymap*)

(define-key "end"       #'open-message-link-window-scroll-end           *open-message-link-keymap*)

(define-key "]"         #'gemini-jump-to-link                           *open-message-link-keymap*)

;; chats list window

(define-key "r"         #'refresh-chat-messages                         *chats-list-keymap*)

(define-key "R"         #'refresh-chats                                 *chats-list-keymap*)

(define-key "q"         #'close-chats-list-window                       *chats-list-keymap*)

(define-key "C-J"       #'show-chat-to-screen                           *chats-list-keymap*)

(define-key "l"         #'change-chat-label                             *chats-list-keymap*)

(define-key "c"         #'chat-create-new                               *chats-list-keymap*)

(define-key "up"        #'chat-list-go-up                               *chats-list-keymap*)

(define-key "down"      #'chat-list-go-down                             *chats-list-keymap*)

(define-key "k"         #'chat-list-go-up                               *chats-list-keymap*)

(define-key "j"         #'chat-list-go-down                             *chats-list-keymap*)


;; chat  window

(defun write-to-chat ()
  (chat-loop (message-window:metadata specials:*message-window*)))

(define-key "M-c"       #'write-to-chat                                 *chat-message-keymap*)

(define-key "up"        #'message-scroll-up                             *chat-message-keymap*)

(define-key "down"      #'message-scroll-down                           *chat-message-keymap*)

(define-key "k"         #'message-scroll-up                             *chat-message-keymap*)

(define-key "j"         #'message-scroll-down                           *chat-message-keymap*)

(define-key "home"      #'message-scroll-begin                          *chat-message-keymap*)

(define-key "end"       #'message-scroll-end                            *chat-message-keymap*)

(define-key "/"         #'message-search-regex                          *chat-message-keymap*)

(define-key "npage"     #'message-scroll-next-page                      *chat-message-keymap*)

(define-key "ppage"     #'message-scroll-previous-page                  *chat-message-keymap*)

(define-key "a"         #'open-chat-link-window                         *chat-message-keymap*)

;; file explorer

(define-key "x"         #'file-explorer-expand-path     *filesystem-explorer-keymap*)

(define-key "c"         #'file-explorer-close-path      *filesystem-explorer-keymap*)

(define-key "r"         #'file-explorer-rename-path     *filesystem-explorer-keymap*)

(define-key "D"         #'file-explorer-delete-tree     *filesystem-explorer-keymap*)

(define-key "X"         #'file-explorer-delete-marked   *filesystem-explorer-keymap*)

(define-key "a"         #'file-explorer-create-path     *filesystem-explorer-keymap*)

(define-key "d"         #'file-explorer-download-path   *filesystem-explorer-keymap*)

(define-key "u"         #'file-explorer-upload-path     *filesystem-explorer-keymap*)

(define-key "/"         #'file-explorer-search          *filesystem-explorer-keymap*)

(define-key "N"         #'repeat-search                 *filesystem-explorer-keymap*)

(define-key "m"         #'file-explorer-mark-entry      *filesystem-explorer-keymap*)

(define-key "M-m"       #'file-explorer-mark-by-regexp  *filesystem-explorer-keymap*)

(define-key "up"        #'file-explorer-go-up           *filesystem-explorer-keymap*)

(define-key "down"      #'file-explorer-go-down         *filesystem-explorer-keymap*)

(define-key "k"         #'file-explorer-go-up           *filesystem-explorer-keymap*)

(define-key "j"         #'file-explorer-go-down         *filesystem-explorer-keymap*)

(define-key "home"      #'file-explorer-scroll-begin    *filesystem-explorer-keymap*)

(define-key "end"       #'file-explorer-scroll-end      *filesystem-explorer-keymap*)

(define-key "q"         #'file-explorer-close-window    *filesystem-explorer-keymap*)

(define-key "C-J"       #'file-explorer-open-node       *filesystem-explorer-keymap*)

(define-key "e"         #'file-explorer-edit-file       *filesystem-explorer-keymap*)

(define-key "i"         #'file-explorer-node-details    *filesystem-explorer-keymap*)

(define-key "M u"       #'file-explorer-upload-mirror   *filesystem-explorer-keymap*)

(define-key "M d"       #'file-explorer-download-mirror *filesystem-explorer-keymap*)

;;;; hooks

;; this module  will install an  hook to  rewrite urls; By  default it
;; does nothing, see the source for configuration

(load-module "rewrite-message-urls.lisp")

;;; this hooks will skips toots with contain less than 20 words
;;; (note: it is commented out)

;; (hooks:add-hook 'hooks:*skip-message-hook*
;;                 (lambda (toot timeline folder kind localp)
;;                   (declare (ignore timeline folder kind localp))
;;                   (when-let* ((text        (html-utils:html->text (tooter:content toot)
;;                                                                   :add-link-footnotes nil))
;;                               (trimmed     (text-utils:trim-blanks text))
;;                               (word-counts (length (text-utils:split-words trimmed))))
;;                     (< word-counts 10))))
