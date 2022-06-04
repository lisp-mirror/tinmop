;; tinmop: an humble gemini and pleroma client
;; Copyright (C) 2020  cage

;; This  program is  free  software: you  can  redistribute it  and/or
;; modify it  under the  terms of  the GNU  General Public  License as
;; published by the Free Software  Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or FITNESS  FOR A PARTICULAR PURPOSE.   See the GNU
;; General Public License for more  details.  You should have received
;; a copy of  the GNU General Public License along  with this program.
;; If not, see <http://www.gnu.org/licenses/>.

(in-package :resources-utils)

(alexandria:define-constant +virtual-fs-dir-separator+ "/" :test #'string=)

(alexandria:define-constant +virtual-fs-dir-separator-regexp+ "\\/" :test #'string=)

(defun construct-path (p)
  (let ((splitted (split +virtual-fs-dir-separator-regexp+ p)))
    (strcat *directory-sep* (join-with-strings splitted *directory-sep*)
            (if (cl-ppcre:scan (strcat +virtual-fs-dir-separator-regexp+ "$") p)
                *directory-sep*))))

(defun home-datadir ()
  (concatenate 'string
               (pathname->namestring (uiop:xdg-data-home +program-name+))
               "/"))

(defun home-confdir ()
  (pathname->namestring (uiop:xdg-config-home +program-name+)))

(defun init ()
  #+debug-mode
  (progn
    (when (not (directory-exists-p (home-datadir)))
      (misc:dbg "creating ~a" (home-datadir)))
    (when (not (directory-exists-p (home-confdir)))
      (misc:dbg "creating ~a" (home-confdir))))
  (fs:make-directory (home-datadir))
  (fs:make-directory (home-confdir)))

(defun get-resource-file (system-dir home-dir path)
  (let ((system-file (fs:cat-parent-dir system-dir path))
        (home-file   (fs:cat-parent-dir home-dir   path)))
    (cond
      ((fs:file-exists-p home-file)
       home-file)
      ((fs:file-exists-p system-file)
       system-file)
      (t
       (let ((msg (_ "Cannot find ~s in either ~s or ~s.")))
         (restart-case
             (error (format nil msg path system-file home-file))
           (return-home-filename ()
             home-file)
           (return-system-filename ()
             system-file)
           (create-empty-in-home  ()
             (fs:create-file home-file :skip-if-exists t)
             (get-resource-file system-dir home-dir path))))))))

(defun get-resource-dir (system-dir home-dir path)
  (let ((system-dir (fs:cat-parent-dir system-dir path))
        (home-dir   (fs:cat-parent-dir home-dir   path)))
    (cond
      ((fs:dirp home-dir)
       home-dir)
      ((fs:dirp system-dir)
       system-dir)
      (t
       (let ((msg (_ "Cannot find ~s in either ~s or ~s.")))
         (restart-case
             (error (format nil msg path system-dir home-dir))
           (return-home-dirname ()
             home-dir)
           (return-system-dirname ()
             system-dir)
           (create-empty-in-home  ()
             (fs:make-directory home-dir)
             (get-resource-dir system-dir home-dir path))))))))

(defun get-config-file (path)
  (get-resource-file +sys-conf-dir+ (home-confdir) path))

(defun get-sys-config-file (path &optional (system-dir +sys-conf-dir+))
  (let ((system-file (fs:cat-parent-dir system-dir path)))
    (if (fs:file-exists-p system-file)
        system-file
        (let ((msg (_ "Unrecoverable error: cannot find file ~s.")))
          (error (format nil msg system-file))))))

(defun get-data-file (path)
  (get-resource-file +sys-data-dir+ (home-datadir) path))

(defun get-data-dir (path)
  (get-resource-dir +sys-data-dir+ (home-datadir) path))
