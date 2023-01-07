;;; compat-26.el --- Compatibility Layer for Emacs 26.1  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Find here the functionality added in Emacs 26.1, needed by older
;; versions.

;;; Code:

(eval-when-compile (load "compat-macs.el" nil t t))
(compat-declare-version "26.1")

;;;; Defined in fns.c

(compat-defun assoc (key alist &optional testfn) ;; <OK>
  "Handle the optional TESTFN."
  :explicit t
  (if testfn
      (catch 'found
        (dolist (ent alist)
          (when (funcall testfn (car ent) key)
            (throw 'found ent))))
    (assoc key alist)))

(compat-defun mapcan (func sequence) ;; <OK>
  "Apply FUNC to each element of SEQUENCE.
Concatenate the results by altering them (using `nconc').
SEQUENCE may be a list, a vector, a boolean vector, or a string."
  (apply #'nconc (mapcar func sequence)))

(compat-defun line-number-at-pos (&optional position absolute) ;; <OK>
  "Handle optional argument ABSOLUTE."
  :explicit t
  (if absolute
      (save-restriction
        (widen)
        (line-number-at-pos position))
    (line-number-at-pos position)))

;;;; Defined in subr.el

(compat-defun alist-get (key alist &optional default remove testfn) ;; <OK>
  "Handle optional argument TESTFN."
  :explicit t
  (if testfn
      (let (entry)
        (cond
         ((eq testfn 'eq)
          (setq entry (assq key alist)))
         ((eq testfn 'equal)
          (setq entry (assoc key alist)))
         ((catch 'found
            (dolist (ent alist)
              (when (and (consp ent) (funcall testfn (car ent) key))
                (throw 'found (setq entry ent))))
            default)))
        (if entry (cdr entry) default))
    (alist-get key alist default remove)))

;; NOTE: Define gv expander only if `compat--alist-get' is defined.
(when (eval-when-compile (version< emacs-version "26.1"))
  (gv-define-expander compat--alist-get
    (lambda (do key alist &optional default remove testfn)
      (macroexp-let2 macroexp-copyable-p k key
        (gv-letplace (getter setter) alist
          (macroexp-let2 nil p `(compat--assoc ,k ,getter ,testfn)
            (funcall do (if (null default) `(cdr ,p)
                          `(if ,p (cdr ,p) ,default))
                     (lambda (v)
                       (macroexp-let2 nil v v
                         (let ((set-exp
                                `(if ,p (setcdr ,p ,v)
                                   ,(funcall setter
                                             `(cons (setq ,p (cons ,k ,v))
                                                    ,getter)))))
                           `(progn
                              ,(cond
                                ((null remove) set-exp)
                                ((or (eql v default)
                                     (and (eq (car-safe v) 'quote)
                                          (eq (car-safe default) 'quote)
                                          (eql (cadr v) (cadr default))))
                                 `(if ,p ,(funcall setter `(delq ,p ,getter))))
                                (t
                                 `(cond
                                   ((not (eql ,default ,v)) ,set-exp)
                                   (,p ,(funcall setter
                                                 `(delq ,p ,getter))))))
                              ,v)))))))))))

(compat-defun string-trim-left (string &optional regexp) ;; <OK>
  "Handle optional argument REGEXP."
  :explicit t
  (if (string-match (concat "\\`\\(?:" (or regexp "[ \t\n\r]+") "\\)") string)
      (substring string (match-end 0))
    string))

(compat-defun string-trim-right (string &optional regexp) ;; <OK>
  "Handle optional argument REGEXP."
  :explicit t
  (let ((i (string-match-p
            (concat "\\(?:" (or regexp "[ \t\n\r]+") "\\)\\'")
            string)))
    (if i (substring string 0 i) string)))

(compat-defun string-trim (string &optional trim-left trim-right) ;; <OK>
  "Handle optional arguments TRIM-LEFT and TRIM-RIGHT."
  :explicit t
  (compat--string-trim-left
   (compat--string-trim-right
    string
    trim-right)
   trim-left))

(compat-defun caaar (x) ;; <OK>
  "Return the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (car (car (car x))))

(compat-defun caadr (x) ;; <OK>
  "Return the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (car (cdr x))))

(compat-defun cadar (x) ;; <OK>
  "Return the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (cdr (car x))))

(compat-defun caddr (x) ;; <OK>
  "Return the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (cdr x))))

(compat-defun cdaar (x) ;; <OK>
  "Return the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (car (car x))))

(compat-defun cdadr (x) ;; <OK>
  "Return the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (cdr x))))

(compat-defun cddar (x) ;; <OK>
  "Return the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (car x))))

(compat-defun cdddr (x) ;; <OK>
  "Return the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (cdr x))))

(compat-defun caaaar (x) ;; <OK>
  "Return the `car' of the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (car (car (car (car x)))))

(compat-defun caaadr (x) ;; <OK>
  "Return the `car' of the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (car (car (cdr x)))))

(compat-defun caadar (x) ;; <OK>
  "Return the `car' of the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (car (cdr (car x)))))

(compat-defun caaddr (x) ;; <OK>
  "Return the `car' of the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (car (cdr (cdr x)))))

(compat-defun cadaar (x) ;; <OK>
  "Return the `car' of the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (car (cdr (car (car x)))))

(compat-defun cadadr (x) ;; <OK>
  "Return the `car' of the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (car (cdr x)))))

(compat-defun caddar (x) ;; <OK>
  "Return the `car' of the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (car (cdr (cdr (car x)))))

(compat-defun cadddr (x) ;; <OK>
  "Return the `car' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (car (cdr (cdr (cdr x)))))

(compat-defun cdaaar (x) ;; <OK>
  "Return the `cdr' of the `car' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (car (car (car x)))))

(compat-defun cdaadr (x) ;; <OK>
  "Return the `cdr' of the `car' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (car (cdr x)))))

(compat-defun cdadar (x) ;; <OK>
  "Return the `cdr' of the `car' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (car (cdr (car x)))))

(compat-defun cdaddr (x) ;; <OK>
  "Return the `cdr' of the `car' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (car (cdr (cdr x)))))

(compat-defun cddaar (x) ;; <OK>
  "Return the `cdr' of the `cdr' of the `car' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (car (car x)))))

(compat-defun cddadr (x) ;; <OK>
  "Return the `cdr' of the `cdr' of the `car' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (car (cdr x)))))

(compat-defun cdddar (x) ;; <OK>
  "Return the `cdr' of the `cdr' of the `cdr' of the `car' of X."
  (declare (pure t))
  (cdr (cdr (cdr (car x)))))

(compat-defun cddddr (x) ;; <OK>
  "Return the `cdr' of the `cdr' of the `cdr' of the `cdr' of X."
  (declare (pure t))
  (cdr (cdr (cdr (cdr x)))))

(compat-defvar gensym-counter 0 ;; <OK>
  "Number used to construct the name of the next symbol created by `gensym'.")

(compat-defun gensym (&optional prefix) ;; <OK>
  "Return a new uninterned symbol.
The name is made by appending `gensym-counter' to PREFIX.
PREFIX is a string, and defaults to \"g\"."
  (let ((num (prog1 gensym-counter
               (setq gensym-counter
                     (1+ gensym-counter)))))
    (make-symbol (format "%s%d" (or prefix "g") num))))

(compat-defmacro if-let* (varlist then &rest else) ;; <OK>
  "Bind variables according to VARLIST and evaluate THEN or ELSE.
This is like `if-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 2)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var varlist)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(if (cdr var) (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (if ,(caar list) ,then ,@else))))

(compat-defmacro when-let* (varlist &rest body) ;; <OK>
  "Bind variables according to VARLIST and conditionally evaluate BODY.
This is like `when-let' but doesn't handle a VARLIST of the form
\(SYMBOL SOMETHING) specially."
  (declare (indent 1)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var varlist)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(if (cdr var) (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (when ,(caar list) ,@body))))

(compat-defmacro and-let* (varlist &rest body) ;; <OK>
  "Bind variables according to VARLIST and conditionally evaluate BODY.
Like `when-let*', except if BODY is empty and all the bindings
are non-nil, then the result is non-nil."
  (declare (indent 1)
           (debug ((&rest [&or symbolp (symbolp form) (form)])
                   body)))
  (let ((empty (make-symbol "s"))
        (last t) list)
    (dolist (var varlist)
      (push `(,(if (cdr var) (car var) empty)
              (and ,last ,(if (cdr var) (cadr var) (car var))))
            list)
      (when (or (cdr var) (consp (car var)))
        (setq last (caar list))))
    `(let* ,(nreverse list)
       (if ,(caar list) ,(macroexp-progn (or body '(t)))))))

;;;; Defined in files.el

(compat-defvar mounted-file-systems ;; <OK>
    (eval-when-compile
      (if (memq system-type '(windows-nt cygwin))
          "^//[^/]+/"
        (concat
         "^" (regexp-opt '("/afs/" "/media/" "/mnt" "/net/" "/tmp_mnt/")))))
  "File systems that ought to be mounted.")

(compat-defun file-local-name (file) ;; <OK>
  "Return the local name component of FILE.
This function removes from FILE the specification of the remote host
and the method of accessing the host, leaving only the part that
identifies FILE locally on the remote system.
The returned file name can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
  (or (file-remote-p file 'localname) file))

(compat-defun file-name-quoted-p (name &optional top) ;; <OK>
  "Handle optional argument TOP."
  :explicit t
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (string-prefix-p "/:" (file-local-name name))))

(compat-defun file-name-quote (name &optional top) ;; <OK>
  "Handle optional argument TOP."
  :explicit t
  (let ((file-name-handler-alist (unless top file-name-handler-alist)))
    (if (string-prefix-p "/:" (file-local-name name))
        name
      (concat (file-remote-p name) "/:" (file-local-name name)))))

(compat-defun temporary-file-directory () ;; <UNTESTED>
  "The directory for writing temporary files.
In case of a remote `default-directory', this is a directory for
temporary files on that remote host.  If such a directory does
not exist, or `default-directory' ought to be located on a
mounted file system (see `mounted-file-systems'), the function
returns `default-directory'.
For a non-remote and non-mounted `default-directory', the value of
the variable `temporary-file-directory' is returned."
  (let ((handler (find-file-name-handler
                  default-directory 'temporary-file-directory)))
    (if handler
        (funcall handler 'temporary-file-directory)
      (if (string-match mounted-file-systems default-directory)
          default-directory
        temporary-file-directory))))

(compat-defun make-nearby-temp-file (prefix &optional dir-flag suffix) ;; <UNTESTED>
  "Create a temporary file as close as possible to `default-directory'.
If PREFIX is a relative file name, and `default-directory' is a
remote file name or located on a mounted file systems, the
temporary file is created in the directory returned by the
function `temporary-file-directory'.  Otherwise, the function
`make-temp-file' is used.  PREFIX, DIR-FLAG and SUFFIX have the
same meaning as in `make-temp-file'."
  (let ((handler (find-file-name-handler
                  default-directory 'make-nearby-temp-file)))
    (if (and handler (not (file-name-absolute-p default-directory)))
        (funcall handler 'make-nearby-temp-file prefix dir-flag suffix)
      (let ((temporary-file-directory (temporary-file-directory)))
        (make-temp-file prefix dir-flag suffix)))))

(compat-defun file-attribute-type (attributes) ;; <OK>
  "The type field in ATTRIBUTES returned by `file-attributes'.
The value is either t for directory, string (name linked to) for
symbolic link, or nil."
  (nth 0 attributes))

(compat-defun file-attribute-link-number (attributes) ;; <OK>
  "Return the number of links in ATTRIBUTES returned by `file-attributes'."
  (nth 1 attributes))

(compat-defun file-attribute-user-id (attributes) ;; <OK>
  "The UID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
  (nth 2 attributes))

(compat-defun file-attribute-group-id (attributes) ;; <OK>
  "The GID field in ATTRIBUTES returned by `file-attributes'.
This is either a string or a number.  If a string value cannot be
looked up, a numeric value, either an integer or a float, is
returned."
  (nth 3 attributes))

(compat-defun file-attribute-access-time (attributes) ;; <OK>
  "The last access time in ATTRIBUTES returned by `file-attributes'.
This a Lisp timestamp in the style of `current-time'."
  (nth 4 attributes))

(compat-defun file-attribute-modification-time (attributes) ;; <OK>
  "The modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of the last change to the file's contents, and
is a Lisp timestamp in the style of `current-time'."
  (nth 5 attributes))

(compat-defun file-attribute-status-change-time (attributes) ;; <OK>
  "The status modification time in ATTRIBUTES returned by `file-attributes'.
This is the time of last change to the file's attributes: owner
and group, access mode bits, etc., and is a Lisp timestamp in the
style of `current-time'."
  (nth 6 attributes))

(compat-defun file-attribute-size (attributes) ;; <OK>
  "The integer size (in bytes) in ATTRIBUTES returned by `file-attributes'."
  (nth 7 attributes))

(compat-defun file-attribute-modes (attributes) ;; <OK>
  "The file modes in ATTRIBUTES returned by `file-attributes'.
This is a string of ten letters or dashes as in ls -l."
  (nth 8 attributes))

(compat-defun file-attribute-inode-number (attributes) ;; <OK>
  "The inode number in ATTRIBUTES returned by `file-attributes'.
It is a nonnegative integer."
  (nth 10 attributes))

(compat-defun file-attribute-device-number (attributes) ;; <OK>
  "The file system device number in ATTRIBUTES returned by `file-attributes'.
It is an integer."
  (nth 11 attributes))

(compat-defun file-attribute-collect (attributes &rest attr-names) ;; <OK>
  "Return a sublist of ATTRIBUTES returned by `file-attributes'.
ATTR-NAMES are symbols with the selected attribute names.

Valid attribute names are: type, link-number, user-id, group-id,
access-time, modification-time, status-change-time, size, modes,
inode-number and device-number."
  (let ((idx '((type . 0)
               (link-number . 1)
               (user-id . 2)
               (group-id . 3)
               (access-time . 4)
               (modification-time . 5)
               (status-change-time . 6)
               (size . 7)
               (modes . 8)
               (inode-number . 10)
               (device-number . 11)))
        result)
    (while attr-names
      (let ((attr (pop attr-names)))
        (if (assq attr idx)
            (push (nth (cdr (assq attr idx))
                       attributes)
                  result)
          (error "Wrong attribute name '%S'" attr))))
    (nreverse result)))

;;;; Defined in image.el

(compat-defun image-property (image property) ;; <OK>
  "Return the value of PROPERTY in IMAGE.
Properties can be set with

  (setf (image-property IMAGE PROPERTY) VALUE)

If VALUE is nil, PROPERTY is removed from IMAGE."
  :feature image
  (plist-get (cdr image) property))

;;;; Defined in rmc.el

(compat-defun read-multiple-choice (prompt choices) ;; <OK>
  "Ask user to select an entry from CHOICES, promting with PROMPT.
This function allows to ask the user a multiple-choice question.

CHOICES should be a list of the form (KEY NAME [DESCRIPTION]).
KEY is a character the user should type to select the entry.
NAME is a short name for the entry to be displayed while prompting
\(if there's no room, it might be shortened).

NOTE: This is a partial implementation of `read-multiple-choice', that
among other things doesn't offer any help and ignores the
optional DESCRIPTION field."
  (let ((options
         (mapconcat
          (lambda (opt)
            (format
             "[%s] %s"
             (key-description (string (car opt)))
             (cadr opt)))
          choices " "))
        choice)
    (setq prompt (concat prompt " (" options "): "))
    (while (not (setq choice (assq (read-event prompt) choices)))
      (message "Invalid choice")
      (sit-for 1))
    choice))

(provide 'compat-26)
;;; compat-26.el ends here
